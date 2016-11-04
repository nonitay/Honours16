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
#ROUND 5
##

#ROUND 5, Goal***************************************************************
#NA

round = 5
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
ADEL05_Gg2 <- data.frame(ADEL05_G)
ADEL05_Gg2 <- ADEL05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_Gg2$player1
player2vector <- ADEL05_Gg2$player2
ADEL05_Gg3 <- ADEL05_Gg2
ADEL05_Gg3$p1inp2vec <- is.element(ADEL05_Gg3$player1, player2vector)
ADEL05_Gg3$p2inp1vec <- is.element(ADEL05_Gg3$player2, player1vector)

addPlayer1 <- ADEL05_Gg3[ which(ADEL05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_Gg3[ which(ADEL05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_Gg2 <- rbind(ADEL05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
ADEL05_Gft <- ftable(ADEL05_Gg2$player1, ADEL05_Gg2$player2)
ADEL05_Gft2 <- as.matrix(ADEL05_Gft)
numRows <- nrow(ADEL05_Gft2)
numCols <- ncol(ADEL05_Gft2)
ADEL05_Gft3 <- ADEL05_Gft2[c(2:numRows) , c(2:numCols)]
ADEL05_GTable <- graph.adjacency(ADEL05_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(ADEL05_GTable, vertex.label = V(ADEL05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
ADEL05_G.clusterCoef <- transitivity(ADEL05_GTable, type="global") #cluster coefficient
ADEL05_G.degreeCent <- centralization.degree(ADEL05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_Gftn <- as.network.matrix(ADEL05_Gft)
ADEL05_G.netDensity <- network.density(ADEL05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_G.entropy <- entropy(ADEL05_Gft) #entropy

ADEL05_G.netMx <- cbind(ADEL05_G.netMx, ADEL05_G.clusterCoef, ADEL05_G.degreeCent$centralization,
                        ADEL05_G.netDensity, ADEL05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************

round = 5
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
ADEL05_Bg2 <- data.frame(ADEL05_B)
ADEL05_Bg2 <- ADEL05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_Bg2$player1
player2vector <- ADEL05_Bg2$player2
ADEL05_Bg3 <- ADEL05_Bg2
ADEL05_Bg3$p1inp2vec <- is.element(ADEL05_Bg3$player1, player2vector)
ADEL05_Bg3$p2inp1vec <- is.element(ADEL05_Bg3$player2, player1vector)

addPlayer1 <- ADEL05_Bg3[ which(ADEL05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_Bg3[ which(ADEL05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_Bg2 <- rbind(ADEL05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
ADEL05_Bft <- ftable(ADEL05_Bg2$player1, ADEL05_Bg2$player2)
ADEL05_Bft2 <- as.matrix(ADEL05_Bft)
numRows <- nrow(ADEL05_Bft2)
numCols <- ncol(ADEL05_Bft2)
ADEL05_Bft3 <- ADEL05_Bft2[c(2:numRows) , c(2:numCols)]
ADEL05_BTable <- graph.adjacency(ADEL05_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(ADEL05_BTable, vertex.label = V(ADEL05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
ADEL05_B.clusterCoef <- transitivity(ADEL05_BTable, type="global") #cluster coefficient
ADEL05_B.degreeCent <- centralization.degree(ADEL05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_Bftn <- as.network.matrix(ADEL05_Bft)
ADEL05_B.netDensity <- network.density(ADEL05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_B.entropy <- entropy(ADEL05_Bft) #entropy

ADEL05_B.netMx <- cbind(ADEL05_B.netMx, ADEL05_B.clusterCoef, ADEL05_B.degreeCent$centralization,
                        ADEL05_B.netDensity, ADEL05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
ADEL05_SFg2 <- data.frame(ADEL05_SF)
ADEL05_SFg2 <- ADEL05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_SFg2$player1
player2vector <- ADEL05_SFg2$player2
ADEL05_SFg3 <- ADEL05_SFg2
ADEL05_SFg3$p1inp2vec <- is.element(ADEL05_SFg3$player1, player2vector)
ADEL05_SFg3$p2inp1vec <- is.element(ADEL05_SFg3$player2, player1vector)

addPlayer1 <- ADEL05_SFg3[ which(ADEL05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_SFg3[ which(ADEL05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_SFg2 <- rbind(ADEL05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
ADEL05_SFft <- ftable(ADEL05_SFg2$player1, ADEL05_SFg2$player2)
ADEL05_SFft2 <- as.matrix(ADEL05_SFft)
numRows <- nrow(ADEL05_SFft2)
numCols <- ncol(ADEL05_SFft2)
ADEL05_SFft3 <- ADEL05_SFft2[c(2:numRows) , c(2:numCols)]
ADEL05_SFTable <- graph.adjacency(ADEL05_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(ADEL05_SFTable, vertex.label = V(ADEL05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
ADEL05_SF.clusterCoef <- transitivity(ADEL05_SFTable, type="global") #cluster coefficient
ADEL05_SF.degreeCent <- centralization.degree(ADEL05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_SFftn <- as.network.matrix(ADEL05_SFft)
ADEL05_SF.netDensity <- network.density(ADEL05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_SF.entropy <- entropy(ADEL05_SFft) #entropy

ADEL05_SF.netMx <- cbind(ADEL05_SF.netMx, ADEL05_SF.clusterCoef, ADEL05_SF.degreeCent$centralization,
                         ADEL05_SF.netDensity, ADEL05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************

round = 5
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
ADEL05_TFg2 <- data.frame(ADEL05_TF)
ADEL05_TFg2 <- ADEL05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_TFg2$player1
player2vector <- ADEL05_TFg2$player2
ADEL05_TFg3 <- ADEL05_TFg2
ADEL05_TFg3$p1inp2vec <- is.element(ADEL05_TFg3$player1, player2vector)
ADEL05_TFg3$p2inp1vec <- is.element(ADEL05_TFg3$player2, player1vector)

addPlayer1 <- ADEL05_TFg3[ which(ADEL05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_TFg3[ which(ADEL05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_TFg2 <- rbind(ADEL05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
ADEL05_TFft <- ftable(ADEL05_TFg2$player1, ADEL05_TFg2$player2)
ADEL05_TFft2 <- as.matrix(ADEL05_TFft)
numRows <- nrow(ADEL05_TFft2)
numCols <- ncol(ADEL05_TFft2)
ADEL05_TFft3 <- ADEL05_TFft2[c(2:numRows) , c(2:numCols)]
ADEL05_TFTable <- graph.adjacency(ADEL05_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(ADEL05_TFTable, vertex.label = V(ADEL05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
ADEL05_TF.clusterCoef <- transitivity(ADEL05_TFTable, type="global") #cluster coefficient
ADEL05_TF.degreeCent <- centralization.degree(ADEL05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_TFftn <- as.network.matrix(ADEL05_TFft)
ADEL05_TF.netDensity <- network.density(ADEL05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_TF.entropy <- entropy(ADEL05_TFft) #entropy

ADEL05_TF.netMx <- cbind(ADEL05_TF.netMx, ADEL05_TF.clusterCoef, ADEL05_TF.degreeCent$centralization,
                         ADEL05_TF.netDensity, ADEL05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
ADEL05_SAMg2 <- data.frame(ADEL05_SAM)
ADEL05_SAMg2 <- ADEL05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_SAMg2$player1
player2vector <- ADEL05_SAMg2$player2
ADEL05_SAMg3 <- ADEL05_SAMg2
ADEL05_SAMg3$p1inp2vec <- is.element(ADEL05_SAMg3$player1, player2vector)
ADEL05_SAMg3$p2inp1vec <- is.element(ADEL05_SAMg3$player2, player1vector)

addPlayer1 <- ADEL05_SAMg3[ which(ADEL05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_SAMg3[ which(ADEL05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_SAMg2 <- rbind(ADEL05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
ADEL05_SAMft <- ftable(ADEL05_SAMg2$player1, ADEL05_SAMg2$player2)
ADEL05_SAMft2 <- as.matrix(ADEL05_SAMft)
numRows <- nrow(ADEL05_SAMft2)
numCols <- ncol(ADEL05_SAMft2)
ADEL05_SAMft3 <- ADEL05_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL05_SAMTable <- graph.adjacency(ADEL05_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(ADEL05_SAMTable, vertex.label = V(ADEL05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
ADEL05_SAM.clusterCoef <- transitivity(ADEL05_SAMTable, type="global") #cluster coefficient
ADEL05_SAM.degreeCent <- centralization.degree(ADEL05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_SAMftn <- as.network.matrix(ADEL05_SAMft)
ADEL05_SAM.netDensity <- network.density(ADEL05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_SAM.entropy <- entropy(ADEL05_SAMft) #entropy

ADEL05_SAM.netMx <- cbind(ADEL05_SAM.netMx, ADEL05_SAM.clusterCoef, ADEL05_SAM.degreeCent$centralization,
                          ADEL05_SAM.netDensity, ADEL05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
ADEL05_TAMg2 <- data.frame(ADEL05_TAM)
ADEL05_TAMg2 <- ADEL05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_TAMg2$player1
player2vector <- ADEL05_TAMg2$player2
ADEL05_TAMg3 <- ADEL05_TAMg2
ADEL05_TAMg3$p1inp2vec <- is.element(ADEL05_TAMg3$player1, player2vector)
ADEL05_TAMg3$p2inp1vec <- is.element(ADEL05_TAMg3$player2, player1vector)

addPlayer1 <- ADEL05_TAMg3[ which(ADEL05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_TAMg3[ which(ADEL05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_TAMg2 <- rbind(ADEL05_TAMg2, addPlayers)


#ROUND 5, AM Turnover graph using weighted edges
ADEL05_TAMft <- ftable(ADEL05_TAMg2$player1, ADEL05_TAMg2$player2)
ADEL05_TAMft2 <- as.matrix(ADEL05_TAMft)
numRows <- nrow(ADEL05_TAMft2)
numCols <- ncol(ADEL05_TAMft2)
ADEL05_TAMft3 <- ADEL05_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL05_TAMTable <- graph.adjacency(ADEL05_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(ADEL05_TAMTable, vertex.label = V(ADEL05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
ADEL05_TAM.clusterCoef <- transitivity(ADEL05_TAMTable, type="global") #cluster coefficient
ADEL05_TAM.degreeCent <- centralization.degree(ADEL05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_TAMftn <- as.network.matrix(ADEL05_TAMft)
ADEL05_TAM.netDensity <- network.density(ADEL05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_TAM.entropy <- entropy(ADEL05_TAMft) #entropy

ADEL05_TAM.netMx <- cbind(ADEL05_TAM.netMx, ADEL05_TAM.clusterCoef, ADEL05_TAM.degreeCent$centralization,
                          ADEL05_TAM.netDensity, ADEL05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
ADEL05_SDMg2 <- data.frame(ADEL05_SDM)
ADEL05_SDMg2 <- ADEL05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_SDMg2$player1
player2vector <- ADEL05_SDMg2$player2
ADEL05_SDMg3 <- ADEL05_SDMg2
ADEL05_SDMg3$p1inp2vec <- is.element(ADEL05_SDMg3$player1, player2vector)
ADEL05_SDMg3$p2inp1vec <- is.element(ADEL05_SDMg3$player2, player1vector)

addPlayer1 <- ADEL05_SDMg3[ which(ADEL05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_SDMg3[ which(ADEL05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_SDMg2 <- rbind(ADEL05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
ADEL05_SDMft <- ftable(ADEL05_SDMg2$player1, ADEL05_SDMg2$player2)
ADEL05_SDMft2 <- as.matrix(ADEL05_SDMft)
numRows <- nrow(ADEL05_SDMft2)
numCols <- ncol(ADEL05_SDMft2)
ADEL05_SDMft3 <- ADEL05_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL05_SDMTable <- graph.adjacency(ADEL05_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(ADEL05_SDMTable, vertex.label = V(ADEL05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
ADEL05_SDM.clusterCoef <- transitivity(ADEL05_SDMTable, type="global") #cluster coefficient
ADEL05_SDM.degreeCent <- centralization.degree(ADEL05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_SDMftn <- as.network.matrix(ADEL05_SDMft)
ADEL05_SDM.netDensity <- network.density(ADEL05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_SDM.entropy <- entropy(ADEL05_SDMft) #entropy

ADEL05_SDM.netMx <- cbind(ADEL05_SDM.netMx, ADEL05_SDM.clusterCoef, ADEL05_SDM.degreeCent$centralization,
                          ADEL05_SDM.netDensity, ADEL05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
ADEL05_TDMg2 <- data.frame(ADEL05_TDM)
ADEL05_TDMg2 <- ADEL05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_TDMg2$player1
player2vector <- ADEL05_TDMg2$player2
ADEL05_TDMg3 <- ADEL05_TDMg2
ADEL05_TDMg3$p1inp2vec <- is.element(ADEL05_TDMg3$player1, player2vector)
ADEL05_TDMg3$p2inp1vec <- is.element(ADEL05_TDMg3$player2, player1vector)

addPlayer1 <- ADEL05_TDMg3[ which(ADEL05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ADEL05_TDMg3[ which(ADEL05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_TDMg2 <- rbind(ADEL05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
ADEL05_TDMft <- ftable(ADEL05_TDMg2$player1, ADEL05_TDMg2$player2)
ADEL05_TDMft2 <- as.matrix(ADEL05_TDMft)
numRows <- nrow(ADEL05_TDMft2)
numCols <- ncol(ADEL05_TDMft2)
ADEL05_TDMft3 <- ADEL05_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL05_TDMTable <- graph.adjacency(ADEL05_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(ADEL05_TDMTable, vertex.label = V(ADEL05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
ADEL05_TDM.clusterCoef <- transitivity(ADEL05_TDMTable, type="global") #cluster coefficient
ADEL05_TDM.degreeCent <- centralization.degree(ADEL05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_TDMftn <- as.network.matrix(ADEL05_TDMft)
ADEL05_TDM.netDensity <- network.density(ADEL05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_TDM.entropy <- entropy(ADEL05_TDMft) #entropy

ADEL05_TDM.netMx <- cbind(ADEL05_TDM.netMx, ADEL05_TDM.clusterCoef, ADEL05_TDM.degreeCent$centralization,
                          ADEL05_TDM.netDensity, ADEL05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
ADEL05_SDg2 <- data.frame(ADEL05_SD)
ADEL05_SDg2 <- ADEL05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_SDg2$player1
player2vector <- ADEL05_SDg2$player2
ADEL05_SDg3 <- ADEL05_SDg2
ADEL05_SDg3$p1inp2vec <- is.element(ADEL05_SDg3$player1, player2vector)
ADEL05_SDg3$p2inp1vec <- is.element(ADEL05_SDg3$player2, player1vector)

addPlayer1 <- ADEL05_SDg3[ which(ADEL05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_SDg3[ which(ADEL05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_SDg2 <- rbind(ADEL05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
ADEL05_SDft <- ftable(ADEL05_SDg2$player1, ADEL05_SDg2$player2)
ADEL05_SDft2 <- as.matrix(ADEL05_SDft)
numRows <- nrow(ADEL05_SDft2)
numCols <- ncol(ADEL05_SDft2)
ADEL05_SDft3 <- ADEL05_SDft2[c(2:numRows) , c(2:numCols)]
ADEL05_SDTable <- graph.adjacency(ADEL05_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(ADEL05_SDTable, vertex.label = V(ADEL05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
ADEL05_SD.clusterCoef <- transitivity(ADEL05_SDTable, type="global") #cluster coefficient
ADEL05_SD.degreeCent <- centralization.degree(ADEL05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_SDftn <- as.network.matrix(ADEL05_SDft)
ADEL05_SD.netDensity <- network.density(ADEL05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_SD.entropy <- entropy(ADEL05_SDft) #entropy

ADEL05_SD.netMx <- cbind(ADEL05_SD.netMx, ADEL05_SD.clusterCoef, ADEL05_SD.degreeCent$centralization,
                         ADEL05_SD.netDensity, ADEL05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
ADEL05_TDg2 <- data.frame(ADEL05_TD)
ADEL05_TDg2 <- ADEL05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_TDg2$player1
player2vector <- ADEL05_TDg2$player2
ADEL05_TDg3 <- ADEL05_TDg2
ADEL05_TDg3$p1inp2vec <- is.element(ADEL05_TDg3$player1, player2vector)
ADEL05_TDg3$p2inp1vec <- is.element(ADEL05_TDg3$player2, player1vector)

addPlayer1 <- ADEL05_TDg3[ which(ADEL05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_TDg3[ which(ADEL05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_TDg2 <- rbind(ADEL05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
ADEL05_TDft <- ftable(ADEL05_TDg2$player1, ADEL05_TDg2$player2)
ADEL05_TDft2 <- as.matrix(ADEL05_TDft)
numRows <- nrow(ADEL05_TDft2)
numCols <- ncol(ADEL05_TDft2)
ADEL05_TDft3 <- ADEL05_TDft2[c(2:numRows) , c(2:numCols)]
ADEL05_TDTable <- graph.adjacency(ADEL05_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(ADEL05_TDTable, vertex.label = V(ADEL05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
ADEL05_TD.clusterCoef <- transitivity(ADEL05_TDTable, type="global") #cluster coefficient
ADEL05_TD.degreeCent <- centralization.degree(ADEL05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_TDftn <- as.network.matrix(ADEL05_TDft)
ADEL05_TD.netDensity <- network.density(ADEL05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_TD.entropy <- entropy(ADEL05_TDft) #entropy

ADEL05_TD.netMx <- cbind(ADEL05_TD.netMx, ADEL05_TD.clusterCoef, ADEL05_TD.degreeCent$centralization,
                         ADEL05_TD.netDensity, ADEL05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
ADEL05_QTg2 <- data.frame(ADEL05_QT)
ADEL05_QTg2 <- ADEL05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL05_QTg2$player1
player2vector <- ADEL05_QTg2$player2
ADEL05_QTg3 <- ADEL05_QTg2
ADEL05_QTg3$p1inp2vec <- is.element(ADEL05_QTg3$player1, player2vector)
ADEL05_QTg3$p2inp1vec <- is.element(ADEL05_QTg3$player2, player1vector)

addPlayer1 <- ADEL05_QTg3[ which(ADEL05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL05_QTg3[ which(ADEL05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL05_QTg2 <- rbind(ADEL05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
ADEL05_QTft <- ftable(ADEL05_QTg2$player1, ADEL05_QTg2$player2)
ADEL05_QTft2 <- as.matrix(ADEL05_QTft)
numRows <- nrow(ADEL05_QTft2)
numCols <- ncol(ADEL05_QTft2)
ADEL05_QTft3 <- ADEL05_QTft2[c(2:numRows) , c(2:numCols)]
ADEL05_QTTable <- graph.adjacency(ADEL05_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(ADEL05_QTTable, vertex.label = V(ADEL05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
ADEL05_QT.clusterCoef <- transitivity(ADEL05_QTTable, type="global") #cluster coefficient
ADEL05_QT.degreeCent <- centralization.degree(ADEL05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL05_QTftn <- as.network.matrix(ADEL05_QTft)
ADEL05_QT.netDensity <- network.density(ADEL05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL05_QT.entropy <- entropy(ADEL05_QTft) #entropy

ADEL05_QT.netMx <- cbind(ADEL05_QT.netMx, ADEL05_QT.clusterCoef, ADEL05_QT.degreeCent$centralization,
                         ADEL05_QT.netDensity, ADEL05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL05_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "BL"
KIoutcome = "Goal_F"
BL05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
BL05_Gg2 <- data.frame(BL05_G)
BL05_Gg2 <- BL05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_Gg2$player1
player2vector <- BL05_Gg2$player2
BL05_Gg3 <- BL05_Gg2
BL05_Gg3$p1inp2vec <- is.element(BL05_Gg3$player1, player2vector)
BL05_Gg3$p2inp1vec <- is.element(BL05_Gg3$player2, player1vector)

addPlayer1 <- BL05_Gg3[ which(BL05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL05_Gg3[ which(BL05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_Gg2 <- rbind(BL05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
BL05_Gft <- ftable(BL05_Gg2$player1, BL05_Gg2$player2)
BL05_Gft2 <- as.matrix(BL05_Gft)
numRows <- nrow(BL05_Gft2)
numCols <- ncol(BL05_Gft2)
BL05_Gft3 <- BL05_Gft2[c(2:numRows) , c(2:numCols)]
BL05_GTable <- graph.adjacency(BL05_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(BL05_GTable, vertex.label = V(BL05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
BL05_G.clusterCoef <- transitivity(BL05_GTable, type="global") #cluster coefficient
BL05_G.degreeCent <- centralization.degree(BL05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_Gftn <- as.network.matrix(BL05_Gft)
BL05_G.netDensity <- network.density(BL05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_G.entropy <- entropy(BL05_Gft) #entropy

BL05_G.netMx <- cbind(BL05_G.netMx, BL05_G.clusterCoef, BL05_G.degreeCent$centralization,
                      BL05_G.netDensity, BL05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************

round = 5
teamName = "BL"
KIoutcome = "Behind_F"
BL05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
BL05_Bg2 <- data.frame(BL05_B)
BL05_Bg2 <- BL05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_Bg2$player1
player2vector <- BL05_Bg2$player2
BL05_Bg3 <- BL05_Bg2
BL05_Bg3$p1inp2vec <- is.element(BL05_Bg3$player1, player2vector)
BL05_Bg3$p2inp1vec <- is.element(BL05_Bg3$player2, player1vector)

addPlayer1 <- BL05_Bg3[ which(BL05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_Bg3[ which(BL05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_Bg2 <- rbind(BL05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
BL05_Bft <- ftable(BL05_Bg2$player1, BL05_Bg2$player2)
BL05_Bft2 <- as.matrix(BL05_Bft)
numRows <- nrow(BL05_Bft2)
numCols <- ncol(BL05_Bft2)
BL05_Bft3 <- BL05_Bft2[c(2:numRows) , c(2:numCols)]
BL05_BTable <- graph.adjacency(BL05_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(BL05_BTable, vertex.label = V(BL05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
BL05_B.clusterCoef <- transitivity(BL05_BTable, type="global") #cluster coefficient
BL05_B.degreeCent <- centralization.degree(BL05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_Bftn <- as.network.matrix(BL05_Bft)
BL05_B.netDensity <- network.density(BL05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_B.entropy <- entropy(BL05_Bft) #entropy

BL05_B.netMx <- cbind(BL05_B.netMx, BL05_B.clusterCoef, BL05_B.degreeCent$centralization,
                      BL05_B.netDensity, BL05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************

round = 5
teamName = "BL"
KIoutcome = "Stoppage_F"
BL05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
BL05_SFg2 <- data.frame(BL05_SF)
BL05_SFg2 <- BL05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_SFg2$player1
player2vector <- BL05_SFg2$player2
BL05_SFg3 <- BL05_SFg2
BL05_SFg3$p1inp2vec <- is.element(BL05_SFg3$player1, player2vector)
BL05_SFg3$p2inp1vec <- is.element(BL05_SFg3$player2, player1vector)

addPlayer1 <- BL05_SFg3[ which(BL05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_SFg3[ which(BL05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_SFg2 <- rbind(BL05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
BL05_SFft <- ftable(BL05_SFg2$player1, BL05_SFg2$player2)
BL05_SFft2 <- as.matrix(BL05_SFft)
numRows <- nrow(BL05_SFft2)
numCols <- ncol(BL05_SFft2)
BL05_SFft3 <- BL05_SFft2[c(2:numRows) , c(2:numCols)]
BL05_SFTable <- graph.adjacency(BL05_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(BL05_SFTable, vertex.label = V(BL05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
BL05_SF.clusterCoef <- transitivity(BL05_SFTable, type="global") #cluster coefficient
BL05_SF.degreeCent <- centralization.degree(BL05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_SFftn <- as.network.matrix(BL05_SFft)
BL05_SF.netDensity <- network.density(BL05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_SF.entropy <- entropy(BL05_SFft) #entropy

BL05_SF.netMx <- cbind(BL05_SF.netMx, BL05_SF.clusterCoef, BL05_SF.degreeCent$centralization,
                       BL05_SF.netDensity, BL05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************
#NA

round = 5
teamName = "BL"
KIoutcome = "Turnover_F"
BL05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
BL05_TFg2 <- data.frame(BL05_TF)
BL05_TFg2 <- BL05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_TFg2$player1
player2vector <- BL05_TFg2$player2
BL05_TFg3 <- BL05_TFg2
BL05_TFg3$p1inp2vec <- is.element(BL05_TFg3$player1, player2vector)
BL05_TFg3$p2inp1vec <- is.element(BL05_TFg3$player2, player1vector)

addPlayer1 <- BL05_TFg3[ which(BL05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_TFg3[ which(BL05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_TFg2 <- rbind(BL05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
BL05_TFft <- ftable(BL05_TFg2$player1, BL05_TFg2$player2)
BL05_TFft2 <- as.matrix(BL05_TFft)
numRows <- nrow(BL05_TFft2)
numCols <- ncol(BL05_TFft2)
BL05_TFft3 <- BL05_TFft2[c(2:numRows) , c(2:numCols)]
BL05_TFTable <- graph.adjacency(BL05_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(BL05_TFTable, vertex.label = V(BL05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
BL05_TF.clusterCoef <- transitivity(BL05_TFTable, type="global") #cluster coefficient
BL05_TF.degreeCent <- centralization.degree(BL05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_TFftn <- as.network.matrix(BL05_TFft)
BL05_TF.netDensity <- network.density(BL05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_TF.entropy <- entropy(BL05_TFft) #entropy

BL05_TF.netMx <- cbind(BL05_TF.netMx, BL05_TF.clusterCoef, BL05_TF.degreeCent$centralization,
                       BL05_TF.netDensity, BL05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************
#NA

round = 5
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
BL05_SAMg2 <- data.frame(BL05_SAM)
BL05_SAMg2 <- BL05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_SAMg2$player1
player2vector <- BL05_SAMg2$player2
BL05_SAMg3 <- BL05_SAMg2
BL05_SAMg3$p1inp2vec <- is.element(BL05_SAMg3$player1, player2vector)
BL05_SAMg3$p2inp1vec <- is.element(BL05_SAMg3$player2, player1vector)

addPlayer1 <- BL05_SAMg3[ which(BL05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_SAMg3[ which(BL05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_SAMg2 <- rbind(BL05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
BL05_SAMft <- ftable(BL05_SAMg2$player1, BL05_SAMg2$player2)
BL05_SAMft2 <- as.matrix(BL05_SAMft)
numRows <- nrow(BL05_SAMft2)
numCols <- ncol(BL05_SAMft2)
BL05_SAMft3 <- BL05_SAMft2[c(2:numRows) , c(2:numCols)]
BL05_SAMTable <- graph.adjacency(BL05_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(BL05_SAMTable, vertex.label = V(BL05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
BL05_SAM.clusterCoef <- transitivity(BL05_SAMTable, type="global") #cluster coefficient
BL05_SAM.degreeCent <- centralization.degree(BL05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_SAMftn <- as.network.matrix(BL05_SAMft)
BL05_SAM.netDensity <- network.density(BL05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_SAM.entropy <- entropy(BL05_SAMft) #entropy

BL05_SAM.netMx <- cbind(BL05_SAM.netMx, BL05_SAM.clusterCoef, BL05_SAM.degreeCent$centralization,
                        BL05_SAM.netDensity, BL05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "BL"
KIoutcome = "Turnover_AM"
BL05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
BL05_TAMg2 <- data.frame(BL05_TAM)
BL05_TAMg2 <- BL05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_TAMg2$player1
player2vector <- BL05_TAMg2$player2
BL05_TAMg3 <- BL05_TAMg2
BL05_TAMg3$p1inp2vec <- is.element(BL05_TAMg3$player1, player2vector)
BL05_TAMg3$p2inp1vec <- is.element(BL05_TAMg3$player2, player1vector)

addPlayer1 <- BL05_TAMg3[ which(BL05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_TAMg3[ which(BL05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_TAMg2 <- rbind(BL05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
BL05_TAMft <- ftable(BL05_TAMg2$player1, BL05_TAMg2$player2)
BL05_TAMft2 <- as.matrix(BL05_TAMft)
numRows <- nrow(BL05_TAMft2)
numCols <- ncol(BL05_TAMft2)
BL05_TAMft3 <- BL05_TAMft2[c(2:numRows) , c(2:numCols)]
BL05_TAMTable <- graph.adjacency(BL05_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(BL05_TAMTable, vertex.label = V(BL05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
BL05_TAM.clusterCoef <- transitivity(BL05_TAMTable, type="global") #cluster coefficient
BL05_TAM.degreeCent <- centralization.degree(BL05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_TAMftn <- as.network.matrix(BL05_TAMft)
BL05_TAM.netDensity <- network.density(BL05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_TAM.entropy <- entropy(BL05_TAMft) #entropy

BL05_TAM.netMx <- cbind(BL05_TAM.netMx, BL05_TAM.clusterCoef, BL05_TAM.degreeCent$centralization,
                        BL05_TAM.netDensity, BL05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
BL05_SDMg2 <- data.frame(BL05_SDM)
BL05_SDMg2 <- BL05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_SDMg2$player1
player2vector <- BL05_SDMg2$player2
BL05_SDMg3 <- BL05_SDMg2
BL05_SDMg3$p1inp2vec <- is.element(BL05_SDMg3$player1, player2vector)
BL05_SDMg3$p2inp1vec <- is.element(BL05_SDMg3$player2, player1vector)

addPlayer1 <- BL05_SDMg3[ which(BL05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_SDMg3[ which(BL05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_SDMg2 <- rbind(BL05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
BL05_SDMft <- ftable(BL05_SDMg2$player1, BL05_SDMg2$player2)
BL05_SDMft2 <- as.matrix(BL05_SDMft)
numRows <- nrow(BL05_SDMft2)
numCols <- ncol(BL05_SDMft2)
BL05_SDMft3 <- BL05_SDMft2[c(2:numRows) , c(2:numCols)]
BL05_SDMTable <- graph.adjacency(BL05_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(BL05_SDMTable, vertex.label = V(BL05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
BL05_SDM.clusterCoef <- transitivity(BL05_SDMTable, type="global") #cluster coefficient
BL05_SDM.degreeCent <- centralization.degree(BL05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_SDMftn <- as.network.matrix(BL05_SDMft)
BL05_SDM.netDensity <- network.density(BL05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_SDM.entropy <- entropy(BL05_SDMft) #entropy

BL05_SDM.netMx <- cbind(BL05_SDM.netMx, BL05_SDM.clusterCoef, BL05_SDM.degreeCent$centralization,
                        BL05_SDM.netDensity, BL05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "BL"
KIoutcome = "Turnover_DM"
BL05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
BL05_TDMg2 <- data.frame(BL05_TDM)
BL05_TDMg2 <- BL05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_TDMg2$player1
player2vector <- BL05_TDMg2$player2
BL05_TDMg3 <- BL05_TDMg2
BL05_TDMg3$p1inp2vec <- is.element(BL05_TDMg3$player1, player2vector)
BL05_TDMg3$p2inp1vec <- is.element(BL05_TDMg3$player2, player1vector)

addPlayer1 <- BL05_TDMg3[ which(BL05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_TDMg3[ which(BL05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_TDMg2 <- rbind(BL05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
BL05_TDMft <- ftable(BL05_TDMg2$player1, BL05_TDMg2$player2)
BL05_TDMft2 <- as.matrix(BL05_TDMft)
numRows <- nrow(BL05_TDMft2)
numCols <- ncol(BL05_TDMft2)
BL05_TDMft3 <- BL05_TDMft2[c(2:numRows) , c(2:numCols)]
BL05_TDMTable <- graph.adjacency(BL05_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(BL05_TDMTable, vertex.label = V(BL05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
BL05_TDM.clusterCoef <- transitivity(BL05_TDMTable, type="global") #cluster coefficient
BL05_TDM.degreeCent <- centralization.degree(BL05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_TDMftn <- as.network.matrix(BL05_TDMft)
BL05_TDM.netDensity <- network.density(BL05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_TDM.entropy <- entropy(BL05_TDMft) #entropy

BL05_TDM.netMx <- cbind(BL05_TDM.netMx, BL05_TDM.clusterCoef, BL05_TDM.degreeCent$centralization,
                        BL05_TDM.netDensity, BL05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "BL"
KIoutcome = "Stoppage_D"
BL05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
BL05_SDg2 <- data.frame(BL05_SD)
BL05_SDg2 <- BL05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_SDg2$player1
player2vector <- BL05_SDg2$player2
BL05_SDg3 <- BL05_SDg2
BL05_SDg3$p1inp2vec <- is.element(BL05_SDg3$player1, player2vector)
BL05_SDg3$p2inp1vec <- is.element(BL05_SDg3$player2, player1vector)

addPlayer1 <- BL05_SDg3[ which(BL05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_SDg3[ which(BL05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_SDg2 <- rbind(BL05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
BL05_SDft <- ftable(BL05_SDg2$player1, BL05_SDg2$player2)
BL05_SDft2 <- as.matrix(BL05_SDft)
numRows <- nrow(BL05_SDft2)
numCols <- ncol(BL05_SDft2)
BL05_SDft3 <- BL05_SDft2[c(2:numRows) , c(2:numCols)]
BL05_SDTable <- graph.adjacency(BL05_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(BL05_SDTable, vertex.label = V(BL05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
BL05_SD.clusterCoef <- transitivity(BL05_SDTable, type="global") #cluster coefficient
BL05_SD.degreeCent <- centralization.degree(BL05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_SDftn <- as.network.matrix(BL05_SDft)
BL05_SD.netDensity <- network.density(BL05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_SD.entropy <- entropy(BL05_SDft) #entropy

BL05_SD.netMx <- cbind(BL05_SD.netMx, BL05_SD.clusterCoef, BL05_SD.degreeCent$centralization,
                       BL05_SD.netDensity, BL05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "BL"
KIoutcome = "Turnover_D"
BL05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
BL05_TDg2 <- data.frame(BL05_TD)
BL05_TDg2 <- BL05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_TDg2$player1
player2vector <- BL05_TDg2$player2
BL05_TDg3 <- BL05_TDg2
BL05_TDg3$p1inp2vec <- is.element(BL05_TDg3$player1, player2vector)
BL05_TDg3$p2inp1vec <- is.element(BL05_TDg3$player2, player1vector)

addPlayer1 <- BL05_TDg3[ which(BL05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_TDg3[ which(BL05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_TDg2 <- rbind(BL05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
BL05_TDft <- ftable(BL05_TDg2$player1, BL05_TDg2$player2)
BL05_TDft2 <- as.matrix(BL05_TDft)
numRows <- nrow(BL05_TDft2)
numCols <- ncol(BL05_TDft2)
BL05_TDft3 <- BL05_TDft2[c(2:numRows) , c(2:numCols)]
BL05_TDTable <- graph.adjacency(BL05_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(BL05_TDTable, vertex.label = V(BL05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
BL05_TD.clusterCoef <- transitivity(BL05_TDTable, type="global") #cluster coefficient
BL05_TD.degreeCent <- centralization.degree(BL05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_TDftn <- as.network.matrix(BL05_TDft)
BL05_TD.netDensity <- network.density(BL05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_TD.entropy <- entropy(BL05_TDft) #entropy

BL05_TD.netMx <- cbind(BL05_TD.netMx, BL05_TD.clusterCoef, BL05_TD.degreeCent$centralization,
                       BL05_TD.netDensity, BL05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
BL05_QTg2 <- data.frame(BL05_QT)
BL05_QTg2 <- BL05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL05_QTg2$player1
player2vector <- BL05_QTg2$player2
BL05_QTg3 <- BL05_QTg2
BL05_QTg3$p1inp2vec <- is.element(BL05_QTg3$player1, player2vector)
BL05_QTg3$p2inp1vec <- is.element(BL05_QTg3$player2, player1vector)

addPlayer1 <- BL05_QTg3[ which(BL05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL05_QTg3[ which(BL05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL05_QTg2 <- rbind(BL05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
BL05_QTft <- ftable(BL05_QTg2$player1, BL05_QTg2$player2)
BL05_QTft2 <- as.matrix(BL05_QTft)
numRows <- nrow(BL05_QTft2)
numCols <- ncol(BL05_QTft2)
BL05_QTft3 <- BL05_QTft2[c(2:numRows) , c(2:numCols)]
BL05_QTTable <- graph.adjacency(BL05_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(BL05_QTTable, vertex.label = V(BL05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
BL05_QT.clusterCoef <- transitivity(BL05_QTTable, type="global") #cluster coefficient
BL05_QT.degreeCent <- centralization.degree(BL05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL05_QTftn <- as.network.matrix(BL05_QTft)
BL05_QT.netDensity <- network.density(BL05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL05_QT.entropy <- entropy(BL05_QTft) #entropy

BL05_QT.netMx <- cbind(BL05_QT.netMx, BL05_QT.clusterCoef, BL05_QT.degreeCent$centralization,
                       BL05_QT.netDensity, BL05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL05_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************
#NA

round = 5
teamName = "CARL"
KIoutcome = "Goal_F"
CARL05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
CARL05_Gg2 <- data.frame(CARL05_G)
CARL05_Gg2 <- CARL05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_Gg2$player1
player2vector <- CARL05_Gg2$player2
CARL05_Gg3 <- CARL05_Gg2
CARL05_Gg3$p1inp2vec <- is.element(CARL05_Gg3$player1, player2vector)
CARL05_Gg3$p2inp1vec <- is.element(CARL05_Gg3$player2, player1vector)

addPlayer1 <- CARL05_Gg3[ which(CARL05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL05_Gg3[ which(CARL05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_Gg2 <- rbind(CARL05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
CARL05_Gft <- ftable(CARL05_Gg2$player1, CARL05_Gg2$player2)
CARL05_Gft2 <- as.matrix(CARL05_Gft)
numRows <- nrow(CARL05_Gft2)
numCols <- ncol(CARL05_Gft2)
CARL05_Gft3 <- CARL05_Gft2[c(2:numRows) , c(2:numCols)]
CARL05_GTable <- graph.adjacency(CARL05_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(CARL05_GTable, vertex.label = V(CARL05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
CARL05_G.clusterCoef <- transitivity(CARL05_GTable, type="global") #cluster coefficient
CARL05_G.degreeCent <- centralization.degree(CARL05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_Gftn <- as.network.matrix(CARL05_Gft)
CARL05_G.netDensity <- network.density(CARL05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_G.entropy <- entropy(CARL05_Gft) #entropy

CARL05_G.netMx <- cbind(CARL05_G.netMx, CARL05_G.clusterCoef, CARL05_G.degreeCent$centralization,
                        CARL05_G.netDensity, CARL05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************
#NA

round = 5
teamName = "CARL"
KIoutcome = "Behind_F"
CARL05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
CARL05_Bg2 <- data.frame(CARL05_B)
CARL05_Bg2 <- CARL05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_Bg2$player1
player2vector <- CARL05_Bg2$player2
CARL05_Bg3 <- CARL05_Bg2
CARL05_Bg3$p1inp2vec <- is.element(CARL05_Bg3$player1, player2vector)
CARL05_Bg3$p2inp1vec <- is.element(CARL05_Bg3$player2, player1vector)

addPlayer1 <- CARL05_Bg3[ which(CARL05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL05_Bg3[ which(CARL05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_Bg2 <- rbind(CARL05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
CARL05_Bft <- ftable(CARL05_Bg2$player1, CARL05_Bg2$player2)
CARL05_Bft2 <- as.matrix(CARL05_Bft)
numRows <- nrow(CARL05_Bft2)
numCols <- ncol(CARL05_Bft2)
CARL05_Bft3 <- CARL05_Bft2[c(2:numRows) , c(2:numCols)]
CARL05_BTable <- graph.adjacency(CARL05_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(CARL05_BTable, vertex.label = V(CARL05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
CARL05_B.clusterCoef <- transitivity(CARL05_BTable, type="global") #cluster coefficient
CARL05_B.degreeCent <- centralization.degree(CARL05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_Bftn <- as.network.matrix(CARL05_Bft)
CARL05_B.netDensity <- network.density(CARL05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_B.entropy <- entropy(CARL05_Bft) #entropy

CARL05_B.netMx <- cbind(CARL05_B.netMx, CARL05_B.clusterCoef, CARL05_B.degreeCent$centralization,
                        CARL05_B.netDensity, CARL05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
CARL05_SFg2 <- data.frame(CARL05_SF)
CARL05_SFg2 <- CARL05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_SFg2$player1
player2vector <- CARL05_SFg2$player2
CARL05_SFg3 <- CARL05_SFg2
CARL05_SFg3$p1inp2vec <- is.element(CARL05_SFg3$player1, player2vector)
CARL05_SFg3$p2inp1vec <- is.element(CARL05_SFg3$player2, player1vector)

addPlayer1 <- CARL05_SFg3[ which(CARL05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL05_SFg3[ which(CARL05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_SFg2 <- rbind(CARL05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
CARL05_SFft <- ftable(CARL05_SFg2$player1, CARL05_SFg2$player2)
CARL05_SFft2 <- as.matrix(CARL05_SFft)
numRows <- nrow(CARL05_SFft2)
numCols <- ncol(CARL05_SFft2)
CARL05_SFft3 <- CARL05_SFft2[c(2:numRows) , c(2:numCols)]
CARL05_SFTable <- graph.adjacency(CARL05_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(CARL05_SFTable, vertex.label = V(CARL05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
CARL05_SF.clusterCoef <- transitivity(CARL05_SFTable, type="global") #cluster coefficient
CARL05_SF.degreeCent <- centralization.degree(CARL05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_SFftn <- as.network.matrix(CARL05_SFft)
CARL05_SF.netDensity <- network.density(CARL05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_SF.entropy <- entropy(CARL05_SFft) #entropy

CARL05_SF.netMx <- cbind(CARL05_SF.netMx, CARL05_SF.clusterCoef, CARL05_SF.degreeCent$centralization,
                         CARL05_SF.netDensity, CARL05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************
#NA

round = 5
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
CARL05_TFg2 <- data.frame(CARL05_TF)
CARL05_TFg2 <- CARL05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_TFg2$player1
player2vector <- CARL05_TFg2$player2
CARL05_TFg3 <- CARL05_TFg2
CARL05_TFg3$p1inp2vec <- is.element(CARL05_TFg3$player1, player2vector)
CARL05_TFg3$p2inp1vec <- is.element(CARL05_TFg3$player2, player1vector)

addPlayer1 <- CARL05_TFg3[ which(CARL05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL05_TFg3[ which(CARL05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_TFg2 <- rbind(CARL05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
CARL05_TFft <- ftable(CARL05_TFg2$player1, CARL05_TFg2$player2)
CARL05_TFft2 <- as.matrix(CARL05_TFft)
numRows <- nrow(CARL05_TFft2)
numCols <- ncol(CARL05_TFft2)
CARL05_TFft3 <- CARL05_TFft2[c(2:numRows) , c(2:numCols)]
CARL05_TFTable <- graph.adjacency(CARL05_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(CARL05_TFTable, vertex.label = V(CARL05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
CARL05_TF.clusterCoef <- transitivity(CARL05_TFTable, type="global") #cluster coefficient
CARL05_TF.degreeCent <- centralization.degree(CARL05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_TFftn <- as.network.matrix(CARL05_TFft)
CARL05_TF.netDensity <- network.density(CARL05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_TF.entropy <- entropy(CARL05_TFft) #entropy

CARL05_TF.netMx <- cbind(CARL05_TF.netMx, CARL05_TF.clusterCoef, CARL05_TF.degreeCent$centralization,
                         CARL05_TF.netDensity, CARL05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
CARL05_SAMg2 <- data.frame(CARL05_SAM)
CARL05_SAMg2 <- CARL05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_SAMg2$player1
player2vector <- CARL05_SAMg2$player2
CARL05_SAMg3 <- CARL05_SAMg2
CARL05_SAMg3$p1inp2vec <- is.element(CARL05_SAMg3$player1, player2vector)
CARL05_SAMg3$p2inp1vec <- is.element(CARL05_SAMg3$player2, player1vector)

addPlayer1 <- CARL05_SAMg3[ which(CARL05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL05_SAMg3[ which(CARL05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_SAMg2 <- rbind(CARL05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
CARL05_SAMft <- ftable(CARL05_SAMg2$player1, CARL05_SAMg2$player2)
CARL05_SAMft2 <- as.matrix(CARL05_SAMft)
numRows <- nrow(CARL05_SAMft2)
numCols <- ncol(CARL05_SAMft2)
CARL05_SAMft3 <- CARL05_SAMft2[c(2:numRows) , c(2:numCols)]
CARL05_SAMTable <- graph.adjacency(CARL05_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(CARL05_SAMTable, vertex.label = V(CARL05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
CARL05_SAM.clusterCoef <- transitivity(CARL05_SAMTable, type="global") #cluster coefficient
CARL05_SAM.degreeCent <- centralization.degree(CARL05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_SAMftn <- as.network.matrix(CARL05_SAMft)
CARL05_SAM.netDensity <- network.density(CARL05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_SAM.entropy <- entropy(CARL05_SAMft) #entropy

CARL05_SAM.netMx <- cbind(CARL05_SAM.netMx, CARL05_SAM.clusterCoef, CARL05_SAM.degreeCent$centralization,
                          CARL05_SAM.netDensity, CARL05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************
#NA

round = 5
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
CARL05_TAMg2 <- data.frame(CARL05_TAM)
CARL05_TAMg2 <- CARL05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_TAMg2$player1
player2vector <- CARL05_TAMg2$player2
CARL05_TAMg3 <- CARL05_TAMg2
CARL05_TAMg3$p1inp2vec <- is.element(CARL05_TAMg3$player1, player2vector)
CARL05_TAMg3$p2inp1vec <- is.element(CARL05_TAMg3$player2, player1vector)

addPlayer1 <- CARL05_TAMg3[ which(CARL05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL05_TAMg3[ which(CARL05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_TAMg2 <- rbind(CARL05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
CARL05_TAMft <- ftable(CARL05_TAMg2$player1, CARL05_TAMg2$player2)
CARL05_TAMft2 <- as.matrix(CARL05_TAMft)
numRows <- nrow(CARL05_TAMft2)
numCols <- ncol(CARL05_TAMft2)
CARL05_TAMft3 <- CARL05_TAMft2[c(2:numRows) , c(2:numCols)]
CARL05_TAMTable <- graph.adjacency(CARL05_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(CARL05_TAMTable, vertex.label = V(CARL05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
CARL05_TAM.clusterCoef <- transitivity(CARL05_TAMTable, type="global") #cluster coefficient
CARL05_TAM.degreeCent <- centralization.degree(CARL05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_TAMftn <- as.network.matrix(CARL05_TAMft)
CARL05_TAM.netDensity <- network.density(CARL05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_TAM.entropy <- entropy(CARL05_TAMft) #entropy

CARL05_TAM.netMx <- cbind(CARL05_TAM.netMx, CARL05_TAM.clusterCoef, CARL05_TAM.degreeCent$centralization,
                          CARL05_TAM.netDensity, CARL05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
CARL05_SDMg2 <- data.frame(CARL05_SDM)
CARL05_SDMg2 <- CARL05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_SDMg2$player1
player2vector <- CARL05_SDMg2$player2
CARL05_SDMg3 <- CARL05_SDMg2
CARL05_SDMg3$p1inp2vec <- is.element(CARL05_SDMg3$player1, player2vector)
CARL05_SDMg3$p2inp1vec <- is.element(CARL05_SDMg3$player2, player1vector)

addPlayer1 <- CARL05_SDMg3[ which(CARL05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL05_SDMg3[ which(CARL05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_SDMg2 <- rbind(CARL05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
CARL05_SDMft <- ftable(CARL05_SDMg2$player1, CARL05_SDMg2$player2)
CARL05_SDMft2 <- as.matrix(CARL05_SDMft)
numRows <- nrow(CARL05_SDMft2)
numCols <- ncol(CARL05_SDMft2)
CARL05_SDMft3 <- CARL05_SDMft2[c(2:numRows) , c(2:numCols)]
CARL05_SDMTable <- graph.adjacency(CARL05_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(CARL05_SDMTable, vertex.label = V(CARL05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
CARL05_SDM.clusterCoef <- transitivity(CARL05_SDMTable, type="global") #cluster coefficient
CARL05_SDM.degreeCent <- centralization.degree(CARL05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_SDMftn <- as.network.matrix(CARL05_SDMft)
CARL05_SDM.netDensity <- network.density(CARL05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_SDM.entropy <- entropy(CARL05_SDMft) #entropy

CARL05_SDM.netMx <- cbind(CARL05_SDM.netMx, CARL05_SDM.clusterCoef, CARL05_SDM.degreeCent$centralization,
                          CARL05_SDM.netDensity, CARL05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
CARL05_TDMg2 <- data.frame(CARL05_TDM)
CARL05_TDMg2 <- CARL05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_TDMg2$player1
player2vector <- CARL05_TDMg2$player2
CARL05_TDMg3 <- CARL05_TDMg2
CARL05_TDMg3$p1inp2vec <- is.element(CARL05_TDMg3$player1, player2vector)
CARL05_TDMg3$p2inp1vec <- is.element(CARL05_TDMg3$player2, player1vector)

addPlayer1 <- CARL05_TDMg3[ which(CARL05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL05_TDMg3[ which(CARL05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_TDMg2 <- rbind(CARL05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
CARL05_TDMft <- ftable(CARL05_TDMg2$player1, CARL05_TDMg2$player2)
CARL05_TDMft2 <- as.matrix(CARL05_TDMft)
numRows <- nrow(CARL05_TDMft2)
numCols <- ncol(CARL05_TDMft2)
CARL05_TDMft3 <- CARL05_TDMft2[c(2:numRows) , c(2:numCols)]
CARL05_TDMTable <- graph.adjacency(CARL05_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(CARL05_TDMTable, vertex.label = V(CARL05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
CARL05_TDM.clusterCoef <- transitivity(CARL05_TDMTable, type="global") #cluster coefficient
CARL05_TDM.degreeCent <- centralization.degree(CARL05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_TDMftn <- as.network.matrix(CARL05_TDMft)
CARL05_TDM.netDensity <- network.density(CARL05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_TDM.entropy <- entropy(CARL05_TDMft) #entropy

CARL05_TDM.netMx <- cbind(CARL05_TDM.netMx, CARL05_TDM.clusterCoef, CARL05_TDM.degreeCent$centralization,
                          CARL05_TDM.netDensity, CARL05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
CARL05_SDg2 <- data.frame(CARL05_SD)
CARL05_SDg2 <- CARL05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_SDg2$player1
player2vector <- CARL05_SDg2$player2
CARL05_SDg3 <- CARL05_SDg2
CARL05_SDg3$p1inp2vec <- is.element(CARL05_SDg3$player1, player2vector)
CARL05_SDg3$p2inp1vec <- is.element(CARL05_SDg3$player2, player1vector)

addPlayer1 <- CARL05_SDg3[ which(CARL05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL05_SDg3[ which(CARL05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_SDg2 <- rbind(CARL05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
CARL05_SDft <- ftable(CARL05_SDg2$player1, CARL05_SDg2$player2)
CARL05_SDft2 <- as.matrix(CARL05_SDft)
numRows <- nrow(CARL05_SDft2)
numCols <- ncol(CARL05_SDft2)
CARL05_SDft3 <- CARL05_SDft2[c(2:numRows) , c(2:numCols)]
CARL05_SDTable <- graph.adjacency(CARL05_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(CARL05_SDTable, vertex.label = V(CARL05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
CARL05_SD.clusterCoef <- transitivity(CARL05_SDTable, type="global") #cluster coefficient
CARL05_SD.degreeCent <- centralization.degree(CARL05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_SDftn <- as.network.matrix(CARL05_SDft)
CARL05_SD.netDensity <- network.density(CARL05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_SD.entropy <- entropy(CARL05_SDft) #entropy

CARL05_SD.netMx <- cbind(CARL05_SD.netMx, CARL05_SD.clusterCoef, CARL05_SD.degreeCent$centralization,
                         CARL05_SD.netDensity, CARL05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
CARL05_TDg2 <- data.frame(CARL05_TD)
CARL05_TDg2 <- CARL05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_TDg2$player1
player2vector <- CARL05_TDg2$player2
CARL05_TDg3 <- CARL05_TDg2
CARL05_TDg3$p1inp2vec <- is.element(CARL05_TDg3$player1, player2vector)
CARL05_TDg3$p2inp1vec <- is.element(CARL05_TDg3$player2, player1vector)

addPlayer1 <- CARL05_TDg3[ which(CARL05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL05_TDg3[ which(CARL05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_TDg2 <- rbind(CARL05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
CARL05_TDft <- ftable(CARL05_TDg2$player1, CARL05_TDg2$player2)
CARL05_TDft2 <- as.matrix(CARL05_TDft)
numRows <- nrow(CARL05_TDft2)
numCols <- ncol(CARL05_TDft2)
CARL05_TDft3 <- CARL05_TDft2[c(2:numRows) , c(2:numCols)]
CARL05_TDTable <- graph.adjacency(CARL05_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(CARL05_TDTable, vertex.label = V(CARL05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
CARL05_TD.clusterCoef <- transitivity(CARL05_TDTable, type="global") #cluster coefficient
CARL05_TD.degreeCent <- centralization.degree(CARL05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_TDftn <- as.network.matrix(CARL05_TDft)
CARL05_TD.netDensity <- network.density(CARL05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_TD.entropy <- entropy(CARL05_TDft) #entropy

CARL05_TD.netMx <- cbind(CARL05_TD.netMx, CARL05_TD.clusterCoef, CARL05_TD.degreeCent$centralization,
                         CARL05_TD.netDensity, CARL05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
CARL05_QTg2 <- data.frame(CARL05_QT)
CARL05_QTg2 <- CARL05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL05_QTg2$player1
player2vector <- CARL05_QTg2$player2
CARL05_QTg3 <- CARL05_QTg2
CARL05_QTg3$p1inp2vec <- is.element(CARL05_QTg3$player1, player2vector)
CARL05_QTg3$p2inp1vec <- is.element(CARL05_QTg3$player2, player1vector)

addPlayer1 <- CARL05_QTg3[ which(CARL05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL05_QTg3[ which(CARL05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL05_QTg2 <- rbind(CARL05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
CARL05_QTft <- ftable(CARL05_QTg2$player1, CARL05_QTg2$player2)
CARL05_QTft2 <- as.matrix(CARL05_QTft)
numRows <- nrow(CARL05_QTft2)
numCols <- ncol(CARL05_QTft2)
CARL05_QTft3 <- CARL05_QTft2[c(2:numRows) , c(2:numCols)]
CARL05_QTTable <- graph.adjacency(CARL05_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(CARL05_QTTable, vertex.label = V(CARL05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
CARL05_QT.clusterCoef <- transitivity(CARL05_QTTable, type="global") #cluster coefficient
CARL05_QT.degreeCent <- centralization.degree(CARL05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL05_QTftn <- as.network.matrix(CARL05_QTft)
CARL05_QT.netDensity <- network.density(CARL05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL05_QT.entropy <- entropy(CARL05_QTft) #entropy

CARL05_QT.netMx <- cbind(CARL05_QT.netMx, CARL05_QT.clusterCoef, CARL05_QT.degreeCent$centralization,
                         CARL05_QT.netDensity, CARL05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL05_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "COLL"
KIoutcome = "Goal_F"
COLL05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
COLL05_Gg2 <- data.frame(COLL05_G)
COLL05_Gg2 <- COLL05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_Gg2$player1
player2vector <- COLL05_Gg2$player2
COLL05_Gg3 <- COLL05_Gg2
COLL05_Gg3$p1inp2vec <- is.element(COLL05_Gg3$player1, player2vector)
COLL05_Gg3$p2inp1vec <- is.element(COLL05_Gg3$player2, player1vector)

addPlayer1 <- COLL05_Gg3[ which(COLL05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_Gg3[ which(COLL05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_Gg2 <- rbind(COLL05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
COLL05_Gft <- ftable(COLL05_Gg2$player1, COLL05_Gg2$player2)
COLL05_Gft2 <- as.matrix(COLL05_Gft)
numRows <- nrow(COLL05_Gft2)
numCols <- ncol(COLL05_Gft2)
COLL05_Gft3 <- COLL05_Gft2[c(2:numRows) , c(2:numCols)]
COLL05_GTable <- graph.adjacency(COLL05_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(COLL05_GTable, vertex.label = V(COLL05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
COLL05_G.clusterCoef <- transitivity(COLL05_GTable, type="global") #cluster coefficient
COLL05_G.degreeCent <- centralization.degree(COLL05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_Gftn <- as.network.matrix(COLL05_Gft)
COLL05_G.netDensity <- network.density(COLL05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_G.entropy <- entropy(COLL05_Gft) #entropy

COLL05_G.netMx <- cbind(COLL05_G.netMx, COLL05_G.clusterCoef, COLL05_G.degreeCent$centralization,
                        COLL05_G.netDensity, COLL05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************
#NA

round = 5
teamName = "COLL"
KIoutcome = "Behind_F"
COLL05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
COLL05_Bg2 <- data.frame(COLL05_B)
COLL05_Bg2 <- COLL05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_Bg2$player1
player2vector <- COLL05_Bg2$player2
COLL05_Bg3 <- COLL05_Bg2
COLL05_Bg3$p1inp2vec <- is.element(COLL05_Bg3$player1, player2vector)
COLL05_Bg3$p2inp1vec <- is.element(COLL05_Bg3$player2, player1vector)

addPlayer1 <- COLL05_Bg3[ which(COLL05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_Bg3[ which(COLL05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_Bg2 <- rbind(COLL05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
COLL05_Bft <- ftable(COLL05_Bg2$player1, COLL05_Bg2$player2)
COLL05_Bft2 <- as.matrix(COLL05_Bft)
numRows <- nrow(COLL05_Bft2)
numCols <- ncol(COLL05_Bft2)
COLL05_Bft3 <- COLL05_Bft2[c(2:numRows) , c(2:numCols)]
COLL05_BTable <- graph.adjacency(COLL05_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(COLL05_BTable, vertex.label = V(COLL05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
COLL05_B.clusterCoef <- transitivity(COLL05_BTable, type="global") #cluster coefficient
COLL05_B.degreeCent <- centralization.degree(COLL05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_Bftn <- as.network.matrix(COLL05_Bft)
COLL05_B.netDensity <- network.density(COLL05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_B.entropy <- entropy(COLL05_Bft) #entropy

COLL05_B.netMx <- cbind(COLL05_B.netMx, COLL05_B.clusterCoef, COLL05_B.degreeCent$centralization,
                        COLL05_B.netDensity, COLL05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
COLL05_SFg2 <- data.frame(COLL05_SF)
COLL05_SFg2 <- COLL05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_SFg2$player1
player2vector <- COLL05_SFg2$player2
COLL05_SFg3 <- COLL05_SFg2
COLL05_SFg3$p1inp2vec <- is.element(COLL05_SFg3$player1, player2vector)
COLL05_SFg3$p2inp1vec <- is.element(COLL05_SFg3$player2, player1vector)

addPlayer1 <- COLL05_SFg3[ which(COLL05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_SFg3[ which(COLL05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_SFg2 <- rbind(COLL05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
COLL05_SFft <- ftable(COLL05_SFg2$player1, COLL05_SFg2$player2)
COLL05_SFft2 <- as.matrix(COLL05_SFft)
numRows <- nrow(COLL05_SFft2)
numCols <- ncol(COLL05_SFft2)
COLL05_SFft3 <- COLL05_SFft2[c(2:numRows) , c(2:numCols)]
COLL05_SFTable <- graph.adjacency(COLL05_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(COLL05_SFTable, vertex.label = V(COLL05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
COLL05_SF.clusterCoef <- transitivity(COLL05_SFTable, type="global") #cluster coefficient
COLL05_SF.degreeCent <- centralization.degree(COLL05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_SFftn <- as.network.matrix(COLL05_SFft)
COLL05_SF.netDensity <- network.density(COLL05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_SF.entropy <- entropy(COLL05_SFft) #entropy

COLL05_SF.netMx <- cbind(COLL05_SF.netMx, COLL05_SF.clusterCoef, COLL05_SF.degreeCent$centralization,
                         COLL05_SF.netDensity, COLL05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************

round = 5
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
COLL05_TFg2 <- data.frame(COLL05_TF)
COLL05_TFg2 <- COLL05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_TFg2$player1
player2vector <- COLL05_TFg2$player2
COLL05_TFg3 <- COLL05_TFg2
COLL05_TFg3$p1inp2vec <- is.element(COLL05_TFg3$player1, player2vector)
COLL05_TFg3$p2inp1vec <- is.element(COLL05_TFg3$player2, player1vector)

addPlayer1 <- COLL05_TFg3[ which(COLL05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL05_TFg3[ which(COLL05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_TFg2 <- rbind(COLL05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
COLL05_TFft <- ftable(COLL05_TFg2$player1, COLL05_TFg2$player2)
COLL05_TFft2 <- as.matrix(COLL05_TFft)
numRows <- nrow(COLL05_TFft2)
numCols <- ncol(COLL05_TFft2)
COLL05_TFft3 <- COLL05_TFft2[c(2:numRows) , c(2:numCols)]
COLL05_TFTable <- graph.adjacency(COLL05_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(COLL05_TFTable, vertex.label = V(COLL05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
COLL05_TF.clusterCoef <- transitivity(COLL05_TFTable, type="global") #cluster coefficient
COLL05_TF.degreeCent <- centralization.degree(COLL05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_TFftn <- as.network.matrix(COLL05_TFft)
COLL05_TF.netDensity <- network.density(COLL05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_TF.entropy <- entropy(COLL05_TFft) #entropy

COLL05_TF.netMx <- cbind(COLL05_TF.netMx, COLL05_TF.clusterCoef, COLL05_TF.degreeCent$centralization,
                         COLL05_TF.netDensity, COLL05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************
#NA

round = 5
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
COLL05_SAMg2 <- data.frame(COLL05_SAM)
COLL05_SAMg2 <- COLL05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_SAMg2$player1
player2vector <- COLL05_SAMg2$player2
COLL05_SAMg3 <- COLL05_SAMg2
COLL05_SAMg3$p1inp2vec <- is.element(COLL05_SAMg3$player1, player2vector)
COLL05_SAMg3$p2inp1vec <- is.element(COLL05_SAMg3$player2, player1vector)

addPlayer1 <- COLL05_SAMg3[ which(COLL05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_SAMg3[ which(COLL05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_SAMg2 <- rbind(COLL05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
COLL05_SAMft <- ftable(COLL05_SAMg2$player1, COLL05_SAMg2$player2)
COLL05_SAMft2 <- as.matrix(COLL05_SAMft)
numRows <- nrow(COLL05_SAMft2)
numCols <- ncol(COLL05_SAMft2)
COLL05_SAMft3 <- COLL05_SAMft2[c(2:numRows) , c(2:numCols)]
COLL05_SAMTable <- graph.adjacency(COLL05_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(COLL05_SAMTable, vertex.label = V(COLL05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
COLL05_SAM.clusterCoef <- transitivity(COLL05_SAMTable, type="global") #cluster coefficient
COLL05_SAM.degreeCent <- centralization.degree(COLL05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_SAMftn <- as.network.matrix(COLL05_SAMft)
COLL05_SAM.netDensity <- network.density(COLL05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_SAM.entropy <- entropy(COLL05_SAMft) #entropy

COLL05_SAM.netMx <- cbind(COLL05_SAM.netMx, COLL05_SAM.clusterCoef, COLL05_SAM.degreeCent$centralization,
                          COLL05_SAM.netDensity, COLL05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
COLL05_TAMg2 <- data.frame(COLL05_TAM)
COLL05_TAMg2 <- COLL05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_TAMg2$player1
player2vector <- COLL05_TAMg2$player2
COLL05_TAMg3 <- COLL05_TAMg2
COLL05_TAMg3$p1inp2vec <- is.element(COLL05_TAMg3$player1, player2vector)
COLL05_TAMg3$p2inp1vec <- is.element(COLL05_TAMg3$player2, player1vector)

addPlayer1 <- COLL05_TAMg3[ which(COLL05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_TAMg3[ which(COLL05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_TAMg2 <- rbind(COLL05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
COLL05_TAMft <- ftable(COLL05_TAMg2$player1, COLL05_TAMg2$player2)
COLL05_TAMft2 <- as.matrix(COLL05_TAMft)
numRows <- nrow(COLL05_TAMft2)
numCols <- ncol(COLL05_TAMft2)
COLL05_TAMft3 <- COLL05_TAMft2[c(2:numRows) , c(2:numCols)]
COLL05_TAMTable <- graph.adjacency(COLL05_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(COLL05_TAMTable, vertex.label = V(COLL05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
COLL05_TAM.clusterCoef <- transitivity(COLL05_TAMTable, type="global") #cluster coefficient
COLL05_TAM.degreeCent <- centralization.degree(COLL05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_TAMftn <- as.network.matrix(COLL05_TAMft)
COLL05_TAM.netDensity <- network.density(COLL05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_TAM.entropy <- entropy(COLL05_TAMft) #entropy

COLL05_TAM.netMx <- cbind(COLL05_TAM.netMx, COLL05_TAM.clusterCoef, COLL05_TAM.degreeCent$centralization,
                          COLL05_TAM.netDensity, COLL05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
COLL05_SDMg2 <- data.frame(COLL05_SDM)
COLL05_SDMg2 <- COLL05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_SDMg2$player1
player2vector <- COLL05_SDMg2$player2
COLL05_SDMg3 <- COLL05_SDMg2
COLL05_SDMg3$p1inp2vec <- is.element(COLL05_SDMg3$player1, player2vector)
COLL05_SDMg3$p2inp1vec <- is.element(COLL05_SDMg3$player2, player1vector)

addPlayer1 <- COLL05_SDMg3[ which(COLL05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_SDMg3[ which(COLL05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_SDMg2 <- rbind(COLL05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
COLL05_SDMft <- ftable(COLL05_SDMg2$player1, COLL05_SDMg2$player2)
COLL05_SDMft2 <- as.matrix(COLL05_SDMft)
numRows <- nrow(COLL05_SDMft2)
numCols <- ncol(COLL05_SDMft2)
COLL05_SDMft3 <- COLL05_SDMft2[c(2:numRows) , c(2:numCols)]
COLL05_SDMTable <- graph.adjacency(COLL05_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(COLL05_SDMTable, vertex.label = V(COLL05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
COLL05_SDM.clusterCoef <- transitivity(COLL05_SDMTable, type="global") #cluster coefficient
COLL05_SDM.degreeCent <- centralization.degree(COLL05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_SDMftn <- as.network.matrix(COLL05_SDMft)
COLL05_SDM.netDensity <- network.density(COLL05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_SDM.entropy <- entropy(COLL05_SDMft) #entropy

COLL05_SDM.netMx <- cbind(COLL05_SDM.netMx, COLL05_SDM.clusterCoef, COLL05_SDM.degreeCent$centralization,
                          COLL05_SDM.netDensity, COLL05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
COLL05_TDMg2 <- data.frame(COLL05_TDM)
COLL05_TDMg2 <- COLL05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_TDMg2$player1
player2vector <- COLL05_TDMg2$player2
COLL05_TDMg3 <- COLL05_TDMg2
COLL05_TDMg3$p1inp2vec <- is.element(COLL05_TDMg3$player1, player2vector)
COLL05_TDMg3$p2inp1vec <- is.element(COLL05_TDMg3$player2, player1vector)

addPlayer1 <- COLL05_TDMg3[ which(COLL05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_TDMg3[ which(COLL05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_TDMg2 <- rbind(COLL05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
COLL05_TDMft <- ftable(COLL05_TDMg2$player1, COLL05_TDMg2$player2)
COLL05_TDMft2 <- as.matrix(COLL05_TDMft)
numRows <- nrow(COLL05_TDMft2)
numCols <- ncol(COLL05_TDMft2)
COLL05_TDMft3 <- COLL05_TDMft2[c(2:numRows) , c(2:numCols)]
COLL05_TDMTable <- graph.adjacency(COLL05_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(COLL05_TDMTable, vertex.label = V(COLL05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
COLL05_TDM.clusterCoef <- transitivity(COLL05_TDMTable, type="global") #cluster coefficient
COLL05_TDM.degreeCent <- centralization.degree(COLL05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_TDMftn <- as.network.matrix(COLL05_TDMft)
COLL05_TDM.netDensity <- network.density(COLL05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_TDM.entropy <- entropy(COLL05_TDMft) #entropy

COLL05_TDM.netMx <- cbind(COLL05_TDM.netMx, COLL05_TDM.clusterCoef, COLL05_TDM.degreeCent$centralization,
                          COLL05_TDM.netDensity, COLL05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
COLL05_SDg2 <- data.frame(COLL05_SD)
COLL05_SDg2 <- COLL05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_SDg2$player1
player2vector <- COLL05_SDg2$player2
COLL05_SDg3 <- COLL05_SDg2
COLL05_SDg3$p1inp2vec <- is.element(COLL05_SDg3$player1, player2vector)
COLL05_SDg3$p2inp1vec <- is.element(COLL05_SDg3$player2, player1vector)

addPlayer1 <- COLL05_SDg3[ which(COLL05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_SDg3[ which(COLL05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_SDg2 <- rbind(COLL05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
COLL05_SDft <- ftable(COLL05_SDg2$player1, COLL05_SDg2$player2)
COLL05_SDft2 <- as.matrix(COLL05_SDft)
numRows <- nrow(COLL05_SDft2)
numCols <- ncol(COLL05_SDft2)
COLL05_SDft3 <- COLL05_SDft2[c(2:numRows) , c(2:numCols)]
COLL05_SDTable <- graph.adjacency(COLL05_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(COLL05_SDTable, vertex.label = V(COLL05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
COLL05_SD.clusterCoef <- transitivity(COLL05_SDTable, type="global") #cluster coefficient
COLL05_SD.degreeCent <- centralization.degree(COLL05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_SDftn <- as.network.matrix(COLL05_SDft)
COLL05_SD.netDensity <- network.density(COLL05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_SD.entropy <- entropy(COLL05_SDft) #entropy

COLL05_SD.netMx <- cbind(COLL05_SD.netMx, COLL05_SD.clusterCoef, COLL05_SD.degreeCent$centralization,
                         COLL05_SD.netDensity, COLL05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
COLL05_TDg2 <- data.frame(COLL05_TD)
COLL05_TDg2 <- COLL05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_TDg2$player1
player2vector <- COLL05_TDg2$player2
COLL05_TDg3 <- COLL05_TDg2
COLL05_TDg3$p1inp2vec <- is.element(COLL05_TDg3$player1, player2vector)
COLL05_TDg3$p2inp1vec <- is.element(COLL05_TDg3$player2, player1vector)

addPlayer1 <- COLL05_TDg3[ which(COLL05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_TDg3[ which(COLL05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_TDg2 <- rbind(COLL05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
COLL05_TDft <- ftable(COLL05_TDg2$player1, COLL05_TDg2$player2)
COLL05_TDft2 <- as.matrix(COLL05_TDft)
numRows <- nrow(COLL05_TDft2)
numCols <- ncol(COLL05_TDft2)
COLL05_TDft3 <- COLL05_TDft2[c(2:numRows) , c(2:numCols)]
COLL05_TDTable <- graph.adjacency(COLL05_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(COLL05_TDTable, vertex.label = V(COLL05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
COLL05_TD.clusterCoef <- transitivity(COLL05_TDTable, type="global") #cluster coefficient
COLL05_TD.degreeCent <- centralization.degree(COLL05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_TDftn <- as.network.matrix(COLL05_TDft)
COLL05_TD.netDensity <- network.density(COLL05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_TD.entropy <- entropy(COLL05_TDft) #entropy

COLL05_TD.netMx <- cbind(COLL05_TD.netMx, COLL05_TD.clusterCoef, COLL05_TD.degreeCent$centralization,
                         COLL05_TD.netDensity, COLL05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
COLL05_QTg2 <- data.frame(COLL05_QT)
COLL05_QTg2 <- COLL05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL05_QTg2$player1
player2vector <- COLL05_QTg2$player2
COLL05_QTg3 <- COLL05_QTg2
COLL05_QTg3$p1inp2vec <- is.element(COLL05_QTg3$player1, player2vector)
COLL05_QTg3$p2inp1vec <- is.element(COLL05_QTg3$player2, player1vector)

addPlayer1 <- COLL05_QTg3[ which(COLL05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL05_QTg3[ which(COLL05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL05_QTg2 <- rbind(COLL05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
COLL05_QTft <- ftable(COLL05_QTg2$player1, COLL05_QTg2$player2)
COLL05_QTft2 <- as.matrix(COLL05_QTft)
numRows <- nrow(COLL05_QTft2)
numCols <- ncol(COLL05_QTft2)
COLL05_QTft3 <- COLL05_QTft2[c(2:numRows) , c(2:numCols)]
COLL05_QTTable <- graph.adjacency(COLL05_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(COLL05_QTTable, vertex.label = V(COLL05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
COLL05_QT.clusterCoef <- transitivity(COLL05_QTTable, type="global") #cluster coefficient
COLL05_QT.degreeCent <- centralization.degree(COLL05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL05_QTftn <- as.network.matrix(COLL05_QTft)
COLL05_QT.netDensity <- network.density(COLL05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL05_QT.entropy <- entropy(COLL05_QTft) #entropy

COLL05_QT.netMx <- cbind(COLL05_QT.netMx, COLL05_QT.clusterCoef, COLL05_QT.degreeCent$centralization,
                         COLL05_QT.netDensity, COLL05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL05_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "ESS"
KIoutcome = "Goal_F"
ESS05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
ESS05_Gg2 <- data.frame(ESS05_G)
ESS05_Gg2 <- ESS05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_Gg2$player1
player2vector <- ESS05_Gg2$player2
ESS05_Gg3 <- ESS05_Gg2
ESS05_Gg3$p1inp2vec <- is.element(ESS05_Gg3$player1, player2vector)
ESS05_Gg3$p2inp1vec <- is.element(ESS05_Gg3$player2, player1vector)

addPlayer1 <- ESS05_Gg3[ which(ESS05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS05_Gg3[ which(ESS05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_Gg2 <- rbind(ESS05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
ESS05_Gft <- ftable(ESS05_Gg2$player1, ESS05_Gg2$player2)
ESS05_Gft2 <- as.matrix(ESS05_Gft)
numRows <- nrow(ESS05_Gft2)
numCols <- ncol(ESS05_Gft2)
ESS05_Gft3 <- ESS05_Gft2[c(2:numRows) , c(2:numCols)]
ESS05_GTable <- graph.adjacency(ESS05_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(ESS05_GTable, vertex.label = V(ESS05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
ESS05_G.clusterCoef <- transitivity(ESS05_GTable, type="global") #cluster coefficient
ESS05_G.degreeCent <- centralization.degree(ESS05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_Gftn <- as.network.matrix(ESS05_Gft)
ESS05_G.netDensity <- network.density(ESS05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_G.entropy <- entropy(ESS05_Gft) #entropy

ESS05_G.netMx <- cbind(ESS05_G.netMx, ESS05_G.clusterCoef, ESS05_G.degreeCent$centralization,
                       ESS05_G.netDensity, ESS05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************
#NA

round = 5
teamName = "ESS"
KIoutcome = "Behind_F"
ESS05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
ESS05_Bg2 <- data.frame(ESS05_B)
ESS05_Bg2 <- ESS05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_Bg2$player1
player2vector <- ESS05_Bg2$player2
ESS05_Bg3 <- ESS05_Bg2
ESS05_Bg3$p1inp2vec <- is.element(ESS05_Bg3$player1, player2vector)
ESS05_Bg3$p2inp1vec <- is.element(ESS05_Bg3$player2, player1vector)

addPlayer1 <- ESS05_Bg3[ which(ESS05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_Bg3[ which(ESS05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_Bg2 <- rbind(ESS05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
ESS05_Bft <- ftable(ESS05_Bg2$player1, ESS05_Bg2$player2)
ESS05_Bft2 <- as.matrix(ESS05_Bft)
numRows <- nrow(ESS05_Bft2)
numCols <- ncol(ESS05_Bft2)
ESS05_Bft3 <- ESS05_Bft2[c(2:numRows) , c(2:numCols)]
ESS05_BTable <- graph.adjacency(ESS05_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(ESS05_BTable, vertex.label = V(ESS05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
ESS05_B.clusterCoef <- transitivity(ESS05_BTable, type="global") #cluster coefficient
ESS05_B.degreeCent <- centralization.degree(ESS05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_Bftn <- as.network.matrix(ESS05_Bft)
ESS05_B.netDensity <- network.density(ESS05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_B.entropy <- entropy(ESS05_Bft) #entropy

ESS05_B.netMx <- cbind(ESS05_B.netMx, ESS05_B.clusterCoef, ESS05_B.degreeCent$centralization,
                       ESS05_B.netDensity, ESS05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
ESS05_SFg2 <- data.frame(ESS05_SF)
ESS05_SFg2 <- ESS05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_SFg2$player1
player2vector <- ESS05_SFg2$player2
ESS05_SFg3 <- ESS05_SFg2
ESS05_SFg3$p1inp2vec <- is.element(ESS05_SFg3$player1, player2vector)
ESS05_SFg3$p2inp1vec <- is.element(ESS05_SFg3$player2, player1vector)

addPlayer1 <- ESS05_SFg3[ which(ESS05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_SFg3[ which(ESS05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_SFg2 <- rbind(ESS05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
ESS05_SFft <- ftable(ESS05_SFg2$player1, ESS05_SFg2$player2)
ESS05_SFft2 <- as.matrix(ESS05_SFft)
numRows <- nrow(ESS05_SFft2)
numCols <- ncol(ESS05_SFft2)
ESS05_SFft3 <- ESS05_SFft2[c(2:numRows) , c(2:numCols)]
ESS05_SFTable <- graph.adjacency(ESS05_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(ESS05_SFTable, vertex.label = V(ESS05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
ESS05_SF.clusterCoef <- transitivity(ESS05_SFTable, type="global") #cluster coefficient
ESS05_SF.degreeCent <- centralization.degree(ESS05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_SFftn <- as.network.matrix(ESS05_SFft)
ESS05_SF.netDensity <- network.density(ESS05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_SF.entropy <- entropy(ESS05_SFft) #entropy

ESS05_SF.netMx <- cbind(ESS05_SF.netMx, ESS05_SF.clusterCoef, ESS05_SF.degreeCent$centralization,
                        ESS05_SF.netDensity, ESS05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************
#NA

round = 5
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
ESS05_TFg2 <- data.frame(ESS05_TF)
ESS05_TFg2 <- ESS05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_TFg2$player1
player2vector <- ESS05_TFg2$player2
ESS05_TFg3 <- ESS05_TFg2
ESS05_TFg3$p1inp2vec <- is.element(ESS05_TFg3$player1, player2vector)
ESS05_TFg3$p2inp1vec <- is.element(ESS05_TFg3$player2, player1vector)

addPlayer1 <- ESS05_TFg3[ which(ESS05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_TFg3[ which(ESS05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_TFg2 <- rbind(ESS05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
ESS05_TFft <- ftable(ESS05_TFg2$player1, ESS05_TFg2$player2)
ESS05_TFft2 <- as.matrix(ESS05_TFft)
numRows <- nrow(ESS05_TFft2)
numCols <- ncol(ESS05_TFft2)
ESS05_TFft3 <- ESS05_TFft2[c(2:numRows) , c(2:numCols)]
ESS05_TFTable <- graph.adjacency(ESS05_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(ESS05_TFTable, vertex.label = V(ESS05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
ESS05_TF.clusterCoef <- transitivity(ESS05_TFTable, type="global") #cluster coefficient
ESS05_TF.degreeCent <- centralization.degree(ESS05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_TFftn <- as.network.matrix(ESS05_TFft)
ESS05_TF.netDensity <- network.density(ESS05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_TF.entropy <- entropy(ESS05_TFft) #entropy

ESS05_TF.netMx <- cbind(ESS05_TF.netMx, ESS05_TF.clusterCoef, ESS05_TF.degreeCent$centralization,
                        ESS05_TF.netDensity, ESS05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
ESS05_SAMg2 <- data.frame(ESS05_SAM)
ESS05_SAMg2 <- ESS05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_SAMg2$player1
player2vector <- ESS05_SAMg2$player2
ESS05_SAMg3 <- ESS05_SAMg2
ESS05_SAMg3$p1inp2vec <- is.element(ESS05_SAMg3$player1, player2vector)
ESS05_SAMg3$p2inp1vec <- is.element(ESS05_SAMg3$player2, player1vector)

addPlayer1 <- ESS05_SAMg3[ which(ESS05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_SAMg3[ which(ESS05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_SAMg2 <- rbind(ESS05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
ESS05_SAMft <- ftable(ESS05_SAMg2$player1, ESS05_SAMg2$player2)
ESS05_SAMft2 <- as.matrix(ESS05_SAMft)
numRows <- nrow(ESS05_SAMft2)
numCols <- ncol(ESS05_SAMft2)
ESS05_SAMft3 <- ESS05_SAMft2[c(2:numRows) , c(2:numCols)]
ESS05_SAMTable <- graph.adjacency(ESS05_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(ESS05_SAMTable, vertex.label = V(ESS05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
ESS05_SAM.clusterCoef <- transitivity(ESS05_SAMTable, type="global") #cluster coefficient
ESS05_SAM.degreeCent <- centralization.degree(ESS05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_SAMftn <- as.network.matrix(ESS05_SAMft)
ESS05_SAM.netDensity <- network.density(ESS05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_SAM.entropy <- entropy(ESS05_SAMft) #entropy

ESS05_SAM.netMx <- cbind(ESS05_SAM.netMx, ESS05_SAM.clusterCoef, ESS05_SAM.degreeCent$centralization,
                         ESS05_SAM.netDensity, ESS05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
ESS05_TAMg2 <- data.frame(ESS05_TAM)
ESS05_TAMg2 <- ESS05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_TAMg2$player1
player2vector <- ESS05_TAMg2$player2
ESS05_TAMg3 <- ESS05_TAMg2
ESS05_TAMg3$p1inp2vec <- is.element(ESS05_TAMg3$player1, player2vector)
ESS05_TAMg3$p2inp1vec <- is.element(ESS05_TAMg3$player2, player1vector)

addPlayer1 <- ESS05_TAMg3[ which(ESS05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_TAMg3[ which(ESS05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_TAMg2 <- rbind(ESS05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
ESS05_TAMft <- ftable(ESS05_TAMg2$player1, ESS05_TAMg2$player2)
ESS05_TAMft2 <- as.matrix(ESS05_TAMft)
numRows <- nrow(ESS05_TAMft2)
numCols <- ncol(ESS05_TAMft2)
ESS05_TAMft3 <- ESS05_TAMft2[c(2:numRows) , c(2:numCols)]
ESS05_TAMTable <- graph.adjacency(ESS05_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(ESS05_TAMTable, vertex.label = V(ESS05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
ESS05_TAM.clusterCoef <- transitivity(ESS05_TAMTable, type="global") #cluster coefficient
ESS05_TAM.degreeCent <- centralization.degree(ESS05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_TAMftn <- as.network.matrix(ESS05_TAMft)
ESS05_TAM.netDensity <- network.density(ESS05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_TAM.entropy <- entropy(ESS05_TAMft) #entropy

ESS05_TAM.netMx <- cbind(ESS05_TAM.netMx, ESS05_TAM.clusterCoef, ESS05_TAM.degreeCent$centralization,
                         ESS05_TAM.netDensity, ESS05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
ESS05_SDMg2 <- data.frame(ESS05_SDM)
ESS05_SDMg2 <- ESS05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_SDMg2$player1
player2vector <- ESS05_SDMg2$player2
ESS05_SDMg3 <- ESS05_SDMg2
ESS05_SDMg3$p1inp2vec <- is.element(ESS05_SDMg3$player1, player2vector)
ESS05_SDMg3$p2inp1vec <- is.element(ESS05_SDMg3$player2, player1vector)

addPlayer1 <- ESS05_SDMg3[ which(ESS05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_SDMg3[ which(ESS05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_SDMg2 <- rbind(ESS05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
ESS05_SDMft <- ftable(ESS05_SDMg2$player1, ESS05_SDMg2$player2)
ESS05_SDMft2 <- as.matrix(ESS05_SDMft)
numRows <- nrow(ESS05_SDMft2)
numCols <- ncol(ESS05_SDMft2)
ESS05_SDMft3 <- ESS05_SDMft2[c(2:numRows) , c(2:numCols)]
ESS05_SDMTable <- graph.adjacency(ESS05_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(ESS05_SDMTable, vertex.label = V(ESS05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
ESS05_SDM.clusterCoef <- transitivity(ESS05_SDMTable, type="global") #cluster coefficient
ESS05_SDM.degreeCent <- centralization.degree(ESS05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_SDMftn <- as.network.matrix(ESS05_SDMft)
ESS05_SDM.netDensity <- network.density(ESS05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_SDM.entropy <- entropy(ESS05_SDMft) #entropy

ESS05_SDM.netMx <- cbind(ESS05_SDM.netMx, ESS05_SDM.clusterCoef, ESS05_SDM.degreeCent$centralization,
                         ESS05_SDM.netDensity, ESS05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
ESS05_TDMg2 <- data.frame(ESS05_TDM)
ESS05_TDMg2 <- ESS05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_TDMg2$player1
player2vector <- ESS05_TDMg2$player2
ESS05_TDMg3 <- ESS05_TDMg2
ESS05_TDMg3$p1inp2vec <- is.element(ESS05_TDMg3$player1, player2vector)
ESS05_TDMg3$p2inp1vec <- is.element(ESS05_TDMg3$player2, player1vector)

addPlayer1 <- ESS05_TDMg3[ which(ESS05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_TDMg3[ which(ESS05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_TDMg2 <- rbind(ESS05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
ESS05_TDMft <- ftable(ESS05_TDMg2$player1, ESS05_TDMg2$player2)
ESS05_TDMft2 <- as.matrix(ESS05_TDMft)
numRows <- nrow(ESS05_TDMft2)
numCols <- ncol(ESS05_TDMft2)
ESS05_TDMft3 <- ESS05_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS05_TDMTable <- graph.adjacency(ESS05_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(ESS05_TDMTable, vertex.label = V(ESS05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
ESS05_TDM.clusterCoef <- transitivity(ESS05_TDMTable, type="global") #cluster coefficient
ESS05_TDM.degreeCent <- centralization.degree(ESS05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_TDMftn <- as.network.matrix(ESS05_TDMft)
ESS05_TDM.netDensity <- network.density(ESS05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_TDM.entropy <- entropy(ESS05_TDMft) #entropy

ESS05_TDM.netMx <- cbind(ESS05_TDM.netMx, ESS05_TDM.clusterCoef, ESS05_TDM.degreeCent$centralization,
                         ESS05_TDM.netDensity, ESS05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
ESS05_SDg2 <- data.frame(ESS05_SD)
ESS05_SDg2 <- ESS05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_SDg2$player1
player2vector <- ESS05_SDg2$player2
ESS05_SDg3 <- ESS05_SDg2
ESS05_SDg3$p1inp2vec <- is.element(ESS05_SDg3$player1, player2vector)
ESS05_SDg3$p2inp1vec <- is.element(ESS05_SDg3$player2, player1vector)

addPlayer1 <- ESS05_SDg3[ which(ESS05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_SDg3[ which(ESS05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_SDg2 <- rbind(ESS05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
ESS05_SDft <- ftable(ESS05_SDg2$player1, ESS05_SDg2$player2)
ESS05_SDft2 <- as.matrix(ESS05_SDft)
numRows <- nrow(ESS05_SDft2)
numCols <- ncol(ESS05_SDft2)
ESS05_SDft3 <- ESS05_SDft2[c(2:numRows) , c(2:numCols)]
ESS05_SDTable <- graph.adjacency(ESS05_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(ESS05_SDTable, vertex.label = V(ESS05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
ESS05_SD.clusterCoef <- transitivity(ESS05_SDTable, type="global") #cluster coefficient
ESS05_SD.degreeCent <- centralization.degree(ESS05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_SDftn <- as.network.matrix(ESS05_SDft)
ESS05_SD.netDensity <- network.density(ESS05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_SD.entropy <- entropy(ESS05_SDft) #entropy

ESS05_SD.netMx <- cbind(ESS05_SD.netMx, ESS05_SD.clusterCoef, ESS05_SD.degreeCent$centralization,
                        ESS05_SD.netDensity, ESS05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************

round = 5
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
ESS05_TDg2 <- data.frame(ESS05_TD)
ESS05_TDg2 <- ESS05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_TDg2$player1
player2vector <- ESS05_TDg2$player2
ESS05_TDg3 <- ESS05_TDg2
ESS05_TDg3$p1inp2vec <- is.element(ESS05_TDg3$player1, player2vector)
ESS05_TDg3$p2inp1vec <- is.element(ESS05_TDg3$player2, player1vector)

addPlayer1 <- ESS05_TDg3[ which(ESS05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_TDg3[ which(ESS05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_TDg2 <- rbind(ESS05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
ESS05_TDft <- ftable(ESS05_TDg2$player1, ESS05_TDg2$player2)
ESS05_TDft2 <- as.matrix(ESS05_TDft)
numRows <- nrow(ESS05_TDft2)
numCols <- ncol(ESS05_TDft2)
ESS05_TDft3 <- ESS05_TDft2[c(2:numRows) , c(2:numCols)]
ESS05_TDTable <- graph.adjacency(ESS05_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(ESS05_TDTable, vertex.label = V(ESS05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
ESS05_TD.clusterCoef <- transitivity(ESS05_TDTable, type="global") #cluster coefficient
ESS05_TD.degreeCent <- centralization.degree(ESS05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_TDftn <- as.network.matrix(ESS05_TDft)
ESS05_TD.netDensity <- network.density(ESS05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_TD.entropy <- entropy(ESS05_TDft) #entropy

ESS05_TD.netMx <- cbind(ESS05_TD.netMx, ESS05_TD.clusterCoef, ESS05_TD.degreeCent$centralization,
                        ESS05_TD.netDensity, ESS05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
ESS05_QTg2 <- data.frame(ESS05_QT)
ESS05_QTg2 <- ESS05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS05_QTg2$player1
player2vector <- ESS05_QTg2$player2
ESS05_QTg3 <- ESS05_QTg2
ESS05_QTg3$p1inp2vec <- is.element(ESS05_QTg3$player1, player2vector)
ESS05_QTg3$p2inp1vec <- is.element(ESS05_QTg3$player2, player1vector)

addPlayer1 <- ESS05_QTg3[ which(ESS05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS05_QTg3[ which(ESS05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS05_QTg2 <- rbind(ESS05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
ESS05_QTft <- ftable(ESS05_QTg2$player1, ESS05_QTg2$player2)
ESS05_QTft2 <- as.matrix(ESS05_QTft)
numRows <- nrow(ESS05_QTft2)
numCols <- ncol(ESS05_QTft2)
ESS05_QTft3 <- ESS05_QTft2[c(2:numRows) , c(2:numCols)]
ESS05_QTTable <- graph.adjacency(ESS05_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(ESS05_QTTable, vertex.label = V(ESS05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
ESS05_QT.clusterCoef <- transitivity(ESS05_QTTable, type="global") #cluster coefficient
ESS05_QT.degreeCent <- centralization.degree(ESS05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS05_QTftn <- as.network.matrix(ESS05_QTft)
ESS05_QT.netDensity <- network.density(ESS05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS05_QT.entropy <- entropy(ESS05_QTft) #entropy

ESS05_QT.netMx <- cbind(ESS05_QT.netMx, ESS05_QT.clusterCoef, ESS05_QT.degreeCent$centralization,
                        ESS05_QT.netDensity, ESS05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS05_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "FRE"
KIoutcome = "Goal_F"
FRE05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
FRE05_Gg2 <- data.frame(FRE05_G)
FRE05_Gg2 <- FRE05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_Gg2$player1
player2vector <- FRE05_Gg2$player2
FRE05_Gg3 <- FRE05_Gg2
FRE05_Gg3$p1inp2vec <- is.element(FRE05_Gg3$player1, player2vector)
FRE05_Gg3$p2inp1vec <- is.element(FRE05_Gg3$player2, player1vector)

addPlayer1 <- FRE05_Gg3[ which(FRE05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE05_Gg3[ which(FRE05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_Gg2 <- rbind(FRE05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
FRE05_Gft <- ftable(FRE05_Gg2$player1, FRE05_Gg2$player2)
FRE05_Gft2 <- as.matrix(FRE05_Gft)
numRows <- nrow(FRE05_Gft2)
numCols <- ncol(FRE05_Gft2)
FRE05_Gft3 <- FRE05_Gft2[c(2:numRows) , c(2:numCols)]
FRE05_GTable <- graph.adjacency(FRE05_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(FRE05_GTable, vertex.label = V(FRE05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
FRE05_G.clusterCoef <- transitivity(FRE05_GTable, type="global") #cluster coefficient
FRE05_G.degreeCent <- centralization.degree(FRE05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_Gftn <- as.network.matrix(FRE05_Gft)
FRE05_G.netDensity <- network.density(FRE05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_G.entropy <- entropy(FRE05_Gft) #entropy

FRE05_G.netMx <- cbind(FRE05_G.netMx, FRE05_G.clusterCoef, FRE05_G.degreeCent$centralization,
                       FRE05_G.netDensity, FRE05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************

round = 5
teamName = "FRE"
KIoutcome = "Behind_F"
FRE05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
FRE05_Bg2 <- data.frame(FRE05_B)
FRE05_Bg2 <- FRE05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_Bg2$player1
player2vector <- FRE05_Bg2$player2
FRE05_Bg3 <- FRE05_Bg2
FRE05_Bg3$p1inp2vec <- is.element(FRE05_Bg3$player1, player2vector)
FRE05_Bg3$p2inp1vec <- is.element(FRE05_Bg3$player2, player1vector)

addPlayer1 <- FRE05_Bg3[ which(FRE05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE05_Bg3[ which(FRE05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_Bg2 <- rbind(FRE05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
FRE05_Bft <- ftable(FRE05_Bg2$player1, FRE05_Bg2$player2)
FRE05_Bft2 <- as.matrix(FRE05_Bft)
numRows <- nrow(FRE05_Bft2)
numCols <- ncol(FRE05_Bft2)
FRE05_Bft3 <- FRE05_Bft2[c(2:numRows) , c(2:numCols)]
FRE05_BTable <- graph.adjacency(FRE05_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(FRE05_BTable, vertex.label = V(FRE05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
FRE05_B.clusterCoef <- transitivity(FRE05_BTable, type="global") #cluster coefficient
FRE05_B.degreeCent <- centralization.degree(FRE05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_Bftn <- as.network.matrix(FRE05_Bft)
FRE05_B.netDensity <- network.density(FRE05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_B.entropy <- entropy(FRE05_Bft) #entropy

FRE05_B.netMx <- cbind(FRE05_B.netMx, FRE05_B.clusterCoef, FRE05_B.degreeCent$centralization,
                       FRE05_B.netDensity, FRE05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
FRE05_SFg2 <- data.frame(FRE05_SF)
FRE05_SFg2 <- FRE05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_SFg2$player1
player2vector <- FRE05_SFg2$player2
FRE05_SFg3 <- FRE05_SFg2
FRE05_SFg3$p1inp2vec <- is.element(FRE05_SFg3$player1, player2vector)
FRE05_SFg3$p2inp1vec <- is.element(FRE05_SFg3$player2, player1vector)

addPlayer1 <- FRE05_SFg3[ which(FRE05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE05_SFg3[ which(FRE05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_SFg2 <- rbind(FRE05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
FRE05_SFft <- ftable(FRE05_SFg2$player1, FRE05_SFg2$player2)
FRE05_SFft2 <- as.matrix(FRE05_SFft)
numRows <- nrow(FRE05_SFft2)
numCols <- ncol(FRE05_SFft2)
FRE05_SFft3 <- FRE05_SFft2[c(2:numRows) , c(2:numCols)]
FRE05_SFTable <- graph.adjacency(FRE05_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(FRE05_SFTable, vertex.label = V(FRE05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
FRE05_SF.clusterCoef <- transitivity(FRE05_SFTable, type="global") #cluster coefficient
FRE05_SF.degreeCent <- centralization.degree(FRE05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_SFftn <- as.network.matrix(FRE05_SFft)
FRE05_SF.netDensity <- network.density(FRE05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_SF.entropy <- entropy(FRE05_SFft) #entropy

FRE05_SF.netMx <- cbind(FRE05_SF.netMx, FRE05_SF.clusterCoef, FRE05_SF.degreeCent$centralization,
                        FRE05_SF.netDensity, FRE05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************

round = 5
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
FRE05_TFg2 <- data.frame(FRE05_TF)
FRE05_TFg2 <- FRE05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_TFg2$player1
player2vector <- FRE05_TFg2$player2
FRE05_TFg3 <- FRE05_TFg2
FRE05_TFg3$p1inp2vec <- is.element(FRE05_TFg3$player1, player2vector)
FRE05_TFg3$p2inp1vec <- is.element(FRE05_TFg3$player2, player1vector)

addPlayer1 <- FRE05_TFg3[ which(FRE05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE05_TFg3[ which(FRE05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_TFg2 <- rbind(FRE05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
FRE05_TFft <- ftable(FRE05_TFg2$player1, FRE05_TFg2$player2)
FRE05_TFft2 <- as.matrix(FRE05_TFft)
numRows <- nrow(FRE05_TFft2)
numCols <- ncol(FRE05_TFft2)
FRE05_TFft3 <- FRE05_TFft2[c(2:numRows) , c(2:numCols)]
FRE05_TFTable <- graph.adjacency(FRE05_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(FRE05_TFTable, vertex.label = V(FRE05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
FRE05_TF.clusterCoef <- transitivity(FRE05_TFTable, type="global") #cluster coefficient
FRE05_TF.degreeCent <- centralization.degree(FRE05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_TFftn <- as.network.matrix(FRE05_TFft)
FRE05_TF.netDensity <- network.density(FRE05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_TF.entropy <- entropy(FRE05_TFft) #entropy

FRE05_TF.netMx <- cbind(FRE05_TF.netMx, FRE05_TF.clusterCoef, FRE05_TF.degreeCent$centralization,
                        FRE05_TF.netDensity, FRE05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
FRE05_SAMg2 <- data.frame(FRE05_SAM)
FRE05_SAMg2 <- FRE05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_SAMg2$player1
player2vector <- FRE05_SAMg2$player2
FRE05_SAMg3 <- FRE05_SAMg2
FRE05_SAMg3$p1inp2vec <- is.element(FRE05_SAMg3$player1, player2vector)
FRE05_SAMg3$p2inp1vec <- is.element(FRE05_SAMg3$player2, player1vector)

addPlayer1 <- FRE05_SAMg3[ which(FRE05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE05_SAMg3[ which(FRE05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_SAMg2 <- rbind(FRE05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
FRE05_SAMft <- ftable(FRE05_SAMg2$player1, FRE05_SAMg2$player2)
FRE05_SAMft2 <- as.matrix(FRE05_SAMft)
numRows <- nrow(FRE05_SAMft2)
numCols <- ncol(FRE05_SAMft2)
FRE05_SAMft3 <- FRE05_SAMft2[c(2:numRows) , c(2:numCols)]
FRE05_SAMTable <- graph.adjacency(FRE05_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(FRE05_SAMTable, vertex.label = V(FRE05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
FRE05_SAM.clusterCoef <- transitivity(FRE05_SAMTable, type="global") #cluster coefficient
FRE05_SAM.degreeCent <- centralization.degree(FRE05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_SAMftn <- as.network.matrix(FRE05_SAMft)
FRE05_SAM.netDensity <- network.density(FRE05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_SAM.entropy <- entropy(FRE05_SAMft) #entropy

FRE05_SAM.netMx <- cbind(FRE05_SAM.netMx, FRE05_SAM.clusterCoef, FRE05_SAM.degreeCent$centralization,
                         FRE05_SAM.netDensity, FRE05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
FRE05_TAMg2 <- data.frame(FRE05_TAM)
FRE05_TAMg2 <- FRE05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_TAMg2$player1
player2vector <- FRE05_TAMg2$player2
FRE05_TAMg3 <- FRE05_TAMg2
FRE05_TAMg3$p1inp2vec <- is.element(FRE05_TAMg3$player1, player2vector)
FRE05_TAMg3$p2inp1vec <- is.element(FRE05_TAMg3$player2, player1vector)

addPlayer1 <- FRE05_TAMg3[ which(FRE05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE05_TAMg3[ which(FRE05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_TAMg2 <- rbind(FRE05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
FRE05_TAMft <- ftable(FRE05_TAMg2$player1, FRE05_TAMg2$player2)
FRE05_TAMft2 <- as.matrix(FRE05_TAMft)
numRows <- nrow(FRE05_TAMft2)
numCols <- ncol(FRE05_TAMft2)
FRE05_TAMft3 <- FRE05_TAMft2[c(2:numRows) , c(2:numCols)]
FRE05_TAMTable <- graph.adjacency(FRE05_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(FRE05_TAMTable, vertex.label = V(FRE05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
FRE05_TAM.clusterCoef <- transitivity(FRE05_TAMTable, type="global") #cluster coefficient
FRE05_TAM.degreeCent <- centralization.degree(FRE05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_TAMftn <- as.network.matrix(FRE05_TAMft)
FRE05_TAM.netDensity <- network.density(FRE05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_TAM.entropy <- entropy(FRE05_TAMft) #entropy

FRE05_TAM.netMx <- cbind(FRE05_TAM.netMx, FRE05_TAM.clusterCoef, FRE05_TAM.degreeCent$centralization,
                         FRE05_TAM.netDensity, FRE05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************
#NA

round = 5
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
FRE05_SDMg2 <- data.frame(FRE05_SDM)
FRE05_SDMg2 <- FRE05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_SDMg2$player1
player2vector <- FRE05_SDMg2$player2
FRE05_SDMg3 <- FRE05_SDMg2
FRE05_SDMg3$p1inp2vec <- is.element(FRE05_SDMg3$player1, player2vector)
FRE05_SDMg3$p2inp1vec <- is.element(FRE05_SDMg3$player2, player1vector)

addPlayer1 <- FRE05_SDMg3[ which(FRE05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE05_SDMg3[ which(FRE05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_SDMg2 <- rbind(FRE05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
FRE05_SDMft <- ftable(FRE05_SDMg2$player1, FRE05_SDMg2$player2)
FRE05_SDMft2 <- as.matrix(FRE05_SDMft)
numRows <- nrow(FRE05_SDMft2)
numCols <- ncol(FRE05_SDMft2)
FRE05_SDMft3 <- FRE05_SDMft2[c(2:numRows) , c(2:numCols)]
FRE05_SDMTable <- graph.adjacency(FRE05_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(FRE05_SDMTable, vertex.label = V(FRE05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
FRE05_SDM.clusterCoef <- transitivity(FRE05_SDMTable, type="global") #cluster coefficient
FRE05_SDM.degreeCent <- centralization.degree(FRE05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_SDMftn <- as.network.matrix(FRE05_SDMft)
FRE05_SDM.netDensity <- network.density(FRE05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_SDM.entropy <- entropy(FRE05_SDMft) #entropy

FRE05_SDM.netMx <- cbind(FRE05_SDM.netMx, FRE05_SDM.clusterCoef, FRE05_SDM.degreeCent$centralization,
                         FRE05_SDM.netDensity, FRE05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
FRE05_TDMg2 <- data.frame(FRE05_TDM)
FRE05_TDMg2 <- FRE05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_TDMg2$player1
player2vector <- FRE05_TDMg2$player2
FRE05_TDMg3 <- FRE05_TDMg2
FRE05_TDMg3$p1inp2vec <- is.element(FRE05_TDMg3$player1, player2vector)
FRE05_TDMg3$p2inp1vec <- is.element(FRE05_TDMg3$player2, player1vector)

addPlayer1 <- FRE05_TDMg3[ which(FRE05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE05_TDMg3[ which(FRE05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_TDMg2 <- rbind(FRE05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
FRE05_TDMft <- ftable(FRE05_TDMg2$player1, FRE05_TDMg2$player2)
FRE05_TDMft2 <- as.matrix(FRE05_TDMft)
numRows <- nrow(FRE05_TDMft2)
numCols <- ncol(FRE05_TDMft2)
FRE05_TDMft3 <- FRE05_TDMft2[c(2:numRows) , c(2:numCols)]
FRE05_TDMTable <- graph.adjacency(FRE05_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(FRE05_TDMTable, vertex.label = V(FRE05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
FRE05_TDM.clusterCoef <- transitivity(FRE05_TDMTable, type="global") #cluster coefficient
FRE05_TDM.degreeCent <- centralization.degree(FRE05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_TDMftn <- as.network.matrix(FRE05_TDMft)
FRE05_TDM.netDensity <- network.density(FRE05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_TDM.entropy <- entropy(FRE05_TDMft) #entropy

FRE05_TDM.netMx <- cbind(FRE05_TDM.netMx, FRE05_TDM.clusterCoef, FRE05_TDM.degreeCent$centralization,
                         FRE05_TDM.netDensity, FRE05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
FRE05_SDg2 <- data.frame(FRE05_SD)
FRE05_SDg2 <- FRE05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_SDg2$player1
player2vector <- FRE05_SDg2$player2
FRE05_SDg3 <- FRE05_SDg2
FRE05_SDg3$p1inp2vec <- is.element(FRE05_SDg3$player1, player2vector)
FRE05_SDg3$p2inp1vec <- is.element(FRE05_SDg3$player2, player1vector)

addPlayer1 <- FRE05_SDg3[ which(FRE05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE05_SDg3[ which(FRE05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_SDg2 <- rbind(FRE05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
FRE05_SDft <- ftable(FRE05_SDg2$player1, FRE05_SDg2$player2)
FRE05_SDft2 <- as.matrix(FRE05_SDft)
numRows <- nrow(FRE05_SDft2)
numCols <- ncol(FRE05_SDft2)
FRE05_SDft3 <- FRE05_SDft2[c(2:numRows) , c(2:numCols)]
FRE05_SDTable <- graph.adjacency(FRE05_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(FRE05_SDTable, vertex.label = V(FRE05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
FRE05_SD.clusterCoef <- transitivity(FRE05_SDTable, type="global") #cluster coefficient
FRE05_SD.degreeCent <- centralization.degree(FRE05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_SDftn <- as.network.matrix(FRE05_SDft)
FRE05_SD.netDensity <- network.density(FRE05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_SD.entropy <- entropy(FRE05_SDft) #entropy

FRE05_SD.netMx <- cbind(FRE05_SD.netMx, FRE05_SD.clusterCoef, FRE05_SD.degreeCent$centralization,
                        FRE05_SD.netDensity, FRE05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
FRE05_TDg2 <- data.frame(FRE05_TD)
FRE05_TDg2 <- FRE05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_TDg2$player1
player2vector <- FRE05_TDg2$player2
FRE05_TDg3 <- FRE05_TDg2
FRE05_TDg3$p1inp2vec <- is.element(FRE05_TDg3$player1, player2vector)
FRE05_TDg3$p2inp1vec <- is.element(FRE05_TDg3$player2, player1vector)

addPlayer1 <- FRE05_TDg3[ which(FRE05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE05_TDg3[ which(FRE05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_TDg2 <- rbind(FRE05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
FRE05_TDft <- ftable(FRE05_TDg2$player1, FRE05_TDg2$player2)
FRE05_TDft2 <- as.matrix(FRE05_TDft)
numRows <- nrow(FRE05_TDft2)
numCols <- ncol(FRE05_TDft2)
FRE05_TDft3 <- FRE05_TDft2[c(2:numRows) , c(2:numCols)]
FRE05_TDTable <- graph.adjacency(FRE05_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(FRE05_TDTable, vertex.label = V(FRE05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
FRE05_TD.clusterCoef <- transitivity(FRE05_TDTable, type="global") #cluster coefficient
FRE05_TD.degreeCent <- centralization.degree(FRE05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_TDftn <- as.network.matrix(FRE05_TDft)
FRE05_TD.netDensity <- network.density(FRE05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_TD.entropy <- entropy(FRE05_TDft) #entropy

FRE05_TD.netMx <- cbind(FRE05_TD.netMx, FRE05_TD.clusterCoef, FRE05_TD.degreeCent$centralization,
                        FRE05_TD.netDensity, FRE05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************

round = 5
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
FRE05_QTg2 <- data.frame(FRE05_QT)
FRE05_QTg2 <- FRE05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE05_QTg2$player1
player2vector <- FRE05_QTg2$player2
FRE05_QTg3 <- FRE05_QTg2
FRE05_QTg3$p1inp2vec <- is.element(FRE05_QTg3$player1, player2vector)
FRE05_QTg3$p2inp1vec <- is.element(FRE05_QTg3$player2, player1vector)

addPlayer1 <- FRE05_QTg3[ which(FRE05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE05_QTg3[ which(FRE05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE05_QTg2 <- rbind(FRE05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
FRE05_QTft <- ftable(FRE05_QTg2$player1, FRE05_QTg2$player2)
FRE05_QTft2 <- as.matrix(FRE05_QTft)
numRows <- nrow(FRE05_QTft2)
numCols <- ncol(FRE05_QTft2)
FRE05_QTft3 <- FRE05_QTft2[c(2:numRows) , c(2:numCols)]
FRE05_QTTable <- graph.adjacency(FRE05_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(FRE05_QTTable, vertex.label = V(FRE05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
FRE05_QT.clusterCoef <- transitivity(FRE05_QTTable, type="global") #cluster coefficient
FRE05_QT.degreeCent <- centralization.degree(FRE05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE05_QTftn <- as.network.matrix(FRE05_QTft)
FRE05_QT.netDensity <- network.density(FRE05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE05_QT.entropy <- entropy(FRE05_QTft) #entropy

FRE05_QT.netMx <- cbind(FRE05_QT.netMx, FRE05_QT.clusterCoef, FRE05_QT.degreeCent$centralization,
                        FRE05_QT.netDensity, FRE05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE05_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************
#NA

round = 5
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
GCFC05_Gg2 <- data.frame(GCFC05_G)
GCFC05_Gg2 <- GCFC05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_Gg2$player1
player2vector <- GCFC05_Gg2$player2
GCFC05_Gg3 <- GCFC05_Gg2
GCFC05_Gg3$p1inp2vec <- is.element(GCFC05_Gg3$player1, player2vector)
GCFC05_Gg3$p2inp1vec <- is.element(GCFC05_Gg3$player2, player1vector)

addPlayer1 <- GCFC05_Gg3[ which(GCFC05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC05_Gg3[ which(GCFC05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_Gg2 <- rbind(GCFC05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
GCFC05_Gft <- ftable(GCFC05_Gg2$player1, GCFC05_Gg2$player2)
GCFC05_Gft2 <- as.matrix(GCFC05_Gft)
numRows <- nrow(GCFC05_Gft2)
numCols <- ncol(GCFC05_Gft2)
GCFC05_Gft3 <- GCFC05_Gft2[c(2:numRows) , c(2:numCols)]
GCFC05_GTable <- graph.adjacency(GCFC05_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(GCFC05_GTable, vertex.label = V(GCFC05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
GCFC05_G.clusterCoef <- transitivity(GCFC05_GTable, type="global") #cluster coefficient
GCFC05_G.degreeCent <- centralization.degree(GCFC05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_Gftn <- as.network.matrix(GCFC05_Gft)
GCFC05_G.netDensity <- network.density(GCFC05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_G.entropy <- entropy(GCFC05_Gft) #entropy

GCFC05_G.netMx <- cbind(GCFC05_G.netMx, GCFC05_G.clusterCoef, GCFC05_G.degreeCent$centralization,
                        GCFC05_G.netDensity, GCFC05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************
#NA

round = 5
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
GCFC05_Bg2 <- data.frame(GCFC05_B)
GCFC05_Bg2 <- GCFC05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_Bg2$player1
player2vector <- GCFC05_Bg2$player2
GCFC05_Bg3 <- GCFC05_Bg2
GCFC05_Bg3$p1inp2vec <- is.element(GCFC05_Bg3$player1, player2vector)
GCFC05_Bg3$p2inp1vec <- is.element(GCFC05_Bg3$player2, player1vector)

addPlayer1 <- GCFC05_Bg3[ which(GCFC05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC05_Bg3[ which(GCFC05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_Bg2 <- rbind(GCFC05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
GCFC05_Bft <- ftable(GCFC05_Bg2$player1, GCFC05_Bg2$player2)
GCFC05_Bft2 <- as.matrix(GCFC05_Bft)
numRows <- nrow(GCFC05_Bft2)
numCols <- ncol(GCFC05_Bft2)
GCFC05_Bft3 <- GCFC05_Bft2[c(2:numRows) , c(2:numCols)]
GCFC05_BTable <- graph.adjacency(GCFC05_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(GCFC05_BTable, vertex.label = V(GCFC05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
GCFC05_B.clusterCoef <- transitivity(GCFC05_BTable, type="global") #cluster coefficient
GCFC05_B.degreeCent <- centralization.degree(GCFC05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_Bftn <- as.network.matrix(GCFC05_Bft)
GCFC05_B.netDensity <- network.density(GCFC05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_B.entropy <- entropy(GCFC05_Bft) #entropy

GCFC05_B.netMx <- cbind(GCFC05_B.netMx, GCFC05_B.clusterCoef, GCFC05_B.degreeCent$centralization,
                        GCFC05_B.netDensity, GCFC05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************

round = 5
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
GCFC05_SFg2 <- data.frame(GCFC05_SF)
GCFC05_SFg2 <- GCFC05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_SFg2$player1
player2vector <- GCFC05_SFg2$player2
GCFC05_SFg3 <- GCFC05_SFg2
GCFC05_SFg3$p1inp2vec <- is.element(GCFC05_SFg3$player1, player2vector)
GCFC05_SFg3$p2inp1vec <- is.element(GCFC05_SFg3$player2, player1vector)

addPlayer1 <- GCFC05_SFg3[ which(GCFC05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC05_SFg3[ which(GCFC05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_SFg2 <- rbind(GCFC05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
GCFC05_SFft <- ftable(GCFC05_SFg2$player1, GCFC05_SFg2$player2)
GCFC05_SFft2 <- as.matrix(GCFC05_SFft)
numRows <- nrow(GCFC05_SFft2)
numCols <- ncol(GCFC05_SFft2)
GCFC05_SFft3 <- GCFC05_SFft2[c(2:numRows) , c(2:numCols)]
GCFC05_SFTable <- graph.adjacency(GCFC05_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(GCFC05_SFTable, vertex.label = V(GCFC05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
GCFC05_SF.clusterCoef <- transitivity(GCFC05_SFTable, type="global") #cluster coefficient
GCFC05_SF.degreeCent <- centralization.degree(GCFC05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_SFftn <- as.network.matrix(GCFC05_SFft)
GCFC05_SF.netDensity <- network.density(GCFC05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_SF.entropy <- entropy(GCFC05_SFft) #entropy

GCFC05_SF.netMx <- cbind(GCFC05_SF.netMx, GCFC05_SF.clusterCoef, GCFC05_SF.degreeCent$centralization,
                         GCFC05_SF.netDensity, GCFC05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************

round = 5
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
GCFC05_TFg2 <- data.frame(GCFC05_TF)
GCFC05_TFg2 <- GCFC05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_TFg2$player1
player2vector <- GCFC05_TFg2$player2
GCFC05_TFg3 <- GCFC05_TFg2
GCFC05_TFg3$p1inp2vec <- is.element(GCFC05_TFg3$player1, player2vector)
GCFC05_TFg3$p2inp1vec <- is.element(GCFC05_TFg3$player2, player1vector)

addPlayer1 <- GCFC05_TFg3[ which(GCFC05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC05_TFg3[ which(GCFC05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_TFg2 <- rbind(GCFC05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
GCFC05_TFft <- ftable(GCFC05_TFg2$player1, GCFC05_TFg2$player2)
GCFC05_TFft2 <- as.matrix(GCFC05_TFft)
numRows <- nrow(GCFC05_TFft2)
numCols <- ncol(GCFC05_TFft2)
GCFC05_TFft3 <- GCFC05_TFft2[c(2:numRows) , c(2:numCols)]
GCFC05_TFTable <- graph.adjacency(GCFC05_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(GCFC05_TFTable, vertex.label = V(GCFC05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
GCFC05_TF.clusterCoef <- transitivity(GCFC05_TFTable, type="global") #cluster coefficient
GCFC05_TF.degreeCent <- centralization.degree(GCFC05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_TFftn <- as.network.matrix(GCFC05_TFft)
GCFC05_TF.netDensity <- network.density(GCFC05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_TF.entropy <- entropy(GCFC05_TFft) #entropy

GCFC05_TF.netMx <- cbind(GCFC05_TF.netMx, GCFC05_TF.clusterCoef, GCFC05_TF.degreeCent$centralization,
                         GCFC05_TF.netDensity, GCFC05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************
#NA

round = 5
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
GCFC05_SAMg2 <- data.frame(GCFC05_SAM)
GCFC05_SAMg2 <- GCFC05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_SAMg2$player1
player2vector <- GCFC05_SAMg2$player2
GCFC05_SAMg3 <- GCFC05_SAMg2
GCFC05_SAMg3$p1inp2vec <- is.element(GCFC05_SAMg3$player1, player2vector)
GCFC05_SAMg3$p2inp1vec <- is.element(GCFC05_SAMg3$player2, player1vector)

addPlayer1 <- GCFC05_SAMg3[ which(GCFC05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC05_SAMg3[ which(GCFC05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_SAMg2 <- rbind(GCFC05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
GCFC05_SAMft <- ftable(GCFC05_SAMg2$player1, GCFC05_SAMg2$player2)
GCFC05_SAMft2 <- as.matrix(GCFC05_SAMft)
numRows <- nrow(GCFC05_SAMft2)
numCols <- ncol(GCFC05_SAMft2)
GCFC05_SAMft3 <- GCFC05_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC05_SAMTable <- graph.adjacency(GCFC05_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(GCFC05_SAMTable, vertex.label = V(GCFC05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
GCFC05_SAM.clusterCoef <- transitivity(GCFC05_SAMTable, type="global") #cluster coefficient
GCFC05_SAM.degreeCent <- centralization.degree(GCFC05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_SAMftn <- as.network.matrix(GCFC05_SAMft)
GCFC05_SAM.netDensity <- network.density(GCFC05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_SAM.entropy <- entropy(GCFC05_SAMft) #entropy

GCFC05_SAM.netMx <- cbind(GCFC05_SAM.netMx, GCFC05_SAM.clusterCoef, GCFC05_SAM.degreeCent$centralization,
                          GCFC05_SAM.netDensity, GCFC05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
GCFC05_TAMg2 <- data.frame(GCFC05_TAM)
GCFC05_TAMg2 <- GCFC05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_TAMg2$player1
player2vector <- GCFC05_TAMg2$player2
GCFC05_TAMg3 <- GCFC05_TAMg2
GCFC05_TAMg3$p1inp2vec <- is.element(GCFC05_TAMg3$player1, player2vector)
GCFC05_TAMg3$p2inp1vec <- is.element(GCFC05_TAMg3$player2, player1vector)

addPlayer1 <- GCFC05_TAMg3[ which(GCFC05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC05_TAMg3[ which(GCFC05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_TAMg2 <- rbind(GCFC05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
GCFC05_TAMft <- ftable(GCFC05_TAMg2$player1, GCFC05_TAMg2$player2)
GCFC05_TAMft2 <- as.matrix(GCFC05_TAMft)
numRows <- nrow(GCFC05_TAMft2)
numCols <- ncol(GCFC05_TAMft2)
GCFC05_TAMft3 <- GCFC05_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC05_TAMTable <- graph.adjacency(GCFC05_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(GCFC05_TAMTable, vertex.label = V(GCFC05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
GCFC05_TAM.clusterCoef <- transitivity(GCFC05_TAMTable, type="global") #cluster coefficient
GCFC05_TAM.degreeCent <- centralization.degree(GCFC05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_TAMftn <- as.network.matrix(GCFC05_TAMft)
GCFC05_TAM.netDensity <- network.density(GCFC05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_TAM.entropy <- entropy(GCFC05_TAMft) #entropy

GCFC05_TAM.netMx <- cbind(GCFC05_TAM.netMx, GCFC05_TAM.clusterCoef, GCFC05_TAM.degreeCent$centralization,
                          GCFC05_TAM.netDensity, GCFC05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************
#NA

round = 5
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
GCFC05_SDMg2 <- data.frame(GCFC05_SDM)
GCFC05_SDMg2 <- GCFC05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_SDMg2$player1
player2vector <- GCFC05_SDMg2$player2
GCFC05_SDMg3 <- GCFC05_SDMg2
GCFC05_SDMg3$p1inp2vec <- is.element(GCFC05_SDMg3$player1, player2vector)
GCFC05_SDMg3$p2inp1vec <- is.element(GCFC05_SDMg3$player2, player1vector)

addPlayer1 <- GCFC05_SDMg3[ which(GCFC05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC05_SDMg3[ which(GCFC05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_SDMg2 <- rbind(GCFC05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
GCFC05_SDMft <- ftable(GCFC05_SDMg2$player1, GCFC05_SDMg2$player2)
GCFC05_SDMft2 <- as.matrix(GCFC05_SDMft)
numRows <- nrow(GCFC05_SDMft2)
numCols <- ncol(GCFC05_SDMft2)
GCFC05_SDMft3 <- GCFC05_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC05_SDMTable <- graph.adjacency(GCFC05_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(GCFC05_SDMTable, vertex.label = V(GCFC05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
GCFC05_SDM.clusterCoef <- transitivity(GCFC05_SDMTable, type="global") #cluster coefficient
GCFC05_SDM.degreeCent <- centralization.degree(GCFC05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_SDMftn <- as.network.matrix(GCFC05_SDMft)
GCFC05_SDM.netDensity <- network.density(GCFC05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_SDM.entropy <- entropy(GCFC05_SDMft) #entropy

GCFC05_SDM.netMx <- cbind(GCFC05_SDM.netMx, GCFC05_SDM.clusterCoef, GCFC05_SDM.degreeCent$centralization,
                          GCFC05_SDM.netDensity, GCFC05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
GCFC05_TDMg2 <- data.frame(GCFC05_TDM)
GCFC05_TDMg2 <- GCFC05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_TDMg2$player1
player2vector <- GCFC05_TDMg2$player2
GCFC05_TDMg3 <- GCFC05_TDMg2
GCFC05_TDMg3$p1inp2vec <- is.element(GCFC05_TDMg3$player1, player2vector)
GCFC05_TDMg3$p2inp1vec <- is.element(GCFC05_TDMg3$player2, player1vector)

addPlayer1 <- GCFC05_TDMg3[ which(GCFC05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC05_TDMg3[ which(GCFC05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_TDMg2 <- rbind(GCFC05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
GCFC05_TDMft <- ftable(GCFC05_TDMg2$player1, GCFC05_TDMg2$player2)
GCFC05_TDMft2 <- as.matrix(GCFC05_TDMft)
numRows <- nrow(GCFC05_TDMft2)
numCols <- ncol(GCFC05_TDMft2)
GCFC05_TDMft3 <- GCFC05_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC05_TDMTable <- graph.adjacency(GCFC05_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(GCFC05_TDMTable, vertex.label = V(GCFC05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
GCFC05_TDM.clusterCoef <- transitivity(GCFC05_TDMTable, type="global") #cluster coefficient
GCFC05_TDM.degreeCent <- centralization.degree(GCFC05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_TDMftn <- as.network.matrix(GCFC05_TDMft)
GCFC05_TDM.netDensity <- network.density(GCFC05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_TDM.entropy <- entropy(GCFC05_TDMft) #entropy

GCFC05_TDM.netMx <- cbind(GCFC05_TDM.netMx, GCFC05_TDM.clusterCoef, GCFC05_TDM.degreeCent$centralization,
                          GCFC05_TDM.netDensity, GCFC05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
GCFC05_SDg2 <- data.frame(GCFC05_SD)
GCFC05_SDg2 <- GCFC05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_SDg2$player1
player2vector <- GCFC05_SDg2$player2
GCFC05_SDg3 <- GCFC05_SDg2
GCFC05_SDg3$p1inp2vec <- is.element(GCFC05_SDg3$player1, player2vector)
GCFC05_SDg3$p2inp1vec <- is.element(GCFC05_SDg3$player2, player1vector)

addPlayer1 <- GCFC05_SDg3[ which(GCFC05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC05_SDg3[ which(GCFC05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_SDg2 <- rbind(GCFC05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
GCFC05_SDft <- ftable(GCFC05_SDg2$player1, GCFC05_SDg2$player2)
GCFC05_SDft2 <- as.matrix(GCFC05_SDft)
numRows <- nrow(GCFC05_SDft2)
numCols <- ncol(GCFC05_SDft2)
GCFC05_SDft3 <- GCFC05_SDft2[c(2:numRows) , c(2:numCols)]
GCFC05_SDTable <- graph.adjacency(GCFC05_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(GCFC05_SDTable, vertex.label = V(GCFC05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
GCFC05_SD.clusterCoef <- transitivity(GCFC05_SDTable, type="global") #cluster coefficient
GCFC05_SD.degreeCent <- centralization.degree(GCFC05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_SDftn <- as.network.matrix(GCFC05_SDft)
GCFC05_SD.netDensity <- network.density(GCFC05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_SD.entropy <- entropy(GCFC05_SDft) #entropy

GCFC05_SD.netMx <- cbind(GCFC05_SD.netMx, GCFC05_SD.clusterCoef, GCFC05_SD.degreeCent$centralization,
                         GCFC05_SD.netDensity, GCFC05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************

round = 5
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
GCFC05_TDg2 <- data.frame(GCFC05_TD)
GCFC05_TDg2 <- GCFC05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_TDg2$player1
player2vector <- GCFC05_TDg2$player2
GCFC05_TDg3 <- GCFC05_TDg2
GCFC05_TDg3$p1inp2vec <- is.element(GCFC05_TDg3$player1, player2vector)
GCFC05_TDg3$p2inp1vec <- is.element(GCFC05_TDg3$player2, player1vector)

addPlayer1 <- GCFC05_TDg3[ which(GCFC05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC05_TDg3[ which(GCFC05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_TDg2 <- rbind(GCFC05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
GCFC05_TDft <- ftable(GCFC05_TDg2$player1, GCFC05_TDg2$player2)
GCFC05_TDft2 <- as.matrix(GCFC05_TDft)
numRows <- nrow(GCFC05_TDft2)
numCols <- ncol(GCFC05_TDft2)
GCFC05_TDft3 <- GCFC05_TDft2[c(2:numRows) , c(2:numCols)]
GCFC05_TDTable <- graph.adjacency(GCFC05_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(GCFC05_TDTable, vertex.label = V(GCFC05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
GCFC05_TD.clusterCoef <- transitivity(GCFC05_TDTable, type="global") #cluster coefficient
GCFC05_TD.degreeCent <- centralization.degree(GCFC05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_TDftn <- as.network.matrix(GCFC05_TDft)
GCFC05_TD.netDensity <- network.density(GCFC05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_TD.entropy <- entropy(GCFC05_TDft) #entropy

GCFC05_TD.netMx <- cbind(GCFC05_TD.netMx, GCFC05_TD.clusterCoef, GCFC05_TD.degreeCent$centralization,
                         GCFC05_TD.netDensity, GCFC05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
GCFC05_QTg2 <- data.frame(GCFC05_QT)
GCFC05_QTg2 <- GCFC05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC05_QTg2$player1
player2vector <- GCFC05_QTg2$player2
GCFC05_QTg3 <- GCFC05_QTg2
GCFC05_QTg3$p1inp2vec <- is.element(GCFC05_QTg3$player1, player2vector)
GCFC05_QTg3$p2inp1vec <- is.element(GCFC05_QTg3$player2, player1vector)

addPlayer1 <- GCFC05_QTg3[ which(GCFC05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC05_QTg3[ which(GCFC05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC05_QTg2 <- rbind(GCFC05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
GCFC05_QTft <- ftable(GCFC05_QTg2$player1, GCFC05_QTg2$player2)
GCFC05_QTft2 <- as.matrix(GCFC05_QTft)
numRows <- nrow(GCFC05_QTft2)
numCols <- ncol(GCFC05_QTft2)
GCFC05_QTft3 <- GCFC05_QTft2[c(2:numRows) , c(2:numCols)]
GCFC05_QTTable <- graph.adjacency(GCFC05_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(GCFC05_QTTable, vertex.label = V(GCFC05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
GCFC05_QT.clusterCoef <- transitivity(GCFC05_QTTable, type="global") #cluster coefficient
GCFC05_QT.degreeCent <- centralization.degree(GCFC05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC05_QTftn <- as.network.matrix(GCFC05_QTft)
GCFC05_QT.netDensity <- network.density(GCFC05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC05_QT.entropy <- entropy(GCFC05_QTft) #entropy

GCFC05_QT.netMx <- cbind(GCFC05_QT.netMx, GCFC05_QT.clusterCoef, GCFC05_QT.degreeCent$centralization,
                         GCFC05_QT.netDensity, GCFC05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC05_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
GEEL05_Gg2 <- data.frame(GEEL05_G)
GEEL05_Gg2 <- GEEL05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_Gg2$player1
player2vector <- GEEL05_Gg2$player2
GEEL05_Gg3 <- GEEL05_Gg2
GEEL05_Gg3$p1inp2vec <- is.element(GEEL05_Gg3$player1, player2vector)
GEEL05_Gg3$p2inp1vec <- is.element(GEEL05_Gg3$player2, player1vector)

addPlayer1 <- GEEL05_Gg3[ which(GEEL05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_Gg3[ which(GEEL05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_Gg2 <- rbind(GEEL05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
GEEL05_Gft <- ftable(GEEL05_Gg2$player1, GEEL05_Gg2$player2)
GEEL05_Gft2 <- as.matrix(GEEL05_Gft)
numRows <- nrow(GEEL05_Gft2)
numCols <- ncol(GEEL05_Gft2)
GEEL05_Gft3 <- GEEL05_Gft2[c(2:numRows) , c(2:numCols)]
GEEL05_GTable <- graph.adjacency(GEEL05_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(GEEL05_GTable, vertex.label = V(GEEL05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
GEEL05_G.clusterCoef <- transitivity(GEEL05_GTable, type="global") #cluster coefficient
GEEL05_G.degreeCent <- centralization.degree(GEEL05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_Gftn <- as.network.matrix(GEEL05_Gft)
GEEL05_G.netDensity <- network.density(GEEL05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_G.entropy <- entropy(GEEL05_Gft) #entropy

GEEL05_G.netMx <- cbind(GEEL05_G.netMx, GEEL05_G.clusterCoef, GEEL05_G.degreeCent$centralization,
                        GEEL05_G.netDensity, GEEL05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************
#NA

round = 5
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
GEEL05_Bg2 <- data.frame(GEEL05_B)
GEEL05_Bg2 <- GEEL05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_Bg2$player1
player2vector <- GEEL05_Bg2$player2
GEEL05_Bg3 <- GEEL05_Bg2
GEEL05_Bg3$p1inp2vec <- is.element(GEEL05_Bg3$player1, player2vector)
GEEL05_Bg3$p2inp1vec <- is.element(GEEL05_Bg3$player2, player1vector)

addPlayer1 <- GEEL05_Bg3[ which(GEEL05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_Bg3[ which(GEEL05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_Bg2 <- rbind(GEEL05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
GEEL05_Bft <- ftable(GEEL05_Bg2$player1, GEEL05_Bg2$player2)
GEEL05_Bft2 <- as.matrix(GEEL05_Bft)
numRows <- nrow(GEEL05_Bft2)
numCols <- ncol(GEEL05_Bft2)
GEEL05_Bft3 <- GEEL05_Bft2[c(2:numRows) , c(2:numCols)]
GEEL05_BTable <- graph.adjacency(GEEL05_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(GEEL05_BTable, vertex.label = V(GEEL05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
GEEL05_B.clusterCoef <- transitivity(GEEL05_BTable, type="global") #cluster coefficient
GEEL05_B.degreeCent <- centralization.degree(GEEL05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_Bftn <- as.network.matrix(GEEL05_Bft)
GEEL05_B.netDensity <- network.density(GEEL05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_B.entropy <- entropy(GEEL05_Bft) #entropy

GEEL05_B.netMx <- cbind(GEEL05_B.netMx, GEEL05_B.clusterCoef, GEEL05_B.degreeCent$centralization,
                        GEEL05_B.netDensity, GEEL05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************

round = 5
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
GEEL05_SFg2 <- data.frame(GEEL05_SF)
GEEL05_SFg2 <- GEEL05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_SFg2$player1
player2vector <- GEEL05_SFg2$player2
GEEL05_SFg3 <- GEEL05_SFg2
GEEL05_SFg3$p1inp2vec <- is.element(GEEL05_SFg3$player1, player2vector)
GEEL05_SFg3$p2inp1vec <- is.element(GEEL05_SFg3$player2, player1vector)

addPlayer1 <- GEEL05_SFg3[ which(GEEL05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_SFg3[ which(GEEL05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_SFg2 <- rbind(GEEL05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
GEEL05_SFft <- ftable(GEEL05_SFg2$player1, GEEL05_SFg2$player2)
GEEL05_SFft2 <- as.matrix(GEEL05_SFft)
numRows <- nrow(GEEL05_SFft2)
numCols <- ncol(GEEL05_SFft2)
GEEL05_SFft3 <- GEEL05_SFft2[c(2:numRows) , c(2:numCols)]
GEEL05_SFTable <- graph.adjacency(GEEL05_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(GEEL05_SFTable, vertex.label = V(GEEL05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
GEEL05_SF.clusterCoef <- transitivity(GEEL05_SFTable, type="global") #cluster coefficient
GEEL05_SF.degreeCent <- centralization.degree(GEEL05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_SFftn <- as.network.matrix(GEEL05_SFft)
GEEL05_SF.netDensity <- network.density(GEEL05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_SF.entropy <- entropy(GEEL05_SFft) #entropy

GEEL05_SF.netMx <- cbind(GEEL05_SF.netMx, GEEL05_SF.clusterCoef, GEEL05_SF.degreeCent$centralization,
                         GEEL05_SF.netDensity, GEEL05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************

round = 5
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
GEEL05_TFg2 <- data.frame(GEEL05_TF)
GEEL05_TFg2 <- GEEL05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_TFg2$player1
player2vector <- GEEL05_TFg2$player2
GEEL05_TFg3 <- GEEL05_TFg2
GEEL05_TFg3$p1inp2vec <- is.element(GEEL05_TFg3$player1, player2vector)
GEEL05_TFg3$p2inp1vec <- is.element(GEEL05_TFg3$player2, player1vector)

addPlayer1 <- GEEL05_TFg3[ which(GEEL05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_TFg3[ which(GEEL05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_TFg2 <- rbind(GEEL05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
GEEL05_TFft <- ftable(GEEL05_TFg2$player1, GEEL05_TFg2$player2)
GEEL05_TFft2 <- as.matrix(GEEL05_TFft)
numRows <- nrow(GEEL05_TFft2)
numCols <- ncol(GEEL05_TFft2)
GEEL05_TFft3 <- GEEL05_TFft2[c(2:numRows) , c(2:numCols)]
GEEL05_TFTable <- graph.adjacency(GEEL05_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(GEEL05_TFTable, vertex.label = V(GEEL05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
GEEL05_TF.clusterCoef <- transitivity(GEEL05_TFTable, type="global") #cluster coefficient
GEEL05_TF.degreeCent <- centralization.degree(GEEL05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_TFftn <- as.network.matrix(GEEL05_TFft)
GEEL05_TF.netDensity <- network.density(GEEL05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_TF.entropy <- entropy(GEEL05_TFft) #entropy

GEEL05_TF.netMx <- cbind(GEEL05_TF.netMx, GEEL05_TF.clusterCoef, GEEL05_TF.degreeCent$centralization,
                         GEEL05_TF.netDensity, GEEL05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
GEEL05_SAMg2 <- data.frame(GEEL05_SAM)
GEEL05_SAMg2 <- GEEL05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_SAMg2$player1
player2vector <- GEEL05_SAMg2$player2
GEEL05_SAMg3 <- GEEL05_SAMg2
GEEL05_SAMg3$p1inp2vec <- is.element(GEEL05_SAMg3$player1, player2vector)
GEEL05_SAMg3$p2inp1vec <- is.element(GEEL05_SAMg3$player2, player1vector)

addPlayer1 <- GEEL05_SAMg3[ which(GEEL05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_SAMg3[ which(GEEL05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_SAMg2 <- rbind(GEEL05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
GEEL05_SAMft <- ftable(GEEL05_SAMg2$player1, GEEL05_SAMg2$player2)
GEEL05_SAMft2 <- as.matrix(GEEL05_SAMft)
numRows <- nrow(GEEL05_SAMft2)
numCols <- ncol(GEEL05_SAMft2)
GEEL05_SAMft3 <- GEEL05_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL05_SAMTable <- graph.adjacency(GEEL05_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(GEEL05_SAMTable, vertex.label = V(GEEL05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
GEEL05_SAM.clusterCoef <- transitivity(GEEL05_SAMTable, type="global") #cluster coefficient
GEEL05_SAM.degreeCent <- centralization.degree(GEEL05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_SAMftn <- as.network.matrix(GEEL05_SAMft)
GEEL05_SAM.netDensity <- network.density(GEEL05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_SAM.entropy <- entropy(GEEL05_SAMft) #entropy

GEEL05_SAM.netMx <- cbind(GEEL05_SAM.netMx, GEEL05_SAM.clusterCoef, GEEL05_SAM.degreeCent$centralization,
                          GEEL05_SAM.netDensity, GEEL05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
GEEL05_TAMg2 <- data.frame(GEEL05_TAM)
GEEL05_TAMg2 <- GEEL05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_TAMg2$player1
player2vector <- GEEL05_TAMg2$player2
GEEL05_TAMg3 <- GEEL05_TAMg2
GEEL05_TAMg3$p1inp2vec <- is.element(GEEL05_TAMg3$player1, player2vector)
GEEL05_TAMg3$p2inp1vec <- is.element(GEEL05_TAMg3$player2, player1vector)

addPlayer1 <- GEEL05_TAMg3[ which(GEEL05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_TAMg3[ which(GEEL05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_TAMg2 <- rbind(GEEL05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
GEEL05_TAMft <- ftable(GEEL05_TAMg2$player1, GEEL05_TAMg2$player2)
GEEL05_TAMft2 <- as.matrix(GEEL05_TAMft)
numRows <- nrow(GEEL05_TAMft2)
numCols <- ncol(GEEL05_TAMft2)
GEEL05_TAMft3 <- GEEL05_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL05_TAMTable <- graph.adjacency(GEEL05_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(GEEL05_TAMTable, vertex.label = V(GEEL05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
GEEL05_TAM.clusterCoef <- transitivity(GEEL05_TAMTable, type="global") #cluster coefficient
GEEL05_TAM.degreeCent <- centralization.degree(GEEL05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_TAMftn <- as.network.matrix(GEEL05_TAMft)
GEEL05_TAM.netDensity <- network.density(GEEL05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_TAM.entropy <- entropy(GEEL05_TAMft) #entropy

GEEL05_TAM.netMx <- cbind(GEEL05_TAM.netMx, GEEL05_TAM.clusterCoef, GEEL05_TAM.degreeCent$centralization,
                          GEEL05_TAM.netDensity, GEEL05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
GEEL05_SDMg2 <- data.frame(GEEL05_SDM)
GEEL05_SDMg2 <- GEEL05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_SDMg2$player1
player2vector <- GEEL05_SDMg2$player2
GEEL05_SDMg3 <- GEEL05_SDMg2
GEEL05_SDMg3$p1inp2vec <- is.element(GEEL05_SDMg3$player1, player2vector)
GEEL05_SDMg3$p2inp1vec <- is.element(GEEL05_SDMg3$player2, player1vector)

addPlayer1 <- GEEL05_SDMg3[ which(GEEL05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_SDMg3[ which(GEEL05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_SDMg2 <- rbind(GEEL05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
GEEL05_SDMft <- ftable(GEEL05_SDMg2$player1, GEEL05_SDMg2$player2)
GEEL05_SDMft2 <- as.matrix(GEEL05_SDMft)
numRows <- nrow(GEEL05_SDMft2)
numCols <- ncol(GEEL05_SDMft2)
GEEL05_SDMft3 <- GEEL05_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL05_SDMTable <- graph.adjacency(GEEL05_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(GEEL05_SDMTable, vertex.label = V(GEEL05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
GEEL05_SDM.clusterCoef <- transitivity(GEEL05_SDMTable, type="global") #cluster coefficient
GEEL05_SDM.degreeCent <- centralization.degree(GEEL05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_SDMftn <- as.network.matrix(GEEL05_SDMft)
GEEL05_SDM.netDensity <- network.density(GEEL05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_SDM.entropy <- entropy(GEEL05_SDMft) #entropy

GEEL05_SDM.netMx <- cbind(GEEL05_SDM.netMx, GEEL05_SDM.clusterCoef, GEEL05_SDM.degreeCent$centralization,
                          GEEL05_SDM.netDensity, GEEL05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
GEEL05_TDMg2 <- data.frame(GEEL05_TDM)
GEEL05_TDMg2 <- GEEL05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_TDMg2$player1
player2vector <- GEEL05_TDMg2$player2
GEEL05_TDMg3 <- GEEL05_TDMg2
GEEL05_TDMg3$p1inp2vec <- is.element(GEEL05_TDMg3$player1, player2vector)
GEEL05_TDMg3$p2inp1vec <- is.element(GEEL05_TDMg3$player2, player1vector)

addPlayer1 <- GEEL05_TDMg3[ which(GEEL05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_TDMg3[ which(GEEL05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_TDMg2 <- rbind(GEEL05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
GEEL05_TDMft <- ftable(GEEL05_TDMg2$player1, GEEL05_TDMg2$player2)
GEEL05_TDMft2 <- as.matrix(GEEL05_TDMft)
numRows <- nrow(GEEL05_TDMft2)
numCols <- ncol(GEEL05_TDMft2)
GEEL05_TDMft3 <- GEEL05_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL05_TDMTable <- graph.adjacency(GEEL05_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(GEEL05_TDMTable, vertex.label = V(GEEL05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
GEEL05_TDM.clusterCoef <- transitivity(GEEL05_TDMTable, type="global") #cluster coefficient
GEEL05_TDM.degreeCent <- centralization.degree(GEEL05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_TDMftn <- as.network.matrix(GEEL05_TDMft)
GEEL05_TDM.netDensity <- network.density(GEEL05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_TDM.entropy <- entropy(GEEL05_TDMft) #entropy

GEEL05_TDM.netMx <- cbind(GEEL05_TDM.netMx, GEEL05_TDM.clusterCoef, GEEL05_TDM.degreeCent$centralization,
                          GEEL05_TDM.netDensity, GEEL05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
GEEL05_SDg2 <- data.frame(GEEL05_SD)
GEEL05_SDg2 <- GEEL05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_SDg2$player1
player2vector <- GEEL05_SDg2$player2
GEEL05_SDg3 <- GEEL05_SDg2
GEEL05_SDg3$p1inp2vec <- is.element(GEEL05_SDg3$player1, player2vector)
GEEL05_SDg3$p2inp1vec <- is.element(GEEL05_SDg3$player2, player1vector)

addPlayer1 <- GEEL05_SDg3[ which(GEEL05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_SDg3[ which(GEEL05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_SDg2 <- rbind(GEEL05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
GEEL05_SDft <- ftable(GEEL05_SDg2$player1, GEEL05_SDg2$player2)
GEEL05_SDft2 <- as.matrix(GEEL05_SDft)
numRows <- nrow(GEEL05_SDft2)
numCols <- ncol(GEEL05_SDft2)
GEEL05_SDft3 <- GEEL05_SDft2[c(2:numRows) , c(2:numCols)]
GEEL05_SDTable <- graph.adjacency(GEEL05_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(GEEL05_SDTable, vertex.label = V(GEEL05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
GEEL05_SD.clusterCoef <- transitivity(GEEL05_SDTable, type="global") #cluster coefficient
GEEL05_SD.degreeCent <- centralization.degree(GEEL05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_SDftn <- as.network.matrix(GEEL05_SDft)
GEEL05_SD.netDensity <- network.density(GEEL05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_SD.entropy <- entropy(GEEL05_SDft) #entropy

GEEL05_SD.netMx <- cbind(GEEL05_SD.netMx, GEEL05_SD.clusterCoef, GEEL05_SD.degreeCent$centralization,
                         GEEL05_SD.netDensity, GEEL05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
GEEL05_TDg2 <- data.frame(GEEL05_TD)
GEEL05_TDg2 <- GEEL05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_TDg2$player1
player2vector <- GEEL05_TDg2$player2
GEEL05_TDg3 <- GEEL05_TDg2
GEEL05_TDg3$p1inp2vec <- is.element(GEEL05_TDg3$player1, player2vector)
GEEL05_TDg3$p2inp1vec <- is.element(GEEL05_TDg3$player2, player1vector)

addPlayer1 <- GEEL05_TDg3[ which(GEEL05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_TDg3[ which(GEEL05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_TDg2 <- rbind(GEEL05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
GEEL05_TDft <- ftable(GEEL05_TDg2$player1, GEEL05_TDg2$player2)
GEEL05_TDft2 <- as.matrix(GEEL05_TDft)
numRows <- nrow(GEEL05_TDft2)
numCols <- ncol(GEEL05_TDft2)
GEEL05_TDft3 <- GEEL05_TDft2[c(2:numRows) , c(2:numCols)]
GEEL05_TDTable <- graph.adjacency(GEEL05_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(GEEL05_TDTable, vertex.label = V(GEEL05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
GEEL05_TD.clusterCoef <- transitivity(GEEL05_TDTable, type="global") #cluster coefficient
GEEL05_TD.degreeCent <- centralization.degree(GEEL05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_TDftn <- as.network.matrix(GEEL05_TDft)
GEEL05_TD.netDensity <- network.density(GEEL05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_TD.entropy <- entropy(GEEL05_TDft) #entropy

GEEL05_TD.netMx <- cbind(GEEL05_TD.netMx, GEEL05_TD.clusterCoef, GEEL05_TD.degreeCent$centralization,
                         GEEL05_TD.netDensity, GEEL05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
GEEL05_QTg2 <- data.frame(GEEL05_QT)
GEEL05_QTg2 <- GEEL05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL05_QTg2$player1
player2vector <- GEEL05_QTg2$player2
GEEL05_QTg3 <- GEEL05_QTg2
GEEL05_QTg3$p1inp2vec <- is.element(GEEL05_QTg3$player1, player2vector)
GEEL05_QTg3$p2inp1vec <- is.element(GEEL05_QTg3$player2, player1vector)

addPlayer1 <- GEEL05_QTg3[ which(GEEL05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL05_QTg3[ which(GEEL05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL05_QTg2 <- rbind(GEEL05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
GEEL05_QTft <- ftable(GEEL05_QTg2$player1, GEEL05_QTg2$player2)
GEEL05_QTft2 <- as.matrix(GEEL05_QTft)
numRows <- nrow(GEEL05_QTft2)
numCols <- ncol(GEEL05_QTft2)
GEEL05_QTft3 <- GEEL05_QTft2[c(2:numRows) , c(2:numCols)]
GEEL05_QTTable <- graph.adjacency(GEEL05_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(GEEL05_QTTable, vertex.label = V(GEEL05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
GEEL05_QT.clusterCoef <- transitivity(GEEL05_QTTable, type="global") #cluster coefficient
GEEL05_QT.degreeCent <- centralization.degree(GEEL05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL05_QTftn <- as.network.matrix(GEEL05_QTft)
GEEL05_QT.netDensity <- network.density(GEEL05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL05_QT.entropy <- entropy(GEEL05_QTft) #entropy

GEEL05_QT.netMx <- cbind(GEEL05_QT.netMx, GEEL05_QT.clusterCoef, GEEL05_QT.degreeCent$centralization,
                         GEEL05_QT.netDensity, GEEL05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL05_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "GWS"
KIoutcome = "Goal_F"
GWS05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
GWS05_Gg2 <- data.frame(GWS05_G)
GWS05_Gg2 <- GWS05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_Gg2$player1
player2vector <- GWS05_Gg2$player2
GWS05_Gg3 <- GWS05_Gg2
GWS05_Gg3$p1inp2vec <- is.element(GWS05_Gg3$player1, player2vector)
GWS05_Gg3$p2inp1vec <- is.element(GWS05_Gg3$player2, player1vector)

addPlayer1 <- GWS05_Gg3[ which(GWS05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS05_Gg3[ which(GWS05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_Gg2 <- rbind(GWS05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
GWS05_Gft <- ftable(GWS05_Gg2$player1, GWS05_Gg2$player2)
GWS05_Gft2 <- as.matrix(GWS05_Gft)
numRows <- nrow(GWS05_Gft2)
numCols <- ncol(GWS05_Gft2)
GWS05_Gft3 <- GWS05_Gft2[c(2:numRows) , c(2:numCols)]
GWS05_GTable <- graph.adjacency(GWS05_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(GWS05_GTable, vertex.label = V(GWS05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
GWS05_G.clusterCoef <- transitivity(GWS05_GTable, type="global") #cluster coefficient
GWS05_G.degreeCent <- centralization.degree(GWS05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_Gftn <- as.network.matrix(GWS05_Gft)
GWS05_G.netDensity <- network.density(GWS05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_G.entropy <- entropy(GWS05_Gft) #entropy

GWS05_G.netMx <- cbind(GWS05_G.netMx, GWS05_G.clusterCoef, GWS05_G.degreeCent$centralization,
                       GWS05_G.netDensity, GWS05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************

round = 5
teamName = "GWS"
KIoutcome = "Behind_F"
GWS05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
GWS05_Bg2 <- data.frame(GWS05_B)
GWS05_Bg2 <- GWS05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_Bg2$player1
player2vector <- GWS05_Bg2$player2
GWS05_Bg3 <- GWS05_Bg2
GWS05_Bg3$p1inp2vec <- is.element(GWS05_Bg3$player1, player2vector)
GWS05_Bg3$p2inp1vec <- is.element(GWS05_Bg3$player2, player1vector)

addPlayer1 <- GWS05_Bg3[ which(GWS05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_Bg3[ which(GWS05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_Bg2 <- rbind(GWS05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
GWS05_Bft <- ftable(GWS05_Bg2$player1, GWS05_Bg2$player2)
GWS05_Bft2 <- as.matrix(GWS05_Bft)
numRows <- nrow(GWS05_Bft2)
numCols <- ncol(GWS05_Bft2)
GWS05_Bft3 <- GWS05_Bft2[c(2:numRows) , c(2:numCols)]
GWS05_BTable <- graph.adjacency(GWS05_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(GWS05_BTable, vertex.label = V(GWS05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
GWS05_B.clusterCoef <- transitivity(GWS05_BTable, type="global") #cluster coefficient
GWS05_B.degreeCent <- centralization.degree(GWS05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_Bftn <- as.network.matrix(GWS05_Bft)
GWS05_B.netDensity <- network.density(GWS05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_B.entropy <- entropy(GWS05_Bft) #entropy

GWS05_B.netMx <- cbind(GWS05_B.netMx, GWS05_B.clusterCoef, GWS05_B.degreeCent$centralization,
                       GWS05_B.netDensity, GWS05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
GWS05_SFg2 <- data.frame(GWS05_SF)
GWS05_SFg2 <- GWS05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_SFg2$player1
player2vector <- GWS05_SFg2$player2
GWS05_SFg3 <- GWS05_SFg2
GWS05_SFg3$p1inp2vec <- is.element(GWS05_SFg3$player1, player2vector)
GWS05_SFg3$p2inp1vec <- is.element(GWS05_SFg3$player2, player1vector)

addPlayer1 <- GWS05_SFg3[ which(GWS05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_SFg3[ which(GWS05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_SFg2 <- rbind(GWS05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
GWS05_SFft <- ftable(GWS05_SFg2$player1, GWS05_SFg2$player2)
GWS05_SFft2 <- as.matrix(GWS05_SFft)
numRows <- nrow(GWS05_SFft2)
numCols <- ncol(GWS05_SFft2)
GWS05_SFft3 <- GWS05_SFft2[c(2:numRows) , c(2:numCols)]
GWS05_SFTable <- graph.adjacency(GWS05_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(GWS05_SFTable, vertex.label = V(GWS05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
GWS05_SF.clusterCoef <- transitivity(GWS05_SFTable, type="global") #cluster coefficient
GWS05_SF.degreeCent <- centralization.degree(GWS05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_SFftn <- as.network.matrix(GWS05_SFft)
GWS05_SF.netDensity <- network.density(GWS05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_SF.entropy <- entropy(GWS05_SFft) #entropy

GWS05_SF.netMx <- cbind(GWS05_SF.netMx, GWS05_SF.clusterCoef, GWS05_SF.degreeCent$centralization,
                        GWS05_SF.netDensity, GWS05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************
#NA

round = 5
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
GWS05_TFg2 <- data.frame(GWS05_TF)
GWS05_TFg2 <- GWS05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_TFg2$player1
player2vector <- GWS05_TFg2$player2
GWS05_TFg3 <- GWS05_TFg2
GWS05_TFg3$p1inp2vec <- is.element(GWS05_TFg3$player1, player2vector)
GWS05_TFg3$p2inp1vec <- is.element(GWS05_TFg3$player2, player1vector)

addPlayer1 <- GWS05_TFg3[ which(GWS05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_TFg3[ which(GWS05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_TFg2 <- rbind(GWS05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
GWS05_TFft <- ftable(GWS05_TFg2$player1, GWS05_TFg2$player2)
GWS05_TFft2 <- as.matrix(GWS05_TFft)
numRows <- nrow(GWS05_TFft2)
numCols <- ncol(GWS05_TFft2)
GWS05_TFft3 <- GWS05_TFft2[c(2:numRows) , c(2:numCols)]
GWS05_TFTable <- graph.adjacency(GWS05_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(GWS05_TFTable, vertex.label = V(GWS05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
GWS05_TF.clusterCoef <- transitivity(GWS05_TFTable, type="global") #cluster coefficient
GWS05_TF.degreeCent <- centralization.degree(GWS05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_TFftn <- as.network.matrix(GWS05_TFft)
GWS05_TF.netDensity <- network.density(GWS05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_TF.entropy <- entropy(GWS05_TFft) #entropy

GWS05_TF.netMx <- cbind(GWS05_TF.netMx, GWS05_TF.clusterCoef, GWS05_TF.degreeCent$centralization,
                        GWS05_TF.netDensity, GWS05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
GWS05_SAMg2 <- data.frame(GWS05_SAM)
GWS05_SAMg2 <- GWS05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_SAMg2$player1
player2vector <- GWS05_SAMg2$player2
GWS05_SAMg3 <- GWS05_SAMg2
GWS05_SAMg3$p1inp2vec <- is.element(GWS05_SAMg3$player1, player2vector)
GWS05_SAMg3$p2inp1vec <- is.element(GWS05_SAMg3$player2, player1vector)

addPlayer1 <- GWS05_SAMg3[ which(GWS05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_SAMg3[ which(GWS05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_SAMg2 <- rbind(GWS05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
GWS05_SAMft <- ftable(GWS05_SAMg2$player1, GWS05_SAMg2$player2)
GWS05_SAMft2 <- as.matrix(GWS05_SAMft)
numRows <- nrow(GWS05_SAMft2)
numCols <- ncol(GWS05_SAMft2)
GWS05_SAMft3 <- GWS05_SAMft2[c(2:numRows) , c(2:numCols)]
GWS05_SAMTable <- graph.adjacency(GWS05_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(GWS05_SAMTable, vertex.label = V(GWS05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
GWS05_SAM.clusterCoef <- transitivity(GWS05_SAMTable, type="global") #cluster coefficient
GWS05_SAM.degreeCent <- centralization.degree(GWS05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_SAMftn <- as.network.matrix(GWS05_SAMft)
GWS05_SAM.netDensity <- network.density(GWS05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_SAM.entropy <- entropy(GWS05_SAMft) #entropy

GWS05_SAM.netMx <- cbind(GWS05_SAM.netMx, GWS05_SAM.clusterCoef, GWS05_SAM.degreeCent$centralization,
                         GWS05_SAM.netDensity, GWS05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
GWS05_TAMg2 <- data.frame(GWS05_TAM)
GWS05_TAMg2 <- GWS05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_TAMg2$player1
player2vector <- GWS05_TAMg2$player2
GWS05_TAMg3 <- GWS05_TAMg2
GWS05_TAMg3$p1inp2vec <- is.element(GWS05_TAMg3$player1, player2vector)
GWS05_TAMg3$p2inp1vec <- is.element(GWS05_TAMg3$player2, player1vector)

addPlayer1 <- GWS05_TAMg3[ which(GWS05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_TAMg3[ which(GWS05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_TAMg2 <- rbind(GWS05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
GWS05_TAMft <- ftable(GWS05_TAMg2$player1, GWS05_TAMg2$player2)
GWS05_TAMft2 <- as.matrix(GWS05_TAMft)
numRows <- nrow(GWS05_TAMft2)
numCols <- ncol(GWS05_TAMft2)
GWS05_TAMft3 <- GWS05_TAMft2[c(2:numRows) , c(2:numCols)]
GWS05_TAMTable <- graph.adjacency(GWS05_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(GWS05_TAMTable, vertex.label = V(GWS05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
GWS05_TAM.clusterCoef <- transitivity(GWS05_TAMTable, type="global") #cluster coefficient
GWS05_TAM.degreeCent <- centralization.degree(GWS05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_TAMftn <- as.network.matrix(GWS05_TAMft)
GWS05_TAM.netDensity <- network.density(GWS05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_TAM.entropy <- entropy(GWS05_TAMft) #entropy

GWS05_TAM.netMx <- cbind(GWS05_TAM.netMx, GWS05_TAM.clusterCoef, GWS05_TAM.degreeCent$centralization,
                         GWS05_TAM.netDensity, GWS05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
GWS05_SDMg2 <- data.frame(GWS05_SDM)
GWS05_SDMg2 <- GWS05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_SDMg2$player1
player2vector <- GWS05_SDMg2$player2
GWS05_SDMg3 <- GWS05_SDMg2
GWS05_SDMg3$p1inp2vec <- is.element(GWS05_SDMg3$player1, player2vector)
GWS05_SDMg3$p2inp1vec <- is.element(GWS05_SDMg3$player2, player1vector)

addPlayer1 <- GWS05_SDMg3[ which(GWS05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_SDMg3[ which(GWS05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_SDMg2 <- rbind(GWS05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
GWS05_SDMft <- ftable(GWS05_SDMg2$player1, GWS05_SDMg2$player2)
GWS05_SDMft2 <- as.matrix(GWS05_SDMft)
numRows <- nrow(GWS05_SDMft2)
numCols <- ncol(GWS05_SDMft2)
GWS05_SDMft3 <- GWS05_SDMft2[c(2:numRows) , c(2:numCols)]
GWS05_SDMTable <- graph.adjacency(GWS05_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(GWS05_SDMTable, vertex.label = V(GWS05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
GWS05_SDM.clusterCoef <- transitivity(GWS05_SDMTable, type="global") #cluster coefficient
GWS05_SDM.degreeCent <- centralization.degree(GWS05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_SDMftn <- as.network.matrix(GWS05_SDMft)
GWS05_SDM.netDensity <- network.density(GWS05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_SDM.entropy <- entropy(GWS05_SDMft) #entropy

GWS05_SDM.netMx <- cbind(GWS05_SDM.netMx, GWS05_SDM.clusterCoef, GWS05_SDM.degreeCent$centralization,
                         GWS05_SDM.netDensity, GWS05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
GWS05_TDMg2 <- data.frame(GWS05_TDM)
GWS05_TDMg2 <- GWS05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_TDMg2$player1
player2vector <- GWS05_TDMg2$player2
GWS05_TDMg3 <- GWS05_TDMg2
GWS05_TDMg3$p1inp2vec <- is.element(GWS05_TDMg3$player1, player2vector)
GWS05_TDMg3$p2inp1vec <- is.element(GWS05_TDMg3$player2, player1vector)

addPlayer1 <- GWS05_TDMg3[ which(GWS05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_TDMg3[ which(GWS05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_TDMg2 <- rbind(GWS05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
GWS05_TDMft <- ftable(GWS05_TDMg2$player1, GWS05_TDMg2$player2)
GWS05_TDMft2 <- as.matrix(GWS05_TDMft)
numRows <- nrow(GWS05_TDMft2)
numCols <- ncol(GWS05_TDMft2)
GWS05_TDMft3 <- GWS05_TDMft2[c(2:numRows) , c(2:numCols)]
GWS05_TDMTable <- graph.adjacency(GWS05_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(GWS05_TDMTable, vertex.label = V(GWS05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
GWS05_TDM.clusterCoef <- transitivity(GWS05_TDMTable, type="global") #cluster coefficient
GWS05_TDM.degreeCent <- centralization.degree(GWS05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_TDMftn <- as.network.matrix(GWS05_TDMft)
GWS05_TDM.netDensity <- network.density(GWS05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_TDM.entropy <- entropy(GWS05_TDMft) #entropy

GWS05_TDM.netMx <- cbind(GWS05_TDM.netMx, GWS05_TDM.clusterCoef, GWS05_TDM.degreeCent$centralization,
                         GWS05_TDM.netDensity, GWS05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
GWS05_SDg2 <- data.frame(GWS05_SD)
GWS05_SDg2 <- GWS05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_SDg2$player1
player2vector <- GWS05_SDg2$player2
GWS05_SDg3 <- GWS05_SDg2
GWS05_SDg3$p1inp2vec <- is.element(GWS05_SDg3$player1, player2vector)
GWS05_SDg3$p2inp1vec <- is.element(GWS05_SDg3$player2, player1vector)

addPlayer1 <- GWS05_SDg3[ which(GWS05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_SDg3[ which(GWS05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_SDg2 <- rbind(GWS05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
GWS05_SDft <- ftable(GWS05_SDg2$player1, GWS05_SDg2$player2)
GWS05_SDft2 <- as.matrix(GWS05_SDft)
numRows <- nrow(GWS05_SDft2)
numCols <- ncol(GWS05_SDft2)
GWS05_SDft3 <- GWS05_SDft2[c(2:numRows) , c(2:numCols)]
GWS05_SDTable <- graph.adjacency(GWS05_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(GWS05_SDTable, vertex.label = V(GWS05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
GWS05_SD.clusterCoef <- transitivity(GWS05_SDTable, type="global") #cluster coefficient
GWS05_SD.degreeCent <- centralization.degree(GWS05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_SDftn <- as.network.matrix(GWS05_SDft)
GWS05_SD.netDensity <- network.density(GWS05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_SD.entropy <- entropy(GWS05_SDft) #entropy

GWS05_SD.netMx <- cbind(GWS05_SD.netMx, GWS05_SD.clusterCoef, GWS05_SD.degreeCent$centralization,
                        GWS05_SD.netDensity, GWS05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
GWS05_TDg2 <- data.frame(GWS05_TD)
GWS05_TDg2 <- GWS05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_TDg2$player1
player2vector <- GWS05_TDg2$player2
GWS05_TDg3 <- GWS05_TDg2
GWS05_TDg3$p1inp2vec <- is.element(GWS05_TDg3$player1, player2vector)
GWS05_TDg3$p2inp1vec <- is.element(GWS05_TDg3$player2, player1vector)

addPlayer1 <- GWS05_TDg3[ which(GWS05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_TDg3[ which(GWS05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_TDg2 <- rbind(GWS05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
GWS05_TDft <- ftable(GWS05_TDg2$player1, GWS05_TDg2$player2)
GWS05_TDft2 <- as.matrix(GWS05_TDft)
numRows <- nrow(GWS05_TDft2)
numCols <- ncol(GWS05_TDft2)
GWS05_TDft3 <- GWS05_TDft2[c(2:numRows) , c(2:numCols)]
GWS05_TDTable <- graph.adjacency(GWS05_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(GWS05_TDTable, vertex.label = V(GWS05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
GWS05_TD.clusterCoef <- transitivity(GWS05_TDTable, type="global") #cluster coefficient
GWS05_TD.degreeCent <- centralization.degree(GWS05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_TDftn <- as.network.matrix(GWS05_TDft)
GWS05_TD.netDensity <- network.density(GWS05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_TD.entropy <- entropy(GWS05_TDft) #entropy

GWS05_TD.netMx <- cbind(GWS05_TD.netMx, GWS05_TD.clusterCoef, GWS05_TD.degreeCent$centralization,
                        GWS05_TD.netDensity, GWS05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
GWS05_QTg2 <- data.frame(GWS05_QT)
GWS05_QTg2 <- GWS05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS05_QTg2$player1
player2vector <- GWS05_QTg2$player2
GWS05_QTg3 <- GWS05_QTg2
GWS05_QTg3$p1inp2vec <- is.element(GWS05_QTg3$player1, player2vector)
GWS05_QTg3$p2inp1vec <- is.element(GWS05_QTg3$player2, player1vector)

addPlayer1 <- GWS05_QTg3[ which(GWS05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS05_QTg3[ which(GWS05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS05_QTg2 <- rbind(GWS05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
GWS05_QTft <- ftable(GWS05_QTg2$player1, GWS05_QTg2$player2)
GWS05_QTft2 <- as.matrix(GWS05_QTft)
numRows <- nrow(GWS05_QTft2)
numCols <- ncol(GWS05_QTft2)
GWS05_QTft3 <- GWS05_QTft2[c(2:numRows) , c(2:numCols)]
GWS05_QTTable <- graph.adjacency(GWS05_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(GWS05_QTTable, vertex.label = V(GWS05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
GWS05_QT.clusterCoef <- transitivity(GWS05_QTTable, type="global") #cluster coefficient
GWS05_QT.degreeCent <- centralization.degree(GWS05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS05_QTftn <- as.network.matrix(GWS05_QTft)
GWS05_QT.netDensity <- network.density(GWS05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS05_QT.entropy <- entropy(GWS05_QTft) #entropy

GWS05_QT.netMx <- cbind(GWS05_QT.netMx, GWS05_QT.clusterCoef, GWS05_QT.degreeCent$centralization,
                        GWS05_QT.netDensity, GWS05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS05_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "HAW"
KIoutcome = "Goal_F"
HAW05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
HAW05_Gg2 <- data.frame(HAW05_G)
HAW05_Gg2 <- HAW05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_Gg2$player1
player2vector <- HAW05_Gg2$player2
HAW05_Gg3 <- HAW05_Gg2
HAW05_Gg3$p1inp2vec <- is.element(HAW05_Gg3$player1, player2vector)
HAW05_Gg3$p2inp1vec <- is.element(HAW05_Gg3$player2, player1vector)

addPlayer1 <- HAW05_Gg3[ which(HAW05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_Gg3[ which(HAW05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_Gg2 <- rbind(HAW05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
HAW05_Gft <- ftable(HAW05_Gg2$player1, HAW05_Gg2$player2)
HAW05_Gft2 <- as.matrix(HAW05_Gft)
numRows <- nrow(HAW05_Gft2)
numCols <- ncol(HAW05_Gft2)
HAW05_Gft3 <- HAW05_Gft2[c(2:numRows) , c(2:numCols)]
HAW05_GTable <- graph.adjacency(HAW05_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(HAW05_GTable, vertex.label = V(HAW05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
HAW05_G.clusterCoef <- transitivity(HAW05_GTable, type="global") #cluster coefficient
HAW05_G.degreeCent <- centralization.degree(HAW05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_Gftn <- as.network.matrix(HAW05_Gft)
HAW05_G.netDensity <- network.density(HAW05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_G.entropy <- entropy(HAW05_Gft) #entropy

HAW05_G.netMx <- cbind(HAW05_G.netMx, HAW05_G.clusterCoef, HAW05_G.degreeCent$centralization,
                       HAW05_G.netDensity, HAW05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************

round = 5
teamName = "HAW"
KIoutcome = "Behind_F"
HAW05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
HAW05_Bg2 <- data.frame(HAW05_B)
HAW05_Bg2 <- HAW05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_Bg2$player1
player2vector <- HAW05_Bg2$player2
HAW05_Bg3 <- HAW05_Bg2
HAW05_Bg3$p1inp2vec <- is.element(HAW05_Bg3$player1, player2vector)
HAW05_Bg3$p2inp1vec <- is.element(HAW05_Bg3$player2, player1vector)

addPlayer1 <- HAW05_Bg3[ which(HAW05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_Bg3[ which(HAW05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_Bg2 <- rbind(HAW05_Bg2, addPlayers)


#ROUND 5, Behind graph using weighted edges
HAW05_Bft <- ftable(HAW05_Bg2$player1, HAW05_Bg2$player2)
HAW05_Bft2 <- as.matrix(HAW05_Bft)
numRows <- nrow(HAW05_Bft2)
numCols <- ncol(HAW05_Bft2)
HAW05_Bft3 <- HAW05_Bft2[c(2:numRows) , c(2:numCols)]
HAW05_BTable <- graph.adjacency(HAW05_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(HAW05_BTable, vertex.label = V(HAW05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
HAW05_B.clusterCoef <- transitivity(HAW05_BTable, type="global") #cluster coefficient
HAW05_B.degreeCent <- centralization.degree(HAW05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_Bftn <- as.network.matrix(HAW05_Bft)
HAW05_B.netDensity <- network.density(HAW05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_B.entropy <- entropy(HAW05_Bft) #entropy

HAW05_B.netMx <- cbind(HAW05_B.netMx, HAW05_B.clusterCoef, HAW05_B.degreeCent$centralization,
                       HAW05_B.netDensity, HAW05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
HAW05_SFg2 <- data.frame(HAW05_SF)
HAW05_SFg2 <- HAW05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_SFg2$player1
player2vector <- HAW05_SFg2$player2
HAW05_SFg3 <- HAW05_SFg2
HAW05_SFg3$p1inp2vec <- is.element(HAW05_SFg3$player1, player2vector)
HAW05_SFg3$p2inp1vec <- is.element(HAW05_SFg3$player2, player1vector)

addPlayer1 <- HAW05_SFg3[ which(HAW05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_SFg3[ which(HAW05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_SFg2 <- rbind(HAW05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
HAW05_SFft <- ftable(HAW05_SFg2$player1, HAW05_SFg2$player2)
HAW05_SFft2 <- as.matrix(HAW05_SFft)
numRows <- nrow(HAW05_SFft2)
numCols <- ncol(HAW05_SFft2)
HAW05_SFft3 <- HAW05_SFft2[c(2:numRows) , c(2:numCols)]
HAW05_SFTable <- graph.adjacency(HAW05_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(HAW05_SFTable, vertex.label = V(HAW05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
HAW05_SF.clusterCoef <- transitivity(HAW05_SFTable, type="global") #cluster coefficient
HAW05_SF.degreeCent <- centralization.degree(HAW05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_SFftn <- as.network.matrix(HAW05_SFft)
HAW05_SF.netDensity <- network.density(HAW05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_SF.entropy <- entropy(HAW05_SFft) #entropy

HAW05_SF.netMx <- cbind(HAW05_SF.netMx, HAW05_SF.clusterCoef, HAW05_SF.degreeCent$centralization,
                        HAW05_SF.netDensity, HAW05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************

round = 5
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
HAW05_TFg2 <- data.frame(HAW05_TF)
HAW05_TFg2 <- HAW05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_TFg2$player1
player2vector <- HAW05_TFg2$player2
HAW05_TFg3 <- HAW05_TFg2
HAW05_TFg3$p1inp2vec <- is.element(HAW05_TFg3$player1, player2vector)
HAW05_TFg3$p2inp1vec <- is.element(HAW05_TFg3$player2, player1vector)

addPlayer1 <- HAW05_TFg3[ which(HAW05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_TFg3[ which(HAW05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_TFg2 <- rbind(HAW05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
HAW05_TFft <- ftable(HAW05_TFg2$player1, HAW05_TFg2$player2)
HAW05_TFft2 <- as.matrix(HAW05_TFft)
numRows <- nrow(HAW05_TFft2)
numCols <- ncol(HAW05_TFft2)
HAW05_TFft3 <- HAW05_TFft2[c(2:numRows) , c(2:numCols)]
HAW05_TFTable <- graph.adjacency(HAW05_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(HAW05_TFTable, vertex.label = V(HAW05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
HAW05_TF.clusterCoef <- transitivity(HAW05_TFTable, type="global") #cluster coefficient
HAW05_TF.degreeCent <- centralization.degree(HAW05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_TFftn <- as.network.matrix(HAW05_TFft)
HAW05_TF.netDensity <- network.density(HAW05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_TF.entropy <- entropy(HAW05_TFft) #entropy

HAW05_TF.netMx <- cbind(HAW05_TF.netMx, HAW05_TF.clusterCoef, HAW05_TF.degreeCent$centralization,
                        HAW05_TF.netDensity, HAW05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
HAW05_SAMg2 <- data.frame(HAW05_SAM)
HAW05_SAMg2 <- HAW05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_SAMg2$player1
player2vector <- HAW05_SAMg2$player2
HAW05_SAMg3 <- HAW05_SAMg2
HAW05_SAMg3$p1inp2vec <- is.element(HAW05_SAMg3$player1, player2vector)
HAW05_SAMg3$p2inp1vec <- is.element(HAW05_SAMg3$player2, player1vector)

addPlayer1 <- HAW05_SAMg3[ which(HAW05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_SAMg3[ which(HAW05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_SAMg2 <- rbind(HAW05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
HAW05_SAMft <- ftable(HAW05_SAMg2$player1, HAW05_SAMg2$player2)
HAW05_SAMft2 <- as.matrix(HAW05_SAMft)
numRows <- nrow(HAW05_SAMft2)
numCols <- ncol(HAW05_SAMft2)
HAW05_SAMft3 <- HAW05_SAMft2[c(2:numRows) , c(2:numCols)]
HAW05_SAMTable <- graph.adjacency(HAW05_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(HAW05_SAMTable, vertex.label = V(HAW05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
HAW05_SAM.clusterCoef <- transitivity(HAW05_SAMTable, type="global") #cluster coefficient
HAW05_SAM.degreeCent <- centralization.degree(HAW05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_SAMftn <- as.network.matrix(HAW05_SAMft)
HAW05_SAM.netDensity <- network.density(HAW05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_SAM.entropy <- entropy(HAW05_SAMft) #entropy

HAW05_SAM.netMx <- cbind(HAW05_SAM.netMx, HAW05_SAM.clusterCoef, HAW05_SAM.degreeCent$centralization,
                         HAW05_SAM.netDensity, HAW05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
HAW05_TAMg2 <- data.frame(HAW05_TAM)
HAW05_TAMg2 <- HAW05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_TAMg2$player1
player2vector <- HAW05_TAMg2$player2
HAW05_TAMg3 <- HAW05_TAMg2
HAW05_TAMg3$p1inp2vec <- is.element(HAW05_TAMg3$player1, player2vector)
HAW05_TAMg3$p2inp1vec <- is.element(HAW05_TAMg3$player2, player1vector)

addPlayer1 <- HAW05_TAMg3[ which(HAW05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_TAMg3[ which(HAW05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_TAMg2 <- rbind(HAW05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
HAW05_TAMft <- ftable(HAW05_TAMg2$player1, HAW05_TAMg2$player2)
HAW05_TAMft2 <- as.matrix(HAW05_TAMft)
numRows <- nrow(HAW05_TAMft2)
numCols <- ncol(HAW05_TAMft2)
HAW05_TAMft3 <- HAW05_TAMft2[c(2:numRows) , c(2:numCols)]
HAW05_TAMTable <- graph.adjacency(HAW05_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(HAW05_TAMTable, vertex.label = V(HAW05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
HAW05_TAM.clusterCoef <- transitivity(HAW05_TAMTable, type="global") #cluster coefficient
HAW05_TAM.degreeCent <- centralization.degree(HAW05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_TAMftn <- as.network.matrix(HAW05_TAMft)
HAW05_TAM.netDensity <- network.density(HAW05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_TAM.entropy <- entropy(HAW05_TAMft) #entropy

HAW05_TAM.netMx <- cbind(HAW05_TAM.netMx, HAW05_TAM.clusterCoef, HAW05_TAM.degreeCent$centralization,
                         HAW05_TAM.netDensity, HAW05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
HAW05_SDMg2 <- data.frame(HAW05_SDM)
HAW05_SDMg2 <- HAW05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_SDMg2$player1
player2vector <- HAW05_SDMg2$player2
HAW05_SDMg3 <- HAW05_SDMg2
HAW05_SDMg3$p1inp2vec <- is.element(HAW05_SDMg3$player1, player2vector)
HAW05_SDMg3$p2inp1vec <- is.element(HAW05_SDMg3$player2, player1vector)

addPlayer1 <- HAW05_SDMg3[ which(HAW05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW05_SDMg3[ which(HAW05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_SDMg2 <- rbind(HAW05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
HAW05_SDMft <- ftable(HAW05_SDMg2$player1, HAW05_SDMg2$player2)
HAW05_SDMft2 <- as.matrix(HAW05_SDMft)
numRows <- nrow(HAW05_SDMft2)
numCols <- ncol(HAW05_SDMft2)
HAW05_SDMft3 <- HAW05_SDMft2[c(2:numRows) , c(2:numCols)]
HAW05_SDMTable <- graph.adjacency(HAW05_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(HAW05_SDMTable, vertex.label = V(HAW05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
HAW05_SDM.clusterCoef <- transitivity(HAW05_SDMTable, type="global") #cluster coefficient
HAW05_SDM.degreeCent <- centralization.degree(HAW05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_SDMftn <- as.network.matrix(HAW05_SDMft)
HAW05_SDM.netDensity <- network.density(HAW05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_SDM.entropy <- entropy(HAW05_SDMft) #entropy

HAW05_SDM.netMx <- cbind(HAW05_SDM.netMx, HAW05_SDM.clusterCoef, HAW05_SDM.degreeCent$centralization,
                         HAW05_SDM.netDensity, HAW05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************
#NA

round = 5
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
HAW05_TDMg2 <- data.frame(HAW05_TDM)
HAW05_TDMg2 <- HAW05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_TDMg2$player1
player2vector <- HAW05_TDMg2$player2
HAW05_TDMg3 <- HAW05_TDMg2
HAW05_TDMg3$p1inp2vec <- is.element(HAW05_TDMg3$player1, player2vector)
HAW05_TDMg3$p2inp1vec <- is.element(HAW05_TDMg3$player2, player1vector)

addPlayer1 <- HAW05_TDMg3[ which(HAW05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_TDMg3[ which(HAW05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_TDMg2 <- rbind(HAW05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
HAW05_TDMft <- ftable(HAW05_TDMg2$player1, HAW05_TDMg2$player2)
HAW05_TDMft2 <- as.matrix(HAW05_TDMft)
numRows <- nrow(HAW05_TDMft2)
numCols <- ncol(HAW05_TDMft2)
HAW05_TDMft3 <- HAW05_TDMft2[c(2:numRows) , c(2:numCols)]
HAW05_TDMTable <- graph.adjacency(HAW05_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(HAW05_TDMTable, vertex.label = V(HAW05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
HAW05_TDM.clusterCoef <- transitivity(HAW05_TDMTable, type="global") #cluster coefficient
HAW05_TDM.degreeCent <- centralization.degree(HAW05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_TDMftn <- as.network.matrix(HAW05_TDMft)
HAW05_TDM.netDensity <- network.density(HAW05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_TDM.entropy <- entropy(HAW05_TDMft) #entropy

HAW05_TDM.netMx <- cbind(HAW05_TDM.netMx, HAW05_TDM.clusterCoef, HAW05_TDM.degreeCent$centralization,
                         HAW05_TDM.netDensity, HAW05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
HAW05_SDg2 <- data.frame(HAW05_SD)
HAW05_SDg2 <- HAW05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_SDg2$player1
player2vector <- HAW05_SDg2$player2
HAW05_SDg3 <- HAW05_SDg2
HAW05_SDg3$p1inp2vec <- is.element(HAW05_SDg3$player1, player2vector)
HAW05_SDg3$p2inp1vec <- is.element(HAW05_SDg3$player2, player1vector)

addPlayer1 <- HAW05_SDg3[ which(HAW05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_SDg3[ which(HAW05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_SDg2 <- rbind(HAW05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
HAW05_SDft <- ftable(HAW05_SDg2$player1, HAW05_SDg2$player2)
HAW05_SDft2 <- as.matrix(HAW05_SDft)
numRows <- nrow(HAW05_SDft2)
numCols <- ncol(HAW05_SDft2)
HAW05_SDft3 <- HAW05_SDft2[c(2:numRows) , c(2:numCols)]
HAW05_SDTable <- graph.adjacency(HAW05_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(HAW05_SDTable, vertex.label = V(HAW05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
HAW05_SD.clusterCoef <- transitivity(HAW05_SDTable, type="global") #cluster coefficient
HAW05_SD.degreeCent <- centralization.degree(HAW05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_SDftn <- as.network.matrix(HAW05_SDft)
HAW05_SD.netDensity <- network.density(HAW05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_SD.entropy <- entropy(HAW05_SDft) #entropy

HAW05_SD.netMx <- cbind(HAW05_SD.netMx, HAW05_SD.clusterCoef, HAW05_SD.degreeCent$centralization,
                        HAW05_SD.netDensity, HAW05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
HAW05_TDg2 <- data.frame(HAW05_TD)
HAW05_TDg2 <- HAW05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_TDg2$player1
player2vector <- HAW05_TDg2$player2
HAW05_TDg3 <- HAW05_TDg2
HAW05_TDg3$p1inp2vec <- is.element(HAW05_TDg3$player1, player2vector)
HAW05_TDg3$p2inp1vec <- is.element(HAW05_TDg3$player2, player1vector)

addPlayer1 <- HAW05_TDg3[ which(HAW05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_TDg3[ which(HAW05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_TDg2 <- rbind(HAW05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
HAW05_TDft <- ftable(HAW05_TDg2$player1, HAW05_TDg2$player2)
HAW05_TDft2 <- as.matrix(HAW05_TDft)
numRows <- nrow(HAW05_TDft2)
numCols <- ncol(HAW05_TDft2)
HAW05_TDft3 <- HAW05_TDft2[c(2:numRows) , c(2:numCols)]
HAW05_TDTable <- graph.adjacency(HAW05_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(HAW05_TDTable, vertex.label = V(HAW05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
HAW05_TD.clusterCoef <- transitivity(HAW05_TDTable, type="global") #cluster coefficient
HAW05_TD.degreeCent <- centralization.degree(HAW05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_TDftn <- as.network.matrix(HAW05_TDft)
HAW05_TD.netDensity <- network.density(HAW05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_TD.entropy <- entropy(HAW05_TDft) #entropy

HAW05_TD.netMx <- cbind(HAW05_TD.netMx, HAW05_TD.clusterCoef, HAW05_TD.degreeCent$centralization,
                        HAW05_TD.netDensity, HAW05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
HAW05_QTg2 <- data.frame(HAW05_QT)
HAW05_QTg2 <- HAW05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW05_QTg2$player1
player2vector <- HAW05_QTg2$player2
HAW05_QTg3 <- HAW05_QTg2
HAW05_QTg3$p1inp2vec <- is.element(HAW05_QTg3$player1, player2vector)
HAW05_QTg3$p2inp1vec <- is.element(HAW05_QTg3$player2, player1vector)

addPlayer1 <- HAW05_QTg3[ which(HAW05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW05_QTg3[ which(HAW05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW05_QTg2 <- rbind(HAW05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
HAW05_QTft <- ftable(HAW05_QTg2$player1, HAW05_QTg2$player2)
HAW05_QTft2 <- as.matrix(HAW05_QTft)
numRows <- nrow(HAW05_QTft2)
numCols <- ncol(HAW05_QTft2)
HAW05_QTft3 <- HAW05_QTft2[c(2:numRows) , c(2:numCols)]
HAW05_QTTable <- graph.adjacency(HAW05_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(HAW05_QTTable, vertex.label = V(HAW05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
HAW05_QT.clusterCoef <- transitivity(HAW05_QTTable, type="global") #cluster coefficient
HAW05_QT.degreeCent <- centralization.degree(HAW05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW05_QTftn <- as.network.matrix(HAW05_QTft)
HAW05_QT.netDensity <- network.density(HAW05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW05_QT.entropy <- entropy(HAW05_QTft) #entropy

HAW05_QT.netMx <- cbind(HAW05_QT.netMx, HAW05_QT.clusterCoef, HAW05_QT.degreeCent$centralization,
                        HAW05_QT.netDensity, HAW05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW05_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************
#NA

round = 5
teamName = "MELB"
KIoutcome = "Goal_F"
MELB05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
MELB05_Gg2 <- data.frame(MELB05_G)
MELB05_Gg2 <- MELB05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_Gg2$player1
player2vector <- MELB05_Gg2$player2
MELB05_Gg3 <- MELB05_Gg2
MELB05_Gg3$p1inp2vec <- is.element(MELB05_Gg3$player1, player2vector)
MELB05_Gg3$p2inp1vec <- is.element(MELB05_Gg3$player2, player1vector)

addPlayer1 <- MELB05_Gg3[ which(MELB05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB05_Gg2 <- rbind(MELB05_Gg2, addPlayer1)

#ROUND 5, Goal graph using weighted edges
MELB05_Gft <- ftable(MELB05_Gg2$player1, MELB05_Gg2$player2)
MELB05_Gft2 <- as.matrix(MELB05_Gft)
numRows <- nrow(MELB05_Gft2)
numCols <- ncol(MELB05_Gft2)
MELB05_Gft3 <- MELB05_Gft2[c(2:numRows) , c(1:numCols)]
MELB05_GTable <- graph.adjacency(MELB05_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(MELB05_GTable, vertex.label = V(MELB05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
MELB05_G.clusterCoef <- transitivity(MELB05_GTable, type="global") #cluster coefficient
MELB05_G.degreeCent <- centralization.degree(MELB05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_Gftn <- as.network.matrix(MELB05_Gft)
MELB05_G.netDensity <- network.density(MELB05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_G.entropy <- entropy(MELB05_Gft) #entropy

MELB05_G.netMx <- cbind(MELB05_G.netMx, MELB05_G.clusterCoef, MELB05_G.degreeCent$centralization,
                        MELB05_G.netDensity, MELB05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************
#NA

round = 5
teamName = "MELB"
KIoutcome = "Behind_F"
MELB05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
MELB05_Bg2 <- data.frame(MELB05_B)
MELB05_Bg2 <- MELB05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_Bg2$player1
player2vector <- MELB05_Bg2$player2
MELB05_Bg3 <- MELB05_Bg2
MELB05_Bg3$p1inp2vec <- is.element(MELB05_Bg3$player1, player2vector)
MELB05_Bg3$p2inp1vec <- is.element(MELB05_Bg3$player2, player1vector)

addPlayer1 <- MELB05_Bg3[ which(MELB05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_Bg3[ which(MELB05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_Bg2 <- rbind(MELB05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
MELB05_Bft <- ftable(MELB05_Bg2$player1, MELB05_Bg2$player2)
MELB05_Bft2 <- as.matrix(MELB05_Bft)
numRows <- nrow(MELB05_Bft2)
numCols <- ncol(MELB05_Bft2)
MELB05_Bft3 <- MELB05_Bft2[c(2:numRows) , c(2:numCols)]
MELB05_BTable <- graph.adjacency(MELB05_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(MELB05_BTable, vertex.label = V(MELB05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
MELB05_B.clusterCoef <- transitivity(MELB05_BTable, type="global") #cluster coefficient
MELB05_B.degreeCent <- centralization.degree(MELB05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_Bftn <- as.network.matrix(MELB05_Bft)
MELB05_B.netDensity <- network.density(MELB05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_B.entropy <- entropy(MELB05_Bft) #entropy

MELB05_B.netMx <- cbind(MELB05_B.netMx, MELB05_B.clusterCoef, MELB05_B.degreeCent$centralization,
                        MELB05_B.netDensity, MELB05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************

round = 5
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
MELB05_SFg2 <- data.frame(MELB05_SF)
MELB05_SFg2 <- MELB05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_SFg2$player1
player2vector <- MELB05_SFg2$player2
MELB05_SFg3 <- MELB05_SFg2
MELB05_SFg3$p1inp2vec <- is.element(MELB05_SFg3$player1, player2vector)
MELB05_SFg3$p2inp1vec <- is.element(MELB05_SFg3$player2, player1vector)

addPlayer1 <- MELB05_SFg3[ which(MELB05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_SFg3[ which(MELB05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_SFg2 <- rbind(MELB05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
MELB05_SFft <- ftable(MELB05_SFg2$player1, MELB05_SFg2$player2)
MELB05_SFft2 <- as.matrix(MELB05_SFft)
numRows <- nrow(MELB05_SFft2)
numCols <- ncol(MELB05_SFft2)
MELB05_SFft3 <- MELB05_SFft2[c(2:numRows) , c(2:numCols)]
MELB05_SFTable <- graph.adjacency(MELB05_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(MELB05_SFTable, vertex.label = V(MELB05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
MELB05_SF.clusterCoef <- transitivity(MELB05_SFTable, type="global") #cluster coefficient
MELB05_SF.degreeCent <- centralization.degree(MELB05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_SFftn <- as.network.matrix(MELB05_SFft)
MELB05_SF.netDensity <- network.density(MELB05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_SF.entropy <- entropy(MELB05_SFft) #entropy

MELB05_SF.netMx <- cbind(MELB05_SF.netMx, MELB05_SF.clusterCoef, MELB05_SF.degreeCent$centralization,
                         MELB05_SF.netDensity, MELB05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************
#NA

round = 5
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
MELB05_TFg2 <- data.frame(MELB05_TF)
MELB05_TFg2 <- MELB05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_TFg2$player1
player2vector <- MELB05_TFg2$player2
MELB05_TFg3 <- MELB05_TFg2
MELB05_TFg3$p1inp2vec <- is.element(MELB05_TFg3$player1, player2vector)
MELB05_TFg3$p2inp1vec <- is.element(MELB05_TFg3$player2, player1vector)

addPlayer1 <- MELB05_TFg3[ which(MELB05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_TFg3[ which(MELB05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_TFg2 <- rbind(MELB05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
MELB05_TFft <- ftable(MELB05_TFg2$player1, MELB05_TFg2$player2)
MELB05_TFft2 <- as.matrix(MELB05_TFft)
numRows <- nrow(MELB05_TFft2)
numCols <- ncol(MELB05_TFft2)
MELB05_TFft3 <- MELB05_TFft2[c(2:numRows) , c(2:numCols)]
MELB05_TFTable <- graph.adjacency(MELB05_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(MELB05_TFTable, vertex.label = V(MELB05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
MELB05_TF.clusterCoef <- transitivity(MELB05_TFTable, type="global") #cluster coefficient
MELB05_TF.degreeCent <- centralization.degree(MELB05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_TFftn <- as.network.matrix(MELB05_TFft)
MELB05_TF.netDensity <- network.density(MELB05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_TF.entropy <- entropy(MELB05_TFft) #entropy

MELB05_TF.netMx <- cbind(MELB05_TF.netMx, MELB05_TF.clusterCoef, MELB05_TF.degreeCent$centralization,
                         MELB05_TF.netDensity, MELB05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
MELB05_SAMg2 <- data.frame(MELB05_SAM)
MELB05_SAMg2 <- MELB05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_SAMg2$player1
player2vector <- MELB05_SAMg2$player2
MELB05_SAMg3 <- MELB05_SAMg2
MELB05_SAMg3$p1inp2vec <- is.element(MELB05_SAMg3$player1, player2vector)
MELB05_SAMg3$p2inp1vec <- is.element(MELB05_SAMg3$player2, player1vector)

addPlayer1 <- MELB05_SAMg3[ which(MELB05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_SAMg3[ which(MELB05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_SAMg2 <- rbind(MELB05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
MELB05_SAMft <- ftable(MELB05_SAMg2$player1, MELB05_SAMg2$player2)
MELB05_SAMft2 <- as.matrix(MELB05_SAMft)
numRows <- nrow(MELB05_SAMft2)
numCols <- ncol(MELB05_SAMft2)
MELB05_SAMft3 <- MELB05_SAMft2[c(2:numRows) , c(2:numCols)]
MELB05_SAMTable <- graph.adjacency(MELB05_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(MELB05_SAMTable, vertex.label = V(MELB05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
MELB05_SAM.clusterCoef <- transitivity(MELB05_SAMTable, type="global") #cluster coefficient
MELB05_SAM.degreeCent <- centralization.degree(MELB05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_SAMftn <- as.network.matrix(MELB05_SAMft)
MELB05_SAM.netDensity <- network.density(MELB05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_SAM.entropy <- entropy(MELB05_SAMft) #entropy

MELB05_SAM.netMx <- cbind(MELB05_SAM.netMx, MELB05_SAM.clusterCoef, MELB05_SAM.degreeCent$centralization,
                          MELB05_SAM.netDensity, MELB05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
MELB05_TAMg2 <- data.frame(MELB05_TAM)
MELB05_TAMg2 <- MELB05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_TAMg2$player1
player2vector <- MELB05_TAMg2$player2
MELB05_TAMg3 <- MELB05_TAMg2
MELB05_TAMg3$p1inp2vec <- is.element(MELB05_TAMg3$player1, player2vector)
MELB05_TAMg3$p2inp1vec <- is.element(MELB05_TAMg3$player2, player1vector)

addPlayer1 <- MELB05_TAMg3[ which(MELB05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_TAMg3[ which(MELB05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_TAMg2 <- rbind(MELB05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
MELB05_TAMft <- ftable(MELB05_TAMg2$player1, MELB05_TAMg2$player2)
MELB05_TAMft2 <- as.matrix(MELB05_TAMft)
numRows <- nrow(MELB05_TAMft2)
numCols <- ncol(MELB05_TAMft2)
MELB05_TAMft3 <- MELB05_TAMft2[c(2:numRows) , c(2:numCols)]
MELB05_TAMTable <- graph.adjacency(MELB05_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(MELB05_TAMTable, vertex.label = V(MELB05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
MELB05_TAM.clusterCoef <- transitivity(MELB05_TAMTable, type="global") #cluster coefficient
MELB05_TAM.degreeCent <- centralization.degree(MELB05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_TAMftn <- as.network.matrix(MELB05_TAMft)
MELB05_TAM.netDensity <- network.density(MELB05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_TAM.entropy <- entropy(MELB05_TAMft) #entropy

MELB05_TAM.netMx <- cbind(MELB05_TAM.netMx, MELB05_TAM.clusterCoef, MELB05_TAM.degreeCent$centralization,
                          MELB05_TAM.netDensity, MELB05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
MELB05_SDMg2 <- data.frame(MELB05_SDM)
MELB05_SDMg2 <- MELB05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_SDMg2$player1
player2vector <- MELB05_SDMg2$player2
MELB05_SDMg3 <- MELB05_SDMg2
MELB05_SDMg3$p1inp2vec <- is.element(MELB05_SDMg3$player1, player2vector)
MELB05_SDMg3$p2inp1vec <- is.element(MELB05_SDMg3$player2, player1vector)

addPlayer1 <- MELB05_SDMg3[ which(MELB05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_SDMg3[ which(MELB05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_SDMg2 <- rbind(MELB05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
MELB05_SDMft <- ftable(MELB05_SDMg2$player1, MELB05_SDMg2$player2)
MELB05_SDMft2 <- as.matrix(MELB05_SDMft)
numRows <- nrow(MELB05_SDMft2)
numCols <- ncol(MELB05_SDMft2)
MELB05_SDMft3 <- MELB05_SDMft2[c(2:numRows) , c(2:numCols)]
MELB05_SDMTable <- graph.adjacency(MELB05_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(MELB05_SDMTable, vertex.label = V(MELB05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
MELB05_SDM.clusterCoef <- transitivity(MELB05_SDMTable, type="global") #cluster coefficient
MELB05_SDM.degreeCent <- centralization.degree(MELB05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_SDMftn <- as.network.matrix(MELB05_SDMft)
MELB05_SDM.netDensity <- network.density(MELB05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_SDM.entropy <- entropy(MELB05_SDMft) #entropy

MELB05_SDM.netMx <- cbind(MELB05_SDM.netMx, MELB05_SDM.clusterCoef, MELB05_SDM.degreeCent$centralization,
                          MELB05_SDM.netDensity, MELB05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
MELB05_TDMg2 <- data.frame(MELB05_TDM)
MELB05_TDMg2 <- MELB05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_TDMg2$player1
player2vector <- MELB05_TDMg2$player2
MELB05_TDMg3 <- MELB05_TDMg2
MELB05_TDMg3$p1inp2vec <- is.element(MELB05_TDMg3$player1, player2vector)
MELB05_TDMg3$p2inp1vec <- is.element(MELB05_TDMg3$player2, player1vector)

addPlayer1 <- MELB05_TDMg3[ which(MELB05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_TDMg3[ which(MELB05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_TDMg2 <- rbind(MELB05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
MELB05_TDMft <- ftable(MELB05_TDMg2$player1, MELB05_TDMg2$player2)
MELB05_TDMft2 <- as.matrix(MELB05_TDMft)
numRows <- nrow(MELB05_TDMft2)
numCols <- ncol(MELB05_TDMft2)
MELB05_TDMft3 <- MELB05_TDMft2[c(2:numRows) , c(2:numCols)]
MELB05_TDMTable <- graph.adjacency(MELB05_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(MELB05_TDMTable, vertex.label = V(MELB05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
MELB05_TDM.clusterCoef <- transitivity(MELB05_TDMTable, type="global") #cluster coefficient
MELB05_TDM.degreeCent <- centralization.degree(MELB05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_TDMftn <- as.network.matrix(MELB05_TDMft)
MELB05_TDM.netDensity <- network.density(MELB05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_TDM.entropy <- entropy(MELB05_TDMft) #entropy

MELB05_TDM.netMx <- cbind(MELB05_TDM.netMx, MELB05_TDM.clusterCoef, MELB05_TDM.degreeCent$centralization,
                          MELB05_TDM.netDensity, MELB05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
MELB05_SDg2 <- data.frame(MELB05_SD)
MELB05_SDg2 <- MELB05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_SDg2$player1
player2vector <- MELB05_SDg2$player2
MELB05_SDg3 <- MELB05_SDg2
MELB05_SDg3$p1inp2vec <- is.element(MELB05_SDg3$player1, player2vector)
MELB05_SDg3$p2inp1vec <- is.element(MELB05_SDg3$player2, player1vector)

addPlayer1 <- MELB05_SDg3[ which(MELB05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_SDg3[ which(MELB05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_SDg2 <- rbind(MELB05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
MELB05_SDft <- ftable(MELB05_SDg2$player1, MELB05_SDg2$player2)
MELB05_SDft2 <- as.matrix(MELB05_SDft)
numRows <- nrow(MELB05_SDft2)
numCols <- ncol(MELB05_SDft2)
MELB05_SDft3 <- MELB05_SDft2[c(2:numRows) , c(2:numCols)]
MELB05_SDTable <- graph.adjacency(MELB05_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(MELB05_SDTable, vertex.label = V(MELB05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
MELB05_SD.clusterCoef <- transitivity(MELB05_SDTable, type="global") #cluster coefficient
MELB05_SD.degreeCent <- centralization.degree(MELB05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_SDftn <- as.network.matrix(MELB05_SDft)
MELB05_SD.netDensity <- network.density(MELB05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_SD.entropy <- entropy(MELB05_SDft) #entropy

MELB05_SD.netMx <- cbind(MELB05_SD.netMx, MELB05_SD.clusterCoef, MELB05_SD.degreeCent$centralization,
                         MELB05_SD.netDensity, MELB05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
MELB05_TDg2 <- data.frame(MELB05_TD)
MELB05_TDg2 <- MELB05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_TDg2$player1
player2vector <- MELB05_TDg2$player2
MELB05_TDg3 <- MELB05_TDg2
MELB05_TDg3$p1inp2vec <- is.element(MELB05_TDg3$player1, player2vector)
MELB05_TDg3$p2inp1vec <- is.element(MELB05_TDg3$player2, player1vector)

addPlayer1 <- MELB05_TDg3[ which(MELB05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_TDg3[ which(MELB05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_TDg2 <- rbind(MELB05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
MELB05_TDft <- ftable(MELB05_TDg2$player1, MELB05_TDg2$player2)
MELB05_TDft2 <- as.matrix(MELB05_TDft)
numRows <- nrow(MELB05_TDft2)
numCols <- ncol(MELB05_TDft2)
MELB05_TDft3 <- MELB05_TDft2[c(2:numRows) , c(2:numCols)]
MELB05_TDTable <- graph.adjacency(MELB05_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(MELB05_TDTable, vertex.label = V(MELB05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
MELB05_TD.clusterCoef <- transitivity(MELB05_TDTable, type="global") #cluster coefficient
MELB05_TD.degreeCent <- centralization.degree(MELB05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_TDftn <- as.network.matrix(MELB05_TDft)
MELB05_TD.netDensity <- network.density(MELB05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_TD.entropy <- entropy(MELB05_TDft) #entropy

MELB05_TD.netMx <- cbind(MELB05_TD.netMx, MELB05_TD.clusterCoef, MELB05_TD.degreeCent$centralization,
                         MELB05_TD.netDensity, MELB05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
MELB05_QTg2 <- data.frame(MELB05_QT)
MELB05_QTg2 <- MELB05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB05_QTg2$player1
player2vector <- MELB05_QTg2$player2
MELB05_QTg3 <- MELB05_QTg2
MELB05_QTg3$p1inp2vec <- is.element(MELB05_QTg3$player1, player2vector)
MELB05_QTg3$p2inp1vec <- is.element(MELB05_QTg3$player2, player1vector)

addPlayer1 <- MELB05_QTg3[ which(MELB05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB05_QTg3[ which(MELB05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB05_QTg2 <- rbind(MELB05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
MELB05_QTft <- ftable(MELB05_QTg2$player1, MELB05_QTg2$player2)
MELB05_QTft2 <- as.matrix(MELB05_QTft)
numRows <- nrow(MELB05_QTft2)
numCols <- ncol(MELB05_QTft2)
MELB05_QTft3 <- MELB05_QTft2[c(2:numRows) , c(2:numCols)]
MELB05_QTTable <- graph.adjacency(MELB05_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(MELB05_QTTable, vertex.label = V(MELB05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
MELB05_QT.clusterCoef <- transitivity(MELB05_QTTable, type="global") #cluster coefficient
MELB05_QT.degreeCent <- centralization.degree(MELB05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB05_QTftn <- as.network.matrix(MELB05_QTft)
MELB05_QT.netDensity <- network.density(MELB05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB05_QT.entropy <- entropy(MELB05_QTft) #entropy

MELB05_QT.netMx <- cbind(MELB05_QT.netMx, MELB05_QT.clusterCoef, MELB05_QT.degreeCent$centralization,
                         MELB05_QT.netDensity, MELB05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB05_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
NMFC05_Gg2 <- data.frame(NMFC05_G)
NMFC05_Gg2 <- NMFC05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_Gg2$player1
player2vector <- NMFC05_Gg2$player2
NMFC05_Gg3 <- NMFC05_Gg2
NMFC05_Gg3$p1inp2vec <- is.element(NMFC05_Gg3$player1, player2vector)
NMFC05_Gg3$p2inp1vec <- is.element(NMFC05_Gg3$player2, player1vector)

addPlayer1 <- NMFC05_Gg3[ which(NMFC05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_Gg3[ which(NMFC05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_Gg2 <- rbind(NMFC05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
NMFC05_Gft <- ftable(NMFC05_Gg2$player1, NMFC05_Gg2$player2)
NMFC05_Gft2 <- as.matrix(NMFC05_Gft)
numRows <- nrow(NMFC05_Gft2)
numCols <- ncol(NMFC05_Gft2)
NMFC05_Gft3 <- NMFC05_Gft2[c(2:numRows) , c(2:numCols)]
NMFC05_GTable <- graph.adjacency(NMFC05_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(NMFC05_GTable, vertex.label = V(NMFC05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
NMFC05_G.clusterCoef <- transitivity(NMFC05_GTable, type="global") #cluster coefficient
NMFC05_G.degreeCent <- centralization.degree(NMFC05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_Gftn <- as.network.matrix(NMFC05_Gft)
NMFC05_G.netDensity <- network.density(NMFC05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_G.entropy <- entropy(NMFC05_Gft) #entropy

NMFC05_G.netMx <- cbind(NMFC05_G.netMx, NMFC05_G.clusterCoef, NMFC05_G.degreeCent$centralization,
                        NMFC05_G.netDensity, NMFC05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************

round = 5
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
NMFC05_Bg2 <- data.frame(NMFC05_B)
NMFC05_Bg2 <- NMFC05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_Bg2$player1
player2vector <- NMFC05_Bg2$player2
NMFC05_Bg3 <- NMFC05_Bg2
NMFC05_Bg3$p1inp2vec <- is.element(NMFC05_Bg3$player1, player2vector)
NMFC05_Bg3$p2inp1vec <- is.element(NMFC05_Bg3$player2, player1vector)

addPlayer1 <- NMFC05_Bg3[ which(NMFC05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_Bg3[ which(NMFC05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_Bg2 <- rbind(NMFC05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
NMFC05_Bft <- ftable(NMFC05_Bg2$player1, NMFC05_Bg2$player2)
NMFC05_Bft2 <- as.matrix(NMFC05_Bft)
numRows <- nrow(NMFC05_Bft2)
numCols <- ncol(NMFC05_Bft2)
NMFC05_Bft3 <- NMFC05_Bft2[c(2:numRows) , c(2:numCols)]
NMFC05_BTable <- graph.adjacency(NMFC05_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(NMFC05_BTable, vertex.label = V(NMFC05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
NMFC05_B.clusterCoef <- transitivity(NMFC05_BTable, type="global") #cluster coefficient
NMFC05_B.degreeCent <- centralization.degree(NMFC05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_Bftn <- as.network.matrix(NMFC05_Bft)
NMFC05_B.netDensity <- network.density(NMFC05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_B.entropy <- entropy(NMFC05_Bft) #entropy

NMFC05_B.netMx <- cbind(NMFC05_B.netMx, NMFC05_B.clusterCoef, NMFC05_B.degreeCent$centralization,
                        NMFC05_B.netDensity, NMFC05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
NMFC05_SFg2 <- data.frame(NMFC05_SF)
NMFC05_SFg2 <- NMFC05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_SFg2$player1
player2vector <- NMFC05_SFg2$player2
NMFC05_SFg3 <- NMFC05_SFg2
NMFC05_SFg3$p1inp2vec <- is.element(NMFC05_SFg3$player1, player2vector)
NMFC05_SFg3$p2inp1vec <- is.element(NMFC05_SFg3$player2, player1vector)

addPlayer1 <- NMFC05_SFg3[ which(NMFC05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_SFg3[ which(NMFC05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_SFg2 <- rbind(NMFC05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
NMFC05_SFft <- ftable(NMFC05_SFg2$player1, NMFC05_SFg2$player2)
NMFC05_SFft2 <- as.matrix(NMFC05_SFft)
numRows <- nrow(NMFC05_SFft2)
numCols <- ncol(NMFC05_SFft2)
NMFC05_SFft3 <- NMFC05_SFft2[c(2:numRows) , c(2:numCols)]
NMFC05_SFTable <- graph.adjacency(NMFC05_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(NMFC05_SFTable, vertex.label = V(NMFC05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
NMFC05_SF.clusterCoef <- transitivity(NMFC05_SFTable, type="global") #cluster coefficient
NMFC05_SF.degreeCent <- centralization.degree(NMFC05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_SFftn <- as.network.matrix(NMFC05_SFft)
NMFC05_SF.netDensity <- network.density(NMFC05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_SF.entropy <- entropy(NMFC05_SFft) #entropy

NMFC05_SF.netMx <- cbind(NMFC05_SF.netMx, NMFC05_SF.clusterCoef, NMFC05_SF.degreeCent$centralization,
                         NMFC05_SF.netDensity, NMFC05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************

round = 5
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
NMFC05_TFg2 <- data.frame(NMFC05_TF)
NMFC05_TFg2 <- NMFC05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_TFg2$player1
player2vector <- NMFC05_TFg2$player2
NMFC05_TFg3 <- NMFC05_TFg2
NMFC05_TFg3$p1inp2vec <- is.element(NMFC05_TFg3$player1, player2vector)
NMFC05_TFg3$p2inp1vec <- is.element(NMFC05_TFg3$player2, player1vector)

addPlayer1 <- NMFC05_TFg3[ which(NMFC05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_TFg3[ which(NMFC05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_TFg2 <- rbind(NMFC05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
NMFC05_TFft <- ftable(NMFC05_TFg2$player1, NMFC05_TFg2$player2)
NMFC05_TFft2 <- as.matrix(NMFC05_TFft)
numRows <- nrow(NMFC05_TFft2)
numCols <- ncol(NMFC05_TFft2)
NMFC05_TFft3 <- NMFC05_TFft2[c(2:numRows) , c(2:numCols)]
NMFC05_TFTable <- graph.adjacency(NMFC05_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(NMFC05_TFTable, vertex.label = V(NMFC05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
NMFC05_TF.clusterCoef <- transitivity(NMFC05_TFTable, type="global") #cluster coefficient
NMFC05_TF.degreeCent <- centralization.degree(NMFC05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_TFftn <- as.network.matrix(NMFC05_TFft)
NMFC05_TF.netDensity <- network.density(NMFC05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_TF.entropy <- entropy(NMFC05_TFft) #entropy

NMFC05_TF.netMx <- cbind(NMFC05_TF.netMx, NMFC05_TF.clusterCoef, NMFC05_TF.degreeCent$centralization,
                         NMFC05_TF.netDensity, NMFC05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************
#NA

round = 5
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
NMFC05_SAMg2 <- data.frame(NMFC05_SAM)
NMFC05_SAMg2 <- NMFC05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_SAMg2$player1
player2vector <- NMFC05_SAMg2$player2
NMFC05_SAMg3 <- NMFC05_SAMg2
NMFC05_SAMg3$p1inp2vec <- is.element(NMFC05_SAMg3$player1, player2vector)
NMFC05_SAMg3$p2inp1vec <- is.element(NMFC05_SAMg3$player2, player1vector)

addPlayer1 <- NMFC05_SAMg3[ which(NMFC05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_SAMg3[ which(NMFC05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_SAMg2 <- rbind(NMFC05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
NMFC05_SAMft <- ftable(NMFC05_SAMg2$player1, NMFC05_SAMg2$player2)
NMFC05_SAMft2 <- as.matrix(NMFC05_SAMft)
numRows <- nrow(NMFC05_SAMft2)
numCols <- ncol(NMFC05_SAMft2)
NMFC05_SAMft3 <- NMFC05_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC05_SAMTable <- graph.adjacency(NMFC05_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(NMFC05_SAMTable, vertex.label = V(NMFC05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
NMFC05_SAM.clusterCoef <- transitivity(NMFC05_SAMTable, type="global") #cluster coefficient
NMFC05_SAM.degreeCent <- centralization.degree(NMFC05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_SAMftn <- as.network.matrix(NMFC05_SAMft)
NMFC05_SAM.netDensity <- network.density(NMFC05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_SAM.entropy <- entropy(NMFC05_SAMft) #entropy

NMFC05_SAM.netMx <- cbind(NMFC05_SAM.netMx, NMFC05_SAM.clusterCoef, NMFC05_SAM.degreeCent$centralization,
                          NMFC05_SAM.netDensity, NMFC05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
NMFC05_TAMg2 <- data.frame(NMFC05_TAM)
NMFC05_TAMg2 <- NMFC05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_TAMg2$player1
player2vector <- NMFC05_TAMg2$player2
NMFC05_TAMg3 <- NMFC05_TAMg2
NMFC05_TAMg3$p1inp2vec <- is.element(NMFC05_TAMg3$player1, player2vector)
NMFC05_TAMg3$p2inp1vec <- is.element(NMFC05_TAMg3$player2, player1vector)

addPlayer1 <- NMFC05_TAMg3[ which(NMFC05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_TAMg3[ which(NMFC05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_TAMg2 <- rbind(NMFC05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
NMFC05_TAMft <- ftable(NMFC05_TAMg2$player1, NMFC05_TAMg2$player2)
NMFC05_TAMft2 <- as.matrix(NMFC05_TAMft)
numRows <- nrow(NMFC05_TAMft2)
numCols <- ncol(NMFC05_TAMft2)
NMFC05_TAMft3 <- NMFC05_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC05_TAMTable <- graph.adjacency(NMFC05_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(NMFC05_TAMTable, vertex.label = V(NMFC05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
NMFC05_TAM.clusterCoef <- transitivity(NMFC05_TAMTable, type="global") #cluster coefficient
NMFC05_TAM.degreeCent <- centralization.degree(NMFC05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_TAMftn <- as.network.matrix(NMFC05_TAMft)
NMFC05_TAM.netDensity <- network.density(NMFC05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_TAM.entropy <- entropy(NMFC05_TAMft) #entropy

NMFC05_TAM.netMx <- cbind(NMFC05_TAM.netMx, NMFC05_TAM.clusterCoef, NMFC05_TAM.degreeCent$centralization,
                          NMFC05_TAM.netDensity, NMFC05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
NMFC05_SDMg2 <- data.frame(NMFC05_SDM)
NMFC05_SDMg2 <- NMFC05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_SDMg2$player1
player2vector <- NMFC05_SDMg2$player2
NMFC05_SDMg3 <- NMFC05_SDMg2
NMFC05_SDMg3$p1inp2vec <- is.element(NMFC05_SDMg3$player1, player2vector)
NMFC05_SDMg3$p2inp1vec <- is.element(NMFC05_SDMg3$player2, player1vector)

addPlayer1 <- NMFC05_SDMg3[ which(NMFC05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_SDMg3[ which(NMFC05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_SDMg2 <- rbind(NMFC05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
NMFC05_SDMft <- ftable(NMFC05_SDMg2$player1, NMFC05_SDMg2$player2)
NMFC05_SDMft2 <- as.matrix(NMFC05_SDMft)
numRows <- nrow(NMFC05_SDMft2)
numCols <- ncol(NMFC05_SDMft2)
NMFC05_SDMft3 <- NMFC05_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC05_SDMTable <- graph.adjacency(NMFC05_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(NMFC05_SDMTable, vertex.label = V(NMFC05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
NMFC05_SDM.clusterCoef <- transitivity(NMFC05_SDMTable, type="global") #cluster coefficient
NMFC05_SDM.degreeCent <- centralization.degree(NMFC05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_SDMftn <- as.network.matrix(NMFC05_SDMft)
NMFC05_SDM.netDensity <- network.density(NMFC05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_SDM.entropy <- entropy(NMFC05_SDMft) #entropy

NMFC05_SDM.netMx <- cbind(NMFC05_SDM.netMx, NMFC05_SDM.clusterCoef, NMFC05_SDM.degreeCent$centralization,
                          NMFC05_SDM.netDensity, NMFC05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
NMFC05_TDMg2 <- data.frame(NMFC05_TDM)
NMFC05_TDMg2 <- NMFC05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_TDMg2$player1
player2vector <- NMFC05_TDMg2$player2
NMFC05_TDMg3 <- NMFC05_TDMg2
NMFC05_TDMg3$p1inp2vec <- is.element(NMFC05_TDMg3$player1, player2vector)
NMFC05_TDMg3$p2inp1vec <- is.element(NMFC05_TDMg3$player2, player1vector)

addPlayer1 <- NMFC05_TDMg3[ which(NMFC05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_TDMg3[ which(NMFC05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_TDMg2 <- rbind(NMFC05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
NMFC05_TDMft <- ftable(NMFC05_TDMg2$player1, NMFC05_TDMg2$player2)
NMFC05_TDMft2 <- as.matrix(NMFC05_TDMft)
numRows <- nrow(NMFC05_TDMft2)
numCols <- ncol(NMFC05_TDMft2)
NMFC05_TDMft3 <- NMFC05_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC05_TDMTable <- graph.adjacency(NMFC05_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(NMFC05_TDMTable, vertex.label = V(NMFC05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
NMFC05_TDM.clusterCoef <- transitivity(NMFC05_TDMTable, type="global") #cluster coefficient
NMFC05_TDM.degreeCent <- centralization.degree(NMFC05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_TDMftn <- as.network.matrix(NMFC05_TDMft)
NMFC05_TDM.netDensity <- network.density(NMFC05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_TDM.entropy <- entropy(NMFC05_TDMft) #entropy

NMFC05_TDM.netMx <- cbind(NMFC05_TDM.netMx, NMFC05_TDM.clusterCoef, NMFC05_TDM.degreeCent$centralization,
                          NMFC05_TDM.netDensity, NMFC05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************

round = 5
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
NMFC05_SDg2 <- data.frame(NMFC05_SD)
NMFC05_SDg2 <- NMFC05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_SDg2$player1
player2vector <- NMFC05_SDg2$player2
NMFC05_SDg3 <- NMFC05_SDg2
NMFC05_SDg3$p1inp2vec <- is.element(NMFC05_SDg3$player1, player2vector)
NMFC05_SDg3$p2inp1vec <- is.element(NMFC05_SDg3$player2, player1vector)

addPlayer1 <- NMFC05_SDg3[ which(NMFC05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_SDg3[ which(NMFC05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_SDg2 <- rbind(NMFC05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
NMFC05_SDft <- ftable(NMFC05_SDg2$player1, NMFC05_SDg2$player2)
NMFC05_SDft2 <- as.matrix(NMFC05_SDft)
numRows <- nrow(NMFC05_SDft2)
numCols <- ncol(NMFC05_SDft2)
NMFC05_SDft3 <- NMFC05_SDft2[c(2:numRows) , c(2:numCols)]
NMFC05_SDTable <- graph.adjacency(NMFC05_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(NMFC05_SDTable, vertex.label = V(NMFC05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
NMFC05_SD.clusterCoef <- transitivity(NMFC05_SDTable, type="global") #cluster coefficient
NMFC05_SD.degreeCent <- centralization.degree(NMFC05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_SDftn <- as.network.matrix(NMFC05_SDft)
NMFC05_SD.netDensity <- network.density(NMFC05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_SD.entropy <- entropy(NMFC05_SDft) #entropy

NMFC05_SD.netMx <- cbind(NMFC05_SD.netMx, NMFC05_SD.clusterCoef, NMFC05_SD.degreeCent$centralization,
                         NMFC05_SD.netDensity, NMFC05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
NMFC05_TDg2 <- data.frame(NMFC05_TD)
NMFC05_TDg2 <- NMFC05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_TDg2$player1
player2vector <- NMFC05_TDg2$player2
NMFC05_TDg3 <- NMFC05_TDg2
NMFC05_TDg3$p1inp2vec <- is.element(NMFC05_TDg3$player1, player2vector)
NMFC05_TDg3$p2inp1vec <- is.element(NMFC05_TDg3$player2, player1vector)

addPlayer1 <- NMFC05_TDg3[ which(NMFC05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_TDg3[ which(NMFC05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_TDg2 <- rbind(NMFC05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
NMFC05_TDft <- ftable(NMFC05_TDg2$player1, NMFC05_TDg2$player2)
NMFC05_TDft2 <- as.matrix(NMFC05_TDft)
numRows <- nrow(NMFC05_TDft2)
numCols <- ncol(NMFC05_TDft2)
NMFC05_TDft3 <- NMFC05_TDft2[c(2:numRows) , c(2:numCols)]
NMFC05_TDTable <- graph.adjacency(NMFC05_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(NMFC05_TDTable, vertex.label = V(NMFC05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
NMFC05_TD.clusterCoef <- transitivity(NMFC05_TDTable, type="global") #cluster coefficient
NMFC05_TD.degreeCent <- centralization.degree(NMFC05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_TDftn <- as.network.matrix(NMFC05_TDft)
NMFC05_TD.netDensity <- network.density(NMFC05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_TD.entropy <- entropy(NMFC05_TDft) #entropy

NMFC05_TD.netMx <- cbind(NMFC05_TD.netMx, NMFC05_TD.clusterCoef, NMFC05_TD.degreeCent$centralization,
                         NMFC05_TD.netDensity, NMFC05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
NMFC05_QTg2 <- data.frame(NMFC05_QT)
NMFC05_QTg2 <- NMFC05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC05_QTg2$player1
player2vector <- NMFC05_QTg2$player2
NMFC05_QTg3 <- NMFC05_QTg2
NMFC05_QTg3$p1inp2vec <- is.element(NMFC05_QTg3$player1, player2vector)
NMFC05_QTg3$p2inp1vec <- is.element(NMFC05_QTg3$player2, player1vector)

addPlayer1 <- NMFC05_QTg3[ which(NMFC05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC05_QTg3[ which(NMFC05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC05_QTg2 <- rbind(NMFC05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
NMFC05_QTft <- ftable(NMFC05_QTg2$player1, NMFC05_QTg2$player2)
NMFC05_QTft2 <- as.matrix(NMFC05_QTft)
numRows <- nrow(NMFC05_QTft2)
numCols <- ncol(NMFC05_QTft2)
NMFC05_QTft3 <- NMFC05_QTft2[c(2:numRows) , c(2:numCols)]
NMFC05_QTTable <- graph.adjacency(NMFC05_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(NMFC05_QTTable, vertex.label = V(NMFC05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
NMFC05_QT.clusterCoef <- transitivity(NMFC05_QTTable, type="global") #cluster coefficient
NMFC05_QT.degreeCent <- centralization.degree(NMFC05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC05_QTftn <- as.network.matrix(NMFC05_QTft)
NMFC05_QT.netDensity <- network.density(NMFC05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC05_QT.entropy <- entropy(NMFC05_QTft) #entropy

NMFC05_QT.netMx <- cbind(NMFC05_QT.netMx, NMFC05_QT.clusterCoef, NMFC05_QT.degreeCent$centralization,
                         NMFC05_QT.netDensity, NMFC05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC05_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************
#NA

round = 5
teamName = "PORT"
KIoutcome = "Goal_F"
PORT05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
PORT05_Gg2 <- data.frame(PORT05_G)
PORT05_Gg2 <- PORT05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_Gg2$player1
player2vector <- PORT05_Gg2$player2
PORT05_Gg3 <- PORT05_Gg2
PORT05_Gg3$p1inp2vec <- is.element(PORT05_Gg3$player1, player2vector)
PORT05_Gg3$p2inp1vec <- is.element(PORT05_Gg3$player2, player1vector)

addPlayer1 <- PORT05_Gg3[ which(PORT05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT05_Gg2 <- rbind(PORT05_Gg2, addPlayer1)

#ROUND 5, Goal graph using weighted edges
PORT05_Gft <- ftable(PORT05_Gg2$player1, PORT05_Gg2$player2)
PORT05_Gft2 <- as.matrix(PORT05_Gft)
numRows <- nrow(PORT05_Gft2)
numCols <- ncol(PORT05_Gft2)
PORT05_Gft3 <- PORT05_Gft2[c(2:numRows) , c(1:numCols)]
PORT05_GTable <- graph.adjacency(PORT05_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(PORT05_GTable, vertex.label = V(PORT05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
PORT05_G.clusterCoef <- transitivity(PORT05_GTable, type="global") #cluster coefficient
PORT05_G.degreeCent <- centralization.degree(PORT05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_Gftn <- as.network.matrix(PORT05_Gft)
PORT05_G.netDensity <- network.density(PORT05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_G.entropy <- entropy(PORT05_Gft) #entropy

PORT05_G.netMx <- cbind(PORT05_G.netMx, PORT05_G.clusterCoef, PORT05_G.degreeCent$centralization,
                        PORT05_G.netDensity, PORT05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************
#NA

round = 5
teamName = "PORT"
KIoutcome = "Behind_F"
PORT05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
PORT05_Bg2 <- data.frame(PORT05_B)
PORT05_Bg2 <- PORT05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_Bg2$player1
player2vector <- PORT05_Bg2$player2
PORT05_Bg3 <- PORT05_Bg2
PORT05_Bg3$p1inp2vec <- is.element(PORT05_Bg3$player1, player2vector)
PORT05_Bg3$p2inp1vec <- is.element(PORT05_Bg3$player2, player1vector)

addPlayer1 <- PORT05_Bg3[ which(PORT05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_Bg3[ which(PORT05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_Bg2 <- rbind(PORT05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
PORT05_Bft <- ftable(PORT05_Bg2$player1, PORT05_Bg2$player2)
PORT05_Bft2 <- as.matrix(PORT05_Bft)
numRows <- nrow(PORT05_Bft2)
numCols <- ncol(PORT05_Bft2)
PORT05_Bft3 <- PORT05_Bft2[c(2:numRows) , c(2:numCols)]
PORT05_BTable <- graph.adjacency(PORT05_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(PORT05_BTable, vertex.label = V(PORT05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
PORT05_B.clusterCoef <- transitivity(PORT05_BTable, type="global") #cluster coefficient
PORT05_B.degreeCent <- centralization.degree(PORT05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_Bftn <- as.network.matrix(PORT05_Bft)
PORT05_B.netDensity <- network.density(PORT05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_B.entropy <- entropy(PORT05_Bft) #entropy

PORT05_B.netMx <- cbind(PORT05_B.netMx, PORT05_B.clusterCoef, PORT05_B.degreeCent$centralization,
                        PORT05_B.netDensity, PORT05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
PORT05_SFg2 <- data.frame(PORT05_SF)
PORT05_SFg2 <- PORT05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_SFg2$player1
player2vector <- PORT05_SFg2$player2
PORT05_SFg3 <- PORT05_SFg2
PORT05_SFg3$p1inp2vec <- is.element(PORT05_SFg3$player1, player2vector)
PORT05_SFg3$p2inp1vec <- is.element(PORT05_SFg3$player2, player1vector)

addPlayer1 <- PORT05_SFg3[ which(PORT05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_SFg3[ which(PORT05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_SFg2 <- rbind(PORT05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
PORT05_SFft <- ftable(PORT05_SFg2$player1, PORT05_SFg2$player2)
PORT05_SFft2 <- as.matrix(PORT05_SFft)
numRows <- nrow(PORT05_SFft2)
numCols <- ncol(PORT05_SFft2)
PORT05_SFft3 <- PORT05_SFft2[c(2:numRows) , c(2:numCols)]
PORT05_SFTable <- graph.adjacency(PORT05_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(PORT05_SFTable, vertex.label = V(PORT05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
PORT05_SF.clusterCoef <- transitivity(PORT05_SFTable, type="global") #cluster coefficient
PORT05_SF.degreeCent <- centralization.degree(PORT05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_SFftn <- as.network.matrix(PORT05_SFft)
PORT05_SF.netDensity <- network.density(PORT05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_SF.entropy <- entropy(PORT05_SFft) #entropy

PORT05_SF.netMx <- cbind(PORT05_SF.netMx, PORT05_SF.clusterCoef, PORT05_SF.degreeCent$centralization,
                         PORT05_SF.netDensity, PORT05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************

round = 5
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
PORT05_TFg2 <- data.frame(PORT05_TF)
PORT05_TFg2 <- PORT05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_TFg2$player1
player2vector <- PORT05_TFg2$player2
PORT05_TFg3 <- PORT05_TFg2
PORT05_TFg3$p1inp2vec <- is.element(PORT05_TFg3$player1, player2vector)
PORT05_TFg3$p2inp1vec <- is.element(PORT05_TFg3$player2, player1vector)

addPlayer1 <- PORT05_TFg3[ which(PORT05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_TFg3[ which(PORT05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_TFg2 <- rbind(PORT05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
PORT05_TFft <- ftable(PORT05_TFg2$player1, PORT05_TFg2$player2)
PORT05_TFft2 <- as.matrix(PORT05_TFft)
numRows <- nrow(PORT05_TFft2)
numCols <- ncol(PORT05_TFft2)
PORT05_TFft3 <- PORT05_TFft2[c(2:numRows) , c(2:numCols)]
PORT05_TFTable <- graph.adjacency(PORT05_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(PORT05_TFTable, vertex.label = V(PORT05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
PORT05_TF.clusterCoef <- transitivity(PORT05_TFTable, type="global") #cluster coefficient
PORT05_TF.degreeCent <- centralization.degree(PORT05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_TFftn <- as.network.matrix(PORT05_TFft)
PORT05_TF.netDensity <- network.density(PORT05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_TF.entropy <- entropy(PORT05_TFft) #entropy

PORT05_TF.netMx <- cbind(PORT05_TF.netMx, PORT05_TF.clusterCoef, PORT05_TF.degreeCent$centralization,
                         PORT05_TF.netDensity, PORT05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************
#NA

round = 5
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
PORT05_SAMg2 <- data.frame(PORT05_SAM)
PORT05_SAMg2 <- PORT05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_SAMg2$player1
player2vector <- PORT05_SAMg2$player2
PORT05_SAMg3 <- PORT05_SAMg2
PORT05_SAMg3$p1inp2vec <- is.element(PORT05_SAMg3$player1, player2vector)
PORT05_SAMg3$p2inp1vec <- is.element(PORT05_SAMg3$player2, player1vector)

addPlayer1 <- PORT05_SAMg3[ which(PORT05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_SAMg3[ which(PORT05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_SAMg2 <- rbind(PORT05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
PORT05_SAMft <- ftable(PORT05_SAMg2$player1, PORT05_SAMg2$player2)
PORT05_SAMft2 <- as.matrix(PORT05_SAMft)
numRows <- nrow(PORT05_SAMft2)
numCols <- ncol(PORT05_SAMft2)
PORT05_SAMft3 <- PORT05_SAMft2[c(2:numRows) , c(2:numCols)]
PORT05_SAMTable <- graph.adjacency(PORT05_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(PORT05_SAMTable, vertex.label = V(PORT05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
PORT05_SAM.clusterCoef <- transitivity(PORT05_SAMTable, type="global") #cluster coefficient
PORT05_SAM.degreeCent <- centralization.degree(PORT05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_SAMftn <- as.network.matrix(PORT05_SAMft)
PORT05_SAM.netDensity <- network.density(PORT05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_SAM.entropy <- entropy(PORT05_SAMft) #entropy

PORT05_SAM.netMx <- cbind(PORT05_SAM.netMx, PORT05_SAM.clusterCoef, PORT05_SAM.degreeCent$centralization,
                          PORT05_SAM.netDensity, PORT05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
PORT05_TAMg2 <- data.frame(PORT05_TAM)
PORT05_TAMg2 <- PORT05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_TAMg2$player1
player2vector <- PORT05_TAMg2$player2
PORT05_TAMg3 <- PORT05_TAMg2
PORT05_TAMg3$p1inp2vec <- is.element(PORT05_TAMg3$player1, player2vector)
PORT05_TAMg3$p2inp1vec <- is.element(PORT05_TAMg3$player2, player1vector)

addPlayer1 <- PORT05_TAMg3[ which(PORT05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_TAMg3[ which(PORT05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_TAMg2 <- rbind(PORT05_TAMg2, addPlayers)


#ROUND 5, AM Turnover graph using weighted edges
PORT05_TAMft <- ftable(PORT05_TAMg2$player1, PORT05_TAMg2$player2)
PORT05_TAMft2 <- as.matrix(PORT05_TAMft)
numRows <- nrow(PORT05_TAMft2)
numCols <- ncol(PORT05_TAMft2)
PORT05_TAMft3 <- PORT05_TAMft2[c(2:numRows) , c(2:numCols)]
PORT05_TAMTable <- graph.adjacency(PORT05_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(PORT05_TAMTable, vertex.label = V(PORT05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
PORT05_TAM.clusterCoef <- transitivity(PORT05_TAMTable, type="global") #cluster coefficient
PORT05_TAM.degreeCent <- centralization.degree(PORT05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_TAMftn <- as.network.matrix(PORT05_TAMft)
PORT05_TAM.netDensity <- network.density(PORT05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_TAM.entropy <- entropy(PORT05_TAMft) #entropy

PORT05_TAM.netMx <- cbind(PORT05_TAM.netMx, PORT05_TAM.clusterCoef, PORT05_TAM.degreeCent$centralization,
                          PORT05_TAM.netDensity, PORT05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
PORT05_SDMg2 <- data.frame(PORT05_SDM)
PORT05_SDMg2 <- PORT05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_SDMg2$player1
player2vector <- PORT05_SDMg2$player2
PORT05_SDMg3 <- PORT05_SDMg2
PORT05_SDMg3$p1inp2vec <- is.element(PORT05_SDMg3$player1, player2vector)
PORT05_SDMg3$p2inp1vec <- is.element(PORT05_SDMg3$player2, player1vector)

addPlayer1 <- PORT05_SDMg3[ which(PORT05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_SDMg3[ which(PORT05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_SDMg2 <- rbind(PORT05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
PORT05_SDMft <- ftable(PORT05_SDMg2$player1, PORT05_SDMg2$player2)
PORT05_SDMft2 <- as.matrix(PORT05_SDMft)
numRows <- nrow(PORT05_SDMft2)
numCols <- ncol(PORT05_SDMft2)
PORT05_SDMft3 <- PORT05_SDMft2[c(2:numRows) , c(2:numCols)]
PORT05_SDMTable <- graph.adjacency(PORT05_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(PORT05_SDMTable, vertex.label = V(PORT05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
PORT05_SDM.clusterCoef <- transitivity(PORT05_SDMTable, type="global") #cluster coefficient
PORT05_SDM.degreeCent <- centralization.degree(PORT05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_SDMftn <- as.network.matrix(PORT05_SDMft)
PORT05_SDM.netDensity <- network.density(PORT05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_SDM.entropy <- entropy(PORT05_SDMft) #entropy

PORT05_SDM.netMx <- cbind(PORT05_SDM.netMx, PORT05_SDM.clusterCoef, PORT05_SDM.degreeCent$centralization,
                          PORT05_SDM.netDensity, PORT05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************
#NA

round = 5
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
PORT05_TDMg2 <- data.frame(PORT05_TDM)
PORT05_TDMg2 <- PORT05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_TDMg2$player1
player2vector <- PORT05_TDMg2$player2
PORT05_TDMg3 <- PORT05_TDMg2
PORT05_TDMg3$p1inp2vec <- is.element(PORT05_TDMg3$player1, player2vector)
PORT05_TDMg3$p2inp1vec <- is.element(PORT05_TDMg3$player2, player1vector)

addPlayer1 <- PORT05_TDMg3[ which(PORT05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_TDMg3[ which(PORT05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_TDMg2 <- rbind(PORT05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
PORT05_TDMft <- ftable(PORT05_TDMg2$player1, PORT05_TDMg2$player2)
PORT05_TDMft2 <- as.matrix(PORT05_TDMft)
numRows <- nrow(PORT05_TDMft2)
numCols <- ncol(PORT05_TDMft2)
PORT05_TDMft3 <- PORT05_TDMft2[c(2:numRows) , c(2:numCols)]
PORT05_TDMTable <- graph.adjacency(PORT05_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(PORT05_TDMTable, vertex.label = V(PORT05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
PORT05_TDM.clusterCoef <- transitivity(PORT05_TDMTable, type="global") #cluster coefficient
PORT05_TDM.degreeCent <- centralization.degree(PORT05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_TDMftn <- as.network.matrix(PORT05_TDMft)
PORT05_TDM.netDensity <- network.density(PORT05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_TDM.entropy <- entropy(PORT05_TDMft) #entropy

PORT05_TDM.netMx <- cbind(PORT05_TDM.netMx, PORT05_TDM.clusterCoef, PORT05_TDM.degreeCent$centralization,
                          PORT05_TDM.netDensity, PORT05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
PORT05_SDg2 <- data.frame(PORT05_SD)
PORT05_SDg2 <- PORT05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_SDg2$player1
player2vector <- PORT05_SDg2$player2
PORT05_SDg3 <- PORT05_SDg2
PORT05_SDg3$p1inp2vec <- is.element(PORT05_SDg3$player1, player2vector)
PORT05_SDg3$p2inp1vec <- is.element(PORT05_SDg3$player2, player1vector)

addPlayer1 <- PORT05_SDg3[ which(PORT05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_SDg3[ which(PORT05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_SDg2 <- rbind(PORT05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
PORT05_SDft <- ftable(PORT05_SDg2$player1, PORT05_SDg2$player2)
PORT05_SDft2 <- as.matrix(PORT05_SDft)
numRows <- nrow(PORT05_SDft2)
numCols <- ncol(PORT05_SDft2)
PORT05_SDft3 <- PORT05_SDft2[c(2:numRows) , c(2:numCols)]
PORT05_SDTable <- graph.adjacency(PORT05_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(PORT05_SDTable, vertex.label = V(PORT05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
PORT05_SD.clusterCoef <- transitivity(PORT05_SDTable, type="global") #cluster coefficient
PORT05_SD.degreeCent <- centralization.degree(PORT05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_SDftn <- as.network.matrix(PORT05_SDft)
PORT05_SD.netDensity <- network.density(PORT05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_SD.entropy <- entropy(PORT05_SDft) #entropy

PORT05_SD.netMx <- cbind(PORT05_SD.netMx, PORT05_SD.clusterCoef, PORT05_SD.degreeCent$centralization,
                         PORT05_SD.netDensity, PORT05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
PORT05_TDg2 <- data.frame(PORT05_TD)
PORT05_TDg2 <- PORT05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_TDg2$player1
player2vector <- PORT05_TDg2$player2
PORT05_TDg3 <- PORT05_TDg2
PORT05_TDg3$p1inp2vec <- is.element(PORT05_TDg3$player1, player2vector)
PORT05_TDg3$p2inp1vec <- is.element(PORT05_TDg3$player2, player1vector)

addPlayer1 <- PORT05_TDg3[ which(PORT05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_TDg3[ which(PORT05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_TDg2 <- rbind(PORT05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
PORT05_TDft <- ftable(PORT05_TDg2$player1, PORT05_TDg2$player2)
PORT05_TDft2 <- as.matrix(PORT05_TDft)
numRows <- nrow(PORT05_TDft2)
numCols <- ncol(PORT05_TDft2)
PORT05_TDft3 <- PORT05_TDft2[c(2:numRows) , c(2:numCols)]
PORT05_TDTable <- graph.adjacency(PORT05_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(PORT05_TDTable, vertex.label = V(PORT05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
PORT05_TD.clusterCoef <- transitivity(PORT05_TDTable, type="global") #cluster coefficient
PORT05_TD.degreeCent <- centralization.degree(PORT05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_TDftn <- as.network.matrix(PORT05_TDft)
PORT05_TD.netDensity <- network.density(PORT05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_TD.entropy <- entropy(PORT05_TDft) #entropy

PORT05_TD.netMx <- cbind(PORT05_TD.netMx, PORT05_TD.clusterCoef, PORT05_TD.degreeCent$centralization,
                         PORT05_TD.netDensity, PORT05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
PORT05_QTg2 <- data.frame(PORT05_QT)
PORT05_QTg2 <- PORT05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT05_QTg2$player1
player2vector <- PORT05_QTg2$player2
PORT05_QTg3 <- PORT05_QTg2
PORT05_QTg3$p1inp2vec <- is.element(PORT05_QTg3$player1, player2vector)
PORT05_QTg3$p2inp1vec <- is.element(PORT05_QTg3$player2, player1vector)

addPlayer1 <- PORT05_QTg3[ which(PORT05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT05_QTg3[ which(PORT05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT05_QTg2 <- rbind(PORT05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
PORT05_QTft <- ftable(PORT05_QTg2$player1, PORT05_QTg2$player2)
PORT05_QTft2 <- as.matrix(PORT05_QTft)
numRows <- nrow(PORT05_QTft2)
numCols <- ncol(PORT05_QTft2)
PORT05_QTft3 <- PORT05_QTft2[c(2:numRows) , c(2:numCols)]
PORT05_QTTable <- graph.adjacency(PORT05_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(PORT05_QTTable, vertex.label = V(PORT05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
PORT05_QT.clusterCoef <- transitivity(PORT05_QTTable, type="global") #cluster coefficient
PORT05_QT.degreeCent <- centralization.degree(PORT05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT05_QTftn <- as.network.matrix(PORT05_QTft)
PORT05_QT.netDensity <- network.density(PORT05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT05_QT.entropy <- entropy(PORT05_QTft) #entropy

PORT05_QT.netMx <- cbind(PORT05_QT.netMx, PORT05_QT.clusterCoef, PORT05_QT.degreeCent$centralization,
                         PORT05_QT.netDensity, PORT05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT05_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "RICH"
KIoutcome = "Goal_F"
RICH05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
RICH05_Gg2 <- data.frame(RICH05_G)
RICH05_Gg2 <- RICH05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_Gg2$player1
player2vector <- RICH05_Gg2$player2
RICH05_Gg3 <- RICH05_Gg2
RICH05_Gg3$p1inp2vec <- is.element(RICH05_Gg3$player1, player2vector)
RICH05_Gg3$p2inp1vec <- is.element(RICH05_Gg3$player2, player1vector)

addPlayer1 <- RICH05_Gg3[ which(RICH05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_Gg3[ which(RICH05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_Gg2 <- rbind(RICH05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
RICH05_Gft <- ftable(RICH05_Gg2$player1, RICH05_Gg2$player2)
RICH05_Gft2 <- as.matrix(RICH05_Gft)
numRows <- nrow(RICH05_Gft2)
numCols <- ncol(RICH05_Gft2)
RICH05_Gft3 <- RICH05_Gft2[c(2:numRows) , c(2:numCols)]
RICH05_GTable <- graph.adjacency(RICH05_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(RICH05_GTable, vertex.label = V(RICH05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
RICH05_G.clusterCoef <- transitivity(RICH05_GTable, type="global") #cluster coefficient
RICH05_G.degreeCent <- centralization.degree(RICH05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_Gftn <- as.network.matrix(RICH05_Gft)
RICH05_G.netDensity <- network.density(RICH05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_G.entropy <- entropy(RICH05_Gft) #entropy

RICH05_G.netMx <- cbind(RICH05_G.netMx, RICH05_G.clusterCoef, RICH05_G.degreeCent$centralization,
                        RICH05_G.netDensity, RICH05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************

round = 5
teamName = "RICH"
KIoutcome = "Behind_F"
RICH05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
RICH05_Bg2 <- data.frame(RICH05_B)
RICH05_Bg2 <- RICH05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_Bg2$player1
player2vector <- RICH05_Bg2$player2
RICH05_Bg3 <- RICH05_Bg2
RICH05_Bg3$p1inp2vec <- is.element(RICH05_Bg3$player1, player2vector)
RICH05_Bg3$p2inp1vec <- is.element(RICH05_Bg3$player2, player1vector)

addPlayer1 <- RICH05_Bg3[ which(RICH05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_Bg3[ which(RICH05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_Bg2 <- rbind(RICH05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
RICH05_Bft <- ftable(RICH05_Bg2$player1, RICH05_Bg2$player2)
RICH05_Bft2 <- as.matrix(RICH05_Bft)
numRows <- nrow(RICH05_Bft2)
numCols <- ncol(RICH05_Bft2)
RICH05_Bft3 <- RICH05_Bft2[c(2:numRows) , c(2:numCols)]
RICH05_BTable <- graph.adjacency(RICH05_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(RICH05_BTable, vertex.label = V(RICH05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
RICH05_B.clusterCoef <- transitivity(RICH05_BTable, type="global") #cluster coefficient
RICH05_B.degreeCent <- centralization.degree(RICH05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_Bftn <- as.network.matrix(RICH05_Bft)
RICH05_B.netDensity <- network.density(RICH05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_B.entropy <- entropy(RICH05_Bft) #entropy

RICH05_B.netMx <- cbind(RICH05_B.netMx, RICH05_B.clusterCoef, RICH05_B.degreeCent$centralization,
                        RICH05_B.netDensity, RICH05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************

round = 5
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
RICH05_SFg2 <- data.frame(RICH05_SF)
RICH05_SFg2 <- RICH05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_SFg2$player1
player2vector <- RICH05_SFg2$player2
RICH05_SFg3 <- RICH05_SFg2
RICH05_SFg3$p1inp2vec <- is.element(RICH05_SFg3$player1, player2vector)
RICH05_SFg3$p2inp1vec <- is.element(RICH05_SFg3$player2, player1vector)

addPlayer1 <- RICH05_SFg3[ which(RICH05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH05_SFg3[ which(RICH05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_SFg2 <- rbind(RICH05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
RICH05_SFft <- ftable(RICH05_SFg2$player1, RICH05_SFg2$player2)
RICH05_SFft2 <- as.matrix(RICH05_SFft)
numRows <- nrow(RICH05_SFft2)
numCols <- ncol(RICH05_SFft2)
RICH05_SFft3 <- RICH05_SFft2[c(2:numRows) , c(2:numCols)]
RICH05_SFTable <- graph.adjacency(RICH05_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(RICH05_SFTable, vertex.label = V(RICH05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
RICH05_SF.clusterCoef <- transitivity(RICH05_SFTable, type="global") #cluster coefficient
RICH05_SF.degreeCent <- centralization.degree(RICH05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_SFftn <- as.network.matrix(RICH05_SFft)
RICH05_SF.netDensity <- network.density(RICH05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_SF.entropy <- entropy(RICH05_SFft) #entropy

RICH05_SF.netMx <- cbind(RICH05_SF.netMx, RICH05_SF.clusterCoef, RICH05_SF.degreeCent$centralization,
                         RICH05_SF.netDensity, RICH05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************

round = 5
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
RICH05_TFg2 <- data.frame(RICH05_TF)
RICH05_TFg2 <- RICH05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_TFg2$player1
player2vector <- RICH05_TFg2$player2
RICH05_TFg3 <- RICH05_TFg2
RICH05_TFg3$p1inp2vec <- is.element(RICH05_TFg3$player1, player2vector)
RICH05_TFg3$p2inp1vec <- is.element(RICH05_TFg3$player2, player1vector)

addPlayer1 <- RICH05_TFg3[ which(RICH05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_TFg3[ which(RICH05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_TFg2 <- rbind(RICH05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
RICH05_TFft <- ftable(RICH05_TFg2$player1, RICH05_TFg2$player2)
RICH05_TFft2 <- as.matrix(RICH05_TFft)
numRows <- nrow(RICH05_TFft2)
numCols <- ncol(RICH05_TFft2)
RICH05_TFft3 <- RICH05_TFft2[c(2:numRows) , c(2:numCols)]
RICH05_TFTable <- graph.adjacency(RICH05_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(RICH05_TFTable, vertex.label = V(RICH05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
RICH05_TF.clusterCoef <- transitivity(RICH05_TFTable, type="global") #cluster coefficient
RICH05_TF.degreeCent <- centralization.degree(RICH05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_TFftn <- as.network.matrix(RICH05_TFft)
RICH05_TF.netDensity <- network.density(RICH05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_TF.entropy <- entropy(RICH05_TFft) #entropy

RICH05_TF.netMx <- cbind(RICH05_TF.netMx, RICH05_TF.clusterCoef, RICH05_TF.degreeCent$centralization,
                         RICH05_TF.netDensity, RICH05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
RICH05_SAMg2 <- data.frame(RICH05_SAM)
RICH05_SAMg2 <- RICH05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_SAMg2$player1
player2vector <- RICH05_SAMg2$player2
RICH05_SAMg3 <- RICH05_SAMg2
RICH05_SAMg3$p1inp2vec <- is.element(RICH05_SAMg3$player1, player2vector)
RICH05_SAMg3$p2inp1vec <- is.element(RICH05_SAMg3$player2, player1vector)

addPlayer1 <- RICH05_SAMg3[ which(RICH05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_SAMg3[ which(RICH05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_SAMg2 <- rbind(RICH05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
RICH05_SAMft <- ftable(RICH05_SAMg2$player1, RICH05_SAMg2$player2)
RICH05_SAMft2 <- as.matrix(RICH05_SAMft)
numRows <- nrow(RICH05_SAMft2)
numCols <- ncol(RICH05_SAMft2)
RICH05_SAMft3 <- RICH05_SAMft2[c(2:numRows) , c(2:numCols)]
RICH05_SAMTable <- graph.adjacency(RICH05_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(RICH05_SAMTable, vertex.label = V(RICH05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
RICH05_SAM.clusterCoef <- transitivity(RICH05_SAMTable, type="global") #cluster coefficient
RICH05_SAM.degreeCent <- centralization.degree(RICH05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_SAMftn <- as.network.matrix(RICH05_SAMft)
RICH05_SAM.netDensity <- network.density(RICH05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_SAM.entropy <- entropy(RICH05_SAMft) #entropy

RICH05_SAM.netMx <- cbind(RICH05_SAM.netMx, RICH05_SAM.clusterCoef, RICH05_SAM.degreeCent$centralization,
                          RICH05_SAM.netDensity, RICH05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
RICH05_TAMg2 <- data.frame(RICH05_TAM)
RICH05_TAMg2 <- RICH05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_TAMg2$player1
player2vector <- RICH05_TAMg2$player2
RICH05_TAMg3 <- RICH05_TAMg2
RICH05_TAMg3$p1inp2vec <- is.element(RICH05_TAMg3$player1, player2vector)
RICH05_TAMg3$p2inp1vec <- is.element(RICH05_TAMg3$player2, player1vector)

addPlayer1 <- RICH05_TAMg3[ which(RICH05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_TAMg3[ which(RICH05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_TAMg2 <- rbind(RICH05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
RICH05_TAMft <- ftable(RICH05_TAMg2$player1, RICH05_TAMg2$player2)
RICH05_TAMft2 <- as.matrix(RICH05_TAMft)
numRows <- nrow(RICH05_TAMft2)
numCols <- ncol(RICH05_TAMft2)
RICH05_TAMft3 <- RICH05_TAMft2[c(2:numRows) , c(2:numCols)]
RICH05_TAMTable <- graph.adjacency(RICH05_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(RICH05_TAMTable, vertex.label = V(RICH05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
RICH05_TAM.clusterCoef <- transitivity(RICH05_TAMTable, type="global") #cluster coefficient
RICH05_TAM.degreeCent <- centralization.degree(RICH05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_TAMftn <- as.network.matrix(RICH05_TAMft)
RICH05_TAM.netDensity <- network.density(RICH05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_TAM.entropy <- entropy(RICH05_TAMft) #entropy

RICH05_TAM.netMx <- cbind(RICH05_TAM.netMx, RICH05_TAM.clusterCoef, RICH05_TAM.degreeCent$centralization,
                          RICH05_TAM.netDensity, RICH05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
RICH05_SDMg2 <- data.frame(RICH05_SDM)
RICH05_SDMg2 <- RICH05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_SDMg2$player1
player2vector <- RICH05_SDMg2$player2
RICH05_SDMg3 <- RICH05_SDMg2
RICH05_SDMg3$p1inp2vec <- is.element(RICH05_SDMg3$player1, player2vector)
RICH05_SDMg3$p2inp1vec <- is.element(RICH05_SDMg3$player2, player1vector)

addPlayer1 <- RICH05_SDMg3[ which(RICH05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_SDMg3[ which(RICH05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_SDMg2 <- rbind(RICH05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
RICH05_SDMft <- ftable(RICH05_SDMg2$player1, RICH05_SDMg2$player2)
RICH05_SDMft2 <- as.matrix(RICH05_SDMft)
numRows <- nrow(RICH05_SDMft2)
numCols <- ncol(RICH05_SDMft2)
RICH05_SDMft3 <- RICH05_SDMft2[c(2:numRows) , c(2:numCols)]
RICH05_SDMTable <- graph.adjacency(RICH05_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(RICH05_SDMTable, vertex.label = V(RICH05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
RICH05_SDM.clusterCoef <- transitivity(RICH05_SDMTable, type="global") #cluster coefficient
RICH05_SDM.degreeCent <- centralization.degree(RICH05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_SDMftn <- as.network.matrix(RICH05_SDMft)
RICH05_SDM.netDensity <- network.density(RICH05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_SDM.entropy <- entropy(RICH05_SDMft) #entropy

RICH05_SDM.netMx <- cbind(RICH05_SDM.netMx, RICH05_SDM.clusterCoef, RICH05_SDM.degreeCent$centralization,
                          RICH05_SDM.netDensity, RICH05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
RICH05_TDMg2 <- data.frame(RICH05_TDM)
RICH05_TDMg2 <- RICH05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_TDMg2$player1
player2vector <- RICH05_TDMg2$player2
RICH05_TDMg3 <- RICH05_TDMg2
RICH05_TDMg3$p1inp2vec <- is.element(RICH05_TDMg3$player1, player2vector)
RICH05_TDMg3$p2inp1vec <- is.element(RICH05_TDMg3$player2, player1vector)

addPlayer1 <- RICH05_TDMg3[ which(RICH05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_TDMg3[ which(RICH05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_TDMg2 <- rbind(RICH05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
RICH05_TDMft <- ftable(RICH05_TDMg2$player1, RICH05_TDMg2$player2)
RICH05_TDMft2 <- as.matrix(RICH05_TDMft)
numRows <- nrow(RICH05_TDMft2)
numCols <- ncol(RICH05_TDMft2)
RICH05_TDMft3 <- RICH05_TDMft2[c(2:numRows) , c(2:numCols)]
RICH05_TDMTable <- graph.adjacency(RICH05_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(RICH05_TDMTable, vertex.label = V(RICH05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
RICH05_TDM.clusterCoef <- transitivity(RICH05_TDMTable, type="global") #cluster coefficient
RICH05_TDM.degreeCent <- centralization.degree(RICH05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_TDMftn <- as.network.matrix(RICH05_TDMft)
RICH05_TDM.netDensity <- network.density(RICH05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_TDM.entropy <- entropy(RICH05_TDMft) #entropy

RICH05_TDM.netMx <- cbind(RICH05_TDM.netMx, RICH05_TDM.clusterCoef, RICH05_TDM.degreeCent$centralization,
                          RICH05_TDM.netDensity, RICH05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
RICH05_SDg2 <- data.frame(RICH05_SD)
RICH05_SDg2 <- RICH05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_SDg2$player1
player2vector <- RICH05_SDg2$player2
RICH05_SDg3 <- RICH05_SDg2
RICH05_SDg3$p1inp2vec <- is.element(RICH05_SDg3$player1, player2vector)
RICH05_SDg3$p2inp1vec <- is.element(RICH05_SDg3$player2, player1vector)

addPlayer1 <- RICH05_SDg3[ which(RICH05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_SDg3[ which(RICH05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_SDg2 <- rbind(RICH05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
RICH05_SDft <- ftable(RICH05_SDg2$player1, RICH05_SDg2$player2)
RICH05_SDft2 <- as.matrix(RICH05_SDft)
numRows <- nrow(RICH05_SDft2)
numCols <- ncol(RICH05_SDft2)
RICH05_SDft3 <- RICH05_SDft2[c(2:numRows) , c(2:numCols)]
RICH05_SDTable <- graph.adjacency(RICH05_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(RICH05_SDTable, vertex.label = V(RICH05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
RICH05_SD.clusterCoef <- transitivity(RICH05_SDTable, type="global") #cluster coefficient
RICH05_SD.degreeCent <- centralization.degree(RICH05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_SDftn <- as.network.matrix(RICH05_SDft)
RICH05_SD.netDensity <- network.density(RICH05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_SD.entropy <- entropy(RICH05_SDft) #entropy

RICH05_SD.netMx <- cbind(RICH05_SD.netMx, RICH05_SD.clusterCoef, RICH05_SD.degreeCent$centralization,
                         RICH05_SD.netDensity, RICH05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
RICH05_TDg2 <- data.frame(RICH05_TD)
RICH05_TDg2 <- RICH05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_TDg2$player1
player2vector <- RICH05_TDg2$player2
RICH05_TDg3 <- RICH05_TDg2
RICH05_TDg3$p1inp2vec <- is.element(RICH05_TDg3$player1, player2vector)
RICH05_TDg3$p2inp1vec <- is.element(RICH05_TDg3$player2, player1vector)

addPlayer1 <- RICH05_TDg3[ which(RICH05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_TDg3[ which(RICH05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_TDg2 <- rbind(RICH05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
RICH05_TDft <- ftable(RICH05_TDg2$player1, RICH05_TDg2$player2)
RICH05_TDft2 <- as.matrix(RICH05_TDft)
numRows <- nrow(RICH05_TDft2)
numCols <- ncol(RICH05_TDft2)
RICH05_TDft3 <- RICH05_TDft2[c(2:numRows) , c(2:numCols)]
RICH05_TDTable <- graph.adjacency(RICH05_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(RICH05_TDTable, vertex.label = V(RICH05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
RICH05_TD.clusterCoef <- transitivity(RICH05_TDTable, type="global") #cluster coefficient
RICH05_TD.degreeCent <- centralization.degree(RICH05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_TDftn <- as.network.matrix(RICH05_TDft)
RICH05_TD.netDensity <- network.density(RICH05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_TD.entropy <- entropy(RICH05_TDft) #entropy

RICH05_TD.netMx <- cbind(RICH05_TD.netMx, RICH05_TD.clusterCoef, RICH05_TD.degreeCent$centralization,
                         RICH05_TD.netDensity, RICH05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
RICH05_QTg2 <- data.frame(RICH05_QT)
RICH05_QTg2 <- RICH05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH05_QTg2$player1
player2vector <- RICH05_QTg2$player2
RICH05_QTg3 <- RICH05_QTg2
RICH05_QTg3$p1inp2vec <- is.element(RICH05_QTg3$player1, player2vector)
RICH05_QTg3$p2inp1vec <- is.element(RICH05_QTg3$player2, player1vector)

addPlayer1 <- RICH05_QTg3[ which(RICH05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH05_QTg3[ which(RICH05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH05_QTg2 <- rbind(RICH05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
RICH05_QTft <- ftable(RICH05_QTg2$player1, RICH05_QTg2$player2)
RICH05_QTft2 <- as.matrix(RICH05_QTft)
numRows <- nrow(RICH05_QTft2)
numCols <- ncol(RICH05_QTft2)
RICH05_QTft3 <- RICH05_QTft2[c(2:numRows) , c(2:numCols)]
RICH05_QTTable <- graph.adjacency(RICH05_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(RICH05_QTTable, vertex.label = V(RICH05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
RICH05_QT.clusterCoef <- transitivity(RICH05_QTTable, type="global") #cluster coefficient
RICH05_QT.degreeCent <- centralization.degree(RICH05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH05_QTftn <- as.network.matrix(RICH05_QTft)
RICH05_QT.netDensity <- network.density(RICH05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH05_QT.entropy <- entropy(RICH05_QTft) #entropy

RICH05_QT.netMx <- cbind(RICH05_QT.netMx, RICH05_QT.clusterCoef, RICH05_QT.degreeCent$centralization,
                         RICH05_QT.netDensity, RICH05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH05_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "STK"
KIoutcome = "Goal_F"
STK05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
STK05_Gg2 <- data.frame(STK05_G)
STK05_Gg2 <- STK05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_Gg2$player1
player2vector <- STK05_Gg2$player2
STK05_Gg3 <- STK05_Gg2
STK05_Gg3$p1inp2vec <- is.element(STK05_Gg3$player1, player2vector)
STK05_Gg3$p2inp1vec <- is.element(STK05_Gg3$player2, player1vector)

addPlayer1 <- STK05_Gg3[ which(STK05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_Gg3[ which(STK05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_Gg2 <- rbind(STK05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
STK05_Gft <- ftable(STK05_Gg2$player1, STK05_Gg2$player2)
STK05_Gft2 <- as.matrix(STK05_Gft)
numRows <- nrow(STK05_Gft2)
numCols <- ncol(STK05_Gft2)
STK05_Gft3 <- STK05_Gft2[c(2:numRows) , c(2:numCols)]
STK05_GTable <- graph.adjacency(STK05_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(STK05_GTable, vertex.label = V(STK05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
STK05_G.clusterCoef <- transitivity(STK05_GTable, type="global") #cluster coefficient
STK05_G.degreeCent <- centralization.degree(STK05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_Gftn <- as.network.matrix(STK05_Gft)
STK05_G.netDensity <- network.density(STK05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_G.entropy <- entropy(STK05_Gft) #entropy

STK05_G.netMx <- cbind(STK05_G.netMx, STK05_G.clusterCoef, STK05_G.degreeCent$centralization,
                       STK05_G.netDensity, STK05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************

round = 5
teamName = "STK"
KIoutcome = "Behind_F"
STK05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
STK05_Bg2 <- data.frame(STK05_B)
STK05_Bg2 <- STK05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_Bg2$player1
player2vector <- STK05_Bg2$player2
STK05_Bg3 <- STK05_Bg2
STK05_Bg3$p1inp2vec <- is.element(STK05_Bg3$player1, player2vector)
STK05_Bg3$p2inp1vec <- is.element(STK05_Bg3$player2, player1vector)

addPlayer1 <- STK05_Bg3[ which(STK05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_Bg3[ which(STK05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_Bg2 <- rbind(STK05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
STK05_Bft <- ftable(STK05_Bg2$player1, STK05_Bg2$player2)
STK05_Bft2 <- as.matrix(STK05_Bft)
numRows <- nrow(STK05_Bft2)
numCols <- ncol(STK05_Bft2)
STK05_Bft3 <- STK05_Bft2[c(2:numRows) , c(2:numCols)]
STK05_BTable <- graph.adjacency(STK05_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(STK05_BTable, vertex.label = V(STK05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
STK05_B.clusterCoef <- transitivity(STK05_BTable, type="global") #cluster coefficient
STK05_B.degreeCent <- centralization.degree(STK05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_Bftn <- as.network.matrix(STK05_Bft)
STK05_B.netDensity <- network.density(STK05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_B.entropy <- entropy(STK05_Bft) #entropy

STK05_B.netMx <- cbind(STK05_B.netMx, STK05_B.clusterCoef, STK05_B.degreeCent$centralization,
                       STK05_B.netDensity, STK05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "STK"
KIoutcome = "Stoppage_F"
STK05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
STK05_SFg2 <- data.frame(STK05_SF)
STK05_SFg2 <- STK05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_SFg2$player1
player2vector <- STK05_SFg2$player2
STK05_SFg3 <- STK05_SFg2
STK05_SFg3$p1inp2vec <- is.element(STK05_SFg3$player1, player2vector)
STK05_SFg3$p2inp1vec <- is.element(STK05_SFg3$player2, player1vector)

addPlayer1 <- STK05_SFg3[ which(STK05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_SFg3[ which(STK05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_SFg2 <- rbind(STK05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
STK05_SFft <- ftable(STK05_SFg2$player1, STK05_SFg2$player2)
STK05_SFft2 <- as.matrix(STK05_SFft)
numRows <- nrow(STK05_SFft2)
numCols <- ncol(STK05_SFft2)
STK05_SFft3 <- STK05_SFft2[c(2:numRows) , c(2:numCols)]
STK05_SFTable <- graph.adjacency(STK05_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(STK05_SFTable, vertex.label = V(STK05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
STK05_SF.clusterCoef <- transitivity(STK05_SFTable, type="global") #cluster coefficient
STK05_SF.degreeCent <- centralization.degree(STK05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_SFftn <- as.network.matrix(STK05_SFft)
STK05_SF.netDensity <- network.density(STK05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_SF.entropy <- entropy(STK05_SFft) #entropy

STK05_SF.netMx <- cbind(STK05_SF.netMx, STK05_SF.clusterCoef, STK05_SF.degreeCent$centralization,
                        STK05_SF.netDensity, STK05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************
#NA

round = 5
teamName = "STK"
KIoutcome = "Turnover_F"
STK05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
STK05_TFg2 <- data.frame(STK05_TF)
STK05_TFg2 <- STK05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_TFg2$player1
player2vector <- STK05_TFg2$player2
STK05_TFg3 <- STK05_TFg2
STK05_TFg3$p1inp2vec <- is.element(STK05_TFg3$player1, player2vector)
STK05_TFg3$p2inp1vec <- is.element(STK05_TFg3$player2, player1vector)

addPlayer1 <- STK05_TFg3[ which(STK05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_TFg3[ which(STK05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_TFg2 <- rbind(STK05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
STK05_TFft <- ftable(STK05_TFg2$player1, STK05_TFg2$player2)
STK05_TFft2 <- as.matrix(STK05_TFft)
numRows <- nrow(STK05_TFft2)
numCols <- ncol(STK05_TFft2)
STK05_TFft3 <- STK05_TFft2[c(2:numRows) , c(2:numCols)]
STK05_TFTable <- graph.adjacency(STK05_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(STK05_TFTable, vertex.label = V(STK05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
STK05_TF.clusterCoef <- transitivity(STK05_TFTable, type="global") #cluster coefficient
STK05_TF.degreeCent <- centralization.degree(STK05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_TFftn <- as.network.matrix(STK05_TFft)
STK05_TF.netDensity <- network.density(STK05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_TF.entropy <- entropy(STK05_TFft) #entropy

STK05_TF.netMx <- cbind(STK05_TF.netMx, STK05_TF.clusterCoef, STK05_TF.degreeCent$centralization,
                        STK05_TF.netDensity, STK05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************

round = 5
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
STK05_SAMg2 <- data.frame(STK05_SAM)
STK05_SAMg2 <- STK05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_SAMg2$player1
player2vector <- STK05_SAMg2$player2
STK05_SAMg3 <- STK05_SAMg2
STK05_SAMg3$p1inp2vec <- is.element(STK05_SAMg3$player1, player2vector)
STK05_SAMg3$p2inp1vec <- is.element(STK05_SAMg3$player2, player1vector)

addPlayer1 <- STK05_SAMg3[ which(STK05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_SAMg3[ which(STK05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_SAMg2 <- rbind(STK05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
STK05_SAMft <- ftable(STK05_SAMg2$player1, STK05_SAMg2$player2)
STK05_SAMft2 <- as.matrix(STK05_SAMft)
numRows <- nrow(STK05_SAMft2)
numCols <- ncol(STK05_SAMft2)
STK05_SAMft3 <- STK05_SAMft2[c(2:numRows) , c(2:numCols)]
STK05_SAMTable <- graph.adjacency(STK05_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(STK05_SAMTable, vertex.label = V(STK05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
STK05_SAM.clusterCoef <- transitivity(STK05_SAMTable, type="global") #cluster coefficient
STK05_SAM.degreeCent <- centralization.degree(STK05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_SAMftn <- as.network.matrix(STK05_SAMft)
STK05_SAM.netDensity <- network.density(STK05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_SAM.entropy <- entropy(STK05_SAMft) #entropy

STK05_SAM.netMx <- cbind(STK05_SAM.netMx, STK05_SAM.clusterCoef, STK05_SAM.degreeCent$centralization,
                         STK05_SAM.netDensity, STK05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "STK"
KIoutcome = "Turnover_AM"
STK05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
STK05_TAMg2 <- data.frame(STK05_TAM)
STK05_TAMg2 <- STK05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_TAMg2$player1
player2vector <- STK05_TAMg2$player2
STK05_TAMg3 <- STK05_TAMg2
STK05_TAMg3$p1inp2vec <- is.element(STK05_TAMg3$player1, player2vector)
STK05_TAMg3$p2inp1vec <- is.element(STK05_TAMg3$player2, player1vector)

addPlayer1 <- STK05_TAMg3[ which(STK05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_TAMg3[ which(STK05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_TAMg2 <- rbind(STK05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
STK05_TAMft <- ftable(STK05_TAMg2$player1, STK05_TAMg2$player2)
STK05_TAMft2 <- as.matrix(STK05_TAMft)
numRows <- nrow(STK05_TAMft2)
numCols <- ncol(STK05_TAMft2)
STK05_TAMft3 <- STK05_TAMft2[c(2:numRows) , c(2:numCols)]
STK05_TAMTable <- graph.adjacency(STK05_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(STK05_TAMTable, vertex.label = V(STK05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
STK05_TAM.clusterCoef <- transitivity(STK05_TAMTable, type="global") #cluster coefficient
STK05_TAM.degreeCent <- centralization.degree(STK05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_TAMftn <- as.network.matrix(STK05_TAMft)
STK05_TAM.netDensity <- network.density(STK05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_TAM.entropy <- entropy(STK05_TAMft) #entropy

STK05_TAM.netMx <- cbind(STK05_TAM.netMx, STK05_TAM.clusterCoef, STK05_TAM.degreeCent$centralization,
                         STK05_TAM.netDensity, STK05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************
#NA

round = 5
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
STK05_SDMg2 <- data.frame(STK05_SDM)
STK05_SDMg2 <- STK05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_SDMg2$player1
player2vector <- STK05_SDMg2$player2
STK05_SDMg3 <- STK05_SDMg2
STK05_SDMg3$p1inp2vec <- is.element(STK05_SDMg3$player1, player2vector)
STK05_SDMg3$p2inp1vec <- is.element(STK05_SDMg3$player2, player1vector)

addPlayer1 <- STK05_SDMg3[ which(STK05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_SDMg3[ which(STK05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_SDMg2 <- rbind(STK05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
STK05_SDMft <- ftable(STK05_SDMg2$player1, STK05_SDMg2$player2)
STK05_SDMft2 <- as.matrix(STK05_SDMft)
numRows <- nrow(STK05_SDMft2)
numCols <- ncol(STK05_SDMft2)
STK05_SDMft3 <- STK05_SDMft2[c(2:numRows) , c(2:numCols)]
STK05_SDMTable <- graph.adjacency(STK05_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(STK05_SDMTable, vertex.label = V(STK05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
STK05_SDM.clusterCoef <- transitivity(STK05_SDMTable, type="global") #cluster coefficient
STK05_SDM.degreeCent <- centralization.degree(STK05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_SDMftn <- as.network.matrix(STK05_SDMft)
STK05_SDM.netDensity <- network.density(STK05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_SDM.entropy <- entropy(STK05_SDMft) #entropy

STK05_SDM.netMx <- cbind(STK05_SDM.netMx, STK05_SDM.clusterCoef, STK05_SDM.degreeCent$centralization,
                         STK05_SDM.netDensity, STK05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "STK"
KIoutcome = "Turnover_DM"
STK05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
STK05_TDMg2 <- data.frame(STK05_TDM)
STK05_TDMg2 <- STK05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_TDMg2$player1
player2vector <- STK05_TDMg2$player2
STK05_TDMg3 <- STK05_TDMg2
STK05_TDMg3$p1inp2vec <- is.element(STK05_TDMg3$player1, player2vector)
STK05_TDMg3$p2inp1vec <- is.element(STK05_TDMg3$player2, player1vector)

addPlayer1 <- STK05_TDMg3[ which(STK05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_TDMg3[ which(STK05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_TDMg2 <- rbind(STK05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
STK05_TDMft <- ftable(STK05_TDMg2$player1, STK05_TDMg2$player2)
STK05_TDMft2 <- as.matrix(STK05_TDMft)
numRows <- nrow(STK05_TDMft2)
numCols <- ncol(STK05_TDMft2)
STK05_TDMft3 <- STK05_TDMft2[c(2:numRows) , c(2:numCols)]
STK05_TDMTable <- graph.adjacency(STK05_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(STK05_TDMTable, vertex.label = V(STK05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
STK05_TDM.clusterCoef <- transitivity(STK05_TDMTable, type="global") #cluster coefficient
STK05_TDM.degreeCent <- centralization.degree(STK05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_TDMftn <- as.network.matrix(STK05_TDMft)
STK05_TDM.netDensity <- network.density(STK05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_TDM.entropy <- entropy(STK05_TDMft) #entropy

STK05_TDM.netMx <- cbind(STK05_TDM.netMx, STK05_TDM.clusterCoef, STK05_TDM.degreeCent$centralization,
                         STK05_TDM.netDensity, STK05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "STK"
KIoutcome = "Stoppage_D"
STK05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
STK05_SDg2 <- data.frame(STK05_SD)
STK05_SDg2 <- STK05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_SDg2$player1
player2vector <- STK05_SDg2$player2
STK05_SDg3 <- STK05_SDg2
STK05_SDg3$p1inp2vec <- is.element(STK05_SDg3$player1, player2vector)
STK05_SDg3$p2inp1vec <- is.element(STK05_SDg3$player2, player1vector)

addPlayer1 <- STK05_SDg3[ which(STK05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_SDg3[ which(STK05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_SDg2 <- rbind(STK05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
STK05_SDft <- ftable(STK05_SDg2$player1, STK05_SDg2$player2)
STK05_SDft2 <- as.matrix(STK05_SDft)
numRows <- nrow(STK05_SDft2)
numCols <- ncol(STK05_SDft2)
STK05_SDft3 <- STK05_SDft2[c(2:numRows) , c(2:numCols)]
STK05_SDTable <- graph.adjacency(STK05_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(STK05_SDTable, vertex.label = V(STK05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
STK05_SD.clusterCoef <- transitivity(STK05_SDTable, type="global") #cluster coefficient
STK05_SD.degreeCent <- centralization.degree(STK05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_SDftn <- as.network.matrix(STK05_SDft)
STK05_SD.netDensity <- network.density(STK05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_SD.entropy <- entropy(STK05_SDft) #entropy

STK05_SD.netMx <- cbind(STK05_SD.netMx, STK05_SD.clusterCoef, STK05_SD.degreeCent$centralization,
                        STK05_SD.netDensity, STK05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************

round = 5
teamName = "STK"
KIoutcome = "Turnover_D"
STK05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
STK05_TDg2 <- data.frame(STK05_TD)
STK05_TDg2 <- STK05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_TDg2$player1
player2vector <- STK05_TDg2$player2
STK05_TDg3 <- STK05_TDg2
STK05_TDg3$p1inp2vec <- is.element(STK05_TDg3$player1, player2vector)
STK05_TDg3$p2inp1vec <- is.element(STK05_TDg3$player2, player1vector)

addPlayer1 <- STK05_TDg3[ which(STK05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK05_TDg3[ which(STK05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_TDg2 <- rbind(STK05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
STK05_TDft <- ftable(STK05_TDg2$player1, STK05_TDg2$player2)
STK05_TDft2 <- as.matrix(STK05_TDft)
numRows <- nrow(STK05_TDft2)
numCols <- ncol(STK05_TDft2)
STK05_TDft3 <- STK05_TDft2[c(2:numRows) , c(2:numCols)]
STK05_TDTable <- graph.adjacency(STK05_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(STK05_TDTable, vertex.label = V(STK05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
STK05_TD.clusterCoef <- transitivity(STK05_TDTable, type="global") #cluster coefficient
STK05_TD.degreeCent <- centralization.degree(STK05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_TDftn <- as.network.matrix(STK05_TDft)
STK05_TD.netDensity <- network.density(STK05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_TD.entropy <- entropy(STK05_TDft) #entropy

STK05_TD.netMx <- cbind(STK05_TD.netMx, STK05_TD.clusterCoef, STK05_TD.degreeCent$centralization,
                        STK05_TD.netDensity, STK05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
STK05_QTg2 <- data.frame(STK05_QT)
STK05_QTg2 <- STK05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK05_QTg2$player1
player2vector <- STK05_QTg2$player2
STK05_QTg3 <- STK05_QTg2
STK05_QTg3$p1inp2vec <- is.element(STK05_QTg3$player1, player2vector)
STK05_QTg3$p2inp1vec <- is.element(STK05_QTg3$player2, player1vector)

addPlayer1 <- STK05_QTg3[ which(STK05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK05_QTg3[ which(STK05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK05_QTg2 <- rbind(STK05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
STK05_QTft <- ftable(STK05_QTg2$player1, STK05_QTg2$player2)
STK05_QTft2 <- as.matrix(STK05_QTft)
numRows <- nrow(STK05_QTft2)
numCols <- ncol(STK05_QTft2)
STK05_QTft3 <- STK05_QTft2[c(2:numRows) , c(2:numCols)]
STK05_QTTable <- graph.adjacency(STK05_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(STK05_QTTable, vertex.label = V(STK05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
STK05_QT.clusterCoef <- transitivity(STK05_QTTable, type="global") #cluster coefficient
STK05_QT.degreeCent <- centralization.degree(STK05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK05_QTftn <- as.network.matrix(STK05_QTft)
STK05_QT.netDensity <- network.density(STK05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK05_QT.entropy <- entropy(STK05_QTft) #entropy

STK05_QT.netMx <- cbind(STK05_QT.netMx, STK05_QT.clusterCoef, STK05_QT.degreeCent$centralization,
                        STK05_QT.netDensity, STK05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK05_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 5 #NA
##

#ROUND 5, Goal***************************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Goal_F"
SYD05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
SYD05_Gg2 <- data.frame(SYD05_G)
SYD05_Gg2 <- SYD05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_Gg2$player1
player2vector <- SYD05_Gg2$player2
SYD05_Gg3 <- SYD05_Gg2
SYD05_Gg3$p1inp2vec <- is.element(SYD05_Gg3$player1, player2vector)
SYD05_Gg3$p2inp1vec <- is.element(SYD05_Gg3$player2, player1vector)

addPlayer1 <- SYD05_Gg3[ which(SYD05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_Gg3[ which(SYD05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_Gg2 <- rbind(SYD05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
SYD05_Gft <- ftable(SYD05_Gg2$player1, SYD05_Gg2$player2)
SYD05_Gft2 <- as.matrix(SYD05_Gft)
numRows <- nrow(SYD05_Gft2)
numCols <- ncol(SYD05_Gft2)
SYD05_Gft3 <- SYD05_Gft2[c(2:numRows) , c(2:numCols)]
SYD05_GTable <- graph.adjacency(SYD05_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(SYD05_GTable, vertex.label = V(SYD05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
SYD05_G.clusterCoef <- transitivity(SYD05_GTable, type="global") #cluster coefficient
SYD05_G.degreeCent <- centralization.degree(SYD05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_Gftn <- as.network.matrix(SYD05_Gft)
SYD05_G.netDensity <- network.density(SYD05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_G.entropy <- entropy(SYD05_Gft) #entropy

SYD05_G.netMx <- cbind(SYD05_G.netMx, SYD05_G.clusterCoef, SYD05_G.degreeCent$centralization,
                       SYD05_G.netDensity, SYD05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Behind_F"
SYD05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
SYD05_Bg2 <- data.frame(SYD05_B)
SYD05_Bg2 <- SYD05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_Bg2$player1
player2vector <- SYD05_Bg2$player2
SYD05_Bg3 <- SYD05_Bg2
SYD05_Bg3$p1inp2vec <- is.element(SYD05_Bg3$player1, player2vector)
SYD05_Bg3$p2inp1vec <- is.element(SYD05_Bg3$player2, player1vector)

addPlayer1 <- SYD05_Bg3[ which(SYD05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_Bg3[ which(SYD05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_Bg2 <- rbind(SYD05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
SYD05_Bft <- ftable(SYD05_Bg2$player1, SYD05_Bg2$player2)
SYD05_Bft2 <- as.matrix(SYD05_Bft)
numRows <- nrow(SYD05_Bft2)
numCols <- ncol(SYD05_Bft2)
SYD05_Bft3 <- SYD05_Bft2[c(2:numRows) , c(2:numCols)]
SYD05_BTable <- graph.adjacency(SYD05_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(SYD05_BTable, vertex.label = V(SYD05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
SYD05_B.clusterCoef <- transitivity(SYD05_BTable, type="global") #cluster coefficient
SYD05_B.degreeCent <- centralization.degree(SYD05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_Bftn <- as.network.matrix(SYD05_Bft)
SYD05_B.netDensity <- network.density(SYD05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_B.entropy <- entropy(SYD05_Bft) #entropy

SYD05_B.netMx <- cbind(SYD05_B.netMx, SYD05_B.clusterCoef, SYD05_B.degreeCent$centralization,
                       SYD05_B.netDensity, SYD05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
SYD05_SFg2 <- data.frame(SYD05_SF)
SYD05_SFg2 <- SYD05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_SFg2$player1
player2vector <- SYD05_SFg2$player2
SYD05_SFg3 <- SYD05_SFg2
SYD05_SFg3$p1inp2vec <- is.element(SYD05_SFg3$player1, player2vector)
SYD05_SFg3$p2inp1vec <- is.element(SYD05_SFg3$player2, player1vector)

addPlayer1 <- SYD05_SFg3[ which(SYD05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_SFg3[ which(SYD05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_SFg2 <- rbind(SYD05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
SYD05_SFft <- ftable(SYD05_SFg2$player1, SYD05_SFg2$player2)
SYD05_SFft2 <- as.matrix(SYD05_SFft)
numRows <- nrow(SYD05_SFft2)
numCols <- ncol(SYD05_SFft2)
SYD05_SFft3 <- SYD05_SFft2[c(2:numRows) , c(2:numCols)]
SYD05_SFTable <- graph.adjacency(SYD05_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(SYD05_SFTable, vertex.label = V(SYD05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
SYD05_SF.clusterCoef <- transitivity(SYD05_SFTable, type="global") #cluster coefficient
SYD05_SF.degreeCent <- centralization.degree(SYD05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_SFftn <- as.network.matrix(SYD05_SFft)
SYD05_SF.netDensity <- network.density(SYD05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_SF.entropy <- entropy(SYD05_SFft) #entropy

SYD05_SF.netMx <- cbind(SYD05_SF.netMx, SYD05_SF.clusterCoef, SYD05_SF.degreeCent$centralization,
                        SYD05_SF.netDensity, SYD05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
SYD05_TFg2 <- data.frame(SYD05_TF)
SYD05_TFg2 <- SYD05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_TFg2$player1
player2vector <- SYD05_TFg2$player2
SYD05_TFg3 <- SYD05_TFg2
SYD05_TFg3$p1inp2vec <- is.element(SYD05_TFg3$player1, player2vector)
SYD05_TFg3$p2inp1vec <- is.element(SYD05_TFg3$player2, player1vector)

addPlayer1 <- SYD05_TFg3[ which(SYD05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_TFg3[ which(SYD05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_TFg2 <- rbind(SYD05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
SYD05_TFft <- ftable(SYD05_TFg2$player1, SYD05_TFg2$player2)
SYD05_TFft2 <- as.matrix(SYD05_TFft)
numRows <- nrow(SYD05_TFft2)
numCols <- ncol(SYD05_TFft2)
SYD05_TFft3 <- SYD05_TFft2[c(2:numRows) , c(2:numCols)]
SYD05_TFTable <- graph.adjacency(SYD05_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(SYD05_TFTable, vertex.label = V(SYD05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
SYD05_TF.clusterCoef <- transitivity(SYD05_TFTable, type="global") #cluster coefficient
SYD05_TF.degreeCent <- centralization.degree(SYD05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_TFftn <- as.network.matrix(SYD05_TFft)
SYD05_TF.netDensity <- network.density(SYD05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_TF.entropy <- entropy(SYD05_TFft) #entropy

SYD05_TF.netMx <- cbind(SYD05_TF.netMx, SYD05_TF.clusterCoef, SYD05_TF.degreeCent$centralization,
                        SYD05_TF.netDensity, SYD05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
SYD05_SAMg2 <- data.frame(SYD05_SAM)
SYD05_SAMg2 <- SYD05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_SAMg2$player1
player2vector <- SYD05_SAMg2$player2
SYD05_SAMg3 <- SYD05_SAMg2
SYD05_SAMg3$p1inp2vec <- is.element(SYD05_SAMg3$player1, player2vector)
SYD05_SAMg3$p2inp1vec <- is.element(SYD05_SAMg3$player2, player1vector)

addPlayer1 <- SYD05_SAMg3[ which(SYD05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_SAMg3[ which(SYD05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_SAMg2 <- rbind(SYD05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
SYD05_SAMft <- ftable(SYD05_SAMg2$player1, SYD05_SAMg2$player2)
SYD05_SAMft2 <- as.matrix(SYD05_SAMft)
numRows <- nrow(SYD05_SAMft2)
numCols <- ncol(SYD05_SAMft2)
SYD05_SAMft3 <- SYD05_SAMft2[c(2:numRows) , c(2:numCols)]
SYD05_SAMTable <- graph.adjacency(SYD05_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(SYD05_SAMTable, vertex.label = V(SYD05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
SYD05_SAM.clusterCoef <- transitivity(SYD05_SAMTable, type="global") #cluster coefficient
SYD05_SAM.degreeCent <- centralization.degree(SYD05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_SAMftn <- as.network.matrix(SYD05_SAMft)
SYD05_SAM.netDensity <- network.density(SYD05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_SAM.entropy <- entropy(SYD05_SAMft) #entropy

SYD05_SAM.netMx <- cbind(SYD05_SAM.netMx, SYD05_SAM.clusterCoef, SYD05_SAM.degreeCent$centralization,
                         SYD05_SAM.netDensity, SYD05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
SYD05_TAMg2 <- data.frame(SYD05_TAM)
SYD05_TAMg2 <- SYD05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_TAMg2$player1
player2vector <- SYD05_TAMg2$player2
SYD05_TAMg3 <- SYD05_TAMg2
SYD05_TAMg3$p1inp2vec <- is.element(SYD05_TAMg3$player1, player2vector)
SYD05_TAMg3$p2inp1vec <- is.element(SYD05_TAMg3$player2, player1vector)

addPlayer1 <- SYD05_TAMg3[ which(SYD05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_TAMg3[ which(SYD05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_TAMg2 <- rbind(SYD05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
SYD05_TAMft <- ftable(SYD05_TAMg2$player1, SYD05_TAMg2$player2)
SYD05_TAMft2 <- as.matrix(SYD05_TAMft)
numRows <- nrow(SYD05_TAMft2)
numCols <- ncol(SYD05_TAMft2)
SYD05_TAMft3 <- SYD05_TAMft2[c(2:numRows) , c(2:numCols)]
SYD05_TAMTable <- graph.adjacency(SYD05_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(SYD05_TAMTable, vertex.label = V(SYD05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
SYD05_TAM.clusterCoef <- transitivity(SYD05_TAMTable, type="global") #cluster coefficient
SYD05_TAM.degreeCent <- centralization.degree(SYD05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_TAMftn <- as.network.matrix(SYD05_TAMft)
SYD05_TAM.netDensity <- network.density(SYD05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_TAM.entropy <- entropy(SYD05_TAMft) #entropy

SYD05_TAM.netMx <- cbind(SYD05_TAM.netMx, SYD05_TAM.clusterCoef, SYD05_TAM.degreeCent$centralization,
                         SYD05_TAM.netDensity, SYD05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
SYD05_SDMg2 <- data.frame(SYD05_SDM)
SYD05_SDMg2 <- SYD05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_SDMg2$player1
player2vector <- SYD05_SDMg2$player2
SYD05_SDMg3 <- SYD05_SDMg2
SYD05_SDMg3$p1inp2vec <- is.element(SYD05_SDMg3$player1, player2vector)
SYD05_SDMg3$p2inp1vec <- is.element(SYD05_SDMg3$player2, player1vector)

addPlayer1 <- SYD05_SDMg3[ which(SYD05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_SDMg3[ which(SYD05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_SDMg2 <- rbind(SYD05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
SYD05_SDMft <- ftable(SYD05_SDMg2$player1, SYD05_SDMg2$player2)
SYD05_SDMft2 <- as.matrix(SYD05_SDMft)
numRows <- nrow(SYD05_SDMft2)
numCols <- ncol(SYD05_SDMft2)
SYD05_SDMft3 <- SYD05_SDMft2[c(2:numRows) , c(2:numCols)]
SYD05_SDMTable <- graph.adjacency(SYD05_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(SYD05_SDMTable, vertex.label = V(SYD05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
SYD05_SDM.clusterCoef <- transitivity(SYD05_SDMTable, type="global") #cluster coefficient
SYD05_SDM.degreeCent <- centralization.degree(SYD05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_SDMftn <- as.network.matrix(SYD05_SDMft)
SYD05_SDM.netDensity <- network.density(SYD05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_SDM.entropy <- entropy(SYD05_SDMft) #entropy

SYD05_SDM.netMx <- cbind(SYD05_SDM.netMx, SYD05_SDM.clusterCoef, SYD05_SDM.degreeCent$centralization,
                         SYD05_SDM.netDensity, SYD05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
SYD05_TDMg2 <- data.frame(SYD05_TDM)
SYD05_TDMg2 <- SYD05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_TDMg2$player1
player2vector <- SYD05_TDMg2$player2
SYD05_TDMg3 <- SYD05_TDMg2
SYD05_TDMg3$p1inp2vec <- is.element(SYD05_TDMg3$player1, player2vector)
SYD05_TDMg3$p2inp1vec <- is.element(SYD05_TDMg3$player2, player1vector)

addPlayer1 <- SYD05_TDMg3[ which(SYD05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_TDMg3[ which(SYD05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_TDMg2 <- rbind(SYD05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
SYD05_TDMft <- ftable(SYD05_TDMg2$player1, SYD05_TDMg2$player2)
SYD05_TDMft2 <- as.matrix(SYD05_TDMft)
numRows <- nrow(SYD05_TDMft2)
numCols <- ncol(SYD05_TDMft2)
SYD05_TDMft3 <- SYD05_TDMft2[c(2:numRows) , c(2:numCols)]
SYD05_TDMTable <- graph.adjacency(SYD05_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(SYD05_TDMTable, vertex.label = V(SYD05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
SYD05_TDM.clusterCoef <- transitivity(SYD05_TDMTable, type="global") #cluster coefficient
SYD05_TDM.degreeCent <- centralization.degree(SYD05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_TDMftn <- as.network.matrix(SYD05_TDMft)
SYD05_TDM.netDensity <- network.density(SYD05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_TDM.entropy <- entropy(SYD05_TDMft) #entropy

SYD05_TDM.netMx <- cbind(SYD05_TDM.netMx, SYD05_TDM.clusterCoef, SYD05_TDM.degreeCent$centralization,
                         SYD05_TDM.netDensity, SYD05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
SYD05_SDg2 <- data.frame(SYD05_SD)
SYD05_SDg2 <- SYD05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_SDg2$player1
player2vector <- SYD05_SDg2$player2
SYD05_SDg3 <- SYD05_SDg2
SYD05_SDg3$p1inp2vec <- is.element(SYD05_SDg3$player1, player2vector)
SYD05_SDg3$p2inp1vec <- is.element(SYD05_SDg3$player2, player1vector)

addPlayer1 <- SYD05_SDg3[ which(SYD05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_SDg3[ which(SYD05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_SDg2 <- rbind(SYD05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
SYD05_SDft <- ftable(SYD05_SDg2$player1, SYD05_SDg2$player2)
SYD05_SDft2 <- as.matrix(SYD05_SDft)
numRows <- nrow(SYD05_SDft2)
numCols <- ncol(SYD05_SDft2)
SYD05_SDft3 <- SYD05_SDft2[c(2:numRows) , c(2:numCols)]
SYD05_SDTable <- graph.adjacency(SYD05_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(SYD05_SDTable, vertex.label = V(SYD05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
SYD05_SD.clusterCoef <- transitivity(SYD05_SDTable, type="global") #cluster coefficient
SYD05_SD.degreeCent <- centralization.degree(SYD05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_SDftn <- as.network.matrix(SYD05_SDft)
SYD05_SD.netDensity <- network.density(SYD05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_SD.entropy <- entropy(SYD05_SDft) #entropy

SYD05_SD.netMx <- cbind(SYD05_SD.netMx, SYD05_SD.clusterCoef, SYD05_SD.degreeCent$centralization,
                        SYD05_SD.netDensity, SYD05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
SYD05_TDg2 <- data.frame(SYD05_TD)
SYD05_TDg2 <- SYD05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_TDg2$player1
player2vector <- SYD05_TDg2$player2
SYD05_TDg3 <- SYD05_TDg2
SYD05_TDg3$p1inp2vec <- is.element(SYD05_TDg3$player1, player2vector)
SYD05_TDg3$p2inp1vec <- is.element(SYD05_TDg3$player2, player1vector)

addPlayer1 <- SYD05_TDg3[ which(SYD05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_TDg3[ which(SYD05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_TDg2 <- rbind(SYD05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
SYD05_TDft <- ftable(SYD05_TDg2$player1, SYD05_TDg2$player2)
SYD05_TDft2 <- as.matrix(SYD05_TDft)
numRows <- nrow(SYD05_TDft2)
numCols <- ncol(SYD05_TDft2)
SYD05_TDft3 <- SYD05_TDft2[c(2:numRows) , c(2:numCols)]
SYD05_TDTable <- graph.adjacency(SYD05_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(SYD05_TDTable, vertex.label = V(SYD05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
SYD05_TD.clusterCoef <- transitivity(SYD05_TDTable, type="global") #cluster coefficient
SYD05_TD.degreeCent <- centralization.degree(SYD05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_TDftn <- as.network.matrix(SYD05_TDft)
SYD05_TD.netDensity <- network.density(SYD05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_TD.entropy <- entropy(SYD05_TDft) #entropy

SYD05_TD.netMx <- cbind(SYD05_TD.netMx, SYD05_TD.clusterCoef, SYD05_TD.degreeCent$centralization,
                        SYD05_TD.netDensity, SYD05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
SYD05_QTg2 <- data.frame(SYD05_QT)
SYD05_QTg2 <- SYD05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD05_QTg2$player1
player2vector <- SYD05_QTg2$player2
SYD05_QTg3 <- SYD05_QTg2
SYD05_QTg3$p1inp2vec <- is.element(SYD05_QTg3$player1, player2vector)
SYD05_QTg3$p2inp1vec <- is.element(SYD05_QTg3$player2, player1vector)

addPlayer1 <- SYD05_QTg3[ which(SYD05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD05_QTg3[ which(SYD05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD05_QTg2 <- rbind(SYD05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
SYD05_QTft <- ftable(SYD05_QTg2$player1, SYD05_QTg2$player2)
SYD05_QTft2 <- as.matrix(SYD05_QTft)
numRows <- nrow(SYD05_QTft2)
numCols <- ncol(SYD05_QTft2)
SYD05_QTft3 <- SYD05_QTft2[c(2:numRows) , c(2:numCols)]
SYD05_QTTable <- graph.adjacency(SYD05_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(SYD05_QTTable, vertex.label = V(SYD05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
SYD05_QT.clusterCoef <- transitivity(SYD05_QTTable, type="global") #cluster coefficient
SYD05_QT.degreeCent <- centralization.degree(SYD05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD05_QTftn <- as.network.matrix(SYD05_QTft)
SYD05_QT.netDensity <- network.density(SYD05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD05_QT.entropy <- entropy(SYD05_QTft) #entropy

SYD05_QT.netMx <- cbind(SYD05_QT.netMx, SYD05_QT.clusterCoef, SYD05_QT.degreeCent$centralization,
                        SYD05_QT.netDensity, SYD05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD05_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************
#NA

round = 5
teamName = "WB"
KIoutcome = "Goal_F"
WB05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
WB05_Gg2 <- data.frame(WB05_G)
WB05_Gg2 <- WB05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_Gg2$player1
player2vector <- WB05_Gg2$player2
WB05_Gg3 <- WB05_Gg2
WB05_Gg3$p1inp2vec <- is.element(WB05_Gg3$player1, player2vector)
WB05_Gg3$p2inp1vec <- is.element(WB05_Gg3$player2, player1vector)

addPlayer1 <- WB05_Gg3[ which(WB05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_Gg3[ which(WB05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_Gg2 <- rbind(WB05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
WB05_Gft <- ftable(WB05_Gg2$player1, WB05_Gg2$player2)
WB05_Gft2 <- as.matrix(WB05_Gft)
numRows <- nrow(WB05_Gft2)
numCols <- ncol(WB05_Gft2)
WB05_Gft3 <- WB05_Gft2[c(2:numRows) , c(2:numCols)]
WB05_GTable <- graph.adjacency(WB05_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(WB05_GTable, vertex.label = V(WB05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
WB05_G.clusterCoef <- transitivity(WB05_GTable, type="global") #cluster coefficient
WB05_G.degreeCent <- centralization.degree(WB05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_Gftn <- as.network.matrix(WB05_Gft)
WB05_G.netDensity <- network.density(WB05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_G.entropy <- entropy(WB05_Gft) #entropy

WB05_G.netMx <- cbind(WB05_G.netMx, WB05_G.clusterCoef, WB05_G.degreeCent$centralization,
                      WB05_G.netDensity, WB05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************
#NA

round = 5
teamName = "WB"
KIoutcome = "Behind_F"
WB05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
WB05_Bg2 <- data.frame(WB05_B)
WB05_Bg2 <- WB05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_Bg2$player1
player2vector <- WB05_Bg2$player2
WB05_Bg3 <- WB05_Bg2
WB05_Bg3$p1inp2vec <- is.element(WB05_Bg3$player1, player2vector)
WB05_Bg3$p2inp1vec <- is.element(WB05_Bg3$player2, player1vector)

addPlayer1 <- WB05_Bg3[ which(WB05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_Bg3[ which(WB05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_Bg2 <- rbind(WB05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
WB05_Bft <- ftable(WB05_Bg2$player1, WB05_Bg2$player2)
WB05_Bft2 <- as.matrix(WB05_Bft)
numRows <- nrow(WB05_Bft2)
numCols <- ncol(WB05_Bft2)
WB05_Bft3 <- WB05_Bft2[c(2:numRows) , c(2:numCols)]
WB05_BTable <- graph.adjacency(WB05_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(WB05_BTable, vertex.label = V(WB05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
WB05_B.clusterCoef <- transitivity(WB05_BTable, type="global") #cluster coefficient
WB05_B.degreeCent <- centralization.degree(WB05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_Bftn <- as.network.matrix(WB05_Bft)
WB05_B.netDensity <- network.density(WB05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_B.entropy <- entropy(WB05_Bft) #entropy

WB05_B.netMx <- cbind(WB05_B.netMx, WB05_B.clusterCoef, WB05_B.degreeCent$centralization,
                      WB05_B.netDensity, WB05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************

round = 5
teamName = "WB"
KIoutcome = "Stoppage_F"
WB05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
WB05_SFg2 <- data.frame(WB05_SF)
WB05_SFg2 <- WB05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_SFg2$player1
player2vector <- WB05_SFg2$player2
WB05_SFg3 <- WB05_SFg2
WB05_SFg3$p1inp2vec <- is.element(WB05_SFg3$player1, player2vector)
WB05_SFg3$p2inp1vec <- is.element(WB05_SFg3$player2, player1vector)

addPlayer1 <- WB05_SFg3[ which(WB05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_SFg3[ which(WB05_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_SFg2 <- rbind(WB05_SFg2, addPlayers)

#ROUND 5, FWD Stoppage graph using weighted edges
WB05_SFft <- ftable(WB05_SFg2$player1, WB05_SFg2$player2)
WB05_SFft2 <- as.matrix(WB05_SFft)
numRows <- nrow(WB05_SFft2)
numCols <- ncol(WB05_SFft2)
WB05_SFft3 <- WB05_SFft2[c(2:numRows) , c(2:numCols)]
WB05_SFTable <- graph.adjacency(WB05_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(WB05_SFTable, vertex.label = V(WB05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
WB05_SF.clusterCoef <- transitivity(WB05_SFTable, type="global") #cluster coefficient
WB05_SF.degreeCent <- centralization.degree(WB05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_SFftn <- as.network.matrix(WB05_SFft)
WB05_SF.netDensity <- network.density(WB05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_SF.entropy <- entropy(WB05_SFft) #entropy

WB05_SF.netMx <- cbind(WB05_SF.netMx, WB05_SF.clusterCoef, WB05_SF.degreeCent$centralization,
                       WB05_SF.netDensity, WB05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************
#NA

round = 5
teamName = "WB"
KIoutcome = "Turnover_F"
WB05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
WB05_TFg2 <- data.frame(WB05_TF)
WB05_TFg2 <- WB05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_TFg2$player1
player2vector <- WB05_TFg2$player2
WB05_TFg3 <- WB05_TFg2
WB05_TFg3$p1inp2vec <- is.element(WB05_TFg3$player1, player2vector)
WB05_TFg3$p2inp1vec <- is.element(WB05_TFg3$player2, player1vector)

addPlayer1 <- WB05_TFg3[ which(WB05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_TFg3[ which(WB05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_TFg2 <- rbind(WB05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
WB05_TFft <- ftable(WB05_TFg2$player1, WB05_TFg2$player2)
WB05_TFft2 <- as.matrix(WB05_TFft)
numRows <- nrow(WB05_TFft2)
numCols <- ncol(WB05_TFft2)
WB05_TFft3 <- WB05_TFft2[c(2:numRows) , c(2:numCols)]
WB05_TFTable <- graph.adjacency(WB05_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(WB05_TFTable, vertex.label = V(WB05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
WB05_TF.clusterCoef <- transitivity(WB05_TFTable, type="global") #cluster coefficient
WB05_TF.degreeCent <- centralization.degree(WB05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_TFftn <- as.network.matrix(WB05_TFft)
WB05_TF.netDensity <- network.density(WB05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_TF.entropy <- entropy(WB05_TFft) #entropy

WB05_TF.netMx <- cbind(WB05_TF.netMx, WB05_TF.clusterCoef, WB05_TF.degreeCent$centralization,
                       WB05_TF.netDensity, WB05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************
#NA

round = 5
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
WB05_SAMg2 <- data.frame(WB05_SAM)
WB05_SAMg2 <- WB05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_SAMg2$player1
player2vector <- WB05_SAMg2$player2
WB05_SAMg3 <- WB05_SAMg2
WB05_SAMg3$p1inp2vec <- is.element(WB05_SAMg3$player1, player2vector)
WB05_SAMg3$p2inp1vec <- is.element(WB05_SAMg3$player2, player1vector)

addPlayer1 <- WB05_SAMg3[ which(WB05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_SAMg3[ which(WB05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_SAMg2 <- rbind(WB05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
WB05_SAMft <- ftable(WB05_SAMg2$player1, WB05_SAMg2$player2)
WB05_SAMft2 <- as.matrix(WB05_SAMft)
numRows <- nrow(WB05_SAMft2)
numCols <- ncol(WB05_SAMft2)
WB05_SAMft3 <- WB05_SAMft2[c(2:numRows) , c(2:numCols)]
WB05_SAMTable <- graph.adjacency(WB05_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(WB05_SAMTable, vertex.label = V(WB05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
WB05_SAM.clusterCoef <- transitivity(WB05_SAMTable, type="global") #cluster coefficient
WB05_SAM.degreeCent <- centralization.degree(WB05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_SAMftn <- as.network.matrix(WB05_SAMft)
WB05_SAM.netDensity <- network.density(WB05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_SAM.entropy <- entropy(WB05_SAMft) #entropy

WB05_SAM.netMx <- cbind(WB05_SAM.netMx, WB05_SAM.clusterCoef, WB05_SAM.degreeCent$centralization,
                        WB05_SAM.netDensity, WB05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************
#NA

round = 5
teamName = "WB"
KIoutcome = "Turnover_AM"
WB05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
WB05_TAMg2 <- data.frame(WB05_TAM)
WB05_TAMg2 <- WB05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_TAMg2$player1
player2vector <- WB05_TAMg2$player2
WB05_TAMg3 <- WB05_TAMg2
WB05_TAMg3$p1inp2vec <- is.element(WB05_TAMg3$player1, player2vector)
WB05_TAMg3$p2inp1vec <- is.element(WB05_TAMg3$player2, player1vector)

addPlayer1 <- WB05_TAMg3[ which(WB05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_TAMg3[ which(WB05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_TAMg2 <- rbind(WB05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
WB05_TAMft <- ftable(WB05_TAMg2$player1, WB05_TAMg2$player2)
WB05_TAMft2 <- as.matrix(WB05_TAMft)
numRows <- nrow(WB05_TAMft2)
numCols <- ncol(WB05_TAMft2)
WB05_TAMft3 <- WB05_TAMft2[c(2:numRows) , c(2:numCols)]
WB05_TAMTable <- graph.adjacency(WB05_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(WB05_TAMTable, vertex.label = V(WB05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
WB05_TAM.clusterCoef <- transitivity(WB05_TAMTable, type="global") #cluster coefficient
WB05_TAM.degreeCent <- centralization.degree(WB05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_TAMftn <- as.network.matrix(WB05_TAMft)
WB05_TAM.netDensity <- network.density(WB05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_TAM.entropy <- entropy(WB05_TAMft) #entropy

WB05_TAM.netMx <- cbind(WB05_TAM.netMx, WB05_TAM.clusterCoef, WB05_TAM.degreeCent$centralization,
                        WB05_TAM.netDensity, WB05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
WB05_SDMg2 <- data.frame(WB05_SDM)
WB05_SDMg2 <- WB05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_SDMg2$player1
player2vector <- WB05_SDMg2$player2
WB05_SDMg3 <- WB05_SDMg2
WB05_SDMg3$p1inp2vec <- is.element(WB05_SDMg3$player1, player2vector)
WB05_SDMg3$p2inp1vec <- is.element(WB05_SDMg3$player2, player1vector)

addPlayer1 <- WB05_SDMg3[ which(WB05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_SDMg3[ which(WB05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_SDMg2 <- rbind(WB05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
WB05_SDMft <- ftable(WB05_SDMg2$player1, WB05_SDMg2$player2)
WB05_SDMft2 <- as.matrix(WB05_SDMft)
numRows <- nrow(WB05_SDMft2)
numCols <- ncol(WB05_SDMft2)
WB05_SDMft3 <- WB05_SDMft2[c(2:numRows) , c(2:numCols)]
WB05_SDMTable <- graph.adjacency(WB05_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(WB05_SDMTable, vertex.label = V(WB05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
WB05_SDM.clusterCoef <- transitivity(WB05_SDMTable, type="global") #cluster coefficient
WB05_SDM.degreeCent <- centralization.degree(WB05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_SDMftn <- as.network.matrix(WB05_SDMft)
WB05_SDM.netDensity <- network.density(WB05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_SDM.entropy <- entropy(WB05_SDMft) #entropy

WB05_SDM.netMx <- cbind(WB05_SDM.netMx, WB05_SDM.clusterCoef, WB05_SDM.degreeCent$centralization,
                        WB05_SDM.netDensity, WB05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "WB"
KIoutcome = "Turnover_DM"
WB05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
WB05_TDMg2 <- data.frame(WB05_TDM)
WB05_TDMg2 <- WB05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_TDMg2$player1
player2vector <- WB05_TDMg2$player2
WB05_TDMg3 <- WB05_TDMg2
WB05_TDMg3$p1inp2vec <- is.element(WB05_TDMg3$player1, player2vector)
WB05_TDMg3$p2inp1vec <- is.element(WB05_TDMg3$player2, player1vector)

addPlayer1 <- WB05_TDMg3[ which(WB05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_TDMg3[ which(WB05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_TDMg2 <- rbind(WB05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
WB05_TDMft <- ftable(WB05_TDMg2$player1, WB05_TDMg2$player2)
WB05_TDMft2 <- as.matrix(WB05_TDMft)
numRows <- nrow(WB05_TDMft2)
numCols <- ncol(WB05_TDMft2)
WB05_TDMft3 <- WB05_TDMft2[c(2:numRows) , c(2:numCols)]
WB05_TDMTable <- graph.adjacency(WB05_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(WB05_TDMTable, vertex.label = V(WB05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
WB05_TDM.clusterCoef <- transitivity(WB05_TDMTable, type="global") #cluster coefficient
WB05_TDM.degreeCent <- centralization.degree(WB05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_TDMftn <- as.network.matrix(WB05_TDMft)
WB05_TDM.netDensity <- network.density(WB05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_TDM.entropy <- entropy(WB05_TDMft) #entropy

WB05_TDM.netMx <- cbind(WB05_TDM.netMx, WB05_TDM.clusterCoef, WB05_TDM.degreeCent$centralization,
                        WB05_TDM.netDensity, WB05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "WB"
KIoutcome = "Stoppage_D"
WB05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
WB05_SDg2 <- data.frame(WB05_SD)
WB05_SDg2 <- WB05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_SDg2$player1
player2vector <- WB05_SDg2$player2
WB05_SDg3 <- WB05_SDg2
WB05_SDg3$p1inp2vec <- is.element(WB05_SDg3$player1, player2vector)
WB05_SDg3$p2inp1vec <- is.element(WB05_SDg3$player2, player1vector)

addPlayer1 <- WB05_SDg3[ which(WB05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_SDg3[ which(WB05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_SDg2 <- rbind(WB05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
WB05_SDft <- ftable(WB05_SDg2$player1, WB05_SDg2$player2)
WB05_SDft2 <- as.matrix(WB05_SDft)
numRows <- nrow(WB05_SDft2)
numCols <- ncol(WB05_SDft2)
WB05_SDft3 <- WB05_SDft2[c(2:numRows) , c(2:numCols)]
WB05_SDTable <- graph.adjacency(WB05_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(WB05_SDTable, vertex.label = V(WB05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
WB05_SD.clusterCoef <- transitivity(WB05_SDTable, type="global") #cluster coefficient
WB05_SD.degreeCent <- centralization.degree(WB05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_SDftn <- as.network.matrix(WB05_SDft)
WB05_SD.netDensity <- network.density(WB05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_SD.entropy <- entropy(WB05_SDft) #entropy

WB05_SD.netMx <- cbind(WB05_SD.netMx, WB05_SD.clusterCoef, WB05_SD.degreeCent$centralization,
                       WB05_SD.netDensity, WB05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "WB"
KIoutcome = "Turnover_D"
WB05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
WB05_TDg2 <- data.frame(WB05_TD)
WB05_TDg2 <- WB05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_TDg2$player1
player2vector <- WB05_TDg2$player2
WB05_TDg3 <- WB05_TDg2
WB05_TDg3$p1inp2vec <- is.element(WB05_TDg3$player1, player2vector)
WB05_TDg3$p2inp1vec <- is.element(WB05_TDg3$player2, player1vector)

addPlayer1 <- WB05_TDg3[ which(WB05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_TDg3[ which(WB05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_TDg2 <- rbind(WB05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
WB05_TDft <- ftable(WB05_TDg2$player1, WB05_TDg2$player2)
WB05_TDft2 <- as.matrix(WB05_TDft)
numRows <- nrow(WB05_TDft2)
numCols <- ncol(WB05_TDft2)
WB05_TDft3 <- WB05_TDft2[c(2:numRows) , c(2:numCols)]
WB05_TDTable <- graph.adjacency(WB05_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(WB05_TDTable, vertex.label = V(WB05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
WB05_TD.clusterCoef <- transitivity(WB05_TDTable, type="global") #cluster coefficient
WB05_TD.degreeCent <- centralization.degree(WB05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_TDftn <- as.network.matrix(WB05_TDft)
WB05_TD.netDensity <- network.density(WB05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_TD.entropy <- entropy(WB05_TDft) #entropy

WB05_TD.netMx <- cbind(WB05_TD.netMx, WB05_TD.clusterCoef, WB05_TD.degreeCent$centralization,
                       WB05_TD.netDensity, WB05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
WB05_QTg2 <- data.frame(WB05_QT)
WB05_QTg2 <- WB05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB05_QTg2$player1
player2vector <- WB05_QTg2$player2
WB05_QTg3 <- WB05_QTg2
WB05_QTg3$p1inp2vec <- is.element(WB05_QTg3$player1, player2vector)
WB05_QTg3$p2inp1vec <- is.element(WB05_QTg3$player2, player1vector)

addPlayer1 <- WB05_QTg3[ which(WB05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB05_QTg3[ which(WB05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB05_QTg2 <- rbind(WB05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
WB05_QTft <- ftable(WB05_QTg2$player1, WB05_QTg2$player2)
WB05_QTft2 <- as.matrix(WB05_QTft)
numRows <- nrow(WB05_QTft2)
numCols <- ncol(WB05_QTft2)
WB05_QTft3 <- WB05_QTft2[c(2:numRows) , c(2:numCols)]
WB05_QTTable <- graph.adjacency(WB05_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(WB05_QTTable, vertex.label = V(WB05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
WB05_QT.clusterCoef <- transitivity(WB05_QTTable, type="global") #cluster coefficient
WB05_QT.degreeCent <- centralization.degree(WB05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB05_QTftn <- as.network.matrix(WB05_QTft)
WB05_QT.netDensity <- network.density(WB05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB05_QT.entropy <- entropy(WB05_QTft) #entropy

WB05_QT.netMx <- cbind(WB05_QT.netMx, WB05_QT.clusterCoef, WB05_QT.degreeCent$centralization,
                       WB05_QT.netDensity, WB05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB05_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 5
##

#ROUND 5, Goal***************************************************************

round = 5
teamName = "WCE"
KIoutcome = "Goal_F"
WCE05_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Goal with weighted edges
WCE05_Gg2 <- data.frame(WCE05_G)
WCE05_Gg2 <- WCE05_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_Gg2$player1
player2vector <- WCE05_Gg2$player2
WCE05_Gg3 <- WCE05_Gg2
WCE05_Gg3$p1inp2vec <- is.element(WCE05_Gg3$player1, player2vector)
WCE05_Gg3$p2inp1vec <- is.element(WCE05_Gg3$player2, player1vector)

addPlayer1 <- WCE05_Gg3[ which(WCE05_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_Gg3[ which(WCE05_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_Gg2 <- rbind(WCE05_Gg2, addPlayers)

#ROUND 5, Goal graph using weighted edges
WCE05_Gft <- ftable(WCE05_Gg2$player1, WCE05_Gg2$player2)
WCE05_Gft2 <- as.matrix(WCE05_Gft)
numRows <- nrow(WCE05_Gft2)
numCols <- ncol(WCE05_Gft2)
WCE05_Gft3 <- WCE05_Gft2[c(2:numRows) , c(2:numCols)]
WCE05_GTable <- graph.adjacency(WCE05_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Goal graph=weighted
plot.igraph(WCE05_GTable, vertex.label = V(WCE05_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Goal calulation of network metrics
#igraph
WCE05_G.clusterCoef <- transitivity(WCE05_GTable, type="global") #cluster coefficient
WCE05_G.degreeCent <- centralization.degree(WCE05_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_Gftn <- as.network.matrix(WCE05_Gft)
WCE05_G.netDensity <- network.density(WCE05_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_G.entropy <- entropy(WCE05_Gft) #entropy

WCE05_G.netMx <- cbind(WCE05_G.netMx, WCE05_G.clusterCoef, WCE05_G.degreeCent$centralization,
                       WCE05_G.netDensity, WCE05_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_G.netMx) <- varnames

#ROUND 5, Behind***************************************************************

round = 5
teamName = "WCE"
KIoutcome = "Behind_F"
WCE05_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, Behind with weighted edges
WCE05_Bg2 <- data.frame(WCE05_B)
WCE05_Bg2 <- WCE05_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_Bg2$player1
player2vector <- WCE05_Bg2$player2
WCE05_Bg3 <- WCE05_Bg2
WCE05_Bg3$p1inp2vec <- is.element(WCE05_Bg3$player1, player2vector)
WCE05_Bg3$p2inp1vec <- is.element(WCE05_Bg3$player2, player1vector)

addPlayer1 <- WCE05_Bg3[ which(WCE05_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_Bg3[ which(WCE05_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_Bg2 <- rbind(WCE05_Bg2, addPlayers)

#ROUND 5, Behind graph using weighted edges
WCE05_Bft <- ftable(WCE05_Bg2$player1, WCE05_Bg2$player2)
WCE05_Bft2 <- as.matrix(WCE05_Bft)
numRows <- nrow(WCE05_Bft2)
numCols <- ncol(WCE05_Bft2)
WCE05_Bft3 <- WCE05_Bft2[c(2:numRows) , c(2:numCols)]
WCE05_BTable <- graph.adjacency(WCE05_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 5, Behind graph=weighted
plot.igraph(WCE05_BTable, vertex.label = V(WCE05_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, Behind calulation of network metrics
#igraph
WCE05_B.clusterCoef <- transitivity(WCE05_BTable, type="global") #cluster coefficient
WCE05_B.degreeCent <- centralization.degree(WCE05_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_Bftn <- as.network.matrix(WCE05_Bft)
WCE05_B.netDensity <- network.density(WCE05_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_B.entropy <- entropy(WCE05_Bft) #entropy

WCE05_B.netMx <- cbind(WCE05_B.netMx, WCE05_B.clusterCoef, WCE05_B.degreeCent$centralization,
                       WCE05_B.netDensity, WCE05_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_B.netMx) <- varnames

#ROUND 5, FWD Stoppage**********************************************************
#NA

round = 5
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE05_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Stoppage with weighted edges
WCE05_SFg2 <- data.frame(WCE05_SF)
WCE05_SFg2 <- WCE05_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_SFg2$player1
player2vector <- WCE05_SFg2$player2
WCE05_SFg3 <- WCE05_SFg2
WCE05_SFg3$p1inp2vec <- is.element(WCE05_SFg3$player1, player2vector)
WCE05_SFg3$p2inp1vec <- is.element(WCE05_SFg3$player2, player1vector)

addPlayer1 <- WCE05_SFg3[ which(WCE05_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE05_SFg2 <- rbind(WCE05_SFg2, addPlayer1)

#ROUND 5, FWD Stoppage graph using weighted edges
WCE05_SFft <- ftable(WCE05_SFg2$player1, WCE05_SFg2$player2)
WCE05_SFft2 <- as.matrix(WCE05_SFft)
numRows <- nrow(WCE05_SFft2)
numCols <- ncol(WCE05_SFft2)
WCE05_SFft3 <- WCE05_SFft2[c(2:numRows) , c(1:numCols)]
WCE05_SFTable <- graph.adjacency(WCE05_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Stoppage graph=weighted
plot.igraph(WCE05_SFTable, vertex.label = V(WCE05_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Stoppage calulation of network metrics
#igraph
WCE05_SF.clusterCoef <- transitivity(WCE05_SFTable, type="global") #cluster coefficient
WCE05_SF.degreeCent <- centralization.degree(WCE05_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_SFftn <- as.network.matrix(WCE05_SFft)
WCE05_SF.netDensity <- network.density(WCE05_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_SF.entropy <- entropy(WCE05_SFft) #entropy

WCE05_SF.netMx <- cbind(WCE05_SF.netMx, WCE05_SF.clusterCoef, WCE05_SF.degreeCent$centralization,
                        WCE05_SF.netDensity, WCE05_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_SF.netMx) <- varnames

#ROUND 5, FWD Turnover**********************************************************
#NA

round = 5
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE05_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, FWD Turnover with weighted edges
WCE05_TFg2 <- data.frame(WCE05_TF)
WCE05_TFg2 <- WCE05_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_TFg2$player1
player2vector <- WCE05_TFg2$player2
WCE05_TFg3 <- WCE05_TFg2
WCE05_TFg3$p1inp2vec <- is.element(WCE05_TFg3$player1, player2vector)
WCE05_TFg3$p2inp1vec <- is.element(WCE05_TFg3$player2, player1vector)

addPlayer1 <- WCE05_TFg3[ which(WCE05_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_TFg3[ which(WCE05_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_TFg2 <- rbind(WCE05_TFg2, addPlayers)

#ROUND 5, FWD Turnover graph using weighted edges
WCE05_TFft <- ftable(WCE05_TFg2$player1, WCE05_TFg2$player2)
WCE05_TFft2 <- as.matrix(WCE05_TFft)
numRows <- nrow(WCE05_TFft2)
numCols <- ncol(WCE05_TFft2)
WCE05_TFft3 <- WCE05_TFft2[c(2:numRows) , c(2:numCols)]
WCE05_TFTable <- graph.adjacency(WCE05_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, FWD Turnover graph=weighted
plot.igraph(WCE05_TFTable, vertex.label = V(WCE05_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, FWD Turnover calulation of network metrics
#igraph
WCE05_TF.clusterCoef <- transitivity(WCE05_TFTable, type="global") #cluster coefficient
WCE05_TF.degreeCent <- centralization.degree(WCE05_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_TFftn <- as.network.matrix(WCE05_TFft)
WCE05_TF.netDensity <- network.density(WCE05_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_TF.entropy <- entropy(WCE05_TFft) #entropy

WCE05_TF.netMx <- cbind(WCE05_TF.netMx, WCE05_TF.clusterCoef, WCE05_TF.degreeCent$centralization,
                        WCE05_TF.netDensity, WCE05_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_TF.netMx) <- varnames

#ROUND 5, AM Stoppage**********************************************************
#NA

round = 5
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE05_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Stoppage with weighted edges
WCE05_SAMg2 <- data.frame(WCE05_SAM)
WCE05_SAMg2 <- WCE05_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_SAMg2$player1
player2vector <- WCE05_SAMg2$player2
WCE05_SAMg3 <- WCE05_SAMg2
WCE05_SAMg3$p1inp2vec <- is.element(WCE05_SAMg3$player1, player2vector)
WCE05_SAMg3$p2inp1vec <- is.element(WCE05_SAMg3$player2, player1vector)

addPlayer1 <- WCE05_SAMg3[ which(WCE05_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_SAMg3[ which(WCE05_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_SAMg2 <- rbind(WCE05_SAMg2, addPlayers)

#ROUND 5, AM Stoppage graph using weighted edges
WCE05_SAMft <- ftable(WCE05_SAMg2$player1, WCE05_SAMg2$player2)
WCE05_SAMft2 <- as.matrix(WCE05_SAMft)
numRows <- nrow(WCE05_SAMft2)
numCols <- ncol(WCE05_SAMft2)
WCE05_SAMft3 <- WCE05_SAMft2[c(2:numRows) , c(2:numCols)]
WCE05_SAMTable <- graph.adjacency(WCE05_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Stoppage graph=weighted
plot.igraph(WCE05_SAMTable, vertex.label = V(WCE05_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Stoppage calulation of network metrics
#igraph
WCE05_SAM.clusterCoef <- transitivity(WCE05_SAMTable, type="global") #cluster coefficient
WCE05_SAM.degreeCent <- centralization.degree(WCE05_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_SAMftn <- as.network.matrix(WCE05_SAMft)
WCE05_SAM.netDensity <- network.density(WCE05_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_SAM.entropy <- entropy(WCE05_SAMft) #entropy

WCE05_SAM.netMx <- cbind(WCE05_SAM.netMx, WCE05_SAM.clusterCoef, WCE05_SAM.degreeCent$centralization,
                         WCE05_SAM.netDensity, WCE05_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_SAM.netMx) <- varnames

#ROUND 5, AM Turnover**********************************************************

round = 5
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE05_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, AM Turnover with weighted edges
WCE05_TAMg2 <- data.frame(WCE05_TAM)
WCE05_TAMg2 <- WCE05_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_TAMg2$player1
player2vector <- WCE05_TAMg2$player2
WCE05_TAMg3 <- WCE05_TAMg2
WCE05_TAMg3$p1inp2vec <- is.element(WCE05_TAMg3$player1, player2vector)
WCE05_TAMg3$p2inp1vec <- is.element(WCE05_TAMg3$player2, player1vector)

addPlayer1 <- WCE05_TAMg3[ which(WCE05_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_TAMg3[ which(WCE05_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_TAMg2 <- rbind(WCE05_TAMg2, addPlayers)

#ROUND 5, AM Turnover graph using weighted edges
WCE05_TAMft <- ftable(WCE05_TAMg2$player1, WCE05_TAMg2$player2)
WCE05_TAMft2 <- as.matrix(WCE05_TAMft)
numRows <- nrow(WCE05_TAMft2)
numCols <- ncol(WCE05_TAMft2)
WCE05_TAMft3 <- WCE05_TAMft2[c(2:numRows) , c(2:numCols)]
WCE05_TAMTable <- graph.adjacency(WCE05_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, AM Turnover graph=weighted
plot.igraph(WCE05_TAMTable, vertex.label = V(WCE05_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, AM Turnover calulation of network metrics
#igraph
WCE05_TAM.clusterCoef <- transitivity(WCE05_TAMTable, type="global") #cluster coefficient
WCE05_TAM.degreeCent <- centralization.degree(WCE05_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_TAMftn <- as.network.matrix(WCE05_TAMft)
WCE05_TAM.netDensity <- network.density(WCE05_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_TAM.entropy <- entropy(WCE05_TAMft) #entropy

WCE05_TAM.netMx <- cbind(WCE05_TAM.netMx, WCE05_TAM.clusterCoef, WCE05_TAM.degreeCent$centralization,
                         WCE05_TAM.netDensity, WCE05_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_TAM.netMx) <- varnames

#ROUND 5, DM Stoppage**********************************************************

round = 5
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE05_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Stoppage with weighted edges
WCE05_SDMg2 <- data.frame(WCE05_SDM)
WCE05_SDMg2 <- WCE05_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_SDMg2$player1
player2vector <- WCE05_SDMg2$player2
WCE05_SDMg3 <- WCE05_SDMg2
WCE05_SDMg3$p1inp2vec <- is.element(WCE05_SDMg3$player1, player2vector)
WCE05_SDMg3$p2inp1vec <- is.element(WCE05_SDMg3$player2, player1vector)

addPlayer1 <- WCE05_SDMg3[ which(WCE05_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_SDMg3[ which(WCE05_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_SDMg2 <- rbind(WCE05_SDMg2, addPlayers)

#ROUND 5, DM Stoppage graph using weighted edges
WCE05_SDMft <- ftable(WCE05_SDMg2$player1, WCE05_SDMg2$player2)
WCE05_SDMft2 <- as.matrix(WCE05_SDMft)
numRows <- nrow(WCE05_SDMft2)
numCols <- ncol(WCE05_SDMft2)
WCE05_SDMft3 <- WCE05_SDMft2[c(2:numRows) , c(2:numCols)]
WCE05_SDMTable <- graph.adjacency(WCE05_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Stoppage graph=weighted
plot.igraph(WCE05_SDMTable, vertex.label = V(WCE05_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Stoppage calulation of network metrics
#igraph
WCE05_SDM.clusterCoef <- transitivity(WCE05_SDMTable, type="global") #cluster coefficient
WCE05_SDM.degreeCent <- centralization.degree(WCE05_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_SDMftn <- as.network.matrix(WCE05_SDMft)
WCE05_SDM.netDensity <- network.density(WCE05_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_SDM.entropy <- entropy(WCE05_SDMft) #entropy

WCE05_SDM.netMx <- cbind(WCE05_SDM.netMx, WCE05_SDM.clusterCoef, WCE05_SDM.degreeCent$centralization,
                         WCE05_SDM.netDensity, WCE05_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_SDM.netMx) <- varnames

#ROUND 5, DM Turnover**********************************************************

round = 5
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE05_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, DM Turnover with weighted edges
WCE05_TDMg2 <- data.frame(WCE05_TDM)
WCE05_TDMg2 <- WCE05_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_TDMg2$player1
player2vector <- WCE05_TDMg2$player2
WCE05_TDMg3 <- WCE05_TDMg2
WCE05_TDMg3$p1inp2vec <- is.element(WCE05_TDMg3$player1, player2vector)
WCE05_TDMg3$p2inp1vec <- is.element(WCE05_TDMg3$player2, player1vector)

addPlayer1 <- WCE05_TDMg3[ which(WCE05_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_TDMg3[ which(WCE05_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_TDMg2 <- rbind(WCE05_TDMg2, addPlayers)

#ROUND 5, DM Turnover graph using weighted edges
WCE05_TDMft <- ftable(WCE05_TDMg2$player1, WCE05_TDMg2$player2)
WCE05_TDMft2 <- as.matrix(WCE05_TDMft)
numRows <- nrow(WCE05_TDMft2)
numCols <- ncol(WCE05_TDMft2)
WCE05_TDMft3 <- WCE05_TDMft2[c(2:numRows) , c(2:numCols)]
WCE05_TDMTable <- graph.adjacency(WCE05_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 5, DM Turnover graph=weighted
plot.igraph(WCE05_TDMTable, vertex.label = V(WCE05_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, DM Turnover calulation of network metrics
#igraph
WCE05_TDM.clusterCoef <- transitivity(WCE05_TDMTable, type="global") #cluster coefficient
WCE05_TDM.degreeCent <- centralization.degree(WCE05_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_TDMftn <- as.network.matrix(WCE05_TDMft)
WCE05_TDM.netDensity <- network.density(WCE05_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_TDM.entropy <- entropy(WCE05_TDMft) #entropy

WCE05_TDM.netMx <- cbind(WCE05_TDM.netMx, WCE05_TDM.clusterCoef, WCE05_TDM.degreeCent$centralization,
                         WCE05_TDM.netDensity, WCE05_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_TDM.netMx) <- varnames

#ROUND 5, D Stoppage**********************************************************
#NA

round = 5
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE05_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Stoppage with weighted edges
WCE05_SDg2 <- data.frame(WCE05_SD)
WCE05_SDg2 <- WCE05_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_SDg2$player1
player2vector <- WCE05_SDg2$player2
WCE05_SDg3 <- WCE05_SDg2
WCE05_SDg3$p1inp2vec <- is.element(WCE05_SDg3$player1, player2vector)
WCE05_SDg3$p2inp1vec <- is.element(WCE05_SDg3$player2, player1vector)

addPlayer1 <- WCE05_SDg3[ which(WCE05_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_SDg3[ which(WCE05_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_SDg2 <- rbind(WCE05_SDg2, addPlayers)

#ROUND 5, D Stoppage graph using weighted edges
WCE05_SDft <- ftable(WCE05_SDg2$player1, WCE05_SDg2$player2)
WCE05_SDft2 <- as.matrix(WCE05_SDft)
numRows <- nrow(WCE05_SDft2)
numCols <- ncol(WCE05_SDft2)
WCE05_SDft3 <- WCE05_SDft2[c(2:numRows) , c(2:numCols)]
WCE05_SDTable <- graph.adjacency(WCE05_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Stoppage graph=weighted
plot.igraph(WCE05_SDTable, vertex.label = V(WCE05_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Stoppage calulation of network metrics
#igraph
WCE05_SD.clusterCoef <- transitivity(WCE05_SDTable, type="global") #cluster coefficient
WCE05_SD.degreeCent <- centralization.degree(WCE05_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_SDftn <- as.network.matrix(WCE05_SDft)
WCE05_SD.netDensity <- network.density(WCE05_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_SD.entropy <- entropy(WCE05_SDft) #entropy

WCE05_SD.netMx <- cbind(WCE05_SD.netMx, WCE05_SD.clusterCoef, WCE05_SD.degreeCent$centralization,
                        WCE05_SD.netDensity, WCE05_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_SD.netMx) <- varnames

#ROUND 5, D Turnover**********************************************************
#NA

round = 5
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE05_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, D Turnover with weighted edges
WCE05_TDg2 <- data.frame(WCE05_TD)
WCE05_TDg2 <- WCE05_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_TDg2$player1
player2vector <- WCE05_TDg2$player2
WCE05_TDg3 <- WCE05_TDg2
WCE05_TDg3$p1inp2vec <- is.element(WCE05_TDg3$player1, player2vector)
WCE05_TDg3$p2inp1vec <- is.element(WCE05_TDg3$player2, player1vector)

addPlayer1 <- WCE05_TDg3[ which(WCE05_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_TDg3[ which(WCE05_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_TDg2 <- rbind(WCE05_TDg2, addPlayers)

#ROUND 5, D Turnover graph using weighted edges
WCE05_TDft <- ftable(WCE05_TDg2$player1, WCE05_TDg2$player2)
WCE05_TDft2 <- as.matrix(WCE05_TDft)
numRows <- nrow(WCE05_TDft2)
numCols <- ncol(WCE05_TDft2)
WCE05_TDft3 <- WCE05_TDft2[c(2:numRows) , c(2:numCols)]
WCE05_TDTable <- graph.adjacency(WCE05_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, D Turnover graph=weighted
plot.igraph(WCE05_TDTable, vertex.label = V(WCE05_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, D Turnover calulation of network metrics
#igraph
WCE05_TD.clusterCoef <- transitivity(WCE05_TDTable, type="global") #cluster coefficient
WCE05_TD.degreeCent <- centralization.degree(WCE05_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_TDftn <- as.network.matrix(WCE05_TDft)
WCE05_TD.netDensity <- network.density(WCE05_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_TD.entropy <- entropy(WCE05_TDft) #entropy

WCE05_TD.netMx <- cbind(WCE05_TD.netMx, WCE05_TD.clusterCoef, WCE05_TD.degreeCent$centralization,
                        WCE05_TD.netDensity, WCE05_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_TD.netMx) <- varnames

#ROUND 5, End of Qtr**********************************************************
#NA

round = 5
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE05_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 5, End of Qtr with weighted edges
WCE05_QTg2 <- data.frame(WCE05_QT)
WCE05_QTg2 <- WCE05_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE05_QTg2$player1
player2vector <- WCE05_QTg2$player2
WCE05_QTg3 <- WCE05_QTg2
WCE05_QTg3$p1inp2vec <- is.element(WCE05_QTg3$player1, player2vector)
WCE05_QTg3$p2inp1vec <- is.element(WCE05_QTg3$player2, player1vector)

addPlayer1 <- WCE05_QTg3[ which(WCE05_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE05_QTg3[ which(WCE05_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE05_QTg2 <- rbind(WCE05_QTg2, addPlayers)

#ROUND 5, End of Qtr graph using weighted edges
WCE05_QTft <- ftable(WCE05_QTg2$player1, WCE05_QTg2$player2)
WCE05_QTft2 <- as.matrix(WCE05_QTft)
numRows <- nrow(WCE05_QTft2)
numCols <- ncol(WCE05_QTft2)
WCE05_QTft3 <- WCE05_QTft2[c(2:numRows) , c(2:numCols)]
WCE05_QTTable <- graph.adjacency(WCE05_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 5, End of Qtr graph=weighted
plot.igraph(WCE05_QTTable, vertex.label = V(WCE05_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE05_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 5, End of Qtr calulation of network metrics
#igraph
WCE05_QT.clusterCoef <- transitivity(WCE05_QTTable, type="global") #cluster coefficient
WCE05_QT.degreeCent <- centralization.degree(WCE05_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE05_QTftn <- as.network.matrix(WCE05_QTft)
WCE05_QT.netDensity <- network.density(WCE05_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE05_QT.entropy <- entropy(WCE05_QTft) #entropy

WCE05_QT.netMx <- cbind(WCE05_QT.netMx, WCE05_QT.clusterCoef, WCE05_QT.degreeCent$centralization,
                        WCE05_QT.netDensity, WCE05_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE05_QT.netMx) <- varnames
