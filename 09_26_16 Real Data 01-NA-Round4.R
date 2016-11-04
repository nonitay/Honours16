#####
#09-26-16- Real data 04
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************

round = 4
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
ADEL04_Gg2 <- data.frame(ADEL04_G)
ADEL04_Gg2 <- ADEL04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_Gg2$player1
player2vector <- ADEL04_Gg2$player2
ADEL04_Gg3 <- ADEL04_Gg2
ADEL04_Gg3$p1inp2vec <- is.element(ADEL04_Gg3$player1, player2vector)
ADEL04_Gg3$p2inp1vec <- is.element(ADEL04_Gg3$player2, player1vector)

addPlayer1 <- ADEL04_Gg3[ which(ADEL04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_Gg3[ which(ADEL04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_Gg2 <- rbind(ADEL04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
ADEL04_Gft <- ftable(ADEL04_Gg2$player1, ADEL04_Gg2$player2)
ADEL04_Gft2 <- as.matrix(ADEL04_Gft)
numRows <- nrow(ADEL04_Gft2)
numCols <- ncol(ADEL04_Gft2)
ADEL04_Gft3 <- ADEL04_Gft2[c(2:numRows) , c(2:numCols)]
ADEL04_GTable <- graph.adjacency(ADEL04_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(ADEL04_GTable, vertex.label = V(ADEL04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
ADEL04_G.clusterCoef <- transitivity(ADEL04_GTable, type="global") #cluster coefficient
ADEL04_G.degreeCent <- centralization.degree(ADEL04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_Gftn <- as.network.matrix(ADEL04_Gft)
ADEL04_G.netDensity <- network.density(ADEL04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_G.entropy <- entropy(ADEL04_Gft) #entropy

ADEL04_G.netMx <- cbind(ADEL04_G.netMx, ADEL04_G.clusterCoef, ADEL04_G.degreeCent$centralization,
                        ADEL04_G.netDensity, ADEL04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
ADEL04_Bg2 <- data.frame(ADEL04_B)
ADEL04_Bg2 <- ADEL04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_Bg2$player1
player2vector <- ADEL04_Bg2$player2
ADEL04_Bg3 <- ADEL04_Bg2
ADEL04_Bg3$p1inp2vec <- is.element(ADEL04_Bg3$player1, player2vector)
ADEL04_Bg3$p2inp1vec <- is.element(ADEL04_Bg3$player2, player1vector)

addPlayer1 <- ADEL04_Bg3[ which(ADEL04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_Bg3[ which(ADEL04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_Bg2 <- rbind(ADEL04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
ADEL04_Bft <- ftable(ADEL04_Bg2$player1, ADEL04_Bg2$player2)
ADEL04_Bft2 <- as.matrix(ADEL04_Bft)
numRows <- nrow(ADEL04_Bft2)
numCols <- ncol(ADEL04_Bft2)
ADEL04_Bft3 <- ADEL04_Bft2[c(2:numRows) , c(2:numCols)]
ADEL04_BTable <- graph.adjacency(ADEL04_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(ADEL04_BTable, vertex.label = V(ADEL04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
ADEL04_B.clusterCoef <- transitivity(ADEL04_BTable, type="global") #cluster coefficient
ADEL04_B.degreeCent <- centralization.degree(ADEL04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_Bftn <- as.network.matrix(ADEL04_Bft)
ADEL04_B.netDensity <- network.density(ADEL04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_B.entropy <- entropy(ADEL04_Bft) #entropy

ADEL04_B.netMx <- cbind(ADEL04_B.netMx, ADEL04_B.clusterCoef, ADEL04_B.degreeCent$centralization,
                        ADEL04_B.netDensity, ADEL04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************

round = 4
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
ADEL04_SFg2 <- data.frame(ADEL04_SF)
ADEL04_SFg2 <- ADEL04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_SFg2$player1
player2vector <- ADEL04_SFg2$player2
ADEL04_SFg3 <- ADEL04_SFg2
ADEL04_SFg3$p1inp2vec <- is.element(ADEL04_SFg3$player1, player2vector)
ADEL04_SFg3$p2inp1vec <- is.element(ADEL04_SFg3$player2, player1vector)

addPlayer1 <- ADEL04_SFg3[ which(ADEL04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_SFg3[ which(ADEL04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_SFg2 <- rbind(ADEL04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
ADEL04_SFft <- ftable(ADEL04_SFg2$player1, ADEL04_SFg2$player2)
ADEL04_SFft2 <- as.matrix(ADEL04_SFft)
numRows <- nrow(ADEL04_SFft2)
numCols <- ncol(ADEL04_SFft2)
ADEL04_SFft3 <- ADEL04_SFft2[c(2:numRows) , c(2:numCols)]
ADEL04_SFTable <- graph.adjacency(ADEL04_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(ADEL04_SFTable, vertex.label = V(ADEL04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
ADEL04_SF.clusterCoef <- transitivity(ADEL04_SFTable, type="global") #cluster coefficient
ADEL04_SF.degreeCent <- centralization.degree(ADEL04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_SFftn <- as.network.matrix(ADEL04_SFft)
ADEL04_SF.netDensity <- network.density(ADEL04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_SF.entropy <- entropy(ADEL04_SFft) #entropy

ADEL04_SF.netMx <- cbind(ADEL04_SF.netMx, ADEL04_SF.clusterCoef, ADEL04_SF.degreeCent$centralization,
                         ADEL04_SF.netDensity, ADEL04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
ADEL04_TFg2 <- data.frame(ADEL04_TF)
ADEL04_TFg2 <- ADEL04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_TFg2$player1
player2vector <- ADEL04_TFg2$player2
ADEL04_TFg3 <- ADEL04_TFg2
ADEL04_TFg3$p1inp2vec <- is.element(ADEL04_TFg3$player1, player2vector)
ADEL04_TFg3$p2inp1vec <- is.element(ADEL04_TFg3$player2, player1vector)

addPlayer1 <- ADEL04_TFg3[ which(ADEL04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_TFg3[ which(ADEL04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_TFg2 <- rbind(ADEL04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
ADEL04_TFft <- ftable(ADEL04_TFg2$player1, ADEL04_TFg2$player2)
ADEL04_TFft2 <- as.matrix(ADEL04_TFft)
numRows <- nrow(ADEL04_TFft2)
numCols <- ncol(ADEL04_TFft2)
ADEL04_TFft3 <- ADEL04_TFft2[c(2:numRows) , c(2:numCols)]
ADEL04_TFTable <- graph.adjacency(ADEL04_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(ADEL04_TFTable, vertex.label = V(ADEL04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
ADEL04_TF.clusterCoef <- transitivity(ADEL04_TFTable, type="global") #cluster coefficient
ADEL04_TF.degreeCent <- centralization.degree(ADEL04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_TFftn <- as.network.matrix(ADEL04_TFft)
ADEL04_TF.netDensity <- network.density(ADEL04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_TF.entropy <- entropy(ADEL04_TFft) #entropy

ADEL04_TF.netMx <- cbind(ADEL04_TF.netMx, ADEL04_TF.clusterCoef, ADEL04_TF.degreeCent$centralization,
                         ADEL04_TF.netDensity, ADEL04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
ADEL04_SAMg2 <- data.frame(ADEL04_SAM)
ADEL04_SAMg2 <- ADEL04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_SAMg2$player1
player2vector <- ADEL04_SAMg2$player2
ADEL04_SAMg3 <- ADEL04_SAMg2
ADEL04_SAMg3$p1inp2vec <- is.element(ADEL04_SAMg3$player1, player2vector)
ADEL04_SAMg3$p2inp1vec <- is.element(ADEL04_SAMg3$player2, player1vector)

addPlayer1 <- ADEL04_SAMg3[ which(ADEL04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_SAMg3[ which(ADEL04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_SAMg2 <- rbind(ADEL04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
ADEL04_SAMft <- ftable(ADEL04_SAMg2$player1, ADEL04_SAMg2$player2)
ADEL04_SAMft2 <- as.matrix(ADEL04_SAMft)
numRows <- nrow(ADEL04_SAMft2)
numCols <- ncol(ADEL04_SAMft2)
ADEL04_SAMft3 <- ADEL04_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL04_SAMTable <- graph.adjacency(ADEL04_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(ADEL04_SAMTable, vertex.label = V(ADEL04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
ADEL04_SAM.clusterCoef <- transitivity(ADEL04_SAMTable, type="global") #cluster coefficient
ADEL04_SAM.degreeCent <- centralization.degree(ADEL04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_SAMftn <- as.network.matrix(ADEL04_SAMft)
ADEL04_SAM.netDensity <- network.density(ADEL04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_SAM.entropy <- entropy(ADEL04_SAMft) #entropy

ADEL04_SAM.netMx <- cbind(ADEL04_SAM.netMx, ADEL04_SAM.clusterCoef, ADEL04_SAM.degreeCent$centralization,
                          ADEL04_SAM.netDensity, ADEL04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
ADEL04_TAMg2 <- data.frame(ADEL04_TAM)
ADEL04_TAMg2 <- ADEL04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_TAMg2$player1
player2vector <- ADEL04_TAMg2$player2
ADEL04_TAMg3 <- ADEL04_TAMg2
ADEL04_TAMg3$p1inp2vec <- is.element(ADEL04_TAMg3$player1, player2vector)
ADEL04_TAMg3$p2inp1vec <- is.element(ADEL04_TAMg3$player2, player1vector)

addPlayer1 <- ADEL04_TAMg3[ which(ADEL04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_TAMg3[ which(ADEL04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_TAMg2 <- rbind(ADEL04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
ADEL04_TAMft <- ftable(ADEL04_TAMg2$player1, ADEL04_TAMg2$player2)
ADEL04_TAMft2 <- as.matrix(ADEL04_TAMft)
numRows <- nrow(ADEL04_TAMft2)
numCols <- ncol(ADEL04_TAMft2)
ADEL04_TAMft3 <- ADEL04_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL04_TAMTable <- graph.adjacency(ADEL04_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(ADEL04_TAMTable, vertex.label = V(ADEL04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
ADEL04_TAM.clusterCoef <- transitivity(ADEL04_TAMTable, type="global") #cluster coefficient
ADEL04_TAM.degreeCent <- centralization.degree(ADEL04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_TAMftn <- as.network.matrix(ADEL04_TAMft)
ADEL04_TAM.netDensity <- network.density(ADEL04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_TAM.entropy <- entropy(ADEL04_TAMft) #entropy

ADEL04_TAM.netMx <- cbind(ADEL04_TAM.netMx, ADEL04_TAM.clusterCoef, ADEL04_TAM.degreeCent$centralization,
                          ADEL04_TAM.netDensity, ADEL04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************
#NA

round = 4
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
ADEL04_SDMg2 <- data.frame(ADEL04_SDM)
ADEL04_SDMg2 <- ADEL04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_SDMg2$player1
player2vector <- ADEL04_SDMg2$player2
ADEL04_SDMg3 <- ADEL04_SDMg2
ADEL04_SDMg3$p1inp2vec <- is.element(ADEL04_SDMg3$player1, player2vector)
ADEL04_SDMg3$p2inp1vec <- is.element(ADEL04_SDMg3$player2, player1vector)

addPlayer1 <- ADEL04_SDMg3[ which(ADEL04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_SDMg3[ which(ADEL04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_SDMg2 <- rbind(ADEL04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
ADEL04_SDMft <- ftable(ADEL04_SDMg2$player1, ADEL04_SDMg2$player2)
ADEL04_SDMft2 <- as.matrix(ADEL04_SDMft)
numRows <- nrow(ADEL04_SDMft2)
numCols <- ncol(ADEL04_SDMft2)
ADEL04_SDMft3 <- ADEL04_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL04_SDMTable <- graph.adjacency(ADEL04_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(ADEL04_SDMTable, vertex.label = V(ADEL04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
ADEL04_SDM.clusterCoef <- transitivity(ADEL04_SDMTable, type="global") #cluster coefficient
ADEL04_SDM.degreeCent <- centralization.degree(ADEL04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_SDMftn <- as.network.matrix(ADEL04_SDMft)
ADEL04_SDM.netDensity <- network.density(ADEL04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_SDM.entropy <- entropy(ADEL04_SDMft) #entropy

ADEL04_SDM.netMx <- cbind(ADEL04_SDM.netMx, ADEL04_SDM.clusterCoef, ADEL04_SDM.degreeCent$centralization,
                          ADEL04_SDM.netDensity, ADEL04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
ADEL04_TDMg2 <- data.frame(ADEL04_TDM)
ADEL04_TDMg2 <- ADEL04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_TDMg2$player1
player2vector <- ADEL04_TDMg2$player2
ADEL04_TDMg3 <- ADEL04_TDMg2
ADEL04_TDMg3$p1inp2vec <- is.element(ADEL04_TDMg3$player1, player2vector)
ADEL04_TDMg3$p2inp1vec <- is.element(ADEL04_TDMg3$player2, player1vector)

addPlayer1 <- ADEL04_TDMg3[ which(ADEL04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_TDMg3[ which(ADEL04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_TDMg2 <- rbind(ADEL04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
ADEL04_TDMft <- ftable(ADEL04_TDMg2$player1, ADEL04_TDMg2$player2)
ADEL04_TDMft2 <- as.matrix(ADEL04_TDMft)
numRows <- nrow(ADEL04_TDMft2)
numCols <- ncol(ADEL04_TDMft2)
ADEL04_TDMft3 <- ADEL04_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL04_TDMTable <- graph.adjacency(ADEL04_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(ADEL04_TDMTable, vertex.label = V(ADEL04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
ADEL04_TDM.clusterCoef <- transitivity(ADEL04_TDMTable, type="global") #cluster coefficient
ADEL04_TDM.degreeCent <- centralization.degree(ADEL04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_TDMftn <- as.network.matrix(ADEL04_TDMft)
ADEL04_TDM.netDensity <- network.density(ADEL04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_TDM.entropy <- entropy(ADEL04_TDMft) #entropy

ADEL04_TDM.netMx <- cbind(ADEL04_TDM.netMx, ADEL04_TDM.clusterCoef, ADEL04_TDM.degreeCent$centralization,
                          ADEL04_TDM.netDensity, ADEL04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
ADEL04_SDg2 <- data.frame(ADEL04_SD)
ADEL04_SDg2 <- ADEL04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_SDg2$player1
player2vector <- ADEL04_SDg2$player2
ADEL04_SDg3 <- ADEL04_SDg2
ADEL04_SDg3$p1inp2vec <- is.element(ADEL04_SDg3$player1, player2vector)
ADEL04_SDg3$p2inp1vec <- is.element(ADEL04_SDg3$player2, player1vector)

addPlayer1 <- ADEL04_SDg3[ which(ADEL04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_SDg3[ which(ADEL04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_SDg2 <- rbind(ADEL04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
ADEL04_SDft <- ftable(ADEL04_SDg2$player1, ADEL04_SDg2$player2)
ADEL04_SDft2 <- as.matrix(ADEL04_SDft)
numRows <- nrow(ADEL04_SDft2)
numCols <- ncol(ADEL04_SDft2)
ADEL04_SDft3 <- ADEL04_SDft2[c(2:numRows) , c(2:numCols)]
ADEL04_SDTable <- graph.adjacency(ADEL04_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(ADEL04_SDTable, vertex.label = V(ADEL04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
ADEL04_SD.clusterCoef <- transitivity(ADEL04_SDTable, type="global") #cluster coefficient
ADEL04_SD.degreeCent <- centralization.degree(ADEL04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_SDftn <- as.network.matrix(ADEL04_SDft)
ADEL04_SD.netDensity <- network.density(ADEL04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_SD.entropy <- entropy(ADEL04_SDft) #entropy

ADEL04_SD.netMx <- cbind(ADEL04_SD.netMx, ADEL04_SD.clusterCoef, ADEL04_SD.degreeCent$centralization,
                         ADEL04_SD.netDensity, ADEL04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
ADEL04_TDg2 <- data.frame(ADEL04_TD)
ADEL04_TDg2 <- ADEL04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_TDg2$player1
player2vector <- ADEL04_TDg2$player2
ADEL04_TDg3 <- ADEL04_TDg2
ADEL04_TDg3$p1inp2vec <- is.element(ADEL04_TDg3$player1, player2vector)
ADEL04_TDg3$p2inp1vec <- is.element(ADEL04_TDg3$player2, player1vector)

addPlayer1 <- ADEL04_TDg3[ which(ADEL04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL04_TDg3[ which(ADEL04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_TDg2 <- rbind(ADEL04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
ADEL04_TDft <- ftable(ADEL04_TDg2$player1, ADEL04_TDg2$player2)
ADEL04_TDft2 <- as.matrix(ADEL04_TDft)
numRows <- nrow(ADEL04_TDft2)
numCols <- ncol(ADEL04_TDft2)
ADEL04_TDft3 <- ADEL04_TDft2[c(2:numRows) , c(2:numCols)]
ADEL04_TDTable <- graph.adjacency(ADEL04_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(ADEL04_TDTable, vertex.label = V(ADEL04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
ADEL04_TD.clusterCoef <- transitivity(ADEL04_TDTable, type="global") #cluster coefficient
ADEL04_TD.degreeCent <- centralization.degree(ADEL04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_TDftn <- as.network.matrix(ADEL04_TDft)
ADEL04_TD.netDensity <- network.density(ADEL04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_TD.entropy <- entropy(ADEL04_TDft) #entropy

ADEL04_TD.netMx <- cbind(ADEL04_TD.netMx, ADEL04_TD.clusterCoef, ADEL04_TD.degreeCent$centralization,
                         ADEL04_TD.netDensity, ADEL04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************

round = 4
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
ADEL04_QTg2 <- data.frame(ADEL04_QT)
ADEL04_QTg2 <- ADEL04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL04_QTg2$player1
player2vector <- ADEL04_QTg2$player2
ADEL04_QTg3 <- ADEL04_QTg2
ADEL04_QTg3$p1inp2vec <- is.element(ADEL04_QTg3$player1, player2vector)
ADEL04_QTg3$p2inp1vec <- is.element(ADEL04_QTg3$player2, player1vector)

addPlayer1 <- ADEL04_QTg3[ which(ADEL04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ADEL04_QTg3[ which(ADEL04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL04_QTg2 <- rbind(ADEL04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
ADEL04_QTft <- ftable(ADEL04_QTg2$player1, ADEL04_QTg2$player2)
ADEL04_QTft2 <- as.matrix(ADEL04_QTft)
numRows <- nrow(ADEL04_QTft2)
numCols <- ncol(ADEL04_QTft2)
ADEL04_QTft3 <- ADEL04_QTft2[c(2:numRows) , c(2:numCols)]
ADEL04_QTTable <- graph.adjacency(ADEL04_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(ADEL04_QTTable, vertex.label = V(ADEL04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
ADEL04_QT.clusterCoef <- transitivity(ADEL04_QTTable, type="global") #cluster coefficient
ADEL04_QT.degreeCent <- centralization.degree(ADEL04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL04_QTftn <- as.network.matrix(ADEL04_QTft)
ADEL04_QT.netDensity <- network.density(ADEL04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL04_QT.entropy <- entropy(ADEL04_QTft) #entropy

ADEL04_QT.netMx <- cbind(ADEL04_QT.netMx, ADEL04_QT.clusterCoef, ADEL04_QT.degreeCent$centralization,
                         ADEL04_QT.netDensity, ADEL04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL04_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "BL"
KIoutcome = "Goal_F"
BL04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
BL04_Gg2 <- data.frame(BL04_G)
BL04_Gg2 <- BL04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_Gg2$player1
player2vector <- BL04_Gg2$player2
BL04_Gg3 <- BL04_Gg2
BL04_Gg3$p1inp2vec <- is.element(BL04_Gg3$player1, player2vector)
BL04_Gg3$p2inp1vec <- is.element(BL04_Gg3$player2, player1vector)

addPlayer1 <- BL04_Gg3[ which(BL04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_Gg3[ which(BL04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_Gg2 <- rbind(BL04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
BL04_Gft <- ftable(BL04_Gg2$player1, BL04_Gg2$player2)
BL04_Gft2 <- as.matrix(BL04_Gft)
numRows <- nrow(BL04_Gft2)
numCols <- ncol(BL04_Gft2)
BL04_Gft3 <- BL04_Gft2[c(2:numRows) , c(2:numCols)]
BL04_GTable <- graph.adjacency(BL04_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(BL04_GTable, vertex.label = V(BL04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
BL04_G.clusterCoef <- transitivity(BL04_GTable, type="global") #cluster coefficient
BL04_G.degreeCent <- centralization.degree(BL04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_Gftn <- as.network.matrix(BL04_Gft)
BL04_G.netDensity <- network.density(BL04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_G.entropy <- entropy(BL04_Gft) #entropy

BL04_G.netMx <- cbind(BL04_G.netMx, BL04_G.clusterCoef, BL04_G.degreeCent$centralization,
                      BL04_G.netDensity, BL04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************

round = 4
teamName = "BL"
KIoutcome = "Behind_F"
BL04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
BL04_Bg2 <- data.frame(BL04_B)
BL04_Bg2 <- BL04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_Bg2$player1
player2vector <- BL04_Bg2$player2
BL04_Bg3 <- BL04_Bg2
BL04_Bg3$p1inp2vec <- is.element(BL04_Bg3$player1, player2vector)
BL04_Bg3$p2inp1vec <- is.element(BL04_Bg3$player2, player1vector)

addPlayer1 <- BL04_Bg3[ which(BL04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_Bg3[ which(BL04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_Bg2 <- rbind(BL04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
BL04_Bft <- ftable(BL04_Bg2$player1, BL04_Bg2$player2)
BL04_Bft2 <- as.matrix(BL04_Bft)
numRows <- nrow(BL04_Bft2)
numCols <- ncol(BL04_Bft2)
BL04_Bft3 <- BL04_Bft2[c(2:numRows) , c(2:numCols)]
BL04_BTable <- graph.adjacency(BL04_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(BL04_BTable, vertex.label = V(BL04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
BL04_B.clusterCoef <- transitivity(BL04_BTable, type="global") #cluster coefficient
BL04_B.degreeCent <- centralization.degree(BL04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_Bftn <- as.network.matrix(BL04_Bft)
BL04_B.netDensity <- network.density(BL04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_B.entropy <- entropy(BL04_Bft) #entropy

BL04_B.netMx <- cbind(BL04_B.netMx, BL04_B.clusterCoef, BL04_B.degreeCent$centralization,
                      BL04_B.netDensity, BL04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "BL"
KIoutcome = "Stoppage_F"
BL04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
BL04_SFg2 <- data.frame(BL04_SF)
BL04_SFg2 <- BL04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_SFg2$player1
player2vector <- BL04_SFg2$player2
BL04_SFg3 <- BL04_SFg2
BL04_SFg3$p1inp2vec <- is.element(BL04_SFg3$player1, player2vector)
BL04_SFg3$p2inp1vec <- is.element(BL04_SFg3$player2, player1vector)

addPlayer1 <- BL04_SFg3[ which(BL04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_SFg3[ which(BL04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_SFg2 <- rbind(BL04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
BL04_SFft <- ftable(BL04_SFg2$player1, BL04_SFg2$player2)
BL04_SFft2 <- as.matrix(BL04_SFft)
numRows <- nrow(BL04_SFft2)
numCols <- ncol(BL04_SFft2)
BL04_SFft3 <- BL04_SFft2[c(2:numRows) , c(2:numCols)]
BL04_SFTable <- graph.adjacency(BL04_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(BL04_SFTable, vertex.label = V(BL04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
BL04_SF.clusterCoef <- transitivity(BL04_SFTable, type="global") #cluster coefficient
BL04_SF.degreeCent <- centralization.degree(BL04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_SFftn <- as.network.matrix(BL04_SFft)
BL04_SF.netDensity <- network.density(BL04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_SF.entropy <- entropy(BL04_SFft) #entropy

BL04_SF.netMx <- cbind(BL04_SF.netMx, BL04_SF.clusterCoef, BL04_SF.degreeCent$centralization,
                       BL04_SF.netDensity, BL04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************
#NA

round = 4
teamName = "BL"
KIoutcome = "Turnover_F"
BL04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
BL04_TFg2 <- data.frame(BL04_TF)
BL04_TFg2 <- BL04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_TFg2$player1
player2vector <- BL04_TFg2$player2
BL04_TFg3 <- BL04_TFg2
BL04_TFg3$p1inp2vec <- is.element(BL04_TFg3$player1, player2vector)
BL04_TFg3$p2inp1vec <- is.element(BL04_TFg3$player2, player1vector)

addPlayer1 <- BL04_TFg3[ which(BL04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_TFg3[ which(BL04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_TFg2 <- rbind(BL04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
BL04_TFft <- ftable(BL04_TFg2$player1, BL04_TFg2$player2)
BL04_TFft2 <- as.matrix(BL04_TFft)
numRows <- nrow(BL04_TFft2)
numCols <- ncol(BL04_TFft2)
BL04_TFft3 <- BL04_TFft2[c(2:numRows) , c(2:numCols)]
BL04_TFTable <- graph.adjacency(BL04_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(BL04_TFTable, vertex.label = V(BL04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
BL04_TF.clusterCoef <- transitivity(BL04_TFTable, type="global") #cluster coefficient
BL04_TF.degreeCent <- centralization.degree(BL04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_TFftn <- as.network.matrix(BL04_TFft)
BL04_TF.netDensity <- network.density(BL04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_TF.entropy <- entropy(BL04_TFft) #entropy

BL04_TF.netMx <- cbind(BL04_TF.netMx, BL04_TF.clusterCoef, BL04_TF.degreeCent$centralization,
                       BL04_TF.netDensity, BL04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
BL04_SAMg2 <- data.frame(BL04_SAM)
BL04_SAMg2 <- BL04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_SAMg2$player1
player2vector <- BL04_SAMg2$player2
BL04_SAMg3 <- BL04_SAMg2
BL04_SAMg3$p1inp2vec <- is.element(BL04_SAMg3$player1, player2vector)
BL04_SAMg3$p2inp1vec <- is.element(BL04_SAMg3$player2, player1vector)

addPlayer1 <- BL04_SAMg3[ which(BL04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_SAMg3[ which(BL04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_SAMg2 <- rbind(BL04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
BL04_SAMft <- ftable(BL04_SAMg2$player1, BL04_SAMg2$player2)
BL04_SAMft2 <- as.matrix(BL04_SAMft)
numRows <- nrow(BL04_SAMft2)
numCols <- ncol(BL04_SAMft2)
BL04_SAMft3 <- BL04_SAMft2[c(2:numRows) , c(2:numCols)]
BL04_SAMTable <- graph.adjacency(BL04_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(BL04_SAMTable, vertex.label = V(BL04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
BL04_SAM.clusterCoef <- transitivity(BL04_SAMTable, type="global") #cluster coefficient
BL04_SAM.degreeCent <- centralization.degree(BL04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_SAMftn <- as.network.matrix(BL04_SAMft)
BL04_SAM.netDensity <- network.density(BL04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_SAM.entropy <- entropy(BL04_SAMft) #entropy

BL04_SAM.netMx <- cbind(BL04_SAM.netMx, BL04_SAM.clusterCoef, BL04_SAM.degreeCent$centralization,
                        BL04_SAM.netDensity, BL04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************
#NA

round = 4
teamName = "BL"
KIoutcome = "Turnover_AM"
BL04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
BL04_TAMg2 <- data.frame(BL04_TAM)
BL04_TAMg2 <- BL04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_TAMg2$player1
player2vector <- BL04_TAMg2$player2
BL04_TAMg3 <- BL04_TAMg2
BL04_TAMg3$p1inp2vec <- is.element(BL04_TAMg3$player1, player2vector)
BL04_TAMg3$p2inp1vec <- is.element(BL04_TAMg3$player2, player1vector)

addPlayer1 <- BL04_TAMg3[ which(BL04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_TAMg3[ which(BL04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_TAMg2 <- rbind(BL04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
BL04_TAMft <- ftable(BL04_TAMg2$player1, BL04_TAMg2$player2)
BL04_TAMft2 <- as.matrix(BL04_TAMft)
numRows <- nrow(BL04_TAMft2)
numCols <- ncol(BL04_TAMft2)
BL04_TAMft3 <- BL04_TAMft2[c(2:numRows) , c(2:numCols)]
BL04_TAMTable <- graph.adjacency(BL04_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(BL04_TAMTable, vertex.label = V(BL04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
BL04_TAM.clusterCoef <- transitivity(BL04_TAMTable, type="global") #cluster coefficient
BL04_TAM.degreeCent <- centralization.degree(BL04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_TAMftn <- as.network.matrix(BL04_TAMft)
BL04_TAM.netDensity <- network.density(BL04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_TAM.entropy <- entropy(BL04_TAMft) #entropy

BL04_TAM.netMx <- cbind(BL04_TAM.netMx, BL04_TAM.clusterCoef, BL04_TAM.degreeCent$centralization,
                        BL04_TAM.netDensity, BL04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
BL04_SDMg2 <- data.frame(BL04_SDM)
BL04_SDMg2 <- BL04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_SDMg2$player1
player2vector <- BL04_SDMg2$player2
BL04_SDMg3 <- BL04_SDMg2
BL04_SDMg3$p1inp2vec <- is.element(BL04_SDMg3$player1, player2vector)
BL04_SDMg3$p2inp1vec <- is.element(BL04_SDMg3$player2, player1vector)

addPlayer1 <- BL04_SDMg3[ which(BL04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_SDMg3[ which(BL04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_SDMg2 <- rbind(BL04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
BL04_SDMft <- ftable(BL04_SDMg2$player1, BL04_SDMg2$player2)
BL04_SDMft2 <- as.matrix(BL04_SDMft)
numRows <- nrow(BL04_SDMft2)
numCols <- ncol(BL04_SDMft2)
BL04_SDMft3 <- BL04_SDMft2[c(2:numRows) , c(2:numCols)]
BL04_SDMTable <- graph.adjacency(BL04_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(BL04_SDMTable, vertex.label = V(BL04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
BL04_SDM.clusterCoef <- transitivity(BL04_SDMTable, type="global") #cluster coefficient
BL04_SDM.degreeCent <- centralization.degree(BL04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_SDMftn <- as.network.matrix(BL04_SDMft)
BL04_SDM.netDensity <- network.density(BL04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_SDM.entropy <- entropy(BL04_SDMft) #entropy

BL04_SDM.netMx <- cbind(BL04_SDM.netMx, BL04_SDM.clusterCoef, BL04_SDM.degreeCent$centralization,
                        BL04_SDM.netDensity, BL04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "BL"
KIoutcome = "Turnover_DM"
BL04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
BL04_TDMg2 <- data.frame(BL04_TDM)
BL04_TDMg2 <- BL04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_TDMg2$player1
player2vector <- BL04_TDMg2$player2
BL04_TDMg3 <- BL04_TDMg2
BL04_TDMg3$p1inp2vec <- is.element(BL04_TDMg3$player1, player2vector)
BL04_TDMg3$p2inp1vec <- is.element(BL04_TDMg3$player2, player1vector)

addPlayer1 <- BL04_TDMg3[ which(BL04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_TDMg3[ which(BL04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_TDMg2 <- rbind(BL04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
BL04_TDMft <- ftable(BL04_TDMg2$player1, BL04_TDMg2$player2)
BL04_TDMft2 <- as.matrix(BL04_TDMft)
numRows <- nrow(BL04_TDMft2)
numCols <- ncol(BL04_TDMft2)
BL04_TDMft3 <- BL04_TDMft2[c(2:numRows) , c(2:numCols)]
BL04_TDMTable <- graph.adjacency(BL04_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(BL04_TDMTable, vertex.label = V(BL04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
BL04_TDM.clusterCoef <- transitivity(BL04_TDMTable, type="global") #cluster coefficient
BL04_TDM.degreeCent <- centralization.degree(BL04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_TDMftn <- as.network.matrix(BL04_TDMft)
BL04_TDM.netDensity <- network.density(BL04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_TDM.entropy <- entropy(BL04_TDMft) #entropy

BL04_TDM.netMx <- cbind(BL04_TDM.netMx, BL04_TDM.clusterCoef, BL04_TDM.degreeCent$centralization,
                        BL04_TDM.netDensity, BL04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "BL"
KIoutcome = "Stoppage_D"
BL04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
BL04_SDg2 <- data.frame(BL04_SD)
BL04_SDg2 <- BL04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_SDg2$player1
player2vector <- BL04_SDg2$player2
BL04_SDg3 <- BL04_SDg2
BL04_SDg3$p1inp2vec <- is.element(BL04_SDg3$player1, player2vector)
BL04_SDg3$p2inp1vec <- is.element(BL04_SDg3$player2, player1vector)

addPlayer1 <- BL04_SDg3[ which(BL04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_SDg3[ which(BL04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_SDg2 <- rbind(BL04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
BL04_SDft <- ftable(BL04_SDg2$player1, BL04_SDg2$player2)
BL04_SDft2 <- as.matrix(BL04_SDft)
numRows <- nrow(BL04_SDft2)
numCols <- ncol(BL04_SDft2)
BL04_SDft3 <- BL04_SDft2[c(2:numRows) , c(2:numCols)]
BL04_SDTable <- graph.adjacency(BL04_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(BL04_SDTable, vertex.label = V(BL04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
BL04_SD.clusterCoef <- transitivity(BL04_SDTable, type="global") #cluster coefficient
BL04_SD.degreeCent <- centralization.degree(BL04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_SDftn <- as.network.matrix(BL04_SDft)
BL04_SD.netDensity <- network.density(BL04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_SD.entropy <- entropy(BL04_SDft) #entropy

BL04_SD.netMx <- cbind(BL04_SD.netMx, BL04_SD.clusterCoef, BL04_SD.degreeCent$centralization,
                       BL04_SD.netDensity, BL04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "BL"
KIoutcome = "Turnover_D"
BL04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
BL04_TDg2 <- data.frame(BL04_TD)
BL04_TDg2 <- BL04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_TDg2$player1
player2vector <- BL04_TDg2$player2
BL04_TDg3 <- BL04_TDg2
BL04_TDg3$p1inp2vec <- is.element(BL04_TDg3$player1, player2vector)
BL04_TDg3$p2inp1vec <- is.element(BL04_TDg3$player2, player1vector)

addPlayer1 <- BL04_TDg3[ which(BL04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_TDg3[ which(BL04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_TDg2 <- rbind(BL04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
BL04_TDft <- ftable(BL04_TDg2$player1, BL04_TDg2$player2)
BL04_TDft2 <- as.matrix(BL04_TDft)
numRows <- nrow(BL04_TDft2)
numCols <- ncol(BL04_TDft2)
BL04_TDft3 <- BL04_TDft2[c(2:numRows) , c(2:numCols)]
BL04_TDTable <- graph.adjacency(BL04_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(BL04_TDTable, vertex.label = V(BL04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
BL04_TD.clusterCoef <- transitivity(BL04_TDTable, type="global") #cluster coefficient
BL04_TD.degreeCent <- centralization.degree(BL04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_TDftn <- as.network.matrix(BL04_TDft)
BL04_TD.netDensity <- network.density(BL04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_TD.entropy <- entropy(BL04_TDft) #entropy

BL04_TD.netMx <- cbind(BL04_TD.netMx, BL04_TD.clusterCoef, BL04_TD.degreeCent$centralization,
                       BL04_TD.netDensity, BL04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
BL04_QTg2 <- data.frame(BL04_QT)
BL04_QTg2 <- BL04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL04_QTg2$player1
player2vector <- BL04_QTg2$player2
BL04_QTg3 <- BL04_QTg2
BL04_QTg3$p1inp2vec <- is.element(BL04_QTg3$player1, player2vector)
BL04_QTg3$p2inp1vec <- is.element(BL04_QTg3$player2, player1vector)

addPlayer1 <- BL04_QTg3[ which(BL04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL04_QTg3[ which(BL04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL04_QTg2 <- rbind(BL04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
BL04_QTft <- ftable(BL04_QTg2$player1, BL04_QTg2$player2)
BL04_QTft2 <- as.matrix(BL04_QTft)
numRows <- nrow(BL04_QTft2)
numCols <- ncol(BL04_QTft2)
BL04_QTft3 <- BL04_QTft2[c(2:numRows) , c(2:numCols)]
BL04_QTTable <- graph.adjacency(BL04_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(BL04_QTTable, vertex.label = V(BL04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
BL04_QT.clusterCoef <- transitivity(BL04_QTTable, type="global") #cluster coefficient
BL04_QT.degreeCent <- centralization.degree(BL04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL04_QTftn <- as.network.matrix(BL04_QTft)
BL04_QT.netDensity <- network.density(BL04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL04_QT.entropy <- entropy(BL04_QTft) #entropy

BL04_QT.netMx <- cbind(BL04_QT.netMx, BL04_QT.clusterCoef, BL04_QT.degreeCent$centralization,
                       BL04_QT.netDensity, BL04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL04_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "CARL"
KIoutcome = "Goal_F"
CARL04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
CARL04_Gg2 <- data.frame(CARL04_G)
CARL04_Gg2 <- CARL04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_Gg2$player1
player2vector <- CARL04_Gg2$player2
CARL04_Gg3 <- CARL04_Gg2
CARL04_Gg3$p1inp2vec <- is.element(CARL04_Gg3$player1, player2vector)
CARL04_Gg3$p2inp1vec <- is.element(CARL04_Gg3$player2, player1vector)

addPlayer1 <- CARL04_Gg3[ which(CARL04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_Gg3[ which(CARL04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_Gg2 <- rbind(CARL04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
CARL04_Gft <- ftable(CARL04_Gg2$player1, CARL04_Gg2$player2)
CARL04_Gft2 <- as.matrix(CARL04_Gft)
numRows <- nrow(CARL04_Gft2)
numCols <- ncol(CARL04_Gft2)
CARL04_Gft3 <- CARL04_Gft2[c(2:numRows) , c(2:numCols)]
CARL04_GTable <- graph.adjacency(CARL04_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(CARL04_GTable, vertex.label = V(CARL04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
CARL04_G.clusterCoef <- transitivity(CARL04_GTable, type="global") #cluster coefficient
CARL04_G.degreeCent <- centralization.degree(CARL04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_Gftn <- as.network.matrix(CARL04_Gft)
CARL04_G.netDensity <- network.density(CARL04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_G.entropy <- entropy(CARL04_Gft) #entropy

CARL04_G.netMx <- cbind(CARL04_G.netMx, CARL04_G.clusterCoef, CARL04_G.degreeCent$centralization,
                        CARL04_G.netDensity, CARL04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "CARL"
KIoutcome = "Behind_F"
CARL04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
CARL04_Bg2 <- data.frame(CARL04_B)
CARL04_Bg2 <- CARL04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_Bg2$player1
player2vector <- CARL04_Bg2$player2
CARL04_Bg3 <- CARL04_Bg2
CARL04_Bg3$p1inp2vec <- is.element(CARL04_Bg3$player1, player2vector)
CARL04_Bg3$p2inp1vec <- is.element(CARL04_Bg3$player2, player1vector)

addPlayer1 <- CARL04_Bg3[ which(CARL04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_Bg3[ which(CARL04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_Bg2 <- rbind(CARL04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
CARL04_Bft <- ftable(CARL04_Bg2$player1, CARL04_Bg2$player2)
CARL04_Bft2 <- as.matrix(CARL04_Bft)
numRows <- nrow(CARL04_Bft2)
numCols <- ncol(CARL04_Bft2)
CARL04_Bft3 <- CARL04_Bft2[c(2:numRows) , c(2:numCols)]
CARL04_BTable <- graph.adjacency(CARL04_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(CARL04_BTable, vertex.label = V(CARL04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
CARL04_B.clusterCoef <- transitivity(CARL04_BTable, type="global") #cluster coefficient
CARL04_B.degreeCent <- centralization.degree(CARL04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_Bftn <- as.network.matrix(CARL04_Bft)
CARL04_B.netDensity <- network.density(CARL04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_B.entropy <- entropy(CARL04_Bft) #entropy

CARL04_B.netMx <- cbind(CARL04_B.netMx, CARL04_B.clusterCoef, CARL04_B.degreeCent$centralization,
                        CARL04_B.netDensity, CARL04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
CARL04_SFg2 <- data.frame(CARL04_SF)
CARL04_SFg2 <- CARL04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_SFg2$player1
player2vector <- CARL04_SFg2$player2
CARL04_SFg3 <- CARL04_SFg2
CARL04_SFg3$p1inp2vec <- is.element(CARL04_SFg3$player1, player2vector)
CARL04_SFg3$p2inp1vec <- is.element(CARL04_SFg3$player2, player1vector)

addPlayer1 <- CARL04_SFg3[ which(CARL04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_SFg3[ which(CARL04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_SFg2 <- rbind(CARL04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
CARL04_SFft <- ftable(CARL04_SFg2$player1, CARL04_SFg2$player2)
CARL04_SFft2 <- as.matrix(CARL04_SFft)
numRows <- nrow(CARL04_SFft2)
numCols <- ncol(CARL04_SFft2)
CARL04_SFft3 <- CARL04_SFft2[c(2:numRows) , c(2:numCols)]
CARL04_SFTable <- graph.adjacency(CARL04_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(CARL04_SFTable, vertex.label = V(CARL04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
CARL04_SF.clusterCoef <- transitivity(CARL04_SFTable, type="global") #cluster coefficient
CARL04_SF.degreeCent <- centralization.degree(CARL04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_SFftn <- as.network.matrix(CARL04_SFft)
CARL04_SF.netDensity <- network.density(CARL04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_SF.entropy <- entropy(CARL04_SFft) #entropy

CARL04_SF.netMx <- cbind(CARL04_SF.netMx, CARL04_SF.clusterCoef, CARL04_SF.degreeCent$centralization,
                         CARL04_SF.netDensity, CARL04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
CARL04_TFg2 <- data.frame(CARL04_TF)
CARL04_TFg2 <- CARL04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_TFg2$player1
player2vector <- CARL04_TFg2$player2
CARL04_TFg3 <- CARL04_TFg2
CARL04_TFg3$p1inp2vec <- is.element(CARL04_TFg3$player1, player2vector)
CARL04_TFg3$p2inp1vec <- is.element(CARL04_TFg3$player2, player1vector)

addPlayer1 <- CARL04_TFg3[ which(CARL04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_TFg3[ which(CARL04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_TFg2 <- rbind(CARL04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
CARL04_TFft <- ftable(CARL04_TFg2$player1, CARL04_TFg2$player2)
CARL04_TFft2 <- as.matrix(CARL04_TFft)
numRows <- nrow(CARL04_TFft2)
numCols <- ncol(CARL04_TFft2)
CARL04_TFft3 <- CARL04_TFft2[c(2:numRows) , c(2:numCols)]
CARL04_TFTable <- graph.adjacency(CARL04_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(CARL04_TFTable, vertex.label = V(CARL04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
CARL04_TF.clusterCoef <- transitivity(CARL04_TFTable, type="global") #cluster coefficient
CARL04_TF.degreeCent <- centralization.degree(CARL04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_TFftn <- as.network.matrix(CARL04_TFft)
CARL04_TF.netDensity <- network.density(CARL04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_TF.entropy <- entropy(CARL04_TFft) #entropy

CARL04_TF.netMx <- cbind(CARL04_TF.netMx, CARL04_TF.clusterCoef, CARL04_TF.degreeCent$centralization,
                         CARL04_TF.netDensity, CARL04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
CARL04_SAMg2 <- data.frame(CARL04_SAM)
CARL04_SAMg2 <- CARL04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_SAMg2$player1
player2vector <- CARL04_SAMg2$player2
CARL04_SAMg3 <- CARL04_SAMg2
CARL04_SAMg3$p1inp2vec <- is.element(CARL04_SAMg3$player1, player2vector)
CARL04_SAMg3$p2inp1vec <- is.element(CARL04_SAMg3$player2, player1vector)

addPlayer1 <- CARL04_SAMg3[ which(CARL04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_SAMg3[ which(CARL04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_SAMg2 <- rbind(CARL04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
CARL04_SAMft <- ftable(CARL04_SAMg2$player1, CARL04_SAMg2$player2)
CARL04_SAMft2 <- as.matrix(CARL04_SAMft)
numRows <- nrow(CARL04_SAMft2)
numCols <- ncol(CARL04_SAMft2)
CARL04_SAMft3 <- CARL04_SAMft2[c(2:numRows) , c(2:numCols)]
CARL04_SAMTable <- graph.adjacency(CARL04_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(CARL04_SAMTable, vertex.label = V(CARL04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
CARL04_SAM.clusterCoef <- transitivity(CARL04_SAMTable, type="global") #cluster coefficient
CARL04_SAM.degreeCent <- centralization.degree(CARL04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_SAMftn <- as.network.matrix(CARL04_SAMft)
CARL04_SAM.netDensity <- network.density(CARL04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_SAM.entropy <- entropy(CARL04_SAMft) #entropy

CARL04_SAM.netMx <- cbind(CARL04_SAM.netMx, CARL04_SAM.clusterCoef, CARL04_SAM.degreeCent$centralization,
                          CARL04_SAM.netDensity, CARL04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
CARL04_TAMg2 <- data.frame(CARL04_TAM)
CARL04_TAMg2 <- CARL04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_TAMg2$player1
player2vector <- CARL04_TAMg2$player2
CARL04_TAMg3 <- CARL04_TAMg2
CARL04_TAMg3$p1inp2vec <- is.element(CARL04_TAMg3$player1, player2vector)
CARL04_TAMg3$p2inp1vec <- is.element(CARL04_TAMg3$player2, player1vector)

addPlayer1 <- CARL04_TAMg3[ which(CARL04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_TAMg3[ which(CARL04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_TAMg2 <- rbind(CARL04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
CARL04_TAMft <- ftable(CARL04_TAMg2$player1, CARL04_TAMg2$player2)
CARL04_TAMft2 <- as.matrix(CARL04_TAMft)
numRows <- nrow(CARL04_TAMft2)
numCols <- ncol(CARL04_TAMft2)
CARL04_TAMft3 <- CARL04_TAMft2[c(2:numRows) , c(2:numCols)]
CARL04_TAMTable <- graph.adjacency(CARL04_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(CARL04_TAMTable, vertex.label = V(CARL04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
CARL04_TAM.clusterCoef <- transitivity(CARL04_TAMTable, type="global") #cluster coefficient
CARL04_TAM.degreeCent <- centralization.degree(CARL04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_TAMftn <- as.network.matrix(CARL04_TAMft)
CARL04_TAM.netDensity <- network.density(CARL04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_TAM.entropy <- entropy(CARL04_TAMft) #entropy

CARL04_TAM.netMx <- cbind(CARL04_TAM.netMx, CARL04_TAM.clusterCoef, CARL04_TAM.degreeCent$centralization,
                          CARL04_TAM.netDensity, CARL04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
CARL04_SDMg2 <- data.frame(CARL04_SDM)
CARL04_SDMg2 <- CARL04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_SDMg2$player1
player2vector <- CARL04_SDMg2$player2
CARL04_SDMg3 <- CARL04_SDMg2
CARL04_SDMg3$p1inp2vec <- is.element(CARL04_SDMg3$player1, player2vector)
CARL04_SDMg3$p2inp1vec <- is.element(CARL04_SDMg3$player2, player1vector)

addPlayer1 <- CARL04_SDMg3[ which(CARL04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_SDMg3[ which(CARL04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_SDMg2 <- rbind(CARL04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
CARL04_SDMft <- ftable(CARL04_SDMg2$player1, CARL04_SDMg2$player2)
CARL04_SDMft2 <- as.matrix(CARL04_SDMft)
numRows <- nrow(CARL04_SDMft2)
numCols <- ncol(CARL04_SDMft2)
CARL04_SDMft3 <- CARL04_SDMft2[c(2:numRows) , c(2:numCols)]
CARL04_SDMTable <- graph.adjacency(CARL04_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(CARL04_SDMTable, vertex.label = V(CARL04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
CARL04_SDM.clusterCoef <- transitivity(CARL04_SDMTable, type="global") #cluster coefficient
CARL04_SDM.degreeCent <- centralization.degree(CARL04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_SDMftn <- as.network.matrix(CARL04_SDMft)
CARL04_SDM.netDensity <- network.density(CARL04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_SDM.entropy <- entropy(CARL04_SDMft) #entropy

CARL04_SDM.netMx <- cbind(CARL04_SDM.netMx, CARL04_SDM.clusterCoef, CARL04_SDM.degreeCent$centralization,
                          CARL04_SDM.netDensity, CARL04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
CARL04_TDMg2 <- data.frame(CARL04_TDM)
CARL04_TDMg2 <- CARL04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_TDMg2$player1
player2vector <- CARL04_TDMg2$player2
CARL04_TDMg3 <- CARL04_TDMg2
CARL04_TDMg3$p1inp2vec <- is.element(CARL04_TDMg3$player1, player2vector)
CARL04_TDMg3$p2inp1vec <- is.element(CARL04_TDMg3$player2, player1vector)

addPlayer1 <- CARL04_TDMg3[ which(CARL04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_TDMg3[ which(CARL04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_TDMg2 <- rbind(CARL04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
CARL04_TDMft <- ftable(CARL04_TDMg2$player1, CARL04_TDMg2$player2)
CARL04_TDMft2 <- as.matrix(CARL04_TDMft)
numRows <- nrow(CARL04_TDMft2)
numCols <- ncol(CARL04_TDMft2)
CARL04_TDMft3 <- CARL04_TDMft2[c(2:numRows) , c(2:numCols)]
CARL04_TDMTable <- graph.adjacency(CARL04_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(CARL04_TDMTable, vertex.label = V(CARL04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
CARL04_TDM.clusterCoef <- transitivity(CARL04_TDMTable, type="global") #cluster coefficient
CARL04_TDM.degreeCent <- centralization.degree(CARL04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_TDMftn <- as.network.matrix(CARL04_TDMft)
CARL04_TDM.netDensity <- network.density(CARL04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_TDM.entropy <- entropy(CARL04_TDMft) #entropy

CARL04_TDM.netMx <- cbind(CARL04_TDM.netMx, CARL04_TDM.clusterCoef, CARL04_TDM.degreeCent$centralization,
                          CARL04_TDM.netDensity, CARL04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
CARL04_SDg2 <- data.frame(CARL04_SD)
CARL04_SDg2 <- CARL04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_SDg2$player1
player2vector <- CARL04_SDg2$player2
CARL04_SDg3 <- CARL04_SDg2
CARL04_SDg3$p1inp2vec <- is.element(CARL04_SDg3$player1, player2vector)
CARL04_SDg3$p2inp1vec <- is.element(CARL04_SDg3$player2, player1vector)

addPlayer1 <- CARL04_SDg3[ which(CARL04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_SDg3[ which(CARL04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_SDg2 <- rbind(CARL04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
CARL04_SDft <- ftable(CARL04_SDg2$player1, CARL04_SDg2$player2)
CARL04_SDft2 <- as.matrix(CARL04_SDft)
numRows <- nrow(CARL04_SDft2)
numCols <- ncol(CARL04_SDft2)
CARL04_SDft3 <- CARL04_SDft2[c(2:numRows) , c(2:numCols)]
CARL04_SDTable <- graph.adjacency(CARL04_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(CARL04_SDTable, vertex.label = V(CARL04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
CARL04_SD.clusterCoef <- transitivity(CARL04_SDTable, type="global") #cluster coefficient
CARL04_SD.degreeCent <- centralization.degree(CARL04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_SDftn <- as.network.matrix(CARL04_SDft)
CARL04_SD.netDensity <- network.density(CARL04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_SD.entropy <- entropy(CARL04_SDft) #entropy

CARL04_SD.netMx <- cbind(CARL04_SD.netMx, CARL04_SD.clusterCoef, CARL04_SD.degreeCent$centralization,
                         CARL04_SD.netDensity, CARL04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
CARL04_TDg2 <- data.frame(CARL04_TD)
CARL04_TDg2 <- CARL04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_TDg2$player1
player2vector <- CARL04_TDg2$player2
CARL04_TDg3 <- CARL04_TDg2
CARL04_TDg3$p1inp2vec <- is.element(CARL04_TDg3$player1, player2vector)
CARL04_TDg3$p2inp1vec <- is.element(CARL04_TDg3$player2, player1vector)

addPlayer1 <- CARL04_TDg3[ which(CARL04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_TDg3[ which(CARL04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_TDg2 <- rbind(CARL04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
CARL04_TDft <- ftable(CARL04_TDg2$player1, CARL04_TDg2$player2)
CARL04_TDft2 <- as.matrix(CARL04_TDft)
numRows <- nrow(CARL04_TDft2)
numCols <- ncol(CARL04_TDft2)
CARL04_TDft3 <- CARL04_TDft2[c(2:numRows) , c(2:numCols)]
CARL04_TDTable <- graph.adjacency(CARL04_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(CARL04_TDTable, vertex.label = V(CARL04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
CARL04_TD.clusterCoef <- transitivity(CARL04_TDTable, type="global") #cluster coefficient
CARL04_TD.degreeCent <- centralization.degree(CARL04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_TDftn <- as.network.matrix(CARL04_TDft)
CARL04_TD.netDensity <- network.density(CARL04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_TD.entropy <- entropy(CARL04_TDft) #entropy

CARL04_TD.netMx <- cbind(CARL04_TD.netMx, CARL04_TD.clusterCoef, CARL04_TD.degreeCent$centralization,
                         CARL04_TD.netDensity, CARL04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
CARL04_QTg2 <- data.frame(CARL04_QT)
CARL04_QTg2 <- CARL04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL04_QTg2$player1
player2vector <- CARL04_QTg2$player2
CARL04_QTg3 <- CARL04_QTg2
CARL04_QTg3$p1inp2vec <- is.element(CARL04_QTg3$player1, player2vector)
CARL04_QTg3$p2inp1vec <- is.element(CARL04_QTg3$player2, player1vector)

addPlayer1 <- CARL04_QTg3[ which(CARL04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL04_QTg3[ which(CARL04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL04_QTg2 <- rbind(CARL04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
CARL04_QTft <- ftable(CARL04_QTg2$player1, CARL04_QTg2$player2)
CARL04_QTft2 <- as.matrix(CARL04_QTft)
numRows <- nrow(CARL04_QTft2)
numCols <- ncol(CARL04_QTft2)
CARL04_QTft3 <- CARL04_QTft2[c(2:numRows) , c(2:numCols)]
CARL04_QTTable <- graph.adjacency(CARL04_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(CARL04_QTTable, vertex.label = V(CARL04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
CARL04_QT.clusterCoef <- transitivity(CARL04_QTTable, type="global") #cluster coefficient
CARL04_QT.degreeCent <- centralization.degree(CARL04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL04_QTftn <- as.network.matrix(CARL04_QTft)
CARL04_QT.netDensity <- network.density(CARL04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL04_QT.entropy <- entropy(CARL04_QTft) #entropy

CARL04_QT.netMx <- cbind(CARL04_QT.netMx, CARL04_QT.clusterCoef, CARL04_QT.degreeCent$centralization,
                         CARL04_QT.netDensity, CARL04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL04_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "COLL"
KIoutcome = "Goal_F"
COLL04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
COLL04_Gg2 <- data.frame(COLL04_G)
COLL04_Gg2 <- COLL04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_Gg2$player1
player2vector <- COLL04_Gg2$player2
COLL04_Gg3 <- COLL04_Gg2
COLL04_Gg3$p1inp2vec <- is.element(COLL04_Gg3$player1, player2vector)
COLL04_Gg3$p2inp1vec <- is.element(COLL04_Gg3$player2, player1vector)

addPlayer1 <- COLL04_Gg3[ which(COLL04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_Gg3[ which(COLL04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_Gg2 <- rbind(COLL04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
COLL04_Gft <- ftable(COLL04_Gg2$player1, COLL04_Gg2$player2)
COLL04_Gft2 <- as.matrix(COLL04_Gft)
numRows <- nrow(COLL04_Gft2)
numCols <- ncol(COLL04_Gft2)
COLL04_Gft3 <- COLL04_Gft2[c(2:numRows) , c(2:numCols)]
COLL04_GTable <- graph.adjacency(COLL04_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(COLL04_GTable, vertex.label = V(COLL04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
COLL04_G.clusterCoef <- transitivity(COLL04_GTable, type="global") #cluster coefficient
COLL04_G.degreeCent <- centralization.degree(COLL04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_Gftn <- as.network.matrix(COLL04_Gft)
COLL04_G.netDensity <- network.density(COLL04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_G.entropy <- entropy(COLL04_Gft) #entropy

COLL04_G.netMx <- cbind(COLL04_G.netMx, COLL04_G.clusterCoef, COLL04_G.degreeCent$centralization,
                        COLL04_G.netDensity, COLL04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************

round = 4
teamName = "COLL"
KIoutcome = "Behind_F"
COLL04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
COLL04_Bg2 <- data.frame(COLL04_B)
COLL04_Bg2 <- COLL04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_Bg2$player1
player2vector <- COLL04_Bg2$player2
COLL04_Bg3 <- COLL04_Bg2
COLL04_Bg3$p1inp2vec <- is.element(COLL04_Bg3$player1, player2vector)
COLL04_Bg3$p2inp1vec <- is.element(COLL04_Bg3$player2, player1vector)

addPlayer1 <- COLL04_Bg3[ which(COLL04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_Bg3[ which(COLL04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_Bg2 <- rbind(COLL04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
COLL04_Bft <- ftable(COLL04_Bg2$player1, COLL04_Bg2$player2)
COLL04_Bft2 <- as.matrix(COLL04_Bft)
numRows <- nrow(COLL04_Bft2)
numCols <- ncol(COLL04_Bft2)
COLL04_Bft3 <- COLL04_Bft2[c(2:numRows) , c(2:numCols)]
COLL04_BTable <- graph.adjacency(COLL04_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(COLL04_BTable, vertex.label = V(COLL04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
COLL04_B.clusterCoef <- transitivity(COLL04_BTable, type="global") #cluster coefficient
COLL04_B.degreeCent <- centralization.degree(COLL04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_Bftn <- as.network.matrix(COLL04_Bft)
COLL04_B.netDensity <- network.density(COLL04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_B.entropy <- entropy(COLL04_Bft) #entropy

COLL04_B.netMx <- cbind(COLL04_B.netMx, COLL04_B.clusterCoef, COLL04_B.degreeCent$centralization,
                        COLL04_B.netDensity, COLL04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************

round = 4
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
COLL04_SFg2 <- data.frame(COLL04_SF)
COLL04_SFg2 <- COLL04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_SFg2$player1
player2vector <- COLL04_SFg2$player2
COLL04_SFg3 <- COLL04_SFg2
COLL04_SFg3$p1inp2vec <- is.element(COLL04_SFg3$player1, player2vector)
COLL04_SFg3$p2inp1vec <- is.element(COLL04_SFg3$player2, player1vector)

addPlayer1 <- COLL04_SFg3[ which(COLL04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_SFg3[ which(COLL04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_SFg2 <- rbind(COLL04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
COLL04_SFft <- ftable(COLL04_SFg2$player1, COLL04_SFg2$player2)
COLL04_SFft2 <- as.matrix(COLL04_SFft)
numRows <- nrow(COLL04_SFft2)
numCols <- ncol(COLL04_SFft2)
COLL04_SFft3 <- COLL04_SFft2[c(2:numRows) , c(2:numCols)]
COLL04_SFTable <- graph.adjacency(COLL04_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(COLL04_SFTable, vertex.label = V(COLL04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
COLL04_SF.clusterCoef <- transitivity(COLL04_SFTable, type="global") #cluster coefficient
COLL04_SF.degreeCent <- centralization.degree(COLL04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_SFftn <- as.network.matrix(COLL04_SFft)
COLL04_SF.netDensity <- network.density(COLL04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_SF.entropy <- entropy(COLL04_SFft) #entropy

COLL04_SF.netMx <- cbind(COLL04_SF.netMx, COLL04_SF.clusterCoef, COLL04_SF.degreeCent$centralization,
                         COLL04_SF.netDensity, COLL04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
COLL04_TFg2 <- data.frame(COLL04_TF)
COLL04_TFg2 <- COLL04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_TFg2$player1
player2vector <- COLL04_TFg2$player2
COLL04_TFg3 <- COLL04_TFg2
COLL04_TFg3$p1inp2vec <- is.element(COLL04_TFg3$player1, player2vector)
COLL04_TFg3$p2inp1vec <- is.element(COLL04_TFg3$player2, player1vector)

addPlayer1 <- COLL04_TFg3[ which(COLL04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL04_TFg3[ which(COLL04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_TFg2 <- rbind(COLL04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
COLL04_TFft <- ftable(COLL04_TFg2$player1, COLL04_TFg2$player2)
COLL04_TFft2 <- as.matrix(COLL04_TFft)
numRows <- nrow(COLL04_TFft2)
numCols <- ncol(COLL04_TFft2)
COLL04_TFft3 <- COLL04_TFft2[c(2:numRows) , c(2:numCols)]
COLL04_TFTable <- graph.adjacency(COLL04_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(COLL04_TFTable, vertex.label = V(COLL04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
COLL04_TF.clusterCoef <- transitivity(COLL04_TFTable, type="global") #cluster coefficient
COLL04_TF.degreeCent <- centralization.degree(COLL04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_TFftn <- as.network.matrix(COLL04_TFft)
COLL04_TF.netDensity <- network.density(COLL04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_TF.entropy <- entropy(COLL04_TFft) #entropy

COLL04_TF.netMx <- cbind(COLL04_TF.netMx, COLL04_TF.clusterCoef, COLL04_TF.degreeCent$centralization,
                         COLL04_TF.netDensity, COLL04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
COLL04_SAMg2 <- data.frame(COLL04_SAM)
COLL04_SAMg2 <- COLL04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_SAMg2$player1
player2vector <- COLL04_SAMg2$player2
COLL04_SAMg3 <- COLL04_SAMg2
COLL04_SAMg3$p1inp2vec <- is.element(COLL04_SAMg3$player1, player2vector)
COLL04_SAMg3$p2inp1vec <- is.element(COLL04_SAMg3$player2, player1vector)

addPlayer1 <- COLL04_SAMg3[ which(COLL04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_SAMg3[ which(COLL04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_SAMg2 <- rbind(COLL04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
COLL04_SAMft <- ftable(COLL04_SAMg2$player1, COLL04_SAMg2$player2)
COLL04_SAMft2 <- as.matrix(COLL04_SAMft)
numRows <- nrow(COLL04_SAMft2)
numCols <- ncol(COLL04_SAMft2)
COLL04_SAMft3 <- COLL04_SAMft2[c(2:numRows) , c(2:numCols)]
COLL04_SAMTable <- graph.adjacency(COLL04_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(COLL04_SAMTable, vertex.label = V(COLL04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
COLL04_SAM.clusterCoef <- transitivity(COLL04_SAMTable, type="global") #cluster coefficient
COLL04_SAM.degreeCent <- centralization.degree(COLL04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_SAMftn <- as.network.matrix(COLL04_SAMft)
COLL04_SAM.netDensity <- network.density(COLL04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_SAM.entropy <- entropy(COLL04_SAMft) #entropy

COLL04_SAM.netMx <- cbind(COLL04_SAM.netMx, COLL04_SAM.clusterCoef, COLL04_SAM.degreeCent$centralization,
                          COLL04_SAM.netDensity, COLL04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
COLL04_TAMg2 <- data.frame(COLL04_TAM)
COLL04_TAMg2 <- COLL04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_TAMg2$player1
player2vector <- COLL04_TAMg2$player2
COLL04_TAMg3 <- COLL04_TAMg2
COLL04_TAMg3$p1inp2vec <- is.element(COLL04_TAMg3$player1, player2vector)
COLL04_TAMg3$p2inp1vec <- is.element(COLL04_TAMg3$player2, player1vector)

addPlayer1 <- COLL04_TAMg3[ which(COLL04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_TAMg3[ which(COLL04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_TAMg2 <- rbind(COLL04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
COLL04_TAMft <- ftable(COLL04_TAMg2$player1, COLL04_TAMg2$player2)
COLL04_TAMft2 <- as.matrix(COLL04_TAMft)
numRows <- nrow(COLL04_TAMft2)
numCols <- ncol(COLL04_TAMft2)
COLL04_TAMft3 <- COLL04_TAMft2[c(2:numRows) , c(2:numCols)]
COLL04_TAMTable <- graph.adjacency(COLL04_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(COLL04_TAMTable, vertex.label = V(COLL04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
COLL04_TAM.clusterCoef <- transitivity(COLL04_TAMTable, type="global") #cluster coefficient
COLL04_TAM.degreeCent <- centralization.degree(COLL04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_TAMftn <- as.network.matrix(COLL04_TAMft)
COLL04_TAM.netDensity <- network.density(COLL04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_TAM.entropy <- entropy(COLL04_TAMft) #entropy

COLL04_TAM.netMx <- cbind(COLL04_TAM.netMx, COLL04_TAM.clusterCoef, COLL04_TAM.degreeCent$centralization,
                          COLL04_TAM.netDensity, COLL04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************
#NA

round = 4
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
COLL04_SDMg2 <- data.frame(COLL04_SDM)
COLL04_SDMg2 <- COLL04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_SDMg2$player1
player2vector <- COLL04_SDMg2$player2
COLL04_SDMg3 <- COLL04_SDMg2
COLL04_SDMg3$p1inp2vec <- is.element(COLL04_SDMg3$player1, player2vector)
COLL04_SDMg3$p2inp1vec <- is.element(COLL04_SDMg3$player2, player1vector)

addPlayer1 <- COLL04_SDMg3[ which(COLL04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_SDMg3[ which(COLL04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_SDMg2 <- rbind(COLL04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
COLL04_SDMft <- ftable(COLL04_SDMg2$player1, COLL04_SDMg2$player2)
COLL04_SDMft2 <- as.matrix(COLL04_SDMft)
numRows <- nrow(COLL04_SDMft2)
numCols <- ncol(COLL04_SDMft2)
COLL04_SDMft3 <- COLL04_SDMft2[c(2:numRows) , c(2:numCols)]
COLL04_SDMTable <- graph.adjacency(COLL04_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(COLL04_SDMTable, vertex.label = V(COLL04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
COLL04_SDM.clusterCoef <- transitivity(COLL04_SDMTable, type="global") #cluster coefficient
COLL04_SDM.degreeCent <- centralization.degree(COLL04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_SDMftn <- as.network.matrix(COLL04_SDMft)
COLL04_SDM.netDensity <- network.density(COLL04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_SDM.entropy <- entropy(COLL04_SDMft) #entropy

COLL04_SDM.netMx <- cbind(COLL04_SDM.netMx, COLL04_SDM.clusterCoef, COLL04_SDM.degreeCent$centralization,
                          COLL04_SDM.netDensity, COLL04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
COLL04_TDMg2 <- data.frame(COLL04_TDM)
COLL04_TDMg2 <- COLL04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_TDMg2$player1
player2vector <- COLL04_TDMg2$player2
COLL04_TDMg3 <- COLL04_TDMg2
COLL04_TDMg3$p1inp2vec <- is.element(COLL04_TDMg3$player1, player2vector)
COLL04_TDMg3$p2inp1vec <- is.element(COLL04_TDMg3$player2, player1vector)

addPlayer1 <- COLL04_TDMg3[ which(COLL04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_TDMg3[ which(COLL04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_TDMg2 <- rbind(COLL04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
COLL04_TDMft <- ftable(COLL04_TDMg2$player1, COLL04_TDMg2$player2)
COLL04_TDMft2 <- as.matrix(COLL04_TDMft)
numRows <- nrow(COLL04_TDMft2)
numCols <- ncol(COLL04_TDMft2)
COLL04_TDMft3 <- COLL04_TDMft2[c(2:numRows) , c(2:numCols)]
COLL04_TDMTable <- graph.adjacency(COLL04_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(COLL04_TDMTable, vertex.label = V(COLL04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
COLL04_TDM.clusterCoef <- transitivity(COLL04_TDMTable, type="global") #cluster coefficient
COLL04_TDM.degreeCent <- centralization.degree(COLL04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_TDMftn <- as.network.matrix(COLL04_TDMft)
COLL04_TDM.netDensity <- network.density(COLL04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_TDM.entropy <- entropy(COLL04_TDMft) #entropy

COLL04_TDM.netMx <- cbind(COLL04_TDM.netMx, COLL04_TDM.clusterCoef, COLL04_TDM.degreeCent$centralization,
                          COLL04_TDM.netDensity, COLL04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
COLL04_SDg2 <- data.frame(COLL04_SD)
COLL04_SDg2 <- COLL04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_SDg2$player1
player2vector <- COLL04_SDg2$player2
COLL04_SDg3 <- COLL04_SDg2
COLL04_SDg3$p1inp2vec <- is.element(COLL04_SDg3$player1, player2vector)
COLL04_SDg3$p2inp1vec <- is.element(COLL04_SDg3$player2, player1vector)

addPlayer1 <- COLL04_SDg3[ which(COLL04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_SDg3[ which(COLL04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_SDg2 <- rbind(COLL04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
COLL04_SDft <- ftable(COLL04_SDg2$player1, COLL04_SDg2$player2)
COLL04_SDft2 <- as.matrix(COLL04_SDft)
numRows <- nrow(COLL04_SDft2)
numCols <- ncol(COLL04_SDft2)
COLL04_SDft3 <- COLL04_SDft2[c(2:numRows) , c(2:numCols)]
COLL04_SDTable <- graph.adjacency(COLL04_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(COLL04_SDTable, vertex.label = V(COLL04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
COLL04_SD.clusterCoef <- transitivity(COLL04_SDTable, type="global") #cluster coefficient
COLL04_SD.degreeCent <- centralization.degree(COLL04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_SDftn <- as.network.matrix(COLL04_SDft)
COLL04_SD.netDensity <- network.density(COLL04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_SD.entropy <- entropy(COLL04_SDft) #entropy

COLL04_SD.netMx <- cbind(COLL04_SD.netMx, COLL04_SD.clusterCoef, COLL04_SD.degreeCent$centralization,
                         COLL04_SD.netDensity, COLL04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
COLL04_TDg2 <- data.frame(COLL04_TD)
COLL04_TDg2 <- COLL04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_TDg2$player1
player2vector <- COLL04_TDg2$player2
COLL04_TDg3 <- COLL04_TDg2
COLL04_TDg3$p1inp2vec <- is.element(COLL04_TDg3$player1, player2vector)
COLL04_TDg3$p2inp1vec <- is.element(COLL04_TDg3$player2, player1vector)

addPlayer1 <- COLL04_TDg3[ which(COLL04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_TDg3[ which(COLL04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_TDg2 <- rbind(COLL04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
COLL04_TDft <- ftable(COLL04_TDg2$player1, COLL04_TDg2$player2)
COLL04_TDft2 <- as.matrix(COLL04_TDft)
numRows <- nrow(COLL04_TDft2)
numCols <- ncol(COLL04_TDft2)
COLL04_TDft3 <- COLL04_TDft2[c(2:numRows) , c(2:numCols)]
COLL04_TDTable <- graph.adjacency(COLL04_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(COLL04_TDTable, vertex.label = V(COLL04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
COLL04_TD.clusterCoef <- transitivity(COLL04_TDTable, type="global") #cluster coefficient
COLL04_TD.degreeCent <- centralization.degree(COLL04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_TDftn <- as.network.matrix(COLL04_TDft)
COLL04_TD.netDensity <- network.density(COLL04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_TD.entropy <- entropy(COLL04_TDft) #entropy

COLL04_TD.netMx <- cbind(COLL04_TD.netMx, COLL04_TD.clusterCoef, COLL04_TD.degreeCent$centralization,
                         COLL04_TD.netDensity, COLL04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
COLL04_QTg2 <- data.frame(COLL04_QT)
COLL04_QTg2 <- COLL04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL04_QTg2$player1
player2vector <- COLL04_QTg2$player2
COLL04_QTg3 <- COLL04_QTg2
COLL04_QTg3$p1inp2vec <- is.element(COLL04_QTg3$player1, player2vector)
COLL04_QTg3$p2inp1vec <- is.element(COLL04_QTg3$player2, player1vector)

addPlayer1 <- COLL04_QTg3[ which(COLL04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL04_QTg3[ which(COLL04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL04_QTg2 <- rbind(COLL04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
COLL04_QTft <- ftable(COLL04_QTg2$player1, COLL04_QTg2$player2)
COLL04_QTft2 <- as.matrix(COLL04_QTft)
numRows <- nrow(COLL04_QTft2)
numCols <- ncol(COLL04_QTft2)
COLL04_QTft3 <- COLL04_QTft2[c(2:numRows) , c(2:numCols)]
COLL04_QTTable <- graph.adjacency(COLL04_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(COLL04_QTTable, vertex.label = V(COLL04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
COLL04_QT.clusterCoef <- transitivity(COLL04_QTTable, type="global") #cluster coefficient
COLL04_QT.degreeCent <- centralization.degree(COLL04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL04_QTftn <- as.network.matrix(COLL04_QTft)
COLL04_QT.netDensity <- network.density(COLL04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL04_QT.entropy <- entropy(COLL04_QTft) #entropy

COLL04_QT.netMx <- cbind(COLL04_QT.netMx, COLL04_QT.clusterCoef, COLL04_QT.degreeCent$centralization,
                         COLL04_QT.netDensity, COLL04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL04_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "ESS"
KIoutcome = "Goal_F"
ESS04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
ESS04_Gg2 <- data.frame(ESS04_G)
ESS04_Gg2 <- ESS04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_Gg2$player1
player2vector <- ESS04_Gg2$player2
ESS04_Gg3 <- ESS04_Gg2
ESS04_Gg3$p1inp2vec <- is.element(ESS04_Gg3$player1, player2vector)
ESS04_Gg3$p2inp1vec <- is.element(ESS04_Gg3$player2, player1vector)

addPlayer1 <- ESS04_Gg3[ which(ESS04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_Gg3[ which(ESS04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_Gg2 <- rbind(ESS04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
ESS04_Gft <- ftable(ESS04_Gg2$player1, ESS04_Gg2$player2)
ESS04_Gft2 <- as.matrix(ESS04_Gft)
numRows <- nrow(ESS04_Gft2)
numCols <- ncol(ESS04_Gft2)
ESS04_Gft3 <- ESS04_Gft2[c(2:numRows) , c(2:numCols)]
ESS04_GTable <- graph.adjacency(ESS04_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(ESS04_GTable, vertex.label = V(ESS04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
ESS04_G.clusterCoef <- transitivity(ESS04_GTable, type="global") #cluster coefficient
ESS04_G.degreeCent <- centralization.degree(ESS04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_Gftn <- as.network.matrix(ESS04_Gft)
ESS04_G.netDensity <- network.density(ESS04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_G.entropy <- entropy(ESS04_Gft) #entropy

ESS04_G.netMx <- cbind(ESS04_G.netMx, ESS04_G.clusterCoef, ESS04_G.degreeCent$centralization,
                       ESS04_G.netDensity, ESS04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "ESS"
KIoutcome = "Behind_F"
ESS04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
ESS04_Bg2 <- data.frame(ESS04_B)
ESS04_Bg2 <- ESS04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_Bg2$player1
player2vector <- ESS04_Bg2$player2
ESS04_Bg3 <- ESS04_Bg2
ESS04_Bg3$p1inp2vec <- is.element(ESS04_Bg3$player1, player2vector)
ESS04_Bg3$p2inp1vec <- is.element(ESS04_Bg3$player2, player1vector)

addPlayer1 <- ESS04_Bg3[ which(ESS04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_Bg3[ which(ESS04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_Bg2 <- rbind(ESS04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
ESS04_Bft <- ftable(ESS04_Bg2$player1, ESS04_Bg2$player2)
ESS04_Bft2 <- as.matrix(ESS04_Bft)
numRows <- nrow(ESS04_Bft2)
numCols <- ncol(ESS04_Bft2)
ESS04_Bft3 <- ESS04_Bft2[c(2:numRows) , c(2:numCols)]
ESS04_BTable <- graph.adjacency(ESS04_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(ESS04_BTable, vertex.label = V(ESS04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
ESS04_B.clusterCoef <- transitivity(ESS04_BTable, type="global") #cluster coefficient
ESS04_B.degreeCent <- centralization.degree(ESS04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_Bftn <- as.network.matrix(ESS04_Bft)
ESS04_B.netDensity <- network.density(ESS04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_B.entropy <- entropy(ESS04_Bft) #entropy

ESS04_B.netMx <- cbind(ESS04_B.netMx, ESS04_B.clusterCoef, ESS04_B.degreeCent$centralization,
                       ESS04_B.netDensity, ESS04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
ESS04_SFg2 <- data.frame(ESS04_SF)
ESS04_SFg2 <- ESS04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_SFg2$player1
player2vector <- ESS04_SFg2$player2
ESS04_SFg3 <- ESS04_SFg2
ESS04_SFg3$p1inp2vec <- is.element(ESS04_SFg3$player1, player2vector)
ESS04_SFg3$p2inp1vec <- is.element(ESS04_SFg3$player2, player1vector)

addPlayer1 <- ESS04_SFg3[ which(ESS04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_SFg3[ which(ESS04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_SFg2 <- rbind(ESS04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
ESS04_SFft <- ftable(ESS04_SFg2$player1, ESS04_SFg2$player2)
ESS04_SFft2 <- as.matrix(ESS04_SFft)
numRows <- nrow(ESS04_SFft2)
numCols <- ncol(ESS04_SFft2)
ESS04_SFft3 <- ESS04_SFft2[c(2:numRows) , c(2:numCols)]
ESS04_SFTable <- graph.adjacency(ESS04_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(ESS04_SFTable, vertex.label = V(ESS04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
ESS04_SF.clusterCoef <- transitivity(ESS04_SFTable, type="global") #cluster coefficient
ESS04_SF.degreeCent <- centralization.degree(ESS04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_SFftn <- as.network.matrix(ESS04_SFft)
ESS04_SF.netDensity <- network.density(ESS04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_SF.entropy <- entropy(ESS04_SFft) #entropy

ESS04_SF.netMx <- cbind(ESS04_SF.netMx, ESS04_SF.clusterCoef, ESS04_SF.degreeCent$centralization,
                        ESS04_SF.netDensity, ESS04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
ESS04_TFg2 <- data.frame(ESS04_TF)
ESS04_TFg2 <- ESS04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_TFg2$player1
player2vector <- ESS04_TFg2$player2
ESS04_TFg3 <- ESS04_TFg2
ESS04_TFg3$p1inp2vec <- is.element(ESS04_TFg3$player1, player2vector)
ESS04_TFg3$p2inp1vec <- is.element(ESS04_TFg3$player2, player1vector)

addPlayer1 <- ESS04_TFg3[ which(ESS04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_TFg3[ which(ESS04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_TFg2 <- rbind(ESS04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
ESS04_TFft <- ftable(ESS04_TFg2$player1, ESS04_TFg2$player2)
ESS04_TFft2 <- as.matrix(ESS04_TFft)
numRows <- nrow(ESS04_TFft2)
numCols <- ncol(ESS04_TFft2)
ESS04_TFft3 <- ESS04_TFft2[c(2:numRows) , c(2:numCols)]
ESS04_TFTable <- graph.adjacency(ESS04_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(ESS04_TFTable, vertex.label = V(ESS04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
ESS04_TF.clusterCoef <- transitivity(ESS04_TFTable, type="global") #cluster coefficient
ESS04_TF.degreeCent <- centralization.degree(ESS04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_TFftn <- as.network.matrix(ESS04_TFft)
ESS04_TF.netDensity <- network.density(ESS04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_TF.entropy <- entropy(ESS04_TFft) #entropy

ESS04_TF.netMx <- cbind(ESS04_TF.netMx, ESS04_TF.clusterCoef, ESS04_TF.degreeCent$centralization,
                        ESS04_TF.netDensity, ESS04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
ESS04_SAMg2 <- data.frame(ESS04_SAM)
ESS04_SAMg2 <- ESS04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_SAMg2$player1
player2vector <- ESS04_SAMg2$player2
ESS04_SAMg3 <- ESS04_SAMg2
ESS04_SAMg3$p1inp2vec <- is.element(ESS04_SAMg3$player1, player2vector)
ESS04_SAMg3$p2inp1vec <- is.element(ESS04_SAMg3$player2, player1vector)

addPlayer1 <- ESS04_SAMg3[ which(ESS04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_SAMg3[ which(ESS04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_SAMg2 <- rbind(ESS04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
ESS04_SAMft <- ftable(ESS04_SAMg2$player1, ESS04_SAMg2$player2)
ESS04_SAMft2 <- as.matrix(ESS04_SAMft)
numRows <- nrow(ESS04_SAMft2)
numCols <- ncol(ESS04_SAMft2)
ESS04_SAMft3 <- ESS04_SAMft2[c(2:numRows) , c(2:numCols)]
ESS04_SAMTable <- graph.adjacency(ESS04_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(ESS04_SAMTable, vertex.label = V(ESS04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
ESS04_SAM.clusterCoef <- transitivity(ESS04_SAMTable, type="global") #cluster coefficient
ESS04_SAM.degreeCent <- centralization.degree(ESS04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_SAMftn <- as.network.matrix(ESS04_SAMft)
ESS04_SAM.netDensity <- network.density(ESS04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_SAM.entropy <- entropy(ESS04_SAMft) #entropy

ESS04_SAM.netMx <- cbind(ESS04_SAM.netMx, ESS04_SAM.clusterCoef, ESS04_SAM.degreeCent$centralization,
                         ESS04_SAM.netDensity, ESS04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
ESS04_TAMg2 <- data.frame(ESS04_TAM)
ESS04_TAMg2 <- ESS04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_TAMg2$player1
player2vector <- ESS04_TAMg2$player2
ESS04_TAMg3 <- ESS04_TAMg2
ESS04_TAMg3$p1inp2vec <- is.element(ESS04_TAMg3$player1, player2vector)
ESS04_TAMg3$p2inp1vec <- is.element(ESS04_TAMg3$player2, player1vector)

addPlayer1 <- ESS04_TAMg3[ which(ESS04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_TAMg3[ which(ESS04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_TAMg2 <- rbind(ESS04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
ESS04_TAMft <- ftable(ESS04_TAMg2$player1, ESS04_TAMg2$player2)
ESS04_TAMft2 <- as.matrix(ESS04_TAMft)
numRows <- nrow(ESS04_TAMft2)
numCols <- ncol(ESS04_TAMft2)
ESS04_TAMft3 <- ESS04_TAMft2[c(2:numRows) , c(2:numCols)]
ESS04_TAMTable <- graph.adjacency(ESS04_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(ESS04_TAMTable, vertex.label = V(ESS04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
ESS04_TAM.clusterCoef <- transitivity(ESS04_TAMTable, type="global") #cluster coefficient
ESS04_TAM.degreeCent <- centralization.degree(ESS04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_TAMftn <- as.network.matrix(ESS04_TAMft)
ESS04_TAM.netDensity <- network.density(ESS04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_TAM.entropy <- entropy(ESS04_TAMft) #entropy

ESS04_TAM.netMx <- cbind(ESS04_TAM.netMx, ESS04_TAM.clusterCoef, ESS04_TAM.degreeCent$centralization,
                         ESS04_TAM.netDensity, ESS04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
ESS04_SDMg2 <- data.frame(ESS04_SDM)
ESS04_SDMg2 <- ESS04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_SDMg2$player1
player2vector <- ESS04_SDMg2$player2
ESS04_SDMg3 <- ESS04_SDMg2
ESS04_SDMg3$p1inp2vec <- is.element(ESS04_SDMg3$player1, player2vector)
ESS04_SDMg3$p2inp1vec <- is.element(ESS04_SDMg3$player2, player1vector)

addPlayer1 <- ESS04_SDMg3[ which(ESS04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_SDMg3[ which(ESS04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_SDMg2 <- rbind(ESS04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
ESS04_SDMft <- ftable(ESS04_SDMg2$player1, ESS04_SDMg2$player2)
ESS04_SDMft2 <- as.matrix(ESS04_SDMft)
numRows <- nrow(ESS04_SDMft2)
numCols <- ncol(ESS04_SDMft2)
ESS04_SDMft3 <- ESS04_SDMft2[c(2:numRows) , c(2:numCols)]
ESS04_SDMTable <- graph.adjacency(ESS04_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(ESS04_SDMTable, vertex.label = V(ESS04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
ESS04_SDM.clusterCoef <- transitivity(ESS04_SDMTable, type="global") #cluster coefficient
ESS04_SDM.degreeCent <- centralization.degree(ESS04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_SDMftn <- as.network.matrix(ESS04_SDMft)
ESS04_SDM.netDensity <- network.density(ESS04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_SDM.entropy <- entropy(ESS04_SDMft) #entropy

ESS04_SDM.netMx <- cbind(ESS04_SDM.netMx, ESS04_SDM.clusterCoef, ESS04_SDM.degreeCent$centralization,
                         ESS04_SDM.netDensity, ESS04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
ESS04_TDMg2 <- data.frame(ESS04_TDM)
ESS04_TDMg2 <- ESS04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_TDMg2$player1
player2vector <- ESS04_TDMg2$player2
ESS04_TDMg3 <- ESS04_TDMg2
ESS04_TDMg3$p1inp2vec <- is.element(ESS04_TDMg3$player1, player2vector)
ESS04_TDMg3$p2inp1vec <- is.element(ESS04_TDMg3$player2, player1vector)

addPlayer1 <- ESS04_TDMg3[ which(ESS04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_TDMg3[ which(ESS04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_TDMg2 <- rbind(ESS04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
ESS04_TDMft <- ftable(ESS04_TDMg2$player1, ESS04_TDMg2$player2)
ESS04_TDMft2 <- as.matrix(ESS04_TDMft)
numRows <- nrow(ESS04_TDMft2)
numCols <- ncol(ESS04_TDMft2)
ESS04_TDMft3 <- ESS04_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS04_TDMTable <- graph.adjacency(ESS04_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(ESS04_TDMTable, vertex.label = V(ESS04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
ESS04_TDM.clusterCoef <- transitivity(ESS04_TDMTable, type="global") #cluster coefficient
ESS04_TDM.degreeCent <- centralization.degree(ESS04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_TDMftn <- as.network.matrix(ESS04_TDMft)
ESS04_TDM.netDensity <- network.density(ESS04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_TDM.entropy <- entropy(ESS04_TDMft) #entropy

ESS04_TDM.netMx <- cbind(ESS04_TDM.netMx, ESS04_TDM.clusterCoef, ESS04_TDM.degreeCent$centralization,
                         ESS04_TDM.netDensity, ESS04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
ESS04_SDg2 <- data.frame(ESS04_SD)
ESS04_SDg2 <- ESS04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_SDg2$player1
player2vector <- ESS04_SDg2$player2
ESS04_SDg3 <- ESS04_SDg2
ESS04_SDg3$p1inp2vec <- is.element(ESS04_SDg3$player1, player2vector)
ESS04_SDg3$p2inp1vec <- is.element(ESS04_SDg3$player2, player1vector)

addPlayer1 <- ESS04_SDg3[ which(ESS04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_SDg3[ which(ESS04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_SDg2 <- rbind(ESS04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
ESS04_SDft <- ftable(ESS04_SDg2$player1, ESS04_SDg2$player2)
ESS04_SDft2 <- as.matrix(ESS04_SDft)
numRows <- nrow(ESS04_SDft2)
numCols <- ncol(ESS04_SDft2)
ESS04_SDft3 <- ESS04_SDft2[c(2:numRows) , c(2:numCols)]
ESS04_SDTable <- graph.adjacency(ESS04_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(ESS04_SDTable, vertex.label = V(ESS04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
ESS04_SD.clusterCoef <- transitivity(ESS04_SDTable, type="global") #cluster coefficient
ESS04_SD.degreeCent <- centralization.degree(ESS04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_SDftn <- as.network.matrix(ESS04_SDft)
ESS04_SD.netDensity <- network.density(ESS04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_SD.entropy <- entropy(ESS04_SDft) #entropy

ESS04_SD.netMx <- cbind(ESS04_SD.netMx, ESS04_SD.clusterCoef, ESS04_SD.degreeCent$centralization,
                        ESS04_SD.netDensity, ESS04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
ESS04_TDg2 <- data.frame(ESS04_TD)
ESS04_TDg2 <- ESS04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_TDg2$player1
player2vector <- ESS04_TDg2$player2
ESS04_TDg3 <- ESS04_TDg2
ESS04_TDg3$p1inp2vec <- is.element(ESS04_TDg3$player1, player2vector)
ESS04_TDg3$p2inp1vec <- is.element(ESS04_TDg3$player2, player1vector)

addPlayer1 <- ESS04_TDg3[ which(ESS04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_TDg3[ which(ESS04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_TDg2 <- rbind(ESS04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
ESS04_TDft <- ftable(ESS04_TDg2$player1, ESS04_TDg2$player2)
ESS04_TDft2 <- as.matrix(ESS04_TDft)
numRows <- nrow(ESS04_TDft2)
numCols <- ncol(ESS04_TDft2)
ESS04_TDft3 <- ESS04_TDft2[c(2:numRows) , c(2:numCols)]
ESS04_TDTable <- graph.adjacency(ESS04_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(ESS04_TDTable, vertex.label = V(ESS04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
ESS04_TD.clusterCoef <- transitivity(ESS04_TDTable, type="global") #cluster coefficient
ESS04_TD.degreeCent <- centralization.degree(ESS04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_TDftn <- as.network.matrix(ESS04_TDft)
ESS04_TD.netDensity <- network.density(ESS04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_TD.entropy <- entropy(ESS04_TDft) #entropy

ESS04_TD.netMx <- cbind(ESS04_TD.netMx, ESS04_TD.clusterCoef, ESS04_TD.degreeCent$centralization,
                        ESS04_TD.netDensity, ESS04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
ESS04_QTg2 <- data.frame(ESS04_QT)
ESS04_QTg2 <- ESS04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS04_QTg2$player1
player2vector <- ESS04_QTg2$player2
ESS04_QTg3 <- ESS04_QTg2
ESS04_QTg3$p1inp2vec <- is.element(ESS04_QTg3$player1, player2vector)
ESS04_QTg3$p2inp1vec <- is.element(ESS04_QTg3$player2, player1vector)

addPlayer1 <- ESS04_QTg3[ which(ESS04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS04_QTg3[ which(ESS04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS04_QTg2 <- rbind(ESS04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
ESS04_QTft <- ftable(ESS04_QTg2$player1, ESS04_QTg2$player2)
ESS04_QTft2 <- as.matrix(ESS04_QTft)
numRows <- nrow(ESS04_QTft2)
numCols <- ncol(ESS04_QTft2)
ESS04_QTft3 <- ESS04_QTft2[c(2:numRows) , c(2:numCols)]
ESS04_QTTable <- graph.adjacency(ESS04_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(ESS04_QTTable, vertex.label = V(ESS04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
ESS04_QT.clusterCoef <- transitivity(ESS04_QTTable, type="global") #cluster coefficient
ESS04_QT.degreeCent <- centralization.degree(ESS04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS04_QTftn <- as.network.matrix(ESS04_QTft)
ESS04_QT.netDensity <- network.density(ESS04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS04_QT.entropy <- entropy(ESS04_QTft) #entropy

ESS04_QT.netMx <- cbind(ESS04_QT.netMx, ESS04_QT.clusterCoef, ESS04_QT.degreeCent$centralization,
                        ESS04_QT.netDensity, ESS04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS04_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "FRE"
KIoutcome = "Goal_F"
FRE04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
FRE04_Gg2 <- data.frame(FRE04_G)
FRE04_Gg2 <- FRE04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_Gg2$player1
player2vector <- FRE04_Gg2$player2
FRE04_Gg3 <- FRE04_Gg2
FRE04_Gg3$p1inp2vec <- is.element(FRE04_Gg3$player1, player2vector)
FRE04_Gg3$p2inp1vec <- is.element(FRE04_Gg3$player2, player1vector)

addPlayer1 <- FRE04_Gg3[ which(FRE04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_Gg3[ which(FRE04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_Gg2 <- rbind(FRE04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
FRE04_Gft <- ftable(FRE04_Gg2$player1, FRE04_Gg2$player2)
FRE04_Gft2 <- as.matrix(FRE04_Gft)
numRows <- nrow(FRE04_Gft2)
numCols <- ncol(FRE04_Gft2)
FRE04_Gft3 <- FRE04_Gft2[c(2:numRows) , c(2:numCols)]
FRE04_GTable <- graph.adjacency(FRE04_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(FRE04_GTable, vertex.label = V(FRE04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
FRE04_G.clusterCoef <- transitivity(FRE04_GTable, type="global") #cluster coefficient
FRE04_G.degreeCent <- centralization.degree(FRE04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_Gftn <- as.network.matrix(FRE04_Gft)
FRE04_G.netDensity <- network.density(FRE04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_G.entropy <- entropy(FRE04_Gft) #entropy

FRE04_G.netMx <- cbind(FRE04_G.netMx, FRE04_G.clusterCoef, FRE04_G.degreeCent$centralization,
                       FRE04_G.netDensity, FRE04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "FRE"
KIoutcome = "Behind_F"
FRE04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
FRE04_Bg2 <- data.frame(FRE04_B)
FRE04_Bg2 <- FRE04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_Bg2$player1
player2vector <- FRE04_Bg2$player2
FRE04_Bg3 <- FRE04_Bg2
FRE04_Bg3$p1inp2vec <- is.element(FRE04_Bg3$player1, player2vector)
FRE04_Bg3$p2inp1vec <- is.element(FRE04_Bg3$player2, player1vector)

addPlayer1 <- FRE04_Bg3[ which(FRE04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_Bg3[ which(FRE04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_Bg2 <- rbind(FRE04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
FRE04_Bft <- ftable(FRE04_Bg2$player1, FRE04_Bg2$player2)
FRE04_Bft2 <- as.matrix(FRE04_Bft)
numRows <- nrow(FRE04_Bft2)
numCols <- ncol(FRE04_Bft2)
FRE04_Bft3 <- FRE04_Bft2[c(2:numRows) , c(2:numCols)]
FRE04_BTable <- graph.adjacency(FRE04_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(FRE04_BTable, vertex.label = V(FRE04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
FRE04_B.clusterCoef <- transitivity(FRE04_BTable, type="global") #cluster coefficient
FRE04_B.degreeCent <- centralization.degree(FRE04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_Bftn <- as.network.matrix(FRE04_Bft)
FRE04_B.netDensity <- network.density(FRE04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_B.entropy <- entropy(FRE04_Bft) #entropy

FRE04_B.netMx <- cbind(FRE04_B.netMx, FRE04_B.clusterCoef, FRE04_B.degreeCent$centralization,
                       FRE04_B.netDensity, FRE04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************

round = 4
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
FRE04_SFg2 <- data.frame(FRE04_SF)
FRE04_SFg2 <- FRE04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_SFg2$player1
player2vector <- FRE04_SFg2$player2
FRE04_SFg3 <- FRE04_SFg2
FRE04_SFg3$p1inp2vec <- is.element(FRE04_SFg3$player1, player2vector)
FRE04_SFg3$p2inp1vec <- is.element(FRE04_SFg3$player2, player1vector)

addPlayer1 <- FRE04_SFg3[ which(FRE04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_SFg3[ which(FRE04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_SFg2 <- rbind(FRE04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
FRE04_SFft <- ftable(FRE04_SFg2$player1, FRE04_SFg2$player2)
FRE04_SFft2 <- as.matrix(FRE04_SFft)
numRows <- nrow(FRE04_SFft2)
numCols <- ncol(FRE04_SFft2)
FRE04_SFft3 <- FRE04_SFft2[c(2:numRows) , c(2:numCols)]
FRE04_SFTable <- graph.adjacency(FRE04_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(FRE04_SFTable, vertex.label = V(FRE04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
FRE04_SF.clusterCoef <- transitivity(FRE04_SFTable, type="global") #cluster coefficient
FRE04_SF.degreeCent <- centralization.degree(FRE04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_SFftn <- as.network.matrix(FRE04_SFft)
FRE04_SF.netDensity <- network.density(FRE04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_SF.entropy <- entropy(FRE04_SFft) #entropy

FRE04_SF.netMx <- cbind(FRE04_SF.netMx, FRE04_SF.clusterCoef, FRE04_SF.degreeCent$centralization,
                        FRE04_SF.netDensity, FRE04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
FRE04_TFg2 <- data.frame(FRE04_TF)
FRE04_TFg2 <- FRE04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_TFg2$player1
player2vector <- FRE04_TFg2$player2
FRE04_TFg3 <- FRE04_TFg2
FRE04_TFg3$p1inp2vec <- is.element(FRE04_TFg3$player1, player2vector)
FRE04_TFg3$p2inp1vec <- is.element(FRE04_TFg3$player2, player1vector)

addPlayer1 <- FRE04_TFg3[ which(FRE04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_TFg3[ which(FRE04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_TFg2 <- rbind(FRE04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
FRE04_TFft <- ftable(FRE04_TFg2$player1, FRE04_TFg2$player2)
FRE04_TFft2 <- as.matrix(FRE04_TFft)
numRows <- nrow(FRE04_TFft2)
numCols <- ncol(FRE04_TFft2)
FRE04_TFft3 <- FRE04_TFft2[c(2:numRows) , c(2:numCols)]
FRE04_TFTable <- graph.adjacency(FRE04_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(FRE04_TFTable, vertex.label = V(FRE04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
FRE04_TF.clusterCoef <- transitivity(FRE04_TFTable, type="global") #cluster coefficient
FRE04_TF.degreeCent <- centralization.degree(FRE04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_TFftn <- as.network.matrix(FRE04_TFft)
FRE04_TF.netDensity <- network.density(FRE04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_TF.entropy <- entropy(FRE04_TFft) #entropy

FRE04_TF.netMx <- cbind(FRE04_TF.netMx, FRE04_TF.clusterCoef, FRE04_TF.degreeCent$centralization,
                        FRE04_TF.netDensity, FRE04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************

round = 4
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
FRE04_SAMg2 <- data.frame(FRE04_SAM)
FRE04_SAMg2 <- FRE04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_SAMg2$player1
player2vector <- FRE04_SAMg2$player2
FRE04_SAMg3 <- FRE04_SAMg2
FRE04_SAMg3$p1inp2vec <- is.element(FRE04_SAMg3$player1, player2vector)
FRE04_SAMg3$p2inp1vec <- is.element(FRE04_SAMg3$player2, player1vector)

addPlayer1 <- FRE04_SAMg3[ which(FRE04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_SAMg3[ which(FRE04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_SAMg2 <- rbind(FRE04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
FRE04_SAMft <- ftable(FRE04_SAMg2$player1, FRE04_SAMg2$player2)
FRE04_SAMft2 <- as.matrix(FRE04_SAMft)
numRows <- nrow(FRE04_SAMft2)
numCols <- ncol(FRE04_SAMft2)
FRE04_SAMft3 <- FRE04_SAMft2[c(2:numRows) , c(2:numCols)]
FRE04_SAMTable <- graph.adjacency(FRE04_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(FRE04_SAMTable, vertex.label = V(FRE04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
FRE04_SAM.clusterCoef <- transitivity(FRE04_SAMTable, type="global") #cluster coefficient
FRE04_SAM.degreeCent <- centralization.degree(FRE04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_SAMftn <- as.network.matrix(FRE04_SAMft)
FRE04_SAM.netDensity <- network.density(FRE04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_SAM.entropy <- entropy(FRE04_SAMft) #entropy

FRE04_SAM.netMx <- cbind(FRE04_SAM.netMx, FRE04_SAM.clusterCoef, FRE04_SAM.degreeCent$centralization,
                         FRE04_SAM.netDensity, FRE04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
FRE04_TAMg2 <- data.frame(FRE04_TAM)
FRE04_TAMg2 <- FRE04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_TAMg2$player1
player2vector <- FRE04_TAMg2$player2
FRE04_TAMg3 <- FRE04_TAMg2
FRE04_TAMg3$p1inp2vec <- is.element(FRE04_TAMg3$player1, player2vector)
FRE04_TAMg3$p2inp1vec <- is.element(FRE04_TAMg3$player2, player1vector)

addPlayer1 <- FRE04_TAMg3[ which(FRE04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_TAMg3[ which(FRE04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_TAMg2 <- rbind(FRE04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
FRE04_TAMft <- ftable(FRE04_TAMg2$player1, FRE04_TAMg2$player2)
FRE04_TAMft2 <- as.matrix(FRE04_TAMft)
numRows <- nrow(FRE04_TAMft2)
numCols <- ncol(FRE04_TAMft2)
FRE04_TAMft3 <- FRE04_TAMft2[c(2:numRows) , c(2:numCols)]
FRE04_TAMTable <- graph.adjacency(FRE04_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(FRE04_TAMTable, vertex.label = V(FRE04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
FRE04_TAM.clusterCoef <- transitivity(FRE04_TAMTable, type="global") #cluster coefficient
FRE04_TAM.degreeCent <- centralization.degree(FRE04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_TAMftn <- as.network.matrix(FRE04_TAMft)
FRE04_TAM.netDensity <- network.density(FRE04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_TAM.entropy <- entropy(FRE04_TAMft) #entropy

FRE04_TAM.netMx <- cbind(FRE04_TAM.netMx, FRE04_TAM.clusterCoef, FRE04_TAM.degreeCent$centralization,
                         FRE04_TAM.netDensity, FRE04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************
#NA

round = 4
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
FRE04_SDMg2 <- data.frame(FRE04_SDM)
FRE04_SDMg2 <- FRE04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_SDMg2$player1
player2vector <- FRE04_SDMg2$player2
FRE04_SDMg3 <- FRE04_SDMg2
FRE04_SDMg3$p1inp2vec <- is.element(FRE04_SDMg3$player1, player2vector)
FRE04_SDMg3$p2inp1vec <- is.element(FRE04_SDMg3$player2, player1vector)

addPlayer1 <- FRE04_SDMg3[ which(FRE04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_SDMg3[ which(FRE04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_SDMg2 <- rbind(FRE04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
FRE04_SDMft <- ftable(FRE04_SDMg2$player1, FRE04_SDMg2$player2)
FRE04_SDMft2 <- as.matrix(FRE04_SDMft)
numRows <- nrow(FRE04_SDMft2)
numCols <- ncol(FRE04_SDMft2)
FRE04_SDMft3 <- FRE04_SDMft2[c(2:numRows) , c(2:numCols)]
FRE04_SDMTable <- graph.adjacency(FRE04_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(FRE04_SDMTable, vertex.label = V(FRE04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
FRE04_SDM.clusterCoef <- transitivity(FRE04_SDMTable, type="global") #cluster coefficient
FRE04_SDM.degreeCent <- centralization.degree(FRE04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_SDMftn <- as.network.matrix(FRE04_SDMft)
FRE04_SDM.netDensity <- network.density(FRE04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_SDM.entropy <- entropy(FRE04_SDMft) #entropy

FRE04_SDM.netMx <- cbind(FRE04_SDM.netMx, FRE04_SDM.clusterCoef, FRE04_SDM.degreeCent$centralization,
                         FRE04_SDM.netDensity, FRE04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
FRE04_TDMg2 <- data.frame(FRE04_TDM)
FRE04_TDMg2 <- FRE04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_TDMg2$player1
player2vector <- FRE04_TDMg2$player2
FRE04_TDMg3 <- FRE04_TDMg2
FRE04_TDMg3$p1inp2vec <- is.element(FRE04_TDMg3$player1, player2vector)
FRE04_TDMg3$p2inp1vec <- is.element(FRE04_TDMg3$player2, player1vector)

addPlayer1 <- FRE04_TDMg3[ which(FRE04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_TDMg3[ which(FRE04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_TDMg2 <- rbind(FRE04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
FRE04_TDMft <- ftable(FRE04_TDMg2$player1, FRE04_TDMg2$player2)
FRE04_TDMft2 <- as.matrix(FRE04_TDMft)
numRows <- nrow(FRE04_TDMft2)
numCols <- ncol(FRE04_TDMft2)
FRE04_TDMft3 <- FRE04_TDMft2[c(2:numRows) , c(2:numCols)]
FRE04_TDMTable <- graph.adjacency(FRE04_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(FRE04_TDMTable, vertex.label = V(FRE04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
FRE04_TDM.clusterCoef <- transitivity(FRE04_TDMTable, type="global") #cluster coefficient
FRE04_TDM.degreeCent <- centralization.degree(FRE04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_TDMftn <- as.network.matrix(FRE04_TDMft)
FRE04_TDM.netDensity <- network.density(FRE04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_TDM.entropy <- entropy(FRE04_TDMft) #entropy

FRE04_TDM.netMx <- cbind(FRE04_TDM.netMx, FRE04_TDM.clusterCoef, FRE04_TDM.degreeCent$centralization,
                         FRE04_TDM.netDensity, FRE04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
FRE04_SDg2 <- data.frame(FRE04_SD)
FRE04_SDg2 <- FRE04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_SDg2$player1
player2vector <- FRE04_SDg2$player2
FRE04_SDg3 <- FRE04_SDg2
FRE04_SDg3$p1inp2vec <- is.element(FRE04_SDg3$player1, player2vector)
FRE04_SDg3$p2inp1vec <- is.element(FRE04_SDg3$player2, player1vector)

addPlayer1 <- FRE04_SDg3[ which(FRE04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_SDg3[ which(FRE04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_SDg2 <- rbind(FRE04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
FRE04_SDft <- ftable(FRE04_SDg2$player1, FRE04_SDg2$player2)
FRE04_SDft2 <- as.matrix(FRE04_SDft)
numRows <- nrow(FRE04_SDft2)
numCols <- ncol(FRE04_SDft2)
FRE04_SDft3 <- FRE04_SDft2[c(2:numRows) , c(2:numCols)]
FRE04_SDTable <- graph.adjacency(FRE04_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(FRE04_SDTable, vertex.label = V(FRE04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
FRE04_SD.clusterCoef <- transitivity(FRE04_SDTable, type="global") #cluster coefficient
FRE04_SD.degreeCent <- centralization.degree(FRE04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_SDftn <- as.network.matrix(FRE04_SDft)
FRE04_SD.netDensity <- network.density(FRE04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_SD.entropy <- entropy(FRE04_SDft) #entropy

FRE04_SD.netMx <- cbind(FRE04_SD.netMx, FRE04_SD.clusterCoef, FRE04_SD.degreeCent$centralization,
                        FRE04_SD.netDensity, FRE04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
FRE04_TDg2 <- data.frame(FRE04_TD)
FRE04_TDg2 <- FRE04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_TDg2$player1
player2vector <- FRE04_TDg2$player2
FRE04_TDg3 <- FRE04_TDg2
FRE04_TDg3$p1inp2vec <- is.element(FRE04_TDg3$player1, player2vector)
FRE04_TDg3$p2inp1vec <- is.element(FRE04_TDg3$player2, player1vector)

addPlayer1 <- FRE04_TDg3[ which(FRE04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_TDg3[ which(FRE04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_TDg2 <- rbind(FRE04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
FRE04_TDft <- ftable(FRE04_TDg2$player1, FRE04_TDg2$player2)
FRE04_TDft2 <- as.matrix(FRE04_TDft)
numRows <- nrow(FRE04_TDft2)
numCols <- ncol(FRE04_TDft2)
FRE04_TDft3 <- FRE04_TDft2[c(2:numRows) , c(2:numCols)]
FRE04_TDTable <- graph.adjacency(FRE04_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(FRE04_TDTable, vertex.label = V(FRE04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
FRE04_TD.clusterCoef <- transitivity(FRE04_TDTable, type="global") #cluster coefficient
FRE04_TD.degreeCent <- centralization.degree(FRE04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_TDftn <- as.network.matrix(FRE04_TDft)
FRE04_TD.netDensity <- network.density(FRE04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_TD.entropy <- entropy(FRE04_TDft) #entropy

FRE04_TD.netMx <- cbind(FRE04_TD.netMx, FRE04_TD.clusterCoef, FRE04_TD.degreeCent$centralization,
                        FRE04_TD.netDensity, FRE04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
FRE04_QTg2 <- data.frame(FRE04_QT)
FRE04_QTg2 <- FRE04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE04_QTg2$player1
player2vector <- FRE04_QTg2$player2
FRE04_QTg3 <- FRE04_QTg2
FRE04_QTg3$p1inp2vec <- is.element(FRE04_QTg3$player1, player2vector)
FRE04_QTg3$p2inp1vec <- is.element(FRE04_QTg3$player2, player1vector)

addPlayer1 <- FRE04_QTg3[ which(FRE04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE04_QTg3[ which(FRE04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE04_QTg2 <- rbind(FRE04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
FRE04_QTft <- ftable(FRE04_QTg2$player1, FRE04_QTg2$player2)
FRE04_QTft2 <- as.matrix(FRE04_QTft)
numRows <- nrow(FRE04_QTft2)
numCols <- ncol(FRE04_QTft2)
FRE04_QTft3 <- FRE04_QTft2[c(2:numRows) , c(2:numCols)]
FRE04_QTTable <- graph.adjacency(FRE04_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(FRE04_QTTable, vertex.label = V(FRE04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
FRE04_QT.clusterCoef <- transitivity(FRE04_QTTable, type="global") #cluster coefficient
FRE04_QT.degreeCent <- centralization.degree(FRE04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE04_QTftn <- as.network.matrix(FRE04_QTft)
FRE04_QT.netDensity <- network.density(FRE04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE04_QT.entropy <- entropy(FRE04_QTft) #entropy

FRE04_QT.netMx <- cbind(FRE04_QT.netMx, FRE04_QT.clusterCoef, FRE04_QT.degreeCent$centralization,
                        FRE04_QT.netDensity, FRE04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE04_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
GCFC04_Gg2 <- data.frame(GCFC04_G)
GCFC04_Gg2 <- GCFC04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_Gg2$player1
player2vector <- GCFC04_Gg2$player2
GCFC04_Gg3 <- GCFC04_Gg2
GCFC04_Gg3$p1inp2vec <- is.element(GCFC04_Gg3$player1, player2vector)
GCFC04_Gg3$p2inp1vec <- is.element(GCFC04_Gg3$player2, player1vector)

addPlayer1 <- GCFC04_Gg3[ which(GCFC04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_Gg3[ which(GCFC04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_Gg2 <- rbind(GCFC04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
GCFC04_Gft <- ftable(GCFC04_Gg2$player1, GCFC04_Gg2$player2)
GCFC04_Gft2 <- as.matrix(GCFC04_Gft)
numRows <- nrow(GCFC04_Gft2)
numCols <- ncol(GCFC04_Gft2)
GCFC04_Gft3 <- GCFC04_Gft2[c(2:numRows) , c(2:numCols)]
GCFC04_GTable <- graph.adjacency(GCFC04_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(GCFC04_GTable, vertex.label = V(GCFC04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
GCFC04_G.clusterCoef <- transitivity(GCFC04_GTable, type="global") #cluster coefficient
GCFC04_G.degreeCent <- centralization.degree(GCFC04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_Gftn <- as.network.matrix(GCFC04_Gft)
GCFC04_G.netDensity <- network.density(GCFC04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_G.entropy <- entropy(GCFC04_Gft) #entropy

GCFC04_G.netMx <- cbind(GCFC04_G.netMx, GCFC04_G.clusterCoef, GCFC04_G.degreeCent$centralization,
                        GCFC04_G.netDensity, GCFC04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************

round = 4
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
GCFC04_Bg2 <- data.frame(GCFC04_B)
GCFC04_Bg2 <- GCFC04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_Bg2$player1
player2vector <- GCFC04_Bg2$player2
GCFC04_Bg3 <- GCFC04_Bg2
GCFC04_Bg3$p1inp2vec <- is.element(GCFC04_Bg3$player1, player2vector)
GCFC04_Bg3$p2inp1vec <- is.element(GCFC04_Bg3$player2, player1vector)

addPlayer1 <- GCFC04_Bg3[ which(GCFC04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_Bg3[ which(GCFC04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_Bg2 <- rbind(GCFC04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
GCFC04_Bft <- ftable(GCFC04_Bg2$player1, GCFC04_Bg2$player2)
GCFC04_Bft2 <- as.matrix(GCFC04_Bft)
numRows <- nrow(GCFC04_Bft2)
numCols <- ncol(GCFC04_Bft2)
GCFC04_Bft3 <- GCFC04_Bft2[c(2:numRows) , c(2:numCols)]
GCFC04_BTable <- graph.adjacency(GCFC04_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(GCFC04_BTable, vertex.label = V(GCFC04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
GCFC04_B.clusterCoef <- transitivity(GCFC04_BTable, type="global") #cluster coefficient
GCFC04_B.degreeCent <- centralization.degree(GCFC04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_Bftn <- as.network.matrix(GCFC04_Bft)
GCFC04_B.netDensity <- network.density(GCFC04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_B.entropy <- entropy(GCFC04_Bft) #entropy

GCFC04_B.netMx <- cbind(GCFC04_B.netMx, GCFC04_B.clusterCoef, GCFC04_B.degreeCent$centralization,
                        GCFC04_B.netDensity, GCFC04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
GCFC04_SFg2 <- data.frame(GCFC04_SF)
GCFC04_SFg2 <- GCFC04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_SFg2$player1
player2vector <- GCFC04_SFg2$player2
GCFC04_SFg3 <- GCFC04_SFg2
GCFC04_SFg3$p1inp2vec <- is.element(GCFC04_SFg3$player1, player2vector)
GCFC04_SFg3$p2inp1vec <- is.element(GCFC04_SFg3$player2, player1vector)

addPlayer1 <- GCFC04_SFg3[ which(GCFC04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC04_SFg2 <- rbind(GCFC04_SFg2, addPlayer1)

#ROUND 4, FWD Stoppage graph using weighted edges
GCFC04_SFft <- ftable(GCFC04_SFg2$player1, GCFC04_SFg2$player2)
GCFC04_SFft2 <- as.matrix(GCFC04_SFft)
numRows <- nrow(GCFC04_SFft2)
numCols <- ncol(GCFC04_SFft2)
GCFC04_SFft3 <- GCFC04_SFft2[c(2:numRows) , c(1:numCols)]
GCFC04_SFTable <- graph.adjacency(GCFC04_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(GCFC04_SFTable, vertex.label = V(GCFC04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
GCFC04_SF.clusterCoef <- transitivity(GCFC04_SFTable, type="global") #cluster coefficient
GCFC04_SF.degreeCent <- centralization.degree(GCFC04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_SFftn <- as.network.matrix(GCFC04_SFft)
GCFC04_SF.netDensity <- network.density(GCFC04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_SF.entropy <- entropy(GCFC04_SFft) #entropy

GCFC04_SF.netMx <- cbind(GCFC04_SF.netMx, GCFC04_SF.clusterCoef, GCFC04_SF.degreeCent$centralization,
                         GCFC04_SF.netDensity, GCFC04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
GCFC04_TFg2 <- data.frame(GCFC04_TF)
GCFC04_TFg2 <- GCFC04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_TFg2$player1
player2vector <- GCFC04_TFg2$player2
GCFC04_TFg3 <- GCFC04_TFg2
GCFC04_TFg3$p1inp2vec <- is.element(GCFC04_TFg3$player1, player2vector)
GCFC04_TFg3$p2inp1vec <- is.element(GCFC04_TFg3$player2, player1vector)

addPlayer1 <- GCFC04_TFg3[ which(GCFC04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_TFg3[ which(GCFC04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_TFg2 <- rbind(GCFC04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
GCFC04_TFft <- ftable(GCFC04_TFg2$player1, GCFC04_TFg2$player2)
GCFC04_TFft2 <- as.matrix(GCFC04_TFft)
numRows <- nrow(GCFC04_TFft2)
numCols <- ncol(GCFC04_TFft2)
GCFC04_TFft3 <- GCFC04_TFft2[c(2:numRows) , c(2:numCols)]
GCFC04_TFTable <- graph.adjacency(GCFC04_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(GCFC04_TFTable, vertex.label = V(GCFC04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
GCFC04_TF.clusterCoef <- transitivity(GCFC04_TFTable, type="global") #cluster coefficient
GCFC04_TF.degreeCent <- centralization.degree(GCFC04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_TFftn <- as.network.matrix(GCFC04_TFft)
GCFC04_TF.netDensity <- network.density(GCFC04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_TF.entropy <- entropy(GCFC04_TFft) #entropy

GCFC04_TF.netMx <- cbind(GCFC04_TF.netMx, GCFC04_TF.clusterCoef, GCFC04_TF.degreeCent$centralization,
                         GCFC04_TF.netDensity, GCFC04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
GCFC04_SAMg2 <- data.frame(GCFC04_SAM)
GCFC04_SAMg2 <- GCFC04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_SAMg2$player1
player2vector <- GCFC04_SAMg2$player2
GCFC04_SAMg3 <- GCFC04_SAMg2
GCFC04_SAMg3$p1inp2vec <- is.element(GCFC04_SAMg3$player1, player2vector)
GCFC04_SAMg3$p2inp1vec <- is.element(GCFC04_SAMg3$player2, player1vector)

addPlayer1 <- GCFC04_SAMg3[ which(GCFC04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_SAMg3[ which(GCFC04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_SAMg2 <- rbind(GCFC04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
GCFC04_SAMft <- ftable(GCFC04_SAMg2$player1, GCFC04_SAMg2$player2)
GCFC04_SAMft2 <- as.matrix(GCFC04_SAMft)
numRows <- nrow(GCFC04_SAMft2)
numCols <- ncol(GCFC04_SAMft2)
GCFC04_SAMft3 <- GCFC04_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC04_SAMTable <- graph.adjacency(GCFC04_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(GCFC04_SAMTable, vertex.label = V(GCFC04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
GCFC04_SAM.clusterCoef <- transitivity(GCFC04_SAMTable, type="global") #cluster coefficient
GCFC04_SAM.degreeCent <- centralization.degree(GCFC04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_SAMftn <- as.network.matrix(GCFC04_SAMft)
GCFC04_SAM.netDensity <- network.density(GCFC04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_SAM.entropy <- entropy(GCFC04_SAMft) #entropy

GCFC04_SAM.netMx <- cbind(GCFC04_SAM.netMx, GCFC04_SAM.clusterCoef, GCFC04_SAM.degreeCent$centralization,
                          GCFC04_SAM.netDensity, GCFC04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
GCFC04_TAMg2 <- data.frame(GCFC04_TAM)
GCFC04_TAMg2 <- GCFC04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_TAMg2$player1
player2vector <- GCFC04_TAMg2$player2
GCFC04_TAMg3 <- GCFC04_TAMg2
GCFC04_TAMg3$p1inp2vec <- is.element(GCFC04_TAMg3$player1, player2vector)
GCFC04_TAMg3$p2inp1vec <- is.element(GCFC04_TAMg3$player2, player1vector)

addPlayer1 <- GCFC04_TAMg3[ which(GCFC04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_TAMg3[ which(GCFC04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_TAMg2 <- rbind(GCFC04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
GCFC04_TAMft <- ftable(GCFC04_TAMg2$player1, GCFC04_TAMg2$player2)
GCFC04_TAMft2 <- as.matrix(GCFC04_TAMft)
numRows <- nrow(GCFC04_TAMft2)
numCols <- ncol(GCFC04_TAMft2)
GCFC04_TAMft3 <- GCFC04_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC04_TAMTable <- graph.adjacency(GCFC04_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(GCFC04_TAMTable, vertex.label = V(GCFC04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
GCFC04_TAM.clusterCoef <- transitivity(GCFC04_TAMTable, type="global") #cluster coefficient
GCFC04_TAM.degreeCent <- centralization.degree(GCFC04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_TAMftn <- as.network.matrix(GCFC04_TAMft)
GCFC04_TAM.netDensity <- network.density(GCFC04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_TAM.entropy <- entropy(GCFC04_TAMft) #entropy

GCFC04_TAM.netMx <- cbind(GCFC04_TAM.netMx, GCFC04_TAM.clusterCoef, GCFC04_TAM.degreeCent$centralization,
                          GCFC04_TAM.netDensity, GCFC04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************
#NA

round = 4
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
GCFC04_SDMg2 <- data.frame(GCFC04_SDM)
GCFC04_SDMg2 <- GCFC04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_SDMg2$player1
player2vector <- GCFC04_SDMg2$player2
GCFC04_SDMg3 <- GCFC04_SDMg2
GCFC04_SDMg3$p1inp2vec <- is.element(GCFC04_SDMg3$player1, player2vector)
GCFC04_SDMg3$p2inp1vec <- is.element(GCFC04_SDMg3$player2, player1vector)

addPlayer1 <- GCFC04_SDMg3[ which(GCFC04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_SDMg3[ which(GCFC04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_SDMg2 <- rbind(GCFC04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
GCFC04_SDMft <- ftable(GCFC04_SDMg2$player1, GCFC04_SDMg2$player2)
GCFC04_SDMft2 <- as.matrix(GCFC04_SDMft)
numRows <- nrow(GCFC04_SDMft2)
numCols <- ncol(GCFC04_SDMft2)
GCFC04_SDMft3 <- GCFC04_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC04_SDMTable <- graph.adjacency(GCFC04_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(GCFC04_SDMTable, vertex.label = V(GCFC04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
GCFC04_SDM.clusterCoef <- transitivity(GCFC04_SDMTable, type="global") #cluster coefficient
GCFC04_SDM.degreeCent <- centralization.degree(GCFC04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_SDMftn <- as.network.matrix(GCFC04_SDMft)
GCFC04_SDM.netDensity <- network.density(GCFC04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_SDM.entropy <- entropy(GCFC04_SDMft) #entropy

GCFC04_SDM.netMx <- cbind(GCFC04_SDM.netMx, GCFC04_SDM.clusterCoef, GCFC04_SDM.degreeCent$centralization,
                          GCFC04_SDM.netDensity, GCFC04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
GCFC04_TDMg2 <- data.frame(GCFC04_TDM)
GCFC04_TDMg2 <- GCFC04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_TDMg2$player1
player2vector <- GCFC04_TDMg2$player2
GCFC04_TDMg3 <- GCFC04_TDMg2
GCFC04_TDMg3$p1inp2vec <- is.element(GCFC04_TDMg3$player1, player2vector)
GCFC04_TDMg3$p2inp1vec <- is.element(GCFC04_TDMg3$player2, player1vector)

addPlayer1 <- GCFC04_TDMg3[ which(GCFC04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_TDMg3[ which(GCFC04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_TDMg2 <- rbind(GCFC04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
GCFC04_TDMft <- ftable(GCFC04_TDMg2$player1, GCFC04_TDMg2$player2)
GCFC04_TDMft2 <- as.matrix(GCFC04_TDMft)
numRows <- nrow(GCFC04_TDMft2)
numCols <- ncol(GCFC04_TDMft2)
GCFC04_TDMft3 <- GCFC04_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC04_TDMTable <- graph.adjacency(GCFC04_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(GCFC04_TDMTable, vertex.label = V(GCFC04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
GCFC04_TDM.clusterCoef <- transitivity(GCFC04_TDMTable, type="global") #cluster coefficient
GCFC04_TDM.degreeCent <- centralization.degree(GCFC04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_TDMftn <- as.network.matrix(GCFC04_TDMft)
GCFC04_TDM.netDensity <- network.density(GCFC04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_TDM.entropy <- entropy(GCFC04_TDMft) #entropy

GCFC04_TDM.netMx <- cbind(GCFC04_TDM.netMx, GCFC04_TDM.clusterCoef, GCFC04_TDM.degreeCent$centralization,
                          GCFC04_TDM.netDensity, GCFC04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
GCFC04_SDg2 <- data.frame(GCFC04_SD)
GCFC04_SDg2 <- GCFC04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_SDg2$player1
player2vector <- GCFC04_SDg2$player2
GCFC04_SDg3 <- GCFC04_SDg2
GCFC04_SDg3$p1inp2vec <- is.element(GCFC04_SDg3$player1, player2vector)
GCFC04_SDg3$p2inp1vec <- is.element(GCFC04_SDg3$player2, player1vector)

addPlayer1 <- GCFC04_SDg3[ which(GCFC04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_SDg3[ which(GCFC04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_SDg2 <- rbind(GCFC04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
GCFC04_SDft <- ftable(GCFC04_SDg2$player1, GCFC04_SDg2$player2)
GCFC04_SDft2 <- as.matrix(GCFC04_SDft)
numRows <- nrow(GCFC04_SDft2)
numCols <- ncol(GCFC04_SDft2)
GCFC04_SDft3 <- GCFC04_SDft2[c(2:numRows) , c(2:numCols)]
GCFC04_SDTable <- graph.adjacency(GCFC04_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(GCFC04_SDTable, vertex.label = V(GCFC04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
GCFC04_SD.clusterCoef <- transitivity(GCFC04_SDTable, type="global") #cluster coefficient
GCFC04_SD.degreeCent <- centralization.degree(GCFC04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_SDftn <- as.network.matrix(GCFC04_SDft)
GCFC04_SD.netDensity <- network.density(GCFC04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_SD.entropy <- entropy(GCFC04_SDft) #entropy

GCFC04_SD.netMx <- cbind(GCFC04_SD.netMx, GCFC04_SD.clusterCoef, GCFC04_SD.degreeCent$centralization,
                         GCFC04_SD.netDensity, GCFC04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************

round = 4
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
GCFC04_TDg2 <- data.frame(GCFC04_TD)
GCFC04_TDg2 <- GCFC04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_TDg2$player1
player2vector <- GCFC04_TDg2$player2
GCFC04_TDg3 <- GCFC04_TDg2
GCFC04_TDg3$p1inp2vec <- is.element(GCFC04_TDg3$player1, player2vector)
GCFC04_TDg3$p2inp1vec <- is.element(GCFC04_TDg3$player2, player1vector)

addPlayer1 <- GCFC04_TDg3[ which(GCFC04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_TDg3[ which(GCFC04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_TDg2 <- rbind(GCFC04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
GCFC04_TDft <- ftable(GCFC04_TDg2$player1, GCFC04_TDg2$player2)
GCFC04_TDft2 <- as.matrix(GCFC04_TDft)
numRows <- nrow(GCFC04_TDft2)
numCols <- ncol(GCFC04_TDft2)
GCFC04_TDft3 <- GCFC04_TDft2[c(2:numRows) , c(2:numCols)]
GCFC04_TDTable <- graph.adjacency(GCFC04_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(GCFC04_TDTable, vertex.label = V(GCFC04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
GCFC04_TD.clusterCoef <- transitivity(GCFC04_TDTable, type="global") #cluster coefficient
GCFC04_TD.degreeCent <- centralization.degree(GCFC04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_TDftn <- as.network.matrix(GCFC04_TDft)
GCFC04_TD.netDensity <- network.density(GCFC04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_TD.entropy <- entropy(GCFC04_TDft) #entropy

GCFC04_TD.netMx <- cbind(GCFC04_TD.netMx, GCFC04_TD.clusterCoef, GCFC04_TD.degreeCent$centralization,
                         GCFC04_TD.netDensity, GCFC04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
GCFC04_QTg2 <- data.frame(GCFC04_QT)
GCFC04_QTg2 <- GCFC04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC04_QTg2$player1
player2vector <- GCFC04_QTg2$player2
GCFC04_QTg3 <- GCFC04_QTg2
GCFC04_QTg3$p1inp2vec <- is.element(GCFC04_QTg3$player1, player2vector)
GCFC04_QTg3$p2inp1vec <- is.element(GCFC04_QTg3$player2, player1vector)

addPlayer1 <- GCFC04_QTg3[ which(GCFC04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC04_QTg3[ which(GCFC04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC04_QTg2 <- rbind(GCFC04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
GCFC04_QTft <- ftable(GCFC04_QTg2$player1, GCFC04_QTg2$player2)
GCFC04_QTft2 <- as.matrix(GCFC04_QTft)
numRows <- nrow(GCFC04_QTft2)
numCols <- ncol(GCFC04_QTft2)
GCFC04_QTft3 <- GCFC04_QTft2[c(2:numRows) , c(2:numCols)]
GCFC04_QTTable <- graph.adjacency(GCFC04_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(GCFC04_QTTable, vertex.label = V(GCFC04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
GCFC04_QT.clusterCoef <- transitivity(GCFC04_QTTable, type="global") #cluster coefficient
GCFC04_QT.degreeCent <- centralization.degree(GCFC04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC04_QTftn <- as.network.matrix(GCFC04_QTft)
GCFC04_QT.netDensity <- network.density(GCFC04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC04_QT.entropy <- entropy(GCFC04_QTft) #entropy

GCFC04_QT.netMx <- cbind(GCFC04_QT.netMx, GCFC04_QT.clusterCoef, GCFC04_QT.degreeCent$centralization,
                         GCFC04_QT.netDensity, GCFC04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC04_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************

round = 4
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
GEEL04_Gg2 <- data.frame(GEEL04_G)
GEEL04_Gg2 <- GEEL04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_Gg2$player1
player2vector <- GEEL04_Gg2$player2
GEEL04_Gg3 <- GEEL04_Gg2
GEEL04_Gg3$p1inp2vec <- is.element(GEEL04_Gg3$player1, player2vector)
GEEL04_Gg3$p2inp1vec <- is.element(GEEL04_Gg3$player2, player1vector)

addPlayer1 <- GEEL04_Gg3[ which(GEEL04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_Gg3[ which(GEEL04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_Gg2 <- rbind(GEEL04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
GEEL04_Gft <- ftable(GEEL04_Gg2$player1, GEEL04_Gg2$player2)
GEEL04_Gft2 <- as.matrix(GEEL04_Gft)
numRows <- nrow(GEEL04_Gft2)
numCols <- ncol(GEEL04_Gft2)
GEEL04_Gft3 <- GEEL04_Gft2[c(2:numRows) , c(2:numCols)]
GEEL04_GTable <- graph.adjacency(GEEL04_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(GEEL04_GTable, vertex.label = V(GEEL04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
GEEL04_G.clusterCoef <- transitivity(GEEL04_GTable, type="global") #cluster coefficient
GEEL04_G.degreeCent <- centralization.degree(GEEL04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_Gftn <- as.network.matrix(GEEL04_Gft)
GEEL04_G.netDensity <- network.density(GEEL04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_G.entropy <- entropy(GEEL04_Gft) #entropy

GEEL04_G.netMx <- cbind(GEEL04_G.netMx, GEEL04_G.clusterCoef, GEEL04_G.degreeCent$centralization,
                        GEEL04_G.netDensity, GEEL04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************

round = 4
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
GEEL04_Bg2 <- data.frame(GEEL04_B)
GEEL04_Bg2 <- GEEL04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_Bg2$player1
player2vector <- GEEL04_Bg2$player2
GEEL04_Bg3 <- GEEL04_Bg2
GEEL04_Bg3$p1inp2vec <- is.element(GEEL04_Bg3$player1, player2vector)
GEEL04_Bg3$p2inp1vec <- is.element(GEEL04_Bg3$player2, player1vector)

addPlayer1 <- GEEL04_Bg3[ which(GEEL04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_Bg3[ which(GEEL04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_Bg2 <- rbind(GEEL04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
GEEL04_Bft <- ftable(GEEL04_Bg2$player1, GEEL04_Bg2$player2)
GEEL04_Bft2 <- as.matrix(GEEL04_Bft)
numRows <- nrow(GEEL04_Bft2)
numCols <- ncol(GEEL04_Bft2)
GEEL04_Bft3 <- GEEL04_Bft2[c(2:numRows) , c(2:numCols)]
GEEL04_BTable <- graph.adjacency(GEEL04_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(GEEL04_BTable, vertex.label = V(GEEL04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
GEEL04_B.clusterCoef <- transitivity(GEEL04_BTable, type="global") #cluster coefficient
GEEL04_B.degreeCent <- centralization.degree(GEEL04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_Bftn <- as.network.matrix(GEEL04_Bft)
GEEL04_B.netDensity <- network.density(GEEL04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_B.entropy <- entropy(GEEL04_Bft) #entropy

GEEL04_B.netMx <- cbind(GEEL04_B.netMx, GEEL04_B.clusterCoef, GEEL04_B.degreeCent$centralization,
                        GEEL04_B.netDensity, GEEL04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************

round = 4
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
GEEL04_SFg2 <- data.frame(GEEL04_SF)
GEEL04_SFg2 <- GEEL04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_SFg2$player1
player2vector <- GEEL04_SFg2$player2
GEEL04_SFg3 <- GEEL04_SFg2
GEEL04_SFg3$p1inp2vec <- is.element(GEEL04_SFg3$player1, player2vector)
GEEL04_SFg3$p2inp1vec <- is.element(GEEL04_SFg3$player2, player1vector)

addPlayer1 <- GEEL04_SFg3[ which(GEEL04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_SFg3[ which(GEEL04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_SFg2 <- rbind(GEEL04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
GEEL04_SFft <- ftable(GEEL04_SFg2$player1, GEEL04_SFg2$player2)
GEEL04_SFft2 <- as.matrix(GEEL04_SFft)
numRows <- nrow(GEEL04_SFft2)
numCols <- ncol(GEEL04_SFft2)
GEEL04_SFft3 <- GEEL04_SFft2[c(2:numRows) , c(2:numCols)]
GEEL04_SFTable <- graph.adjacency(GEEL04_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(GEEL04_SFTable, vertex.label = V(GEEL04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
GEEL04_SF.clusterCoef <- transitivity(GEEL04_SFTable, type="global") #cluster coefficient
GEEL04_SF.degreeCent <- centralization.degree(GEEL04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_SFftn <- as.network.matrix(GEEL04_SFft)
GEEL04_SF.netDensity <- network.density(GEEL04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_SF.entropy <- entropy(GEEL04_SFft) #entropy

GEEL04_SF.netMx <- cbind(GEEL04_SF.netMx, GEEL04_SF.clusterCoef, GEEL04_SF.degreeCent$centralization,
                         GEEL04_SF.netDensity, GEEL04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
GEEL04_TFg2 <- data.frame(GEEL04_TF)
GEEL04_TFg2 <- GEEL04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_TFg2$player1
player2vector <- GEEL04_TFg2$player2
GEEL04_TFg3 <- GEEL04_TFg2
GEEL04_TFg3$p1inp2vec <- is.element(GEEL04_TFg3$player1, player2vector)
GEEL04_TFg3$p2inp1vec <- is.element(GEEL04_TFg3$player2, player1vector)

addPlayer1 <- GEEL04_TFg3[ which(GEEL04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_TFg3[ which(GEEL04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_TFg2 <- rbind(GEEL04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
GEEL04_TFft <- ftable(GEEL04_TFg2$player1, GEEL04_TFg2$player2)
GEEL04_TFft2 <- as.matrix(GEEL04_TFft)
numRows <- nrow(GEEL04_TFft2)
numCols <- ncol(GEEL04_TFft2)
GEEL04_TFft3 <- GEEL04_TFft2[c(2:numRows) , c(2:numCols)]
GEEL04_TFTable <- graph.adjacency(GEEL04_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(GEEL04_TFTable, vertex.label = V(GEEL04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
GEEL04_TF.clusterCoef <- transitivity(GEEL04_TFTable, type="global") #cluster coefficient
GEEL04_TF.degreeCent <- centralization.degree(GEEL04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_TFftn <- as.network.matrix(GEEL04_TFft)
GEEL04_TF.netDensity <- network.density(GEEL04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_TF.entropy <- entropy(GEEL04_TFft) #entropy

GEEL04_TF.netMx <- cbind(GEEL04_TF.netMx, GEEL04_TF.clusterCoef, GEEL04_TF.degreeCent$centralization,
                         GEEL04_TF.netDensity, GEEL04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
GEEL04_SAMg2 <- data.frame(GEEL04_SAM)
GEEL04_SAMg2 <- GEEL04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_SAMg2$player1
player2vector <- GEEL04_SAMg2$player2
GEEL04_SAMg3 <- GEEL04_SAMg2
GEEL04_SAMg3$p1inp2vec <- is.element(GEEL04_SAMg3$player1, player2vector)
GEEL04_SAMg3$p2inp1vec <- is.element(GEEL04_SAMg3$player2, player1vector)

addPlayer1 <- GEEL04_SAMg3[ which(GEEL04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_SAMg3[ which(GEEL04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_SAMg2 <- rbind(GEEL04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
GEEL04_SAMft <- ftable(GEEL04_SAMg2$player1, GEEL04_SAMg2$player2)
GEEL04_SAMft2 <- as.matrix(GEEL04_SAMft)
numRows <- nrow(GEEL04_SAMft2)
numCols <- ncol(GEEL04_SAMft2)
GEEL04_SAMft3 <- GEEL04_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL04_SAMTable <- graph.adjacency(GEEL04_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(GEEL04_SAMTable, vertex.label = V(GEEL04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
GEEL04_SAM.clusterCoef <- transitivity(GEEL04_SAMTable, type="global") #cluster coefficient
GEEL04_SAM.degreeCent <- centralization.degree(GEEL04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_SAMftn <- as.network.matrix(GEEL04_SAMft)
GEEL04_SAM.netDensity <- network.density(GEEL04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_SAM.entropy <- entropy(GEEL04_SAMft) #entropy

GEEL04_SAM.netMx <- cbind(GEEL04_SAM.netMx, GEEL04_SAM.clusterCoef, GEEL04_SAM.degreeCent$centralization,
                          GEEL04_SAM.netDensity, GEEL04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************
#NA

round = 4
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
GEEL04_TAMg2 <- data.frame(GEEL04_TAM)
GEEL04_TAMg2 <- GEEL04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_TAMg2$player1
player2vector <- GEEL04_TAMg2$player2
GEEL04_TAMg3 <- GEEL04_TAMg2
GEEL04_TAMg3$p1inp2vec <- is.element(GEEL04_TAMg3$player1, player2vector)
GEEL04_TAMg3$p2inp1vec <- is.element(GEEL04_TAMg3$player2, player1vector)

addPlayer1 <- GEEL04_TAMg3[ which(GEEL04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_TAMg3[ which(GEEL04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_TAMg2 <- rbind(GEEL04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
GEEL04_TAMft <- ftable(GEEL04_TAMg2$player1, GEEL04_TAMg2$player2)
GEEL04_TAMft2 <- as.matrix(GEEL04_TAMft)
numRows <- nrow(GEEL04_TAMft2)
numCols <- ncol(GEEL04_TAMft2)
GEEL04_TAMft3 <- GEEL04_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL04_TAMTable <- graph.adjacency(GEEL04_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(GEEL04_TAMTable, vertex.label = V(GEEL04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
GEEL04_TAM.clusterCoef <- transitivity(GEEL04_TAMTable, type="global") #cluster coefficient
GEEL04_TAM.degreeCent <- centralization.degree(GEEL04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_TAMftn <- as.network.matrix(GEEL04_TAMft)
GEEL04_TAM.netDensity <- network.density(GEEL04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_TAM.entropy <- entropy(GEEL04_TAMft) #entropy

GEEL04_TAM.netMx <- cbind(GEEL04_TAM.netMx, GEEL04_TAM.clusterCoef, GEEL04_TAM.degreeCent$centralization,
                          GEEL04_TAM.netDensity, GEEL04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
GEEL04_SDMg2 <- data.frame(GEEL04_SDM)
GEEL04_SDMg2 <- GEEL04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_SDMg2$player1
player2vector <- GEEL04_SDMg2$player2
GEEL04_SDMg3 <- GEEL04_SDMg2
GEEL04_SDMg3$p1inp2vec <- is.element(GEEL04_SDMg3$player1, player2vector)
GEEL04_SDMg3$p2inp1vec <- is.element(GEEL04_SDMg3$player2, player1vector)

addPlayer1 <- GEEL04_SDMg3[ which(GEEL04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL04_SDMg3[ which(GEEL04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_SDMg2 <- rbind(GEEL04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
GEEL04_SDMft <- ftable(GEEL04_SDMg2$player1, GEEL04_SDMg2$player2)
GEEL04_SDMft2 <- as.matrix(GEEL04_SDMft)
numRows <- nrow(GEEL04_SDMft2)
numCols <- ncol(GEEL04_SDMft2)
GEEL04_SDMft3 <- GEEL04_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL04_SDMTable <- graph.adjacency(GEEL04_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(GEEL04_SDMTable, vertex.label = V(GEEL04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
GEEL04_SDM.clusterCoef <- transitivity(GEEL04_SDMTable, type="global") #cluster coefficient
GEEL04_SDM.degreeCent <- centralization.degree(GEEL04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_SDMftn <- as.network.matrix(GEEL04_SDMft)
GEEL04_SDM.netDensity <- network.density(GEEL04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_SDM.entropy <- entropy(GEEL04_SDMft) #entropy

GEEL04_SDM.netMx <- cbind(GEEL04_SDM.netMx, GEEL04_SDM.clusterCoef, GEEL04_SDM.degreeCent$centralization,
                          GEEL04_SDM.netDensity, GEEL04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
GEEL04_TDMg2 <- data.frame(GEEL04_TDM)
GEEL04_TDMg2 <- GEEL04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_TDMg2$player1
player2vector <- GEEL04_TDMg2$player2
GEEL04_TDMg3 <- GEEL04_TDMg2
GEEL04_TDMg3$p1inp2vec <- is.element(GEEL04_TDMg3$player1, player2vector)
GEEL04_TDMg3$p2inp1vec <- is.element(GEEL04_TDMg3$player2, player1vector)

addPlayer1 <- GEEL04_TDMg3[ which(GEEL04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_TDMg3[ which(GEEL04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_TDMg2 <- rbind(GEEL04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
GEEL04_TDMft <- ftable(GEEL04_TDMg2$player1, GEEL04_TDMg2$player2)
GEEL04_TDMft2 <- as.matrix(GEEL04_TDMft)
numRows <- nrow(GEEL04_TDMft2)
numCols <- ncol(GEEL04_TDMft2)
GEEL04_TDMft3 <- GEEL04_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL04_TDMTable <- graph.adjacency(GEEL04_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(GEEL04_TDMTable, vertex.label = V(GEEL04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
GEEL04_TDM.clusterCoef <- transitivity(GEEL04_TDMTable, type="global") #cluster coefficient
GEEL04_TDM.degreeCent <- centralization.degree(GEEL04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_TDMftn <- as.network.matrix(GEEL04_TDMft)
GEEL04_TDM.netDensity <- network.density(GEEL04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_TDM.entropy <- entropy(GEEL04_TDMft) #entropy

GEEL04_TDM.netMx <- cbind(GEEL04_TDM.netMx, GEEL04_TDM.clusterCoef, GEEL04_TDM.degreeCent$centralization,
                          GEEL04_TDM.netDensity, GEEL04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
GEEL04_SDg2 <- data.frame(GEEL04_SD)
GEEL04_SDg2 <- GEEL04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_SDg2$player1
player2vector <- GEEL04_SDg2$player2
GEEL04_SDg3 <- GEEL04_SDg2
GEEL04_SDg3$p1inp2vec <- is.element(GEEL04_SDg3$player1, player2vector)
GEEL04_SDg3$p2inp1vec <- is.element(GEEL04_SDg3$player2, player1vector)

addPlayer1 <- GEEL04_SDg3[ which(GEEL04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_SDg3[ which(GEEL04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_SDg2 <- rbind(GEEL04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
GEEL04_SDft <- ftable(GEEL04_SDg2$player1, GEEL04_SDg2$player2)
GEEL04_SDft2 <- as.matrix(GEEL04_SDft)
numRows <- nrow(GEEL04_SDft2)
numCols <- ncol(GEEL04_SDft2)
GEEL04_SDft3 <- GEEL04_SDft2[c(2:numRows) , c(2:numCols)]
GEEL04_SDTable <- graph.adjacency(GEEL04_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(GEEL04_SDTable, vertex.label = V(GEEL04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
GEEL04_SD.clusterCoef <- transitivity(GEEL04_SDTable, type="global") #cluster coefficient
GEEL04_SD.degreeCent <- centralization.degree(GEEL04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_SDftn <- as.network.matrix(GEEL04_SDft)
GEEL04_SD.netDensity <- network.density(GEEL04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_SD.entropy <- entropy(GEEL04_SDft) #entropy

GEEL04_SD.netMx <- cbind(GEEL04_SD.netMx, GEEL04_SD.clusterCoef, GEEL04_SD.degreeCent$centralization,
                         GEEL04_SD.netDensity, GEEL04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************

round = 4
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
GEEL04_TDg2 <- data.frame(GEEL04_TD)
GEEL04_TDg2 <- GEEL04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_TDg2$player1
player2vector <- GEEL04_TDg2$player2
GEEL04_TDg3 <- GEEL04_TDg2
GEEL04_TDg3$p1inp2vec <- is.element(GEEL04_TDg3$player1, player2vector)
GEEL04_TDg3$p2inp1vec <- is.element(GEEL04_TDg3$player2, player1vector)

addPlayer1 <- GEEL04_TDg3[ which(GEEL04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_TDg3[ which(GEEL04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_TDg2 <- rbind(GEEL04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
GEEL04_TDft <- ftable(GEEL04_TDg2$player1, GEEL04_TDg2$player2)
GEEL04_TDft2 <- as.matrix(GEEL04_TDft)
numRows <- nrow(GEEL04_TDft2)
numCols <- ncol(GEEL04_TDft2)
GEEL04_TDft3 <- GEEL04_TDft2[c(2:numRows) , c(2:numCols)]
GEEL04_TDTable <- graph.adjacency(GEEL04_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(GEEL04_TDTable, vertex.label = V(GEEL04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
GEEL04_TD.clusterCoef <- transitivity(GEEL04_TDTable, type="global") #cluster coefficient
GEEL04_TD.degreeCent <- centralization.degree(GEEL04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_TDftn <- as.network.matrix(GEEL04_TDft)
GEEL04_TD.netDensity <- network.density(GEEL04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_TD.entropy <- entropy(GEEL04_TDft) #entropy

GEEL04_TD.netMx <- cbind(GEEL04_TD.netMx, GEEL04_TD.clusterCoef, GEEL04_TD.degreeCent$centralization,
                         GEEL04_TD.netDensity, GEEL04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
GEEL04_QTg2 <- data.frame(GEEL04_QT)
GEEL04_QTg2 <- GEEL04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL04_QTg2$player1
player2vector <- GEEL04_QTg2$player2
GEEL04_QTg3 <- GEEL04_QTg2
GEEL04_QTg3$p1inp2vec <- is.element(GEEL04_QTg3$player1, player2vector)
GEEL04_QTg3$p2inp1vec <- is.element(GEEL04_QTg3$player2, player1vector)

addPlayer1 <- GEEL04_QTg3[ which(GEEL04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL04_QTg3[ which(GEEL04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL04_QTg2 <- rbind(GEEL04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
GEEL04_QTft <- ftable(GEEL04_QTg2$player1, GEEL04_QTg2$player2)
GEEL04_QTft2 <- as.matrix(GEEL04_QTft)
numRows <- nrow(GEEL04_QTft2)
numCols <- ncol(GEEL04_QTft2)
GEEL04_QTft3 <- GEEL04_QTft2[c(2:numRows) , c(2:numCols)]
GEEL04_QTTable <- graph.adjacency(GEEL04_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(GEEL04_QTTable, vertex.label = V(GEEL04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
GEEL04_QT.clusterCoef <- transitivity(GEEL04_QTTable, type="global") #cluster coefficient
GEEL04_QT.degreeCent <- centralization.degree(GEEL04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL04_QTftn <- as.network.matrix(GEEL04_QTft)
GEEL04_QT.netDensity <- network.density(GEEL04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL04_QT.entropy <- entropy(GEEL04_QTft) #entropy

GEEL04_QT.netMx <- cbind(GEEL04_QT.netMx, GEEL04_QT.clusterCoef, GEEL04_QT.degreeCent$centralization,
                         GEEL04_QT.netDensity, GEEL04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL04_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************

round = 4
teamName = "GWS"
KIoutcome = "Goal_F"
GWS04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
GWS04_Gg2 <- data.frame(GWS04_G)
GWS04_Gg2 <- GWS04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_Gg2$player1
player2vector <- GWS04_Gg2$player2
GWS04_Gg3 <- GWS04_Gg2
GWS04_Gg3$p1inp2vec <- is.element(GWS04_Gg3$player1, player2vector)
GWS04_Gg3$p2inp1vec <- is.element(GWS04_Gg3$player2, player1vector)

addPlayer1 <- GWS04_Gg3[ which(GWS04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_Gg3[ which(GWS04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_Gg2 <- rbind(GWS04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
GWS04_Gft <- ftable(GWS04_Gg2$player1, GWS04_Gg2$player2)
GWS04_Gft2 <- as.matrix(GWS04_Gft)
numRows <- nrow(GWS04_Gft2)
numCols <- ncol(GWS04_Gft2)
GWS04_Gft3 <- GWS04_Gft2[c(2:numRows) , c(2:numCols)]
GWS04_GTable <- graph.adjacency(GWS04_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(GWS04_GTable, vertex.label = V(GWS04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
GWS04_G.clusterCoef <- transitivity(GWS04_GTable, type="global") #cluster coefficient
GWS04_G.degreeCent <- centralization.degree(GWS04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_Gftn <- as.network.matrix(GWS04_Gft)
GWS04_G.netDensity <- network.density(GWS04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_G.entropy <- entropy(GWS04_Gft) #entropy

GWS04_G.netMx <- cbind(GWS04_G.netMx, GWS04_G.clusterCoef, GWS04_G.degreeCent$centralization,
                       GWS04_G.netDensity, GWS04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************

round = 4
teamName = "GWS"
KIoutcome = "Behind_F"
GWS04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
GWS04_Bg2 <- data.frame(GWS04_B)
GWS04_Bg2 <- GWS04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_Bg2$player1
player2vector <- GWS04_Bg2$player2
GWS04_Bg3 <- GWS04_Bg2
GWS04_Bg3$p1inp2vec <- is.element(GWS04_Bg3$player1, player2vector)
GWS04_Bg3$p2inp1vec <- is.element(GWS04_Bg3$player2, player1vector)

addPlayer1 <- GWS04_Bg3[ which(GWS04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_Bg3[ which(GWS04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_Bg2 <- rbind(GWS04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
GWS04_Bft <- ftable(GWS04_Bg2$player1, GWS04_Bg2$player2)
GWS04_Bft2 <- as.matrix(GWS04_Bft)
numRows <- nrow(GWS04_Bft2)
numCols <- ncol(GWS04_Bft2)
GWS04_Bft3 <- GWS04_Bft2[c(2:numRows) , c(2:numCols)]
GWS04_BTable <- graph.adjacency(GWS04_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(GWS04_BTable, vertex.label = V(GWS04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
GWS04_B.clusterCoef <- transitivity(GWS04_BTable, type="global") #cluster coefficient
GWS04_B.degreeCent <- centralization.degree(GWS04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_Bftn <- as.network.matrix(GWS04_Bft)
GWS04_B.netDensity <- network.density(GWS04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_B.entropy <- entropy(GWS04_Bft) #entropy

GWS04_B.netMx <- cbind(GWS04_B.netMx, GWS04_B.clusterCoef, GWS04_B.degreeCent$centralization,
                       GWS04_B.netDensity, GWS04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************

round = 4
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
GWS04_SFg2 <- data.frame(GWS04_SF)
GWS04_SFg2 <- GWS04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_SFg2$player1
player2vector <- GWS04_SFg2$player2
GWS04_SFg3 <- GWS04_SFg2
GWS04_SFg3$p1inp2vec <- is.element(GWS04_SFg3$player1, player2vector)
GWS04_SFg3$p2inp1vec <- is.element(GWS04_SFg3$player2, player1vector)

addPlayer1 <- GWS04_SFg3[ which(GWS04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_SFg3[ which(GWS04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_SFg2 <- rbind(GWS04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
GWS04_SFft <- ftable(GWS04_SFg2$player1, GWS04_SFg2$player2)
GWS04_SFft2 <- as.matrix(GWS04_SFft)
numRows <- nrow(GWS04_SFft2)
numCols <- ncol(GWS04_SFft2)
GWS04_SFft3 <- GWS04_SFft2[c(2:numRows) , c(2:numCols)]
GWS04_SFTable <- graph.adjacency(GWS04_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(GWS04_SFTable, vertex.label = V(GWS04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
GWS04_SF.clusterCoef <- transitivity(GWS04_SFTable, type="global") #cluster coefficient
GWS04_SF.degreeCent <- centralization.degree(GWS04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_SFftn <- as.network.matrix(GWS04_SFft)
GWS04_SF.netDensity <- network.density(GWS04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_SF.entropy <- entropy(GWS04_SFft) #entropy

GWS04_SF.netMx <- cbind(GWS04_SF.netMx, GWS04_SF.clusterCoef, GWS04_SF.degreeCent$centralization,
                        GWS04_SF.netDensity, GWS04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
GWS04_TFg2 <- data.frame(GWS04_TF)
GWS04_TFg2 <- GWS04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_TFg2$player1
player2vector <- GWS04_TFg2$player2
GWS04_TFg3 <- GWS04_TFg2
GWS04_TFg3$p1inp2vec <- is.element(GWS04_TFg3$player1, player2vector)
GWS04_TFg3$p2inp1vec <- is.element(GWS04_TFg3$player2, player1vector)

addPlayer1 <- GWS04_TFg3[ which(GWS04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_TFg3[ which(GWS04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_TFg2 <- rbind(GWS04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
GWS04_TFft <- ftable(GWS04_TFg2$player1, GWS04_TFg2$player2)
GWS04_TFft2 <- as.matrix(GWS04_TFft)
numRows <- nrow(GWS04_TFft2)
numCols <- ncol(GWS04_TFft2)
GWS04_TFft3 <- GWS04_TFft2[c(2:numRows) , c(2:numCols)]
GWS04_TFTable <- graph.adjacency(GWS04_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(GWS04_TFTable, vertex.label = V(GWS04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
GWS04_TF.clusterCoef <- transitivity(GWS04_TFTable, type="global") #cluster coefficient
GWS04_TF.degreeCent <- centralization.degree(GWS04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_TFftn <- as.network.matrix(GWS04_TFft)
GWS04_TF.netDensity <- network.density(GWS04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_TF.entropy <- entropy(GWS04_TFft) #entropy

GWS04_TF.netMx <- cbind(GWS04_TF.netMx, GWS04_TF.clusterCoef, GWS04_TF.degreeCent$centralization,
                        GWS04_TF.netDensity, GWS04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
GWS04_SAMg2 <- data.frame(GWS04_SAM)
GWS04_SAMg2 <- GWS04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_SAMg2$player1
player2vector <- GWS04_SAMg2$player2
GWS04_SAMg3 <- GWS04_SAMg2
GWS04_SAMg3$p1inp2vec <- is.element(GWS04_SAMg3$player1, player2vector)
GWS04_SAMg3$p2inp1vec <- is.element(GWS04_SAMg3$player2, player1vector)

addPlayer1 <- GWS04_SAMg3[ which(GWS04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_SAMg3[ which(GWS04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_SAMg2 <- rbind(GWS04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
GWS04_SAMft <- ftable(GWS04_SAMg2$player1, GWS04_SAMg2$player2)
GWS04_SAMft2 <- as.matrix(GWS04_SAMft)
numRows <- nrow(GWS04_SAMft2)
numCols <- ncol(GWS04_SAMft2)
GWS04_SAMft3 <- GWS04_SAMft2[c(2:numRows) , c(2:numCols)]
GWS04_SAMTable <- graph.adjacency(GWS04_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(GWS04_SAMTable, vertex.label = V(GWS04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
GWS04_SAM.clusterCoef <- transitivity(GWS04_SAMTable, type="global") #cluster coefficient
GWS04_SAM.degreeCent <- centralization.degree(GWS04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_SAMftn <- as.network.matrix(GWS04_SAMft)
GWS04_SAM.netDensity <- network.density(GWS04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_SAM.entropy <- entropy(GWS04_SAMft) #entropy

GWS04_SAM.netMx <- cbind(GWS04_SAM.netMx, GWS04_SAM.clusterCoef, GWS04_SAM.degreeCent$centralization,
                         GWS04_SAM.netDensity, GWS04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
GWS04_TAMg2 <- data.frame(GWS04_TAM)
GWS04_TAMg2 <- GWS04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_TAMg2$player1
player2vector <- GWS04_TAMg2$player2
GWS04_TAMg3 <- GWS04_TAMg2
GWS04_TAMg3$p1inp2vec <- is.element(GWS04_TAMg3$player1, player2vector)
GWS04_TAMg3$p2inp1vec <- is.element(GWS04_TAMg3$player2, player1vector)

addPlayer1 <- GWS04_TAMg3[ which(GWS04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_TAMg3[ which(GWS04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_TAMg2 <- rbind(GWS04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
GWS04_TAMft <- ftable(GWS04_TAMg2$player1, GWS04_TAMg2$player2)
GWS04_TAMft2 <- as.matrix(GWS04_TAMft)
numRows <- nrow(GWS04_TAMft2)
numCols <- ncol(GWS04_TAMft2)
GWS04_TAMft3 <- GWS04_TAMft2[c(2:numRows) , c(2:numCols)]
GWS04_TAMTable <- graph.adjacency(GWS04_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(GWS04_TAMTable, vertex.label = V(GWS04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
GWS04_TAM.clusterCoef <- transitivity(GWS04_TAMTable, type="global") #cluster coefficient
GWS04_TAM.degreeCent <- centralization.degree(GWS04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_TAMftn <- as.network.matrix(GWS04_TAMft)
GWS04_TAM.netDensity <- network.density(GWS04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_TAM.entropy <- entropy(GWS04_TAMft) #entropy

GWS04_TAM.netMx <- cbind(GWS04_TAM.netMx, GWS04_TAM.clusterCoef, GWS04_TAM.degreeCent$centralization,
                         GWS04_TAM.netDensity, GWS04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
GWS04_SDMg2 <- data.frame(GWS04_SDM)
GWS04_SDMg2 <- GWS04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_SDMg2$player1
player2vector <- GWS04_SDMg2$player2
GWS04_SDMg3 <- GWS04_SDMg2
GWS04_SDMg3$p1inp2vec <- is.element(GWS04_SDMg3$player1, player2vector)
GWS04_SDMg3$p2inp1vec <- is.element(GWS04_SDMg3$player2, player1vector)

addPlayer1 <- GWS04_SDMg3[ which(GWS04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_SDMg3[ which(GWS04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_SDMg2 <- rbind(GWS04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
GWS04_SDMft <- ftable(GWS04_SDMg2$player1, GWS04_SDMg2$player2)
GWS04_SDMft2 <- as.matrix(GWS04_SDMft)
numRows <- nrow(GWS04_SDMft2)
numCols <- ncol(GWS04_SDMft2)
GWS04_SDMft3 <- GWS04_SDMft2[c(2:numRows) , c(2:numCols)]
GWS04_SDMTable <- graph.adjacency(GWS04_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(GWS04_SDMTable, vertex.label = V(GWS04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
GWS04_SDM.clusterCoef <- transitivity(GWS04_SDMTable, type="global") #cluster coefficient
GWS04_SDM.degreeCent <- centralization.degree(GWS04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_SDMftn <- as.network.matrix(GWS04_SDMft)
GWS04_SDM.netDensity <- network.density(GWS04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_SDM.entropy <- entropy(GWS04_SDMft) #entropy

GWS04_SDM.netMx <- cbind(GWS04_SDM.netMx, GWS04_SDM.clusterCoef, GWS04_SDM.degreeCent$centralization,
                         GWS04_SDM.netDensity, GWS04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************
#NA

round = 4
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
GWS04_TDMg2 <- data.frame(GWS04_TDM)
GWS04_TDMg2 <- GWS04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_TDMg2$player1
player2vector <- GWS04_TDMg2$player2
GWS04_TDMg3 <- GWS04_TDMg2
GWS04_TDMg3$p1inp2vec <- is.element(GWS04_TDMg3$player1, player2vector)
GWS04_TDMg3$p2inp1vec <- is.element(GWS04_TDMg3$player2, player1vector)

addPlayer1 <- GWS04_TDMg3[ which(GWS04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_TDMg3[ which(GWS04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_TDMg2 <- rbind(GWS04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
GWS04_TDMft <- ftable(GWS04_TDMg2$player1, GWS04_TDMg2$player2)
GWS04_TDMft2 <- as.matrix(GWS04_TDMft)
numRows <- nrow(GWS04_TDMft2)
numCols <- ncol(GWS04_TDMft2)
GWS04_TDMft3 <- GWS04_TDMft2[c(2:numRows) , c(2:numCols)]
GWS04_TDMTable <- graph.adjacency(GWS04_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(GWS04_TDMTable, vertex.label = V(GWS04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
GWS04_TDM.clusterCoef <- transitivity(GWS04_TDMTable, type="global") #cluster coefficient
GWS04_TDM.degreeCent <- centralization.degree(GWS04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_TDMftn <- as.network.matrix(GWS04_TDMft)
GWS04_TDM.netDensity <- network.density(GWS04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_TDM.entropy <- entropy(GWS04_TDMft) #entropy

GWS04_TDM.netMx <- cbind(GWS04_TDM.netMx, GWS04_TDM.clusterCoef, GWS04_TDM.degreeCent$centralization,
                         GWS04_TDM.netDensity, GWS04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
GWS04_SDg2 <- data.frame(GWS04_SD)
GWS04_SDg2 <- GWS04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_SDg2$player1
player2vector <- GWS04_SDg2$player2
GWS04_SDg3 <- GWS04_SDg2
GWS04_SDg3$p1inp2vec <- is.element(GWS04_SDg3$player1, player2vector)
GWS04_SDg3$p2inp1vec <- is.element(GWS04_SDg3$player2, player1vector)

addPlayer1 <- GWS04_SDg3[ which(GWS04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_SDg3[ which(GWS04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_SDg2 <- rbind(GWS04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
GWS04_SDft <- ftable(GWS04_SDg2$player1, GWS04_SDg2$player2)
GWS04_SDft2 <- as.matrix(GWS04_SDft)
numRows <- nrow(GWS04_SDft2)
numCols <- ncol(GWS04_SDft2)
GWS04_SDft3 <- GWS04_SDft2[c(2:numRows) , c(2:numCols)]
GWS04_SDTable <- graph.adjacency(GWS04_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(GWS04_SDTable, vertex.label = V(GWS04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
GWS04_SD.clusterCoef <- transitivity(GWS04_SDTable, type="global") #cluster coefficient
GWS04_SD.degreeCent <- centralization.degree(GWS04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_SDftn <- as.network.matrix(GWS04_SDft)
GWS04_SD.netDensity <- network.density(GWS04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_SD.entropy <- entropy(GWS04_SDft) #entropy

GWS04_SD.netMx <- cbind(GWS04_SD.netMx, GWS04_SD.clusterCoef, GWS04_SD.degreeCent$centralization,
                        GWS04_SD.netDensity, GWS04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
GWS04_TDg2 <- data.frame(GWS04_TD)
GWS04_TDg2 <- GWS04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_TDg2$player1
player2vector <- GWS04_TDg2$player2
GWS04_TDg3 <- GWS04_TDg2
GWS04_TDg3$p1inp2vec <- is.element(GWS04_TDg3$player1, player2vector)
GWS04_TDg3$p2inp1vec <- is.element(GWS04_TDg3$player2, player1vector)

addPlayer1 <- GWS04_TDg3[ which(GWS04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_TDg3[ which(GWS04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_TDg2 <- rbind(GWS04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
GWS04_TDft <- ftable(GWS04_TDg2$player1, GWS04_TDg2$player2)
GWS04_TDft2 <- as.matrix(GWS04_TDft)
numRows <- nrow(GWS04_TDft2)
numCols <- ncol(GWS04_TDft2)
GWS04_TDft3 <- GWS04_TDft2[c(2:numRows) , c(2:numCols)]
GWS04_TDTable <- graph.adjacency(GWS04_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(GWS04_TDTable, vertex.label = V(GWS04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
GWS04_TD.clusterCoef <- transitivity(GWS04_TDTable, type="global") #cluster coefficient
GWS04_TD.degreeCent <- centralization.degree(GWS04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_TDftn <- as.network.matrix(GWS04_TDft)
GWS04_TD.netDensity <- network.density(GWS04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_TD.entropy <- entropy(GWS04_TDft) #entropy

GWS04_TD.netMx <- cbind(GWS04_TD.netMx, GWS04_TD.clusterCoef, GWS04_TD.degreeCent$centralization,
                        GWS04_TD.netDensity, GWS04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
GWS04_QTg2 <- data.frame(GWS04_QT)
GWS04_QTg2 <- GWS04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS04_QTg2$player1
player2vector <- GWS04_QTg2$player2
GWS04_QTg3 <- GWS04_QTg2
GWS04_QTg3$p1inp2vec <- is.element(GWS04_QTg3$player1, player2vector)
GWS04_QTg3$p2inp1vec <- is.element(GWS04_QTg3$player2, player1vector)

addPlayer1 <- GWS04_QTg3[ which(GWS04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS04_QTg3[ which(GWS04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS04_QTg2 <- rbind(GWS04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
GWS04_QTft <- ftable(GWS04_QTg2$player1, GWS04_QTg2$player2)
GWS04_QTft2 <- as.matrix(GWS04_QTft)
numRows <- nrow(GWS04_QTft2)
numCols <- ncol(GWS04_QTft2)
GWS04_QTft3 <- GWS04_QTft2[c(2:numRows) , c(2:numCols)]
GWS04_QTTable <- graph.adjacency(GWS04_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(GWS04_QTTable, vertex.label = V(GWS04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
GWS04_QT.clusterCoef <- transitivity(GWS04_QTTable, type="global") #cluster coefficient
GWS04_QT.degreeCent <- centralization.degree(GWS04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS04_QTftn <- as.network.matrix(GWS04_QTft)
GWS04_QT.netDensity <- network.density(GWS04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS04_QT.entropy <- entropy(GWS04_QTft) #entropy

GWS04_QT.netMx <- cbind(GWS04_QT.netMx, GWS04_QT.clusterCoef, GWS04_QT.degreeCent$centralization,
                        GWS04_QT.netDensity, GWS04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS04_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "HAW"
KIoutcome = "Goal_F"
HAW04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
HAW04_Gg2 <- data.frame(HAW04_G)
HAW04_Gg2 <- HAW04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_Gg2$player1
player2vector <- HAW04_Gg2$player2
HAW04_Gg3 <- HAW04_Gg2
HAW04_Gg3$p1inp2vec <- is.element(HAW04_Gg3$player1, player2vector)
HAW04_Gg3$p2inp1vec <- is.element(HAW04_Gg3$player2, player1vector)

addPlayer1 <- HAW04_Gg3[ which(HAW04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW04_Gg3[ which(HAW04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_Gg2 <- rbind(HAW04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
HAW04_Gft <- ftable(HAW04_Gg2$player1, HAW04_Gg2$player2)
HAW04_Gft2 <- as.matrix(HAW04_Gft)
numRows <- nrow(HAW04_Gft2)
numCols <- ncol(HAW04_Gft2)
HAW04_Gft3 <- HAW04_Gft2[c(2:numRows) , c(2:numCols)]
HAW04_GTable <- graph.adjacency(HAW04_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(HAW04_GTable, vertex.label = V(HAW04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
HAW04_G.clusterCoef <- transitivity(HAW04_GTable, type="global") #cluster coefficient
HAW04_G.degreeCent <- centralization.degree(HAW04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_Gftn <- as.network.matrix(HAW04_Gft)
HAW04_G.netDensity <- network.density(HAW04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_G.entropy <- entropy(HAW04_Gft) #entropy

HAW04_G.netMx <- cbind(HAW04_G.netMx, HAW04_G.clusterCoef, HAW04_G.degreeCent$centralization,
                       HAW04_G.netDensity, HAW04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "HAW"
KIoutcome = "Behind_F"
HAW04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
HAW04_Bg2 <- data.frame(HAW04_B)
HAW04_Bg2 <- HAW04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_Bg2$player1
player2vector <- HAW04_Bg2$player2
HAW04_Bg3 <- HAW04_Bg2
HAW04_Bg3$p1inp2vec <- is.element(HAW04_Bg3$player1, player2vector)
HAW04_Bg3$p2inp1vec <- is.element(HAW04_Bg3$player2, player1vector)

addPlayer1 <- HAW04_Bg3[ which(HAW04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW04_Bg2 <- rbind(HAW04_Bg2, addPlayer1)

#ROUND 4, Behind graph using weighted edges
HAW04_Bft <- ftable(HAW04_Bg2$player1, HAW04_Bg2$player2)
HAW04_Bft2 <- as.matrix(HAW04_Bft)
numRows <- nrow(HAW04_Bft2)
numCols <- ncol(HAW04_Bft2)
HAW04_Bft3 <- HAW04_Bft2[c(2:numRows) , c(1:numCols)]
HAW04_BTable <- graph.adjacency(HAW04_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(HAW04_BTable, vertex.label = V(HAW04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
HAW04_B.clusterCoef <- transitivity(HAW04_BTable, type="global") #cluster coefficient
HAW04_B.degreeCent <- centralization.degree(HAW04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_Bftn <- as.network.matrix(HAW04_Bft)
HAW04_B.netDensity <- network.density(HAW04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_B.entropy <- entropy(HAW04_Bft) #entropy

HAW04_B.netMx <- cbind(HAW04_B.netMx, HAW04_B.clusterCoef, HAW04_B.degreeCent$centralization,
                       HAW04_B.netDensity, HAW04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
HAW04_SFg2 <- data.frame(HAW04_SF)
HAW04_SFg2 <- HAW04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_SFg2$player1
player2vector <- HAW04_SFg2$player2
HAW04_SFg3 <- HAW04_SFg2
HAW04_SFg3$p1inp2vec <- is.element(HAW04_SFg3$player1, player2vector)
HAW04_SFg3$p2inp1vec <- is.element(HAW04_SFg3$player2, player1vector)

addPlayer1 <- HAW04_SFg3[ which(HAW04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW04_SFg3[ which(HAW04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_SFg2 <- rbind(HAW04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
HAW04_SFft <- ftable(HAW04_SFg2$player1, HAW04_SFg2$player2)
HAW04_SFft2 <- as.matrix(HAW04_SFft)
numRows <- nrow(HAW04_SFft2)
numCols <- ncol(HAW04_SFft2)
HAW04_SFft3 <- HAW04_SFft2[c(2:numRows) , c(2:numCols)]
HAW04_SFTable <- graph.adjacency(HAW04_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(HAW04_SFTable, vertex.label = V(HAW04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
HAW04_SF.clusterCoef <- transitivity(HAW04_SFTable, type="global") #cluster coefficient
HAW04_SF.degreeCent <- centralization.degree(HAW04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_SFftn <- as.network.matrix(HAW04_SFft)
HAW04_SF.netDensity <- network.density(HAW04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_SF.entropy <- entropy(HAW04_SFft) #entropy

HAW04_SF.netMx <- cbind(HAW04_SF.netMx, HAW04_SF.clusterCoef, HAW04_SF.degreeCent$centralization,
                        HAW04_SF.netDensity, HAW04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
HAW04_TFg2 <- data.frame(HAW04_TF)
HAW04_TFg2 <- HAW04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_TFg2$player1
player2vector <- HAW04_TFg2$player2
HAW04_TFg3 <- HAW04_TFg2
HAW04_TFg3$p1inp2vec <- is.element(HAW04_TFg3$player1, player2vector)
HAW04_TFg3$p2inp1vec <- is.element(HAW04_TFg3$player2, player1vector)

addPlayer1 <- HAW04_TFg3[ which(HAW04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW04_TFg3[ which(HAW04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_TFg2 <- rbind(HAW04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
HAW04_TFft <- ftable(HAW04_TFg2$player1, HAW04_TFg2$player2)
HAW04_TFft2 <- as.matrix(HAW04_TFft)
numRows <- nrow(HAW04_TFft2)
numCols <- ncol(HAW04_TFft2)
HAW04_TFft3 <- HAW04_TFft2[c(2:numRows) , c(2:numCols)]
HAW04_TFTable <- graph.adjacency(HAW04_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(HAW04_TFTable, vertex.label = V(HAW04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
HAW04_TF.clusterCoef <- transitivity(HAW04_TFTable, type="global") #cluster coefficient
HAW04_TF.degreeCent <- centralization.degree(HAW04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_TFftn <- as.network.matrix(HAW04_TFft)
HAW04_TF.netDensity <- network.density(HAW04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_TF.entropy <- entropy(HAW04_TFft) #entropy

HAW04_TF.netMx <- cbind(HAW04_TF.netMx, HAW04_TF.clusterCoef, HAW04_TF.degreeCent$centralization,
                        HAW04_TF.netDensity, HAW04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************

round = 4
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
HAW04_SAMg2 <- data.frame(HAW04_SAM)
HAW04_SAMg2 <- HAW04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_SAMg2$player1
player2vector <- HAW04_SAMg2$player2
HAW04_SAMg3 <- HAW04_SAMg2
HAW04_SAMg3$p1inp2vec <- is.element(HAW04_SAMg3$player1, player2vector)
HAW04_SAMg3$p2inp1vec <- is.element(HAW04_SAMg3$player2, player1vector)

addPlayer1 <- HAW04_SAMg3[ which(HAW04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW04_SAMg3[ which(HAW04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_SAMg2 <- rbind(HAW04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
HAW04_SAMft <- ftable(HAW04_SAMg2$player1, HAW04_SAMg2$player2)
HAW04_SAMft2 <- as.matrix(HAW04_SAMft)
numRows <- nrow(HAW04_SAMft2)
numCols <- ncol(HAW04_SAMft2)
HAW04_SAMft3 <- HAW04_SAMft2[c(2:numRows) , c(2:numCols)]
HAW04_SAMTable <- graph.adjacency(HAW04_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(HAW04_SAMTable, vertex.label = V(HAW04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
HAW04_SAM.clusterCoef <- transitivity(HAW04_SAMTable, type="global") #cluster coefficient
HAW04_SAM.degreeCent <- centralization.degree(HAW04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_SAMftn <- as.network.matrix(HAW04_SAMft)
HAW04_SAM.netDensity <- network.density(HAW04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_SAM.entropy <- entropy(HAW04_SAMft) #entropy

HAW04_SAM.netMx <- cbind(HAW04_SAM.netMx, HAW04_SAM.clusterCoef, HAW04_SAM.degreeCent$centralization,
                         HAW04_SAM.netDensity, HAW04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************
#NA

round = 4
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
HAW04_TAMg2 <- data.frame(HAW04_TAM)
HAW04_TAMg2 <- HAW04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_TAMg2$player1
player2vector <- HAW04_TAMg2$player2
HAW04_TAMg3 <- HAW04_TAMg2
HAW04_TAMg3$p1inp2vec <- is.element(HAW04_TAMg3$player1, player2vector)
HAW04_TAMg3$p2inp1vec <- is.element(HAW04_TAMg3$player2, player1vector)

addPlayer1 <- HAW04_TAMg3[ which(HAW04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW04_TAMg3[ which(HAW04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_TAMg2 <- rbind(HAW04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
HAW04_TAMft <- ftable(HAW04_TAMg2$player1, HAW04_TAMg2$player2)
HAW04_TAMft2 <- as.matrix(HAW04_TAMft)
numRows <- nrow(HAW04_TAMft2)
numCols <- ncol(HAW04_TAMft2)
HAW04_TAMft3 <- HAW04_TAMft2[c(2:numRows) , c(2:numCols)]
HAW04_TAMTable <- graph.adjacency(HAW04_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(HAW04_TAMTable, vertex.label = V(HAW04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
HAW04_TAM.clusterCoef <- transitivity(HAW04_TAMTable, type="global") #cluster coefficient
HAW04_TAM.degreeCent <- centralization.degree(HAW04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_TAMftn <- as.network.matrix(HAW04_TAMft)
HAW04_TAM.netDensity <- network.density(HAW04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_TAM.entropy <- entropy(HAW04_TAMft) #entropy

HAW04_TAM.netMx <- cbind(HAW04_TAM.netMx, HAW04_TAM.clusterCoef, HAW04_TAM.degreeCent$centralization,
                         HAW04_TAM.netDensity, HAW04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************
#NA

round = 4
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
HAW04_SDMg2 <- data.frame(HAW04_SDM)
HAW04_SDMg2 <- HAW04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_SDMg2$player1
player2vector <- HAW04_SDMg2$player2
HAW04_SDMg3 <- HAW04_SDMg2
HAW04_SDMg3$p1inp2vec <- is.element(HAW04_SDMg3$player1, player2vector)
HAW04_SDMg3$p2inp1vec <- is.element(HAW04_SDMg3$player2, player1vector)

addPlayer1 <- HAW04_SDMg3[ which(HAW04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW04_SDMg3[ which(HAW04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_SDMg2 <- rbind(HAW04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
HAW04_SDMft <- ftable(HAW04_SDMg2$player1, HAW04_SDMg2$player2)
HAW04_SDMft2 <- as.matrix(HAW04_SDMft)
numRows <- nrow(HAW04_SDMft2)
numCols <- ncol(HAW04_SDMft2)
HAW04_SDMft3 <- HAW04_SDMft2[c(2:numRows) , c(2:numCols)]
HAW04_SDMTable <- graph.adjacency(HAW04_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(HAW04_SDMTable, vertex.label = V(HAW04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
HAW04_SDM.clusterCoef <- transitivity(HAW04_SDMTable, type="global") #cluster coefficient
HAW04_SDM.degreeCent <- centralization.degree(HAW04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_SDMftn <- as.network.matrix(HAW04_SDMft)
HAW04_SDM.netDensity <- network.density(HAW04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_SDM.entropy <- entropy(HAW04_SDMft) #entropy

HAW04_SDM.netMx <- cbind(HAW04_SDM.netMx, HAW04_SDM.clusterCoef, HAW04_SDM.degreeCent$centralization,
                         HAW04_SDM.netDensity, HAW04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
HAW04_TDMg2 <- data.frame(HAW04_TDM)
HAW04_TDMg2 <- HAW04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_TDMg2$player1
player2vector <- HAW04_TDMg2$player2
HAW04_TDMg3 <- HAW04_TDMg2
HAW04_TDMg3$p1inp2vec <- is.element(HAW04_TDMg3$player1, player2vector)
HAW04_TDMg3$p2inp1vec <- is.element(HAW04_TDMg3$player2, player1vector)

addPlayer1 <- HAW04_TDMg3[ which(HAW04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW04_TDMg3[ which(HAW04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_TDMg2 <- rbind(HAW04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
HAW04_TDMft <- ftable(HAW04_TDMg2$player1, HAW04_TDMg2$player2)
HAW04_TDMft2 <- as.matrix(HAW04_TDMft)
numRows <- nrow(HAW04_TDMft2)
numCols <- ncol(HAW04_TDMft2)
HAW04_TDMft3 <- HAW04_TDMft2[c(2:numRows) , c(2:numCols)]
HAW04_TDMTable <- graph.adjacency(HAW04_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(HAW04_TDMTable, vertex.label = V(HAW04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
HAW04_TDM.clusterCoef <- transitivity(HAW04_TDMTable, type="global") #cluster coefficient
HAW04_TDM.degreeCent <- centralization.degree(HAW04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_TDMftn <- as.network.matrix(HAW04_TDMft)
HAW04_TDM.netDensity <- network.density(HAW04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_TDM.entropy <- entropy(HAW04_TDMft) #entropy

HAW04_TDM.netMx <- cbind(HAW04_TDM.netMx, HAW04_TDM.clusterCoef, HAW04_TDM.degreeCent$centralization,
                         HAW04_TDM.netDensity, HAW04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
HAW04_SDg2 <- data.frame(HAW04_SD)
HAW04_SDg2 <- HAW04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_SDg2$player1
player2vector <- HAW04_SDg2$player2
HAW04_SDg3 <- HAW04_SDg2
HAW04_SDg3$p1inp2vec <- is.element(HAW04_SDg3$player1, player2vector)
HAW04_SDg3$p2inp1vec <- is.element(HAW04_SDg3$player2, player1vector)

addPlayer1 <- HAW04_SDg3[ which(HAW04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW04_SDg3[ which(HAW04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_SDg2 <- rbind(HAW04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
HAW04_SDft <- ftable(HAW04_SDg2$player1, HAW04_SDg2$player2)
HAW04_SDft2 <- as.matrix(HAW04_SDft)
numRows <- nrow(HAW04_SDft2)
numCols <- ncol(HAW04_SDft2)
HAW04_SDft3 <- HAW04_SDft2[c(2:numRows) , c(2:numCols)]
HAW04_SDTable <- graph.adjacency(HAW04_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(HAW04_SDTable, vertex.label = V(HAW04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
HAW04_SD.clusterCoef <- transitivity(HAW04_SDTable, type="global") #cluster coefficient
HAW04_SD.degreeCent <- centralization.degree(HAW04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_SDftn <- as.network.matrix(HAW04_SDft)
HAW04_SD.netDensity <- network.density(HAW04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_SD.entropy <- entropy(HAW04_SDft) #entropy

HAW04_SD.netMx <- cbind(HAW04_SD.netMx, HAW04_SD.clusterCoef, HAW04_SD.degreeCent$centralization,
                        HAW04_SD.netDensity, HAW04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
HAW04_TDg2 <- data.frame(HAW04_TD)
HAW04_TDg2 <- HAW04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_TDg2$player1
player2vector <- HAW04_TDg2$player2
HAW04_TDg3 <- HAW04_TDg2
HAW04_TDg3$p1inp2vec <- is.element(HAW04_TDg3$player1, player2vector)
HAW04_TDg3$p2inp1vec <- is.element(HAW04_TDg3$player2, player1vector)

addPlayer1 <- HAW04_TDg3[ which(HAW04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW04_TDg3[ which(HAW04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_TDg2 <- rbind(HAW04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
HAW04_TDft <- ftable(HAW04_TDg2$player1, HAW04_TDg2$player2)
HAW04_TDft2 <- as.matrix(HAW04_TDft)
numRows <- nrow(HAW04_TDft2)
numCols <- ncol(HAW04_TDft2)
HAW04_TDft3 <- HAW04_TDft2[c(2:numRows) , c(2:numCols)]
HAW04_TDTable <- graph.adjacency(HAW04_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(HAW04_TDTable, vertex.label = V(HAW04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
HAW04_TD.clusterCoef <- transitivity(HAW04_TDTable, type="global") #cluster coefficient
HAW04_TD.degreeCent <- centralization.degree(HAW04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_TDftn <- as.network.matrix(HAW04_TDft)
HAW04_TD.netDensity <- network.density(HAW04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_TD.entropy <- entropy(HAW04_TDft) #entropy

HAW04_TD.netMx <- cbind(HAW04_TD.netMx, HAW04_TD.clusterCoef, HAW04_TD.degreeCent$centralization,
                        HAW04_TD.netDensity, HAW04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
HAW04_QTg2 <- data.frame(HAW04_QT)
HAW04_QTg2 <- HAW04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW04_QTg2$player1
player2vector <- HAW04_QTg2$player2
HAW04_QTg3 <- HAW04_QTg2
HAW04_QTg3$p1inp2vec <- is.element(HAW04_QTg3$player1, player2vector)
HAW04_QTg3$p2inp1vec <- is.element(HAW04_QTg3$player2, player1vector)

addPlayer1 <- HAW04_QTg3[ which(HAW04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW04_QTg3[ which(HAW04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW04_QTg2 <- rbind(HAW04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
HAW04_QTft <- ftable(HAW04_QTg2$player1, HAW04_QTg2$player2)
HAW04_QTft2 <- as.matrix(HAW04_QTft)
numRows <- nrow(HAW04_QTft2)
numCols <- ncol(HAW04_QTft2)
HAW04_QTft3 <- HAW04_QTft2[c(2:numRows) , c(2:numCols)]
HAW04_QTTable <- graph.adjacency(HAW04_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(HAW04_QTTable, vertex.label = V(HAW04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
HAW04_QT.clusterCoef <- transitivity(HAW04_QTTable, type="global") #cluster coefficient
HAW04_QT.degreeCent <- centralization.degree(HAW04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW04_QTftn <- as.network.matrix(HAW04_QTft)
HAW04_QT.netDensity <- network.density(HAW04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW04_QT.entropy <- entropy(HAW04_QTft) #entropy

HAW04_QT.netMx <- cbind(HAW04_QT.netMx, HAW04_QT.clusterCoef, HAW04_QT.degreeCent$centralization,
                        HAW04_QT.netDensity, HAW04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW04_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "MELB"
KIoutcome = "Goal_F"
MELB04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
MELB04_Gg2 <- data.frame(MELB04_G)
MELB04_Gg2 <- MELB04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_Gg2$player1
player2vector <- MELB04_Gg2$player2
MELB04_Gg3 <- MELB04_Gg2
MELB04_Gg3$p1inp2vec <- is.element(MELB04_Gg3$player1, player2vector)
MELB04_Gg3$p2inp1vec <- is.element(MELB04_Gg3$player2, player1vector)

addPlayer1 <- MELB04_Gg3[ which(MELB04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB04_Gg2 <- rbind(MELB04_Gg2, addPlayer1)

#ROUND 4, Goal graph using weighted edges
MELB04_Gft <- ftable(MELB04_Gg2$player1, MELB04_Gg2$player2)
MELB04_Gft2 <- as.matrix(MELB04_Gft)
numRows <- nrow(MELB04_Gft2)
numCols <- ncol(MELB04_Gft2)
MELB04_Gft3 <- MELB04_Gft2[c(2:numRows) , c(1:numCols)]
MELB04_GTable <- graph.adjacency(MELB04_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(MELB04_GTable, vertex.label = V(MELB04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
MELB04_G.clusterCoef <- transitivity(MELB04_GTable, type="global") #cluster coefficient
MELB04_G.degreeCent <- centralization.degree(MELB04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_Gftn <- as.network.matrix(MELB04_Gft)
MELB04_G.netDensity <- network.density(MELB04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_G.entropy <- entropy(MELB04_Gft) #entropy

MELB04_G.netMx <- cbind(MELB04_G.netMx, MELB04_G.clusterCoef, MELB04_G.degreeCent$centralization,
                        MELB04_G.netDensity, MELB04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "MELB"
KIoutcome = "Behind_F"
MELB04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
MELB04_Bg2 <- data.frame(MELB04_B)
MELB04_Bg2 <- MELB04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_Bg2$player1
player2vector <- MELB04_Bg2$player2
MELB04_Bg3 <- MELB04_Bg2
MELB04_Bg3$p1inp2vec <- is.element(MELB04_Bg3$player1, player2vector)
MELB04_Bg3$p2inp1vec <- is.element(MELB04_Bg3$player2, player1vector)

addPlayer1 <- MELB04_Bg3[ which(MELB04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_Bg3[ which(MELB04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_Bg2 <- rbind(MELB04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
MELB04_Bft <- ftable(MELB04_Bg2$player1, MELB04_Bg2$player2)
MELB04_Bft2 <- as.matrix(MELB04_Bft)
numRows <- nrow(MELB04_Bft2)
numCols <- ncol(MELB04_Bft2)
MELB04_Bft3 <- MELB04_Bft2[c(2:numRows) , c(2:numCols)]
MELB04_BTable <- graph.adjacency(MELB04_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(MELB04_BTable, vertex.label = V(MELB04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
MELB04_B.clusterCoef <- transitivity(MELB04_BTable, type="global") #cluster coefficient
MELB04_B.degreeCent <- centralization.degree(MELB04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_Bftn <- as.network.matrix(MELB04_Bft)
MELB04_B.netDensity <- network.density(MELB04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_B.entropy <- entropy(MELB04_Bft) #entropy

MELB04_B.netMx <- cbind(MELB04_B.netMx, MELB04_B.clusterCoef, MELB04_B.degreeCent$centralization,
                        MELB04_B.netDensity, MELB04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
MELB04_SFg2 <- data.frame(MELB04_SF)
MELB04_SFg2 <- MELB04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_SFg2$player1
player2vector <- MELB04_SFg2$player2
MELB04_SFg3 <- MELB04_SFg2
MELB04_SFg3$p1inp2vec <- is.element(MELB04_SFg3$player1, player2vector)
MELB04_SFg3$p2inp1vec <- is.element(MELB04_SFg3$player2, player1vector)

addPlayer1 <- MELB04_SFg3[ which(MELB04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_SFg3[ which(MELB04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_SFg2 <- rbind(MELB04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
MELB04_SFft <- ftable(MELB04_SFg2$player1, MELB04_SFg2$player2)
MELB04_SFft2 <- as.matrix(MELB04_SFft)
numRows <- nrow(MELB04_SFft2)
numCols <- ncol(MELB04_SFft2)
MELB04_SFft3 <- MELB04_SFft2[c(2:numRows) , c(2:numCols)]
MELB04_SFTable <- graph.adjacency(MELB04_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(MELB04_SFTable, vertex.label = V(MELB04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
MELB04_SF.clusterCoef <- transitivity(MELB04_SFTable, type="global") #cluster coefficient
MELB04_SF.degreeCent <- centralization.degree(MELB04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_SFftn <- as.network.matrix(MELB04_SFft)
MELB04_SF.netDensity <- network.density(MELB04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_SF.entropy <- entropy(MELB04_SFft) #entropy

MELB04_SF.netMx <- cbind(MELB04_SF.netMx, MELB04_SF.clusterCoef, MELB04_SF.degreeCent$centralization,
                         MELB04_SF.netDensity, MELB04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************
#NA

round = 4
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
MELB04_TFg2 <- data.frame(MELB04_TF)
MELB04_TFg2 <- MELB04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_TFg2$player1
player2vector <- MELB04_TFg2$player2
MELB04_TFg3 <- MELB04_TFg2
MELB04_TFg3$p1inp2vec <- is.element(MELB04_TFg3$player1, player2vector)
MELB04_TFg3$p2inp1vec <- is.element(MELB04_TFg3$player2, player1vector)

addPlayer1 <- MELB04_TFg3[ which(MELB04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_TFg3[ which(MELB04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_TFg2 <- rbind(MELB04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
MELB04_TFft <- ftable(MELB04_TFg2$player1, MELB04_TFg2$player2)
MELB04_TFft2 <- as.matrix(MELB04_TFft)
numRows <- nrow(MELB04_TFft2)
numCols <- ncol(MELB04_TFft2)
MELB04_TFft3 <- MELB04_TFft2[c(2:numRows) , c(2:numCols)]
MELB04_TFTable <- graph.adjacency(MELB04_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(MELB04_TFTable, vertex.label = V(MELB04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
MELB04_TF.clusterCoef <- transitivity(MELB04_TFTable, type="global") #cluster coefficient
MELB04_TF.degreeCent <- centralization.degree(MELB04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_TFftn <- as.network.matrix(MELB04_TFft)
MELB04_TF.netDensity <- network.density(MELB04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_TF.entropy <- entropy(MELB04_TFft) #entropy

MELB04_TF.netMx <- cbind(MELB04_TF.netMx, MELB04_TF.clusterCoef, MELB04_TF.degreeCent$centralization,
                         MELB04_TF.netDensity, MELB04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************

round = 4
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
MELB04_SAMg2 <- data.frame(MELB04_SAM)
MELB04_SAMg2 <- MELB04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_SAMg2$player1
player2vector <- MELB04_SAMg2$player2
MELB04_SAMg3 <- MELB04_SAMg2
MELB04_SAMg3$p1inp2vec <- is.element(MELB04_SAMg3$player1, player2vector)
MELB04_SAMg3$p2inp1vec <- is.element(MELB04_SAMg3$player2, player1vector)

addPlayer1 <- MELB04_SAMg3[ which(MELB04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_SAMg3[ which(MELB04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_SAMg2 <- rbind(MELB04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
MELB04_SAMft <- ftable(MELB04_SAMg2$player1, MELB04_SAMg2$player2)
MELB04_SAMft2 <- as.matrix(MELB04_SAMft)
numRows <- nrow(MELB04_SAMft2)
numCols <- ncol(MELB04_SAMft2)
MELB04_SAMft3 <- MELB04_SAMft2[c(2:numRows) , c(2:numCols)]
MELB04_SAMTable <- graph.adjacency(MELB04_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(MELB04_SAMTable, vertex.label = V(MELB04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
MELB04_SAM.clusterCoef <- transitivity(MELB04_SAMTable, type="global") #cluster coefficient
MELB04_SAM.degreeCent <- centralization.degree(MELB04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_SAMftn <- as.network.matrix(MELB04_SAMft)
MELB04_SAM.netDensity <- network.density(MELB04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_SAM.entropy <- entropy(MELB04_SAMft) #entropy

MELB04_SAM.netMx <- cbind(MELB04_SAM.netMx, MELB04_SAM.clusterCoef, MELB04_SAM.degreeCent$centralization,
                          MELB04_SAM.netDensity, MELB04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
MELB04_TAMg2 <- data.frame(MELB04_TAM)
MELB04_TAMg2 <- MELB04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_TAMg2$player1
player2vector <- MELB04_TAMg2$player2
MELB04_TAMg3 <- MELB04_TAMg2
MELB04_TAMg3$p1inp2vec <- is.element(MELB04_TAMg3$player1, player2vector)
MELB04_TAMg3$p2inp1vec <- is.element(MELB04_TAMg3$player2, player1vector)

addPlayer1 <- MELB04_TAMg3[ which(MELB04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_TAMg3[ which(MELB04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_TAMg2 <- rbind(MELB04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
MELB04_TAMft <- ftable(MELB04_TAMg2$player1, MELB04_TAMg2$player2)
MELB04_TAMft2 <- as.matrix(MELB04_TAMft)
numRows <- nrow(MELB04_TAMft2)
numCols <- ncol(MELB04_TAMft2)
MELB04_TAMft3 <- MELB04_TAMft2[c(2:numRows) , c(2:numCols)]
MELB04_TAMTable <- graph.adjacency(MELB04_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(MELB04_TAMTable, vertex.label = V(MELB04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
MELB04_TAM.clusterCoef <- transitivity(MELB04_TAMTable, type="global") #cluster coefficient
MELB04_TAM.degreeCent <- centralization.degree(MELB04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_TAMftn <- as.network.matrix(MELB04_TAMft)
MELB04_TAM.netDensity <- network.density(MELB04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_TAM.entropy <- entropy(MELB04_TAMft) #entropy

MELB04_TAM.netMx <- cbind(MELB04_TAM.netMx, MELB04_TAM.clusterCoef, MELB04_TAM.degreeCent$centralization,
                          MELB04_TAM.netDensity, MELB04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************
#NA

round = 4
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
MELB04_SDMg2 <- data.frame(MELB04_SDM)
MELB04_SDMg2 <- MELB04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_SDMg2$player1
player2vector <- MELB04_SDMg2$player2
MELB04_SDMg3 <- MELB04_SDMg2
MELB04_SDMg3$p1inp2vec <- is.element(MELB04_SDMg3$player1, player2vector)
MELB04_SDMg3$p2inp1vec <- is.element(MELB04_SDMg3$player2, player1vector)

addPlayer1 <- MELB04_SDMg3[ which(MELB04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_SDMg3[ which(MELB04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_SDMg2 <- rbind(MELB04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
MELB04_SDMft <- ftable(MELB04_SDMg2$player1, MELB04_SDMg2$player2)
MELB04_SDMft2 <- as.matrix(MELB04_SDMft)
numRows <- nrow(MELB04_SDMft2)
numCols <- ncol(MELB04_SDMft2)
MELB04_SDMft3 <- MELB04_SDMft2[c(2:numRows) , c(2:numCols)]
MELB04_SDMTable <- graph.adjacency(MELB04_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(MELB04_SDMTable, vertex.label = V(MELB04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
MELB04_SDM.clusterCoef <- transitivity(MELB04_SDMTable, type="global") #cluster coefficient
MELB04_SDM.degreeCent <- centralization.degree(MELB04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_SDMftn <- as.network.matrix(MELB04_SDMft)
MELB04_SDM.netDensity <- network.density(MELB04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_SDM.entropy <- entropy(MELB04_SDMft) #entropy

MELB04_SDM.netMx <- cbind(MELB04_SDM.netMx, MELB04_SDM.clusterCoef, MELB04_SDM.degreeCent$centralization,
                          MELB04_SDM.netDensity, MELB04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
MELB04_TDMg2 <- data.frame(MELB04_TDM)
MELB04_TDMg2 <- MELB04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_TDMg2$player1
player2vector <- MELB04_TDMg2$player2
MELB04_TDMg3 <- MELB04_TDMg2
MELB04_TDMg3$p1inp2vec <- is.element(MELB04_TDMg3$player1, player2vector)
MELB04_TDMg3$p2inp1vec <- is.element(MELB04_TDMg3$player2, player1vector)

addPlayer1 <- MELB04_TDMg3[ which(MELB04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_TDMg3[ which(MELB04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_TDMg2 <- rbind(MELB04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
MELB04_TDMft <- ftable(MELB04_TDMg2$player1, MELB04_TDMg2$player2)
MELB04_TDMft2 <- as.matrix(MELB04_TDMft)
numRows <- nrow(MELB04_TDMft2)
numCols <- ncol(MELB04_TDMft2)
MELB04_TDMft3 <- MELB04_TDMft2[c(2:numRows) , c(2:numCols)]
MELB04_TDMTable <- graph.adjacency(MELB04_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(MELB04_TDMTable, vertex.label = V(MELB04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
MELB04_TDM.clusterCoef <- transitivity(MELB04_TDMTable, type="global") #cluster coefficient
MELB04_TDM.degreeCent <- centralization.degree(MELB04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_TDMftn <- as.network.matrix(MELB04_TDMft)
MELB04_TDM.netDensity <- network.density(MELB04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_TDM.entropy <- entropy(MELB04_TDMft) #entropy

MELB04_TDM.netMx <- cbind(MELB04_TDM.netMx, MELB04_TDM.clusterCoef, MELB04_TDM.degreeCent$centralization,
                          MELB04_TDM.netDensity, MELB04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
MELB04_SDg2 <- data.frame(MELB04_SD)
MELB04_SDg2 <- MELB04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_SDg2$player1
player2vector <- MELB04_SDg2$player2
MELB04_SDg3 <- MELB04_SDg2
MELB04_SDg3$p1inp2vec <- is.element(MELB04_SDg3$player1, player2vector)
MELB04_SDg3$p2inp1vec <- is.element(MELB04_SDg3$player2, player1vector)

addPlayer1 <- MELB04_SDg3[ which(MELB04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_SDg3[ which(MELB04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_SDg2 <- rbind(MELB04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
MELB04_SDft <- ftable(MELB04_SDg2$player1, MELB04_SDg2$player2)
MELB04_SDft2 <- as.matrix(MELB04_SDft)
numRows <- nrow(MELB04_SDft2)
numCols <- ncol(MELB04_SDft2)
MELB04_SDft3 <- MELB04_SDft2[c(2:numRows) , c(2:numCols)]
MELB04_SDTable <- graph.adjacency(MELB04_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(MELB04_SDTable, vertex.label = V(MELB04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
MELB04_SD.clusterCoef <- transitivity(MELB04_SDTable, type="global") #cluster coefficient
MELB04_SD.degreeCent <- centralization.degree(MELB04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_SDftn <- as.network.matrix(MELB04_SDft)
MELB04_SD.netDensity <- network.density(MELB04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_SD.entropy <- entropy(MELB04_SDft) #entropy

MELB04_SD.netMx <- cbind(MELB04_SD.netMx, MELB04_SD.clusterCoef, MELB04_SD.degreeCent$centralization,
                         MELB04_SD.netDensity, MELB04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************

round = 4
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
MELB04_TDg2 <- data.frame(MELB04_TD)
MELB04_TDg2 <- MELB04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_TDg2$player1
player2vector <- MELB04_TDg2$player2
MELB04_TDg3 <- MELB04_TDg2
MELB04_TDg3$p1inp2vec <- is.element(MELB04_TDg3$player1, player2vector)
MELB04_TDg3$p2inp1vec <- is.element(MELB04_TDg3$player2, player1vector)

addPlayer1 <- MELB04_TDg3[ which(MELB04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_TDg3[ which(MELB04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_TDg2 <- rbind(MELB04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
MELB04_TDft <- ftable(MELB04_TDg2$player1, MELB04_TDg2$player2)
MELB04_TDft2 <- as.matrix(MELB04_TDft)
numRows <- nrow(MELB04_TDft2)
numCols <- ncol(MELB04_TDft2)
MELB04_TDft3 <- MELB04_TDft2[c(2:numRows) , c(2:numCols)]
MELB04_TDTable <- graph.adjacency(MELB04_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(MELB04_TDTable, vertex.label = V(MELB04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
MELB04_TD.clusterCoef <- transitivity(MELB04_TDTable, type="global") #cluster coefficient
MELB04_TD.degreeCent <- centralization.degree(MELB04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_TDftn <- as.network.matrix(MELB04_TDft)
MELB04_TD.netDensity <- network.density(MELB04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_TD.entropy <- entropy(MELB04_TDft) #entropy

MELB04_TD.netMx <- cbind(MELB04_TD.netMx, MELB04_TD.clusterCoef, MELB04_TD.degreeCent$centralization,
                         MELB04_TD.netDensity, MELB04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
MELB04_QTg2 <- data.frame(MELB04_QT)
MELB04_QTg2 <- MELB04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB04_QTg2$player1
player2vector <- MELB04_QTg2$player2
MELB04_QTg3 <- MELB04_QTg2
MELB04_QTg3$p1inp2vec <- is.element(MELB04_QTg3$player1, player2vector)
MELB04_QTg3$p2inp1vec <- is.element(MELB04_QTg3$player2, player1vector)

addPlayer1 <- MELB04_QTg3[ which(MELB04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB04_QTg3[ which(MELB04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB04_QTg2 <- rbind(MELB04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
MELB04_QTft <- ftable(MELB04_QTg2$player1, MELB04_QTg2$player2)
MELB04_QTft2 <- as.matrix(MELB04_QTft)
numRows <- nrow(MELB04_QTft2)
numCols <- ncol(MELB04_QTft2)
MELB04_QTft3 <- MELB04_QTft2[c(2:numRows) , c(2:numCols)]
MELB04_QTTable <- graph.adjacency(MELB04_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(MELB04_QTTable, vertex.label = V(MELB04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
MELB04_QT.clusterCoef <- transitivity(MELB04_QTTable, type="global") #cluster coefficient
MELB04_QT.degreeCent <- centralization.degree(MELB04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB04_QTftn <- as.network.matrix(MELB04_QTft)
MELB04_QT.netDensity <- network.density(MELB04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB04_QT.entropy <- entropy(MELB04_QTft) #entropy

MELB04_QT.netMx <- cbind(MELB04_QT.netMx, MELB04_QT.clusterCoef, MELB04_QT.degreeCent$centralization,
                         MELB04_QT.netDensity, MELB04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB04_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************

round = 4
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
NMFC04_Gg2 <- data.frame(NMFC04_G)
NMFC04_Gg2 <- NMFC04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_Gg2$player1
player2vector <- NMFC04_Gg2$player2
NMFC04_Gg3 <- NMFC04_Gg2
NMFC04_Gg3$p1inp2vec <- is.element(NMFC04_Gg3$player1, player2vector)
NMFC04_Gg3$p2inp1vec <- is.element(NMFC04_Gg3$player2, player1vector)

addPlayer1 <- NMFC04_Gg3[ which(NMFC04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_Gg3[ which(NMFC04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_Gg2 <- rbind(NMFC04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
NMFC04_Gft <- ftable(NMFC04_Gg2$player1, NMFC04_Gg2$player2)
NMFC04_Gft2 <- as.matrix(NMFC04_Gft)
numRows <- nrow(NMFC04_Gft2)
numCols <- ncol(NMFC04_Gft2)
NMFC04_Gft3 <- NMFC04_Gft2[c(2:numRows) , c(2:numCols)]
NMFC04_GTable <- graph.adjacency(NMFC04_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(NMFC04_GTable, vertex.label = V(NMFC04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
NMFC04_G.clusterCoef <- transitivity(NMFC04_GTable, type="global") #cluster coefficient
NMFC04_G.degreeCent <- centralization.degree(NMFC04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_Gftn <- as.network.matrix(NMFC04_Gft)
NMFC04_G.netDensity <- network.density(NMFC04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_G.entropy <- entropy(NMFC04_Gft) #entropy

NMFC04_G.netMx <- cbind(NMFC04_G.netMx, NMFC04_G.clusterCoef, NMFC04_G.degreeCent$centralization,
                        NMFC04_G.netDensity, NMFC04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
NMFC04_Bg2 <- data.frame(NMFC04_B)
NMFC04_Bg2 <- NMFC04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_Bg2$player1
player2vector <- NMFC04_Bg2$player2
NMFC04_Bg3 <- NMFC04_Bg2
NMFC04_Bg3$p1inp2vec <- is.element(NMFC04_Bg3$player1, player2vector)
NMFC04_Bg3$p2inp1vec <- is.element(NMFC04_Bg3$player2, player1vector)

addPlayer1 <- NMFC04_Bg3[ which(NMFC04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_Bg3[ which(NMFC04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_Bg2 <- rbind(NMFC04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
NMFC04_Bft <- ftable(NMFC04_Bg2$player1, NMFC04_Bg2$player2)
NMFC04_Bft2 <- as.matrix(NMFC04_Bft)
numRows <- nrow(NMFC04_Bft2)
numCols <- ncol(NMFC04_Bft2)
NMFC04_Bft3 <- NMFC04_Bft2[c(2:numRows) , c(2:numCols)]
NMFC04_BTable <- graph.adjacency(NMFC04_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(NMFC04_BTable, vertex.label = V(NMFC04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
NMFC04_B.clusterCoef <- transitivity(NMFC04_BTable, type="global") #cluster coefficient
NMFC04_B.degreeCent <- centralization.degree(NMFC04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_Bftn <- as.network.matrix(NMFC04_Bft)
NMFC04_B.netDensity <- network.density(NMFC04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_B.entropy <- entropy(NMFC04_Bft) #entropy

NMFC04_B.netMx <- cbind(NMFC04_B.netMx, NMFC04_B.clusterCoef, NMFC04_B.degreeCent$centralization,
                        NMFC04_B.netDensity, NMFC04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
NMFC04_SFg2 <- data.frame(NMFC04_SF)
NMFC04_SFg2 <- NMFC04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_SFg2$player1
player2vector <- NMFC04_SFg2$player2
NMFC04_SFg3 <- NMFC04_SFg2
NMFC04_SFg3$p1inp2vec <- is.element(NMFC04_SFg3$player1, player2vector)
NMFC04_SFg3$p2inp1vec <- is.element(NMFC04_SFg3$player2, player1vector)

addPlayer1 <- NMFC04_SFg3[ which(NMFC04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_SFg3[ which(NMFC04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_SFg2 <- rbind(NMFC04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
NMFC04_SFft <- ftable(NMFC04_SFg2$player1, NMFC04_SFg2$player2)
NMFC04_SFft2 <- as.matrix(NMFC04_SFft)
numRows <- nrow(NMFC04_SFft2)
numCols <- ncol(NMFC04_SFft2)
NMFC04_SFft3 <- NMFC04_SFft2[c(2:numRows) , c(2:numCols)]
NMFC04_SFTable <- graph.adjacency(NMFC04_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(NMFC04_SFTable, vertex.label = V(NMFC04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
NMFC04_SF.clusterCoef <- transitivity(NMFC04_SFTable, type="global") #cluster coefficient
NMFC04_SF.degreeCent <- centralization.degree(NMFC04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_SFftn <- as.network.matrix(NMFC04_SFft)
NMFC04_SF.netDensity <- network.density(NMFC04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_SF.entropy <- entropy(NMFC04_SFft) #entropy

NMFC04_SF.netMx <- cbind(NMFC04_SF.netMx, NMFC04_SF.clusterCoef, NMFC04_SF.degreeCent$centralization,
                         NMFC04_SF.netDensity, NMFC04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
NMFC04_TFg2 <- data.frame(NMFC04_TF)
NMFC04_TFg2 <- NMFC04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_TFg2$player1
player2vector <- NMFC04_TFg2$player2
NMFC04_TFg3 <- NMFC04_TFg2
NMFC04_TFg3$p1inp2vec <- is.element(NMFC04_TFg3$player1, player2vector)
NMFC04_TFg3$p2inp1vec <- is.element(NMFC04_TFg3$player2, player1vector)

addPlayer1 <- NMFC04_TFg3[ which(NMFC04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_TFg3[ which(NMFC04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_TFg2 <- rbind(NMFC04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
NMFC04_TFft <- ftable(NMFC04_TFg2$player1, NMFC04_TFg2$player2)
NMFC04_TFft2 <- as.matrix(NMFC04_TFft)
numRows <- nrow(NMFC04_TFft2)
numCols <- ncol(NMFC04_TFft2)
NMFC04_TFft3 <- NMFC04_TFft2[c(2:numRows) , c(2:numCols)]
NMFC04_TFTable <- graph.adjacency(NMFC04_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(NMFC04_TFTable, vertex.label = V(NMFC04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
NMFC04_TF.clusterCoef <- transitivity(NMFC04_TFTable, type="global") #cluster coefficient
NMFC04_TF.degreeCent <- centralization.degree(NMFC04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_TFftn <- as.network.matrix(NMFC04_TFft)
NMFC04_TF.netDensity <- network.density(NMFC04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_TF.entropy <- entropy(NMFC04_TFft) #entropy

NMFC04_TF.netMx <- cbind(NMFC04_TF.netMx, NMFC04_TF.clusterCoef, NMFC04_TF.degreeCent$centralization,
                         NMFC04_TF.netDensity, NMFC04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
NMFC04_SAMg2 <- data.frame(NMFC04_SAM)
NMFC04_SAMg2 <- NMFC04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_SAMg2$player1
player2vector <- NMFC04_SAMg2$player2
NMFC04_SAMg3 <- NMFC04_SAMg2
NMFC04_SAMg3$p1inp2vec <- is.element(NMFC04_SAMg3$player1, player2vector)
NMFC04_SAMg3$p2inp1vec <- is.element(NMFC04_SAMg3$player2, player1vector)

addPlayer1 <- NMFC04_SAMg3[ which(NMFC04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_SAMg3[ which(NMFC04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_SAMg2 <- rbind(NMFC04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
NMFC04_SAMft <- ftable(NMFC04_SAMg2$player1, NMFC04_SAMg2$player2)
NMFC04_SAMft2 <- as.matrix(NMFC04_SAMft)
numRows <- nrow(NMFC04_SAMft2)
numCols <- ncol(NMFC04_SAMft2)
NMFC04_SAMft3 <- NMFC04_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC04_SAMTable <- graph.adjacency(NMFC04_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(NMFC04_SAMTable, vertex.label = V(NMFC04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
NMFC04_SAM.clusterCoef <- transitivity(NMFC04_SAMTable, type="global") #cluster coefficient
NMFC04_SAM.degreeCent <- centralization.degree(NMFC04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_SAMftn <- as.network.matrix(NMFC04_SAMft)
NMFC04_SAM.netDensity <- network.density(NMFC04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_SAM.entropy <- entropy(NMFC04_SAMft) #entropy

NMFC04_SAM.netMx <- cbind(NMFC04_SAM.netMx, NMFC04_SAM.clusterCoef, NMFC04_SAM.degreeCent$centralization,
                          NMFC04_SAM.netDensity, NMFC04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
NMFC04_TAMg2 <- data.frame(NMFC04_TAM)
NMFC04_TAMg2 <- NMFC04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_TAMg2$player1
player2vector <- NMFC04_TAMg2$player2
NMFC04_TAMg3 <- NMFC04_TAMg2
NMFC04_TAMg3$p1inp2vec <- is.element(NMFC04_TAMg3$player1, player2vector)
NMFC04_TAMg3$p2inp1vec <- is.element(NMFC04_TAMg3$player2, player1vector)

addPlayer1 <- NMFC04_TAMg3[ which(NMFC04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC04_TAMg3[ which(NMFC04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_TAMg2 <- rbind(NMFC04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
NMFC04_TAMft <- ftable(NMFC04_TAMg2$player1, NMFC04_TAMg2$player2)
NMFC04_TAMft2 <- as.matrix(NMFC04_TAMft)
numRows <- nrow(NMFC04_TAMft2)
numCols <- ncol(NMFC04_TAMft2)
NMFC04_TAMft3 <- NMFC04_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC04_TAMTable <- graph.adjacency(NMFC04_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(NMFC04_TAMTable, vertex.label = V(NMFC04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
NMFC04_TAM.clusterCoef <- transitivity(NMFC04_TAMTable, type="global") #cluster coefficient
NMFC04_TAM.degreeCent <- centralization.degree(NMFC04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_TAMftn <- as.network.matrix(NMFC04_TAMft)
NMFC04_TAM.netDensity <- network.density(NMFC04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_TAM.entropy <- entropy(NMFC04_TAMft) #entropy

NMFC04_TAM.netMx <- cbind(NMFC04_TAM.netMx, NMFC04_TAM.clusterCoef, NMFC04_TAM.degreeCent$centralization,
                          NMFC04_TAM.netDensity, NMFC04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************
#NA

round = 4
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
NMFC04_SDMg2 <- data.frame(NMFC04_SDM)
NMFC04_SDMg2 <- NMFC04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_SDMg2$player1
player2vector <- NMFC04_SDMg2$player2
NMFC04_SDMg3 <- NMFC04_SDMg2
NMFC04_SDMg3$p1inp2vec <- is.element(NMFC04_SDMg3$player1, player2vector)
NMFC04_SDMg3$p2inp1vec <- is.element(NMFC04_SDMg3$player2, player1vector)

addPlayer1 <- NMFC04_SDMg3[ which(NMFC04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_SDMg3[ which(NMFC04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_SDMg2 <- rbind(NMFC04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
NMFC04_SDMft <- ftable(NMFC04_SDMg2$player1, NMFC04_SDMg2$player2)
NMFC04_SDMft2 <- as.matrix(NMFC04_SDMft)
numRows <- nrow(NMFC04_SDMft2)
numCols <- ncol(NMFC04_SDMft2)
NMFC04_SDMft3 <- NMFC04_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC04_SDMTable <- graph.adjacency(NMFC04_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(NMFC04_SDMTable, vertex.label = V(NMFC04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
NMFC04_SDM.clusterCoef <- transitivity(NMFC04_SDMTable, type="global") #cluster coefficient
NMFC04_SDM.degreeCent <- centralization.degree(NMFC04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_SDMftn <- as.network.matrix(NMFC04_SDMft)
NMFC04_SDM.netDensity <- network.density(NMFC04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_SDM.entropy <- entropy(NMFC04_SDMft) #entropy

NMFC04_SDM.netMx <- cbind(NMFC04_SDM.netMx, NMFC04_SDM.clusterCoef, NMFC04_SDM.degreeCent$centralization,
                          NMFC04_SDM.netDensity, NMFC04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
NMFC04_TDMg2 <- data.frame(NMFC04_TDM)
NMFC04_TDMg2 <- NMFC04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_TDMg2$player1
player2vector <- NMFC04_TDMg2$player2
NMFC04_TDMg3 <- NMFC04_TDMg2
NMFC04_TDMg3$p1inp2vec <- is.element(NMFC04_TDMg3$player1, player2vector)
NMFC04_TDMg3$p2inp1vec <- is.element(NMFC04_TDMg3$player2, player1vector)

addPlayer1 <- NMFC04_TDMg3[ which(NMFC04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_TDMg3[ which(NMFC04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_TDMg2 <- rbind(NMFC04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
NMFC04_TDMft <- ftable(NMFC04_TDMg2$player1, NMFC04_TDMg2$player2)
NMFC04_TDMft2 <- as.matrix(NMFC04_TDMft)
numRows <- nrow(NMFC04_TDMft2)
numCols <- ncol(NMFC04_TDMft2)
NMFC04_TDMft3 <- NMFC04_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC04_TDMTable <- graph.adjacency(NMFC04_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(NMFC04_TDMTable, vertex.label = V(NMFC04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
NMFC04_TDM.clusterCoef <- transitivity(NMFC04_TDMTable, type="global") #cluster coefficient
NMFC04_TDM.degreeCent <- centralization.degree(NMFC04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_TDMftn <- as.network.matrix(NMFC04_TDMft)
NMFC04_TDM.netDensity <- network.density(NMFC04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_TDM.entropy <- entropy(NMFC04_TDMft) #entropy

NMFC04_TDM.netMx <- cbind(NMFC04_TDM.netMx, NMFC04_TDM.clusterCoef, NMFC04_TDM.degreeCent$centralization,
                          NMFC04_TDM.netDensity, NMFC04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
NMFC04_SDg2 <- data.frame(NMFC04_SD)
NMFC04_SDg2 <- NMFC04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_SDg2$player1
player2vector <- NMFC04_SDg2$player2
NMFC04_SDg3 <- NMFC04_SDg2
NMFC04_SDg3$p1inp2vec <- is.element(NMFC04_SDg3$player1, player2vector)
NMFC04_SDg3$p2inp1vec <- is.element(NMFC04_SDg3$player2, player1vector)

addPlayer1 <- NMFC04_SDg3[ which(NMFC04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_SDg3[ which(NMFC04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_SDg2 <- rbind(NMFC04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
NMFC04_SDft <- ftable(NMFC04_SDg2$player1, NMFC04_SDg2$player2)
NMFC04_SDft2 <- as.matrix(NMFC04_SDft)
numRows <- nrow(NMFC04_SDft2)
numCols <- ncol(NMFC04_SDft2)
NMFC04_SDft3 <- NMFC04_SDft2[c(2:numRows) , c(2:numCols)]
NMFC04_SDTable <- graph.adjacency(NMFC04_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(NMFC04_SDTable, vertex.label = V(NMFC04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
NMFC04_SD.clusterCoef <- transitivity(NMFC04_SDTable, type="global") #cluster coefficient
NMFC04_SD.degreeCent <- centralization.degree(NMFC04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_SDftn <- as.network.matrix(NMFC04_SDft)
NMFC04_SD.netDensity <- network.density(NMFC04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_SD.entropy <- entropy(NMFC04_SDft) #entropy

NMFC04_SD.netMx <- cbind(NMFC04_SD.netMx, NMFC04_SD.clusterCoef, NMFC04_SD.degreeCent$centralization,
                         NMFC04_SD.netDensity, NMFC04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
NMFC04_TDg2 <- data.frame(NMFC04_TD)
NMFC04_TDg2 <- NMFC04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_TDg2$player1
player2vector <- NMFC04_TDg2$player2
NMFC04_TDg3 <- NMFC04_TDg2
NMFC04_TDg3$p1inp2vec <- is.element(NMFC04_TDg3$player1, player2vector)
NMFC04_TDg3$p2inp1vec <- is.element(NMFC04_TDg3$player2, player1vector)

addPlayer1 <- NMFC04_TDg3[ which(NMFC04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_TDg3[ which(NMFC04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_TDg2 <- rbind(NMFC04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
NMFC04_TDft <- ftable(NMFC04_TDg2$player1, NMFC04_TDg2$player2)
NMFC04_TDft2 <- as.matrix(NMFC04_TDft)
numRows <- nrow(NMFC04_TDft2)
numCols <- ncol(NMFC04_TDft2)
NMFC04_TDft3 <- NMFC04_TDft2[c(2:numRows) , c(2:numCols)]
NMFC04_TDTable <- graph.adjacency(NMFC04_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(NMFC04_TDTable, vertex.label = V(NMFC04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
NMFC04_TD.clusterCoef <- transitivity(NMFC04_TDTable, type="global") #cluster coefficient
NMFC04_TD.degreeCent <- centralization.degree(NMFC04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_TDftn <- as.network.matrix(NMFC04_TDft)
NMFC04_TD.netDensity <- network.density(NMFC04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_TD.entropy <- entropy(NMFC04_TDft) #entropy

NMFC04_TD.netMx <- cbind(NMFC04_TD.netMx, NMFC04_TD.clusterCoef, NMFC04_TD.degreeCent$centralization,
                         NMFC04_TD.netDensity, NMFC04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************

round = 4
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
NMFC04_QTg2 <- data.frame(NMFC04_QT)
NMFC04_QTg2 <- NMFC04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC04_QTg2$player1
player2vector <- NMFC04_QTg2$player2
NMFC04_QTg3 <- NMFC04_QTg2
NMFC04_QTg3$p1inp2vec <- is.element(NMFC04_QTg3$player1, player2vector)
NMFC04_QTg3$p2inp1vec <- is.element(NMFC04_QTg3$player2, player1vector)

addPlayer1 <- NMFC04_QTg3[ which(NMFC04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC04_QTg3[ which(NMFC04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC04_QTg2 <- rbind(NMFC04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
NMFC04_QTft <- ftable(NMFC04_QTg2$player1, NMFC04_QTg2$player2)
NMFC04_QTft2 <- as.matrix(NMFC04_QTft)
numRows <- nrow(NMFC04_QTft2)
numCols <- ncol(NMFC04_QTft2)
NMFC04_QTft3 <- NMFC04_QTft2[c(2:numRows) , c(2:numCols)]
NMFC04_QTTable <- graph.adjacency(NMFC04_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(NMFC04_QTTable, vertex.label = V(NMFC04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
NMFC04_QT.clusterCoef <- transitivity(NMFC04_QTTable, type="global") #cluster coefficient
NMFC04_QT.degreeCent <- centralization.degree(NMFC04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC04_QTftn <- as.network.matrix(NMFC04_QTft)
NMFC04_QT.netDensity <- network.density(NMFC04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC04_QT.entropy <- entropy(NMFC04_QTft) #entropy

NMFC04_QT.netMx <- cbind(NMFC04_QT.netMx, NMFC04_QT.clusterCoef, NMFC04_QT.degreeCent$centralization,
                         NMFC04_QT.netDensity, NMFC04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC04_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************

round = 4
teamName = "PORT"
KIoutcome = "Goal_F"
PORT04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
PORT04_Gg2 <- data.frame(PORT04_G)
PORT04_Gg2 <- PORT04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_Gg2$player1
player2vector <- PORT04_Gg2$player2
PORT04_Gg3 <- PORT04_Gg2
PORT04_Gg3$p1inp2vec <- is.element(PORT04_Gg3$player1, player2vector)
PORT04_Gg3$p2inp1vec <- is.element(PORT04_Gg3$player2, player1vector)

addPlayer1 <- PORT04_Gg3[ which(PORT04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT04_Gg3[ which(PORT04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT04_Gg2 <- rbind(PORT04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
PORT04_Gft <- ftable(PORT04_Gg2$player1, PORT04_Gg2$player2)
PORT04_Gft2 <- as.matrix(PORT04_Gft)
numRows <- nrow(PORT04_Gft2)
numCols <- ncol(PORT04_Gft2)
PORT04_Gft3 <- PORT04_Gft2[c(2:numRows) , c(2:numCols)]
PORT04_GTable <- graph.adjacency(PORT04_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(PORT04_GTable, vertex.label = V(PORT04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
PORT04_G.clusterCoef <- transitivity(PORT04_GTable, type="global") #cluster coefficient
PORT04_G.degreeCent <- centralization.degree(PORT04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_Gftn <- as.network.matrix(PORT04_Gft)
PORT04_G.netDensity <- network.density(PORT04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_G.entropy <- entropy(PORT04_Gft) #entropy

PORT04_G.netMx <- cbind(PORT04_G.netMx, PORT04_G.clusterCoef, PORT04_G.degreeCent$centralization,
                        PORT04_G.netDensity, PORT04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "PORT"
KIoutcome = "Behind_F"
PORT04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
PORT04_Bg2 <- data.frame(PORT04_B)
PORT04_Bg2 <- PORT04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_Bg2$player1
player2vector <- PORT04_Bg2$player2
PORT04_Bg3 <- PORT04_Bg2
PORT04_Bg3$p1inp2vec <- is.element(PORT04_Bg3$player1, player2vector)
PORT04_Bg3$p2inp1vec <- is.element(PORT04_Bg3$player2, player1vector)

addPlayer1 <- PORT04_Bg3[ which(PORT04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT04_Bg3[ which(PORT04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT04_Bg2 <- rbind(PORT04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
PORT04_Bft <- ftable(PORT04_Bg2$player1, PORT04_Bg2$player2)
PORT04_Bft2 <- as.matrix(PORT04_Bft)
numRows <- nrow(PORT04_Bft2)
numCols <- ncol(PORT04_Bft2)
PORT04_Bft3 <- PORT04_Bft2[c(2:numRows) , c(2:numCols)]
PORT04_BTable <- graph.adjacency(PORT04_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(PORT04_BTable, vertex.label = V(PORT04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
PORT04_B.clusterCoef <- transitivity(PORT04_BTable, type="global") #cluster coefficient
PORT04_B.degreeCent <- centralization.degree(PORT04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_Bftn <- as.network.matrix(PORT04_Bft)
PORT04_B.netDensity <- network.density(PORT04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_B.entropy <- entropy(PORT04_Bft) #entropy

PORT04_B.netMx <- cbind(PORT04_B.netMx, PORT04_B.clusterCoef, PORT04_B.degreeCent$centralization,
                        PORT04_B.netDensity, PORT04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
PORT04_SFg2 <- data.frame(PORT04_SF)
PORT04_SFg2 <- PORT04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_SFg2$player1
player2vector <- PORT04_SFg2$player2
PORT04_SFg3 <- PORT04_SFg2
PORT04_SFg3$p1inp2vec <- is.element(PORT04_SFg3$player1, player2vector)
PORT04_SFg3$p2inp1vec <- is.element(PORT04_SFg3$player2, player1vector)

addPlayer1 <- PORT04_SFg3[ which(PORT04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT04_SFg3[ which(PORT04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT04_SFg2 <- rbind(PORT04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
PORT04_SFft <- ftable(PORT04_SFg2$player1, PORT04_SFg2$player2)
PORT04_SFft2 <- as.matrix(PORT04_SFft)
numRows <- nrow(PORT04_SFft2)
numCols <- ncol(PORT04_SFft2)
PORT04_SFft3 <- PORT04_SFft2[c(2:numRows) , c(2:numCols)]
PORT04_SFTable <- graph.adjacency(PORT04_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(PORT04_SFTable, vertex.label = V(PORT04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
PORT04_SF.clusterCoef <- transitivity(PORT04_SFTable, type="global") #cluster coefficient
PORT04_SF.degreeCent <- centralization.degree(PORT04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_SFftn <- as.network.matrix(PORT04_SFft)
PORT04_SF.netDensity <- network.density(PORT04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_SF.entropy <- entropy(PORT04_SFft) #entropy

PORT04_SF.netMx <- cbind(PORT04_SF.netMx, PORT04_SF.clusterCoef, PORT04_SF.degreeCent$centralization,
                         PORT04_SF.netDensity, PORT04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************
#NA

round = 4
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
PORT04_TFg2 <- data.frame(PORT04_TF)
PORT04_TFg2 <- PORT04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_TFg2$player1
player2vector <- PORT04_TFg2$player2
PORT04_TFg3 <- PORT04_TFg2
PORT04_TFg3$p1inp2vec <- is.element(PORT04_TFg3$player1, player2vector)
PORT04_TFg3$p2inp1vec <- is.element(PORT04_TFg3$player2, player1vector)

addPlayer1 <- PORT04_TFg3[ which(PORT04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT04_TFg2 <- rbind(PORT04_TFg2, addPlayer1)

#ROUND 4, FWD Turnover graph using weighted edges
PORT04_TFft <- ftable(PORT04_TFg2$player1, PORT04_TFg2$player2)
PORT04_TFft2 <- as.matrix(PORT04_TFft)
numRows <- nrow(PORT04_TFft2)
numCols <- ncol(PORT04_TFft2)
PORT04_TFft3 <- PORT04_TFft2[c(2:numRows) , c(1:numCols)]
PORT04_TFTable <- graph.adjacency(PORT04_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(PORT04_TFTable, vertex.label = V(PORT04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
PORT04_TF.clusterCoef <- transitivity(PORT04_TFTable, type="global") #cluster coefficient
PORT04_TF.degreeCent <- centralization.degree(PORT04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_TFftn <- as.network.matrix(PORT04_TFft)
PORT04_TF.netDensity <- network.density(PORT04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_TF.entropy <- entropy(PORT04_TFft) #entropy

PORT04_TF.netMx <- cbind(PORT04_TF.netMx, PORT04_TF.clusterCoef, PORT04_TF.degreeCent$centralization,
                         PORT04_TF.netDensity, PORT04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
PORT04_SAMg2 <- data.frame(PORT04_SAM)
PORT04_SAMg2 <- PORT04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_SAMg2$player1
player2vector <- PORT04_SAMg2$player2
PORT04_SAMg3 <- PORT04_SAMg2
PORT04_SAMg3$p1inp2vec <- is.element(PORT04_SAMg3$player1, player2vector)
PORT04_SAMg3$p2inp1vec <- is.element(PORT04_SAMg3$player2, player1vector)

addPlayer1 <- PORT04_SAMg3[ which(PORT04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT04_SAMg3[ which(PORT04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT04_SAMg2 <- rbind(PORT04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
PORT04_SAMft <- ftable(PORT04_SAMg2$player1, PORT04_SAMg2$player2)
PORT04_SAMft2 <- as.matrix(PORT04_SAMft)
numRows <- nrow(PORT04_SAMft2)
numCols <- ncol(PORT04_SAMft2)
PORT04_SAMft3 <- PORT04_SAMft2[c(2:numRows) , c(2:numCols)]
PORT04_SAMTable <- graph.adjacency(PORT04_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(PORT04_SAMTable, vertex.label = V(PORT04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
PORT04_SAM.clusterCoef <- transitivity(PORT04_SAMTable, type="global") #cluster coefficient
PORT04_SAM.degreeCent <- centralization.degree(PORT04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_SAMftn <- as.network.matrix(PORT04_SAMft)
PORT04_SAM.netDensity <- network.density(PORT04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_SAM.entropy <- entropy(PORT04_SAMft) #entropy

PORT04_SAM.netMx <- cbind(PORT04_SAM.netMx, PORT04_SAM.clusterCoef, PORT04_SAM.degreeCent$centralization,
                          PORT04_SAM.netDensity, PORT04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************
#NA

round = 4
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
PORT04_TAMg2 <- data.frame(PORT04_TAM)
PORT04_TAMg2 <- PORT04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_TAMg2$player1
player2vector <- PORT04_TAMg2$player2
PORT04_TAMg3 <- PORT04_TAMg2
PORT04_TAMg3$p1inp2vec <- is.element(PORT04_TAMg3$player1, player2vector)
PORT04_TAMg3$p2inp1vec <- is.element(PORT04_TAMg3$player2, player1vector)

addPlayer1 <- PORT04_TAMg3[ which(PORT04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT04_TAMg2 <- rbind(PORT04_TAMg2, addPlayer1)

#ROUND 4, AM Turnover graph using weighted edges
PORT04_TAMft <- ftable(PORT04_TAMg2$player1, PORT04_TAMg2$player2)
PORT04_TAMft2 <- as.matrix(PORT04_TAMft)
numRows <- nrow(PORT04_TAMft2)
numCols <- ncol(PORT04_TAMft2)
PORT04_TAMft3 <- PORT04_TAMft2[c(2:numRows) , c(1:numCols)]
PORT04_TAMTable <- graph.adjacency(PORT04_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(PORT04_TAMTable, vertex.label = V(PORT04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
PORT04_TAM.clusterCoef <- transitivity(PORT04_TAMTable, type="global") #cluster coefficient
PORT04_TAM.degreeCent <- centralization.degree(PORT04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_TAMftn <- as.network.matrix(PORT04_TAMft)
PORT04_TAM.netDensity <- network.density(PORT04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_TAM.entropy <- entropy(PORT04_TAMft) #entropy

PORT04_TAM.netMx <- cbind(PORT04_TAM.netMx, PORT04_TAM.clusterCoef, PORT04_TAM.degreeCent$centralization,
                          PORT04_TAM.netDensity, PORT04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
PORT04_SDMg2 <- data.frame(PORT04_SDM)
PORT04_SDMg2 <- PORT04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_SDMg2$player1
player2vector <- PORT04_SDMg2$player2
PORT04_SDMg3 <- PORT04_SDMg2
PORT04_SDMg3$p1inp2vec <- is.element(PORT04_SDMg3$player1, player2vector)
PORT04_SDMg3$p2inp1vec <- is.element(PORT04_SDMg3$player2, player1vector)

addPlayer1 <- PORT04_SDMg3[ which(PORT04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT04_SDMg3[ which(PORT04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT04_SDMg2 <- rbind(PORT04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
PORT04_SDMft <- ftable(PORT04_SDMg2$player1, PORT04_SDMg2$player2)
PORT04_SDMft2 <- as.matrix(PORT04_SDMft)
numRows <- nrow(PORT04_SDMft2)
numCols <- ncol(PORT04_SDMft2)
PORT04_SDMft3 <- PORT04_SDMft2[c(2:numRows) , c(2:numCols)]
PORT04_SDMTable <- graph.adjacency(PORT04_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(PORT04_SDMTable, vertex.label = V(PORT04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
PORT04_SDM.clusterCoef <- transitivity(PORT04_SDMTable, type="global") #cluster coefficient
PORT04_SDM.degreeCent <- centralization.degree(PORT04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_SDMftn <- as.network.matrix(PORT04_SDMft)
PORT04_SDM.netDensity <- network.density(PORT04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_SDM.entropy <- entropy(PORT04_SDMft) #entropy

PORT04_SDM.netMx <- cbind(PORT04_SDM.netMx, PORT04_SDM.clusterCoef, PORT04_SDM.degreeCent$centralization,
                          PORT04_SDM.netDensity, PORT04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
PORT04_TDMg2 <- data.frame(PORT04_TDM)
PORT04_TDMg2 <- PORT04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_TDMg2$player1
player2vector <- PORT04_TDMg2$player2
PORT04_TDMg3 <- PORT04_TDMg2
PORT04_TDMg3$p1inp2vec <- is.element(PORT04_TDMg3$player1, player2vector)
PORT04_TDMg3$p2inp1vec <- is.element(PORT04_TDMg3$player2, player1vector)

addPlayer1 <- PORT04_TDMg3[ which(PORT04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- PORT04_TDMg3[ which(PORT04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT04_TDMg2 <- rbind(PORT04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
PORT04_TDMft <- ftable(PORT04_TDMg2$player1, PORT04_TDMg2$player2)
PORT04_TDMft2 <- as.matrix(PORT04_TDMft)
numRows <- nrow(PORT04_TDMft2)
numCols <- ncol(PORT04_TDMft2)
PORT04_TDMft3 <- PORT04_TDMft2[c(2:numRows) , c(2:numCols)]
PORT04_TDMTable <- graph.adjacency(PORT04_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(PORT04_TDMTable, vertex.label = V(PORT04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
PORT04_TDM.clusterCoef <- transitivity(PORT04_TDMTable, type="global") #cluster coefficient
PORT04_TDM.degreeCent <- centralization.degree(PORT04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_TDMftn <- as.network.matrix(PORT04_TDMft)
PORT04_TDM.netDensity <- network.density(PORT04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_TDM.entropy <- entropy(PORT04_TDMft) #entropy

PORT04_TDM.netMx <- cbind(PORT04_TDM.netMx, PORT04_TDM.clusterCoef, PORT04_TDM.degreeCent$centralization,
                          PORT04_TDM.netDensity, PORT04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
PORT04_SDg2 <- data.frame(PORT04_SD)
PORT04_SDg2 <- PORT04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_SDg2$player1
player2vector <- PORT04_SDg2$player2
PORT04_SDg3 <- PORT04_SDg2
PORT04_SDg3$p1inp2vec <- is.element(PORT04_SDg3$player1, player2vector)
PORT04_SDg3$p2inp1vec <- is.element(PORT04_SDg3$player2, player1vector)

addPlayer1 <- PORT04_SDg3[ which(PORT04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT04_SDg3[ which(PORT04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT04_SDg2 <- rbind(PORT04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
PORT04_SDft <- ftable(PORT04_SDg2$player1, PORT04_SDg2$player2)
PORT04_SDft2 <- as.matrix(PORT04_SDft)
numRows <- nrow(PORT04_SDft2)
numCols <- ncol(PORT04_SDft2)
PORT04_SDft3 <- PORT04_SDft2[c(2:numRows) , c(2:numCols)]
PORT04_SDTable <- graph.adjacency(PORT04_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(PORT04_SDTable, vertex.label = V(PORT04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
PORT04_SD.clusterCoef <- transitivity(PORT04_SDTable, type="global") #cluster coefficient
PORT04_SD.degreeCent <- centralization.degree(PORT04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_SDftn <- as.network.matrix(PORT04_SDft)
PORT04_SD.netDensity <- network.density(PORT04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_SD.entropy <- entropy(PORT04_SDft) #entropy

PORT04_SD.netMx <- cbind(PORT04_SD.netMx, PORT04_SD.clusterCoef, PORT04_SD.degreeCent$centralization,
                         PORT04_SD.netDensity, PORT04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
PORT04_TDg2 <- data.frame(PORT04_TD)
PORT04_TDg2 <- PORT04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_TDg2$player1
player2vector <- PORT04_TDg2$player2
PORT04_TDg3 <- PORT04_TDg2
PORT04_TDg3$p1inp2vec <- is.element(PORT04_TDg3$player1, player2vector)
PORT04_TDg3$p2inp1vec <- is.element(PORT04_TDg3$player2, player1vector)

addPlayer1 <- PORT04_TDg3[ which(PORT04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT04_TDg3[ which(PORT04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT04_TDg2 <- rbind(PORT04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
PORT04_TDft <- ftable(PORT04_TDg2$player1, PORT04_TDg2$player2)
PORT04_TDft2 <- as.matrix(PORT04_TDft)
numRows <- nrow(PORT04_TDft2)
numCols <- ncol(PORT04_TDft2)
PORT04_TDft3 <- PORT04_TDft2[c(2:numRows) , c(2:numCols)]
PORT04_TDTable <- graph.adjacency(PORT04_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(PORT04_TDTable, vertex.label = V(PORT04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
PORT04_TD.clusterCoef <- transitivity(PORT04_TDTable, type="global") #cluster coefficient
PORT04_TD.degreeCent <- centralization.degree(PORT04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_TDftn <- as.network.matrix(PORT04_TDft)
PORT04_TD.netDensity <- network.density(PORT04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_TD.entropy <- entropy(PORT04_TDft) #entropy

PORT04_TD.netMx <- cbind(PORT04_TD.netMx, PORT04_TD.clusterCoef, PORT04_TD.degreeCent$centralization,
                         PORT04_TD.netDensity, PORT04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
PORT04_QTg2 <- data.frame(PORT04_QT)
PORT04_QTg2 <- PORT04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT04_QTg2$player1
player2vector <- PORT04_QTg2$player2
PORT04_QTg3 <- PORT04_QTg2
PORT04_QTg3$p1inp2vec <- is.element(PORT04_QTg3$player1, player2vector)
PORT04_QTg3$p2inp1vec <- is.element(PORT04_QTg3$player2, player1vector)

addPlayer1 <- PORT04_QTg3[ which(PORT04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT04_QTg3[ which(PORT04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT04_QTg2 <- rbind(PORT04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
PORT04_QTft <- ftable(PORT04_QTg2$player1, PORT04_QTg2$player2)
PORT04_QTft2 <- as.matrix(PORT04_QTft)
numRows <- nrow(PORT04_QTft2)
numCols <- ncol(PORT04_QTft2)
PORT04_QTft3 <- PORT04_QTft2[c(2:numRows) , c(2:numCols)]
PORT04_QTTable <- graph.adjacency(PORT04_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(PORT04_QTTable, vertex.label = V(PORT04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
PORT04_QT.clusterCoef <- transitivity(PORT04_QTTable, type="global") #cluster coefficient
PORT04_QT.degreeCent <- centralization.degree(PORT04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT04_QTftn <- as.network.matrix(PORT04_QTft)
PORT04_QT.netDensity <- network.density(PORT04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT04_QT.entropy <- entropy(PORT04_QTft) #entropy

PORT04_QT.netMx <- cbind(PORT04_QT.netMx, PORT04_QT.clusterCoef, PORT04_QT.degreeCent$centralization,
                         PORT04_QT.netDensity, PORT04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT04_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "RICH"
KIoutcome = "Goal_F"
RICH04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
RICH04_Gg2 <- data.frame(RICH04_G)
RICH04_Gg2 <- RICH04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_Gg2$player1
player2vector <- RICH04_Gg2$player2
RICH04_Gg3 <- RICH04_Gg2
RICH04_Gg3$p1inp2vec <- is.element(RICH04_Gg3$player1, player2vector)
RICH04_Gg3$p2inp1vec <- is.element(RICH04_Gg3$player2, player1vector)

addPlayer1 <- RICH04_Gg3[ which(RICH04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_Gg3[ which(RICH04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_Gg2 <- rbind(RICH04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
RICH04_Gft <- ftable(RICH04_Gg2$player1, RICH04_Gg2$player2)
RICH04_Gft2 <- as.matrix(RICH04_Gft)
numRows <- nrow(RICH04_Gft2)
numCols <- ncol(RICH04_Gft2)
RICH04_Gft3 <- RICH04_Gft2[c(2:numRows) , c(2:numCols)]
RICH04_GTable <- graph.adjacency(RICH04_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(RICH04_GTable, vertex.label = V(RICH04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
RICH04_G.clusterCoef <- transitivity(RICH04_GTable, type="global") #cluster coefficient
RICH04_G.degreeCent <- centralization.degree(RICH04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_Gftn <- as.network.matrix(RICH04_Gft)
RICH04_G.netDensity <- network.density(RICH04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_G.entropy <- entropy(RICH04_Gft) #entropy

RICH04_G.netMx <- cbind(RICH04_G.netMx, RICH04_G.clusterCoef, RICH04_G.degreeCent$centralization,
                        RICH04_G.netDensity, RICH04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************

round = 4
teamName = "RICH"
KIoutcome = "Behind_F"
RICH04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
RICH04_Bg2 <- data.frame(RICH04_B)
RICH04_Bg2 <- RICH04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_Bg2$player1
player2vector <- RICH04_Bg2$player2
RICH04_Bg3 <- RICH04_Bg2
RICH04_Bg3$p1inp2vec <- is.element(RICH04_Bg3$player1, player2vector)
RICH04_Bg3$p2inp1vec <- is.element(RICH04_Bg3$player2, player1vector)

addPlayer1 <- RICH04_Bg3[ which(RICH04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_Bg3[ which(RICH04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_Bg2 <- rbind(RICH04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
RICH04_Bft <- ftable(RICH04_Bg2$player1, RICH04_Bg2$player2)
RICH04_Bft2 <- as.matrix(RICH04_Bft)
numRows <- nrow(RICH04_Bft2)
numCols <- ncol(RICH04_Bft2)
RICH04_Bft3 <- RICH04_Bft2[c(2:numRows) , c(2:numCols)]
RICH04_BTable <- graph.adjacency(RICH04_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(RICH04_BTable, vertex.label = V(RICH04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
RICH04_B.clusterCoef <- transitivity(RICH04_BTable, type="global") #cluster coefficient
RICH04_B.degreeCent <- centralization.degree(RICH04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_Bftn <- as.network.matrix(RICH04_Bft)
RICH04_B.netDensity <- network.density(RICH04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_B.entropy <- entropy(RICH04_Bft) #entropy

RICH04_B.netMx <- cbind(RICH04_B.netMx, RICH04_B.clusterCoef, RICH04_B.degreeCent$centralization,
                        RICH04_B.netDensity, RICH04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
RICH04_SFg2 <- data.frame(RICH04_SF)
RICH04_SFg2 <- RICH04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_SFg2$player1
player2vector <- RICH04_SFg2$player2
RICH04_SFg3 <- RICH04_SFg2
RICH04_SFg3$p1inp2vec <- is.element(RICH04_SFg3$player1, player2vector)
RICH04_SFg3$p2inp1vec <- is.element(RICH04_SFg3$player2, player1vector)

addPlayer1 <- RICH04_SFg3[ which(RICH04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_SFg3[ which(RICH04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_SFg2 <- rbind(RICH04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
RICH04_SFft <- ftable(RICH04_SFg2$player1, RICH04_SFg2$player2)
RICH04_SFft2 <- as.matrix(RICH04_SFft)
numRows <- nrow(RICH04_SFft2)
numCols <- ncol(RICH04_SFft2)
RICH04_SFft3 <- RICH04_SFft2[c(2:numRows) , c(2:numCols)]
RICH04_SFTable <- graph.adjacency(RICH04_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(RICH04_SFTable, vertex.label = V(RICH04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
RICH04_SF.clusterCoef <- transitivity(RICH04_SFTable, type="global") #cluster coefficient
RICH04_SF.degreeCent <- centralization.degree(RICH04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_SFftn <- as.network.matrix(RICH04_SFft)
RICH04_SF.netDensity <- network.density(RICH04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_SF.entropy <- entropy(RICH04_SFft) #entropy

RICH04_SF.netMx <- cbind(RICH04_SF.netMx, RICH04_SF.clusterCoef, RICH04_SF.degreeCent$centralization,
                         RICH04_SF.netDensity, RICH04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
RICH04_TFg2 <- data.frame(RICH04_TF)
RICH04_TFg2 <- RICH04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_TFg2$player1
player2vector <- RICH04_TFg2$player2
RICH04_TFg3 <- RICH04_TFg2
RICH04_TFg3$p1inp2vec <- is.element(RICH04_TFg3$player1, player2vector)
RICH04_TFg3$p2inp1vec <- is.element(RICH04_TFg3$player2, player1vector)

addPlayer1 <- RICH04_TFg3[ which(RICH04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_TFg3[ which(RICH04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_TFg2 <- rbind(RICH04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
RICH04_TFft <- ftable(RICH04_TFg2$player1, RICH04_TFg2$player2)
RICH04_TFft2 <- as.matrix(RICH04_TFft)
numRows <- nrow(RICH04_TFft2)
numCols <- ncol(RICH04_TFft2)
RICH04_TFft3 <- RICH04_TFft2[c(2:numRows) , c(2:numCols)]
RICH04_TFTable <- graph.adjacency(RICH04_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(RICH04_TFTable, vertex.label = V(RICH04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
RICH04_TF.clusterCoef <- transitivity(RICH04_TFTable, type="global") #cluster coefficient
RICH04_TF.degreeCent <- centralization.degree(RICH04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_TFftn <- as.network.matrix(RICH04_TFft)
RICH04_TF.netDensity <- network.density(RICH04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_TF.entropy <- entropy(RICH04_TFft) #entropy

RICH04_TF.netMx <- cbind(RICH04_TF.netMx, RICH04_TF.clusterCoef, RICH04_TF.degreeCent$centralization,
                         RICH04_TF.netDensity, RICH04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
RICH04_SAMg2 <- data.frame(RICH04_SAM)
RICH04_SAMg2 <- RICH04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_SAMg2$player1
player2vector <- RICH04_SAMg2$player2
RICH04_SAMg3 <- RICH04_SAMg2
RICH04_SAMg3$p1inp2vec <- is.element(RICH04_SAMg3$player1, player2vector)
RICH04_SAMg3$p2inp1vec <- is.element(RICH04_SAMg3$player2, player1vector)

addPlayer1 <- RICH04_SAMg3[ which(RICH04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_SAMg3[ which(RICH04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_SAMg2 <- rbind(RICH04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
RICH04_SAMft <- ftable(RICH04_SAMg2$player1, RICH04_SAMg2$player2)
RICH04_SAMft2 <- as.matrix(RICH04_SAMft)
numRows <- nrow(RICH04_SAMft2)
numCols <- ncol(RICH04_SAMft2)
RICH04_SAMft3 <- RICH04_SAMft2[c(2:numRows) , c(2:numCols)]
RICH04_SAMTable <- graph.adjacency(RICH04_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(RICH04_SAMTable, vertex.label = V(RICH04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
RICH04_SAM.clusterCoef <- transitivity(RICH04_SAMTable, type="global") #cluster coefficient
RICH04_SAM.degreeCent <- centralization.degree(RICH04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_SAMftn <- as.network.matrix(RICH04_SAMft)
RICH04_SAM.netDensity <- network.density(RICH04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_SAM.entropy <- entropy(RICH04_SAMft) #entropy

RICH04_SAM.netMx <- cbind(RICH04_SAM.netMx, RICH04_SAM.clusterCoef, RICH04_SAM.degreeCent$centralization,
                          RICH04_SAM.netDensity, RICH04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
RICH04_TAMg2 <- data.frame(RICH04_TAM)
RICH04_TAMg2 <- RICH04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_TAMg2$player1
player2vector <- RICH04_TAMg2$player2
RICH04_TAMg3 <- RICH04_TAMg2
RICH04_TAMg3$p1inp2vec <- is.element(RICH04_TAMg3$player1, player2vector)
RICH04_TAMg3$p2inp1vec <- is.element(RICH04_TAMg3$player2, player1vector)

addPlayer1 <- RICH04_TAMg3[ which(RICH04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_TAMg3[ which(RICH04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_TAMg2 <- rbind(RICH04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
RICH04_TAMft <- ftable(RICH04_TAMg2$player1, RICH04_TAMg2$player2)
RICH04_TAMft2 <- as.matrix(RICH04_TAMft)
numRows <- nrow(RICH04_TAMft2)
numCols <- ncol(RICH04_TAMft2)
RICH04_TAMft3 <- RICH04_TAMft2[c(2:numRows) , c(2:numCols)]
RICH04_TAMTable <- graph.adjacency(RICH04_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(RICH04_TAMTable, vertex.label = V(RICH04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
RICH04_TAM.clusterCoef <- transitivity(RICH04_TAMTable, type="global") #cluster coefficient
RICH04_TAM.degreeCent <- centralization.degree(RICH04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_TAMftn <- as.network.matrix(RICH04_TAMft)
RICH04_TAM.netDensity <- network.density(RICH04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_TAM.entropy <- entropy(RICH04_TAMft) #entropy

RICH04_TAM.netMx <- cbind(RICH04_TAM.netMx, RICH04_TAM.clusterCoef, RICH04_TAM.degreeCent$centralization,
                          RICH04_TAM.netDensity, RICH04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************
#NA

round = 4
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
RICH04_SDMg2 <- data.frame(RICH04_SDM)
RICH04_SDMg2 <- RICH04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_SDMg2$player1
player2vector <- RICH04_SDMg2$player2
RICH04_SDMg3 <- RICH04_SDMg2
RICH04_SDMg3$p1inp2vec <- is.element(RICH04_SDMg3$player1, player2vector)
RICH04_SDMg3$p2inp1vec <- is.element(RICH04_SDMg3$player2, player1vector)

addPlayer1 <- RICH04_SDMg3[ which(RICH04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_SDMg3[ which(RICH04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_SDMg2 <- rbind(RICH04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
RICH04_SDMft <- ftable(RICH04_SDMg2$player1, RICH04_SDMg2$player2)
RICH04_SDMft2 <- as.matrix(RICH04_SDMft)
numRows <- nrow(RICH04_SDMft2)
numCols <- ncol(RICH04_SDMft2)
RICH04_SDMft3 <- RICH04_SDMft2[c(2:numRows) , c(2:numCols)]
RICH04_SDMTable <- graph.adjacency(RICH04_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(RICH04_SDMTable, vertex.label = V(RICH04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
RICH04_SDM.clusterCoef <- transitivity(RICH04_SDMTable, type="global") #cluster coefficient
RICH04_SDM.degreeCent <- centralization.degree(RICH04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_SDMftn <- as.network.matrix(RICH04_SDMft)
RICH04_SDM.netDensity <- network.density(RICH04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_SDM.entropy <- entropy(RICH04_SDMft) #entropy

RICH04_SDM.netMx <- cbind(RICH04_SDM.netMx, RICH04_SDM.clusterCoef, RICH04_SDM.degreeCent$centralization,
                          RICH04_SDM.netDensity, RICH04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
RICH04_TDMg2 <- data.frame(RICH04_TDM)
RICH04_TDMg2 <- RICH04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_TDMg2$player1
player2vector <- RICH04_TDMg2$player2
RICH04_TDMg3 <- RICH04_TDMg2
RICH04_TDMg3$p1inp2vec <- is.element(RICH04_TDMg3$player1, player2vector)
RICH04_TDMg3$p2inp1vec <- is.element(RICH04_TDMg3$player2, player1vector)

addPlayer1 <- RICH04_TDMg3[ which(RICH04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH04_TDMg3[ which(RICH04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_TDMg2 <- rbind(RICH04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
RICH04_TDMft <- ftable(RICH04_TDMg2$player1, RICH04_TDMg2$player2)
RICH04_TDMft2 <- as.matrix(RICH04_TDMft)
numRows <- nrow(RICH04_TDMft2)
numCols <- ncol(RICH04_TDMft2)
RICH04_TDMft3 <- RICH04_TDMft2[c(2:numRows) , c(2:numCols)]
RICH04_TDMTable <- graph.adjacency(RICH04_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(RICH04_TDMTable, vertex.label = V(RICH04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
RICH04_TDM.clusterCoef <- transitivity(RICH04_TDMTable, type="global") #cluster coefficient
RICH04_TDM.degreeCent <- centralization.degree(RICH04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_TDMftn <- as.network.matrix(RICH04_TDMft)
RICH04_TDM.netDensity <- network.density(RICH04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_TDM.entropy <- entropy(RICH04_TDMft) #entropy

RICH04_TDM.netMx <- cbind(RICH04_TDM.netMx, RICH04_TDM.clusterCoef, RICH04_TDM.degreeCent$centralization,
                          RICH04_TDM.netDensity, RICH04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
RICH04_SDg2 <- data.frame(RICH04_SD)
RICH04_SDg2 <- RICH04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_SDg2$player1
player2vector <- RICH04_SDg2$player2
RICH04_SDg3 <- RICH04_SDg2
RICH04_SDg3$p1inp2vec <- is.element(RICH04_SDg3$player1, player2vector)
RICH04_SDg3$p2inp1vec <- is.element(RICH04_SDg3$player2, player1vector)

addPlayer1 <- RICH04_SDg3[ which(RICH04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_SDg3[ which(RICH04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_SDg2 <- rbind(RICH04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
RICH04_SDft <- ftable(RICH04_SDg2$player1, RICH04_SDg2$player2)
RICH04_SDft2 <- as.matrix(RICH04_SDft)
numRows <- nrow(RICH04_SDft2)
numCols <- ncol(RICH04_SDft2)
RICH04_SDft3 <- RICH04_SDft2[c(2:numRows) , c(2:numCols)]
RICH04_SDTable <- graph.adjacency(RICH04_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(RICH04_SDTable, vertex.label = V(RICH04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
RICH04_SD.clusterCoef <- transitivity(RICH04_SDTable, type="global") #cluster coefficient
RICH04_SD.degreeCent <- centralization.degree(RICH04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_SDftn <- as.network.matrix(RICH04_SDft)
RICH04_SD.netDensity <- network.density(RICH04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_SD.entropy <- entropy(RICH04_SDft) #entropy

RICH04_SD.netMx <- cbind(RICH04_SD.netMx, RICH04_SD.clusterCoef, RICH04_SD.degreeCent$centralization,
                         RICH04_SD.netDensity, RICH04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************

round = 4
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
RICH04_TDg2 <- data.frame(RICH04_TD)
RICH04_TDg2 <- RICH04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_TDg2$player1
player2vector <- RICH04_TDg2$player2
RICH04_TDg3 <- RICH04_TDg2
RICH04_TDg3$p1inp2vec <- is.element(RICH04_TDg3$player1, player2vector)
RICH04_TDg3$p2inp1vec <- is.element(RICH04_TDg3$player2, player1vector)

addPlayer1 <- RICH04_TDg3[ which(RICH04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_TDg3[ which(RICH04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_TDg2 <- rbind(RICH04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
RICH04_TDft <- ftable(RICH04_TDg2$player1, RICH04_TDg2$player2)
RICH04_TDft2 <- as.matrix(RICH04_TDft)
numRows <- nrow(RICH04_TDft2)
numCols <- ncol(RICH04_TDft2)
RICH04_TDft3 <- RICH04_TDft2[c(2:numRows) , c(2:numCols)]
RICH04_TDTable <- graph.adjacency(RICH04_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(RICH04_TDTable, vertex.label = V(RICH04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
RICH04_TD.clusterCoef <- transitivity(RICH04_TDTable, type="global") #cluster coefficient
RICH04_TD.degreeCent <- centralization.degree(RICH04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_TDftn <- as.network.matrix(RICH04_TDft)
RICH04_TD.netDensity <- network.density(RICH04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_TD.entropy <- entropy(RICH04_TDft) #entropy

RICH04_TD.netMx <- cbind(RICH04_TD.netMx, RICH04_TD.clusterCoef, RICH04_TD.degreeCent$centralization,
                         RICH04_TD.netDensity, RICH04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
RICH04_QTg2 <- data.frame(RICH04_QT)
RICH04_QTg2 <- RICH04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH04_QTg2$player1
player2vector <- RICH04_QTg2$player2
RICH04_QTg3 <- RICH04_QTg2
RICH04_QTg3$p1inp2vec <- is.element(RICH04_QTg3$player1, player2vector)
RICH04_QTg3$p2inp1vec <- is.element(RICH04_QTg3$player2, player1vector)

addPlayer1 <- RICH04_QTg3[ which(RICH04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH04_QTg3[ which(RICH04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH04_QTg2 <- rbind(RICH04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
RICH04_QTft <- ftable(RICH04_QTg2$player1, RICH04_QTg2$player2)
RICH04_QTft2 <- as.matrix(RICH04_QTft)
numRows <- nrow(RICH04_QTft2)
numCols <- ncol(RICH04_QTft2)
RICH04_QTft3 <- RICH04_QTft2[c(2:numRows) , c(2:numCols)]
RICH04_QTTable <- graph.adjacency(RICH04_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(RICH04_QTTable, vertex.label = V(RICH04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
RICH04_QT.clusterCoef <- transitivity(RICH04_QTTable, type="global") #cluster coefficient
RICH04_QT.degreeCent <- centralization.degree(RICH04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH04_QTftn <- as.network.matrix(RICH04_QTft)
RICH04_QT.netDensity <- network.density(RICH04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH04_QT.entropy <- entropy(RICH04_QTft) #entropy

RICH04_QT.netMx <- cbind(RICH04_QT.netMx, RICH04_QT.clusterCoef, RICH04_QT.degreeCent$centralization,
                         RICH04_QT.netDensity, RICH04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH04_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "STK"
KIoutcome = "Goal_F"
STK04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
STK04_Gg2 <- data.frame(STK04_G)
STK04_Gg2 <- STK04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_Gg2$player1
player2vector <- STK04_Gg2$player2
STK04_Gg3 <- STK04_Gg2
STK04_Gg3$p1inp2vec <- is.element(STK04_Gg3$player1, player2vector)
STK04_Gg3$p2inp1vec <- is.element(STK04_Gg3$player2, player1vector)

addPlayer1 <- STK04_Gg3[ which(STK04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_Gg3[ which(STK04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_Gg2 <- rbind(STK04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
STK04_Gft <- ftable(STK04_Gg2$player1, STK04_Gg2$player2)
STK04_Gft2 <- as.matrix(STK04_Gft)
numRows <- nrow(STK04_Gft2)
numCols <- ncol(STK04_Gft2)
STK04_Gft3 <- STK04_Gft2[c(2:numRows) , c(2:numCols)]
STK04_GTable <- graph.adjacency(STK04_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(STK04_GTable, vertex.label = V(STK04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
STK04_G.clusterCoef <- transitivity(STK04_GTable, type="global") #cluster coefficient
STK04_G.degreeCent <- centralization.degree(STK04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_Gftn <- as.network.matrix(STK04_Gft)
STK04_G.netDensity <- network.density(STK04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_G.entropy <- entropy(STK04_Gft) #entropy

STK04_G.netMx <- cbind(STK04_G.netMx, STK04_G.clusterCoef, STK04_G.degreeCent$centralization,
                       STK04_G.netDensity, STK04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "STK"
KIoutcome = "Behind_F"
STK04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
STK04_Bg2 <- data.frame(STK04_B)
STK04_Bg2 <- STK04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_Bg2$player1
player2vector <- STK04_Bg2$player2
STK04_Bg3 <- STK04_Bg2
STK04_Bg3$p1inp2vec <- is.element(STK04_Bg3$player1, player2vector)
STK04_Bg3$p2inp1vec <- is.element(STK04_Bg3$player2, player1vector)

addPlayer1 <- STK04_Bg3[ which(STK04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_Bg3[ which(STK04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_Bg2 <- rbind(STK04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
STK04_Bft <- ftable(STK04_Bg2$player1, STK04_Bg2$player2)
STK04_Bft2 <- as.matrix(STK04_Bft)
numRows <- nrow(STK04_Bft2)
numCols <- ncol(STK04_Bft2)
STK04_Bft3 <- STK04_Bft2[c(2:numRows) , c(2:numCols)]
STK04_BTable <- graph.adjacency(STK04_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(STK04_BTable, vertex.label = V(STK04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
STK04_B.clusterCoef <- transitivity(STK04_BTable, type="global") #cluster coefficient
STK04_B.degreeCent <- centralization.degree(STK04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_Bftn <- as.network.matrix(STK04_Bft)
STK04_B.netDensity <- network.density(STK04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_B.entropy <- entropy(STK04_Bft) #entropy

STK04_B.netMx <- cbind(STK04_B.netMx, STK04_B.clusterCoef, STK04_B.degreeCent$centralization,
                       STK04_B.netDensity, STK04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "STK"
KIoutcome = "Stoppage_F"
STK04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
STK04_SFg2 <- data.frame(STK04_SF)
STK04_SFg2 <- STK04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_SFg2$player1
player2vector <- STK04_SFg2$player2
STK04_SFg3 <- STK04_SFg2
STK04_SFg3$p1inp2vec <- is.element(STK04_SFg3$player1, player2vector)
STK04_SFg3$p2inp1vec <- is.element(STK04_SFg3$player2, player1vector)

addPlayer1 <- STK04_SFg3[ which(STK04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_SFg3[ which(STK04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_SFg2 <- rbind(STK04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
STK04_SFft <- ftable(STK04_SFg2$player1, STK04_SFg2$player2)
STK04_SFft2 <- as.matrix(STK04_SFft)
numRows <- nrow(STK04_SFft2)
numCols <- ncol(STK04_SFft2)
STK04_SFft3 <- STK04_SFft2[c(2:numRows) , c(2:numCols)]
STK04_SFTable <- graph.adjacency(STK04_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(STK04_SFTable, vertex.label = V(STK04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
STK04_SF.clusterCoef <- transitivity(STK04_SFTable, type="global") #cluster coefficient
STK04_SF.degreeCent <- centralization.degree(STK04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_SFftn <- as.network.matrix(STK04_SFft)
STK04_SF.netDensity <- network.density(STK04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_SF.entropy <- entropy(STK04_SFft) #entropy

STK04_SF.netMx <- cbind(STK04_SF.netMx, STK04_SF.clusterCoef, STK04_SF.degreeCent$centralization,
                        STK04_SF.netDensity, STK04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "STK"
KIoutcome = "Turnover_F"
STK04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
STK04_TFg2 <- data.frame(STK04_TF)
STK04_TFg2 <- STK04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_TFg2$player1
player2vector <- STK04_TFg2$player2
STK04_TFg3 <- STK04_TFg2
STK04_TFg3$p1inp2vec <- is.element(STK04_TFg3$player1, player2vector)
STK04_TFg3$p2inp1vec <- is.element(STK04_TFg3$player2, player1vector)

addPlayer1 <- STK04_TFg3[ which(STK04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_TFg3[ which(STK04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_TFg2 <- rbind(STK04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
STK04_TFft <- ftable(STK04_TFg2$player1, STK04_TFg2$player2)
STK04_TFft2 <- as.matrix(STK04_TFft)
numRows <- nrow(STK04_TFft2)
numCols <- ncol(STK04_TFft2)
STK04_TFft3 <- STK04_TFft2[c(2:numRows) , c(2:numCols)]
STK04_TFTable <- graph.adjacency(STK04_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(STK04_TFTable, vertex.label = V(STK04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
STK04_TF.clusterCoef <- transitivity(STK04_TFTable, type="global") #cluster coefficient
STK04_TF.degreeCent <- centralization.degree(STK04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_TFftn <- as.network.matrix(STK04_TFft)
STK04_TF.netDensity <- network.density(STK04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_TF.entropy <- entropy(STK04_TFft) #entropy

STK04_TF.netMx <- cbind(STK04_TF.netMx, STK04_TF.clusterCoef, STK04_TF.degreeCent$centralization,
                        STK04_TF.netDensity, STK04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
STK04_SAMg2 <- data.frame(STK04_SAM)
STK04_SAMg2 <- STK04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_SAMg2$player1
player2vector <- STK04_SAMg2$player2
STK04_SAMg3 <- STK04_SAMg2
STK04_SAMg3$p1inp2vec <- is.element(STK04_SAMg3$player1, player2vector)
STK04_SAMg3$p2inp1vec <- is.element(STK04_SAMg3$player2, player1vector)

addPlayer1 <- STK04_SAMg3[ which(STK04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_SAMg3[ which(STK04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_SAMg2 <- rbind(STK04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
STK04_SAMft <- ftable(STK04_SAMg2$player1, STK04_SAMg2$player2)
STK04_SAMft2 <- as.matrix(STK04_SAMft)
numRows <- nrow(STK04_SAMft2)
numCols <- ncol(STK04_SAMft2)
STK04_SAMft3 <- STK04_SAMft2[c(2:numRows) , c(2:numCols)]
STK04_SAMTable <- graph.adjacency(STK04_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(STK04_SAMTable, vertex.label = V(STK04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
STK04_SAM.clusterCoef <- transitivity(STK04_SAMTable, type="global") #cluster coefficient
STK04_SAM.degreeCent <- centralization.degree(STK04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_SAMftn <- as.network.matrix(STK04_SAMft)
STK04_SAM.netDensity <- network.density(STK04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_SAM.entropy <- entropy(STK04_SAMft) #entropy

STK04_SAM.netMx <- cbind(STK04_SAM.netMx, STK04_SAM.clusterCoef, STK04_SAM.degreeCent$centralization,
                         STK04_SAM.netDensity, STK04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "STK"
KIoutcome = "Turnover_AM"
STK04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
STK04_TAMg2 <- data.frame(STK04_TAM)
STK04_TAMg2 <- STK04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_TAMg2$player1
player2vector <- STK04_TAMg2$player2
STK04_TAMg3 <- STK04_TAMg2
STK04_TAMg3$p1inp2vec <- is.element(STK04_TAMg3$player1, player2vector)
STK04_TAMg3$p2inp1vec <- is.element(STK04_TAMg3$player2, player1vector)

addPlayer1 <- STK04_TAMg3[ which(STK04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_TAMg3[ which(STK04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_TAMg2 <- rbind(STK04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
STK04_TAMft <- ftable(STK04_TAMg2$player1, STK04_TAMg2$player2)
STK04_TAMft2 <- as.matrix(STK04_TAMft)
numRows <- nrow(STK04_TAMft2)
numCols <- ncol(STK04_TAMft2)
STK04_TAMft3 <- STK04_TAMft2[c(2:numRows) , c(2:numCols)]
STK04_TAMTable <- graph.adjacency(STK04_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(STK04_TAMTable, vertex.label = V(STK04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
STK04_TAM.clusterCoef <- transitivity(STK04_TAMTable, type="global") #cluster coefficient
STK04_TAM.degreeCent <- centralization.degree(STK04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_TAMftn <- as.network.matrix(STK04_TAMft)
STK04_TAM.netDensity <- network.density(STK04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_TAM.entropy <- entropy(STK04_TAMft) #entropy

STK04_TAM.netMx <- cbind(STK04_TAM.netMx, STK04_TAM.clusterCoef, STK04_TAM.degreeCent$centralization,
                         STK04_TAM.netDensity, STK04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
STK04_SDMg2 <- data.frame(STK04_SDM)
STK04_SDMg2 <- STK04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_SDMg2$player1
player2vector <- STK04_SDMg2$player2
STK04_SDMg3 <- STK04_SDMg2
STK04_SDMg3$p1inp2vec <- is.element(STK04_SDMg3$player1, player2vector)
STK04_SDMg3$p2inp1vec <- is.element(STK04_SDMg3$player2, player1vector)

addPlayer1 <- STK04_SDMg3[ which(STK04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_SDMg3[ which(STK04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_SDMg2 <- rbind(STK04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
STK04_SDMft <- ftable(STK04_SDMg2$player1, STK04_SDMg2$player2)
STK04_SDMft2 <- as.matrix(STK04_SDMft)
numRows <- nrow(STK04_SDMft2)
numCols <- ncol(STK04_SDMft2)
STK04_SDMft3 <- STK04_SDMft2[c(2:numRows) , c(2:numCols)]
STK04_SDMTable <- graph.adjacency(STK04_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(STK04_SDMTable, vertex.label = V(STK04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
STK04_SDM.clusterCoef <- transitivity(STK04_SDMTable, type="global") #cluster coefficient
STK04_SDM.degreeCent <- centralization.degree(STK04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_SDMftn <- as.network.matrix(STK04_SDMft)
STK04_SDM.netDensity <- network.density(STK04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_SDM.entropy <- entropy(STK04_SDMft) #entropy

STK04_SDM.netMx <- cbind(STK04_SDM.netMx, STK04_SDM.clusterCoef, STK04_SDM.degreeCent$centralization,
                         STK04_SDM.netDensity, STK04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "STK"
KIoutcome = "Turnover_DM"
STK04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
STK04_TDMg2 <- data.frame(STK04_TDM)
STK04_TDMg2 <- STK04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_TDMg2$player1
player2vector <- STK04_TDMg2$player2
STK04_TDMg3 <- STK04_TDMg2
STK04_TDMg3$p1inp2vec <- is.element(STK04_TDMg3$player1, player2vector)
STK04_TDMg3$p2inp1vec <- is.element(STK04_TDMg3$player2, player1vector)

addPlayer1 <- STK04_TDMg3[ which(STK04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_TDMg3[ which(STK04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_TDMg2 <- rbind(STK04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
STK04_TDMft <- ftable(STK04_TDMg2$player1, STK04_TDMg2$player2)
STK04_TDMft2 <- as.matrix(STK04_TDMft)
numRows <- nrow(STK04_TDMft2)
numCols <- ncol(STK04_TDMft2)
STK04_TDMft3 <- STK04_TDMft2[c(2:numRows) , c(2:numCols)]
STK04_TDMTable <- graph.adjacency(STK04_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(STK04_TDMTable, vertex.label = V(STK04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
STK04_TDM.clusterCoef <- transitivity(STK04_TDMTable, type="global") #cluster coefficient
STK04_TDM.degreeCent <- centralization.degree(STK04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_TDMftn <- as.network.matrix(STK04_TDMft)
STK04_TDM.netDensity <- network.density(STK04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_TDM.entropy <- entropy(STK04_TDMft) #entropy

STK04_TDM.netMx <- cbind(STK04_TDM.netMx, STK04_TDM.clusterCoef, STK04_TDM.degreeCent$centralization,
                         STK04_TDM.netDensity, STK04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "STK"
KIoutcome = "Stoppage_D"
STK04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
STK04_SDg2 <- data.frame(STK04_SD)
STK04_SDg2 <- STK04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_SDg2$player1
player2vector <- STK04_SDg2$player2
STK04_SDg3 <- STK04_SDg2
STK04_SDg3$p1inp2vec <- is.element(STK04_SDg3$player1, player2vector)
STK04_SDg3$p2inp1vec <- is.element(STK04_SDg3$player2, player1vector)

addPlayer1 <- STK04_SDg3[ which(STK04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_SDg3[ which(STK04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_SDg2 <- rbind(STK04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
STK04_SDft <- ftable(STK04_SDg2$player1, STK04_SDg2$player2)
STK04_SDft2 <- as.matrix(STK04_SDft)
numRows <- nrow(STK04_SDft2)
numCols <- ncol(STK04_SDft2)
STK04_SDft3 <- STK04_SDft2[c(2:numRows) , c(2:numCols)]
STK04_SDTable <- graph.adjacency(STK04_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(STK04_SDTable, vertex.label = V(STK04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
STK04_SD.clusterCoef <- transitivity(STK04_SDTable, type="global") #cluster coefficient
STK04_SD.degreeCent <- centralization.degree(STK04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_SDftn <- as.network.matrix(STK04_SDft)
STK04_SD.netDensity <- network.density(STK04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_SD.entropy <- entropy(STK04_SDft) #entropy

STK04_SD.netMx <- cbind(STK04_SD.netMx, STK04_SD.clusterCoef, STK04_SD.degreeCent$centralization,
                        STK04_SD.netDensity, STK04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************

round = 4
teamName = "STK"
KIoutcome = "Turnover_D"
STK04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
STK04_TDg2 <- data.frame(STK04_TD)
STK04_TDg2 <- STK04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_TDg2$player1
player2vector <- STK04_TDg2$player2
STK04_TDg3 <- STK04_TDg2
STK04_TDg3$p1inp2vec <- is.element(STK04_TDg3$player1, player2vector)
STK04_TDg3$p2inp1vec <- is.element(STK04_TDg3$player2, player1vector)

addPlayer1 <- STK04_TDg3[ which(STK04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_TDg3[ which(STK04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_TDg2 <- rbind(STK04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
STK04_TDft <- ftable(STK04_TDg2$player1, STK04_TDg2$player2)
STK04_TDft2 <- as.matrix(STK04_TDft)
numRows <- nrow(STK04_TDft2)
numCols <- ncol(STK04_TDft2)
STK04_TDft3 <- STK04_TDft2[c(2:numRows) , c(2:numCols)]
STK04_TDTable <- graph.adjacency(STK04_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(STK04_TDTable, vertex.label = V(STK04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
STK04_TD.clusterCoef <- transitivity(STK04_TDTable, type="global") #cluster coefficient
STK04_TD.degreeCent <- centralization.degree(STK04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_TDftn <- as.network.matrix(STK04_TDft)
STK04_TD.netDensity <- network.density(STK04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_TD.entropy <- entropy(STK04_TDft) #entropy

STK04_TD.netMx <- cbind(STK04_TD.netMx, STK04_TD.clusterCoef, STK04_TD.degreeCent$centralization,
                        STK04_TD.netDensity, STK04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
STK04_QTg2 <- data.frame(STK04_QT)
STK04_QTg2 <- STK04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK04_QTg2$player1
player2vector <- STK04_QTg2$player2
STK04_QTg3 <- STK04_QTg2
STK04_QTg3$p1inp2vec <- is.element(STK04_QTg3$player1, player2vector)
STK04_QTg3$p2inp1vec <- is.element(STK04_QTg3$player2, player1vector)

addPlayer1 <- STK04_QTg3[ which(STK04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK04_QTg3[ which(STK04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK04_QTg2 <- rbind(STK04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
STK04_QTft <- ftable(STK04_QTg2$player1, STK04_QTg2$player2)
STK04_QTft2 <- as.matrix(STK04_QTft)
numRows <- nrow(STK04_QTft2)
numCols <- ncol(STK04_QTft2)
STK04_QTft3 <- STK04_QTft2[c(2:numRows) , c(2:numCols)]
STK04_QTTable <- graph.adjacency(STK04_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(STK04_QTTable, vertex.label = V(STK04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
STK04_QT.clusterCoef <- transitivity(STK04_QTTable, type="global") #cluster coefficient
STK04_QT.degreeCent <- centralization.degree(STK04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK04_QTftn <- as.network.matrix(STK04_QTft)
STK04_QT.netDensity <- network.density(STK04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK04_QT.entropy <- entropy(STK04_QTft) #entropy

STK04_QT.netMx <- cbind(STK04_QT.netMx, STK04_QT.clusterCoef, STK04_QT.degreeCent$centralization,
                        STK04_QT.netDensity, STK04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK04_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "SYD"
KIoutcome = "Goal_F"
SYD04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
SYD04_Gg2 <- data.frame(SYD04_G)
SYD04_Gg2 <- SYD04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_Gg2$player1
player2vector <- SYD04_Gg2$player2
SYD04_Gg3 <- SYD04_Gg2
SYD04_Gg3$p1inp2vec <- is.element(SYD04_Gg3$player1, player2vector)
SYD04_Gg3$p2inp1vec <- is.element(SYD04_Gg3$player2, player1vector)

addPlayer1 <- SYD04_Gg3[ which(SYD04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_Gg3[ which(SYD04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_Gg2 <- rbind(SYD04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
SYD04_Gft <- ftable(SYD04_Gg2$player1, SYD04_Gg2$player2)
SYD04_Gft2 <- as.matrix(SYD04_Gft)
numRows <- nrow(SYD04_Gft2)
numCols <- ncol(SYD04_Gft2)
SYD04_Gft3 <- SYD04_Gft2[c(2:numRows) , c(2:numCols)]
SYD04_GTable <- graph.adjacency(SYD04_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(SYD04_GTable, vertex.label = V(SYD04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
SYD04_G.clusterCoef <- transitivity(SYD04_GTable, type="global") #cluster coefficient
SYD04_G.degreeCent <- centralization.degree(SYD04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_Gftn <- as.network.matrix(SYD04_Gft)
SYD04_G.netDensity <- network.density(SYD04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_G.entropy <- entropy(SYD04_Gft) #entropy

SYD04_G.netMx <- cbind(SYD04_G.netMx, SYD04_G.clusterCoef, SYD04_G.degreeCent$centralization,
                       SYD04_G.netDensity, SYD04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************

round = 4
teamName = "SYD"
KIoutcome = "Behind_F"
SYD04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
SYD04_Bg2 <- data.frame(SYD04_B)
SYD04_Bg2 <- SYD04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_Bg2$player1
player2vector <- SYD04_Bg2$player2
SYD04_Bg3 <- SYD04_Bg2
SYD04_Bg3$p1inp2vec <- is.element(SYD04_Bg3$player1, player2vector)
SYD04_Bg3$p2inp1vec <- is.element(SYD04_Bg3$player2, player1vector)

addPlayer1 <- SYD04_Bg3[ which(SYD04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_Bg3[ which(SYD04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_Bg2 <- rbind(SYD04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
SYD04_Bft <- ftable(SYD04_Bg2$player1, SYD04_Bg2$player2)
SYD04_Bft2 <- as.matrix(SYD04_Bft)
numRows <- nrow(SYD04_Bft2)
numCols <- ncol(SYD04_Bft2)
SYD04_Bft3 <- SYD04_Bft2[c(2:numRows) , c(2:numCols)]
SYD04_BTable <- graph.adjacency(SYD04_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(SYD04_BTable, vertex.label = V(SYD04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
SYD04_B.clusterCoef <- transitivity(SYD04_BTable, type="global") #cluster coefficient
SYD04_B.degreeCent <- centralization.degree(SYD04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_Bftn <- as.network.matrix(SYD04_Bft)
SYD04_B.netDensity <- network.density(SYD04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_B.entropy <- entropy(SYD04_Bft) #entropy

SYD04_B.netMx <- cbind(SYD04_B.netMx, SYD04_B.clusterCoef, SYD04_B.degreeCent$centralization,
                       SYD04_B.netDensity, SYD04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
SYD04_SFg2 <- data.frame(SYD04_SF)
SYD04_SFg2 <- SYD04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_SFg2$player1
player2vector <- SYD04_SFg2$player2
SYD04_SFg3 <- SYD04_SFg2
SYD04_SFg3$p1inp2vec <- is.element(SYD04_SFg3$player1, player2vector)
SYD04_SFg3$p2inp1vec <- is.element(SYD04_SFg3$player2, player1vector)

addPlayer1 <- SYD04_SFg3[ which(SYD04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_SFg3[ which(SYD04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_SFg2 <- rbind(SYD04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
SYD04_SFft <- ftable(SYD04_SFg2$player1, SYD04_SFg2$player2)
SYD04_SFft2 <- as.matrix(SYD04_SFft)
numRows <- nrow(SYD04_SFft2)
numCols <- ncol(SYD04_SFft2)
SYD04_SFft3 <- SYD04_SFft2[c(2:numRows) , c(2:numCols)]
SYD04_SFTable <- graph.adjacency(SYD04_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(SYD04_SFTable, vertex.label = V(SYD04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
SYD04_SF.clusterCoef <- transitivity(SYD04_SFTable, type="global") #cluster coefficient
SYD04_SF.degreeCent <- centralization.degree(SYD04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_SFftn <- as.network.matrix(SYD04_SFft)
SYD04_SF.netDensity <- network.density(SYD04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_SF.entropy <- entropy(SYD04_SFft) #entropy

SYD04_SF.netMx <- cbind(SYD04_SF.netMx, SYD04_SF.clusterCoef, SYD04_SF.degreeCent$centralization,
                        SYD04_SF.netDensity, SYD04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
SYD04_TFg2 <- data.frame(SYD04_TF)
SYD04_TFg2 <- SYD04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_TFg2$player1
player2vector <- SYD04_TFg2$player2
SYD04_TFg3 <- SYD04_TFg2
SYD04_TFg3$p1inp2vec <- is.element(SYD04_TFg3$player1, player2vector)
SYD04_TFg3$p2inp1vec <- is.element(SYD04_TFg3$player2, player1vector)

addPlayer1 <- SYD04_TFg3[ which(SYD04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_TFg3[ which(SYD04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_TFg2 <- rbind(SYD04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
SYD04_TFft <- ftable(SYD04_TFg2$player1, SYD04_TFg2$player2)
SYD04_TFft2 <- as.matrix(SYD04_TFft)
numRows <- nrow(SYD04_TFft2)
numCols <- ncol(SYD04_TFft2)
SYD04_TFft3 <- SYD04_TFft2[c(2:numRows) , c(2:numCols)]
SYD04_TFTable <- graph.adjacency(SYD04_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(SYD04_TFTable, vertex.label = V(SYD04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
SYD04_TF.clusterCoef <- transitivity(SYD04_TFTable, type="global") #cluster coefficient
SYD04_TF.degreeCent <- centralization.degree(SYD04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_TFftn <- as.network.matrix(SYD04_TFft)
SYD04_TF.netDensity <- network.density(SYD04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_TF.entropy <- entropy(SYD04_TFft) #entropy

SYD04_TF.netMx <- cbind(SYD04_TF.netMx, SYD04_TF.clusterCoef, SYD04_TF.degreeCent$centralization,
                        SYD04_TF.netDensity, SYD04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
SYD04_SAMg2 <- data.frame(SYD04_SAM)
SYD04_SAMg2 <- SYD04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_SAMg2$player1
player2vector <- SYD04_SAMg2$player2
SYD04_SAMg3 <- SYD04_SAMg2
SYD04_SAMg3$p1inp2vec <- is.element(SYD04_SAMg3$player1, player2vector)
SYD04_SAMg3$p2inp1vec <- is.element(SYD04_SAMg3$player2, player1vector)

addPlayer1 <- SYD04_SAMg3[ which(SYD04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_SAMg3[ which(SYD04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_SAMg2 <- rbind(SYD04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
SYD04_SAMft <- ftable(SYD04_SAMg2$player1, SYD04_SAMg2$player2)
SYD04_SAMft2 <- as.matrix(SYD04_SAMft)
numRows <- nrow(SYD04_SAMft2)
numCols <- ncol(SYD04_SAMft2)
SYD04_SAMft3 <- SYD04_SAMft2[c(2:numRows) , c(2:numCols)]
SYD04_SAMTable <- graph.adjacency(SYD04_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(SYD04_SAMTable, vertex.label = V(SYD04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
SYD04_SAM.clusterCoef <- transitivity(SYD04_SAMTable, type="global") #cluster coefficient
SYD04_SAM.degreeCent <- centralization.degree(SYD04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_SAMftn <- as.network.matrix(SYD04_SAMft)
SYD04_SAM.netDensity <- network.density(SYD04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_SAM.entropy <- entropy(SYD04_SAMft) #entropy

SYD04_SAM.netMx <- cbind(SYD04_SAM.netMx, SYD04_SAM.clusterCoef, SYD04_SAM.degreeCent$centralization,
                         SYD04_SAM.netDensity, SYD04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
SYD04_TAMg2 <- data.frame(SYD04_TAM)
SYD04_TAMg2 <- SYD04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_TAMg2$player1
player2vector <- SYD04_TAMg2$player2
SYD04_TAMg3 <- SYD04_TAMg2
SYD04_TAMg3$p1inp2vec <- is.element(SYD04_TAMg3$player1, player2vector)
SYD04_TAMg3$p2inp1vec <- is.element(SYD04_TAMg3$player2, player1vector)

addPlayer1 <- SYD04_TAMg3[ which(SYD04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_TAMg3[ which(SYD04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_TAMg2 <- rbind(SYD04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
SYD04_TAMft <- ftable(SYD04_TAMg2$player1, SYD04_TAMg2$player2)
SYD04_TAMft2 <- as.matrix(SYD04_TAMft)
numRows <- nrow(SYD04_TAMft2)
numCols <- ncol(SYD04_TAMft2)
SYD04_TAMft3 <- SYD04_TAMft2[c(2:numRows) , c(2:numCols)]
SYD04_TAMTable <- graph.adjacency(SYD04_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(SYD04_TAMTable, vertex.label = V(SYD04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
SYD04_TAM.clusterCoef <- transitivity(SYD04_TAMTable, type="global") #cluster coefficient
SYD04_TAM.degreeCent <- centralization.degree(SYD04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_TAMftn <- as.network.matrix(SYD04_TAMft)
SYD04_TAM.netDensity <- network.density(SYD04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_TAM.entropy <- entropy(SYD04_TAMft) #entropy

SYD04_TAM.netMx <- cbind(SYD04_TAM.netMx, SYD04_TAM.clusterCoef, SYD04_TAM.degreeCent$centralization,
                         SYD04_TAM.netDensity, SYD04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
SYD04_SDMg2 <- data.frame(SYD04_SDM)
SYD04_SDMg2 <- SYD04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_SDMg2$player1
player2vector <- SYD04_SDMg2$player2
SYD04_SDMg3 <- SYD04_SDMg2
SYD04_SDMg3$p1inp2vec <- is.element(SYD04_SDMg3$player1, player2vector)
SYD04_SDMg3$p2inp1vec <- is.element(SYD04_SDMg3$player2, player1vector)

addPlayer1 <- SYD04_SDMg3[ which(SYD04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_SDMg3[ which(SYD04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_SDMg2 <- rbind(SYD04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
SYD04_SDMft <- ftable(SYD04_SDMg2$player1, SYD04_SDMg2$player2)
SYD04_SDMft2 <- as.matrix(SYD04_SDMft)
numRows <- nrow(SYD04_SDMft2)
numCols <- ncol(SYD04_SDMft2)
SYD04_SDMft3 <- SYD04_SDMft2[c(2:numRows) , c(2:numCols)]
SYD04_SDMTable <- graph.adjacency(SYD04_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(SYD04_SDMTable, vertex.label = V(SYD04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
SYD04_SDM.clusterCoef <- transitivity(SYD04_SDMTable, type="global") #cluster coefficient
SYD04_SDM.degreeCent <- centralization.degree(SYD04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_SDMftn <- as.network.matrix(SYD04_SDMft)
SYD04_SDM.netDensity <- network.density(SYD04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_SDM.entropy <- entropy(SYD04_SDMft) #entropy

SYD04_SDM.netMx <- cbind(SYD04_SDM.netMx, SYD04_SDM.clusterCoef, SYD04_SDM.degreeCent$centralization,
                         SYD04_SDM.netDensity, SYD04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
SYD04_TDMg2 <- data.frame(SYD04_TDM)
SYD04_TDMg2 <- SYD04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_TDMg2$player1
player2vector <- SYD04_TDMg2$player2
SYD04_TDMg3 <- SYD04_TDMg2
SYD04_TDMg3$p1inp2vec <- is.element(SYD04_TDMg3$player1, player2vector)
SYD04_TDMg3$p2inp1vec <- is.element(SYD04_TDMg3$player2, player1vector)

addPlayer1 <- SYD04_TDMg3[ which(SYD04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_TDMg3[ which(SYD04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_TDMg2 <- rbind(SYD04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
SYD04_TDMft <- ftable(SYD04_TDMg2$player1, SYD04_TDMg2$player2)
SYD04_TDMft2 <- as.matrix(SYD04_TDMft)
numRows <- nrow(SYD04_TDMft2)
numCols <- ncol(SYD04_TDMft2)
SYD04_TDMft3 <- SYD04_TDMft2[c(2:numRows) , c(2:numCols)]
SYD04_TDMTable <- graph.adjacency(SYD04_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(SYD04_TDMTable, vertex.label = V(SYD04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
SYD04_TDM.clusterCoef <- transitivity(SYD04_TDMTable, type="global") #cluster coefficient
SYD04_TDM.degreeCent <- centralization.degree(SYD04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_TDMftn <- as.network.matrix(SYD04_TDMft)
SYD04_TDM.netDensity <- network.density(SYD04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_TDM.entropy <- entropy(SYD04_TDMft) #entropy

SYD04_TDM.netMx <- cbind(SYD04_TDM.netMx, SYD04_TDM.clusterCoef, SYD04_TDM.degreeCent$centralization,
                         SYD04_TDM.netDensity, SYD04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
SYD04_SDg2 <- data.frame(SYD04_SD)
SYD04_SDg2 <- SYD04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_SDg2$player1
player2vector <- SYD04_SDg2$player2
SYD04_SDg3 <- SYD04_SDg2
SYD04_SDg3$p1inp2vec <- is.element(SYD04_SDg3$player1, player2vector)
SYD04_SDg3$p2inp1vec <- is.element(SYD04_SDg3$player2, player1vector)

addPlayer1 <- SYD04_SDg3[ which(SYD04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_SDg3[ which(SYD04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_SDg2 <- rbind(SYD04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
SYD04_SDft <- ftable(SYD04_SDg2$player1, SYD04_SDg2$player2)
SYD04_SDft2 <- as.matrix(SYD04_SDft)
numRows <- nrow(SYD04_SDft2)
numCols <- ncol(SYD04_SDft2)
SYD04_SDft3 <- SYD04_SDft2[c(2:numRows) , c(2:numCols)]
SYD04_SDTable <- graph.adjacency(SYD04_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(SYD04_SDTable, vertex.label = V(SYD04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
SYD04_SD.clusterCoef <- transitivity(SYD04_SDTable, type="global") #cluster coefficient
SYD04_SD.degreeCent <- centralization.degree(SYD04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_SDftn <- as.network.matrix(SYD04_SDft)
SYD04_SD.netDensity <- network.density(SYD04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_SD.entropy <- entropy(SYD04_SDft) #entropy

SYD04_SD.netMx <- cbind(SYD04_SD.netMx, SYD04_SD.clusterCoef, SYD04_SD.degreeCent$centralization,
                        SYD04_SD.netDensity, SYD04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
SYD04_TDg2 <- data.frame(SYD04_TD)
SYD04_TDg2 <- SYD04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_TDg2$player1
player2vector <- SYD04_TDg2$player2
SYD04_TDg3 <- SYD04_TDg2
SYD04_TDg3$p1inp2vec <- is.element(SYD04_TDg3$player1, player2vector)
SYD04_TDg3$p2inp1vec <- is.element(SYD04_TDg3$player2, player1vector)

addPlayer1 <- SYD04_TDg3[ which(SYD04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_TDg3[ which(SYD04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_TDg2 <- rbind(SYD04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
SYD04_TDft <- ftable(SYD04_TDg2$player1, SYD04_TDg2$player2)
SYD04_TDft2 <- as.matrix(SYD04_TDft)
numRows <- nrow(SYD04_TDft2)
numCols <- ncol(SYD04_TDft2)
SYD04_TDft3 <- SYD04_TDft2[c(2:numRows) , c(2:numCols)]
SYD04_TDTable <- graph.adjacency(SYD04_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(SYD04_TDTable, vertex.label = V(SYD04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
SYD04_TD.clusterCoef <- transitivity(SYD04_TDTable, type="global") #cluster coefficient
SYD04_TD.degreeCent <- centralization.degree(SYD04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_TDftn <- as.network.matrix(SYD04_TDft)
SYD04_TD.netDensity <- network.density(SYD04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_TD.entropy <- entropy(SYD04_TDft) #entropy

SYD04_TD.netMx <- cbind(SYD04_TD.netMx, SYD04_TD.clusterCoef, SYD04_TD.degreeCent$centralization,
                        SYD04_TD.netDensity, SYD04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
SYD04_QTg2 <- data.frame(SYD04_QT)
SYD04_QTg2 <- SYD04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD04_QTg2$player1
player2vector <- SYD04_QTg2$player2
SYD04_QTg3 <- SYD04_QTg2
SYD04_QTg3$p1inp2vec <- is.element(SYD04_QTg3$player1, player2vector)
SYD04_QTg3$p2inp1vec <- is.element(SYD04_QTg3$player2, player1vector)

addPlayer1 <- SYD04_QTg3[ which(SYD04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD04_QTg3[ which(SYD04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD04_QTg2 <- rbind(SYD04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
SYD04_QTft <- ftable(SYD04_QTg2$player1, SYD04_QTg2$player2)
SYD04_QTft2 <- as.matrix(SYD04_QTft)
numRows <- nrow(SYD04_QTft2)
numCols <- ncol(SYD04_QTft2)
SYD04_QTft3 <- SYD04_QTft2[c(2:numRows) , c(2:numCols)]
SYD04_QTTable <- graph.adjacency(SYD04_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(SYD04_QTTable, vertex.label = V(SYD04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
SYD04_QT.clusterCoef <- transitivity(SYD04_QTTable, type="global") #cluster coefficient
SYD04_QT.degreeCent <- centralization.degree(SYD04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD04_QTftn <- as.network.matrix(SYD04_QTft)
SYD04_QT.netDensity <- network.density(SYD04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD04_QT.entropy <- entropy(SYD04_QTft) #entropy

SYD04_QT.netMx <- cbind(SYD04_QT.netMx, SYD04_QT.clusterCoef, SYD04_QT.degreeCent$centralization,
                        SYD04_QT.netDensity, SYD04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD04_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************
#NA

round = 4
teamName = "WB"
KIoutcome = "Goal_F"
WB04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
WB04_Gg2 <- data.frame(WB04_G)
WB04_Gg2 <- WB04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_Gg2$player1
player2vector <- WB04_Gg2$player2
WB04_Gg3 <- WB04_Gg2
WB04_Gg3$p1inp2vec <- is.element(WB04_Gg3$player1, player2vector)
WB04_Gg3$p2inp1vec <- is.element(WB04_Gg3$player2, player1vector)

addPlayer1 <- WB04_Gg3[ which(WB04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_Gg3[ which(WB04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_Gg2 <- rbind(WB04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
WB04_Gft <- ftable(WB04_Gg2$player1, WB04_Gg2$player2)
WB04_Gft2 <- as.matrix(WB04_Gft)
numRows <- nrow(WB04_Gft2)
numCols <- ncol(WB04_Gft2)
WB04_Gft3 <- WB04_Gft2[c(2:numRows) , c(2:numCols)]
WB04_GTable <- graph.adjacency(WB04_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(WB04_GTable, vertex.label = V(WB04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
WB04_G.clusterCoef <- transitivity(WB04_GTable, type="global") #cluster coefficient
WB04_G.degreeCent <- centralization.degree(WB04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_Gftn <- as.network.matrix(WB04_Gft)
WB04_G.netDensity <- network.density(WB04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_G.entropy <- entropy(WB04_Gft) #entropy

WB04_G.netMx <- cbind(WB04_G.netMx, WB04_G.clusterCoef, WB04_G.degreeCent$centralization,
                      WB04_G.netDensity, WB04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "WB"
KIoutcome = "Behind_F"
WB04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
WB04_Bg2 <- data.frame(WB04_B)
WB04_Bg2 <- WB04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_Bg2$player1
player2vector <- WB04_Bg2$player2
WB04_Bg3 <- WB04_Bg2
WB04_Bg3$p1inp2vec <- is.element(WB04_Bg3$player1, player2vector)
WB04_Bg3$p2inp1vec <- is.element(WB04_Bg3$player2, player1vector)

addPlayer1 <- WB04_Bg3[ which(WB04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_Bg3[ which(WB04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_Bg2 <- rbind(WB04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
WB04_Bft <- ftable(WB04_Bg2$player1, WB04_Bg2$player2)
WB04_Bft2 <- as.matrix(WB04_Bft)
numRows <- nrow(WB04_Bft2)
numCols <- ncol(WB04_Bft2)
WB04_Bft3 <- WB04_Bft2[c(2:numRows) , c(2:numCols)]
WB04_BTable <- graph.adjacency(WB04_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(WB04_BTable, vertex.label = V(WB04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
WB04_B.clusterCoef <- transitivity(WB04_BTable, type="global") #cluster coefficient
WB04_B.degreeCent <- centralization.degree(WB04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_Bftn <- as.network.matrix(WB04_Bft)
WB04_B.netDensity <- network.density(WB04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_B.entropy <- entropy(WB04_Bft) #entropy

WB04_B.netMx <- cbind(WB04_B.netMx, WB04_B.clusterCoef, WB04_B.degreeCent$centralization,
                      WB04_B.netDensity, WB04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "WB"
KIoutcome = "Stoppage_F"
WB04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
WB04_SFg2 <- data.frame(WB04_SF)
WB04_SFg2 <- WB04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_SFg2$player1
player2vector <- WB04_SFg2$player2
WB04_SFg3 <- WB04_SFg2
WB04_SFg3$p1inp2vec <- is.element(WB04_SFg3$player1, player2vector)
WB04_SFg3$p2inp1vec <- is.element(WB04_SFg3$player2, player1vector)

addPlayer1 <- WB04_SFg3[ which(WB04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_SFg3[ which(WB04_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_SFg2 <- rbind(WB04_SFg2, addPlayers)

#ROUND 4, FWD Stoppage graph using weighted edges
WB04_SFft <- ftable(WB04_SFg2$player1, WB04_SFg2$player2)
WB04_SFft2 <- as.matrix(WB04_SFft)
numRows <- nrow(WB04_SFft2)
numCols <- ncol(WB04_SFft2)
WB04_SFft3 <- WB04_SFft2[c(2:numRows) , c(2:numCols)]
WB04_SFTable <- graph.adjacency(WB04_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(WB04_SFTable, vertex.label = V(WB04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
WB04_SF.clusterCoef <- transitivity(WB04_SFTable, type="global") #cluster coefficient
WB04_SF.degreeCent <- centralization.degree(WB04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_SFftn <- as.network.matrix(WB04_SFft)
WB04_SF.netDensity <- network.density(WB04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_SF.entropy <- entropy(WB04_SFft) #entropy

WB04_SF.netMx <- cbind(WB04_SF.netMx, WB04_SF.clusterCoef, WB04_SF.degreeCent$centralization,
                       WB04_SF.netDensity, WB04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "WB"
KIoutcome = "Turnover_F"
WB04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
WB04_TFg2 <- data.frame(WB04_TF)
WB04_TFg2 <- WB04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_TFg2$player1
player2vector <- WB04_TFg2$player2
WB04_TFg3 <- WB04_TFg2
WB04_TFg3$p1inp2vec <- is.element(WB04_TFg3$player1, player2vector)
WB04_TFg3$p2inp1vec <- is.element(WB04_TFg3$player2, player1vector)

addPlayer1 <- WB04_TFg3[ which(WB04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_TFg3[ which(WB04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_TFg2 <- rbind(WB04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
WB04_TFft <- ftable(WB04_TFg2$player1, WB04_TFg2$player2)
WB04_TFft2 <- as.matrix(WB04_TFft)
numRows <- nrow(WB04_TFft2)
numCols <- ncol(WB04_TFft2)
WB04_TFft3 <- WB04_TFft2[c(2:numRows) , c(2:numCols)]
WB04_TFTable <- graph.adjacency(WB04_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(WB04_TFTable, vertex.label = V(WB04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
WB04_TF.clusterCoef <- transitivity(WB04_TFTable, type="global") #cluster coefficient
WB04_TF.degreeCent <- centralization.degree(WB04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_TFftn <- as.network.matrix(WB04_TFft)
WB04_TF.netDensity <- network.density(WB04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_TF.entropy <- entropy(WB04_TFft) #entropy

WB04_TF.netMx <- cbind(WB04_TF.netMx, WB04_TF.clusterCoef, WB04_TF.degreeCent$centralization,
                       WB04_TF.netDensity, WB04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
WB04_SAMg2 <- data.frame(WB04_SAM)
WB04_SAMg2 <- WB04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_SAMg2$player1
player2vector <- WB04_SAMg2$player2
WB04_SAMg3 <- WB04_SAMg2
WB04_SAMg3$p1inp2vec <- is.element(WB04_SAMg3$player1, player2vector)
WB04_SAMg3$p2inp1vec <- is.element(WB04_SAMg3$player2, player1vector)

addPlayer1 <- WB04_SAMg3[ which(WB04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_SAMg3[ which(WB04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_SAMg2 <- rbind(WB04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
WB04_SAMft <- ftable(WB04_SAMg2$player1, WB04_SAMg2$player2)
WB04_SAMft2 <- as.matrix(WB04_SAMft)
numRows <- nrow(WB04_SAMft2)
numCols <- ncol(WB04_SAMft2)
WB04_SAMft3 <- WB04_SAMft2[c(2:numRows) , c(2:numCols)]
WB04_SAMTable <- graph.adjacency(WB04_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(WB04_SAMTable, vertex.label = V(WB04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
WB04_SAM.clusterCoef <- transitivity(WB04_SAMTable, type="global") #cluster coefficient
WB04_SAM.degreeCent <- centralization.degree(WB04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_SAMftn <- as.network.matrix(WB04_SAMft)
WB04_SAM.netDensity <- network.density(WB04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_SAM.entropy <- entropy(WB04_SAMft) #entropy

WB04_SAM.netMx <- cbind(WB04_SAM.netMx, WB04_SAM.clusterCoef, WB04_SAM.degreeCent$centralization,
                        WB04_SAM.netDensity, WB04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "WB"
KIoutcome = "Turnover_AM"
WB04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
WB04_TAMg2 <- data.frame(WB04_TAM)
WB04_TAMg2 <- WB04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_TAMg2$player1
player2vector <- WB04_TAMg2$player2
WB04_TAMg3 <- WB04_TAMg2
WB04_TAMg3$p1inp2vec <- is.element(WB04_TAMg3$player1, player2vector)
WB04_TAMg3$p2inp1vec <- is.element(WB04_TAMg3$player2, player1vector)

addPlayer1 <- WB04_TAMg3[ which(WB04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_TAMg3[ which(WB04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_TAMg2 <- rbind(WB04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
WB04_TAMft <- ftable(WB04_TAMg2$player1, WB04_TAMg2$player2)
WB04_TAMft2 <- as.matrix(WB04_TAMft)
numRows <- nrow(WB04_TAMft2)
numCols <- ncol(WB04_TAMft2)
WB04_TAMft3 <- WB04_TAMft2[c(2:numRows) , c(2:numCols)]
WB04_TAMTable <- graph.adjacency(WB04_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(WB04_TAMTable, vertex.label = V(WB04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
WB04_TAM.clusterCoef <- transitivity(WB04_TAMTable, type="global") #cluster coefficient
WB04_TAM.degreeCent <- centralization.degree(WB04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_TAMftn <- as.network.matrix(WB04_TAMft)
WB04_TAM.netDensity <- network.density(WB04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_TAM.entropy <- entropy(WB04_TAMft) #entropy

WB04_TAM.netMx <- cbind(WB04_TAM.netMx, WB04_TAM.clusterCoef, WB04_TAM.degreeCent$centralization,
                        WB04_TAM.netDensity, WB04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
WB04_SDMg2 <- data.frame(WB04_SDM)
WB04_SDMg2 <- WB04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_SDMg2$player1
player2vector <- WB04_SDMg2$player2
WB04_SDMg3 <- WB04_SDMg2
WB04_SDMg3$p1inp2vec <- is.element(WB04_SDMg3$player1, player2vector)
WB04_SDMg3$p2inp1vec <- is.element(WB04_SDMg3$player2, player1vector)

addPlayer1 <- WB04_SDMg3[ which(WB04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_SDMg3[ which(WB04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_SDMg2 <- rbind(WB04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
WB04_SDMft <- ftable(WB04_SDMg2$player1, WB04_SDMg2$player2)
WB04_SDMft2 <- as.matrix(WB04_SDMft)
numRows <- nrow(WB04_SDMft2)
numCols <- ncol(WB04_SDMft2)
WB04_SDMft3 <- WB04_SDMft2[c(2:numRows) , c(2:numCols)]
WB04_SDMTable <- graph.adjacency(WB04_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(WB04_SDMTable, vertex.label = V(WB04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
WB04_SDM.clusterCoef <- transitivity(WB04_SDMTable, type="global") #cluster coefficient
WB04_SDM.degreeCent <- centralization.degree(WB04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_SDMftn <- as.network.matrix(WB04_SDMft)
WB04_SDM.netDensity <- network.density(WB04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_SDM.entropy <- entropy(WB04_SDMft) #entropy

WB04_SDM.netMx <- cbind(WB04_SDM.netMx, WB04_SDM.clusterCoef, WB04_SDM.degreeCent$centralization,
                        WB04_SDM.netDensity, WB04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "WB"
KIoutcome = "Turnover_DM"
WB04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
WB04_TDMg2 <- data.frame(WB04_TDM)
WB04_TDMg2 <- WB04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_TDMg2$player1
player2vector <- WB04_TDMg2$player2
WB04_TDMg3 <- WB04_TDMg2
WB04_TDMg3$p1inp2vec <- is.element(WB04_TDMg3$player1, player2vector)
WB04_TDMg3$p2inp1vec <- is.element(WB04_TDMg3$player2, player1vector)

addPlayer1 <- WB04_TDMg3[ which(WB04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_TDMg3[ which(WB04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_TDMg2 <- rbind(WB04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
WB04_TDMft <- ftable(WB04_TDMg2$player1, WB04_TDMg2$player2)
WB04_TDMft2 <- as.matrix(WB04_TDMft)
numRows <- nrow(WB04_TDMft2)
numCols <- ncol(WB04_TDMft2)
WB04_TDMft3 <- WB04_TDMft2[c(2:numRows) , c(2:numCols)]
WB04_TDMTable <- graph.adjacency(WB04_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(WB04_TDMTable, vertex.label = V(WB04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
WB04_TDM.clusterCoef <- transitivity(WB04_TDMTable, type="global") #cluster coefficient
WB04_TDM.degreeCent <- centralization.degree(WB04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_TDMftn <- as.network.matrix(WB04_TDMft)
WB04_TDM.netDensity <- network.density(WB04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_TDM.entropy <- entropy(WB04_TDMft) #entropy

WB04_TDM.netMx <- cbind(WB04_TDM.netMx, WB04_TDM.clusterCoef, WB04_TDM.degreeCent$centralization,
                        WB04_TDM.netDensity, WB04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "WB"
KIoutcome = "Stoppage_D"
WB04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
WB04_SDg2 <- data.frame(WB04_SD)
WB04_SDg2 <- WB04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_SDg2$player1
player2vector <- WB04_SDg2$player2
WB04_SDg3 <- WB04_SDg2
WB04_SDg3$p1inp2vec <- is.element(WB04_SDg3$player1, player2vector)
WB04_SDg3$p2inp1vec <- is.element(WB04_SDg3$player2, player1vector)

addPlayer1 <- WB04_SDg3[ which(WB04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_SDg3[ which(WB04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_SDg2 <- rbind(WB04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
WB04_SDft <- ftable(WB04_SDg2$player1, WB04_SDg2$player2)
WB04_SDft2 <- as.matrix(WB04_SDft)
numRows <- nrow(WB04_SDft2)
numCols <- ncol(WB04_SDft2)
WB04_SDft3 <- WB04_SDft2[c(2:numRows) , c(2:numCols)]
WB04_SDTable <- graph.adjacency(WB04_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(WB04_SDTable, vertex.label = V(WB04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
WB04_SD.clusterCoef <- transitivity(WB04_SDTable, type="global") #cluster coefficient
WB04_SD.degreeCent <- centralization.degree(WB04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_SDftn <- as.network.matrix(WB04_SDft)
WB04_SD.netDensity <- network.density(WB04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_SD.entropy <- entropy(WB04_SDft) #entropy

WB04_SD.netMx <- cbind(WB04_SD.netMx, WB04_SD.clusterCoef, WB04_SD.degreeCent$centralization,
                       WB04_SD.netDensity, WB04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "WB"
KIoutcome = "Turnover_D"
WB04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
WB04_TDg2 <- data.frame(WB04_TD)
WB04_TDg2 <- WB04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_TDg2$player1
player2vector <- WB04_TDg2$player2
WB04_TDg3 <- WB04_TDg2
WB04_TDg3$p1inp2vec <- is.element(WB04_TDg3$player1, player2vector)
WB04_TDg3$p2inp1vec <- is.element(WB04_TDg3$player2, player1vector)

addPlayer1 <- WB04_TDg3[ which(WB04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_TDg3[ which(WB04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_TDg2 <- rbind(WB04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
WB04_TDft <- ftable(WB04_TDg2$player1, WB04_TDg2$player2)
WB04_TDft2 <- as.matrix(WB04_TDft)
numRows <- nrow(WB04_TDft2)
numCols <- ncol(WB04_TDft2)
WB04_TDft3 <- WB04_TDft2[c(2:numRows) , c(2:numCols)]
WB04_TDTable <- graph.adjacency(WB04_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(WB04_TDTable, vertex.label = V(WB04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
WB04_TD.clusterCoef <- transitivity(WB04_TDTable, type="global") #cluster coefficient
WB04_TD.degreeCent <- centralization.degree(WB04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_TDftn <- as.network.matrix(WB04_TDft)
WB04_TD.netDensity <- network.density(WB04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_TD.entropy <- entropy(WB04_TDft) #entropy

WB04_TD.netMx <- cbind(WB04_TD.netMx, WB04_TD.clusterCoef, WB04_TD.degreeCent$centralization,
                       WB04_TD.netDensity, WB04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
WB04_QTg2 <- data.frame(WB04_QT)
WB04_QTg2 <- WB04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB04_QTg2$player1
player2vector <- WB04_QTg2$player2
WB04_QTg3 <- WB04_QTg2
WB04_QTg3$p1inp2vec <- is.element(WB04_QTg3$player1, player2vector)
WB04_QTg3$p2inp1vec <- is.element(WB04_QTg3$player2, player1vector)

addPlayer1 <- WB04_QTg3[ which(WB04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB04_QTg3[ which(WB04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB04_QTg2 <- rbind(WB04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
WB04_QTft <- ftable(WB04_QTg2$player1, WB04_QTg2$player2)
WB04_QTft2 <- as.matrix(WB04_QTft)
numRows <- nrow(WB04_QTft2)
numCols <- ncol(WB04_QTft2)
WB04_QTft3 <- WB04_QTft2[c(2:numRows) , c(2:numCols)]
WB04_QTTable <- graph.adjacency(WB04_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(WB04_QTTable, vertex.label = V(WB04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
WB04_QT.clusterCoef <- transitivity(WB04_QTTable, type="global") #cluster coefficient
WB04_QT.degreeCent <- centralization.degree(WB04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB04_QTftn <- as.network.matrix(WB04_QTft)
WB04_QT.netDensity <- network.density(WB04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB04_QT.entropy <- entropy(WB04_QTft) #entropy

WB04_QT.netMx <- cbind(WB04_QT.netMx, WB04_QT.clusterCoef, WB04_QT.degreeCent$centralization,
                       WB04_QT.netDensity, WB04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB04_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 4
##

#ROUND 4, Goal***************************************************************

round = 4
teamName = "WCE"
KIoutcome = "Goal_F"
WCE04_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Goal with weighted edges
WCE04_Gg2 <- data.frame(WCE04_G)
WCE04_Gg2 <- WCE04_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_Gg2$player1
player2vector <- WCE04_Gg2$player2
WCE04_Gg3 <- WCE04_Gg2
WCE04_Gg3$p1inp2vec <- is.element(WCE04_Gg3$player1, player2vector)
WCE04_Gg3$p2inp1vec <- is.element(WCE04_Gg3$player2, player1vector)

addPlayer1 <- WCE04_Gg3[ which(WCE04_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_Gg3[ which(WCE04_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_Gg2 <- rbind(WCE04_Gg2, addPlayers)

#ROUND 4, Goal graph using weighted edges
WCE04_Gft <- ftable(WCE04_Gg2$player1, WCE04_Gg2$player2)
WCE04_Gft2 <- as.matrix(WCE04_Gft)
numRows <- nrow(WCE04_Gft2)
numCols <- ncol(WCE04_Gft2)
WCE04_Gft3 <- WCE04_Gft2[c(2:numRows) , c(2:numCols)]
WCE04_GTable <- graph.adjacency(WCE04_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Goal graph=weighted
plot.igraph(WCE04_GTable, vertex.label = V(WCE04_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Goal calulation of network metrics
#igraph
WCE04_G.clusterCoef <- transitivity(WCE04_GTable, type="global") #cluster coefficient
WCE04_G.degreeCent <- centralization.degree(WCE04_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_Gftn <- as.network.matrix(WCE04_Gft)
WCE04_G.netDensity <- network.density(WCE04_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_G.entropy <- entropy(WCE04_Gft) #entropy

WCE04_G.netMx <- cbind(WCE04_G.netMx, WCE04_G.clusterCoef, WCE04_G.degreeCent$centralization,
                       WCE04_G.netDensity, WCE04_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_G.netMx) <- varnames

#ROUND 4, Behind***************************************************************
#NA

round = 4
teamName = "WCE"
KIoutcome = "Behind_F"
WCE04_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, Behind with weighted edges
WCE04_Bg2 <- data.frame(WCE04_B)
WCE04_Bg2 <- WCE04_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_Bg2$player1
player2vector <- WCE04_Bg2$player2
WCE04_Bg3 <- WCE04_Bg2
WCE04_Bg3$p1inp2vec <- is.element(WCE04_Bg3$player1, player2vector)
WCE04_Bg3$p2inp1vec <- is.element(WCE04_Bg3$player2, player1vector)

addPlayer1 <- WCE04_Bg3[ which(WCE04_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_Bg3[ which(WCE04_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_Bg2 <- rbind(WCE04_Bg2, addPlayers)

#ROUND 4, Behind graph using weighted edges
WCE04_Bft <- ftable(WCE04_Bg2$player1, WCE04_Bg2$player2)
WCE04_Bft2 <- as.matrix(WCE04_Bft)
numRows <- nrow(WCE04_Bft2)
numCols <- ncol(WCE04_Bft2)
WCE04_Bft3 <- WCE04_Bft2[c(2:numRows) , c(2:numCols)]
WCE04_BTable <- graph.adjacency(WCE04_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 4, Behind graph=weighted
plot.igraph(WCE04_BTable, vertex.label = V(WCE04_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, Behind calulation of network metrics
#igraph
WCE04_B.clusterCoef <- transitivity(WCE04_BTable, type="global") #cluster coefficient
WCE04_B.degreeCent <- centralization.degree(WCE04_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_Bftn <- as.network.matrix(WCE04_Bft)
WCE04_B.netDensity <- network.density(WCE04_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_B.entropy <- entropy(WCE04_Bft) #entropy

WCE04_B.netMx <- cbind(WCE04_B.netMx, WCE04_B.clusterCoef, WCE04_B.degreeCent$centralization,
                       WCE04_B.netDensity, WCE04_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_B.netMx) <- varnames

#ROUND 4, FWD Stoppage**********************************************************
#NA

round = 4
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE04_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Stoppage with weighted edges
WCE04_SFg2 <- data.frame(WCE04_SF)
WCE04_SFg2 <- WCE04_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_SFg2$player1
player2vector <- WCE04_SFg2$player2
WCE04_SFg3 <- WCE04_SFg2
WCE04_SFg3$p1inp2vec <- is.element(WCE04_SFg3$player1, player2vector)
WCE04_SFg3$p2inp1vec <- is.element(WCE04_SFg3$player2, player1vector)

addPlayer1 <- WCE04_SFg3[ which(WCE04_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE04_SFg2 <- rbind(WCE04_SFg2, addPlayer1)

#ROUND 4, FWD Stoppage graph using weighted edges
WCE04_SFft <- ftable(WCE04_SFg2$player1, WCE04_SFg2$player2)
WCE04_SFft2 <- as.matrix(WCE04_SFft)
numRows <- nrow(WCE04_SFft2)
numCols <- ncol(WCE04_SFft2)
WCE04_SFft3 <- WCE04_SFft2[c(2:numRows) , c(1:numCols)]
WCE04_SFTable <- graph.adjacency(WCE04_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Stoppage graph=weighted
plot.igraph(WCE04_SFTable, vertex.label = V(WCE04_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Stoppage calulation of network metrics
#igraph
WCE04_SF.clusterCoef <- transitivity(WCE04_SFTable, type="global") #cluster coefficient
WCE04_SF.degreeCent <- centralization.degree(WCE04_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_SFftn <- as.network.matrix(WCE04_SFft)
WCE04_SF.netDensity <- network.density(WCE04_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_SF.entropy <- entropy(WCE04_SFft) #entropy

WCE04_SF.netMx <- cbind(WCE04_SF.netMx, WCE04_SF.clusterCoef, WCE04_SF.degreeCent$centralization,
                        WCE04_SF.netDensity, WCE04_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_SF.netMx) <- varnames

#ROUND 4, FWD Turnover**********************************************************

round = 4
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE04_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, FWD Turnover with weighted edges
WCE04_TFg2 <- data.frame(WCE04_TF)
WCE04_TFg2 <- WCE04_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_TFg2$player1
player2vector <- WCE04_TFg2$player2
WCE04_TFg3 <- WCE04_TFg2
WCE04_TFg3$p1inp2vec <- is.element(WCE04_TFg3$player1, player2vector)
WCE04_TFg3$p2inp1vec <- is.element(WCE04_TFg3$player2, player1vector)

addPlayer1 <- WCE04_TFg3[ which(WCE04_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_TFg3[ which(WCE04_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_TFg2 <- rbind(WCE04_TFg2, addPlayers)

#ROUND 4, FWD Turnover graph using weighted edges
WCE04_TFft <- ftable(WCE04_TFg2$player1, WCE04_TFg2$player2)
WCE04_TFft2 <- as.matrix(WCE04_TFft)
numRows <- nrow(WCE04_TFft2)
numCols <- ncol(WCE04_TFft2)
WCE04_TFft3 <- WCE04_TFft2[c(2:numRows) , c(2:numCols)]
WCE04_TFTable <- graph.adjacency(WCE04_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, FWD Turnover graph=weighted
plot.igraph(WCE04_TFTable, vertex.label = V(WCE04_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, FWD Turnover calulation of network metrics
#igraph
WCE04_TF.clusterCoef <- transitivity(WCE04_TFTable, type="global") #cluster coefficient
WCE04_TF.degreeCent <- centralization.degree(WCE04_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_TFftn <- as.network.matrix(WCE04_TFft)
WCE04_TF.netDensity <- network.density(WCE04_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_TF.entropy <- entropy(WCE04_TFft) #entropy

WCE04_TF.netMx <- cbind(WCE04_TF.netMx, WCE04_TF.clusterCoef, WCE04_TF.degreeCent$centralization,
                        WCE04_TF.netDensity, WCE04_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_TF.netMx) <- varnames

#ROUND 4, AM Stoppage**********************************************************
#NA

round = 4
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE04_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Stoppage with weighted edges
WCE04_SAMg2 <- data.frame(WCE04_SAM)
WCE04_SAMg2 <- WCE04_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_SAMg2$player1
player2vector <- WCE04_SAMg2$player2
WCE04_SAMg3 <- WCE04_SAMg2
WCE04_SAMg3$p1inp2vec <- is.element(WCE04_SAMg3$player1, player2vector)
WCE04_SAMg3$p2inp1vec <- is.element(WCE04_SAMg3$player2, player1vector)

addPlayer1 <- WCE04_SAMg3[ which(WCE04_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_SAMg3[ which(WCE04_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_SAMg2 <- rbind(WCE04_SAMg2, addPlayers)

#ROUND 4, AM Stoppage graph using weighted edges
WCE04_SAMft <- ftable(WCE04_SAMg2$player1, WCE04_SAMg2$player2)
WCE04_SAMft2 <- as.matrix(WCE04_SAMft)
numRows <- nrow(WCE04_SAMft2)
numCols <- ncol(WCE04_SAMft2)
WCE04_SAMft3 <- WCE04_SAMft2[c(2:numRows) , c(2:numCols)]
WCE04_SAMTable <- graph.adjacency(WCE04_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Stoppage graph=weighted
plot.igraph(WCE04_SAMTable, vertex.label = V(WCE04_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Stoppage calulation of network metrics
#igraph
WCE04_SAM.clusterCoef <- transitivity(WCE04_SAMTable, type="global") #cluster coefficient
WCE04_SAM.degreeCent <- centralization.degree(WCE04_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_SAMftn <- as.network.matrix(WCE04_SAMft)
WCE04_SAM.netDensity <- network.density(WCE04_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_SAM.entropy <- entropy(WCE04_SAMft) #entropy

WCE04_SAM.netMx <- cbind(WCE04_SAM.netMx, WCE04_SAM.clusterCoef, WCE04_SAM.degreeCent$centralization,
                         WCE04_SAM.netDensity, WCE04_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_SAM.netMx) <- varnames

#ROUND 4, AM Turnover**********************************************************

round = 4
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE04_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, AM Turnover with weighted edges
WCE04_TAMg2 <- data.frame(WCE04_TAM)
WCE04_TAMg2 <- WCE04_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_TAMg2$player1
player2vector <- WCE04_TAMg2$player2
WCE04_TAMg3 <- WCE04_TAMg2
WCE04_TAMg3$p1inp2vec <- is.element(WCE04_TAMg3$player1, player2vector)
WCE04_TAMg3$p2inp1vec <- is.element(WCE04_TAMg3$player2, player1vector)

addPlayer1 <- WCE04_TAMg3[ which(WCE04_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_TAMg3[ which(WCE04_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_TAMg2 <- rbind(WCE04_TAMg2, addPlayers)

#ROUND 4, AM Turnover graph using weighted edges
WCE04_TAMft <- ftable(WCE04_TAMg2$player1, WCE04_TAMg2$player2)
WCE04_TAMft2 <- as.matrix(WCE04_TAMft)
numRows <- nrow(WCE04_TAMft2)
numCols <- ncol(WCE04_TAMft2)
WCE04_TAMft3 <- WCE04_TAMft2[c(2:numRows) , c(2:numCols)]
WCE04_TAMTable <- graph.adjacency(WCE04_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, AM Turnover graph=weighted
plot.igraph(WCE04_TAMTable, vertex.label = V(WCE04_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, AM Turnover calulation of network metrics
#igraph
WCE04_TAM.clusterCoef <- transitivity(WCE04_TAMTable, type="global") #cluster coefficient
WCE04_TAM.degreeCent <- centralization.degree(WCE04_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_TAMftn <- as.network.matrix(WCE04_TAMft)
WCE04_TAM.netDensity <- network.density(WCE04_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_TAM.entropy <- entropy(WCE04_TAMft) #entropy

WCE04_TAM.netMx <- cbind(WCE04_TAM.netMx, WCE04_TAM.clusterCoef, WCE04_TAM.degreeCent$centralization,
                         WCE04_TAM.netDensity, WCE04_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_TAM.netMx) <- varnames

#ROUND 4, DM Stoppage**********************************************************

round = 4
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE04_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Stoppage with weighted edges
WCE04_SDMg2 <- data.frame(WCE04_SDM)
WCE04_SDMg2 <- WCE04_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_SDMg2$player1
player2vector <- WCE04_SDMg2$player2
WCE04_SDMg3 <- WCE04_SDMg2
WCE04_SDMg3$p1inp2vec <- is.element(WCE04_SDMg3$player1, player2vector)
WCE04_SDMg3$p2inp1vec <- is.element(WCE04_SDMg3$player2, player1vector)

addPlayer1 <- WCE04_SDMg3[ which(WCE04_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_SDMg3[ which(WCE04_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_SDMg2 <- rbind(WCE04_SDMg2, addPlayers)

#ROUND 4, DM Stoppage graph using weighted edges
WCE04_SDMft <- ftable(WCE04_SDMg2$player1, WCE04_SDMg2$player2)
WCE04_SDMft2 <- as.matrix(WCE04_SDMft)
numRows <- nrow(WCE04_SDMft2)
numCols <- ncol(WCE04_SDMft2)
WCE04_SDMft3 <- WCE04_SDMft2[c(2:numRows) , c(2:numCols)]
WCE04_SDMTable <- graph.adjacency(WCE04_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Stoppage graph=weighted
plot.igraph(WCE04_SDMTable, vertex.label = V(WCE04_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Stoppage calulation of network metrics
#igraph
WCE04_SDM.clusterCoef <- transitivity(WCE04_SDMTable, type="global") #cluster coefficient
WCE04_SDM.degreeCent <- centralization.degree(WCE04_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_SDMftn <- as.network.matrix(WCE04_SDMft)
WCE04_SDM.netDensity <- network.density(WCE04_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_SDM.entropy <- entropy(WCE04_SDMft) #entropy

WCE04_SDM.netMx <- cbind(WCE04_SDM.netMx, WCE04_SDM.clusterCoef, WCE04_SDM.degreeCent$centralization,
                         WCE04_SDM.netDensity, WCE04_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_SDM.netMx) <- varnames

#ROUND 4, DM Turnover**********************************************************

round = 4
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE04_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, DM Turnover with weighted edges
WCE04_TDMg2 <- data.frame(WCE04_TDM)
WCE04_TDMg2 <- WCE04_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_TDMg2$player1
player2vector <- WCE04_TDMg2$player2
WCE04_TDMg3 <- WCE04_TDMg2
WCE04_TDMg3$p1inp2vec <- is.element(WCE04_TDMg3$player1, player2vector)
WCE04_TDMg3$p2inp1vec <- is.element(WCE04_TDMg3$player2, player1vector)

addPlayer1 <- WCE04_TDMg3[ which(WCE04_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_TDMg3[ which(WCE04_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_TDMg2 <- rbind(WCE04_TDMg2, addPlayers)

#ROUND 4, DM Turnover graph using weighted edges
WCE04_TDMft <- ftable(WCE04_TDMg2$player1, WCE04_TDMg2$player2)
WCE04_TDMft2 <- as.matrix(WCE04_TDMft)
numRows <- nrow(WCE04_TDMft2)
numCols <- ncol(WCE04_TDMft2)
WCE04_TDMft3 <- WCE04_TDMft2[c(2:numRows) , c(2:numCols)]
WCE04_TDMTable <- graph.adjacency(WCE04_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 4, DM Turnover graph=weighted
plot.igraph(WCE04_TDMTable, vertex.label = V(WCE04_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, DM Turnover calulation of network metrics
#igraph
WCE04_TDM.clusterCoef <- transitivity(WCE04_TDMTable, type="global") #cluster coefficient
WCE04_TDM.degreeCent <- centralization.degree(WCE04_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_TDMftn <- as.network.matrix(WCE04_TDMft)
WCE04_TDM.netDensity <- network.density(WCE04_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_TDM.entropy <- entropy(WCE04_TDMft) #entropy

WCE04_TDM.netMx <- cbind(WCE04_TDM.netMx, WCE04_TDM.clusterCoef, WCE04_TDM.degreeCent$centralization,
                         WCE04_TDM.netDensity, WCE04_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_TDM.netMx) <- varnames

#ROUND 4, D Stoppage**********************************************************
#NA

round = 4
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE04_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Stoppage with weighted edges
WCE04_SDg2 <- data.frame(WCE04_SD)
WCE04_SDg2 <- WCE04_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_SDg2$player1
player2vector <- WCE04_SDg2$player2
WCE04_SDg3 <- WCE04_SDg2
WCE04_SDg3$p1inp2vec <- is.element(WCE04_SDg3$player1, player2vector)
WCE04_SDg3$p2inp1vec <- is.element(WCE04_SDg3$player2, player1vector)

addPlayer1 <- WCE04_SDg3[ which(WCE04_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_SDg3[ which(WCE04_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_SDg2 <- rbind(WCE04_SDg2, addPlayers)

#ROUND 4, D Stoppage graph using weighted edges
WCE04_SDft <- ftable(WCE04_SDg2$player1, WCE04_SDg2$player2)
WCE04_SDft2 <- as.matrix(WCE04_SDft)
numRows <- nrow(WCE04_SDft2)
numCols <- ncol(WCE04_SDft2)
WCE04_SDft3 <- WCE04_SDft2[c(2:numRows) , c(2:numCols)]
WCE04_SDTable <- graph.adjacency(WCE04_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Stoppage graph=weighted
plot.igraph(WCE04_SDTable, vertex.label = V(WCE04_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Stoppage calulation of network metrics
#igraph
WCE04_SD.clusterCoef <- transitivity(WCE04_SDTable, type="global") #cluster coefficient
WCE04_SD.degreeCent <- centralization.degree(WCE04_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_SDftn <- as.network.matrix(WCE04_SDft)
WCE04_SD.netDensity <- network.density(WCE04_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_SD.entropy <- entropy(WCE04_SDft) #entropy

WCE04_SD.netMx <- cbind(WCE04_SD.netMx, WCE04_SD.clusterCoef, WCE04_SD.degreeCent$centralization,
                        WCE04_SD.netDensity, WCE04_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_SD.netMx) <- varnames

#ROUND 4, D Turnover**********************************************************
#NA

round = 4
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE04_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, D Turnover with weighted edges
WCE04_TDg2 <- data.frame(WCE04_TD)
WCE04_TDg2 <- WCE04_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_TDg2$player1
player2vector <- WCE04_TDg2$player2
WCE04_TDg3 <- WCE04_TDg2
WCE04_TDg3$p1inp2vec <- is.element(WCE04_TDg3$player1, player2vector)
WCE04_TDg3$p2inp1vec <- is.element(WCE04_TDg3$player2, player1vector)

addPlayer1 <- WCE04_TDg3[ which(WCE04_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_TDg3[ which(WCE04_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_TDg2 <- rbind(WCE04_TDg2, addPlayers)

#ROUND 4, D Turnover graph using weighted edges
WCE04_TDft <- ftable(WCE04_TDg2$player1, WCE04_TDg2$player2)
WCE04_TDft2 <- as.matrix(WCE04_TDft)
numRows <- nrow(WCE04_TDft2)
numCols <- ncol(WCE04_TDft2)
WCE04_TDft3 <- WCE04_TDft2[c(2:numRows) , c(2:numCols)]
WCE04_TDTable <- graph.adjacency(WCE04_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, D Turnover graph=weighted
plot.igraph(WCE04_TDTable, vertex.label = V(WCE04_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, D Turnover calulation of network metrics
#igraph
WCE04_TD.clusterCoef <- transitivity(WCE04_TDTable, type="global") #cluster coefficient
WCE04_TD.degreeCent <- centralization.degree(WCE04_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_TDftn <- as.network.matrix(WCE04_TDft)
WCE04_TD.netDensity <- network.density(WCE04_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_TD.entropy <- entropy(WCE04_TDft) #entropy

WCE04_TD.netMx <- cbind(WCE04_TD.netMx, WCE04_TD.clusterCoef, WCE04_TD.degreeCent$centralization,
                        WCE04_TD.netDensity, WCE04_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_TD.netMx) <- varnames

#ROUND 4, End of Qtr**********************************************************
#NA

round = 4
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE04_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 4, End of Qtr with weighted edges
WCE04_QTg2 <- data.frame(WCE04_QT)
WCE04_QTg2 <- WCE04_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE04_QTg2$player1
player2vector <- WCE04_QTg2$player2
WCE04_QTg3 <- WCE04_QTg2
WCE04_QTg3$p1inp2vec <- is.element(WCE04_QTg3$player1, player2vector)
WCE04_QTg3$p2inp1vec <- is.element(WCE04_QTg3$player2, player1vector)

addPlayer1 <- WCE04_QTg3[ which(WCE04_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE04_QTg3[ which(WCE04_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE04_QTg2 <- rbind(WCE04_QTg2, addPlayers)

#ROUND 4, End of Qtr graph using weighted edges
WCE04_QTft <- ftable(WCE04_QTg2$player1, WCE04_QTg2$player2)
WCE04_QTft2 <- as.matrix(WCE04_QTft)
numRows <- nrow(WCE04_QTft2)
numCols <- ncol(WCE04_QTft2)
WCE04_QTft3 <- WCE04_QTft2[c(2:numRows) , c(2:numCols)]
WCE04_QTTable <- graph.adjacency(WCE04_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 4, End of Qtr graph=weighted
plot.igraph(WCE04_QTTable, vertex.label = V(WCE04_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE04_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 4, End of Qtr calulation of network metrics
#igraph
WCE04_QT.clusterCoef <- transitivity(WCE04_QTTable, type="global") #cluster coefficient
WCE04_QT.degreeCent <- centralization.degree(WCE04_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE04_QTftn <- as.network.matrix(WCE04_QTft)
WCE04_QT.netDensity <- network.density(WCE04_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE04_QT.entropy <- entropy(WCE04_QTft) #entropy

WCE04_QT.netMx <- cbind(WCE04_QT.netMx, WCE04_QT.clusterCoef, WCE04_QT.degreeCent$centralization,
                        WCE04_QT.netDensity, WCE04_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE04_QT.netMx) <- varnames
