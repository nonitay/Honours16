#####
#09-30-16- Real data 17
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************

round = 17
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
ADEL17_Gg2 <- data.frame(ADEL17_G)
ADEL17_Gg2 <- ADEL17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_Gg2$player1
player2vector <- ADEL17_Gg2$player2
ADEL17_Gg3 <- ADEL17_Gg2
ADEL17_Gg3$p1inp2vec <- is.element(ADEL17_Gg3$player1, player2vector)
ADEL17_Gg3$p2inp1vec <- is.element(ADEL17_Gg3$player2, player1vector)

addPlayer1 <- ADEL17_Gg3[ which(ADEL17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_Gg3[ which(ADEL17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_Gg2 <- rbind(ADEL17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
ADEL17_Gft <- ftable(ADEL17_Gg2$player1, ADEL17_Gg2$player2)
ADEL17_Gft2 <- as.matrix(ADEL17_Gft)
numRows <- nrow(ADEL17_Gft2)
numCols <- ncol(ADEL17_Gft2)
ADEL17_Gft3 <- ADEL17_Gft2[c(2:numRows) , c(2:numCols)]
ADEL17_GTable <- graph.adjacency(ADEL17_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(ADEL17_GTable, vertex.label = V(ADEL17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
ADEL17_G.clusterCoef <- transitivity(ADEL17_GTable, type="global") #cluster coefficient
ADEL17_G.degreeCent <- centralization.degree(ADEL17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_Gftn <- as.network.matrix(ADEL17_Gft)
ADEL17_G.netDensity <- network.density(ADEL17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_G.entropy <- entropy(ADEL17_Gft) #entropy

ADEL17_G.netMx <- cbind(ADEL17_G.netMx, ADEL17_G.clusterCoef, ADEL17_G.degreeCent$centralization,
                        ADEL17_G.netDensity, ADEL17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
ADEL17_Bg2 <- data.frame(ADEL17_B)
ADEL17_Bg2 <- ADEL17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_Bg2$player1
player2vector <- ADEL17_Bg2$player2
ADEL17_Bg3 <- ADEL17_Bg2
ADEL17_Bg3$p1inp2vec <- is.element(ADEL17_Bg3$player1, player2vector)
ADEL17_Bg3$p2inp1vec <- is.element(ADEL17_Bg3$player2, player1vector)

addPlayer1 <- ADEL17_Bg3[ which(ADEL17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_Bg3[ which(ADEL17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_Bg2 <- rbind(ADEL17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
ADEL17_Bft <- ftable(ADEL17_Bg2$player1, ADEL17_Bg2$player2)
ADEL17_Bft2 <- as.matrix(ADEL17_Bft)
numRows <- nrow(ADEL17_Bft2)
numCols <- ncol(ADEL17_Bft2)
ADEL17_Bft3 <- ADEL17_Bft2[c(2:numRows) , c(2:numCols)]
ADEL17_BTable <- graph.adjacency(ADEL17_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(ADEL17_BTable, vertex.label = V(ADEL17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
ADEL17_B.clusterCoef <- transitivity(ADEL17_BTable, type="global") #cluster coefficient
ADEL17_B.degreeCent <- centralization.degree(ADEL17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_Bftn <- as.network.matrix(ADEL17_Bft)
ADEL17_B.netDensity <- network.density(ADEL17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_B.entropy <- entropy(ADEL17_Bft) #entropy

ADEL17_B.netMx <- cbind(ADEL17_B.netMx, ADEL17_B.clusterCoef, ADEL17_B.degreeCent$centralization,
                        ADEL17_B.netDensity, ADEL17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************

round = 17
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
ADEL17_SFg2 <- data.frame(ADEL17_SF)
ADEL17_SFg2 <- ADEL17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_SFg2$player1
player2vector <- ADEL17_SFg2$player2
ADEL17_SFg3 <- ADEL17_SFg2
ADEL17_SFg3$p1inp2vec <- is.element(ADEL17_SFg3$player1, player2vector)
ADEL17_SFg3$p2inp1vec <- is.element(ADEL17_SFg3$player2, player1vector)

addPlayer1 <- ADEL17_SFg3[ which(ADEL17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_SFg3[ which(ADEL17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_SFg2 <- rbind(ADEL17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
ADEL17_SFft <- ftable(ADEL17_SFg2$player1, ADEL17_SFg2$player2)
ADEL17_SFft2 <- as.matrix(ADEL17_SFft)
numRows <- nrow(ADEL17_SFft2)
numCols <- ncol(ADEL17_SFft2)
ADEL17_SFft3 <- ADEL17_SFft2[c(2:numRows) , c(2:numCols)]
ADEL17_SFTable <- graph.adjacency(ADEL17_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(ADEL17_SFTable, vertex.label = V(ADEL17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
ADEL17_SF.clusterCoef <- transitivity(ADEL17_SFTable, type="global") #cluster coefficient
ADEL17_SF.degreeCent <- centralization.degree(ADEL17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_SFftn <- as.network.matrix(ADEL17_SFft)
ADEL17_SF.netDensity <- network.density(ADEL17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_SF.entropy <- entropy(ADEL17_SFft) #entropy

ADEL17_SF.netMx <- cbind(ADEL17_SF.netMx, ADEL17_SF.clusterCoef, ADEL17_SF.degreeCent$centralization,
                         ADEL17_SF.netDensity, ADEL17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
ADEL17_TFg2 <- data.frame(ADEL17_TF)
ADEL17_TFg2 <- ADEL17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_TFg2$player1
player2vector <- ADEL17_TFg2$player2
ADEL17_TFg3 <- ADEL17_TFg2
ADEL17_TFg3$p1inp2vec <- is.element(ADEL17_TFg3$player1, player2vector)
ADEL17_TFg3$p2inp1vec <- is.element(ADEL17_TFg3$player2, player1vector)

addPlayer1 <- ADEL17_TFg3[ which(ADEL17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_TFg3[ which(ADEL17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_TFg2 <- rbind(ADEL17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
ADEL17_TFft <- ftable(ADEL17_TFg2$player1, ADEL17_TFg2$player2)
ADEL17_TFft2 <- as.matrix(ADEL17_TFft)
numRows <- nrow(ADEL17_TFft2)
numCols <- ncol(ADEL17_TFft2)
ADEL17_TFft3 <- ADEL17_TFft2[c(2:numRows) , c(2:numCols)]
ADEL17_TFTable <- graph.adjacency(ADEL17_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(ADEL17_TFTable, vertex.label = V(ADEL17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
ADEL17_TF.clusterCoef <- transitivity(ADEL17_TFTable, type="global") #cluster coefficient
ADEL17_TF.degreeCent <- centralization.degree(ADEL17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_TFftn <- as.network.matrix(ADEL17_TFft)
ADEL17_TF.netDensity <- network.density(ADEL17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_TF.entropy <- entropy(ADEL17_TFft) #entropy

ADEL17_TF.netMx <- cbind(ADEL17_TF.netMx, ADEL17_TF.clusterCoef, ADEL17_TF.degreeCent$centralization,
                         ADEL17_TF.netDensity, ADEL17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************
#NA

round = 17
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
ADEL17_SAMg2 <- data.frame(ADEL17_SAM)
ADEL17_SAMg2 <- ADEL17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_SAMg2$player1
player2vector <- ADEL17_SAMg2$player2
ADEL17_SAMg3 <- ADEL17_SAMg2
ADEL17_SAMg3$p1inp2vec <- is.element(ADEL17_SAMg3$player1, player2vector)
ADEL17_SAMg3$p2inp1vec <- is.element(ADEL17_SAMg3$player2, player1vector)

addPlayer1 <- ADEL17_SAMg3[ which(ADEL17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_SAMg3[ which(ADEL17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_SAMg2 <- rbind(ADEL17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
ADEL17_SAMft <- ftable(ADEL17_SAMg2$player1, ADEL17_SAMg2$player2)
ADEL17_SAMft2 <- as.matrix(ADEL17_SAMft)
numRows <- nrow(ADEL17_SAMft2)
numCols <- ncol(ADEL17_SAMft2)
ADEL17_SAMft3 <- ADEL17_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL17_SAMTable <- graph.adjacency(ADEL17_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(ADEL17_SAMTable, vertex.label = V(ADEL17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
ADEL17_SAM.clusterCoef <- transitivity(ADEL17_SAMTable, type="global") #cluster coefficient
ADEL17_SAM.degreeCent <- centralization.degree(ADEL17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_SAMftn <- as.network.matrix(ADEL17_SAMft)
ADEL17_SAM.netDensity <- network.density(ADEL17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_SAM.entropy <- entropy(ADEL17_SAMft) #entropy

ADEL17_SAM.netMx <- cbind(ADEL17_SAM.netMx, ADEL17_SAM.clusterCoef, ADEL17_SAM.degreeCent$centralization,
                          ADEL17_SAM.netDensity, ADEL17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
ADEL17_TAMg2 <- data.frame(ADEL17_TAM)
ADEL17_TAMg2 <- ADEL17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_TAMg2$player1
player2vector <- ADEL17_TAMg2$player2
ADEL17_TAMg3 <- ADEL17_TAMg2
ADEL17_TAMg3$p1inp2vec <- is.element(ADEL17_TAMg3$player1, player2vector)
ADEL17_TAMg3$p2inp1vec <- is.element(ADEL17_TAMg3$player2, player1vector)

addPlayer1 <- ADEL17_TAMg3[ which(ADEL17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_TAMg3[ which(ADEL17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_TAMg2 <- rbind(ADEL17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
ADEL17_TAMft <- ftable(ADEL17_TAMg2$player1, ADEL17_TAMg2$player2)
ADEL17_TAMft2 <- as.matrix(ADEL17_TAMft)
numRows <- nrow(ADEL17_TAMft2)
numCols <- ncol(ADEL17_TAMft2)
ADEL17_TAMft3 <- ADEL17_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL17_TAMTable <- graph.adjacency(ADEL17_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(ADEL17_TAMTable, vertex.label = V(ADEL17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
ADEL17_TAM.clusterCoef <- transitivity(ADEL17_TAMTable, type="global") #cluster coefficient
ADEL17_TAM.degreeCent <- centralization.degree(ADEL17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_TAMftn <- as.network.matrix(ADEL17_TAMft)
ADEL17_TAM.netDensity <- network.density(ADEL17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_TAM.entropy <- entropy(ADEL17_TAMft) #entropy

ADEL17_TAM.netMx <- cbind(ADEL17_TAM.netMx, ADEL17_TAM.clusterCoef, ADEL17_TAM.degreeCent$centralization,
                          ADEL17_TAM.netDensity, ADEL17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************
#NA

round = 17
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
ADEL17_SDMg2 <- data.frame(ADEL17_SDM)
ADEL17_SDMg2 <- ADEL17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_SDMg2$player1
player2vector <- ADEL17_SDMg2$player2
ADEL17_SDMg3 <- ADEL17_SDMg2
ADEL17_SDMg3$p1inp2vec <- is.element(ADEL17_SDMg3$player1, player2vector)
ADEL17_SDMg3$p2inp1vec <- is.element(ADEL17_SDMg3$player2, player1vector)

addPlayer1 <- ADEL17_SDMg3[ which(ADEL17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_SDMg3[ which(ADEL17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_SDMg2 <- rbind(ADEL17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
ADEL17_SDMft <- ftable(ADEL17_SDMg2$player1, ADEL17_SDMg2$player2)
ADEL17_SDMft2 <- as.matrix(ADEL17_SDMft)
numRows <- nrow(ADEL17_SDMft2)
numCols <- ncol(ADEL17_SDMft2)
ADEL17_SDMft3 <- ADEL17_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL17_SDMTable <- graph.adjacency(ADEL17_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(ADEL17_SDMTable, vertex.label = V(ADEL17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
ADEL17_SDM.clusterCoef <- transitivity(ADEL17_SDMTable, type="global") #cluster coefficient
ADEL17_SDM.degreeCent <- centralization.degree(ADEL17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_SDMftn <- as.network.matrix(ADEL17_SDMft)
ADEL17_SDM.netDensity <- network.density(ADEL17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_SDM.entropy <- entropy(ADEL17_SDMft) #entropy

ADEL17_SDM.netMx <- cbind(ADEL17_SDM.netMx, ADEL17_SDM.clusterCoef, ADEL17_SDM.degreeCent$centralization,
                          ADEL17_SDM.netDensity, ADEL17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
ADEL17_TDMg2 <- data.frame(ADEL17_TDM)
ADEL17_TDMg2 <- ADEL17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_TDMg2$player1
player2vector <- ADEL17_TDMg2$player2
ADEL17_TDMg3 <- ADEL17_TDMg2
ADEL17_TDMg3$p1inp2vec <- is.element(ADEL17_TDMg3$player1, player2vector)
ADEL17_TDMg3$p2inp1vec <- is.element(ADEL17_TDMg3$player2, player1vector)

addPlayer1 <- ADEL17_TDMg3[ which(ADEL17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_TDMg3[ which(ADEL17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_TDMg2 <- rbind(ADEL17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
ADEL17_TDMft <- ftable(ADEL17_TDMg2$player1, ADEL17_TDMg2$player2)
ADEL17_TDMft2 <- as.matrix(ADEL17_TDMft)
numRows <- nrow(ADEL17_TDMft2)
numCols <- ncol(ADEL17_TDMft2)
ADEL17_TDMft3 <- ADEL17_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL17_TDMTable <- graph.adjacency(ADEL17_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(ADEL17_TDMTable, vertex.label = V(ADEL17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
ADEL17_TDM.clusterCoef <- transitivity(ADEL17_TDMTable, type="global") #cluster coefficient
ADEL17_TDM.degreeCent <- centralization.degree(ADEL17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_TDMftn <- as.network.matrix(ADEL17_TDMft)
ADEL17_TDM.netDensity <- network.density(ADEL17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_TDM.entropy <- entropy(ADEL17_TDMft) #entropy

ADEL17_TDM.netMx <- cbind(ADEL17_TDM.netMx, ADEL17_TDM.clusterCoef, ADEL17_TDM.degreeCent$centralization,
                          ADEL17_TDM.netDensity, ADEL17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
ADEL17_SDg2 <- data.frame(ADEL17_SD)
ADEL17_SDg2 <- ADEL17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_SDg2$player1
player2vector <- ADEL17_SDg2$player2
ADEL17_SDg3 <- ADEL17_SDg2
ADEL17_SDg3$p1inp2vec <- is.element(ADEL17_SDg3$player1, player2vector)
ADEL17_SDg3$p2inp1vec <- is.element(ADEL17_SDg3$player2, player1vector)

addPlayer1 <- ADEL17_SDg3[ which(ADEL17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_SDg3[ which(ADEL17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_SDg2 <- rbind(ADEL17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
ADEL17_SDft <- ftable(ADEL17_SDg2$player1, ADEL17_SDg2$player2)
ADEL17_SDft2 <- as.matrix(ADEL17_SDft)
numRows <- nrow(ADEL17_SDft2)
numCols <- ncol(ADEL17_SDft2)
ADEL17_SDft3 <- ADEL17_SDft2[c(2:numRows) , c(2:numCols)]
ADEL17_SDTable <- graph.adjacency(ADEL17_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(ADEL17_SDTable, vertex.label = V(ADEL17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
ADEL17_SD.clusterCoef <- transitivity(ADEL17_SDTable, type="global") #cluster coefficient
ADEL17_SD.degreeCent <- centralization.degree(ADEL17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_SDftn <- as.network.matrix(ADEL17_SDft)
ADEL17_SD.netDensity <- network.density(ADEL17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_SD.entropy <- entropy(ADEL17_SDft) #entropy

ADEL17_SD.netMx <- cbind(ADEL17_SD.netMx, ADEL17_SD.clusterCoef, ADEL17_SD.degreeCent$centralization,
                         ADEL17_SD.netDensity, ADEL17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
ADEL17_TDg2 <- data.frame(ADEL17_TD)
ADEL17_TDg2 <- ADEL17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_TDg2$player1
player2vector <- ADEL17_TDg2$player2
ADEL17_TDg3 <- ADEL17_TDg2
ADEL17_TDg3$p1inp2vec <- is.element(ADEL17_TDg3$player1, player2vector)
ADEL17_TDg3$p2inp1vec <- is.element(ADEL17_TDg3$player2, player1vector)

addPlayer1 <- ADEL17_TDg3[ which(ADEL17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_TDg3[ which(ADEL17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_TDg2 <- rbind(ADEL17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
ADEL17_TDft <- ftable(ADEL17_TDg2$player1, ADEL17_TDg2$player2)
ADEL17_TDft2 <- as.matrix(ADEL17_TDft)
numRows <- nrow(ADEL17_TDft2)
numCols <- ncol(ADEL17_TDft2)
ADEL17_TDft3 <- ADEL17_TDft2[c(2:numRows) , c(2:numCols)]
ADEL17_TDTable <- graph.adjacency(ADEL17_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(ADEL17_TDTable, vertex.label = V(ADEL17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
ADEL17_TD.clusterCoef <- transitivity(ADEL17_TDTable, type="global") #cluster coefficient
ADEL17_TD.degreeCent <- centralization.degree(ADEL17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_TDftn <- as.network.matrix(ADEL17_TDft)
ADEL17_TD.netDensity <- network.density(ADEL17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_TD.entropy <- entropy(ADEL17_TDft) #entropy

ADEL17_TD.netMx <- cbind(ADEL17_TD.netMx, ADEL17_TD.clusterCoef, ADEL17_TD.degreeCent$centralization,
                         ADEL17_TD.netDensity, ADEL17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
ADEL17_QTg2 <- data.frame(ADEL17_QT)
ADEL17_QTg2 <- ADEL17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL17_QTg2$player1
player2vector <- ADEL17_QTg2$player2
ADEL17_QTg3 <- ADEL17_QTg2
ADEL17_QTg3$p1inp2vec <- is.element(ADEL17_QTg3$player1, player2vector)
ADEL17_QTg3$p2inp1vec <- is.element(ADEL17_QTg3$player2, player1vector)

addPlayer1 <- ADEL17_QTg3[ which(ADEL17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL17_QTg3[ which(ADEL17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL17_QTg2 <- rbind(ADEL17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
ADEL17_QTft <- ftable(ADEL17_QTg2$player1, ADEL17_QTg2$player2)
ADEL17_QTft2 <- as.matrix(ADEL17_QTft)
numRows <- nrow(ADEL17_QTft2)
numCols <- ncol(ADEL17_QTft2)
ADEL17_QTft3 <- ADEL17_QTft2[c(2:numRows) , c(2:numCols)]
ADEL17_QTTable <- graph.adjacency(ADEL17_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(ADEL17_QTTable, vertex.label = V(ADEL17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
ADEL17_QT.clusterCoef <- transitivity(ADEL17_QTTable, type="global") #cluster coefficient
ADEL17_QT.degreeCent <- centralization.degree(ADEL17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL17_QTftn <- as.network.matrix(ADEL17_QTft)
ADEL17_QT.netDensity <- network.density(ADEL17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL17_QT.entropy <- entropy(ADEL17_QTft) #entropy

ADEL17_QT.netMx <- cbind(ADEL17_QT.netMx, ADEL17_QT.clusterCoef, ADEL17_QT.degreeCent$centralization,
                         ADEL17_QT.netDensity, ADEL17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL17_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************
#NA

round = 17
teamName = "BL"
KIoutcome = "Goal_F"
BL17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
BL17_Gg2 <- data.frame(BL17_G)
BL17_Gg2 <- BL17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_Gg2$player1
player2vector <- BL17_Gg2$player2
BL17_Gg3 <- BL17_Gg2
BL17_Gg3$p1inp2vec <- is.element(BL17_Gg3$player1, player2vector)
BL17_Gg3$p2inp1vec <- is.element(BL17_Gg3$player2, player1vector)

addPlayer1 <- BL17_Gg3[ which(BL17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_Gg3[ which(BL17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_Gg2 <- rbind(BL17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
BL17_Gft <- ftable(BL17_Gg2$player1, BL17_Gg2$player2)
BL17_Gft2 <- as.matrix(BL17_Gft)
numRows <- nrow(BL17_Gft2)
numCols <- ncol(BL17_Gft2)
BL17_Gft3 <- BL17_Gft2[c(2:numRows) , c(2:numCols)]
BL17_GTable <- graph.adjacency(BL17_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(BL17_GTable, vertex.label = V(BL17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
BL17_G.clusterCoef <- transitivity(BL17_GTable, type="global") #cluster coefficient
BL17_G.degreeCent <- centralization.degree(BL17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_Gftn <- as.network.matrix(BL17_Gft)
BL17_G.netDensity <- network.density(BL17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_G.entropy <- entropy(BL17_Gft) #entropy

BL17_G.netMx <- cbind(BL17_G.netMx, BL17_G.clusterCoef, BL17_G.degreeCent$centralization,
                      BL17_G.netDensity, BL17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "BL"
KIoutcome = "Behind_F"
BL17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
BL17_Bg2 <- data.frame(BL17_B)
BL17_Bg2 <- BL17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_Bg2$player1
player2vector <- BL17_Bg2$player2
BL17_Bg3 <- BL17_Bg2
BL17_Bg3$p1inp2vec <- is.element(BL17_Bg3$player1, player2vector)
BL17_Bg3$p2inp1vec <- is.element(BL17_Bg3$player2, player1vector)

addPlayer1 <- BL17_Bg3[ which(BL17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_Bg3[ which(BL17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_Bg2 <- rbind(BL17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
BL17_Bft <- ftable(BL17_Bg2$player1, BL17_Bg2$player2)
BL17_Bft2 <- as.matrix(BL17_Bft)
numRows <- nrow(BL17_Bft2)
numCols <- ncol(BL17_Bft2)
BL17_Bft3 <- BL17_Bft2[c(2:numRows) , c(2:numCols)]
BL17_BTable <- graph.adjacency(BL17_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(BL17_BTable, vertex.label = V(BL17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
BL17_B.clusterCoef <- transitivity(BL17_BTable, type="global") #cluster coefficient
BL17_B.degreeCent <- centralization.degree(BL17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_Bftn <- as.network.matrix(BL17_Bft)
BL17_B.netDensity <- network.density(BL17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_B.entropy <- entropy(BL17_Bft) #entropy

BL17_B.netMx <- cbind(BL17_B.netMx, BL17_B.clusterCoef, BL17_B.degreeCent$centralization,
                      BL17_B.netDensity, BL17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "BL"
KIoutcome = "Stoppage_F"
BL17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
BL17_SFg2 <- data.frame(BL17_SF)
BL17_SFg2 <- BL17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_SFg2$player1
player2vector <- BL17_SFg2$player2
BL17_SFg3 <- BL17_SFg2
BL17_SFg3$p1inp2vec <- is.element(BL17_SFg3$player1, player2vector)
BL17_SFg3$p2inp1vec <- is.element(BL17_SFg3$player2, player1vector)

addPlayer1 <- BL17_SFg3[ which(BL17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_SFg3[ which(BL17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_SFg2 <- rbind(BL17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
BL17_SFft <- ftable(BL17_SFg2$player1, BL17_SFg2$player2)
BL17_SFft2 <- as.matrix(BL17_SFft)
numRows <- nrow(BL17_SFft2)
numCols <- ncol(BL17_SFft2)
BL17_SFft3 <- BL17_SFft2[c(2:numRows) , c(2:numCols)]
BL17_SFTable <- graph.adjacency(BL17_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(BL17_SFTable, vertex.label = V(BL17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
BL17_SF.clusterCoef <- transitivity(BL17_SFTable, type="global") #cluster coefficient
BL17_SF.degreeCent <- centralization.degree(BL17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_SFftn <- as.network.matrix(BL17_SFft)
BL17_SF.netDensity <- network.density(BL17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_SF.entropy <- entropy(BL17_SFft) #entropy

BL17_SF.netMx <- cbind(BL17_SF.netMx, BL17_SF.clusterCoef, BL17_SF.degreeCent$centralization,
                       BL17_SF.netDensity, BL17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************

round = 17
teamName = "BL"
KIoutcome = "Turnover_F"
BL17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
BL17_TFg2 <- data.frame(BL17_TF)
BL17_TFg2 <- BL17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_TFg2$player1
player2vector <- BL17_TFg2$player2
BL17_TFg3 <- BL17_TFg2
BL17_TFg3$p1inp2vec <- is.element(BL17_TFg3$player1, player2vector)
BL17_TFg3$p2inp1vec <- is.element(BL17_TFg3$player2, player1vector)

addPlayer1 <- BL17_TFg3[ which(BL17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_TFg3[ which(BL17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_TFg2 <- rbind(BL17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
BL17_TFft <- ftable(BL17_TFg2$player1, BL17_TFg2$player2)
BL17_TFft2 <- as.matrix(BL17_TFft)
numRows <- nrow(BL17_TFft2)
numCols <- ncol(BL17_TFft2)
BL17_TFft3 <- BL17_TFft2[c(2:numRows) , c(2:numCols)]
BL17_TFTable <- graph.adjacency(BL17_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(BL17_TFTable, vertex.label = V(BL17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
BL17_TF.clusterCoef <- transitivity(BL17_TFTable, type="global") #cluster coefficient
BL17_TF.degreeCent <- centralization.degree(BL17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_TFftn <- as.network.matrix(BL17_TFft)
BL17_TF.netDensity <- network.density(BL17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_TF.entropy <- entropy(BL17_TFft) #entropy

BL17_TF.netMx <- cbind(BL17_TF.netMx, BL17_TF.clusterCoef, BL17_TF.degreeCent$centralization,
                       BL17_TF.netDensity, BL17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************
#NA

round = 17
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
BL17_SAMg2 <- data.frame(BL17_SAM)
BL17_SAMg2 <- BL17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_SAMg2$player1
player2vector <- BL17_SAMg2$player2
BL17_SAMg3 <- BL17_SAMg2
BL17_SAMg3$p1inp2vec <- is.element(BL17_SAMg3$player1, player2vector)
BL17_SAMg3$p2inp1vec <- is.element(BL17_SAMg3$player2, player1vector)

addPlayer1 <- BL17_SAMg3[ which(BL17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_SAMg3[ which(BL17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_SAMg2 <- rbind(BL17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
BL17_SAMft <- ftable(BL17_SAMg2$player1, BL17_SAMg2$player2)
BL17_SAMft2 <- as.matrix(BL17_SAMft)
numRows <- nrow(BL17_SAMft2)
numCols <- ncol(BL17_SAMft2)
BL17_SAMft3 <- BL17_SAMft2[c(2:numRows) , c(2:numCols)]
BL17_SAMTable <- graph.adjacency(BL17_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(BL17_SAMTable, vertex.label = V(BL17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
BL17_SAM.clusterCoef <- transitivity(BL17_SAMTable, type="global") #cluster coefficient
BL17_SAM.degreeCent <- centralization.degree(BL17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_SAMftn <- as.network.matrix(BL17_SAMft)
BL17_SAM.netDensity <- network.density(BL17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_SAM.entropy <- entropy(BL17_SAMft) #entropy

BL17_SAM.netMx <- cbind(BL17_SAM.netMx, BL17_SAM.clusterCoef, BL17_SAM.degreeCent$centralization,
                        BL17_SAM.netDensity, BL17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "BL"
KIoutcome = "Turnover_AM"
BL17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
BL17_TAMg2 <- data.frame(BL17_TAM)
BL17_TAMg2 <- BL17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_TAMg2$player1
player2vector <- BL17_TAMg2$player2
BL17_TAMg3 <- BL17_TAMg2
BL17_TAMg3$p1inp2vec <- is.element(BL17_TAMg3$player1, player2vector)
BL17_TAMg3$p2inp1vec <- is.element(BL17_TAMg3$player2, player1vector)

addPlayer1 <- BL17_TAMg3[ which(BL17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL17_TAMg3[ which(BL17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_TAMg2 <- rbind(BL17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
BL17_TAMft <- ftable(BL17_TAMg2$player1, BL17_TAMg2$player2)
BL17_TAMft2 <- as.matrix(BL17_TAMft)
numRows <- nrow(BL17_TAMft2)
numCols <- ncol(BL17_TAMft2)
BL17_TAMft3 <- BL17_TAMft2[c(2:numRows) , c(2:numCols)]
BL17_TAMTable <- graph.adjacency(BL17_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(BL17_TAMTable, vertex.label = V(BL17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
BL17_TAM.clusterCoef <- transitivity(BL17_TAMTable, type="global") #cluster coefficient
BL17_TAM.degreeCent <- centralization.degree(BL17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_TAMftn <- as.network.matrix(BL17_TAMft)
BL17_TAM.netDensity <- network.density(BL17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_TAM.entropy <- entropy(BL17_TAMft) #entropy

BL17_TAM.netMx <- cbind(BL17_TAM.netMx, BL17_TAM.clusterCoef, BL17_TAM.degreeCent$centralization,
                        BL17_TAM.netDensity, BL17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************

round = 17
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
BL17_SDMg2 <- data.frame(BL17_SDM)
BL17_SDMg2 <- BL17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_SDMg2$player1
player2vector <- BL17_SDMg2$player2
BL17_SDMg3 <- BL17_SDMg2
BL17_SDMg3$p1inp2vec <- is.element(BL17_SDMg3$player1, player2vector)
BL17_SDMg3$p2inp1vec <- is.element(BL17_SDMg3$player2, player1vector)

addPlayer1 <- BL17_SDMg3[ which(BL17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_SDMg3[ which(BL17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_SDMg2 <- rbind(BL17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
BL17_SDMft <- ftable(BL17_SDMg2$player1, BL17_SDMg2$player2)
BL17_SDMft2 <- as.matrix(BL17_SDMft)
numRows <- nrow(BL17_SDMft2)
numCols <- ncol(BL17_SDMft2)
BL17_SDMft3 <- BL17_SDMft2[c(2:numRows) , c(2:numCols)]
BL17_SDMTable <- graph.adjacency(BL17_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(BL17_SDMTable, vertex.label = V(BL17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
BL17_SDM.clusterCoef <- transitivity(BL17_SDMTable, type="global") #cluster coefficient
BL17_SDM.degreeCent <- centralization.degree(BL17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_SDMftn <- as.network.matrix(BL17_SDMft)
BL17_SDM.netDensity <- network.density(BL17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_SDM.entropy <- entropy(BL17_SDMft) #entropy

BL17_SDM.netMx <- cbind(BL17_SDM.netMx, BL17_SDM.clusterCoef, BL17_SDM.degreeCent$centralization,
                        BL17_SDM.netDensity, BL17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "BL"
KIoutcome = "Turnover_DM"
BL17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
BL17_TDMg2 <- data.frame(BL17_TDM)
BL17_TDMg2 <- BL17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_TDMg2$player1
player2vector <- BL17_TDMg2$player2
BL17_TDMg3 <- BL17_TDMg2
BL17_TDMg3$p1inp2vec <- is.element(BL17_TDMg3$player1, player2vector)
BL17_TDMg3$p2inp1vec <- is.element(BL17_TDMg3$player2, player1vector)

addPlayer1 <- BL17_TDMg3[ which(BL17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_TDMg3[ which(BL17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_TDMg2 <- rbind(BL17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
BL17_TDMft <- ftable(BL17_TDMg2$player1, BL17_TDMg2$player2)
BL17_TDMft2 <- as.matrix(BL17_TDMft)
numRows <- nrow(BL17_TDMft2)
numCols <- ncol(BL17_TDMft2)
BL17_TDMft3 <- BL17_TDMft2[c(2:numRows) , c(2:numCols)]
BL17_TDMTable <- graph.adjacency(BL17_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(BL17_TDMTable, vertex.label = V(BL17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
BL17_TDM.clusterCoef <- transitivity(BL17_TDMTable, type="global") #cluster coefficient
BL17_TDM.degreeCent <- centralization.degree(BL17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_TDMftn <- as.network.matrix(BL17_TDMft)
BL17_TDM.netDensity <- network.density(BL17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_TDM.entropy <- entropy(BL17_TDMft) #entropy

BL17_TDM.netMx <- cbind(BL17_TDM.netMx, BL17_TDM.clusterCoef, BL17_TDM.degreeCent$centralization,
                        BL17_TDM.netDensity, BL17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************

round = 17
teamName = "BL"
KIoutcome = "Stoppage_D"
BL17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
BL17_SDg2 <- data.frame(BL17_SD)
BL17_SDg2 <- BL17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_SDg2$player1
player2vector <- BL17_SDg2$player2
BL17_SDg3 <- BL17_SDg2
BL17_SDg3$p1inp2vec <- is.element(BL17_SDg3$player1, player2vector)
BL17_SDg3$p2inp1vec <- is.element(BL17_SDg3$player2, player1vector)

addPlayer1 <- BL17_SDg3[ which(BL17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_SDg3[ which(BL17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_SDg2 <- rbind(BL17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
BL17_SDft <- ftable(BL17_SDg2$player1, BL17_SDg2$player2)
BL17_SDft2 <- as.matrix(BL17_SDft)
numRows <- nrow(BL17_SDft2)
numCols <- ncol(BL17_SDft2)
BL17_SDft3 <- BL17_SDft2[c(2:numRows) , c(2:numCols)]
BL17_SDTable <- graph.adjacency(BL17_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(BL17_SDTable, vertex.label = V(BL17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
BL17_SD.clusterCoef <- transitivity(BL17_SDTable, type="global") #cluster coefficient
BL17_SD.degreeCent <- centralization.degree(BL17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_SDftn <- as.network.matrix(BL17_SDft)
BL17_SD.netDensity <- network.density(BL17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_SD.entropy <- entropy(BL17_SDft) #entropy

BL17_SD.netMx <- cbind(BL17_SD.netMx, BL17_SD.clusterCoef, BL17_SD.degreeCent$centralization,
                       BL17_SD.netDensity, BL17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************

round = 17
teamName = "BL"
KIoutcome = "Turnover_D"
BL17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
BL17_TDg2 <- data.frame(BL17_TD)
BL17_TDg2 <- BL17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_TDg2$player1
player2vector <- BL17_TDg2$player2
BL17_TDg3 <- BL17_TDg2
BL17_TDg3$p1inp2vec <- is.element(BL17_TDg3$player1, player2vector)
BL17_TDg3$p2inp1vec <- is.element(BL17_TDg3$player2, player1vector)

addPlayer1 <- BL17_TDg3[ which(BL17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_TDg3[ which(BL17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_TDg2 <- rbind(BL17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
BL17_TDft <- ftable(BL17_TDg2$player1, BL17_TDg2$player2)
BL17_TDft2 <- as.matrix(BL17_TDft)
numRows <- nrow(BL17_TDft2)
numCols <- ncol(BL17_TDft2)
BL17_TDft3 <- BL17_TDft2[c(2:numRows) , c(2:numCols)]
BL17_TDTable <- graph.adjacency(BL17_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(BL17_TDTable, vertex.label = V(BL17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
BL17_TD.clusterCoef <- transitivity(BL17_TDTable, type="global") #cluster coefficient
BL17_TD.degreeCent <- centralization.degree(BL17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_TDftn <- as.network.matrix(BL17_TDft)
BL17_TD.netDensity <- network.density(BL17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_TD.entropy <- entropy(BL17_TDft) #entropy

BL17_TD.netMx <- cbind(BL17_TD.netMx, BL17_TD.clusterCoef, BL17_TD.degreeCent$centralization,
                       BL17_TD.netDensity, BL17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
BL17_QTg2 <- data.frame(BL17_QT)
BL17_QTg2 <- BL17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL17_QTg2$player1
player2vector <- BL17_QTg2$player2
BL17_QTg3 <- BL17_QTg2
BL17_QTg3$p1inp2vec <- is.element(BL17_QTg3$player1, player2vector)
BL17_QTg3$p2inp1vec <- is.element(BL17_QTg3$player2, player1vector)

addPlayer1 <- BL17_QTg3[ which(BL17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL17_QTg3[ which(BL17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL17_QTg2 <- rbind(BL17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
BL17_QTft <- ftable(BL17_QTg2$player1, BL17_QTg2$player2)
BL17_QTft2 <- as.matrix(BL17_QTft)
numRows <- nrow(BL17_QTft2)
numCols <- ncol(BL17_QTft2)
BL17_QTft3 <- BL17_QTft2[c(2:numRows) , c(2:numCols)]
BL17_QTTable <- graph.adjacency(BL17_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(BL17_QTTable, vertex.label = V(BL17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
BL17_QT.clusterCoef <- transitivity(BL17_QTTable, type="global") #cluster coefficient
BL17_QT.degreeCent <- centralization.degree(BL17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL17_QTftn <- as.network.matrix(BL17_QTft)
BL17_QT.netDensity <- network.density(BL17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL17_QT.entropy <- entropy(BL17_QTft) #entropy

BL17_QT.netMx <- cbind(BL17_QT.netMx, BL17_QT.clusterCoef, BL17_QT.degreeCent$centralization,
                       BL17_QT.netDensity, BL17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL17_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************
#NA

round = 17
teamName = "CARL"
KIoutcome = "Goal_F"
CARL17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
CARL17_Gg2 <- data.frame(CARL17_G)
CARL17_Gg2 <- CARL17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_Gg2$player1
player2vector <- CARL17_Gg2$player2
CARL17_Gg3 <- CARL17_Gg2
CARL17_Gg3$p1inp2vec <- is.element(CARL17_Gg3$player1, player2vector)
CARL17_Gg3$p2inp1vec <- is.element(CARL17_Gg3$player2, player1vector)

addPlayer1 <- CARL17_Gg3[ which(CARL17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL17_Gg3[ which(CARL17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_Gg2 <- rbind(CARL17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
CARL17_Gft <- ftable(CARL17_Gg2$player1, CARL17_Gg2$player2)
CARL17_Gft2 <- as.matrix(CARL17_Gft)
numRows <- nrow(CARL17_Gft2)
numCols <- ncol(CARL17_Gft2)
CARL17_Gft3 <- CARL17_Gft2[c(2:numRows) , c(2:numCols)]
CARL17_GTable <- graph.adjacency(CARL17_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(CARL17_GTable, vertex.label = V(CARL17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
CARL17_G.clusterCoef <- transitivity(CARL17_GTable, type="global") #cluster coefficient
CARL17_G.degreeCent <- centralization.degree(CARL17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_Gftn <- as.network.matrix(CARL17_Gft)
CARL17_G.netDensity <- network.density(CARL17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_G.entropy <- entropy(CARL17_Gft) #entropy

CARL17_G.netMx <- cbind(CARL17_G.netMx, CARL17_G.clusterCoef, CARL17_G.degreeCent$centralization,
                        CARL17_G.netDensity, CARL17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************

round = 17
teamName = "CARL"
KIoutcome = "Behind_F"
CARL17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
CARL17_Bg2 <- data.frame(CARL17_B)
CARL17_Bg2 <- CARL17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_Bg2$player1
player2vector <- CARL17_Bg2$player2
CARL17_Bg3 <- CARL17_Bg2
CARL17_Bg3$p1inp2vec <- is.element(CARL17_Bg3$player1, player2vector)
CARL17_Bg3$p2inp1vec <- is.element(CARL17_Bg3$player2, player1vector)

addPlayer1 <- CARL17_Bg3[ which(CARL17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL17_Bg3[ which(CARL17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_Bg2 <- rbind(CARL17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
CARL17_Bft <- ftable(CARL17_Bg2$player1, CARL17_Bg2$player2)
CARL17_Bft2 <- as.matrix(CARL17_Bft)
numRows <- nrow(CARL17_Bft2)
numCols <- ncol(CARL17_Bft2)
CARL17_Bft3 <- CARL17_Bft2[c(2:numRows) , c(2:numCols)]
CARL17_BTable <- graph.adjacency(CARL17_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(CARL17_BTable, vertex.label = V(CARL17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
CARL17_B.clusterCoef <- transitivity(CARL17_BTable, type="global") #cluster coefficient
CARL17_B.degreeCent <- centralization.degree(CARL17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_Bftn <- as.network.matrix(CARL17_Bft)
CARL17_B.netDensity <- network.density(CARL17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_B.entropy <- entropy(CARL17_Bft) #entropy

CARL17_B.netMx <- cbind(CARL17_B.netMx, CARL17_B.clusterCoef, CARL17_B.degreeCent$centralization,
                        CARL17_B.netDensity, CARL17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
CARL17_SFg2 <- data.frame(CARL17_SF)
CARL17_SFg2 <- CARL17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_SFg2$player1
player2vector <- CARL17_SFg2$player2
CARL17_SFg3 <- CARL17_SFg2
CARL17_SFg3$p1inp2vec <- is.element(CARL17_SFg3$player1, player2vector)
CARL17_SFg3$p2inp1vec <- is.element(CARL17_SFg3$player2, player1vector)

addPlayer1 <- CARL17_SFg3[ which(CARL17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL17_SFg3[ which(CARL17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_SFg2 <- rbind(CARL17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
CARL17_SFft <- ftable(CARL17_SFg2$player1, CARL17_SFg2$player2)
CARL17_SFft2 <- as.matrix(CARL17_SFft)
numRows <- nrow(CARL17_SFft2)
numCols <- ncol(CARL17_SFft2)
CARL17_SFft3 <- CARL17_SFft2[c(2:numRows) , c(2:numCols)]
CARL17_SFTable <- graph.adjacency(CARL17_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(CARL17_SFTable, vertex.label = V(CARL17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
CARL17_SF.clusterCoef <- transitivity(CARL17_SFTable, type="global") #cluster coefficient
CARL17_SF.degreeCent <- centralization.degree(CARL17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_SFftn <- as.network.matrix(CARL17_SFft)
CARL17_SF.netDensity <- network.density(CARL17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_SF.entropy <- entropy(CARL17_SFft) #entropy

CARL17_SF.netMx <- cbind(CARL17_SF.netMx, CARL17_SF.clusterCoef, CARL17_SF.degreeCent$centralization,
                         CARL17_SF.netDensity, CARL17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
CARL17_TFg2 <- data.frame(CARL17_TF)
CARL17_TFg2 <- CARL17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_TFg2$player1
player2vector <- CARL17_TFg2$player2
CARL17_TFg3 <- CARL17_TFg2
CARL17_TFg3$p1inp2vec <- is.element(CARL17_TFg3$player1, player2vector)
CARL17_TFg3$p2inp1vec <- is.element(CARL17_TFg3$player2, player1vector)

addPlayer1 <- CARL17_TFg3[ which(CARL17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL17_TFg3[ which(CARL17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_TFg2 <- rbind(CARL17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
CARL17_TFft <- ftable(CARL17_TFg2$player1, CARL17_TFg2$player2)
CARL17_TFft2 <- as.matrix(CARL17_TFft)
numRows <- nrow(CARL17_TFft2)
numCols <- ncol(CARL17_TFft2)
CARL17_TFft3 <- CARL17_TFft2[c(2:numRows) , c(2:numCols)]
CARL17_TFTable <- graph.adjacency(CARL17_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(CARL17_TFTable, vertex.label = V(CARL17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
CARL17_TF.clusterCoef <- transitivity(CARL17_TFTable, type="global") #cluster coefficient
CARL17_TF.degreeCent <- centralization.degree(CARL17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_TFftn <- as.network.matrix(CARL17_TFft)
CARL17_TF.netDensity <- network.density(CARL17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_TF.entropy <- entropy(CARL17_TFft) #entropy

CARL17_TF.netMx <- cbind(CARL17_TF.netMx, CARL17_TF.clusterCoef, CARL17_TF.degreeCent$centralization,
                         CARL17_TF.netDensity, CARL17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************

round = 17
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
CARL17_SAMg2 <- data.frame(CARL17_SAM)
CARL17_SAMg2 <- CARL17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_SAMg2$player1
player2vector <- CARL17_SAMg2$player2
CARL17_SAMg3 <- CARL17_SAMg2
CARL17_SAMg3$p1inp2vec <- is.element(CARL17_SAMg3$player1, player2vector)
CARL17_SAMg3$p2inp1vec <- is.element(CARL17_SAMg3$player2, player1vector)

addPlayer1 <- CARL17_SAMg3[ which(CARL17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL17_SAMg3[ which(CARL17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_SAMg2 <- rbind(CARL17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
CARL17_SAMft <- ftable(CARL17_SAMg2$player1, CARL17_SAMg2$player2)
CARL17_SAMft2 <- as.matrix(CARL17_SAMft)
numRows <- nrow(CARL17_SAMft2)
numCols <- ncol(CARL17_SAMft2)
CARL17_SAMft3 <- CARL17_SAMft2[c(2:numRows) , c(2:numCols)]
CARL17_SAMTable <- graph.adjacency(CARL17_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(CARL17_SAMTable, vertex.label = V(CARL17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
CARL17_SAM.clusterCoef <- transitivity(CARL17_SAMTable, type="global") #cluster coefficient
CARL17_SAM.degreeCent <- centralization.degree(CARL17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_SAMftn <- as.network.matrix(CARL17_SAMft)
CARL17_SAM.netDensity <- network.density(CARL17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_SAM.entropy <- entropy(CARL17_SAMft) #entropy

CARL17_SAM.netMx <- cbind(CARL17_SAM.netMx, CARL17_SAM.clusterCoef, CARL17_SAM.degreeCent$centralization,
                          CARL17_SAM.netDensity, CARL17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************
#NA

round = 17
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
CARL17_TAMg2 <- data.frame(CARL17_TAM)
CARL17_TAMg2 <- CARL17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_TAMg2$player1
player2vector <- CARL17_TAMg2$player2
CARL17_TAMg3 <- CARL17_TAMg2
CARL17_TAMg3$p1inp2vec <- is.element(CARL17_TAMg3$player1, player2vector)
CARL17_TAMg3$p2inp1vec <- is.element(CARL17_TAMg3$player2, player1vector)

addPlayer1 <- CARL17_TAMg3[ which(CARL17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL17_TAMg3[ which(CARL17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_TAMg2 <- rbind(CARL17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
CARL17_TAMft <- ftable(CARL17_TAMg2$player1, CARL17_TAMg2$player2)
CARL17_TAMft2 <- as.matrix(CARL17_TAMft)
numRows <- nrow(CARL17_TAMft2)
numCols <- ncol(CARL17_TAMft2)
CARL17_TAMft3 <- CARL17_TAMft2[c(2:numRows) , c(2:numCols)]
CARL17_TAMTable <- graph.adjacency(CARL17_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(CARL17_TAMTable, vertex.label = V(CARL17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
CARL17_TAM.clusterCoef <- transitivity(CARL17_TAMTable, type="global") #cluster coefficient
CARL17_TAM.degreeCent <- centralization.degree(CARL17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_TAMftn <- as.network.matrix(CARL17_TAMft)
CARL17_TAM.netDensity <- network.density(CARL17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_TAM.entropy <- entropy(CARL17_TAMft) #entropy

CARL17_TAM.netMx <- cbind(CARL17_TAM.netMx, CARL17_TAM.clusterCoef, CARL17_TAM.degreeCent$centralization,
                          CARL17_TAM.netDensity, CARL17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************

round = 17
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
CARL17_SDMg2 <- data.frame(CARL17_SDM)
CARL17_SDMg2 <- CARL17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_SDMg2$player1
player2vector <- CARL17_SDMg2$player2
CARL17_SDMg3 <- CARL17_SDMg2
CARL17_SDMg3$p1inp2vec <- is.element(CARL17_SDMg3$player1, player2vector)
CARL17_SDMg3$p2inp1vec <- is.element(CARL17_SDMg3$player2, player1vector)

addPlayer1 <- CARL17_SDMg3[ which(CARL17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL17_SDMg3[ which(CARL17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_SDMg2 <- rbind(CARL17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
CARL17_SDMft <- ftable(CARL17_SDMg2$player1, CARL17_SDMg2$player2)
CARL17_SDMft2 <- as.matrix(CARL17_SDMft)
numRows <- nrow(CARL17_SDMft2)
numCols <- ncol(CARL17_SDMft2)
CARL17_SDMft3 <- CARL17_SDMft2[c(2:numRows) , c(2:numCols)]
CARL17_SDMTable <- graph.adjacency(CARL17_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(CARL17_SDMTable, vertex.label = V(CARL17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
CARL17_SDM.clusterCoef <- transitivity(CARL17_SDMTable, type="global") #cluster coefficient
CARL17_SDM.degreeCent <- centralization.degree(CARL17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_SDMftn <- as.network.matrix(CARL17_SDMft)
CARL17_SDM.netDensity <- network.density(CARL17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_SDM.entropy <- entropy(CARL17_SDMft) #entropy

CARL17_SDM.netMx <- cbind(CARL17_SDM.netMx, CARL17_SDM.clusterCoef, CARL17_SDM.degreeCent$centralization,
                          CARL17_SDM.netDensity, CARL17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
CARL17_TDMg2 <- data.frame(CARL17_TDM)
CARL17_TDMg2 <- CARL17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_TDMg2$player1
player2vector <- CARL17_TDMg2$player2
CARL17_TDMg3 <- CARL17_TDMg2
CARL17_TDMg3$p1inp2vec <- is.element(CARL17_TDMg3$player1, player2vector)
CARL17_TDMg3$p2inp1vec <- is.element(CARL17_TDMg3$player2, player1vector)

addPlayer1 <- CARL17_TDMg3[ which(CARL17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL17_TDMg3[ which(CARL17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_TDMg2 <- rbind(CARL17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
CARL17_TDMft <- ftable(CARL17_TDMg2$player1, CARL17_TDMg2$player2)
CARL17_TDMft2 <- as.matrix(CARL17_TDMft)
numRows <- nrow(CARL17_TDMft2)
numCols <- ncol(CARL17_TDMft2)
CARL17_TDMft3 <- CARL17_TDMft2[c(2:numRows) , c(2:numCols)]
CARL17_TDMTable <- graph.adjacency(CARL17_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(CARL17_TDMTable, vertex.label = V(CARL17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
CARL17_TDM.clusterCoef <- transitivity(CARL17_TDMTable, type="global") #cluster coefficient
CARL17_TDM.degreeCent <- centralization.degree(CARL17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_TDMftn <- as.network.matrix(CARL17_TDMft)
CARL17_TDM.netDensity <- network.density(CARL17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_TDM.entropy <- entropy(CARL17_TDMft) #entropy

CARL17_TDM.netMx <- cbind(CARL17_TDM.netMx, CARL17_TDM.clusterCoef, CARL17_TDM.degreeCent$centralization,
                          CARL17_TDM.netDensity, CARL17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
CARL17_SDg2 <- data.frame(CARL17_SD)
CARL17_SDg2 <- CARL17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_SDg2$player1
player2vector <- CARL17_SDg2$player2
CARL17_SDg3 <- CARL17_SDg2
CARL17_SDg3$p1inp2vec <- is.element(CARL17_SDg3$player1, player2vector)
CARL17_SDg3$p2inp1vec <- is.element(CARL17_SDg3$player2, player1vector)

addPlayer1 <- CARL17_SDg3[ which(CARL17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL17_SDg3[ which(CARL17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_SDg2 <- rbind(CARL17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
CARL17_SDft <- ftable(CARL17_SDg2$player1, CARL17_SDg2$player2)
CARL17_SDft2 <- as.matrix(CARL17_SDft)
numRows <- nrow(CARL17_SDft2)
numCols <- ncol(CARL17_SDft2)
CARL17_SDft3 <- CARL17_SDft2[c(2:numRows) , c(2:numCols)]
CARL17_SDTable <- graph.adjacency(CARL17_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(CARL17_SDTable, vertex.label = V(CARL17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
CARL17_SD.clusterCoef <- transitivity(CARL17_SDTable, type="global") #cluster coefficient
CARL17_SD.degreeCent <- centralization.degree(CARL17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_SDftn <- as.network.matrix(CARL17_SDft)
CARL17_SD.netDensity <- network.density(CARL17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_SD.entropy <- entropy(CARL17_SDft) #entropy

CARL17_SD.netMx <- cbind(CARL17_SD.netMx, CARL17_SD.clusterCoef, CARL17_SD.degreeCent$centralization,
                         CARL17_SD.netDensity, CARL17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
CARL17_TDg2 <- data.frame(CARL17_TD)
CARL17_TDg2 <- CARL17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_TDg2$player1
player2vector <- CARL17_TDg2$player2
CARL17_TDg3 <- CARL17_TDg2
CARL17_TDg3$p1inp2vec <- is.element(CARL17_TDg3$player1, player2vector)
CARL17_TDg3$p2inp1vec <- is.element(CARL17_TDg3$player2, player1vector)

addPlayer1 <- CARL17_TDg3[ which(CARL17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL17_TDg3[ which(CARL17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_TDg2 <- rbind(CARL17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
CARL17_TDft <- ftable(CARL17_TDg2$player1, CARL17_TDg2$player2)
CARL17_TDft2 <- as.matrix(CARL17_TDft)
numRows <- nrow(CARL17_TDft2)
numCols <- ncol(CARL17_TDft2)
CARL17_TDft3 <- CARL17_TDft2[c(2:numRows) , c(2:numCols)]
CARL17_TDTable <- graph.adjacency(CARL17_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(CARL17_TDTable, vertex.label = V(CARL17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
CARL17_TD.clusterCoef <- transitivity(CARL17_TDTable, type="global") #cluster coefficient
CARL17_TD.degreeCent <- centralization.degree(CARL17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_TDftn <- as.network.matrix(CARL17_TDft)
CARL17_TD.netDensity <- network.density(CARL17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_TD.entropy <- entropy(CARL17_TDft) #entropy

CARL17_TD.netMx <- cbind(CARL17_TD.netMx, CARL17_TD.clusterCoef, CARL17_TD.degreeCent$centralization,
                         CARL17_TD.netDensity, CARL17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
CARL17_QTg2 <- data.frame(CARL17_QT)
CARL17_QTg2 <- CARL17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL17_QTg2$player1
player2vector <- CARL17_QTg2$player2
CARL17_QTg3 <- CARL17_QTg2
CARL17_QTg3$p1inp2vec <- is.element(CARL17_QTg3$player1, player2vector)
CARL17_QTg3$p2inp1vec <- is.element(CARL17_QTg3$player2, player1vector)

addPlayer1 <- CARL17_QTg3[ which(CARL17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL17_QTg3[ which(CARL17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL17_QTg2 <- rbind(CARL17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
CARL17_QTft <- ftable(CARL17_QTg2$player1, CARL17_QTg2$player2)
CARL17_QTft2 <- as.matrix(CARL17_QTft)
numRows <- nrow(CARL17_QTft2)
numCols <- ncol(CARL17_QTft2)
CARL17_QTft3 <- CARL17_QTft2[c(2:numRows) , c(2:numCols)]
CARL17_QTTable <- graph.adjacency(CARL17_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(CARL17_QTTable, vertex.label = V(CARL17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
CARL17_QT.clusterCoef <- transitivity(CARL17_QTTable, type="global") #cluster coefficient
CARL17_QT.degreeCent <- centralization.degree(CARL17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL17_QTftn <- as.network.matrix(CARL17_QTft)
CARL17_QT.netDensity <- network.density(CARL17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL17_QT.entropy <- entropy(CARL17_QTft) #entropy

CARL17_QT.netMx <- cbind(CARL17_QT.netMx, CARL17_QT.clusterCoef, CARL17_QT.degreeCent$centralization,
                         CARL17_QT.netDensity, CARL17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL17_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************

round = 17
teamName = "COLL"
KIoutcome = "Goal_F"
COLL17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
COLL17_Gg2 <- data.frame(COLL17_G)
COLL17_Gg2 <- COLL17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_Gg2$player1
player2vector <- COLL17_Gg2$player2
COLL17_Gg3 <- COLL17_Gg2
COLL17_Gg3$p1inp2vec <- is.element(COLL17_Gg3$player1, player2vector)
COLL17_Gg3$p2inp1vec <- is.element(COLL17_Gg3$player2, player1vector)

addPlayer1 <- COLL17_Gg3[ which(COLL17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_Gg3[ which(COLL17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_Gg2 <- rbind(COLL17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
COLL17_Gft <- ftable(COLL17_Gg2$player1, COLL17_Gg2$player2)
COLL17_Gft2 <- as.matrix(COLL17_Gft)
numRows <- nrow(COLL17_Gft2)
numCols <- ncol(COLL17_Gft2)
COLL17_Gft3 <- COLL17_Gft2[c(2:numRows) , c(2:numCols)]
COLL17_GTable <- graph.adjacency(COLL17_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(COLL17_GTable, vertex.label = V(COLL17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
COLL17_G.clusterCoef <- transitivity(COLL17_GTable, type="global") #cluster coefficient
COLL17_G.degreeCent <- centralization.degree(COLL17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_Gftn <- as.network.matrix(COLL17_Gft)
COLL17_G.netDensity <- network.density(COLL17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_G.entropy <- entropy(COLL17_Gft) #entropy

COLL17_G.netMx <- cbind(COLL17_G.netMx, COLL17_G.clusterCoef, COLL17_G.degreeCent$centralization,
                        COLL17_G.netDensity, COLL17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "COLL"
KIoutcome = "Behind_F"
COLL17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
COLL17_Bg2 <- data.frame(COLL17_B)
COLL17_Bg2 <- COLL17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_Bg2$player1
player2vector <- COLL17_Bg2$player2
COLL17_Bg3 <- COLL17_Bg2
COLL17_Bg3$p1inp2vec <- is.element(COLL17_Bg3$player1, player2vector)
COLL17_Bg3$p2inp1vec <- is.element(COLL17_Bg3$player2, player1vector)

addPlayer1 <- COLL17_Bg3[ which(COLL17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_Bg3[ which(COLL17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_Bg2 <- rbind(COLL17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
COLL17_Bft <- ftable(COLL17_Bg2$player1, COLL17_Bg2$player2)
COLL17_Bft2 <- as.matrix(COLL17_Bft)
numRows <- nrow(COLL17_Bft2)
numCols <- ncol(COLL17_Bft2)
COLL17_Bft3 <- COLL17_Bft2[c(2:numRows) , c(2:numCols)]
COLL17_BTable <- graph.adjacency(COLL17_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(COLL17_BTable, vertex.label = V(COLL17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
COLL17_B.clusterCoef <- transitivity(COLL17_BTable, type="global") #cluster coefficient
COLL17_B.degreeCent <- centralization.degree(COLL17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_Bftn <- as.network.matrix(COLL17_Bft)
COLL17_B.netDensity <- network.density(COLL17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_B.entropy <- entropy(COLL17_Bft) #entropy

COLL17_B.netMx <- cbind(COLL17_B.netMx, COLL17_B.clusterCoef, COLL17_B.degreeCent$centralization,
                        COLL17_B.netDensity, COLL17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
COLL17_SFg2 <- data.frame(COLL17_SF)
COLL17_SFg2 <- COLL17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_SFg2$player1
player2vector <- COLL17_SFg2$player2
COLL17_SFg3 <- COLL17_SFg2
COLL17_SFg3$p1inp2vec <- is.element(COLL17_SFg3$player1, player2vector)
COLL17_SFg3$p2inp1vec <- is.element(COLL17_SFg3$player2, player1vector)

addPlayer1 <- COLL17_SFg3[ which(COLL17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_SFg3[ which(COLL17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_SFg2 <- rbind(COLL17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
COLL17_SFft <- ftable(COLL17_SFg2$player1, COLL17_SFg2$player2)
COLL17_SFft2 <- as.matrix(COLL17_SFft)
numRows <- nrow(COLL17_SFft2)
numCols <- ncol(COLL17_SFft2)
COLL17_SFft3 <- COLL17_SFft2[c(2:numRows) , c(2:numCols)]
COLL17_SFTable <- graph.adjacency(COLL17_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(COLL17_SFTable, vertex.label = V(COLL17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
COLL17_SF.clusterCoef <- transitivity(COLL17_SFTable, type="global") #cluster coefficient
COLL17_SF.degreeCent <- centralization.degree(COLL17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_SFftn <- as.network.matrix(COLL17_SFft)
COLL17_SF.netDensity <- network.density(COLL17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_SF.entropy <- entropy(COLL17_SFft) #entropy

COLL17_SF.netMx <- cbind(COLL17_SF.netMx, COLL17_SF.clusterCoef, COLL17_SF.degreeCent$centralization,
                         COLL17_SF.netDensity, COLL17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
COLL17_TFg2 <- data.frame(COLL17_TF)
COLL17_TFg2 <- COLL17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_TFg2$player1
player2vector <- COLL17_TFg2$player2
COLL17_TFg3 <- COLL17_TFg2
COLL17_TFg3$p1inp2vec <- is.element(COLL17_TFg3$player1, player2vector)
COLL17_TFg3$p2inp1vec <- is.element(COLL17_TFg3$player2, player1vector)

addPlayer1 <- COLL17_TFg3[ which(COLL17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_TFg3[ which(COLL17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_TFg2 <- rbind(COLL17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
COLL17_TFft <- ftable(COLL17_TFg2$player1, COLL17_TFg2$player2)
COLL17_TFft2 <- as.matrix(COLL17_TFft)
numRows <- nrow(COLL17_TFft2)
numCols <- ncol(COLL17_TFft2)
COLL17_TFft3 <- COLL17_TFft2[c(2:numRows) , c(2:numCols)]
COLL17_TFTable <- graph.adjacency(COLL17_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(COLL17_TFTable, vertex.label = V(COLL17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
COLL17_TF.clusterCoef <- transitivity(COLL17_TFTable, type="global") #cluster coefficient
COLL17_TF.degreeCent <- centralization.degree(COLL17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_TFftn <- as.network.matrix(COLL17_TFft)
COLL17_TF.netDensity <- network.density(COLL17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_TF.entropy <- entropy(COLL17_TFft) #entropy

COLL17_TF.netMx <- cbind(COLL17_TF.netMx, COLL17_TF.clusterCoef, COLL17_TF.degreeCent$centralization,
                         COLL17_TF.netDensity, COLL17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************

round = 17
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
COLL17_SAMg2 <- data.frame(COLL17_SAM)
COLL17_SAMg2 <- COLL17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_SAMg2$player1
player2vector <- COLL17_SAMg2$player2
COLL17_SAMg3 <- COLL17_SAMg2
COLL17_SAMg3$p1inp2vec <- is.element(COLL17_SAMg3$player1, player2vector)
COLL17_SAMg3$p2inp1vec <- is.element(COLL17_SAMg3$player2, player1vector)

addPlayer1 <- COLL17_SAMg3[ which(COLL17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_SAMg3[ which(COLL17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_SAMg2 <- rbind(COLL17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
COLL17_SAMft <- ftable(COLL17_SAMg2$player1, COLL17_SAMg2$player2)
COLL17_SAMft2 <- as.matrix(COLL17_SAMft)
numRows <- nrow(COLL17_SAMft2)
numCols <- ncol(COLL17_SAMft2)
COLL17_SAMft3 <- COLL17_SAMft2[c(2:numRows) , c(2:numCols)]
COLL17_SAMTable <- graph.adjacency(COLL17_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(COLL17_SAMTable, vertex.label = V(COLL17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
COLL17_SAM.clusterCoef <- transitivity(COLL17_SAMTable, type="global") #cluster coefficient
COLL17_SAM.degreeCent <- centralization.degree(COLL17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_SAMftn <- as.network.matrix(COLL17_SAMft)
COLL17_SAM.netDensity <- network.density(COLL17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_SAM.entropy <- entropy(COLL17_SAMft) #entropy

COLL17_SAM.netMx <- cbind(COLL17_SAM.netMx, COLL17_SAM.clusterCoef, COLL17_SAM.degreeCent$centralization,
                          COLL17_SAM.netDensity, COLL17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
COLL17_TAMg2 <- data.frame(COLL17_TAM)
COLL17_TAMg2 <- COLL17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_TAMg2$player1
player2vector <- COLL17_TAMg2$player2
COLL17_TAMg3 <- COLL17_TAMg2
COLL17_TAMg3$p1inp2vec <- is.element(COLL17_TAMg3$player1, player2vector)
COLL17_TAMg3$p2inp1vec <- is.element(COLL17_TAMg3$player2, player1vector)

addPlayer1 <- COLL17_TAMg3[ which(COLL17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_TAMg3[ which(COLL17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_TAMg2 <- rbind(COLL17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
COLL17_TAMft <- ftable(COLL17_TAMg2$player1, COLL17_TAMg2$player2)
COLL17_TAMft2 <- as.matrix(COLL17_TAMft)
numRows <- nrow(COLL17_TAMft2)
numCols <- ncol(COLL17_TAMft2)
COLL17_TAMft3 <- COLL17_TAMft2[c(2:numRows) , c(2:numCols)]
COLL17_TAMTable <- graph.adjacency(COLL17_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(COLL17_TAMTable, vertex.label = V(COLL17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
COLL17_TAM.clusterCoef <- transitivity(COLL17_TAMTable, type="global") #cluster coefficient
COLL17_TAM.degreeCent <- centralization.degree(COLL17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_TAMftn <- as.network.matrix(COLL17_TAMft)
COLL17_TAM.netDensity <- network.density(COLL17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_TAM.entropy <- entropy(COLL17_TAMft) #entropy

COLL17_TAM.netMx <- cbind(COLL17_TAM.netMx, COLL17_TAM.clusterCoef, COLL17_TAM.degreeCent$centralization,
                          COLL17_TAM.netDensity, COLL17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************

round = 17
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
COLL17_SDMg2 <- data.frame(COLL17_SDM)
COLL17_SDMg2 <- COLL17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_SDMg2$player1
player2vector <- COLL17_SDMg2$player2
COLL17_SDMg3 <- COLL17_SDMg2
COLL17_SDMg3$p1inp2vec <- is.element(COLL17_SDMg3$player1, player2vector)
COLL17_SDMg3$p2inp1vec <- is.element(COLL17_SDMg3$player2, player1vector)

addPlayer1 <- COLL17_SDMg3[ which(COLL17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_SDMg3[ which(COLL17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_SDMg2 <- rbind(COLL17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
COLL17_SDMft <- ftable(COLL17_SDMg2$player1, COLL17_SDMg2$player2)
COLL17_SDMft2 <- as.matrix(COLL17_SDMft)
numRows <- nrow(COLL17_SDMft2)
numCols <- ncol(COLL17_SDMft2)
COLL17_SDMft3 <- COLL17_SDMft2[c(2:numRows) , c(2:numCols)]
COLL17_SDMTable <- graph.adjacency(COLL17_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(COLL17_SDMTable, vertex.label = V(COLL17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
COLL17_SDM.clusterCoef <- transitivity(COLL17_SDMTable, type="global") #cluster coefficient
COLL17_SDM.degreeCent <- centralization.degree(COLL17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_SDMftn <- as.network.matrix(COLL17_SDMft)
COLL17_SDM.netDensity <- network.density(COLL17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_SDM.entropy <- entropy(COLL17_SDMft) #entropy

COLL17_SDM.netMx <- cbind(COLL17_SDM.netMx, COLL17_SDM.clusterCoef, COLL17_SDM.degreeCent$centralization,
                          COLL17_SDM.netDensity, COLL17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************
#NA

round = 17
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
COLL17_TDMg2 <- data.frame(COLL17_TDM)
COLL17_TDMg2 <- COLL17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_TDMg2$player1
player2vector <- COLL17_TDMg2$player2
COLL17_TDMg3 <- COLL17_TDMg2
COLL17_TDMg3$p1inp2vec <- is.element(COLL17_TDMg3$player1, player2vector)
COLL17_TDMg3$p2inp1vec <- is.element(COLL17_TDMg3$player2, player1vector)

addPlayer1 <- COLL17_TDMg3[ which(COLL17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_TDMg3[ which(COLL17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_TDMg2 <- rbind(COLL17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
COLL17_TDMft <- ftable(COLL17_TDMg2$player1, COLL17_TDMg2$player2)
COLL17_TDMft2 <- as.matrix(COLL17_TDMft)
numRows <- nrow(COLL17_TDMft2)
numCols <- ncol(COLL17_TDMft2)
COLL17_TDMft3 <- COLL17_TDMft2[c(2:numRows) , c(2:numCols)]
COLL17_TDMTable <- graph.adjacency(COLL17_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(COLL17_TDMTable, vertex.label = V(COLL17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
COLL17_TDM.clusterCoef <- transitivity(COLL17_TDMTable, type="global") #cluster coefficient
COLL17_TDM.degreeCent <- centralization.degree(COLL17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_TDMftn <- as.network.matrix(COLL17_TDMft)
COLL17_TDM.netDensity <- network.density(COLL17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_TDM.entropy <- entropy(COLL17_TDMft) #entropy

COLL17_TDM.netMx <- cbind(COLL17_TDM.netMx, COLL17_TDM.clusterCoef, COLL17_TDM.degreeCent$centralization,
                          COLL17_TDM.netDensity, COLL17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
COLL17_SDg2 <- data.frame(COLL17_SD)
COLL17_SDg2 <- COLL17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_SDg2$player1
player2vector <- COLL17_SDg2$player2
COLL17_SDg3 <- COLL17_SDg2
COLL17_SDg3$p1inp2vec <- is.element(COLL17_SDg3$player1, player2vector)
COLL17_SDg3$p2inp1vec <- is.element(COLL17_SDg3$player2, player1vector)

addPlayer1 <- COLL17_SDg3[ which(COLL17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_SDg3[ which(COLL17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_SDg2 <- rbind(COLL17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
COLL17_SDft <- ftable(COLL17_SDg2$player1, COLL17_SDg2$player2)
COLL17_SDft2 <- as.matrix(COLL17_SDft)
numRows <- nrow(COLL17_SDft2)
numCols <- ncol(COLL17_SDft2)
COLL17_SDft3 <- COLL17_SDft2[c(2:numRows) , c(2:numCols)]
COLL17_SDTable <- graph.adjacency(COLL17_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(COLL17_SDTable, vertex.label = V(COLL17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
COLL17_SD.clusterCoef <- transitivity(COLL17_SDTable, type="global") #cluster coefficient
COLL17_SD.degreeCent <- centralization.degree(COLL17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_SDftn <- as.network.matrix(COLL17_SDft)
COLL17_SD.netDensity <- network.density(COLL17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_SD.entropy <- entropy(COLL17_SDft) #entropy

COLL17_SD.netMx <- cbind(COLL17_SD.netMx, COLL17_SD.clusterCoef, COLL17_SD.degreeCent$centralization,
                         COLL17_SD.netDensity, COLL17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
COLL17_TDg2 <- data.frame(COLL17_TD)
COLL17_TDg2 <- COLL17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_TDg2$player1
player2vector <- COLL17_TDg2$player2
COLL17_TDg3 <- COLL17_TDg2
COLL17_TDg3$p1inp2vec <- is.element(COLL17_TDg3$player1, player2vector)
COLL17_TDg3$p2inp1vec <- is.element(COLL17_TDg3$player2, player1vector)

addPlayer1 <- COLL17_TDg3[ which(COLL17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_TDg3[ which(COLL17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_TDg2 <- rbind(COLL17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
COLL17_TDft <- ftable(COLL17_TDg2$player1, COLL17_TDg2$player2)
COLL17_TDft2 <- as.matrix(COLL17_TDft)
numRows <- nrow(COLL17_TDft2)
numCols <- ncol(COLL17_TDft2)
COLL17_TDft3 <- COLL17_TDft2[c(2:numRows) , c(2:numCols)]
COLL17_TDTable <- graph.adjacency(COLL17_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(COLL17_TDTable, vertex.label = V(COLL17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
COLL17_TD.clusterCoef <- transitivity(COLL17_TDTable, type="global") #cluster coefficient
COLL17_TD.degreeCent <- centralization.degree(COLL17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_TDftn <- as.network.matrix(COLL17_TDft)
COLL17_TD.netDensity <- network.density(COLL17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_TD.entropy <- entropy(COLL17_TDft) #entropy

COLL17_TD.netMx <- cbind(COLL17_TD.netMx, COLL17_TD.clusterCoef, COLL17_TD.degreeCent$centralization,
                         COLL17_TD.netDensity, COLL17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
COLL17_QTg2 <- data.frame(COLL17_QT)
COLL17_QTg2 <- COLL17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL17_QTg2$player1
player2vector <- COLL17_QTg2$player2
COLL17_QTg3 <- COLL17_QTg2
COLL17_QTg3$p1inp2vec <- is.element(COLL17_QTg3$player1, player2vector)
COLL17_QTg3$p2inp1vec <- is.element(COLL17_QTg3$player2, player1vector)

addPlayer1 <- COLL17_QTg3[ which(COLL17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL17_QTg3[ which(COLL17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL17_QTg2 <- rbind(COLL17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
COLL17_QTft <- ftable(COLL17_QTg2$player1, COLL17_QTg2$player2)
COLL17_QTft2 <- as.matrix(COLL17_QTft)
numRows <- nrow(COLL17_QTft2)
numCols <- ncol(COLL17_QTft2)
COLL17_QTft3 <- COLL17_QTft2[c(2:numRows) , c(2:numCols)]
COLL17_QTTable <- graph.adjacency(COLL17_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(COLL17_QTTable, vertex.label = V(COLL17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
COLL17_QT.clusterCoef <- transitivity(COLL17_QTTable, type="global") #cluster coefficient
COLL17_QT.degreeCent <- centralization.degree(COLL17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL17_QTftn <- as.network.matrix(COLL17_QTft)
COLL17_QT.netDensity <- network.density(COLL17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL17_QT.entropy <- entropy(COLL17_QTft) #entropy

COLL17_QT.netMx <- cbind(COLL17_QT.netMx, COLL17_QT.clusterCoef, COLL17_QT.degreeCent$centralization,
                         COLL17_QT.netDensity, COLL17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL17_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************

round = 17
teamName = "ESS"
KIoutcome = "Goal_F"
ESS17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
ESS17_Gg2 <- data.frame(ESS17_G)
ESS17_Gg2 <- ESS17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_Gg2$player1
player2vector <- ESS17_Gg2$player2
ESS17_Gg3 <- ESS17_Gg2
ESS17_Gg3$p1inp2vec <- is.element(ESS17_Gg3$player1, player2vector)
ESS17_Gg3$p2inp1vec <- is.element(ESS17_Gg3$player2, player1vector)

addPlayer1 <- ESS17_Gg3[ which(ESS17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_Gg3[ which(ESS17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_Gg2 <- rbind(ESS17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
ESS17_Gft <- ftable(ESS17_Gg2$player1, ESS17_Gg2$player2)
ESS17_Gft2 <- as.matrix(ESS17_Gft)
numRows <- nrow(ESS17_Gft2)
numCols <- ncol(ESS17_Gft2)
ESS17_Gft3 <- ESS17_Gft2[c(2:numRows) , c(2:numCols)]
ESS17_GTable <- graph.adjacency(ESS17_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(ESS17_GTable, vertex.label = V(ESS17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
ESS17_G.clusterCoef <- transitivity(ESS17_GTable, type="global") #cluster coefficient
ESS17_G.degreeCent <- centralization.degree(ESS17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_Gftn <- as.network.matrix(ESS17_Gft)
ESS17_G.netDensity <- network.density(ESS17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_G.entropy <- entropy(ESS17_Gft) #entropy

ESS17_G.netMx <- cbind(ESS17_G.netMx, ESS17_G.clusterCoef, ESS17_G.degreeCent$centralization,
                       ESS17_G.netDensity, ESS17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "ESS"
KIoutcome = "Behind_F"
ESS17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
ESS17_Bg2 <- data.frame(ESS17_B)
ESS17_Bg2 <- ESS17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_Bg2$player1
player2vector <- ESS17_Bg2$player2
ESS17_Bg3 <- ESS17_Bg2
ESS17_Bg3$p1inp2vec <- is.element(ESS17_Bg3$player1, player2vector)
ESS17_Bg3$p2inp1vec <- is.element(ESS17_Bg3$player2, player1vector)

addPlayer1 <- ESS17_Bg3[ which(ESS17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_Bg3[ which(ESS17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_Bg2 <- rbind(ESS17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
ESS17_Bft <- ftable(ESS17_Bg2$player1, ESS17_Bg2$player2)
ESS17_Bft2 <- as.matrix(ESS17_Bft)
numRows <- nrow(ESS17_Bft2)
numCols <- ncol(ESS17_Bft2)
ESS17_Bft3 <- ESS17_Bft2[c(2:numRows) , c(2:numCols)]
ESS17_BTable <- graph.adjacency(ESS17_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(ESS17_BTable, vertex.label = V(ESS17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
ESS17_B.clusterCoef <- transitivity(ESS17_BTable, type="global") #cluster coefficient
ESS17_B.degreeCent <- centralization.degree(ESS17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_Bftn <- as.network.matrix(ESS17_Bft)
ESS17_B.netDensity <- network.density(ESS17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_B.entropy <- entropy(ESS17_Bft) #entropy

ESS17_B.netMx <- cbind(ESS17_B.netMx, ESS17_B.clusterCoef, ESS17_B.degreeCent$centralization,
                       ESS17_B.netDensity, ESS17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
ESS17_SFg2 <- data.frame(ESS17_SF)
ESS17_SFg2 <- ESS17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_SFg2$player1
player2vector <- ESS17_SFg2$player2
ESS17_SFg3 <- ESS17_SFg2
ESS17_SFg3$p1inp2vec <- is.element(ESS17_SFg3$player1, player2vector)
ESS17_SFg3$p2inp1vec <- is.element(ESS17_SFg3$player2, player1vector)

addPlayer1 <- ESS17_SFg3[ which(ESS17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_SFg3[ which(ESS17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_SFg2 <- rbind(ESS17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
ESS17_SFft <- ftable(ESS17_SFg2$player1, ESS17_SFg2$player2)
ESS17_SFft2 <- as.matrix(ESS17_SFft)
numRows <- nrow(ESS17_SFft2)
numCols <- ncol(ESS17_SFft2)
ESS17_SFft3 <- ESS17_SFft2[c(2:numRows) , c(2:numCols)]
ESS17_SFTable <- graph.adjacency(ESS17_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(ESS17_SFTable, vertex.label = V(ESS17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
ESS17_SF.clusterCoef <- transitivity(ESS17_SFTable, type="global") #cluster coefficient
ESS17_SF.degreeCent <- centralization.degree(ESS17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_SFftn <- as.network.matrix(ESS17_SFft)
ESS17_SF.netDensity <- network.density(ESS17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_SF.entropy <- entropy(ESS17_SFft) #entropy

ESS17_SF.netMx <- cbind(ESS17_SF.netMx, ESS17_SF.clusterCoef, ESS17_SF.degreeCent$centralization,
                        ESS17_SF.netDensity, ESS17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************

round = 17
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
ESS17_TFg2 <- data.frame(ESS17_TF)
ESS17_TFg2 <- ESS17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_TFg2$player1
player2vector <- ESS17_TFg2$player2
ESS17_TFg3 <- ESS17_TFg2
ESS17_TFg3$p1inp2vec <- is.element(ESS17_TFg3$player1, player2vector)
ESS17_TFg3$p2inp1vec <- is.element(ESS17_TFg3$player2, player1vector)

addPlayer1 <- ESS17_TFg3[ which(ESS17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_TFg3[ which(ESS17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_TFg2 <- rbind(ESS17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
ESS17_TFft <- ftable(ESS17_TFg2$player1, ESS17_TFg2$player2)
ESS17_TFft2 <- as.matrix(ESS17_TFft)
numRows <- nrow(ESS17_TFft2)
numCols <- ncol(ESS17_TFft2)
ESS17_TFft3 <- ESS17_TFft2[c(2:numRows) , c(2:numCols)]
ESS17_TFTable <- graph.adjacency(ESS17_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(ESS17_TFTable, vertex.label = V(ESS17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
ESS17_TF.clusterCoef <- transitivity(ESS17_TFTable, type="global") #cluster coefficient
ESS17_TF.degreeCent <- centralization.degree(ESS17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_TFftn <- as.network.matrix(ESS17_TFft)
ESS17_TF.netDensity <- network.density(ESS17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_TF.entropy <- entropy(ESS17_TFft) #entropy

ESS17_TF.netMx <- cbind(ESS17_TF.netMx, ESS17_TF.clusterCoef, ESS17_TF.degreeCent$centralization,
                        ESS17_TF.netDensity, ESS17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************

round = 17
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
ESS17_SAMg2 <- data.frame(ESS17_SAM)
ESS17_SAMg2 <- ESS17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_SAMg2$player1
player2vector <- ESS17_SAMg2$player2
ESS17_SAMg3 <- ESS17_SAMg2
ESS17_SAMg3$p1inp2vec <- is.element(ESS17_SAMg3$player1, player2vector)
ESS17_SAMg3$p2inp1vec <- is.element(ESS17_SAMg3$player2, player1vector)

addPlayer1 <- ESS17_SAMg3[ which(ESS17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_SAMg3[ which(ESS17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_SAMg2 <- rbind(ESS17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
ESS17_SAMft <- ftable(ESS17_SAMg2$player1, ESS17_SAMg2$player2)
ESS17_SAMft2 <- as.matrix(ESS17_SAMft)
numRows <- nrow(ESS17_SAMft2)
numCols <- ncol(ESS17_SAMft2)
ESS17_SAMft3 <- ESS17_SAMft2[c(2:numRows) , c(2:numCols)]
ESS17_SAMTable <- graph.adjacency(ESS17_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(ESS17_SAMTable, vertex.label = V(ESS17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
ESS17_SAM.clusterCoef <- transitivity(ESS17_SAMTable, type="global") #cluster coefficient
ESS17_SAM.degreeCent <- centralization.degree(ESS17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_SAMftn <- as.network.matrix(ESS17_SAMft)
ESS17_SAM.netDensity <- network.density(ESS17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_SAM.entropy <- entropy(ESS17_SAMft) #entropy

ESS17_SAM.netMx <- cbind(ESS17_SAM.netMx, ESS17_SAM.clusterCoef, ESS17_SAM.degreeCent$centralization,
                         ESS17_SAM.netDensity, ESS17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
ESS17_TAMg2 <- data.frame(ESS17_TAM)
ESS17_TAMg2 <- ESS17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_TAMg2$player1
player2vector <- ESS17_TAMg2$player2
ESS17_TAMg3 <- ESS17_TAMg2
ESS17_TAMg3$p1inp2vec <- is.element(ESS17_TAMg3$player1, player2vector)
ESS17_TAMg3$p2inp1vec <- is.element(ESS17_TAMg3$player2, player1vector)

addPlayer1 <- ESS17_TAMg3[ which(ESS17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS17_TAMg3[ which(ESS17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_TAMg2 <- rbind(ESS17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
ESS17_TAMft <- ftable(ESS17_TAMg2$player1, ESS17_TAMg2$player2)
ESS17_TAMft2 <- as.matrix(ESS17_TAMft)
numRows <- nrow(ESS17_TAMft2)
numCols <- ncol(ESS17_TAMft2)
ESS17_TAMft3 <- ESS17_TAMft2[c(2:numRows) , c(2:numCols)]
ESS17_TAMTable <- graph.adjacency(ESS17_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(ESS17_TAMTable, vertex.label = V(ESS17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
ESS17_TAM.clusterCoef <- transitivity(ESS17_TAMTable, type="global") #cluster coefficient
ESS17_TAM.degreeCent <- centralization.degree(ESS17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_TAMftn <- as.network.matrix(ESS17_TAMft)
ESS17_TAM.netDensity <- network.density(ESS17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_TAM.entropy <- entropy(ESS17_TAMft) #entropy

ESS17_TAM.netMx <- cbind(ESS17_TAM.netMx, ESS17_TAM.clusterCoef, ESS17_TAM.degreeCent$centralization,
                         ESS17_TAM.netDensity, ESS17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************
#NA

round = 17
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
ESS17_SDMg2 <- data.frame(ESS17_SDM)
ESS17_SDMg2 <- ESS17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_SDMg2$player1
player2vector <- ESS17_SDMg2$player2
ESS17_SDMg3 <- ESS17_SDMg2
ESS17_SDMg3$p1inp2vec <- is.element(ESS17_SDMg3$player1, player2vector)
ESS17_SDMg3$p2inp1vec <- is.element(ESS17_SDMg3$player2, player1vector)

addPlayer1 <- ESS17_SDMg3[ which(ESS17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_SDMg3[ which(ESS17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_SDMg2 <- rbind(ESS17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
ESS17_SDMft <- ftable(ESS17_SDMg2$player1, ESS17_SDMg2$player2)
ESS17_SDMft2 <- as.matrix(ESS17_SDMft)
numRows <- nrow(ESS17_SDMft2)
numCols <- ncol(ESS17_SDMft2)
ESS17_SDMft3 <- ESS17_SDMft2[c(2:numRows) , c(2:numCols)]
ESS17_SDMTable <- graph.adjacency(ESS17_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(ESS17_SDMTable, vertex.label = V(ESS17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
ESS17_SDM.clusterCoef <- transitivity(ESS17_SDMTable, type="global") #cluster coefficient
ESS17_SDM.degreeCent <- centralization.degree(ESS17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_SDMftn <- as.network.matrix(ESS17_SDMft)
ESS17_SDM.netDensity <- network.density(ESS17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_SDM.entropy <- entropy(ESS17_SDMft) #entropy

ESS17_SDM.netMx <- cbind(ESS17_SDM.netMx, ESS17_SDM.clusterCoef, ESS17_SDM.degreeCent$centralization,
                         ESS17_SDM.netDensity, ESS17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
ESS17_TDMg2 <- data.frame(ESS17_TDM)
ESS17_TDMg2 <- ESS17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_TDMg2$player1
player2vector <- ESS17_TDMg2$player2
ESS17_TDMg3 <- ESS17_TDMg2
ESS17_TDMg3$p1inp2vec <- is.element(ESS17_TDMg3$player1, player2vector)
ESS17_TDMg3$p2inp1vec <- is.element(ESS17_TDMg3$player2, player1vector)

addPlayer1 <- ESS17_TDMg3[ which(ESS17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_TDMg3[ which(ESS17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_TDMg2 <- rbind(ESS17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
ESS17_TDMft <- ftable(ESS17_TDMg2$player1, ESS17_TDMg2$player2)
ESS17_TDMft2 <- as.matrix(ESS17_TDMft)
numRows <- nrow(ESS17_TDMft2)
numCols <- ncol(ESS17_TDMft2)
ESS17_TDMft3 <- ESS17_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS17_TDMTable <- graph.adjacency(ESS17_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(ESS17_TDMTable, vertex.label = V(ESS17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
ESS17_TDM.clusterCoef <- transitivity(ESS17_TDMTable, type="global") #cluster coefficient
ESS17_TDM.degreeCent <- centralization.degree(ESS17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_TDMftn <- as.network.matrix(ESS17_TDMft)
ESS17_TDM.netDensity <- network.density(ESS17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_TDM.entropy <- entropy(ESS17_TDMft) #entropy

ESS17_TDM.netMx <- cbind(ESS17_TDM.netMx, ESS17_TDM.clusterCoef, ESS17_TDM.degreeCent$centralization,
                         ESS17_TDM.netDensity, ESS17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
ESS17_SDg2 <- data.frame(ESS17_SD)
ESS17_SDg2 <- ESS17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_SDg2$player1
player2vector <- ESS17_SDg2$player2
ESS17_SDg3 <- ESS17_SDg2
ESS17_SDg3$p1inp2vec <- is.element(ESS17_SDg3$player1, player2vector)
ESS17_SDg3$p2inp1vec <- is.element(ESS17_SDg3$player2, player1vector)

addPlayer1 <- ESS17_SDg3[ which(ESS17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_SDg3[ which(ESS17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_SDg2 <- rbind(ESS17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
ESS17_SDft <- ftable(ESS17_SDg2$player1, ESS17_SDg2$player2)
ESS17_SDft2 <- as.matrix(ESS17_SDft)
numRows <- nrow(ESS17_SDft2)
numCols <- ncol(ESS17_SDft2)
ESS17_SDft3 <- ESS17_SDft2[c(2:numRows) , c(2:numCols)]
ESS17_SDTable <- graph.adjacency(ESS17_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(ESS17_SDTable, vertex.label = V(ESS17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
ESS17_SD.clusterCoef <- transitivity(ESS17_SDTable, type="global") #cluster coefficient
ESS17_SD.degreeCent <- centralization.degree(ESS17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_SDftn <- as.network.matrix(ESS17_SDft)
ESS17_SD.netDensity <- network.density(ESS17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_SD.entropy <- entropy(ESS17_SDft) #entropy

ESS17_SD.netMx <- cbind(ESS17_SD.netMx, ESS17_SD.clusterCoef, ESS17_SD.degreeCent$centralization,
                        ESS17_SD.netDensity, ESS17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
ESS17_TDg2 <- data.frame(ESS17_TD)
ESS17_TDg2 <- ESS17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_TDg2$player1
player2vector <- ESS17_TDg2$player2
ESS17_TDg3 <- ESS17_TDg2
ESS17_TDg3$p1inp2vec <- is.element(ESS17_TDg3$player1, player2vector)
ESS17_TDg3$p2inp1vec <- is.element(ESS17_TDg3$player2, player1vector)

addPlayer1 <- ESS17_TDg3[ which(ESS17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_TDg3[ which(ESS17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_TDg2 <- rbind(ESS17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
ESS17_TDft <- ftable(ESS17_TDg2$player1, ESS17_TDg2$player2)
ESS17_TDft2 <- as.matrix(ESS17_TDft)
numRows <- nrow(ESS17_TDft2)
numCols <- ncol(ESS17_TDft2)
ESS17_TDft3 <- ESS17_TDft2[c(2:numRows) , c(2:numCols)]
ESS17_TDTable <- graph.adjacency(ESS17_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(ESS17_TDTable, vertex.label = V(ESS17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
ESS17_TD.clusterCoef <- transitivity(ESS17_TDTable, type="global") #cluster coefficient
ESS17_TD.degreeCent <- centralization.degree(ESS17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_TDftn <- as.network.matrix(ESS17_TDft)
ESS17_TD.netDensity <- network.density(ESS17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_TD.entropy <- entropy(ESS17_TDft) #entropy

ESS17_TD.netMx <- cbind(ESS17_TD.netMx, ESS17_TD.clusterCoef, ESS17_TD.degreeCent$centralization,
                        ESS17_TD.netDensity, ESS17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
ESS17_QTg2 <- data.frame(ESS17_QT)
ESS17_QTg2 <- ESS17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS17_QTg2$player1
player2vector <- ESS17_QTg2$player2
ESS17_QTg3 <- ESS17_QTg2
ESS17_QTg3$p1inp2vec <- is.element(ESS17_QTg3$player1, player2vector)
ESS17_QTg3$p2inp1vec <- is.element(ESS17_QTg3$player2, player1vector)

addPlayer1 <- ESS17_QTg3[ which(ESS17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS17_QTg3[ which(ESS17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS17_QTg2 <- rbind(ESS17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
ESS17_QTft <- ftable(ESS17_QTg2$player1, ESS17_QTg2$player2)
ESS17_QTft2 <- as.matrix(ESS17_QTft)
numRows <- nrow(ESS17_QTft2)
numCols <- ncol(ESS17_QTft2)
ESS17_QTft3 <- ESS17_QTft2[c(2:numRows) , c(2:numCols)]
ESS17_QTTable <- graph.adjacency(ESS17_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(ESS17_QTTable, vertex.label = V(ESS17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
ESS17_QT.clusterCoef <- transitivity(ESS17_QTTable, type="global") #cluster coefficient
ESS17_QT.degreeCent <- centralization.degree(ESS17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS17_QTftn <- as.network.matrix(ESS17_QTft)
ESS17_QT.netDensity <- network.density(ESS17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS17_QT.entropy <- entropy(ESS17_QTft) #entropy

ESS17_QT.netMx <- cbind(ESS17_QT.netMx, ESS17_QT.clusterCoef, ESS17_QT.degreeCent$centralization,
                        ESS17_QT.netDensity, ESS17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS17_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************

round = 17
teamName = "FRE"
KIoutcome = "Goal_F"
FRE17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
FRE17_Gg2 <- data.frame(FRE17_G)
FRE17_Gg2 <- FRE17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_Gg2$player1
player2vector <- FRE17_Gg2$player2
FRE17_Gg3 <- FRE17_Gg2
FRE17_Gg3$p1inp2vec <- is.element(FRE17_Gg3$player1, player2vector)
FRE17_Gg3$p2inp1vec <- is.element(FRE17_Gg3$player2, player1vector)

addPlayer1 <- FRE17_Gg3[ which(FRE17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_Gg3[ which(FRE17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_Gg2 <- rbind(FRE17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
FRE17_Gft <- ftable(FRE17_Gg2$player1, FRE17_Gg2$player2)
FRE17_Gft2 <- as.matrix(FRE17_Gft)
numRows <- nrow(FRE17_Gft2)
numCols <- ncol(FRE17_Gft2)
FRE17_Gft3 <- FRE17_Gft2[c(2:numRows) , c(2:numCols)]
FRE17_GTable <- graph.adjacency(FRE17_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(FRE17_GTable, vertex.label = V(FRE17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
FRE17_G.clusterCoef <- transitivity(FRE17_GTable, type="global") #cluster coefficient
FRE17_G.degreeCent <- centralization.degree(FRE17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_Gftn <- as.network.matrix(FRE17_Gft)
FRE17_G.netDensity <- network.density(FRE17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_G.entropy <- entropy(FRE17_Gft) #entropy

FRE17_G.netMx <- cbind(FRE17_G.netMx, FRE17_G.clusterCoef, FRE17_G.degreeCent$centralization,
                       FRE17_G.netDensity, FRE17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************

round = 17
teamName = "FRE"
KIoutcome = "Behind_F"
FRE17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
FRE17_Bg2 <- data.frame(FRE17_B)
FRE17_Bg2 <- FRE17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_Bg2$player1
player2vector <- FRE17_Bg2$player2
FRE17_Bg3 <- FRE17_Bg2
FRE17_Bg3$p1inp2vec <- is.element(FRE17_Bg3$player1, player2vector)
FRE17_Bg3$p2inp1vec <- is.element(FRE17_Bg3$player2, player1vector)

addPlayer1 <- FRE17_Bg3[ which(FRE17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_Bg3[ which(FRE17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_Bg2 <- rbind(FRE17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
FRE17_Bft <- ftable(FRE17_Bg2$player1, FRE17_Bg2$player2)
FRE17_Bft2 <- as.matrix(FRE17_Bft)
numRows <- nrow(FRE17_Bft2)
numCols <- ncol(FRE17_Bft2)
FRE17_Bft3 <- FRE17_Bft2[c(2:numRows) , c(2:numCols)]
FRE17_BTable <- graph.adjacency(FRE17_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(FRE17_BTable, vertex.label = V(FRE17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
FRE17_B.clusterCoef <- transitivity(FRE17_BTable, type="global") #cluster coefficient
FRE17_B.degreeCent <- centralization.degree(FRE17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_Bftn <- as.network.matrix(FRE17_Bft)
FRE17_B.netDensity <- network.density(FRE17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_B.entropy <- entropy(FRE17_Bft) #entropy

FRE17_B.netMx <- cbind(FRE17_B.netMx, FRE17_B.clusterCoef, FRE17_B.degreeCent$centralization,
                       FRE17_B.netDensity, FRE17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************

round = 17
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
FRE17_SFg2 <- data.frame(FRE17_SF)
FRE17_SFg2 <- FRE17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_SFg2$player1
player2vector <- FRE17_SFg2$player2
FRE17_SFg3 <- FRE17_SFg2
FRE17_SFg3$p1inp2vec <- is.element(FRE17_SFg3$player1, player2vector)
FRE17_SFg3$p2inp1vec <- is.element(FRE17_SFg3$player2, player1vector)

addPlayer1 <- FRE17_SFg3[ which(FRE17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_SFg3[ which(FRE17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_SFg2 <- rbind(FRE17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
FRE17_SFft <- ftable(FRE17_SFg2$player1, FRE17_SFg2$player2)
FRE17_SFft2 <- as.matrix(FRE17_SFft)
numRows <- nrow(FRE17_SFft2)
numCols <- ncol(FRE17_SFft2)
FRE17_SFft3 <- FRE17_SFft2[c(2:numRows) , c(2:numCols)]
FRE17_SFTable <- graph.adjacency(FRE17_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(FRE17_SFTable, vertex.label = V(FRE17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
FRE17_SF.clusterCoef <- transitivity(FRE17_SFTable, type="global") #cluster coefficient
FRE17_SF.degreeCent <- centralization.degree(FRE17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_SFftn <- as.network.matrix(FRE17_SFft)
FRE17_SF.netDensity <- network.density(FRE17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_SF.entropy <- entropy(FRE17_SFft) #entropy

FRE17_SF.netMx <- cbind(FRE17_SF.netMx, FRE17_SF.clusterCoef, FRE17_SF.degreeCent$centralization,
                        FRE17_SF.netDensity, FRE17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************

round = 17
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
FRE17_TFg2 <- data.frame(FRE17_TF)
FRE17_TFg2 <- FRE17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_TFg2$player1
player2vector <- FRE17_TFg2$player2
FRE17_TFg3 <- FRE17_TFg2
FRE17_TFg3$p1inp2vec <- is.element(FRE17_TFg3$player1, player2vector)
FRE17_TFg3$p2inp1vec <- is.element(FRE17_TFg3$player2, player1vector)

addPlayer1 <- FRE17_TFg3[ which(FRE17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_TFg3[ which(FRE17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_TFg2 <- rbind(FRE17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
FRE17_TFft <- ftable(FRE17_TFg2$player1, FRE17_TFg2$player2)
FRE17_TFft2 <- as.matrix(FRE17_TFft)
numRows <- nrow(FRE17_TFft2)
numCols <- ncol(FRE17_TFft2)
FRE17_TFft3 <- FRE17_TFft2[c(2:numRows) , c(2:numCols)]
FRE17_TFTable <- graph.adjacency(FRE17_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(FRE17_TFTable, vertex.label = V(FRE17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
FRE17_TF.clusterCoef <- transitivity(FRE17_TFTable, type="global") #cluster coefficient
FRE17_TF.degreeCent <- centralization.degree(FRE17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_TFftn <- as.network.matrix(FRE17_TFft)
FRE17_TF.netDensity <- network.density(FRE17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_TF.entropy <- entropy(FRE17_TFft) #entropy

FRE17_TF.netMx <- cbind(FRE17_TF.netMx, FRE17_TF.clusterCoef, FRE17_TF.degreeCent$centralization,
                        FRE17_TF.netDensity, FRE17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************

round = 17
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
FRE17_SAMg2 <- data.frame(FRE17_SAM)
FRE17_SAMg2 <- FRE17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_SAMg2$player1
player2vector <- FRE17_SAMg2$player2
FRE17_SAMg3 <- FRE17_SAMg2
FRE17_SAMg3$p1inp2vec <- is.element(FRE17_SAMg3$player1, player2vector)
FRE17_SAMg3$p2inp1vec <- is.element(FRE17_SAMg3$player2, player1vector)

addPlayer1 <- FRE17_SAMg3[ which(FRE17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_SAMg3[ which(FRE17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_SAMg2 <- rbind(FRE17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
FRE17_SAMft <- ftable(FRE17_SAMg2$player1, FRE17_SAMg2$player2)
FRE17_SAMft2 <- as.matrix(FRE17_SAMft)
numRows <- nrow(FRE17_SAMft2)
numCols <- ncol(FRE17_SAMft2)
FRE17_SAMft3 <- FRE17_SAMft2[c(2:numRows) , c(2:numCols)]
FRE17_SAMTable <- graph.adjacency(FRE17_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(FRE17_SAMTable, vertex.label = V(FRE17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
FRE17_SAM.clusterCoef <- transitivity(FRE17_SAMTable, type="global") #cluster coefficient
FRE17_SAM.degreeCent <- centralization.degree(FRE17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_SAMftn <- as.network.matrix(FRE17_SAMft)
FRE17_SAM.netDensity <- network.density(FRE17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_SAM.entropy <- entropy(FRE17_SAMft) #entropy

FRE17_SAM.netMx <- cbind(FRE17_SAM.netMx, FRE17_SAM.clusterCoef, FRE17_SAM.degreeCent$centralization,
                         FRE17_SAM.netDensity, FRE17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
FRE17_TAMg2 <- data.frame(FRE17_TAM)
FRE17_TAMg2 <- FRE17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_TAMg2$player1
player2vector <- FRE17_TAMg2$player2
FRE17_TAMg3 <- FRE17_TAMg2
FRE17_TAMg3$p1inp2vec <- is.element(FRE17_TAMg3$player1, player2vector)
FRE17_TAMg3$p2inp1vec <- is.element(FRE17_TAMg3$player2, player1vector)

addPlayer1 <- FRE17_TAMg3[ which(FRE17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_TAMg3[ which(FRE17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_TAMg2 <- rbind(FRE17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
FRE17_TAMft <- ftable(FRE17_TAMg2$player1, FRE17_TAMg2$player2)
FRE17_TAMft2 <- as.matrix(FRE17_TAMft)
numRows <- nrow(FRE17_TAMft2)
numCols <- ncol(FRE17_TAMft2)
FRE17_TAMft3 <- FRE17_TAMft2[c(2:numRows) , c(2:numCols)]
FRE17_TAMTable <- graph.adjacency(FRE17_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(FRE17_TAMTable, vertex.label = V(FRE17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
FRE17_TAM.clusterCoef <- transitivity(FRE17_TAMTable, type="global") #cluster coefficient
FRE17_TAM.degreeCent <- centralization.degree(FRE17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_TAMftn <- as.network.matrix(FRE17_TAMft)
FRE17_TAM.netDensity <- network.density(FRE17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_TAM.entropy <- entropy(FRE17_TAMft) #entropy

FRE17_TAM.netMx <- cbind(FRE17_TAM.netMx, FRE17_TAM.clusterCoef, FRE17_TAM.degreeCent$centralization,
                         FRE17_TAM.netDensity, FRE17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************

round = 17
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
FRE17_SDMg2 <- data.frame(FRE17_SDM)
FRE17_SDMg2 <- FRE17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_SDMg2$player1
player2vector <- FRE17_SDMg2$player2
FRE17_SDMg3 <- FRE17_SDMg2
FRE17_SDMg3$p1inp2vec <- is.element(FRE17_SDMg3$player1, player2vector)
FRE17_SDMg3$p2inp1vec <- is.element(FRE17_SDMg3$player2, player1vector)

addPlayer1 <- FRE17_SDMg3[ which(FRE17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_SDMg3[ which(FRE17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_SDMg2 <- rbind(FRE17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
FRE17_SDMft <- ftable(FRE17_SDMg2$player1, FRE17_SDMg2$player2)
FRE17_SDMft2 <- as.matrix(FRE17_SDMft)
numRows <- nrow(FRE17_SDMft2)
numCols <- ncol(FRE17_SDMft2)
FRE17_SDMft3 <- FRE17_SDMft2[c(2:numRows) , c(2:numCols)]
FRE17_SDMTable <- graph.adjacency(FRE17_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(FRE17_SDMTable, vertex.label = V(FRE17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
FRE17_SDM.clusterCoef <- transitivity(FRE17_SDMTable, type="global") #cluster coefficient
FRE17_SDM.degreeCent <- centralization.degree(FRE17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_SDMftn <- as.network.matrix(FRE17_SDMft)
FRE17_SDM.netDensity <- network.density(FRE17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_SDM.entropy <- entropy(FRE17_SDMft) #entropy

FRE17_SDM.netMx <- cbind(FRE17_SDM.netMx, FRE17_SDM.clusterCoef, FRE17_SDM.degreeCent$centralization,
                         FRE17_SDM.netDensity, FRE17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
FRE17_TDMg2 <- data.frame(FRE17_TDM)
FRE17_TDMg2 <- FRE17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_TDMg2$player1
player2vector <- FRE17_TDMg2$player2
FRE17_TDMg3 <- FRE17_TDMg2
FRE17_TDMg3$p1inp2vec <- is.element(FRE17_TDMg3$player1, player2vector)
FRE17_TDMg3$p2inp1vec <- is.element(FRE17_TDMg3$player2, player1vector)

addPlayer1 <- FRE17_TDMg3[ which(FRE17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_TDMg3[ which(FRE17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_TDMg2 <- rbind(FRE17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
FRE17_TDMft <- ftable(FRE17_TDMg2$player1, FRE17_TDMg2$player2)
FRE17_TDMft2 <- as.matrix(FRE17_TDMft)
numRows <- nrow(FRE17_TDMft2)
numCols <- ncol(FRE17_TDMft2)
FRE17_TDMft3 <- FRE17_TDMft2[c(2:numRows) , c(2:numCols)]
FRE17_TDMTable <- graph.adjacency(FRE17_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(FRE17_TDMTable, vertex.label = V(FRE17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
FRE17_TDM.clusterCoef <- transitivity(FRE17_TDMTable, type="global") #cluster coefficient
FRE17_TDM.degreeCent <- centralization.degree(FRE17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_TDMftn <- as.network.matrix(FRE17_TDMft)
FRE17_TDM.netDensity <- network.density(FRE17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_TDM.entropy <- entropy(FRE17_TDMft) #entropy

FRE17_TDM.netMx <- cbind(FRE17_TDM.netMx, FRE17_TDM.clusterCoef, FRE17_TDM.degreeCent$centralization,
                         FRE17_TDM.netDensity, FRE17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
FRE17_SDg2 <- data.frame(FRE17_SD)
FRE17_SDg2 <- FRE17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_SDg2$player1
player2vector <- FRE17_SDg2$player2
FRE17_SDg3 <- FRE17_SDg2
FRE17_SDg3$p1inp2vec <- is.element(FRE17_SDg3$player1, player2vector)
FRE17_SDg3$p2inp1vec <- is.element(FRE17_SDg3$player2, player1vector)

addPlayer1 <- FRE17_SDg3[ which(FRE17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_SDg3[ which(FRE17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_SDg2 <- rbind(FRE17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
FRE17_SDft <- ftable(FRE17_SDg2$player1, FRE17_SDg2$player2)
FRE17_SDft2 <- as.matrix(FRE17_SDft)
numRows <- nrow(FRE17_SDft2)
numCols <- ncol(FRE17_SDft2)
FRE17_SDft3 <- FRE17_SDft2[c(2:numRows) , c(2:numCols)]
FRE17_SDTable <- graph.adjacency(FRE17_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(FRE17_SDTable, vertex.label = V(FRE17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
FRE17_SD.clusterCoef <- transitivity(FRE17_SDTable, type="global") #cluster coefficient
FRE17_SD.degreeCent <- centralization.degree(FRE17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_SDftn <- as.network.matrix(FRE17_SDft)
FRE17_SD.netDensity <- network.density(FRE17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_SD.entropy <- entropy(FRE17_SDft) #entropy

FRE17_SD.netMx <- cbind(FRE17_SD.netMx, FRE17_SD.clusterCoef, FRE17_SD.degreeCent$centralization,
                        FRE17_SD.netDensity, FRE17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
FRE17_TDg2 <- data.frame(FRE17_TD)
FRE17_TDg2 <- FRE17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_TDg2$player1
player2vector <- FRE17_TDg2$player2
FRE17_TDg3 <- FRE17_TDg2
FRE17_TDg3$p1inp2vec <- is.element(FRE17_TDg3$player1, player2vector)
FRE17_TDg3$p2inp1vec <- is.element(FRE17_TDg3$player2, player1vector)

addPlayer1 <- FRE17_TDg3[ which(FRE17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_TDg3[ which(FRE17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_TDg2 <- rbind(FRE17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
FRE17_TDft <- ftable(FRE17_TDg2$player1, FRE17_TDg2$player2)
FRE17_TDft2 <- as.matrix(FRE17_TDft)
numRows <- nrow(FRE17_TDft2)
numCols <- ncol(FRE17_TDft2)
FRE17_TDft3 <- FRE17_TDft2[c(2:numRows) , c(2:numCols)]
FRE17_TDTable <- graph.adjacency(FRE17_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(FRE17_TDTable, vertex.label = V(FRE17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
FRE17_TD.clusterCoef <- transitivity(FRE17_TDTable, type="global") #cluster coefficient
FRE17_TD.degreeCent <- centralization.degree(FRE17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_TDftn <- as.network.matrix(FRE17_TDft)
FRE17_TD.netDensity <- network.density(FRE17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_TD.entropy <- entropy(FRE17_TDft) #entropy

FRE17_TD.netMx <- cbind(FRE17_TD.netMx, FRE17_TD.clusterCoef, FRE17_TD.degreeCent$centralization,
                        FRE17_TD.netDensity, FRE17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
FRE17_QTg2 <- data.frame(FRE17_QT)
FRE17_QTg2 <- FRE17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE17_QTg2$player1
player2vector <- FRE17_QTg2$player2
FRE17_QTg3 <- FRE17_QTg2
FRE17_QTg3$p1inp2vec <- is.element(FRE17_QTg3$player1, player2vector)
FRE17_QTg3$p2inp1vec <- is.element(FRE17_QTg3$player2, player1vector)

addPlayer1 <- FRE17_QTg3[ which(FRE17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE17_QTg3[ which(FRE17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE17_QTg2 <- rbind(FRE17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
FRE17_QTft <- ftable(FRE17_QTg2$player1, FRE17_QTg2$player2)
FRE17_QTft2 <- as.matrix(FRE17_QTft)
numRows <- nrow(FRE17_QTft2)
numCols <- ncol(FRE17_QTft2)
FRE17_QTft3 <- FRE17_QTft2[c(2:numRows) , c(2:numCols)]
FRE17_QTTable <- graph.adjacency(FRE17_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(FRE17_QTTable, vertex.label = V(FRE17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
FRE17_QT.clusterCoef <- transitivity(FRE17_QTTable, type="global") #cluster coefficient
FRE17_QT.degreeCent <- centralization.degree(FRE17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE17_QTftn <- as.network.matrix(FRE17_QTft)
FRE17_QT.netDensity <- network.density(FRE17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE17_QT.entropy <- entropy(FRE17_QTft) #entropy

FRE17_QT.netMx <- cbind(FRE17_QT.netMx, FRE17_QT.clusterCoef, FRE17_QT.degreeCent$centralization,
                        FRE17_QT.netDensity, FRE17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE17_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************
#NA

round = 17
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
GCFC17_Gg2 <- data.frame(GCFC17_G)
GCFC17_Gg2 <- GCFC17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_Gg2$player1
player2vector <- GCFC17_Gg2$player2
GCFC17_Gg3 <- GCFC17_Gg2
GCFC17_Gg3$p1inp2vec <- is.element(GCFC17_Gg3$player1, player2vector)
GCFC17_Gg3$p2inp1vec <- is.element(GCFC17_Gg3$player2, player1vector)

addPlayer1 <- GCFC17_Gg3[ which(GCFC17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC17_Gg3[ which(GCFC17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_Gg2 <- rbind(GCFC17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
GCFC17_Gft <- ftable(GCFC17_Gg2$player1, GCFC17_Gg2$player2)
GCFC17_Gft2 <- as.matrix(GCFC17_Gft)
numRows <- nrow(GCFC17_Gft2)
numCols <- ncol(GCFC17_Gft2)
GCFC17_Gft3 <- GCFC17_Gft2[c(2:numRows) , c(2:numCols)]
GCFC17_GTable <- graph.adjacency(GCFC17_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(GCFC17_GTable, vertex.label = V(GCFC17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
GCFC17_G.clusterCoef <- transitivity(GCFC17_GTable, type="global") #cluster coefficient
GCFC17_G.degreeCent <- centralization.degree(GCFC17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_Gftn <- as.network.matrix(GCFC17_Gft)
GCFC17_G.netDensity <- network.density(GCFC17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_G.entropy <- entropy(GCFC17_Gft) #entropy

GCFC17_G.netMx <- cbind(GCFC17_G.netMx, GCFC17_G.clusterCoef, GCFC17_G.degreeCent$centralization,
                        GCFC17_G.netDensity, GCFC17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
GCFC17_Bg2 <- data.frame(GCFC17_B)
GCFC17_Bg2 <- GCFC17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_Bg2$player1
player2vector <- GCFC17_Bg2$player2
GCFC17_Bg3 <- GCFC17_Bg2
GCFC17_Bg3$p1inp2vec <- is.element(GCFC17_Bg3$player1, player2vector)
GCFC17_Bg3$p2inp1vec <- is.element(GCFC17_Bg3$player2, player1vector)

addPlayer1 <- GCFC17_Bg3[ which(GCFC17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC17_Bg3[ which(GCFC17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_Bg2 <- rbind(GCFC17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
GCFC17_Bft <- ftable(GCFC17_Bg2$player1, GCFC17_Bg2$player2)
GCFC17_Bft2 <- as.matrix(GCFC17_Bft)
numRows <- nrow(GCFC17_Bft2)
numCols <- ncol(GCFC17_Bft2)
GCFC17_Bft3 <- GCFC17_Bft2[c(2:numRows) , c(2:numCols)]
GCFC17_BTable <- graph.adjacency(GCFC17_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(GCFC17_BTable, vertex.label = V(GCFC17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
GCFC17_B.clusterCoef <- transitivity(GCFC17_BTable, type="global") #cluster coefficient
GCFC17_B.degreeCent <- centralization.degree(GCFC17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_Bftn <- as.network.matrix(GCFC17_Bft)
GCFC17_B.netDensity <- network.density(GCFC17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_B.entropy <- entropy(GCFC17_Bft) #entropy

GCFC17_B.netMx <- cbind(GCFC17_B.netMx, GCFC17_B.clusterCoef, GCFC17_B.degreeCent$centralization,
                        GCFC17_B.netDensity, GCFC17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
GCFC17_SFg2 <- data.frame(GCFC17_SF)
GCFC17_SFg2 <- GCFC17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_SFg2$player1
player2vector <- GCFC17_SFg2$player2
GCFC17_SFg3 <- GCFC17_SFg2
GCFC17_SFg3$p1inp2vec <- is.element(GCFC17_SFg3$player1, player2vector)
GCFC17_SFg3$p2inp1vec <- is.element(GCFC17_SFg3$player2, player1vector)

addPlayer1 <- GCFC17_SFg3[ which(GCFC17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC17_SFg2 <- rbind(GCFC17_SFg2, addPlayer1)

#ROUND 17, FWD Stoppage graph using weighted edges
GCFC17_SFft <- ftable(GCFC17_SFg2$player1, GCFC17_SFg2$player2)
GCFC17_SFft2 <- as.matrix(GCFC17_SFft)
numRows <- nrow(GCFC17_SFft2)
numCols <- ncol(GCFC17_SFft2)
GCFC17_SFft3 <- GCFC17_SFft2[c(2:numRows) , c(1:numCols)]
GCFC17_SFTable <- graph.adjacency(GCFC17_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(GCFC17_SFTable, vertex.label = V(GCFC17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
GCFC17_SF.clusterCoef <- transitivity(GCFC17_SFTable, type="global") #cluster coefficient
GCFC17_SF.degreeCent <- centralization.degree(GCFC17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_SFftn <- as.network.matrix(GCFC17_SFft)
GCFC17_SF.netDensity <- network.density(GCFC17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_SF.entropy <- entropy(GCFC17_SFft) #entropy

GCFC17_SF.netMx <- cbind(GCFC17_SF.netMx, GCFC17_SF.clusterCoef, GCFC17_SF.degreeCent$centralization,
                         GCFC17_SF.netDensity, GCFC17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************

round = 17
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
GCFC17_TFg2 <- data.frame(GCFC17_TF)
GCFC17_TFg2 <- GCFC17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_TFg2$player1
player2vector <- GCFC17_TFg2$player2
GCFC17_TFg3 <- GCFC17_TFg2
GCFC17_TFg3$p1inp2vec <- is.element(GCFC17_TFg3$player1, player2vector)
GCFC17_TFg3$p2inp1vec <- is.element(GCFC17_TFg3$player2, player1vector)

addPlayer1 <- GCFC17_TFg3[ which(GCFC17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC17_TFg3[ which(GCFC17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_TFg2 <- rbind(GCFC17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
GCFC17_TFft <- ftable(GCFC17_TFg2$player1, GCFC17_TFg2$player2)
GCFC17_TFft2 <- as.matrix(GCFC17_TFft)
numRows <- nrow(GCFC17_TFft2)
numCols <- ncol(GCFC17_TFft2)
GCFC17_TFft3 <- GCFC17_TFft2[c(2:numRows) , c(2:numCols)]
GCFC17_TFTable <- graph.adjacency(GCFC17_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(GCFC17_TFTable, vertex.label = V(GCFC17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
GCFC17_TF.clusterCoef <- transitivity(GCFC17_TFTable, type="global") #cluster coefficient
GCFC17_TF.degreeCent <- centralization.degree(GCFC17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_TFftn <- as.network.matrix(GCFC17_TFft)
GCFC17_TF.netDensity <- network.density(GCFC17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_TF.entropy <- entropy(GCFC17_TFft) #entropy

GCFC17_TF.netMx <- cbind(GCFC17_TF.netMx, GCFC17_TF.clusterCoef, GCFC17_TF.degreeCent$centralization,
                         GCFC17_TF.netDensity, GCFC17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************
#NA

round = 17
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
GCFC17_SAMg2 <- data.frame(GCFC17_SAM)
GCFC17_SAMg2 <- GCFC17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_SAMg2$player1
player2vector <- GCFC17_SAMg2$player2
GCFC17_SAMg3 <- GCFC17_SAMg2
GCFC17_SAMg3$p1inp2vec <- is.element(GCFC17_SAMg3$player1, player2vector)
GCFC17_SAMg3$p2inp1vec <- is.element(GCFC17_SAMg3$player2, player1vector)

addPlayer1 <- GCFC17_SAMg3[ which(GCFC17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC17_SAMg3[ which(GCFC17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_SAMg2 <- rbind(GCFC17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
GCFC17_SAMft <- ftable(GCFC17_SAMg2$player1, GCFC17_SAMg2$player2)
GCFC17_SAMft2 <- as.matrix(GCFC17_SAMft)
numRows <- nrow(GCFC17_SAMft2)
numCols <- ncol(GCFC17_SAMft2)
GCFC17_SAMft3 <- GCFC17_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC17_SAMTable <- graph.adjacency(GCFC17_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(GCFC17_SAMTable, vertex.label = V(GCFC17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
GCFC17_SAM.clusterCoef <- transitivity(GCFC17_SAMTable, type="global") #cluster coefficient
GCFC17_SAM.degreeCent <- centralization.degree(GCFC17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_SAMftn <- as.network.matrix(GCFC17_SAMft)
GCFC17_SAM.netDensity <- network.density(GCFC17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_SAM.entropy <- entropy(GCFC17_SAMft) #entropy

GCFC17_SAM.netMx <- cbind(GCFC17_SAM.netMx, GCFC17_SAM.clusterCoef, GCFC17_SAM.degreeCent$centralization,
                          GCFC17_SAM.netDensity, GCFC17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
GCFC17_TAMg2 <- data.frame(GCFC17_TAM)
GCFC17_TAMg2 <- GCFC17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_TAMg2$player1
player2vector <- GCFC17_TAMg2$player2
GCFC17_TAMg3 <- GCFC17_TAMg2
GCFC17_TAMg3$p1inp2vec <- is.element(GCFC17_TAMg3$player1, player2vector)
GCFC17_TAMg3$p2inp1vec <- is.element(GCFC17_TAMg3$player2, player1vector)

addPlayer1 <- GCFC17_TAMg3[ which(GCFC17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC17_TAMg3[ which(GCFC17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_TAMg2 <- rbind(GCFC17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
GCFC17_TAMft <- ftable(GCFC17_TAMg2$player1, GCFC17_TAMg2$player2)
GCFC17_TAMft2 <- as.matrix(GCFC17_TAMft)
numRows <- nrow(GCFC17_TAMft2)
numCols <- ncol(GCFC17_TAMft2)
GCFC17_TAMft3 <- GCFC17_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC17_TAMTable <- graph.adjacency(GCFC17_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(GCFC17_TAMTable, vertex.label = V(GCFC17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
GCFC17_TAM.clusterCoef <- transitivity(GCFC17_TAMTable, type="global") #cluster coefficient
GCFC17_TAM.degreeCent <- centralization.degree(GCFC17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_TAMftn <- as.network.matrix(GCFC17_TAMft)
GCFC17_TAM.netDensity <- network.density(GCFC17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_TAM.entropy <- entropy(GCFC17_TAMft) #entropy

GCFC17_TAM.netMx <- cbind(GCFC17_TAM.netMx, GCFC17_TAM.clusterCoef, GCFC17_TAM.degreeCent$centralization,
                          GCFC17_TAM.netDensity, GCFC17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************
#NA

round = 17
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
GCFC17_SDMg2 <- data.frame(GCFC17_SDM)
GCFC17_SDMg2 <- GCFC17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_SDMg2$player1
player2vector <- GCFC17_SDMg2$player2
GCFC17_SDMg3 <- GCFC17_SDMg2
GCFC17_SDMg3$p1inp2vec <- is.element(GCFC17_SDMg3$player1, player2vector)
GCFC17_SDMg3$p2inp1vec <- is.element(GCFC17_SDMg3$player2, player1vector)

addPlayer1 <- GCFC17_SDMg3[ which(GCFC17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC17_SDMg3[ which(GCFC17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_SDMg2 <- rbind(GCFC17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
GCFC17_SDMft <- ftable(GCFC17_SDMg2$player1, GCFC17_SDMg2$player2)
GCFC17_SDMft2 <- as.matrix(GCFC17_SDMft)
numRows <- nrow(GCFC17_SDMft2)
numCols <- ncol(GCFC17_SDMft2)
GCFC17_SDMft3 <- GCFC17_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC17_SDMTable <- graph.adjacency(GCFC17_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(GCFC17_SDMTable, vertex.label = V(GCFC17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
GCFC17_SDM.clusterCoef <- transitivity(GCFC17_SDMTable, type="global") #cluster coefficient
GCFC17_SDM.degreeCent <- centralization.degree(GCFC17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_SDMftn <- as.network.matrix(GCFC17_SDMft)
GCFC17_SDM.netDensity <- network.density(GCFC17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_SDM.entropy <- entropy(GCFC17_SDMft) #entropy

GCFC17_SDM.netMx <- cbind(GCFC17_SDM.netMx, GCFC17_SDM.clusterCoef, GCFC17_SDM.degreeCent$centralization,
                          GCFC17_SDM.netDensity, GCFC17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
GCFC17_TDMg2 <- data.frame(GCFC17_TDM)
GCFC17_TDMg2 <- GCFC17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_TDMg2$player1
player2vector <- GCFC17_TDMg2$player2
GCFC17_TDMg3 <- GCFC17_TDMg2
GCFC17_TDMg3$p1inp2vec <- is.element(GCFC17_TDMg3$player1, player2vector)
GCFC17_TDMg3$p2inp1vec <- is.element(GCFC17_TDMg3$player2, player1vector)

addPlayer1 <- GCFC17_TDMg3[ which(GCFC17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC17_TDMg3[ which(GCFC17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_TDMg2 <- rbind(GCFC17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
GCFC17_TDMft <- ftable(GCFC17_TDMg2$player1, GCFC17_TDMg2$player2)
GCFC17_TDMft2 <- as.matrix(GCFC17_TDMft)
numRows <- nrow(GCFC17_TDMft2)
numCols <- ncol(GCFC17_TDMft2)
GCFC17_TDMft3 <- GCFC17_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC17_TDMTable <- graph.adjacency(GCFC17_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(GCFC17_TDMTable, vertex.label = V(GCFC17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
GCFC17_TDM.clusterCoef <- transitivity(GCFC17_TDMTable, type="global") #cluster coefficient
GCFC17_TDM.degreeCent <- centralization.degree(GCFC17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_TDMftn <- as.network.matrix(GCFC17_TDMft)
GCFC17_TDM.netDensity <- network.density(GCFC17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_TDM.entropy <- entropy(GCFC17_TDMft) #entropy

GCFC17_TDM.netMx <- cbind(GCFC17_TDM.netMx, GCFC17_TDM.clusterCoef, GCFC17_TDM.degreeCent$centralization,
                          GCFC17_TDM.netDensity, GCFC17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
GCFC17_SDg2 <- data.frame(GCFC17_SD)
GCFC17_SDg2 <- GCFC17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_SDg2$player1
player2vector <- GCFC17_SDg2$player2
GCFC17_SDg3 <- GCFC17_SDg2
GCFC17_SDg3$p1inp2vec <- is.element(GCFC17_SDg3$player1, player2vector)
GCFC17_SDg3$p2inp1vec <- is.element(GCFC17_SDg3$player2, player1vector)

addPlayer1 <- GCFC17_SDg3[ which(GCFC17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC17_SDg3[ which(GCFC17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_SDg2 <- rbind(GCFC17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
GCFC17_SDft <- ftable(GCFC17_SDg2$player1, GCFC17_SDg2$player2)
GCFC17_SDft2 <- as.matrix(GCFC17_SDft)
numRows <- nrow(GCFC17_SDft2)
numCols <- ncol(GCFC17_SDft2)
GCFC17_SDft3 <- GCFC17_SDft2[c(2:numRows) , c(2:numCols)]
GCFC17_SDTable <- graph.adjacency(GCFC17_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(GCFC17_SDTable, vertex.label = V(GCFC17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
GCFC17_SD.clusterCoef <- transitivity(GCFC17_SDTable, type="global") #cluster coefficient
GCFC17_SD.degreeCent <- centralization.degree(GCFC17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_SDftn <- as.network.matrix(GCFC17_SDft)
GCFC17_SD.netDensity <- network.density(GCFC17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_SD.entropy <- entropy(GCFC17_SDft) #entropy

GCFC17_SD.netMx <- cbind(GCFC17_SD.netMx, GCFC17_SD.clusterCoef, GCFC17_SD.degreeCent$centralization,
                         GCFC17_SD.netDensity, GCFC17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
GCFC17_TDg2 <- data.frame(GCFC17_TD)
GCFC17_TDg2 <- GCFC17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_TDg2$player1
player2vector <- GCFC17_TDg2$player2
GCFC17_TDg3 <- GCFC17_TDg2
GCFC17_TDg3$p1inp2vec <- is.element(GCFC17_TDg3$player1, player2vector)
GCFC17_TDg3$p2inp1vec <- is.element(GCFC17_TDg3$player2, player1vector)

addPlayer1 <- GCFC17_TDg3[ which(GCFC17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC17_TDg3[ which(GCFC17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_TDg2 <- rbind(GCFC17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
GCFC17_TDft <- ftable(GCFC17_TDg2$player1, GCFC17_TDg2$player2)
GCFC17_TDft2 <- as.matrix(GCFC17_TDft)
numRows <- nrow(GCFC17_TDft2)
numCols <- ncol(GCFC17_TDft2)
GCFC17_TDft3 <- GCFC17_TDft2[c(2:numRows) , c(2:numCols)]
GCFC17_TDTable <- graph.adjacency(GCFC17_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(GCFC17_TDTable, vertex.label = V(GCFC17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
GCFC17_TD.clusterCoef <- transitivity(GCFC17_TDTable, type="global") #cluster coefficient
GCFC17_TD.degreeCent <- centralization.degree(GCFC17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_TDftn <- as.network.matrix(GCFC17_TDft)
GCFC17_TD.netDensity <- network.density(GCFC17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_TD.entropy <- entropy(GCFC17_TDft) #entropy

GCFC17_TD.netMx <- cbind(GCFC17_TD.netMx, GCFC17_TD.clusterCoef, GCFC17_TD.degreeCent$centralization,
                         GCFC17_TD.netDensity, GCFC17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
GCFC17_QTg2 <- data.frame(GCFC17_QT)
GCFC17_QTg2 <- GCFC17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC17_QTg2$player1
player2vector <- GCFC17_QTg2$player2
GCFC17_QTg3 <- GCFC17_QTg2
GCFC17_QTg3$p1inp2vec <- is.element(GCFC17_QTg3$player1, player2vector)
GCFC17_QTg3$p2inp1vec <- is.element(GCFC17_QTg3$player2, player1vector)

addPlayer1 <- GCFC17_QTg3[ which(GCFC17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC17_QTg3[ which(GCFC17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC17_QTg2 <- rbind(GCFC17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
GCFC17_QTft <- ftable(GCFC17_QTg2$player1, GCFC17_QTg2$player2)
GCFC17_QTft2 <- as.matrix(GCFC17_QTft)
numRows <- nrow(GCFC17_QTft2)
numCols <- ncol(GCFC17_QTft2)
GCFC17_QTft3 <- GCFC17_QTft2[c(2:numRows) , c(2:numCols)]
GCFC17_QTTable <- graph.adjacency(GCFC17_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(GCFC17_QTTable, vertex.label = V(GCFC17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
GCFC17_QT.clusterCoef <- transitivity(GCFC17_QTTable, type="global") #cluster coefficient
GCFC17_QT.degreeCent <- centralization.degree(GCFC17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC17_QTftn <- as.network.matrix(GCFC17_QTft)
GCFC17_QT.netDensity <- network.density(GCFC17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC17_QT.entropy <- entropy(GCFC17_QTft) #entropy

GCFC17_QT.netMx <- cbind(GCFC17_QT.netMx, GCFC17_QT.clusterCoef, GCFC17_QT.degreeCent$centralization,
                         GCFC17_QT.netDensity, GCFC17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC17_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************

round = 17
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
GEEL17_Gg2 <- data.frame(GEEL17_G)
GEEL17_Gg2 <- GEEL17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_Gg2$player1
player2vector <- GEEL17_Gg2$player2
GEEL17_Gg3 <- GEEL17_Gg2
GEEL17_Gg3$p1inp2vec <- is.element(GEEL17_Gg3$player1, player2vector)
GEEL17_Gg3$p2inp1vec <- is.element(GEEL17_Gg3$player2, player1vector)

addPlayer1 <- GEEL17_Gg3[ which(GEEL17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL17_Gg3[ which(GEEL17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_Gg2 <- rbind(GEEL17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
GEEL17_Gft <- ftable(GEEL17_Gg2$player1, GEEL17_Gg2$player2)
GEEL17_Gft2 <- as.matrix(GEEL17_Gft)
numRows <- nrow(GEEL17_Gft2)
numCols <- ncol(GEEL17_Gft2)
GEEL17_Gft3 <- GEEL17_Gft2[c(2:numRows) , c(2:numCols)]
GEEL17_GTable <- graph.adjacency(GEEL17_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(GEEL17_GTable, vertex.label = V(GEEL17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
GEEL17_G.clusterCoef <- transitivity(GEEL17_GTable, type="global") #cluster coefficient
GEEL17_G.degreeCent <- centralization.degree(GEEL17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_Gftn <- as.network.matrix(GEEL17_Gft)
GEEL17_G.netDensity <- network.density(GEEL17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_G.entropy <- entropy(GEEL17_Gft) #entropy

GEEL17_G.netMx <- cbind(GEEL17_G.netMx, GEEL17_G.clusterCoef, GEEL17_G.degreeCent$centralization,
                        GEEL17_G.netDensity, GEEL17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
GEEL17_Bg2 <- data.frame(GEEL17_B)
GEEL17_Bg2 <- GEEL17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_Bg2$player1
player2vector <- GEEL17_Bg2$player2
GEEL17_Bg3 <- GEEL17_Bg2
GEEL17_Bg3$p1inp2vec <- is.element(GEEL17_Bg3$player1, player2vector)
GEEL17_Bg3$p2inp1vec <- is.element(GEEL17_Bg3$player2, player1vector)

addPlayer1 <- GEEL17_Bg3[ which(GEEL17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL17_Bg3[ which(GEEL17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_Bg2 <- rbind(GEEL17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
GEEL17_Bft <- ftable(GEEL17_Bg2$player1, GEEL17_Bg2$player2)
GEEL17_Bft2 <- as.matrix(GEEL17_Bft)
numRows <- nrow(GEEL17_Bft2)
numCols <- ncol(GEEL17_Bft2)
GEEL17_Bft3 <- GEEL17_Bft2[c(2:numRows) , c(2:numCols)]
GEEL17_BTable <- graph.adjacency(GEEL17_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(GEEL17_BTable, vertex.label = V(GEEL17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
GEEL17_B.clusterCoef <- transitivity(GEEL17_BTable, type="global") #cluster coefficient
GEEL17_B.degreeCent <- centralization.degree(GEEL17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_Bftn <- as.network.matrix(GEEL17_Bft)
GEEL17_B.netDensity <- network.density(GEEL17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_B.entropy <- entropy(GEEL17_Bft) #entropy

GEEL17_B.netMx <- cbind(GEEL17_B.netMx, GEEL17_B.clusterCoef, GEEL17_B.degreeCent$centralization,
                        GEEL17_B.netDensity, GEEL17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
GEEL17_SFg2 <- data.frame(GEEL17_SF)
GEEL17_SFg2 <- GEEL17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_SFg2$player1
player2vector <- GEEL17_SFg2$player2
GEEL17_SFg3 <- GEEL17_SFg2
GEEL17_SFg3$p1inp2vec <- is.element(GEEL17_SFg3$player1, player2vector)
GEEL17_SFg3$p2inp1vec <- is.element(GEEL17_SFg3$player2, player1vector)

addPlayer1 <- GEEL17_SFg3[ which(GEEL17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL17_SFg3[ which(GEEL17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_SFg2 <- rbind(GEEL17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
GEEL17_SFft <- ftable(GEEL17_SFg2$player1, GEEL17_SFg2$player2)
GEEL17_SFft2 <- as.matrix(GEEL17_SFft)
numRows <- nrow(GEEL17_SFft2)
numCols <- ncol(GEEL17_SFft2)
GEEL17_SFft3 <- GEEL17_SFft2[c(2:numRows) , c(2:numCols)]
GEEL17_SFTable <- graph.adjacency(GEEL17_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(GEEL17_SFTable, vertex.label = V(GEEL17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
GEEL17_SF.clusterCoef <- transitivity(GEEL17_SFTable, type="global") #cluster coefficient
GEEL17_SF.degreeCent <- centralization.degree(GEEL17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_SFftn <- as.network.matrix(GEEL17_SFft)
GEEL17_SF.netDensity <- network.density(GEEL17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_SF.entropy <- entropy(GEEL17_SFft) #entropy

GEEL17_SF.netMx <- cbind(GEEL17_SF.netMx, GEEL17_SF.clusterCoef, GEEL17_SF.degreeCent$centralization,
                         GEEL17_SF.netDensity, GEEL17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************

round = 17
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
GEEL17_TFg2 <- data.frame(GEEL17_TF)
GEEL17_TFg2 <- GEEL17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_TFg2$player1
player2vector <- GEEL17_TFg2$player2
GEEL17_TFg3 <- GEEL17_TFg2
GEEL17_TFg3$p1inp2vec <- is.element(GEEL17_TFg3$player1, player2vector)
GEEL17_TFg3$p2inp1vec <- is.element(GEEL17_TFg3$player2, player1vector)

addPlayer1 <- GEEL17_TFg3[ which(GEEL17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL17_TFg3[ which(GEEL17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_TFg2 <- rbind(GEEL17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
GEEL17_TFft <- ftable(GEEL17_TFg2$player1, GEEL17_TFg2$player2)
GEEL17_TFft2 <- as.matrix(GEEL17_TFft)
numRows <- nrow(GEEL17_TFft2)
numCols <- ncol(GEEL17_TFft2)
GEEL17_TFft3 <- GEEL17_TFft2[c(2:numRows) , c(2:numCols)]
GEEL17_TFTable <- graph.adjacency(GEEL17_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(GEEL17_TFTable, vertex.label = V(GEEL17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
GEEL17_TF.clusterCoef <- transitivity(GEEL17_TFTable, type="global") #cluster coefficient
GEEL17_TF.degreeCent <- centralization.degree(GEEL17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_TFftn <- as.network.matrix(GEEL17_TFft)
GEEL17_TF.netDensity <- network.density(GEEL17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_TF.entropy <- entropy(GEEL17_TFft) #entropy

GEEL17_TF.netMx <- cbind(GEEL17_TF.netMx, GEEL17_TF.clusterCoef, GEEL17_TF.degreeCent$centralization,
                         GEEL17_TF.netDensity, GEEL17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************

round = 17
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
GEEL17_SAMg2 <- data.frame(GEEL17_SAM)
GEEL17_SAMg2 <- GEEL17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_SAMg2$player1
player2vector <- GEEL17_SAMg2$player2
GEEL17_SAMg3 <- GEEL17_SAMg2
GEEL17_SAMg3$p1inp2vec <- is.element(GEEL17_SAMg3$player1, player2vector)
GEEL17_SAMg3$p2inp1vec <- is.element(GEEL17_SAMg3$player2, player1vector)

addPlayer1 <- GEEL17_SAMg3[ which(GEEL17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL17_SAMg3[ which(GEEL17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_SAMg2 <- rbind(GEEL17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
GEEL17_SAMft <- ftable(GEEL17_SAMg2$player1, GEEL17_SAMg2$player2)
GEEL17_SAMft2 <- as.matrix(GEEL17_SAMft)
numRows <- nrow(GEEL17_SAMft2)
numCols <- ncol(GEEL17_SAMft2)
GEEL17_SAMft3 <- GEEL17_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL17_SAMTable <- graph.adjacency(GEEL17_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(GEEL17_SAMTable, vertex.label = V(GEEL17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
GEEL17_SAM.clusterCoef <- transitivity(GEEL17_SAMTable, type="global") #cluster coefficient
GEEL17_SAM.degreeCent <- centralization.degree(GEEL17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_SAMftn <- as.network.matrix(GEEL17_SAMft)
GEEL17_SAM.netDensity <- network.density(GEEL17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_SAM.entropy <- entropy(GEEL17_SAMft) #entropy

GEEL17_SAM.netMx <- cbind(GEEL17_SAM.netMx, GEEL17_SAM.clusterCoef, GEEL17_SAM.degreeCent$centralization,
                          GEEL17_SAM.netDensity, GEEL17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
GEEL17_TAMg2 <- data.frame(GEEL17_TAM)
GEEL17_TAMg2 <- GEEL17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_TAMg2$player1
player2vector <- GEEL17_TAMg2$player2
GEEL17_TAMg3 <- GEEL17_TAMg2
GEEL17_TAMg3$p1inp2vec <- is.element(GEEL17_TAMg3$player1, player2vector)
GEEL17_TAMg3$p2inp1vec <- is.element(GEEL17_TAMg3$player2, player1vector)

addPlayer1 <- GEEL17_TAMg3[ which(GEEL17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL17_TAMg3[ which(GEEL17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_TAMg2 <- rbind(GEEL17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
GEEL17_TAMft <- ftable(GEEL17_TAMg2$player1, GEEL17_TAMg2$player2)
GEEL17_TAMft2 <- as.matrix(GEEL17_TAMft)
numRows <- nrow(GEEL17_TAMft2)
numCols <- ncol(GEEL17_TAMft2)
GEEL17_TAMft3 <- GEEL17_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL17_TAMTable <- graph.adjacency(GEEL17_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(GEEL17_TAMTable, vertex.label = V(GEEL17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
GEEL17_TAM.clusterCoef <- transitivity(GEEL17_TAMTable, type="global") #cluster coefficient
GEEL17_TAM.degreeCent <- centralization.degree(GEEL17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_TAMftn <- as.network.matrix(GEEL17_TAMft)
GEEL17_TAM.netDensity <- network.density(GEEL17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_TAM.entropy <- entropy(GEEL17_TAMft) #entropy

GEEL17_TAM.netMx <- cbind(GEEL17_TAM.netMx, GEEL17_TAM.clusterCoef, GEEL17_TAM.degreeCent$centralization,
                          GEEL17_TAM.netDensity, GEEL17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************
#NA

round = 17
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
GEEL17_SDMg2 <- data.frame(GEEL17_SDM)
GEEL17_SDMg2 <- GEEL17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_SDMg2$player1
player2vector <- GEEL17_SDMg2$player2
GEEL17_SDMg3 <- GEEL17_SDMg2
GEEL17_SDMg3$p1inp2vec <- is.element(GEEL17_SDMg3$player1, player2vector)
GEEL17_SDMg3$p2inp1vec <- is.element(GEEL17_SDMg3$player2, player1vector)

addPlayer1 <- GEEL17_SDMg3[ which(GEEL17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL17_SDMg3[ which(GEEL17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_SDMg2 <- rbind(GEEL17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
GEEL17_SDMft <- ftable(GEEL17_SDMg2$player1, GEEL17_SDMg2$player2)
GEEL17_SDMft2 <- as.matrix(GEEL17_SDMft)
numRows <- nrow(GEEL17_SDMft2)
numCols <- ncol(GEEL17_SDMft2)
GEEL17_SDMft3 <- GEEL17_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL17_SDMTable <- graph.adjacency(GEEL17_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(GEEL17_SDMTable, vertex.label = V(GEEL17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
GEEL17_SDM.clusterCoef <- transitivity(GEEL17_SDMTable, type="global") #cluster coefficient
GEEL17_SDM.degreeCent <- centralization.degree(GEEL17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_SDMftn <- as.network.matrix(GEEL17_SDMft)
GEEL17_SDM.netDensity <- network.density(GEEL17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_SDM.entropy <- entropy(GEEL17_SDMft) #entropy

GEEL17_SDM.netMx <- cbind(GEEL17_SDM.netMx, GEEL17_SDM.clusterCoef, GEEL17_SDM.degreeCent$centralization,
                          GEEL17_SDM.netDensity, GEEL17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************
#NA

round = 17
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
GEEL17_TDMg2 <- data.frame(GEEL17_TDM)
GEEL17_TDMg2 <- GEEL17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_TDMg2$player1
player2vector <- GEEL17_TDMg2$player2
GEEL17_TDMg3 <- GEEL17_TDMg2
GEEL17_TDMg3$p1inp2vec <- is.element(GEEL17_TDMg3$player1, player2vector)
GEEL17_TDMg3$p2inp1vec <- is.element(GEEL17_TDMg3$player2, player1vector)

addPlayer1 <- GEEL17_TDMg3[ which(GEEL17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL17_TDMg3[ which(GEEL17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_TDMg2 <- rbind(GEEL17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
GEEL17_TDMft <- ftable(GEEL17_TDMg2$player1, GEEL17_TDMg2$player2)
GEEL17_TDMft2 <- as.matrix(GEEL17_TDMft)
numRows <- nrow(GEEL17_TDMft2)
numCols <- ncol(GEEL17_TDMft2)
GEEL17_TDMft3 <- GEEL17_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL17_TDMTable <- graph.adjacency(GEEL17_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(GEEL17_TDMTable, vertex.label = V(GEEL17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
GEEL17_TDM.clusterCoef <- transitivity(GEEL17_TDMTable, type="global") #cluster coefficient
GEEL17_TDM.degreeCent <- centralization.degree(GEEL17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_TDMftn <- as.network.matrix(GEEL17_TDMft)
GEEL17_TDM.netDensity <- network.density(GEEL17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_TDM.entropy <- entropy(GEEL17_TDMft) #entropy

GEEL17_TDM.netMx <- cbind(GEEL17_TDM.netMx, GEEL17_TDM.clusterCoef, GEEL17_TDM.degreeCent$centralization,
                          GEEL17_TDM.netDensity, GEEL17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
GEEL17_SDg2 <- data.frame(GEEL17_SD)
GEEL17_SDg2 <- GEEL17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_SDg2$player1
player2vector <- GEEL17_SDg2$player2
GEEL17_SDg3 <- GEEL17_SDg2
GEEL17_SDg3$p1inp2vec <- is.element(GEEL17_SDg3$player1, player2vector)
GEEL17_SDg3$p2inp1vec <- is.element(GEEL17_SDg3$player2, player1vector)

addPlayer1 <- GEEL17_SDg3[ which(GEEL17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL17_SDg3[ which(GEEL17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_SDg2 <- rbind(GEEL17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
GEEL17_SDft <- ftable(GEEL17_SDg2$player1, GEEL17_SDg2$player2)
GEEL17_SDft2 <- as.matrix(GEEL17_SDft)
numRows <- nrow(GEEL17_SDft2)
numCols <- ncol(GEEL17_SDft2)
GEEL17_SDft3 <- GEEL17_SDft2[c(2:numRows) , c(2:numCols)]
GEEL17_SDTable <- graph.adjacency(GEEL17_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(GEEL17_SDTable, vertex.label = V(GEEL17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
GEEL17_SD.clusterCoef <- transitivity(GEEL17_SDTable, type="global") #cluster coefficient
GEEL17_SD.degreeCent <- centralization.degree(GEEL17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_SDftn <- as.network.matrix(GEEL17_SDft)
GEEL17_SD.netDensity <- network.density(GEEL17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_SD.entropy <- entropy(GEEL17_SDft) #entropy

GEEL17_SD.netMx <- cbind(GEEL17_SD.netMx, GEEL17_SD.clusterCoef, GEEL17_SD.degreeCent$centralization,
                         GEEL17_SD.netDensity, GEEL17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
GEEL17_TDg2 <- data.frame(GEEL17_TD)
GEEL17_TDg2 <- GEEL17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_TDg2$player1
player2vector <- GEEL17_TDg2$player2
GEEL17_TDg3 <- GEEL17_TDg2
GEEL17_TDg3$p1inp2vec <- is.element(GEEL17_TDg3$player1, player2vector)
GEEL17_TDg3$p2inp1vec <- is.element(GEEL17_TDg3$player2, player1vector)

addPlayer1 <- GEEL17_TDg3[ which(GEEL17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL17_TDg3[ which(GEEL17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_TDg2 <- rbind(GEEL17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
GEEL17_TDft <- ftable(GEEL17_TDg2$player1, GEEL17_TDg2$player2)
GEEL17_TDft2 <- as.matrix(GEEL17_TDft)
numRows <- nrow(GEEL17_TDft2)
numCols <- ncol(GEEL17_TDft2)
GEEL17_TDft3 <- GEEL17_TDft2[c(2:numRows) , c(2:numCols)]
GEEL17_TDTable <- graph.adjacency(GEEL17_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(GEEL17_TDTable, vertex.label = V(GEEL17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
GEEL17_TD.clusterCoef <- transitivity(GEEL17_TDTable, type="global") #cluster coefficient
GEEL17_TD.degreeCent <- centralization.degree(GEEL17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_TDftn <- as.network.matrix(GEEL17_TDft)
GEEL17_TD.netDensity <- network.density(GEEL17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_TD.entropy <- entropy(GEEL17_TDft) #entropy

GEEL17_TD.netMx <- cbind(GEEL17_TD.netMx, GEEL17_TD.clusterCoef, GEEL17_TD.degreeCent$centralization,
                         GEEL17_TD.netDensity, GEEL17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
GEEL17_QTg2 <- data.frame(GEEL17_QT)
GEEL17_QTg2 <- GEEL17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL17_QTg2$player1
player2vector <- GEEL17_QTg2$player2
GEEL17_QTg3 <- GEEL17_QTg2
GEEL17_QTg3$p1inp2vec <- is.element(GEEL17_QTg3$player1, player2vector)
GEEL17_QTg3$p2inp1vec <- is.element(GEEL17_QTg3$player2, player1vector)

addPlayer1 <- GEEL17_QTg3[ which(GEEL17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL17_QTg3[ which(GEEL17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL17_QTg2 <- rbind(GEEL17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
GEEL17_QTft <- ftable(GEEL17_QTg2$player1, GEEL17_QTg2$player2)
GEEL17_QTft2 <- as.matrix(GEEL17_QTft)
numRows <- nrow(GEEL17_QTft2)
numCols <- ncol(GEEL17_QTft2)
GEEL17_QTft3 <- GEEL17_QTft2[c(2:numRows) , c(2:numCols)]
GEEL17_QTTable <- graph.adjacency(GEEL17_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(GEEL17_QTTable, vertex.label = V(GEEL17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
GEEL17_QT.clusterCoef <- transitivity(GEEL17_QTTable, type="global") #cluster coefficient
GEEL17_QT.degreeCent <- centralization.degree(GEEL17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL17_QTftn <- as.network.matrix(GEEL17_QTft)
GEEL17_QT.netDensity <- network.density(GEEL17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL17_QT.entropy <- entropy(GEEL17_QTft) #entropy

GEEL17_QT.netMx <- cbind(GEEL17_QT.netMx, GEEL17_QT.clusterCoef, GEEL17_QT.degreeCent$centralization,
                         GEEL17_QT.netDensity, GEEL17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL17_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************

round = 17
teamName = "GWS"
KIoutcome = "Goal_F"
GWS17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
GWS17_Gg2 <- data.frame(GWS17_G)
GWS17_Gg2 <- GWS17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_Gg2$player1
player2vector <- GWS17_Gg2$player2
GWS17_Gg3 <- GWS17_Gg2
GWS17_Gg3$p1inp2vec <- is.element(GWS17_Gg3$player1, player2vector)
GWS17_Gg3$p2inp1vec <- is.element(GWS17_Gg3$player2, player1vector)

addPlayer1 <- GWS17_Gg3[ which(GWS17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS17_Gg3[ which(GWS17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_Gg2 <- rbind(GWS17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
GWS17_Gft <- ftable(GWS17_Gg2$player1, GWS17_Gg2$player2)
GWS17_Gft2 <- as.matrix(GWS17_Gft)
numRows <- nrow(GWS17_Gft2)
numCols <- ncol(GWS17_Gft2)
GWS17_Gft3 <- GWS17_Gft2[c(2:numRows) , c(2:numCols)]
GWS17_GTable <- graph.adjacency(GWS17_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(GWS17_GTable, vertex.label = V(GWS17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
GWS17_G.clusterCoef <- transitivity(GWS17_GTable, type="global") #cluster coefficient
GWS17_G.degreeCent <- centralization.degree(GWS17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_Gftn <- as.network.matrix(GWS17_Gft)
GWS17_G.netDensity <- network.density(GWS17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_G.entropy <- entropy(GWS17_Gft) #entropy

GWS17_G.netMx <- cbind(GWS17_G.netMx, GWS17_G.clusterCoef, GWS17_G.degreeCent$centralization,
                       GWS17_G.netDensity, GWS17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "GWS"
KIoutcome = "Behind_F"
GWS17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
GWS17_Bg2 <- data.frame(GWS17_B)
GWS17_Bg2 <- GWS17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_Bg2$player1
player2vector <- GWS17_Bg2$player2
GWS17_Bg3 <- GWS17_Bg2
GWS17_Bg3$p1inp2vec <- is.element(GWS17_Bg3$player1, player2vector)
GWS17_Bg3$p2inp1vec <- is.element(GWS17_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS17_Bg3[ which(GWS17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS17_Bg2 <- rbind(GWS17_Bg2, addPlayer2)

#ROUND 17, Behind graph using weighted edges
GWS17_Bft <- ftable(GWS17_Bg2$player1, GWS17_Bg2$player2)
GWS17_Bft2 <- as.matrix(GWS17_Bft)
numRows <- nrow(GWS17_Bft2)
numCols <- ncol(GWS17_Bft2)
GWS17_Bft3 <- GWS17_Bft2[c(1:numRows) , c(2:numCols)]
GWS17_BTable <- graph.adjacency(GWS17_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(GWS17_BTable, vertex.label = V(GWS17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
GWS17_B.clusterCoef <- transitivity(GWS17_BTable, type="global") #cluster coefficient
GWS17_B.degreeCent <- centralization.degree(GWS17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_Bftn <- as.network.matrix(GWS17_Bft)
GWS17_B.netDensity <- network.density(GWS17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_B.entropy <- entropy(GWS17_Bft) #entropy

GWS17_B.netMx <- cbind(GWS17_B.netMx, GWS17_B.clusterCoef, GWS17_B.degreeCent$centralization,
                       GWS17_B.netDensity, GWS17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
GWS17_SFg2 <- data.frame(GWS17_SF)
GWS17_SFg2 <- GWS17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_SFg2$player1
player2vector <- GWS17_SFg2$player2
GWS17_SFg3 <- GWS17_SFg2
GWS17_SFg3$p1inp2vec <- is.element(GWS17_SFg3$player1, player2vector)
GWS17_SFg3$p2inp1vec <- is.element(GWS17_SFg3$player2, player1vector)

addPlayer1 <- GWS17_SFg3[ which(GWS17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS17_SFg3[ which(GWS17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_SFg2 <- rbind(GWS17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
GWS17_SFft <- ftable(GWS17_SFg2$player1, GWS17_SFg2$player2)
GWS17_SFft2 <- as.matrix(GWS17_SFft)
numRows <- nrow(GWS17_SFft2)
numCols <- ncol(GWS17_SFft2)
GWS17_SFft3 <- GWS17_SFft2[c(2:numRows) , c(2:numCols)]
GWS17_SFTable <- graph.adjacency(GWS17_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(GWS17_SFTable, vertex.label = V(GWS17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
GWS17_SF.clusterCoef <- transitivity(GWS17_SFTable, type="global") #cluster coefficient
GWS17_SF.degreeCent <- centralization.degree(GWS17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_SFftn <- as.network.matrix(GWS17_SFft)
GWS17_SF.netDensity <- network.density(GWS17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_SF.entropy <- entropy(GWS17_SFft) #entropy

GWS17_SF.netMx <- cbind(GWS17_SF.netMx, GWS17_SF.clusterCoef, GWS17_SF.degreeCent$centralization,
                        GWS17_SF.netDensity, GWS17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************

round = 17
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
GWS17_TFg2 <- data.frame(GWS17_TF)
GWS17_TFg2 <- GWS17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_TFg2$player1
player2vector <- GWS17_TFg2$player2
GWS17_TFg3 <- GWS17_TFg2
GWS17_TFg3$p1inp2vec <- is.element(GWS17_TFg3$player1, player2vector)
GWS17_TFg3$p2inp1vec <- is.element(GWS17_TFg3$player2, player1vector)

addPlayer1 <- GWS17_TFg3[ which(GWS17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS17_TFg3[ which(GWS17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_TFg2 <- rbind(GWS17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
GWS17_TFft <- ftable(GWS17_TFg2$player1, GWS17_TFg2$player2)
GWS17_TFft2 <- as.matrix(GWS17_TFft)
numRows <- nrow(GWS17_TFft2)
numCols <- ncol(GWS17_TFft2)
GWS17_TFft3 <- GWS17_TFft2[c(2:numRows) , c(2:numCols)]
GWS17_TFTable <- graph.adjacency(GWS17_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(GWS17_TFTable, vertex.label = V(GWS17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
GWS17_TF.clusterCoef <- transitivity(GWS17_TFTable, type="global") #cluster coefficient
GWS17_TF.degreeCent <- centralization.degree(GWS17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_TFftn <- as.network.matrix(GWS17_TFft)
GWS17_TF.netDensity <- network.density(GWS17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_TF.entropy <- entropy(GWS17_TFft) #entropy

GWS17_TF.netMx <- cbind(GWS17_TF.netMx, GWS17_TF.clusterCoef, GWS17_TF.degreeCent$centralization,
                        GWS17_TF.netDensity, GWS17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************

round = 17
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
GWS17_SAMg2 <- data.frame(GWS17_SAM)
GWS17_SAMg2 <- GWS17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_SAMg2$player1
player2vector <- GWS17_SAMg2$player2
GWS17_SAMg3 <- GWS17_SAMg2
GWS17_SAMg3$p1inp2vec <- is.element(GWS17_SAMg3$player1, player2vector)
GWS17_SAMg3$p2inp1vec <- is.element(GWS17_SAMg3$player2, player1vector)

addPlayer1 <- GWS17_SAMg3[ which(GWS17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS17_SAMg3[ which(GWS17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_SAMg2 <- rbind(GWS17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
GWS17_SAMft <- ftable(GWS17_SAMg2$player1, GWS17_SAMg2$player2)
GWS17_SAMft2 <- as.matrix(GWS17_SAMft)
numRows <- nrow(GWS17_SAMft2)
numCols <- ncol(GWS17_SAMft2)
GWS17_SAMft3 <- GWS17_SAMft2[c(2:numRows) , c(2:numCols)]
GWS17_SAMTable <- graph.adjacency(GWS17_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(GWS17_SAMTable, vertex.label = V(GWS17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
GWS17_SAM.clusterCoef <- transitivity(GWS17_SAMTable, type="global") #cluster coefficient
GWS17_SAM.degreeCent <- centralization.degree(GWS17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_SAMftn <- as.network.matrix(GWS17_SAMft)
GWS17_SAM.netDensity <- network.density(GWS17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_SAM.entropy <- entropy(GWS17_SAMft) #entropy

GWS17_SAM.netMx <- cbind(GWS17_SAM.netMx, GWS17_SAM.clusterCoef, GWS17_SAM.degreeCent$centralization,
                         GWS17_SAM.netDensity, GWS17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
GWS17_TAMg2 <- data.frame(GWS17_TAM)
GWS17_TAMg2 <- GWS17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_TAMg2$player1
player2vector <- GWS17_TAMg2$player2
GWS17_TAMg3 <- GWS17_TAMg2
GWS17_TAMg3$p1inp2vec <- is.element(GWS17_TAMg3$player1, player2vector)
GWS17_TAMg3$p2inp1vec <- is.element(GWS17_TAMg3$player2, player1vector)

addPlayer1 <- GWS17_TAMg3[ which(GWS17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS17_TAMg3[ which(GWS17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_TAMg2 <- rbind(GWS17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
GWS17_TAMft <- ftable(GWS17_TAMg2$player1, GWS17_TAMg2$player2)
GWS17_TAMft2 <- as.matrix(GWS17_TAMft)
numRows <- nrow(GWS17_TAMft2)
numCols <- ncol(GWS17_TAMft2)
GWS17_TAMft3 <- GWS17_TAMft2[c(2:numRows) , c(2:numCols)]
GWS17_TAMTable <- graph.adjacency(GWS17_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(GWS17_TAMTable, vertex.label = V(GWS17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
GWS17_TAM.clusterCoef <- transitivity(GWS17_TAMTable, type="global") #cluster coefficient
GWS17_TAM.degreeCent <- centralization.degree(GWS17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_TAMftn <- as.network.matrix(GWS17_TAMft)
GWS17_TAM.netDensity <- network.density(GWS17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_TAM.entropy <- entropy(GWS17_TAMft) #entropy

GWS17_TAM.netMx <- cbind(GWS17_TAM.netMx, GWS17_TAM.clusterCoef, GWS17_TAM.degreeCent$centralization,
                         GWS17_TAM.netDensity, GWS17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************

round = 17
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
GWS17_SDMg2 <- data.frame(GWS17_SDM)
GWS17_SDMg2 <- GWS17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_SDMg2$player1
player2vector <- GWS17_SDMg2$player2
GWS17_SDMg3 <- GWS17_SDMg2
GWS17_SDMg3$p1inp2vec <- is.element(GWS17_SDMg3$player1, player2vector)
GWS17_SDMg3$p2inp1vec <- is.element(GWS17_SDMg3$player2, player1vector)

addPlayer1 <- GWS17_SDMg3[ which(GWS17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS17_SDMg3[ which(GWS17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_SDMg2 <- rbind(GWS17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
GWS17_SDMft <- ftable(GWS17_SDMg2$player1, GWS17_SDMg2$player2)
GWS17_SDMft2 <- as.matrix(GWS17_SDMft)
numRows <- nrow(GWS17_SDMft2)
numCols <- ncol(GWS17_SDMft2)
GWS17_SDMft3 <- GWS17_SDMft2[c(2:numRows) , c(2:numCols)]
GWS17_SDMTable <- graph.adjacency(GWS17_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(GWS17_SDMTable, vertex.label = V(GWS17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
GWS17_SDM.clusterCoef <- transitivity(GWS17_SDMTable, type="global") #cluster coefficient
GWS17_SDM.degreeCent <- centralization.degree(GWS17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_SDMftn <- as.network.matrix(GWS17_SDMft)
GWS17_SDM.netDensity <- network.density(GWS17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_SDM.entropy <- entropy(GWS17_SDMft) #entropy

GWS17_SDM.netMx <- cbind(GWS17_SDM.netMx, GWS17_SDM.clusterCoef, GWS17_SDM.degreeCent$centralization,
                         GWS17_SDM.netDensity, GWS17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
GWS17_TDMg2 <- data.frame(GWS17_TDM)
GWS17_TDMg2 <- GWS17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_TDMg2$player1
player2vector <- GWS17_TDMg2$player2
GWS17_TDMg3 <- GWS17_TDMg2
GWS17_TDMg3$p1inp2vec <- is.element(GWS17_TDMg3$player1, player2vector)
GWS17_TDMg3$p2inp1vec <- is.element(GWS17_TDMg3$player2, player1vector)

addPlayer1 <- GWS17_TDMg3[ which(GWS17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS17_TDMg3[ which(GWS17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_TDMg2 <- rbind(GWS17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
GWS17_TDMft <- ftable(GWS17_TDMg2$player1, GWS17_TDMg2$player2)
GWS17_TDMft2 <- as.matrix(GWS17_TDMft)
numRows <- nrow(GWS17_TDMft2)
numCols <- ncol(GWS17_TDMft2)
GWS17_TDMft3 <- GWS17_TDMft2[c(2:numRows) , c(2:numCols)]
GWS17_TDMTable <- graph.adjacency(GWS17_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(GWS17_TDMTable, vertex.label = V(GWS17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
GWS17_TDM.clusterCoef <- transitivity(GWS17_TDMTable, type="global") #cluster coefficient
GWS17_TDM.degreeCent <- centralization.degree(GWS17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_TDMftn <- as.network.matrix(GWS17_TDMft)
GWS17_TDM.netDensity <- network.density(GWS17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_TDM.entropy <- entropy(GWS17_TDMft) #entropy

GWS17_TDM.netMx <- cbind(GWS17_TDM.netMx, GWS17_TDM.clusterCoef, GWS17_TDM.degreeCent$centralization,
                         GWS17_TDM.netDensity, GWS17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
GWS17_SDg2 <- data.frame(GWS17_SD)
GWS17_SDg2 <- GWS17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_SDg2$player1
player2vector <- GWS17_SDg2$player2
GWS17_SDg3 <- GWS17_SDg2
GWS17_SDg3$p1inp2vec <- is.element(GWS17_SDg3$player1, player2vector)
GWS17_SDg3$p2inp1vec <- is.element(GWS17_SDg3$player2, player1vector)

addPlayer1 <- GWS17_SDg3[ which(GWS17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS17_SDg3[ which(GWS17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_SDg2 <- rbind(GWS17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
GWS17_SDft <- ftable(GWS17_SDg2$player1, GWS17_SDg2$player2)
GWS17_SDft2 <- as.matrix(GWS17_SDft)
numRows <- nrow(GWS17_SDft2)
numCols <- ncol(GWS17_SDft2)
GWS17_SDft3 <- GWS17_SDft2[c(2:numRows) , c(2:numCols)]
GWS17_SDTable <- graph.adjacency(GWS17_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(GWS17_SDTable, vertex.label = V(GWS17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
GWS17_SD.clusterCoef <- transitivity(GWS17_SDTable, type="global") #cluster coefficient
GWS17_SD.degreeCent <- centralization.degree(GWS17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_SDftn <- as.network.matrix(GWS17_SDft)
GWS17_SD.netDensity <- network.density(GWS17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_SD.entropy <- entropy(GWS17_SDft) #entropy

GWS17_SD.netMx <- cbind(GWS17_SD.netMx, GWS17_SD.clusterCoef, GWS17_SD.degreeCent$centralization,
                        GWS17_SD.netDensity, GWS17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************

round = 17
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
GWS17_TDg2 <- data.frame(GWS17_TD)
GWS17_TDg2 <- GWS17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_TDg2$player1
player2vector <- GWS17_TDg2$player2
GWS17_TDg3 <- GWS17_TDg2
GWS17_TDg3$p1inp2vec <- is.element(GWS17_TDg3$player1, player2vector)
GWS17_TDg3$p2inp1vec <- is.element(GWS17_TDg3$player2, player1vector)

addPlayer1 <- GWS17_TDg3[ which(GWS17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS17_TDg3[ which(GWS17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_TDg2 <- rbind(GWS17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
GWS17_TDft <- ftable(GWS17_TDg2$player1, GWS17_TDg2$player2)
GWS17_TDft2 <- as.matrix(GWS17_TDft)
numRows <- nrow(GWS17_TDft2)
numCols <- ncol(GWS17_TDft2)
GWS17_TDft3 <- GWS17_TDft2[c(2:numRows) , c(2:numCols)]
GWS17_TDTable <- graph.adjacency(GWS17_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(GWS17_TDTable, vertex.label = V(GWS17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
GWS17_TD.clusterCoef <- transitivity(GWS17_TDTable, type="global") #cluster coefficient
GWS17_TD.degreeCent <- centralization.degree(GWS17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_TDftn <- as.network.matrix(GWS17_TDft)
GWS17_TD.netDensity <- network.density(GWS17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_TD.entropy <- entropy(GWS17_TDft) #entropy

GWS17_TD.netMx <- cbind(GWS17_TD.netMx, GWS17_TD.clusterCoef, GWS17_TD.degreeCent$centralization,
                        GWS17_TD.netDensity, GWS17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
GWS17_QTg2 <- data.frame(GWS17_QT)
GWS17_QTg2 <- GWS17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS17_QTg2$player1
player2vector <- GWS17_QTg2$player2
GWS17_QTg3 <- GWS17_QTg2
GWS17_QTg3$p1inp2vec <- is.element(GWS17_QTg3$player1, player2vector)
GWS17_QTg3$p2inp1vec <- is.element(GWS17_QTg3$player2, player1vector)

addPlayer1 <- GWS17_QTg3[ which(GWS17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS17_QTg3[ which(GWS17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS17_QTg2 <- rbind(GWS17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
GWS17_QTft <- ftable(GWS17_QTg2$player1, GWS17_QTg2$player2)
GWS17_QTft2 <- as.matrix(GWS17_QTft)
numRows <- nrow(GWS17_QTft2)
numCols <- ncol(GWS17_QTft2)
GWS17_QTft3 <- GWS17_QTft2[c(2:numRows) , c(2:numCols)]
GWS17_QTTable <- graph.adjacency(GWS17_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(GWS17_QTTable, vertex.label = V(GWS17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
GWS17_QT.clusterCoef <- transitivity(GWS17_QTTable, type="global") #cluster coefficient
GWS17_QT.degreeCent <- centralization.degree(GWS17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS17_QTftn <- as.network.matrix(GWS17_QTft)
GWS17_QT.netDensity <- network.density(GWS17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS17_QT.entropy <- entropy(GWS17_QTft) #entropy

GWS17_QT.netMx <- cbind(GWS17_QT.netMx, GWS17_QT.clusterCoef, GWS17_QT.degreeCent$centralization,
                        GWS17_QT.netDensity, GWS17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS17_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************
#NA

round = 17
teamName = "HAW"
KIoutcome = "Goal_F"
HAW17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
HAW17_Gg2 <- data.frame(HAW17_G)
HAW17_Gg2 <- HAW17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_Gg2$player1
player2vector <- HAW17_Gg2$player2
HAW17_Gg3 <- HAW17_Gg2
HAW17_Gg3$p1inp2vec <- is.element(HAW17_Gg3$player1, player2vector)
HAW17_Gg3$p2inp1vec <- is.element(HAW17_Gg3$player2, player1vector)

addPlayer1 <- HAW17_Gg3[ which(HAW17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_Gg3[ which(HAW17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_Gg2 <- rbind(HAW17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
HAW17_Gft <- ftable(HAW17_Gg2$player1, HAW17_Gg2$player2)
HAW17_Gft2 <- as.matrix(HAW17_Gft)
numRows <- nrow(HAW17_Gft2)
numCols <- ncol(HAW17_Gft2)
HAW17_Gft3 <- HAW17_Gft2[c(2:numRows) , c(2:numCols)]
HAW17_GTable <- graph.adjacency(HAW17_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(HAW17_GTable, vertex.label = V(HAW17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
HAW17_G.clusterCoef <- transitivity(HAW17_GTable, type="global") #cluster coefficient
HAW17_G.degreeCent <- centralization.degree(HAW17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_Gftn <- as.network.matrix(HAW17_Gft)
HAW17_G.netDensity <- network.density(HAW17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_G.entropy <- entropy(HAW17_Gft) #entropy

HAW17_G.netMx <- cbind(HAW17_G.netMx, HAW17_G.clusterCoef, HAW17_G.degreeCent$centralization,
                       HAW17_G.netDensity, HAW17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************

round = 17
teamName = "HAW"
KIoutcome = "Behind_F"
HAW17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
HAW17_Bg2 <- data.frame(HAW17_B)
HAW17_Bg2 <- HAW17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_Bg2$player1
player2vector <- HAW17_Bg2$player2
HAW17_Bg3 <- HAW17_Bg2
HAW17_Bg3$p1inp2vec <- is.element(HAW17_Bg3$player1, player2vector)
HAW17_Bg3$p2inp1vec <- is.element(HAW17_Bg3$player2, player1vector)

addPlayer1 <- HAW17_Bg3[ which(HAW17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_Bg3[ which(HAW17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_Bg2 <- rbind(HAW17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
HAW17_Bft <- ftable(HAW17_Bg2$player1, HAW17_Bg2$player2)
HAW17_Bft2 <- as.matrix(HAW17_Bft)
numRows <- nrow(HAW17_Bft2)
numCols <- ncol(HAW17_Bft2)
HAW17_Bft3 <- HAW17_Bft2[c(2:numRows) , c(2:numCols)]
HAW17_BTable <- graph.adjacency(HAW17_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(HAW17_BTable, vertex.label = V(HAW17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
HAW17_B.clusterCoef <- transitivity(HAW17_BTable, type="global") #cluster coefficient
HAW17_B.degreeCent <- centralization.degree(HAW17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_Bftn <- as.network.matrix(HAW17_Bft)
HAW17_B.netDensity <- network.density(HAW17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_B.entropy <- entropy(HAW17_Bft) #entropy

HAW17_B.netMx <- cbind(HAW17_B.netMx, HAW17_B.clusterCoef, HAW17_B.degreeCent$centralization,
                       HAW17_B.netDensity, HAW17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
HAW17_SFg2 <- data.frame(HAW17_SF)
HAW17_SFg2 <- HAW17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_SFg2$player1
player2vector <- HAW17_SFg2$player2
HAW17_SFg3 <- HAW17_SFg2
HAW17_SFg3$p1inp2vec <- is.element(HAW17_SFg3$player1, player2vector)
HAW17_SFg3$p2inp1vec <- is.element(HAW17_SFg3$player2, player1vector)

addPlayer1 <- HAW17_SFg3[ which(HAW17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_SFg3[ which(HAW17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_SFg2 <- rbind(HAW17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
HAW17_SFft <- ftable(HAW17_SFg2$player1, HAW17_SFg2$player2)
HAW17_SFft2 <- as.matrix(HAW17_SFft)
numRows <- nrow(HAW17_SFft2)
numCols <- ncol(HAW17_SFft2)
HAW17_SFft3 <- HAW17_SFft2[c(2:numRows) , c(2:numCols)]
HAW17_SFTable <- graph.adjacency(HAW17_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(HAW17_SFTable, vertex.label = V(HAW17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
HAW17_SF.clusterCoef <- transitivity(HAW17_SFTable, type="global") #cluster coefficient
HAW17_SF.degreeCent <- centralization.degree(HAW17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_SFftn <- as.network.matrix(HAW17_SFft)
HAW17_SF.netDensity <- network.density(HAW17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_SF.entropy <- entropy(HAW17_SFft) #entropy

HAW17_SF.netMx <- cbind(HAW17_SF.netMx, HAW17_SF.clusterCoef, HAW17_SF.degreeCent$centralization,
                        HAW17_SF.netDensity, HAW17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************

round = 17
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
HAW17_TFg2 <- data.frame(HAW17_TF)
HAW17_TFg2 <- HAW17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_TFg2$player1
player2vector <- HAW17_TFg2$player2
HAW17_TFg3 <- HAW17_TFg2
HAW17_TFg3$p1inp2vec <- is.element(HAW17_TFg3$player1, player2vector)
HAW17_TFg3$p2inp1vec <- is.element(HAW17_TFg3$player2, player1vector)

addPlayer1 <- HAW17_TFg3[ which(HAW17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_TFg3[ which(HAW17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_TFg2 <- rbind(HAW17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
HAW17_TFft <- ftable(HAW17_TFg2$player1, HAW17_TFg2$player2)
HAW17_TFft2 <- as.matrix(HAW17_TFft)
numRows <- nrow(HAW17_TFft2)
numCols <- ncol(HAW17_TFft2)
HAW17_TFft3 <- HAW17_TFft2[c(2:numRows) , c(2:numCols)]
HAW17_TFTable <- graph.adjacency(HAW17_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(HAW17_TFTable, vertex.label = V(HAW17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
HAW17_TF.clusterCoef <- transitivity(HAW17_TFTable, type="global") #cluster coefficient
HAW17_TF.degreeCent <- centralization.degree(HAW17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_TFftn <- as.network.matrix(HAW17_TFft)
HAW17_TF.netDensity <- network.density(HAW17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_TF.entropy <- entropy(HAW17_TFft) #entropy

HAW17_TF.netMx <- cbind(HAW17_TF.netMx, HAW17_TF.clusterCoef, HAW17_TF.degreeCent$centralization,
                        HAW17_TF.netDensity, HAW17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************
#NA

round = 17
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
HAW17_SAMg2 <- data.frame(HAW17_SAM)
HAW17_SAMg2 <- HAW17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_SAMg2$player1
player2vector <- HAW17_SAMg2$player2
HAW17_SAMg3 <- HAW17_SAMg2
HAW17_SAMg3$p1inp2vec <- is.element(HAW17_SAMg3$player1, player2vector)
HAW17_SAMg3$p2inp1vec <- is.element(HAW17_SAMg3$player2, player1vector)

addPlayer1 <- HAW17_SAMg3[ which(HAW17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_SAMg3[ which(HAW17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_SAMg2 <- rbind(HAW17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
HAW17_SAMft <- ftable(HAW17_SAMg2$player1, HAW17_SAMg2$player2)
HAW17_SAMft2 <- as.matrix(HAW17_SAMft)
numRows <- nrow(HAW17_SAMft2)
numCols <- ncol(HAW17_SAMft2)
HAW17_SAMft3 <- HAW17_SAMft2[c(2:numRows) , c(2:numCols)]
HAW17_SAMTable <- graph.adjacency(HAW17_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(HAW17_SAMTable, vertex.label = V(HAW17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
HAW17_SAM.clusterCoef <- transitivity(HAW17_SAMTable, type="global") #cluster coefficient
HAW17_SAM.degreeCent <- centralization.degree(HAW17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_SAMftn <- as.network.matrix(HAW17_SAMft)
HAW17_SAM.netDensity <- network.density(HAW17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_SAM.entropy <- entropy(HAW17_SAMft) #entropy

HAW17_SAM.netMx <- cbind(HAW17_SAM.netMx, HAW17_SAM.clusterCoef, HAW17_SAM.degreeCent$centralization,
                         HAW17_SAM.netDensity, HAW17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
HAW17_TAMg2 <- data.frame(HAW17_TAM)
HAW17_TAMg2 <- HAW17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_TAMg2$player1
player2vector <- HAW17_TAMg2$player2
HAW17_TAMg3 <- HAW17_TAMg2
HAW17_TAMg3$p1inp2vec <- is.element(HAW17_TAMg3$player1, player2vector)
HAW17_TAMg3$p2inp1vec <- is.element(HAW17_TAMg3$player2, player1vector)

addPlayer1 <- HAW17_TAMg3[ which(HAW17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_TAMg3[ which(HAW17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_TAMg2 <- rbind(HAW17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
HAW17_TAMft <- ftable(HAW17_TAMg2$player1, HAW17_TAMg2$player2)
HAW17_TAMft2 <- as.matrix(HAW17_TAMft)
numRows <- nrow(HAW17_TAMft2)
numCols <- ncol(HAW17_TAMft2)
HAW17_TAMft3 <- HAW17_TAMft2[c(2:numRows) , c(2:numCols)]
HAW17_TAMTable <- graph.adjacency(HAW17_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(HAW17_TAMTable, vertex.label = V(HAW17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
HAW17_TAM.clusterCoef <- transitivity(HAW17_TAMTable, type="global") #cluster coefficient
HAW17_TAM.degreeCent <- centralization.degree(HAW17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_TAMftn <- as.network.matrix(HAW17_TAMft)
HAW17_TAM.netDensity <- network.density(HAW17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_TAM.entropy <- entropy(HAW17_TAMft) #entropy

HAW17_TAM.netMx <- cbind(HAW17_TAM.netMx, HAW17_TAM.clusterCoef, HAW17_TAM.degreeCent$centralization,
                         HAW17_TAM.netDensity, HAW17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************
#NA

round = 17
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
HAW17_SDMg2 <- data.frame(HAW17_SDM)
HAW17_SDMg2 <- HAW17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_SDMg2$player1
player2vector <- HAW17_SDMg2$player2
HAW17_SDMg3 <- HAW17_SDMg2
HAW17_SDMg3$p1inp2vec <- is.element(HAW17_SDMg3$player1, player2vector)
HAW17_SDMg3$p2inp1vec <- is.element(HAW17_SDMg3$player2, player1vector)

addPlayer1 <- HAW17_SDMg3[ which(HAW17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_SDMg3[ which(HAW17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_SDMg2 <- rbind(HAW17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
HAW17_SDMft <- ftable(HAW17_SDMg2$player1, HAW17_SDMg2$player2)
HAW17_SDMft2 <- as.matrix(HAW17_SDMft)
numRows <- nrow(HAW17_SDMft2)
numCols <- ncol(HAW17_SDMft2)
HAW17_SDMft3 <- HAW17_SDMft2[c(2:numRows) , c(2:numCols)]
HAW17_SDMTable <- graph.adjacency(HAW17_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(HAW17_SDMTable, vertex.label = V(HAW17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
HAW17_SDM.clusterCoef <- transitivity(HAW17_SDMTable, type="global") #cluster coefficient
HAW17_SDM.degreeCent <- centralization.degree(HAW17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_SDMftn <- as.network.matrix(HAW17_SDMft)
HAW17_SDM.netDensity <- network.density(HAW17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_SDM.entropy <- entropy(HAW17_SDMft) #entropy

HAW17_SDM.netMx <- cbind(HAW17_SDM.netMx, HAW17_SDM.clusterCoef, HAW17_SDM.degreeCent$centralization,
                         HAW17_SDM.netDensity, HAW17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
HAW17_TDMg2 <- data.frame(HAW17_TDM)
HAW17_TDMg2 <- HAW17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_TDMg2$player1
player2vector <- HAW17_TDMg2$player2
HAW17_TDMg3 <- HAW17_TDMg2
HAW17_TDMg3$p1inp2vec <- is.element(HAW17_TDMg3$player1, player2vector)
HAW17_TDMg3$p2inp1vec <- is.element(HAW17_TDMg3$player2, player1vector)

addPlayer1 <- HAW17_TDMg3[ which(HAW17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_TDMg3[ which(HAW17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_TDMg2 <- rbind(HAW17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
HAW17_TDMft <- ftable(HAW17_TDMg2$player1, HAW17_TDMg2$player2)
HAW17_TDMft2 <- as.matrix(HAW17_TDMft)
numRows <- nrow(HAW17_TDMft2)
numCols <- ncol(HAW17_TDMft2)
HAW17_TDMft3 <- HAW17_TDMft2[c(2:numRows) , c(2:numCols)]
HAW17_TDMTable <- graph.adjacency(HAW17_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(HAW17_TDMTable, vertex.label = V(HAW17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
HAW17_TDM.clusterCoef <- transitivity(HAW17_TDMTable, type="global") #cluster coefficient
HAW17_TDM.degreeCent <- centralization.degree(HAW17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_TDMftn <- as.network.matrix(HAW17_TDMft)
HAW17_TDM.netDensity <- network.density(HAW17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_TDM.entropy <- entropy(HAW17_TDMft) #entropy

HAW17_TDM.netMx <- cbind(HAW17_TDM.netMx, HAW17_TDM.clusterCoef, HAW17_TDM.degreeCent$centralization,
                         HAW17_TDM.netDensity, HAW17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
HAW17_SDg2 <- data.frame(HAW17_SD)
HAW17_SDg2 <- HAW17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_SDg2$player1
player2vector <- HAW17_SDg2$player2
HAW17_SDg3 <- HAW17_SDg2
HAW17_SDg3$p1inp2vec <- is.element(HAW17_SDg3$player1, player2vector)
HAW17_SDg3$p2inp1vec <- is.element(HAW17_SDg3$player2, player1vector)

addPlayer1 <- HAW17_SDg3[ which(HAW17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_SDg3[ which(HAW17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_SDg2 <- rbind(HAW17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
HAW17_SDft <- ftable(HAW17_SDg2$player1, HAW17_SDg2$player2)
HAW17_SDft2 <- as.matrix(HAW17_SDft)
numRows <- nrow(HAW17_SDft2)
numCols <- ncol(HAW17_SDft2)
HAW17_SDft3 <- HAW17_SDft2[c(2:numRows) , c(2:numCols)]
HAW17_SDTable <- graph.adjacency(HAW17_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(HAW17_SDTable, vertex.label = V(HAW17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
HAW17_SD.clusterCoef <- transitivity(HAW17_SDTable, type="global") #cluster coefficient
HAW17_SD.degreeCent <- centralization.degree(HAW17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_SDftn <- as.network.matrix(HAW17_SDft)
HAW17_SD.netDensity <- network.density(HAW17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_SD.entropy <- entropy(HAW17_SDft) #entropy

HAW17_SD.netMx <- cbind(HAW17_SD.netMx, HAW17_SD.clusterCoef, HAW17_SD.degreeCent$centralization,
                        HAW17_SD.netDensity, HAW17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
HAW17_TDg2 <- data.frame(HAW17_TD)
HAW17_TDg2 <- HAW17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_TDg2$player1
player2vector <- HAW17_TDg2$player2
HAW17_TDg3 <- HAW17_TDg2
HAW17_TDg3$p1inp2vec <- is.element(HAW17_TDg3$player1, player2vector)
HAW17_TDg3$p2inp1vec <- is.element(HAW17_TDg3$player2, player1vector)

addPlayer1 <- HAW17_TDg3[ which(HAW17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_TDg3[ which(HAW17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_TDg2 <- rbind(HAW17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
HAW17_TDft <- ftable(HAW17_TDg2$player1, HAW17_TDg2$player2)
HAW17_TDft2 <- as.matrix(HAW17_TDft)
numRows <- nrow(HAW17_TDft2)
numCols <- ncol(HAW17_TDft2)
HAW17_TDft3 <- HAW17_TDft2[c(2:numRows) , c(2:numCols)]
HAW17_TDTable <- graph.adjacency(HAW17_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(HAW17_TDTable, vertex.label = V(HAW17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
HAW17_TD.clusterCoef <- transitivity(HAW17_TDTable, type="global") #cluster coefficient
HAW17_TD.degreeCent <- centralization.degree(HAW17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_TDftn <- as.network.matrix(HAW17_TDft)
HAW17_TD.netDensity <- network.density(HAW17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_TD.entropy <- entropy(HAW17_TDft) #entropy

HAW17_TD.netMx <- cbind(HAW17_TD.netMx, HAW17_TD.clusterCoef, HAW17_TD.degreeCent$centralization,
                        HAW17_TD.netDensity, HAW17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
HAW17_QTg2 <- data.frame(HAW17_QT)
HAW17_QTg2 <- HAW17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW17_QTg2$player1
player2vector <- HAW17_QTg2$player2
HAW17_QTg3 <- HAW17_QTg2
HAW17_QTg3$p1inp2vec <- is.element(HAW17_QTg3$player1, player2vector)
HAW17_QTg3$p2inp1vec <- is.element(HAW17_QTg3$player2, player1vector)

addPlayer1 <- HAW17_QTg3[ which(HAW17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW17_QTg3[ which(HAW17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW17_QTg2 <- rbind(HAW17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
HAW17_QTft <- ftable(HAW17_QTg2$player1, HAW17_QTg2$player2)
HAW17_QTft2 <- as.matrix(HAW17_QTft)
numRows <- nrow(HAW17_QTft2)
numCols <- ncol(HAW17_QTft2)
HAW17_QTft3 <- HAW17_QTft2[c(2:numRows) , c(2:numCols)]
HAW17_QTTable <- graph.adjacency(HAW17_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(HAW17_QTTable, vertex.label = V(HAW17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
HAW17_QT.clusterCoef <- transitivity(HAW17_QTTable, type="global") #cluster coefficient
HAW17_QT.degreeCent <- centralization.degree(HAW17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW17_QTftn <- as.network.matrix(HAW17_QTft)
HAW17_QT.netDensity <- network.density(HAW17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW17_QT.entropy <- entropy(HAW17_QTft) #entropy

HAW17_QT.netMx <- cbind(HAW17_QT.netMx, HAW17_QT.clusterCoef, HAW17_QT.degreeCent$centralization,
                        HAW17_QT.netDensity, HAW17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW17_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************
#NA

round = 17
teamName = "MELB"
KIoutcome = "Goal_F"
MELB17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
MELB17_Gg2 <- data.frame(MELB17_G)
MELB17_Gg2 <- MELB17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_Gg2$player1
player2vector <- MELB17_Gg2$player2
MELB17_Gg3 <- MELB17_Gg2
MELB17_Gg3$p1inp2vec <- is.element(MELB17_Gg3$player1, player2vector)
MELB17_Gg3$p2inp1vec <- is.element(MELB17_Gg3$player2, player1vector)

addPlayer1 <- MELB17_Gg3[ which(MELB17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB17_Gg2 <- rbind(MELB17_Gg2, addPlayer1)

#ROUND 17, Goal graph using weighted edges
MELB17_Gft <- ftable(MELB17_Gg2$player1, MELB17_Gg2$player2)
MELB17_Gft2 <- as.matrix(MELB17_Gft)
numRows <- nrow(MELB17_Gft2)
numCols <- ncol(MELB17_Gft2)
MELB17_Gft3 <- MELB17_Gft2[c(2:numRows) , c(1:numCols)]
MELB17_GTable <- graph.adjacency(MELB17_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(MELB17_GTable, vertex.label = V(MELB17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
MELB17_G.clusterCoef <- transitivity(MELB17_GTable, type="global") #cluster coefficient
MELB17_G.degreeCent <- centralization.degree(MELB17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_Gftn <- as.network.matrix(MELB17_Gft)
MELB17_G.netDensity <- network.density(MELB17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_G.entropy <- entropy(MELB17_Gft) #entropy

MELB17_G.netMx <- cbind(MELB17_G.netMx, MELB17_G.clusterCoef, MELB17_G.degreeCent$centralization,
                        MELB17_G.netDensity, MELB17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "MELB"
KIoutcome = "Behind_F"
MELB17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
MELB17_Bg2 <- data.frame(MELB17_B)
MELB17_Bg2 <- MELB17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_Bg2$player1
player2vector <- MELB17_Bg2$player2
MELB17_Bg3 <- MELB17_Bg2
MELB17_Bg3$p1inp2vec <- is.element(MELB17_Bg3$player1, player2vector)
MELB17_Bg3$p2inp1vec <- is.element(MELB17_Bg3$player2, player1vector)

addPlayer1 <- MELB17_Bg3[ which(MELB17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB17_Bg3[ which(MELB17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_Bg2 <- rbind(MELB17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
MELB17_Bft <- ftable(MELB17_Bg2$player1, MELB17_Bg2$player2)
MELB17_Bft2 <- as.matrix(MELB17_Bft)
numRows <- nrow(MELB17_Bft2)
numCols <- ncol(MELB17_Bft2)
MELB17_Bft3 <- MELB17_Bft2[c(2:numRows) , c(2:numCols)]
MELB17_BTable <- graph.adjacency(MELB17_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(MELB17_BTable, vertex.label = V(MELB17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
MELB17_B.clusterCoef <- transitivity(MELB17_BTable, type="global") #cluster coefficient
MELB17_B.degreeCent <- centralization.degree(MELB17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_Bftn <- as.network.matrix(MELB17_Bft)
MELB17_B.netDensity <- network.density(MELB17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_B.entropy <- entropy(MELB17_Bft) #entropy

MELB17_B.netMx <- cbind(MELB17_B.netMx, MELB17_B.clusterCoef, MELB17_B.degreeCent$centralization,
                        MELB17_B.netDensity, MELB17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
MELB17_SFg2 <- data.frame(MELB17_SF)
MELB17_SFg2 <- MELB17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_SFg2$player1
player2vector <- MELB17_SFg2$player2
MELB17_SFg3 <- MELB17_SFg2
MELB17_SFg3$p1inp2vec <- is.element(MELB17_SFg3$player1, player2vector)
MELB17_SFg3$p2inp1vec <- is.element(MELB17_SFg3$player2, player1vector)

addPlayer1 <- MELB17_SFg3[ which(MELB17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB17_SFg3[ which(MELB17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_SFg2 <- rbind(MELB17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
MELB17_SFft <- ftable(MELB17_SFg2$player1, MELB17_SFg2$player2)
MELB17_SFft2 <- as.matrix(MELB17_SFft)
numRows <- nrow(MELB17_SFft2)
numCols <- ncol(MELB17_SFft2)
MELB17_SFft3 <- MELB17_SFft2[c(2:numRows) , c(2:numCols)]
MELB17_SFTable <- graph.adjacency(MELB17_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(MELB17_SFTable, vertex.label = V(MELB17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
MELB17_SF.clusterCoef <- transitivity(MELB17_SFTable, type="global") #cluster coefficient
MELB17_SF.degreeCent <- centralization.degree(MELB17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_SFftn <- as.network.matrix(MELB17_SFft)
MELB17_SF.netDensity <- network.density(MELB17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_SF.entropy <- entropy(MELB17_SFft) #entropy

MELB17_SF.netMx <- cbind(MELB17_SF.netMx, MELB17_SF.clusterCoef, MELB17_SF.degreeCent$centralization,
                         MELB17_SF.netDensity, MELB17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
MELB17_TFg2 <- data.frame(MELB17_TF)
MELB17_TFg2 <- MELB17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_TFg2$player1
player2vector <- MELB17_TFg2$player2
MELB17_TFg3 <- MELB17_TFg2
MELB17_TFg3$p1inp2vec <- is.element(MELB17_TFg3$player1, player2vector)
MELB17_TFg3$p2inp1vec <- is.element(MELB17_TFg3$player2, player1vector)

addPlayer1 <- MELB17_TFg3[ which(MELB17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB17_TFg3[ which(MELB17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_TFg2 <- rbind(MELB17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
MELB17_TFft <- ftable(MELB17_TFg2$player1, MELB17_TFg2$player2)
MELB17_TFft2 <- as.matrix(MELB17_TFft)
numRows <- nrow(MELB17_TFft2)
numCols <- ncol(MELB17_TFft2)
MELB17_TFft3 <- MELB17_TFft2[c(2:numRows) , c(2:numCols)]
MELB17_TFTable <- graph.adjacency(MELB17_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(MELB17_TFTable, vertex.label = V(MELB17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
MELB17_TF.clusterCoef <- transitivity(MELB17_TFTable, type="global") #cluster coefficient
MELB17_TF.degreeCent <- centralization.degree(MELB17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_TFftn <- as.network.matrix(MELB17_TFft)
MELB17_TF.netDensity <- network.density(MELB17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_TF.entropy <- entropy(MELB17_TFft) #entropy

MELB17_TF.netMx <- cbind(MELB17_TF.netMx, MELB17_TF.clusterCoef, MELB17_TF.degreeCent$centralization,
                         MELB17_TF.netDensity, MELB17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************
#NA

round = 17
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
MELB17_SAMg2 <- data.frame(MELB17_SAM)
MELB17_SAMg2 <- MELB17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_SAMg2$player1
player2vector <- MELB17_SAMg2$player2
MELB17_SAMg3 <- MELB17_SAMg2
MELB17_SAMg3$p1inp2vec <- is.element(MELB17_SAMg3$player1, player2vector)
MELB17_SAMg3$p2inp1vec <- is.element(MELB17_SAMg3$player2, player1vector)

addPlayer1 <- MELB17_SAMg3[ which(MELB17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB17_SAMg3[ which(MELB17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_SAMg2 <- rbind(MELB17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
MELB17_SAMft <- ftable(MELB17_SAMg2$player1, MELB17_SAMg2$player2)
MELB17_SAMft2 <- as.matrix(MELB17_SAMft)
numRows <- nrow(MELB17_SAMft2)
numCols <- ncol(MELB17_SAMft2)
MELB17_SAMft3 <- MELB17_SAMft2[c(2:numRows) , c(2:numCols)]
MELB17_SAMTable <- graph.adjacency(MELB17_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(MELB17_SAMTable, vertex.label = V(MELB17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
MELB17_SAM.clusterCoef <- transitivity(MELB17_SAMTable, type="global") #cluster coefficient
MELB17_SAM.degreeCent <- centralization.degree(MELB17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_SAMftn <- as.network.matrix(MELB17_SAMft)
MELB17_SAM.netDensity <- network.density(MELB17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_SAM.entropy <- entropy(MELB17_SAMft) #entropy

MELB17_SAM.netMx <- cbind(MELB17_SAM.netMx, MELB17_SAM.clusterCoef, MELB17_SAM.degreeCent$centralization,
                          MELB17_SAM.netDensity, MELB17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
MELB17_TAMg2 <- data.frame(MELB17_TAM)
MELB17_TAMg2 <- MELB17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_TAMg2$player1
player2vector <- MELB17_TAMg2$player2
MELB17_TAMg3 <- MELB17_TAMg2
MELB17_TAMg3$p1inp2vec <- is.element(MELB17_TAMg3$player1, player2vector)
MELB17_TAMg3$p2inp1vec <- is.element(MELB17_TAMg3$player2, player1vector)

addPlayer1 <- MELB17_TAMg3[ which(MELB17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB17_TAMg3[ which(MELB17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_TAMg2 <- rbind(MELB17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
MELB17_TAMft <- ftable(MELB17_TAMg2$player1, MELB17_TAMg2$player2)
MELB17_TAMft2 <- as.matrix(MELB17_TAMft)
numRows <- nrow(MELB17_TAMft2)
numCols <- ncol(MELB17_TAMft2)
MELB17_TAMft3 <- MELB17_TAMft2[c(2:numRows) , c(2:numCols)]
MELB17_TAMTable <- graph.adjacency(MELB17_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(MELB17_TAMTable, vertex.label = V(MELB17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
MELB17_TAM.clusterCoef <- transitivity(MELB17_TAMTable, type="global") #cluster coefficient
MELB17_TAM.degreeCent <- centralization.degree(MELB17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_TAMftn <- as.network.matrix(MELB17_TAMft)
MELB17_TAM.netDensity <- network.density(MELB17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_TAM.entropy <- entropy(MELB17_TAMft) #entropy

MELB17_TAM.netMx <- cbind(MELB17_TAM.netMx, MELB17_TAM.clusterCoef, MELB17_TAM.degreeCent$centralization,
                          MELB17_TAM.netDensity, MELB17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************

round = 17
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
MELB17_SDMg2 <- data.frame(MELB17_SDM)
MELB17_SDMg2 <- MELB17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_SDMg2$player1
player2vector <- MELB17_SDMg2$player2
MELB17_SDMg3 <- MELB17_SDMg2
MELB17_SDMg3$p1inp2vec <- is.element(MELB17_SDMg3$player1, player2vector)
MELB17_SDMg3$p2inp1vec <- is.element(MELB17_SDMg3$player2, player1vector)

addPlayer1 <- MELB17_SDMg3[ which(MELB17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB17_SDMg3[ which(MELB17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_SDMg2 <- rbind(MELB17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
MELB17_SDMft <- ftable(MELB17_SDMg2$player1, MELB17_SDMg2$player2)
MELB17_SDMft2 <- as.matrix(MELB17_SDMft)
numRows <- nrow(MELB17_SDMft2)
numCols <- ncol(MELB17_SDMft2)
MELB17_SDMft3 <- MELB17_SDMft2[c(2:numRows) , c(2:numCols)]
MELB17_SDMTable <- graph.adjacency(MELB17_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(MELB17_SDMTable, vertex.label = V(MELB17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
MELB17_SDM.clusterCoef <- transitivity(MELB17_SDMTable, type="global") #cluster coefficient
MELB17_SDM.degreeCent <- centralization.degree(MELB17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_SDMftn <- as.network.matrix(MELB17_SDMft)
MELB17_SDM.netDensity <- network.density(MELB17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_SDM.entropy <- entropy(MELB17_SDMft) #entropy

MELB17_SDM.netMx <- cbind(MELB17_SDM.netMx, MELB17_SDM.clusterCoef, MELB17_SDM.degreeCent$centralization,
                          MELB17_SDM.netDensity, MELB17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
MELB17_TDMg2 <- data.frame(MELB17_TDM)
MELB17_TDMg2 <- MELB17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_TDMg2$player1
player2vector <- MELB17_TDMg2$player2
MELB17_TDMg3 <- MELB17_TDMg2
MELB17_TDMg3$p1inp2vec <- is.element(MELB17_TDMg3$player1, player2vector)
MELB17_TDMg3$p2inp1vec <- is.element(MELB17_TDMg3$player2, player1vector)

addPlayer1 <- MELB17_TDMg3[ which(MELB17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB17_TDMg3[ which(MELB17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_TDMg2 <- rbind(MELB17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
MELB17_TDMft <- ftable(MELB17_TDMg2$player1, MELB17_TDMg2$player2)
MELB17_TDMft2 <- as.matrix(MELB17_TDMft)
numRows <- nrow(MELB17_TDMft2)
numCols <- ncol(MELB17_TDMft2)
MELB17_TDMft3 <- MELB17_TDMft2[c(2:numRows) , c(2:numCols)]
MELB17_TDMTable <- graph.adjacency(MELB17_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(MELB17_TDMTable, vertex.label = V(MELB17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
MELB17_TDM.clusterCoef <- transitivity(MELB17_TDMTable, type="global") #cluster coefficient
MELB17_TDM.degreeCent <- centralization.degree(MELB17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_TDMftn <- as.network.matrix(MELB17_TDMft)
MELB17_TDM.netDensity <- network.density(MELB17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_TDM.entropy <- entropy(MELB17_TDMft) #entropy

MELB17_TDM.netMx <- cbind(MELB17_TDM.netMx, MELB17_TDM.clusterCoef, MELB17_TDM.degreeCent$centralization,
                          MELB17_TDM.netDensity, MELB17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
MELB17_SDg2 <- data.frame(MELB17_SD)
MELB17_SDg2 <- MELB17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_SDg2$player1
player2vector <- MELB17_SDg2$player2
MELB17_SDg3 <- MELB17_SDg2
MELB17_SDg3$p1inp2vec <- is.element(MELB17_SDg3$player1, player2vector)
MELB17_SDg3$p2inp1vec <- is.element(MELB17_SDg3$player2, player1vector)

addPlayer1 <- MELB17_SDg3[ which(MELB17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB17_SDg3[ which(MELB17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_SDg2 <- rbind(MELB17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
MELB17_SDft <- ftable(MELB17_SDg2$player1, MELB17_SDg2$player2)
MELB17_SDft2 <- as.matrix(MELB17_SDft)
numRows <- nrow(MELB17_SDft2)
numCols <- ncol(MELB17_SDft2)
MELB17_SDft3 <- MELB17_SDft2[c(2:numRows) , c(2:numCols)]
MELB17_SDTable <- graph.adjacency(MELB17_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(MELB17_SDTable, vertex.label = V(MELB17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
MELB17_SD.clusterCoef <- transitivity(MELB17_SDTable, type="global") #cluster coefficient
MELB17_SD.degreeCent <- centralization.degree(MELB17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_SDftn <- as.network.matrix(MELB17_SDft)
MELB17_SD.netDensity <- network.density(MELB17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_SD.entropy <- entropy(MELB17_SDft) #entropy

MELB17_SD.netMx <- cbind(MELB17_SD.netMx, MELB17_SD.clusterCoef, MELB17_SD.degreeCent$centralization,
                         MELB17_SD.netDensity, MELB17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
MELB17_TDg2 <- data.frame(MELB17_TD)
MELB17_TDg2 <- MELB17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_TDg2$player1
player2vector <- MELB17_TDg2$player2
MELB17_TDg3 <- MELB17_TDg2
MELB17_TDg3$p1inp2vec <- is.element(MELB17_TDg3$player1, player2vector)
MELB17_TDg3$p2inp1vec <- is.element(MELB17_TDg3$player2, player1vector)

addPlayer1 <- MELB17_TDg3[ which(MELB17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB17_TDg3[ which(MELB17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_TDg2 <- rbind(MELB17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
MELB17_TDft <- ftable(MELB17_TDg2$player1, MELB17_TDg2$player2)
MELB17_TDft2 <- as.matrix(MELB17_TDft)
numRows <- nrow(MELB17_TDft2)
numCols <- ncol(MELB17_TDft2)
MELB17_TDft3 <- MELB17_TDft2[c(2:numRows) , c(2:numCols)]
MELB17_TDTable <- graph.adjacency(MELB17_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(MELB17_TDTable, vertex.label = V(MELB17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
MELB17_TD.clusterCoef <- transitivity(MELB17_TDTable, type="global") #cluster coefficient
MELB17_TD.degreeCent <- centralization.degree(MELB17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_TDftn <- as.network.matrix(MELB17_TDft)
MELB17_TD.netDensity <- network.density(MELB17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_TD.entropy <- entropy(MELB17_TDft) #entropy

MELB17_TD.netMx <- cbind(MELB17_TD.netMx, MELB17_TD.clusterCoef, MELB17_TD.degreeCent$centralization,
                         MELB17_TD.netDensity, MELB17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
MELB17_QTg2 <- data.frame(MELB17_QT)
MELB17_QTg2 <- MELB17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB17_QTg2$player1
player2vector <- MELB17_QTg2$player2
MELB17_QTg3 <- MELB17_QTg2
MELB17_QTg3$p1inp2vec <- is.element(MELB17_QTg3$player1, player2vector)
MELB17_QTg3$p2inp1vec <- is.element(MELB17_QTg3$player2, player1vector)

addPlayer1 <- MELB17_QTg3[ which(MELB17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB17_QTg3[ which(MELB17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB17_QTg2 <- rbind(MELB17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
MELB17_QTft <- ftable(MELB17_QTg2$player1, MELB17_QTg2$player2)
MELB17_QTft2 <- as.matrix(MELB17_QTft)
numRows <- nrow(MELB17_QTft2)
numCols <- ncol(MELB17_QTft2)
MELB17_QTft3 <- MELB17_QTft2[c(2:numRows) , c(2:numCols)]
MELB17_QTTable <- graph.adjacency(MELB17_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(MELB17_QTTable, vertex.label = V(MELB17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
MELB17_QT.clusterCoef <- transitivity(MELB17_QTTable, type="global") #cluster coefficient
MELB17_QT.degreeCent <- centralization.degree(MELB17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB17_QTftn <- as.network.matrix(MELB17_QTft)
MELB17_QT.netDensity <- network.density(MELB17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB17_QT.entropy <- entropy(MELB17_QTft) #entropy

MELB17_QT.netMx <- cbind(MELB17_QT.netMx, MELB17_QT.clusterCoef, MELB17_QT.degreeCent$centralization,
                         MELB17_QT.netDensity, MELB17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB17_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************

round = 17
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
NMFC17_Gg2 <- data.frame(NMFC17_G)
NMFC17_Gg2 <- NMFC17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_Gg2$player1
player2vector <- NMFC17_Gg2$player2
NMFC17_Gg3 <- NMFC17_Gg2
NMFC17_Gg3$p1inp2vec <- is.element(NMFC17_Gg3$player1, player2vector)
NMFC17_Gg3$p2inp1vec <- is.element(NMFC17_Gg3$player2, player1vector)

addPlayer1 <- NMFC17_Gg3[ which(NMFC17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_Gg3[ which(NMFC17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_Gg2 <- rbind(NMFC17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
NMFC17_Gft <- ftable(NMFC17_Gg2$player1, NMFC17_Gg2$player2)
NMFC17_Gft2 <- as.matrix(NMFC17_Gft)
numRows <- nrow(NMFC17_Gft2)
numCols <- ncol(NMFC17_Gft2)
NMFC17_Gft3 <- NMFC17_Gft2[c(2:numRows) , c(2:numCols)]
NMFC17_GTable <- graph.adjacency(NMFC17_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(NMFC17_GTable, vertex.label = V(NMFC17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
NMFC17_G.clusterCoef <- transitivity(NMFC17_GTable, type="global") #cluster coefficient
NMFC17_G.degreeCent <- centralization.degree(NMFC17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_Gftn <- as.network.matrix(NMFC17_Gft)
NMFC17_G.netDensity <- network.density(NMFC17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_G.entropy <- entropy(NMFC17_Gft) #entropy

NMFC17_G.netMx <- cbind(NMFC17_G.netMx, NMFC17_G.clusterCoef, NMFC17_G.degreeCent$centralization,
                        NMFC17_G.netDensity, NMFC17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************

round = 17
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
NMFC17_Bg2 <- data.frame(NMFC17_B)
NMFC17_Bg2 <- NMFC17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_Bg2$player1
player2vector <- NMFC17_Bg2$player2
NMFC17_Bg3 <- NMFC17_Bg2
NMFC17_Bg3$p1inp2vec <- is.element(NMFC17_Bg3$player1, player2vector)
NMFC17_Bg3$p2inp1vec <- is.element(NMFC17_Bg3$player2, player1vector)

addPlayer1 <- NMFC17_Bg3[ which(NMFC17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC17_Bg3[ which(NMFC17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_Bg2 <- rbind(NMFC17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
NMFC17_Bft <- ftable(NMFC17_Bg2$player1, NMFC17_Bg2$player2)
NMFC17_Bft2 <- as.matrix(NMFC17_Bft)
numRows <- nrow(NMFC17_Bft2)
numCols <- ncol(NMFC17_Bft2)
NMFC17_Bft3 <- NMFC17_Bft2[c(2:numRows) , c(2:numCols)]
NMFC17_BTable <- graph.adjacency(NMFC17_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(NMFC17_BTable, vertex.label = V(NMFC17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
NMFC17_B.clusterCoef <- transitivity(NMFC17_BTable, type="global") #cluster coefficient
NMFC17_B.degreeCent <- centralization.degree(NMFC17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_Bftn <- as.network.matrix(NMFC17_Bft)
NMFC17_B.netDensity <- network.density(NMFC17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_B.entropy <- entropy(NMFC17_Bft) #entropy

NMFC17_B.netMx <- cbind(NMFC17_B.netMx, NMFC17_B.clusterCoef, NMFC17_B.degreeCent$centralization,
                        NMFC17_B.netDensity, NMFC17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
NMFC17_SFg2 <- data.frame(NMFC17_SF)
NMFC17_SFg2 <- NMFC17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_SFg2$player1
player2vector <- NMFC17_SFg2$player2
NMFC17_SFg3 <- NMFC17_SFg2
NMFC17_SFg3$p1inp2vec <- is.element(NMFC17_SFg3$player1, player2vector)
NMFC17_SFg3$p2inp1vec <- is.element(NMFC17_SFg3$player2, player1vector)

addPlayer1 <- NMFC17_SFg3[ which(NMFC17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_SFg3[ which(NMFC17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_SFg2 <- rbind(NMFC17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
NMFC17_SFft <- ftable(NMFC17_SFg2$player1, NMFC17_SFg2$player2)
NMFC17_SFft2 <- as.matrix(NMFC17_SFft)
numRows <- nrow(NMFC17_SFft2)
numCols <- ncol(NMFC17_SFft2)
NMFC17_SFft3 <- NMFC17_SFft2[c(2:numRows) , c(2:numCols)]
NMFC17_SFTable <- graph.adjacency(NMFC17_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(NMFC17_SFTable, vertex.label = V(NMFC17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
NMFC17_SF.clusterCoef <- transitivity(NMFC17_SFTable, type="global") #cluster coefficient
NMFC17_SF.degreeCent <- centralization.degree(NMFC17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_SFftn <- as.network.matrix(NMFC17_SFft)
NMFC17_SF.netDensity <- network.density(NMFC17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_SF.entropy <- entropy(NMFC17_SFft) #entropy

NMFC17_SF.netMx <- cbind(NMFC17_SF.netMx, NMFC17_SF.clusterCoef, NMFC17_SF.degreeCent$centralization,
                         NMFC17_SF.netDensity, NMFC17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
NMFC17_TFg2 <- data.frame(NMFC17_TF)
NMFC17_TFg2 <- NMFC17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_TFg2$player1
player2vector <- NMFC17_TFg2$player2
NMFC17_TFg3 <- NMFC17_TFg2
NMFC17_TFg3$p1inp2vec <- is.element(NMFC17_TFg3$player1, player2vector)
NMFC17_TFg3$p2inp1vec <- is.element(NMFC17_TFg3$player2, player1vector)

addPlayer1 <- NMFC17_TFg3[ which(NMFC17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_TFg3[ which(NMFC17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_TFg2 <- rbind(NMFC17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
NMFC17_TFft <- ftable(NMFC17_TFg2$player1, NMFC17_TFg2$player2)
NMFC17_TFft2 <- as.matrix(NMFC17_TFft)
numRows <- nrow(NMFC17_TFft2)
numCols <- ncol(NMFC17_TFft2)
NMFC17_TFft3 <- NMFC17_TFft2[c(2:numRows) , c(2:numCols)]
NMFC17_TFTable <- graph.adjacency(NMFC17_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(NMFC17_TFTable, vertex.label = V(NMFC17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
NMFC17_TF.clusterCoef <- transitivity(NMFC17_TFTable, type="global") #cluster coefficient
NMFC17_TF.degreeCent <- centralization.degree(NMFC17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_TFftn <- as.network.matrix(NMFC17_TFft)
NMFC17_TF.netDensity <- network.density(NMFC17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_TF.entropy <- entropy(NMFC17_TFft) #entropy

NMFC17_TF.netMx <- cbind(NMFC17_TF.netMx, NMFC17_TF.clusterCoef, NMFC17_TF.degreeCent$centralization,
                         NMFC17_TF.netDensity, NMFC17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************

round = 17
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
NMFC17_SAMg2 <- data.frame(NMFC17_SAM)
NMFC17_SAMg2 <- NMFC17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_SAMg2$player1
player2vector <- NMFC17_SAMg2$player2
NMFC17_SAMg3 <- NMFC17_SAMg2
NMFC17_SAMg3$p1inp2vec <- is.element(NMFC17_SAMg3$player1, player2vector)
NMFC17_SAMg3$p2inp1vec <- is.element(NMFC17_SAMg3$player2, player1vector)

addPlayer1 <- NMFC17_SAMg3[ which(NMFC17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_SAMg3[ which(NMFC17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_SAMg2 <- rbind(NMFC17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
NMFC17_SAMft <- ftable(NMFC17_SAMg2$player1, NMFC17_SAMg2$player2)
NMFC17_SAMft2 <- as.matrix(NMFC17_SAMft)
numRows <- nrow(NMFC17_SAMft2)
numCols <- ncol(NMFC17_SAMft2)
NMFC17_SAMft3 <- NMFC17_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC17_SAMTable <- graph.adjacency(NMFC17_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(NMFC17_SAMTable, vertex.label = V(NMFC17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
NMFC17_SAM.clusterCoef <- transitivity(NMFC17_SAMTable, type="global") #cluster coefficient
NMFC17_SAM.degreeCent <- centralization.degree(NMFC17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_SAMftn <- as.network.matrix(NMFC17_SAMft)
NMFC17_SAM.netDensity <- network.density(NMFC17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_SAM.entropy <- entropy(NMFC17_SAMft) #entropy

NMFC17_SAM.netMx <- cbind(NMFC17_SAM.netMx, NMFC17_SAM.clusterCoef, NMFC17_SAM.degreeCent$centralization,
                          NMFC17_SAM.netDensity, NMFC17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************
#NA

round = 17
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
NMFC17_TAMg2 <- data.frame(NMFC17_TAM)
NMFC17_TAMg2 <- NMFC17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_TAMg2$player1
player2vector <- NMFC17_TAMg2$player2
NMFC17_TAMg3 <- NMFC17_TAMg2
NMFC17_TAMg3$p1inp2vec <- is.element(NMFC17_TAMg3$player1, player2vector)
NMFC17_TAMg3$p2inp1vec <- is.element(NMFC17_TAMg3$player2, player1vector)

addPlayer1 <- NMFC17_TAMg3[ which(NMFC17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_TAMg3[ which(NMFC17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_TAMg2 <- rbind(NMFC17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
NMFC17_TAMft <- ftable(NMFC17_TAMg2$player1, NMFC17_TAMg2$player2)
NMFC17_TAMft2 <- as.matrix(NMFC17_TAMft)
numRows <- nrow(NMFC17_TAMft2)
numCols <- ncol(NMFC17_TAMft2)
NMFC17_TAMft3 <- NMFC17_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC17_TAMTable <- graph.adjacency(NMFC17_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(NMFC17_TAMTable, vertex.label = V(NMFC17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
NMFC17_TAM.clusterCoef <- transitivity(NMFC17_TAMTable, type="global") #cluster coefficient
NMFC17_TAM.degreeCent <- centralization.degree(NMFC17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_TAMftn <- as.network.matrix(NMFC17_TAMft)
NMFC17_TAM.netDensity <- network.density(NMFC17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_TAM.entropy <- entropy(NMFC17_TAMft) #entropy

NMFC17_TAM.netMx <- cbind(NMFC17_TAM.netMx, NMFC17_TAM.clusterCoef, NMFC17_TAM.degreeCent$centralization,
                          NMFC17_TAM.netDensity, NMFC17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************
#NA

round = 17
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
NMFC17_SDMg2 <- data.frame(NMFC17_SDM)
NMFC17_SDMg2 <- NMFC17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_SDMg2$player1
player2vector <- NMFC17_SDMg2$player2
NMFC17_SDMg3 <- NMFC17_SDMg2
NMFC17_SDMg3$p1inp2vec <- is.element(NMFC17_SDMg3$player1, player2vector)
NMFC17_SDMg3$p2inp1vec <- is.element(NMFC17_SDMg3$player2, player1vector)

addPlayer1 <- NMFC17_SDMg3[ which(NMFC17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_SDMg3[ which(NMFC17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_SDMg2 <- rbind(NMFC17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
NMFC17_SDMft <- ftable(NMFC17_SDMg2$player1, NMFC17_SDMg2$player2)
NMFC17_SDMft2 <- as.matrix(NMFC17_SDMft)
numRows <- nrow(NMFC17_SDMft2)
numCols <- ncol(NMFC17_SDMft2)
NMFC17_SDMft3 <- NMFC17_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC17_SDMTable <- graph.adjacency(NMFC17_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(NMFC17_SDMTable, vertex.label = V(NMFC17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
NMFC17_SDM.clusterCoef <- transitivity(NMFC17_SDMTable, type="global") #cluster coefficient
NMFC17_SDM.degreeCent <- centralization.degree(NMFC17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_SDMftn <- as.network.matrix(NMFC17_SDMft)
NMFC17_SDM.netDensity <- network.density(NMFC17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_SDM.entropy <- entropy(NMFC17_SDMft) #entropy

NMFC17_SDM.netMx <- cbind(NMFC17_SDM.netMx, NMFC17_SDM.clusterCoef, NMFC17_SDM.degreeCent$centralization,
                          NMFC17_SDM.netDensity, NMFC17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
NMFC17_TDMg2 <- data.frame(NMFC17_TDM)
NMFC17_TDMg2 <- NMFC17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_TDMg2$player1
player2vector <- NMFC17_TDMg2$player2
NMFC17_TDMg3 <- NMFC17_TDMg2
NMFC17_TDMg3$p1inp2vec <- is.element(NMFC17_TDMg3$player1, player2vector)
NMFC17_TDMg3$p2inp1vec <- is.element(NMFC17_TDMg3$player2, player1vector)

addPlayer1 <- NMFC17_TDMg3[ which(NMFC17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_TDMg3[ which(NMFC17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_TDMg2 <- rbind(NMFC17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
NMFC17_TDMft <- ftable(NMFC17_TDMg2$player1, NMFC17_TDMg2$player2)
NMFC17_TDMft2 <- as.matrix(NMFC17_TDMft)
numRows <- nrow(NMFC17_TDMft2)
numCols <- ncol(NMFC17_TDMft2)
NMFC17_TDMft3 <- NMFC17_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC17_TDMTable <- graph.adjacency(NMFC17_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(NMFC17_TDMTable, vertex.label = V(NMFC17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
NMFC17_TDM.clusterCoef <- transitivity(NMFC17_TDMTable, type="global") #cluster coefficient
NMFC17_TDM.degreeCent <- centralization.degree(NMFC17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_TDMftn <- as.network.matrix(NMFC17_TDMft)
NMFC17_TDM.netDensity <- network.density(NMFC17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_TDM.entropy <- entropy(NMFC17_TDMft) #entropy

NMFC17_TDM.netMx <- cbind(NMFC17_TDM.netMx, NMFC17_TDM.clusterCoef, NMFC17_TDM.degreeCent$centralization,
                          NMFC17_TDM.netDensity, NMFC17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
NMFC17_SDg2 <- data.frame(NMFC17_SD)
NMFC17_SDg2 <- NMFC17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_SDg2$player1
player2vector <- NMFC17_SDg2$player2
NMFC17_SDg3 <- NMFC17_SDg2
NMFC17_SDg3$p1inp2vec <- is.element(NMFC17_SDg3$player1, player2vector)
NMFC17_SDg3$p2inp1vec <- is.element(NMFC17_SDg3$player2, player1vector)

addPlayer1 <- NMFC17_SDg3[ which(NMFC17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_SDg3[ which(NMFC17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_SDg2 <- rbind(NMFC17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
NMFC17_SDft <- ftable(NMFC17_SDg2$player1, NMFC17_SDg2$player2)
NMFC17_SDft2 <- as.matrix(NMFC17_SDft)
numRows <- nrow(NMFC17_SDft2)
numCols <- ncol(NMFC17_SDft2)
NMFC17_SDft3 <- NMFC17_SDft2[c(2:numRows) , c(2:numCols)]
NMFC17_SDTable <- graph.adjacency(NMFC17_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(NMFC17_SDTable, vertex.label = V(NMFC17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
NMFC17_SD.clusterCoef <- transitivity(NMFC17_SDTable, type="global") #cluster coefficient
NMFC17_SD.degreeCent <- centralization.degree(NMFC17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_SDftn <- as.network.matrix(NMFC17_SDft)
NMFC17_SD.netDensity <- network.density(NMFC17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_SD.entropy <- entropy(NMFC17_SDft) #entropy

NMFC17_SD.netMx <- cbind(NMFC17_SD.netMx, NMFC17_SD.clusterCoef, NMFC17_SD.degreeCent$centralization,
                         NMFC17_SD.netDensity, NMFC17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
NMFC17_TDg2 <- data.frame(NMFC17_TD)
NMFC17_TDg2 <- NMFC17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_TDg2$player1
player2vector <- NMFC17_TDg2$player2
NMFC17_TDg3 <- NMFC17_TDg2
NMFC17_TDg3$p1inp2vec <- is.element(NMFC17_TDg3$player1, player2vector)
NMFC17_TDg3$p2inp1vec <- is.element(NMFC17_TDg3$player2, player1vector)

addPlayer1 <- NMFC17_TDg3[ which(NMFC17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_TDg3[ which(NMFC17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_TDg2 <- rbind(NMFC17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
NMFC17_TDft <- ftable(NMFC17_TDg2$player1, NMFC17_TDg2$player2)
NMFC17_TDft2 <- as.matrix(NMFC17_TDft)
numRows <- nrow(NMFC17_TDft2)
numCols <- ncol(NMFC17_TDft2)
NMFC17_TDft3 <- NMFC17_TDft2[c(2:numRows) , c(2:numCols)]
NMFC17_TDTable <- graph.adjacency(NMFC17_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(NMFC17_TDTable, vertex.label = V(NMFC17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
NMFC17_TD.clusterCoef <- transitivity(NMFC17_TDTable, type="global") #cluster coefficient
NMFC17_TD.degreeCent <- centralization.degree(NMFC17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_TDftn <- as.network.matrix(NMFC17_TDft)
NMFC17_TD.netDensity <- network.density(NMFC17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_TD.entropy <- entropy(NMFC17_TDft) #entropy

NMFC17_TD.netMx <- cbind(NMFC17_TD.netMx, NMFC17_TD.clusterCoef, NMFC17_TD.degreeCent$centralization,
                         NMFC17_TD.netDensity, NMFC17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
NMFC17_QTg2 <- data.frame(NMFC17_QT)
NMFC17_QTg2 <- NMFC17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC17_QTg2$player1
player2vector <- NMFC17_QTg2$player2
NMFC17_QTg3 <- NMFC17_QTg2
NMFC17_QTg3$p1inp2vec <- is.element(NMFC17_QTg3$player1, player2vector)
NMFC17_QTg3$p2inp1vec <- is.element(NMFC17_QTg3$player2, player1vector)

addPlayer1 <- NMFC17_QTg3[ which(NMFC17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC17_QTg3[ which(NMFC17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC17_QTg2 <- rbind(NMFC17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
NMFC17_QTft <- ftable(NMFC17_QTg2$player1, NMFC17_QTg2$player2)
NMFC17_QTft2 <- as.matrix(NMFC17_QTft)
numRows <- nrow(NMFC17_QTft2)
numCols <- ncol(NMFC17_QTft2)
NMFC17_QTft3 <- NMFC17_QTft2[c(2:numRows) , c(2:numCols)]
NMFC17_QTTable <- graph.adjacency(NMFC17_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(NMFC17_QTTable, vertex.label = V(NMFC17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
NMFC17_QT.clusterCoef <- transitivity(NMFC17_QTTable, type="global") #cluster coefficient
NMFC17_QT.degreeCent <- centralization.degree(NMFC17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC17_QTftn <- as.network.matrix(NMFC17_QTft)
NMFC17_QT.netDensity <- network.density(NMFC17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC17_QT.entropy <- entropy(NMFC17_QTft) #entropy

NMFC17_QT.netMx <- cbind(NMFC17_QT.netMx, NMFC17_QT.clusterCoef, NMFC17_QT.degreeCent$centralization,
                         NMFC17_QT.netDensity, NMFC17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC17_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************

round = 17
teamName = "PORT"
KIoutcome = "Goal_F"
PORT17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
PORT17_Gg2 <- data.frame(PORT17_G)
PORT17_Gg2 <- PORT17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_Gg2$player1
player2vector <- PORT17_Gg2$player2
PORT17_Gg3 <- PORT17_Gg2
PORT17_Gg3$p1inp2vec <- is.element(PORT17_Gg3$player1, player2vector)
PORT17_Gg3$p2inp1vec <- is.element(PORT17_Gg3$player2, player1vector)

addPlayer1 <- PORT17_Gg3[ which(PORT17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT17_Gg3[ which(PORT17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_Gg2 <- rbind(PORT17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
PORT17_Gft <- ftable(PORT17_Gg2$player1, PORT17_Gg2$player2)
PORT17_Gft2 <- as.matrix(PORT17_Gft)
numRows <- nrow(PORT17_Gft2)
numCols <- ncol(PORT17_Gft2)
PORT17_Gft3 <- PORT17_Gft2[c(2:numRows) , c(2:numCols)]
PORT17_GTable <- graph.adjacency(PORT17_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(PORT17_GTable, vertex.label = V(PORT17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
PORT17_G.clusterCoef <- transitivity(PORT17_GTable, type="global") #cluster coefficient
PORT17_G.degreeCent <- centralization.degree(PORT17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_Gftn <- as.network.matrix(PORT17_Gft)
PORT17_G.netDensity <- network.density(PORT17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_G.entropy <- entropy(PORT17_Gft) #entropy

PORT17_G.netMx <- cbind(PORT17_G.netMx, PORT17_G.clusterCoef, PORT17_G.degreeCent$centralization,
                        PORT17_G.netDensity, PORT17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************

round = 17
teamName = "PORT"
KIoutcome = "Behind_F"
PORT17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
PORT17_Bg2 <- data.frame(PORT17_B)
PORT17_Bg2 <- PORT17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_Bg2$player1
player2vector <- PORT17_Bg2$player2
PORT17_Bg3 <- PORT17_Bg2
PORT17_Bg3$p1inp2vec <- is.element(PORT17_Bg3$player1, player2vector)
PORT17_Bg3$p2inp1vec <- is.element(PORT17_Bg3$player2, player1vector)

addPlayer1 <- PORT17_Bg3[ which(PORT17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT17_Bg3[ which(PORT17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_Bg2 <- rbind(PORT17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
PORT17_Bft <- ftable(PORT17_Bg2$player1, PORT17_Bg2$player2)
PORT17_Bft2 <- as.matrix(PORT17_Bft)
numRows <- nrow(PORT17_Bft2)
numCols <- ncol(PORT17_Bft2)
PORT17_Bft3 <- PORT17_Bft2[c(2:numRows) , c(2:numCols)]
PORT17_BTable <- graph.adjacency(PORT17_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(PORT17_BTable, vertex.label = V(PORT17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
PORT17_B.clusterCoef <- transitivity(PORT17_BTable, type="global") #cluster coefficient
PORT17_B.degreeCent <- centralization.degree(PORT17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_Bftn <- as.network.matrix(PORT17_Bft)
PORT17_B.netDensity <- network.density(PORT17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_B.entropy <- entropy(PORT17_Bft) #entropy

PORT17_B.netMx <- cbind(PORT17_B.netMx, PORT17_B.clusterCoef, PORT17_B.degreeCent$centralization,
                        PORT17_B.netDensity, PORT17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
PORT17_SFg2 <- data.frame(PORT17_SF)
PORT17_SFg2 <- PORT17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_SFg2$player1
player2vector <- PORT17_SFg2$player2
PORT17_SFg3 <- PORT17_SFg2
PORT17_SFg3$p1inp2vec <- is.element(PORT17_SFg3$player1, player2vector)
PORT17_SFg3$p2inp1vec <- is.element(PORT17_SFg3$player2, player1vector)

addPlayer1 <- PORT17_SFg3[ which(PORT17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT17_SFg3[ which(PORT17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_SFg2 <- rbind(PORT17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
PORT17_SFft <- ftable(PORT17_SFg2$player1, PORT17_SFg2$player2)
PORT17_SFft2 <- as.matrix(PORT17_SFft)
numRows <- nrow(PORT17_SFft2)
numCols <- ncol(PORT17_SFft2)
PORT17_SFft3 <- PORT17_SFft2[c(2:numRows) , c(2:numCols)]
PORT17_SFTable <- graph.adjacency(PORT17_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(PORT17_SFTable, vertex.label = V(PORT17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
PORT17_SF.clusterCoef <- transitivity(PORT17_SFTable, type="global") #cluster coefficient
PORT17_SF.degreeCent <- centralization.degree(PORT17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_SFftn <- as.network.matrix(PORT17_SFft)
PORT17_SF.netDensity <- network.density(PORT17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_SF.entropy <- entropy(PORT17_SFft) #entropy

PORT17_SF.netMx <- cbind(PORT17_SF.netMx, PORT17_SF.clusterCoef, PORT17_SF.degreeCent$centralization,
                         PORT17_SF.netDensity, PORT17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************

round = 17
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
PORT17_TFg2 <- data.frame(PORT17_TF)
PORT17_TFg2 <- PORT17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_TFg2$player1
player2vector <- PORT17_TFg2$player2
PORT17_TFg3 <- PORT17_TFg2
PORT17_TFg3$p1inp2vec <- is.element(PORT17_TFg3$player1, player2vector)
PORT17_TFg3$p2inp1vec <- is.element(PORT17_TFg3$player2, player1vector)

addPlayer1 <- PORT17_TFg3[ which(PORT17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT17_TFg3[ which(PORT17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_TFg2 <- rbind(PORT17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
PORT17_TFft <- ftable(PORT17_TFg2$player1, PORT17_TFg2$player2)
PORT17_TFft2 <- as.matrix(PORT17_TFft)
numRows <- nrow(PORT17_TFft2)
numCols <- ncol(PORT17_TFft2)
PORT17_TFft3 <- PORT17_TFft2[c(2:numRows) , c(2:numCols)]
PORT17_TFTable <- graph.adjacency(PORT17_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(PORT17_TFTable, vertex.label = V(PORT17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
PORT17_TF.clusterCoef <- transitivity(PORT17_TFTable, type="global") #cluster coefficient
PORT17_TF.degreeCent <- centralization.degree(PORT17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_TFftn <- as.network.matrix(PORT17_TFft)
PORT17_TF.netDensity <- network.density(PORT17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_TF.entropy <- entropy(PORT17_TFft) #entropy

PORT17_TF.netMx <- cbind(PORT17_TF.netMx, PORT17_TF.clusterCoef, PORT17_TF.degreeCent$centralization,
                         PORT17_TF.netDensity, PORT17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************
#NA

round = 17
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
PORT17_SAMg2 <- data.frame(PORT17_SAM)
PORT17_SAMg2 <- PORT17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_SAMg2$player1
player2vector <- PORT17_SAMg2$player2
PORT17_SAMg3 <- PORT17_SAMg2
PORT17_SAMg3$p1inp2vec <- is.element(PORT17_SAMg3$player1, player2vector)
PORT17_SAMg3$p2inp1vec <- is.element(PORT17_SAMg3$player2, player1vector)

addPlayer1 <- PORT17_SAMg3[ which(PORT17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT17_SAMg3[ which(PORT17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_SAMg2 <- rbind(PORT17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
PORT17_SAMft <- ftable(PORT17_SAMg2$player1, PORT17_SAMg2$player2)
PORT17_SAMft2 <- as.matrix(PORT17_SAMft)
numRows <- nrow(PORT17_SAMft2)
numCols <- ncol(PORT17_SAMft2)
PORT17_SAMft3 <- PORT17_SAMft2[c(2:numRows) , c(2:numCols)]
PORT17_SAMTable <- graph.adjacency(PORT17_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(PORT17_SAMTable, vertex.label = V(PORT17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
PORT17_SAM.clusterCoef <- transitivity(PORT17_SAMTable, type="global") #cluster coefficient
PORT17_SAM.degreeCent <- centralization.degree(PORT17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_SAMftn <- as.network.matrix(PORT17_SAMft)
PORT17_SAM.netDensity <- network.density(PORT17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_SAM.entropy <- entropy(PORT17_SAMft) #entropy

PORT17_SAM.netMx <- cbind(PORT17_SAM.netMx, PORT17_SAM.clusterCoef, PORT17_SAM.degreeCent$centralization,
                          PORT17_SAM.netDensity, PORT17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************
#NA

round = 17
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
PORT17_TAMg2 <- data.frame(PORT17_TAM)
PORT17_TAMg2 <- PORT17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_TAMg2$player1
player2vector <- PORT17_TAMg2$player2
PORT17_TAMg3 <- PORT17_TAMg2
PORT17_TAMg3$p1inp2vec <- is.element(PORT17_TAMg3$player1, player2vector)
PORT17_TAMg3$p2inp1vec <- is.element(PORT17_TAMg3$player2, player1vector)

addPlayer1 <- PORT17_TAMg3[ which(PORT17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT17_TAMg2 <- rbind(PORT17_TAMg2, addPlayer1)

#ROUND 17, AM Turnover graph using weighted edges
PORT17_TAMft <- ftable(PORT17_TAMg2$player1, PORT17_TAMg2$player2)
PORT17_TAMft2 <- as.matrix(PORT17_TAMft)
numRows <- nrow(PORT17_TAMft2)
numCols <- ncol(PORT17_TAMft2)
PORT17_TAMft3 <- PORT17_TAMft2[c(2:numRows) , c(1:numCols)]
PORT17_TAMTable <- graph.adjacency(PORT17_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(PORT17_TAMTable, vertex.label = V(PORT17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
PORT17_TAM.clusterCoef <- transitivity(PORT17_TAMTable, type="global") #cluster coefficient
PORT17_TAM.degreeCent <- centralization.degree(PORT17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_TAMftn <- as.network.matrix(PORT17_TAMft)
PORT17_TAM.netDensity <- network.density(PORT17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_TAM.entropy <- entropy(PORT17_TAMft) #entropy

PORT17_TAM.netMx <- cbind(PORT17_TAM.netMx, PORT17_TAM.clusterCoef, PORT17_TAM.degreeCent$centralization,
                          PORT17_TAM.netDensity, PORT17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************

round = 17
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
PORT17_SDMg2 <- data.frame(PORT17_SDM)
PORT17_SDMg2 <- PORT17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_SDMg2$player1
player2vector <- PORT17_SDMg2$player2
PORT17_SDMg3 <- PORT17_SDMg2
PORT17_SDMg3$p1inp2vec <- is.element(PORT17_SDMg3$player1, player2vector)
PORT17_SDMg3$p2inp1vec <- is.element(PORT17_SDMg3$player2, player1vector)

addPlayer1 <- PORT17_SDMg3[ which(PORT17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- PORT17_SDMg3[ which(PORT17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_SDMg2 <- rbind(PORT17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
PORT17_SDMft <- ftable(PORT17_SDMg2$player1, PORT17_SDMg2$player2)
PORT17_SDMft2 <- as.matrix(PORT17_SDMft)
numRows <- nrow(PORT17_SDMft2)
numCols <- ncol(PORT17_SDMft2)
PORT17_SDMft3 <- PORT17_SDMft2[c(2:numRows) , c(2:numCols)]
PORT17_SDMTable <- graph.adjacency(PORT17_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(PORT17_SDMTable, vertex.label = V(PORT17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
PORT17_SDM.clusterCoef <- transitivity(PORT17_SDMTable, type="global") #cluster coefficient
PORT17_SDM.degreeCent <- centralization.degree(PORT17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_SDMftn <- as.network.matrix(PORT17_SDMft)
PORT17_SDM.netDensity <- network.density(PORT17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_SDM.entropy <- entropy(PORT17_SDMft) #entropy

PORT17_SDM.netMx <- cbind(PORT17_SDM.netMx, PORT17_SDM.clusterCoef, PORT17_SDM.degreeCent$centralization,
                          PORT17_SDM.netDensity, PORT17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
PORT17_TDMg2 <- data.frame(PORT17_TDM)
PORT17_TDMg2 <- PORT17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_TDMg2$player1
player2vector <- PORT17_TDMg2$player2
PORT17_TDMg3 <- PORT17_TDMg2
PORT17_TDMg3$p1inp2vec <- is.element(PORT17_TDMg3$player1, player2vector)
PORT17_TDMg3$p2inp1vec <- is.element(PORT17_TDMg3$player2, player1vector)

addPlayer1 <- PORT17_TDMg3[ which(PORT17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT17_TDMg3[ which(PORT17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_TDMg2 <- rbind(PORT17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
PORT17_TDMft <- ftable(PORT17_TDMg2$player1, PORT17_TDMg2$player2)
PORT17_TDMft2 <- as.matrix(PORT17_TDMft)
numRows <- nrow(PORT17_TDMft2)
numCols <- ncol(PORT17_TDMft2)
PORT17_TDMft3 <- PORT17_TDMft2[c(2:numRows) , c(2:numCols)]
PORT17_TDMTable <- graph.adjacency(PORT17_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(PORT17_TDMTable, vertex.label = V(PORT17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
PORT17_TDM.clusterCoef <- transitivity(PORT17_TDMTable, type="global") #cluster coefficient
PORT17_TDM.degreeCent <- centralization.degree(PORT17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_TDMftn <- as.network.matrix(PORT17_TDMft)
PORT17_TDM.netDensity <- network.density(PORT17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_TDM.entropy <- entropy(PORT17_TDMft) #entropy

PORT17_TDM.netMx <- cbind(PORT17_TDM.netMx, PORT17_TDM.clusterCoef, PORT17_TDM.degreeCent$centralization,
                          PORT17_TDM.netDensity, PORT17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
PORT17_SDg2 <- data.frame(PORT17_SD)
PORT17_SDg2 <- PORT17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_SDg2$player1
player2vector <- PORT17_SDg2$player2
PORT17_SDg3 <- PORT17_SDg2
PORT17_SDg3$p1inp2vec <- is.element(PORT17_SDg3$player1, player2vector)
PORT17_SDg3$p2inp1vec <- is.element(PORT17_SDg3$player2, player1vector)

addPlayer1 <- PORT17_SDg3[ which(PORT17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT17_SDg3[ which(PORT17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_SDg2 <- rbind(PORT17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
PORT17_SDft <- ftable(PORT17_SDg2$player1, PORT17_SDg2$player2)
PORT17_SDft2 <- as.matrix(PORT17_SDft)
numRows <- nrow(PORT17_SDft2)
numCols <- ncol(PORT17_SDft2)
PORT17_SDft3 <- PORT17_SDft2[c(2:numRows) , c(2:numCols)]
PORT17_SDTable <- graph.adjacency(PORT17_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(PORT17_SDTable, vertex.label = V(PORT17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
PORT17_SD.clusterCoef <- transitivity(PORT17_SDTable, type="global") #cluster coefficient
PORT17_SD.degreeCent <- centralization.degree(PORT17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_SDftn <- as.network.matrix(PORT17_SDft)
PORT17_SD.netDensity <- network.density(PORT17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_SD.entropy <- entropy(PORT17_SDft) #entropy

PORT17_SD.netMx <- cbind(PORT17_SD.netMx, PORT17_SD.clusterCoef, PORT17_SD.degreeCent$centralization,
                         PORT17_SD.netDensity, PORT17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************

round = 17
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
PORT17_TDg2 <- data.frame(PORT17_TD)
PORT17_TDg2 <- PORT17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_TDg2$player1
player2vector <- PORT17_TDg2$player2
PORT17_TDg3 <- PORT17_TDg2
PORT17_TDg3$p1inp2vec <- is.element(PORT17_TDg3$player1, player2vector)
PORT17_TDg3$p2inp1vec <- is.element(PORT17_TDg3$player2, player1vector)

addPlayer1 <- PORT17_TDg3[ which(PORT17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT17_TDg3[ which(PORT17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_TDg2 <- rbind(PORT17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
PORT17_TDft <- ftable(PORT17_TDg2$player1, PORT17_TDg2$player2)
PORT17_TDft2 <- as.matrix(PORT17_TDft)
numRows <- nrow(PORT17_TDft2)
numCols <- ncol(PORT17_TDft2)
PORT17_TDft3 <- PORT17_TDft2[c(2:numRows) , c(2:numCols)]
PORT17_TDTable <- graph.adjacency(PORT17_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(PORT17_TDTable, vertex.label = V(PORT17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
PORT17_TD.clusterCoef <- transitivity(PORT17_TDTable, type="global") #cluster coefficient
PORT17_TD.degreeCent <- centralization.degree(PORT17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_TDftn <- as.network.matrix(PORT17_TDft)
PORT17_TD.netDensity <- network.density(PORT17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_TD.entropy <- entropy(PORT17_TDft) #entropy

PORT17_TD.netMx <- cbind(PORT17_TD.netMx, PORT17_TD.clusterCoef, PORT17_TD.degreeCent$centralization,
                         PORT17_TD.netDensity, PORT17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
PORT17_QTg2 <- data.frame(PORT17_QT)
PORT17_QTg2 <- PORT17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT17_QTg2$player1
player2vector <- PORT17_QTg2$player2
PORT17_QTg3 <- PORT17_QTg2
PORT17_QTg3$p1inp2vec <- is.element(PORT17_QTg3$player1, player2vector)
PORT17_QTg3$p2inp1vec <- is.element(PORT17_QTg3$player2, player1vector)

addPlayer1 <- PORT17_QTg3[ which(PORT17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT17_QTg3[ which(PORT17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT17_QTg2 <- rbind(PORT17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
PORT17_QTft <- ftable(PORT17_QTg2$player1, PORT17_QTg2$player2)
PORT17_QTft2 <- as.matrix(PORT17_QTft)
numRows <- nrow(PORT17_QTft2)
numCols <- ncol(PORT17_QTft2)
PORT17_QTft3 <- PORT17_QTft2[c(2:numRows) , c(2:numCols)]
PORT17_QTTable <- graph.adjacency(PORT17_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(PORT17_QTTable, vertex.label = V(PORT17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
PORT17_QT.clusterCoef <- transitivity(PORT17_QTTable, type="global") #cluster coefficient
PORT17_QT.degreeCent <- centralization.degree(PORT17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT17_QTftn <- as.network.matrix(PORT17_QTft)
PORT17_QT.netDensity <- network.density(PORT17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT17_QT.entropy <- entropy(PORT17_QTft) #entropy

PORT17_QT.netMx <- cbind(PORT17_QT.netMx, PORT17_QT.clusterCoef, PORT17_QT.degreeCent$centralization,
                         PORT17_QT.netDensity, PORT17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT17_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************
#NA

round = 17
teamName = "RICH"
KIoutcome = "Goal_F"
RICH17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
RICH17_Gg2 <- data.frame(RICH17_G)
RICH17_Gg2 <- RICH17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_Gg2$player1
player2vector <- RICH17_Gg2$player2
RICH17_Gg3 <- RICH17_Gg2
RICH17_Gg3$p1inp2vec <- is.element(RICH17_Gg3$player1, player2vector)
RICH17_Gg3$p2inp1vec <- is.element(RICH17_Gg3$player2, player1vector)

addPlayer1 <- RICH17_Gg3[ which(RICH17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_Gg3[ which(RICH17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_Gg2 <- rbind(RICH17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
RICH17_Gft <- ftable(RICH17_Gg2$player1, RICH17_Gg2$player2)
RICH17_Gft2 <- as.matrix(RICH17_Gft)
numRows <- nrow(RICH17_Gft2)
numCols <- ncol(RICH17_Gft2)
RICH17_Gft3 <- RICH17_Gft2[c(2:numRows) , c(2:numCols)]
RICH17_GTable <- graph.adjacency(RICH17_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(RICH17_GTable, vertex.label = V(RICH17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
RICH17_G.clusterCoef <- transitivity(RICH17_GTable, type="global") #cluster coefficient
RICH17_G.degreeCent <- centralization.degree(RICH17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_Gftn <- as.network.matrix(RICH17_Gft)
RICH17_G.netDensity <- network.density(RICH17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_G.entropy <- entropy(RICH17_Gft) #entropy

RICH17_G.netMx <- cbind(RICH17_G.netMx, RICH17_G.clusterCoef, RICH17_G.degreeCent$centralization,
                        RICH17_G.netDensity, RICH17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "RICH"
KIoutcome = "Behind_F"
RICH17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
RICH17_Bg2 <- data.frame(RICH17_B)
RICH17_Bg2 <- RICH17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_Bg2$player1
player2vector <- RICH17_Bg2$player2
RICH17_Bg3 <- RICH17_Bg2
RICH17_Bg3$p1inp2vec <- is.element(RICH17_Bg3$player1, player2vector)
RICH17_Bg3$p2inp1vec <- is.element(RICH17_Bg3$player2, player1vector)

addPlayer1 <- RICH17_Bg3[ which(RICH17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_Bg3[ which(RICH17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_Bg2 <- rbind(RICH17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
RICH17_Bft <- ftable(RICH17_Bg2$player1, RICH17_Bg2$player2)
RICH17_Bft2 <- as.matrix(RICH17_Bft)
numRows <- nrow(RICH17_Bft2)
numCols <- ncol(RICH17_Bft2)
RICH17_Bft3 <- RICH17_Bft2[c(2:numRows) , c(2:numCols)]
RICH17_BTable <- graph.adjacency(RICH17_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(RICH17_BTable, vertex.label = V(RICH17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
RICH17_B.clusterCoef <- transitivity(RICH17_BTable, type="global") #cluster coefficient
RICH17_B.degreeCent <- centralization.degree(RICH17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_Bftn <- as.network.matrix(RICH17_Bft)
RICH17_B.netDensity <- network.density(RICH17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_B.entropy <- entropy(RICH17_Bft) #entropy

RICH17_B.netMx <- cbind(RICH17_B.netMx, RICH17_B.clusterCoef, RICH17_B.degreeCent$centralization,
                        RICH17_B.netDensity, RICH17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
RICH17_SFg2 <- data.frame(RICH17_SF)
RICH17_SFg2 <- RICH17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_SFg2$player1
player2vector <- RICH17_SFg2$player2
RICH17_SFg3 <- RICH17_SFg2
RICH17_SFg3$p1inp2vec <- is.element(RICH17_SFg3$player1, player2vector)
RICH17_SFg3$p2inp1vec <- is.element(RICH17_SFg3$player2, player1vector)

addPlayer1 <- RICH17_SFg3[ which(RICH17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_SFg3[ which(RICH17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_SFg2 <- rbind(RICH17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
RICH17_SFft <- ftable(RICH17_SFg2$player1, RICH17_SFg2$player2)
RICH17_SFft2 <- as.matrix(RICH17_SFft)
numRows <- nrow(RICH17_SFft2)
numCols <- ncol(RICH17_SFft2)
RICH17_SFft3 <- RICH17_SFft2[c(2:numRows) , c(2:numCols)]
RICH17_SFTable <- graph.adjacency(RICH17_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(RICH17_SFTable, vertex.label = V(RICH17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
RICH17_SF.clusterCoef <- transitivity(RICH17_SFTable, type="global") #cluster coefficient
RICH17_SF.degreeCent <- centralization.degree(RICH17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_SFftn <- as.network.matrix(RICH17_SFft)
RICH17_SF.netDensity <- network.density(RICH17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_SF.entropy <- entropy(RICH17_SFft) #entropy

RICH17_SF.netMx <- cbind(RICH17_SF.netMx, RICH17_SF.clusterCoef, RICH17_SF.degreeCent$centralization,
                         RICH17_SF.netDensity, RICH17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
RICH17_TFg2 <- data.frame(RICH17_TF)
RICH17_TFg2 <- RICH17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_TFg2$player1
player2vector <- RICH17_TFg2$player2
RICH17_TFg3 <- RICH17_TFg2
RICH17_TFg3$p1inp2vec <- is.element(RICH17_TFg3$player1, player2vector)
RICH17_TFg3$p2inp1vec <- is.element(RICH17_TFg3$player2, player1vector)

addPlayer1 <- RICH17_TFg3[ which(RICH17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_TFg3[ which(RICH17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_TFg2 <- rbind(RICH17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
RICH17_TFft <- ftable(RICH17_TFg2$player1, RICH17_TFg2$player2)
RICH17_TFft2 <- as.matrix(RICH17_TFft)
numRows <- nrow(RICH17_TFft2)
numCols <- ncol(RICH17_TFft2)
RICH17_TFft3 <- RICH17_TFft2[c(2:numRows) , c(2:numCols)]
RICH17_TFTable <- graph.adjacency(RICH17_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(RICH17_TFTable, vertex.label = V(RICH17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
RICH17_TF.clusterCoef <- transitivity(RICH17_TFTable, type="global") #cluster coefficient
RICH17_TF.degreeCent <- centralization.degree(RICH17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_TFftn <- as.network.matrix(RICH17_TFft)
RICH17_TF.netDensity <- network.density(RICH17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_TF.entropy <- entropy(RICH17_TFft) #entropy

RICH17_TF.netMx <- cbind(RICH17_TF.netMx, RICH17_TF.clusterCoef, RICH17_TF.degreeCent$centralization,
                         RICH17_TF.netDensity, RICH17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************
#NA

round = 17
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
RICH17_SAMg2 <- data.frame(RICH17_SAM)
RICH17_SAMg2 <- RICH17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_SAMg2$player1
player2vector <- RICH17_SAMg2$player2
RICH17_SAMg3 <- RICH17_SAMg2
RICH17_SAMg3$p1inp2vec <- is.element(RICH17_SAMg3$player1, player2vector)
RICH17_SAMg3$p2inp1vec <- is.element(RICH17_SAMg3$player2, player1vector)

addPlayer1 <- RICH17_SAMg3[ which(RICH17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_SAMg3[ which(RICH17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_SAMg2 <- rbind(RICH17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
RICH17_SAMft <- ftable(RICH17_SAMg2$player1, RICH17_SAMg2$player2)
RICH17_SAMft2 <- as.matrix(RICH17_SAMft)
numRows <- nrow(RICH17_SAMft2)
numCols <- ncol(RICH17_SAMft2)
RICH17_SAMft3 <- RICH17_SAMft2[c(2:numRows) , c(2:numCols)]
RICH17_SAMTable <- graph.adjacency(RICH17_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(RICH17_SAMTable, vertex.label = V(RICH17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
RICH17_SAM.clusterCoef <- transitivity(RICH17_SAMTable, type="global") #cluster coefficient
RICH17_SAM.degreeCent <- centralization.degree(RICH17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_SAMftn <- as.network.matrix(RICH17_SAMft)
RICH17_SAM.netDensity <- network.density(RICH17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_SAM.entropy <- entropy(RICH17_SAMft) #entropy

RICH17_SAM.netMx <- cbind(RICH17_SAM.netMx, RICH17_SAM.clusterCoef, RICH17_SAM.degreeCent$centralization,
                          RICH17_SAM.netDensity, RICH17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
RICH17_TAMg2 <- data.frame(RICH17_TAM)
RICH17_TAMg2 <- RICH17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_TAMg2$player1
player2vector <- RICH17_TAMg2$player2
RICH17_TAMg3 <- RICH17_TAMg2
RICH17_TAMg3$p1inp2vec <- is.element(RICH17_TAMg3$player1, player2vector)
RICH17_TAMg3$p2inp1vec <- is.element(RICH17_TAMg3$player2, player1vector)

addPlayer1 <- RICH17_TAMg3[ which(RICH17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_TAMg3[ which(RICH17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_TAMg2 <- rbind(RICH17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
RICH17_TAMft <- ftable(RICH17_TAMg2$player1, RICH17_TAMg2$player2)
RICH17_TAMft2 <- as.matrix(RICH17_TAMft)
numRows <- nrow(RICH17_TAMft2)
numCols <- ncol(RICH17_TAMft2)
RICH17_TAMft3 <- RICH17_TAMft2[c(2:numRows) , c(2:numCols)]
RICH17_TAMTable <- graph.adjacency(RICH17_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(RICH17_TAMTable, vertex.label = V(RICH17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
RICH17_TAM.clusterCoef <- transitivity(RICH17_TAMTable, type="global") #cluster coefficient
RICH17_TAM.degreeCent <- centralization.degree(RICH17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_TAMftn <- as.network.matrix(RICH17_TAMft)
RICH17_TAM.netDensity <- network.density(RICH17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_TAM.entropy <- entropy(RICH17_TAMft) #entropy

RICH17_TAM.netMx <- cbind(RICH17_TAM.netMx, RICH17_TAM.clusterCoef, RICH17_TAM.degreeCent$centralization,
                          RICH17_TAM.netDensity, RICH17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************

round = 17
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
RICH17_SDMg2 <- data.frame(RICH17_SDM)
RICH17_SDMg2 <- RICH17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_SDMg2$player1
player2vector <- RICH17_SDMg2$player2
RICH17_SDMg3 <- RICH17_SDMg2
RICH17_SDMg3$p1inp2vec <- is.element(RICH17_SDMg3$player1, player2vector)
RICH17_SDMg3$p2inp1vec <- is.element(RICH17_SDMg3$player2, player1vector)

addPlayer1 <- RICH17_SDMg3[ which(RICH17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_SDMg3[ which(RICH17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_SDMg2 <- rbind(RICH17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
RICH17_SDMft <- ftable(RICH17_SDMg2$player1, RICH17_SDMg2$player2)
RICH17_SDMft2 <- as.matrix(RICH17_SDMft)
numRows <- nrow(RICH17_SDMft2)
numCols <- ncol(RICH17_SDMft2)
RICH17_SDMft3 <- RICH17_SDMft2[c(2:numRows) , c(2:numCols)]
RICH17_SDMTable <- graph.adjacency(RICH17_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(RICH17_SDMTable, vertex.label = V(RICH17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
RICH17_SDM.clusterCoef <- transitivity(RICH17_SDMTable, type="global") #cluster coefficient
RICH17_SDM.degreeCent <- centralization.degree(RICH17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_SDMftn <- as.network.matrix(RICH17_SDMft)
RICH17_SDM.netDensity <- network.density(RICH17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_SDM.entropy <- entropy(RICH17_SDMft) #entropy

RICH17_SDM.netMx <- cbind(RICH17_SDM.netMx, RICH17_SDM.clusterCoef, RICH17_SDM.degreeCent$centralization,
                          RICH17_SDM.netDensity, RICH17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
RICH17_TDMg2 <- data.frame(RICH17_TDM)
RICH17_TDMg2 <- RICH17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_TDMg2$player1
player2vector <- RICH17_TDMg2$player2
RICH17_TDMg3 <- RICH17_TDMg2
RICH17_TDMg3$p1inp2vec <- is.element(RICH17_TDMg3$player1, player2vector)
RICH17_TDMg3$p2inp1vec <- is.element(RICH17_TDMg3$player2, player1vector)

addPlayer1 <- RICH17_TDMg3[ which(RICH17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- RICH17_TDMg3[ which(RICH17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_TDMg2 <- rbind(RICH17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
RICH17_TDMft <- ftable(RICH17_TDMg2$player1, RICH17_TDMg2$player2)
RICH17_TDMft2 <- as.matrix(RICH17_TDMft)
numRows <- nrow(RICH17_TDMft2)
numCols <- ncol(RICH17_TDMft2)
RICH17_TDMft3 <- RICH17_TDMft2[c(2:numRows) , c(2:numCols)]
RICH17_TDMTable <- graph.adjacency(RICH17_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(RICH17_TDMTable, vertex.label = V(RICH17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
RICH17_TDM.clusterCoef <- transitivity(RICH17_TDMTable, type="global") #cluster coefficient
RICH17_TDM.degreeCent <- centralization.degree(RICH17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_TDMftn <- as.network.matrix(RICH17_TDMft)
RICH17_TDM.netDensity <- network.density(RICH17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_TDM.entropy <- entropy(RICH17_TDMft) #entropy

RICH17_TDM.netMx <- cbind(RICH17_TDM.netMx, RICH17_TDM.clusterCoef, RICH17_TDM.degreeCent$centralization,
                          RICH17_TDM.netDensity, RICH17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
RICH17_SDg2 <- data.frame(RICH17_SD)
RICH17_SDg2 <- RICH17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_SDg2$player1
player2vector <- RICH17_SDg2$player2
RICH17_SDg3 <- RICH17_SDg2
RICH17_SDg3$p1inp2vec <- is.element(RICH17_SDg3$player1, player2vector)
RICH17_SDg3$p2inp1vec <- is.element(RICH17_SDg3$player2, player1vector)

addPlayer1 <- RICH17_SDg3[ which(RICH17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_SDg3[ which(RICH17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_SDg2 <- rbind(RICH17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
RICH17_SDft <- ftable(RICH17_SDg2$player1, RICH17_SDg2$player2)
RICH17_SDft2 <- as.matrix(RICH17_SDft)
numRows <- nrow(RICH17_SDft2)
numCols <- ncol(RICH17_SDft2)
RICH17_SDft3 <- RICH17_SDft2[c(2:numRows) , c(2:numCols)]
RICH17_SDTable <- graph.adjacency(RICH17_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(RICH17_SDTable, vertex.label = V(RICH17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
RICH17_SD.clusterCoef <- transitivity(RICH17_SDTable, type="global") #cluster coefficient
RICH17_SD.degreeCent <- centralization.degree(RICH17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_SDftn <- as.network.matrix(RICH17_SDft)
RICH17_SD.netDensity <- network.density(RICH17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_SD.entropy <- entropy(RICH17_SDft) #entropy

RICH17_SD.netMx <- cbind(RICH17_SD.netMx, RICH17_SD.clusterCoef, RICH17_SD.degreeCent$centralization,
                         RICH17_SD.netDensity, RICH17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
RICH17_TDg2 <- data.frame(RICH17_TD)
RICH17_TDg2 <- RICH17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_TDg2$player1
player2vector <- RICH17_TDg2$player2
RICH17_TDg3 <- RICH17_TDg2
RICH17_TDg3$p1inp2vec <- is.element(RICH17_TDg3$player1, player2vector)
RICH17_TDg3$p2inp1vec <- is.element(RICH17_TDg3$player2, player1vector)

addPlayer1 <- RICH17_TDg3[ which(RICH17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_TDg3[ which(RICH17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_TDg2 <- rbind(RICH17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
RICH17_TDft <- ftable(RICH17_TDg2$player1, RICH17_TDg2$player2)
RICH17_TDft2 <- as.matrix(RICH17_TDft)
numRows <- nrow(RICH17_TDft2)
numCols <- ncol(RICH17_TDft2)
RICH17_TDft3 <- RICH17_TDft2[c(2:numRows) , c(2:numCols)]
RICH17_TDTable <- graph.adjacency(RICH17_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(RICH17_TDTable, vertex.label = V(RICH17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
RICH17_TD.clusterCoef <- transitivity(RICH17_TDTable, type="global") #cluster coefficient
RICH17_TD.degreeCent <- centralization.degree(RICH17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_TDftn <- as.network.matrix(RICH17_TDft)
RICH17_TD.netDensity <- network.density(RICH17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_TD.entropy <- entropy(RICH17_TDft) #entropy

RICH17_TD.netMx <- cbind(RICH17_TD.netMx, RICH17_TD.clusterCoef, RICH17_TD.degreeCent$centralization,
                         RICH17_TD.netDensity, RICH17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
RICH17_QTg2 <- data.frame(RICH17_QT)
RICH17_QTg2 <- RICH17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH17_QTg2$player1
player2vector <- RICH17_QTg2$player2
RICH17_QTg3 <- RICH17_QTg2
RICH17_QTg3$p1inp2vec <- is.element(RICH17_QTg3$player1, player2vector)
RICH17_QTg3$p2inp1vec <- is.element(RICH17_QTg3$player2, player1vector)

addPlayer1 <- RICH17_QTg3[ which(RICH17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH17_QTg3[ which(RICH17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH17_QTg2 <- rbind(RICH17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
RICH17_QTft <- ftable(RICH17_QTg2$player1, RICH17_QTg2$player2)
RICH17_QTft2 <- as.matrix(RICH17_QTft)
numRows <- nrow(RICH17_QTft2)
numCols <- ncol(RICH17_QTft2)
RICH17_QTft3 <- RICH17_QTft2[c(2:numRows) , c(2:numCols)]
RICH17_QTTable <- graph.adjacency(RICH17_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(RICH17_QTTable, vertex.label = V(RICH17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
RICH17_QT.clusterCoef <- transitivity(RICH17_QTTable, type="global") #cluster coefficient
RICH17_QT.degreeCent <- centralization.degree(RICH17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH17_QTftn <- as.network.matrix(RICH17_QTft)
RICH17_QT.netDensity <- network.density(RICH17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH17_QT.entropy <- entropy(RICH17_QTft) #entropy

RICH17_QT.netMx <- cbind(RICH17_QT.netMx, RICH17_QT.clusterCoef, RICH17_QT.degreeCent$centralization,
                         RICH17_QT.netDensity, RICH17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH17_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************
#NA

round = 17
teamName = "STK"
KIoutcome = "Goal_F"
STK17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
STK17_Gg2 <- data.frame(STK17_G)
STK17_Gg2 <- STK17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_Gg2$player1
player2vector <- STK17_Gg2$player2
STK17_Gg3 <- STK17_Gg2
STK17_Gg3$p1inp2vec <- is.element(STK17_Gg3$player1, player2vector)
STK17_Gg3$p2inp1vec <- is.element(STK17_Gg3$player2, player1vector)

addPlayer1 <- STK17_Gg3[ which(STK17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_Gg3[ which(STK17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_Gg2 <- rbind(STK17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
STK17_Gft <- ftable(STK17_Gg2$player1, STK17_Gg2$player2)
STK17_Gft2 <- as.matrix(STK17_Gft)
numRows <- nrow(STK17_Gft2)
numCols <- ncol(STK17_Gft2)
STK17_Gft3 <- STK17_Gft2[c(2:numRows) , c(2:numCols)]
STK17_GTable <- graph.adjacency(STK17_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(STK17_GTable, vertex.label = V(STK17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
STK17_G.clusterCoef <- transitivity(STK17_GTable, type="global") #cluster coefficient
STK17_G.degreeCent <- centralization.degree(STK17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_Gftn <- as.network.matrix(STK17_Gft)
STK17_G.netDensity <- network.density(STK17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_G.entropy <- entropy(STK17_Gft) #entropy

STK17_G.netMx <- cbind(STK17_G.netMx, STK17_G.clusterCoef, STK17_G.degreeCent$centralization,
                       STK17_G.netDensity, STK17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "STK"
KIoutcome = "Behind_F"
STK17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
STK17_Bg2 <- data.frame(STK17_B)
STK17_Bg2 <- STK17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_Bg2$player1
player2vector <- STK17_Bg2$player2
STK17_Bg3 <- STK17_Bg2
STK17_Bg3$p1inp2vec <- is.element(STK17_Bg3$player1, player2vector)
STK17_Bg3$p2inp1vec <- is.element(STK17_Bg3$player2, player1vector)

addPlayer1 <- STK17_Bg3[ which(STK17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_Bg3[ which(STK17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_Bg2 <- rbind(STK17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
STK17_Bft <- ftable(STK17_Bg2$player1, STK17_Bg2$player2)
STK17_Bft2 <- as.matrix(STK17_Bft)
numRows <- nrow(STK17_Bft2)
numCols <- ncol(STK17_Bft2)
STK17_Bft3 <- STK17_Bft2[c(2:numRows) , c(2:numCols)]
STK17_BTable <- graph.adjacency(STK17_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(STK17_BTable, vertex.label = V(STK17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
STK17_B.clusterCoef <- transitivity(STK17_BTable, type="global") #cluster coefficient
STK17_B.degreeCent <- centralization.degree(STK17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_Bftn <- as.network.matrix(STK17_Bft)
STK17_B.netDensity <- network.density(STK17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_B.entropy <- entropy(STK17_Bft) #entropy

STK17_B.netMx <- cbind(STK17_B.netMx, STK17_B.clusterCoef, STK17_B.degreeCent$centralization,
                       STK17_B.netDensity, STK17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "STK"
KIoutcome = "Stoppage_F"
STK17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
STK17_SFg2 <- data.frame(STK17_SF)
STK17_SFg2 <- STK17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_SFg2$player1
player2vector <- STK17_SFg2$player2
STK17_SFg3 <- STK17_SFg2
STK17_SFg3$p1inp2vec <- is.element(STK17_SFg3$player1, player2vector)
STK17_SFg3$p2inp1vec <- is.element(STK17_SFg3$player2, player1vector)

addPlayer1 <- STK17_SFg3[ which(STK17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_SFg3[ which(STK17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_SFg2 <- rbind(STK17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
STK17_SFft <- ftable(STK17_SFg2$player1, STK17_SFg2$player2)
STK17_SFft2 <- as.matrix(STK17_SFft)
numRows <- nrow(STK17_SFft2)
numCols <- ncol(STK17_SFft2)
STK17_SFft3 <- STK17_SFft2[c(2:numRows) , c(2:numCols)]
STK17_SFTable <- graph.adjacency(STK17_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(STK17_SFTable, vertex.label = V(STK17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
STK17_SF.clusterCoef <- transitivity(STK17_SFTable, type="global") #cluster coefficient
STK17_SF.degreeCent <- centralization.degree(STK17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_SFftn <- as.network.matrix(STK17_SFft)
STK17_SF.netDensity <- network.density(STK17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_SF.entropy <- entropy(STK17_SFft) #entropy

STK17_SF.netMx <- cbind(STK17_SF.netMx, STK17_SF.clusterCoef, STK17_SF.degreeCent$centralization,
                        STK17_SF.netDensity, STK17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "STK"
KIoutcome = "Turnover_F"
STK17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
STK17_TFg2 <- data.frame(STK17_TF)
STK17_TFg2 <- STK17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_TFg2$player1
player2vector <- STK17_TFg2$player2
STK17_TFg3 <- STK17_TFg2
STK17_TFg3$p1inp2vec <- is.element(STK17_TFg3$player1, player2vector)
STK17_TFg3$p2inp1vec <- is.element(STK17_TFg3$player2, player1vector)

addPlayer1 <- STK17_TFg3[ which(STK17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_TFg3[ which(STK17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_TFg2 <- rbind(STK17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
STK17_TFft <- ftable(STK17_TFg2$player1, STK17_TFg2$player2)
STK17_TFft2 <- as.matrix(STK17_TFft)
numRows <- nrow(STK17_TFft2)
numCols <- ncol(STK17_TFft2)
STK17_TFft3 <- STK17_TFft2[c(2:numRows) , c(2:numCols)]
STK17_TFTable <- graph.adjacency(STK17_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(STK17_TFTable, vertex.label = V(STK17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
STK17_TF.clusterCoef <- transitivity(STK17_TFTable, type="global") #cluster coefficient
STK17_TF.degreeCent <- centralization.degree(STK17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_TFftn <- as.network.matrix(STK17_TFft)
STK17_TF.netDensity <- network.density(STK17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_TF.entropy <- entropy(STK17_TFft) #entropy

STK17_TF.netMx <- cbind(STK17_TF.netMx, STK17_TF.clusterCoef, STK17_TF.degreeCent$centralization,
                        STK17_TF.netDensity, STK17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************

round = 17
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
STK17_SAMg2 <- data.frame(STK17_SAM)
STK17_SAMg2 <- STK17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_SAMg2$player1
player2vector <- STK17_SAMg2$player2
STK17_SAMg3 <- STK17_SAMg2
STK17_SAMg3$p1inp2vec <- is.element(STK17_SAMg3$player1, player2vector)
STK17_SAMg3$p2inp1vec <- is.element(STK17_SAMg3$player2, player1vector)

addPlayer1 <- STK17_SAMg3[ which(STK17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_SAMg3[ which(STK17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_SAMg2 <- rbind(STK17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
STK17_SAMft <- ftable(STK17_SAMg2$player1, STK17_SAMg2$player2)
STK17_SAMft2 <- as.matrix(STK17_SAMft)
numRows <- nrow(STK17_SAMft2)
numCols <- ncol(STK17_SAMft2)
STK17_SAMft3 <- STK17_SAMft2[c(2:numRows) , c(2:numCols)]
STK17_SAMTable <- graph.adjacency(STK17_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(STK17_SAMTable, vertex.label = V(STK17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
STK17_SAM.clusterCoef <- transitivity(STK17_SAMTable, type="global") #cluster coefficient
STK17_SAM.degreeCent <- centralization.degree(STK17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_SAMftn <- as.network.matrix(STK17_SAMft)
STK17_SAM.netDensity <- network.density(STK17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_SAM.entropy <- entropy(STK17_SAMft) #entropy

STK17_SAM.netMx <- cbind(STK17_SAM.netMx, STK17_SAM.clusterCoef, STK17_SAM.degreeCent$centralization,
                         STK17_SAM.netDensity, STK17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "STK"
KIoutcome = "Turnover_AM"
STK17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
STK17_TAMg2 <- data.frame(STK17_TAM)
STK17_TAMg2 <- STK17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_TAMg2$player1
player2vector <- STK17_TAMg2$player2
STK17_TAMg3 <- STK17_TAMg2
STK17_TAMg3$p1inp2vec <- is.element(STK17_TAMg3$player1, player2vector)
STK17_TAMg3$p2inp1vec <- is.element(STK17_TAMg3$player2, player1vector)

addPlayer1 <- STK17_TAMg3[ which(STK17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_TAMg3[ which(STK17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_TAMg2 <- rbind(STK17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
STK17_TAMft <- ftable(STK17_TAMg2$player1, STK17_TAMg2$player2)
STK17_TAMft2 <- as.matrix(STK17_TAMft)
numRows <- nrow(STK17_TAMft2)
numCols <- ncol(STK17_TAMft2)
STK17_TAMft3 <- STK17_TAMft2[c(2:numRows) , c(2:numCols)]
STK17_TAMTable <- graph.adjacency(STK17_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(STK17_TAMTable, vertex.label = V(STK17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
STK17_TAM.clusterCoef <- transitivity(STK17_TAMTable, type="global") #cluster coefficient
STK17_TAM.degreeCent <- centralization.degree(STK17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_TAMftn <- as.network.matrix(STK17_TAMft)
STK17_TAM.netDensity <- network.density(STK17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_TAM.entropy <- entropy(STK17_TAMft) #entropy

STK17_TAM.netMx <- cbind(STK17_TAM.netMx, STK17_TAM.clusterCoef, STK17_TAM.degreeCent$centralization,
                         STK17_TAM.netDensity, STK17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************

round = 17
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
STK17_SDMg2 <- data.frame(STK17_SDM)
STK17_SDMg2 <- STK17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_SDMg2$player1
player2vector <- STK17_SDMg2$player2
STK17_SDMg3 <- STK17_SDMg2
STK17_SDMg3$p1inp2vec <- is.element(STK17_SDMg3$player1, player2vector)
STK17_SDMg3$p2inp1vec <- is.element(STK17_SDMg3$player2, player1vector)

addPlayer1 <- STK17_SDMg3[ which(STK17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_SDMg3[ which(STK17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_SDMg2 <- rbind(STK17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
STK17_SDMft <- ftable(STK17_SDMg2$player1, STK17_SDMg2$player2)
STK17_SDMft2 <- as.matrix(STK17_SDMft)
numRows <- nrow(STK17_SDMft2)
numCols <- ncol(STK17_SDMft2)
STK17_SDMft3 <- STK17_SDMft2[c(2:numRows) , c(2:numCols)]
STK17_SDMTable <- graph.adjacency(STK17_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(STK17_SDMTable, vertex.label = V(STK17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
STK17_SDM.clusterCoef <- transitivity(STK17_SDMTable, type="global") #cluster coefficient
STK17_SDM.degreeCent <- centralization.degree(STK17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_SDMftn <- as.network.matrix(STK17_SDMft)
STK17_SDM.netDensity <- network.density(STK17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_SDM.entropy <- entropy(STK17_SDMft) #entropy

STK17_SDM.netMx <- cbind(STK17_SDM.netMx, STK17_SDM.clusterCoef, STK17_SDM.degreeCent$centralization,
                         STK17_SDM.netDensity, STK17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "STK"
KIoutcome = "Turnover_DM"
STK17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
STK17_TDMg2 <- data.frame(STK17_TDM)
STK17_TDMg2 <- STK17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_TDMg2$player1
player2vector <- STK17_TDMg2$player2
STK17_TDMg3 <- STK17_TDMg2
STK17_TDMg3$p1inp2vec <- is.element(STK17_TDMg3$player1, player2vector)
STK17_TDMg3$p2inp1vec <- is.element(STK17_TDMg3$player2, player1vector)

addPlayer1 <- STK17_TDMg3[ which(STK17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_TDMg3[ which(STK17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_TDMg2 <- rbind(STK17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
STK17_TDMft <- ftable(STK17_TDMg2$player1, STK17_TDMg2$player2)
STK17_TDMft2 <- as.matrix(STK17_TDMft)
numRows <- nrow(STK17_TDMft2)
numCols <- ncol(STK17_TDMft2)
STK17_TDMft3 <- STK17_TDMft2[c(2:numRows) , c(2:numCols)]
STK17_TDMTable <- graph.adjacency(STK17_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(STK17_TDMTable, vertex.label = V(STK17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
STK17_TDM.clusterCoef <- transitivity(STK17_TDMTable, type="global") #cluster coefficient
STK17_TDM.degreeCent <- centralization.degree(STK17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_TDMftn <- as.network.matrix(STK17_TDMft)
STK17_TDM.netDensity <- network.density(STK17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_TDM.entropy <- entropy(STK17_TDMft) #entropy

STK17_TDM.netMx <- cbind(STK17_TDM.netMx, STK17_TDM.clusterCoef, STK17_TDM.degreeCent$centralization,
                         STK17_TDM.netDensity, STK17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "STK"
KIoutcome = "Stoppage_D"
STK17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
STK17_SDg2 <- data.frame(STK17_SD)
STK17_SDg2 <- STK17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_SDg2$player1
player2vector <- STK17_SDg2$player2
STK17_SDg3 <- STK17_SDg2
STK17_SDg3$p1inp2vec <- is.element(STK17_SDg3$player1, player2vector)
STK17_SDg3$p2inp1vec <- is.element(STK17_SDg3$player2, player1vector)

addPlayer1 <- STK17_SDg3[ which(STK17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_SDg3[ which(STK17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_SDg2 <- rbind(STK17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
STK17_SDft <- ftable(STK17_SDg2$player1, STK17_SDg2$player2)
STK17_SDft2 <- as.matrix(STK17_SDft)
numRows <- nrow(STK17_SDft2)
numCols <- ncol(STK17_SDft2)
STK17_SDft3 <- STK17_SDft2[c(2:numRows) , c(2:numCols)]
STK17_SDTable <- graph.adjacency(STK17_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(STK17_SDTable, vertex.label = V(STK17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
STK17_SD.clusterCoef <- transitivity(STK17_SDTable, type="global") #cluster coefficient
STK17_SD.degreeCent <- centralization.degree(STK17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_SDftn <- as.network.matrix(STK17_SDft)
STK17_SD.netDensity <- network.density(STK17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_SD.entropy <- entropy(STK17_SDft) #entropy

STK17_SD.netMx <- cbind(STK17_SD.netMx, STK17_SD.clusterCoef, STK17_SD.degreeCent$centralization,
                        STK17_SD.netDensity, STK17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "STK"
KIoutcome = "Turnover_D"
STK17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
STK17_TDg2 <- data.frame(STK17_TD)
STK17_TDg2 <- STK17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_TDg2$player1
player2vector <- STK17_TDg2$player2
STK17_TDg3 <- STK17_TDg2
STK17_TDg3$p1inp2vec <- is.element(STK17_TDg3$player1, player2vector)
STK17_TDg3$p2inp1vec <- is.element(STK17_TDg3$player2, player1vector)

addPlayer1 <- STK17_TDg3[ which(STK17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_TDg3[ which(STK17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_TDg2 <- rbind(STK17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
STK17_TDft <- ftable(STK17_TDg2$player1, STK17_TDg2$player2)
STK17_TDft2 <- as.matrix(STK17_TDft)
numRows <- nrow(STK17_TDft2)
numCols <- ncol(STK17_TDft2)
STK17_TDft3 <- STK17_TDft2[c(2:numRows) , c(2:numCols)]
STK17_TDTable <- graph.adjacency(STK17_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(STK17_TDTable, vertex.label = V(STK17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
STK17_TD.clusterCoef <- transitivity(STK17_TDTable, type="global") #cluster coefficient
STK17_TD.degreeCent <- centralization.degree(STK17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_TDftn <- as.network.matrix(STK17_TDft)
STK17_TD.netDensity <- network.density(STK17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_TD.entropy <- entropy(STK17_TDft) #entropy

STK17_TD.netMx <- cbind(STK17_TD.netMx, STK17_TD.clusterCoef, STK17_TD.degreeCent$centralization,
                        STK17_TD.netDensity, STK17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
STK17_QTg2 <- data.frame(STK17_QT)
STK17_QTg2 <- STK17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK17_QTg2$player1
player2vector <- STK17_QTg2$player2
STK17_QTg3 <- STK17_QTg2
STK17_QTg3$p1inp2vec <- is.element(STK17_QTg3$player1, player2vector)
STK17_QTg3$p2inp1vec <- is.element(STK17_QTg3$player2, player1vector)

addPlayer1 <- STK17_QTg3[ which(STK17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK17_QTg3[ which(STK17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK17_QTg2 <- rbind(STK17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
STK17_QTft <- ftable(STK17_QTg2$player1, STK17_QTg2$player2)
STK17_QTft2 <- as.matrix(STK17_QTft)
numRows <- nrow(STK17_QTft2)
numCols <- ncol(STK17_QTft2)
STK17_QTft3 <- STK17_QTft2[c(2:numRows) , c(2:numCols)]
STK17_QTTable <- graph.adjacency(STK17_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(STK17_QTTable, vertex.label = V(STK17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
STK17_QT.clusterCoef <- transitivity(STK17_QTTable, type="global") #cluster coefficient
STK17_QT.degreeCent <- centralization.degree(STK17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK17_QTftn <- as.network.matrix(STK17_QTft)
STK17_QT.netDensity <- network.density(STK17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK17_QT.entropy <- entropy(STK17_QTft) #entropy

STK17_QT.netMx <- cbind(STK17_QT.netMx, STK17_QT.clusterCoef, STK17_QT.degreeCent$centralization,
                        STK17_QT.netDensity, STK17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK17_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************
#NA

round = 17
teamName = "SYD"
KIoutcome = "Goal_F"
SYD17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
SYD17_Gg2 <- data.frame(SYD17_G)
SYD17_Gg2 <- SYD17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_Gg2$player1
player2vector <- SYD17_Gg2$player2
SYD17_Gg3 <- SYD17_Gg2
SYD17_Gg3$p1inp2vec <- is.element(SYD17_Gg3$player1, player2vector)
SYD17_Gg3$p2inp1vec <- is.element(SYD17_Gg3$player2, player1vector)

addPlayer1 <- SYD17_Gg3[ which(SYD17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_Gg3[ which(SYD17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_Gg2 <- rbind(SYD17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
SYD17_Gft <- ftable(SYD17_Gg2$player1, SYD17_Gg2$player2)
SYD17_Gft2 <- as.matrix(SYD17_Gft)
numRows <- nrow(SYD17_Gft2)
numCols <- ncol(SYD17_Gft2)
SYD17_Gft3 <- SYD17_Gft2[c(2:numRows) , c(2:numCols)]
SYD17_GTable <- graph.adjacency(SYD17_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(SYD17_GTable, vertex.label = V(SYD17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
SYD17_G.clusterCoef <- transitivity(SYD17_GTable, type="global") #cluster coefficient
SYD17_G.degreeCent <- centralization.degree(SYD17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_Gftn <- as.network.matrix(SYD17_Gft)
SYD17_G.netDensity <- network.density(SYD17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_G.entropy <- entropy(SYD17_Gft) #entropy

SYD17_G.netMx <- cbind(SYD17_G.netMx, SYD17_G.clusterCoef, SYD17_G.degreeCent$centralization,
                       SYD17_G.netDensity, SYD17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************

round = 17
teamName = "SYD"
KIoutcome = "Behind_F"
SYD17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
SYD17_Bg2 <- data.frame(SYD17_B)
SYD17_Bg2 <- SYD17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_Bg2$player1
player2vector <- SYD17_Bg2$player2
SYD17_Bg3 <- SYD17_Bg2
SYD17_Bg3$p1inp2vec <- is.element(SYD17_Bg3$player1, player2vector)
SYD17_Bg3$p2inp1vec <- is.element(SYD17_Bg3$player2, player1vector)

addPlayer1 <- SYD17_Bg3[ which(SYD17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_Bg3[ which(SYD17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_Bg2 <- rbind(SYD17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
SYD17_Bft <- ftable(SYD17_Bg2$player1, SYD17_Bg2$player2)
SYD17_Bft2 <- as.matrix(SYD17_Bft)
numRows <- nrow(SYD17_Bft2)
numCols <- ncol(SYD17_Bft2)
SYD17_Bft3 <- SYD17_Bft2[c(2:numRows) , c(2:numCols)]
SYD17_BTable <- graph.adjacency(SYD17_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(SYD17_BTable, vertex.label = V(SYD17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
SYD17_B.clusterCoef <- transitivity(SYD17_BTable, type="global") #cluster coefficient
SYD17_B.degreeCent <- centralization.degree(SYD17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_Bftn <- as.network.matrix(SYD17_Bft)
SYD17_B.netDensity <- network.density(SYD17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_B.entropy <- entropy(SYD17_Bft) #entropy

SYD17_B.netMx <- cbind(SYD17_B.netMx, SYD17_B.clusterCoef, SYD17_B.degreeCent$centralization,
                       SYD17_B.netDensity, SYD17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************

round = 17
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
SYD17_SFg2 <- data.frame(SYD17_SF)
SYD17_SFg2 <- SYD17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_SFg2$player1
player2vector <- SYD17_SFg2$player2
SYD17_SFg3 <- SYD17_SFg2
SYD17_SFg3$p1inp2vec <- is.element(SYD17_SFg3$player1, player2vector)
SYD17_SFg3$p2inp1vec <- is.element(SYD17_SFg3$player2, player1vector)

addPlayer1 <- SYD17_SFg3[ which(SYD17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_SFg3[ which(SYD17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_SFg2 <- rbind(SYD17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
SYD17_SFft <- ftable(SYD17_SFg2$player1, SYD17_SFg2$player2)
SYD17_SFft2 <- as.matrix(SYD17_SFft)
numRows <- nrow(SYD17_SFft2)
numCols <- ncol(SYD17_SFft2)
SYD17_SFft3 <- SYD17_SFft2[c(2:numRows) , c(2:numCols)]
SYD17_SFTable <- graph.adjacency(SYD17_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(SYD17_SFTable, vertex.label = V(SYD17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
SYD17_SF.clusterCoef <- transitivity(SYD17_SFTable, type="global") #cluster coefficient
SYD17_SF.degreeCent <- centralization.degree(SYD17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_SFftn <- as.network.matrix(SYD17_SFft)
SYD17_SF.netDensity <- network.density(SYD17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_SF.entropy <- entropy(SYD17_SFft) #entropy

SYD17_SF.netMx <- cbind(SYD17_SF.netMx, SYD17_SF.clusterCoef, SYD17_SF.degreeCent$centralization,
                        SYD17_SF.netDensity, SYD17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
SYD17_TFg2 <- data.frame(SYD17_TF)
SYD17_TFg2 <- SYD17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_TFg2$player1
player2vector <- SYD17_TFg2$player2
SYD17_TFg3 <- SYD17_TFg2
SYD17_TFg3$p1inp2vec <- is.element(SYD17_TFg3$player1, player2vector)
SYD17_TFg3$p2inp1vec <- is.element(SYD17_TFg3$player2, player1vector)

addPlayer1 <- SYD17_TFg3[ which(SYD17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_TFg3[ which(SYD17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_TFg2 <- rbind(SYD17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
SYD17_TFft <- ftable(SYD17_TFg2$player1, SYD17_TFg2$player2)
SYD17_TFft2 <- as.matrix(SYD17_TFft)
numRows <- nrow(SYD17_TFft2)
numCols <- ncol(SYD17_TFft2)
SYD17_TFft3 <- SYD17_TFft2[c(2:numRows) , c(2:numCols)]
SYD17_TFTable <- graph.adjacency(SYD17_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(SYD17_TFTable, vertex.label = V(SYD17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
SYD17_TF.clusterCoef <- transitivity(SYD17_TFTable, type="global") #cluster coefficient
SYD17_TF.degreeCent <- centralization.degree(SYD17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_TFftn <- as.network.matrix(SYD17_TFft)
SYD17_TF.netDensity <- network.density(SYD17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_TF.entropy <- entropy(SYD17_TFft) #entropy

SYD17_TF.netMx <- cbind(SYD17_TF.netMx, SYD17_TF.clusterCoef, SYD17_TF.degreeCent$centralization,
                        SYD17_TF.netDensity, SYD17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************
#NA

round = 17
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
SYD17_SAMg2 <- data.frame(SYD17_SAM)
SYD17_SAMg2 <- SYD17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_SAMg2$player1
player2vector <- SYD17_SAMg2$player2
SYD17_SAMg3 <- SYD17_SAMg2
SYD17_SAMg3$p1inp2vec <- is.element(SYD17_SAMg3$player1, player2vector)
SYD17_SAMg3$p2inp1vec <- is.element(SYD17_SAMg3$player2, player1vector)

addPlayer1 <- SYD17_SAMg3[ which(SYD17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_SAMg3[ which(SYD17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_SAMg2 <- rbind(SYD17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
SYD17_SAMft <- ftable(SYD17_SAMg2$player1, SYD17_SAMg2$player2)
SYD17_SAMft2 <- as.matrix(SYD17_SAMft)
numRows <- nrow(SYD17_SAMft2)
numCols <- ncol(SYD17_SAMft2)
SYD17_SAMft3 <- SYD17_SAMft2[c(2:numRows) , c(2:numCols)]
SYD17_SAMTable <- graph.adjacency(SYD17_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(SYD17_SAMTable, vertex.label = V(SYD17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
SYD17_SAM.clusterCoef <- transitivity(SYD17_SAMTable, type="global") #cluster coefficient
SYD17_SAM.degreeCent <- centralization.degree(SYD17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_SAMftn <- as.network.matrix(SYD17_SAMft)
SYD17_SAM.netDensity <- network.density(SYD17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_SAM.entropy <- entropy(SYD17_SAMft) #entropy

SYD17_SAM.netMx <- cbind(SYD17_SAM.netMx, SYD17_SAM.clusterCoef, SYD17_SAM.degreeCent$centralization,
                         SYD17_SAM.netDensity, SYD17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************
#NA

round = 17
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
SYD17_TAMg2 <- data.frame(SYD17_TAM)
SYD17_TAMg2 <- SYD17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_TAMg2$player1
player2vector <- SYD17_TAMg2$player2
SYD17_TAMg3 <- SYD17_TAMg2
SYD17_TAMg3$p1inp2vec <- is.element(SYD17_TAMg3$player1, player2vector)
SYD17_TAMg3$p2inp1vec <- is.element(SYD17_TAMg3$player2, player1vector)

addPlayer1 <- SYD17_TAMg3[ which(SYD17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_TAMg3[ which(SYD17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_TAMg2 <- rbind(SYD17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
SYD17_TAMft <- ftable(SYD17_TAMg2$player1, SYD17_TAMg2$player2)
SYD17_TAMft2 <- as.matrix(SYD17_TAMft)
numRows <- nrow(SYD17_TAMft2)
numCols <- ncol(SYD17_TAMft2)
SYD17_TAMft3 <- SYD17_TAMft2[c(2:numRows) , c(2:numCols)]
SYD17_TAMTable <- graph.adjacency(SYD17_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(SYD17_TAMTable, vertex.label = V(SYD17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
SYD17_TAM.clusterCoef <- transitivity(SYD17_TAMTable, type="global") #cluster coefficient
SYD17_TAM.degreeCent <- centralization.degree(SYD17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_TAMftn <- as.network.matrix(SYD17_TAMft)
SYD17_TAM.netDensity <- network.density(SYD17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_TAM.entropy <- entropy(SYD17_TAMft) #entropy

SYD17_TAM.netMx <- cbind(SYD17_TAM.netMx, SYD17_TAM.clusterCoef, SYD17_TAM.degreeCent$centralization,
                         SYD17_TAM.netDensity, SYD17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************
#NA

round = 17
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
SYD17_SDMg2 <- data.frame(SYD17_SDM)
SYD17_SDMg2 <- SYD17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_SDMg2$player1
player2vector <- SYD17_SDMg2$player2
SYD17_SDMg3 <- SYD17_SDMg2
SYD17_SDMg3$p1inp2vec <- is.element(SYD17_SDMg3$player1, player2vector)
SYD17_SDMg3$p2inp1vec <- is.element(SYD17_SDMg3$player2, player1vector)

addPlayer1 <- SYD17_SDMg3[ which(SYD17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_SDMg3[ which(SYD17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_SDMg2 <- rbind(SYD17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
SYD17_SDMft <- ftable(SYD17_SDMg2$player1, SYD17_SDMg2$player2)
SYD17_SDMft2 <- as.matrix(SYD17_SDMft)
numRows <- nrow(SYD17_SDMft2)
numCols <- ncol(SYD17_SDMft2)
SYD17_SDMft3 <- SYD17_SDMft2[c(2:numRows) , c(2:numCols)]
SYD17_SDMTable <- graph.adjacency(SYD17_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(SYD17_SDMTable, vertex.label = V(SYD17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
SYD17_SDM.clusterCoef <- transitivity(SYD17_SDMTable, type="global") #cluster coefficient
SYD17_SDM.degreeCent <- centralization.degree(SYD17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_SDMftn <- as.network.matrix(SYD17_SDMft)
SYD17_SDM.netDensity <- network.density(SYD17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_SDM.entropy <- entropy(SYD17_SDMft) #entropy

SYD17_SDM.netMx <- cbind(SYD17_SDM.netMx, SYD17_SDM.clusterCoef, SYD17_SDM.degreeCent$centralization,
                         SYD17_SDM.netDensity, SYD17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
SYD17_TDMg2 <- data.frame(SYD17_TDM)
SYD17_TDMg2 <- SYD17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_TDMg2$player1
player2vector <- SYD17_TDMg2$player2
SYD17_TDMg3 <- SYD17_TDMg2
SYD17_TDMg3$p1inp2vec <- is.element(SYD17_TDMg3$player1, player2vector)
SYD17_TDMg3$p2inp1vec <- is.element(SYD17_TDMg3$player2, player1vector)

addPlayer1 <- SYD17_TDMg3[ which(SYD17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_TDMg3[ which(SYD17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_TDMg2 <- rbind(SYD17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
SYD17_TDMft <- ftable(SYD17_TDMg2$player1, SYD17_TDMg2$player2)
SYD17_TDMft2 <- as.matrix(SYD17_TDMft)
numRows <- nrow(SYD17_TDMft2)
numCols <- ncol(SYD17_TDMft2)
SYD17_TDMft3 <- SYD17_TDMft2[c(2:numRows) , c(2:numCols)]
SYD17_TDMTable <- graph.adjacency(SYD17_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(SYD17_TDMTable, vertex.label = V(SYD17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
SYD17_TDM.clusterCoef <- transitivity(SYD17_TDMTable, type="global") #cluster coefficient
SYD17_TDM.degreeCent <- centralization.degree(SYD17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_TDMftn <- as.network.matrix(SYD17_TDMft)
SYD17_TDM.netDensity <- network.density(SYD17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_TDM.entropy <- entropy(SYD17_TDMft) #entropy

SYD17_TDM.netMx <- cbind(SYD17_TDM.netMx, SYD17_TDM.clusterCoef, SYD17_TDM.degreeCent$centralization,
                         SYD17_TDM.netDensity, SYD17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************

round = 17
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
SYD17_SDg2 <- data.frame(SYD17_SD)
SYD17_SDg2 <- SYD17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_SDg2$player1
player2vector <- SYD17_SDg2$player2
SYD17_SDg3 <- SYD17_SDg2
SYD17_SDg3$p1inp2vec <- is.element(SYD17_SDg3$player1, player2vector)
SYD17_SDg3$p2inp1vec <- is.element(SYD17_SDg3$player2, player1vector)

addPlayer1 <- SYD17_SDg3[ which(SYD17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- SYD17_SDg3[ which(SYD17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_SDg2 <- rbind(SYD17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
SYD17_SDft <- ftable(SYD17_SDg2$player1, SYD17_SDg2$player2)
SYD17_SDft2 <- as.matrix(SYD17_SDft)
numRows <- nrow(SYD17_SDft2)
numCols <- ncol(SYD17_SDft2)
SYD17_SDft3 <- SYD17_SDft2[c(2:numRows) , c(2:numCols)]
SYD17_SDTable <- graph.adjacency(SYD17_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(SYD17_SDTable, vertex.label = V(SYD17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
SYD17_SD.clusterCoef <- transitivity(SYD17_SDTable, type="global") #cluster coefficient
SYD17_SD.degreeCent <- centralization.degree(SYD17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_SDftn <- as.network.matrix(SYD17_SDft)
SYD17_SD.netDensity <- network.density(SYD17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_SD.entropy <- entropy(SYD17_SDft) #entropy

SYD17_SD.netMx <- cbind(SYD17_SD.netMx, SYD17_SD.clusterCoef, SYD17_SD.degreeCent$centralization,
                        SYD17_SD.netDensity, SYD17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************
#NA

round = 17
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
SYD17_TDg2 <- data.frame(SYD17_TD)
SYD17_TDg2 <- SYD17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_TDg2$player1
player2vector <- SYD17_TDg2$player2
SYD17_TDg3 <- SYD17_TDg2
SYD17_TDg3$p1inp2vec <- is.element(SYD17_TDg3$player1, player2vector)
SYD17_TDg3$p2inp1vec <- is.element(SYD17_TDg3$player2, player1vector)

addPlayer1 <- SYD17_TDg3[ which(SYD17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_TDg3[ which(SYD17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_TDg2 <- rbind(SYD17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
SYD17_TDft <- ftable(SYD17_TDg2$player1, SYD17_TDg2$player2)
SYD17_TDft2 <- as.matrix(SYD17_TDft)
numRows <- nrow(SYD17_TDft2)
numCols <- ncol(SYD17_TDft2)
SYD17_TDft3 <- SYD17_TDft2[c(2:numRows) , c(2:numCols)]
SYD17_TDTable <- graph.adjacency(SYD17_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(SYD17_TDTable, vertex.label = V(SYD17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
SYD17_TD.clusterCoef <- transitivity(SYD17_TDTable, type="global") #cluster coefficient
SYD17_TD.degreeCent <- centralization.degree(SYD17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_TDftn <- as.network.matrix(SYD17_TDft)
SYD17_TD.netDensity <- network.density(SYD17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_TD.entropy <- entropy(SYD17_TDft) #entropy

SYD17_TD.netMx <- cbind(SYD17_TD.netMx, SYD17_TD.clusterCoef, SYD17_TD.degreeCent$centralization,
                        SYD17_TD.netDensity, SYD17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
SYD17_QTg2 <- data.frame(SYD17_QT)
SYD17_QTg2 <- SYD17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD17_QTg2$player1
player2vector <- SYD17_QTg2$player2
SYD17_QTg3 <- SYD17_QTg2
SYD17_QTg3$p1inp2vec <- is.element(SYD17_QTg3$player1, player2vector)
SYD17_QTg3$p2inp1vec <- is.element(SYD17_QTg3$player2, player1vector)

addPlayer1 <- SYD17_QTg3[ which(SYD17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD17_QTg3[ which(SYD17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD17_QTg2 <- rbind(SYD17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
SYD17_QTft <- ftable(SYD17_QTg2$player1, SYD17_QTg2$player2)
SYD17_QTft2 <- as.matrix(SYD17_QTft)
numRows <- nrow(SYD17_QTft2)
numCols <- ncol(SYD17_QTft2)
SYD17_QTft3 <- SYD17_QTft2[c(2:numRows) , c(2:numCols)]
SYD17_QTTable <- graph.adjacency(SYD17_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(SYD17_QTTable, vertex.label = V(SYD17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
SYD17_QT.clusterCoef <- transitivity(SYD17_QTTable, type="global") #cluster coefficient
SYD17_QT.degreeCent <- centralization.degree(SYD17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD17_QTftn <- as.network.matrix(SYD17_QTft)
SYD17_QT.netDensity <- network.density(SYD17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD17_QT.entropy <- entropy(SYD17_QTft) #entropy

SYD17_QT.netMx <- cbind(SYD17_QT.netMx, SYD17_QT.clusterCoef, SYD17_QT.degreeCent$centralization,
                        SYD17_QT.netDensity, SYD17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD17_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************

round = 17
teamName = "WB"
KIoutcome = "Goal_F"
WB17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
WB17_Gg2 <- data.frame(WB17_G)
WB17_Gg2 <- WB17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_Gg2$player1
player2vector <- WB17_Gg2$player2
WB17_Gg3 <- WB17_Gg2
WB17_Gg3$p1inp2vec <- is.element(WB17_Gg3$player1, player2vector)
WB17_Gg3$p2inp1vec <- is.element(WB17_Gg3$player2, player1vector)

addPlayer1 <- WB17_Gg3[ which(WB17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_Gg3[ which(WB17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_Gg2 <- rbind(WB17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
WB17_Gft <- ftable(WB17_Gg2$player1, WB17_Gg2$player2)
WB17_Gft2 <- as.matrix(WB17_Gft)
numRows <- nrow(WB17_Gft2)
numCols <- ncol(WB17_Gft2)
WB17_Gft3 <- WB17_Gft2[c(2:numRows) , c(2:numCols)]
WB17_GTable <- graph.adjacency(WB17_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(WB17_GTable, vertex.label = V(WB17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
WB17_G.clusterCoef <- transitivity(WB17_GTable, type="global") #cluster coefficient
WB17_G.degreeCent <- centralization.degree(WB17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_Gftn <- as.network.matrix(WB17_Gft)
WB17_G.netDensity <- network.density(WB17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_G.entropy <- entropy(WB17_Gft) #entropy

WB17_G.netMx <- cbind(WB17_G.netMx, WB17_G.clusterCoef, WB17_G.degreeCent$centralization,
                      WB17_G.netDensity, WB17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************

round = 17
teamName = "WB"
KIoutcome = "Behind_F"
WB17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
WB17_Bg2 <- data.frame(WB17_B)
WB17_Bg2 <- WB17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_Bg2$player1
player2vector <- WB17_Bg2$player2
WB17_Bg3 <- WB17_Bg2
WB17_Bg3$p1inp2vec <- is.element(WB17_Bg3$player1, player2vector)
WB17_Bg3$p2inp1vec <- is.element(WB17_Bg3$player2, player1vector)

addPlayer1 <- WB17_Bg3[ which(WB17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WB17_Bg3[ which(WB17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_Bg2 <- rbind(WB17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
WB17_Bft <- ftable(WB17_Bg2$player1, WB17_Bg2$player2)
WB17_Bft2 <- as.matrix(WB17_Bft)
numRows <- nrow(WB17_Bft2)
numCols <- ncol(WB17_Bft2)
WB17_Bft3 <- WB17_Bft2[c(2:numRows) , c(2:numCols)]
WB17_BTable <- graph.adjacency(WB17_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(WB17_BTable, vertex.label = V(WB17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
WB17_B.clusterCoef <- transitivity(WB17_BTable, type="global") #cluster coefficient
WB17_B.degreeCent <- centralization.degree(WB17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_Bftn <- as.network.matrix(WB17_Bft)
WB17_B.netDensity <- network.density(WB17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_B.entropy <- entropy(WB17_Bft) #entropy

WB17_B.netMx <- cbind(WB17_B.netMx, WB17_B.clusterCoef, WB17_B.degreeCent$centralization,
                      WB17_B.netDensity, WB17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "WB"
KIoutcome = "Stoppage_F"
WB17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
WB17_SFg2 <- data.frame(WB17_SF)
WB17_SFg2 <- WB17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_SFg2$player1
player2vector <- WB17_SFg2$player2
WB17_SFg3 <- WB17_SFg2
WB17_SFg3$p1inp2vec <- is.element(WB17_SFg3$player1, player2vector)
WB17_SFg3$p2inp1vec <- is.element(WB17_SFg3$player2, player1vector)

addPlayer1 <- WB17_SFg3[ which(WB17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_SFg3[ which(WB17_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_SFg2 <- rbind(WB17_SFg2, addPlayers)

#ROUND 17, FWD Stoppage graph using weighted edges
WB17_SFft <- ftable(WB17_SFg2$player1, WB17_SFg2$player2)
WB17_SFft2 <- as.matrix(WB17_SFft)
numRows <- nrow(WB17_SFft2)
numCols <- ncol(WB17_SFft2)
WB17_SFft3 <- WB17_SFft2[c(2:numRows) , c(2:numCols)]
WB17_SFTable <- graph.adjacency(WB17_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(WB17_SFTable, vertex.label = V(WB17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
WB17_SF.clusterCoef <- transitivity(WB17_SFTable, type="global") #cluster coefficient
WB17_SF.degreeCent <- centralization.degree(WB17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_SFftn <- as.network.matrix(WB17_SFft)
WB17_SF.netDensity <- network.density(WB17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_SF.entropy <- entropy(WB17_SFft) #entropy

WB17_SF.netMx <- cbind(WB17_SF.netMx, WB17_SF.clusterCoef, WB17_SF.degreeCent$centralization,
                       WB17_SF.netDensity, WB17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "WB"
KIoutcome = "Turnover_F"
WB17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
WB17_TFg2 <- data.frame(WB17_TF)
WB17_TFg2 <- WB17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_TFg2$player1
player2vector <- WB17_TFg2$player2
WB17_TFg3 <- WB17_TFg2
WB17_TFg3$p1inp2vec <- is.element(WB17_TFg3$player1, player2vector)
WB17_TFg3$p2inp1vec <- is.element(WB17_TFg3$player2, player1vector)

addPlayer1 <- WB17_TFg3[ which(WB17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_TFg3[ which(WB17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_TFg2 <- rbind(WB17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
WB17_TFft <- ftable(WB17_TFg2$player1, WB17_TFg2$player2)
WB17_TFft2 <- as.matrix(WB17_TFft)
numRows <- nrow(WB17_TFft2)
numCols <- ncol(WB17_TFft2)
WB17_TFft3 <- WB17_TFft2[c(2:numRows) , c(2:numCols)]
WB17_TFTable <- graph.adjacency(WB17_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(WB17_TFTable, vertex.label = V(WB17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
WB17_TF.clusterCoef <- transitivity(WB17_TFTable, type="global") #cluster coefficient
WB17_TF.degreeCent <- centralization.degree(WB17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_TFftn <- as.network.matrix(WB17_TFft)
WB17_TF.netDensity <- network.density(WB17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_TF.entropy <- entropy(WB17_TFft) #entropy

WB17_TF.netMx <- cbind(WB17_TF.netMx, WB17_TF.clusterCoef, WB17_TF.degreeCent$centralization,
                       WB17_TF.netDensity, WB17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************
#NA

round = 17
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
WB17_SAMg2 <- data.frame(WB17_SAM)
WB17_SAMg2 <- WB17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_SAMg2$player1
player2vector <- WB17_SAMg2$player2
WB17_SAMg3 <- WB17_SAMg2
WB17_SAMg3$p1inp2vec <- is.element(WB17_SAMg3$player1, player2vector)
WB17_SAMg3$p2inp1vec <- is.element(WB17_SAMg3$player2, player1vector)

addPlayer1 <- WB17_SAMg3[ which(WB17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_SAMg3[ which(WB17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_SAMg2 <- rbind(WB17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
WB17_SAMft <- ftable(WB17_SAMg2$player1, WB17_SAMg2$player2)
WB17_SAMft2 <- as.matrix(WB17_SAMft)
numRows <- nrow(WB17_SAMft2)
numCols <- ncol(WB17_SAMft2)
WB17_SAMft3 <- WB17_SAMft2[c(2:numRows) , c(2:numCols)]
WB17_SAMTable <- graph.adjacency(WB17_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(WB17_SAMTable, vertex.label = V(WB17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
WB17_SAM.clusterCoef <- transitivity(WB17_SAMTable, type="global") #cluster coefficient
WB17_SAM.degreeCent <- centralization.degree(WB17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_SAMftn <- as.network.matrix(WB17_SAMft)
WB17_SAM.netDensity <- network.density(WB17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_SAM.entropy <- entropy(WB17_SAMft) #entropy

WB17_SAM.netMx <- cbind(WB17_SAM.netMx, WB17_SAM.clusterCoef, WB17_SAM.degreeCent$centralization,
                        WB17_SAM.netDensity, WB17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "WB"
KIoutcome = "Turnover_AM"
WB17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
WB17_TAMg2 <- data.frame(WB17_TAM)
WB17_TAMg2 <- WB17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_TAMg2$player1
player2vector <- WB17_TAMg2$player2
WB17_TAMg3 <- WB17_TAMg2
WB17_TAMg3$p1inp2vec <- is.element(WB17_TAMg3$player1, player2vector)
WB17_TAMg3$p2inp1vec <- is.element(WB17_TAMg3$player2, player1vector)

addPlayer1 <- WB17_TAMg3[ which(WB17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_TAMg3[ which(WB17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_TAMg2 <- rbind(WB17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
WB17_TAMft <- ftable(WB17_TAMg2$player1, WB17_TAMg2$player2)
WB17_TAMft2 <- as.matrix(WB17_TAMft)
numRows <- nrow(WB17_TAMft2)
numCols <- ncol(WB17_TAMft2)
WB17_TAMft3 <- WB17_TAMft2[c(2:numRows) , c(2:numCols)]
WB17_TAMTable <- graph.adjacency(WB17_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(WB17_TAMTable, vertex.label = V(WB17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
WB17_TAM.clusterCoef <- transitivity(WB17_TAMTable, type="global") #cluster coefficient
WB17_TAM.degreeCent <- centralization.degree(WB17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_TAMftn <- as.network.matrix(WB17_TAMft)
WB17_TAM.netDensity <- network.density(WB17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_TAM.entropy <- entropy(WB17_TAMft) #entropy

WB17_TAM.netMx <- cbind(WB17_TAM.netMx, WB17_TAM.clusterCoef, WB17_TAM.degreeCent$centralization,
                        WB17_TAM.netDensity, WB17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************
#NA

round = 17
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
WB17_SDMg2 <- data.frame(WB17_SDM)
WB17_SDMg2 <- WB17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_SDMg2$player1
player2vector <- WB17_SDMg2$player2
WB17_SDMg3 <- WB17_SDMg2
WB17_SDMg3$p1inp2vec <- is.element(WB17_SDMg3$player1, player2vector)
WB17_SDMg3$p2inp1vec <- is.element(WB17_SDMg3$player2, player1vector)

addPlayer1 <- WB17_SDMg3[ which(WB17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_SDMg3[ which(WB17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_SDMg2 <- rbind(WB17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
WB17_SDMft <- ftable(WB17_SDMg2$player1, WB17_SDMg2$player2)
WB17_SDMft2 <- as.matrix(WB17_SDMft)
numRows <- nrow(WB17_SDMft2)
numCols <- ncol(WB17_SDMft2)
WB17_SDMft3 <- WB17_SDMft2[c(2:numRows) , c(2:numCols)]
WB17_SDMTable <- graph.adjacency(WB17_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(WB17_SDMTable, vertex.label = V(WB17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
WB17_SDM.clusterCoef <- transitivity(WB17_SDMTable, type="global") #cluster coefficient
WB17_SDM.degreeCent <- centralization.degree(WB17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_SDMftn <- as.network.matrix(WB17_SDMft)
WB17_SDM.netDensity <- network.density(WB17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_SDM.entropy <- entropy(WB17_SDMft) #entropy

WB17_SDM.netMx <- cbind(WB17_SDM.netMx, WB17_SDM.clusterCoef, WB17_SDM.degreeCent$centralization,
                        WB17_SDM.netDensity, WB17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************
#NA

round = 17
teamName = "WB"
KIoutcome = "Turnover_DM"
WB17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
WB17_TDMg2 <- data.frame(WB17_TDM)
WB17_TDMg2 <- WB17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_TDMg2$player1
player2vector <- WB17_TDMg2$player2
WB17_TDMg3 <- WB17_TDMg2
WB17_TDMg3$p1inp2vec <- is.element(WB17_TDMg3$player1, player2vector)
WB17_TDMg3$p2inp1vec <- is.element(WB17_TDMg3$player2, player1vector)

addPlayer1 <- WB17_TDMg3[ which(WB17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_TDMg3[ which(WB17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_TDMg2 <- rbind(WB17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
WB17_TDMft <- ftable(WB17_TDMg2$player1, WB17_TDMg2$player2)
WB17_TDMft2 <- as.matrix(WB17_TDMft)
numRows <- nrow(WB17_TDMft2)
numCols <- ncol(WB17_TDMft2)
WB17_TDMft3 <- WB17_TDMft2[c(2:numRows) , c(2:numCols)]
WB17_TDMTable <- graph.adjacency(WB17_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(WB17_TDMTable, vertex.label = V(WB17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
WB17_TDM.clusterCoef <- transitivity(WB17_TDMTable, type="global") #cluster coefficient
WB17_TDM.degreeCent <- centralization.degree(WB17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_TDMftn <- as.network.matrix(WB17_TDMft)
WB17_TDM.netDensity <- network.density(WB17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_TDM.entropy <- entropy(WB17_TDMft) #entropy

WB17_TDM.netMx <- cbind(WB17_TDM.netMx, WB17_TDM.clusterCoef, WB17_TDM.degreeCent$centralization,
                        WB17_TDM.netDensity, WB17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************

round = 17
teamName = "WB"
KIoutcome = "Stoppage_D"
WB17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
WB17_SDg2 <- data.frame(WB17_SD)
WB17_SDg2 <- WB17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_SDg2$player1
player2vector <- WB17_SDg2$player2
WB17_SDg3 <- WB17_SDg2
WB17_SDg3$p1inp2vec <- is.element(WB17_SDg3$player1, player2vector)
WB17_SDg3$p2inp1vec <- is.element(WB17_SDg3$player2, player1vector)

addPlayer1 <- WB17_SDg3[ which(WB17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_SDg3[ which(WB17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_SDg2 <- rbind(WB17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
WB17_SDft <- ftable(WB17_SDg2$player1, WB17_SDg2$player2)
WB17_SDft2 <- as.matrix(WB17_SDft)
numRows <- nrow(WB17_SDft2)
numCols <- ncol(WB17_SDft2)
WB17_SDft3 <- WB17_SDft2[c(2:numRows) , c(2:numCols)]
WB17_SDTable <- graph.adjacency(WB17_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(WB17_SDTable, vertex.label = V(WB17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
WB17_SD.clusterCoef <- transitivity(WB17_SDTable, type="global") #cluster coefficient
WB17_SD.degreeCent <- centralization.degree(WB17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_SDftn <- as.network.matrix(WB17_SDft)
WB17_SD.netDensity <- network.density(WB17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_SD.entropy <- entropy(WB17_SDft) #entropy

WB17_SD.netMx <- cbind(WB17_SD.netMx, WB17_SD.clusterCoef, WB17_SD.degreeCent$centralization,
                       WB17_SD.netDensity, WB17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************

round = 17
teamName = "WB"
KIoutcome = "Turnover_D"
WB17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
WB17_TDg2 <- data.frame(WB17_TD)
WB17_TDg2 <- WB17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_TDg2$player1
player2vector <- WB17_TDg2$player2
WB17_TDg3 <- WB17_TDg2
WB17_TDg3$p1inp2vec <- is.element(WB17_TDg3$player1, player2vector)
WB17_TDg3$p2inp1vec <- is.element(WB17_TDg3$player2, player1vector)

addPlayer1 <- WB17_TDg3[ which(WB17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_TDg3[ which(WB17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_TDg2 <- rbind(WB17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
WB17_TDft <- ftable(WB17_TDg2$player1, WB17_TDg2$player2)
WB17_TDft2 <- as.matrix(WB17_TDft)
numRows <- nrow(WB17_TDft2)
numCols <- ncol(WB17_TDft2)
WB17_TDft3 <- WB17_TDft2[c(2:numRows) , c(2:numCols)]
WB17_TDTable <- graph.adjacency(WB17_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(WB17_TDTable, vertex.label = V(WB17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
WB17_TD.clusterCoef <- transitivity(WB17_TDTable, type="global") #cluster coefficient
WB17_TD.degreeCent <- centralization.degree(WB17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_TDftn <- as.network.matrix(WB17_TDft)
WB17_TD.netDensity <- network.density(WB17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_TD.entropy <- entropy(WB17_TDft) #entropy

WB17_TD.netMx <- cbind(WB17_TD.netMx, WB17_TD.clusterCoef, WB17_TD.degreeCent$centralization,
                       WB17_TD.netDensity, WB17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
WB17_QTg2 <- data.frame(WB17_QT)
WB17_QTg2 <- WB17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB17_QTg2$player1
player2vector <- WB17_QTg2$player2
WB17_QTg3 <- WB17_QTg2
WB17_QTg3$p1inp2vec <- is.element(WB17_QTg3$player1, player2vector)
WB17_QTg3$p2inp1vec <- is.element(WB17_QTg3$player2, player1vector)

addPlayer1 <- WB17_QTg3[ which(WB17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB17_QTg3[ which(WB17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB17_QTg2 <- rbind(WB17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
WB17_QTft <- ftable(WB17_QTg2$player1, WB17_QTg2$player2)
WB17_QTft2 <- as.matrix(WB17_QTft)
numRows <- nrow(WB17_QTft2)
numCols <- ncol(WB17_QTft2)
WB17_QTft3 <- WB17_QTft2[c(2:numRows) , c(2:numCols)]
WB17_QTTable <- graph.adjacency(WB17_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(WB17_QTTable, vertex.label = V(WB17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
WB17_QT.clusterCoef <- transitivity(WB17_QTTable, type="global") #cluster coefficient
WB17_QT.degreeCent <- centralization.degree(WB17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB17_QTftn <- as.network.matrix(WB17_QTft)
WB17_QT.netDensity <- network.density(WB17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB17_QT.entropy <- entropy(WB17_QTft) #entropy

WB17_QT.netMx <- cbind(WB17_QT.netMx, WB17_QT.clusterCoef, WB17_QT.degreeCent$centralization,
                       WB17_QT.netDensity, WB17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB17_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 17
##

#ROUND 17, Goal***************************************************************
#NA

round = 17
teamName = "WCE"
KIoutcome = "Goal_F"
WCE17_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Goal with weighted edges
WCE17_Gg2 <- data.frame(WCE17_G)
WCE17_Gg2 <- WCE17_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_Gg2$player1
player2vector <- WCE17_Gg2$player2
WCE17_Gg3 <- WCE17_Gg2
WCE17_Gg3$p1inp2vec <- is.element(WCE17_Gg3$player1, player2vector)
WCE17_Gg3$p2inp1vec <- is.element(WCE17_Gg3$player2, player1vector)

addPlayer1 <- WCE17_Gg3[ which(WCE17_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_Gg3[ which(WCE17_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_Gg2 <- rbind(WCE17_Gg2, addPlayers)

#ROUND 17, Goal graph using weighted edges
WCE17_Gft <- ftable(WCE17_Gg2$player1, WCE17_Gg2$player2)
WCE17_Gft2 <- as.matrix(WCE17_Gft)
numRows <- nrow(WCE17_Gft2)
numCols <- ncol(WCE17_Gft2)
WCE17_Gft3 <- WCE17_Gft2[c(2:numRows) , c(2:numCols)]
WCE17_GTable <- graph.adjacency(WCE17_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Goal graph=weighted
plot.igraph(WCE17_GTable, vertex.label = V(WCE17_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Goal calulation of network metrics
#igraph
WCE17_G.clusterCoef <- transitivity(WCE17_GTable, type="global") #cluster coefficient
WCE17_G.degreeCent <- centralization.degree(WCE17_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_Gftn <- as.network.matrix(WCE17_Gft)
WCE17_G.netDensity <- network.density(WCE17_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_G.entropy <- entropy(WCE17_Gft) #entropy

WCE17_G.netMx <- cbind(WCE17_G.netMx, WCE17_G.clusterCoef, WCE17_G.degreeCent$centralization,
                       WCE17_G.netDensity, WCE17_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_G.netMx) <- varnames

#ROUND 17, Behind***************************************************************
#NA

round = 17
teamName = "WCE"
KIoutcome = "Behind_F"
WCE17_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, Behind with weighted edges
WCE17_Bg2 <- data.frame(WCE17_B)
WCE17_Bg2 <- WCE17_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_Bg2$player1
player2vector <- WCE17_Bg2$player2
WCE17_Bg3 <- WCE17_Bg2
WCE17_Bg3$p1inp2vec <- is.element(WCE17_Bg3$player1, player2vector)
WCE17_Bg3$p2inp1vec <- is.element(WCE17_Bg3$player2, player1vector)

addPlayer1 <- WCE17_Bg3[ which(WCE17_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_Bg3[ which(WCE17_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_Bg2 <- rbind(WCE17_Bg2, addPlayers)

#ROUND 17, Behind graph using weighted edges
WCE17_Bft <- ftable(WCE17_Bg2$player1, WCE17_Bg2$player2)
WCE17_Bft2 <- as.matrix(WCE17_Bft)
numRows <- nrow(WCE17_Bft2)
numCols <- ncol(WCE17_Bft2)
WCE17_Bft3 <- WCE17_Bft2[c(2:numRows) , c(2:numCols)]
WCE17_BTable <- graph.adjacency(WCE17_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 17, Behind graph=weighted
plot.igraph(WCE17_BTable, vertex.label = V(WCE17_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, Behind calulation of network metrics
#igraph
WCE17_B.clusterCoef <- transitivity(WCE17_BTable, type="global") #cluster coefficient
WCE17_B.degreeCent <- centralization.degree(WCE17_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_Bftn <- as.network.matrix(WCE17_Bft)
WCE17_B.netDensity <- network.density(WCE17_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_B.entropy <- entropy(WCE17_Bft) #entropy

WCE17_B.netMx <- cbind(WCE17_B.netMx, WCE17_B.clusterCoef, WCE17_B.degreeCent$centralization,
                       WCE17_B.netDensity, WCE17_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_B.netMx) <- varnames

#ROUND 17, FWD Stoppage**********************************************************
#NA

round = 17
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE17_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Stoppage with weighted edges
WCE17_SFg2 <- data.frame(WCE17_SF)
WCE17_SFg2 <- WCE17_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_SFg2$player1
player2vector <- WCE17_SFg2$player2
WCE17_SFg3 <- WCE17_SFg2
WCE17_SFg3$p1inp2vec <- is.element(WCE17_SFg3$player1, player2vector)
WCE17_SFg3$p2inp1vec <- is.element(WCE17_SFg3$player2, player1vector)

addPlayer1 <- WCE17_SFg3[ which(WCE17_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE17_SFg2 <- rbind(WCE17_SFg2, addPlayer1)

#ROUND 17, FWD Stoppage graph using weighted edges
WCE17_SFft <- ftable(WCE17_SFg2$player1, WCE17_SFg2$player2)
WCE17_SFft2 <- as.matrix(WCE17_SFft)
numRows <- nrow(WCE17_SFft2)
numCols <- ncol(WCE17_SFft2)
WCE17_SFft3 <- WCE17_SFft2[c(2:numRows) , c(1:numCols)]
WCE17_SFTable <- graph.adjacency(WCE17_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Stoppage graph=weighted
plot.igraph(WCE17_SFTable, vertex.label = V(WCE17_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Stoppage calulation of network metrics
#igraph
WCE17_SF.clusterCoef <- transitivity(WCE17_SFTable, type="global") #cluster coefficient
WCE17_SF.degreeCent <- centralization.degree(WCE17_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_SFftn <- as.network.matrix(WCE17_SFft)
WCE17_SF.netDensity <- network.density(WCE17_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_SF.entropy <- entropy(WCE17_SFft) #entropy

WCE17_SF.netMx <- cbind(WCE17_SF.netMx, WCE17_SF.clusterCoef, WCE17_SF.degreeCent$centralization,
                        WCE17_SF.netDensity, WCE17_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_SF.netMx) <- varnames

#ROUND 17, FWD Turnover**********************************************************
#NA

round = 17
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE17_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, FWD Turnover with weighted edges
WCE17_TFg2 <- data.frame(WCE17_TF)
WCE17_TFg2 <- WCE17_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_TFg2$player1
player2vector <- WCE17_TFg2$player2
WCE17_TFg3 <- WCE17_TFg2
WCE17_TFg3$p1inp2vec <- is.element(WCE17_TFg3$player1, player2vector)
WCE17_TFg3$p2inp1vec <- is.element(WCE17_TFg3$player2, player1vector)

addPlayer1 <- WCE17_TFg3[ which(WCE17_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_TFg3[ which(WCE17_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_TFg2 <- rbind(WCE17_TFg2, addPlayers)

#ROUND 17, FWD Turnover graph using weighted edges
WCE17_TFft <- ftable(WCE17_TFg2$player1, WCE17_TFg2$player2)
WCE17_TFft2 <- as.matrix(WCE17_TFft)
numRows <- nrow(WCE17_TFft2)
numCols <- ncol(WCE17_TFft2)
WCE17_TFft3 <- WCE17_TFft2[c(2:numRows) , c(2:numCols)]
WCE17_TFTable <- graph.adjacency(WCE17_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, FWD Turnover graph=weighted
plot.igraph(WCE17_TFTable, vertex.label = V(WCE17_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, FWD Turnover calulation of network metrics
#igraph
WCE17_TF.clusterCoef <- transitivity(WCE17_TFTable, type="global") #cluster coefficient
WCE17_TF.degreeCent <- centralization.degree(WCE17_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_TFftn <- as.network.matrix(WCE17_TFft)
WCE17_TF.netDensity <- network.density(WCE17_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_TF.entropy <- entropy(WCE17_TFft) #entropy

WCE17_TF.netMx <- cbind(WCE17_TF.netMx, WCE17_TF.clusterCoef, WCE17_TF.degreeCent$centralization,
                        WCE17_TF.netDensity, WCE17_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_TF.netMx) <- varnames

#ROUND 17, AM Stoppage**********************************************************

round = 17
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE17_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Stoppage with weighted edges
WCE17_SAMg2 <- data.frame(WCE17_SAM)
WCE17_SAMg2 <- WCE17_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_SAMg2$player1
player2vector <- WCE17_SAMg2$player2
WCE17_SAMg3 <- WCE17_SAMg2
WCE17_SAMg3$p1inp2vec <- is.element(WCE17_SAMg3$player1, player2vector)
WCE17_SAMg3$p2inp1vec <- is.element(WCE17_SAMg3$player2, player1vector)

addPlayer1 <- WCE17_SAMg3[ which(WCE17_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_SAMg3[ which(WCE17_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_SAMg2 <- rbind(WCE17_SAMg2, addPlayers)

#ROUND 17, AM Stoppage graph using weighted edges
WCE17_SAMft <- ftable(WCE17_SAMg2$player1, WCE17_SAMg2$player2)
WCE17_SAMft2 <- as.matrix(WCE17_SAMft)
numRows <- nrow(WCE17_SAMft2)
numCols <- ncol(WCE17_SAMft2)
WCE17_SAMft3 <- WCE17_SAMft2[c(2:numRows) , c(2:numCols)]
WCE17_SAMTable <- graph.adjacency(WCE17_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Stoppage graph=weighted
plot.igraph(WCE17_SAMTable, vertex.label = V(WCE17_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Stoppage calulation of network metrics
#igraph
WCE17_SAM.clusterCoef <- transitivity(WCE17_SAMTable, type="global") #cluster coefficient
WCE17_SAM.degreeCent <- centralization.degree(WCE17_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_SAMftn <- as.network.matrix(WCE17_SAMft)
WCE17_SAM.netDensity <- network.density(WCE17_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_SAM.entropy <- entropy(WCE17_SAMft) #entropy

WCE17_SAM.netMx <- cbind(WCE17_SAM.netMx, WCE17_SAM.clusterCoef, WCE17_SAM.degreeCent$centralization,
                         WCE17_SAM.netDensity, WCE17_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_SAM.netMx) <- varnames

#ROUND 17, AM Turnover**********************************************************

round = 17
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE17_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, AM Turnover with weighted edges
WCE17_TAMg2 <- data.frame(WCE17_TAM)
WCE17_TAMg2 <- WCE17_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_TAMg2$player1
player2vector <- WCE17_TAMg2$player2
WCE17_TAMg3 <- WCE17_TAMg2
WCE17_TAMg3$p1inp2vec <- is.element(WCE17_TAMg3$player1, player2vector)
WCE17_TAMg3$p2inp1vec <- is.element(WCE17_TAMg3$player2, player1vector)

addPlayer1 <- WCE17_TAMg3[ which(WCE17_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_TAMg3[ which(WCE17_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_TAMg2 <- rbind(WCE17_TAMg2, addPlayers)

#ROUND 17, AM Turnover graph using weighted edges
WCE17_TAMft <- ftable(WCE17_TAMg2$player1, WCE17_TAMg2$player2)
WCE17_TAMft2 <- as.matrix(WCE17_TAMft)
numRows <- nrow(WCE17_TAMft2)
numCols <- ncol(WCE17_TAMft2)
WCE17_TAMft3 <- WCE17_TAMft2[c(2:numRows) , c(2:numCols)]
WCE17_TAMTable <- graph.adjacency(WCE17_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, AM Turnover graph=weighted
plot.igraph(WCE17_TAMTable, vertex.label = V(WCE17_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, AM Turnover calulation of network metrics
#igraph
WCE17_TAM.clusterCoef <- transitivity(WCE17_TAMTable, type="global") #cluster coefficient
WCE17_TAM.degreeCent <- centralization.degree(WCE17_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_TAMftn <- as.network.matrix(WCE17_TAMft)
WCE17_TAM.netDensity <- network.density(WCE17_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_TAM.entropy <- entropy(WCE17_TAMft) #entropy

WCE17_TAM.netMx <- cbind(WCE17_TAM.netMx, WCE17_TAM.clusterCoef, WCE17_TAM.degreeCent$centralization,
                         WCE17_TAM.netDensity, WCE17_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_TAM.netMx) <- varnames

#ROUND 17, DM Stoppage**********************************************************
#NA

round = 17
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE17_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Stoppage with weighted edges
WCE17_SDMg2 <- data.frame(WCE17_SDM)
WCE17_SDMg2 <- WCE17_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_SDMg2$player1
player2vector <- WCE17_SDMg2$player2
WCE17_SDMg3 <- WCE17_SDMg2
WCE17_SDMg3$p1inp2vec <- is.element(WCE17_SDMg3$player1, player2vector)
WCE17_SDMg3$p2inp1vec <- is.element(WCE17_SDMg3$player2, player1vector)

addPlayer1 <- WCE17_SDMg3[ which(WCE17_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_SDMg3[ which(WCE17_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_SDMg2 <- rbind(WCE17_SDMg2, addPlayers)

#ROUND 17, DM Stoppage graph using weighted edges
WCE17_SDMft <- ftable(WCE17_SDMg2$player1, WCE17_SDMg2$player2)
WCE17_SDMft2 <- as.matrix(WCE17_SDMft)
numRows <- nrow(WCE17_SDMft2)
numCols <- ncol(WCE17_SDMft2)
WCE17_SDMft3 <- WCE17_SDMft2[c(2:numRows) , c(2:numCols)]
WCE17_SDMTable <- graph.adjacency(WCE17_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Stoppage graph=weighted
plot.igraph(WCE17_SDMTable, vertex.label = V(WCE17_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Stoppage calulation of network metrics
#igraph
WCE17_SDM.clusterCoef <- transitivity(WCE17_SDMTable, type="global") #cluster coefficient
WCE17_SDM.degreeCent <- centralization.degree(WCE17_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_SDMftn <- as.network.matrix(WCE17_SDMft)
WCE17_SDM.netDensity <- network.density(WCE17_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_SDM.entropy <- entropy(WCE17_SDMft) #entropy

WCE17_SDM.netMx <- cbind(WCE17_SDM.netMx, WCE17_SDM.clusterCoef, WCE17_SDM.degreeCent$centralization,
                         WCE17_SDM.netDensity, WCE17_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_SDM.netMx) <- varnames

#ROUND 17, DM Turnover**********************************************************

round = 17
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE17_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, DM Turnover with weighted edges
WCE17_TDMg2 <- data.frame(WCE17_TDM)
WCE17_TDMg2 <- WCE17_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_TDMg2$player1
player2vector <- WCE17_TDMg2$player2
WCE17_TDMg3 <- WCE17_TDMg2
WCE17_TDMg3$p1inp2vec <- is.element(WCE17_TDMg3$player1, player2vector)
WCE17_TDMg3$p2inp1vec <- is.element(WCE17_TDMg3$player2, player1vector)

addPlayer1 <- WCE17_TDMg3[ which(WCE17_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_TDMg3[ which(WCE17_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_TDMg2 <- rbind(WCE17_TDMg2, addPlayers)

#ROUND 17, DM Turnover graph using weighted edges
WCE17_TDMft <- ftable(WCE17_TDMg2$player1, WCE17_TDMg2$player2)
WCE17_TDMft2 <- as.matrix(WCE17_TDMft)
numRows <- nrow(WCE17_TDMft2)
numCols <- ncol(WCE17_TDMft2)
WCE17_TDMft3 <- WCE17_TDMft2[c(2:numRows) , c(2:numCols)]
WCE17_TDMTable <- graph.adjacency(WCE17_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 17, DM Turnover graph=weighted
plot.igraph(WCE17_TDMTable, vertex.label = V(WCE17_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, DM Turnover calulation of network metrics
#igraph
WCE17_TDM.clusterCoef <- transitivity(WCE17_TDMTable, type="global") #cluster coefficient
WCE17_TDM.degreeCent <- centralization.degree(WCE17_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_TDMftn <- as.network.matrix(WCE17_TDMft)
WCE17_TDM.netDensity <- network.density(WCE17_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_TDM.entropy <- entropy(WCE17_TDMft) #entropy

WCE17_TDM.netMx <- cbind(WCE17_TDM.netMx, WCE17_TDM.clusterCoef, WCE17_TDM.degreeCent$centralization,
                         WCE17_TDM.netDensity, WCE17_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_TDM.netMx) <- varnames

#ROUND 17, D Stoppage**********************************************************
#NA

round = 17
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE17_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Stoppage with weighted edges
WCE17_SDg2 <- data.frame(WCE17_SD)
WCE17_SDg2 <- WCE17_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_SDg2$player1
player2vector <- WCE17_SDg2$player2
WCE17_SDg3 <- WCE17_SDg2
WCE17_SDg3$p1inp2vec <- is.element(WCE17_SDg3$player1, player2vector)
WCE17_SDg3$p2inp1vec <- is.element(WCE17_SDg3$player2, player1vector)

addPlayer1 <- WCE17_SDg3[ which(WCE17_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_SDg3[ which(WCE17_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_SDg2 <- rbind(WCE17_SDg2, addPlayers)

#ROUND 17, D Stoppage graph using weighted edges
WCE17_SDft <- ftable(WCE17_SDg2$player1, WCE17_SDg2$player2)
WCE17_SDft2 <- as.matrix(WCE17_SDft)
numRows <- nrow(WCE17_SDft2)
numCols <- ncol(WCE17_SDft2)
WCE17_SDft3 <- WCE17_SDft2[c(2:numRows) , c(2:numCols)]
WCE17_SDTable <- graph.adjacency(WCE17_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Stoppage graph=weighted
plot.igraph(WCE17_SDTable, vertex.label = V(WCE17_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Stoppage calulation of network metrics
#igraph
WCE17_SD.clusterCoef <- transitivity(WCE17_SDTable, type="global") #cluster coefficient
WCE17_SD.degreeCent <- centralization.degree(WCE17_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_SDftn <- as.network.matrix(WCE17_SDft)
WCE17_SD.netDensity <- network.density(WCE17_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_SD.entropy <- entropy(WCE17_SDft) #entropy

WCE17_SD.netMx <- cbind(WCE17_SD.netMx, WCE17_SD.clusterCoef, WCE17_SD.degreeCent$centralization,
                        WCE17_SD.netDensity, WCE17_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_SD.netMx) <- varnames

#ROUND 17, D Turnover**********************************************************

round = 17
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE17_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, D Turnover with weighted edges
WCE17_TDg2 <- data.frame(WCE17_TD)
WCE17_TDg2 <- WCE17_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_TDg2$player1
player2vector <- WCE17_TDg2$player2
WCE17_TDg3 <- WCE17_TDg2
WCE17_TDg3$p1inp2vec <- is.element(WCE17_TDg3$player1, player2vector)
WCE17_TDg3$p2inp1vec <- is.element(WCE17_TDg3$player2, player1vector)

addPlayer1 <- WCE17_TDg3[ which(WCE17_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_TDg3[ which(WCE17_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_TDg2 <- rbind(WCE17_TDg2, addPlayers)

#ROUND 17, D Turnover graph using weighted edges
WCE17_TDft <- ftable(WCE17_TDg2$player1, WCE17_TDg2$player2)
WCE17_TDft2 <- as.matrix(WCE17_TDft)
numRows <- nrow(WCE17_TDft2)
numCols <- ncol(WCE17_TDft2)
WCE17_TDft3 <- WCE17_TDft2[c(2:numRows) , c(2:numCols)]
WCE17_TDTable <- graph.adjacency(WCE17_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, D Turnover graph=weighted
plot.igraph(WCE17_TDTable, vertex.label = V(WCE17_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, D Turnover calulation of network metrics
#igraph
WCE17_TD.clusterCoef <- transitivity(WCE17_TDTable, type="global") #cluster coefficient
WCE17_TD.degreeCent <- centralization.degree(WCE17_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_TDftn <- as.network.matrix(WCE17_TDft)
WCE17_TD.netDensity <- network.density(WCE17_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_TD.entropy <- entropy(WCE17_TDft) #entropy

WCE17_TD.netMx <- cbind(WCE17_TD.netMx, WCE17_TD.clusterCoef, WCE17_TD.degreeCent$centralization,
                        WCE17_TD.netDensity, WCE17_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_TD.netMx) <- varnames

#ROUND 17, End of Qtr**********************************************************
#NA

round = 17
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE17_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 17, End of Qtr with weighted edges
WCE17_QTg2 <- data.frame(WCE17_QT)
WCE17_QTg2 <- WCE17_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE17_QTg2$player1
player2vector <- WCE17_QTg2$player2
WCE17_QTg3 <- WCE17_QTg2
WCE17_QTg3$p1inp2vec <- is.element(WCE17_QTg3$player1, player2vector)
WCE17_QTg3$p2inp1vec <- is.element(WCE17_QTg3$player2, player1vector)

addPlayer1 <- WCE17_QTg3[ which(WCE17_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE17_QTg3[ which(WCE17_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE17_QTg2 <- rbind(WCE17_QTg2, addPlayers)

#ROUND 17, End of Qtr graph using weighted edges
WCE17_QTft <- ftable(WCE17_QTg2$player1, WCE17_QTg2$player2)
WCE17_QTft2 <- as.matrix(WCE17_QTft)
numRows <- nrow(WCE17_QTft2)
numCols <- ncol(WCE17_QTft2)
WCE17_QTft3 <- WCE17_QTft2[c(2:numRows) , c(2:numCols)]
WCE17_QTTable <- graph.adjacency(WCE17_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 17, End of Qtr graph=weighted
plot.igraph(WCE17_QTTable, vertex.label = V(WCE17_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE17_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 17, End of Qtr calulation of network metrics
#igraph
WCE17_QT.clusterCoef <- transitivity(WCE17_QTTable, type="global") #cluster coefficient
WCE17_QT.degreeCent <- centralization.degree(WCE17_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE17_QTftn <- as.network.matrix(WCE17_QTft)
WCE17_QT.netDensity <- network.density(WCE17_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE17_QT.entropy <- entropy(WCE17_QTft) #entropy

WCE17_QT.netMx <- cbind(WCE17_QT.netMx, WCE17_QT.clusterCoef, WCE17_QT.degreeCent$centralization,
                        WCE17_QT.netDensity, WCE17_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE17_QT.netMx) <- varnames
