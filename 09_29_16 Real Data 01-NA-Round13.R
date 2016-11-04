#####
#09-29-16- Real data 13
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************
#NA

round = 13
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
ADEL13_Gg2 <- data.frame(ADEL13_G)
ADEL13_Gg2 <- ADEL13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_Gg2$player1
player2vector <- ADEL13_Gg2$player2
ADEL13_Gg3 <- ADEL13_Gg2
ADEL13_Gg3$p1inp2vec <- is.element(ADEL13_Gg3$player1, player2vector)
ADEL13_Gg3$p2inp1vec <- is.element(ADEL13_Gg3$player2, player1vector)

addPlayer1 <- ADEL13_Gg3[ which(ADEL13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_Gg3[ which(ADEL13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_Gg2 <- rbind(ADEL13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
ADEL13_Gft <- ftable(ADEL13_Gg2$player1, ADEL13_Gg2$player2)
ADEL13_Gft2 <- as.matrix(ADEL13_Gft)
numRows <- nrow(ADEL13_Gft2)
numCols <- ncol(ADEL13_Gft2)
ADEL13_Gft3 <- ADEL13_Gft2[c(2:numRows) , c(2:numCols)]
ADEL13_GTable <- graph.adjacency(ADEL13_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(ADEL13_GTable, vertex.label = V(ADEL13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
ADEL13_G.clusterCoef <- transitivity(ADEL13_GTable, type="global") #cluster coefficient
ADEL13_G.degreeCent <- centralization.degree(ADEL13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_Gftn <- as.network.matrix(ADEL13_Gft)
ADEL13_G.netDensity <- network.density(ADEL13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_G.entropy <- entropy(ADEL13_Gft) #entropy

ADEL13_G.netMx <- cbind(ADEL13_G.netMx, ADEL13_G.clusterCoef, ADEL13_G.degreeCent$centralization,
                        ADEL13_G.netDensity, ADEL13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************
#NA

round = 13
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
ADEL13_Bg2 <- data.frame(ADEL13_B)
ADEL13_Bg2 <- ADEL13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_Bg2$player1
player2vector <- ADEL13_Bg2$player2
ADEL13_Bg3 <- ADEL13_Bg2
ADEL13_Bg3$p1inp2vec <- is.element(ADEL13_Bg3$player1, player2vector)
ADEL13_Bg3$p2inp1vec <- is.element(ADEL13_Bg3$player2, player1vector)

addPlayer1 <- ADEL13_Bg3[ which(ADEL13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_Bg3[ which(ADEL13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_Bg2 <- rbind(ADEL13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
ADEL13_Bft <- ftable(ADEL13_Bg2$player1, ADEL13_Bg2$player2)
ADEL13_Bft2 <- as.matrix(ADEL13_Bft)
numRows <- nrow(ADEL13_Bft2)
numCols <- ncol(ADEL13_Bft2)
ADEL13_Bft3 <- ADEL13_Bft2[c(2:numRows) , c(2:numCols)]
ADEL13_BTable <- graph.adjacency(ADEL13_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(ADEL13_BTable, vertex.label = V(ADEL13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
ADEL13_B.clusterCoef <- transitivity(ADEL13_BTable, type="global") #cluster coefficient
ADEL13_B.degreeCent <- centralization.degree(ADEL13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_Bftn <- as.network.matrix(ADEL13_Bft)
ADEL13_B.netDensity <- network.density(ADEL13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_B.entropy <- entropy(ADEL13_Bft) #entropy

ADEL13_B.netMx <- cbind(ADEL13_B.netMx, ADEL13_B.clusterCoef, ADEL13_B.degreeCent$centralization,
                        ADEL13_B.netDensity, ADEL13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************
#NA

round = 13
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
ADEL13_SFg2 <- data.frame(ADEL13_SF)
ADEL13_SFg2 <- ADEL13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_SFg2$player1
player2vector <- ADEL13_SFg2$player2
ADEL13_SFg3 <- ADEL13_SFg2
ADEL13_SFg3$p1inp2vec <- is.element(ADEL13_SFg3$player1, player2vector)
ADEL13_SFg3$p2inp1vec <- is.element(ADEL13_SFg3$player2, player1vector)

addPlayer1 <- ADEL13_SFg3[ which(ADEL13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_SFg3[ which(ADEL13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_SFg2 <- rbind(ADEL13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
ADEL13_SFft <- ftable(ADEL13_SFg2$player1, ADEL13_SFg2$player2)
ADEL13_SFft2 <- as.matrix(ADEL13_SFft)
numRows <- nrow(ADEL13_SFft2)
numCols <- ncol(ADEL13_SFft2)
ADEL13_SFft3 <- ADEL13_SFft2[c(2:numRows) , c(2:numCols)]
ADEL13_SFTable <- graph.adjacency(ADEL13_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(ADEL13_SFTable, vertex.label = V(ADEL13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
ADEL13_SF.clusterCoef <- transitivity(ADEL13_SFTable, type="global") #cluster coefficient
ADEL13_SF.degreeCent <- centralization.degree(ADEL13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_SFftn <- as.network.matrix(ADEL13_SFft)
ADEL13_SF.netDensity <- network.density(ADEL13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_SF.entropy <- entropy(ADEL13_SFft) #entropy

ADEL13_SF.netMx <- cbind(ADEL13_SF.netMx, ADEL13_SF.clusterCoef, ADEL13_SF.degreeCent$centralization,
                         ADEL13_SF.netDensity, ADEL13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************

round = 13
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
ADEL13_TFg2 <- data.frame(ADEL13_TF)
ADEL13_TFg2 <- ADEL13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_TFg2$player1
player2vector <- ADEL13_TFg2$player2
ADEL13_TFg3 <- ADEL13_TFg2
ADEL13_TFg3$p1inp2vec <- is.element(ADEL13_TFg3$player1, player2vector)
ADEL13_TFg3$p2inp1vec <- is.element(ADEL13_TFg3$player2, player1vector)

addPlayer1 <- ADEL13_TFg3[ which(ADEL13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_TFg3[ which(ADEL13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_TFg2 <- rbind(ADEL13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
ADEL13_TFft <- ftable(ADEL13_TFg2$player1, ADEL13_TFg2$player2)
ADEL13_TFft2 <- as.matrix(ADEL13_TFft)
numRows <- nrow(ADEL13_TFft2)
numCols <- ncol(ADEL13_TFft2)
ADEL13_TFft3 <- ADEL13_TFft2[c(2:numRows) , c(2:numCols)]
ADEL13_TFTable <- graph.adjacency(ADEL13_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(ADEL13_TFTable, vertex.label = V(ADEL13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
ADEL13_TF.clusterCoef <- transitivity(ADEL13_TFTable, type="global") #cluster coefficient
ADEL13_TF.degreeCent <- centralization.degree(ADEL13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_TFftn <- as.network.matrix(ADEL13_TFft)
ADEL13_TF.netDensity <- network.density(ADEL13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_TF.entropy <- entropy(ADEL13_TFft) #entropy

ADEL13_TF.netMx <- cbind(ADEL13_TF.netMx, ADEL13_TF.clusterCoef, ADEL13_TF.degreeCent$centralization,
                         ADEL13_TF.netDensity, ADEL13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************

round = 13
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
ADEL13_SAMg2 <- data.frame(ADEL13_SAM)
ADEL13_SAMg2 <- ADEL13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_SAMg2$player1
player2vector <- ADEL13_SAMg2$player2
ADEL13_SAMg3 <- ADEL13_SAMg2
ADEL13_SAMg3$p1inp2vec <- is.element(ADEL13_SAMg3$player1, player2vector)
ADEL13_SAMg3$p2inp1vec <- is.element(ADEL13_SAMg3$player2, player1vector)

addPlayer1 <- ADEL13_SAMg3[ which(ADEL13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_SAMg3[ which(ADEL13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_SAMg2 <- rbind(ADEL13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
ADEL13_SAMft <- ftable(ADEL13_SAMg2$player1, ADEL13_SAMg2$player2)
ADEL13_SAMft2 <- as.matrix(ADEL13_SAMft)
numRows <- nrow(ADEL13_SAMft2)
numCols <- ncol(ADEL13_SAMft2)
ADEL13_SAMft3 <- ADEL13_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL13_SAMTable <- graph.adjacency(ADEL13_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(ADEL13_SAMTable, vertex.label = V(ADEL13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
ADEL13_SAM.clusterCoef <- transitivity(ADEL13_SAMTable, type="global") #cluster coefficient
ADEL13_SAM.degreeCent <- centralization.degree(ADEL13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_SAMftn <- as.network.matrix(ADEL13_SAMft)
ADEL13_SAM.netDensity <- network.density(ADEL13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_SAM.entropy <- entropy(ADEL13_SAMft) #entropy

ADEL13_SAM.netMx <- cbind(ADEL13_SAM.netMx, ADEL13_SAM.clusterCoef, ADEL13_SAM.degreeCent$centralization,
                          ADEL13_SAM.netDensity, ADEL13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
ADEL13_TAMg2 <- data.frame(ADEL13_TAM)
ADEL13_TAMg2 <- ADEL13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_TAMg2$player1
player2vector <- ADEL13_TAMg2$player2
ADEL13_TAMg3 <- ADEL13_TAMg2
ADEL13_TAMg3$p1inp2vec <- is.element(ADEL13_TAMg3$player1, player2vector)
ADEL13_TAMg3$p2inp1vec <- is.element(ADEL13_TAMg3$player2, player1vector)

addPlayer1 <- ADEL13_TAMg3[ which(ADEL13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_TAMg3[ which(ADEL13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_TAMg2 <- rbind(ADEL13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
ADEL13_TAMft <- ftable(ADEL13_TAMg2$player1, ADEL13_TAMg2$player2)
ADEL13_TAMft2 <- as.matrix(ADEL13_TAMft)
numRows <- nrow(ADEL13_TAMft2)
numCols <- ncol(ADEL13_TAMft2)
ADEL13_TAMft3 <- ADEL13_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL13_TAMTable <- graph.adjacency(ADEL13_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(ADEL13_TAMTable, vertex.label = V(ADEL13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
ADEL13_TAM.clusterCoef <- transitivity(ADEL13_TAMTable, type="global") #cluster coefficient
ADEL13_TAM.degreeCent <- centralization.degree(ADEL13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_TAMftn <- as.network.matrix(ADEL13_TAMft)
ADEL13_TAM.netDensity <- network.density(ADEL13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_TAM.entropy <- entropy(ADEL13_TAMft) #entropy

ADEL13_TAM.netMx <- cbind(ADEL13_TAM.netMx, ADEL13_TAM.clusterCoef, ADEL13_TAM.degreeCent$centralization,
                          ADEL13_TAM.netDensity, ADEL13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************
#NA

round = 13
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
ADEL13_SDMg2 <- data.frame(ADEL13_SDM)
ADEL13_SDMg2 <- ADEL13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_SDMg2$player1
player2vector <- ADEL13_SDMg2$player2
ADEL13_SDMg3 <- ADEL13_SDMg2
ADEL13_SDMg3$p1inp2vec <- is.element(ADEL13_SDMg3$player1, player2vector)
ADEL13_SDMg3$p2inp1vec <- is.element(ADEL13_SDMg3$player2, player1vector)

addPlayer1 <- ADEL13_SDMg3[ which(ADEL13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_SDMg3[ which(ADEL13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_SDMg2 <- rbind(ADEL13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
ADEL13_SDMft <- ftable(ADEL13_SDMg2$player1, ADEL13_SDMg2$player2)
ADEL13_SDMft2 <- as.matrix(ADEL13_SDMft)
numRows <- nrow(ADEL13_SDMft2)
numCols <- ncol(ADEL13_SDMft2)
ADEL13_SDMft3 <- ADEL13_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL13_SDMTable <- graph.adjacency(ADEL13_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(ADEL13_SDMTable, vertex.label = V(ADEL13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
ADEL13_SDM.clusterCoef <- transitivity(ADEL13_SDMTable, type="global") #cluster coefficient
ADEL13_SDM.degreeCent <- centralization.degree(ADEL13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_SDMftn <- as.network.matrix(ADEL13_SDMft)
ADEL13_SDM.netDensity <- network.density(ADEL13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_SDM.entropy <- entropy(ADEL13_SDMft) #entropy

ADEL13_SDM.netMx <- cbind(ADEL13_SDM.netMx, ADEL13_SDM.clusterCoef, ADEL13_SDM.degreeCent$centralization,
                          ADEL13_SDM.netDensity, ADEL13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
ADEL13_TDMg2 <- data.frame(ADEL13_TDM)
ADEL13_TDMg2 <- ADEL13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_TDMg2$player1
player2vector <- ADEL13_TDMg2$player2
ADEL13_TDMg3 <- ADEL13_TDMg2
ADEL13_TDMg3$p1inp2vec <- is.element(ADEL13_TDMg3$player1, player2vector)
ADEL13_TDMg3$p2inp1vec <- is.element(ADEL13_TDMg3$player2, player1vector)

addPlayer1 <- ADEL13_TDMg3[ which(ADEL13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ADEL13_TDMg3[ which(ADEL13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_TDMg2 <- rbind(ADEL13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
ADEL13_TDMft <- ftable(ADEL13_TDMg2$player1, ADEL13_TDMg2$player2)
ADEL13_TDMft2 <- as.matrix(ADEL13_TDMft)
numRows <- nrow(ADEL13_TDMft2)
numCols <- ncol(ADEL13_TDMft2)
ADEL13_TDMft3 <- ADEL13_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL13_TDMTable <- graph.adjacency(ADEL13_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(ADEL13_TDMTable, vertex.label = V(ADEL13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
ADEL13_TDM.clusterCoef <- transitivity(ADEL13_TDMTable, type="global") #cluster coefficient
ADEL13_TDM.degreeCent <- centralization.degree(ADEL13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_TDMftn <- as.network.matrix(ADEL13_TDMft)
ADEL13_TDM.netDensity <- network.density(ADEL13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_TDM.entropy <- entropy(ADEL13_TDMft) #entropy

ADEL13_TDM.netMx <- cbind(ADEL13_TDM.netMx, ADEL13_TDM.clusterCoef, ADEL13_TDM.degreeCent$centralization,
                          ADEL13_TDM.netDensity, ADEL13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
ADEL13_SDg2 <- data.frame(ADEL13_SD)
ADEL13_SDg2 <- ADEL13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_SDg2$player1
player2vector <- ADEL13_SDg2$player2
ADEL13_SDg3 <- ADEL13_SDg2
ADEL13_SDg3$p1inp2vec <- is.element(ADEL13_SDg3$player1, player2vector)
ADEL13_SDg3$p2inp1vec <- is.element(ADEL13_SDg3$player2, player1vector)

addPlayer1 <- ADEL13_SDg3[ which(ADEL13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_SDg3[ which(ADEL13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_SDg2 <- rbind(ADEL13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
ADEL13_SDft <- ftable(ADEL13_SDg2$player1, ADEL13_SDg2$player2)
ADEL13_SDft2 <- as.matrix(ADEL13_SDft)
numRows <- nrow(ADEL13_SDft2)
numCols <- ncol(ADEL13_SDft2)
ADEL13_SDft3 <- ADEL13_SDft2[c(2:numRows) , c(2:numCols)]
ADEL13_SDTable <- graph.adjacency(ADEL13_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(ADEL13_SDTable, vertex.label = V(ADEL13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
ADEL13_SD.clusterCoef <- transitivity(ADEL13_SDTable, type="global") #cluster coefficient
ADEL13_SD.degreeCent <- centralization.degree(ADEL13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_SDftn <- as.network.matrix(ADEL13_SDft)
ADEL13_SD.netDensity <- network.density(ADEL13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_SD.entropy <- entropy(ADEL13_SDft) #entropy

ADEL13_SD.netMx <- cbind(ADEL13_SD.netMx, ADEL13_SD.clusterCoef, ADEL13_SD.degreeCent$centralization,
                         ADEL13_SD.netDensity, ADEL13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
ADEL13_TDg2 <- data.frame(ADEL13_TD)
ADEL13_TDg2 <- ADEL13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_TDg2$player1
player2vector <- ADEL13_TDg2$player2
ADEL13_TDg3 <- ADEL13_TDg2
ADEL13_TDg3$p1inp2vec <- is.element(ADEL13_TDg3$player1, player2vector)
ADEL13_TDg3$p2inp1vec <- is.element(ADEL13_TDg3$player2, player1vector)

addPlayer1 <- ADEL13_TDg3[ which(ADEL13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_TDg3[ which(ADEL13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_TDg2 <- rbind(ADEL13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
ADEL13_TDft <- ftable(ADEL13_TDg2$player1, ADEL13_TDg2$player2)
ADEL13_TDft2 <- as.matrix(ADEL13_TDft)
numRows <- nrow(ADEL13_TDft2)
numCols <- ncol(ADEL13_TDft2)
ADEL13_TDft3 <- ADEL13_TDft2[c(2:numRows) , c(2:numCols)]
ADEL13_TDTable <- graph.adjacency(ADEL13_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(ADEL13_TDTable, vertex.label = V(ADEL13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
ADEL13_TD.clusterCoef <- transitivity(ADEL13_TDTable, type="global") #cluster coefficient
ADEL13_TD.degreeCent <- centralization.degree(ADEL13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_TDftn <- as.network.matrix(ADEL13_TDft)
ADEL13_TD.netDensity <- network.density(ADEL13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_TD.entropy <- entropy(ADEL13_TDft) #entropy

ADEL13_TD.netMx <- cbind(ADEL13_TD.netMx, ADEL13_TD.clusterCoef, ADEL13_TD.degreeCent$centralization,
                         ADEL13_TD.netDensity, ADEL13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
ADEL13_QTg2 <- data.frame(ADEL13_QT)
ADEL13_QTg2 <- ADEL13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL13_QTg2$player1
player2vector <- ADEL13_QTg2$player2
ADEL13_QTg3 <- ADEL13_QTg2
ADEL13_QTg3$p1inp2vec <- is.element(ADEL13_QTg3$player1, player2vector)
ADEL13_QTg3$p2inp1vec <- is.element(ADEL13_QTg3$player2, player1vector)

addPlayer1 <- ADEL13_QTg3[ which(ADEL13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL13_QTg3[ which(ADEL13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL13_QTg2 <- rbind(ADEL13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
ADEL13_QTft <- ftable(ADEL13_QTg2$player1, ADEL13_QTg2$player2)
ADEL13_QTft2 <- as.matrix(ADEL13_QTft)
numRows <- nrow(ADEL13_QTft2)
numCols <- ncol(ADEL13_QTft2)
ADEL13_QTft3 <- ADEL13_QTft2[c(2:numRows) , c(2:numCols)]
ADEL13_QTTable <- graph.adjacency(ADEL13_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(ADEL13_QTTable, vertex.label = V(ADEL13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
ADEL13_QT.clusterCoef <- transitivity(ADEL13_QTTable, type="global") #cluster coefficient
ADEL13_QT.degreeCent <- centralization.degree(ADEL13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL13_QTftn <- as.network.matrix(ADEL13_QTft)
ADEL13_QT.netDensity <- network.density(ADEL13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL13_QT.entropy <- entropy(ADEL13_QTft) #entropy

ADEL13_QT.netMx <- cbind(ADEL13_QT.netMx, ADEL13_QT.clusterCoef, ADEL13_QT.degreeCent$centralization,
                         ADEL13_QT.netDensity, ADEL13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL13_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************
#NA

round = 13
teamName = "BL"
KIoutcome = "Goal_F"
BL13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
BL13_Gg2 <- data.frame(BL13_G)
BL13_Gg2 <- BL13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_Gg2$player1
player2vector <- BL13_Gg2$player2
BL13_Gg3 <- BL13_Gg2
BL13_Gg3$p1inp2vec <- is.element(BL13_Gg3$player1, player2vector)
BL13_Gg3$p2inp1vec <- is.element(BL13_Gg3$player2, player1vector)

addPlayer1 <- BL13_Gg3[ which(BL13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_Gg3[ which(BL13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_Gg2 <- rbind(BL13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
BL13_Gft <- ftable(BL13_Gg2$player1, BL13_Gg2$player2)
BL13_Gft2 <- as.matrix(BL13_Gft)
numRows <- nrow(BL13_Gft2)
numCols <- ncol(BL13_Gft2)
BL13_Gft3 <- BL13_Gft2[c(2:numRows) , c(2:numCols)]
BL13_GTable <- graph.adjacency(BL13_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(BL13_GTable, vertex.label = V(BL13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
BL13_G.clusterCoef <- transitivity(BL13_GTable, type="global") #cluster coefficient
BL13_G.degreeCent <- centralization.degree(BL13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_Gftn <- as.network.matrix(BL13_Gft)
BL13_G.netDensity <- network.density(BL13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_G.entropy <- entropy(BL13_Gft) #entropy

BL13_G.netMx <- cbind(BL13_G.netMx, BL13_G.clusterCoef, BL13_G.degreeCent$centralization,
                      BL13_G.netDensity, BL13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************
#NA

round = 13
teamName = "BL"
KIoutcome = "Behind_F"
BL13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
BL13_Bg2 <- data.frame(BL13_B)
BL13_Bg2 <- BL13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_Bg2$player1
player2vector <- BL13_Bg2$player2
BL13_Bg3 <- BL13_Bg2
BL13_Bg3$p1inp2vec <- is.element(BL13_Bg3$player1, player2vector)
BL13_Bg3$p2inp1vec <- is.element(BL13_Bg3$player2, player1vector)

addPlayer1 <- BL13_Bg3[ which(BL13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_Bg3[ which(BL13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_Bg2 <- rbind(BL13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
BL13_Bft <- ftable(BL13_Bg2$player1, BL13_Bg2$player2)
BL13_Bft2 <- as.matrix(BL13_Bft)
numRows <- nrow(BL13_Bft2)
numCols <- ncol(BL13_Bft2)
BL13_Bft3 <- BL13_Bft2[c(2:numRows) , c(2:numCols)]
BL13_BTable <- graph.adjacency(BL13_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(BL13_BTable, vertex.label = V(BL13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
BL13_B.clusterCoef <- transitivity(BL13_BTable, type="global") #cluster coefficient
BL13_B.degreeCent <- centralization.degree(BL13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_Bftn <- as.network.matrix(BL13_Bft)
BL13_B.netDensity <- network.density(BL13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_B.entropy <- entropy(BL13_Bft) #entropy

BL13_B.netMx <- cbind(BL13_B.netMx, BL13_B.clusterCoef, BL13_B.degreeCent$centralization,
                      BL13_B.netDensity, BL13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************
#NA

round = 13
teamName = "BL"
KIoutcome = "Stoppage_F"
BL13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
BL13_SFg2 <- data.frame(BL13_SF)
BL13_SFg2 <- BL13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_SFg2$player1
player2vector <- BL13_SFg2$player2
BL13_SFg3 <- BL13_SFg2
BL13_SFg3$p1inp2vec <- is.element(BL13_SFg3$player1, player2vector)
BL13_SFg3$p2inp1vec <- is.element(BL13_SFg3$player2, player1vector)

addPlayer1 <- BL13_SFg3[ which(BL13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_SFg3[ which(BL13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_SFg2 <- rbind(BL13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
BL13_SFft <- ftable(BL13_SFg2$player1, BL13_SFg2$player2)
BL13_SFft2 <- as.matrix(BL13_SFft)
numRows <- nrow(BL13_SFft2)
numCols <- ncol(BL13_SFft2)
BL13_SFft3 <- BL13_SFft2[c(2:numRows) , c(2:numCols)]
BL13_SFTable <- graph.adjacency(BL13_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(BL13_SFTable, vertex.label = V(BL13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
BL13_SF.clusterCoef <- transitivity(BL13_SFTable, type="global") #cluster coefficient
BL13_SF.degreeCent <- centralization.degree(BL13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_SFftn <- as.network.matrix(BL13_SFft)
BL13_SF.netDensity <- network.density(BL13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_SF.entropy <- entropy(BL13_SFft) #entropy

BL13_SF.netMx <- cbind(BL13_SF.netMx, BL13_SF.clusterCoef, BL13_SF.degreeCent$centralization,
                       BL13_SF.netDensity, BL13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************

round = 13
teamName = "BL"
KIoutcome = "Turnover_F"
BL13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
BL13_TFg2 <- data.frame(BL13_TF)
BL13_TFg2 <- BL13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_TFg2$player1
player2vector <- BL13_TFg2$player2
BL13_TFg3 <- BL13_TFg2
BL13_TFg3$p1inp2vec <- is.element(BL13_TFg3$player1, player2vector)
BL13_TFg3$p2inp1vec <- is.element(BL13_TFg3$player2, player1vector)

addPlayer1 <- BL13_TFg3[ which(BL13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_TFg3[ which(BL13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_TFg2 <- rbind(BL13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
BL13_TFft <- ftable(BL13_TFg2$player1, BL13_TFg2$player2)
BL13_TFft2 <- as.matrix(BL13_TFft)
numRows <- nrow(BL13_TFft2)
numCols <- ncol(BL13_TFft2)
BL13_TFft3 <- BL13_TFft2[c(2:numRows) , c(2:numCols)]
BL13_TFTable <- graph.adjacency(BL13_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(BL13_TFTable, vertex.label = V(BL13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
BL13_TF.clusterCoef <- transitivity(BL13_TFTable, type="global") #cluster coefficient
BL13_TF.degreeCent <- centralization.degree(BL13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_TFftn <- as.network.matrix(BL13_TFft)
BL13_TF.netDensity <- network.density(BL13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_TF.entropy <- entropy(BL13_TFft) #entropy

BL13_TF.netMx <- cbind(BL13_TF.netMx, BL13_TF.clusterCoef, BL13_TF.degreeCent$centralization,
                       BL13_TF.netDensity, BL13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************

round = 13
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
BL13_SAMg2 <- data.frame(BL13_SAM)
BL13_SAMg2 <- BL13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_SAMg2$player1
player2vector <- BL13_SAMg2$player2
BL13_SAMg3 <- BL13_SAMg2
BL13_SAMg3$p1inp2vec <- is.element(BL13_SAMg3$player1, player2vector)
BL13_SAMg3$p2inp1vec <- is.element(BL13_SAMg3$player2, player1vector)

addPlayer1 <- BL13_SAMg3[ which(BL13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_SAMg3[ which(BL13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_SAMg2 <- rbind(BL13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
BL13_SAMft <- ftable(BL13_SAMg2$player1, BL13_SAMg2$player2)
BL13_SAMft2 <- as.matrix(BL13_SAMft)
numRows <- nrow(BL13_SAMft2)
numCols <- ncol(BL13_SAMft2)
BL13_SAMft3 <- BL13_SAMft2[c(2:numRows) , c(2:numCols)]
BL13_SAMTable <- graph.adjacency(BL13_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(BL13_SAMTable, vertex.label = V(BL13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
BL13_SAM.clusterCoef <- transitivity(BL13_SAMTable, type="global") #cluster coefficient
BL13_SAM.degreeCent <- centralization.degree(BL13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_SAMftn <- as.network.matrix(BL13_SAMft)
BL13_SAM.netDensity <- network.density(BL13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_SAM.entropy <- entropy(BL13_SAMft) #entropy

BL13_SAM.netMx <- cbind(BL13_SAM.netMx, BL13_SAM.clusterCoef, BL13_SAM.degreeCent$centralization,
                        BL13_SAM.netDensity, BL13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "BL"
KIoutcome = "Turnover_AM"
BL13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
BL13_TAMg2 <- data.frame(BL13_TAM)
BL13_TAMg2 <- BL13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_TAMg2$player1
player2vector <- BL13_TAMg2$player2
BL13_TAMg3 <- BL13_TAMg2
BL13_TAMg3$p1inp2vec <- is.element(BL13_TAMg3$player1, player2vector)
BL13_TAMg3$p2inp1vec <- is.element(BL13_TAMg3$player2, player1vector)

addPlayer1 <- BL13_TAMg3[ which(BL13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_TAMg3[ which(BL13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_TAMg2 <- rbind(BL13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
BL13_TAMft <- ftable(BL13_TAMg2$player1, BL13_TAMg2$player2)
BL13_TAMft2 <- as.matrix(BL13_TAMft)
numRows <- nrow(BL13_TAMft2)
numCols <- ncol(BL13_TAMft2)
BL13_TAMft3 <- BL13_TAMft2[c(2:numRows) , c(2:numCols)]
BL13_TAMTable <- graph.adjacency(BL13_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(BL13_TAMTable, vertex.label = V(BL13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
BL13_TAM.clusterCoef <- transitivity(BL13_TAMTable, type="global") #cluster coefficient
BL13_TAM.degreeCent <- centralization.degree(BL13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_TAMftn <- as.network.matrix(BL13_TAMft)
BL13_TAM.netDensity <- network.density(BL13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_TAM.entropy <- entropy(BL13_TAMft) #entropy

BL13_TAM.netMx <- cbind(BL13_TAM.netMx, BL13_TAM.clusterCoef, BL13_TAM.degreeCent$centralization,
                        BL13_TAM.netDensity, BL13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************

round = 13
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
BL13_SDMg2 <- data.frame(BL13_SDM)
BL13_SDMg2 <- BL13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_SDMg2$player1
player2vector <- BL13_SDMg2$player2
BL13_SDMg3 <- BL13_SDMg2
BL13_SDMg3$p1inp2vec <- is.element(BL13_SDMg3$player1, player2vector)
BL13_SDMg3$p2inp1vec <- is.element(BL13_SDMg3$player2, player1vector)

addPlayer1 <- BL13_SDMg3[ which(BL13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_SDMg3[ which(BL13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_SDMg2 <- rbind(BL13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
BL13_SDMft <- ftable(BL13_SDMg2$player1, BL13_SDMg2$player2)
BL13_SDMft2 <- as.matrix(BL13_SDMft)
numRows <- nrow(BL13_SDMft2)
numCols <- ncol(BL13_SDMft2)
BL13_SDMft3 <- BL13_SDMft2[c(2:numRows) , c(2:numCols)]
BL13_SDMTable <- graph.adjacency(BL13_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(BL13_SDMTable, vertex.label = V(BL13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
BL13_SDM.clusterCoef <- transitivity(BL13_SDMTable, type="global") #cluster coefficient
BL13_SDM.degreeCent <- centralization.degree(BL13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_SDMftn <- as.network.matrix(BL13_SDMft)
BL13_SDM.netDensity <- network.density(BL13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_SDM.entropy <- entropy(BL13_SDMft) #entropy

BL13_SDM.netMx <- cbind(BL13_SDM.netMx, BL13_SDM.clusterCoef, BL13_SDM.degreeCent$centralization,
                        BL13_SDM.netDensity, BL13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "BL"
KIoutcome = "Turnover_DM"
BL13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
BL13_TDMg2 <- data.frame(BL13_TDM)
BL13_TDMg2 <- BL13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_TDMg2$player1
player2vector <- BL13_TDMg2$player2
BL13_TDMg3 <- BL13_TDMg2
BL13_TDMg3$p1inp2vec <- is.element(BL13_TDMg3$player1, player2vector)
BL13_TDMg3$p2inp1vec <- is.element(BL13_TDMg3$player2, player1vector)

addPlayer1 <- BL13_TDMg3[ which(BL13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_TDMg3[ which(BL13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_TDMg2 <- rbind(BL13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
BL13_TDMft <- ftable(BL13_TDMg2$player1, BL13_TDMg2$player2)
BL13_TDMft2 <- as.matrix(BL13_TDMft)
numRows <- nrow(BL13_TDMft2)
numCols <- ncol(BL13_TDMft2)
BL13_TDMft3 <- BL13_TDMft2[c(2:numRows) , c(2:numCols)]
BL13_TDMTable <- graph.adjacency(BL13_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(BL13_TDMTable, vertex.label = V(BL13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
BL13_TDM.clusterCoef <- transitivity(BL13_TDMTable, type="global") #cluster coefficient
BL13_TDM.degreeCent <- centralization.degree(BL13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_TDMftn <- as.network.matrix(BL13_TDMft)
BL13_TDM.netDensity <- network.density(BL13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_TDM.entropy <- entropy(BL13_TDMft) #entropy

BL13_TDM.netMx <- cbind(BL13_TDM.netMx, BL13_TDM.clusterCoef, BL13_TDM.degreeCent$centralization,
                        BL13_TDM.netDensity, BL13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "BL"
KIoutcome = "Stoppage_D"
BL13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
BL13_SDg2 <- data.frame(BL13_SD)
BL13_SDg2 <- BL13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_SDg2$player1
player2vector <- BL13_SDg2$player2
BL13_SDg3 <- BL13_SDg2
BL13_SDg3$p1inp2vec <- is.element(BL13_SDg3$player1, player2vector)
BL13_SDg3$p2inp1vec <- is.element(BL13_SDg3$player2, player1vector)

addPlayer1 <- BL13_SDg3[ which(BL13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_SDg3[ which(BL13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_SDg2 <- rbind(BL13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
BL13_SDft <- ftable(BL13_SDg2$player1, BL13_SDg2$player2)
BL13_SDft2 <- as.matrix(BL13_SDft)
numRows <- nrow(BL13_SDft2)
numCols <- ncol(BL13_SDft2)
BL13_SDft3 <- BL13_SDft2[c(2:numRows) , c(2:numCols)]
BL13_SDTable <- graph.adjacency(BL13_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(BL13_SDTable, vertex.label = V(BL13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
BL13_SD.clusterCoef <- transitivity(BL13_SDTable, type="global") #cluster coefficient
BL13_SD.degreeCent <- centralization.degree(BL13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_SDftn <- as.network.matrix(BL13_SDft)
BL13_SD.netDensity <- network.density(BL13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_SD.entropy <- entropy(BL13_SDft) #entropy

BL13_SD.netMx <- cbind(BL13_SD.netMx, BL13_SD.clusterCoef, BL13_SD.degreeCent$centralization,
                       BL13_SD.netDensity, BL13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "BL"
KIoutcome = "Turnover_D"
BL13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
BL13_TDg2 <- data.frame(BL13_TD)
BL13_TDg2 <- BL13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_TDg2$player1
player2vector <- BL13_TDg2$player2
BL13_TDg3 <- BL13_TDg2
BL13_TDg3$p1inp2vec <- is.element(BL13_TDg3$player1, player2vector)
BL13_TDg3$p2inp1vec <- is.element(BL13_TDg3$player2, player1vector)

addPlayer1 <- BL13_TDg3[ which(BL13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_TDg3[ which(BL13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_TDg2 <- rbind(BL13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
BL13_TDft <- ftable(BL13_TDg2$player1, BL13_TDg2$player2)
BL13_TDft2 <- as.matrix(BL13_TDft)
numRows <- nrow(BL13_TDft2)
numCols <- ncol(BL13_TDft2)
BL13_TDft3 <- BL13_TDft2[c(2:numRows) , c(2:numCols)]
BL13_TDTable <- graph.adjacency(BL13_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(BL13_TDTable, vertex.label = V(BL13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
BL13_TD.clusterCoef <- transitivity(BL13_TDTable, type="global") #cluster coefficient
BL13_TD.degreeCent <- centralization.degree(BL13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_TDftn <- as.network.matrix(BL13_TDft)
BL13_TD.netDensity <- network.density(BL13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_TD.entropy <- entropy(BL13_TDft) #entropy

BL13_TD.netMx <- cbind(BL13_TD.netMx, BL13_TD.clusterCoef, BL13_TD.degreeCent$centralization,
                       BL13_TD.netDensity, BL13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
BL13_QTg2 <- data.frame(BL13_QT)
BL13_QTg2 <- BL13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL13_QTg2$player1
player2vector <- BL13_QTg2$player2
BL13_QTg3 <- BL13_QTg2
BL13_QTg3$p1inp2vec <- is.element(BL13_QTg3$player1, player2vector)
BL13_QTg3$p2inp1vec <- is.element(BL13_QTg3$player2, player1vector)

addPlayer1 <- BL13_QTg3[ which(BL13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL13_QTg3[ which(BL13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL13_QTg2 <- rbind(BL13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
BL13_QTft <- ftable(BL13_QTg2$player1, BL13_QTg2$player2)
BL13_QTft2 <- as.matrix(BL13_QTft)
numRows <- nrow(BL13_QTft2)
numCols <- ncol(BL13_QTft2)
BL13_QTft3 <- BL13_QTft2[c(2:numRows) , c(2:numCols)]
BL13_QTTable <- graph.adjacency(BL13_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(BL13_QTTable, vertex.label = V(BL13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
BL13_QT.clusterCoef <- transitivity(BL13_QTTable, type="global") #cluster coefficient
BL13_QT.degreeCent <- centralization.degree(BL13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL13_QTftn <- as.network.matrix(BL13_QTft)
BL13_QT.netDensity <- network.density(BL13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL13_QT.entropy <- entropy(BL13_QTft) #entropy

BL13_QT.netMx <- cbind(BL13_QT.netMx, BL13_QT.clusterCoef, BL13_QT.degreeCent$centralization,
                       BL13_QT.netDensity, BL13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL13_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************

round = 13
teamName = "CARL"
KIoutcome = "Goal_F"
CARL13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
CARL13_Gg2 <- data.frame(CARL13_G)
CARL13_Gg2 <- CARL13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_Gg2$player1
player2vector <- CARL13_Gg2$player2
CARL13_Gg3 <- CARL13_Gg2
CARL13_Gg3$p1inp2vec <- is.element(CARL13_Gg3$player1, player2vector)
CARL13_Gg3$p2inp1vec <- is.element(CARL13_Gg3$player2, player1vector)

addPlayer1 <- CARL13_Gg3[ which(CARL13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_Gg3[ which(CARL13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_Gg2 <- rbind(CARL13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
CARL13_Gft <- ftable(CARL13_Gg2$player1, CARL13_Gg2$player2)
CARL13_Gft2 <- as.matrix(CARL13_Gft)
numRows <- nrow(CARL13_Gft2)
numCols <- ncol(CARL13_Gft2)
CARL13_Gft3 <- CARL13_Gft2[c(2:numRows) , c(2:numCols)]
CARL13_GTable <- graph.adjacency(CARL13_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(CARL13_GTable, vertex.label = V(CARL13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
CARL13_G.clusterCoef <- transitivity(CARL13_GTable, type="global") #cluster coefficient
CARL13_G.degreeCent <- centralization.degree(CARL13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_Gftn <- as.network.matrix(CARL13_Gft)
CARL13_G.netDensity <- network.density(CARL13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_G.entropy <- entropy(CARL13_Gft) #entropy

CARL13_G.netMx <- cbind(CARL13_G.netMx, CARL13_G.clusterCoef, CARL13_G.degreeCent$centralization,
                        CARL13_G.netDensity, CARL13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************

round = 13
teamName = "CARL"
KIoutcome = "Behind_F"
CARL13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
CARL13_Bg2 <- data.frame(CARL13_B)
CARL13_Bg2 <- CARL13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_Bg2$player1
player2vector <- CARL13_Bg2$player2
CARL13_Bg3 <- CARL13_Bg2
CARL13_Bg3$p1inp2vec <- is.element(CARL13_Bg3$player1, player2vector)
CARL13_Bg3$p2inp1vec <- is.element(CARL13_Bg3$player2, player1vector)

addPlayer1 <- CARL13_Bg3[ which(CARL13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL13_Bg3[ which(CARL13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_Bg2 <- rbind(CARL13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
CARL13_Bft <- ftable(CARL13_Bg2$player1, CARL13_Bg2$player2)
CARL13_Bft2 <- as.matrix(CARL13_Bft)
numRows <- nrow(CARL13_Bft2)
numCols <- ncol(CARL13_Bft2)
CARL13_Bft3 <- CARL13_Bft2[c(2:numRows) , c(2:numCols)]
CARL13_BTable <- graph.adjacency(CARL13_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(CARL13_BTable, vertex.label = V(CARL13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
CARL13_B.clusterCoef <- transitivity(CARL13_BTable, type="global") #cluster coefficient
CARL13_B.degreeCent <- centralization.degree(CARL13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_Bftn <- as.network.matrix(CARL13_Bft)
CARL13_B.netDensity <- network.density(CARL13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_B.entropy <- entropy(CARL13_Bft) #entropy

CARL13_B.netMx <- cbind(CARL13_B.netMx, CARL13_B.clusterCoef, CARL13_B.degreeCent$centralization,
                        CARL13_B.netDensity, CARL13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************

round = 13
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
CARL13_SFg2 <- data.frame(CARL13_SF)
CARL13_SFg2 <- CARL13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_SFg2$player1
player2vector <- CARL13_SFg2$player2
CARL13_SFg3 <- CARL13_SFg2
CARL13_SFg3$p1inp2vec <- is.element(CARL13_SFg3$player1, player2vector)
CARL13_SFg3$p2inp1vec <- is.element(CARL13_SFg3$player2, player1vector)

addPlayer1 <- CARL13_SFg3[ which(CARL13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_SFg3[ which(CARL13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_SFg2 <- rbind(CARL13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
CARL13_SFft <- ftable(CARL13_SFg2$player1, CARL13_SFg2$player2)
CARL13_SFft2 <- as.matrix(CARL13_SFft)
numRows <- nrow(CARL13_SFft2)
numCols <- ncol(CARL13_SFft2)
CARL13_SFft3 <- CARL13_SFft2[c(2:numRows) , c(2:numCols)]
CARL13_SFTable <- graph.adjacency(CARL13_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(CARL13_SFTable, vertex.label = V(CARL13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
CARL13_SF.clusterCoef <- transitivity(CARL13_SFTable, type="global") #cluster coefficient
CARL13_SF.degreeCent <- centralization.degree(CARL13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_SFftn <- as.network.matrix(CARL13_SFft)
CARL13_SF.netDensity <- network.density(CARL13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_SF.entropy <- entropy(CARL13_SFft) #entropy

CARL13_SF.netMx <- cbind(CARL13_SF.netMx, CARL13_SF.clusterCoef, CARL13_SF.degreeCent$centralization,
                         CARL13_SF.netDensity, CARL13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************
#NA

round = 13
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
CARL13_TFg2 <- data.frame(CARL13_TF)
CARL13_TFg2 <- CARL13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_TFg2$player1
player2vector <- CARL13_TFg2$player2
CARL13_TFg3 <- CARL13_TFg2
CARL13_TFg3$p1inp2vec <- is.element(CARL13_TFg3$player1, player2vector)
CARL13_TFg3$p2inp1vec <- is.element(CARL13_TFg3$player2, player1vector)

addPlayer1 <- CARL13_TFg3[ which(CARL13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_TFg3[ which(CARL13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_TFg2 <- rbind(CARL13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
CARL13_TFft <- ftable(CARL13_TFg2$player1, CARL13_TFg2$player2)
CARL13_TFft2 <- as.matrix(CARL13_TFft)
numRows <- nrow(CARL13_TFft2)
numCols <- ncol(CARL13_TFft2)
CARL13_TFft3 <- CARL13_TFft2[c(2:numRows) , c(2:numCols)]
CARL13_TFTable <- graph.adjacency(CARL13_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(CARL13_TFTable, vertex.label = V(CARL13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
CARL13_TF.clusterCoef <- transitivity(CARL13_TFTable, type="global") #cluster coefficient
CARL13_TF.degreeCent <- centralization.degree(CARL13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_TFftn <- as.network.matrix(CARL13_TFft)
CARL13_TF.netDensity <- network.density(CARL13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_TF.entropy <- entropy(CARL13_TFft) #entropy

CARL13_TF.netMx <- cbind(CARL13_TF.netMx, CARL13_TF.clusterCoef, CARL13_TF.degreeCent$centralization,
                         CARL13_TF.netDensity, CARL13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************

round = 13
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
CARL13_SAMg2 <- data.frame(CARL13_SAM)
CARL13_SAMg2 <- CARL13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_SAMg2$player1
player2vector <- CARL13_SAMg2$player2
CARL13_SAMg3 <- CARL13_SAMg2
CARL13_SAMg3$p1inp2vec <- is.element(CARL13_SAMg3$player1, player2vector)
CARL13_SAMg3$p2inp1vec <- is.element(CARL13_SAMg3$player2, player1vector)

addPlayer1 <- CARL13_SAMg3[ which(CARL13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_SAMg3[ which(CARL13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_SAMg2 <- rbind(CARL13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
CARL13_SAMft <- ftable(CARL13_SAMg2$player1, CARL13_SAMg2$player2)
CARL13_SAMft2 <- as.matrix(CARL13_SAMft)
numRows <- nrow(CARL13_SAMft2)
numCols <- ncol(CARL13_SAMft2)
CARL13_SAMft3 <- CARL13_SAMft2[c(2:numRows) , c(2:numCols)]
CARL13_SAMTable <- graph.adjacency(CARL13_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(CARL13_SAMTable, vertex.label = V(CARL13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
CARL13_SAM.clusterCoef <- transitivity(CARL13_SAMTable, type="global") #cluster coefficient
CARL13_SAM.degreeCent <- centralization.degree(CARL13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_SAMftn <- as.network.matrix(CARL13_SAMft)
CARL13_SAM.netDensity <- network.density(CARL13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_SAM.entropy <- entropy(CARL13_SAMft) #entropy

CARL13_SAM.netMx <- cbind(CARL13_SAM.netMx, CARL13_SAM.clusterCoef, CARL13_SAM.degreeCent$centralization,
                          CARL13_SAM.netDensity, CARL13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
CARL13_TAMg2 <- data.frame(CARL13_TAM)
CARL13_TAMg2 <- CARL13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_TAMg2$player1
player2vector <- CARL13_TAMg2$player2
CARL13_TAMg3 <- CARL13_TAMg2
CARL13_TAMg3$p1inp2vec <- is.element(CARL13_TAMg3$player1, player2vector)
CARL13_TAMg3$p2inp1vec <- is.element(CARL13_TAMg3$player2, player1vector)

addPlayer1 <- CARL13_TAMg3[ which(CARL13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_TAMg3[ which(CARL13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_TAMg2 <- rbind(CARL13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
CARL13_TAMft <- ftable(CARL13_TAMg2$player1, CARL13_TAMg2$player2)
CARL13_TAMft2 <- as.matrix(CARL13_TAMft)
numRows <- nrow(CARL13_TAMft2)
numCols <- ncol(CARL13_TAMft2)
CARL13_TAMft3 <- CARL13_TAMft2[c(2:numRows) , c(2:numCols)]
CARL13_TAMTable <- graph.adjacency(CARL13_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(CARL13_TAMTable, vertex.label = V(CARL13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
CARL13_TAM.clusterCoef <- transitivity(CARL13_TAMTable, type="global") #cluster coefficient
CARL13_TAM.degreeCent <- centralization.degree(CARL13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_TAMftn <- as.network.matrix(CARL13_TAMft)
CARL13_TAM.netDensity <- network.density(CARL13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_TAM.entropy <- entropy(CARL13_TAMft) #entropy

CARL13_TAM.netMx <- cbind(CARL13_TAM.netMx, CARL13_TAM.clusterCoef, CARL13_TAM.degreeCent$centralization,
                          CARL13_TAM.netDensity, CARL13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************

round = 13
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
CARL13_SDMg2 <- data.frame(CARL13_SDM)
CARL13_SDMg2 <- CARL13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_SDMg2$player1
player2vector <- CARL13_SDMg2$player2
CARL13_SDMg3 <- CARL13_SDMg2
CARL13_SDMg3$p1inp2vec <- is.element(CARL13_SDMg3$player1, player2vector)
CARL13_SDMg3$p2inp1vec <- is.element(CARL13_SDMg3$player2, player1vector)

addPlayer1 <- CARL13_SDMg3[ which(CARL13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_SDMg3[ which(CARL13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_SDMg2 <- rbind(CARL13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
CARL13_SDMft <- ftable(CARL13_SDMg2$player1, CARL13_SDMg2$player2)
CARL13_SDMft2 <- as.matrix(CARL13_SDMft)
numRows <- nrow(CARL13_SDMft2)
numCols <- ncol(CARL13_SDMft2)
CARL13_SDMft3 <- CARL13_SDMft2[c(2:numRows) , c(2:numCols)]
CARL13_SDMTable <- graph.adjacency(CARL13_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(CARL13_SDMTable, vertex.label = V(CARL13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
CARL13_SDM.clusterCoef <- transitivity(CARL13_SDMTable, type="global") #cluster coefficient
CARL13_SDM.degreeCent <- centralization.degree(CARL13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_SDMftn <- as.network.matrix(CARL13_SDMft)
CARL13_SDM.netDensity <- network.density(CARL13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_SDM.entropy <- entropy(CARL13_SDMft) #entropy

CARL13_SDM.netMx <- cbind(CARL13_SDM.netMx, CARL13_SDM.clusterCoef, CARL13_SDM.degreeCent$centralization,
                          CARL13_SDM.netDensity, CARL13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
CARL13_TDMg2 <- data.frame(CARL13_TDM)
CARL13_TDMg2 <- CARL13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_TDMg2$player1
player2vector <- CARL13_TDMg2$player2
CARL13_TDMg3 <- CARL13_TDMg2
CARL13_TDMg3$p1inp2vec <- is.element(CARL13_TDMg3$player1, player2vector)
CARL13_TDMg3$p2inp1vec <- is.element(CARL13_TDMg3$player2, player1vector)

addPlayer1 <- CARL13_TDMg3[ which(CARL13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_TDMg3[ which(CARL13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_TDMg2 <- rbind(CARL13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
CARL13_TDMft <- ftable(CARL13_TDMg2$player1, CARL13_TDMg2$player2)
CARL13_TDMft2 <- as.matrix(CARL13_TDMft)
numRows <- nrow(CARL13_TDMft2)
numCols <- ncol(CARL13_TDMft2)
CARL13_TDMft3 <- CARL13_TDMft2[c(2:numRows) , c(2:numCols)]
CARL13_TDMTable <- graph.adjacency(CARL13_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(CARL13_TDMTable, vertex.label = V(CARL13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
CARL13_TDM.clusterCoef <- transitivity(CARL13_TDMTable, type="global") #cluster coefficient
CARL13_TDM.degreeCent <- centralization.degree(CARL13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_TDMftn <- as.network.matrix(CARL13_TDMft)
CARL13_TDM.netDensity <- network.density(CARL13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_TDM.entropy <- entropy(CARL13_TDMft) #entropy

CARL13_TDM.netMx <- cbind(CARL13_TDM.netMx, CARL13_TDM.clusterCoef, CARL13_TDM.degreeCent$centralization,
                          CARL13_TDM.netDensity, CARL13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
CARL13_SDg2 <- data.frame(CARL13_SD)
CARL13_SDg2 <- CARL13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_SDg2$player1
player2vector <- CARL13_SDg2$player2
CARL13_SDg3 <- CARL13_SDg2
CARL13_SDg3$p1inp2vec <- is.element(CARL13_SDg3$player1, player2vector)
CARL13_SDg3$p2inp1vec <- is.element(CARL13_SDg3$player2, player1vector)

addPlayer1 <- CARL13_SDg3[ which(CARL13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_SDg3[ which(CARL13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_SDg2 <- rbind(CARL13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
CARL13_SDft <- ftable(CARL13_SDg2$player1, CARL13_SDg2$player2)
CARL13_SDft2 <- as.matrix(CARL13_SDft)
numRows <- nrow(CARL13_SDft2)
numCols <- ncol(CARL13_SDft2)
CARL13_SDft3 <- CARL13_SDft2[c(2:numRows) , c(2:numCols)]
CARL13_SDTable <- graph.adjacency(CARL13_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(CARL13_SDTable, vertex.label = V(CARL13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
CARL13_SD.clusterCoef <- transitivity(CARL13_SDTable, type="global") #cluster coefficient
CARL13_SD.degreeCent <- centralization.degree(CARL13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_SDftn <- as.network.matrix(CARL13_SDft)
CARL13_SD.netDensity <- network.density(CARL13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_SD.entropy <- entropy(CARL13_SDft) #entropy

CARL13_SD.netMx <- cbind(CARL13_SD.netMx, CARL13_SD.clusterCoef, CARL13_SD.degreeCent$centralization,
                         CARL13_SD.netDensity, CARL13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
CARL13_TDg2 <- data.frame(CARL13_TD)
CARL13_TDg2 <- CARL13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_TDg2$player1
player2vector <- CARL13_TDg2$player2
CARL13_TDg3 <- CARL13_TDg2
CARL13_TDg3$p1inp2vec <- is.element(CARL13_TDg3$player1, player2vector)
CARL13_TDg3$p2inp1vec <- is.element(CARL13_TDg3$player2, player1vector)

addPlayer1 <- CARL13_TDg3[ which(CARL13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_TDg3[ which(CARL13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_TDg2 <- rbind(CARL13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
CARL13_TDft <- ftable(CARL13_TDg2$player1, CARL13_TDg2$player2)
CARL13_TDft2 <- as.matrix(CARL13_TDft)
numRows <- nrow(CARL13_TDft2)
numCols <- ncol(CARL13_TDft2)
CARL13_TDft3 <- CARL13_TDft2[c(2:numRows) , c(2:numCols)]
CARL13_TDTable <- graph.adjacency(CARL13_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(CARL13_TDTable, vertex.label = V(CARL13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
CARL13_TD.clusterCoef <- transitivity(CARL13_TDTable, type="global") #cluster coefficient
CARL13_TD.degreeCent <- centralization.degree(CARL13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_TDftn <- as.network.matrix(CARL13_TDft)
CARL13_TD.netDensity <- network.density(CARL13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_TD.entropy <- entropy(CARL13_TDft) #entropy

CARL13_TD.netMx <- cbind(CARL13_TD.netMx, CARL13_TD.clusterCoef, CARL13_TD.degreeCent$centralization,
                         CARL13_TD.netDensity, CARL13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************

round = 13
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
CARL13_QTg2 <- data.frame(CARL13_QT)
CARL13_QTg2 <- CARL13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL13_QTg2$player1
player2vector <- CARL13_QTg2$player2
CARL13_QTg3 <- CARL13_QTg2
CARL13_QTg3$p1inp2vec <- is.element(CARL13_QTg3$player1, player2vector)
CARL13_QTg3$p2inp1vec <- is.element(CARL13_QTg3$player2, player1vector)

addPlayer1 <- CARL13_QTg3[ which(CARL13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL13_QTg3[ which(CARL13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL13_QTg2 <- rbind(CARL13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
CARL13_QTft <- ftable(CARL13_QTg2$player1, CARL13_QTg2$player2)
CARL13_QTft2 <- as.matrix(CARL13_QTft)
numRows <- nrow(CARL13_QTft2)
numCols <- ncol(CARL13_QTft2)
CARL13_QTft3 <- CARL13_QTft2[c(2:numRows) , c(2:numCols)]
CARL13_QTTable <- graph.adjacency(CARL13_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(CARL13_QTTable, vertex.label = V(CARL13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
CARL13_QT.clusterCoef <- transitivity(CARL13_QTTable, type="global") #cluster coefficient
CARL13_QT.degreeCent <- centralization.degree(CARL13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL13_QTftn <- as.network.matrix(CARL13_QTft)
CARL13_QT.netDensity <- network.density(CARL13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL13_QT.entropy <- entropy(CARL13_QTft) #entropy

CARL13_QT.netMx <- cbind(CARL13_QT.netMx, CARL13_QT.clusterCoef, CARL13_QT.degreeCent$centralization,
                         CARL13_QT.netDensity, CARL13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL13_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************
#NA

round = 13
teamName = "COLL"
KIoutcome = "Goal_F"
COLL13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
COLL13_Gg2 <- data.frame(COLL13_G)
COLL13_Gg2 <- COLL13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_Gg2$player1
player2vector <- COLL13_Gg2$player2
COLL13_Gg3 <- COLL13_Gg2
COLL13_Gg3$p1inp2vec <- is.element(COLL13_Gg3$player1, player2vector)
COLL13_Gg3$p2inp1vec <- is.element(COLL13_Gg3$player2, player1vector)

addPlayer1 <- COLL13_Gg3[ which(COLL13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_Gg3[ which(COLL13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_Gg2 <- rbind(COLL13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
COLL13_Gft <- ftable(COLL13_Gg2$player1, COLL13_Gg2$player2)
COLL13_Gft2 <- as.matrix(COLL13_Gft)
numRows <- nrow(COLL13_Gft2)
numCols <- ncol(COLL13_Gft2)
COLL13_Gft3 <- COLL13_Gft2[c(2:numRows) , c(2:numCols)]
COLL13_GTable <- graph.adjacency(COLL13_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(COLL13_GTable, vertex.label = V(COLL13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
COLL13_G.clusterCoef <- transitivity(COLL13_GTable, type="global") #cluster coefficient
COLL13_G.degreeCent <- centralization.degree(COLL13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_Gftn <- as.network.matrix(COLL13_Gft)
COLL13_G.netDensity <- network.density(COLL13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_G.entropy <- entropy(COLL13_Gft) #entropy

COLL13_G.netMx <- cbind(COLL13_G.netMx, COLL13_G.clusterCoef, COLL13_G.degreeCent$centralization,
                        COLL13_G.netDensity, COLL13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************
#NA

round = 13
teamName = "COLL"
KIoutcome = "Behind_F"
COLL13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
COLL13_Bg2 <- data.frame(COLL13_B)
COLL13_Bg2 <- COLL13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_Bg2$player1
player2vector <- COLL13_Bg2$player2
COLL13_Bg3 <- COLL13_Bg2
COLL13_Bg3$p1inp2vec <- is.element(COLL13_Bg3$player1, player2vector)
COLL13_Bg3$p2inp1vec <- is.element(COLL13_Bg3$player2, player1vector)

addPlayer1 <- COLL13_Bg3[ which(COLL13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_Bg3[ which(COLL13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_Bg2 <- rbind(COLL13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
COLL13_Bft <- ftable(COLL13_Bg2$player1, COLL13_Bg2$player2)
COLL13_Bft2 <- as.matrix(COLL13_Bft)
numRows <- nrow(COLL13_Bft2)
numCols <- ncol(COLL13_Bft2)
COLL13_Bft3 <- COLL13_Bft2[c(2:numRows) , c(2:numCols)]
COLL13_BTable <- graph.adjacency(COLL13_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(COLL13_BTable, vertex.label = V(COLL13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
COLL13_B.clusterCoef <- transitivity(COLL13_BTable, type="global") #cluster coefficient
COLL13_B.degreeCent <- centralization.degree(COLL13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_Bftn <- as.network.matrix(COLL13_Bft)
COLL13_B.netDensity <- network.density(COLL13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_B.entropy <- entropy(COLL13_Bft) #entropy

COLL13_B.netMx <- cbind(COLL13_B.netMx, COLL13_B.clusterCoef, COLL13_B.degreeCent$centralization,
                        COLL13_B.netDensity, COLL13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************

round = 13
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
COLL13_SFg2 <- data.frame(COLL13_SF)
COLL13_SFg2 <- COLL13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_SFg2$player1
player2vector <- COLL13_SFg2$player2
COLL13_SFg3 <- COLL13_SFg2
COLL13_SFg3$p1inp2vec <- is.element(COLL13_SFg3$player1, player2vector)
COLL13_SFg3$p2inp1vec <- is.element(COLL13_SFg3$player2, player1vector)

addPlayer1 <- COLL13_SFg3[ which(COLL13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_SFg3[ which(COLL13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_SFg2 <- rbind(COLL13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
COLL13_SFft <- ftable(COLL13_SFg2$player1, COLL13_SFg2$player2)
COLL13_SFft2 <- as.matrix(COLL13_SFft)
numRows <- nrow(COLL13_SFft2)
numCols <- ncol(COLL13_SFft2)
COLL13_SFft3 <- COLL13_SFft2[c(2:numRows) , c(2:numCols)]
COLL13_SFTable <- graph.adjacency(COLL13_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(COLL13_SFTable, vertex.label = V(COLL13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
COLL13_SF.clusterCoef <- transitivity(COLL13_SFTable, type="global") #cluster coefficient
COLL13_SF.degreeCent <- centralization.degree(COLL13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_SFftn <- as.network.matrix(COLL13_SFft)
COLL13_SF.netDensity <- network.density(COLL13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_SF.entropy <- entropy(COLL13_SFft) #entropy

COLL13_SF.netMx <- cbind(COLL13_SF.netMx, COLL13_SF.clusterCoef, COLL13_SF.degreeCent$centralization,
                         COLL13_SF.netDensity, COLL13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************

round = 13
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
COLL13_TFg2 <- data.frame(COLL13_TF)
COLL13_TFg2 <- COLL13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_TFg2$player1
player2vector <- COLL13_TFg2$player2
COLL13_TFg3 <- COLL13_TFg2
COLL13_TFg3$p1inp2vec <- is.element(COLL13_TFg3$player1, player2vector)
COLL13_TFg3$p2inp1vec <- is.element(COLL13_TFg3$player2, player1vector)

addPlayer1 <- COLL13_TFg3[ which(COLL13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_TFg3[ which(COLL13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_TFg2 <- rbind(COLL13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
COLL13_TFft <- ftable(COLL13_TFg2$player1, COLL13_TFg2$player2)
COLL13_TFft2 <- as.matrix(COLL13_TFft)
numRows <- nrow(COLL13_TFft2)
numCols <- ncol(COLL13_TFft2)
COLL13_TFft3 <- COLL13_TFft2[c(2:numRows) , c(2:numCols)]
COLL13_TFTable <- graph.adjacency(COLL13_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(COLL13_TFTable, vertex.label = V(COLL13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
COLL13_TF.clusterCoef <- transitivity(COLL13_TFTable, type="global") #cluster coefficient
COLL13_TF.degreeCent <- centralization.degree(COLL13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_TFftn <- as.network.matrix(COLL13_TFft)
COLL13_TF.netDensity <- network.density(COLL13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_TF.entropy <- entropy(COLL13_TFft) #entropy

COLL13_TF.netMx <- cbind(COLL13_TF.netMx, COLL13_TF.clusterCoef, COLL13_TF.degreeCent$centralization,
                         COLL13_TF.netDensity, COLL13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************
#NA

round = 13
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
COLL13_SAMg2 <- data.frame(COLL13_SAM)
COLL13_SAMg2 <- COLL13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_SAMg2$player1
player2vector <- COLL13_SAMg2$player2
COLL13_SAMg3 <- COLL13_SAMg2
COLL13_SAMg3$p1inp2vec <- is.element(COLL13_SAMg3$player1, player2vector)
COLL13_SAMg3$p2inp1vec <- is.element(COLL13_SAMg3$player2, player1vector)

addPlayer1 <- COLL13_SAMg3[ which(COLL13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_SAMg3[ which(COLL13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_SAMg2 <- rbind(COLL13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
COLL13_SAMft <- ftable(COLL13_SAMg2$player1, COLL13_SAMg2$player2)
COLL13_SAMft2 <- as.matrix(COLL13_SAMft)
numRows <- nrow(COLL13_SAMft2)
numCols <- ncol(COLL13_SAMft2)
COLL13_SAMft3 <- COLL13_SAMft2[c(2:numRows) , c(2:numCols)]
COLL13_SAMTable <- graph.adjacency(COLL13_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(COLL13_SAMTable, vertex.label = V(COLL13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
COLL13_SAM.clusterCoef <- transitivity(COLL13_SAMTable, type="global") #cluster coefficient
COLL13_SAM.degreeCent <- centralization.degree(COLL13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_SAMftn <- as.network.matrix(COLL13_SAMft)
COLL13_SAM.netDensity <- network.density(COLL13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_SAM.entropy <- entropy(COLL13_SAMft) #entropy

COLL13_SAM.netMx <- cbind(COLL13_SAM.netMx, COLL13_SAM.clusterCoef, COLL13_SAM.degreeCent$centralization,
                          COLL13_SAM.netDensity, COLL13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
COLL13_TAMg2 <- data.frame(COLL13_TAM)
COLL13_TAMg2 <- COLL13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_TAMg2$player1
player2vector <- COLL13_TAMg2$player2
COLL13_TAMg3 <- COLL13_TAMg2
COLL13_TAMg3$p1inp2vec <- is.element(COLL13_TAMg3$player1, player2vector)
COLL13_TAMg3$p2inp1vec <- is.element(COLL13_TAMg3$player2, player1vector)

addPlayer1 <- COLL13_TAMg3[ which(COLL13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_TAMg3[ which(COLL13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_TAMg2 <- rbind(COLL13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
COLL13_TAMft <- ftable(COLL13_TAMg2$player1, COLL13_TAMg2$player2)
COLL13_TAMft2 <- as.matrix(COLL13_TAMft)
numRows <- nrow(COLL13_TAMft2)
numCols <- ncol(COLL13_TAMft2)
COLL13_TAMft3 <- COLL13_TAMft2[c(2:numRows) , c(2:numCols)]
COLL13_TAMTable <- graph.adjacency(COLL13_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(COLL13_TAMTable, vertex.label = V(COLL13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
COLL13_TAM.clusterCoef <- transitivity(COLL13_TAMTable, type="global") #cluster coefficient
COLL13_TAM.degreeCent <- centralization.degree(COLL13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_TAMftn <- as.network.matrix(COLL13_TAMft)
COLL13_TAM.netDensity <- network.density(COLL13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_TAM.entropy <- entropy(COLL13_TAMft) #entropy

COLL13_TAM.netMx <- cbind(COLL13_TAM.netMx, COLL13_TAM.clusterCoef, COLL13_TAM.degreeCent$centralization,
                          COLL13_TAM.netDensity, COLL13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************

round = 13
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
COLL13_SDMg2 <- data.frame(COLL13_SDM)
COLL13_SDMg2 <- COLL13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_SDMg2$player1
player2vector <- COLL13_SDMg2$player2
COLL13_SDMg3 <- COLL13_SDMg2
COLL13_SDMg3$p1inp2vec <- is.element(COLL13_SDMg3$player1, player2vector)
COLL13_SDMg3$p2inp1vec <- is.element(COLL13_SDMg3$player2, player1vector)

addPlayer1 <- COLL13_SDMg3[ which(COLL13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_SDMg3[ which(COLL13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_SDMg2 <- rbind(COLL13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
COLL13_SDMft <- ftable(COLL13_SDMg2$player1, COLL13_SDMg2$player2)
COLL13_SDMft2 <- as.matrix(COLL13_SDMft)
numRows <- nrow(COLL13_SDMft2)
numCols <- ncol(COLL13_SDMft2)
COLL13_SDMft3 <- COLL13_SDMft2[c(2:numRows) , c(2:numCols)]
COLL13_SDMTable <- graph.adjacency(COLL13_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(COLL13_SDMTable, vertex.label = V(COLL13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
COLL13_SDM.clusterCoef <- transitivity(COLL13_SDMTable, type="global") #cluster coefficient
COLL13_SDM.degreeCent <- centralization.degree(COLL13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_SDMftn <- as.network.matrix(COLL13_SDMft)
COLL13_SDM.netDensity <- network.density(COLL13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_SDM.entropy <- entropy(COLL13_SDMft) #entropy

COLL13_SDM.netMx <- cbind(COLL13_SDM.netMx, COLL13_SDM.clusterCoef, COLL13_SDM.degreeCent$centralization,
                          COLL13_SDM.netDensity, COLL13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
COLL13_TDMg2 <- data.frame(COLL13_TDM)
COLL13_TDMg2 <- COLL13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_TDMg2$player1
player2vector <- COLL13_TDMg2$player2
COLL13_TDMg3 <- COLL13_TDMg2
COLL13_TDMg3$p1inp2vec <- is.element(COLL13_TDMg3$player1, player2vector)
COLL13_TDMg3$p2inp1vec <- is.element(COLL13_TDMg3$player2, player1vector)

addPlayer1 <- COLL13_TDMg3[ which(COLL13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_TDMg3[ which(COLL13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_TDMg2 <- rbind(COLL13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
COLL13_TDMft <- ftable(COLL13_TDMg2$player1, COLL13_TDMg2$player2)
COLL13_TDMft2 <- as.matrix(COLL13_TDMft)
numRows <- nrow(COLL13_TDMft2)
numCols <- ncol(COLL13_TDMft2)
COLL13_TDMft3 <- COLL13_TDMft2[c(2:numRows) , c(2:numCols)]
COLL13_TDMTable <- graph.adjacency(COLL13_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(COLL13_TDMTable, vertex.label = V(COLL13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
COLL13_TDM.clusterCoef <- transitivity(COLL13_TDMTable, type="global") #cluster coefficient
COLL13_TDM.degreeCent <- centralization.degree(COLL13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_TDMftn <- as.network.matrix(COLL13_TDMft)
COLL13_TDM.netDensity <- network.density(COLL13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_TDM.entropy <- entropy(COLL13_TDMft) #entropy

COLL13_TDM.netMx <- cbind(COLL13_TDM.netMx, COLL13_TDM.clusterCoef, COLL13_TDM.degreeCent$centralization,
                          COLL13_TDM.netDensity, COLL13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************

round = 13
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
COLL13_SDg2 <- data.frame(COLL13_SD)
COLL13_SDg2 <- COLL13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_SDg2$player1
player2vector <- COLL13_SDg2$player2
COLL13_SDg3 <- COLL13_SDg2
COLL13_SDg3$p1inp2vec <- is.element(COLL13_SDg3$player1, player2vector)
COLL13_SDg3$p2inp1vec <- is.element(COLL13_SDg3$player2, player1vector)

addPlayer1 <- COLL13_SDg3[ which(COLL13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_SDg3[ which(COLL13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_SDg2 <- rbind(COLL13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
COLL13_SDft <- ftable(COLL13_SDg2$player1, COLL13_SDg2$player2)
COLL13_SDft2 <- as.matrix(COLL13_SDft)
numRows <- nrow(COLL13_SDft2)
numCols <- ncol(COLL13_SDft2)
COLL13_SDft3 <- COLL13_SDft2[c(2:numRows) , c(2:numCols)]
COLL13_SDTable <- graph.adjacency(COLL13_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(COLL13_SDTable, vertex.label = V(COLL13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
COLL13_SD.clusterCoef <- transitivity(COLL13_SDTable, type="global") #cluster coefficient
COLL13_SD.degreeCent <- centralization.degree(COLL13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_SDftn <- as.network.matrix(COLL13_SDft)
COLL13_SD.netDensity <- network.density(COLL13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_SD.entropy <- entropy(COLL13_SDft) #entropy

COLL13_SD.netMx <- cbind(COLL13_SD.netMx, COLL13_SD.clusterCoef, COLL13_SD.degreeCent$centralization,
                         COLL13_SD.netDensity, COLL13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
COLL13_TDg2 <- data.frame(COLL13_TD)
COLL13_TDg2 <- COLL13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_TDg2$player1
player2vector <- COLL13_TDg2$player2
COLL13_TDg3 <- COLL13_TDg2
COLL13_TDg3$p1inp2vec <- is.element(COLL13_TDg3$player1, player2vector)
COLL13_TDg3$p2inp1vec <- is.element(COLL13_TDg3$player2, player1vector)

addPlayer1 <- COLL13_TDg3[ which(COLL13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_TDg3[ which(COLL13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_TDg2 <- rbind(COLL13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
COLL13_TDft <- ftable(COLL13_TDg2$player1, COLL13_TDg2$player2)
COLL13_TDft2 <- as.matrix(COLL13_TDft)
numRows <- nrow(COLL13_TDft2)
numCols <- ncol(COLL13_TDft2)
COLL13_TDft3 <- COLL13_TDft2[c(2:numRows) , c(2:numCols)]
COLL13_TDTable <- graph.adjacency(COLL13_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(COLL13_TDTable, vertex.label = V(COLL13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
COLL13_TD.clusterCoef <- transitivity(COLL13_TDTable, type="global") #cluster coefficient
COLL13_TD.degreeCent <- centralization.degree(COLL13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_TDftn <- as.network.matrix(COLL13_TDft)
COLL13_TD.netDensity <- network.density(COLL13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_TD.entropy <- entropy(COLL13_TDft) #entropy

COLL13_TD.netMx <- cbind(COLL13_TD.netMx, COLL13_TD.clusterCoef, COLL13_TD.degreeCent$centralization,
                         COLL13_TD.netDensity, COLL13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
COLL13_QTg2 <- data.frame(COLL13_QT)
COLL13_QTg2 <- COLL13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL13_QTg2$player1
player2vector <- COLL13_QTg2$player2
COLL13_QTg3 <- COLL13_QTg2
COLL13_QTg3$p1inp2vec <- is.element(COLL13_QTg3$player1, player2vector)
COLL13_QTg3$p2inp1vec <- is.element(COLL13_QTg3$player2, player1vector)

addPlayer1 <- COLL13_QTg3[ which(COLL13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL13_QTg3[ which(COLL13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL13_QTg2 <- rbind(COLL13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
COLL13_QTft <- ftable(COLL13_QTg2$player1, COLL13_QTg2$player2)
COLL13_QTft2 <- as.matrix(COLL13_QTft)
numRows <- nrow(COLL13_QTft2)
numCols <- ncol(COLL13_QTft2)
COLL13_QTft3 <- COLL13_QTft2[c(2:numRows) , c(2:numCols)]
COLL13_QTTable <- graph.adjacency(COLL13_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(COLL13_QTTable, vertex.label = V(COLL13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
COLL13_QT.clusterCoef <- transitivity(COLL13_QTTable, type="global") #cluster coefficient
COLL13_QT.degreeCent <- centralization.degree(COLL13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL13_QTftn <- as.network.matrix(COLL13_QTft)
COLL13_QT.netDensity <- network.density(COLL13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL13_QT.entropy <- entropy(COLL13_QTft) #entropy

COLL13_QT.netMx <- cbind(COLL13_QT.netMx, COLL13_QT.clusterCoef, COLL13_QT.degreeCent$centralization,
                         COLL13_QT.netDensity, COLL13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL13_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************

round = 13
teamName = "ESS"
KIoutcome = "Goal_F"
ESS13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
ESS13_Gg2 <- data.frame(ESS13_G)
ESS13_Gg2 <- ESS13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_Gg2$player1
player2vector <- ESS13_Gg2$player2
ESS13_Gg3 <- ESS13_Gg2
ESS13_Gg3$p1inp2vec <- is.element(ESS13_Gg3$player1, player2vector)
ESS13_Gg3$p2inp1vec <- is.element(ESS13_Gg3$player2, player1vector)

addPlayer1 <- ESS13_Gg3[ which(ESS13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS13_Gg3[ which(ESS13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_Gg2 <- rbind(ESS13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
ESS13_Gft <- ftable(ESS13_Gg2$player1, ESS13_Gg2$player2)
ESS13_Gft2 <- as.matrix(ESS13_Gft)
numRows <- nrow(ESS13_Gft2)
numCols <- ncol(ESS13_Gft2)
ESS13_Gft3 <- ESS13_Gft2[c(2:numRows) , c(2:numCols)]
ESS13_GTable <- graph.adjacency(ESS13_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(ESS13_GTable, vertex.label = V(ESS13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
ESS13_G.clusterCoef <- transitivity(ESS13_GTable, type="global") #cluster coefficient
ESS13_G.degreeCent <- centralization.degree(ESS13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_Gftn <- as.network.matrix(ESS13_Gft)
ESS13_G.netDensity <- network.density(ESS13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_G.entropy <- entropy(ESS13_Gft) #entropy

ESS13_G.netMx <- cbind(ESS13_G.netMx, ESS13_G.clusterCoef, ESS13_G.degreeCent$centralization,
                       ESS13_G.netDensity, ESS13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************

round = 13
teamName = "ESS"
KIoutcome = "Behind_F"
ESS13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
ESS13_Bg2 <- data.frame(ESS13_B)
ESS13_Bg2 <- ESS13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_Bg2$player1
player2vector <- ESS13_Bg2$player2
ESS13_Bg3 <- ESS13_Bg2
ESS13_Bg3$p1inp2vec <- is.element(ESS13_Bg3$player1, player2vector)
ESS13_Bg3$p2inp1vec <- is.element(ESS13_Bg3$player2, player1vector)

addPlayer1 <- ESS13_Bg3[ which(ESS13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_Bg3[ which(ESS13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_Bg2 <- rbind(ESS13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
ESS13_Bft <- ftable(ESS13_Bg2$player1, ESS13_Bg2$player2)
ESS13_Bft2 <- as.matrix(ESS13_Bft)
numRows <- nrow(ESS13_Bft2)
numCols <- ncol(ESS13_Bft2)
ESS13_Bft3 <- ESS13_Bft2[c(2:numRows) , c(2:numCols)]
ESS13_BTable <- graph.adjacency(ESS13_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(ESS13_BTable, vertex.label = V(ESS13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
ESS13_B.clusterCoef <- transitivity(ESS13_BTable, type="global") #cluster coefficient
ESS13_B.degreeCent <- centralization.degree(ESS13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_Bftn <- as.network.matrix(ESS13_Bft)
ESS13_B.netDensity <- network.density(ESS13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_B.entropy <- entropy(ESS13_Bft) #entropy

ESS13_B.netMx <- cbind(ESS13_B.netMx, ESS13_B.clusterCoef, ESS13_B.degreeCent$centralization,
                       ESS13_B.netDensity, ESS13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************
#NA

round = 13
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
ESS13_SFg2 <- data.frame(ESS13_SF)
ESS13_SFg2 <- ESS13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_SFg2$player1
player2vector <- ESS13_SFg2$player2
ESS13_SFg3 <- ESS13_SFg2
ESS13_SFg3$p1inp2vec <- is.element(ESS13_SFg3$player1, player2vector)
ESS13_SFg3$p2inp1vec <- is.element(ESS13_SFg3$player2, player1vector)

addPlayer1 <- ESS13_SFg3[ which(ESS13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_SFg3[ which(ESS13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_SFg2 <- rbind(ESS13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
ESS13_SFft <- ftable(ESS13_SFg2$player1, ESS13_SFg2$player2)
ESS13_SFft2 <- as.matrix(ESS13_SFft)
numRows <- nrow(ESS13_SFft2)
numCols <- ncol(ESS13_SFft2)
ESS13_SFft3 <- ESS13_SFft2[c(2:numRows) , c(2:numCols)]
ESS13_SFTable <- graph.adjacency(ESS13_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(ESS13_SFTable, vertex.label = V(ESS13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
ESS13_SF.clusterCoef <- transitivity(ESS13_SFTable, type="global") #cluster coefficient
ESS13_SF.degreeCent <- centralization.degree(ESS13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_SFftn <- as.network.matrix(ESS13_SFft)
ESS13_SF.netDensity <- network.density(ESS13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_SF.entropy <- entropy(ESS13_SFft) #entropy

ESS13_SF.netMx <- cbind(ESS13_SF.netMx, ESS13_SF.clusterCoef, ESS13_SF.degreeCent$centralization,
                        ESS13_SF.netDensity, ESS13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************
#NA

round = 13
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
ESS13_TFg2 <- data.frame(ESS13_TF)
ESS13_TFg2 <- ESS13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_TFg2$player1
player2vector <- ESS13_TFg2$player2
ESS13_TFg3 <- ESS13_TFg2
ESS13_TFg3$p1inp2vec <- is.element(ESS13_TFg3$player1, player2vector)
ESS13_TFg3$p2inp1vec <- is.element(ESS13_TFg3$player2, player1vector)

addPlayer1 <- ESS13_TFg3[ which(ESS13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_TFg3[ which(ESS13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_TFg2 <- rbind(ESS13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
ESS13_TFft <- ftable(ESS13_TFg2$player1, ESS13_TFg2$player2)
ESS13_TFft2 <- as.matrix(ESS13_TFft)
numRows <- nrow(ESS13_TFft2)
numCols <- ncol(ESS13_TFft2)
ESS13_TFft3 <- ESS13_TFft2[c(2:numRows) , c(2:numCols)]
ESS13_TFTable <- graph.adjacency(ESS13_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(ESS13_TFTable, vertex.label = V(ESS13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
ESS13_TF.clusterCoef <- transitivity(ESS13_TFTable, type="global") #cluster coefficient
ESS13_TF.degreeCent <- centralization.degree(ESS13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_TFftn <- as.network.matrix(ESS13_TFft)
ESS13_TF.netDensity <- network.density(ESS13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_TF.entropy <- entropy(ESS13_TFft) #entropy

ESS13_TF.netMx <- cbind(ESS13_TF.netMx, ESS13_TF.clusterCoef, ESS13_TF.degreeCent$centralization,
                        ESS13_TF.netDensity, ESS13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************
#NA

round = 13
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
ESS13_SAMg2 <- data.frame(ESS13_SAM)
ESS13_SAMg2 <- ESS13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_SAMg2$player1
player2vector <- ESS13_SAMg2$player2
ESS13_SAMg3 <- ESS13_SAMg2
ESS13_SAMg3$p1inp2vec <- is.element(ESS13_SAMg3$player1, player2vector)
ESS13_SAMg3$p2inp1vec <- is.element(ESS13_SAMg3$player2, player1vector)

addPlayer1 <- ESS13_SAMg3[ which(ESS13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_SAMg3[ which(ESS13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_SAMg2 <- rbind(ESS13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
ESS13_SAMft <- ftable(ESS13_SAMg2$player1, ESS13_SAMg2$player2)
ESS13_SAMft2 <- as.matrix(ESS13_SAMft)
numRows <- nrow(ESS13_SAMft2)
numCols <- ncol(ESS13_SAMft2)
ESS13_SAMft3 <- ESS13_SAMft2[c(2:numRows) , c(2:numCols)]
ESS13_SAMTable <- graph.adjacency(ESS13_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(ESS13_SAMTable, vertex.label = V(ESS13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
ESS13_SAM.clusterCoef <- transitivity(ESS13_SAMTable, type="global") #cluster coefficient
ESS13_SAM.degreeCent <- centralization.degree(ESS13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_SAMftn <- as.network.matrix(ESS13_SAMft)
ESS13_SAM.netDensity <- network.density(ESS13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_SAM.entropy <- entropy(ESS13_SAMft) #entropy

ESS13_SAM.netMx <- cbind(ESS13_SAM.netMx, ESS13_SAM.clusterCoef, ESS13_SAM.degreeCent$centralization,
                         ESS13_SAM.netDensity, ESS13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
ESS13_TAMg2 <- data.frame(ESS13_TAM)
ESS13_TAMg2 <- ESS13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_TAMg2$player1
player2vector <- ESS13_TAMg2$player2
ESS13_TAMg3 <- ESS13_TAMg2
ESS13_TAMg3$p1inp2vec <- is.element(ESS13_TAMg3$player1, player2vector)
ESS13_TAMg3$p2inp1vec <- is.element(ESS13_TAMg3$player2, player1vector)

addPlayer1 <- ESS13_TAMg3[ which(ESS13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_TAMg3[ which(ESS13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_TAMg2 <- rbind(ESS13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
ESS13_TAMft <- ftable(ESS13_TAMg2$player1, ESS13_TAMg2$player2)
ESS13_TAMft2 <- as.matrix(ESS13_TAMft)
numRows <- nrow(ESS13_TAMft2)
numCols <- ncol(ESS13_TAMft2)
ESS13_TAMft3 <- ESS13_TAMft2[c(2:numRows) , c(2:numCols)]
ESS13_TAMTable <- graph.adjacency(ESS13_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(ESS13_TAMTable, vertex.label = V(ESS13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
ESS13_TAM.clusterCoef <- transitivity(ESS13_TAMTable, type="global") #cluster coefficient
ESS13_TAM.degreeCent <- centralization.degree(ESS13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_TAMftn <- as.network.matrix(ESS13_TAMft)
ESS13_TAM.netDensity <- network.density(ESS13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_TAM.entropy <- entropy(ESS13_TAMft) #entropy

ESS13_TAM.netMx <- cbind(ESS13_TAM.netMx, ESS13_TAM.clusterCoef, ESS13_TAM.degreeCent$centralization,
                         ESS13_TAM.netDensity, ESS13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************
#NA

round = 13
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
ESS13_SDMg2 <- data.frame(ESS13_SDM)
ESS13_SDMg2 <- ESS13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_SDMg2$player1
player2vector <- ESS13_SDMg2$player2
ESS13_SDMg3 <- ESS13_SDMg2
ESS13_SDMg3$p1inp2vec <- is.element(ESS13_SDMg3$player1, player2vector)
ESS13_SDMg3$p2inp1vec <- is.element(ESS13_SDMg3$player2, player1vector)

addPlayer1 <- ESS13_SDMg3[ which(ESS13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_SDMg3[ which(ESS13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_SDMg2 <- rbind(ESS13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
ESS13_SDMft <- ftable(ESS13_SDMg2$player1, ESS13_SDMg2$player2)
ESS13_SDMft2 <- as.matrix(ESS13_SDMft)
numRows <- nrow(ESS13_SDMft2)
numCols <- ncol(ESS13_SDMft2)
ESS13_SDMft3 <- ESS13_SDMft2[c(2:numRows) , c(2:numCols)]
ESS13_SDMTable <- graph.adjacency(ESS13_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(ESS13_SDMTable, vertex.label = V(ESS13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
ESS13_SDM.clusterCoef <- transitivity(ESS13_SDMTable, type="global") #cluster coefficient
ESS13_SDM.degreeCent <- centralization.degree(ESS13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_SDMftn <- as.network.matrix(ESS13_SDMft)
ESS13_SDM.netDensity <- network.density(ESS13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_SDM.entropy <- entropy(ESS13_SDMft) #entropy

ESS13_SDM.netMx <- cbind(ESS13_SDM.netMx, ESS13_SDM.clusterCoef, ESS13_SDM.degreeCent$centralization,
                         ESS13_SDM.netDensity, ESS13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
ESS13_TDMg2 <- data.frame(ESS13_TDM)
ESS13_TDMg2 <- ESS13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_TDMg2$player1
player2vector <- ESS13_TDMg2$player2
ESS13_TDMg3 <- ESS13_TDMg2
ESS13_TDMg3$p1inp2vec <- is.element(ESS13_TDMg3$player1, player2vector)
ESS13_TDMg3$p2inp1vec <- is.element(ESS13_TDMg3$player2, player1vector)

addPlayer1 <- ESS13_TDMg3[ which(ESS13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_TDMg3[ which(ESS13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_TDMg2 <- rbind(ESS13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
ESS13_TDMft <- ftable(ESS13_TDMg2$player1, ESS13_TDMg2$player2)
ESS13_TDMft2 <- as.matrix(ESS13_TDMft)
numRows <- nrow(ESS13_TDMft2)
numCols <- ncol(ESS13_TDMft2)
ESS13_TDMft3 <- ESS13_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS13_TDMTable <- graph.adjacency(ESS13_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(ESS13_TDMTable, vertex.label = V(ESS13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
ESS13_TDM.clusterCoef <- transitivity(ESS13_TDMTable, type="global") #cluster coefficient
ESS13_TDM.degreeCent <- centralization.degree(ESS13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_TDMftn <- as.network.matrix(ESS13_TDMft)
ESS13_TDM.netDensity <- network.density(ESS13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_TDM.entropy <- entropy(ESS13_TDMft) #entropy

ESS13_TDM.netMx <- cbind(ESS13_TDM.netMx, ESS13_TDM.clusterCoef, ESS13_TDM.degreeCent$centralization,
                         ESS13_TDM.netDensity, ESS13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
ESS13_SDg2 <- data.frame(ESS13_SD)
ESS13_SDg2 <- ESS13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_SDg2$player1
player2vector <- ESS13_SDg2$player2
ESS13_SDg3 <- ESS13_SDg2
ESS13_SDg3$p1inp2vec <- is.element(ESS13_SDg3$player1, player2vector)
ESS13_SDg3$p2inp1vec <- is.element(ESS13_SDg3$player2, player1vector)

addPlayer1 <- ESS13_SDg3[ which(ESS13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_SDg3[ which(ESS13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_SDg2 <- rbind(ESS13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
ESS13_SDft <- ftable(ESS13_SDg2$player1, ESS13_SDg2$player2)
ESS13_SDft2 <- as.matrix(ESS13_SDft)
numRows <- nrow(ESS13_SDft2)
numCols <- ncol(ESS13_SDft2)
ESS13_SDft3 <- ESS13_SDft2[c(2:numRows) , c(2:numCols)]
ESS13_SDTable <- graph.adjacency(ESS13_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(ESS13_SDTable, vertex.label = V(ESS13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
ESS13_SD.clusterCoef <- transitivity(ESS13_SDTable, type="global") #cluster coefficient
ESS13_SD.degreeCent <- centralization.degree(ESS13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_SDftn <- as.network.matrix(ESS13_SDft)
ESS13_SD.netDensity <- network.density(ESS13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_SD.entropy <- entropy(ESS13_SDft) #entropy

ESS13_SD.netMx <- cbind(ESS13_SD.netMx, ESS13_SD.clusterCoef, ESS13_SD.degreeCent$centralization,
                        ESS13_SD.netDensity, ESS13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************

round = 13
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
ESS13_TDg2 <- data.frame(ESS13_TD)
ESS13_TDg2 <- ESS13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_TDg2$player1
player2vector <- ESS13_TDg2$player2
ESS13_TDg3 <- ESS13_TDg2
ESS13_TDg3$p1inp2vec <- is.element(ESS13_TDg3$player1, player2vector)
ESS13_TDg3$p2inp1vec <- is.element(ESS13_TDg3$player2, player1vector)

addPlayer1 <- ESS13_TDg3[ which(ESS13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_TDg3[ which(ESS13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_TDg2 <- rbind(ESS13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
ESS13_TDft <- ftable(ESS13_TDg2$player1, ESS13_TDg2$player2)
ESS13_TDft2 <- as.matrix(ESS13_TDft)
numRows <- nrow(ESS13_TDft2)
numCols <- ncol(ESS13_TDft2)
ESS13_TDft3 <- ESS13_TDft2[c(2:numRows) , c(2:numCols)]
ESS13_TDTable <- graph.adjacency(ESS13_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(ESS13_TDTable, vertex.label = V(ESS13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
ESS13_TD.clusterCoef <- transitivity(ESS13_TDTable, type="global") #cluster coefficient
ESS13_TD.degreeCent <- centralization.degree(ESS13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_TDftn <- as.network.matrix(ESS13_TDft)
ESS13_TD.netDensity <- network.density(ESS13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_TD.entropy <- entropy(ESS13_TDft) #entropy

ESS13_TD.netMx <- cbind(ESS13_TD.netMx, ESS13_TD.clusterCoef, ESS13_TD.degreeCent$centralization,
                        ESS13_TD.netDensity, ESS13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************

round = 13
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
ESS13_QTg2 <- data.frame(ESS13_QT)
ESS13_QTg2 <- ESS13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS13_QTg2$player1
player2vector <- ESS13_QTg2$player2
ESS13_QTg3 <- ESS13_QTg2
ESS13_QTg3$p1inp2vec <- is.element(ESS13_QTg3$player1, player2vector)
ESS13_QTg3$p2inp1vec <- is.element(ESS13_QTg3$player2, player1vector)

addPlayer1 <- ESS13_QTg3[ which(ESS13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS13_QTg3[ which(ESS13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS13_QTg2 <- rbind(ESS13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
ESS13_QTft <- ftable(ESS13_QTg2$player1, ESS13_QTg2$player2)
ESS13_QTft2 <- as.matrix(ESS13_QTft)
numRows <- nrow(ESS13_QTft2)
numCols <- ncol(ESS13_QTft2)
ESS13_QTft3 <- ESS13_QTft2[c(2:numRows) , c(2:numCols)]
ESS13_QTTable <- graph.adjacency(ESS13_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(ESS13_QTTable, vertex.label = V(ESS13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
ESS13_QT.clusterCoef <- transitivity(ESS13_QTTable, type="global") #cluster coefficient
ESS13_QT.degreeCent <- centralization.degree(ESS13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS13_QTftn <- as.network.matrix(ESS13_QTft)
ESS13_QT.netDensity <- network.density(ESS13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS13_QT.entropy <- entropy(ESS13_QTft) #entropy

ESS13_QT.netMx <- cbind(ESS13_QT.netMx, ESS13_QT.clusterCoef, ESS13_QT.degreeCent$centralization,
                        ESS13_QT.netDensity, ESS13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS13_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************

round = 13
teamName = "FRE"
KIoutcome = "Goal_F"
FRE13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
FRE13_Gg2 <- data.frame(FRE13_G)
FRE13_Gg2 <- FRE13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_Gg2$player1
player2vector <- FRE13_Gg2$player2
FRE13_Gg3 <- FRE13_Gg2
FRE13_Gg3$p1inp2vec <- is.element(FRE13_Gg3$player1, player2vector)
FRE13_Gg3$p2inp1vec <- is.element(FRE13_Gg3$player2, player1vector)

addPlayer1 <- FRE13_Gg3[ which(FRE13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_Gg3[ which(FRE13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_Gg2 <- rbind(FRE13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
FRE13_Gft <- ftable(FRE13_Gg2$player1, FRE13_Gg2$player2)
FRE13_Gft2 <- as.matrix(FRE13_Gft)
numRows <- nrow(FRE13_Gft2)
numCols <- ncol(FRE13_Gft2)
FRE13_Gft3 <- FRE13_Gft2[c(2:numRows) , c(2:numCols)]
FRE13_GTable <- graph.adjacency(FRE13_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(FRE13_GTable, vertex.label = V(FRE13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
FRE13_G.clusterCoef <- transitivity(FRE13_GTable, type="global") #cluster coefficient
FRE13_G.degreeCent <- centralization.degree(FRE13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_Gftn <- as.network.matrix(FRE13_Gft)
FRE13_G.netDensity <- network.density(FRE13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_G.entropy <- entropy(FRE13_Gft) #entropy

FRE13_G.netMx <- cbind(FRE13_G.netMx, FRE13_G.clusterCoef, FRE13_G.degreeCent$centralization,
                       FRE13_G.netDensity, FRE13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************
#NA

round = 13
teamName = "FRE"
KIoutcome = "Behind_F"
FRE13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
FRE13_Bg2 <- data.frame(FRE13_B)
FRE13_Bg2 <- FRE13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_Bg2$player1
player2vector <- FRE13_Bg2$player2
FRE13_Bg3 <- FRE13_Bg2
FRE13_Bg3$p1inp2vec <- is.element(FRE13_Bg3$player1, player2vector)
FRE13_Bg3$p2inp1vec <- is.element(FRE13_Bg3$player2, player1vector)

addPlayer1 <- FRE13_Bg3[ which(FRE13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_Bg3[ which(FRE13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_Bg2 <- rbind(FRE13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
FRE13_Bft <- ftable(FRE13_Bg2$player1, FRE13_Bg2$player2)
FRE13_Bft2 <- as.matrix(FRE13_Bft)
numRows <- nrow(FRE13_Bft2)
numCols <- ncol(FRE13_Bft2)
FRE13_Bft3 <- FRE13_Bft2[c(2:numRows) , c(2:numCols)]
FRE13_BTable <- graph.adjacency(FRE13_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(FRE13_BTable, vertex.label = V(FRE13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
FRE13_B.clusterCoef <- transitivity(FRE13_BTable, type="global") #cluster coefficient
FRE13_B.degreeCent <- centralization.degree(FRE13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_Bftn <- as.network.matrix(FRE13_Bft)
FRE13_B.netDensity <- network.density(FRE13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_B.entropy <- entropy(FRE13_Bft) #entropy

FRE13_B.netMx <- cbind(FRE13_B.netMx, FRE13_B.clusterCoef, FRE13_B.degreeCent$centralization,
                       FRE13_B.netDensity, FRE13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************
#NA

round = 13
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
FRE13_SFg2 <- data.frame(FRE13_SF)
FRE13_SFg2 <- FRE13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_SFg2$player1
player2vector <- FRE13_SFg2$player2
FRE13_SFg3 <- FRE13_SFg2
FRE13_SFg3$p1inp2vec <- is.element(FRE13_SFg3$player1, player2vector)
FRE13_SFg3$p2inp1vec <- is.element(FRE13_SFg3$player2, player1vector)

addPlayer1 <- FRE13_SFg3[ which(FRE13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_SFg3[ which(FRE13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_SFg2 <- rbind(FRE13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
FRE13_SFft <- ftable(FRE13_SFg2$player1, FRE13_SFg2$player2)
FRE13_SFft2 <- as.matrix(FRE13_SFft)
numRows <- nrow(FRE13_SFft2)
numCols <- ncol(FRE13_SFft2)
FRE13_SFft3 <- FRE13_SFft2[c(2:numRows) , c(2:numCols)]
FRE13_SFTable <- graph.adjacency(FRE13_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(FRE13_SFTable, vertex.label = V(FRE13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
FRE13_SF.clusterCoef <- transitivity(FRE13_SFTable, type="global") #cluster coefficient
FRE13_SF.degreeCent <- centralization.degree(FRE13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_SFftn <- as.network.matrix(FRE13_SFft)
FRE13_SF.netDensity <- network.density(FRE13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_SF.entropy <- entropy(FRE13_SFft) #entropy

FRE13_SF.netMx <- cbind(FRE13_SF.netMx, FRE13_SF.clusterCoef, FRE13_SF.degreeCent$centralization,
                        FRE13_SF.netDensity, FRE13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************
#NA

round = 13
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
FRE13_TFg2 <- data.frame(FRE13_TF)
FRE13_TFg2 <- FRE13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_TFg2$player1
player2vector <- FRE13_TFg2$player2
FRE13_TFg3 <- FRE13_TFg2
FRE13_TFg3$p1inp2vec <- is.element(FRE13_TFg3$player1, player2vector)
FRE13_TFg3$p2inp1vec <- is.element(FRE13_TFg3$player2, player1vector)

addPlayer1 <- FRE13_TFg3[ which(FRE13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_TFg3[ which(FRE13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_TFg2 <- rbind(FRE13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
FRE13_TFft <- ftable(FRE13_TFg2$player1, FRE13_TFg2$player2)
FRE13_TFft2 <- as.matrix(FRE13_TFft)
numRows <- nrow(FRE13_TFft2)
numCols <- ncol(FRE13_TFft2)
FRE13_TFft3 <- FRE13_TFft2[c(2:numRows) , c(2:numCols)]
FRE13_TFTable <- graph.adjacency(FRE13_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(FRE13_TFTable, vertex.label = V(FRE13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
FRE13_TF.clusterCoef <- transitivity(FRE13_TFTable, type="global") #cluster coefficient
FRE13_TF.degreeCent <- centralization.degree(FRE13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_TFftn <- as.network.matrix(FRE13_TFft)
FRE13_TF.netDensity <- network.density(FRE13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_TF.entropy <- entropy(FRE13_TFft) #entropy

FRE13_TF.netMx <- cbind(FRE13_TF.netMx, FRE13_TF.clusterCoef, FRE13_TF.degreeCent$centralization,
                        FRE13_TF.netDensity, FRE13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************
#NA

round = 13
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
FRE13_SAMg2 <- data.frame(FRE13_SAM)
FRE13_SAMg2 <- FRE13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_SAMg2$player1
player2vector <- FRE13_SAMg2$player2
FRE13_SAMg3 <- FRE13_SAMg2
FRE13_SAMg3$p1inp2vec <- is.element(FRE13_SAMg3$player1, player2vector)
FRE13_SAMg3$p2inp1vec <- is.element(FRE13_SAMg3$player2, player1vector)

addPlayer1 <- FRE13_SAMg3[ which(FRE13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_SAMg3[ which(FRE13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_SAMg2 <- rbind(FRE13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
FRE13_SAMft <- ftable(FRE13_SAMg2$player1, FRE13_SAMg2$player2)
FRE13_SAMft2 <- as.matrix(FRE13_SAMft)
numRows <- nrow(FRE13_SAMft2)
numCols <- ncol(FRE13_SAMft2)
FRE13_SAMft3 <- FRE13_SAMft2[c(2:numRows) , c(2:numCols)]
FRE13_SAMTable <- graph.adjacency(FRE13_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(FRE13_SAMTable, vertex.label = V(FRE13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
FRE13_SAM.clusterCoef <- transitivity(FRE13_SAMTable, type="global") #cluster coefficient
FRE13_SAM.degreeCent <- centralization.degree(FRE13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_SAMftn <- as.network.matrix(FRE13_SAMft)
FRE13_SAM.netDensity <- network.density(FRE13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_SAM.entropy <- entropy(FRE13_SAMft) #entropy

FRE13_SAM.netMx <- cbind(FRE13_SAM.netMx, FRE13_SAM.clusterCoef, FRE13_SAM.degreeCent$centralization,
                         FRE13_SAM.netDensity, FRE13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************
#NA

round = 13
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
FRE13_TAMg2 <- data.frame(FRE13_TAM)
FRE13_TAMg2 <- FRE13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_TAMg2$player1
player2vector <- FRE13_TAMg2$player2
FRE13_TAMg3 <- FRE13_TAMg2
FRE13_TAMg3$p1inp2vec <- is.element(FRE13_TAMg3$player1, player2vector)
FRE13_TAMg3$p2inp1vec <- is.element(FRE13_TAMg3$player2, player1vector)

addPlayer1 <- FRE13_TAMg3[ which(FRE13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_TAMg3[ which(FRE13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_TAMg2 <- rbind(FRE13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
FRE13_TAMft <- ftable(FRE13_TAMg2$player1, FRE13_TAMg2$player2)
FRE13_TAMft2 <- as.matrix(FRE13_TAMft)
numRows <- nrow(FRE13_TAMft2)
numCols <- ncol(FRE13_TAMft2)
FRE13_TAMft3 <- FRE13_TAMft2[c(2:numRows) , c(2:numCols)]
FRE13_TAMTable <- graph.adjacency(FRE13_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(FRE13_TAMTable, vertex.label = V(FRE13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
FRE13_TAM.clusterCoef <- transitivity(FRE13_TAMTable, type="global") #cluster coefficient
FRE13_TAM.degreeCent <- centralization.degree(FRE13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_TAMftn <- as.network.matrix(FRE13_TAMft)
FRE13_TAM.netDensity <- network.density(FRE13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_TAM.entropy <- entropy(FRE13_TAMft) #entropy

FRE13_TAM.netMx <- cbind(FRE13_TAM.netMx, FRE13_TAM.clusterCoef, FRE13_TAM.degreeCent$centralization,
                         FRE13_TAM.netDensity, FRE13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************

round = 13
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
FRE13_SDMg2 <- data.frame(FRE13_SDM)
FRE13_SDMg2 <- FRE13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_SDMg2$player1
player2vector <- FRE13_SDMg2$player2
FRE13_SDMg3 <- FRE13_SDMg2
FRE13_SDMg3$p1inp2vec <- is.element(FRE13_SDMg3$player1, player2vector)
FRE13_SDMg3$p2inp1vec <- is.element(FRE13_SDMg3$player2, player1vector)

addPlayer1 <- FRE13_SDMg3[ which(FRE13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_SDMg3[ which(FRE13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_SDMg2 <- rbind(FRE13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
FRE13_SDMft <- ftable(FRE13_SDMg2$player1, FRE13_SDMg2$player2)
FRE13_SDMft2 <- as.matrix(FRE13_SDMft)
numRows <- nrow(FRE13_SDMft2)
numCols <- ncol(FRE13_SDMft2)
FRE13_SDMft3 <- FRE13_SDMft2[c(2:numRows) , c(2:numCols)]
FRE13_SDMTable <- graph.adjacency(FRE13_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(FRE13_SDMTable, vertex.label = V(FRE13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
FRE13_SDM.clusterCoef <- transitivity(FRE13_SDMTable, type="global") #cluster coefficient
FRE13_SDM.degreeCent <- centralization.degree(FRE13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_SDMftn <- as.network.matrix(FRE13_SDMft)
FRE13_SDM.netDensity <- network.density(FRE13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_SDM.entropy <- entropy(FRE13_SDMft) #entropy

FRE13_SDM.netMx <- cbind(FRE13_SDM.netMx, FRE13_SDM.clusterCoef, FRE13_SDM.degreeCent$centralization,
                         FRE13_SDM.netDensity, FRE13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************
#NA

round = 13
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
FRE13_TDMg2 <- data.frame(FRE13_TDM)
FRE13_TDMg2 <- FRE13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_TDMg2$player1
player2vector <- FRE13_TDMg2$player2
FRE13_TDMg3 <- FRE13_TDMg2
FRE13_TDMg3$p1inp2vec <- is.element(FRE13_TDMg3$player1, player2vector)
FRE13_TDMg3$p2inp1vec <- is.element(FRE13_TDMg3$player2, player1vector)

addPlayer1 <- FRE13_TDMg3[ which(FRE13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_TDMg3[ which(FRE13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_TDMg2 <- rbind(FRE13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
FRE13_TDMft <- ftable(FRE13_TDMg2$player1, FRE13_TDMg2$player2)
FRE13_TDMft2 <- as.matrix(FRE13_TDMft)
numRows <- nrow(FRE13_TDMft2)
numCols <- ncol(FRE13_TDMft2)
FRE13_TDMft3 <- FRE13_TDMft2[c(2:numRows) , c(2:numCols)]
FRE13_TDMTable <- graph.adjacency(FRE13_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(FRE13_TDMTable, vertex.label = V(FRE13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
FRE13_TDM.clusterCoef <- transitivity(FRE13_TDMTable, type="global") #cluster coefficient
FRE13_TDM.degreeCent <- centralization.degree(FRE13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_TDMftn <- as.network.matrix(FRE13_TDMft)
FRE13_TDM.netDensity <- network.density(FRE13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_TDM.entropy <- entropy(FRE13_TDMft) #entropy

FRE13_TDM.netMx <- cbind(FRE13_TDM.netMx, FRE13_TDM.clusterCoef, FRE13_TDM.degreeCent$centralization,
                         FRE13_TDM.netDensity, FRE13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
FRE13_SDg2 <- data.frame(FRE13_SD)
FRE13_SDg2 <- FRE13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_SDg2$player1
player2vector <- FRE13_SDg2$player2
FRE13_SDg3 <- FRE13_SDg2
FRE13_SDg3$p1inp2vec <- is.element(FRE13_SDg3$player1, player2vector)
FRE13_SDg3$p2inp1vec <- is.element(FRE13_SDg3$player2, player1vector)

addPlayer1 <- FRE13_SDg3[ which(FRE13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_SDg3[ which(FRE13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_SDg2 <- rbind(FRE13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
FRE13_SDft <- ftable(FRE13_SDg2$player1, FRE13_SDg2$player2)
FRE13_SDft2 <- as.matrix(FRE13_SDft)
numRows <- nrow(FRE13_SDft2)
numCols <- ncol(FRE13_SDft2)
FRE13_SDft3 <- FRE13_SDft2[c(2:numRows) , c(2:numCols)]
FRE13_SDTable <- graph.adjacency(FRE13_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(FRE13_SDTable, vertex.label = V(FRE13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
FRE13_SD.clusterCoef <- transitivity(FRE13_SDTable, type="global") #cluster coefficient
FRE13_SD.degreeCent <- centralization.degree(FRE13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_SDftn <- as.network.matrix(FRE13_SDft)
FRE13_SD.netDensity <- network.density(FRE13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_SD.entropy <- entropy(FRE13_SDft) #entropy

FRE13_SD.netMx <- cbind(FRE13_SD.netMx, FRE13_SD.clusterCoef, FRE13_SD.degreeCent$centralization,
                        FRE13_SD.netDensity, FRE13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
FRE13_TDg2 <- data.frame(FRE13_TD)
FRE13_TDg2 <- FRE13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_TDg2$player1
player2vector <- FRE13_TDg2$player2
FRE13_TDg3 <- FRE13_TDg2
FRE13_TDg3$p1inp2vec <- is.element(FRE13_TDg3$player1, player2vector)
FRE13_TDg3$p2inp1vec <- is.element(FRE13_TDg3$player2, player1vector)

addPlayer1 <- FRE13_TDg3[ which(FRE13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_TDg3[ which(FRE13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_TDg2 <- rbind(FRE13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
FRE13_TDft <- ftable(FRE13_TDg2$player1, FRE13_TDg2$player2)
FRE13_TDft2 <- as.matrix(FRE13_TDft)
numRows <- nrow(FRE13_TDft2)
numCols <- ncol(FRE13_TDft2)
FRE13_TDft3 <- FRE13_TDft2[c(2:numRows) , c(2:numCols)]
FRE13_TDTable <- graph.adjacency(FRE13_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(FRE13_TDTable, vertex.label = V(FRE13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
FRE13_TD.clusterCoef <- transitivity(FRE13_TDTable, type="global") #cluster coefficient
FRE13_TD.degreeCent <- centralization.degree(FRE13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_TDftn <- as.network.matrix(FRE13_TDft)
FRE13_TD.netDensity <- network.density(FRE13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_TD.entropy <- entropy(FRE13_TDft) #entropy

FRE13_TD.netMx <- cbind(FRE13_TD.netMx, FRE13_TD.clusterCoef, FRE13_TD.degreeCent$centralization,
                        FRE13_TD.netDensity, FRE13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
FRE13_QTg2 <- data.frame(FRE13_QT)
FRE13_QTg2 <- FRE13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE13_QTg2$player1
player2vector <- FRE13_QTg2$player2
FRE13_QTg3 <- FRE13_QTg2
FRE13_QTg3$p1inp2vec <- is.element(FRE13_QTg3$player1, player2vector)
FRE13_QTg3$p2inp1vec <- is.element(FRE13_QTg3$player2, player1vector)

addPlayer1 <- FRE13_QTg3[ which(FRE13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE13_QTg3[ which(FRE13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE13_QTg2 <- rbind(FRE13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
FRE13_QTft <- ftable(FRE13_QTg2$player1, FRE13_QTg2$player2)
FRE13_QTft2 <- as.matrix(FRE13_QTft)
numRows <- nrow(FRE13_QTft2)
numCols <- ncol(FRE13_QTft2)
FRE13_QTft3 <- FRE13_QTft2[c(2:numRows) , c(2:numCols)]
FRE13_QTTable <- graph.adjacency(FRE13_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(FRE13_QTTable, vertex.label = V(FRE13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
FRE13_QT.clusterCoef <- transitivity(FRE13_QTTable, type="global") #cluster coefficient
FRE13_QT.degreeCent <- centralization.degree(FRE13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE13_QTftn <- as.network.matrix(FRE13_QTft)
FRE13_QT.netDensity <- network.density(FRE13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE13_QT.entropy <- entropy(FRE13_QTft) #entropy

FRE13_QT.netMx <- cbind(FRE13_QT.netMx, FRE13_QT.clusterCoef, FRE13_QT.degreeCent$centralization,
                        FRE13_QT.netDensity, FRE13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE13_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************
#NA

round = 13
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
GCFC13_Gg2 <- data.frame(GCFC13_G)
GCFC13_Gg2 <- GCFC13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_Gg2$player1
player2vector <- GCFC13_Gg2$player2
GCFC13_Gg3 <- GCFC13_Gg2
GCFC13_Gg3$p1inp2vec <- is.element(GCFC13_Gg3$player1, player2vector)
GCFC13_Gg3$p2inp1vec <- is.element(GCFC13_Gg3$player2, player1vector)

addPlayer1 <- GCFC13_Gg3[ which(GCFC13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_Gg3[ which(GCFC13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_Gg2 <- rbind(GCFC13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
GCFC13_Gft <- ftable(GCFC13_Gg2$player1, GCFC13_Gg2$player2)
GCFC13_Gft2 <- as.matrix(GCFC13_Gft)
numRows <- nrow(GCFC13_Gft2)
numCols <- ncol(GCFC13_Gft2)
GCFC13_Gft3 <- GCFC13_Gft2[c(2:numRows) , c(2:numCols)]
GCFC13_GTable <- graph.adjacency(GCFC13_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(GCFC13_GTable, vertex.label = V(GCFC13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
GCFC13_G.clusterCoef <- transitivity(GCFC13_GTable, type="global") #cluster coefficient
GCFC13_G.degreeCent <- centralization.degree(GCFC13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_Gftn <- as.network.matrix(GCFC13_Gft)
GCFC13_G.netDensity <- network.density(GCFC13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_G.entropy <- entropy(GCFC13_Gft) #entropy

GCFC13_G.netMx <- cbind(GCFC13_G.netMx, GCFC13_G.clusterCoef, GCFC13_G.degreeCent$centralization,
                        GCFC13_G.netDensity, GCFC13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************

round = 13
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
GCFC13_Bg2 <- data.frame(GCFC13_B)
GCFC13_Bg2 <- GCFC13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_Bg2$player1
player2vector <- GCFC13_Bg2$player2
GCFC13_Bg3 <- GCFC13_Bg2
GCFC13_Bg3$p1inp2vec <- is.element(GCFC13_Bg3$player1, player2vector)
GCFC13_Bg3$p2inp1vec <- is.element(GCFC13_Bg3$player2, player1vector)

addPlayer1 <- GCFC13_Bg3[ which(GCFC13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_Bg3[ which(GCFC13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_Bg2 <- rbind(GCFC13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
GCFC13_Bft <- ftable(GCFC13_Bg2$player1, GCFC13_Bg2$player2)
GCFC13_Bft2 <- as.matrix(GCFC13_Bft)
numRows <- nrow(GCFC13_Bft2)
numCols <- ncol(GCFC13_Bft2)
GCFC13_Bft3 <- GCFC13_Bft2[c(2:numRows) , c(2:numCols)]
GCFC13_BTable <- graph.adjacency(GCFC13_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(GCFC13_BTable, vertex.label = V(GCFC13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
GCFC13_B.clusterCoef <- transitivity(GCFC13_BTable, type="global") #cluster coefficient
GCFC13_B.degreeCent <- centralization.degree(GCFC13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_Bftn <- as.network.matrix(GCFC13_Bft)
GCFC13_B.netDensity <- network.density(GCFC13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_B.entropy <- entropy(GCFC13_Bft) #entropy

GCFC13_B.netMx <- cbind(GCFC13_B.netMx, GCFC13_B.clusterCoef, GCFC13_B.degreeCent$centralization,
                        GCFC13_B.netDensity, GCFC13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************
#NA

round = 13
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
GCFC13_SFg2 <- data.frame(GCFC13_SF)
GCFC13_SFg2 <- GCFC13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_SFg2$player1
player2vector <- GCFC13_SFg2$player2
GCFC13_SFg3 <- GCFC13_SFg2
GCFC13_SFg3$p1inp2vec <- is.element(GCFC13_SFg3$player1, player2vector)
GCFC13_SFg3$p2inp1vec <- is.element(GCFC13_SFg3$player2, player1vector)

addPlayer1 <- GCFC13_SFg3[ which(GCFC13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC13_SFg2 <- rbind(GCFC13_SFg2, addPlayer1)

#ROUND 13, FWD Stoppage graph using weighted edges
GCFC13_SFft <- ftable(GCFC13_SFg2$player1, GCFC13_SFg2$player2)
GCFC13_SFft2 <- as.matrix(GCFC13_SFft)
numRows <- nrow(GCFC13_SFft2)
numCols <- ncol(GCFC13_SFft2)
GCFC13_SFft3 <- GCFC13_SFft2[c(2:numRows) , c(1:numCols)]
GCFC13_SFTable <- graph.adjacency(GCFC13_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(GCFC13_SFTable, vertex.label = V(GCFC13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
GCFC13_SF.clusterCoef <- transitivity(GCFC13_SFTable, type="global") #cluster coefficient
GCFC13_SF.degreeCent <- centralization.degree(GCFC13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_SFftn <- as.network.matrix(GCFC13_SFft)
GCFC13_SF.netDensity <- network.density(GCFC13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_SF.entropy <- entropy(GCFC13_SFft) #entropy

GCFC13_SF.netMx <- cbind(GCFC13_SF.netMx, GCFC13_SF.clusterCoef, GCFC13_SF.degreeCent$centralization,
                         GCFC13_SF.netDensity, GCFC13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************

round = 13
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
GCFC13_TFg2 <- data.frame(GCFC13_TF)
GCFC13_TFg2 <- GCFC13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_TFg2$player1
player2vector <- GCFC13_TFg2$player2
GCFC13_TFg3 <- GCFC13_TFg2
GCFC13_TFg3$p1inp2vec <- is.element(GCFC13_TFg3$player1, player2vector)
GCFC13_TFg3$p2inp1vec <- is.element(GCFC13_TFg3$player2, player1vector)

addPlayer1 <- GCFC13_TFg3[ which(GCFC13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_TFg3[ which(GCFC13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_TFg2 <- rbind(GCFC13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
GCFC13_TFft <- ftable(GCFC13_TFg2$player1, GCFC13_TFg2$player2)
GCFC13_TFft2 <- as.matrix(GCFC13_TFft)
numRows <- nrow(GCFC13_TFft2)
numCols <- ncol(GCFC13_TFft2)
GCFC13_TFft3 <- GCFC13_TFft2[c(2:numRows) , c(2:numCols)]
GCFC13_TFTable <- graph.adjacency(GCFC13_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(GCFC13_TFTable, vertex.label = V(GCFC13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
GCFC13_TF.clusterCoef <- transitivity(GCFC13_TFTable, type="global") #cluster coefficient
GCFC13_TF.degreeCent <- centralization.degree(GCFC13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_TFftn <- as.network.matrix(GCFC13_TFft)
GCFC13_TF.netDensity <- network.density(GCFC13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_TF.entropy <- entropy(GCFC13_TFft) #entropy

GCFC13_TF.netMx <- cbind(GCFC13_TF.netMx, GCFC13_TF.clusterCoef, GCFC13_TF.degreeCent$centralization,
                         GCFC13_TF.netDensity, GCFC13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************
#NA

round = 13
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
GCFC13_SAMg2 <- data.frame(GCFC13_SAM)
GCFC13_SAMg2 <- GCFC13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_SAMg2$player1
player2vector <- GCFC13_SAMg2$player2
GCFC13_SAMg3 <- GCFC13_SAMg2
GCFC13_SAMg3$p1inp2vec <- is.element(GCFC13_SAMg3$player1, player2vector)
GCFC13_SAMg3$p2inp1vec <- is.element(GCFC13_SAMg3$player2, player1vector)

addPlayer1 <- GCFC13_SAMg3[ which(GCFC13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_SAMg3[ which(GCFC13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_SAMg2 <- rbind(GCFC13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
GCFC13_SAMft <- ftable(GCFC13_SAMg2$player1, GCFC13_SAMg2$player2)
GCFC13_SAMft2 <- as.matrix(GCFC13_SAMft)
numRows <- nrow(GCFC13_SAMft2)
numCols <- ncol(GCFC13_SAMft2)
GCFC13_SAMft3 <- GCFC13_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC13_SAMTable <- graph.adjacency(GCFC13_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(GCFC13_SAMTable, vertex.label = V(GCFC13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
GCFC13_SAM.clusterCoef <- transitivity(GCFC13_SAMTable, type="global") #cluster coefficient
GCFC13_SAM.degreeCent <- centralization.degree(GCFC13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_SAMftn <- as.network.matrix(GCFC13_SAMft)
GCFC13_SAM.netDensity <- network.density(GCFC13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_SAM.entropy <- entropy(GCFC13_SAMft) #entropy

GCFC13_SAM.netMx <- cbind(GCFC13_SAM.netMx, GCFC13_SAM.clusterCoef, GCFC13_SAM.degreeCent$centralization,
                          GCFC13_SAM.netDensity, GCFC13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
GCFC13_TAMg2 <- data.frame(GCFC13_TAM)
GCFC13_TAMg2 <- GCFC13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_TAMg2$player1
player2vector <- GCFC13_TAMg2$player2
GCFC13_TAMg3 <- GCFC13_TAMg2
GCFC13_TAMg3$p1inp2vec <- is.element(GCFC13_TAMg3$player1, player2vector)
GCFC13_TAMg3$p2inp1vec <- is.element(GCFC13_TAMg3$player2, player1vector)

addPlayer1 <- GCFC13_TAMg3[ which(GCFC13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_TAMg3[ which(GCFC13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_TAMg2 <- rbind(GCFC13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
GCFC13_TAMft <- ftable(GCFC13_TAMg2$player1, GCFC13_TAMg2$player2)
GCFC13_TAMft2 <- as.matrix(GCFC13_TAMft)
numRows <- nrow(GCFC13_TAMft2)
numCols <- ncol(GCFC13_TAMft2)
GCFC13_TAMft3 <- GCFC13_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC13_TAMTable <- graph.adjacency(GCFC13_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(GCFC13_TAMTable, vertex.label = V(GCFC13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
GCFC13_TAM.clusterCoef <- transitivity(GCFC13_TAMTable, type="global") #cluster coefficient
GCFC13_TAM.degreeCent <- centralization.degree(GCFC13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_TAMftn <- as.network.matrix(GCFC13_TAMft)
GCFC13_TAM.netDensity <- network.density(GCFC13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_TAM.entropy <- entropy(GCFC13_TAMft) #entropy

GCFC13_TAM.netMx <- cbind(GCFC13_TAM.netMx, GCFC13_TAM.clusterCoef, GCFC13_TAM.degreeCent$centralization,
                          GCFC13_TAM.netDensity, GCFC13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************

round = 13
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
GCFC13_SDMg2 <- data.frame(GCFC13_SDM)
GCFC13_SDMg2 <- GCFC13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_SDMg2$player1
player2vector <- GCFC13_SDMg2$player2
GCFC13_SDMg3 <- GCFC13_SDMg2
GCFC13_SDMg3$p1inp2vec <- is.element(GCFC13_SDMg3$player1, player2vector)
GCFC13_SDMg3$p2inp1vec <- is.element(GCFC13_SDMg3$player2, player1vector)

addPlayer1 <- GCFC13_SDMg3[ which(GCFC13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_SDMg3[ which(GCFC13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_SDMg2 <- rbind(GCFC13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
GCFC13_SDMft <- ftable(GCFC13_SDMg2$player1, GCFC13_SDMg2$player2)
GCFC13_SDMft2 <- as.matrix(GCFC13_SDMft)
numRows <- nrow(GCFC13_SDMft2)
numCols <- ncol(GCFC13_SDMft2)
GCFC13_SDMft3 <- GCFC13_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC13_SDMTable <- graph.adjacency(GCFC13_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(GCFC13_SDMTable, vertex.label = V(GCFC13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
GCFC13_SDM.clusterCoef <- transitivity(GCFC13_SDMTable, type="global") #cluster coefficient
GCFC13_SDM.degreeCent <- centralization.degree(GCFC13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_SDMftn <- as.network.matrix(GCFC13_SDMft)
GCFC13_SDM.netDensity <- network.density(GCFC13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_SDM.entropy <- entropy(GCFC13_SDMft) #entropy

GCFC13_SDM.netMx <- cbind(GCFC13_SDM.netMx, GCFC13_SDM.clusterCoef, GCFC13_SDM.degreeCent$centralization,
                          GCFC13_SDM.netDensity, GCFC13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
GCFC13_TDMg2 <- data.frame(GCFC13_TDM)
GCFC13_TDMg2 <- GCFC13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_TDMg2$player1
player2vector <- GCFC13_TDMg2$player2
GCFC13_TDMg3 <- GCFC13_TDMg2
GCFC13_TDMg3$p1inp2vec <- is.element(GCFC13_TDMg3$player1, player2vector)
GCFC13_TDMg3$p2inp1vec <- is.element(GCFC13_TDMg3$player2, player1vector)

addPlayer1 <- GCFC13_TDMg3[ which(GCFC13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_TDMg3[ which(GCFC13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_TDMg2 <- rbind(GCFC13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
GCFC13_TDMft <- ftable(GCFC13_TDMg2$player1, GCFC13_TDMg2$player2)
GCFC13_TDMft2 <- as.matrix(GCFC13_TDMft)
numRows <- nrow(GCFC13_TDMft2)
numCols <- ncol(GCFC13_TDMft2)
GCFC13_TDMft3 <- GCFC13_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC13_TDMTable <- graph.adjacency(GCFC13_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(GCFC13_TDMTable, vertex.label = V(GCFC13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
GCFC13_TDM.clusterCoef <- transitivity(GCFC13_TDMTable, type="global") #cluster coefficient
GCFC13_TDM.degreeCent <- centralization.degree(GCFC13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_TDMftn <- as.network.matrix(GCFC13_TDMft)
GCFC13_TDM.netDensity <- network.density(GCFC13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_TDM.entropy <- entropy(GCFC13_TDMft) #entropy

GCFC13_TDM.netMx <- cbind(GCFC13_TDM.netMx, GCFC13_TDM.clusterCoef, GCFC13_TDM.degreeCent$centralization,
                          GCFC13_TDM.netDensity, GCFC13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
GCFC13_SDg2 <- data.frame(GCFC13_SD)
GCFC13_SDg2 <- GCFC13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_SDg2$player1
player2vector <- GCFC13_SDg2$player2
GCFC13_SDg3 <- GCFC13_SDg2
GCFC13_SDg3$p1inp2vec <- is.element(GCFC13_SDg3$player1, player2vector)
GCFC13_SDg3$p2inp1vec <- is.element(GCFC13_SDg3$player2, player1vector)

addPlayer1 <- GCFC13_SDg3[ which(GCFC13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_SDg3[ which(GCFC13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_SDg2 <- rbind(GCFC13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
GCFC13_SDft <- ftable(GCFC13_SDg2$player1, GCFC13_SDg2$player2)
GCFC13_SDft2 <- as.matrix(GCFC13_SDft)
numRows <- nrow(GCFC13_SDft2)
numCols <- ncol(GCFC13_SDft2)
GCFC13_SDft3 <- GCFC13_SDft2[c(2:numRows) , c(2:numCols)]
GCFC13_SDTable <- graph.adjacency(GCFC13_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(GCFC13_SDTable, vertex.label = V(GCFC13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
GCFC13_SD.clusterCoef <- transitivity(GCFC13_SDTable, type="global") #cluster coefficient
GCFC13_SD.degreeCent <- centralization.degree(GCFC13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_SDftn <- as.network.matrix(GCFC13_SDft)
GCFC13_SD.netDensity <- network.density(GCFC13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_SD.entropy <- entropy(GCFC13_SDft) #entropy

GCFC13_SD.netMx <- cbind(GCFC13_SD.netMx, GCFC13_SD.clusterCoef, GCFC13_SD.degreeCent$centralization,
                         GCFC13_SD.netDensity, GCFC13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
GCFC13_TDg2 <- data.frame(GCFC13_TD)
GCFC13_TDg2 <- GCFC13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_TDg2$player1
player2vector <- GCFC13_TDg2$player2
GCFC13_TDg3 <- GCFC13_TDg2
GCFC13_TDg3$p1inp2vec <- is.element(GCFC13_TDg3$player1, player2vector)
GCFC13_TDg3$p2inp1vec <- is.element(GCFC13_TDg3$player2, player1vector)

addPlayer1 <- GCFC13_TDg3[ which(GCFC13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_TDg3[ which(GCFC13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_TDg2 <- rbind(GCFC13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
GCFC13_TDft <- ftable(GCFC13_TDg2$player1, GCFC13_TDg2$player2)
GCFC13_TDft2 <- as.matrix(GCFC13_TDft)
numRows <- nrow(GCFC13_TDft2)
numCols <- ncol(GCFC13_TDft2)
GCFC13_TDft3 <- GCFC13_TDft2[c(2:numRows) , c(2:numCols)]
GCFC13_TDTable <- graph.adjacency(GCFC13_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(GCFC13_TDTable, vertex.label = V(GCFC13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
GCFC13_TD.clusterCoef <- transitivity(GCFC13_TDTable, type="global") #cluster coefficient
GCFC13_TD.degreeCent <- centralization.degree(GCFC13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_TDftn <- as.network.matrix(GCFC13_TDft)
GCFC13_TD.netDensity <- network.density(GCFC13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_TD.entropy <- entropy(GCFC13_TDft) #entropy

GCFC13_TD.netMx <- cbind(GCFC13_TD.netMx, GCFC13_TD.clusterCoef, GCFC13_TD.degreeCent$centralization,
                         GCFC13_TD.netDensity, GCFC13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
GCFC13_QTg2 <- data.frame(GCFC13_QT)
GCFC13_QTg2 <- GCFC13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC13_QTg2$player1
player2vector <- GCFC13_QTg2$player2
GCFC13_QTg3 <- GCFC13_QTg2
GCFC13_QTg3$p1inp2vec <- is.element(GCFC13_QTg3$player1, player2vector)
GCFC13_QTg3$p2inp1vec <- is.element(GCFC13_QTg3$player2, player1vector)

addPlayer1 <- GCFC13_QTg3[ which(GCFC13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC13_QTg3[ which(GCFC13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC13_QTg2 <- rbind(GCFC13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
GCFC13_QTft <- ftable(GCFC13_QTg2$player1, GCFC13_QTg2$player2)
GCFC13_QTft2 <- as.matrix(GCFC13_QTft)
numRows <- nrow(GCFC13_QTft2)
numCols <- ncol(GCFC13_QTft2)
GCFC13_QTft3 <- GCFC13_QTft2[c(2:numRows) , c(2:numCols)]
GCFC13_QTTable <- graph.adjacency(GCFC13_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(GCFC13_QTTable, vertex.label = V(GCFC13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
GCFC13_QT.clusterCoef <- transitivity(GCFC13_QTTable, type="global") #cluster coefficient
GCFC13_QT.degreeCent <- centralization.degree(GCFC13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC13_QTftn <- as.network.matrix(GCFC13_QTft)
GCFC13_QT.netDensity <- network.density(GCFC13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC13_QT.entropy <- entropy(GCFC13_QTft) #entropy

GCFC13_QT.netMx <- cbind(GCFC13_QT.netMx, GCFC13_QT.clusterCoef, GCFC13_QT.degreeCent$centralization,
                         GCFC13_QT.netDensity, GCFC13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC13_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************

round = 13
teamName = "HAW"
KIoutcome = "Goal_F"
HAW13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
HAW13_Gg2 <- data.frame(HAW13_G)
HAW13_Gg2 <- HAW13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_Gg2$player1
player2vector <- HAW13_Gg2$player2
HAW13_Gg3 <- HAW13_Gg2
HAW13_Gg3$p1inp2vec <- is.element(HAW13_Gg3$player1, player2vector)
HAW13_Gg3$p2inp1vec <- is.element(HAW13_Gg3$player2, player1vector)

addPlayer1 <- HAW13_Gg3[ which(HAW13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_Gg3[ which(HAW13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_Gg2 <- rbind(HAW13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
HAW13_Gft <- ftable(HAW13_Gg2$player1, HAW13_Gg2$player2)
HAW13_Gft2 <- as.matrix(HAW13_Gft)
numRows <- nrow(HAW13_Gft2)
numCols <- ncol(HAW13_Gft2)
HAW13_Gft3 <- HAW13_Gft2[c(2:numRows) , c(2:numCols)]
HAW13_GTable <- graph.adjacency(HAW13_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(HAW13_GTable, vertex.label = V(HAW13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
HAW13_G.clusterCoef <- transitivity(HAW13_GTable, type="global") #cluster coefficient
HAW13_G.degreeCent <- centralization.degree(HAW13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_Gftn <- as.network.matrix(HAW13_Gft)
HAW13_G.netDensity <- network.density(HAW13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_G.entropy <- entropy(HAW13_Gft) #entropy

HAW13_G.netMx <- cbind(HAW13_G.netMx, HAW13_G.clusterCoef, HAW13_G.degreeCent$centralization,
                       HAW13_G.netDensity, HAW13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************
#NA

round = 13
teamName = "HAW"
KIoutcome = "Behind_F"
HAW13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
HAW13_Bg2 <- data.frame(HAW13_B)
HAW13_Bg2 <- HAW13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_Bg2$player1
player2vector <- HAW13_Bg2$player2
HAW13_Bg3 <- HAW13_Bg2
HAW13_Bg3$p1inp2vec <- is.element(HAW13_Bg3$player1, player2vector)
HAW13_Bg3$p2inp1vec <- is.element(HAW13_Bg3$player2, player1vector)

addPlayer1 <- HAW13_Bg3[ which(HAW13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW13_Bg2 <- rbind(HAW13_Bg2, addPlayer1)

#ROUND 13, Behind graph using weighted edges
HAW13_Bft <- ftable(HAW13_Bg2$player1, HAW13_Bg2$player2)
HAW13_Bft2 <- as.matrix(HAW13_Bft)
numRows <- nrow(HAW13_Bft2)
numCols <- ncol(HAW13_Bft2)
HAW13_Bft3 <- HAW13_Bft2[c(2:numRows) , c(1:numCols)]
HAW13_BTable <- graph.adjacency(HAW13_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(HAW13_BTable, vertex.label = V(HAW13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
HAW13_B.clusterCoef <- transitivity(HAW13_BTable, type="global") #cluster coefficient
HAW13_B.degreeCent <- centralization.degree(HAW13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_Bftn <- as.network.matrix(HAW13_Bft)
HAW13_B.netDensity <- network.density(HAW13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_B.entropy <- entropy(HAW13_Bft) #entropy

HAW13_B.netMx <- cbind(HAW13_B.netMx, HAW13_B.clusterCoef, HAW13_B.degreeCent$centralization,
                       HAW13_B.netDensity, HAW13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************
#NA

round = 13
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
HAW13_SFg2 <- data.frame(HAW13_SF)
HAW13_SFg2 <- HAW13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_SFg2$player1
player2vector <- HAW13_SFg2$player2
HAW13_SFg3 <- HAW13_SFg2
HAW13_SFg3$p1inp2vec <- is.element(HAW13_SFg3$player1, player2vector)
HAW13_SFg3$p2inp1vec <- is.element(HAW13_SFg3$player2, player1vector)

addPlayer1 <- HAW13_SFg3[ which(HAW13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_SFg3[ which(HAW13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_SFg2 <- rbind(HAW13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
HAW13_SFft <- ftable(HAW13_SFg2$player1, HAW13_SFg2$player2)
HAW13_SFft2 <- as.matrix(HAW13_SFft)
numRows <- nrow(HAW13_SFft2)
numCols <- ncol(HAW13_SFft2)
HAW13_SFft3 <- HAW13_SFft2[c(2:numRows) , c(2:numCols)]
HAW13_SFTable <- graph.adjacency(HAW13_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(HAW13_SFTable, vertex.label = V(HAW13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
HAW13_SF.clusterCoef <- transitivity(HAW13_SFTable, type="global") #cluster coefficient
HAW13_SF.degreeCent <- centralization.degree(HAW13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_SFftn <- as.network.matrix(HAW13_SFft)
HAW13_SF.netDensity <- network.density(HAW13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_SF.entropy <- entropy(HAW13_SFft) #entropy

HAW13_SF.netMx <- cbind(HAW13_SF.netMx, HAW13_SF.clusterCoef, HAW13_SF.degreeCent$centralization,
                        HAW13_SF.netDensity, HAW13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************

round = 13
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
HAW13_TFg2 <- data.frame(HAW13_TF)
HAW13_TFg2 <- HAW13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_TFg2$player1
player2vector <- HAW13_TFg2$player2
HAW13_TFg3 <- HAW13_TFg2
HAW13_TFg3$p1inp2vec <- is.element(HAW13_TFg3$player1, player2vector)
HAW13_TFg3$p2inp1vec <- is.element(HAW13_TFg3$player2, player1vector)

addPlayer1 <- HAW13_TFg3[ which(HAW13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_TFg3[ which(HAW13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_TFg2 <- rbind(HAW13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
HAW13_TFft <- ftable(HAW13_TFg2$player1, HAW13_TFg2$player2)
HAW13_TFft2 <- as.matrix(HAW13_TFft)
numRows <- nrow(HAW13_TFft2)
numCols <- ncol(HAW13_TFft2)
HAW13_TFft3 <- HAW13_TFft2[c(2:numRows) , c(2:numCols)]
HAW13_TFTable <- graph.adjacency(HAW13_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(HAW13_TFTable, vertex.label = V(HAW13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
HAW13_TF.clusterCoef <- transitivity(HAW13_TFTable, type="global") #cluster coefficient
HAW13_TF.degreeCent <- centralization.degree(HAW13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_TFftn <- as.network.matrix(HAW13_TFft)
HAW13_TF.netDensity <- network.density(HAW13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_TF.entropy <- entropy(HAW13_TFft) #entropy

HAW13_TF.netMx <- cbind(HAW13_TF.netMx, HAW13_TF.clusterCoef, HAW13_TF.degreeCent$centralization,
                        HAW13_TF.netDensity, HAW13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************

round = 13
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
HAW13_SAMg2 <- data.frame(HAW13_SAM)
HAW13_SAMg2 <- HAW13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_SAMg2$player1
player2vector <- HAW13_SAMg2$player2
HAW13_SAMg3 <- HAW13_SAMg2
HAW13_SAMg3$p1inp2vec <- is.element(HAW13_SAMg3$player1, player2vector)
HAW13_SAMg3$p2inp1vec <- is.element(HAW13_SAMg3$player2, player1vector)

addPlayer1 <- HAW13_SAMg3[ which(HAW13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_SAMg3[ which(HAW13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_SAMg2 <- rbind(HAW13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
HAW13_SAMft <- ftable(HAW13_SAMg2$player1, HAW13_SAMg2$player2)
HAW13_SAMft2 <- as.matrix(HAW13_SAMft)
numRows <- nrow(HAW13_SAMft2)
numCols <- ncol(HAW13_SAMft2)
HAW13_SAMft3 <- HAW13_SAMft2[c(2:numRows) , c(2:numCols)]
HAW13_SAMTable <- graph.adjacency(HAW13_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(HAW13_SAMTable, vertex.label = V(HAW13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
HAW13_SAM.clusterCoef <- transitivity(HAW13_SAMTable, type="global") #cluster coefficient
HAW13_SAM.degreeCent <- centralization.degree(HAW13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_SAMftn <- as.network.matrix(HAW13_SAMft)
HAW13_SAM.netDensity <- network.density(HAW13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_SAM.entropy <- entropy(HAW13_SAMft) #entropy

HAW13_SAM.netMx <- cbind(HAW13_SAM.netMx, HAW13_SAM.clusterCoef, HAW13_SAM.degreeCent$centralization,
                         HAW13_SAM.netDensity, HAW13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************
#NA

round = 13
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
HAW13_TAMg2 <- data.frame(HAW13_TAM)
HAW13_TAMg2 <- HAW13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_TAMg2$player1
player2vector <- HAW13_TAMg2$player2
HAW13_TAMg3 <- HAW13_TAMg2
HAW13_TAMg3$p1inp2vec <- is.element(HAW13_TAMg3$player1, player2vector)
HAW13_TAMg3$p2inp1vec <- is.element(HAW13_TAMg3$player2, player1vector)

addPlayer1 <- HAW13_TAMg3[ which(HAW13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_TAMg3[ which(HAW13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_TAMg2 <- rbind(HAW13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
HAW13_TAMft <- ftable(HAW13_TAMg2$player1, HAW13_TAMg2$player2)
HAW13_TAMft2 <- as.matrix(HAW13_TAMft)
numRows <- nrow(HAW13_TAMft2)
numCols <- ncol(HAW13_TAMft2)
HAW13_TAMft3 <- HAW13_TAMft2[c(2:numRows) , c(2:numCols)]
HAW13_TAMTable <- graph.adjacency(HAW13_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(HAW13_TAMTable, vertex.label = V(HAW13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
HAW13_TAM.clusterCoef <- transitivity(HAW13_TAMTable, type="global") #cluster coefficient
HAW13_TAM.degreeCent <- centralization.degree(HAW13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_TAMftn <- as.network.matrix(HAW13_TAMft)
HAW13_TAM.netDensity <- network.density(HAW13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_TAM.entropy <- entropy(HAW13_TAMft) #entropy

HAW13_TAM.netMx <- cbind(HAW13_TAM.netMx, HAW13_TAM.clusterCoef, HAW13_TAM.degreeCent$centralization,
                         HAW13_TAM.netDensity, HAW13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************
#NA

round = 13
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
HAW13_SDMg2 <- data.frame(HAW13_SDM)
HAW13_SDMg2 <- HAW13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_SDMg2$player1
player2vector <- HAW13_SDMg2$player2
HAW13_SDMg3 <- HAW13_SDMg2
HAW13_SDMg3$p1inp2vec <- is.element(HAW13_SDMg3$player1, player2vector)
HAW13_SDMg3$p2inp1vec <- is.element(HAW13_SDMg3$player2, player1vector)

addPlayer1 <- HAW13_SDMg3[ which(HAW13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_SDMg3[ which(HAW13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_SDMg2 <- rbind(HAW13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
HAW13_SDMft <- ftable(HAW13_SDMg2$player1, HAW13_SDMg2$player2)
HAW13_SDMft2 <- as.matrix(HAW13_SDMft)
numRows <- nrow(HAW13_SDMft2)
numCols <- ncol(HAW13_SDMft2)
HAW13_SDMft3 <- HAW13_SDMft2[c(2:numRows) , c(2:numCols)]
HAW13_SDMTable <- graph.adjacency(HAW13_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(HAW13_SDMTable, vertex.label = V(HAW13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
HAW13_SDM.clusterCoef <- transitivity(HAW13_SDMTable, type="global") #cluster coefficient
HAW13_SDM.degreeCent <- centralization.degree(HAW13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_SDMftn <- as.network.matrix(HAW13_SDMft)
HAW13_SDM.netDensity <- network.density(HAW13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_SDM.entropy <- entropy(HAW13_SDMft) #entropy

HAW13_SDM.netMx <- cbind(HAW13_SDM.netMx, HAW13_SDM.clusterCoef, HAW13_SDM.degreeCent$centralization,
                         HAW13_SDM.netDensity, HAW13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
HAW13_TDMg2 <- data.frame(HAW13_TDM)
HAW13_TDMg2 <- HAW13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_TDMg2$player1
player2vector <- HAW13_TDMg2$player2
HAW13_TDMg3 <- HAW13_TDMg2
HAW13_TDMg3$p1inp2vec <- is.element(HAW13_TDMg3$player1, player2vector)
HAW13_TDMg3$p2inp1vec <- is.element(HAW13_TDMg3$player2, player1vector)

addPlayer1 <- HAW13_TDMg3[ which(HAW13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_TDMg3[ which(HAW13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_TDMg2 <- rbind(HAW13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
HAW13_TDMft <- ftable(HAW13_TDMg2$player1, HAW13_TDMg2$player2)
HAW13_TDMft2 <- as.matrix(HAW13_TDMft)
numRows <- nrow(HAW13_TDMft2)
numCols <- ncol(HAW13_TDMft2)
HAW13_TDMft3 <- HAW13_TDMft2[c(2:numRows) , c(2:numCols)]
HAW13_TDMTable <- graph.adjacency(HAW13_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(HAW13_TDMTable, vertex.label = V(HAW13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
HAW13_TDM.clusterCoef <- transitivity(HAW13_TDMTable, type="global") #cluster coefficient
HAW13_TDM.degreeCent <- centralization.degree(HAW13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_TDMftn <- as.network.matrix(HAW13_TDMft)
HAW13_TDM.netDensity <- network.density(HAW13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_TDM.entropy <- entropy(HAW13_TDMft) #entropy

HAW13_TDM.netMx <- cbind(HAW13_TDM.netMx, HAW13_TDM.clusterCoef, HAW13_TDM.degreeCent$centralization,
                         HAW13_TDM.netDensity, HAW13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
HAW13_SDg2 <- data.frame(HAW13_SD)
HAW13_SDg2 <- HAW13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_SDg2$player1
player2vector <- HAW13_SDg2$player2
HAW13_SDg3 <- HAW13_SDg2
HAW13_SDg3$p1inp2vec <- is.element(HAW13_SDg3$player1, player2vector)
HAW13_SDg3$p2inp1vec <- is.element(HAW13_SDg3$player2, player1vector)

addPlayer1 <- HAW13_SDg3[ which(HAW13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_SDg3[ which(HAW13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_SDg2 <- rbind(HAW13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
HAW13_SDft <- ftable(HAW13_SDg2$player1, HAW13_SDg2$player2)
HAW13_SDft2 <- as.matrix(HAW13_SDft)
numRows <- nrow(HAW13_SDft2)
numCols <- ncol(HAW13_SDft2)
HAW13_SDft3 <- HAW13_SDft2[c(2:numRows) , c(2:numCols)]
HAW13_SDTable <- graph.adjacency(HAW13_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(HAW13_SDTable, vertex.label = V(HAW13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
HAW13_SD.clusterCoef <- transitivity(HAW13_SDTable, type="global") #cluster coefficient
HAW13_SD.degreeCent <- centralization.degree(HAW13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_SDftn <- as.network.matrix(HAW13_SDft)
HAW13_SD.netDensity <- network.density(HAW13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_SD.entropy <- entropy(HAW13_SDft) #entropy

HAW13_SD.netMx <- cbind(HAW13_SD.netMx, HAW13_SD.clusterCoef, HAW13_SD.degreeCent$centralization,
                        HAW13_SD.netDensity, HAW13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
HAW13_TDg2 <- data.frame(HAW13_TD)
HAW13_TDg2 <- HAW13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_TDg2$player1
player2vector <- HAW13_TDg2$player2
HAW13_TDg3 <- HAW13_TDg2
HAW13_TDg3$p1inp2vec <- is.element(HAW13_TDg3$player1, player2vector)
HAW13_TDg3$p2inp1vec <- is.element(HAW13_TDg3$player2, player1vector)

addPlayer1 <- HAW13_TDg3[ which(HAW13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_TDg3[ which(HAW13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_TDg2 <- rbind(HAW13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
HAW13_TDft <- ftable(HAW13_TDg2$player1, HAW13_TDg2$player2)
HAW13_TDft2 <- as.matrix(HAW13_TDft)
numRows <- nrow(HAW13_TDft2)
numCols <- ncol(HAW13_TDft2)
HAW13_TDft3 <- HAW13_TDft2[c(2:numRows) , c(2:numCols)]
HAW13_TDTable <- graph.adjacency(HAW13_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(HAW13_TDTable, vertex.label = V(HAW13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
HAW13_TD.clusterCoef <- transitivity(HAW13_TDTable, type="global") #cluster coefficient
HAW13_TD.degreeCent <- centralization.degree(HAW13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_TDftn <- as.network.matrix(HAW13_TDft)
HAW13_TD.netDensity <- network.density(HAW13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_TD.entropy <- entropy(HAW13_TDft) #entropy

HAW13_TD.netMx <- cbind(HAW13_TD.netMx, HAW13_TD.clusterCoef, HAW13_TD.degreeCent$centralization,
                        HAW13_TD.netDensity, HAW13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
HAW13_QTg2 <- data.frame(HAW13_QT)
HAW13_QTg2 <- HAW13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW13_QTg2$player1
player2vector <- HAW13_QTg2$player2
HAW13_QTg3 <- HAW13_QTg2
HAW13_QTg3$p1inp2vec <- is.element(HAW13_QTg3$player1, player2vector)
HAW13_QTg3$p2inp1vec <- is.element(HAW13_QTg3$player2, player1vector)

addPlayer1 <- HAW13_QTg3[ which(HAW13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW13_QTg3[ which(HAW13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW13_QTg2 <- rbind(HAW13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
HAW13_QTft <- ftable(HAW13_QTg2$player1, HAW13_QTg2$player2)
HAW13_QTft2 <- as.matrix(HAW13_QTft)
numRows <- nrow(HAW13_QTft2)
numCols <- ncol(HAW13_QTft2)
HAW13_QTft3 <- HAW13_QTft2[c(2:numRows) , c(2:numCols)]
HAW13_QTTable <- graph.adjacency(HAW13_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(HAW13_QTTable, vertex.label = V(HAW13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
HAW13_QT.clusterCoef <- transitivity(HAW13_QTTable, type="global") #cluster coefficient
HAW13_QT.degreeCent <- centralization.degree(HAW13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW13_QTftn <- as.network.matrix(HAW13_QTft)
HAW13_QT.netDensity <- network.density(HAW13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW13_QT.entropy <- entropy(HAW13_QTft) #entropy

HAW13_QT.netMx <- cbind(HAW13_QT.netMx, HAW13_QT.clusterCoef, HAW13_QT.degreeCent$centralization,
                        HAW13_QT.netDensity, HAW13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW13_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************
#NA

round = 13
teamName = "RICH"
KIoutcome = "Goal_F"
RICH13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
RICH13_Gg2 <- data.frame(RICH13_G)
RICH13_Gg2 <- RICH13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_Gg2$player1
player2vector <- RICH13_Gg2$player2
RICH13_Gg3 <- RICH13_Gg2
RICH13_Gg3$p1inp2vec <- is.element(RICH13_Gg3$player1, player2vector)
RICH13_Gg3$p2inp1vec <- is.element(RICH13_Gg3$player2, player1vector)

addPlayer1 <- RICH13_Gg3[ which(RICH13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_Gg3[ which(RICH13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_Gg2 <- rbind(RICH13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
RICH13_Gft <- ftable(RICH13_Gg2$player1, RICH13_Gg2$player2)
RICH13_Gft2 <- as.matrix(RICH13_Gft)
numRows <- nrow(RICH13_Gft2)
numCols <- ncol(RICH13_Gft2)
RICH13_Gft3 <- RICH13_Gft2[c(2:numRows) , c(2:numCols)]
RICH13_GTable <- graph.adjacency(RICH13_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(RICH13_GTable, vertex.label = V(RICH13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
RICH13_G.clusterCoef <- transitivity(RICH13_GTable, type="global") #cluster coefficient
RICH13_G.degreeCent <- centralization.degree(RICH13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_Gftn <- as.network.matrix(RICH13_Gft)
RICH13_G.netDensity <- network.density(RICH13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_G.entropy <- entropy(RICH13_Gft) #entropy

RICH13_G.netMx <- cbind(RICH13_G.netMx, RICH13_G.clusterCoef, RICH13_G.degreeCent$centralization,
                        RICH13_G.netDensity, RICH13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************

round = 13
teamName = "RICH"
KIoutcome = "Behind_F"
RICH13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
RICH13_Bg2 <- data.frame(RICH13_B)
RICH13_Bg2 <- RICH13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_Bg2$player1
player2vector <- RICH13_Bg2$player2
RICH13_Bg3 <- RICH13_Bg2
RICH13_Bg3$p1inp2vec <- is.element(RICH13_Bg3$player1, player2vector)
RICH13_Bg3$p2inp1vec <- is.element(RICH13_Bg3$player2, player1vector)

addPlayer1 <- RICH13_Bg3[ which(RICH13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_Bg3[ which(RICH13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_Bg2 <- rbind(RICH13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
RICH13_Bft <- ftable(RICH13_Bg2$player1, RICH13_Bg2$player2)
RICH13_Bft2 <- as.matrix(RICH13_Bft)
numRows <- nrow(RICH13_Bft2)
numCols <- ncol(RICH13_Bft2)
RICH13_Bft3 <- RICH13_Bft2[c(2:numRows) , c(2:numCols)]
RICH13_BTable <- graph.adjacency(RICH13_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(RICH13_BTable, vertex.label = V(RICH13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
RICH13_B.clusterCoef <- transitivity(RICH13_BTable, type="global") #cluster coefficient
RICH13_B.degreeCent <- centralization.degree(RICH13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_Bftn <- as.network.matrix(RICH13_Bft)
RICH13_B.netDensity <- network.density(RICH13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_B.entropy <- entropy(RICH13_Bft) #entropy

RICH13_B.netMx <- cbind(RICH13_B.netMx, RICH13_B.clusterCoef, RICH13_B.degreeCent$centralization,
                        RICH13_B.netDensity, RICH13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************
#NA

round = 13
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
RICH13_SFg2 <- data.frame(RICH13_SF)
RICH13_SFg2 <- RICH13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_SFg2$player1
player2vector <- RICH13_SFg2$player2
RICH13_SFg3 <- RICH13_SFg2
RICH13_SFg3$p1inp2vec <- is.element(RICH13_SFg3$player1, player2vector)
RICH13_SFg3$p2inp1vec <- is.element(RICH13_SFg3$player2, player1vector)

addPlayer1 <- RICH13_SFg3[ which(RICH13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_SFg3[ which(RICH13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_SFg2 <- rbind(RICH13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
RICH13_SFft <- ftable(RICH13_SFg2$player1, RICH13_SFg2$player2)
RICH13_SFft2 <- as.matrix(RICH13_SFft)
numRows <- nrow(RICH13_SFft2)
numCols <- ncol(RICH13_SFft2)
RICH13_SFft3 <- RICH13_SFft2[c(2:numRows) , c(2:numCols)]
RICH13_SFTable <- graph.adjacency(RICH13_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(RICH13_SFTable, vertex.label = V(RICH13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
RICH13_SF.clusterCoef <- transitivity(RICH13_SFTable, type="global") #cluster coefficient
RICH13_SF.degreeCent <- centralization.degree(RICH13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_SFftn <- as.network.matrix(RICH13_SFft)
RICH13_SF.netDensity <- network.density(RICH13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_SF.entropy <- entropy(RICH13_SFft) #entropy

RICH13_SF.netMx <- cbind(RICH13_SF.netMx, RICH13_SF.clusterCoef, RICH13_SF.degreeCent$centralization,
                         RICH13_SF.netDensity, RICH13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************
#NA

round = 13
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
RICH13_TFg2 <- data.frame(RICH13_TF)
RICH13_TFg2 <- RICH13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_TFg2$player1
player2vector <- RICH13_TFg2$player2
RICH13_TFg3 <- RICH13_TFg2
RICH13_TFg3$p1inp2vec <- is.element(RICH13_TFg3$player1, player2vector)
RICH13_TFg3$p2inp1vec <- is.element(RICH13_TFg3$player2, player1vector)

addPlayer1 <- RICH13_TFg3[ which(RICH13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_TFg3[ which(RICH13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_TFg2 <- rbind(RICH13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
RICH13_TFft <- ftable(RICH13_TFg2$player1, RICH13_TFg2$player2)
RICH13_TFft2 <- as.matrix(RICH13_TFft)
numRows <- nrow(RICH13_TFft2)
numCols <- ncol(RICH13_TFft2)
RICH13_TFft3 <- RICH13_TFft2[c(2:numRows) , c(2:numCols)]
RICH13_TFTable <- graph.adjacency(RICH13_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(RICH13_TFTable, vertex.label = V(RICH13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
RICH13_TF.clusterCoef <- transitivity(RICH13_TFTable, type="global") #cluster coefficient
RICH13_TF.degreeCent <- centralization.degree(RICH13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_TFftn <- as.network.matrix(RICH13_TFft)
RICH13_TF.netDensity <- network.density(RICH13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_TF.entropy <- entropy(RICH13_TFft) #entropy

RICH13_TF.netMx <- cbind(RICH13_TF.netMx, RICH13_TF.clusterCoef, RICH13_TF.degreeCent$centralization,
                         RICH13_TF.netDensity, RICH13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************

round = 13
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
RICH13_SAMg2 <- data.frame(RICH13_SAM)
RICH13_SAMg2 <- RICH13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_SAMg2$player1
player2vector <- RICH13_SAMg2$player2
RICH13_SAMg3 <- RICH13_SAMg2
RICH13_SAMg3$p1inp2vec <- is.element(RICH13_SAMg3$player1, player2vector)
RICH13_SAMg3$p2inp1vec <- is.element(RICH13_SAMg3$player2, player1vector)

addPlayer1 <- RICH13_SAMg3[ which(RICH13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_SAMg3[ which(RICH13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_SAMg2 <- rbind(RICH13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
RICH13_SAMft <- ftable(RICH13_SAMg2$player1, RICH13_SAMg2$player2)
RICH13_SAMft2 <- as.matrix(RICH13_SAMft)
numRows <- nrow(RICH13_SAMft2)
numCols <- ncol(RICH13_SAMft2)
RICH13_SAMft3 <- RICH13_SAMft2[c(2:numRows) , c(2:numCols)]
RICH13_SAMTable <- graph.adjacency(RICH13_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(RICH13_SAMTable, vertex.label = V(RICH13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
RICH13_SAM.clusterCoef <- transitivity(RICH13_SAMTable, type="global") #cluster coefficient
RICH13_SAM.degreeCent <- centralization.degree(RICH13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_SAMftn <- as.network.matrix(RICH13_SAMft)
RICH13_SAM.netDensity <- network.density(RICH13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_SAM.entropy <- entropy(RICH13_SAMft) #entropy

RICH13_SAM.netMx <- cbind(RICH13_SAM.netMx, RICH13_SAM.clusterCoef, RICH13_SAM.degreeCent$centralization,
                          RICH13_SAM.netDensity, RICH13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
RICH13_TAMg2 <- data.frame(RICH13_TAM)
RICH13_TAMg2 <- RICH13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_TAMg2$player1
player2vector <- RICH13_TAMg2$player2
RICH13_TAMg3 <- RICH13_TAMg2
RICH13_TAMg3$p1inp2vec <- is.element(RICH13_TAMg3$player1, player2vector)
RICH13_TAMg3$p2inp1vec <- is.element(RICH13_TAMg3$player2, player1vector)

addPlayer1 <- RICH13_TAMg3[ which(RICH13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_TAMg3[ which(RICH13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_TAMg2 <- rbind(RICH13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
RICH13_TAMft <- ftable(RICH13_TAMg2$player1, RICH13_TAMg2$player2)
RICH13_TAMft2 <- as.matrix(RICH13_TAMft)
numRows <- nrow(RICH13_TAMft2)
numCols <- ncol(RICH13_TAMft2)
RICH13_TAMft3 <- RICH13_TAMft2[c(2:numRows) , c(2:numCols)]
RICH13_TAMTable <- graph.adjacency(RICH13_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(RICH13_TAMTable, vertex.label = V(RICH13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
RICH13_TAM.clusterCoef <- transitivity(RICH13_TAMTable, type="global") #cluster coefficient
RICH13_TAM.degreeCent <- centralization.degree(RICH13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_TAMftn <- as.network.matrix(RICH13_TAMft)
RICH13_TAM.netDensity <- network.density(RICH13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_TAM.entropy <- entropy(RICH13_TAMft) #entropy

RICH13_TAM.netMx <- cbind(RICH13_TAM.netMx, RICH13_TAM.clusterCoef, RICH13_TAM.degreeCent$centralization,
                          RICH13_TAM.netDensity, RICH13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************

round = 13
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
RICH13_SDMg2 <- data.frame(RICH13_SDM)
RICH13_SDMg2 <- RICH13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_SDMg2$player1
player2vector <- RICH13_SDMg2$player2
RICH13_SDMg3 <- RICH13_SDMg2
RICH13_SDMg3$p1inp2vec <- is.element(RICH13_SDMg3$player1, player2vector)
RICH13_SDMg3$p2inp1vec <- is.element(RICH13_SDMg3$player2, player1vector)

addPlayer1 <- RICH13_SDMg3[ which(RICH13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_SDMg3[ which(RICH13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_SDMg2 <- rbind(RICH13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
RICH13_SDMft <- ftable(RICH13_SDMg2$player1, RICH13_SDMg2$player2)
RICH13_SDMft2 <- as.matrix(RICH13_SDMft)
numRows <- nrow(RICH13_SDMft2)
numCols <- ncol(RICH13_SDMft2)
RICH13_SDMft3 <- RICH13_SDMft2[c(2:numRows) , c(2:numCols)]
RICH13_SDMTable <- graph.adjacency(RICH13_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(RICH13_SDMTable, vertex.label = V(RICH13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
RICH13_SDM.clusterCoef <- transitivity(RICH13_SDMTable, type="global") #cluster coefficient
RICH13_SDM.degreeCent <- centralization.degree(RICH13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_SDMftn <- as.network.matrix(RICH13_SDMft)
RICH13_SDM.netDensity <- network.density(RICH13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_SDM.entropy <- entropy(RICH13_SDMft) #entropy

RICH13_SDM.netMx <- cbind(RICH13_SDM.netMx, RICH13_SDM.clusterCoef, RICH13_SDM.degreeCent$centralization,
                          RICH13_SDM.netDensity, RICH13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
RICH13_TDMg2 <- data.frame(RICH13_TDM)
RICH13_TDMg2 <- RICH13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_TDMg2$player1
player2vector <- RICH13_TDMg2$player2
RICH13_TDMg3 <- RICH13_TDMg2
RICH13_TDMg3$p1inp2vec <- is.element(RICH13_TDMg3$player1, player2vector)
RICH13_TDMg3$p2inp1vec <- is.element(RICH13_TDMg3$player2, player1vector)

addPlayer1 <- RICH13_TDMg3[ which(RICH13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_TDMg3[ which(RICH13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_TDMg2 <- rbind(RICH13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
RICH13_TDMft <- ftable(RICH13_TDMg2$player1, RICH13_TDMg2$player2)
RICH13_TDMft2 <- as.matrix(RICH13_TDMft)
numRows <- nrow(RICH13_TDMft2)
numCols <- ncol(RICH13_TDMft2)
RICH13_TDMft3 <- RICH13_TDMft2[c(2:numRows) , c(2:numCols)]
RICH13_TDMTable <- graph.adjacency(RICH13_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(RICH13_TDMTable, vertex.label = V(RICH13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
RICH13_TDM.clusterCoef <- transitivity(RICH13_TDMTable, type="global") #cluster coefficient
RICH13_TDM.degreeCent <- centralization.degree(RICH13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_TDMftn <- as.network.matrix(RICH13_TDMft)
RICH13_TDM.netDensity <- network.density(RICH13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_TDM.entropy <- entropy(RICH13_TDMft) #entropy

RICH13_TDM.netMx <- cbind(RICH13_TDM.netMx, RICH13_TDM.clusterCoef, RICH13_TDM.degreeCent$centralization,
                          RICH13_TDM.netDensity, RICH13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
RICH13_SDg2 <- data.frame(RICH13_SD)
RICH13_SDg2 <- RICH13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_SDg2$player1
player2vector <- RICH13_SDg2$player2
RICH13_SDg3 <- RICH13_SDg2
RICH13_SDg3$p1inp2vec <- is.element(RICH13_SDg3$player1, player2vector)
RICH13_SDg3$p2inp1vec <- is.element(RICH13_SDg3$player2, player1vector)

addPlayer1 <- RICH13_SDg3[ which(RICH13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_SDg3[ which(RICH13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_SDg2 <- rbind(RICH13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
RICH13_SDft <- ftable(RICH13_SDg2$player1, RICH13_SDg2$player2)
RICH13_SDft2 <- as.matrix(RICH13_SDft)
numRows <- nrow(RICH13_SDft2)
numCols <- ncol(RICH13_SDft2)
RICH13_SDft3 <- RICH13_SDft2[c(2:numRows) , c(2:numCols)]
RICH13_SDTable <- graph.adjacency(RICH13_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(RICH13_SDTable, vertex.label = V(RICH13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
RICH13_SD.clusterCoef <- transitivity(RICH13_SDTable, type="global") #cluster coefficient
RICH13_SD.degreeCent <- centralization.degree(RICH13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_SDftn <- as.network.matrix(RICH13_SDft)
RICH13_SD.netDensity <- network.density(RICH13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_SD.entropy <- entropy(RICH13_SDft) #entropy

RICH13_SD.netMx <- cbind(RICH13_SD.netMx, RICH13_SD.clusterCoef, RICH13_SD.degreeCent$centralization,
                         RICH13_SD.netDensity, RICH13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
RICH13_TDg2 <- data.frame(RICH13_TD)
RICH13_TDg2 <- RICH13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_TDg2$player1
player2vector <- RICH13_TDg2$player2
RICH13_TDg3 <- RICH13_TDg2
RICH13_TDg3$p1inp2vec <- is.element(RICH13_TDg3$player1, player2vector)
RICH13_TDg3$p2inp1vec <- is.element(RICH13_TDg3$player2, player1vector)

addPlayer1 <- RICH13_TDg3[ which(RICH13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_TDg3[ which(RICH13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_TDg2 <- rbind(RICH13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
RICH13_TDft <- ftable(RICH13_TDg2$player1, RICH13_TDg2$player2)
RICH13_TDft2 <- as.matrix(RICH13_TDft)
numRows <- nrow(RICH13_TDft2)
numCols <- ncol(RICH13_TDft2)
RICH13_TDft3 <- RICH13_TDft2[c(2:numRows) , c(2:numCols)]
RICH13_TDTable <- graph.adjacency(RICH13_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(RICH13_TDTable, vertex.label = V(RICH13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
RICH13_TD.clusterCoef <- transitivity(RICH13_TDTable, type="global") #cluster coefficient
RICH13_TD.degreeCent <- centralization.degree(RICH13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_TDftn <- as.network.matrix(RICH13_TDft)
RICH13_TD.netDensity <- network.density(RICH13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_TD.entropy <- entropy(RICH13_TDft) #entropy

RICH13_TD.netMx <- cbind(RICH13_TD.netMx, RICH13_TD.clusterCoef, RICH13_TD.degreeCent$centralization,
                         RICH13_TD.netDensity, RICH13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
RICH13_QTg2 <- data.frame(RICH13_QT)
RICH13_QTg2 <- RICH13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH13_QTg2$player1
player2vector <- RICH13_QTg2$player2
RICH13_QTg3 <- RICH13_QTg2
RICH13_QTg3$p1inp2vec <- is.element(RICH13_QTg3$player1, player2vector)
RICH13_QTg3$p2inp1vec <- is.element(RICH13_QTg3$player2, player1vector)

addPlayer1 <- RICH13_QTg3[ which(RICH13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH13_QTg3[ which(RICH13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH13_QTg2 <- rbind(RICH13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
RICH13_QTft <- ftable(RICH13_QTg2$player1, RICH13_QTg2$player2)
RICH13_QTft2 <- as.matrix(RICH13_QTft)
numRows <- nrow(RICH13_QTft2)
numCols <- ncol(RICH13_QTft2)
RICH13_QTft3 <- RICH13_QTft2[c(2:numRows) , c(2:numCols)]
RICH13_QTTable <- graph.adjacency(RICH13_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(RICH13_QTTable, vertex.label = V(RICH13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
RICH13_QT.clusterCoef <- transitivity(RICH13_QTTable, type="global") #cluster coefficient
RICH13_QT.degreeCent <- centralization.degree(RICH13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH13_QTftn <- as.network.matrix(RICH13_QTft)
RICH13_QT.netDensity <- network.density(RICH13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH13_QT.entropy <- entropy(RICH13_QTft) #entropy

RICH13_QT.netMx <- cbind(RICH13_QT.netMx, RICH13_QT.clusterCoef, RICH13_QT.degreeCent$centralization,
                         RICH13_QT.netDensity, RICH13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH13_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************
#NA

round = 13
teamName = "STK"
KIoutcome = "Goal_F"
STK13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
STK13_Gg2 <- data.frame(STK13_G)
STK13_Gg2 <- STK13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_Gg2$player1
player2vector <- STK13_Gg2$player2
STK13_Gg3 <- STK13_Gg2
STK13_Gg3$p1inp2vec <- is.element(STK13_Gg3$player1, player2vector)
STK13_Gg3$p2inp1vec <- is.element(STK13_Gg3$player2, player1vector)

addPlayer1 <- STK13_Gg3[ which(STK13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_Gg3[ which(STK13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_Gg2 <- rbind(STK13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
STK13_Gft <- ftable(STK13_Gg2$player1, STK13_Gg2$player2)
STK13_Gft2 <- as.matrix(STK13_Gft)
numRows <- nrow(STK13_Gft2)
numCols <- ncol(STK13_Gft2)
STK13_Gft3 <- STK13_Gft2[c(2:numRows) , c(2:numCols)]
STK13_GTable <- graph.adjacency(STK13_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(STK13_GTable, vertex.label = V(STK13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
STK13_G.clusterCoef <- transitivity(STK13_GTable, type="global") #cluster coefficient
STK13_G.degreeCent <- centralization.degree(STK13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_Gftn <- as.network.matrix(STK13_Gft)
STK13_G.netDensity <- network.density(STK13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_G.entropy <- entropy(STK13_Gft) #entropy

STK13_G.netMx <- cbind(STK13_G.netMx, STK13_G.clusterCoef, STK13_G.degreeCent$centralization,
                       STK13_G.netDensity, STK13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************

round = 13
teamName = "STK"
KIoutcome = "Behind_F"
STK13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
STK13_Bg2 <- data.frame(STK13_B)
STK13_Bg2 <- STK13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_Bg2$player1
player2vector <- STK13_Bg2$player2
STK13_Bg3 <- STK13_Bg2
STK13_Bg3$p1inp2vec <- is.element(STK13_Bg3$player1, player2vector)
STK13_Bg3$p2inp1vec <- is.element(STK13_Bg3$player2, player1vector)

addPlayer1 <- STK13_Bg3[ which(STK13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK13_Bg3[ which(STK13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_Bg2 <- rbind(STK13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
STK13_Bft <- ftable(STK13_Bg2$player1, STK13_Bg2$player2)
STK13_Bft2 <- as.matrix(STK13_Bft)
numRows <- nrow(STK13_Bft2)
numCols <- ncol(STK13_Bft2)
STK13_Bft3 <- STK13_Bft2[c(2:numRows) , c(2:numCols)]
STK13_BTable <- graph.adjacency(STK13_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(STK13_BTable, vertex.label = V(STK13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
STK13_B.clusterCoef <- transitivity(STK13_BTable, type="global") #cluster coefficient
STK13_B.degreeCent <- centralization.degree(STK13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_Bftn <- as.network.matrix(STK13_Bft)
STK13_B.netDensity <- network.density(STK13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_B.entropy <- entropy(STK13_Bft) #entropy

STK13_B.netMx <- cbind(STK13_B.netMx, STK13_B.clusterCoef, STK13_B.degreeCent$centralization,
                       STK13_B.netDensity, STK13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************
#NA

round = 13
teamName = "STK"
KIoutcome = "Stoppage_F"
STK13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
STK13_SFg2 <- data.frame(STK13_SF)
STK13_SFg2 <- STK13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_SFg2$player1
player2vector <- STK13_SFg2$player2
STK13_SFg3 <- STK13_SFg2
STK13_SFg3$p1inp2vec <- is.element(STK13_SFg3$player1, player2vector)
STK13_SFg3$p2inp1vec <- is.element(STK13_SFg3$player2, player1vector)

addPlayer1 <- STK13_SFg3[ which(STK13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_SFg3[ which(STK13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_SFg2 <- rbind(STK13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
STK13_SFft <- ftable(STK13_SFg2$player1, STK13_SFg2$player2)
STK13_SFft2 <- as.matrix(STK13_SFft)
numRows <- nrow(STK13_SFft2)
numCols <- ncol(STK13_SFft2)
STK13_SFft3 <- STK13_SFft2[c(2:numRows) , c(2:numCols)]
STK13_SFTable <- graph.adjacency(STK13_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(STK13_SFTable, vertex.label = V(STK13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
STK13_SF.clusterCoef <- transitivity(STK13_SFTable, type="global") #cluster coefficient
STK13_SF.degreeCent <- centralization.degree(STK13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_SFftn <- as.network.matrix(STK13_SFft)
STK13_SF.netDensity <- network.density(STK13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_SF.entropy <- entropy(STK13_SFft) #entropy

STK13_SF.netMx <- cbind(STK13_SF.netMx, STK13_SF.clusterCoef, STK13_SF.degreeCent$centralization,
                        STK13_SF.netDensity, STK13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************

round = 13
teamName = "STK"
KIoutcome = "Turnover_F"
STK13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
STK13_TFg2 <- data.frame(STK13_TF)
STK13_TFg2 <- STK13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_TFg2$player1
player2vector <- STK13_TFg2$player2
STK13_TFg3 <- STK13_TFg2
STK13_TFg3$p1inp2vec <- is.element(STK13_TFg3$player1, player2vector)
STK13_TFg3$p2inp1vec <- is.element(STK13_TFg3$player2, player1vector)

addPlayer1 <- STK13_TFg3[ which(STK13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_TFg3[ which(STK13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_TFg2 <- rbind(STK13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
STK13_TFft <- ftable(STK13_TFg2$player1, STK13_TFg2$player2)
STK13_TFft2 <- as.matrix(STK13_TFft)
numRows <- nrow(STK13_TFft2)
numCols <- ncol(STK13_TFft2)
STK13_TFft3 <- STK13_TFft2[c(2:numRows) , c(2:numCols)]
STK13_TFTable <- graph.adjacency(STK13_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(STK13_TFTable, vertex.label = V(STK13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
STK13_TF.clusterCoef <- transitivity(STK13_TFTable, type="global") #cluster coefficient
STK13_TF.degreeCent <- centralization.degree(STK13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_TFftn <- as.network.matrix(STK13_TFft)
STK13_TF.netDensity <- network.density(STK13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_TF.entropy <- entropy(STK13_TFft) #entropy

STK13_TF.netMx <- cbind(STK13_TF.netMx, STK13_TF.clusterCoef, STK13_TF.degreeCent$centralization,
                        STK13_TF.netDensity, STK13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************

round = 13
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
STK13_SAMg2 <- data.frame(STK13_SAM)
STK13_SAMg2 <- STK13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_SAMg2$player1
player2vector <- STK13_SAMg2$player2
STK13_SAMg3 <- STK13_SAMg2
STK13_SAMg3$p1inp2vec <- is.element(STK13_SAMg3$player1, player2vector)
STK13_SAMg3$p2inp1vec <- is.element(STK13_SAMg3$player2, player1vector)

addPlayer1 <- STK13_SAMg3[ which(STK13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_SAMg3[ which(STK13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_SAMg2 <- rbind(STK13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
STK13_SAMft <- ftable(STK13_SAMg2$player1, STK13_SAMg2$player2)
STK13_SAMft2 <- as.matrix(STK13_SAMft)
numRows <- nrow(STK13_SAMft2)
numCols <- ncol(STK13_SAMft2)
STK13_SAMft3 <- STK13_SAMft2[c(2:numRows) , c(2:numCols)]
STK13_SAMTable <- graph.adjacency(STK13_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(STK13_SAMTable, vertex.label = V(STK13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
STK13_SAM.clusterCoef <- transitivity(STK13_SAMTable, type="global") #cluster coefficient
STK13_SAM.degreeCent <- centralization.degree(STK13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_SAMftn <- as.network.matrix(STK13_SAMft)
STK13_SAM.netDensity <- network.density(STK13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_SAM.entropy <- entropy(STK13_SAMft) #entropy

STK13_SAM.netMx <- cbind(STK13_SAM.netMx, STK13_SAM.clusterCoef, STK13_SAM.degreeCent$centralization,
                         STK13_SAM.netDensity, STK13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "STK"
KIoutcome = "Turnover_AM"
STK13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
STK13_TAMg2 <- data.frame(STK13_TAM)
STK13_TAMg2 <- STK13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_TAMg2$player1
player2vector <- STK13_TAMg2$player2
STK13_TAMg3 <- STK13_TAMg2
STK13_TAMg3$p1inp2vec <- is.element(STK13_TAMg3$player1, player2vector)
STK13_TAMg3$p2inp1vec <- is.element(STK13_TAMg3$player2, player1vector)

addPlayer1 <- STK13_TAMg3[ which(STK13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_TAMg3[ which(STK13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_TAMg2 <- rbind(STK13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
STK13_TAMft <- ftable(STK13_TAMg2$player1, STK13_TAMg2$player2)
STK13_TAMft2 <- as.matrix(STK13_TAMft)
numRows <- nrow(STK13_TAMft2)
numCols <- ncol(STK13_TAMft2)
STK13_TAMft3 <- STK13_TAMft2[c(2:numRows) , c(2:numCols)]
STK13_TAMTable <- graph.adjacency(STK13_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(STK13_TAMTable, vertex.label = V(STK13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
STK13_TAM.clusterCoef <- transitivity(STK13_TAMTable, type="global") #cluster coefficient
STK13_TAM.degreeCent <- centralization.degree(STK13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_TAMftn <- as.network.matrix(STK13_TAMft)
STK13_TAM.netDensity <- network.density(STK13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_TAM.entropy <- entropy(STK13_TAMft) #entropy

STK13_TAM.netMx <- cbind(STK13_TAM.netMx, STK13_TAM.clusterCoef, STK13_TAM.degreeCent$centralization,
                         STK13_TAM.netDensity, STK13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************

round = 13
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
STK13_SDMg2 <- data.frame(STK13_SDM)
STK13_SDMg2 <- STK13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_SDMg2$player1
player2vector <- STK13_SDMg2$player2
STK13_SDMg3 <- STK13_SDMg2
STK13_SDMg3$p1inp2vec <- is.element(STK13_SDMg3$player1, player2vector)
STK13_SDMg3$p2inp1vec <- is.element(STK13_SDMg3$player2, player1vector)

addPlayer1 <- STK13_SDMg3[ which(STK13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_SDMg3[ which(STK13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_SDMg2 <- rbind(STK13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
STK13_SDMft <- ftable(STK13_SDMg2$player1, STK13_SDMg2$player2)
STK13_SDMft2 <- as.matrix(STK13_SDMft)
numRows <- nrow(STK13_SDMft2)
numCols <- ncol(STK13_SDMft2)
STK13_SDMft3 <- STK13_SDMft2[c(2:numRows) , c(2:numCols)]
STK13_SDMTable <- graph.adjacency(STK13_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(STK13_SDMTable, vertex.label = V(STK13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
STK13_SDM.clusterCoef <- transitivity(STK13_SDMTable, type="global") #cluster coefficient
STK13_SDM.degreeCent <- centralization.degree(STK13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_SDMftn <- as.network.matrix(STK13_SDMft)
STK13_SDM.netDensity <- network.density(STK13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_SDM.entropy <- entropy(STK13_SDMft) #entropy

STK13_SDM.netMx <- cbind(STK13_SDM.netMx, STK13_SDM.clusterCoef, STK13_SDM.degreeCent$centralization,
                         STK13_SDM.netDensity, STK13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "STK"
KIoutcome = "Turnover_DM"
STK13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
STK13_TDMg2 <- data.frame(STK13_TDM)
STK13_TDMg2 <- STK13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_TDMg2$player1
player2vector <- STK13_TDMg2$player2
STK13_TDMg3 <- STK13_TDMg2
STK13_TDMg3$p1inp2vec <- is.element(STK13_TDMg3$player1, player2vector)
STK13_TDMg3$p2inp1vec <- is.element(STK13_TDMg3$player2, player1vector)

addPlayer1 <- STK13_TDMg3[ which(STK13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_TDMg3[ which(STK13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_TDMg2 <- rbind(STK13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
STK13_TDMft <- ftable(STK13_TDMg2$player1, STK13_TDMg2$player2)
STK13_TDMft2 <- as.matrix(STK13_TDMft)
numRows <- nrow(STK13_TDMft2)
numCols <- ncol(STK13_TDMft2)
STK13_TDMft3 <- STK13_TDMft2[c(2:numRows) , c(2:numCols)]
STK13_TDMTable <- graph.adjacency(STK13_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(STK13_TDMTable, vertex.label = V(STK13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
STK13_TDM.clusterCoef <- transitivity(STK13_TDMTable, type="global") #cluster coefficient
STK13_TDM.degreeCent <- centralization.degree(STK13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_TDMftn <- as.network.matrix(STK13_TDMft)
STK13_TDM.netDensity <- network.density(STK13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_TDM.entropy <- entropy(STK13_TDMft) #entropy

STK13_TDM.netMx <- cbind(STK13_TDM.netMx, STK13_TDM.clusterCoef, STK13_TDM.degreeCent$centralization,
                         STK13_TDM.netDensity, STK13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "STK"
KIoutcome = "Stoppage_D"
STK13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
STK13_SDg2 <- data.frame(STK13_SD)
STK13_SDg2 <- STK13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_SDg2$player1
player2vector <- STK13_SDg2$player2
STK13_SDg3 <- STK13_SDg2
STK13_SDg3$p1inp2vec <- is.element(STK13_SDg3$player1, player2vector)
STK13_SDg3$p2inp1vec <- is.element(STK13_SDg3$player2, player1vector)

addPlayer1 <- STK13_SDg3[ which(STK13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_SDg3[ which(STK13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_SDg2 <- rbind(STK13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
STK13_SDft <- ftable(STK13_SDg2$player1, STK13_SDg2$player2)
STK13_SDft2 <- as.matrix(STK13_SDft)
numRows <- nrow(STK13_SDft2)
numCols <- ncol(STK13_SDft2)
STK13_SDft3 <- STK13_SDft2[c(2:numRows) , c(2:numCols)]
STK13_SDTable <- graph.adjacency(STK13_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(STK13_SDTable, vertex.label = V(STK13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
STK13_SD.clusterCoef <- transitivity(STK13_SDTable, type="global") #cluster coefficient
STK13_SD.degreeCent <- centralization.degree(STK13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_SDftn <- as.network.matrix(STK13_SDft)
STK13_SD.netDensity <- network.density(STK13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_SD.entropy <- entropy(STK13_SDft) #entropy

STK13_SD.netMx <- cbind(STK13_SD.netMx, STK13_SD.clusterCoef, STK13_SD.degreeCent$centralization,
                        STK13_SD.netDensity, STK13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "STK"
KIoutcome = "Turnover_D"
STK13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
STK13_TDg2 <- data.frame(STK13_TD)
STK13_TDg2 <- STK13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_TDg2$player1
player2vector <- STK13_TDg2$player2
STK13_TDg3 <- STK13_TDg2
STK13_TDg3$p1inp2vec <- is.element(STK13_TDg3$player1, player2vector)
STK13_TDg3$p2inp1vec <- is.element(STK13_TDg3$player2, player1vector)

addPlayer1 <- STK13_TDg3[ which(STK13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_TDg3[ which(STK13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_TDg2 <- rbind(STK13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
STK13_TDft <- ftable(STK13_TDg2$player1, STK13_TDg2$player2)
STK13_TDft2 <- as.matrix(STK13_TDft)
numRows <- nrow(STK13_TDft2)
numCols <- ncol(STK13_TDft2)
STK13_TDft3 <- STK13_TDft2[c(2:numRows) , c(2:numCols)]
STK13_TDTable <- graph.adjacency(STK13_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(STK13_TDTable, vertex.label = V(STK13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
STK13_TD.clusterCoef <- transitivity(STK13_TDTable, type="global") #cluster coefficient
STK13_TD.degreeCent <- centralization.degree(STK13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_TDftn <- as.network.matrix(STK13_TDft)
STK13_TD.netDensity <- network.density(STK13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_TD.entropy <- entropy(STK13_TDft) #entropy

STK13_TD.netMx <- cbind(STK13_TD.netMx, STK13_TD.clusterCoef, STK13_TD.degreeCent$centralization,
                        STK13_TD.netDensity, STK13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
STK13_QTg2 <- data.frame(STK13_QT)
STK13_QTg2 <- STK13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK13_QTg2$player1
player2vector <- STK13_QTg2$player2
STK13_QTg3 <- STK13_QTg2
STK13_QTg3$p1inp2vec <- is.element(STK13_QTg3$player1, player2vector)
STK13_QTg3$p2inp1vec <- is.element(STK13_QTg3$player2, player1vector)

addPlayer1 <- STK13_QTg3[ which(STK13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK13_QTg3[ which(STK13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK13_QTg2 <- rbind(STK13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
STK13_QTft <- ftable(STK13_QTg2$player1, STK13_QTg2$player2)
STK13_QTft2 <- as.matrix(STK13_QTft)
numRows <- nrow(STK13_QTft2)
numCols <- ncol(STK13_QTft2)
STK13_QTft3 <- STK13_QTft2[c(2:numRows) , c(2:numCols)]
STK13_QTTable <- graph.adjacency(STK13_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(STK13_QTTable, vertex.label = V(STK13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
STK13_QT.clusterCoef <- transitivity(STK13_QTTable, type="global") #cluster coefficient
STK13_QT.degreeCent <- centralization.degree(STK13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK13_QTftn <- as.network.matrix(STK13_QTft)
STK13_QT.netDensity <- network.density(STK13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK13_QT.entropy <- entropy(STK13_QTft) #entropy

STK13_QT.netMx <- cbind(STK13_QT.netMx, STK13_QT.clusterCoef, STK13_QT.degreeCent$centralization,
                        STK13_QT.netDensity, STK13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK13_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************
#NA

round = 13
teamName = "SYD"
KIoutcome = "Goal_F"
SYD13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
SYD13_Gg2 <- data.frame(SYD13_G)
SYD13_Gg2 <- SYD13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_Gg2$player1
player2vector <- SYD13_Gg2$player2
SYD13_Gg3 <- SYD13_Gg2
SYD13_Gg3$p1inp2vec <- is.element(SYD13_Gg3$player1, player2vector)
SYD13_Gg3$p2inp1vec <- is.element(SYD13_Gg3$player2, player1vector)

addPlayer1 <- SYD13_Gg3[ which(SYD13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_Gg3[ which(SYD13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_Gg2 <- rbind(SYD13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
SYD13_Gft <- ftable(SYD13_Gg2$player1, SYD13_Gg2$player2)
SYD13_Gft2 <- as.matrix(SYD13_Gft)
numRows <- nrow(SYD13_Gft2)
numCols <- ncol(SYD13_Gft2)
SYD13_Gft3 <- SYD13_Gft2[c(2:numRows) , c(2:numCols)]
SYD13_GTable <- graph.adjacency(SYD13_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(SYD13_GTable, vertex.label = V(SYD13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
SYD13_G.clusterCoef <- transitivity(SYD13_GTable, type="global") #cluster coefficient
SYD13_G.degreeCent <- centralization.degree(SYD13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_Gftn <- as.network.matrix(SYD13_Gft)
SYD13_G.netDensity <- network.density(SYD13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_G.entropy <- entropy(SYD13_Gft) #entropy

SYD13_G.netMx <- cbind(SYD13_G.netMx, SYD13_G.clusterCoef, SYD13_G.degreeCent$centralization,
                       SYD13_G.netDensity, SYD13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************
#NA

round = 13
teamName = "SYD"
KIoutcome = "Behind_F"
SYD13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
SYD13_Bg2 <- data.frame(SYD13_B)
SYD13_Bg2 <- SYD13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_Bg2$player1
player2vector <- SYD13_Bg2$player2
SYD13_Bg3 <- SYD13_Bg2
SYD13_Bg3$p1inp2vec <- is.element(SYD13_Bg3$player1, player2vector)
SYD13_Bg3$p2inp1vec <- is.element(SYD13_Bg3$player2, player1vector)

addPlayer1 <- SYD13_Bg3[ which(SYD13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_Bg3[ which(SYD13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_Bg2 <- rbind(SYD13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
SYD13_Bft <- ftable(SYD13_Bg2$player1, SYD13_Bg2$player2)
SYD13_Bft2 <- as.matrix(SYD13_Bft)
numRows <- nrow(SYD13_Bft2)
numCols <- ncol(SYD13_Bft2)
SYD13_Bft3 <- SYD13_Bft2[c(2:numRows) , c(2:numCols)]
SYD13_BTable <- graph.adjacency(SYD13_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(SYD13_BTable, vertex.label = V(SYD13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
SYD13_B.clusterCoef <- transitivity(SYD13_BTable, type="global") #cluster coefficient
SYD13_B.degreeCent <- centralization.degree(SYD13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_Bftn <- as.network.matrix(SYD13_Bft)
SYD13_B.netDensity <- network.density(SYD13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_B.entropy <- entropy(SYD13_Bft) #entropy

SYD13_B.netMx <- cbind(SYD13_B.netMx, SYD13_B.clusterCoef, SYD13_B.degreeCent$centralization,
                       SYD13_B.netDensity, SYD13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************

round = 13
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
SYD13_SFg2 <- data.frame(SYD13_SF)
SYD13_SFg2 <- SYD13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_SFg2$player1
player2vector <- SYD13_SFg2$player2
SYD13_SFg3 <- SYD13_SFg2
SYD13_SFg3$p1inp2vec <- is.element(SYD13_SFg3$player1, player2vector)
SYD13_SFg3$p2inp1vec <- is.element(SYD13_SFg3$player2, player1vector)

addPlayer1 <- SYD13_SFg3[ which(SYD13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_SFg3[ which(SYD13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_SFg2 <- rbind(SYD13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
SYD13_SFft <- ftable(SYD13_SFg2$player1, SYD13_SFg2$player2)
SYD13_SFft2 <- as.matrix(SYD13_SFft)
numRows <- nrow(SYD13_SFft2)
numCols <- ncol(SYD13_SFft2)
SYD13_SFft3 <- SYD13_SFft2[c(2:numRows) , c(2:numCols)]
SYD13_SFTable <- graph.adjacency(SYD13_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(SYD13_SFTable, vertex.label = V(SYD13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
SYD13_SF.clusterCoef <- transitivity(SYD13_SFTable, type="global") #cluster coefficient
SYD13_SF.degreeCent <- centralization.degree(SYD13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_SFftn <- as.network.matrix(SYD13_SFft)
SYD13_SF.netDensity <- network.density(SYD13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_SF.entropy <- entropy(SYD13_SFft) #entropy

SYD13_SF.netMx <- cbind(SYD13_SF.netMx, SYD13_SF.clusterCoef, SYD13_SF.degreeCent$centralization,
                        SYD13_SF.netDensity, SYD13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************

round = 13
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
SYD13_TFg2 <- data.frame(SYD13_TF)
SYD13_TFg2 <- SYD13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_TFg2$player1
player2vector <- SYD13_TFg2$player2
SYD13_TFg3 <- SYD13_TFg2
SYD13_TFg3$p1inp2vec <- is.element(SYD13_TFg3$player1, player2vector)
SYD13_TFg3$p2inp1vec <- is.element(SYD13_TFg3$player2, player1vector)

addPlayer1 <- SYD13_TFg3[ which(SYD13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_TFg3[ which(SYD13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_TFg2 <- rbind(SYD13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
SYD13_TFft <- ftable(SYD13_TFg2$player1, SYD13_TFg2$player2)
SYD13_TFft2 <- as.matrix(SYD13_TFft)
numRows <- nrow(SYD13_TFft2)
numCols <- ncol(SYD13_TFft2)
SYD13_TFft3 <- SYD13_TFft2[c(2:numRows) , c(2:numCols)]
SYD13_TFTable <- graph.adjacency(SYD13_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(SYD13_TFTable, vertex.label = V(SYD13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
SYD13_TF.clusterCoef <- transitivity(SYD13_TFTable, type="global") #cluster coefficient
SYD13_TF.degreeCent <- centralization.degree(SYD13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_TFftn <- as.network.matrix(SYD13_TFft)
SYD13_TF.netDensity <- network.density(SYD13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_TF.entropy <- entropy(SYD13_TFft) #entropy

SYD13_TF.netMx <- cbind(SYD13_TF.netMx, SYD13_TF.clusterCoef, SYD13_TF.degreeCent$centralization,
                        SYD13_TF.netDensity, SYD13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************
#NA

round = 13
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
SYD13_SAMg2 <- data.frame(SYD13_SAM)
SYD13_SAMg2 <- SYD13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_SAMg2$player1
player2vector <- SYD13_SAMg2$player2
SYD13_SAMg3 <- SYD13_SAMg2
SYD13_SAMg3$p1inp2vec <- is.element(SYD13_SAMg3$player1, player2vector)
SYD13_SAMg3$p2inp1vec <- is.element(SYD13_SAMg3$player2, player1vector)

addPlayer1 <- SYD13_SAMg3[ which(SYD13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_SAMg3[ which(SYD13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_SAMg2 <- rbind(SYD13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
SYD13_SAMft <- ftable(SYD13_SAMg2$player1, SYD13_SAMg2$player2)
SYD13_SAMft2 <- as.matrix(SYD13_SAMft)
numRows <- nrow(SYD13_SAMft2)
numCols <- ncol(SYD13_SAMft2)
SYD13_SAMft3 <- SYD13_SAMft2[c(2:numRows) , c(2:numCols)]
SYD13_SAMTable <- graph.adjacency(SYD13_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(SYD13_SAMTable, vertex.label = V(SYD13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
SYD13_SAM.clusterCoef <- transitivity(SYD13_SAMTable, type="global") #cluster coefficient
SYD13_SAM.degreeCent <- centralization.degree(SYD13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_SAMftn <- as.network.matrix(SYD13_SAMft)
SYD13_SAM.netDensity <- network.density(SYD13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_SAM.entropy <- entropy(SYD13_SAMft) #entropy

SYD13_SAM.netMx <- cbind(SYD13_SAM.netMx, SYD13_SAM.clusterCoef, SYD13_SAM.degreeCent$centralization,
                         SYD13_SAM.netDensity, SYD13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
SYD13_TAMg2 <- data.frame(SYD13_TAM)
SYD13_TAMg2 <- SYD13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_TAMg2$player1
player2vector <- SYD13_TAMg2$player2
SYD13_TAMg3 <- SYD13_TAMg2
SYD13_TAMg3$p1inp2vec <- is.element(SYD13_TAMg3$player1, player2vector)
SYD13_TAMg3$p2inp1vec <- is.element(SYD13_TAMg3$player2, player1vector)

addPlayer1 <- SYD13_TAMg3[ which(SYD13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_TAMg3[ which(SYD13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_TAMg2 <- rbind(SYD13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
SYD13_TAMft <- ftable(SYD13_TAMg2$player1, SYD13_TAMg2$player2)
SYD13_TAMft2 <- as.matrix(SYD13_TAMft)
numRows <- nrow(SYD13_TAMft2)
numCols <- ncol(SYD13_TAMft2)
SYD13_TAMft3 <- SYD13_TAMft2[c(2:numRows) , c(2:numCols)]
SYD13_TAMTable <- graph.adjacency(SYD13_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(SYD13_TAMTable, vertex.label = V(SYD13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
SYD13_TAM.clusterCoef <- transitivity(SYD13_TAMTable, type="global") #cluster coefficient
SYD13_TAM.degreeCent <- centralization.degree(SYD13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_TAMftn <- as.network.matrix(SYD13_TAMft)
SYD13_TAM.netDensity <- network.density(SYD13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_TAM.entropy <- entropy(SYD13_TAMft) #entropy

SYD13_TAM.netMx <- cbind(SYD13_TAM.netMx, SYD13_TAM.clusterCoef, SYD13_TAM.degreeCent$centralization,
                         SYD13_TAM.netDensity, SYD13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************
#NA

round = 13
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
SYD13_SDMg2 <- data.frame(SYD13_SDM)
SYD13_SDMg2 <- SYD13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_SDMg2$player1
player2vector <- SYD13_SDMg2$player2
SYD13_SDMg3 <- SYD13_SDMg2
SYD13_SDMg3$p1inp2vec <- is.element(SYD13_SDMg3$player1, player2vector)
SYD13_SDMg3$p2inp1vec <- is.element(SYD13_SDMg3$player2, player1vector)

addPlayer1 <- SYD13_SDMg3[ which(SYD13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_SDMg3[ which(SYD13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_SDMg2 <- rbind(SYD13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
SYD13_SDMft <- ftable(SYD13_SDMg2$player1, SYD13_SDMg2$player2)
SYD13_SDMft2 <- as.matrix(SYD13_SDMft)
numRows <- nrow(SYD13_SDMft2)
numCols <- ncol(SYD13_SDMft2)
SYD13_SDMft3 <- SYD13_SDMft2[c(2:numRows) , c(2:numCols)]
SYD13_SDMTable <- graph.adjacency(SYD13_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(SYD13_SDMTable, vertex.label = V(SYD13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
SYD13_SDM.clusterCoef <- transitivity(SYD13_SDMTable, type="global") #cluster coefficient
SYD13_SDM.degreeCent <- centralization.degree(SYD13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_SDMftn <- as.network.matrix(SYD13_SDMft)
SYD13_SDM.netDensity <- network.density(SYD13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_SDM.entropy <- entropy(SYD13_SDMft) #entropy

SYD13_SDM.netMx <- cbind(SYD13_SDM.netMx, SYD13_SDM.clusterCoef, SYD13_SDM.degreeCent$centralization,
                         SYD13_SDM.netDensity, SYD13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
SYD13_TDMg2 <- data.frame(SYD13_TDM)
SYD13_TDMg2 <- SYD13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_TDMg2$player1
player2vector <- SYD13_TDMg2$player2
SYD13_TDMg3 <- SYD13_TDMg2
SYD13_TDMg3$p1inp2vec <- is.element(SYD13_TDMg3$player1, player2vector)
SYD13_TDMg3$p2inp1vec <- is.element(SYD13_TDMg3$player2, player1vector)

addPlayer1 <- SYD13_TDMg3[ which(SYD13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_TDMg3[ which(SYD13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_TDMg2 <- rbind(SYD13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
SYD13_TDMft <- ftable(SYD13_TDMg2$player1, SYD13_TDMg2$player2)
SYD13_TDMft2 <- as.matrix(SYD13_TDMft)
numRows <- nrow(SYD13_TDMft2)
numCols <- ncol(SYD13_TDMft2)
SYD13_TDMft3 <- SYD13_TDMft2[c(2:numRows) , c(2:numCols)]
SYD13_TDMTable <- graph.adjacency(SYD13_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(SYD13_TDMTable, vertex.label = V(SYD13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
SYD13_TDM.clusterCoef <- transitivity(SYD13_TDMTable, type="global") #cluster coefficient
SYD13_TDM.degreeCent <- centralization.degree(SYD13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_TDMftn <- as.network.matrix(SYD13_TDMft)
SYD13_TDM.netDensity <- network.density(SYD13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_TDM.entropy <- entropy(SYD13_TDMft) #entropy

SYD13_TDM.netMx <- cbind(SYD13_TDM.netMx, SYD13_TDM.clusterCoef, SYD13_TDM.degreeCent$centralization,
                         SYD13_TDM.netDensity, SYD13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
SYD13_SDg2 <- data.frame(SYD13_SD)
SYD13_SDg2 <- SYD13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_SDg2$player1
player2vector <- SYD13_SDg2$player2
SYD13_SDg3 <- SYD13_SDg2
SYD13_SDg3$p1inp2vec <- is.element(SYD13_SDg3$player1, player2vector)
SYD13_SDg3$p2inp1vec <- is.element(SYD13_SDg3$player2, player1vector)

addPlayer1 <- SYD13_SDg3[ which(SYD13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_SDg3[ which(SYD13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_SDg2 <- rbind(SYD13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
SYD13_SDft <- ftable(SYD13_SDg2$player1, SYD13_SDg2$player2)
SYD13_SDft2 <- as.matrix(SYD13_SDft)
numRows <- nrow(SYD13_SDft2)
numCols <- ncol(SYD13_SDft2)
SYD13_SDft3 <- SYD13_SDft2[c(2:numRows) , c(2:numCols)]
SYD13_SDTable <- graph.adjacency(SYD13_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(SYD13_SDTable, vertex.label = V(SYD13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
SYD13_SD.clusterCoef <- transitivity(SYD13_SDTable, type="global") #cluster coefficient
SYD13_SD.degreeCent <- centralization.degree(SYD13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_SDftn <- as.network.matrix(SYD13_SDft)
SYD13_SD.netDensity <- network.density(SYD13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_SD.entropy <- entropy(SYD13_SDft) #entropy

SYD13_SD.netMx <- cbind(SYD13_SD.netMx, SYD13_SD.clusterCoef, SYD13_SD.degreeCent$centralization,
                        SYD13_SD.netDensity, SYD13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
SYD13_TDg2 <- data.frame(SYD13_TD)
SYD13_TDg2 <- SYD13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_TDg2$player1
player2vector <- SYD13_TDg2$player2
SYD13_TDg3 <- SYD13_TDg2
SYD13_TDg3$p1inp2vec <- is.element(SYD13_TDg3$player1, player2vector)
SYD13_TDg3$p2inp1vec <- is.element(SYD13_TDg3$player2, player1vector)

addPlayer1 <- SYD13_TDg3[ which(SYD13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_TDg3[ which(SYD13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_TDg2 <- rbind(SYD13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
SYD13_TDft <- ftable(SYD13_TDg2$player1, SYD13_TDg2$player2)
SYD13_TDft2 <- as.matrix(SYD13_TDft)
numRows <- nrow(SYD13_TDft2)
numCols <- ncol(SYD13_TDft2)
SYD13_TDft3 <- SYD13_TDft2[c(2:numRows) , c(2:numCols)]
SYD13_TDTable <- graph.adjacency(SYD13_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(SYD13_TDTable, vertex.label = V(SYD13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
SYD13_TD.clusterCoef <- transitivity(SYD13_TDTable, type="global") #cluster coefficient
SYD13_TD.degreeCent <- centralization.degree(SYD13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_TDftn <- as.network.matrix(SYD13_TDft)
SYD13_TD.netDensity <- network.density(SYD13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_TD.entropy <- entropy(SYD13_TDft) #entropy

SYD13_TD.netMx <- cbind(SYD13_TD.netMx, SYD13_TD.clusterCoef, SYD13_TD.degreeCent$centralization,
                        SYD13_TD.netDensity, SYD13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
SYD13_QTg2 <- data.frame(SYD13_QT)
SYD13_QTg2 <- SYD13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD13_QTg2$player1
player2vector <- SYD13_QTg2$player2
SYD13_QTg3 <- SYD13_QTg2
SYD13_QTg3$p1inp2vec <- is.element(SYD13_QTg3$player1, player2vector)
SYD13_QTg3$p2inp1vec <- is.element(SYD13_QTg3$player2, player1vector)

addPlayer1 <- SYD13_QTg3[ which(SYD13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD13_QTg3[ which(SYD13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD13_QTg2 <- rbind(SYD13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
SYD13_QTft <- ftable(SYD13_QTg2$player1, SYD13_QTg2$player2)
SYD13_QTft2 <- as.matrix(SYD13_QTft)
numRows <- nrow(SYD13_QTft2)
numCols <- ncol(SYD13_QTft2)
SYD13_QTft3 <- SYD13_QTft2[c(2:numRows) , c(2:numCols)]
SYD13_QTTable <- graph.adjacency(SYD13_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(SYD13_QTTable, vertex.label = V(SYD13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
SYD13_QT.clusterCoef <- transitivity(SYD13_QTTable, type="global") #cluster coefficient
SYD13_QT.degreeCent <- centralization.degree(SYD13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD13_QTftn <- as.network.matrix(SYD13_QTft)
SYD13_QT.netDensity <- network.density(SYD13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD13_QT.entropy <- entropy(SYD13_QTft) #entropy

SYD13_QT.netMx <- cbind(SYD13_QT.netMx, SYD13_QT.clusterCoef, SYD13_QT.degreeCent$centralization,
                        SYD13_QT.netDensity, SYD13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD13_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 13
##

#ROUND 13, Goal***************************************************************
#NA

round = 13
teamName = "WB"
KIoutcome = "Goal_F"
WB13_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Goal with weighted edges
WB13_Gg2 <- data.frame(WB13_G)
WB13_Gg2 <- WB13_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_Gg2$player1
player2vector <- WB13_Gg2$player2
WB13_Gg3 <- WB13_Gg2
WB13_Gg3$p1inp2vec <- is.element(WB13_Gg3$player1, player2vector)
WB13_Gg3$p2inp1vec <- is.element(WB13_Gg3$player2, player1vector)

addPlayer1 <- WB13_Gg3[ which(WB13_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_Gg3[ which(WB13_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_Gg2 <- rbind(WB13_Gg2, addPlayers)

#ROUND 13, Goal graph using weighted edges
WB13_Gft <- ftable(WB13_Gg2$player1, WB13_Gg2$player2)
WB13_Gft2 <- as.matrix(WB13_Gft)
numRows <- nrow(WB13_Gft2)
numCols <- ncol(WB13_Gft2)
WB13_Gft3 <- WB13_Gft2[c(2:numRows) , c(2:numCols)]
WB13_GTable <- graph.adjacency(WB13_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 13, Goal graph=weighted
plot.igraph(WB13_GTable, vertex.label = V(WB13_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Goal calulation of network metrics
#igraph
WB13_G.clusterCoef <- transitivity(WB13_GTable, type="global") #cluster coefficient
WB13_G.degreeCent <- centralization.degree(WB13_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_Gftn <- as.network.matrix(WB13_Gft)
WB13_G.netDensity <- network.density(WB13_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_G.entropy <- entropy(WB13_Gft) #entropy

WB13_G.netMx <- cbind(WB13_G.netMx, WB13_G.clusterCoef, WB13_G.degreeCent$centralization,
                      WB13_G.netDensity, WB13_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_G.netMx) <- varnames

#ROUND 13, Behind***************************************************************
#NA

round = 13
teamName = "WB"
KIoutcome = "Behind_F"
WB13_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, Behind with weighted edges
WB13_Bg2 <- data.frame(WB13_B)
WB13_Bg2 <- WB13_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_Bg2$player1
player2vector <- WB13_Bg2$player2
WB13_Bg3 <- WB13_Bg2
WB13_Bg3$p1inp2vec <- is.element(WB13_Bg3$player1, player2vector)
WB13_Bg3$p2inp1vec <- is.element(WB13_Bg3$player2, player1vector)

addPlayer1 <- WB13_Bg3[ which(WB13_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_Bg3[ which(WB13_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_Bg2 <- rbind(WB13_Bg2, addPlayers)

#ROUND 13, Behind graph using weighted edges
WB13_Bft <- ftable(WB13_Bg2$player1, WB13_Bg2$player2)
WB13_Bft2 <- as.matrix(WB13_Bft)
numRows <- nrow(WB13_Bft2)
numCols <- ncol(WB13_Bft2)
WB13_Bft3 <- WB13_Bft2[c(2:numRows) , c(2:numCols)]
WB13_BTable <- graph.adjacency(WB13_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 13, Behind graph=weighted
plot.igraph(WB13_BTable, vertex.label = V(WB13_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, Behind calulation of network metrics
#igraph
WB13_B.clusterCoef <- transitivity(WB13_BTable, type="global") #cluster coefficient
WB13_B.degreeCent <- centralization.degree(WB13_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_Bftn <- as.network.matrix(WB13_Bft)
WB13_B.netDensity <- network.density(WB13_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_B.entropy <- entropy(WB13_Bft) #entropy

WB13_B.netMx <- cbind(WB13_B.netMx, WB13_B.clusterCoef, WB13_B.degreeCent$centralization,
                      WB13_B.netDensity, WB13_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_B.netMx) <- varnames

#ROUND 13, FWD Stoppage**********************************************************

round = 13
teamName = "WB"
KIoutcome = "Stoppage_F"
WB13_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Stoppage with weighted edges
WB13_SFg2 <- data.frame(WB13_SF)
WB13_SFg2 <- WB13_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_SFg2$player1
player2vector <- WB13_SFg2$player2
WB13_SFg3 <- WB13_SFg2
WB13_SFg3$p1inp2vec <- is.element(WB13_SFg3$player1, player2vector)
WB13_SFg3$p2inp1vec <- is.element(WB13_SFg3$player2, player1vector)

addPlayer1 <- WB13_SFg3[ which(WB13_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_SFg3[ which(WB13_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_SFg2 <- rbind(WB13_SFg2, addPlayers)

#ROUND 13, FWD Stoppage graph using weighted edges
WB13_SFft <- ftable(WB13_SFg2$player1, WB13_SFg2$player2)
WB13_SFft2 <- as.matrix(WB13_SFft)
numRows <- nrow(WB13_SFft2)
numCols <- ncol(WB13_SFft2)
WB13_SFft3 <- WB13_SFft2[c(2:numRows) , c(2:numCols)]
WB13_SFTable <- graph.adjacency(WB13_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, FWD Stoppage graph=weighted
plot.igraph(WB13_SFTable, vertex.label = V(WB13_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Stoppage calulation of network metrics
#igraph
WB13_SF.clusterCoef <- transitivity(WB13_SFTable, type="global") #cluster coefficient
WB13_SF.degreeCent <- centralization.degree(WB13_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_SFftn <- as.network.matrix(WB13_SFft)
WB13_SF.netDensity <- network.density(WB13_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_SF.entropy <- entropy(WB13_SFft) #entropy

WB13_SF.netMx <- cbind(WB13_SF.netMx, WB13_SF.clusterCoef, WB13_SF.degreeCent$centralization,
                       WB13_SF.netDensity, WB13_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_SF.netMx) <- varnames

#ROUND 13, FWD Turnover**********************************************************

round = 13
teamName = "WB"
KIoutcome = "Turnover_F"
WB13_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, FWD Turnover with weighted edges
WB13_TFg2 <- data.frame(WB13_TF)
WB13_TFg2 <- WB13_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_TFg2$player1
player2vector <- WB13_TFg2$player2
WB13_TFg3 <- WB13_TFg2
WB13_TFg3$p1inp2vec <- is.element(WB13_TFg3$player1, player2vector)
WB13_TFg3$p2inp1vec <- is.element(WB13_TFg3$player2, player1vector)

addPlayer1 <- WB13_TFg3[ which(WB13_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_TFg3[ which(WB13_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_TFg2 <- rbind(WB13_TFg2, addPlayers)

#ROUND 13, FWD Turnover graph using weighted edges
WB13_TFft <- ftable(WB13_TFg2$player1, WB13_TFg2$player2)
WB13_TFft2 <- as.matrix(WB13_TFft)
numRows <- nrow(WB13_TFft2)
numCols <- ncol(WB13_TFft2)
WB13_TFft3 <- WB13_TFft2[c(2:numRows) , c(2:numCols)]
WB13_TFTable <- graph.adjacency(WB13_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, FWD Turnover graph=weighted
plot.igraph(WB13_TFTable, vertex.label = V(WB13_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, FWD Turnover calulation of network metrics
#igraph
WB13_TF.clusterCoef <- transitivity(WB13_TFTable, type="global") #cluster coefficient
WB13_TF.degreeCent <- centralization.degree(WB13_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_TFftn <- as.network.matrix(WB13_TFft)
WB13_TF.netDensity <- network.density(WB13_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_TF.entropy <- entropy(WB13_TFft) #entropy

WB13_TF.netMx <- cbind(WB13_TF.netMx, WB13_TF.clusterCoef, WB13_TF.degreeCent$centralization,
                       WB13_TF.netDensity, WB13_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_TF.netMx) <- varnames

#ROUND 13, AM Stoppage**********************************************************
#NA

round = 13
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB13_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Stoppage with weighted edges
WB13_SAMg2 <- data.frame(WB13_SAM)
WB13_SAMg2 <- WB13_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_SAMg2$player1
player2vector <- WB13_SAMg2$player2
WB13_SAMg3 <- WB13_SAMg2
WB13_SAMg3$p1inp2vec <- is.element(WB13_SAMg3$player1, player2vector)
WB13_SAMg3$p2inp1vec <- is.element(WB13_SAMg3$player2, player1vector)

addPlayer1 <- WB13_SAMg3[ which(WB13_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_SAMg3[ which(WB13_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_SAMg2 <- rbind(WB13_SAMg2, addPlayers)

#ROUND 13, AM Stoppage graph using weighted edges
WB13_SAMft <- ftable(WB13_SAMg2$player1, WB13_SAMg2$player2)
WB13_SAMft2 <- as.matrix(WB13_SAMft)
numRows <- nrow(WB13_SAMft2)
numCols <- ncol(WB13_SAMft2)
WB13_SAMft3 <- WB13_SAMft2[c(2:numRows) , c(2:numCols)]
WB13_SAMTable <- graph.adjacency(WB13_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, AM Stoppage graph=weighted
plot.igraph(WB13_SAMTable, vertex.label = V(WB13_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Stoppage calulation of network metrics
#igraph
WB13_SAM.clusterCoef <- transitivity(WB13_SAMTable, type="global") #cluster coefficient
WB13_SAM.degreeCent <- centralization.degree(WB13_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_SAMftn <- as.network.matrix(WB13_SAMft)
WB13_SAM.netDensity <- network.density(WB13_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_SAM.entropy <- entropy(WB13_SAMft) #entropy

WB13_SAM.netMx <- cbind(WB13_SAM.netMx, WB13_SAM.clusterCoef, WB13_SAM.degreeCent$centralization,
                        WB13_SAM.netDensity, WB13_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_SAM.netMx) <- varnames

#ROUND 13, AM Turnover**********************************************************

round = 13
teamName = "WB"
KIoutcome = "Turnover_AM"
WB13_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, AM Turnover with weighted edges
WB13_TAMg2 <- data.frame(WB13_TAM)
WB13_TAMg2 <- WB13_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_TAMg2$player1
player2vector <- WB13_TAMg2$player2
WB13_TAMg3 <- WB13_TAMg2
WB13_TAMg3$p1inp2vec <- is.element(WB13_TAMg3$player1, player2vector)
WB13_TAMg3$p2inp1vec <- is.element(WB13_TAMg3$player2, player1vector)

addPlayer1 <- WB13_TAMg3[ which(WB13_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_TAMg3[ which(WB13_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_TAMg2 <- rbind(WB13_TAMg2, addPlayers)

#ROUND 13, AM Turnover graph using weighted edges
WB13_TAMft <- ftable(WB13_TAMg2$player1, WB13_TAMg2$player2)
WB13_TAMft2 <- as.matrix(WB13_TAMft)
numRows <- nrow(WB13_TAMft2)
numCols <- ncol(WB13_TAMft2)
WB13_TAMft3 <- WB13_TAMft2[c(2:numRows) , c(2:numCols)]
WB13_TAMTable <- graph.adjacency(WB13_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, AM Turnover graph=weighted
plot.igraph(WB13_TAMTable, vertex.label = V(WB13_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, AM Turnover calulation of network metrics
#igraph
WB13_TAM.clusterCoef <- transitivity(WB13_TAMTable, type="global") #cluster coefficient
WB13_TAM.degreeCent <- centralization.degree(WB13_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_TAMftn <- as.network.matrix(WB13_TAMft)
WB13_TAM.netDensity <- network.density(WB13_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_TAM.entropy <- entropy(WB13_TAMft) #entropy

WB13_TAM.netMx <- cbind(WB13_TAM.netMx, WB13_TAM.clusterCoef, WB13_TAM.degreeCent$centralization,
                        WB13_TAM.netDensity, WB13_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_TAM.netMx) <- varnames

#ROUND 13, DM Stoppage**********************************************************

round = 13
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB13_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Stoppage with weighted edges
WB13_SDMg2 <- data.frame(WB13_SDM)
WB13_SDMg2 <- WB13_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_SDMg2$player1
player2vector <- WB13_SDMg2$player2
WB13_SDMg3 <- WB13_SDMg2
WB13_SDMg3$p1inp2vec <- is.element(WB13_SDMg3$player1, player2vector)
WB13_SDMg3$p2inp1vec <- is.element(WB13_SDMg3$player2, player1vector)

addPlayer1 <- WB13_SDMg3[ which(WB13_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_SDMg3[ which(WB13_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_SDMg2 <- rbind(WB13_SDMg2, addPlayers)

#ROUND 13, DM Stoppage graph using weighted edges
WB13_SDMft <- ftable(WB13_SDMg2$player1, WB13_SDMg2$player2)
WB13_SDMft2 <- as.matrix(WB13_SDMft)
numRows <- nrow(WB13_SDMft2)
numCols <- ncol(WB13_SDMft2)
WB13_SDMft3 <- WB13_SDMft2[c(2:numRows) , c(2:numCols)]
WB13_SDMTable <- graph.adjacency(WB13_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, DM Stoppage graph=weighted
plot.igraph(WB13_SDMTable, vertex.label = V(WB13_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Stoppage calulation of network metrics
#igraph
WB13_SDM.clusterCoef <- transitivity(WB13_SDMTable, type="global") #cluster coefficient
WB13_SDM.degreeCent <- centralization.degree(WB13_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_SDMftn <- as.network.matrix(WB13_SDMft)
WB13_SDM.netDensity <- network.density(WB13_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_SDM.entropy <- entropy(WB13_SDMft) #entropy

WB13_SDM.netMx <- cbind(WB13_SDM.netMx, WB13_SDM.clusterCoef, WB13_SDM.degreeCent$centralization,
                        WB13_SDM.netDensity, WB13_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_SDM.netMx) <- varnames

#ROUND 13, DM Turnover**********************************************************

round = 13
teamName = "WB"
KIoutcome = "Turnover_DM"
WB13_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, DM Turnover with weighted edges
WB13_TDMg2 <- data.frame(WB13_TDM)
WB13_TDMg2 <- WB13_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_TDMg2$player1
player2vector <- WB13_TDMg2$player2
WB13_TDMg3 <- WB13_TDMg2
WB13_TDMg3$p1inp2vec <- is.element(WB13_TDMg3$player1, player2vector)
WB13_TDMg3$p2inp1vec <- is.element(WB13_TDMg3$player2, player1vector)

addPlayer1 <- WB13_TDMg3[ which(WB13_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_TDMg3[ which(WB13_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_TDMg2 <- rbind(WB13_TDMg2, addPlayers)

#ROUND 13, DM Turnover graph using weighted edges
WB13_TDMft <- ftable(WB13_TDMg2$player1, WB13_TDMg2$player2)
WB13_TDMft2 <- as.matrix(WB13_TDMft)
numRows <- nrow(WB13_TDMft2)
numCols <- ncol(WB13_TDMft2)
WB13_TDMft3 <- WB13_TDMft2[c(2:numRows) , c(2:numCols)]
WB13_TDMTable <- graph.adjacency(WB13_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 13, DM Turnover graph=weighted
plot.igraph(WB13_TDMTable, vertex.label = V(WB13_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, DM Turnover calulation of network metrics
#igraph
WB13_TDM.clusterCoef <- transitivity(WB13_TDMTable, type="global") #cluster coefficient
WB13_TDM.degreeCent <- centralization.degree(WB13_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_TDMftn <- as.network.matrix(WB13_TDMft)
WB13_TDM.netDensity <- network.density(WB13_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_TDM.entropy <- entropy(WB13_TDMft) #entropy

WB13_TDM.netMx <- cbind(WB13_TDM.netMx, WB13_TDM.clusterCoef, WB13_TDM.degreeCent$centralization,
                        WB13_TDM.netDensity, WB13_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_TDM.netMx) <- varnames

#ROUND 13, D Stoppage**********************************************************
#NA

round = 13
teamName = "WB"
KIoutcome = "Stoppage_D"
WB13_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Stoppage with weighted edges
WB13_SDg2 <- data.frame(WB13_SD)
WB13_SDg2 <- WB13_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_SDg2$player1
player2vector <- WB13_SDg2$player2
WB13_SDg3 <- WB13_SDg2
WB13_SDg3$p1inp2vec <- is.element(WB13_SDg3$player1, player2vector)
WB13_SDg3$p2inp1vec <- is.element(WB13_SDg3$player2, player1vector)

addPlayer1 <- WB13_SDg3[ which(WB13_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_SDg3[ which(WB13_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_SDg2 <- rbind(WB13_SDg2, addPlayers)

#ROUND 13, D Stoppage graph using weighted edges
WB13_SDft <- ftable(WB13_SDg2$player1, WB13_SDg2$player2)
WB13_SDft2 <- as.matrix(WB13_SDft)
numRows <- nrow(WB13_SDft2)
numCols <- ncol(WB13_SDft2)
WB13_SDft3 <- WB13_SDft2[c(2:numRows) , c(2:numCols)]
WB13_SDTable <- graph.adjacency(WB13_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, D Stoppage graph=weighted
plot.igraph(WB13_SDTable, vertex.label = V(WB13_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Stoppage calulation of network metrics
#igraph
WB13_SD.clusterCoef <- transitivity(WB13_SDTable, type="global") #cluster coefficient
WB13_SD.degreeCent <- centralization.degree(WB13_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_SDftn <- as.network.matrix(WB13_SDft)
WB13_SD.netDensity <- network.density(WB13_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_SD.entropy <- entropy(WB13_SDft) #entropy

WB13_SD.netMx <- cbind(WB13_SD.netMx, WB13_SD.clusterCoef, WB13_SD.degreeCent$centralization,
                       WB13_SD.netDensity, WB13_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_SD.netMx) <- varnames

#ROUND 13, D Turnover**********************************************************
#NA

round = 13
teamName = "WB"
KIoutcome = "Turnover_D"
WB13_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, D Turnover with weighted edges
WB13_TDg2 <- data.frame(WB13_TD)
WB13_TDg2 <- WB13_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_TDg2$player1
player2vector <- WB13_TDg2$player2
WB13_TDg3 <- WB13_TDg2
WB13_TDg3$p1inp2vec <- is.element(WB13_TDg3$player1, player2vector)
WB13_TDg3$p2inp1vec <- is.element(WB13_TDg3$player2, player1vector)

addPlayer1 <- WB13_TDg3[ which(WB13_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_TDg3[ which(WB13_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_TDg2 <- rbind(WB13_TDg2, addPlayers)

#ROUND 13, D Turnover graph using weighted edges
WB13_TDft <- ftable(WB13_TDg2$player1, WB13_TDg2$player2)
WB13_TDft2 <- as.matrix(WB13_TDft)
numRows <- nrow(WB13_TDft2)
numCols <- ncol(WB13_TDft2)
WB13_TDft3 <- WB13_TDft2[c(2:numRows) , c(2:numCols)]
WB13_TDTable <- graph.adjacency(WB13_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, D Turnover graph=weighted
plot.igraph(WB13_TDTable, vertex.label = V(WB13_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, D Turnover calulation of network metrics
#igraph
WB13_TD.clusterCoef <- transitivity(WB13_TDTable, type="global") #cluster coefficient
WB13_TD.degreeCent <- centralization.degree(WB13_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_TDftn <- as.network.matrix(WB13_TDft)
WB13_TD.netDensity <- network.density(WB13_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_TD.entropy <- entropy(WB13_TDft) #entropy

WB13_TD.netMx <- cbind(WB13_TD.netMx, WB13_TD.clusterCoef, WB13_TD.degreeCent$centralization,
                       WB13_TD.netDensity, WB13_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_TD.netMx) <- varnames

#ROUND 13, End of Qtr**********************************************************
#NA

round = 13
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB13_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 13, End of Qtr with weighted edges
WB13_QTg2 <- data.frame(WB13_QT)
WB13_QTg2 <- WB13_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB13_QTg2$player1
player2vector <- WB13_QTg2$player2
WB13_QTg3 <- WB13_QTg2
WB13_QTg3$p1inp2vec <- is.element(WB13_QTg3$player1, player2vector)
WB13_QTg3$p2inp1vec <- is.element(WB13_QTg3$player2, player1vector)

addPlayer1 <- WB13_QTg3[ which(WB13_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB13_QTg3[ which(WB13_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB13_QTg2 <- rbind(WB13_QTg2, addPlayers)

#ROUND 13, End of Qtr graph using weighted edges
WB13_QTft <- ftable(WB13_QTg2$player1, WB13_QTg2$player2)
WB13_QTft2 <- as.matrix(WB13_QTft)
numRows <- nrow(WB13_QTft2)
numCols <- ncol(WB13_QTft2)
WB13_QTft3 <- WB13_QTft2[c(2:numRows) , c(2:numCols)]
WB13_QTTable <- graph.adjacency(WB13_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 13, End of Qtr graph=weighted
plot.igraph(WB13_QTTable, vertex.label = V(WB13_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB13_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 13, End of Qtr calulation of network metrics
#igraph
WB13_QT.clusterCoef <- transitivity(WB13_QTTable, type="global") #cluster coefficient
WB13_QT.degreeCent <- centralization.degree(WB13_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB13_QTftn <- as.network.matrix(WB13_QTft)
WB13_QT.netDensity <- network.density(WB13_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB13_QT.entropy <- entropy(WB13_QTft) #entropy

WB13_QT.netMx <- cbind(WB13_QT.netMx, WB13_QT.clusterCoef, WB13_QT.degreeCent$centralization,
                       WB13_QT.netDensity, WB13_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB13_QT.netMx) <- varnames
