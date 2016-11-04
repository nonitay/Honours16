#####
#10-02-16- Real data 19
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
ADEL19_Gg2 <- data.frame(ADEL19_G)
ADEL19_Gg2 <- ADEL19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_Gg2$player1
player2vector <- ADEL19_Gg2$player2
ADEL19_Gg3 <- ADEL19_Gg2
ADEL19_Gg3$p1inp2vec <- is.element(ADEL19_Gg3$player1, player2vector)
ADEL19_Gg3$p2inp1vec <- is.element(ADEL19_Gg3$player2, player1vector)

addPlayer1 <- ADEL19_Gg3[ which(ADEL19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_Gg3[ which(ADEL19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_Gg2 <- rbind(ADEL19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
ADEL19_Gft <- ftable(ADEL19_Gg2$player1, ADEL19_Gg2$player2)
ADEL19_Gft2 <- as.matrix(ADEL19_Gft)
numRows <- nrow(ADEL19_Gft2)
numCols <- ncol(ADEL19_Gft2)
ADEL19_Gft3 <- ADEL19_Gft2[c(2:numRows) , c(2:numCols)]
ADEL19_GTable <- graph.adjacency(ADEL19_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(ADEL19_GTable, vertex.label = V(ADEL19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
ADEL19_G.clusterCoef <- transitivity(ADEL19_GTable, type="global") #cluster coefficient
ADEL19_G.degreeCent <- centralization.degree(ADEL19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_Gftn <- as.network.matrix(ADEL19_Gft)
ADEL19_G.netDensity <- network.density(ADEL19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_G.entropy <- entropy(ADEL19_Gft) #entropy

ADEL19_G.netMx <- cbind(ADEL19_G.netMx, ADEL19_G.clusterCoef, ADEL19_G.degreeCent$centralization,
                        ADEL19_G.netDensity, ADEL19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
ADEL19_Bg2 <- data.frame(ADEL19_B)
ADEL19_Bg2 <- ADEL19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_Bg2$player1
player2vector <- ADEL19_Bg2$player2
ADEL19_Bg3 <- ADEL19_Bg2
ADEL19_Bg3$p1inp2vec <- is.element(ADEL19_Bg3$player1, player2vector)
ADEL19_Bg3$p2inp1vec <- is.element(ADEL19_Bg3$player2, player1vector)

addPlayer1 <- ADEL19_Bg3[ which(ADEL19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_Bg3[ which(ADEL19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_Bg2 <- rbind(ADEL19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
ADEL19_Bft <- ftable(ADEL19_Bg2$player1, ADEL19_Bg2$player2)
ADEL19_Bft2 <- as.matrix(ADEL19_Bft)
numRows <- nrow(ADEL19_Bft2)
numCols <- ncol(ADEL19_Bft2)
ADEL19_Bft3 <- ADEL19_Bft2[c(2:numRows) , c(2:numCols)]
ADEL19_BTable <- graph.adjacency(ADEL19_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(ADEL19_BTable, vertex.label = V(ADEL19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
ADEL19_B.clusterCoef <- transitivity(ADEL19_BTable, type="global") #cluster coefficient
ADEL19_B.degreeCent <- centralization.degree(ADEL19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_Bftn <- as.network.matrix(ADEL19_Bft)
ADEL19_B.netDensity <- network.density(ADEL19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_B.entropy <- entropy(ADEL19_Bft) #entropy

ADEL19_B.netMx <- cbind(ADEL19_B.netMx, ADEL19_B.clusterCoef, ADEL19_B.degreeCent$centralization,
                        ADEL19_B.netDensity, ADEL19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
ADEL19_SFg2 <- data.frame(ADEL19_SF)
ADEL19_SFg2 <- ADEL19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_SFg2$player1
player2vector <- ADEL19_SFg2$player2
ADEL19_SFg3 <- ADEL19_SFg2
ADEL19_SFg3$p1inp2vec <- is.element(ADEL19_SFg3$player1, player2vector)
ADEL19_SFg3$p2inp1vec <- is.element(ADEL19_SFg3$player2, player1vector)

addPlayer1 <- ADEL19_SFg3[ which(ADEL19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_SFg3[ which(ADEL19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_SFg2 <- rbind(ADEL19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
ADEL19_SFft <- ftable(ADEL19_SFg2$player1, ADEL19_SFg2$player2)
ADEL19_SFft2 <- as.matrix(ADEL19_SFft)
numRows <- nrow(ADEL19_SFft2)
numCols <- ncol(ADEL19_SFft2)
ADEL19_SFft3 <- ADEL19_SFft2[c(2:numRows) , c(2:numCols)]
ADEL19_SFTable <- graph.adjacency(ADEL19_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(ADEL19_SFTable, vertex.label = V(ADEL19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
ADEL19_SF.clusterCoef <- transitivity(ADEL19_SFTable, type="global") #cluster coefficient
ADEL19_SF.degreeCent <- centralization.degree(ADEL19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_SFftn <- as.network.matrix(ADEL19_SFft)
ADEL19_SF.netDensity <- network.density(ADEL19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_SF.entropy <- entropy(ADEL19_SFft) #entropy

ADEL19_SF.netMx <- cbind(ADEL19_SF.netMx, ADEL19_SF.clusterCoef, ADEL19_SF.degreeCent$centralization,
                         ADEL19_SF.netDensity, ADEL19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
ADEL19_TFg2 <- data.frame(ADEL19_TF)
ADEL19_TFg2 <- ADEL19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_TFg2$player1
player2vector <- ADEL19_TFg2$player2
ADEL19_TFg3 <- ADEL19_TFg2
ADEL19_TFg3$p1inp2vec <- is.element(ADEL19_TFg3$player1, player2vector)
ADEL19_TFg3$p2inp1vec <- is.element(ADEL19_TFg3$player2, player1vector)

addPlayer1 <- ADEL19_TFg3[ which(ADEL19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_TFg3[ which(ADEL19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_TFg2 <- rbind(ADEL19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
ADEL19_TFft <- ftable(ADEL19_TFg2$player1, ADEL19_TFg2$player2)
ADEL19_TFft2 <- as.matrix(ADEL19_TFft)
numRows <- nrow(ADEL19_TFft2)
numCols <- ncol(ADEL19_TFft2)
ADEL19_TFft3 <- ADEL19_TFft2[c(2:numRows) , c(2:numCols)]
ADEL19_TFTable <- graph.adjacency(ADEL19_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(ADEL19_TFTable, vertex.label = V(ADEL19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
ADEL19_TF.clusterCoef <- transitivity(ADEL19_TFTable, type="global") #cluster coefficient
ADEL19_TF.degreeCent <- centralization.degree(ADEL19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_TFftn <- as.network.matrix(ADEL19_TFft)
ADEL19_TF.netDensity <- network.density(ADEL19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_TF.entropy <- entropy(ADEL19_TFft) #entropy

ADEL19_TF.netMx <- cbind(ADEL19_TF.netMx, ADEL19_TF.clusterCoef, ADEL19_TF.degreeCent$centralization,
                         ADEL19_TF.netDensity, ADEL19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
ADEL19_SAMg2 <- data.frame(ADEL19_SAM)
ADEL19_SAMg2 <- ADEL19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_SAMg2$player1
player2vector <- ADEL19_SAMg2$player2
ADEL19_SAMg3 <- ADEL19_SAMg2
ADEL19_SAMg3$p1inp2vec <- is.element(ADEL19_SAMg3$player1, player2vector)
ADEL19_SAMg3$p2inp1vec <- is.element(ADEL19_SAMg3$player2, player1vector)

addPlayer1 <- ADEL19_SAMg3[ which(ADEL19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_SAMg3[ which(ADEL19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_SAMg2 <- rbind(ADEL19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
ADEL19_SAMft <- ftable(ADEL19_SAMg2$player1, ADEL19_SAMg2$player2)
ADEL19_SAMft2 <- as.matrix(ADEL19_SAMft)
numRows <- nrow(ADEL19_SAMft2)
numCols <- ncol(ADEL19_SAMft2)
ADEL19_SAMft3 <- ADEL19_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL19_SAMTable <- graph.adjacency(ADEL19_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(ADEL19_SAMTable, vertex.label = V(ADEL19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
ADEL19_SAM.clusterCoef <- transitivity(ADEL19_SAMTable, type="global") #cluster coefficient
ADEL19_SAM.degreeCent <- centralization.degree(ADEL19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_SAMftn <- as.network.matrix(ADEL19_SAMft)
ADEL19_SAM.netDensity <- network.density(ADEL19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_SAM.entropy <- entropy(ADEL19_SAMft) #entropy

ADEL19_SAM.netMx <- cbind(ADEL19_SAM.netMx, ADEL19_SAM.clusterCoef, ADEL19_SAM.degreeCent$centralization,
                          ADEL19_SAM.netDensity, ADEL19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
ADEL19_TAMg2 <- data.frame(ADEL19_TAM)
ADEL19_TAMg2 <- ADEL19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_TAMg2$player1
player2vector <- ADEL19_TAMg2$player2
ADEL19_TAMg3 <- ADEL19_TAMg2
ADEL19_TAMg3$p1inp2vec <- is.element(ADEL19_TAMg3$player1, player2vector)
ADEL19_TAMg3$p2inp1vec <- is.element(ADEL19_TAMg3$player2, player1vector)

#Only need to add row for player 2 (one player presents twice in player 1)
empty <- ""
zero <- 0
addPlayer2 <- ADEL19_TAMg3[ which(ADEL19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

ADEL19_TAMg2 <- rbind(ADEL19_TAMg2, addPlayer2)

#ROUND 19, AM Turnover graph using weighted edges
ADEL19_TAMft <- ftable(ADEL19_TAMg2$player1, ADEL19_TAMg2$player2)
ADEL19_TAMft2 <- as.matrix(ADEL19_TAMft)
numRows <- nrow(ADEL19_TAMft2)
numCols <- ncol(ADEL19_TAMft2)
ADEL19_TAMft3 <- ADEL19_TAMft2[c(1:numRows) , c(2:numCols)]
ADEL19_TAMTable <- graph.adjacency(ADEL19_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(ADEL19_TAMTable, vertex.label = V(ADEL19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
ADEL19_TAM.clusterCoef <- transitivity(ADEL19_TAMTable, type="global") #cluster coefficient
ADEL19_TAM.degreeCent <- centralization.degree(ADEL19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_TAMftn <- as.network.matrix(ADEL19_TAMft)
ADEL19_TAM.netDensity <- network.density(ADEL19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_TAM.entropy <- entropy(ADEL19_TAMft) #entropy

ADEL19_TAM.netMx <- cbind(ADEL19_TAM.netMx, ADEL19_TAM.clusterCoef, ADEL19_TAM.degreeCent$centralization,
                          ADEL19_TAM.netDensity, ADEL19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
ADEL19_SDMg2 <- data.frame(ADEL19_SDM)
ADEL19_SDMg2 <- ADEL19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_SDMg2$player1
player2vector <- ADEL19_SDMg2$player2
ADEL19_SDMg3 <- ADEL19_SDMg2
ADEL19_SDMg3$p1inp2vec <- is.element(ADEL19_SDMg3$player1, player2vector)
ADEL19_SDMg3$p2inp1vec <- is.element(ADEL19_SDMg3$player2, player1vector)

addPlayer1 <- ADEL19_SDMg3[ which(ADEL19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_SDMg3[ which(ADEL19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_SDMg2 <- rbind(ADEL19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
ADEL19_SDMft <- ftable(ADEL19_SDMg2$player1, ADEL19_SDMg2$player2)
ADEL19_SDMft2 <- as.matrix(ADEL19_SDMft)
numRows <- nrow(ADEL19_SDMft2)
numCols <- ncol(ADEL19_SDMft2)
ADEL19_SDMft3 <- ADEL19_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL19_SDMTable <- graph.adjacency(ADEL19_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(ADEL19_SDMTable, vertex.label = V(ADEL19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
ADEL19_SDM.clusterCoef <- transitivity(ADEL19_SDMTable, type="global") #cluster coefficient
ADEL19_SDM.degreeCent <- centralization.degree(ADEL19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_SDMftn <- as.network.matrix(ADEL19_SDMft)
ADEL19_SDM.netDensity <- network.density(ADEL19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_SDM.entropy <- entropy(ADEL19_SDMft) #entropy

ADEL19_SDM.netMx <- cbind(ADEL19_SDM.netMx, ADEL19_SDM.clusterCoef, ADEL19_SDM.degreeCent$centralization,
                          ADEL19_SDM.netDensity, ADEL19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
ADEL19_TDMg2 <- data.frame(ADEL19_TDM)
ADEL19_TDMg2 <- ADEL19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_TDMg2$player1
player2vector <- ADEL19_TDMg2$player2
ADEL19_TDMg3 <- ADEL19_TDMg2
ADEL19_TDMg3$p1inp2vec <- is.element(ADEL19_TDMg3$player1, player2vector)
ADEL19_TDMg3$p2inp1vec <- is.element(ADEL19_TDMg3$player2, player1vector)

addPlayer1 <- ADEL19_TDMg3[ which(ADEL19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_TDMg3[ which(ADEL19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_TDMg2 <- rbind(ADEL19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
ADEL19_TDMft <- ftable(ADEL19_TDMg2$player1, ADEL19_TDMg2$player2)
ADEL19_TDMft2 <- as.matrix(ADEL19_TDMft)
numRows <- nrow(ADEL19_TDMft2)
numCols <- ncol(ADEL19_TDMft2)
ADEL19_TDMft3 <- ADEL19_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL19_TDMTable <- graph.adjacency(ADEL19_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(ADEL19_TDMTable, vertex.label = V(ADEL19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
ADEL19_TDM.clusterCoef <- transitivity(ADEL19_TDMTable, type="global") #cluster coefficient
ADEL19_TDM.degreeCent <- centralization.degree(ADEL19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_TDMftn <- as.network.matrix(ADEL19_TDMft)
ADEL19_TDM.netDensity <- network.density(ADEL19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_TDM.entropy <- entropy(ADEL19_TDMft) #entropy

ADEL19_TDM.netMx <- cbind(ADEL19_TDM.netMx, ADEL19_TDM.clusterCoef, ADEL19_TDM.degreeCent$centralization,
                          ADEL19_TDM.netDensity, ADEL19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
ADEL19_SDg2 <- data.frame(ADEL19_SD)
ADEL19_SDg2 <- ADEL19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_SDg2$player1
player2vector <- ADEL19_SDg2$player2
ADEL19_SDg3 <- ADEL19_SDg2
ADEL19_SDg3$p1inp2vec <- is.element(ADEL19_SDg3$player1, player2vector)
ADEL19_SDg3$p2inp1vec <- is.element(ADEL19_SDg3$player2, player1vector)

addPlayer1 <- ADEL19_SDg3[ which(ADEL19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_SDg3[ which(ADEL19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_SDg2 <- rbind(ADEL19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
ADEL19_SDft <- ftable(ADEL19_SDg2$player1, ADEL19_SDg2$player2)
ADEL19_SDft2 <- as.matrix(ADEL19_SDft)
numRows <- nrow(ADEL19_SDft2)
numCols <- ncol(ADEL19_SDft2)
ADEL19_SDft3 <- ADEL19_SDft2[c(2:numRows) , c(2:numCols)]
ADEL19_SDTable <- graph.adjacency(ADEL19_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(ADEL19_SDTable, vertex.label = V(ADEL19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
ADEL19_SD.clusterCoef <- transitivity(ADEL19_SDTable, type="global") #cluster coefficient
ADEL19_SD.degreeCent <- centralization.degree(ADEL19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_SDftn <- as.network.matrix(ADEL19_SDft)
ADEL19_SD.netDensity <- network.density(ADEL19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_SD.entropy <- entropy(ADEL19_SDft) #entropy

ADEL19_SD.netMx <- cbind(ADEL19_SD.netMx, ADEL19_SD.clusterCoef, ADEL19_SD.degreeCent$centralization,
                         ADEL19_SD.netDensity, ADEL19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
ADEL19_TDg2 <- data.frame(ADEL19_TD)
ADEL19_TDg2 <- ADEL19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_TDg2$player1
player2vector <- ADEL19_TDg2$player2
ADEL19_TDg3 <- ADEL19_TDg2
ADEL19_TDg3$p1inp2vec <- is.element(ADEL19_TDg3$player1, player2vector)
ADEL19_TDg3$p2inp1vec <- is.element(ADEL19_TDg3$player2, player1vector)

addPlayer1 <- ADEL19_TDg3[ which(ADEL19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_TDg3[ which(ADEL19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_TDg2 <- rbind(ADEL19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
ADEL19_TDft <- ftable(ADEL19_TDg2$player1, ADEL19_TDg2$player2)
ADEL19_TDft2 <- as.matrix(ADEL19_TDft)
numRows <- nrow(ADEL19_TDft2)
numCols <- ncol(ADEL19_TDft2)
ADEL19_TDft3 <- ADEL19_TDft2[c(2:numRows) , c(2:numCols)]
ADEL19_TDTable <- graph.adjacency(ADEL19_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(ADEL19_TDTable, vertex.label = V(ADEL19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
ADEL19_TD.clusterCoef <- transitivity(ADEL19_TDTable, type="global") #cluster coefficient
ADEL19_TD.degreeCent <- centralization.degree(ADEL19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_TDftn <- as.network.matrix(ADEL19_TDft)
ADEL19_TD.netDensity <- network.density(ADEL19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_TD.entropy <- entropy(ADEL19_TDft) #entropy

ADEL19_TD.netMx <- cbind(ADEL19_TD.netMx, ADEL19_TD.clusterCoef, ADEL19_TD.degreeCent$centralization,
                         ADEL19_TD.netDensity, ADEL19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
ADEL19_QTg2 <- data.frame(ADEL19_QT)
ADEL19_QTg2 <- ADEL19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL19_QTg2$player1
player2vector <- ADEL19_QTg2$player2
ADEL19_QTg3 <- ADEL19_QTg2
ADEL19_QTg3$p1inp2vec <- is.element(ADEL19_QTg3$player1, player2vector)
ADEL19_QTg3$p2inp1vec <- is.element(ADEL19_QTg3$player2, player1vector)

addPlayer1 <- ADEL19_QTg3[ which(ADEL19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL19_QTg3[ which(ADEL19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL19_QTg2 <- rbind(ADEL19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
ADEL19_QTft <- ftable(ADEL19_QTg2$player1, ADEL19_QTg2$player2)
ADEL19_QTft2 <- as.matrix(ADEL19_QTft)
numRows <- nrow(ADEL19_QTft2)
numCols <- ncol(ADEL19_QTft2)
ADEL19_QTft3 <- ADEL19_QTft2[c(2:numRows) , c(2:numCols)]
ADEL19_QTTable <- graph.adjacency(ADEL19_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(ADEL19_QTTable, vertex.label = V(ADEL19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
ADEL19_QT.clusterCoef <- transitivity(ADEL19_QTTable, type="global") #cluster coefficient
ADEL19_QT.degreeCent <- centralization.degree(ADEL19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL19_QTftn <- as.network.matrix(ADEL19_QTft)
ADEL19_QT.netDensity <- network.density(ADEL19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL19_QT.entropy <- entropy(ADEL19_QTft) #entropy

ADEL19_QT.netMx <- cbind(ADEL19_QT.netMx, ADEL19_QT.clusterCoef, ADEL19_QT.degreeCent$centralization,
                         ADEL19_QT.netDensity, ADEL19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL19_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************

round = 19
teamName = "BL"
KIoutcome = "Goal_F"
BL19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
BL19_Gg2 <- data.frame(BL19_G)
BL19_Gg2 <- BL19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_Gg2$player1
player2vector <- BL19_Gg2$player2
BL19_Gg3 <- BL19_Gg2
BL19_Gg3$p1inp2vec <- is.element(BL19_Gg3$player1, player2vector)
BL19_Gg3$p2inp1vec <- is.element(BL19_Gg3$player2, player1vector)

addPlayer1 <- BL19_Gg3[ which(BL19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_Gg3[ which(BL19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_Gg2 <- rbind(BL19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
BL19_Gft <- ftable(BL19_Gg2$player1, BL19_Gg2$player2)
BL19_Gft2 <- as.matrix(BL19_Gft)
numRows <- nrow(BL19_Gft2)
numCols <- ncol(BL19_Gft2)
BL19_Gft3 <- BL19_Gft2[c(2:numRows) , c(2:numCols)]
BL19_GTable <- graph.adjacency(BL19_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(BL19_GTable, vertex.label = V(BL19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
BL19_G.clusterCoef <- transitivity(BL19_GTable, type="global") #cluster coefficient
BL19_G.degreeCent <- centralization.degree(BL19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_Gftn <- as.network.matrix(BL19_Gft)
BL19_G.netDensity <- network.density(BL19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_G.entropy <- entropy(BL19_Gft) #entropy

BL19_G.netMx <- cbind(BL19_G.netMx, BL19_G.clusterCoef, BL19_G.degreeCent$centralization,
                      BL19_G.netDensity, BL19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "BL"
KIoutcome = "Behind_F"
BL19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
BL19_Bg2 <- data.frame(BL19_B)
BL19_Bg2 <- BL19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_Bg2$player1
player2vector <- BL19_Bg2$player2
BL19_Bg3 <- BL19_Bg2
BL19_Bg3$p1inp2vec <- is.element(BL19_Bg3$player1, player2vector)
BL19_Bg3$p2inp1vec <- is.element(BL19_Bg3$player2, player1vector)

addPlayer1 <- BL19_Bg3[ which(BL19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_Bg3[ which(BL19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_Bg2 <- rbind(BL19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
BL19_Bft <- ftable(BL19_Bg2$player1, BL19_Bg2$player2)
BL19_Bft2 <- as.matrix(BL19_Bft)
numRows <- nrow(BL19_Bft2)
numCols <- ncol(BL19_Bft2)
BL19_Bft3 <- BL19_Bft2[c(2:numRows) , c(2:numCols)]
BL19_BTable <- graph.adjacency(BL19_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(BL19_BTable, vertex.label = V(BL19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
BL19_B.clusterCoef <- transitivity(BL19_BTable, type="global") #cluster coefficient
BL19_B.degreeCent <- centralization.degree(BL19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_Bftn <- as.network.matrix(BL19_Bft)
BL19_B.netDensity <- network.density(BL19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_B.entropy <- entropy(BL19_Bft) #entropy

BL19_B.netMx <- cbind(BL19_B.netMx, BL19_B.clusterCoef, BL19_B.degreeCent$centralization,
                      BL19_B.netDensity, BL19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "BL"
KIoutcome = "Stoppage_F"
BL19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
BL19_SFg2 <- data.frame(BL19_SF)
BL19_SFg2 <- BL19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_SFg2$player1
player2vector <- BL19_SFg2$player2
BL19_SFg3 <- BL19_SFg2
BL19_SFg3$p1inp2vec <- is.element(BL19_SFg3$player1, player2vector)
BL19_SFg3$p2inp1vec <- is.element(BL19_SFg3$player2, player1vector)

addPlayer1 <- BL19_SFg3[ which(BL19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_SFg3[ which(BL19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_SFg2 <- rbind(BL19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
BL19_SFft <- ftable(BL19_SFg2$player1, BL19_SFg2$player2)
BL19_SFft2 <- as.matrix(BL19_SFft)
numRows <- nrow(BL19_SFft2)
numCols <- ncol(BL19_SFft2)
BL19_SFft3 <- BL19_SFft2[c(2:numRows) , c(2:numCols)]
BL19_SFTable <- graph.adjacency(BL19_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(BL19_SFTable, vertex.label = V(BL19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
BL19_SF.clusterCoef <- transitivity(BL19_SFTable, type="global") #cluster coefficient
BL19_SF.degreeCent <- centralization.degree(BL19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_SFftn <- as.network.matrix(BL19_SFft)
BL19_SF.netDensity <- network.density(BL19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_SF.entropy <- entropy(BL19_SFft) #entropy

BL19_SF.netMx <- cbind(BL19_SF.netMx, BL19_SF.clusterCoef, BL19_SF.degreeCent$centralization,
                       BL19_SF.netDensity, BL19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "BL"
KIoutcome = "Turnover_F"
BL19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
BL19_TFg2 <- data.frame(BL19_TF)
BL19_TFg2 <- BL19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_TFg2$player1
player2vector <- BL19_TFg2$player2
BL19_TFg3 <- BL19_TFg2
BL19_TFg3$p1inp2vec <- is.element(BL19_TFg3$player1, player2vector)
BL19_TFg3$p2inp1vec <- is.element(BL19_TFg3$player2, player1vector)

addPlayer1 <- BL19_TFg3[ which(BL19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_TFg3[ which(BL19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_TFg2 <- rbind(BL19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
BL19_TFft <- ftable(BL19_TFg2$player1, BL19_TFg2$player2)
BL19_TFft2 <- as.matrix(BL19_TFft)
numRows <- nrow(BL19_TFft2)
numCols <- ncol(BL19_TFft2)
BL19_TFft3 <- BL19_TFft2[c(2:numRows) , c(2:numCols)]
BL19_TFTable <- graph.adjacency(BL19_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(BL19_TFTable, vertex.label = V(BL19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
BL19_TF.clusterCoef <- transitivity(BL19_TFTable, type="global") #cluster coefficient
BL19_TF.degreeCent <- centralization.degree(BL19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_TFftn <- as.network.matrix(BL19_TFft)
BL19_TF.netDensity <- network.density(BL19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_TF.entropy <- entropy(BL19_TFft) #entropy

BL19_TF.netMx <- cbind(BL19_TF.netMx, BL19_TF.clusterCoef, BL19_TF.degreeCent$centralization,
                       BL19_TF.netDensity, BL19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
BL19_SAMg2 <- data.frame(BL19_SAM)
BL19_SAMg2 <- BL19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_SAMg2$player1
player2vector <- BL19_SAMg2$player2
BL19_SAMg3 <- BL19_SAMg2
BL19_SAMg3$p1inp2vec <- is.element(BL19_SAMg3$player1, player2vector)
BL19_SAMg3$p2inp1vec <- is.element(BL19_SAMg3$player2, player1vector)

addPlayer1 <- BL19_SAMg3[ which(BL19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_SAMg3[ which(BL19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_SAMg2 <- rbind(BL19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
BL19_SAMft <- ftable(BL19_SAMg2$player1, BL19_SAMg2$player2)
BL19_SAMft2 <- as.matrix(BL19_SAMft)
numRows <- nrow(BL19_SAMft2)
numCols <- ncol(BL19_SAMft2)
BL19_SAMft3 <- BL19_SAMft2[c(2:numRows) , c(2:numCols)]
BL19_SAMTable <- graph.adjacency(BL19_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(BL19_SAMTable, vertex.label = V(BL19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
BL19_SAM.clusterCoef <- transitivity(BL19_SAMTable, type="global") #cluster coefficient
BL19_SAM.degreeCent <- centralization.degree(BL19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_SAMftn <- as.network.matrix(BL19_SAMft)
BL19_SAM.netDensity <- network.density(BL19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_SAM.entropy <- entropy(BL19_SAMft) #entropy

BL19_SAM.netMx <- cbind(BL19_SAM.netMx, BL19_SAM.clusterCoef, BL19_SAM.degreeCent$centralization,
                        BL19_SAM.netDensity, BL19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "BL"
KIoutcome = "Turnover_AM"
BL19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
BL19_TAMg2 <- data.frame(BL19_TAM)
BL19_TAMg2 <- BL19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_TAMg2$player1
player2vector <- BL19_TAMg2$player2
BL19_TAMg3 <- BL19_TAMg2
BL19_TAMg3$p1inp2vec <- is.element(BL19_TAMg3$player1, player2vector)
BL19_TAMg3$p2inp1vec <- is.element(BL19_TAMg3$player2, player1vector)

addPlayer1 <- BL19_TAMg3[ which(BL19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL19_TAMg3[ which(BL19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_TAMg2 <- rbind(BL19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
BL19_TAMft <- ftable(BL19_TAMg2$player1, BL19_TAMg2$player2)
BL19_TAMft2 <- as.matrix(BL19_TAMft)
numRows <- nrow(BL19_TAMft2)
numCols <- ncol(BL19_TAMft2)
BL19_TAMft3 <- BL19_TAMft2[c(2:numRows) , c(2:numCols)]
BL19_TAMTable <- graph.adjacency(BL19_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(BL19_TAMTable, vertex.label = V(BL19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
BL19_TAM.clusterCoef <- transitivity(BL19_TAMTable, type="global") #cluster coefficient
BL19_TAM.degreeCent <- centralization.degree(BL19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_TAMftn <- as.network.matrix(BL19_TAMft)
BL19_TAM.netDensity <- network.density(BL19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_TAM.entropy <- entropy(BL19_TAMft) #entropy

BL19_TAM.netMx <- cbind(BL19_TAM.netMx, BL19_TAM.clusterCoef, BL19_TAM.degreeCent$centralization,
                        BL19_TAM.netDensity, BL19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************

round = 19
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
BL19_SDMg2 <- data.frame(BL19_SDM)
BL19_SDMg2 <- BL19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_SDMg2$player1
player2vector <- BL19_SDMg2$player2
BL19_SDMg3 <- BL19_SDMg2
BL19_SDMg3$p1inp2vec <- is.element(BL19_SDMg3$player1, player2vector)
BL19_SDMg3$p2inp1vec <- is.element(BL19_SDMg3$player2, player1vector)

addPlayer1 <- BL19_SDMg3[ which(BL19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_SDMg3[ which(BL19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_SDMg2 <- rbind(BL19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
BL19_SDMft <- ftable(BL19_SDMg2$player1, BL19_SDMg2$player2)
BL19_SDMft2 <- as.matrix(BL19_SDMft)
numRows <- nrow(BL19_SDMft2)
numCols <- ncol(BL19_SDMft2)
BL19_SDMft3 <- BL19_SDMft2[c(2:numRows) , c(2:numCols)]
BL19_SDMTable <- graph.adjacency(BL19_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(BL19_SDMTable, vertex.label = V(BL19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
BL19_SDM.clusterCoef <- transitivity(BL19_SDMTable, type="global") #cluster coefficient
BL19_SDM.degreeCent <- centralization.degree(BL19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_SDMftn <- as.network.matrix(BL19_SDMft)
BL19_SDM.netDensity <- network.density(BL19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_SDM.entropy <- entropy(BL19_SDMft) #entropy

BL19_SDM.netMx <- cbind(BL19_SDM.netMx, BL19_SDM.clusterCoef, BL19_SDM.degreeCent$centralization,
                        BL19_SDM.netDensity, BL19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "BL"
KIoutcome = "Turnover_DM"
BL19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
BL19_TDMg2 <- data.frame(BL19_TDM)
BL19_TDMg2 <- BL19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_TDMg2$player1
player2vector <- BL19_TDMg2$player2
BL19_TDMg3 <- BL19_TDMg2
BL19_TDMg3$p1inp2vec <- is.element(BL19_TDMg3$player1, player2vector)
BL19_TDMg3$p2inp1vec <- is.element(BL19_TDMg3$player2, player1vector)

addPlayer1 <- BL19_TDMg3[ which(BL19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_TDMg3[ which(BL19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_TDMg2 <- rbind(BL19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
BL19_TDMft <- ftable(BL19_TDMg2$player1, BL19_TDMg2$player2)
BL19_TDMft2 <- as.matrix(BL19_TDMft)
numRows <- nrow(BL19_TDMft2)
numCols <- ncol(BL19_TDMft2)
BL19_TDMft3 <- BL19_TDMft2[c(2:numRows) , c(2:numCols)]
BL19_TDMTable <- graph.adjacency(BL19_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(BL19_TDMTable, vertex.label = V(BL19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
BL19_TDM.clusterCoef <- transitivity(BL19_TDMTable, type="global") #cluster coefficient
BL19_TDM.degreeCent <- centralization.degree(BL19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_TDMftn <- as.network.matrix(BL19_TDMft)
BL19_TDM.netDensity <- network.density(BL19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_TDM.entropy <- entropy(BL19_TDMft) #entropy

BL19_TDM.netMx <- cbind(BL19_TDM.netMx, BL19_TDM.clusterCoef, BL19_TDM.degreeCent$centralization,
                        BL19_TDM.netDensity, BL19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "BL"
KIoutcome = "Stoppage_D"
BL19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
BL19_SDg2 <- data.frame(BL19_SD)
BL19_SDg2 <- BL19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_SDg2$player1
player2vector <- BL19_SDg2$player2
BL19_SDg3 <- BL19_SDg2
BL19_SDg3$p1inp2vec <- is.element(BL19_SDg3$player1, player2vector)
BL19_SDg3$p2inp1vec <- is.element(BL19_SDg3$player2, player1vector)

addPlayer1 <- BL19_SDg3[ which(BL19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_SDg3[ which(BL19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_SDg2 <- rbind(BL19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
BL19_SDft <- ftable(BL19_SDg2$player1, BL19_SDg2$player2)
BL19_SDft2 <- as.matrix(BL19_SDft)
numRows <- nrow(BL19_SDft2)
numCols <- ncol(BL19_SDft2)
BL19_SDft3 <- BL19_SDft2[c(2:numRows) , c(2:numCols)]
BL19_SDTable <- graph.adjacency(BL19_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(BL19_SDTable, vertex.label = V(BL19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
BL19_SD.clusterCoef <- transitivity(BL19_SDTable, type="global") #cluster coefficient
BL19_SD.degreeCent <- centralization.degree(BL19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_SDftn <- as.network.matrix(BL19_SDft)
BL19_SD.netDensity <- network.density(BL19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_SD.entropy <- entropy(BL19_SDft) #entropy

BL19_SD.netMx <- cbind(BL19_SD.netMx, BL19_SD.clusterCoef, BL19_SD.degreeCent$centralization,
                       BL19_SD.netDensity, BL19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "BL"
KIoutcome = "Turnover_D"
BL19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
BL19_TDg2 <- data.frame(BL19_TD)
BL19_TDg2 <- BL19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_TDg2$player1
player2vector <- BL19_TDg2$player2
BL19_TDg3 <- BL19_TDg2
BL19_TDg3$p1inp2vec <- is.element(BL19_TDg3$player1, player2vector)
BL19_TDg3$p2inp1vec <- is.element(BL19_TDg3$player2, player1vector)

addPlayer1 <- BL19_TDg3[ which(BL19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_TDg3[ which(BL19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_TDg2 <- rbind(BL19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
BL19_TDft <- ftable(BL19_TDg2$player1, BL19_TDg2$player2)
BL19_TDft2 <- as.matrix(BL19_TDft)
numRows <- nrow(BL19_TDft2)
numCols <- ncol(BL19_TDft2)
BL19_TDft3 <- BL19_TDft2[c(2:numRows) , c(2:numCols)]
BL19_TDTable <- graph.adjacency(BL19_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(BL19_TDTable, vertex.label = V(BL19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
BL19_TD.clusterCoef <- transitivity(BL19_TDTable, type="global") #cluster coefficient
BL19_TD.degreeCent <- centralization.degree(BL19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_TDftn <- as.network.matrix(BL19_TDft)
BL19_TD.netDensity <- network.density(BL19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_TD.entropy <- entropy(BL19_TDft) #entropy

BL19_TD.netMx <- cbind(BL19_TD.netMx, BL19_TD.clusterCoef, BL19_TD.degreeCent$centralization,
                       BL19_TD.netDensity, BL19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
BL19_QTg2 <- data.frame(BL19_QT)
BL19_QTg2 <- BL19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL19_QTg2$player1
player2vector <- BL19_QTg2$player2
BL19_QTg3 <- BL19_QTg2
BL19_QTg3$p1inp2vec <- is.element(BL19_QTg3$player1, player2vector)
BL19_QTg3$p2inp1vec <- is.element(BL19_QTg3$player2, player1vector)

addPlayer1 <- BL19_QTg3[ which(BL19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL19_QTg3[ which(BL19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL19_QTg2 <- rbind(BL19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
BL19_QTft <- ftable(BL19_QTg2$player1, BL19_QTg2$player2)
BL19_QTft2 <- as.matrix(BL19_QTft)
numRows <- nrow(BL19_QTft2)
numCols <- ncol(BL19_QTft2)
BL19_QTft3 <- BL19_QTft2[c(2:numRows) , c(2:numCols)]
BL19_QTTable <- graph.adjacency(BL19_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(BL19_QTTable, vertex.label = V(BL19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
BL19_QT.clusterCoef <- transitivity(BL19_QTTable, type="global") #cluster coefficient
BL19_QT.degreeCent <- centralization.degree(BL19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL19_QTftn <- as.network.matrix(BL19_QTft)
BL19_QT.netDensity <- network.density(BL19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL19_QT.entropy <- entropy(BL19_QTft) #entropy

BL19_QT.netMx <- cbind(BL19_QT.netMx, BL19_QT.clusterCoef, BL19_QT.degreeCent$centralization,
                       BL19_QT.netDensity, BL19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL19_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************

round = 19
teamName = "CARL"
KIoutcome = "Goal_F"
CARL19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
CARL19_Gg2 <- data.frame(CARL19_G)
CARL19_Gg2 <- CARL19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_Gg2$player1
player2vector <- CARL19_Gg2$player2
CARL19_Gg3 <- CARL19_Gg2
CARL19_Gg3$p1inp2vec <- is.element(CARL19_Gg3$player1, player2vector)
CARL19_Gg3$p2inp1vec <- is.element(CARL19_Gg3$player2, player1vector)

addPlayer1 <- CARL19_Gg3[ which(CARL19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_Gg3[ which(CARL19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_Gg2 <- rbind(CARL19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
CARL19_Gft <- ftable(CARL19_Gg2$player1, CARL19_Gg2$player2)
CARL19_Gft2 <- as.matrix(CARL19_Gft)
numRows <- nrow(CARL19_Gft2)
numCols <- ncol(CARL19_Gft2)
CARL19_Gft3 <- CARL19_Gft2[c(2:numRows) , c(2:numCols)]
CARL19_GTable <- graph.adjacency(CARL19_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(CARL19_GTable, vertex.label = V(CARL19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
CARL19_G.clusterCoef <- transitivity(CARL19_GTable, type="global") #cluster coefficient
CARL19_G.degreeCent <- centralization.degree(CARL19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_Gftn <- as.network.matrix(CARL19_Gft)
CARL19_G.netDensity <- network.density(CARL19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_G.entropy <- entropy(CARL19_Gft) #entropy

CARL19_G.netMx <- cbind(CARL19_G.netMx, CARL19_G.clusterCoef, CARL19_G.degreeCent$centralization,
                        CARL19_G.netDensity, CARL19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "CARL"
KIoutcome = "Behind_F"
CARL19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
CARL19_Bg2 <- data.frame(CARL19_B)
CARL19_Bg2 <- CARL19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_Bg2$player1
player2vector <- CARL19_Bg2$player2
CARL19_Bg3 <- CARL19_Bg2
CARL19_Bg3$p1inp2vec <- is.element(CARL19_Bg3$player1, player2vector)
CARL19_Bg3$p2inp1vec <- is.element(CARL19_Bg3$player2, player1vector)

addPlayer1 <- CARL19_Bg3[ which(CARL19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_Bg3[ which(CARL19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_Bg2 <- rbind(CARL19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
CARL19_Bft <- ftable(CARL19_Bg2$player1, CARL19_Bg2$player2)
CARL19_Bft2 <- as.matrix(CARL19_Bft)
numRows <- nrow(CARL19_Bft2)
numCols <- ncol(CARL19_Bft2)
CARL19_Bft3 <- CARL19_Bft2[c(2:numRows) , c(2:numCols)]
CARL19_BTable <- graph.adjacency(CARL19_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(CARL19_BTable, vertex.label = V(CARL19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
CARL19_B.clusterCoef <- transitivity(CARL19_BTable, type="global") #cluster coefficient
CARL19_B.degreeCent <- centralization.degree(CARL19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_Bftn <- as.network.matrix(CARL19_Bft)
CARL19_B.netDensity <- network.density(CARL19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_B.entropy <- entropy(CARL19_Bft) #entropy

CARL19_B.netMx <- cbind(CARL19_B.netMx, CARL19_B.clusterCoef, CARL19_B.degreeCent$centralization,
                        CARL19_B.netDensity, CARL19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
CARL19_SFg2 <- data.frame(CARL19_SF)
CARL19_SFg2 <- CARL19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_SFg2$player1
player2vector <- CARL19_SFg2$player2
CARL19_SFg3 <- CARL19_SFg2
CARL19_SFg3$p1inp2vec <- is.element(CARL19_SFg3$player1, player2vector)
CARL19_SFg3$p2inp1vec <- is.element(CARL19_SFg3$player2, player1vector)

addPlayer1 <- CARL19_SFg3[ which(CARL19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_SFg3[ which(CARL19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_SFg2 <- rbind(CARL19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
CARL19_SFft <- ftable(CARL19_SFg2$player1, CARL19_SFg2$player2)
CARL19_SFft2 <- as.matrix(CARL19_SFft)
numRows <- nrow(CARL19_SFft2)
numCols <- ncol(CARL19_SFft2)
CARL19_SFft3 <- CARL19_SFft2[c(2:numRows) , c(2:numCols)]
CARL19_SFTable <- graph.adjacency(CARL19_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(CARL19_SFTable, vertex.label = V(CARL19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
CARL19_SF.clusterCoef <- transitivity(CARL19_SFTable, type="global") #cluster coefficient
CARL19_SF.degreeCent <- centralization.degree(CARL19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_SFftn <- as.network.matrix(CARL19_SFft)
CARL19_SF.netDensity <- network.density(CARL19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_SF.entropy <- entropy(CARL19_SFft) #entropy

CARL19_SF.netMx <- cbind(CARL19_SF.netMx, CARL19_SF.clusterCoef, CARL19_SF.degreeCent$centralization,
                         CARL19_SF.netDensity, CARL19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
CARL19_TFg2 <- data.frame(CARL19_TF)
CARL19_TFg2 <- CARL19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_TFg2$player1
player2vector <- CARL19_TFg2$player2
CARL19_TFg3 <- CARL19_TFg2
CARL19_TFg3$p1inp2vec <- is.element(CARL19_TFg3$player1, player2vector)
CARL19_TFg3$p2inp1vec <- is.element(CARL19_TFg3$player2, player1vector)

addPlayer1 <- CARL19_TFg3[ which(CARL19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL19_TFg3[ which(CARL19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_TFg2 <- rbind(CARL19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
CARL19_TFft <- ftable(CARL19_TFg2$player1, CARL19_TFg2$player2)
CARL19_TFft2 <- as.matrix(CARL19_TFft)
numRows <- nrow(CARL19_TFft2)
numCols <- ncol(CARL19_TFft2)
CARL19_TFft3 <- CARL19_TFft2[c(2:numRows) , c(2:numCols)]
CARL19_TFTable <- graph.adjacency(CARL19_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(CARL19_TFTable, vertex.label = V(CARL19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
CARL19_TF.clusterCoef <- transitivity(CARL19_TFTable, type="global") #cluster coefficient
CARL19_TF.degreeCent <- centralization.degree(CARL19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_TFftn <- as.network.matrix(CARL19_TFft)
CARL19_TF.netDensity <- network.density(CARL19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_TF.entropy <- entropy(CARL19_TFft) #entropy

CARL19_TF.netMx <- cbind(CARL19_TF.netMx, CARL19_TF.clusterCoef, CARL19_TF.degreeCent$centralization,
                         CARL19_TF.netDensity, CARL19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************

round = 19
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
CARL19_SAMg2 <- data.frame(CARL19_SAM)
CARL19_SAMg2 <- CARL19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_SAMg2$player1
player2vector <- CARL19_SAMg2$player2
CARL19_SAMg3 <- CARL19_SAMg2
CARL19_SAMg3$p1inp2vec <- is.element(CARL19_SAMg3$player1, player2vector)
CARL19_SAMg3$p2inp1vec <- is.element(CARL19_SAMg3$player2, player1vector)

addPlayer1 <- CARL19_SAMg3[ which(CARL19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_SAMg3[ which(CARL19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_SAMg2 <- rbind(CARL19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
CARL19_SAMft <- ftable(CARL19_SAMg2$player1, CARL19_SAMg2$player2)
CARL19_SAMft2 <- as.matrix(CARL19_SAMft)
numRows <- nrow(CARL19_SAMft2)
numCols <- ncol(CARL19_SAMft2)
CARL19_SAMft3 <- CARL19_SAMft2[c(2:numRows) , c(2:numCols)]
CARL19_SAMTable <- graph.adjacency(CARL19_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(CARL19_SAMTable, vertex.label = V(CARL19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
CARL19_SAM.clusterCoef <- transitivity(CARL19_SAMTable, type="global") #cluster coefficient
CARL19_SAM.degreeCent <- centralization.degree(CARL19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_SAMftn <- as.network.matrix(CARL19_SAMft)
CARL19_SAM.netDensity <- network.density(CARL19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_SAM.entropy <- entropy(CARL19_SAMft) #entropy

CARL19_SAM.netMx <- cbind(CARL19_SAM.netMx, CARL19_SAM.clusterCoef, CARL19_SAM.degreeCent$centralization,
                          CARL19_SAM.netDensity, CARL19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
CARL19_TAMg2 <- data.frame(CARL19_TAM)
CARL19_TAMg2 <- CARL19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_TAMg2$player1
player2vector <- CARL19_TAMg2$player2
CARL19_TAMg3 <- CARL19_TAMg2
CARL19_TAMg3$p1inp2vec <- is.element(CARL19_TAMg3$player1, player2vector)
CARL19_TAMg3$p2inp1vec <- is.element(CARL19_TAMg3$player2, player1vector)

addPlayer1 <- CARL19_TAMg3[ which(CARL19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_TAMg3[ which(CARL19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_TAMg2 <- rbind(CARL19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
CARL19_TAMft <- ftable(CARL19_TAMg2$player1, CARL19_TAMg2$player2)
CARL19_TAMft2 <- as.matrix(CARL19_TAMft)
numRows <- nrow(CARL19_TAMft2)
numCols <- ncol(CARL19_TAMft2)
CARL19_TAMft3 <- CARL19_TAMft2[c(2:numRows) , c(2:numCols)]
CARL19_TAMTable <- graph.adjacency(CARL19_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(CARL19_TAMTable, vertex.label = V(CARL19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
CARL19_TAM.clusterCoef <- transitivity(CARL19_TAMTable, type="global") #cluster coefficient
CARL19_TAM.degreeCent <- centralization.degree(CARL19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_TAMftn <- as.network.matrix(CARL19_TAMft)
CARL19_TAM.netDensity <- network.density(CARL19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_TAM.entropy <- entropy(CARL19_TAMft) #entropy

CARL19_TAM.netMx <- cbind(CARL19_TAM.netMx, CARL19_TAM.clusterCoef, CARL19_TAM.degreeCent$centralization,
                          CARL19_TAM.netDensity, CARL19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************

round = 19
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
CARL19_SDMg2 <- data.frame(CARL19_SDM)
CARL19_SDMg2 <- CARL19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_SDMg2$player1
player2vector <- CARL19_SDMg2$player2
CARL19_SDMg3 <- CARL19_SDMg2
CARL19_SDMg3$p1inp2vec <- is.element(CARL19_SDMg3$player1, player2vector)
CARL19_SDMg3$p2inp1vec <- is.element(CARL19_SDMg3$player2, player1vector)

addPlayer1 <- CARL19_SDMg3[ which(CARL19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_SDMg3[ which(CARL19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_SDMg2 <- rbind(CARL19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
CARL19_SDMft <- ftable(CARL19_SDMg2$player1, CARL19_SDMg2$player2)
CARL19_SDMft2 <- as.matrix(CARL19_SDMft)
numRows <- nrow(CARL19_SDMft2)
numCols <- ncol(CARL19_SDMft2)
CARL19_SDMft3 <- CARL19_SDMft2[c(2:numRows) , c(2:numCols)]
CARL19_SDMTable <- graph.adjacency(CARL19_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(CARL19_SDMTable, vertex.label = V(CARL19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
CARL19_SDM.clusterCoef <- transitivity(CARL19_SDMTable, type="global") #cluster coefficient
CARL19_SDM.degreeCent <- centralization.degree(CARL19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_SDMftn <- as.network.matrix(CARL19_SDMft)
CARL19_SDM.netDensity <- network.density(CARL19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_SDM.entropy <- entropy(CARL19_SDMft) #entropy

CARL19_SDM.netMx <- cbind(CARL19_SDM.netMx, CARL19_SDM.clusterCoef, CARL19_SDM.degreeCent$centralization,
                          CARL19_SDM.netDensity, CARL19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************
#NA

round = 19
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
CARL19_TDMg2 <- data.frame(CARL19_TDM)
CARL19_TDMg2 <- CARL19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_TDMg2$player1
player2vector <- CARL19_TDMg2$player2
CARL19_TDMg3 <- CARL19_TDMg2
CARL19_TDMg3$p1inp2vec <- is.element(CARL19_TDMg3$player1, player2vector)
CARL19_TDMg3$p2inp1vec <- is.element(CARL19_TDMg3$player2, player1vector)

addPlayer1 <- CARL19_TDMg3[ which(CARL19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_TDMg3[ which(CARL19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_TDMg2 <- rbind(CARL19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
CARL19_TDMft <- ftable(CARL19_TDMg2$player1, CARL19_TDMg2$player2)
CARL19_TDMft2 <- as.matrix(CARL19_TDMft)
numRows <- nrow(CARL19_TDMft2)
numCols <- ncol(CARL19_TDMft2)
CARL19_TDMft3 <- CARL19_TDMft2[c(2:numRows) , c(2:numCols)]
CARL19_TDMTable <- graph.adjacency(CARL19_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(CARL19_TDMTable, vertex.label = V(CARL19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
CARL19_TDM.clusterCoef <- transitivity(CARL19_TDMTable, type="global") #cluster coefficient
CARL19_TDM.degreeCent <- centralization.degree(CARL19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_TDMftn <- as.network.matrix(CARL19_TDMft)
CARL19_TDM.netDensity <- network.density(CARL19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_TDM.entropy <- entropy(CARL19_TDMft) #entropy

CARL19_TDM.netMx <- cbind(CARL19_TDM.netMx, CARL19_TDM.clusterCoef, CARL19_TDM.degreeCent$centralization,
                          CARL19_TDM.netDensity, CARL19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
CARL19_SDg2 <- data.frame(CARL19_SD)
CARL19_SDg2 <- CARL19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_SDg2$player1
player2vector <- CARL19_SDg2$player2
CARL19_SDg3 <- CARL19_SDg2
CARL19_SDg3$p1inp2vec <- is.element(CARL19_SDg3$player1, player2vector)
CARL19_SDg3$p2inp1vec <- is.element(CARL19_SDg3$player2, player1vector)

addPlayer1 <- CARL19_SDg3[ which(CARL19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_SDg3[ which(CARL19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_SDg2 <- rbind(CARL19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
CARL19_SDft <- ftable(CARL19_SDg2$player1, CARL19_SDg2$player2)
CARL19_SDft2 <- as.matrix(CARL19_SDft)
numRows <- nrow(CARL19_SDft2)
numCols <- ncol(CARL19_SDft2)
CARL19_SDft3 <- CARL19_SDft2[c(2:numRows) , c(2:numCols)]
CARL19_SDTable <- graph.adjacency(CARL19_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(CARL19_SDTable, vertex.label = V(CARL19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
CARL19_SD.clusterCoef <- transitivity(CARL19_SDTable, type="global") #cluster coefficient
CARL19_SD.degreeCent <- centralization.degree(CARL19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_SDftn <- as.network.matrix(CARL19_SDft)
CARL19_SD.netDensity <- network.density(CARL19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_SD.entropy <- entropy(CARL19_SDft) #entropy

CARL19_SD.netMx <- cbind(CARL19_SD.netMx, CARL19_SD.clusterCoef, CARL19_SD.degreeCent$centralization,
                         CARL19_SD.netDensity, CARL19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
CARL19_TDg2 <- data.frame(CARL19_TD)
CARL19_TDg2 <- CARL19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_TDg2$player1
player2vector <- CARL19_TDg2$player2
CARL19_TDg3 <- CARL19_TDg2
CARL19_TDg3$p1inp2vec <- is.element(CARL19_TDg3$player1, player2vector)
CARL19_TDg3$p2inp1vec <- is.element(CARL19_TDg3$player2, player1vector)

addPlayer1 <- CARL19_TDg3[ which(CARL19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_TDg3[ which(CARL19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_TDg2 <- rbind(CARL19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
CARL19_TDft <- ftable(CARL19_TDg2$player1, CARL19_TDg2$player2)
CARL19_TDft2 <- as.matrix(CARL19_TDft)
numRows <- nrow(CARL19_TDft2)
numCols <- ncol(CARL19_TDft2)
CARL19_TDft3 <- CARL19_TDft2[c(2:numRows) , c(2:numCols)]
CARL19_TDTable <- graph.adjacency(CARL19_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(CARL19_TDTable, vertex.label = V(CARL19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
CARL19_TD.clusterCoef <- transitivity(CARL19_TDTable, type="global") #cluster coefficient
CARL19_TD.degreeCent <- centralization.degree(CARL19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_TDftn <- as.network.matrix(CARL19_TDft)
CARL19_TD.netDensity <- network.density(CARL19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_TD.entropy <- entropy(CARL19_TDft) #entropy

CARL19_TD.netMx <- cbind(CARL19_TD.netMx, CARL19_TD.clusterCoef, CARL19_TD.degreeCent$centralization,
                         CARL19_TD.netDensity, CARL19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
CARL19_QTg2 <- data.frame(CARL19_QT)
CARL19_QTg2 <- CARL19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL19_QTg2$player1
player2vector <- CARL19_QTg2$player2
CARL19_QTg3 <- CARL19_QTg2
CARL19_QTg3$p1inp2vec <- is.element(CARL19_QTg3$player1, player2vector)
CARL19_QTg3$p2inp1vec <- is.element(CARL19_QTg3$player2, player1vector)

addPlayer1 <- CARL19_QTg3[ which(CARL19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL19_QTg3[ which(CARL19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL19_QTg2 <- rbind(CARL19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
CARL19_QTft <- ftable(CARL19_QTg2$player1, CARL19_QTg2$player2)
CARL19_QTft2 <- as.matrix(CARL19_QTft)
numRows <- nrow(CARL19_QTft2)
numCols <- ncol(CARL19_QTft2)
CARL19_QTft3 <- CARL19_QTft2[c(2:numRows) , c(2:numCols)]
CARL19_QTTable <- graph.adjacency(CARL19_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(CARL19_QTTable, vertex.label = V(CARL19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
CARL19_QT.clusterCoef <- transitivity(CARL19_QTTable, type="global") #cluster coefficient
CARL19_QT.degreeCent <- centralization.degree(CARL19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL19_QTftn <- as.network.matrix(CARL19_QTft)
CARL19_QT.netDensity <- network.density(CARL19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL19_QT.entropy <- entropy(CARL19_QTft) #entropy

CARL19_QT.netMx <- cbind(CARL19_QT.netMx, CARL19_QT.clusterCoef, CARL19_QT.degreeCent$centralization,
                         CARL19_QT.netDensity, CARL19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL19_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "COLL"
KIoutcome = "Goal_F"
COLL19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
COLL19_Gg2 <- data.frame(COLL19_G)
COLL19_Gg2 <- COLL19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_Gg2$player1
player2vector <- COLL19_Gg2$player2
COLL19_Gg3 <- COLL19_Gg2
COLL19_Gg3$p1inp2vec <- is.element(COLL19_Gg3$player1, player2vector)
COLL19_Gg3$p2inp1vec <- is.element(COLL19_Gg3$player2, player1vector)

addPlayer1 <- COLL19_Gg3[ which(COLL19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_Gg3[ which(COLL19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_Gg2 <- rbind(COLL19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
COLL19_Gft <- ftable(COLL19_Gg2$player1, COLL19_Gg2$player2)
COLL19_Gft2 <- as.matrix(COLL19_Gft)
numRows <- nrow(COLL19_Gft2)
numCols <- ncol(COLL19_Gft2)
COLL19_Gft3 <- COLL19_Gft2[c(2:numRows) , c(2:numCols)]
COLL19_GTable <- graph.adjacency(COLL19_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(COLL19_GTable, vertex.label = V(COLL19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
COLL19_G.clusterCoef <- transitivity(COLL19_GTable, type="global") #cluster coefficient
COLL19_G.degreeCent <- centralization.degree(COLL19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_Gftn <- as.network.matrix(COLL19_Gft)
COLL19_G.netDensity <- network.density(COLL19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_G.entropy <- entropy(COLL19_Gft) #entropy

COLL19_G.netMx <- cbind(COLL19_G.netMx, COLL19_G.clusterCoef, COLL19_G.degreeCent$centralization,
                        COLL19_G.netDensity, COLL19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "COLL"
KIoutcome = "Behind_F"
COLL19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
COLL19_Bg2 <- data.frame(COLL19_B)
COLL19_Bg2 <- COLL19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_Bg2$player1
player2vector <- COLL19_Bg2$player2
COLL19_Bg3 <- COLL19_Bg2
COLL19_Bg3$p1inp2vec <- is.element(COLL19_Bg3$player1, player2vector)
COLL19_Bg3$p2inp1vec <- is.element(COLL19_Bg3$player2, player1vector)

addPlayer1 <- COLL19_Bg3[ which(COLL19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_Bg3[ which(COLL19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_Bg2 <- rbind(COLL19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
COLL19_Bft <- ftable(COLL19_Bg2$player1, COLL19_Bg2$player2)
COLL19_Bft2 <- as.matrix(COLL19_Bft)
numRows <- nrow(COLL19_Bft2)
numCols <- ncol(COLL19_Bft2)
COLL19_Bft3 <- COLL19_Bft2[c(2:numRows) , c(2:numCols)]
COLL19_BTable <- graph.adjacency(COLL19_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(COLL19_BTable, vertex.label = V(COLL19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
COLL19_B.clusterCoef <- transitivity(COLL19_BTable, type="global") #cluster coefficient
COLL19_B.degreeCent <- centralization.degree(COLL19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_Bftn <- as.network.matrix(COLL19_Bft)
COLL19_B.netDensity <- network.density(COLL19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_B.entropy <- entropy(COLL19_Bft) #entropy

COLL19_B.netMx <- cbind(COLL19_B.netMx, COLL19_B.clusterCoef, COLL19_B.degreeCent$centralization,
                        COLL19_B.netDensity, COLL19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************

round = 19
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
COLL19_SFg2 <- data.frame(COLL19_SF)
COLL19_SFg2 <- COLL19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_SFg2$player1
player2vector <- COLL19_SFg2$player2
COLL19_SFg3 <- COLL19_SFg2
COLL19_SFg3$p1inp2vec <- is.element(COLL19_SFg3$player1, player2vector)
COLL19_SFg3$p2inp1vec <- is.element(COLL19_SFg3$player2, player1vector)

addPlayer1 <- COLL19_SFg3[ which(COLL19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_SFg3[ which(COLL19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_SFg2 <- rbind(COLL19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
COLL19_SFft <- ftable(COLL19_SFg2$player1, COLL19_SFg2$player2)
COLL19_SFft2 <- as.matrix(COLL19_SFft)
numRows <- nrow(COLL19_SFft2)
numCols <- ncol(COLL19_SFft2)
COLL19_SFft3 <- COLL19_SFft2[c(2:numRows) , c(2:numCols)]
COLL19_SFTable <- graph.adjacency(COLL19_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(COLL19_SFTable, vertex.label = V(COLL19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
COLL19_SF.clusterCoef <- transitivity(COLL19_SFTable, type="global") #cluster coefficient
COLL19_SF.degreeCent <- centralization.degree(COLL19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_SFftn <- as.network.matrix(COLL19_SFft)
COLL19_SF.netDensity <- network.density(COLL19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_SF.entropy <- entropy(COLL19_SFft) #entropy

COLL19_SF.netMx <- cbind(COLL19_SF.netMx, COLL19_SF.clusterCoef, COLL19_SF.degreeCent$centralization,
                         COLL19_SF.netDensity, COLL19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************
#NA

round = 19
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
COLL19_TFg2 <- data.frame(COLL19_TF)
COLL19_TFg2 <- COLL19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_TFg2$player1
player2vector <- COLL19_TFg2$player2
COLL19_TFg3 <- COLL19_TFg2
COLL19_TFg3$p1inp2vec <- is.element(COLL19_TFg3$player1, player2vector)
COLL19_TFg3$p2inp1vec <- is.element(COLL19_TFg3$player2, player1vector)

addPlayer1 <- COLL19_TFg3[ which(COLL19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_TFg3[ which(COLL19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_TFg2 <- rbind(COLL19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
COLL19_TFft <- ftable(COLL19_TFg2$player1, COLL19_TFg2$player2)
COLL19_TFft2 <- as.matrix(COLL19_TFft)
numRows <- nrow(COLL19_TFft2)
numCols <- ncol(COLL19_TFft2)
COLL19_TFft3 <- COLL19_TFft2[c(2:numRows) , c(2:numCols)]
COLL19_TFTable <- graph.adjacency(COLL19_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(COLL19_TFTable, vertex.label = V(COLL19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
COLL19_TF.clusterCoef <- transitivity(COLL19_TFTable, type="global") #cluster coefficient
COLL19_TF.degreeCent <- centralization.degree(COLL19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_TFftn <- as.network.matrix(COLL19_TFft)
COLL19_TF.netDensity <- network.density(COLL19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_TF.entropy <- entropy(COLL19_TFft) #entropy

COLL19_TF.netMx <- cbind(COLL19_TF.netMx, COLL19_TF.clusterCoef, COLL19_TF.degreeCent$centralization,
                         COLL19_TF.netDensity, COLL19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************

round = 19
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
COLL19_SAMg2 <- data.frame(COLL19_SAM)
COLL19_SAMg2 <- COLL19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_SAMg2$player1
player2vector <- COLL19_SAMg2$player2
COLL19_SAMg3 <- COLL19_SAMg2
COLL19_SAMg3$p1inp2vec <- is.element(COLL19_SAMg3$player1, player2vector)
COLL19_SAMg3$p2inp1vec <- is.element(COLL19_SAMg3$player2, player1vector)

addPlayer1 <- COLL19_SAMg3[ which(COLL19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_SAMg3[ which(COLL19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_SAMg2 <- rbind(COLL19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
COLL19_SAMft <- ftable(COLL19_SAMg2$player1, COLL19_SAMg2$player2)
COLL19_SAMft2 <- as.matrix(COLL19_SAMft)
numRows <- nrow(COLL19_SAMft2)
numCols <- ncol(COLL19_SAMft2)
COLL19_SAMft3 <- COLL19_SAMft2[c(2:numRows) , c(2:numCols)]
COLL19_SAMTable <- graph.adjacency(COLL19_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(COLL19_SAMTable, vertex.label = V(COLL19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
COLL19_SAM.clusterCoef <- transitivity(COLL19_SAMTable, type="global") #cluster coefficient
COLL19_SAM.degreeCent <- centralization.degree(COLL19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_SAMftn <- as.network.matrix(COLL19_SAMft)
COLL19_SAM.netDensity <- network.density(COLL19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_SAM.entropy <- entropy(COLL19_SAMft) #entropy

COLL19_SAM.netMx <- cbind(COLL19_SAM.netMx, COLL19_SAM.clusterCoef, COLL19_SAM.degreeCent$centralization,
                          COLL19_SAM.netDensity, COLL19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
COLL19_TAMg2 <- data.frame(COLL19_TAM)
COLL19_TAMg2 <- COLL19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_TAMg2$player1
player2vector <- COLL19_TAMg2$player2
COLL19_TAMg3 <- COLL19_TAMg2
COLL19_TAMg3$p1inp2vec <- is.element(COLL19_TAMg3$player1, player2vector)
COLL19_TAMg3$p2inp1vec <- is.element(COLL19_TAMg3$player2, player1vector)

addPlayer1 <- COLL19_TAMg3[ which(COLL19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_TAMg3[ which(COLL19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_TAMg2 <- rbind(COLL19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
COLL19_TAMft <- ftable(COLL19_TAMg2$player1, COLL19_TAMg2$player2)
COLL19_TAMft2 <- as.matrix(COLL19_TAMft)
numRows <- nrow(COLL19_TAMft2)
numCols <- ncol(COLL19_TAMft2)
COLL19_TAMft3 <- COLL19_TAMft2[c(2:numRows) , c(2:numCols)]
COLL19_TAMTable <- graph.adjacency(COLL19_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(COLL19_TAMTable, vertex.label = V(COLL19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
COLL19_TAM.clusterCoef <- transitivity(COLL19_TAMTable, type="global") #cluster coefficient
COLL19_TAM.degreeCent <- centralization.degree(COLL19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_TAMftn <- as.network.matrix(COLL19_TAMft)
COLL19_TAM.netDensity <- network.density(COLL19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_TAM.entropy <- entropy(COLL19_TAMft) #entropy

COLL19_TAM.netMx <- cbind(COLL19_TAM.netMx, COLL19_TAM.clusterCoef, COLL19_TAM.degreeCent$centralization,
                          COLL19_TAM.netDensity, COLL19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************
#NA

round = 19
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
COLL19_SDMg2 <- data.frame(COLL19_SDM)
COLL19_SDMg2 <- COLL19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_SDMg2$player1
player2vector <- COLL19_SDMg2$player2
COLL19_SDMg3 <- COLL19_SDMg2
COLL19_SDMg3$p1inp2vec <- is.element(COLL19_SDMg3$player1, player2vector)
COLL19_SDMg3$p2inp1vec <- is.element(COLL19_SDMg3$player2, player1vector)

addPlayer1 <- COLL19_SDMg3[ which(COLL19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_SDMg3[ which(COLL19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_SDMg2 <- rbind(COLL19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
COLL19_SDMft <- ftable(COLL19_SDMg2$player1, COLL19_SDMg2$player2)
COLL19_SDMft2 <- as.matrix(COLL19_SDMft)
numRows <- nrow(COLL19_SDMft2)
numCols <- ncol(COLL19_SDMft2)
COLL19_SDMft3 <- COLL19_SDMft2[c(2:numRows) , c(2:numCols)]
COLL19_SDMTable <- graph.adjacency(COLL19_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(COLL19_SDMTable, vertex.label = V(COLL19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
COLL19_SDM.clusterCoef <- transitivity(COLL19_SDMTable, type="global") #cluster coefficient
COLL19_SDM.degreeCent <- centralization.degree(COLL19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_SDMftn <- as.network.matrix(COLL19_SDMft)
COLL19_SDM.netDensity <- network.density(COLL19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_SDM.entropy <- entropy(COLL19_SDMft) #entropy

COLL19_SDM.netMx <- cbind(COLL19_SDM.netMx, COLL19_SDM.clusterCoef, COLL19_SDM.degreeCent$centralization,
                          COLL19_SDM.netDensity, COLL19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
COLL19_TDMg2 <- data.frame(COLL19_TDM)
COLL19_TDMg2 <- COLL19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_TDMg2$player1
player2vector <- COLL19_TDMg2$player2
COLL19_TDMg3 <- COLL19_TDMg2
COLL19_TDMg3$p1inp2vec <- is.element(COLL19_TDMg3$player1, player2vector)
COLL19_TDMg3$p2inp1vec <- is.element(COLL19_TDMg3$player2, player1vector)

addPlayer1 <- COLL19_TDMg3[ which(COLL19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_TDMg3[ which(COLL19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_TDMg2 <- rbind(COLL19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
COLL19_TDMft <- ftable(COLL19_TDMg2$player1, COLL19_TDMg2$player2)
COLL19_TDMft2 <- as.matrix(COLL19_TDMft)
numRows <- nrow(COLL19_TDMft2)
numCols <- ncol(COLL19_TDMft2)
COLL19_TDMft3 <- COLL19_TDMft2[c(2:numRows) , c(2:numCols)]
COLL19_TDMTable <- graph.adjacency(COLL19_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(COLL19_TDMTable, vertex.label = V(COLL19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
COLL19_TDM.clusterCoef <- transitivity(COLL19_TDMTable, type="global") #cluster coefficient
COLL19_TDM.degreeCent <- centralization.degree(COLL19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_TDMftn <- as.network.matrix(COLL19_TDMft)
COLL19_TDM.netDensity <- network.density(COLL19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_TDM.entropy <- entropy(COLL19_TDMft) #entropy

COLL19_TDM.netMx <- cbind(COLL19_TDM.netMx, COLL19_TDM.clusterCoef, COLL19_TDM.degreeCent$centralization,
                          COLL19_TDM.netDensity, COLL19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
COLL19_SDg2 <- data.frame(COLL19_SD)
COLL19_SDg2 <- COLL19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_SDg2$player1
player2vector <- COLL19_SDg2$player2
COLL19_SDg3 <- COLL19_SDg2
COLL19_SDg3$p1inp2vec <- is.element(COLL19_SDg3$player1, player2vector)
COLL19_SDg3$p2inp1vec <- is.element(COLL19_SDg3$player2, player1vector)

addPlayer1 <- COLL19_SDg3[ which(COLL19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_SDg3[ which(COLL19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_SDg2 <- rbind(COLL19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
COLL19_SDft <- ftable(COLL19_SDg2$player1, COLL19_SDg2$player2)
COLL19_SDft2 <- as.matrix(COLL19_SDft)
numRows <- nrow(COLL19_SDft2)
numCols <- ncol(COLL19_SDft2)
COLL19_SDft3 <- COLL19_SDft2[c(2:numRows) , c(2:numCols)]
COLL19_SDTable <- graph.adjacency(COLL19_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(COLL19_SDTable, vertex.label = V(COLL19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
COLL19_SD.clusterCoef <- transitivity(COLL19_SDTable, type="global") #cluster coefficient
COLL19_SD.degreeCent <- centralization.degree(COLL19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_SDftn <- as.network.matrix(COLL19_SDft)
COLL19_SD.netDensity <- network.density(COLL19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_SD.entropy <- entropy(COLL19_SDft) #entropy

COLL19_SD.netMx <- cbind(COLL19_SD.netMx, COLL19_SD.clusterCoef, COLL19_SD.degreeCent$centralization,
                         COLL19_SD.netDensity, COLL19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
COLL19_TDg2 <- data.frame(COLL19_TD)
COLL19_TDg2 <- COLL19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_TDg2$player1
player2vector <- COLL19_TDg2$player2
COLL19_TDg3 <- COLL19_TDg2
COLL19_TDg3$p1inp2vec <- is.element(COLL19_TDg3$player1, player2vector)
COLL19_TDg3$p2inp1vec <- is.element(COLL19_TDg3$player2, player1vector)

addPlayer1 <- COLL19_TDg3[ which(COLL19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_TDg3[ which(COLL19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_TDg2 <- rbind(COLL19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
COLL19_TDft <- ftable(COLL19_TDg2$player1, COLL19_TDg2$player2)
COLL19_TDft2 <- as.matrix(COLL19_TDft)
numRows <- nrow(COLL19_TDft2)
numCols <- ncol(COLL19_TDft2)
COLL19_TDft3 <- COLL19_TDft2[c(2:numRows) , c(2:numCols)]
COLL19_TDTable <- graph.adjacency(COLL19_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(COLL19_TDTable, vertex.label = V(COLL19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
COLL19_TD.clusterCoef <- transitivity(COLL19_TDTable, type="global") #cluster coefficient
COLL19_TD.degreeCent <- centralization.degree(COLL19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_TDftn <- as.network.matrix(COLL19_TDft)
COLL19_TD.netDensity <- network.density(COLL19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_TD.entropy <- entropy(COLL19_TDft) #entropy

COLL19_TD.netMx <- cbind(COLL19_TD.netMx, COLL19_TD.clusterCoef, COLL19_TD.degreeCent$centralization,
                         COLL19_TD.netDensity, COLL19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
COLL19_QTg2 <- data.frame(COLL19_QT)
COLL19_QTg2 <- COLL19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL19_QTg2$player1
player2vector <- COLL19_QTg2$player2
COLL19_QTg3 <- COLL19_QTg2
COLL19_QTg3$p1inp2vec <- is.element(COLL19_QTg3$player1, player2vector)
COLL19_QTg3$p2inp1vec <- is.element(COLL19_QTg3$player2, player1vector)

addPlayer1 <- COLL19_QTg3[ which(COLL19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL19_QTg3[ which(COLL19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL19_QTg2 <- rbind(COLL19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
COLL19_QTft <- ftable(COLL19_QTg2$player1, COLL19_QTg2$player2)
COLL19_QTft2 <- as.matrix(COLL19_QTft)
numRows <- nrow(COLL19_QTft2)
numCols <- ncol(COLL19_QTft2)
COLL19_QTft3 <- COLL19_QTft2[c(2:numRows) , c(2:numCols)]
COLL19_QTTable <- graph.adjacency(COLL19_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(COLL19_QTTable, vertex.label = V(COLL19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
COLL19_QT.clusterCoef <- transitivity(COLL19_QTTable, type="global") #cluster coefficient
COLL19_QT.degreeCent <- centralization.degree(COLL19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL19_QTftn <- as.network.matrix(COLL19_QTft)
COLL19_QT.netDensity <- network.density(COLL19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL19_QT.entropy <- entropy(COLL19_QTft) #entropy

COLL19_QT.netMx <- cbind(COLL19_QT.netMx, COLL19_QT.clusterCoef, COLL19_QT.degreeCent$centralization,
                         COLL19_QT.netDensity, COLL19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL19_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "ESS"
KIoutcome = "Goal_F"
ESS19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
ESS19_Gg2 <- data.frame(ESS19_G)
ESS19_Gg2 <- ESS19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_Gg2$player1
player2vector <- ESS19_Gg2$player2
ESS19_Gg3 <- ESS19_Gg2
ESS19_Gg3$p1inp2vec <- is.element(ESS19_Gg3$player1, player2vector)
ESS19_Gg3$p2inp1vec <- is.element(ESS19_Gg3$player2, player1vector)

addPlayer1 <- ESS19_Gg3[ which(ESS19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS19_Gg3[ which(ESS19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_Gg2 <- rbind(ESS19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
ESS19_Gft <- ftable(ESS19_Gg2$player1, ESS19_Gg2$player2)
ESS19_Gft2 <- as.matrix(ESS19_Gft)
numRows <- nrow(ESS19_Gft2)
numCols <- ncol(ESS19_Gft2)
ESS19_Gft3 <- ESS19_Gft2[c(2:numRows) , c(2:numCols)]
ESS19_GTable <- graph.adjacency(ESS19_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(ESS19_GTable, vertex.label = V(ESS19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
ESS19_G.clusterCoef <- transitivity(ESS19_GTable, type="global") #cluster coefficient
ESS19_G.degreeCent <- centralization.degree(ESS19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_Gftn <- as.network.matrix(ESS19_Gft)
ESS19_G.netDensity <- network.density(ESS19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_G.entropy <- entropy(ESS19_Gft) #entropy

ESS19_G.netMx <- cbind(ESS19_G.netMx, ESS19_G.clusterCoef, ESS19_G.degreeCent$centralization,
                       ESS19_G.netDensity, ESS19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************

round = 19
teamName = "ESS"
KIoutcome = "Behind_F"
ESS19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
ESS19_Bg2 <- data.frame(ESS19_B)
ESS19_Bg2 <- ESS19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_Bg2$player1
player2vector <- ESS19_Bg2$player2
ESS19_Bg3 <- ESS19_Bg2
ESS19_Bg3$p1inp2vec <- is.element(ESS19_Bg3$player1, player2vector)
ESS19_Bg3$p2inp1vec <- is.element(ESS19_Bg3$player2, player1vector)

addPlayer1 <- ESS19_Bg3[ which(ESS19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS19_Bg3[ which(ESS19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_Bg2 <- rbind(ESS19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
ESS19_Bft <- ftable(ESS19_Bg2$player1, ESS19_Bg2$player2)
ESS19_Bft2 <- as.matrix(ESS19_Bft)
numRows <- nrow(ESS19_Bft2)
numCols <- ncol(ESS19_Bft2)
ESS19_Bft3 <- ESS19_Bft2[c(2:numRows) , c(2:numCols)]
ESS19_BTable <- graph.adjacency(ESS19_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(ESS19_BTable, vertex.label = V(ESS19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
ESS19_B.clusterCoef <- transitivity(ESS19_BTable, type="global") #cluster coefficient
ESS19_B.degreeCent <- centralization.degree(ESS19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_Bftn <- as.network.matrix(ESS19_Bft)
ESS19_B.netDensity <- network.density(ESS19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_B.entropy <- entropy(ESS19_Bft) #entropy

ESS19_B.netMx <- cbind(ESS19_B.netMx, ESS19_B.clusterCoef, ESS19_B.degreeCent$centralization,
                       ESS19_B.netDensity, ESS19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
ESS19_SFg2 <- data.frame(ESS19_SF)
ESS19_SFg2 <- ESS19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_SFg2$player1
player2vector <- ESS19_SFg2$player2
ESS19_SFg3 <- ESS19_SFg2
ESS19_SFg3$p1inp2vec <- is.element(ESS19_SFg3$player1, player2vector)
ESS19_SFg3$p2inp1vec <- is.element(ESS19_SFg3$player2, player1vector)

addPlayer1 <- ESS19_SFg3[ which(ESS19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS19_SFg3[ which(ESS19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_SFg2 <- rbind(ESS19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
ESS19_SFft <- ftable(ESS19_SFg2$player1, ESS19_SFg2$player2)
ESS19_SFft2 <- as.matrix(ESS19_SFft)
numRows <- nrow(ESS19_SFft2)
numCols <- ncol(ESS19_SFft2)
ESS19_SFft3 <- ESS19_SFft2[c(2:numRows) , c(2:numCols)]
ESS19_SFTable <- graph.adjacency(ESS19_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(ESS19_SFTable, vertex.label = V(ESS19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
ESS19_SF.clusterCoef <- transitivity(ESS19_SFTable, type="global") #cluster coefficient
ESS19_SF.degreeCent <- centralization.degree(ESS19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_SFftn <- as.network.matrix(ESS19_SFft)
ESS19_SF.netDensity <- network.density(ESS19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_SF.entropy <- entropy(ESS19_SFft) #entropy

ESS19_SF.netMx <- cbind(ESS19_SF.netMx, ESS19_SF.clusterCoef, ESS19_SF.degreeCent$centralization,
                        ESS19_SF.netDensity, ESS19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
ESS19_TFg2 <- data.frame(ESS19_TF)
ESS19_TFg2 <- ESS19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_TFg2$player1
player2vector <- ESS19_TFg2$player2
ESS19_TFg3 <- ESS19_TFg2
ESS19_TFg3$p1inp2vec <- is.element(ESS19_TFg3$player1, player2vector)
ESS19_TFg3$p2inp1vec <- is.element(ESS19_TFg3$player2, player1vector)

addPlayer1 <- ESS19_TFg3[ which(ESS19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS19_TFg3[ which(ESS19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_TFg2 <- rbind(ESS19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
ESS19_TFft <- ftable(ESS19_TFg2$player1, ESS19_TFg2$player2)
ESS19_TFft2 <- as.matrix(ESS19_TFft)
numRows <- nrow(ESS19_TFft2)
numCols <- ncol(ESS19_TFft2)
ESS19_TFft3 <- ESS19_TFft2[c(2:numRows) , c(2:numCols)]
ESS19_TFTable <- graph.adjacency(ESS19_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(ESS19_TFTable, vertex.label = V(ESS19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
ESS19_TF.clusterCoef <- transitivity(ESS19_TFTable, type="global") #cluster coefficient
ESS19_TF.degreeCent <- centralization.degree(ESS19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_TFftn <- as.network.matrix(ESS19_TFft)
ESS19_TF.netDensity <- network.density(ESS19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_TF.entropy <- entropy(ESS19_TFft) #entropy

ESS19_TF.netMx <- cbind(ESS19_TF.netMx, ESS19_TF.clusterCoef, ESS19_TF.degreeCent$centralization,
                        ESS19_TF.netDensity, ESS19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
ESS19_SAMg2 <- data.frame(ESS19_SAM)
ESS19_SAMg2 <- ESS19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_SAMg2$player1
player2vector <- ESS19_SAMg2$player2
ESS19_SAMg3 <- ESS19_SAMg2
ESS19_SAMg3$p1inp2vec <- is.element(ESS19_SAMg3$player1, player2vector)
ESS19_SAMg3$p2inp1vec <- is.element(ESS19_SAMg3$player2, player1vector)

addPlayer1 <- ESS19_SAMg3[ which(ESS19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS19_SAMg3[ which(ESS19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_SAMg2 <- rbind(ESS19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
ESS19_SAMft <- ftable(ESS19_SAMg2$player1, ESS19_SAMg2$player2)
ESS19_SAMft2 <- as.matrix(ESS19_SAMft)
numRows <- nrow(ESS19_SAMft2)
numCols <- ncol(ESS19_SAMft2)
ESS19_SAMft3 <- ESS19_SAMft2[c(2:numRows) , c(2:numCols)]
ESS19_SAMTable <- graph.adjacency(ESS19_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(ESS19_SAMTable, vertex.label = V(ESS19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
ESS19_SAM.clusterCoef <- transitivity(ESS19_SAMTable, type="global") #cluster coefficient
ESS19_SAM.degreeCent <- centralization.degree(ESS19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_SAMftn <- as.network.matrix(ESS19_SAMft)
ESS19_SAM.netDensity <- network.density(ESS19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_SAM.entropy <- entropy(ESS19_SAMft) #entropy

ESS19_SAM.netMx <- cbind(ESS19_SAM.netMx, ESS19_SAM.clusterCoef, ESS19_SAM.degreeCent$centralization,
                         ESS19_SAM.netDensity, ESS19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
ESS19_TAMg2 <- data.frame(ESS19_TAM)
ESS19_TAMg2 <- ESS19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_TAMg2$player1
player2vector <- ESS19_TAMg2$player2
ESS19_TAMg3 <- ESS19_TAMg2
ESS19_TAMg3$p1inp2vec <- is.element(ESS19_TAMg3$player1, player2vector)
ESS19_TAMg3$p2inp1vec <- is.element(ESS19_TAMg3$player2, player1vector)

addPlayer1 <- ESS19_TAMg3[ which(ESS19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS19_TAMg3[ which(ESS19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_TAMg2 <- rbind(ESS19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
ESS19_TAMft <- ftable(ESS19_TAMg2$player1, ESS19_TAMg2$player2)
ESS19_TAMft2 <- as.matrix(ESS19_TAMft)
numRows <- nrow(ESS19_TAMft2)
numCols <- ncol(ESS19_TAMft2)
ESS19_TAMft3 <- ESS19_TAMft2[c(2:numRows) , c(2:numCols)]
ESS19_TAMTable <- graph.adjacency(ESS19_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(ESS19_TAMTable, vertex.label = V(ESS19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
ESS19_TAM.clusterCoef <- transitivity(ESS19_TAMTable, type="global") #cluster coefficient
ESS19_TAM.degreeCent <- centralization.degree(ESS19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_TAMftn <- as.network.matrix(ESS19_TAMft)
ESS19_TAM.netDensity <- network.density(ESS19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_TAM.entropy <- entropy(ESS19_TAMft) #entropy

ESS19_TAM.netMx <- cbind(ESS19_TAM.netMx, ESS19_TAM.clusterCoef, ESS19_TAM.degreeCent$centralization,
                         ESS19_TAM.netDensity, ESS19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************

round = 19
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
ESS19_SDMg2 <- data.frame(ESS19_SDM)
ESS19_SDMg2 <- ESS19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_SDMg2$player1
player2vector <- ESS19_SDMg2$player2
ESS19_SDMg3 <- ESS19_SDMg2
ESS19_SDMg3$p1inp2vec <- is.element(ESS19_SDMg3$player1, player2vector)
ESS19_SDMg3$p2inp1vec <- is.element(ESS19_SDMg3$player2, player1vector)

addPlayer1 <- ESS19_SDMg3[ which(ESS19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS19_SDMg3[ which(ESS19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_SDMg2 <- rbind(ESS19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
ESS19_SDMft <- ftable(ESS19_SDMg2$player1, ESS19_SDMg2$player2)
ESS19_SDMft2 <- as.matrix(ESS19_SDMft)
numRows <- nrow(ESS19_SDMft2)
numCols <- ncol(ESS19_SDMft2)
ESS19_SDMft3 <- ESS19_SDMft2[c(2:numRows) , c(2:numCols)]
ESS19_SDMTable <- graph.adjacency(ESS19_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(ESS19_SDMTable, vertex.label = V(ESS19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
ESS19_SDM.clusterCoef <- transitivity(ESS19_SDMTable, type="global") #cluster coefficient
ESS19_SDM.degreeCent <- centralization.degree(ESS19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_SDMftn <- as.network.matrix(ESS19_SDMft)
ESS19_SDM.netDensity <- network.density(ESS19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_SDM.entropy <- entropy(ESS19_SDMft) #entropy

ESS19_SDM.netMx <- cbind(ESS19_SDM.netMx, ESS19_SDM.clusterCoef, ESS19_SDM.degreeCent$centralization,
                         ESS19_SDM.netDensity, ESS19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
ESS19_TDMg2 <- data.frame(ESS19_TDM)
ESS19_TDMg2 <- ESS19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_TDMg2$player1
player2vector <- ESS19_TDMg2$player2
ESS19_TDMg3 <- ESS19_TDMg2
ESS19_TDMg3$p1inp2vec <- is.element(ESS19_TDMg3$player1, player2vector)
ESS19_TDMg3$p2inp1vec <- is.element(ESS19_TDMg3$player2, player1vector)

addPlayer1 <- ESS19_TDMg3[ which(ESS19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS19_TDMg3[ which(ESS19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_TDMg2 <- rbind(ESS19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
ESS19_TDMft <- ftable(ESS19_TDMg2$player1, ESS19_TDMg2$player2)
ESS19_TDMft2 <- as.matrix(ESS19_TDMft)
numRows <- nrow(ESS19_TDMft2)
numCols <- ncol(ESS19_TDMft2)
ESS19_TDMft3 <- ESS19_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS19_TDMTable <- graph.adjacency(ESS19_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(ESS19_TDMTable, vertex.label = V(ESS19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
ESS19_TDM.clusterCoef <- transitivity(ESS19_TDMTable, type="global") #cluster coefficient
ESS19_TDM.degreeCent <- centralization.degree(ESS19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_TDMftn <- as.network.matrix(ESS19_TDMft)
ESS19_TDM.netDensity <- network.density(ESS19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_TDM.entropy <- entropy(ESS19_TDMft) #entropy

ESS19_TDM.netMx <- cbind(ESS19_TDM.netMx, ESS19_TDM.clusterCoef, ESS19_TDM.degreeCent$centralization,
                         ESS19_TDM.netDensity, ESS19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
ESS19_SDg2 <- data.frame(ESS19_SD)
ESS19_SDg2 <- ESS19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_SDg2$player1
player2vector <- ESS19_SDg2$player2
ESS19_SDg3 <- ESS19_SDg2
ESS19_SDg3$p1inp2vec <- is.element(ESS19_SDg3$player1, player2vector)
ESS19_SDg3$p2inp1vec <- is.element(ESS19_SDg3$player2, player1vector)

addPlayer1 <- ESS19_SDg3[ which(ESS19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS19_SDg3[ which(ESS19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_SDg2 <- rbind(ESS19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
ESS19_SDft <- ftable(ESS19_SDg2$player1, ESS19_SDg2$player2)
ESS19_SDft2 <- as.matrix(ESS19_SDft)
numRows <- nrow(ESS19_SDft2)
numCols <- ncol(ESS19_SDft2)
ESS19_SDft3 <- ESS19_SDft2[c(2:numRows) , c(2:numCols)]
ESS19_SDTable <- graph.adjacency(ESS19_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(ESS19_SDTable, vertex.label = V(ESS19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
ESS19_SD.clusterCoef <- transitivity(ESS19_SDTable, type="global") #cluster coefficient
ESS19_SD.degreeCent <- centralization.degree(ESS19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_SDftn <- as.network.matrix(ESS19_SDft)
ESS19_SD.netDensity <- network.density(ESS19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_SD.entropy <- entropy(ESS19_SDft) #entropy

ESS19_SD.netMx <- cbind(ESS19_SD.netMx, ESS19_SD.clusterCoef, ESS19_SD.degreeCent$centralization,
                        ESS19_SD.netDensity, ESS19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************

round = 19
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
ESS19_TDg2 <- data.frame(ESS19_TD)
ESS19_TDg2 <- ESS19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_TDg2$player1
player2vector <- ESS19_TDg2$player2
ESS19_TDg3 <- ESS19_TDg2
ESS19_TDg3$p1inp2vec <- is.element(ESS19_TDg3$player1, player2vector)
ESS19_TDg3$p2inp1vec <- is.element(ESS19_TDg3$player2, player1vector)

addPlayer1 <- ESS19_TDg3[ which(ESS19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS19_TDg3[ which(ESS19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_TDg2 <- rbind(ESS19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
ESS19_TDft <- ftable(ESS19_TDg2$player1, ESS19_TDg2$player2)
ESS19_TDft2 <- as.matrix(ESS19_TDft)
numRows <- nrow(ESS19_TDft2)
numCols <- ncol(ESS19_TDft2)
ESS19_TDft3 <- ESS19_TDft2[c(2:numRows) , c(2:numCols)]
ESS19_TDTable <- graph.adjacency(ESS19_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(ESS19_TDTable, vertex.label = V(ESS19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
ESS19_TD.clusterCoef <- transitivity(ESS19_TDTable, type="global") #cluster coefficient
ESS19_TD.degreeCent <- centralization.degree(ESS19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_TDftn <- as.network.matrix(ESS19_TDft)
ESS19_TD.netDensity <- network.density(ESS19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_TD.entropy <- entropy(ESS19_TDft) #entropy

ESS19_TD.netMx <- cbind(ESS19_TD.netMx, ESS19_TD.clusterCoef, ESS19_TD.degreeCent$centralization,
                        ESS19_TD.netDensity, ESS19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
ESS19_QTg2 <- data.frame(ESS19_QT)
ESS19_QTg2 <- ESS19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS19_QTg2$player1
player2vector <- ESS19_QTg2$player2
ESS19_QTg3 <- ESS19_QTg2
ESS19_QTg3$p1inp2vec <- is.element(ESS19_QTg3$player1, player2vector)
ESS19_QTg3$p2inp1vec <- is.element(ESS19_QTg3$player2, player1vector)

addPlayer1 <- ESS19_QTg3[ which(ESS19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS19_QTg3[ which(ESS19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS19_QTg2 <- rbind(ESS19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
ESS19_QTft <- ftable(ESS19_QTg2$player1, ESS19_QTg2$player2)
ESS19_QTft2 <- as.matrix(ESS19_QTft)
numRows <- nrow(ESS19_QTft2)
numCols <- ncol(ESS19_QTft2)
ESS19_QTft3 <- ESS19_QTft2[c(2:numRows) , c(2:numCols)]
ESS19_QTTable <- graph.adjacency(ESS19_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(ESS19_QTTable, vertex.label = V(ESS19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
ESS19_QT.clusterCoef <- transitivity(ESS19_QTTable, type="global") #cluster coefficient
ESS19_QT.degreeCent <- centralization.degree(ESS19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS19_QTftn <- as.network.matrix(ESS19_QTft)
ESS19_QT.netDensity <- network.density(ESS19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS19_QT.entropy <- entropy(ESS19_QTft) #entropy

ESS19_QT.netMx <- cbind(ESS19_QT.netMx, ESS19_QT.clusterCoef, ESS19_QT.degreeCent$centralization,
                        ESS19_QT.netDensity, ESS19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS19_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************

round = 19
teamName = "FRE"
KIoutcome = "Goal_F"
FRE19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
FRE19_Gg2 <- data.frame(FRE19_G)
FRE19_Gg2 <- FRE19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_Gg2$player1
player2vector <- FRE19_Gg2$player2
FRE19_Gg3 <- FRE19_Gg2
FRE19_Gg3$p1inp2vec <- is.element(FRE19_Gg3$player1, player2vector)
FRE19_Gg3$p2inp1vec <- is.element(FRE19_Gg3$player2, player1vector)

addPlayer1 <- FRE19_Gg3[ which(FRE19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE19_Gg3[ which(FRE19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_Gg2 <- rbind(FRE19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
FRE19_Gft <- ftable(FRE19_Gg2$player1, FRE19_Gg2$player2)
FRE19_Gft2 <- as.matrix(FRE19_Gft)
numRows <- nrow(FRE19_Gft2)
numCols <- ncol(FRE19_Gft2)
FRE19_Gft3 <- FRE19_Gft2[c(2:numRows) , c(2:numCols)]
FRE19_GTable <- graph.adjacency(FRE19_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(FRE19_GTable, vertex.label = V(FRE19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
FRE19_G.clusterCoef <- transitivity(FRE19_GTable, type="global") #cluster coefficient
FRE19_G.degreeCent <- centralization.degree(FRE19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_Gftn <- as.network.matrix(FRE19_Gft)
FRE19_G.netDensity <- network.density(FRE19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_G.entropy <- entropy(FRE19_Gft) #entropy

FRE19_G.netMx <- cbind(FRE19_G.netMx, FRE19_G.clusterCoef, FRE19_G.degreeCent$centralization,
                       FRE19_G.netDensity, FRE19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "FRE"
KIoutcome = "Behind_F"
FRE19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
FRE19_Bg2 <- data.frame(FRE19_B)
FRE19_Bg2 <- FRE19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_Bg2$player1
player2vector <- FRE19_Bg2$player2
FRE19_Bg3 <- FRE19_Bg2
FRE19_Bg3$p1inp2vec <- is.element(FRE19_Bg3$player1, player2vector)
FRE19_Bg3$p2inp1vec <- is.element(FRE19_Bg3$player2, player1vector)

addPlayer1 <- FRE19_Bg3[ which(FRE19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_Bg3[ which(FRE19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_Bg2 <- rbind(FRE19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
FRE19_Bft <- ftable(FRE19_Bg2$player1, FRE19_Bg2$player2)
FRE19_Bft2 <- as.matrix(FRE19_Bft)
numRows <- nrow(FRE19_Bft2)
numCols <- ncol(FRE19_Bft2)
FRE19_Bft3 <- FRE19_Bft2[c(2:numRows) , c(2:numCols)]
FRE19_BTable <- graph.adjacency(FRE19_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(FRE19_BTable, vertex.label = V(FRE19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
FRE19_B.clusterCoef <- transitivity(FRE19_BTable, type="global") #cluster coefficient
FRE19_B.degreeCent <- centralization.degree(FRE19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_Bftn <- as.network.matrix(FRE19_Bft)
FRE19_B.netDensity <- network.density(FRE19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_B.entropy <- entropy(FRE19_Bft) #entropy

FRE19_B.netMx <- cbind(FRE19_B.netMx, FRE19_B.clusterCoef, FRE19_B.degreeCent$centralization,
                       FRE19_B.netDensity, FRE19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
FRE19_SFg2 <- data.frame(FRE19_SF)
FRE19_SFg2 <- FRE19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_SFg2$player1
player2vector <- FRE19_SFg2$player2
FRE19_SFg3 <- FRE19_SFg2
FRE19_SFg3$p1inp2vec <- is.element(FRE19_SFg3$player1, player2vector)
FRE19_SFg3$p2inp1vec <- is.element(FRE19_SFg3$player2, player1vector)

addPlayer1 <- FRE19_SFg3[ which(FRE19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_SFg3[ which(FRE19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_SFg2 <- rbind(FRE19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
FRE19_SFft <- ftable(FRE19_SFg2$player1, FRE19_SFg2$player2)
FRE19_SFft2 <- as.matrix(FRE19_SFft)
numRows <- nrow(FRE19_SFft2)
numCols <- ncol(FRE19_SFft2)
FRE19_SFft3 <- FRE19_SFft2[c(2:numRows) , c(2:numCols)]
FRE19_SFTable <- graph.adjacency(FRE19_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(FRE19_SFTable, vertex.label = V(FRE19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
FRE19_SF.clusterCoef <- transitivity(FRE19_SFTable, type="global") #cluster coefficient
FRE19_SF.degreeCent <- centralization.degree(FRE19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_SFftn <- as.network.matrix(FRE19_SFft)
FRE19_SF.netDensity <- network.density(FRE19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_SF.entropy <- entropy(FRE19_SFft) #entropy

FRE19_SF.netMx <- cbind(FRE19_SF.netMx, FRE19_SF.clusterCoef, FRE19_SF.degreeCent$centralization,
                        FRE19_SF.netDensity, FRE19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************
#NA

round = 19
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
FRE19_TFg2 <- data.frame(FRE19_TF)
FRE19_TFg2 <- FRE19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_TFg2$player1
player2vector <- FRE19_TFg2$player2
FRE19_TFg3 <- FRE19_TFg2
FRE19_TFg3$p1inp2vec <- is.element(FRE19_TFg3$player1, player2vector)
FRE19_TFg3$p2inp1vec <- is.element(FRE19_TFg3$player2, player1vector)

addPlayer1 <- FRE19_TFg3[ which(FRE19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_TFg3[ which(FRE19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_TFg2 <- rbind(FRE19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
FRE19_TFft <- ftable(FRE19_TFg2$player1, FRE19_TFg2$player2)
FRE19_TFft2 <- as.matrix(FRE19_TFft)
numRows <- nrow(FRE19_TFft2)
numCols <- ncol(FRE19_TFft2)
FRE19_TFft3 <- FRE19_TFft2[c(2:numRows) , c(2:numCols)]
FRE19_TFTable <- graph.adjacency(FRE19_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(FRE19_TFTable, vertex.label = V(FRE19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
FRE19_TF.clusterCoef <- transitivity(FRE19_TFTable, type="global") #cluster coefficient
FRE19_TF.degreeCent <- centralization.degree(FRE19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_TFftn <- as.network.matrix(FRE19_TFft)
FRE19_TF.netDensity <- network.density(FRE19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_TF.entropy <- entropy(FRE19_TFft) #entropy

FRE19_TF.netMx <- cbind(FRE19_TF.netMx, FRE19_TF.clusterCoef, FRE19_TF.degreeCent$centralization,
                        FRE19_TF.netDensity, FRE19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
FRE19_SAMg2 <- data.frame(FRE19_SAM)
FRE19_SAMg2 <- FRE19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_SAMg2$player1
player2vector <- FRE19_SAMg2$player2
FRE19_SAMg3 <- FRE19_SAMg2
FRE19_SAMg3$p1inp2vec <- is.element(FRE19_SAMg3$player1, player2vector)
FRE19_SAMg3$p2inp1vec <- is.element(FRE19_SAMg3$player2, player1vector)

addPlayer1 <- FRE19_SAMg3[ which(FRE19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_SAMg3[ which(FRE19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_SAMg2 <- rbind(FRE19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
FRE19_SAMft <- ftable(FRE19_SAMg2$player1, FRE19_SAMg2$player2)
FRE19_SAMft2 <- as.matrix(FRE19_SAMft)
numRows <- nrow(FRE19_SAMft2)
numCols <- ncol(FRE19_SAMft2)
FRE19_SAMft3 <- FRE19_SAMft2[c(2:numRows) , c(2:numCols)]
FRE19_SAMTable <- graph.adjacency(FRE19_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(FRE19_SAMTable, vertex.label = V(FRE19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
FRE19_SAM.clusterCoef <- transitivity(FRE19_SAMTable, type="global") #cluster coefficient
FRE19_SAM.degreeCent <- centralization.degree(FRE19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_SAMftn <- as.network.matrix(FRE19_SAMft)
FRE19_SAM.netDensity <- network.density(FRE19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_SAM.entropy <- entropy(FRE19_SAMft) #entropy

FRE19_SAM.netMx <- cbind(FRE19_SAM.netMx, FRE19_SAM.clusterCoef, FRE19_SAM.degreeCent$centralization,
                         FRE19_SAM.netDensity, FRE19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
FRE19_TAMg2 <- data.frame(FRE19_TAM)
FRE19_TAMg2 <- FRE19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_TAMg2$player1
player2vector <- FRE19_TAMg2$player2
FRE19_TAMg3 <- FRE19_TAMg2
FRE19_TAMg3$p1inp2vec <- is.element(FRE19_TAMg3$player1, player2vector)
FRE19_TAMg3$p2inp1vec <- is.element(FRE19_TAMg3$player2, player1vector)

addPlayer1 <- FRE19_TAMg3[ which(FRE19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_TAMg3[ which(FRE19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_TAMg2 <- rbind(FRE19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
FRE19_TAMft <- ftable(FRE19_TAMg2$player1, FRE19_TAMg2$player2)
FRE19_TAMft2 <- as.matrix(FRE19_TAMft)
numRows <- nrow(FRE19_TAMft2)
numCols <- ncol(FRE19_TAMft2)
FRE19_TAMft3 <- FRE19_TAMft2[c(2:numRows) , c(2:numCols)]
FRE19_TAMTable <- graph.adjacency(FRE19_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(FRE19_TAMTable, vertex.label = V(FRE19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
FRE19_TAM.clusterCoef <- transitivity(FRE19_TAMTable, type="global") #cluster coefficient
FRE19_TAM.degreeCent <- centralization.degree(FRE19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_TAMftn <- as.network.matrix(FRE19_TAMft)
FRE19_TAM.netDensity <- network.density(FRE19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_TAM.entropy <- entropy(FRE19_TAMft) #entropy

FRE19_TAM.netMx <- cbind(FRE19_TAM.netMx, FRE19_TAM.clusterCoef, FRE19_TAM.degreeCent$centralization,
                         FRE19_TAM.netDensity, FRE19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************

round = 19
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
FRE19_SDMg2 <- data.frame(FRE19_SDM)
FRE19_SDMg2 <- FRE19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_SDMg2$player1
player2vector <- FRE19_SDMg2$player2
FRE19_SDMg3 <- FRE19_SDMg2
FRE19_SDMg3$p1inp2vec <- is.element(FRE19_SDMg3$player1, player2vector)
FRE19_SDMg3$p2inp1vec <- is.element(FRE19_SDMg3$player2, player1vector)

addPlayer1 <- FRE19_SDMg3[ which(FRE19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_SDMg3[ which(FRE19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_SDMg2 <- rbind(FRE19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
FRE19_SDMft <- ftable(FRE19_SDMg2$player1, FRE19_SDMg2$player2)
FRE19_SDMft2 <- as.matrix(FRE19_SDMft)
numRows <- nrow(FRE19_SDMft2)
numCols <- ncol(FRE19_SDMft2)
FRE19_SDMft3 <- FRE19_SDMft2[c(2:numRows) , c(2:numCols)]
FRE19_SDMTable <- graph.adjacency(FRE19_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(FRE19_SDMTable, vertex.label = V(FRE19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
FRE19_SDM.clusterCoef <- transitivity(FRE19_SDMTable, type="global") #cluster coefficient
FRE19_SDM.degreeCent <- centralization.degree(FRE19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_SDMftn <- as.network.matrix(FRE19_SDMft)
FRE19_SDM.netDensity <- network.density(FRE19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_SDM.entropy <- entropy(FRE19_SDMft) #entropy

FRE19_SDM.netMx <- cbind(FRE19_SDM.netMx, FRE19_SDM.clusterCoef, FRE19_SDM.degreeCent$centralization,
                         FRE19_SDM.netDensity, FRE19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
FRE19_TDMg2 <- data.frame(FRE19_TDM)
FRE19_TDMg2 <- FRE19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_TDMg2$player1
player2vector <- FRE19_TDMg2$player2
FRE19_TDMg3 <- FRE19_TDMg2
FRE19_TDMg3$p1inp2vec <- is.element(FRE19_TDMg3$player1, player2vector)
FRE19_TDMg3$p2inp1vec <- is.element(FRE19_TDMg3$player2, player1vector)

addPlayer1 <- FRE19_TDMg3[ which(FRE19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_TDMg3[ which(FRE19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_TDMg2 <- rbind(FRE19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
FRE19_TDMft <- ftable(FRE19_TDMg2$player1, FRE19_TDMg2$player2)
FRE19_TDMft2 <- as.matrix(FRE19_TDMft)
numRows <- nrow(FRE19_TDMft2)
numCols <- ncol(FRE19_TDMft2)
FRE19_TDMft3 <- FRE19_TDMft2[c(2:numRows) , c(2:numCols)]
FRE19_TDMTable <- graph.adjacency(FRE19_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(FRE19_TDMTable, vertex.label = V(FRE19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
FRE19_TDM.clusterCoef <- transitivity(FRE19_TDMTable, type="global") #cluster coefficient
FRE19_TDM.degreeCent <- centralization.degree(FRE19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_TDMftn <- as.network.matrix(FRE19_TDMft)
FRE19_TDM.netDensity <- network.density(FRE19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_TDM.entropy <- entropy(FRE19_TDMft) #entropy

FRE19_TDM.netMx <- cbind(FRE19_TDM.netMx, FRE19_TDM.clusterCoef, FRE19_TDM.degreeCent$centralization,
                         FRE19_TDM.netDensity, FRE19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
FRE19_SDg2 <- data.frame(FRE19_SD)
FRE19_SDg2 <- FRE19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_SDg2$player1
player2vector <- FRE19_SDg2$player2
FRE19_SDg3 <- FRE19_SDg2
FRE19_SDg3$p1inp2vec <- is.element(FRE19_SDg3$player1, player2vector)
FRE19_SDg3$p2inp1vec <- is.element(FRE19_SDg3$player2, player1vector)

addPlayer1 <- FRE19_SDg3[ which(FRE19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_SDg3[ which(FRE19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_SDg2 <- rbind(FRE19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
FRE19_SDft <- ftable(FRE19_SDg2$player1, FRE19_SDg2$player2)
FRE19_SDft2 <- as.matrix(FRE19_SDft)
numRows <- nrow(FRE19_SDft2)
numCols <- ncol(FRE19_SDft2)
FRE19_SDft3 <- FRE19_SDft2[c(2:numRows) , c(2:numCols)]
FRE19_SDTable <- graph.adjacency(FRE19_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(FRE19_SDTable, vertex.label = V(FRE19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
FRE19_SD.clusterCoef <- transitivity(FRE19_SDTable, type="global") #cluster coefficient
FRE19_SD.degreeCent <- centralization.degree(FRE19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_SDftn <- as.network.matrix(FRE19_SDft)
FRE19_SD.netDensity <- network.density(FRE19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_SD.entropy <- entropy(FRE19_SDft) #entropy

FRE19_SD.netMx <- cbind(FRE19_SD.netMx, FRE19_SD.clusterCoef, FRE19_SD.degreeCent$centralization,
                        FRE19_SD.netDensity, FRE19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
FRE19_TDg2 <- data.frame(FRE19_TD)
FRE19_TDg2 <- FRE19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_TDg2$player1
player2vector <- FRE19_TDg2$player2
FRE19_TDg3 <- FRE19_TDg2
FRE19_TDg3$p1inp2vec <- is.element(FRE19_TDg3$player1, player2vector)
FRE19_TDg3$p2inp1vec <- is.element(FRE19_TDg3$player2, player1vector)

addPlayer1 <- FRE19_TDg3[ which(FRE19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_TDg3[ which(FRE19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_TDg2 <- rbind(FRE19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
FRE19_TDft <- ftable(FRE19_TDg2$player1, FRE19_TDg2$player2)
FRE19_TDft2 <- as.matrix(FRE19_TDft)
numRows <- nrow(FRE19_TDft2)
numCols <- ncol(FRE19_TDft2)
FRE19_TDft3 <- FRE19_TDft2[c(2:numRows) , c(2:numCols)]
FRE19_TDTable <- graph.adjacency(FRE19_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(FRE19_TDTable, vertex.label = V(FRE19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
FRE19_TD.clusterCoef <- transitivity(FRE19_TDTable, type="global") #cluster coefficient
FRE19_TD.degreeCent <- centralization.degree(FRE19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_TDftn <- as.network.matrix(FRE19_TDft)
FRE19_TD.netDensity <- network.density(FRE19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_TD.entropy <- entropy(FRE19_TDft) #entropy

FRE19_TD.netMx <- cbind(FRE19_TD.netMx, FRE19_TD.clusterCoef, FRE19_TD.degreeCent$centralization,
                        FRE19_TD.netDensity, FRE19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
FRE19_QTg2 <- data.frame(FRE19_QT)
FRE19_QTg2 <- FRE19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE19_QTg2$player1
player2vector <- FRE19_QTg2$player2
FRE19_QTg3 <- FRE19_QTg2
FRE19_QTg3$p1inp2vec <- is.element(FRE19_QTg3$player1, player2vector)
FRE19_QTg3$p2inp1vec <- is.element(FRE19_QTg3$player2, player1vector)

addPlayer1 <- FRE19_QTg3[ which(FRE19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE19_QTg3[ which(FRE19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE19_QTg2 <- rbind(FRE19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
FRE19_QTft <- ftable(FRE19_QTg2$player1, FRE19_QTg2$player2)
FRE19_QTft2 <- as.matrix(FRE19_QTft)
numRows <- nrow(FRE19_QTft2)
numCols <- ncol(FRE19_QTft2)
FRE19_QTft3 <- FRE19_QTft2[c(2:numRows) , c(2:numCols)]
FRE19_QTTable <- graph.adjacency(FRE19_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(FRE19_QTTable, vertex.label = V(FRE19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
FRE19_QT.clusterCoef <- transitivity(FRE19_QTTable, type="global") #cluster coefficient
FRE19_QT.degreeCent <- centralization.degree(FRE19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE19_QTftn <- as.network.matrix(FRE19_QTft)
FRE19_QT.netDensity <- network.density(FRE19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE19_QT.entropy <- entropy(FRE19_QTft) #entropy

FRE19_QT.netMx <- cbind(FRE19_QT.netMx, FRE19_QT.clusterCoef, FRE19_QT.degreeCent$centralization,
                        FRE19_QT.netDensity, FRE19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE19_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************

round = 19
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
GCFC19_Gg2 <- data.frame(GCFC19_G)
GCFC19_Gg2 <- GCFC19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_Gg2$player1
player2vector <- GCFC19_Gg2$player2
GCFC19_Gg3 <- GCFC19_Gg2
GCFC19_Gg3$p1inp2vec <- is.element(GCFC19_Gg3$player1, player2vector)
GCFC19_Gg3$p2inp1vec <- is.element(GCFC19_Gg3$player2, player1vector)

addPlayer1 <- GCFC19_Gg3[ which(GCFC19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_Gg3[ which(GCFC19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_Gg2 <- rbind(GCFC19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
GCFC19_Gft <- ftable(GCFC19_Gg2$player1, GCFC19_Gg2$player2)
GCFC19_Gft2 <- as.matrix(GCFC19_Gft)
numRows <- nrow(GCFC19_Gft2)
numCols <- ncol(GCFC19_Gft2)
GCFC19_Gft3 <- GCFC19_Gft2[c(2:numRows) , c(2:numCols)]
GCFC19_GTable <- graph.adjacency(GCFC19_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(GCFC19_GTable, vertex.label = V(GCFC19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
GCFC19_G.clusterCoef <- transitivity(GCFC19_GTable, type="global") #cluster coefficient
GCFC19_G.degreeCent <- centralization.degree(GCFC19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_Gftn <- as.network.matrix(GCFC19_Gft)
GCFC19_G.netDensity <- network.density(GCFC19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_G.entropy <- entropy(GCFC19_Gft) #entropy

GCFC19_G.netMx <- cbind(GCFC19_G.netMx, GCFC19_G.clusterCoef, GCFC19_G.degreeCent$centralization,
                        GCFC19_G.netDensity, GCFC19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
GCFC19_Bg2 <- data.frame(GCFC19_B)
GCFC19_Bg2 <- GCFC19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_Bg2$player1
player2vector <- GCFC19_Bg2$player2
GCFC19_Bg3 <- GCFC19_Bg2
GCFC19_Bg3$p1inp2vec <- is.element(GCFC19_Bg3$player1, player2vector)
GCFC19_Bg3$p2inp1vec <- is.element(GCFC19_Bg3$player2, player1vector)

addPlayer1 <- GCFC19_Bg3[ which(GCFC19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_Bg3[ which(GCFC19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_Bg2 <- rbind(GCFC19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
GCFC19_Bft <- ftable(GCFC19_Bg2$player1, GCFC19_Bg2$player2)
GCFC19_Bft2 <- as.matrix(GCFC19_Bft)
numRows <- nrow(GCFC19_Bft2)
numCols <- ncol(GCFC19_Bft2)
GCFC19_Bft3 <- GCFC19_Bft2[c(2:numRows) , c(2:numCols)]
GCFC19_BTable <- graph.adjacency(GCFC19_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(GCFC19_BTable, vertex.label = V(GCFC19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
GCFC19_B.clusterCoef <- transitivity(GCFC19_BTable, type="global") #cluster coefficient
GCFC19_B.degreeCent <- centralization.degree(GCFC19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_Bftn <- as.network.matrix(GCFC19_Bft)
GCFC19_B.netDensity <- network.density(GCFC19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_B.entropy <- entropy(GCFC19_Bft) #entropy

GCFC19_B.netMx <- cbind(GCFC19_B.netMx, GCFC19_B.clusterCoef, GCFC19_B.degreeCent$centralization,
                        GCFC19_B.netDensity, GCFC19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
GCFC19_SFg2 <- data.frame(GCFC19_SF)
GCFC19_SFg2 <- GCFC19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_SFg2$player1
player2vector <- GCFC19_SFg2$player2
GCFC19_SFg3 <- GCFC19_SFg2
GCFC19_SFg3$p1inp2vec <- is.element(GCFC19_SFg3$player1, player2vector)
GCFC19_SFg3$p2inp1vec <- is.element(GCFC19_SFg3$player2, player1vector)

addPlayer1 <- GCFC19_SFg3[ which(GCFC19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC19_SFg2 <- rbind(GCFC19_SFg2, addPlayer1)

#ROUND 19, FWD Stoppage graph using weighted edges
GCFC19_SFft <- ftable(GCFC19_SFg2$player1, GCFC19_SFg2$player2)
GCFC19_SFft2 <- as.matrix(GCFC19_SFft)
numRows <- nrow(GCFC19_SFft2)
numCols <- ncol(GCFC19_SFft2)
GCFC19_SFft3 <- GCFC19_SFft2[c(2:numRows) , c(1:numCols)]
GCFC19_SFTable <- graph.adjacency(GCFC19_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(GCFC19_SFTable, vertex.label = V(GCFC19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
GCFC19_SF.clusterCoef <- transitivity(GCFC19_SFTable, type="global") #cluster coefficient
GCFC19_SF.degreeCent <- centralization.degree(GCFC19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_SFftn <- as.network.matrix(GCFC19_SFft)
GCFC19_SF.netDensity <- network.density(GCFC19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_SF.entropy <- entropy(GCFC19_SFft) #entropy

GCFC19_SF.netMx <- cbind(GCFC19_SF.netMx, GCFC19_SF.clusterCoef, GCFC19_SF.degreeCent$centralization,
                         GCFC19_SF.netDensity, GCFC19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
GCFC19_TFg2 <- data.frame(GCFC19_TF)
GCFC19_TFg2 <- GCFC19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_TFg2$player1
player2vector <- GCFC19_TFg2$player2
GCFC19_TFg3 <- GCFC19_TFg2
GCFC19_TFg3$p1inp2vec <- is.element(GCFC19_TFg3$player1, player2vector)
GCFC19_TFg3$p2inp1vec <- is.element(GCFC19_TFg3$player2, player1vector)

addPlayer1 <- GCFC19_TFg3[ which(GCFC19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_TFg3[ which(GCFC19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_TFg2 <- rbind(GCFC19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
GCFC19_TFft <- ftable(GCFC19_TFg2$player1, GCFC19_TFg2$player2)
GCFC19_TFft2 <- as.matrix(GCFC19_TFft)
numRows <- nrow(GCFC19_TFft2)
numCols <- ncol(GCFC19_TFft2)
GCFC19_TFft3 <- GCFC19_TFft2[c(2:numRows) , c(2:numCols)]
GCFC19_TFTable <- graph.adjacency(GCFC19_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(GCFC19_TFTable, vertex.label = V(GCFC19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
GCFC19_TF.clusterCoef <- transitivity(GCFC19_TFTable, type="global") #cluster coefficient
GCFC19_TF.degreeCent <- centralization.degree(GCFC19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_TFftn <- as.network.matrix(GCFC19_TFft)
GCFC19_TF.netDensity <- network.density(GCFC19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_TF.entropy <- entropy(GCFC19_TFft) #entropy

GCFC19_TF.netMx <- cbind(GCFC19_TF.netMx, GCFC19_TF.clusterCoef, GCFC19_TF.degreeCent$centralization,
                         GCFC19_TF.netDensity, GCFC19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
GCFC19_SAMg2 <- data.frame(GCFC19_SAM)
GCFC19_SAMg2 <- GCFC19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_SAMg2$player1
player2vector <- GCFC19_SAMg2$player2
GCFC19_SAMg3 <- GCFC19_SAMg2
GCFC19_SAMg3$p1inp2vec <- is.element(GCFC19_SAMg3$player1, player2vector)
GCFC19_SAMg3$p2inp1vec <- is.element(GCFC19_SAMg3$player2, player1vector)

addPlayer1 <- GCFC19_SAMg3[ which(GCFC19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_SAMg3[ which(GCFC19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_SAMg2 <- rbind(GCFC19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
GCFC19_SAMft <- ftable(GCFC19_SAMg2$player1, GCFC19_SAMg2$player2)
GCFC19_SAMft2 <- as.matrix(GCFC19_SAMft)
numRows <- nrow(GCFC19_SAMft2)
numCols <- ncol(GCFC19_SAMft2)
GCFC19_SAMft3 <- GCFC19_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC19_SAMTable <- graph.adjacency(GCFC19_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(GCFC19_SAMTable, vertex.label = V(GCFC19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
GCFC19_SAM.clusterCoef <- transitivity(GCFC19_SAMTable, type="global") #cluster coefficient
GCFC19_SAM.degreeCent <- centralization.degree(GCFC19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_SAMftn <- as.network.matrix(GCFC19_SAMft)
GCFC19_SAM.netDensity <- network.density(GCFC19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_SAM.entropy <- entropy(GCFC19_SAMft) #entropy

GCFC19_SAM.netMx <- cbind(GCFC19_SAM.netMx, GCFC19_SAM.clusterCoef, GCFC19_SAM.degreeCent$centralization,
                          GCFC19_SAM.netDensity, GCFC19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************
#NA

round = 19
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
GCFC19_TAMg2 <- data.frame(GCFC19_TAM)
GCFC19_TAMg2 <- GCFC19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_TAMg2$player1
player2vector <- GCFC19_TAMg2$player2
GCFC19_TAMg3 <- GCFC19_TAMg2
GCFC19_TAMg3$p1inp2vec <- is.element(GCFC19_TAMg3$player1, player2vector)
GCFC19_TAMg3$p2inp1vec <- is.element(GCFC19_TAMg3$player2, player1vector)

addPlayer1 <- GCFC19_TAMg3[ which(GCFC19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_TAMg3[ which(GCFC19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_TAMg2 <- rbind(GCFC19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
GCFC19_TAMft <- ftable(GCFC19_TAMg2$player1, GCFC19_TAMg2$player2)
GCFC19_TAMft2 <- as.matrix(GCFC19_TAMft)
numRows <- nrow(GCFC19_TAMft2)
numCols <- ncol(GCFC19_TAMft2)
GCFC19_TAMft3 <- GCFC19_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC19_TAMTable <- graph.adjacency(GCFC19_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(GCFC19_TAMTable, vertex.label = V(GCFC19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
GCFC19_TAM.clusterCoef <- transitivity(GCFC19_TAMTable, type="global") #cluster coefficient
GCFC19_TAM.degreeCent <- centralization.degree(GCFC19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_TAMftn <- as.network.matrix(GCFC19_TAMft)
GCFC19_TAM.netDensity <- network.density(GCFC19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_TAM.entropy <- entropy(GCFC19_TAMft) #entropy

GCFC19_TAM.netMx <- cbind(GCFC19_TAM.netMx, GCFC19_TAM.clusterCoef, GCFC19_TAM.degreeCent$centralization,
                          GCFC19_TAM.netDensity, GCFC19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************
#NA

round = 19
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
GCFC19_SDMg2 <- data.frame(GCFC19_SDM)
GCFC19_SDMg2 <- GCFC19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_SDMg2$player1
player2vector <- GCFC19_SDMg2$player2
GCFC19_SDMg3 <- GCFC19_SDMg2
GCFC19_SDMg3$p1inp2vec <- is.element(GCFC19_SDMg3$player1, player2vector)
GCFC19_SDMg3$p2inp1vec <- is.element(GCFC19_SDMg3$player2, player1vector)

addPlayer1 <- GCFC19_SDMg3[ which(GCFC19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_SDMg3[ which(GCFC19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_SDMg2 <- rbind(GCFC19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
GCFC19_SDMft <- ftable(GCFC19_SDMg2$player1, GCFC19_SDMg2$player2)
GCFC19_SDMft2 <- as.matrix(GCFC19_SDMft)
numRows <- nrow(GCFC19_SDMft2)
numCols <- ncol(GCFC19_SDMft2)
GCFC19_SDMft3 <- GCFC19_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC19_SDMTable <- graph.adjacency(GCFC19_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(GCFC19_SDMTable, vertex.label = V(GCFC19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
GCFC19_SDM.clusterCoef <- transitivity(GCFC19_SDMTable, type="global") #cluster coefficient
GCFC19_SDM.degreeCent <- centralization.degree(GCFC19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_SDMftn <- as.network.matrix(GCFC19_SDMft)
GCFC19_SDM.netDensity <- network.density(GCFC19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_SDM.entropy <- entropy(GCFC19_SDMft) #entropy

GCFC19_SDM.netMx <- cbind(GCFC19_SDM.netMx, GCFC19_SDM.clusterCoef, GCFC19_SDM.degreeCent$centralization,
                          GCFC19_SDM.netDensity, GCFC19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
GCFC19_TDMg2 <- data.frame(GCFC19_TDM)
GCFC19_TDMg2 <- GCFC19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_TDMg2$player1
player2vector <- GCFC19_TDMg2$player2
GCFC19_TDMg3 <- GCFC19_TDMg2
GCFC19_TDMg3$p1inp2vec <- is.element(GCFC19_TDMg3$player1, player2vector)
GCFC19_TDMg3$p2inp1vec <- is.element(GCFC19_TDMg3$player2, player1vector)

addPlayer1 <- GCFC19_TDMg3[ which(GCFC19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_TDMg3[ which(GCFC19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_TDMg2 <- rbind(GCFC19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
GCFC19_TDMft <- ftable(GCFC19_TDMg2$player1, GCFC19_TDMg2$player2)
GCFC19_TDMft2 <- as.matrix(GCFC19_TDMft)
numRows <- nrow(GCFC19_TDMft2)
numCols <- ncol(GCFC19_TDMft2)
GCFC19_TDMft3 <- GCFC19_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC19_TDMTable <- graph.adjacency(GCFC19_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(GCFC19_TDMTable, vertex.label = V(GCFC19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
GCFC19_TDM.clusterCoef <- transitivity(GCFC19_TDMTable, type="global") #cluster coefficient
GCFC19_TDM.degreeCent <- centralization.degree(GCFC19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_TDMftn <- as.network.matrix(GCFC19_TDMft)
GCFC19_TDM.netDensity <- network.density(GCFC19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_TDM.entropy <- entropy(GCFC19_TDMft) #entropy

GCFC19_TDM.netMx <- cbind(GCFC19_TDM.netMx, GCFC19_TDM.clusterCoef, GCFC19_TDM.degreeCent$centralization,
                          GCFC19_TDM.netDensity, GCFC19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
GCFC19_SDg2 <- data.frame(GCFC19_SD)
GCFC19_SDg2 <- GCFC19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_SDg2$player1
player2vector <- GCFC19_SDg2$player2
GCFC19_SDg3 <- GCFC19_SDg2
GCFC19_SDg3$p1inp2vec <- is.element(GCFC19_SDg3$player1, player2vector)
GCFC19_SDg3$p2inp1vec <- is.element(GCFC19_SDg3$player2, player1vector)

addPlayer1 <- GCFC19_SDg3[ which(GCFC19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_SDg3[ which(GCFC19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_SDg2 <- rbind(GCFC19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
GCFC19_SDft <- ftable(GCFC19_SDg2$player1, GCFC19_SDg2$player2)
GCFC19_SDft2 <- as.matrix(GCFC19_SDft)
numRows <- nrow(GCFC19_SDft2)
numCols <- ncol(GCFC19_SDft2)
GCFC19_SDft3 <- GCFC19_SDft2[c(2:numRows) , c(2:numCols)]
GCFC19_SDTable <- graph.adjacency(GCFC19_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(GCFC19_SDTable, vertex.label = V(GCFC19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
GCFC19_SD.clusterCoef <- transitivity(GCFC19_SDTable, type="global") #cluster coefficient
GCFC19_SD.degreeCent <- centralization.degree(GCFC19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_SDftn <- as.network.matrix(GCFC19_SDft)
GCFC19_SD.netDensity <- network.density(GCFC19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_SD.entropy <- entropy(GCFC19_SDft) #entropy

GCFC19_SD.netMx <- cbind(GCFC19_SD.netMx, GCFC19_SD.clusterCoef, GCFC19_SD.degreeCent$centralization,
                         GCFC19_SD.netDensity, GCFC19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
GCFC19_TDg2 <- data.frame(GCFC19_TD)
GCFC19_TDg2 <- GCFC19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_TDg2$player1
player2vector <- GCFC19_TDg2$player2
GCFC19_TDg3 <- GCFC19_TDg2
GCFC19_TDg3$p1inp2vec <- is.element(GCFC19_TDg3$player1, player2vector)
GCFC19_TDg3$p2inp1vec <- is.element(GCFC19_TDg3$player2, player1vector)

addPlayer1 <- GCFC19_TDg3[ which(GCFC19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_TDg3[ which(GCFC19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_TDg2 <- rbind(GCFC19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
GCFC19_TDft <- ftable(GCFC19_TDg2$player1, GCFC19_TDg2$player2)
GCFC19_TDft2 <- as.matrix(GCFC19_TDft)
numRows <- nrow(GCFC19_TDft2)
numCols <- ncol(GCFC19_TDft2)
GCFC19_TDft3 <- GCFC19_TDft2[c(2:numRows) , c(2:numCols)]
GCFC19_TDTable <- graph.adjacency(GCFC19_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(GCFC19_TDTable, vertex.label = V(GCFC19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
GCFC19_TD.clusterCoef <- transitivity(GCFC19_TDTable, type="global") #cluster coefficient
GCFC19_TD.degreeCent <- centralization.degree(GCFC19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_TDftn <- as.network.matrix(GCFC19_TDft)
GCFC19_TD.netDensity <- network.density(GCFC19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_TD.entropy <- entropy(GCFC19_TDft) #entropy

GCFC19_TD.netMx <- cbind(GCFC19_TD.netMx, GCFC19_TD.clusterCoef, GCFC19_TD.degreeCent$centralization,
                         GCFC19_TD.netDensity, GCFC19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
GCFC19_QTg2 <- data.frame(GCFC19_QT)
GCFC19_QTg2 <- GCFC19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC19_QTg2$player1
player2vector <- GCFC19_QTg2$player2
GCFC19_QTg3 <- GCFC19_QTg2
GCFC19_QTg3$p1inp2vec <- is.element(GCFC19_QTg3$player1, player2vector)
GCFC19_QTg3$p2inp1vec <- is.element(GCFC19_QTg3$player2, player1vector)

addPlayer1 <- GCFC19_QTg3[ which(GCFC19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC19_QTg3[ which(GCFC19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC19_QTg2 <- rbind(GCFC19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
GCFC19_QTft <- ftable(GCFC19_QTg2$player1, GCFC19_QTg2$player2)
GCFC19_QTft2 <- as.matrix(GCFC19_QTft)
numRows <- nrow(GCFC19_QTft2)
numCols <- ncol(GCFC19_QTft2)
GCFC19_QTft3 <- GCFC19_QTft2[c(2:numRows) , c(2:numCols)]
GCFC19_QTTable <- graph.adjacency(GCFC19_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(GCFC19_QTTable, vertex.label = V(GCFC19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
GCFC19_QT.clusterCoef <- transitivity(GCFC19_QTTable, type="global") #cluster coefficient
GCFC19_QT.degreeCent <- centralization.degree(GCFC19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC19_QTftn <- as.network.matrix(GCFC19_QTft)
GCFC19_QT.netDensity <- network.density(GCFC19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC19_QT.entropy <- entropy(GCFC19_QTft) #entropy

GCFC19_QT.netMx <- cbind(GCFC19_QT.netMx, GCFC19_QT.clusterCoef, GCFC19_QT.degreeCent$centralization,
                         GCFC19_QT.netDensity, GCFC19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC19_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
GEEL19_Gg2 <- data.frame(GEEL19_G)
GEEL19_Gg2 <- GEEL19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_Gg2$player1
player2vector <- GEEL19_Gg2$player2
GEEL19_Gg3 <- GEEL19_Gg2
GEEL19_Gg3$p1inp2vec <- is.element(GEEL19_Gg3$player1, player2vector)
GEEL19_Gg3$p2inp1vec <- is.element(GEEL19_Gg3$player2, player1vector)

addPlayer1 <- GEEL19_Gg3[ which(GEEL19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_Gg3[ which(GEEL19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_Gg2 <- rbind(GEEL19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
GEEL19_Gft <- ftable(GEEL19_Gg2$player1, GEEL19_Gg2$player2)
GEEL19_Gft2 <- as.matrix(GEEL19_Gft)
numRows <- nrow(GEEL19_Gft2)
numCols <- ncol(GEEL19_Gft2)
GEEL19_Gft3 <- GEEL19_Gft2[c(2:numRows) , c(2:numCols)]
GEEL19_GTable <- graph.adjacency(GEEL19_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(GEEL19_GTable, vertex.label = V(GEEL19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
GEEL19_G.clusterCoef <- transitivity(GEEL19_GTable, type="global") #cluster coefficient
GEEL19_G.degreeCent <- centralization.degree(GEEL19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_Gftn <- as.network.matrix(GEEL19_Gft)
GEEL19_G.netDensity <- network.density(GEEL19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_G.entropy <- entropy(GEEL19_Gft) #entropy

GEEL19_G.netMx <- cbind(GEEL19_G.netMx, GEEL19_G.clusterCoef, GEEL19_G.degreeCent$centralization,
                        GEEL19_G.netDensity, GEEL19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
GEEL19_Bg2 <- data.frame(GEEL19_B)
GEEL19_Bg2 <- GEEL19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_Bg2$player1
player2vector <- GEEL19_Bg2$player2
GEEL19_Bg3 <- GEEL19_Bg2
GEEL19_Bg3$p1inp2vec <- is.element(GEEL19_Bg3$player1, player2vector)
GEEL19_Bg3$p2inp1vec <- is.element(GEEL19_Bg3$player2, player1vector)

addPlayer1 <- GEEL19_Bg3[ which(GEEL19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_Bg3[ which(GEEL19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_Bg2 <- rbind(GEEL19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
GEEL19_Bft <- ftable(GEEL19_Bg2$player1, GEEL19_Bg2$player2)
GEEL19_Bft2 <- as.matrix(GEEL19_Bft)
numRows <- nrow(GEEL19_Bft2)
numCols <- ncol(GEEL19_Bft2)
GEEL19_Bft3 <- GEEL19_Bft2[c(2:numRows) , c(2:numCols)]
GEEL19_BTable <- graph.adjacency(GEEL19_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(GEEL19_BTable, vertex.label = V(GEEL19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
GEEL19_B.clusterCoef <- transitivity(GEEL19_BTable, type="global") #cluster coefficient
GEEL19_B.degreeCent <- centralization.degree(GEEL19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_Bftn <- as.network.matrix(GEEL19_Bft)
GEEL19_B.netDensity <- network.density(GEEL19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_B.entropy <- entropy(GEEL19_Bft) #entropy

GEEL19_B.netMx <- cbind(GEEL19_B.netMx, GEEL19_B.clusterCoef, GEEL19_B.degreeCent$centralization,
                        GEEL19_B.netDensity, GEEL19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************

round = 19
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
GEEL19_SFg2 <- data.frame(GEEL19_SF)
GEEL19_SFg2 <- GEEL19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_SFg2$player1
player2vector <- GEEL19_SFg2$player2
GEEL19_SFg3 <- GEEL19_SFg2
GEEL19_SFg3$p1inp2vec <- is.element(GEEL19_SFg3$player1, player2vector)
GEEL19_SFg3$p2inp1vec <- is.element(GEEL19_SFg3$player2, player1vector)

addPlayer1 <- GEEL19_SFg3[ which(GEEL19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_SFg3[ which(GEEL19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_SFg2 <- rbind(GEEL19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
GEEL19_SFft <- ftable(GEEL19_SFg2$player1, GEEL19_SFg2$player2)
GEEL19_SFft2 <- as.matrix(GEEL19_SFft)
numRows <- nrow(GEEL19_SFft2)
numCols <- ncol(GEEL19_SFft2)
GEEL19_SFft3 <- GEEL19_SFft2[c(2:numRows) , c(2:numCols)]
GEEL19_SFTable <- graph.adjacency(GEEL19_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(GEEL19_SFTable, vertex.label = V(GEEL19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
GEEL19_SF.clusterCoef <- transitivity(GEEL19_SFTable, type="global") #cluster coefficient
GEEL19_SF.degreeCent <- centralization.degree(GEEL19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_SFftn <- as.network.matrix(GEEL19_SFft)
GEEL19_SF.netDensity <- network.density(GEEL19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_SF.entropy <- entropy(GEEL19_SFft) #entropy

GEEL19_SF.netMx <- cbind(GEEL19_SF.netMx, GEEL19_SF.clusterCoef, GEEL19_SF.degreeCent$centralization,
                         GEEL19_SF.netDensity, GEEL19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
GEEL19_TFg2 <- data.frame(GEEL19_TF)
GEEL19_TFg2 <- GEEL19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_TFg2$player1
player2vector <- GEEL19_TFg2$player2
GEEL19_TFg3 <- GEEL19_TFg2
GEEL19_TFg3$p1inp2vec <- is.element(GEEL19_TFg3$player1, player2vector)
GEEL19_TFg3$p2inp1vec <- is.element(GEEL19_TFg3$player2, player1vector)

addPlayer1 <- GEEL19_TFg3[ which(GEEL19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_TFg3[ which(GEEL19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_TFg2 <- rbind(GEEL19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
GEEL19_TFft <- ftable(GEEL19_TFg2$player1, GEEL19_TFg2$player2)
GEEL19_TFft2 <- as.matrix(GEEL19_TFft)
numRows <- nrow(GEEL19_TFft2)
numCols <- ncol(GEEL19_TFft2)
GEEL19_TFft3 <- GEEL19_TFft2[c(2:numRows) , c(2:numCols)]
GEEL19_TFTable <- graph.adjacency(GEEL19_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(GEEL19_TFTable, vertex.label = V(GEEL19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
GEEL19_TF.clusterCoef <- transitivity(GEEL19_TFTable, type="global") #cluster coefficient
GEEL19_TF.degreeCent <- centralization.degree(GEEL19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_TFftn <- as.network.matrix(GEEL19_TFft)
GEEL19_TF.netDensity <- network.density(GEEL19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_TF.entropy <- entropy(GEEL19_TFft) #entropy

GEEL19_TF.netMx <- cbind(GEEL19_TF.netMx, GEEL19_TF.clusterCoef, GEEL19_TF.degreeCent$centralization,
                         GEEL19_TF.netDensity, GEEL19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
GEEL19_SAMg2 <- data.frame(GEEL19_SAM)
GEEL19_SAMg2 <- GEEL19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_SAMg2$player1
player2vector <- GEEL19_SAMg2$player2
GEEL19_SAMg3 <- GEEL19_SAMg2
GEEL19_SAMg3$p1inp2vec <- is.element(GEEL19_SAMg3$player1, player2vector)
GEEL19_SAMg3$p2inp1vec <- is.element(GEEL19_SAMg3$player2, player1vector)

addPlayer1 <- GEEL19_SAMg3[ which(GEEL19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_SAMg3[ which(GEEL19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_SAMg2 <- rbind(GEEL19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
GEEL19_SAMft <- ftable(GEEL19_SAMg2$player1, GEEL19_SAMg2$player2)
GEEL19_SAMft2 <- as.matrix(GEEL19_SAMft)
numRows <- nrow(GEEL19_SAMft2)
numCols <- ncol(GEEL19_SAMft2)
GEEL19_SAMft3 <- GEEL19_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL19_SAMTable <- graph.adjacency(GEEL19_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(GEEL19_SAMTable, vertex.label = V(GEEL19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
GEEL19_SAM.clusterCoef <- transitivity(GEEL19_SAMTable, type="global") #cluster coefficient
GEEL19_SAM.degreeCent <- centralization.degree(GEEL19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_SAMftn <- as.network.matrix(GEEL19_SAMft)
GEEL19_SAM.netDensity <- network.density(GEEL19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_SAM.entropy <- entropy(GEEL19_SAMft) #entropy

GEEL19_SAM.netMx <- cbind(GEEL19_SAM.netMx, GEEL19_SAM.clusterCoef, GEEL19_SAM.degreeCent$centralization,
                          GEEL19_SAM.netDensity, GEEL19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
GEEL19_TAMg2 <- data.frame(GEEL19_TAM)
GEEL19_TAMg2 <- GEEL19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_TAMg2$player1
player2vector <- GEEL19_TAMg2$player2
GEEL19_TAMg3 <- GEEL19_TAMg2
GEEL19_TAMg3$p1inp2vec <- is.element(GEEL19_TAMg3$player1, player2vector)
GEEL19_TAMg3$p2inp1vec <- is.element(GEEL19_TAMg3$player2, player1vector)

addPlayer1 <- GEEL19_TAMg3[ which(GEEL19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_TAMg3[ which(GEEL19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_TAMg2 <- rbind(GEEL19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
GEEL19_TAMft <- ftable(GEEL19_TAMg2$player1, GEEL19_TAMg2$player2)
GEEL19_TAMft2 <- as.matrix(GEEL19_TAMft)
numRows <- nrow(GEEL19_TAMft2)
numCols <- ncol(GEEL19_TAMft2)
GEEL19_TAMft3 <- GEEL19_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL19_TAMTable <- graph.adjacency(GEEL19_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(GEEL19_TAMTable, vertex.label = V(GEEL19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
GEEL19_TAM.clusterCoef <- transitivity(GEEL19_TAMTable, type="global") #cluster coefficient
GEEL19_TAM.degreeCent <- centralization.degree(GEEL19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_TAMftn <- as.network.matrix(GEEL19_TAMft)
GEEL19_TAM.netDensity <- network.density(GEEL19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_TAM.entropy <- entropy(GEEL19_TAMft) #entropy

GEEL19_TAM.netMx <- cbind(GEEL19_TAM.netMx, GEEL19_TAM.clusterCoef, GEEL19_TAM.degreeCent$centralization,
                          GEEL19_TAM.netDensity, GEEL19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************
#NA

round = 19
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
GEEL19_SDMg2 <- data.frame(GEEL19_SDM)
GEEL19_SDMg2 <- GEEL19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_SDMg2$player1
player2vector <- GEEL19_SDMg2$player2
GEEL19_SDMg3 <- GEEL19_SDMg2
GEEL19_SDMg3$p1inp2vec <- is.element(GEEL19_SDMg3$player1, player2vector)
GEEL19_SDMg3$p2inp1vec <- is.element(GEEL19_SDMg3$player2, player1vector)

addPlayer1 <- GEEL19_SDMg3[ which(GEEL19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_SDMg3[ which(GEEL19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_SDMg2 <- rbind(GEEL19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
GEEL19_SDMft <- ftable(GEEL19_SDMg2$player1, GEEL19_SDMg2$player2)
GEEL19_SDMft2 <- as.matrix(GEEL19_SDMft)
numRows <- nrow(GEEL19_SDMft2)
numCols <- ncol(GEEL19_SDMft2)
GEEL19_SDMft3 <- GEEL19_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL19_SDMTable <- graph.adjacency(GEEL19_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(GEEL19_SDMTable, vertex.label = V(GEEL19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
GEEL19_SDM.clusterCoef <- transitivity(GEEL19_SDMTable, type="global") #cluster coefficient
GEEL19_SDM.degreeCent <- centralization.degree(GEEL19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_SDMftn <- as.network.matrix(GEEL19_SDMft)
GEEL19_SDM.netDensity <- network.density(GEEL19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_SDM.entropy <- entropy(GEEL19_SDMft) #entropy

GEEL19_SDM.netMx <- cbind(GEEL19_SDM.netMx, GEEL19_SDM.clusterCoef, GEEL19_SDM.degreeCent$centralization,
                          GEEL19_SDM.netDensity, GEEL19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
GEEL19_TDMg2 <- data.frame(GEEL19_TDM)
GEEL19_TDMg2 <- GEEL19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_TDMg2$player1
player2vector <- GEEL19_TDMg2$player2
GEEL19_TDMg3 <- GEEL19_TDMg2
GEEL19_TDMg3$p1inp2vec <- is.element(GEEL19_TDMg3$player1, player2vector)
GEEL19_TDMg3$p2inp1vec <- is.element(GEEL19_TDMg3$player2, player1vector)

addPlayer1 <- GEEL19_TDMg3[ which(GEEL19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL19_TDMg3[ which(GEEL19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_TDMg2 <- rbind(GEEL19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
GEEL19_TDMft <- ftable(GEEL19_TDMg2$player1, GEEL19_TDMg2$player2)
GEEL19_TDMft2 <- as.matrix(GEEL19_TDMft)
numRows <- nrow(GEEL19_TDMft2)
numCols <- ncol(GEEL19_TDMft2)
GEEL19_TDMft3 <- GEEL19_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL19_TDMTable <- graph.adjacency(GEEL19_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(GEEL19_TDMTable, vertex.label = V(GEEL19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
GEEL19_TDM.clusterCoef <- transitivity(GEEL19_TDMTable, type="global") #cluster coefficient
GEEL19_TDM.degreeCent <- centralization.degree(GEEL19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_TDMftn <- as.network.matrix(GEEL19_TDMft)
GEEL19_TDM.netDensity <- network.density(GEEL19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_TDM.entropy <- entropy(GEEL19_TDMft) #entropy

GEEL19_TDM.netMx <- cbind(GEEL19_TDM.netMx, GEEL19_TDM.clusterCoef, GEEL19_TDM.degreeCent$centralization,
                          GEEL19_TDM.netDensity, GEEL19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
GEEL19_SDg2 <- data.frame(GEEL19_SD)
GEEL19_SDg2 <- GEEL19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_SDg2$player1
player2vector <- GEEL19_SDg2$player2
GEEL19_SDg3 <- GEEL19_SDg2
GEEL19_SDg3$p1inp2vec <- is.element(GEEL19_SDg3$player1, player2vector)
GEEL19_SDg3$p2inp1vec <- is.element(GEEL19_SDg3$player2, player1vector)

addPlayer1 <- GEEL19_SDg3[ which(GEEL19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_SDg3[ which(GEEL19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_SDg2 <- rbind(GEEL19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
GEEL19_SDft <- ftable(GEEL19_SDg2$player1, GEEL19_SDg2$player2)
GEEL19_SDft2 <- as.matrix(GEEL19_SDft)
numRows <- nrow(GEEL19_SDft2)
numCols <- ncol(GEEL19_SDft2)
GEEL19_SDft3 <- GEEL19_SDft2[c(2:numRows) , c(2:numCols)]
GEEL19_SDTable <- graph.adjacency(GEEL19_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(GEEL19_SDTable, vertex.label = V(GEEL19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
GEEL19_SD.clusterCoef <- transitivity(GEEL19_SDTable, type="global") #cluster coefficient
GEEL19_SD.degreeCent <- centralization.degree(GEEL19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_SDftn <- as.network.matrix(GEEL19_SDft)
GEEL19_SD.netDensity <- network.density(GEEL19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_SD.entropy <- entropy(GEEL19_SDft) #entropy

GEEL19_SD.netMx <- cbind(GEEL19_SD.netMx, GEEL19_SD.clusterCoef, GEEL19_SD.degreeCent$centralization,
                         GEEL19_SD.netDensity, GEEL19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
GEEL19_TDg2 <- data.frame(GEEL19_TD)
GEEL19_TDg2 <- GEEL19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_TDg2$player1
player2vector <- GEEL19_TDg2$player2
GEEL19_TDg3 <- GEEL19_TDg2
GEEL19_TDg3$p1inp2vec <- is.element(GEEL19_TDg3$player1, player2vector)
GEEL19_TDg3$p2inp1vec <- is.element(GEEL19_TDg3$player2, player1vector)

addPlayer1 <- GEEL19_TDg3[ which(GEEL19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_TDg3[ which(GEEL19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_TDg2 <- rbind(GEEL19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
GEEL19_TDft <- ftable(GEEL19_TDg2$player1, GEEL19_TDg2$player2)
GEEL19_TDft2 <- as.matrix(GEEL19_TDft)
numRows <- nrow(GEEL19_TDft2)
numCols <- ncol(GEEL19_TDft2)
GEEL19_TDft3 <- GEEL19_TDft2[c(2:numRows) , c(2:numCols)]
GEEL19_TDTable <- graph.adjacency(GEEL19_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(GEEL19_TDTable, vertex.label = V(GEEL19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
GEEL19_TD.clusterCoef <- transitivity(GEEL19_TDTable, type="global") #cluster coefficient
GEEL19_TD.degreeCent <- centralization.degree(GEEL19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_TDftn <- as.network.matrix(GEEL19_TDft)
GEEL19_TD.netDensity <- network.density(GEEL19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_TD.entropy <- entropy(GEEL19_TDft) #entropy

GEEL19_TD.netMx <- cbind(GEEL19_TD.netMx, GEEL19_TD.clusterCoef, GEEL19_TD.degreeCent$centralization,
                         GEEL19_TD.netDensity, GEEL19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
GEEL19_QTg2 <- data.frame(GEEL19_QT)
GEEL19_QTg2 <- GEEL19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL19_QTg2$player1
player2vector <- GEEL19_QTg2$player2
GEEL19_QTg3 <- GEEL19_QTg2
GEEL19_QTg3$p1inp2vec <- is.element(GEEL19_QTg3$player1, player2vector)
GEEL19_QTg3$p2inp1vec <- is.element(GEEL19_QTg3$player2, player1vector)

addPlayer1 <- GEEL19_QTg3[ which(GEEL19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL19_QTg3[ which(GEEL19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL19_QTg2 <- rbind(GEEL19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
GEEL19_QTft <- ftable(GEEL19_QTg2$player1, GEEL19_QTg2$player2)
GEEL19_QTft2 <- as.matrix(GEEL19_QTft)
numRows <- nrow(GEEL19_QTft2)
numCols <- ncol(GEEL19_QTft2)
GEEL19_QTft3 <- GEEL19_QTft2[c(2:numRows) , c(2:numCols)]
GEEL19_QTTable <- graph.adjacency(GEEL19_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(GEEL19_QTTable, vertex.label = V(GEEL19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
GEEL19_QT.clusterCoef <- transitivity(GEEL19_QTTable, type="global") #cluster coefficient
GEEL19_QT.degreeCent <- centralization.degree(GEEL19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL19_QTftn <- as.network.matrix(GEEL19_QTft)
GEEL19_QT.netDensity <- network.density(GEEL19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL19_QT.entropy <- entropy(GEEL19_QTft) #entropy

GEEL19_QT.netMx <- cbind(GEEL19_QT.netMx, GEEL19_QT.clusterCoef, GEEL19_QT.degreeCent$centralization,
                         GEEL19_QT.netDensity, GEEL19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL19_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************

round = 19
teamName = "GWS"
KIoutcome = "Goal_F"
GWS19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
GWS19_Gg2 <- data.frame(GWS19_G)
GWS19_Gg2 <- GWS19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_Gg2$player1
player2vector <- GWS19_Gg2$player2
GWS19_Gg3 <- GWS19_Gg2
GWS19_Gg3$p1inp2vec <- is.element(GWS19_Gg3$player1, player2vector)
GWS19_Gg3$p2inp1vec <- is.element(GWS19_Gg3$player2, player1vector)

addPlayer1 <- GWS19_Gg3[ which(GWS19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_Gg3[ which(GWS19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_Gg2 <- rbind(GWS19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
GWS19_Gft <- ftable(GWS19_Gg2$player1, GWS19_Gg2$player2)
GWS19_Gft2 <- as.matrix(GWS19_Gft)
numRows <- nrow(GWS19_Gft2)
numCols <- ncol(GWS19_Gft2)
GWS19_Gft3 <- GWS19_Gft2[c(2:numRows) , c(2:numCols)]
GWS19_GTable <- graph.adjacency(GWS19_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(GWS19_GTable, vertex.label = V(GWS19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
GWS19_G.clusterCoef <- transitivity(GWS19_GTable, type="global") #cluster coefficient
GWS19_G.degreeCent <- centralization.degree(GWS19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_Gftn <- as.network.matrix(GWS19_Gft)
GWS19_G.netDensity <- network.density(GWS19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_G.entropy <- entropy(GWS19_Gft) #entropy

GWS19_G.netMx <- cbind(GWS19_G.netMx, GWS19_G.clusterCoef, GWS19_G.degreeCent$centralization,
                       GWS19_G.netDensity, GWS19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "GWS"
KIoutcome = "Behind_F"
GWS19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
GWS19_Bg2 <- data.frame(GWS19_B)
GWS19_Bg2 <- GWS19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_Bg2$player1
player2vector <- GWS19_Bg2$player2
GWS19_Bg3 <- GWS19_Bg2
GWS19_Bg3$p1inp2vec <- is.element(GWS19_Bg3$player1, player2vector)
GWS19_Bg3$p2inp1vec <- is.element(GWS19_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS19_Bg3[ which(GWS19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS19_Bg2 <- rbind(GWS19_Bg2, addPlayer2)

#ROUND 19, Behind graph using weighted edges
GWS19_Bft <- ftable(GWS19_Bg2$player1, GWS19_Bg2$player2)
GWS19_Bft2 <- as.matrix(GWS19_Bft)
numRows <- nrow(GWS19_Bft2)
numCols <- ncol(GWS19_Bft2)
GWS19_Bft3 <- GWS19_Bft2[c(1:numRows) , c(2:numCols)]
GWS19_BTable <- graph.adjacency(GWS19_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(GWS19_BTable, vertex.label = V(GWS19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
GWS19_B.clusterCoef <- transitivity(GWS19_BTable, type="global") #cluster coefficient
GWS19_B.degreeCent <- centralization.degree(GWS19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_Bftn <- as.network.matrix(GWS19_Bft)
GWS19_B.netDensity <- network.density(GWS19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_B.entropy <- entropy(GWS19_Bft) #entropy

GWS19_B.netMx <- cbind(GWS19_B.netMx, GWS19_B.clusterCoef, GWS19_B.degreeCent$centralization,
                       GWS19_B.netDensity, GWS19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
GWS19_SFg2 <- data.frame(GWS19_SF)
GWS19_SFg2 <- GWS19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_SFg2$player1
player2vector <- GWS19_SFg2$player2
GWS19_SFg3 <- GWS19_SFg2
GWS19_SFg3$p1inp2vec <- is.element(GWS19_SFg3$player1, player2vector)
GWS19_SFg3$p2inp1vec <- is.element(GWS19_SFg3$player2, player1vector)

addPlayer1 <- GWS19_SFg3[ which(GWS19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_SFg3[ which(GWS19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_SFg2 <- rbind(GWS19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
GWS19_SFft <- ftable(GWS19_SFg2$player1, GWS19_SFg2$player2)
GWS19_SFft2 <- as.matrix(GWS19_SFft)
numRows <- nrow(GWS19_SFft2)
numCols <- ncol(GWS19_SFft2)
GWS19_SFft3 <- GWS19_SFft2[c(2:numRows) , c(2:numCols)]
GWS19_SFTable <- graph.adjacency(GWS19_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(GWS19_SFTable, vertex.label = V(GWS19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
GWS19_SF.clusterCoef <- transitivity(GWS19_SFTable, type="global") #cluster coefficient
GWS19_SF.degreeCent <- centralization.degree(GWS19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_SFftn <- as.network.matrix(GWS19_SFft)
GWS19_SF.netDensity <- network.density(GWS19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_SF.entropy <- entropy(GWS19_SFft) #entropy

GWS19_SF.netMx <- cbind(GWS19_SF.netMx, GWS19_SF.clusterCoef, GWS19_SF.degreeCent$centralization,
                        GWS19_SF.netDensity, GWS19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
GWS19_TFg2 <- data.frame(GWS19_TF)
GWS19_TFg2 <- GWS19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_TFg2$player1
player2vector <- GWS19_TFg2$player2
GWS19_TFg3 <- GWS19_TFg2
GWS19_TFg3$p1inp2vec <- is.element(GWS19_TFg3$player1, player2vector)
GWS19_TFg3$p2inp1vec <- is.element(GWS19_TFg3$player2, player1vector)

addPlayer1 <- GWS19_TFg3[ which(GWS19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_TFg3[ which(GWS19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_TFg2 <- rbind(GWS19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
GWS19_TFft <- ftable(GWS19_TFg2$player1, GWS19_TFg2$player2)
GWS19_TFft2 <- as.matrix(GWS19_TFft)
numRows <- nrow(GWS19_TFft2)
numCols <- ncol(GWS19_TFft2)
GWS19_TFft3 <- GWS19_TFft2[c(2:numRows) , c(2:numCols)]
GWS19_TFTable <- graph.adjacency(GWS19_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(GWS19_TFTable, vertex.label = V(GWS19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
GWS19_TF.clusterCoef <- transitivity(GWS19_TFTable, type="global") #cluster coefficient
GWS19_TF.degreeCent <- centralization.degree(GWS19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_TFftn <- as.network.matrix(GWS19_TFft)
GWS19_TF.netDensity <- network.density(GWS19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_TF.entropy <- entropy(GWS19_TFft) #entropy

GWS19_TF.netMx <- cbind(GWS19_TF.netMx, GWS19_TF.clusterCoef, GWS19_TF.degreeCent$centralization,
                        GWS19_TF.netDensity, GWS19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
GWS19_SAMg2 <- data.frame(GWS19_SAM)
GWS19_SAMg2 <- GWS19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_SAMg2$player1
player2vector <- GWS19_SAMg2$player2
GWS19_SAMg3 <- GWS19_SAMg2
GWS19_SAMg3$p1inp2vec <- is.element(GWS19_SAMg3$player1, player2vector)
GWS19_SAMg3$p2inp1vec <- is.element(GWS19_SAMg3$player2, player1vector)

addPlayer1 <- GWS19_SAMg3[ which(GWS19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_SAMg3[ which(GWS19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_SAMg2 <- rbind(GWS19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
GWS19_SAMft <- ftable(GWS19_SAMg2$player1, GWS19_SAMg2$player2)
GWS19_SAMft2 <- as.matrix(GWS19_SAMft)
numRows <- nrow(GWS19_SAMft2)
numCols <- ncol(GWS19_SAMft2)
GWS19_SAMft3 <- GWS19_SAMft2[c(2:numRows) , c(2:numCols)]
GWS19_SAMTable <- graph.adjacency(GWS19_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(GWS19_SAMTable, vertex.label = V(GWS19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
GWS19_SAM.clusterCoef <- transitivity(GWS19_SAMTable, type="global") #cluster coefficient
GWS19_SAM.degreeCent <- centralization.degree(GWS19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_SAMftn <- as.network.matrix(GWS19_SAMft)
GWS19_SAM.netDensity <- network.density(GWS19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_SAM.entropy <- entropy(GWS19_SAMft) #entropy

GWS19_SAM.netMx <- cbind(GWS19_SAM.netMx, GWS19_SAM.clusterCoef, GWS19_SAM.degreeCent$centralization,
                         GWS19_SAM.netDensity, GWS19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
GWS19_TAMg2 <- data.frame(GWS19_TAM)
GWS19_TAMg2 <- GWS19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_TAMg2$player1
player2vector <- GWS19_TAMg2$player2
GWS19_TAMg3 <- GWS19_TAMg2
GWS19_TAMg3$p1inp2vec <- is.element(GWS19_TAMg3$player1, player2vector)
GWS19_TAMg3$p2inp1vec <- is.element(GWS19_TAMg3$player2, player1vector)

addPlayer1 <- GWS19_TAMg3[ which(GWS19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_TAMg3[ which(GWS19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_TAMg2 <- rbind(GWS19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
GWS19_TAMft <- ftable(GWS19_TAMg2$player1, GWS19_TAMg2$player2)
GWS19_TAMft2 <- as.matrix(GWS19_TAMft)
numRows <- nrow(GWS19_TAMft2)
numCols <- ncol(GWS19_TAMft2)
GWS19_TAMft3 <- GWS19_TAMft2[c(2:numRows) , c(2:numCols)]
GWS19_TAMTable <- graph.adjacency(GWS19_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(GWS19_TAMTable, vertex.label = V(GWS19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
GWS19_TAM.clusterCoef <- transitivity(GWS19_TAMTable, type="global") #cluster coefficient
GWS19_TAM.degreeCent <- centralization.degree(GWS19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_TAMftn <- as.network.matrix(GWS19_TAMft)
GWS19_TAM.netDensity <- network.density(GWS19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_TAM.entropy <- entropy(GWS19_TAMft) #entropy

GWS19_TAM.netMx <- cbind(GWS19_TAM.netMx, GWS19_TAM.clusterCoef, GWS19_TAM.degreeCent$centralization,
                         GWS19_TAM.netDensity, GWS19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************

round = 19
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
GWS19_SDMg2 <- data.frame(GWS19_SDM)
GWS19_SDMg2 <- GWS19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_SDMg2$player1
player2vector <- GWS19_SDMg2$player2
GWS19_SDMg3 <- GWS19_SDMg2
GWS19_SDMg3$p1inp2vec <- is.element(GWS19_SDMg3$player1, player2vector)
GWS19_SDMg3$p2inp1vec <- is.element(GWS19_SDMg3$player2, player1vector)

addPlayer1 <- GWS19_SDMg3[ which(GWS19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_SDMg3[ which(GWS19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_SDMg2 <- rbind(GWS19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
GWS19_SDMft <- ftable(GWS19_SDMg2$player1, GWS19_SDMg2$player2)
GWS19_SDMft2 <- as.matrix(GWS19_SDMft)
numRows <- nrow(GWS19_SDMft2)
numCols <- ncol(GWS19_SDMft2)
GWS19_SDMft3 <- GWS19_SDMft2[c(2:numRows) , c(2:numCols)]
GWS19_SDMTable <- graph.adjacency(GWS19_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(GWS19_SDMTable, vertex.label = V(GWS19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
GWS19_SDM.clusterCoef <- transitivity(GWS19_SDMTable, type="global") #cluster coefficient
GWS19_SDM.degreeCent <- centralization.degree(GWS19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_SDMftn <- as.network.matrix(GWS19_SDMft)
GWS19_SDM.netDensity <- network.density(GWS19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_SDM.entropy <- entropy(GWS19_SDMft) #entropy

GWS19_SDM.netMx <- cbind(GWS19_SDM.netMx, GWS19_SDM.clusterCoef, GWS19_SDM.degreeCent$centralization,
                         GWS19_SDM.netDensity, GWS19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
GWS19_TDMg2 <- data.frame(GWS19_TDM)
GWS19_TDMg2 <- GWS19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_TDMg2$player1
player2vector <- GWS19_TDMg2$player2
GWS19_TDMg3 <- GWS19_TDMg2
GWS19_TDMg3$p1inp2vec <- is.element(GWS19_TDMg3$player1, player2vector)
GWS19_TDMg3$p2inp1vec <- is.element(GWS19_TDMg3$player2, player1vector)

addPlayer1 <- GWS19_TDMg3[ which(GWS19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_TDMg3[ which(GWS19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_TDMg2 <- rbind(GWS19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
GWS19_TDMft <- ftable(GWS19_TDMg2$player1, GWS19_TDMg2$player2)
GWS19_TDMft2 <- as.matrix(GWS19_TDMft)
numRows <- nrow(GWS19_TDMft2)
numCols <- ncol(GWS19_TDMft2)
GWS19_TDMft3 <- GWS19_TDMft2[c(2:numRows) , c(2:numCols)]
GWS19_TDMTable <- graph.adjacency(GWS19_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(GWS19_TDMTable, vertex.label = V(GWS19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
GWS19_TDM.clusterCoef <- transitivity(GWS19_TDMTable, type="global") #cluster coefficient
GWS19_TDM.degreeCent <- centralization.degree(GWS19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_TDMftn <- as.network.matrix(GWS19_TDMft)
GWS19_TDM.netDensity <- network.density(GWS19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_TDM.entropy <- entropy(GWS19_TDMft) #entropy

GWS19_TDM.netMx <- cbind(GWS19_TDM.netMx, GWS19_TDM.clusterCoef, GWS19_TDM.degreeCent$centralization,
                         GWS19_TDM.netDensity, GWS19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
GWS19_SDg2 <- data.frame(GWS19_SD)
GWS19_SDg2 <- GWS19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_SDg2$player1
player2vector <- GWS19_SDg2$player2
GWS19_SDg3 <- GWS19_SDg2
GWS19_SDg3$p1inp2vec <- is.element(GWS19_SDg3$player1, player2vector)
GWS19_SDg3$p2inp1vec <- is.element(GWS19_SDg3$player2, player1vector)

addPlayer1 <- GWS19_SDg3[ which(GWS19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_SDg3[ which(GWS19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_SDg2 <- rbind(GWS19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
GWS19_SDft <- ftable(GWS19_SDg2$player1, GWS19_SDg2$player2)
GWS19_SDft2 <- as.matrix(GWS19_SDft)
numRows <- nrow(GWS19_SDft2)
numCols <- ncol(GWS19_SDft2)
GWS19_SDft3 <- GWS19_SDft2[c(2:numRows) , c(2:numCols)]
GWS19_SDTable <- graph.adjacency(GWS19_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(GWS19_SDTable, vertex.label = V(GWS19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
GWS19_SD.clusterCoef <- transitivity(GWS19_SDTable, type="global") #cluster coefficient
GWS19_SD.degreeCent <- centralization.degree(GWS19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_SDftn <- as.network.matrix(GWS19_SDft)
GWS19_SD.netDensity <- network.density(GWS19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_SD.entropy <- entropy(GWS19_SDft) #entropy

GWS19_SD.netMx <- cbind(GWS19_SD.netMx, GWS19_SD.clusterCoef, GWS19_SD.degreeCent$centralization,
                        GWS19_SD.netDensity, GWS19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
GWS19_TDg2 <- data.frame(GWS19_TD)
GWS19_TDg2 <- GWS19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_TDg2$player1
player2vector <- GWS19_TDg2$player2
GWS19_TDg3 <- GWS19_TDg2
GWS19_TDg3$p1inp2vec <- is.element(GWS19_TDg3$player1, player2vector)
GWS19_TDg3$p2inp1vec <- is.element(GWS19_TDg3$player2, player1vector)

addPlayer1 <- GWS19_TDg3[ which(GWS19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_TDg3[ which(GWS19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_TDg2 <- rbind(GWS19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
GWS19_TDft <- ftable(GWS19_TDg2$player1, GWS19_TDg2$player2)
GWS19_TDft2 <- as.matrix(GWS19_TDft)
numRows <- nrow(GWS19_TDft2)
numCols <- ncol(GWS19_TDft2)
GWS19_TDft3 <- GWS19_TDft2[c(2:numRows) , c(2:numCols)]
GWS19_TDTable <- graph.adjacency(GWS19_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(GWS19_TDTable, vertex.label = V(GWS19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
GWS19_TD.clusterCoef <- transitivity(GWS19_TDTable, type="global") #cluster coefficient
GWS19_TD.degreeCent <- centralization.degree(GWS19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_TDftn <- as.network.matrix(GWS19_TDft)
GWS19_TD.netDensity <- network.density(GWS19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_TD.entropy <- entropy(GWS19_TDft) #entropy

GWS19_TD.netMx <- cbind(GWS19_TD.netMx, GWS19_TD.clusterCoef, GWS19_TD.degreeCent$centralization,
                        GWS19_TD.netDensity, GWS19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
GWS19_QTg2 <- data.frame(GWS19_QT)
GWS19_QTg2 <- GWS19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS19_QTg2$player1
player2vector <- GWS19_QTg2$player2
GWS19_QTg3 <- GWS19_QTg2
GWS19_QTg3$p1inp2vec <- is.element(GWS19_QTg3$player1, player2vector)
GWS19_QTg3$p2inp1vec <- is.element(GWS19_QTg3$player2, player1vector)

addPlayer1 <- GWS19_QTg3[ which(GWS19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS19_QTg3[ which(GWS19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS19_QTg2 <- rbind(GWS19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
GWS19_QTft <- ftable(GWS19_QTg2$player1, GWS19_QTg2$player2)
GWS19_QTft2 <- as.matrix(GWS19_QTft)
numRows <- nrow(GWS19_QTft2)
numCols <- ncol(GWS19_QTft2)
GWS19_QTft3 <- GWS19_QTft2[c(2:numRows) , c(2:numCols)]
GWS19_QTTable <- graph.adjacency(GWS19_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(GWS19_QTTable, vertex.label = V(GWS19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
GWS19_QT.clusterCoef <- transitivity(GWS19_QTTable, type="global") #cluster coefficient
GWS19_QT.degreeCent <- centralization.degree(GWS19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS19_QTftn <- as.network.matrix(GWS19_QTft)
GWS19_QT.netDensity <- network.density(GWS19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS19_QT.entropy <- entropy(GWS19_QTft) #entropy

GWS19_QT.netMx <- cbind(GWS19_QT.netMx, GWS19_QT.clusterCoef, GWS19_QT.degreeCent$centralization,
                        GWS19_QT.netDensity, GWS19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS19_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "Goal_F"
HAW19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
HAW19_Gg2 <- data.frame(HAW19_G)
HAW19_Gg2 <- HAW19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_Gg2$player1
player2vector <- HAW19_Gg2$player2
HAW19_Gg3 <- HAW19_Gg2
HAW19_Gg3$p1inp2vec <- is.element(HAW19_Gg3$player1, player2vector)
HAW19_Gg3$p2inp1vec <- is.element(HAW19_Gg3$player2, player1vector)

addPlayer1 <- HAW19_Gg3[ which(HAW19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_Gg3[ which(HAW19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_Gg2 <- rbind(HAW19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
HAW19_Gft <- ftable(HAW19_Gg2$player1, HAW19_Gg2$player2)
HAW19_Gft2 <- as.matrix(HAW19_Gft)
numRows <- nrow(HAW19_Gft2)
numCols <- ncol(HAW19_Gft2)
HAW19_Gft3 <- HAW19_Gft2[c(2:numRows) , c(2:numCols)]
HAW19_GTable <- graph.adjacency(HAW19_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(HAW19_GTable, vertex.label = V(HAW19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
HAW19_G.clusterCoef <- transitivity(HAW19_GTable, type="global") #cluster coefficient
HAW19_G.degreeCent <- centralization.degree(HAW19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_Gftn <- as.network.matrix(HAW19_Gft)
HAW19_G.netDensity <- network.density(HAW19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_G.entropy <- entropy(HAW19_Gft) #entropy

HAW19_G.netMx <- cbind(HAW19_G.netMx, HAW19_G.clusterCoef, HAW19_G.degreeCent$centralization,
                       HAW19_G.netDensity, HAW19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "Behind_F"
HAW19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
HAW19_Bg2 <- data.frame(HAW19_B)
HAW19_Bg2 <- HAW19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_Bg2$player1
player2vector <- HAW19_Bg2$player2
HAW19_Bg3 <- HAW19_Bg2
HAW19_Bg3$p1inp2vec <- is.element(HAW19_Bg3$player1, player2vector)
HAW19_Bg3$p2inp1vec <- is.element(HAW19_Bg3$player2, player1vector)

addPlayer1 <- HAW19_Bg3[ which(HAW19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW19_Bg2 <- rbind(HAW19_Bg2, addPlayer1)

#ROUND 19, Behind graph using weighted edges
HAW19_Bft <- ftable(HAW19_Bg2$player1, HAW19_Bg2$player2)
HAW19_Bft2 <- as.matrix(HAW19_Bft)
numRows <- nrow(HAW19_Bft2)
numCols <- ncol(HAW19_Bft2)
HAW19_Bft3 <- HAW19_Bft2[c(2:numRows) , c(1:numCols)]
HAW19_BTable <- graph.adjacency(HAW19_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(HAW19_BTable, vertex.label = V(HAW19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
HAW19_B.clusterCoef <- transitivity(HAW19_BTable, type="global") #cluster coefficient
HAW19_B.degreeCent <- centralization.degree(HAW19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_Bftn <- as.network.matrix(HAW19_Bft)
HAW19_B.netDensity <- network.density(HAW19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_B.entropy <- entropy(HAW19_Bft) #entropy

HAW19_B.netMx <- cbind(HAW19_B.netMx, HAW19_B.clusterCoef, HAW19_B.degreeCent$centralization,
                       HAW19_B.netDensity, HAW19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
HAW19_SFg2 <- data.frame(HAW19_SF)
HAW19_SFg2 <- HAW19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_SFg2$player1
player2vector <- HAW19_SFg2$player2
HAW19_SFg3 <- HAW19_SFg2
HAW19_SFg3$p1inp2vec <- is.element(HAW19_SFg3$player1, player2vector)
HAW19_SFg3$p2inp1vec <- is.element(HAW19_SFg3$player2, player1vector)

addPlayer1 <- HAW19_SFg3[ which(HAW19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_SFg3[ which(HAW19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_SFg2 <- rbind(HAW19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
HAW19_SFft <- ftable(HAW19_SFg2$player1, HAW19_SFg2$player2)
HAW19_SFft2 <- as.matrix(HAW19_SFft)
numRows <- nrow(HAW19_SFft2)
numCols <- ncol(HAW19_SFft2)
HAW19_SFft3 <- HAW19_SFft2[c(2:numRows) , c(2:numCols)]
HAW19_SFTable <- graph.adjacency(HAW19_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(HAW19_SFTable, vertex.label = V(HAW19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
HAW19_SF.clusterCoef <- transitivity(HAW19_SFTable, type="global") #cluster coefficient
HAW19_SF.degreeCent <- centralization.degree(HAW19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_SFftn <- as.network.matrix(HAW19_SFft)
HAW19_SF.netDensity <- network.density(HAW19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_SF.entropy <- entropy(HAW19_SFft) #entropy

HAW19_SF.netMx <- cbind(HAW19_SF.netMx, HAW19_SF.clusterCoef, HAW19_SF.degreeCent$centralization,
                        HAW19_SF.netDensity, HAW19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
HAW19_TFg2 <- data.frame(HAW19_TF)
HAW19_TFg2 <- HAW19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_TFg2$player1
player2vector <- HAW19_TFg2$player2
HAW19_TFg3 <- HAW19_TFg2
HAW19_TFg3$p1inp2vec <- is.element(HAW19_TFg3$player1, player2vector)
HAW19_TFg3$p2inp1vec <- is.element(HAW19_TFg3$player2, player1vector)

addPlayer1 <- HAW19_TFg3[ which(HAW19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_TFg3[ which(HAW19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_TFg2 <- rbind(HAW19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
HAW19_TFft <- ftable(HAW19_TFg2$player1, HAW19_TFg2$player2)
HAW19_TFft2 <- as.matrix(HAW19_TFft)
numRows <- nrow(HAW19_TFft2)
numCols <- ncol(HAW19_TFft2)
HAW19_TFft3 <- HAW19_TFft2[c(2:numRows) , c(2:numCols)]
HAW19_TFTable <- graph.adjacency(HAW19_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(HAW19_TFTable, vertex.label = V(HAW19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
HAW19_TF.clusterCoef <- transitivity(HAW19_TFTable, type="global") #cluster coefficient
HAW19_TF.degreeCent <- centralization.degree(HAW19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_TFftn <- as.network.matrix(HAW19_TFft)
HAW19_TF.netDensity <- network.density(HAW19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_TF.entropy <- entropy(HAW19_TFft) #entropy

HAW19_TF.netMx <- cbind(HAW19_TF.netMx, HAW19_TF.clusterCoef, HAW19_TF.degreeCent$centralization,
                        HAW19_TF.netDensity, HAW19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
HAW19_SAMg2 <- data.frame(HAW19_SAM)
HAW19_SAMg2 <- HAW19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_SAMg2$player1
player2vector <- HAW19_SAMg2$player2
HAW19_SAMg3 <- HAW19_SAMg2
HAW19_SAMg3$p1inp2vec <- is.element(HAW19_SAMg3$player1, player2vector)
HAW19_SAMg3$p2inp1vec <- is.element(HAW19_SAMg3$player2, player1vector)

addPlayer1 <- HAW19_SAMg3[ which(HAW19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_SAMg3[ which(HAW19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_SAMg2 <- rbind(HAW19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
HAW19_SAMft <- ftable(HAW19_SAMg2$player1, HAW19_SAMg2$player2)
HAW19_SAMft2 <- as.matrix(HAW19_SAMft)
numRows <- nrow(HAW19_SAMft2)
numCols <- ncol(HAW19_SAMft2)
HAW19_SAMft3 <- HAW19_SAMft2[c(2:numRows) , c(2:numCols)]
HAW19_SAMTable <- graph.adjacency(HAW19_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(HAW19_SAMTable, vertex.label = V(HAW19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
HAW19_SAM.clusterCoef <- transitivity(HAW19_SAMTable, type="global") #cluster coefficient
HAW19_SAM.degreeCent <- centralization.degree(HAW19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_SAMftn <- as.network.matrix(HAW19_SAMft)
HAW19_SAM.netDensity <- network.density(HAW19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_SAM.entropy <- entropy(HAW19_SAMft) #entropy

HAW19_SAM.netMx <- cbind(HAW19_SAM.netMx, HAW19_SAM.clusterCoef, HAW19_SAM.degreeCent$centralization,
                         HAW19_SAM.netDensity, HAW19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
HAW19_TAMg2 <- data.frame(HAW19_TAM)
HAW19_TAMg2 <- HAW19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_TAMg2$player1
player2vector <- HAW19_TAMg2$player2
HAW19_TAMg3 <- HAW19_TAMg2
HAW19_TAMg3$p1inp2vec <- is.element(HAW19_TAMg3$player1, player2vector)
HAW19_TAMg3$p2inp1vec <- is.element(HAW19_TAMg3$player2, player1vector)

addPlayer1 <- HAW19_TAMg3[ which(HAW19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_TAMg3[ which(HAW19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_TAMg2 <- rbind(HAW19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
HAW19_TAMft <- ftable(HAW19_TAMg2$player1, HAW19_TAMg2$player2)
HAW19_TAMft2 <- as.matrix(HAW19_TAMft)
numRows <- nrow(HAW19_TAMft2)
numCols <- ncol(HAW19_TAMft2)
HAW19_TAMft3 <- HAW19_TAMft2[c(2:numRows) , c(2:numCols)]
HAW19_TAMTable <- graph.adjacency(HAW19_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(HAW19_TAMTable, vertex.label = V(HAW19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
HAW19_TAM.clusterCoef <- transitivity(HAW19_TAMTable, type="global") #cluster coefficient
HAW19_TAM.degreeCent <- centralization.degree(HAW19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_TAMftn <- as.network.matrix(HAW19_TAMft)
HAW19_TAM.netDensity <- network.density(HAW19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_TAM.entropy <- entropy(HAW19_TAMft) #entropy

HAW19_TAM.netMx <- cbind(HAW19_TAM.netMx, HAW19_TAM.clusterCoef, HAW19_TAM.degreeCent$centralization,
                         HAW19_TAM.netDensity, HAW19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
HAW19_SDMg2 <- data.frame(HAW19_SDM)
HAW19_SDMg2 <- HAW19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_SDMg2$player1
player2vector <- HAW19_SDMg2$player2
HAW19_SDMg3 <- HAW19_SDMg2
HAW19_SDMg3$p1inp2vec <- is.element(HAW19_SDMg3$player1, player2vector)
HAW19_SDMg3$p2inp1vec <- is.element(HAW19_SDMg3$player2, player1vector)

addPlayer1 <- HAW19_SDMg3[ which(HAW19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_SDMg3[ which(HAW19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_SDMg2 <- rbind(HAW19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
HAW19_SDMft <- ftable(HAW19_SDMg2$player1, HAW19_SDMg2$player2)
HAW19_SDMft2 <- as.matrix(HAW19_SDMft)
numRows <- nrow(HAW19_SDMft2)
numCols <- ncol(HAW19_SDMft2)
HAW19_SDMft3 <- HAW19_SDMft2[c(2:numRows) , c(2:numCols)]
HAW19_SDMTable <- graph.adjacency(HAW19_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(HAW19_SDMTable, vertex.label = V(HAW19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
HAW19_SDM.clusterCoef <- transitivity(HAW19_SDMTable, type="global") #cluster coefficient
HAW19_SDM.degreeCent <- centralization.degree(HAW19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_SDMftn <- as.network.matrix(HAW19_SDMft)
HAW19_SDM.netDensity <- network.density(HAW19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_SDM.entropy <- entropy(HAW19_SDMft) #entropy

HAW19_SDM.netMx <- cbind(HAW19_SDM.netMx, HAW19_SDM.clusterCoef, HAW19_SDM.degreeCent$centralization,
                         HAW19_SDM.netDensity, HAW19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
HAW19_TDMg2 <- data.frame(HAW19_TDM)
HAW19_TDMg2 <- HAW19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_TDMg2$player1
player2vector <- HAW19_TDMg2$player2
HAW19_TDMg3 <- HAW19_TDMg2
HAW19_TDMg3$p1inp2vec <- is.element(HAW19_TDMg3$player1, player2vector)
HAW19_TDMg3$p2inp1vec <- is.element(HAW19_TDMg3$player2, player1vector)

addPlayer1 <- HAW19_TDMg3[ which(HAW19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_TDMg3[ which(HAW19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_TDMg2 <- rbind(HAW19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
HAW19_TDMft <- ftable(HAW19_TDMg2$player1, HAW19_TDMg2$player2)
HAW19_TDMft2 <- as.matrix(HAW19_TDMft)
numRows <- nrow(HAW19_TDMft2)
numCols <- ncol(HAW19_TDMft2)
HAW19_TDMft3 <- HAW19_TDMft2[c(2:numRows) , c(2:numCols)]
HAW19_TDMTable <- graph.adjacency(HAW19_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(HAW19_TDMTable, vertex.label = V(HAW19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
HAW19_TDM.clusterCoef <- transitivity(HAW19_TDMTable, type="global") #cluster coefficient
HAW19_TDM.degreeCent <- centralization.degree(HAW19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_TDMftn <- as.network.matrix(HAW19_TDMft)
HAW19_TDM.netDensity <- network.density(HAW19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_TDM.entropy <- entropy(HAW19_TDMft) #entropy

HAW19_TDM.netMx <- cbind(HAW19_TDM.netMx, HAW19_TDM.clusterCoef, HAW19_TDM.degreeCent$centralization,
                         HAW19_TDM.netDensity, HAW19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
HAW19_SDg2 <- data.frame(HAW19_SD)
HAW19_SDg2 <- HAW19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_SDg2$player1
player2vector <- HAW19_SDg2$player2
HAW19_SDg3 <- HAW19_SDg2
HAW19_SDg3$p1inp2vec <- is.element(HAW19_SDg3$player1, player2vector)
HAW19_SDg3$p2inp1vec <- is.element(HAW19_SDg3$player2, player1vector)

addPlayer1 <- HAW19_SDg3[ which(HAW19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_SDg3[ which(HAW19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_SDg2 <- rbind(HAW19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
HAW19_SDft <- ftable(HAW19_SDg2$player1, HAW19_SDg2$player2)
HAW19_SDft2 <- as.matrix(HAW19_SDft)
numRows <- nrow(HAW19_SDft2)
numCols <- ncol(HAW19_SDft2)
HAW19_SDft3 <- HAW19_SDft2[c(2:numRows) , c(2:numCols)]
HAW19_SDTable <- graph.adjacency(HAW19_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(HAW19_SDTable, vertex.label = V(HAW19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
HAW19_SD.clusterCoef <- transitivity(HAW19_SDTable, type="global") #cluster coefficient
HAW19_SD.degreeCent <- centralization.degree(HAW19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_SDftn <- as.network.matrix(HAW19_SDft)
HAW19_SD.netDensity <- network.density(HAW19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_SD.entropy <- entropy(HAW19_SDft) #entropy

HAW19_SD.netMx <- cbind(HAW19_SD.netMx, HAW19_SD.clusterCoef, HAW19_SD.degreeCent$centralization,
                        HAW19_SD.netDensity, HAW19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
HAW19_TDg2 <- data.frame(HAW19_TD)
HAW19_TDg2 <- HAW19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_TDg2$player1
player2vector <- HAW19_TDg2$player2
HAW19_TDg3 <- HAW19_TDg2
HAW19_TDg3$p1inp2vec <- is.element(HAW19_TDg3$player1, player2vector)
HAW19_TDg3$p2inp1vec <- is.element(HAW19_TDg3$player2, player1vector)

addPlayer1 <- HAW19_TDg3[ which(HAW19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_TDg3[ which(HAW19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_TDg2 <- rbind(HAW19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
HAW19_TDft <- ftable(HAW19_TDg2$player1, HAW19_TDg2$player2)
HAW19_TDft2 <- as.matrix(HAW19_TDft)
numRows <- nrow(HAW19_TDft2)
numCols <- ncol(HAW19_TDft2)
HAW19_TDft3 <- HAW19_TDft2[c(2:numRows) , c(2:numCols)]
HAW19_TDTable <- graph.adjacency(HAW19_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(HAW19_TDTable, vertex.label = V(HAW19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
HAW19_TD.clusterCoef <- transitivity(HAW19_TDTable, type="global") #cluster coefficient
HAW19_TD.degreeCent <- centralization.degree(HAW19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_TDftn <- as.network.matrix(HAW19_TDft)
HAW19_TD.netDensity <- network.density(HAW19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_TD.entropy <- entropy(HAW19_TDft) #entropy

HAW19_TD.netMx <- cbind(HAW19_TD.netMx, HAW19_TD.clusterCoef, HAW19_TD.degreeCent$centralization,
                        HAW19_TD.netDensity, HAW19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
HAW19_QTg2 <- data.frame(HAW19_QT)
HAW19_QTg2 <- HAW19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW19_QTg2$player1
player2vector <- HAW19_QTg2$player2
HAW19_QTg3 <- HAW19_QTg2
HAW19_QTg3$p1inp2vec <- is.element(HAW19_QTg3$player1, player2vector)
HAW19_QTg3$p2inp1vec <- is.element(HAW19_QTg3$player2, player1vector)

addPlayer1 <- HAW19_QTg3[ which(HAW19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW19_QTg3[ which(HAW19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW19_QTg2 <- rbind(HAW19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
HAW19_QTft <- ftable(HAW19_QTg2$player1, HAW19_QTg2$player2)
HAW19_QTft2 <- as.matrix(HAW19_QTft)
numRows <- nrow(HAW19_QTft2)
numCols <- ncol(HAW19_QTft2)
HAW19_QTft3 <- HAW19_QTft2[c(2:numRows) , c(2:numCols)]
HAW19_QTTable <- graph.adjacency(HAW19_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(HAW19_QTTable, vertex.label = V(HAW19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
HAW19_QT.clusterCoef <- transitivity(HAW19_QTTable, type="global") #cluster coefficient
HAW19_QT.degreeCent <- centralization.degree(HAW19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW19_QTftn <- as.network.matrix(HAW19_QTft)
HAW19_QT.netDensity <- network.density(HAW19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW19_QT.entropy <- entropy(HAW19_QTft) #entropy

HAW19_QT.netMx <- cbind(HAW19_QT.netMx, HAW19_QT.clusterCoef, HAW19_QT.degreeCent$centralization,
                        HAW19_QT.netDensity, HAW19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW19_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "MELB"
KIoutcome = "Goal_F"
MELB19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
MELB19_Gg2 <- data.frame(MELB19_G)
MELB19_Gg2 <- MELB19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_Gg2$player1
player2vector <- MELB19_Gg2$player2
MELB19_Gg3 <- MELB19_Gg2
MELB19_Gg3$p1inp2vec <- is.element(MELB19_Gg3$player1, player2vector)
MELB19_Gg3$p2inp1vec <- is.element(MELB19_Gg3$player2, player1vector)

addPlayer1 <- MELB19_Gg3[ which(MELB19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB19_Gg2 <- rbind(MELB19_Gg2, addPlayer1)

#ROUND 19, Goal graph using weighted edges
MELB19_Gft <- ftable(MELB19_Gg2$player1, MELB19_Gg2$player2)
MELB19_Gft2 <- as.matrix(MELB19_Gft)
numRows <- nrow(MELB19_Gft2)
numCols <- ncol(MELB19_Gft2)
MELB19_Gft3 <- MELB19_Gft2[c(2:numRows) , c(1:numCols)]
MELB19_GTable <- graph.adjacency(MELB19_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(MELB19_GTable, vertex.label = V(MELB19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
MELB19_G.clusterCoef <- transitivity(MELB19_GTable, type="global") #cluster coefficient
MELB19_G.degreeCent <- centralization.degree(MELB19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_Gftn <- as.network.matrix(MELB19_Gft)
MELB19_G.netDensity <- network.density(MELB19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_G.entropy <- entropy(MELB19_Gft) #entropy

MELB19_G.netMx <- cbind(MELB19_G.netMx, MELB19_G.clusterCoef, MELB19_G.degreeCent$centralization,
                        MELB19_G.netDensity, MELB19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "MELB"
KIoutcome = "Behind_F"
MELB19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
MELB19_Bg2 <- data.frame(MELB19_B)
MELB19_Bg2 <- MELB19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_Bg2$player1
player2vector <- MELB19_Bg2$player2
MELB19_Bg3 <- MELB19_Bg2
MELB19_Bg3$p1inp2vec <- is.element(MELB19_Bg3$player1, player2vector)
MELB19_Bg3$p2inp1vec <- is.element(MELB19_Bg3$player2, player1vector)

addPlayer1 <- MELB19_Bg3[ which(MELB19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_Bg3[ which(MELB19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_Bg2 <- rbind(MELB19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
MELB19_Bft <- ftable(MELB19_Bg2$player1, MELB19_Bg2$player2)
MELB19_Bft2 <- as.matrix(MELB19_Bft)
numRows <- nrow(MELB19_Bft2)
numCols <- ncol(MELB19_Bft2)
MELB19_Bft3 <- MELB19_Bft2[c(2:numRows) , c(2:numCols)]
MELB19_BTable <- graph.adjacency(MELB19_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(MELB19_BTable, vertex.label = V(MELB19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
MELB19_B.clusterCoef <- transitivity(MELB19_BTable, type="global") #cluster coefficient
MELB19_B.degreeCent <- centralization.degree(MELB19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_Bftn <- as.network.matrix(MELB19_Bft)
MELB19_B.netDensity <- network.density(MELB19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_B.entropy <- entropy(MELB19_Bft) #entropy

MELB19_B.netMx <- cbind(MELB19_B.netMx, MELB19_B.clusterCoef, MELB19_B.degreeCent$centralization,
                        MELB19_B.netDensity, MELB19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
MELB19_SFg2 <- data.frame(MELB19_SF)
MELB19_SFg2 <- MELB19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_SFg2$player1
player2vector <- MELB19_SFg2$player2
MELB19_SFg3 <- MELB19_SFg2
MELB19_SFg3$p1inp2vec <- is.element(MELB19_SFg3$player1, player2vector)
MELB19_SFg3$p2inp1vec <- is.element(MELB19_SFg3$player2, player1vector)

addPlayer1 <- MELB19_SFg3[ which(MELB19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_SFg3[ which(MELB19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_SFg2 <- rbind(MELB19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
MELB19_SFft <- ftable(MELB19_SFg2$player1, MELB19_SFg2$player2)
MELB19_SFft2 <- as.matrix(MELB19_SFft)
numRows <- nrow(MELB19_SFft2)
numCols <- ncol(MELB19_SFft2)
MELB19_SFft3 <- MELB19_SFft2[c(2:numRows) , c(2:numCols)]
MELB19_SFTable <- graph.adjacency(MELB19_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(MELB19_SFTable, vertex.label = V(MELB19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
MELB19_SF.clusterCoef <- transitivity(MELB19_SFTable, type="global") #cluster coefficient
MELB19_SF.degreeCent <- centralization.degree(MELB19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_SFftn <- as.network.matrix(MELB19_SFft)
MELB19_SF.netDensity <- network.density(MELB19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_SF.entropy <- entropy(MELB19_SFft) #entropy

MELB19_SF.netMx <- cbind(MELB19_SF.netMx, MELB19_SF.clusterCoef, MELB19_SF.degreeCent$centralization,
                         MELB19_SF.netDensity, MELB19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************
#NA

round = 19
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
MELB19_TFg2 <- data.frame(MELB19_TF)
MELB19_TFg2 <- MELB19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_TFg2$player1
player2vector <- MELB19_TFg2$player2
MELB19_TFg3 <- MELB19_TFg2
MELB19_TFg3$p1inp2vec <- is.element(MELB19_TFg3$player1, player2vector)
MELB19_TFg3$p2inp1vec <- is.element(MELB19_TFg3$player2, player1vector)

addPlayer1 <- MELB19_TFg3[ which(MELB19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_TFg3[ which(MELB19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_TFg2 <- rbind(MELB19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
MELB19_TFft <- ftable(MELB19_TFg2$player1, MELB19_TFg2$player2)
MELB19_TFft2 <- as.matrix(MELB19_TFft)
numRows <- nrow(MELB19_TFft2)
numCols <- ncol(MELB19_TFft2)
MELB19_TFft3 <- MELB19_TFft2[c(2:numRows) , c(2:numCols)]
MELB19_TFTable <- graph.adjacency(MELB19_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(MELB19_TFTable, vertex.label = V(MELB19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
MELB19_TF.clusterCoef <- transitivity(MELB19_TFTable, type="global") #cluster coefficient
MELB19_TF.degreeCent <- centralization.degree(MELB19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_TFftn <- as.network.matrix(MELB19_TFft)
MELB19_TF.netDensity <- network.density(MELB19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_TF.entropy <- entropy(MELB19_TFft) #entropy

MELB19_TF.netMx <- cbind(MELB19_TF.netMx, MELB19_TF.clusterCoef, MELB19_TF.degreeCent$centralization,
                         MELB19_TF.netDensity, MELB19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************

round = 19
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
MELB19_SAMg2 <- data.frame(MELB19_SAM)
MELB19_SAMg2 <- MELB19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_SAMg2$player1
player2vector <- MELB19_SAMg2$player2
MELB19_SAMg3 <- MELB19_SAMg2
MELB19_SAMg3$p1inp2vec <- is.element(MELB19_SAMg3$player1, player2vector)
MELB19_SAMg3$p2inp1vec <- is.element(MELB19_SAMg3$player2, player1vector)

addPlayer1 <- MELB19_SAMg3[ which(MELB19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_SAMg3[ which(MELB19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_SAMg2 <- rbind(MELB19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
MELB19_SAMft <- ftable(MELB19_SAMg2$player1, MELB19_SAMg2$player2)
MELB19_SAMft2 <- as.matrix(MELB19_SAMft)
numRows <- nrow(MELB19_SAMft2)
numCols <- ncol(MELB19_SAMft2)
MELB19_SAMft3 <- MELB19_SAMft2[c(2:numRows) , c(2:numCols)]
MELB19_SAMTable <- graph.adjacency(MELB19_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(MELB19_SAMTable, vertex.label = V(MELB19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
MELB19_SAM.clusterCoef <- transitivity(MELB19_SAMTable, type="global") #cluster coefficient
MELB19_SAM.degreeCent <- centralization.degree(MELB19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_SAMftn <- as.network.matrix(MELB19_SAMft)
MELB19_SAM.netDensity <- network.density(MELB19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_SAM.entropy <- entropy(MELB19_SAMft) #entropy

MELB19_SAM.netMx <- cbind(MELB19_SAM.netMx, MELB19_SAM.clusterCoef, MELB19_SAM.degreeCent$centralization,
                          MELB19_SAM.netDensity, MELB19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
MELB19_TAMg2 <- data.frame(MELB19_TAM)
MELB19_TAMg2 <- MELB19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_TAMg2$player1
player2vector <- MELB19_TAMg2$player2
MELB19_TAMg3 <- MELB19_TAMg2
MELB19_TAMg3$p1inp2vec <- is.element(MELB19_TAMg3$player1, player2vector)
MELB19_TAMg3$p2inp1vec <- is.element(MELB19_TAMg3$player2, player1vector)

addPlayer1 <- MELB19_TAMg3[ which(MELB19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_TAMg3[ which(MELB19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_TAMg2 <- rbind(MELB19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
MELB19_TAMft <- ftable(MELB19_TAMg2$player1, MELB19_TAMg2$player2)
MELB19_TAMft2 <- as.matrix(MELB19_TAMft)
numRows <- nrow(MELB19_TAMft2)
numCols <- ncol(MELB19_TAMft2)
MELB19_TAMft3 <- MELB19_TAMft2[c(2:numRows) , c(2:numCols)]
MELB19_TAMTable <- graph.adjacency(MELB19_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(MELB19_TAMTable, vertex.label = V(MELB19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
MELB19_TAM.clusterCoef <- transitivity(MELB19_TAMTable, type="global") #cluster coefficient
MELB19_TAM.degreeCent <- centralization.degree(MELB19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_TAMftn <- as.network.matrix(MELB19_TAMft)
MELB19_TAM.netDensity <- network.density(MELB19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_TAM.entropy <- entropy(MELB19_TAMft) #entropy

MELB19_TAM.netMx <- cbind(MELB19_TAM.netMx, MELB19_TAM.clusterCoef, MELB19_TAM.degreeCent$centralization,
                          MELB19_TAM.netDensity, MELB19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************

round = 19
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
MELB19_SDMg2 <- data.frame(MELB19_SDM)
MELB19_SDMg2 <- MELB19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_SDMg2$player1
player2vector <- MELB19_SDMg2$player2
MELB19_SDMg3 <- MELB19_SDMg2
MELB19_SDMg3$p1inp2vec <- is.element(MELB19_SDMg3$player1, player2vector)
MELB19_SDMg3$p2inp1vec <- is.element(MELB19_SDMg3$player2, player1vector)

addPlayer1 <- MELB19_SDMg3[ which(MELB19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_SDMg3[ which(MELB19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_SDMg2 <- rbind(MELB19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
MELB19_SDMft <- ftable(MELB19_SDMg2$player1, MELB19_SDMg2$player2)
MELB19_SDMft2 <- as.matrix(MELB19_SDMft)
numRows <- nrow(MELB19_SDMft2)
numCols <- ncol(MELB19_SDMft2)
MELB19_SDMft3 <- MELB19_SDMft2[c(2:numRows) , c(2:numCols)]
MELB19_SDMTable <- graph.adjacency(MELB19_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(MELB19_SDMTable, vertex.label = V(MELB19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
MELB19_SDM.clusterCoef <- transitivity(MELB19_SDMTable, type="global") #cluster coefficient
MELB19_SDM.degreeCent <- centralization.degree(MELB19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_SDMftn <- as.network.matrix(MELB19_SDMft)
MELB19_SDM.netDensity <- network.density(MELB19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_SDM.entropy <- entropy(MELB19_SDMft) #entropy

MELB19_SDM.netMx <- cbind(MELB19_SDM.netMx, MELB19_SDM.clusterCoef, MELB19_SDM.degreeCent$centralization,
                          MELB19_SDM.netDensity, MELB19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
MELB19_TDMg2 <- data.frame(MELB19_TDM)
MELB19_TDMg2 <- MELB19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_TDMg2$player1
player2vector <- MELB19_TDMg2$player2
MELB19_TDMg3 <- MELB19_TDMg2
MELB19_TDMg3$p1inp2vec <- is.element(MELB19_TDMg3$player1, player2vector)
MELB19_TDMg3$p2inp1vec <- is.element(MELB19_TDMg3$player2, player1vector)

addPlayer1 <- MELB19_TDMg3[ which(MELB19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_TDMg3[ which(MELB19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_TDMg2 <- rbind(MELB19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
MELB19_TDMft <- ftable(MELB19_TDMg2$player1, MELB19_TDMg2$player2)
MELB19_TDMft2 <- as.matrix(MELB19_TDMft)
numRows <- nrow(MELB19_TDMft2)
numCols <- ncol(MELB19_TDMft2)
MELB19_TDMft3 <- MELB19_TDMft2[c(2:numRows) , c(2:numCols)]
MELB19_TDMTable <- graph.adjacency(MELB19_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(MELB19_TDMTable, vertex.label = V(MELB19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
MELB19_TDM.clusterCoef <- transitivity(MELB19_TDMTable, type="global") #cluster coefficient
MELB19_TDM.degreeCent <- centralization.degree(MELB19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_TDMftn <- as.network.matrix(MELB19_TDMft)
MELB19_TDM.netDensity <- network.density(MELB19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_TDM.entropy <- entropy(MELB19_TDMft) #entropy

MELB19_TDM.netMx <- cbind(MELB19_TDM.netMx, MELB19_TDM.clusterCoef, MELB19_TDM.degreeCent$centralization,
                          MELB19_TDM.netDensity, MELB19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
MELB19_SDg2 <- data.frame(MELB19_SD)
MELB19_SDg2 <- MELB19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_SDg2$player1
player2vector <- MELB19_SDg2$player2
MELB19_SDg3 <- MELB19_SDg2
MELB19_SDg3$p1inp2vec <- is.element(MELB19_SDg3$player1, player2vector)
MELB19_SDg3$p2inp1vec <- is.element(MELB19_SDg3$player2, player1vector)

addPlayer1 <- MELB19_SDg3[ which(MELB19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_SDg3[ which(MELB19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_SDg2 <- rbind(MELB19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
MELB19_SDft <- ftable(MELB19_SDg2$player1, MELB19_SDg2$player2)
MELB19_SDft2 <- as.matrix(MELB19_SDft)
numRows <- nrow(MELB19_SDft2)
numCols <- ncol(MELB19_SDft2)
MELB19_SDft3 <- MELB19_SDft2[c(2:numRows) , c(2:numCols)]
MELB19_SDTable <- graph.adjacency(MELB19_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(MELB19_SDTable, vertex.label = V(MELB19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
MELB19_SD.clusterCoef <- transitivity(MELB19_SDTable, type="global") #cluster coefficient
MELB19_SD.degreeCent <- centralization.degree(MELB19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_SDftn <- as.network.matrix(MELB19_SDft)
MELB19_SD.netDensity <- network.density(MELB19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_SD.entropy <- entropy(MELB19_SDft) #entropy

MELB19_SD.netMx <- cbind(MELB19_SD.netMx, MELB19_SD.clusterCoef, MELB19_SD.degreeCent$centralization,
                         MELB19_SD.netDensity, MELB19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
MELB19_TDg2 <- data.frame(MELB19_TD)
MELB19_TDg2 <- MELB19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_TDg2$player1
player2vector <- MELB19_TDg2$player2
MELB19_TDg3 <- MELB19_TDg2
MELB19_TDg3$p1inp2vec <- is.element(MELB19_TDg3$player1, player2vector)
MELB19_TDg3$p2inp1vec <- is.element(MELB19_TDg3$player2, player1vector)

addPlayer1 <- MELB19_TDg3[ which(MELB19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_TDg3[ which(MELB19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_TDg2 <- rbind(MELB19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
MELB19_TDft <- ftable(MELB19_TDg2$player1, MELB19_TDg2$player2)
MELB19_TDft2 <- as.matrix(MELB19_TDft)
numRows <- nrow(MELB19_TDft2)
numCols <- ncol(MELB19_TDft2)
MELB19_TDft3 <- MELB19_TDft2[c(2:numRows) , c(2:numCols)]
MELB19_TDTable <- graph.adjacency(MELB19_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(MELB19_TDTable, vertex.label = V(MELB19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
MELB19_TD.clusterCoef <- transitivity(MELB19_TDTable, type="global") #cluster coefficient
MELB19_TD.degreeCent <- centralization.degree(MELB19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_TDftn <- as.network.matrix(MELB19_TDft)
MELB19_TD.netDensity <- network.density(MELB19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_TD.entropy <- entropy(MELB19_TDft) #entropy

MELB19_TD.netMx <- cbind(MELB19_TD.netMx, MELB19_TD.clusterCoef, MELB19_TD.degreeCent$centralization,
                         MELB19_TD.netDensity, MELB19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
MELB19_QTg2 <- data.frame(MELB19_QT)
MELB19_QTg2 <- MELB19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB19_QTg2$player1
player2vector <- MELB19_QTg2$player2
MELB19_QTg3 <- MELB19_QTg2
MELB19_QTg3$p1inp2vec <- is.element(MELB19_QTg3$player1, player2vector)
MELB19_QTg3$p2inp1vec <- is.element(MELB19_QTg3$player2, player1vector)

addPlayer1 <- MELB19_QTg3[ which(MELB19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB19_QTg3[ which(MELB19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB19_QTg2 <- rbind(MELB19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
MELB19_QTft <- ftable(MELB19_QTg2$player1, MELB19_QTg2$player2)
MELB19_QTft2 <- as.matrix(MELB19_QTft)
numRows <- nrow(MELB19_QTft2)
numCols <- ncol(MELB19_QTft2)
MELB19_QTft3 <- MELB19_QTft2[c(2:numRows) , c(2:numCols)]
MELB19_QTTable <- graph.adjacency(MELB19_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(MELB19_QTTable, vertex.label = V(MELB19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
MELB19_QT.clusterCoef <- transitivity(MELB19_QTTable, type="global") #cluster coefficient
MELB19_QT.degreeCent <- centralization.degree(MELB19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB19_QTftn <- as.network.matrix(MELB19_QTft)
MELB19_QT.netDensity <- network.density(MELB19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB19_QT.entropy <- entropy(MELB19_QTft) #entropy

MELB19_QT.netMx <- cbind(MELB19_QT.netMx, MELB19_QT.clusterCoef, MELB19_QT.degreeCent$centralization,
                         MELB19_QT.netDensity, MELB19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB19_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************

round = 19
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
NMFC19_Gg2 <- data.frame(NMFC19_G)
NMFC19_Gg2 <- NMFC19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_Gg2$player1
player2vector <- NMFC19_Gg2$player2
NMFC19_Gg3 <- NMFC19_Gg2
NMFC19_Gg3$p1inp2vec <- is.element(NMFC19_Gg3$player1, player2vector)
NMFC19_Gg3$p2inp1vec <- is.element(NMFC19_Gg3$player2, player1vector)

addPlayer1 <- NMFC19_Gg3[ which(NMFC19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_Gg3[ which(NMFC19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_Gg2 <- rbind(NMFC19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
NMFC19_Gft <- ftable(NMFC19_Gg2$player1, NMFC19_Gg2$player2)
NMFC19_Gft2 <- as.matrix(NMFC19_Gft)
numRows <- nrow(NMFC19_Gft2)
numCols <- ncol(NMFC19_Gft2)
NMFC19_Gft3 <- NMFC19_Gft2[c(2:numRows) , c(2:numCols)]
NMFC19_GTable <- graph.adjacency(NMFC19_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(NMFC19_GTable, vertex.label = V(NMFC19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
NMFC19_G.clusterCoef <- transitivity(NMFC19_GTable, type="global") #cluster coefficient
NMFC19_G.degreeCent <- centralization.degree(NMFC19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_Gftn <- as.network.matrix(NMFC19_Gft)
NMFC19_G.netDensity <- network.density(NMFC19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_G.entropy <- entropy(NMFC19_Gft) #entropy

NMFC19_G.netMx <- cbind(NMFC19_G.netMx, NMFC19_G.clusterCoef, NMFC19_G.degreeCent$centralization,
                        NMFC19_G.netDensity, NMFC19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************

round = 19
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
NMFC19_Bg2 <- data.frame(NMFC19_B)
NMFC19_Bg2 <- NMFC19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_Bg2$player1
player2vector <- NMFC19_Bg2$player2
NMFC19_Bg3 <- NMFC19_Bg2
NMFC19_Bg3$p1inp2vec <- is.element(NMFC19_Bg3$player1, player2vector)
NMFC19_Bg3$p2inp1vec <- is.element(NMFC19_Bg3$player2, player1vector)

addPlayer1 <- NMFC19_Bg3[ which(NMFC19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_Bg3[ which(NMFC19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_Bg2 <- rbind(NMFC19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
NMFC19_Bft <- ftable(NMFC19_Bg2$player1, NMFC19_Bg2$player2)
NMFC19_Bft2 <- as.matrix(NMFC19_Bft)
numRows <- nrow(NMFC19_Bft2)
numCols <- ncol(NMFC19_Bft2)
NMFC19_Bft3 <- NMFC19_Bft2[c(2:numRows) , c(2:numCols)]
NMFC19_BTable <- graph.adjacency(NMFC19_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(NMFC19_BTable, vertex.label = V(NMFC19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
NMFC19_B.clusterCoef <- transitivity(NMFC19_BTable, type="global") #cluster coefficient
NMFC19_B.degreeCent <- centralization.degree(NMFC19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_Bftn <- as.network.matrix(NMFC19_Bft)
NMFC19_B.netDensity <- network.density(NMFC19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_B.entropy <- entropy(NMFC19_Bft) #entropy

NMFC19_B.netMx <- cbind(NMFC19_B.netMx, NMFC19_B.clusterCoef, NMFC19_B.degreeCent$centralization,
                        NMFC19_B.netDensity, NMFC19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************

round = 19
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
NMFC19_SFg2 <- data.frame(NMFC19_SF)
NMFC19_SFg2 <- NMFC19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_SFg2$player1
player2vector <- NMFC19_SFg2$player2
NMFC19_SFg3 <- NMFC19_SFg2
NMFC19_SFg3$p1inp2vec <- is.element(NMFC19_SFg3$player1, player2vector)
NMFC19_SFg3$p2inp1vec <- is.element(NMFC19_SFg3$player2, player1vector)

addPlayer1 <- NMFC19_SFg3[ which(NMFC19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC19_SFg3[ which(NMFC19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_SFg2 <- rbind(NMFC19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
NMFC19_SFft <- ftable(NMFC19_SFg2$player1, NMFC19_SFg2$player2)
NMFC19_SFft2 <- as.matrix(NMFC19_SFft)
numRows <- nrow(NMFC19_SFft2)
numCols <- ncol(NMFC19_SFft2)
NMFC19_SFft3 <- NMFC19_SFft2[c(2:numRows) , c(2:numCols)]
NMFC19_SFTable <- graph.adjacency(NMFC19_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(NMFC19_SFTable, vertex.label = V(NMFC19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
NMFC19_SF.clusterCoef <- transitivity(NMFC19_SFTable, type="global") #cluster coefficient
NMFC19_SF.degreeCent <- centralization.degree(NMFC19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_SFftn <- as.network.matrix(NMFC19_SFft)
NMFC19_SF.netDensity <- network.density(NMFC19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_SF.entropy <- entropy(NMFC19_SFft) #entropy

NMFC19_SF.netMx <- cbind(NMFC19_SF.netMx, NMFC19_SF.clusterCoef, NMFC19_SF.degreeCent$centralization,
                         NMFC19_SF.netDensity, NMFC19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
NMFC19_TFg2 <- data.frame(NMFC19_TF)
NMFC19_TFg2 <- NMFC19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_TFg2$player1
player2vector <- NMFC19_TFg2$player2
NMFC19_TFg3 <- NMFC19_TFg2
NMFC19_TFg3$p1inp2vec <- is.element(NMFC19_TFg3$player1, player2vector)
NMFC19_TFg3$p2inp1vec <- is.element(NMFC19_TFg3$player2, player1vector)

addPlayer1 <- NMFC19_TFg3[ which(NMFC19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_TFg3[ which(NMFC19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_TFg2 <- rbind(NMFC19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
NMFC19_TFft <- ftable(NMFC19_TFg2$player1, NMFC19_TFg2$player2)
NMFC19_TFft2 <- as.matrix(NMFC19_TFft)
numRows <- nrow(NMFC19_TFft2)
numCols <- ncol(NMFC19_TFft2)
NMFC19_TFft3 <- NMFC19_TFft2[c(2:numRows) , c(2:numCols)]
NMFC19_TFTable <- graph.adjacency(NMFC19_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(NMFC19_TFTable, vertex.label = V(NMFC19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
NMFC19_TF.clusterCoef <- transitivity(NMFC19_TFTable, type="global") #cluster coefficient
NMFC19_TF.degreeCent <- centralization.degree(NMFC19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_TFftn <- as.network.matrix(NMFC19_TFft)
NMFC19_TF.netDensity <- network.density(NMFC19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_TF.entropy <- entropy(NMFC19_TFft) #entropy

NMFC19_TF.netMx <- cbind(NMFC19_TF.netMx, NMFC19_TF.clusterCoef, NMFC19_TF.degreeCent$centralization,
                         NMFC19_TF.netDensity, NMFC19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************

round = 19
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
NMFC19_SAMg2 <- data.frame(NMFC19_SAM)
NMFC19_SAMg2 <- NMFC19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_SAMg2$player1
player2vector <- NMFC19_SAMg2$player2
NMFC19_SAMg3 <- NMFC19_SAMg2
NMFC19_SAMg3$p1inp2vec <- is.element(NMFC19_SAMg3$player1, player2vector)
NMFC19_SAMg3$p2inp1vec <- is.element(NMFC19_SAMg3$player2, player1vector)

addPlayer1 <- NMFC19_SAMg3[ which(NMFC19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_SAMg3[ which(NMFC19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_SAMg2 <- rbind(NMFC19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
NMFC19_SAMft <- ftable(NMFC19_SAMg2$player1, NMFC19_SAMg2$player2)
NMFC19_SAMft2 <- as.matrix(NMFC19_SAMft)
numRows <- nrow(NMFC19_SAMft2)
numCols <- ncol(NMFC19_SAMft2)
NMFC19_SAMft3 <- NMFC19_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC19_SAMTable <- graph.adjacency(NMFC19_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(NMFC19_SAMTable, vertex.label = V(NMFC19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
NMFC19_SAM.clusterCoef <- transitivity(NMFC19_SAMTable, type="global") #cluster coefficient
NMFC19_SAM.degreeCent <- centralization.degree(NMFC19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_SAMftn <- as.network.matrix(NMFC19_SAMft)
NMFC19_SAM.netDensity <- network.density(NMFC19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_SAM.entropy <- entropy(NMFC19_SAMft) #entropy

NMFC19_SAM.netMx <- cbind(NMFC19_SAM.netMx, NMFC19_SAM.clusterCoef, NMFC19_SAM.degreeCent$centralization,
                          NMFC19_SAM.netDensity, NMFC19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************
#NA

round = 19
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
NMFC19_TAMg2 <- data.frame(NMFC19_TAM)
NMFC19_TAMg2 <- NMFC19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_TAMg2$player1
player2vector <- NMFC19_TAMg2$player2
NMFC19_TAMg3 <- NMFC19_TAMg2
NMFC19_TAMg3$p1inp2vec <- is.element(NMFC19_TAMg3$player1, player2vector)
NMFC19_TAMg3$p2inp1vec <- is.element(NMFC19_TAMg3$player2, player1vector)

addPlayer1 <- NMFC19_TAMg3[ which(NMFC19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_TAMg3[ which(NMFC19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_TAMg2 <- rbind(NMFC19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
NMFC19_TAMft <- ftable(NMFC19_TAMg2$player1, NMFC19_TAMg2$player2)
NMFC19_TAMft2 <- as.matrix(NMFC19_TAMft)
numRows <- nrow(NMFC19_TAMft2)
numCols <- ncol(NMFC19_TAMft2)
NMFC19_TAMft3 <- NMFC19_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC19_TAMTable <- graph.adjacency(NMFC19_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(NMFC19_TAMTable, vertex.label = V(NMFC19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
NMFC19_TAM.clusterCoef <- transitivity(NMFC19_TAMTable, type="global") #cluster coefficient
NMFC19_TAM.degreeCent <- centralization.degree(NMFC19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_TAMftn <- as.network.matrix(NMFC19_TAMft)
NMFC19_TAM.netDensity <- network.density(NMFC19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_TAM.entropy <- entropy(NMFC19_TAMft) #entropy

NMFC19_TAM.netMx <- cbind(NMFC19_TAM.netMx, NMFC19_TAM.clusterCoef, NMFC19_TAM.degreeCent$centralization,
                          NMFC19_TAM.netDensity, NMFC19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************
#NA

round = 19
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
NMFC19_SDMg2 <- data.frame(NMFC19_SDM)
NMFC19_SDMg2 <- NMFC19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_SDMg2$player1
player2vector <- NMFC19_SDMg2$player2
NMFC19_SDMg3 <- NMFC19_SDMg2
NMFC19_SDMg3$p1inp2vec <- is.element(NMFC19_SDMg3$player1, player2vector)
NMFC19_SDMg3$p2inp1vec <- is.element(NMFC19_SDMg3$player2, player1vector)

addPlayer1 <- NMFC19_SDMg3[ which(NMFC19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_SDMg3[ which(NMFC19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_SDMg2 <- rbind(NMFC19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
NMFC19_SDMft <- ftable(NMFC19_SDMg2$player1, NMFC19_SDMg2$player2)
NMFC19_SDMft2 <- as.matrix(NMFC19_SDMft)
numRows <- nrow(NMFC19_SDMft2)
numCols <- ncol(NMFC19_SDMft2)
NMFC19_SDMft3 <- NMFC19_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC19_SDMTable <- graph.adjacency(NMFC19_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(NMFC19_SDMTable, vertex.label = V(NMFC19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
NMFC19_SDM.clusterCoef <- transitivity(NMFC19_SDMTable, type="global") #cluster coefficient
NMFC19_SDM.degreeCent <- centralization.degree(NMFC19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_SDMftn <- as.network.matrix(NMFC19_SDMft)
NMFC19_SDM.netDensity <- network.density(NMFC19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_SDM.entropy <- entropy(NMFC19_SDMft) #entropy

NMFC19_SDM.netMx <- cbind(NMFC19_SDM.netMx, NMFC19_SDM.clusterCoef, NMFC19_SDM.degreeCent$centralization,
                          NMFC19_SDM.netDensity, NMFC19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
NMFC19_TDMg2 <- data.frame(NMFC19_TDM)
NMFC19_TDMg2 <- NMFC19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_TDMg2$player1
player2vector <- NMFC19_TDMg2$player2
NMFC19_TDMg3 <- NMFC19_TDMg2
NMFC19_TDMg3$p1inp2vec <- is.element(NMFC19_TDMg3$player1, player2vector)
NMFC19_TDMg3$p2inp1vec <- is.element(NMFC19_TDMg3$player2, player1vector)

addPlayer1 <- NMFC19_TDMg3[ which(NMFC19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_TDMg3[ which(NMFC19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_TDMg2 <- rbind(NMFC19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
NMFC19_TDMft <- ftable(NMFC19_TDMg2$player1, NMFC19_TDMg2$player2)
NMFC19_TDMft2 <- as.matrix(NMFC19_TDMft)
numRows <- nrow(NMFC19_TDMft2)
numCols <- ncol(NMFC19_TDMft2)
NMFC19_TDMft3 <- NMFC19_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC19_TDMTable <- graph.adjacency(NMFC19_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(NMFC19_TDMTable, vertex.label = V(NMFC19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
NMFC19_TDM.clusterCoef <- transitivity(NMFC19_TDMTable, type="global") #cluster coefficient
NMFC19_TDM.degreeCent <- centralization.degree(NMFC19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_TDMftn <- as.network.matrix(NMFC19_TDMft)
NMFC19_TDM.netDensity <- network.density(NMFC19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_TDM.entropy <- entropy(NMFC19_TDMft) #entropy

NMFC19_TDM.netMx <- cbind(NMFC19_TDM.netMx, NMFC19_TDM.clusterCoef, NMFC19_TDM.degreeCent$centralization,
                          NMFC19_TDM.netDensity, NMFC19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
NMFC19_SDg2 <- data.frame(NMFC19_SD)
NMFC19_SDg2 <- NMFC19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_SDg2$player1
player2vector <- NMFC19_SDg2$player2
NMFC19_SDg3 <- NMFC19_SDg2
NMFC19_SDg3$p1inp2vec <- is.element(NMFC19_SDg3$player1, player2vector)
NMFC19_SDg3$p2inp1vec <- is.element(NMFC19_SDg3$player2, player1vector)

addPlayer1 <- NMFC19_SDg3[ which(NMFC19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_SDg3[ which(NMFC19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_SDg2 <- rbind(NMFC19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
NMFC19_SDft <- ftable(NMFC19_SDg2$player1, NMFC19_SDg2$player2)
NMFC19_SDft2 <- as.matrix(NMFC19_SDft)
numRows <- nrow(NMFC19_SDft2)
numCols <- ncol(NMFC19_SDft2)
NMFC19_SDft3 <- NMFC19_SDft2[c(2:numRows) , c(2:numCols)]
NMFC19_SDTable <- graph.adjacency(NMFC19_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(NMFC19_SDTable, vertex.label = V(NMFC19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
NMFC19_SD.clusterCoef <- transitivity(NMFC19_SDTable, type="global") #cluster coefficient
NMFC19_SD.degreeCent <- centralization.degree(NMFC19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_SDftn <- as.network.matrix(NMFC19_SDft)
NMFC19_SD.netDensity <- network.density(NMFC19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_SD.entropy <- entropy(NMFC19_SDft) #entropy

NMFC19_SD.netMx <- cbind(NMFC19_SD.netMx, NMFC19_SD.clusterCoef, NMFC19_SD.degreeCent$centralization,
                         NMFC19_SD.netDensity, NMFC19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
NMFC19_TDg2 <- data.frame(NMFC19_TD)
NMFC19_TDg2 <- NMFC19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_TDg2$player1
player2vector <- NMFC19_TDg2$player2
NMFC19_TDg3 <- NMFC19_TDg2
NMFC19_TDg3$p1inp2vec <- is.element(NMFC19_TDg3$player1, player2vector)
NMFC19_TDg3$p2inp1vec <- is.element(NMFC19_TDg3$player2, player1vector)

addPlayer1 <- NMFC19_TDg3[ which(NMFC19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_TDg3[ which(NMFC19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_TDg2 <- rbind(NMFC19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
NMFC19_TDft <- ftable(NMFC19_TDg2$player1, NMFC19_TDg2$player2)
NMFC19_TDft2 <- as.matrix(NMFC19_TDft)
numRows <- nrow(NMFC19_TDft2)
numCols <- ncol(NMFC19_TDft2)
NMFC19_TDft3 <- NMFC19_TDft2[c(2:numRows) , c(2:numCols)]
NMFC19_TDTable <- graph.adjacency(NMFC19_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(NMFC19_TDTable, vertex.label = V(NMFC19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
NMFC19_TD.clusterCoef <- transitivity(NMFC19_TDTable, type="global") #cluster coefficient
NMFC19_TD.degreeCent <- centralization.degree(NMFC19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_TDftn <- as.network.matrix(NMFC19_TDft)
NMFC19_TD.netDensity <- network.density(NMFC19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_TD.entropy <- entropy(NMFC19_TDft) #entropy

NMFC19_TD.netMx <- cbind(NMFC19_TD.netMx, NMFC19_TD.clusterCoef, NMFC19_TD.degreeCent$centralization,
                         NMFC19_TD.netDensity, NMFC19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
NMFC19_QTg2 <- data.frame(NMFC19_QT)
NMFC19_QTg2 <- NMFC19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC19_QTg2$player1
player2vector <- NMFC19_QTg2$player2
NMFC19_QTg3 <- NMFC19_QTg2
NMFC19_QTg3$p1inp2vec <- is.element(NMFC19_QTg3$player1, player2vector)
NMFC19_QTg3$p2inp1vec <- is.element(NMFC19_QTg3$player2, player1vector)

addPlayer1 <- NMFC19_QTg3[ which(NMFC19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC19_QTg3[ which(NMFC19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC19_QTg2 <- rbind(NMFC19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
NMFC19_QTft <- ftable(NMFC19_QTg2$player1, NMFC19_QTg2$player2)
NMFC19_QTft2 <- as.matrix(NMFC19_QTft)
numRows <- nrow(NMFC19_QTft2)
numCols <- ncol(NMFC19_QTft2)
NMFC19_QTft3 <- NMFC19_QTft2[c(2:numRows) , c(2:numCols)]
NMFC19_QTTable <- graph.adjacency(NMFC19_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(NMFC19_QTTable, vertex.label = V(NMFC19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
NMFC19_QT.clusterCoef <- transitivity(NMFC19_QTTable, type="global") #cluster coefficient
NMFC19_QT.degreeCent <- centralization.degree(NMFC19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC19_QTftn <- as.network.matrix(NMFC19_QTft)
NMFC19_QT.netDensity <- network.density(NMFC19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC19_QT.entropy <- entropy(NMFC19_QTft) #entropy

NMFC19_QT.netMx <- cbind(NMFC19_QT.netMx, NMFC19_QT.clusterCoef, NMFC19_QT.degreeCent$centralization,
                         NMFC19_QT.netDensity, NMFC19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC19_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "PORT"
KIoutcome = "Goal_F"
PORT19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
PORT19_Gg2 <- data.frame(PORT19_G)
PORT19_Gg2 <- PORT19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_Gg2$player1
player2vector <- PORT19_Gg2$player2
PORT19_Gg3 <- PORT19_Gg2
PORT19_Gg3$p1inp2vec <- is.element(PORT19_Gg3$player1, player2vector)
PORT19_Gg3$p2inp1vec <- is.element(PORT19_Gg3$player2, player1vector)

addPlayer1 <- PORT19_Gg3[ which(PORT19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT19_Gg2 <- rbind(PORT19_Gg2, addPlayer1)

#ROUND 19, Goal graph using weighted edges
PORT19_Gft <- ftable(PORT19_Gg2$player1, PORT19_Gg2$player2)
PORT19_Gft2 <- as.matrix(PORT19_Gft)
numRows <- nrow(PORT19_Gft2)
numCols <- ncol(PORT19_Gft2)
PORT19_Gft3 <- PORT19_Gft2[c(2:numRows) , c(1:numCols)]
PORT19_GTable <- graph.adjacency(PORT19_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(PORT19_GTable, vertex.label = V(PORT19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
PORT19_G.clusterCoef <- transitivity(PORT19_GTable, type="global") #cluster coefficient
PORT19_G.degreeCent <- centralization.degree(PORT19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_Gftn <- as.network.matrix(PORT19_Gft)
PORT19_G.netDensity <- network.density(PORT19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_G.entropy <- entropy(PORT19_Gft) #entropy

PORT19_G.netMx <- cbind(PORT19_G.netMx, PORT19_G.clusterCoef, PORT19_G.degreeCent$centralization,
                        PORT19_G.netDensity, PORT19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "PORT"
KIoutcome = "Behind_F"
PORT19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
PORT19_Bg2 <- data.frame(PORT19_B)
PORT19_Bg2 <- PORT19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_Bg2$player1
player2vector <- PORT19_Bg2$player2
PORT19_Bg3 <- PORT19_Bg2
PORT19_Bg3$p1inp2vec <- is.element(PORT19_Bg3$player1, player2vector)
PORT19_Bg3$p2inp1vec <- is.element(PORT19_Bg3$player2, player1vector)

addPlayer1 <- PORT19_Bg3[ which(PORT19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_Bg3[ which(PORT19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_Bg2 <- rbind(PORT19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
PORT19_Bft <- ftable(PORT19_Bg2$player1, PORT19_Bg2$player2)
PORT19_Bft2 <- as.matrix(PORT19_Bft)
numRows <- nrow(PORT19_Bft2)
numCols <- ncol(PORT19_Bft2)
PORT19_Bft3 <- PORT19_Bft2[c(2:numRows) , c(2:numCols)]
PORT19_BTable <- graph.adjacency(PORT19_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(PORT19_BTable, vertex.label = V(PORT19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
PORT19_B.clusterCoef <- transitivity(PORT19_BTable, type="global") #cluster coefficient
PORT19_B.degreeCent <- centralization.degree(PORT19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_Bftn <- as.network.matrix(PORT19_Bft)
PORT19_B.netDensity <- network.density(PORT19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_B.entropy <- entropy(PORT19_Bft) #entropy

PORT19_B.netMx <- cbind(PORT19_B.netMx, PORT19_B.clusterCoef, PORT19_B.degreeCent$centralization,
                        PORT19_B.netDensity, PORT19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
PORT19_SFg2 <- data.frame(PORT19_SF)
PORT19_SFg2 <- PORT19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_SFg2$player1
player2vector <- PORT19_SFg2$player2
PORT19_SFg3 <- PORT19_SFg2
PORT19_SFg3$p1inp2vec <- is.element(PORT19_SFg3$player1, player2vector)
PORT19_SFg3$p2inp1vec <- is.element(PORT19_SFg3$player2, player1vector)

addPlayer1 <- PORT19_SFg3[ which(PORT19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_SFg3[ which(PORT19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_SFg2 <- rbind(PORT19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
PORT19_SFft <- ftable(PORT19_SFg2$player1, PORT19_SFg2$player2)
PORT19_SFft2 <- as.matrix(PORT19_SFft)
numRows <- nrow(PORT19_SFft2)
numCols <- ncol(PORT19_SFft2)
PORT19_SFft3 <- PORT19_SFft2[c(2:numRows) , c(2:numCols)]
PORT19_SFTable <- graph.adjacency(PORT19_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(PORT19_SFTable, vertex.label = V(PORT19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
PORT19_SF.clusterCoef <- transitivity(PORT19_SFTable, type="global") #cluster coefficient
PORT19_SF.degreeCent <- centralization.degree(PORT19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_SFftn <- as.network.matrix(PORT19_SFft)
PORT19_SF.netDensity <- network.density(PORT19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_SF.entropy <- entropy(PORT19_SFft) #entropy

PORT19_SF.netMx <- cbind(PORT19_SF.netMx, PORT19_SF.clusterCoef, PORT19_SF.degreeCent$centralization,
                         PORT19_SF.netDensity, PORT19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
PORT19_TFg2 <- data.frame(PORT19_TF)
PORT19_TFg2 <- PORT19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_TFg2$player1
player2vector <- PORT19_TFg2$player2
PORT19_TFg3 <- PORT19_TFg2
PORT19_TFg3$p1inp2vec <- is.element(PORT19_TFg3$player1, player2vector)
PORT19_TFg3$p2inp1vec <- is.element(PORT19_TFg3$player2, player1vector)

addPlayer1 <- PORT19_TFg3[ which(PORT19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_TFg3[ which(PORT19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_TFg2 <- rbind(PORT19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
PORT19_TFft <- ftable(PORT19_TFg2$player1, PORT19_TFg2$player2)
PORT19_TFft2 <- as.matrix(PORT19_TFft)
numRows <- nrow(PORT19_TFft2)
numCols <- ncol(PORT19_TFft2)
PORT19_TFft3 <- PORT19_TFft2[c(2:numRows) , c(2:numCols)]
PORT19_TFTable <- graph.adjacency(PORT19_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(PORT19_TFTable, vertex.label = V(PORT19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
PORT19_TF.clusterCoef <- transitivity(PORT19_TFTable, type="global") #cluster coefficient
PORT19_TF.degreeCent <- centralization.degree(PORT19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_TFftn <- as.network.matrix(PORT19_TFft)
PORT19_TF.netDensity <- network.density(PORT19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_TF.entropy <- entropy(PORT19_TFft) #entropy

PORT19_TF.netMx <- cbind(PORT19_TF.netMx, PORT19_TF.clusterCoef, PORT19_TF.degreeCent$centralization,
                         PORT19_TF.netDensity, PORT19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
PORT19_SAMg2 <- data.frame(PORT19_SAM)
PORT19_SAMg2 <- PORT19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_SAMg2$player1
player2vector <- PORT19_SAMg2$player2
PORT19_SAMg3 <- PORT19_SAMg2
PORT19_SAMg3$p1inp2vec <- is.element(PORT19_SAMg3$player1, player2vector)
PORT19_SAMg3$p2inp1vec <- is.element(PORT19_SAMg3$player2, player1vector)

addPlayer1 <- PORT19_SAMg3[ which(PORT19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_SAMg3[ which(PORT19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_SAMg2 <- rbind(PORT19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
PORT19_SAMft <- ftable(PORT19_SAMg2$player1, PORT19_SAMg2$player2)
PORT19_SAMft2 <- as.matrix(PORT19_SAMft)
numRows <- nrow(PORT19_SAMft2)
numCols <- ncol(PORT19_SAMft2)
PORT19_SAMft3 <- PORT19_SAMft2[c(2:numRows) , c(2:numCols)]
PORT19_SAMTable <- graph.adjacency(PORT19_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(PORT19_SAMTable, vertex.label = V(PORT19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
PORT19_SAM.clusterCoef <- transitivity(PORT19_SAMTable, type="global") #cluster coefficient
PORT19_SAM.degreeCent <- centralization.degree(PORT19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_SAMftn <- as.network.matrix(PORT19_SAMft)
PORT19_SAM.netDensity <- network.density(PORT19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_SAM.entropy <- entropy(PORT19_SAMft) #entropy

PORT19_SAM.netMx <- cbind(PORT19_SAM.netMx, PORT19_SAM.clusterCoef, PORT19_SAM.degreeCent$centralization,
                          PORT19_SAM.netDensity, PORT19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
PORT19_TAMg2 <- data.frame(PORT19_TAM)
PORT19_TAMg2 <- PORT19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_TAMg2$player1
player2vector <- PORT19_TAMg2$player2
PORT19_TAMg3 <- PORT19_TAMg2
PORT19_TAMg3$p1inp2vec <- is.element(PORT19_TAMg3$player1, player2vector)
PORT19_TAMg3$p2inp1vec <- is.element(PORT19_TAMg3$player2, player1vector)

addPlayer1 <- PORT19_TAMg3[ which(PORT19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_TAMg3[ which(PORT19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_TAMg2 <- rbind(PORT19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
PORT19_TAMft <- ftable(PORT19_TAMg2$player1, PORT19_TAMg2$player2)
PORT19_TAMft2 <- as.matrix(PORT19_TAMft)
numRows <- nrow(PORT19_TAMft2)
numCols <- ncol(PORT19_TAMft2)
PORT19_TAMft3 <- PORT19_TAMft2[c(2:numRows) , c(2:numCols)]
PORT19_TAMTable <- graph.adjacency(PORT19_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(PORT19_TAMTable, vertex.label = V(PORT19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
PORT19_TAM.clusterCoef <- transitivity(PORT19_TAMTable, type="global") #cluster coefficient
PORT19_TAM.degreeCent <- centralization.degree(PORT19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_TAMftn <- as.network.matrix(PORT19_TAMft)
PORT19_TAM.netDensity <- network.density(PORT19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_TAM.entropy <- entropy(PORT19_TAMft) #entropy

PORT19_TAM.netMx <- cbind(PORT19_TAM.netMx, PORT19_TAM.clusterCoef, PORT19_TAM.degreeCent$centralization,
                          PORT19_TAM.netDensity, PORT19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************

round = 19
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
PORT19_SDMg2 <- data.frame(PORT19_SDM)
PORT19_SDMg2 <- PORT19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_SDMg2$player1
player2vector <- PORT19_SDMg2$player2
PORT19_SDMg3 <- PORT19_SDMg2
PORT19_SDMg3$p1inp2vec <- is.element(PORT19_SDMg3$player1, player2vector)
PORT19_SDMg3$p2inp1vec <- is.element(PORT19_SDMg3$player2, player1vector)

addPlayer1 <- PORT19_SDMg3[ which(PORT19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_SDMg3[ which(PORT19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_SDMg2 <- rbind(PORT19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
PORT19_SDMft <- ftable(PORT19_SDMg2$player1, PORT19_SDMg2$player2)
PORT19_SDMft2 <- as.matrix(PORT19_SDMft)
numRows <- nrow(PORT19_SDMft2)
numCols <- ncol(PORT19_SDMft2)
PORT19_SDMft3 <- PORT19_SDMft2[c(2:numRows) , c(2:numCols)]
PORT19_SDMTable <- graph.adjacency(PORT19_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(PORT19_SDMTable, vertex.label = V(PORT19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
PORT19_SDM.clusterCoef <- transitivity(PORT19_SDMTable, type="global") #cluster coefficient
PORT19_SDM.degreeCent <- centralization.degree(PORT19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_SDMftn <- as.network.matrix(PORT19_SDMft)
PORT19_SDM.netDensity <- network.density(PORT19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_SDM.entropy <- entropy(PORT19_SDMft) #entropy

PORT19_SDM.netMx <- cbind(PORT19_SDM.netMx, PORT19_SDM.clusterCoef, PORT19_SDM.degreeCent$centralization,
                          PORT19_SDM.netDensity, PORT19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
PORT19_TDMg2 <- data.frame(PORT19_TDM)
PORT19_TDMg2 <- PORT19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_TDMg2$player1
player2vector <- PORT19_TDMg2$player2
PORT19_TDMg3 <- PORT19_TDMg2
PORT19_TDMg3$p1inp2vec <- is.element(PORT19_TDMg3$player1, player2vector)
PORT19_TDMg3$p2inp1vec <- is.element(PORT19_TDMg3$player2, player1vector)

addPlayer1 <- PORT19_TDMg3[ which(PORT19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_TDMg3[ which(PORT19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_TDMg2 <- rbind(PORT19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
PORT19_TDMft <- ftable(PORT19_TDMg2$player1, PORT19_TDMg2$player2)
PORT19_TDMft2 <- as.matrix(PORT19_TDMft)
numRows <- nrow(PORT19_TDMft2)
numCols <- ncol(PORT19_TDMft2)
PORT19_TDMft3 <- PORT19_TDMft2[c(2:numRows) , c(2:numCols)]
PORT19_TDMTable <- graph.adjacency(PORT19_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(PORT19_TDMTable, vertex.label = V(PORT19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
PORT19_TDM.clusterCoef <- transitivity(PORT19_TDMTable, type="global") #cluster coefficient
PORT19_TDM.degreeCent <- centralization.degree(PORT19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_TDMftn <- as.network.matrix(PORT19_TDMft)
PORT19_TDM.netDensity <- network.density(PORT19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_TDM.entropy <- entropy(PORT19_TDMft) #entropy

PORT19_TDM.netMx <- cbind(PORT19_TDM.netMx, PORT19_TDM.clusterCoef, PORT19_TDM.degreeCent$centralization,
                          PORT19_TDM.netDensity, PORT19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
PORT19_SDg2 <- data.frame(PORT19_SD)
PORT19_SDg2 <- PORT19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_SDg2$player1
player2vector <- PORT19_SDg2$player2
PORT19_SDg3 <- PORT19_SDg2
PORT19_SDg3$p1inp2vec <- is.element(PORT19_SDg3$player1, player2vector)
PORT19_SDg3$p2inp1vec <- is.element(PORT19_SDg3$player2, player1vector)

addPlayer1 <- PORT19_SDg3[ which(PORT19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_SDg3[ which(PORT19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_SDg2 <- rbind(PORT19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
PORT19_SDft <- ftable(PORT19_SDg2$player1, PORT19_SDg2$player2)
PORT19_SDft2 <- as.matrix(PORT19_SDft)
numRows <- nrow(PORT19_SDft2)
numCols <- ncol(PORT19_SDft2)
PORT19_SDft3 <- PORT19_SDft2[c(2:numRows) , c(2:numCols)]
PORT19_SDTable <- graph.adjacency(PORT19_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(PORT19_SDTable, vertex.label = V(PORT19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
PORT19_SD.clusterCoef <- transitivity(PORT19_SDTable, type="global") #cluster coefficient
PORT19_SD.degreeCent <- centralization.degree(PORT19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_SDftn <- as.network.matrix(PORT19_SDft)
PORT19_SD.netDensity <- network.density(PORT19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_SD.entropy <- entropy(PORT19_SDft) #entropy

PORT19_SD.netMx <- cbind(PORT19_SD.netMx, PORT19_SD.clusterCoef, PORT19_SD.degreeCent$centralization,
                         PORT19_SD.netDensity, PORT19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************

round = 19
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
PORT19_TDg2 <- data.frame(PORT19_TD)
PORT19_TDg2 <- PORT19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_TDg2$player1
player2vector <- PORT19_TDg2$player2
PORT19_TDg3 <- PORT19_TDg2
PORT19_TDg3$p1inp2vec <- is.element(PORT19_TDg3$player1, player2vector)
PORT19_TDg3$p2inp1vec <- is.element(PORT19_TDg3$player2, player1vector)

addPlayer1 <- PORT19_TDg3[ which(PORT19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_TDg3[ which(PORT19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_TDg2 <- rbind(PORT19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
PORT19_TDft <- ftable(PORT19_TDg2$player1, PORT19_TDg2$player2)
PORT19_TDft2 <- as.matrix(PORT19_TDft)
numRows <- nrow(PORT19_TDft2)
numCols <- ncol(PORT19_TDft2)
PORT19_TDft3 <- PORT19_TDft2[c(2:numRows) , c(2:numCols)]
PORT19_TDTable <- graph.adjacency(PORT19_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(PORT19_TDTable, vertex.label = V(PORT19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
PORT19_TD.clusterCoef <- transitivity(PORT19_TDTable, type="global") #cluster coefficient
PORT19_TD.degreeCent <- centralization.degree(PORT19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_TDftn <- as.network.matrix(PORT19_TDft)
PORT19_TD.netDensity <- network.density(PORT19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_TD.entropy <- entropy(PORT19_TDft) #entropy

PORT19_TD.netMx <- cbind(PORT19_TD.netMx, PORT19_TD.clusterCoef, PORT19_TD.degreeCent$centralization,
                         PORT19_TD.netDensity, PORT19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************

round = 19
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
PORT19_QTg2 <- data.frame(PORT19_QT)
PORT19_QTg2 <- PORT19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT19_QTg2$player1
player2vector <- PORT19_QTg2$player2
PORT19_QTg3 <- PORT19_QTg2
PORT19_QTg3$p1inp2vec <- is.element(PORT19_QTg3$player1, player2vector)
PORT19_QTg3$p2inp1vec <- is.element(PORT19_QTg3$player2, player1vector)

addPlayer1 <- PORT19_QTg3[ which(PORT19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT19_QTg3[ which(PORT19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT19_QTg2 <- rbind(PORT19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
PORT19_QTft <- ftable(PORT19_QTg2$player1, PORT19_QTg2$player2)
PORT19_QTft2 <- as.matrix(PORT19_QTft)
numRows <- nrow(PORT19_QTft2)
numCols <- ncol(PORT19_QTft2)
PORT19_QTft3 <- PORT19_QTft2[c(2:numRows) , c(2:numCols)]
PORT19_QTTable <- graph.adjacency(PORT19_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(PORT19_QTTable, vertex.label = V(PORT19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
PORT19_QT.clusterCoef <- transitivity(PORT19_QTTable, type="global") #cluster coefficient
PORT19_QT.degreeCent <- centralization.degree(PORT19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT19_QTftn <- as.network.matrix(PORT19_QTft)
PORT19_QT.netDensity <- network.density(PORT19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT19_QT.entropy <- entropy(PORT19_QTft) #entropy

PORT19_QT.netMx <- cbind(PORT19_QT.netMx, PORT19_QT.clusterCoef, PORT19_QT.degreeCent$centralization,
                         PORT19_QT.netDensity, PORT19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT19_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "RICH"
KIoutcome = "Goal_F"
RICH19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
RICH19_Gg2 <- data.frame(RICH19_G)
RICH19_Gg2 <- RICH19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_Gg2$player1
player2vector <- RICH19_Gg2$player2
RICH19_Gg3 <- RICH19_Gg2
RICH19_Gg3$p1inp2vec <- is.element(RICH19_Gg3$player1, player2vector)
RICH19_Gg3$p2inp1vec <- is.element(RICH19_Gg3$player2, player1vector)

addPlayer1 <- RICH19_Gg3[ which(RICH19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_Gg3[ which(RICH19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_Gg2 <- rbind(RICH19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
RICH19_Gft <- ftable(RICH19_Gg2$player1, RICH19_Gg2$player2)
RICH19_Gft2 <- as.matrix(RICH19_Gft)
numRows <- nrow(RICH19_Gft2)
numCols <- ncol(RICH19_Gft2)
RICH19_Gft3 <- RICH19_Gft2[c(2:numRows) , c(2:numCols)]
RICH19_GTable <- graph.adjacency(RICH19_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(RICH19_GTable, vertex.label = V(RICH19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
RICH19_G.clusterCoef <- transitivity(RICH19_GTable, type="global") #cluster coefficient
RICH19_G.degreeCent <- centralization.degree(RICH19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_Gftn <- as.network.matrix(RICH19_Gft)
RICH19_G.netDensity <- network.density(RICH19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_G.entropy <- entropy(RICH19_Gft) #entropy

RICH19_G.netMx <- cbind(RICH19_G.netMx, RICH19_G.clusterCoef, RICH19_G.degreeCent$centralization,
                        RICH19_G.netDensity, RICH19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************

round = 19
teamName = "RICH"
KIoutcome = "Behind_F"
RICH19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
RICH19_Bg2 <- data.frame(RICH19_B)
RICH19_Bg2 <- RICH19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_Bg2$player1
player2vector <- RICH19_Bg2$player2
RICH19_Bg3 <- RICH19_Bg2
RICH19_Bg3$p1inp2vec <- is.element(RICH19_Bg3$player1, player2vector)
RICH19_Bg3$p2inp1vec <- is.element(RICH19_Bg3$player2, player1vector)

addPlayer1 <- RICH19_Bg3[ which(RICH19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_Bg3[ which(RICH19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_Bg2 <- rbind(RICH19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
RICH19_Bft <- ftable(RICH19_Bg2$player1, RICH19_Bg2$player2)
RICH19_Bft2 <- as.matrix(RICH19_Bft)
numRows <- nrow(RICH19_Bft2)
numCols <- ncol(RICH19_Bft2)
RICH19_Bft3 <- RICH19_Bft2[c(2:numRows) , c(2:numCols)]
RICH19_BTable <- graph.adjacency(RICH19_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(RICH19_BTable, vertex.label = V(RICH19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
RICH19_B.clusterCoef <- transitivity(RICH19_BTable, type="global") #cluster coefficient
RICH19_B.degreeCent <- centralization.degree(RICH19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_Bftn <- as.network.matrix(RICH19_Bft)
RICH19_B.netDensity <- network.density(RICH19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_B.entropy <- entropy(RICH19_Bft) #entropy

RICH19_B.netMx <- cbind(RICH19_B.netMx, RICH19_B.clusterCoef, RICH19_B.degreeCent$centralization,
                        RICH19_B.netDensity, RICH19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************

round = 19
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
RICH19_SFg2 <- data.frame(RICH19_SF)
RICH19_SFg2 <- RICH19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_SFg2$player1
player2vector <- RICH19_SFg2$player2
RICH19_SFg3 <- RICH19_SFg2
RICH19_SFg3$p1inp2vec <- is.element(RICH19_SFg3$player1, player2vector)
RICH19_SFg3$p2inp1vec <- is.element(RICH19_SFg3$player2, player1vector)

addPlayer1 <- RICH19_SFg3[ which(RICH19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_SFg3[ which(RICH19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_SFg2 <- rbind(RICH19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
RICH19_SFft <- ftable(RICH19_SFg2$player1, RICH19_SFg2$player2)
RICH19_SFft2 <- as.matrix(RICH19_SFft)
numRows <- nrow(RICH19_SFft2)
numCols <- ncol(RICH19_SFft2)
RICH19_SFft3 <- RICH19_SFft2[c(2:numRows) , c(2:numCols)]
RICH19_SFTable <- graph.adjacency(RICH19_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(RICH19_SFTable, vertex.label = V(RICH19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
RICH19_SF.clusterCoef <- transitivity(RICH19_SFTable, type="global") #cluster coefficient
RICH19_SF.degreeCent <- centralization.degree(RICH19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_SFftn <- as.network.matrix(RICH19_SFft)
RICH19_SF.netDensity <- network.density(RICH19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_SF.entropy <- entropy(RICH19_SFft) #entropy

RICH19_SF.netMx <- cbind(RICH19_SF.netMx, RICH19_SF.clusterCoef, RICH19_SF.degreeCent$centralization,
                         RICH19_SF.netDensity, RICH19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
RICH19_TFg2 <- data.frame(RICH19_TF)
RICH19_TFg2 <- RICH19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_TFg2$player1
player2vector <- RICH19_TFg2$player2
RICH19_TFg3 <- RICH19_TFg2
RICH19_TFg3$p1inp2vec <- is.element(RICH19_TFg3$player1, player2vector)
RICH19_TFg3$p2inp1vec <- is.element(RICH19_TFg3$player2, player1vector)

addPlayer1 <- RICH19_TFg3[ which(RICH19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_TFg3[ which(RICH19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_TFg2 <- rbind(RICH19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
RICH19_TFft <- ftable(RICH19_TFg2$player1, RICH19_TFg2$player2)
RICH19_TFft2 <- as.matrix(RICH19_TFft)
numRows <- nrow(RICH19_TFft2)
numCols <- ncol(RICH19_TFft2)
RICH19_TFft3 <- RICH19_TFft2[c(2:numRows) , c(2:numCols)]
RICH19_TFTable <- graph.adjacency(RICH19_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(RICH19_TFTable, vertex.label = V(RICH19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
RICH19_TF.clusterCoef <- transitivity(RICH19_TFTable, type="global") #cluster coefficient
RICH19_TF.degreeCent <- centralization.degree(RICH19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_TFftn <- as.network.matrix(RICH19_TFft)
RICH19_TF.netDensity <- network.density(RICH19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_TF.entropy <- entropy(RICH19_TFft) #entropy

RICH19_TF.netMx <- cbind(RICH19_TF.netMx, RICH19_TF.clusterCoef, RICH19_TF.degreeCent$centralization,
                         RICH19_TF.netDensity, RICH19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
RICH19_SAMg2 <- data.frame(RICH19_SAM)
RICH19_SAMg2 <- RICH19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_SAMg2$player1
player2vector <- RICH19_SAMg2$player2
RICH19_SAMg3 <- RICH19_SAMg2
RICH19_SAMg3$p1inp2vec <- is.element(RICH19_SAMg3$player1, player2vector)
RICH19_SAMg3$p2inp1vec <- is.element(RICH19_SAMg3$player2, player1vector)

addPlayer1 <- RICH19_SAMg3[ which(RICH19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_SAMg3[ which(RICH19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_SAMg2 <- rbind(RICH19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
RICH19_SAMft <- ftable(RICH19_SAMg2$player1, RICH19_SAMg2$player2)
RICH19_SAMft2 <- as.matrix(RICH19_SAMft)
numRows <- nrow(RICH19_SAMft2)
numCols <- ncol(RICH19_SAMft2)
RICH19_SAMft3 <- RICH19_SAMft2[c(2:numRows) , c(2:numCols)]
RICH19_SAMTable <- graph.adjacency(RICH19_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(RICH19_SAMTable, vertex.label = V(RICH19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
RICH19_SAM.clusterCoef <- transitivity(RICH19_SAMTable, type="global") #cluster coefficient
RICH19_SAM.degreeCent <- centralization.degree(RICH19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_SAMftn <- as.network.matrix(RICH19_SAMft)
RICH19_SAM.netDensity <- network.density(RICH19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_SAM.entropy <- entropy(RICH19_SAMft) #entropy

RICH19_SAM.netMx <- cbind(RICH19_SAM.netMx, RICH19_SAM.clusterCoef, RICH19_SAM.degreeCent$centralization,
                          RICH19_SAM.netDensity, RICH19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
RICH19_TAMg2 <- data.frame(RICH19_TAM)
RICH19_TAMg2 <- RICH19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_TAMg2$player1
player2vector <- RICH19_TAMg2$player2
RICH19_TAMg3 <- RICH19_TAMg2
RICH19_TAMg3$p1inp2vec <- is.element(RICH19_TAMg3$player1, player2vector)
RICH19_TAMg3$p2inp1vec <- is.element(RICH19_TAMg3$player2, player1vector)

addPlayer1 <- RICH19_TAMg3[ which(RICH19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_TAMg3[ which(RICH19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_TAMg2 <- rbind(RICH19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
RICH19_TAMft <- ftable(RICH19_TAMg2$player1, RICH19_TAMg2$player2)
RICH19_TAMft2 <- as.matrix(RICH19_TAMft)
numRows <- nrow(RICH19_TAMft2)
numCols <- ncol(RICH19_TAMft2)
RICH19_TAMft3 <- RICH19_TAMft2[c(2:numRows) , c(2:numCols)]
RICH19_TAMTable <- graph.adjacency(RICH19_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(RICH19_TAMTable, vertex.label = V(RICH19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
RICH19_TAM.clusterCoef <- transitivity(RICH19_TAMTable, type="global") #cluster coefficient
RICH19_TAM.degreeCent <- centralization.degree(RICH19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_TAMftn <- as.network.matrix(RICH19_TAMft)
RICH19_TAM.netDensity <- network.density(RICH19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_TAM.entropy <- entropy(RICH19_TAMft) #entropy

RICH19_TAM.netMx <- cbind(RICH19_TAM.netMx, RICH19_TAM.clusterCoef, RICH19_TAM.degreeCent$centralization,
                          RICH19_TAM.netDensity, RICH19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************
#NA

round = 19
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
RICH19_SDMg2 <- data.frame(RICH19_SDM)
RICH19_SDMg2 <- RICH19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_SDMg2$player1
player2vector <- RICH19_SDMg2$player2
RICH19_SDMg3 <- RICH19_SDMg2
RICH19_SDMg3$p1inp2vec <- is.element(RICH19_SDMg3$player1, player2vector)
RICH19_SDMg3$p2inp1vec <- is.element(RICH19_SDMg3$player2, player1vector)

addPlayer1 <- RICH19_SDMg3[ which(RICH19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_SDMg3[ which(RICH19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_SDMg2 <- rbind(RICH19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
RICH19_SDMft <- ftable(RICH19_SDMg2$player1, RICH19_SDMg2$player2)
RICH19_SDMft2 <- as.matrix(RICH19_SDMft)
numRows <- nrow(RICH19_SDMft2)
numCols <- ncol(RICH19_SDMft2)
RICH19_SDMft3 <- RICH19_SDMft2[c(2:numRows) , c(2:numCols)]
RICH19_SDMTable <- graph.adjacency(RICH19_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(RICH19_SDMTable, vertex.label = V(RICH19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
RICH19_SDM.clusterCoef <- transitivity(RICH19_SDMTable, type="global") #cluster coefficient
RICH19_SDM.degreeCent <- centralization.degree(RICH19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_SDMftn <- as.network.matrix(RICH19_SDMft)
RICH19_SDM.netDensity <- network.density(RICH19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_SDM.entropy <- entropy(RICH19_SDMft) #entropy

RICH19_SDM.netMx <- cbind(RICH19_SDM.netMx, RICH19_SDM.clusterCoef, RICH19_SDM.degreeCent$centralization,
                          RICH19_SDM.netDensity, RICH19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
RICH19_TDMg2 <- data.frame(RICH19_TDM)
RICH19_TDMg2 <- RICH19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_TDMg2$player1
player2vector <- RICH19_TDMg2$player2
RICH19_TDMg3 <- RICH19_TDMg2
RICH19_TDMg3$p1inp2vec <- is.element(RICH19_TDMg3$player1, player2vector)
RICH19_TDMg3$p2inp1vec <- is.element(RICH19_TDMg3$player2, player1vector)

addPlayer1 <- RICH19_TDMg3[ which(RICH19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_TDMg3[ which(RICH19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_TDMg2 <- rbind(RICH19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
RICH19_TDMft <- ftable(RICH19_TDMg2$player1, RICH19_TDMg2$player2)
RICH19_TDMft2 <- as.matrix(RICH19_TDMft)
numRows <- nrow(RICH19_TDMft2)
numCols <- ncol(RICH19_TDMft2)
RICH19_TDMft3 <- RICH19_TDMft2[c(2:numRows) , c(2:numCols)]
RICH19_TDMTable <- graph.adjacency(RICH19_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(RICH19_TDMTable, vertex.label = V(RICH19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
RICH19_TDM.clusterCoef <- transitivity(RICH19_TDMTable, type="global") #cluster coefficient
RICH19_TDM.degreeCent <- centralization.degree(RICH19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_TDMftn <- as.network.matrix(RICH19_TDMft)
RICH19_TDM.netDensity <- network.density(RICH19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_TDM.entropy <- entropy(RICH19_TDMft) #entropy

RICH19_TDM.netMx <- cbind(RICH19_TDM.netMx, RICH19_TDM.clusterCoef, RICH19_TDM.degreeCent$centralization,
                          RICH19_TDM.netDensity, RICH19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
RICH19_SDg2 <- data.frame(RICH19_SD)
RICH19_SDg2 <- RICH19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_SDg2$player1
player2vector <- RICH19_SDg2$player2
RICH19_SDg3 <- RICH19_SDg2
RICH19_SDg3$p1inp2vec <- is.element(RICH19_SDg3$player1, player2vector)
RICH19_SDg3$p2inp1vec <- is.element(RICH19_SDg3$player2, player1vector)

addPlayer1 <- RICH19_SDg3[ which(RICH19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_SDg3[ which(RICH19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_SDg2 <- rbind(RICH19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
RICH19_SDft <- ftable(RICH19_SDg2$player1, RICH19_SDg2$player2)
RICH19_SDft2 <- as.matrix(RICH19_SDft)
numRows <- nrow(RICH19_SDft2)
numCols <- ncol(RICH19_SDft2)
RICH19_SDft3 <- RICH19_SDft2[c(2:numRows) , c(2:numCols)]
RICH19_SDTable <- graph.adjacency(RICH19_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(RICH19_SDTable, vertex.label = V(RICH19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
RICH19_SD.clusterCoef <- transitivity(RICH19_SDTable, type="global") #cluster coefficient
RICH19_SD.degreeCent <- centralization.degree(RICH19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_SDftn <- as.network.matrix(RICH19_SDft)
RICH19_SD.netDensity <- network.density(RICH19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_SD.entropy <- entropy(RICH19_SDft) #entropy

RICH19_SD.netMx <- cbind(RICH19_SD.netMx, RICH19_SD.clusterCoef, RICH19_SD.degreeCent$centralization,
                         RICH19_SD.netDensity, RICH19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
RICH19_TDg2 <- data.frame(RICH19_TD)
RICH19_TDg2 <- RICH19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_TDg2$player1
player2vector <- RICH19_TDg2$player2
RICH19_TDg3 <- RICH19_TDg2
RICH19_TDg3$p1inp2vec <- is.element(RICH19_TDg3$player1, player2vector)
RICH19_TDg3$p2inp1vec <- is.element(RICH19_TDg3$player2, player1vector)

addPlayer1 <- RICH19_TDg3[ which(RICH19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_TDg3[ which(RICH19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_TDg2 <- rbind(RICH19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
RICH19_TDft <- ftable(RICH19_TDg2$player1, RICH19_TDg2$player2)
RICH19_TDft2 <- as.matrix(RICH19_TDft)
numRows <- nrow(RICH19_TDft2)
numCols <- ncol(RICH19_TDft2)
RICH19_TDft3 <- RICH19_TDft2[c(2:numRows) , c(2:numCols)]
RICH19_TDTable <- graph.adjacency(RICH19_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(RICH19_TDTable, vertex.label = V(RICH19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
RICH19_TD.clusterCoef <- transitivity(RICH19_TDTable, type="global") #cluster coefficient
RICH19_TD.degreeCent <- centralization.degree(RICH19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_TDftn <- as.network.matrix(RICH19_TDft)
RICH19_TD.netDensity <- network.density(RICH19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_TD.entropy <- entropy(RICH19_TDft) #entropy

RICH19_TD.netMx <- cbind(RICH19_TD.netMx, RICH19_TD.clusterCoef, RICH19_TD.degreeCent$centralization,
                         RICH19_TD.netDensity, RICH19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
RICH19_QTg2 <- data.frame(RICH19_QT)
RICH19_QTg2 <- RICH19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH19_QTg2$player1
player2vector <- RICH19_QTg2$player2
RICH19_QTg3 <- RICH19_QTg2
RICH19_QTg3$p1inp2vec <- is.element(RICH19_QTg3$player1, player2vector)
RICH19_QTg3$p2inp1vec <- is.element(RICH19_QTg3$player2, player1vector)

addPlayer1 <- RICH19_QTg3[ which(RICH19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH19_QTg3[ which(RICH19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH19_QTg2 <- rbind(RICH19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
RICH19_QTft <- ftable(RICH19_QTg2$player1, RICH19_QTg2$player2)
RICH19_QTft2 <- as.matrix(RICH19_QTft)
numRows <- nrow(RICH19_QTft2)
numCols <- ncol(RICH19_QTft2)
RICH19_QTft3 <- RICH19_QTft2[c(2:numRows) , c(2:numCols)]
RICH19_QTTable <- graph.adjacency(RICH19_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(RICH19_QTTable, vertex.label = V(RICH19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
RICH19_QT.clusterCoef <- transitivity(RICH19_QTTable, type="global") #cluster coefficient
RICH19_QT.degreeCent <- centralization.degree(RICH19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH19_QTftn <- as.network.matrix(RICH19_QTft)
RICH19_QT.netDensity <- network.density(RICH19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH19_QT.entropy <- entropy(RICH19_QTft) #entropy

RICH19_QT.netMx <- cbind(RICH19_QT.netMx, RICH19_QT.clusterCoef, RICH19_QT.degreeCent$centralization,
                         RICH19_QT.netDensity, RICH19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH19_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "STK"
KIoutcome = "Goal_F"
STK19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
STK19_Gg2 <- data.frame(STK19_G)
STK19_Gg2 <- STK19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_Gg2$player1
player2vector <- STK19_Gg2$player2
STK19_Gg3 <- STK19_Gg2
STK19_Gg3$p1inp2vec <- is.element(STK19_Gg3$player1, player2vector)
STK19_Gg3$p2inp1vec <- is.element(STK19_Gg3$player2, player1vector)

addPlayer1 <- STK19_Gg3[ which(STK19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_Gg3[ which(STK19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_Gg2 <- rbind(STK19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
STK19_Gft <- ftable(STK19_Gg2$player1, STK19_Gg2$player2)
STK19_Gft2 <- as.matrix(STK19_Gft)
numRows <- nrow(STK19_Gft2)
numCols <- ncol(STK19_Gft2)
STK19_Gft3 <- STK19_Gft2[c(2:numRows) , c(2:numCols)]
STK19_GTable <- graph.adjacency(STK19_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(STK19_GTable, vertex.label = V(STK19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
STK19_G.clusterCoef <- transitivity(STK19_GTable, type="global") #cluster coefficient
STK19_G.degreeCent <- centralization.degree(STK19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_Gftn <- as.network.matrix(STK19_Gft)
STK19_G.netDensity <- network.density(STK19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_G.entropy <- entropy(STK19_Gft) #entropy

STK19_G.netMx <- cbind(STK19_G.netMx, STK19_G.clusterCoef, STK19_G.degreeCent$centralization,
                       STK19_G.netDensity, STK19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "STK"
KIoutcome = "Behind_F"
STK19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
STK19_Bg2 <- data.frame(STK19_B)
STK19_Bg2 <- STK19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_Bg2$player1
player2vector <- STK19_Bg2$player2
STK19_Bg3 <- STK19_Bg2
STK19_Bg3$p1inp2vec <- is.element(STK19_Bg3$player1, player2vector)
STK19_Bg3$p2inp1vec <- is.element(STK19_Bg3$player2, player1vector)

addPlayer1 <- STK19_Bg3[ which(STK19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_Bg3[ which(STK19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_Bg2 <- rbind(STK19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
STK19_Bft <- ftable(STK19_Bg2$player1, STK19_Bg2$player2)
STK19_Bft2 <- as.matrix(STK19_Bft)
numRows <- nrow(STK19_Bft2)
numCols <- ncol(STK19_Bft2)
STK19_Bft3 <- STK19_Bft2[c(2:numRows) , c(2:numCols)]
STK19_BTable <- graph.adjacency(STK19_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(STK19_BTable, vertex.label = V(STK19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
STK19_B.clusterCoef <- transitivity(STK19_BTable, type="global") #cluster coefficient
STK19_B.degreeCent <- centralization.degree(STK19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_Bftn <- as.network.matrix(STK19_Bft)
STK19_B.netDensity <- network.density(STK19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_B.entropy <- entropy(STK19_Bft) #entropy

STK19_B.netMx <- cbind(STK19_B.netMx, STK19_B.clusterCoef, STK19_B.degreeCent$centralization,
                       STK19_B.netDensity, STK19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "STK"
KIoutcome = "Stoppage_F"
STK19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
STK19_SFg2 <- data.frame(STK19_SF)
STK19_SFg2 <- STK19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_SFg2$player1
player2vector <- STK19_SFg2$player2
STK19_SFg3 <- STK19_SFg2
STK19_SFg3$p1inp2vec <- is.element(STK19_SFg3$player1, player2vector)
STK19_SFg3$p2inp1vec <- is.element(STK19_SFg3$player2, player1vector)

addPlayer1 <- STK19_SFg3[ which(STK19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_SFg3[ which(STK19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_SFg2 <- rbind(STK19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
STK19_SFft <- ftable(STK19_SFg2$player1, STK19_SFg2$player2)
STK19_SFft2 <- as.matrix(STK19_SFft)
numRows <- nrow(STK19_SFft2)
numCols <- ncol(STK19_SFft2)
STK19_SFft3 <- STK19_SFft2[c(2:numRows) , c(2:numCols)]
STK19_SFTable <- graph.adjacency(STK19_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(STK19_SFTable, vertex.label = V(STK19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
STK19_SF.clusterCoef <- transitivity(STK19_SFTable, type="global") #cluster coefficient
STK19_SF.degreeCent <- centralization.degree(STK19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_SFftn <- as.network.matrix(STK19_SFft)
STK19_SF.netDensity <- network.density(STK19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_SF.entropy <- entropy(STK19_SFft) #entropy

STK19_SF.netMx <- cbind(STK19_SF.netMx, STK19_SF.clusterCoef, STK19_SF.degreeCent$centralization,
                        STK19_SF.netDensity, STK19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "STK"
KIoutcome = "Turnover_F"
STK19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
STK19_TFg2 <- data.frame(STK19_TF)
STK19_TFg2 <- STK19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_TFg2$player1
player2vector <- STK19_TFg2$player2
STK19_TFg3 <- STK19_TFg2
STK19_TFg3$p1inp2vec <- is.element(STK19_TFg3$player1, player2vector)
STK19_TFg3$p2inp1vec <- is.element(STK19_TFg3$player2, player1vector)

addPlayer1 <- STK19_TFg3[ which(STK19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_TFg3[ which(STK19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_TFg2 <- rbind(STK19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
STK19_TFft <- ftable(STK19_TFg2$player1, STK19_TFg2$player2)
STK19_TFft2 <- as.matrix(STK19_TFft)
numRows <- nrow(STK19_TFft2)
numCols <- ncol(STK19_TFft2)
STK19_TFft3 <- STK19_TFft2[c(2:numRows) , c(2:numCols)]
STK19_TFTable <- graph.adjacency(STK19_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(STK19_TFTable, vertex.label = V(STK19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
STK19_TF.clusterCoef <- transitivity(STK19_TFTable, type="global") #cluster coefficient
STK19_TF.degreeCent <- centralization.degree(STK19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_TFftn <- as.network.matrix(STK19_TFft)
STK19_TF.netDensity <- network.density(STK19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_TF.entropy <- entropy(STK19_TFft) #entropy

STK19_TF.netMx <- cbind(STK19_TF.netMx, STK19_TF.clusterCoef, STK19_TF.degreeCent$centralization,
                        STK19_TF.netDensity, STK19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
STK19_SAMg2 <- data.frame(STK19_SAM)
STK19_SAMg2 <- STK19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_SAMg2$player1
player2vector <- STK19_SAMg2$player2
STK19_SAMg3 <- STK19_SAMg2
STK19_SAMg3$p1inp2vec <- is.element(STK19_SAMg3$player1, player2vector)
STK19_SAMg3$p2inp1vec <- is.element(STK19_SAMg3$player2, player1vector)

addPlayer1 <- STK19_SAMg3[ which(STK19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_SAMg3[ which(STK19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_SAMg2 <- rbind(STK19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
STK19_SAMft <- ftable(STK19_SAMg2$player1, STK19_SAMg2$player2)
STK19_SAMft2 <- as.matrix(STK19_SAMft)
numRows <- nrow(STK19_SAMft2)
numCols <- ncol(STK19_SAMft2)
STK19_SAMft3 <- STK19_SAMft2[c(2:numRows) , c(2:numCols)]
STK19_SAMTable <- graph.adjacency(STK19_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(STK19_SAMTable, vertex.label = V(STK19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
STK19_SAM.clusterCoef <- transitivity(STK19_SAMTable, type="global") #cluster coefficient
STK19_SAM.degreeCent <- centralization.degree(STK19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_SAMftn <- as.network.matrix(STK19_SAMft)
STK19_SAM.netDensity <- network.density(STK19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_SAM.entropy <- entropy(STK19_SAMft) #entropy

STK19_SAM.netMx <- cbind(STK19_SAM.netMx, STK19_SAM.clusterCoef, STK19_SAM.degreeCent$centralization,
                         STK19_SAM.netDensity, STK19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "STK"
KIoutcome = "Turnover_AM"
STK19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
STK19_TAMg2 <- data.frame(STK19_TAM)
STK19_TAMg2 <- STK19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_TAMg2$player1
player2vector <- STK19_TAMg2$player2
STK19_TAMg3 <- STK19_TAMg2
STK19_TAMg3$p1inp2vec <- is.element(STK19_TAMg3$player1, player2vector)
STK19_TAMg3$p2inp1vec <- is.element(STK19_TAMg3$player2, player1vector)

addPlayer1 <- STK19_TAMg3[ which(STK19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_TAMg3[ which(STK19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_TAMg2 <- rbind(STK19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
STK19_TAMft <- ftable(STK19_TAMg2$player1, STK19_TAMg2$player2)
STK19_TAMft2 <- as.matrix(STK19_TAMft)
numRows <- nrow(STK19_TAMft2)
numCols <- ncol(STK19_TAMft2)
STK19_TAMft3 <- STK19_TAMft2[c(2:numRows) , c(2:numCols)]
STK19_TAMTable <- graph.adjacency(STK19_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(STK19_TAMTable, vertex.label = V(STK19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
STK19_TAM.clusterCoef <- transitivity(STK19_TAMTable, type="global") #cluster coefficient
STK19_TAM.degreeCent <- centralization.degree(STK19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_TAMftn <- as.network.matrix(STK19_TAMft)
STK19_TAM.netDensity <- network.density(STK19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_TAM.entropy <- entropy(STK19_TAMft) #entropy

STK19_TAM.netMx <- cbind(STK19_TAM.netMx, STK19_TAM.clusterCoef, STK19_TAM.degreeCent$centralization,
                         STK19_TAM.netDensity, STK19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************
#NA

round = 19
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
STK19_SDMg2 <- data.frame(STK19_SDM)
STK19_SDMg2 <- STK19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_SDMg2$player1
player2vector <- STK19_SDMg2$player2
STK19_SDMg3 <- STK19_SDMg2
STK19_SDMg3$p1inp2vec <- is.element(STK19_SDMg3$player1, player2vector)
STK19_SDMg3$p2inp1vec <- is.element(STK19_SDMg3$player2, player1vector)

addPlayer1 <- STK19_SDMg3[ which(STK19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_SDMg3[ which(STK19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_SDMg2 <- rbind(STK19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
STK19_SDMft <- ftable(STK19_SDMg2$player1, STK19_SDMg2$player2)
STK19_SDMft2 <- as.matrix(STK19_SDMft)
numRows <- nrow(STK19_SDMft2)
numCols <- ncol(STK19_SDMft2)
STK19_SDMft3 <- STK19_SDMft2[c(2:numRows) , c(2:numCols)]
STK19_SDMTable <- graph.adjacency(STK19_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(STK19_SDMTable, vertex.label = V(STK19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
STK19_SDM.clusterCoef <- transitivity(STK19_SDMTable, type="global") #cluster coefficient
STK19_SDM.degreeCent <- centralization.degree(STK19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_SDMftn <- as.network.matrix(STK19_SDMft)
STK19_SDM.netDensity <- network.density(STK19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_SDM.entropy <- entropy(STK19_SDMft) #entropy

STK19_SDM.netMx <- cbind(STK19_SDM.netMx, STK19_SDM.clusterCoef, STK19_SDM.degreeCent$centralization,
                         STK19_SDM.netDensity, STK19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "STK"
KIoutcome = "Turnover_DM"
STK19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
STK19_TDMg2 <- data.frame(STK19_TDM)
STK19_TDMg2 <- STK19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_TDMg2$player1
player2vector <- STK19_TDMg2$player2
STK19_TDMg3 <- STK19_TDMg2
STK19_TDMg3$p1inp2vec <- is.element(STK19_TDMg3$player1, player2vector)
STK19_TDMg3$p2inp1vec <- is.element(STK19_TDMg3$player2, player1vector)

addPlayer1 <- STK19_TDMg3[ which(STK19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_TDMg3[ which(STK19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_TDMg2 <- rbind(STK19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
STK19_TDMft <- ftable(STK19_TDMg2$player1, STK19_TDMg2$player2)
STK19_TDMft2 <- as.matrix(STK19_TDMft)
numRows <- nrow(STK19_TDMft2)
numCols <- ncol(STK19_TDMft2)
STK19_TDMft3 <- STK19_TDMft2[c(2:numRows) , c(2:numCols)]
STK19_TDMTable <- graph.adjacency(STK19_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(STK19_TDMTable, vertex.label = V(STK19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
STK19_TDM.clusterCoef <- transitivity(STK19_TDMTable, type="global") #cluster coefficient
STK19_TDM.degreeCent <- centralization.degree(STK19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_TDMftn <- as.network.matrix(STK19_TDMft)
STK19_TDM.netDensity <- network.density(STK19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_TDM.entropy <- entropy(STK19_TDMft) #entropy

STK19_TDM.netMx <- cbind(STK19_TDM.netMx, STK19_TDM.clusterCoef, STK19_TDM.degreeCent$centralization,
                         STK19_TDM.netDensity, STK19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "STK"
KIoutcome = "Stoppage_D"
STK19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
STK19_SDg2 <- data.frame(STK19_SD)
STK19_SDg2 <- STK19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_SDg2$player1
player2vector <- STK19_SDg2$player2
STK19_SDg3 <- STK19_SDg2
STK19_SDg3$p1inp2vec <- is.element(STK19_SDg3$player1, player2vector)
STK19_SDg3$p2inp1vec <- is.element(STK19_SDg3$player2, player1vector)

addPlayer1 <- STK19_SDg3[ which(STK19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_SDg3[ which(STK19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_SDg2 <- rbind(STK19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
STK19_SDft <- ftable(STK19_SDg2$player1, STK19_SDg2$player2)
STK19_SDft2 <- as.matrix(STK19_SDft)
numRows <- nrow(STK19_SDft2)
numCols <- ncol(STK19_SDft2)
STK19_SDft3 <- STK19_SDft2[c(2:numRows) , c(2:numCols)]
STK19_SDTable <- graph.adjacency(STK19_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(STK19_SDTable, vertex.label = V(STK19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
STK19_SD.clusterCoef <- transitivity(STK19_SDTable, type="global") #cluster coefficient
STK19_SD.degreeCent <- centralization.degree(STK19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_SDftn <- as.network.matrix(STK19_SDft)
STK19_SD.netDensity <- network.density(STK19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_SD.entropy <- entropy(STK19_SDft) #entropy

STK19_SD.netMx <- cbind(STK19_SD.netMx, STK19_SD.clusterCoef, STK19_SD.degreeCent$centralization,
                        STK19_SD.netDensity, STK19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "STK"
KIoutcome = "Turnover_D"
STK19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
STK19_TDg2 <- data.frame(STK19_TD)
STK19_TDg2 <- STK19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_TDg2$player1
player2vector <- STK19_TDg2$player2
STK19_TDg3 <- STK19_TDg2
STK19_TDg3$p1inp2vec <- is.element(STK19_TDg3$player1, player2vector)
STK19_TDg3$p2inp1vec <- is.element(STK19_TDg3$player2, player1vector)

addPlayer1 <- STK19_TDg3[ which(STK19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_TDg3[ which(STK19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_TDg2 <- rbind(STK19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
STK19_TDft <- ftable(STK19_TDg2$player1, STK19_TDg2$player2)
STK19_TDft2 <- as.matrix(STK19_TDft)
numRows <- nrow(STK19_TDft2)
numCols <- ncol(STK19_TDft2)
STK19_TDft3 <- STK19_TDft2[c(2:numRows) , c(2:numCols)]
STK19_TDTable <- graph.adjacency(STK19_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(STK19_TDTable, vertex.label = V(STK19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
STK19_TD.clusterCoef <- transitivity(STK19_TDTable, type="global") #cluster coefficient
STK19_TD.degreeCent <- centralization.degree(STK19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_TDftn <- as.network.matrix(STK19_TDft)
STK19_TD.netDensity <- network.density(STK19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_TD.entropy <- entropy(STK19_TDft) #entropy

STK19_TD.netMx <- cbind(STK19_TD.netMx, STK19_TD.clusterCoef, STK19_TD.degreeCent$centralization,
                        STK19_TD.netDensity, STK19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
STK19_QTg2 <- data.frame(STK19_QT)
STK19_QTg2 <- STK19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK19_QTg2$player1
player2vector <- STK19_QTg2$player2
STK19_QTg3 <- STK19_QTg2
STK19_QTg3$p1inp2vec <- is.element(STK19_QTg3$player1, player2vector)
STK19_QTg3$p2inp1vec <- is.element(STK19_QTg3$player2, player1vector)

addPlayer1 <- STK19_QTg3[ which(STK19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK19_QTg3[ which(STK19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK19_QTg2 <- rbind(STK19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
STK19_QTft <- ftable(STK19_QTg2$player1, STK19_QTg2$player2)
STK19_QTft2 <- as.matrix(STK19_QTft)
numRows <- nrow(STK19_QTft2)
numCols <- ncol(STK19_QTft2)
STK19_QTft3 <- STK19_QTft2[c(2:numRows) , c(2:numCols)]
STK19_QTTable <- graph.adjacency(STK19_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(STK19_QTTable, vertex.label = V(STK19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
STK19_QT.clusterCoef <- transitivity(STK19_QTTable, type="global") #cluster coefficient
STK19_QT.degreeCent <- centralization.degree(STK19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK19_QTftn <- as.network.matrix(STK19_QTft)
STK19_QT.netDensity <- network.density(STK19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK19_QT.entropy <- entropy(STK19_QTft) #entropy

STK19_QT.netMx <- cbind(STK19_QT.netMx, STK19_QT.clusterCoef, STK19_QT.degreeCent$centralization,
                        STK19_QT.netDensity, STK19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK19_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************

round = 19
teamName = "SYD"
KIoutcome = "Goal_F"
SYD19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
SYD19_Gg2 <- data.frame(SYD19_G)
SYD19_Gg2 <- SYD19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_Gg2$player1
player2vector <- SYD19_Gg2$player2
SYD19_Gg3 <- SYD19_Gg2
SYD19_Gg3$p1inp2vec <- is.element(SYD19_Gg3$player1, player2vector)
SYD19_Gg3$p2inp1vec <- is.element(SYD19_Gg3$player2, player1vector)

addPlayer1 <- SYD19_Gg3[ which(SYD19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_Gg3[ which(SYD19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_Gg2 <- rbind(SYD19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
SYD19_Gft <- ftable(SYD19_Gg2$player1, SYD19_Gg2$player2)
SYD19_Gft2 <- as.matrix(SYD19_Gft)
numRows <- nrow(SYD19_Gft2)
numCols <- ncol(SYD19_Gft2)
SYD19_Gft3 <- SYD19_Gft2[c(2:numRows) , c(2:numCols)]
SYD19_GTable <- graph.adjacency(SYD19_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(SYD19_GTable, vertex.label = V(SYD19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
SYD19_G.clusterCoef <- transitivity(SYD19_GTable, type="global") #cluster coefficient
SYD19_G.degreeCent <- centralization.degree(SYD19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_Gftn <- as.network.matrix(SYD19_Gft)
SYD19_G.netDensity <- network.density(SYD19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_G.entropy <- entropy(SYD19_Gft) #entropy

SYD19_G.netMx <- cbind(SYD19_G.netMx, SYD19_G.clusterCoef, SYD19_G.degreeCent$centralization,
                       SYD19_G.netDensity, SYD19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************

round = 19
teamName = "SYD"
KIoutcome = "Behind_F"
SYD19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
SYD19_Bg2 <- data.frame(SYD19_B)
SYD19_Bg2 <- SYD19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_Bg2$player1
player2vector <- SYD19_Bg2$player2
SYD19_Bg3 <- SYD19_Bg2
SYD19_Bg3$p1inp2vec <- is.element(SYD19_Bg3$player1, player2vector)
SYD19_Bg3$p2inp1vec <- is.element(SYD19_Bg3$player2, player1vector)

addPlayer1 <- SYD19_Bg3[ which(SYD19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_Bg3[ which(SYD19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_Bg2 <- rbind(SYD19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
SYD19_Bft <- ftable(SYD19_Bg2$player1, SYD19_Bg2$player2)
SYD19_Bft2 <- as.matrix(SYD19_Bft)
numRows <- nrow(SYD19_Bft2)
numCols <- ncol(SYD19_Bft2)
SYD19_Bft3 <- SYD19_Bft2[c(2:numRows) , c(2:numCols)]
SYD19_BTable <- graph.adjacency(SYD19_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(SYD19_BTable, vertex.label = V(SYD19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
SYD19_B.clusterCoef <- transitivity(SYD19_BTable, type="global") #cluster coefficient
SYD19_B.degreeCent <- centralization.degree(SYD19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_Bftn <- as.network.matrix(SYD19_Bft)
SYD19_B.netDensity <- network.density(SYD19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_B.entropy <- entropy(SYD19_Bft) #entropy

SYD19_B.netMx <- cbind(SYD19_B.netMx, SYD19_B.clusterCoef, SYD19_B.degreeCent$centralization,
                       SYD19_B.netDensity, SYD19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
SYD19_SFg2 <- data.frame(SYD19_SF)
SYD19_SFg2 <- SYD19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_SFg2$player1
player2vector <- SYD19_SFg2$player2
SYD19_SFg3 <- SYD19_SFg2
SYD19_SFg3$p1inp2vec <- is.element(SYD19_SFg3$player1, player2vector)
SYD19_SFg3$p2inp1vec <- is.element(SYD19_SFg3$player2, player1vector)

addPlayer1 <- SYD19_SFg3[ which(SYD19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_SFg3[ which(SYD19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_SFg2 <- rbind(SYD19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
SYD19_SFft <- ftable(SYD19_SFg2$player1, SYD19_SFg2$player2)
SYD19_SFft2 <- as.matrix(SYD19_SFft)
numRows <- nrow(SYD19_SFft2)
numCols <- ncol(SYD19_SFft2)
SYD19_SFft3 <- SYD19_SFft2[c(2:numRows) , c(2:numCols)]
SYD19_SFTable <- graph.adjacency(SYD19_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(SYD19_SFTable, vertex.label = V(SYD19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
SYD19_SF.clusterCoef <- transitivity(SYD19_SFTable, type="global") #cluster coefficient
SYD19_SF.degreeCent <- centralization.degree(SYD19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_SFftn <- as.network.matrix(SYD19_SFft)
SYD19_SF.netDensity <- network.density(SYD19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_SF.entropy <- entropy(SYD19_SFft) #entropy

SYD19_SF.netMx <- cbind(SYD19_SF.netMx, SYD19_SF.clusterCoef, SYD19_SF.degreeCent$centralization,
                        SYD19_SF.netDensity, SYD19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
SYD19_TFg2 <- data.frame(SYD19_TF)
SYD19_TFg2 <- SYD19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_TFg2$player1
player2vector <- SYD19_TFg2$player2
SYD19_TFg3 <- SYD19_TFg2
SYD19_TFg3$p1inp2vec <- is.element(SYD19_TFg3$player1, player2vector)
SYD19_TFg3$p2inp1vec <- is.element(SYD19_TFg3$player2, player1vector)

addPlayer1 <- SYD19_TFg3[ which(SYD19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_TFg3[ which(SYD19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_TFg2 <- rbind(SYD19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
SYD19_TFft <- ftable(SYD19_TFg2$player1, SYD19_TFg2$player2)
SYD19_TFft2 <- as.matrix(SYD19_TFft)
numRows <- nrow(SYD19_TFft2)
numCols <- ncol(SYD19_TFft2)
SYD19_TFft3 <- SYD19_TFft2[c(2:numRows) , c(2:numCols)]
SYD19_TFTable <- graph.adjacency(SYD19_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(SYD19_TFTable, vertex.label = V(SYD19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
SYD19_TF.clusterCoef <- transitivity(SYD19_TFTable, type="global") #cluster coefficient
SYD19_TF.degreeCent <- centralization.degree(SYD19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_TFftn <- as.network.matrix(SYD19_TFft)
SYD19_TF.netDensity <- network.density(SYD19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_TF.entropy <- entropy(SYD19_TFft) #entropy

SYD19_TF.netMx <- cbind(SYD19_TF.netMx, SYD19_TF.clusterCoef, SYD19_TF.degreeCent$centralization,
                        SYD19_TF.netDensity, SYD19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
SYD19_SAMg2 <- data.frame(SYD19_SAM)
SYD19_SAMg2 <- SYD19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_SAMg2$player1
player2vector <- SYD19_SAMg2$player2
SYD19_SAMg3 <- SYD19_SAMg2
SYD19_SAMg3$p1inp2vec <- is.element(SYD19_SAMg3$player1, player2vector)
SYD19_SAMg3$p2inp1vec <- is.element(SYD19_SAMg3$player2, player1vector)

addPlayer1 <- SYD19_SAMg3[ which(SYD19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_SAMg3[ which(SYD19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_SAMg2 <- rbind(SYD19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
SYD19_SAMft <- ftable(SYD19_SAMg2$player1, SYD19_SAMg2$player2)
SYD19_SAMft2 <- as.matrix(SYD19_SAMft)
numRows <- nrow(SYD19_SAMft2)
numCols <- ncol(SYD19_SAMft2)
SYD19_SAMft3 <- SYD19_SAMft2[c(2:numRows) , c(2:numCols)]
SYD19_SAMTable <- graph.adjacency(SYD19_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(SYD19_SAMTable, vertex.label = V(SYD19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
SYD19_SAM.clusterCoef <- transitivity(SYD19_SAMTable, type="global") #cluster coefficient
SYD19_SAM.degreeCent <- centralization.degree(SYD19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_SAMftn <- as.network.matrix(SYD19_SAMft)
SYD19_SAM.netDensity <- network.density(SYD19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_SAM.entropy <- entropy(SYD19_SAMft) #entropy

SYD19_SAM.netMx <- cbind(SYD19_SAM.netMx, SYD19_SAM.clusterCoef, SYD19_SAM.degreeCent$centralization,
                         SYD19_SAM.netDensity, SYD19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
SYD19_TAMg2 <- data.frame(SYD19_TAM)
SYD19_TAMg2 <- SYD19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_TAMg2$player1
player2vector <- SYD19_TAMg2$player2
SYD19_TAMg3 <- SYD19_TAMg2
SYD19_TAMg3$p1inp2vec <- is.element(SYD19_TAMg3$player1, player2vector)
SYD19_TAMg3$p2inp1vec <- is.element(SYD19_TAMg3$player2, player1vector)

addPlayer1 <- SYD19_TAMg3[ which(SYD19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_TAMg3[ which(SYD19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_TAMg2 <- rbind(SYD19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
SYD19_TAMft <- ftable(SYD19_TAMg2$player1, SYD19_TAMg2$player2)
SYD19_TAMft2 <- as.matrix(SYD19_TAMft)
numRows <- nrow(SYD19_TAMft2)
numCols <- ncol(SYD19_TAMft2)
SYD19_TAMft3 <- SYD19_TAMft2[c(2:numRows) , c(2:numCols)]
SYD19_TAMTable <- graph.adjacency(SYD19_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(SYD19_TAMTable, vertex.label = V(SYD19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
SYD19_TAM.clusterCoef <- transitivity(SYD19_TAMTable, type="global") #cluster coefficient
SYD19_TAM.degreeCent <- centralization.degree(SYD19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_TAMftn <- as.network.matrix(SYD19_TAMft)
SYD19_TAM.netDensity <- network.density(SYD19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_TAM.entropy <- entropy(SYD19_TAMft) #entropy

SYD19_TAM.netMx <- cbind(SYD19_TAM.netMx, SYD19_TAM.clusterCoef, SYD19_TAM.degreeCent$centralization,
                         SYD19_TAM.netDensity, SYD19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************

round = 19
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
SYD19_SDMg2 <- data.frame(SYD19_SDM)
SYD19_SDMg2 <- SYD19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_SDMg2$player1
player2vector <- SYD19_SDMg2$player2
SYD19_SDMg3 <- SYD19_SDMg2
SYD19_SDMg3$p1inp2vec <- is.element(SYD19_SDMg3$player1, player2vector)
SYD19_SDMg3$p2inp1vec <- is.element(SYD19_SDMg3$player2, player1vector)

addPlayer1 <- SYD19_SDMg3[ which(SYD19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_SDMg3[ which(SYD19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_SDMg2 <- rbind(SYD19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
SYD19_SDMft <- ftable(SYD19_SDMg2$player1, SYD19_SDMg2$player2)
SYD19_SDMft2 <- as.matrix(SYD19_SDMft)
numRows <- nrow(SYD19_SDMft2)
numCols <- ncol(SYD19_SDMft2)
SYD19_SDMft3 <- SYD19_SDMft2[c(2:numRows) , c(2:numCols)]
SYD19_SDMTable <- graph.adjacency(SYD19_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(SYD19_SDMTable, vertex.label = V(SYD19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
SYD19_SDM.clusterCoef <- transitivity(SYD19_SDMTable, type="global") #cluster coefficient
SYD19_SDM.degreeCent <- centralization.degree(SYD19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_SDMftn <- as.network.matrix(SYD19_SDMft)
SYD19_SDM.netDensity <- network.density(SYD19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_SDM.entropy <- entropy(SYD19_SDMft) #entropy

SYD19_SDM.netMx <- cbind(SYD19_SDM.netMx, SYD19_SDM.clusterCoef, SYD19_SDM.degreeCent$centralization,
                         SYD19_SDM.netDensity, SYD19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
SYD19_TDMg2 <- data.frame(SYD19_TDM)
SYD19_TDMg2 <- SYD19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_TDMg2$player1
player2vector <- SYD19_TDMg2$player2
SYD19_TDMg3 <- SYD19_TDMg2
SYD19_TDMg3$p1inp2vec <- is.element(SYD19_TDMg3$player1, player2vector)
SYD19_TDMg3$p2inp1vec <- is.element(SYD19_TDMg3$player2, player1vector)

addPlayer1 <- SYD19_TDMg3[ which(SYD19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_TDMg3[ which(SYD19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_TDMg2 <- rbind(SYD19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
SYD19_TDMft <- ftable(SYD19_TDMg2$player1, SYD19_TDMg2$player2)
SYD19_TDMft2 <- as.matrix(SYD19_TDMft)
numRows <- nrow(SYD19_TDMft2)
numCols <- ncol(SYD19_TDMft2)
SYD19_TDMft3 <- SYD19_TDMft2[c(2:numRows) , c(2:numCols)]
SYD19_TDMTable <- graph.adjacency(SYD19_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(SYD19_TDMTable, vertex.label = V(SYD19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
SYD19_TDM.clusterCoef <- transitivity(SYD19_TDMTable, type="global") #cluster coefficient
SYD19_TDM.degreeCent <- centralization.degree(SYD19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_TDMftn <- as.network.matrix(SYD19_TDMft)
SYD19_TDM.netDensity <- network.density(SYD19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_TDM.entropy <- entropy(SYD19_TDMft) #entropy

SYD19_TDM.netMx <- cbind(SYD19_TDM.netMx, SYD19_TDM.clusterCoef, SYD19_TDM.degreeCent$centralization,
                         SYD19_TDM.netDensity, SYD19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
SYD19_SDg2 <- data.frame(SYD19_SD)
SYD19_SDg2 <- SYD19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_SDg2$player1
player2vector <- SYD19_SDg2$player2
SYD19_SDg3 <- SYD19_SDg2
SYD19_SDg3$p1inp2vec <- is.element(SYD19_SDg3$player1, player2vector)
SYD19_SDg3$p2inp1vec <- is.element(SYD19_SDg3$player2, player1vector)

addPlayer1 <- SYD19_SDg3[ which(SYD19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_SDg3[ which(SYD19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_SDg2 <- rbind(SYD19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
SYD19_SDft <- ftable(SYD19_SDg2$player1, SYD19_SDg2$player2)
SYD19_SDft2 <- as.matrix(SYD19_SDft)
numRows <- nrow(SYD19_SDft2)
numCols <- ncol(SYD19_SDft2)
SYD19_SDft3 <- SYD19_SDft2[c(2:numRows) , c(2:numCols)]
SYD19_SDTable <- graph.adjacency(SYD19_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(SYD19_SDTable, vertex.label = V(SYD19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
SYD19_SD.clusterCoef <- transitivity(SYD19_SDTable, type="global") #cluster coefficient
SYD19_SD.degreeCent <- centralization.degree(SYD19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_SDftn <- as.network.matrix(SYD19_SDft)
SYD19_SD.netDensity <- network.density(SYD19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_SD.entropy <- entropy(SYD19_SDft) #entropy

SYD19_SD.netMx <- cbind(SYD19_SD.netMx, SYD19_SD.clusterCoef, SYD19_SD.degreeCent$centralization,
                        SYD19_SD.netDensity, SYD19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
SYD19_TDg2 <- data.frame(SYD19_TD)
SYD19_TDg2 <- SYD19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_TDg2$player1
player2vector <- SYD19_TDg2$player2
SYD19_TDg3 <- SYD19_TDg2
SYD19_TDg3$p1inp2vec <- is.element(SYD19_TDg3$player1, player2vector)
SYD19_TDg3$p2inp1vec <- is.element(SYD19_TDg3$player2, player1vector)

addPlayer1 <- SYD19_TDg3[ which(SYD19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_TDg3[ which(SYD19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_TDg2 <- rbind(SYD19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
SYD19_TDft <- ftable(SYD19_TDg2$player1, SYD19_TDg2$player2)
SYD19_TDft2 <- as.matrix(SYD19_TDft)
numRows <- nrow(SYD19_TDft2)
numCols <- ncol(SYD19_TDft2)
SYD19_TDft3 <- SYD19_TDft2[c(2:numRows) , c(2:numCols)]
SYD19_TDTable <- graph.adjacency(SYD19_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(SYD19_TDTable, vertex.label = V(SYD19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
SYD19_TD.clusterCoef <- transitivity(SYD19_TDTable, type="global") #cluster coefficient
SYD19_TD.degreeCent <- centralization.degree(SYD19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_TDftn <- as.network.matrix(SYD19_TDft)
SYD19_TD.netDensity <- network.density(SYD19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_TD.entropy <- entropy(SYD19_TDft) #entropy

SYD19_TD.netMx <- cbind(SYD19_TD.netMx, SYD19_TD.clusterCoef, SYD19_TD.degreeCent$centralization,
                        SYD19_TD.netDensity, SYD19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
SYD19_QTg2 <- data.frame(SYD19_QT)
SYD19_QTg2 <- SYD19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD19_QTg2$player1
player2vector <- SYD19_QTg2$player2
SYD19_QTg3 <- SYD19_QTg2
SYD19_QTg3$p1inp2vec <- is.element(SYD19_QTg3$player1, player2vector)
SYD19_QTg3$p2inp1vec <- is.element(SYD19_QTg3$player2, player1vector)

addPlayer1 <- SYD19_QTg3[ which(SYD19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD19_QTg3[ which(SYD19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD19_QTg2 <- rbind(SYD19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
SYD19_QTft <- ftable(SYD19_QTg2$player1, SYD19_QTg2$player2)
SYD19_QTft2 <- as.matrix(SYD19_QTft)
numRows <- nrow(SYD19_QTft2)
numCols <- ncol(SYD19_QTft2)
SYD19_QTft3 <- SYD19_QTft2[c(2:numRows) , c(2:numCols)]
SYD19_QTTable <- graph.adjacency(SYD19_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(SYD19_QTTable, vertex.label = V(SYD19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
SYD19_QT.clusterCoef <- transitivity(SYD19_QTTable, type="global") #cluster coefficient
SYD19_QT.degreeCent <- centralization.degree(SYD19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD19_QTftn <- as.network.matrix(SYD19_QTft)
SYD19_QT.netDensity <- network.density(SYD19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD19_QT.entropy <- entropy(SYD19_QTft) #entropy

SYD19_QT.netMx <- cbind(SYD19_QT.netMx, SYD19_QT.clusterCoef, SYD19_QT.degreeCent$centralization,
                        SYD19_QT.netDensity, SYD19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD19_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "WB"
KIoutcome = "Goal_F"
WB19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
WB19_Gg2 <- data.frame(WB19_G)
WB19_Gg2 <- WB19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_Gg2$player1
player2vector <- WB19_Gg2$player2
WB19_Gg3 <- WB19_Gg2
WB19_Gg3$p1inp2vec <- is.element(WB19_Gg3$player1, player2vector)
WB19_Gg3$p2inp1vec <- is.element(WB19_Gg3$player2, player1vector)

addPlayer1 <- WB19_Gg3[ which(WB19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_Gg3[ which(WB19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_Gg2 <- rbind(WB19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
WB19_Gft <- ftable(WB19_Gg2$player1, WB19_Gg2$player2)
WB19_Gft2 <- as.matrix(WB19_Gft)
numRows <- nrow(WB19_Gft2)
numCols <- ncol(WB19_Gft2)
WB19_Gft3 <- WB19_Gft2[c(2:numRows) , c(2:numCols)]
WB19_GTable <- graph.adjacency(WB19_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(WB19_GTable, vertex.label = V(WB19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
WB19_G.clusterCoef <- transitivity(WB19_GTable, type="global") #cluster coefficient
WB19_G.degreeCent <- centralization.degree(WB19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_Gftn <- as.network.matrix(WB19_Gft)
WB19_G.netDensity <- network.density(WB19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_G.entropy <- entropy(WB19_Gft) #entropy

WB19_G.netMx <- cbind(WB19_G.netMx, WB19_G.clusterCoef, WB19_G.degreeCent$centralization,
                      WB19_G.netDensity, WB19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************
#NA

round = 19
teamName = "WB"
KIoutcome = "Behind_F"
WB19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
WB19_Bg2 <- data.frame(WB19_B)
WB19_Bg2 <- WB19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_Bg2$player1
player2vector <- WB19_Bg2$player2
WB19_Bg3 <- WB19_Bg2
WB19_Bg3$p1inp2vec <- is.element(WB19_Bg3$player1, player2vector)
WB19_Bg3$p2inp1vec <- is.element(WB19_Bg3$player2, player1vector)

addPlayer1 <- WB19_Bg3[ which(WB19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_Bg3[ which(WB19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_Bg2 <- rbind(WB19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
WB19_Bft <- ftable(WB19_Bg2$player1, WB19_Bg2$player2)
WB19_Bft2 <- as.matrix(WB19_Bft)
numRows <- nrow(WB19_Bft2)
numCols <- ncol(WB19_Bft2)
WB19_Bft3 <- WB19_Bft2[c(2:numRows) , c(2:numCols)]
WB19_BTable <- graph.adjacency(WB19_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(WB19_BTable, vertex.label = V(WB19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
WB19_B.clusterCoef <- transitivity(WB19_BTable, type="global") #cluster coefficient
WB19_B.degreeCent <- centralization.degree(WB19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_Bftn <- as.network.matrix(WB19_Bft)
WB19_B.netDensity <- network.density(WB19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_B.entropy <- entropy(WB19_Bft) #entropy

WB19_B.netMx <- cbind(WB19_B.netMx, WB19_B.clusterCoef, WB19_B.degreeCent$centralization,
                      WB19_B.netDensity, WB19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************

round = 19
teamName = "WB"
KIoutcome = "Stoppage_F"
WB19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
WB19_SFg2 <- data.frame(WB19_SF)
WB19_SFg2 <- WB19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_SFg2$player1
player2vector <- WB19_SFg2$player2
WB19_SFg3 <- WB19_SFg2
WB19_SFg3$p1inp2vec <- is.element(WB19_SFg3$player1, player2vector)
WB19_SFg3$p2inp1vec <- is.element(WB19_SFg3$player2, player1vector)

addPlayer1 <- WB19_SFg3[ which(WB19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_SFg3[ which(WB19_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_SFg2 <- rbind(WB19_SFg2, addPlayers)

#ROUND 19, FWD Stoppage graph using weighted edges
WB19_SFft <- ftable(WB19_SFg2$player1, WB19_SFg2$player2)
WB19_SFft2 <- as.matrix(WB19_SFft)
numRows <- nrow(WB19_SFft2)
numCols <- ncol(WB19_SFft2)
WB19_SFft3 <- WB19_SFft2[c(2:numRows) , c(2:numCols)]
WB19_SFTable <- graph.adjacency(WB19_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(WB19_SFTable, vertex.label = V(WB19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
WB19_SF.clusterCoef <- transitivity(WB19_SFTable, type="global") #cluster coefficient
WB19_SF.degreeCent <- centralization.degree(WB19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_SFftn <- as.network.matrix(WB19_SFft)
WB19_SF.netDensity <- network.density(WB19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_SF.entropy <- entropy(WB19_SFft) #entropy

WB19_SF.netMx <- cbind(WB19_SF.netMx, WB19_SF.clusterCoef, WB19_SF.degreeCent$centralization,
                       WB19_SF.netDensity, WB19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************
#NA

round = 19
teamName = "WB"
KIoutcome = "Turnover_F"
WB19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
WB19_TFg2 <- data.frame(WB19_TF)
WB19_TFg2 <- WB19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_TFg2$player1
player2vector <- WB19_TFg2$player2
WB19_TFg3 <- WB19_TFg2
WB19_TFg3$p1inp2vec <- is.element(WB19_TFg3$player1, player2vector)
WB19_TFg3$p2inp1vec <- is.element(WB19_TFg3$player2, player1vector)

addPlayer1 <- WB19_TFg3[ which(WB19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_TFg3[ which(WB19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_TFg2 <- rbind(WB19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
WB19_TFft <- ftable(WB19_TFg2$player1, WB19_TFg2$player2)
WB19_TFft2 <- as.matrix(WB19_TFft)
numRows <- nrow(WB19_TFft2)
numCols <- ncol(WB19_TFft2)
WB19_TFft3 <- WB19_TFft2[c(2:numRows) , c(2:numCols)]
WB19_TFTable <- graph.adjacency(WB19_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(WB19_TFTable, vertex.label = V(WB19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
WB19_TF.clusterCoef <- transitivity(WB19_TFTable, type="global") #cluster coefficient
WB19_TF.degreeCent <- centralization.degree(WB19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_TFftn <- as.network.matrix(WB19_TFft)
WB19_TF.netDensity <- network.density(WB19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_TF.entropy <- entropy(WB19_TFft) #entropy

WB19_TF.netMx <- cbind(WB19_TF.netMx, WB19_TF.clusterCoef, WB19_TF.degreeCent$centralization,
                       WB19_TF.netDensity, WB19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
WB19_SAMg2 <- data.frame(WB19_SAM)
WB19_SAMg2 <- WB19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_SAMg2$player1
player2vector <- WB19_SAMg2$player2
WB19_SAMg3 <- WB19_SAMg2
WB19_SAMg3$p1inp2vec <- is.element(WB19_SAMg3$player1, player2vector)
WB19_SAMg3$p2inp1vec <- is.element(WB19_SAMg3$player2, player1vector)

addPlayer1 <- WB19_SAMg3[ which(WB19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_SAMg3[ which(WB19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_SAMg2 <- rbind(WB19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
WB19_SAMft <- ftable(WB19_SAMg2$player1, WB19_SAMg2$player2)
WB19_SAMft2 <- as.matrix(WB19_SAMft)
numRows <- nrow(WB19_SAMft2)
numCols <- ncol(WB19_SAMft2)
WB19_SAMft3 <- WB19_SAMft2[c(2:numRows) , c(2:numCols)]
WB19_SAMTable <- graph.adjacency(WB19_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(WB19_SAMTable, vertex.label = V(WB19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
WB19_SAM.clusterCoef <- transitivity(WB19_SAMTable, type="global") #cluster coefficient
WB19_SAM.degreeCent <- centralization.degree(WB19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_SAMftn <- as.network.matrix(WB19_SAMft)
WB19_SAM.netDensity <- network.density(WB19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_SAM.entropy <- entropy(WB19_SAMft) #entropy

WB19_SAM.netMx <- cbind(WB19_SAM.netMx, WB19_SAM.clusterCoef, WB19_SAM.degreeCent$centralization,
                        WB19_SAM.netDensity, WB19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "WB"
KIoutcome = "Turnover_AM"
WB19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
WB19_TAMg2 <- data.frame(WB19_TAM)
WB19_TAMg2 <- WB19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_TAMg2$player1
player2vector <- WB19_TAMg2$player2
WB19_TAMg3 <- WB19_TAMg2
WB19_TAMg3$p1inp2vec <- is.element(WB19_TAMg3$player1, player2vector)
WB19_TAMg3$p2inp1vec <- is.element(WB19_TAMg3$player2, player1vector)

addPlayer1 <- WB19_TAMg3[ which(WB19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WB19_TAMg3[ which(WB19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_TAMg2 <- rbind(WB19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
WB19_TAMft <- ftable(WB19_TAMg2$player1, WB19_TAMg2$player2)
WB19_TAMft2 <- as.matrix(WB19_TAMft)
numRows <- nrow(WB19_TAMft2)
numCols <- ncol(WB19_TAMft2)
WB19_TAMft3 <- WB19_TAMft2[c(2:numRows) , c(2:numCols)]
WB19_TAMTable <- graph.adjacency(WB19_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(WB19_TAMTable, vertex.label = V(WB19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
WB19_TAM.clusterCoef <- transitivity(WB19_TAMTable, type="global") #cluster coefficient
WB19_TAM.degreeCent <- centralization.degree(WB19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_TAMftn <- as.network.matrix(WB19_TAMft)
WB19_TAM.netDensity <- network.density(WB19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_TAM.entropy <- entropy(WB19_TAMft) #entropy

WB19_TAM.netMx <- cbind(WB19_TAM.netMx, WB19_TAM.clusterCoef, WB19_TAM.degreeCent$centralization,
                        WB19_TAM.netDensity, WB19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************

round = 19
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
WB19_SDMg2 <- data.frame(WB19_SDM)
WB19_SDMg2 <- WB19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_SDMg2$player1
player2vector <- WB19_SDMg2$player2
WB19_SDMg3 <- WB19_SDMg2
WB19_SDMg3$p1inp2vec <- is.element(WB19_SDMg3$player1, player2vector)
WB19_SDMg3$p2inp1vec <- is.element(WB19_SDMg3$player2, player1vector)

addPlayer1 <- WB19_SDMg3[ which(WB19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_SDMg3[ which(WB19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_SDMg2 <- rbind(WB19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
WB19_SDMft <- ftable(WB19_SDMg2$player1, WB19_SDMg2$player2)
WB19_SDMft2 <- as.matrix(WB19_SDMft)
numRows <- nrow(WB19_SDMft2)
numCols <- ncol(WB19_SDMft2)
WB19_SDMft3 <- WB19_SDMft2[c(2:numRows) , c(2:numCols)]
WB19_SDMTable <- graph.adjacency(WB19_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(WB19_SDMTable, vertex.label = V(WB19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
WB19_SDM.clusterCoef <- transitivity(WB19_SDMTable, type="global") #cluster coefficient
WB19_SDM.degreeCent <- centralization.degree(WB19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_SDMftn <- as.network.matrix(WB19_SDMft)
WB19_SDM.netDensity <- network.density(WB19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_SDM.entropy <- entropy(WB19_SDMft) #entropy

WB19_SDM.netMx <- cbind(WB19_SDM.netMx, WB19_SDM.clusterCoef, WB19_SDM.degreeCent$centralization,
                        WB19_SDM.netDensity, WB19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "WB"
KIoutcome = "Turnover_DM"
WB19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
WB19_TDMg2 <- data.frame(WB19_TDM)
WB19_TDMg2 <- WB19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_TDMg2$player1
player2vector <- WB19_TDMg2$player2
WB19_TDMg3 <- WB19_TDMg2
WB19_TDMg3$p1inp2vec <- is.element(WB19_TDMg3$player1, player2vector)
WB19_TDMg3$p2inp1vec <- is.element(WB19_TDMg3$player2, player1vector)

addPlayer1 <- WB19_TDMg3[ which(WB19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_TDMg3[ which(WB19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_TDMg2 <- rbind(WB19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
WB19_TDMft <- ftable(WB19_TDMg2$player1, WB19_TDMg2$player2)
WB19_TDMft2 <- as.matrix(WB19_TDMft)
numRows <- nrow(WB19_TDMft2)
numCols <- ncol(WB19_TDMft2)
WB19_TDMft3 <- WB19_TDMft2[c(2:numRows) , c(2:numCols)]
WB19_TDMTable <- graph.adjacency(WB19_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(WB19_TDMTable, vertex.label = V(WB19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
WB19_TDM.clusterCoef <- transitivity(WB19_TDMTable, type="global") #cluster coefficient
WB19_TDM.degreeCent <- centralization.degree(WB19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_TDMftn <- as.network.matrix(WB19_TDMft)
WB19_TDM.netDensity <- network.density(WB19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_TDM.entropy <- entropy(WB19_TDMft) #entropy

WB19_TDM.netMx <- cbind(WB19_TDM.netMx, WB19_TDM.clusterCoef, WB19_TDM.degreeCent$centralization,
                        WB19_TDM.netDensity, WB19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "WB"
KIoutcome = "Stoppage_D"
WB19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
WB19_SDg2 <- data.frame(WB19_SD)
WB19_SDg2 <- WB19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_SDg2$player1
player2vector <- WB19_SDg2$player2
WB19_SDg3 <- WB19_SDg2
WB19_SDg3$p1inp2vec <- is.element(WB19_SDg3$player1, player2vector)
WB19_SDg3$p2inp1vec <- is.element(WB19_SDg3$player2, player1vector)

addPlayer1 <- WB19_SDg3[ which(WB19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_SDg3[ which(WB19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_SDg2 <- rbind(WB19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
WB19_SDft <- ftable(WB19_SDg2$player1, WB19_SDg2$player2)
WB19_SDft2 <- as.matrix(WB19_SDft)
numRows <- nrow(WB19_SDft2)
numCols <- ncol(WB19_SDft2)
WB19_SDft3 <- WB19_SDft2[c(2:numRows) , c(2:numCols)]
WB19_SDTable <- graph.adjacency(WB19_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(WB19_SDTable, vertex.label = V(WB19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
WB19_SD.clusterCoef <- transitivity(WB19_SDTable, type="global") #cluster coefficient
WB19_SD.degreeCent <- centralization.degree(WB19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_SDftn <- as.network.matrix(WB19_SDft)
WB19_SD.netDensity <- network.density(WB19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_SD.entropy <- entropy(WB19_SDft) #entropy

WB19_SD.netMx <- cbind(WB19_SD.netMx, WB19_SD.clusterCoef, WB19_SD.degreeCent$centralization,
                       WB19_SD.netDensity, WB19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************

round = 19
teamName = "WB"
KIoutcome = "Turnover_D"
WB19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
WB19_TDg2 <- data.frame(WB19_TD)
WB19_TDg2 <- WB19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_TDg2$player1
player2vector <- WB19_TDg2$player2
WB19_TDg3 <- WB19_TDg2
WB19_TDg3$p1inp2vec <- is.element(WB19_TDg3$player1, player2vector)
WB19_TDg3$p2inp1vec <- is.element(WB19_TDg3$player2, player1vector)

addPlayer1 <- WB19_TDg3[ which(WB19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_TDg3[ which(WB19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_TDg2 <- rbind(WB19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
WB19_TDft <- ftable(WB19_TDg2$player1, WB19_TDg2$player2)
WB19_TDft2 <- as.matrix(WB19_TDft)
numRows <- nrow(WB19_TDft2)
numCols <- ncol(WB19_TDft2)
WB19_TDft3 <- WB19_TDft2[c(2:numRows) , c(2:numCols)]
WB19_TDTable <- graph.adjacency(WB19_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(WB19_TDTable, vertex.label = V(WB19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
WB19_TD.clusterCoef <- transitivity(WB19_TDTable, type="global") #cluster coefficient
WB19_TD.degreeCent <- centralization.degree(WB19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_TDftn <- as.network.matrix(WB19_TDft)
WB19_TD.netDensity <- network.density(WB19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_TD.entropy <- entropy(WB19_TDft) #entropy

WB19_TD.netMx <- cbind(WB19_TD.netMx, WB19_TD.clusterCoef, WB19_TD.degreeCent$centralization,
                       WB19_TD.netDensity, WB19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
WB19_QTg2 <- data.frame(WB19_QT)
WB19_QTg2 <- WB19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB19_QTg2$player1
player2vector <- WB19_QTg2$player2
WB19_QTg3 <- WB19_QTg2
WB19_QTg3$p1inp2vec <- is.element(WB19_QTg3$player1, player2vector)
WB19_QTg3$p2inp1vec <- is.element(WB19_QTg3$player2, player1vector)

addPlayer1 <- WB19_QTg3[ which(WB19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB19_QTg3[ which(WB19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB19_QTg2 <- rbind(WB19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
WB19_QTft <- ftable(WB19_QTg2$player1, WB19_QTg2$player2)
WB19_QTft2 <- as.matrix(WB19_QTft)
numRows <- nrow(WB19_QTft2)
numCols <- ncol(WB19_QTft2)
WB19_QTft3 <- WB19_QTft2[c(2:numRows) , c(2:numCols)]
WB19_QTTable <- graph.adjacency(WB19_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(WB19_QTTable, vertex.label = V(WB19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
WB19_QT.clusterCoef <- transitivity(WB19_QTTable, type="global") #cluster coefficient
WB19_QT.degreeCent <- centralization.degree(WB19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB19_QTftn <- as.network.matrix(WB19_QTft)
WB19_QT.netDensity <- network.density(WB19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB19_QT.entropy <- entropy(WB19_QTft) #entropy

WB19_QT.netMx <- cbind(WB19_QT.netMx, WB19_QT.clusterCoef, WB19_QT.degreeCent$centralization,
                       WB19_QT.netDensity, WB19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB19_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 19
##

#ROUND 19, Goal***************************************************************
#NA

round = 19
teamName = "WCE"
KIoutcome = "Goal_F"
WCE19_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Goal with weighted edges
WCE19_Gg2 <- data.frame(WCE19_G)
WCE19_Gg2 <- WCE19_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_Gg2$player1
player2vector <- WCE19_Gg2$player2
WCE19_Gg3 <- WCE19_Gg2
WCE19_Gg3$p1inp2vec <- is.element(WCE19_Gg3$player1, player2vector)
WCE19_Gg3$p2inp1vec <- is.element(WCE19_Gg3$player2, player1vector)

addPlayer1 <- WCE19_Gg3[ which(WCE19_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_Gg3[ which(WCE19_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_Gg2 <- rbind(WCE19_Gg2, addPlayers)

#ROUND 19, Goal graph using weighted edges
WCE19_Gft <- ftable(WCE19_Gg2$player1, WCE19_Gg2$player2)
WCE19_Gft2 <- as.matrix(WCE19_Gft)
numRows <- nrow(WCE19_Gft2)
numCols <- ncol(WCE19_Gft2)
WCE19_Gft3 <- WCE19_Gft2[c(2:numRows) , c(2:numCols)]
WCE19_GTable <- graph.adjacency(WCE19_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Goal graph=weighted
plot.igraph(WCE19_GTable, vertex.label = V(WCE19_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Goal calulation of network metrics
#igraph
WCE19_G.clusterCoef <- transitivity(WCE19_GTable, type="global") #cluster coefficient
WCE19_G.degreeCent <- centralization.degree(WCE19_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_Gftn <- as.network.matrix(WCE19_Gft)
WCE19_G.netDensity <- network.density(WCE19_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_G.entropy <- entropy(WCE19_Gft) #entropy

WCE19_G.netMx <- cbind(WCE19_G.netMx, WCE19_G.clusterCoef, WCE19_G.degreeCent$centralization,
                       WCE19_G.netDensity, WCE19_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_G.netMx) <- varnames

#ROUND 19, Behind***************************************************************

round = 19
teamName = "WCE"
KIoutcome = "Behind_F"
WCE19_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, Behind with weighted edges
WCE19_Bg2 <- data.frame(WCE19_B)
WCE19_Bg2 <- WCE19_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_Bg2$player1
player2vector <- WCE19_Bg2$player2
WCE19_Bg3 <- WCE19_Bg2
WCE19_Bg3$p1inp2vec <- is.element(WCE19_Bg3$player1, player2vector)
WCE19_Bg3$p2inp1vec <- is.element(WCE19_Bg3$player2, player1vector)

addPlayer1 <- WCE19_Bg3[ which(WCE19_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_Bg3[ which(WCE19_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_Bg2 <- rbind(WCE19_Bg2, addPlayers)

#ROUND 19, Behind graph using weighted edges
WCE19_Bft <- ftable(WCE19_Bg2$player1, WCE19_Bg2$player2)
WCE19_Bft2 <- as.matrix(WCE19_Bft)
numRows <- nrow(WCE19_Bft2)
numCols <- ncol(WCE19_Bft2)
WCE19_Bft3 <- WCE19_Bft2[c(2:numRows) , c(2:numCols)]
WCE19_BTable <- graph.adjacency(WCE19_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 19, Behind graph=weighted
plot.igraph(WCE19_BTable, vertex.label = V(WCE19_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, Behind calulation of network metrics
#igraph
WCE19_B.clusterCoef <- transitivity(WCE19_BTable, type="global") #cluster coefficient
WCE19_B.degreeCent <- centralization.degree(WCE19_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_Bftn <- as.network.matrix(WCE19_Bft)
WCE19_B.netDensity <- network.density(WCE19_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_B.entropy <- entropy(WCE19_Bft) #entropy

WCE19_B.netMx <- cbind(WCE19_B.netMx, WCE19_B.clusterCoef, WCE19_B.degreeCent$centralization,
                       WCE19_B.netDensity, WCE19_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_B.netMx) <- varnames

#ROUND 19, FWD Stoppage**********************************************************
#NA

round = 19
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE19_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Stoppage with weighted edges
WCE19_SFg2 <- data.frame(WCE19_SF)
WCE19_SFg2 <- WCE19_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_SFg2$player1
player2vector <- WCE19_SFg2$player2
WCE19_SFg3 <- WCE19_SFg2
WCE19_SFg3$p1inp2vec <- is.element(WCE19_SFg3$player1, player2vector)
WCE19_SFg3$p2inp1vec <- is.element(WCE19_SFg3$player2, player1vector)

addPlayer1 <- WCE19_SFg3[ which(WCE19_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE19_SFg2 <- rbind(WCE19_SFg2, addPlayer1)

#ROUND 19, FWD Stoppage graph using weighted edges
WCE19_SFft <- ftable(WCE19_SFg2$player1, WCE19_SFg2$player2)
WCE19_SFft2 <- as.matrix(WCE19_SFft)
numRows <- nrow(WCE19_SFft2)
numCols <- ncol(WCE19_SFft2)
WCE19_SFft3 <- WCE19_SFft2[c(2:numRows) , c(1:numCols)]
WCE19_SFTable <- graph.adjacency(WCE19_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Stoppage graph=weighted
plot.igraph(WCE19_SFTable, vertex.label = V(WCE19_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Stoppage calulation of network metrics
#igraph
WCE19_SF.clusterCoef <- transitivity(WCE19_SFTable, type="global") #cluster coefficient
WCE19_SF.degreeCent <- centralization.degree(WCE19_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_SFftn <- as.network.matrix(WCE19_SFft)
WCE19_SF.netDensity <- network.density(WCE19_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_SF.entropy <- entropy(WCE19_SFft) #entropy

WCE19_SF.netMx <- cbind(WCE19_SF.netMx, WCE19_SF.clusterCoef, WCE19_SF.degreeCent$centralization,
                        WCE19_SF.netDensity, WCE19_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_SF.netMx) <- varnames

#ROUND 19, FWD Turnover**********************************************************

round = 19
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE19_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, FWD Turnover with weighted edges
WCE19_TFg2 <- data.frame(WCE19_TF)
WCE19_TFg2 <- WCE19_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_TFg2$player1
player2vector <- WCE19_TFg2$player2
WCE19_TFg3 <- WCE19_TFg2
WCE19_TFg3$p1inp2vec <- is.element(WCE19_TFg3$player1, player2vector)
WCE19_TFg3$p2inp1vec <- is.element(WCE19_TFg3$player2, player1vector)

addPlayer1 <- WCE19_TFg3[ which(WCE19_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_TFg3[ which(WCE19_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_TFg2 <- rbind(WCE19_TFg2, addPlayers)

#ROUND 19, FWD Turnover graph using weighted edges
WCE19_TFft <- ftable(WCE19_TFg2$player1, WCE19_TFg2$player2)
WCE19_TFft2 <- as.matrix(WCE19_TFft)
numRows <- nrow(WCE19_TFft2)
numCols <- ncol(WCE19_TFft2)
WCE19_TFft3 <- WCE19_TFft2[c(2:numRows) , c(2:numCols)]
WCE19_TFTable <- graph.adjacency(WCE19_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, FWD Turnover graph=weighted
plot.igraph(WCE19_TFTable, vertex.label = V(WCE19_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, FWD Turnover calulation of network metrics
#igraph
WCE19_TF.clusterCoef <- transitivity(WCE19_TFTable, type="global") #cluster coefficient
WCE19_TF.degreeCent <- centralization.degree(WCE19_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_TFftn <- as.network.matrix(WCE19_TFft)
WCE19_TF.netDensity <- network.density(WCE19_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_TF.entropy <- entropy(WCE19_TFft) #entropy

WCE19_TF.netMx <- cbind(WCE19_TF.netMx, WCE19_TF.clusterCoef, WCE19_TF.degreeCent$centralization,
                        WCE19_TF.netDensity, WCE19_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_TF.netMx) <- varnames

#ROUND 19, AM Stoppage**********************************************************
#NA

round = 19
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE19_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Stoppage with weighted edges
WCE19_SAMg2 <- data.frame(WCE19_SAM)
WCE19_SAMg2 <- WCE19_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_SAMg2$player1
player2vector <- WCE19_SAMg2$player2
WCE19_SAMg3 <- WCE19_SAMg2
WCE19_SAMg3$p1inp2vec <- is.element(WCE19_SAMg3$player1, player2vector)
WCE19_SAMg3$p2inp1vec <- is.element(WCE19_SAMg3$player2, player1vector)

addPlayer1 <- WCE19_SAMg3[ which(WCE19_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_SAMg3[ which(WCE19_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_SAMg2 <- rbind(WCE19_SAMg2, addPlayers)

#ROUND 19, AM Stoppage graph using weighted edges
WCE19_SAMft <- ftable(WCE19_SAMg2$player1, WCE19_SAMg2$player2)
WCE19_SAMft2 <- as.matrix(WCE19_SAMft)
numRows <- nrow(WCE19_SAMft2)
numCols <- ncol(WCE19_SAMft2)
WCE19_SAMft3 <- WCE19_SAMft2[c(2:numRows) , c(2:numCols)]
WCE19_SAMTable <- graph.adjacency(WCE19_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Stoppage graph=weighted
plot.igraph(WCE19_SAMTable, vertex.label = V(WCE19_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Stoppage calulation of network metrics
#igraph
WCE19_SAM.clusterCoef <- transitivity(WCE19_SAMTable, type="global") #cluster coefficient
WCE19_SAM.degreeCent <- centralization.degree(WCE19_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_SAMftn <- as.network.matrix(WCE19_SAMft)
WCE19_SAM.netDensity <- network.density(WCE19_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_SAM.entropy <- entropy(WCE19_SAMft) #entropy

WCE19_SAM.netMx <- cbind(WCE19_SAM.netMx, WCE19_SAM.clusterCoef, WCE19_SAM.degreeCent$centralization,
                         WCE19_SAM.netDensity, WCE19_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_SAM.netMx) <- varnames

#ROUND 19, AM Turnover**********************************************************

round = 19
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE19_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, AM Turnover with weighted edges
WCE19_TAMg2 <- data.frame(WCE19_TAM)
WCE19_TAMg2 <- WCE19_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_TAMg2$player1
player2vector <- WCE19_TAMg2$player2
WCE19_TAMg3 <- WCE19_TAMg2
WCE19_TAMg3$p1inp2vec <- is.element(WCE19_TAMg3$player1, player2vector)
WCE19_TAMg3$p2inp1vec <- is.element(WCE19_TAMg3$player2, player1vector)

addPlayer1 <- WCE19_TAMg3[ which(WCE19_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_TAMg3[ which(WCE19_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_TAMg2 <- rbind(WCE19_TAMg2, addPlayers)

#ROUND 19, AM Turnover graph using weighted edges
WCE19_TAMft <- ftable(WCE19_TAMg2$player1, WCE19_TAMg2$player2)
WCE19_TAMft2 <- as.matrix(WCE19_TAMft)
numRows <- nrow(WCE19_TAMft2)
numCols <- ncol(WCE19_TAMft2)
WCE19_TAMft3 <- WCE19_TAMft2[c(2:numRows) , c(2:numCols)]
WCE19_TAMTable <- graph.adjacency(WCE19_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, AM Turnover graph=weighted
plot.igraph(WCE19_TAMTable, vertex.label = V(WCE19_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, AM Turnover calulation of network metrics
#igraph
WCE19_TAM.clusterCoef <- transitivity(WCE19_TAMTable, type="global") #cluster coefficient
WCE19_TAM.degreeCent <- centralization.degree(WCE19_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_TAMftn <- as.network.matrix(WCE19_TAMft)
WCE19_TAM.netDensity <- network.density(WCE19_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_TAM.entropy <- entropy(WCE19_TAMft) #entropy

WCE19_TAM.netMx <- cbind(WCE19_TAM.netMx, WCE19_TAM.clusterCoef, WCE19_TAM.degreeCent$centralization,
                         WCE19_TAM.netDensity, WCE19_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_TAM.netMx) <- varnames

#ROUND 19, DM Stoppage**********************************************************
#NA

round = 19
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE19_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Stoppage with weighted edges
WCE19_SDMg2 <- data.frame(WCE19_SDM)
WCE19_SDMg2 <- WCE19_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_SDMg2$player1
player2vector <- WCE19_SDMg2$player2
WCE19_SDMg3 <- WCE19_SDMg2
WCE19_SDMg3$p1inp2vec <- is.element(WCE19_SDMg3$player1, player2vector)
WCE19_SDMg3$p2inp1vec <- is.element(WCE19_SDMg3$player2, player1vector)

addPlayer1 <- WCE19_SDMg3[ which(WCE19_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_SDMg3[ which(WCE19_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_SDMg2 <- rbind(WCE19_SDMg2, addPlayers)

#ROUND 19, DM Stoppage graph using weighted edges
WCE19_SDMft <- ftable(WCE19_SDMg2$player1, WCE19_SDMg2$player2)
WCE19_SDMft2 <- as.matrix(WCE19_SDMft)
numRows <- nrow(WCE19_SDMft2)
numCols <- ncol(WCE19_SDMft2)
WCE19_SDMft3 <- WCE19_SDMft2[c(2:numRows) , c(2:numCols)]
WCE19_SDMTable <- graph.adjacency(WCE19_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Stoppage graph=weighted
plot.igraph(WCE19_SDMTable, vertex.label = V(WCE19_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Stoppage calulation of network metrics
#igraph
WCE19_SDM.clusterCoef <- transitivity(WCE19_SDMTable, type="global") #cluster coefficient
WCE19_SDM.degreeCent <- centralization.degree(WCE19_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_SDMftn <- as.network.matrix(WCE19_SDMft)
WCE19_SDM.netDensity <- network.density(WCE19_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_SDM.entropy <- entropy(WCE19_SDMft) #entropy

WCE19_SDM.netMx <- cbind(WCE19_SDM.netMx, WCE19_SDM.clusterCoef, WCE19_SDM.degreeCent$centralization,
                         WCE19_SDM.netDensity, WCE19_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_SDM.netMx) <- varnames

#ROUND 19, DM Turnover**********************************************************

round = 19
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE19_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, DM Turnover with weighted edges
WCE19_TDMg2 <- data.frame(WCE19_TDM)
WCE19_TDMg2 <- WCE19_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_TDMg2$player1
player2vector <- WCE19_TDMg2$player2
WCE19_TDMg3 <- WCE19_TDMg2
WCE19_TDMg3$p1inp2vec <- is.element(WCE19_TDMg3$player1, player2vector)
WCE19_TDMg3$p2inp1vec <- is.element(WCE19_TDMg3$player2, player1vector)

addPlayer1 <- WCE19_TDMg3[ which(WCE19_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_TDMg3[ which(WCE19_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_TDMg2 <- rbind(WCE19_TDMg2, addPlayers)

#ROUND 19, DM Turnover graph using weighted edges
WCE19_TDMft <- ftable(WCE19_TDMg2$player1, WCE19_TDMg2$player2)
WCE19_TDMft2 <- as.matrix(WCE19_TDMft)
numRows <- nrow(WCE19_TDMft2)
numCols <- ncol(WCE19_TDMft2)
WCE19_TDMft3 <- WCE19_TDMft2[c(2:numRows) , c(2:numCols)]
WCE19_TDMTable <- graph.adjacency(WCE19_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 19, DM Turnover graph=weighted
plot.igraph(WCE19_TDMTable, vertex.label = V(WCE19_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, DM Turnover calulation of network metrics
#igraph
WCE19_TDM.clusterCoef <- transitivity(WCE19_TDMTable, type="global") #cluster coefficient
WCE19_TDM.degreeCent <- centralization.degree(WCE19_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_TDMftn <- as.network.matrix(WCE19_TDMft)
WCE19_TDM.netDensity <- network.density(WCE19_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_TDM.entropy <- entropy(WCE19_TDMft) #entropy

WCE19_TDM.netMx <- cbind(WCE19_TDM.netMx, WCE19_TDM.clusterCoef, WCE19_TDM.degreeCent$centralization,
                         WCE19_TDM.netDensity, WCE19_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_TDM.netMx) <- varnames

#ROUND 19, D Stoppage**********************************************************
#NA

round = 19
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE19_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Stoppage with weighted edges
WCE19_SDg2 <- data.frame(WCE19_SD)
WCE19_SDg2 <- WCE19_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_SDg2$player1
player2vector <- WCE19_SDg2$player2
WCE19_SDg3 <- WCE19_SDg2
WCE19_SDg3$p1inp2vec <- is.element(WCE19_SDg3$player1, player2vector)
WCE19_SDg3$p2inp1vec <- is.element(WCE19_SDg3$player2, player1vector)

addPlayer1 <- WCE19_SDg3[ which(WCE19_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_SDg3[ which(WCE19_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_SDg2 <- rbind(WCE19_SDg2, addPlayers)

#ROUND 19, D Stoppage graph using weighted edges
WCE19_SDft <- ftable(WCE19_SDg2$player1, WCE19_SDg2$player2)
WCE19_SDft2 <- as.matrix(WCE19_SDft)
numRows <- nrow(WCE19_SDft2)
numCols <- ncol(WCE19_SDft2)
WCE19_SDft3 <- WCE19_SDft2[c(2:numRows) , c(2:numCols)]
WCE19_SDTable <- graph.adjacency(WCE19_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Stoppage graph=weighted
plot.igraph(WCE19_SDTable, vertex.label = V(WCE19_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Stoppage calulation of network metrics
#igraph
WCE19_SD.clusterCoef <- transitivity(WCE19_SDTable, type="global") #cluster coefficient
WCE19_SD.degreeCent <- centralization.degree(WCE19_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_SDftn <- as.network.matrix(WCE19_SDft)
WCE19_SD.netDensity <- network.density(WCE19_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_SD.entropy <- entropy(WCE19_SDft) #entropy

WCE19_SD.netMx <- cbind(WCE19_SD.netMx, WCE19_SD.clusterCoef, WCE19_SD.degreeCent$centralization,
                        WCE19_SD.netDensity, WCE19_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_SD.netMx) <- varnames

#ROUND 19, D Turnover**********************************************************
#NA

round = 19
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE19_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, D Turnover with weighted edges
WCE19_TDg2 <- data.frame(WCE19_TD)
WCE19_TDg2 <- WCE19_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_TDg2$player1
player2vector <- WCE19_TDg2$player2
WCE19_TDg3 <- WCE19_TDg2
WCE19_TDg3$p1inp2vec <- is.element(WCE19_TDg3$player1, player2vector)
WCE19_TDg3$p2inp1vec <- is.element(WCE19_TDg3$player2, player1vector)

addPlayer1 <- WCE19_TDg3[ which(WCE19_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_TDg3[ which(WCE19_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_TDg2 <- rbind(WCE19_TDg2, addPlayers)

#ROUND 19, D Turnover graph using weighted edges
WCE19_TDft <- ftable(WCE19_TDg2$player1, WCE19_TDg2$player2)
WCE19_TDft2 <- as.matrix(WCE19_TDft)
numRows <- nrow(WCE19_TDft2)
numCols <- ncol(WCE19_TDft2)
WCE19_TDft3 <- WCE19_TDft2[c(2:numRows) , c(2:numCols)]
WCE19_TDTable <- graph.adjacency(WCE19_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, D Turnover graph=weighted
plot.igraph(WCE19_TDTable, vertex.label = V(WCE19_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, D Turnover calulation of network metrics
#igraph
WCE19_TD.clusterCoef <- transitivity(WCE19_TDTable, type="global") #cluster coefficient
WCE19_TD.degreeCent <- centralization.degree(WCE19_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_TDftn <- as.network.matrix(WCE19_TDft)
WCE19_TD.netDensity <- network.density(WCE19_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_TD.entropy <- entropy(WCE19_TDft) #entropy

WCE19_TD.netMx <- cbind(WCE19_TD.netMx, WCE19_TD.clusterCoef, WCE19_TD.degreeCent$centralization,
                        WCE19_TD.netDensity, WCE19_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_TD.netMx) <- varnames

#ROUND 19, End of Qtr**********************************************************
#NA

round = 19
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE19_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 19, End of Qtr with weighted edges
WCE19_QTg2 <- data.frame(WCE19_QT)
WCE19_QTg2 <- WCE19_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE19_QTg2$player1
player2vector <- WCE19_QTg2$player2
WCE19_QTg3 <- WCE19_QTg2
WCE19_QTg3$p1inp2vec <- is.element(WCE19_QTg3$player1, player2vector)
WCE19_QTg3$p2inp1vec <- is.element(WCE19_QTg3$player2, player1vector)

addPlayer1 <- WCE19_QTg3[ which(WCE19_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE19_QTg3[ which(WCE19_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE19_QTg2 <- rbind(WCE19_QTg2, addPlayers)

#ROUND 19, End of Qtr graph using weighted edges
WCE19_QTft <- ftable(WCE19_QTg2$player1, WCE19_QTg2$player2)
WCE19_QTft2 <- as.matrix(WCE19_QTft)
numRows <- nrow(WCE19_QTft2)
numCols <- ncol(WCE19_QTft2)
WCE19_QTft3 <- WCE19_QTft2[c(2:numRows) , c(2:numCols)]
WCE19_QTTable <- graph.adjacency(WCE19_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 19, End of Qtr graph=weighted
plot.igraph(WCE19_QTTable, vertex.label = V(WCE19_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE19_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 19, End of Qtr calulation of network metrics
#igraph
WCE19_QT.clusterCoef <- transitivity(WCE19_QTTable, type="global") #cluster coefficient
WCE19_QT.degreeCent <- centralization.degree(WCE19_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE19_QTftn <- as.network.matrix(WCE19_QTft)
WCE19_QT.netDensity <- network.density(WCE19_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE19_QT.entropy <- entropy(WCE19_QTft) #entropy

WCE19_QT.netMx <- cbind(WCE19_QT.netMx, WCE19_QT.clusterCoef, WCE19_QT.degreeCent$centralization,
                        WCE19_QT.netDensity, WCE19_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE19_QT.netMx) <- varnames
