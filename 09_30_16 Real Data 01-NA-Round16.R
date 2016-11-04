#####
#09-30-16- Real data 16
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
ADEL16_Gg2 <- data.frame(ADEL16_G)
ADEL16_Gg2 <- ADEL16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_Gg2$player1
player2vector <- ADEL16_Gg2$player2
ADEL16_Gg3 <- ADEL16_Gg2
ADEL16_Gg3$p1inp2vec <- is.element(ADEL16_Gg3$player1, player2vector)
ADEL16_Gg3$p2inp1vec <- is.element(ADEL16_Gg3$player2, player1vector)

addPlayer1 <- ADEL16_Gg3[ which(ADEL16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_Gg3[ which(ADEL16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_Gg2 <- rbind(ADEL16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
ADEL16_Gft <- ftable(ADEL16_Gg2$player1, ADEL16_Gg2$player2)
ADEL16_Gft2 <- as.matrix(ADEL16_Gft)
numRows <- nrow(ADEL16_Gft2)
numCols <- ncol(ADEL16_Gft2)
ADEL16_Gft3 <- ADEL16_Gft2[c(2:numRows) , c(2:numCols)]
ADEL16_GTable <- graph.adjacency(ADEL16_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(ADEL16_GTable, vertex.label = V(ADEL16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
ADEL16_G.clusterCoef <- transitivity(ADEL16_GTable, type="global") #cluster coefficient
ADEL16_G.degreeCent <- centralization.degree(ADEL16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_Gftn <- as.network.matrix(ADEL16_Gft)
ADEL16_G.netDensity <- network.density(ADEL16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_G.entropy <- entropy(ADEL16_Gft) #entropy

ADEL16_G.netMx <- cbind(ADEL16_G.netMx, ADEL16_G.clusterCoef, ADEL16_G.degreeCent$centralization,
                        ADEL16_G.netDensity, ADEL16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************

round = 16
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
ADEL16_Bg2 <- data.frame(ADEL16_B)
ADEL16_Bg2 <- ADEL16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_Bg2$player1
player2vector <- ADEL16_Bg2$player2
ADEL16_Bg3 <- ADEL16_Bg2
ADEL16_Bg3$p1inp2vec <- is.element(ADEL16_Bg3$player1, player2vector)
ADEL16_Bg3$p2inp1vec <- is.element(ADEL16_Bg3$player2, player1vector)

addPlayer1 <- ADEL16_Bg3[ which(ADEL16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_Bg3[ which(ADEL16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_Bg2 <- rbind(ADEL16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
ADEL16_Bft <- ftable(ADEL16_Bg2$player1, ADEL16_Bg2$player2)
ADEL16_Bft2 <- as.matrix(ADEL16_Bft)
numRows <- nrow(ADEL16_Bft2)
numCols <- ncol(ADEL16_Bft2)
ADEL16_Bft3 <- ADEL16_Bft2[c(2:numRows) , c(2:numCols)]
ADEL16_BTable <- graph.adjacency(ADEL16_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(ADEL16_BTable, vertex.label = V(ADEL16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
ADEL16_B.clusterCoef <- transitivity(ADEL16_BTable, type="global") #cluster coefficient
ADEL16_B.degreeCent <- centralization.degree(ADEL16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_Bftn <- as.network.matrix(ADEL16_Bft)
ADEL16_B.netDensity <- network.density(ADEL16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_B.entropy <- entropy(ADEL16_Bft) #entropy

ADEL16_B.netMx <- cbind(ADEL16_B.netMx, ADEL16_B.clusterCoef, ADEL16_B.degreeCent$centralization,
                        ADEL16_B.netDensity, ADEL16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
ADEL16_SFg2 <- data.frame(ADEL16_SF)
ADEL16_SFg2 <- ADEL16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_SFg2$player1
player2vector <- ADEL16_SFg2$player2
ADEL16_SFg3 <- ADEL16_SFg2
ADEL16_SFg3$p1inp2vec <- is.element(ADEL16_SFg3$player1, player2vector)
ADEL16_SFg3$p2inp1vec <- is.element(ADEL16_SFg3$player2, player1vector)

addPlayer1 <- ADEL16_SFg3[ which(ADEL16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_SFg3[ which(ADEL16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_SFg2 <- rbind(ADEL16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
ADEL16_SFft <- ftable(ADEL16_SFg2$player1, ADEL16_SFg2$player2)
ADEL16_SFft2 <- as.matrix(ADEL16_SFft)
numRows <- nrow(ADEL16_SFft2)
numCols <- ncol(ADEL16_SFft2)
ADEL16_SFft3 <- ADEL16_SFft2[c(2:numRows) , c(2:numCols)]
ADEL16_SFTable <- graph.adjacency(ADEL16_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(ADEL16_SFTable, vertex.label = V(ADEL16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
ADEL16_SF.clusterCoef <- transitivity(ADEL16_SFTable, type="global") #cluster coefficient
ADEL16_SF.degreeCent <- centralization.degree(ADEL16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_SFftn <- as.network.matrix(ADEL16_SFft)
ADEL16_SF.netDensity <- network.density(ADEL16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_SF.entropy <- entropy(ADEL16_SFft) #entropy

ADEL16_SF.netMx <- cbind(ADEL16_SF.netMx, ADEL16_SF.clusterCoef, ADEL16_SF.degreeCent$centralization,
                         ADEL16_SF.netDensity, ADEL16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************
#NA

round = 16
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
ADEL16_TFg2 <- data.frame(ADEL16_TF)
ADEL16_TFg2 <- ADEL16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_TFg2$player1
player2vector <- ADEL16_TFg2$player2
ADEL16_TFg3 <- ADEL16_TFg2
ADEL16_TFg3$p1inp2vec <- is.element(ADEL16_TFg3$player1, player2vector)
ADEL16_TFg3$p2inp1vec <- is.element(ADEL16_TFg3$player2, player1vector)

addPlayer1 <- ADEL16_TFg3[ which(ADEL16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_TFg3[ which(ADEL16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_TFg2 <- rbind(ADEL16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
ADEL16_TFft <- ftable(ADEL16_TFg2$player1, ADEL16_TFg2$player2)
ADEL16_TFft2 <- as.matrix(ADEL16_TFft)
numRows <- nrow(ADEL16_TFft2)
numCols <- ncol(ADEL16_TFft2)
ADEL16_TFft3 <- ADEL16_TFft2[c(2:numRows) , c(2:numCols)]
ADEL16_TFTable <- graph.adjacency(ADEL16_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(ADEL16_TFTable, vertex.label = V(ADEL16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
ADEL16_TF.clusterCoef <- transitivity(ADEL16_TFTable, type="global") #cluster coefficient
ADEL16_TF.degreeCent <- centralization.degree(ADEL16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_TFftn <- as.network.matrix(ADEL16_TFft)
ADEL16_TF.netDensity <- network.density(ADEL16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_TF.entropy <- entropy(ADEL16_TFft) #entropy

ADEL16_TF.netMx <- cbind(ADEL16_TF.netMx, ADEL16_TF.clusterCoef, ADEL16_TF.degreeCent$centralization,
                         ADEL16_TF.netDensity, ADEL16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
ADEL16_SAMg2 <- data.frame(ADEL16_SAM)
ADEL16_SAMg2 <- ADEL16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_SAMg2$player1
player2vector <- ADEL16_SAMg2$player2
ADEL16_SAMg3 <- ADEL16_SAMg2
ADEL16_SAMg3$p1inp2vec <- is.element(ADEL16_SAMg3$player1, player2vector)
ADEL16_SAMg3$p2inp1vec <- is.element(ADEL16_SAMg3$player2, player1vector)

addPlayer1 <- ADEL16_SAMg3[ which(ADEL16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_SAMg3[ which(ADEL16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_SAMg2 <- rbind(ADEL16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
ADEL16_SAMft <- ftable(ADEL16_SAMg2$player1, ADEL16_SAMg2$player2)
ADEL16_SAMft2 <- as.matrix(ADEL16_SAMft)
numRows <- nrow(ADEL16_SAMft2)
numCols <- ncol(ADEL16_SAMft2)
ADEL16_SAMft3 <- ADEL16_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL16_SAMTable <- graph.adjacency(ADEL16_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(ADEL16_SAMTable, vertex.label = V(ADEL16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
ADEL16_SAM.clusterCoef <- transitivity(ADEL16_SAMTable, type="global") #cluster coefficient
ADEL16_SAM.degreeCent <- centralization.degree(ADEL16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_SAMftn <- as.network.matrix(ADEL16_SAMft)
ADEL16_SAM.netDensity <- network.density(ADEL16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_SAM.entropy <- entropy(ADEL16_SAMft) #entropy

ADEL16_SAM.netMx <- cbind(ADEL16_SAM.netMx, ADEL16_SAM.clusterCoef, ADEL16_SAM.degreeCent$centralization,
                          ADEL16_SAM.netDensity, ADEL16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
ADEL16_TAMg2 <- data.frame(ADEL16_TAM)
ADEL16_TAMg2 <- ADEL16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_TAMg2$player1
player2vector <- ADEL16_TAMg2$player2
ADEL16_TAMg3 <- ADEL16_TAMg2
ADEL16_TAMg3$p1inp2vec <- is.element(ADEL16_TAMg3$player1, player2vector)
ADEL16_TAMg3$p2inp1vec <- is.element(ADEL16_TAMg3$player2, player1vector)

addPlayer1 <- ADEL16_TAMg3[ which(ADEL16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_TAMg3[ which(ADEL16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_TAMg2 <- rbind(ADEL16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
ADEL16_TAMft <- ftable(ADEL16_TAMg2$player1, ADEL16_TAMg2$player2)
ADEL16_TAMft2 <- as.matrix(ADEL16_TAMft)
numRows <- nrow(ADEL16_TAMft2)
numCols <- ncol(ADEL16_TAMft2)
ADEL16_TAMft3 <- ADEL16_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL16_TAMTable <- graph.adjacency(ADEL16_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(ADEL16_TAMTable, vertex.label = V(ADEL16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
ADEL16_TAM.clusterCoef <- transitivity(ADEL16_TAMTable, type="global") #cluster coefficient
ADEL16_TAM.degreeCent <- centralization.degree(ADEL16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_TAMftn <- as.network.matrix(ADEL16_TAMft)
ADEL16_TAM.netDensity <- network.density(ADEL16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_TAM.entropy <- entropy(ADEL16_TAMft) #entropy

ADEL16_TAM.netMx <- cbind(ADEL16_TAM.netMx, ADEL16_TAM.clusterCoef, ADEL16_TAM.degreeCent$centralization,
                          ADEL16_TAM.netDensity, ADEL16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
ADEL16_SDMg2 <- data.frame(ADEL16_SDM)
ADEL16_SDMg2 <- ADEL16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_SDMg2$player1
player2vector <- ADEL16_SDMg2$player2
ADEL16_SDMg3 <- ADEL16_SDMg2
ADEL16_SDMg3$p1inp2vec <- is.element(ADEL16_SDMg3$player1, player2vector)
ADEL16_SDMg3$p2inp1vec <- is.element(ADEL16_SDMg3$player2, player1vector)

addPlayer1 <- ADEL16_SDMg3[ which(ADEL16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_SDMg3[ which(ADEL16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_SDMg2 <- rbind(ADEL16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
ADEL16_SDMft <- ftable(ADEL16_SDMg2$player1, ADEL16_SDMg2$player2)
ADEL16_SDMft2 <- as.matrix(ADEL16_SDMft)
numRows <- nrow(ADEL16_SDMft2)
numCols <- ncol(ADEL16_SDMft2)
ADEL16_SDMft3 <- ADEL16_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL16_SDMTable <- graph.adjacency(ADEL16_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(ADEL16_SDMTable, vertex.label = V(ADEL16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
ADEL16_SDM.clusterCoef <- transitivity(ADEL16_SDMTable, type="global") #cluster coefficient
ADEL16_SDM.degreeCent <- centralization.degree(ADEL16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_SDMftn <- as.network.matrix(ADEL16_SDMft)
ADEL16_SDM.netDensity <- network.density(ADEL16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_SDM.entropy <- entropy(ADEL16_SDMft) #entropy

ADEL16_SDM.netMx <- cbind(ADEL16_SDM.netMx, ADEL16_SDM.clusterCoef, ADEL16_SDM.degreeCent$centralization,
                          ADEL16_SDM.netDensity, ADEL16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
ADEL16_TDMg2 <- data.frame(ADEL16_TDM)
ADEL16_TDMg2 <- ADEL16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_TDMg2$player1
player2vector <- ADEL16_TDMg2$player2
ADEL16_TDMg3 <- ADEL16_TDMg2
ADEL16_TDMg3$p1inp2vec <- is.element(ADEL16_TDMg3$player1, player2vector)
ADEL16_TDMg3$p2inp1vec <- is.element(ADEL16_TDMg3$player2, player1vector)

addPlayer1 <- ADEL16_TDMg3[ which(ADEL16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_TDMg3[ which(ADEL16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_TDMg2 <- rbind(ADEL16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
ADEL16_TDMft <- ftable(ADEL16_TDMg2$player1, ADEL16_TDMg2$player2)
ADEL16_TDMft2 <- as.matrix(ADEL16_TDMft)
numRows <- nrow(ADEL16_TDMft2)
numCols <- ncol(ADEL16_TDMft2)
ADEL16_TDMft3 <- ADEL16_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL16_TDMTable <- graph.adjacency(ADEL16_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(ADEL16_TDMTable, vertex.label = V(ADEL16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
ADEL16_TDM.clusterCoef <- transitivity(ADEL16_TDMTable, type="global") #cluster coefficient
ADEL16_TDM.degreeCent <- centralization.degree(ADEL16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_TDMftn <- as.network.matrix(ADEL16_TDMft)
ADEL16_TDM.netDensity <- network.density(ADEL16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_TDM.entropy <- entropy(ADEL16_TDMft) #entropy

ADEL16_TDM.netMx <- cbind(ADEL16_TDM.netMx, ADEL16_TDM.clusterCoef, ADEL16_TDM.degreeCent$centralization,
                          ADEL16_TDM.netDensity, ADEL16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************

round = 16
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
ADEL16_SDg2 <- data.frame(ADEL16_SD)
ADEL16_SDg2 <- ADEL16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_SDg2$player1
player2vector <- ADEL16_SDg2$player2
ADEL16_SDg3 <- ADEL16_SDg2
ADEL16_SDg3$p1inp2vec <- is.element(ADEL16_SDg3$player1, player2vector)
ADEL16_SDg3$p2inp1vec <- is.element(ADEL16_SDg3$player2, player1vector)

addPlayer1 <- ADEL16_SDg3[ which(ADEL16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_SDg3[ which(ADEL16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_SDg2 <- rbind(ADEL16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
ADEL16_SDft <- ftable(ADEL16_SDg2$player1, ADEL16_SDg2$player2)
ADEL16_SDft2 <- as.matrix(ADEL16_SDft)
numRows <- nrow(ADEL16_SDft2)
numCols <- ncol(ADEL16_SDft2)
ADEL16_SDft3 <- ADEL16_SDft2[c(2:numRows) , c(2:numCols)]
ADEL16_SDTable <- graph.adjacency(ADEL16_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(ADEL16_SDTable, vertex.label = V(ADEL16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
ADEL16_SD.clusterCoef <- transitivity(ADEL16_SDTable, type="global") #cluster coefficient
ADEL16_SD.degreeCent <- centralization.degree(ADEL16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_SDftn <- as.network.matrix(ADEL16_SDft)
ADEL16_SD.netDensity <- network.density(ADEL16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_SD.entropy <- entropy(ADEL16_SDft) #entropy

ADEL16_SD.netMx <- cbind(ADEL16_SD.netMx, ADEL16_SD.clusterCoef, ADEL16_SD.degreeCent$centralization,
                         ADEL16_SD.netDensity, ADEL16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
ADEL16_TDg2 <- data.frame(ADEL16_TD)
ADEL16_TDg2 <- ADEL16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_TDg2$player1
player2vector <- ADEL16_TDg2$player2
ADEL16_TDg3 <- ADEL16_TDg2
ADEL16_TDg3$p1inp2vec <- is.element(ADEL16_TDg3$player1, player2vector)
ADEL16_TDg3$p2inp1vec <- is.element(ADEL16_TDg3$player2, player1vector)

addPlayer1 <- ADEL16_TDg3[ which(ADEL16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_TDg3[ which(ADEL16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_TDg2 <- rbind(ADEL16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
ADEL16_TDft <- ftable(ADEL16_TDg2$player1, ADEL16_TDg2$player2)
ADEL16_TDft2 <- as.matrix(ADEL16_TDft)
numRows <- nrow(ADEL16_TDft2)
numCols <- ncol(ADEL16_TDft2)
ADEL16_TDft3 <- ADEL16_TDft2[c(2:numRows) , c(2:numCols)]
ADEL16_TDTable <- graph.adjacency(ADEL16_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(ADEL16_TDTable, vertex.label = V(ADEL16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
ADEL16_TD.clusterCoef <- transitivity(ADEL16_TDTable, type="global") #cluster coefficient
ADEL16_TD.degreeCent <- centralization.degree(ADEL16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_TDftn <- as.network.matrix(ADEL16_TDft)
ADEL16_TD.netDensity <- network.density(ADEL16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_TD.entropy <- entropy(ADEL16_TDft) #entropy

ADEL16_TD.netMx <- cbind(ADEL16_TD.netMx, ADEL16_TD.clusterCoef, ADEL16_TD.degreeCent$centralization,
                         ADEL16_TD.netDensity, ADEL16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
ADEL16_QTg2 <- data.frame(ADEL16_QT)
ADEL16_QTg2 <- ADEL16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL16_QTg2$player1
player2vector <- ADEL16_QTg2$player2
ADEL16_QTg3 <- ADEL16_QTg2
ADEL16_QTg3$p1inp2vec <- is.element(ADEL16_QTg3$player1, player2vector)
ADEL16_QTg3$p2inp1vec <- is.element(ADEL16_QTg3$player2, player1vector)

addPlayer1 <- ADEL16_QTg3[ which(ADEL16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL16_QTg3[ which(ADEL16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL16_QTg2 <- rbind(ADEL16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
ADEL16_QTft <- ftable(ADEL16_QTg2$player1, ADEL16_QTg2$player2)
ADEL16_QTft2 <- as.matrix(ADEL16_QTft)
numRows <- nrow(ADEL16_QTft2)
numCols <- ncol(ADEL16_QTft2)
ADEL16_QTft3 <- ADEL16_QTft2[c(2:numRows) , c(2:numCols)]
ADEL16_QTTable <- graph.adjacency(ADEL16_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(ADEL16_QTTable, vertex.label = V(ADEL16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
ADEL16_QT.clusterCoef <- transitivity(ADEL16_QTTable, type="global") #cluster coefficient
ADEL16_QT.degreeCent <- centralization.degree(ADEL16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL16_QTftn <- as.network.matrix(ADEL16_QTft)
ADEL16_QT.netDensity <- network.density(ADEL16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL16_QT.entropy <- entropy(ADEL16_QTft) #entropy

ADEL16_QT.netMx <- cbind(ADEL16_QT.netMx, ADEL16_QT.clusterCoef, ADEL16_QT.degreeCent$centralization,
                         ADEL16_QT.netDensity, ADEL16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL16_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "BL"
KIoutcome = "Goal_F"
BL16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
BL16_Gg2 <- data.frame(BL16_G)
BL16_Gg2 <- BL16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_Gg2$player1
player2vector <- BL16_Gg2$player2
BL16_Gg3 <- BL16_Gg2
BL16_Gg3$p1inp2vec <- is.element(BL16_Gg3$player1, player2vector)
BL16_Gg3$p2inp1vec <- is.element(BL16_Gg3$player2, player1vector)

addPlayer1 <- BL16_Gg3[ which(BL16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_Gg3[ which(BL16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_Gg2 <- rbind(BL16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
BL16_Gft <- ftable(BL16_Gg2$player1, BL16_Gg2$player2)
BL16_Gft2 <- as.matrix(BL16_Gft)
numRows <- nrow(BL16_Gft2)
numCols <- ncol(BL16_Gft2)
BL16_Gft3 <- BL16_Gft2[c(2:numRows) , c(2:numCols)]
BL16_GTable <- graph.adjacency(BL16_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(BL16_GTable, vertex.label = V(BL16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
BL16_G.clusterCoef <- transitivity(BL16_GTable, type="global") #cluster coefficient
BL16_G.degreeCent <- centralization.degree(BL16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_Gftn <- as.network.matrix(BL16_Gft)
BL16_G.netDensity <- network.density(BL16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_G.entropy <- entropy(BL16_Gft) #entropy

BL16_G.netMx <- cbind(BL16_G.netMx, BL16_G.clusterCoef, BL16_G.degreeCent$centralization,
                      BL16_G.netDensity, BL16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************

round = 16
teamName = "BL"
KIoutcome = "Behind_F"
BL16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
BL16_Bg2 <- data.frame(BL16_B)
BL16_Bg2 <- BL16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_Bg2$player1
player2vector <- BL16_Bg2$player2
BL16_Bg3 <- BL16_Bg2
BL16_Bg3$p1inp2vec <- is.element(BL16_Bg3$player1, player2vector)
BL16_Bg3$p2inp1vec <- is.element(BL16_Bg3$player2, player1vector)

addPlayer1 <- BL16_Bg3[ which(BL16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL16_Bg3[ which(BL16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_Bg2 <- rbind(BL16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
BL16_Bft <- ftable(BL16_Bg2$player1, BL16_Bg2$player2)
BL16_Bft2 <- as.matrix(BL16_Bft)
numRows <- nrow(BL16_Bft2)
numCols <- ncol(BL16_Bft2)
BL16_Bft3 <- BL16_Bft2[c(2:numRows) , c(2:numCols)]
BL16_BTable <- graph.adjacency(BL16_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(BL16_BTable, vertex.label = V(BL16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
BL16_B.clusterCoef <- transitivity(BL16_BTable, type="global") #cluster coefficient
BL16_B.degreeCent <- centralization.degree(BL16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_Bftn <- as.network.matrix(BL16_Bft)
BL16_B.netDensity <- network.density(BL16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_B.entropy <- entropy(BL16_Bft) #entropy

BL16_B.netMx <- cbind(BL16_B.netMx, BL16_B.clusterCoef, BL16_B.degreeCent$centralization,
                      BL16_B.netDensity, BL16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "BL"
KIoutcome = "Stoppage_F"
BL16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
BL16_SFg2 <- data.frame(BL16_SF)
BL16_SFg2 <- BL16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_SFg2$player1
player2vector <- BL16_SFg2$player2
BL16_SFg3 <- BL16_SFg2
BL16_SFg3$p1inp2vec <- is.element(BL16_SFg3$player1, player2vector)
BL16_SFg3$p2inp1vec <- is.element(BL16_SFg3$player2, player1vector)

addPlayer1 <- BL16_SFg3[ which(BL16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_SFg3[ which(BL16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_SFg2 <- rbind(BL16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
BL16_SFft <- ftable(BL16_SFg2$player1, BL16_SFg2$player2)
BL16_SFft2 <- as.matrix(BL16_SFft)
numRows <- nrow(BL16_SFft2)
numCols <- ncol(BL16_SFft2)
BL16_SFft3 <- BL16_SFft2[c(2:numRows) , c(2:numCols)]
BL16_SFTable <- graph.adjacency(BL16_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(BL16_SFTable, vertex.label = V(BL16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
BL16_SF.clusterCoef <- transitivity(BL16_SFTable, type="global") #cluster coefficient
BL16_SF.degreeCent <- centralization.degree(BL16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_SFftn <- as.network.matrix(BL16_SFft)
BL16_SF.netDensity <- network.density(BL16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_SF.entropy <- entropy(BL16_SFft) #entropy

BL16_SF.netMx <- cbind(BL16_SF.netMx, BL16_SF.clusterCoef, BL16_SF.degreeCent$centralization,
                       BL16_SF.netDensity, BL16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "BL"
KIoutcome = "Turnover_F"
BL16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
BL16_TFg2 <- data.frame(BL16_TF)
BL16_TFg2 <- BL16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_TFg2$player1
player2vector <- BL16_TFg2$player2
BL16_TFg3 <- BL16_TFg2
BL16_TFg3$p1inp2vec <- is.element(BL16_TFg3$player1, player2vector)
BL16_TFg3$p2inp1vec <- is.element(BL16_TFg3$player2, player1vector)

addPlayer1 <- BL16_TFg3[ which(BL16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_TFg3[ which(BL16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_TFg2 <- rbind(BL16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
BL16_TFft <- ftable(BL16_TFg2$player1, BL16_TFg2$player2)
BL16_TFft2 <- as.matrix(BL16_TFft)
numRows <- nrow(BL16_TFft2)
numCols <- ncol(BL16_TFft2)
BL16_TFft3 <- BL16_TFft2[c(2:numRows) , c(2:numCols)]
BL16_TFTable <- graph.adjacency(BL16_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(BL16_TFTable, vertex.label = V(BL16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
BL16_TF.clusterCoef <- transitivity(BL16_TFTable, type="global") #cluster coefficient
BL16_TF.degreeCent <- centralization.degree(BL16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_TFftn <- as.network.matrix(BL16_TFft)
BL16_TF.netDensity <- network.density(BL16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_TF.entropy <- entropy(BL16_TFft) #entropy

BL16_TF.netMx <- cbind(BL16_TF.netMx, BL16_TF.clusterCoef, BL16_TF.degreeCent$centralization,
                       BL16_TF.netDensity, BL16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
BL16_SAMg2 <- data.frame(BL16_SAM)
BL16_SAMg2 <- BL16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_SAMg2$player1
player2vector <- BL16_SAMg2$player2
BL16_SAMg3 <- BL16_SAMg2
BL16_SAMg3$p1inp2vec <- is.element(BL16_SAMg3$player1, player2vector)
BL16_SAMg3$p2inp1vec <- is.element(BL16_SAMg3$player2, player1vector)

addPlayer1 <- BL16_SAMg3[ which(BL16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_SAMg3[ which(BL16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_SAMg2 <- rbind(BL16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
BL16_SAMft <- ftable(BL16_SAMg2$player1, BL16_SAMg2$player2)
BL16_SAMft2 <- as.matrix(BL16_SAMft)
numRows <- nrow(BL16_SAMft2)
numCols <- ncol(BL16_SAMft2)
BL16_SAMft3 <- BL16_SAMft2[c(2:numRows) , c(2:numCols)]
BL16_SAMTable <- graph.adjacency(BL16_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(BL16_SAMTable, vertex.label = V(BL16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
BL16_SAM.clusterCoef <- transitivity(BL16_SAMTable, type="global") #cluster coefficient
BL16_SAM.degreeCent <- centralization.degree(BL16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_SAMftn <- as.network.matrix(BL16_SAMft)
BL16_SAM.netDensity <- network.density(BL16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_SAM.entropy <- entropy(BL16_SAMft) #entropy

BL16_SAM.netMx <- cbind(BL16_SAM.netMx, BL16_SAM.clusterCoef, BL16_SAM.degreeCent$centralization,
                        BL16_SAM.netDensity, BL16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "BL"
KIoutcome = "Turnover_AM"
BL16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
BL16_TAMg2 <- data.frame(BL16_TAM)
BL16_TAMg2 <- BL16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_TAMg2$player1
player2vector <- BL16_TAMg2$player2
BL16_TAMg3 <- BL16_TAMg2
BL16_TAMg3$p1inp2vec <- is.element(BL16_TAMg3$player1, player2vector)
BL16_TAMg3$p2inp1vec <- is.element(BL16_TAMg3$player2, player1vector)

addPlayer1 <- BL16_TAMg3[ which(BL16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_TAMg3[ which(BL16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_TAMg2 <- rbind(BL16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
BL16_TAMft <- ftable(BL16_TAMg2$player1, BL16_TAMg2$player2)
BL16_TAMft2 <- as.matrix(BL16_TAMft)
numRows <- nrow(BL16_TAMft2)
numCols <- ncol(BL16_TAMft2)
BL16_TAMft3 <- BL16_TAMft2[c(2:numRows) , c(2:numCols)]
BL16_TAMTable <- graph.adjacency(BL16_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(BL16_TAMTable, vertex.label = V(BL16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
BL16_TAM.clusterCoef <- transitivity(BL16_TAMTable, type="global") #cluster coefficient
BL16_TAM.degreeCent <- centralization.degree(BL16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_TAMftn <- as.network.matrix(BL16_TAMft)
BL16_TAM.netDensity <- network.density(BL16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_TAM.entropy <- entropy(BL16_TAMft) #entropy

BL16_TAM.netMx <- cbind(BL16_TAM.netMx, BL16_TAM.clusterCoef, BL16_TAM.degreeCent$centralization,
                        BL16_TAM.netDensity, BL16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************
#NA

round = 16
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
BL16_SDMg2 <- data.frame(BL16_SDM)
BL16_SDMg2 <- BL16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_SDMg2$player1
player2vector <- BL16_SDMg2$player2
BL16_SDMg3 <- BL16_SDMg2
BL16_SDMg3$p1inp2vec <- is.element(BL16_SDMg3$player1, player2vector)
BL16_SDMg3$p2inp1vec <- is.element(BL16_SDMg3$player2, player1vector)

addPlayer1 <- BL16_SDMg3[ which(BL16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_SDMg3[ which(BL16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_SDMg2 <- rbind(BL16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
BL16_SDMft <- ftable(BL16_SDMg2$player1, BL16_SDMg2$player2)
BL16_SDMft2 <- as.matrix(BL16_SDMft)
numRows <- nrow(BL16_SDMft2)
numCols <- ncol(BL16_SDMft2)
BL16_SDMft3 <- BL16_SDMft2[c(2:numRows) , c(2:numCols)]
BL16_SDMTable <- graph.adjacency(BL16_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(BL16_SDMTable, vertex.label = V(BL16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
BL16_SDM.clusterCoef <- transitivity(BL16_SDMTable, type="global") #cluster coefficient
BL16_SDM.degreeCent <- centralization.degree(BL16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_SDMftn <- as.network.matrix(BL16_SDMft)
BL16_SDM.netDensity <- network.density(BL16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_SDM.entropy <- entropy(BL16_SDMft) #entropy

BL16_SDM.netMx <- cbind(BL16_SDM.netMx, BL16_SDM.clusterCoef, BL16_SDM.degreeCent$centralization,
                        BL16_SDM.netDensity, BL16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "BL"
KIoutcome = "Turnover_DM"
BL16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
BL16_TDMg2 <- data.frame(BL16_TDM)
BL16_TDMg2 <- BL16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_TDMg2$player1
player2vector <- BL16_TDMg2$player2
BL16_TDMg3 <- BL16_TDMg2
BL16_TDMg3$p1inp2vec <- is.element(BL16_TDMg3$player1, player2vector)
BL16_TDMg3$p2inp1vec <- is.element(BL16_TDMg3$player2, player1vector)

addPlayer1 <- BL16_TDMg3[ which(BL16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_TDMg3[ which(BL16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_TDMg2 <- rbind(BL16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
BL16_TDMft <- ftable(BL16_TDMg2$player1, BL16_TDMg2$player2)
BL16_TDMft2 <- as.matrix(BL16_TDMft)
numRows <- nrow(BL16_TDMft2)
numCols <- ncol(BL16_TDMft2)
BL16_TDMft3 <- BL16_TDMft2[c(2:numRows) , c(2:numCols)]
BL16_TDMTable <- graph.adjacency(BL16_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(BL16_TDMTable, vertex.label = V(BL16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
BL16_TDM.clusterCoef <- transitivity(BL16_TDMTable, type="global") #cluster coefficient
BL16_TDM.degreeCent <- centralization.degree(BL16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_TDMftn <- as.network.matrix(BL16_TDMft)
BL16_TDM.netDensity <- network.density(BL16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_TDM.entropy <- entropy(BL16_TDMft) #entropy

BL16_TDM.netMx <- cbind(BL16_TDM.netMx, BL16_TDM.clusterCoef, BL16_TDM.degreeCent$centralization,
                        BL16_TDM.netDensity, BL16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************

round = 16
teamName = "BL"
KIoutcome = "Stoppage_D"
BL16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
BL16_SDg2 <- data.frame(BL16_SD)
BL16_SDg2 <- BL16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_SDg2$player1
player2vector <- BL16_SDg2$player2
BL16_SDg3 <- BL16_SDg2
BL16_SDg3$p1inp2vec <- is.element(BL16_SDg3$player1, player2vector)
BL16_SDg3$p2inp1vec <- is.element(BL16_SDg3$player2, player1vector)

addPlayer1 <- BL16_SDg3[ which(BL16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_SDg3[ which(BL16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_SDg2 <- rbind(BL16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
BL16_SDft <- ftable(BL16_SDg2$player1, BL16_SDg2$player2)
BL16_SDft2 <- as.matrix(BL16_SDft)
numRows <- nrow(BL16_SDft2)
numCols <- ncol(BL16_SDft2)
BL16_SDft3 <- BL16_SDft2[c(2:numRows) , c(2:numCols)]
BL16_SDTable <- graph.adjacency(BL16_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(BL16_SDTable, vertex.label = V(BL16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
BL16_SD.clusterCoef <- transitivity(BL16_SDTable, type="global") #cluster coefficient
BL16_SD.degreeCent <- centralization.degree(BL16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_SDftn <- as.network.matrix(BL16_SDft)
BL16_SD.netDensity <- network.density(BL16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_SD.entropy <- entropy(BL16_SDft) #entropy

BL16_SD.netMx <- cbind(BL16_SD.netMx, BL16_SD.clusterCoef, BL16_SD.degreeCent$centralization,
                       BL16_SD.netDensity, BL16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "BL"
KIoutcome = "Turnover_D"
BL16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
BL16_TDg2 <- data.frame(BL16_TD)
BL16_TDg2 <- BL16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_TDg2$player1
player2vector <- BL16_TDg2$player2
BL16_TDg3 <- BL16_TDg2
BL16_TDg3$p1inp2vec <- is.element(BL16_TDg3$player1, player2vector)
BL16_TDg3$p2inp1vec <- is.element(BL16_TDg3$player2, player1vector)

addPlayer1 <- BL16_TDg3[ which(BL16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_TDg3[ which(BL16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_TDg2 <- rbind(BL16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
BL16_TDft <- ftable(BL16_TDg2$player1, BL16_TDg2$player2)
BL16_TDft2 <- as.matrix(BL16_TDft)
numRows <- nrow(BL16_TDft2)
numCols <- ncol(BL16_TDft2)
BL16_TDft3 <- BL16_TDft2[c(2:numRows) , c(2:numCols)]
BL16_TDTable <- graph.adjacency(BL16_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(BL16_TDTable, vertex.label = V(BL16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
BL16_TD.clusterCoef <- transitivity(BL16_TDTable, type="global") #cluster coefficient
BL16_TD.degreeCent <- centralization.degree(BL16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_TDftn <- as.network.matrix(BL16_TDft)
BL16_TD.netDensity <- network.density(BL16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_TD.entropy <- entropy(BL16_TDft) #entropy

BL16_TD.netMx <- cbind(BL16_TD.netMx, BL16_TD.clusterCoef, BL16_TD.degreeCent$centralization,
                       BL16_TD.netDensity, BL16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
BL16_QTg2 <- data.frame(BL16_QT)
BL16_QTg2 <- BL16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL16_QTg2$player1
player2vector <- BL16_QTg2$player2
BL16_QTg3 <- BL16_QTg2
BL16_QTg3$p1inp2vec <- is.element(BL16_QTg3$player1, player2vector)
BL16_QTg3$p2inp1vec <- is.element(BL16_QTg3$player2, player1vector)

addPlayer1 <- BL16_QTg3[ which(BL16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL16_QTg3[ which(BL16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL16_QTg2 <- rbind(BL16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
BL16_QTft <- ftable(BL16_QTg2$player1, BL16_QTg2$player2)
BL16_QTft2 <- as.matrix(BL16_QTft)
numRows <- nrow(BL16_QTft2)
numCols <- ncol(BL16_QTft2)
BL16_QTft3 <- BL16_QTft2[c(2:numRows) , c(2:numCols)]
BL16_QTTable <- graph.adjacency(BL16_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(BL16_QTTable, vertex.label = V(BL16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
BL16_QT.clusterCoef <- transitivity(BL16_QTTable, type="global") #cluster coefficient
BL16_QT.degreeCent <- centralization.degree(BL16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL16_QTftn <- as.network.matrix(BL16_QTft)
BL16_QT.netDensity <- network.density(BL16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL16_QT.entropy <- entropy(BL16_QTft) #entropy

BL16_QT.netMx <- cbind(BL16_QT.netMx, BL16_QT.clusterCoef, BL16_QT.degreeCent$centralization,
                       BL16_QT.netDensity, BL16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL16_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************

round = 16
teamName = "CARL"
KIoutcome = "Goal_F"
CARL16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
CARL16_Gg2 <- data.frame(CARL16_G)
CARL16_Gg2 <- CARL16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_Gg2$player1
player2vector <- CARL16_Gg2$player2
CARL16_Gg3 <- CARL16_Gg2
CARL16_Gg3$p1inp2vec <- is.element(CARL16_Gg3$player1, player2vector)
CARL16_Gg3$p2inp1vec <- is.element(CARL16_Gg3$player2, player1vector)

addPlayer1 <- CARL16_Gg3[ which(CARL16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_Gg3[ which(CARL16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_Gg2 <- rbind(CARL16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
CARL16_Gft <- ftable(CARL16_Gg2$player1, CARL16_Gg2$player2)
CARL16_Gft2 <- as.matrix(CARL16_Gft)
numRows <- nrow(CARL16_Gft2)
numCols <- ncol(CARL16_Gft2)
CARL16_Gft3 <- CARL16_Gft2[c(2:numRows) , c(2:numCols)]
CARL16_GTable <- graph.adjacency(CARL16_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(CARL16_GTable, vertex.label = V(CARL16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
CARL16_G.clusterCoef <- transitivity(CARL16_GTable, type="global") #cluster coefficient
CARL16_G.degreeCent <- centralization.degree(CARL16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_Gftn <- as.network.matrix(CARL16_Gft)
CARL16_G.netDensity <- network.density(CARL16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_G.entropy <- entropy(CARL16_Gft) #entropy

CARL16_G.netMx <- cbind(CARL16_G.netMx, CARL16_G.clusterCoef, CARL16_G.degreeCent$centralization,
                        CARL16_G.netDensity, CARL16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************

round = 16
teamName = "CARL"
KIoutcome = "Behind_F"
CARL16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
CARL16_Bg2 <- data.frame(CARL16_B)
CARL16_Bg2 <- CARL16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_Bg2$player1
player2vector <- CARL16_Bg2$player2
CARL16_Bg3 <- CARL16_Bg2
CARL16_Bg3$p1inp2vec <- is.element(CARL16_Bg3$player1, player2vector)
CARL16_Bg3$p2inp1vec <- is.element(CARL16_Bg3$player2, player1vector)

addPlayer1 <- CARL16_Bg3[ which(CARL16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_Bg3[ which(CARL16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_Bg2 <- rbind(CARL16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
CARL16_Bft <- ftable(CARL16_Bg2$player1, CARL16_Bg2$player2)
CARL16_Bft2 <- as.matrix(CARL16_Bft)
numRows <- nrow(CARL16_Bft2)
numCols <- ncol(CARL16_Bft2)
CARL16_Bft3 <- CARL16_Bft2[c(2:numRows) , c(2:numCols)]
CARL16_BTable <- graph.adjacency(CARL16_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(CARL16_BTable, vertex.label = V(CARL16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
CARL16_B.clusterCoef <- transitivity(CARL16_BTable, type="global") #cluster coefficient
CARL16_B.degreeCent <- centralization.degree(CARL16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_Bftn <- as.network.matrix(CARL16_Bft)
CARL16_B.netDensity <- network.density(CARL16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_B.entropy <- entropy(CARL16_Bft) #entropy

CARL16_B.netMx <- cbind(CARL16_B.netMx, CARL16_B.clusterCoef, CARL16_B.degreeCent$centralization,
                        CARL16_B.netDensity, CARL16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
CARL16_SFg2 <- data.frame(CARL16_SF)
CARL16_SFg2 <- CARL16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_SFg2$player1
player2vector <- CARL16_SFg2$player2
CARL16_SFg3 <- CARL16_SFg2
CARL16_SFg3$p1inp2vec <- is.element(CARL16_SFg3$player1, player2vector)
CARL16_SFg3$p2inp1vec <- is.element(CARL16_SFg3$player2, player1vector)

addPlayer1 <- CARL16_SFg3[ which(CARL16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_SFg3[ which(CARL16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_SFg2 <- rbind(CARL16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
CARL16_SFft <- ftable(CARL16_SFg2$player1, CARL16_SFg2$player2)
CARL16_SFft2 <- as.matrix(CARL16_SFft)
numRows <- nrow(CARL16_SFft2)
numCols <- ncol(CARL16_SFft2)
CARL16_SFft3 <- CARL16_SFft2[c(2:numRows) , c(2:numCols)]
CARL16_SFTable <- graph.adjacency(CARL16_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(CARL16_SFTable, vertex.label = V(CARL16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
CARL16_SF.clusterCoef <- transitivity(CARL16_SFTable, type="global") #cluster coefficient
CARL16_SF.degreeCent <- centralization.degree(CARL16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_SFftn <- as.network.matrix(CARL16_SFft)
CARL16_SF.netDensity <- network.density(CARL16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_SF.entropy <- entropy(CARL16_SFft) #entropy

CARL16_SF.netMx <- cbind(CARL16_SF.netMx, CARL16_SF.clusterCoef, CARL16_SF.degreeCent$centralization,
                         CARL16_SF.netDensity, CARL16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
CARL16_TFg2 <- data.frame(CARL16_TF)
CARL16_TFg2 <- CARL16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_TFg2$player1
player2vector <- CARL16_TFg2$player2
CARL16_TFg3 <- CARL16_TFg2
CARL16_TFg3$p1inp2vec <- is.element(CARL16_TFg3$player1, player2vector)
CARL16_TFg3$p2inp1vec <- is.element(CARL16_TFg3$player2, player1vector)

addPlayer1 <- CARL16_TFg3[ which(CARL16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_TFg3[ which(CARL16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty,empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_TFg2 <- rbind(CARL16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
CARL16_TFft <- ftable(CARL16_TFg2$player1, CARL16_TFg2$player2)
CARL16_TFft2 <- as.matrix(CARL16_TFft)
numRows <- nrow(CARL16_TFft2)
numCols <- ncol(CARL16_TFft2)
CARL16_TFft3 <- CARL16_TFft2[c(2:numRows) , c(2:numCols)]
CARL16_TFTable <- graph.adjacency(CARL16_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(CARL16_TFTable, vertex.label = V(CARL16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
CARL16_TF.clusterCoef <- transitivity(CARL16_TFTable, type="global") #cluster coefficient
CARL16_TF.degreeCent <- centralization.degree(CARL16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_TFftn <- as.network.matrix(CARL16_TFft)
CARL16_TF.netDensity <- network.density(CARL16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_TF.entropy <- entropy(CARL16_TFft) #entropy

CARL16_TF.netMx <- cbind(CARL16_TF.netMx, CARL16_TF.clusterCoef, CARL16_TF.degreeCent$centralization,
                         CARL16_TF.netDensity, CARL16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
CARL16_SAMg2 <- data.frame(CARL16_SAM)
CARL16_SAMg2 <- CARL16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_SAMg2$player1
player2vector <- CARL16_SAMg2$player2
CARL16_SAMg3 <- CARL16_SAMg2
CARL16_SAMg3$p1inp2vec <- is.element(CARL16_SAMg3$player1, player2vector)
CARL16_SAMg3$p2inp1vec <- is.element(CARL16_SAMg3$player2, player1vector)

addPlayer1 <- CARL16_SAMg3[ which(CARL16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_SAMg3[ which(CARL16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_SAMg2 <- rbind(CARL16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
CARL16_SAMft <- ftable(CARL16_SAMg2$player1, CARL16_SAMg2$player2)
CARL16_SAMft2 <- as.matrix(CARL16_SAMft)
numRows <- nrow(CARL16_SAMft2)
numCols <- ncol(CARL16_SAMft2)
CARL16_SAMft3 <- CARL16_SAMft2[c(2:numRows) , c(2:numCols)]
CARL16_SAMTable <- graph.adjacency(CARL16_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(CARL16_SAMTable, vertex.label = V(CARL16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
CARL16_SAM.clusterCoef <- transitivity(CARL16_SAMTable, type="global") #cluster coefficient
CARL16_SAM.degreeCent <- centralization.degree(CARL16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_SAMftn <- as.network.matrix(CARL16_SAMft)
CARL16_SAM.netDensity <- network.density(CARL16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_SAM.entropy <- entropy(CARL16_SAMft) #entropy

CARL16_SAM.netMx <- cbind(CARL16_SAM.netMx, CARL16_SAM.clusterCoef, CARL16_SAM.degreeCent$centralization,
                          CARL16_SAM.netDensity, CARL16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
CARL16_TAMg2 <- data.frame(CARL16_TAM)
CARL16_TAMg2 <- CARL16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_TAMg2$player1
player2vector <- CARL16_TAMg2$player2
CARL16_TAMg3 <- CARL16_TAMg2
CARL16_TAMg3$p1inp2vec <- is.element(CARL16_TAMg3$player1, player2vector)
CARL16_TAMg3$p2inp1vec <- is.element(CARL16_TAMg3$player2, player1vector)

addPlayer1 <- CARL16_TAMg3[ which(CARL16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_TAMg3[ which(CARL16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_TAMg2 <- rbind(CARL16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
CARL16_TAMft <- ftable(CARL16_TAMg2$player1, CARL16_TAMg2$player2)
CARL16_TAMft2 <- as.matrix(CARL16_TAMft)
numRows <- nrow(CARL16_TAMft2)
numCols <- ncol(CARL16_TAMft2)
CARL16_TAMft3 <- CARL16_TAMft2[c(2:numRows) , c(2:numCols)]
CARL16_TAMTable <- graph.adjacency(CARL16_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(CARL16_TAMTable, vertex.label = V(CARL16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
CARL16_TAM.clusterCoef <- transitivity(CARL16_TAMTable, type="global") #cluster coefficient
CARL16_TAM.degreeCent <- centralization.degree(CARL16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_TAMftn <- as.network.matrix(CARL16_TAMft)
CARL16_TAM.netDensity <- network.density(CARL16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_TAM.entropy <- entropy(CARL16_TAMft) #entropy

CARL16_TAM.netMx <- cbind(CARL16_TAM.netMx, CARL16_TAM.clusterCoef, CARL16_TAM.degreeCent$centralization,
                          CARL16_TAM.netDensity, CARL16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
CARL16_SDMg2 <- data.frame(CARL16_SDM)
CARL16_SDMg2 <- CARL16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_SDMg2$player1
player2vector <- CARL16_SDMg2$player2
CARL16_SDMg3 <- CARL16_SDMg2
CARL16_SDMg3$p1inp2vec <- is.element(CARL16_SDMg3$player1, player2vector)
CARL16_SDMg3$p2inp1vec <- is.element(CARL16_SDMg3$player2, player1vector)

addPlayer1 <- CARL16_SDMg3[ which(CARL16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_SDMg3[ which(CARL16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_SDMg2 <- rbind(CARL16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
CARL16_SDMft <- ftable(CARL16_SDMg2$player1, CARL16_SDMg2$player2)
CARL16_SDMft2 <- as.matrix(CARL16_SDMft)
numRows <- nrow(CARL16_SDMft2)
numCols <- ncol(CARL16_SDMft2)
CARL16_SDMft3 <- CARL16_SDMft2[c(2:numRows) , c(2:numCols)]
CARL16_SDMTable <- graph.adjacency(CARL16_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(CARL16_SDMTable, vertex.label = V(CARL16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
CARL16_SDM.clusterCoef <- transitivity(CARL16_SDMTable, type="global") #cluster coefficient
CARL16_SDM.degreeCent <- centralization.degree(CARL16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_SDMftn <- as.network.matrix(CARL16_SDMft)
CARL16_SDM.netDensity <- network.density(CARL16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_SDM.entropy <- entropy(CARL16_SDMft) #entropy

CARL16_SDM.netMx <- cbind(CARL16_SDM.netMx, CARL16_SDM.clusterCoef, CARL16_SDM.degreeCent$centralization,
                          CARL16_SDM.netDensity, CARL16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
CARL16_TDMg2 <- data.frame(CARL16_TDM)
CARL16_TDMg2 <- CARL16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_TDMg2$player1
player2vector <- CARL16_TDMg2$player2
CARL16_TDMg3 <- CARL16_TDMg2
CARL16_TDMg3$p1inp2vec <- is.element(CARL16_TDMg3$player1, player2vector)
CARL16_TDMg3$p2inp1vec <- is.element(CARL16_TDMg3$player2, player1vector)

addPlayer1 <- CARL16_TDMg3[ which(CARL16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_TDMg3[ which(CARL16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_TDMg2 <- rbind(CARL16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
CARL16_TDMft <- ftable(CARL16_TDMg2$player1, CARL16_TDMg2$player2)
CARL16_TDMft2 <- as.matrix(CARL16_TDMft)
numRows <- nrow(CARL16_TDMft2)
numCols <- ncol(CARL16_TDMft2)
CARL16_TDMft3 <- CARL16_TDMft2[c(2:numRows) , c(2:numCols)]
CARL16_TDMTable <- graph.adjacency(CARL16_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(CARL16_TDMTable, vertex.label = V(CARL16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
CARL16_TDM.clusterCoef <- transitivity(CARL16_TDMTable, type="global") #cluster coefficient
CARL16_TDM.degreeCent <- centralization.degree(CARL16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_TDMftn <- as.network.matrix(CARL16_TDMft)
CARL16_TDM.netDensity <- network.density(CARL16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_TDM.entropy <- entropy(CARL16_TDMft) #entropy

CARL16_TDM.netMx <- cbind(CARL16_TDM.netMx, CARL16_TDM.clusterCoef, CARL16_TDM.degreeCent$centralization,
                          CARL16_TDM.netDensity, CARL16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
CARL16_SDg2 <- data.frame(CARL16_SD)
CARL16_SDg2 <- CARL16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_SDg2$player1
player2vector <- CARL16_SDg2$player2
CARL16_SDg3 <- CARL16_SDg2
CARL16_SDg3$p1inp2vec <- is.element(CARL16_SDg3$player1, player2vector)
CARL16_SDg3$p2inp1vec <- is.element(CARL16_SDg3$player2, player1vector)

addPlayer1 <- CARL16_SDg3[ which(CARL16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_SDg3[ which(CARL16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_SDg2 <- rbind(CARL16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
CARL16_SDft <- ftable(CARL16_SDg2$player1, CARL16_SDg2$player2)
CARL16_SDft2 <- as.matrix(CARL16_SDft)
numRows <- nrow(CARL16_SDft2)
numCols <- ncol(CARL16_SDft2)
CARL16_SDft3 <- CARL16_SDft2[c(2:numRows) , c(2:numCols)]
CARL16_SDTable <- graph.adjacency(CARL16_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(CARL16_SDTable, vertex.label = V(CARL16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
CARL16_SD.clusterCoef <- transitivity(CARL16_SDTable, type="global") #cluster coefficient
CARL16_SD.degreeCent <- centralization.degree(CARL16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_SDftn <- as.network.matrix(CARL16_SDft)
CARL16_SD.netDensity <- network.density(CARL16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_SD.entropy <- entropy(CARL16_SDft) #entropy

CARL16_SD.netMx <- cbind(CARL16_SD.netMx, CARL16_SD.clusterCoef, CARL16_SD.degreeCent$centralization,
                         CARL16_SD.netDensity, CARL16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
CARL16_TDg2 <- data.frame(CARL16_TD)
CARL16_TDg2 <- CARL16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_TDg2$player1
player2vector <- CARL16_TDg2$player2
CARL16_TDg3 <- CARL16_TDg2
CARL16_TDg3$p1inp2vec <- is.element(CARL16_TDg3$player1, player2vector)
CARL16_TDg3$p2inp1vec <- is.element(CARL16_TDg3$player2, player1vector)

addPlayer1 <- CARL16_TDg3[ which(CARL16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_TDg3[ which(CARL16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_TDg2 <- rbind(CARL16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
CARL16_TDft <- ftable(CARL16_TDg2$player1, CARL16_TDg2$player2)
CARL16_TDft2 <- as.matrix(CARL16_TDft)
numRows <- nrow(CARL16_TDft2)
numCols <- ncol(CARL16_TDft2)
CARL16_TDft3 <- CARL16_TDft2[c(2:numRows) , c(2:numCols)]
CARL16_TDTable <- graph.adjacency(CARL16_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(CARL16_TDTable, vertex.label = V(CARL16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
CARL16_TD.clusterCoef <- transitivity(CARL16_TDTable, type="global") #cluster coefficient
CARL16_TD.degreeCent <- centralization.degree(CARL16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_TDftn <- as.network.matrix(CARL16_TDft)
CARL16_TD.netDensity <- network.density(CARL16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_TD.entropy <- entropy(CARL16_TDft) #entropy

CARL16_TD.netMx <- cbind(CARL16_TD.netMx, CARL16_TD.clusterCoef, CARL16_TD.degreeCent$centralization,
                         CARL16_TD.netDensity, CARL16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
CARL16_QTg2 <- data.frame(CARL16_QT)
CARL16_QTg2 <- CARL16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL16_QTg2$player1
player2vector <- CARL16_QTg2$player2
CARL16_QTg3 <- CARL16_QTg2
CARL16_QTg3$p1inp2vec <- is.element(CARL16_QTg3$player1, player2vector)
CARL16_QTg3$p2inp1vec <- is.element(CARL16_QTg3$player2, player1vector)

addPlayer1 <- CARL16_QTg3[ which(CARL16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL16_QTg3[ which(CARL16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL16_QTg2 <- rbind(CARL16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
CARL16_QTft <- ftable(CARL16_QTg2$player1, CARL16_QTg2$player2)
CARL16_QTft2 <- as.matrix(CARL16_QTft)
numRows <- nrow(CARL16_QTft2)
numCols <- ncol(CARL16_QTft2)
CARL16_QTft3 <- CARL16_QTft2[c(2:numRows) , c(2:numCols)]
CARL16_QTTable <- graph.adjacency(CARL16_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(CARL16_QTTable, vertex.label = V(CARL16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
CARL16_QT.clusterCoef <- transitivity(CARL16_QTTable, type="global") #cluster coefficient
CARL16_QT.degreeCent <- centralization.degree(CARL16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL16_QTftn <- as.network.matrix(CARL16_QTft)
CARL16_QT.netDensity <- network.density(CARL16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL16_QT.entropy <- entropy(CARL16_QTft) #entropy

CARL16_QT.netMx <- cbind(CARL16_QT.netMx, CARL16_QT.clusterCoef, CARL16_QT.degreeCent$centralization,
                         CARL16_QT.netDensity, CARL16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL16_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "COLL"
KIoutcome = "Goal_F"
COLL16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
COLL16_Gg2 <- data.frame(COLL16_G)
COLL16_Gg2 <- COLL16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_Gg2$player1
player2vector <- COLL16_Gg2$player2
COLL16_Gg3 <- COLL16_Gg2
COLL16_Gg3$p1inp2vec <- is.element(COLL16_Gg3$player1, player2vector)
COLL16_Gg3$p2inp1vec <- is.element(COLL16_Gg3$player2, player1vector)

addPlayer1 <- COLL16_Gg3[ which(COLL16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_Gg3[ which(COLL16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_Gg2 <- rbind(COLL16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
COLL16_Gft <- ftable(COLL16_Gg2$player1, COLL16_Gg2$player2)
COLL16_Gft2 <- as.matrix(COLL16_Gft)
numRows <- nrow(COLL16_Gft2)
numCols <- ncol(COLL16_Gft2)
COLL16_Gft3 <- COLL16_Gft2[c(2:numRows) , c(2:numCols)]
COLL16_GTable <- graph.adjacency(COLL16_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(COLL16_GTable, vertex.label = V(COLL16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
COLL16_G.clusterCoef <- transitivity(COLL16_GTable, type="global") #cluster coefficient
COLL16_G.degreeCent <- centralization.degree(COLL16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_Gftn <- as.network.matrix(COLL16_Gft)
COLL16_G.netDensity <- network.density(COLL16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_G.entropy <- entropy(COLL16_Gft) #entropy

COLL16_G.netMx <- cbind(COLL16_G.netMx, COLL16_G.clusterCoef, COLL16_G.degreeCent$centralization,
                        COLL16_G.netDensity, COLL16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "COLL"
KIoutcome = "Behind_F"
COLL16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
COLL16_Bg2 <- data.frame(COLL16_B)
COLL16_Bg2 <- COLL16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_Bg2$player1
player2vector <- COLL16_Bg2$player2
COLL16_Bg3 <- COLL16_Bg2
COLL16_Bg3$p1inp2vec <- is.element(COLL16_Bg3$player1, player2vector)
COLL16_Bg3$p2inp1vec <- is.element(COLL16_Bg3$player2, player1vector)

addPlayer1 <- COLL16_Bg3[ which(COLL16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_Bg3[ which(COLL16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_Bg2 <- rbind(COLL16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
COLL16_Bft <- ftable(COLL16_Bg2$player1, COLL16_Bg2$player2)
COLL16_Bft2 <- as.matrix(COLL16_Bft)
numRows <- nrow(COLL16_Bft2)
numCols <- ncol(COLL16_Bft2)
COLL16_Bft3 <- COLL16_Bft2[c(2:numRows) , c(2:numCols)]
COLL16_BTable <- graph.adjacency(COLL16_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(COLL16_BTable, vertex.label = V(COLL16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
COLL16_B.clusterCoef <- transitivity(COLL16_BTable, type="global") #cluster coefficient
COLL16_B.degreeCent <- centralization.degree(COLL16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_Bftn <- as.network.matrix(COLL16_Bft)
COLL16_B.netDensity <- network.density(COLL16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_B.entropy <- entropy(COLL16_Bft) #entropy

COLL16_B.netMx <- cbind(COLL16_B.netMx, COLL16_B.clusterCoef, COLL16_B.degreeCent$centralization,
                        COLL16_B.netDensity, COLL16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************

round = 16
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
COLL16_SFg2 <- data.frame(COLL16_SF)
COLL16_SFg2 <- COLL16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_SFg2$player1
player2vector <- COLL16_SFg2$player2
COLL16_SFg3 <- COLL16_SFg2
COLL16_SFg3$p1inp2vec <- is.element(COLL16_SFg3$player1, player2vector)
COLL16_SFg3$p2inp1vec <- is.element(COLL16_SFg3$player2, player1vector)

addPlayer1 <- COLL16_SFg3[ which(COLL16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_SFg3[ which(COLL16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_SFg2 <- rbind(COLL16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
COLL16_SFft <- ftable(COLL16_SFg2$player1, COLL16_SFg2$player2)
COLL16_SFft2 <- as.matrix(COLL16_SFft)
numRows <- nrow(COLL16_SFft2)
numCols <- ncol(COLL16_SFft2)
COLL16_SFft3 <- COLL16_SFft2[c(2:numRows) , c(2:numCols)]
COLL16_SFTable <- graph.adjacency(COLL16_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(COLL16_SFTable, vertex.label = V(COLL16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
COLL16_SF.clusterCoef <- transitivity(COLL16_SFTable, type="global") #cluster coefficient
COLL16_SF.degreeCent <- centralization.degree(COLL16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_SFftn <- as.network.matrix(COLL16_SFft)
COLL16_SF.netDensity <- network.density(COLL16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_SF.entropy <- entropy(COLL16_SFft) #entropy

COLL16_SF.netMx <- cbind(COLL16_SF.netMx, COLL16_SF.clusterCoef, COLL16_SF.degreeCent$centralization,
                         COLL16_SF.netDensity, COLL16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
COLL16_TFg2 <- data.frame(COLL16_TF)
COLL16_TFg2 <- COLL16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_TFg2$player1
player2vector <- COLL16_TFg2$player2
COLL16_TFg3 <- COLL16_TFg2
COLL16_TFg3$p1inp2vec <- is.element(COLL16_TFg3$player1, player2vector)
COLL16_TFg3$p2inp1vec <- is.element(COLL16_TFg3$player2, player1vector)

addPlayer1 <- COLL16_TFg3[ which(COLL16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_TFg3[ which(COLL16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_TFg2 <- rbind(COLL16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
COLL16_TFft <- ftable(COLL16_TFg2$player1, COLL16_TFg2$player2)
COLL16_TFft2 <- as.matrix(COLL16_TFft)
numRows <- nrow(COLL16_TFft2)
numCols <- ncol(COLL16_TFft2)
COLL16_TFft3 <- COLL16_TFft2[c(2:numRows) , c(2:numCols)]
COLL16_TFTable <- graph.adjacency(COLL16_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(COLL16_TFTable, vertex.label = V(COLL16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
COLL16_TF.clusterCoef <- transitivity(COLL16_TFTable, type="global") #cluster coefficient
COLL16_TF.degreeCent <- centralization.degree(COLL16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_TFftn <- as.network.matrix(COLL16_TFft)
COLL16_TF.netDensity <- network.density(COLL16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_TF.entropy <- entropy(COLL16_TFft) #entropy

COLL16_TF.netMx <- cbind(COLL16_TF.netMx, COLL16_TF.clusterCoef, COLL16_TF.degreeCent$centralization,
                         COLL16_TF.netDensity, COLL16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
COLL16_SAMg2 <- data.frame(COLL16_SAM)
COLL16_SAMg2 <- COLL16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_SAMg2$player1
player2vector <- COLL16_SAMg2$player2
COLL16_SAMg3 <- COLL16_SAMg2
COLL16_SAMg3$p1inp2vec <- is.element(COLL16_SAMg3$player1, player2vector)
COLL16_SAMg3$p2inp1vec <- is.element(COLL16_SAMg3$player2, player1vector)

addPlayer1 <- COLL16_SAMg3[ which(COLL16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_SAMg3[ which(COLL16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_SAMg2 <- rbind(COLL16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
COLL16_SAMft <- ftable(COLL16_SAMg2$player1, COLL16_SAMg2$player2)
COLL16_SAMft2 <- as.matrix(COLL16_SAMft)
numRows <- nrow(COLL16_SAMft2)
numCols <- ncol(COLL16_SAMft2)
COLL16_SAMft3 <- COLL16_SAMft2[c(2:numRows) , c(2:numCols)]
COLL16_SAMTable <- graph.adjacency(COLL16_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(COLL16_SAMTable, vertex.label = V(COLL16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
COLL16_SAM.clusterCoef <- transitivity(COLL16_SAMTable, type="global") #cluster coefficient
COLL16_SAM.degreeCent <- centralization.degree(COLL16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_SAMftn <- as.network.matrix(COLL16_SAMft)
COLL16_SAM.netDensity <- network.density(COLL16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_SAM.entropy <- entropy(COLL16_SAMft) #entropy

COLL16_SAM.netMx <- cbind(COLL16_SAM.netMx, COLL16_SAM.clusterCoef, COLL16_SAM.degreeCent$centralization,
                          COLL16_SAM.netDensity, COLL16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************
#NA

round = 16
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
COLL16_TAMg2 <- data.frame(COLL16_TAM)
COLL16_TAMg2 <- COLL16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_TAMg2$player1
player2vector <- COLL16_TAMg2$player2
COLL16_TAMg3 <- COLL16_TAMg2
COLL16_TAMg3$p1inp2vec <- is.element(COLL16_TAMg3$player1, player2vector)
COLL16_TAMg3$p2inp1vec <- is.element(COLL16_TAMg3$player2, player1vector)

addPlayer1 <- COLL16_TAMg3[ which(COLL16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_TAMg3[ which(COLL16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_TAMg2 <- rbind(COLL16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
COLL16_TAMft <- ftable(COLL16_TAMg2$player1, COLL16_TAMg2$player2)
COLL16_TAMft2 <- as.matrix(COLL16_TAMft)
numRows <- nrow(COLL16_TAMft2)
numCols <- ncol(COLL16_TAMft2)
COLL16_TAMft3 <- COLL16_TAMft2[c(2:numRows) , c(2:numCols)]
COLL16_TAMTable <- graph.adjacency(COLL16_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(COLL16_TAMTable, vertex.label = V(COLL16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
COLL16_TAM.clusterCoef <- transitivity(COLL16_TAMTable, type="global") #cluster coefficient
COLL16_TAM.degreeCent <- centralization.degree(COLL16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_TAMftn <- as.network.matrix(COLL16_TAMft)
COLL16_TAM.netDensity <- network.density(COLL16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_TAM.entropy <- entropy(COLL16_TAMft) #entropy

COLL16_TAM.netMx <- cbind(COLL16_TAM.netMx, COLL16_TAM.clusterCoef, COLL16_TAM.degreeCent$centralization,
                          COLL16_TAM.netDensity, COLL16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
COLL16_SDMg2 <- data.frame(COLL16_SDM)
COLL16_SDMg2 <- COLL16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_SDMg2$player1
player2vector <- COLL16_SDMg2$player2
COLL16_SDMg3 <- COLL16_SDMg2
COLL16_SDMg3$p1inp2vec <- is.element(COLL16_SDMg3$player1, player2vector)
COLL16_SDMg3$p2inp1vec <- is.element(COLL16_SDMg3$player2, player1vector)

addPlayer1 <- COLL16_SDMg3[ which(COLL16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_SDMg3[ which(COLL16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_SDMg2 <- rbind(COLL16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
COLL16_SDMft <- ftable(COLL16_SDMg2$player1, COLL16_SDMg2$player2)
COLL16_SDMft2 <- as.matrix(COLL16_SDMft)
numRows <- nrow(COLL16_SDMft2)
numCols <- ncol(COLL16_SDMft2)
COLL16_SDMft3 <- COLL16_SDMft2[c(2:numRows) , c(2:numCols)]
COLL16_SDMTable <- graph.adjacency(COLL16_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(COLL16_SDMTable, vertex.label = V(COLL16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
COLL16_SDM.clusterCoef <- transitivity(COLL16_SDMTable, type="global") #cluster coefficient
COLL16_SDM.degreeCent <- centralization.degree(COLL16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_SDMftn <- as.network.matrix(COLL16_SDMft)
COLL16_SDM.netDensity <- network.density(COLL16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_SDM.entropy <- entropy(COLL16_SDMft) #entropy

COLL16_SDM.netMx <- cbind(COLL16_SDM.netMx, COLL16_SDM.clusterCoef, COLL16_SDM.degreeCent$centralization,
                          COLL16_SDM.netDensity, COLL16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
COLL16_TDMg2 <- data.frame(COLL16_TDM)
COLL16_TDMg2 <- COLL16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_TDMg2$player1
player2vector <- COLL16_TDMg2$player2
COLL16_TDMg3 <- COLL16_TDMg2
COLL16_TDMg3$p1inp2vec <- is.element(COLL16_TDMg3$player1, player2vector)
COLL16_TDMg3$p2inp1vec <- is.element(COLL16_TDMg3$player2, player1vector)

addPlayer1 <- COLL16_TDMg3[ which(COLL16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_TDMg3[ which(COLL16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_TDMg2 <- rbind(COLL16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
COLL16_TDMft <- ftable(COLL16_TDMg2$player1, COLL16_TDMg2$player2)
COLL16_TDMft2 <- as.matrix(COLL16_TDMft)
numRows <- nrow(COLL16_TDMft2)
numCols <- ncol(COLL16_TDMft2)
COLL16_TDMft3 <- COLL16_TDMft2[c(2:numRows) , c(2:numCols)]
COLL16_TDMTable <- graph.adjacency(COLL16_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(COLL16_TDMTable, vertex.label = V(COLL16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
COLL16_TDM.clusterCoef <- transitivity(COLL16_TDMTable, type="global") #cluster coefficient
COLL16_TDM.degreeCent <- centralization.degree(COLL16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_TDMftn <- as.network.matrix(COLL16_TDMft)
COLL16_TDM.netDensity <- network.density(COLL16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_TDM.entropy <- entropy(COLL16_TDMft) #entropy

COLL16_TDM.netMx <- cbind(COLL16_TDM.netMx, COLL16_TDM.clusterCoef, COLL16_TDM.degreeCent$centralization,
                          COLL16_TDM.netDensity, COLL16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
COLL16_SDg2 <- data.frame(COLL16_SD)
COLL16_SDg2 <- COLL16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_SDg2$player1
player2vector <- COLL16_SDg2$player2
COLL16_SDg3 <- COLL16_SDg2
COLL16_SDg3$p1inp2vec <- is.element(COLL16_SDg3$player1, player2vector)
COLL16_SDg3$p2inp1vec <- is.element(COLL16_SDg3$player2, player1vector)

addPlayer1 <- COLL16_SDg3[ which(COLL16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_SDg3[ which(COLL16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_SDg2 <- rbind(COLL16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
COLL16_SDft <- ftable(COLL16_SDg2$player1, COLL16_SDg2$player2)
COLL16_SDft2 <- as.matrix(COLL16_SDft)
numRows <- nrow(COLL16_SDft2)
numCols <- ncol(COLL16_SDft2)
COLL16_SDft3 <- COLL16_SDft2[c(2:numRows) , c(2:numCols)]
COLL16_SDTable <- graph.adjacency(COLL16_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(COLL16_SDTable, vertex.label = V(COLL16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
COLL16_SD.clusterCoef <- transitivity(COLL16_SDTable, type="global") #cluster coefficient
COLL16_SD.degreeCent <- centralization.degree(COLL16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_SDftn <- as.network.matrix(COLL16_SDft)
COLL16_SD.netDensity <- network.density(COLL16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_SD.entropy <- entropy(COLL16_SDft) #entropy

COLL16_SD.netMx <- cbind(COLL16_SD.netMx, COLL16_SD.clusterCoef, COLL16_SD.degreeCent$centralization,
                         COLL16_SD.netDensity, COLL16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
COLL16_TDg2 <- data.frame(COLL16_TD)
COLL16_TDg2 <- COLL16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_TDg2$player1
player2vector <- COLL16_TDg2$player2
COLL16_TDg3 <- COLL16_TDg2
COLL16_TDg3$p1inp2vec <- is.element(COLL16_TDg3$player1, player2vector)
COLL16_TDg3$p2inp1vec <- is.element(COLL16_TDg3$player2, player1vector)

addPlayer1 <- COLL16_TDg3[ which(COLL16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_TDg3[ which(COLL16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_TDg2 <- rbind(COLL16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
COLL16_TDft <- ftable(COLL16_TDg2$player1, COLL16_TDg2$player2)
COLL16_TDft2 <- as.matrix(COLL16_TDft)
numRows <- nrow(COLL16_TDft2)
numCols <- ncol(COLL16_TDft2)
COLL16_TDft3 <- COLL16_TDft2[c(2:numRows) , c(2:numCols)]
COLL16_TDTable <- graph.adjacency(COLL16_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(COLL16_TDTable, vertex.label = V(COLL16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
COLL16_TD.clusterCoef <- transitivity(COLL16_TDTable, type="global") #cluster coefficient
COLL16_TD.degreeCent <- centralization.degree(COLL16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_TDftn <- as.network.matrix(COLL16_TDft)
COLL16_TD.netDensity <- network.density(COLL16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_TD.entropy <- entropy(COLL16_TDft) #entropy

COLL16_TD.netMx <- cbind(COLL16_TD.netMx, COLL16_TD.clusterCoef, COLL16_TD.degreeCent$centralization,
                         COLL16_TD.netDensity, COLL16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
COLL16_QTg2 <- data.frame(COLL16_QT)
COLL16_QTg2 <- COLL16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL16_QTg2$player1
player2vector <- COLL16_QTg2$player2
COLL16_QTg3 <- COLL16_QTg2
COLL16_QTg3$p1inp2vec <- is.element(COLL16_QTg3$player1, player2vector)
COLL16_QTg3$p2inp1vec <- is.element(COLL16_QTg3$player2, player1vector)

addPlayer1 <- COLL16_QTg3[ which(COLL16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL16_QTg3[ which(COLL16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL16_QTg2 <- rbind(COLL16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
COLL16_QTft <- ftable(COLL16_QTg2$player1, COLL16_QTg2$player2)
COLL16_QTft2 <- as.matrix(COLL16_QTft)
numRows <- nrow(COLL16_QTft2)
numCols <- ncol(COLL16_QTft2)
COLL16_QTft3 <- COLL16_QTft2[c(2:numRows) , c(2:numCols)]
COLL16_QTTable <- graph.adjacency(COLL16_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(COLL16_QTTable, vertex.label = V(COLL16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
COLL16_QT.clusterCoef <- transitivity(COLL16_QTTable, type="global") #cluster coefficient
COLL16_QT.degreeCent <- centralization.degree(COLL16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL16_QTftn <- as.network.matrix(COLL16_QTft)
COLL16_QT.netDensity <- network.density(COLL16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL16_QT.entropy <- entropy(COLL16_QTft) #entropy

COLL16_QT.netMx <- cbind(COLL16_QT.netMx, COLL16_QT.clusterCoef, COLL16_QT.degreeCent$centralization,
                         COLL16_QT.netDensity, COLL16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL16_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "ESS"
KIoutcome = "Goal_F"
ESS16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
ESS16_Gg2 <- data.frame(ESS16_G)
ESS16_Gg2 <- ESS16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_Gg2$player1
player2vector <- ESS16_Gg2$player2
ESS16_Gg3 <- ESS16_Gg2
ESS16_Gg3$p1inp2vec <- is.element(ESS16_Gg3$player1, player2vector)
ESS16_Gg3$p2inp1vec <- is.element(ESS16_Gg3$player2, player1vector)

addPlayer1 <- ESS16_Gg3[ which(ESS16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_Gg3[ which(ESS16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_Gg2 <- rbind(ESS16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
ESS16_Gft <- ftable(ESS16_Gg2$player1, ESS16_Gg2$player2)
ESS16_Gft2 <- as.matrix(ESS16_Gft)
numRows <- nrow(ESS16_Gft2)
numCols <- ncol(ESS16_Gft2)
ESS16_Gft3 <- ESS16_Gft2[c(2:numRows) , c(2:numCols)]
ESS16_GTable <- graph.adjacency(ESS16_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(ESS16_GTable, vertex.label = V(ESS16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
ESS16_G.clusterCoef <- transitivity(ESS16_GTable, type="global") #cluster coefficient
ESS16_G.degreeCent <- centralization.degree(ESS16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_Gftn <- as.network.matrix(ESS16_Gft)
ESS16_G.netDensity <- network.density(ESS16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_G.entropy <- entropy(ESS16_Gft) #entropy

ESS16_G.netMx <- cbind(ESS16_G.netMx, ESS16_G.clusterCoef, ESS16_G.degreeCent$centralization,
                       ESS16_G.netDensity, ESS16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "ESS"
KIoutcome = "Behind_F"
ESS16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
ESS16_Bg2 <- data.frame(ESS16_B)
ESS16_Bg2 <- ESS16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_Bg2$player1
player2vector <- ESS16_Bg2$player2
ESS16_Bg3 <- ESS16_Bg2
ESS16_Bg3$p1inp2vec <- is.element(ESS16_Bg3$player1, player2vector)
ESS16_Bg3$p2inp1vec <- is.element(ESS16_Bg3$player2, player1vector)

addPlayer1 <- ESS16_Bg3[ which(ESS16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_Bg3[ which(ESS16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_Bg2 <- rbind(ESS16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
ESS16_Bft <- ftable(ESS16_Bg2$player1, ESS16_Bg2$player2)
ESS16_Bft2 <- as.matrix(ESS16_Bft)
numRows <- nrow(ESS16_Bft2)
numCols <- ncol(ESS16_Bft2)
ESS16_Bft3 <- ESS16_Bft2[c(2:numRows) , c(2:numCols)]
ESS16_BTable <- graph.adjacency(ESS16_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(ESS16_BTable, vertex.label = V(ESS16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
ESS16_B.clusterCoef <- transitivity(ESS16_BTable, type="global") #cluster coefficient
ESS16_B.degreeCent <- centralization.degree(ESS16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_Bftn <- as.network.matrix(ESS16_Bft)
ESS16_B.netDensity <- network.density(ESS16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_B.entropy <- entropy(ESS16_Bft) #entropy

ESS16_B.netMx <- cbind(ESS16_B.netMx, ESS16_B.clusterCoef, ESS16_B.degreeCent$centralization,
                       ESS16_B.netDensity, ESS16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************

round = 16
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
ESS16_SFg2 <- data.frame(ESS16_SF)
ESS16_SFg2 <- ESS16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_SFg2$player1
player2vector <- ESS16_SFg2$player2
ESS16_SFg3 <- ESS16_SFg2
ESS16_SFg3$p1inp2vec <- is.element(ESS16_SFg3$player1, player2vector)
ESS16_SFg3$p2inp1vec <- is.element(ESS16_SFg3$player2, player1vector)

addPlayer1 <- ESS16_SFg3[ which(ESS16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_SFg3[ which(ESS16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_SFg2 <- rbind(ESS16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
ESS16_SFft <- ftable(ESS16_SFg2$player1, ESS16_SFg2$player2)
ESS16_SFft2 <- as.matrix(ESS16_SFft)
numRows <- nrow(ESS16_SFft2)
numCols <- ncol(ESS16_SFft2)
ESS16_SFft3 <- ESS16_SFft2[c(2:numRows) , c(2:numCols)]
ESS16_SFTable <- graph.adjacency(ESS16_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(ESS16_SFTable, vertex.label = V(ESS16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
ESS16_SF.clusterCoef <- transitivity(ESS16_SFTable, type="global") #cluster coefficient
ESS16_SF.degreeCent <- centralization.degree(ESS16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_SFftn <- as.network.matrix(ESS16_SFft)
ESS16_SF.netDensity <- network.density(ESS16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_SF.entropy <- entropy(ESS16_SFft) #entropy

ESS16_SF.netMx <- cbind(ESS16_SF.netMx, ESS16_SF.clusterCoef, ESS16_SF.degreeCent$centralization,
                        ESS16_SF.netDensity, ESS16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
ESS16_TFg2 <- data.frame(ESS16_TF)
ESS16_TFg2 <- ESS16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_TFg2$player1
player2vector <- ESS16_TFg2$player2
ESS16_TFg3 <- ESS16_TFg2
ESS16_TFg3$p1inp2vec <- is.element(ESS16_TFg3$player1, player2vector)
ESS16_TFg3$p2inp1vec <- is.element(ESS16_TFg3$player2, player1vector)

addPlayer1 <- ESS16_TFg3[ which(ESS16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_TFg3[ which(ESS16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_TFg2 <- rbind(ESS16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
ESS16_TFft <- ftable(ESS16_TFg2$player1, ESS16_TFg2$player2)
ESS16_TFft2 <- as.matrix(ESS16_TFft)
numRows <- nrow(ESS16_TFft2)
numCols <- ncol(ESS16_TFft2)
ESS16_TFft3 <- ESS16_TFft2[c(2:numRows) , c(2:numCols)]
ESS16_TFTable <- graph.adjacency(ESS16_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(ESS16_TFTable, vertex.label = V(ESS16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
ESS16_TF.clusterCoef <- transitivity(ESS16_TFTable, type="global") #cluster coefficient
ESS16_TF.degreeCent <- centralization.degree(ESS16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_TFftn <- as.network.matrix(ESS16_TFft)
ESS16_TF.netDensity <- network.density(ESS16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_TF.entropy <- entropy(ESS16_TFft) #entropy

ESS16_TF.netMx <- cbind(ESS16_TF.netMx, ESS16_TF.clusterCoef, ESS16_TF.degreeCent$centralization,
                        ESS16_TF.netDensity, ESS16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
ESS16_SAMg2 <- data.frame(ESS16_SAM)
ESS16_SAMg2 <- ESS16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_SAMg2$player1
player2vector <- ESS16_SAMg2$player2
ESS16_SAMg3 <- ESS16_SAMg2
ESS16_SAMg3$p1inp2vec <- is.element(ESS16_SAMg3$player1, player2vector)
ESS16_SAMg3$p2inp1vec <- is.element(ESS16_SAMg3$player2, player1vector)

addPlayer1 <- ESS16_SAMg3[ which(ESS16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_SAMg3[ which(ESS16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_SAMg2 <- rbind(ESS16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
ESS16_SAMft <- ftable(ESS16_SAMg2$player1, ESS16_SAMg2$player2)
ESS16_SAMft2 <- as.matrix(ESS16_SAMft)
numRows <- nrow(ESS16_SAMft2)
numCols <- ncol(ESS16_SAMft2)
ESS16_SAMft3 <- ESS16_SAMft2[c(2:numRows) , c(2:numCols)]
ESS16_SAMTable <- graph.adjacency(ESS16_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(ESS16_SAMTable, vertex.label = V(ESS16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
ESS16_SAM.clusterCoef <- transitivity(ESS16_SAMTable, type="global") #cluster coefficient
ESS16_SAM.degreeCent <- centralization.degree(ESS16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_SAMftn <- as.network.matrix(ESS16_SAMft)
ESS16_SAM.netDensity <- network.density(ESS16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_SAM.entropy <- entropy(ESS16_SAMft) #entropy

ESS16_SAM.netMx <- cbind(ESS16_SAM.netMx, ESS16_SAM.clusterCoef, ESS16_SAM.degreeCent$centralization,
                         ESS16_SAM.netDensity, ESS16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
ESS16_TAMg2 <- data.frame(ESS16_TAM)
ESS16_TAMg2 <- ESS16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_TAMg2$player1
player2vector <- ESS16_TAMg2$player2
ESS16_TAMg3 <- ESS16_TAMg2
ESS16_TAMg3$p1inp2vec <- is.element(ESS16_TAMg3$player1, player2vector)
ESS16_TAMg3$p2inp1vec <- is.element(ESS16_TAMg3$player2, player1vector)

addPlayer1 <- ESS16_TAMg3[ which(ESS16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_TAMg3[ which(ESS16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_TAMg2 <- rbind(ESS16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
ESS16_TAMft <- ftable(ESS16_TAMg2$player1, ESS16_TAMg2$player2)
ESS16_TAMft2 <- as.matrix(ESS16_TAMft)
numRows <- nrow(ESS16_TAMft2)
numCols <- ncol(ESS16_TAMft2)
ESS16_TAMft3 <- ESS16_TAMft2[c(2:numRows) , c(2:numCols)]
ESS16_TAMTable <- graph.adjacency(ESS16_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(ESS16_TAMTable, vertex.label = V(ESS16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
ESS16_TAM.clusterCoef <- transitivity(ESS16_TAMTable, type="global") #cluster coefficient
ESS16_TAM.degreeCent <- centralization.degree(ESS16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_TAMftn <- as.network.matrix(ESS16_TAMft)
ESS16_TAM.netDensity <- network.density(ESS16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_TAM.entropy <- entropy(ESS16_TAMft) #entropy

ESS16_TAM.netMx <- cbind(ESS16_TAM.netMx, ESS16_TAM.clusterCoef, ESS16_TAM.degreeCent$centralization,
                         ESS16_TAM.netDensity, ESS16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************
#NA

round = 16
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
ESS16_SDMg2 <- data.frame(ESS16_SDM)
ESS16_SDMg2 <- ESS16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_SDMg2$player1
player2vector <- ESS16_SDMg2$player2
ESS16_SDMg3 <- ESS16_SDMg2
ESS16_SDMg3$p1inp2vec <- is.element(ESS16_SDMg3$player1, player2vector)
ESS16_SDMg3$p2inp1vec <- is.element(ESS16_SDMg3$player2, player1vector)

addPlayer1 <- ESS16_SDMg3[ which(ESS16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_SDMg3[ which(ESS16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_SDMg2 <- rbind(ESS16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
ESS16_SDMft <- ftable(ESS16_SDMg2$player1, ESS16_SDMg2$player2)
ESS16_SDMft2 <- as.matrix(ESS16_SDMft)
numRows <- nrow(ESS16_SDMft2)
numCols <- ncol(ESS16_SDMft2)
ESS16_SDMft3 <- ESS16_SDMft2[c(2:numRows) , c(2:numCols)]
ESS16_SDMTable <- graph.adjacency(ESS16_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(ESS16_SDMTable, vertex.label = V(ESS16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
ESS16_SDM.clusterCoef <- transitivity(ESS16_SDMTable, type="global") #cluster coefficient
ESS16_SDM.degreeCent <- centralization.degree(ESS16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_SDMftn <- as.network.matrix(ESS16_SDMft)
ESS16_SDM.netDensity <- network.density(ESS16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_SDM.entropy <- entropy(ESS16_SDMft) #entropy

ESS16_SDM.netMx <- cbind(ESS16_SDM.netMx, ESS16_SDM.clusterCoef, ESS16_SDM.degreeCent$centralization,
                         ESS16_SDM.netDensity, ESS16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
ESS16_TDMg2 <- data.frame(ESS16_TDM)
ESS16_TDMg2 <- ESS16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_TDMg2$player1
player2vector <- ESS16_TDMg2$player2
ESS16_TDMg3 <- ESS16_TDMg2
ESS16_TDMg3$p1inp2vec <- is.element(ESS16_TDMg3$player1, player2vector)
ESS16_TDMg3$p2inp1vec <- is.element(ESS16_TDMg3$player2, player1vector)

addPlayer1 <- ESS16_TDMg3[ which(ESS16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_TDMg3[ which(ESS16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_TDMg2 <- rbind(ESS16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
ESS16_TDMft <- ftable(ESS16_TDMg2$player1, ESS16_TDMg2$player2)
ESS16_TDMft2 <- as.matrix(ESS16_TDMft)
numRows <- nrow(ESS16_TDMft2)
numCols <- ncol(ESS16_TDMft2)
ESS16_TDMft3 <- ESS16_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS16_TDMTable <- graph.adjacency(ESS16_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(ESS16_TDMTable, vertex.label = V(ESS16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
ESS16_TDM.clusterCoef <- transitivity(ESS16_TDMTable, type="global") #cluster coefficient
ESS16_TDM.degreeCent <- centralization.degree(ESS16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_TDMftn <- as.network.matrix(ESS16_TDMft)
ESS16_TDM.netDensity <- network.density(ESS16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_TDM.entropy <- entropy(ESS16_TDMft) #entropy

ESS16_TDM.netMx <- cbind(ESS16_TDM.netMx, ESS16_TDM.clusterCoef, ESS16_TDM.degreeCent$centralization,
                         ESS16_TDM.netDensity, ESS16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
ESS16_SDg2 <- data.frame(ESS16_SD)
ESS16_SDg2 <- ESS16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_SDg2$player1
player2vector <- ESS16_SDg2$player2
ESS16_SDg3 <- ESS16_SDg2
ESS16_SDg3$p1inp2vec <- is.element(ESS16_SDg3$player1, player2vector)
ESS16_SDg3$p2inp1vec <- is.element(ESS16_SDg3$player2, player1vector)

addPlayer1 <- ESS16_SDg3[ which(ESS16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_SDg3[ which(ESS16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_SDg2 <- rbind(ESS16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
ESS16_SDft <- ftable(ESS16_SDg2$player1, ESS16_SDg2$player2)
ESS16_SDft2 <- as.matrix(ESS16_SDft)
numRows <- nrow(ESS16_SDft2)
numCols <- ncol(ESS16_SDft2)
ESS16_SDft3 <- ESS16_SDft2[c(2:numRows) , c(2:numCols)]
ESS16_SDTable <- graph.adjacency(ESS16_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(ESS16_SDTable, vertex.label = V(ESS16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
ESS16_SD.clusterCoef <- transitivity(ESS16_SDTable, type="global") #cluster coefficient
ESS16_SD.degreeCent <- centralization.degree(ESS16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_SDftn <- as.network.matrix(ESS16_SDft)
ESS16_SD.netDensity <- network.density(ESS16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_SD.entropy <- entropy(ESS16_SDft) #entropy

ESS16_SD.netMx <- cbind(ESS16_SD.netMx, ESS16_SD.clusterCoef, ESS16_SD.degreeCent$centralization,
                        ESS16_SD.netDensity, ESS16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
ESS16_TDg2 <- data.frame(ESS16_TD)
ESS16_TDg2 <- ESS16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_TDg2$player1
player2vector <- ESS16_TDg2$player2
ESS16_TDg3 <- ESS16_TDg2
ESS16_TDg3$p1inp2vec <- is.element(ESS16_TDg3$player1, player2vector)
ESS16_TDg3$p2inp1vec <- is.element(ESS16_TDg3$player2, player1vector)

addPlayer1 <- ESS16_TDg3[ which(ESS16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_TDg3[ which(ESS16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_TDg2 <- rbind(ESS16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
ESS16_TDft <- ftable(ESS16_TDg2$player1, ESS16_TDg2$player2)
ESS16_TDft2 <- as.matrix(ESS16_TDft)
numRows <- nrow(ESS16_TDft2)
numCols <- ncol(ESS16_TDft2)
ESS16_TDft3 <- ESS16_TDft2[c(2:numRows) , c(2:numCols)]
ESS16_TDTable <- graph.adjacency(ESS16_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(ESS16_TDTable, vertex.label = V(ESS16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
ESS16_TD.clusterCoef <- transitivity(ESS16_TDTable, type="global") #cluster coefficient
ESS16_TD.degreeCent <- centralization.degree(ESS16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_TDftn <- as.network.matrix(ESS16_TDft)
ESS16_TD.netDensity <- network.density(ESS16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_TD.entropy <- entropy(ESS16_TDft) #entropy

ESS16_TD.netMx <- cbind(ESS16_TD.netMx, ESS16_TD.clusterCoef, ESS16_TD.degreeCent$centralization,
                        ESS16_TD.netDensity, ESS16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
ESS16_QTg2 <- data.frame(ESS16_QT)
ESS16_QTg2 <- ESS16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS16_QTg2$player1
player2vector <- ESS16_QTg2$player2
ESS16_QTg3 <- ESS16_QTg2
ESS16_QTg3$p1inp2vec <- is.element(ESS16_QTg3$player1, player2vector)
ESS16_QTg3$p2inp1vec <- is.element(ESS16_QTg3$player2, player1vector)

addPlayer1 <- ESS16_QTg3[ which(ESS16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS16_QTg3[ which(ESS16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS16_QTg2 <- rbind(ESS16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
ESS16_QTft <- ftable(ESS16_QTg2$player1, ESS16_QTg2$player2)
ESS16_QTft2 <- as.matrix(ESS16_QTft)
numRows <- nrow(ESS16_QTft2)
numCols <- ncol(ESS16_QTft2)
ESS16_QTft3 <- ESS16_QTft2[c(2:numRows) , c(2:numCols)]
ESS16_QTTable <- graph.adjacency(ESS16_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(ESS16_QTTable, vertex.label = V(ESS16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
ESS16_QT.clusterCoef <- transitivity(ESS16_QTTable, type="global") #cluster coefficient
ESS16_QT.degreeCent <- centralization.degree(ESS16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS16_QTftn <- as.network.matrix(ESS16_QTft)
ESS16_QT.netDensity <- network.density(ESS16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS16_QT.entropy <- entropy(ESS16_QTft) #entropy

ESS16_QT.netMx <- cbind(ESS16_QT.netMx, ESS16_QT.clusterCoef, ESS16_QT.degreeCent$centralization,
                        ESS16_QT.netDensity, ESS16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS16_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "FRE"
KIoutcome = "Goal_F"
FRE16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
FRE16_Gg2 <- data.frame(FRE16_G)
FRE16_Gg2 <- FRE16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_Gg2$player1
player2vector <- FRE16_Gg2$player2
FRE16_Gg3 <- FRE16_Gg2
FRE16_Gg3$p1inp2vec <- is.element(FRE16_Gg3$player1, player2vector)
FRE16_Gg3$p2inp1vec <- is.element(FRE16_Gg3$player2, player1vector)

addPlayer1 <- FRE16_Gg3[ which(FRE16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_Gg3[ which(FRE16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_Gg2 <- rbind(FRE16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
FRE16_Gft <- ftable(FRE16_Gg2$player1, FRE16_Gg2$player2)
FRE16_Gft2 <- as.matrix(FRE16_Gft)
numRows <- nrow(FRE16_Gft2)
numCols <- ncol(FRE16_Gft2)
FRE16_Gft3 <- FRE16_Gft2[c(2:numRows) , c(2:numCols)]
FRE16_GTable <- graph.adjacency(FRE16_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(FRE16_GTable, vertex.label = V(FRE16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
FRE16_G.clusterCoef <- transitivity(FRE16_GTable, type="global") #cluster coefficient
FRE16_G.degreeCent <- centralization.degree(FRE16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_Gftn <- as.network.matrix(FRE16_Gft)
FRE16_G.netDensity <- network.density(FRE16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_G.entropy <- entropy(FRE16_Gft) #entropy

FRE16_G.netMx <- cbind(FRE16_G.netMx, FRE16_G.clusterCoef, FRE16_G.degreeCent$centralization,
                       FRE16_G.netDensity, FRE16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "FRE"
KIoutcome = "Behind_F"
FRE16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
FRE16_Bg2 <- data.frame(FRE16_B)
FRE16_Bg2 <- FRE16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_Bg2$player1
player2vector <- FRE16_Bg2$player2
FRE16_Bg3 <- FRE16_Bg2
FRE16_Bg3$p1inp2vec <- is.element(FRE16_Bg3$player1, player2vector)
FRE16_Bg3$p2inp1vec <- is.element(FRE16_Bg3$player2, player1vector)

addPlayer1 <- FRE16_Bg3[ which(FRE16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_Bg3[ which(FRE16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_Bg2 <- rbind(FRE16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
FRE16_Bft <- ftable(FRE16_Bg2$player1, FRE16_Bg2$player2)
FRE16_Bft2 <- as.matrix(FRE16_Bft)
numRows <- nrow(FRE16_Bft2)
numCols <- ncol(FRE16_Bft2)
FRE16_Bft3 <- FRE16_Bft2[c(2:numRows) , c(2:numCols)]
FRE16_BTable <- graph.adjacency(FRE16_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(FRE16_BTable, vertex.label = V(FRE16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
FRE16_B.clusterCoef <- transitivity(FRE16_BTable, type="global") #cluster coefficient
FRE16_B.degreeCent <- centralization.degree(FRE16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_Bftn <- as.network.matrix(FRE16_Bft)
FRE16_B.netDensity <- network.density(FRE16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_B.entropy <- entropy(FRE16_Bft) #entropy

FRE16_B.netMx <- cbind(FRE16_B.netMx, FRE16_B.clusterCoef, FRE16_B.degreeCent$centralization,
                       FRE16_B.netDensity, FRE16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************

round = 16
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
FRE16_SFg2 <- data.frame(FRE16_SF)
FRE16_SFg2 <- FRE16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_SFg2$player1
player2vector <- FRE16_SFg2$player2
FRE16_SFg3 <- FRE16_SFg2
FRE16_SFg3$p1inp2vec <- is.element(FRE16_SFg3$player1, player2vector)
FRE16_SFg3$p2inp1vec <- is.element(FRE16_SFg3$player2, player1vector)

addPlayer1 <- FRE16_SFg3[ which(FRE16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_SFg3[ which(FRE16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_SFg2 <- rbind(FRE16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
FRE16_SFft <- ftable(FRE16_SFg2$player1, FRE16_SFg2$player2)
FRE16_SFft2 <- as.matrix(FRE16_SFft)
numRows <- nrow(FRE16_SFft2)
numCols <- ncol(FRE16_SFft2)
FRE16_SFft3 <- FRE16_SFft2[c(2:numRows) , c(2:numCols)]
FRE16_SFTable <- graph.adjacency(FRE16_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(FRE16_SFTable, vertex.label = V(FRE16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
FRE16_SF.clusterCoef <- transitivity(FRE16_SFTable, type="global") #cluster coefficient
FRE16_SF.degreeCent <- centralization.degree(FRE16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_SFftn <- as.network.matrix(FRE16_SFft)
FRE16_SF.netDensity <- network.density(FRE16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_SF.entropy <- entropy(FRE16_SFft) #entropy

FRE16_SF.netMx <- cbind(FRE16_SF.netMx, FRE16_SF.clusterCoef, FRE16_SF.degreeCent$centralization,
                        FRE16_SF.netDensity, FRE16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************
#NA

round = 16
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
FRE16_TFg2 <- data.frame(FRE16_TF)
FRE16_TFg2 <- FRE16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_TFg2$player1
player2vector <- FRE16_TFg2$player2
FRE16_TFg3 <- FRE16_TFg2
FRE16_TFg3$p1inp2vec <- is.element(FRE16_TFg3$player1, player2vector)
FRE16_TFg3$p2inp1vec <- is.element(FRE16_TFg3$player2, player1vector)

addPlayer1 <- FRE16_TFg3[ which(FRE16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_TFg3[ which(FRE16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_TFg2 <- rbind(FRE16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
FRE16_TFft <- ftable(FRE16_TFg2$player1, FRE16_TFg2$player2)
FRE16_TFft2 <- as.matrix(FRE16_TFft)
numRows <- nrow(FRE16_TFft2)
numCols <- ncol(FRE16_TFft2)
FRE16_TFft3 <- FRE16_TFft2[c(2:numRows) , c(2:numCols)]
FRE16_TFTable <- graph.adjacency(FRE16_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(FRE16_TFTable, vertex.label = V(FRE16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
FRE16_TF.clusterCoef <- transitivity(FRE16_TFTable, type="global") #cluster coefficient
FRE16_TF.degreeCent <- centralization.degree(FRE16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_TFftn <- as.network.matrix(FRE16_TFft)
FRE16_TF.netDensity <- network.density(FRE16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_TF.entropy <- entropy(FRE16_TFft) #entropy

FRE16_TF.netMx <- cbind(FRE16_TF.netMx, FRE16_TF.clusterCoef, FRE16_TF.degreeCent$centralization,
                        FRE16_TF.netDensity, FRE16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
FRE16_SAMg2 <- data.frame(FRE16_SAM)
FRE16_SAMg2 <- FRE16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_SAMg2$player1
player2vector <- FRE16_SAMg2$player2
FRE16_SAMg3 <- FRE16_SAMg2
FRE16_SAMg3$p1inp2vec <- is.element(FRE16_SAMg3$player1, player2vector)
FRE16_SAMg3$p2inp1vec <- is.element(FRE16_SAMg3$player2, player1vector)

addPlayer1 <- FRE16_SAMg3[ which(FRE16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_SAMg3[ which(FRE16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_SAMg2 <- rbind(FRE16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
FRE16_SAMft <- ftable(FRE16_SAMg2$player1, FRE16_SAMg2$player2)
FRE16_SAMft2 <- as.matrix(FRE16_SAMft)
numRows <- nrow(FRE16_SAMft2)
numCols <- ncol(FRE16_SAMft2)
FRE16_SAMft3 <- FRE16_SAMft2[c(2:numRows) , c(2:numCols)]
FRE16_SAMTable <- graph.adjacency(FRE16_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(FRE16_SAMTable, vertex.label = V(FRE16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
FRE16_SAM.clusterCoef <- transitivity(FRE16_SAMTable, type="global") #cluster coefficient
FRE16_SAM.degreeCent <- centralization.degree(FRE16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_SAMftn <- as.network.matrix(FRE16_SAMft)
FRE16_SAM.netDensity <- network.density(FRE16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_SAM.entropy <- entropy(FRE16_SAMft) #entropy

FRE16_SAM.netMx <- cbind(FRE16_SAM.netMx, FRE16_SAM.clusterCoef, FRE16_SAM.degreeCent$centralization,
                         FRE16_SAM.netDensity, FRE16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
FRE16_TAMg2 <- data.frame(FRE16_TAM)
FRE16_TAMg2 <- FRE16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_TAMg2$player1
player2vector <- FRE16_TAMg2$player2
FRE16_TAMg3 <- FRE16_TAMg2
FRE16_TAMg3$p1inp2vec <- is.element(FRE16_TAMg3$player1, player2vector)
FRE16_TAMg3$p2inp1vec <- is.element(FRE16_TAMg3$player2, player1vector)

addPlayer1 <- FRE16_TAMg3[ which(FRE16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_TAMg3[ which(FRE16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_TAMg2 <- rbind(FRE16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
FRE16_TAMft <- ftable(FRE16_TAMg2$player1, FRE16_TAMg2$player2)
FRE16_TAMft2 <- as.matrix(FRE16_TAMft)
numRows <- nrow(FRE16_TAMft2)
numCols <- ncol(FRE16_TAMft2)
FRE16_TAMft3 <- FRE16_TAMft2[c(2:numRows) , c(2:numCols)]
FRE16_TAMTable <- graph.adjacency(FRE16_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(FRE16_TAMTable, vertex.label = V(FRE16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
FRE16_TAM.clusterCoef <- transitivity(FRE16_TAMTable, type="global") #cluster coefficient
FRE16_TAM.degreeCent <- centralization.degree(FRE16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_TAMftn <- as.network.matrix(FRE16_TAMft)
FRE16_TAM.netDensity <- network.density(FRE16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_TAM.entropy <- entropy(FRE16_TAMft) #entropy

FRE16_TAM.netMx <- cbind(FRE16_TAM.netMx, FRE16_TAM.clusterCoef, FRE16_TAM.degreeCent$centralization,
                         FRE16_TAM.netDensity, FRE16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
FRE16_SDMg2 <- data.frame(FRE16_SDM)
FRE16_SDMg2 <- FRE16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_SDMg2$player1
player2vector <- FRE16_SDMg2$player2
FRE16_SDMg3 <- FRE16_SDMg2
FRE16_SDMg3$p1inp2vec <- is.element(FRE16_SDMg3$player1, player2vector)
FRE16_SDMg3$p2inp1vec <- is.element(FRE16_SDMg3$player2, player1vector)

addPlayer1 <- FRE16_SDMg3[ which(FRE16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_SDMg3[ which(FRE16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_SDMg2 <- rbind(FRE16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
FRE16_SDMft <- ftable(FRE16_SDMg2$player1, FRE16_SDMg2$player2)
FRE16_SDMft2 <- as.matrix(FRE16_SDMft)
numRows <- nrow(FRE16_SDMft2)
numCols <- ncol(FRE16_SDMft2)
FRE16_SDMft3 <- FRE16_SDMft2[c(2:numRows) , c(2:numCols)]
FRE16_SDMTable <- graph.adjacency(FRE16_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(FRE16_SDMTable, vertex.label = V(FRE16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
FRE16_SDM.clusterCoef <- transitivity(FRE16_SDMTable, type="global") #cluster coefficient
FRE16_SDM.degreeCent <- centralization.degree(FRE16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_SDMftn <- as.network.matrix(FRE16_SDMft)
FRE16_SDM.netDensity <- network.density(FRE16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_SDM.entropy <- entropy(FRE16_SDMft) #entropy

FRE16_SDM.netMx <- cbind(FRE16_SDM.netMx, FRE16_SDM.clusterCoef, FRE16_SDM.degreeCent$centralization,
                         FRE16_SDM.netDensity, FRE16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************
#NA

round = 16
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
FRE16_TDMg2 <- data.frame(FRE16_TDM)
FRE16_TDMg2 <- FRE16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_TDMg2$player1
player2vector <- FRE16_TDMg2$player2
FRE16_TDMg3 <- FRE16_TDMg2
FRE16_TDMg3$p1inp2vec <- is.element(FRE16_TDMg3$player1, player2vector)
FRE16_TDMg3$p2inp1vec <- is.element(FRE16_TDMg3$player2, player1vector)

addPlayer1 <- FRE16_TDMg3[ which(FRE16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_TDMg3[ which(FRE16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_TDMg2 <- rbind(FRE16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
FRE16_TDMft <- ftable(FRE16_TDMg2$player1, FRE16_TDMg2$player2)
FRE16_TDMft2 <- as.matrix(FRE16_TDMft)
numRows <- nrow(FRE16_TDMft2)
numCols <- ncol(FRE16_TDMft2)
FRE16_TDMft3 <- FRE16_TDMft2[c(2:numRows) , c(2:numCols)]
FRE16_TDMTable <- graph.adjacency(FRE16_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(FRE16_TDMTable, vertex.label = V(FRE16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
FRE16_TDM.clusterCoef <- transitivity(FRE16_TDMTable, type="global") #cluster coefficient
FRE16_TDM.degreeCent <- centralization.degree(FRE16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_TDMftn <- as.network.matrix(FRE16_TDMft)
FRE16_TDM.netDensity <- network.density(FRE16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_TDM.entropy <- entropy(FRE16_TDMft) #entropy

FRE16_TDM.netMx <- cbind(FRE16_TDM.netMx, FRE16_TDM.clusterCoef, FRE16_TDM.degreeCent$centralization,
                         FRE16_TDM.netDensity, FRE16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
FRE16_SDg2 <- data.frame(FRE16_SD)
FRE16_SDg2 <- FRE16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_SDg2$player1
player2vector <- FRE16_SDg2$player2
FRE16_SDg3 <- FRE16_SDg2
FRE16_SDg3$p1inp2vec <- is.element(FRE16_SDg3$player1, player2vector)
FRE16_SDg3$p2inp1vec <- is.element(FRE16_SDg3$player2, player1vector)

addPlayer1 <- FRE16_SDg3[ which(FRE16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_SDg3[ which(FRE16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_SDg2 <- rbind(FRE16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
FRE16_SDft <- ftable(FRE16_SDg2$player1, FRE16_SDg2$player2)
FRE16_SDft2 <- as.matrix(FRE16_SDft)
numRows <- nrow(FRE16_SDft2)
numCols <- ncol(FRE16_SDft2)
FRE16_SDft3 <- FRE16_SDft2[c(2:numRows) , c(2:numCols)]
FRE16_SDTable <- graph.adjacency(FRE16_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(FRE16_SDTable, vertex.label = V(FRE16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
FRE16_SD.clusterCoef <- transitivity(FRE16_SDTable, type="global") #cluster coefficient
FRE16_SD.degreeCent <- centralization.degree(FRE16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_SDftn <- as.network.matrix(FRE16_SDft)
FRE16_SD.netDensity <- network.density(FRE16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_SD.entropy <- entropy(FRE16_SDft) #entropy

FRE16_SD.netMx <- cbind(FRE16_SD.netMx, FRE16_SD.clusterCoef, FRE16_SD.degreeCent$centralization,
                        FRE16_SD.netDensity, FRE16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
FRE16_TDg2 <- data.frame(FRE16_TD)
FRE16_TDg2 <- FRE16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_TDg2$player1
player2vector <- FRE16_TDg2$player2
FRE16_TDg3 <- FRE16_TDg2
FRE16_TDg3$p1inp2vec <- is.element(FRE16_TDg3$player1, player2vector)
FRE16_TDg3$p2inp1vec <- is.element(FRE16_TDg3$player2, player1vector)

addPlayer1 <- FRE16_TDg3[ which(FRE16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_TDg3[ which(FRE16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_TDg2 <- rbind(FRE16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
FRE16_TDft <- ftable(FRE16_TDg2$player1, FRE16_TDg2$player2)
FRE16_TDft2 <- as.matrix(FRE16_TDft)
numRows <- nrow(FRE16_TDft2)
numCols <- ncol(FRE16_TDft2)
FRE16_TDft3 <- FRE16_TDft2[c(2:numRows) , c(2:numCols)]
FRE16_TDTable <- graph.adjacency(FRE16_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(FRE16_TDTable, vertex.label = V(FRE16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
FRE16_TD.clusterCoef <- transitivity(FRE16_TDTable, type="global") #cluster coefficient
FRE16_TD.degreeCent <- centralization.degree(FRE16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_TDftn <- as.network.matrix(FRE16_TDft)
FRE16_TD.netDensity <- network.density(FRE16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_TD.entropy <- entropy(FRE16_TDft) #entropy

FRE16_TD.netMx <- cbind(FRE16_TD.netMx, FRE16_TD.clusterCoef, FRE16_TD.degreeCent$centralization,
                        FRE16_TD.netDensity, FRE16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
FRE16_QTg2 <- data.frame(FRE16_QT)
FRE16_QTg2 <- FRE16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE16_QTg2$player1
player2vector <- FRE16_QTg2$player2
FRE16_QTg3 <- FRE16_QTg2
FRE16_QTg3$p1inp2vec <- is.element(FRE16_QTg3$player1, player2vector)
FRE16_QTg3$p2inp1vec <- is.element(FRE16_QTg3$player2, player1vector)

addPlayer1 <- FRE16_QTg3[ which(FRE16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE16_QTg3[ which(FRE16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE16_QTg2 <- rbind(FRE16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
FRE16_QTft <- ftable(FRE16_QTg2$player1, FRE16_QTg2$player2)
FRE16_QTft2 <- as.matrix(FRE16_QTft)
numRows <- nrow(FRE16_QTft2)
numCols <- ncol(FRE16_QTft2)
FRE16_QTft3 <- FRE16_QTft2[c(2:numRows) , c(2:numCols)]
FRE16_QTTable <- graph.adjacency(FRE16_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(FRE16_QTTable, vertex.label = V(FRE16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
FRE16_QT.clusterCoef <- transitivity(FRE16_QTTable, type="global") #cluster coefficient
FRE16_QT.degreeCent <- centralization.degree(FRE16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE16_QTftn <- as.network.matrix(FRE16_QTft)
FRE16_QT.netDensity <- network.density(FRE16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE16_QT.entropy <- entropy(FRE16_QTft) #entropy

FRE16_QT.netMx <- cbind(FRE16_QT.netMx, FRE16_QT.clusterCoef, FRE16_QT.degreeCent$centralization,
                        FRE16_QT.netDensity, FRE16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE16_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************

round = 16
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
GCFC16_Gg2 <- data.frame(GCFC16_G)
GCFC16_Gg2 <- GCFC16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_Gg2$player1
player2vector <- GCFC16_Gg2$player2
GCFC16_Gg3 <- GCFC16_Gg2
GCFC16_Gg3$p1inp2vec <- is.element(GCFC16_Gg3$player1, player2vector)
GCFC16_Gg3$p2inp1vec <- is.element(GCFC16_Gg3$player2, player1vector)

addPlayer1 <- GCFC16_Gg3[ which(GCFC16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC16_Gg3[ which(GCFC16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_Gg2 <- rbind(GCFC16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
GCFC16_Gft <- ftable(GCFC16_Gg2$player1, GCFC16_Gg2$player2)
GCFC16_Gft2 <- as.matrix(GCFC16_Gft)
numRows <- nrow(GCFC16_Gft2)
numCols <- ncol(GCFC16_Gft2)
GCFC16_Gft3 <- GCFC16_Gft2[c(2:numRows) , c(2:numCols)]
GCFC16_GTable <- graph.adjacency(GCFC16_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(GCFC16_GTable, vertex.label = V(GCFC16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
GCFC16_G.clusterCoef <- transitivity(GCFC16_GTable, type="global") #cluster coefficient
GCFC16_G.degreeCent <- centralization.degree(GCFC16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_Gftn <- as.network.matrix(GCFC16_Gft)
GCFC16_G.netDensity <- network.density(GCFC16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_G.entropy <- entropy(GCFC16_Gft) #entropy

GCFC16_G.netMx <- cbind(GCFC16_G.netMx, GCFC16_G.clusterCoef, GCFC16_G.degreeCent$centralization,
                        GCFC16_G.netDensity, GCFC16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
GCFC16_Bg2 <- data.frame(GCFC16_B)
GCFC16_Bg2 <- GCFC16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_Bg2$player1
player2vector <- GCFC16_Bg2$player2
GCFC16_Bg3 <- GCFC16_Bg2
GCFC16_Bg3$p1inp2vec <- is.element(GCFC16_Bg3$player1, player2vector)
GCFC16_Bg3$p2inp1vec <- is.element(GCFC16_Bg3$player2, player1vector)

addPlayer1 <- GCFC16_Bg3[ which(GCFC16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC16_Bg3[ which(GCFC16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_Bg2 <- rbind(GCFC16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
GCFC16_Bft <- ftable(GCFC16_Bg2$player1, GCFC16_Bg2$player2)
GCFC16_Bft2 <- as.matrix(GCFC16_Bft)
numRows <- nrow(GCFC16_Bft2)
numCols <- ncol(GCFC16_Bft2)
GCFC16_Bft3 <- GCFC16_Bft2[c(2:numRows) , c(2:numCols)]
GCFC16_BTable <- graph.adjacency(GCFC16_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(GCFC16_BTable, vertex.label = V(GCFC16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
GCFC16_B.clusterCoef <- transitivity(GCFC16_BTable, type="global") #cluster coefficient
GCFC16_B.degreeCent <- centralization.degree(GCFC16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_Bftn <- as.network.matrix(GCFC16_Bft)
GCFC16_B.netDensity <- network.density(GCFC16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_B.entropy <- entropy(GCFC16_Bft) #entropy

GCFC16_B.netMx <- cbind(GCFC16_B.netMx, GCFC16_B.clusterCoef, GCFC16_B.degreeCent$centralization,
                        GCFC16_B.netDensity, GCFC16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
GCFC16_SFg2 <- data.frame(GCFC16_SF)
GCFC16_SFg2 <- GCFC16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_SFg2$player1
player2vector <- GCFC16_SFg2$player2
GCFC16_SFg3 <- GCFC16_SFg2
GCFC16_SFg3$p1inp2vec <- is.element(GCFC16_SFg3$player1, player2vector)
GCFC16_SFg3$p2inp1vec <- is.element(GCFC16_SFg3$player2, player1vector)

addPlayer1 <- GCFC16_SFg3[ which(GCFC16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC16_SFg2 <- rbind(GCFC16_SFg2, addPlayer1)

#ROUND 16, FWD Stoppage graph using weighted edges
GCFC16_SFft <- ftable(GCFC16_SFg2$player1, GCFC16_SFg2$player2)
GCFC16_SFft2 <- as.matrix(GCFC16_SFft)
numRows <- nrow(GCFC16_SFft2)
numCols <- ncol(GCFC16_SFft2)
GCFC16_SFft3 <- GCFC16_SFft2[c(2:numRows) , c(1:numCols)]
GCFC16_SFTable <- graph.adjacency(GCFC16_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(GCFC16_SFTable, vertex.label = V(GCFC16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
GCFC16_SF.clusterCoef <- transitivity(GCFC16_SFTable, type="global") #cluster coefficient
GCFC16_SF.degreeCent <- centralization.degree(GCFC16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_SFftn <- as.network.matrix(GCFC16_SFft)
GCFC16_SF.netDensity <- network.density(GCFC16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_SF.entropy <- entropy(GCFC16_SFft) #entropy

GCFC16_SF.netMx <- cbind(GCFC16_SF.netMx, GCFC16_SF.clusterCoef, GCFC16_SF.degreeCent$centralization,
                         GCFC16_SF.netDensity, GCFC16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
GCFC16_TFg2 <- data.frame(GCFC16_TF)
GCFC16_TFg2 <- GCFC16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_TFg2$player1
player2vector <- GCFC16_TFg2$player2
GCFC16_TFg3 <- GCFC16_TFg2
GCFC16_TFg3$p1inp2vec <- is.element(GCFC16_TFg3$player1, player2vector)
GCFC16_TFg3$p2inp1vec <- is.element(GCFC16_TFg3$player2, player1vector)

addPlayer1 <- GCFC16_TFg3[ which(GCFC16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC16_TFg3[ which(GCFC16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_TFg2 <- rbind(GCFC16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
GCFC16_TFft <- ftable(GCFC16_TFg2$player1, GCFC16_TFg2$player2)
GCFC16_TFft2 <- as.matrix(GCFC16_TFft)
numRows <- nrow(GCFC16_TFft2)
numCols <- ncol(GCFC16_TFft2)
GCFC16_TFft3 <- GCFC16_TFft2[c(2:numRows) , c(2:numCols)]
GCFC16_TFTable <- graph.adjacency(GCFC16_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(GCFC16_TFTable, vertex.label = V(GCFC16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
GCFC16_TF.clusterCoef <- transitivity(GCFC16_TFTable, type="global") #cluster coefficient
GCFC16_TF.degreeCent <- centralization.degree(GCFC16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_TFftn <- as.network.matrix(GCFC16_TFft)
GCFC16_TF.netDensity <- network.density(GCFC16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_TF.entropy <- entropy(GCFC16_TFft) #entropy

GCFC16_TF.netMx <- cbind(GCFC16_TF.netMx, GCFC16_TF.clusterCoef, GCFC16_TF.degreeCent$centralization,
                         GCFC16_TF.netDensity, GCFC16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
GCFC16_SAMg2 <- data.frame(GCFC16_SAM)
GCFC16_SAMg2 <- GCFC16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_SAMg2$player1
player2vector <- GCFC16_SAMg2$player2
GCFC16_SAMg3 <- GCFC16_SAMg2
GCFC16_SAMg3$p1inp2vec <- is.element(GCFC16_SAMg3$player1, player2vector)
GCFC16_SAMg3$p2inp1vec <- is.element(GCFC16_SAMg3$player2, player1vector)

addPlayer1 <- GCFC16_SAMg3[ which(GCFC16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC16_SAMg3[ which(GCFC16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_SAMg2 <- rbind(GCFC16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
GCFC16_SAMft <- ftable(GCFC16_SAMg2$player1, GCFC16_SAMg2$player2)
GCFC16_SAMft2 <- as.matrix(GCFC16_SAMft)
numRows <- nrow(GCFC16_SAMft2)
numCols <- ncol(GCFC16_SAMft2)
GCFC16_SAMft3 <- GCFC16_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC16_SAMTable <- graph.adjacency(GCFC16_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(GCFC16_SAMTable, vertex.label = V(GCFC16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
GCFC16_SAM.clusterCoef <- transitivity(GCFC16_SAMTable, type="global") #cluster coefficient
GCFC16_SAM.degreeCent <- centralization.degree(GCFC16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_SAMftn <- as.network.matrix(GCFC16_SAMft)
GCFC16_SAM.netDensity <- network.density(GCFC16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_SAM.entropy <- entropy(GCFC16_SAMft) #entropy

GCFC16_SAM.netMx <- cbind(GCFC16_SAM.netMx, GCFC16_SAM.clusterCoef, GCFC16_SAM.degreeCent$centralization,
                          GCFC16_SAM.netDensity, GCFC16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************
#NA

round = 16
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
GCFC16_TAMg2 <- data.frame(GCFC16_TAM)
GCFC16_TAMg2 <- GCFC16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_TAMg2$player1
player2vector <- GCFC16_TAMg2$player2
GCFC16_TAMg3 <- GCFC16_TAMg2
GCFC16_TAMg3$p1inp2vec <- is.element(GCFC16_TAMg3$player1, player2vector)
GCFC16_TAMg3$p2inp1vec <- is.element(GCFC16_TAMg3$player2, player1vector)

addPlayer1 <- GCFC16_TAMg3[ which(GCFC16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC16_TAMg3[ which(GCFC16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_TAMg2 <- rbind(GCFC16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
GCFC16_TAMft <- ftable(GCFC16_TAMg2$player1, GCFC16_TAMg2$player2)
GCFC16_TAMft2 <- as.matrix(GCFC16_TAMft)
numRows <- nrow(GCFC16_TAMft2)
numCols <- ncol(GCFC16_TAMft2)
GCFC16_TAMft3 <- GCFC16_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC16_TAMTable <- graph.adjacency(GCFC16_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(GCFC16_TAMTable, vertex.label = V(GCFC16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
GCFC16_TAM.clusterCoef <- transitivity(GCFC16_TAMTable, type="global") #cluster coefficient
GCFC16_TAM.degreeCent <- centralization.degree(GCFC16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_TAMftn <- as.network.matrix(GCFC16_TAMft)
GCFC16_TAM.netDensity <- network.density(GCFC16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_TAM.entropy <- entropy(GCFC16_TAMft) #entropy

GCFC16_TAM.netMx <- cbind(GCFC16_TAM.netMx, GCFC16_TAM.clusterCoef, GCFC16_TAM.degreeCent$centralization,
                          GCFC16_TAM.netDensity, GCFC16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
GCFC16_SDMg2 <- data.frame(GCFC16_SDM)
GCFC16_SDMg2 <- GCFC16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_SDMg2$player1
player2vector <- GCFC16_SDMg2$player2
GCFC16_SDMg3 <- GCFC16_SDMg2
GCFC16_SDMg3$p1inp2vec <- is.element(GCFC16_SDMg3$player1, player2vector)
GCFC16_SDMg3$p2inp1vec <- is.element(GCFC16_SDMg3$player2, player1vector)

addPlayer1 <- GCFC16_SDMg3[ which(GCFC16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC16_SDMg3[ which(GCFC16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_SDMg2 <- rbind(GCFC16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
GCFC16_SDMft <- ftable(GCFC16_SDMg2$player1, GCFC16_SDMg2$player2)
GCFC16_SDMft2 <- as.matrix(GCFC16_SDMft)
numRows <- nrow(GCFC16_SDMft2)
numCols <- ncol(GCFC16_SDMft2)
GCFC16_SDMft3 <- GCFC16_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC16_SDMTable <- graph.adjacency(GCFC16_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(GCFC16_SDMTable, vertex.label = V(GCFC16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
GCFC16_SDM.clusterCoef <- transitivity(GCFC16_SDMTable, type="global") #cluster coefficient
GCFC16_SDM.degreeCent <- centralization.degree(GCFC16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_SDMftn <- as.network.matrix(GCFC16_SDMft)
GCFC16_SDM.netDensity <- network.density(GCFC16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_SDM.entropy <- entropy(GCFC16_SDMft) #entropy

GCFC16_SDM.netMx <- cbind(GCFC16_SDM.netMx, GCFC16_SDM.clusterCoef, GCFC16_SDM.degreeCent$centralization,
                          GCFC16_SDM.netDensity, GCFC16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
GCFC16_TDMg2 <- data.frame(GCFC16_TDM)
GCFC16_TDMg2 <- GCFC16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_TDMg2$player1
player2vector <- GCFC16_TDMg2$player2
GCFC16_TDMg3 <- GCFC16_TDMg2
GCFC16_TDMg3$p1inp2vec <- is.element(GCFC16_TDMg3$player1, player2vector)
GCFC16_TDMg3$p2inp1vec <- is.element(GCFC16_TDMg3$player2, player1vector)

addPlayer1 <- GCFC16_TDMg3[ which(GCFC16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC16_TDMg3[ which(GCFC16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_TDMg2 <- rbind(GCFC16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
GCFC16_TDMft <- ftable(GCFC16_TDMg2$player1, GCFC16_TDMg2$player2)
GCFC16_TDMft2 <- as.matrix(GCFC16_TDMft)
numRows <- nrow(GCFC16_TDMft2)
numCols <- ncol(GCFC16_TDMft2)
GCFC16_TDMft3 <- GCFC16_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC16_TDMTable <- graph.adjacency(GCFC16_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(GCFC16_TDMTable, vertex.label = V(GCFC16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
GCFC16_TDM.clusterCoef <- transitivity(GCFC16_TDMTable, type="global") #cluster coefficient
GCFC16_TDM.degreeCent <- centralization.degree(GCFC16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_TDMftn <- as.network.matrix(GCFC16_TDMft)
GCFC16_TDM.netDensity <- network.density(GCFC16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_TDM.entropy <- entropy(GCFC16_TDMft) #entropy

GCFC16_TDM.netMx <- cbind(GCFC16_TDM.netMx, GCFC16_TDM.clusterCoef, GCFC16_TDM.degreeCent$centralization,
                          GCFC16_TDM.netDensity, GCFC16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
GCFC16_SDg2 <- data.frame(GCFC16_SD)
GCFC16_SDg2 <- GCFC16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_SDg2$player1
player2vector <- GCFC16_SDg2$player2
GCFC16_SDg3 <- GCFC16_SDg2
GCFC16_SDg3$p1inp2vec <- is.element(GCFC16_SDg3$player1, player2vector)
GCFC16_SDg3$p2inp1vec <- is.element(GCFC16_SDg3$player2, player1vector)

addPlayer1 <- GCFC16_SDg3[ which(GCFC16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC16_SDg3[ which(GCFC16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_SDg2 <- rbind(GCFC16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
GCFC16_SDft <- ftable(GCFC16_SDg2$player1, GCFC16_SDg2$player2)
GCFC16_SDft2 <- as.matrix(GCFC16_SDft)
numRows <- nrow(GCFC16_SDft2)
numCols <- ncol(GCFC16_SDft2)
GCFC16_SDft3 <- GCFC16_SDft2[c(2:numRows) , c(2:numCols)]
GCFC16_SDTable <- graph.adjacency(GCFC16_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(GCFC16_SDTable, vertex.label = V(GCFC16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
GCFC16_SD.clusterCoef <- transitivity(GCFC16_SDTable, type="global") #cluster coefficient
GCFC16_SD.degreeCent <- centralization.degree(GCFC16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_SDftn <- as.network.matrix(GCFC16_SDft)
GCFC16_SD.netDensity <- network.density(GCFC16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_SD.entropy <- entropy(GCFC16_SDft) #entropy

GCFC16_SD.netMx <- cbind(GCFC16_SD.netMx, GCFC16_SD.clusterCoef, GCFC16_SD.degreeCent$centralization,
                         GCFC16_SD.netDensity, GCFC16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************

round = 16
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
GCFC16_TDg2 <- data.frame(GCFC16_TD)
GCFC16_TDg2 <- GCFC16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_TDg2$player1
player2vector <- GCFC16_TDg2$player2
GCFC16_TDg3 <- GCFC16_TDg2
GCFC16_TDg3$p1inp2vec <- is.element(GCFC16_TDg3$player1, player2vector)
GCFC16_TDg3$p2inp1vec <- is.element(GCFC16_TDg3$player2, player1vector)

addPlayer1 <- GCFC16_TDg3[ which(GCFC16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC16_TDg3[ which(GCFC16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_TDg2 <- rbind(GCFC16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
GCFC16_TDft <- ftable(GCFC16_TDg2$player1, GCFC16_TDg2$player2)
GCFC16_TDft2 <- as.matrix(GCFC16_TDft)
numRows <- nrow(GCFC16_TDft2)
numCols <- ncol(GCFC16_TDft2)
GCFC16_TDft3 <- GCFC16_TDft2[c(2:numRows) , c(2:numCols)]
GCFC16_TDTable <- graph.adjacency(GCFC16_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(GCFC16_TDTable, vertex.label = V(GCFC16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
GCFC16_TD.clusterCoef <- transitivity(GCFC16_TDTable, type="global") #cluster coefficient
GCFC16_TD.degreeCent <- centralization.degree(GCFC16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_TDftn <- as.network.matrix(GCFC16_TDft)
GCFC16_TD.netDensity <- network.density(GCFC16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_TD.entropy <- entropy(GCFC16_TDft) #entropy

GCFC16_TD.netMx <- cbind(GCFC16_TD.netMx, GCFC16_TD.clusterCoef, GCFC16_TD.degreeCent$centralization,
                         GCFC16_TD.netDensity, GCFC16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
GCFC16_QTg2 <- data.frame(GCFC16_QT)
GCFC16_QTg2 <- GCFC16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC16_QTg2$player1
player2vector <- GCFC16_QTg2$player2
GCFC16_QTg3 <- GCFC16_QTg2
GCFC16_QTg3$p1inp2vec <- is.element(GCFC16_QTg3$player1, player2vector)
GCFC16_QTg3$p2inp1vec <- is.element(GCFC16_QTg3$player2, player1vector)

addPlayer1 <- GCFC16_QTg3[ which(GCFC16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC16_QTg3[ which(GCFC16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC16_QTg2 <- rbind(GCFC16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
GCFC16_QTft <- ftable(GCFC16_QTg2$player1, GCFC16_QTg2$player2)
GCFC16_QTft2 <- as.matrix(GCFC16_QTft)
numRows <- nrow(GCFC16_QTft2)
numCols <- ncol(GCFC16_QTft2)
GCFC16_QTft3 <- GCFC16_QTft2[c(2:numRows) , c(2:numCols)]
GCFC16_QTTable <- graph.adjacency(GCFC16_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(GCFC16_QTTable, vertex.label = V(GCFC16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
GCFC16_QT.clusterCoef <- transitivity(GCFC16_QTTable, type="global") #cluster coefficient
GCFC16_QT.degreeCent <- centralization.degree(GCFC16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC16_QTftn <- as.network.matrix(GCFC16_QTft)
GCFC16_QT.netDensity <- network.density(GCFC16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC16_QT.entropy <- entropy(GCFC16_QTft) #entropy

GCFC16_QT.netMx <- cbind(GCFC16_QT.netMx, GCFC16_QT.clusterCoef, GCFC16_QT.degreeCent$centralization,
                         GCFC16_QT.netDensity, GCFC16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC16_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************

round = 16
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
GEEL16_Gg2 <- data.frame(GEEL16_G)
GEEL16_Gg2 <- GEEL16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_Gg2$player1
player2vector <- GEEL16_Gg2$player2
GEEL16_Gg3 <- GEEL16_Gg2
GEEL16_Gg3$p1inp2vec <- is.element(GEEL16_Gg3$player1, player2vector)
GEEL16_Gg3$p2inp1vec <- is.element(GEEL16_Gg3$player2, player1vector)

addPlayer1 <- GEEL16_Gg3[ which(GEEL16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_Gg3[ which(GEEL16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_Gg2 <- rbind(GEEL16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
GEEL16_Gft <- ftable(GEEL16_Gg2$player1, GEEL16_Gg2$player2)
GEEL16_Gft2 <- as.matrix(GEEL16_Gft)
numRows <- nrow(GEEL16_Gft2)
numCols <- ncol(GEEL16_Gft2)
GEEL16_Gft3 <- GEEL16_Gft2[c(2:numRows) , c(2:numCols)]
GEEL16_GTable <- graph.adjacency(GEEL16_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(GEEL16_GTable, vertex.label = V(GEEL16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
GEEL16_G.clusterCoef <- transitivity(GEEL16_GTable, type="global") #cluster coefficient
GEEL16_G.degreeCent <- centralization.degree(GEEL16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_Gftn <- as.network.matrix(GEEL16_Gft)
GEEL16_G.netDensity <- network.density(GEEL16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_G.entropy <- entropy(GEEL16_Gft) #entropy

GEEL16_G.netMx <- cbind(GEEL16_G.netMx, GEEL16_G.clusterCoef, GEEL16_G.degreeCent$centralization,
                        GEEL16_G.netDensity, GEEL16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
GEEL16_Bg2 <- data.frame(GEEL16_B)
GEEL16_Bg2 <- GEEL16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_Bg2$player1
player2vector <- GEEL16_Bg2$player2
GEEL16_Bg3 <- GEEL16_Bg2
GEEL16_Bg3$p1inp2vec <- is.element(GEEL16_Bg3$player1, player2vector)
GEEL16_Bg3$p2inp1vec <- is.element(GEEL16_Bg3$player2, player1vector)

addPlayer1 <- GEEL16_Bg3[ which(GEEL16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_Bg3[ which(GEEL16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_Bg2 <- rbind(GEEL16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
GEEL16_Bft <- ftable(GEEL16_Bg2$player1, GEEL16_Bg2$player2)
GEEL16_Bft2 <- as.matrix(GEEL16_Bft)
numRows <- nrow(GEEL16_Bft2)
numCols <- ncol(GEEL16_Bft2)
GEEL16_Bft3 <- GEEL16_Bft2[c(2:numRows) , c(2:numCols)]
GEEL16_BTable <- graph.adjacency(GEEL16_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(GEEL16_BTable, vertex.label = V(GEEL16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
GEEL16_B.clusterCoef <- transitivity(GEEL16_BTable, type="global") #cluster coefficient
GEEL16_B.degreeCent <- centralization.degree(GEEL16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_Bftn <- as.network.matrix(GEEL16_Bft)
GEEL16_B.netDensity <- network.density(GEEL16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_B.entropy <- entropy(GEEL16_Bft) #entropy

GEEL16_B.netMx <- cbind(GEEL16_B.netMx, GEEL16_B.clusterCoef, GEEL16_B.degreeCent$centralization,
                        GEEL16_B.netDensity, GEEL16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
GEEL16_SFg2 <- data.frame(GEEL16_SF)
GEEL16_SFg2 <- GEEL16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_SFg2$player1
player2vector <- GEEL16_SFg2$player2
GEEL16_SFg3 <- GEEL16_SFg2
GEEL16_SFg3$p1inp2vec <- is.element(GEEL16_SFg3$player1, player2vector)
GEEL16_SFg3$p2inp1vec <- is.element(GEEL16_SFg3$player2, player1vector)

addPlayer1 <- GEEL16_SFg3[ which(GEEL16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_SFg3[ which(GEEL16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_SFg2 <- rbind(GEEL16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
GEEL16_SFft <- ftable(GEEL16_SFg2$player1, GEEL16_SFg2$player2)
GEEL16_SFft2 <- as.matrix(GEEL16_SFft)
numRows <- nrow(GEEL16_SFft2)
numCols <- ncol(GEEL16_SFft2)
GEEL16_SFft3 <- GEEL16_SFft2[c(2:numRows) , c(2:numCols)]
GEEL16_SFTable <- graph.adjacency(GEEL16_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(GEEL16_SFTable, vertex.label = V(GEEL16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
GEEL16_SF.clusterCoef <- transitivity(GEEL16_SFTable, type="global") #cluster coefficient
GEEL16_SF.degreeCent <- centralization.degree(GEEL16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_SFftn <- as.network.matrix(GEEL16_SFft)
GEEL16_SF.netDensity <- network.density(GEEL16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_SF.entropy <- entropy(GEEL16_SFft) #entropy

GEEL16_SF.netMx <- cbind(GEEL16_SF.netMx, GEEL16_SF.clusterCoef, GEEL16_SF.degreeCent$centralization,
                         GEEL16_SF.netDensity, GEEL16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
GEEL16_TFg2 <- data.frame(GEEL16_TF)
GEEL16_TFg2 <- GEEL16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_TFg2$player1
player2vector <- GEEL16_TFg2$player2
GEEL16_TFg3 <- GEEL16_TFg2
GEEL16_TFg3$p1inp2vec <- is.element(GEEL16_TFg3$player1, player2vector)
GEEL16_TFg3$p2inp1vec <- is.element(GEEL16_TFg3$player2, player1vector)

addPlayer1 <- GEEL16_TFg3[ which(GEEL16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_TFg3[ which(GEEL16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_TFg2 <- rbind(GEEL16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
GEEL16_TFft <- ftable(GEEL16_TFg2$player1, GEEL16_TFg2$player2)
GEEL16_TFft2 <- as.matrix(GEEL16_TFft)
numRows <- nrow(GEEL16_TFft2)
numCols <- ncol(GEEL16_TFft2)
GEEL16_TFft3 <- GEEL16_TFft2[c(2:numRows) , c(2:numCols)]
GEEL16_TFTable <- graph.adjacency(GEEL16_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(GEEL16_TFTable, vertex.label = V(GEEL16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
GEEL16_TF.clusterCoef <- transitivity(GEEL16_TFTable, type="global") #cluster coefficient
GEEL16_TF.degreeCent <- centralization.degree(GEEL16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_TFftn <- as.network.matrix(GEEL16_TFft)
GEEL16_TF.netDensity <- network.density(GEEL16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_TF.entropy <- entropy(GEEL16_TFft) #entropy

GEEL16_TF.netMx <- cbind(GEEL16_TF.netMx, GEEL16_TF.clusterCoef, GEEL16_TF.degreeCent$centralization,
                         GEEL16_TF.netDensity, GEEL16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************

round = 16
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
GEEL16_SAMg2 <- data.frame(GEEL16_SAM)
GEEL16_SAMg2 <- GEEL16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_SAMg2$player1
player2vector <- GEEL16_SAMg2$player2
GEEL16_SAMg3 <- GEEL16_SAMg2
GEEL16_SAMg3$p1inp2vec <- is.element(GEEL16_SAMg3$player1, player2vector)
GEEL16_SAMg3$p2inp1vec <- is.element(GEEL16_SAMg3$player2, player1vector)

addPlayer1 <- GEEL16_SAMg3[ which(GEEL16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_SAMg3[ which(GEEL16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_SAMg2 <- rbind(GEEL16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
GEEL16_SAMft <- ftable(GEEL16_SAMg2$player1, GEEL16_SAMg2$player2)
GEEL16_SAMft2 <- as.matrix(GEEL16_SAMft)
numRows <- nrow(GEEL16_SAMft2)
numCols <- ncol(GEEL16_SAMft2)
GEEL16_SAMft3 <- GEEL16_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL16_SAMTable <- graph.adjacency(GEEL16_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(GEEL16_SAMTable, vertex.label = V(GEEL16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
GEEL16_SAM.clusterCoef <- transitivity(GEEL16_SAMTable, type="global") #cluster coefficient
GEEL16_SAM.degreeCent <- centralization.degree(GEEL16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_SAMftn <- as.network.matrix(GEEL16_SAMft)
GEEL16_SAM.netDensity <- network.density(GEEL16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_SAM.entropy <- entropy(GEEL16_SAMft) #entropy

GEEL16_SAM.netMx <- cbind(GEEL16_SAM.netMx, GEEL16_SAM.clusterCoef, GEEL16_SAM.degreeCent$centralization,
                          GEEL16_SAM.netDensity, GEEL16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
GEEL16_TAMg2 <- data.frame(GEEL16_TAM)
GEEL16_TAMg2 <- GEEL16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_TAMg2$player1
player2vector <- GEEL16_TAMg2$player2
GEEL16_TAMg3 <- GEEL16_TAMg2
GEEL16_TAMg3$p1inp2vec <- is.element(GEEL16_TAMg3$player1, player2vector)
GEEL16_TAMg3$p2inp1vec <- is.element(GEEL16_TAMg3$player2, player1vector)

addPlayer1 <- GEEL16_TAMg3[ which(GEEL16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_TAMg3[ which(GEEL16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_TAMg2 <- rbind(GEEL16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
GEEL16_TAMft <- ftable(GEEL16_TAMg2$player1, GEEL16_TAMg2$player2)
GEEL16_TAMft2 <- as.matrix(GEEL16_TAMft)
numRows <- nrow(GEEL16_TAMft2)
numCols <- ncol(GEEL16_TAMft2)
GEEL16_TAMft3 <- GEEL16_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL16_TAMTable <- graph.adjacency(GEEL16_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(GEEL16_TAMTable, vertex.label = V(GEEL16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
GEEL16_TAM.clusterCoef <- transitivity(GEEL16_TAMTable, type="global") #cluster coefficient
GEEL16_TAM.degreeCent <- centralization.degree(GEEL16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_TAMftn <- as.network.matrix(GEEL16_TAMft)
GEEL16_TAM.netDensity <- network.density(GEEL16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_TAM.entropy <- entropy(GEEL16_TAMft) #entropy

GEEL16_TAM.netMx <- cbind(GEEL16_TAM.netMx, GEEL16_TAM.clusterCoef, GEEL16_TAM.degreeCent$centralization,
                          GEEL16_TAM.netDensity, GEEL16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
GEEL16_SDMg2 <- data.frame(GEEL16_SDM)
GEEL16_SDMg2 <- GEEL16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_SDMg2$player1
player2vector <- GEEL16_SDMg2$player2
GEEL16_SDMg3 <- GEEL16_SDMg2
GEEL16_SDMg3$p1inp2vec <- is.element(GEEL16_SDMg3$player1, player2vector)
GEEL16_SDMg3$p2inp1vec <- is.element(GEEL16_SDMg3$player2, player1vector)

addPlayer1 <- GEEL16_SDMg3[ which(GEEL16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_SDMg3[ which(GEEL16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_SDMg2 <- rbind(GEEL16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
GEEL16_SDMft <- ftable(GEEL16_SDMg2$player1, GEEL16_SDMg2$player2)
GEEL16_SDMft2 <- as.matrix(GEEL16_SDMft)
numRows <- nrow(GEEL16_SDMft2)
numCols <- ncol(GEEL16_SDMft2)
GEEL16_SDMft3 <- GEEL16_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL16_SDMTable <- graph.adjacency(GEEL16_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(GEEL16_SDMTable, vertex.label = V(GEEL16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
GEEL16_SDM.clusterCoef <- transitivity(GEEL16_SDMTable, type="global") #cluster coefficient
GEEL16_SDM.degreeCent <- centralization.degree(GEEL16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_SDMftn <- as.network.matrix(GEEL16_SDMft)
GEEL16_SDM.netDensity <- network.density(GEEL16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_SDM.entropy <- entropy(GEEL16_SDMft) #entropy

GEEL16_SDM.netMx <- cbind(GEEL16_SDM.netMx, GEEL16_SDM.clusterCoef, GEEL16_SDM.degreeCent$centralization,
                          GEEL16_SDM.netDensity, GEEL16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
GEEL16_TDMg2 <- data.frame(GEEL16_TDM)
GEEL16_TDMg2 <- GEEL16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_TDMg2$player1
player2vector <- GEEL16_TDMg2$player2
GEEL16_TDMg3 <- GEEL16_TDMg2
GEEL16_TDMg3$p1inp2vec <- is.element(GEEL16_TDMg3$player1, player2vector)
GEEL16_TDMg3$p2inp1vec <- is.element(GEEL16_TDMg3$player2, player1vector)

addPlayer1 <- GEEL16_TDMg3[ which(GEEL16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL16_TDMg3[ which(GEEL16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_TDMg2 <- rbind(GEEL16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
GEEL16_TDMft <- ftable(GEEL16_TDMg2$player1, GEEL16_TDMg2$player2)
GEEL16_TDMft2 <- as.matrix(GEEL16_TDMft)
numRows <- nrow(GEEL16_TDMft2)
numCols <- ncol(GEEL16_TDMft2)
GEEL16_TDMft3 <- GEEL16_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL16_TDMTable <- graph.adjacency(GEEL16_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(GEEL16_TDMTable, vertex.label = V(GEEL16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
GEEL16_TDM.clusterCoef <- transitivity(GEEL16_TDMTable, type="global") #cluster coefficient
GEEL16_TDM.degreeCent <- centralization.degree(GEEL16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_TDMftn <- as.network.matrix(GEEL16_TDMft)
GEEL16_TDM.netDensity <- network.density(GEEL16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_TDM.entropy <- entropy(GEEL16_TDMft) #entropy

GEEL16_TDM.netMx <- cbind(GEEL16_TDM.netMx, GEEL16_TDM.clusterCoef, GEEL16_TDM.degreeCent$centralization,
                          GEEL16_TDM.netDensity, GEEL16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
GEEL16_SDg2 <- data.frame(GEEL16_SD)
GEEL16_SDg2 <- GEEL16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_SDg2$player1
player2vector <- GEEL16_SDg2$player2
GEEL16_SDg3 <- GEEL16_SDg2
GEEL16_SDg3$p1inp2vec <- is.element(GEEL16_SDg3$player1, player2vector)
GEEL16_SDg3$p2inp1vec <- is.element(GEEL16_SDg3$player2, player1vector)

addPlayer1 <- GEEL16_SDg3[ which(GEEL16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_SDg3[ which(GEEL16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_SDg2 <- rbind(GEEL16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
GEEL16_SDft <- ftable(GEEL16_SDg2$player1, GEEL16_SDg2$player2)
GEEL16_SDft2 <- as.matrix(GEEL16_SDft)
numRows <- nrow(GEEL16_SDft2)
numCols <- ncol(GEEL16_SDft2)
GEEL16_SDft3 <- GEEL16_SDft2[c(2:numRows) , c(2:numCols)]
GEEL16_SDTable <- graph.adjacency(GEEL16_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(GEEL16_SDTable, vertex.label = V(GEEL16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
GEEL16_SD.clusterCoef <- transitivity(GEEL16_SDTable, type="global") #cluster coefficient
GEEL16_SD.degreeCent <- centralization.degree(GEEL16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_SDftn <- as.network.matrix(GEEL16_SDft)
GEEL16_SD.netDensity <- network.density(GEEL16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_SD.entropy <- entropy(GEEL16_SDft) #entropy

GEEL16_SD.netMx <- cbind(GEEL16_SD.netMx, GEEL16_SD.clusterCoef, GEEL16_SD.degreeCent$centralization,
                         GEEL16_SD.netDensity, GEEL16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
GEEL16_TDg2 <- data.frame(GEEL16_TD)
GEEL16_TDg2 <- GEEL16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_TDg2$player1
player2vector <- GEEL16_TDg2$player2
GEEL16_TDg3 <- GEEL16_TDg2
GEEL16_TDg3$p1inp2vec <- is.element(GEEL16_TDg3$player1, player2vector)
GEEL16_TDg3$p2inp1vec <- is.element(GEEL16_TDg3$player2, player1vector)

addPlayer1 <- GEEL16_TDg3[ which(GEEL16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_TDg3[ which(GEEL16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_TDg2 <- rbind(GEEL16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
GEEL16_TDft <- ftable(GEEL16_TDg2$player1, GEEL16_TDg2$player2)
GEEL16_TDft2 <- as.matrix(GEEL16_TDft)
numRows <- nrow(GEEL16_TDft2)
numCols <- ncol(GEEL16_TDft2)
GEEL16_TDft3 <- GEEL16_TDft2[c(2:numRows) , c(2:numCols)]
GEEL16_TDTable <- graph.adjacency(GEEL16_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(GEEL16_TDTable, vertex.label = V(GEEL16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
GEEL16_TD.clusterCoef <- transitivity(GEEL16_TDTable, type="global") #cluster coefficient
GEEL16_TD.degreeCent <- centralization.degree(GEEL16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_TDftn <- as.network.matrix(GEEL16_TDft)
GEEL16_TD.netDensity <- network.density(GEEL16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_TD.entropy <- entropy(GEEL16_TDft) #entropy

GEEL16_TD.netMx <- cbind(GEEL16_TD.netMx, GEEL16_TD.clusterCoef, GEEL16_TD.degreeCent$centralization,
                         GEEL16_TD.netDensity, GEEL16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
GEEL16_QTg2 <- data.frame(GEEL16_QT)
GEEL16_QTg2 <- GEEL16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL16_QTg2$player1
player2vector <- GEEL16_QTg2$player2
GEEL16_QTg3 <- GEEL16_QTg2
GEEL16_QTg3$p1inp2vec <- is.element(GEEL16_QTg3$player1, player2vector)
GEEL16_QTg3$p2inp1vec <- is.element(GEEL16_QTg3$player2, player1vector)

addPlayer1 <- GEEL16_QTg3[ which(GEEL16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL16_QTg3[ which(GEEL16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL16_QTg2 <- rbind(GEEL16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
GEEL16_QTft <- ftable(GEEL16_QTg2$player1, GEEL16_QTg2$player2)
GEEL16_QTft2 <- as.matrix(GEEL16_QTft)
numRows <- nrow(GEEL16_QTft2)
numCols <- ncol(GEEL16_QTft2)
GEEL16_QTft3 <- GEEL16_QTft2[c(2:numRows) , c(2:numCols)]
GEEL16_QTTable <- graph.adjacency(GEEL16_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(GEEL16_QTTable, vertex.label = V(GEEL16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
GEEL16_QT.clusterCoef <- transitivity(GEEL16_QTTable, type="global") #cluster coefficient
GEEL16_QT.degreeCent <- centralization.degree(GEEL16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL16_QTftn <- as.network.matrix(GEEL16_QTft)
GEEL16_QT.netDensity <- network.density(GEEL16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL16_QT.entropy <- entropy(GEEL16_QTft) #entropy

GEEL16_QT.netMx <- cbind(GEEL16_QT.netMx, GEEL16_QT.clusterCoef, GEEL16_QT.degreeCent$centralization,
                         GEEL16_QT.netDensity, GEEL16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL16_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "GWS"
KIoutcome = "Goal_F"
GWS16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
GWS16_Gg2 <- data.frame(GWS16_G)
GWS16_Gg2 <- GWS16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_Gg2$player1
player2vector <- GWS16_Gg2$player2
GWS16_Gg3 <- GWS16_Gg2
GWS16_Gg3$p1inp2vec <- is.element(GWS16_Gg3$player1, player2vector)
GWS16_Gg3$p2inp1vec <- is.element(GWS16_Gg3$player2, player1vector)

addPlayer1 <- GWS16_Gg3[ which(GWS16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_Gg3[ which(GWS16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_Gg2 <- rbind(GWS16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
GWS16_Gft <- ftable(GWS16_Gg2$player1, GWS16_Gg2$player2)
GWS16_Gft2 <- as.matrix(GWS16_Gft)
numRows <- nrow(GWS16_Gft2)
numCols <- ncol(GWS16_Gft2)
GWS16_Gft3 <- GWS16_Gft2[c(1:numRows) , c(1:numCols)]
GWS16_GTable <- graph.adjacency(GWS16_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(GWS16_GTable, vertex.label = V(GWS16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
GWS16_G.clusterCoef <- transitivity(GWS16_GTable, type="global") #cluster coefficient
GWS16_G.degreeCent <- centralization.degree(GWS16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_Gftn <- as.network.matrix(GWS16_Gft)
GWS16_G.netDensity <- network.density(GWS16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_G.entropy <- entropy(GWS16_Gft) #entropy

GWS16_G.netMx <- cbind(GWS16_G.netMx, GWS16_G.clusterCoef, GWS16_G.degreeCent$centralization,
                       GWS16_G.netDensity, GWS16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "GWS"
KIoutcome = "Behind_F"
GWS16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
GWS16_Bg2 <- data.frame(GWS16_B)
GWS16_Bg2 <- GWS16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_Bg2$player1
player2vector <- GWS16_Bg2$player2
GWS16_Bg3 <- GWS16_Bg2
GWS16_Bg3$p1inp2vec <- is.element(GWS16_Bg3$player1, player2vector)
GWS16_Bg3$p2inp1vec <- is.element(GWS16_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS16_Bg3[ which(GWS16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS16_Bg2 <- rbind(GWS16_Bg2, addPlayer2)

#ROUND 16, Behind graph using weighted edges
GWS16_Bft <- ftable(GWS16_Bg2$player1, GWS16_Bg2$player2)
GWS16_Bft2 <- as.matrix(GWS16_Bft)
numRows <- nrow(GWS16_Bft2)
numCols <- ncol(GWS16_Bft2)
GWS16_Bft3 <- GWS16_Bft2[c(1:numRows) , c(2:numCols)]
GWS16_BTable <- graph.adjacency(GWS16_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(GWS16_BTable, vertex.label = V(GWS16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
GWS16_B.clusterCoef <- transitivity(GWS16_BTable, type="global") #cluster coefficient
GWS16_B.degreeCent <- centralization.degree(GWS16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_Bftn <- as.network.matrix(GWS16_Bft)
GWS16_B.netDensity <- network.density(GWS16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_B.entropy <- entropy(GWS16_Bft) #entropy

GWS16_B.netMx <- cbind(GWS16_B.netMx, GWS16_B.clusterCoef, GWS16_B.degreeCent$centralization,
                       GWS16_B.netDensity, GWS16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************

round = 16
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
GWS16_SFg2 <- data.frame(GWS16_SF)
GWS16_SFg2 <- GWS16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_SFg2$player1
player2vector <- GWS16_SFg2$player2
GWS16_SFg3 <- GWS16_SFg2
GWS16_SFg3$p1inp2vec <- is.element(GWS16_SFg3$player1, player2vector)
GWS16_SFg3$p2inp1vec <- is.element(GWS16_SFg3$player2, player1vector)

addPlayer1 <- GWS16_SFg3[ which(GWS16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_SFg3[ which(GWS16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_SFg2 <- rbind(GWS16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
GWS16_SFft <- ftable(GWS16_SFg2$player1, GWS16_SFg2$player2)
GWS16_SFft2 <- as.matrix(GWS16_SFft)
numRows <- nrow(GWS16_SFft2)
numCols <- ncol(GWS16_SFft2)
GWS16_SFft3 <- GWS16_SFft2[c(2:numRows) , c(2:numCols)]
GWS16_SFTable <- graph.adjacency(GWS16_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(GWS16_SFTable, vertex.label = V(GWS16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
GWS16_SF.clusterCoef <- transitivity(GWS16_SFTable, type="global") #cluster coefficient
GWS16_SF.degreeCent <- centralization.degree(GWS16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_SFftn <- as.network.matrix(GWS16_SFft)
GWS16_SF.netDensity <- network.density(GWS16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_SF.entropy <- entropy(GWS16_SFft) #entropy

GWS16_SF.netMx <- cbind(GWS16_SF.netMx, GWS16_SF.clusterCoef, GWS16_SF.degreeCent$centralization,
                        GWS16_SF.netDensity, GWS16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************
#NA

round = 16
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
GWS16_TFg2 <- data.frame(GWS16_TF)
GWS16_TFg2 <- GWS16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_TFg2$player1
player2vector <- GWS16_TFg2$player2
GWS16_TFg3 <- GWS16_TFg2
GWS16_TFg3$p1inp2vec <- is.element(GWS16_TFg3$player1, player2vector)
GWS16_TFg3$p2inp1vec <- is.element(GWS16_TFg3$player2, player1vector)

addPlayer1 <- GWS16_TFg3[ which(GWS16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_TFg3[ which(GWS16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_TFg2 <- rbind(GWS16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
GWS16_TFft <- ftable(GWS16_TFg2$player1, GWS16_TFg2$player2)
GWS16_TFft2 <- as.matrix(GWS16_TFft)
numRows <- nrow(GWS16_TFft2)
numCols <- ncol(GWS16_TFft2)
GWS16_TFft3 <- GWS16_TFft2[c(2:numRows) , c(2:numCols)]
GWS16_TFTable <- graph.adjacency(GWS16_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(GWS16_TFTable, vertex.label = V(GWS16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
GWS16_TF.clusterCoef <- transitivity(GWS16_TFTable, type="global") #cluster coefficient
GWS16_TF.degreeCent <- centralization.degree(GWS16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_TFftn <- as.network.matrix(GWS16_TFft)
GWS16_TF.netDensity <- network.density(GWS16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_TF.entropy <- entropy(GWS16_TFft) #entropy

GWS16_TF.netMx <- cbind(GWS16_TF.netMx, GWS16_TF.clusterCoef, GWS16_TF.degreeCent$centralization,
                        GWS16_TF.netDensity, GWS16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************

round = 16
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
GWS16_SAMg2 <- data.frame(GWS16_SAM)
GWS16_SAMg2 <- GWS16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_SAMg2$player1
player2vector <- GWS16_SAMg2$player2
GWS16_SAMg3 <- GWS16_SAMg2
GWS16_SAMg3$p1inp2vec <- is.element(GWS16_SAMg3$player1, player2vector)
GWS16_SAMg3$p2inp1vec <- is.element(GWS16_SAMg3$player2, player1vector)

addPlayer1 <- GWS16_SAMg3[ which(GWS16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_SAMg3[ which(GWS16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_SAMg2 <- rbind(GWS16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
GWS16_SAMft <- ftable(GWS16_SAMg2$player1, GWS16_SAMg2$player2)
GWS16_SAMft2 <- as.matrix(GWS16_SAMft)
numRows <- nrow(GWS16_SAMft2)
numCols <- ncol(GWS16_SAMft2)
GWS16_SAMft3 <- GWS16_SAMft2[c(2:numRows) , c(2:numCols)]
GWS16_SAMTable <- graph.adjacency(GWS16_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(GWS16_SAMTable, vertex.label = V(GWS16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
GWS16_SAM.clusterCoef <- transitivity(GWS16_SAMTable, type="global") #cluster coefficient
GWS16_SAM.degreeCent <- centralization.degree(GWS16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_SAMftn <- as.network.matrix(GWS16_SAMft)
GWS16_SAM.netDensity <- network.density(GWS16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_SAM.entropy <- entropy(GWS16_SAMft) #entropy

GWS16_SAM.netMx <- cbind(GWS16_SAM.netMx, GWS16_SAM.clusterCoef, GWS16_SAM.degreeCent$centralization,
                         GWS16_SAM.netDensity, GWS16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
GWS16_TAMg2 <- data.frame(GWS16_TAM)
GWS16_TAMg2 <- GWS16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_TAMg2$player1
player2vector <- GWS16_TAMg2$player2
GWS16_TAMg3 <- GWS16_TAMg2
GWS16_TAMg3$p1inp2vec <- is.element(GWS16_TAMg3$player1, player2vector)
GWS16_TAMg3$p2inp1vec <- is.element(GWS16_TAMg3$player2, player1vector)

addPlayer1 <- GWS16_TAMg3[ which(GWS16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_TAMg3[ which(GWS16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_TAMg2 <- rbind(GWS16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
GWS16_TAMft <- ftable(GWS16_TAMg2$player1, GWS16_TAMg2$player2)
GWS16_TAMft2 <- as.matrix(GWS16_TAMft)
numRows <- nrow(GWS16_TAMft2)
numCols <- ncol(GWS16_TAMft2)
GWS16_TAMft3 <- GWS16_TAMft2[c(2:numRows) , c(2:numCols)]
GWS16_TAMTable <- graph.adjacency(GWS16_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(GWS16_TAMTable, vertex.label = V(GWS16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
GWS16_TAM.clusterCoef <- transitivity(GWS16_TAMTable, type="global") #cluster coefficient
GWS16_TAM.degreeCent <- centralization.degree(GWS16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_TAMftn <- as.network.matrix(GWS16_TAMft)
GWS16_TAM.netDensity <- network.density(GWS16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_TAM.entropy <- entropy(GWS16_TAMft) #entropy

GWS16_TAM.netMx <- cbind(GWS16_TAM.netMx, GWS16_TAM.clusterCoef, GWS16_TAM.degreeCent$centralization,
                         GWS16_TAM.netDensity, GWS16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
GWS16_SDMg2 <- data.frame(GWS16_SDM)
GWS16_SDMg2 <- GWS16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_SDMg2$player1
player2vector <- GWS16_SDMg2$player2
GWS16_SDMg3 <- GWS16_SDMg2
GWS16_SDMg3$p1inp2vec <- is.element(GWS16_SDMg3$player1, player2vector)
GWS16_SDMg3$p2inp1vec <- is.element(GWS16_SDMg3$player2, player1vector)

addPlayer1 <- GWS16_SDMg3[ which(GWS16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_SDMg3[ which(GWS16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_SDMg2 <- rbind(GWS16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
GWS16_SDMft <- ftable(GWS16_SDMg2$player1, GWS16_SDMg2$player2)
GWS16_SDMft2 <- as.matrix(GWS16_SDMft)
numRows <- nrow(GWS16_SDMft2)
numCols <- ncol(GWS16_SDMft2)
GWS16_SDMft3 <- GWS16_SDMft2[c(2:numRows) , c(2:numCols)]
GWS16_SDMTable <- graph.adjacency(GWS16_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(GWS16_SDMTable, vertex.label = V(GWS16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
GWS16_SDM.clusterCoef <- transitivity(GWS16_SDMTable, type="global") #cluster coefficient
GWS16_SDM.degreeCent <- centralization.degree(GWS16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_SDMftn <- as.network.matrix(GWS16_SDMft)
GWS16_SDM.netDensity <- network.density(GWS16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_SDM.entropy <- entropy(GWS16_SDMft) #entropy

GWS16_SDM.netMx <- cbind(GWS16_SDM.netMx, GWS16_SDM.clusterCoef, GWS16_SDM.degreeCent$centralization,
                         GWS16_SDM.netDensity, GWS16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************
#NA

round = 16
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
GWS16_TDMg2 <- data.frame(GWS16_TDM)
GWS16_TDMg2 <- GWS16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_TDMg2$player1
player2vector <- GWS16_TDMg2$player2
GWS16_TDMg3 <- GWS16_TDMg2
GWS16_TDMg3$p1inp2vec <- is.element(GWS16_TDMg3$player1, player2vector)
GWS16_TDMg3$p2inp1vec <- is.element(GWS16_TDMg3$player2, player1vector)

addPlayer1 <- GWS16_TDMg3[ which(GWS16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_TDMg3[ which(GWS16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_TDMg2 <- rbind(GWS16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
GWS16_TDMft <- ftable(GWS16_TDMg2$player1, GWS16_TDMg2$player2)
GWS16_TDMft2 <- as.matrix(GWS16_TDMft)
numRows <- nrow(GWS16_TDMft2)
numCols <- ncol(GWS16_TDMft2)
GWS16_TDMft3 <- GWS16_TDMft2[c(2:numRows) , c(2:numCols)]
GWS16_TDMTable <- graph.adjacency(GWS16_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(GWS16_TDMTable, vertex.label = V(GWS16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
GWS16_TDM.clusterCoef <- transitivity(GWS16_TDMTable, type="global") #cluster coefficient
GWS16_TDM.degreeCent <- centralization.degree(GWS16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_TDMftn <- as.network.matrix(GWS16_TDMft)
GWS16_TDM.netDensity <- network.density(GWS16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_TDM.entropy <- entropy(GWS16_TDMft) #entropy

GWS16_TDM.netMx <- cbind(GWS16_TDM.netMx, GWS16_TDM.clusterCoef, GWS16_TDM.degreeCent$centralization,
                         GWS16_TDM.netDensity, GWS16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
GWS16_SDg2 <- data.frame(GWS16_SD)
GWS16_SDg2 <- GWS16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_SDg2$player1
player2vector <- GWS16_SDg2$player2
GWS16_SDg3 <- GWS16_SDg2
GWS16_SDg3$p1inp2vec <- is.element(GWS16_SDg3$player1, player2vector)
GWS16_SDg3$p2inp1vec <- is.element(GWS16_SDg3$player2, player1vector)

addPlayer1 <- GWS16_SDg3[ which(GWS16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_SDg3[ which(GWS16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_SDg2 <- rbind(GWS16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
GWS16_SDft <- ftable(GWS16_SDg2$player1, GWS16_SDg2$player2)
GWS16_SDft2 <- as.matrix(GWS16_SDft)
numRows <- nrow(GWS16_SDft2)
numCols <- ncol(GWS16_SDft2)
GWS16_SDft3 <- GWS16_SDft2[c(2:numRows) , c(2:numCols)]
GWS16_SDTable <- graph.adjacency(GWS16_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(GWS16_SDTable, vertex.label = V(GWS16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
GWS16_SD.clusterCoef <- transitivity(GWS16_SDTable, type="global") #cluster coefficient
GWS16_SD.degreeCent <- centralization.degree(GWS16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_SDftn <- as.network.matrix(GWS16_SDft)
GWS16_SD.netDensity <- network.density(GWS16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_SD.entropy <- entropy(GWS16_SDft) #entropy

GWS16_SD.netMx <- cbind(GWS16_SD.netMx, GWS16_SD.clusterCoef, GWS16_SD.degreeCent$centralization,
                        GWS16_SD.netDensity, GWS16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
GWS16_TDg2 <- data.frame(GWS16_TD)
GWS16_TDg2 <- GWS16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_TDg2$player1
player2vector <- GWS16_TDg2$player2
GWS16_TDg3 <- GWS16_TDg2
GWS16_TDg3$p1inp2vec <- is.element(GWS16_TDg3$player1, player2vector)
GWS16_TDg3$p2inp1vec <- is.element(GWS16_TDg3$player2, player1vector)

addPlayer1 <- GWS16_TDg3[ which(GWS16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_TDg3[ which(GWS16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_TDg2 <- rbind(GWS16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
GWS16_TDft <- ftable(GWS16_TDg2$player1, GWS16_TDg2$player2)
GWS16_TDft2 <- as.matrix(GWS16_TDft)
numRows <- nrow(GWS16_TDft2)
numCols <- ncol(GWS16_TDft2)
GWS16_TDft3 <- GWS16_TDft2[c(2:numRows) , c(2:numCols)]
GWS16_TDTable <- graph.adjacency(GWS16_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(GWS16_TDTable, vertex.label = V(GWS16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
GWS16_TD.clusterCoef <- transitivity(GWS16_TDTable, type="global") #cluster coefficient
GWS16_TD.degreeCent <- centralization.degree(GWS16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_TDftn <- as.network.matrix(GWS16_TDft)
GWS16_TD.netDensity <- network.density(GWS16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_TD.entropy <- entropy(GWS16_TDft) #entropy

GWS16_TD.netMx <- cbind(GWS16_TD.netMx, GWS16_TD.clusterCoef, GWS16_TD.degreeCent$centralization,
                        GWS16_TD.netDensity, GWS16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
GWS16_QTg2 <- data.frame(GWS16_QT)
GWS16_QTg2 <- GWS16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS16_QTg2$player1
player2vector <- GWS16_QTg2$player2
GWS16_QTg3 <- GWS16_QTg2
GWS16_QTg3$p1inp2vec <- is.element(GWS16_QTg3$player1, player2vector)
GWS16_QTg3$p2inp1vec <- is.element(GWS16_QTg3$player2, player1vector)

addPlayer1 <- GWS16_QTg3[ which(GWS16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS16_QTg3[ which(GWS16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS16_QTg2 <- rbind(GWS16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
GWS16_QTft <- ftable(GWS16_QTg2$player1, GWS16_QTg2$player2)
GWS16_QTft2 <- as.matrix(GWS16_QTft)
numRows <- nrow(GWS16_QTft2)
numCols <- ncol(GWS16_QTft2)
GWS16_QTft3 <- GWS16_QTft2[c(2:numRows) , c(2:numCols)]
GWS16_QTTable <- graph.adjacency(GWS16_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(GWS16_QTTable, vertex.label = V(GWS16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
GWS16_QT.clusterCoef <- transitivity(GWS16_QTTable, type="global") #cluster coefficient
GWS16_QT.degreeCent <- centralization.degree(GWS16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS16_QTftn <- as.network.matrix(GWS16_QTft)
GWS16_QT.netDensity <- network.density(GWS16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS16_QT.entropy <- entropy(GWS16_QTft) #entropy

GWS16_QT.netMx <- cbind(GWS16_QT.netMx, GWS16_QT.clusterCoef, GWS16_QT.degreeCent$centralization,
                        GWS16_QT.netDensity, GWS16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS16_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************

round = 16
teamName = "HAW"
KIoutcome = "Goal_F"
HAW16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
HAW16_Gg2 <- data.frame(HAW16_G)
HAW16_Gg2 <- HAW16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_Gg2$player1
player2vector <- HAW16_Gg2$player2
HAW16_Gg3 <- HAW16_Gg2
HAW16_Gg3$p1inp2vec <- is.element(HAW16_Gg3$player1, player2vector)
HAW16_Gg3$p2inp1vec <- is.element(HAW16_Gg3$player2, player1vector)

addPlayer1 <- HAW16_Gg3[ which(HAW16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_Gg3[ which(HAW16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_Gg2 <- rbind(HAW16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
HAW16_Gft <- ftable(HAW16_Gg2$player1, HAW16_Gg2$player2)
HAW16_Gft2 <- as.matrix(HAW16_Gft)
numRows <- nrow(HAW16_Gft2)
numCols <- ncol(HAW16_Gft2)
HAW16_Gft3 <- HAW16_Gft2[c(2:numRows) , c(2:numCols)]
HAW16_GTable <- graph.adjacency(HAW16_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(HAW16_GTable, vertex.label = V(HAW16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
HAW16_G.clusterCoef <- transitivity(HAW16_GTable, type="global") #cluster coefficient
HAW16_G.degreeCent <- centralization.degree(HAW16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_Gftn <- as.network.matrix(HAW16_Gft)
HAW16_G.netDensity <- network.density(HAW16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_G.entropy <- entropy(HAW16_Gft) #entropy

HAW16_G.netMx <- cbind(HAW16_G.netMx, HAW16_G.clusterCoef, HAW16_G.degreeCent$centralization,
                       HAW16_G.netDensity, HAW16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA 

round = 16
teamName = "HAW"
KIoutcome = "Behind_F"
HAW16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
HAW16_Bg2 <- data.frame(HAW16_B)
HAW16_Bg2 <- HAW16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_Bg2$player1
player2vector <- HAW16_Bg2$player2
HAW16_Bg3 <- HAW16_Bg2
HAW16_Bg3$p1inp2vec <- is.element(HAW16_Bg3$player1, player2vector)
HAW16_Bg3$p2inp1vec <- is.element(HAW16_Bg3$player2, player1vector)

addPlayer1 <- HAW16_Bg3[ which(HAW16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW16_Bg2 <- rbind(HAW16_Bg2, addPlayer1)

#ROUND 16, Behind graph using weighted edges
HAW16_Bft <- ftable(HAW16_Bg2$player1, HAW16_Bg2$player2)
HAW16_Bft2 <- as.matrix(HAW16_Bft)
numRows <- nrow(HAW16_Bft2)
numCols <- ncol(HAW16_Bft2)
HAW16_Bft3 <- HAW16_Bft2[c(2:numRows) , c(1:numCols)]
HAW16_BTable <- graph.adjacency(HAW16_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(HAW16_BTable, vertex.label = V(HAW16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
HAW16_B.clusterCoef <- transitivity(HAW16_BTable, type="global") #cluster coefficient
HAW16_B.degreeCent <- centralization.degree(HAW16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_Bftn <- as.network.matrix(HAW16_Bft)
HAW16_B.netDensity <- network.density(HAW16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_B.entropy <- entropy(HAW16_Bft) #entropy

HAW16_B.netMx <- cbind(HAW16_B.netMx, HAW16_B.clusterCoef, HAW16_B.degreeCent$centralization,
                       HAW16_B.netDensity, HAW16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
HAW16_SFg2 <- data.frame(HAW16_SF)
HAW16_SFg2 <- HAW16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_SFg2$player1
player2vector <- HAW16_SFg2$player2
HAW16_SFg3 <- HAW16_SFg2
HAW16_SFg3$p1inp2vec <- is.element(HAW16_SFg3$player1, player2vector)
HAW16_SFg3$p2inp1vec <- is.element(HAW16_SFg3$player2, player1vector)

addPlayer1 <- HAW16_SFg3[ which(HAW16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_SFg3[ which(HAW16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_SFg2 <- rbind(HAW16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
HAW16_SFft <- ftable(HAW16_SFg2$player1, HAW16_SFg2$player2)
HAW16_SFft2 <- as.matrix(HAW16_SFft)
numRows <- nrow(HAW16_SFft2)
numCols <- ncol(HAW16_SFft2)
HAW16_SFft3 <- HAW16_SFft2[c(2:numRows) , c(2:numCols)]
HAW16_SFTable <- graph.adjacency(HAW16_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(HAW16_SFTable, vertex.label = V(HAW16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
HAW16_SF.clusterCoef <- transitivity(HAW16_SFTable, type="global") #cluster coefficient
HAW16_SF.degreeCent <- centralization.degree(HAW16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_SFftn <- as.network.matrix(HAW16_SFft)
HAW16_SF.netDensity <- network.density(HAW16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_SF.entropy <- entropy(HAW16_SFft) #entropy

HAW16_SF.netMx <- cbind(HAW16_SF.netMx, HAW16_SF.clusterCoef, HAW16_SF.degreeCent$centralization,
                        HAW16_SF.netDensity, HAW16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************
#NA

round = 16
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
HAW16_TFg2 <- data.frame(HAW16_TF)
HAW16_TFg2 <- HAW16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_TFg2$player1
player2vector <- HAW16_TFg2$player2
HAW16_TFg3 <- HAW16_TFg2
HAW16_TFg3$p1inp2vec <- is.element(HAW16_TFg3$player1, player2vector)
HAW16_TFg3$p2inp1vec <- is.element(HAW16_TFg3$player2, player1vector)

addPlayer1 <- HAW16_TFg3[ which(HAW16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_TFg3[ which(HAW16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_TFg2 <- rbind(HAW16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
HAW16_TFft <- ftable(HAW16_TFg2$player1, HAW16_TFg2$player2)
HAW16_TFft2 <- as.matrix(HAW16_TFft)
numRows <- nrow(HAW16_TFft2)
numCols <- ncol(HAW16_TFft2)
HAW16_TFft3 <- HAW16_TFft2[c(2:numRows) , c(2:numCols)]
HAW16_TFTable <- graph.adjacency(HAW16_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(HAW16_TFTable, vertex.label = V(HAW16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
HAW16_TF.clusterCoef <- transitivity(HAW16_TFTable, type="global") #cluster coefficient
HAW16_TF.degreeCent <- centralization.degree(HAW16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_TFftn <- as.network.matrix(HAW16_TFft)
HAW16_TF.netDensity <- network.density(HAW16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_TF.entropy <- entropy(HAW16_TFft) #entropy

HAW16_TF.netMx <- cbind(HAW16_TF.netMx, HAW16_TF.clusterCoef, HAW16_TF.degreeCent$centralization,
                        HAW16_TF.netDensity, HAW16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
HAW16_SAMg2 <- data.frame(HAW16_SAM)
HAW16_SAMg2 <- HAW16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_SAMg2$player1
player2vector <- HAW16_SAMg2$player2
HAW16_SAMg3 <- HAW16_SAMg2
HAW16_SAMg3$p1inp2vec <- is.element(HAW16_SAMg3$player1, player2vector)
HAW16_SAMg3$p2inp1vec <- is.element(HAW16_SAMg3$player2, player1vector)

addPlayer1 <- HAW16_SAMg3[ which(HAW16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_SAMg3[ which(HAW16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_SAMg2 <- rbind(HAW16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
HAW16_SAMft <- ftable(HAW16_SAMg2$player1, HAW16_SAMg2$player2)
HAW16_SAMft2 <- as.matrix(HAW16_SAMft)
numRows <- nrow(HAW16_SAMft2)
numCols <- ncol(HAW16_SAMft2)
HAW16_SAMft3 <- HAW16_SAMft2[c(2:numRows) , c(2:numCols)]
HAW16_SAMTable <- graph.adjacency(HAW16_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(HAW16_SAMTable, vertex.label = V(HAW16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
HAW16_SAM.clusterCoef <- transitivity(HAW16_SAMTable, type="global") #cluster coefficient
HAW16_SAM.degreeCent <- centralization.degree(HAW16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_SAMftn <- as.network.matrix(HAW16_SAMft)
HAW16_SAM.netDensity <- network.density(HAW16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_SAM.entropy <- entropy(HAW16_SAMft) #entropy

HAW16_SAM.netMx <- cbind(HAW16_SAM.netMx, HAW16_SAM.clusterCoef, HAW16_SAM.degreeCent$centralization,
                         HAW16_SAM.netDensity, HAW16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
HAW16_TAMg2 <- data.frame(HAW16_TAM)
HAW16_TAMg2 <- HAW16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_TAMg2$player1
player2vector <- HAW16_TAMg2$player2
HAW16_TAMg3 <- HAW16_TAMg2
HAW16_TAMg3$p1inp2vec <- is.element(HAW16_TAMg3$player1, player2vector)
HAW16_TAMg3$p2inp1vec <- is.element(HAW16_TAMg3$player2, player1vector)

addPlayer1 <- HAW16_TAMg3[ which(HAW16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_TAMg3[ which(HAW16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_TAMg2 <- rbind(HAW16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
HAW16_TAMft <- ftable(HAW16_TAMg2$player1, HAW16_TAMg2$player2)
HAW16_TAMft2 <- as.matrix(HAW16_TAMft)
numRows <- nrow(HAW16_TAMft2)
numCols <- ncol(HAW16_TAMft2)
HAW16_TAMft3 <- HAW16_TAMft2[c(2:numRows) , c(2:numCols)]
HAW16_TAMTable <- graph.adjacency(HAW16_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(HAW16_TAMTable, vertex.label = V(HAW16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
HAW16_TAM.clusterCoef <- transitivity(HAW16_TAMTable, type="global") #cluster coefficient
HAW16_TAM.degreeCent <- centralization.degree(HAW16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_TAMftn <- as.network.matrix(HAW16_TAMft)
HAW16_TAM.netDensity <- network.density(HAW16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_TAM.entropy <- entropy(HAW16_TAMft) #entropy

HAW16_TAM.netMx <- cbind(HAW16_TAM.netMx, HAW16_TAM.clusterCoef, HAW16_TAM.degreeCent$centralization,
                         HAW16_TAM.netDensity, HAW16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************
#NA

round = 16
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
HAW16_SDMg2 <- data.frame(HAW16_SDM)
HAW16_SDMg2 <- HAW16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_SDMg2$player1
player2vector <- HAW16_SDMg2$player2
HAW16_SDMg3 <- HAW16_SDMg2
HAW16_SDMg3$p1inp2vec <- is.element(HAW16_SDMg3$player1, player2vector)
HAW16_SDMg3$p2inp1vec <- is.element(HAW16_SDMg3$player2, player1vector)

addPlayer1 <- HAW16_SDMg3[ which(HAW16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_SDMg3[ which(HAW16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_SDMg2 <- rbind(HAW16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
HAW16_SDMft <- ftable(HAW16_SDMg2$player1, HAW16_SDMg2$player2)
HAW16_SDMft2 <- as.matrix(HAW16_SDMft)
numRows <- nrow(HAW16_SDMft2)
numCols <- ncol(HAW16_SDMft2)
HAW16_SDMft3 <- HAW16_SDMft2[c(2:numRows) , c(2:numCols)]
HAW16_SDMTable <- graph.adjacency(HAW16_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(HAW16_SDMTable, vertex.label = V(HAW16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
HAW16_SDM.clusterCoef <- transitivity(HAW16_SDMTable, type="global") #cluster coefficient
HAW16_SDM.degreeCent <- centralization.degree(HAW16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_SDMftn <- as.network.matrix(HAW16_SDMft)
HAW16_SDM.netDensity <- network.density(HAW16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_SDM.entropy <- entropy(HAW16_SDMft) #entropy

HAW16_SDM.netMx <- cbind(HAW16_SDM.netMx, HAW16_SDM.clusterCoef, HAW16_SDM.degreeCent$centralization,
                         HAW16_SDM.netDensity, HAW16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
HAW16_TDMg2 <- data.frame(HAW16_TDM)
HAW16_TDMg2 <- HAW16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_TDMg2$player1
player2vector <- HAW16_TDMg2$player2
HAW16_TDMg3 <- HAW16_TDMg2
HAW16_TDMg3$p1inp2vec <- is.element(HAW16_TDMg3$player1, player2vector)
HAW16_TDMg3$p2inp1vec <- is.element(HAW16_TDMg3$player2, player1vector)

addPlayer1 <- HAW16_TDMg3[ which(HAW16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_TDMg3[ which(HAW16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_TDMg2 <- rbind(HAW16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
HAW16_TDMft <- ftable(HAW16_TDMg2$player1, HAW16_TDMg2$player2)
HAW16_TDMft2 <- as.matrix(HAW16_TDMft)
numRows <- nrow(HAW16_TDMft2)
numCols <- ncol(HAW16_TDMft2)
HAW16_TDMft3 <- HAW16_TDMft2[c(2:numRows) , c(2:numCols)]
HAW16_TDMTable <- graph.adjacency(HAW16_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(HAW16_TDMTable, vertex.label = V(HAW16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
HAW16_TDM.clusterCoef <- transitivity(HAW16_TDMTable, type="global") #cluster coefficient
HAW16_TDM.degreeCent <- centralization.degree(HAW16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_TDMftn <- as.network.matrix(HAW16_TDMft)
HAW16_TDM.netDensity <- network.density(HAW16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_TDM.entropy <- entropy(HAW16_TDMft) #entropy

HAW16_TDM.netMx <- cbind(HAW16_TDM.netMx, HAW16_TDM.clusterCoef, HAW16_TDM.degreeCent$centralization,
                         HAW16_TDM.netDensity, HAW16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
HAW16_SDg2 <- data.frame(HAW16_SD)
HAW16_SDg2 <- HAW16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_SDg2$player1
player2vector <- HAW16_SDg2$player2
HAW16_SDg3 <- HAW16_SDg2
HAW16_SDg3$p1inp2vec <- is.element(HAW16_SDg3$player1, player2vector)
HAW16_SDg3$p2inp1vec <- is.element(HAW16_SDg3$player2, player1vector)

addPlayer1 <- HAW16_SDg3[ which(HAW16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_SDg3[ which(HAW16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_SDg2 <- rbind(HAW16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
HAW16_SDft <- ftable(HAW16_SDg2$player1, HAW16_SDg2$player2)
HAW16_SDft2 <- as.matrix(HAW16_SDft)
numRows <- nrow(HAW16_SDft2)
numCols <- ncol(HAW16_SDft2)
HAW16_SDft3 <- HAW16_SDft2[c(2:numRows) , c(2:numCols)]
HAW16_SDTable <- graph.adjacency(HAW16_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(HAW16_SDTable, vertex.label = V(HAW16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
HAW16_SD.clusterCoef <- transitivity(HAW16_SDTable, type="global") #cluster coefficient
HAW16_SD.degreeCent <- centralization.degree(HAW16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_SDftn <- as.network.matrix(HAW16_SDft)
HAW16_SD.netDensity <- network.density(HAW16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_SD.entropy <- entropy(HAW16_SDft) #entropy

HAW16_SD.netMx <- cbind(HAW16_SD.netMx, HAW16_SD.clusterCoef, HAW16_SD.degreeCent$centralization,
                        HAW16_SD.netDensity, HAW16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
HAW16_TDg2 <- data.frame(HAW16_TD)
HAW16_TDg2 <- HAW16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_TDg2$player1
player2vector <- HAW16_TDg2$player2
HAW16_TDg3 <- HAW16_TDg2
HAW16_TDg3$p1inp2vec <- is.element(HAW16_TDg3$player1, player2vector)
HAW16_TDg3$p2inp1vec <- is.element(HAW16_TDg3$player2, player1vector)

addPlayer1 <- HAW16_TDg3[ which(HAW16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_TDg3[ which(HAW16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_TDg2 <- rbind(HAW16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
HAW16_TDft <- ftable(HAW16_TDg2$player1, HAW16_TDg2$player2)
HAW16_TDft2 <- as.matrix(HAW16_TDft)
numRows <- nrow(HAW16_TDft2)
numCols <- ncol(HAW16_TDft2)
HAW16_TDft3 <- HAW16_TDft2[c(2:numRows) , c(2:numCols)]
HAW16_TDTable <- graph.adjacency(HAW16_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(HAW16_TDTable, vertex.label = V(HAW16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
HAW16_TD.clusterCoef <- transitivity(HAW16_TDTable, type="global") #cluster coefficient
HAW16_TD.degreeCent <- centralization.degree(HAW16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_TDftn <- as.network.matrix(HAW16_TDft)
HAW16_TD.netDensity <- network.density(HAW16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_TD.entropy <- entropy(HAW16_TDft) #entropy

HAW16_TD.netMx <- cbind(HAW16_TD.netMx, HAW16_TD.clusterCoef, HAW16_TD.degreeCent$centralization,
                        HAW16_TD.netDensity, HAW16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
HAW16_QTg2 <- data.frame(HAW16_QT)
HAW16_QTg2 <- HAW16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_QTg2$player1
player2vector <- HAW16_QTg2$player2
HAW16_QTg3 <- HAW16_QTg2
HAW16_QTg3$p1inp2vec <- is.element(HAW16_QTg3$player1, player2vector)
HAW16_QTg3$p2inp1vec <- is.element(HAW16_QTg3$player2, player1vector)

addPlayer1 <- HAW16_QTg3[ which(HAW16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_QTg3[ which(HAW16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_QTg2 <- rbind(HAW16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
HAW16_QTft <- ftable(HAW16_QTg2$player1, HAW16_QTg2$player2)
HAW16_QTft2 <- as.matrix(HAW16_QTft)
numRows <- nrow(HAW16_QTft2)
numCols <- ncol(HAW16_QTft2)
HAW16_QTft3 <- HAW16_QTft2[c(2:numRows) , c(2:numCols)]
HAW16_QTTable <- graph.adjacency(HAW16_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(HAW16_QTTable, vertex.label = V(HAW16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
HAW16_QT.clusterCoef <- transitivity(HAW16_QTTable, type="global") #cluster coefficient
HAW16_QT.degreeCent <- centralization.degree(HAW16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_QTftn <- as.network.matrix(HAW16_QTft)
HAW16_QT.netDensity <- network.density(HAW16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_QT.entropy <- entropy(HAW16_QTft) #entropy

HAW16_QT.netMx <- cbind(HAW16_QT.netMx, HAW16_QT.clusterCoef, HAW16_QT.degreeCent$centralization,
                        HAW16_QT.netDensity, HAW16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_QT.netMx) <- varnames

#ROUND 16, Rushed Oppo**********************************************************

round = 16
teamName = "HAW"
KIoutcome = "RushedOpp_D"
HAW16_RO.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Rushed Oppo with weighted edges
HAW16_ROg2 <- data.frame(HAW16_RO)
HAW16_ROg2 <- HAW16_ROg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW16_ROg2$player1
player2vector <- HAW16_ROg2$player2
HAW16_ROg3 <- HAW16_ROg2
HAW16_ROg3$p1inp2vec <- is.element(HAW16_ROg3$player1, player2vector)
HAW16_ROg3$p2inp1vec <- is.element(HAW16_ROg3$player2, player1vector)

addPlayer1 <- HAW16_ROg3[ which(HAW16_ROg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW16_ROg3[ which(HAW16_ROg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW16_ROg2 <- rbind(HAW16_ROg2, addPlayers)

#ROUND 16, Rushed Oppo graph using weighted edges
HAW16_ROft <- ftable(HAW16_ROg2$player1, HAW16_ROg2$player2)
HAW16_ROft2 <- as.matrix(HAW16_ROft)
numRows <- nrow(HAW16_ROft2)
numCols <- ncol(HAW16_ROft2)
HAW16_ROft3 <- HAW16_ROft2[c(2:numRows) , c(2:numCols)]
HAW16_ROTable <- graph.adjacency(HAW16_ROft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Rushed Oppo graph=weighted
plot.igraph(HAW16_ROTable, vertex.label = V(HAW16_ROTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW16_ROTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Rushed Oppo calulation of network metrics
#igraph
HAW16_RO.clusterCoef <- transitivity(HAW16_ROTable, type="global") #cluster coefficient
HAW16_RO.degreeCent <- centralization.degree(HAW16_ROTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW16_ROftn <- as.network.matrix(HAW16_ROft)
HAW16_RO.netDensity <- network.density(HAW16_ROftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW16_RO.entropy <- entropy(HAW16_ROft) #entropy

HAW16_RO.netMx <- cbind(HAW16_RO.netMx, HAW16_RO.clusterCoef, HAW16_RO.degreeCent$centralization,
                        HAW16_RO.netDensity, HAW16_RO.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW16_RO.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "MELB"
KIoutcome = "Goal_F"
MELB16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
MELB16_Gg2 <- data.frame(MELB16_G)
MELB16_Gg2 <- MELB16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_Gg2$player1
player2vector <- MELB16_Gg2$player2
MELB16_Gg3 <- MELB16_Gg2
MELB16_Gg3$p1inp2vec <- is.element(MELB16_Gg3$player1, player2vector)
MELB16_Gg3$p2inp1vec <- is.element(MELB16_Gg3$player2, player1vector)

addPlayer1 <- MELB16_Gg3[ which(MELB16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB16_Gg2 <- rbind(MELB16_Gg2, addPlayer1)

#ROUND 16, Goal graph using weighted edges
MELB16_Gft <- ftable(MELB16_Gg2$player1, MELB16_Gg2$player2)
MELB16_Gft2 <- as.matrix(MELB16_Gft)
numRows <- nrow(MELB16_Gft2)
numCols <- ncol(MELB16_Gft2)
MELB16_Gft3 <- MELB16_Gft2[c(2:numRows) , c(1:numCols)]
MELB16_GTable <- graph.adjacency(MELB16_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(MELB16_GTable, vertex.label = V(MELB16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
MELB16_G.clusterCoef <- transitivity(MELB16_GTable, type="global") #cluster coefficient
MELB16_G.degreeCent <- centralization.degree(MELB16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_Gftn <- as.network.matrix(MELB16_Gft)
MELB16_G.netDensity <- network.density(MELB16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_G.entropy <- entropy(MELB16_Gft) #entropy

MELB16_G.netMx <- cbind(MELB16_G.netMx, MELB16_G.clusterCoef, MELB16_G.degreeCent$centralization,
                        MELB16_G.netDensity, MELB16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************

round = 16
teamName = "MELB"
KIoutcome = "Behind_F"
MELB16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
MELB16_Bg2 <- data.frame(MELB16_B)
MELB16_Bg2 <- MELB16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_Bg2$player1
player2vector <- MELB16_Bg2$player2
MELB16_Bg3 <- MELB16_Bg2
MELB16_Bg3$p1inp2vec <- is.element(MELB16_Bg3$player1, player2vector)
MELB16_Bg3$p2inp1vec <- is.element(MELB16_Bg3$player2, player1vector)

addPlayer1 <- MELB16_Bg3[ which(MELB16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB16_Bg3[ which(MELB16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_Bg2 <- rbind(MELB16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
MELB16_Bft <- ftable(MELB16_Bg2$player1, MELB16_Bg2$player2)
MELB16_Bft2 <- as.matrix(MELB16_Bft)
numRows <- nrow(MELB16_Bft2)
numCols <- ncol(MELB16_Bft2)
MELB16_Bft3 <- MELB16_Bft2[c(2:numRows) , c(2:numCols)]
MELB16_BTable <- graph.adjacency(MELB16_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(MELB16_BTable, vertex.label = V(MELB16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
MELB16_B.clusterCoef <- transitivity(MELB16_BTable, type="global") #cluster coefficient
MELB16_B.degreeCent <- centralization.degree(MELB16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_Bftn <- as.network.matrix(MELB16_Bft)
MELB16_B.netDensity <- network.density(MELB16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_B.entropy <- entropy(MELB16_Bft) #entropy

MELB16_B.netMx <- cbind(MELB16_B.netMx, MELB16_B.clusterCoef, MELB16_B.degreeCent$centralization,
                        MELB16_B.netDensity, MELB16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
MELB16_SFg2 <- data.frame(MELB16_SF)
MELB16_SFg2 <- MELB16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_SFg2$player1
player2vector <- MELB16_SFg2$player2
MELB16_SFg3 <- MELB16_SFg2
MELB16_SFg3$p1inp2vec <- is.element(MELB16_SFg3$player1, player2vector)
MELB16_SFg3$p2inp1vec <- is.element(MELB16_SFg3$player2, player1vector)

addPlayer1 <- MELB16_SFg3[ which(MELB16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB16_SFg3[ which(MELB16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_SFg2 <- rbind(MELB16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
MELB16_SFft <- ftable(MELB16_SFg2$player1, MELB16_SFg2$player2)
MELB16_SFft2 <- as.matrix(MELB16_SFft)
numRows <- nrow(MELB16_SFft2)
numCols <- ncol(MELB16_SFft2)
MELB16_SFft3 <- MELB16_SFft2[c(2:numRows) , c(2:numCols)]
MELB16_SFTable <- graph.adjacency(MELB16_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(MELB16_SFTable, vertex.label = V(MELB16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
MELB16_SF.clusterCoef <- transitivity(MELB16_SFTable, type="global") #cluster coefficient
MELB16_SF.degreeCent <- centralization.degree(MELB16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_SFftn <- as.network.matrix(MELB16_SFft)
MELB16_SF.netDensity <- network.density(MELB16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_SF.entropy <- entropy(MELB16_SFft) #entropy

MELB16_SF.netMx <- cbind(MELB16_SF.netMx, MELB16_SF.clusterCoef, MELB16_SF.degreeCent$centralization,
                         MELB16_SF.netDensity, MELB16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
MELB16_TFg2 <- data.frame(MELB16_TF)
MELB16_TFg2 <- MELB16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_TFg2$player1
player2vector <- MELB16_TFg2$player2
MELB16_TFg3 <- MELB16_TFg2
MELB16_TFg3$p1inp2vec <- is.element(MELB16_TFg3$player1, player2vector)
MELB16_TFg3$p2inp1vec <- is.element(MELB16_TFg3$player2, player1vector)

addPlayer1 <- MELB16_TFg3[ which(MELB16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB16_TFg3[ which(MELB16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_TFg2 <- rbind(MELB16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
MELB16_TFft <- ftable(MELB16_TFg2$player1, MELB16_TFg2$player2)
MELB16_TFft2 <- as.matrix(MELB16_TFft)
numRows <- nrow(MELB16_TFft2)
numCols <- ncol(MELB16_TFft2)
MELB16_TFft3 <- MELB16_TFft2[c(2:numRows) , c(2:numCols)]
MELB16_TFTable <- graph.adjacency(MELB16_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(MELB16_TFTable, vertex.label = V(MELB16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
MELB16_TF.clusterCoef <- transitivity(MELB16_TFTable, type="global") #cluster coefficient
MELB16_TF.degreeCent <- centralization.degree(MELB16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_TFftn <- as.network.matrix(MELB16_TFft)
MELB16_TF.netDensity <- network.density(MELB16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_TF.entropy <- entropy(MELB16_TFft) #entropy

MELB16_TF.netMx <- cbind(MELB16_TF.netMx, MELB16_TF.clusterCoef, MELB16_TF.degreeCent$centralization,
                         MELB16_TF.netDensity, MELB16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************

round = 16
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
MELB16_SAMg2 <- data.frame(MELB16_SAM)
MELB16_SAMg2 <- MELB16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_SAMg2$player1
player2vector <- MELB16_SAMg2$player2
MELB16_SAMg3 <- MELB16_SAMg2
MELB16_SAMg3$p1inp2vec <- is.element(MELB16_SAMg3$player1, player2vector)
MELB16_SAMg3$p2inp1vec <- is.element(MELB16_SAMg3$player2, player1vector)

addPlayer1 <- MELB16_SAMg3[ which(MELB16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB16_SAMg3[ which(MELB16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_SAMg2 <- rbind(MELB16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
MELB16_SAMft <- ftable(MELB16_SAMg2$player1, MELB16_SAMg2$player2)
MELB16_SAMft2 <- as.matrix(MELB16_SAMft)
numRows <- nrow(MELB16_SAMft2)
numCols <- ncol(MELB16_SAMft2)
MELB16_SAMft3 <- MELB16_SAMft2[c(2:numRows) , c(2:numCols)]
MELB16_SAMTable <- graph.adjacency(MELB16_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(MELB16_SAMTable, vertex.label = V(MELB16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
MELB16_SAM.clusterCoef <- transitivity(MELB16_SAMTable, type="global") #cluster coefficient
MELB16_SAM.degreeCent <- centralization.degree(MELB16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_SAMftn <- as.network.matrix(MELB16_SAMft)
MELB16_SAM.netDensity <- network.density(MELB16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_SAM.entropy <- entropy(MELB16_SAMft) #entropy

MELB16_SAM.netMx <- cbind(MELB16_SAM.netMx, MELB16_SAM.clusterCoef, MELB16_SAM.degreeCent$centralization,
                          MELB16_SAM.netDensity, MELB16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************
#NA

round = 16
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
MELB16_TAMg2 <- data.frame(MELB16_TAM)
MELB16_TAMg2 <- MELB16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_TAMg2$player1
player2vector <- MELB16_TAMg2$player2
MELB16_TAMg3 <- MELB16_TAMg2
MELB16_TAMg3$p1inp2vec <- is.element(MELB16_TAMg3$player1, player2vector)
MELB16_TAMg3$p2inp1vec <- is.element(MELB16_TAMg3$player2, player1vector)

addPlayer1 <- MELB16_TAMg3[ which(MELB16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB16_TAMg3[ which(MELB16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_TAMg2 <- rbind(MELB16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
MELB16_TAMft <- ftable(MELB16_TAMg2$player1, MELB16_TAMg2$player2)
MELB16_TAMft2 <- as.matrix(MELB16_TAMft)
numRows <- nrow(MELB16_TAMft2)
numCols <- ncol(MELB16_TAMft2)
MELB16_TAMft3 <- MELB16_TAMft2[c(2:numRows) , c(2:numCols)]
MELB16_TAMTable <- graph.adjacency(MELB16_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(MELB16_TAMTable, vertex.label = V(MELB16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
MELB16_TAM.clusterCoef <- transitivity(MELB16_TAMTable, type="global") #cluster coefficient
MELB16_TAM.degreeCent <- centralization.degree(MELB16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_TAMftn <- as.network.matrix(MELB16_TAMft)
MELB16_TAM.netDensity <- network.density(MELB16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_TAM.entropy <- entropy(MELB16_TAMft) #entropy

MELB16_TAM.netMx <- cbind(MELB16_TAM.netMx, MELB16_TAM.clusterCoef, MELB16_TAM.degreeCent$centralization,
                          MELB16_TAM.netDensity, MELB16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
MELB16_SDMg2 <- data.frame(MELB16_SDM)
MELB16_SDMg2 <- MELB16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_SDMg2$player1
player2vector <- MELB16_SDMg2$player2
MELB16_SDMg3 <- MELB16_SDMg2
MELB16_SDMg3$p1inp2vec <- is.element(MELB16_SDMg3$player1, player2vector)
MELB16_SDMg3$p2inp1vec <- is.element(MELB16_SDMg3$player2, player1vector)

addPlayer1 <- MELB16_SDMg3[ which(MELB16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB16_SDMg3[ which(MELB16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_SDMg2 <- rbind(MELB16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
MELB16_SDMft <- ftable(MELB16_SDMg2$player1, MELB16_SDMg2$player2)
MELB16_SDMft2 <- as.matrix(MELB16_SDMft)
numRows <- nrow(MELB16_SDMft2)
numCols <- ncol(MELB16_SDMft2)
MELB16_SDMft3 <- MELB16_SDMft2[c(2:numRows) , c(2:numCols)]
MELB16_SDMTable <- graph.adjacency(MELB16_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(MELB16_SDMTable, vertex.label = V(MELB16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
MELB16_SDM.clusterCoef <- transitivity(MELB16_SDMTable, type="global") #cluster coefficient
MELB16_SDM.degreeCent <- centralization.degree(MELB16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_SDMftn <- as.network.matrix(MELB16_SDMft)
MELB16_SDM.netDensity <- network.density(MELB16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_SDM.entropy <- entropy(MELB16_SDMft) #entropy

MELB16_SDM.netMx <- cbind(MELB16_SDM.netMx, MELB16_SDM.clusterCoef, MELB16_SDM.degreeCent$centralization,
                          MELB16_SDM.netDensity, MELB16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
MELB16_TDMg2 <- data.frame(MELB16_TDM)
MELB16_TDMg2 <- MELB16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_TDMg2$player1
player2vector <- MELB16_TDMg2$player2
MELB16_TDMg3 <- MELB16_TDMg2
MELB16_TDMg3$p1inp2vec <- is.element(MELB16_TDMg3$player1, player2vector)
MELB16_TDMg3$p2inp1vec <- is.element(MELB16_TDMg3$player2, player1vector)

addPlayer1 <- MELB16_TDMg3[ which(MELB16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB16_TDMg3[ which(MELB16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_TDMg2 <- rbind(MELB16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
MELB16_TDMft <- ftable(MELB16_TDMg2$player1, MELB16_TDMg2$player2)
MELB16_TDMft2 <- as.matrix(MELB16_TDMft)
numRows <- nrow(MELB16_TDMft2)
numCols <- ncol(MELB16_TDMft2)
MELB16_TDMft3 <- MELB16_TDMft2[c(2:numRows) , c(2:numCols)]
MELB16_TDMTable <- graph.adjacency(MELB16_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(MELB16_TDMTable, vertex.label = V(MELB16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
MELB16_TDM.clusterCoef <- transitivity(MELB16_TDMTable, type="global") #cluster coefficient
MELB16_TDM.degreeCent <- centralization.degree(MELB16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_TDMftn <- as.network.matrix(MELB16_TDMft)
MELB16_TDM.netDensity <- network.density(MELB16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_TDM.entropy <- entropy(MELB16_TDMft) #entropy

MELB16_TDM.netMx <- cbind(MELB16_TDM.netMx, MELB16_TDM.clusterCoef, MELB16_TDM.degreeCent$centralization,
                          MELB16_TDM.netDensity, MELB16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
MELB16_SDg2 <- data.frame(MELB16_SD)
MELB16_SDg2 <- MELB16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_SDg2$player1
player2vector <- MELB16_SDg2$player2
MELB16_SDg3 <- MELB16_SDg2
MELB16_SDg3$p1inp2vec <- is.element(MELB16_SDg3$player1, player2vector)
MELB16_SDg3$p2inp1vec <- is.element(MELB16_SDg3$player2, player1vector)

addPlayer1 <- MELB16_SDg3[ which(MELB16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB16_SDg3[ which(MELB16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_SDg2 <- rbind(MELB16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
MELB16_SDft <- ftable(MELB16_SDg2$player1, MELB16_SDg2$player2)
MELB16_SDft2 <- as.matrix(MELB16_SDft)
numRows <- nrow(MELB16_SDft2)
numCols <- ncol(MELB16_SDft2)
MELB16_SDft3 <- MELB16_SDft2[c(2:numRows) , c(2:numCols)]
MELB16_SDTable <- graph.adjacency(MELB16_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(MELB16_SDTable, vertex.label = V(MELB16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
MELB16_SD.clusterCoef <- transitivity(MELB16_SDTable, type="global") #cluster coefficient
MELB16_SD.degreeCent <- centralization.degree(MELB16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_SDftn <- as.network.matrix(MELB16_SDft)
MELB16_SD.netDensity <- network.density(MELB16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_SD.entropy <- entropy(MELB16_SDft) #entropy

MELB16_SD.netMx <- cbind(MELB16_SD.netMx, MELB16_SD.clusterCoef, MELB16_SD.degreeCent$centralization,
                         MELB16_SD.netDensity, MELB16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
MELB16_TDg2 <- data.frame(MELB16_TD)
MELB16_TDg2 <- MELB16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_TDg2$player1
player2vector <- MELB16_TDg2$player2
MELB16_TDg3 <- MELB16_TDg2
MELB16_TDg3$p1inp2vec <- is.element(MELB16_TDg3$player1, player2vector)
MELB16_TDg3$p2inp1vec <- is.element(MELB16_TDg3$player2, player1vector)

addPlayer1 <- MELB16_TDg3[ which(MELB16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB16_TDg3[ which(MELB16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_TDg2 <- rbind(MELB16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
MELB16_TDft <- ftable(MELB16_TDg2$player1, MELB16_TDg2$player2)
MELB16_TDft2 <- as.matrix(MELB16_TDft)
numRows <- nrow(MELB16_TDft2)
numCols <- ncol(MELB16_TDft2)
MELB16_TDft3 <- MELB16_TDft2[c(2:numRows) , c(2:numCols)]
MELB16_TDTable <- graph.adjacency(MELB16_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(MELB16_TDTable, vertex.label = V(MELB16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
MELB16_TD.clusterCoef <- transitivity(MELB16_TDTable, type="global") #cluster coefficient
MELB16_TD.degreeCent <- centralization.degree(MELB16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_TDftn <- as.network.matrix(MELB16_TDft)
MELB16_TD.netDensity <- network.density(MELB16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_TD.entropy <- entropy(MELB16_TDft) #entropy

MELB16_TD.netMx <- cbind(MELB16_TD.netMx, MELB16_TD.clusterCoef, MELB16_TD.degreeCent$centralization,
                         MELB16_TD.netDensity, MELB16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
MELB16_QTg2 <- data.frame(MELB16_QT)
MELB16_QTg2 <- MELB16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB16_QTg2$player1
player2vector <- MELB16_QTg2$player2
MELB16_QTg3 <- MELB16_QTg2
MELB16_QTg3$p1inp2vec <- is.element(MELB16_QTg3$player1, player2vector)
MELB16_QTg3$p2inp1vec <- is.element(MELB16_QTg3$player2, player1vector)

addPlayer1 <- MELB16_QTg3[ which(MELB16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB16_QTg3[ which(MELB16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB16_QTg2 <- rbind(MELB16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
MELB16_QTft <- ftable(MELB16_QTg2$player1, MELB16_QTg2$player2)
MELB16_QTft2 <- as.matrix(MELB16_QTft)
numRows <- nrow(MELB16_QTft2)
numCols <- ncol(MELB16_QTft2)
MELB16_QTft3 <- MELB16_QTft2[c(2:numRows) , c(2:numCols)]
MELB16_QTTable <- graph.adjacency(MELB16_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(MELB16_QTTable, vertex.label = V(MELB16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
MELB16_QT.clusterCoef <- transitivity(MELB16_QTTable, type="global") #cluster coefficient
MELB16_QT.degreeCent <- centralization.degree(MELB16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB16_QTftn <- as.network.matrix(MELB16_QTft)
MELB16_QT.netDensity <- network.density(MELB16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB16_QT.entropy <- entropy(MELB16_QTft) #entropy

MELB16_QT.netMx <- cbind(MELB16_QT.netMx, MELB16_QT.clusterCoef, MELB16_QT.degreeCent$centralization,
                         MELB16_QT.netDensity, MELB16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB16_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************

round = 16
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
NMFC16_Gg2 <- data.frame(NMFC16_G)
NMFC16_Gg2 <- NMFC16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_Gg2$player1
player2vector <- NMFC16_Gg2$player2
NMFC16_Gg3 <- NMFC16_Gg2
NMFC16_Gg3$p1inp2vec <- is.element(NMFC16_Gg3$player1, player2vector)
NMFC16_Gg3$p2inp1vec <- is.element(NMFC16_Gg3$player2, player1vector)

addPlayer1 <- NMFC16_Gg3[ which(NMFC16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_Gg3[ which(NMFC16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_Gg2 <- rbind(NMFC16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
NMFC16_Gft <- ftable(NMFC16_Gg2$player1, NMFC16_Gg2$player2)
NMFC16_Gft2 <- as.matrix(NMFC16_Gft)
numRows <- nrow(NMFC16_Gft2)
numCols <- ncol(NMFC16_Gft2)
NMFC16_Gft3 <- NMFC16_Gft2[c(2:numRows) , c(2:numCols)]
NMFC16_GTable <- graph.adjacency(NMFC16_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(NMFC16_GTable, vertex.label = V(NMFC16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
NMFC16_G.clusterCoef <- transitivity(NMFC16_GTable, type="global") #cluster coefficient
NMFC16_G.degreeCent <- centralization.degree(NMFC16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_Gftn <- as.network.matrix(NMFC16_Gft)
NMFC16_G.netDensity <- network.density(NMFC16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_G.entropy <- entropy(NMFC16_Gft) #entropy

NMFC16_G.netMx <- cbind(NMFC16_G.netMx, NMFC16_G.clusterCoef, NMFC16_G.degreeCent$centralization,
                        NMFC16_G.netDensity, NMFC16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
NMFC16_Bg2 <- data.frame(NMFC16_B)
NMFC16_Bg2 <- NMFC16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_Bg2$player1
player2vector <- NMFC16_Bg2$player2
NMFC16_Bg3 <- NMFC16_Bg2
NMFC16_Bg3$p1inp2vec <- is.element(NMFC16_Bg3$player1, player2vector)
NMFC16_Bg3$p2inp1vec <- is.element(NMFC16_Bg3$player2, player1vector)

addPlayer1 <- NMFC16_Bg3[ which(NMFC16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_Bg3[ which(NMFC16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_Bg2 <- rbind(NMFC16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
NMFC16_Bft <- ftable(NMFC16_Bg2$player1, NMFC16_Bg2$player2)
NMFC16_Bft2 <- as.matrix(NMFC16_Bft)
numRows <- nrow(NMFC16_Bft2)
numCols <- ncol(NMFC16_Bft2)
NMFC16_Bft3 <- NMFC16_Bft2[c(2:numRows) , c(2:numCols)]
NMFC16_BTable <- graph.adjacency(NMFC16_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(NMFC16_BTable, vertex.label = V(NMFC16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
NMFC16_B.clusterCoef <- transitivity(NMFC16_BTable, type="global") #cluster coefficient
NMFC16_B.degreeCent <- centralization.degree(NMFC16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_Bftn <- as.network.matrix(NMFC16_Bft)
NMFC16_B.netDensity <- network.density(NMFC16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_B.entropy <- entropy(NMFC16_Bft) #entropy

NMFC16_B.netMx <- cbind(NMFC16_B.netMx, NMFC16_B.clusterCoef, NMFC16_B.degreeCent$centralization,
                        NMFC16_B.netDensity, NMFC16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
NMFC16_SFg2 <- data.frame(NMFC16_SF)
NMFC16_SFg2 <- NMFC16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_SFg2$player1
player2vector <- NMFC16_SFg2$player2
NMFC16_SFg3 <- NMFC16_SFg2
NMFC16_SFg3$p1inp2vec <- is.element(NMFC16_SFg3$player1, player2vector)
NMFC16_SFg3$p2inp1vec <- is.element(NMFC16_SFg3$player2, player1vector)

addPlayer1 <- NMFC16_SFg3[ which(NMFC16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_SFg3[ which(NMFC16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_SFg2 <- rbind(NMFC16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
NMFC16_SFft <- ftable(NMFC16_SFg2$player1, NMFC16_SFg2$player2)
NMFC16_SFft2 <- as.matrix(NMFC16_SFft)
numRows <- nrow(NMFC16_SFft2)
numCols <- ncol(NMFC16_SFft2)
NMFC16_SFft3 <- NMFC16_SFft2[c(2:numRows) , c(2:numCols)]
NMFC16_SFTable <- graph.adjacency(NMFC16_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(NMFC16_SFTable, vertex.label = V(NMFC16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
NMFC16_SF.clusterCoef <- transitivity(NMFC16_SFTable, type="global") #cluster coefficient
NMFC16_SF.degreeCent <- centralization.degree(NMFC16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_SFftn <- as.network.matrix(NMFC16_SFft)
NMFC16_SF.netDensity <- network.density(NMFC16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_SF.entropy <- entropy(NMFC16_SFft) #entropy

NMFC16_SF.netMx <- cbind(NMFC16_SF.netMx, NMFC16_SF.clusterCoef, NMFC16_SF.degreeCent$centralization,
                         NMFC16_SF.netDensity, NMFC16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
NMFC16_TFg2 <- data.frame(NMFC16_TF)
NMFC16_TFg2 <- NMFC16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_TFg2$player1
player2vector <- NMFC16_TFg2$player2
NMFC16_TFg3 <- NMFC16_TFg2
NMFC16_TFg3$p1inp2vec <- is.element(NMFC16_TFg3$player1, player2vector)
NMFC16_TFg3$p2inp1vec <- is.element(NMFC16_TFg3$player2, player1vector)

addPlayer1 <- NMFC16_TFg3[ which(NMFC16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_TFg3[ which(NMFC16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_TFg2 <- rbind(NMFC16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
NMFC16_TFft <- ftable(NMFC16_TFg2$player1, NMFC16_TFg2$player2)
NMFC16_TFft2 <- as.matrix(NMFC16_TFft)
numRows <- nrow(NMFC16_TFft2)
numCols <- ncol(NMFC16_TFft2)
NMFC16_TFft3 <- NMFC16_TFft2[c(2:numRows) , c(2:numCols)]
NMFC16_TFTable <- graph.adjacency(NMFC16_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(NMFC16_TFTable, vertex.label = V(NMFC16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
NMFC16_TF.clusterCoef <- transitivity(NMFC16_TFTable, type="global") #cluster coefficient
NMFC16_TF.degreeCent <- centralization.degree(NMFC16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_TFftn <- as.network.matrix(NMFC16_TFft)
NMFC16_TF.netDensity <- network.density(NMFC16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_TF.entropy <- entropy(NMFC16_TFft) #entropy

NMFC16_TF.netMx <- cbind(NMFC16_TF.netMx, NMFC16_TF.clusterCoef, NMFC16_TF.degreeCent$centralization,
                         NMFC16_TF.netDensity, NMFC16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
NMFC16_SAMg2 <- data.frame(NMFC16_SAM)
NMFC16_SAMg2 <- NMFC16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_SAMg2$player1
player2vector <- NMFC16_SAMg2$player2
NMFC16_SAMg3 <- NMFC16_SAMg2
NMFC16_SAMg3$p1inp2vec <- is.element(NMFC16_SAMg3$player1, player2vector)
NMFC16_SAMg3$p2inp1vec <- is.element(NMFC16_SAMg3$player2, player1vector)

addPlayer1 <- NMFC16_SAMg3[ which(NMFC16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_SAMg3[ which(NMFC16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_SAMg2 <- rbind(NMFC16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
NMFC16_SAMft <- ftable(NMFC16_SAMg2$player1, NMFC16_SAMg2$player2)
NMFC16_SAMft2 <- as.matrix(NMFC16_SAMft)
numRows <- nrow(NMFC16_SAMft2)
numCols <- ncol(NMFC16_SAMft2)
NMFC16_SAMft3 <- NMFC16_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC16_SAMTable <- graph.adjacency(NMFC16_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(NMFC16_SAMTable, vertex.label = V(NMFC16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
NMFC16_SAM.clusterCoef <- transitivity(NMFC16_SAMTable, type="global") #cluster coefficient
NMFC16_SAM.degreeCent <- centralization.degree(NMFC16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_SAMftn <- as.network.matrix(NMFC16_SAMft)
NMFC16_SAM.netDensity <- network.density(NMFC16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_SAM.entropy <- entropy(NMFC16_SAMft) #entropy

NMFC16_SAM.netMx <- cbind(NMFC16_SAM.netMx, NMFC16_SAM.clusterCoef, NMFC16_SAM.degreeCent$centralization,
                          NMFC16_SAM.netDensity, NMFC16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
NMFC16_TAMg2 <- data.frame(NMFC16_TAM)
NMFC16_TAMg2 <- NMFC16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_TAMg2$player1
player2vector <- NMFC16_TAMg2$player2
NMFC16_TAMg3 <- NMFC16_TAMg2
NMFC16_TAMg3$p1inp2vec <- is.element(NMFC16_TAMg3$player1, player2vector)
NMFC16_TAMg3$p2inp1vec <- is.element(NMFC16_TAMg3$player2, player1vector)

addPlayer1 <- NMFC16_TAMg3[ which(NMFC16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_TAMg3[ which(NMFC16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_TAMg2 <- rbind(NMFC16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
NMFC16_TAMft <- ftable(NMFC16_TAMg2$player1, NMFC16_TAMg2$player2)
NMFC16_TAMft2 <- as.matrix(NMFC16_TAMft)
numRows <- nrow(NMFC16_TAMft2)
numCols <- ncol(NMFC16_TAMft2)
NMFC16_TAMft3 <- NMFC16_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC16_TAMTable <- graph.adjacency(NMFC16_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(NMFC16_TAMTable, vertex.label = V(NMFC16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
NMFC16_TAM.clusterCoef <- transitivity(NMFC16_TAMTable, type="global") #cluster coefficient
NMFC16_TAM.degreeCent <- centralization.degree(NMFC16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_TAMftn <- as.network.matrix(NMFC16_TAMft)
NMFC16_TAM.netDensity <- network.density(NMFC16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_TAM.entropy <- entropy(NMFC16_TAMft) #entropy

NMFC16_TAM.netMx <- cbind(NMFC16_TAM.netMx, NMFC16_TAM.clusterCoef, NMFC16_TAM.degreeCent$centralization,
                          NMFC16_TAM.netDensity, NMFC16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
NMFC16_SDMg2 <- data.frame(NMFC16_SDM)
NMFC16_SDMg2 <- NMFC16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_SDMg2$player1
player2vector <- NMFC16_SDMg2$player2
NMFC16_SDMg3 <- NMFC16_SDMg2
NMFC16_SDMg3$p1inp2vec <- is.element(NMFC16_SDMg3$player1, player2vector)
NMFC16_SDMg3$p2inp1vec <- is.element(NMFC16_SDMg3$player2, player1vector)

addPlayer1 <- NMFC16_SDMg3[ which(NMFC16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_SDMg3[ which(NMFC16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_SDMg2 <- rbind(NMFC16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
NMFC16_SDMft <- ftable(NMFC16_SDMg2$player1, NMFC16_SDMg2$player2)
NMFC16_SDMft2 <- as.matrix(NMFC16_SDMft)
numRows <- nrow(NMFC16_SDMft2)
numCols <- ncol(NMFC16_SDMft2)
NMFC16_SDMft3 <- NMFC16_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC16_SDMTable <- graph.adjacency(NMFC16_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(NMFC16_SDMTable, vertex.label = V(NMFC16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
NMFC16_SDM.clusterCoef <- transitivity(NMFC16_SDMTable, type="global") #cluster coefficient
NMFC16_SDM.degreeCent <- centralization.degree(NMFC16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_SDMftn <- as.network.matrix(NMFC16_SDMft)
NMFC16_SDM.netDensity <- network.density(NMFC16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_SDM.entropy <- entropy(NMFC16_SDMft) #entropy

NMFC16_SDM.netMx <- cbind(NMFC16_SDM.netMx, NMFC16_SDM.clusterCoef, NMFC16_SDM.degreeCent$centralization,
                          NMFC16_SDM.netDensity, NMFC16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
NMFC16_TDMg2 <- data.frame(NMFC16_TDM)
NMFC16_TDMg2 <- NMFC16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_TDMg2$player1
player2vector <- NMFC16_TDMg2$player2
NMFC16_TDMg3 <- NMFC16_TDMg2
NMFC16_TDMg3$p1inp2vec <- is.element(NMFC16_TDMg3$player1, player2vector)
NMFC16_TDMg3$p2inp1vec <- is.element(NMFC16_TDMg3$player2, player1vector)

addPlayer1 <- NMFC16_TDMg3[ which(NMFC16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_TDMg3[ which(NMFC16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_TDMg2 <- rbind(NMFC16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
NMFC16_TDMft <- ftable(NMFC16_TDMg2$player1, NMFC16_TDMg2$player2)
NMFC16_TDMft2 <- as.matrix(NMFC16_TDMft)
numRows <- nrow(NMFC16_TDMft2)
numCols <- ncol(NMFC16_TDMft2)
NMFC16_TDMft3 <- NMFC16_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC16_TDMTable <- graph.adjacency(NMFC16_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(NMFC16_TDMTable, vertex.label = V(NMFC16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
NMFC16_TDM.clusterCoef <- transitivity(NMFC16_TDMTable, type="global") #cluster coefficient
NMFC16_TDM.degreeCent <- centralization.degree(NMFC16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_TDMftn <- as.network.matrix(NMFC16_TDMft)
NMFC16_TDM.netDensity <- network.density(NMFC16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_TDM.entropy <- entropy(NMFC16_TDMft) #entropy

NMFC16_TDM.netMx <- cbind(NMFC16_TDM.netMx, NMFC16_TDM.clusterCoef, NMFC16_TDM.degreeCent$centralization,
                          NMFC16_TDM.netDensity, NMFC16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
NMFC16_SDg2 <- data.frame(NMFC16_SD)
NMFC16_SDg2 <- NMFC16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_SDg2$player1
player2vector <- NMFC16_SDg2$player2
NMFC16_SDg3 <- NMFC16_SDg2
NMFC16_SDg3$p1inp2vec <- is.element(NMFC16_SDg3$player1, player2vector)
NMFC16_SDg3$p2inp1vec <- is.element(NMFC16_SDg3$player2, player1vector)

addPlayer1 <- NMFC16_SDg3[ which(NMFC16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_SDg3[ which(NMFC16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_SDg2 <- rbind(NMFC16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
NMFC16_SDft <- ftable(NMFC16_SDg2$player1, NMFC16_SDg2$player2)
NMFC16_SDft2 <- as.matrix(NMFC16_SDft)
numRows <- nrow(NMFC16_SDft2)
numCols <- ncol(NMFC16_SDft2)
NMFC16_SDft3 <- NMFC16_SDft2[c(2:numRows) , c(2:numCols)]
NMFC16_SDTable <- graph.adjacency(NMFC16_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(NMFC16_SDTable, vertex.label = V(NMFC16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
NMFC16_SD.clusterCoef <- transitivity(NMFC16_SDTable, type="global") #cluster coefficient
NMFC16_SD.degreeCent <- centralization.degree(NMFC16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_SDftn <- as.network.matrix(NMFC16_SDft)
NMFC16_SD.netDensity <- network.density(NMFC16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_SD.entropy <- entropy(NMFC16_SDft) #entropy

NMFC16_SD.netMx <- cbind(NMFC16_SD.netMx, NMFC16_SD.clusterCoef, NMFC16_SD.degreeCent$centralization,
                         NMFC16_SD.netDensity, NMFC16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
NMFC16_TDg2 <- data.frame(NMFC16_TD)
NMFC16_TDg2 <- NMFC16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_TDg2$player1
player2vector <- NMFC16_TDg2$player2
NMFC16_TDg3 <- NMFC16_TDg2
NMFC16_TDg3$p1inp2vec <- is.element(NMFC16_TDg3$player1, player2vector)
NMFC16_TDg3$p2inp1vec <- is.element(NMFC16_TDg3$player2, player1vector)

addPlayer1 <- NMFC16_TDg3[ which(NMFC16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_TDg3[ which(NMFC16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_TDg2 <- rbind(NMFC16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
NMFC16_TDft <- ftable(NMFC16_TDg2$player1, NMFC16_TDg2$player2)
NMFC16_TDft2 <- as.matrix(NMFC16_TDft)
numRows <- nrow(NMFC16_TDft2)
numCols <- ncol(NMFC16_TDft2)
NMFC16_TDft3 <- NMFC16_TDft2[c(2:numRows) , c(2:numCols)]
NMFC16_TDTable <- graph.adjacency(NMFC16_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(NMFC16_TDTable, vertex.label = V(NMFC16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
NMFC16_TD.clusterCoef <- transitivity(NMFC16_TDTable, type="global") #cluster coefficient
NMFC16_TD.degreeCent <- centralization.degree(NMFC16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_TDftn <- as.network.matrix(NMFC16_TDft)
NMFC16_TD.netDensity <- network.density(NMFC16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_TD.entropy <- entropy(NMFC16_TDft) #entropy

NMFC16_TD.netMx <- cbind(NMFC16_TD.netMx, NMFC16_TD.clusterCoef, NMFC16_TD.degreeCent$centralization,
                         NMFC16_TD.netDensity, NMFC16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
NMFC16_QTg2 <- data.frame(NMFC16_QT)
NMFC16_QTg2 <- NMFC16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC16_QTg2$player1
player2vector <- NMFC16_QTg2$player2
NMFC16_QTg3 <- NMFC16_QTg2
NMFC16_QTg3$p1inp2vec <- is.element(NMFC16_QTg3$player1, player2vector)
NMFC16_QTg3$p2inp1vec <- is.element(NMFC16_QTg3$player2, player1vector)

addPlayer1 <- NMFC16_QTg3[ which(NMFC16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC16_QTg3[ which(NMFC16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC16_QTg2 <- rbind(NMFC16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
NMFC16_QTft <- ftable(NMFC16_QTg2$player1, NMFC16_QTg2$player2)
NMFC16_QTft2 <- as.matrix(NMFC16_QTft)
numRows <- nrow(NMFC16_QTft2)
numCols <- ncol(NMFC16_QTft2)
NMFC16_QTft3 <- NMFC16_QTft2[c(2:numRows) , c(2:numCols)]
NMFC16_QTTable <- graph.adjacency(NMFC16_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(NMFC16_QTTable, vertex.label = V(NMFC16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
NMFC16_QT.clusterCoef <- transitivity(NMFC16_QTTable, type="global") #cluster coefficient
NMFC16_QT.degreeCent <- centralization.degree(NMFC16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC16_QTftn <- as.network.matrix(NMFC16_QTft)
NMFC16_QT.netDensity <- network.density(NMFC16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC16_QT.entropy <- entropy(NMFC16_QTft) #entropy

NMFC16_QT.netMx <- cbind(NMFC16_QT.netMx, NMFC16_QT.clusterCoef, NMFC16_QT.degreeCent$centralization,
                         NMFC16_QT.netDensity, NMFC16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC16_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "PORT"
KIoutcome = "Goal_F"
PORT16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
PORT16_Gg2 <- data.frame(PORT16_G)
PORT16_Gg2 <- PORT16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_Gg2$player1
player2vector <- PORT16_Gg2$player2
PORT16_Gg3 <- PORT16_Gg2
PORT16_Gg3$p1inp2vec <- is.element(PORT16_Gg3$player1, player2vector)
PORT16_Gg3$p2inp1vec <- is.element(PORT16_Gg3$player2, player1vector)

addPlayer1 <- PORT16_Gg3[ which(PORT16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT16_Gg2 <- rbind(PORT16_Gg2, addPlayer1)

#ROUND 16, Goal graph using weighted edges
PORT16_Gft <- ftable(PORT16_Gg2$player1, PORT16_Gg2$player2)
PORT16_Gft2 <- as.matrix(PORT16_Gft)
numRows <- nrow(PORT16_Gft2)
numCols <- ncol(PORT16_Gft2)
PORT16_Gft3 <- PORT16_Gft2[c(2:numRows) , c(1:numCols)]
PORT16_GTable <- graph.adjacency(PORT16_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(PORT16_GTable, vertex.label = V(PORT16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
PORT16_G.clusterCoef <- transitivity(PORT16_GTable, type="global") #cluster coefficient
PORT16_G.degreeCent <- centralization.degree(PORT16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_Gftn <- as.network.matrix(PORT16_Gft)
PORT16_G.netDensity <- network.density(PORT16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_G.entropy <- entropy(PORT16_Gft) #entropy

PORT16_G.netMx <- cbind(PORT16_G.netMx, PORT16_G.clusterCoef, PORT16_G.degreeCent$centralization,
                        PORT16_G.netDensity, PORT16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************

round = 16
teamName = "PORT"
KIoutcome = "Behind_F"
PORT16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
PORT16_Bg2 <- data.frame(PORT16_B)
PORT16_Bg2 <- PORT16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_Bg2$player1
player2vector <- PORT16_Bg2$player2
PORT16_Bg3 <- PORT16_Bg2
PORT16_Bg3$p1inp2vec <- is.element(PORT16_Bg3$player1, player2vector)
PORT16_Bg3$p2inp1vec <- is.element(PORT16_Bg3$player2, player1vector)

addPlayer1 <- PORT16_Bg3[ which(PORT16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT16_Bg3[ which(PORT16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT16_Bg2 <- rbind(PORT16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
PORT16_Bft <- ftable(PORT16_Bg2$player1, PORT16_Bg2$player2)
PORT16_Bft2 <- as.matrix(PORT16_Bft)
numRows <- nrow(PORT16_Bft2)
numCols <- ncol(PORT16_Bft2)
PORT16_Bft3 <- PORT16_Bft2[c(2:numRows) , c(2:numCols)]
PORT16_BTable <- graph.adjacency(PORT16_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(PORT16_BTable, vertex.label = V(PORT16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
PORT16_B.clusterCoef <- transitivity(PORT16_BTable, type="global") #cluster coefficient
PORT16_B.degreeCent <- centralization.degree(PORT16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_Bftn <- as.network.matrix(PORT16_Bft)
PORT16_B.netDensity <- network.density(PORT16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_B.entropy <- entropy(PORT16_Bft) #entropy

PORT16_B.netMx <- cbind(PORT16_B.netMx, PORT16_B.clusterCoef, PORT16_B.degreeCent$centralization,
                        PORT16_B.netDensity, PORT16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
PORT16_SFg2 <- data.frame(PORT16_SF)
PORT16_SFg2 <- PORT16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_SFg2$player1
player2vector <- PORT16_SFg2$player2
PORT16_SFg3 <- PORT16_SFg2
PORT16_SFg3$p1inp2vec <- is.element(PORT16_SFg3$player1, player2vector)
PORT16_SFg3$p2inp1vec <- is.element(PORT16_SFg3$player2, player1vector)

addPlayer1 <- PORT16_SFg3[ which(PORT16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT16_SFg3[ which(PORT16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT16_SFg2 <- rbind(PORT16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
PORT16_SFft <- ftable(PORT16_SFg2$player1, PORT16_SFg2$player2)
PORT16_SFft2 <- as.matrix(PORT16_SFft)
numRows <- nrow(PORT16_SFft2)
numCols <- ncol(PORT16_SFft2)
PORT16_SFft3 <- PORT16_SFft2[c(2:numRows) , c(2:numCols)]
PORT16_SFTable <- graph.adjacency(PORT16_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(PORT16_SFTable, vertex.label = V(PORT16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
PORT16_SF.clusterCoef <- transitivity(PORT16_SFTable, type="global") #cluster coefficient
PORT16_SF.degreeCent <- centralization.degree(PORT16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_SFftn <- as.network.matrix(PORT16_SFft)
PORT16_SF.netDensity <- network.density(PORT16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_SF.entropy <- entropy(PORT16_SFft) #entropy

PORT16_SF.netMx <- cbind(PORT16_SF.netMx, PORT16_SF.clusterCoef, PORT16_SF.degreeCent$centralization,
                         PORT16_SF.netDensity, PORT16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************
#NA

round = 16
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
PORT16_TFg2 <- data.frame(PORT16_TF)
PORT16_TFg2 <- PORT16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_TFg2$player1
player2vector <- PORT16_TFg2$player2
PORT16_TFg3 <- PORT16_TFg2
PORT16_TFg3$p1inp2vec <- is.element(PORT16_TFg3$player1, player2vector)
PORT16_TFg3$p2inp1vec <- is.element(PORT16_TFg3$player2, player1vector)

addPlayer1 <- PORT16_TFg3[ which(PORT16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT16_TFg2 <- rbind(PORT16_TFg2, addPlayer1)

#ROUND 16, FWD Turnover graph using weighted edges
PORT16_TFft <- ftable(PORT16_TFg2$player1, PORT16_TFg2$player2)
PORT16_TFft2 <- as.matrix(PORT16_TFft)
numRows <- nrow(PORT16_TFft2)
numCols <- ncol(PORT16_TFft2)
PORT16_TFft3 <- PORT16_TFft2[c(2:numRows) , c(1:numCols)]
PORT16_TFTable <- graph.adjacency(PORT16_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(PORT16_TFTable, vertex.label = V(PORT16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
PORT16_TF.clusterCoef <- transitivity(PORT16_TFTable, type="global") #cluster coefficient
PORT16_TF.degreeCent <- centralization.degree(PORT16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_TFftn <- as.network.matrix(PORT16_TFft)
PORT16_TF.netDensity <- network.density(PORT16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_TF.entropy <- entropy(PORT16_TFft) #entropy

PORT16_TF.netMx <- cbind(PORT16_TF.netMx, PORT16_TF.clusterCoef, PORT16_TF.degreeCent$centralization,
                         PORT16_TF.netDensity, PORT16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************

round = 16
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
PORT16_SAMg2 <- data.frame(PORT16_SAM)
PORT16_SAMg2 <- PORT16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_SAMg2$player1
player2vector <- PORT16_SAMg2$player2
PORT16_SAMg3 <- PORT16_SAMg2
PORT16_SAMg3$p1inp2vec <- is.element(PORT16_SAMg3$player1, player2vector)
PORT16_SAMg3$p2inp1vec <- is.element(PORT16_SAMg3$player2, player1vector)

addPlayer1 <- PORT16_SAMg3[ which(PORT16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT16_SAMg3[ which(PORT16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT16_SAMg2 <- rbind(PORT16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
PORT16_SAMft <- ftable(PORT16_SAMg2$player1, PORT16_SAMg2$player2)
PORT16_SAMft2 <- as.matrix(PORT16_SAMft)
numRows <- nrow(PORT16_SAMft2)
numCols <- ncol(PORT16_SAMft2)
PORT16_SAMft3 <- PORT16_SAMft2[c(2:numRows) , c(2:numCols)]
PORT16_SAMTable <- graph.adjacency(PORT16_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(PORT16_SAMTable, vertex.label = V(PORT16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
PORT16_SAM.clusterCoef <- transitivity(PORT16_SAMTable, type="global") #cluster coefficient
PORT16_SAM.degreeCent <- centralization.degree(PORT16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_SAMftn <- as.network.matrix(PORT16_SAMft)
PORT16_SAM.netDensity <- network.density(PORT16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_SAM.entropy <- entropy(PORT16_SAMft) #entropy

PORT16_SAM.netMx <- cbind(PORT16_SAM.netMx, PORT16_SAM.clusterCoef, PORT16_SAM.degreeCent$centralization,
                          PORT16_SAM.netDensity, PORT16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************
#NA

round = 16
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
PORT16_TAMg2 <- data.frame(PORT16_TAM)
PORT16_TAMg2 <- PORT16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_TAMg2$player1
player2vector <- PORT16_TAMg2$player2
PORT16_TAMg3 <- PORT16_TAMg2
PORT16_TAMg3$p1inp2vec <- is.element(PORT16_TAMg3$player1, player2vector)
PORT16_TAMg3$p2inp1vec <- is.element(PORT16_TAMg3$player2, player1vector)

addPlayer1 <- PORT16_TAMg3[ which(PORT16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT16_TAMg2 <- rbind(PORT16_TAMg2, addPlayer1)

#ROUND 16, AM Turnover graph using weighted edges
PORT16_TAMft <- ftable(PORT16_TAMg2$player1, PORT16_TAMg2$player2)
PORT16_TAMft2 <- as.matrix(PORT16_TAMft)
numRows <- nrow(PORT16_TAMft2)
numCols <- ncol(PORT16_TAMft2)
PORT16_TAMft3 <- PORT16_TAMft2[c(2:numRows) , c(1:numCols)]
PORT16_TAMTable <- graph.adjacency(PORT16_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(PORT16_TAMTable, vertex.label = V(PORT16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
PORT16_TAM.clusterCoef <- transitivity(PORT16_TAMTable, type="global") #cluster coefficient
PORT16_TAM.degreeCent <- centralization.degree(PORT16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_TAMftn <- as.network.matrix(PORT16_TAMft)
PORT16_TAM.netDensity <- network.density(PORT16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_TAM.entropy <- entropy(PORT16_TAMft) #entropy

PORT16_TAM.netMx <- cbind(PORT16_TAM.netMx, PORT16_TAM.clusterCoef, PORT16_TAM.degreeCent$centralization,
                          PORT16_TAM.netDensity, PORT16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************
#NA

round = 16
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
PORT16_SDMg2 <- data.frame(PORT16_SDM)
PORT16_SDMg2 <- PORT16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_SDMg2$player1
player2vector <- PORT16_SDMg2$player2
PORT16_SDMg3 <- PORT16_SDMg2
PORT16_SDMg3$p1inp2vec <- is.element(PORT16_SDMg3$player1, player2vector)
PORT16_SDMg3$p2inp1vec <- is.element(PORT16_SDMg3$player2, player1vector)

addPlayer1 <- PORT16_SDMg3[ which(PORT16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT16_SDMg3[ which(PORT16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT16_SDMg2 <- rbind(PORT16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
PORT16_SDMft <- ftable(PORT16_SDMg2$player1, PORT16_SDMg2$player2)
PORT16_SDMft2 <- as.matrix(PORT16_SDMft)
numRows <- nrow(PORT16_SDMft2)
numCols <- ncol(PORT16_SDMft2)
PORT16_SDMft3 <- PORT16_SDMft2[c(2:numRows) , c(2:numCols)]
PORT16_SDMTable <- graph.adjacency(PORT16_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(PORT16_SDMTable, vertex.label = V(PORT16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
PORT16_SDM.clusterCoef <- transitivity(PORT16_SDMTable, type="global") #cluster coefficient
PORT16_SDM.degreeCent <- centralization.degree(PORT16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_SDMftn <- as.network.matrix(PORT16_SDMft)
PORT16_SDM.netDensity <- network.density(PORT16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_SDM.entropy <- entropy(PORT16_SDMft) #entropy

PORT16_SDM.netMx <- cbind(PORT16_SDM.netMx, PORT16_SDM.clusterCoef, PORT16_SDM.degreeCent$centralization,
                          PORT16_SDM.netDensity, PORT16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
PORT16_TDMg2 <- data.frame(PORT16_TDM)
PORT16_TDMg2 <- PORT16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_TDMg2$player1
player2vector <- PORT16_TDMg2$player2
PORT16_TDMg3 <- PORT16_TDMg2
PORT16_TDMg3$p1inp2vec <- is.element(PORT16_TDMg3$player1, player2vector)
PORT16_TDMg3$p2inp1vec <- is.element(PORT16_TDMg3$player2, player1vector)

addPlayer1 <- PORT16_TDMg3[ which(PORT16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT16_TDMg3[ which(PORT16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT16_TDMg2 <- rbind(PORT16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
PORT16_TDMft <- ftable(PORT16_TDMg2$player1, PORT16_TDMg2$player2)
PORT16_TDMft2 <- as.matrix(PORT16_TDMft)
numRows <- nrow(PORT16_TDMft2)
numCols <- ncol(PORT16_TDMft2)
PORT16_TDMft3 <- PORT16_TDMft2[c(2:numRows) , c(2:numCols)]
PORT16_TDMTable <- graph.adjacency(PORT16_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(PORT16_TDMTable, vertex.label = V(PORT16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
PORT16_TDM.clusterCoef <- transitivity(PORT16_TDMTable, type="global") #cluster coefficient
PORT16_TDM.degreeCent <- centralization.degree(PORT16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_TDMftn <- as.network.matrix(PORT16_TDMft)
PORT16_TDM.netDensity <- network.density(PORT16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_TDM.entropy <- entropy(PORT16_TDMft) #entropy

PORT16_TDM.netMx <- cbind(PORT16_TDM.netMx, PORT16_TDM.clusterCoef, PORT16_TDM.degreeCent$centralization,
                          PORT16_TDM.netDensity, PORT16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
PORT16_SDg2 <- data.frame(PORT16_SD)
PORT16_SDg2 <- PORT16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_SDg2$player1
player2vector <- PORT16_SDg2$player2
PORT16_SDg3 <- PORT16_SDg2
PORT16_SDg3$p1inp2vec <- is.element(PORT16_SDg3$player1, player2vector)
PORT16_SDg3$p2inp1vec <- is.element(PORT16_SDg3$player2, player1vector)

addPlayer1 <- PORT16_SDg3[ which(PORT16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT16_SDg3[ which(PORT16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT16_SDg2 <- rbind(PORT16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
PORT16_SDft <- ftable(PORT16_SDg2$player1, PORT16_SDg2$player2)
PORT16_SDft2 <- as.matrix(PORT16_SDft)
numRows <- nrow(PORT16_SDft2)
numCols <- ncol(PORT16_SDft2)
PORT16_SDft3 <- PORT16_SDft2[c(2:numRows) , c(2:numCols)]
PORT16_SDTable <- graph.adjacency(PORT16_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(PORT16_SDTable, vertex.label = V(PORT16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
PORT16_SD.clusterCoef <- transitivity(PORT16_SDTable, type="global") #cluster coefficient
PORT16_SD.degreeCent <- centralization.degree(PORT16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_SDftn <- as.network.matrix(PORT16_SDft)
PORT16_SD.netDensity <- network.density(PORT16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_SD.entropy <- entropy(PORT16_SDft) #entropy

PORT16_SD.netMx <- cbind(PORT16_SD.netMx, PORT16_SD.clusterCoef, PORT16_SD.degreeCent$centralization,
                         PORT16_SD.netDensity, PORT16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
PORT16_TDg2 <- data.frame(PORT16_TD)
PORT16_TDg2 <- PORT16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_TDg2$player1
player2vector <- PORT16_TDg2$player2
PORT16_TDg3 <- PORT16_TDg2
PORT16_TDg3$p1inp2vec <- is.element(PORT16_TDg3$player1, player2vector)
PORT16_TDg3$p2inp1vec <- is.element(PORT16_TDg3$player2, player1vector)

addPlayer1 <- PORT16_TDg3[ which(PORT16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT16_TDg3[ which(PORT16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT16_TDg2 <- rbind(PORT16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
PORT16_TDft <- ftable(PORT16_TDg2$player1, PORT16_TDg2$player2)
PORT16_TDft2 <- as.matrix(PORT16_TDft)
numRows <- nrow(PORT16_TDft2)
numCols <- ncol(PORT16_TDft2)
PORT16_TDft3 <- PORT16_TDft2[c(2:numRows) , c(2:numCols)]
PORT16_TDTable <- graph.adjacency(PORT16_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(PORT16_TDTable, vertex.label = V(PORT16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
PORT16_TD.clusterCoef <- transitivity(PORT16_TDTable, type="global") #cluster coefficient
PORT16_TD.degreeCent <- centralization.degree(PORT16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_TDftn <- as.network.matrix(PORT16_TDft)
PORT16_TD.netDensity <- network.density(PORT16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_TD.entropy <- entropy(PORT16_TDft) #entropy

PORT16_TD.netMx <- cbind(PORT16_TD.netMx, PORT16_TD.clusterCoef, PORT16_TD.degreeCent$centralization,
                         PORT16_TD.netDensity, PORT16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
PORT16_QTg2 <- data.frame(PORT16_QT)
PORT16_QTg2 <- PORT16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT16_QTg2$player1
player2vector <- PORT16_QTg2$player2
PORT16_QTg3 <- PORT16_QTg2
PORT16_QTg3$p1inp2vec <- is.element(PORT16_QTg3$player1, player2vector)
PORT16_QTg3$p2inp1vec <- is.element(PORT16_QTg3$player2, player1vector)

addPlayer1 <- PORT16_QTg3[ which(PORT16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT16_QTg3[ which(PORT16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT16_QTg2 <- rbind(PORT16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
PORT16_QTft <- ftable(PORT16_QTg2$player1, PORT16_QTg2$player2)
PORT16_QTft2 <- as.matrix(PORT16_QTft)
numRows <- nrow(PORT16_QTft2)
numCols <- ncol(PORT16_QTft2)
PORT16_QTft3 <- PORT16_QTft2[c(2:numRows) , c(2:numCols)]
PORT16_QTTable <- graph.adjacency(PORT16_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(PORT16_QTTable, vertex.label = V(PORT16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
PORT16_QT.clusterCoef <- transitivity(PORT16_QTTable, type="global") #cluster coefficient
PORT16_QT.degreeCent <- centralization.degree(PORT16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT16_QTftn <- as.network.matrix(PORT16_QTft)
PORT16_QT.netDensity <- network.density(PORT16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT16_QT.entropy <- entropy(PORT16_QTft) #entropy

PORT16_QT.netMx <- cbind(PORT16_QT.netMx, PORT16_QT.clusterCoef, PORT16_QT.degreeCent$centralization,
                         PORT16_QT.netDensity, PORT16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT16_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "RICH"
KIoutcome = "Goal_F"
RICH16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
RICH16_Gg2 <- data.frame(RICH16_G)
RICH16_Gg2 <- RICH16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_Gg2$player1
player2vector <- RICH16_Gg2$player2
RICH16_Gg3 <- RICH16_Gg2
RICH16_Gg3$p1inp2vec <- is.element(RICH16_Gg3$player1, player2vector)
RICH16_Gg3$p2inp1vec <- is.element(RICH16_Gg3$player2, player1vector)

addPlayer1 <- RICH16_Gg3[ which(RICH16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_Gg3[ which(RICH16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_Gg2 <- rbind(RICH16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
RICH16_Gft <- ftable(RICH16_Gg2$player1, RICH16_Gg2$player2)
RICH16_Gft2 <- as.matrix(RICH16_Gft)
numRows <- nrow(RICH16_Gft2)
numCols <- ncol(RICH16_Gft2)
RICH16_Gft3 <- RICH16_Gft2[c(2:numRows) , c(2:numCols)]
RICH16_GTable <- graph.adjacency(RICH16_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(RICH16_GTable, vertex.label = V(RICH16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
RICH16_G.clusterCoef <- transitivity(RICH16_GTable, type="global") #cluster coefficient
RICH16_G.degreeCent <- centralization.degree(RICH16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_Gftn <- as.network.matrix(RICH16_Gft)
RICH16_G.netDensity <- network.density(RICH16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_G.entropy <- entropy(RICH16_Gft) #entropy

RICH16_G.netMx <- cbind(RICH16_G.netMx, RICH16_G.clusterCoef, RICH16_G.degreeCent$centralization,
                        RICH16_G.netDensity, RICH16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************

round = 16
teamName = "RICH"
KIoutcome = "Behind_F"
RICH16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
RICH16_Bg2 <- data.frame(RICH16_B)
RICH16_Bg2 <- RICH16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_Bg2$player1
player2vector <- RICH16_Bg2$player2
RICH16_Bg3 <- RICH16_Bg2
RICH16_Bg3$p1inp2vec <- is.element(RICH16_Bg3$player1, player2vector)
RICH16_Bg3$p2inp1vec <- is.element(RICH16_Bg3$player2, player1vector)

addPlayer1 <- RICH16_Bg3[ which(RICH16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_Bg3[ which(RICH16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_Bg2 <- rbind(RICH16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
RICH16_Bft <- ftable(RICH16_Bg2$player1, RICH16_Bg2$player2)
RICH16_Bft2 <- as.matrix(RICH16_Bft)
numRows <- nrow(RICH16_Bft2)
numCols <- ncol(RICH16_Bft2)
RICH16_Bft3 <- RICH16_Bft2[c(2:numRows) , c(2:numCols)]
RICH16_BTable <- graph.adjacency(RICH16_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(RICH16_BTable, vertex.label = V(RICH16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
RICH16_B.clusterCoef <- transitivity(RICH16_BTable, type="global") #cluster coefficient
RICH16_B.degreeCent <- centralization.degree(RICH16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_Bftn <- as.network.matrix(RICH16_Bft)
RICH16_B.netDensity <- network.density(RICH16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_B.entropy <- entropy(RICH16_Bft) #entropy

RICH16_B.netMx <- cbind(RICH16_B.netMx, RICH16_B.clusterCoef, RICH16_B.degreeCent$centralization,
                        RICH16_B.netDensity, RICH16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
RICH16_SFg2 <- data.frame(RICH16_SF)
RICH16_SFg2 <- RICH16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_SFg2$player1
player2vector <- RICH16_SFg2$player2
RICH16_SFg3 <- RICH16_SFg2
RICH16_SFg3$p1inp2vec <- is.element(RICH16_SFg3$player1, player2vector)
RICH16_SFg3$p2inp1vec <- is.element(RICH16_SFg3$player2, player1vector)

addPlayer1 <- RICH16_SFg3[ which(RICH16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_SFg3[ which(RICH16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_SFg2 <- rbind(RICH16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
RICH16_SFft <- ftable(RICH16_SFg2$player1, RICH16_SFg2$player2)
RICH16_SFft2 <- as.matrix(RICH16_SFft)
numRows <- nrow(RICH16_SFft2)
numCols <- ncol(RICH16_SFft2)
RICH16_SFft3 <- RICH16_SFft2[c(2:numRows) , c(2:numCols)]
RICH16_SFTable <- graph.adjacency(RICH16_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(RICH16_SFTable, vertex.label = V(RICH16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
RICH16_SF.clusterCoef <- transitivity(RICH16_SFTable, type="global") #cluster coefficient
RICH16_SF.degreeCent <- centralization.degree(RICH16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_SFftn <- as.network.matrix(RICH16_SFft)
RICH16_SF.netDensity <- network.density(RICH16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_SF.entropy <- entropy(RICH16_SFft) #entropy

RICH16_SF.netMx <- cbind(RICH16_SF.netMx, RICH16_SF.clusterCoef, RICH16_SF.degreeCent$centralization,
                         RICH16_SF.netDensity, RICH16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
RICH16_TFg2 <- data.frame(RICH16_TF)
RICH16_TFg2 <- RICH16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_TFg2$player1
player2vector <- RICH16_TFg2$player2
RICH16_TFg3 <- RICH16_TFg2
RICH16_TFg3$p1inp2vec <- is.element(RICH16_TFg3$player1, player2vector)
RICH16_TFg3$p2inp1vec <- is.element(RICH16_TFg3$player2, player1vector)

addPlayer1 <- RICH16_TFg3[ which(RICH16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_TFg3[ which(RICH16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_TFg2 <- rbind(RICH16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
RICH16_TFft <- ftable(RICH16_TFg2$player1, RICH16_TFg2$player2)
RICH16_TFft2 <- as.matrix(RICH16_TFft)
numRows <- nrow(RICH16_TFft2)
numCols <- ncol(RICH16_TFft2)
RICH16_TFft3 <- RICH16_TFft2[c(2:numRows) , c(2:numCols)]
RICH16_TFTable <- graph.adjacency(RICH16_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(RICH16_TFTable, vertex.label = V(RICH16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
RICH16_TF.clusterCoef <- transitivity(RICH16_TFTable, type="global") #cluster coefficient
RICH16_TF.degreeCent <- centralization.degree(RICH16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_TFftn <- as.network.matrix(RICH16_TFft)
RICH16_TF.netDensity <- network.density(RICH16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_TF.entropy <- entropy(RICH16_TFft) #entropy

RICH16_TF.netMx <- cbind(RICH16_TF.netMx, RICH16_TF.clusterCoef, RICH16_TF.degreeCent$centralization,
                         RICH16_TF.netDensity, RICH16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
RICH16_SAMg2 <- data.frame(RICH16_SAM)
RICH16_SAMg2 <- RICH16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_SAMg2$player1
player2vector <- RICH16_SAMg2$player2
RICH16_SAMg3 <- RICH16_SAMg2
RICH16_SAMg3$p1inp2vec <- is.element(RICH16_SAMg3$player1, player2vector)
RICH16_SAMg3$p2inp1vec <- is.element(RICH16_SAMg3$player2, player1vector)

addPlayer1 <- RICH16_SAMg3[ which(RICH16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_SAMg3[ which(RICH16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_SAMg2 <- rbind(RICH16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
RICH16_SAMft <- ftable(RICH16_SAMg2$player1, RICH16_SAMg2$player2)
RICH16_SAMft2 <- as.matrix(RICH16_SAMft)
numRows <- nrow(RICH16_SAMft2)
numCols <- ncol(RICH16_SAMft2)
RICH16_SAMft3 <- RICH16_SAMft2[c(2:numRows) , c(2:numCols)]
RICH16_SAMTable <- graph.adjacency(RICH16_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(RICH16_SAMTable, vertex.label = V(RICH16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
RICH16_SAM.clusterCoef <- transitivity(RICH16_SAMTable, type="global") #cluster coefficient
RICH16_SAM.degreeCent <- centralization.degree(RICH16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_SAMftn <- as.network.matrix(RICH16_SAMft)
RICH16_SAM.netDensity <- network.density(RICH16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_SAM.entropy <- entropy(RICH16_SAMft) #entropy

RICH16_SAM.netMx <- cbind(RICH16_SAM.netMx, RICH16_SAM.clusterCoef, RICH16_SAM.degreeCent$centralization,
                          RICH16_SAM.netDensity, RICH16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
RICH16_TAMg2 <- data.frame(RICH16_TAM)
RICH16_TAMg2 <- RICH16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_TAMg2$player1
player2vector <- RICH16_TAMg2$player2
RICH16_TAMg3 <- RICH16_TAMg2
RICH16_TAMg3$p1inp2vec <- is.element(RICH16_TAMg3$player1, player2vector)
RICH16_TAMg3$p2inp1vec <- is.element(RICH16_TAMg3$player2, player1vector)

addPlayer1 <- RICH16_TAMg3[ which(RICH16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_TAMg3[ which(RICH16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_TAMg2 <- rbind(RICH16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
RICH16_TAMft <- ftable(RICH16_TAMg2$player1, RICH16_TAMg2$player2)
RICH16_TAMft2 <- as.matrix(RICH16_TAMft)
numRows <- nrow(RICH16_TAMft2)
numCols <- ncol(RICH16_TAMft2)
RICH16_TAMft3 <- RICH16_TAMft2[c(2:numRows) , c(2:numCols)]
RICH16_TAMTable <- graph.adjacency(RICH16_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(RICH16_TAMTable, vertex.label = V(RICH16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
RICH16_TAM.clusterCoef <- transitivity(RICH16_TAMTable, type="global") #cluster coefficient
RICH16_TAM.degreeCent <- centralization.degree(RICH16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_TAMftn <- as.network.matrix(RICH16_TAMft)
RICH16_TAM.netDensity <- network.density(RICH16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_TAM.entropy <- entropy(RICH16_TAMft) #entropy

RICH16_TAM.netMx <- cbind(RICH16_TAM.netMx, RICH16_TAM.clusterCoef, RICH16_TAM.degreeCent$centralization,
                          RICH16_TAM.netDensity, RICH16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
RICH16_SDMg2 <- data.frame(RICH16_SDM)
RICH16_SDMg2 <- RICH16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_SDMg2$player1
player2vector <- RICH16_SDMg2$player2
RICH16_SDMg3 <- RICH16_SDMg2
RICH16_SDMg3$p1inp2vec <- is.element(RICH16_SDMg3$player1, player2vector)
RICH16_SDMg3$p2inp1vec <- is.element(RICH16_SDMg3$player2, player1vector)

addPlayer1 <- RICH16_SDMg3[ which(RICH16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_SDMg3[ which(RICH16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_SDMg2 <- rbind(RICH16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
RICH16_SDMft <- ftable(RICH16_SDMg2$player1, RICH16_SDMg2$player2)
RICH16_SDMft2 <- as.matrix(RICH16_SDMft)
numRows <- nrow(RICH16_SDMft2)
numCols <- ncol(RICH16_SDMft2)
RICH16_SDMft3 <- RICH16_SDMft2[c(2:numRows) , c(2:numCols)]
RICH16_SDMTable <- graph.adjacency(RICH16_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(RICH16_SDMTable, vertex.label = V(RICH16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
RICH16_SDM.clusterCoef <- transitivity(RICH16_SDMTable, type="global") #cluster coefficient
RICH16_SDM.degreeCent <- centralization.degree(RICH16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_SDMftn <- as.network.matrix(RICH16_SDMft)
RICH16_SDM.netDensity <- network.density(RICH16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_SDM.entropy <- entropy(RICH16_SDMft) #entropy

RICH16_SDM.netMx <- cbind(RICH16_SDM.netMx, RICH16_SDM.clusterCoef, RICH16_SDM.degreeCent$centralization,
                          RICH16_SDM.netDensity, RICH16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
RICH16_TDMg2 <- data.frame(RICH16_TDM)
RICH16_TDMg2 <- RICH16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_TDMg2$player1
player2vector <- RICH16_TDMg2$player2
RICH16_TDMg3 <- RICH16_TDMg2
RICH16_TDMg3$p1inp2vec <- is.element(RICH16_TDMg3$player1, player2vector)
RICH16_TDMg3$p2inp1vec <- is.element(RICH16_TDMg3$player2, player1vector)

addPlayer1 <- RICH16_TDMg3[ which(RICH16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_TDMg3[ which(RICH16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_TDMg2 <- rbind(RICH16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
RICH16_TDMft <- ftable(RICH16_TDMg2$player1, RICH16_TDMg2$player2)
RICH16_TDMft2 <- as.matrix(RICH16_TDMft)
numRows <- nrow(RICH16_TDMft2)
numCols <- ncol(RICH16_TDMft2)
RICH16_TDMft3 <- RICH16_TDMft2[c(2:numRows) , c(2:numCols)]
RICH16_TDMTable <- graph.adjacency(RICH16_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(RICH16_TDMTable, vertex.label = V(RICH16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
RICH16_TDM.clusterCoef <- transitivity(RICH16_TDMTable, type="global") #cluster coefficient
RICH16_TDM.degreeCent <- centralization.degree(RICH16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_TDMftn <- as.network.matrix(RICH16_TDMft)
RICH16_TDM.netDensity <- network.density(RICH16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_TDM.entropy <- entropy(RICH16_TDMft) #entropy

RICH16_TDM.netMx <- cbind(RICH16_TDM.netMx, RICH16_TDM.clusterCoef, RICH16_TDM.degreeCent$centralization,
                          RICH16_TDM.netDensity, RICH16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
RICH16_SDg2 <- data.frame(RICH16_SD)
RICH16_SDg2 <- RICH16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_SDg2$player1
player2vector <- RICH16_SDg2$player2
RICH16_SDg3 <- RICH16_SDg2
RICH16_SDg3$p1inp2vec <- is.element(RICH16_SDg3$player1, player2vector)
RICH16_SDg3$p2inp1vec <- is.element(RICH16_SDg3$player2, player1vector)

addPlayer1 <- RICH16_SDg3[ which(RICH16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_SDg3[ which(RICH16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_SDg2 <- rbind(RICH16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
RICH16_SDft <- ftable(RICH16_SDg2$player1, RICH16_SDg2$player2)
RICH16_SDft2 <- as.matrix(RICH16_SDft)
numRows <- nrow(RICH16_SDft2)
numCols <- ncol(RICH16_SDft2)
RICH16_SDft3 <- RICH16_SDft2[c(2:numRows) , c(2:numCols)]
RICH16_SDTable <- graph.adjacency(RICH16_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(RICH16_SDTable, vertex.label = V(RICH16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
RICH16_SD.clusterCoef <- transitivity(RICH16_SDTable, type="global") #cluster coefficient
RICH16_SD.degreeCent <- centralization.degree(RICH16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_SDftn <- as.network.matrix(RICH16_SDft)
RICH16_SD.netDensity <- network.density(RICH16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_SD.entropy <- entropy(RICH16_SDft) #entropy

RICH16_SD.netMx <- cbind(RICH16_SD.netMx, RICH16_SD.clusterCoef, RICH16_SD.degreeCent$centralization,
                         RICH16_SD.netDensity, RICH16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
RICH16_TDg2 <- data.frame(RICH16_TD)
RICH16_TDg2 <- RICH16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_TDg2$player1
player2vector <- RICH16_TDg2$player2
RICH16_TDg3 <- RICH16_TDg2
RICH16_TDg3$p1inp2vec <- is.element(RICH16_TDg3$player1, player2vector)
RICH16_TDg3$p2inp1vec <- is.element(RICH16_TDg3$player2, player1vector)

addPlayer1 <- RICH16_TDg3[ which(RICH16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_TDg3[ which(RICH16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_TDg2 <- rbind(RICH16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
RICH16_TDft <- ftable(RICH16_TDg2$player1, RICH16_TDg2$player2)
RICH16_TDft2 <- as.matrix(RICH16_TDft)
numRows <- nrow(RICH16_TDft2)
numCols <- ncol(RICH16_TDft2)
RICH16_TDft3 <- RICH16_TDft2[c(2:numRows) , c(2:numCols)]
RICH16_TDTable <- graph.adjacency(RICH16_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(RICH16_TDTable, vertex.label = V(RICH16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
RICH16_TD.clusterCoef <- transitivity(RICH16_TDTable, type="global") #cluster coefficient
RICH16_TD.degreeCent <- centralization.degree(RICH16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_TDftn <- as.network.matrix(RICH16_TDft)
RICH16_TD.netDensity <- network.density(RICH16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_TD.entropy <- entropy(RICH16_TDft) #entropy

RICH16_TD.netMx <- cbind(RICH16_TD.netMx, RICH16_TD.clusterCoef, RICH16_TD.degreeCent$centralization,
                         RICH16_TD.netDensity, RICH16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
RICH16_QTg2 <- data.frame(RICH16_QT)
RICH16_QTg2 <- RICH16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH16_QTg2$player1
player2vector <- RICH16_QTg2$player2
RICH16_QTg3 <- RICH16_QTg2
RICH16_QTg3$p1inp2vec <- is.element(RICH16_QTg3$player1, player2vector)
RICH16_QTg3$p2inp1vec <- is.element(RICH16_QTg3$player2, player1vector)

addPlayer1 <- RICH16_QTg3[ which(RICH16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH16_QTg3[ which(RICH16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH16_QTg2 <- rbind(RICH16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
RICH16_QTft <- ftable(RICH16_QTg2$player1, RICH16_QTg2$player2)
RICH16_QTft2 <- as.matrix(RICH16_QTft)
numRows <- nrow(RICH16_QTft2)
numCols <- ncol(RICH16_QTft2)
RICH16_QTft3 <- RICH16_QTft2[c(2:numRows) , c(2:numCols)]
RICH16_QTTable <- graph.adjacency(RICH16_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(RICH16_QTTable, vertex.label = V(RICH16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
RICH16_QT.clusterCoef <- transitivity(RICH16_QTTable, type="global") #cluster coefficient
RICH16_QT.degreeCent <- centralization.degree(RICH16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH16_QTftn <- as.network.matrix(RICH16_QTft)
RICH16_QT.netDensity <- network.density(RICH16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH16_QT.entropy <- entropy(RICH16_QTft) #entropy

RICH16_QT.netMx <- cbind(RICH16_QT.netMx, RICH16_QT.clusterCoef, RICH16_QT.degreeCent$centralization,
                         RICH16_QT.netDensity, RICH16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH16_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************

round = 16
teamName = "STK"
KIoutcome = "Goal_F"
STK16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
STK16_Gg2 <- data.frame(STK16_G)
STK16_Gg2 <- STK16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_Gg2$player1
player2vector <- STK16_Gg2$player2
STK16_Gg3 <- STK16_Gg2
STK16_Gg3$p1inp2vec <- is.element(STK16_Gg3$player1, player2vector)
STK16_Gg3$p2inp1vec <- is.element(STK16_Gg3$player2, player1vector)

addPlayer1 <- STK16_Gg3[ which(STK16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK16_Gg3[ which(STK16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_Gg2 <- rbind(STK16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
STK16_Gft <- ftable(STK16_Gg2$player1, STK16_Gg2$player2)
STK16_Gft2 <- as.matrix(STK16_Gft)
numRows <- nrow(STK16_Gft2)
numCols <- ncol(STK16_Gft2)
STK16_Gft3 <- STK16_Gft2[c(2:numRows) , c(2:numCols)]
STK16_GTable <- graph.adjacency(STK16_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(STK16_GTable, vertex.label = V(STK16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
STK16_G.clusterCoef <- transitivity(STK16_GTable, type="global") #cluster coefficient
STK16_G.degreeCent <- centralization.degree(STK16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_Gftn <- as.network.matrix(STK16_Gft)
STK16_G.netDensity <- network.density(STK16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_G.entropy <- entropy(STK16_Gft) #entropy

STK16_G.netMx <- cbind(STK16_G.netMx, STK16_G.clusterCoef, STK16_G.degreeCent$centralization,
                       STK16_G.netDensity, STK16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "STK"
KIoutcome = "Behind_F"
STK16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
STK16_Bg2 <- data.frame(STK16_B)
STK16_Bg2 <- STK16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_Bg2$player1
player2vector <- STK16_Bg2$player2
STK16_Bg3 <- STK16_Bg2
STK16_Bg3$p1inp2vec <- is.element(STK16_Bg3$player1, player2vector)
STK16_Bg3$p2inp1vec <- is.element(STK16_Bg3$player2, player1vector)

addPlayer1 <- STK16_Bg3[ which(STK16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK16_Bg3[ which(STK16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_Bg2 <- rbind(STK16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
STK16_Bft <- ftable(STK16_Bg2$player1, STK16_Bg2$player2)
STK16_Bft2 <- as.matrix(STK16_Bft)
numRows <- nrow(STK16_Bft2)
numCols <- ncol(STK16_Bft2)
STK16_Bft3 <- STK16_Bft2[c(2:numRows) , c(2:numCols)]
STK16_BTable <- graph.adjacency(STK16_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(STK16_BTable, vertex.label = V(STK16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
STK16_B.clusterCoef <- transitivity(STK16_BTable, type="global") #cluster coefficient
STK16_B.degreeCent <- centralization.degree(STK16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_Bftn <- as.network.matrix(STK16_Bft)
STK16_B.netDensity <- network.density(STK16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_B.entropy <- entropy(STK16_Bft) #entropy

STK16_B.netMx <- cbind(STK16_B.netMx, STK16_B.clusterCoef, STK16_B.degreeCent$centralization,
                       STK16_B.netDensity, STK16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "STK"
KIoutcome = "Stoppage_F"
STK16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
STK16_SFg2 <- data.frame(STK16_SF)
STK16_SFg2 <- STK16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_SFg2$player1
player2vector <- STK16_SFg2$player2
STK16_SFg3 <- STK16_SFg2
STK16_SFg3$p1inp2vec <- is.element(STK16_SFg3$player1, player2vector)
STK16_SFg3$p2inp1vec <- is.element(STK16_SFg3$player2, player1vector)

addPlayer1 <- STK16_SFg3[ which(STK16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK16_SFg3[ which(STK16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_SFg2 <- rbind(STK16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
STK16_SFft <- ftable(STK16_SFg2$player1, STK16_SFg2$player2)
STK16_SFft2 <- as.matrix(STK16_SFft)
numRows <- nrow(STK16_SFft2)
numCols <- ncol(STK16_SFft2)
STK16_SFft3 <- STK16_SFft2[c(2:numRows) , c(2:numCols)]
STK16_SFTable <- graph.adjacency(STK16_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(STK16_SFTable, vertex.label = V(STK16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
STK16_SF.clusterCoef <- transitivity(STK16_SFTable, type="global") #cluster coefficient
STK16_SF.degreeCent <- centralization.degree(STK16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_SFftn <- as.network.matrix(STK16_SFft)
STK16_SF.netDensity <- network.density(STK16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_SF.entropy <- entropy(STK16_SFft) #entropy

STK16_SF.netMx <- cbind(STK16_SF.netMx, STK16_SF.clusterCoef, STK16_SF.degreeCent$centralization,
                        STK16_SF.netDensity, STK16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "STK"
KIoutcome = "Turnover_F"
STK16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
STK16_TFg2 <- data.frame(STK16_TF)
STK16_TFg2 <- STK16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_TFg2$player1
player2vector <- STK16_TFg2$player2
STK16_TFg3 <- STK16_TFg2
STK16_TFg3$p1inp2vec <- is.element(STK16_TFg3$player1, player2vector)
STK16_TFg3$p2inp1vec <- is.element(STK16_TFg3$player2, player1vector)

addPlayer1 <- STK16_TFg3[ which(STK16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK16_TFg3[ which(STK16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_TFg2 <- rbind(STK16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
STK16_TFft <- ftable(STK16_TFg2$player1, STK16_TFg2$player2)
STK16_TFft2 <- as.matrix(STK16_TFft)
numRows <- nrow(STK16_TFft2)
numCols <- ncol(STK16_TFft2)
STK16_TFft3 <- STK16_TFft2[c(2:numRows) , c(2:numCols)]
STK16_TFTable <- graph.adjacency(STK16_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(STK16_TFTable, vertex.label = V(STK16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
STK16_TF.clusterCoef <- transitivity(STK16_TFTable, type="global") #cluster coefficient
STK16_TF.degreeCent <- centralization.degree(STK16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_TFftn <- as.network.matrix(STK16_TFft)
STK16_TF.netDensity <- network.density(STK16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_TF.entropy <- entropy(STK16_TFft) #entropy

STK16_TF.netMx <- cbind(STK16_TF.netMx, STK16_TF.clusterCoef, STK16_TF.degreeCent$centralization,
                        STK16_TF.netDensity, STK16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
STK16_SAMg2 <- data.frame(STK16_SAM)
STK16_SAMg2 <- STK16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_SAMg2$player1
player2vector <- STK16_SAMg2$player2
STK16_SAMg3 <- STK16_SAMg2
STK16_SAMg3$p1inp2vec <- is.element(STK16_SAMg3$player1, player2vector)
STK16_SAMg3$p2inp1vec <- is.element(STK16_SAMg3$player2, player1vector)

addPlayer1 <- STK16_SAMg3[ which(STK16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK16_SAMg3[ which(STK16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_SAMg2 <- rbind(STK16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
STK16_SAMft <- ftable(STK16_SAMg2$player1, STK16_SAMg2$player2)
STK16_SAMft2 <- as.matrix(STK16_SAMft)
numRows <- nrow(STK16_SAMft2)
numCols <- ncol(STK16_SAMft2)
STK16_SAMft3 <- STK16_SAMft2[c(2:numRows) , c(2:numCols)]
STK16_SAMTable <- graph.adjacency(STK16_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(STK16_SAMTable, vertex.label = V(STK16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
STK16_SAM.clusterCoef <- transitivity(STK16_SAMTable, type="global") #cluster coefficient
STK16_SAM.degreeCent <- centralization.degree(STK16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_SAMftn <- as.network.matrix(STK16_SAMft)
STK16_SAM.netDensity <- network.density(STK16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_SAM.entropy <- entropy(STK16_SAMft) #entropy

STK16_SAM.netMx <- cbind(STK16_SAM.netMx, STK16_SAM.clusterCoef, STK16_SAM.degreeCent$centralization,
                         STK16_SAM.netDensity, STK16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************
#NA

round = 16
teamName = "STK"
KIoutcome = "Turnover_AM"
STK16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
STK16_TAMg2 <- data.frame(STK16_TAM)
STK16_TAMg2 <- STK16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_TAMg2$player1
player2vector <- STK16_TAMg2$player2
STK16_TAMg3 <- STK16_TAMg2
STK16_TAMg3$p1inp2vec <- is.element(STK16_TAMg3$player1, player2vector)
STK16_TAMg3$p2inp1vec <- is.element(STK16_TAMg3$player2, player1vector)

addPlayer1 <- STK16_TAMg3[ which(STK16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK16_TAMg3[ which(STK16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_TAMg2 <- rbind(STK16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
STK16_TAMft <- ftable(STK16_TAMg2$player1, STK16_TAMg2$player2)
STK16_TAMft2 <- as.matrix(STK16_TAMft)
numRows <- nrow(STK16_TAMft2)
numCols <- ncol(STK16_TAMft2)
STK16_TAMft3 <- STK16_TAMft2[c(1:numRows) , c(1:numCols)]
STK16_TAMTable <- graph.adjacency(STK16_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(STK16_TAMTable, vertex.label = V(STK16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
STK16_TAM.clusterCoef <- transitivity(STK16_TAMTable, type="global") #cluster coefficient
STK16_TAM.degreeCent <- centralization.degree(STK16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_TAMftn <- as.network.matrix(STK16_TAMft)
STK16_TAM.netDensity <- network.density(STK16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_TAM.entropy <- entropy(STK16_TAMft) #entropy

STK16_TAM.netMx <- cbind(STK16_TAM.netMx, STK16_TAM.clusterCoef, STK16_TAM.degreeCent$centralization,
                         STK16_TAM.netDensity, STK16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
STK16_SDMg2 <- data.frame(STK16_SDM)
STK16_SDMg2 <- STK16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_SDMg2$player1
player2vector <- STK16_SDMg2$player2
STK16_SDMg3 <- STK16_SDMg2
STK16_SDMg3$p1inp2vec <- is.element(STK16_SDMg3$player1, player2vector)
STK16_SDMg3$p2inp1vec <- is.element(STK16_SDMg3$player2, player1vector)

addPlayer1 <- STK16_SDMg3[ which(STK16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK16_SDMg3[ which(STK16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_SDMg2 <- rbind(STK16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
STK16_SDMft <- ftable(STK16_SDMg2$player1, STK16_SDMg2$player2)
STK16_SDMft2 <- as.matrix(STK16_SDMft)
numRows <- nrow(STK16_SDMft2)
numCols <- ncol(STK16_SDMft2)
STK16_SDMft3 <- STK16_SDMft2[c(2:numRows) , c(2:numCols)]
STK16_SDMTable <- graph.adjacency(STK16_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(STK16_SDMTable, vertex.label = V(STK16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
STK16_SDM.clusterCoef <- transitivity(STK16_SDMTable, type="global") #cluster coefficient
STK16_SDM.degreeCent <- centralization.degree(STK16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_SDMftn <- as.network.matrix(STK16_SDMft)
STK16_SDM.netDensity <- network.density(STK16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_SDM.entropy <- entropy(STK16_SDMft) #entropy

STK16_SDM.netMx <- cbind(STK16_SDM.netMx, STK16_SDM.clusterCoef, STK16_SDM.degreeCent$centralization,
                         STK16_SDM.netDensity, STK16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "STK"
KIoutcome = "Turnover_DM"
STK16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
STK16_TDMg2 <- data.frame(STK16_TDM)
STK16_TDMg2 <- STK16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_TDMg2$player1
player2vector <- STK16_TDMg2$player2
STK16_TDMg3 <- STK16_TDMg2
STK16_TDMg3$p1inp2vec <- is.element(STK16_TDMg3$player1, player2vector)
STK16_TDMg3$p2inp1vec <- is.element(STK16_TDMg3$player2, player1vector)

addPlayer1 <- STK16_TDMg3[ which(STK16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK16_TDMg3[ which(STK16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_TDMg2 <- rbind(STK16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
STK16_TDMft <- ftable(STK16_TDMg2$player1, STK16_TDMg2$player2)
STK16_TDMft2 <- as.matrix(STK16_TDMft)
numRows <- nrow(STK16_TDMft2)
numCols <- ncol(STK16_TDMft2)
STK16_TDMft3 <- STK16_TDMft2[c(2:numRows) , c(2:numCols)]
STK16_TDMTable <- graph.adjacency(STK16_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(STK16_TDMTable, vertex.label = V(STK16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
STK16_TDM.clusterCoef <- transitivity(STK16_TDMTable, type="global") #cluster coefficient
STK16_TDM.degreeCent <- centralization.degree(STK16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_TDMftn <- as.network.matrix(STK16_TDMft)
STK16_TDM.netDensity <- network.density(STK16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_TDM.entropy <- entropy(STK16_TDMft) #entropy

STK16_TDM.netMx <- cbind(STK16_TDM.netMx, STK16_TDM.clusterCoef, STK16_TDM.degreeCent$centralization,
                         STK16_TDM.netDensity, STK16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "STK"
KIoutcome = "Stoppage_D"
STK16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
STK16_SDg2 <- data.frame(STK16_SD)
STK16_SDg2 <- STK16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_SDg2$player1
player2vector <- STK16_SDg2$player2
STK16_SDg3 <- STK16_SDg2
STK16_SDg3$p1inp2vec <- is.element(STK16_SDg3$player1, player2vector)
STK16_SDg3$p2inp1vec <- is.element(STK16_SDg3$player2, player1vector)

addPlayer1 <- STK16_SDg3[ which(STK16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK16_SDg3[ which(STK16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_SDg2 <- rbind(STK16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
STK16_SDft <- ftable(STK16_SDg2$player1, STK16_SDg2$player2)
STK16_SDft2 <- as.matrix(STK16_SDft)
numRows <- nrow(STK16_SDft2)
numCols <- ncol(STK16_SDft2)
STK16_SDft3 <- STK16_SDft2[c(2:numRows) , c(2:numCols)]
STK16_SDTable <- graph.adjacency(STK16_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(STK16_SDTable, vertex.label = V(STK16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
STK16_SD.clusterCoef <- transitivity(STK16_SDTable, type="global") #cluster coefficient
STK16_SD.degreeCent <- centralization.degree(STK16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_SDftn <- as.network.matrix(STK16_SDft)
STK16_SD.netDensity <- network.density(STK16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_SD.entropy <- entropy(STK16_SDft) #entropy

STK16_SD.netMx <- cbind(STK16_SD.netMx, STK16_SD.clusterCoef, STK16_SD.degreeCent$centralization,
                        STK16_SD.netDensity, STK16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************

round = 16
teamName = "STK"
KIoutcome = "Turnover_D"
STK16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
STK16_TDg2 <- data.frame(STK16_TD)
STK16_TDg2 <- STK16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_TDg2$player1
player2vector <- STK16_TDg2$player2
STK16_TDg3 <- STK16_TDg2
STK16_TDg3$p1inp2vec <- is.element(STK16_TDg3$player1, player2vector)
STK16_TDg3$p2inp1vec <- is.element(STK16_TDg3$player2, player1vector)

addPlayer1 <- STK16_TDg3[ which(STK16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK16_TDg3[ which(STK16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_TDg2 <- rbind(STK16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
STK16_TDft <- ftable(STK16_TDg2$player1, STK16_TDg2$player2)
STK16_TDft2 <- as.matrix(STK16_TDft)
numRows <- nrow(STK16_TDft2)
numCols <- ncol(STK16_TDft2)
STK16_TDft3 <- STK16_TDft2[c(2:numRows) , c(2:numCols)]
STK16_TDTable <- graph.adjacency(STK16_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(STK16_TDTable, vertex.label = V(STK16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
STK16_TD.clusterCoef <- transitivity(STK16_TDTable, type="global") #cluster coefficient
STK16_TD.degreeCent <- centralization.degree(STK16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_TDftn <- as.network.matrix(STK16_TDft)
STK16_TD.netDensity <- network.density(STK16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_TD.entropy <- entropy(STK16_TDft) #entropy

STK16_TD.netMx <- cbind(STK16_TD.netMx, STK16_TD.clusterCoef, STK16_TD.degreeCent$centralization,
                        STK16_TD.netDensity, STK16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
STK16_QTg2 <- data.frame(STK16_QT)
STK16_QTg2 <- STK16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK16_QTg2$player1
player2vector <- STK16_QTg2$player2
STK16_QTg3 <- STK16_QTg2
STK16_QTg3$p1inp2vec <- is.element(STK16_QTg3$player1, player2vector)
STK16_QTg3$p2inp1vec <- is.element(STK16_QTg3$player2, player1vector)

addPlayer1 <- STK16_QTg3[ which(STK16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK16_QTg3[ which(STK16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK16_QTg2 <- rbind(STK16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
STK16_QTft <- ftable(STK16_QTg2$player1, STK16_QTg2$player2)
STK16_QTft2 <- as.matrix(STK16_QTft)
numRows <- nrow(STK16_QTft2)
numCols <- ncol(STK16_QTft2)
STK16_QTft3 <- STK16_QTft2[c(2:numRows) , c(2:numCols)]
STK16_QTTable <- graph.adjacency(STK16_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(STK16_QTTable, vertex.label = V(STK16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
STK16_QT.clusterCoef <- transitivity(STK16_QTTable, type="global") #cluster coefficient
STK16_QT.degreeCent <- centralization.degree(STK16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK16_QTftn <- as.network.matrix(STK16_QTft)
STK16_QT.netDensity <- network.density(STK16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK16_QT.entropy <- entropy(STK16_QTft) #entropy

STK16_QT.netMx <- cbind(STK16_QT.netMx, STK16_QT.clusterCoef, STK16_QT.degreeCent$centralization,
                        STK16_QT.netDensity, STK16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK16_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "SYD"
KIoutcome = "Goal_F"
SYD16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
SYD16_Gg2 <- data.frame(SYD16_G)
SYD16_Gg2 <- SYD16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_Gg2$player1
player2vector <- SYD16_Gg2$player2
SYD16_Gg3 <- SYD16_Gg2
SYD16_Gg3$p1inp2vec <- is.element(SYD16_Gg3$player1, player2vector)
SYD16_Gg3$p2inp1vec <- is.element(SYD16_Gg3$player2, player1vector)

addPlayer1 <- SYD16_Gg3[ which(SYD16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_Gg3[ which(SYD16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_Gg2 <- rbind(SYD16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
SYD16_Gft <- ftable(SYD16_Gg2$player1, SYD16_Gg2$player2)
SYD16_Gft2 <- as.matrix(SYD16_Gft)
numRows <- nrow(SYD16_Gft2)
numCols <- ncol(SYD16_Gft2)
SYD16_Gft3 <- SYD16_Gft2[c(2:numRows) , c(2:numCols)]
SYD16_GTable <- graph.adjacency(SYD16_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(SYD16_GTable, vertex.label = V(SYD16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
SYD16_G.clusterCoef <- transitivity(SYD16_GTable, type="global") #cluster coefficient
SYD16_G.degreeCent <- centralization.degree(SYD16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_Gftn <- as.network.matrix(SYD16_Gft)
SYD16_G.netDensity <- network.density(SYD16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_G.entropy <- entropy(SYD16_Gft) #entropy

SYD16_G.netMx <- cbind(SYD16_G.netMx, SYD16_G.clusterCoef, SYD16_G.degreeCent$centralization,
                       SYD16_G.netDensity, SYD16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "SYD"
KIoutcome = "Behind_F"
SYD16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
SYD16_Bg2 <- data.frame(SYD16_B)
SYD16_Bg2 <- SYD16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_Bg2$player1
player2vector <- SYD16_Bg2$player2
SYD16_Bg3 <- SYD16_Bg2
SYD16_Bg3$p1inp2vec <- is.element(SYD16_Bg3$player1, player2vector)
SYD16_Bg3$p2inp1vec <- is.element(SYD16_Bg3$player2, player1vector)

addPlayer1 <- SYD16_Bg3[ which(SYD16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_Bg3[ which(SYD16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_Bg2 <- rbind(SYD16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
SYD16_Bft <- ftable(SYD16_Bg2$player1, SYD16_Bg2$player2)
SYD16_Bft2 <- as.matrix(SYD16_Bft)
numRows <- nrow(SYD16_Bft2)
numCols <- ncol(SYD16_Bft2)
SYD16_Bft3 <- SYD16_Bft2[c(2:numRows) , c(2:numCols)]
SYD16_BTable <- graph.adjacency(SYD16_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(SYD16_BTable, vertex.label = V(SYD16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
SYD16_B.clusterCoef <- transitivity(SYD16_BTable, type="global") #cluster coefficient
SYD16_B.degreeCent <- centralization.degree(SYD16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_Bftn <- as.network.matrix(SYD16_Bft)
SYD16_B.netDensity <- network.density(SYD16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_B.entropy <- entropy(SYD16_Bft) #entropy

SYD16_B.netMx <- cbind(SYD16_B.netMx, SYD16_B.clusterCoef, SYD16_B.degreeCent$centralization,
                       SYD16_B.netDensity, SYD16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************

round = 16
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
SYD16_SFg2 <- data.frame(SYD16_SF)
SYD16_SFg2 <- SYD16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_SFg2$player1
player2vector <- SYD16_SFg2$player2
SYD16_SFg3 <- SYD16_SFg2
SYD16_SFg3$p1inp2vec <- is.element(SYD16_SFg3$player1, player2vector)
SYD16_SFg3$p2inp1vec <- is.element(SYD16_SFg3$player2, player1vector)

addPlayer1 <- SYD16_SFg3[ which(SYD16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_SFg3[ which(SYD16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_SFg2 <- rbind(SYD16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
SYD16_SFft <- ftable(SYD16_SFg2$player1, SYD16_SFg2$player2)
SYD16_SFft2 <- as.matrix(SYD16_SFft)
numRows <- nrow(SYD16_SFft2)
numCols <- ncol(SYD16_SFft2)
SYD16_SFft3 <- SYD16_SFft2[c(2:numRows) , c(2:numCols)]
SYD16_SFTable <- graph.adjacency(SYD16_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(SYD16_SFTable, vertex.label = V(SYD16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
SYD16_SF.clusterCoef <- transitivity(SYD16_SFTable, type="global") #cluster coefficient
SYD16_SF.degreeCent <- centralization.degree(SYD16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_SFftn <- as.network.matrix(SYD16_SFft)
SYD16_SF.netDensity <- network.density(SYD16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_SF.entropy <- entropy(SYD16_SFft) #entropy

SYD16_SF.netMx <- cbind(SYD16_SF.netMx, SYD16_SF.clusterCoef, SYD16_SF.degreeCent$centralization,
                        SYD16_SF.netDensity, SYD16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
SYD16_TFg2 <- data.frame(SYD16_TF)
SYD16_TFg2 <- SYD16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_TFg2$player1
player2vector <- SYD16_TFg2$player2
SYD16_TFg3 <- SYD16_TFg2
SYD16_TFg3$p1inp2vec <- is.element(SYD16_TFg3$player1, player2vector)
SYD16_TFg3$p2inp1vec <- is.element(SYD16_TFg3$player2, player1vector)

addPlayer1 <- SYD16_TFg3[ which(SYD16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_TFg3[ which(SYD16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_TFg2 <- rbind(SYD16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
SYD16_TFft <- ftable(SYD16_TFg2$player1, SYD16_TFg2$player2)
SYD16_TFft2 <- as.matrix(SYD16_TFft)
numRows <- nrow(SYD16_TFft2)
numCols <- ncol(SYD16_TFft2)
SYD16_TFft3 <- SYD16_TFft2[c(2:numRows) , c(2:numCols)]
SYD16_TFTable <- graph.adjacency(SYD16_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(SYD16_TFTable, vertex.label = V(SYD16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
SYD16_TF.clusterCoef <- transitivity(SYD16_TFTable, type="global") #cluster coefficient
SYD16_TF.degreeCent <- centralization.degree(SYD16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_TFftn <- as.network.matrix(SYD16_TFft)
SYD16_TF.netDensity <- network.density(SYD16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_TF.entropy <- entropy(SYD16_TFft) #entropy

SYD16_TF.netMx <- cbind(SYD16_TF.netMx, SYD16_TF.clusterCoef, SYD16_TF.degreeCent$centralization,
                        SYD16_TF.netDensity, SYD16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
SYD16_SAMg2 <- data.frame(SYD16_SAM)
SYD16_SAMg2 <- SYD16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_SAMg2$player1
player2vector <- SYD16_SAMg2$player2
SYD16_SAMg3 <- SYD16_SAMg2
SYD16_SAMg3$p1inp2vec <- is.element(SYD16_SAMg3$player1, player2vector)
SYD16_SAMg3$p2inp1vec <- is.element(SYD16_SAMg3$player2, player1vector)

addPlayer1 <- SYD16_SAMg3[ which(SYD16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_SAMg3[ which(SYD16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_SAMg2 <- rbind(SYD16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
SYD16_SAMft <- ftable(SYD16_SAMg2$player1, SYD16_SAMg2$player2)
SYD16_SAMft2 <- as.matrix(SYD16_SAMft)
numRows <- nrow(SYD16_SAMft2)
numCols <- ncol(SYD16_SAMft2)
SYD16_SAMft3 <- SYD16_SAMft2[c(2:numRows) , c(2:numCols)]
SYD16_SAMTable <- graph.adjacency(SYD16_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(SYD16_SAMTable, vertex.label = V(SYD16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
SYD16_SAM.clusterCoef <- transitivity(SYD16_SAMTable, type="global") #cluster coefficient
SYD16_SAM.degreeCent <- centralization.degree(SYD16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_SAMftn <- as.network.matrix(SYD16_SAMft)
SYD16_SAM.netDensity <- network.density(SYD16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_SAM.entropy <- entropy(SYD16_SAMft) #entropy

SYD16_SAM.netMx <- cbind(SYD16_SAM.netMx, SYD16_SAM.clusterCoef, SYD16_SAM.degreeCent$centralization,
                         SYD16_SAM.netDensity, SYD16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************
#NA

round = 16
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
SYD16_TAMg2 <- data.frame(SYD16_TAM)
SYD16_TAMg2 <- SYD16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_TAMg2$player1
player2vector <- SYD16_TAMg2$player2
SYD16_TAMg3 <- SYD16_TAMg2
SYD16_TAMg3$p1inp2vec <- is.element(SYD16_TAMg3$player1, player2vector)
SYD16_TAMg3$p2inp1vec <- is.element(SYD16_TAMg3$player2, player1vector)

addPlayer1 <- SYD16_TAMg3[ which(SYD16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_TAMg3[ which(SYD16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_TAMg2 <- rbind(SYD16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
SYD16_TAMft <- ftable(SYD16_TAMg2$player1, SYD16_TAMg2$player2)
SYD16_TAMft2 <- as.matrix(SYD16_TAMft)
numRows <- nrow(SYD16_TAMft2)
numCols <- ncol(SYD16_TAMft2)
SYD16_TAMft3 <- SYD16_TAMft2[c(2:numRows) , c(2:numCols)]
SYD16_TAMTable <- graph.adjacency(SYD16_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(SYD16_TAMTable, vertex.label = V(SYD16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
SYD16_TAM.clusterCoef <- transitivity(SYD16_TAMTable, type="global") #cluster coefficient
SYD16_TAM.degreeCent <- centralization.degree(SYD16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_TAMftn <- as.network.matrix(SYD16_TAMft)
SYD16_TAM.netDensity <- network.density(SYD16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_TAM.entropy <- entropy(SYD16_TAMft) #entropy

SYD16_TAM.netMx <- cbind(SYD16_TAM.netMx, SYD16_TAM.clusterCoef, SYD16_TAM.degreeCent$centralization,
                         SYD16_TAM.netDensity, SYD16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
SYD16_SDMg2 <- data.frame(SYD16_SDM)
SYD16_SDMg2 <- SYD16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_SDMg2$player1
player2vector <- SYD16_SDMg2$player2
SYD16_SDMg3 <- SYD16_SDMg2
SYD16_SDMg3$p1inp2vec <- is.element(SYD16_SDMg3$player1, player2vector)
SYD16_SDMg3$p2inp1vec <- is.element(SYD16_SDMg3$player2, player1vector)

addPlayer1 <- SYD16_SDMg3[ which(SYD16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_SDMg3[ which(SYD16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_SDMg2 <- rbind(SYD16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
SYD16_SDMft <- ftable(SYD16_SDMg2$player1, SYD16_SDMg2$player2)
SYD16_SDMft2 <- as.matrix(SYD16_SDMft)
numRows <- nrow(SYD16_SDMft2)
numCols <- ncol(SYD16_SDMft2)
SYD16_SDMft3 <- SYD16_SDMft2[c(2:numRows) , c(2:numCols)]
SYD16_SDMTable <- graph.adjacency(SYD16_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(SYD16_SDMTable, vertex.label = V(SYD16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
SYD16_SDM.clusterCoef <- transitivity(SYD16_SDMTable, type="global") #cluster coefficient
SYD16_SDM.degreeCent <- centralization.degree(SYD16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_SDMftn <- as.network.matrix(SYD16_SDMft)
SYD16_SDM.netDensity <- network.density(SYD16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_SDM.entropy <- entropy(SYD16_SDMft) #entropy

SYD16_SDM.netMx <- cbind(SYD16_SDM.netMx, SYD16_SDM.clusterCoef, SYD16_SDM.degreeCent$centralization,
                         SYD16_SDM.netDensity, SYD16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
SYD16_TDMg2 <- data.frame(SYD16_TDM)
SYD16_TDMg2 <- SYD16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_TDMg2$player1
player2vector <- SYD16_TDMg2$player2
SYD16_TDMg3 <- SYD16_TDMg2
SYD16_TDMg3$p1inp2vec <- is.element(SYD16_TDMg3$player1, player2vector)
SYD16_TDMg3$p2inp1vec <- is.element(SYD16_TDMg3$player2, player1vector)

addPlayer1 <- SYD16_TDMg3[ which(SYD16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_TDMg3[ which(SYD16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_TDMg2 <- rbind(SYD16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
SYD16_TDMft <- ftable(SYD16_TDMg2$player1, SYD16_TDMg2$player2)
SYD16_TDMft2 <- as.matrix(SYD16_TDMft)
numRows <- nrow(SYD16_TDMft2)
numCols <- ncol(SYD16_TDMft2)
SYD16_TDMft3 <- SYD16_TDMft2[c(2:numRows) , c(2:numCols)]
SYD16_TDMTable <- graph.adjacency(SYD16_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(SYD16_TDMTable, vertex.label = V(SYD16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
SYD16_TDM.clusterCoef <- transitivity(SYD16_TDMTable, type="global") #cluster coefficient
SYD16_TDM.degreeCent <- centralization.degree(SYD16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_TDMftn <- as.network.matrix(SYD16_TDMft)
SYD16_TDM.netDensity <- network.density(SYD16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_TDM.entropy <- entropy(SYD16_TDMft) #entropy

SYD16_TDM.netMx <- cbind(SYD16_TDM.netMx, SYD16_TDM.clusterCoef, SYD16_TDM.degreeCent$centralization,
                         SYD16_TDM.netDensity, SYD16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
SYD16_SDg2 <- data.frame(SYD16_SD)
SYD16_SDg2 <- SYD16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_SDg2$player1
player2vector <- SYD16_SDg2$player2
SYD16_SDg3 <- SYD16_SDg2
SYD16_SDg3$p1inp2vec <- is.element(SYD16_SDg3$player1, player2vector)
SYD16_SDg3$p2inp1vec <- is.element(SYD16_SDg3$player2, player1vector)

addPlayer1 <- SYD16_SDg3[ which(SYD16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_SDg3[ which(SYD16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_SDg2 <- rbind(SYD16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
SYD16_SDft <- ftable(SYD16_SDg2$player1, SYD16_SDg2$player2)
SYD16_SDft2 <- as.matrix(SYD16_SDft)
numRows <- nrow(SYD16_SDft2)
numCols <- ncol(SYD16_SDft2)
SYD16_SDft3 <- SYD16_SDft2[c(2:numRows) , c(2:numCols)]
SYD16_SDTable <- graph.adjacency(SYD16_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(SYD16_SDTable, vertex.label = V(SYD16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
SYD16_SD.clusterCoef <- transitivity(SYD16_SDTable, type="global") #cluster coefficient
SYD16_SD.degreeCent <- centralization.degree(SYD16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_SDftn <- as.network.matrix(SYD16_SDft)
SYD16_SD.netDensity <- network.density(SYD16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_SD.entropy <- entropy(SYD16_SDft) #entropy

SYD16_SD.netMx <- cbind(SYD16_SD.netMx, SYD16_SD.clusterCoef, SYD16_SD.degreeCent$centralization,
                        SYD16_SD.netDensity, SYD16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
SYD16_TDg2 <- data.frame(SYD16_TD)
SYD16_TDg2 <- SYD16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_TDg2$player1
player2vector <- SYD16_TDg2$player2
SYD16_TDg3 <- SYD16_TDg2
SYD16_TDg3$p1inp2vec <- is.element(SYD16_TDg3$player1, player2vector)
SYD16_TDg3$p2inp1vec <- is.element(SYD16_TDg3$player2, player1vector)

addPlayer1 <- SYD16_TDg3[ which(SYD16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_TDg3[ which(SYD16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_TDg2 <- rbind(SYD16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
SYD16_TDft <- ftable(SYD16_TDg2$player1, SYD16_TDg2$player2)
SYD16_TDft2 <- as.matrix(SYD16_TDft)
numRows <- nrow(SYD16_TDft2)
numCols <- ncol(SYD16_TDft2)
SYD16_TDft3 <- SYD16_TDft2[c(2:numRows) , c(2:numCols)]
SYD16_TDTable <- graph.adjacency(SYD16_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(SYD16_TDTable, vertex.label = V(SYD16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
SYD16_TD.clusterCoef <- transitivity(SYD16_TDTable, type="global") #cluster coefficient
SYD16_TD.degreeCent <- centralization.degree(SYD16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_TDftn <- as.network.matrix(SYD16_TDft)
SYD16_TD.netDensity <- network.density(SYD16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_TD.entropy <- entropy(SYD16_TDft) #entropy

SYD16_TD.netMx <- cbind(SYD16_TD.netMx, SYD16_TD.clusterCoef, SYD16_TD.degreeCent$centralization,
                        SYD16_TD.netDensity, SYD16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
SYD16_QTg2 <- data.frame(SYD16_QT)
SYD16_QTg2 <- SYD16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD16_QTg2$player1
player2vector <- SYD16_QTg2$player2
SYD16_QTg3 <- SYD16_QTg2
SYD16_QTg3$p1inp2vec <- is.element(SYD16_QTg3$player1, player2vector)
SYD16_QTg3$p2inp1vec <- is.element(SYD16_QTg3$player2, player1vector)

addPlayer1 <- SYD16_QTg3[ which(SYD16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD16_QTg3[ which(SYD16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD16_QTg2 <- rbind(SYD16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
SYD16_QTft <- ftable(SYD16_QTg2$player1, SYD16_QTg2$player2)
SYD16_QTft2 <- as.matrix(SYD16_QTft)
numRows <- nrow(SYD16_QTft2)
numCols <- ncol(SYD16_QTft2)
SYD16_QTft3 <- SYD16_QTft2[c(2:numRows) , c(2:numCols)]
SYD16_QTTable <- graph.adjacency(SYD16_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(SYD16_QTTable, vertex.label = V(SYD16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
SYD16_QT.clusterCoef <- transitivity(SYD16_QTTable, type="global") #cluster coefficient
SYD16_QT.degreeCent <- centralization.degree(SYD16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD16_QTftn <- as.network.matrix(SYD16_QTft)
SYD16_QT.netDensity <- network.density(SYD16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD16_QT.entropy <- entropy(SYD16_QTft) #entropy

SYD16_QT.netMx <- cbind(SYD16_QT.netMx, SYD16_QT.clusterCoef, SYD16_QT.degreeCent$centralization,
                        SYD16_QT.netDensity, SYD16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD16_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************

round = 16
teamName = "WB"
KIoutcome = "Goal_F"
WB16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
WB16_Gg2 <- data.frame(WB16_G)
WB16_Gg2 <- WB16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_Gg2$player1
player2vector <- WB16_Gg2$player2
WB16_Gg3 <- WB16_Gg2
WB16_Gg3$p1inp2vec <- is.element(WB16_Gg3$player1, player2vector)
WB16_Gg3$p2inp1vec <- is.element(WB16_Gg3$player2, player1vector)

addPlayer1 <- WB16_Gg3[ which(WB16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_Gg3[ which(WB16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_Gg2 <- rbind(WB16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
WB16_Gft <- ftable(WB16_Gg2$player1, WB16_Gg2$player2)
WB16_Gft2 <- as.matrix(WB16_Gft)
numRows <- nrow(WB16_Gft2)
numCols <- ncol(WB16_Gft2)
WB16_Gft3 <- WB16_Gft2[c(2:numRows) , c(2:numCols)]
WB16_GTable <- graph.adjacency(WB16_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(WB16_GTable, vertex.label = V(WB16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
WB16_G.clusterCoef <- transitivity(WB16_GTable, type="global") #cluster coefficient
WB16_G.degreeCent <- centralization.degree(WB16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_Gftn <- as.network.matrix(WB16_Gft)
WB16_G.netDensity <- network.density(WB16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_G.entropy <- entropy(WB16_Gft) #entropy

WB16_G.netMx <- cbind(WB16_G.netMx, WB16_G.clusterCoef, WB16_G.degreeCent$centralization,
                      WB16_G.netDensity, WB16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************
#NA

round = 16
teamName = "WB"
KIoutcome = "Behind_F"
WB16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
WB16_Bg2 <- data.frame(WB16_B)
WB16_Bg2 <- WB16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_Bg2$player1
player2vector <- WB16_Bg2$player2
WB16_Bg3 <- WB16_Bg2
WB16_Bg3$p1inp2vec <- is.element(WB16_Bg3$player1, player2vector)
WB16_Bg3$p2inp1vec <- is.element(WB16_Bg3$player2, player1vector)

addPlayer1 <- WB16_Bg3[ which(WB16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_Bg3[ which(WB16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_Bg2 <- rbind(WB16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
WB16_Bft <- ftable(WB16_Bg2$player1, WB16_Bg2$player2)
WB16_Bft2 <- as.matrix(WB16_Bft)
numRows <- nrow(WB16_Bft2)
numCols <- ncol(WB16_Bft2)
WB16_Bft3 <- WB16_Bft2[c(2:numRows) , c(2:numCols)]
WB16_BTable <- graph.adjacency(WB16_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(WB16_BTable, vertex.label = V(WB16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
WB16_B.clusterCoef <- transitivity(WB16_BTable, type="global") #cluster coefficient
WB16_B.degreeCent <- centralization.degree(WB16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_Bftn <- as.network.matrix(WB16_Bft)
WB16_B.netDensity <- network.density(WB16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_B.entropy <- entropy(WB16_Bft) #entropy

WB16_B.netMx <- cbind(WB16_B.netMx, WB16_B.clusterCoef, WB16_B.degreeCent$centralization,
                      WB16_B.netDensity, WB16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************
#NA

round = 16
teamName = "WB"
KIoutcome = "Stoppage_F"
WB16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
WB16_SFg2 <- data.frame(WB16_SF)
WB16_SFg2 <- WB16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_SFg2$player1
player2vector <- WB16_SFg2$player2
WB16_SFg3 <- WB16_SFg2
WB16_SFg3$p1inp2vec <- is.element(WB16_SFg3$player1, player2vector)
WB16_SFg3$p2inp1vec <- is.element(WB16_SFg3$player2, player1vector)

addPlayer1 <- WB16_SFg3[ which(WB16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_SFg3[ which(WB16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_SFg2 <- rbind(WB16_SFg2, addPlayers)

#ROUND 16, FWD Stoppage graph using weighted edges
WB16_SFft <- ftable(WB16_SFg2$player1, WB16_SFg2$player2)
WB16_SFft2 <- as.matrix(WB16_SFft)
numRows <- nrow(WB16_SFft2)
numCols <- ncol(WB16_SFft2)
WB16_SFft3 <- WB16_SFft2[c(2:numRows) , c(2:numCols)]
WB16_SFTable <- graph.adjacency(WB16_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(WB16_SFTable, vertex.label = V(WB16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
WB16_SF.clusterCoef <- transitivity(WB16_SFTable, type="global") #cluster coefficient
WB16_SF.degreeCent <- centralization.degree(WB16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_SFftn <- as.network.matrix(WB16_SFft)
WB16_SF.netDensity <- network.density(WB16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_SF.entropy <- entropy(WB16_SFft) #entropy

WB16_SF.netMx <- cbind(WB16_SF.netMx, WB16_SF.clusterCoef, WB16_SF.degreeCent$centralization,
                       WB16_SF.netDensity, WB16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "WB"
KIoutcome = "Turnover_F"
WB16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
WB16_TFg2 <- data.frame(WB16_TF)
WB16_TFg2 <- WB16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_TFg2$player1
player2vector <- WB16_TFg2$player2
WB16_TFg3 <- WB16_TFg2
WB16_TFg3$p1inp2vec <- is.element(WB16_TFg3$player1, player2vector)
WB16_TFg3$p2inp1vec <- is.element(WB16_TFg3$player2, player1vector)

addPlayer1 <- WB16_TFg3[ which(WB16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_TFg3[ which(WB16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_TFg2 <- rbind(WB16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
WB16_TFft <- ftable(WB16_TFg2$player1, WB16_TFg2$player2)
WB16_TFft2 <- as.matrix(WB16_TFft)
numRows <- nrow(WB16_TFft2)
numCols <- ncol(WB16_TFft2)
WB16_TFft3 <- WB16_TFft2[c(2:numRows) , c(2:numCols)]
WB16_TFTable <- graph.adjacency(WB16_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(WB16_TFTable, vertex.label = V(WB16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
WB16_TF.clusterCoef <- transitivity(WB16_TFTable, type="global") #cluster coefficient
WB16_TF.degreeCent <- centralization.degree(WB16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_TFftn <- as.network.matrix(WB16_TFft)
WB16_TF.netDensity <- network.density(WB16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_TF.entropy <- entropy(WB16_TFft) #entropy

WB16_TF.netMx <- cbind(WB16_TF.netMx, WB16_TF.clusterCoef, WB16_TF.degreeCent$centralization,
                       WB16_TF.netDensity, WB16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************
#NA

round = 16
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
WB16_SAMg2 <- data.frame(WB16_SAM)
WB16_SAMg2 <- WB16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_SAMg2$player1
player2vector <- WB16_SAMg2$player2
WB16_SAMg3 <- WB16_SAMg2
WB16_SAMg3$p1inp2vec <- is.element(WB16_SAMg3$player1, player2vector)
WB16_SAMg3$p2inp1vec <- is.element(WB16_SAMg3$player2, player1vector)

addPlayer1 <- WB16_SAMg3[ which(WB16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_SAMg3[ which(WB16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_SAMg2 <- rbind(WB16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
WB16_SAMft <- ftable(WB16_SAMg2$player1, WB16_SAMg2$player2)
WB16_SAMft2 <- as.matrix(WB16_SAMft)
numRows <- nrow(WB16_SAMft2)
numCols <- ncol(WB16_SAMft2)
WB16_SAMft3 <- WB16_SAMft2[c(2:numRows) , c(2:numCols)]
WB16_SAMTable <- graph.adjacency(WB16_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(WB16_SAMTable, vertex.label = V(WB16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
WB16_SAM.clusterCoef <- transitivity(WB16_SAMTable, type="global") #cluster coefficient
WB16_SAM.degreeCent <- centralization.degree(WB16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_SAMftn <- as.network.matrix(WB16_SAMft)
WB16_SAM.netDensity <- network.density(WB16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_SAM.entropy <- entropy(WB16_SAMft) #entropy

WB16_SAM.netMx <- cbind(WB16_SAM.netMx, WB16_SAM.clusterCoef, WB16_SAM.degreeCent$centralization,
                        WB16_SAM.netDensity, WB16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************
#NA

round = 16
teamName = "WB"
KIoutcome = "Turnover_AM"
WB16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
WB16_TAMg2 <- data.frame(WB16_TAM)
WB16_TAMg2 <- WB16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_TAMg2$player1
player2vector <- WB16_TAMg2$player2
WB16_TAMg3 <- WB16_TAMg2
WB16_TAMg3$p1inp2vec <- is.element(WB16_TAMg3$player1, player2vector)
WB16_TAMg3$p2inp1vec <- is.element(WB16_TAMg3$player2, player1vector)

addPlayer1 <- WB16_TAMg3[ which(WB16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_TAMg3[ which(WB16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_TAMg2 <- rbind(WB16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
WB16_TAMft <- ftable(WB16_TAMg2$player1, WB16_TAMg2$player2)
WB16_TAMft2 <- as.matrix(WB16_TAMft)
numRows <- nrow(WB16_TAMft2)
numCols <- ncol(WB16_TAMft2)
WB16_TAMft3 <- WB16_TAMft2[c(2:numRows) , c(2:numCols)]
WB16_TAMTable <- graph.adjacency(WB16_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(WB16_TAMTable, vertex.label = V(WB16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
WB16_TAM.clusterCoef <- transitivity(WB16_TAMTable, type="global") #cluster coefficient
WB16_TAM.degreeCent <- centralization.degree(WB16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_TAMftn <- as.network.matrix(WB16_TAMft)
WB16_TAM.netDensity <- network.density(WB16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_TAM.entropy <- entropy(WB16_TAMft) #entropy

WB16_TAM.netMx <- cbind(WB16_TAM.netMx, WB16_TAM.clusterCoef, WB16_TAM.degreeCent$centralization,
                        WB16_TAM.netDensity, WB16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
WB16_SDMg2 <- data.frame(WB16_SDM)
WB16_SDMg2 <- WB16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_SDMg2$player1
player2vector <- WB16_SDMg2$player2
WB16_SDMg3 <- WB16_SDMg2
WB16_SDMg3$p1inp2vec <- is.element(WB16_SDMg3$player1, player2vector)
WB16_SDMg3$p2inp1vec <- is.element(WB16_SDMg3$player2, player1vector)

addPlayer1 <- WB16_SDMg3[ which(WB16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_SDMg3[ which(WB16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_SDMg2 <- rbind(WB16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
WB16_SDMft <- ftable(WB16_SDMg2$player1, WB16_SDMg2$player2)
WB16_SDMft2 <- as.matrix(WB16_SDMft)
numRows <- nrow(WB16_SDMft2)
numCols <- ncol(WB16_SDMft2)
WB16_SDMft3 <- WB16_SDMft2[c(2:numRows) , c(2:numCols)]
WB16_SDMTable <- graph.adjacency(WB16_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(WB16_SDMTable, vertex.label = V(WB16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
WB16_SDM.clusterCoef <- transitivity(WB16_SDMTable, type="global") #cluster coefficient
WB16_SDM.degreeCent <- centralization.degree(WB16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_SDMftn <- as.network.matrix(WB16_SDMft)
WB16_SDM.netDensity <- network.density(WB16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_SDM.entropy <- entropy(WB16_SDMft) #entropy

WB16_SDM.netMx <- cbind(WB16_SDM.netMx, WB16_SDM.clusterCoef, WB16_SDM.degreeCent$centralization,
                        WB16_SDM.netDensity, WB16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "WB"
KIoutcome = "Turnover_DM"
WB16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
WB16_TDMg2 <- data.frame(WB16_TDM)
WB16_TDMg2 <- WB16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_TDMg2$player1
player2vector <- WB16_TDMg2$player2
WB16_TDMg3 <- WB16_TDMg2
WB16_TDMg3$p1inp2vec <- is.element(WB16_TDMg3$player1, player2vector)
WB16_TDMg3$p2inp1vec <- is.element(WB16_TDMg3$player2, player1vector)

addPlayer1 <- WB16_TDMg3[ which(WB16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_TDMg3[ which(WB16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_TDMg2 <- rbind(WB16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
WB16_TDMft <- ftable(WB16_TDMg2$player1, WB16_TDMg2$player2)
WB16_TDMft2 <- as.matrix(WB16_TDMft)
numRows <- nrow(WB16_TDMft2)
numCols <- ncol(WB16_TDMft2)
WB16_TDMft3 <- WB16_TDMft2[c(2:numRows) , c(2:numCols)]
WB16_TDMTable <- graph.adjacency(WB16_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(WB16_TDMTable, vertex.label = V(WB16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
WB16_TDM.clusterCoef <- transitivity(WB16_TDMTable, type="global") #cluster coefficient
WB16_TDM.degreeCent <- centralization.degree(WB16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_TDMftn <- as.network.matrix(WB16_TDMft)
WB16_TDM.netDensity <- network.density(WB16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_TDM.entropy <- entropy(WB16_TDMft) #entropy

WB16_TDM.netMx <- cbind(WB16_TDM.netMx, WB16_TDM.clusterCoef, WB16_TDM.degreeCent$centralization,
                        WB16_TDM.netDensity, WB16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "WB"
KIoutcome = "Stoppage_D"
WB16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
WB16_SDg2 <- data.frame(WB16_SD)
WB16_SDg2 <- WB16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_SDg2$player1
player2vector <- WB16_SDg2$player2
WB16_SDg3 <- WB16_SDg2
WB16_SDg3$p1inp2vec <- is.element(WB16_SDg3$player1, player2vector)
WB16_SDg3$p2inp1vec <- is.element(WB16_SDg3$player2, player1vector)

addPlayer1 <- WB16_SDg3[ which(WB16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_SDg3[ which(WB16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_SDg2 <- rbind(WB16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
WB16_SDft <- ftable(WB16_SDg2$player1, WB16_SDg2$player2)
WB16_SDft2 <- as.matrix(WB16_SDft)
numRows <- nrow(WB16_SDft2)
numCols <- ncol(WB16_SDft2)
WB16_SDft3 <- WB16_SDft2[c(2:numRows) , c(2:numCols)]
WB16_SDTable <- graph.adjacency(WB16_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(WB16_SDTable, vertex.label = V(WB16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
WB16_SD.clusterCoef <- transitivity(WB16_SDTable, type="global") #cluster coefficient
WB16_SD.degreeCent <- centralization.degree(WB16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_SDftn <- as.network.matrix(WB16_SDft)
WB16_SD.netDensity <- network.density(WB16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_SD.entropy <- entropy(WB16_SDft) #entropy

WB16_SD.netMx <- cbind(WB16_SD.netMx, WB16_SD.clusterCoef, WB16_SD.degreeCent$centralization,
                       WB16_SD.netDensity, WB16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "WB"
KIoutcome = "Turnover_D"
WB16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
WB16_TDg2 <- data.frame(WB16_TD)
WB16_TDg2 <- WB16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_TDg2$player1
player2vector <- WB16_TDg2$player2
WB16_TDg3 <- WB16_TDg2
WB16_TDg3$p1inp2vec <- is.element(WB16_TDg3$player1, player2vector)
WB16_TDg3$p2inp1vec <- is.element(WB16_TDg3$player2, player1vector)

addPlayer1 <- WB16_TDg3[ which(WB16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_TDg3[ which(WB16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_TDg2 <- rbind(WB16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
WB16_TDft <- ftable(WB16_TDg2$player1, WB16_TDg2$player2)
WB16_TDft2 <- as.matrix(WB16_TDft)
numRows <- nrow(WB16_TDft2)
numCols <- ncol(WB16_TDft2)
WB16_TDft3 <- WB16_TDft2[c(2:numRows) , c(2:numCols)]
WB16_TDTable <- graph.adjacency(WB16_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(WB16_TDTable, vertex.label = V(WB16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
WB16_TD.clusterCoef <- transitivity(WB16_TDTable, type="global") #cluster coefficient
WB16_TD.degreeCent <- centralization.degree(WB16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_TDftn <- as.network.matrix(WB16_TDft)
WB16_TD.netDensity <- network.density(WB16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_TD.entropy <- entropy(WB16_TDft) #entropy

WB16_TD.netMx <- cbind(WB16_TD.netMx, WB16_TD.clusterCoef, WB16_TD.degreeCent$centralization,
                       WB16_TD.netDensity, WB16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
WB16_QTg2 <- data.frame(WB16_QT)
WB16_QTg2 <- WB16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB16_QTg2$player1
player2vector <- WB16_QTg2$player2
WB16_QTg3 <- WB16_QTg2
WB16_QTg3$p1inp2vec <- is.element(WB16_QTg3$player1, player2vector)
WB16_QTg3$p2inp1vec <- is.element(WB16_QTg3$player2, player1vector)

addPlayer1 <- WB16_QTg3[ which(WB16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB16_QTg3[ which(WB16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB16_QTg2 <- rbind(WB16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
WB16_QTft <- ftable(WB16_QTg2$player1, WB16_QTg2$player2)
WB16_QTft2 <- as.matrix(WB16_QTft)
numRows <- nrow(WB16_QTft2)
numCols <- ncol(WB16_QTft2)
WB16_QTft3 <- WB16_QTft2[c(2:numRows) , c(2:numCols)]
WB16_QTTable <- graph.adjacency(WB16_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(WB16_QTTable, vertex.label = V(WB16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
WB16_QT.clusterCoef <- transitivity(WB16_QTTable, type="global") #cluster coefficient
WB16_QT.degreeCent <- centralization.degree(WB16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB16_QTftn <- as.network.matrix(WB16_QTft)
WB16_QT.netDensity <- network.density(WB16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB16_QT.entropy <- entropy(WB16_QTft) #entropy

WB16_QT.netMx <- cbind(WB16_QT.netMx, WB16_QT.clusterCoef, WB16_QT.degreeCent$centralization,
                       WB16_QT.netDensity, WB16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB16_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 16
##

#ROUND 16, Goal***************************************************************
#NA

round = 16
teamName = "WCE"
KIoutcome = "Goal_F"
WCE16_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Goal with weighted edges
WCE16_Gg2 <- data.frame(WCE16_G)
WCE16_Gg2 <- WCE16_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_Gg2$player1
player2vector <- WCE16_Gg2$player2
WCE16_Gg3 <- WCE16_Gg2
WCE16_Gg3$p1inp2vec <- is.element(WCE16_Gg3$player1, player2vector)
WCE16_Gg3$p2inp1vec <- is.element(WCE16_Gg3$player2, player1vector)

addPlayer1 <- WCE16_Gg3[ which(WCE16_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE16_Gg3[ which(WCE16_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_Gg2 <- rbind(WCE16_Gg2, addPlayers)

#ROUND 16, Goal graph using weighted edges
WCE16_Gft <- ftable(WCE16_Gg2$player1, WCE16_Gg2$player2)
WCE16_Gft2 <- as.matrix(WCE16_Gft)
numRows <- nrow(WCE16_Gft2)
numCols <- ncol(WCE16_Gft2)
WCE16_Gft3 <- WCE16_Gft2[c(2:numRows) , c(2:numCols)]
WCE16_GTable <- graph.adjacency(WCE16_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Goal graph=weighted
plot.igraph(WCE16_GTable, vertex.label = V(WCE16_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Goal calulation of network metrics
#igraph
WCE16_G.clusterCoef <- transitivity(WCE16_GTable, type="global") #cluster coefficient
WCE16_G.degreeCent <- centralization.degree(WCE16_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_Gftn <- as.network.matrix(WCE16_Gft)
WCE16_G.netDensity <- network.density(WCE16_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_G.entropy <- entropy(WCE16_Gft) #entropy

WCE16_G.netMx <- cbind(WCE16_G.netMx, WCE16_G.clusterCoef, WCE16_G.degreeCent$centralization,
                       WCE16_G.netDensity, WCE16_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_G.netMx) <- varnames

#ROUND 16, Behind***************************************************************

round = 16
teamName = "WCE"
KIoutcome = "Behind_F"
WCE16_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, Behind with weighted edges
WCE16_Bg2 <- data.frame(WCE16_B)
WCE16_Bg2 <- WCE16_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_Bg2$player1
player2vector <- WCE16_Bg2$player2
WCE16_Bg3 <- WCE16_Bg2
WCE16_Bg3$p1inp2vec <- is.element(WCE16_Bg3$player1, player2vector)
WCE16_Bg3$p2inp1vec <- is.element(WCE16_Bg3$player2, player1vector)

addPlayer1 <- WCE16_Bg3[ which(WCE16_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE16_Bg3[ which(WCE16_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_Bg2 <- rbind(WCE16_Bg2, addPlayers)

#ROUND 16, Behind graph using weighted edges
WCE16_Bft <- ftable(WCE16_Bg2$player1, WCE16_Bg2$player2)
WCE16_Bft2 <- as.matrix(WCE16_Bft)
numRows <- nrow(WCE16_Bft2)
numCols <- ncol(WCE16_Bft2)
WCE16_Bft3 <- WCE16_Bft2[c(2:numRows) , c(2:numCols)]
WCE16_BTable <- graph.adjacency(WCE16_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 16, Behind graph=weighted
plot.igraph(WCE16_BTable, vertex.label = V(WCE16_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, Behind calulation of network metrics
#igraph
WCE16_B.clusterCoef <- transitivity(WCE16_BTable, type="global") #cluster coefficient
WCE16_B.degreeCent <- centralization.degree(WCE16_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_Bftn <- as.network.matrix(WCE16_Bft)
WCE16_B.netDensity <- network.density(WCE16_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_B.entropy <- entropy(WCE16_Bft) #entropy

WCE16_B.netMx <- cbind(WCE16_B.netMx, WCE16_B.clusterCoef, WCE16_B.degreeCent$centralization,
                       WCE16_B.netDensity, WCE16_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_B.netMx) <- varnames

#ROUND 16, FWD Stoppage**********************************************************

round = 16
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE16_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Stoppage with weighted edges
WCE16_SFg2 <- data.frame(WCE16_SF)
WCE16_SFg2 <- WCE16_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_SFg2$player1
player2vector <- WCE16_SFg2$player2
WCE16_SFg3 <- WCE16_SFg2
WCE16_SFg3$p1inp2vec <- is.element(WCE16_SFg3$player1, player2vector)
WCE16_SFg3$p2inp1vec <- is.element(WCE16_SFg3$player2, player1vector)

addPlayer1 <- WCE16_TFg3[ which(WCE16_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE16_SFg3[ which(WCE16_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_SFg2 <- rbind(WCE16_SFg2, addPlayers)


#ROUND 16, FWD Stoppage graph using weighted edges
WCE16_SFft <- ftable(WCE16_SFg2$player1, WCE16_SFg2$player2)
WCE16_SFft2 <- as.matrix(WCE16_SFft)
numRows <- nrow(WCE16_SFft2)
numCols <- ncol(WCE16_SFft2)
WCE16_SFft3 <- WCE16_SFft2[c(2:numRows) , c(2:numCols)]
WCE16_SFTable <- graph.adjacency(WCE16_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Stoppage graph=weighted
plot.igraph(WCE16_SFTable, vertex.label = V(WCE16_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Stoppage calulation of network metrics
#igraph
WCE16_SF.clusterCoef <- transitivity(WCE16_SFTable, type="global") #cluster coefficient
WCE16_SF.degreeCent <- centralization.degree(WCE16_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_SFftn <- as.network.matrix(WCE16_SFft)
WCE16_SF.netDensity <- network.density(WCE16_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_SF.entropy <- entropy(WCE16_SFft) #entropy

WCE16_SF.netMx <- cbind(WCE16_SF.netMx, WCE16_SF.clusterCoef, WCE16_SF.degreeCent$centralization,
                        WCE16_SF.netDensity, WCE16_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_SF.netMx) <- varnames

#ROUND 16, FWD Turnover**********************************************************

round = 16
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE16_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, FWD Turnover with weighted edges
WCE16_TFg2 <- data.frame(WCE16_TF)
WCE16_TFg2 <- WCE16_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_TFg2$player1
player2vector <- WCE16_TFg2$player2
WCE16_TFg3 <- WCE16_TFg2
WCE16_TFg3$p1inp2vec <- is.element(WCE16_TFg3$player1, player2vector)
WCE16_TFg3$p2inp1vec <- is.element(WCE16_TFg3$player2, player1vector)

addPlayer1 <- WCE16_TFg3[ which(WCE16_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WCE16_TFg3[ which(WCE16_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_TFg2 <- rbind(WCE16_TFg2, addPlayers)

#ROUND 16, FWD Turnover graph using weighted edges
WCE16_TFft <- ftable(WCE16_TFg2$player1, WCE16_TFg2$player2)
WCE16_TFft2 <- as.matrix(WCE16_TFft)
numRows <- nrow(WCE16_TFft2)
numCols <- ncol(WCE16_TFft2)
WCE16_TFft3 <- WCE16_TFft2[c(2:numRows) , c(2:numCols)]
WCE16_TFTable <- graph.adjacency(WCE16_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, FWD Turnover graph=weighted
plot.igraph(WCE16_TFTable, vertex.label = V(WCE16_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, FWD Turnover calulation of network metrics
#igraph
WCE16_TF.clusterCoef <- transitivity(WCE16_TFTable, type="global") #cluster coefficient
WCE16_TF.degreeCent <- centralization.degree(WCE16_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_TFftn <- as.network.matrix(WCE16_TFft)
WCE16_TF.netDensity <- network.density(WCE16_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_TF.entropy <- entropy(WCE16_TFft) #entropy

WCE16_TF.netMx <- cbind(WCE16_TF.netMx, WCE16_TF.clusterCoef, WCE16_TF.degreeCent$centralization,
                        WCE16_TF.netDensity, WCE16_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_TF.netMx) <- varnames

#ROUND 16, AM Stoppage**********************************************************

round = 16
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE16_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Stoppage with weighted edges
WCE16_SAMg2 <- data.frame(WCE16_SAM)
WCE16_SAMg2 <- WCE16_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_SAMg2$player1
player2vector <- WCE16_SAMg2$player2
WCE16_SAMg3 <- WCE16_SAMg2
WCE16_SAMg3$p1inp2vec <- is.element(WCE16_SAMg3$player1, player2vector)
WCE16_SAMg3$p2inp1vec <- is.element(WCE16_SAMg3$player2, player1vector)

addPlayer1 <- WCE16_SAMg3[ which(WCE16_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE16_SAMg3[ which(WCE16_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_SAMg2 <- rbind(WCE16_SAMg2, addPlayers)

#ROUND 16, AM Stoppage graph using weighted edges
WCE16_SAMft <- ftable(WCE16_SAMg2$player1, WCE16_SAMg2$player2)
WCE16_SAMft2 <- as.matrix(WCE16_SAMft)
numRows <- nrow(WCE16_SAMft2)
numCols <- ncol(WCE16_SAMft2)
WCE16_SAMft3 <- WCE16_SAMft2[c(2:numRows) , c(2:numCols)]
WCE16_SAMTable <- graph.adjacency(WCE16_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Stoppage graph=weighted
plot.igraph(WCE16_SAMTable, vertex.label = V(WCE16_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Stoppage calulation of network metrics
#igraph
WCE16_SAM.clusterCoef <- transitivity(WCE16_SAMTable, type="global") #cluster coefficient
WCE16_SAM.degreeCent <- centralization.degree(WCE16_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_SAMftn <- as.network.matrix(WCE16_SAMft)
WCE16_SAM.netDensity <- network.density(WCE16_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_SAM.entropy <- entropy(WCE16_SAMft) #entropy

WCE16_SAM.netMx <- cbind(WCE16_SAM.netMx, WCE16_SAM.clusterCoef, WCE16_SAM.degreeCent$centralization,
                         WCE16_SAM.netDensity, WCE16_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_SAM.netMx) <- varnames

#ROUND 16, AM Turnover**********************************************************

round = 16
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE16_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, AM Turnover with weighted edges
WCE16_TAMg2 <- data.frame(WCE16_TAM)
WCE16_TAMg2 <- WCE16_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_TAMg2$player1
player2vector <- WCE16_TAMg2$player2
WCE16_TAMg3 <- WCE16_TAMg2
WCE16_TAMg3$p1inp2vec <- is.element(WCE16_TAMg3$player1, player2vector)
WCE16_TAMg3$p2inp1vec <- is.element(WCE16_TAMg3$player2, player1vector)

addPlayer1 <- WCE16_TAMg3[ which(WCE16_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE16_TAMg3[ which(WCE16_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_TAMg2 <- rbind(WCE16_TAMg2, addPlayers)

#ROUND 16, AM Turnover graph using weighted edges
WCE16_TAMft <- ftable(WCE16_TAMg2$player1, WCE16_TAMg2$player2)
WCE16_TAMft2 <- as.matrix(WCE16_TAMft)
numRows <- nrow(WCE16_TAMft2)
numCols <- ncol(WCE16_TAMft2)
WCE16_TAMft3 <- WCE16_TAMft2[c(2:numRows) , c(2:numCols)]
WCE16_TAMTable <- graph.adjacency(WCE16_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, AM Turnover graph=weighted
plot.igraph(WCE16_TAMTable, vertex.label = V(WCE16_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, AM Turnover calulation of network metrics
#igraph
WCE16_TAM.clusterCoef <- transitivity(WCE16_TAMTable, type="global") #cluster coefficient
WCE16_TAM.degreeCent <- centralization.degree(WCE16_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_TAMftn <- as.network.matrix(WCE16_TAMft)
WCE16_TAM.netDensity <- network.density(WCE16_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_TAM.entropy <- entropy(WCE16_TAMft) #entropy

WCE16_TAM.netMx <- cbind(WCE16_TAM.netMx, WCE16_TAM.clusterCoef, WCE16_TAM.degreeCent$centralization,
                         WCE16_TAM.netDensity, WCE16_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_TAM.netMx) <- varnames

#ROUND 16, DM Stoppage**********************************************************

round = 16
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE16_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Stoppage with weighted edges
WCE16_SDMg2 <- data.frame(WCE16_SDM)
WCE16_SDMg2 <- WCE16_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_SDMg2$player1
player2vector <- WCE16_SDMg2$player2
WCE16_SDMg3 <- WCE16_SDMg2
WCE16_SDMg3$p1inp2vec <- is.element(WCE16_SDMg3$player1, player2vector)
WCE16_SDMg3$p2inp1vec <- is.element(WCE16_SDMg3$player2, player1vector)

addPlayer1 <- WCE16_SDMg3[ which(WCE16_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE16_SDMg3[ which(WCE16_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_SDMg2 <- rbind(WCE16_SDMg2, addPlayers)

#ROUND 16, DM Stoppage graph using weighted edges
WCE16_SDMft <- ftable(WCE16_SDMg2$player1, WCE16_SDMg2$player2)
WCE16_SDMft2 <- as.matrix(WCE16_SDMft)
numRows <- nrow(WCE16_SDMft2)
numCols <- ncol(WCE16_SDMft2)
WCE16_SDMft3 <- WCE16_SDMft2[c(2:numRows) , c(2:numCols)]
WCE16_SDMTable <- graph.adjacency(WCE16_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Stoppage graph=weighted
plot.igraph(WCE16_SDMTable, vertex.label = V(WCE16_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Stoppage calulation of network metrics
#igraph
WCE16_SDM.clusterCoef <- transitivity(WCE16_SDMTable, type="global") #cluster coefficient
WCE16_SDM.degreeCent <- centralization.degree(WCE16_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_SDMftn <- as.network.matrix(WCE16_SDMft)
WCE16_SDM.netDensity <- network.density(WCE16_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_SDM.entropy <- entropy(WCE16_SDMft) #entropy

WCE16_SDM.netMx <- cbind(WCE16_SDM.netMx, WCE16_SDM.clusterCoef, WCE16_SDM.degreeCent$centralization,
                         WCE16_SDM.netDensity, WCE16_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_SDM.netMx) <- varnames

#ROUND 16, DM Turnover**********************************************************

round = 16
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE16_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, DM Turnover with weighted edges
WCE16_TDMg2 <- data.frame(WCE16_TDM)
WCE16_TDMg2 <- WCE16_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_TDMg2$player1
player2vector <- WCE16_TDMg2$player2
WCE16_TDMg3 <- WCE16_TDMg2
WCE16_TDMg3$p1inp2vec <- is.element(WCE16_TDMg3$player1, player2vector)
WCE16_TDMg3$p2inp1vec <- is.element(WCE16_TDMg3$player2, player1vector)

addPlayer1 <- WCE16_TDMg3[ which(WCE16_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE16_TDMg3[ which(WCE16_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_TDMg2 <- rbind(WCE16_TDMg2, addPlayers)

#ROUND 16, DM Turnover graph using weighted edges
WCE16_TDMft <- ftable(WCE16_TDMg2$player1, WCE16_TDMg2$player2)
WCE16_TDMft2 <- as.matrix(WCE16_TDMft)
numRows <- nrow(WCE16_TDMft2)
numCols <- ncol(WCE16_TDMft2)
WCE16_TDMft3 <- WCE16_TDMft2[c(2:numRows) , c(2:numCols)]
WCE16_TDMTable <- graph.adjacency(WCE16_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 16, DM Turnover graph=weighted
plot.igraph(WCE16_TDMTable, vertex.label = V(WCE16_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, DM Turnover calulation of network metrics
#igraph
WCE16_TDM.clusterCoef <- transitivity(WCE16_TDMTable, type="global") #cluster coefficient
WCE16_TDM.degreeCent <- centralization.degree(WCE16_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_TDMftn <- as.network.matrix(WCE16_TDMft)
WCE16_TDM.netDensity <- network.density(WCE16_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_TDM.entropy <- entropy(WCE16_TDMft) #entropy

WCE16_TDM.netMx <- cbind(WCE16_TDM.netMx, WCE16_TDM.clusterCoef, WCE16_TDM.degreeCent$centralization,
                         WCE16_TDM.netDensity, WCE16_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_TDM.netMx) <- varnames

#ROUND 16, D Stoppage**********************************************************
#NA

round = 16
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE16_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Stoppage with weighted edges
WCE16_SDg2 <- data.frame(WCE16_SD)
WCE16_SDg2 <- WCE16_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_SDg2$player1
player2vector <- WCE16_SDg2$player2
WCE16_SDg3 <- WCE16_SDg2
WCE16_SDg3$p1inp2vec <- is.element(WCE16_SDg3$player1, player2vector)
WCE16_SDg3$p2inp1vec <- is.element(WCE16_SDg3$player2, player1vector)

addPlayer1 <- WCE16_SDg3[ which(WCE16_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE16_SDg3[ which(WCE16_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_SDg2 <- rbind(WCE16_SDg2, addPlayers)

#ROUND 16, D Stoppage graph using weighted edges
WCE16_SDft <- ftable(WCE16_SDg2$player1, WCE16_SDg2$player2)
WCE16_SDft2 <- as.matrix(WCE16_SDft)
numRows <- nrow(WCE16_SDft2)
numCols <- ncol(WCE16_SDft2)
WCE16_SDft3 <- WCE16_SDft2[c(2:numRows) , c(2:numCols)]
WCE16_SDTable <- graph.adjacency(WCE16_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Stoppage graph=weighted
plot.igraph(WCE16_SDTable, vertex.label = V(WCE16_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Stoppage calulation of network metrics
#igraph
WCE16_SD.clusterCoef <- transitivity(WCE16_SDTable, type="global") #cluster coefficient
WCE16_SD.degreeCent <- centralization.degree(WCE16_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_SDftn <- as.network.matrix(WCE16_SDft)
WCE16_SD.netDensity <- network.density(WCE16_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_SD.entropy <- entropy(WCE16_SDft) #entropy

WCE16_SD.netMx <- cbind(WCE16_SD.netMx, WCE16_SD.clusterCoef, WCE16_SD.degreeCent$centralization,
                        WCE16_SD.netDensity, WCE16_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_SD.netMx) <- varnames

#ROUND 16, D Turnover**********************************************************
#NA

round = 16
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE16_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, D Turnover with weighted edges
WCE16_TDg2 <- data.frame(WCE16_TD)
WCE16_TDg2 <- WCE16_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_TDg2$player1
player2vector <- WCE16_TDg2$player2
WCE16_TDg3 <- WCE16_TDg2
WCE16_TDg3$p1inp2vec <- is.element(WCE16_TDg3$player1, player2vector)
WCE16_TDg3$p2inp1vec <- is.element(WCE16_TDg3$player2, player1vector)

addPlayer1 <- WCE16_TDg3[ which(WCE16_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE16_TDg3[ which(WCE16_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_TDg2 <- rbind(WCE16_TDg2, addPlayers)

#ROUND 16, D Turnover graph using weighted edges
WCE16_TDft <- ftable(WCE16_TDg2$player1, WCE16_TDg2$player2)
WCE16_TDft2 <- as.matrix(WCE16_TDft)
numRows <- nrow(WCE16_TDft2)
numCols <- ncol(WCE16_TDft2)
WCE16_TDft3 <- WCE16_TDft2[c(2:numRows) , c(2:numCols)]
WCE16_TDTable <- graph.adjacency(WCE16_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, D Turnover graph=weighted
plot.igraph(WCE16_TDTable, vertex.label = V(WCE16_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, D Turnover calulation of network metrics
#igraph
WCE16_TD.clusterCoef <- transitivity(WCE16_TDTable, type="global") #cluster coefficient
WCE16_TD.degreeCent <- centralization.degree(WCE16_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_TDftn <- as.network.matrix(WCE16_TDft)
WCE16_TD.netDensity <- network.density(WCE16_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_TD.entropy <- entropy(WCE16_TDft) #entropy

WCE16_TD.netMx <- cbind(WCE16_TD.netMx, WCE16_TD.clusterCoef, WCE16_TD.degreeCent$centralization,
                        WCE16_TD.netDensity, WCE16_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_TD.netMx) <- varnames

#ROUND 16, End of Qtr**********************************************************
#NA

round = 16
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE16_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 16, End of Qtr with weighted edges
WCE16_QTg2 <- data.frame(WCE16_QT)
WCE16_QTg2 <- WCE16_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE16_QTg2$player1
player2vector <- WCE16_QTg2$player2
WCE16_QTg3 <- WCE16_QTg2
WCE16_QTg3$p1inp2vec <- is.element(WCE16_QTg3$player1, player2vector)
WCE16_QTg3$p2inp1vec <- is.element(WCE16_QTg3$player2, player1vector)

addPlayer1 <- WCE16_QTg3[ which(WCE16_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE16_QTg3[ which(WCE16_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE16_QTg2 <- rbind(WCE16_QTg2, addPlayers)

#ROUND 16, End of Qtr graph using weighted edges
WCE16_QTft <- ftable(WCE16_QTg2$player1, WCE16_QTg2$player2)
WCE16_QTft2 <- as.matrix(WCE16_QTft)
numRows <- nrow(WCE16_QTft2)
numCols <- ncol(WCE16_QTft2)
WCE16_QTft3 <- WCE16_QTft2[c(2:numRows) , c(2:numCols)]
WCE16_QTTable <- graph.adjacency(WCE16_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 16, End of Qtr graph=weighted
plot.igraph(WCE16_QTTable, vertex.label = V(WCE16_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE16_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 16, End of Qtr calulation of network metrics
#igraph
WCE16_QT.clusterCoef <- transitivity(WCE16_QTTable, type="global") #cluster coefficient
WCE16_QT.degreeCent <- centralization.degree(WCE16_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE16_QTftn <- as.network.matrix(WCE16_QTft)
WCE16_QT.netDensity <- network.density(WCE16_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE16_QT.entropy <- entropy(WCE16_QTft) #entropy

WCE16_QT.netMx <- cbind(WCE16_QT.netMx, WCE16_QT.clusterCoef, WCE16_QT.degreeCent$centralization,
                        WCE16_QT.netDensity, WCE16_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE16_QT.netMx) <- varnames
