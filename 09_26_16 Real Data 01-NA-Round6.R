#####
#09-26-16- Real data 06
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************

round = 6
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
ADEL06_Gg2 <- data.frame(ADEL06_G)
ADEL06_Gg2 <- ADEL06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_Gg2$player1
player2vector <- ADEL06_Gg2$player2
ADEL06_Gg3 <- ADEL06_Gg2
ADEL06_Gg3$p1inp2vec <- is.element(ADEL06_Gg3$player1, player2vector)
ADEL06_Gg3$p2inp1vec <- is.element(ADEL06_Gg3$player2, player1vector)

addPlayer1 <- ADEL06_Gg3[ which(ADEL06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_Gg3[ which(ADEL06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_Gg2 <- rbind(ADEL06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
ADEL06_Gft <- ftable(ADEL06_Gg2$player1, ADEL06_Gg2$player2)
ADEL06_Gft2 <- as.matrix(ADEL06_Gft)
numRows <- nrow(ADEL06_Gft2)
numCols <- ncol(ADEL06_Gft2)
ADEL06_Gft3 <- ADEL06_Gft2[c(2:numRows) , c(2:numCols)]
ADEL06_GTable <- graph.adjacency(ADEL06_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(ADEL06_GTable, vertex.label = V(ADEL06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
ADEL06_G.clusterCoef <- transitivity(ADEL06_GTable, type="global") #cluster coefficient
ADEL06_G.degreeCent <- centralization.degree(ADEL06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_Gftn <- as.network.matrix(ADEL06_Gft)
ADEL06_G.netDensity <- network.density(ADEL06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_G.entropy <- entropy(ADEL06_Gft) #entropy

ADEL06_G.netMx <- cbind(ADEL06_G.netMx, ADEL06_G.clusterCoef, ADEL06_G.degreeCent$centralization,
                        ADEL06_G.netDensity, ADEL06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************
#NA

round = 6
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
ADEL06_Bg2 <- data.frame(ADEL06_B)
ADEL06_Bg2 <- ADEL06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_Bg2$player1
player2vector <- ADEL06_Bg2$player2
ADEL06_Bg3 <- ADEL06_Bg2
ADEL06_Bg3$p1inp2vec <- is.element(ADEL06_Bg3$player1, player2vector)
ADEL06_Bg3$p2inp1vec <- is.element(ADEL06_Bg3$player2, player1vector)

addPlayer1 <- ADEL06_Bg3[ which(ADEL06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_Bg3[ which(ADEL06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_Bg2 <- rbind(ADEL06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
ADEL06_Bft <- ftable(ADEL06_Bg2$player1, ADEL06_Bg2$player2)
ADEL06_Bft2 <- as.matrix(ADEL06_Bft)
numRows <- nrow(ADEL06_Bft2)
numCols <- ncol(ADEL06_Bft2)
ADEL06_Bft3 <- ADEL06_Bft2[c(2:numRows) , c(2:numCols)]
ADEL06_BTable <- graph.adjacency(ADEL06_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(ADEL06_BTable, vertex.label = V(ADEL06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
ADEL06_B.clusterCoef <- transitivity(ADEL06_BTable, type="global") #cluster coefficient
ADEL06_B.degreeCent <- centralization.degree(ADEL06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_Bftn <- as.network.matrix(ADEL06_Bft)
ADEL06_B.netDensity <- network.density(ADEL06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_B.entropy <- entropy(ADEL06_Bft) #entropy

ADEL06_B.netMx <- cbind(ADEL06_B.netMx, ADEL06_B.clusterCoef, ADEL06_B.degreeCent$centralization,
                        ADEL06_B.netDensity, ADEL06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
ADEL06_SFg2 <- data.frame(ADEL06_SF)
ADEL06_SFg2 <- ADEL06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_SFg2$player1
player2vector <- ADEL06_SFg2$player2
ADEL06_SFg3 <- ADEL06_SFg2
ADEL06_SFg3$p1inp2vec <- is.element(ADEL06_SFg3$player1, player2vector)
ADEL06_SFg3$p2inp1vec <- is.element(ADEL06_SFg3$player2, player1vector)

addPlayer1 <- ADEL06_SFg3[ which(ADEL06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_SFg3[ which(ADEL06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_SFg2 <- rbind(ADEL06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
ADEL06_SFft <- ftable(ADEL06_SFg2$player1, ADEL06_SFg2$player2)
ADEL06_SFft2 <- as.matrix(ADEL06_SFft)
numRows <- nrow(ADEL06_SFft2)
numCols <- ncol(ADEL06_SFft2)
ADEL06_SFft3 <- ADEL06_SFft2[c(2:numRows) , c(2:numCols)]
ADEL06_SFTable <- graph.adjacency(ADEL06_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(ADEL06_SFTable, vertex.label = V(ADEL06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
ADEL06_SF.clusterCoef <- transitivity(ADEL06_SFTable, type="global") #cluster coefficient
ADEL06_SF.degreeCent <- centralization.degree(ADEL06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_SFftn <- as.network.matrix(ADEL06_SFft)
ADEL06_SF.netDensity <- network.density(ADEL06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_SF.entropy <- entropy(ADEL06_SFft) #entropy

ADEL06_SF.netMx <- cbind(ADEL06_SF.netMx, ADEL06_SF.clusterCoef, ADEL06_SF.degreeCent$centralization,
                         ADEL06_SF.netDensity, ADEL06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
ADEL06_TFg2 <- data.frame(ADEL06_TF)
ADEL06_TFg2 <- ADEL06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_TFg2$player1
player2vector <- ADEL06_TFg2$player2
ADEL06_TFg3 <- ADEL06_TFg2
ADEL06_TFg3$p1inp2vec <- is.element(ADEL06_TFg3$player1, player2vector)
ADEL06_TFg3$p2inp1vec <- is.element(ADEL06_TFg3$player2, player1vector)

addPlayer1 <- ADEL06_TFg3[ which(ADEL06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ADEL06_TFg3[ which(ADEL06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_TFg2 <- rbind(ADEL06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
ADEL06_TFft <- ftable(ADEL06_TFg2$player1, ADEL06_TFg2$player2)
ADEL06_TFft2 <- as.matrix(ADEL06_TFft)
numRows <- nrow(ADEL06_TFft2)
numCols <- ncol(ADEL06_TFft2)
ADEL06_TFft3 <- ADEL06_TFft2[c(2:numRows) , c(2:numCols)]
ADEL06_TFTable <- graph.adjacency(ADEL06_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(ADEL06_TFTable, vertex.label = V(ADEL06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
ADEL06_TF.clusterCoef <- transitivity(ADEL06_TFTable, type="global") #cluster coefficient
ADEL06_TF.degreeCent <- centralization.degree(ADEL06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_TFftn <- as.network.matrix(ADEL06_TFft)
ADEL06_TF.netDensity <- network.density(ADEL06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_TF.entropy <- entropy(ADEL06_TFft) #entropy

ADEL06_TF.netMx <- cbind(ADEL06_TF.netMx, ADEL06_TF.clusterCoef, ADEL06_TF.degreeCent$centralization,
                         ADEL06_TF.netDensity, ADEL06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
ADEL06_SAMg2 <- data.frame(ADEL06_SAM)
ADEL06_SAMg2 <- ADEL06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_SAMg2$player1
player2vector <- ADEL06_SAMg2$player2
ADEL06_SAMg3 <- ADEL06_SAMg2
ADEL06_SAMg3$p1inp2vec <- is.element(ADEL06_SAMg3$player1, player2vector)
ADEL06_SAMg3$p2inp1vec <- is.element(ADEL06_SAMg3$player2, player1vector)

addPlayer1 <- ADEL06_SAMg3[ which(ADEL06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_SAMg3[ which(ADEL06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_SAMg2 <- rbind(ADEL06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
ADEL06_SAMft <- ftable(ADEL06_SAMg2$player1, ADEL06_SAMg2$player2)
ADEL06_SAMft2 <- as.matrix(ADEL06_SAMft)
numRows <- nrow(ADEL06_SAMft2)
numCols <- ncol(ADEL06_SAMft2)
ADEL06_SAMft3 <- ADEL06_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL06_SAMTable <- graph.adjacency(ADEL06_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(ADEL06_SAMTable, vertex.label = V(ADEL06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
ADEL06_SAM.clusterCoef <- transitivity(ADEL06_SAMTable, type="global") #cluster coefficient
ADEL06_SAM.degreeCent <- centralization.degree(ADEL06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_SAMftn <- as.network.matrix(ADEL06_SAMft)
ADEL06_SAM.netDensity <- network.density(ADEL06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_SAM.entropy <- entropy(ADEL06_SAMft) #entropy

ADEL06_SAM.netMx <- cbind(ADEL06_SAM.netMx, ADEL06_SAM.clusterCoef, ADEL06_SAM.degreeCent$centralization,
                          ADEL06_SAM.netDensity, ADEL06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************

round = 6
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
ADEL06_TAMg2 <- data.frame(ADEL06_TAM)
ADEL06_TAMg2 <- ADEL06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_TAMg2$player1
player2vector <- ADEL06_TAMg2$player2
ADEL06_TAMg3 <- ADEL06_TAMg2
ADEL06_TAMg3$p1inp2vec <- is.element(ADEL06_TAMg3$player1, player2vector)
ADEL06_TAMg3$p2inp1vec <- is.element(ADEL06_TAMg3$player2, player1vector)

addPlayer1 <- ADEL06_TAMg3[ which(ADEL06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_TAMg3[ which(ADEL06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_TAMg2 <- rbind(ADEL06_TAMg2, addPlayers)


#ROUND 6, AM Turnover graph using weighted edges
ADEL06_TAMft <- ftable(ADEL06_TAMg2$player1, ADEL06_TAMg2$player2)
ADEL06_TAMft2 <- as.matrix(ADEL06_TAMft)
numRows <- nrow(ADEL06_TAMft2)
numCols <- ncol(ADEL06_TAMft2)
ADEL06_TAMft3 <- ADEL06_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL06_TAMTable <- graph.adjacency(ADEL06_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(ADEL06_TAMTable, vertex.label = V(ADEL06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
ADEL06_TAM.clusterCoef <- transitivity(ADEL06_TAMTable, type="global") #cluster coefficient
ADEL06_TAM.degreeCent <- centralization.degree(ADEL06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_TAMftn <- as.network.matrix(ADEL06_TAMft)
ADEL06_TAM.netDensity <- network.density(ADEL06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_TAM.entropy <- entropy(ADEL06_TAMft) #entropy

ADEL06_TAM.netMx <- cbind(ADEL06_TAM.netMx, ADEL06_TAM.clusterCoef, ADEL06_TAM.degreeCent$centralization,
                          ADEL06_TAM.netDensity, ADEL06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************
#NA

round = 6
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
ADEL06_SDMg2 <- data.frame(ADEL06_SDM)
ADEL06_SDMg2 <- ADEL06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_SDMg2$player1
player2vector <- ADEL06_SDMg2$player2
ADEL06_SDMg3 <- ADEL06_SDMg2
ADEL06_SDMg3$p1inp2vec <- is.element(ADEL06_SDMg3$player1, player2vector)
ADEL06_SDMg3$p2inp1vec <- is.element(ADEL06_SDMg3$player2, player1vector)

addPlayer1 <- ADEL06_SDMg3[ which(ADEL06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_SDMg3[ which(ADEL06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_SDMg2 <- rbind(ADEL06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
ADEL06_SDMft <- ftable(ADEL06_SDMg2$player1, ADEL06_SDMg2$player2)
ADEL06_SDMft2 <- as.matrix(ADEL06_SDMft)
numRows <- nrow(ADEL06_SDMft2)
numCols <- ncol(ADEL06_SDMft2)
ADEL06_SDMft3 <- ADEL06_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL06_SDMTable <- graph.adjacency(ADEL06_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(ADEL06_SDMTable, vertex.label = V(ADEL06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
ADEL06_SDM.clusterCoef <- transitivity(ADEL06_SDMTable, type="global") #cluster coefficient
ADEL06_SDM.degreeCent <- centralization.degree(ADEL06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_SDMftn <- as.network.matrix(ADEL06_SDMft)
ADEL06_SDM.netDensity <- network.density(ADEL06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_SDM.entropy <- entropy(ADEL06_SDMft) #entropy

ADEL06_SDM.netMx <- cbind(ADEL06_SDM.netMx, ADEL06_SDM.clusterCoef, ADEL06_SDM.degreeCent$centralization,
                          ADEL06_SDM.netDensity, ADEL06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
ADEL06_TDMg2 <- data.frame(ADEL06_TDM)
ADEL06_TDMg2 <- ADEL06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_TDMg2$player1
player2vector <- ADEL06_TDMg2$player2
ADEL06_TDMg3 <- ADEL06_TDMg2
ADEL06_TDMg3$p1inp2vec <- is.element(ADEL06_TDMg3$player1, player2vector)
ADEL06_TDMg3$p2inp1vec <- is.element(ADEL06_TDMg3$player2, player1vector)

addPlayer1 <- ADEL06_TDMg3[ which(ADEL06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_TDMg3[ which(ADEL06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_TDMg2 <- rbind(ADEL06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
ADEL06_TDMft <- ftable(ADEL06_TDMg2$player1, ADEL06_TDMg2$player2)
ADEL06_TDMft2 <- as.matrix(ADEL06_TDMft)
numRows <- nrow(ADEL06_TDMft2)
numCols <- ncol(ADEL06_TDMft2)
ADEL06_TDMft3 <- ADEL06_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL06_TDMTable <- graph.adjacency(ADEL06_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(ADEL06_TDMTable, vertex.label = V(ADEL06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
ADEL06_TDM.clusterCoef <- transitivity(ADEL06_TDMTable, type="global") #cluster coefficient
ADEL06_TDM.degreeCent <- centralization.degree(ADEL06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_TDMftn <- as.network.matrix(ADEL06_TDMft)
ADEL06_TDM.netDensity <- network.density(ADEL06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_TDM.entropy <- entropy(ADEL06_TDMft) #entropy

ADEL06_TDM.netMx <- cbind(ADEL06_TDM.netMx, ADEL06_TDM.clusterCoef, ADEL06_TDM.degreeCent$centralization,
                          ADEL06_TDM.netDensity, ADEL06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
ADEL06_SDg2 <- data.frame(ADEL06_SD)
ADEL06_SDg2 <- ADEL06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_SDg2$player1
player2vector <- ADEL06_SDg2$player2
ADEL06_SDg3 <- ADEL06_SDg2
ADEL06_SDg3$p1inp2vec <- is.element(ADEL06_SDg3$player1, player2vector)
ADEL06_SDg3$p2inp1vec <- is.element(ADEL06_SDg3$player2, player1vector)

addPlayer1 <- ADEL06_SDg3[ which(ADEL06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_SDg3[ which(ADEL06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_SDg2 <- rbind(ADEL06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
ADEL06_SDft <- ftable(ADEL06_SDg2$player1, ADEL06_SDg2$player2)
ADEL06_SDft2 <- as.matrix(ADEL06_SDft)
numRows <- nrow(ADEL06_SDft2)
numCols <- ncol(ADEL06_SDft2)
ADEL06_SDft3 <- ADEL06_SDft2[c(2:numRows) , c(2:numCols)]
ADEL06_SDTable <- graph.adjacency(ADEL06_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(ADEL06_SDTable, vertex.label = V(ADEL06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
ADEL06_SD.clusterCoef <- transitivity(ADEL06_SDTable, type="global") #cluster coefficient
ADEL06_SD.degreeCent <- centralization.degree(ADEL06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_SDftn <- as.network.matrix(ADEL06_SDft)
ADEL06_SD.netDensity <- network.density(ADEL06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_SD.entropy <- entropy(ADEL06_SDft) #entropy

ADEL06_SD.netMx <- cbind(ADEL06_SD.netMx, ADEL06_SD.clusterCoef, ADEL06_SD.degreeCent$centralization,
                         ADEL06_SD.netDensity, ADEL06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************

round = 6
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
ADEL06_TDg2 <- data.frame(ADEL06_TD)
ADEL06_TDg2 <- ADEL06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_TDg2$player1
player2vector <- ADEL06_TDg2$player2
ADEL06_TDg3 <- ADEL06_TDg2
ADEL06_TDg3$p1inp2vec <- is.element(ADEL06_TDg3$player1, player2vector)
ADEL06_TDg3$p2inp1vec <- is.element(ADEL06_TDg3$player2, player1vector)

addPlayer1 <- ADEL06_TDg3[ which(ADEL06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_TDg3[ which(ADEL06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_TDg2 <- rbind(ADEL06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
ADEL06_TDft <- ftable(ADEL06_TDg2$player1, ADEL06_TDg2$player2)
ADEL06_TDft2 <- as.matrix(ADEL06_TDft)
numRows <- nrow(ADEL06_TDft2)
numCols <- ncol(ADEL06_TDft2)
ADEL06_TDft3 <- ADEL06_TDft2[c(2:numRows) , c(2:numCols)]
ADEL06_TDTable <- graph.adjacency(ADEL06_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(ADEL06_TDTable, vertex.label = V(ADEL06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
ADEL06_TD.clusterCoef <- transitivity(ADEL06_TDTable, type="global") #cluster coefficient
ADEL06_TD.degreeCent <- centralization.degree(ADEL06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_TDftn <- as.network.matrix(ADEL06_TDft)
ADEL06_TD.netDensity <- network.density(ADEL06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_TD.entropy <- entropy(ADEL06_TDft) #entropy

ADEL06_TD.netMx <- cbind(ADEL06_TD.netMx, ADEL06_TD.clusterCoef, ADEL06_TD.degreeCent$centralization,
                         ADEL06_TD.netDensity, ADEL06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
ADEL06_QTg2 <- data.frame(ADEL06_QT)
ADEL06_QTg2 <- ADEL06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL06_QTg2$player1
player2vector <- ADEL06_QTg2$player2
ADEL06_QTg3 <- ADEL06_QTg2
ADEL06_QTg3$p1inp2vec <- is.element(ADEL06_QTg3$player1, player2vector)
ADEL06_QTg3$p2inp1vec <- is.element(ADEL06_QTg3$player2, player1vector)

addPlayer1 <- ADEL06_QTg3[ which(ADEL06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL06_QTg3[ which(ADEL06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL06_QTg2 <- rbind(ADEL06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
ADEL06_QTft <- ftable(ADEL06_QTg2$player1, ADEL06_QTg2$player2)
ADEL06_QTft2 <- as.matrix(ADEL06_QTft)
numRows <- nrow(ADEL06_QTft2)
numCols <- ncol(ADEL06_QTft2)
ADEL06_QTft3 <- ADEL06_QTft2[c(2:numRows) , c(2:numCols)]
ADEL06_QTTable <- graph.adjacency(ADEL06_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(ADEL06_QTTable, vertex.label = V(ADEL06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
ADEL06_QT.clusterCoef <- transitivity(ADEL06_QTTable, type="global") #cluster coefficient
ADEL06_QT.degreeCent <- centralization.degree(ADEL06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL06_QTftn <- as.network.matrix(ADEL06_QTft)
ADEL06_QT.netDensity <- network.density(ADEL06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL06_QT.entropy <- entropy(ADEL06_QTft) #entropy

ADEL06_QT.netMx <- cbind(ADEL06_QT.netMx, ADEL06_QT.clusterCoef, ADEL06_QT.degreeCent$centralization,
                         ADEL06_QT.netDensity, ADEL06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL06_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "BL"
KIoutcome = "Goal_F"
BL06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
BL06_Gg2 <- data.frame(BL06_G)
BL06_Gg2 <- BL06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_Gg2$player1
player2vector <- BL06_Gg2$player2
BL06_Gg3 <- BL06_Gg2
BL06_Gg3$p1inp2vec <- is.element(BL06_Gg3$player1, player2vector)
BL06_Gg3$p2inp1vec <- is.element(BL06_Gg3$player2, player1vector)

addPlayer1 <- BL06_Gg3[ which(BL06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_Gg3[ which(BL06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_Gg2 <- rbind(BL06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
BL06_Gft <- ftable(BL06_Gg2$player1, BL06_Gg2$player2)
BL06_Gft2 <- as.matrix(BL06_Gft)
numRows <- nrow(BL06_Gft2)
numCols <- ncol(BL06_Gft2)
BL06_Gft3 <- BL06_Gft2[c(2:numRows) , c(2:numCols)]
BL06_GTable <- graph.adjacency(BL06_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(BL06_GTable, vertex.label = V(BL06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
BL06_G.clusterCoef <- transitivity(BL06_GTable, type="global") #cluster coefficient
BL06_G.degreeCent <- centralization.degree(BL06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_Gftn <- as.network.matrix(BL06_Gft)
BL06_G.netDensity <- network.density(BL06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_G.entropy <- entropy(BL06_Gft) #entropy

BL06_G.netMx <- cbind(BL06_G.netMx, BL06_G.clusterCoef, BL06_G.degreeCent$centralization,
                      BL06_G.netDensity, BL06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************
#NA

round = 6
teamName = "BL"
KIoutcome = "Behind_F"
BL06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
BL06_Bg2 <- data.frame(BL06_B)
BL06_Bg2 <- BL06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_Bg2$player1
player2vector <- BL06_Bg2$player2
BL06_Bg3 <- BL06_Bg2
BL06_Bg3$p1inp2vec <- is.element(BL06_Bg3$player1, player2vector)
BL06_Bg3$p2inp1vec <- is.element(BL06_Bg3$player2, player1vector)

addPlayer1 <- BL06_Bg3[ which(BL06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_Bg3[ which(BL06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_Bg2 <- rbind(BL06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
BL06_Bft <- ftable(BL06_Bg2$player1, BL06_Bg2$player2)
BL06_Bft2 <- as.matrix(BL06_Bft)
numRows <- nrow(BL06_Bft2)
numCols <- ncol(BL06_Bft2)
BL06_Bft3 <- BL06_Bft2[c(2:numRows) , c(2:numCols)]
BL06_BTable <- graph.adjacency(BL06_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(BL06_BTable, vertex.label = V(BL06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
BL06_B.clusterCoef <- transitivity(BL06_BTable, type="global") #cluster coefficient
BL06_B.degreeCent <- centralization.degree(BL06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_Bftn <- as.network.matrix(BL06_Bft)
BL06_B.netDensity <- network.density(BL06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_B.entropy <- entropy(BL06_Bft) #entropy

BL06_B.netMx <- cbind(BL06_B.netMx, BL06_B.clusterCoef, BL06_B.degreeCent$centralization,
                      BL06_B.netDensity, BL06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "BL"
KIoutcome = "Stoppage_F"
BL06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
BL06_SFg2 <- data.frame(BL06_SF)
BL06_SFg2 <- BL06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_SFg2$player1
player2vector <- BL06_SFg2$player2
BL06_SFg3 <- BL06_SFg2
BL06_SFg3$p1inp2vec <- is.element(BL06_SFg3$player1, player2vector)
BL06_SFg3$p2inp1vec <- is.element(BL06_SFg3$player2, player1vector)

addPlayer1 <- BL06_SFg3[ which(BL06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_SFg3[ which(BL06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_SFg2 <- rbind(BL06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
BL06_SFft <- ftable(BL06_SFg2$player1, BL06_SFg2$player2)
BL06_SFft2 <- as.matrix(BL06_SFft)
numRows <- nrow(BL06_SFft2)
numCols <- ncol(BL06_SFft2)
BL06_SFft3 <- BL06_SFft2[c(2:numRows) , c(2:numCols)]
BL06_SFTable <- graph.adjacency(BL06_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(BL06_SFTable, vertex.label = V(BL06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
BL06_SF.clusterCoef <- transitivity(BL06_SFTable, type="global") #cluster coefficient
BL06_SF.degreeCent <- centralization.degree(BL06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_SFftn <- as.network.matrix(BL06_SFft)
BL06_SF.netDensity <- network.density(BL06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_SF.entropy <- entropy(BL06_SFft) #entropy

BL06_SF.netMx <- cbind(BL06_SF.netMx, BL06_SF.clusterCoef, BL06_SF.degreeCent$centralization,
                       BL06_SF.netDensity, BL06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************
#NA

round = 6
teamName = "BL"
KIoutcome = "Turnover_F"
BL06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
BL06_TFg2 <- data.frame(BL06_TF)
BL06_TFg2 <- BL06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_TFg2$player1
player2vector <- BL06_TFg2$player2
BL06_TFg3 <- BL06_TFg2
BL06_TFg3$p1inp2vec <- is.element(BL06_TFg3$player1, player2vector)
BL06_TFg3$p2inp1vec <- is.element(BL06_TFg3$player2, player1vector)

addPlayer1 <- BL06_TFg3[ which(BL06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_TFg3[ which(BL06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_TFg2 <- rbind(BL06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
BL06_TFft <- ftable(BL06_TFg2$player1, BL06_TFg2$player2)
BL06_TFft2 <- as.matrix(BL06_TFft)
numRows <- nrow(BL06_TFft2)
numCols <- ncol(BL06_TFft2)
BL06_TFft3 <- BL06_TFft2[c(2:numRows) , c(2:numCols)]
BL06_TFTable <- graph.adjacency(BL06_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(BL06_TFTable, vertex.label = V(BL06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
BL06_TF.clusterCoef <- transitivity(BL06_TFTable, type="global") #cluster coefficient
BL06_TF.degreeCent <- centralization.degree(BL06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_TFftn <- as.network.matrix(BL06_TFft)
BL06_TF.netDensity <- network.density(BL06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_TF.entropy <- entropy(BL06_TFft) #entropy

BL06_TF.netMx <- cbind(BL06_TF.netMx, BL06_TF.clusterCoef, BL06_TF.degreeCent$centralization,
                       BL06_TF.netDensity, BL06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************

round = 6
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
BL06_SAMg2 <- data.frame(BL06_SAM)
BL06_SAMg2 <- BL06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_SAMg2$player1
player2vector <- BL06_SAMg2$player2
BL06_SAMg3 <- BL06_SAMg2
BL06_SAMg3$p1inp2vec <- is.element(BL06_SAMg3$player1, player2vector)
BL06_SAMg3$p2inp1vec <- is.element(BL06_SAMg3$player2, player1vector)

addPlayer1 <- BL06_SAMg3[ which(BL06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_SAMg3[ which(BL06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_SAMg2 <- rbind(BL06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
BL06_SAMft <- ftable(BL06_SAMg2$player1, BL06_SAMg2$player2)
BL06_SAMft2 <- as.matrix(BL06_SAMft)
numRows <- nrow(BL06_SAMft2)
numCols <- ncol(BL06_SAMft2)
BL06_SAMft3 <- BL06_SAMft2[c(2:numRows) , c(2:numCols)]
BL06_SAMTable <- graph.adjacency(BL06_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(BL06_SAMTable, vertex.label = V(BL06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
BL06_SAM.clusterCoef <- transitivity(BL06_SAMTable, type="global") #cluster coefficient
BL06_SAM.degreeCent <- centralization.degree(BL06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_SAMftn <- as.network.matrix(BL06_SAMft)
BL06_SAM.netDensity <- network.density(BL06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_SAM.entropy <- entropy(BL06_SAMft) #entropy

BL06_SAM.netMx <- cbind(BL06_SAM.netMx, BL06_SAM.clusterCoef, BL06_SAM.degreeCent$centralization,
                        BL06_SAM.netDensity, BL06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************
#NA

round = 6
teamName = "BL"
KIoutcome = "Turnover_AM"
BL06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
BL06_TAMg2 <- data.frame(BL06_TAM)
BL06_TAMg2 <- BL06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_TAMg2$player1
player2vector <- BL06_TAMg2$player2
BL06_TAMg3 <- BL06_TAMg2
BL06_TAMg3$p1inp2vec <- is.element(BL06_TAMg3$player1, player2vector)
BL06_TAMg3$p2inp1vec <- is.element(BL06_TAMg3$player2, player1vector)

addPlayer1 <- BL06_TAMg3[ which(BL06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_TAMg3[ which(BL06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_TAMg2 <- rbind(BL06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
BL06_TAMft <- ftable(BL06_TAMg2$player1, BL06_TAMg2$player2)
BL06_TAMft2 <- as.matrix(BL06_TAMft)
numRows <- nrow(BL06_TAMft2)
numCols <- ncol(BL06_TAMft2)
BL06_TAMft3 <- BL06_TAMft2[c(2:numRows) , c(2:numCols)]
BL06_TAMTable <- graph.adjacency(BL06_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(BL06_TAMTable, vertex.label = V(BL06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
BL06_TAM.clusterCoef <- transitivity(BL06_TAMTable, type="global") #cluster coefficient
BL06_TAM.degreeCent <- centralization.degree(BL06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_TAMftn <- as.network.matrix(BL06_TAMft)
BL06_TAM.netDensity <- network.density(BL06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_TAM.entropy <- entropy(BL06_TAMft) #entropy

BL06_TAM.netMx <- cbind(BL06_TAM.netMx, BL06_TAM.clusterCoef, BL06_TAM.degreeCent$centralization,
                        BL06_TAM.netDensity, BL06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************
#NA

round = 6
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
BL06_SDMg2 <- data.frame(BL06_SDM)
BL06_SDMg2 <- BL06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_SDMg2$player1
player2vector <- BL06_SDMg2$player2
BL06_SDMg3 <- BL06_SDMg2
BL06_SDMg3$p1inp2vec <- is.element(BL06_SDMg3$player1, player2vector)
BL06_SDMg3$p2inp1vec <- is.element(BL06_SDMg3$player2, player1vector)

addPlayer1 <- BL06_SDMg3[ which(BL06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_SDMg3[ which(BL06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_SDMg2 <- rbind(BL06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
BL06_SDMft <- ftable(BL06_SDMg2$player1, BL06_SDMg2$player2)
BL06_SDMft2 <- as.matrix(BL06_SDMft)
numRows <- nrow(BL06_SDMft2)
numCols <- ncol(BL06_SDMft2)
BL06_SDMft3 <- BL06_SDMft2[c(2:numRows) , c(2:numCols)]
BL06_SDMTable <- graph.adjacency(BL06_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(BL06_SDMTable, vertex.label = V(BL06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
BL06_SDM.clusterCoef <- transitivity(BL06_SDMTable, type="global") #cluster coefficient
BL06_SDM.degreeCent <- centralization.degree(BL06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_SDMftn <- as.network.matrix(BL06_SDMft)
BL06_SDM.netDensity <- network.density(BL06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_SDM.entropy <- entropy(BL06_SDMft) #entropy

BL06_SDM.netMx <- cbind(BL06_SDM.netMx, BL06_SDM.clusterCoef, BL06_SDM.degreeCent$centralization,
                        BL06_SDM.netDensity, BL06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "BL"
KIoutcome = "Turnover_DM"
BL06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
BL06_TDMg2 <- data.frame(BL06_TDM)
BL06_TDMg2 <- BL06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_TDMg2$player1
player2vector <- BL06_TDMg2$player2
BL06_TDMg3 <- BL06_TDMg2
BL06_TDMg3$p1inp2vec <- is.element(BL06_TDMg3$player1, player2vector)
BL06_TDMg3$p2inp1vec <- is.element(BL06_TDMg3$player2, player1vector)

addPlayer1 <- BL06_TDMg3[ which(BL06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_TDMg3[ which(BL06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_TDMg2 <- rbind(BL06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
BL06_TDMft <- ftable(BL06_TDMg2$player1, BL06_TDMg2$player2)
BL06_TDMft2 <- as.matrix(BL06_TDMft)
numRows <- nrow(BL06_TDMft2)
numCols <- ncol(BL06_TDMft2)
BL06_TDMft3 <- BL06_TDMft2[c(2:numRows) , c(2:numCols)]
BL06_TDMTable <- graph.adjacency(BL06_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(BL06_TDMTable, vertex.label = V(BL06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
BL06_TDM.clusterCoef <- transitivity(BL06_TDMTable, type="global") #cluster coefficient
BL06_TDM.degreeCent <- centralization.degree(BL06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_TDMftn <- as.network.matrix(BL06_TDMft)
BL06_TDM.netDensity <- network.density(BL06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_TDM.entropy <- entropy(BL06_TDMft) #entropy

BL06_TDM.netMx <- cbind(BL06_TDM.netMx, BL06_TDM.clusterCoef, BL06_TDM.degreeCent$centralization,
                        BL06_TDM.netDensity, BL06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "BL"
KIoutcome = "Stoppage_D"
BL06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
BL06_SDg2 <- data.frame(BL06_SD)
BL06_SDg2 <- BL06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_SDg2$player1
player2vector <- BL06_SDg2$player2
BL06_SDg3 <- BL06_SDg2
BL06_SDg3$p1inp2vec <- is.element(BL06_SDg3$player1, player2vector)
BL06_SDg3$p2inp1vec <- is.element(BL06_SDg3$player2, player1vector)

addPlayer1 <- BL06_SDg3[ which(BL06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_SDg3[ which(BL06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_SDg2 <- rbind(BL06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
BL06_SDft <- ftable(BL06_SDg2$player1, BL06_SDg2$player2)
BL06_SDft2 <- as.matrix(BL06_SDft)
numRows <- nrow(BL06_SDft2)
numCols <- ncol(BL06_SDft2)
BL06_SDft3 <- BL06_SDft2[c(2:numRows) , c(2:numCols)]
BL06_SDTable <- graph.adjacency(BL06_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(BL06_SDTable, vertex.label = V(BL06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
BL06_SD.clusterCoef <- transitivity(BL06_SDTable, type="global") #cluster coefficient
BL06_SD.degreeCent <- centralization.degree(BL06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_SDftn <- as.network.matrix(BL06_SDft)
BL06_SD.netDensity <- network.density(BL06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_SD.entropy <- entropy(BL06_SDft) #entropy

BL06_SD.netMx <- cbind(BL06_SD.netMx, BL06_SD.clusterCoef, BL06_SD.degreeCent$centralization,
                       BL06_SD.netDensity, BL06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "BL"
KIoutcome = "Turnover_D"
BL06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
BL06_TDg2 <- data.frame(BL06_TD)
BL06_TDg2 <- BL06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_TDg2$player1
player2vector <- BL06_TDg2$player2
BL06_TDg3 <- BL06_TDg2
BL06_TDg3$p1inp2vec <- is.element(BL06_TDg3$player1, player2vector)
BL06_TDg3$p2inp1vec <- is.element(BL06_TDg3$player2, player1vector)

addPlayer1 <- BL06_TDg3[ which(BL06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_TDg3[ which(BL06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_TDg2 <- rbind(BL06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
BL06_TDft <- ftable(BL06_TDg2$player1, BL06_TDg2$player2)
BL06_TDft2 <- as.matrix(BL06_TDft)
numRows <- nrow(BL06_TDft2)
numCols <- ncol(BL06_TDft2)
BL06_TDft3 <- BL06_TDft2[c(2:numRows) , c(2:numCols)]
BL06_TDTable <- graph.adjacency(BL06_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(BL06_TDTable, vertex.label = V(BL06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
BL06_TD.clusterCoef <- transitivity(BL06_TDTable, type="global") #cluster coefficient
BL06_TD.degreeCent <- centralization.degree(BL06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_TDftn <- as.network.matrix(BL06_TDft)
BL06_TD.netDensity <- network.density(BL06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_TD.entropy <- entropy(BL06_TDft) #entropy

BL06_TD.netMx <- cbind(BL06_TD.netMx, BL06_TD.clusterCoef, BL06_TD.degreeCent$centralization,
                       BL06_TD.netDensity, BL06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
BL06_QTg2 <- data.frame(BL06_QT)
BL06_QTg2 <- BL06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL06_QTg2$player1
player2vector <- BL06_QTg2$player2
BL06_QTg3 <- BL06_QTg2
BL06_QTg3$p1inp2vec <- is.element(BL06_QTg3$player1, player2vector)
BL06_QTg3$p2inp1vec <- is.element(BL06_QTg3$player2, player1vector)

addPlayer1 <- BL06_QTg3[ which(BL06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL06_QTg3[ which(BL06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL06_QTg2 <- rbind(BL06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
BL06_QTft <- ftable(BL06_QTg2$player1, BL06_QTg2$player2)
BL06_QTft2 <- as.matrix(BL06_QTft)
numRows <- nrow(BL06_QTft2)
numCols <- ncol(BL06_QTft2)
BL06_QTft3 <- BL06_QTft2[c(2:numRows) , c(2:numCols)]
BL06_QTTable <- graph.adjacency(BL06_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(BL06_QTTable, vertex.label = V(BL06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
BL06_QT.clusterCoef <- transitivity(BL06_QTTable, type="global") #cluster coefficient
BL06_QT.degreeCent <- centralization.degree(BL06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL06_QTftn <- as.network.matrix(BL06_QTft)
BL06_QT.netDensity <- network.density(BL06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL06_QT.entropy <- entropy(BL06_QTft) #entropy

BL06_QT.netMx <- cbind(BL06_QT.netMx, BL06_QT.clusterCoef, BL06_QT.degreeCent$centralization,
                       BL06_QT.netDensity, BL06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL06_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "CARL"
KIoutcome = "Goal_F"
CARL06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
CARL06_Gg2 <- data.frame(CARL06_G)
CARL06_Gg2 <- CARL06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_Gg2$player1
player2vector <- CARL06_Gg2$player2
CARL06_Gg3 <- CARL06_Gg2
CARL06_Gg3$p1inp2vec <- is.element(CARL06_Gg3$player1, player2vector)
CARL06_Gg3$p2inp1vec <- is.element(CARL06_Gg3$player2, player1vector)

addPlayer1 <- CARL06_Gg3[ which(CARL06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_Gg3[ which(CARL06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_Gg2 <- rbind(CARL06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
CARL06_Gft <- ftable(CARL06_Gg2$player1, CARL06_Gg2$player2)
CARL06_Gft2 <- as.matrix(CARL06_Gft)
numRows <- nrow(CARL06_Gft2)
numCols <- ncol(CARL06_Gft2)
CARL06_Gft3 <- CARL06_Gft2[c(2:numRows) , c(2:numCols)]
CARL06_GTable <- graph.adjacency(CARL06_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(CARL06_GTable, vertex.label = V(CARL06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
CARL06_G.clusterCoef <- transitivity(CARL06_GTable, type="global") #cluster coefficient
CARL06_G.degreeCent <- centralization.degree(CARL06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_Gftn <- as.network.matrix(CARL06_Gft)
CARL06_G.netDensity <- network.density(CARL06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_G.entropy <- entropy(CARL06_Gft) #entropy

CARL06_G.netMx <- cbind(CARL06_G.netMx, CARL06_G.clusterCoef, CARL06_G.degreeCent$centralization,
                        CARL06_G.netDensity, CARL06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************
#NA

round = 6
teamName = "CARL"
KIoutcome = "Behind_F"
CARL06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
CARL06_Bg2 <- data.frame(CARL06_B)
CARL06_Bg2 <- CARL06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_Bg2$player1
player2vector <- CARL06_Bg2$player2
CARL06_Bg3 <- CARL06_Bg2
CARL06_Bg3$p1inp2vec <- is.element(CARL06_Bg3$player1, player2vector)
CARL06_Bg3$p2inp1vec <- is.element(CARL06_Bg3$player2, player1vector)

addPlayer1 <- CARL06_Bg3[ which(CARL06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_Bg3[ which(CARL06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_Bg2 <- rbind(CARL06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
CARL06_Bft <- ftable(CARL06_Bg2$player1, CARL06_Bg2$player2)
CARL06_Bft2 <- as.matrix(CARL06_Bft)
numRows <- nrow(CARL06_Bft2)
numCols <- ncol(CARL06_Bft2)
CARL06_Bft3 <- CARL06_Bft2[c(2:numRows) , c(2:numCols)]
CARL06_BTable <- graph.adjacency(CARL06_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(CARL06_BTable, vertex.label = V(CARL06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
CARL06_B.clusterCoef <- transitivity(CARL06_BTable, type="global") #cluster coefficient
CARL06_B.degreeCent <- centralization.degree(CARL06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_Bftn <- as.network.matrix(CARL06_Bft)
CARL06_B.netDensity <- network.density(CARL06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_B.entropy <- entropy(CARL06_Bft) #entropy

CARL06_B.netMx <- cbind(CARL06_B.netMx, CARL06_B.clusterCoef, CARL06_B.degreeCent$centralization,
                        CARL06_B.netDensity, CARL06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
CARL06_SFg2 <- data.frame(CARL06_SF)
CARL06_SFg2 <- CARL06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_SFg2$player1
player2vector <- CARL06_SFg2$player2
CARL06_SFg3 <- CARL06_SFg2
CARL06_SFg3$p1inp2vec <- is.element(CARL06_SFg3$player1, player2vector)
CARL06_SFg3$p2inp1vec <- is.element(CARL06_SFg3$player2, player1vector)

addPlayer1 <- CARL06_SFg3[ which(CARL06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_SFg3[ which(CARL06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_SFg2 <- rbind(CARL06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
CARL06_SFft <- ftable(CARL06_SFg2$player1, CARL06_SFg2$player2)
CARL06_SFft2 <- as.matrix(CARL06_SFft)
numRows <- nrow(CARL06_SFft2)
numCols <- ncol(CARL06_SFft2)
CARL06_SFft3 <- CARL06_SFft2[c(2:numRows) , c(2:numCols)]
CARL06_SFTable <- graph.adjacency(CARL06_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(CARL06_SFTable, vertex.label = V(CARL06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
CARL06_SF.clusterCoef <- transitivity(CARL06_SFTable, type="global") #cluster coefficient
CARL06_SF.degreeCent <- centralization.degree(CARL06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_SFftn <- as.network.matrix(CARL06_SFft)
CARL06_SF.netDensity <- network.density(CARL06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_SF.entropy <- entropy(CARL06_SFft) #entropy

CARL06_SF.netMx <- cbind(CARL06_SF.netMx, CARL06_SF.clusterCoef, CARL06_SF.degreeCent$centralization,
                         CARL06_SF.netDensity, CARL06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
CARL06_TFg2 <- data.frame(CARL06_TF)
CARL06_TFg2 <- CARL06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_TFg2$player1
player2vector <- CARL06_TFg2$player2
CARL06_TFg3 <- CARL06_TFg2
CARL06_TFg3$p1inp2vec <- is.element(CARL06_TFg3$player1, player2vector)
CARL06_TFg3$p2inp1vec <- is.element(CARL06_TFg3$player2, player1vector)

addPlayer1 <- CARL06_TFg3[ which(CARL06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_TFg3[ which(CARL06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_TFg2 <- rbind(CARL06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
CARL06_TFft <- ftable(CARL06_TFg2$player1, CARL06_TFg2$player2)
CARL06_TFft2 <- as.matrix(CARL06_TFft)
numRows <- nrow(CARL06_TFft2)
numCols <- ncol(CARL06_TFft2)
CARL06_TFft3 <- CARL06_TFft2[c(2:numRows) , c(2:numCols)]
CARL06_TFTable <- graph.adjacency(CARL06_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(CARL06_TFTable, vertex.label = V(CARL06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
CARL06_TF.clusterCoef <- transitivity(CARL06_TFTable, type="global") #cluster coefficient
CARL06_TF.degreeCent <- centralization.degree(CARL06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_TFftn <- as.network.matrix(CARL06_TFft)
CARL06_TF.netDensity <- network.density(CARL06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_TF.entropy <- entropy(CARL06_TFft) #entropy

CARL06_TF.netMx <- cbind(CARL06_TF.netMx, CARL06_TF.clusterCoef, CARL06_TF.degreeCent$centralization,
                         CARL06_TF.netDensity, CARL06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************

round = 6
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
CARL06_SAMg2 <- data.frame(CARL06_SAM)
CARL06_SAMg2 <- CARL06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_SAMg2$player1
player2vector <- CARL06_SAMg2$player2
CARL06_SAMg3 <- CARL06_SAMg2
CARL06_SAMg3$p1inp2vec <- is.element(CARL06_SAMg3$player1, player2vector)
CARL06_SAMg3$p2inp1vec <- is.element(CARL06_SAMg3$player2, player1vector)

addPlayer1 <- CARL06_SAMg3[ which(CARL06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_SAMg3[ which(CARL06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_SAMg2 <- rbind(CARL06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
CARL06_SAMft <- ftable(CARL06_SAMg2$player1, CARL06_SAMg2$player2)
CARL06_SAMft2 <- as.matrix(CARL06_SAMft)
numRows <- nrow(CARL06_SAMft2)
numCols <- ncol(CARL06_SAMft2)
CARL06_SAMft3 <- CARL06_SAMft2[c(2:numRows) , c(2:numCols)]
CARL06_SAMTable <- graph.adjacency(CARL06_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(CARL06_SAMTable, vertex.label = V(CARL06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
CARL06_SAM.clusterCoef <- transitivity(CARL06_SAMTable, type="global") #cluster coefficient
CARL06_SAM.degreeCent <- centralization.degree(CARL06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_SAMftn <- as.network.matrix(CARL06_SAMft)
CARL06_SAM.netDensity <- network.density(CARL06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_SAM.entropy <- entropy(CARL06_SAMft) #entropy

CARL06_SAM.netMx <- cbind(CARL06_SAM.netMx, CARL06_SAM.clusterCoef, CARL06_SAM.degreeCent$centralization,
                          CARL06_SAM.netDensity, CARL06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************
#NA

round = 6
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
CARL06_TAMg2 <- data.frame(CARL06_TAM)
CARL06_TAMg2 <- CARL06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_TAMg2$player1
player2vector <- CARL06_TAMg2$player2
CARL06_TAMg3 <- CARL06_TAMg2
CARL06_TAMg3$p1inp2vec <- is.element(CARL06_TAMg3$player1, player2vector)
CARL06_TAMg3$p2inp1vec <- is.element(CARL06_TAMg3$player2, player1vector)

addPlayer1 <- CARL06_TAMg3[ which(CARL06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_TAMg3[ which(CARL06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_TAMg2 <- rbind(CARL06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
CARL06_TAMft <- ftable(CARL06_TAMg2$player1, CARL06_TAMg2$player2)
CARL06_TAMft2 <- as.matrix(CARL06_TAMft)
numRows <- nrow(CARL06_TAMft2)
numCols <- ncol(CARL06_TAMft2)
CARL06_TAMft3 <- CARL06_TAMft2[c(2:numRows) , c(2:numCols)]
CARL06_TAMTable <- graph.adjacency(CARL06_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(CARL06_TAMTable, vertex.label = V(CARL06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
CARL06_TAM.clusterCoef <- transitivity(CARL06_TAMTable, type="global") #cluster coefficient
CARL06_TAM.degreeCent <- centralization.degree(CARL06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_TAMftn <- as.network.matrix(CARL06_TAMft)
CARL06_TAM.netDensity <- network.density(CARL06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_TAM.entropy <- entropy(CARL06_TAMft) #entropy

CARL06_TAM.netMx <- cbind(CARL06_TAM.netMx, CARL06_TAM.clusterCoef, CARL06_TAM.degreeCent$centralization,
                          CARL06_TAM.netDensity, CARL06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
CARL06_SDMg2 <- data.frame(CARL06_SDM)
CARL06_SDMg2 <- CARL06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_SDMg2$player1
player2vector <- CARL06_SDMg2$player2
CARL06_SDMg3 <- CARL06_SDMg2
CARL06_SDMg3$p1inp2vec <- is.element(CARL06_SDMg3$player1, player2vector)
CARL06_SDMg3$p2inp1vec <- is.element(CARL06_SDMg3$player2, player1vector)

addPlayer1 <- CARL06_SDMg3[ which(CARL06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_SDMg3[ which(CARL06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_SDMg2 <- rbind(CARL06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
CARL06_SDMft <- ftable(CARL06_SDMg2$player1, CARL06_SDMg2$player2)
CARL06_SDMft2 <- as.matrix(CARL06_SDMft)
numRows <- nrow(CARL06_SDMft2)
numCols <- ncol(CARL06_SDMft2)
CARL06_SDMft3 <- CARL06_SDMft2[c(2:numRows) , c(2:numCols)]
CARL06_SDMTable <- graph.adjacency(CARL06_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(CARL06_SDMTable, vertex.label = V(CARL06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
CARL06_SDM.clusterCoef <- transitivity(CARL06_SDMTable, type="global") #cluster coefficient
CARL06_SDM.degreeCent <- centralization.degree(CARL06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_SDMftn <- as.network.matrix(CARL06_SDMft)
CARL06_SDM.netDensity <- network.density(CARL06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_SDM.entropy <- entropy(CARL06_SDMft) #entropy

CARL06_SDM.netMx <- cbind(CARL06_SDM.netMx, CARL06_SDM.clusterCoef, CARL06_SDM.degreeCent$centralization,
                          CARL06_SDM.netDensity, CARL06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
CARL06_TDMg2 <- data.frame(CARL06_TDM)
CARL06_TDMg2 <- CARL06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_TDMg2$player1
player2vector <- CARL06_TDMg2$player2
CARL06_TDMg3 <- CARL06_TDMg2
CARL06_TDMg3$p1inp2vec <- is.element(CARL06_TDMg3$player1, player2vector)
CARL06_TDMg3$p2inp1vec <- is.element(CARL06_TDMg3$player2, player1vector)

addPlayer1 <- CARL06_TDMg3[ which(CARL06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_TDMg3[ which(CARL06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_TDMg2 <- rbind(CARL06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
CARL06_TDMft <- ftable(CARL06_TDMg2$player1, CARL06_TDMg2$player2)
CARL06_TDMft2 <- as.matrix(CARL06_TDMft)
numRows <- nrow(CARL06_TDMft2)
numCols <- ncol(CARL06_TDMft2)
CARL06_TDMft3 <- CARL06_TDMft2[c(2:numRows) , c(2:numCols)]
CARL06_TDMTable <- graph.adjacency(CARL06_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(CARL06_TDMTable, vertex.label = V(CARL06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
CARL06_TDM.clusterCoef <- transitivity(CARL06_TDMTable, type="global") #cluster coefficient
CARL06_TDM.degreeCent <- centralization.degree(CARL06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_TDMftn <- as.network.matrix(CARL06_TDMft)
CARL06_TDM.netDensity <- network.density(CARL06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_TDM.entropy <- entropy(CARL06_TDMft) #entropy

CARL06_TDM.netMx <- cbind(CARL06_TDM.netMx, CARL06_TDM.clusterCoef, CARL06_TDM.degreeCent$centralization,
                          CARL06_TDM.netDensity, CARL06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
CARL06_SDg2 <- data.frame(CARL06_SD)
CARL06_SDg2 <- CARL06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_SDg2$player1
player2vector <- CARL06_SDg2$player2
CARL06_SDg3 <- CARL06_SDg2
CARL06_SDg3$p1inp2vec <- is.element(CARL06_SDg3$player1, player2vector)
CARL06_SDg3$p2inp1vec <- is.element(CARL06_SDg3$player2, player1vector)

addPlayer1 <- CARL06_SDg3[ which(CARL06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_SDg3[ which(CARL06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_SDg2 <- rbind(CARL06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
CARL06_SDft <- ftable(CARL06_SDg2$player1, CARL06_SDg2$player2)
CARL06_SDft2 <- as.matrix(CARL06_SDft)
numRows <- nrow(CARL06_SDft2)
numCols <- ncol(CARL06_SDft2)
CARL06_SDft3 <- CARL06_SDft2[c(2:numRows) , c(2:numCols)]
CARL06_SDTable <- graph.adjacency(CARL06_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(CARL06_SDTable, vertex.label = V(CARL06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
CARL06_SD.clusterCoef <- transitivity(CARL06_SDTable, type="global") #cluster coefficient
CARL06_SD.degreeCent <- centralization.degree(CARL06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_SDftn <- as.network.matrix(CARL06_SDft)
CARL06_SD.netDensity <- network.density(CARL06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_SD.entropy <- entropy(CARL06_SDft) #entropy

CARL06_SD.netMx <- cbind(CARL06_SD.netMx, CARL06_SD.clusterCoef, CARL06_SD.degreeCent$centralization,
                         CARL06_SD.netDensity, CARL06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************

round = 6
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
CARL06_TDg2 <- data.frame(CARL06_TD)
CARL06_TDg2 <- CARL06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_TDg2$player1
player2vector <- CARL06_TDg2$player2
CARL06_TDg3 <- CARL06_TDg2
CARL06_TDg3$p1inp2vec <- is.element(CARL06_TDg3$player1, player2vector)
CARL06_TDg3$p2inp1vec <- is.element(CARL06_TDg3$player2, player1vector)

addPlayer1 <- CARL06_TDg3[ which(CARL06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_TDg3[ which(CARL06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_TDg2 <- rbind(CARL06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
CARL06_TDft <- ftable(CARL06_TDg2$player1, CARL06_TDg2$player2)
CARL06_TDft2 <- as.matrix(CARL06_TDft)
numRows <- nrow(CARL06_TDft2)
numCols <- ncol(CARL06_TDft2)
CARL06_TDft3 <- CARL06_TDft2[c(2:numRows) , c(2:numCols)]
CARL06_TDTable <- graph.adjacency(CARL06_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(CARL06_TDTable, vertex.label = V(CARL06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
CARL06_TD.clusterCoef <- transitivity(CARL06_TDTable, type="global") #cluster coefficient
CARL06_TD.degreeCent <- centralization.degree(CARL06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_TDftn <- as.network.matrix(CARL06_TDft)
CARL06_TD.netDensity <- network.density(CARL06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_TD.entropy <- entropy(CARL06_TDft) #entropy

CARL06_TD.netMx <- cbind(CARL06_TD.netMx, CARL06_TD.clusterCoef, CARL06_TD.degreeCent$centralization,
                         CARL06_TD.netDensity, CARL06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
CARL06_QTg2 <- data.frame(CARL06_QT)
CARL06_QTg2 <- CARL06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL06_QTg2$player1
player2vector <- CARL06_QTg2$player2
CARL06_QTg3 <- CARL06_QTg2
CARL06_QTg3$p1inp2vec <- is.element(CARL06_QTg3$player1, player2vector)
CARL06_QTg3$p2inp1vec <- is.element(CARL06_QTg3$player2, player1vector)

addPlayer1 <- CARL06_QTg3[ which(CARL06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL06_QTg3[ which(CARL06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL06_QTg2 <- rbind(CARL06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
CARL06_QTft <- ftable(CARL06_QTg2$player1, CARL06_QTg2$player2)
CARL06_QTft2 <- as.matrix(CARL06_QTft)
numRows <- nrow(CARL06_QTft2)
numCols <- ncol(CARL06_QTft2)
CARL06_QTft3 <- CARL06_QTft2[c(2:numRows) , c(2:numCols)]
CARL06_QTTable <- graph.adjacency(CARL06_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(CARL06_QTTable, vertex.label = V(CARL06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
CARL06_QT.clusterCoef <- transitivity(CARL06_QTTable, type="global") #cluster coefficient
CARL06_QT.degreeCent <- centralization.degree(CARL06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL06_QTftn <- as.network.matrix(CARL06_QTft)
CARL06_QT.netDensity <- network.density(CARL06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL06_QT.entropy <- entropy(CARL06_QTft) #entropy

CARL06_QT.netMx <- cbind(CARL06_QT.netMx, CARL06_QT.clusterCoef, CARL06_QT.degreeCent$centralization,
                         CARL06_QT.netDensity, CARL06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL06_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************

round = 6
teamName = "COLL"
KIoutcome = "Goal_F"
COLL06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
COLL06_Gg2 <- data.frame(COLL06_G)
COLL06_Gg2 <- COLL06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_Gg2$player1
player2vector <- COLL06_Gg2$player2
COLL06_Gg3 <- COLL06_Gg2
COLL06_Gg3$p1inp2vec <- is.element(COLL06_Gg3$player1, player2vector)
COLL06_Gg3$p2inp1vec <- is.element(COLL06_Gg3$player2, player1vector)

addPlayer1 <- COLL06_Gg3[ which(COLL06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_Gg3[ which(COLL06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_Gg2 <- rbind(COLL06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
COLL06_Gft <- ftable(COLL06_Gg2$player1, COLL06_Gg2$player2)
COLL06_Gft2 <- as.matrix(COLL06_Gft)
numRows <- nrow(COLL06_Gft2)
numCols <- ncol(COLL06_Gft2)
COLL06_Gft3 <- COLL06_Gft2[c(2:numRows) , c(2:numCols)]
COLL06_GTable <- graph.adjacency(COLL06_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(COLL06_GTable, vertex.label = V(COLL06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
COLL06_G.clusterCoef <- transitivity(COLL06_GTable, type="global") #cluster coefficient
COLL06_G.degreeCent <- centralization.degree(COLL06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_Gftn <- as.network.matrix(COLL06_Gft)
COLL06_G.netDensity <- network.density(COLL06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_G.entropy <- entropy(COLL06_Gft) #entropy

COLL06_G.netMx <- cbind(COLL06_G.netMx, COLL06_G.clusterCoef, COLL06_G.degreeCent$centralization,
                        COLL06_G.netDensity, COLL06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "COLL"
KIoutcome = "Behind_F"
COLL06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
COLL06_Bg2 <- data.frame(COLL06_B)
COLL06_Bg2 <- COLL06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_Bg2$player1
player2vector <- COLL06_Bg2$player2
COLL06_Bg3 <- COLL06_Bg2
COLL06_Bg3$p1inp2vec <- is.element(COLL06_Bg3$player1, player2vector)
COLL06_Bg3$p2inp1vec <- is.element(COLL06_Bg3$player2, player1vector)

addPlayer1 <- COLL06_Bg3[ which(COLL06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_Bg3[ which(COLL06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_Bg2 <- rbind(COLL06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
COLL06_Bft <- ftable(COLL06_Bg2$player1, COLL06_Bg2$player2)
COLL06_Bft2 <- as.matrix(COLL06_Bft)
numRows <- nrow(COLL06_Bft2)
numCols <- ncol(COLL06_Bft2)
COLL06_Bft3 <- COLL06_Bft2[c(2:numRows) , c(2:numCols)]
COLL06_BTable <- graph.adjacency(COLL06_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(COLL06_BTable, vertex.label = V(COLL06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
COLL06_B.clusterCoef <- transitivity(COLL06_BTable, type="global") #cluster coefficient
COLL06_B.degreeCent <- centralization.degree(COLL06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_Bftn <- as.network.matrix(COLL06_Bft)
COLL06_B.netDensity <- network.density(COLL06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_B.entropy <- entropy(COLL06_Bft) #entropy

COLL06_B.netMx <- cbind(COLL06_B.netMx, COLL06_B.clusterCoef, COLL06_B.degreeCent$centralization,
                        COLL06_B.netDensity, COLL06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************

round = 6
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
COLL06_SFg2 <- data.frame(COLL06_SF)
COLL06_SFg2 <- COLL06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_SFg2$player1
player2vector <- COLL06_SFg2$player2
COLL06_SFg3 <- COLL06_SFg2
COLL06_SFg3$p1inp2vec <- is.element(COLL06_SFg3$player1, player2vector)
COLL06_SFg3$p2inp1vec <- is.element(COLL06_SFg3$player2, player1vector)

addPlayer1 <- COLL06_SFg3[ which(COLL06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_SFg3[ which(COLL06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_SFg2 <- rbind(COLL06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
COLL06_SFft <- ftable(COLL06_SFg2$player1, COLL06_SFg2$player2)
COLL06_SFft2 <- as.matrix(COLL06_SFft)
numRows <- nrow(COLL06_SFft2)
numCols <- ncol(COLL06_SFft2)
COLL06_SFft3 <- COLL06_SFft2[c(2:numRows) , c(2:numCols)]
COLL06_SFTable <- graph.adjacency(COLL06_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(COLL06_SFTable, vertex.label = V(COLL06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
COLL06_SF.clusterCoef <- transitivity(COLL06_SFTable, type="global") #cluster coefficient
COLL06_SF.degreeCent <- centralization.degree(COLL06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_SFftn <- as.network.matrix(COLL06_SFft)
COLL06_SF.netDensity <- network.density(COLL06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_SF.entropy <- entropy(COLL06_SFft) #entropy

COLL06_SF.netMx <- cbind(COLL06_SF.netMx, COLL06_SF.clusterCoef, COLL06_SF.degreeCent$centralization,
                         COLL06_SF.netDensity, COLL06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
COLL06_TFg2 <- data.frame(COLL06_TF)
COLL06_TFg2 <- COLL06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_TFg2$player1
player2vector <- COLL06_TFg2$player2
COLL06_TFg3 <- COLL06_TFg2
COLL06_TFg3$p1inp2vec <- is.element(COLL06_TFg3$player1, player2vector)
COLL06_TFg3$p2inp1vec <- is.element(COLL06_TFg3$player2, player1vector)

addPlayer1 <- COLL06_TFg3[ which(COLL06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_TFg3[ which(COLL06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_TFg2 <- rbind(COLL06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
COLL06_TFft <- ftable(COLL06_TFg2$player1, COLL06_TFg2$player2)
COLL06_TFft2 <- as.matrix(COLL06_TFft)
numRows <- nrow(COLL06_TFft2)
numCols <- ncol(COLL06_TFft2)
COLL06_TFft3 <- COLL06_TFft2[c(2:numRows) , c(2:numCols)]
COLL06_TFTable <- graph.adjacency(COLL06_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(COLL06_TFTable, vertex.label = V(COLL06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
COLL06_TF.clusterCoef <- transitivity(COLL06_TFTable, type="global") #cluster coefficient
COLL06_TF.degreeCent <- centralization.degree(COLL06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_TFftn <- as.network.matrix(COLL06_TFft)
COLL06_TF.netDensity <- network.density(COLL06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_TF.entropy <- entropy(COLL06_TFft) #entropy

COLL06_TF.netMx <- cbind(COLL06_TF.netMx, COLL06_TF.clusterCoef, COLL06_TF.degreeCent$centralization,
                         COLL06_TF.netDensity, COLL06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
COLL06_SAMg2 <- data.frame(COLL06_SAM)
COLL06_SAMg2 <- COLL06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_SAMg2$player1
player2vector <- COLL06_SAMg2$player2
COLL06_SAMg3 <- COLL06_SAMg2
COLL06_SAMg3$p1inp2vec <- is.element(COLL06_SAMg3$player1, player2vector)
COLL06_SAMg3$p2inp1vec <- is.element(COLL06_SAMg3$player2, player1vector)

addPlayer1 <- COLL06_SAMg3[ which(COLL06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_SAMg3[ which(COLL06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_SAMg2 <- rbind(COLL06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
COLL06_SAMft <- ftable(COLL06_SAMg2$player1, COLL06_SAMg2$player2)
COLL06_SAMft2 <- as.matrix(COLL06_SAMft)
numRows <- nrow(COLL06_SAMft2)
numCols <- ncol(COLL06_SAMft2)
COLL06_SAMft3 <- COLL06_SAMft2[c(2:numRows) , c(2:numCols)]
COLL06_SAMTable <- graph.adjacency(COLL06_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(COLL06_SAMTable, vertex.label = V(COLL06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
COLL06_SAM.clusterCoef <- transitivity(COLL06_SAMTable, type="global") #cluster coefficient
COLL06_SAM.degreeCent <- centralization.degree(COLL06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_SAMftn <- as.network.matrix(COLL06_SAMft)
COLL06_SAM.netDensity <- network.density(COLL06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_SAM.entropy <- entropy(COLL06_SAMft) #entropy

COLL06_SAM.netMx <- cbind(COLL06_SAM.netMx, COLL06_SAM.clusterCoef, COLL06_SAM.degreeCent$centralization,
                          COLL06_SAM.netDensity, COLL06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************
#NA

round = 6
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
COLL06_TAMg2 <- data.frame(COLL06_TAM)
COLL06_TAMg2 <- COLL06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_TAMg2$player1
player2vector <- COLL06_TAMg2$player2
COLL06_TAMg3 <- COLL06_TAMg2
COLL06_TAMg3$p1inp2vec <- is.element(COLL06_TAMg3$player1, player2vector)
COLL06_TAMg3$p2inp1vec <- is.element(COLL06_TAMg3$player2, player1vector)

addPlayer1 <- COLL06_TAMg3[ which(COLL06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_TAMg3[ which(COLL06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_TAMg2 <- rbind(COLL06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
COLL06_TAMft <- ftable(COLL06_TAMg2$player1, COLL06_TAMg2$player2)
COLL06_TAMft2 <- as.matrix(COLL06_TAMft)
numRows <- nrow(COLL06_TAMft2)
numCols <- ncol(COLL06_TAMft2)
COLL06_TAMft3 <- COLL06_TAMft2[c(2:numRows) , c(2:numCols)]
COLL06_TAMTable <- graph.adjacency(COLL06_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(COLL06_TAMTable, vertex.label = V(COLL06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
COLL06_TAM.clusterCoef <- transitivity(COLL06_TAMTable, type="global") #cluster coefficient
COLL06_TAM.degreeCent <- centralization.degree(COLL06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_TAMftn <- as.network.matrix(COLL06_TAMft)
COLL06_TAM.netDensity <- network.density(COLL06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_TAM.entropy <- entropy(COLL06_TAMft) #entropy

COLL06_TAM.netMx <- cbind(COLL06_TAM.netMx, COLL06_TAM.clusterCoef, COLL06_TAM.degreeCent$centralization,
                          COLL06_TAM.netDensity, COLL06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
COLL06_SDMg2 <- data.frame(COLL06_SDM)
COLL06_SDMg2 <- COLL06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_SDMg2$player1
player2vector <- COLL06_SDMg2$player2
COLL06_SDMg3 <- COLL06_SDMg2
COLL06_SDMg3$p1inp2vec <- is.element(COLL06_SDMg3$player1, player2vector)
COLL06_SDMg3$p2inp1vec <- is.element(COLL06_SDMg3$player2, player1vector)

addPlayer1 <- COLL06_SDMg3[ which(COLL06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_SDMg3[ which(COLL06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_SDMg2 <- rbind(COLL06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
COLL06_SDMft <- ftable(COLL06_SDMg2$player1, COLL06_SDMg2$player2)
COLL06_SDMft2 <- as.matrix(COLL06_SDMft)
numRows <- nrow(COLL06_SDMft2)
numCols <- ncol(COLL06_SDMft2)
COLL06_SDMft3 <- COLL06_SDMft2[c(2:numRows) , c(2:numCols)]
COLL06_SDMTable <- graph.adjacency(COLL06_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(COLL06_SDMTable, vertex.label = V(COLL06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
COLL06_SDM.clusterCoef <- transitivity(COLL06_SDMTable, type="global") #cluster coefficient
COLL06_SDM.degreeCent <- centralization.degree(COLL06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_SDMftn <- as.network.matrix(COLL06_SDMft)
COLL06_SDM.netDensity <- network.density(COLL06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_SDM.entropy <- entropy(COLL06_SDMft) #entropy

COLL06_SDM.netMx <- cbind(COLL06_SDM.netMx, COLL06_SDM.clusterCoef, COLL06_SDM.degreeCent$centralization,
                          COLL06_SDM.netDensity, COLL06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
COLL06_TDMg2 <- data.frame(COLL06_TDM)
COLL06_TDMg2 <- COLL06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_TDMg2$player1
player2vector <- COLL06_TDMg2$player2
COLL06_TDMg3 <- COLL06_TDMg2
COLL06_TDMg3$p1inp2vec <- is.element(COLL06_TDMg3$player1, player2vector)
COLL06_TDMg3$p2inp1vec <- is.element(COLL06_TDMg3$player2, player1vector)

addPlayer1 <- COLL06_TDMg3[ which(COLL06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_TDMg3[ which(COLL06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_TDMg2 <- rbind(COLL06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
COLL06_TDMft <- ftable(COLL06_TDMg2$player1, COLL06_TDMg2$player2)
COLL06_TDMft2 <- as.matrix(COLL06_TDMft)
numRows <- nrow(COLL06_TDMft2)
numCols <- ncol(COLL06_TDMft2)
COLL06_TDMft3 <- COLL06_TDMft2[c(2:numRows) , c(2:numCols)]
COLL06_TDMTable <- graph.adjacency(COLL06_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(COLL06_TDMTable, vertex.label = V(COLL06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
COLL06_TDM.clusterCoef <- transitivity(COLL06_TDMTable, type="global") #cluster coefficient
COLL06_TDM.degreeCent <- centralization.degree(COLL06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_TDMftn <- as.network.matrix(COLL06_TDMft)
COLL06_TDM.netDensity <- network.density(COLL06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_TDM.entropy <- entropy(COLL06_TDMft) #entropy

COLL06_TDM.netMx <- cbind(COLL06_TDM.netMx, COLL06_TDM.clusterCoef, COLL06_TDM.degreeCent$centralization,
                          COLL06_TDM.netDensity, COLL06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
COLL06_SDg2 <- data.frame(COLL06_SD)
COLL06_SDg2 <- COLL06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_SDg2$player1
player2vector <- COLL06_SDg2$player2
COLL06_SDg3 <- COLL06_SDg2
COLL06_SDg3$p1inp2vec <- is.element(COLL06_SDg3$player1, player2vector)
COLL06_SDg3$p2inp1vec <- is.element(COLL06_SDg3$player2, player1vector)

addPlayer1 <- COLL06_SDg3[ which(COLL06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_SDg3[ which(COLL06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_SDg2 <- rbind(COLL06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
COLL06_SDft <- ftable(COLL06_SDg2$player1, COLL06_SDg2$player2)
COLL06_SDft2 <- as.matrix(COLL06_SDft)
numRows <- nrow(COLL06_SDft2)
numCols <- ncol(COLL06_SDft2)
COLL06_SDft3 <- COLL06_SDft2[c(2:numRows) , c(2:numCols)]
COLL06_SDTable <- graph.adjacency(COLL06_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(COLL06_SDTable, vertex.label = V(COLL06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
COLL06_SD.clusterCoef <- transitivity(COLL06_SDTable, type="global") #cluster coefficient
COLL06_SD.degreeCent <- centralization.degree(COLL06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_SDftn <- as.network.matrix(COLL06_SDft)
COLL06_SD.netDensity <- network.density(COLL06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_SD.entropy <- entropy(COLL06_SDft) #entropy

COLL06_SD.netMx <- cbind(COLL06_SD.netMx, COLL06_SD.clusterCoef, COLL06_SD.degreeCent$centralization,
                         COLL06_SD.netDensity, COLL06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
COLL06_TDg2 <- data.frame(COLL06_TD)
COLL06_TDg2 <- COLL06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_TDg2$player1
player2vector <- COLL06_TDg2$player2
COLL06_TDg3 <- COLL06_TDg2
COLL06_TDg3$p1inp2vec <- is.element(COLL06_TDg3$player1, player2vector)
COLL06_TDg3$p2inp1vec <- is.element(COLL06_TDg3$player2, player1vector)

addPlayer1 <- COLL06_TDg3[ which(COLL06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_TDg3[ which(COLL06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_TDg2 <- rbind(COLL06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
COLL06_TDft <- ftable(COLL06_TDg2$player1, COLL06_TDg2$player2)
COLL06_TDft2 <- as.matrix(COLL06_TDft)
numRows <- nrow(COLL06_TDft2)
numCols <- ncol(COLL06_TDft2)
COLL06_TDft3 <- COLL06_TDft2[c(2:numRows) , c(2:numCols)]
COLL06_TDTable <- graph.adjacency(COLL06_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(COLL06_TDTable, vertex.label = V(COLL06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
COLL06_TD.clusterCoef <- transitivity(COLL06_TDTable, type="global") #cluster coefficient
COLL06_TD.degreeCent <- centralization.degree(COLL06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_TDftn <- as.network.matrix(COLL06_TDft)
COLL06_TD.netDensity <- network.density(COLL06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_TD.entropy <- entropy(COLL06_TDft) #entropy

COLL06_TD.netMx <- cbind(COLL06_TD.netMx, COLL06_TD.clusterCoef, COLL06_TD.degreeCent$centralization,
                         COLL06_TD.netDensity, COLL06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
COLL06_QTg2 <- data.frame(COLL06_QT)
COLL06_QTg2 <- COLL06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL06_QTg2$player1
player2vector <- COLL06_QTg2$player2
COLL06_QTg3 <- COLL06_QTg2
COLL06_QTg3$p1inp2vec <- is.element(COLL06_QTg3$player1, player2vector)
COLL06_QTg3$p2inp1vec <- is.element(COLL06_QTg3$player2, player1vector)

addPlayer1 <- COLL06_QTg3[ which(COLL06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL06_QTg3[ which(COLL06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL06_QTg2 <- rbind(COLL06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
COLL06_QTft <- ftable(COLL06_QTg2$player1, COLL06_QTg2$player2)
COLL06_QTft2 <- as.matrix(COLL06_QTft)
numRows <- nrow(COLL06_QTft2)
numCols <- ncol(COLL06_QTft2)
COLL06_QTft3 <- COLL06_QTft2[c(2:numRows) , c(2:numCols)]
COLL06_QTTable <- graph.adjacency(COLL06_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(COLL06_QTTable, vertex.label = V(COLL06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
COLL06_QT.clusterCoef <- transitivity(COLL06_QTTable, type="global") #cluster coefficient
COLL06_QT.degreeCent <- centralization.degree(COLL06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL06_QTftn <- as.network.matrix(COLL06_QTft)
COLL06_QT.netDensity <- network.density(COLL06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL06_QT.entropy <- entropy(COLL06_QTft) #entropy

COLL06_QT.netMx <- cbind(COLL06_QT.netMx, COLL06_QT.clusterCoef, COLL06_QT.degreeCent$centralization,
                         COLL06_QT.netDensity, COLL06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL06_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "ESS"
KIoutcome = "Goal_F"
ESS06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
ESS06_Gg2 <- data.frame(ESS06_G)
ESS06_Gg2 <- ESS06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_Gg2$player1
player2vector <- ESS06_Gg2$player2
ESS06_Gg3 <- ESS06_Gg2
ESS06_Gg3$p1inp2vec <- is.element(ESS06_Gg3$player1, player2vector)
ESS06_Gg3$p2inp1vec <- is.element(ESS06_Gg3$player2, player1vector)

addPlayer1 <- ESS06_Gg3[ which(ESS06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_Gg3[ which(ESS06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_Gg2 <- rbind(ESS06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
ESS06_Gft <- ftable(ESS06_Gg2$player1, ESS06_Gg2$player2)
ESS06_Gft2 <- as.matrix(ESS06_Gft)
numRows <- nrow(ESS06_Gft2)
numCols <- ncol(ESS06_Gft2)
ESS06_Gft3 <- ESS06_Gft2[c(2:numRows) , c(2:numCols)]
ESS06_GTable <- graph.adjacency(ESS06_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(ESS06_GTable, vertex.label = V(ESS06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
ESS06_G.clusterCoef <- transitivity(ESS06_GTable, type="global") #cluster coefficient
ESS06_G.degreeCent <- centralization.degree(ESS06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_Gftn <- as.network.matrix(ESS06_Gft)
ESS06_G.netDensity <- network.density(ESS06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_G.entropy <- entropy(ESS06_Gft) #entropy

ESS06_G.netMx <- cbind(ESS06_G.netMx, ESS06_G.clusterCoef, ESS06_G.degreeCent$centralization,
                       ESS06_G.netDensity, ESS06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************
#NA

round = 6
teamName = "ESS"
KIoutcome = "Behind_F"
ESS06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
ESS06_Bg2 <- data.frame(ESS06_B)
ESS06_Bg2 <- ESS06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_Bg2$player1
player2vector <- ESS06_Bg2$player2
ESS06_Bg3 <- ESS06_Bg2
ESS06_Bg3$p1inp2vec <- is.element(ESS06_Bg3$player1, player2vector)
ESS06_Bg3$p2inp1vec <- is.element(ESS06_Bg3$player2, player1vector)

addPlayer1 <- ESS06_Bg3[ which(ESS06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_Bg3[ which(ESS06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_Bg2 <- rbind(ESS06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
ESS06_Bft <- ftable(ESS06_Bg2$player1, ESS06_Bg2$player2)
ESS06_Bft2 <- as.matrix(ESS06_Bft)
numRows <- nrow(ESS06_Bft2)
numCols <- ncol(ESS06_Bft2)
ESS06_Bft3 <- ESS06_Bft2[c(2:numRows) , c(2:numCols)]
ESS06_BTable <- graph.adjacency(ESS06_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(ESS06_BTable, vertex.label = V(ESS06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
ESS06_B.clusterCoef <- transitivity(ESS06_BTable, type="global") #cluster coefficient
ESS06_B.degreeCent <- centralization.degree(ESS06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_Bftn <- as.network.matrix(ESS06_Bft)
ESS06_B.netDensity <- network.density(ESS06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_B.entropy <- entropy(ESS06_Bft) #entropy

ESS06_B.netMx <- cbind(ESS06_B.netMx, ESS06_B.clusterCoef, ESS06_B.degreeCent$centralization,
                       ESS06_B.netDensity, ESS06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
ESS06_SFg2 <- data.frame(ESS06_SF)
ESS06_SFg2 <- ESS06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_SFg2$player1
player2vector <- ESS06_SFg2$player2
ESS06_SFg3 <- ESS06_SFg2
ESS06_SFg3$p1inp2vec <- is.element(ESS06_SFg3$player1, player2vector)
ESS06_SFg3$p2inp1vec <- is.element(ESS06_SFg3$player2, player1vector)

addPlayer1 <- ESS06_SFg3[ which(ESS06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_SFg3[ which(ESS06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_SFg2 <- rbind(ESS06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
ESS06_SFft <- ftable(ESS06_SFg2$player1, ESS06_SFg2$player2)
ESS06_SFft2 <- as.matrix(ESS06_SFft)
numRows <- nrow(ESS06_SFft2)
numCols <- ncol(ESS06_SFft2)
ESS06_SFft3 <- ESS06_SFft2[c(2:numRows) , c(2:numCols)]
ESS06_SFTable <- graph.adjacency(ESS06_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(ESS06_SFTable, vertex.label = V(ESS06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
ESS06_SF.clusterCoef <- transitivity(ESS06_SFTable, type="global") #cluster coefficient
ESS06_SF.degreeCent <- centralization.degree(ESS06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_SFftn <- as.network.matrix(ESS06_SFft)
ESS06_SF.netDensity <- network.density(ESS06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_SF.entropy <- entropy(ESS06_SFft) #entropy

ESS06_SF.netMx <- cbind(ESS06_SF.netMx, ESS06_SF.clusterCoef, ESS06_SF.degreeCent$centralization,
                        ESS06_SF.netDensity, ESS06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************
#NA

round = 6
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
ESS06_TFg2 <- data.frame(ESS06_TF)
ESS06_TFg2 <- ESS06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_TFg2$player1
player2vector <- ESS06_TFg2$player2
ESS06_TFg3 <- ESS06_TFg2
ESS06_TFg3$p1inp2vec <- is.element(ESS06_TFg3$player1, player2vector)
ESS06_TFg3$p2inp1vec <- is.element(ESS06_TFg3$player2, player1vector)

addPlayer1 <- ESS06_TFg3[ which(ESS06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_TFg3[ which(ESS06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_TFg2 <- rbind(ESS06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
ESS06_TFft <- ftable(ESS06_TFg2$player1, ESS06_TFg2$player2)
ESS06_TFft2 <- as.matrix(ESS06_TFft)
numRows <- nrow(ESS06_TFft2)
numCols <- ncol(ESS06_TFft2)
ESS06_TFft3 <- ESS06_TFft2[c(2:numRows) , c(2:numCols)]
ESS06_TFTable <- graph.adjacency(ESS06_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(ESS06_TFTable, vertex.label = V(ESS06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
ESS06_TF.clusterCoef <- transitivity(ESS06_TFTable, type="global") #cluster coefficient
ESS06_TF.degreeCent <- centralization.degree(ESS06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_TFftn <- as.network.matrix(ESS06_TFft)
ESS06_TF.netDensity <- network.density(ESS06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_TF.entropy <- entropy(ESS06_TFft) #entropy

ESS06_TF.netMx <- cbind(ESS06_TF.netMx, ESS06_TF.clusterCoef, ESS06_TF.degreeCent$centralization,
                        ESS06_TF.netDensity, ESS06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
ESS06_SAMg2 <- data.frame(ESS06_SAM)
ESS06_SAMg2 <- ESS06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_SAMg2$player1
player2vector <- ESS06_SAMg2$player2
ESS06_SAMg3 <- ESS06_SAMg2
ESS06_SAMg3$p1inp2vec <- is.element(ESS06_SAMg3$player1, player2vector)
ESS06_SAMg3$p2inp1vec <- is.element(ESS06_SAMg3$player2, player1vector)

addPlayer1 <- ESS06_SAMg3[ which(ESS06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_SAMg3[ which(ESS06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_SAMg2 <- rbind(ESS06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
ESS06_SAMft <- ftable(ESS06_SAMg2$player1, ESS06_SAMg2$player2)
ESS06_SAMft2 <- as.matrix(ESS06_SAMft)
numRows <- nrow(ESS06_SAMft2)
numCols <- ncol(ESS06_SAMft2)
ESS06_SAMft3 <- ESS06_SAMft2[c(2:numRows) , c(2:numCols)]
ESS06_SAMTable <- graph.adjacency(ESS06_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(ESS06_SAMTable, vertex.label = V(ESS06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
ESS06_SAM.clusterCoef <- transitivity(ESS06_SAMTable, type="global") #cluster coefficient
ESS06_SAM.degreeCent <- centralization.degree(ESS06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_SAMftn <- as.network.matrix(ESS06_SAMft)
ESS06_SAM.netDensity <- network.density(ESS06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_SAM.entropy <- entropy(ESS06_SAMft) #entropy

ESS06_SAM.netMx <- cbind(ESS06_SAM.netMx, ESS06_SAM.clusterCoef, ESS06_SAM.degreeCent$centralization,
                         ESS06_SAM.netDensity, ESS06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************

round = 6
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
ESS06_TAMg2 <- data.frame(ESS06_TAM)
ESS06_TAMg2 <- ESS06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_TAMg2$player1
player2vector <- ESS06_TAMg2$player2
ESS06_TAMg3 <- ESS06_TAMg2
ESS06_TAMg3$p1inp2vec <- is.element(ESS06_TAMg3$player1, player2vector)
ESS06_TAMg3$p2inp1vec <- is.element(ESS06_TAMg3$player2, player1vector)

addPlayer1 <- ESS06_TAMg3[ which(ESS06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_TAMg3[ which(ESS06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_TAMg2 <- rbind(ESS06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
ESS06_TAMft <- ftable(ESS06_TAMg2$player1, ESS06_TAMg2$player2)
ESS06_TAMft2 <- as.matrix(ESS06_TAMft)
numRows <- nrow(ESS06_TAMft2)
numCols <- ncol(ESS06_TAMft2)
ESS06_TAMft3 <- ESS06_TAMft2[c(2:numRows) , c(2:numCols)]
ESS06_TAMTable <- graph.adjacency(ESS06_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(ESS06_TAMTable, vertex.label = V(ESS06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
ESS06_TAM.clusterCoef <- transitivity(ESS06_TAMTable, type="global") #cluster coefficient
ESS06_TAM.degreeCent <- centralization.degree(ESS06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_TAMftn <- as.network.matrix(ESS06_TAMft)
ESS06_TAM.netDensity <- network.density(ESS06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_TAM.entropy <- entropy(ESS06_TAMft) #entropy

ESS06_TAM.netMx <- cbind(ESS06_TAM.netMx, ESS06_TAM.clusterCoef, ESS06_TAM.degreeCent$centralization,
                         ESS06_TAM.netDensity, ESS06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************
#NA

round = 6
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
ESS06_SDMg2 <- data.frame(ESS06_SDM)
ESS06_SDMg2 <- ESS06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_SDMg2$player1
player2vector <- ESS06_SDMg2$player2
ESS06_SDMg3 <- ESS06_SDMg2
ESS06_SDMg3$p1inp2vec <- is.element(ESS06_SDMg3$player1, player2vector)
ESS06_SDMg3$p2inp1vec <- is.element(ESS06_SDMg3$player2, player1vector)

addPlayer1 <- ESS06_SDMg3[ which(ESS06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_SDMg3[ which(ESS06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_SDMg2 <- rbind(ESS06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
ESS06_SDMft <- ftable(ESS06_SDMg2$player1, ESS06_SDMg2$player2)
ESS06_SDMft2 <- as.matrix(ESS06_SDMft)
numRows <- nrow(ESS06_SDMft2)
numCols <- ncol(ESS06_SDMft2)
ESS06_SDMft3 <- ESS06_SDMft2[c(2:numRows) , c(2:numCols)]
ESS06_SDMTable <- graph.adjacency(ESS06_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(ESS06_SDMTable, vertex.label = V(ESS06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
ESS06_SDM.clusterCoef <- transitivity(ESS06_SDMTable, type="global") #cluster coefficient
ESS06_SDM.degreeCent <- centralization.degree(ESS06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_SDMftn <- as.network.matrix(ESS06_SDMft)
ESS06_SDM.netDensity <- network.density(ESS06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_SDM.entropy <- entropy(ESS06_SDMft) #entropy

ESS06_SDM.netMx <- cbind(ESS06_SDM.netMx, ESS06_SDM.clusterCoef, ESS06_SDM.degreeCent$centralization,
                         ESS06_SDM.netDensity, ESS06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
ESS06_TDMg2 <- data.frame(ESS06_TDM)
ESS06_TDMg2 <- ESS06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_TDMg2$player1
player2vector <- ESS06_TDMg2$player2
ESS06_TDMg3 <- ESS06_TDMg2
ESS06_TDMg3$p1inp2vec <- is.element(ESS06_TDMg3$player1, player2vector)
ESS06_TDMg3$p2inp1vec <- is.element(ESS06_TDMg3$player2, player1vector)

addPlayer1 <- ESS06_TDMg3[ which(ESS06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_TDMg3[ which(ESS06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_TDMg2 <- rbind(ESS06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
ESS06_TDMft <- ftable(ESS06_TDMg2$player1, ESS06_TDMg2$player2)
ESS06_TDMft2 <- as.matrix(ESS06_TDMft)
numRows <- nrow(ESS06_TDMft2)
numCols <- ncol(ESS06_TDMft2)
ESS06_TDMft3 <- ESS06_TDMft2[c(2:numRows) , c(2:numCols)]
ESS06_TDMTable <- graph.adjacency(ESS06_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(ESS06_TDMTable, vertex.label = V(ESS06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
ESS06_TDM.clusterCoef <- transitivity(ESS06_TDMTable, type="global") #cluster coefficient
ESS06_TDM.degreeCent <- centralization.degree(ESS06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_TDMftn <- as.network.matrix(ESS06_TDMft)
ESS06_TDM.netDensity <- network.density(ESS06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_TDM.entropy <- entropy(ESS06_TDMft) #entropy

ESS06_TDM.netMx <- cbind(ESS06_TDM.netMx, ESS06_TDM.clusterCoef, ESS06_TDM.degreeCent$centralization,
                         ESS06_TDM.netDensity, ESS06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
ESS06_SDg2 <- data.frame(ESS06_SD)
ESS06_SDg2 <- ESS06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_SDg2$player1
player2vector <- ESS06_SDg2$player2
ESS06_SDg3 <- ESS06_SDg2
ESS06_SDg3$p1inp2vec <- is.element(ESS06_SDg3$player1, player2vector)
ESS06_SDg3$p2inp1vec <- is.element(ESS06_SDg3$player2, player1vector)

addPlayer1 <- ESS06_SDg3[ which(ESS06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_SDg3[ which(ESS06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_SDg2 <- rbind(ESS06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
ESS06_SDft <- ftable(ESS06_SDg2$player1, ESS06_SDg2$player2)
ESS06_SDft2 <- as.matrix(ESS06_SDft)
numRows <- nrow(ESS06_SDft2)
numCols <- ncol(ESS06_SDft2)
ESS06_SDft3 <- ESS06_SDft2[c(2:numRows) , c(2:numCols)]
ESS06_SDTable <- graph.adjacency(ESS06_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(ESS06_SDTable, vertex.label = V(ESS06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
ESS06_SD.clusterCoef <- transitivity(ESS06_SDTable, type="global") #cluster coefficient
ESS06_SD.degreeCent <- centralization.degree(ESS06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_SDftn <- as.network.matrix(ESS06_SDft)
ESS06_SD.netDensity <- network.density(ESS06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_SD.entropy <- entropy(ESS06_SDft) #entropy

ESS06_SD.netMx <- cbind(ESS06_SD.netMx, ESS06_SD.clusterCoef, ESS06_SD.degreeCent$centralization,
                        ESS06_SD.netDensity, ESS06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
ESS06_TDg2 <- data.frame(ESS06_TD)
ESS06_TDg2 <- ESS06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_TDg2$player1
player2vector <- ESS06_TDg2$player2
ESS06_TDg3 <- ESS06_TDg2
ESS06_TDg3$p1inp2vec <- is.element(ESS06_TDg3$player1, player2vector)
ESS06_TDg3$p2inp1vec <- is.element(ESS06_TDg3$player2, player1vector)

addPlayer1 <- ESS06_TDg3[ which(ESS06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_TDg3[ which(ESS06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_TDg2 <- rbind(ESS06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
ESS06_TDft <- ftable(ESS06_TDg2$player1, ESS06_TDg2$player2)
ESS06_TDft2 <- as.matrix(ESS06_TDft)
numRows <- nrow(ESS06_TDft2)
numCols <- ncol(ESS06_TDft2)
ESS06_TDft3 <- ESS06_TDft2[c(2:numRows) , c(2:numCols)]
ESS06_TDTable <- graph.adjacency(ESS06_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(ESS06_TDTable, vertex.label = V(ESS06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
ESS06_TD.clusterCoef <- transitivity(ESS06_TDTable, type="global") #cluster coefficient
ESS06_TD.degreeCent <- centralization.degree(ESS06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_TDftn <- as.network.matrix(ESS06_TDft)
ESS06_TD.netDensity <- network.density(ESS06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_TD.entropy <- entropy(ESS06_TDft) #entropy

ESS06_TD.netMx <- cbind(ESS06_TD.netMx, ESS06_TD.clusterCoef, ESS06_TD.degreeCent$centralization,
                        ESS06_TD.netDensity, ESS06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
ESS06_QTg2 <- data.frame(ESS06_QT)
ESS06_QTg2 <- ESS06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS06_QTg2$player1
player2vector <- ESS06_QTg2$player2
ESS06_QTg3 <- ESS06_QTg2
ESS06_QTg3$p1inp2vec <- is.element(ESS06_QTg3$player1, player2vector)
ESS06_QTg3$p2inp1vec <- is.element(ESS06_QTg3$player2, player1vector)

addPlayer1 <- ESS06_QTg3[ which(ESS06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS06_QTg3[ which(ESS06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS06_QTg2 <- rbind(ESS06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
ESS06_QTft <- ftable(ESS06_QTg2$player1, ESS06_QTg2$player2)
ESS06_QTft2 <- as.matrix(ESS06_QTft)
numRows <- nrow(ESS06_QTft2)
numCols <- ncol(ESS06_QTft2)
ESS06_QTft3 <- ESS06_QTft2[c(2:numRows) , c(2:numCols)]
ESS06_QTTable <- graph.adjacency(ESS06_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(ESS06_QTTable, vertex.label = V(ESS06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
ESS06_QT.clusterCoef <- transitivity(ESS06_QTTable, type="global") #cluster coefficient
ESS06_QT.degreeCent <- centralization.degree(ESS06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS06_QTftn <- as.network.matrix(ESS06_QTft)
ESS06_QT.netDensity <- network.density(ESS06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS06_QT.entropy <- entropy(ESS06_QTft) #entropy

ESS06_QT.netMx <- cbind(ESS06_QT.netMx, ESS06_QT.clusterCoef, ESS06_QT.degreeCent$centralization,
                        ESS06_QT.netDensity, ESS06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS06_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "FRE"
KIoutcome = "Goal_F"
FRE06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
FRE06_Gg2 <- data.frame(FRE06_G)
FRE06_Gg2 <- FRE06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_Gg2$player1
player2vector <- FRE06_Gg2$player2
FRE06_Gg3 <- FRE06_Gg2
FRE06_Gg3$p1inp2vec <- is.element(FRE06_Gg3$player1, player2vector)
FRE06_Gg3$p2inp1vec <- is.element(FRE06_Gg3$player2, player1vector)

addPlayer1 <- FRE06_Gg3[ which(FRE06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_Gg3[ which(FRE06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_Gg2 <- rbind(FRE06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
FRE06_Gft <- ftable(FRE06_Gg2$player1, FRE06_Gg2$player2)
FRE06_Gft2 <- as.matrix(FRE06_Gft)
numRows <- nrow(FRE06_Gft2)
numCols <- ncol(FRE06_Gft2)
FRE06_Gft3 <- FRE06_Gft2[c(2:numRows) , c(2:numCols)]
FRE06_GTable <- graph.adjacency(FRE06_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(FRE06_GTable, vertex.label = V(FRE06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
FRE06_G.clusterCoef <- transitivity(FRE06_GTable, type="global") #cluster coefficient
FRE06_G.degreeCent <- centralization.degree(FRE06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_Gftn <- as.network.matrix(FRE06_Gft)
FRE06_G.netDensity <- network.density(FRE06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_G.entropy <- entropy(FRE06_Gft) #entropy

FRE06_G.netMx <- cbind(FRE06_G.netMx, FRE06_G.clusterCoef, FRE06_G.degreeCent$centralization,
                       FRE06_G.netDensity, FRE06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************
#NA

round = 6
teamName = "FRE"
KIoutcome = "Behind_F"
FRE06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
FRE06_Bg2 <- data.frame(FRE06_B)
FRE06_Bg2 <- FRE06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_Bg2$player1
player2vector <- FRE06_Bg2$player2
FRE06_Bg3 <- FRE06_Bg2
FRE06_Bg3$p1inp2vec <- is.element(FRE06_Bg3$player1, player2vector)
FRE06_Bg3$p2inp1vec <- is.element(FRE06_Bg3$player2, player1vector)

addPlayer1 <- FRE06_Bg3[ which(FRE06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_Bg3[ which(FRE06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_Bg2 <- rbind(FRE06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
FRE06_Bft <- ftable(FRE06_Bg2$player1, FRE06_Bg2$player2)
FRE06_Bft2 <- as.matrix(FRE06_Bft)
numRows <- nrow(FRE06_Bft2)
numCols <- ncol(FRE06_Bft2)
FRE06_Bft3 <- FRE06_Bft2[c(2:numRows) , c(2:numCols)]
FRE06_BTable <- graph.adjacency(FRE06_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(FRE06_BTable, vertex.label = V(FRE06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
FRE06_B.clusterCoef <- transitivity(FRE06_BTable, type="global") #cluster coefficient
FRE06_B.degreeCent <- centralization.degree(FRE06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_Bftn <- as.network.matrix(FRE06_Bft)
FRE06_B.netDensity <- network.density(FRE06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_B.entropy <- entropy(FRE06_Bft) #entropy

FRE06_B.netMx <- cbind(FRE06_B.netMx, FRE06_B.clusterCoef, FRE06_B.degreeCent$centralization,
                       FRE06_B.netDensity, FRE06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
FRE06_SFg2 <- data.frame(FRE06_SF)
FRE06_SFg2 <- FRE06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_SFg2$player1
player2vector <- FRE06_SFg2$player2
FRE06_SFg3 <- FRE06_SFg2
FRE06_SFg3$p1inp2vec <- is.element(FRE06_SFg3$player1, player2vector)
FRE06_SFg3$p2inp1vec <- is.element(FRE06_SFg3$player2, player1vector)

addPlayer1 <- FRE06_SFg3[ which(FRE06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_SFg3[ which(FRE06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_SFg2 <- rbind(FRE06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
FRE06_SFft <- ftable(FRE06_SFg2$player1, FRE06_SFg2$player2)
FRE06_SFft2 <- as.matrix(FRE06_SFft)
numRows <- nrow(FRE06_SFft2)
numCols <- ncol(FRE06_SFft2)
FRE06_SFft3 <- FRE06_SFft2[c(2:numRows) , c(2:numCols)]
FRE06_SFTable <- graph.adjacency(FRE06_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(FRE06_SFTable, vertex.label = V(FRE06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
FRE06_SF.clusterCoef <- transitivity(FRE06_SFTable, type="global") #cluster coefficient
FRE06_SF.degreeCent <- centralization.degree(FRE06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_SFftn <- as.network.matrix(FRE06_SFft)
FRE06_SF.netDensity <- network.density(FRE06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_SF.entropy <- entropy(FRE06_SFft) #entropy

FRE06_SF.netMx <- cbind(FRE06_SF.netMx, FRE06_SF.clusterCoef, FRE06_SF.degreeCent$centralization,
                        FRE06_SF.netDensity, FRE06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
FRE06_TFg2 <- data.frame(FRE06_TF)
FRE06_TFg2 <- FRE06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_TFg2$player1
player2vector <- FRE06_TFg2$player2
FRE06_TFg3 <- FRE06_TFg2
FRE06_TFg3$p1inp2vec <- is.element(FRE06_TFg3$player1, player2vector)
FRE06_TFg3$p2inp1vec <- is.element(FRE06_TFg3$player2, player1vector)

addPlayer1 <- FRE06_TFg3[ which(FRE06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE06_TFg3[ which(FRE06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_TFg2 <- rbind(FRE06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
FRE06_TFft <- ftable(FRE06_TFg2$player1, FRE06_TFg2$player2)
FRE06_TFft2 <- as.matrix(FRE06_TFft)
numRows <- nrow(FRE06_TFft2)
numCols <- ncol(FRE06_TFft2)
FRE06_TFft3 <- FRE06_TFft2[c(2:numRows) , c(2:numCols)]
FRE06_TFTable <- graph.adjacency(FRE06_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(FRE06_TFTable, vertex.label = V(FRE06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
FRE06_TF.clusterCoef <- transitivity(FRE06_TFTable, type="global") #cluster coefficient
FRE06_TF.degreeCent <- centralization.degree(FRE06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_TFftn <- as.network.matrix(FRE06_TFft)
FRE06_TF.netDensity <- network.density(FRE06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_TF.entropy <- entropy(FRE06_TFft) #entropy

FRE06_TF.netMx <- cbind(FRE06_TF.netMx, FRE06_TF.clusterCoef, FRE06_TF.degreeCent$centralization,
                        FRE06_TF.netDensity, FRE06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
FRE06_SAMg2 <- data.frame(FRE06_SAM)
FRE06_SAMg2 <- FRE06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_SAMg2$player1
player2vector <- FRE06_SAMg2$player2
FRE06_SAMg3 <- FRE06_SAMg2
FRE06_SAMg3$p1inp2vec <- is.element(FRE06_SAMg3$player1, player2vector)
FRE06_SAMg3$p2inp1vec <- is.element(FRE06_SAMg3$player2, player1vector)

addPlayer1 <- FRE06_SAMg3[ which(FRE06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_SAMg3[ which(FRE06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_SAMg2 <- rbind(FRE06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
FRE06_SAMft <- ftable(FRE06_SAMg2$player1, FRE06_SAMg2$player2)
FRE06_SAMft2 <- as.matrix(FRE06_SAMft)
numRows <- nrow(FRE06_SAMft2)
numCols <- ncol(FRE06_SAMft2)
FRE06_SAMft3 <- FRE06_SAMft2[c(2:numRows) , c(2:numCols)]
FRE06_SAMTable <- graph.adjacency(FRE06_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(FRE06_SAMTable, vertex.label = V(FRE06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
FRE06_SAM.clusterCoef <- transitivity(FRE06_SAMTable, type="global") #cluster coefficient
FRE06_SAM.degreeCent <- centralization.degree(FRE06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_SAMftn <- as.network.matrix(FRE06_SAMft)
FRE06_SAM.netDensity <- network.density(FRE06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_SAM.entropy <- entropy(FRE06_SAMft) #entropy

FRE06_SAM.netMx <- cbind(FRE06_SAM.netMx, FRE06_SAM.clusterCoef, FRE06_SAM.degreeCent$centralization,
                         FRE06_SAM.netDensity, FRE06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************

round = 6
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
FRE06_TAMg2 <- data.frame(FRE06_TAM)
FRE06_TAMg2 <- FRE06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_TAMg2$player1
player2vector <- FRE06_TAMg2$player2
FRE06_TAMg3 <- FRE06_TAMg2
FRE06_TAMg3$p1inp2vec <- is.element(FRE06_TAMg3$player1, player2vector)
FRE06_TAMg3$p2inp1vec <- is.element(FRE06_TAMg3$player2, player1vector)

addPlayer1 <- FRE06_TAMg3[ which(FRE06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_TAMg3[ which(FRE06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_TAMg2 <- rbind(FRE06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
FRE06_TAMft <- ftable(FRE06_TAMg2$player1, FRE06_TAMg2$player2)
FRE06_TAMft2 <- as.matrix(FRE06_TAMft)
numRows <- nrow(FRE06_TAMft2)
numCols <- ncol(FRE06_TAMft2)
FRE06_TAMft3 <- FRE06_TAMft2[c(2:numRows) , c(2:numCols)]
FRE06_TAMTable <- graph.adjacency(FRE06_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(FRE06_TAMTable, vertex.label = V(FRE06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
FRE06_TAM.clusterCoef <- transitivity(FRE06_TAMTable, type="global") #cluster coefficient
FRE06_TAM.degreeCent <- centralization.degree(FRE06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_TAMftn <- as.network.matrix(FRE06_TAMft)
FRE06_TAM.netDensity <- network.density(FRE06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_TAM.entropy <- entropy(FRE06_TAMft) #entropy

FRE06_TAM.netMx <- cbind(FRE06_TAM.netMx, FRE06_TAM.clusterCoef, FRE06_TAM.degreeCent$centralization,
                         FRE06_TAM.netDensity, FRE06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
FRE06_SDMg2 <- data.frame(FRE06_SDM)
FRE06_SDMg2 <- FRE06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_SDMg2$player1
player2vector <- FRE06_SDMg2$player2
FRE06_SDMg3 <- FRE06_SDMg2
FRE06_SDMg3$p1inp2vec <- is.element(FRE06_SDMg3$player1, player2vector)
FRE06_SDMg3$p2inp1vec <- is.element(FRE06_SDMg3$player2, player1vector)

addPlayer1 <- FRE06_SDMg3[ which(FRE06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_SDMg3[ which(FRE06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_SDMg2 <- rbind(FRE06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
FRE06_SDMft <- ftable(FRE06_SDMg2$player1, FRE06_SDMg2$player2)
FRE06_SDMft2 <- as.matrix(FRE06_SDMft)
numRows <- nrow(FRE06_SDMft2)
numCols <- ncol(FRE06_SDMft2)
FRE06_SDMft3 <- FRE06_SDMft2[c(2:numRows) , c(2:numCols)]
FRE06_SDMTable <- graph.adjacency(FRE06_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(FRE06_SDMTable, vertex.label = V(FRE06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
FRE06_SDM.clusterCoef <- transitivity(FRE06_SDMTable, type="global") #cluster coefficient
FRE06_SDM.degreeCent <- centralization.degree(FRE06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_SDMftn <- as.network.matrix(FRE06_SDMft)
FRE06_SDM.netDensity <- network.density(FRE06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_SDM.entropy <- entropy(FRE06_SDMft) #entropy

FRE06_SDM.netMx <- cbind(FRE06_SDM.netMx, FRE06_SDM.clusterCoef, FRE06_SDM.degreeCent$centralization,
                         FRE06_SDM.netDensity, FRE06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
FRE06_TDMg2 <- data.frame(FRE06_TDM)
FRE06_TDMg2 <- FRE06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_TDMg2$player1
player2vector <- FRE06_TDMg2$player2
FRE06_TDMg3 <- FRE06_TDMg2
FRE06_TDMg3$p1inp2vec <- is.element(FRE06_TDMg3$player1, player2vector)
FRE06_TDMg3$p2inp1vec <- is.element(FRE06_TDMg3$player2, player1vector)

addPlayer1 <- FRE06_TDMg3[ which(FRE06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_TDMg3[ which(FRE06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_TDMg2 <- rbind(FRE06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
FRE06_TDMft <- ftable(FRE06_TDMg2$player1, FRE06_TDMg2$player2)
FRE06_TDMft2 <- as.matrix(FRE06_TDMft)
numRows <- nrow(FRE06_TDMft2)
numCols <- ncol(FRE06_TDMft2)
FRE06_TDMft3 <- FRE06_TDMft2[c(2:numRows) , c(2:numCols)]
FRE06_TDMTable <- graph.adjacency(FRE06_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(FRE06_TDMTable, vertex.label = V(FRE06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
FRE06_TDM.clusterCoef <- transitivity(FRE06_TDMTable, type="global") #cluster coefficient
FRE06_TDM.degreeCent <- centralization.degree(FRE06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_TDMftn <- as.network.matrix(FRE06_TDMft)
FRE06_TDM.netDensity <- network.density(FRE06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_TDM.entropy <- entropy(FRE06_TDMft) #entropy

FRE06_TDM.netMx <- cbind(FRE06_TDM.netMx, FRE06_TDM.clusterCoef, FRE06_TDM.degreeCent$centralization,
                         FRE06_TDM.netDensity, FRE06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
FRE06_SDg2 <- data.frame(FRE06_SD)
FRE06_SDg2 <- FRE06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_SDg2$player1
player2vector <- FRE06_SDg2$player2
FRE06_SDg3 <- FRE06_SDg2
FRE06_SDg3$p1inp2vec <- is.element(FRE06_SDg3$player1, player2vector)
FRE06_SDg3$p2inp1vec <- is.element(FRE06_SDg3$player2, player1vector)

addPlayer1 <- FRE06_SDg3[ which(FRE06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_SDg3[ which(FRE06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_SDg2 <- rbind(FRE06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
FRE06_SDft <- ftable(FRE06_SDg2$player1, FRE06_SDg2$player2)
FRE06_SDft2 <- as.matrix(FRE06_SDft)
numRows <- nrow(FRE06_SDft2)
numCols <- ncol(FRE06_SDft2)
FRE06_SDft3 <- FRE06_SDft2[c(2:numRows) , c(2:numCols)]
FRE06_SDTable <- graph.adjacency(FRE06_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(FRE06_SDTable, vertex.label = V(FRE06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
FRE06_SD.clusterCoef <- transitivity(FRE06_SDTable, type="global") #cluster coefficient
FRE06_SD.degreeCent <- centralization.degree(FRE06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_SDftn <- as.network.matrix(FRE06_SDft)
FRE06_SD.netDensity <- network.density(FRE06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_SD.entropy <- entropy(FRE06_SDft) #entropy

FRE06_SD.netMx <- cbind(FRE06_SD.netMx, FRE06_SD.clusterCoef, FRE06_SD.degreeCent$centralization,
                        FRE06_SD.netDensity, FRE06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
FRE06_TDg2 <- data.frame(FRE06_TD)
FRE06_TDg2 <- FRE06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_TDg2$player1
player2vector <- FRE06_TDg2$player2
FRE06_TDg3 <- FRE06_TDg2
FRE06_TDg3$p1inp2vec <- is.element(FRE06_TDg3$player1, player2vector)
FRE06_TDg3$p2inp1vec <- is.element(FRE06_TDg3$player2, player1vector)

addPlayer1 <- FRE06_TDg3[ which(FRE06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_TDg3[ which(FRE06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_TDg2 <- rbind(FRE06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
FRE06_TDft <- ftable(FRE06_TDg2$player1, FRE06_TDg2$player2)
FRE06_TDft2 <- as.matrix(FRE06_TDft)
numRows <- nrow(FRE06_TDft2)
numCols <- ncol(FRE06_TDft2)
FRE06_TDft3 <- FRE06_TDft2[c(2:numRows) , c(2:numCols)]
FRE06_TDTable <- graph.adjacency(FRE06_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(FRE06_TDTable, vertex.label = V(FRE06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
FRE06_TD.clusterCoef <- transitivity(FRE06_TDTable, type="global") #cluster coefficient
FRE06_TD.degreeCent <- centralization.degree(FRE06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_TDftn <- as.network.matrix(FRE06_TDft)
FRE06_TD.netDensity <- network.density(FRE06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_TD.entropy <- entropy(FRE06_TDft) #entropy

FRE06_TD.netMx <- cbind(FRE06_TD.netMx, FRE06_TD.clusterCoef, FRE06_TD.degreeCent$centralization,
                        FRE06_TD.netDensity, FRE06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
FRE06_QTg2 <- data.frame(FRE06_QT)
FRE06_QTg2 <- FRE06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE06_QTg2$player1
player2vector <- FRE06_QTg2$player2
FRE06_QTg3 <- FRE06_QTg2
FRE06_QTg3$p1inp2vec <- is.element(FRE06_QTg3$player1, player2vector)
FRE06_QTg3$p2inp1vec <- is.element(FRE06_QTg3$player2, player1vector)

addPlayer1 <- FRE06_QTg3[ which(FRE06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE06_QTg3[ which(FRE06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE06_QTg2 <- rbind(FRE06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
FRE06_QTft <- ftable(FRE06_QTg2$player1, FRE06_QTg2$player2)
FRE06_QTft2 <- as.matrix(FRE06_QTft)
numRows <- nrow(FRE06_QTft2)
numCols <- ncol(FRE06_QTft2)
FRE06_QTft3 <- FRE06_QTft2[c(2:numRows) , c(2:numCols)]
FRE06_QTTable <- graph.adjacency(FRE06_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(FRE06_QTTable, vertex.label = V(FRE06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
FRE06_QT.clusterCoef <- transitivity(FRE06_QTTable, type="global") #cluster coefficient
FRE06_QT.degreeCent <- centralization.degree(FRE06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE06_QTftn <- as.network.matrix(FRE06_QTft)
FRE06_QT.netDensity <- network.density(FRE06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE06_QT.entropy <- entropy(FRE06_QTft) #entropy

FRE06_QT.netMx <- cbind(FRE06_QT.netMx, FRE06_QT.clusterCoef, FRE06_QT.degreeCent$centralization,
                        FRE06_QT.netDensity, FRE06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE06_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************

round = 6
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
GCFC06_Gg2 <- data.frame(GCFC06_G)
GCFC06_Gg2 <- GCFC06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_Gg2$player1
player2vector <- GCFC06_Gg2$player2
GCFC06_Gg3 <- GCFC06_Gg2
GCFC06_Gg3$p1inp2vec <- is.element(GCFC06_Gg3$player1, player2vector)
GCFC06_Gg3$p2inp1vec <- is.element(GCFC06_Gg3$player2, player1vector)

addPlayer1 <- GCFC06_Gg3[ which(GCFC06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC06_Gg3[ which(GCFC06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_Gg2 <- rbind(GCFC06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
GCFC06_Gft <- ftable(GCFC06_Gg2$player1, GCFC06_Gg2$player2)
GCFC06_Gft2 <- as.matrix(GCFC06_Gft)
numRows <- nrow(GCFC06_Gft2)
numCols <- ncol(GCFC06_Gft2)
GCFC06_Gft3 <- GCFC06_Gft2[c(2:numRows) , c(2:numCols)]
GCFC06_GTable <- graph.adjacency(GCFC06_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(GCFC06_GTable, vertex.label = V(GCFC06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
GCFC06_G.clusterCoef <- transitivity(GCFC06_GTable, type="global") #cluster coefficient
GCFC06_G.degreeCent <- centralization.degree(GCFC06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_Gftn <- as.network.matrix(GCFC06_Gft)
GCFC06_G.netDensity <- network.density(GCFC06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_G.entropy <- entropy(GCFC06_Gft) #entropy

GCFC06_G.netMx <- cbind(GCFC06_G.netMx, GCFC06_G.clusterCoef, GCFC06_G.degreeCent$centralization,
                        GCFC06_G.netDensity, GCFC06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
GCFC06_Bg2 <- data.frame(GCFC06_B)
GCFC06_Bg2 <- GCFC06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_Bg2$player1
player2vector <- GCFC06_Bg2$player2
GCFC06_Bg3 <- GCFC06_Bg2
GCFC06_Bg3$p1inp2vec <- is.element(GCFC06_Bg3$player1, player2vector)
GCFC06_Bg3$p2inp1vec <- is.element(GCFC06_Bg3$player2, player1vector)

addPlayer1 <- GCFC06_Bg3[ which(GCFC06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC06_Bg3[ which(GCFC06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_Bg2 <- rbind(GCFC06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
GCFC06_Bft <- ftable(GCFC06_Bg2$player1, GCFC06_Bg2$player2)
GCFC06_Bft2 <- as.matrix(GCFC06_Bft)
numRows <- nrow(GCFC06_Bft2)
numCols <- ncol(GCFC06_Bft2)
GCFC06_Bft3 <- GCFC06_Bft2[c(2:numRows) , c(2:numCols)]
GCFC06_BTable <- graph.adjacency(GCFC06_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(GCFC06_BTable, vertex.label = V(GCFC06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
GCFC06_B.clusterCoef <- transitivity(GCFC06_BTable, type="global") #cluster coefficient
GCFC06_B.degreeCent <- centralization.degree(GCFC06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_Bftn <- as.network.matrix(GCFC06_Bft)
GCFC06_B.netDensity <- network.density(GCFC06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_B.entropy <- entropy(GCFC06_Bft) #entropy

GCFC06_B.netMx <- cbind(GCFC06_B.netMx, GCFC06_B.clusterCoef, GCFC06_B.degreeCent$centralization,
                        GCFC06_B.netDensity, GCFC06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************

round = 6
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
GCFC06_SFg2 <- data.frame(GCFC06_SF)
GCFC06_SFg2 <- GCFC06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_SFg2$player1
player2vector <- GCFC06_SFg2$player2
GCFC06_SFg3 <- GCFC06_SFg2
GCFC06_SFg3$p1inp2vec <- is.element(GCFC06_SFg3$player1, player2vector)
GCFC06_SFg3$p2inp1vec <- is.element(GCFC06_SFg3$player2, player1vector)

addPlayer1 <- GCFC06_Bg3[ which(GCFC06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC06_SFg3[ which(GCFC06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_SFg2 <- rbind(GCFC06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
GCFC06_SFft <- ftable(GCFC06_SFg2$player1, GCFC06_SFg2$player2)
GCFC06_SFft2 <- as.matrix(GCFC06_SFft)
numRows <- nrow(GCFC06_SFft2)
numCols <- ncol(GCFC06_SFft2)
GCFC06_SFft3 <- GCFC06_SFft2[c(2:numRows) , c(2:numCols)]
GCFC06_SFTable <- graph.adjacency(GCFC06_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(GCFC06_SFTable, vertex.label = V(GCFC06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
GCFC06_SF.clusterCoef <- transitivity(GCFC06_SFTable, type="global") #cluster coefficient
GCFC06_SF.degreeCent <- centralization.degree(GCFC06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_SFftn <- as.network.matrix(GCFC06_SFft)
GCFC06_SF.netDensity <- network.density(GCFC06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_SF.entropy <- entropy(GCFC06_SFft) #entropy

GCFC06_SF.netMx <- cbind(GCFC06_SF.netMx, GCFC06_SF.clusterCoef, GCFC06_SF.degreeCent$centralization,
                         GCFC06_SF.netDensity, GCFC06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
GCFC06_TFg2 <- data.frame(GCFC06_TF)
GCFC06_TFg2 <- GCFC06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_TFg2$player1
player2vector <- GCFC06_TFg2$player2
GCFC06_TFg3 <- GCFC06_TFg2
GCFC06_TFg3$p1inp2vec <- is.element(GCFC06_TFg3$player1, player2vector)
GCFC06_TFg3$p2inp1vec <- is.element(GCFC06_TFg3$player2, player1vector)

addPlayer1 <- GCFC06_TFg3[ which(GCFC06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC06_TFg3[ which(GCFC06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_TFg2 <- rbind(GCFC06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
GCFC06_TFft <- ftable(GCFC06_TFg2$player1, GCFC06_TFg2$player2)
GCFC06_TFft2 <- as.matrix(GCFC06_TFft)
numRows <- nrow(GCFC06_TFft2)
numCols <- ncol(GCFC06_TFft2)
GCFC06_TFft3 <- GCFC06_TFft2[c(2:numRows) , c(2:numCols)]
GCFC06_TFTable <- graph.adjacency(GCFC06_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(GCFC06_TFTable, vertex.label = V(GCFC06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
GCFC06_TF.clusterCoef <- transitivity(GCFC06_TFTable, type="global") #cluster coefficient
GCFC06_TF.degreeCent <- centralization.degree(GCFC06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_TFftn <- as.network.matrix(GCFC06_TFft)
GCFC06_TF.netDensity <- network.density(GCFC06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_TF.entropy <- entropy(GCFC06_TFft) #entropy

GCFC06_TF.netMx <- cbind(GCFC06_TF.netMx, GCFC06_TF.clusterCoef, GCFC06_TF.degreeCent$centralization,
                         GCFC06_TF.netDensity, GCFC06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
GCFC06_SAMg2 <- data.frame(GCFC06_SAM)
GCFC06_SAMg2 <- GCFC06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_SAMg2$player1
player2vector <- GCFC06_SAMg2$player2
GCFC06_SAMg3 <- GCFC06_SAMg2
GCFC06_SAMg3$p1inp2vec <- is.element(GCFC06_SAMg3$player1, player2vector)
GCFC06_SAMg3$p2inp1vec <- is.element(GCFC06_SAMg3$player2, player1vector)

addPlayer1 <- GCFC06_SAMg3[ which(GCFC06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC06_SAMg3[ which(GCFC06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_SAMg2 <- rbind(GCFC06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
GCFC06_SAMft <- ftable(GCFC06_SAMg2$player1, GCFC06_SAMg2$player2)
GCFC06_SAMft2 <- as.matrix(GCFC06_SAMft)
numRows <- nrow(GCFC06_SAMft2)
numCols <- ncol(GCFC06_SAMft2)
GCFC06_SAMft3 <- GCFC06_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC06_SAMTable <- graph.adjacency(GCFC06_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(GCFC06_SAMTable, vertex.label = V(GCFC06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
GCFC06_SAM.clusterCoef <- transitivity(GCFC06_SAMTable, type="global") #cluster coefficient
GCFC06_SAM.degreeCent <- centralization.degree(GCFC06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_SAMftn <- as.network.matrix(GCFC06_SAMft)
GCFC06_SAM.netDensity <- network.density(GCFC06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_SAM.entropy <- entropy(GCFC06_SAMft) #entropy

GCFC06_SAM.netMx <- cbind(GCFC06_SAM.netMx, GCFC06_SAM.clusterCoef, GCFC06_SAM.degreeCent$centralization,
                          GCFC06_SAM.netDensity, GCFC06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************
#NA

round = 6
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
GCFC06_TAMg2 <- data.frame(GCFC06_TAM)
GCFC06_TAMg2 <- GCFC06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_TAMg2$player1
player2vector <- GCFC06_TAMg2$player2
GCFC06_TAMg3 <- GCFC06_TAMg2
GCFC06_TAMg3$p1inp2vec <- is.element(GCFC06_TAMg3$player1, player2vector)
GCFC06_TAMg3$p2inp1vec <- is.element(GCFC06_TAMg3$player2, player1vector)

addPlayer1 <- GCFC06_TAMg3[ which(GCFC06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC06_TAMg3[ which(GCFC06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_TAMg2 <- rbind(GCFC06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
GCFC06_TAMft <- ftable(GCFC06_TAMg2$player1, GCFC06_TAMg2$player2)
GCFC06_TAMft2 <- as.matrix(GCFC06_TAMft)
numRows <- nrow(GCFC06_TAMft2)
numCols <- ncol(GCFC06_TAMft2)
GCFC06_TAMft3 <- GCFC06_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC06_TAMTable <- graph.adjacency(GCFC06_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(GCFC06_TAMTable, vertex.label = V(GCFC06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
GCFC06_TAM.clusterCoef <- transitivity(GCFC06_TAMTable, type="global") #cluster coefficient
GCFC06_TAM.degreeCent <- centralization.degree(GCFC06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_TAMftn <- as.network.matrix(GCFC06_TAMft)
GCFC06_TAM.netDensity <- network.density(GCFC06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_TAM.entropy <- entropy(GCFC06_TAMft) #entropy

GCFC06_TAM.netMx <- cbind(GCFC06_TAM.netMx, GCFC06_TAM.clusterCoef, GCFC06_TAM.degreeCent$centralization,
                          GCFC06_TAM.netDensity, GCFC06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
GCFC06_SDMg2 <- data.frame(GCFC06_SDM)
GCFC06_SDMg2 <- GCFC06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_SDMg2$player1
player2vector <- GCFC06_SDMg2$player2
GCFC06_SDMg3 <- GCFC06_SDMg2
GCFC06_SDMg3$p1inp2vec <- is.element(GCFC06_SDMg3$player1, player2vector)
GCFC06_SDMg3$p2inp1vec <- is.element(GCFC06_SDMg3$player2, player1vector)

addPlayer1 <- GCFC06_SDMg3[ which(GCFC06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC06_SDMg3[ which(GCFC06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_SDMg2 <- rbind(GCFC06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
GCFC06_SDMft <- ftable(GCFC06_SDMg2$player1, GCFC06_SDMg2$player2)
GCFC06_SDMft2 <- as.matrix(GCFC06_SDMft)
numRows <- nrow(GCFC06_SDMft2)
numCols <- ncol(GCFC06_SDMft2)
GCFC06_SDMft3 <- GCFC06_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC06_SDMTable <- graph.adjacency(GCFC06_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(GCFC06_SDMTable, vertex.label = V(GCFC06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
GCFC06_SDM.clusterCoef <- transitivity(GCFC06_SDMTable, type="global") #cluster coefficient
GCFC06_SDM.degreeCent <- centralization.degree(GCFC06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_SDMftn <- as.network.matrix(GCFC06_SDMft)
GCFC06_SDM.netDensity <- network.density(GCFC06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_SDM.entropy <- entropy(GCFC06_SDMft) #entropy

GCFC06_SDM.netMx <- cbind(GCFC06_SDM.netMx, GCFC06_SDM.clusterCoef, GCFC06_SDM.degreeCent$centralization,
                          GCFC06_SDM.netDensity, GCFC06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
GCFC06_TDMg2 <- data.frame(GCFC06_TDM)
GCFC06_TDMg2 <- GCFC06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_TDMg2$player1
player2vector <- GCFC06_TDMg2$player2
GCFC06_TDMg3 <- GCFC06_TDMg2
GCFC06_TDMg3$p1inp2vec <- is.element(GCFC06_TDMg3$player1, player2vector)
GCFC06_TDMg3$p2inp1vec <- is.element(GCFC06_TDMg3$player2, player1vector)

addPlayer1 <- GCFC06_TDMg3[ which(GCFC06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC06_TDMg3[ which(GCFC06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_TDMg2 <- rbind(GCFC06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
GCFC06_TDMft <- ftable(GCFC06_TDMg2$player1, GCFC06_TDMg2$player2)
GCFC06_TDMft2 <- as.matrix(GCFC06_TDMft)
numRows <- nrow(GCFC06_TDMft2)
numCols <- ncol(GCFC06_TDMft2)
GCFC06_TDMft3 <- GCFC06_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC06_TDMTable <- graph.adjacency(GCFC06_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(GCFC06_TDMTable, vertex.label = V(GCFC06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
GCFC06_TDM.clusterCoef <- transitivity(GCFC06_TDMTable, type="global") #cluster coefficient
GCFC06_TDM.degreeCent <- centralization.degree(GCFC06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_TDMftn <- as.network.matrix(GCFC06_TDMft)
GCFC06_TDM.netDensity <- network.density(GCFC06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_TDM.entropy <- entropy(GCFC06_TDMft) #entropy

GCFC06_TDM.netMx <- cbind(GCFC06_TDM.netMx, GCFC06_TDM.clusterCoef, GCFC06_TDM.degreeCent$centralization,
                          GCFC06_TDM.netDensity, GCFC06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************

round = 6
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
GCFC06_SDg2 <- data.frame(GCFC06_SD)
GCFC06_SDg2 <- GCFC06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_SDg2$player1
player2vector <- GCFC06_SDg2$player2
GCFC06_SDg3 <- GCFC06_SDg2
GCFC06_SDg3$p1inp2vec <- is.element(GCFC06_SDg3$player1, player2vector)
GCFC06_SDg3$p2inp1vec <- is.element(GCFC06_SDg3$player2, player1vector)

addPlayer1 <- GCFC06_SDg3[ which(GCFC06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GCFC06_SDg3[ which(GCFC06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_SDg2 <- rbind(GCFC06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
GCFC06_SDft <- ftable(GCFC06_SDg2$player1, GCFC06_SDg2$player2)
GCFC06_SDft2 <- as.matrix(GCFC06_SDft)
numRows <- nrow(GCFC06_SDft2)
numCols <- ncol(GCFC06_SDft2)
GCFC06_SDft3 <- GCFC06_SDft2[c(2:numRows) , c(2:numCols)]
GCFC06_SDTable <- graph.adjacency(GCFC06_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(GCFC06_SDTable, vertex.label = V(GCFC06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
GCFC06_SD.clusterCoef <- transitivity(GCFC06_SDTable, type="global") #cluster coefficient
GCFC06_SD.degreeCent <- centralization.degree(GCFC06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_SDftn <- as.network.matrix(GCFC06_SDft)
GCFC06_SD.netDensity <- network.density(GCFC06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_SD.entropy <- entropy(GCFC06_SDft) #entropy

GCFC06_SD.netMx <- cbind(GCFC06_SD.netMx, GCFC06_SD.clusterCoef, GCFC06_SD.degreeCent$centralization,
                         GCFC06_SD.netDensity, GCFC06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
GCFC06_TDg2 <- data.frame(GCFC06_TD)
GCFC06_TDg2 <- GCFC06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_TDg2$player1
player2vector <- GCFC06_TDg2$player2
GCFC06_TDg3 <- GCFC06_TDg2
GCFC06_TDg3$p1inp2vec <- is.element(GCFC06_TDg3$player1, player2vector)
GCFC06_TDg3$p2inp1vec <- is.element(GCFC06_TDg3$player2, player1vector)

addPlayer1 <- GCFC06_TDg3[ which(GCFC06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC06_TDg3[ which(GCFC06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_TDg2 <- rbind(GCFC06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
GCFC06_TDft <- ftable(GCFC06_TDg2$player1, GCFC06_TDg2$player2)
GCFC06_TDft2 <- as.matrix(GCFC06_TDft)
numRows <- nrow(GCFC06_TDft2)
numCols <- ncol(GCFC06_TDft2)
GCFC06_TDft3 <- GCFC06_TDft2[c(2:numRows) , c(2:numCols)]
GCFC06_TDTable <- graph.adjacency(GCFC06_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(GCFC06_TDTable, vertex.label = V(GCFC06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
GCFC06_TD.clusterCoef <- transitivity(GCFC06_TDTable, type="global") #cluster coefficient
GCFC06_TD.degreeCent <- centralization.degree(GCFC06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_TDftn <- as.network.matrix(GCFC06_TDft)
GCFC06_TD.netDensity <- network.density(GCFC06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_TD.entropy <- entropy(GCFC06_TDft) #entropy

GCFC06_TD.netMx <- cbind(GCFC06_TD.netMx, GCFC06_TD.clusterCoef, GCFC06_TD.degreeCent$centralization,
                         GCFC06_TD.netDensity, GCFC06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
GCFC06_QTg2 <- data.frame(GCFC06_QT)
GCFC06_QTg2 <- GCFC06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC06_QTg2$player1
player2vector <- GCFC06_QTg2$player2
GCFC06_QTg3 <- GCFC06_QTg2
GCFC06_QTg3$p1inp2vec <- is.element(GCFC06_QTg3$player1, player2vector)
GCFC06_QTg3$p2inp1vec <- is.element(GCFC06_QTg3$player2, player1vector)

addPlayer1 <- GCFC06_QTg3[ which(GCFC06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC06_QTg3[ which(GCFC06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC06_QTg2 <- rbind(GCFC06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
GCFC06_QTft <- ftable(GCFC06_QTg2$player1, GCFC06_QTg2$player2)
GCFC06_QTft2 <- as.matrix(GCFC06_QTft)
numRows <- nrow(GCFC06_QTft2)
numCols <- ncol(GCFC06_QTft2)
GCFC06_QTft3 <- GCFC06_QTft2[c(2:numRows) , c(2:numCols)]
GCFC06_QTTable <- graph.adjacency(GCFC06_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(GCFC06_QTTable, vertex.label = V(GCFC06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
GCFC06_QT.clusterCoef <- transitivity(GCFC06_QTTable, type="global") #cluster coefficient
GCFC06_QT.degreeCent <- centralization.degree(GCFC06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC06_QTftn <- as.network.matrix(GCFC06_QTft)
GCFC06_QT.netDensity <- network.density(GCFC06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC06_QT.entropy <- entropy(GCFC06_QTft) #entropy

GCFC06_QT.netMx <- cbind(GCFC06_QT.netMx, GCFC06_QT.clusterCoef, GCFC06_QT.degreeCent$centralization,
                         GCFC06_QT.netDensity, GCFC06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC06_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************

round = 6
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
GEEL06_Gg2 <- data.frame(GEEL06_G)
GEEL06_Gg2 <- GEEL06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_Gg2$player1
player2vector <- GEEL06_Gg2$player2
GEEL06_Gg3 <- GEEL06_Gg2
GEEL06_Gg3$p1inp2vec <- is.element(GEEL06_Gg3$player1, player2vector)
GEEL06_Gg3$p2inp1vec <- is.element(GEEL06_Gg3$player2, player1vector)

addPlayer1 <- GEEL06_Gg3[ which(GEEL06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_Gg3[ which(GEEL06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_Gg2 <- rbind(GEEL06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
GEEL06_Gft <- ftable(GEEL06_Gg2$player1, GEEL06_Gg2$player2)
GEEL06_Gft2 <- as.matrix(GEEL06_Gft)
numRows <- nrow(GEEL06_Gft2)
numCols <- ncol(GEEL06_Gft2)
GEEL06_Gft3 <- GEEL06_Gft2[c(2:numRows) , c(2:numCols)]
GEEL06_GTable <- graph.adjacency(GEEL06_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(GEEL06_GTable, vertex.label = V(GEEL06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
GEEL06_G.clusterCoef <- transitivity(GEEL06_GTable, type="global") #cluster coefficient
GEEL06_G.degreeCent <- centralization.degree(GEEL06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_Gftn <- as.network.matrix(GEEL06_Gft)
GEEL06_G.netDensity <- network.density(GEEL06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_G.entropy <- entropy(GEEL06_Gft) #entropy

GEEL06_G.netMx <- cbind(GEEL06_G.netMx, GEEL06_G.clusterCoef, GEEL06_G.degreeCent$centralization,
                        GEEL06_G.netDensity, GEEL06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
GEEL06_Bg2 <- data.frame(GEEL06_B)
GEEL06_Bg2 <- GEEL06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_Bg2$player1
player2vector <- GEEL06_Bg2$player2
GEEL06_Bg3 <- GEEL06_Bg2
GEEL06_Bg3$p1inp2vec <- is.element(GEEL06_Bg3$player1, player2vector)
GEEL06_Bg3$p2inp1vec <- is.element(GEEL06_Bg3$player2, player1vector)

addPlayer1 <- GEEL06_Bg3[ which(GEEL06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL06_Bg3[ which(GEEL06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_Bg2 <- rbind(GEEL06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
GEEL06_Bft <- ftable(GEEL06_Bg2$player1, GEEL06_Bg2$player2)
GEEL06_Bft2 <- as.matrix(GEEL06_Bft)
numRows <- nrow(GEEL06_Bft2)
numCols <- ncol(GEEL06_Bft2)
GEEL06_Bft3 <- GEEL06_Bft2[c(2:numRows) , c(2:numCols)]
GEEL06_BTable <- graph.adjacency(GEEL06_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(GEEL06_BTable, vertex.label = V(GEEL06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
GEEL06_B.clusterCoef <- transitivity(GEEL06_BTable, type="global") #cluster coefficient
GEEL06_B.degreeCent <- centralization.degree(GEEL06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_Bftn <- as.network.matrix(GEEL06_Bft)
GEEL06_B.netDensity <- network.density(GEEL06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_B.entropy <- entropy(GEEL06_Bft) #entropy

GEEL06_B.netMx <- cbind(GEEL06_B.netMx, GEEL06_B.clusterCoef, GEEL06_B.degreeCent$centralization,
                        GEEL06_B.netDensity, GEEL06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
GEEL06_SFg2 <- data.frame(GEEL06_SF)
GEEL06_SFg2 <- GEEL06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_SFg2$player1
player2vector <- GEEL06_SFg2$player2
GEEL06_SFg3 <- GEEL06_SFg2
GEEL06_SFg3$p1inp2vec <- is.element(GEEL06_SFg3$player1, player2vector)
GEEL06_SFg3$p2inp1vec <- is.element(GEEL06_SFg3$player2, player1vector)

addPlayer1 <- GEEL06_SFg3[ which(GEEL06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_SFg3[ which(GEEL06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_SFg2 <- rbind(GEEL06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
GEEL06_SFft <- ftable(GEEL06_SFg2$player1, GEEL06_SFg2$player2)
GEEL06_SFft2 <- as.matrix(GEEL06_SFft)
numRows <- nrow(GEEL06_SFft2)
numCols <- ncol(GEEL06_SFft2)
GEEL06_SFft3 <- GEEL06_SFft2[c(2:numRows) , c(2:numCols)]
GEEL06_SFTable <- graph.adjacency(GEEL06_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(GEEL06_SFTable, vertex.label = V(GEEL06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
GEEL06_SF.clusterCoef <- transitivity(GEEL06_SFTable, type="global") #cluster coefficient
GEEL06_SF.degreeCent <- centralization.degree(GEEL06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_SFftn <- as.network.matrix(GEEL06_SFft)
GEEL06_SF.netDensity <- network.density(GEEL06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_SF.entropy <- entropy(GEEL06_SFft) #entropy

GEEL06_SF.netMx <- cbind(GEEL06_SF.netMx, GEEL06_SF.clusterCoef, GEEL06_SF.degreeCent$centralization,
                         GEEL06_SF.netDensity, GEEL06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
GEEL06_TFg2 <- data.frame(GEEL06_TF)
GEEL06_TFg2 <- GEEL06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_TFg2$player1
player2vector <- GEEL06_TFg2$player2
GEEL06_TFg3 <- GEEL06_TFg2
GEEL06_TFg3$p1inp2vec <- is.element(GEEL06_TFg3$player1, player2vector)
GEEL06_TFg3$p2inp1vec <- is.element(GEEL06_TFg3$player2, player1vector)

addPlayer1 <- GEEL06_TFg3[ which(GEEL06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_TFg3[ which(GEEL06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_TFg2 <- rbind(GEEL06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
GEEL06_TFft <- ftable(GEEL06_TFg2$player1, GEEL06_TFg2$player2)
GEEL06_TFft2 <- as.matrix(GEEL06_TFft)
numRows <- nrow(GEEL06_TFft2)
numCols <- ncol(GEEL06_TFft2)
GEEL06_TFft3 <- GEEL06_TFft2[c(2:numRows) , c(2:numCols)]
GEEL06_TFTable <- graph.adjacency(GEEL06_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(GEEL06_TFTable, vertex.label = V(GEEL06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
GEEL06_TF.clusterCoef <- transitivity(GEEL06_TFTable, type="global") #cluster coefficient
GEEL06_TF.degreeCent <- centralization.degree(GEEL06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_TFftn <- as.network.matrix(GEEL06_TFft)
GEEL06_TF.netDensity <- network.density(GEEL06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_TF.entropy <- entropy(GEEL06_TFft) #entropy

GEEL06_TF.netMx <- cbind(GEEL06_TF.netMx, GEEL06_TF.clusterCoef, GEEL06_TF.degreeCent$centralization,
                         GEEL06_TF.netDensity, GEEL06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
GEEL06_SAMg2 <- data.frame(GEEL06_SAM)
GEEL06_SAMg2 <- GEEL06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_SAMg2$player1
player2vector <- GEEL06_SAMg2$player2
GEEL06_SAMg3 <- GEEL06_SAMg2
GEEL06_SAMg3$p1inp2vec <- is.element(GEEL06_SAMg3$player1, player2vector)
GEEL06_SAMg3$p2inp1vec <- is.element(GEEL06_SAMg3$player2, player1vector)

addPlayer1 <- GEEL06_SAMg3[ which(GEEL06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_SAMg3[ which(GEEL06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_SAMg2 <- rbind(GEEL06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
GEEL06_SAMft <- ftable(GEEL06_SAMg2$player1, GEEL06_SAMg2$player2)
GEEL06_SAMft2 <- as.matrix(GEEL06_SAMft)
numRows <- nrow(GEEL06_SAMft2)
numCols <- ncol(GEEL06_SAMft2)
GEEL06_SAMft3 <- GEEL06_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL06_SAMTable <- graph.adjacency(GEEL06_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(GEEL06_SAMTable, vertex.label = V(GEEL06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
GEEL06_SAM.clusterCoef <- transitivity(GEEL06_SAMTable, type="global") #cluster coefficient
GEEL06_SAM.degreeCent <- centralization.degree(GEEL06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_SAMftn <- as.network.matrix(GEEL06_SAMft)
GEEL06_SAM.netDensity <- network.density(GEEL06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_SAM.entropy <- entropy(GEEL06_SAMft) #entropy

GEEL06_SAM.netMx <- cbind(GEEL06_SAM.netMx, GEEL06_SAM.clusterCoef, GEEL06_SAM.degreeCent$centralization,
                          GEEL06_SAM.netDensity, GEEL06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************

round = 6
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
GEEL06_TAMg2 <- data.frame(GEEL06_TAM)
GEEL06_TAMg2 <- GEEL06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_TAMg2$player1
player2vector <- GEEL06_TAMg2$player2
GEEL06_TAMg3 <- GEEL06_TAMg2
GEEL06_TAMg3$p1inp2vec <- is.element(GEEL06_TAMg3$player1, player2vector)
GEEL06_TAMg3$p2inp1vec <- is.element(GEEL06_TAMg3$player2, player1vector)

addPlayer1 <- GEEL06_TAMg3[ which(GEEL06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_TAMg3[ which(GEEL06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_TAMg2 <- rbind(GEEL06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
GEEL06_TAMft <- ftable(GEEL06_TAMg2$player1, GEEL06_TAMg2$player2)
GEEL06_TAMft2 <- as.matrix(GEEL06_TAMft)
numRows <- nrow(GEEL06_TAMft2)
numCols <- ncol(GEEL06_TAMft2)
GEEL06_TAMft3 <- GEEL06_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL06_TAMTable <- graph.adjacency(GEEL06_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(GEEL06_TAMTable, vertex.label = V(GEEL06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
GEEL06_TAM.clusterCoef <- transitivity(GEEL06_TAMTable, type="global") #cluster coefficient
GEEL06_TAM.degreeCent <- centralization.degree(GEEL06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_TAMftn <- as.network.matrix(GEEL06_TAMft)
GEEL06_TAM.netDensity <- network.density(GEEL06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_TAM.entropy <- entropy(GEEL06_TAMft) #entropy

GEEL06_TAM.netMx <- cbind(GEEL06_TAM.netMx, GEEL06_TAM.clusterCoef, GEEL06_TAM.degreeCent$centralization,
                          GEEL06_TAM.netDensity, GEEL06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************
#NA

round = 6
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
GEEL06_SDMg2 <- data.frame(GEEL06_SDM)
GEEL06_SDMg2 <- GEEL06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_SDMg2$player1
player2vector <- GEEL06_SDMg2$player2
GEEL06_SDMg3 <- GEEL06_SDMg2
GEEL06_SDMg3$p1inp2vec <- is.element(GEEL06_SDMg3$player1, player2vector)
GEEL06_SDMg3$p2inp1vec <- is.element(GEEL06_SDMg3$player2, player1vector)

addPlayer1 <- GEEL06_SDMg3[ which(GEEL06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_SDMg3[ which(GEEL06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_SDMg2 <- rbind(GEEL06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
GEEL06_SDMft <- ftable(GEEL06_SDMg2$player1, GEEL06_SDMg2$player2)
GEEL06_SDMft2 <- as.matrix(GEEL06_SDMft)
numRows <- nrow(GEEL06_SDMft2)
numCols <- ncol(GEEL06_SDMft2)
GEEL06_SDMft3 <- GEEL06_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL06_SDMTable <- graph.adjacency(GEEL06_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(GEEL06_SDMTable, vertex.label = V(GEEL06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
GEEL06_SDM.clusterCoef <- transitivity(GEEL06_SDMTable, type="global") #cluster coefficient
GEEL06_SDM.degreeCent <- centralization.degree(GEEL06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_SDMftn <- as.network.matrix(GEEL06_SDMft)
GEEL06_SDM.netDensity <- network.density(GEEL06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_SDM.entropy <- entropy(GEEL06_SDMft) #entropy

GEEL06_SDM.netMx <- cbind(GEEL06_SDM.netMx, GEEL06_SDM.clusterCoef, GEEL06_SDM.degreeCent$centralization,
                          GEEL06_SDM.netDensity, GEEL06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
GEEL06_TDMg2 <- data.frame(GEEL06_TDM)
GEEL06_TDMg2 <- GEEL06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_TDMg2$player1
player2vector <- GEEL06_TDMg2$player2
GEEL06_TDMg3 <- GEEL06_TDMg2
GEEL06_TDMg3$p1inp2vec <- is.element(GEEL06_TDMg3$player1, player2vector)
GEEL06_TDMg3$p2inp1vec <- is.element(GEEL06_TDMg3$player2, player1vector)

addPlayer1 <- GEEL06_TDMg3[ which(GEEL06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_TDMg3[ which(GEEL06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_TDMg2 <- rbind(GEEL06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
GEEL06_TDMft <- ftable(GEEL06_TDMg2$player1, GEEL06_TDMg2$player2)
GEEL06_TDMft2 <- as.matrix(GEEL06_TDMft)
numRows <- nrow(GEEL06_TDMft2)
numCols <- ncol(GEEL06_TDMft2)
GEEL06_TDMft3 <- GEEL06_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL06_TDMTable <- graph.adjacency(GEEL06_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(GEEL06_TDMTable, vertex.label = V(GEEL06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
GEEL06_TDM.clusterCoef <- transitivity(GEEL06_TDMTable, type="global") #cluster coefficient
GEEL06_TDM.degreeCent <- centralization.degree(GEEL06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_TDMftn <- as.network.matrix(GEEL06_TDMft)
GEEL06_TDM.netDensity <- network.density(GEEL06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_TDM.entropy <- entropy(GEEL06_TDMft) #entropy

GEEL06_TDM.netMx <- cbind(GEEL06_TDM.netMx, GEEL06_TDM.clusterCoef, GEEL06_TDM.degreeCent$centralization,
                          GEEL06_TDM.netDensity, GEEL06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
GEEL06_SDg2 <- data.frame(GEEL06_SD)
GEEL06_SDg2 <- GEEL06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_SDg2$player1
player2vector <- GEEL06_SDg2$player2
GEEL06_SDg3 <- GEEL06_SDg2
GEEL06_SDg3$p1inp2vec <- is.element(GEEL06_SDg3$player1, player2vector)
GEEL06_SDg3$p2inp1vec <- is.element(GEEL06_SDg3$player2, player1vector)

addPlayer1 <- GEEL06_SDg3[ which(GEEL06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_SDg3[ which(GEEL06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_SDg2 <- rbind(GEEL06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
GEEL06_SDft <- ftable(GEEL06_SDg2$player1, GEEL06_SDg2$player2)
GEEL06_SDft2 <- as.matrix(GEEL06_SDft)
numRows <- nrow(GEEL06_SDft2)
numCols <- ncol(GEEL06_SDft2)
GEEL06_SDft3 <- GEEL06_SDft2[c(2:numRows) , c(2:numCols)]
GEEL06_SDTable <- graph.adjacency(GEEL06_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(GEEL06_SDTable, vertex.label = V(GEEL06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
GEEL06_SD.clusterCoef <- transitivity(GEEL06_SDTable, type="global") #cluster coefficient
GEEL06_SD.degreeCent <- centralization.degree(GEEL06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_SDftn <- as.network.matrix(GEEL06_SDft)
GEEL06_SD.netDensity <- network.density(GEEL06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_SD.entropy <- entropy(GEEL06_SDft) #entropy

GEEL06_SD.netMx <- cbind(GEEL06_SD.netMx, GEEL06_SD.clusterCoef, GEEL06_SD.degreeCent$centralization,
                         GEEL06_SD.netDensity, GEEL06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
GEEL06_TDg2 <- data.frame(GEEL06_TD)
GEEL06_TDg2 <- GEEL06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_TDg2$player1
player2vector <- GEEL06_TDg2$player2
GEEL06_TDg3 <- GEEL06_TDg2
GEEL06_TDg3$p1inp2vec <- is.element(GEEL06_TDg3$player1, player2vector)
GEEL06_TDg3$p2inp1vec <- is.element(GEEL06_TDg3$player2, player1vector)

addPlayer1 <- GEEL06_TDg3[ which(GEEL06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_TDg3[ which(GEEL06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_TDg2 <- rbind(GEEL06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
GEEL06_TDft <- ftable(GEEL06_TDg2$player1, GEEL06_TDg2$player2)
GEEL06_TDft2 <- as.matrix(GEEL06_TDft)
numRows <- nrow(GEEL06_TDft2)
numCols <- ncol(GEEL06_TDft2)
GEEL06_TDft3 <- GEEL06_TDft2[c(2:numRows) , c(2:numCols)]
GEEL06_TDTable <- graph.adjacency(GEEL06_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(GEEL06_TDTable, vertex.label = V(GEEL06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
GEEL06_TD.clusterCoef <- transitivity(GEEL06_TDTable, type="global") #cluster coefficient
GEEL06_TD.degreeCent <- centralization.degree(GEEL06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_TDftn <- as.network.matrix(GEEL06_TDft)
GEEL06_TD.netDensity <- network.density(GEEL06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_TD.entropy <- entropy(GEEL06_TDft) #entropy

GEEL06_TD.netMx <- cbind(GEEL06_TD.netMx, GEEL06_TD.clusterCoef, GEEL06_TD.degreeCent$centralization,
                         GEEL06_TD.netDensity, GEEL06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************

round = 6
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
GEEL06_QTg2 <- data.frame(GEEL06_QT)
GEEL06_QTg2 <- GEEL06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL06_QTg2$player1
player2vector <- GEEL06_QTg2$player2
GEEL06_QTg3 <- GEEL06_QTg2
GEEL06_QTg3$p1inp2vec <- is.element(GEEL06_QTg3$player1, player2vector)
GEEL06_QTg3$p2inp1vec <- is.element(GEEL06_QTg3$player2, player1vector)

addPlayer1 <- GEEL06_QTg3[ which(GEEL06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL06_QTg3[ which(GEEL06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL06_QTg2 <- rbind(GEEL06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
GEEL06_QTft <- ftable(GEEL06_QTg2$player1, GEEL06_QTg2$player2)
GEEL06_QTft2 <- as.matrix(GEEL06_QTft)
numRows <- nrow(GEEL06_QTft2)
numCols <- ncol(GEEL06_QTft2)
GEEL06_QTft3 <- GEEL06_QTft2[c(2:numRows) , c(2:numCols)]
GEEL06_QTTable <- graph.adjacency(GEEL06_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(GEEL06_QTTable, vertex.label = V(GEEL06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
GEEL06_QT.clusterCoef <- transitivity(GEEL06_QTTable, type="global") #cluster coefficient
GEEL06_QT.degreeCent <- centralization.degree(GEEL06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL06_QTftn <- as.network.matrix(GEEL06_QTft)
GEEL06_QT.netDensity <- network.density(GEEL06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL06_QT.entropy <- entropy(GEEL06_QTft) #entropy

GEEL06_QT.netMx <- cbind(GEEL06_QT.netMx, GEEL06_QT.clusterCoef, GEEL06_QT.degreeCent$centralization,
                         GEEL06_QT.netDensity, GEEL06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL06_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "GWS"
KIoutcome = "Goal_F"
GWS06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
GWS06_Gg2 <- data.frame(GWS06_G)
GWS06_Gg2 <- GWS06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_Gg2$player1
player2vector <- GWS06_Gg2$player2
GWS06_Gg3 <- GWS06_Gg2
GWS06_Gg3$p1inp2vec <- is.element(GWS06_Gg3$player1, player2vector)
GWS06_Gg3$p2inp1vec <- is.element(GWS06_Gg3$player2, player1vector)

addPlayer1 <- GWS06_Gg3[ which(GWS06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_Gg3[ which(GWS06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_Gg2 <- rbind(GWS06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
GWS06_Gft <- ftable(GWS06_Gg2$player1, GWS06_Gg2$player2)
GWS06_Gft2 <- as.matrix(GWS06_Gft)
numRows <- nrow(GWS06_Gft2)
numCols <- ncol(GWS06_Gft2)
GWS06_Gft3 <- GWS06_Gft2[c(1:numRows) , c(1:numCols)]
GWS06_GTable <- graph.adjacency(GWS06_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(GWS06_GTable, vertex.label = V(GWS06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
GWS06_G.clusterCoef <- transitivity(GWS06_GTable, type="global") #cluster coefficient
GWS06_G.degreeCent <- centralization.degree(GWS06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_Gftn <- as.network.matrix(GWS06_Gft)
GWS06_G.netDensity <- network.density(GWS06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_G.entropy <- entropy(GWS06_Gft) #entropy

GWS06_G.netMx <- cbind(GWS06_G.netMx, GWS06_G.clusterCoef, GWS06_G.degreeCent$centralization,
                       GWS06_G.netDensity, GWS06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "GWS"
KIoutcome = "Behind_F"
GWS06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
GWS06_Bg2 <- data.frame(GWS06_B)
GWS06_Bg2 <- GWS06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_Bg2$player1
player2vector <- GWS06_Bg2$player2
GWS06_Bg3 <- GWS06_Bg2
GWS06_Bg3$p1inp2vec <- is.element(GWS06_Bg3$player1, player2vector)
GWS06_Bg3$p2inp1vec <- is.element(GWS06_Bg3$player2, player1vector)

addPlayer1 <- GWS06_Bg3[ which(GWS06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_Bg3[ which(GWS06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_Bg2 <- rbind(GWS06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
GWS06_Bft <- ftable(GWS06_Bg2$player1, GWS06_Bg2$player2)
GWS06_Bft2 <- as.matrix(GWS06_Bft)
numRows <- nrow(GWS06_Bft2)
numCols <- ncol(GWS06_Bft2)
GWS06_Bft3 <- GWS06_Bft2[c(2:numRows) , c(2:numCols)]
GWS06_BTable <- graph.adjacency(GWS06_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(GWS06_BTable, vertex.label = V(GWS06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
GWS06_B.clusterCoef <- transitivity(GWS06_BTable, type="global") #cluster coefficient
GWS06_B.degreeCent <- centralization.degree(GWS06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_Bftn <- as.network.matrix(GWS06_Bft)
GWS06_B.netDensity <- network.density(GWS06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_B.entropy <- entropy(GWS06_Bft) #entropy

GWS06_B.netMx <- cbind(GWS06_B.netMx, GWS06_B.clusterCoef, GWS06_B.degreeCent$centralization,
                       GWS06_B.netDensity, GWS06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
GWS06_SFg2 <- data.frame(GWS06_SF)
GWS06_SFg2 <- GWS06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_SFg2$player1
player2vector <- GWS06_SFg2$player2
GWS06_SFg3 <- GWS06_SFg2
GWS06_SFg3$p1inp2vec <- is.element(GWS06_SFg3$player1, player2vector)
GWS06_SFg3$p2inp1vec <- is.element(GWS06_SFg3$player2, player1vector)

addPlayer1 <- GWS06_SFg3[ which(GWS06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_SFg3[ which(GWS06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_SFg2 <- rbind(GWS06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
GWS06_SFft <- ftable(GWS06_SFg2$player1, GWS06_SFg2$player2)
GWS06_SFft2 <- as.matrix(GWS06_SFft)
numRows <- nrow(GWS06_SFft2)
numCols <- ncol(GWS06_SFft2)
GWS06_SFft3 <- GWS06_SFft2[c(2:numRows) , c(2:numCols)]
GWS06_SFTable <- graph.adjacency(GWS06_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(GWS06_SFTable, vertex.label = V(GWS06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
GWS06_SF.clusterCoef <- transitivity(GWS06_SFTable, type="global") #cluster coefficient
GWS06_SF.degreeCent <- centralization.degree(GWS06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_SFftn <- as.network.matrix(GWS06_SFft)
GWS06_SF.netDensity <- network.density(GWS06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_SF.entropy <- entropy(GWS06_SFft) #entropy

GWS06_SF.netMx <- cbind(GWS06_SF.netMx, GWS06_SF.clusterCoef, GWS06_SF.degreeCent$centralization,
                        GWS06_SF.netDensity, GWS06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************
#NA

round = 6
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
GWS06_TFg2 <- data.frame(GWS06_TF)
GWS06_TFg2 <- GWS06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_TFg2$player1
player2vector <- GWS06_TFg2$player2
GWS06_TFg3 <- GWS06_TFg2
GWS06_TFg3$p1inp2vec <- is.element(GWS06_TFg3$player1, player2vector)
GWS06_TFg3$p2inp1vec <- is.element(GWS06_TFg3$player2, player1vector)

addPlayer1 <- GWS06_TFg3[ which(GWS06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_TFg3[ which(GWS06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_TFg2 <- rbind(GWS06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
GWS06_TFft <- ftable(GWS06_TFg2$player1, GWS06_TFg2$player2)
GWS06_TFft2 <- as.matrix(GWS06_TFft)
numRows <- nrow(GWS06_TFft2)
numCols <- ncol(GWS06_TFft2)
GWS06_TFft3 <- GWS06_TFft2[c(2:numRows) , c(2:numCols)]
GWS06_TFTable <- graph.adjacency(GWS06_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(GWS06_TFTable, vertex.label = V(GWS06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
GWS06_TF.clusterCoef <- transitivity(GWS06_TFTable, type="global") #cluster coefficient
GWS06_TF.degreeCent <- centralization.degree(GWS06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_TFftn <- as.network.matrix(GWS06_TFft)
GWS06_TF.netDensity <- network.density(GWS06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_TF.entropy <- entropy(GWS06_TFft) #entropy

GWS06_TF.netMx <- cbind(GWS06_TF.netMx, GWS06_TF.clusterCoef, GWS06_TF.degreeCent$centralization,
                        GWS06_TF.netDensity, GWS06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
GWS06_SAMg2 <- data.frame(GWS06_SAM)
GWS06_SAMg2 <- GWS06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_SAMg2$player1
player2vector <- GWS06_SAMg2$player2
GWS06_SAMg3 <- GWS06_SAMg2
GWS06_SAMg3$p1inp2vec <- is.element(GWS06_SAMg3$player1, player2vector)
GWS06_SAMg3$p2inp1vec <- is.element(GWS06_SAMg3$player2, player1vector)

addPlayer1 <- GWS06_SAMg3[ which(GWS06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_SAMg3[ which(GWS06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_SAMg2 <- rbind(GWS06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
GWS06_SAMft <- ftable(GWS06_SAMg2$player1, GWS06_SAMg2$player2)
GWS06_SAMft2 <- as.matrix(GWS06_SAMft)
numRows <- nrow(GWS06_SAMft2)
numCols <- ncol(GWS06_SAMft2)
GWS06_SAMft3 <- GWS06_SAMft2[c(2:numRows) , c(2:numCols)]
GWS06_SAMTable <- graph.adjacency(GWS06_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(GWS06_SAMTable, vertex.label = V(GWS06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
GWS06_SAM.clusterCoef <- transitivity(GWS06_SAMTable, type="global") #cluster coefficient
GWS06_SAM.degreeCent <- centralization.degree(GWS06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_SAMftn <- as.network.matrix(GWS06_SAMft)
GWS06_SAM.netDensity <- network.density(GWS06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_SAM.entropy <- entropy(GWS06_SAMft) #entropy

GWS06_SAM.netMx <- cbind(GWS06_SAM.netMx, GWS06_SAM.clusterCoef, GWS06_SAM.degreeCent$centralization,
                         GWS06_SAM.netDensity, GWS06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************
#NA

round = 6
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
GWS06_TAMg2 <- data.frame(GWS06_TAM)
GWS06_TAMg2 <- GWS06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_TAMg2$player1
player2vector <- GWS06_TAMg2$player2
GWS06_TAMg3 <- GWS06_TAMg2
GWS06_TAMg3$p1inp2vec <- is.element(GWS06_TAMg3$player1, player2vector)
GWS06_TAMg3$p2inp1vec <- is.element(GWS06_TAMg3$player2, player1vector)

addPlayer1 <- GWS06_TAMg3[ which(GWS06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_TAMg3[ which(GWS06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_TAMg2 <- rbind(GWS06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
GWS06_TAMft <- ftable(GWS06_TAMg2$player1, GWS06_TAMg2$player2)
GWS06_TAMft2 <- as.matrix(GWS06_TAMft)
numRows <- nrow(GWS06_TAMft2)
numCols <- ncol(GWS06_TAMft2)
GWS06_TAMft3 <- GWS06_TAMft2[c(2:numRows) , c(2:numCols)]
GWS06_TAMTable <- graph.adjacency(GWS06_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(GWS06_TAMTable, vertex.label = V(GWS06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
GWS06_TAM.clusterCoef <- transitivity(GWS06_TAMTable, type="global") #cluster coefficient
GWS06_TAM.degreeCent <- centralization.degree(GWS06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_TAMftn <- as.network.matrix(GWS06_TAMft)
GWS06_TAM.netDensity <- network.density(GWS06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_TAM.entropy <- entropy(GWS06_TAMft) #entropy

GWS06_TAM.netMx <- cbind(GWS06_TAM.netMx, GWS06_TAM.clusterCoef, GWS06_TAM.degreeCent$centralization,
                         GWS06_TAM.netDensity, GWS06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
GWS06_SDMg2 <- data.frame(GWS06_SDM)
GWS06_SDMg2 <- GWS06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_SDMg2$player1
player2vector <- GWS06_SDMg2$player2
GWS06_SDMg3 <- GWS06_SDMg2
GWS06_SDMg3$p1inp2vec <- is.element(GWS06_SDMg3$player1, player2vector)
GWS06_SDMg3$p2inp1vec <- is.element(GWS06_SDMg3$player2, player1vector)

addPlayer1 <- GWS06_SDMg3[ which(GWS06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS06_SDMg3[ which(GWS06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_SDMg2 <- rbind(GWS06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
GWS06_SDMft <- ftable(GWS06_SDMg2$player1, GWS06_SDMg2$player2)
GWS06_SDMft2 <- as.matrix(GWS06_SDMft)
numRows <- nrow(GWS06_SDMft2)
numCols <- ncol(GWS06_SDMft2)
GWS06_SDMft3 <- GWS06_SDMft2[c(2:numRows) , c(2:numCols)]
GWS06_SDMTable <- graph.adjacency(GWS06_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(GWS06_SDMTable, vertex.label = V(GWS06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
GWS06_SDM.clusterCoef <- transitivity(GWS06_SDMTable, type="global") #cluster coefficient
GWS06_SDM.degreeCent <- centralization.degree(GWS06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_SDMftn <- as.network.matrix(GWS06_SDMft)
GWS06_SDM.netDensity <- network.density(GWS06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_SDM.entropy <- entropy(GWS06_SDMft) #entropy

GWS06_SDM.netMx <- cbind(GWS06_SDM.netMx, GWS06_SDM.clusterCoef, GWS06_SDM.degreeCent$centralization,
                         GWS06_SDM.netDensity, GWS06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
GWS06_TDMg2 <- data.frame(GWS06_TDM)
GWS06_TDMg2 <- GWS06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_TDMg2$player1
player2vector <- GWS06_TDMg2$player2
GWS06_TDMg3 <- GWS06_TDMg2
GWS06_TDMg3$p1inp2vec <- is.element(GWS06_TDMg3$player1, player2vector)
GWS06_TDMg3$p2inp1vec <- is.element(GWS06_TDMg3$player2, player1vector)

addPlayer1 <- GWS06_TDMg3[ which(GWS06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_TDMg3[ which(GWS06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_TDMg2 <- rbind(GWS06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
GWS06_TDMft <- ftable(GWS06_TDMg2$player1, GWS06_TDMg2$player2)
GWS06_TDMft2 <- as.matrix(GWS06_TDMft)
numRows <- nrow(GWS06_TDMft2)
numCols <- ncol(GWS06_TDMft2)
GWS06_TDMft3 <- GWS06_TDMft2[c(2:numRows) , c(2:numCols)]
GWS06_TDMTable <- graph.adjacency(GWS06_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(GWS06_TDMTable, vertex.label = V(GWS06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
GWS06_TDM.clusterCoef <- transitivity(GWS06_TDMTable, type="global") #cluster coefficient
GWS06_TDM.degreeCent <- centralization.degree(GWS06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_TDMftn <- as.network.matrix(GWS06_TDMft)
GWS06_TDM.netDensity <- network.density(GWS06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_TDM.entropy <- entropy(GWS06_TDMft) #entropy

GWS06_TDM.netMx <- cbind(GWS06_TDM.netMx, GWS06_TDM.clusterCoef, GWS06_TDM.degreeCent$centralization,
                         GWS06_TDM.netDensity, GWS06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
GWS06_SDg2 <- data.frame(GWS06_SD)
GWS06_SDg2 <- GWS06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_SDg2$player1
player2vector <- GWS06_SDg2$player2
GWS06_SDg3 <- GWS06_SDg2
GWS06_SDg3$p1inp2vec <- is.element(GWS06_SDg3$player1, player2vector)
GWS06_SDg3$p2inp1vec <- is.element(GWS06_SDg3$player2, player1vector)

addPlayer1 <- GWS06_SDg3[ which(GWS06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_SDg3[ which(GWS06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_SDg2 <- rbind(GWS06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
GWS06_SDft <- ftable(GWS06_SDg2$player1, GWS06_SDg2$player2)
GWS06_SDft2 <- as.matrix(GWS06_SDft)
numRows <- nrow(GWS06_SDft2)
numCols <- ncol(GWS06_SDft2)
GWS06_SDft3 <- GWS06_SDft2[c(2:numRows) , c(2:numCols)]
GWS06_SDTable <- graph.adjacency(GWS06_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(GWS06_SDTable, vertex.label = V(GWS06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
GWS06_SD.clusterCoef <- transitivity(GWS06_SDTable, type="global") #cluster coefficient
GWS06_SD.degreeCent <- centralization.degree(GWS06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_SDftn <- as.network.matrix(GWS06_SDft)
GWS06_SD.netDensity <- network.density(GWS06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_SD.entropy <- entropy(GWS06_SDft) #entropy

GWS06_SD.netMx <- cbind(GWS06_SD.netMx, GWS06_SD.clusterCoef, GWS06_SD.degreeCent$centralization,
                        GWS06_SD.netDensity, GWS06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
GWS06_TDg2 <- data.frame(GWS06_TD)
GWS06_TDg2 <- GWS06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_TDg2$player1
player2vector <- GWS06_TDg2$player2
GWS06_TDg3 <- GWS06_TDg2
GWS06_TDg3$p1inp2vec <- is.element(GWS06_TDg3$player1, player2vector)
GWS06_TDg3$p2inp1vec <- is.element(GWS06_TDg3$player2, player1vector)

addPlayer1 <- GWS06_TDg3[ which(GWS06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_TDg3[ which(GWS06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_TDg2 <- rbind(GWS06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
GWS06_TDft <- ftable(GWS06_TDg2$player1, GWS06_TDg2$player2)
GWS06_TDft2 <- as.matrix(GWS06_TDft)
numRows <- nrow(GWS06_TDft2)
numCols <- ncol(GWS06_TDft2)
GWS06_TDft3 <- GWS06_TDft2[c(2:numRows) , c(2:numCols)]
GWS06_TDTable <- graph.adjacency(GWS06_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(GWS06_TDTable, vertex.label = V(GWS06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
GWS06_TD.clusterCoef <- transitivity(GWS06_TDTable, type="global") #cluster coefficient
GWS06_TD.degreeCent <- centralization.degree(GWS06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_TDftn <- as.network.matrix(GWS06_TDft)
GWS06_TD.netDensity <- network.density(GWS06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_TD.entropy <- entropy(GWS06_TDft) #entropy

GWS06_TD.netMx <- cbind(GWS06_TD.netMx, GWS06_TD.clusterCoef, GWS06_TD.degreeCent$centralization,
                        GWS06_TD.netDensity, GWS06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
GWS06_QTg2 <- data.frame(GWS06_QT)
GWS06_QTg2 <- GWS06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS06_QTg2$player1
player2vector <- GWS06_QTg2$player2
GWS06_QTg3 <- GWS06_QTg2
GWS06_QTg3$p1inp2vec <- is.element(GWS06_QTg3$player1, player2vector)
GWS06_QTg3$p2inp1vec <- is.element(GWS06_QTg3$player2, player1vector)

addPlayer1 <- GWS06_QTg3[ which(GWS06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS06_QTg3[ which(GWS06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS06_QTg2 <- rbind(GWS06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
GWS06_QTft <- ftable(GWS06_QTg2$player1, GWS06_QTg2$player2)
GWS06_QTft2 <- as.matrix(GWS06_QTft)
numRows <- nrow(GWS06_QTft2)
numCols <- ncol(GWS06_QTft2)
GWS06_QTft3 <- GWS06_QTft2[c(2:numRows) , c(2:numCols)]
GWS06_QTTable <- graph.adjacency(GWS06_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(GWS06_QTTable, vertex.label = V(GWS06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
GWS06_QT.clusterCoef <- transitivity(GWS06_QTTable, type="global") #cluster coefficient
GWS06_QT.degreeCent <- centralization.degree(GWS06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS06_QTftn <- as.network.matrix(GWS06_QTft)
GWS06_QT.netDensity <- network.density(GWS06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS06_QT.entropy <- entropy(GWS06_QTft) #entropy

GWS06_QT.netMx <- cbind(GWS06_QT.netMx, GWS06_QT.clusterCoef, GWS06_QT.degreeCent$centralization,
                        GWS06_QT.netDensity, GWS06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS06_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "HAW"
KIoutcome = "Goal_F"
HAW06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
HAW06_Gg2 <- data.frame(HAW06_G)
HAW06_Gg2 <- HAW06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_Gg2$player1
player2vector <- HAW06_Gg2$player2
HAW06_Gg3 <- HAW06_Gg2
HAW06_Gg3$p1inp2vec <- is.element(HAW06_Gg3$player1, player2vector)
HAW06_Gg3$p2inp1vec <- is.element(HAW06_Gg3$player2, player1vector)

addPlayer1 <- HAW06_Gg3[ which(HAW06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW06_Gg3[ which(HAW06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_Gg2 <- rbind(HAW06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
HAW06_Gft <- ftable(HAW06_Gg2$player1, HAW06_Gg2$player2)
HAW06_Gft2 <- as.matrix(HAW06_Gft)
numRows <- nrow(HAW06_Gft2)
numCols <- ncol(HAW06_Gft2)
HAW06_Gft3 <- HAW06_Gft2[c(2:numRows) , c(2:numCols)]
HAW06_GTable <- graph.adjacency(HAW06_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(HAW06_GTable, vertex.label = V(HAW06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
HAW06_G.clusterCoef <- transitivity(HAW06_GTable, type="global") #cluster coefficient
HAW06_G.degreeCent <- centralization.degree(HAW06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_Gftn <- as.network.matrix(HAW06_Gft)
HAW06_G.netDensity <- network.density(HAW06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_G.entropy <- entropy(HAW06_Gft) #entropy

HAW06_G.netMx <- cbind(HAW06_G.netMx, HAW06_G.clusterCoef, HAW06_G.degreeCent$centralization,
                       HAW06_G.netDensity, HAW06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************
#NA

round = 6
teamName = "HAW"
KIoutcome = "Behind_F"
HAW06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
HAW06_Bg2 <- data.frame(HAW06_B)
HAW06_Bg2 <- HAW06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_Bg2$player1
player2vector <- HAW06_Bg2$player2
HAW06_Bg3 <- HAW06_Bg2
HAW06_Bg3$p1inp2vec <- is.element(HAW06_Bg3$player1, player2vector)
HAW06_Bg3$p2inp1vec <- is.element(HAW06_Bg3$player2, player1vector)

addPlayer1 <- HAW06_Bg3[ which(HAW06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW06_Bg2 <- rbind(HAW06_Bg2, addPlayer1)

#ROUND 6, Behind graph using weighted edges
HAW06_Bft <- ftable(HAW06_Bg2$player1, HAW06_Bg2$player2)
HAW06_Bft2 <- as.matrix(HAW06_Bft)
numRows <- nrow(HAW06_Bft2)
numCols <- ncol(HAW06_Bft2)
HAW06_Bft3 <- HAW06_Bft2[c(2:numRows) , c(1:numCols)]
HAW06_BTable <- graph.adjacency(HAW06_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(HAW06_BTable, vertex.label = V(HAW06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
HAW06_B.clusterCoef <- transitivity(HAW06_BTable, type="global") #cluster coefficient
HAW06_B.degreeCent <- centralization.degree(HAW06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_Bftn <- as.network.matrix(HAW06_Bft)
HAW06_B.netDensity <- network.density(HAW06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_B.entropy <- entropy(HAW06_Bft) #entropy

HAW06_B.netMx <- cbind(HAW06_B.netMx, HAW06_B.clusterCoef, HAW06_B.degreeCent$centralization,
                       HAW06_B.netDensity, HAW06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
HAW06_SFg2 <- data.frame(HAW06_SF)
HAW06_SFg2 <- HAW06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_SFg2$player1
player2vector <- HAW06_SFg2$player2
HAW06_SFg3 <- HAW06_SFg2
HAW06_SFg3$p1inp2vec <- is.element(HAW06_SFg3$player1, player2vector)
HAW06_SFg3$p2inp1vec <- is.element(HAW06_SFg3$player2, player1vector)

addPlayer1 <- HAW06_SFg3[ which(HAW06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW06_SFg3[ which(HAW06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_SFg2 <- rbind(HAW06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
HAW06_SFft <- ftable(HAW06_SFg2$player1, HAW06_SFg2$player2)
HAW06_SFft2 <- as.matrix(HAW06_SFft)
numRows <- nrow(HAW06_SFft2)
numCols <- ncol(HAW06_SFft2)
HAW06_SFft3 <- HAW06_SFft2[c(2:numRows) , c(2:numCols)]
HAW06_SFTable <- graph.adjacency(HAW06_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(HAW06_SFTable, vertex.label = V(HAW06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
HAW06_SF.clusterCoef <- transitivity(HAW06_SFTable, type="global") #cluster coefficient
HAW06_SF.degreeCent <- centralization.degree(HAW06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_SFftn <- as.network.matrix(HAW06_SFft)
HAW06_SF.netDensity <- network.density(HAW06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_SF.entropy <- entropy(HAW06_SFft) #entropy

HAW06_SF.netMx <- cbind(HAW06_SF.netMx, HAW06_SF.clusterCoef, HAW06_SF.degreeCent$centralization,
                        HAW06_SF.netDensity, HAW06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************
#NA

round = 6
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
HAW06_TFg2 <- data.frame(HAW06_TF)
HAW06_TFg2 <- HAW06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_TFg2$player1
player2vector <- HAW06_TFg2$player2
HAW06_TFg3 <- HAW06_TFg2
HAW06_TFg3$p1inp2vec <- is.element(HAW06_TFg3$player1, player2vector)
HAW06_TFg3$p2inp1vec <- is.element(HAW06_TFg3$player2, player1vector)

addPlayer1 <- HAW06_TFg3[ which(HAW06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW06_TFg3[ which(HAW06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_TFg2 <- rbind(HAW06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
HAW06_TFft <- ftable(HAW06_TFg2$player1, HAW06_TFg2$player2)
HAW06_TFft2 <- as.matrix(HAW06_TFft)
numRows <- nrow(HAW06_TFft2)
numCols <- ncol(HAW06_TFft2)
HAW06_TFft3 <- HAW06_TFft2[c(2:numRows) , c(2:numCols)]
HAW06_TFTable <- graph.adjacency(HAW06_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(HAW06_TFTable, vertex.label = V(HAW06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
HAW06_TF.clusterCoef <- transitivity(HAW06_TFTable, type="global") #cluster coefficient
HAW06_TF.degreeCent <- centralization.degree(HAW06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_TFftn <- as.network.matrix(HAW06_TFft)
HAW06_TF.netDensity <- network.density(HAW06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_TF.entropy <- entropy(HAW06_TFft) #entropy

HAW06_TF.netMx <- cbind(HAW06_TF.netMx, HAW06_TF.clusterCoef, HAW06_TF.degreeCent$centralization,
                        HAW06_TF.netDensity, HAW06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
HAW06_SAMg2 <- data.frame(HAW06_SAM)
HAW06_SAMg2 <- HAW06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_SAMg2$player1
player2vector <- HAW06_SAMg2$player2
HAW06_SAMg3 <- HAW06_SAMg2
HAW06_SAMg3$p1inp2vec <- is.element(HAW06_SAMg3$player1, player2vector)
HAW06_SAMg3$p2inp1vec <- is.element(HAW06_SAMg3$player2, player1vector)

addPlayer1 <- HAW06_SAMg3[ which(HAW06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW06_SAMg3[ which(HAW06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_SAMg2 <- rbind(HAW06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
HAW06_SAMft <- ftable(HAW06_SAMg2$player1, HAW06_SAMg2$player2)
HAW06_SAMft2 <- as.matrix(HAW06_SAMft)
numRows <- nrow(HAW06_SAMft2)
numCols <- ncol(HAW06_SAMft2)
HAW06_SAMft3 <- HAW06_SAMft2[c(2:numRows) , c(2:numCols)]
HAW06_SAMTable <- graph.adjacency(HAW06_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(HAW06_SAMTable, vertex.label = V(HAW06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
HAW06_SAM.clusterCoef <- transitivity(HAW06_SAMTable, type="global") #cluster coefficient
HAW06_SAM.degreeCent <- centralization.degree(HAW06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_SAMftn <- as.network.matrix(HAW06_SAMft)
HAW06_SAM.netDensity <- network.density(HAW06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_SAM.entropy <- entropy(HAW06_SAMft) #entropy

HAW06_SAM.netMx <- cbind(HAW06_SAM.netMx, HAW06_SAM.clusterCoef, HAW06_SAM.degreeCent$centralization,
                         HAW06_SAM.netDensity, HAW06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************

round = 6
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
HAW06_TAMg2 <- data.frame(HAW06_TAM)
HAW06_TAMg2 <- HAW06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_TAMg2$player1
player2vector <- HAW06_TAMg2$player2
HAW06_TAMg3 <- HAW06_TAMg2
HAW06_TAMg3$p1inp2vec <- is.element(HAW06_TAMg3$player1, player2vector)
HAW06_TAMg3$p2inp1vec <- is.element(HAW06_TAMg3$player2, player1vector)

addPlayer1 <- HAW06_TAMg3[ which(HAW06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW06_TAMg3[ which(HAW06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_TAMg2 <- rbind(HAW06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
HAW06_TAMft <- ftable(HAW06_TAMg2$player1, HAW06_TAMg2$player2)
HAW06_TAMft2 <- as.matrix(HAW06_TAMft)
numRows <- nrow(HAW06_TAMft2)
numCols <- ncol(HAW06_TAMft2)
HAW06_TAMft3 <- HAW06_TAMft2[c(2:numRows) , c(2:numCols)]
HAW06_TAMTable <- graph.adjacency(HAW06_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(HAW06_TAMTable, vertex.label = V(HAW06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
HAW06_TAM.clusterCoef <- transitivity(HAW06_TAMTable, type="global") #cluster coefficient
HAW06_TAM.degreeCent <- centralization.degree(HAW06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_TAMftn <- as.network.matrix(HAW06_TAMft)
HAW06_TAM.netDensity <- network.density(HAW06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_TAM.entropy <- entropy(HAW06_TAMft) #entropy

HAW06_TAM.netMx <- cbind(HAW06_TAM.netMx, HAW06_TAM.clusterCoef, HAW06_TAM.degreeCent$centralization,
                         HAW06_TAM.netDensity, HAW06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************
#NA

round = 6
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
HAW06_SDMg2 <- data.frame(HAW06_SDM)
HAW06_SDMg2 <- HAW06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_SDMg2$player1
player2vector <- HAW06_SDMg2$player2
HAW06_SDMg3 <- HAW06_SDMg2
HAW06_SDMg3$p1inp2vec <- is.element(HAW06_SDMg3$player1, player2vector)
HAW06_SDMg3$p2inp1vec <- is.element(HAW06_SDMg3$player2, player1vector)

addPlayer1 <- HAW06_SDMg3[ which(HAW06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW06_SDMg3[ which(HAW06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_SDMg2 <- rbind(HAW06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
HAW06_SDMft <- ftable(HAW06_SDMg2$player1, HAW06_SDMg2$player2)
HAW06_SDMft2 <- as.matrix(HAW06_SDMft)
numRows <- nrow(HAW06_SDMft2)
numCols <- ncol(HAW06_SDMft2)
HAW06_SDMft3 <- HAW06_SDMft2[c(2:numRows) , c(2:numCols)]
HAW06_SDMTable <- graph.adjacency(HAW06_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(HAW06_SDMTable, vertex.label = V(HAW06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
HAW06_SDM.clusterCoef <- transitivity(HAW06_SDMTable, type="global") #cluster coefficient
HAW06_SDM.degreeCent <- centralization.degree(HAW06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_SDMftn <- as.network.matrix(HAW06_SDMft)
HAW06_SDM.netDensity <- network.density(HAW06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_SDM.entropy <- entropy(HAW06_SDMft) #entropy

HAW06_SDM.netMx <- cbind(HAW06_SDM.netMx, HAW06_SDM.clusterCoef, HAW06_SDM.degreeCent$centralization,
                         HAW06_SDM.netDensity, HAW06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
HAW06_TDMg2 <- data.frame(HAW06_TDM)
HAW06_TDMg2 <- HAW06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_TDMg2$player1
player2vector <- HAW06_TDMg2$player2
HAW06_TDMg3 <- HAW06_TDMg2
HAW06_TDMg3$p1inp2vec <- is.element(HAW06_TDMg3$player1, player2vector)
HAW06_TDMg3$p2inp1vec <- is.element(HAW06_TDMg3$player2, player1vector)

addPlayer1 <- HAW06_TDMg3[ which(HAW06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW06_TDMg3[ which(HAW06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_TDMg2 <- rbind(HAW06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
HAW06_TDMft <- ftable(HAW06_TDMg2$player1, HAW06_TDMg2$player2)
HAW06_TDMft2 <- as.matrix(HAW06_TDMft)
numRows <- nrow(HAW06_TDMft2)
numCols <- ncol(HAW06_TDMft2)
HAW06_TDMft3 <- HAW06_TDMft2[c(2:numRows) , c(2:numCols)]
HAW06_TDMTable <- graph.adjacency(HAW06_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(HAW06_TDMTable, vertex.label = V(HAW06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
HAW06_TDM.clusterCoef <- transitivity(HAW06_TDMTable, type="global") #cluster coefficient
HAW06_TDM.degreeCent <- centralization.degree(HAW06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_TDMftn <- as.network.matrix(HAW06_TDMft)
HAW06_TDM.netDensity <- network.density(HAW06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_TDM.entropy <- entropy(HAW06_TDMft) #entropy

HAW06_TDM.netMx <- cbind(HAW06_TDM.netMx, HAW06_TDM.clusterCoef, HAW06_TDM.degreeCent$centralization,
                         HAW06_TDM.netDensity, HAW06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************

round = 6
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
HAW06_SDg2 <- data.frame(HAW06_SD)
HAW06_SDg2 <- HAW06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_SDg2$player1
player2vector <- HAW06_SDg2$player2
HAW06_SDg3 <- HAW06_SDg2
HAW06_SDg3$p1inp2vec <- is.element(HAW06_SDg3$player1, player2vector)
HAW06_SDg3$p2inp1vec <- is.element(HAW06_SDg3$player2, player1vector)

addPlayer1 <- HAW06_SDg3[ which(HAW06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW06_SDg3[ which(HAW06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_SDg2 <- rbind(HAW06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
HAW06_SDft <- ftable(HAW06_SDg2$player1, HAW06_SDg2$player2)
HAW06_SDft2 <- as.matrix(HAW06_SDft)
numRows <- nrow(HAW06_SDft2)
numCols <- ncol(HAW06_SDft2)
HAW06_SDft3 <- HAW06_SDft2[c(2:numRows) , c(2:numCols)]
HAW06_SDTable <- graph.adjacency(HAW06_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(HAW06_SDTable, vertex.label = V(HAW06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
HAW06_SD.clusterCoef <- transitivity(HAW06_SDTable, type="global") #cluster coefficient
HAW06_SD.degreeCent <- centralization.degree(HAW06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_SDftn <- as.network.matrix(HAW06_SDft)
HAW06_SD.netDensity <- network.density(HAW06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_SD.entropy <- entropy(HAW06_SDft) #entropy

HAW06_SD.netMx <- cbind(HAW06_SD.netMx, HAW06_SD.clusterCoef, HAW06_SD.degreeCent$centralization,
                        HAW06_SD.netDensity, HAW06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************

round = 6
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
HAW06_TDg2 <- data.frame(HAW06_TD)
HAW06_TDg2 <- HAW06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_TDg2$player1
player2vector <- HAW06_TDg2$player2
HAW06_TDg3 <- HAW06_TDg2
HAW06_TDg3$p1inp2vec <- is.element(HAW06_TDg3$player1, player2vector)
HAW06_TDg3$p2inp1vec <- is.element(HAW06_TDg3$player2, player1vector)

addPlayer1 <- HAW06_TDg3[ which(HAW06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW06_TDg3[ which(HAW06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_TDg2 <- rbind(HAW06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
HAW06_TDft <- ftable(HAW06_TDg2$player1, HAW06_TDg2$player2)
HAW06_TDft2 <- as.matrix(HAW06_TDft)
numRows <- nrow(HAW06_TDft2)
numCols <- ncol(HAW06_TDft2)
HAW06_TDft3 <- HAW06_TDft2[c(2:numRows) , c(2:numCols)]
HAW06_TDTable <- graph.adjacency(HAW06_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(HAW06_TDTable, vertex.label = V(HAW06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
HAW06_TD.clusterCoef <- transitivity(HAW06_TDTable, type="global") #cluster coefficient
HAW06_TD.degreeCent <- centralization.degree(HAW06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_TDftn <- as.network.matrix(HAW06_TDft)
HAW06_TD.netDensity <- network.density(HAW06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_TD.entropy <- entropy(HAW06_TDft) #entropy

HAW06_TD.netMx <- cbind(HAW06_TD.netMx, HAW06_TD.clusterCoef, HAW06_TD.degreeCent$centralization,
                        HAW06_TD.netDensity, HAW06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
HAW06_QTg2 <- data.frame(HAW06_QT)
HAW06_QTg2 <- HAW06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW06_QTg2$player1
player2vector <- HAW06_QTg2$player2
HAW06_QTg3 <- HAW06_QTg2
HAW06_QTg3$p1inp2vec <- is.element(HAW06_QTg3$player1, player2vector)
HAW06_QTg3$p2inp1vec <- is.element(HAW06_QTg3$player2, player1vector)

addPlayer1 <- HAW06_QTg3[ which(HAW06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW06_QTg3[ which(HAW06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW06_QTg2 <- rbind(HAW06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
HAW06_QTft <- ftable(HAW06_QTg2$player1, HAW06_QTg2$player2)
HAW06_QTft2 <- as.matrix(HAW06_QTft)
numRows <- nrow(HAW06_QTft2)
numCols <- ncol(HAW06_QTft2)
HAW06_QTft3 <- HAW06_QTft2[c(2:numRows) , c(2:numCols)]
HAW06_QTTable <- graph.adjacency(HAW06_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(HAW06_QTTable, vertex.label = V(HAW06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
HAW06_QT.clusterCoef <- transitivity(HAW06_QTTable, type="global") #cluster coefficient
HAW06_QT.degreeCent <- centralization.degree(HAW06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW06_QTftn <- as.network.matrix(HAW06_QTft)
HAW06_QT.netDensity <- network.density(HAW06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW06_QT.entropy <- entropy(HAW06_QTft) #entropy

HAW06_QT.netMx <- cbind(HAW06_QT.netMx, HAW06_QT.clusterCoef, HAW06_QT.degreeCent$centralization,
                        HAW06_QT.netDensity, HAW06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW06_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "MELB"
KIoutcome = "Goal_F"
MELB06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
MELB06_Gg2 <- data.frame(MELB06_G)
MELB06_Gg2 <- MELB06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_Gg2$player1
player2vector <- MELB06_Gg2$player2
MELB06_Gg3 <- MELB06_Gg2
MELB06_Gg3$p1inp2vec <- is.element(MELB06_Gg3$player1, player2vector)
MELB06_Gg3$p2inp1vec <- is.element(MELB06_Gg3$player2, player1vector)

addPlayer1 <- MELB06_Gg3[ which(MELB06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB06_Gg2 <- rbind(MELB06_Gg2, addPlayer1)

#ROUND 6, Goal graph using weighted edges
MELB06_Gft <- ftable(MELB06_Gg2$player1, MELB06_Gg2$player2)
MELB06_Gft2 <- as.matrix(MELB06_Gft)
numRows <- nrow(MELB06_Gft2)
numCols <- ncol(MELB06_Gft2)
MELB06_Gft3 <- MELB06_Gft2[c(2:numRows) , c(1:numCols)]
MELB06_GTable <- graph.adjacency(MELB06_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(MELB06_GTable, vertex.label = V(MELB06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
MELB06_G.clusterCoef <- transitivity(MELB06_GTable, type="global") #cluster coefficient
MELB06_G.degreeCent <- centralization.degree(MELB06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_Gftn <- as.network.matrix(MELB06_Gft)
MELB06_G.netDensity <- network.density(MELB06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_G.entropy <- entropy(MELB06_Gft) #entropy

MELB06_G.netMx <- cbind(MELB06_G.netMx, MELB06_G.clusterCoef, MELB06_G.degreeCent$centralization,
                        MELB06_G.netDensity, MELB06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "MELB"
KIoutcome = "Behind_F"
MELB06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
MELB06_Bg2 <- data.frame(MELB06_B)
MELB06_Bg2 <- MELB06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_Bg2$player1
player2vector <- MELB06_Bg2$player2
MELB06_Bg3 <- MELB06_Bg2
MELB06_Bg3$p1inp2vec <- is.element(MELB06_Bg3$player1, player2vector)
MELB06_Bg3$p2inp1vec <- is.element(MELB06_Bg3$player2, player1vector)

addPlayer1 <- MELB06_Bg3[ which(MELB06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_Bg3[ which(MELB06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_Bg2 <- rbind(MELB06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
MELB06_Bft <- ftable(MELB06_Bg2$player1, MELB06_Bg2$player2)
MELB06_Bft2 <- as.matrix(MELB06_Bft)
numRows <- nrow(MELB06_Bft2)
numCols <- ncol(MELB06_Bft2)
MELB06_Bft3 <- MELB06_Bft2[c(2:numRows) , c(2:numCols)]
MELB06_BTable <- graph.adjacency(MELB06_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(MELB06_BTable, vertex.label = V(MELB06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
MELB06_B.clusterCoef <- transitivity(MELB06_BTable, type="global") #cluster coefficient
MELB06_B.degreeCent <- centralization.degree(MELB06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_Bftn <- as.network.matrix(MELB06_Bft)
MELB06_B.netDensity <- network.density(MELB06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_B.entropy <- entropy(MELB06_Bft) #entropy

MELB06_B.netMx <- cbind(MELB06_B.netMx, MELB06_B.clusterCoef, MELB06_B.degreeCent$centralization,
                        MELB06_B.netDensity, MELB06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
MELB06_SFg2 <- data.frame(MELB06_SF)
MELB06_SFg2 <- MELB06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_SFg2$player1
player2vector <- MELB06_SFg2$player2
MELB06_SFg3 <- MELB06_SFg2
MELB06_SFg3$p1inp2vec <- is.element(MELB06_SFg3$player1, player2vector)
MELB06_SFg3$p2inp1vec <- is.element(MELB06_SFg3$player2, player1vector)

addPlayer1 <- MELB06_SFg3[ which(MELB06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_SFg3[ which(MELB06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_SFg2 <- rbind(MELB06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
MELB06_SFft <- ftable(MELB06_SFg2$player1, MELB06_SFg2$player2)
MELB06_SFft2 <- as.matrix(MELB06_SFft)
numRows <- nrow(MELB06_SFft2)
numCols <- ncol(MELB06_SFft2)
MELB06_SFft3 <- MELB06_SFft2[c(2:numRows) , c(2:numCols)]
MELB06_SFTable <- graph.adjacency(MELB06_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(MELB06_SFTable, vertex.label = V(MELB06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
MELB06_SF.clusterCoef <- transitivity(MELB06_SFTable, type="global") #cluster coefficient
MELB06_SF.degreeCent <- centralization.degree(MELB06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_SFftn <- as.network.matrix(MELB06_SFft)
MELB06_SF.netDensity <- network.density(MELB06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_SF.entropy <- entropy(MELB06_SFft) #entropy

MELB06_SF.netMx <- cbind(MELB06_SF.netMx, MELB06_SF.clusterCoef, MELB06_SF.degreeCent$centralization,
                         MELB06_SF.netDensity, MELB06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
MELB06_TFg2 <- data.frame(MELB06_TF)
MELB06_TFg2 <- MELB06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_TFg2$player1
player2vector <- MELB06_TFg2$player2
MELB06_TFg3 <- MELB06_TFg2
MELB06_TFg3$p1inp2vec <- is.element(MELB06_TFg3$player1, player2vector)
MELB06_TFg3$p2inp1vec <- is.element(MELB06_TFg3$player2, player1vector)

addPlayer1 <- MELB06_TFg3[ which(MELB06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_TFg3[ which(MELB06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_TFg2 <- rbind(MELB06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
MELB06_TFft <- ftable(MELB06_TFg2$player1, MELB06_TFg2$player2)
MELB06_TFft2 <- as.matrix(MELB06_TFft)
numRows <- nrow(MELB06_TFft2)
numCols <- ncol(MELB06_TFft2)
MELB06_TFft3 <- MELB06_TFft2[c(2:numRows) , c(2:numCols)]
MELB06_TFTable <- graph.adjacency(MELB06_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(MELB06_TFTable, vertex.label = V(MELB06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
MELB06_TF.clusterCoef <- transitivity(MELB06_TFTable, type="global") #cluster coefficient
MELB06_TF.degreeCent <- centralization.degree(MELB06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_TFftn <- as.network.matrix(MELB06_TFft)
MELB06_TF.netDensity <- network.density(MELB06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_TF.entropy <- entropy(MELB06_TFft) #entropy

MELB06_TF.netMx <- cbind(MELB06_TF.netMx, MELB06_TF.clusterCoef, MELB06_TF.degreeCent$centralization,
                         MELB06_TF.netDensity, MELB06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************

round = 6
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
MELB06_SAMg2 <- data.frame(MELB06_SAM)
MELB06_SAMg2 <- MELB06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_SAMg2$player1
player2vector <- MELB06_SAMg2$player2
MELB06_SAMg3 <- MELB06_SAMg2
MELB06_SAMg3$p1inp2vec <- is.element(MELB06_SAMg3$player1, player2vector)
MELB06_SAMg3$p2inp1vec <- is.element(MELB06_SAMg3$player2, player1vector)

addPlayer1 <- MELB06_SAMg3[ which(MELB06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_SAMg3[ which(MELB06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_SAMg2 <- rbind(MELB06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
MELB06_SAMft <- ftable(MELB06_SAMg2$player1, MELB06_SAMg2$player2)
MELB06_SAMft2 <- as.matrix(MELB06_SAMft)
numRows <- nrow(MELB06_SAMft2)
numCols <- ncol(MELB06_SAMft2)
MELB06_SAMft3 <- MELB06_SAMft2[c(2:numRows) , c(2:numCols)]
MELB06_SAMTable <- graph.adjacency(MELB06_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(MELB06_SAMTable, vertex.label = V(MELB06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
MELB06_SAM.clusterCoef <- transitivity(MELB06_SAMTable, type="global") #cluster coefficient
MELB06_SAM.degreeCent <- centralization.degree(MELB06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_SAMftn <- as.network.matrix(MELB06_SAMft)
MELB06_SAM.netDensity <- network.density(MELB06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_SAM.entropy <- entropy(MELB06_SAMft) #entropy

MELB06_SAM.netMx <- cbind(MELB06_SAM.netMx, MELB06_SAM.clusterCoef, MELB06_SAM.degreeCent$centralization,
                          MELB06_SAM.netDensity, MELB06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************

round = 6
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
MELB06_TAMg2 <- data.frame(MELB06_TAM)
MELB06_TAMg2 <- MELB06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_TAMg2$player1
player2vector <- MELB06_TAMg2$player2
MELB06_TAMg3 <- MELB06_TAMg2
MELB06_TAMg3$p1inp2vec <- is.element(MELB06_TAMg3$player1, player2vector)
MELB06_TAMg3$p2inp1vec <- is.element(MELB06_TAMg3$player2, player1vector)

addPlayer1 <- MELB06_TAMg3[ which(MELB06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_TAMg3[ which(MELB06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_TAMg2 <- rbind(MELB06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
MELB06_TAMft <- ftable(MELB06_TAMg2$player1, MELB06_TAMg2$player2)
MELB06_TAMft2 <- as.matrix(MELB06_TAMft)
numRows <- nrow(MELB06_TAMft2)
numCols <- ncol(MELB06_TAMft2)
MELB06_TAMft3 <- MELB06_TAMft2[c(2:numRows) , c(2:numCols)]
MELB06_TAMTable <- graph.adjacency(MELB06_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(MELB06_TAMTable, vertex.label = V(MELB06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
MELB06_TAM.clusterCoef <- transitivity(MELB06_TAMTable, type="global") #cluster coefficient
MELB06_TAM.degreeCent <- centralization.degree(MELB06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_TAMftn <- as.network.matrix(MELB06_TAMft)
MELB06_TAM.netDensity <- network.density(MELB06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_TAM.entropy <- entropy(MELB06_TAMft) #entropy

MELB06_TAM.netMx <- cbind(MELB06_TAM.netMx, MELB06_TAM.clusterCoef, MELB06_TAM.degreeCent$centralization,
                          MELB06_TAM.netDensity, MELB06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
MELB06_SDMg2 <- data.frame(MELB06_SDM)
MELB06_SDMg2 <- MELB06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_SDMg2$player1
player2vector <- MELB06_SDMg2$player2
MELB06_SDMg3 <- MELB06_SDMg2
MELB06_SDMg3$p1inp2vec <- is.element(MELB06_SDMg3$player1, player2vector)
MELB06_SDMg3$p2inp1vec <- is.element(MELB06_SDMg3$player2, player1vector)

addPlayer1 <- MELB06_SDMg3[ which(MELB06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_SDMg3[ which(MELB06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_SDMg2 <- rbind(MELB06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
MELB06_SDMft <- ftable(MELB06_SDMg2$player1, MELB06_SDMg2$player2)
MELB06_SDMft2 <- as.matrix(MELB06_SDMft)
numRows <- nrow(MELB06_SDMft2)
numCols <- ncol(MELB06_SDMft2)
MELB06_SDMft3 <- MELB06_SDMft2[c(2:numRows) , c(2:numCols)]
MELB06_SDMTable <- graph.adjacency(MELB06_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(MELB06_SDMTable, vertex.label = V(MELB06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
MELB06_SDM.clusterCoef <- transitivity(MELB06_SDMTable, type="global") #cluster coefficient
MELB06_SDM.degreeCent <- centralization.degree(MELB06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_SDMftn <- as.network.matrix(MELB06_SDMft)
MELB06_SDM.netDensity <- network.density(MELB06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_SDM.entropy <- entropy(MELB06_SDMft) #entropy

MELB06_SDM.netMx <- cbind(MELB06_SDM.netMx, MELB06_SDM.clusterCoef, MELB06_SDM.degreeCent$centralization,
                          MELB06_SDM.netDensity, MELB06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
MELB06_TDMg2 <- data.frame(MELB06_TDM)
MELB06_TDMg2 <- MELB06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_TDMg2$player1
player2vector <- MELB06_TDMg2$player2
MELB06_TDMg3 <- MELB06_TDMg2
MELB06_TDMg3$p1inp2vec <- is.element(MELB06_TDMg3$player1, player2vector)
MELB06_TDMg3$p2inp1vec <- is.element(MELB06_TDMg3$player2, player1vector)

addPlayer1 <- MELB06_TDMg3[ which(MELB06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_TDMg3[ which(MELB06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_TDMg2 <- rbind(MELB06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
MELB06_TDMft <- ftable(MELB06_TDMg2$player1, MELB06_TDMg2$player2)
MELB06_TDMft2 <- as.matrix(MELB06_TDMft)
numRows <- nrow(MELB06_TDMft2)
numCols <- ncol(MELB06_TDMft2)
MELB06_TDMft3 <- MELB06_TDMft2[c(2:numRows) , c(2:numCols)]
MELB06_TDMTable <- graph.adjacency(MELB06_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(MELB06_TDMTable, vertex.label = V(MELB06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
MELB06_TDM.clusterCoef <- transitivity(MELB06_TDMTable, type="global") #cluster coefficient
MELB06_TDM.degreeCent <- centralization.degree(MELB06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_TDMftn <- as.network.matrix(MELB06_TDMft)
MELB06_TDM.netDensity <- network.density(MELB06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_TDM.entropy <- entropy(MELB06_TDMft) #entropy

MELB06_TDM.netMx <- cbind(MELB06_TDM.netMx, MELB06_TDM.clusterCoef, MELB06_TDM.degreeCent$centralization,
                          MELB06_TDM.netDensity, MELB06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
MELB06_SDg2 <- data.frame(MELB06_SD)
MELB06_SDg2 <- MELB06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_SDg2$player1
player2vector <- MELB06_SDg2$player2
MELB06_SDg3 <- MELB06_SDg2
MELB06_SDg3$p1inp2vec <- is.element(MELB06_SDg3$player1, player2vector)
MELB06_SDg3$p2inp1vec <- is.element(MELB06_SDg3$player2, player1vector)

addPlayer1 <- MELB06_SDg3[ which(MELB06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_SDg3[ which(MELB06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_SDg2 <- rbind(MELB06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
MELB06_SDft <- ftable(MELB06_SDg2$player1, MELB06_SDg2$player2)
MELB06_SDft2 <- as.matrix(MELB06_SDft)
numRows <- nrow(MELB06_SDft2)
numCols <- ncol(MELB06_SDft2)
MELB06_SDft3 <- MELB06_SDft2[c(2:numRows) , c(2:numCols)]
MELB06_SDTable <- graph.adjacency(MELB06_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(MELB06_SDTable, vertex.label = V(MELB06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
MELB06_SD.clusterCoef <- transitivity(MELB06_SDTable, type="global") #cluster coefficient
MELB06_SD.degreeCent <- centralization.degree(MELB06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_SDftn <- as.network.matrix(MELB06_SDft)
MELB06_SD.netDensity <- network.density(MELB06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_SD.entropy <- entropy(MELB06_SDft) #entropy

MELB06_SD.netMx <- cbind(MELB06_SD.netMx, MELB06_SD.clusterCoef, MELB06_SD.degreeCent$centralization,
                         MELB06_SD.netDensity, MELB06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************

round = 6
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
MELB06_TDg2 <- data.frame(MELB06_TD)
MELB06_TDg2 <- MELB06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_TDg2$player1
player2vector <- MELB06_TDg2$player2
MELB06_TDg3 <- MELB06_TDg2
MELB06_TDg3$p1inp2vec <- is.element(MELB06_TDg3$player1, player2vector)
MELB06_TDg3$p2inp1vec <- is.element(MELB06_TDg3$player2, player1vector)

addPlayer1 <- MELB06_TDg3[ which(MELB06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_TDg3[ which(MELB06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_TDg2 <- rbind(MELB06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
MELB06_TDft <- ftable(MELB06_TDg2$player1, MELB06_TDg2$player2)
MELB06_TDft2 <- as.matrix(MELB06_TDft)
numRows <- nrow(MELB06_TDft2)
numCols <- ncol(MELB06_TDft2)
MELB06_TDft3 <- MELB06_TDft2[c(2:numRows) , c(2:numCols)]
MELB06_TDTable <- graph.adjacency(MELB06_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(MELB06_TDTable, vertex.label = V(MELB06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
MELB06_TD.clusterCoef <- transitivity(MELB06_TDTable, type="global") #cluster coefficient
MELB06_TD.degreeCent <- centralization.degree(MELB06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_TDftn <- as.network.matrix(MELB06_TDft)
MELB06_TD.netDensity <- network.density(MELB06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_TD.entropy <- entropy(MELB06_TDft) #entropy

MELB06_TD.netMx <- cbind(MELB06_TD.netMx, MELB06_TD.clusterCoef, MELB06_TD.degreeCent$centralization,
                         MELB06_TD.netDensity, MELB06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
MELB06_QTg2 <- data.frame(MELB06_QT)
MELB06_QTg2 <- MELB06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB06_QTg2$player1
player2vector <- MELB06_QTg2$player2
MELB06_QTg3 <- MELB06_QTg2
MELB06_QTg3$p1inp2vec <- is.element(MELB06_QTg3$player1, player2vector)
MELB06_QTg3$p2inp1vec <- is.element(MELB06_QTg3$player2, player1vector)

addPlayer1 <- MELB06_QTg3[ which(MELB06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB06_QTg3[ which(MELB06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB06_QTg2 <- rbind(MELB06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
MELB06_QTft <- ftable(MELB06_QTg2$player1, MELB06_QTg2$player2)
MELB06_QTft2 <- as.matrix(MELB06_QTft)
numRows <- nrow(MELB06_QTft2)
numCols <- ncol(MELB06_QTft2)
MELB06_QTft3 <- MELB06_QTft2[c(2:numRows) , c(2:numCols)]
MELB06_QTTable <- graph.adjacency(MELB06_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(MELB06_QTTable, vertex.label = V(MELB06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
MELB06_QT.clusterCoef <- transitivity(MELB06_QTTable, type="global") #cluster coefficient
MELB06_QT.degreeCent <- centralization.degree(MELB06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB06_QTftn <- as.network.matrix(MELB06_QTft)
MELB06_QT.netDensity <- network.density(MELB06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB06_QT.entropy <- entropy(MELB06_QTft) #entropy

MELB06_QT.netMx <- cbind(MELB06_QT.netMx, MELB06_QT.clusterCoef, MELB06_QT.degreeCent$centralization,
                         MELB06_QT.netDensity, MELB06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB06_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
NMFC06_Gg2 <- data.frame(NMFC06_G)
NMFC06_Gg2 <- NMFC06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_Gg2$player1
player2vector <- NMFC06_Gg2$player2
NMFC06_Gg3 <- NMFC06_Gg2
NMFC06_Gg3$p1inp2vec <- is.element(NMFC06_Gg3$player1, player2vector)
NMFC06_Gg3$p2inp1vec <- is.element(NMFC06_Gg3$player2, player1vector)

addPlayer1 <- NMFC06_Gg3[ which(NMFC06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC06_Gg3[ which(NMFC06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_Gg2 <- rbind(NMFC06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
NMFC06_Gft <- ftable(NMFC06_Gg2$player1, NMFC06_Gg2$player2)
NMFC06_Gft2 <- as.matrix(NMFC06_Gft)
numRows <- nrow(NMFC06_Gft2)
numCols <- ncol(NMFC06_Gft2)
NMFC06_Gft3 <- NMFC06_Gft2[c(2:numRows) , c(2:numCols)]
NMFC06_GTable <- graph.adjacency(NMFC06_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(NMFC06_GTable, vertex.label = V(NMFC06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
NMFC06_G.clusterCoef <- transitivity(NMFC06_GTable, type="global") #cluster coefficient
NMFC06_G.degreeCent <- centralization.degree(NMFC06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_Gftn <- as.network.matrix(NMFC06_Gft)
NMFC06_G.netDensity <- network.density(NMFC06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_G.entropy <- entropy(NMFC06_Gft) #entropy

NMFC06_G.netMx <- cbind(NMFC06_G.netMx, NMFC06_G.clusterCoef, NMFC06_G.degreeCent$centralization,
                        NMFC06_G.netDensity, NMFC06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
NMFC06_Bg2 <- data.frame(NMFC06_B)
NMFC06_Bg2 <- NMFC06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_Bg2$player1
player2vector <- NMFC06_Bg2$player2
NMFC06_Bg3 <- NMFC06_Bg2
NMFC06_Bg3$p1inp2vec <- is.element(NMFC06_Bg3$player1, player2vector)
NMFC06_Bg3$p2inp1vec <- is.element(NMFC06_Bg3$player2, player1vector)

addPlayer1 <- NMFC06_Bg3[ which(NMFC06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC06_Bg3[ which(NMFC06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_Bg2 <- rbind(NMFC06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
NMFC06_Bft <- ftable(NMFC06_Bg2$player1, NMFC06_Bg2$player2)
NMFC06_Bft2 <- as.matrix(NMFC06_Bft)
numRows <- nrow(NMFC06_Bft2)
numCols <- ncol(NMFC06_Bft2)
NMFC06_Bft3 <- NMFC06_Bft2[c(2:numRows) , c(2:numCols)]
NMFC06_BTable <- graph.adjacency(NMFC06_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(NMFC06_BTable, vertex.label = V(NMFC06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
NMFC06_B.clusterCoef <- transitivity(NMFC06_BTable, type="global") #cluster coefficient
NMFC06_B.degreeCent <- centralization.degree(NMFC06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_Bftn <- as.network.matrix(NMFC06_Bft)
NMFC06_B.netDensity <- network.density(NMFC06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_B.entropy <- entropy(NMFC06_Bft) #entropy

NMFC06_B.netMx <- cbind(NMFC06_B.netMx, NMFC06_B.clusterCoef, NMFC06_B.degreeCent$centralization,
                        NMFC06_B.netDensity, NMFC06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
NMFC06_SFg2 <- data.frame(NMFC06_SF)
NMFC06_SFg2 <- NMFC06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_SFg2$player1
player2vector <- NMFC06_SFg2$player2
NMFC06_SFg3 <- NMFC06_SFg2
NMFC06_SFg3$p1inp2vec <- is.element(NMFC06_SFg3$player1, player2vector)
NMFC06_SFg3$p2inp1vec <- is.element(NMFC06_SFg3$player2, player1vector)

addPlayer1 <- NMFC06_SFg3[ which(NMFC06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC06_SFg3[ which(NMFC06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_SFg2 <- rbind(NMFC06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
NMFC06_SFft <- ftable(NMFC06_SFg2$player1, NMFC06_SFg2$player2)
NMFC06_SFft2 <- as.matrix(NMFC06_SFft)
numRows <- nrow(NMFC06_SFft2)
numCols <- ncol(NMFC06_SFft2)
NMFC06_SFft3 <- NMFC06_SFft2[c(2:numRows) , c(2:numCols)]
NMFC06_SFTable <- graph.adjacency(NMFC06_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(NMFC06_SFTable, vertex.label = V(NMFC06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
NMFC06_SF.clusterCoef <- transitivity(NMFC06_SFTable, type="global") #cluster coefficient
NMFC06_SF.degreeCent <- centralization.degree(NMFC06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_SFftn <- as.network.matrix(NMFC06_SFft)
NMFC06_SF.netDensity <- network.density(NMFC06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_SF.entropy <- entropy(NMFC06_SFft) #entropy

NMFC06_SF.netMx <- cbind(NMFC06_SF.netMx, NMFC06_SF.clusterCoef, NMFC06_SF.degreeCent$centralization,
                         NMFC06_SF.netDensity, NMFC06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
NMFC06_TFg2 <- data.frame(NMFC06_TF)
NMFC06_TFg2 <- NMFC06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_TFg2$player1
player2vector <- NMFC06_TFg2$player2
NMFC06_TFg3 <- NMFC06_TFg2
NMFC06_TFg3$p1inp2vec <- is.element(NMFC06_TFg3$player1, player2vector)
NMFC06_TFg3$p2inp1vec <- is.element(NMFC06_TFg3$player2, player1vector)

addPlayer1 <- NMFC06_TFg3[ which(NMFC06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC06_TFg3[ which(NMFC06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_TFg2 <- rbind(NMFC06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
NMFC06_TFft <- ftable(NMFC06_TFg2$player1, NMFC06_TFg2$player2)
NMFC06_TFft2 <- as.matrix(NMFC06_TFft)
numRows <- nrow(NMFC06_TFft2)
numCols <- ncol(NMFC06_TFft2)
NMFC06_TFft3 <- NMFC06_TFft2[c(2:numRows) , c(2:numCols)]
NMFC06_TFTable <- graph.adjacency(NMFC06_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(NMFC06_TFTable, vertex.label = V(NMFC06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
NMFC06_TF.clusterCoef <- transitivity(NMFC06_TFTable, type="global") #cluster coefficient
NMFC06_TF.degreeCent <- centralization.degree(NMFC06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_TFftn <- as.network.matrix(NMFC06_TFft)
NMFC06_TF.netDensity <- network.density(NMFC06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_TF.entropy <- entropy(NMFC06_TFft) #entropy

NMFC06_TF.netMx <- cbind(NMFC06_TF.netMx, NMFC06_TF.clusterCoef, NMFC06_TF.degreeCent$centralization,
                         NMFC06_TF.netDensity, NMFC06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************

round = 6
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
NMFC06_SAMg2 <- data.frame(NMFC06_SAM)
NMFC06_SAMg2 <- NMFC06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_SAMg2$player1
player2vector <- NMFC06_SAMg2$player2
NMFC06_SAMg3 <- NMFC06_SAMg2
NMFC06_SAMg3$p1inp2vec <- is.element(NMFC06_SAMg3$player1, player2vector)
NMFC06_SAMg3$p2inp1vec <- is.element(NMFC06_SAMg3$player2, player1vector)

addPlayer1 <- NMFC06_SAMg3[ which(NMFC06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC06_SAMg3[ which(NMFC06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_SAMg2 <- rbind(NMFC06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
NMFC06_SAMft <- ftable(NMFC06_SAMg2$player1, NMFC06_SAMg2$player2)
NMFC06_SAMft2 <- as.matrix(NMFC06_SAMft)
numRows <- nrow(NMFC06_SAMft2)
numCols <- ncol(NMFC06_SAMft2)
NMFC06_SAMft3 <- NMFC06_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC06_SAMTable <- graph.adjacency(NMFC06_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(NMFC06_SAMTable, vertex.label = V(NMFC06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
NMFC06_SAM.clusterCoef <- transitivity(NMFC06_SAMTable, type="global") #cluster coefficient
NMFC06_SAM.degreeCent <- centralization.degree(NMFC06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_SAMftn <- as.network.matrix(NMFC06_SAMft)
NMFC06_SAM.netDensity <- network.density(NMFC06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_SAM.entropy <- entropy(NMFC06_SAMft) #entropy

NMFC06_SAM.netMx <- cbind(NMFC06_SAM.netMx, NMFC06_SAM.clusterCoef, NMFC06_SAM.degreeCent$centralization,
                          NMFC06_SAM.netDensity, NMFC06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************

round = 6
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
NMFC06_TAMg2 <- data.frame(NMFC06_TAM)
NMFC06_TAMg2 <- NMFC06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_TAMg2$player1
player2vector <- NMFC06_TAMg2$player2
NMFC06_TAMg3 <- NMFC06_TAMg2
NMFC06_TAMg3$p1inp2vec <- is.element(NMFC06_TAMg3$player1, player2vector)
NMFC06_TAMg3$p2inp1vec <- is.element(NMFC06_TAMg3$player2, player1vector)

addPlayer1 <- NMFC06_TAMg3[ which(NMFC06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC06_TAMg3[ which(NMFC06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_TAMg2 <- rbind(NMFC06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
NMFC06_TAMft <- ftable(NMFC06_TAMg2$player1, NMFC06_TAMg2$player2)
NMFC06_TAMft2 <- as.matrix(NMFC06_TAMft)
numRows <- nrow(NMFC06_TAMft2)
numCols <- ncol(NMFC06_TAMft2)
NMFC06_TAMft3 <- NMFC06_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC06_TAMTable <- graph.adjacency(NMFC06_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(NMFC06_TAMTable, vertex.label = V(NMFC06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
NMFC06_TAM.clusterCoef <- transitivity(NMFC06_TAMTable, type="global") #cluster coefficient
NMFC06_TAM.degreeCent <- centralization.degree(NMFC06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_TAMftn <- as.network.matrix(NMFC06_TAMft)
NMFC06_TAM.netDensity <- network.density(NMFC06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_TAM.entropy <- entropy(NMFC06_TAMft) #entropy

NMFC06_TAM.netMx <- cbind(NMFC06_TAM.netMx, NMFC06_TAM.clusterCoef, NMFC06_TAM.degreeCent$centralization,
                          NMFC06_TAM.netDensity, NMFC06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
NMFC06_SDMg2 <- data.frame(NMFC06_SDM)
NMFC06_SDMg2 <- NMFC06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_SDMg2$player1
player2vector <- NMFC06_SDMg2$player2
NMFC06_SDMg3 <- NMFC06_SDMg2
NMFC06_SDMg3$p1inp2vec <- is.element(NMFC06_SDMg3$player1, player2vector)
NMFC06_SDMg3$p2inp1vec <- is.element(NMFC06_SDMg3$player2, player1vector)

addPlayer1 <- NMFC06_SDMg3[ which(NMFC06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC06_SDMg3[ which(NMFC06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_SDMg2 <- rbind(NMFC06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
NMFC06_SDMft <- ftable(NMFC06_SDMg2$player1, NMFC06_SDMg2$player2)
NMFC06_SDMft2 <- as.matrix(NMFC06_SDMft)
numRows <- nrow(NMFC06_SDMft2)
numCols <- ncol(NMFC06_SDMft2)
NMFC06_SDMft3 <- NMFC06_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC06_SDMTable <- graph.adjacency(NMFC06_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(NMFC06_SDMTable, vertex.label = V(NMFC06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
NMFC06_SDM.clusterCoef <- transitivity(NMFC06_SDMTable, type="global") #cluster coefficient
NMFC06_SDM.degreeCent <- centralization.degree(NMFC06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_SDMftn <- as.network.matrix(NMFC06_SDMft)
NMFC06_SDM.netDensity <- network.density(NMFC06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_SDM.entropy <- entropy(NMFC06_SDMft) #entropy

NMFC06_SDM.netMx <- cbind(NMFC06_SDM.netMx, NMFC06_SDM.clusterCoef, NMFC06_SDM.degreeCent$centralization,
                          NMFC06_SDM.netDensity, NMFC06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
NMFC06_TDMg2 <- data.frame(NMFC06_TDM)
NMFC06_TDMg2 <- NMFC06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_TDMg2$player1
player2vector <- NMFC06_TDMg2$player2
NMFC06_TDMg3 <- NMFC06_TDMg2
NMFC06_TDMg3$p1inp2vec <- is.element(NMFC06_TDMg3$player1, player2vector)
NMFC06_TDMg3$p2inp1vec <- is.element(NMFC06_TDMg3$player2, player1vector)

addPlayer1 <- NMFC06_TDMg3[ which(NMFC06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC06_TDMg3[ which(NMFC06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_TDMg2 <- rbind(NMFC06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
NMFC06_TDMft <- ftable(NMFC06_TDMg2$player1, NMFC06_TDMg2$player2)
NMFC06_TDMft2 <- as.matrix(NMFC06_TDMft)
numRows <- nrow(NMFC06_TDMft2)
numCols <- ncol(NMFC06_TDMft2)
NMFC06_TDMft3 <- NMFC06_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC06_TDMTable <- graph.adjacency(NMFC06_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(NMFC06_TDMTable, vertex.label = V(NMFC06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
NMFC06_TDM.clusterCoef <- transitivity(NMFC06_TDMTable, type="global") #cluster coefficient
NMFC06_TDM.degreeCent <- centralization.degree(NMFC06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_TDMftn <- as.network.matrix(NMFC06_TDMft)
NMFC06_TDM.netDensity <- network.density(NMFC06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_TDM.entropy <- entropy(NMFC06_TDMft) #entropy

NMFC06_TDM.netMx <- cbind(NMFC06_TDM.netMx, NMFC06_TDM.clusterCoef, NMFC06_TDM.degreeCent$centralization,
                          NMFC06_TDM.netDensity, NMFC06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
NMFC06_SDg2 <- data.frame(NMFC06_SD)
NMFC06_SDg2 <- NMFC06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_SDg2$player1
player2vector <- NMFC06_SDg2$player2
NMFC06_SDg3 <- NMFC06_SDg2
NMFC06_SDg3$p1inp2vec <- is.element(NMFC06_SDg3$player1, player2vector)
NMFC06_SDg3$p2inp1vec <- is.element(NMFC06_SDg3$player2, player1vector)

addPlayer1 <- NMFC06_SDg3[ which(NMFC06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC06_SDg3[ which(NMFC06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_SDg2 <- rbind(NMFC06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
NMFC06_SDft <- ftable(NMFC06_SDg2$player1, NMFC06_SDg2$player2)
NMFC06_SDft2 <- as.matrix(NMFC06_SDft)
numRows <- nrow(NMFC06_SDft2)
numCols <- ncol(NMFC06_SDft2)
NMFC06_SDft3 <- NMFC06_SDft2[c(2:numRows) , c(2:numCols)]
NMFC06_SDTable <- graph.adjacency(NMFC06_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(NMFC06_SDTable, vertex.label = V(NMFC06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
NMFC06_SD.clusterCoef <- transitivity(NMFC06_SDTable, type="global") #cluster coefficient
NMFC06_SD.degreeCent <- centralization.degree(NMFC06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_SDftn <- as.network.matrix(NMFC06_SDft)
NMFC06_SD.netDensity <- network.density(NMFC06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_SD.entropy <- entropy(NMFC06_SDft) #entropy

NMFC06_SD.netMx <- cbind(NMFC06_SD.netMx, NMFC06_SD.clusterCoef, NMFC06_SD.degreeCent$centralization,
                         NMFC06_SD.netDensity, NMFC06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
NMFC06_TDg2 <- data.frame(NMFC06_TD)
NMFC06_TDg2 <- NMFC06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_TDg2$player1
player2vector <- NMFC06_TDg2$player2
NMFC06_TDg3 <- NMFC06_TDg2
NMFC06_TDg3$p1inp2vec <- is.element(NMFC06_TDg3$player1, player2vector)
NMFC06_TDg3$p2inp1vec <- is.element(NMFC06_TDg3$player2, player1vector)

addPlayer1 <- NMFC06_TDg3[ which(NMFC06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC06_TDg3[ which(NMFC06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_TDg2 <- rbind(NMFC06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
NMFC06_TDft <- ftable(NMFC06_TDg2$player1, NMFC06_TDg2$player2)
NMFC06_TDft2 <- as.matrix(NMFC06_TDft)
numRows <- nrow(NMFC06_TDft2)
numCols <- ncol(NMFC06_TDft2)
NMFC06_TDft3 <- NMFC06_TDft2[c(2:numRows) , c(2:numCols)]
NMFC06_TDTable <- graph.adjacency(NMFC06_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(NMFC06_TDTable, vertex.label = V(NMFC06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
NMFC06_TD.clusterCoef <- transitivity(NMFC06_TDTable, type="global") #cluster coefficient
NMFC06_TD.degreeCent <- centralization.degree(NMFC06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_TDftn <- as.network.matrix(NMFC06_TDft)
NMFC06_TD.netDensity <- network.density(NMFC06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_TD.entropy <- entropy(NMFC06_TDft) #entropy

NMFC06_TD.netMx <- cbind(NMFC06_TD.netMx, NMFC06_TD.clusterCoef, NMFC06_TD.degreeCent$centralization,
                         NMFC06_TD.netDensity, NMFC06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
NMFC06_QTg2 <- data.frame(NMFC06_QT)
NMFC06_QTg2 <- NMFC06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC06_QTg2$player1
player2vector <- NMFC06_QTg2$player2
NMFC06_QTg3 <- NMFC06_QTg2
NMFC06_QTg3$p1inp2vec <- is.element(NMFC06_QTg3$player1, player2vector)
NMFC06_QTg3$p2inp1vec <- is.element(NMFC06_QTg3$player2, player1vector)

addPlayer1 <- NMFC06_QTg3[ which(NMFC06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC06_QTg3[ which(NMFC06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC06_QTg2 <- rbind(NMFC06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
NMFC06_QTft <- ftable(NMFC06_QTg2$player1, NMFC06_QTg2$player2)
NMFC06_QTft2 <- as.matrix(NMFC06_QTft)
numRows <- nrow(NMFC06_QTft2)
numCols <- ncol(NMFC06_QTft2)
NMFC06_QTft3 <- NMFC06_QTft2[c(2:numRows) , c(2:numCols)]
NMFC06_QTTable <- graph.adjacency(NMFC06_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(NMFC06_QTTable, vertex.label = V(NMFC06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
NMFC06_QT.clusterCoef <- transitivity(NMFC06_QTTable, type="global") #cluster coefficient
NMFC06_QT.degreeCent <- centralization.degree(NMFC06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC06_QTftn <- as.network.matrix(NMFC06_QTft)
NMFC06_QT.netDensity <- network.density(NMFC06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC06_QT.entropy <- entropy(NMFC06_QTft) #entropy

NMFC06_QT.netMx <- cbind(NMFC06_QT.netMx, NMFC06_QT.clusterCoef, NMFC06_QT.degreeCent$centralization,
                         NMFC06_QT.netDensity, NMFC06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC06_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************

round = 6
teamName = "PORT"
KIoutcome = "Goal_F"
PORT06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
PORT06_Gg2 <- data.frame(PORT06_G)
PORT06_Gg2 <- PORT06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_Gg2$player1
player2vector <- PORT06_Gg2$player2
PORT06_Gg3 <- PORT06_Gg2
PORT06_Gg3$p1inp2vec <- is.element(PORT06_Gg3$player1, player2vector)
PORT06_Gg3$p2inp1vec <- is.element(PORT06_Gg3$player2, player1vector)

addPlayer1 <- PORT06_Gg3[ which(PORT06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT06_Gg3[ which(PORT06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_Gg2 <- rbind(PORT06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
PORT06_Gft <- ftable(PORT06_Gg2$player1, PORT06_Gg2$player2)
PORT06_Gft2 <- as.matrix(PORT06_Gft)
numRows <- nrow(PORT06_Gft2)
numCols <- ncol(PORT06_Gft2)
PORT06_Gft3 <- PORT06_Gft2[c(2:numRows) , c(2:numCols)]
PORT06_GTable <- graph.adjacency(PORT06_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(PORT06_GTable, vertex.label = V(PORT06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
PORT06_G.clusterCoef <- transitivity(PORT06_GTable, type="global") #cluster coefficient
PORT06_G.degreeCent <- centralization.degree(PORT06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_Gftn <- as.network.matrix(PORT06_Gft)
PORT06_G.netDensity <- network.density(PORT06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_G.entropy <- entropy(PORT06_Gft) #entropy

PORT06_G.netMx <- cbind(PORT06_G.netMx, PORT06_G.clusterCoef, PORT06_G.degreeCent$centralization,
                        PORT06_G.netDensity, PORT06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************
#NA

round = 6
teamName = "PORT"
KIoutcome = "Behind_F"
PORT06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
PORT06_Bg2 <- data.frame(PORT06_B)
PORT06_Bg2 <- PORT06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_Bg2$player1
player2vector <- PORT06_Bg2$player2
PORT06_Bg3 <- PORT06_Bg2
PORT06_Bg3$p1inp2vec <- is.element(PORT06_Bg3$player1, player2vector)
PORT06_Bg3$p2inp1vec <- is.element(PORT06_Bg3$player2, player1vector)

addPlayer1 <- PORT06_Bg3[ which(PORT06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT06_Bg3[ which(PORT06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_Bg2 <- rbind(PORT06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
PORT06_Bft <- ftable(PORT06_Bg2$player1, PORT06_Bg2$player2)
PORT06_Bft2 <- as.matrix(PORT06_Bft)
numRows <- nrow(PORT06_Bft2)
numCols <- ncol(PORT06_Bft2)
PORT06_Bft3 <- PORT06_Bft2[c(2:numRows) , c(2:numCols)]
PORT06_BTable <- graph.adjacency(PORT06_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(PORT06_BTable, vertex.label = V(PORT06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
PORT06_B.clusterCoef <- transitivity(PORT06_BTable, type="global") #cluster coefficient
PORT06_B.degreeCent <- centralization.degree(PORT06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_Bftn <- as.network.matrix(PORT06_Bft)
PORT06_B.netDensity <- network.density(PORT06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_B.entropy <- entropy(PORT06_Bft) #entropy

PORT06_B.netMx <- cbind(PORT06_B.netMx, PORT06_B.clusterCoef, PORT06_B.degreeCent$centralization,
                        PORT06_B.netDensity, PORT06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
PORT06_SFg2 <- data.frame(PORT06_SF)
PORT06_SFg2 <- PORT06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_SFg2$player1
player2vector <- PORT06_SFg2$player2
PORT06_SFg3 <- PORT06_SFg2
PORT06_SFg3$p1inp2vec <- is.element(PORT06_SFg3$player1, player2vector)
PORT06_SFg3$p2inp1vec <- is.element(PORT06_SFg3$player2, player1vector)

addPlayer1 <- PORT06_SFg3[ which(PORT06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT06_SFg3[ which(PORT06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_SFg2 <- rbind(PORT06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
PORT06_SFft <- ftable(PORT06_SFg2$player1, PORT06_SFg2$player2)
PORT06_SFft2 <- as.matrix(PORT06_SFft)
numRows <- nrow(PORT06_SFft2)
numCols <- ncol(PORT06_SFft2)
PORT06_SFft3 <- PORT06_SFft2[c(2:numRows) , c(2:numCols)]
PORT06_SFTable <- graph.adjacency(PORT06_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(PORT06_SFTable, vertex.label = V(PORT06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
PORT06_SF.clusterCoef <- transitivity(PORT06_SFTable, type="global") #cluster coefficient
PORT06_SF.degreeCent <- centralization.degree(PORT06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_SFftn <- as.network.matrix(PORT06_SFft)
PORT06_SF.netDensity <- network.density(PORT06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_SF.entropy <- entropy(PORT06_SFft) #entropy

PORT06_SF.netMx <- cbind(PORT06_SF.netMx, PORT06_SF.clusterCoef, PORT06_SF.degreeCent$centralization,
                         PORT06_SF.netDensity, PORT06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
PORT06_TFg2 <- data.frame(PORT06_TF)
PORT06_TFg2 <- PORT06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_TFg2$player1
player2vector <- PORT06_TFg2$player2
PORT06_TFg3 <- PORT06_TFg2
PORT06_TFg3$p1inp2vec <- is.element(PORT06_TFg3$player1, player2vector)
PORT06_TFg3$p2inp1vec <- is.element(PORT06_TFg3$player2, player1vector)

addPlayer1 <- PORT06_TFg3[ which(PORT06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- PORT06_TFg3[ which(PORT06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_TFg2 <- rbind(PORT06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
PORT06_TFft <- ftable(PORT06_TFg2$player1, PORT06_TFg2$player2)
PORT06_TFft2 <- as.matrix(PORT06_TFft)
numRows <- nrow(PORT06_TFft2)
numCols <- ncol(PORT06_TFft2)
PORT06_TFft3 <- PORT06_TFft2[c(2:numRows) , c(2:numCols)]
PORT06_TFTable <- graph.adjacency(PORT06_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(PORT06_TFTable, vertex.label = V(PORT06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
PORT06_TF.clusterCoef <- transitivity(PORT06_TFTable, type="global") #cluster coefficient
PORT06_TF.degreeCent <- centralization.degree(PORT06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_TFftn <- as.network.matrix(PORT06_TFft)
PORT06_TF.netDensity <- network.density(PORT06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_TF.entropy <- entropy(PORT06_TFft) #entropy

PORT06_TF.netMx <- cbind(PORT06_TF.netMx, PORT06_TF.clusterCoef, PORT06_TF.degreeCent$centralization,
                         PORT06_TF.netDensity, PORT06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
PORT06_SAMg2 <- data.frame(PORT06_SAM)
PORT06_SAMg2 <- PORT06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_SAMg2$player1
player2vector <- PORT06_SAMg2$player2
PORT06_SAMg3 <- PORT06_SAMg2
PORT06_SAMg3$p1inp2vec <- is.element(PORT06_SAMg3$player1, player2vector)
PORT06_SAMg3$p2inp1vec <- is.element(PORT06_SAMg3$player2, player1vector)

addPlayer1 <- PORT06_SAMg3[ which(PORT06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT06_SAMg3[ which(PORT06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_SAMg2 <- rbind(PORT06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
PORT06_SAMft <- ftable(PORT06_SAMg2$player1, PORT06_SAMg2$player2)
PORT06_SAMft2 <- as.matrix(PORT06_SAMft)
numRows <- nrow(PORT06_SAMft2)
numCols <- ncol(PORT06_SAMft2)
PORT06_SAMft3 <- PORT06_SAMft2[c(2:numRows) , c(2:numCols)]
PORT06_SAMTable <- graph.adjacency(PORT06_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(PORT06_SAMTable, vertex.label = V(PORT06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
PORT06_SAM.clusterCoef <- transitivity(PORT06_SAMTable, type="global") #cluster coefficient
PORT06_SAM.degreeCent <- centralization.degree(PORT06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_SAMftn <- as.network.matrix(PORT06_SAMft)
PORT06_SAM.netDensity <- network.density(PORT06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_SAM.entropy <- entropy(PORT06_SAMft) #entropy

PORT06_SAM.netMx <- cbind(PORT06_SAM.netMx, PORT06_SAM.clusterCoef, PORT06_SAM.degreeCent$centralization,
                          PORT06_SAM.netDensity, PORT06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************
#NA

round = 6
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
PORT06_TAMg2 <- data.frame(PORT06_TAM)
PORT06_TAMg2 <- PORT06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_TAMg2$player1
player2vector <- PORT06_TAMg2$player2
PORT06_TAMg3 <- PORT06_TAMg2
PORT06_TAMg3$p1inp2vec <- is.element(PORT06_TAMg3$player1, player2vector)
PORT06_TAMg3$p2inp1vec <- is.element(PORT06_TAMg3$player2, player1vector)

addPlayer1 <- PORT06_TAMg3[ which(PORT06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT06_TAMg2 <- rbind(PORT06_TAMg2, addPlayer1)

#ROUND 6, AM Turnover graph using weighted edges
PORT06_TAMft <- ftable(PORT06_TAMg2$player1, PORT06_TAMg2$player2)
PORT06_TAMft2 <- as.matrix(PORT06_TAMft)
numRows <- nrow(PORT06_TAMft2)
numCols <- ncol(PORT06_TAMft2)
PORT06_TAMft3 <- PORT06_TAMft2[c(2:numRows) , c(1:numCols)]
PORT06_TAMTable <- graph.adjacency(PORT06_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(PORT06_TAMTable, vertex.label = V(PORT06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
PORT06_TAM.clusterCoef <- transitivity(PORT06_TAMTable, type="global") #cluster coefficient
PORT06_TAM.degreeCent <- centralization.degree(PORT06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_TAMftn <- as.network.matrix(PORT06_TAMft)
PORT06_TAM.netDensity <- network.density(PORT06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_TAM.entropy <- entropy(PORT06_TAMft) #entropy

PORT06_TAM.netMx <- cbind(PORT06_TAM.netMx, PORT06_TAM.clusterCoef, PORT06_TAM.degreeCent$centralization,
                          PORT06_TAM.netDensity, PORT06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************
#NA

round = 6
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
PORT06_SDMg2 <- data.frame(PORT06_SDM)
PORT06_SDMg2 <- PORT06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_SDMg2$player1
player2vector <- PORT06_SDMg2$player2
PORT06_SDMg3 <- PORT06_SDMg2
PORT06_SDMg3$p1inp2vec <- is.element(PORT06_SDMg3$player1, player2vector)
PORT06_SDMg3$p2inp1vec <- is.element(PORT06_SDMg3$player2, player1vector)

addPlayer1 <- PORT06_SDMg3[ which(PORT06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT06_SDMg3[ which(PORT06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_SDMg2 <- rbind(PORT06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
PORT06_SDMft <- ftable(PORT06_SDMg2$player1, PORT06_SDMg2$player2)
PORT06_SDMft2 <- as.matrix(PORT06_SDMft)
numRows <- nrow(PORT06_SDMft2)
numCols <- ncol(PORT06_SDMft2)
PORT06_SDMft3 <- PORT06_SDMft2[c(2:numRows) , c(2:numCols)]
PORT06_SDMTable <- graph.adjacency(PORT06_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(PORT06_SDMTable, vertex.label = V(PORT06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
PORT06_SDM.clusterCoef <- transitivity(PORT06_SDMTable, type="global") #cluster coefficient
PORT06_SDM.degreeCent <- centralization.degree(PORT06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_SDMftn <- as.network.matrix(PORT06_SDMft)
PORT06_SDM.netDensity <- network.density(PORT06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_SDM.entropy <- entropy(PORT06_SDMft) #entropy

PORT06_SDM.netMx <- cbind(PORT06_SDM.netMx, PORT06_SDM.clusterCoef, PORT06_SDM.degreeCent$centralization,
                          PORT06_SDM.netDensity, PORT06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
PORT06_TDMg2 <- data.frame(PORT06_TDM)
PORT06_TDMg2 <- PORT06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_TDMg2$player1
player2vector <- PORT06_TDMg2$player2
PORT06_TDMg3 <- PORT06_TDMg2
PORT06_TDMg3$p1inp2vec <- is.element(PORT06_TDMg3$player1, player2vector)
PORT06_TDMg3$p2inp1vec <- is.element(PORT06_TDMg3$player2, player1vector)

addPlayer1 <- PORT06_TDMg3[ which(PORT06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT06_TDMg3[ which(PORT06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_TDMg2 <- rbind(PORT06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
PORT06_TDMft <- ftable(PORT06_TDMg2$player1, PORT06_TDMg2$player2)
PORT06_TDMft2 <- as.matrix(PORT06_TDMft)
numRows <- nrow(PORT06_TDMft2)
numCols <- ncol(PORT06_TDMft2)
PORT06_TDMft3 <- PORT06_TDMft2[c(2:numRows) , c(2:numCols)]
PORT06_TDMTable <- graph.adjacency(PORT06_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(PORT06_TDMTable, vertex.label = V(PORT06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
PORT06_TDM.clusterCoef <- transitivity(PORT06_TDMTable, type="global") #cluster coefficient
PORT06_TDM.degreeCent <- centralization.degree(PORT06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_TDMftn <- as.network.matrix(PORT06_TDMft)
PORT06_TDM.netDensity <- network.density(PORT06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_TDM.entropy <- entropy(PORT06_TDMft) #entropy

PORT06_TDM.netMx <- cbind(PORT06_TDM.netMx, PORT06_TDM.clusterCoef, PORT06_TDM.degreeCent$centralization,
                          PORT06_TDM.netDensity, PORT06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
PORT06_SDg2 <- data.frame(PORT06_SD)
PORT06_SDg2 <- PORT06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_SDg2$player1
player2vector <- PORT06_SDg2$player2
PORT06_SDg3 <- PORT06_SDg2
PORT06_SDg3$p1inp2vec <- is.element(PORT06_SDg3$player1, player2vector)
PORT06_SDg3$p2inp1vec <- is.element(PORT06_SDg3$player2, player1vector)

addPlayer1 <- PORT06_SDg3[ which(PORT06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT06_SDg3[ which(PORT06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_SDg2 <- rbind(PORT06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
PORT06_SDft <- ftable(PORT06_SDg2$player1, PORT06_SDg2$player2)
PORT06_SDft2 <- as.matrix(PORT06_SDft)
numRows <- nrow(PORT06_SDft2)
numCols <- ncol(PORT06_SDft2)
PORT06_SDft3 <- PORT06_SDft2[c(2:numRows) , c(2:numCols)]
PORT06_SDTable <- graph.adjacency(PORT06_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(PORT06_SDTable, vertex.label = V(PORT06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
PORT06_SD.clusterCoef <- transitivity(PORT06_SDTable, type="global") #cluster coefficient
PORT06_SD.degreeCent <- centralization.degree(PORT06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_SDftn <- as.network.matrix(PORT06_SDft)
PORT06_SD.netDensity <- network.density(PORT06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_SD.entropy <- entropy(PORT06_SDft) #entropy

PORT06_SD.netMx <- cbind(PORT06_SD.netMx, PORT06_SD.clusterCoef, PORT06_SD.degreeCent$centralization,
                         PORT06_SD.netDensity, PORT06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************

round = 6
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
PORT06_TDg2 <- data.frame(PORT06_TD)
PORT06_TDg2 <- PORT06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_TDg2$player1
player2vector <- PORT06_TDg2$player2
PORT06_TDg3 <- PORT06_TDg2
PORT06_TDg3$p1inp2vec <- is.element(PORT06_TDg3$player1, player2vector)
PORT06_TDg3$p2inp1vec <- is.element(PORT06_TDg3$player2, player1vector)

addPlayer1 <- PORT06_TDg3[ which(PORT06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT06_TDg3[ which(PORT06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_TDg2 <- rbind(PORT06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
PORT06_TDft <- ftable(PORT06_TDg2$player1, PORT06_TDg2$player2)
PORT06_TDft2 <- as.matrix(PORT06_TDft)
numRows <- nrow(PORT06_TDft2)
numCols <- ncol(PORT06_TDft2)
PORT06_TDft3 <- PORT06_TDft2[c(2:numRows) , c(2:numCols)]
PORT06_TDTable <- graph.adjacency(PORT06_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(PORT06_TDTable, vertex.label = V(PORT06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
PORT06_TD.clusterCoef <- transitivity(PORT06_TDTable, type="global") #cluster coefficient
PORT06_TD.degreeCent <- centralization.degree(PORT06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_TDftn <- as.network.matrix(PORT06_TDft)
PORT06_TD.netDensity <- network.density(PORT06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_TD.entropy <- entropy(PORT06_TDft) #entropy

PORT06_TD.netMx <- cbind(PORT06_TD.netMx, PORT06_TD.clusterCoef, PORT06_TD.degreeCent$centralization,
                         PORT06_TD.netDensity, PORT06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
PORT06_QTg2 <- data.frame(PORT06_QT)
PORT06_QTg2 <- PORT06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT06_QTg2$player1
player2vector <- PORT06_QTg2$player2
PORT06_QTg3 <- PORT06_QTg2
PORT06_QTg3$p1inp2vec <- is.element(PORT06_QTg3$player1, player2vector)
PORT06_QTg3$p2inp1vec <- is.element(PORT06_QTg3$player2, player1vector)

addPlayer1 <- PORT06_QTg3[ which(PORT06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT06_QTg3[ which(PORT06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT06_QTg2 <- rbind(PORT06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
PORT06_QTft <- ftable(PORT06_QTg2$player1, PORT06_QTg2$player2)
PORT06_QTft2 <- as.matrix(PORT06_QTft)
numRows <- nrow(PORT06_QTft2)
numCols <- ncol(PORT06_QTft2)
PORT06_QTft3 <- PORT06_QTft2[c(2:numRows) , c(2:numCols)]
PORT06_QTTable <- graph.adjacency(PORT06_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(PORT06_QTTable, vertex.label = V(PORT06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
PORT06_QT.clusterCoef <- transitivity(PORT06_QTTable, type="global") #cluster coefficient
PORT06_QT.degreeCent <- centralization.degree(PORT06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT06_QTftn <- as.network.matrix(PORT06_QTft)
PORT06_QT.netDensity <- network.density(PORT06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT06_QT.entropy <- entropy(PORT06_QTft) #entropy

PORT06_QT.netMx <- cbind(PORT06_QT.netMx, PORT06_QT.clusterCoef, PORT06_QT.degreeCent$centralization,
                         PORT06_QT.netDensity, PORT06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT06_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************

round = 6
teamName = "RICH"
KIoutcome = "Goal_F"
RICH06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
RICH06_Gg2 <- data.frame(RICH06_G)
RICH06_Gg2 <- RICH06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_Gg2$player1
player2vector <- RICH06_Gg2$player2
RICH06_Gg3 <- RICH06_Gg2
RICH06_Gg3$p1inp2vec <- is.element(RICH06_Gg3$player1, player2vector)
RICH06_Gg3$p2inp1vec <- is.element(RICH06_Gg3$player2, player1vector)

addPlayer1 <- RICH06_Gg3[ which(RICH06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_Gg3[ which(RICH06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_Gg2 <- rbind(RICH06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
RICH06_Gft <- ftable(RICH06_Gg2$player1, RICH06_Gg2$player2)
RICH06_Gft2 <- as.matrix(RICH06_Gft)
numRows <- nrow(RICH06_Gft2)
numCols <- ncol(RICH06_Gft2)
RICH06_Gft3 <- RICH06_Gft2[c(2:numRows) , c(2:numCols)]
RICH06_GTable <- graph.adjacency(RICH06_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(RICH06_GTable, vertex.label = V(RICH06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
RICH06_G.clusterCoef <- transitivity(RICH06_GTable, type="global") #cluster coefficient
RICH06_G.degreeCent <- centralization.degree(RICH06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_Gftn <- as.network.matrix(RICH06_Gft)
RICH06_G.netDensity <- network.density(RICH06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_G.entropy <- entropy(RICH06_Gft) #entropy

RICH06_G.netMx <- cbind(RICH06_G.netMx, RICH06_G.clusterCoef, RICH06_G.degreeCent$centralization,
                        RICH06_G.netDensity, RICH06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "RICH"
KIoutcome = "Behind_F"
RICH06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
RICH06_Bg2 <- data.frame(RICH06_B)
RICH06_Bg2 <- RICH06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_Bg2$player1
player2vector <- RICH06_Bg2$player2
RICH06_Bg3 <- RICH06_Bg2
RICH06_Bg3$p1inp2vec <- is.element(RICH06_Bg3$player1, player2vector)
RICH06_Bg3$p2inp1vec <- is.element(RICH06_Bg3$player2, player1vector)

addPlayer1 <- RICH06_Bg3[ which(RICH06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_Bg3[ which(RICH06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_Bg2 <- rbind(RICH06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
RICH06_Bft <- ftable(RICH06_Bg2$player1, RICH06_Bg2$player2)
RICH06_Bft2 <- as.matrix(RICH06_Bft)
numRows <- nrow(RICH06_Bft2)
numCols <- ncol(RICH06_Bft2)
RICH06_Bft3 <- RICH06_Bft2[c(2:numRows) , c(2:numCols)]
RICH06_BTable <- graph.adjacency(RICH06_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(RICH06_BTable, vertex.label = V(RICH06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
RICH06_B.clusterCoef <- transitivity(RICH06_BTable, type="global") #cluster coefficient
RICH06_B.degreeCent <- centralization.degree(RICH06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_Bftn <- as.network.matrix(RICH06_Bft)
RICH06_B.netDensity <- network.density(RICH06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_B.entropy <- entropy(RICH06_Bft) #entropy

RICH06_B.netMx <- cbind(RICH06_B.netMx, RICH06_B.clusterCoef, RICH06_B.degreeCent$centralization,
                        RICH06_B.netDensity, RICH06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
RICH06_SFg2 <- data.frame(RICH06_SF)
RICH06_SFg2 <- RICH06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_SFg2$player1
player2vector <- RICH06_SFg2$player2
RICH06_SFg3 <- RICH06_SFg2
RICH06_SFg3$p1inp2vec <- is.element(RICH06_SFg3$player1, player2vector)
RICH06_SFg3$p2inp1vec <- is.element(RICH06_SFg3$player2, player1vector)

addPlayer1 <- RICH06_SFg3[ which(RICH06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_SFg3[ which(RICH06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_SFg2 <- rbind(RICH06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
RICH06_SFft <- ftable(RICH06_SFg2$player1, RICH06_SFg2$player2)
RICH06_SFft2 <- as.matrix(RICH06_SFft)
numRows <- nrow(RICH06_SFft2)
numCols <- ncol(RICH06_SFft2)
RICH06_SFft3 <- RICH06_SFft2[c(2:numRows) , c(2:numCols)]
RICH06_SFTable <- graph.adjacency(RICH06_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(RICH06_SFTable, vertex.label = V(RICH06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
RICH06_SF.clusterCoef <- transitivity(RICH06_SFTable, type="global") #cluster coefficient
RICH06_SF.degreeCent <- centralization.degree(RICH06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_SFftn <- as.network.matrix(RICH06_SFft)
RICH06_SF.netDensity <- network.density(RICH06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_SF.entropy <- entropy(RICH06_SFft) #entropy

RICH06_SF.netMx <- cbind(RICH06_SF.netMx, RICH06_SF.clusterCoef, RICH06_SF.degreeCent$centralization,
                         RICH06_SF.netDensity, RICH06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
RICH06_TFg2 <- data.frame(RICH06_TF)
RICH06_TFg2 <- RICH06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_TFg2$player1
player2vector <- RICH06_TFg2$player2
RICH06_TFg3 <- RICH06_TFg2
RICH06_TFg3$p1inp2vec <- is.element(RICH06_TFg3$player1, player2vector)
RICH06_TFg3$p2inp1vec <- is.element(RICH06_TFg3$player2, player1vector)

addPlayer1 <- RICH06_TFg3[ which(RICH06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_TFg3[ which(RICH06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_TFg2 <- rbind(RICH06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
RICH06_TFft <- ftable(RICH06_TFg2$player1, RICH06_TFg2$player2)
RICH06_TFft2 <- as.matrix(RICH06_TFft)
numRows <- nrow(RICH06_TFft2)
numCols <- ncol(RICH06_TFft2)
RICH06_TFft3 <- RICH06_TFft2[c(2:numRows) , c(2:numCols)]
RICH06_TFTable <- graph.adjacency(RICH06_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(RICH06_TFTable, vertex.label = V(RICH06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
RICH06_TF.clusterCoef <- transitivity(RICH06_TFTable, type="global") #cluster coefficient
RICH06_TF.degreeCent <- centralization.degree(RICH06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_TFftn <- as.network.matrix(RICH06_TFft)
RICH06_TF.netDensity <- network.density(RICH06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_TF.entropy <- entropy(RICH06_TFft) #entropy

RICH06_TF.netMx <- cbind(RICH06_TF.netMx, RICH06_TF.clusterCoef, RICH06_TF.degreeCent$centralization,
                         RICH06_TF.netDensity, RICH06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
RICH06_SAMg2 <- data.frame(RICH06_SAM)
RICH06_SAMg2 <- RICH06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_SAMg2$player1
player2vector <- RICH06_SAMg2$player2
RICH06_SAMg3 <- RICH06_SAMg2
RICH06_SAMg3$p1inp2vec <- is.element(RICH06_SAMg3$player1, player2vector)
RICH06_SAMg3$p2inp1vec <- is.element(RICH06_SAMg3$player2, player1vector)

addPlayer1 <- RICH06_SAMg3[ which(RICH06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_SAMg3[ which(RICH06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_SAMg2 <- rbind(RICH06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
RICH06_SAMft <- ftable(RICH06_SAMg2$player1, RICH06_SAMg2$player2)
RICH06_SAMft2 <- as.matrix(RICH06_SAMft)
numRows <- nrow(RICH06_SAMft2)
numCols <- ncol(RICH06_SAMft2)
RICH06_SAMft3 <- RICH06_SAMft2[c(2:numRows) , c(2:numCols)]
RICH06_SAMTable <- graph.adjacency(RICH06_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(RICH06_SAMTable, vertex.label = V(RICH06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
RICH06_SAM.clusterCoef <- transitivity(RICH06_SAMTable, type="global") #cluster coefficient
RICH06_SAM.degreeCent <- centralization.degree(RICH06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_SAMftn <- as.network.matrix(RICH06_SAMft)
RICH06_SAM.netDensity <- network.density(RICH06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_SAM.entropy <- entropy(RICH06_SAMft) #entropy

RICH06_SAM.netMx <- cbind(RICH06_SAM.netMx, RICH06_SAM.clusterCoef, RICH06_SAM.degreeCent$centralization,
                          RICH06_SAM.netDensity, RICH06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************

round = 6
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
RICH06_TAMg2 <- data.frame(RICH06_TAM)
RICH06_TAMg2 <- RICH06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_TAMg2$player1
player2vector <- RICH06_TAMg2$player2
RICH06_TAMg3 <- RICH06_TAMg2
RICH06_TAMg3$p1inp2vec <- is.element(RICH06_TAMg3$player1, player2vector)
RICH06_TAMg3$p2inp1vec <- is.element(RICH06_TAMg3$player2, player1vector)

addPlayer1 <- RICH06_TAMg3[ which(RICH06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_TAMg3[ which(RICH06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_TAMg2 <- rbind(RICH06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
RICH06_TAMft <- ftable(RICH06_TAMg2$player1, RICH06_TAMg2$player2)
RICH06_TAMft2 <- as.matrix(RICH06_TAMft)
numRows <- nrow(RICH06_TAMft2)
numCols <- ncol(RICH06_TAMft2)
RICH06_TAMft3 <- RICH06_TAMft2[c(2:numRows) , c(2:numCols)]
RICH06_TAMTable <- graph.adjacency(RICH06_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(RICH06_TAMTable, vertex.label = V(RICH06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
RICH06_TAM.clusterCoef <- transitivity(RICH06_TAMTable, type="global") #cluster coefficient
RICH06_TAM.degreeCent <- centralization.degree(RICH06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_TAMftn <- as.network.matrix(RICH06_TAMft)
RICH06_TAM.netDensity <- network.density(RICH06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_TAM.entropy <- entropy(RICH06_TAMft) #entropy

RICH06_TAM.netMx <- cbind(RICH06_TAM.netMx, RICH06_TAM.clusterCoef, RICH06_TAM.degreeCent$centralization,
                          RICH06_TAM.netDensity, RICH06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************
#NA

round = 6
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
RICH06_SDMg2 <- data.frame(RICH06_SDM)
RICH06_SDMg2 <- RICH06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_SDMg2$player1
player2vector <- RICH06_SDMg2$player2
RICH06_SDMg3 <- RICH06_SDMg2
RICH06_SDMg3$p1inp2vec <- is.element(RICH06_SDMg3$player1, player2vector)
RICH06_SDMg3$p2inp1vec <- is.element(RICH06_SDMg3$player2, player1vector)

addPlayer1 <- RICH06_SDMg3[ which(RICH06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_SDMg3[ which(RICH06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_SDMg2 <- rbind(RICH06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
RICH06_SDMft <- ftable(RICH06_SDMg2$player1, RICH06_SDMg2$player2)
RICH06_SDMft2 <- as.matrix(RICH06_SDMft)
numRows <- nrow(RICH06_SDMft2)
numCols <- ncol(RICH06_SDMft2)
RICH06_SDMft3 <- RICH06_SDMft2[c(2:numRows) , c(2:numCols)]
RICH06_SDMTable <- graph.adjacency(RICH06_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(RICH06_SDMTable, vertex.label = V(RICH06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
RICH06_SDM.clusterCoef <- transitivity(RICH06_SDMTable, type="global") #cluster coefficient
RICH06_SDM.degreeCent <- centralization.degree(RICH06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_SDMftn <- as.network.matrix(RICH06_SDMft)
RICH06_SDM.netDensity <- network.density(RICH06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_SDM.entropy <- entropy(RICH06_SDMft) #entropy

RICH06_SDM.netMx <- cbind(RICH06_SDM.netMx, RICH06_SDM.clusterCoef, RICH06_SDM.degreeCent$centralization,
                          RICH06_SDM.netDensity, RICH06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
RICH06_TDMg2 <- data.frame(RICH06_TDM)
RICH06_TDMg2 <- RICH06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_TDMg2$player1
player2vector <- RICH06_TDMg2$player2
RICH06_TDMg3 <- RICH06_TDMg2
RICH06_TDMg3$p1inp2vec <- is.element(RICH06_TDMg3$player1, player2vector)
RICH06_TDMg3$p2inp1vec <- is.element(RICH06_TDMg3$player2, player1vector)

addPlayer1 <- RICH06_TDMg3[ which(RICH06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_TDMg3[ which(RICH06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_TDMg2 <- rbind(RICH06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
RICH06_TDMft <- ftable(RICH06_TDMg2$player1, RICH06_TDMg2$player2)
RICH06_TDMft2 <- as.matrix(RICH06_TDMft)
numRows <- nrow(RICH06_TDMft2)
numCols <- ncol(RICH06_TDMft2)
RICH06_TDMft3 <- RICH06_TDMft2[c(2:numRows) , c(2:numCols)]
RICH06_TDMTable <- graph.adjacency(RICH06_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(RICH06_TDMTable, vertex.label = V(RICH06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
RICH06_TDM.clusterCoef <- transitivity(RICH06_TDMTable, type="global") #cluster coefficient
RICH06_TDM.degreeCent <- centralization.degree(RICH06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_TDMftn <- as.network.matrix(RICH06_TDMft)
RICH06_TDM.netDensity <- network.density(RICH06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_TDM.entropy <- entropy(RICH06_TDMft) #entropy

RICH06_TDM.netMx <- cbind(RICH06_TDM.netMx, RICH06_TDM.clusterCoef, RICH06_TDM.degreeCent$centralization,
                          RICH06_TDM.netDensity, RICH06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
RICH06_SDg2 <- data.frame(RICH06_SD)
RICH06_SDg2 <- RICH06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_SDg2$player1
player2vector <- RICH06_SDg2$player2
RICH06_SDg3 <- RICH06_SDg2
RICH06_SDg3$p1inp2vec <- is.element(RICH06_SDg3$player1, player2vector)
RICH06_SDg3$p2inp1vec <- is.element(RICH06_SDg3$player2, player1vector)

addPlayer1 <- RICH06_SDg3[ which(RICH06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_SDg3[ which(RICH06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_SDg2 <- rbind(RICH06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
RICH06_SDft <- ftable(RICH06_SDg2$player1, RICH06_SDg2$player2)
RICH06_SDft2 <- as.matrix(RICH06_SDft)
numRows <- nrow(RICH06_SDft2)
numCols <- ncol(RICH06_SDft2)
RICH06_SDft3 <- RICH06_SDft2[c(2:numRows) , c(2:numCols)]
RICH06_SDTable <- graph.adjacency(RICH06_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(RICH06_SDTable, vertex.label = V(RICH06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
RICH06_SD.clusterCoef <- transitivity(RICH06_SDTable, type="global") #cluster coefficient
RICH06_SD.degreeCent <- centralization.degree(RICH06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_SDftn <- as.network.matrix(RICH06_SDft)
RICH06_SD.netDensity <- network.density(RICH06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_SD.entropy <- entropy(RICH06_SDft) #entropy

RICH06_SD.netMx <- cbind(RICH06_SD.netMx, RICH06_SD.clusterCoef, RICH06_SD.degreeCent$centralization,
                         RICH06_SD.netDensity, RICH06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
RICH06_TDg2 <- data.frame(RICH06_TD)
RICH06_TDg2 <- RICH06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_TDg2$player1
player2vector <- RICH06_TDg2$player2
RICH06_TDg3 <- RICH06_TDg2
RICH06_TDg3$p1inp2vec <- is.element(RICH06_TDg3$player1, player2vector)
RICH06_TDg3$p2inp1vec <- is.element(RICH06_TDg3$player2, player1vector)

addPlayer1 <- RICH06_TDg3[ which(RICH06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_TDg3[ which(RICH06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_TDg2 <- rbind(RICH06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
RICH06_TDft <- ftable(RICH06_TDg2$player1, RICH06_TDg2$player2)
RICH06_TDft2 <- as.matrix(RICH06_TDft)
numRows <- nrow(RICH06_TDft2)
numCols <- ncol(RICH06_TDft2)
RICH06_TDft3 <- RICH06_TDft2[c(2:numRows) , c(2:numCols)]
RICH06_TDTable <- graph.adjacency(RICH06_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(RICH06_TDTable, vertex.label = V(RICH06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
RICH06_TD.clusterCoef <- transitivity(RICH06_TDTable, type="global") #cluster coefficient
RICH06_TD.degreeCent <- centralization.degree(RICH06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_TDftn <- as.network.matrix(RICH06_TDft)
RICH06_TD.netDensity <- network.density(RICH06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_TD.entropy <- entropy(RICH06_TDft) #entropy

RICH06_TD.netMx <- cbind(RICH06_TD.netMx, RICH06_TD.clusterCoef, RICH06_TD.degreeCent$centralization,
                         RICH06_TD.netDensity, RICH06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
RICH06_QTg2 <- data.frame(RICH06_QT)
RICH06_QTg2 <- RICH06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH06_QTg2$player1
player2vector <- RICH06_QTg2$player2
RICH06_QTg3 <- RICH06_QTg2
RICH06_QTg3$p1inp2vec <- is.element(RICH06_QTg3$player1, player2vector)
RICH06_QTg3$p2inp1vec <- is.element(RICH06_QTg3$player2, player1vector)

addPlayer1 <- RICH06_QTg3[ which(RICH06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH06_QTg3[ which(RICH06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH06_QTg2 <- rbind(RICH06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
RICH06_QTft <- ftable(RICH06_QTg2$player1, RICH06_QTg2$player2)
RICH06_QTft2 <- as.matrix(RICH06_QTft)
numRows <- nrow(RICH06_QTft2)
numCols <- ncol(RICH06_QTft2)
RICH06_QTft3 <- RICH06_QTft2[c(2:numRows) , c(2:numCols)]
RICH06_QTTable <- graph.adjacency(RICH06_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(RICH06_QTTable, vertex.label = V(RICH06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
RICH06_QT.clusterCoef <- transitivity(RICH06_QTTable, type="global") #cluster coefficient
RICH06_QT.degreeCent <- centralization.degree(RICH06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH06_QTftn <- as.network.matrix(RICH06_QTft)
RICH06_QT.netDensity <- network.density(RICH06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH06_QT.entropy <- entropy(RICH06_QTft) #entropy

RICH06_QT.netMx <- cbind(RICH06_QT.netMx, RICH06_QT.clusterCoef, RICH06_QT.degreeCent$centralization,
                         RICH06_QT.netDensity, RICH06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH06_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "STK"
KIoutcome = "Goal_F"
STK06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
STK06_Gg2 <- data.frame(STK06_G)
STK06_Gg2 <- STK06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_Gg2$player1
player2vector <- STK06_Gg2$player2
STK06_Gg3 <- STK06_Gg2
STK06_Gg3$p1inp2vec <- is.element(STK06_Gg3$player1, player2vector)
STK06_Gg3$p2inp1vec <- is.element(STK06_Gg3$player2, player1vector)

addPlayer1 <- STK06_Gg3[ which(STK06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_Gg3[ which(STK06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_Gg2 <- rbind(STK06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
STK06_Gft <- ftable(STK06_Gg2$player1, STK06_Gg2$player2)
STK06_Gft2 <- as.matrix(STK06_Gft)
numRows <- nrow(STK06_Gft2)
numCols <- ncol(STK06_Gft2)
STK06_Gft3 <- STK06_Gft2[c(2:numRows) , c(2:numCols)]
STK06_GTable <- graph.adjacency(STK06_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(STK06_GTable, vertex.label = V(STK06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
STK06_G.clusterCoef <- transitivity(STK06_GTable, type="global") #cluster coefficient
STK06_G.degreeCent <- centralization.degree(STK06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_Gftn <- as.network.matrix(STK06_Gft)
STK06_G.netDensity <- network.density(STK06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_G.entropy <- entropy(STK06_Gft) #entropy

STK06_G.netMx <- cbind(STK06_G.netMx, STK06_G.clusterCoef, STK06_G.degreeCent$centralization,
                       STK06_G.netDensity, STK06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "STK"
KIoutcome = "Behind_F"
STK06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
STK06_Bg2 <- data.frame(STK06_B)
STK06_Bg2 <- STK06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_Bg2$player1
player2vector <- STK06_Bg2$player2
STK06_Bg3 <- STK06_Bg2
STK06_Bg3$p1inp2vec <- is.element(STK06_Bg3$player1, player2vector)
STK06_Bg3$p2inp1vec <- is.element(STK06_Bg3$player2, player1vector)

addPlayer1 <- STK06_Bg3[ which(STK06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_Bg3[ which(STK06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_Bg2 <- rbind(STK06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
STK06_Bft <- ftable(STK06_Bg2$player1, STK06_Bg2$player2)
STK06_Bft2 <- as.matrix(STK06_Bft)
numRows <- nrow(STK06_Bft2)
numCols <- ncol(STK06_Bft2)
STK06_Bft3 <- STK06_Bft2[c(2:numRows) , c(2:numCols)]
STK06_BTable <- graph.adjacency(STK06_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(STK06_BTable, vertex.label = V(STK06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
STK06_B.clusterCoef <- transitivity(STK06_BTable, type="global") #cluster coefficient
STK06_B.degreeCent <- centralization.degree(STK06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_Bftn <- as.network.matrix(STK06_Bft)
STK06_B.netDensity <- network.density(STK06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_B.entropy <- entropy(STK06_Bft) #entropy

STK06_B.netMx <- cbind(STK06_B.netMx, STK06_B.clusterCoef, STK06_B.degreeCent$centralization,
                       STK06_B.netDensity, STK06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "STK"
KIoutcome = "Stoppage_F"
STK06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
STK06_SFg2 <- data.frame(STK06_SF)
STK06_SFg2 <- STK06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_SFg2$player1
player2vector <- STK06_SFg2$player2
STK06_SFg3 <- STK06_SFg2
STK06_SFg3$p1inp2vec <- is.element(STK06_SFg3$player1, player2vector)
STK06_SFg3$p2inp1vec <- is.element(STK06_SFg3$player2, player1vector)

addPlayer1 <- STK06_SFg3[ which(STK06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_SFg3[ which(STK06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_SFg2 <- rbind(STK06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
STK06_SFft <- ftable(STK06_SFg2$player1, STK06_SFg2$player2)
STK06_SFft2 <- as.matrix(STK06_SFft)
numRows <- nrow(STK06_SFft2)
numCols <- ncol(STK06_SFft2)
STK06_SFft3 <- STK06_SFft2[c(2:numRows) , c(2:numCols)]
STK06_SFTable <- graph.adjacency(STK06_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(STK06_SFTable, vertex.label = V(STK06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
STK06_SF.clusterCoef <- transitivity(STK06_SFTable, type="global") #cluster coefficient
STK06_SF.degreeCent <- centralization.degree(STK06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_SFftn <- as.network.matrix(STK06_SFft)
STK06_SF.netDensity <- network.density(STK06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_SF.entropy <- entropy(STK06_SFft) #entropy

STK06_SF.netMx <- cbind(STK06_SF.netMx, STK06_SF.clusterCoef, STK06_SF.degreeCent$centralization,
                        STK06_SF.netDensity, STK06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************
#NA

round = 6
teamName = "STK"
KIoutcome = "Turnover_F"
STK06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
STK06_TFg2 <- data.frame(STK06_TF)
STK06_TFg2 <- STK06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_TFg2$player1
player2vector <- STK06_TFg2$player2
STK06_TFg3 <- STK06_TFg2
STK06_TFg3$p1inp2vec <- is.element(STK06_TFg3$player1, player2vector)
STK06_TFg3$p2inp1vec <- is.element(STK06_TFg3$player2, player1vector)

addPlayer1 <- STK06_TFg3[ which(STK06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_TFg3[ which(STK06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_TFg2 <- rbind(STK06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
STK06_TFft <- ftable(STK06_TFg2$player1, STK06_TFg2$player2)
STK06_TFft2 <- as.matrix(STK06_TFft)
numRows <- nrow(STK06_TFft2)
numCols <- ncol(STK06_TFft2)
STK06_TFft3 <- STK06_TFft2[c(2:numRows) , c(2:numCols)]
STK06_TFTable <- graph.adjacency(STK06_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(STK06_TFTable, vertex.label = V(STK06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
STK06_TF.clusterCoef <- transitivity(STK06_TFTable, type="global") #cluster coefficient
STK06_TF.degreeCent <- centralization.degree(STK06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_TFftn <- as.network.matrix(STK06_TFft)
STK06_TF.netDensity <- network.density(STK06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_TF.entropy <- entropy(STK06_TFft) #entropy

STK06_TF.netMx <- cbind(STK06_TF.netMx, STK06_TF.clusterCoef, STK06_TF.degreeCent$centralization,
                        STK06_TF.netDensity, STK06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
STK06_SAMg2 <- data.frame(STK06_SAM)
STK06_SAMg2 <- STK06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_SAMg2$player1
player2vector <- STK06_SAMg2$player2
STK06_SAMg3 <- STK06_SAMg2
STK06_SAMg3$p1inp2vec <- is.element(STK06_SAMg3$player1, player2vector)
STK06_SAMg3$p2inp1vec <- is.element(STK06_SAMg3$player2, player1vector)

addPlayer1 <- STK06_SAMg3[ which(STK06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_SAMg3[ which(STK06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_SAMg2 <- rbind(STK06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
STK06_SAMft <- ftable(STK06_SAMg2$player1, STK06_SAMg2$player2)
STK06_SAMft2 <- as.matrix(STK06_SAMft)
numRows <- nrow(STK06_SAMft2)
numCols <- ncol(STK06_SAMft2)
STK06_SAMft3 <- STK06_SAMft2[c(2:numRows) , c(2:numCols)]
STK06_SAMTable <- graph.adjacency(STK06_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(STK06_SAMTable, vertex.label = V(STK06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
STK06_SAM.clusterCoef <- transitivity(STK06_SAMTable, type="global") #cluster coefficient
STK06_SAM.degreeCent <- centralization.degree(STK06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_SAMftn <- as.network.matrix(STK06_SAMft)
STK06_SAM.netDensity <- network.density(STK06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_SAM.entropy <- entropy(STK06_SAMft) #entropy

STK06_SAM.netMx <- cbind(STK06_SAM.netMx, STK06_SAM.clusterCoef, STK06_SAM.degreeCent$centralization,
                         STK06_SAM.netDensity, STK06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************
#NA

round = 6
teamName = "STK"
KIoutcome = "Turnover_AM"
STK06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
STK06_TAMg2 <- data.frame(STK06_TAM)
STK06_TAMg2 <- STK06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_TAMg2$player1
player2vector <- STK06_TAMg2$player2
STK06_TAMg3 <- STK06_TAMg2
STK06_TAMg3$p1inp2vec <- is.element(STK06_TAMg3$player1, player2vector)
STK06_TAMg3$p2inp1vec <- is.element(STK06_TAMg3$player2, player1vector)

addPlayer1 <- STK06_TAMg3[ which(STK06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_TAMg3[ which(STK06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_TAMg2 <- rbind(STK06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
STK06_TAMft <- ftable(STK06_TAMg2$player1, STK06_TAMg2$player2)
STK06_TAMft2 <- as.matrix(STK06_TAMft)
numRows <- nrow(STK06_TAMft2)
numCols <- ncol(STK06_TAMft2)
STK06_TAMft3 <- STK06_TAMft2[c(1:numRows) , c(1:numCols)]
STK06_TAMTable <- graph.adjacency(STK06_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(STK06_TAMTable, vertex.label = V(STK06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
STK06_TAM.clusterCoef <- transitivity(STK06_TAMTable, type="global") #cluster coefficient
STK06_TAM.degreeCent <- centralization.degree(STK06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_TAMftn <- as.network.matrix(STK06_TAMft)
STK06_TAM.netDensity <- network.density(STK06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_TAM.entropy <- entropy(STK06_TAMft) #entropy

STK06_TAM.netMx <- cbind(STK06_TAM.netMx, STK06_TAM.clusterCoef, STK06_TAM.degreeCent$centralization,
                         STK06_TAM.netDensity, STK06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
STK06_SDMg2 <- data.frame(STK06_SDM)
STK06_SDMg2 <- STK06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_SDMg2$player1
player2vector <- STK06_SDMg2$player2
STK06_SDMg3 <- STK06_SDMg2
STK06_SDMg3$p1inp2vec <- is.element(STK06_SDMg3$player1, player2vector)
STK06_SDMg3$p2inp1vec <- is.element(STK06_SDMg3$player2, player1vector)

addPlayer1 <- STK06_SDMg3[ which(STK06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_SDMg3[ which(STK06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_SDMg2 <- rbind(STK06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
STK06_SDMft <- ftable(STK06_SDMg2$player1, STK06_SDMg2$player2)
STK06_SDMft2 <- as.matrix(STK06_SDMft)
numRows <- nrow(STK06_SDMft2)
numCols <- ncol(STK06_SDMft2)
STK06_SDMft3 <- STK06_SDMft2[c(2:numRows) , c(2:numCols)]
STK06_SDMTable <- graph.adjacency(STK06_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(STK06_SDMTable, vertex.label = V(STK06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
STK06_SDM.clusterCoef <- transitivity(STK06_SDMTable, type="global") #cluster coefficient
STK06_SDM.degreeCent <- centralization.degree(STK06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_SDMftn <- as.network.matrix(STK06_SDMft)
STK06_SDM.netDensity <- network.density(STK06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_SDM.entropy <- entropy(STK06_SDMft) #entropy

STK06_SDM.netMx <- cbind(STK06_SDM.netMx, STK06_SDM.clusterCoef, STK06_SDM.degreeCent$centralization,
                         STK06_SDM.netDensity, STK06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "STK"
KIoutcome = "Turnover_DM"
STK06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
STK06_TDMg2 <- data.frame(STK06_TDM)
STK06_TDMg2 <- STK06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_TDMg2$player1
player2vector <- STK06_TDMg2$player2
STK06_TDMg3 <- STK06_TDMg2
STK06_TDMg3$p1inp2vec <- is.element(STK06_TDMg3$player1, player2vector)
STK06_TDMg3$p2inp1vec <- is.element(STK06_TDMg3$player2, player1vector)

addPlayer1 <- STK06_TDMg3[ which(STK06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_TDMg3[ which(STK06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_TDMg2 <- rbind(STK06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
STK06_TDMft <- ftable(STK06_TDMg2$player1, STK06_TDMg2$player2)
STK06_TDMft2 <- as.matrix(STK06_TDMft)
numRows <- nrow(STK06_TDMft2)
numCols <- ncol(STK06_TDMft2)
STK06_TDMft3 <- STK06_TDMft2[c(2:numRows) , c(2:numCols)]
STK06_TDMTable <- graph.adjacency(STK06_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(STK06_TDMTable, vertex.label = V(STK06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
STK06_TDM.clusterCoef <- transitivity(STK06_TDMTable, type="global") #cluster coefficient
STK06_TDM.degreeCent <- centralization.degree(STK06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_TDMftn <- as.network.matrix(STK06_TDMft)
STK06_TDM.netDensity <- network.density(STK06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_TDM.entropy <- entropy(STK06_TDMft) #entropy

STK06_TDM.netMx <- cbind(STK06_TDM.netMx, STK06_TDM.clusterCoef, STK06_TDM.degreeCent$centralization,
                         STK06_TDM.netDensity, STK06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "STK"
KIoutcome = "Stoppage_D"
STK06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
STK06_SDg2 <- data.frame(STK06_SD)
STK06_SDg2 <- STK06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_SDg2$player1
player2vector <- STK06_SDg2$player2
STK06_SDg3 <- STK06_SDg2
STK06_SDg3$p1inp2vec <- is.element(STK06_SDg3$player1, player2vector)
STK06_SDg3$p2inp1vec <- is.element(STK06_SDg3$player2, player1vector)

addPlayer1 <- STK06_SDg3[ which(STK06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_SDg3[ which(STK06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_SDg2 <- rbind(STK06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
STK06_SDft <- ftable(STK06_SDg2$player1, STK06_SDg2$player2)
STK06_SDft2 <- as.matrix(STK06_SDft)
numRows <- nrow(STK06_SDft2)
numCols <- ncol(STK06_SDft2)
STK06_SDft3 <- STK06_SDft2[c(2:numRows) , c(2:numCols)]
STK06_SDTable <- graph.adjacency(STK06_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(STK06_SDTable, vertex.label = V(STK06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
STK06_SD.clusterCoef <- transitivity(STK06_SDTable, type="global") #cluster coefficient
STK06_SD.degreeCent <- centralization.degree(STK06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_SDftn <- as.network.matrix(STK06_SDft)
STK06_SD.netDensity <- network.density(STK06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_SD.entropy <- entropy(STK06_SDft) #entropy

STK06_SD.netMx <- cbind(STK06_SD.netMx, STK06_SD.clusterCoef, STK06_SD.degreeCent$centralization,
                        STK06_SD.netDensity, STK06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "STK"
KIoutcome = "Turnover_D"
STK06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
STK06_TDg2 <- data.frame(STK06_TD)
STK06_TDg2 <- STK06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_TDg2$player1
player2vector <- STK06_TDg2$player2
STK06_TDg3 <- STK06_TDg2
STK06_TDg3$p1inp2vec <- is.element(STK06_TDg3$player1, player2vector)
STK06_TDg3$p2inp1vec <- is.element(STK06_TDg3$player2, player1vector)

addPlayer1 <- STK06_TDg3[ which(STK06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_TDg3[ which(STK06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_TDg2 <- rbind(STK06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
STK06_TDft <- ftable(STK06_TDg2$player1, STK06_TDg2$player2)
STK06_TDft2 <- as.matrix(STK06_TDft)
numRows <- nrow(STK06_TDft2)
numCols <- ncol(STK06_TDft2)
STK06_TDft3 <- STK06_TDft2[c(2:numRows) , c(2:numCols)]
STK06_TDTable <- graph.adjacency(STK06_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(STK06_TDTable, vertex.label = V(STK06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
STK06_TD.clusterCoef <- transitivity(STK06_TDTable, type="global") #cluster coefficient
STK06_TD.degreeCent <- centralization.degree(STK06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_TDftn <- as.network.matrix(STK06_TDft)
STK06_TD.netDensity <- network.density(STK06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_TD.entropy <- entropy(STK06_TDft) #entropy

STK06_TD.netMx <- cbind(STK06_TD.netMx, STK06_TD.clusterCoef, STK06_TD.degreeCent$centralization,
                        STK06_TD.netDensity, STK06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
STK06_QTg2 <- data.frame(STK06_QT)
STK06_QTg2 <- STK06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK06_QTg2$player1
player2vector <- STK06_QTg2$player2
STK06_QTg3 <- STK06_QTg2
STK06_QTg3$p1inp2vec <- is.element(STK06_QTg3$player1, player2vector)
STK06_QTg3$p2inp1vec <- is.element(STK06_QTg3$player2, player1vector)

addPlayer1 <- STK06_QTg3[ which(STK06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK06_QTg3[ which(STK06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK06_QTg2 <- rbind(STK06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
STK06_QTft <- ftable(STK06_QTg2$player1, STK06_QTg2$player2)
STK06_QTft2 <- as.matrix(STK06_QTft)
numRows <- nrow(STK06_QTft2)
numCols <- ncol(STK06_QTft2)
STK06_QTft3 <- STK06_QTft2[c(2:numRows) , c(2:numCols)]
STK06_QTTable <- graph.adjacency(STK06_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(STK06_QTTable, vertex.label = V(STK06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
STK06_QT.clusterCoef <- transitivity(STK06_QTTable, type="global") #cluster coefficient
STK06_QT.degreeCent <- centralization.degree(STK06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK06_QTftn <- as.network.matrix(STK06_QTft)
STK06_QT.netDensity <- network.density(STK06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK06_QT.entropy <- entropy(STK06_QTft) #entropy

STK06_QT.netMx <- cbind(STK06_QT.netMx, STK06_QT.clusterCoef, STK06_QT.degreeCent$centralization,
                        STK06_QT.netDensity, STK06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK06_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "SYD"
KIoutcome = "Goal_F"
SYD06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
SYD06_Gg2 <- data.frame(SYD06_G)
SYD06_Gg2 <- SYD06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_Gg2$player1
player2vector <- SYD06_Gg2$player2
SYD06_Gg3 <- SYD06_Gg2
SYD06_Gg3$p1inp2vec <- is.element(SYD06_Gg3$player1, player2vector)
SYD06_Gg3$p2inp1vec <- is.element(SYD06_Gg3$player2, player1vector)

addPlayer1 <- SYD06_Gg3[ which(SYD06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_Gg3[ which(SYD06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_Gg2 <- rbind(SYD06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
SYD06_Gft <- ftable(SYD06_Gg2$player1, SYD06_Gg2$player2)
SYD06_Gft2 <- as.matrix(SYD06_Gft)
numRows <- nrow(SYD06_Gft2)
numCols <- ncol(SYD06_Gft2)
SYD06_Gft3 <- SYD06_Gft2[c(2:numRows) , c(2:numCols)]
SYD06_GTable <- graph.adjacency(SYD06_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(SYD06_GTable, vertex.label = V(SYD06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
SYD06_G.clusterCoef <- transitivity(SYD06_GTable, type="global") #cluster coefficient
SYD06_G.degreeCent <- centralization.degree(SYD06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_Gftn <- as.network.matrix(SYD06_Gft)
SYD06_G.netDensity <- network.density(SYD06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_G.entropy <- entropy(SYD06_Gft) #entropy

SYD06_G.netMx <- cbind(SYD06_G.netMx, SYD06_G.clusterCoef, SYD06_G.degreeCent$centralization,
                       SYD06_G.netDensity, SYD06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "SYD"
KIoutcome = "Behind_F"
SYD06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
SYD06_Bg2 <- data.frame(SYD06_B)
SYD06_Bg2 <- SYD06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_Bg2$player1
player2vector <- SYD06_Bg2$player2
SYD06_Bg3 <- SYD06_Bg2
SYD06_Bg3$p1inp2vec <- is.element(SYD06_Bg3$player1, player2vector)
SYD06_Bg3$p2inp1vec <- is.element(SYD06_Bg3$player2, player1vector)

addPlayer1 <- SYD06_Bg3[ which(SYD06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_Bg3[ which(SYD06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_Bg2 <- rbind(SYD06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
SYD06_Bft <- ftable(SYD06_Bg2$player1, SYD06_Bg2$player2)
SYD06_Bft2 <- as.matrix(SYD06_Bft)
numRows <- nrow(SYD06_Bft2)
numCols <- ncol(SYD06_Bft2)
SYD06_Bft3 <- SYD06_Bft2[c(2:numRows) , c(2:numCols)]
SYD06_BTable <- graph.adjacency(SYD06_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(SYD06_BTable, vertex.label = V(SYD06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
SYD06_B.clusterCoef <- transitivity(SYD06_BTable, type="global") #cluster coefficient
SYD06_B.degreeCent <- centralization.degree(SYD06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_Bftn <- as.network.matrix(SYD06_Bft)
SYD06_B.netDensity <- network.density(SYD06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_B.entropy <- entropy(SYD06_Bft) #entropy

SYD06_B.netMx <- cbind(SYD06_B.netMx, SYD06_B.clusterCoef, SYD06_B.degreeCent$centralization,
                       SYD06_B.netDensity, SYD06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************

round = 6
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
SYD06_SFg2 <- data.frame(SYD06_SF)
SYD06_SFg2 <- SYD06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_SFg2$player1
player2vector <- SYD06_SFg2$player2
SYD06_SFg3 <- SYD06_SFg2
SYD06_SFg3$p1inp2vec <- is.element(SYD06_SFg3$player1, player2vector)
SYD06_SFg3$p2inp1vec <- is.element(SYD06_SFg3$player2, player1vector)

addPlayer1 <- SYD06_SFg3[ which(SYD06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_SFg3[ which(SYD06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_SFg2 <- rbind(SYD06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
SYD06_SFft <- ftable(SYD06_SFg2$player1, SYD06_SFg2$player2)
SYD06_SFft2 <- as.matrix(SYD06_SFft)
numRows <- nrow(SYD06_SFft2)
numCols <- ncol(SYD06_SFft2)
SYD06_SFft3 <- SYD06_SFft2[c(2:numRows) , c(2:numCols)]
SYD06_SFTable <- graph.adjacency(SYD06_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(SYD06_SFTable, vertex.label = V(SYD06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
SYD06_SF.clusterCoef <- transitivity(SYD06_SFTable, type="global") #cluster coefficient
SYD06_SF.degreeCent <- centralization.degree(SYD06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_SFftn <- as.network.matrix(SYD06_SFft)
SYD06_SF.netDensity <- network.density(SYD06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_SF.entropy <- entropy(SYD06_SFft) #entropy

SYD06_SF.netMx <- cbind(SYD06_SF.netMx, SYD06_SF.clusterCoef, SYD06_SF.degreeCent$centralization,
                        SYD06_SF.netDensity, SYD06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
SYD06_TFg2 <- data.frame(SYD06_TF)
SYD06_TFg2 <- SYD06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_TFg2$player1
player2vector <- SYD06_TFg2$player2
SYD06_TFg3 <- SYD06_TFg2
SYD06_TFg3$p1inp2vec <- is.element(SYD06_TFg3$player1, player2vector)
SYD06_TFg3$p2inp1vec <- is.element(SYD06_TFg3$player2, player1vector)

addPlayer1 <- SYD06_TFg3[ which(SYD06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_TFg3[ which(SYD06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_TFg2 <- rbind(SYD06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
SYD06_TFft <- ftable(SYD06_TFg2$player1, SYD06_TFg2$player2)
SYD06_TFft2 <- as.matrix(SYD06_TFft)
numRows <- nrow(SYD06_TFft2)
numCols <- ncol(SYD06_TFft2)
SYD06_TFft3 <- SYD06_TFft2[c(2:numRows) , c(2:numCols)]
SYD06_TFTable <- graph.adjacency(SYD06_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(SYD06_TFTable, vertex.label = V(SYD06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
SYD06_TF.clusterCoef <- transitivity(SYD06_TFTable, type="global") #cluster coefficient
SYD06_TF.degreeCent <- centralization.degree(SYD06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_TFftn <- as.network.matrix(SYD06_TFft)
SYD06_TF.netDensity <- network.density(SYD06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_TF.entropy <- entropy(SYD06_TFft) #entropy

SYD06_TF.netMx <- cbind(SYD06_TF.netMx, SYD06_TF.clusterCoef, SYD06_TF.degreeCent$centralization,
                        SYD06_TF.netDensity, SYD06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
SYD06_SAMg2 <- data.frame(SYD06_SAM)
SYD06_SAMg2 <- SYD06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_SAMg2$player1
player2vector <- SYD06_SAMg2$player2
SYD06_SAMg3 <- SYD06_SAMg2
SYD06_SAMg3$p1inp2vec <- is.element(SYD06_SAMg3$player1, player2vector)
SYD06_SAMg3$p2inp1vec <- is.element(SYD06_SAMg3$player2, player1vector)

addPlayer1 <- SYD06_SAMg3[ which(SYD06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_SAMg3[ which(SYD06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_SAMg2 <- rbind(SYD06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
SYD06_SAMft <- ftable(SYD06_SAMg2$player1, SYD06_SAMg2$player2)
SYD06_SAMft2 <- as.matrix(SYD06_SAMft)
numRows <- nrow(SYD06_SAMft2)
numCols <- ncol(SYD06_SAMft2)
SYD06_SAMft3 <- SYD06_SAMft2[c(2:numRows) , c(2:numCols)]
SYD06_SAMTable <- graph.adjacency(SYD06_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(SYD06_SAMTable, vertex.label = V(SYD06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
SYD06_SAM.clusterCoef <- transitivity(SYD06_SAMTable, type="global") #cluster coefficient
SYD06_SAM.degreeCent <- centralization.degree(SYD06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_SAMftn <- as.network.matrix(SYD06_SAMft)
SYD06_SAM.netDensity <- network.density(SYD06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_SAM.entropy <- entropy(SYD06_SAMft) #entropy

SYD06_SAM.netMx <- cbind(SYD06_SAM.netMx, SYD06_SAM.clusterCoef, SYD06_SAM.degreeCent$centralization,
                         SYD06_SAM.netDensity, SYD06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************

round = 6
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
SYD06_TAMg2 <- data.frame(SYD06_TAM)
SYD06_TAMg2 <- SYD06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_TAMg2$player1
player2vector <- SYD06_TAMg2$player2
SYD06_TAMg3 <- SYD06_TAMg2
SYD06_TAMg3$p1inp2vec <- is.element(SYD06_TAMg3$player1, player2vector)
SYD06_TAMg3$p2inp1vec <- is.element(SYD06_TAMg3$player2, player1vector)

addPlayer1 <- SYD06_TAMg3[ which(SYD06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_TAMg3[ which(SYD06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_TAMg2 <- rbind(SYD06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
SYD06_TAMft <- ftable(SYD06_TAMg2$player1, SYD06_TAMg2$player2)
SYD06_TAMft2 <- as.matrix(SYD06_TAMft)
numRows <- nrow(SYD06_TAMft2)
numCols <- ncol(SYD06_TAMft2)
SYD06_TAMft3 <- SYD06_TAMft2[c(2:numRows) , c(2:numCols)]
SYD06_TAMTable <- graph.adjacency(SYD06_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(SYD06_TAMTable, vertex.label = V(SYD06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
SYD06_TAM.clusterCoef <- transitivity(SYD06_TAMTable, type="global") #cluster coefficient
SYD06_TAM.degreeCent <- centralization.degree(SYD06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_TAMftn <- as.network.matrix(SYD06_TAMft)
SYD06_TAM.netDensity <- network.density(SYD06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_TAM.entropy <- entropy(SYD06_TAMft) #entropy

SYD06_TAM.netMx <- cbind(SYD06_TAM.netMx, SYD06_TAM.clusterCoef, SYD06_TAM.degreeCent$centralization,
                         SYD06_TAM.netDensity, SYD06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
SYD06_SDMg2 <- data.frame(SYD06_SDM)
SYD06_SDMg2 <- SYD06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_SDMg2$player1
player2vector <- SYD06_SDMg2$player2
SYD06_SDMg3 <- SYD06_SDMg2
SYD06_SDMg3$p1inp2vec <- is.element(SYD06_SDMg3$player1, player2vector)
SYD06_SDMg3$p2inp1vec <- is.element(SYD06_SDMg3$player2, player1vector)

addPlayer1 <- SYD06_SDMg3[ which(SYD06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_SDMg3[ which(SYD06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_SDMg2 <- rbind(SYD06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
SYD06_SDMft <- ftable(SYD06_SDMg2$player1, SYD06_SDMg2$player2)
SYD06_SDMft2 <- as.matrix(SYD06_SDMft)
numRows <- nrow(SYD06_SDMft2)
numCols <- ncol(SYD06_SDMft2)
SYD06_SDMft3 <- SYD06_SDMft2[c(2:numRows) , c(2:numCols)]
SYD06_SDMTable <- graph.adjacency(SYD06_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(SYD06_SDMTable, vertex.label = V(SYD06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
SYD06_SDM.clusterCoef <- transitivity(SYD06_SDMTable, type="global") #cluster coefficient
SYD06_SDM.degreeCent <- centralization.degree(SYD06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_SDMftn <- as.network.matrix(SYD06_SDMft)
SYD06_SDM.netDensity <- network.density(SYD06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_SDM.entropy <- entropy(SYD06_SDMft) #entropy

SYD06_SDM.netMx <- cbind(SYD06_SDM.netMx, SYD06_SDM.clusterCoef, SYD06_SDM.degreeCent$centralization,
                         SYD06_SDM.netDensity, SYD06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************
#NA

round = 6
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
SYD06_TDMg2 <- data.frame(SYD06_TDM)
SYD06_TDMg2 <- SYD06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_TDMg2$player1
player2vector <- SYD06_TDMg2$player2
SYD06_TDMg3 <- SYD06_TDMg2
SYD06_TDMg3$p1inp2vec <- is.element(SYD06_TDMg3$player1, player2vector)
SYD06_TDMg3$p2inp1vec <- is.element(SYD06_TDMg3$player2, player1vector)

addPlayer1 <- SYD06_TDMg3[ which(SYD06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_TDMg3[ which(SYD06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_TDMg2 <- rbind(SYD06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
SYD06_TDMft <- ftable(SYD06_TDMg2$player1, SYD06_TDMg2$player2)
SYD06_TDMft2 <- as.matrix(SYD06_TDMft)
numRows <- nrow(SYD06_TDMft2)
numCols <- ncol(SYD06_TDMft2)
SYD06_TDMft3 <- SYD06_TDMft2[c(2:numRows) , c(2:numCols)]
SYD06_TDMTable <- graph.adjacency(SYD06_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(SYD06_TDMTable, vertex.label = V(SYD06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
SYD06_TDM.clusterCoef <- transitivity(SYD06_TDMTable, type="global") #cluster coefficient
SYD06_TDM.degreeCent <- centralization.degree(SYD06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_TDMftn <- as.network.matrix(SYD06_TDMft)
SYD06_TDM.netDensity <- network.density(SYD06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_TDM.entropy <- entropy(SYD06_TDMft) #entropy

SYD06_TDM.netMx <- cbind(SYD06_TDM.netMx, SYD06_TDM.clusterCoef, SYD06_TDM.degreeCent$centralization,
                         SYD06_TDM.netDensity, SYD06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
SYD06_SDg2 <- data.frame(SYD06_SD)
SYD06_SDg2 <- SYD06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_SDg2$player1
player2vector <- SYD06_SDg2$player2
SYD06_SDg3 <- SYD06_SDg2
SYD06_SDg3$p1inp2vec <- is.element(SYD06_SDg3$player1, player2vector)
SYD06_SDg3$p2inp1vec <- is.element(SYD06_SDg3$player2, player1vector)

addPlayer1 <- SYD06_SDg3[ which(SYD06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_SDg3[ which(SYD06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_SDg2 <- rbind(SYD06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
SYD06_SDft <- ftable(SYD06_SDg2$player1, SYD06_SDg2$player2)
SYD06_SDft2 <- as.matrix(SYD06_SDft)
numRows <- nrow(SYD06_SDft2)
numCols <- ncol(SYD06_SDft2)
SYD06_SDft3 <- SYD06_SDft2[c(2:numRows) , c(2:numCols)]
SYD06_SDTable <- graph.adjacency(SYD06_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(SYD06_SDTable, vertex.label = V(SYD06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
SYD06_SD.clusterCoef <- transitivity(SYD06_SDTable, type="global") #cluster coefficient
SYD06_SD.degreeCent <- centralization.degree(SYD06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_SDftn <- as.network.matrix(SYD06_SDft)
SYD06_SD.netDensity <- network.density(SYD06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_SD.entropy <- entropy(SYD06_SDft) #entropy

SYD06_SD.netMx <- cbind(SYD06_SD.netMx, SYD06_SD.clusterCoef, SYD06_SD.degreeCent$centralization,
                        SYD06_SD.netDensity, SYD06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
SYD06_TDg2 <- data.frame(SYD06_TD)
SYD06_TDg2 <- SYD06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_TDg2$player1
player2vector <- SYD06_TDg2$player2
SYD06_TDg3 <- SYD06_TDg2
SYD06_TDg3$p1inp2vec <- is.element(SYD06_TDg3$player1, player2vector)
SYD06_TDg3$p2inp1vec <- is.element(SYD06_TDg3$player2, player1vector)

addPlayer1 <- SYD06_TDg3[ which(SYD06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_TDg3[ which(SYD06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_TDg2 <- rbind(SYD06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
SYD06_TDft <- ftable(SYD06_TDg2$player1, SYD06_TDg2$player2)
SYD06_TDft2 <- as.matrix(SYD06_TDft)
numRows <- nrow(SYD06_TDft2)
numCols <- ncol(SYD06_TDft2)
SYD06_TDft3 <- SYD06_TDft2[c(2:numRows) , c(2:numCols)]
SYD06_TDTable <- graph.adjacency(SYD06_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(SYD06_TDTable, vertex.label = V(SYD06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
SYD06_TD.clusterCoef <- transitivity(SYD06_TDTable, type="global") #cluster coefficient
SYD06_TD.degreeCent <- centralization.degree(SYD06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_TDftn <- as.network.matrix(SYD06_TDft)
SYD06_TD.netDensity <- network.density(SYD06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_TD.entropy <- entropy(SYD06_TDft) #entropy

SYD06_TD.netMx <- cbind(SYD06_TD.netMx, SYD06_TD.clusterCoef, SYD06_TD.degreeCent$centralization,
                        SYD06_TD.netDensity, SYD06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
SYD06_QTg2 <- data.frame(SYD06_QT)
SYD06_QTg2 <- SYD06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD06_QTg2$player1
player2vector <- SYD06_QTg2$player2
SYD06_QTg3 <- SYD06_QTg2
SYD06_QTg3$p1inp2vec <- is.element(SYD06_QTg3$player1, player2vector)
SYD06_QTg3$p2inp1vec <- is.element(SYD06_QTg3$player2, player1vector)

addPlayer1 <- SYD06_QTg3[ which(SYD06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD06_QTg3[ which(SYD06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD06_QTg2 <- rbind(SYD06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
SYD06_QTft <- ftable(SYD06_QTg2$player1, SYD06_QTg2$player2)
SYD06_QTft2 <- as.matrix(SYD06_QTft)
numRows <- nrow(SYD06_QTft2)
numCols <- ncol(SYD06_QTft2)
SYD06_QTft3 <- SYD06_QTft2[c(2:numRows) , c(2:numCols)]
SYD06_QTTable <- graph.adjacency(SYD06_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(SYD06_QTTable, vertex.label = V(SYD06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
SYD06_QT.clusterCoef <- transitivity(SYD06_QTTable, type="global") #cluster coefficient
SYD06_QT.degreeCent <- centralization.degree(SYD06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD06_QTftn <- as.network.matrix(SYD06_QTft)
SYD06_QT.netDensity <- network.density(SYD06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD06_QT.entropy <- entropy(SYD06_QTft) #entropy

SYD06_QT.netMx <- cbind(SYD06_QT.netMx, SYD06_QT.clusterCoef, SYD06_QT.degreeCent$centralization,
                        SYD06_QT.netDensity, SYD06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD06_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA

round = 6
teamName = "WB"
KIoutcome = "Goal_F"
WB06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
WB06_Gg2 <- data.frame(WB06_G)
WB06_Gg2 <- WB06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_Gg2$player1
player2vector <- WB06_Gg2$player2
WB06_Gg3 <- WB06_Gg2
WB06_Gg3$p1inp2vec <- is.element(WB06_Gg3$player1, player2vector)
WB06_Gg3$p2inp1vec <- is.element(WB06_Gg3$player2, player1vector)

addPlayer1 <- WB06_Gg3[ which(WB06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_Gg3[ which(WB06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_Gg2 <- rbind(WB06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
WB06_Gft <- ftable(WB06_Gg2$player1, WB06_Gg2$player2)
WB06_Gft2 <- as.matrix(WB06_Gft)
numRows <- nrow(WB06_Gft2)
numCols <- ncol(WB06_Gft2)
WB06_Gft3 <- WB06_Gft2[c(2:numRows) , c(2:numCols)]
WB06_GTable <- graph.adjacency(WB06_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(WB06_GTable, vertex.label = V(WB06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
WB06_G.clusterCoef <- transitivity(WB06_GTable, type="global") #cluster coefficient
WB06_G.degreeCent <- centralization.degree(WB06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_Gftn <- as.network.matrix(WB06_Gft)
WB06_G.netDensity <- network.density(WB06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_G.entropy <- entropy(WB06_Gft) #entropy

WB06_G.netMx <- cbind(WB06_G.netMx, WB06_G.clusterCoef, WB06_G.degreeCent$centralization,
                      WB06_G.netDensity, WB06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************

round = 6
teamName = "WB"
KIoutcome = "Behind_F"
WB06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
WB06_Bg2 <- data.frame(WB06_B)
WB06_Bg2 <- WB06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_Bg2$player1
player2vector <- WB06_Bg2$player2
WB06_Bg3 <- WB06_Bg2
WB06_Bg3$p1inp2vec <- is.element(WB06_Bg3$player1, player2vector)
WB06_Bg3$p2inp1vec <- is.element(WB06_Bg3$player2, player1vector)

addPlayer1 <- WB06_Bg3[ which(WB06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_Bg3[ which(WB06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_Bg2 <- rbind(WB06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
WB06_Bft <- ftable(WB06_Bg2$player1, WB06_Bg2$player2)
WB06_Bft2 <- as.matrix(WB06_Bft)
numRows <- nrow(WB06_Bft2)
numCols <- ncol(WB06_Bft2)
WB06_Bft3 <- WB06_Bft2[c(2:numRows) , c(2:numCols)]
WB06_BTable <- graph.adjacency(WB06_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(WB06_BTable, vertex.label = V(WB06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
WB06_B.clusterCoef <- transitivity(WB06_BTable, type="global") #cluster coefficient
WB06_B.degreeCent <- centralization.degree(WB06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_Bftn <- as.network.matrix(WB06_Bft)
WB06_B.netDensity <- network.density(WB06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_B.entropy <- entropy(WB06_Bft) #entropy

WB06_B.netMx <- cbind(WB06_B.netMx, WB06_B.clusterCoef, WB06_B.degreeCent$centralization,
                      WB06_B.netDensity, WB06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "WB"
KIoutcome = "Stoppage_F"
WB06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
WB06_SFg2 <- data.frame(WB06_SF)
WB06_SFg2 <- WB06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_SFg2$player1
player2vector <- WB06_SFg2$player2
WB06_SFg3 <- WB06_SFg2
WB06_SFg3$p1inp2vec <- is.element(WB06_SFg3$player1, player2vector)
WB06_SFg3$p2inp1vec <- is.element(WB06_SFg3$player2, player1vector)

addPlayer1 <- WB06_SFg3[ which(WB06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_SFg3[ which(WB06_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_SFg2 <- rbind(WB06_SFg2, addPlayers)

#ROUND 6, FWD Stoppage graph using weighted edges
WB06_SFft <- ftable(WB06_SFg2$player1, WB06_SFg2$player2)
WB06_SFft2 <- as.matrix(WB06_SFft)
numRows <- nrow(WB06_SFft2)
numCols <- ncol(WB06_SFft2)
WB06_SFft3 <- WB06_SFft2[c(2:numRows) , c(2:numCols)]
WB06_SFTable <- graph.adjacency(WB06_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(WB06_SFTable, vertex.label = V(WB06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
WB06_SF.clusterCoef <- transitivity(WB06_SFTable, type="global") #cluster coefficient
WB06_SF.degreeCent <- centralization.degree(WB06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_SFftn <- as.network.matrix(WB06_SFft)
WB06_SF.netDensity <- network.density(WB06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_SF.entropy <- entropy(WB06_SFft) #entropy

WB06_SF.netMx <- cbind(WB06_SF.netMx, WB06_SF.clusterCoef, WB06_SF.degreeCent$centralization,
                       WB06_SF.netDensity, WB06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "WB"
KIoutcome = "Turnover_F"
WB06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
WB06_TFg2 <- data.frame(WB06_TF)
WB06_TFg2 <- WB06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_TFg2$player1
player2vector <- WB06_TFg2$player2
WB06_TFg3 <- WB06_TFg2
WB06_TFg3$p1inp2vec <- is.element(WB06_TFg3$player1, player2vector)
WB06_TFg3$p2inp1vec <- is.element(WB06_TFg3$player2, player1vector)

addPlayer1 <- WB06_TFg3[ which(WB06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_TFg3[ which(WB06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_TFg2 <- rbind(WB06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
WB06_TFft <- ftable(WB06_TFg2$player1, WB06_TFg2$player2)
WB06_TFft2 <- as.matrix(WB06_TFft)
numRows <- nrow(WB06_TFft2)
numCols <- ncol(WB06_TFft2)
WB06_TFft3 <- WB06_TFft2[c(2:numRows) , c(2:numCols)]
WB06_TFTable <- graph.adjacency(WB06_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(WB06_TFTable, vertex.label = V(WB06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
WB06_TF.clusterCoef <- transitivity(WB06_TFTable, type="global") #cluster coefficient
WB06_TF.degreeCent <- centralization.degree(WB06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_TFftn <- as.network.matrix(WB06_TFft)
WB06_TF.netDensity <- network.density(WB06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_TF.entropy <- entropy(WB06_TFft) #entropy

WB06_TF.netMx <- cbind(WB06_TF.netMx, WB06_TF.clusterCoef, WB06_TF.degreeCent$centralization,
                       WB06_TF.netDensity, WB06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************

round = 6
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
WB06_SAMg2 <- data.frame(WB06_SAM)
WB06_SAMg2 <- WB06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_SAMg2$player1
player2vector <- WB06_SAMg2$player2
WB06_SAMg3 <- WB06_SAMg2
WB06_SAMg3$p1inp2vec <- is.element(WB06_SAMg3$player1, player2vector)
WB06_SAMg3$p2inp1vec <- is.element(WB06_SAMg3$player2, player1vector)

addPlayer1 <- WB06_SAMg3[ which(WB06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_SAMg3[ which(WB06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_SAMg2 <- rbind(WB06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
WB06_SAMft <- ftable(WB06_SAMg2$player1, WB06_SAMg2$player2)
WB06_SAMft2 <- as.matrix(WB06_SAMft)
numRows <- nrow(WB06_SAMft2)
numCols <- ncol(WB06_SAMft2)
WB06_SAMft3 <- WB06_SAMft2[c(2:numRows) , c(2:numCols)]
WB06_SAMTable <- graph.adjacency(WB06_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(WB06_SAMTable, vertex.label = V(WB06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
WB06_SAM.clusterCoef <- transitivity(WB06_SAMTable, type="global") #cluster coefficient
WB06_SAM.degreeCent <- centralization.degree(WB06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_SAMftn <- as.network.matrix(WB06_SAMft)
WB06_SAM.netDensity <- network.density(WB06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_SAM.entropy <- entropy(WB06_SAMft) #entropy

WB06_SAM.netMx <- cbind(WB06_SAM.netMx, WB06_SAM.clusterCoef, WB06_SAM.degreeCent$centralization,
                        WB06_SAM.netDensity, WB06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************
#NA

round = 6
teamName = "WB"
KIoutcome = "Turnover_AM"
WB06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
WB06_TAMg2 <- data.frame(WB06_TAM)
WB06_TAMg2 <- WB06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_TAMg2$player1
player2vector <- WB06_TAMg2$player2
WB06_TAMg3 <- WB06_TAMg2
WB06_TAMg3$p1inp2vec <- is.element(WB06_TAMg3$player1, player2vector)
WB06_TAMg3$p2inp1vec <- is.element(WB06_TAMg3$player2, player1vector)

addPlayer1 <- WB06_TAMg3[ which(WB06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_TAMg3[ which(WB06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_TAMg2 <- rbind(WB06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
WB06_TAMft <- ftable(WB06_TAMg2$player1, WB06_TAMg2$player2)
WB06_TAMft2 <- as.matrix(WB06_TAMft)
numRows <- nrow(WB06_TAMft2)
numCols <- ncol(WB06_TAMft2)
WB06_TAMft3 <- WB06_TAMft2[c(2:numRows) , c(2:numCols)]
WB06_TAMTable <- graph.adjacency(WB06_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(WB06_TAMTable, vertex.label = V(WB06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
WB06_TAM.clusterCoef <- transitivity(WB06_TAMTable, type="global") #cluster coefficient
WB06_TAM.degreeCent <- centralization.degree(WB06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_TAMftn <- as.network.matrix(WB06_TAMft)
WB06_TAM.netDensity <- network.density(WB06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_TAM.entropy <- entropy(WB06_TAMft) #entropy

WB06_TAM.netMx <- cbind(WB06_TAM.netMx, WB06_TAM.clusterCoef, WB06_TAM.degreeCent$centralization,
                        WB06_TAM.netDensity, WB06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************

round = 6
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
WB06_SDMg2 <- data.frame(WB06_SDM)
WB06_SDMg2 <- WB06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_SDMg2$player1
player2vector <- WB06_SDMg2$player2
WB06_SDMg3 <- WB06_SDMg2
WB06_SDMg3$p1inp2vec <- is.element(WB06_SDMg3$player1, player2vector)
WB06_SDMg3$p2inp1vec <- is.element(WB06_SDMg3$player2, player1vector)

addPlayer1 <- WB06_SDMg3[ which(WB06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_SDMg3[ which(WB06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_SDMg2 <- rbind(WB06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
WB06_SDMft <- ftable(WB06_SDMg2$player1, WB06_SDMg2$player2)
WB06_SDMft2 <- as.matrix(WB06_SDMft)
numRows <- nrow(WB06_SDMft2)
numCols <- ncol(WB06_SDMft2)
WB06_SDMft3 <- WB06_SDMft2[c(2:numRows) , c(2:numCols)]
WB06_SDMTable <- graph.adjacency(WB06_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(WB06_SDMTable, vertex.label = V(WB06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
WB06_SDM.clusterCoef <- transitivity(WB06_SDMTable, type="global") #cluster coefficient
WB06_SDM.degreeCent <- centralization.degree(WB06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_SDMftn <- as.network.matrix(WB06_SDMft)
WB06_SDM.netDensity <- network.density(WB06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_SDM.entropy <- entropy(WB06_SDMft) #entropy

WB06_SDM.netMx <- cbind(WB06_SDM.netMx, WB06_SDM.clusterCoef, WB06_SDM.degreeCent$centralization,
                        WB06_SDM.netDensity, WB06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "WB"
KIoutcome = "Turnover_DM"
WB06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
WB06_TDMg2 <- data.frame(WB06_TDM)
WB06_TDMg2 <- WB06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_TDMg2$player1
player2vector <- WB06_TDMg2$player2
WB06_TDMg3 <- WB06_TDMg2
WB06_TDMg3$p1inp2vec <- is.element(WB06_TDMg3$player1, player2vector)
WB06_TDMg3$p2inp1vec <- is.element(WB06_TDMg3$player2, player1vector)

addPlayer1 <- WB06_TDMg3[ which(WB06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_TDMg3[ which(WB06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_TDMg2 <- rbind(WB06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
WB06_TDMft <- ftable(WB06_TDMg2$player1, WB06_TDMg2$player2)
WB06_TDMft2 <- as.matrix(WB06_TDMft)
numRows <- nrow(WB06_TDMft2)
numCols <- ncol(WB06_TDMft2)
WB06_TDMft3 <- WB06_TDMft2[c(2:numRows) , c(2:numCols)]
WB06_TDMTable <- graph.adjacency(WB06_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(WB06_TDMTable, vertex.label = V(WB06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
WB06_TDM.clusterCoef <- transitivity(WB06_TDMTable, type="global") #cluster coefficient
WB06_TDM.degreeCent <- centralization.degree(WB06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_TDMftn <- as.network.matrix(WB06_TDMft)
WB06_TDM.netDensity <- network.density(WB06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_TDM.entropy <- entropy(WB06_TDMft) #entropy

WB06_TDM.netMx <- cbind(WB06_TDM.netMx, WB06_TDM.clusterCoef, WB06_TDM.degreeCent$centralization,
                        WB06_TDM.netDensity, WB06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "WB"
KIoutcome = "Stoppage_D"
WB06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
WB06_SDg2 <- data.frame(WB06_SD)
WB06_SDg2 <- WB06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_SDg2$player1
player2vector <- WB06_SDg2$player2
WB06_SDg3 <- WB06_SDg2
WB06_SDg3$p1inp2vec <- is.element(WB06_SDg3$player1, player2vector)
WB06_SDg3$p2inp1vec <- is.element(WB06_SDg3$player2, player1vector)

addPlayer1 <- WB06_SDg3[ which(WB06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_SDg3[ which(WB06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_SDg2 <- rbind(WB06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
WB06_SDft <- ftable(WB06_SDg2$player1, WB06_SDg2$player2)
WB06_SDft2 <- as.matrix(WB06_SDft)
numRows <- nrow(WB06_SDft2)
numCols <- ncol(WB06_SDft2)
WB06_SDft3 <- WB06_SDft2[c(2:numRows) , c(2:numCols)]
WB06_SDTable <- graph.adjacency(WB06_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(WB06_SDTable, vertex.label = V(WB06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
WB06_SD.clusterCoef <- transitivity(WB06_SDTable, type="global") #cluster coefficient
WB06_SD.degreeCent <- centralization.degree(WB06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_SDftn <- as.network.matrix(WB06_SDft)
WB06_SD.netDensity <- network.density(WB06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_SD.entropy <- entropy(WB06_SDft) #entropy

WB06_SD.netMx <- cbind(WB06_SD.netMx, WB06_SD.clusterCoef, WB06_SD.degreeCent$centralization,
                       WB06_SD.netDensity, WB06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "WB"
KIoutcome = "Turnover_D"
WB06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
WB06_TDg2 <- data.frame(WB06_TD)
WB06_TDg2 <- WB06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_TDg2$player1
player2vector <- WB06_TDg2$player2
WB06_TDg3 <- WB06_TDg2
WB06_TDg3$p1inp2vec <- is.element(WB06_TDg3$player1, player2vector)
WB06_TDg3$p2inp1vec <- is.element(WB06_TDg3$player2, player1vector)

addPlayer1 <- WB06_TDg3[ which(WB06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_TDg3[ which(WB06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_TDg2 <- rbind(WB06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
WB06_TDft <- ftable(WB06_TDg2$player1, WB06_TDg2$player2)
WB06_TDft2 <- as.matrix(WB06_TDft)
numRows <- nrow(WB06_TDft2)
numCols <- ncol(WB06_TDft2)
WB06_TDft3 <- WB06_TDft2[c(2:numRows) , c(2:numCols)]
WB06_TDTable <- graph.adjacency(WB06_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(WB06_TDTable, vertex.label = V(WB06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
WB06_TD.clusterCoef <- transitivity(WB06_TDTable, type="global") #cluster coefficient
WB06_TD.degreeCent <- centralization.degree(WB06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_TDftn <- as.network.matrix(WB06_TDft)
WB06_TD.netDensity <- network.density(WB06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_TD.entropy <- entropy(WB06_TDft) #entropy

WB06_TD.netMx <- cbind(WB06_TD.netMx, WB06_TD.clusterCoef, WB06_TD.degreeCent$centralization,
                       WB06_TD.netDensity, WB06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
WB06_QTg2 <- data.frame(WB06_QT)
WB06_QTg2 <- WB06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB06_QTg2$player1
player2vector <- WB06_QTg2$player2
WB06_QTg3 <- WB06_QTg2
WB06_QTg3$p1inp2vec <- is.element(WB06_QTg3$player1, player2vector)
WB06_QTg3$p2inp1vec <- is.element(WB06_QTg3$player2, player1vector)

addPlayer1 <- WB06_QTg3[ which(WB06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB06_QTg3[ which(WB06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB06_QTg2 <- rbind(WB06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
WB06_QTft <- ftable(WB06_QTg2$player1, WB06_QTg2$player2)
WB06_QTft2 <- as.matrix(WB06_QTft)
numRows <- nrow(WB06_QTft2)
numCols <- ncol(WB06_QTft2)
WB06_QTft3 <- WB06_QTft2[c(2:numRows) , c(2:numCols)]
WB06_QTTable <- graph.adjacency(WB06_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(WB06_QTTable, vertex.label = V(WB06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
WB06_QT.clusterCoef <- transitivity(WB06_QTTable, type="global") #cluster coefficient
WB06_QT.degreeCent <- centralization.degree(WB06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB06_QTftn <- as.network.matrix(WB06_QTft)
WB06_QT.netDensity <- network.density(WB06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB06_QT.entropy <- entropy(WB06_QTft) #entropy

WB06_QT.netMx <- cbind(WB06_QT.netMx, WB06_QT.clusterCoef, WB06_QT.degreeCent$centralization,
                       WB06_QT.netDensity, WB06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB06_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 6
##

#ROUND 6, Goal***************************************************************
#NA
round = 6
teamName = "WCE"
KIoutcome = "Goal_F"
WCE06_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Goal with weighted edges
WCE06_Gg2 <- data.frame(WCE06_G)
WCE06_Gg2 <- WCE06_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_Gg2$player1
player2vector <- WCE06_Gg2$player2
WCE06_Gg3 <- WCE06_Gg2
WCE06_Gg3$p1inp2vec <- is.element(WCE06_Gg3$player1, player2vector)
WCE06_Gg3$p2inp1vec <- is.element(WCE06_Gg3$player2, player1vector)

addPlayer1 <- WCE06_Gg3[ which(WCE06_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_Gg3[ which(WCE06_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_Gg2 <- rbind(WCE06_Gg2, addPlayers)

#ROUND 6, Goal graph using weighted edges
WCE06_Gft <- ftable(WCE06_Gg2$player1, WCE06_Gg2$player2)
WCE06_Gft2 <- as.matrix(WCE06_Gft)
numRows <- nrow(WCE06_Gft2)
numCols <- ncol(WCE06_Gft2)
WCE06_Gft3 <- WCE06_Gft2[c(2:numRows) , c(2:numCols)]
WCE06_GTable <- graph.adjacency(WCE06_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Goal graph=weighted
plot.igraph(WCE06_GTable, vertex.label = V(WCE06_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Goal calulation of network metrics
#igraph
WCE06_G.clusterCoef <- transitivity(WCE06_GTable, type="global") #cluster coefficient
WCE06_G.degreeCent <- centralization.degree(WCE06_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_Gftn <- as.network.matrix(WCE06_Gft)
WCE06_G.netDensity <- network.density(WCE06_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_G.entropy <- entropy(WCE06_Gft) #entropy

WCE06_G.netMx <- cbind(WCE06_G.netMx, WCE06_G.clusterCoef, WCE06_G.degreeCent$centralization,
                       WCE06_G.netDensity, WCE06_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_G.netMx) <- varnames

#ROUND 6, Behind***************************************************************
#NA

round = 6
teamName = "WCE"
KIoutcome = "Behind_F"
WCE06_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, Behind with weighted edges
WCE06_Bg2 <- data.frame(WCE06_B)
WCE06_Bg2 <- WCE06_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_Bg2$player1
player2vector <- WCE06_Bg2$player2
WCE06_Bg3 <- WCE06_Bg2
WCE06_Bg3$p1inp2vec <- is.element(WCE06_Bg3$player1, player2vector)
WCE06_Bg3$p2inp1vec <- is.element(WCE06_Bg3$player2, player1vector)

addPlayer1 <- WCE06_Bg3[ which(WCE06_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_Bg3[ which(WCE06_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_Bg2 <- rbind(WCE06_Bg2, addPlayers)

#ROUND 6, Behind graph using weighted edges
WCE06_Bft <- ftable(WCE06_Bg2$player1, WCE06_Bg2$player2)
WCE06_Bft2 <- as.matrix(WCE06_Bft)
numRows <- nrow(WCE06_Bft2)
numCols <- ncol(WCE06_Bft2)
WCE06_Bft3 <- WCE06_Bft2[c(2:numRows) , c(2:numCols)]
WCE06_BTable <- graph.adjacency(WCE06_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 6, Behind graph=weighted
plot.igraph(WCE06_BTable, vertex.label = V(WCE06_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, Behind calulation of network metrics
#igraph
WCE06_B.clusterCoef <- transitivity(WCE06_BTable, type="global") #cluster coefficient
WCE06_B.degreeCent <- centralization.degree(WCE06_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_Bftn <- as.network.matrix(WCE06_Bft)
WCE06_B.netDensity <- network.density(WCE06_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_B.entropy <- entropy(WCE06_Bft) #entropy

WCE06_B.netMx <- cbind(WCE06_B.netMx, WCE06_B.clusterCoef, WCE06_B.degreeCent$centralization,
                       WCE06_B.netDensity, WCE06_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_B.netMx) <- varnames

#ROUND 6, FWD Stoppage**********************************************************
#NA

round = 6
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE06_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Stoppage with weighted edges
WCE06_SFg2 <- data.frame(WCE06_SF)
WCE06_SFg2 <- WCE06_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_SFg2$player1
player2vector <- WCE06_SFg2$player2
WCE06_SFg3 <- WCE06_SFg2
WCE06_SFg3$p1inp2vec <- is.element(WCE06_SFg3$player1, player2vector)
WCE06_SFg3$p2inp1vec <- is.element(WCE06_SFg3$player2, player1vector)

addPlayer1 <- WCE06_SFg3[ which(WCE06_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE06_SFg2 <- rbind(WCE06_SFg2, addPlayer1)

#ROUND 6, FWD Stoppage graph using weighted edges
WCE06_SFft <- ftable(WCE06_SFg2$player1, WCE06_SFg2$player2)
WCE06_SFft2 <- as.matrix(WCE06_SFft)
numRows <- nrow(WCE06_SFft2)
numCols <- ncol(WCE06_SFft2)
WCE06_SFft3 <- WCE06_SFft2[c(2:numRows) , c(1:numCols)]
WCE06_SFTable <- graph.adjacency(WCE06_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Stoppage graph=weighted
plot.igraph(WCE06_SFTable, vertex.label = V(WCE06_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Stoppage calulation of network metrics
#igraph
WCE06_SF.clusterCoef <- transitivity(WCE06_SFTable, type="global") #cluster coefficient
WCE06_SF.degreeCent <- centralization.degree(WCE06_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_SFftn <- as.network.matrix(WCE06_SFft)
WCE06_SF.netDensity <- network.density(WCE06_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_SF.entropy <- entropy(WCE06_SFft) #entropy

WCE06_SF.netMx <- cbind(WCE06_SF.netMx, WCE06_SF.clusterCoef, WCE06_SF.degreeCent$centralization,
                        WCE06_SF.netDensity, WCE06_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_SF.netMx) <- varnames

#ROUND 6, FWD Turnover**********************************************************

round = 6
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE06_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, FWD Turnover with weighted edges
WCE06_TFg2 <- data.frame(WCE06_TF)
WCE06_TFg2 <- WCE06_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_TFg2$player1
player2vector <- WCE06_TFg2$player2
WCE06_TFg3 <- WCE06_TFg2
WCE06_TFg3$p1inp2vec <- is.element(WCE06_TFg3$player1, player2vector)
WCE06_TFg3$p2inp1vec <- is.element(WCE06_TFg3$player2, player1vector)

addPlayer1 <- WCE06_TFg3[ which(WCE06_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_TFg3[ which(WCE06_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_TFg2 <- rbind(WCE06_TFg2, addPlayers)

#ROUND 6, FWD Turnover graph using weighted edges
WCE06_TFft <- ftable(WCE06_TFg2$player1, WCE06_TFg2$player2)
WCE06_TFft2 <- as.matrix(WCE06_TFft)
numRows <- nrow(WCE06_TFft2)
numCols <- ncol(WCE06_TFft2)
WCE06_TFft3 <- WCE06_TFft2[c(2:numRows) , c(2:numCols)]
WCE06_TFTable <- graph.adjacency(WCE06_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, FWD Turnover graph=weighted
plot.igraph(WCE06_TFTable, vertex.label = V(WCE06_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, FWD Turnover calulation of network metrics
#igraph
WCE06_TF.clusterCoef <- transitivity(WCE06_TFTable, type="global") #cluster coefficient
WCE06_TF.degreeCent <- centralization.degree(WCE06_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_TFftn <- as.network.matrix(WCE06_TFft)
WCE06_TF.netDensity <- network.density(WCE06_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_TF.entropy <- entropy(WCE06_TFft) #entropy

WCE06_TF.netMx <- cbind(WCE06_TF.netMx, WCE06_TF.clusterCoef, WCE06_TF.degreeCent$centralization,
                        WCE06_TF.netDensity, WCE06_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_TF.netMx) <- varnames

#ROUND 6, AM Stoppage**********************************************************
#NA

round = 6
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE06_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Stoppage with weighted edges
WCE06_SAMg2 <- data.frame(WCE06_SAM)
WCE06_SAMg2 <- WCE06_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_SAMg2$player1
player2vector <- WCE06_SAMg2$player2
WCE06_SAMg3 <- WCE06_SAMg2
WCE06_SAMg3$p1inp2vec <- is.element(WCE06_SAMg3$player1, player2vector)
WCE06_SAMg3$p2inp1vec <- is.element(WCE06_SAMg3$player2, player1vector)

addPlayer1 <- WCE06_SAMg3[ which(WCE06_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_SAMg3[ which(WCE06_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_SAMg2 <- rbind(WCE06_SAMg2, addPlayers)

#ROUND 6, AM Stoppage graph using weighted edges
WCE06_SAMft <- ftable(WCE06_SAMg2$player1, WCE06_SAMg2$player2)
WCE06_SAMft2 <- as.matrix(WCE06_SAMft)
numRows <- nrow(WCE06_SAMft2)
numCols <- ncol(WCE06_SAMft2)
WCE06_SAMft3 <- WCE06_SAMft2[c(2:numRows) , c(2:numCols)]
WCE06_SAMTable <- graph.adjacency(WCE06_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Stoppage graph=weighted
plot.igraph(WCE06_SAMTable, vertex.label = V(WCE06_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Stoppage calulation of network metrics
#igraph
WCE06_SAM.clusterCoef <- transitivity(WCE06_SAMTable, type="global") #cluster coefficient
WCE06_SAM.degreeCent <- centralization.degree(WCE06_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_SAMftn <- as.network.matrix(WCE06_SAMft)
WCE06_SAM.netDensity <- network.density(WCE06_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_SAM.entropy <- entropy(WCE06_SAMft) #entropy

WCE06_SAM.netMx <- cbind(WCE06_SAM.netMx, WCE06_SAM.clusterCoef, WCE06_SAM.degreeCent$centralization,
                         WCE06_SAM.netDensity, WCE06_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_SAM.netMx) <- varnames

#ROUND 6, AM Turnover**********************************************************
#NA

round = 6
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE06_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, AM Turnover with weighted edges
WCE06_TAMg2 <- data.frame(WCE06_TAM)
WCE06_TAMg2 <- WCE06_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_TAMg2$player1
player2vector <- WCE06_TAMg2$player2
WCE06_TAMg3 <- WCE06_TAMg2
WCE06_TAMg3$p1inp2vec <- is.element(WCE06_TAMg3$player1, player2vector)
WCE06_TAMg3$p2inp1vec <- is.element(WCE06_TAMg3$player2, player1vector)

addPlayer1 <- WCE06_TAMg3[ which(WCE06_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_TAMg3[ which(WCE06_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_TAMg2 <- rbind(WCE06_TAMg2, addPlayers)

#ROUND 6, AM Turnover graph using weighted edges
WCE06_TAMft <- ftable(WCE06_TAMg2$player1, WCE06_TAMg2$player2)
WCE06_TAMft2 <- as.matrix(WCE06_TAMft)
numRows <- nrow(WCE06_TAMft2)
numCols <- ncol(WCE06_TAMft2)
WCE06_TAMft3 <- WCE06_TAMft2[c(2:numRows) , c(2:numCols)]
WCE06_TAMTable <- graph.adjacency(WCE06_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, AM Turnover graph=weighted
plot.igraph(WCE06_TAMTable, vertex.label = V(WCE06_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, AM Turnover calulation of network metrics
#igraph
WCE06_TAM.clusterCoef <- transitivity(WCE06_TAMTable, type="global") #cluster coefficient
WCE06_TAM.degreeCent <- centralization.degree(WCE06_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_TAMftn <- as.network.matrix(WCE06_TAMft)
WCE06_TAM.netDensity <- network.density(WCE06_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_TAM.entropy <- entropy(WCE06_TAMft) #entropy

WCE06_TAM.netMx <- cbind(WCE06_TAM.netMx, WCE06_TAM.clusterCoef, WCE06_TAM.degreeCent$centralization,
                         WCE06_TAM.netDensity, WCE06_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_TAM.netMx) <- varnames

#ROUND 6, DM Stoppage**********************************************************
#NA

round = 6
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE06_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Stoppage with weighted edges
WCE06_SDMg2 <- data.frame(WCE06_SDM)
WCE06_SDMg2 <- WCE06_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_SDMg2$player1
player2vector <- WCE06_SDMg2$player2
WCE06_SDMg3 <- WCE06_SDMg2
WCE06_SDMg3$p1inp2vec <- is.element(WCE06_SDMg3$player1, player2vector)
WCE06_SDMg3$p2inp1vec <- is.element(WCE06_SDMg3$player2, player1vector)

addPlayer1 <- WCE06_SDMg3[ which(WCE06_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_SDMg3[ which(WCE06_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_SDMg2 <- rbind(WCE06_SDMg2, addPlayers)

#ROUND 6, DM Stoppage graph using weighted edges
WCE06_SDMft <- ftable(WCE06_SDMg2$player1, WCE06_SDMg2$player2)
WCE06_SDMft2 <- as.matrix(WCE06_SDMft)
numRows <- nrow(WCE06_SDMft2)
numCols <- ncol(WCE06_SDMft2)
WCE06_SDMft3 <- WCE06_SDMft2[c(2:numRows) , c(2:numCols)]
WCE06_SDMTable <- graph.adjacency(WCE06_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Stoppage graph=weighted
plot.igraph(WCE06_SDMTable, vertex.label = V(WCE06_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Stoppage calulation of network metrics
#igraph
WCE06_SDM.clusterCoef <- transitivity(WCE06_SDMTable, type="global") #cluster coefficient
WCE06_SDM.degreeCent <- centralization.degree(WCE06_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_SDMftn <- as.network.matrix(WCE06_SDMft)
WCE06_SDM.netDensity <- network.density(WCE06_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_SDM.entropy <- entropy(WCE06_SDMft) #entropy

WCE06_SDM.netMx <- cbind(WCE06_SDM.netMx, WCE06_SDM.clusterCoef, WCE06_SDM.degreeCent$centralization,
                         WCE06_SDM.netDensity, WCE06_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_SDM.netMx) <- varnames

#ROUND 6, DM Turnover**********************************************************

round = 6
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE06_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, DM Turnover with weighted edges
WCE06_TDMg2 <- data.frame(WCE06_TDM)
WCE06_TDMg2 <- WCE06_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_TDMg2$player1
player2vector <- WCE06_TDMg2$player2
WCE06_TDMg3 <- WCE06_TDMg2
WCE06_TDMg3$p1inp2vec <- is.element(WCE06_TDMg3$player1, player2vector)
WCE06_TDMg3$p2inp1vec <- is.element(WCE06_TDMg3$player2, player1vector)

addPlayer1 <- WCE06_TDMg3[ which(WCE06_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_TDMg3[ which(WCE06_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_TDMg2 <- rbind(WCE06_TDMg2, addPlayers)

#ROUND 6, DM Turnover graph using weighted edges
WCE06_TDMft <- ftable(WCE06_TDMg2$player1, WCE06_TDMg2$player2)
WCE06_TDMft2 <- as.matrix(WCE06_TDMft)
numRows <- nrow(WCE06_TDMft2)
numCols <- ncol(WCE06_TDMft2)
WCE06_TDMft3 <- WCE06_TDMft2[c(2:numRows) , c(2:numCols)]
WCE06_TDMTable <- graph.adjacency(WCE06_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 6, DM Turnover graph=weighted
plot.igraph(WCE06_TDMTable, vertex.label = V(WCE06_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, DM Turnover calulation of network metrics
#igraph
WCE06_TDM.clusterCoef <- transitivity(WCE06_TDMTable, type="global") #cluster coefficient
WCE06_TDM.degreeCent <- centralization.degree(WCE06_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_TDMftn <- as.network.matrix(WCE06_TDMft)
WCE06_TDM.netDensity <- network.density(WCE06_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_TDM.entropy <- entropy(WCE06_TDMft) #entropy

WCE06_TDM.netMx <- cbind(WCE06_TDM.netMx, WCE06_TDM.clusterCoef, WCE06_TDM.degreeCent$centralization,
                         WCE06_TDM.netDensity, WCE06_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_TDM.netMx) <- varnames

#ROUND 6, D Stoppage**********************************************************
#NA

round = 6
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE06_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Stoppage with weighted edges
WCE06_SDg2 <- data.frame(WCE06_SD)
WCE06_SDg2 <- WCE06_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_SDg2$player1
player2vector <- WCE06_SDg2$player2
WCE06_SDg3 <- WCE06_SDg2
WCE06_SDg3$p1inp2vec <- is.element(WCE06_SDg3$player1, player2vector)
WCE06_SDg3$p2inp1vec <- is.element(WCE06_SDg3$player2, player1vector)

addPlayer1 <- WCE06_SDg3[ which(WCE06_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_SDg3[ which(WCE06_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_SDg2 <- rbind(WCE06_SDg2, addPlayers)

#ROUND 6, D Stoppage graph using weighted edges
WCE06_SDft <- ftable(WCE06_SDg2$player1, WCE06_SDg2$player2)
WCE06_SDft2 <- as.matrix(WCE06_SDft)
numRows <- nrow(WCE06_SDft2)
numCols <- ncol(WCE06_SDft2)
WCE06_SDft3 <- WCE06_SDft2[c(2:numRows) , c(2:numCols)]
WCE06_SDTable <- graph.adjacency(WCE06_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Stoppage graph=weighted
plot.igraph(WCE06_SDTable, vertex.label = V(WCE06_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Stoppage calulation of network metrics
#igraph
WCE06_SD.clusterCoef <- transitivity(WCE06_SDTable, type="global") #cluster coefficient
WCE06_SD.degreeCent <- centralization.degree(WCE06_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_SDftn <- as.network.matrix(WCE06_SDft)
WCE06_SD.netDensity <- network.density(WCE06_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_SD.entropy <- entropy(WCE06_SDft) #entropy

WCE06_SD.netMx <- cbind(WCE06_SD.netMx, WCE06_SD.clusterCoef, WCE06_SD.degreeCent$centralization,
                        WCE06_SD.netDensity, WCE06_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_SD.netMx) <- varnames

#ROUND 6, D Turnover**********************************************************
#NA

round = 6
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE06_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, D Turnover with weighted edges
WCE06_TDg2 <- data.frame(WCE06_TD)
WCE06_TDg2 <- WCE06_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_TDg2$player1
player2vector <- WCE06_TDg2$player2
WCE06_TDg3 <- WCE06_TDg2
WCE06_TDg3$p1inp2vec <- is.element(WCE06_TDg3$player1, player2vector)
WCE06_TDg3$p2inp1vec <- is.element(WCE06_TDg3$player2, player1vector)

addPlayer1 <- WCE06_TDg3[ which(WCE06_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_TDg3[ which(WCE06_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_TDg2 <- rbind(WCE06_TDg2, addPlayers)

#ROUND 6, D Turnover graph using weighted edges
WCE06_TDft <- ftable(WCE06_TDg2$player1, WCE06_TDg2$player2)
WCE06_TDft2 <- as.matrix(WCE06_TDft)
numRows <- nrow(WCE06_TDft2)
numCols <- ncol(WCE06_TDft2)
WCE06_TDft3 <- WCE06_TDft2[c(2:numRows) , c(2:numCols)]
WCE06_TDTable <- graph.adjacency(WCE06_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, D Turnover graph=weighted
plot.igraph(WCE06_TDTable, vertex.label = V(WCE06_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, D Turnover calulation of network metrics
#igraph
WCE06_TD.clusterCoef <- transitivity(WCE06_TDTable, type="global") #cluster coefficient
WCE06_TD.degreeCent <- centralization.degree(WCE06_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_TDftn <- as.network.matrix(WCE06_TDft)
WCE06_TD.netDensity <- network.density(WCE06_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_TD.entropy <- entropy(WCE06_TDft) #entropy

WCE06_TD.netMx <- cbind(WCE06_TD.netMx, WCE06_TD.clusterCoef, WCE06_TD.degreeCent$centralization,
                        WCE06_TD.netDensity, WCE06_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_TD.netMx) <- varnames

#ROUND 6, End of Qtr**********************************************************
#NA

round = 6
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE06_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 6, End of Qtr with weighted edges
WCE06_QTg2 <- data.frame(WCE06_QT)
WCE06_QTg2 <- WCE06_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE06_QTg2$player1
player2vector <- WCE06_QTg2$player2
WCE06_QTg3 <- WCE06_QTg2
WCE06_QTg3$p1inp2vec <- is.element(WCE06_QTg3$player1, player2vector)
WCE06_QTg3$p2inp1vec <- is.element(WCE06_QTg3$player2, player1vector)

addPlayer1 <- WCE06_QTg3[ which(WCE06_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE06_QTg3[ which(WCE06_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE06_QTg2 <- rbind(WCE06_QTg2, addPlayers)

#ROUND 6, End of Qtr graph using weighted edges
WCE06_QTft <- ftable(WCE06_QTg2$player1, WCE06_QTg2$player2)
WCE06_QTft2 <- as.matrix(WCE06_QTft)
numRows <- nrow(WCE06_QTft2)
numCols <- ncol(WCE06_QTft2)
WCE06_QTft3 <- WCE06_QTft2[c(2:numRows) , c(2:numCols)]
WCE06_QTTable <- graph.adjacency(WCE06_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 6, End of Qtr graph=weighted
plot.igraph(WCE06_QTTable, vertex.label = V(WCE06_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE06_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 6, End of Qtr calulation of network metrics
#igraph
WCE06_QT.clusterCoef <- transitivity(WCE06_QTTable, type="global") #cluster coefficient
WCE06_QT.degreeCent <- centralization.degree(WCE06_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE06_QTftn <- as.network.matrix(WCE06_QTft)
WCE06_QT.netDensity <- network.density(WCE06_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE06_QT.entropy <- entropy(WCE06_QTft) #entropy

WCE06_QT.netMx <- cbind(WCE06_QT.netMx, WCE06_QT.clusterCoef, WCE06_QT.degreeCent$centralization,
                        WCE06_QT.netDensity, WCE06_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE06_QT.netMx) <- varnames
