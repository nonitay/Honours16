#####
#09-28-16- Real data 12
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************
#NA

round = 12
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
ADEL12_Gg2 <- data.frame(ADEL12_G)
ADEL12_Gg2 <- ADEL12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_Gg2$player1
player2vector <- ADEL12_Gg2$player2
ADEL12_Gg3 <- ADEL12_Gg2
ADEL12_Gg3$p1inp2vec <- is.element(ADEL12_Gg3$player1, player2vector)
ADEL12_Gg3$p2inp1vec <- is.element(ADEL12_Gg3$player2, player1vector)

addPlayer1 <- ADEL12_Gg3[ which(ADEL12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_Gg3[ which(ADEL12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_Gg2 <- rbind(ADEL12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
ADEL12_Gft <- ftable(ADEL12_Gg2$player1, ADEL12_Gg2$player2)
ADEL12_Gft2 <- as.matrix(ADEL12_Gft)
numRows <- nrow(ADEL12_Gft2)
numCols <- ncol(ADEL12_Gft2)
ADEL12_Gft3 <- ADEL12_Gft2[c(2:numRows) , c(2:numCols)]
ADEL12_GTable <- graph.adjacency(ADEL12_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(ADEL12_GTable, vertex.label = V(ADEL12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
ADEL12_G.clusterCoef <- transitivity(ADEL12_GTable, type="global") #cluster coefficient
ADEL12_G.degreeCent <- centralization.degree(ADEL12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_Gftn <- as.network.matrix(ADEL12_Gft)
ADEL12_G.netDensity <- network.density(ADEL12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_G.entropy <- entropy(ADEL12_Gft) #entropy

ADEL12_G.netMx <- cbind(ADEL12_G.netMx, ADEL12_G.clusterCoef, ADEL12_G.degreeCent$centralization,
                        ADEL12_G.netDensity, ADEL12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************

round = 12
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
ADEL12_Bg2 <- data.frame(ADEL12_B)
ADEL12_Bg2 <- ADEL12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_Bg2$player1
player2vector <- ADEL12_Bg2$player2
ADEL12_Bg3 <- ADEL12_Bg2
ADEL12_Bg3$p1inp2vec <- is.element(ADEL12_Bg3$player1, player2vector)
ADEL12_Bg3$p2inp1vec <- is.element(ADEL12_Bg3$player2, player1vector)

addPlayer1 <- ADEL12_Bg3[ which(ADEL12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_Bg3[ which(ADEL12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_Bg2 <- rbind(ADEL12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
ADEL12_Bft <- ftable(ADEL12_Bg2$player1, ADEL12_Bg2$player2)
ADEL12_Bft2 <- as.matrix(ADEL12_Bft)
numRows <- nrow(ADEL12_Bft2)
numCols <- ncol(ADEL12_Bft2)
ADEL12_Bft3 <- ADEL12_Bft2[c(2:numRows) , c(2:numCols)]
ADEL12_BTable <- graph.adjacency(ADEL12_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(ADEL12_BTable, vertex.label = V(ADEL12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
ADEL12_B.clusterCoef <- transitivity(ADEL12_BTable, type="global") #cluster coefficient
ADEL12_B.degreeCent <- centralization.degree(ADEL12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_Bftn <- as.network.matrix(ADEL12_Bft)
ADEL12_B.netDensity <- network.density(ADEL12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_B.entropy <- entropy(ADEL12_Bft) #entropy

ADEL12_B.netMx <- cbind(ADEL12_B.netMx, ADEL12_B.clusterCoef, ADEL12_B.degreeCent$centralization,
                        ADEL12_B.netDensity, ADEL12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************
#NA

round = 12
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
ADEL12_SFg2 <- data.frame(ADEL12_SF)
ADEL12_SFg2 <- ADEL12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_SFg2$player1
player2vector <- ADEL12_SFg2$player2
ADEL12_SFg3 <- ADEL12_SFg2
ADEL12_SFg3$p1inp2vec <- is.element(ADEL12_SFg3$player1, player2vector)
ADEL12_SFg3$p2inp1vec <- is.element(ADEL12_SFg3$player2, player1vector)

addPlayer1 <- ADEL12_SFg3[ which(ADEL12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_SFg3[ which(ADEL12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_SFg2 <- rbind(ADEL12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
ADEL12_SFft <- ftable(ADEL12_SFg2$player1, ADEL12_SFg2$player2)
ADEL12_SFft2 <- as.matrix(ADEL12_SFft)
numRows <- nrow(ADEL12_SFft2)
numCols <- ncol(ADEL12_SFft2)
ADEL12_SFft3 <- ADEL12_SFft2[c(2:numRows) , c(2:numCols)]
ADEL12_SFTable <- graph.adjacency(ADEL12_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(ADEL12_SFTable, vertex.label = V(ADEL12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
ADEL12_SF.clusterCoef <- transitivity(ADEL12_SFTable, type="global") #cluster coefficient
ADEL12_SF.degreeCent <- centralization.degree(ADEL12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_SFftn <- as.network.matrix(ADEL12_SFft)
ADEL12_SF.netDensity <- network.density(ADEL12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_SF.entropy <- entropy(ADEL12_SFft) #entropy

ADEL12_SF.netMx <- cbind(ADEL12_SF.netMx, ADEL12_SF.clusterCoef, ADEL12_SF.degreeCent$centralization,
                         ADEL12_SF.netDensity, ADEL12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************

round = 12
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
ADEL12_TFg2 <- data.frame(ADEL12_TF)
ADEL12_TFg2 <- ADEL12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_TFg2$player1
player2vector <- ADEL12_TFg2$player2
ADEL12_TFg3 <- ADEL12_TFg2
ADEL12_TFg3$p1inp2vec <- is.element(ADEL12_TFg3$player1, player2vector)
ADEL12_TFg3$p2inp1vec <- is.element(ADEL12_TFg3$player2, player1vector)

addPlayer1 <- ADEL12_TFg3[ which(ADEL12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_TFg3[ which(ADEL12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_TFg2 <- rbind(ADEL12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
ADEL12_TFft <- ftable(ADEL12_TFg2$player1, ADEL12_TFg2$player2)
ADEL12_TFft2 <- as.matrix(ADEL12_TFft)
numRows <- nrow(ADEL12_TFft2)
numCols <- ncol(ADEL12_TFft2)
ADEL12_TFft3 <- ADEL12_TFft2[c(2:numRows) , c(2:numCols)]
ADEL12_TFTable <- graph.adjacency(ADEL12_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(ADEL12_TFTable, vertex.label = V(ADEL12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
ADEL12_TF.clusterCoef <- transitivity(ADEL12_TFTable, type="global") #cluster coefficient
ADEL12_TF.degreeCent <- centralization.degree(ADEL12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_TFftn <- as.network.matrix(ADEL12_TFft)
ADEL12_TF.netDensity <- network.density(ADEL12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_TF.entropy <- entropy(ADEL12_TFft) #entropy

ADEL12_TF.netMx <- cbind(ADEL12_TF.netMx, ADEL12_TF.clusterCoef, ADEL12_TF.degreeCent$centralization,
                         ADEL12_TF.netDensity, ADEL12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************
#NA

round = 12
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
ADEL12_SAMg2 <- data.frame(ADEL12_SAM)
ADEL12_SAMg2 <- ADEL12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_SAMg2$player1
player2vector <- ADEL12_SAMg2$player2
ADEL12_SAMg3 <- ADEL12_SAMg2
ADEL12_SAMg3$p1inp2vec <- is.element(ADEL12_SAMg3$player1, player2vector)
ADEL12_SAMg3$p2inp1vec <- is.element(ADEL12_SAMg3$player2, player1vector)

addPlayer1 <- ADEL12_SAMg3[ which(ADEL12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_SAMg3[ which(ADEL12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_SAMg2 <- rbind(ADEL12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
ADEL12_SAMft <- ftable(ADEL12_SAMg2$player1, ADEL12_SAMg2$player2)
ADEL12_SAMft2 <- as.matrix(ADEL12_SAMft)
numRows <- nrow(ADEL12_SAMft2)
numCols <- ncol(ADEL12_SAMft2)
ADEL12_SAMft3 <- ADEL12_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL12_SAMTable <- graph.adjacency(ADEL12_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(ADEL12_SAMTable, vertex.label = V(ADEL12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
ADEL12_SAM.clusterCoef <- transitivity(ADEL12_SAMTable, type="global") #cluster coefficient
ADEL12_SAM.degreeCent <- centralization.degree(ADEL12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_SAMftn <- as.network.matrix(ADEL12_SAMft)
ADEL12_SAM.netDensity <- network.density(ADEL12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_SAM.entropy <- entropy(ADEL12_SAMft) #entropy

ADEL12_SAM.netMx <- cbind(ADEL12_SAM.netMx, ADEL12_SAM.clusterCoef, ADEL12_SAM.degreeCent$centralization,
                          ADEL12_SAM.netDensity, ADEL12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************

round = 12
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
ADEL12_TAMg2 <- data.frame(ADEL12_TAM)
ADEL12_TAMg2 <- ADEL12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_TAMg2$player1
player2vector <- ADEL12_TAMg2$player2
ADEL12_TAMg3 <- ADEL12_TAMg2
ADEL12_TAMg3$p1inp2vec <- is.element(ADEL12_TAMg3$player1, player2vector)
ADEL12_TAMg3$p2inp1vec <- is.element(ADEL12_TAMg3$player2, player1vector)

addPlayer1 <- ADEL12_TAMg3[ which(ADEL12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_TAMg3[ which(ADEL12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_TAMg2 <- rbind(ADEL12_TAMg2, addPlayers)


#ROUND 12, AM Turnover graph using weighted edges
ADEL12_TAMft <- ftable(ADEL12_TAMg2$player1, ADEL12_TAMg2$player2)
ADEL12_TAMft2 <- as.matrix(ADEL12_TAMft)
numRows <- nrow(ADEL12_TAMft2)
numCols <- ncol(ADEL12_TAMft2)
ADEL12_TAMft3 <- ADEL12_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL12_TAMTable <- graph.adjacency(ADEL12_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(ADEL12_TAMTable, vertex.label = V(ADEL12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
ADEL12_TAM.clusterCoef <- transitivity(ADEL12_TAMTable, type="global") #cluster coefficient
ADEL12_TAM.degreeCent <- centralization.degree(ADEL12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_TAMftn <- as.network.matrix(ADEL12_TAMft)
ADEL12_TAM.netDensity <- network.density(ADEL12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_TAM.entropy <- entropy(ADEL12_TAMft) #entropy

ADEL12_TAM.netMx <- cbind(ADEL12_TAM.netMx, ADEL12_TAM.clusterCoef, ADEL12_TAM.degreeCent$centralization,
                          ADEL12_TAM.netDensity, ADEL12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************
#NA

round = 12
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
ADEL12_SDMg2 <- data.frame(ADEL12_SDM)
ADEL12_SDMg2 <- ADEL12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_SDMg2$player1
player2vector <- ADEL12_SDMg2$player2
ADEL12_SDMg3 <- ADEL12_SDMg2
ADEL12_SDMg3$p1inp2vec <- is.element(ADEL12_SDMg3$player1, player2vector)
ADEL12_SDMg3$p2inp1vec <- is.element(ADEL12_SDMg3$player2, player1vector)

addPlayer1 <- ADEL12_SDMg3[ which(ADEL12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_SDMg3[ which(ADEL12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_SDMg2 <- rbind(ADEL12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
ADEL12_SDMft <- ftable(ADEL12_SDMg2$player1, ADEL12_SDMg2$player2)
ADEL12_SDMft2 <- as.matrix(ADEL12_SDMft)
numRows <- nrow(ADEL12_SDMft2)
numCols <- ncol(ADEL12_SDMft2)
ADEL12_SDMft3 <- ADEL12_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL12_SDMTable <- graph.adjacency(ADEL12_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(ADEL12_SDMTable, vertex.label = V(ADEL12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
ADEL12_SDM.clusterCoef <- transitivity(ADEL12_SDMTable, type="global") #cluster coefficient
ADEL12_SDM.degreeCent <- centralization.degree(ADEL12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_SDMftn <- as.network.matrix(ADEL12_SDMft)
ADEL12_SDM.netDensity <- network.density(ADEL12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_SDM.entropy <- entropy(ADEL12_SDMft) #entropy

ADEL12_SDM.netMx <- cbind(ADEL12_SDM.netMx, ADEL12_SDM.clusterCoef, ADEL12_SDM.degreeCent$centralization,
                          ADEL12_SDM.netDensity, ADEL12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
ADEL12_TDMg2 <- data.frame(ADEL12_TDM)
ADEL12_TDMg2 <- ADEL12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_TDMg2$player1
player2vector <- ADEL12_TDMg2$player2
ADEL12_TDMg3 <- ADEL12_TDMg2
ADEL12_TDMg3$p1inp2vec <- is.element(ADEL12_TDMg3$player1, player2vector)
ADEL12_TDMg3$p2inp1vec <- is.element(ADEL12_TDMg3$player2, player1vector)

addPlayer1 <- ADEL12_TDMg3[ which(ADEL12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_TDMg3[ which(ADEL12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_TDMg2 <- rbind(ADEL12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
ADEL12_TDMft <- ftable(ADEL12_TDMg2$player1, ADEL12_TDMg2$player2)
ADEL12_TDMft2 <- as.matrix(ADEL12_TDMft)
numRows <- nrow(ADEL12_TDMft2)
numCols <- ncol(ADEL12_TDMft2)
ADEL12_TDMft3 <- ADEL12_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL12_TDMTable <- graph.adjacency(ADEL12_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(ADEL12_TDMTable, vertex.label = V(ADEL12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
ADEL12_TDM.clusterCoef <- transitivity(ADEL12_TDMTable, type="global") #cluster coefficient
ADEL12_TDM.degreeCent <- centralization.degree(ADEL12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_TDMftn <- as.network.matrix(ADEL12_TDMft)
ADEL12_TDM.netDensity <- network.density(ADEL12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_TDM.entropy <- entropy(ADEL12_TDMft) #entropy

ADEL12_TDM.netMx <- cbind(ADEL12_TDM.netMx, ADEL12_TDM.clusterCoef, ADEL12_TDM.degreeCent$centralization,
                          ADEL12_TDM.netDensity, ADEL12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
ADEL12_SDg2 <- data.frame(ADEL12_SD)
ADEL12_SDg2 <- ADEL12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_SDg2$player1
player2vector <- ADEL12_SDg2$player2
ADEL12_SDg3 <- ADEL12_SDg2
ADEL12_SDg3$p1inp2vec <- is.element(ADEL12_SDg3$player1, player2vector)
ADEL12_SDg3$p2inp1vec <- is.element(ADEL12_SDg3$player2, player1vector)

addPlayer1 <- ADEL12_SDg3[ which(ADEL12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_SDg3[ which(ADEL12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_SDg2 <- rbind(ADEL12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
ADEL12_SDft <- ftable(ADEL12_SDg2$player1, ADEL12_SDg2$player2)
ADEL12_SDft2 <- as.matrix(ADEL12_SDft)
numRows <- nrow(ADEL12_SDft2)
numCols <- ncol(ADEL12_SDft2)
ADEL12_SDft3 <- ADEL12_SDft2[c(2:numRows) , c(2:numCols)]
ADEL12_SDTable <- graph.adjacency(ADEL12_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(ADEL12_SDTable, vertex.label = V(ADEL12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
ADEL12_SD.clusterCoef <- transitivity(ADEL12_SDTable, type="global") #cluster coefficient
ADEL12_SD.degreeCent <- centralization.degree(ADEL12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_SDftn <- as.network.matrix(ADEL12_SDft)
ADEL12_SD.netDensity <- network.density(ADEL12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_SD.entropy <- entropy(ADEL12_SDft) #entropy

ADEL12_SD.netMx <- cbind(ADEL12_SD.netMx, ADEL12_SD.clusterCoef, ADEL12_SD.degreeCent$centralization,
                         ADEL12_SD.netDensity, ADEL12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
ADEL12_TDg2 <- data.frame(ADEL12_TD)
ADEL12_TDg2 <- ADEL12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_TDg2$player1
player2vector <- ADEL12_TDg2$player2
ADEL12_TDg3 <- ADEL12_TDg2
ADEL12_TDg3$p1inp2vec <- is.element(ADEL12_TDg3$player1, player2vector)
ADEL12_TDg3$p2inp1vec <- is.element(ADEL12_TDg3$player2, player1vector)

addPlayer1 <- ADEL12_TDg3[ which(ADEL12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_TDg3[ which(ADEL12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_TDg2 <- rbind(ADEL12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
ADEL12_TDft <- ftable(ADEL12_TDg2$player1, ADEL12_TDg2$player2)
ADEL12_TDft2 <- as.matrix(ADEL12_TDft)
numRows <- nrow(ADEL12_TDft2)
numCols <- ncol(ADEL12_TDft2)
ADEL12_TDft3 <- ADEL12_TDft2[c(2:numRows) , c(2:numCols)]
ADEL12_TDTable <- graph.adjacency(ADEL12_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(ADEL12_TDTable, vertex.label = V(ADEL12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
ADEL12_TD.clusterCoef <- transitivity(ADEL12_TDTable, type="global") #cluster coefficient
ADEL12_TD.degreeCent <- centralization.degree(ADEL12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_TDftn <- as.network.matrix(ADEL12_TDft)
ADEL12_TD.netDensity <- network.density(ADEL12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_TD.entropy <- entropy(ADEL12_TDft) #entropy

ADEL12_TD.netMx <- cbind(ADEL12_TD.netMx, ADEL12_TD.clusterCoef, ADEL12_TD.degreeCent$centralization,
                         ADEL12_TD.netDensity, ADEL12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
ADEL12_QTg2 <- data.frame(ADEL12_QT)
ADEL12_QTg2 <- ADEL12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL12_QTg2$player1
player2vector <- ADEL12_QTg2$player2
ADEL12_QTg3 <- ADEL12_QTg2
ADEL12_QTg3$p1inp2vec <- is.element(ADEL12_QTg3$player1, player2vector)
ADEL12_QTg3$p2inp1vec <- is.element(ADEL12_QTg3$player2, player1vector)

addPlayer1 <- ADEL12_QTg3[ which(ADEL12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL12_QTg3[ which(ADEL12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL12_QTg2 <- rbind(ADEL12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
ADEL12_QTft <- ftable(ADEL12_QTg2$player1, ADEL12_QTg2$player2)
ADEL12_QTft2 <- as.matrix(ADEL12_QTft)
numRows <- nrow(ADEL12_QTft2)
numCols <- ncol(ADEL12_QTft2)
ADEL12_QTft3 <- ADEL12_QTft2[c(2:numRows) , c(2:numCols)]
ADEL12_QTTable <- graph.adjacency(ADEL12_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(ADEL12_QTTable, vertex.label = V(ADEL12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
ADEL12_QT.clusterCoef <- transitivity(ADEL12_QTTable, type="global") #cluster coefficient
ADEL12_QT.degreeCent <- centralization.degree(ADEL12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL12_QTftn <- as.network.matrix(ADEL12_QTft)
ADEL12_QT.netDensity <- network.density(ADEL12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL12_QT.entropy <- entropy(ADEL12_QTft) #entropy

ADEL12_QT.netMx <- cbind(ADEL12_QT.netMx, ADEL12_QT.clusterCoef, ADEL12_QT.degreeCent$centralization,
                         ADEL12_QT.netDensity, ADEL12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL12_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************
#NA

round = 12
teamName = "BL"
KIoutcome = "Goal_F"
BL12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
BL12_Gg2 <- data.frame(BL12_G)
BL12_Gg2 <- BL12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_Gg2$player1
player2vector <- BL12_Gg2$player2
BL12_Gg3 <- BL12_Gg2
BL12_Gg3$p1inp2vec <- is.element(BL12_Gg3$player1, player2vector)
BL12_Gg3$p2inp1vec <- is.element(BL12_Gg3$player2, player1vector)

addPlayer1 <- BL12_Gg3[ which(BL12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_Gg3[ which(BL12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_Gg2 <- rbind(BL12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
BL12_Gft <- ftable(BL12_Gg2$player1, BL12_Gg2$player2)
BL12_Gft2 <- as.matrix(BL12_Gft)
numRows <- nrow(BL12_Gft2)
numCols <- ncol(BL12_Gft2)
BL12_Gft3 <- BL12_Gft2[c(2:numRows) , c(2:numCols)]
BL12_GTable <- graph.adjacency(BL12_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(BL12_GTable, vertex.label = V(BL12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
BL12_G.clusterCoef <- transitivity(BL12_GTable, type="global") #cluster coefficient
BL12_G.degreeCent <- centralization.degree(BL12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_Gftn <- as.network.matrix(BL12_Gft)
BL12_G.netDensity <- network.density(BL12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_G.entropy <- entropy(BL12_Gft) #entropy

BL12_G.netMx <- cbind(BL12_G.netMx, BL12_G.clusterCoef, BL12_G.degreeCent$centralization,
                      BL12_G.netDensity, BL12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "BL"
KIoutcome = "Behind_F"
BL12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
BL12_Bg2 <- data.frame(BL12_B)
BL12_Bg2 <- BL12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_Bg2$player1
player2vector <- BL12_Bg2$player2
BL12_Bg3 <- BL12_Bg2
BL12_Bg3$p1inp2vec <- is.element(BL12_Bg3$player1, player2vector)
BL12_Bg3$p2inp1vec <- is.element(BL12_Bg3$player2, player1vector)

addPlayer1 <- BL12_Bg3[ which(BL12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_Bg3[ which(BL12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_Bg2 <- rbind(BL12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
BL12_Bft <- ftable(BL12_Bg2$player1, BL12_Bg2$player2)
BL12_Bft2 <- as.matrix(BL12_Bft)
numRows <- nrow(BL12_Bft2)
numCols <- ncol(BL12_Bft2)
BL12_Bft3 <- BL12_Bft2[c(2:numRows) , c(2:numCols)]
BL12_BTable <- graph.adjacency(BL12_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(BL12_BTable, vertex.label = V(BL12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
BL12_B.clusterCoef <- transitivity(BL12_BTable, type="global") #cluster coefficient
BL12_B.degreeCent <- centralization.degree(BL12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_Bftn <- as.network.matrix(BL12_Bft)
BL12_B.netDensity <- network.density(BL12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_B.entropy <- entropy(BL12_Bft) #entropy

BL12_B.netMx <- cbind(BL12_B.netMx, BL12_B.clusterCoef, BL12_B.degreeCent$centralization,
                      BL12_B.netDensity, BL12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************

round = 12
teamName = "BL"
KIoutcome = "Stoppage_F"
BL12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
BL12_SFg2 <- data.frame(BL12_SF)
BL12_SFg2 <- BL12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_SFg2$player1
player2vector <- BL12_SFg2$player2
BL12_SFg3 <- BL12_SFg2
BL12_SFg3$p1inp2vec <- is.element(BL12_SFg3$player1, player2vector)
BL12_SFg3$p2inp1vec <- is.element(BL12_SFg3$player2, player1vector)

addPlayer1 <- BL12_SFg3[ which(BL12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_SFg3[ which(BL12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_SFg2 <- rbind(BL12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
BL12_SFft <- ftable(BL12_SFg2$player1, BL12_SFg2$player2)
BL12_SFft2 <- as.matrix(BL12_SFft)
numRows <- nrow(BL12_SFft2)
numCols <- ncol(BL12_SFft2)
BL12_SFft3 <- BL12_SFft2[c(2:numRows) , c(2:numCols)]
BL12_SFTable <- graph.adjacency(BL12_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(BL12_SFTable, vertex.label = V(BL12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
BL12_SF.clusterCoef <- transitivity(BL12_SFTable, type="global") #cluster coefficient
BL12_SF.degreeCent <- centralization.degree(BL12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_SFftn <- as.network.matrix(BL12_SFft)
BL12_SF.netDensity <- network.density(BL12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_SF.entropy <- entropy(BL12_SFft) #entropy

BL12_SF.netMx <- cbind(BL12_SF.netMx, BL12_SF.clusterCoef, BL12_SF.degreeCent$centralization,
                       BL12_SF.netDensity, BL12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************
#NA

round = 12
teamName = "BL"
KIoutcome = "Turnover_F"
BL12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
BL12_TFg2 <- data.frame(BL12_TF)
BL12_TFg2 <- BL12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_TFg2$player1
player2vector <- BL12_TFg2$player2
BL12_TFg3 <- BL12_TFg2
BL12_TFg3$p1inp2vec <- is.element(BL12_TFg3$player1, player2vector)
BL12_TFg3$p2inp1vec <- is.element(BL12_TFg3$player2, player1vector)

addPlayer1 <- BL12_TFg3[ which(BL12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_TFg3[ which(BL12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_TFg2 <- rbind(BL12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
BL12_TFft <- ftable(BL12_TFg2$player1, BL12_TFg2$player2)
BL12_TFft2 <- as.matrix(BL12_TFft)
numRows <- nrow(BL12_TFft2)
numCols <- ncol(BL12_TFft2)
BL12_TFft3 <- BL12_TFft2[c(2:numRows) , c(2:numCols)]
BL12_TFTable <- graph.adjacency(BL12_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(BL12_TFTable, vertex.label = V(BL12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
BL12_TF.clusterCoef <- transitivity(BL12_TFTable, type="global") #cluster coefficient
BL12_TF.degreeCent <- centralization.degree(BL12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_TFftn <- as.network.matrix(BL12_TFft)
BL12_TF.netDensity <- network.density(BL12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_TF.entropy <- entropy(BL12_TFft) #entropy

BL12_TF.netMx <- cbind(BL12_TF.netMx, BL12_TF.clusterCoef, BL12_TF.degreeCent$centralization,
                       BL12_TF.netDensity, BL12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************

round = 12
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
BL12_SAMg2 <- data.frame(BL12_SAM)
BL12_SAMg2 <- BL12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_SAMg2$player1
player2vector <- BL12_SAMg2$player2
BL12_SAMg3 <- BL12_SAMg2
BL12_SAMg3$p1inp2vec <- is.element(BL12_SAMg3$player1, player2vector)
BL12_SAMg3$p2inp1vec <- is.element(BL12_SAMg3$player2, player1vector)

addPlayer1 <- BL12_SAMg3[ which(BL12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_SAMg3[ which(BL12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_SAMg2 <- rbind(BL12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
BL12_SAMft <- ftable(BL12_SAMg2$player1, BL12_SAMg2$player2)
BL12_SAMft2 <- as.matrix(BL12_SAMft)
numRows <- nrow(BL12_SAMft2)
numCols <- ncol(BL12_SAMft2)
BL12_SAMft3 <- BL12_SAMft2[c(2:numRows) , c(2:numCols)]
BL12_SAMTable <- graph.adjacency(BL12_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(BL12_SAMTable, vertex.label = V(BL12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
BL12_SAM.clusterCoef <- transitivity(BL12_SAMTable, type="global") #cluster coefficient
BL12_SAM.degreeCent <- centralization.degree(BL12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_SAMftn <- as.network.matrix(BL12_SAMft)
BL12_SAM.netDensity <- network.density(BL12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_SAM.entropy <- entropy(BL12_SAMft) #entropy

BL12_SAM.netMx <- cbind(BL12_SAM.netMx, BL12_SAM.clusterCoef, BL12_SAM.degreeCent$centralization,
                        BL12_SAM.netDensity, BL12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************

round = 12
teamName = "BL"
KIoutcome = "Turnover_AM"
BL12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
BL12_TAMg2 <- data.frame(BL12_TAM)
BL12_TAMg2 <- BL12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_TAMg2$player1
player2vector <- BL12_TAMg2$player2
BL12_TAMg3 <- BL12_TAMg2
BL12_TAMg3$p1inp2vec <- is.element(BL12_TAMg3$player1, player2vector)
BL12_TAMg3$p2inp1vec <- is.element(BL12_TAMg3$player2, player1vector)

addPlayer1 <- BL12_TAMg3[ which(BL12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_TAMg3[ which(BL12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_TAMg2 <- rbind(BL12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
BL12_TAMft <- ftable(BL12_TAMg2$player1, BL12_TAMg2$player2)
BL12_TAMft2 <- as.matrix(BL12_TAMft)
numRows <- nrow(BL12_TAMft2)
numCols <- ncol(BL12_TAMft2)
BL12_TAMft3 <- BL12_TAMft2[c(2:numRows) , c(2:numCols)]
BL12_TAMTable <- graph.adjacency(BL12_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(BL12_TAMTable, vertex.label = V(BL12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
BL12_TAM.clusterCoef <- transitivity(BL12_TAMTable, type="global") #cluster coefficient
BL12_TAM.degreeCent <- centralization.degree(BL12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_TAMftn <- as.network.matrix(BL12_TAMft)
BL12_TAM.netDensity <- network.density(BL12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_TAM.entropy <- entropy(BL12_TAMft) #entropy

BL12_TAM.netMx <- cbind(BL12_TAM.netMx, BL12_TAM.clusterCoef, BL12_TAM.degreeCent$centralization,
                        BL12_TAM.netDensity, BL12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************

round = 12
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
BL12_SDMg2 <- data.frame(BL12_SDM)
BL12_SDMg2 <- BL12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_SDMg2$player1
player2vector <- BL12_SDMg2$player2
BL12_SDMg3 <- BL12_SDMg2
BL12_SDMg3$p1inp2vec <- is.element(BL12_SDMg3$player1, player2vector)
BL12_SDMg3$p2inp1vec <- is.element(BL12_SDMg3$player2, player1vector)

addPlayer1 <- BL12_SDMg3[ which(BL12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_SDMg3[ which(BL12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_SDMg2 <- rbind(BL12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
BL12_SDMft <- ftable(BL12_SDMg2$player1, BL12_SDMg2$player2)
BL12_SDMft2 <- as.matrix(BL12_SDMft)
numRows <- nrow(BL12_SDMft2)
numCols <- ncol(BL12_SDMft2)
BL12_SDMft3 <- BL12_SDMft2[c(2:numRows) , c(2:numCols)]
BL12_SDMTable <- graph.adjacency(BL12_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(BL12_SDMTable, vertex.label = V(BL12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
BL12_SDM.clusterCoef <- transitivity(BL12_SDMTable, type="global") #cluster coefficient
BL12_SDM.degreeCent <- centralization.degree(BL12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_SDMftn <- as.network.matrix(BL12_SDMft)
BL12_SDM.netDensity <- network.density(BL12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_SDM.entropy <- entropy(BL12_SDMft) #entropy

BL12_SDM.netMx <- cbind(BL12_SDM.netMx, BL12_SDM.clusterCoef, BL12_SDM.degreeCent$centralization,
                        BL12_SDM.netDensity, BL12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "BL"
KIoutcome = "Turnover_DM"
BL12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
BL12_TDMg2 <- data.frame(BL12_TDM)
BL12_TDMg2 <- BL12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_TDMg2$player1
player2vector <- BL12_TDMg2$player2
BL12_TDMg3 <- BL12_TDMg2
BL12_TDMg3$p1inp2vec <- is.element(BL12_TDMg3$player1, player2vector)
BL12_TDMg3$p2inp1vec <- is.element(BL12_TDMg3$player2, player1vector)

addPlayer1 <- BL12_TDMg3[ which(BL12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_TDMg3[ which(BL12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_TDMg2 <- rbind(BL12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
BL12_TDMft <- ftable(BL12_TDMg2$player1, BL12_TDMg2$player2)
BL12_TDMft2 <- as.matrix(BL12_TDMft)
numRows <- nrow(BL12_TDMft2)
numCols <- ncol(BL12_TDMft2)
BL12_TDMft3 <- BL12_TDMft2[c(2:numRows) , c(2:numCols)]
BL12_TDMTable <- graph.adjacency(BL12_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(BL12_TDMTable, vertex.label = V(BL12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
BL12_TDM.clusterCoef <- transitivity(BL12_TDMTable, type="global") #cluster coefficient
BL12_TDM.degreeCent <- centralization.degree(BL12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_TDMftn <- as.network.matrix(BL12_TDMft)
BL12_TDM.netDensity <- network.density(BL12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_TDM.entropy <- entropy(BL12_TDMft) #entropy

BL12_TDM.netMx <- cbind(BL12_TDM.netMx, BL12_TDM.clusterCoef, BL12_TDM.degreeCent$centralization,
                        BL12_TDM.netDensity, BL12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "BL"
KIoutcome = "Stoppage_D"
BL12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
BL12_SDg2 <- data.frame(BL12_SD)
BL12_SDg2 <- BL12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_SDg2$player1
player2vector <- BL12_SDg2$player2
BL12_SDg3 <- BL12_SDg2
BL12_SDg3$p1inp2vec <- is.element(BL12_SDg3$player1, player2vector)
BL12_SDg3$p2inp1vec <- is.element(BL12_SDg3$player2, player1vector)

addPlayer1 <- BL12_SDg3[ which(BL12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_SDg3[ which(BL12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_SDg2 <- rbind(BL12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
BL12_SDft <- ftable(BL12_SDg2$player1, BL12_SDg2$player2)
BL12_SDft2 <- as.matrix(BL12_SDft)
numRows <- nrow(BL12_SDft2)
numCols <- ncol(BL12_SDft2)
BL12_SDft3 <- BL12_SDft2[c(2:numRows) , c(2:numCols)]
BL12_SDTable <- graph.adjacency(BL12_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(BL12_SDTable, vertex.label = V(BL12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
BL12_SD.clusterCoef <- transitivity(BL12_SDTable, type="global") #cluster coefficient
BL12_SD.degreeCent <- centralization.degree(BL12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_SDftn <- as.network.matrix(BL12_SDft)
BL12_SD.netDensity <- network.density(BL12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_SD.entropy <- entropy(BL12_SDft) #entropy

BL12_SD.netMx <- cbind(BL12_SD.netMx, BL12_SD.clusterCoef, BL12_SD.degreeCent$centralization,
                       BL12_SD.netDensity, BL12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "BL"
KIoutcome = "Turnover_D"
BL12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
BL12_TDg2 <- data.frame(BL12_TD)
BL12_TDg2 <- BL12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_TDg2$player1
player2vector <- BL12_TDg2$player2
BL12_TDg3 <- BL12_TDg2
BL12_TDg3$p1inp2vec <- is.element(BL12_TDg3$player1, player2vector)
BL12_TDg3$p2inp1vec <- is.element(BL12_TDg3$player2, player1vector)

addPlayer1 <- BL12_TDg3[ which(BL12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_TDg3[ which(BL12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_TDg2 <- rbind(BL12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
BL12_TDft <- ftable(BL12_TDg2$player1, BL12_TDg2$player2)
BL12_TDft2 <- as.matrix(BL12_TDft)
numRows <- nrow(BL12_TDft2)
numCols <- ncol(BL12_TDft2)
BL12_TDft3 <- BL12_TDft2[c(2:numRows) , c(2:numCols)]
BL12_TDTable <- graph.adjacency(BL12_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(BL12_TDTable, vertex.label = V(BL12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
BL12_TD.clusterCoef <- transitivity(BL12_TDTable, type="global") #cluster coefficient
BL12_TD.degreeCent <- centralization.degree(BL12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_TDftn <- as.network.matrix(BL12_TDft)
BL12_TD.netDensity <- network.density(BL12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_TD.entropy <- entropy(BL12_TDft) #entropy

BL12_TD.netMx <- cbind(BL12_TD.netMx, BL12_TD.clusterCoef, BL12_TD.degreeCent$centralization,
                       BL12_TD.netDensity, BL12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
BL12_QTg2 <- data.frame(BL12_QT)
BL12_QTg2 <- BL12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL12_QTg2$player1
player2vector <- BL12_QTg2$player2
BL12_QTg3 <- BL12_QTg2
BL12_QTg3$p1inp2vec <- is.element(BL12_QTg3$player1, player2vector)
BL12_QTg3$p2inp1vec <- is.element(BL12_QTg3$player2, player1vector)

addPlayer1 <- BL12_QTg3[ which(BL12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL12_QTg3[ which(BL12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL12_QTg2 <- rbind(BL12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
BL12_QTft <- ftable(BL12_QTg2$player1, BL12_QTg2$player2)
BL12_QTft2 <- as.matrix(BL12_QTft)
numRows <- nrow(BL12_QTft2)
numCols <- ncol(BL12_QTft2)
BL12_QTft3 <- BL12_QTft2[c(2:numRows) , c(2:numCols)]
BL12_QTTable <- graph.adjacency(BL12_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(BL12_QTTable, vertex.label = V(BL12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
BL12_QT.clusterCoef <- transitivity(BL12_QTTable, type="global") #cluster coefficient
BL12_QT.degreeCent <- centralization.degree(BL12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL12_QTftn <- as.network.matrix(BL12_QTft)
BL12_QT.netDensity <- network.density(BL12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL12_QT.entropy <- entropy(BL12_QTft) #entropy

BL12_QT.netMx <- cbind(BL12_QT.netMx, BL12_QT.clusterCoef, BL12_QT.degreeCent$centralization,
                       BL12_QT.netDensity, BL12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL12_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************
#NA

round = 12
teamName = "CARL"
KIoutcome = "Goal_F"
CARL12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
CARL12_Gg2 <- data.frame(CARL12_G)
CARL12_Gg2 <- CARL12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_Gg2$player1
player2vector <- CARL12_Gg2$player2
CARL12_Gg3 <- CARL12_Gg2
CARL12_Gg3$p1inp2vec <- is.element(CARL12_Gg3$player1, player2vector)
CARL12_Gg3$p2inp1vec <- is.element(CARL12_Gg3$player2, player1vector)

addPlayer1 <- CARL12_Gg3[ which(CARL12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL12_Gg3[ which(CARL12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_Gg2 <- rbind(CARL12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
CARL12_Gft <- ftable(CARL12_Gg2$player1, CARL12_Gg2$player2)
CARL12_Gft2 <- as.matrix(CARL12_Gft)
numRows <- nrow(CARL12_Gft2)
numCols <- ncol(CARL12_Gft2)
CARL12_Gft3 <- CARL12_Gft2[c(2:numRows) , c(2:numCols)]
CARL12_GTable <- graph.adjacency(CARL12_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(CARL12_GTable, vertex.label = V(CARL12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
CARL12_G.clusterCoef <- transitivity(CARL12_GTable, type="global") #cluster coefficient
CARL12_G.degreeCent <- centralization.degree(CARL12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_Gftn <- as.network.matrix(CARL12_Gft)
CARL12_G.netDensity <- network.density(CARL12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_G.entropy <- entropy(CARL12_Gft) #entropy

CARL12_G.netMx <- cbind(CARL12_G.netMx, CARL12_G.clusterCoef, CARL12_G.degreeCent$centralization,
                        CARL12_G.netDensity, CARL12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "CARL"
KIoutcome = "Behind_F"
CARL12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
CARL12_Bg2 <- data.frame(CARL12_B)
CARL12_Bg2 <- CARL12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_Bg2$player1
player2vector <- CARL12_Bg2$player2
CARL12_Bg3 <- CARL12_Bg2
CARL12_Bg3$p1inp2vec <- is.element(CARL12_Bg3$player1, player2vector)
CARL12_Bg3$p2inp1vec <- is.element(CARL12_Bg3$player2, player1vector)

addPlayer1 <- CARL12_Bg3[ which(CARL12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL12_Bg3[ which(CARL12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_Bg2 <- rbind(CARL12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
CARL12_Bft <- ftable(CARL12_Bg2$player1, CARL12_Bg2$player2)
CARL12_Bft2 <- as.matrix(CARL12_Bft)
numRows <- nrow(CARL12_Bft2)
numCols <- ncol(CARL12_Bft2)
CARL12_Bft3 <- CARL12_Bft2[c(2:numRows) , c(2:numCols)]
CARL12_BTable <- graph.adjacency(CARL12_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(CARL12_BTable, vertex.label = V(CARL12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
CARL12_B.clusterCoef <- transitivity(CARL12_BTable, type="global") #cluster coefficient
CARL12_B.degreeCent <- centralization.degree(CARL12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_Bftn <- as.network.matrix(CARL12_Bft)
CARL12_B.netDensity <- network.density(CARL12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_B.entropy <- entropy(CARL12_Bft) #entropy

CARL12_B.netMx <- cbind(CARL12_B.netMx, CARL12_B.clusterCoef, CARL12_B.degreeCent$centralization,
                        CARL12_B.netDensity, CARL12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************

round = 12
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
CARL12_SFg2 <- data.frame(CARL12_SF)
CARL12_SFg2 <- CARL12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_SFg2$player1
player2vector <- CARL12_SFg2$player2
CARL12_SFg3 <- CARL12_SFg2
CARL12_SFg3$p1inp2vec <- is.element(CARL12_SFg3$player1, player2vector)
CARL12_SFg3$p2inp1vec <- is.element(CARL12_SFg3$player2, player1vector)

addPlayer1 <- CARL12_SFg3[ which(CARL12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL12_SFg3[ which(CARL12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_SFg2 <- rbind(CARL12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
CARL12_SFft <- ftable(CARL12_SFg2$player1, CARL12_SFg2$player2)
CARL12_SFft2 <- as.matrix(CARL12_SFft)
numRows <- nrow(CARL12_SFft2)
numCols <- ncol(CARL12_SFft2)
CARL12_SFft3 <- CARL12_SFft2[c(2:numRows) , c(2:numCols)]
CARL12_SFTable <- graph.adjacency(CARL12_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(CARL12_SFTable, vertex.label = V(CARL12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
CARL12_SF.clusterCoef <- transitivity(CARL12_SFTable, type="global") #cluster coefficient
CARL12_SF.degreeCent <- centralization.degree(CARL12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_SFftn <- as.network.matrix(CARL12_SFft)
CARL12_SF.netDensity <- network.density(CARL12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_SF.entropy <- entropy(CARL12_SFft) #entropy

CARL12_SF.netMx <- cbind(CARL12_SF.netMx, CARL12_SF.clusterCoef, CARL12_SF.degreeCent$centralization,
                         CARL12_SF.netDensity, CARL12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************
#NA

round = 12
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
CARL12_TFg2 <- data.frame(CARL12_TF)
CARL12_TFg2 <- CARL12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_TFg2$player1
player2vector <- CARL12_TFg2$player2
CARL12_TFg3 <- CARL12_TFg2
CARL12_TFg3$p1inp2vec <- is.element(CARL12_TFg3$player1, player2vector)
CARL12_TFg3$p2inp1vec <- is.element(CARL12_TFg3$player2, player1vector)

addPlayer1 <- CARL12_TFg3[ which(CARL12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL12_TFg3[ which(CARL12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_TFg2 <- rbind(CARL12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
CARL12_TFft <- ftable(CARL12_TFg2$player1, CARL12_TFg2$player2)
CARL12_TFft2 <- as.matrix(CARL12_TFft)
numRows <- nrow(CARL12_TFft2)
numCols <- ncol(CARL12_TFft2)
CARL12_TFft3 <- CARL12_TFft2[c(2:numRows) , c(2:numCols)]
CARL12_TFTable <- graph.adjacency(CARL12_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(CARL12_TFTable, vertex.label = V(CARL12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
CARL12_TF.clusterCoef <- transitivity(CARL12_TFTable, type="global") #cluster coefficient
CARL12_TF.degreeCent <- centralization.degree(CARL12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_TFftn <- as.network.matrix(CARL12_TFft)
CARL12_TF.netDensity <- network.density(CARL12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_TF.entropy <- entropy(CARL12_TFft) #entropy

CARL12_TF.netMx <- cbind(CARL12_TF.netMx, CARL12_TF.clusterCoef, CARL12_TF.degreeCent$centralization,
                         CARL12_TF.netDensity, CARL12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************

round = 12
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
CARL12_SAMg2 <- data.frame(CARL12_SAM)
CARL12_SAMg2 <- CARL12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_SAMg2$player1
player2vector <- CARL12_SAMg2$player2
CARL12_SAMg3 <- CARL12_SAMg2
CARL12_SAMg3$p1inp2vec <- is.element(CARL12_SAMg3$player1, player2vector)
CARL12_SAMg3$p2inp1vec <- is.element(CARL12_SAMg3$player2, player1vector)

addPlayer1 <- CARL12_SAMg3[ which(CARL12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL12_SAMg3[ which(CARL12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_SAMg2 <- rbind(CARL12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
CARL12_SAMft <- ftable(CARL12_SAMg2$player1, CARL12_SAMg2$player2)
CARL12_SAMft2 <- as.matrix(CARL12_SAMft)
numRows <- nrow(CARL12_SAMft2)
numCols <- ncol(CARL12_SAMft2)
CARL12_SAMft3 <- CARL12_SAMft2[c(2:numRows) , c(2:numCols)]
CARL12_SAMTable <- graph.adjacency(CARL12_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(CARL12_SAMTable, vertex.label = V(CARL12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
CARL12_SAM.clusterCoef <- transitivity(CARL12_SAMTable, type="global") #cluster coefficient
CARL12_SAM.degreeCent <- centralization.degree(CARL12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_SAMftn <- as.network.matrix(CARL12_SAMft)
CARL12_SAM.netDensity <- network.density(CARL12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_SAM.entropy <- entropy(CARL12_SAMft) #entropy

CARL12_SAM.netMx <- cbind(CARL12_SAM.netMx, CARL12_SAM.clusterCoef, CARL12_SAM.degreeCent$centralization,
                          CARL12_SAM.netDensity, CARL12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************

round = 12
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
CARL12_TAMg2 <- data.frame(CARL12_TAM)
CARL12_TAMg2 <- CARL12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_TAMg2$player1
player2vector <- CARL12_TAMg2$player2
CARL12_TAMg3 <- CARL12_TAMg2
CARL12_TAMg3$p1inp2vec <- is.element(CARL12_TAMg3$player1, player2vector)
CARL12_TAMg3$p2inp1vec <- is.element(CARL12_TAMg3$player2, player1vector)

addPlayer1 <- CARL12_TAMg3[ which(CARL12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL12_TAMg3[ which(CARL12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_TAMg2 <- rbind(CARL12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
CARL12_TAMft <- ftable(CARL12_TAMg2$player1, CARL12_TAMg2$player2)
CARL12_TAMft2 <- as.matrix(CARL12_TAMft)
numRows <- nrow(CARL12_TAMft2)
numCols <- ncol(CARL12_TAMft2)
CARL12_TAMft3 <- CARL12_TAMft2[c(2:numRows) , c(2:numCols)]
CARL12_TAMTable <- graph.adjacency(CARL12_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(CARL12_TAMTable, vertex.label = V(CARL12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
CARL12_TAM.clusterCoef <- transitivity(CARL12_TAMTable, type="global") #cluster coefficient
CARL12_TAM.degreeCent <- centralization.degree(CARL12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_TAMftn <- as.network.matrix(CARL12_TAMft)
CARL12_TAM.netDensity <- network.density(CARL12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_TAM.entropy <- entropy(CARL12_TAMft) #entropy

CARL12_TAM.netMx <- cbind(CARL12_TAM.netMx, CARL12_TAM.clusterCoef, CARL12_TAM.degreeCent$centralization,
                          CARL12_TAM.netDensity, CARL12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************

round = 12
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
CARL12_SDMg2 <- data.frame(CARL12_SDM)
CARL12_SDMg2 <- CARL12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_SDMg2$player1
player2vector <- CARL12_SDMg2$player2
CARL12_SDMg3 <- CARL12_SDMg2
CARL12_SDMg3$p1inp2vec <- is.element(CARL12_SDMg3$player1, player2vector)
CARL12_SDMg3$p2inp1vec <- is.element(CARL12_SDMg3$player2, player1vector)

addPlayer1 <- CARL12_SDMg3[ which(CARL12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL12_SDMg3[ which(CARL12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_SDMg2 <- rbind(CARL12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
CARL12_SDMft <- ftable(CARL12_SDMg2$player1, CARL12_SDMg2$player2)
CARL12_SDMft2 <- as.matrix(CARL12_SDMft)
numRows <- nrow(CARL12_SDMft2)
numCols <- ncol(CARL12_SDMft2)
CARL12_SDMft3 <- CARL12_SDMft2[c(2:numRows) , c(2:numCols)]
CARL12_SDMTable <- graph.adjacency(CARL12_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(CARL12_SDMTable, vertex.label = V(CARL12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
CARL12_SDM.clusterCoef <- transitivity(CARL12_SDMTable, type="global") #cluster coefficient
CARL12_SDM.degreeCent <- centralization.degree(CARL12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_SDMftn <- as.network.matrix(CARL12_SDMft)
CARL12_SDM.netDensity <- network.density(CARL12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_SDM.entropy <- entropy(CARL12_SDMft) #entropy

CARL12_SDM.netMx <- cbind(CARL12_SDM.netMx, CARL12_SDM.clusterCoef, CARL12_SDM.degreeCent$centralization,
                          CARL12_SDM.netDensity, CARL12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
CARL12_TDMg2 <- data.frame(CARL12_TDM)
CARL12_TDMg2 <- CARL12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_TDMg2$player1
player2vector <- CARL12_TDMg2$player2
CARL12_TDMg3 <- CARL12_TDMg2
CARL12_TDMg3$p1inp2vec <- is.element(CARL12_TDMg3$player1, player2vector)
CARL12_TDMg3$p2inp1vec <- is.element(CARL12_TDMg3$player2, player1vector)

addPlayer1 <- CARL12_TDMg3[ which(CARL12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- CARL12_TDMg3[ which(CARL12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_TDMg2 <- rbind(CARL12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
CARL12_TDMft <- ftable(CARL12_TDMg2$player1, CARL12_TDMg2$player2)
CARL12_TDMft2 <- as.matrix(CARL12_TDMft)
numRows <- nrow(CARL12_TDMft2)
numCols <- ncol(CARL12_TDMft2)
CARL12_TDMft3 <- CARL12_TDMft2[c(2:numRows) , c(2:numCols)]
CARL12_TDMTable <- graph.adjacency(CARL12_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(CARL12_TDMTable, vertex.label = V(CARL12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
CARL12_TDM.clusterCoef <- transitivity(CARL12_TDMTable, type="global") #cluster coefficient
CARL12_TDM.degreeCent <- centralization.degree(CARL12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_TDMftn <- as.network.matrix(CARL12_TDMft)
CARL12_TDM.netDensity <- network.density(CARL12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_TDM.entropy <- entropy(CARL12_TDMft) #entropy

CARL12_TDM.netMx <- cbind(CARL12_TDM.netMx, CARL12_TDM.clusterCoef, CARL12_TDM.degreeCent$centralization,
                          CARL12_TDM.netDensity, CARL12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
CARL12_SDg2 <- data.frame(CARL12_SD)
CARL12_SDg2 <- CARL12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_SDg2$player1
player2vector <- CARL12_SDg2$player2
CARL12_SDg3 <- CARL12_SDg2
CARL12_SDg3$p1inp2vec <- is.element(CARL12_SDg3$player1, player2vector)
CARL12_SDg3$p2inp1vec <- is.element(CARL12_SDg3$player2, player1vector)

addPlayer1 <- CARL12_SDg3[ which(CARL12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL12_SDg3[ which(CARL12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_SDg2 <- rbind(CARL12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
CARL12_SDft <- ftable(CARL12_SDg2$player1, CARL12_SDg2$player2)
CARL12_SDft2 <- as.matrix(CARL12_SDft)
numRows <- nrow(CARL12_SDft2)
numCols <- ncol(CARL12_SDft2)
CARL12_SDft3 <- CARL12_SDft2[c(2:numRows) , c(2:numCols)]
CARL12_SDTable <- graph.adjacency(CARL12_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(CARL12_SDTable, vertex.label = V(CARL12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
CARL12_SD.clusterCoef <- transitivity(CARL12_SDTable, type="global") #cluster coefficient
CARL12_SD.degreeCent <- centralization.degree(CARL12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_SDftn <- as.network.matrix(CARL12_SDft)
CARL12_SD.netDensity <- network.density(CARL12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_SD.entropy <- entropy(CARL12_SDft) #entropy

CARL12_SD.netMx <- cbind(CARL12_SD.netMx, CARL12_SD.clusterCoef, CARL12_SD.degreeCent$centralization,
                         CARL12_SD.netDensity, CARL12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
CARL12_TDg2 <- data.frame(CARL12_TD)
CARL12_TDg2 <- CARL12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_TDg2$player1
player2vector <- CARL12_TDg2$player2
CARL12_TDg3 <- CARL12_TDg2
CARL12_TDg3$p1inp2vec <- is.element(CARL12_TDg3$player1, player2vector)
CARL12_TDg3$p2inp1vec <- is.element(CARL12_TDg3$player2, player1vector)

addPlayer1 <- CARL12_TDg3[ which(CARL12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL12_TDg3[ which(CARL12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_TDg2 <- rbind(CARL12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
CARL12_TDft <- ftable(CARL12_TDg2$player1, CARL12_TDg2$player2)
CARL12_TDft2 <- as.matrix(CARL12_TDft)
numRows <- nrow(CARL12_TDft2)
numCols <- ncol(CARL12_TDft2)
CARL12_TDft3 <- CARL12_TDft2[c(2:numRows) , c(2:numCols)]
CARL12_TDTable <- graph.adjacency(CARL12_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(CARL12_TDTable, vertex.label = V(CARL12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
CARL12_TD.clusterCoef <- transitivity(CARL12_TDTable, type="global") #cluster coefficient
CARL12_TD.degreeCent <- centralization.degree(CARL12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_TDftn <- as.network.matrix(CARL12_TDft)
CARL12_TD.netDensity <- network.density(CARL12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_TD.entropy <- entropy(CARL12_TDft) #entropy

CARL12_TD.netMx <- cbind(CARL12_TD.netMx, CARL12_TD.clusterCoef, CARL12_TD.degreeCent$centralization,
                         CARL12_TD.netDensity, CARL12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
CARL12_QTg2 <- data.frame(CARL12_QT)
CARL12_QTg2 <- CARL12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL12_QTg2$player1
player2vector <- CARL12_QTg2$player2
CARL12_QTg3 <- CARL12_QTg2
CARL12_QTg3$p1inp2vec <- is.element(CARL12_QTg3$player1, player2vector)
CARL12_QTg3$p2inp1vec <- is.element(CARL12_QTg3$player2, player1vector)

addPlayer1 <- CARL12_QTg3[ which(CARL12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL12_QTg3[ which(CARL12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL12_QTg2 <- rbind(CARL12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
CARL12_QTft <- ftable(CARL12_QTg2$player1, CARL12_QTg2$player2)
CARL12_QTft2 <- as.matrix(CARL12_QTft)
numRows <- nrow(CARL12_QTft2)
numCols <- ncol(CARL12_QTft2)
CARL12_QTft3 <- CARL12_QTft2[c(2:numRows) , c(2:numCols)]
CARL12_QTTable <- graph.adjacency(CARL12_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(CARL12_QTTable, vertex.label = V(CARL12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
CARL12_QT.clusterCoef <- transitivity(CARL12_QTTable, type="global") #cluster coefficient
CARL12_QT.degreeCent <- centralization.degree(CARL12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL12_QTftn <- as.network.matrix(CARL12_QTft)
CARL12_QT.netDensity <- network.density(CARL12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL12_QT.entropy <- entropy(CARL12_QTft) #entropy

CARL12_QT.netMx <- cbind(CARL12_QT.netMx, CARL12_QT.clusterCoef, CARL12_QT.degreeCent$centralization,
                         CARL12_QT.netDensity, CARL12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL12_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************

round = 12
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
GEEL12_Gg2 <- data.frame(GEEL12_G)
GEEL12_Gg2 <- GEEL12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_Gg2$player1
player2vector <- GEEL12_Gg2$player2
GEEL12_Gg3 <- GEEL12_Gg2
GEEL12_Gg3$p1inp2vec <- is.element(GEEL12_Gg3$player1, player2vector)
GEEL12_Gg3$p2inp1vec <- is.element(GEEL12_Gg3$player2, player1vector)

addPlayer1 <- GEEL12_Gg3[ which(GEEL12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GEEL12_Gg3[ which(GEEL12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_Gg2 <- rbind(GEEL12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
GEEL12_Gft <- ftable(GEEL12_Gg2$player1, GEEL12_Gg2$player2)
GEEL12_Gft2 <- as.matrix(GEEL12_Gft)
numRows <- nrow(GEEL12_Gft2)
numCols <- ncol(GEEL12_Gft2)
GEEL12_Gft3 <- GEEL12_Gft2[c(2:numRows) , c(2:numCols)]
GEEL12_GTable <- graph.adjacency(GEEL12_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(GEEL12_GTable, vertex.label = V(GEEL12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
GEEL12_G.clusterCoef <- transitivity(GEEL12_GTable, type="global") #cluster coefficient
GEEL12_G.degreeCent <- centralization.degree(GEEL12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_Gftn <- as.network.matrix(GEEL12_Gft)
GEEL12_G.netDensity <- network.density(GEEL12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_G.entropy <- entropy(GEEL12_Gft) #entropy

GEEL12_G.netMx <- cbind(GEEL12_G.netMx, GEEL12_G.clusterCoef, GEEL12_G.degreeCent$centralization,
                        GEEL12_G.netDensity, GEEL12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
GEEL12_Bg2 <- data.frame(GEEL12_B)
GEEL12_Bg2 <- GEEL12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_Bg2$player1
player2vector <- GEEL12_Bg2$player2
GEEL12_Bg3 <- GEEL12_Bg2
GEEL12_Bg3$p1inp2vec <- is.element(GEEL12_Bg3$player1, player2vector)
GEEL12_Bg3$p2inp1vec <- is.element(GEEL12_Bg3$player2, player1vector)

addPlayer1 <- GEEL12_Bg3[ which(GEEL12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_Bg3[ which(GEEL12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_Bg2 <- rbind(GEEL12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
GEEL12_Bft <- ftable(GEEL12_Bg2$player1, GEEL12_Bg2$player2)
GEEL12_Bft2 <- as.matrix(GEEL12_Bft)
numRows <- nrow(GEEL12_Bft2)
numCols <- ncol(GEEL12_Bft2)
GEEL12_Bft3 <- GEEL12_Bft2[c(2:numRows) , c(2:numCols)]
GEEL12_BTable <- graph.adjacency(GEEL12_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(GEEL12_BTable, vertex.label = V(GEEL12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
GEEL12_B.clusterCoef <- transitivity(GEEL12_BTable, type="global") #cluster coefficient
GEEL12_B.degreeCent <- centralization.degree(GEEL12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_Bftn <- as.network.matrix(GEEL12_Bft)
GEEL12_B.netDensity <- network.density(GEEL12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_B.entropy <- entropy(GEEL12_Bft) #entropy

GEEL12_B.netMx <- cbind(GEEL12_B.netMx, GEEL12_B.clusterCoef, GEEL12_B.degreeCent$centralization,
                        GEEL12_B.netDensity, GEEL12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
GEEL12_SFg2 <- data.frame(GEEL12_SF)
GEEL12_SFg2 <- GEEL12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_SFg2$player1
player2vector <- GEEL12_SFg2$player2
GEEL12_SFg3 <- GEEL12_SFg2
GEEL12_SFg3$p1inp2vec <- is.element(GEEL12_SFg3$player1, player2vector)
GEEL12_SFg3$p2inp1vec <- is.element(GEEL12_SFg3$player2, player1vector)

addPlayer1 <- GEEL12_SFg3[ which(GEEL12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_SFg3[ which(GEEL12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_SFg2 <- rbind(GEEL12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
GEEL12_SFft <- ftable(GEEL12_SFg2$player1, GEEL12_SFg2$player2)
GEEL12_SFft2 <- as.matrix(GEEL12_SFft)
numRows <- nrow(GEEL12_SFft2)
numCols <- ncol(GEEL12_SFft2)
GEEL12_SFft3 <- GEEL12_SFft2[c(2:numRows) , c(2:numCols)]
GEEL12_SFTable <- graph.adjacency(GEEL12_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(GEEL12_SFTable, vertex.label = V(GEEL12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
GEEL12_SF.clusterCoef <- transitivity(GEEL12_SFTable, type="global") #cluster coefficient
GEEL12_SF.degreeCent <- centralization.degree(GEEL12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_SFftn <- as.network.matrix(GEEL12_SFft)
GEEL12_SF.netDensity <- network.density(GEEL12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_SF.entropy <- entropy(GEEL12_SFft) #entropy

GEEL12_SF.netMx <- cbind(GEEL12_SF.netMx, GEEL12_SF.clusterCoef, GEEL12_SF.degreeCent$centralization,
                         GEEL12_SF.netDensity, GEEL12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
GEEL12_TFg2 <- data.frame(GEEL12_TF)
GEEL12_TFg2 <- GEEL12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_TFg2$player1
player2vector <- GEEL12_TFg2$player2
GEEL12_TFg3 <- GEEL12_TFg2
GEEL12_TFg3$p1inp2vec <- is.element(GEEL12_TFg3$player1, player2vector)
GEEL12_TFg3$p2inp1vec <- is.element(GEEL12_TFg3$player2, player1vector)

addPlayer1 <- GEEL12_TFg3[ which(GEEL12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_TFg3[ which(GEEL12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_TFg2 <- rbind(GEEL12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
GEEL12_TFft <- ftable(GEEL12_TFg2$player1, GEEL12_TFg2$player2)
GEEL12_TFft2 <- as.matrix(GEEL12_TFft)
numRows <- nrow(GEEL12_TFft2)
numCols <- ncol(GEEL12_TFft2)
GEEL12_TFft3 <- GEEL12_TFft2[c(2:numRows) , c(2:numCols)]
GEEL12_TFTable <- graph.adjacency(GEEL12_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(GEEL12_TFTable, vertex.label = V(GEEL12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
GEEL12_TF.clusterCoef <- transitivity(GEEL12_TFTable, type="global") #cluster coefficient
GEEL12_TF.degreeCent <- centralization.degree(GEEL12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_TFftn <- as.network.matrix(GEEL12_TFft)
GEEL12_TF.netDensity <- network.density(GEEL12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_TF.entropy <- entropy(GEEL12_TFft) #entropy

GEEL12_TF.netMx <- cbind(GEEL12_TF.netMx, GEEL12_TF.clusterCoef, GEEL12_TF.degreeCent$centralization,
                         GEEL12_TF.netDensity, GEEL12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
GEEL12_SAMg2 <- data.frame(GEEL12_SAM)
GEEL12_SAMg2 <- GEEL12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_SAMg2$player1
player2vector <- GEEL12_SAMg2$player2
GEEL12_SAMg3 <- GEEL12_SAMg2
GEEL12_SAMg3$p1inp2vec <- is.element(GEEL12_SAMg3$player1, player2vector)
GEEL12_SAMg3$p2inp1vec <- is.element(GEEL12_SAMg3$player2, player1vector)

addPlayer1 <- GEEL12_SAMg3[ which(GEEL12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_SAMg3[ which(GEEL12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_SAMg2 <- rbind(GEEL12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
GEEL12_SAMft <- ftable(GEEL12_SAMg2$player1, GEEL12_SAMg2$player2)
GEEL12_SAMft2 <- as.matrix(GEEL12_SAMft)
numRows <- nrow(GEEL12_SAMft2)
numCols <- ncol(GEEL12_SAMft2)
GEEL12_SAMft3 <- GEEL12_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL12_SAMTable <- graph.adjacency(GEEL12_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(GEEL12_SAMTable, vertex.label = V(GEEL12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
GEEL12_SAM.clusterCoef <- transitivity(GEEL12_SAMTable, type="global") #cluster coefficient
GEEL12_SAM.degreeCent <- centralization.degree(GEEL12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_SAMftn <- as.network.matrix(GEEL12_SAMft)
GEEL12_SAM.netDensity <- network.density(GEEL12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_SAM.entropy <- entropy(GEEL12_SAMft) #entropy

GEEL12_SAM.netMx <- cbind(GEEL12_SAM.netMx, GEEL12_SAM.clusterCoef, GEEL12_SAM.degreeCent$centralization,
                          GEEL12_SAM.netDensity, GEEL12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
GEEL12_TAMg2 <- data.frame(GEEL12_TAM)
GEEL12_TAMg2 <- GEEL12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_TAMg2$player1
player2vector <- GEEL12_TAMg2$player2
GEEL12_TAMg3 <- GEEL12_TAMg2
GEEL12_TAMg3$p1inp2vec <- is.element(GEEL12_TAMg3$player1, player2vector)
GEEL12_TAMg3$p2inp1vec <- is.element(GEEL12_TAMg3$player2, player1vector)

addPlayer1 <- GEEL12_TAMg3[ which(GEEL12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_TAMg3[ which(GEEL12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_TAMg2 <- rbind(GEEL12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
GEEL12_TAMft <- ftable(GEEL12_TAMg2$player1, GEEL12_TAMg2$player2)
GEEL12_TAMft2 <- as.matrix(GEEL12_TAMft)
numRows <- nrow(GEEL12_TAMft2)
numCols <- ncol(GEEL12_TAMft2)
GEEL12_TAMft3 <- GEEL12_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL12_TAMTable <- graph.adjacency(GEEL12_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(GEEL12_TAMTable, vertex.label = V(GEEL12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
GEEL12_TAM.clusterCoef <- transitivity(GEEL12_TAMTable, type="global") #cluster coefficient
GEEL12_TAM.degreeCent <- centralization.degree(GEEL12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_TAMftn <- as.network.matrix(GEEL12_TAMft)
GEEL12_TAM.netDensity <- network.density(GEEL12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_TAM.entropy <- entropy(GEEL12_TAMft) #entropy

GEEL12_TAM.netMx <- cbind(GEEL12_TAM.netMx, GEEL12_TAM.clusterCoef, GEEL12_TAM.degreeCent$centralization,
                          GEEL12_TAM.netDensity, GEEL12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
GEEL12_SDMg2 <- data.frame(GEEL12_SDM)
GEEL12_SDMg2 <- GEEL12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_SDMg2$player1
player2vector <- GEEL12_SDMg2$player2
GEEL12_SDMg3 <- GEEL12_SDMg2
GEEL12_SDMg3$p1inp2vec <- is.element(GEEL12_SDMg3$player1, player2vector)
GEEL12_SDMg3$p2inp1vec <- is.element(GEEL12_SDMg3$player2, player1vector)

addPlayer1 <- GEEL12_SDMg3[ which(GEEL12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_SDMg3[ which(GEEL12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_SDMg2 <- rbind(GEEL12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
GEEL12_SDMft <- ftable(GEEL12_SDMg2$player1, GEEL12_SDMg2$player2)
GEEL12_SDMft2 <- as.matrix(GEEL12_SDMft)
numRows <- nrow(GEEL12_SDMft2)
numCols <- ncol(GEEL12_SDMft2)
GEEL12_SDMft3 <- GEEL12_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL12_SDMTable <- graph.adjacency(GEEL12_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(GEEL12_SDMTable, vertex.label = V(GEEL12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
GEEL12_SDM.clusterCoef <- transitivity(GEEL12_SDMTable, type="global") #cluster coefficient
GEEL12_SDM.degreeCent <- centralization.degree(GEEL12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_SDMftn <- as.network.matrix(GEEL12_SDMft)
GEEL12_SDM.netDensity <- network.density(GEEL12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_SDM.entropy <- entropy(GEEL12_SDMft) #entropy

GEEL12_SDM.netMx <- cbind(GEEL12_SDM.netMx, GEEL12_SDM.clusterCoef, GEEL12_SDM.degreeCent$centralization,
                          GEEL12_SDM.netDensity, GEEL12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
GEEL12_TDMg2 <- data.frame(GEEL12_TDM)
GEEL12_TDMg2 <- GEEL12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_TDMg2$player1
player2vector <- GEEL12_TDMg2$player2
GEEL12_TDMg3 <- GEEL12_TDMg2
GEEL12_TDMg3$p1inp2vec <- is.element(GEEL12_TDMg3$player1, player2vector)
GEEL12_TDMg3$p2inp1vec <- is.element(GEEL12_TDMg3$player2, player1vector)

addPlayer1 <- GEEL12_TDMg3[ which(GEEL12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_TDMg3[ which(GEEL12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_TDMg2 <- rbind(GEEL12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
GEEL12_TDMft <- ftable(GEEL12_TDMg2$player1, GEEL12_TDMg2$player2)
GEEL12_TDMft2 <- as.matrix(GEEL12_TDMft)
numRows <- nrow(GEEL12_TDMft2)
numCols <- ncol(GEEL12_TDMft2)
GEEL12_TDMft3 <- GEEL12_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL12_TDMTable <- graph.adjacency(GEEL12_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(GEEL12_TDMTable, vertex.label = V(GEEL12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
GEEL12_TDM.clusterCoef <- transitivity(GEEL12_TDMTable, type="global") #cluster coefficient
GEEL12_TDM.degreeCent <- centralization.degree(GEEL12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_TDMftn <- as.network.matrix(GEEL12_TDMft)
GEEL12_TDM.netDensity <- network.density(GEEL12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_TDM.entropy <- entropy(GEEL12_TDMft) #entropy

GEEL12_TDM.netMx <- cbind(GEEL12_TDM.netMx, GEEL12_TDM.clusterCoef, GEEL12_TDM.degreeCent$centralization,
                          GEEL12_TDM.netDensity, GEEL12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
GEEL12_SDg2 <- data.frame(GEEL12_SD)
GEEL12_SDg2 <- GEEL12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_SDg2$player1
player2vector <- GEEL12_SDg2$player2
GEEL12_SDg3 <- GEEL12_SDg2
GEEL12_SDg3$p1inp2vec <- is.element(GEEL12_SDg3$player1, player2vector)
GEEL12_SDg3$p2inp1vec <- is.element(GEEL12_SDg3$player2, player1vector)

addPlayer1 <- GEEL12_SDg3[ which(GEEL12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_SDg3[ which(GEEL12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_SDg2 <- rbind(GEEL12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
GEEL12_SDft <- ftable(GEEL12_SDg2$player1, GEEL12_SDg2$player2)
GEEL12_SDft2 <- as.matrix(GEEL12_SDft)
numRows <- nrow(GEEL12_SDft2)
numCols <- ncol(GEEL12_SDft2)
GEEL12_SDft3 <- GEEL12_SDft2[c(2:numRows) , c(2:numCols)]
GEEL12_SDTable <- graph.adjacency(GEEL12_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(GEEL12_SDTable, vertex.label = V(GEEL12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
GEEL12_SD.clusterCoef <- transitivity(GEEL12_SDTable, type="global") #cluster coefficient
GEEL12_SD.degreeCent <- centralization.degree(GEEL12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_SDftn <- as.network.matrix(GEEL12_SDft)
GEEL12_SD.netDensity <- network.density(GEEL12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_SD.entropy <- entropy(GEEL12_SDft) #entropy

GEEL12_SD.netMx <- cbind(GEEL12_SD.netMx, GEEL12_SD.clusterCoef, GEEL12_SD.degreeCent$centralization,
                         GEEL12_SD.netDensity, GEEL12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
GEEL12_TDg2 <- data.frame(GEEL12_TD)
GEEL12_TDg2 <- GEEL12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_TDg2$player1
player2vector <- GEEL12_TDg2$player2
GEEL12_TDg3 <- GEEL12_TDg2
GEEL12_TDg3$p1inp2vec <- is.element(GEEL12_TDg3$player1, player2vector)
GEEL12_TDg3$p2inp1vec <- is.element(GEEL12_TDg3$player2, player1vector)

addPlayer1 <- GEEL12_TDg3[ which(GEEL12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_TDg3[ which(GEEL12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_TDg2 <- rbind(GEEL12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
GEEL12_TDft <- ftable(GEEL12_TDg2$player1, GEEL12_TDg2$player2)
GEEL12_TDft2 <- as.matrix(GEEL12_TDft)
numRows <- nrow(GEEL12_TDft2)
numCols <- ncol(GEEL12_TDft2)
GEEL12_TDft3 <- GEEL12_TDft2[c(2:numRows) , c(2:numCols)]
GEEL12_TDTable <- graph.adjacency(GEEL12_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(GEEL12_TDTable, vertex.label = V(GEEL12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
GEEL12_TD.clusterCoef <- transitivity(GEEL12_TDTable, type="global") #cluster coefficient
GEEL12_TD.degreeCent <- centralization.degree(GEEL12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_TDftn <- as.network.matrix(GEEL12_TDft)
GEEL12_TD.netDensity <- network.density(GEEL12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_TD.entropy <- entropy(GEEL12_TDft) #entropy

GEEL12_TD.netMx <- cbind(GEEL12_TD.netMx, GEEL12_TD.clusterCoef, GEEL12_TD.degreeCent$centralization,
                         GEEL12_TD.netDensity, GEEL12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
GEEL12_QTg2 <- data.frame(GEEL12_QT)
GEEL12_QTg2 <- GEEL12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL12_QTg2$player1
player2vector <- GEEL12_QTg2$player2
GEEL12_QTg3 <- GEEL12_QTg2
GEEL12_QTg3$p1inp2vec <- is.element(GEEL12_QTg3$player1, player2vector)
GEEL12_QTg3$p2inp1vec <- is.element(GEEL12_QTg3$player2, player1vector)

addPlayer1 <- GEEL12_QTg3[ which(GEEL12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL12_QTg3[ which(GEEL12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL12_QTg2 <- rbind(GEEL12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
GEEL12_QTft <- ftable(GEEL12_QTg2$player1, GEEL12_QTg2$player2)
GEEL12_QTft2 <- as.matrix(GEEL12_QTft)
numRows <- nrow(GEEL12_QTft2)
numCols <- ncol(GEEL12_QTft2)
GEEL12_QTft3 <- GEEL12_QTft2[c(2:numRows) , c(2:numCols)]
GEEL12_QTTable <- graph.adjacency(GEEL12_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(GEEL12_QTTable, vertex.label = V(GEEL12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
GEEL12_QT.clusterCoef <- transitivity(GEEL12_QTTable, type="global") #cluster coefficient
GEEL12_QT.degreeCent <- centralization.degree(GEEL12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL12_QTftn <- as.network.matrix(GEEL12_QTft)
GEEL12_QT.netDensity <- network.density(GEEL12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL12_QT.entropy <- entropy(GEEL12_QTft) #entropy

GEEL12_QT.netMx <- cbind(GEEL12_QT.netMx, GEEL12_QT.clusterCoef, GEEL12_QT.degreeCent$centralization,
                         GEEL12_QT.netDensity, GEEL12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL12_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************
#NA

round = 12
teamName = "GWS"
KIoutcome = "Goal_F"
GWS12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
GWS12_Gg2 <- data.frame(GWS12_G)
GWS12_Gg2 <- GWS12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_Gg2$player1
player2vector <- GWS12_Gg2$player2
GWS12_Gg3 <- GWS12_Gg2
GWS12_Gg3$p1inp2vec <- is.element(GWS12_Gg3$player1, player2vector)
GWS12_Gg3$p2inp1vec <- is.element(GWS12_Gg3$player2, player1vector)

addPlayer1 <- GWS12_Gg3[ which(GWS12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS12_Gg3[ which(GWS12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_Gg2 <- rbind(GWS12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
GWS12_Gft <- ftable(GWS12_Gg2$player1, GWS12_Gg2$player2)
GWS12_Gft2 <- as.matrix(GWS12_Gft)
numRows <- nrow(GWS12_Gft2)
numCols <- ncol(GWS12_Gft2)
GWS12_Gft3 <- GWS12_Gft2[c(1:numRows) , c(1:numCols)]
GWS12_GTable <- graph.adjacency(GWS12_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(GWS12_GTable, vertex.label = V(GWS12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
GWS12_G.clusterCoef <- transitivity(GWS12_GTable, type="global") #cluster coefficient
GWS12_G.degreeCent <- centralization.degree(GWS12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_Gftn <- as.network.matrix(GWS12_Gft)
GWS12_G.netDensity <- network.density(GWS12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_G.entropy <- entropy(GWS12_Gft) #entropy

GWS12_G.netMx <- cbind(GWS12_G.netMx, GWS12_G.clusterCoef, GWS12_G.degreeCent$centralization,
                       GWS12_G.netDensity, GWS12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "GWS"
KIoutcome = "Behind_F"
GWS12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
GWS12_Bg2 <- data.frame(GWS12_B)
GWS12_Bg2 <- GWS12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_Bg2$player1
player2vector <- GWS12_Bg2$player2
GWS12_Bg3 <- GWS12_Bg2
GWS12_Bg3$p1inp2vec <- is.element(GWS12_Bg3$player1, player2vector)
GWS12_Bg3$p2inp1vec <- is.element(GWS12_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS12_Bg3[ which(GWS12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS12_Bg2 <- rbind(GWS12_Bg2, addPlayer2)

#ROUND 12, Behind graph using weighted edges
GWS12_Bft <- ftable(GWS12_Bg2$player1, GWS12_Bg2$player2)
GWS12_Bft2 <- as.matrix(GWS12_Bft)
numRows <- nrow(GWS12_Bft2)
numCols <- ncol(GWS12_Bft2)
GWS12_Bft3 <- GWS12_Bft2[c(1:numRows) , c(2:numCols)]
GWS12_BTable <- graph.adjacency(GWS12_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(GWS12_BTable, vertex.label = V(GWS12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
GWS12_B.clusterCoef <- transitivity(GWS12_BTable, type="global") #cluster coefficient
GWS12_B.degreeCent <- centralization.degree(GWS12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_Bftn <- as.network.matrix(GWS12_Bft)
GWS12_B.netDensity <- network.density(GWS12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_B.entropy <- entropy(GWS12_Bft) #entropy

GWS12_B.netMx <- cbind(GWS12_B.netMx, GWS12_B.clusterCoef, GWS12_B.degreeCent$centralization,
                       GWS12_B.netDensity, GWS12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************
#NA

round = 12
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
GWS12_SFg2 <- data.frame(GWS12_SF)
GWS12_SFg2 <- GWS12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_SFg2$player1
player2vector <- GWS12_SFg2$player2
GWS12_SFg3 <- GWS12_SFg2
GWS12_SFg3$p1inp2vec <- is.element(GWS12_SFg3$player1, player2vector)
GWS12_SFg3$p2inp1vec <- is.element(GWS12_SFg3$player2, player1vector)

addPlayer1 <- GWS12_SFg3[ which(GWS12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS12_SFg3[ which(GWS12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_SFg2 <- rbind(GWS12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
GWS12_SFft <- ftable(GWS12_SFg2$player1, GWS12_SFg2$player2)
GWS12_SFft2 <- as.matrix(GWS12_SFft)
numRows <- nrow(GWS12_SFft2)
numCols <- ncol(GWS12_SFft2)
GWS12_SFft3 <- GWS12_SFft2[c(2:numRows) , c(2:numCols)]
GWS12_SFTable <- graph.adjacency(GWS12_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(GWS12_SFTable, vertex.label = V(GWS12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
GWS12_SF.clusterCoef <- transitivity(GWS12_SFTable, type="global") #cluster coefficient
GWS12_SF.degreeCent <- centralization.degree(GWS12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_SFftn <- as.network.matrix(GWS12_SFft)
GWS12_SF.netDensity <- network.density(GWS12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_SF.entropy <- entropy(GWS12_SFft) #entropy

GWS12_SF.netMx <- cbind(GWS12_SF.netMx, GWS12_SF.clusterCoef, GWS12_SF.degreeCent$centralization,
                        GWS12_SF.netDensity, GWS12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************

round = 12
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
GWS12_TFg2 <- data.frame(GWS12_TF)
GWS12_TFg2 <- GWS12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_TFg2$player1
player2vector <- GWS12_TFg2$player2
GWS12_TFg3 <- GWS12_TFg2
GWS12_TFg3$p1inp2vec <- is.element(GWS12_TFg3$player1, player2vector)
GWS12_TFg3$p2inp1vec <- is.element(GWS12_TFg3$player2, player1vector)

addPlayer1 <- GWS12_TFg3[ which(GWS12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS12_TFg3[ which(GWS12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_TFg2 <- rbind(GWS12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
GWS12_TFft <- ftable(GWS12_TFg2$player1, GWS12_TFg2$player2)
GWS12_TFft2 <- as.matrix(GWS12_TFft)
numRows <- nrow(GWS12_TFft2)
numCols <- ncol(GWS12_TFft2)
GWS12_TFft3 <- GWS12_TFft2[c(2:numRows) , c(2:numCols)]
GWS12_TFTable <- graph.adjacency(GWS12_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(GWS12_TFTable, vertex.label = V(GWS12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
GWS12_TF.clusterCoef <- transitivity(GWS12_TFTable, type="global") #cluster coefficient
GWS12_TF.degreeCent <- centralization.degree(GWS12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_TFftn <- as.network.matrix(GWS12_TFft)
GWS12_TF.netDensity <- network.density(GWS12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_TF.entropy <- entropy(GWS12_TFft) #entropy

GWS12_TF.netMx <- cbind(GWS12_TF.netMx, GWS12_TF.clusterCoef, GWS12_TF.degreeCent$centralization,
                        GWS12_TF.netDensity, GWS12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************

round = 12
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
GWS12_SAMg2 <- data.frame(GWS12_SAM)
GWS12_SAMg2 <- GWS12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_SAMg2$player1
player2vector <- GWS12_SAMg2$player2
GWS12_SAMg3 <- GWS12_SAMg2
GWS12_SAMg3$p1inp2vec <- is.element(GWS12_SAMg3$player1, player2vector)
GWS12_SAMg3$p2inp1vec <- is.element(GWS12_SAMg3$player2, player1vector)

addPlayer1 <- GWS12_SAMg3[ which(GWS12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- GWS12_SAMg3[ which(GWS12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_SAMg2 <- rbind(GWS12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
GWS12_SAMft <- ftable(GWS12_SAMg2$player1, GWS12_SAMg2$player2)
GWS12_SAMft2 <- as.matrix(GWS12_SAMft)
numRows <- nrow(GWS12_SAMft2)
numCols <- ncol(GWS12_SAMft2)
GWS12_SAMft3 <- GWS12_SAMft2[c(2:numRows) , c(2:numCols)]
GWS12_SAMTable <- graph.adjacency(GWS12_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(GWS12_SAMTable, vertex.label = V(GWS12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
GWS12_SAM.clusterCoef <- transitivity(GWS12_SAMTable, type="global") #cluster coefficient
GWS12_SAM.degreeCent <- centralization.degree(GWS12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_SAMftn <- as.network.matrix(GWS12_SAMft)
GWS12_SAM.netDensity <- network.density(GWS12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_SAM.entropy <- entropy(GWS12_SAMft) #entropy

GWS12_SAM.netMx <- cbind(GWS12_SAM.netMx, GWS12_SAM.clusterCoef, GWS12_SAM.degreeCent$centralization,
                         GWS12_SAM.netDensity, GWS12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************

round = 12
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
GWS12_TAMg2 <- data.frame(GWS12_TAM)
GWS12_TAMg2 <- GWS12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_TAMg2$player1
player2vector <- GWS12_TAMg2$player2
GWS12_TAMg3 <- GWS12_TAMg2
GWS12_TAMg3$p1inp2vec <- is.element(GWS12_TAMg3$player1, player2vector)
GWS12_TAMg3$p2inp1vec <- is.element(GWS12_TAMg3$player2, player1vector)

addPlayer1 <- GWS12_TAMg3[ which(GWS12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS12_TAMg3[ which(GWS12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_TAMg2 <- rbind(GWS12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
GWS12_TAMft <- ftable(GWS12_TAMg2$player1, GWS12_TAMg2$player2)
GWS12_TAMft2 <- as.matrix(GWS12_TAMft)
numRows <- nrow(GWS12_TAMft2)
numCols <- ncol(GWS12_TAMft2)
GWS12_TAMft3 <- GWS12_TAMft2[c(2:numRows) , c(2:numCols)]
GWS12_TAMTable <- graph.adjacency(GWS12_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(GWS12_TAMTable, vertex.label = V(GWS12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
GWS12_TAM.clusterCoef <- transitivity(GWS12_TAMTable, type="global") #cluster coefficient
GWS12_TAM.degreeCent <- centralization.degree(GWS12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_TAMftn <- as.network.matrix(GWS12_TAMft)
GWS12_TAM.netDensity <- network.density(GWS12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_TAM.entropy <- entropy(GWS12_TAMft) #entropy

GWS12_TAM.netMx <- cbind(GWS12_TAM.netMx, GWS12_TAM.clusterCoef, GWS12_TAM.degreeCent$centralization,
                         GWS12_TAM.netDensity, GWS12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************
#NA
round = 12
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
GWS12_SDMg2 <- data.frame(GWS12_SDM)
GWS12_SDMg2 <- GWS12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_SDMg2$player1
player2vector <- GWS12_SDMg2$player2
GWS12_SDMg3 <- GWS12_SDMg2
GWS12_SDMg3$p1inp2vec <- is.element(GWS12_SDMg3$player1, player2vector)
GWS12_SDMg3$p2inp1vec <- is.element(GWS12_SDMg3$player2, player1vector)

addPlayer1 <- GWS12_SDMg3[ which(GWS12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS12_SDMg3[ which(GWS12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_SDMg2 <- rbind(GWS12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
GWS12_SDMft <- ftable(GWS12_SDMg2$player1, GWS12_SDMg2$player2)
GWS12_SDMft2 <- as.matrix(GWS12_SDMft)
numRows <- nrow(GWS12_SDMft2)
numCols <- ncol(GWS12_SDMft2)
GWS12_SDMft3 <- GWS12_SDMft2[c(2:numRows) , c(2:numCols)]
GWS12_SDMTable <- graph.adjacency(GWS12_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(GWS12_SDMTable, vertex.label = V(GWS12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
GWS12_SDM.clusterCoef <- transitivity(GWS12_SDMTable, type="global") #cluster coefficient
GWS12_SDM.degreeCent <- centralization.degree(GWS12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_SDMftn <- as.network.matrix(GWS12_SDMft)
GWS12_SDM.netDensity <- network.density(GWS12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_SDM.entropy <- entropy(GWS12_SDMft) #entropy

GWS12_SDM.netMx <- cbind(GWS12_SDM.netMx, GWS12_SDM.clusterCoef, GWS12_SDM.degreeCent$centralization,
                         GWS12_SDM.netDensity, GWS12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
GWS12_TDMg2 <- data.frame(GWS12_TDM)
GWS12_TDMg2 <- GWS12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_TDMg2$player1
player2vector <- GWS12_TDMg2$player2
GWS12_TDMg3 <- GWS12_TDMg2
GWS12_TDMg3$p1inp2vec <- is.element(GWS12_TDMg3$player1, player2vector)
GWS12_TDMg3$p2inp1vec <- is.element(GWS12_TDMg3$player2, player1vector)

addPlayer1 <- GWS12_TDMg3[ which(GWS12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS12_TDMg3[ which(GWS12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_TDMg2 <- rbind(GWS12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
GWS12_TDMft <- ftable(GWS12_TDMg2$player1, GWS12_TDMg2$player2)
GWS12_TDMft2 <- as.matrix(GWS12_TDMft)
numRows <- nrow(GWS12_TDMft2)
numCols <- ncol(GWS12_TDMft2)
GWS12_TDMft3 <- GWS12_TDMft2[c(2:numRows) , c(2:numCols)]
GWS12_TDMTable <- graph.adjacency(GWS12_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(GWS12_TDMTable, vertex.label = V(GWS12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
GWS12_TDM.clusterCoef <- transitivity(GWS12_TDMTable, type="global") #cluster coefficient
GWS12_TDM.degreeCent <- centralization.degree(GWS12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_TDMftn <- as.network.matrix(GWS12_TDMft)
GWS12_TDM.netDensity <- network.density(GWS12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_TDM.entropy <- entropy(GWS12_TDMft) #entropy

GWS12_TDM.netMx <- cbind(GWS12_TDM.netMx, GWS12_TDM.clusterCoef, GWS12_TDM.degreeCent$centralization,
                         GWS12_TDM.netDensity, GWS12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
GWS12_SDg2 <- data.frame(GWS12_SD)
GWS12_SDg2 <- GWS12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_SDg2$player1
player2vector <- GWS12_SDg2$player2
GWS12_SDg3 <- GWS12_SDg2
GWS12_SDg3$p1inp2vec <- is.element(GWS12_SDg3$player1, player2vector)
GWS12_SDg3$p2inp1vec <- is.element(GWS12_SDg3$player2, player1vector)

addPlayer1 <- GWS12_SDg3[ which(GWS12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS12_SDg3[ which(GWS12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_SDg2 <- rbind(GWS12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
GWS12_SDft <- ftable(GWS12_SDg2$player1, GWS12_SDg2$player2)
GWS12_SDft2 <- as.matrix(GWS12_SDft)
numRows <- nrow(GWS12_SDft2)
numCols <- ncol(GWS12_SDft2)
GWS12_SDft3 <- GWS12_SDft2[c(2:numRows) , c(2:numCols)]
GWS12_SDTable <- graph.adjacency(GWS12_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(GWS12_SDTable, vertex.label = V(GWS12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
GWS12_SD.clusterCoef <- transitivity(GWS12_SDTable, type="global") #cluster coefficient
GWS12_SD.degreeCent <- centralization.degree(GWS12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_SDftn <- as.network.matrix(GWS12_SDft)
GWS12_SD.netDensity <- network.density(GWS12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_SD.entropy <- entropy(GWS12_SDft) #entropy

GWS12_SD.netMx <- cbind(GWS12_SD.netMx, GWS12_SD.clusterCoef, GWS12_SD.degreeCent$centralization,
                        GWS12_SD.netDensity, GWS12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************

round = 12
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
GWS12_TDg2 <- data.frame(GWS12_TD)
GWS12_TDg2 <- GWS12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_TDg2$player1
player2vector <- GWS12_TDg2$player2
GWS12_TDg3 <- GWS12_TDg2
GWS12_TDg3$p1inp2vec <- is.element(GWS12_TDg3$player1, player2vector)
GWS12_TDg3$p2inp1vec <- is.element(GWS12_TDg3$player2, player1vector)

addPlayer1 <- GWS12_TDg3[ which(GWS12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS12_TDg3[ which(GWS12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_TDg2 <- rbind(GWS12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
GWS12_TDft <- ftable(GWS12_TDg2$player1, GWS12_TDg2$player2)
GWS12_TDft2 <- as.matrix(GWS12_TDft)
numRows <- nrow(GWS12_TDft2)
numCols <- ncol(GWS12_TDft2)
GWS12_TDft3 <- GWS12_TDft2[c(2:numRows) , c(2:numCols)]
GWS12_TDTable <- graph.adjacency(GWS12_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(GWS12_TDTable, vertex.label = V(GWS12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
GWS12_TD.clusterCoef <- transitivity(GWS12_TDTable, type="global") #cluster coefficient
GWS12_TD.degreeCent <- centralization.degree(GWS12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_TDftn <- as.network.matrix(GWS12_TDft)
GWS12_TD.netDensity <- network.density(GWS12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_TD.entropy <- entropy(GWS12_TDft) #entropy

GWS12_TD.netMx <- cbind(GWS12_TD.netMx, GWS12_TD.clusterCoef, GWS12_TD.degreeCent$centralization,
                        GWS12_TD.netDensity, GWS12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
GWS12_QTg2 <- data.frame(GWS12_QT)
GWS12_QTg2 <- GWS12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS12_QTg2$player1
player2vector <- GWS12_QTg2$player2
GWS12_QTg3 <- GWS12_QTg2
GWS12_QTg3$p1inp2vec <- is.element(GWS12_QTg3$player1, player2vector)
GWS12_QTg3$p2inp1vec <- is.element(GWS12_QTg3$player2, player1vector)

addPlayer1 <- GWS12_QTg3[ which(GWS12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS12_QTg3[ which(GWS12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS12_QTg2 <- rbind(GWS12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
GWS12_QTft <- ftable(GWS12_QTg2$player1, GWS12_QTg2$player2)
GWS12_QTft2 <- as.matrix(GWS12_QTft)
numRows <- nrow(GWS12_QTft2)
numCols <- ncol(GWS12_QTft2)
GWS12_QTft3 <- GWS12_QTft2[c(2:numRows) , c(2:numCols)]
GWS12_QTTable <- graph.adjacency(GWS12_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(GWS12_QTTable, vertex.label = V(GWS12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
GWS12_QT.clusterCoef <- transitivity(GWS12_QTTable, type="global") #cluster coefficient
GWS12_QT.degreeCent <- centralization.degree(GWS12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS12_QTftn <- as.network.matrix(GWS12_QTft)
GWS12_QT.netDensity <- network.density(GWS12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS12_QT.entropy <- entropy(GWS12_QTft) #entropy

GWS12_QT.netMx <- cbind(GWS12_QT.netMx, GWS12_QT.clusterCoef, GWS12_QT.degreeCent$centralization,
                        GWS12_QT.netDensity, GWS12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS12_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************
#NA

round = 12
teamName = "HAW"
KIoutcome = "Goal_F"
HAW12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
HAW12_Gg2 <- data.frame(HAW12_G)
HAW12_Gg2 <- HAW12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_Gg2$player1
player2vector <- HAW12_Gg2$player2
HAW12_Gg3 <- HAW12_Gg2
HAW12_Gg3$p1inp2vec <- is.element(HAW12_Gg3$player1, player2vector)
HAW12_Gg3$p2inp1vec <- is.element(HAW12_Gg3$player2, player1vector)

addPlayer1 <- HAW12_Gg3[ which(HAW12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_Gg3[ which(HAW12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_Gg2 <- rbind(HAW12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
HAW12_Gft <- ftable(HAW12_Gg2$player1, HAW12_Gg2$player2)
HAW12_Gft2 <- as.matrix(HAW12_Gft)
numRows <- nrow(HAW12_Gft2)
numCols <- ncol(HAW12_Gft2)
HAW12_Gft3 <- HAW12_Gft2[c(2:numRows) , c(2:numCols)]
HAW12_GTable <- graph.adjacency(HAW12_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(HAW12_GTable, vertex.label = V(HAW12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
HAW12_G.clusterCoef <- transitivity(HAW12_GTable, type="global") #cluster coefficient
HAW12_G.degreeCent <- centralization.degree(HAW12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_Gftn <- as.network.matrix(HAW12_Gft)
HAW12_G.netDensity <- network.density(HAW12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_G.entropy <- entropy(HAW12_Gft) #entropy

HAW12_G.netMx <- cbind(HAW12_G.netMx, HAW12_G.clusterCoef, HAW12_G.degreeCent$centralization,
                       HAW12_G.netDensity, HAW12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "HAW"
KIoutcome = "Behind_F"
HAW12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
HAW12_Bg2 <- data.frame(HAW12_B)
HAW12_Bg2 <- HAW12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_Bg2$player1
player2vector <- HAW12_Bg2$player2
HAW12_Bg3 <- HAW12_Bg2
HAW12_Bg3$p1inp2vec <- is.element(HAW12_Bg3$player1, player2vector)
HAW12_Bg3$p2inp1vec <- is.element(HAW12_Bg3$player2, player1vector)

addPlayer1 <- HAW12_Bg3[ which(HAW12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW12_Bg2 <- rbind(HAW12_Bg2, addPlayer1)

#ROUND 12, Behind graph using weighted edges
HAW12_Bft <- ftable(HAW12_Bg2$player1, HAW12_Bg2$player2)
HAW12_Bft2 <- as.matrix(HAW12_Bft)
numRows <- nrow(HAW12_Bft2)
numCols <- ncol(HAW12_Bft2)
HAW12_Bft3 <- HAW12_Bft2[c(2:numRows) , c(1:numCols)]
HAW12_BTable <- graph.adjacency(HAW12_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(HAW12_BTable, vertex.label = V(HAW12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
HAW12_B.clusterCoef <- transitivity(HAW12_BTable, type="global") #cluster coefficient
HAW12_B.degreeCent <- centralization.degree(HAW12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_Bftn <- as.network.matrix(HAW12_Bft)
HAW12_B.netDensity <- network.density(HAW12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_B.entropy <- entropy(HAW12_Bft) #entropy

HAW12_B.netMx <- cbind(HAW12_B.netMx, HAW12_B.clusterCoef, HAW12_B.degreeCent$centralization,
                       HAW12_B.netDensity, HAW12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************
#NA

round = 12
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
HAW12_SFg2 <- data.frame(HAW12_SF)
HAW12_SFg2 <- HAW12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_SFg2$player1
player2vector <- HAW12_SFg2$player2
HAW12_SFg3 <- HAW12_SFg2
HAW12_SFg3$p1inp2vec <- is.element(HAW12_SFg3$player1, player2vector)
HAW12_SFg3$p2inp1vec <- is.element(HAW12_SFg3$player2, player1vector)

addPlayer1 <- HAW12_SFg3[ which(HAW12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_SFg3[ which(HAW12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_SFg2 <- rbind(HAW12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
HAW12_SFft <- ftable(HAW12_SFg2$player1, HAW12_SFg2$player2)
HAW12_SFft2 <- as.matrix(HAW12_SFft)
numRows <- nrow(HAW12_SFft2)
numCols <- ncol(HAW12_SFft2)
HAW12_SFft3 <- HAW12_SFft2[c(2:numRows) , c(2:numCols)]
HAW12_SFTable <- graph.adjacency(HAW12_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(HAW12_SFTable, vertex.label = V(HAW12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
HAW12_SF.clusterCoef <- transitivity(HAW12_SFTable, type="global") #cluster coefficient
HAW12_SF.degreeCent <- centralization.degree(HAW12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_SFftn <- as.network.matrix(HAW12_SFft)
HAW12_SF.netDensity <- network.density(HAW12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_SF.entropy <- entropy(HAW12_SFft) #entropy

HAW12_SF.netMx <- cbind(HAW12_SF.netMx, HAW12_SF.clusterCoef, HAW12_SF.degreeCent$centralization,
                        HAW12_SF.netDensity, HAW12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************

round = 12
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
HAW12_TFg2 <- data.frame(HAW12_TF)
HAW12_TFg2 <- HAW12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_TFg2$player1
player2vector <- HAW12_TFg2$player2
HAW12_TFg3 <- HAW12_TFg2
HAW12_TFg3$p1inp2vec <- is.element(HAW12_TFg3$player1, player2vector)
HAW12_TFg3$p2inp1vec <- is.element(HAW12_TFg3$player2, player1vector)

addPlayer1 <- HAW12_TFg3[ which(HAW12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_TFg3[ which(HAW12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_TFg2 <- rbind(HAW12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
HAW12_TFft <- ftable(HAW12_TFg2$player1, HAW12_TFg2$player2)
HAW12_TFft2 <- as.matrix(HAW12_TFft)
numRows <- nrow(HAW12_TFft2)
numCols <- ncol(HAW12_TFft2)
HAW12_TFft3 <- HAW12_TFft2[c(2:numRows) , c(2:numCols)]
HAW12_TFTable <- graph.adjacency(HAW12_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(HAW12_TFTable, vertex.label = V(HAW12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
HAW12_TF.clusterCoef <- transitivity(HAW12_TFTable, type="global") #cluster coefficient
HAW12_TF.degreeCent <- centralization.degree(HAW12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_TFftn <- as.network.matrix(HAW12_TFft)
HAW12_TF.netDensity <- network.density(HAW12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_TF.entropy <- entropy(HAW12_TFft) #entropy

HAW12_TF.netMx <- cbind(HAW12_TF.netMx, HAW12_TF.clusterCoef, HAW12_TF.degreeCent$centralization,
                        HAW12_TF.netDensity, HAW12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************

round = 12
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
HAW12_SAMg2 <- data.frame(HAW12_SAM)
HAW12_SAMg2 <- HAW12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_SAMg2$player1
player2vector <- HAW12_SAMg2$player2
HAW12_SAMg3 <- HAW12_SAMg2
HAW12_SAMg3$p1inp2vec <- is.element(HAW12_SAMg3$player1, player2vector)
HAW12_SAMg3$p2inp1vec <- is.element(HAW12_SAMg3$player2, player1vector)

addPlayer1 <- HAW12_SAMg3[ which(HAW12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_SAMg3[ which(HAW12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_SAMg2 <- rbind(HAW12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
HAW12_SAMft <- ftable(HAW12_SAMg2$player1, HAW12_SAMg2$player2)
HAW12_SAMft2 <- as.matrix(HAW12_SAMft)
numRows <- nrow(HAW12_SAMft2)
numCols <- ncol(HAW12_SAMft2)
HAW12_SAMft3 <- HAW12_SAMft2[c(2:numRows) , c(2:numCols)]
HAW12_SAMTable <- graph.adjacency(HAW12_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(HAW12_SAMTable, vertex.label = V(HAW12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
HAW12_SAM.clusterCoef <- transitivity(HAW12_SAMTable, type="global") #cluster coefficient
HAW12_SAM.degreeCent <- centralization.degree(HAW12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_SAMftn <- as.network.matrix(HAW12_SAMft)
HAW12_SAM.netDensity <- network.density(HAW12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_SAM.entropy <- entropy(HAW12_SAMft) #entropy

HAW12_SAM.netMx <- cbind(HAW12_SAM.netMx, HAW12_SAM.clusterCoef, HAW12_SAM.degreeCent$centralization,
                         HAW12_SAM.netDensity, HAW12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************
#NA

round = 12
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
HAW12_TAMg2 <- data.frame(HAW12_TAM)
HAW12_TAMg2 <- HAW12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_TAMg2$player1
player2vector <- HAW12_TAMg2$player2
HAW12_TAMg3 <- HAW12_TAMg2
HAW12_TAMg3$p1inp2vec <- is.element(HAW12_TAMg3$player1, player2vector)
HAW12_TAMg3$p2inp1vec <- is.element(HAW12_TAMg3$player2, player1vector)

addPlayer1 <- HAW12_TAMg3[ which(HAW12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_TAMg3[ which(HAW12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_TAMg2 <- rbind(HAW12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
HAW12_TAMft <- ftable(HAW12_TAMg2$player1, HAW12_TAMg2$player2)
HAW12_TAMft2 <- as.matrix(HAW12_TAMft)
numRows <- nrow(HAW12_TAMft2)
numCols <- ncol(HAW12_TAMft2)
HAW12_TAMft3 <- HAW12_TAMft2[c(2:numRows) , c(2:numCols)]
HAW12_TAMTable <- graph.adjacency(HAW12_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(HAW12_TAMTable, vertex.label = V(HAW12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
HAW12_TAM.clusterCoef <- transitivity(HAW12_TAMTable, type="global") #cluster coefficient
HAW12_TAM.degreeCent <- centralization.degree(HAW12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_TAMftn <- as.network.matrix(HAW12_TAMft)
HAW12_TAM.netDensity <- network.density(HAW12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_TAM.entropy <- entropy(HAW12_TAMft) #entropy

HAW12_TAM.netMx <- cbind(HAW12_TAM.netMx, HAW12_TAM.clusterCoef, HAW12_TAM.degreeCent$centralization,
                         HAW12_TAM.netDensity, HAW12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************

round = 12
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
HAW12_SDMg2 <- data.frame(HAW12_SDM)
HAW12_SDMg2 <- HAW12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_SDMg2$player1
player2vector <- HAW12_SDMg2$player2
HAW12_SDMg3 <- HAW12_SDMg2
HAW12_SDMg3$p1inp2vec <- is.element(HAW12_SDMg3$player1, player2vector)
HAW12_SDMg3$p2inp1vec <- is.element(HAW12_SDMg3$player2, player1vector)

addPlayer1 <- HAW12_SDMg3[ which(HAW12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_SDMg3[ which(HAW12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_SDMg2 <- rbind(HAW12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
HAW12_SDMft <- ftable(HAW12_SDMg2$player1, HAW12_SDMg2$player2)
HAW12_SDMft2 <- as.matrix(HAW12_SDMft)
numRows <- nrow(HAW12_SDMft2)
numCols <- ncol(HAW12_SDMft2)
HAW12_SDMft3 <- HAW12_SDMft2[c(2:numRows) , c(2:numCols)]
HAW12_SDMTable <- graph.adjacency(HAW12_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(HAW12_SDMTable, vertex.label = V(HAW12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
HAW12_SDM.clusterCoef <- transitivity(HAW12_SDMTable, type="global") #cluster coefficient
HAW12_SDM.degreeCent <- centralization.degree(HAW12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_SDMftn <- as.network.matrix(HAW12_SDMft)
HAW12_SDM.netDensity <- network.density(HAW12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_SDM.entropy <- entropy(HAW12_SDMft) #entropy

HAW12_SDM.netMx <- cbind(HAW12_SDM.netMx, HAW12_SDM.clusterCoef, HAW12_SDM.degreeCent$centralization,
                         HAW12_SDM.netDensity, HAW12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
HAW12_TDMg2 <- data.frame(HAW12_TDM)
HAW12_TDMg2 <- HAW12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_TDMg2$player1
player2vector <- HAW12_TDMg2$player2
HAW12_TDMg3 <- HAW12_TDMg2
HAW12_TDMg3$p1inp2vec <- is.element(HAW12_TDMg3$player1, player2vector)
HAW12_TDMg3$p2inp1vec <- is.element(HAW12_TDMg3$player2, player1vector)

addPlayer1 <- HAW12_TDMg3[ which(HAW12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_TDMg3[ which(HAW12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_TDMg2 <- rbind(HAW12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
HAW12_TDMft <- ftable(HAW12_TDMg2$player1, HAW12_TDMg2$player2)
HAW12_TDMft2 <- as.matrix(HAW12_TDMft)
numRows <- nrow(HAW12_TDMft2)
numCols <- ncol(HAW12_TDMft2)
HAW12_TDMft3 <- HAW12_TDMft2[c(2:numRows) , c(2:numCols)]
HAW12_TDMTable <- graph.adjacency(HAW12_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(HAW12_TDMTable, vertex.label = V(HAW12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
HAW12_TDM.clusterCoef <- transitivity(HAW12_TDMTable, type="global") #cluster coefficient
HAW12_TDM.degreeCent <- centralization.degree(HAW12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_TDMftn <- as.network.matrix(HAW12_TDMft)
HAW12_TDM.netDensity <- network.density(HAW12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_TDM.entropy <- entropy(HAW12_TDMft) #entropy

HAW12_TDM.netMx <- cbind(HAW12_TDM.netMx, HAW12_TDM.clusterCoef, HAW12_TDM.degreeCent$centralization,
                         HAW12_TDM.netDensity, HAW12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
HAW12_SDg2 <- data.frame(HAW12_SD)
HAW12_SDg2 <- HAW12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_SDg2$player1
player2vector <- HAW12_SDg2$player2
HAW12_SDg3 <- HAW12_SDg2
HAW12_SDg3$p1inp2vec <- is.element(HAW12_SDg3$player1, player2vector)
HAW12_SDg3$p2inp1vec <- is.element(HAW12_SDg3$player2, player1vector)

addPlayer1 <- HAW12_SDg3[ which(HAW12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_SDg3[ which(HAW12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_SDg2 <- rbind(HAW12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
HAW12_SDft <- ftable(HAW12_SDg2$player1, HAW12_SDg2$player2)
HAW12_SDft2 <- as.matrix(HAW12_SDft)
numRows <- nrow(HAW12_SDft2)
numCols <- ncol(HAW12_SDft2)
HAW12_SDft3 <- HAW12_SDft2[c(2:numRows) , c(2:numCols)]
HAW12_SDTable <- graph.adjacency(HAW12_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(HAW12_SDTable, vertex.label = V(HAW12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
HAW12_SD.clusterCoef <- transitivity(HAW12_SDTable, type="global") #cluster coefficient
HAW12_SD.degreeCent <- centralization.degree(HAW12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_SDftn <- as.network.matrix(HAW12_SDft)
HAW12_SD.netDensity <- network.density(HAW12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_SD.entropy <- entropy(HAW12_SDft) #entropy

HAW12_SD.netMx <- cbind(HAW12_SD.netMx, HAW12_SD.clusterCoef, HAW12_SD.degreeCent$centralization,
                        HAW12_SD.netDensity, HAW12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
HAW12_TDg2 <- data.frame(HAW12_TD)
HAW12_TDg2 <- HAW12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_TDg2$player1
player2vector <- HAW12_TDg2$player2
HAW12_TDg3 <- HAW12_TDg2
HAW12_TDg3$p1inp2vec <- is.element(HAW12_TDg3$player1, player2vector)
HAW12_TDg3$p2inp1vec <- is.element(HAW12_TDg3$player2, player1vector)

addPlayer1 <- HAW12_TDg3[ which(HAW12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_TDg3[ which(HAW12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_TDg2 <- rbind(HAW12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
HAW12_TDft <- ftable(HAW12_TDg2$player1, HAW12_TDg2$player2)
HAW12_TDft2 <- as.matrix(HAW12_TDft)
numRows <- nrow(HAW12_TDft2)
numCols <- ncol(HAW12_TDft2)
HAW12_TDft3 <- HAW12_TDft2[c(2:numRows) , c(2:numCols)]
HAW12_TDTable <- graph.adjacency(HAW12_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(HAW12_TDTable, vertex.label = V(HAW12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
HAW12_TD.clusterCoef <- transitivity(HAW12_TDTable, type="global") #cluster coefficient
HAW12_TD.degreeCent <- centralization.degree(HAW12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_TDftn <- as.network.matrix(HAW12_TDft)
HAW12_TD.netDensity <- network.density(HAW12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_TD.entropy <- entropy(HAW12_TDft) #entropy

HAW12_TD.netMx <- cbind(HAW12_TD.netMx, HAW12_TD.clusterCoef, HAW12_TD.degreeCent$centralization,
                        HAW12_TD.netDensity, HAW12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
HAW12_QTg2 <- data.frame(HAW12_QT)
HAW12_QTg2 <- HAW12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW12_QTg2$player1
player2vector <- HAW12_QTg2$player2
HAW12_QTg3 <- HAW12_QTg2
HAW12_QTg3$p1inp2vec <- is.element(HAW12_QTg3$player1, player2vector)
HAW12_QTg3$p2inp1vec <- is.element(HAW12_QTg3$player2, player1vector)

addPlayer1 <- HAW12_QTg3[ which(HAW12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW12_QTg3[ which(HAW12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW12_QTg2 <- rbind(HAW12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
HAW12_QTft <- ftable(HAW12_QTg2$player1, HAW12_QTg2$player2)
HAW12_QTft2 <- as.matrix(HAW12_QTft)
numRows <- nrow(HAW12_QTft2)
numCols <- ncol(HAW12_QTft2)
HAW12_QTft3 <- HAW12_QTft2[c(2:numRows) , c(2:numCols)]
HAW12_QTTable <- graph.adjacency(HAW12_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(HAW12_QTTable, vertex.label = V(HAW12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
HAW12_QT.clusterCoef <- transitivity(HAW12_QTTable, type="global") #cluster coefficient
HAW12_QT.degreeCent <- centralization.degree(HAW12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW12_QTftn <- as.network.matrix(HAW12_QTft)
HAW12_QT.netDensity <- network.density(HAW12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW12_QT.entropy <- entropy(HAW12_QTft) #entropy

HAW12_QT.netMx <- cbind(HAW12_QT.netMx, HAW12_QT.clusterCoef, HAW12_QT.degreeCent$centralization,
                        HAW12_QT.netDensity, HAW12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW12_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************

round = 12
teamName = "MELB"
KIoutcome = "Goal_F"
MELB12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
MELB12_Gg2 <- data.frame(MELB12_G)
MELB12_Gg2 <- MELB12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_Gg2$player1
player2vector <- MELB12_Gg2$player2
MELB12_Gg3 <- MELB12_Gg2
MELB12_Gg3$p1inp2vec <- is.element(MELB12_Gg3$player1, player2vector)
MELB12_Gg3$p2inp1vec <- is.element(MELB12_Gg3$player2, player1vector)

addPlayer1 <- MELB12_Gg3[ which(MELB12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB12_Gg3[ which(MELB12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_Gg2 <- rbind(MELB12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
MELB12_Gft <- ftable(MELB12_Gg2$player1, MELB12_Gg2$player2)
MELB12_Gft2 <- as.matrix(MELB12_Gft)
numRows <- nrow(MELB12_Gft2)
numCols <- ncol(MELB12_Gft2)
MELB12_Gft3 <- MELB12_Gft2[c(2:numRows) , c(2:numCols)]
MELB12_GTable <- graph.adjacency(MELB12_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(MELB12_GTable, vertex.label = V(MELB12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
MELB12_G.clusterCoef <- transitivity(MELB12_GTable, type="global") #cluster coefficient
MELB12_G.degreeCent <- centralization.degree(MELB12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_Gftn <- as.network.matrix(MELB12_Gft)
MELB12_G.netDensity <- network.density(MELB12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_G.entropy <- entropy(MELB12_Gft) #entropy

MELB12_G.netMx <- cbind(MELB12_G.netMx, MELB12_G.clusterCoef, MELB12_G.degreeCent$centralization,
                        MELB12_G.netDensity, MELB12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "MELB"
KIoutcome = "Behind_F"
MELB12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
MELB12_Bg2 <- data.frame(MELB12_B)
MELB12_Bg2 <- MELB12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_Bg2$player1
player2vector <- MELB12_Bg2$player2
MELB12_Bg3 <- MELB12_Bg2
MELB12_Bg3$p1inp2vec <- is.element(MELB12_Bg3$player1, player2vector)
MELB12_Bg3$p2inp1vec <- is.element(MELB12_Bg3$player2, player1vector)

addPlayer1 <- MELB12_Bg3[ which(MELB12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB12_Bg3[ which(MELB12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_Bg2 <- rbind(MELB12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
MELB12_Bft <- ftable(MELB12_Bg2$player1, MELB12_Bg2$player2)
MELB12_Bft2 <- as.matrix(MELB12_Bft)
numRows <- nrow(MELB12_Bft2)
numCols <- ncol(MELB12_Bft2)
MELB12_Bft3 <- MELB12_Bft2[c(2:numRows) , c(2:numCols)]
MELB12_BTable <- graph.adjacency(MELB12_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(MELB12_BTable, vertex.label = V(MELB12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
MELB12_B.clusterCoef <- transitivity(MELB12_BTable, type="global") #cluster coefficient
MELB12_B.degreeCent <- centralization.degree(MELB12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_Bftn <- as.network.matrix(MELB12_Bft)
MELB12_B.netDensity <- network.density(MELB12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_B.entropy <- entropy(MELB12_Bft) #entropy

MELB12_B.netMx <- cbind(MELB12_B.netMx, MELB12_B.clusterCoef, MELB12_B.degreeCent$centralization,
                        MELB12_B.netDensity, MELB12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************
#NA

round = 12
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
MELB12_SFg2 <- data.frame(MELB12_SF)
MELB12_SFg2 <- MELB12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_SFg2$player1
player2vector <- MELB12_SFg2$player2
MELB12_SFg3 <- MELB12_SFg2
MELB12_SFg3$p1inp2vec <- is.element(MELB12_SFg3$player1, player2vector)
MELB12_SFg3$p2inp1vec <- is.element(MELB12_SFg3$player2, player1vector)

addPlayer1 <- MELB12_SFg3[ which(MELB12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB12_SFg3[ which(MELB12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_SFg2 <- rbind(MELB12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
MELB12_SFft <- ftable(MELB12_SFg2$player1, MELB12_SFg2$player2)
MELB12_SFft2 <- as.matrix(MELB12_SFft)
numRows <- nrow(MELB12_SFft2)
numCols <- ncol(MELB12_SFft2)
MELB12_SFft3 <- MELB12_SFft2[c(2:numRows) , c(2:numCols)]
MELB12_SFTable <- graph.adjacency(MELB12_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(MELB12_SFTable, vertex.label = V(MELB12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
MELB12_SF.clusterCoef <- transitivity(MELB12_SFTable, type="global") #cluster coefficient
MELB12_SF.degreeCent <- centralization.degree(MELB12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_SFftn <- as.network.matrix(MELB12_SFft)
MELB12_SF.netDensity <- network.density(MELB12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_SF.entropy <- entropy(MELB12_SFft) #entropy

MELB12_SF.netMx <- cbind(MELB12_SF.netMx, MELB12_SF.clusterCoef, MELB12_SF.degreeCent$centralization,
                         MELB12_SF.netDensity, MELB12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************
#NA

round = 12
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
MELB12_TFg2 <- data.frame(MELB12_TF)
MELB12_TFg2 <- MELB12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_TFg2$player1
player2vector <- MELB12_TFg2$player2
MELB12_TFg3 <- MELB12_TFg2
MELB12_TFg3$p1inp2vec <- is.element(MELB12_TFg3$player1, player2vector)
MELB12_TFg3$p2inp1vec <- is.element(MELB12_TFg3$player2, player1vector)

addPlayer1 <- MELB12_TFg3[ which(MELB12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB12_TFg3[ which(MELB12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_TFg2 <- rbind(MELB12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
MELB12_TFft <- ftable(MELB12_TFg2$player1, MELB12_TFg2$player2)
MELB12_TFft2 <- as.matrix(MELB12_TFft)
numRows <- nrow(MELB12_TFft2)
numCols <- ncol(MELB12_TFft2)
MELB12_TFft3 <- MELB12_TFft2[c(2:numRows) , c(2:numCols)]
MELB12_TFTable <- graph.adjacency(MELB12_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(MELB12_TFTable, vertex.label = V(MELB12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
MELB12_TF.clusterCoef <- transitivity(MELB12_TFTable, type="global") #cluster coefficient
MELB12_TF.degreeCent <- centralization.degree(MELB12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_TFftn <- as.network.matrix(MELB12_TFft)
MELB12_TF.netDensity <- network.density(MELB12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_TF.entropy <- entropy(MELB12_TFft) #entropy

MELB12_TF.netMx <- cbind(MELB12_TF.netMx, MELB12_TF.clusterCoef, MELB12_TF.degreeCent$centralization,
                         MELB12_TF.netDensity, MELB12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************

round = 12
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
MELB12_SAMg2 <- data.frame(MELB12_SAM)
MELB12_SAMg2 <- MELB12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_SAMg2$player1
player2vector <- MELB12_SAMg2$player2
MELB12_SAMg3 <- MELB12_SAMg2
MELB12_SAMg3$p1inp2vec <- is.element(MELB12_SAMg3$player1, player2vector)
MELB12_SAMg3$p2inp1vec <- is.element(MELB12_SAMg3$player2, player1vector)

addPlayer1 <- MELB12_SAMg3[ which(MELB12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB12_SAMg3[ which(MELB12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_SAMg2 <- rbind(MELB12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
MELB12_SAMft <- ftable(MELB12_SAMg2$player1, MELB12_SAMg2$player2)
MELB12_SAMft2 <- as.matrix(MELB12_SAMft)
numRows <- nrow(MELB12_SAMft2)
numCols <- ncol(MELB12_SAMft2)
MELB12_SAMft3 <- MELB12_SAMft2[c(2:numRows) , c(2:numCols)]
MELB12_SAMTable <- graph.adjacency(MELB12_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(MELB12_SAMTable, vertex.label = V(MELB12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
MELB12_SAM.clusterCoef <- transitivity(MELB12_SAMTable, type="global") #cluster coefficient
MELB12_SAM.degreeCent <- centralization.degree(MELB12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_SAMftn <- as.network.matrix(MELB12_SAMft)
MELB12_SAM.netDensity <- network.density(MELB12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_SAM.entropy <- entropy(MELB12_SAMft) #entropy

MELB12_SAM.netMx <- cbind(MELB12_SAM.netMx, MELB12_SAM.clusterCoef, MELB12_SAM.degreeCent$centralization,
                          MELB12_SAM.netDensity, MELB12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************

round = 12
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
MELB12_TAMg2 <- data.frame(MELB12_TAM)
MELB12_TAMg2 <- MELB12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_TAMg2$player1
player2vector <- MELB12_TAMg2$player2
MELB12_TAMg3 <- MELB12_TAMg2
MELB12_TAMg3$p1inp2vec <- is.element(MELB12_TAMg3$player1, player2vector)
MELB12_TAMg3$p2inp1vec <- is.element(MELB12_TAMg3$player2, player1vector)

addPlayer1 <- MELB12_TAMg3[ which(MELB12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB12_TAMg3[ which(MELB12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_TAMg2 <- rbind(MELB12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
MELB12_TAMft <- ftable(MELB12_TAMg2$player1, MELB12_TAMg2$player2)
MELB12_TAMft2 <- as.matrix(MELB12_TAMft)
numRows <- nrow(MELB12_TAMft2)
numCols <- ncol(MELB12_TAMft2)
MELB12_TAMft3 <- MELB12_TAMft2[c(2:numRows) , c(2:numCols)]
MELB12_TAMTable <- graph.adjacency(MELB12_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(MELB12_TAMTable, vertex.label = V(MELB12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
MELB12_TAM.clusterCoef <- transitivity(MELB12_TAMTable, type="global") #cluster coefficient
MELB12_TAM.degreeCent <- centralization.degree(MELB12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_TAMftn <- as.network.matrix(MELB12_TAMft)
MELB12_TAM.netDensity <- network.density(MELB12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_TAM.entropy <- entropy(MELB12_TAMft) #entropy

MELB12_TAM.netMx <- cbind(MELB12_TAM.netMx, MELB12_TAM.clusterCoef, MELB12_TAM.degreeCent$centralization,
                          MELB12_TAM.netDensity, MELB12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************

round = 12
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
MELB12_SDMg2 <- data.frame(MELB12_SDM)
MELB12_SDMg2 <- MELB12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_SDMg2$player1
player2vector <- MELB12_SDMg2$player2
MELB12_SDMg3 <- MELB12_SDMg2
MELB12_SDMg3$p1inp2vec <- is.element(MELB12_SDMg3$player1, player2vector)
MELB12_SDMg3$p2inp1vec <- is.element(MELB12_SDMg3$player2, player1vector)

addPlayer1 <- MELB12_SDMg3[ which(MELB12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB12_SDMg3[ which(MELB12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_SDMg2 <- rbind(MELB12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
MELB12_SDMft <- ftable(MELB12_SDMg2$player1, MELB12_SDMg2$player2)
MELB12_SDMft2 <- as.matrix(MELB12_SDMft)
numRows <- nrow(MELB12_SDMft2)
numCols <- ncol(MELB12_SDMft2)
MELB12_SDMft3 <- MELB12_SDMft2[c(2:numRows) , c(2:numCols)]
MELB12_SDMTable <- graph.adjacency(MELB12_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(MELB12_SDMTable, vertex.label = V(MELB12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
MELB12_SDM.clusterCoef <- transitivity(MELB12_SDMTable, type="global") #cluster coefficient
MELB12_SDM.degreeCent <- centralization.degree(MELB12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_SDMftn <- as.network.matrix(MELB12_SDMft)
MELB12_SDM.netDensity <- network.density(MELB12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_SDM.entropy <- entropy(MELB12_SDMft) #entropy

MELB12_SDM.netMx <- cbind(MELB12_SDM.netMx, MELB12_SDM.clusterCoef, MELB12_SDM.degreeCent$centralization,
                          MELB12_SDM.netDensity, MELB12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
MELB12_TDMg2 <- data.frame(MELB12_TDM)
MELB12_TDMg2 <- MELB12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_TDMg2$player1
player2vector <- MELB12_TDMg2$player2
MELB12_TDMg3 <- MELB12_TDMg2
MELB12_TDMg3$p1inp2vec <- is.element(MELB12_TDMg3$player1, player2vector)
MELB12_TDMg3$p2inp1vec <- is.element(MELB12_TDMg3$player2, player1vector)

addPlayer1 <- MELB12_TDMg3[ which(MELB12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB12_TDMg3[ which(MELB12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_TDMg2 <- rbind(MELB12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
MELB12_TDMft <- ftable(MELB12_TDMg2$player1, MELB12_TDMg2$player2)
MELB12_TDMft2 <- as.matrix(MELB12_TDMft)
numRows <- nrow(MELB12_TDMft2)
numCols <- ncol(MELB12_TDMft2)
MELB12_TDMft3 <- MELB12_TDMft2[c(2:numRows) , c(2:numCols)]
MELB12_TDMTable <- graph.adjacency(MELB12_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(MELB12_TDMTable, vertex.label = V(MELB12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
MELB12_TDM.clusterCoef <- transitivity(MELB12_TDMTable, type="global") #cluster coefficient
MELB12_TDM.degreeCent <- centralization.degree(MELB12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_TDMftn <- as.network.matrix(MELB12_TDMft)
MELB12_TDM.netDensity <- network.density(MELB12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_TDM.entropy <- entropy(MELB12_TDMft) #entropy

MELB12_TDM.netMx <- cbind(MELB12_TDM.netMx, MELB12_TDM.clusterCoef, MELB12_TDM.degreeCent$centralization,
                          MELB12_TDM.netDensity, MELB12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
MELB12_SDg2 <- data.frame(MELB12_SD)
MELB12_SDg2 <- MELB12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_SDg2$player1
player2vector <- MELB12_SDg2$player2
MELB12_SDg3 <- MELB12_SDg2
MELB12_SDg3$p1inp2vec <- is.element(MELB12_SDg3$player1, player2vector)
MELB12_SDg3$p2inp1vec <- is.element(MELB12_SDg3$player2, player1vector)

addPlayer1 <- MELB12_SDg3[ which(MELB12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB12_SDg3[ which(MELB12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_SDg2 <- rbind(MELB12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
MELB12_SDft <- ftable(MELB12_SDg2$player1, MELB12_SDg2$player2)
MELB12_SDft2 <- as.matrix(MELB12_SDft)
numRows <- nrow(MELB12_SDft2)
numCols <- ncol(MELB12_SDft2)
MELB12_SDft3 <- MELB12_SDft2[c(2:numRows) , c(2:numCols)]
MELB12_SDTable <- graph.adjacency(MELB12_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(MELB12_SDTable, vertex.label = V(MELB12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
MELB12_SD.clusterCoef <- transitivity(MELB12_SDTable, type="global") #cluster coefficient
MELB12_SD.degreeCent <- centralization.degree(MELB12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_SDftn <- as.network.matrix(MELB12_SDft)
MELB12_SD.netDensity <- network.density(MELB12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_SD.entropy <- entropy(MELB12_SDft) #entropy

MELB12_SD.netMx <- cbind(MELB12_SD.netMx, MELB12_SD.clusterCoef, MELB12_SD.degreeCent$centralization,
                         MELB12_SD.netDensity, MELB12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
MELB12_TDg2 <- data.frame(MELB12_TD)
MELB12_TDg2 <- MELB12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_TDg2$player1
player2vector <- MELB12_TDg2$player2
MELB12_TDg3 <- MELB12_TDg2
MELB12_TDg3$p1inp2vec <- is.element(MELB12_TDg3$player1, player2vector)
MELB12_TDg3$p2inp1vec <- is.element(MELB12_TDg3$player2, player1vector)

addPlayer1 <- MELB12_TDg3[ which(MELB12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB12_TDg3[ which(MELB12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_TDg2 <- rbind(MELB12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
MELB12_TDft <- ftable(MELB12_TDg2$player1, MELB12_TDg2$player2)
MELB12_TDft2 <- as.matrix(MELB12_TDft)
numRows <- nrow(MELB12_TDft2)
numCols <- ncol(MELB12_TDft2)
MELB12_TDft3 <- MELB12_TDft2[c(2:numRows) , c(2:numCols)]
MELB12_TDTable <- graph.adjacency(MELB12_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(MELB12_TDTable, vertex.label = V(MELB12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
MELB12_TD.clusterCoef <- transitivity(MELB12_TDTable, type="global") #cluster coefficient
MELB12_TD.degreeCent <- centralization.degree(MELB12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_TDftn <- as.network.matrix(MELB12_TDft)
MELB12_TD.netDensity <- network.density(MELB12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_TD.entropy <- entropy(MELB12_TDft) #entropy

MELB12_TD.netMx <- cbind(MELB12_TD.netMx, MELB12_TD.clusterCoef, MELB12_TD.degreeCent$centralization,
                         MELB12_TD.netDensity, MELB12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
MELB12_QTg2 <- data.frame(MELB12_QT)
MELB12_QTg2 <- MELB12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB12_QTg2$player1
player2vector <- MELB12_QTg2$player2
MELB12_QTg3 <- MELB12_QTg2
MELB12_QTg3$p1inp2vec <- is.element(MELB12_QTg3$player1, player2vector)
MELB12_QTg3$p2inp1vec <- is.element(MELB12_QTg3$player2, player1vector)

addPlayer1 <- MELB12_QTg3[ which(MELB12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB12_QTg3[ which(MELB12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB12_QTg2 <- rbind(MELB12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
MELB12_QTft <- ftable(MELB12_QTg2$player1, MELB12_QTg2$player2)
MELB12_QTft2 <- as.matrix(MELB12_QTft)
numRows <- nrow(MELB12_QTft2)
numCols <- ncol(MELB12_QTft2)
MELB12_QTft3 <- MELB12_QTft2[c(2:numRows) , c(2:numCols)]
MELB12_QTTable <- graph.adjacency(MELB12_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(MELB12_QTTable, vertex.label = V(MELB12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
MELB12_QT.clusterCoef <- transitivity(MELB12_QTTable, type="global") #cluster coefficient
MELB12_QT.degreeCent <- centralization.degree(MELB12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB12_QTftn <- as.network.matrix(MELB12_QTft)
MELB12_QT.netDensity <- network.density(MELB12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB12_QT.entropy <- entropy(MELB12_QTft) #entropy

MELB12_QT.netMx <- cbind(MELB12_QT.netMx, MELB12_QT.clusterCoef, MELB12_QT.degreeCent$centralization,
                         MELB12_QT.netDensity, MELB12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB12_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************

round = 12
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
NMFC12_Gg2 <- data.frame(NMFC12_G)
NMFC12_Gg2 <- NMFC12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_Gg2$player1
player2vector <- NMFC12_Gg2$player2
NMFC12_Gg3 <- NMFC12_Gg2
NMFC12_Gg3$p1inp2vec <- is.element(NMFC12_Gg3$player1, player2vector)
NMFC12_Gg3$p2inp1vec <- is.element(NMFC12_Gg3$player2, player1vector)

addPlayer1 <- NMFC12_Gg3[ which(NMFC12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_Gg3[ which(NMFC12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_Gg2 <- rbind(NMFC12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
NMFC12_Gft <- ftable(NMFC12_Gg2$player1, NMFC12_Gg2$player2)
NMFC12_Gft2 <- as.matrix(NMFC12_Gft)
numRows <- nrow(NMFC12_Gft2)
numCols <- ncol(NMFC12_Gft2)
NMFC12_Gft3 <- NMFC12_Gft2[c(2:numRows) , c(2:numCols)]
NMFC12_GTable <- graph.adjacency(NMFC12_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(NMFC12_GTable, vertex.label = V(NMFC12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
NMFC12_G.clusterCoef <- transitivity(NMFC12_GTable, type="global") #cluster coefficient
NMFC12_G.degreeCent <- centralization.degree(NMFC12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_Gftn <- as.network.matrix(NMFC12_Gft)
NMFC12_G.netDensity <- network.density(NMFC12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_G.entropy <- entropy(NMFC12_Gft) #entropy

NMFC12_G.netMx <- cbind(NMFC12_G.netMx, NMFC12_G.clusterCoef, NMFC12_G.degreeCent$centralization,
                        NMFC12_G.netDensity, NMFC12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
NMFC12_Bg2 <- data.frame(NMFC12_B)
NMFC12_Bg2 <- NMFC12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_Bg2$player1
player2vector <- NMFC12_Bg2$player2
NMFC12_Bg3 <- NMFC12_Bg2
NMFC12_Bg3$p1inp2vec <- is.element(NMFC12_Bg3$player1, player2vector)
NMFC12_Bg3$p2inp1vec <- is.element(NMFC12_Bg3$player2, player1vector)

addPlayer1 <- NMFC12_Bg3[ which(NMFC12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_Bg3[ which(NMFC12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_Bg2 <- rbind(NMFC12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
NMFC12_Bft <- ftable(NMFC12_Bg2$player1, NMFC12_Bg2$player2)
NMFC12_Bft2 <- as.matrix(NMFC12_Bft)
numRows <- nrow(NMFC12_Bft2)
numCols <- ncol(NMFC12_Bft2)
NMFC12_Bft3 <- NMFC12_Bft2[c(2:numRows) , c(2:numCols)]
NMFC12_BTable <- graph.adjacency(NMFC12_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(NMFC12_BTable, vertex.label = V(NMFC12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
NMFC12_B.clusterCoef <- transitivity(NMFC12_BTable, type="global") #cluster coefficient
NMFC12_B.degreeCent <- centralization.degree(NMFC12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_Bftn <- as.network.matrix(NMFC12_Bft)
NMFC12_B.netDensity <- network.density(NMFC12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_B.entropy <- entropy(NMFC12_Bft) #entropy

NMFC12_B.netMx <- cbind(NMFC12_B.netMx, NMFC12_B.clusterCoef, NMFC12_B.degreeCent$centralization,
                        NMFC12_B.netDensity, NMFC12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************
#NA

round = 12
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
NMFC12_SFg2 <- data.frame(NMFC12_SF)
NMFC12_SFg2 <- NMFC12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_SFg2$player1
player2vector <- NMFC12_SFg2$player2
NMFC12_SFg3 <- NMFC12_SFg2
NMFC12_SFg3$p1inp2vec <- is.element(NMFC12_SFg3$player1, player2vector)
NMFC12_SFg3$p2inp1vec <- is.element(NMFC12_SFg3$player2, player1vector)

addPlayer1 <- NMFC12_SFg3[ which(NMFC12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_SFg3[ which(NMFC12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_SFg2 <- rbind(NMFC12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
NMFC12_SFft <- ftable(NMFC12_SFg2$player1, NMFC12_SFg2$player2)
NMFC12_SFft2 <- as.matrix(NMFC12_SFft)
numRows <- nrow(NMFC12_SFft2)
numCols <- ncol(NMFC12_SFft2)
NMFC12_SFft3 <- NMFC12_SFft2[c(2:numRows) , c(2:numCols)]
NMFC12_SFTable <- graph.adjacency(NMFC12_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(NMFC12_SFTable, vertex.label = V(NMFC12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
NMFC12_SF.clusterCoef <- transitivity(NMFC12_SFTable, type="global") #cluster coefficient
NMFC12_SF.degreeCent <- centralization.degree(NMFC12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_SFftn <- as.network.matrix(NMFC12_SFft)
NMFC12_SF.netDensity <- network.density(NMFC12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_SF.entropy <- entropy(NMFC12_SFft) #entropy

NMFC12_SF.netMx <- cbind(NMFC12_SF.netMx, NMFC12_SF.clusterCoef, NMFC12_SF.degreeCent$centralization,
                         NMFC12_SF.netDensity, NMFC12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************

round = 12
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
NMFC12_TFg2 <- data.frame(NMFC12_TF)
NMFC12_TFg2 <- NMFC12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_TFg2$player1
player2vector <- NMFC12_TFg2$player2
NMFC12_TFg3 <- NMFC12_TFg2
NMFC12_TFg3$p1inp2vec <- is.element(NMFC12_TFg3$player1, player2vector)
NMFC12_TFg3$p2inp1vec <- is.element(NMFC12_TFg3$player2, player1vector)

addPlayer1 <- NMFC12_TFg3[ which(NMFC12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC12_TFg3[ which(NMFC12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_TFg2 <- rbind(NMFC12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
NMFC12_TFft <- ftable(NMFC12_TFg2$player1, NMFC12_TFg2$player2)
NMFC12_TFft2 <- as.matrix(NMFC12_TFft)
numRows <- nrow(NMFC12_TFft2)
numCols <- ncol(NMFC12_TFft2)
NMFC12_TFft3 <- NMFC12_TFft2[c(2:numRows) , c(2:numCols)]
NMFC12_TFTable <- graph.adjacency(NMFC12_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(NMFC12_TFTable, vertex.label = V(NMFC12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
NMFC12_TF.clusterCoef <- transitivity(NMFC12_TFTable, type="global") #cluster coefficient
NMFC12_TF.degreeCent <- centralization.degree(NMFC12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_TFftn <- as.network.matrix(NMFC12_TFft)
NMFC12_TF.netDensity <- network.density(NMFC12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_TF.entropy <- entropy(NMFC12_TFft) #entropy

NMFC12_TF.netMx <- cbind(NMFC12_TF.netMx, NMFC12_TF.clusterCoef, NMFC12_TF.degreeCent$centralization,
                         NMFC12_TF.netDensity, NMFC12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************

round = 12
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
NMFC12_SAMg2 <- data.frame(NMFC12_SAM)
NMFC12_SAMg2 <- NMFC12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_SAMg2$player1
player2vector <- NMFC12_SAMg2$player2
NMFC12_SAMg3 <- NMFC12_SAMg2
NMFC12_SAMg3$p1inp2vec <- is.element(NMFC12_SAMg3$player1, player2vector)
NMFC12_SAMg3$p2inp1vec <- is.element(NMFC12_SAMg3$player2, player1vector)

addPlayer1 <- NMFC12_SAMg3[ which(NMFC12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_SAMg3[ which(NMFC12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_SAMg2 <- rbind(NMFC12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
NMFC12_SAMft <- ftable(NMFC12_SAMg2$player1, NMFC12_SAMg2$player2)
NMFC12_SAMft2 <- as.matrix(NMFC12_SAMft)
numRows <- nrow(NMFC12_SAMft2)
numCols <- ncol(NMFC12_SAMft2)
NMFC12_SAMft3 <- NMFC12_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC12_SAMTable <- graph.adjacency(NMFC12_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(NMFC12_SAMTable, vertex.label = V(NMFC12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
NMFC12_SAM.clusterCoef <- transitivity(NMFC12_SAMTable, type="global") #cluster coefficient
NMFC12_SAM.degreeCent <- centralization.degree(NMFC12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_SAMftn <- as.network.matrix(NMFC12_SAMft)
NMFC12_SAM.netDensity <- network.density(NMFC12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_SAM.entropy <- entropy(NMFC12_SAMft) #entropy

NMFC12_SAM.netMx <- cbind(NMFC12_SAM.netMx, NMFC12_SAM.clusterCoef, NMFC12_SAM.degreeCent$centralization,
                          NMFC12_SAM.netDensity, NMFC12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************
#NA

round = 12
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
NMFC12_TAMg2 <- data.frame(NMFC12_TAM)
NMFC12_TAMg2 <- NMFC12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_TAMg2$player1
player2vector <- NMFC12_TAMg2$player2
NMFC12_TAMg3 <- NMFC12_TAMg2
NMFC12_TAMg3$p1inp2vec <- is.element(NMFC12_TAMg3$player1, player2vector)
NMFC12_TAMg3$p2inp1vec <- is.element(NMFC12_TAMg3$player2, player1vector)

addPlayer1 <- NMFC12_TAMg3[ which(NMFC12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_TAMg3[ which(NMFC12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_TAMg2 <- rbind(NMFC12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
NMFC12_TAMft <- ftable(NMFC12_TAMg2$player1, NMFC12_TAMg2$player2)
NMFC12_TAMft2 <- as.matrix(NMFC12_TAMft)
numRows <- nrow(NMFC12_TAMft2)
numCols <- ncol(NMFC12_TAMft2)
NMFC12_TAMft3 <- NMFC12_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC12_TAMTable <- graph.adjacency(NMFC12_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(NMFC12_TAMTable, vertex.label = V(NMFC12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
NMFC12_TAM.clusterCoef <- transitivity(NMFC12_TAMTable, type="global") #cluster coefficient
NMFC12_TAM.degreeCent <- centralization.degree(NMFC12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_TAMftn <- as.network.matrix(NMFC12_TAMft)
NMFC12_TAM.netDensity <- network.density(NMFC12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_TAM.entropy <- entropy(NMFC12_TAMft) #entropy

NMFC12_TAM.netMx <- cbind(NMFC12_TAM.netMx, NMFC12_TAM.clusterCoef, NMFC12_TAM.degreeCent$centralization,
                          NMFC12_TAM.netDensity, NMFC12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************

round = 12
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
NMFC12_SDMg2 <- data.frame(NMFC12_SDM)
NMFC12_SDMg2 <- NMFC12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_SDMg2$player1
player2vector <- NMFC12_SDMg2$player2
NMFC12_SDMg3 <- NMFC12_SDMg2
NMFC12_SDMg3$p1inp2vec <- is.element(NMFC12_SDMg3$player1, player2vector)
NMFC12_SDMg3$p2inp1vec <- is.element(NMFC12_SDMg3$player2, player1vector)

addPlayer1 <- NMFC12_SDMg3[ which(NMFC12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_SDMg3[ which(NMFC12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_SDMg2 <- rbind(NMFC12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
NMFC12_SDMft <- ftable(NMFC12_SDMg2$player1, NMFC12_SDMg2$player2)
NMFC12_SDMft2 <- as.matrix(NMFC12_SDMft)
numRows <- nrow(NMFC12_SDMft2)
numCols <- ncol(NMFC12_SDMft2)
NMFC12_SDMft3 <- NMFC12_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC12_SDMTable <- graph.adjacency(NMFC12_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(NMFC12_SDMTable, vertex.label = V(NMFC12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
NMFC12_SDM.clusterCoef <- transitivity(NMFC12_SDMTable, type="global") #cluster coefficient
NMFC12_SDM.degreeCent <- centralization.degree(NMFC12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_SDMftn <- as.network.matrix(NMFC12_SDMft)
NMFC12_SDM.netDensity <- network.density(NMFC12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_SDM.entropy <- entropy(NMFC12_SDMft) #entropy

NMFC12_SDM.netMx <- cbind(NMFC12_SDM.netMx, NMFC12_SDM.clusterCoef, NMFC12_SDM.degreeCent$centralization,
                          NMFC12_SDM.netDensity, NMFC12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
NMFC12_TDMg2 <- data.frame(NMFC12_TDM)
NMFC12_TDMg2 <- NMFC12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_TDMg2$player1
player2vector <- NMFC12_TDMg2$player2
NMFC12_TDMg3 <- NMFC12_TDMg2
NMFC12_TDMg3$p1inp2vec <- is.element(NMFC12_TDMg3$player1, player2vector)
NMFC12_TDMg3$p2inp1vec <- is.element(NMFC12_TDMg3$player2, player1vector)

addPlayer1 <- NMFC12_TDMg3[ which(NMFC12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_TDMg3[ which(NMFC12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_TDMg2 <- rbind(NMFC12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
NMFC12_TDMft <- ftable(NMFC12_TDMg2$player1, NMFC12_TDMg2$player2)
NMFC12_TDMft2 <- as.matrix(NMFC12_TDMft)
numRows <- nrow(NMFC12_TDMft2)
numCols <- ncol(NMFC12_TDMft2)
NMFC12_TDMft3 <- NMFC12_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC12_TDMTable <- graph.adjacency(NMFC12_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(NMFC12_TDMTable, vertex.label = V(NMFC12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
NMFC12_TDM.clusterCoef <- transitivity(NMFC12_TDMTable, type="global") #cluster coefficient
NMFC12_TDM.degreeCent <- centralization.degree(NMFC12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_TDMftn <- as.network.matrix(NMFC12_TDMft)
NMFC12_TDM.netDensity <- network.density(NMFC12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_TDM.entropy <- entropy(NMFC12_TDMft) #entropy

NMFC12_TDM.netMx <- cbind(NMFC12_TDM.netMx, NMFC12_TDM.clusterCoef, NMFC12_TDM.degreeCent$centralization,
                          NMFC12_TDM.netDensity, NMFC12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
NMFC12_SDg2 <- data.frame(NMFC12_SD)
NMFC12_SDg2 <- NMFC12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_SDg2$player1
player2vector <- NMFC12_SDg2$player2
NMFC12_SDg3 <- NMFC12_SDg2
NMFC12_SDg3$p1inp2vec <- is.element(NMFC12_SDg3$player1, player2vector)
NMFC12_SDg3$p2inp1vec <- is.element(NMFC12_SDg3$player2, player1vector)

addPlayer1 <- NMFC12_SDg3[ which(NMFC12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_SDg3[ which(NMFC12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_SDg2 <- rbind(NMFC12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
NMFC12_SDft <- ftable(NMFC12_SDg2$player1, NMFC12_SDg2$player2)
NMFC12_SDft2 <- as.matrix(NMFC12_SDft)
numRows <- nrow(NMFC12_SDft2)
numCols <- ncol(NMFC12_SDft2)
NMFC12_SDft3 <- NMFC12_SDft2[c(2:numRows) , c(2:numCols)]
NMFC12_SDTable <- graph.adjacency(NMFC12_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(NMFC12_SDTable, vertex.label = V(NMFC12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
NMFC12_SD.clusterCoef <- transitivity(NMFC12_SDTable, type="global") #cluster coefficient
NMFC12_SD.degreeCent <- centralization.degree(NMFC12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_SDftn <- as.network.matrix(NMFC12_SDft)
NMFC12_SD.netDensity <- network.density(NMFC12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_SD.entropy <- entropy(NMFC12_SDft) #entropy

NMFC12_SD.netMx <- cbind(NMFC12_SD.netMx, NMFC12_SD.clusterCoef, NMFC12_SD.degreeCent$centralization,
                         NMFC12_SD.netDensity, NMFC12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
NMFC12_TDg2 <- data.frame(NMFC12_TD)
NMFC12_TDg2 <- NMFC12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_TDg2$player1
player2vector <- NMFC12_TDg2$player2
NMFC12_TDg3 <- NMFC12_TDg2
NMFC12_TDg3$p1inp2vec <- is.element(NMFC12_TDg3$player1, player2vector)
NMFC12_TDg3$p2inp1vec <- is.element(NMFC12_TDg3$player2, player1vector)

addPlayer1 <- NMFC12_TDg3[ which(NMFC12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_TDg3[ which(NMFC12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_TDg2 <- rbind(NMFC12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
NMFC12_TDft <- ftable(NMFC12_TDg2$player1, NMFC12_TDg2$player2)
NMFC12_TDft2 <- as.matrix(NMFC12_TDft)
numRows <- nrow(NMFC12_TDft2)
numCols <- ncol(NMFC12_TDft2)
NMFC12_TDft3 <- NMFC12_TDft2[c(2:numRows) , c(2:numCols)]
NMFC12_TDTable <- graph.adjacency(NMFC12_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(NMFC12_TDTable, vertex.label = V(NMFC12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
NMFC12_TD.clusterCoef <- transitivity(NMFC12_TDTable, type="global") #cluster coefficient
NMFC12_TD.degreeCent <- centralization.degree(NMFC12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_TDftn <- as.network.matrix(NMFC12_TDft)
NMFC12_TD.netDensity <- network.density(NMFC12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_TD.entropy <- entropy(NMFC12_TDft) #entropy

NMFC12_TD.netMx <- cbind(NMFC12_TD.netMx, NMFC12_TD.clusterCoef, NMFC12_TD.degreeCent$centralization,
                         NMFC12_TD.netDensity, NMFC12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
NMFC12_QTg2 <- data.frame(NMFC12_QT)
NMFC12_QTg2 <- NMFC12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC12_QTg2$player1
player2vector <- NMFC12_QTg2$player2
NMFC12_QTg3 <- NMFC12_QTg2
NMFC12_QTg3$p1inp2vec <- is.element(NMFC12_QTg3$player1, player2vector)
NMFC12_QTg3$p2inp1vec <- is.element(NMFC12_QTg3$player2, player1vector)

addPlayer1 <- NMFC12_QTg3[ which(NMFC12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC12_QTg3[ which(NMFC12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC12_QTg2 <- rbind(NMFC12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
NMFC12_QTft <- ftable(NMFC12_QTg2$player1, NMFC12_QTg2$player2)
NMFC12_QTft2 <- as.matrix(NMFC12_QTft)
numRows <- nrow(NMFC12_QTft2)
numCols <- ncol(NMFC12_QTft2)
NMFC12_QTft3 <- NMFC12_QTft2[c(2:numRows) , c(2:numCols)]
NMFC12_QTTable <- graph.adjacency(NMFC12_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(NMFC12_QTTable, vertex.label = V(NMFC12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
NMFC12_QT.clusterCoef <- transitivity(NMFC12_QTTable, type="global") #cluster coefficient
NMFC12_QT.degreeCent <- centralization.degree(NMFC12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC12_QTftn <- as.network.matrix(NMFC12_QTft)
NMFC12_QT.netDensity <- network.density(NMFC12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC12_QT.entropy <- entropy(NMFC12_QTft) #entropy

NMFC12_QT.netMx <- cbind(NMFC12_QT.netMx, NMFC12_QT.clusterCoef, NMFC12_QT.degreeCent$centralization,
                         NMFC12_QT.netDensity, NMFC12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC12_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************

round = 12
teamName = "PORT"
KIoutcome = "Goal_F"
PORT12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
PORT12_Gg2 <- data.frame(PORT12_G)
PORT12_Gg2 <- PORT12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_Gg2$player1
player2vector <- PORT12_Gg2$player2
PORT12_Gg3 <- PORT12_Gg2
PORT12_Gg3$p1inp2vec <- is.element(PORT12_Gg3$player1, player2vector)
PORT12_Gg3$p2inp1vec <- is.element(PORT12_Gg3$player2, player1vector)

addPlayer1 <- PORT12_Gg3[ which(PORT12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT12_Gg3[ which(PORT12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_Gg2 <- rbind(PORT12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
PORT12_Gft <- ftable(PORT12_Gg2$player1, PORT12_Gg2$player2)
PORT12_Gft2 <- as.matrix(PORT12_Gft)
numRows <- nrow(PORT12_Gft2)
numCols <- ncol(PORT12_Gft2)
PORT12_Gft3 <- PORT12_Gft2[c(2:numRows) , c(2:numCols)]
PORT12_GTable <- graph.adjacency(PORT12_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(PORT12_GTable, vertex.label = V(PORT12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
PORT12_G.clusterCoef <- transitivity(PORT12_GTable, type="global") #cluster coefficient
PORT12_G.degreeCent <- centralization.degree(PORT12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_Gftn <- as.network.matrix(PORT12_Gft)
PORT12_G.netDensity <- network.density(PORT12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_G.entropy <- entropy(PORT12_Gft) #entropy

PORT12_G.netMx <- cbind(PORT12_G.netMx, PORT12_G.clusterCoef, PORT12_G.degreeCent$centralization,
                        PORT12_G.netDensity, PORT12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "PORT"
KIoutcome = "Behind_F"
PORT12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
PORT12_Bg2 <- data.frame(PORT12_B)
PORT12_Bg2 <- PORT12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_Bg2$player1
player2vector <- PORT12_Bg2$player2
PORT12_Bg3 <- PORT12_Bg2
PORT12_Bg3$p1inp2vec <- is.element(PORT12_Bg3$player1, player2vector)
PORT12_Bg3$p2inp1vec <- is.element(PORT12_Bg3$player2, player1vector)

addPlayer1 <- PORT12_Bg3[ which(PORT12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT12_Bg3[ which(PORT12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_Bg2 <- rbind(PORT12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
PORT12_Bft <- ftable(PORT12_Bg2$player1, PORT12_Bg2$player2)
PORT12_Bft2 <- as.matrix(PORT12_Bft)
numRows <- nrow(PORT12_Bft2)
numCols <- ncol(PORT12_Bft2)
PORT12_Bft3 <- PORT12_Bft2[c(2:numRows) , c(2:numCols)]
PORT12_BTable <- graph.adjacency(PORT12_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(PORT12_BTable, vertex.label = V(PORT12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
PORT12_B.clusterCoef <- transitivity(PORT12_BTable, type="global") #cluster coefficient
PORT12_B.degreeCent <- centralization.degree(PORT12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_Bftn <- as.network.matrix(PORT12_Bft)
PORT12_B.netDensity <- network.density(PORT12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_B.entropy <- entropy(PORT12_Bft) #entropy

PORT12_B.netMx <- cbind(PORT12_B.netMx, PORT12_B.clusterCoef, PORT12_B.degreeCent$centralization,
                        PORT12_B.netDensity, PORT12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************

round = 12
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
PORT12_SFg2 <- data.frame(PORT12_SF)
PORT12_SFg2 <- PORT12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_SFg2$player1
player2vector <- PORT12_SFg2$player2
PORT12_SFg3 <- PORT12_SFg2
PORT12_SFg3$p1inp2vec <- is.element(PORT12_SFg3$player1, player2vector)
PORT12_SFg3$p2inp1vec <- is.element(PORT12_SFg3$player2, player1vector)

addPlayer1 <- PORT12_SFg3[ which(PORT12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- PORT12_SFg3[ which(PORT12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_SFg2 <- rbind(PORT12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
PORT12_SFft <- ftable(PORT12_SFg2$player1, PORT12_SFg2$player2)
PORT12_SFft2 <- as.matrix(PORT12_SFft)
numRows <- nrow(PORT12_SFft2)
numCols <- ncol(PORT12_SFft2)
PORT12_SFft3 <- PORT12_SFft2[c(2:numRows) , c(2:numCols)]
PORT12_SFTable <- graph.adjacency(PORT12_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(PORT12_SFTable, vertex.label = V(PORT12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
PORT12_SF.clusterCoef <- transitivity(PORT12_SFTable, type="global") #cluster coefficient
PORT12_SF.degreeCent <- centralization.degree(PORT12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_SFftn <- as.network.matrix(PORT12_SFft)
PORT12_SF.netDensity <- network.density(PORT12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_SF.entropy <- entropy(PORT12_SFft) #entropy

PORT12_SF.netMx <- cbind(PORT12_SF.netMx, PORT12_SF.clusterCoef, PORT12_SF.degreeCent$centralization,
                         PORT12_SF.netDensity, PORT12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************
#NA

round = 12
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
PORT12_TFg2 <- data.frame(PORT12_TF)
PORT12_TFg2 <- PORT12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_TFg2$player1
player2vector <- PORT12_TFg2$player2
PORT12_TFg3 <- PORT12_TFg2
PORT12_TFg3$p1inp2vec <- is.element(PORT12_TFg3$player1, player2vector)
PORT12_TFg3$p2inp1vec <- is.element(PORT12_TFg3$player2, player1vector)

addPlayer1 <- PORT12_TFg3[ which(PORT12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT12_TFg2 <- rbind(PORT12_TFg2, addPlayer1)

#ROUND 12, FWD Turnover graph using weighted edges
PORT12_TFft <- ftable(PORT12_TFg2$player1, PORT12_TFg2$player2)
PORT12_TFft2 <- as.matrix(PORT12_TFft)
numRows <- nrow(PORT12_TFft2)
numCols <- ncol(PORT12_TFft2)
PORT12_TFft3 <- PORT12_TFft2[c(2:numRows) , c(1:numCols)]
PORT12_TFTable <- graph.adjacency(PORT12_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(PORT12_TFTable, vertex.label = V(PORT12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
PORT12_TF.clusterCoef <- transitivity(PORT12_TFTable, type="global") #cluster coefficient
PORT12_TF.degreeCent <- centralization.degree(PORT12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_TFftn <- as.network.matrix(PORT12_TFft)
PORT12_TF.netDensity <- network.density(PORT12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_TF.entropy <- entropy(PORT12_TFft) #entropy

PORT12_TF.netMx <- cbind(PORT12_TF.netMx, PORT12_TF.clusterCoef, PORT12_TF.degreeCent$centralization,
                         PORT12_TF.netDensity, PORT12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************
#NA

round = 12
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
PORT12_SAMg2 <- data.frame(PORT12_SAM)
PORT12_SAMg2 <- PORT12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_SAMg2$player1
player2vector <- PORT12_SAMg2$player2
PORT12_SAMg3 <- PORT12_SAMg2
PORT12_SAMg3$p1inp2vec <- is.element(PORT12_SAMg3$player1, player2vector)
PORT12_SAMg3$p2inp1vec <- is.element(PORT12_SAMg3$player2, player1vector)

addPlayer1 <- PORT12_SAMg3[ which(PORT12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT12_SAMg3[ which(PORT12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_SAMg2 <- rbind(PORT12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
PORT12_SAMft <- ftable(PORT12_SAMg2$player1, PORT12_SAMg2$player2)
PORT12_SAMft2 <- as.matrix(PORT12_SAMft)
numRows <- nrow(PORT12_SAMft2)
numCols <- ncol(PORT12_SAMft2)
PORT12_SAMft3 <- PORT12_SAMft2[c(2:numRows) , c(2:numCols)]
PORT12_SAMTable <- graph.adjacency(PORT12_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(PORT12_SAMTable, vertex.label = V(PORT12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
PORT12_SAM.clusterCoef <- transitivity(PORT12_SAMTable, type="global") #cluster coefficient
PORT12_SAM.degreeCent <- centralization.degree(PORT12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_SAMftn <- as.network.matrix(PORT12_SAMft)
PORT12_SAM.netDensity <- network.density(PORT12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_SAM.entropy <- entropy(PORT12_SAMft) #entropy

PORT12_SAM.netMx <- cbind(PORT12_SAM.netMx, PORT12_SAM.clusterCoef, PORT12_SAM.degreeCent$centralization,
                          PORT12_SAM.netDensity, PORT12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************

round = 12
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
PORT12_TAMg2 <- data.frame(PORT12_TAM)
PORT12_TAMg2 <- PORT12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_TAMg2$player1
player2vector <- PORT12_TAMg2$player2
PORT12_TAMg3 <- PORT12_TAMg2
PORT12_TAMg3$p1inp2vec <- is.element(PORT12_TAMg3$player1, player2vector)
PORT12_TAMg3$p2inp1vec <- is.element(PORT12_TAMg3$player2, player1vector)

addPlayer1 <- PORT12_TAMg3[ which(PORT12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT12_TAMg3[ which(PORT12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_TAMg2 <- rbind(PORT12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
PORT12_TAMft <- ftable(PORT12_TAMg2$player1, PORT12_TAMg2$player2)
PORT12_TAMft2 <- as.matrix(PORT12_TAMft)
numRows <- nrow(PORT12_TAMft2)
numCols <- ncol(PORT12_TAMft2)
PORT12_TAMft3 <- PORT12_TAMft2[c(2:numRows) , c(2:numCols)]
PORT12_TAMTable <- graph.adjacency(PORT12_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(PORT12_TAMTable, vertex.label = V(PORT12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
PORT12_TAM.clusterCoef <- transitivity(PORT12_TAMTable, type="global") #cluster coefficient
PORT12_TAM.degreeCent <- centralization.degree(PORT12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_TAMftn <- as.network.matrix(PORT12_TAMft)
PORT12_TAM.netDensity <- network.density(PORT12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_TAM.entropy <- entropy(PORT12_TAMft) #entropy

PORT12_TAM.netMx <- cbind(PORT12_TAM.netMx, PORT12_TAM.clusterCoef, PORT12_TAM.degreeCent$centralization,
                          PORT12_TAM.netDensity, PORT12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************
#NA

round = 12
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
PORT12_SDMg2 <- data.frame(PORT12_SDM)
PORT12_SDMg2 <- PORT12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_SDMg2$player1
player2vector <- PORT12_SDMg2$player2
PORT12_SDMg3 <- PORT12_SDMg2
PORT12_SDMg3$p1inp2vec <- is.element(PORT12_SDMg3$player1, player2vector)
PORT12_SDMg3$p2inp1vec <- is.element(PORT12_SDMg3$player2, player1vector)

addPlayer1 <- PORT12_SDMg3[ which(PORT12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT12_SDMg3[ which(PORT12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_SDMg2 <- rbind(PORT12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
PORT12_SDMft <- ftable(PORT12_SDMg2$player1, PORT12_SDMg2$player2)
PORT12_SDMft2 <- as.matrix(PORT12_SDMft)
numRows <- nrow(PORT12_SDMft2)
numCols <- ncol(PORT12_SDMft2)
PORT12_SDMft3 <- PORT12_SDMft2[c(2:numRows) , c(2:numCols)]
PORT12_SDMTable <- graph.adjacency(PORT12_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(PORT12_SDMTable, vertex.label = V(PORT12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
PORT12_SDM.clusterCoef <- transitivity(PORT12_SDMTable, type="global") #cluster coefficient
PORT12_SDM.degreeCent <- centralization.degree(PORT12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_SDMftn <- as.network.matrix(PORT12_SDMft)
PORT12_SDM.netDensity <- network.density(PORT12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_SDM.entropy <- entropy(PORT12_SDMft) #entropy

PORT12_SDM.netMx <- cbind(PORT12_SDM.netMx, PORT12_SDM.clusterCoef, PORT12_SDM.degreeCent$centralization,
                          PORT12_SDM.netDensity, PORT12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
PORT12_TDMg2 <- data.frame(PORT12_TDM)
PORT12_TDMg2 <- PORT12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_TDMg2$player1
player2vector <- PORT12_TDMg2$player2
PORT12_TDMg3 <- PORT12_TDMg2
PORT12_TDMg3$p1inp2vec <- is.element(PORT12_TDMg3$player1, player2vector)
PORT12_TDMg3$p2inp1vec <- is.element(PORT12_TDMg3$player2, player1vector)

addPlayer1 <- PORT12_TDMg3[ which(PORT12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT12_TDMg3[ which(PORT12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_TDMg2 <- rbind(PORT12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
PORT12_TDMft <- ftable(PORT12_TDMg2$player1, PORT12_TDMg2$player2)
PORT12_TDMft2 <- as.matrix(PORT12_TDMft)
numRows <- nrow(PORT12_TDMft2)
numCols <- ncol(PORT12_TDMft2)
PORT12_TDMft3 <- PORT12_TDMft2[c(2:numRows) , c(2:numCols)]
PORT12_TDMTable <- graph.adjacency(PORT12_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(PORT12_TDMTable, vertex.label = V(PORT12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
PORT12_TDM.clusterCoef <- transitivity(PORT12_TDMTable, type="global") #cluster coefficient
PORT12_TDM.degreeCent <- centralization.degree(PORT12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_TDMftn <- as.network.matrix(PORT12_TDMft)
PORT12_TDM.netDensity <- network.density(PORT12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_TDM.entropy <- entropy(PORT12_TDMft) #entropy

PORT12_TDM.netMx <- cbind(PORT12_TDM.netMx, PORT12_TDM.clusterCoef, PORT12_TDM.degreeCent$centralization,
                          PORT12_TDM.netDensity, PORT12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
PORT12_SDg2 <- data.frame(PORT12_SD)
PORT12_SDg2 <- PORT12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_SDg2$player1
player2vector <- PORT12_SDg2$player2
PORT12_SDg3 <- PORT12_SDg2
PORT12_SDg3$p1inp2vec <- is.element(PORT12_SDg3$player1, player2vector)
PORT12_SDg3$p2inp1vec <- is.element(PORT12_SDg3$player2, player1vector)

addPlayer1 <- PORT12_SDg3[ which(PORT12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT12_SDg3[ which(PORT12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_SDg2 <- rbind(PORT12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
PORT12_SDft <- ftable(PORT12_SDg2$player1, PORT12_SDg2$player2)
PORT12_SDft2 <- as.matrix(PORT12_SDft)
numRows <- nrow(PORT12_SDft2)
numCols <- ncol(PORT12_SDft2)
PORT12_SDft3 <- PORT12_SDft2[c(2:numRows) , c(2:numCols)]
PORT12_SDTable <- graph.adjacency(PORT12_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(PORT12_SDTable, vertex.label = V(PORT12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
PORT12_SD.clusterCoef <- transitivity(PORT12_SDTable, type="global") #cluster coefficient
PORT12_SD.degreeCent <- centralization.degree(PORT12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_SDftn <- as.network.matrix(PORT12_SDft)
PORT12_SD.netDensity <- network.density(PORT12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_SD.entropy <- entropy(PORT12_SDft) #entropy

PORT12_SD.netMx <- cbind(PORT12_SD.netMx, PORT12_SD.clusterCoef, PORT12_SD.degreeCent$centralization,
                         PORT12_SD.netDensity, PORT12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
PORT12_TDg2 <- data.frame(PORT12_TD)
PORT12_TDg2 <- PORT12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_TDg2$player1
player2vector <- PORT12_TDg2$player2
PORT12_TDg3 <- PORT12_TDg2
PORT12_TDg3$p1inp2vec <- is.element(PORT12_TDg3$player1, player2vector)
PORT12_TDg3$p2inp1vec <- is.element(PORT12_TDg3$player2, player1vector)

addPlayer1 <- PORT12_TDg3[ which(PORT12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT12_TDg3[ which(PORT12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_TDg2 <- rbind(PORT12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
PORT12_TDft <- ftable(PORT12_TDg2$player1, PORT12_TDg2$player2)
PORT12_TDft2 <- as.matrix(PORT12_TDft)
numRows <- nrow(PORT12_TDft2)
numCols <- ncol(PORT12_TDft2)
PORT12_TDft3 <- PORT12_TDft2[c(2:numRows) , c(2:numCols)]
PORT12_TDTable <- graph.adjacency(PORT12_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(PORT12_TDTable, vertex.label = V(PORT12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
PORT12_TD.clusterCoef <- transitivity(PORT12_TDTable, type="global") #cluster coefficient
PORT12_TD.degreeCent <- centralization.degree(PORT12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_TDftn <- as.network.matrix(PORT12_TDft)
PORT12_TD.netDensity <- network.density(PORT12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_TD.entropy <- entropy(PORT12_TDft) #entropy

PORT12_TD.netMx <- cbind(PORT12_TD.netMx, PORT12_TD.clusterCoef, PORT12_TD.degreeCent$centralization,
                         PORT12_TD.netDensity, PORT12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
PORT12_QTg2 <- data.frame(PORT12_QT)
PORT12_QTg2 <- PORT12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT12_QTg2$player1
player2vector <- PORT12_QTg2$player2
PORT12_QTg3 <- PORT12_QTg2
PORT12_QTg3$p1inp2vec <- is.element(PORT12_QTg3$player1, player2vector)
PORT12_QTg3$p2inp1vec <- is.element(PORT12_QTg3$player2, player1vector)

addPlayer1 <- PORT12_QTg3[ which(PORT12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT12_QTg3[ which(PORT12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT12_QTg2 <- rbind(PORT12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
PORT12_QTft <- ftable(PORT12_QTg2$player1, PORT12_QTg2$player2)
PORT12_QTft2 <- as.matrix(PORT12_QTft)
numRows <- nrow(PORT12_QTft2)
numCols <- ncol(PORT12_QTft2)
PORT12_QTft3 <- PORT12_QTft2[c(2:numRows) , c(2:numCols)]
PORT12_QTTable <- graph.adjacency(PORT12_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(PORT12_QTTable, vertex.label = V(PORT12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
PORT12_QT.clusterCoef <- transitivity(PORT12_QTTable, type="global") #cluster coefficient
PORT12_QT.degreeCent <- centralization.degree(PORT12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT12_QTftn <- as.network.matrix(PORT12_QTft)
PORT12_QT.netDensity <- network.density(PORT12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT12_QT.entropy <- entropy(PORT12_QTft) #entropy

PORT12_QT.netMx <- cbind(PORT12_QT.netMx, PORT12_QT.clusterCoef, PORT12_QT.degreeCent$centralization,
                         PORT12_QT.netDensity, PORT12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT12_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************
#NA

round = 12
teamName = "RICH"
KIoutcome = "Goal_F"
RICH12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
RICH12_Gg2 <- data.frame(RICH12_G)
RICH12_Gg2 <- RICH12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_Gg2$player1
player2vector <- RICH12_Gg2$player2
RICH12_Gg3 <- RICH12_Gg2
RICH12_Gg3$p1inp2vec <- is.element(RICH12_Gg3$player1, player2vector)
RICH12_Gg3$p2inp1vec <- is.element(RICH12_Gg3$player2, player1vector)

addPlayer1 <- RICH12_Gg3[ which(RICH12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_Gg3[ which(RICH12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_Gg2 <- rbind(RICH12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
RICH12_Gft <- ftable(RICH12_Gg2$player1, RICH12_Gg2$player2)
RICH12_Gft2 <- as.matrix(RICH12_Gft)
numRows <- nrow(RICH12_Gft2)
numCols <- ncol(RICH12_Gft2)
RICH12_Gft3 <- RICH12_Gft2[c(2:numRows) , c(2:numCols)]
RICH12_GTable <- graph.adjacency(RICH12_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(RICH12_GTable, vertex.label = V(RICH12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
RICH12_G.clusterCoef <- transitivity(RICH12_GTable, type="global") #cluster coefficient
RICH12_G.degreeCent <- centralization.degree(RICH12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_Gftn <- as.network.matrix(RICH12_Gft)
RICH12_G.netDensity <- network.density(RICH12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_G.entropy <- entropy(RICH12_Gft) #entropy

RICH12_G.netMx <- cbind(RICH12_G.netMx, RICH12_G.clusterCoef, RICH12_G.degreeCent$centralization,
                        RICH12_G.netDensity, RICH12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************

round = 12
teamName = "RICH"
KIoutcome = "Behind_F"
RICH12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
RICH12_Bg2 <- data.frame(RICH12_B)
RICH12_Bg2 <- RICH12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_Bg2$player1
player2vector <- RICH12_Bg2$player2
RICH12_Bg3 <- RICH12_Bg2
RICH12_Bg3$p1inp2vec <- is.element(RICH12_Bg3$player1, player2vector)
RICH12_Bg3$p2inp1vec <- is.element(RICH12_Bg3$player2, player1vector)

addPlayer1 <- RICH12_Bg3[ which(RICH12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_Bg3[ which(RICH12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_Bg2 <- rbind(RICH12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
RICH12_Bft <- ftable(RICH12_Bg2$player1, RICH12_Bg2$player2)
RICH12_Bft2 <- as.matrix(RICH12_Bft)
numRows <- nrow(RICH12_Bft2)
numCols <- ncol(RICH12_Bft2)
RICH12_Bft3 <- RICH12_Bft2[c(2:numRows) , c(2:numCols)]
RICH12_BTable <- graph.adjacency(RICH12_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(RICH12_BTable, vertex.label = V(RICH12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
RICH12_B.clusterCoef <- transitivity(RICH12_BTable, type="global") #cluster coefficient
RICH12_B.degreeCent <- centralization.degree(RICH12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_Bftn <- as.network.matrix(RICH12_Bft)
RICH12_B.netDensity <- network.density(RICH12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_B.entropy <- entropy(RICH12_Bft) #entropy

RICH12_B.netMx <- cbind(RICH12_B.netMx, RICH12_B.clusterCoef, RICH12_B.degreeCent$centralization,
                        RICH12_B.netDensity, RICH12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************
#NA

round = 12
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
RICH12_SFg2 <- data.frame(RICH12_SF)
RICH12_SFg2 <- RICH12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_SFg2$player1
player2vector <- RICH12_SFg2$player2
RICH12_SFg3 <- RICH12_SFg2
RICH12_SFg3$p1inp2vec <- is.element(RICH12_SFg3$player1, player2vector)
RICH12_SFg3$p2inp1vec <- is.element(RICH12_SFg3$player2, player1vector)

addPlayer1 <- RICH12_SFg3[ which(RICH12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_SFg3[ which(RICH12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_SFg2 <- rbind(RICH12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
RICH12_SFft <- ftable(RICH12_SFg2$player1, RICH12_SFg2$player2)
RICH12_SFft2 <- as.matrix(RICH12_SFft)
numRows <- nrow(RICH12_SFft2)
numCols <- ncol(RICH12_SFft2)
RICH12_SFft3 <- RICH12_SFft2[c(2:numRows) , c(2:numCols)]
RICH12_SFTable <- graph.adjacency(RICH12_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(RICH12_SFTable, vertex.label = V(RICH12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
RICH12_SF.clusterCoef <- transitivity(RICH12_SFTable, type="global") #cluster coefficient
RICH12_SF.degreeCent <- centralization.degree(RICH12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_SFftn <- as.network.matrix(RICH12_SFft)
RICH12_SF.netDensity <- network.density(RICH12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_SF.entropy <- entropy(RICH12_SFft) #entropy

RICH12_SF.netMx <- cbind(RICH12_SF.netMx, RICH12_SF.clusterCoef, RICH12_SF.degreeCent$centralization,
                         RICH12_SF.netDensity, RICH12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************
#NA

round = 12
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
RICH12_TFg2 <- data.frame(RICH12_TF)
RICH12_TFg2 <- RICH12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_TFg2$player1
player2vector <- RICH12_TFg2$player2
RICH12_TFg3 <- RICH12_TFg2
RICH12_TFg3$p1inp2vec <- is.element(RICH12_TFg3$player1, player2vector)
RICH12_TFg3$p2inp1vec <- is.element(RICH12_TFg3$player2, player1vector)

addPlayer1 <- RICH12_TFg3[ which(RICH12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_TFg3[ which(RICH12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_TFg2 <- rbind(RICH12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
RICH12_TFft <- ftable(RICH12_TFg2$player1, RICH12_TFg2$player2)
RICH12_TFft2 <- as.matrix(RICH12_TFft)
numRows <- nrow(RICH12_TFft2)
numCols <- ncol(RICH12_TFft2)
RICH12_TFft3 <- RICH12_TFft2[c(2:numRows) , c(2:numCols)]
RICH12_TFTable <- graph.adjacency(RICH12_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(RICH12_TFTable, vertex.label = V(RICH12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
RICH12_TF.clusterCoef <- transitivity(RICH12_TFTable, type="global") #cluster coefficient
RICH12_TF.degreeCent <- centralization.degree(RICH12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_TFftn <- as.network.matrix(RICH12_TFft)
RICH12_TF.netDensity <- network.density(RICH12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_TF.entropy <- entropy(RICH12_TFft) #entropy

RICH12_TF.netMx <- cbind(RICH12_TF.netMx, RICH12_TF.clusterCoef, RICH12_TF.degreeCent$centralization,
                         RICH12_TF.netDensity, RICH12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************

round = 12
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
RICH12_SAMg2 <- data.frame(RICH12_SAM)
RICH12_SAMg2 <- RICH12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_SAMg2$player1
player2vector <- RICH12_SAMg2$player2
RICH12_SAMg3 <- RICH12_SAMg2
RICH12_SAMg3$p1inp2vec <- is.element(RICH12_SAMg3$player1, player2vector)
RICH12_SAMg3$p2inp1vec <- is.element(RICH12_SAMg3$player2, player1vector)

addPlayer1 <- RICH12_SAMg3[ which(RICH12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_SAMg3[ which(RICH12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_SAMg2 <- rbind(RICH12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
RICH12_SAMft <- ftable(RICH12_SAMg2$player1, RICH12_SAMg2$player2)
RICH12_SAMft2 <- as.matrix(RICH12_SAMft)
numRows <- nrow(RICH12_SAMft2)
numCols <- ncol(RICH12_SAMft2)
RICH12_SAMft3 <- RICH12_SAMft2[c(2:numRows) , c(2:numCols)]
RICH12_SAMTable <- graph.adjacency(RICH12_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(RICH12_SAMTable, vertex.label = V(RICH12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
RICH12_SAM.clusterCoef <- transitivity(RICH12_SAMTable, type="global") #cluster coefficient
RICH12_SAM.degreeCent <- centralization.degree(RICH12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_SAMftn <- as.network.matrix(RICH12_SAMft)
RICH12_SAM.netDensity <- network.density(RICH12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_SAM.entropy <- entropy(RICH12_SAMft) #entropy

RICH12_SAM.netMx <- cbind(RICH12_SAM.netMx, RICH12_SAM.clusterCoef, RICH12_SAM.degreeCent$centralization,
                          RICH12_SAM.netDensity, RICH12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************

round = 12
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
RICH12_TAMg2 <- data.frame(RICH12_TAM)
RICH12_TAMg2 <- RICH12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_TAMg2$player1
player2vector <- RICH12_TAMg2$player2
RICH12_TAMg3 <- RICH12_TAMg2
RICH12_TAMg3$p1inp2vec <- is.element(RICH12_TAMg3$player1, player2vector)
RICH12_TAMg3$p2inp1vec <- is.element(RICH12_TAMg3$player2, player1vector)

addPlayer1 <- RICH12_TAMg3[ which(RICH12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_TAMg3[ which(RICH12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_TAMg2 <- rbind(RICH12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
RICH12_TAMft <- ftable(RICH12_TAMg2$player1, RICH12_TAMg2$player2)
RICH12_TAMft2 <- as.matrix(RICH12_TAMft)
numRows <- nrow(RICH12_TAMft2)
numCols <- ncol(RICH12_TAMft2)
RICH12_TAMft3 <- RICH12_TAMft2[c(2:numRows) , c(2:numCols)]
RICH12_TAMTable <- graph.adjacency(RICH12_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(RICH12_TAMTable, vertex.label = V(RICH12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
RICH12_TAM.clusterCoef <- transitivity(RICH12_TAMTable, type="global") #cluster coefficient
RICH12_TAM.degreeCent <- centralization.degree(RICH12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_TAMftn <- as.network.matrix(RICH12_TAMft)
RICH12_TAM.netDensity <- network.density(RICH12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_TAM.entropy <- entropy(RICH12_TAMft) #entropy

RICH12_TAM.netMx <- cbind(RICH12_TAM.netMx, RICH12_TAM.clusterCoef, RICH12_TAM.degreeCent$centralization,
                          RICH12_TAM.netDensity, RICH12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************
#NA

round = 12
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
RICH12_SDMg2 <- data.frame(RICH12_SDM)
RICH12_SDMg2 <- RICH12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_SDMg2$player1
player2vector <- RICH12_SDMg2$player2
RICH12_SDMg3 <- RICH12_SDMg2
RICH12_SDMg3$p1inp2vec <- is.element(RICH12_SDMg3$player1, player2vector)
RICH12_SDMg3$p2inp1vec <- is.element(RICH12_SDMg3$player2, player1vector)

addPlayer1 <- RICH12_SDMg3[ which(RICH12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_SDMg3[ which(RICH12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_SDMg2 <- rbind(RICH12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
RICH12_SDMft <- ftable(RICH12_SDMg2$player1, RICH12_SDMg2$player2)
RICH12_SDMft2 <- as.matrix(RICH12_SDMft)
numRows <- nrow(RICH12_SDMft2)
numCols <- ncol(RICH12_SDMft2)
RICH12_SDMft3 <- RICH12_SDMft2[c(2:numRows) , c(2:numCols)]
RICH12_SDMTable <- graph.adjacency(RICH12_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(RICH12_SDMTable, vertex.label = V(RICH12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
RICH12_SDM.clusterCoef <- transitivity(RICH12_SDMTable, type="global") #cluster coefficient
RICH12_SDM.degreeCent <- centralization.degree(RICH12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_SDMftn <- as.network.matrix(RICH12_SDMft)
RICH12_SDM.netDensity <- network.density(RICH12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_SDM.entropy <- entropy(RICH12_SDMft) #entropy

RICH12_SDM.netMx <- cbind(RICH12_SDM.netMx, RICH12_SDM.clusterCoef, RICH12_SDM.degreeCent$centralization,
                          RICH12_SDM.netDensity, RICH12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
RICH12_TDMg2 <- data.frame(RICH12_TDM)
RICH12_TDMg2 <- RICH12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_TDMg2$player1
player2vector <- RICH12_TDMg2$player2
RICH12_TDMg3 <- RICH12_TDMg2
RICH12_TDMg3$p1inp2vec <- is.element(RICH12_TDMg3$player1, player2vector)
RICH12_TDMg3$p2inp1vec <- is.element(RICH12_TDMg3$player2, player1vector)

addPlayer1 <- RICH12_TDMg3[ which(RICH12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_TDMg3[ which(RICH12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_TDMg2 <- rbind(RICH12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
RICH12_TDMft <- ftable(RICH12_TDMg2$player1, RICH12_TDMg2$player2)
RICH12_TDMft2 <- as.matrix(RICH12_TDMft)
numRows <- nrow(RICH12_TDMft2)
numCols <- ncol(RICH12_TDMft2)
RICH12_TDMft3 <- RICH12_TDMft2[c(2:numRows) , c(2:numCols)]
RICH12_TDMTable <- graph.adjacency(RICH12_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(RICH12_TDMTable, vertex.label = V(RICH12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
RICH12_TDM.clusterCoef <- transitivity(RICH12_TDMTable, type="global") #cluster coefficient
RICH12_TDM.degreeCent <- centralization.degree(RICH12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_TDMftn <- as.network.matrix(RICH12_TDMft)
RICH12_TDM.netDensity <- network.density(RICH12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_TDM.entropy <- entropy(RICH12_TDMft) #entropy

RICH12_TDM.netMx <- cbind(RICH12_TDM.netMx, RICH12_TDM.clusterCoef, RICH12_TDM.degreeCent$centralization,
                          RICH12_TDM.netDensity, RICH12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
RICH12_SDg2 <- data.frame(RICH12_SD)
RICH12_SDg2 <- RICH12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_SDg2$player1
player2vector <- RICH12_SDg2$player2
RICH12_SDg3 <- RICH12_SDg2
RICH12_SDg3$p1inp2vec <- is.element(RICH12_SDg3$player1, player2vector)
RICH12_SDg3$p2inp1vec <- is.element(RICH12_SDg3$player2, player1vector)

addPlayer1 <- RICH12_SDg3[ which(RICH12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_SDg3[ which(RICH12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_SDg2 <- rbind(RICH12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
RICH12_SDft <- ftable(RICH12_SDg2$player1, RICH12_SDg2$player2)
RICH12_SDft2 <- as.matrix(RICH12_SDft)
numRows <- nrow(RICH12_SDft2)
numCols <- ncol(RICH12_SDft2)
RICH12_SDft3 <- RICH12_SDft2[c(2:numRows) , c(2:numCols)]
RICH12_SDTable <- graph.adjacency(RICH12_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(RICH12_SDTable, vertex.label = V(RICH12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
RICH12_SD.clusterCoef <- transitivity(RICH12_SDTable, type="global") #cluster coefficient
RICH12_SD.degreeCent <- centralization.degree(RICH12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_SDftn <- as.network.matrix(RICH12_SDft)
RICH12_SD.netDensity <- network.density(RICH12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_SD.entropy <- entropy(RICH12_SDft) #entropy

RICH12_SD.netMx <- cbind(RICH12_SD.netMx, RICH12_SD.clusterCoef, RICH12_SD.degreeCent$centralization,
                         RICH12_SD.netDensity, RICH12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
RICH12_TDg2 <- data.frame(RICH12_TD)
RICH12_TDg2 <- RICH12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_TDg2$player1
player2vector <- RICH12_TDg2$player2
RICH12_TDg3 <- RICH12_TDg2
RICH12_TDg3$p1inp2vec <- is.element(RICH12_TDg3$player1, player2vector)
RICH12_TDg3$p2inp1vec <- is.element(RICH12_TDg3$player2, player1vector)

addPlayer1 <- RICH12_TDg3[ which(RICH12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_TDg3[ which(RICH12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_TDg2 <- rbind(RICH12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
RICH12_TDft <- ftable(RICH12_TDg2$player1, RICH12_TDg2$player2)
RICH12_TDft2 <- as.matrix(RICH12_TDft)
numRows <- nrow(RICH12_TDft2)
numCols <- ncol(RICH12_TDft2)
RICH12_TDft3 <- RICH12_TDft2[c(2:numRows) , c(2:numCols)]
RICH12_TDTable <- graph.adjacency(RICH12_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(RICH12_TDTable, vertex.label = V(RICH12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
RICH12_TD.clusterCoef <- transitivity(RICH12_TDTable, type="global") #cluster coefficient
RICH12_TD.degreeCent <- centralization.degree(RICH12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_TDftn <- as.network.matrix(RICH12_TDft)
RICH12_TD.netDensity <- network.density(RICH12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_TD.entropy <- entropy(RICH12_TDft) #entropy

RICH12_TD.netMx <- cbind(RICH12_TD.netMx, RICH12_TD.clusterCoef, RICH12_TD.degreeCent$centralization,
                         RICH12_TD.netDensity, RICH12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
RICH12_QTg2 <- data.frame(RICH12_QT)
RICH12_QTg2 <- RICH12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH12_QTg2$player1
player2vector <- RICH12_QTg2$player2
RICH12_QTg3 <- RICH12_QTg2
RICH12_QTg3$p1inp2vec <- is.element(RICH12_QTg3$player1, player2vector)
RICH12_QTg3$p2inp1vec <- is.element(RICH12_QTg3$player2, player1vector)

addPlayer1 <- RICH12_QTg3[ which(RICH12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH12_QTg3[ which(RICH12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH12_QTg2 <- rbind(RICH12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
RICH12_QTft <- ftable(RICH12_QTg2$player1, RICH12_QTg2$player2)
RICH12_QTft2 <- as.matrix(RICH12_QTft)
numRows <- nrow(RICH12_QTft2)
numCols <- ncol(RICH12_QTft2)
RICH12_QTft3 <- RICH12_QTft2[c(2:numRows) , c(2:numCols)]
RICH12_QTTable <- graph.adjacency(RICH12_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(RICH12_QTTable, vertex.label = V(RICH12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
RICH12_QT.clusterCoef <- transitivity(RICH12_QTTable, type="global") #cluster coefficient
RICH12_QT.degreeCent <- centralization.degree(RICH12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH12_QTftn <- as.network.matrix(RICH12_QTft)
RICH12_QT.netDensity <- network.density(RICH12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH12_QT.entropy <- entropy(RICH12_QTft) #entropy

RICH12_QT.netMx <- cbind(RICH12_QT.netMx, RICH12_QT.clusterCoef, RICH12_QT.degreeCent$centralization,
                         RICH12_QT.netDensity, RICH12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH12_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************

round = 12
teamName = "WB"
KIoutcome = "Goal_F"
WB12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
WB12_Gg2 <- data.frame(WB12_G)
WB12_Gg2 <- WB12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_Gg2$player1
player2vector <- WB12_Gg2$player2
WB12_Gg3 <- WB12_Gg2
WB12_Gg3$p1inp2vec <- is.element(WB12_Gg3$player1, player2vector)
WB12_Gg3$p2inp1vec <- is.element(WB12_Gg3$player2, player1vector)

addPlayer1 <- WB12_Gg3[ which(WB12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_Gg3[ which(WB12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_Gg2 <- rbind(WB12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
WB12_Gft <- ftable(WB12_Gg2$player1, WB12_Gg2$player2)
WB12_Gft2 <- as.matrix(WB12_Gft)
numRows <- nrow(WB12_Gft2)
numCols <- ncol(WB12_Gft2)
WB12_Gft3 <- WB12_Gft2[c(2:numRows) , c(2:numCols)]
WB12_GTable <- graph.adjacency(WB12_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(WB12_GTable, vertex.label = V(WB12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
WB12_G.clusterCoef <- transitivity(WB12_GTable, type="global") #cluster coefficient
WB12_G.degreeCent <- centralization.degree(WB12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_Gftn <- as.network.matrix(WB12_Gft)
WB12_G.netDensity <- network.density(WB12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_G.entropy <- entropy(WB12_Gft) #entropy

WB12_G.netMx <- cbind(WB12_G.netMx, WB12_G.clusterCoef, WB12_G.degreeCent$centralization,
                      WB12_G.netDensity, WB12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "WB"
KIoutcome = "Behind_F"
WB12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
WB12_Bg2 <- data.frame(WB12_B)
WB12_Bg2 <- WB12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_Bg2$player1
player2vector <- WB12_Bg2$player2
WB12_Bg3 <- WB12_Bg2
WB12_Bg3$p1inp2vec <- is.element(WB12_Bg3$player1, player2vector)
WB12_Bg3$p2inp1vec <- is.element(WB12_Bg3$player2, player1vector)

addPlayer1 <- WB12_Bg3[ which(WB12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_Bg3[ which(WB12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_Bg2 <- rbind(WB12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
WB12_Bft <- ftable(WB12_Bg2$player1, WB12_Bg2$player2)
WB12_Bft2 <- as.matrix(WB12_Bft)
numRows <- nrow(WB12_Bft2)
numCols <- ncol(WB12_Bft2)
WB12_Bft3 <- WB12_Bft2[c(2:numRows) , c(2:numCols)]
WB12_BTable <- graph.adjacency(WB12_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(WB12_BTable, vertex.label = V(WB12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
WB12_B.clusterCoef <- transitivity(WB12_BTable, type="global") #cluster coefficient
WB12_B.degreeCent <- centralization.degree(WB12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_Bftn <- as.network.matrix(WB12_Bft)
WB12_B.netDensity <- network.density(WB12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_B.entropy <- entropy(WB12_Bft) #entropy

WB12_B.netMx <- cbind(WB12_B.netMx, WB12_B.clusterCoef, WB12_B.degreeCent$centralization,
                      WB12_B.netDensity, WB12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************
#NA

round = 12
teamName = "WB"
KIoutcome = "Stoppage_F"
WB12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
WB12_SFg2 <- data.frame(WB12_SF)
WB12_SFg2 <- WB12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_SFg2$player1
player2vector <- WB12_SFg2$player2
WB12_SFg3 <- WB12_SFg2
WB12_SFg3$p1inp2vec <- is.element(WB12_SFg3$player1, player2vector)
WB12_SFg3$p2inp1vec <- is.element(WB12_SFg3$player2, player1vector)

addPlayer1 <- WB12_SFg3[ which(WB12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_SFg3[ which(WB12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_SFg2 <- rbind(WB12_SFg2, addPlayers)

#ROUND 12, FWD Stoppage graph using weighted edges
WB12_SFft <- ftable(WB12_SFg2$player1, WB12_SFg2$player2)
WB12_SFft2 <- as.matrix(WB12_SFft)
numRows <- nrow(WB12_SFft2)
numCols <- ncol(WB12_SFft2)
WB12_SFft3 <- WB12_SFft2[c(2:numRows) , c(2:numCols)]
WB12_SFTable <- graph.adjacency(WB12_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(WB12_SFTable, vertex.label = V(WB12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
WB12_SF.clusterCoef <- transitivity(WB12_SFTable, type="global") #cluster coefficient
WB12_SF.degreeCent <- centralization.degree(WB12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_SFftn <- as.network.matrix(WB12_SFft)
WB12_SF.netDensity <- network.density(WB12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_SF.entropy <- entropy(WB12_SFft) #entropy

WB12_SF.netMx <- cbind(WB12_SF.netMx, WB12_SF.clusterCoef, WB12_SF.degreeCent$centralization,
                       WB12_SF.netDensity, WB12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************
#NA

round = 12
teamName = "WB"
KIoutcome = "Turnover_F"
WB12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
WB12_TFg2 <- data.frame(WB12_TF)
WB12_TFg2 <- WB12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_TFg2$player1
player2vector <- WB12_TFg2$player2
WB12_TFg3 <- WB12_TFg2
WB12_TFg3$p1inp2vec <- is.element(WB12_TFg3$player1, player2vector)
WB12_TFg3$p2inp1vec <- is.element(WB12_TFg3$player2, player1vector)

addPlayer1 <- WB12_TFg3[ which(WB12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_TFg3[ which(WB12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_TFg2 <- rbind(WB12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
WB12_TFft <- ftable(WB12_TFg2$player1, WB12_TFg2$player2)
WB12_TFft2 <- as.matrix(WB12_TFft)
numRows <- nrow(WB12_TFft2)
numCols <- ncol(WB12_TFft2)
WB12_TFft3 <- WB12_TFft2[c(2:numRows) , c(2:numCols)]
WB12_TFTable <- graph.adjacency(WB12_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(WB12_TFTable, vertex.label = V(WB12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
WB12_TF.clusterCoef <- transitivity(WB12_TFTable, type="global") #cluster coefficient
WB12_TF.degreeCent <- centralization.degree(WB12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_TFftn <- as.network.matrix(WB12_TFft)
WB12_TF.netDensity <- network.density(WB12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_TF.entropy <- entropy(WB12_TFft) #entropy

WB12_TF.netMx <- cbind(WB12_TF.netMx, WB12_TF.clusterCoef, WB12_TF.degreeCent$centralization,
                       WB12_TF.netDensity, WB12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************

round = 12
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
WB12_SAMg2 <- data.frame(WB12_SAM)
WB12_SAMg2 <- WB12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_SAMg2$player1
player2vector <- WB12_SAMg2$player2
WB12_SAMg3 <- WB12_SAMg2
WB12_SAMg3$p1inp2vec <- is.element(WB12_SAMg3$player1, player2vector)
WB12_SAMg3$p2inp1vec <- is.element(WB12_SAMg3$player2, player1vector)

addPlayer1 <- WB12_SAMg3[ which(WB12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_SAMg3[ which(WB12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_SAMg2 <- rbind(WB12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
WB12_SAMft <- ftable(WB12_SAMg2$player1, WB12_SAMg2$player2)
WB12_SAMft2 <- as.matrix(WB12_SAMft)
numRows <- nrow(WB12_SAMft2)
numCols <- ncol(WB12_SAMft2)
WB12_SAMft3 <- WB12_SAMft2[c(2:numRows) , c(2:numCols)]
WB12_SAMTable <- graph.adjacency(WB12_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(WB12_SAMTable, vertex.label = V(WB12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
WB12_SAM.clusterCoef <- transitivity(WB12_SAMTable, type="global") #cluster coefficient
WB12_SAM.degreeCent <- centralization.degree(WB12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_SAMftn <- as.network.matrix(WB12_SAMft)
WB12_SAM.netDensity <- network.density(WB12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_SAM.entropy <- entropy(WB12_SAMft) #entropy

WB12_SAM.netMx <- cbind(WB12_SAM.netMx, WB12_SAM.clusterCoef, WB12_SAM.degreeCent$centralization,
                        WB12_SAM.netDensity, WB12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************

round = 12
teamName = "WB"
KIoutcome = "Turnover_AM"
WB12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
WB12_TAMg2 <- data.frame(WB12_TAM)
WB12_TAMg2 <- WB12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_TAMg2$player1
player2vector <- WB12_TAMg2$player2
WB12_TAMg3 <- WB12_TAMg2
WB12_TAMg3$p1inp2vec <- is.element(WB12_TAMg3$player1, player2vector)
WB12_TAMg3$p2inp1vec <- is.element(WB12_TAMg3$player2, player1vector)

addPlayer1 <- WB12_TAMg3[ which(WB12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_TAMg3[ which(WB12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_TAMg2 <- rbind(WB12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
WB12_TAMft <- ftable(WB12_TAMg2$player1, WB12_TAMg2$player2)
WB12_TAMft2 <- as.matrix(WB12_TAMft)
numRows <- nrow(WB12_TAMft2)
numCols <- ncol(WB12_TAMft2)
WB12_TAMft3 <- WB12_TAMft2[c(2:numRows) , c(2:numCols)]
WB12_TAMTable <- graph.adjacency(WB12_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(WB12_TAMTable, vertex.label = V(WB12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
WB12_TAM.clusterCoef <- transitivity(WB12_TAMTable, type="global") #cluster coefficient
WB12_TAM.degreeCent <- centralization.degree(WB12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_TAMftn <- as.network.matrix(WB12_TAMft)
WB12_TAM.netDensity <- network.density(WB12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_TAM.entropy <- entropy(WB12_TAMft) #entropy

WB12_TAM.netMx <- cbind(WB12_TAM.netMx, WB12_TAM.clusterCoef, WB12_TAM.degreeCent$centralization,
                        WB12_TAM.netDensity, WB12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************
#NA

round = 12
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
WB12_SDMg2 <- data.frame(WB12_SDM)
WB12_SDMg2 <- WB12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_SDMg2$player1
player2vector <- WB12_SDMg2$player2
WB12_SDMg3 <- WB12_SDMg2
WB12_SDMg3$p1inp2vec <- is.element(WB12_SDMg3$player1, player2vector)
WB12_SDMg3$p2inp1vec <- is.element(WB12_SDMg3$player2, player1vector)

addPlayer1 <- WB12_SDMg3[ which(WB12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_SDMg3[ which(WB12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_SDMg2 <- rbind(WB12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
WB12_SDMft <- ftable(WB12_SDMg2$player1, WB12_SDMg2$player2)
WB12_SDMft2 <- as.matrix(WB12_SDMft)
numRows <- nrow(WB12_SDMft2)
numCols <- ncol(WB12_SDMft2)
WB12_SDMft3 <- WB12_SDMft2[c(2:numRows) , c(2:numCols)]
WB12_SDMTable <- graph.adjacency(WB12_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(WB12_SDMTable, vertex.label = V(WB12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
WB12_SDM.clusterCoef <- transitivity(WB12_SDMTable, type="global") #cluster coefficient
WB12_SDM.degreeCent <- centralization.degree(WB12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_SDMftn <- as.network.matrix(WB12_SDMft)
WB12_SDM.netDensity <- network.density(WB12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_SDM.entropy <- entropy(WB12_SDMft) #entropy

WB12_SDM.netMx <- cbind(WB12_SDM.netMx, WB12_SDM.clusterCoef, WB12_SDM.degreeCent$centralization,
                        WB12_SDM.netDensity, WB12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "WB"
KIoutcome = "Turnover_DM"
WB12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
WB12_TDMg2 <- data.frame(WB12_TDM)
WB12_TDMg2 <- WB12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_TDMg2$player1
player2vector <- WB12_TDMg2$player2
WB12_TDMg3 <- WB12_TDMg2
WB12_TDMg3$p1inp2vec <- is.element(WB12_TDMg3$player1, player2vector)
WB12_TDMg3$p2inp1vec <- is.element(WB12_TDMg3$player2, player1vector)

addPlayer1 <- WB12_TDMg3[ which(WB12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_TDMg3[ which(WB12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_TDMg2 <- rbind(WB12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
WB12_TDMft <- ftable(WB12_TDMg2$player1, WB12_TDMg2$player2)
WB12_TDMft2 <- as.matrix(WB12_TDMft)
numRows <- nrow(WB12_TDMft2)
numCols <- ncol(WB12_TDMft2)
WB12_TDMft3 <- WB12_TDMft2[c(2:numRows) , c(2:numCols)]
WB12_TDMTable <- graph.adjacency(WB12_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(WB12_TDMTable, vertex.label = V(WB12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
WB12_TDM.clusterCoef <- transitivity(WB12_TDMTable, type="global") #cluster coefficient
WB12_TDM.degreeCent <- centralization.degree(WB12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_TDMftn <- as.network.matrix(WB12_TDMft)
WB12_TDM.netDensity <- network.density(WB12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_TDM.entropy <- entropy(WB12_TDMft) #entropy

WB12_TDM.netMx <- cbind(WB12_TDM.netMx, WB12_TDM.clusterCoef, WB12_TDM.degreeCent$centralization,
                        WB12_TDM.netDensity, WB12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "WB"
KIoutcome = "Stoppage_D"
WB12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
WB12_SDg2 <- data.frame(WB12_SD)
WB12_SDg2 <- WB12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_SDg2$player1
player2vector <- WB12_SDg2$player2
WB12_SDg3 <- WB12_SDg2
WB12_SDg3$p1inp2vec <- is.element(WB12_SDg3$player1, player2vector)
WB12_SDg3$p2inp1vec <- is.element(WB12_SDg3$player2, player1vector)

addPlayer1 <- WB12_SDg3[ which(WB12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_SDg3[ which(WB12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_SDg2 <- rbind(WB12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
WB12_SDft <- ftable(WB12_SDg2$player1, WB12_SDg2$player2)
WB12_SDft2 <- as.matrix(WB12_SDft)
numRows <- nrow(WB12_SDft2)
numCols <- ncol(WB12_SDft2)
WB12_SDft3 <- WB12_SDft2[c(2:numRows) , c(2:numCols)]
WB12_SDTable <- graph.adjacency(WB12_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(WB12_SDTable, vertex.label = V(WB12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
WB12_SD.clusterCoef <- transitivity(WB12_SDTable, type="global") #cluster coefficient
WB12_SD.degreeCent <- centralization.degree(WB12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_SDftn <- as.network.matrix(WB12_SDft)
WB12_SD.netDensity <- network.density(WB12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_SD.entropy <- entropy(WB12_SDft) #entropy

WB12_SD.netMx <- cbind(WB12_SD.netMx, WB12_SD.clusterCoef, WB12_SD.degreeCent$centralization,
                       WB12_SD.netDensity, WB12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "WB"
KIoutcome = "Turnover_D"
WB12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
WB12_TDg2 <- data.frame(WB12_TD)
WB12_TDg2 <- WB12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_TDg2$player1
player2vector <- WB12_TDg2$player2
WB12_TDg3 <- WB12_TDg2
WB12_TDg3$p1inp2vec <- is.element(WB12_TDg3$player1, player2vector)
WB12_TDg3$p2inp1vec <- is.element(WB12_TDg3$player2, player1vector)

addPlayer1 <- WB12_TDg3[ which(WB12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_TDg3[ which(WB12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_TDg2 <- rbind(WB12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
WB12_TDft <- ftable(WB12_TDg2$player1, WB12_TDg2$player2)
WB12_TDft2 <- as.matrix(WB12_TDft)
numRows <- nrow(WB12_TDft2)
numCols <- ncol(WB12_TDft2)
WB12_TDft3 <- WB12_TDft2[c(2:numRows) , c(2:numCols)]
WB12_TDTable <- graph.adjacency(WB12_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(WB12_TDTable, vertex.label = V(WB12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
WB12_TD.clusterCoef <- transitivity(WB12_TDTable, type="global") #cluster coefficient
WB12_TD.degreeCent <- centralization.degree(WB12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_TDftn <- as.network.matrix(WB12_TDft)
WB12_TD.netDensity <- network.density(WB12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_TD.entropy <- entropy(WB12_TDft) #entropy

WB12_TD.netMx <- cbind(WB12_TD.netMx, WB12_TD.clusterCoef, WB12_TD.degreeCent$centralization,
                       WB12_TD.netDensity, WB12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
WB12_QTg2 <- data.frame(WB12_QT)
WB12_QTg2 <- WB12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB12_QTg2$player1
player2vector <- WB12_QTg2$player2
WB12_QTg3 <- WB12_QTg2
WB12_QTg3$p1inp2vec <- is.element(WB12_QTg3$player1, player2vector)
WB12_QTg3$p2inp1vec <- is.element(WB12_QTg3$player2, player1vector)

addPlayer1 <- WB12_QTg3[ which(WB12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB12_QTg3[ which(WB12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB12_QTg2 <- rbind(WB12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
WB12_QTft <- ftable(WB12_QTg2$player1, WB12_QTg2$player2)
WB12_QTft2 <- as.matrix(WB12_QTft)
numRows <- nrow(WB12_QTft2)
numCols <- ncol(WB12_QTft2)
WB12_QTft3 <- WB12_QTft2[c(2:numRows) , c(2:numCols)]
WB12_QTTable <- graph.adjacency(WB12_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(WB12_QTTable, vertex.label = V(WB12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
WB12_QT.clusterCoef <- transitivity(WB12_QTTable, type="global") #cluster coefficient
WB12_QT.degreeCent <- centralization.degree(WB12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB12_QTftn <- as.network.matrix(WB12_QTft)
WB12_QT.netDensity <- network.density(WB12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB12_QT.entropy <- entropy(WB12_QTft) #entropy

WB12_QT.netMx <- cbind(WB12_QT.netMx, WB12_QT.clusterCoef, WB12_QT.degreeCent$centralization,
                       WB12_QT.netDensity, WB12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB12_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 12
##

#ROUND 12, Goal***************************************************************
#NA

round = 12
teamName = "WCE"
KIoutcome = "Goal_F"
WCE12_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Goal with weighted edges
WCE12_Gg2 <- data.frame(WCE12_G)
WCE12_Gg2 <- WCE12_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_Gg2$player1
player2vector <- WCE12_Gg2$player2
WCE12_Gg3 <- WCE12_Gg2
WCE12_Gg3$p1inp2vec <- is.element(WCE12_Gg3$player1, player2vector)
WCE12_Gg3$p2inp1vec <- is.element(WCE12_Gg3$player2, player1vector)

addPlayer1 <- WCE12_Gg3[ which(WCE12_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_Gg3[ which(WCE12_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_Gg2 <- rbind(WCE12_Gg2, addPlayers)

#ROUND 12, Goal graph using weighted edges
WCE12_Gft <- ftable(WCE12_Gg2$player1, WCE12_Gg2$player2)
WCE12_Gft2 <- as.matrix(WCE12_Gft)
numRows <- nrow(WCE12_Gft2)
numCols <- ncol(WCE12_Gft2)
WCE12_Gft3 <- WCE12_Gft2[c(2:numRows) , c(2:numCols)]
WCE12_GTable <- graph.adjacency(WCE12_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, Goal graph=weighted
plot.igraph(WCE12_GTable, vertex.label = V(WCE12_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Goal calulation of network metrics
#igraph
WCE12_G.clusterCoef <- transitivity(WCE12_GTable, type="global") #cluster coefficient
WCE12_G.degreeCent <- centralization.degree(WCE12_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_Gftn <- as.network.matrix(WCE12_Gft)
WCE12_G.netDensity <- network.density(WCE12_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_G.entropy <- entropy(WCE12_Gft) #entropy

WCE12_G.netMx <- cbind(WCE12_G.netMx, WCE12_G.clusterCoef, WCE12_G.degreeCent$centralization,
                       WCE12_G.netDensity, WCE12_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_G.netMx) <- varnames

#ROUND 12, Behind***************************************************************
#NA

round = 12
teamName = "WCE"
KIoutcome = "Behind_F"
WCE12_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, Behind with weighted edges
WCE12_Bg2 <- data.frame(WCE12_B)
WCE12_Bg2 <- WCE12_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_Bg2$player1
player2vector <- WCE12_Bg2$player2
WCE12_Bg3 <- WCE12_Bg2
WCE12_Bg3$p1inp2vec <- is.element(WCE12_Bg3$player1, player2vector)
WCE12_Bg3$p2inp1vec <- is.element(WCE12_Bg3$player2, player1vector)

addPlayer1 <- WCE12_Bg3[ which(WCE12_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_Bg3[ which(WCE12_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_Bg2 <- rbind(WCE12_Bg2, addPlayers)

#ROUND 12, Behind graph using weighted edges
WCE12_Bft <- ftable(WCE12_Bg2$player1, WCE12_Bg2$player2)
WCE12_Bft2 <- as.matrix(WCE12_Bft)
numRows <- nrow(WCE12_Bft2)
numCols <- ncol(WCE12_Bft2)
WCE12_Bft3 <- WCE12_Bft2[c(2:numRows) , c(2:numCols)]
WCE12_BTable <- graph.adjacency(WCE12_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 12, Behind graph=weighted
plot.igraph(WCE12_BTable, vertex.label = V(WCE12_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, Behind calulation of network metrics
#igraph
WCE12_B.clusterCoef <- transitivity(WCE12_BTable, type="global") #cluster coefficient
WCE12_B.degreeCent <- centralization.degree(WCE12_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_Bftn <- as.network.matrix(WCE12_Bft)
WCE12_B.netDensity <- network.density(WCE12_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_B.entropy <- entropy(WCE12_Bft) #entropy

WCE12_B.netMx <- cbind(WCE12_B.netMx, WCE12_B.clusterCoef, WCE12_B.degreeCent$centralization,
                       WCE12_B.netDensity, WCE12_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_B.netMx) <- varnames

#ROUND 12, FWD Stoppage**********************************************************

round = 12
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE12_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Stoppage with weighted edges
WCE12_SFg2 <- data.frame(WCE12_SF)
WCE12_SFg2 <- WCE12_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_SFg2$player1
player2vector <- WCE12_SFg2$player2
WCE12_SFg3 <- WCE12_SFg2
WCE12_SFg3$p1inp2vec <- is.element(WCE12_SFg3$player1, player2vector)
WCE12_SFg3$p2inp1vec <- is.element(WCE12_SFg3$player2, player1vector)

addPlayer1 <- WCE12_SFg3[ which(WCE12_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_SFg3[ which(WCE12_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_SFg2 <- rbind(WCE12_SFg2, addPlayers)


#ROUND 12, FWD Stoppage graph using weighted edges
WCE12_SFft <- ftable(WCE12_SFg2$player1, WCE12_SFg2$player2)
WCE12_SFft2 <- as.matrix(WCE12_SFft)
numRows <- nrow(WCE12_SFft2)
numCols <- ncol(WCE12_SFft2)
WCE12_SFft3 <- WCE12_SFft2[c(2:numRows) , c(2:numCols)]
WCE12_SFTable <- graph.adjacency(WCE12_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, FWD Stoppage graph=weighted
plot.igraph(WCE12_SFTable, vertex.label = V(WCE12_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Stoppage calulation of network metrics
#igraph
WCE12_SF.clusterCoef <- transitivity(WCE12_SFTable, type="global") #cluster coefficient
WCE12_SF.degreeCent <- centralization.degree(WCE12_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_SFftn <- as.network.matrix(WCE12_SFft)
WCE12_SF.netDensity <- network.density(WCE12_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_SF.entropy <- entropy(WCE12_SFft) #entropy

WCE12_SF.netMx <- cbind(WCE12_SF.netMx, WCE12_SF.clusterCoef, WCE12_SF.degreeCent$centralization,
                        WCE12_SF.netDensity, WCE12_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_SF.netMx) <- varnames

#ROUND 12, FWD Turnover**********************************************************

round = 12
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE12_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, FWD Turnover with weighted edges
WCE12_TFg2 <- data.frame(WCE12_TF)
WCE12_TFg2 <- WCE12_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_TFg2$player1
player2vector <- WCE12_TFg2$player2
WCE12_TFg3 <- WCE12_TFg2
WCE12_TFg3$p1inp2vec <- is.element(WCE12_TFg3$player1, player2vector)
WCE12_TFg3$p2inp1vec <- is.element(WCE12_TFg3$player2, player1vector)

addPlayer1 <- WCE12_TFg3[ which(WCE12_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_TFg3[ which(WCE12_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_TFg2 <- rbind(WCE12_TFg2, addPlayers)

#ROUND 12, FWD Turnover graph using weighted edges
WCE12_TFft <- ftable(WCE12_TFg2$player1, WCE12_TFg2$player2)
WCE12_TFft2 <- as.matrix(WCE12_TFft)
numRows <- nrow(WCE12_TFft2)
numCols <- ncol(WCE12_TFft2)
WCE12_TFft3 <- WCE12_TFft2[c(2:numRows) , c(2:numCols)]
WCE12_TFTable <- graph.adjacency(WCE12_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, FWD Turnover graph=weighted
plot.igraph(WCE12_TFTable, vertex.label = V(WCE12_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, FWD Turnover calulation of network metrics
#igraph
WCE12_TF.clusterCoef <- transitivity(WCE12_TFTable, type="global") #cluster coefficient
WCE12_TF.degreeCent <- centralization.degree(WCE12_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_TFftn <- as.network.matrix(WCE12_TFft)
WCE12_TF.netDensity <- network.density(WCE12_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_TF.entropy <- entropy(WCE12_TFft) #entropy

WCE12_TF.netMx <- cbind(WCE12_TF.netMx, WCE12_TF.clusterCoef, WCE12_TF.degreeCent$centralization,
                        WCE12_TF.netDensity, WCE12_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_TF.netMx) <- varnames

#ROUND 12, AM Stoppage**********************************************************
#NA

round = 12
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE12_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Stoppage with weighted edges
WCE12_SAMg2 <- data.frame(WCE12_SAM)
WCE12_SAMg2 <- WCE12_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_SAMg2$player1
player2vector <- WCE12_SAMg2$player2
WCE12_SAMg3 <- WCE12_SAMg2
WCE12_SAMg3$p1inp2vec <- is.element(WCE12_SAMg3$player1, player2vector)
WCE12_SAMg3$p2inp1vec <- is.element(WCE12_SAMg3$player2, player1vector)

addPlayer1 <- WCE12_SAMg3[ which(WCE12_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_SAMg3[ which(WCE12_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_SAMg2 <- rbind(WCE12_SAMg2, addPlayers)

#ROUND 12, AM Stoppage graph using weighted edges
WCE12_SAMft <- ftable(WCE12_SAMg2$player1, WCE12_SAMg2$player2)
WCE12_SAMft2 <- as.matrix(WCE12_SAMft)
numRows <- nrow(WCE12_SAMft2)
numCols <- ncol(WCE12_SAMft2)
WCE12_SAMft3 <- WCE12_SAMft2[c(2:numRows) , c(2:numCols)]
WCE12_SAMTable <- graph.adjacency(WCE12_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, AM Stoppage graph=weighted
plot.igraph(WCE12_SAMTable, vertex.label = V(WCE12_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Stoppage calulation of network metrics
#igraph
WCE12_SAM.clusterCoef <- transitivity(WCE12_SAMTable, type="global") #cluster coefficient
WCE12_SAM.degreeCent <- centralization.degree(WCE12_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_SAMftn <- as.network.matrix(WCE12_SAMft)
WCE12_SAM.netDensity <- network.density(WCE12_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_SAM.entropy <- entropy(WCE12_SAMft) #entropy

WCE12_SAM.netMx <- cbind(WCE12_SAM.netMx, WCE12_SAM.clusterCoef, WCE12_SAM.degreeCent$centralization,
                         WCE12_SAM.netDensity, WCE12_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_SAM.netMx) <- varnames

#ROUND 12, AM Turnover**********************************************************
#NA

round = 12
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE12_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, AM Turnover with weighted edges
WCE12_TAMg2 <- data.frame(WCE12_TAM)
WCE12_TAMg2 <- WCE12_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_TAMg2$player1
player2vector <- WCE12_TAMg2$player2
WCE12_TAMg3 <- WCE12_TAMg2
WCE12_TAMg3$p1inp2vec <- is.element(WCE12_TAMg3$player1, player2vector)
WCE12_TAMg3$p2inp1vec <- is.element(WCE12_TAMg3$player2, player1vector)

addPlayer1 <- WCE12_TAMg3[ which(WCE12_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_TAMg3[ which(WCE12_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_TAMg2 <- rbind(WCE12_TAMg2, addPlayers)

#ROUND 12, AM Turnover graph using weighted edges
WCE12_TAMft <- ftable(WCE12_TAMg2$player1, WCE12_TAMg2$player2)
WCE12_TAMft2 <- as.matrix(WCE12_TAMft)
numRows <- nrow(WCE12_TAMft2)
numCols <- ncol(WCE12_TAMft2)
WCE12_TAMft3 <- WCE12_TAMft2[c(2:numRows) , c(2:numCols)]
WCE12_TAMTable <- graph.adjacency(WCE12_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, AM Turnover graph=weighted
plot.igraph(WCE12_TAMTable, vertex.label = V(WCE12_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, AM Turnover calulation of network metrics
#igraph
WCE12_TAM.clusterCoef <- transitivity(WCE12_TAMTable, type="global") #cluster coefficient
WCE12_TAM.degreeCent <- centralization.degree(WCE12_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_TAMftn <- as.network.matrix(WCE12_TAMft)
WCE12_TAM.netDensity <- network.density(WCE12_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_TAM.entropy <- entropy(WCE12_TAMft) #entropy

WCE12_TAM.netMx <- cbind(WCE12_TAM.netMx, WCE12_TAM.clusterCoef, WCE12_TAM.degreeCent$centralization,
                         WCE12_TAM.netDensity, WCE12_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_TAM.netMx) <- varnames

#ROUND 12, DM Stoppage**********************************************************

round = 12
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE12_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Stoppage with weighted edges
WCE12_SDMg2 <- data.frame(WCE12_SDM)
WCE12_SDMg2 <- WCE12_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_SDMg2$player1
player2vector <- WCE12_SDMg2$player2
WCE12_SDMg3 <- WCE12_SDMg2
WCE12_SDMg3$p1inp2vec <- is.element(WCE12_SDMg3$player1, player2vector)
WCE12_SDMg3$p2inp1vec <- is.element(WCE12_SDMg3$player2, player1vector)

addPlayer1 <- WCE12_SDMg3[ which(WCE12_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_SDMg3[ which(WCE12_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_SDMg2 <- rbind(WCE12_SDMg2, addPlayers)

#ROUND 12, DM Stoppage graph using weighted edges
WCE12_SDMft <- ftable(WCE12_SDMg2$player1, WCE12_SDMg2$player2)
WCE12_SDMft2 <- as.matrix(WCE12_SDMft)
numRows <- nrow(WCE12_SDMft2)
numCols <- ncol(WCE12_SDMft2)
WCE12_SDMft3 <- WCE12_SDMft2[c(2:numRows) , c(2:numCols)]
WCE12_SDMTable <- graph.adjacency(WCE12_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, DM Stoppage graph=weighted
plot.igraph(WCE12_SDMTable, vertex.label = V(WCE12_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Stoppage calulation of network metrics
#igraph
WCE12_SDM.clusterCoef <- transitivity(WCE12_SDMTable, type="global") #cluster coefficient
WCE12_SDM.degreeCent <- centralization.degree(WCE12_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_SDMftn <- as.network.matrix(WCE12_SDMft)
WCE12_SDM.netDensity <- network.density(WCE12_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_SDM.entropy <- entropy(WCE12_SDMft) #entropy

WCE12_SDM.netMx <- cbind(WCE12_SDM.netMx, WCE12_SDM.clusterCoef, WCE12_SDM.degreeCent$centralization,
                         WCE12_SDM.netDensity, WCE12_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_SDM.netMx) <- varnames

#ROUND 12, DM Turnover**********************************************************

round = 12
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE12_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, DM Turnover with weighted edges
WCE12_TDMg2 <- data.frame(WCE12_TDM)
WCE12_TDMg2 <- WCE12_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_TDMg2$player1
player2vector <- WCE12_TDMg2$player2
WCE12_TDMg3 <- WCE12_TDMg2
WCE12_TDMg3$p1inp2vec <- is.element(WCE12_TDMg3$player1, player2vector)
WCE12_TDMg3$p2inp1vec <- is.element(WCE12_TDMg3$player2, player1vector)

addPlayer1 <- WCE12_TDMg3[ which(WCE12_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_TDMg3[ which(WCE12_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_TDMg2 <- rbind(WCE12_TDMg2, addPlayers)

#ROUND 12, DM Turnover graph using weighted edges
WCE12_TDMft <- ftable(WCE12_TDMg2$player1, WCE12_TDMg2$player2)
WCE12_TDMft2 <- as.matrix(WCE12_TDMft)
numRows <- nrow(WCE12_TDMft2)
numCols <- ncol(WCE12_TDMft2)
WCE12_TDMft3 <- WCE12_TDMft2[c(2:numRows) , c(2:numCols)]
WCE12_TDMTable <- graph.adjacency(WCE12_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 12, DM Turnover graph=weighted
plot.igraph(WCE12_TDMTable, vertex.label = V(WCE12_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, DM Turnover calulation of network metrics
#igraph
WCE12_TDM.clusterCoef <- transitivity(WCE12_TDMTable, type="global") #cluster coefficient
WCE12_TDM.degreeCent <- centralization.degree(WCE12_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_TDMftn <- as.network.matrix(WCE12_TDMft)
WCE12_TDM.netDensity <- network.density(WCE12_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_TDM.entropy <- entropy(WCE12_TDMft) #entropy

WCE12_TDM.netMx <- cbind(WCE12_TDM.netMx, WCE12_TDM.clusterCoef, WCE12_TDM.degreeCent$centralization,
                         WCE12_TDM.netDensity, WCE12_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_TDM.netMx) <- varnames

#ROUND 12, D Stoppage**********************************************************
#NA

round = 12
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE12_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Stoppage with weighted edges
WCE12_SDg2 <- data.frame(WCE12_SD)
WCE12_SDg2 <- WCE12_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_SDg2$player1
player2vector <- WCE12_SDg2$player2
WCE12_SDg3 <- WCE12_SDg2
WCE12_SDg3$p1inp2vec <- is.element(WCE12_SDg3$player1, player2vector)
WCE12_SDg3$p2inp1vec <- is.element(WCE12_SDg3$player2, player1vector)

addPlayer1 <- WCE12_SDg3[ which(WCE12_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_SDg3[ which(WCE12_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_SDg2 <- rbind(WCE12_SDg2, addPlayers)

#ROUND 12, D Stoppage graph using weighted edges
WCE12_SDft <- ftable(WCE12_SDg2$player1, WCE12_SDg2$player2)
WCE12_SDft2 <- as.matrix(WCE12_SDft)
numRows <- nrow(WCE12_SDft2)
numCols <- ncol(WCE12_SDft2)
WCE12_SDft3 <- WCE12_SDft2[c(2:numRows) , c(2:numCols)]
WCE12_SDTable <- graph.adjacency(WCE12_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, D Stoppage graph=weighted
plot.igraph(WCE12_SDTable, vertex.label = V(WCE12_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Stoppage calulation of network metrics
#igraph
WCE12_SD.clusterCoef <- transitivity(WCE12_SDTable, type="global") #cluster coefficient
WCE12_SD.degreeCent <- centralization.degree(WCE12_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_SDftn <- as.network.matrix(WCE12_SDft)
WCE12_SD.netDensity <- network.density(WCE12_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_SD.entropy <- entropy(WCE12_SDft) #entropy

WCE12_SD.netMx <- cbind(WCE12_SD.netMx, WCE12_SD.clusterCoef, WCE12_SD.degreeCent$centralization,
                        WCE12_SD.netDensity, WCE12_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_SD.netMx) <- varnames

#ROUND 12, D Turnover**********************************************************
#NA

round = 12
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE12_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, D Turnover with weighted edges
WCE12_TDg2 <- data.frame(WCE12_TD)
WCE12_TDg2 <- WCE12_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_TDg2$player1
player2vector <- WCE12_TDg2$player2
WCE12_TDg3 <- WCE12_TDg2
WCE12_TDg3$p1inp2vec <- is.element(WCE12_TDg3$player1, player2vector)
WCE12_TDg3$p2inp1vec <- is.element(WCE12_TDg3$player2, player1vector)

addPlayer1 <- WCE12_TDg3[ which(WCE12_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_TDg3[ which(WCE12_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_TDg2 <- rbind(WCE12_TDg2, addPlayers)

#ROUND 12, D Turnover graph using weighted edges
WCE12_TDft <- ftable(WCE12_TDg2$player1, WCE12_TDg2$player2)
WCE12_TDft2 <- as.matrix(WCE12_TDft)
numRows <- nrow(WCE12_TDft2)
numCols <- ncol(WCE12_TDft2)
WCE12_TDft3 <- WCE12_TDft2[c(2:numRows) , c(2:numCols)]
WCE12_TDTable <- graph.adjacency(WCE12_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, D Turnover graph=weighted
plot.igraph(WCE12_TDTable, vertex.label = V(WCE12_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, D Turnover calulation of network metrics
#igraph
WCE12_TD.clusterCoef <- transitivity(WCE12_TDTable, type="global") #cluster coefficient
WCE12_TD.degreeCent <- centralization.degree(WCE12_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_TDftn <- as.network.matrix(WCE12_TDft)
WCE12_TD.netDensity <- network.density(WCE12_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_TD.entropy <- entropy(WCE12_TDft) #entropy

WCE12_TD.netMx <- cbind(WCE12_TD.netMx, WCE12_TD.clusterCoef, WCE12_TD.degreeCent$centralization,
                        WCE12_TD.netDensity, WCE12_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_TD.netMx) <- varnames

#ROUND 12, End of Qtr**********************************************************
#NA

round = 12
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE12_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 12, End of Qtr with weighted edges
WCE12_QTg2 <- data.frame(WCE12_QT)
WCE12_QTg2 <- WCE12_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE12_QTg2$player1
player2vector <- WCE12_QTg2$player2
WCE12_QTg3 <- WCE12_QTg2
WCE12_QTg3$p1inp2vec <- is.element(WCE12_QTg3$player1, player2vector)
WCE12_QTg3$p2inp1vec <- is.element(WCE12_QTg3$player2, player1vector)

addPlayer1 <- WCE12_QTg3[ which(WCE12_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE12_QTg3[ which(WCE12_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE12_QTg2 <- rbind(WCE12_QTg2, addPlayers)

#ROUND 12, End of Qtr graph using weighted edges
WCE12_QTft <- ftable(WCE12_QTg2$player1, WCE12_QTg2$player2)
WCE12_QTft2 <- as.matrix(WCE12_QTft)
numRows <- nrow(WCE12_QTft2)
numCols <- ncol(WCE12_QTft2)
WCE12_QTft3 <- WCE12_QTft2[c(2:numRows) , c(2:numCols)]
WCE12_QTTable <- graph.adjacency(WCE12_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 12, End of Qtr graph=weighted
plot.igraph(WCE12_QTTable, vertex.label = V(WCE12_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE12_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 12, End of Qtr calulation of network metrics
#igraph
WCE12_QT.clusterCoef <- transitivity(WCE12_QTTable, type="global") #cluster coefficient
WCE12_QT.degreeCent <- centralization.degree(WCE12_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE12_QTftn <- as.network.matrix(WCE12_QTft)
WCE12_QT.netDensity <- network.density(WCE12_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE12_QT.entropy <- entropy(WCE12_QTft) #entropy

WCE12_QT.netMx <- cbind(WCE12_QT.netMx, WCE12_QT.clusterCoef, WCE12_QT.degreeCent$centralization,
                        WCE12_QT.netDensity, WCE12_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE12_QT.netMx) <- varnames
