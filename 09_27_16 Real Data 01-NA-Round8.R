#####
#09-27-16- Real data 08
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
ADEL08_Gg2 <- data.frame(ADEL08_G)
ADEL08_Gg2 <- ADEL08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_Gg2$player1
player2vector <- ADEL08_Gg2$player2
ADEL08_Gg3 <- ADEL08_Gg2
ADEL08_Gg3$p1inp2vec <- is.element(ADEL08_Gg3$player1, player2vector)
ADEL08_Gg3$p2inp1vec <- is.element(ADEL08_Gg3$player2, player1vector)

addPlayer1 <- ADEL08_Gg3[ which(ADEL08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_Gg3[ which(ADEL08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_Gg2 <- rbind(ADEL08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
ADEL08_Gft <- ftable(ADEL08_Gg2$player1, ADEL08_Gg2$player2)
ADEL08_Gft2 <- as.matrix(ADEL08_Gft)
numRows <- nrow(ADEL08_Gft2)
numCols <- ncol(ADEL08_Gft2)
ADEL08_Gft3 <- ADEL08_Gft2[c(2:numRows) , c(2:numCols)]
ADEL08_GTable <- graph.adjacency(ADEL08_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(ADEL08_GTable, vertex.label = V(ADEL08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
ADEL08_G.clusterCoef <- transitivity(ADEL08_GTable, type="global") #cluster coefficient
ADEL08_G.degreeCent <- centralization.degree(ADEL08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_Gftn <- as.network.matrix(ADEL08_Gft)
ADEL08_G.netDensity <- network.density(ADEL08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_G.entropy <- entropy(ADEL08_Gft) #entropy

ADEL08_G.netMx <- cbind(ADEL08_G.netMx, ADEL08_G.clusterCoef, ADEL08_G.degreeCent$centralization,
                        ADEL08_G.netDensity, ADEL08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
ADEL08_Bg2 <- data.frame(ADEL08_B)
ADEL08_Bg2 <- ADEL08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_Bg2$player1
player2vector <- ADEL08_Bg2$player2
ADEL08_Bg3 <- ADEL08_Bg2
ADEL08_Bg3$p1inp2vec <- is.element(ADEL08_Bg3$player1, player2vector)
ADEL08_Bg3$p2inp1vec <- is.element(ADEL08_Bg3$player2, player1vector)

addPlayer1 <- ADEL08_Bg3[ which(ADEL08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_Bg3[ which(ADEL08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_Bg2 <- rbind(ADEL08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
ADEL08_Bft <- ftable(ADEL08_Bg2$player1, ADEL08_Bg2$player2)
ADEL08_Bft2 <- as.matrix(ADEL08_Bft)
numRows <- nrow(ADEL08_Bft2)
numCols <- ncol(ADEL08_Bft2)
ADEL08_Bft3 <- ADEL08_Bft2[c(2:numRows) , c(2:numCols)]
ADEL08_BTable <- graph.adjacency(ADEL08_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(ADEL08_BTable, vertex.label = V(ADEL08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
ADEL08_B.clusterCoef <- transitivity(ADEL08_BTable, type="global") #cluster coefficient
ADEL08_B.degreeCent <- centralization.degree(ADEL08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_Bftn <- as.network.matrix(ADEL08_Bft)
ADEL08_B.netDensity <- network.density(ADEL08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_B.entropy <- entropy(ADEL08_Bft) #entropy

ADEL08_B.netMx <- cbind(ADEL08_B.netMx, ADEL08_B.clusterCoef, ADEL08_B.degreeCent$centralization,
                        ADEL08_B.netDensity, ADEL08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
ADEL08_SFg2 <- data.frame(ADEL08_SF)
ADEL08_SFg2 <- ADEL08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_SFg2$player1
player2vector <- ADEL08_SFg2$player2
ADEL08_SFg3 <- ADEL08_SFg2
ADEL08_SFg3$p1inp2vec <- is.element(ADEL08_SFg3$player1, player2vector)
ADEL08_SFg3$p2inp1vec <- is.element(ADEL08_SFg3$player2, player1vector)

addPlayer1 <- ADEL08_SFg3[ which(ADEL08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_SFg3[ which(ADEL08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_SFg2 <- rbind(ADEL08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
ADEL08_SFft <- ftable(ADEL08_SFg2$player1, ADEL08_SFg2$player2)
ADEL08_SFft2 <- as.matrix(ADEL08_SFft)
numRows <- nrow(ADEL08_SFft2)
numCols <- ncol(ADEL08_SFft2)
ADEL08_SFft3 <- ADEL08_SFft2[c(2:numRows) , c(2:numCols)]
ADEL08_SFTable <- graph.adjacency(ADEL08_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(ADEL08_SFTable, vertex.label = V(ADEL08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
ADEL08_SF.clusterCoef <- transitivity(ADEL08_SFTable, type="global") #cluster coefficient
ADEL08_SF.degreeCent <- centralization.degree(ADEL08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_SFftn <- as.network.matrix(ADEL08_SFft)
ADEL08_SF.netDensity <- network.density(ADEL08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_SF.entropy <- entropy(ADEL08_SFft) #entropy

ADEL08_SF.netMx <- cbind(ADEL08_SF.netMx, ADEL08_SF.clusterCoef, ADEL08_SF.degreeCent$centralization,
                         ADEL08_SF.netDensity, ADEL08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************
#NA

round = 8
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
ADEL08_TFg2 <- data.frame(ADEL08_TF)
ADEL08_TFg2 <- ADEL08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_TFg2$player1
player2vector <- ADEL08_TFg2$player2
ADEL08_TFg3 <- ADEL08_TFg2
ADEL08_TFg3$p1inp2vec <- is.element(ADEL08_TFg3$player1, player2vector)
ADEL08_TFg3$p2inp1vec <- is.element(ADEL08_TFg3$player2, player1vector)

addPlayer1 <- ADEL08_TFg3[ which(ADEL08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_TFg3[ which(ADEL08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_TFg2 <- rbind(ADEL08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
ADEL08_TFft <- ftable(ADEL08_TFg2$player1, ADEL08_TFg2$player2)
ADEL08_TFft2 <- as.matrix(ADEL08_TFft)
numRows <- nrow(ADEL08_TFft2)
numCols <- ncol(ADEL08_TFft2)
ADEL08_TFft3 <- ADEL08_TFft2[c(2:numRows) , c(2:numCols)]
ADEL08_TFTable <- graph.adjacency(ADEL08_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(ADEL08_TFTable, vertex.label = V(ADEL08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
ADEL08_TF.clusterCoef <- transitivity(ADEL08_TFTable, type="global") #cluster coefficient
ADEL08_TF.degreeCent <- centralization.degree(ADEL08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_TFftn <- as.network.matrix(ADEL08_TFft)
ADEL08_TF.netDensity <- network.density(ADEL08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_TF.entropy <- entropy(ADEL08_TFft) #entropy

ADEL08_TF.netMx <- cbind(ADEL08_TF.netMx, ADEL08_TF.clusterCoef, ADEL08_TF.degreeCent$centralization,
                         ADEL08_TF.netDensity, ADEL08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************

round = 8
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
ADEL08_SAMg2 <- data.frame(ADEL08_SAM)
ADEL08_SAMg2 <- ADEL08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_SAMg2$player1
player2vector <- ADEL08_SAMg2$player2
ADEL08_SAMg3 <- ADEL08_SAMg2
ADEL08_SAMg3$p1inp2vec <- is.element(ADEL08_SAMg3$player1, player2vector)
ADEL08_SAMg3$p2inp1vec <- is.element(ADEL08_SAMg3$player2, player1vector)

addPlayer1 <- ADEL08_SAMg3[ which(ADEL08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_SAMg3[ which(ADEL08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_SAMg2 <- rbind(ADEL08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
ADEL08_SAMft <- ftable(ADEL08_SAMg2$player1, ADEL08_SAMg2$player2)
ADEL08_SAMft2 <- as.matrix(ADEL08_SAMft)
numRows <- nrow(ADEL08_SAMft2)
numCols <- ncol(ADEL08_SAMft2)
ADEL08_SAMft3 <- ADEL08_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL08_SAMTable <- graph.adjacency(ADEL08_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(ADEL08_SAMTable, vertex.label = V(ADEL08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
ADEL08_SAM.clusterCoef <- transitivity(ADEL08_SAMTable, type="global") #cluster coefficient
ADEL08_SAM.degreeCent <- centralization.degree(ADEL08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_SAMftn <- as.network.matrix(ADEL08_SAMft)
ADEL08_SAM.netDensity <- network.density(ADEL08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_SAM.entropy <- entropy(ADEL08_SAMft) #entropy

ADEL08_SAM.netMx <- cbind(ADEL08_SAM.netMx, ADEL08_SAM.clusterCoef, ADEL08_SAM.degreeCent$centralization,
                          ADEL08_SAM.netDensity, ADEL08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
ADEL08_TAMg2 <- data.frame(ADEL08_TAM)
ADEL08_TAMg2 <- ADEL08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_TAMg2$player1
player2vector <- ADEL08_TAMg2$player2
ADEL08_TAMg3 <- ADEL08_TAMg2
ADEL08_TAMg3$p1inp2vec <- is.element(ADEL08_TAMg3$player1, player2vector)
ADEL08_TAMg3$p2inp1vec <- is.element(ADEL08_TAMg3$player2, player1vector)

addPlayer1 <- ADEL08_TAMg3[ which(ADEL08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_TAMg3[ which(ADEL08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_TAMg2 <- rbind(ADEL08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
ADEL08_TAMft <- ftable(ADEL08_TAMg2$player1, ADEL08_TAMg2$player2)
ADEL08_TAMft2 <- as.matrix(ADEL08_TAMft)
numRows <- nrow(ADEL08_TAMft2)
numCols <- ncol(ADEL08_TAMft2)
ADEL08_TAMft3 <- ADEL08_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL08_TAMTable <- graph.adjacency(ADEL08_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(ADEL08_TAMTable, vertex.label = V(ADEL08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
ADEL08_TAM.clusterCoef <- transitivity(ADEL08_TAMTable, type="global") #cluster coefficient
ADEL08_TAM.degreeCent <- centralization.degree(ADEL08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_TAMftn <- as.network.matrix(ADEL08_TAMft)
ADEL08_TAM.netDensity <- network.density(ADEL08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_TAM.entropy <- entropy(ADEL08_TAMft) #entropy

ADEL08_TAM.netMx <- cbind(ADEL08_TAM.netMx, ADEL08_TAM.clusterCoef, ADEL08_TAM.degreeCent$centralization,
                          ADEL08_TAM.netDensity, ADEL08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
ADEL08_SDMg2 <- data.frame(ADEL08_SDM)
ADEL08_SDMg2 <- ADEL08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_SDMg2$player1
player2vector <- ADEL08_SDMg2$player2
ADEL08_SDMg3 <- ADEL08_SDMg2
ADEL08_SDMg3$p1inp2vec <- is.element(ADEL08_SDMg3$player1, player2vector)
ADEL08_SDMg3$p2inp1vec <- is.element(ADEL08_SDMg3$player2, player1vector)

addPlayer1 <- ADEL08_SDMg3[ which(ADEL08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_SDMg3[ which(ADEL08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_SDMg2 <- rbind(ADEL08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
ADEL08_SDMft <- ftable(ADEL08_SDMg2$player1, ADEL08_SDMg2$player2)
ADEL08_SDMft2 <- as.matrix(ADEL08_SDMft)
numRows <- nrow(ADEL08_SDMft2)
numCols <- ncol(ADEL08_SDMft2)
ADEL08_SDMft3 <- ADEL08_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL08_SDMTable <- graph.adjacency(ADEL08_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(ADEL08_SDMTable, vertex.label = V(ADEL08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
ADEL08_SDM.clusterCoef <- transitivity(ADEL08_SDMTable, type="global") #cluster coefficient
ADEL08_SDM.degreeCent <- centralization.degree(ADEL08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_SDMftn <- as.network.matrix(ADEL08_SDMft)
ADEL08_SDM.netDensity <- network.density(ADEL08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_SDM.entropy <- entropy(ADEL08_SDMft) #entropy

ADEL08_SDM.netMx <- cbind(ADEL08_SDM.netMx, ADEL08_SDM.clusterCoef, ADEL08_SDM.degreeCent$centralization,
                          ADEL08_SDM.netDensity, ADEL08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
ADEL08_TDMg2 <- data.frame(ADEL08_TDM)
ADEL08_TDMg2 <- ADEL08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_TDMg2$player1
player2vector <- ADEL08_TDMg2$player2
ADEL08_TDMg3 <- ADEL08_TDMg2
ADEL08_TDMg3$p1inp2vec <- is.element(ADEL08_TDMg3$player1, player2vector)
ADEL08_TDMg3$p2inp1vec <- is.element(ADEL08_TDMg3$player2, player1vector)

addPlayer1 <- ADEL08_TDMg3[ which(ADEL08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_TDMg3[ which(ADEL08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_TDMg2 <- rbind(ADEL08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
ADEL08_TDMft <- ftable(ADEL08_TDMg2$player1, ADEL08_TDMg2$player2)
ADEL08_TDMft2 <- as.matrix(ADEL08_TDMft)
numRows <- nrow(ADEL08_TDMft2)
numCols <- ncol(ADEL08_TDMft2)
ADEL08_TDMft3 <- ADEL08_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL08_TDMTable <- graph.adjacency(ADEL08_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(ADEL08_TDMTable, vertex.label = V(ADEL08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
ADEL08_TDM.clusterCoef <- transitivity(ADEL08_TDMTable, type="global") #cluster coefficient
ADEL08_TDM.degreeCent <- centralization.degree(ADEL08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_TDMftn <- as.network.matrix(ADEL08_TDMft)
ADEL08_TDM.netDensity <- network.density(ADEL08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_TDM.entropy <- entropy(ADEL08_TDMft) #entropy

ADEL08_TDM.netMx <- cbind(ADEL08_TDM.netMx, ADEL08_TDM.clusterCoef, ADEL08_TDM.degreeCent$centralization,
                          ADEL08_TDM.netDensity, ADEL08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
ADEL08_SDg2 <- data.frame(ADEL08_SD)
ADEL08_SDg2 <- ADEL08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_SDg2$player1
player2vector <- ADEL08_SDg2$player2
ADEL08_SDg3 <- ADEL08_SDg2
ADEL08_SDg3$p1inp2vec <- is.element(ADEL08_SDg3$player1, player2vector)
ADEL08_SDg3$p2inp1vec <- is.element(ADEL08_SDg3$player2, player1vector)

addPlayer1 <- ADEL08_SDg3[ which(ADEL08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_SDg3[ which(ADEL08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_SDg2 <- rbind(ADEL08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
ADEL08_SDft <- ftable(ADEL08_SDg2$player1, ADEL08_SDg2$player2)
ADEL08_SDft2 <- as.matrix(ADEL08_SDft)
numRows <- nrow(ADEL08_SDft2)
numCols <- ncol(ADEL08_SDft2)
ADEL08_SDft3 <- ADEL08_SDft2[c(2:numRows) , c(2:numCols)]
ADEL08_SDTable <- graph.adjacency(ADEL08_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(ADEL08_SDTable, vertex.label = V(ADEL08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
ADEL08_SD.clusterCoef <- transitivity(ADEL08_SDTable, type="global") #cluster coefficient
ADEL08_SD.degreeCent <- centralization.degree(ADEL08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_SDftn <- as.network.matrix(ADEL08_SDft)
ADEL08_SD.netDensity <- network.density(ADEL08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_SD.entropy <- entropy(ADEL08_SDft) #entropy

ADEL08_SD.netMx <- cbind(ADEL08_SD.netMx, ADEL08_SD.clusterCoef, ADEL08_SD.degreeCent$centralization,
                         ADEL08_SD.netDensity, ADEL08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************

round = 8
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
ADEL08_TDg2 <- data.frame(ADEL08_TD)
ADEL08_TDg2 <- ADEL08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_TDg2$player1
player2vector <- ADEL08_TDg2$player2
ADEL08_TDg3 <- ADEL08_TDg2
ADEL08_TDg3$p1inp2vec <- is.element(ADEL08_TDg3$player1, player2vector)
ADEL08_TDg3$p2inp1vec <- is.element(ADEL08_TDg3$player2, player1vector)

addPlayer1 <- ADEL08_TDg3[ which(ADEL08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_TDg3[ which(ADEL08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_TDg2 <- rbind(ADEL08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
ADEL08_TDft <- ftable(ADEL08_TDg2$player1, ADEL08_TDg2$player2)
ADEL08_TDft2 <- as.matrix(ADEL08_TDft)
numRows <- nrow(ADEL08_TDft2)
numCols <- ncol(ADEL08_TDft2)
ADEL08_TDft3 <- ADEL08_TDft2[c(2:numRows) , c(2:numCols)]
ADEL08_TDTable <- graph.adjacency(ADEL08_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(ADEL08_TDTable, vertex.label = V(ADEL08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
ADEL08_TD.clusterCoef <- transitivity(ADEL08_TDTable, type="global") #cluster coefficient
ADEL08_TD.degreeCent <- centralization.degree(ADEL08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_TDftn <- as.network.matrix(ADEL08_TDft)
ADEL08_TD.netDensity <- network.density(ADEL08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_TD.entropy <- entropy(ADEL08_TDft) #entropy

ADEL08_TD.netMx <- cbind(ADEL08_TD.netMx, ADEL08_TD.clusterCoef, ADEL08_TD.degreeCent$centralization,
                         ADEL08_TD.netDensity, ADEL08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
ADEL08_QTg2 <- data.frame(ADEL08_QT)
ADEL08_QTg2 <- ADEL08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL08_QTg2$player1
player2vector <- ADEL08_QTg2$player2
ADEL08_QTg3 <- ADEL08_QTg2
ADEL08_QTg3$p1inp2vec <- is.element(ADEL08_QTg3$player1, player2vector)
ADEL08_QTg3$p2inp1vec <- is.element(ADEL08_QTg3$player2, player1vector)

addPlayer1 <- ADEL08_QTg3[ which(ADEL08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL08_QTg3[ which(ADEL08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL08_QTg2 <- rbind(ADEL08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
ADEL08_QTft <- ftable(ADEL08_QTg2$player1, ADEL08_QTg2$player2)
ADEL08_QTft2 <- as.matrix(ADEL08_QTft)
numRows <- nrow(ADEL08_QTft2)
numCols <- ncol(ADEL08_QTft2)
ADEL08_QTft3 <- ADEL08_QTft2[c(2:numRows) , c(2:numCols)]
ADEL08_QTTable <- graph.adjacency(ADEL08_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(ADEL08_QTTable, vertex.label = V(ADEL08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
ADEL08_QT.clusterCoef <- transitivity(ADEL08_QTTable, type="global") #cluster coefficient
ADEL08_QT.degreeCent <- centralization.degree(ADEL08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL08_QTftn <- as.network.matrix(ADEL08_QTft)
ADEL08_QT.netDensity <- network.density(ADEL08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL08_QT.entropy <- entropy(ADEL08_QTft) #entropy

ADEL08_QT.netMx <- cbind(ADEL08_QT.netMx, ADEL08_QT.clusterCoef, ADEL08_QT.degreeCent$centralization,
                         ADEL08_QT.netDensity, ADEL08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL08_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "BL"
KIoutcome = "Goal_F"
BL08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
BL08_Gg2 <- data.frame(BL08_G)
BL08_Gg2 <- BL08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_Gg2$player1
player2vector <- BL08_Gg2$player2
BL08_Gg3 <- BL08_Gg2
BL08_Gg3$p1inp2vec <- is.element(BL08_Gg3$player1, player2vector)
BL08_Gg3$p2inp1vec <- is.element(BL08_Gg3$player2, player1vector)

addPlayer1 <- BL08_Gg3[ which(BL08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_Gg3[ which(BL08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_Gg2 <- rbind(BL08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
BL08_Gft <- ftable(BL08_Gg2$player1, BL08_Gg2$player2)
BL08_Gft2 <- as.matrix(BL08_Gft)
numRows <- nrow(BL08_Gft2)
numCols <- ncol(BL08_Gft2)
BL08_Gft3 <- BL08_Gft2[c(2:numRows) , c(2:numCols)]
BL08_GTable <- graph.adjacency(BL08_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(BL08_GTable, vertex.label = V(BL08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
BL08_G.clusterCoef <- transitivity(BL08_GTable, type="global") #cluster coefficient
BL08_G.degreeCent <- centralization.degree(BL08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_Gftn <- as.network.matrix(BL08_Gft)
BL08_G.netDensity <- network.density(BL08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_G.entropy <- entropy(BL08_Gft) #entropy

BL08_G.netMx <- cbind(BL08_G.netMx, BL08_G.clusterCoef, BL08_G.degreeCent$centralization,
                      BL08_G.netDensity, BL08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "BL"
KIoutcome = "Behind_F"
BL08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
BL08_Bg2 <- data.frame(BL08_B)
BL08_Bg2 <- BL08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_Bg2$player1
player2vector <- BL08_Bg2$player2
BL08_Bg3 <- BL08_Bg2
BL08_Bg3$p1inp2vec <- is.element(BL08_Bg3$player1, player2vector)
BL08_Bg3$p2inp1vec <- is.element(BL08_Bg3$player2, player1vector)

addPlayer1 <- BL08_Bg3[ which(BL08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_Bg3[ which(BL08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_Bg2 <- rbind(BL08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
BL08_Bft <- ftable(BL08_Bg2$player1, BL08_Bg2$player2)
BL08_Bft2 <- as.matrix(BL08_Bft)
numRows <- nrow(BL08_Bft2)
numCols <- ncol(BL08_Bft2)
BL08_Bft3 <- BL08_Bft2[c(2:numRows) , c(2:numCols)]
BL08_BTable <- graph.adjacency(BL08_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(BL08_BTable, vertex.label = V(BL08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
BL08_B.clusterCoef <- transitivity(BL08_BTable, type="global") #cluster coefficient
BL08_B.degreeCent <- centralization.degree(BL08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_Bftn <- as.network.matrix(BL08_Bft)
BL08_B.netDensity <- network.density(BL08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_B.entropy <- entropy(BL08_Bft) #entropy

BL08_B.netMx <- cbind(BL08_B.netMx, BL08_B.clusterCoef, BL08_B.degreeCent$centralization,
                      BL08_B.netDensity, BL08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "BL"
KIoutcome = "Stoppage_F"
BL08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
BL08_SFg2 <- data.frame(BL08_SF)
BL08_SFg2 <- BL08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_SFg2$player1
player2vector <- BL08_SFg2$player2
BL08_SFg3 <- BL08_SFg2
BL08_SFg3$p1inp2vec <- is.element(BL08_SFg3$player1, player2vector)
BL08_SFg3$p2inp1vec <- is.element(BL08_SFg3$player2, player1vector)

addPlayer1 <- BL08_SFg3[ which(BL08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_SFg3[ which(BL08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_SFg2 <- rbind(BL08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
BL08_SFft <- ftable(BL08_SFg2$player1, BL08_SFg2$player2)
BL08_SFft2 <- as.matrix(BL08_SFft)
numRows <- nrow(BL08_SFft2)
numCols <- ncol(BL08_SFft2)
BL08_SFft3 <- BL08_SFft2[c(2:numRows) , c(2:numCols)]
BL08_SFTable <- graph.adjacency(BL08_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(BL08_SFTable, vertex.label = V(BL08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
BL08_SF.clusterCoef <- transitivity(BL08_SFTable, type="global") #cluster coefficient
BL08_SF.degreeCent <- centralization.degree(BL08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_SFftn <- as.network.matrix(BL08_SFft)
BL08_SF.netDensity <- network.density(BL08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_SF.entropy <- entropy(BL08_SFft) #entropy

BL08_SF.netMx <- cbind(BL08_SF.netMx, BL08_SF.clusterCoef, BL08_SF.degreeCent$centralization,
                       BL08_SF.netDensity, BL08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "BL"
KIoutcome = "Turnover_F"
BL08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
BL08_TFg2 <- data.frame(BL08_TF)
BL08_TFg2 <- BL08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_TFg2$player1
player2vector <- BL08_TFg2$player2
BL08_TFg3 <- BL08_TFg2
BL08_TFg3$p1inp2vec <- is.element(BL08_TFg3$player1, player2vector)
BL08_TFg3$p2inp1vec <- is.element(BL08_TFg3$player2, player1vector)

addPlayer1 <- BL08_TFg3[ which(BL08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_TFg3[ which(BL08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_TFg2 <- rbind(BL08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
BL08_TFft <- ftable(BL08_TFg2$player1, BL08_TFg2$player2)
BL08_TFft2 <- as.matrix(BL08_TFft)
numRows <- nrow(BL08_TFft2)
numCols <- ncol(BL08_TFft2)
BL08_TFft3 <- BL08_TFft2[c(2:numRows) , c(2:numCols)]
BL08_TFTable <- graph.adjacency(BL08_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(BL08_TFTable, vertex.label = V(BL08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
BL08_TF.clusterCoef <- transitivity(BL08_TFTable, type="global") #cluster coefficient
BL08_TF.degreeCent <- centralization.degree(BL08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_TFftn <- as.network.matrix(BL08_TFft)
BL08_TF.netDensity <- network.density(BL08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_TF.entropy <- entropy(BL08_TFft) #entropy

BL08_TF.netMx <- cbind(BL08_TF.netMx, BL08_TF.clusterCoef, BL08_TF.degreeCent$centralization,
                       BL08_TF.netDensity, BL08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************
#NA

round = 8
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
BL08_SAMg2 <- data.frame(BL08_SAM)
BL08_SAMg2 <- BL08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_SAMg2$player1
player2vector <- BL08_SAMg2$player2
BL08_SAMg3 <- BL08_SAMg2
BL08_SAMg3$p1inp2vec <- is.element(BL08_SAMg3$player1, player2vector)
BL08_SAMg3$p2inp1vec <- is.element(BL08_SAMg3$player2, player1vector)

addPlayer1 <- BL08_SAMg3[ which(BL08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_SAMg3[ which(BL08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_SAMg2 <- rbind(BL08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
BL08_SAMft <- ftable(BL08_SAMg2$player1, BL08_SAMg2$player2)
BL08_SAMft2 <- as.matrix(BL08_SAMft)
numRows <- nrow(BL08_SAMft2)
numCols <- ncol(BL08_SAMft2)
BL08_SAMft3 <- BL08_SAMft2[c(2:numRows) , c(2:numCols)]
BL08_SAMTable <- graph.adjacency(BL08_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(BL08_SAMTable, vertex.label = V(BL08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
BL08_SAM.clusterCoef <- transitivity(BL08_SAMTable, type="global") #cluster coefficient
BL08_SAM.degreeCent <- centralization.degree(BL08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_SAMftn <- as.network.matrix(BL08_SAMft)
BL08_SAM.netDensity <- network.density(BL08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_SAM.entropy <- entropy(BL08_SAMft) #entropy

BL08_SAM.netMx <- cbind(BL08_SAM.netMx, BL08_SAM.clusterCoef, BL08_SAM.degreeCent$centralization,
                        BL08_SAM.netDensity, BL08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************
#NA

round = 8
teamName = "BL"
KIoutcome = "Turnover_AM"
BL08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
BL08_TAMg2 <- data.frame(BL08_TAM)
BL08_TAMg2 <- BL08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_TAMg2$player1
player2vector <- BL08_TAMg2$player2
BL08_TAMg3 <- BL08_TAMg2
BL08_TAMg3$p1inp2vec <- is.element(BL08_TAMg3$player1, player2vector)
BL08_TAMg3$p2inp1vec <- is.element(BL08_TAMg3$player2, player1vector)

addPlayer1 <- BL08_TAMg3[ which(BL08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_TAMg3[ which(BL08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_TAMg2 <- rbind(BL08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
BL08_TAMft <- ftable(BL08_TAMg2$player1, BL08_TAMg2$player2)
BL08_TAMft2 <- as.matrix(BL08_TAMft)
numRows <- nrow(BL08_TAMft2)
numCols <- ncol(BL08_TAMft2)
BL08_TAMft3 <- BL08_TAMft2[c(2:numRows) , c(2:numCols)]
BL08_TAMTable <- graph.adjacency(BL08_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(BL08_TAMTable, vertex.label = V(BL08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
BL08_TAM.clusterCoef <- transitivity(BL08_TAMTable, type="global") #cluster coefficient
BL08_TAM.degreeCent <- centralization.degree(BL08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_TAMftn <- as.network.matrix(BL08_TAMft)
BL08_TAM.netDensity <- network.density(BL08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_TAM.entropy <- entropy(BL08_TAMft) #entropy

BL08_TAM.netMx <- cbind(BL08_TAM.netMx, BL08_TAM.clusterCoef, BL08_TAM.degreeCent$centralization,
                        BL08_TAM.netDensity, BL08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
BL08_SDMg2 <- data.frame(BL08_SDM)
BL08_SDMg2 <- BL08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_SDMg2$player1
player2vector <- BL08_SDMg2$player2
BL08_SDMg3 <- BL08_SDMg2
BL08_SDMg3$p1inp2vec <- is.element(BL08_SDMg3$player1, player2vector)
BL08_SDMg3$p2inp1vec <- is.element(BL08_SDMg3$player2, player1vector)

addPlayer1 <- BL08_SDMg3[ which(BL08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_SDMg3[ which(BL08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_SDMg2 <- rbind(BL08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
BL08_SDMft <- ftable(BL08_SDMg2$player1, BL08_SDMg2$player2)
BL08_SDMft2 <- as.matrix(BL08_SDMft)
numRows <- nrow(BL08_SDMft2)
numCols <- ncol(BL08_SDMft2)
BL08_SDMft3 <- BL08_SDMft2[c(2:numRows) , c(2:numCols)]
BL08_SDMTable <- graph.adjacency(BL08_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(BL08_SDMTable, vertex.label = V(BL08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
BL08_SDM.clusterCoef <- transitivity(BL08_SDMTable, type="global") #cluster coefficient
BL08_SDM.degreeCent <- centralization.degree(BL08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_SDMftn <- as.network.matrix(BL08_SDMft)
BL08_SDM.netDensity <- network.density(BL08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_SDM.entropy <- entropy(BL08_SDMft) #entropy

BL08_SDM.netMx <- cbind(BL08_SDM.netMx, BL08_SDM.clusterCoef, BL08_SDM.degreeCent$centralization,
                        BL08_SDM.netDensity, BL08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "BL"
KIoutcome = "Turnover_DM"
BL08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
BL08_TDMg2 <- data.frame(BL08_TDM)
BL08_TDMg2 <- BL08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_TDMg2$player1
player2vector <- BL08_TDMg2$player2
BL08_TDMg3 <- BL08_TDMg2
BL08_TDMg3$p1inp2vec <- is.element(BL08_TDMg3$player1, player2vector)
BL08_TDMg3$p2inp1vec <- is.element(BL08_TDMg3$player2, player1vector)

addPlayer1 <- BL08_TDMg3[ which(BL08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_TDMg3[ which(BL08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_TDMg2 <- rbind(BL08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
BL08_TDMft <- ftable(BL08_TDMg2$player1, BL08_TDMg2$player2)
BL08_TDMft2 <- as.matrix(BL08_TDMft)
numRows <- nrow(BL08_TDMft2)
numCols <- ncol(BL08_TDMft2)
BL08_TDMft3 <- BL08_TDMft2[c(2:numRows) , c(2:numCols)]
BL08_TDMTable <- graph.adjacency(BL08_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(BL08_TDMTable, vertex.label = V(BL08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
BL08_TDM.clusterCoef <- transitivity(BL08_TDMTable, type="global") #cluster coefficient
BL08_TDM.degreeCent <- centralization.degree(BL08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_TDMftn <- as.network.matrix(BL08_TDMft)
BL08_TDM.netDensity <- network.density(BL08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_TDM.entropy <- entropy(BL08_TDMft) #entropy

BL08_TDM.netMx <- cbind(BL08_TDM.netMx, BL08_TDM.clusterCoef, BL08_TDM.degreeCent$centralization,
                        BL08_TDM.netDensity, BL08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "BL"
KIoutcome = "Stoppage_D"
BL08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
BL08_SDg2 <- data.frame(BL08_SD)
BL08_SDg2 <- BL08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_SDg2$player1
player2vector <- BL08_SDg2$player2
BL08_SDg3 <- BL08_SDg2
BL08_SDg3$p1inp2vec <- is.element(BL08_SDg3$player1, player2vector)
BL08_SDg3$p2inp1vec <- is.element(BL08_SDg3$player2, player1vector)

addPlayer1 <- BL08_SDg3[ which(BL08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_SDg3[ which(BL08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_SDg2 <- rbind(BL08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
BL08_SDft <- ftable(BL08_SDg2$player1, BL08_SDg2$player2)
BL08_SDft2 <- as.matrix(BL08_SDft)
numRows <- nrow(BL08_SDft2)
numCols <- ncol(BL08_SDft2)
BL08_SDft3 <- BL08_SDft2[c(2:numRows) , c(2:numCols)]
BL08_SDTable <- graph.adjacency(BL08_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(BL08_SDTable, vertex.label = V(BL08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
BL08_SD.clusterCoef <- transitivity(BL08_SDTable, type="global") #cluster coefficient
BL08_SD.degreeCent <- centralization.degree(BL08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_SDftn <- as.network.matrix(BL08_SDft)
BL08_SD.netDensity <- network.density(BL08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_SD.entropy <- entropy(BL08_SDft) #entropy

BL08_SD.netMx <- cbind(BL08_SD.netMx, BL08_SD.clusterCoef, BL08_SD.degreeCent$centralization,
                       BL08_SD.netDensity, BL08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "BL"
KIoutcome = "Turnover_D"
BL08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
BL08_TDg2 <- data.frame(BL08_TD)
BL08_TDg2 <- BL08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_TDg2$player1
player2vector <- BL08_TDg2$player2
BL08_TDg3 <- BL08_TDg2
BL08_TDg3$p1inp2vec <- is.element(BL08_TDg3$player1, player2vector)
BL08_TDg3$p2inp1vec <- is.element(BL08_TDg3$player2, player1vector)

addPlayer1 <- BL08_TDg3[ which(BL08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_TDg3[ which(BL08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_TDg2 <- rbind(BL08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
BL08_TDft <- ftable(BL08_TDg2$player1, BL08_TDg2$player2)
BL08_TDft2 <- as.matrix(BL08_TDft)
numRows <- nrow(BL08_TDft2)
numCols <- ncol(BL08_TDft2)
BL08_TDft3 <- BL08_TDft2[c(2:numRows) , c(2:numCols)]
BL08_TDTable <- graph.adjacency(BL08_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(BL08_TDTable, vertex.label = V(BL08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
BL08_TD.clusterCoef <- transitivity(BL08_TDTable, type="global") #cluster coefficient
BL08_TD.degreeCent <- centralization.degree(BL08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_TDftn <- as.network.matrix(BL08_TDft)
BL08_TD.netDensity <- network.density(BL08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_TD.entropy <- entropy(BL08_TDft) #entropy

BL08_TD.netMx <- cbind(BL08_TD.netMx, BL08_TD.clusterCoef, BL08_TD.degreeCent$centralization,
                       BL08_TD.netDensity, BL08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
BL08_QTg2 <- data.frame(BL08_QT)
BL08_QTg2 <- BL08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL08_QTg2$player1
player2vector <- BL08_QTg2$player2
BL08_QTg3 <- BL08_QTg2
BL08_QTg3$p1inp2vec <- is.element(BL08_QTg3$player1, player2vector)
BL08_QTg3$p2inp1vec <- is.element(BL08_QTg3$player2, player1vector)

addPlayer1 <- BL08_QTg3[ which(BL08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL08_QTg3[ which(BL08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL08_QTg2 <- rbind(BL08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
BL08_QTft <- ftable(BL08_QTg2$player1, BL08_QTg2$player2)
BL08_QTft2 <- as.matrix(BL08_QTft)
numRows <- nrow(BL08_QTft2)
numCols <- ncol(BL08_QTft2)
BL08_QTft3 <- BL08_QTft2[c(2:numRows) , c(2:numCols)]
BL08_QTTable <- graph.adjacency(BL08_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(BL08_QTTable, vertex.label = V(BL08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
BL08_QT.clusterCoef <- transitivity(BL08_QTTable, type="global") #cluster coefficient
BL08_QT.degreeCent <- centralization.degree(BL08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL08_QTftn <- as.network.matrix(BL08_QTft)
BL08_QT.netDensity <- network.density(BL08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL08_QT.entropy <- entropy(BL08_QTft) #entropy

BL08_QT.netMx <- cbind(BL08_QT.netMx, BL08_QT.clusterCoef, BL08_QT.degreeCent$centralization,
                       BL08_QT.netDensity, BL08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL08_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************

round = 8
teamName = "CARL"
KIoutcome = "Goal_F"
CARL08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
CARL08_Gg2 <- data.frame(CARL08_G)
CARL08_Gg2 <- CARL08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_Gg2$player1
player2vector <- CARL08_Gg2$player2
CARL08_Gg3 <- CARL08_Gg2
CARL08_Gg3$p1inp2vec <- is.element(CARL08_Gg3$player1, player2vector)
CARL08_Gg3$p2inp1vec <- is.element(CARL08_Gg3$player2, player1vector)

addPlayer1 <- CARL08_Gg3[ which(CARL08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_Gg3[ which(CARL08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_Gg2 <- rbind(CARL08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
CARL08_Gft <- ftable(CARL08_Gg2$player1, CARL08_Gg2$player2)
CARL08_Gft2 <- as.matrix(CARL08_Gft)
numRows <- nrow(CARL08_Gft2)
numCols <- ncol(CARL08_Gft2)
CARL08_Gft3 <- CARL08_Gft2[c(2:numRows) , c(2:numCols)]
CARL08_GTable <- graph.adjacency(CARL08_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(CARL08_GTable, vertex.label = V(CARL08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
CARL08_G.clusterCoef <- transitivity(CARL08_GTable, type="global") #cluster coefficient
CARL08_G.degreeCent <- centralization.degree(CARL08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_Gftn <- as.network.matrix(CARL08_Gft)
CARL08_G.netDensity <- network.density(CARL08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_G.entropy <- entropy(CARL08_Gft) #entropy

CARL08_G.netMx <- cbind(CARL08_G.netMx, CARL08_G.clusterCoef, CARL08_G.degreeCent$centralization,
                        CARL08_G.netDensity, CARL08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "CARL"
KIoutcome = "Behind_F"
CARL08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
CARL08_Bg2 <- data.frame(CARL08_B)
CARL08_Bg2 <- CARL08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_Bg2$player1
player2vector <- CARL08_Bg2$player2
CARL08_Bg3 <- CARL08_Bg2
CARL08_Bg3$p1inp2vec <- is.element(CARL08_Bg3$player1, player2vector)
CARL08_Bg3$p2inp1vec <- is.element(CARL08_Bg3$player2, player1vector)

addPlayer1 <- CARL08_Bg3[ which(CARL08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_Bg3[ which(CARL08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_Bg2 <- rbind(CARL08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
CARL08_Bft <- ftable(CARL08_Bg2$player1, CARL08_Bg2$player2)
CARL08_Bft2 <- as.matrix(CARL08_Bft)
numRows <- nrow(CARL08_Bft2)
numCols <- ncol(CARL08_Bft2)
CARL08_Bft3 <- CARL08_Bft2[c(2:numRows) , c(2:numCols)]
CARL08_BTable <- graph.adjacency(CARL08_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(CARL08_BTable, vertex.label = V(CARL08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
CARL08_B.clusterCoef <- transitivity(CARL08_BTable, type="global") #cluster coefficient
CARL08_B.degreeCent <- centralization.degree(CARL08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_Bftn <- as.network.matrix(CARL08_Bft)
CARL08_B.netDensity <- network.density(CARL08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_B.entropy <- entropy(CARL08_Bft) #entropy

CARL08_B.netMx <- cbind(CARL08_B.netMx, CARL08_B.clusterCoef, CARL08_B.degreeCent$centralization,
                        CARL08_B.netDensity, CARL08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
CARL08_SFg2 <- data.frame(CARL08_SF)
CARL08_SFg2 <- CARL08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_SFg2$player1
player2vector <- CARL08_SFg2$player2
CARL08_SFg3 <- CARL08_SFg2
CARL08_SFg3$p1inp2vec <- is.element(CARL08_SFg3$player1, player2vector)
CARL08_SFg3$p2inp1vec <- is.element(CARL08_SFg3$player2, player1vector)

addPlayer1 <- CARL08_SFg3[ which(CARL08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_SFg3[ which(CARL08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_SFg2 <- rbind(CARL08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
CARL08_SFft <- ftable(CARL08_SFg2$player1, CARL08_SFg2$player2)
CARL08_SFft2 <- as.matrix(CARL08_SFft)
numRows <- nrow(CARL08_SFft2)
numCols <- ncol(CARL08_SFft2)
CARL08_SFft3 <- CARL08_SFft2[c(2:numRows) , c(2:numCols)]
CARL08_SFTable <- graph.adjacency(CARL08_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(CARL08_SFTable, vertex.label = V(CARL08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
CARL08_SF.clusterCoef <- transitivity(CARL08_SFTable, type="global") #cluster coefficient
CARL08_SF.degreeCent <- centralization.degree(CARL08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_SFftn <- as.network.matrix(CARL08_SFft)
CARL08_SF.netDensity <- network.density(CARL08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_SF.entropy <- entropy(CARL08_SFft) #entropy

CARL08_SF.netMx <- cbind(CARL08_SF.netMx, CARL08_SF.clusterCoef, CARL08_SF.degreeCent$centralization,
                         CARL08_SF.netDensity, CARL08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
CARL08_TFg2 <- data.frame(CARL08_TF)
CARL08_TFg2 <- CARL08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_TFg2$player1
player2vector <- CARL08_TFg2$player2
CARL08_TFg3 <- CARL08_TFg2
CARL08_TFg3$p1inp2vec <- is.element(CARL08_TFg3$player1, player2vector)
CARL08_TFg3$p2inp1vec <- is.element(CARL08_TFg3$player2, player1vector)

addPlayer1 <- CARL08_TFg3[ which(CARL08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_TFg3[ which(CARL08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_TFg2 <- rbind(CARL08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
CARL08_TFft <- ftable(CARL08_TFg2$player1, CARL08_TFg2$player2)
CARL08_TFft2 <- as.matrix(CARL08_TFft)
numRows <- nrow(CARL08_TFft2)
numCols <- ncol(CARL08_TFft2)
CARL08_TFft3 <- CARL08_TFft2[c(2:numRows) , c(2:numCols)]
CARL08_TFTable <- graph.adjacency(CARL08_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(CARL08_TFTable, vertex.label = V(CARL08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
CARL08_TF.clusterCoef <- transitivity(CARL08_TFTable, type="global") #cluster coefficient
CARL08_TF.degreeCent <- centralization.degree(CARL08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_TFftn <- as.network.matrix(CARL08_TFft)
CARL08_TF.netDensity <- network.density(CARL08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_TF.entropy <- entropy(CARL08_TFft) #entropy

CARL08_TF.netMx <- cbind(CARL08_TF.netMx, CARL08_TF.clusterCoef, CARL08_TF.degreeCent$centralization,
                         CARL08_TF.netDensity, CARL08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************
#NA

round = 8
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
CARL08_SAMg2 <- data.frame(CARL08_SAM)
CARL08_SAMg2 <- CARL08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_SAMg2$player1
player2vector <- CARL08_SAMg2$player2
CARL08_SAMg3 <- CARL08_SAMg2
CARL08_SAMg3$p1inp2vec <- is.element(CARL08_SAMg3$player1, player2vector)
CARL08_SAMg3$p2inp1vec <- is.element(CARL08_SAMg3$player2, player1vector)

addPlayer1 <- CARL08_SAMg3[ which(CARL08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_SAMg3[ which(CARL08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_SAMg2 <- rbind(CARL08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
CARL08_SAMft <- ftable(CARL08_SAMg2$player1, CARL08_SAMg2$player2)
CARL08_SAMft2 <- as.matrix(CARL08_SAMft)
numRows <- nrow(CARL08_SAMft2)
numCols <- ncol(CARL08_SAMft2)
CARL08_SAMft3 <- CARL08_SAMft2[c(2:numRows) , c(2:numCols)]
CARL08_SAMTable <- graph.adjacency(CARL08_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(CARL08_SAMTable, vertex.label = V(CARL08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
CARL08_SAM.clusterCoef <- transitivity(CARL08_SAMTable, type="global") #cluster coefficient
CARL08_SAM.degreeCent <- centralization.degree(CARL08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_SAMftn <- as.network.matrix(CARL08_SAMft)
CARL08_SAM.netDensity <- network.density(CARL08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_SAM.entropy <- entropy(CARL08_SAMft) #entropy

CARL08_SAM.netMx <- cbind(CARL08_SAM.netMx, CARL08_SAM.clusterCoef, CARL08_SAM.degreeCent$centralization,
                          CARL08_SAM.netDensity, CARL08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************
#NA

round = 8
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
CARL08_TAMg2 <- data.frame(CARL08_TAM)
CARL08_TAMg2 <- CARL08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_TAMg2$player1
player2vector <- CARL08_TAMg2$player2
CARL08_TAMg3 <- CARL08_TAMg2
CARL08_TAMg3$p1inp2vec <- is.element(CARL08_TAMg3$player1, player2vector)
CARL08_TAMg3$p2inp1vec <- is.element(CARL08_TAMg3$player2, player1vector)

addPlayer1 <- CARL08_TAMg3[ which(CARL08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_TAMg3[ which(CARL08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_TAMg2 <- rbind(CARL08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
CARL08_TAMft <- ftable(CARL08_TAMg2$player1, CARL08_TAMg2$player2)
CARL08_TAMft2 <- as.matrix(CARL08_TAMft)
numRows <- nrow(CARL08_TAMft2)
numCols <- ncol(CARL08_TAMft2)
CARL08_TAMft3 <- CARL08_TAMft2[c(2:numRows) , c(2:numCols)]
CARL08_TAMTable <- graph.adjacency(CARL08_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(CARL08_TAMTable, vertex.label = V(CARL08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
CARL08_TAM.clusterCoef <- transitivity(CARL08_TAMTable, type="global") #cluster coefficient
CARL08_TAM.degreeCent <- centralization.degree(CARL08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_TAMftn <- as.network.matrix(CARL08_TAMft)
CARL08_TAM.netDensity <- network.density(CARL08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_TAM.entropy <- entropy(CARL08_TAMft) #entropy

CARL08_TAM.netMx <- cbind(CARL08_TAM.netMx, CARL08_TAM.clusterCoef, CARL08_TAM.degreeCent$centralization,
                          CARL08_TAM.netDensity, CARL08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
CARL08_SDMg2 <- data.frame(CARL08_SDM)
CARL08_SDMg2 <- CARL08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_SDMg2$player1
player2vector <- CARL08_SDMg2$player2
CARL08_SDMg3 <- CARL08_SDMg2
CARL08_SDMg3$p1inp2vec <- is.element(CARL08_SDMg3$player1, player2vector)
CARL08_SDMg3$p2inp1vec <- is.element(CARL08_SDMg3$player2, player1vector)

addPlayer1 <- CARL08_SDMg3[ which(CARL08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_SDMg3[ which(CARL08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_SDMg2 <- rbind(CARL08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
CARL08_SDMft <- ftable(CARL08_SDMg2$player1, CARL08_SDMg2$player2)
CARL08_SDMft2 <- as.matrix(CARL08_SDMft)
numRows <- nrow(CARL08_SDMft2)
numCols <- ncol(CARL08_SDMft2)
CARL08_SDMft3 <- CARL08_SDMft2[c(2:numRows) , c(2:numCols)]
CARL08_SDMTable <- graph.adjacency(CARL08_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(CARL08_SDMTable, vertex.label = V(CARL08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
CARL08_SDM.clusterCoef <- transitivity(CARL08_SDMTable, type="global") #cluster coefficient
CARL08_SDM.degreeCent <- centralization.degree(CARL08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_SDMftn <- as.network.matrix(CARL08_SDMft)
CARL08_SDM.netDensity <- network.density(CARL08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_SDM.entropy <- entropy(CARL08_SDMft) #entropy

CARL08_SDM.netMx <- cbind(CARL08_SDM.netMx, CARL08_SDM.clusterCoef, CARL08_SDM.degreeCent$centralization,
                          CARL08_SDM.netDensity, CARL08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
CARL08_TDMg2 <- data.frame(CARL08_TDM)
CARL08_TDMg2 <- CARL08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_TDMg2$player1
player2vector <- CARL08_TDMg2$player2
CARL08_TDMg3 <- CARL08_TDMg2
CARL08_TDMg3$p1inp2vec <- is.element(CARL08_TDMg3$player1, player2vector)
CARL08_TDMg3$p2inp1vec <- is.element(CARL08_TDMg3$player2, player1vector)

addPlayer1 <- CARL08_TDMg3[ which(CARL08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_TDMg3[ which(CARL08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_TDMg2 <- rbind(CARL08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
CARL08_TDMft <- ftable(CARL08_TDMg2$player1, CARL08_TDMg2$player2)
CARL08_TDMft2 <- as.matrix(CARL08_TDMft)
numRows <- nrow(CARL08_TDMft2)
numCols <- ncol(CARL08_TDMft2)
CARL08_TDMft3 <- CARL08_TDMft2[c(2:numRows) , c(2:numCols)]
CARL08_TDMTable <- graph.adjacency(CARL08_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(CARL08_TDMTable, vertex.label = V(CARL08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
CARL08_TDM.clusterCoef <- transitivity(CARL08_TDMTable, type="global") #cluster coefficient
CARL08_TDM.degreeCent <- centralization.degree(CARL08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_TDMftn <- as.network.matrix(CARL08_TDMft)
CARL08_TDM.netDensity <- network.density(CARL08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_TDM.entropy <- entropy(CARL08_TDMft) #entropy

CARL08_TDM.netMx <- cbind(CARL08_TDM.netMx, CARL08_TDM.clusterCoef, CARL08_TDM.degreeCent$centralization,
                          CARL08_TDM.netDensity, CARL08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
CARL08_SDg2 <- data.frame(CARL08_SD)
CARL08_SDg2 <- CARL08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_SDg2$player1
player2vector <- CARL08_SDg2$player2
CARL08_SDg3 <- CARL08_SDg2
CARL08_SDg3$p1inp2vec <- is.element(CARL08_SDg3$player1, player2vector)
CARL08_SDg3$p2inp1vec <- is.element(CARL08_SDg3$player2, player1vector)

addPlayer1 <- CARL08_SDg3[ which(CARL08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_SDg3[ which(CARL08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_SDg2 <- rbind(CARL08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
CARL08_SDft <- ftable(CARL08_SDg2$player1, CARL08_SDg2$player2)
CARL08_SDft2 <- as.matrix(CARL08_SDft)
numRows <- nrow(CARL08_SDft2)
numCols <- ncol(CARL08_SDft2)
CARL08_SDft3 <- CARL08_SDft2[c(2:numRows) , c(2:numCols)]
CARL08_SDTable <- graph.adjacency(CARL08_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(CARL08_SDTable, vertex.label = V(CARL08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
CARL08_SD.clusterCoef <- transitivity(CARL08_SDTable, type="global") #cluster coefficient
CARL08_SD.degreeCent <- centralization.degree(CARL08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_SDftn <- as.network.matrix(CARL08_SDft)
CARL08_SD.netDensity <- network.density(CARL08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_SD.entropy <- entropy(CARL08_SDft) #entropy

CARL08_SD.netMx <- cbind(CARL08_SD.netMx, CARL08_SD.clusterCoef, CARL08_SD.degreeCent$centralization,
                         CARL08_SD.netDensity, CARL08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************

round = 8
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
CARL08_TDg2 <- data.frame(CARL08_TD)
CARL08_TDg2 <- CARL08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_TDg2$player1
player2vector <- CARL08_TDg2$player2
CARL08_TDg3 <- CARL08_TDg2
CARL08_TDg3$p1inp2vec <- is.element(CARL08_TDg3$player1, player2vector)
CARL08_TDg3$p2inp1vec <- is.element(CARL08_TDg3$player2, player1vector)

addPlayer1 <- CARL08_TDg3[ which(CARL08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_TDg3[ which(CARL08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_TDg2 <- rbind(CARL08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
CARL08_TDft <- ftable(CARL08_TDg2$player1, CARL08_TDg2$player2)
CARL08_TDft2 <- as.matrix(CARL08_TDft)
numRows <- nrow(CARL08_TDft2)
numCols <- ncol(CARL08_TDft2)
CARL08_TDft3 <- CARL08_TDft2[c(2:numRows) , c(2:numCols)]
CARL08_TDTable <- graph.adjacency(CARL08_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(CARL08_TDTable, vertex.label = V(CARL08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
CARL08_TD.clusterCoef <- transitivity(CARL08_TDTable, type="global") #cluster coefficient
CARL08_TD.degreeCent <- centralization.degree(CARL08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_TDftn <- as.network.matrix(CARL08_TDft)
CARL08_TD.netDensity <- network.density(CARL08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_TD.entropy <- entropy(CARL08_TDft) #entropy

CARL08_TD.netMx <- cbind(CARL08_TD.netMx, CARL08_TD.clusterCoef, CARL08_TD.degreeCent$centralization,
                         CARL08_TD.netDensity, CARL08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
CARL08_QTg2 <- data.frame(CARL08_QT)
CARL08_QTg2 <- CARL08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL08_QTg2$player1
player2vector <- CARL08_QTg2$player2
CARL08_QTg3 <- CARL08_QTg2
CARL08_QTg3$p1inp2vec <- is.element(CARL08_QTg3$player1, player2vector)
CARL08_QTg3$p2inp1vec <- is.element(CARL08_QTg3$player2, player1vector)

addPlayer1 <- CARL08_QTg3[ which(CARL08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL08_QTg3[ which(CARL08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL08_QTg2 <- rbind(CARL08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
CARL08_QTft <- ftable(CARL08_QTg2$player1, CARL08_QTg2$player2)
CARL08_QTft2 <- as.matrix(CARL08_QTft)
numRows <- nrow(CARL08_QTft2)
numCols <- ncol(CARL08_QTft2)
CARL08_QTft3 <- CARL08_QTft2[c(2:numRows) , c(2:numCols)]
CARL08_QTTable <- graph.adjacency(CARL08_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(CARL08_QTTable, vertex.label = V(CARL08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
CARL08_QT.clusterCoef <- transitivity(CARL08_QTTable, type="global") #cluster coefficient
CARL08_QT.degreeCent <- centralization.degree(CARL08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL08_QTftn <- as.network.matrix(CARL08_QTft)
CARL08_QT.netDensity <- network.density(CARL08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL08_QT.entropy <- entropy(CARL08_QTft) #entropy

CARL08_QT.netMx <- cbind(CARL08_QT.netMx, CARL08_QT.clusterCoef, CARL08_QT.degreeCent$centralization,
                         CARL08_QT.netDensity, CARL08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL08_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "COLL"
KIoutcome = "Goal_F"
COLL08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
COLL08_Gg2 <- data.frame(COLL08_G)
COLL08_Gg2 <- COLL08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_Gg2$player1
player2vector <- COLL08_Gg2$player2
COLL08_Gg3 <- COLL08_Gg2
COLL08_Gg3$p1inp2vec <- is.element(COLL08_Gg3$player1, player2vector)
COLL08_Gg3$p2inp1vec <- is.element(COLL08_Gg3$player2, player1vector)

addPlayer1 <- COLL08_Gg3[ which(COLL08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL08_Gg3[ which(COLL08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_Gg2 <- rbind(COLL08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
COLL08_Gft <- ftable(COLL08_Gg2$player1, COLL08_Gg2$player2)
COLL08_Gft2 <- as.matrix(COLL08_Gft)
numRows <- nrow(COLL08_Gft2)
numCols <- ncol(COLL08_Gft2)
COLL08_Gft3 <- COLL08_Gft2[c(2:numRows) , c(2:numCols)]
COLL08_GTable <- graph.adjacency(COLL08_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(COLL08_GTable, vertex.label = V(COLL08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
COLL08_G.clusterCoef <- transitivity(COLL08_GTable, type="global") #cluster coefficient
COLL08_G.degreeCent <- centralization.degree(COLL08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_Gftn <- as.network.matrix(COLL08_Gft)
COLL08_G.netDensity <- network.density(COLL08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_G.entropy <- entropy(COLL08_Gft) #entropy

COLL08_G.netMx <- cbind(COLL08_G.netMx, COLL08_G.clusterCoef, COLL08_G.degreeCent$centralization,
                        COLL08_G.netDensity, COLL08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "COLL"
KIoutcome = "Behind_F"
COLL08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
COLL08_Bg2 <- data.frame(COLL08_B)
COLL08_Bg2 <- COLL08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_Bg2$player1
player2vector <- COLL08_Bg2$player2
COLL08_Bg3 <- COLL08_Bg2
COLL08_Bg3$p1inp2vec <- is.element(COLL08_Bg3$player1, player2vector)
COLL08_Bg3$p2inp1vec <- is.element(COLL08_Bg3$player2, player1vector)

addPlayer1 <- COLL08_Bg3[ which(COLL08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL08_Bg3[ which(COLL08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_Bg2 <- rbind(COLL08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
COLL08_Bft <- ftable(COLL08_Bg2$player1, COLL08_Bg2$player2)
COLL08_Bft2 <- as.matrix(COLL08_Bft)
numRows <- nrow(COLL08_Bft2)
numCols <- ncol(COLL08_Bft2)
COLL08_Bft3 <- COLL08_Bft2[c(2:numRows) , c(2:numCols)]
COLL08_BTable <- graph.adjacency(COLL08_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(COLL08_BTable, vertex.label = V(COLL08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
COLL08_B.clusterCoef <- transitivity(COLL08_BTable, type="global") #cluster coefficient
COLL08_B.degreeCent <- centralization.degree(COLL08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_Bftn <- as.network.matrix(COLL08_Bft)
COLL08_B.netDensity <- network.density(COLL08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_B.entropy <- entropy(COLL08_Bft) #entropy

COLL08_B.netMx <- cbind(COLL08_B.netMx, COLL08_B.clusterCoef, COLL08_B.degreeCent$centralization,
                        COLL08_B.netDensity, COLL08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
COLL08_SFg2 <- data.frame(COLL08_SF)
COLL08_SFg2 <- COLL08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_SFg2$player1
player2vector <- COLL08_SFg2$player2
COLL08_SFg3 <- COLL08_SFg2
COLL08_SFg3$p1inp2vec <- is.element(COLL08_SFg3$player1, player2vector)
COLL08_SFg3$p2inp1vec <- is.element(COLL08_SFg3$player2, player1vector)

addPlayer1 <- COLL08_SFg3[ which(COLL08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL08_SFg3[ which(COLL08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_SFg2 <- rbind(COLL08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
COLL08_SFft <- ftable(COLL08_SFg2$player1, COLL08_SFg2$player2)
COLL08_SFft2 <- as.matrix(COLL08_SFft)
numRows <- nrow(COLL08_SFft2)
numCols <- ncol(COLL08_SFft2)
COLL08_SFft3 <- COLL08_SFft2[c(2:numRows) , c(2:numCols)]
COLL08_SFTable <- graph.adjacency(COLL08_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(COLL08_SFTable, vertex.label = V(COLL08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
COLL08_SF.clusterCoef <- transitivity(COLL08_SFTable, type="global") #cluster coefficient
COLL08_SF.degreeCent <- centralization.degree(COLL08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_SFftn <- as.network.matrix(COLL08_SFft)
COLL08_SF.netDensity <- network.density(COLL08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_SF.entropy <- entropy(COLL08_SFft) #entropy

COLL08_SF.netMx <- cbind(COLL08_SF.netMx, COLL08_SF.clusterCoef, COLL08_SF.degreeCent$centralization,
                         COLL08_SF.netDensity, COLL08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************
#NA

round = 8
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
COLL08_TFg2 <- data.frame(COLL08_TF)
COLL08_TFg2 <- COLL08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_TFg2$player1
player2vector <- COLL08_TFg2$player2
COLL08_TFg3 <- COLL08_TFg2
COLL08_TFg3$p1inp2vec <- is.element(COLL08_TFg3$player1, player2vector)
COLL08_TFg3$p2inp1vec <- is.element(COLL08_TFg3$player2, player1vector)

addPlayer1 <- COLL08_TFg3[ which(COLL08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL08_TFg3[ which(COLL08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_TFg2 <- rbind(COLL08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
COLL08_TFft <- ftable(COLL08_TFg2$player1, COLL08_TFg2$player2)
COLL08_TFft2 <- as.matrix(COLL08_TFft)
numRows <- nrow(COLL08_TFft2)
numCols <- ncol(COLL08_TFft2)
COLL08_TFft3 <- COLL08_TFft2[c(2:numRows) , c(2:numCols)]
COLL08_TFTable <- graph.adjacency(COLL08_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(COLL08_TFTable, vertex.label = V(COLL08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
COLL08_TF.clusterCoef <- transitivity(COLL08_TFTable, type="global") #cluster coefficient
COLL08_TF.degreeCent <- centralization.degree(COLL08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_TFftn <- as.network.matrix(COLL08_TFft)
COLL08_TF.netDensity <- network.density(COLL08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_TF.entropy <- entropy(COLL08_TFft) #entropy

COLL08_TF.netMx <- cbind(COLL08_TF.netMx, COLL08_TF.clusterCoef, COLL08_TF.degreeCent$centralization,
                         COLL08_TF.netDensity, COLL08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************

round = 8
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
COLL08_SAMg2 <- data.frame(COLL08_SAM)
COLL08_SAMg2 <- COLL08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_SAMg2$player1
player2vector <- COLL08_SAMg2$player2
COLL08_SAMg3 <- COLL08_SAMg2
COLL08_SAMg3$p1inp2vec <- is.element(COLL08_SAMg3$player1, player2vector)
COLL08_SAMg3$p2inp1vec <- is.element(COLL08_SAMg3$player2, player1vector)

addPlayer1 <- COLL08_SAMg3[ which(COLL08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL08_SAMg3[ which(COLL08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_SAMg2 <- rbind(COLL08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
COLL08_SAMft <- ftable(COLL08_SAMg2$player1, COLL08_SAMg2$player2)
COLL08_SAMft2 <- as.matrix(COLL08_SAMft)
numRows <- nrow(COLL08_SAMft2)
numCols <- ncol(COLL08_SAMft2)
COLL08_SAMft3 <- COLL08_SAMft2[c(2:numRows) , c(2:numCols)]
COLL08_SAMTable <- graph.adjacency(COLL08_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(COLL08_SAMTable, vertex.label = V(COLL08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
COLL08_SAM.clusterCoef <- transitivity(COLL08_SAMTable, type="global") #cluster coefficient
COLL08_SAM.degreeCent <- centralization.degree(COLL08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_SAMftn <- as.network.matrix(COLL08_SAMft)
COLL08_SAM.netDensity <- network.density(COLL08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_SAM.entropy <- entropy(COLL08_SAMft) #entropy

COLL08_SAM.netMx <- cbind(COLL08_SAM.netMx, COLL08_SAM.clusterCoef, COLL08_SAM.degreeCent$centralization,
                          COLL08_SAM.netDensity, COLL08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
COLL08_TAMg2 <- data.frame(COLL08_TAM)
COLL08_TAMg2 <- COLL08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_TAMg2$player1
player2vector <- COLL08_TAMg2$player2
COLL08_TAMg3 <- COLL08_TAMg2
COLL08_TAMg3$p1inp2vec <- is.element(COLL08_TAMg3$player1, player2vector)
COLL08_TAMg3$p2inp1vec <- is.element(COLL08_TAMg3$player2, player1vector)

addPlayer1 <- COLL08_TAMg3[ which(COLL08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL08_TAMg3[ which(COLL08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_TAMg2 <- rbind(COLL08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
COLL08_TAMft <- ftable(COLL08_TAMg2$player1, COLL08_TAMg2$player2)
COLL08_TAMft2 <- as.matrix(COLL08_TAMft)
numRows <- nrow(COLL08_TAMft2)
numCols <- ncol(COLL08_TAMft2)
COLL08_TAMft3 <- COLL08_TAMft2[c(2:numRows) , c(2:numCols)]
COLL08_TAMTable <- graph.adjacency(COLL08_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(COLL08_TAMTable, vertex.label = V(COLL08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
COLL08_TAM.clusterCoef <- transitivity(COLL08_TAMTable, type="global") #cluster coefficient
COLL08_TAM.degreeCent <- centralization.degree(COLL08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_TAMftn <- as.network.matrix(COLL08_TAMft)
COLL08_TAM.netDensity <- network.density(COLL08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_TAM.entropy <- entropy(COLL08_TAMft) #entropy

COLL08_TAM.netMx <- cbind(COLL08_TAM.netMx, COLL08_TAM.clusterCoef, COLL08_TAM.degreeCent$centralization,
                          COLL08_TAM.netDensity, COLL08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
COLL08_SDMg2 <- data.frame(COLL08_SDM)
COLL08_SDMg2 <- COLL08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_SDMg2$player1
player2vector <- COLL08_SDMg2$player2
COLL08_SDMg3 <- COLL08_SDMg2
COLL08_SDMg3$p1inp2vec <- is.element(COLL08_SDMg3$player1, player2vector)
COLL08_SDMg3$p2inp1vec <- is.element(COLL08_SDMg3$player2, player1vector)

addPlayer1 <- COLL08_SDMg3[ which(COLL08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL08_SDMg3[ which(COLL08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_SDMg2 <- rbind(COLL08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
COLL08_SDMft <- ftable(COLL08_SDMg2$player1, COLL08_SDMg2$player2)
COLL08_SDMft2 <- as.matrix(COLL08_SDMft)
numRows <- nrow(COLL08_SDMft2)
numCols <- ncol(COLL08_SDMft2)
COLL08_SDMft3 <- COLL08_SDMft2[c(2:numRows) , c(2:numCols)]
COLL08_SDMTable <- graph.adjacency(COLL08_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(COLL08_SDMTable, vertex.label = V(COLL08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
COLL08_SDM.clusterCoef <- transitivity(COLL08_SDMTable, type="global") #cluster coefficient
COLL08_SDM.degreeCent <- centralization.degree(COLL08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_SDMftn <- as.network.matrix(COLL08_SDMft)
COLL08_SDM.netDensity <- network.density(COLL08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_SDM.entropy <- entropy(COLL08_SDMft) #entropy

COLL08_SDM.netMx <- cbind(COLL08_SDM.netMx, COLL08_SDM.clusterCoef, COLL08_SDM.degreeCent$centralization,
                          COLL08_SDM.netDensity, COLL08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
COLL08_TDMg2 <- data.frame(COLL08_TDM)
COLL08_TDMg2 <- COLL08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_TDMg2$player1
player2vector <- COLL08_TDMg2$player2
COLL08_TDMg3 <- COLL08_TDMg2
COLL08_TDMg3$p1inp2vec <- is.element(COLL08_TDMg3$player1, player2vector)
COLL08_TDMg3$p2inp1vec <- is.element(COLL08_TDMg3$player2, player1vector)

addPlayer1 <- COLL08_TDMg3[ which(COLL08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL08_TDMg3[ which(COLL08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_TDMg2 <- rbind(COLL08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
COLL08_TDMft <- ftable(COLL08_TDMg2$player1, COLL08_TDMg2$player2)
COLL08_TDMft2 <- as.matrix(COLL08_TDMft)
numRows <- nrow(COLL08_TDMft2)
numCols <- ncol(COLL08_TDMft2)
COLL08_TDMft3 <- COLL08_TDMft2[c(2:numRows) , c(2:numCols)]
COLL08_TDMTable <- graph.adjacency(COLL08_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(COLL08_TDMTable, vertex.label = V(COLL08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
COLL08_TDM.clusterCoef <- transitivity(COLL08_TDMTable, type="global") #cluster coefficient
COLL08_TDM.degreeCent <- centralization.degree(COLL08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_TDMftn <- as.network.matrix(COLL08_TDMft)
COLL08_TDM.netDensity <- network.density(COLL08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_TDM.entropy <- entropy(COLL08_TDMft) #entropy

COLL08_TDM.netMx <- cbind(COLL08_TDM.netMx, COLL08_TDM.clusterCoef, COLL08_TDM.degreeCent$centralization,
                          COLL08_TDM.netDensity, COLL08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
COLL08_SDg2 <- data.frame(COLL08_SD)
COLL08_SDg2 <- COLL08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_SDg2$player1
player2vector <- COLL08_SDg2$player2
COLL08_SDg3 <- COLL08_SDg2
COLL08_SDg3$p1inp2vec <- is.element(COLL08_SDg3$player1, player2vector)
COLL08_SDg3$p2inp1vec <- is.element(COLL08_SDg3$player2, player1vector)

addPlayer1 <- COLL08_SDg3[ which(COLL08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL08_SDg3[ which(COLL08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_SDg2 <- rbind(COLL08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
COLL08_SDft <- ftable(COLL08_SDg2$player1, COLL08_SDg2$player2)
COLL08_SDft2 <- as.matrix(COLL08_SDft)
numRows <- nrow(COLL08_SDft2)
numCols <- ncol(COLL08_SDft2)
COLL08_SDft3 <- COLL08_SDft2[c(2:numRows) , c(2:numCols)]
COLL08_SDTable <- graph.adjacency(COLL08_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(COLL08_SDTable, vertex.label = V(COLL08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
COLL08_SD.clusterCoef <- transitivity(COLL08_SDTable, type="global") #cluster coefficient
COLL08_SD.degreeCent <- centralization.degree(COLL08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_SDftn <- as.network.matrix(COLL08_SDft)
COLL08_SD.netDensity <- network.density(COLL08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_SD.entropy <- entropy(COLL08_SDft) #entropy

COLL08_SD.netMx <- cbind(COLL08_SD.netMx, COLL08_SD.clusterCoef, COLL08_SD.degreeCent$centralization,
                         COLL08_SD.netDensity, COLL08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
COLL08_TDg2 <- data.frame(COLL08_TD)
COLL08_TDg2 <- COLL08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_TDg2$player1
player2vector <- COLL08_TDg2$player2
COLL08_TDg3 <- COLL08_TDg2
COLL08_TDg3$p1inp2vec <- is.element(COLL08_TDg3$player1, player2vector)
COLL08_TDg3$p2inp1vec <- is.element(COLL08_TDg3$player2, player1vector)

addPlayer1 <- COLL08_TDg3[ which(COLL08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL08_TDg3[ which(COLL08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_TDg2 <- rbind(COLL08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
COLL08_TDft <- ftable(COLL08_TDg2$player1, COLL08_TDg2$player2)
COLL08_TDft2 <- as.matrix(COLL08_TDft)
numRows <- nrow(COLL08_TDft2)
numCols <- ncol(COLL08_TDft2)
COLL08_TDft3 <- COLL08_TDft2[c(2:numRows) , c(2:numCols)]
COLL08_TDTable <- graph.adjacency(COLL08_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(COLL08_TDTable, vertex.label = V(COLL08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
COLL08_TD.clusterCoef <- transitivity(COLL08_TDTable, type="global") #cluster coefficient
COLL08_TD.degreeCent <- centralization.degree(COLL08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_TDftn <- as.network.matrix(COLL08_TDft)
COLL08_TD.netDensity <- network.density(COLL08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_TD.entropy <- entropy(COLL08_TDft) #entropy

COLL08_TD.netMx <- cbind(COLL08_TD.netMx, COLL08_TD.clusterCoef, COLL08_TD.degreeCent$centralization,
                         COLL08_TD.netDensity, COLL08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
COLL08_QTg2 <- data.frame(COLL08_QT)
COLL08_QTg2 <- COLL08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL08_QTg2$player1
player2vector <- COLL08_QTg2$player2
COLL08_QTg3 <- COLL08_QTg2
COLL08_QTg3$p1inp2vec <- is.element(COLL08_QTg3$player1, player2vector)
COLL08_QTg3$p2inp1vec <- is.element(COLL08_QTg3$player2, player1vector)

addPlayer1 <- COLL08_QTg3[ which(COLL08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL08_QTg3[ which(COLL08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL08_QTg2 <- rbind(COLL08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
COLL08_QTft <- ftable(COLL08_QTg2$player1, COLL08_QTg2$player2)
COLL08_QTft2 <- as.matrix(COLL08_QTft)
numRows <- nrow(COLL08_QTft2)
numCols <- ncol(COLL08_QTft2)
COLL08_QTft3 <- COLL08_QTft2[c(2:numRows) , c(2:numCols)]
COLL08_QTTable <- graph.adjacency(COLL08_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(COLL08_QTTable, vertex.label = V(COLL08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
COLL08_QT.clusterCoef <- transitivity(COLL08_QTTable, type="global") #cluster coefficient
COLL08_QT.degreeCent <- centralization.degree(COLL08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL08_QTftn <- as.network.matrix(COLL08_QTft)
COLL08_QT.netDensity <- network.density(COLL08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL08_QT.entropy <- entropy(COLL08_QTft) #entropy

COLL08_QT.netMx <- cbind(COLL08_QT.netMx, COLL08_QT.clusterCoef, COLL08_QT.degreeCent$centralization,
                         COLL08_QT.netDensity, COLL08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL08_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "Goal_F"
ESS08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
ESS08_Gg2 <- data.frame(ESS08_G)
ESS08_Gg2 <- ESS08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_Gg2$player1
player2vector <- ESS08_Gg2$player2
ESS08_Gg3 <- ESS08_Gg2
ESS08_Gg3$p1inp2vec <- is.element(ESS08_Gg3$player1, player2vector)
ESS08_Gg3$p2inp1vec <- is.element(ESS08_Gg3$player2, player1vector)

addPlayer1 <- ESS08_Gg3[ which(ESS08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_Gg3[ which(ESS08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_Gg2 <- rbind(ESS08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
ESS08_Gft <- ftable(ESS08_Gg2$player1, ESS08_Gg2$player2)
ESS08_Gft2 <- as.matrix(ESS08_Gft)
numRows <- nrow(ESS08_Gft2)
numCols <- ncol(ESS08_Gft2)
ESS08_Gft3 <- ESS08_Gft2[c(2:numRows) , c(2:numCols)]
ESS08_GTable <- graph.adjacency(ESS08_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(ESS08_GTable, vertex.label = V(ESS08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
ESS08_G.clusterCoef <- transitivity(ESS08_GTable, type="global") #cluster coefficient
ESS08_G.degreeCent <- centralization.degree(ESS08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_Gftn <- as.network.matrix(ESS08_Gft)
ESS08_G.netDensity <- network.density(ESS08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_G.entropy <- entropy(ESS08_Gft) #entropy

ESS08_G.netMx <- cbind(ESS08_G.netMx, ESS08_G.clusterCoef, ESS08_G.degreeCent$centralization,
                       ESS08_G.netDensity, ESS08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "Behind_F"
ESS08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
ESS08_Bg2 <- data.frame(ESS08_B)
ESS08_Bg2 <- ESS08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_Bg2$player1
player2vector <- ESS08_Bg2$player2
ESS08_Bg3 <- ESS08_Bg2
ESS08_Bg3$p1inp2vec <- is.element(ESS08_Bg3$player1, player2vector)
ESS08_Bg3$p2inp1vec <- is.element(ESS08_Bg3$player2, player1vector)

addPlayer1 <- ESS08_Bg3[ which(ESS08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_Bg3[ which(ESS08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_Bg2 <- rbind(ESS08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
ESS08_Bft <- ftable(ESS08_Bg2$player1, ESS08_Bg2$player2)
ESS08_Bft2 <- as.matrix(ESS08_Bft)
numRows <- nrow(ESS08_Bft2)
numCols <- ncol(ESS08_Bft2)
ESS08_Bft3 <- ESS08_Bft2[c(2:numRows) , c(2:numCols)]
ESS08_BTable <- graph.adjacency(ESS08_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(ESS08_BTable, vertex.label = V(ESS08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
ESS08_B.clusterCoef <- transitivity(ESS08_BTable, type="global") #cluster coefficient
ESS08_B.degreeCent <- centralization.degree(ESS08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_Bftn <- as.network.matrix(ESS08_Bft)
ESS08_B.netDensity <- network.density(ESS08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_B.entropy <- entropy(ESS08_Bft) #entropy

ESS08_B.netMx <- cbind(ESS08_B.netMx, ESS08_B.clusterCoef, ESS08_B.degreeCent$centralization,
                       ESS08_B.netDensity, ESS08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
ESS08_SFg2 <- data.frame(ESS08_SF)
ESS08_SFg2 <- ESS08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_SFg2$player1
player2vector <- ESS08_SFg2$player2
ESS08_SFg3 <- ESS08_SFg2
ESS08_SFg3$p1inp2vec <- is.element(ESS08_SFg3$player1, player2vector)
ESS08_SFg3$p2inp1vec <- is.element(ESS08_SFg3$player2, player1vector)

addPlayer1 <- ESS08_SFg3[ which(ESS08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_SFg3[ which(ESS08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_SFg2 <- rbind(ESS08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
ESS08_SFft <- ftable(ESS08_SFg2$player1, ESS08_SFg2$player2)
ESS08_SFft2 <- as.matrix(ESS08_SFft)
numRows <- nrow(ESS08_SFft2)
numCols <- ncol(ESS08_SFft2)
ESS08_SFft3 <- ESS08_SFft2[c(2:numRows) , c(2:numCols)]
ESS08_SFTable <- graph.adjacency(ESS08_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(ESS08_SFTable, vertex.label = V(ESS08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
ESS08_SF.clusterCoef <- transitivity(ESS08_SFTable, type="global") #cluster coefficient
ESS08_SF.degreeCent <- centralization.degree(ESS08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_SFftn <- as.network.matrix(ESS08_SFft)
ESS08_SF.netDensity <- network.density(ESS08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_SF.entropy <- entropy(ESS08_SFft) #entropy

ESS08_SF.netMx <- cbind(ESS08_SF.netMx, ESS08_SF.clusterCoef, ESS08_SF.degreeCent$centralization,
                        ESS08_SF.netDensity, ESS08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
ESS08_TFg2 <- data.frame(ESS08_TF)
ESS08_TFg2 <- ESS08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_TFg2$player1
player2vector <- ESS08_TFg2$player2
ESS08_TFg3 <- ESS08_TFg2
ESS08_TFg3$p1inp2vec <- is.element(ESS08_TFg3$player1, player2vector)
ESS08_TFg3$p2inp1vec <- is.element(ESS08_TFg3$player2, player1vector)

addPlayer1 <- ESS08_TFg3[ which(ESS08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_TFg3[ which(ESS08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_TFg2 <- rbind(ESS08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
ESS08_TFft <- ftable(ESS08_TFg2$player1, ESS08_TFg2$player2)
ESS08_TFft2 <- as.matrix(ESS08_TFft)
numRows <- nrow(ESS08_TFft2)
numCols <- ncol(ESS08_TFft2)
ESS08_TFft3 <- ESS08_TFft2[c(2:numRows) , c(2:numCols)]
ESS08_TFTable <- graph.adjacency(ESS08_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(ESS08_TFTable, vertex.label = V(ESS08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
ESS08_TF.clusterCoef <- transitivity(ESS08_TFTable, type="global") #cluster coefficient
ESS08_TF.degreeCent <- centralization.degree(ESS08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_TFftn <- as.network.matrix(ESS08_TFft)
ESS08_TF.netDensity <- network.density(ESS08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_TF.entropy <- entropy(ESS08_TFft) #entropy

ESS08_TF.netMx <- cbind(ESS08_TF.netMx, ESS08_TF.clusterCoef, ESS08_TF.degreeCent$centralization,
                        ESS08_TF.netDensity, ESS08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
ESS08_SAMg2 <- data.frame(ESS08_SAM)
ESS08_SAMg2 <- ESS08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_SAMg2$player1
player2vector <- ESS08_SAMg2$player2
ESS08_SAMg3 <- ESS08_SAMg2
ESS08_SAMg3$p1inp2vec <- is.element(ESS08_SAMg3$player1, player2vector)
ESS08_SAMg3$p2inp1vec <- is.element(ESS08_SAMg3$player2, player1vector)

addPlayer1 <- ESS08_SAMg3[ which(ESS08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_SAMg3[ which(ESS08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_SAMg2 <- rbind(ESS08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
ESS08_SAMft <- ftable(ESS08_SAMg2$player1, ESS08_SAMg2$player2)
ESS08_SAMft2 <- as.matrix(ESS08_SAMft)
numRows <- nrow(ESS08_SAMft2)
numCols <- ncol(ESS08_SAMft2)
ESS08_SAMft3 <- ESS08_SAMft2[c(2:numRows) , c(2:numCols)]
ESS08_SAMTable <- graph.adjacency(ESS08_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(ESS08_SAMTable, vertex.label = V(ESS08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
ESS08_SAM.clusterCoef <- transitivity(ESS08_SAMTable, type="global") #cluster coefficient
ESS08_SAM.degreeCent <- centralization.degree(ESS08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_SAMftn <- as.network.matrix(ESS08_SAMft)
ESS08_SAM.netDensity <- network.density(ESS08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_SAM.entropy <- entropy(ESS08_SAMft) #entropy

ESS08_SAM.netMx <- cbind(ESS08_SAM.netMx, ESS08_SAM.clusterCoef, ESS08_SAM.degreeCent$centralization,
                         ESS08_SAM.netDensity, ESS08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
ESS08_TAMg2 <- data.frame(ESS08_TAM)
ESS08_TAMg2 <- ESS08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_TAMg2$player1
player2vector <- ESS08_TAMg2$player2
ESS08_TAMg3 <- ESS08_TAMg2
ESS08_TAMg3$p1inp2vec <- is.element(ESS08_TAMg3$player1, player2vector)
ESS08_TAMg3$p2inp1vec <- is.element(ESS08_TAMg3$player2, player1vector)

addPlayer1 <- ESS08_TAMg3[ which(ESS08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_TAMg3[ which(ESS08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_TAMg2 <- rbind(ESS08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
ESS08_TAMft <- ftable(ESS08_TAMg2$player1, ESS08_TAMg2$player2)
ESS08_TAMft2 <- as.matrix(ESS08_TAMft)
numRows <- nrow(ESS08_TAMft2)
numCols <- ncol(ESS08_TAMft2)
ESS08_TAMft3 <- ESS08_TAMft2[c(2:numRows) , c(2:numCols)]
ESS08_TAMTable <- graph.adjacency(ESS08_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(ESS08_TAMTable, vertex.label = V(ESS08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
ESS08_TAM.clusterCoef <- transitivity(ESS08_TAMTable, type="global") #cluster coefficient
ESS08_TAM.degreeCent <- centralization.degree(ESS08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_TAMftn <- as.network.matrix(ESS08_TAMft)
ESS08_TAM.netDensity <- network.density(ESS08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_TAM.entropy <- entropy(ESS08_TAMft) #entropy

ESS08_TAM.netMx <- cbind(ESS08_TAM.netMx, ESS08_TAM.clusterCoef, ESS08_TAM.degreeCent$centralization,
                         ESS08_TAM.netDensity, ESS08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
ESS08_SDMg2 <- data.frame(ESS08_SDM)
ESS08_SDMg2 <- ESS08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_SDMg2$player1
player2vector <- ESS08_SDMg2$player2
ESS08_SDMg3 <- ESS08_SDMg2
ESS08_SDMg3$p1inp2vec <- is.element(ESS08_SDMg3$player1, player2vector)
ESS08_SDMg3$p2inp1vec <- is.element(ESS08_SDMg3$player2, player1vector)

addPlayer1 <- ESS08_SDMg3[ which(ESS08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_SDMg3[ which(ESS08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_SDMg2 <- rbind(ESS08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
ESS08_SDMft <- ftable(ESS08_SDMg2$player1, ESS08_SDMg2$player2)
ESS08_SDMft2 <- as.matrix(ESS08_SDMft)
numRows <- nrow(ESS08_SDMft2)
numCols <- ncol(ESS08_SDMft2)
ESS08_SDMft3 <- ESS08_SDMft2[c(2:numRows) , c(2:numCols)]
ESS08_SDMTable <- graph.adjacency(ESS08_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(ESS08_SDMTable, vertex.label = V(ESS08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
ESS08_SDM.clusterCoef <- transitivity(ESS08_SDMTable, type="global") #cluster coefficient
ESS08_SDM.degreeCent <- centralization.degree(ESS08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_SDMftn <- as.network.matrix(ESS08_SDMft)
ESS08_SDM.netDensity <- network.density(ESS08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_SDM.entropy <- entropy(ESS08_SDMft) #entropy

ESS08_SDM.netMx <- cbind(ESS08_SDM.netMx, ESS08_SDM.clusterCoef, ESS08_SDM.degreeCent$centralization,
                         ESS08_SDM.netDensity, ESS08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
ESS08_TDMg2 <- data.frame(ESS08_TDM)
ESS08_TDMg2 <- ESS08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_TDMg2$player1
player2vector <- ESS08_TDMg2$player2
ESS08_TDMg3 <- ESS08_TDMg2
ESS08_TDMg3$p1inp2vec <- is.element(ESS08_TDMg3$player1, player2vector)
ESS08_TDMg3$p2inp1vec <- is.element(ESS08_TDMg3$player2, player1vector)

addPlayer1 <- ESS08_TDMg3[ which(ESS08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

ESS08_TDMg2 <- rbind(ESS08_TDMg2, addPlayer1)

#ROUND 8, DM Turnover graph using weighted edges
ESS08_TDMft <- ftable(ESS08_TDMg2$player1, ESS08_TDMg2$player2)
ESS08_TDMft2 <- as.matrix(ESS08_TDMft)
numRows <- nrow(ESS08_TDMft2)
numCols <- ncol(ESS08_TDMft2)
ESS08_TDMft3 <- ESS08_TDMft2[c(2:numRows) , c(1:numCols)] #Had to change no of cols when only adding rows
ESS08_TDMTable <- graph.adjacency(ESS08_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(ESS08_TDMTable, vertex.label = V(ESS08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
ESS08_TDM.clusterCoef <- transitivity(ESS08_TDMTable, type="global") #cluster coefficient
ESS08_TDM.degreeCent <- centralization.degree(ESS08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_TDMftn <- as.network.matrix(ESS08_TDMft)
ESS08_TDM.netDensity <- network.density(ESS08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_TDM.entropy <- entropy(ESS08_TDMft) #entropy

ESS08_TDM.netMx <- cbind(ESS08_TDM.netMx, ESS08_TDM.clusterCoef, ESS08_TDM.degreeCent$centralization,
                         ESS08_TDM.netDensity, ESS08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
ESS08_SDg2 <- data.frame(ESS08_SD)
ESS08_SDg2 <- ESS08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_SDg2$player1
player2vector <- ESS08_SDg2$player2
ESS08_SDg3 <- ESS08_SDg2
ESS08_SDg3$p1inp2vec <- is.element(ESS08_SDg3$player1, player2vector)
ESS08_SDg3$p2inp1vec <- is.element(ESS08_SDg3$player2, player1vector)

addPlayer1 <- ESS08_SDg3[ which(ESS08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_SDg3[ which(ESS08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_SDg2 <- rbind(ESS08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
ESS08_SDft <- ftable(ESS08_SDg2$player1, ESS08_SDg2$player2)
ESS08_SDft2 <- as.matrix(ESS08_SDft)
numRows <- nrow(ESS08_SDft2)
numCols <- ncol(ESS08_SDft2)
ESS08_SDft3 <- ESS08_SDft2[c(2:numRows) , c(2:numCols)]
ESS08_SDTable <- graph.adjacency(ESS08_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(ESS08_SDTable, vertex.label = V(ESS08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
ESS08_SD.clusterCoef <- transitivity(ESS08_SDTable, type="global") #cluster coefficient
ESS08_SD.degreeCent <- centralization.degree(ESS08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_SDftn <- as.network.matrix(ESS08_SDft)
ESS08_SD.netDensity <- network.density(ESS08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_SD.entropy <- entropy(ESS08_SDft) #entropy

ESS08_SD.netMx <- cbind(ESS08_SD.netMx, ESS08_SD.clusterCoef, ESS08_SD.degreeCent$centralization,
                        ESS08_SD.netDensity, ESS08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
ESS08_TDg2 <- data.frame(ESS08_TD)
ESS08_TDg2 <- ESS08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_TDg2$player1
player2vector <- ESS08_TDg2$player2
ESS08_TDg3 <- ESS08_TDg2
ESS08_TDg3$p1inp2vec <- is.element(ESS08_TDg3$player1, player2vector)
ESS08_TDg3$p2inp1vec <- is.element(ESS08_TDg3$player2, player1vector)

addPlayer1 <- ESS08_TDg3[ which(ESS08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_TDg3[ which(ESS08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_TDg2 <- rbind(ESS08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
ESS08_TDft <- ftable(ESS08_TDg2$player1, ESS08_TDg2$player2)
ESS08_TDft2 <- as.matrix(ESS08_TDft)
numRows <- nrow(ESS08_TDft2)
numCols <- ncol(ESS08_TDft2)
ESS08_TDft3 <- ESS08_TDft2[c(2:numRows) , c(2:numCols)]
ESS08_TDTable <- graph.adjacency(ESS08_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(ESS08_TDTable, vertex.label = V(ESS08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
ESS08_TD.clusterCoef <- transitivity(ESS08_TDTable, type="global") #cluster coefficient
ESS08_TD.degreeCent <- centralization.degree(ESS08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_TDftn <- as.network.matrix(ESS08_TDft)
ESS08_TD.netDensity <- network.density(ESS08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_TD.entropy <- entropy(ESS08_TDft) #entropy

ESS08_TD.netMx <- cbind(ESS08_TD.netMx, ESS08_TD.clusterCoef, ESS08_TD.degreeCent$centralization,
                        ESS08_TD.netDensity, ESS08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
ESS08_QTg2 <- data.frame(ESS08_QT)
ESS08_QTg2 <- ESS08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS08_QTg2$player1
player2vector <- ESS08_QTg2$player2
ESS08_QTg3 <- ESS08_QTg2
ESS08_QTg3$p1inp2vec <- is.element(ESS08_QTg3$player1, player2vector)
ESS08_QTg3$p2inp1vec <- is.element(ESS08_QTg3$player2, player1vector)

addPlayer1 <- ESS08_QTg3[ which(ESS08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS08_QTg3[ which(ESS08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS08_QTg2 <- rbind(ESS08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
ESS08_QTft <- ftable(ESS08_QTg2$player1, ESS08_QTg2$player2)
ESS08_QTft2 <- as.matrix(ESS08_QTft)
numRows <- nrow(ESS08_QTft2)
numCols <- ncol(ESS08_QTft2)
ESS08_QTft3 <- ESS08_QTft2[c(2:numRows) , c(2:numCols)]
ESS08_QTTable <- graph.adjacency(ESS08_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(ESS08_QTTable, vertex.label = V(ESS08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
ESS08_QT.clusterCoef <- transitivity(ESS08_QTTable, type="global") #cluster coefficient
ESS08_QT.degreeCent <- centralization.degree(ESS08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS08_QTftn <- as.network.matrix(ESS08_QTft)
ESS08_QT.netDensity <- network.density(ESS08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS08_QT.entropy <- entropy(ESS08_QTft) #entropy

ESS08_QT.netMx <- cbind(ESS08_QT.netMx, ESS08_QT.clusterCoef, ESS08_QT.degreeCent$centralization,
                        ESS08_QT.netDensity, ESS08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS08_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************

round = 8
teamName = "FRE"
KIoutcome = "Goal_F"
FRE08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
FRE08_Gg2 <- data.frame(FRE08_G)
FRE08_Gg2 <- FRE08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_Gg2$player1
player2vector <- FRE08_Gg2$player2
FRE08_Gg3 <- FRE08_Gg2
FRE08_Gg3$p1inp2vec <- is.element(FRE08_Gg3$player1, player2vector)
FRE08_Gg3$p2inp1vec <- is.element(FRE08_Gg3$player2, player1vector)

addPlayer1 <- FRE08_Gg3[ which(FRE08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_Gg3[ which(FRE08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_Gg2 <- rbind(FRE08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
FRE08_Gft <- ftable(FRE08_Gg2$player1, FRE08_Gg2$player2)
FRE08_Gft2 <- as.matrix(FRE08_Gft)
numRows <- nrow(FRE08_Gft2)
numCols <- ncol(FRE08_Gft2)
FRE08_Gft3 <- FRE08_Gft2[c(2:numRows) , c(2:numCols)]
FRE08_GTable <- graph.adjacency(FRE08_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(FRE08_GTable, vertex.label = V(FRE08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
FRE08_G.clusterCoef <- transitivity(FRE08_GTable, type="global") #cluster coefficient
FRE08_G.degreeCent <- centralization.degree(FRE08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_Gftn <- as.network.matrix(FRE08_Gft)
FRE08_G.netDensity <- network.density(FRE08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_G.entropy <- entropy(FRE08_Gft) #entropy

FRE08_G.netMx <- cbind(FRE08_G.netMx, FRE08_G.clusterCoef, FRE08_G.degreeCent$centralization,
                       FRE08_G.netDensity, FRE08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "FRE"
KIoutcome = "Behind_F"
FRE08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
FRE08_Bg2 <- data.frame(FRE08_B)
FRE08_Bg2 <- FRE08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_Bg2$player1
player2vector <- FRE08_Bg2$player2
FRE08_Bg3 <- FRE08_Bg2
FRE08_Bg3$p1inp2vec <- is.element(FRE08_Bg3$player1, player2vector)
FRE08_Bg3$p2inp1vec <- is.element(FRE08_Bg3$player2, player1vector)

addPlayer1 <- FRE08_Bg3[ which(FRE08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_Bg3[ which(FRE08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_Bg2 <- rbind(FRE08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
FRE08_Bft <- ftable(FRE08_Bg2$player1, FRE08_Bg2$player2)
FRE08_Bft2 <- as.matrix(FRE08_Bft)
numRows <- nrow(FRE08_Bft2)
numCols <- ncol(FRE08_Bft2)
FRE08_Bft3 <- FRE08_Bft2[c(2:numRows) , c(2:numCols)]
FRE08_BTable <- graph.adjacency(FRE08_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(FRE08_BTable, vertex.label = V(FRE08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
FRE08_B.clusterCoef <- transitivity(FRE08_BTable, type="global") #cluster coefficient
FRE08_B.degreeCent <- centralization.degree(FRE08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_Bftn <- as.network.matrix(FRE08_Bft)
FRE08_B.netDensity <- network.density(FRE08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_B.entropy <- entropy(FRE08_Bft) #entropy

FRE08_B.netMx <- cbind(FRE08_B.netMx, FRE08_B.clusterCoef, FRE08_B.degreeCent$centralization,
                       FRE08_B.netDensity, FRE08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************

round = 8
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
FRE08_SFg2 <- data.frame(FRE08_SF)
FRE08_SFg2 <- FRE08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_SFg2$player1
player2vector <- FRE08_SFg2$player2
FRE08_SFg3 <- FRE08_SFg2
FRE08_SFg3$p1inp2vec <- is.element(FRE08_SFg3$player1, player2vector)
FRE08_SFg3$p2inp1vec <- is.element(FRE08_SFg3$player2, player1vector)

addPlayer1 <- FRE08_SFg3[ which(FRE08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE08_SFg3[ which(FRE08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_SFg2 <- rbind(FRE08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
FRE08_SFft <- ftable(FRE08_SFg2$player1, FRE08_SFg2$player2)
FRE08_SFft2 <- as.matrix(FRE08_SFft)
numRows <- nrow(FRE08_SFft2)
numCols <- ncol(FRE08_SFft2)
FRE08_SFft3 <- FRE08_SFft2[c(2:numRows) , c(2:numCols)]
FRE08_SFTable <- graph.adjacency(FRE08_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(FRE08_SFTable, vertex.label = V(FRE08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
FRE08_SF.clusterCoef <- transitivity(FRE08_SFTable, type="global") #cluster coefficient
FRE08_SF.degreeCent <- centralization.degree(FRE08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_SFftn <- as.network.matrix(FRE08_SFft)
FRE08_SF.netDensity <- network.density(FRE08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_SF.entropy <- entropy(FRE08_SFft) #entropy

FRE08_SF.netMx <- cbind(FRE08_SF.netMx, FRE08_SF.clusterCoef, FRE08_SF.degreeCent$centralization,
                        FRE08_SF.netDensity, FRE08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************
#NA

round = 8
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
FRE08_TFg2 <- data.frame(FRE08_TF)
FRE08_TFg2 <- FRE08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_TFg2$player1
player2vector <- FRE08_TFg2$player2
FRE08_TFg3 <- FRE08_TFg2
FRE08_TFg3$p1inp2vec <- is.element(FRE08_TFg3$player1, player2vector)
FRE08_TFg3$p2inp1vec <- is.element(FRE08_TFg3$player2, player1vector)

addPlayer1 <- FRE08_TFg3[ which(FRE08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_TFg3[ which(FRE08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_TFg2 <- rbind(FRE08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
FRE08_TFft <- ftable(FRE08_TFg2$player1, FRE08_TFg2$player2)
FRE08_TFft2 <- as.matrix(FRE08_TFft)
numRows <- nrow(FRE08_TFft2)
numCols <- ncol(FRE08_TFft2)
FRE08_TFft3 <- FRE08_TFft2[c(2:numRows) , c(2:numCols)]
FRE08_TFTable <- graph.adjacency(FRE08_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(FRE08_TFTable, vertex.label = V(FRE08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
FRE08_TF.clusterCoef <- transitivity(FRE08_TFTable, type="global") #cluster coefficient
FRE08_TF.degreeCent <- centralization.degree(FRE08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_TFftn <- as.network.matrix(FRE08_TFft)
FRE08_TF.netDensity <- network.density(FRE08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_TF.entropy <- entropy(FRE08_TFft) #entropy

FRE08_TF.netMx <- cbind(FRE08_TF.netMx, FRE08_TF.clusterCoef, FRE08_TF.degreeCent$centralization,
                        FRE08_TF.netDensity, FRE08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************

round = 8
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
FRE08_SAMg2 <- data.frame(FRE08_SAM)
FRE08_SAMg2 <- FRE08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_SAMg2$player1
player2vector <- FRE08_SAMg2$player2
FRE08_SAMg3 <- FRE08_SAMg2
FRE08_SAMg3$p1inp2vec <- is.element(FRE08_SAMg3$player1, player2vector)
FRE08_SAMg3$p2inp1vec <- is.element(FRE08_SAMg3$player2, player1vector)

addPlayer1 <- FRE08_SAMg3[ which(FRE08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_SAMg3[ which(FRE08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_SAMg2 <- rbind(FRE08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
FRE08_SAMft <- ftable(FRE08_SAMg2$player1, FRE08_SAMg2$player2)
FRE08_SAMft2 <- as.matrix(FRE08_SAMft)
numRows <- nrow(FRE08_SAMft2)
numCols <- ncol(FRE08_SAMft2)
FRE08_SAMft3 <- FRE08_SAMft2[c(2:numRows) , c(2:numCols)]
FRE08_SAMTable <- graph.adjacency(FRE08_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(FRE08_SAMTable, vertex.label = V(FRE08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
FRE08_SAM.clusterCoef <- transitivity(FRE08_SAMTable, type="global") #cluster coefficient
FRE08_SAM.degreeCent <- centralization.degree(FRE08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_SAMftn <- as.network.matrix(FRE08_SAMft)
FRE08_SAM.netDensity <- network.density(FRE08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_SAM.entropy <- entropy(FRE08_SAMft) #entropy

FRE08_SAM.netMx <- cbind(FRE08_SAM.netMx, FRE08_SAM.clusterCoef, FRE08_SAM.degreeCent$centralization,
                         FRE08_SAM.netDensity, FRE08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
FRE08_TAMg2 <- data.frame(FRE08_TAM)
FRE08_TAMg2 <- FRE08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_TAMg2$player1
player2vector <- FRE08_TAMg2$player2
FRE08_TAMg3 <- FRE08_TAMg2
FRE08_TAMg3$p1inp2vec <- is.element(FRE08_TAMg3$player1, player2vector)
FRE08_TAMg3$p2inp1vec <- is.element(FRE08_TAMg3$player2, player1vector)

addPlayer1 <- FRE08_TAMg3[ which(FRE08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_TAMg3[ which(FRE08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_TAMg2 <- rbind(FRE08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
FRE08_TAMft <- ftable(FRE08_TAMg2$player1, FRE08_TAMg2$player2)
FRE08_TAMft2 <- as.matrix(FRE08_TAMft)
numRows <- nrow(FRE08_TAMft2)
numCols <- ncol(FRE08_TAMft2)
FRE08_TAMft3 <- FRE08_TAMft2[c(2:numRows) , c(2:numCols)]
FRE08_TAMTable <- graph.adjacency(FRE08_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(FRE08_TAMTable, vertex.label = V(FRE08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
FRE08_TAM.clusterCoef <- transitivity(FRE08_TAMTable, type="global") #cluster coefficient
FRE08_TAM.degreeCent <- centralization.degree(FRE08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_TAMftn <- as.network.matrix(FRE08_TAMft)
FRE08_TAM.netDensity <- network.density(FRE08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_TAM.entropy <- entropy(FRE08_TAMft) #entropy

FRE08_TAM.netMx <- cbind(FRE08_TAM.netMx, FRE08_TAM.clusterCoef, FRE08_TAM.degreeCent$centralization,
                         FRE08_TAM.netDensity, FRE08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************
#NA

round = 8
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
FRE08_SDMg2 <- data.frame(FRE08_SDM)
FRE08_SDMg2 <- FRE08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_SDMg2$player1
player2vector <- FRE08_SDMg2$player2
FRE08_SDMg3 <- FRE08_SDMg2
FRE08_SDMg3$p1inp2vec <- is.element(FRE08_SDMg3$player1, player2vector)
FRE08_SDMg3$p2inp1vec <- is.element(FRE08_SDMg3$player2, player1vector)

addPlayer1 <- FRE08_SDMg3[ which(FRE08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_SDMg3[ which(FRE08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_SDMg2 <- rbind(FRE08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
FRE08_SDMft <- ftable(FRE08_SDMg2$player1, FRE08_SDMg2$player2)
FRE08_SDMft2 <- as.matrix(FRE08_SDMft)
numRows <- nrow(FRE08_SDMft2)
numCols <- ncol(FRE08_SDMft2)
FRE08_SDMft3 <- FRE08_SDMft2[c(2:numRows) , c(2:numCols)]
FRE08_SDMTable <- graph.adjacency(FRE08_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(FRE08_SDMTable, vertex.label = V(FRE08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
FRE08_SDM.clusterCoef <- transitivity(FRE08_SDMTable, type="global") #cluster coefficient
FRE08_SDM.degreeCent <- centralization.degree(FRE08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_SDMftn <- as.network.matrix(FRE08_SDMft)
FRE08_SDM.netDensity <- network.density(FRE08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_SDM.entropy <- entropy(FRE08_SDMft) #entropy

FRE08_SDM.netMx <- cbind(FRE08_SDM.netMx, FRE08_SDM.clusterCoef, FRE08_SDM.degreeCent$centralization,
                         FRE08_SDM.netDensity, FRE08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
FRE08_TDMg2 <- data.frame(FRE08_TDM)
FRE08_TDMg2 <- FRE08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_TDMg2$player1
player2vector <- FRE08_TDMg2$player2
FRE08_TDMg3 <- FRE08_TDMg2
FRE08_TDMg3$p1inp2vec <- is.element(FRE08_TDMg3$player1, player2vector)
FRE08_TDMg3$p2inp1vec <- is.element(FRE08_TDMg3$player2, player1vector)

addPlayer1 <- FRE08_TDMg3[ which(FRE08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_TDMg3[ which(FRE08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_TDMg2 <- rbind(FRE08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
FRE08_TDMft <- ftable(FRE08_TDMg2$player1, FRE08_TDMg2$player2)
FRE08_TDMft2 <- as.matrix(FRE08_TDMft)
numRows <- nrow(FRE08_TDMft2)
numCols <- ncol(FRE08_TDMft2)
FRE08_TDMft3 <- FRE08_TDMft2[c(2:numRows) , c(2:numCols)]
FRE08_TDMTable <- graph.adjacency(FRE08_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(FRE08_TDMTable, vertex.label = V(FRE08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
FRE08_TDM.clusterCoef <- transitivity(FRE08_TDMTable, type="global") #cluster coefficient
FRE08_TDM.degreeCent <- centralization.degree(FRE08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_TDMftn <- as.network.matrix(FRE08_TDMft)
FRE08_TDM.netDensity <- network.density(FRE08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_TDM.entropy <- entropy(FRE08_TDMft) #entropy

FRE08_TDM.netMx <- cbind(FRE08_TDM.netMx, FRE08_TDM.clusterCoef, FRE08_TDM.degreeCent$centralization,
                         FRE08_TDM.netDensity, FRE08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
FRE08_SDg2 <- data.frame(FRE08_SD)
FRE08_SDg2 <- FRE08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_SDg2$player1
player2vector <- FRE08_SDg2$player2
FRE08_SDg3 <- FRE08_SDg2
FRE08_SDg3$p1inp2vec <- is.element(FRE08_SDg3$player1, player2vector)
FRE08_SDg3$p2inp1vec <- is.element(FRE08_SDg3$player2, player1vector)

addPlayer1 <- FRE08_SDg3[ which(FRE08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_SDg3[ which(FRE08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_SDg2 <- rbind(FRE08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
FRE08_SDft <- ftable(FRE08_SDg2$player1, FRE08_SDg2$player2)
FRE08_SDft2 <- as.matrix(FRE08_SDft)
numRows <- nrow(FRE08_SDft2)
numCols <- ncol(FRE08_SDft2)
FRE08_SDft3 <- FRE08_SDft2[c(2:numRows) , c(2:numCols)]
FRE08_SDTable <- graph.adjacency(FRE08_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(FRE08_SDTable, vertex.label = V(FRE08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
FRE08_SD.clusterCoef <- transitivity(FRE08_SDTable, type="global") #cluster coefficient
FRE08_SD.degreeCent <- centralization.degree(FRE08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_SDftn <- as.network.matrix(FRE08_SDft)
FRE08_SD.netDensity <- network.density(FRE08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_SD.entropy <- entropy(FRE08_SDft) #entropy

FRE08_SD.netMx <- cbind(FRE08_SD.netMx, FRE08_SD.clusterCoef, FRE08_SD.degreeCent$centralization,
                        FRE08_SD.netDensity, FRE08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************

round = 8
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
FRE08_TDg2 <- data.frame(FRE08_TD)
FRE08_TDg2 <- FRE08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_TDg2$player1
player2vector <- FRE08_TDg2$player2
FRE08_TDg3 <- FRE08_TDg2
FRE08_TDg3$p1inp2vec <- is.element(FRE08_TDg3$player1, player2vector)
FRE08_TDg3$p2inp1vec <- is.element(FRE08_TDg3$player2, player1vector)

addPlayer1 <- FRE08_TDg3[ which(FRE08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_TDg3[ which(FRE08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_TDg2 <- rbind(FRE08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
FRE08_TDft <- ftable(FRE08_TDg2$player1, FRE08_TDg2$player2)
FRE08_TDft2 <- as.matrix(FRE08_TDft)
numRows <- nrow(FRE08_TDft2)
numCols <- ncol(FRE08_TDft2)
FRE08_TDft3 <- FRE08_TDft2[c(2:numRows) , c(2:numCols)]
FRE08_TDTable <- graph.adjacency(FRE08_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(FRE08_TDTable, vertex.label = V(FRE08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
FRE08_TD.clusterCoef <- transitivity(FRE08_TDTable, type="global") #cluster coefficient
FRE08_TD.degreeCent <- centralization.degree(FRE08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_TDftn <- as.network.matrix(FRE08_TDft)
FRE08_TD.netDensity <- network.density(FRE08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_TD.entropy <- entropy(FRE08_TDft) #entropy

FRE08_TD.netMx <- cbind(FRE08_TD.netMx, FRE08_TD.clusterCoef, FRE08_TD.degreeCent$centralization,
                        FRE08_TD.netDensity, FRE08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
FRE08_QTg2 <- data.frame(FRE08_QT)
FRE08_QTg2 <- FRE08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE08_QTg2$player1
player2vector <- FRE08_QTg2$player2
FRE08_QTg3 <- FRE08_QTg2
FRE08_QTg3$p1inp2vec <- is.element(FRE08_QTg3$player1, player2vector)
FRE08_QTg3$p2inp1vec <- is.element(FRE08_QTg3$player2, player1vector)

addPlayer1 <- FRE08_QTg3[ which(FRE08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE08_QTg3[ which(FRE08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE08_QTg2 <- rbind(FRE08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
FRE08_QTft <- ftable(FRE08_QTg2$player1, FRE08_QTg2$player2)
FRE08_QTft2 <- as.matrix(FRE08_QTft)
numRows <- nrow(FRE08_QTft2)
numCols <- ncol(FRE08_QTft2)
FRE08_QTft3 <- FRE08_QTft2[c(2:numRows) , c(2:numCols)]
FRE08_QTTable <- graph.adjacency(FRE08_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(FRE08_QTTable, vertex.label = V(FRE08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
FRE08_QT.clusterCoef <- transitivity(FRE08_QTTable, type="global") #cluster coefficient
FRE08_QT.degreeCent <- centralization.degree(FRE08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE08_QTftn <- as.network.matrix(FRE08_QTft)
FRE08_QT.netDensity <- network.density(FRE08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE08_QT.entropy <- entropy(FRE08_QTft) #entropy

FRE08_QT.netMx <- cbind(FRE08_QT.netMx, FRE08_QT.clusterCoef, FRE08_QT.degreeCent$centralization,
                        FRE08_QT.netDensity, FRE08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE08_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************

round = 8
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
GCFC08_Gg2 <- data.frame(GCFC08_G)
GCFC08_Gg2 <- GCFC08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_Gg2$player1
player2vector <- GCFC08_Gg2$player2
GCFC08_Gg3 <- GCFC08_Gg2
GCFC08_Gg3$p1inp2vec <- is.element(GCFC08_Gg3$player1, player2vector)
GCFC08_Gg3$p2inp1vec <- is.element(GCFC08_Gg3$player2, player1vector)

addPlayer1 <- GCFC08_Gg3[ which(GCFC08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_Gg3[ which(GCFC08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_Gg2 <- rbind(GCFC08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
GCFC08_Gft <- ftable(GCFC08_Gg2$player1, GCFC08_Gg2$player2)
GCFC08_Gft2 <- as.matrix(GCFC08_Gft)
numRows <- nrow(GCFC08_Gft2)
numCols <- ncol(GCFC08_Gft2)
GCFC08_Gft3 <- GCFC08_Gft2[c(2:numRows) , c(2:numCols)]
GCFC08_GTable <- graph.adjacency(GCFC08_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(GCFC08_GTable, vertex.label = V(GCFC08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
GCFC08_G.clusterCoef <- transitivity(GCFC08_GTable, type="global") #cluster coefficient
GCFC08_G.degreeCent <- centralization.degree(GCFC08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_Gftn <- as.network.matrix(GCFC08_Gft)
GCFC08_G.netDensity <- network.density(GCFC08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_G.entropy <- entropy(GCFC08_Gft) #entropy

GCFC08_G.netMx <- cbind(GCFC08_G.netMx, GCFC08_G.clusterCoef, GCFC08_G.degreeCent$centralization,
                        GCFC08_G.netDensity, GCFC08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
GCFC08_Bg2 <- data.frame(GCFC08_B)
GCFC08_Bg2 <- GCFC08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_Bg2$player1
player2vector <- GCFC08_Bg2$player2
GCFC08_Bg3 <- GCFC08_Bg2
GCFC08_Bg3$p1inp2vec <- is.element(GCFC08_Bg3$player1, player2vector)
GCFC08_Bg3$p2inp1vec <- is.element(GCFC08_Bg3$player2, player1vector)

addPlayer1 <- GCFC08_Bg3[ which(GCFC08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_Bg3[ which(GCFC08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_Bg2 <- rbind(GCFC08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
GCFC08_Bft <- ftable(GCFC08_Bg2$player1, GCFC08_Bg2$player2)
GCFC08_Bft2 <- as.matrix(GCFC08_Bft)
numRows <- nrow(GCFC08_Bft2)
numCols <- ncol(GCFC08_Bft2)
GCFC08_Bft3 <- GCFC08_Bft2[c(2:numRows) , c(2:numCols)]
GCFC08_BTable <- graph.adjacency(GCFC08_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(GCFC08_BTable, vertex.label = V(GCFC08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
GCFC08_B.clusterCoef <- transitivity(GCFC08_BTable, type="global") #cluster coefficient
GCFC08_B.degreeCent <- centralization.degree(GCFC08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_Bftn <- as.network.matrix(GCFC08_Bft)
GCFC08_B.netDensity <- network.density(GCFC08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_B.entropy <- entropy(GCFC08_Bft) #entropy

GCFC08_B.netMx <- cbind(GCFC08_B.netMx, GCFC08_B.clusterCoef, GCFC08_B.degreeCent$centralization,
                        GCFC08_B.netDensity, GCFC08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
GCFC08_SFg2 <- data.frame(GCFC08_SF)
GCFC08_SFg2 <- GCFC08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_SFg2$player1
player2vector <- GCFC08_SFg2$player2
GCFC08_SFg3 <- GCFC08_SFg2
GCFC08_SFg3$p1inp2vec <- is.element(GCFC08_SFg3$player1, player2vector)
GCFC08_SFg3$p2inp1vec <- is.element(GCFC08_SFg3$player2, player1vector)

addPlayer1 <- GCFC08_SFg3[ which(GCFC08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC08_SFg2 <- rbind(GCFC08_SFg2, addPlayer1)

#ROUND 8, FWD Stoppage graph using weighted edges
GCFC08_SFft <- ftable(GCFC08_SFg2$player1, GCFC08_SFg2$player2)
GCFC08_SFft2 <- as.matrix(GCFC08_SFft)
numRows <- nrow(GCFC08_SFft2)
numCols <- ncol(GCFC08_SFft2)
GCFC08_SFft3 <- GCFC08_SFft2[c(2:numRows) , c(1:numCols)]
GCFC08_SFTable <- graph.adjacency(GCFC08_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(GCFC08_SFTable, vertex.label = V(GCFC08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
GCFC08_SF.clusterCoef <- transitivity(GCFC08_SFTable, type="global") #cluster coefficient
GCFC08_SF.degreeCent <- centralization.degree(GCFC08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_SFftn <- as.network.matrix(GCFC08_SFft)
GCFC08_SF.netDensity <- network.density(GCFC08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_SF.entropy <- entropy(GCFC08_SFft) #entropy

GCFC08_SF.netMx <- cbind(GCFC08_SF.netMx, GCFC08_SF.clusterCoef, GCFC08_SF.degreeCent$centralization,
                         GCFC08_SF.netDensity, GCFC08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
GCFC08_TFg2 <- data.frame(GCFC08_TF)
GCFC08_TFg2 <- GCFC08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_TFg2$player1
player2vector <- GCFC08_TFg2$player2
GCFC08_TFg3 <- GCFC08_TFg2
GCFC08_TFg3$p1inp2vec <- is.element(GCFC08_TFg3$player1, player2vector)
GCFC08_TFg3$p2inp1vec <- is.element(GCFC08_TFg3$player2, player1vector)

addPlayer1 <- GCFC08_TFg3[ which(GCFC08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_TFg3[ which(GCFC08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_TFg2 <- rbind(GCFC08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
GCFC08_TFft <- ftable(GCFC08_TFg2$player1, GCFC08_TFg2$player2)
GCFC08_TFft2 <- as.matrix(GCFC08_TFft)
numRows <- nrow(GCFC08_TFft2)
numCols <- ncol(GCFC08_TFft2)
GCFC08_TFft3 <- GCFC08_TFft2[c(2:numRows) , c(2:numCols)]
GCFC08_TFTable <- graph.adjacency(GCFC08_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(GCFC08_TFTable, vertex.label = V(GCFC08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
GCFC08_TF.clusterCoef <- transitivity(GCFC08_TFTable, type="global") #cluster coefficient
GCFC08_TF.degreeCent <- centralization.degree(GCFC08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_TFftn <- as.network.matrix(GCFC08_TFft)
GCFC08_TF.netDensity <- network.density(GCFC08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_TF.entropy <- entropy(GCFC08_TFft) #entropy

GCFC08_TF.netMx <- cbind(GCFC08_TF.netMx, GCFC08_TF.clusterCoef, GCFC08_TF.degreeCent$centralization,
                         GCFC08_TF.netDensity, GCFC08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************

round = 8
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
GCFC08_SAMg2 <- data.frame(GCFC08_SAM)
GCFC08_SAMg2 <- GCFC08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_SAMg2$player1
player2vector <- GCFC08_SAMg2$player2
GCFC08_SAMg3 <- GCFC08_SAMg2
GCFC08_SAMg3$p1inp2vec <- is.element(GCFC08_SAMg3$player1, player2vector)
GCFC08_SAMg3$p2inp1vec <- is.element(GCFC08_SAMg3$player2, player1vector)

addPlayer1 <- GCFC08_SAMg3[ which(GCFC08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_SAMg3[ which(GCFC08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_SAMg2 <- rbind(GCFC08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
GCFC08_SAMft <- ftable(GCFC08_SAMg2$player1, GCFC08_SAMg2$player2)
GCFC08_SAMft2 <- as.matrix(GCFC08_SAMft)
numRows <- nrow(GCFC08_SAMft2)
numCols <- ncol(GCFC08_SAMft2)
GCFC08_SAMft3 <- GCFC08_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC08_SAMTable <- graph.adjacency(GCFC08_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(GCFC08_SAMTable, vertex.label = V(GCFC08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
GCFC08_SAM.clusterCoef <- transitivity(GCFC08_SAMTable, type="global") #cluster coefficient
GCFC08_SAM.degreeCent <- centralization.degree(GCFC08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_SAMftn <- as.network.matrix(GCFC08_SAMft)
GCFC08_SAM.netDensity <- network.density(GCFC08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_SAM.entropy <- entropy(GCFC08_SAMft) #entropy

GCFC08_SAM.netMx <- cbind(GCFC08_SAM.netMx, GCFC08_SAM.clusterCoef, GCFC08_SAM.degreeCent$centralization,
                          GCFC08_SAM.netDensity, GCFC08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
GCFC08_TAMg2 <- data.frame(GCFC08_TAM)
GCFC08_TAMg2 <- GCFC08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_TAMg2$player1
player2vector <- GCFC08_TAMg2$player2
GCFC08_TAMg3 <- GCFC08_TAMg2
GCFC08_TAMg3$p1inp2vec <- is.element(GCFC08_TAMg3$player1, player2vector)
GCFC08_TAMg3$p2inp1vec <- is.element(GCFC08_TAMg3$player2, player1vector)

addPlayer1 <- GCFC08_TAMg3[ which(GCFC08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_TAMg3[ which(GCFC08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_TAMg2 <- rbind(GCFC08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
GCFC08_TAMft <- ftable(GCFC08_TAMg2$player1, GCFC08_TAMg2$player2)
GCFC08_TAMft2 <- as.matrix(GCFC08_TAMft)
numRows <- nrow(GCFC08_TAMft2)
numCols <- ncol(GCFC08_TAMft2)
GCFC08_TAMft3 <- GCFC08_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC08_TAMTable <- graph.adjacency(GCFC08_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(GCFC08_TAMTable, vertex.label = V(GCFC08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
GCFC08_TAM.clusterCoef <- transitivity(GCFC08_TAMTable, type="global") #cluster coefficient
GCFC08_TAM.degreeCent <- centralization.degree(GCFC08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_TAMftn <- as.network.matrix(GCFC08_TAMft)
GCFC08_TAM.netDensity <- network.density(GCFC08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_TAM.entropy <- entropy(GCFC08_TAMft) #entropy

GCFC08_TAM.netMx <- cbind(GCFC08_TAM.netMx, GCFC08_TAM.clusterCoef, GCFC08_TAM.degreeCent$centralization,
                          GCFC08_TAM.netDensity, GCFC08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
GCFC08_SDMg2 <- data.frame(GCFC08_SDM)
GCFC08_SDMg2 <- GCFC08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_SDMg2$player1
player2vector <- GCFC08_SDMg2$player2
GCFC08_SDMg3 <- GCFC08_SDMg2
GCFC08_SDMg3$p1inp2vec <- is.element(GCFC08_SDMg3$player1, player2vector)
GCFC08_SDMg3$p2inp1vec <- is.element(GCFC08_SDMg3$player2, player1vector)

addPlayer1 <- GCFC08_SDMg3[ which(GCFC08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_SDMg3[ which(GCFC08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_SDMg2 <- rbind(GCFC08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
GCFC08_SDMft <- ftable(GCFC08_SDMg2$player1, GCFC08_SDMg2$player2)
GCFC08_SDMft2 <- as.matrix(GCFC08_SDMft)
numRows <- nrow(GCFC08_SDMft2)
numCols <- ncol(GCFC08_SDMft2)
GCFC08_SDMft3 <- GCFC08_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC08_SDMTable <- graph.adjacency(GCFC08_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(GCFC08_SDMTable, vertex.label = V(GCFC08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
GCFC08_SDM.clusterCoef <- transitivity(GCFC08_SDMTable, type="global") #cluster coefficient
GCFC08_SDM.degreeCent <- centralization.degree(GCFC08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_SDMftn <- as.network.matrix(GCFC08_SDMft)
GCFC08_SDM.netDensity <- network.density(GCFC08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_SDM.entropy <- entropy(GCFC08_SDMft) #entropy

GCFC08_SDM.netMx <- cbind(GCFC08_SDM.netMx, GCFC08_SDM.clusterCoef, GCFC08_SDM.degreeCent$centralization,
                          GCFC08_SDM.netDensity, GCFC08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
GCFC08_TDMg2 <- data.frame(GCFC08_TDM)
GCFC08_TDMg2 <- GCFC08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_TDMg2$player1
player2vector <- GCFC08_TDMg2$player2
GCFC08_TDMg3 <- GCFC08_TDMg2
GCFC08_TDMg3$p1inp2vec <- is.element(GCFC08_TDMg3$player1, player2vector)
GCFC08_TDMg3$p2inp1vec <- is.element(GCFC08_TDMg3$player2, player1vector)

addPlayer1 <- GCFC08_TDMg3[ which(GCFC08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_TDMg3[ which(GCFC08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_TDMg2 <- rbind(GCFC08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
GCFC08_TDMft <- ftable(GCFC08_TDMg2$player1, GCFC08_TDMg2$player2)
GCFC08_TDMft2 <- as.matrix(GCFC08_TDMft)
numRows <- nrow(GCFC08_TDMft2)
numCols <- ncol(GCFC08_TDMft2)
GCFC08_TDMft3 <- GCFC08_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC08_TDMTable <- graph.adjacency(GCFC08_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(GCFC08_TDMTable, vertex.label = V(GCFC08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
GCFC08_TDM.clusterCoef <- transitivity(GCFC08_TDMTable, type="global") #cluster coefficient
GCFC08_TDM.degreeCent <- centralization.degree(GCFC08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_TDMftn <- as.network.matrix(GCFC08_TDMft)
GCFC08_TDM.netDensity <- network.density(GCFC08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_TDM.entropy <- entropy(GCFC08_TDMft) #entropy

GCFC08_TDM.netMx <- cbind(GCFC08_TDM.netMx, GCFC08_TDM.clusterCoef, GCFC08_TDM.degreeCent$centralization,
                          GCFC08_TDM.netDensity, GCFC08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
GCFC08_SDg2 <- data.frame(GCFC08_SD)
GCFC08_SDg2 <- GCFC08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_SDg2$player1
player2vector <- GCFC08_SDg2$player2
GCFC08_SDg3 <- GCFC08_SDg2
GCFC08_SDg3$p1inp2vec <- is.element(GCFC08_SDg3$player1, player2vector)
GCFC08_SDg3$p2inp1vec <- is.element(GCFC08_SDg3$player2, player1vector)

addPlayer1 <- GCFC08_SDg3[ which(GCFC08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_SDg3[ which(GCFC08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_SDg2 <- rbind(GCFC08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
GCFC08_SDft <- ftable(GCFC08_SDg2$player1, GCFC08_SDg2$player2)
GCFC08_SDft2 <- as.matrix(GCFC08_SDft)
numRows <- nrow(GCFC08_SDft2)
numCols <- ncol(GCFC08_SDft2)
GCFC08_SDft3 <- GCFC08_SDft2[c(2:numRows) , c(2:numCols)]
GCFC08_SDTable <- graph.adjacency(GCFC08_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(GCFC08_SDTable, vertex.label = V(GCFC08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
GCFC08_SD.clusterCoef <- transitivity(GCFC08_SDTable, type="global") #cluster coefficient
GCFC08_SD.degreeCent <- centralization.degree(GCFC08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_SDftn <- as.network.matrix(GCFC08_SDft)
GCFC08_SD.netDensity <- network.density(GCFC08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_SD.entropy <- entropy(GCFC08_SDft) #entropy

GCFC08_SD.netMx <- cbind(GCFC08_SD.netMx, GCFC08_SD.clusterCoef, GCFC08_SD.degreeCent$centralization,
                         GCFC08_SD.netDensity, GCFC08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
GCFC08_TDg2 <- data.frame(GCFC08_TD)
GCFC08_TDg2 <- GCFC08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_TDg2$player1
player2vector <- GCFC08_TDg2$player2
GCFC08_TDg3 <- GCFC08_TDg2
GCFC08_TDg3$p1inp2vec <- is.element(GCFC08_TDg3$player1, player2vector)
GCFC08_TDg3$p2inp1vec <- is.element(GCFC08_TDg3$player2, player1vector)

addPlayer1 <- GCFC08_TDg3[ which(GCFC08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_TDg3[ which(GCFC08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_TDg2 <- rbind(GCFC08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
GCFC08_TDft <- ftable(GCFC08_TDg2$player1, GCFC08_TDg2$player2)
GCFC08_TDft2 <- as.matrix(GCFC08_TDft)
numRows <- nrow(GCFC08_TDft2)
numCols <- ncol(GCFC08_TDft2)
GCFC08_TDft3 <- GCFC08_TDft2[c(2:numRows) , c(2:numCols)]
GCFC08_TDTable <- graph.adjacency(GCFC08_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(GCFC08_TDTable, vertex.label = V(GCFC08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
GCFC08_TD.clusterCoef <- transitivity(GCFC08_TDTable, type="global") #cluster coefficient
GCFC08_TD.degreeCent <- centralization.degree(GCFC08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_TDftn <- as.network.matrix(GCFC08_TDft)
GCFC08_TD.netDensity <- network.density(GCFC08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_TD.entropy <- entropy(GCFC08_TDft) #entropy

GCFC08_TD.netMx <- cbind(GCFC08_TD.netMx, GCFC08_TD.clusterCoef, GCFC08_TD.degreeCent$centralization,
                         GCFC08_TD.netDensity, GCFC08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
GCFC08_QTg2 <- data.frame(GCFC08_QT)
GCFC08_QTg2 <- GCFC08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC08_QTg2$player1
player2vector <- GCFC08_QTg2$player2
GCFC08_QTg3 <- GCFC08_QTg2
GCFC08_QTg3$p1inp2vec <- is.element(GCFC08_QTg3$player1, player2vector)
GCFC08_QTg3$p2inp1vec <- is.element(GCFC08_QTg3$player2, player1vector)

addPlayer1 <- GCFC08_QTg3[ which(GCFC08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC08_QTg3[ which(GCFC08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC08_QTg2 <- rbind(GCFC08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
GCFC08_QTft <- ftable(GCFC08_QTg2$player1, GCFC08_QTg2$player2)
GCFC08_QTft2 <- as.matrix(GCFC08_QTft)
numRows <- nrow(GCFC08_QTft2)
numCols <- ncol(GCFC08_QTft2)
GCFC08_QTft3 <- GCFC08_QTft2[c(2:numRows) , c(2:numCols)]
GCFC08_QTTable <- graph.adjacency(GCFC08_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(GCFC08_QTTable, vertex.label = V(GCFC08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
GCFC08_QT.clusterCoef <- transitivity(GCFC08_QTTable, type="global") #cluster coefficient
GCFC08_QT.degreeCent <- centralization.degree(GCFC08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC08_QTftn <- as.network.matrix(GCFC08_QTft)
GCFC08_QT.netDensity <- network.density(GCFC08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC08_QT.entropy <- entropy(GCFC08_QTft) #entropy

GCFC08_QT.netMx <- cbind(GCFC08_QT.netMx, GCFC08_QT.clusterCoef, GCFC08_QT.degreeCent$centralization,
                         GCFC08_QT.netDensity, GCFC08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC08_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************

round = 8
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
GEEL08_Gg2 <- data.frame(GEEL08_G)
GEEL08_Gg2 <- GEEL08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_Gg2$player1
player2vector <- GEEL08_Gg2$player2
GEEL08_Gg3 <- GEEL08_Gg2
GEEL08_Gg3$p1inp2vec <- is.element(GEEL08_Gg3$player1, player2vector)
GEEL08_Gg3$p2inp1vec <- is.element(GEEL08_Gg3$player2, player1vector)

addPlayer1 <- GEEL08_Gg3[ which(GEEL08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_Gg3[ which(GEEL08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_Gg2 <- rbind(GEEL08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
GEEL08_Gft <- ftable(GEEL08_Gg2$player1, GEEL08_Gg2$player2)
GEEL08_Gft2 <- as.matrix(GEEL08_Gft)
numRows <- nrow(GEEL08_Gft2)
numCols <- ncol(GEEL08_Gft2)
GEEL08_Gft3 <- GEEL08_Gft2[c(2:numRows) , c(2:numCols)]
GEEL08_GTable <- graph.adjacency(GEEL08_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(GEEL08_GTable, vertex.label = V(GEEL08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
GEEL08_G.clusterCoef <- transitivity(GEEL08_GTable, type="global") #cluster coefficient
GEEL08_G.degreeCent <- centralization.degree(GEEL08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_Gftn <- as.network.matrix(GEEL08_Gft)
GEEL08_G.netDensity <- network.density(GEEL08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_G.entropy <- entropy(GEEL08_Gft) #entropy

GEEL08_G.netMx <- cbind(GEEL08_G.netMx, GEEL08_G.clusterCoef, GEEL08_G.degreeCent$centralization,
                        GEEL08_G.netDensity, GEEL08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
GEEL08_Bg2 <- data.frame(GEEL08_B)
GEEL08_Bg2 <- GEEL08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_Bg2$player1
player2vector <- GEEL08_Bg2$player2
GEEL08_Bg3 <- GEEL08_Bg2
GEEL08_Bg3$p1inp2vec <- is.element(GEEL08_Bg3$player1, player2vector)
GEEL08_Bg3$p2inp1vec <- is.element(GEEL08_Bg3$player2, player1vector)

addPlayer1 <- GEEL08_Bg3[ which(GEEL08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_Bg3[ which(GEEL08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_Bg2 <- rbind(GEEL08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
GEEL08_Bft <- ftable(GEEL08_Bg2$player1, GEEL08_Bg2$player2)
GEEL08_Bft2 <- as.matrix(GEEL08_Bft)
numRows <- nrow(GEEL08_Bft2)
numCols <- ncol(GEEL08_Bft2)
GEEL08_Bft3 <- GEEL08_Bft2[c(2:numRows) , c(2:numCols)]
GEEL08_BTable <- graph.adjacency(GEEL08_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(GEEL08_BTable, vertex.label = V(GEEL08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
GEEL08_B.clusterCoef <- transitivity(GEEL08_BTable, type="global") #cluster coefficient
GEEL08_B.degreeCent <- centralization.degree(GEEL08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_Bftn <- as.network.matrix(GEEL08_Bft)
GEEL08_B.netDensity <- network.density(GEEL08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_B.entropy <- entropy(GEEL08_Bft) #entropy

GEEL08_B.netMx <- cbind(GEEL08_B.netMx, GEEL08_B.clusterCoef, GEEL08_B.degreeCent$centralization,
                        GEEL08_B.netDensity, GEEL08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
GEEL08_SFg2 <- data.frame(GEEL08_SF)
GEEL08_SFg2 <- GEEL08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_SFg2$player1
player2vector <- GEEL08_SFg2$player2
GEEL08_SFg3 <- GEEL08_SFg2
GEEL08_SFg3$p1inp2vec <- is.element(GEEL08_SFg3$player1, player2vector)
GEEL08_SFg3$p2inp1vec <- is.element(GEEL08_SFg3$player2, player1vector)

addPlayer1 <- GEEL08_SFg3[ which(GEEL08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_SFg3[ which(GEEL08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_SFg2 <- rbind(GEEL08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
GEEL08_SFft <- ftable(GEEL08_SFg2$player1, GEEL08_SFg2$player2)
GEEL08_SFft2 <- as.matrix(GEEL08_SFft)
numRows <- nrow(GEEL08_SFft2)
numCols <- ncol(GEEL08_SFft2)
GEEL08_SFft3 <- GEEL08_SFft2[c(2:numRows) , c(2:numCols)]
GEEL08_SFTable <- graph.adjacency(GEEL08_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(GEEL08_SFTable, vertex.label = V(GEEL08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
GEEL08_SF.clusterCoef <- transitivity(GEEL08_SFTable, type="global") #cluster coefficient
GEEL08_SF.degreeCent <- centralization.degree(GEEL08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_SFftn <- as.network.matrix(GEEL08_SFft)
GEEL08_SF.netDensity <- network.density(GEEL08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_SF.entropy <- entropy(GEEL08_SFft) #entropy

GEEL08_SF.netMx <- cbind(GEEL08_SF.netMx, GEEL08_SF.clusterCoef, GEEL08_SF.degreeCent$centralization,
                         GEEL08_SF.netDensity, GEEL08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
GEEL08_TFg2 <- data.frame(GEEL08_TF)
GEEL08_TFg2 <- GEEL08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_TFg2$player1
player2vector <- GEEL08_TFg2$player2
GEEL08_TFg3 <- GEEL08_TFg2
GEEL08_TFg3$p1inp2vec <- is.element(GEEL08_TFg3$player1, player2vector)
GEEL08_TFg3$p2inp1vec <- is.element(GEEL08_TFg3$player2, player1vector)

addPlayer1 <- GEEL08_TFg3[ which(GEEL08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_TFg3[ which(GEEL08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_TFg2 <- rbind(GEEL08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
GEEL08_TFft <- ftable(GEEL08_TFg2$player1, GEEL08_TFg2$player2)
GEEL08_TFft2 <- as.matrix(GEEL08_TFft)
numRows <- nrow(GEEL08_TFft2)
numCols <- ncol(GEEL08_TFft2)
GEEL08_TFft3 <- GEEL08_TFft2[c(2:numRows) , c(2:numCols)]
GEEL08_TFTable <- graph.adjacency(GEEL08_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(GEEL08_TFTable, vertex.label = V(GEEL08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
GEEL08_TF.clusterCoef <- transitivity(GEEL08_TFTable, type="global") #cluster coefficient
GEEL08_TF.degreeCent <- centralization.degree(GEEL08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_TFftn <- as.network.matrix(GEEL08_TFft)
GEEL08_TF.netDensity <- network.density(GEEL08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_TF.entropy <- entropy(GEEL08_TFft) #entropy

GEEL08_TF.netMx <- cbind(GEEL08_TF.netMx, GEEL08_TF.clusterCoef, GEEL08_TF.degreeCent$centralization,
                         GEEL08_TF.netDensity, GEEL08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************
#NA

round = 8
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
GEEL08_SAMg2 <- data.frame(GEEL08_SAM)
GEEL08_SAMg2 <- GEEL08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_SAMg2$player1
player2vector <- GEEL08_SAMg2$player2
GEEL08_SAMg3 <- GEEL08_SAMg2
GEEL08_SAMg3$p1inp2vec <- is.element(GEEL08_SAMg3$player1, player2vector)
GEEL08_SAMg3$p2inp1vec <- is.element(GEEL08_SAMg3$player2, player1vector)

addPlayer1 <- GEEL08_SAMg3[ which(GEEL08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_SAMg3[ which(GEEL08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_SAMg2 <- rbind(GEEL08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
GEEL08_SAMft <- ftable(GEEL08_SAMg2$player1, GEEL08_SAMg2$player2)
GEEL08_SAMft2 <- as.matrix(GEEL08_SAMft)
numRows <- nrow(GEEL08_SAMft2)
numCols <- ncol(GEEL08_SAMft2)
GEEL08_SAMft3 <- GEEL08_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL08_SAMTable <- graph.adjacency(GEEL08_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(GEEL08_SAMTable, vertex.label = V(GEEL08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
GEEL08_SAM.clusterCoef <- transitivity(GEEL08_SAMTable, type="global") #cluster coefficient
GEEL08_SAM.degreeCent <- centralization.degree(GEEL08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_SAMftn <- as.network.matrix(GEEL08_SAMft)
GEEL08_SAM.netDensity <- network.density(GEEL08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_SAM.entropy <- entropy(GEEL08_SAMft) #entropy

GEEL08_SAM.netMx <- cbind(GEEL08_SAM.netMx, GEEL08_SAM.clusterCoef, GEEL08_SAM.degreeCent$centralization,
                          GEEL08_SAM.netDensity, GEEL08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************
#NA

round = 8
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
GEEL08_TAMg2 <- data.frame(GEEL08_TAM)
GEEL08_TAMg2 <- GEEL08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_TAMg2$player1
player2vector <- GEEL08_TAMg2$player2
GEEL08_TAMg3 <- GEEL08_TAMg2
GEEL08_TAMg3$p1inp2vec <- is.element(GEEL08_TAMg3$player1, player2vector)
GEEL08_TAMg3$p2inp1vec <- is.element(GEEL08_TAMg3$player2, player1vector)

addPlayer1 <- GEEL08_TAMg3[ which(GEEL08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_TAMg3[ which(GEEL08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_TAMg2 <- rbind(GEEL08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
GEEL08_TAMft <- ftable(GEEL08_TAMg2$player1, GEEL08_TAMg2$player2)
GEEL08_TAMft2 <- as.matrix(GEEL08_TAMft)
numRows <- nrow(GEEL08_TAMft2)
numCols <- ncol(GEEL08_TAMft2)
GEEL08_TAMft3 <- GEEL08_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL08_TAMTable <- graph.adjacency(GEEL08_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(GEEL08_TAMTable, vertex.label = V(GEEL08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
GEEL08_TAM.clusterCoef <- transitivity(GEEL08_TAMTable, type="global") #cluster coefficient
GEEL08_TAM.degreeCent <- centralization.degree(GEEL08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_TAMftn <- as.network.matrix(GEEL08_TAMft)
GEEL08_TAM.netDensity <- network.density(GEEL08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_TAM.entropy <- entropy(GEEL08_TAMft) #entropy

GEEL08_TAM.netMx <- cbind(GEEL08_TAM.netMx, GEEL08_TAM.clusterCoef, GEEL08_TAM.degreeCent$centralization,
                          GEEL08_TAM.netDensity, GEEL08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************
#NA

round = 8
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
GEEL08_SDMg2 <- data.frame(GEEL08_SDM)
GEEL08_SDMg2 <- GEEL08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_SDMg2$player1
player2vector <- GEEL08_SDMg2$player2
GEEL08_SDMg3 <- GEEL08_SDMg2
GEEL08_SDMg3$p1inp2vec <- is.element(GEEL08_SDMg3$player1, player2vector)
GEEL08_SDMg3$p2inp1vec <- is.element(GEEL08_SDMg3$player2, player1vector)

addPlayer1 <- GEEL08_SDMg3[ which(GEEL08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_SDMg3[ which(GEEL08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_SDMg2 <- rbind(GEEL08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
GEEL08_SDMft <- ftable(GEEL08_SDMg2$player1, GEEL08_SDMg2$player2)
GEEL08_SDMft2 <- as.matrix(GEEL08_SDMft)
numRows <- nrow(GEEL08_SDMft2)
numCols <- ncol(GEEL08_SDMft2)
GEEL08_SDMft3 <- GEEL08_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL08_SDMTable <- graph.adjacency(GEEL08_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(GEEL08_SDMTable, vertex.label = V(GEEL08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
GEEL08_SDM.clusterCoef <- transitivity(GEEL08_SDMTable, type="global") #cluster coefficient
GEEL08_SDM.degreeCent <- centralization.degree(GEEL08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_SDMftn <- as.network.matrix(GEEL08_SDMft)
GEEL08_SDM.netDensity <- network.density(GEEL08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_SDM.entropy <- entropy(GEEL08_SDMft) #entropy

GEEL08_SDM.netMx <- cbind(GEEL08_SDM.netMx, GEEL08_SDM.clusterCoef, GEEL08_SDM.degreeCent$centralization,
                          GEEL08_SDM.netDensity, GEEL08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
GEEL08_TDMg2 <- data.frame(GEEL08_TDM)
GEEL08_TDMg2 <- GEEL08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_TDMg2$player1
player2vector <- GEEL08_TDMg2$player2
GEEL08_TDMg3 <- GEEL08_TDMg2
GEEL08_TDMg3$p1inp2vec <- is.element(GEEL08_TDMg3$player1, player2vector)
GEEL08_TDMg3$p2inp1vec <- is.element(GEEL08_TDMg3$player2, player1vector)

addPlayer1 <- GEEL08_TDMg3[ which(GEEL08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_TDMg3[ which(GEEL08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_TDMg2 <- rbind(GEEL08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
GEEL08_TDMft <- ftable(GEEL08_TDMg2$player1, GEEL08_TDMg2$player2)
GEEL08_TDMft2 <- as.matrix(GEEL08_TDMft)
numRows <- nrow(GEEL08_TDMft2)
numCols <- ncol(GEEL08_TDMft2)
GEEL08_TDMft3 <- GEEL08_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL08_TDMTable <- graph.adjacency(GEEL08_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(GEEL08_TDMTable, vertex.label = V(GEEL08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
GEEL08_TDM.clusterCoef <- transitivity(GEEL08_TDMTable, type="global") #cluster coefficient
GEEL08_TDM.degreeCent <- centralization.degree(GEEL08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_TDMftn <- as.network.matrix(GEEL08_TDMft)
GEEL08_TDM.netDensity <- network.density(GEEL08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_TDM.entropy <- entropy(GEEL08_TDMft) #entropy

GEEL08_TDM.netMx <- cbind(GEEL08_TDM.netMx, GEEL08_TDM.clusterCoef, GEEL08_TDM.degreeCent$centralization,
                          GEEL08_TDM.netDensity, GEEL08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
GEEL08_SDg2 <- data.frame(GEEL08_SD)
GEEL08_SDg2 <- GEEL08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_SDg2$player1
player2vector <- GEEL08_SDg2$player2
GEEL08_SDg3 <- GEEL08_SDg2
GEEL08_SDg3$p1inp2vec <- is.element(GEEL08_SDg3$player1, player2vector)
GEEL08_SDg3$p2inp1vec <- is.element(GEEL08_SDg3$player2, player1vector)

addPlayer1 <- GEEL08_SDg3[ which(GEEL08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_SDg3[ which(GEEL08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_SDg2 <- rbind(GEEL08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
GEEL08_SDft <- ftable(GEEL08_SDg2$player1, GEEL08_SDg2$player2)
GEEL08_SDft2 <- as.matrix(GEEL08_SDft)
numRows <- nrow(GEEL08_SDft2)
numCols <- ncol(GEEL08_SDft2)
GEEL08_SDft3 <- GEEL08_SDft2[c(2:numRows) , c(2:numCols)]
GEEL08_SDTable <- graph.adjacency(GEEL08_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(GEEL08_SDTable, vertex.label = V(GEEL08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
GEEL08_SD.clusterCoef <- transitivity(GEEL08_SDTable, type="global") #cluster coefficient
GEEL08_SD.degreeCent <- centralization.degree(GEEL08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_SDftn <- as.network.matrix(GEEL08_SDft)
GEEL08_SD.netDensity <- network.density(GEEL08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_SD.entropy <- entropy(GEEL08_SDft) #entropy

GEEL08_SD.netMx <- cbind(GEEL08_SD.netMx, GEEL08_SD.clusterCoef, GEEL08_SD.degreeCent$centralization,
                         GEEL08_SD.netDensity, GEEL08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
GEEL08_TDg2 <- data.frame(GEEL08_TD)
GEEL08_TDg2 <- GEEL08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_TDg2$player1
player2vector <- GEEL08_TDg2$player2
GEEL08_TDg3 <- GEEL08_TDg2
GEEL08_TDg3$p1inp2vec <- is.element(GEEL08_TDg3$player1, player2vector)
GEEL08_TDg3$p2inp1vec <- is.element(GEEL08_TDg3$player2, player1vector)

addPlayer1 <- GEEL08_TDg3[ which(GEEL08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_TDg3[ which(GEEL08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_TDg2 <- rbind(GEEL08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
GEEL08_TDft <- ftable(GEEL08_TDg2$player1, GEEL08_TDg2$player2)
GEEL08_TDft2 <- as.matrix(GEEL08_TDft)
numRows <- nrow(GEEL08_TDft2)
numCols <- ncol(GEEL08_TDft2)
GEEL08_TDft3 <- GEEL08_TDft2[c(2:numRows) , c(2:numCols)]
GEEL08_TDTable <- graph.adjacency(GEEL08_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(GEEL08_TDTable, vertex.label = V(GEEL08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
GEEL08_TD.clusterCoef <- transitivity(GEEL08_TDTable, type="global") #cluster coefficient
GEEL08_TD.degreeCent <- centralization.degree(GEEL08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_TDftn <- as.network.matrix(GEEL08_TDft)
GEEL08_TD.netDensity <- network.density(GEEL08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_TD.entropy <- entropy(GEEL08_TDft) #entropy

GEEL08_TD.netMx <- cbind(GEEL08_TD.netMx, GEEL08_TD.clusterCoef, GEEL08_TD.degreeCent$centralization,
                         GEEL08_TD.netDensity, GEEL08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
GEEL08_QTg2 <- data.frame(GEEL08_QT)
GEEL08_QTg2 <- GEEL08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL08_QTg2$player1
player2vector <- GEEL08_QTg2$player2
GEEL08_QTg3 <- GEEL08_QTg2
GEEL08_QTg3$p1inp2vec <- is.element(GEEL08_QTg3$player1, player2vector)
GEEL08_QTg3$p2inp1vec <- is.element(GEEL08_QTg3$player2, player1vector)

addPlayer1 <- GEEL08_QTg3[ which(GEEL08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL08_QTg3[ which(GEEL08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL08_QTg2 <- rbind(GEEL08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
GEEL08_QTft <- ftable(GEEL08_QTg2$player1, GEEL08_QTg2$player2)
GEEL08_QTft2 <- as.matrix(GEEL08_QTft)
numRows <- nrow(GEEL08_QTft2)
numCols <- ncol(GEEL08_QTft2)
GEEL08_QTft3 <- GEEL08_QTft2[c(2:numRows) , c(2:numCols)]
GEEL08_QTTable <- graph.adjacency(GEEL08_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(GEEL08_QTTable, vertex.label = V(GEEL08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
GEEL08_QT.clusterCoef <- transitivity(GEEL08_QTTable, type="global") #cluster coefficient
GEEL08_QT.degreeCent <- centralization.degree(GEEL08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL08_QTftn <- as.network.matrix(GEEL08_QTft)
GEEL08_QT.netDensity <- network.density(GEEL08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL08_QT.entropy <- entropy(GEEL08_QTft) #entropy

GEEL08_QT.netMx <- cbind(GEEL08_QT.netMx, GEEL08_QT.clusterCoef, GEEL08_QT.degreeCent$centralization,
                         GEEL08_QT.netDensity, GEEL08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL08_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "GWS"
KIoutcome = "Goal_F"
GWS08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
GWS08_Gg2 <- data.frame(GWS08_G)
GWS08_Gg2 <- GWS08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_Gg2$player1
player2vector <- GWS08_Gg2$player2
GWS08_Gg3 <- GWS08_Gg2
GWS08_Gg3$p1inp2vec <- is.element(GWS08_Gg3$player1, player2vector)
GWS08_Gg3$p2inp1vec <- is.element(GWS08_Gg3$player2, player1vector)

addPlayer1 <- GWS08_Gg3[ which(GWS08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_Gg3[ which(GWS08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_Gg2 <- rbind(GWS08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
GWS08_Gft <- ftable(GWS08_Gg2$player1, GWS08_Gg2$player2)
GWS08_Gft2 <- as.matrix(GWS08_Gft)
numRows <- nrow(GWS08_Gft2)
numCols <- ncol(GWS08_Gft2)
GWS08_Gft3 <- GWS08_Gft2[c(1:numRows) , c(1:numCols)]
GWS08_GTable <- graph.adjacency(GWS08_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(GWS08_GTable, vertex.label = V(GWS08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
GWS08_G.clusterCoef <- transitivity(GWS08_GTable, type="global") #cluster coefficient
GWS08_G.degreeCent <- centralization.degree(GWS08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_Gftn <- as.network.matrix(GWS08_Gft)
GWS08_G.netDensity <- network.density(GWS08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_G.entropy <- entropy(GWS08_Gft) #entropy

GWS08_G.netMx <- cbind(GWS08_G.netMx, GWS08_G.clusterCoef, GWS08_G.degreeCent$centralization,
                       GWS08_G.netDensity, GWS08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "GWS"
KIoutcome = "Behind_F"
GWS08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
GWS08_Bg2 <- data.frame(GWS08_B)
GWS08_Bg2 <- GWS08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_Bg2$player1
player2vector <- GWS08_Bg2$player2
GWS08_Bg3 <- GWS08_Bg2
GWS08_Bg3$p1inp2vec <- is.element(GWS08_Bg3$player1, player2vector)
GWS08_Bg3$p2inp1vec <- is.element(GWS08_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS08_Bg3[ which(GWS08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS08_Bg2 <- rbind(GWS08_Bg2, addPlayer2)

#ROUND 8, Behind graph using weighted edges
GWS08_Bft <- ftable(GWS08_Bg2$player1, GWS08_Bg2$player2)
GWS08_Bft2 <- as.matrix(GWS08_Bft)
numRows <- nrow(GWS08_Bft2)
numCols <- ncol(GWS08_Bft2)
GWS08_Bft3 <- GWS08_Bft2[c(1:numRows) , c(2:numCols)]
GWS08_BTable <- graph.adjacency(GWS08_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(GWS08_BTable, vertex.label = V(GWS08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
GWS08_B.clusterCoef <- transitivity(GWS08_BTable, type="global") #cluster coefficient
GWS08_B.degreeCent <- centralization.degree(GWS08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_Bftn <- as.network.matrix(GWS08_Bft)
GWS08_B.netDensity <- network.density(GWS08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_B.entropy <- entropy(GWS08_Bft) #entropy

GWS08_B.netMx <- cbind(GWS08_B.netMx, GWS08_B.clusterCoef, GWS08_B.degreeCent$centralization,
                       GWS08_B.netDensity, GWS08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
GWS08_SFg2 <- data.frame(GWS08_SF)
GWS08_SFg2 <- GWS08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_SFg2$player1
player2vector <- GWS08_SFg2$player2
GWS08_SFg3 <- GWS08_SFg2
GWS08_SFg3$p1inp2vec <- is.element(GWS08_SFg3$player1, player2vector)
GWS08_SFg3$p2inp1vec <- is.element(GWS08_SFg3$player2, player1vector)

addPlayer1 <- GWS08_SFg3[ which(GWS08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_SFg3[ which(GWS08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_SFg2 <- rbind(GWS08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
GWS08_SFft <- ftable(GWS08_SFg2$player1, GWS08_SFg2$player2)
GWS08_SFft2 <- as.matrix(GWS08_SFft)
numRows <- nrow(GWS08_SFft2)
numCols <- ncol(GWS08_SFft2)
GWS08_SFft3 <- GWS08_SFft2[c(2:numRows) , c(2:numCols)]
GWS08_SFTable <- graph.adjacency(GWS08_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(GWS08_SFTable, vertex.label = V(GWS08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
GWS08_SF.clusterCoef <- transitivity(GWS08_SFTable, type="global") #cluster coefficient
GWS08_SF.degreeCent <- centralization.degree(GWS08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_SFftn <- as.network.matrix(GWS08_SFft)
GWS08_SF.netDensity <- network.density(GWS08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_SF.entropy <- entropy(GWS08_SFft) #entropy

GWS08_SF.netMx <- cbind(GWS08_SF.netMx, GWS08_SF.clusterCoef, GWS08_SF.degreeCent$centralization,
                        GWS08_SF.netDensity, GWS08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************
#NA

round = 8
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
GWS08_TFg2 <- data.frame(GWS08_TF)
GWS08_TFg2 <- GWS08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_TFg2$player1
player2vector <- GWS08_TFg2$player2
GWS08_TFg3 <- GWS08_TFg2
GWS08_TFg3$p1inp2vec <- is.element(GWS08_TFg3$player1, player2vector)
GWS08_TFg3$p2inp1vec <- is.element(GWS08_TFg3$player2, player1vector)

addPlayer1 <- GWS08_TFg3[ which(GWS08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_TFg3[ which(GWS08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_TFg2 <- rbind(GWS08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
GWS08_TFft <- ftable(GWS08_TFg2$player1, GWS08_TFg2$player2)
GWS08_TFft2 <- as.matrix(GWS08_TFft)
numRows <- nrow(GWS08_TFft2)
numCols <- ncol(GWS08_TFft2)
GWS08_TFft3 <- GWS08_TFft2[c(2:numRows) , c(2:numCols)]
GWS08_TFTable <- graph.adjacency(GWS08_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(GWS08_TFTable, vertex.label = V(GWS08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
GWS08_TF.clusterCoef <- transitivity(GWS08_TFTable, type="global") #cluster coefficient
GWS08_TF.degreeCent <- centralization.degree(GWS08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_TFftn <- as.network.matrix(GWS08_TFft)
GWS08_TF.netDensity <- network.density(GWS08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_TF.entropy <- entropy(GWS08_TFft) #entropy

GWS08_TF.netMx <- cbind(GWS08_TF.netMx, GWS08_TF.clusterCoef, GWS08_TF.degreeCent$centralization,
                        GWS08_TF.netDensity, GWS08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************

round = 8
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
GWS08_SAMg2 <- data.frame(GWS08_SAM)
GWS08_SAMg2 <- GWS08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_SAMg2$player1
player2vector <- GWS08_SAMg2$player2
GWS08_SAMg3 <- GWS08_SAMg2
GWS08_SAMg3$p1inp2vec <- is.element(GWS08_SAMg3$player1, player2vector)
GWS08_SAMg3$p2inp1vec <- is.element(GWS08_SAMg3$player2, player1vector)

addPlayer1 <- GWS08_SAMg3[ which(GWS08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_SAMg3[ which(GWS08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_SAMg2 <- rbind(GWS08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
GWS08_SAMft <- ftable(GWS08_SAMg2$player1, GWS08_SAMg2$player2)
GWS08_SAMft2 <- as.matrix(GWS08_SAMft)
numRows <- nrow(GWS08_SAMft2)
numCols <- ncol(GWS08_SAMft2)
GWS08_SAMft3 <- GWS08_SAMft2[c(2:numRows) , c(2:numCols)]
GWS08_SAMTable <- graph.adjacency(GWS08_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(GWS08_SAMTable, vertex.label = V(GWS08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
GWS08_SAM.clusterCoef <- transitivity(GWS08_SAMTable, type="global") #cluster coefficient
GWS08_SAM.degreeCent <- centralization.degree(GWS08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_SAMftn <- as.network.matrix(GWS08_SAMft)
GWS08_SAM.netDensity <- network.density(GWS08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_SAM.entropy <- entropy(GWS08_SAMft) #entropy

GWS08_SAM.netMx <- cbind(GWS08_SAM.netMx, GWS08_SAM.clusterCoef, GWS08_SAM.degreeCent$centralization,
                         GWS08_SAM.netDensity, GWS08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
GWS08_TAMg2 <- data.frame(GWS08_TAM)
GWS08_TAMg2 <- GWS08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_TAMg2$player1
player2vector <- GWS08_TAMg2$player2
GWS08_TAMg3 <- GWS08_TAMg2
GWS08_TAMg3$p1inp2vec <- is.element(GWS08_TAMg3$player1, player2vector)
GWS08_TAMg3$p2inp1vec <- is.element(GWS08_TAMg3$player2, player1vector)

addPlayer1 <- GWS08_TAMg3[ which(GWS08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_TAMg3[ which(GWS08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_TAMg2 <- rbind(GWS08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
GWS08_TAMft <- ftable(GWS08_TAMg2$player1, GWS08_TAMg2$player2)
GWS08_TAMft2 <- as.matrix(GWS08_TAMft)
numRows <- nrow(GWS08_TAMft2)
numCols <- ncol(GWS08_TAMft2)
GWS08_TAMft3 <- GWS08_TAMft2[c(2:numRows) , c(2:numCols)]
GWS08_TAMTable <- graph.adjacency(GWS08_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(GWS08_TAMTable, vertex.label = V(GWS08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
GWS08_TAM.clusterCoef <- transitivity(GWS08_TAMTable, type="global") #cluster coefficient
GWS08_TAM.degreeCent <- centralization.degree(GWS08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_TAMftn <- as.network.matrix(GWS08_TAMft)
GWS08_TAM.netDensity <- network.density(GWS08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_TAM.entropy <- entropy(GWS08_TAMft) #entropy

GWS08_TAM.netMx <- cbind(GWS08_TAM.netMx, GWS08_TAM.clusterCoef, GWS08_TAM.degreeCent$centralization,
                         GWS08_TAM.netDensity, GWS08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
GWS08_SDMg2 <- data.frame(GWS08_SDM)
GWS08_SDMg2 <- GWS08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_SDMg2$player1
player2vector <- GWS08_SDMg2$player2
GWS08_SDMg3 <- GWS08_SDMg2
GWS08_SDMg3$p1inp2vec <- is.element(GWS08_SDMg3$player1, player2vector)
GWS08_SDMg3$p2inp1vec <- is.element(GWS08_SDMg3$player2, player1vector)

addPlayer1 <- GWS08_SDMg3[ which(GWS08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_SDMg3[ which(GWS08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_SDMg2 <- rbind(GWS08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
GWS08_SDMft <- ftable(GWS08_SDMg2$player1, GWS08_SDMg2$player2)
GWS08_SDMft2 <- as.matrix(GWS08_SDMft)
numRows <- nrow(GWS08_SDMft2)
numCols <- ncol(GWS08_SDMft2)
GWS08_SDMft3 <- GWS08_SDMft2[c(2:numRows) , c(2:numCols)]
GWS08_SDMTable <- graph.adjacency(GWS08_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(GWS08_SDMTable, vertex.label = V(GWS08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
GWS08_SDM.clusterCoef <- transitivity(GWS08_SDMTable, type="global") #cluster coefficient
GWS08_SDM.degreeCent <- centralization.degree(GWS08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_SDMftn <- as.network.matrix(GWS08_SDMft)
GWS08_SDM.netDensity <- network.density(GWS08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_SDM.entropy <- entropy(GWS08_SDMft) #entropy

GWS08_SDM.netMx <- cbind(GWS08_SDM.netMx, GWS08_SDM.clusterCoef, GWS08_SDM.degreeCent$centralization,
                         GWS08_SDM.netDensity, GWS08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
GWS08_TDMg2 <- data.frame(GWS08_TDM)
GWS08_TDMg2 <- GWS08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_TDMg2$player1
player2vector <- GWS08_TDMg2$player2
GWS08_TDMg3 <- GWS08_TDMg2
GWS08_TDMg3$p1inp2vec <- is.element(GWS08_TDMg3$player1, player2vector)
GWS08_TDMg3$p2inp1vec <- is.element(GWS08_TDMg3$player2, player1vector)

addPlayer1 <- GWS08_TDMg3[ which(GWS08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_TDMg3[ which(GWS08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_TDMg2 <- rbind(GWS08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
GWS08_TDMft <- ftable(GWS08_TDMg2$player1, GWS08_TDMg2$player2)
GWS08_TDMft2 <- as.matrix(GWS08_TDMft)
numRows <- nrow(GWS08_TDMft2)
numCols <- ncol(GWS08_TDMft2)
GWS08_TDMft3 <- GWS08_TDMft2[c(2:numRows) , c(2:numCols)]
GWS08_TDMTable <- graph.adjacency(GWS08_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(GWS08_TDMTable, vertex.label = V(GWS08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
GWS08_TDM.clusterCoef <- transitivity(GWS08_TDMTable, type="global") #cluster coefficient
GWS08_TDM.degreeCent <- centralization.degree(GWS08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_TDMftn <- as.network.matrix(GWS08_TDMft)
GWS08_TDM.netDensity <- network.density(GWS08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_TDM.entropy <- entropy(GWS08_TDMft) #entropy

GWS08_TDM.netMx <- cbind(GWS08_TDM.netMx, GWS08_TDM.clusterCoef, GWS08_TDM.degreeCent$centralization,
                         GWS08_TDM.netDensity, GWS08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
GWS08_SDg2 <- data.frame(GWS08_SD)
GWS08_SDg2 <- GWS08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_SDg2$player1
player2vector <- GWS08_SDg2$player2
GWS08_SDg3 <- GWS08_SDg2
GWS08_SDg3$p1inp2vec <- is.element(GWS08_SDg3$player1, player2vector)
GWS08_SDg3$p2inp1vec <- is.element(GWS08_SDg3$player2, player1vector)

addPlayer1 <- GWS08_SDg3[ which(GWS08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_SDg3[ which(GWS08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_SDg2 <- rbind(GWS08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
GWS08_SDft <- ftable(GWS08_SDg2$player1, GWS08_SDg2$player2)
GWS08_SDft2 <- as.matrix(GWS08_SDft)
numRows <- nrow(GWS08_SDft2)
numCols <- ncol(GWS08_SDft2)
GWS08_SDft3 <- GWS08_SDft2[c(2:numRows) , c(2:numCols)]
GWS08_SDTable <- graph.adjacency(GWS08_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(GWS08_SDTable, vertex.label = V(GWS08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
GWS08_SD.clusterCoef <- transitivity(GWS08_SDTable, type="global") #cluster coefficient
GWS08_SD.degreeCent <- centralization.degree(GWS08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_SDftn <- as.network.matrix(GWS08_SDft)
GWS08_SD.netDensity <- network.density(GWS08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_SD.entropy <- entropy(GWS08_SDft) #entropy

GWS08_SD.netMx <- cbind(GWS08_SD.netMx, GWS08_SD.clusterCoef, GWS08_SD.degreeCent$centralization,
                        GWS08_SD.netDensity, GWS08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
GWS08_TDg2 <- data.frame(GWS08_TD)
GWS08_TDg2 <- GWS08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_TDg2$player1
player2vector <- GWS08_TDg2$player2
GWS08_TDg3 <- GWS08_TDg2
GWS08_TDg3$p1inp2vec <- is.element(GWS08_TDg3$player1, player2vector)
GWS08_TDg3$p2inp1vec <- is.element(GWS08_TDg3$player2, player1vector)

addPlayer1 <- GWS08_TDg3[ which(GWS08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_TDg3[ which(GWS08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_TDg2 <- rbind(GWS08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
GWS08_TDft <- ftable(GWS08_TDg2$player1, GWS08_TDg2$player2)
GWS08_TDft2 <- as.matrix(GWS08_TDft)
numRows <- nrow(GWS08_TDft2)
numCols <- ncol(GWS08_TDft2)
GWS08_TDft3 <- GWS08_TDft2[c(2:numRows) , c(2:numCols)]
GWS08_TDTable <- graph.adjacency(GWS08_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(GWS08_TDTable, vertex.label = V(GWS08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
GWS08_TD.clusterCoef <- transitivity(GWS08_TDTable, type="global") #cluster coefficient
GWS08_TD.degreeCent <- centralization.degree(GWS08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_TDftn <- as.network.matrix(GWS08_TDft)
GWS08_TD.netDensity <- network.density(GWS08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_TD.entropy <- entropy(GWS08_TDft) #entropy

GWS08_TD.netMx <- cbind(GWS08_TD.netMx, GWS08_TD.clusterCoef, GWS08_TD.degreeCent$centralization,
                        GWS08_TD.netDensity, GWS08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
GWS08_QTg2 <- data.frame(GWS08_QT)
GWS08_QTg2 <- GWS08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS08_QTg2$player1
player2vector <- GWS08_QTg2$player2
GWS08_QTg3 <- GWS08_QTg2
GWS08_QTg3$p1inp2vec <- is.element(GWS08_QTg3$player1, player2vector)
GWS08_QTg3$p2inp1vec <- is.element(GWS08_QTg3$player2, player1vector)

addPlayer1 <- GWS08_QTg3[ which(GWS08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS08_QTg3[ which(GWS08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS08_QTg2 <- rbind(GWS08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
GWS08_QTft <- ftable(GWS08_QTg2$player1, GWS08_QTg2$player2)
GWS08_QTft2 <- as.matrix(GWS08_QTft)
numRows <- nrow(GWS08_QTft2)
numCols <- ncol(GWS08_QTft2)
GWS08_QTft3 <- GWS08_QTft2[c(2:numRows) , c(2:numCols)]
GWS08_QTTable <- graph.adjacency(GWS08_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(GWS08_QTTable, vertex.label = V(GWS08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
GWS08_QT.clusterCoef <- transitivity(GWS08_QTTable, type="global") #cluster coefficient
GWS08_QT.degreeCent <- centralization.degree(GWS08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS08_QTftn <- as.network.matrix(GWS08_QTft)
GWS08_QT.netDensity <- network.density(GWS08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS08_QT.entropy <- entropy(GWS08_QTft) #entropy

GWS08_QT.netMx <- cbind(GWS08_QT.netMx, GWS08_QT.clusterCoef, GWS08_QT.degreeCent$centralization,
                        GWS08_QT.netDensity, GWS08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS08_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "HAW"
KIoutcome = "Goal_F"
HAW08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
HAW08_Gg2 <- data.frame(HAW08_G)
HAW08_Gg2 <- HAW08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_Gg2$player1
player2vector <- HAW08_Gg2$player2
HAW08_Gg3 <- HAW08_Gg2
HAW08_Gg3$p1inp2vec <- is.element(HAW08_Gg3$player1, player2vector)
HAW08_Gg3$p2inp1vec <- is.element(HAW08_Gg3$player2, player1vector)

addPlayer1 <- HAW08_Gg3[ which(HAW08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW08_Gg3[ which(HAW08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_Gg2 <- rbind(HAW08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
HAW08_Gft <- ftable(HAW08_Gg2$player1, HAW08_Gg2$player2)
HAW08_Gft2 <- as.matrix(HAW08_Gft)
numRows <- nrow(HAW08_Gft2)
numCols <- ncol(HAW08_Gft2)
HAW08_Gft3 <- HAW08_Gft2[c(2:numRows) , c(2:numCols)]
HAW08_GTable <- graph.adjacency(HAW08_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(HAW08_GTable, vertex.label = V(HAW08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
HAW08_G.clusterCoef <- transitivity(HAW08_GTable, type="global") #cluster coefficient
HAW08_G.degreeCent <- centralization.degree(HAW08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_Gftn <- as.network.matrix(HAW08_Gft)
HAW08_G.netDensity <- network.density(HAW08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_G.entropy <- entropy(HAW08_Gft) #entropy

HAW08_G.netMx <- cbind(HAW08_G.netMx, HAW08_G.clusterCoef, HAW08_G.degreeCent$centralization,
                       HAW08_G.netDensity, HAW08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "HAW"
KIoutcome = "Behind_F"
HAW08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
HAW08_Bg2 <- data.frame(HAW08_B)
HAW08_Bg2 <- HAW08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_Bg2$player1
player2vector <- HAW08_Bg2$player2
HAW08_Bg3 <- HAW08_Bg2
HAW08_Bg3$p1inp2vec <- is.element(HAW08_Bg3$player1, player2vector)
HAW08_Bg3$p2inp1vec <- is.element(HAW08_Bg3$player2, player1vector)

addPlayer1 <- HAW08_Bg3[ which(HAW08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW08_Bg2 <- rbind(HAW08_Bg2, addPlayer1)

#ROUND 8, Behind graph using weighted edges
HAW08_Bft <- ftable(HAW08_Bg2$player1, HAW08_Bg2$player2)
HAW08_Bft2 <- as.matrix(HAW08_Bft)
numRows <- nrow(HAW08_Bft2)
numCols <- ncol(HAW08_Bft2)
HAW08_Bft3 <- HAW08_Bft2[c(2:numRows) , c(1:numCols)]
HAW08_BTable <- graph.adjacency(HAW08_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(HAW08_BTable, vertex.label = V(HAW08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
HAW08_B.clusterCoef <- transitivity(HAW08_BTable, type="global") #cluster coefficient
HAW08_B.degreeCent <- centralization.degree(HAW08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_Bftn <- as.network.matrix(HAW08_Bft)
HAW08_B.netDensity <- network.density(HAW08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_B.entropy <- entropy(HAW08_Bft) #entropy

HAW08_B.netMx <- cbind(HAW08_B.netMx, HAW08_B.clusterCoef, HAW08_B.degreeCent$centralization,
                       HAW08_B.netDensity, HAW08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
HAW08_SFg2 <- data.frame(HAW08_SF)
HAW08_SFg2 <- HAW08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_SFg2$player1
player2vector <- HAW08_SFg2$player2
HAW08_SFg3 <- HAW08_SFg2
HAW08_SFg3$p1inp2vec <- is.element(HAW08_SFg3$player1, player2vector)
HAW08_SFg3$p2inp1vec <- is.element(HAW08_SFg3$player2, player1vector)

addPlayer1 <- HAW08_SFg3[ which(HAW08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW08_SFg3[ which(HAW08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_SFg2 <- rbind(HAW08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
HAW08_SFft <- ftable(HAW08_SFg2$player1, HAW08_SFg2$player2)
HAW08_SFft2 <- as.matrix(HAW08_SFft)
numRows <- nrow(HAW08_SFft2)
numCols <- ncol(HAW08_SFft2)
HAW08_SFft3 <- HAW08_SFft2[c(2:numRows) , c(2:numCols)]
HAW08_SFTable <- graph.adjacency(HAW08_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(HAW08_SFTable, vertex.label = V(HAW08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
HAW08_SF.clusterCoef <- transitivity(HAW08_SFTable, type="global") #cluster coefficient
HAW08_SF.degreeCent <- centralization.degree(HAW08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_SFftn <- as.network.matrix(HAW08_SFft)
HAW08_SF.netDensity <- network.density(HAW08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_SF.entropy <- entropy(HAW08_SFft) #entropy

HAW08_SF.netMx <- cbind(HAW08_SF.netMx, HAW08_SF.clusterCoef, HAW08_SF.degreeCent$centralization,
                        HAW08_SF.netDensity, HAW08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
HAW08_TFg2 <- data.frame(HAW08_TF)
HAW08_TFg2 <- HAW08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_TFg2$player1
player2vector <- HAW08_TFg2$player2
HAW08_TFg3 <- HAW08_TFg2
HAW08_TFg3$p1inp2vec <- is.element(HAW08_TFg3$player1, player2vector)
HAW08_TFg3$p2inp1vec <- is.element(HAW08_TFg3$player2, player1vector)

addPlayer1 <- HAW08_TFg3[ which(HAW08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW08_TFg3[ which(HAW08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_TFg2 <- rbind(HAW08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
HAW08_TFft <- ftable(HAW08_TFg2$player1, HAW08_TFg2$player2)
HAW08_TFft2 <- as.matrix(HAW08_TFft)
numRows <- nrow(HAW08_TFft2)
numCols <- ncol(HAW08_TFft2)
HAW08_TFft3 <- HAW08_TFft2[c(2:numRows) , c(2:numCols)]
HAW08_TFTable <- graph.adjacency(HAW08_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(HAW08_TFTable, vertex.label = V(HAW08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
HAW08_TF.clusterCoef <- transitivity(HAW08_TFTable, type="global") #cluster coefficient
HAW08_TF.degreeCent <- centralization.degree(HAW08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_TFftn <- as.network.matrix(HAW08_TFft)
HAW08_TF.netDensity <- network.density(HAW08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_TF.entropy <- entropy(HAW08_TFft) #entropy

HAW08_TF.netMx <- cbind(HAW08_TF.netMx, HAW08_TF.clusterCoef, HAW08_TF.degreeCent$centralization,
                        HAW08_TF.netDensity, HAW08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************
#NA

round = 8
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
HAW08_SAMg2 <- data.frame(HAW08_SAM)
HAW08_SAMg2 <- HAW08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_SAMg2$player1
player2vector <- HAW08_SAMg2$player2
HAW08_SAMg3 <- HAW08_SAMg2
HAW08_SAMg3$p1inp2vec <- is.element(HAW08_SAMg3$player1, player2vector)
HAW08_SAMg3$p2inp1vec <- is.element(HAW08_SAMg3$player2, player1vector)

addPlayer1 <- HAW08_SAMg3[ which(HAW08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW08_SAMg3[ which(HAW08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_SAMg2 <- rbind(HAW08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
HAW08_SAMft <- ftable(HAW08_SAMg2$player1, HAW08_SAMg2$player2)
HAW08_SAMft2 <- as.matrix(HAW08_SAMft)
numRows <- nrow(HAW08_SAMft2)
numCols <- ncol(HAW08_SAMft2)
HAW08_SAMft3 <- HAW08_SAMft2[c(2:numRows) , c(2:numCols)]
HAW08_SAMTable <- graph.adjacency(HAW08_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(HAW08_SAMTable, vertex.label = V(HAW08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
HAW08_SAM.clusterCoef <- transitivity(HAW08_SAMTable, type="global") #cluster coefficient
HAW08_SAM.degreeCent <- centralization.degree(HAW08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_SAMftn <- as.network.matrix(HAW08_SAMft)
HAW08_SAM.netDensity <- network.density(HAW08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_SAM.entropy <- entropy(HAW08_SAMft) #entropy

HAW08_SAM.netMx <- cbind(HAW08_SAM.netMx, HAW08_SAM.clusterCoef, HAW08_SAM.degreeCent$centralization,
                         HAW08_SAM.netDensity, HAW08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
HAW08_TAMg2 <- data.frame(HAW08_TAM)
HAW08_TAMg2 <- HAW08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_TAMg2$player1
player2vector <- HAW08_TAMg2$player2
HAW08_TAMg3 <- HAW08_TAMg2
HAW08_TAMg3$p1inp2vec <- is.element(HAW08_TAMg3$player1, player2vector)
HAW08_TAMg3$p2inp1vec <- is.element(HAW08_TAMg3$player2, player1vector)

addPlayer1 <- HAW08_TAMg3[ which(HAW08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW08_TAMg3[ which(HAW08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_TAMg2 <- rbind(HAW08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
HAW08_TAMft <- ftable(HAW08_TAMg2$player1, HAW08_TAMg2$player2)
HAW08_TAMft2 <- as.matrix(HAW08_TAMft)
numRows <- nrow(HAW08_TAMft2)
numCols <- ncol(HAW08_TAMft2)
HAW08_TAMft3 <- HAW08_TAMft2[c(2:numRows) , c(2:numCols)]
HAW08_TAMTable <- graph.adjacency(HAW08_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(HAW08_TAMTable, vertex.label = V(HAW08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
HAW08_TAM.clusterCoef <- transitivity(HAW08_TAMTable, type="global") #cluster coefficient
HAW08_TAM.degreeCent <- centralization.degree(HAW08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_TAMftn <- as.network.matrix(HAW08_TAMft)
HAW08_TAM.netDensity <- network.density(HAW08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_TAM.entropy <- entropy(HAW08_TAMft) #entropy

HAW08_TAM.netMx <- cbind(HAW08_TAM.netMx, HAW08_TAM.clusterCoef, HAW08_TAM.degreeCent$centralization,
                         HAW08_TAM.netDensity, HAW08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
HAW08_SDMg2 <- data.frame(HAW08_SDM)
HAW08_SDMg2 <- HAW08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_SDMg2$player1
player2vector <- HAW08_SDMg2$player2
HAW08_SDMg3 <- HAW08_SDMg2
HAW08_SDMg3$p1inp2vec <- is.element(HAW08_SDMg3$player1, player2vector)
HAW08_SDMg3$p2inp1vec <- is.element(HAW08_SDMg3$player2, player1vector)

addPlayer1 <- HAW08_SDMg3[ which(HAW08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW08_SDMg3[ which(HAW08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_SDMg2 <- rbind(HAW08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
HAW08_SDMft <- ftable(HAW08_SDMg2$player1, HAW08_SDMg2$player2)
HAW08_SDMft2 <- as.matrix(HAW08_SDMft)
numRows <- nrow(HAW08_SDMft2)
numCols <- ncol(HAW08_SDMft2)
HAW08_SDMft3 <- HAW08_SDMft2[c(2:numRows) , c(2:numCols)]
HAW08_SDMTable <- graph.adjacency(HAW08_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(HAW08_SDMTable, vertex.label = V(HAW08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
HAW08_SDM.clusterCoef <- transitivity(HAW08_SDMTable, type="global") #cluster coefficient
HAW08_SDM.degreeCent <- centralization.degree(HAW08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_SDMftn <- as.network.matrix(HAW08_SDMft)
HAW08_SDM.netDensity <- network.density(HAW08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_SDM.entropy <- entropy(HAW08_SDMft) #entropy

HAW08_SDM.netMx <- cbind(HAW08_SDM.netMx, HAW08_SDM.clusterCoef, HAW08_SDM.degreeCent$centralization,
                         HAW08_SDM.netDensity, HAW08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
HAW08_TDMg2 <- data.frame(HAW08_TDM)
HAW08_TDMg2 <- HAW08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_TDMg2$player1
player2vector <- HAW08_TDMg2$player2
HAW08_TDMg3 <- HAW08_TDMg2
HAW08_TDMg3$p1inp2vec <- is.element(HAW08_TDMg3$player1, player2vector)
HAW08_TDMg3$p2inp1vec <- is.element(HAW08_TDMg3$player2, player1vector)

addPlayer1 <- HAW08_TDMg3[ which(HAW08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW08_TDMg3[ which(HAW08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_TDMg2 <- rbind(HAW08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
HAW08_TDMft <- ftable(HAW08_TDMg2$player1, HAW08_TDMg2$player2)
HAW08_TDMft2 <- as.matrix(HAW08_TDMft)
numRows <- nrow(HAW08_TDMft2)
numCols <- ncol(HAW08_TDMft2)
HAW08_TDMft3 <- HAW08_TDMft2[c(2:numRows) , c(2:numCols)]
HAW08_TDMTable <- graph.adjacency(HAW08_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(HAW08_TDMTable, vertex.label = V(HAW08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
HAW08_TDM.clusterCoef <- transitivity(HAW08_TDMTable, type="global") #cluster coefficient
HAW08_TDM.degreeCent <- centralization.degree(HAW08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_TDMftn <- as.network.matrix(HAW08_TDMft)
HAW08_TDM.netDensity <- network.density(HAW08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_TDM.entropy <- entropy(HAW08_TDMft) #entropy

HAW08_TDM.netMx <- cbind(HAW08_TDM.netMx, HAW08_TDM.clusterCoef, HAW08_TDM.degreeCent$centralization,
                         HAW08_TDM.netDensity, HAW08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
HAW08_SDg2 <- data.frame(HAW08_SD)
HAW08_SDg2 <- HAW08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_SDg2$player1
player2vector <- HAW08_SDg2$player2
HAW08_SDg3 <- HAW08_SDg2
HAW08_SDg3$p1inp2vec <- is.element(HAW08_SDg3$player1, player2vector)
HAW08_SDg3$p2inp1vec <- is.element(HAW08_SDg3$player2, player1vector)

addPlayer1 <- HAW08_SDg3[ which(HAW08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW08_SDg3[ which(HAW08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_SDg2 <- rbind(HAW08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
HAW08_SDft <- ftable(HAW08_SDg2$player1, HAW08_SDg2$player2)
HAW08_SDft2 <- as.matrix(HAW08_SDft)
numRows <- nrow(HAW08_SDft2)
numCols <- ncol(HAW08_SDft2)
HAW08_SDft3 <- HAW08_SDft2[c(2:numRows) , c(2:numCols)]
HAW08_SDTable <- graph.adjacency(HAW08_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(HAW08_SDTable, vertex.label = V(HAW08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
HAW08_SD.clusterCoef <- transitivity(HAW08_SDTable, type="global") #cluster coefficient
HAW08_SD.degreeCent <- centralization.degree(HAW08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_SDftn <- as.network.matrix(HAW08_SDft)
HAW08_SD.netDensity <- network.density(HAW08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_SD.entropy <- entropy(HAW08_SDft) #entropy

HAW08_SD.netMx <- cbind(HAW08_SD.netMx, HAW08_SD.clusterCoef, HAW08_SD.degreeCent$centralization,
                        HAW08_SD.netDensity, HAW08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
HAW08_TDg2 <- data.frame(HAW08_TD)
HAW08_TDg2 <- HAW08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_TDg2$player1
player2vector <- HAW08_TDg2$player2
HAW08_TDg3 <- HAW08_TDg2
HAW08_TDg3$p1inp2vec <- is.element(HAW08_TDg3$player1, player2vector)
HAW08_TDg3$p2inp1vec <- is.element(HAW08_TDg3$player2, player1vector)

addPlayer1 <- HAW08_TDg3[ which(HAW08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW08_TDg3[ which(HAW08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_TDg2 <- rbind(HAW08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
HAW08_TDft <- ftable(HAW08_TDg2$player1, HAW08_TDg2$player2)
HAW08_TDft2 <- as.matrix(HAW08_TDft)
numRows <- nrow(HAW08_TDft2)
numCols <- ncol(HAW08_TDft2)
HAW08_TDft3 <- HAW08_TDft2[c(2:numRows) , c(2:numCols)]
HAW08_TDTable <- graph.adjacency(HAW08_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(HAW08_TDTable, vertex.label = V(HAW08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
HAW08_TD.clusterCoef <- transitivity(HAW08_TDTable, type="global") #cluster coefficient
HAW08_TD.degreeCent <- centralization.degree(HAW08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_TDftn <- as.network.matrix(HAW08_TDft)
HAW08_TD.netDensity <- network.density(HAW08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_TD.entropy <- entropy(HAW08_TDft) #entropy

HAW08_TD.netMx <- cbind(HAW08_TD.netMx, HAW08_TD.clusterCoef, HAW08_TD.degreeCent$centralization,
                        HAW08_TD.netDensity, HAW08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
HAW08_QTg2 <- data.frame(HAW08_QT)
HAW08_QTg2 <- HAW08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW08_QTg2$player1
player2vector <- HAW08_QTg2$player2
HAW08_QTg3 <- HAW08_QTg2
HAW08_QTg3$p1inp2vec <- is.element(HAW08_QTg3$player1, player2vector)
HAW08_QTg3$p2inp1vec <- is.element(HAW08_QTg3$player2, player1vector)

addPlayer1 <- HAW08_QTg3[ which(HAW08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW08_QTg3[ which(HAW08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW08_QTg2 <- rbind(HAW08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
HAW08_QTft <- ftable(HAW08_QTg2$player1, HAW08_QTg2$player2)
HAW08_QTft2 <- as.matrix(HAW08_QTft)
numRows <- nrow(HAW08_QTft2)
numCols <- ncol(HAW08_QTft2)
HAW08_QTft3 <- HAW08_QTft2[c(2:numRows) , c(2:numCols)]
HAW08_QTTable <- graph.adjacency(HAW08_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(HAW08_QTTable, vertex.label = V(HAW08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
HAW08_QT.clusterCoef <- transitivity(HAW08_QTTable, type="global") #cluster coefficient
HAW08_QT.degreeCent <- centralization.degree(HAW08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW08_QTftn <- as.network.matrix(HAW08_QTft)
HAW08_QT.netDensity <- network.density(HAW08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW08_QT.entropy <- entropy(HAW08_QTft) #entropy

HAW08_QT.netMx <- cbind(HAW08_QT.netMx, HAW08_QT.clusterCoef, HAW08_QT.degreeCent$centralization,
                        HAW08_QT.netDensity, HAW08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW08_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "MELB"
KIoutcome = "Goal_F"
MELB08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
MELB08_Gg2 <- data.frame(MELB08_G)
MELB08_Gg2 <- MELB08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_Gg2$player1
player2vector <- MELB08_Gg2$player2
MELB08_Gg3 <- MELB08_Gg2
MELB08_Gg3$p1inp2vec <- is.element(MELB08_Gg3$player1, player2vector)
MELB08_Gg3$p2inp1vec <- is.element(MELB08_Gg3$player2, player1vector)

addPlayer1 <- MELB08_Gg3[ which(MELB08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB08_Gg2 <- rbind(MELB08_Gg2, addPlayer1)

#ROUND 8, Goal graph using weighted edges
MELB08_Gft <- ftable(MELB08_Gg2$player1, MELB08_Gg2$player2)
MELB08_Gft2 <- as.matrix(MELB08_Gft)
numRows <- nrow(MELB08_Gft2)
numCols <- ncol(MELB08_Gft2)
MELB08_Gft3 <- MELB08_Gft2[c(2:numRows) , c(1:numCols)]
MELB08_GTable <- graph.adjacency(MELB08_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(MELB08_GTable, vertex.label = V(MELB08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
MELB08_G.clusterCoef <- transitivity(MELB08_GTable, type="global") #cluster coefficient
MELB08_G.degreeCent <- centralization.degree(MELB08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_Gftn <- as.network.matrix(MELB08_Gft)
MELB08_G.netDensity <- network.density(MELB08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_G.entropy <- entropy(MELB08_Gft) #entropy

MELB08_G.netMx <- cbind(MELB08_G.netMx, MELB08_G.clusterCoef, MELB08_G.degreeCent$centralization,
                        MELB08_G.netDensity, MELB08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************

round = 8
teamName = "MELB"
KIoutcome = "Behind_F"
MELB08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
MELB08_Bg2 <- data.frame(MELB08_B)
MELB08_Bg2 <- MELB08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_Bg2$player1
player2vector <- MELB08_Bg2$player2
MELB08_Bg3 <- MELB08_Bg2
MELB08_Bg3$p1inp2vec <- is.element(MELB08_Bg3$player1, player2vector)
MELB08_Bg3$p2inp1vec <- is.element(MELB08_Bg3$player2, player1vector)

addPlayer1 <- MELB08_Bg3[ which(MELB08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_Bg3[ which(MELB08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_Bg2 <- rbind(MELB08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
MELB08_Bft <- ftable(MELB08_Bg2$player1, MELB08_Bg2$player2)
MELB08_Bft2 <- as.matrix(MELB08_Bft)
numRows <- nrow(MELB08_Bft2)
numCols <- ncol(MELB08_Bft2)
MELB08_Bft3 <- MELB08_Bft2[c(2:numRows) , c(2:numCols)]
MELB08_BTable <- graph.adjacency(MELB08_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(MELB08_BTable, vertex.label = V(MELB08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
MELB08_B.clusterCoef <- transitivity(MELB08_BTable, type="global") #cluster coefficient
MELB08_B.degreeCent <- centralization.degree(MELB08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_Bftn <- as.network.matrix(MELB08_Bft)
MELB08_B.netDensity <- network.density(MELB08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_B.entropy <- entropy(MELB08_Bft) #entropy

MELB08_B.netMx <- cbind(MELB08_B.netMx, MELB08_B.clusterCoef, MELB08_B.degreeCent$centralization,
                        MELB08_B.netDensity, MELB08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
MELB08_SFg2 <- data.frame(MELB08_SF)
MELB08_SFg2 <- MELB08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_SFg2$player1
player2vector <- MELB08_SFg2$player2
MELB08_SFg3 <- MELB08_SFg2
MELB08_SFg3$p1inp2vec <- is.element(MELB08_SFg3$player1, player2vector)
MELB08_SFg3$p2inp1vec <- is.element(MELB08_SFg3$player2, player1vector)

addPlayer1 <- MELB08_SFg3[ which(MELB08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_SFg3[ which(MELB08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_SFg2 <- rbind(MELB08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
MELB08_SFft <- ftable(MELB08_SFg2$player1, MELB08_SFg2$player2)
MELB08_SFft2 <- as.matrix(MELB08_SFft)
numRows <- nrow(MELB08_SFft2)
numCols <- ncol(MELB08_SFft2)
MELB08_SFft3 <- MELB08_SFft2[c(2:numRows) , c(2:numCols)]
MELB08_SFTable <- graph.adjacency(MELB08_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(MELB08_SFTable, vertex.label = V(MELB08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
MELB08_SF.clusterCoef <- transitivity(MELB08_SFTable, type="global") #cluster coefficient
MELB08_SF.degreeCent <- centralization.degree(MELB08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_SFftn <- as.network.matrix(MELB08_SFft)
MELB08_SF.netDensity <- network.density(MELB08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_SF.entropy <- entropy(MELB08_SFft) #entropy

MELB08_SF.netMx <- cbind(MELB08_SF.netMx, MELB08_SF.clusterCoef, MELB08_SF.degreeCent$centralization,
                         MELB08_SF.netDensity, MELB08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
MELB08_TFg2 <- data.frame(MELB08_TF)
MELB08_TFg2 <- MELB08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_TFg2$player1
player2vector <- MELB08_TFg2$player2
MELB08_TFg3 <- MELB08_TFg2
MELB08_TFg3$p1inp2vec <- is.element(MELB08_TFg3$player1, player2vector)
MELB08_TFg3$p2inp1vec <- is.element(MELB08_TFg3$player2, player1vector)

addPlayer1 <- MELB08_TFg3[ which(MELB08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_TFg3[ which(MELB08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_TFg2 <- rbind(MELB08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
MELB08_TFft <- ftable(MELB08_TFg2$player1, MELB08_TFg2$player2)
MELB08_TFft2 <- as.matrix(MELB08_TFft)
numRows <- nrow(MELB08_TFft2)
numCols <- ncol(MELB08_TFft2)
MELB08_TFft3 <- MELB08_TFft2[c(2:numRows) , c(2:numCols)]
MELB08_TFTable <- graph.adjacency(MELB08_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(MELB08_TFTable, vertex.label = V(MELB08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
MELB08_TF.clusterCoef <- transitivity(MELB08_TFTable, type="global") #cluster coefficient
MELB08_TF.degreeCent <- centralization.degree(MELB08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_TFftn <- as.network.matrix(MELB08_TFft)
MELB08_TF.netDensity <- network.density(MELB08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_TF.entropy <- entropy(MELB08_TFft) #entropy

MELB08_TF.netMx <- cbind(MELB08_TF.netMx, MELB08_TF.clusterCoef, MELB08_TF.degreeCent$centralization,
                         MELB08_TF.netDensity, MELB08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************
#NA

round = 8
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
MELB08_SAMg2 <- data.frame(MELB08_SAM)
MELB08_SAMg2 <- MELB08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_SAMg2$player1
player2vector <- MELB08_SAMg2$player2
MELB08_SAMg3 <- MELB08_SAMg2
MELB08_SAMg3$p1inp2vec <- is.element(MELB08_SAMg3$player1, player2vector)
MELB08_SAMg3$p2inp1vec <- is.element(MELB08_SAMg3$player2, player1vector)

addPlayer1 <- MELB08_SAMg3[ which(MELB08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_SAMg3[ which(MELB08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_SAMg2 <- rbind(MELB08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
MELB08_SAMft <- ftable(MELB08_SAMg2$player1, MELB08_SAMg2$player2)
MELB08_SAMft2 <- as.matrix(MELB08_SAMft)
numRows <- nrow(MELB08_SAMft2)
numCols <- ncol(MELB08_SAMft2)
MELB08_SAMft3 <- MELB08_SAMft2[c(2:numRows) , c(2:numCols)]
MELB08_SAMTable <- graph.adjacency(MELB08_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(MELB08_SAMTable, vertex.label = V(MELB08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
MELB08_SAM.clusterCoef <- transitivity(MELB08_SAMTable, type="global") #cluster coefficient
MELB08_SAM.degreeCent <- centralization.degree(MELB08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_SAMftn <- as.network.matrix(MELB08_SAMft)
MELB08_SAM.netDensity <- network.density(MELB08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_SAM.entropy <- entropy(MELB08_SAMft) #entropy

MELB08_SAM.netMx <- cbind(MELB08_SAM.netMx, MELB08_SAM.clusterCoef, MELB08_SAM.degreeCent$centralization,
                          MELB08_SAM.netDensity, MELB08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************
#NA

round = 8
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
MELB08_TAMg2 <- data.frame(MELB08_TAM)
MELB08_TAMg2 <- MELB08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_TAMg2$player1
player2vector <- MELB08_TAMg2$player2
MELB08_TAMg3 <- MELB08_TAMg2
MELB08_TAMg3$p1inp2vec <- is.element(MELB08_TAMg3$player1, player2vector)
MELB08_TAMg3$p2inp1vec <- is.element(MELB08_TAMg3$player2, player1vector)

addPlayer1 <- MELB08_TAMg3[ which(MELB08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_TAMg3[ which(MELB08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_TAMg2 <- rbind(MELB08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
MELB08_TAMft <- ftable(MELB08_TAMg2$player1, MELB08_TAMg2$player2)
MELB08_TAMft2 <- as.matrix(MELB08_TAMft)
numRows <- nrow(MELB08_TAMft2)
numCols <- ncol(MELB08_TAMft2)
MELB08_TAMft3 <- MELB08_TAMft2[c(2:numRows) , c(2:numCols)]
MELB08_TAMTable <- graph.adjacency(MELB08_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(MELB08_TAMTable, vertex.label = V(MELB08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
MELB08_TAM.clusterCoef <- transitivity(MELB08_TAMTable, type="global") #cluster coefficient
MELB08_TAM.degreeCent <- centralization.degree(MELB08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_TAMftn <- as.network.matrix(MELB08_TAMft)
MELB08_TAM.netDensity <- network.density(MELB08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_TAM.entropy <- entropy(MELB08_TAMft) #entropy

MELB08_TAM.netMx <- cbind(MELB08_TAM.netMx, MELB08_TAM.clusterCoef, MELB08_TAM.degreeCent$centralization,
                          MELB08_TAM.netDensity, MELB08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
MELB08_SDMg2 <- data.frame(MELB08_SDM)
MELB08_SDMg2 <- MELB08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_SDMg2$player1
player2vector <- MELB08_SDMg2$player2
MELB08_SDMg3 <- MELB08_SDMg2
MELB08_SDMg3$p1inp2vec <- is.element(MELB08_SDMg3$player1, player2vector)
MELB08_SDMg3$p2inp1vec <- is.element(MELB08_SDMg3$player2, player1vector)

addPlayer1 <- MELB08_SDMg3[ which(MELB08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_SDMg3[ which(MELB08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_SDMg2 <- rbind(MELB08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
MELB08_SDMft <- ftable(MELB08_SDMg2$player1, MELB08_SDMg2$player2)
MELB08_SDMft2 <- as.matrix(MELB08_SDMft)
numRows <- nrow(MELB08_SDMft2)
numCols <- ncol(MELB08_SDMft2)
MELB08_SDMft3 <- MELB08_SDMft2[c(2:numRows) , c(2:numCols)]
MELB08_SDMTable <- graph.adjacency(MELB08_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(MELB08_SDMTable, vertex.label = V(MELB08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
MELB08_SDM.clusterCoef <- transitivity(MELB08_SDMTable, type="global") #cluster coefficient
MELB08_SDM.degreeCent <- centralization.degree(MELB08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_SDMftn <- as.network.matrix(MELB08_SDMft)
MELB08_SDM.netDensity <- network.density(MELB08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_SDM.entropy <- entropy(MELB08_SDMft) #entropy

MELB08_SDM.netMx <- cbind(MELB08_SDM.netMx, MELB08_SDM.clusterCoef, MELB08_SDM.degreeCent$centralization,
                          MELB08_SDM.netDensity, MELB08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
MELB08_TDMg2 <- data.frame(MELB08_TDM)
MELB08_TDMg2 <- MELB08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_TDMg2$player1
player2vector <- MELB08_TDMg2$player2
MELB08_TDMg3 <- MELB08_TDMg2
MELB08_TDMg3$p1inp2vec <- is.element(MELB08_TDMg3$player1, player2vector)
MELB08_TDMg3$p2inp1vec <- is.element(MELB08_TDMg3$player2, player1vector)

addPlayer1 <- MELB08_TDMg3[ which(MELB08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_TDMg3[ which(MELB08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_TDMg2 <- rbind(MELB08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
MELB08_TDMft <- ftable(MELB08_TDMg2$player1, MELB08_TDMg2$player2)
MELB08_TDMft2 <- as.matrix(MELB08_TDMft)
numRows <- nrow(MELB08_TDMft2)
numCols <- ncol(MELB08_TDMft2)
MELB08_TDMft3 <- MELB08_TDMft2[c(2:numRows) , c(2:numCols)]
MELB08_TDMTable <- graph.adjacency(MELB08_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(MELB08_TDMTable, vertex.label = V(MELB08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
MELB08_TDM.clusterCoef <- transitivity(MELB08_TDMTable, type="global") #cluster coefficient
MELB08_TDM.degreeCent <- centralization.degree(MELB08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_TDMftn <- as.network.matrix(MELB08_TDMft)
MELB08_TDM.netDensity <- network.density(MELB08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_TDM.entropy <- entropy(MELB08_TDMft) #entropy

MELB08_TDM.netMx <- cbind(MELB08_TDM.netMx, MELB08_TDM.clusterCoef, MELB08_TDM.degreeCent$centralization,
                          MELB08_TDM.netDensity, MELB08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
MELB08_SDg2 <- data.frame(MELB08_SD)
MELB08_SDg2 <- MELB08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_SDg2$player1
player2vector <- MELB08_SDg2$player2
MELB08_SDg3 <- MELB08_SDg2
MELB08_SDg3$p1inp2vec <- is.element(MELB08_SDg3$player1, player2vector)
MELB08_SDg3$p2inp1vec <- is.element(MELB08_SDg3$player2, player1vector)

addPlayer1 <- MELB08_SDg3[ which(MELB08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_SDg3[ which(MELB08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_SDg2 <- rbind(MELB08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
MELB08_SDft <- ftable(MELB08_SDg2$player1, MELB08_SDg2$player2)
MELB08_SDft2 <- as.matrix(MELB08_SDft)
numRows <- nrow(MELB08_SDft2)
numCols <- ncol(MELB08_SDft2)
MELB08_SDft3 <- MELB08_SDft2[c(2:numRows) , c(2:numCols)]
MELB08_SDTable <- graph.adjacency(MELB08_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(MELB08_SDTable, vertex.label = V(MELB08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
MELB08_SD.clusterCoef <- transitivity(MELB08_SDTable, type="global") #cluster coefficient
MELB08_SD.degreeCent <- centralization.degree(MELB08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_SDftn <- as.network.matrix(MELB08_SDft)
MELB08_SD.netDensity <- network.density(MELB08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_SD.entropy <- entropy(MELB08_SDft) #entropy

MELB08_SD.netMx <- cbind(MELB08_SD.netMx, MELB08_SD.clusterCoef, MELB08_SD.degreeCent$centralization,
                         MELB08_SD.netDensity, MELB08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
MELB08_TDg2 <- data.frame(MELB08_TD)
MELB08_TDg2 <- MELB08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_TDg2$player1
player2vector <- MELB08_TDg2$player2
MELB08_TDg3 <- MELB08_TDg2
MELB08_TDg3$p1inp2vec <- is.element(MELB08_TDg3$player1, player2vector)
MELB08_TDg3$p2inp1vec <- is.element(MELB08_TDg3$player2, player1vector)

addPlayer1 <- MELB08_TDg3[ which(MELB08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_TDg3[ which(MELB08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_TDg2 <- rbind(MELB08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
MELB08_TDft <- ftable(MELB08_TDg2$player1, MELB08_TDg2$player2)
MELB08_TDft2 <- as.matrix(MELB08_TDft)
numRows <- nrow(MELB08_TDft2)
numCols <- ncol(MELB08_TDft2)
MELB08_TDft3 <- MELB08_TDft2[c(2:numRows) , c(2:numCols)]
MELB08_TDTable <- graph.adjacency(MELB08_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(MELB08_TDTable, vertex.label = V(MELB08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
MELB08_TD.clusterCoef <- transitivity(MELB08_TDTable, type="global") #cluster coefficient
MELB08_TD.degreeCent <- centralization.degree(MELB08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_TDftn <- as.network.matrix(MELB08_TDft)
MELB08_TD.netDensity <- network.density(MELB08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_TD.entropy <- entropy(MELB08_TDft) #entropy

MELB08_TD.netMx <- cbind(MELB08_TD.netMx, MELB08_TD.clusterCoef, MELB08_TD.degreeCent$centralization,
                         MELB08_TD.netDensity, MELB08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
MELB08_QTg2 <- data.frame(MELB08_QT)
MELB08_QTg2 <- MELB08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB08_QTg2$player1
player2vector <- MELB08_QTg2$player2
MELB08_QTg3 <- MELB08_QTg2
MELB08_QTg3$p1inp2vec <- is.element(MELB08_QTg3$player1, player2vector)
MELB08_QTg3$p2inp1vec <- is.element(MELB08_QTg3$player2, player1vector)

addPlayer1 <- MELB08_QTg3[ which(MELB08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB08_QTg3[ which(MELB08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB08_QTg2 <- rbind(MELB08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
MELB08_QTft <- ftable(MELB08_QTg2$player1, MELB08_QTg2$player2)
MELB08_QTft2 <- as.matrix(MELB08_QTft)
numRows <- nrow(MELB08_QTft2)
numCols <- ncol(MELB08_QTft2)
MELB08_QTft3 <- MELB08_QTft2[c(2:numRows) , c(2:numCols)]
MELB08_QTTable <- graph.adjacency(MELB08_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(MELB08_QTTable, vertex.label = V(MELB08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
MELB08_QT.clusterCoef <- transitivity(MELB08_QTTable, type="global") #cluster coefficient
MELB08_QT.degreeCent <- centralization.degree(MELB08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB08_QTftn <- as.network.matrix(MELB08_QTft)
MELB08_QT.netDensity <- network.density(MELB08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB08_QT.entropy <- entropy(MELB08_QTft) #entropy

MELB08_QT.netMx <- cbind(MELB08_QT.netMx, MELB08_QT.clusterCoef, MELB08_QT.degreeCent$centralization,
                         MELB08_QT.netDensity, MELB08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB08_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
NMFC08_Gg2 <- data.frame(NMFC08_G)
NMFC08_Gg2 <- NMFC08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_Gg2$player1
player2vector <- NMFC08_Gg2$player2
NMFC08_Gg3 <- NMFC08_Gg2
NMFC08_Gg3$p1inp2vec <- is.element(NMFC08_Gg3$player1, player2vector)
NMFC08_Gg3$p2inp1vec <- is.element(NMFC08_Gg3$player2, player1vector)

addPlayer1 <- NMFC08_Gg3[ which(NMFC08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC08_Gg3[ which(NMFC08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_Gg2 <- rbind(NMFC08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
NMFC08_Gft <- ftable(NMFC08_Gg2$player1, NMFC08_Gg2$player2)
NMFC08_Gft2 <- as.matrix(NMFC08_Gft)
numRows <- nrow(NMFC08_Gft2)
numCols <- ncol(NMFC08_Gft2)
NMFC08_Gft3 <- NMFC08_Gft2[c(2:numRows) , c(2:numCols)]
NMFC08_GTable <- graph.adjacency(NMFC08_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(NMFC08_GTable, vertex.label = V(NMFC08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
NMFC08_G.clusterCoef <- transitivity(NMFC08_GTable, type="global") #cluster coefficient
NMFC08_G.degreeCent <- centralization.degree(NMFC08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_Gftn <- as.network.matrix(NMFC08_Gft)
NMFC08_G.netDensity <- network.density(NMFC08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_G.entropy <- entropy(NMFC08_Gft) #entropy

NMFC08_G.netMx <- cbind(NMFC08_G.netMx, NMFC08_G.clusterCoef, NMFC08_G.degreeCent$centralization,
                        NMFC08_G.netDensity, NMFC08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************

round = 8
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
NMFC08_Bg2 <- data.frame(NMFC08_B)
NMFC08_Bg2 <- NMFC08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_Bg2$player1
player2vector <- NMFC08_Bg2$player2
NMFC08_Bg3 <- NMFC08_Bg2
NMFC08_Bg3$p1inp2vec <- is.element(NMFC08_Bg3$player1, player2vector)
NMFC08_Bg3$p2inp1vec <- is.element(NMFC08_Bg3$player2, player1vector)

addPlayer1 <- NMFC08_Bg3[ which(NMFC08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC08_Bg3[ which(NMFC08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_Bg2 <- rbind(NMFC08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
NMFC08_Bft <- ftable(NMFC08_Bg2$player1, NMFC08_Bg2$player2)
NMFC08_Bft2 <- as.matrix(NMFC08_Bft)
numRows <- nrow(NMFC08_Bft2)
numCols <- ncol(NMFC08_Bft2)
NMFC08_Bft3 <- NMFC08_Bft2[c(2:numRows) , c(2:numCols)]
NMFC08_BTable <- graph.adjacency(NMFC08_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(NMFC08_BTable, vertex.label = V(NMFC08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
NMFC08_B.clusterCoef <- transitivity(NMFC08_BTable, type="global") #cluster coefficient
NMFC08_B.degreeCent <- centralization.degree(NMFC08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_Bftn <- as.network.matrix(NMFC08_Bft)
NMFC08_B.netDensity <- network.density(NMFC08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_B.entropy <- entropy(NMFC08_Bft) #entropy

NMFC08_B.netMx <- cbind(NMFC08_B.netMx, NMFC08_B.clusterCoef, NMFC08_B.degreeCent$centralization,
                        NMFC08_B.netDensity, NMFC08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
NMFC08_SFg2 <- data.frame(NMFC08_SF)
NMFC08_SFg2 <- NMFC08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_SFg2$player1
player2vector <- NMFC08_SFg2$player2
NMFC08_SFg3 <- NMFC08_SFg2
NMFC08_SFg3$p1inp2vec <- is.element(NMFC08_SFg3$player1, player2vector)
NMFC08_SFg3$p2inp1vec <- is.element(NMFC08_SFg3$player2, player1vector)

addPlayer1 <- NMFC08_SFg3[ which(NMFC08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC08_SFg3[ which(NMFC08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_SFg2 <- rbind(NMFC08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
NMFC08_SFft <- ftable(NMFC08_SFg2$player1, NMFC08_SFg2$player2)
NMFC08_SFft2 <- as.matrix(NMFC08_SFft)
numRows <- nrow(NMFC08_SFft2)
numCols <- ncol(NMFC08_SFft2)
NMFC08_SFft3 <- NMFC08_SFft2[c(2:numRows) , c(2:numCols)]
NMFC08_SFTable <- graph.adjacency(NMFC08_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(NMFC08_SFTable, vertex.label = V(NMFC08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
NMFC08_SF.clusterCoef <- transitivity(NMFC08_SFTable, type="global") #cluster coefficient
NMFC08_SF.degreeCent <- centralization.degree(NMFC08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_SFftn <- as.network.matrix(NMFC08_SFft)
NMFC08_SF.netDensity <- network.density(NMFC08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_SF.entropy <- entropy(NMFC08_SFft) #entropy

NMFC08_SF.netMx <- cbind(NMFC08_SF.netMx, NMFC08_SF.clusterCoef, NMFC08_SF.degreeCent$centralization,
                         NMFC08_SF.netDensity, NMFC08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
NMFC08_TFg2 <- data.frame(NMFC08_TF)
NMFC08_TFg2 <- NMFC08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_TFg2$player1
player2vector <- NMFC08_TFg2$player2
NMFC08_TFg3 <- NMFC08_TFg2
NMFC08_TFg3$p1inp2vec <- is.element(NMFC08_TFg3$player1, player2vector)
NMFC08_TFg3$p2inp1vec <- is.element(NMFC08_TFg3$player2, player1vector)

addPlayer1 <- NMFC08_TFg3[ which(NMFC08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC08_TFg3[ which(NMFC08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_TFg2 <- rbind(NMFC08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
NMFC08_TFft <- ftable(NMFC08_TFg2$player1, NMFC08_TFg2$player2)
NMFC08_TFft2 <- as.matrix(NMFC08_TFft)
numRows <- nrow(NMFC08_TFft2)
numCols <- ncol(NMFC08_TFft2)
NMFC08_TFft3 <- NMFC08_TFft2[c(2:numRows) , c(2:numCols)]
NMFC08_TFTable <- graph.adjacency(NMFC08_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(NMFC08_TFTable, vertex.label = V(NMFC08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
NMFC08_TF.clusterCoef <- transitivity(NMFC08_TFTable, type="global") #cluster coefficient
NMFC08_TF.degreeCent <- centralization.degree(NMFC08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_TFftn <- as.network.matrix(NMFC08_TFft)
NMFC08_TF.netDensity <- network.density(NMFC08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_TF.entropy <- entropy(NMFC08_TFft) #entropy

NMFC08_TF.netMx <- cbind(NMFC08_TF.netMx, NMFC08_TF.clusterCoef, NMFC08_TF.degreeCent$centralization,
                         NMFC08_TF.netDensity, NMFC08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************

round = 8
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
NMFC08_SAMg2 <- data.frame(NMFC08_SAM)
NMFC08_SAMg2 <- NMFC08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_SAMg2$player1
player2vector <- NMFC08_SAMg2$player2
NMFC08_SAMg3 <- NMFC08_SAMg2
NMFC08_SAMg3$p1inp2vec <- is.element(NMFC08_SAMg3$player1, player2vector)
NMFC08_SAMg3$p2inp1vec <- is.element(NMFC08_SAMg3$player2, player1vector)

addPlayer1 <- NMFC08_SAMg3[ which(NMFC08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC08_SAMg3[ which(NMFC08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_SAMg2 <- rbind(NMFC08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
NMFC08_SAMft <- ftable(NMFC08_SAMg2$player1, NMFC08_SAMg2$player2)
NMFC08_SAMft2 <- as.matrix(NMFC08_SAMft)
numRows <- nrow(NMFC08_SAMft2)
numCols <- ncol(NMFC08_SAMft2)
NMFC08_SAMft3 <- NMFC08_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC08_SAMTable <- graph.adjacency(NMFC08_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(NMFC08_SAMTable, vertex.label = V(NMFC08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
NMFC08_SAM.clusterCoef <- transitivity(NMFC08_SAMTable, type="global") #cluster coefficient
NMFC08_SAM.degreeCent <- centralization.degree(NMFC08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_SAMftn <- as.network.matrix(NMFC08_SAMft)
NMFC08_SAM.netDensity <- network.density(NMFC08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_SAM.entropy <- entropy(NMFC08_SAMft) #entropy

NMFC08_SAM.netMx <- cbind(NMFC08_SAM.netMx, NMFC08_SAM.clusterCoef, NMFC08_SAM.degreeCent$centralization,
                          NMFC08_SAM.netDensity, NMFC08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************
#NA

round = 8
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
NMFC08_TAMg2 <- data.frame(NMFC08_TAM)
NMFC08_TAMg2 <- NMFC08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_TAMg2$player1
player2vector <- NMFC08_TAMg2$player2
NMFC08_TAMg3 <- NMFC08_TAMg2
NMFC08_TAMg3$p1inp2vec <- is.element(NMFC08_TAMg3$player1, player2vector)
NMFC08_TAMg3$p2inp1vec <- is.element(NMFC08_TAMg3$player2, player1vector)

addPlayer1 <- NMFC08_TAMg3[ which(NMFC08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC08_TAMg3[ which(NMFC08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_TAMg2 <- rbind(NMFC08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
NMFC08_TAMft <- ftable(NMFC08_TAMg2$player1, NMFC08_TAMg2$player2)
NMFC08_TAMft2 <- as.matrix(NMFC08_TAMft)
numRows <- nrow(NMFC08_TAMft2)
numCols <- ncol(NMFC08_TAMft2)
NMFC08_TAMft3 <- NMFC08_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC08_TAMTable <- graph.adjacency(NMFC08_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(NMFC08_TAMTable, vertex.label = V(NMFC08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
NMFC08_TAM.clusterCoef <- transitivity(NMFC08_TAMTable, type="global") #cluster coefficient
NMFC08_TAM.degreeCent <- centralization.degree(NMFC08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_TAMftn <- as.network.matrix(NMFC08_TAMft)
NMFC08_TAM.netDensity <- network.density(NMFC08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_TAM.entropy <- entropy(NMFC08_TAMft) #entropy

NMFC08_TAM.netMx <- cbind(NMFC08_TAM.netMx, NMFC08_TAM.clusterCoef, NMFC08_TAM.degreeCent$centralization,
                          NMFC08_TAM.netDensity, NMFC08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************
#NA

round = 8
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
NMFC08_SDMg2 <- data.frame(NMFC08_SDM)
NMFC08_SDMg2 <- NMFC08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_SDMg2$player1
player2vector <- NMFC08_SDMg2$player2
NMFC08_SDMg3 <- NMFC08_SDMg2
NMFC08_SDMg3$p1inp2vec <- is.element(NMFC08_SDMg3$player1, player2vector)
NMFC08_SDMg3$p2inp1vec <- is.element(NMFC08_SDMg3$player2, player1vector)

addPlayer1 <- NMFC08_SDMg3[ which(NMFC08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC08_SDMg3[ which(NMFC08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_SDMg2 <- rbind(NMFC08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
NMFC08_SDMft <- ftable(NMFC08_SDMg2$player1, NMFC08_SDMg2$player2)
NMFC08_SDMft2 <- as.matrix(NMFC08_SDMft)
numRows <- nrow(NMFC08_SDMft2)
numCols <- ncol(NMFC08_SDMft2)
NMFC08_SDMft3 <- NMFC08_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC08_SDMTable <- graph.adjacency(NMFC08_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(NMFC08_SDMTable, vertex.label = V(NMFC08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
NMFC08_SDM.clusterCoef <- transitivity(NMFC08_SDMTable, type="global") #cluster coefficient
NMFC08_SDM.degreeCent <- centralization.degree(NMFC08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_SDMftn <- as.network.matrix(NMFC08_SDMft)
NMFC08_SDM.netDensity <- network.density(NMFC08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_SDM.entropy <- entropy(NMFC08_SDMft) #entropy

NMFC08_SDM.netMx <- cbind(NMFC08_SDM.netMx, NMFC08_SDM.clusterCoef, NMFC08_SDM.degreeCent$centralization,
                          NMFC08_SDM.netDensity, NMFC08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
NMFC08_TDMg2 <- data.frame(NMFC08_TDM)
NMFC08_TDMg2 <- NMFC08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_TDMg2$player1
player2vector <- NMFC08_TDMg2$player2
NMFC08_TDMg3 <- NMFC08_TDMg2
NMFC08_TDMg3$p1inp2vec <- is.element(NMFC08_TDMg3$player1, player2vector)
NMFC08_TDMg3$p2inp1vec <- is.element(NMFC08_TDMg3$player2, player1vector)

addPlayer1 <- NMFC08_TDMg3[ which(NMFC08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- NMFC08_TDMg3[ which(NMFC08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_TDMg2 <- rbind(NMFC08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
NMFC08_TDMft <- ftable(NMFC08_TDMg2$player1, NMFC08_TDMg2$player2)
NMFC08_TDMft2 <- as.matrix(NMFC08_TDMft)
numRows <- nrow(NMFC08_TDMft2)
numCols <- ncol(NMFC08_TDMft2)
NMFC08_TDMft3 <- NMFC08_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC08_TDMTable <- graph.adjacency(NMFC08_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(NMFC08_TDMTable, vertex.label = V(NMFC08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
NMFC08_TDM.clusterCoef <- transitivity(NMFC08_TDMTable, type="global") #cluster coefficient
NMFC08_TDM.degreeCent <- centralization.degree(NMFC08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_TDMftn <- as.network.matrix(NMFC08_TDMft)
NMFC08_TDM.netDensity <- network.density(NMFC08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_TDM.entropy <- entropy(NMFC08_TDMft) #entropy

NMFC08_TDM.netMx <- cbind(NMFC08_TDM.netMx, NMFC08_TDM.clusterCoef, NMFC08_TDM.degreeCent$centralization,
                          NMFC08_TDM.netDensity, NMFC08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************

round = 8
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
NMFC08_SDg2 <- data.frame(NMFC08_SD)
NMFC08_SDg2 <- NMFC08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_SDg2$player1
player2vector <- NMFC08_SDg2$player2
NMFC08_SDg3 <- NMFC08_SDg2
NMFC08_SDg3$p1inp2vec <- is.element(NMFC08_SDg3$player1, player2vector)
NMFC08_SDg3$p2inp1vec <- is.element(NMFC08_SDg3$player2, player1vector)

addPlayer1 <- NMFC08_SDg3[ which(NMFC08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC08_SDg3[ which(NMFC08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_SDg2 <- rbind(NMFC08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
NMFC08_SDft <- ftable(NMFC08_SDg2$player1, NMFC08_SDg2$player2)
NMFC08_SDft2 <- as.matrix(NMFC08_SDft)
numRows <- nrow(NMFC08_SDft2)
numCols <- ncol(NMFC08_SDft2)
NMFC08_SDft3 <- NMFC08_SDft2[c(2:numRows) , c(2:numCols)]
NMFC08_SDTable <- graph.adjacency(NMFC08_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(NMFC08_SDTable, vertex.label = V(NMFC08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
NMFC08_SD.clusterCoef <- transitivity(NMFC08_SDTable, type="global") #cluster coefficient
NMFC08_SD.degreeCent <- centralization.degree(NMFC08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_SDftn <- as.network.matrix(NMFC08_SDft)
NMFC08_SD.netDensity <- network.density(NMFC08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_SD.entropy <- entropy(NMFC08_SDft) #entropy

NMFC08_SD.netMx <- cbind(NMFC08_SD.netMx, NMFC08_SD.clusterCoef, NMFC08_SD.degreeCent$centralization,
                         NMFC08_SD.netDensity, NMFC08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
NMFC08_TDg2 <- data.frame(NMFC08_TD)
NMFC08_TDg2 <- NMFC08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_TDg2$player1
player2vector <- NMFC08_TDg2$player2
NMFC08_TDg3 <- NMFC08_TDg2
NMFC08_TDg3$p1inp2vec <- is.element(NMFC08_TDg3$player1, player2vector)
NMFC08_TDg3$p2inp1vec <- is.element(NMFC08_TDg3$player2, player1vector)

addPlayer1 <- NMFC08_TDg3[ which(NMFC08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC08_TDg3[ which(NMFC08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_TDg2 <- rbind(NMFC08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
NMFC08_TDft <- ftable(NMFC08_TDg2$player1, NMFC08_TDg2$player2)
NMFC08_TDft2 <- as.matrix(NMFC08_TDft)
numRows <- nrow(NMFC08_TDft2)
numCols <- ncol(NMFC08_TDft2)
NMFC08_TDft3 <- NMFC08_TDft2[c(2:numRows) , c(2:numCols)]
NMFC08_TDTable <- graph.adjacency(NMFC08_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(NMFC08_TDTable, vertex.label = V(NMFC08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
NMFC08_TD.clusterCoef <- transitivity(NMFC08_TDTable, type="global") #cluster coefficient
NMFC08_TD.degreeCent <- centralization.degree(NMFC08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_TDftn <- as.network.matrix(NMFC08_TDft)
NMFC08_TD.netDensity <- network.density(NMFC08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_TD.entropy <- entropy(NMFC08_TDft) #entropy

NMFC08_TD.netMx <- cbind(NMFC08_TD.netMx, NMFC08_TD.clusterCoef, NMFC08_TD.degreeCent$centralization,
                         NMFC08_TD.netDensity, NMFC08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
NMFC08_QTg2 <- data.frame(NMFC08_QT)
NMFC08_QTg2 <- NMFC08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC08_QTg2$player1
player2vector <- NMFC08_QTg2$player2
NMFC08_QTg3 <- NMFC08_QTg2
NMFC08_QTg3$p1inp2vec <- is.element(NMFC08_QTg3$player1, player2vector)
NMFC08_QTg3$p2inp1vec <- is.element(NMFC08_QTg3$player2, player1vector)

addPlayer1 <- NMFC08_QTg3[ which(NMFC08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC08_QTg3[ which(NMFC08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC08_QTg2 <- rbind(NMFC08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
NMFC08_QTft <- ftable(NMFC08_QTg2$player1, NMFC08_QTg2$player2)
NMFC08_QTft2 <- as.matrix(NMFC08_QTft)
numRows <- nrow(NMFC08_QTft2)
numCols <- ncol(NMFC08_QTft2)
NMFC08_QTft3 <- NMFC08_QTft2[c(2:numRows) , c(2:numCols)]
NMFC08_QTTable <- graph.adjacency(NMFC08_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(NMFC08_QTTable, vertex.label = V(NMFC08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
NMFC08_QT.clusterCoef <- transitivity(NMFC08_QTTable, type="global") #cluster coefficient
NMFC08_QT.degreeCent <- centralization.degree(NMFC08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC08_QTftn <- as.network.matrix(NMFC08_QTft)
NMFC08_QT.netDensity <- network.density(NMFC08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC08_QT.entropy <- entropy(NMFC08_QTft) #entropy

NMFC08_QT.netMx <- cbind(NMFC08_QT.netMx, NMFC08_QT.clusterCoef, NMFC08_QT.degreeCent$centralization,
                         NMFC08_QT.netDensity, NMFC08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC08_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "PORT"
KIoutcome = "Goal_F"
PORT08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
PORT08_Gg2 <- data.frame(PORT08_G)
PORT08_Gg2 <- PORT08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_Gg2$player1
player2vector <- PORT08_Gg2$player2
PORT08_Gg3 <- PORT08_Gg2
PORT08_Gg3$p1inp2vec <- is.element(PORT08_Gg3$player1, player2vector)
PORT08_Gg3$p2inp1vec <- is.element(PORT08_Gg3$player2, player1vector)

addPlayer1 <- PORT08_Gg3[ which(PORT08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT08_Gg2 <- rbind(PORT08_Gg2, addPlayer1)

#ROUND 8, Goal graph using weighted edges
PORT08_Gft <- ftable(PORT08_Gg2$player1, PORT08_Gg2$player2)
PORT08_Gft2 <- as.matrix(PORT08_Gft)
numRows <- nrow(PORT08_Gft2)
numCols <- ncol(PORT08_Gft2)
PORT08_Gft3 <- PORT08_Gft2[c(2:numRows) , c(1:numCols)]
PORT08_GTable <- graph.adjacency(PORT08_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(PORT08_GTable, vertex.label = V(PORT08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
PORT08_G.clusterCoef <- transitivity(PORT08_GTable, type="global") #cluster coefficient
PORT08_G.degreeCent <- centralization.degree(PORT08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_Gftn <- as.network.matrix(PORT08_Gft)
PORT08_G.netDensity <- network.density(PORT08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_G.entropy <- entropy(PORT08_Gft) #entropy

PORT08_G.netMx <- cbind(PORT08_G.netMx, PORT08_G.clusterCoef, PORT08_G.degreeCent$centralization,
                        PORT08_G.netDensity, PORT08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "PORT"
KIoutcome = "Behind_F"
PORT08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
PORT08_Bg2 <- data.frame(PORT08_B)
PORT08_Bg2 <- PORT08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_Bg2$player1
player2vector <- PORT08_Bg2$player2
PORT08_Bg3 <- PORT08_Bg2
PORT08_Bg3$p1inp2vec <- is.element(PORT08_Bg3$player1, player2vector)
PORT08_Bg3$p2inp1vec <- is.element(PORT08_Bg3$player2, player1vector)

addPlayer1 <- PORT08_Bg3[ which(PORT08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_Bg3[ which(PORT08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_Bg2 <- rbind(PORT08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
PORT08_Bft <- ftable(PORT08_Bg2$player1, PORT08_Bg2$player2)
PORT08_Bft2 <- as.matrix(PORT08_Bft)
numRows <- nrow(PORT08_Bft2)
numCols <- ncol(PORT08_Bft2)
PORT08_Bft3 <- PORT08_Bft2[c(2:numRows) , c(2:numCols)]
PORT08_BTable <- graph.adjacency(PORT08_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(PORT08_BTable, vertex.label = V(PORT08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
PORT08_B.clusterCoef <- transitivity(PORT08_BTable, type="global") #cluster coefficient
PORT08_B.degreeCent <- centralization.degree(PORT08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_Bftn <- as.network.matrix(PORT08_Bft)
PORT08_B.netDensity <- network.density(PORT08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_B.entropy <- entropy(PORT08_Bft) #entropy

PORT08_B.netMx <- cbind(PORT08_B.netMx, PORT08_B.clusterCoef, PORT08_B.degreeCent$centralization,
                        PORT08_B.netDensity, PORT08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
PORT08_SFg2 <- data.frame(PORT08_SF)
PORT08_SFg2 <- PORT08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_SFg2$player1
player2vector <- PORT08_SFg2$player2
PORT08_SFg3 <- PORT08_SFg2
PORT08_SFg3$p1inp2vec <- is.element(PORT08_SFg3$player1, player2vector)
PORT08_SFg3$p2inp1vec <- is.element(PORT08_SFg3$player2, player1vector)

addPlayer1 <- PORT08_SFg3[ which(PORT08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_SFg3[ which(PORT08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_SFg2 <- rbind(PORT08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
PORT08_SFft <- ftable(PORT08_SFg2$player1, PORT08_SFg2$player2)
PORT08_SFft2 <- as.matrix(PORT08_SFft)
numRows <- nrow(PORT08_SFft2)
numCols <- ncol(PORT08_SFft2)
PORT08_SFft3 <- PORT08_SFft2[c(2:numRows) , c(2:numCols)]
PORT08_SFTable <- graph.adjacency(PORT08_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(PORT08_SFTable, vertex.label = V(PORT08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
PORT08_SF.clusterCoef <- transitivity(PORT08_SFTable, type="global") #cluster coefficient
PORT08_SF.degreeCent <- centralization.degree(PORT08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_SFftn <- as.network.matrix(PORT08_SFft)
PORT08_SF.netDensity <- network.density(PORT08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_SF.entropy <- entropy(PORT08_SFft) #entropy

PORT08_SF.netMx <- cbind(PORT08_SF.netMx, PORT08_SF.clusterCoef, PORT08_SF.degreeCent$centralization,
                         PORT08_SF.netDensity, PORT08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
PORT08_TFg2 <- data.frame(PORT08_TF)
PORT08_TFg2 <- PORT08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_TFg2$player1
player2vector <- PORT08_TFg2$player2
PORT08_TFg3 <- PORT08_TFg2
PORT08_TFg3$p1inp2vec <- is.element(PORT08_TFg3$player1, player2vector)
PORT08_TFg3$p2inp1vec <- is.element(PORT08_TFg3$player2, player1vector)

addPlayer1 <- PORT08_TFg3[ which(PORT08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_TFg3[ which(PORT08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_TFg2 <- rbind(PORT08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
PORT08_TFft <- ftable(PORT08_TFg2$player1, PORT08_TFg2$player2)
PORT08_TFft2 <- as.matrix(PORT08_TFft)
numRows <- nrow(PORT08_TFft2)
numCols <- ncol(PORT08_TFft2)
PORT08_TFft3 <- PORT08_TFft2[c(2:numRows) , c(2:numCols)]
PORT08_TFTable <- graph.adjacency(PORT08_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(PORT08_TFTable, vertex.label = V(PORT08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
PORT08_TF.clusterCoef <- transitivity(PORT08_TFTable, type="global") #cluster coefficient
PORT08_TF.degreeCent <- centralization.degree(PORT08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_TFftn <- as.network.matrix(PORT08_TFft)
PORT08_TF.netDensity <- network.density(PORT08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_TF.entropy <- entropy(PORT08_TFft) #entropy

PORT08_TF.netMx <- cbind(PORT08_TF.netMx, PORT08_TF.clusterCoef, PORT08_TF.degreeCent$centralization,
                         PORT08_TF.netDensity, PORT08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************
#NA

round = 8
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
PORT08_SAMg2 <- data.frame(PORT08_SAM)
PORT08_SAMg2 <- PORT08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_SAMg2$player1
player2vector <- PORT08_SAMg2$player2
PORT08_SAMg3 <- PORT08_SAMg2
PORT08_SAMg3$p1inp2vec <- is.element(PORT08_SAMg3$player1, player2vector)
PORT08_SAMg3$p2inp1vec <- is.element(PORT08_SAMg3$player2, player1vector)

addPlayer1 <- PORT08_SAMg3[ which(PORT08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_SAMg3[ which(PORT08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_SAMg2 <- rbind(PORT08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
PORT08_SAMft <- ftable(PORT08_SAMg2$player1, PORT08_SAMg2$player2)
PORT08_SAMft2 <- as.matrix(PORT08_SAMft)
numRows <- nrow(PORT08_SAMft2)
numCols <- ncol(PORT08_SAMft2)
PORT08_SAMft3 <- PORT08_SAMft2[c(2:numRows) , c(2:numCols)]
PORT08_SAMTable <- graph.adjacency(PORT08_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(PORT08_SAMTable, vertex.label = V(PORT08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
PORT08_SAM.clusterCoef <- transitivity(PORT08_SAMTable, type="global") #cluster coefficient
PORT08_SAM.degreeCent <- centralization.degree(PORT08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_SAMftn <- as.network.matrix(PORT08_SAMft)
PORT08_SAM.netDensity <- network.density(PORT08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_SAM.entropy <- entropy(PORT08_SAMft) #entropy

PORT08_SAM.netMx <- cbind(PORT08_SAM.netMx, PORT08_SAM.clusterCoef, PORT08_SAM.degreeCent$centralization,
                          PORT08_SAM.netDensity, PORT08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
PORT08_TAMg2 <- data.frame(PORT08_TAM)
PORT08_TAMg2 <- PORT08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_TAMg2$player1
player2vector <- PORT08_TAMg2$player2
PORT08_TAMg3 <- PORT08_TAMg2
PORT08_TAMg3$p1inp2vec <- is.element(PORT08_TAMg3$player1, player2vector)
PORT08_TAMg3$p2inp1vec <- is.element(PORT08_TAMg3$player2, player1vector)

addPlayer1 <- PORT08_TAMg3[ which(PORT08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_TAMg3[ which(PORT08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_TAMg2 <- rbind(PORT08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
PORT08_TAMft <- ftable(PORT08_TAMg2$player1, PORT08_TAMg2$player2)
PORT08_TAMft2 <- as.matrix(PORT08_TAMft)
numRows <- nrow(PORT08_TAMft2)
numCols <- ncol(PORT08_TAMft2)
PORT08_TAMft3 <- PORT08_TAMft2[c(2:numRows) , c(2:numCols)]
PORT08_TAMTable <- graph.adjacency(PORT08_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(PORT08_TAMTable, vertex.label = V(PORT08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
PORT08_TAM.clusterCoef <- transitivity(PORT08_TAMTable, type="global") #cluster coefficient
PORT08_TAM.degreeCent <- centralization.degree(PORT08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_TAMftn <- as.network.matrix(PORT08_TAMft)
PORT08_TAM.netDensity <- network.density(PORT08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_TAM.entropy <- entropy(PORT08_TAMft) #entropy

PORT08_TAM.netMx <- cbind(PORT08_TAM.netMx, PORT08_TAM.clusterCoef, PORT08_TAM.degreeCent$centralization,
                          PORT08_TAM.netDensity, PORT08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
PORT08_SDMg2 <- data.frame(PORT08_SDM)
PORT08_SDMg2 <- PORT08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_SDMg2$player1
player2vector <- PORT08_SDMg2$player2
PORT08_SDMg3 <- PORT08_SDMg2
PORT08_SDMg3$p1inp2vec <- is.element(PORT08_SDMg3$player1, player2vector)
PORT08_SDMg3$p2inp1vec <- is.element(PORT08_SDMg3$player2, player1vector)

addPlayer1 <- PORT08_SDMg3[ which(PORT08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_SDMg3[ which(PORT08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_SDMg2 <- rbind(PORT08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
PORT08_SDMft <- ftable(PORT08_SDMg2$player1, PORT08_SDMg2$player2)
PORT08_SDMft2 <- as.matrix(PORT08_SDMft)
numRows <- nrow(PORT08_SDMft2)
numCols <- ncol(PORT08_SDMft2)
PORT08_SDMft3 <- PORT08_SDMft2[c(2:numRows) , c(2:numCols)]
PORT08_SDMTable <- graph.adjacency(PORT08_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(PORT08_SDMTable, vertex.label = V(PORT08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
PORT08_SDM.clusterCoef <- transitivity(PORT08_SDMTable, type="global") #cluster coefficient
PORT08_SDM.degreeCent <- centralization.degree(PORT08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_SDMftn <- as.network.matrix(PORT08_SDMft)
PORT08_SDM.netDensity <- network.density(PORT08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_SDM.entropy <- entropy(PORT08_SDMft) #entropy

PORT08_SDM.netMx <- cbind(PORT08_SDM.netMx, PORT08_SDM.clusterCoef, PORT08_SDM.degreeCent$centralization,
                          PORT08_SDM.netDensity, PORT08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
PORT08_TDMg2 <- data.frame(PORT08_TDM)
PORT08_TDMg2 <- PORT08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_TDMg2$player1
player2vector <- PORT08_TDMg2$player2
PORT08_TDMg3 <- PORT08_TDMg2
PORT08_TDMg3$p1inp2vec <- is.element(PORT08_TDMg3$player1, player2vector)
PORT08_TDMg3$p2inp1vec <- is.element(PORT08_TDMg3$player2, player1vector)

addPlayer1 <- PORT08_TDMg3[ which(PORT08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_TDMg3[ which(PORT08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_TDMg2 <- rbind(PORT08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
PORT08_TDMft <- ftable(PORT08_TDMg2$player1, PORT08_TDMg2$player2)
PORT08_TDMft2 <- as.matrix(PORT08_TDMft)
numRows <- nrow(PORT08_TDMft2)
numCols <- ncol(PORT08_TDMft2)
PORT08_TDMft3 <- PORT08_TDMft2[c(2:numRows) , c(2:numCols)]
PORT08_TDMTable <- graph.adjacency(PORT08_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(PORT08_TDMTable, vertex.label = V(PORT08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
PORT08_TDM.clusterCoef <- transitivity(PORT08_TDMTable, type="global") #cluster coefficient
PORT08_TDM.degreeCent <- centralization.degree(PORT08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_TDMftn <- as.network.matrix(PORT08_TDMft)
PORT08_TDM.netDensity <- network.density(PORT08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_TDM.entropy <- entropy(PORT08_TDMft) #entropy

PORT08_TDM.netMx <- cbind(PORT08_TDM.netMx, PORT08_TDM.clusterCoef, PORT08_TDM.degreeCent$centralization,
                          PORT08_TDM.netDensity, PORT08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
PORT08_SDg2 <- data.frame(PORT08_SD)
PORT08_SDg2 <- PORT08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_SDg2$player1
player2vector <- PORT08_SDg2$player2
PORT08_SDg3 <- PORT08_SDg2
PORT08_SDg3$p1inp2vec <- is.element(PORT08_SDg3$player1, player2vector)
PORT08_SDg3$p2inp1vec <- is.element(PORT08_SDg3$player2, player1vector)

addPlayer1 <- PORT08_SDg3[ which(PORT08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_SDg3[ which(PORT08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_SDg2 <- rbind(PORT08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
PORT08_SDft <- ftable(PORT08_SDg2$player1, PORT08_SDg2$player2)
PORT08_SDft2 <- as.matrix(PORT08_SDft)
numRows <- nrow(PORT08_SDft2)
numCols <- ncol(PORT08_SDft2)
PORT08_SDft3 <- PORT08_SDft2[c(2:numRows) , c(2:numCols)]
PORT08_SDTable <- graph.adjacency(PORT08_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(PORT08_SDTable, vertex.label = V(PORT08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
PORT08_SD.clusterCoef <- transitivity(PORT08_SDTable, type="global") #cluster coefficient
PORT08_SD.degreeCent <- centralization.degree(PORT08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_SDftn <- as.network.matrix(PORT08_SDft)
PORT08_SD.netDensity <- network.density(PORT08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_SD.entropy <- entropy(PORT08_SDft) #entropy

PORT08_SD.netMx <- cbind(PORT08_SD.netMx, PORT08_SD.clusterCoef, PORT08_SD.degreeCent$centralization,
                         PORT08_SD.netDensity, PORT08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************

round = 8
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
PORT08_TDg2 <- data.frame(PORT08_TD)
PORT08_TDg2 <- PORT08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_TDg2$player1
player2vector <- PORT08_TDg2$player2
PORT08_TDg3 <- PORT08_TDg2
PORT08_TDg3$p1inp2vec <- is.element(PORT08_TDg3$player1, player2vector)
PORT08_TDg3$p2inp1vec <- is.element(PORT08_TDg3$player2, player1vector)

addPlayer1 <- PORT08_TDg3[ which(PORT08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_TDg3[ which(PORT08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_TDg2 <- rbind(PORT08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
PORT08_TDft <- ftable(PORT08_TDg2$player1, PORT08_TDg2$player2)
PORT08_TDft2 <- as.matrix(PORT08_TDft)
numRows <- nrow(PORT08_TDft2)
numCols <- ncol(PORT08_TDft2)
PORT08_TDft3 <- PORT08_TDft2[c(2:numRows) , c(2:numCols)]
PORT08_TDTable <- graph.adjacency(PORT08_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(PORT08_TDTable, vertex.label = V(PORT08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
PORT08_TD.clusterCoef <- transitivity(PORT08_TDTable, type="global") #cluster coefficient
PORT08_TD.degreeCent <- centralization.degree(PORT08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_TDftn <- as.network.matrix(PORT08_TDft)
PORT08_TD.netDensity <- network.density(PORT08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_TD.entropy <- entropy(PORT08_TDft) #entropy

PORT08_TD.netMx <- cbind(PORT08_TD.netMx, PORT08_TD.clusterCoef, PORT08_TD.degreeCent$centralization,
                         PORT08_TD.netDensity, PORT08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
PORT08_QTg2 <- data.frame(PORT08_QT)
PORT08_QTg2 <- PORT08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT08_QTg2$player1
player2vector <- PORT08_QTg2$player2
PORT08_QTg3 <- PORT08_QTg2
PORT08_QTg3$p1inp2vec <- is.element(PORT08_QTg3$player1, player2vector)
PORT08_QTg3$p2inp1vec <- is.element(PORT08_QTg3$player2, player1vector)

addPlayer1 <- PORT08_QTg3[ which(PORT08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT08_QTg3[ which(PORT08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT08_QTg2 <- rbind(PORT08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
PORT08_QTft <- ftable(PORT08_QTg2$player1, PORT08_QTg2$player2)
PORT08_QTft2 <- as.matrix(PORT08_QTft)
numRows <- nrow(PORT08_QTft2)
numCols <- ncol(PORT08_QTft2)
PORT08_QTft3 <- PORT08_QTft2[c(2:numRows) , c(2:numCols)]
PORT08_QTTable <- graph.adjacency(PORT08_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(PORT08_QTTable, vertex.label = V(PORT08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
PORT08_QT.clusterCoef <- transitivity(PORT08_QTTable, type="global") #cluster coefficient
PORT08_QT.degreeCent <- centralization.degree(PORT08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT08_QTftn <- as.network.matrix(PORT08_QTft)
PORT08_QT.netDensity <- network.density(PORT08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT08_QT.entropy <- entropy(PORT08_QTft) #entropy

PORT08_QT.netMx <- cbind(PORT08_QT.netMx, PORT08_QT.clusterCoef, PORT08_QT.degreeCent$centralization,
                         PORT08_QT.netDensity, PORT08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT08_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************

round = 8
teamName = "RICH"
KIoutcome = "Goal_F"
RICH08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
RICH08_Gg2 <- data.frame(RICH08_G)
RICH08_Gg2 <- RICH08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_Gg2$player1
player2vector <- RICH08_Gg2$player2
RICH08_Gg3 <- RICH08_Gg2
RICH08_Gg3$p1inp2vec <- is.element(RICH08_Gg3$player1, player2vector)
RICH08_Gg3$p2inp1vec <- is.element(RICH08_Gg3$player2, player1vector)

addPlayer1 <- RICH08_Gg3[ which(RICH08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_Gg3[ which(RICH08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_Gg2 <- rbind(RICH08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
RICH08_Gft <- ftable(RICH08_Gg2$player1, RICH08_Gg2$player2)
RICH08_Gft2 <- as.matrix(RICH08_Gft)
numRows <- nrow(RICH08_Gft2)
numCols <- ncol(RICH08_Gft2)
RICH08_Gft3 <- RICH08_Gft2[c(2:numRows) , c(2:numCols)]
RICH08_GTable <- graph.adjacency(RICH08_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(RICH08_GTable, vertex.label = V(RICH08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
RICH08_G.clusterCoef <- transitivity(RICH08_GTable, type="global") #cluster coefficient
RICH08_G.degreeCent <- centralization.degree(RICH08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_Gftn <- as.network.matrix(RICH08_Gft)
RICH08_G.netDensity <- network.density(RICH08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_G.entropy <- entropy(RICH08_Gft) #entropy

RICH08_G.netMx <- cbind(RICH08_G.netMx, RICH08_G.clusterCoef, RICH08_G.degreeCent$centralization,
                        RICH08_G.netDensity, RICH08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "RICH"
KIoutcome = "Behind_F"
RICH08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
RICH08_Bg2 <- data.frame(RICH08_B)
RICH08_Bg2 <- RICH08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_Bg2$player1
player2vector <- RICH08_Bg2$player2
RICH08_Bg3 <- RICH08_Bg2
RICH08_Bg3$p1inp2vec <- is.element(RICH08_Bg3$player1, player2vector)
RICH08_Bg3$p2inp1vec <- is.element(RICH08_Bg3$player2, player1vector)

addPlayer1 <- RICH08_Bg3[ which(RICH08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_Bg3[ which(RICH08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_Bg2 <- rbind(RICH08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
RICH08_Bft <- ftable(RICH08_Bg2$player1, RICH08_Bg2$player2)
RICH08_Bft2 <- as.matrix(RICH08_Bft)
numRows <- nrow(RICH08_Bft2)
numCols <- ncol(RICH08_Bft2)
RICH08_Bft3 <- RICH08_Bft2[c(2:numRows) , c(2:numCols)]
RICH08_BTable <- graph.adjacency(RICH08_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(RICH08_BTable, vertex.label = V(RICH08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
RICH08_B.clusterCoef <- transitivity(RICH08_BTable, type="global") #cluster coefficient
RICH08_B.degreeCent <- centralization.degree(RICH08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_Bftn <- as.network.matrix(RICH08_Bft)
RICH08_B.netDensity <- network.density(RICH08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_B.entropy <- entropy(RICH08_Bft) #entropy

RICH08_B.netMx <- cbind(RICH08_B.netMx, RICH08_B.clusterCoef, RICH08_B.degreeCent$centralization,
                        RICH08_B.netDensity, RICH08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
RICH08_SFg2 <- data.frame(RICH08_SF)
RICH08_SFg2 <- RICH08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_SFg2$player1
player2vector <- RICH08_SFg2$player2
RICH08_SFg3 <- RICH08_SFg2
RICH08_SFg3$p1inp2vec <- is.element(RICH08_SFg3$player1, player2vector)
RICH08_SFg3$p2inp1vec <- is.element(RICH08_SFg3$player2, player1vector)

addPlayer1 <- RICH08_SFg3[ which(RICH08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_SFg3[ which(RICH08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_SFg2 <- rbind(RICH08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
RICH08_SFft <- ftable(RICH08_SFg2$player1, RICH08_SFg2$player2)
RICH08_SFft2 <- as.matrix(RICH08_SFft)
numRows <- nrow(RICH08_SFft2)
numCols <- ncol(RICH08_SFft2)
RICH08_SFft3 <- RICH08_SFft2[c(2:numRows) , c(2:numCols)]
RICH08_SFTable <- graph.adjacency(RICH08_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(RICH08_SFTable, vertex.label = V(RICH08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
RICH08_SF.clusterCoef <- transitivity(RICH08_SFTable, type="global") #cluster coefficient
RICH08_SF.degreeCent <- centralization.degree(RICH08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_SFftn <- as.network.matrix(RICH08_SFft)
RICH08_SF.netDensity <- network.density(RICH08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_SF.entropy <- entropy(RICH08_SFft) #entropy

RICH08_SF.netMx <- cbind(RICH08_SF.netMx, RICH08_SF.clusterCoef, RICH08_SF.degreeCent$centralization,
                         RICH08_SF.netDensity, RICH08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
RICH08_TFg2 <- data.frame(RICH08_TF)
RICH08_TFg2 <- RICH08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_TFg2$player1
player2vector <- RICH08_TFg2$player2
RICH08_TFg3 <- RICH08_TFg2
RICH08_TFg3$p1inp2vec <- is.element(RICH08_TFg3$player1, player2vector)
RICH08_TFg3$p2inp1vec <- is.element(RICH08_TFg3$player2, player1vector)

addPlayer1 <- RICH08_TFg3[ which(RICH08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_TFg3[ which(RICH08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_TFg2 <- rbind(RICH08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
RICH08_TFft <- ftable(RICH08_TFg2$player1, RICH08_TFg2$player2)
RICH08_TFft2 <- as.matrix(RICH08_TFft)
numRows <- nrow(RICH08_TFft2)
numCols <- ncol(RICH08_TFft2)
RICH08_TFft3 <- RICH08_TFft2[c(2:numRows) , c(2:numCols)]
RICH08_TFTable <- graph.adjacency(RICH08_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(RICH08_TFTable, vertex.label = V(RICH08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
RICH08_TF.clusterCoef <- transitivity(RICH08_TFTable, type="global") #cluster coefficient
RICH08_TF.degreeCent <- centralization.degree(RICH08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_TFftn <- as.network.matrix(RICH08_TFft)
RICH08_TF.netDensity <- network.density(RICH08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_TF.entropy <- entropy(RICH08_TFft) #entropy

RICH08_TF.netMx <- cbind(RICH08_TF.netMx, RICH08_TF.clusterCoef, RICH08_TF.degreeCent$centralization,
                         RICH08_TF.netDensity, RICH08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************

round = 8
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
RICH08_SAMg2 <- data.frame(RICH08_SAM)
RICH08_SAMg2 <- RICH08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_SAMg2$player1
player2vector <- RICH08_SAMg2$player2
RICH08_SAMg3 <- RICH08_SAMg2
RICH08_SAMg3$p1inp2vec <- is.element(RICH08_SAMg3$player1, player2vector)
RICH08_SAMg3$p2inp1vec <- is.element(RICH08_SAMg3$player2, player1vector)

addPlayer1 <- RICH08_SAMg3[ which(RICH08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_SAMg3[ which(RICH08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_SAMg2 <- rbind(RICH08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
RICH08_SAMft <- ftable(RICH08_SAMg2$player1, RICH08_SAMg2$player2)
RICH08_SAMft2 <- as.matrix(RICH08_SAMft)
numRows <- nrow(RICH08_SAMft2)
numCols <- ncol(RICH08_SAMft2)
RICH08_SAMft3 <- RICH08_SAMft2[c(2:numRows) , c(2:numCols)]
RICH08_SAMTable <- graph.adjacency(RICH08_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(RICH08_SAMTable, vertex.label = V(RICH08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
RICH08_SAM.clusterCoef <- transitivity(RICH08_SAMTable, type="global") #cluster coefficient
RICH08_SAM.degreeCent <- centralization.degree(RICH08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_SAMftn <- as.network.matrix(RICH08_SAMft)
RICH08_SAM.netDensity <- network.density(RICH08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_SAM.entropy <- entropy(RICH08_SAMft) #entropy

RICH08_SAM.netMx <- cbind(RICH08_SAM.netMx, RICH08_SAM.clusterCoef, RICH08_SAM.degreeCent$centralization,
                          RICH08_SAM.netDensity, RICH08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************
#NA

round = 8
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
RICH08_TAMg2 <- data.frame(RICH08_TAM)
RICH08_TAMg2 <- RICH08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_TAMg2$player1
player2vector <- RICH08_TAMg2$player2
RICH08_TAMg3 <- RICH08_TAMg2
RICH08_TAMg3$p1inp2vec <- is.element(RICH08_TAMg3$player1, player2vector)
RICH08_TAMg3$p2inp1vec <- is.element(RICH08_TAMg3$player2, player1vector)

addPlayer1 <- RICH08_TAMg3[ which(RICH08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_TAMg3[ which(RICH08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_TAMg2 <- rbind(RICH08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
RICH08_TAMft <- ftable(RICH08_TAMg2$player1, RICH08_TAMg2$player2)
RICH08_TAMft2 <- as.matrix(RICH08_TAMft)
numRows <- nrow(RICH08_TAMft2)
numCols <- ncol(RICH08_TAMft2)
RICH08_TAMft3 <- RICH08_TAMft2[c(2:numRows) , c(2:numCols)]
RICH08_TAMTable <- graph.adjacency(RICH08_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(RICH08_TAMTable, vertex.label = V(RICH08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
RICH08_TAM.clusterCoef <- transitivity(RICH08_TAMTable, type="global") #cluster coefficient
RICH08_TAM.degreeCent <- centralization.degree(RICH08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_TAMftn <- as.network.matrix(RICH08_TAMft)
RICH08_TAM.netDensity <- network.density(RICH08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_TAM.entropy <- entropy(RICH08_TAMft) #entropy

RICH08_TAM.netMx <- cbind(RICH08_TAM.netMx, RICH08_TAM.clusterCoef, RICH08_TAM.degreeCent$centralization,
                          RICH08_TAM.netDensity, RICH08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************
#NA

round = 8
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
RICH08_SDMg2 <- data.frame(RICH08_SDM)
RICH08_SDMg2 <- RICH08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_SDMg2$player1
player2vector <- RICH08_SDMg2$player2
RICH08_SDMg3 <- RICH08_SDMg2
RICH08_SDMg3$p1inp2vec <- is.element(RICH08_SDMg3$player1, player2vector)
RICH08_SDMg3$p2inp1vec <- is.element(RICH08_SDMg3$player2, player1vector)

addPlayer1 <- RICH08_SDMg3[ which(RICH08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_SDMg3[ which(RICH08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_SDMg2 <- rbind(RICH08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
RICH08_SDMft <- ftable(RICH08_SDMg2$player1, RICH08_SDMg2$player2)
RICH08_SDMft2 <- as.matrix(RICH08_SDMft)
numRows <- nrow(RICH08_SDMft2)
numCols <- ncol(RICH08_SDMft2)
RICH08_SDMft3 <- RICH08_SDMft2[c(2:numRows) , c(2:numCols)]
RICH08_SDMTable <- graph.adjacency(RICH08_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(RICH08_SDMTable, vertex.label = V(RICH08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
RICH08_SDM.clusterCoef <- transitivity(RICH08_SDMTable, type="global") #cluster coefficient
RICH08_SDM.degreeCent <- centralization.degree(RICH08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_SDMftn <- as.network.matrix(RICH08_SDMft)
RICH08_SDM.netDensity <- network.density(RICH08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_SDM.entropy <- entropy(RICH08_SDMft) #entropy

RICH08_SDM.netMx <- cbind(RICH08_SDM.netMx, RICH08_SDM.clusterCoef, RICH08_SDM.degreeCent$centralization,
                          RICH08_SDM.netDensity, RICH08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
RICH08_TDMg2 <- data.frame(RICH08_TDM)
RICH08_TDMg2 <- RICH08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_TDMg2$player1
player2vector <- RICH08_TDMg2$player2
RICH08_TDMg3 <- RICH08_TDMg2
RICH08_TDMg3$p1inp2vec <- is.element(RICH08_TDMg3$player1, player2vector)
RICH08_TDMg3$p2inp1vec <- is.element(RICH08_TDMg3$player2, player1vector)

addPlayer1 <- RICH08_TDMg3[ which(RICH08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_TDMg3[ which(RICH08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_TDMg2 <- rbind(RICH08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
RICH08_TDMft <- ftable(RICH08_TDMg2$player1, RICH08_TDMg2$player2)
RICH08_TDMft2 <- as.matrix(RICH08_TDMft)
numRows <- nrow(RICH08_TDMft2)
numCols <- ncol(RICH08_TDMft2)
RICH08_TDMft3 <- RICH08_TDMft2[c(2:numRows) , c(2:numCols)]
RICH08_TDMTable <- graph.adjacency(RICH08_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(RICH08_TDMTable, vertex.label = V(RICH08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
RICH08_TDM.clusterCoef <- transitivity(RICH08_TDMTable, type="global") #cluster coefficient
RICH08_TDM.degreeCent <- centralization.degree(RICH08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_TDMftn <- as.network.matrix(RICH08_TDMft)
RICH08_TDM.netDensity <- network.density(RICH08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_TDM.entropy <- entropy(RICH08_TDMft) #entropy

RICH08_TDM.netMx <- cbind(RICH08_TDM.netMx, RICH08_TDM.clusterCoef, RICH08_TDM.degreeCent$centralization,
                          RICH08_TDM.netDensity, RICH08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************

round = 8
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
RICH08_SDg2 <- data.frame(RICH08_SD)
RICH08_SDg2 <- RICH08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_SDg2$player1
player2vector <- RICH08_SDg2$player2
RICH08_SDg3 <- RICH08_SDg2
RICH08_SDg3$p1inp2vec <- is.element(RICH08_SDg3$player1, player2vector)
RICH08_SDg3$p2inp1vec <- is.element(RICH08_SDg3$player2, player1vector)

addPlayer1 <- RICH08_SDg3[ which(RICH08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_SDg3[ which(RICH08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_SDg2 <- rbind(RICH08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
RICH08_SDft <- ftable(RICH08_SDg2$player1, RICH08_SDg2$player2)
RICH08_SDft2 <- as.matrix(RICH08_SDft)
numRows <- nrow(RICH08_SDft2)
numCols <- ncol(RICH08_SDft2)
RICH08_SDft3 <- RICH08_SDft2[c(2:numRows) , c(2:numCols)]
RICH08_SDTable <- graph.adjacency(RICH08_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(RICH08_SDTable, vertex.label = V(RICH08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
RICH08_SD.clusterCoef <- transitivity(RICH08_SDTable, type="global") #cluster coefficient
RICH08_SD.degreeCent <- centralization.degree(RICH08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_SDftn <- as.network.matrix(RICH08_SDft)
RICH08_SD.netDensity <- network.density(RICH08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_SD.entropy <- entropy(RICH08_SDft) #entropy

RICH08_SD.netMx <- cbind(RICH08_SD.netMx, RICH08_SD.clusterCoef, RICH08_SD.degreeCent$centralization,
                         RICH08_SD.netDensity, RICH08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
RICH08_TDg2 <- data.frame(RICH08_TD)
RICH08_TDg2 <- RICH08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_TDg2$player1
player2vector <- RICH08_TDg2$player2
RICH08_TDg3 <- RICH08_TDg2
RICH08_TDg3$p1inp2vec <- is.element(RICH08_TDg3$player1, player2vector)
RICH08_TDg3$p2inp1vec <- is.element(RICH08_TDg3$player2, player1vector)

addPlayer1 <- RICH08_TDg3[ which(RICH08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_TDg3[ which(RICH08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_TDg2 <- rbind(RICH08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
RICH08_TDft <- ftable(RICH08_TDg2$player1, RICH08_TDg2$player2)
RICH08_TDft2 <- as.matrix(RICH08_TDft)
numRows <- nrow(RICH08_TDft2)
numCols <- ncol(RICH08_TDft2)
RICH08_TDft3 <- RICH08_TDft2[c(2:numRows) , c(2:numCols)]
RICH08_TDTable <- graph.adjacency(RICH08_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(RICH08_TDTable, vertex.label = V(RICH08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
RICH08_TD.clusterCoef <- transitivity(RICH08_TDTable, type="global") #cluster coefficient
RICH08_TD.degreeCent <- centralization.degree(RICH08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_TDftn <- as.network.matrix(RICH08_TDft)
RICH08_TD.netDensity <- network.density(RICH08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_TD.entropy <- entropy(RICH08_TDft) #entropy

RICH08_TD.netMx <- cbind(RICH08_TD.netMx, RICH08_TD.clusterCoef, RICH08_TD.degreeCent$centralization,
                         RICH08_TD.netDensity, RICH08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
RICH08_QTg2 <- data.frame(RICH08_QT)
RICH08_QTg2 <- RICH08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH08_QTg2$player1
player2vector <- RICH08_QTg2$player2
RICH08_QTg3 <- RICH08_QTg2
RICH08_QTg3$p1inp2vec <- is.element(RICH08_QTg3$player1, player2vector)
RICH08_QTg3$p2inp1vec <- is.element(RICH08_QTg3$player2, player1vector)

addPlayer1 <- RICH08_QTg3[ which(RICH08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH08_QTg3[ which(RICH08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH08_QTg2 <- rbind(RICH08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
RICH08_QTft <- ftable(RICH08_QTg2$player1, RICH08_QTg2$player2)
RICH08_QTft2 <- as.matrix(RICH08_QTft)
numRows <- nrow(RICH08_QTft2)
numCols <- ncol(RICH08_QTft2)
RICH08_QTft3 <- RICH08_QTft2[c(2:numRows) , c(2:numCols)]
RICH08_QTTable <- graph.adjacency(RICH08_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(RICH08_QTTable, vertex.label = V(RICH08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
RICH08_QT.clusterCoef <- transitivity(RICH08_QTTable, type="global") #cluster coefficient
RICH08_QT.degreeCent <- centralization.degree(RICH08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH08_QTftn <- as.network.matrix(RICH08_QTft)
RICH08_QT.netDensity <- network.density(RICH08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH08_QT.entropy <- entropy(RICH08_QTft) #entropy

RICH08_QT.netMx <- cbind(RICH08_QT.netMx, RICH08_QT.clusterCoef, RICH08_QT.degreeCent$centralization,
                         RICH08_QT.netDensity, RICH08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH08_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "STK"
KIoutcome = "Goal_F"
STK08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
STK08_Gg2 <- data.frame(STK08_G)
STK08_Gg2 <- STK08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_Gg2$player1
player2vector <- STK08_Gg2$player2
STK08_Gg3 <- STK08_Gg2
STK08_Gg3$p1inp2vec <- is.element(STK08_Gg3$player1, player2vector)
STK08_Gg3$p2inp1vec <- is.element(STK08_Gg3$player2, player1vector)

addPlayer1 <- STK08_Gg3[ which(STK08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_Gg3[ which(STK08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_Gg2 <- rbind(STK08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
STK08_Gft <- ftable(STK08_Gg2$player1, STK08_Gg2$player2)
STK08_Gft2 <- as.matrix(STK08_Gft)
numRows <- nrow(STK08_Gft2)
numCols <- ncol(STK08_Gft2)
STK08_Gft3 <- STK08_Gft2[c(2:numRows) , c(2:numCols)]
STK08_GTable <- graph.adjacency(STK08_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(STK08_GTable, vertex.label = V(STK08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
STK08_G.clusterCoef <- transitivity(STK08_GTable, type="global") #cluster coefficient
STK08_G.degreeCent <- centralization.degree(STK08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_Gftn <- as.network.matrix(STK08_Gft)
STK08_G.netDensity <- network.density(STK08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_G.entropy <- entropy(STK08_Gft) #entropy

STK08_G.netMx <- cbind(STK08_G.netMx, STK08_G.clusterCoef, STK08_G.degreeCent$centralization,
                       STK08_G.netDensity, STK08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "STK"
KIoutcome = "Behind_F"
STK08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
STK08_Bg2 <- data.frame(STK08_B)
STK08_Bg2 <- STK08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_Bg2$player1
player2vector <- STK08_Bg2$player2
STK08_Bg3 <- STK08_Bg2
STK08_Bg3$p1inp2vec <- is.element(STK08_Bg3$player1, player2vector)
STK08_Bg3$p2inp1vec <- is.element(STK08_Bg3$player2, player1vector)

addPlayer1 <- STK08_Bg3[ which(STK08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_Bg3[ which(STK08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_Bg2 <- rbind(STK08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
STK08_Bft <- ftable(STK08_Bg2$player1, STK08_Bg2$player2)
STK08_Bft2 <- as.matrix(STK08_Bft)
numRows <- nrow(STK08_Bft2)
numCols <- ncol(STK08_Bft2)
STK08_Bft3 <- STK08_Bft2[c(2:numRows) , c(2:numCols)]
STK08_BTable <- graph.adjacency(STK08_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(STK08_BTable, vertex.label = V(STK08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
STK08_B.clusterCoef <- transitivity(STK08_BTable, type="global") #cluster coefficient
STK08_B.degreeCent <- centralization.degree(STK08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_Bftn <- as.network.matrix(STK08_Bft)
STK08_B.netDensity <- network.density(STK08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_B.entropy <- entropy(STK08_Bft) #entropy

STK08_B.netMx <- cbind(STK08_B.netMx, STK08_B.clusterCoef, STK08_B.degreeCent$centralization,
                       STK08_B.netDensity, STK08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************

round = 8
teamName = "STK"
KIoutcome = "Stoppage_F"
STK08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
STK08_SFg2 <- data.frame(STK08_SF)
STK08_SFg2 <- STK08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_SFg2$player1
player2vector <- STK08_SFg2$player2
STK08_SFg3 <- STK08_SFg2
STK08_SFg3$p1inp2vec <- is.element(STK08_SFg3$player1, player2vector)
STK08_SFg3$p2inp1vec <- is.element(STK08_SFg3$player2, player1vector)

addPlayer1 <- STK08_SFg3[ which(STK08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_SFg3[ which(STK08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_SFg2 <- rbind(STK08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
STK08_SFft <- ftable(STK08_SFg2$player1, STK08_SFg2$player2)
STK08_SFft2 <- as.matrix(STK08_SFft)
numRows <- nrow(STK08_SFft2)
numCols <- ncol(STK08_SFft2)
STK08_SFft3 <- STK08_SFft2[c(2:numRows) , c(2:numCols)]
STK08_SFTable <- graph.adjacency(STK08_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(STK08_SFTable, vertex.label = V(STK08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
STK08_SF.clusterCoef <- transitivity(STK08_SFTable, type="global") #cluster coefficient
STK08_SF.degreeCent <- centralization.degree(STK08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_SFftn <- as.network.matrix(STK08_SFft)
STK08_SF.netDensity <- network.density(STK08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_SF.entropy <- entropy(STK08_SFft) #entropy

STK08_SF.netMx <- cbind(STK08_SF.netMx, STK08_SF.clusterCoef, STK08_SF.degreeCent$centralization,
                        STK08_SF.netDensity, STK08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************
#NA

round = 8
teamName = "STK"
KIoutcome = "Turnover_F"
STK08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
STK08_TFg2 <- data.frame(STK08_TF)
STK08_TFg2 <- STK08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_TFg2$player1
player2vector <- STK08_TFg2$player2
STK08_TFg3 <- STK08_TFg2
STK08_TFg3$p1inp2vec <- is.element(STK08_TFg3$player1, player2vector)
STK08_TFg3$p2inp1vec <- is.element(STK08_TFg3$player2, player1vector)

addPlayer1 <- STK08_TFg3[ which(STK08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_TFg3[ which(STK08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_TFg2 <- rbind(STK08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
STK08_TFft <- ftable(STK08_TFg2$player1, STK08_TFg2$player2)
STK08_TFft2 <- as.matrix(STK08_TFft)
numRows <- nrow(STK08_TFft2)
numCols <- ncol(STK08_TFft2)
STK08_TFft3 <- STK08_TFft2[c(2:numRows) , c(2:numCols)]
STK08_TFTable <- graph.adjacency(STK08_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(STK08_TFTable, vertex.label = V(STK08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
STK08_TF.clusterCoef <- transitivity(STK08_TFTable, type="global") #cluster coefficient
STK08_TF.degreeCent <- centralization.degree(STK08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_TFftn <- as.network.matrix(STK08_TFft)
STK08_TF.netDensity <- network.density(STK08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_TF.entropy <- entropy(STK08_TFft) #entropy

STK08_TF.netMx <- cbind(STK08_TF.netMx, STK08_TF.clusterCoef, STK08_TF.degreeCent$centralization,
                        STK08_TF.netDensity, STK08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************

round = 8
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
STK08_SAMg2 <- data.frame(STK08_SAM)
STK08_SAMg2 <- STK08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_SAMg2$player1
player2vector <- STK08_SAMg2$player2
STK08_SAMg3 <- STK08_SAMg2
STK08_SAMg3$p1inp2vec <- is.element(STK08_SAMg3$player1, player2vector)
STK08_SAMg3$p2inp1vec <- is.element(STK08_SAMg3$player2, player1vector)

addPlayer1 <- STK08_SAMg3[ which(STK08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_SAMg3[ which(STK08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_SAMg2 <- rbind(STK08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
STK08_SAMft <- ftable(STK08_SAMg2$player1, STK08_SAMg2$player2)
STK08_SAMft2 <- as.matrix(STK08_SAMft)
numRows <- nrow(STK08_SAMft2)
numCols <- ncol(STK08_SAMft2)
STK08_SAMft3 <- STK08_SAMft2[c(2:numRows) , c(2:numCols)]
STK08_SAMTable <- graph.adjacency(STK08_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(STK08_SAMTable, vertex.label = V(STK08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
STK08_SAM.clusterCoef <- transitivity(STK08_SAMTable, type="global") #cluster coefficient
STK08_SAM.degreeCent <- centralization.degree(STK08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_SAMftn <- as.network.matrix(STK08_SAMft)
STK08_SAM.netDensity <- network.density(STK08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_SAM.entropy <- entropy(STK08_SAMft) #entropy

STK08_SAM.netMx <- cbind(STK08_SAM.netMx, STK08_SAM.clusterCoef, STK08_SAM.degreeCent$centralization,
                         STK08_SAM.netDensity, STK08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "STK"
KIoutcome = "Turnover_AM"
STK08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
STK08_TAMg2 <- data.frame(STK08_TAM)
STK08_TAMg2 <- STK08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_TAMg2$player1
player2vector <- STK08_TAMg2$player2
STK08_TAMg3 <- STK08_TAMg2
STK08_TAMg3$p1inp2vec <- is.element(STK08_TAMg3$player1, player2vector)
STK08_TAMg3$p2inp1vec <- is.element(STK08_TAMg3$player2, player1vector)

addPlayer1 <- STK08_TAMg3[ which(STK08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_TAMg3[ which(STK08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_TAMg2 <- rbind(STK08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
STK08_TAMft <- ftable(STK08_TAMg2$player1, STK08_TAMg2$player2)
STK08_TAMft2 <- as.matrix(STK08_TAMft)
numRows <- nrow(STK08_TAMft2)
numCols <- ncol(STK08_TAMft2)
STK08_TAMft3 <- STK08_TAMft2[c(2:numRows) , c(2:numCols)]
STK08_TAMTable <- graph.adjacency(STK08_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(STK08_TAMTable, vertex.label = V(STK08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
STK08_TAM.clusterCoef <- transitivity(STK08_TAMTable, type="global") #cluster coefficient
STK08_TAM.degreeCent <- centralization.degree(STK08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_TAMftn <- as.network.matrix(STK08_TAMft)
STK08_TAM.netDensity <- network.density(STK08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_TAM.entropy <- entropy(STK08_TAMft) #entropy

STK08_TAM.netMx <- cbind(STK08_TAM.netMx, STK08_TAM.clusterCoef, STK08_TAM.degreeCent$centralization,
                         STK08_TAM.netDensity, STK08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************
#NA

round = 8
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
STK08_SDMg2 <- data.frame(STK08_SDM)
STK08_SDMg2 <- STK08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_SDMg2$player1
player2vector <- STK08_SDMg2$player2
STK08_SDMg3 <- STK08_SDMg2
STK08_SDMg3$p1inp2vec <- is.element(STK08_SDMg3$player1, player2vector)
STK08_SDMg3$p2inp1vec <- is.element(STK08_SDMg3$player2, player1vector)

addPlayer1 <- STK08_SDMg3[ which(STK08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_SDMg3[ which(STK08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_SDMg2 <- rbind(STK08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
STK08_SDMft <- ftable(STK08_SDMg2$player1, STK08_SDMg2$player2)
STK08_SDMft2 <- as.matrix(STK08_SDMft)
numRows <- nrow(STK08_SDMft2)
numCols <- ncol(STK08_SDMft2)
STK08_SDMft3 <- STK08_SDMft2[c(2:numRows) , c(2:numCols)]
STK08_SDMTable <- graph.adjacency(STK08_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(STK08_SDMTable, vertex.label = V(STK08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
STK08_SDM.clusterCoef <- transitivity(STK08_SDMTable, type="global") #cluster coefficient
STK08_SDM.degreeCent <- centralization.degree(STK08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_SDMftn <- as.network.matrix(STK08_SDMft)
STK08_SDM.netDensity <- network.density(STK08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_SDM.entropy <- entropy(STK08_SDMft) #entropy

STK08_SDM.netMx <- cbind(STK08_SDM.netMx, STK08_SDM.clusterCoef, STK08_SDM.degreeCent$centralization,
                         STK08_SDM.netDensity, STK08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "STK"
KIoutcome = "Turnover_DM"
STK08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
STK08_TDMg2 <- data.frame(STK08_TDM)
STK08_TDMg2 <- STK08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_TDMg2$player1
player2vector <- STK08_TDMg2$player2
STK08_TDMg3 <- STK08_TDMg2
STK08_TDMg3$p1inp2vec <- is.element(STK08_TDMg3$player1, player2vector)
STK08_TDMg3$p2inp1vec <- is.element(STK08_TDMg3$player2, player1vector)

addPlayer1 <- STK08_TDMg3[ which(STK08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_TDMg3[ which(STK08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_TDMg2 <- rbind(STK08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
STK08_TDMft <- ftable(STK08_TDMg2$player1, STK08_TDMg2$player2)
STK08_TDMft2 <- as.matrix(STK08_TDMft)
numRows <- nrow(STK08_TDMft2)
numCols <- ncol(STK08_TDMft2)
STK08_TDMft3 <- STK08_TDMft2[c(2:numRows) , c(2:numCols)]
STK08_TDMTable <- graph.adjacency(STK08_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(STK08_TDMTable, vertex.label = V(STK08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
STK08_TDM.clusterCoef <- transitivity(STK08_TDMTable, type="global") #cluster coefficient
STK08_TDM.degreeCent <- centralization.degree(STK08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_TDMftn <- as.network.matrix(STK08_TDMft)
STK08_TDM.netDensity <- network.density(STK08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_TDM.entropy <- entropy(STK08_TDMft) #entropy

STK08_TDM.netMx <- cbind(STK08_TDM.netMx, STK08_TDM.clusterCoef, STK08_TDM.degreeCent$centralization,
                         STK08_TDM.netDensity, STK08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************

round = 8
teamName = "STK"
KIoutcome = "Stoppage_D"
STK08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
STK08_SDg2 <- data.frame(STK08_SD)
STK08_SDg2 <- STK08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_SDg2$player1
player2vector <- STK08_SDg2$player2
STK08_SDg3 <- STK08_SDg2
STK08_SDg3$p1inp2vec <- is.element(STK08_SDg3$player1, player2vector)
STK08_SDg3$p2inp1vec <- is.element(STK08_SDg3$player2, player1vector)

addPlayer1 <- STK08_SDg3[ which(STK08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_SDg3[ which(STK08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_SDg2 <- rbind(STK08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
STK08_SDft <- ftable(STK08_SDg2$player1, STK08_SDg2$player2)
STK08_SDft2 <- as.matrix(STK08_SDft)
numRows <- nrow(STK08_SDft2)
numCols <- ncol(STK08_SDft2)
STK08_SDft3 <- STK08_SDft2[c(2:numRows) , c(2:numCols)]
STK08_SDTable <- graph.adjacency(STK08_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(STK08_SDTable, vertex.label = V(STK08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
STK08_SD.clusterCoef <- transitivity(STK08_SDTable, type="global") #cluster coefficient
STK08_SD.degreeCent <- centralization.degree(STK08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_SDftn <- as.network.matrix(STK08_SDft)
STK08_SD.netDensity <- network.density(STK08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_SD.entropy <- entropy(STK08_SDft) #entropy

STK08_SD.netMx <- cbind(STK08_SD.netMx, STK08_SD.clusterCoef, STK08_SD.degreeCent$centralization,
                        STK08_SD.netDensity, STK08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************

round = 8
teamName = "STK"
KIoutcome = "Turnover_D"
STK08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
STK08_TDg2 <- data.frame(STK08_TD)
STK08_TDg2 <- STK08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_TDg2$player1
player2vector <- STK08_TDg2$player2
STK08_TDg3 <- STK08_TDg2
STK08_TDg3$p1inp2vec <- is.element(STK08_TDg3$player1, player2vector)
STK08_TDg3$p2inp1vec <- is.element(STK08_TDg3$player2, player1vector)

addPlayer1 <- STK08_TDg3[ which(STK08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_TDg3[ which(STK08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_TDg2 <- rbind(STK08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
STK08_TDft <- ftable(STK08_TDg2$player1, STK08_TDg2$player2)
STK08_TDft2 <- as.matrix(STK08_TDft)
numRows <- nrow(STK08_TDft2)
numCols <- ncol(STK08_TDft2)
STK08_TDft3 <- STK08_TDft2[c(2:numRows) , c(2:numCols)]
STK08_TDTable <- graph.adjacency(STK08_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(STK08_TDTable, vertex.label = V(STK08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
STK08_TD.clusterCoef <- transitivity(STK08_TDTable, type="global") #cluster coefficient
STK08_TD.degreeCent <- centralization.degree(STK08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_TDftn <- as.network.matrix(STK08_TDft)
STK08_TD.netDensity <- network.density(STK08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_TD.entropy <- entropy(STK08_TDft) #entropy

STK08_TD.netMx <- cbind(STK08_TD.netMx, STK08_TD.clusterCoef, STK08_TD.degreeCent$centralization,
                        STK08_TD.netDensity, STK08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************

round = 8
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
STK08_QTg2 <- data.frame(STK08_QT)
STK08_QTg2 <- STK08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK08_QTg2$player1
player2vector <- STK08_QTg2$player2
STK08_QTg3 <- STK08_QTg2
STK08_QTg3$p1inp2vec <- is.element(STK08_QTg3$player1, player2vector)
STK08_QTg3$p2inp1vec <- is.element(STK08_QTg3$player2, player1vector)

addPlayer1 <- STK08_QTg3[ which(STK08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK08_QTg3[ which(STK08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK08_QTg2 <- rbind(STK08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
STK08_QTft <- ftable(STK08_QTg2$player1, STK08_QTg2$player2)
STK08_QTft2 <- as.matrix(STK08_QTft)
numRows <- nrow(STK08_QTft2)
numCols <- ncol(STK08_QTft2)
STK08_QTft3 <- STK08_QTft2[c(2:numRows) , c(2:numCols)]
STK08_QTTable <- graph.adjacency(STK08_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(STK08_QTTable, vertex.label = V(STK08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
STK08_QT.clusterCoef <- transitivity(STK08_QTTable, type="global") #cluster coefficient
STK08_QT.degreeCent <- centralization.degree(STK08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK08_QTftn <- as.network.matrix(STK08_QTft)
STK08_QT.netDensity <- network.density(STK08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK08_QT.entropy <- entropy(STK08_QTft) #entropy

STK08_QT.netMx <- cbind(STK08_QT.netMx, STK08_QT.clusterCoef, STK08_QT.degreeCent$centralization,
                        STK08_QT.netDensity, STK08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK08_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "SYD"
KIoutcome = "Goal_F"
SYD08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
SYD08_Gg2 <- data.frame(SYD08_G)
SYD08_Gg2 <- SYD08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_Gg2$player1
player2vector <- SYD08_Gg2$player2
SYD08_Gg3 <- SYD08_Gg2
SYD08_Gg3$p1inp2vec <- is.element(SYD08_Gg3$player1, player2vector)
SYD08_Gg3$p2inp1vec <- is.element(SYD08_Gg3$player2, player1vector)

addPlayer1 <- SYD08_Gg3[ which(SYD08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_Gg3[ which(SYD08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_Gg2 <- rbind(SYD08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
SYD08_Gft <- ftable(SYD08_Gg2$player1, SYD08_Gg2$player2)
SYD08_Gft2 <- as.matrix(SYD08_Gft)
numRows <- nrow(SYD08_Gft2)
numCols <- ncol(SYD08_Gft2)
SYD08_Gft3 <- SYD08_Gft2[c(2:numRows) , c(2:numCols)]
SYD08_GTable <- graph.adjacency(SYD08_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(SYD08_GTable, vertex.label = V(SYD08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
SYD08_G.clusterCoef <- transitivity(SYD08_GTable, type="global") #cluster coefficient
SYD08_G.degreeCent <- centralization.degree(SYD08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_Gftn <- as.network.matrix(SYD08_Gft)
SYD08_G.netDensity <- network.density(SYD08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_G.entropy <- entropy(SYD08_Gft) #entropy

SYD08_G.netMx <- cbind(SYD08_G.netMx, SYD08_G.clusterCoef, SYD08_G.degreeCent$centralization,
                       SYD08_G.netDensity, SYD08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "SYD"
KIoutcome = "Behind_F"
SYD08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
SYD08_Bg2 <- data.frame(SYD08_B)
SYD08_Bg2 <- SYD08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_Bg2$player1
player2vector <- SYD08_Bg2$player2
SYD08_Bg3 <- SYD08_Bg2
SYD08_Bg3$p1inp2vec <- is.element(SYD08_Bg3$player1, player2vector)
SYD08_Bg3$p2inp1vec <- is.element(SYD08_Bg3$player2, player1vector)

addPlayer1 <- SYD08_Bg3[ which(SYD08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_Bg3[ which(SYD08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_Bg2 <- rbind(SYD08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
SYD08_Bft <- ftable(SYD08_Bg2$player1, SYD08_Bg2$player2)
SYD08_Bft2 <- as.matrix(SYD08_Bft)
numRows <- nrow(SYD08_Bft2)
numCols <- ncol(SYD08_Bft2)
SYD08_Bft3 <- SYD08_Bft2[c(2:numRows) , c(2:numCols)]
SYD08_BTable <- graph.adjacency(SYD08_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(SYD08_BTable, vertex.label = V(SYD08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
SYD08_B.clusterCoef <- transitivity(SYD08_BTable, type="global") #cluster coefficient
SYD08_B.degreeCent <- centralization.degree(SYD08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_Bftn <- as.network.matrix(SYD08_Bft)
SYD08_B.netDensity <- network.density(SYD08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_B.entropy <- entropy(SYD08_Bft) #entropy

SYD08_B.netMx <- cbind(SYD08_B.netMx, SYD08_B.clusterCoef, SYD08_B.degreeCent$centralization,
                       SYD08_B.netDensity, SYD08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************

round = 8
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
SYD08_SFg2 <- data.frame(SYD08_SF)
SYD08_SFg2 <- SYD08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_SFg2$player1
player2vector <- SYD08_SFg2$player2
SYD08_SFg3 <- SYD08_SFg2
SYD08_SFg3$p1inp2vec <- is.element(SYD08_SFg3$player1, player2vector)
SYD08_SFg3$p2inp1vec <- is.element(SYD08_SFg3$player2, player1vector)

addPlayer1 <- SYD08_SFg3[ which(SYD08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_SFg3[ which(SYD08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_SFg2 <- rbind(SYD08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
SYD08_SFft <- ftable(SYD08_SFg2$player1, SYD08_SFg2$player2)
SYD08_SFft2 <- as.matrix(SYD08_SFft)
numRows <- nrow(SYD08_SFft2)
numCols <- ncol(SYD08_SFft2)
SYD08_SFft3 <- SYD08_SFft2[c(2:numRows) , c(2:numCols)]
SYD08_SFTable <- graph.adjacency(SYD08_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(SYD08_SFTable, vertex.label = V(SYD08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
SYD08_SF.clusterCoef <- transitivity(SYD08_SFTable, type="global") #cluster coefficient
SYD08_SF.degreeCent <- centralization.degree(SYD08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_SFftn <- as.network.matrix(SYD08_SFft)
SYD08_SF.netDensity <- network.density(SYD08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_SF.entropy <- entropy(SYD08_SFft) #entropy

SYD08_SF.netMx <- cbind(SYD08_SF.netMx, SYD08_SF.clusterCoef, SYD08_SF.degreeCent$centralization,
                        SYD08_SF.netDensity, SYD08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************
#NA

round = 8
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
SYD08_TFg2 <- data.frame(SYD08_TF)
SYD08_TFg2 <- SYD08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_TFg2$player1
player2vector <- SYD08_TFg2$player2
SYD08_TFg3 <- SYD08_TFg2
SYD08_TFg3$p1inp2vec <- is.element(SYD08_TFg3$player1, player2vector)
SYD08_TFg3$p2inp1vec <- is.element(SYD08_TFg3$player2, player1vector)

addPlayer1 <- SYD08_TFg3[ which(SYD08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_TFg3[ which(SYD08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_TFg2 <- rbind(SYD08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
SYD08_TFft <- ftable(SYD08_TFg2$player1, SYD08_TFg2$player2)
SYD08_TFft2 <- as.matrix(SYD08_TFft)
numRows <- nrow(SYD08_TFft2)
numCols <- ncol(SYD08_TFft2)
SYD08_TFft3 <- SYD08_TFft2[c(2:numRows) , c(2:numCols)]
SYD08_TFTable <- graph.adjacency(SYD08_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(SYD08_TFTable, vertex.label = V(SYD08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
SYD08_TF.clusterCoef <- transitivity(SYD08_TFTable, type="global") #cluster coefficient
SYD08_TF.degreeCent <- centralization.degree(SYD08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_TFftn <- as.network.matrix(SYD08_TFft)
SYD08_TF.netDensity <- network.density(SYD08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_TF.entropy <- entropy(SYD08_TFft) #entropy

SYD08_TF.netMx <- cbind(SYD08_TF.netMx, SYD08_TF.clusterCoef, SYD08_TF.degreeCent$centralization,
                        SYD08_TF.netDensity, SYD08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************

round = 8
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
SYD08_SAMg2 <- data.frame(SYD08_SAM)
SYD08_SAMg2 <- SYD08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_SAMg2$player1
player2vector <- SYD08_SAMg2$player2
SYD08_SAMg3 <- SYD08_SAMg2
SYD08_SAMg3$p1inp2vec <- is.element(SYD08_SAMg3$player1, player2vector)
SYD08_SAMg3$p2inp1vec <- is.element(SYD08_SAMg3$player2, player1vector)

addPlayer1 <- SYD08_SAMg3[ which(SYD08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_SAMg3[ which(SYD08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_SAMg2 <- rbind(SYD08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
SYD08_SAMft <- ftable(SYD08_SAMg2$player1, SYD08_SAMg2$player2)
SYD08_SAMft2 <- as.matrix(SYD08_SAMft)
numRows <- nrow(SYD08_SAMft2)
numCols <- ncol(SYD08_SAMft2)
SYD08_SAMft3 <- SYD08_SAMft2[c(2:numRows) , c(2:numCols)]
SYD08_SAMTable <- graph.adjacency(SYD08_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(SYD08_SAMTable, vertex.label = V(SYD08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
SYD08_SAM.clusterCoef <- transitivity(SYD08_SAMTable, type="global") #cluster coefficient
SYD08_SAM.degreeCent <- centralization.degree(SYD08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_SAMftn <- as.network.matrix(SYD08_SAMft)
SYD08_SAM.netDensity <- network.density(SYD08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_SAM.entropy <- entropy(SYD08_SAMft) #entropy

SYD08_SAM.netMx <- cbind(SYD08_SAM.netMx, SYD08_SAM.clusterCoef, SYD08_SAM.degreeCent$centralization,
                         SYD08_SAM.netDensity, SYD08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
SYD08_TAMg2 <- data.frame(SYD08_TAM)
SYD08_TAMg2 <- SYD08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_TAMg2$player1
player2vector <- SYD08_TAMg2$player2
SYD08_TAMg3 <- SYD08_TAMg2
SYD08_TAMg3$p1inp2vec <- is.element(SYD08_TAMg3$player1, player2vector)
SYD08_TAMg3$p2inp1vec <- is.element(SYD08_TAMg3$player2, player1vector)

addPlayer1 <- SYD08_TAMg3[ which(SYD08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- SYD08_TAMg3[ which(SYD08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_TAMg2 <- rbind(SYD08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
SYD08_TAMft <- ftable(SYD08_TAMg2$player1, SYD08_TAMg2$player2)
SYD08_TAMft2 <- as.matrix(SYD08_TAMft)
numRows <- nrow(SYD08_TAMft2)
numCols <- ncol(SYD08_TAMft2)
SYD08_TAMft3 <- SYD08_TAMft2[c(2:numRows) , c(2:numCols)]
SYD08_TAMTable <- graph.adjacency(SYD08_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(SYD08_TAMTable, vertex.label = V(SYD08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
SYD08_TAM.clusterCoef <- transitivity(SYD08_TAMTable, type="global") #cluster coefficient
SYD08_TAM.degreeCent <- centralization.degree(SYD08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_TAMftn <- as.network.matrix(SYD08_TAMft)
SYD08_TAM.netDensity <- network.density(SYD08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_TAM.entropy <- entropy(SYD08_TAMft) #entropy

SYD08_TAM.netMx <- cbind(SYD08_TAM.netMx, SYD08_TAM.clusterCoef, SYD08_TAM.degreeCent$centralization,
                         SYD08_TAM.netDensity, SYD08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
SYD08_SDMg2 <- data.frame(SYD08_SDM)
SYD08_SDMg2 <- SYD08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_SDMg2$player1
player2vector <- SYD08_SDMg2$player2
SYD08_SDMg3 <- SYD08_SDMg2
SYD08_SDMg3$p1inp2vec <- is.element(SYD08_SDMg3$player1, player2vector)
SYD08_SDMg3$p2inp1vec <- is.element(SYD08_SDMg3$player2, player1vector)

addPlayer1 <- SYD08_SDMg3[ which(SYD08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_SDMg3[ which(SYD08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_SDMg2 <- rbind(SYD08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
SYD08_SDMft <- ftable(SYD08_SDMg2$player1, SYD08_SDMg2$player2)
SYD08_SDMft2 <- as.matrix(SYD08_SDMft)
numRows <- nrow(SYD08_SDMft2)
numCols <- ncol(SYD08_SDMft2)
SYD08_SDMft3 <- SYD08_SDMft2[c(2:numRows) , c(2:numCols)]
SYD08_SDMTable <- graph.adjacency(SYD08_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(SYD08_SDMTable, vertex.label = V(SYD08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
SYD08_SDM.clusterCoef <- transitivity(SYD08_SDMTable, type="global") #cluster coefficient
SYD08_SDM.degreeCent <- centralization.degree(SYD08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_SDMftn <- as.network.matrix(SYD08_SDMft)
SYD08_SDM.netDensity <- network.density(SYD08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_SDM.entropy <- entropy(SYD08_SDMft) #entropy

SYD08_SDM.netMx <- cbind(SYD08_SDM.netMx, SYD08_SDM.clusterCoef, SYD08_SDM.degreeCent$centralization,
                         SYD08_SDM.netDensity, SYD08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
SYD08_TDMg2 <- data.frame(SYD08_TDM)
SYD08_TDMg2 <- SYD08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_TDMg2$player1
player2vector <- SYD08_TDMg2$player2
SYD08_TDMg3 <- SYD08_TDMg2
SYD08_TDMg3$p1inp2vec <- is.element(SYD08_TDMg3$player1, player2vector)
SYD08_TDMg3$p2inp1vec <- is.element(SYD08_TDMg3$player2, player1vector)

addPlayer1 <- SYD08_TDMg3[ which(SYD08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_TDMg3[ which(SYD08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_TDMg2 <- rbind(SYD08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
SYD08_TDMft <- ftable(SYD08_TDMg2$player1, SYD08_TDMg2$player2)
SYD08_TDMft2 <- as.matrix(SYD08_TDMft)
numRows <- nrow(SYD08_TDMft2)
numCols <- ncol(SYD08_TDMft2)
SYD08_TDMft3 <- SYD08_TDMft2[c(2:numRows) , c(2:numCols)]
SYD08_TDMTable <- graph.adjacency(SYD08_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(SYD08_TDMTable, vertex.label = V(SYD08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
SYD08_TDM.clusterCoef <- transitivity(SYD08_TDMTable, type="global") #cluster coefficient
SYD08_TDM.degreeCent <- centralization.degree(SYD08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_TDMftn <- as.network.matrix(SYD08_TDMft)
SYD08_TDM.netDensity <- network.density(SYD08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_TDM.entropy <- entropy(SYD08_TDMft) #entropy

SYD08_TDM.netMx <- cbind(SYD08_TDM.netMx, SYD08_TDM.clusterCoef, SYD08_TDM.degreeCent$centralization,
                         SYD08_TDM.netDensity, SYD08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
SYD08_SDg2 <- data.frame(SYD08_SD)
SYD08_SDg2 <- SYD08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_SDg2$player1
player2vector <- SYD08_SDg2$player2
SYD08_SDg3 <- SYD08_SDg2
SYD08_SDg3$p1inp2vec <- is.element(SYD08_SDg3$player1, player2vector)
SYD08_SDg3$p2inp1vec <- is.element(SYD08_SDg3$player2, player1vector)

addPlayer1 <- SYD08_SDg3[ which(SYD08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_SDg3[ which(SYD08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_SDg2 <- rbind(SYD08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
SYD08_SDft <- ftable(SYD08_SDg2$player1, SYD08_SDg2$player2)
SYD08_SDft2 <- as.matrix(SYD08_SDft)
numRows <- nrow(SYD08_SDft2)
numCols <- ncol(SYD08_SDft2)
SYD08_SDft3 <- SYD08_SDft2[c(2:numRows) , c(2:numCols)]
SYD08_SDTable <- graph.adjacency(SYD08_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(SYD08_SDTable, vertex.label = V(SYD08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
SYD08_SD.clusterCoef <- transitivity(SYD08_SDTable, type="global") #cluster coefficient
SYD08_SD.degreeCent <- centralization.degree(SYD08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_SDftn <- as.network.matrix(SYD08_SDft)
SYD08_SD.netDensity <- network.density(SYD08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_SD.entropy <- entropy(SYD08_SDft) #entropy

SYD08_SD.netMx <- cbind(SYD08_SD.netMx, SYD08_SD.clusterCoef, SYD08_SD.degreeCent$centralization,
                        SYD08_SD.netDensity, SYD08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************

round = 8
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
SYD08_TDg2 <- data.frame(SYD08_TD)
SYD08_TDg2 <- SYD08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_TDg2$player1
player2vector <- SYD08_TDg2$player2
SYD08_TDg3 <- SYD08_TDg2
SYD08_TDg3$p1inp2vec <- is.element(SYD08_TDg3$player1, player2vector)
SYD08_TDg3$p2inp1vec <- is.element(SYD08_TDg3$player2, player1vector)

addPlayer1 <- SYD08_TDg3[ which(SYD08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_TDg3[ which(SYD08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_TDg2 <- rbind(SYD08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
SYD08_TDft <- ftable(SYD08_TDg2$player1, SYD08_TDg2$player2)
SYD08_TDft2 <- as.matrix(SYD08_TDft)
numRows <- nrow(SYD08_TDft2)
numCols <- ncol(SYD08_TDft2)
SYD08_TDft3 <- SYD08_TDft2[c(2:numRows) , c(2:numCols)]
SYD08_TDTable <- graph.adjacency(SYD08_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(SYD08_TDTable, vertex.label = V(SYD08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
SYD08_TD.clusterCoef <- transitivity(SYD08_TDTable, type="global") #cluster coefficient
SYD08_TD.degreeCent <- centralization.degree(SYD08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_TDftn <- as.network.matrix(SYD08_TDft)
SYD08_TD.netDensity <- network.density(SYD08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_TD.entropy <- entropy(SYD08_TDft) #entropy

SYD08_TD.netMx <- cbind(SYD08_TD.netMx, SYD08_TD.clusterCoef, SYD08_TD.degreeCent$centralization,
                        SYD08_TD.netDensity, SYD08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
SYD08_QTg2 <- data.frame(SYD08_QT)
SYD08_QTg2 <- SYD08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD08_QTg2$player1
player2vector <- SYD08_QTg2$player2
SYD08_QTg3 <- SYD08_QTg2
SYD08_QTg3$p1inp2vec <- is.element(SYD08_QTg3$player1, player2vector)
SYD08_QTg3$p2inp1vec <- is.element(SYD08_QTg3$player2, player1vector)

addPlayer1 <- SYD08_QTg3[ which(SYD08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD08_QTg3[ which(SYD08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD08_QTg2 <- rbind(SYD08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
SYD08_QTft <- ftable(SYD08_QTg2$player1, SYD08_QTg2$player2)
SYD08_QTft2 <- as.matrix(SYD08_QTft)
numRows <- nrow(SYD08_QTft2)
numCols <- ncol(SYD08_QTft2)
SYD08_QTft3 <- SYD08_QTft2[c(2:numRows) , c(2:numCols)]
SYD08_QTTable <- graph.adjacency(SYD08_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(SYD08_QTTable, vertex.label = V(SYD08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
SYD08_QT.clusterCoef <- transitivity(SYD08_QTTable, type="global") #cluster coefficient
SYD08_QT.degreeCent <- centralization.degree(SYD08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD08_QTftn <- as.network.matrix(SYD08_QTft)
SYD08_QT.netDensity <- network.density(SYD08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD08_QT.entropy <- entropy(SYD08_QTft) #entropy

SYD08_QT.netMx <- cbind(SYD08_QT.netMx, SYD08_QT.clusterCoef, SYD08_QT.degreeCent$centralization,
                        SYD08_QT.netDensity, SYD08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD08_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************
#NA

round = 8
teamName = "WB"
KIoutcome = "Goal_F"
WB08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
WB08_Gg2 <- data.frame(WB08_G)
WB08_Gg2 <- WB08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_Gg2$player1
player2vector <- WB08_Gg2$player2
WB08_Gg3 <- WB08_Gg2
WB08_Gg3$p1inp2vec <- is.element(WB08_Gg3$player1, player2vector)
WB08_Gg3$p2inp1vec <- is.element(WB08_Gg3$player2, player1vector)

addPlayer1 <- WB08_Gg3[ which(WB08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_Gg3[ which(WB08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_Gg2 <- rbind(WB08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
WB08_Gft <- ftable(WB08_Gg2$player1, WB08_Gg2$player2)
WB08_Gft2 <- as.matrix(WB08_Gft)
numRows <- nrow(WB08_Gft2)
numCols <- ncol(WB08_Gft2)
WB08_Gft3 <- WB08_Gft2[c(2:numRows) , c(2:numCols)]
WB08_GTable <- graph.adjacency(WB08_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(WB08_GTable, vertex.label = V(WB08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
WB08_G.clusterCoef <- transitivity(WB08_GTable, type="global") #cluster coefficient
WB08_G.degreeCent <- centralization.degree(WB08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_Gftn <- as.network.matrix(WB08_Gft)
WB08_G.netDensity <- network.density(WB08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_G.entropy <- entropy(WB08_Gft) #entropy

WB08_G.netMx <- cbind(WB08_G.netMx, WB08_G.clusterCoef, WB08_G.degreeCent$centralization,
                      WB08_G.netDensity, WB08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "WB"
KIoutcome = "Behind_F"
WB08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
WB08_Bg2 <- data.frame(WB08_B)
WB08_Bg2 <- WB08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_Bg2$player1
player2vector <- WB08_Bg2$player2
WB08_Bg3 <- WB08_Bg2
WB08_Bg3$p1inp2vec <- is.element(WB08_Bg3$player1, player2vector)
WB08_Bg3$p2inp1vec <- is.element(WB08_Bg3$player2, player1vector)

addPlayer1 <- WB08_Bg3[ which(WB08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_Bg3[ which(WB08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_Bg2 <- rbind(WB08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
WB08_Bft <- ftable(WB08_Bg2$player1, WB08_Bg2$player2)
WB08_Bft2 <- as.matrix(WB08_Bft)
numRows <- nrow(WB08_Bft2)
numCols <- ncol(WB08_Bft2)
WB08_Bft3 <- WB08_Bft2[c(2:numRows) , c(2:numCols)]
WB08_BTable <- graph.adjacency(WB08_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(WB08_BTable, vertex.label = V(WB08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
WB08_B.clusterCoef <- transitivity(WB08_BTable, type="global") #cluster coefficient
WB08_B.degreeCent <- centralization.degree(WB08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_Bftn <- as.network.matrix(WB08_Bft)
WB08_B.netDensity <- network.density(WB08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_B.entropy <- entropy(WB08_Bft) #entropy

WB08_B.netMx <- cbind(WB08_B.netMx, WB08_B.clusterCoef, WB08_B.degreeCent$centralization,
                      WB08_B.netDensity, WB08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************
#NA

round = 8
teamName = "WB"
KIoutcome = "Stoppage_F"
WB08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
WB08_SFg2 <- data.frame(WB08_SF)
WB08_SFg2 <- WB08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_SFg2$player1
player2vector <- WB08_SFg2$player2
WB08_SFg3 <- WB08_SFg2
WB08_SFg3$p1inp2vec <- is.element(WB08_SFg3$player1, player2vector)
WB08_SFg3$p2inp1vec <- is.element(WB08_SFg3$player2, player1vector)

addPlayer1 <- WB08_SFg3[ which(WB08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_SFg3[ which(WB08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_SFg2 <- rbind(WB08_SFg2, addPlayers)

#ROUND 8, FWD Stoppage graph using weighted edges
WB08_SFft <- ftable(WB08_SFg2$player1, WB08_SFg2$player2)
WB08_SFft2 <- as.matrix(WB08_SFft)
numRows <- nrow(WB08_SFft2)
numCols <- ncol(WB08_SFft2)
WB08_SFft3 <- WB08_SFft2[c(2:numRows) , c(2:numCols)]
WB08_SFTable <- graph.adjacency(WB08_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(WB08_SFTable, vertex.label = V(WB08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
WB08_SF.clusterCoef <- transitivity(WB08_SFTable, type="global") #cluster coefficient
WB08_SF.degreeCent <- centralization.degree(WB08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_SFftn <- as.network.matrix(WB08_SFft)
WB08_SF.netDensity <- network.density(WB08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_SF.entropy <- entropy(WB08_SFft) #entropy

WB08_SF.netMx <- cbind(WB08_SF.netMx, WB08_SF.clusterCoef, WB08_SF.degreeCent$centralization,
                       WB08_SF.netDensity, WB08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************

round = 8
teamName = "WB"
KIoutcome = "Turnover_F"
WB08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
WB08_TFg2 <- data.frame(WB08_TF)
WB08_TFg2 <- WB08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_TFg2$player1
player2vector <- WB08_TFg2$player2
WB08_TFg3 <- WB08_TFg2
WB08_TFg3$p1inp2vec <- is.element(WB08_TFg3$player1, player2vector)
WB08_TFg3$p2inp1vec <- is.element(WB08_TFg3$player2, player1vector)

addPlayer1 <- WB08_TFg3[ which(WB08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_TFg3[ which(WB08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_TFg2 <- rbind(WB08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
WB08_TFft <- ftable(WB08_TFg2$player1, WB08_TFg2$player2)
WB08_TFft2 <- as.matrix(WB08_TFft)
numRows <- nrow(WB08_TFft2)
numCols <- ncol(WB08_TFft2)
WB08_TFft3 <- WB08_TFft2[c(2:numRows) , c(2:numCols)]
WB08_TFTable <- graph.adjacency(WB08_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(WB08_TFTable, vertex.label = V(WB08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
WB08_TF.clusterCoef <- transitivity(WB08_TFTable, type="global") #cluster coefficient
WB08_TF.degreeCent <- centralization.degree(WB08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_TFftn <- as.network.matrix(WB08_TFft)
WB08_TF.netDensity <- network.density(WB08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_TF.entropy <- entropy(WB08_TFft) #entropy

WB08_TF.netMx <- cbind(WB08_TF.netMx, WB08_TF.clusterCoef, WB08_TF.degreeCent$centralization,
                       WB08_TF.netDensity, WB08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************
#NA

round = 8
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
WB08_SAMg2 <- data.frame(WB08_SAM)
WB08_SAMg2 <- WB08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_SAMg2$player1
player2vector <- WB08_SAMg2$player2
WB08_SAMg3 <- WB08_SAMg2
WB08_SAMg3$p1inp2vec <- is.element(WB08_SAMg3$player1, player2vector)
WB08_SAMg3$p2inp1vec <- is.element(WB08_SAMg3$player2, player1vector)

addPlayer1 <- WB08_SAMg3[ which(WB08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_SAMg3[ which(WB08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_SAMg2 <- rbind(WB08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
WB08_SAMft <- ftable(WB08_SAMg2$player1, WB08_SAMg2$player2)
WB08_SAMft2 <- as.matrix(WB08_SAMft)
numRows <- nrow(WB08_SAMft2)
numCols <- ncol(WB08_SAMft2)
WB08_SAMft3 <- WB08_SAMft2[c(2:numRows) , c(2:numCols)]
WB08_SAMTable <- graph.adjacency(WB08_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(WB08_SAMTable, vertex.label = V(WB08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
WB08_SAM.clusterCoef <- transitivity(WB08_SAMTable, type="global") #cluster coefficient
WB08_SAM.degreeCent <- centralization.degree(WB08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_SAMftn <- as.network.matrix(WB08_SAMft)
WB08_SAM.netDensity <- network.density(WB08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_SAM.entropy <- entropy(WB08_SAMft) #entropy

WB08_SAM.netMx <- cbind(WB08_SAM.netMx, WB08_SAM.clusterCoef, WB08_SAM.degreeCent$centralization,
                        WB08_SAM.netDensity, WB08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "WB"
KIoutcome = "Turnover_AM"
WB08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
WB08_TAMg2 <- data.frame(WB08_TAM)
WB08_TAMg2 <- WB08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_TAMg2$player1
player2vector <- WB08_TAMg2$player2
WB08_TAMg3 <- WB08_TAMg2
WB08_TAMg3$p1inp2vec <- is.element(WB08_TAMg3$player1, player2vector)
WB08_TAMg3$p2inp1vec <- is.element(WB08_TAMg3$player2, player1vector)

addPlayer1 <- WB08_TAMg3[ which(WB08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_TAMg3[ which(WB08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_TAMg2 <- rbind(WB08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
WB08_TAMft <- ftable(WB08_TAMg2$player1, WB08_TAMg2$player2)
WB08_TAMft2 <- as.matrix(WB08_TAMft)
numRows <- nrow(WB08_TAMft2)
numCols <- ncol(WB08_TAMft2)
WB08_TAMft3 <- WB08_TAMft2[c(2:numRows) , c(2:numCols)]
WB08_TAMTable <- graph.adjacency(WB08_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(WB08_TAMTable, vertex.label = V(WB08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
WB08_TAM.clusterCoef <- transitivity(WB08_TAMTable, type="global") #cluster coefficient
WB08_TAM.degreeCent <- centralization.degree(WB08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_TAMftn <- as.network.matrix(WB08_TAMft)
WB08_TAM.netDensity <- network.density(WB08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_TAM.entropy <- entropy(WB08_TAMft) #entropy

WB08_TAM.netMx <- cbind(WB08_TAM.netMx, WB08_TAM.clusterCoef, WB08_TAM.degreeCent$centralization,
                        WB08_TAM.netDensity, WB08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
WB08_SDMg2 <- data.frame(WB08_SDM)
WB08_SDMg2 <- WB08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_SDMg2$player1
player2vector <- WB08_SDMg2$player2
WB08_SDMg3 <- WB08_SDMg2
WB08_SDMg3$p1inp2vec <- is.element(WB08_SDMg3$player1, player2vector)
WB08_SDMg3$p2inp1vec <- is.element(WB08_SDMg3$player2, player1vector)

addPlayer1 <- WB08_SDMg3[ which(WB08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_SDMg3[ which(WB08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_SDMg2 <- rbind(WB08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
WB08_SDMft <- ftable(WB08_SDMg2$player1, WB08_SDMg2$player2)
WB08_SDMft2 <- as.matrix(WB08_SDMft)
numRows <- nrow(WB08_SDMft2)
numCols <- ncol(WB08_SDMft2)
WB08_SDMft3 <- WB08_SDMft2[c(2:numRows) , c(2:numCols)]
WB08_SDMTable <- graph.adjacency(WB08_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(WB08_SDMTable, vertex.label = V(WB08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
WB08_SDM.clusterCoef <- transitivity(WB08_SDMTable, type="global") #cluster coefficient
WB08_SDM.degreeCent <- centralization.degree(WB08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_SDMftn <- as.network.matrix(WB08_SDMft)
WB08_SDM.netDensity <- network.density(WB08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_SDM.entropy <- entropy(WB08_SDMft) #entropy

WB08_SDM.netMx <- cbind(WB08_SDM.netMx, WB08_SDM.clusterCoef, WB08_SDM.degreeCent$centralization,
                        WB08_SDM.netDensity, WB08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************

round = 8
teamName = "WB"
KIoutcome = "Turnover_DM"
WB08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
WB08_TDMg2 <- data.frame(WB08_TDM)
WB08_TDMg2 <- WB08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_TDMg2$player1
player2vector <- WB08_TDMg2$player2
WB08_TDMg3 <- WB08_TDMg2
WB08_TDMg3$p1inp2vec <- is.element(WB08_TDMg3$player1, player2vector)
WB08_TDMg3$p2inp1vec <- is.element(WB08_TDMg3$player2, player1vector)

addPlayer1 <- WB08_TDMg3[ which(WB08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_TDMg3[ which(WB08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_TDMg2 <- rbind(WB08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
WB08_TDMft <- ftable(WB08_TDMg2$player1, WB08_TDMg2$player2)
WB08_TDMft2 <- as.matrix(WB08_TDMft)
numRows <- nrow(WB08_TDMft2)
numCols <- ncol(WB08_TDMft2)
WB08_TDMft3 <- WB08_TDMft2[c(2:numRows) , c(2:numCols)]
WB08_TDMTable <- graph.adjacency(WB08_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(WB08_TDMTable, vertex.label = V(WB08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
WB08_TDM.clusterCoef <- transitivity(WB08_TDMTable, type="global") #cluster coefficient
WB08_TDM.degreeCent <- centralization.degree(WB08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_TDMftn <- as.network.matrix(WB08_TDMft)
WB08_TDM.netDensity <- network.density(WB08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_TDM.entropy <- entropy(WB08_TDMft) #entropy

WB08_TDM.netMx <- cbind(WB08_TDM.netMx, WB08_TDM.clusterCoef, WB08_TDM.degreeCent$centralization,
                        WB08_TDM.netDensity, WB08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "WB"
KIoutcome = "Stoppage_D"
WB08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
WB08_SDg2 <- data.frame(WB08_SD)
WB08_SDg2 <- WB08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_SDg2$player1
player2vector <- WB08_SDg2$player2
WB08_SDg3 <- WB08_SDg2
WB08_SDg3$p1inp2vec <- is.element(WB08_SDg3$player1, player2vector)
WB08_SDg3$p2inp1vec <- is.element(WB08_SDg3$player2, player1vector)

addPlayer1 <- WB08_SDg3[ which(WB08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_SDg3[ which(WB08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_SDg2 <- rbind(WB08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
WB08_SDft <- ftable(WB08_SDg2$player1, WB08_SDg2$player2)
WB08_SDft2 <- as.matrix(WB08_SDft)
numRows <- nrow(WB08_SDft2)
numCols <- ncol(WB08_SDft2)
WB08_SDft3 <- WB08_SDft2[c(2:numRows) , c(2:numCols)]
WB08_SDTable <- graph.adjacency(WB08_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(WB08_SDTable, vertex.label = V(WB08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
WB08_SD.clusterCoef <- transitivity(WB08_SDTable, type="global") #cluster coefficient
WB08_SD.degreeCent <- centralization.degree(WB08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_SDftn <- as.network.matrix(WB08_SDft)
WB08_SD.netDensity <- network.density(WB08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_SD.entropy <- entropy(WB08_SDft) #entropy

WB08_SD.netMx <- cbind(WB08_SD.netMx, WB08_SD.clusterCoef, WB08_SD.degreeCent$centralization,
                       WB08_SD.netDensity, WB08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "WB"
KIoutcome = "Turnover_D"
WB08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
WB08_TDg2 <- data.frame(WB08_TD)
WB08_TDg2 <- WB08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_TDg2$player1
player2vector <- WB08_TDg2$player2
WB08_TDg3 <- WB08_TDg2
WB08_TDg3$p1inp2vec <- is.element(WB08_TDg3$player1, player2vector)
WB08_TDg3$p2inp1vec <- is.element(WB08_TDg3$player2, player1vector)

addPlayer1 <- WB08_TDg3[ which(WB08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_TDg3[ which(WB08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_TDg2 <- rbind(WB08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
WB08_TDft <- ftable(WB08_TDg2$player1, WB08_TDg2$player2)
WB08_TDft2 <- as.matrix(WB08_TDft)
numRows <- nrow(WB08_TDft2)
numCols <- ncol(WB08_TDft2)
WB08_TDft3 <- WB08_TDft2[c(2:numRows) , c(2:numCols)]
WB08_TDTable <- graph.adjacency(WB08_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(WB08_TDTable, vertex.label = V(WB08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
WB08_TD.clusterCoef <- transitivity(WB08_TDTable, type="global") #cluster coefficient
WB08_TD.degreeCent <- centralization.degree(WB08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_TDftn <- as.network.matrix(WB08_TDft)
WB08_TD.netDensity <- network.density(WB08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_TD.entropy <- entropy(WB08_TDft) #entropy

WB08_TD.netMx <- cbind(WB08_TD.netMx, WB08_TD.clusterCoef, WB08_TD.degreeCent$centralization,
                       WB08_TD.netDensity, WB08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
WB08_QTg2 <- data.frame(WB08_QT)
WB08_QTg2 <- WB08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB08_QTg2$player1
player2vector <- WB08_QTg2$player2
WB08_QTg3 <- WB08_QTg2
WB08_QTg3$p1inp2vec <- is.element(WB08_QTg3$player1, player2vector)
WB08_QTg3$p2inp1vec <- is.element(WB08_QTg3$player2, player1vector)

addPlayer1 <- WB08_QTg3[ which(WB08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB08_QTg3[ which(WB08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB08_QTg2 <- rbind(WB08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
WB08_QTft <- ftable(WB08_QTg2$player1, WB08_QTg2$player2)
WB08_QTft2 <- as.matrix(WB08_QTft)
numRows <- nrow(WB08_QTft2)
numCols <- ncol(WB08_QTft2)
WB08_QTft3 <- WB08_QTft2[c(2:numRows) , c(2:numCols)]
WB08_QTTable <- graph.adjacency(WB08_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(WB08_QTTable, vertex.label = V(WB08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
WB08_QT.clusterCoef <- transitivity(WB08_QTTable, type="global") #cluster coefficient
WB08_QT.degreeCent <- centralization.degree(WB08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB08_QTftn <- as.network.matrix(WB08_QTft)
WB08_QT.netDensity <- network.density(WB08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB08_QT.entropy <- entropy(WB08_QTft) #entropy

WB08_QT.netMx <- cbind(WB08_QT.netMx, WB08_QT.clusterCoef, WB08_QT.degreeCent$centralization,
                       WB08_QT.netDensity, WB08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB08_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 8
##

#ROUND 8, Goal***************************************************************

round = 8
teamName = "WCE"
KIoutcome = "Goal_F"
WCE08_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Goal with weighted edges
WCE08_Gg2 <- data.frame(WCE08_G)
WCE08_Gg2 <- WCE08_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_Gg2$player1
player2vector <- WCE08_Gg2$player2
WCE08_Gg3 <- WCE08_Gg2
WCE08_Gg3$p1inp2vec <- is.element(WCE08_Gg3$player1, player2vector)
WCE08_Gg3$p2inp1vec <- is.element(WCE08_Gg3$player2, player1vector)

addPlayer1 <- WCE08_Gg3[ which(WCE08_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_Gg3[ which(WCE08_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_Gg2 <- rbind(WCE08_Gg2, addPlayers)

#ROUND 8, Goal graph using weighted edges
WCE08_Gft <- ftable(WCE08_Gg2$player1, WCE08_Gg2$player2)
WCE08_Gft2 <- as.matrix(WCE08_Gft)
numRows <- nrow(WCE08_Gft2)
numCols <- ncol(WCE08_Gft2)
WCE08_Gft3 <- WCE08_Gft2[c(2:numRows) , c(2:numCols)]
WCE08_GTable <- graph.adjacency(WCE08_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Goal graph=weighted
plot.igraph(WCE08_GTable, vertex.label = V(WCE08_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Goal calulation of network metrics
#igraph
WCE08_G.clusterCoef <- transitivity(WCE08_GTable, type="global") #cluster coefficient
WCE08_G.degreeCent <- centralization.degree(WCE08_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_Gftn <- as.network.matrix(WCE08_Gft)
WCE08_G.netDensity <- network.density(WCE08_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_G.entropy <- entropy(WCE08_Gft) #entropy

WCE08_G.netMx <- cbind(WCE08_G.netMx, WCE08_G.clusterCoef, WCE08_G.degreeCent$centralization,
                       WCE08_G.netDensity, WCE08_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_G.netMx) <- varnames

#ROUND 8, Behind***************************************************************
#NA

round = 8
teamName = "WCE"
KIoutcome = "Behind_F"
WCE08_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, Behind with weighted edges
WCE08_Bg2 <- data.frame(WCE08_B)
WCE08_Bg2 <- WCE08_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_Bg2$player1
player2vector <- WCE08_Bg2$player2
WCE08_Bg3 <- WCE08_Bg2
WCE08_Bg3$p1inp2vec <- is.element(WCE08_Bg3$player1, player2vector)
WCE08_Bg3$p2inp1vec <- is.element(WCE08_Bg3$player2, player1vector)

addPlayer1 <- WCE08_Bg3[ which(WCE08_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_Bg3[ which(WCE08_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_Bg2 <- rbind(WCE08_Bg2, addPlayers)

#ROUND 8, Behind graph using weighted edges
WCE08_Bft <- ftable(WCE08_Bg2$player1, WCE08_Bg2$player2)
WCE08_Bft2 <- as.matrix(WCE08_Bft)
numRows <- nrow(WCE08_Bft2)
numCols <- ncol(WCE08_Bft2)
WCE08_Bft3 <- WCE08_Bft2[c(2:numRows) , c(2:numCols)]
WCE08_BTable <- graph.adjacency(WCE08_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 8, Behind graph=weighted
plot.igraph(WCE08_BTable, vertex.label = V(WCE08_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, Behind calulation of network metrics
#igraph
WCE08_B.clusterCoef <- transitivity(WCE08_BTable, type="global") #cluster coefficient
WCE08_B.degreeCent <- centralization.degree(WCE08_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_Bftn <- as.network.matrix(WCE08_Bft)
WCE08_B.netDensity <- network.density(WCE08_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_B.entropy <- entropy(WCE08_Bft) #entropy

WCE08_B.netMx <- cbind(WCE08_B.netMx, WCE08_B.clusterCoef, WCE08_B.degreeCent$centralization,
                       WCE08_B.netDensity, WCE08_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_B.netMx) <- varnames

#ROUND 8, FWD Stoppage**********************************************************

round = 8
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE08_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Stoppage with weighted edges
WCE08_SFg2 <- data.frame(WCE08_SF)
WCE08_SFg2 <- WCE08_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_SFg2$player1
player2vector <- WCE08_SFg2$player2
WCE08_SFg3 <- WCE08_SFg2
WCE08_SFg3$p1inp2vec <- is.element(WCE08_SFg3$player1, player2vector)
WCE08_SFg3$p2inp1vec <- is.element(WCE08_SFg3$player2, player1vector)

addPlayer1 <- WCE08_SFg3[ which(WCE08_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_SFg3[ which(WCE08_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_SFg2 <- rbind(WCE08_SFg2, addPlayers)


#ROUND 8, FWD Stoppage graph using weighted edges
WCE08_SFft <- ftable(WCE08_SFg2$player1, WCE08_SFg2$player2)
WCE08_SFft2 <- as.matrix(WCE08_SFft)
numRows <- nrow(WCE08_SFft2)
numCols <- ncol(WCE08_SFft2)
WCE08_SFft3 <- WCE08_SFft2[c(2:numRows) , c(2:numCols)]
WCE08_SFTable <- graph.adjacency(WCE08_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Stoppage graph=weighted
plot.igraph(WCE08_SFTable, vertex.label = V(WCE08_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Stoppage calulation of network metrics
#igraph
WCE08_SF.clusterCoef <- transitivity(WCE08_SFTable, type="global") #cluster coefficient
WCE08_SF.degreeCent <- centralization.degree(WCE08_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_SFftn <- as.network.matrix(WCE08_SFft)
WCE08_SF.netDensity <- network.density(WCE08_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_SF.entropy <- entropy(WCE08_SFft) #entropy

WCE08_SF.netMx <- cbind(WCE08_SF.netMx, WCE08_SF.clusterCoef, WCE08_SF.degreeCent$centralization,
                        WCE08_SF.netDensity, WCE08_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_SF.netMx) <- varnames

#ROUND 8, FWD Turnover**********************************************************
#NA

round = 8
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE08_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, FWD Turnover with weighted edges
WCE08_TFg2 <- data.frame(WCE08_TF)
WCE08_TFg2 <- WCE08_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_TFg2$player1
player2vector <- WCE08_TFg2$player2
WCE08_TFg3 <- WCE08_TFg2
WCE08_TFg3$p1inp2vec <- is.element(WCE08_TFg3$player1, player2vector)
WCE08_TFg3$p2inp1vec <- is.element(WCE08_TFg3$player2, player1vector)

addPlayer1 <- WCE08_TFg3[ which(WCE08_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_TFg3[ which(WCE08_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_TFg2 <- rbind(WCE08_TFg2, addPlayers)

#ROUND 8, FWD Turnover graph using weighted edges
WCE08_TFft <- ftable(WCE08_TFg2$player1, WCE08_TFg2$player2)
WCE08_TFft2 <- as.matrix(WCE08_TFft)
numRows <- nrow(WCE08_TFft2)
numCols <- ncol(WCE08_TFft2)
WCE08_TFft3 <- WCE08_TFft2[c(2:numRows) , c(2:numCols)]
WCE08_TFTable <- graph.adjacency(WCE08_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, FWD Turnover graph=weighted
plot.igraph(WCE08_TFTable, vertex.label = V(WCE08_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, FWD Turnover calulation of network metrics
#igraph
WCE08_TF.clusterCoef <- transitivity(WCE08_TFTable, type="global") #cluster coefficient
WCE08_TF.degreeCent <- centralization.degree(WCE08_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_TFftn <- as.network.matrix(WCE08_TFft)
WCE08_TF.netDensity <- network.density(WCE08_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_TF.entropy <- entropy(WCE08_TFft) #entropy

WCE08_TF.netMx <- cbind(WCE08_TF.netMx, WCE08_TF.clusterCoef, WCE08_TF.degreeCent$centralization,
                        WCE08_TF.netDensity, WCE08_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_TF.netMx) <- varnames

#ROUND 8, AM Stoppage**********************************************************
#NA

round = 8
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE08_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Stoppage with weighted edges
WCE08_SAMg2 <- data.frame(WCE08_SAM)
WCE08_SAMg2 <- WCE08_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_SAMg2$player1
player2vector <- WCE08_SAMg2$player2
WCE08_SAMg3 <- WCE08_SAMg2
WCE08_SAMg3$p1inp2vec <- is.element(WCE08_SAMg3$player1, player2vector)
WCE08_SAMg3$p2inp1vec <- is.element(WCE08_SAMg3$player2, player1vector)

addPlayer1 <- WCE08_SAMg3[ which(WCE08_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_SAMg3[ which(WCE08_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_SAMg2 <- rbind(WCE08_SAMg2, addPlayers)

#ROUND 8, AM Stoppage graph using weighted edges
WCE08_SAMft <- ftable(WCE08_SAMg2$player1, WCE08_SAMg2$player2)
WCE08_SAMft2 <- as.matrix(WCE08_SAMft)
numRows <- nrow(WCE08_SAMft2)
numCols <- ncol(WCE08_SAMft2)
WCE08_SAMft3 <- WCE08_SAMft2[c(2:numRows) , c(2:numCols)]
WCE08_SAMTable <- graph.adjacency(WCE08_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Stoppage graph=weighted
plot.igraph(WCE08_SAMTable, vertex.label = V(WCE08_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Stoppage calulation of network metrics
#igraph
WCE08_SAM.clusterCoef <- transitivity(WCE08_SAMTable, type="global") #cluster coefficient
WCE08_SAM.degreeCent <- centralization.degree(WCE08_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_SAMftn <- as.network.matrix(WCE08_SAMft)
WCE08_SAM.netDensity <- network.density(WCE08_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_SAM.entropy <- entropy(WCE08_SAMft) #entropy

WCE08_SAM.netMx <- cbind(WCE08_SAM.netMx, WCE08_SAM.clusterCoef, WCE08_SAM.degreeCent$centralization,
                         WCE08_SAM.netDensity, WCE08_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_SAM.netMx) <- varnames

#ROUND 8, AM Turnover**********************************************************

round = 8
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE08_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, AM Turnover with weighted edges
WCE08_TAMg2 <- data.frame(WCE08_TAM)
WCE08_TAMg2 <- WCE08_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_TAMg2$player1
player2vector <- WCE08_TAMg2$player2
WCE08_TAMg3 <- WCE08_TAMg2
WCE08_TAMg3$p1inp2vec <- is.element(WCE08_TAMg3$player1, player2vector)
WCE08_TAMg3$p2inp1vec <- is.element(WCE08_TAMg3$player2, player1vector)

addPlayer1 <- WCE08_TAMg3[ which(WCE08_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_TAMg3[ which(WCE08_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_TAMg2 <- rbind(WCE08_TAMg2, addPlayers)

#ROUND 8, AM Turnover graph using weighted edges
WCE08_TAMft <- ftable(WCE08_TAMg2$player1, WCE08_TAMg2$player2)
WCE08_TAMft2 <- as.matrix(WCE08_TAMft)
numRows <- nrow(WCE08_TAMft2)
numCols <- ncol(WCE08_TAMft2)
WCE08_TAMft3 <- WCE08_TAMft2[c(2:numRows) , c(2:numCols)]
WCE08_TAMTable <- graph.adjacency(WCE08_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, AM Turnover graph=weighted
plot.igraph(WCE08_TAMTable, vertex.label = V(WCE08_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, AM Turnover calulation of network metrics
#igraph
WCE08_TAM.clusterCoef <- transitivity(WCE08_TAMTable, type="global") #cluster coefficient
WCE08_TAM.degreeCent <- centralization.degree(WCE08_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_TAMftn <- as.network.matrix(WCE08_TAMft)
WCE08_TAM.netDensity <- network.density(WCE08_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_TAM.entropy <- entropy(WCE08_TAMft) #entropy

WCE08_TAM.netMx <- cbind(WCE08_TAM.netMx, WCE08_TAM.clusterCoef, WCE08_TAM.degreeCent$centralization,
                         WCE08_TAM.netDensity, WCE08_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_TAM.netMx) <- varnames

#ROUND 8, DM Stoppage**********************************************************

round = 8
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE08_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Stoppage with weighted edges
WCE08_SDMg2 <- data.frame(WCE08_SDM)
WCE08_SDMg2 <- WCE08_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_SDMg2$player1
player2vector <- WCE08_SDMg2$player2
WCE08_SDMg3 <- WCE08_SDMg2
WCE08_SDMg3$p1inp2vec <- is.element(WCE08_SDMg3$player1, player2vector)
WCE08_SDMg3$p2inp1vec <- is.element(WCE08_SDMg3$player2, player1vector)

addPlayer1 <- WCE08_SDMg3[ which(WCE08_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_SDMg3[ which(WCE08_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_SDMg2 <- rbind(WCE08_SDMg2, addPlayers)

#ROUND 8, DM Stoppage graph using weighted edges
WCE08_SDMft <- ftable(WCE08_SDMg2$player1, WCE08_SDMg2$player2)
WCE08_SDMft2 <- as.matrix(WCE08_SDMft)
numRows <- nrow(WCE08_SDMft2)
numCols <- ncol(WCE08_SDMft2)
WCE08_SDMft3 <- WCE08_SDMft2[c(2:numRows) , c(2:numCols)]
WCE08_SDMTable <- graph.adjacency(WCE08_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Stoppage graph=weighted
plot.igraph(WCE08_SDMTable, vertex.label = V(WCE08_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Stoppage calulation of network metrics
#igraph
WCE08_SDM.clusterCoef <- transitivity(WCE08_SDMTable, type="global") #cluster coefficient
WCE08_SDM.degreeCent <- centralization.degree(WCE08_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_SDMftn <- as.network.matrix(WCE08_SDMft)
WCE08_SDM.netDensity <- network.density(WCE08_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_SDM.entropy <- entropy(WCE08_SDMft) #entropy

WCE08_SDM.netMx <- cbind(WCE08_SDM.netMx, WCE08_SDM.clusterCoef, WCE08_SDM.degreeCent$centralization,
                         WCE08_SDM.netDensity, WCE08_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_SDM.netMx) <- varnames

#ROUND 8, DM Turnover**********************************************************
#NA

round = 8
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE08_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, DM Turnover with weighted edges
WCE08_TDMg2 <- data.frame(WCE08_TDM)
WCE08_TDMg2 <- WCE08_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_TDMg2$player1
player2vector <- WCE08_TDMg2$player2
WCE08_TDMg3 <- WCE08_TDMg2
WCE08_TDMg3$p1inp2vec <- is.element(WCE08_TDMg3$player1, player2vector)
WCE08_TDMg3$p2inp1vec <- is.element(WCE08_TDMg3$player2, player1vector)

addPlayer1 <- WCE08_TDMg3[ which(WCE08_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_TDMg3[ which(WCE08_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_TDMg2 <- rbind(WCE08_TDMg2, addPlayers)

#ROUND 8, DM Turnover graph using weighted edges
WCE08_TDMft <- ftable(WCE08_TDMg2$player1, WCE08_TDMg2$player2)
WCE08_TDMft2 <- as.matrix(WCE08_TDMft)
numRows <- nrow(WCE08_TDMft2)
numCols <- ncol(WCE08_TDMft2)
WCE08_TDMft3 <- WCE08_TDMft2[c(2:numRows) , c(2:numCols)]
WCE08_TDMTable <- graph.adjacency(WCE08_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 8, DM Turnover graph=weighted
plot.igraph(WCE08_TDMTable, vertex.label = V(WCE08_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, DM Turnover calulation of network metrics
#igraph
WCE08_TDM.clusterCoef <- transitivity(WCE08_TDMTable, type="global") #cluster coefficient
WCE08_TDM.degreeCent <- centralization.degree(WCE08_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_TDMftn <- as.network.matrix(WCE08_TDMft)
WCE08_TDM.netDensity <- network.density(WCE08_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_TDM.entropy <- entropy(WCE08_TDMft) #entropy

WCE08_TDM.netMx <- cbind(WCE08_TDM.netMx, WCE08_TDM.clusterCoef, WCE08_TDM.degreeCent$centralization,
                         WCE08_TDM.netDensity, WCE08_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_TDM.netMx) <- varnames

#ROUND 8, D Stoppage**********************************************************
#NA

round = 8
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE08_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Stoppage with weighted edges
WCE08_SDg2 <- data.frame(WCE08_SD)
WCE08_SDg2 <- WCE08_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_SDg2$player1
player2vector <- WCE08_SDg2$player2
WCE08_SDg3 <- WCE08_SDg2
WCE08_SDg3$p1inp2vec <- is.element(WCE08_SDg3$player1, player2vector)
WCE08_SDg3$p2inp1vec <- is.element(WCE08_SDg3$player2, player1vector)

addPlayer1 <- WCE08_SDg3[ which(WCE08_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_SDg3[ which(WCE08_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_SDg2 <- rbind(WCE08_SDg2, addPlayers)

#ROUND 8, D Stoppage graph using weighted edges
WCE08_SDft <- ftable(WCE08_SDg2$player1, WCE08_SDg2$player2)
WCE08_SDft2 <- as.matrix(WCE08_SDft)
numRows <- nrow(WCE08_SDft2)
numCols <- ncol(WCE08_SDft2)
WCE08_SDft3 <- WCE08_SDft2[c(2:numRows) , c(2:numCols)]
WCE08_SDTable <- graph.adjacency(WCE08_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Stoppage graph=weighted
plot.igraph(WCE08_SDTable, vertex.label = V(WCE08_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Stoppage calulation of network metrics
#igraph
WCE08_SD.clusterCoef <- transitivity(WCE08_SDTable, type="global") #cluster coefficient
WCE08_SD.degreeCent <- centralization.degree(WCE08_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_SDftn <- as.network.matrix(WCE08_SDft)
WCE08_SD.netDensity <- network.density(WCE08_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_SD.entropy <- entropy(WCE08_SDft) #entropy

WCE08_SD.netMx <- cbind(WCE08_SD.netMx, WCE08_SD.clusterCoef, WCE08_SD.degreeCent$centralization,
                        WCE08_SD.netDensity, WCE08_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_SD.netMx) <- varnames

#ROUND 8, D Turnover**********************************************************
#NA

round = 8
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE08_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, D Turnover with weighted edges
WCE08_TDg2 <- data.frame(WCE08_TD)
WCE08_TDg2 <- WCE08_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_TDg2$player1
player2vector <- WCE08_TDg2$player2
WCE08_TDg3 <- WCE08_TDg2
WCE08_TDg3$p1inp2vec <- is.element(WCE08_TDg3$player1, player2vector)
WCE08_TDg3$p2inp1vec <- is.element(WCE08_TDg3$player2, player1vector)

addPlayer1 <- WCE08_TDg3[ which(WCE08_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_TDg3[ which(WCE08_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_TDg2 <- rbind(WCE08_TDg2, addPlayers)

#ROUND 8, D Turnover graph using weighted edges
WCE08_TDft <- ftable(WCE08_TDg2$player1, WCE08_TDg2$player2)
WCE08_TDft2 <- as.matrix(WCE08_TDft)
numRows <- nrow(WCE08_TDft2)
numCols <- ncol(WCE08_TDft2)
WCE08_TDft3 <- WCE08_TDft2[c(2:numRows) , c(2:numCols)]
WCE08_TDTable <- graph.adjacency(WCE08_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, D Turnover graph=weighted
plot.igraph(WCE08_TDTable, vertex.label = V(WCE08_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, D Turnover calulation of network metrics
#igraph
WCE08_TD.clusterCoef <- transitivity(WCE08_TDTable, type="global") #cluster coefficient
WCE08_TD.degreeCent <- centralization.degree(WCE08_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_TDftn <- as.network.matrix(WCE08_TDft)
WCE08_TD.netDensity <- network.density(WCE08_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_TD.entropy <- entropy(WCE08_TDft) #entropy

WCE08_TD.netMx <- cbind(WCE08_TD.netMx, WCE08_TD.clusterCoef, WCE08_TD.degreeCent$centralization,
                        WCE08_TD.netDensity, WCE08_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_TD.netMx) <- varnames

#ROUND 8, End of Qtr**********************************************************
#NA

round = 8
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE08_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 8, End of Qtr with weighted edges
WCE08_QTg2 <- data.frame(WCE08_QT)
WCE08_QTg2 <- WCE08_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE08_QTg2$player1
player2vector <- WCE08_QTg2$player2
WCE08_QTg3 <- WCE08_QTg2
WCE08_QTg3$p1inp2vec <- is.element(WCE08_QTg3$player1, player2vector)
WCE08_QTg3$p2inp1vec <- is.element(WCE08_QTg3$player2, player1vector)

addPlayer1 <- WCE08_QTg3[ which(WCE08_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE08_QTg3[ which(WCE08_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE08_QTg2 <- rbind(WCE08_QTg2, addPlayers)

#ROUND 8, End of Qtr graph using weighted edges
WCE08_QTft <- ftable(WCE08_QTg2$player1, WCE08_QTg2$player2)
WCE08_QTft2 <- as.matrix(WCE08_QTft)
numRows <- nrow(WCE08_QTft2)
numCols <- ncol(WCE08_QTft2)
WCE08_QTft3 <- WCE08_QTft2[c(2:numRows) , c(2:numCols)]
WCE08_QTTable <- graph.adjacency(WCE08_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 8, End of Qtr graph=weighted
plot.igraph(WCE08_QTTable, vertex.label = V(WCE08_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE08_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 8, End of Qtr calulation of network metrics
#igraph
WCE08_QT.clusterCoef <- transitivity(WCE08_QTTable, type="global") #cluster coefficient
WCE08_QT.degreeCent <- centralization.degree(WCE08_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE08_QTftn <- as.network.matrix(WCE08_QTft)
WCE08_QT.netDensity <- network.density(WCE08_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE08_QT.entropy <- entropy(WCE08_QTft) #entropy

WCE08_QT.netMx <- cbind(WCE08_QT.netMx, WCE08_QT.clusterCoef, WCE08_QT.degreeCent$centralization,
                        WCE08_QT.netDensity, WCE08_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE08_QT.netMx) <- varnames
