#####
#10-02-16- Real data 20
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************
#NA

round = 20
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
ADEL20_Gg2 <- data.frame(ADEL20_G)
ADEL20_Gg2 <- ADEL20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_Gg2$player1
player2vector <- ADEL20_Gg2$player2
ADEL20_Gg3 <- ADEL20_Gg2
ADEL20_Gg3$p1inp2vec <- is.element(ADEL20_Gg3$player1, player2vector)
ADEL20_Gg3$p2inp1vec <- is.element(ADEL20_Gg3$player2, player1vector)

addPlayer1 <- ADEL20_Gg3[ which(ADEL20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_Gg3[ which(ADEL20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_Gg2 <- rbind(ADEL20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
ADEL20_Gft <- ftable(ADEL20_Gg2$player1, ADEL20_Gg2$player2)
ADEL20_Gft2 <- as.matrix(ADEL20_Gft)
numRows <- nrow(ADEL20_Gft2)
numCols <- ncol(ADEL20_Gft2)
ADEL20_Gft3 <- ADEL20_Gft2[c(2:numRows) , c(2:numCols)]
ADEL20_GTable <- graph.adjacency(ADEL20_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(ADEL20_GTable, vertex.label = V(ADEL20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
ADEL20_G.clusterCoef <- transitivity(ADEL20_GTable, type="global") #cluster coefficient
ADEL20_G.degreeCent <- centralization.degree(ADEL20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_Gftn <- as.network.matrix(ADEL20_Gft)
ADEL20_G.netDensity <- network.density(ADEL20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_G.entropy <- entropy(ADEL20_Gft) #entropy

ADEL20_G.netMx <- cbind(ADEL20_G.netMx, ADEL20_G.clusterCoef, ADEL20_G.degreeCent$centralization,
                        ADEL20_G.netDensity, ADEL20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************

round = 20
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
ADEL20_Bg2 <- data.frame(ADEL20_B)
ADEL20_Bg2 <- ADEL20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_Bg2$player1
player2vector <- ADEL20_Bg2$player2
ADEL20_Bg3 <- ADEL20_Bg2
ADEL20_Bg3$p1inp2vec <- is.element(ADEL20_Bg3$player1, player2vector)
ADEL20_Bg3$p2inp1vec <- is.element(ADEL20_Bg3$player2, player1vector)

addPlayer1 <- ADEL20_Bg3[ which(ADEL20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_Bg3[ which(ADEL20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_Bg2 <- rbind(ADEL20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
ADEL20_Bft <- ftable(ADEL20_Bg2$player1, ADEL20_Bg2$player2)
ADEL20_Bft2 <- as.matrix(ADEL20_Bft)
numRows <- nrow(ADEL20_Bft2)
numCols <- ncol(ADEL20_Bft2)
ADEL20_Bft3 <- ADEL20_Bft2[c(2:numRows) , c(2:numCols)]
ADEL20_BTable <- graph.adjacency(ADEL20_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(ADEL20_BTable, vertex.label = V(ADEL20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
ADEL20_B.clusterCoef <- transitivity(ADEL20_BTable, type="global") #cluster coefficient
ADEL20_B.degreeCent <- centralization.degree(ADEL20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_Bftn <- as.network.matrix(ADEL20_Bft)
ADEL20_B.netDensity <- network.density(ADEL20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_B.entropy <- entropy(ADEL20_Bft) #entropy

ADEL20_B.netMx <- cbind(ADEL20_B.netMx, ADEL20_B.clusterCoef, ADEL20_B.degreeCent$centralization,
                        ADEL20_B.netDensity, ADEL20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
ADEL20_SFg2 <- data.frame(ADEL20_SF)
ADEL20_SFg2 <- ADEL20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_SFg2$player1
player2vector <- ADEL20_SFg2$player2
ADEL20_SFg3 <- ADEL20_SFg2
ADEL20_SFg3$p1inp2vec <- is.element(ADEL20_SFg3$player1, player2vector)
ADEL20_SFg3$p2inp1vec <- is.element(ADEL20_SFg3$player2, player1vector)

addPlayer1 <- ADEL20_SFg3[ which(ADEL20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_SFg3[ which(ADEL20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_SFg2 <- rbind(ADEL20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
ADEL20_SFft <- ftable(ADEL20_SFg2$player1, ADEL20_SFg2$player2)
ADEL20_SFft2 <- as.matrix(ADEL20_SFft)
numRows <- nrow(ADEL20_SFft2)
numCols <- ncol(ADEL20_SFft2)
ADEL20_SFft3 <- ADEL20_SFft2[c(2:numRows) , c(2:numCols)]
ADEL20_SFTable <- graph.adjacency(ADEL20_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(ADEL20_SFTable, vertex.label = V(ADEL20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
ADEL20_SF.clusterCoef <- transitivity(ADEL20_SFTable, type="global") #cluster coefficient
ADEL20_SF.degreeCent <- centralization.degree(ADEL20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_SFftn <- as.network.matrix(ADEL20_SFft)
ADEL20_SF.netDensity <- network.density(ADEL20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_SF.entropy <- entropy(ADEL20_SFft) #entropy

ADEL20_SF.netMx <- cbind(ADEL20_SF.netMx, ADEL20_SF.clusterCoef, ADEL20_SF.degreeCent$centralization,
                         ADEL20_SF.netDensity, ADEL20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
ADEL20_TFg2 <- data.frame(ADEL20_TF)
ADEL20_TFg2 <- ADEL20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_TFg2$player1
player2vector <- ADEL20_TFg2$player2
ADEL20_TFg3 <- ADEL20_TFg2
ADEL20_TFg3$p1inp2vec <- is.element(ADEL20_TFg3$player1, player2vector)
ADEL20_TFg3$p2inp1vec <- is.element(ADEL20_TFg3$player2, player1vector)

addPlayer1 <- ADEL20_TFg3[ which(ADEL20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_TFg3[ which(ADEL20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_TFg2 <- rbind(ADEL20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
ADEL20_TFft <- ftable(ADEL20_TFg2$player1, ADEL20_TFg2$player2)
ADEL20_TFft2 <- as.matrix(ADEL20_TFft)
numRows <- nrow(ADEL20_TFft2)
numCols <- ncol(ADEL20_TFft2)
ADEL20_TFft3 <- ADEL20_TFft2[c(2:numRows) , c(2:numCols)]
ADEL20_TFTable <- graph.adjacency(ADEL20_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(ADEL20_TFTable, vertex.label = V(ADEL20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
ADEL20_TF.clusterCoef <- transitivity(ADEL20_TFTable, type="global") #cluster coefficient
ADEL20_TF.degreeCent <- centralization.degree(ADEL20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_TFftn <- as.network.matrix(ADEL20_TFft)
ADEL20_TF.netDensity <- network.density(ADEL20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_TF.entropy <- entropy(ADEL20_TFft) #entropy

ADEL20_TF.netMx <- cbind(ADEL20_TF.netMx, ADEL20_TF.clusterCoef, ADEL20_TF.degreeCent$centralization,
                         ADEL20_TF.netDensity, ADEL20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************

round = 20
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
ADEL20_SAMg2 <- data.frame(ADEL20_SAM)
ADEL20_SAMg2 <- ADEL20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_SAMg2$player1
player2vector <- ADEL20_SAMg2$player2
ADEL20_SAMg3 <- ADEL20_SAMg2
ADEL20_SAMg3$p1inp2vec <- is.element(ADEL20_SAMg3$player1, player2vector)
ADEL20_SAMg3$p2inp1vec <- is.element(ADEL20_SAMg3$player2, player1vector)

addPlayer1 <- ADEL20_SAMg3[ which(ADEL20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_SAMg3[ which(ADEL20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_SAMg2 <- rbind(ADEL20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
ADEL20_SAMft <- ftable(ADEL20_SAMg2$player1, ADEL20_SAMg2$player2)
ADEL20_SAMft2 <- as.matrix(ADEL20_SAMft)
numRows <- nrow(ADEL20_SAMft2)
numCols <- ncol(ADEL20_SAMft2)
ADEL20_SAMft3 <- ADEL20_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL20_SAMTable <- graph.adjacency(ADEL20_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(ADEL20_SAMTable, vertex.label = V(ADEL20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
ADEL20_SAM.clusterCoef <- transitivity(ADEL20_SAMTable, type="global") #cluster coefficient
ADEL20_SAM.degreeCent <- centralization.degree(ADEL20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_SAMftn <- as.network.matrix(ADEL20_SAMft)
ADEL20_SAM.netDensity <- network.density(ADEL20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_SAM.entropy <- entropy(ADEL20_SAMft) #entropy

ADEL20_SAM.netMx <- cbind(ADEL20_SAM.netMx, ADEL20_SAM.clusterCoef, ADEL20_SAM.degreeCent$centralization,
                          ADEL20_SAM.netDensity, ADEL20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
ADEL20_TAMg2 <- data.frame(ADEL20_TAM)
ADEL20_TAMg2 <- ADEL20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_TAMg2$player1
player2vector <- ADEL20_TAMg2$player2
ADEL20_TAMg3 <- ADEL20_TAMg2
ADEL20_TAMg3$p1inp2vec <- is.element(ADEL20_TAMg3$player1, player2vector)
ADEL20_TAMg3$p2inp1vec <- is.element(ADEL20_TAMg3$player2, player1vector)

addPlayer1 <- ADEL20_TAMg3[ which(ADEL20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_TAMg3[ which(ADEL20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_TAMg2 <- rbind(ADEL20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
ADEL20_TAMft <- ftable(ADEL20_TAMg2$player1, ADEL20_TAMg2$player2)
ADEL20_TAMft2 <- as.matrix(ADEL20_TAMft)
numRows <- nrow(ADEL20_TAMft2)
numCols <- ncol(ADEL20_TAMft2)
ADEL20_TAMft3 <- ADEL20_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL20_TAMTable <- graph.adjacency(ADEL20_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(ADEL20_TAMTable, vertex.label = V(ADEL20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
ADEL20_TAM.clusterCoef <- transitivity(ADEL20_TAMTable, type="global") #cluster coefficient
ADEL20_TAM.degreeCent <- centralization.degree(ADEL20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_TAMftn <- as.network.matrix(ADEL20_TAMft)
ADEL20_TAM.netDensity <- network.density(ADEL20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_TAM.entropy <- entropy(ADEL20_TAMft) #entropy

ADEL20_TAM.netMx <- cbind(ADEL20_TAM.netMx, ADEL20_TAM.clusterCoef, ADEL20_TAM.degreeCent$centralization,
                          ADEL20_TAM.netDensity, ADEL20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
ADEL20_SDMg2 <- data.frame(ADEL20_SDM)
ADEL20_SDMg2 <- ADEL20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_SDMg2$player1
player2vector <- ADEL20_SDMg2$player2
ADEL20_SDMg3 <- ADEL20_SDMg2
ADEL20_SDMg3$p1inp2vec <- is.element(ADEL20_SDMg3$player1, player2vector)
ADEL20_SDMg3$p2inp1vec <- is.element(ADEL20_SDMg3$player2, player1vector)

addPlayer1 <- ADEL20_SDMg3[ which(ADEL20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_SDMg3[ which(ADEL20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_SDMg2 <- rbind(ADEL20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
ADEL20_SDMft <- ftable(ADEL20_SDMg2$player1, ADEL20_SDMg2$player2)
ADEL20_SDMft2 <- as.matrix(ADEL20_SDMft)
numRows <- nrow(ADEL20_SDMft2)
numCols <- ncol(ADEL20_SDMft2)
ADEL20_SDMft3 <- ADEL20_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL20_SDMTable <- graph.adjacency(ADEL20_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(ADEL20_SDMTable, vertex.label = V(ADEL20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
ADEL20_SDM.clusterCoef <- transitivity(ADEL20_SDMTable, type="global") #cluster coefficient
ADEL20_SDM.degreeCent <- centralization.degree(ADEL20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_SDMftn <- as.network.matrix(ADEL20_SDMft)
ADEL20_SDM.netDensity <- network.density(ADEL20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_SDM.entropy <- entropy(ADEL20_SDMft) #entropy

ADEL20_SDM.netMx <- cbind(ADEL20_SDM.netMx, ADEL20_SDM.clusterCoef, ADEL20_SDM.degreeCent$centralization,
                          ADEL20_SDM.netDensity, ADEL20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************
#NA

round = 20
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
ADEL20_TDMg2 <- data.frame(ADEL20_TDM)
ADEL20_TDMg2 <- ADEL20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_TDMg2$player1
player2vector <- ADEL20_TDMg2$player2
ADEL20_TDMg3 <- ADEL20_TDMg2
ADEL20_TDMg3$p1inp2vec <- is.element(ADEL20_TDMg3$player1, player2vector)
ADEL20_TDMg3$p2inp1vec <- is.element(ADEL20_TDMg3$player2, player1vector)

addPlayer1 <- ADEL20_TDMg3[ which(ADEL20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_TDMg3[ which(ADEL20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_TDMg2 <- rbind(ADEL20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
ADEL20_TDMft <- ftable(ADEL20_TDMg2$player1, ADEL20_TDMg2$player2)
ADEL20_TDMft2 <- as.matrix(ADEL20_TDMft)
numRows <- nrow(ADEL20_TDMft2)
numCols <- ncol(ADEL20_TDMft2)
ADEL20_TDMft3 <- ADEL20_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL20_TDMTable <- graph.adjacency(ADEL20_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(ADEL20_TDMTable, vertex.label = V(ADEL20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
ADEL20_TDM.clusterCoef <- transitivity(ADEL20_TDMTable, type="global") #cluster coefficient
ADEL20_TDM.degreeCent <- centralization.degree(ADEL20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_TDMftn <- as.network.matrix(ADEL20_TDMft)
ADEL20_TDM.netDensity <- network.density(ADEL20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_TDM.entropy <- entropy(ADEL20_TDMft) #entropy

ADEL20_TDM.netMx <- cbind(ADEL20_TDM.netMx, ADEL20_TDM.clusterCoef, ADEL20_TDM.degreeCent$centralization,
                          ADEL20_TDM.netDensity, ADEL20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
ADEL20_SDg2 <- data.frame(ADEL20_SD)
ADEL20_SDg2 <- ADEL20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_SDg2$player1
player2vector <- ADEL20_SDg2$player2
ADEL20_SDg3 <- ADEL20_SDg2
ADEL20_SDg3$p1inp2vec <- is.element(ADEL20_SDg3$player1, player2vector)
ADEL20_SDg3$p2inp1vec <- is.element(ADEL20_SDg3$player2, player1vector)

addPlayer1 <- ADEL20_SDg3[ which(ADEL20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_SDg3[ which(ADEL20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_SDg2 <- rbind(ADEL20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
ADEL20_SDft <- ftable(ADEL20_SDg2$player1, ADEL20_SDg2$player2)
ADEL20_SDft2 <- as.matrix(ADEL20_SDft)
numRows <- nrow(ADEL20_SDft2)
numCols <- ncol(ADEL20_SDft2)
ADEL20_SDft3 <- ADEL20_SDft2[c(2:numRows) , c(2:numCols)]
ADEL20_SDTable <- graph.adjacency(ADEL20_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(ADEL20_SDTable, vertex.label = V(ADEL20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
ADEL20_SD.clusterCoef <- transitivity(ADEL20_SDTable, type="global") #cluster coefficient
ADEL20_SD.degreeCent <- centralization.degree(ADEL20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_SDftn <- as.network.matrix(ADEL20_SDft)
ADEL20_SD.netDensity <- network.density(ADEL20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_SD.entropy <- entropy(ADEL20_SDft) #entropy

ADEL20_SD.netMx <- cbind(ADEL20_SD.netMx, ADEL20_SD.clusterCoef, ADEL20_SD.degreeCent$centralization,
                         ADEL20_SD.netDensity, ADEL20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
ADEL20_TDg2 <- data.frame(ADEL20_TD)
ADEL20_TDg2 <- ADEL20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_TDg2$player1
player2vector <- ADEL20_TDg2$player2
ADEL20_TDg3 <- ADEL20_TDg2
ADEL20_TDg3$p1inp2vec <- is.element(ADEL20_TDg3$player1, player2vector)
ADEL20_TDg3$p2inp1vec <- is.element(ADEL20_TDg3$player2, player1vector)

addPlayer1 <- ADEL20_TDg3[ which(ADEL20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_TDg3[ which(ADEL20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_TDg2 <- rbind(ADEL20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
ADEL20_TDft <- ftable(ADEL20_TDg2$player1, ADEL20_TDg2$player2)
ADEL20_TDft2 <- as.matrix(ADEL20_TDft)
numRows <- nrow(ADEL20_TDft2)
numCols <- ncol(ADEL20_TDft2)
ADEL20_TDft3 <- ADEL20_TDft2[c(2:numRows) , c(2:numCols)]
ADEL20_TDTable <- graph.adjacency(ADEL20_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(ADEL20_TDTable, vertex.label = V(ADEL20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
ADEL20_TD.clusterCoef <- transitivity(ADEL20_TDTable, type="global") #cluster coefficient
ADEL20_TD.degreeCent <- centralization.degree(ADEL20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_TDftn <- as.network.matrix(ADEL20_TDft)
ADEL20_TD.netDensity <- network.density(ADEL20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_TD.entropy <- entropy(ADEL20_TDft) #entropy

ADEL20_TD.netMx <- cbind(ADEL20_TD.netMx, ADEL20_TD.clusterCoef, ADEL20_TD.degreeCent$centralization,
                         ADEL20_TD.netDensity, ADEL20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
ADEL20_QTg2 <- data.frame(ADEL20_QT)
ADEL20_QTg2 <- ADEL20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL20_QTg2$player1
player2vector <- ADEL20_QTg2$player2
ADEL20_QTg3 <- ADEL20_QTg2
ADEL20_QTg3$p1inp2vec <- is.element(ADEL20_QTg3$player1, player2vector)
ADEL20_QTg3$p2inp1vec <- is.element(ADEL20_QTg3$player2, player1vector)

addPlayer1 <- ADEL20_QTg3[ which(ADEL20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL20_QTg3[ which(ADEL20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL20_QTg2 <- rbind(ADEL20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
ADEL20_QTft <- ftable(ADEL20_QTg2$player1, ADEL20_QTg2$player2)
ADEL20_QTft2 <- as.matrix(ADEL20_QTft)
numRows <- nrow(ADEL20_QTft2)
numCols <- ncol(ADEL20_QTft2)
ADEL20_QTft3 <- ADEL20_QTft2[c(2:numRows) , c(2:numCols)]
ADEL20_QTTable <- graph.adjacency(ADEL20_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(ADEL20_QTTable, vertex.label = V(ADEL20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
ADEL20_QT.clusterCoef <- transitivity(ADEL20_QTTable, type="global") #cluster coefficient
ADEL20_QT.degreeCent <- centralization.degree(ADEL20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL20_QTftn <- as.network.matrix(ADEL20_QTft)
ADEL20_QT.netDensity <- network.density(ADEL20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL20_QT.entropy <- entropy(ADEL20_QTft) #entropy

ADEL20_QT.netMx <- cbind(ADEL20_QT.netMx, ADEL20_QT.clusterCoef, ADEL20_QT.degreeCent$centralization,
                         ADEL20_QT.netDensity, ADEL20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL20_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "BL"
KIoutcome = "Goal_F"
BL20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
BL20_Gg2 <- data.frame(BL20_G)
BL20_Gg2 <- BL20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_Gg2$player1
player2vector <- BL20_Gg2$player2
BL20_Gg3 <- BL20_Gg2
BL20_Gg3$p1inp2vec <- is.element(BL20_Gg3$player1, player2vector)
BL20_Gg3$p2inp1vec <- is.element(BL20_Gg3$player2, player1vector)

addPlayer1 <- BL20_Gg3[ which(BL20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_Gg3[ which(BL20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_Gg2 <- rbind(BL20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
BL20_Gft <- ftable(BL20_Gg2$player1, BL20_Gg2$player2)
BL20_Gft2 <- as.matrix(BL20_Gft)
numRows <- nrow(BL20_Gft2)
numCols <- ncol(BL20_Gft2)
BL20_Gft3 <- BL20_Gft2[c(2:numRows) , c(2:numCols)]
BL20_GTable <- graph.adjacency(BL20_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(BL20_GTable, vertex.label = V(BL20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
BL20_G.clusterCoef <- transitivity(BL20_GTable, type="global") #cluster coefficient
BL20_G.degreeCent <- centralization.degree(BL20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_Gftn <- as.network.matrix(BL20_Gft)
BL20_G.netDensity <- network.density(BL20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_G.entropy <- entropy(BL20_Gft) #entropy

BL20_G.netMx <- cbind(BL20_G.netMx, BL20_G.clusterCoef, BL20_G.degreeCent$centralization,
                      BL20_G.netDensity, BL20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "BL"
KIoutcome = "Behind_F"
BL20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
BL20_Bg2 <- data.frame(BL20_B)
BL20_Bg2 <- BL20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_Bg2$player1
player2vector <- BL20_Bg2$player2
BL20_Bg3 <- BL20_Bg2
BL20_Bg3$p1inp2vec <- is.element(BL20_Bg3$player1, player2vector)
BL20_Bg3$p2inp1vec <- is.element(BL20_Bg3$player2, player1vector)

addPlayer1 <- BL20_Bg3[ which(BL20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_Bg3[ which(BL20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_Bg2 <- rbind(BL20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
BL20_Bft <- ftable(BL20_Bg2$player1, BL20_Bg2$player2)
BL20_Bft2 <- as.matrix(BL20_Bft)
numRows <- nrow(BL20_Bft2)
numCols <- ncol(BL20_Bft2)
BL20_Bft3 <- BL20_Bft2[c(2:numRows) , c(2:numCols)]
BL20_BTable <- graph.adjacency(BL20_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(BL20_BTable, vertex.label = V(BL20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
BL20_B.clusterCoef <- transitivity(BL20_BTable, type="global") #cluster coefficient
BL20_B.degreeCent <- centralization.degree(BL20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_Bftn <- as.network.matrix(BL20_Bft)
BL20_B.netDensity <- network.density(BL20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_B.entropy <- entropy(BL20_Bft) #entropy

BL20_B.netMx <- cbind(BL20_B.netMx, BL20_B.clusterCoef, BL20_B.degreeCent$centralization,
                      BL20_B.netDensity, BL20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************

round = 20
teamName = "BL"
KIoutcome = "Stoppage_F"
BL20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
BL20_SFg2 <- data.frame(BL20_SF)
BL20_SFg2 <- BL20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_SFg2$player1
player2vector <- BL20_SFg2$player2
BL20_SFg3 <- BL20_SFg2
BL20_SFg3$p1inp2vec <- is.element(BL20_SFg3$player1, player2vector)
BL20_SFg3$p2inp1vec <- is.element(BL20_SFg3$player2, player1vector)

addPlayer1 <- BL20_SFg3[ which(BL20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- BL20_SFg3[ which(BL20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_SFg2 <- rbind(BL20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
BL20_SFft <- ftable(BL20_SFg2$player1, BL20_SFg2$player2)
BL20_SFft2 <- as.matrix(BL20_SFft)
numRows <- nrow(BL20_SFft2)
numCols <- ncol(BL20_SFft2)
BL20_SFft3 <- BL20_SFft2[c(2:numRows) , c(2:numCols)]
BL20_SFTable <- graph.adjacency(BL20_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(BL20_SFTable, vertex.label = V(BL20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
BL20_SF.clusterCoef <- transitivity(BL20_SFTable, type="global") #cluster coefficient
BL20_SF.degreeCent <- centralization.degree(BL20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_SFftn <- as.network.matrix(BL20_SFft)
BL20_SF.netDensity <- network.density(BL20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_SF.entropy <- entropy(BL20_SFft) #entropy

BL20_SF.netMx <- cbind(BL20_SF.netMx, BL20_SF.clusterCoef, BL20_SF.degreeCent$centralization,
                       BL20_SF.netDensity, BL20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************
#NA

round = 20
teamName = "BL"
KIoutcome = "Turnover_F"
BL20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
BL20_TFg2 <- data.frame(BL20_TF)
BL20_TFg2 <- BL20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_TFg2$player1
player2vector <- BL20_TFg2$player2
BL20_TFg3 <- BL20_TFg2
BL20_TFg3$p1inp2vec <- is.element(BL20_TFg3$player1, player2vector)
BL20_TFg3$p2inp1vec <- is.element(BL20_TFg3$player2, player1vector)

addPlayer1 <- BL20_TFg3[ which(BL20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_TFg3[ which(BL20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_TFg2 <- rbind(BL20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
BL20_TFft <- ftable(BL20_TFg2$player1, BL20_TFg2$player2)
BL20_TFft2 <- as.matrix(BL20_TFft)
numRows <- nrow(BL20_TFft2)
numCols <- ncol(BL20_TFft2)
BL20_TFft3 <- BL20_TFft2[c(2:numRows) , c(2:numCols)]
BL20_TFTable <- graph.adjacency(BL20_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(BL20_TFTable, vertex.label = V(BL20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
BL20_TF.clusterCoef <- transitivity(BL20_TFTable, type="global") #cluster coefficient
BL20_TF.degreeCent <- centralization.degree(BL20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_TFftn <- as.network.matrix(BL20_TFft)
BL20_TF.netDensity <- network.density(BL20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_TF.entropy <- entropy(BL20_TFft) #entropy

BL20_TF.netMx <- cbind(BL20_TF.netMx, BL20_TF.clusterCoef, BL20_TF.degreeCent$centralization,
                       BL20_TF.netDensity, BL20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************
#NA

round = 20
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
BL20_SAMg2 <- data.frame(BL20_SAM)
BL20_SAMg2 <- BL20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_SAMg2$player1
player2vector <- BL20_SAMg2$player2
BL20_SAMg3 <- BL20_SAMg2
BL20_SAMg3$p1inp2vec <- is.element(BL20_SAMg3$player1, player2vector)
BL20_SAMg3$p2inp1vec <- is.element(BL20_SAMg3$player2, player1vector)

addPlayer1 <- BL20_SAMg3[ which(BL20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_SAMg3[ which(BL20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_SAMg2 <- rbind(BL20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
BL20_SAMft <- ftable(BL20_SAMg2$player1, BL20_SAMg2$player2)
BL20_SAMft2 <- as.matrix(BL20_SAMft)
numRows <- nrow(BL20_SAMft2)
numCols <- ncol(BL20_SAMft2)
BL20_SAMft3 <- BL20_SAMft2[c(2:numRows) , c(2:numCols)]
BL20_SAMTable <- graph.adjacency(BL20_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(BL20_SAMTable, vertex.label = V(BL20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
BL20_SAM.clusterCoef <- transitivity(BL20_SAMTable, type="global") #cluster coefficient
BL20_SAM.degreeCent <- centralization.degree(BL20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_SAMftn <- as.network.matrix(BL20_SAMft)
BL20_SAM.netDensity <- network.density(BL20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_SAM.entropy <- entropy(BL20_SAMft) #entropy

BL20_SAM.netMx <- cbind(BL20_SAM.netMx, BL20_SAM.clusterCoef, BL20_SAM.degreeCent$centralization,
                        BL20_SAM.netDensity, BL20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************
#NA

round = 20
teamName = "BL"
KIoutcome = "Turnover_AM"
BL20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
BL20_TAMg2 <- data.frame(BL20_TAM)
BL20_TAMg2 <- BL20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_TAMg2$player1
player2vector <- BL20_TAMg2$player2
BL20_TAMg3 <- BL20_TAMg2
BL20_TAMg3$p1inp2vec <- is.element(BL20_TAMg3$player1, player2vector)
BL20_TAMg3$p2inp1vec <- is.element(BL20_TAMg3$player2, player1vector)

addPlayer1 <- BL20_TAMg3[ which(BL20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_TAMg3[ which(BL20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_TAMg2 <- rbind(BL20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
BL20_TAMft <- ftable(BL20_TAMg2$player1, BL20_TAMg2$player2)
BL20_TAMft2 <- as.matrix(BL20_TAMft)
numRows <- nrow(BL20_TAMft2)
numCols <- ncol(BL20_TAMft2)
BL20_TAMft3 <- BL20_TAMft2[c(2:numRows) , c(2:numCols)]
BL20_TAMTable <- graph.adjacency(BL20_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(BL20_TAMTable, vertex.label = V(BL20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
BL20_TAM.clusterCoef <- transitivity(BL20_TAMTable, type="global") #cluster coefficient
BL20_TAM.degreeCent <- centralization.degree(BL20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_TAMftn <- as.network.matrix(BL20_TAMft)
BL20_TAM.netDensity <- network.density(BL20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_TAM.entropy <- entropy(BL20_TAMft) #entropy

BL20_TAM.netMx <- cbind(BL20_TAM.netMx, BL20_TAM.clusterCoef, BL20_TAM.degreeCent$centralization,
                        BL20_TAM.netDensity, BL20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
BL20_SDMg2 <- data.frame(BL20_SDM)
BL20_SDMg2 <- BL20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_SDMg2$player1
player2vector <- BL20_SDMg2$player2
BL20_SDMg3 <- BL20_SDMg2
BL20_SDMg3$p1inp2vec <- is.element(BL20_SDMg3$player1, player2vector)
BL20_SDMg3$p2inp1vec <- is.element(BL20_SDMg3$player2, player1vector)

addPlayer1 <- BL20_SDMg3[ which(BL20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_SDMg3[ which(BL20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_SDMg2 <- rbind(BL20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
BL20_SDMft <- ftable(BL20_SDMg2$player1, BL20_SDMg2$player2)
BL20_SDMft2 <- as.matrix(BL20_SDMft)
numRows <- nrow(BL20_SDMft2)
numCols <- ncol(BL20_SDMft2)
BL20_SDMft3 <- BL20_SDMft2[c(2:numRows) , c(2:numCols)]
BL20_SDMTable <- graph.adjacency(BL20_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(BL20_SDMTable, vertex.label = V(BL20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
BL20_SDM.clusterCoef <- transitivity(BL20_SDMTable, type="global") #cluster coefficient
BL20_SDM.degreeCent <- centralization.degree(BL20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_SDMftn <- as.network.matrix(BL20_SDMft)
BL20_SDM.netDensity <- network.density(BL20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_SDM.entropy <- entropy(BL20_SDMft) #entropy

BL20_SDM.netMx <- cbind(BL20_SDM.netMx, BL20_SDM.clusterCoef, BL20_SDM.degreeCent$centralization,
                        BL20_SDM.netDensity, BL20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "BL"
KIoutcome = "Turnover_DM"
BL20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
BL20_TDMg2 <- data.frame(BL20_TDM)
BL20_TDMg2 <- BL20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_TDMg2$player1
player2vector <- BL20_TDMg2$player2
BL20_TDMg3 <- BL20_TDMg2
BL20_TDMg3$p1inp2vec <- is.element(BL20_TDMg3$player1, player2vector)
BL20_TDMg3$p2inp1vec <- is.element(BL20_TDMg3$player2, player1vector)

addPlayer1 <- BL20_TDMg3[ which(BL20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_TDMg3[ which(BL20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_TDMg2 <- rbind(BL20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
BL20_TDMft <- ftable(BL20_TDMg2$player1, BL20_TDMg2$player2)
BL20_TDMft2 <- as.matrix(BL20_TDMft)
numRows <- nrow(BL20_TDMft2)
numCols <- ncol(BL20_TDMft2)
BL20_TDMft3 <- BL20_TDMft2[c(2:numRows) , c(2:numCols)]
BL20_TDMTable <- graph.adjacency(BL20_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(BL20_TDMTable, vertex.label = V(BL20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
BL20_TDM.clusterCoef <- transitivity(BL20_TDMTable, type="global") #cluster coefficient
BL20_TDM.degreeCent <- centralization.degree(BL20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_TDMftn <- as.network.matrix(BL20_TDMft)
BL20_TDM.netDensity <- network.density(BL20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_TDM.entropy <- entropy(BL20_TDMft) #entropy

BL20_TDM.netMx <- cbind(BL20_TDM.netMx, BL20_TDM.clusterCoef, BL20_TDM.degreeCent$centralization,
                        BL20_TDM.netDensity, BL20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "BL"
KIoutcome = "Stoppage_D"
BL20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
BL20_SDg2 <- data.frame(BL20_SD)
BL20_SDg2 <- BL20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_SDg2$player1
player2vector <- BL20_SDg2$player2
BL20_SDg3 <- BL20_SDg2
BL20_SDg3$p1inp2vec <- is.element(BL20_SDg3$player1, player2vector)
BL20_SDg3$p2inp1vec <- is.element(BL20_SDg3$player2, player1vector)

addPlayer1 <- BL20_SDg3[ which(BL20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_SDg3[ which(BL20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_SDg2 <- rbind(BL20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
BL20_SDft <- ftable(BL20_SDg2$player1, BL20_SDg2$player2)
BL20_SDft2 <- as.matrix(BL20_SDft)
numRows <- nrow(BL20_SDft2)
numCols <- ncol(BL20_SDft2)
BL20_SDft3 <- BL20_SDft2[c(2:numRows) , c(2:numCols)]
BL20_SDTable <- graph.adjacency(BL20_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(BL20_SDTable, vertex.label = V(BL20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
BL20_SD.clusterCoef <- transitivity(BL20_SDTable, type="global") #cluster coefficient
BL20_SD.degreeCent <- centralization.degree(BL20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_SDftn <- as.network.matrix(BL20_SDft)
BL20_SD.netDensity <- network.density(BL20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_SD.entropy <- entropy(BL20_SDft) #entropy

BL20_SD.netMx <- cbind(BL20_SD.netMx, BL20_SD.clusterCoef, BL20_SD.degreeCent$centralization,
                       BL20_SD.netDensity, BL20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "BL"
KIoutcome = "Turnover_D"
BL20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
BL20_TDg2 <- data.frame(BL20_TD)
BL20_TDg2 <- BL20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_TDg2$player1
player2vector <- BL20_TDg2$player2
BL20_TDg3 <- BL20_TDg2
BL20_TDg3$p1inp2vec <- is.element(BL20_TDg3$player1, player2vector)
BL20_TDg3$p2inp1vec <- is.element(BL20_TDg3$player2, player1vector)

addPlayer1 <- BL20_TDg3[ which(BL20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_TDg3[ which(BL20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_TDg2 <- rbind(BL20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
BL20_TDft <- ftable(BL20_TDg2$player1, BL20_TDg2$player2)
BL20_TDft2 <- as.matrix(BL20_TDft)
numRows <- nrow(BL20_TDft2)
numCols <- ncol(BL20_TDft2)
BL20_TDft3 <- BL20_TDft2[c(2:numRows) , c(2:numCols)]
BL20_TDTable <- graph.adjacency(BL20_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(BL20_TDTable, vertex.label = V(BL20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
BL20_TD.clusterCoef <- transitivity(BL20_TDTable, type="global") #cluster coefficient
BL20_TD.degreeCent <- centralization.degree(BL20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_TDftn <- as.network.matrix(BL20_TDft)
BL20_TD.netDensity <- network.density(BL20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_TD.entropy <- entropy(BL20_TDft) #entropy

BL20_TD.netMx <- cbind(BL20_TD.netMx, BL20_TD.clusterCoef, BL20_TD.degreeCent$centralization,
                       BL20_TD.netDensity, BL20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
BL20_QTg2 <- data.frame(BL20_QT)
BL20_QTg2 <- BL20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL20_QTg2$player1
player2vector <- BL20_QTg2$player2
BL20_QTg3 <- BL20_QTg2
BL20_QTg3$p1inp2vec <- is.element(BL20_QTg3$player1, player2vector)
BL20_QTg3$p2inp1vec <- is.element(BL20_QTg3$player2, player1vector)

addPlayer1 <- BL20_QTg3[ which(BL20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL20_QTg3[ which(BL20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL20_QTg2 <- rbind(BL20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
BL20_QTft <- ftable(BL20_QTg2$player1, BL20_QTg2$player2)
BL20_QTft2 <- as.matrix(BL20_QTft)
numRows <- nrow(BL20_QTft2)
numCols <- ncol(BL20_QTft2)
BL20_QTft3 <- BL20_QTft2[c(2:numRows) , c(2:numCols)]
BL20_QTTable <- graph.adjacency(BL20_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(BL20_QTTable, vertex.label = V(BL20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
BL20_QT.clusterCoef <- transitivity(BL20_QTTable, type="global") #cluster coefficient
BL20_QT.degreeCent <- centralization.degree(BL20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL20_QTftn <- as.network.matrix(BL20_QTft)
BL20_QT.netDensity <- network.density(BL20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL20_QT.entropy <- entropy(BL20_QTft) #entropy

BL20_QT.netMx <- cbind(BL20_QT.netMx, BL20_QT.clusterCoef, BL20_QT.degreeCent$centralization,
                       BL20_QT.netDensity, BL20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL20_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************
#NA

round = 20
teamName = "CARL"
KIoutcome = "Goal_F"
CARL20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
CARL20_Gg2 <- data.frame(CARL20_G)
CARL20_Gg2 <- CARL20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_Gg2$player1
player2vector <- CARL20_Gg2$player2
CARL20_Gg3 <- CARL20_Gg2
CARL20_Gg3$p1inp2vec <- is.element(CARL20_Gg3$player1, player2vector)
CARL20_Gg3$p2inp1vec <- is.element(CARL20_Gg3$player2, player1vector)

addPlayer1 <- CARL20_Gg3[ which(CARL20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_Gg3[ which(CARL20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_Gg2 <- rbind(CARL20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
CARL20_Gft <- ftable(CARL20_Gg2$player1, CARL20_Gg2$player2)
CARL20_Gft2 <- as.matrix(CARL20_Gft)
numRows <- nrow(CARL20_Gft2)
numCols <- ncol(CARL20_Gft2)
CARL20_Gft3 <- CARL20_Gft2[c(2:numRows) , c(2:numCols)]
CARL20_GTable <- graph.adjacency(CARL20_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(CARL20_GTable, vertex.label = V(CARL20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
CARL20_G.clusterCoef <- transitivity(CARL20_GTable, type="global") #cluster coefficient
CARL20_G.degreeCent <- centralization.degree(CARL20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_Gftn <- as.network.matrix(CARL20_Gft)
CARL20_G.netDensity <- network.density(CARL20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_G.entropy <- entropy(CARL20_Gft) #entropy

CARL20_G.netMx <- cbind(CARL20_G.netMx, CARL20_G.clusterCoef, CARL20_G.degreeCent$centralization,
                        CARL20_G.netDensity, CARL20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************

round = 20
teamName = "CARL"
KIoutcome = "Behind_F"
CARL20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
CARL20_Bg2 <- data.frame(CARL20_B)
CARL20_Bg2 <- CARL20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_Bg2$player1
player2vector <- CARL20_Bg2$player2
CARL20_Bg3 <- CARL20_Bg2
CARL20_Bg3$p1inp2vec <- is.element(CARL20_Bg3$player1, player2vector)
CARL20_Bg3$p2inp1vec <- is.element(CARL20_Bg3$player2, player1vector)

addPlayer1 <- CARL20_Bg3[ which(CARL20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_Bg3[ which(CARL20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_Bg2 <- rbind(CARL20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
CARL20_Bft <- ftable(CARL20_Bg2$player1, CARL20_Bg2$player2)
CARL20_Bft2 <- as.matrix(CARL20_Bft)
numRows <- nrow(CARL20_Bft2)
numCols <- ncol(CARL20_Bft2)
CARL20_Bft3 <- CARL20_Bft2[c(2:numRows) , c(2:numCols)]
CARL20_BTable <- graph.adjacency(CARL20_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(CARL20_BTable, vertex.label = V(CARL20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
CARL20_B.clusterCoef <- transitivity(CARL20_BTable, type="global") #cluster coefficient
CARL20_B.degreeCent <- centralization.degree(CARL20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_Bftn <- as.network.matrix(CARL20_Bft)
CARL20_B.netDensity <- network.density(CARL20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_B.entropy <- entropy(CARL20_Bft) #entropy

CARL20_B.netMx <- cbind(CARL20_B.netMx, CARL20_B.clusterCoef, CARL20_B.degreeCent$centralization,
                        CARL20_B.netDensity, CARL20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
CARL20_SFg2 <- data.frame(CARL20_SF)
CARL20_SFg2 <- CARL20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_SFg2$player1
player2vector <- CARL20_SFg2$player2
CARL20_SFg3 <- CARL20_SFg2
CARL20_SFg3$p1inp2vec <- is.element(CARL20_SFg3$player1, player2vector)
CARL20_SFg3$p2inp1vec <- is.element(CARL20_SFg3$player2, player1vector)

addPlayer1 <- CARL20_SFg3[ which(CARL20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_SFg3[ which(CARL20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_SFg2 <- rbind(CARL20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
CARL20_SFft <- ftable(CARL20_SFg2$player1, CARL20_SFg2$player2)
CARL20_SFft2 <- as.matrix(CARL20_SFft)
numRows <- nrow(CARL20_SFft2)
numCols <- ncol(CARL20_SFft2)
CARL20_SFft3 <- CARL20_SFft2[c(2:numRows) , c(2:numCols)]
CARL20_SFTable <- graph.adjacency(CARL20_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(CARL20_SFTable, vertex.label = V(CARL20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
CARL20_SF.clusterCoef <- transitivity(CARL20_SFTable, type="global") #cluster coefficient
CARL20_SF.degreeCent <- centralization.degree(CARL20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_SFftn <- as.network.matrix(CARL20_SFft)
CARL20_SF.netDensity <- network.density(CARL20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_SF.entropy <- entropy(CARL20_SFft) #entropy

CARL20_SF.netMx <- cbind(CARL20_SF.netMx, CARL20_SF.clusterCoef, CARL20_SF.degreeCent$centralization,
                         CARL20_SF.netDensity, CARL20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************
#NA

round = 20
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
CARL20_TFg2 <- data.frame(CARL20_TF)
CARL20_TFg2 <- CARL20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_TFg2$player1
player2vector <- CARL20_TFg2$player2
CARL20_TFg3 <- CARL20_TFg2
CARL20_TFg3$p1inp2vec <- is.element(CARL20_TFg3$player1, player2vector)
CARL20_TFg3$p2inp1vec <- is.element(CARL20_TFg3$player2, player1vector)

addPlayer1 <- CARL20_TFg3[ which(CARL20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_TFg3[ which(CARL20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_TFg2 <- rbind(CARL20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
CARL20_TFft <- ftable(CARL20_TFg2$player1, CARL20_TFg2$player2)
CARL20_TFft2 <- as.matrix(CARL20_TFft)
numRows <- nrow(CARL20_TFft2)
numCols <- ncol(CARL20_TFft2)
CARL20_TFft3 <- CARL20_TFft2[c(2:numRows) , c(2:numCols)]
CARL20_TFTable <- graph.adjacency(CARL20_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(CARL20_TFTable, vertex.label = V(CARL20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
CARL20_TF.clusterCoef <- transitivity(CARL20_TFTable, type="global") #cluster coefficient
CARL20_TF.degreeCent <- centralization.degree(CARL20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_TFftn <- as.network.matrix(CARL20_TFft)
CARL20_TF.netDensity <- network.density(CARL20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_TF.entropy <- entropy(CARL20_TFft) #entropy

CARL20_TF.netMx <- cbind(CARL20_TF.netMx, CARL20_TF.clusterCoef, CARL20_TF.degreeCent$centralization,
                         CARL20_TF.netDensity, CARL20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************

round = 20
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
CARL20_SAMg2 <- data.frame(CARL20_SAM)
CARL20_SAMg2 <- CARL20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_SAMg2$player1
player2vector <- CARL20_SAMg2$player2
CARL20_SAMg3 <- CARL20_SAMg2
CARL20_SAMg3$p1inp2vec <- is.element(CARL20_SAMg3$player1, player2vector)
CARL20_SAMg3$p2inp1vec <- is.element(CARL20_SAMg3$player2, player1vector)

addPlayer1 <- CARL20_SAMg3[ which(CARL20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_SAMg3[ which(CARL20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_SAMg2 <- rbind(CARL20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
CARL20_SAMft <- ftable(CARL20_SAMg2$player1, CARL20_SAMg2$player2)
CARL20_SAMft2 <- as.matrix(CARL20_SAMft)
numRows <- nrow(CARL20_SAMft2)
numCols <- ncol(CARL20_SAMft2)
CARL20_SAMft3 <- CARL20_SAMft2[c(2:numRows) , c(2:numCols)]
CARL20_SAMTable <- graph.adjacency(CARL20_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(CARL20_SAMTable, vertex.label = V(CARL20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
CARL20_SAM.clusterCoef <- transitivity(CARL20_SAMTable, type="global") #cluster coefficient
CARL20_SAM.degreeCent <- centralization.degree(CARL20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_SAMftn <- as.network.matrix(CARL20_SAMft)
CARL20_SAM.netDensity <- network.density(CARL20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_SAM.entropy <- entropy(CARL20_SAMft) #entropy

CARL20_SAM.netMx <- cbind(CARL20_SAM.netMx, CARL20_SAM.clusterCoef, CARL20_SAM.degreeCent$centralization,
                          CARL20_SAM.netDensity, CARL20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
CARL20_TAMg2 <- data.frame(CARL20_TAM)
CARL20_TAMg2 <- CARL20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_TAMg2$player1
player2vector <- CARL20_TAMg2$player2
CARL20_TAMg3 <- CARL20_TAMg2
CARL20_TAMg3$p1inp2vec <- is.element(CARL20_TAMg3$player1, player2vector)
CARL20_TAMg3$p2inp1vec <- is.element(CARL20_TAMg3$player2, player1vector)

addPlayer1 <- CARL20_TAMg3[ which(CARL20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_TAMg3[ which(CARL20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_TAMg2 <- rbind(CARL20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
CARL20_TAMft <- ftable(CARL20_TAMg2$player1, CARL20_TAMg2$player2)
CARL20_TAMft2 <- as.matrix(CARL20_TAMft)
numRows <- nrow(CARL20_TAMft2)
numCols <- ncol(CARL20_TAMft2)
CARL20_TAMft3 <- CARL20_TAMft2[c(2:numRows) , c(2:numCols)]
CARL20_TAMTable <- graph.adjacency(CARL20_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(CARL20_TAMTable, vertex.label = V(CARL20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
CARL20_TAM.clusterCoef <- transitivity(CARL20_TAMTable, type="global") #cluster coefficient
CARL20_TAM.degreeCent <- centralization.degree(CARL20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_TAMftn <- as.network.matrix(CARL20_TAMft)
CARL20_TAM.netDensity <- network.density(CARL20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_TAM.entropy <- entropy(CARL20_TAMft) #entropy

CARL20_TAM.netMx <- cbind(CARL20_TAM.netMx, CARL20_TAM.clusterCoef, CARL20_TAM.degreeCent$centralization,
                          CARL20_TAM.netDensity, CARL20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
CARL20_SDMg2 <- data.frame(CARL20_SDM)
CARL20_SDMg2 <- CARL20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_SDMg2$player1
player2vector <- CARL20_SDMg2$player2
CARL20_SDMg3 <- CARL20_SDMg2
CARL20_SDMg3$p1inp2vec <- is.element(CARL20_SDMg3$player1, player2vector)
CARL20_SDMg3$p2inp1vec <- is.element(CARL20_SDMg3$player2, player1vector)

addPlayer1 <- CARL20_SDMg3[ which(CARL20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_SDMg3[ which(CARL20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_SDMg2 <- rbind(CARL20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
CARL20_SDMft <- ftable(CARL20_SDMg2$player1, CARL20_SDMg2$player2)
CARL20_SDMft2 <- as.matrix(CARL20_SDMft)
numRows <- nrow(CARL20_SDMft2)
numCols <- ncol(CARL20_SDMft2)
CARL20_SDMft3 <- CARL20_SDMft2[c(2:numRows) , c(2:numCols)]
CARL20_SDMTable <- graph.adjacency(CARL20_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(CARL20_SDMTable, vertex.label = V(CARL20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
CARL20_SDM.clusterCoef <- transitivity(CARL20_SDMTable, type="global") #cluster coefficient
CARL20_SDM.degreeCent <- centralization.degree(CARL20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_SDMftn <- as.network.matrix(CARL20_SDMft)
CARL20_SDM.netDensity <- network.density(CARL20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_SDM.entropy <- entropy(CARL20_SDMft) #entropy

CARL20_SDM.netMx <- cbind(CARL20_SDM.netMx, CARL20_SDM.clusterCoef, CARL20_SDM.degreeCent$centralization,
                          CARL20_SDM.netDensity, CARL20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
CARL20_TDMg2 <- data.frame(CARL20_TDM)
CARL20_TDMg2 <- CARL20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_TDMg2$player1
player2vector <- CARL20_TDMg2$player2
CARL20_TDMg3 <- CARL20_TDMg2
CARL20_TDMg3$p1inp2vec <- is.element(CARL20_TDMg3$player1, player2vector)
CARL20_TDMg3$p2inp1vec <- is.element(CARL20_TDMg3$player2, player1vector)

addPlayer1 <- CARL20_TDMg3[ which(CARL20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_TDMg3[ which(CARL20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_TDMg2 <- rbind(CARL20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
CARL20_TDMft <- ftable(CARL20_TDMg2$player1, CARL20_TDMg2$player2)
CARL20_TDMft2 <- as.matrix(CARL20_TDMft)
numRows <- nrow(CARL20_TDMft2)
numCols <- ncol(CARL20_TDMft2)
CARL20_TDMft3 <- CARL20_TDMft2[c(2:numRows) , c(2:numCols)]
CARL20_TDMTable <- graph.adjacency(CARL20_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(CARL20_TDMTable, vertex.label = V(CARL20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
CARL20_TDM.clusterCoef <- transitivity(CARL20_TDMTable, type="global") #cluster coefficient
CARL20_TDM.degreeCent <- centralization.degree(CARL20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_TDMftn <- as.network.matrix(CARL20_TDMft)
CARL20_TDM.netDensity <- network.density(CARL20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_TDM.entropy <- entropy(CARL20_TDMft) #entropy

CARL20_TDM.netMx <- cbind(CARL20_TDM.netMx, CARL20_TDM.clusterCoef, CARL20_TDM.degreeCent$centralization,
                          CARL20_TDM.netDensity, CARL20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
CARL20_SDg2 <- data.frame(CARL20_SD)
CARL20_SDg2 <- CARL20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_SDg2$player1
player2vector <- CARL20_SDg2$player2
CARL20_SDg3 <- CARL20_SDg2
CARL20_SDg3$p1inp2vec <- is.element(CARL20_SDg3$player1, player2vector)
CARL20_SDg3$p2inp1vec <- is.element(CARL20_SDg3$player2, player1vector)

addPlayer1 <- CARL20_SDg3[ which(CARL20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_SDg3[ which(CARL20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_SDg2 <- rbind(CARL20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
CARL20_SDft <- ftable(CARL20_SDg2$player1, CARL20_SDg2$player2)
CARL20_SDft2 <- as.matrix(CARL20_SDft)
numRows <- nrow(CARL20_SDft2)
numCols <- ncol(CARL20_SDft2)
CARL20_SDft3 <- CARL20_SDft2[c(2:numRows) , c(2:numCols)]
CARL20_SDTable <- graph.adjacency(CARL20_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(CARL20_SDTable, vertex.label = V(CARL20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
CARL20_SD.clusterCoef <- transitivity(CARL20_SDTable, type="global") #cluster coefficient
CARL20_SD.degreeCent <- centralization.degree(CARL20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_SDftn <- as.network.matrix(CARL20_SDft)
CARL20_SD.netDensity <- network.density(CARL20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_SD.entropy <- entropy(CARL20_SDft) #entropy

CARL20_SD.netMx <- cbind(CARL20_SD.netMx, CARL20_SD.clusterCoef, CARL20_SD.degreeCent$centralization,
                         CARL20_SD.netDensity, CARL20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
CARL20_TDg2 <- data.frame(CARL20_TD)
CARL20_TDg2 <- CARL20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_TDg2$player1
player2vector <- CARL20_TDg2$player2
CARL20_TDg3 <- CARL20_TDg2
CARL20_TDg3$p1inp2vec <- is.element(CARL20_TDg3$player1, player2vector)
CARL20_TDg3$p2inp1vec <- is.element(CARL20_TDg3$player2, player1vector)

addPlayer1 <- CARL20_TDg3[ which(CARL20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_TDg3[ which(CARL20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_TDg2 <- rbind(CARL20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
CARL20_TDft <- ftable(CARL20_TDg2$player1, CARL20_TDg2$player2)
CARL20_TDft2 <- as.matrix(CARL20_TDft)
numRows <- nrow(CARL20_TDft2)
numCols <- ncol(CARL20_TDft2)
CARL20_TDft3 <- CARL20_TDft2[c(2:numRows) , c(2:numCols)]
CARL20_TDTable <- graph.adjacency(CARL20_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(CARL20_TDTable, vertex.label = V(CARL20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
CARL20_TD.clusterCoef <- transitivity(CARL20_TDTable, type="global") #cluster coefficient
CARL20_TD.degreeCent <- centralization.degree(CARL20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_TDftn <- as.network.matrix(CARL20_TDft)
CARL20_TD.netDensity <- network.density(CARL20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_TD.entropy <- entropy(CARL20_TDft) #entropy

CARL20_TD.netMx <- cbind(CARL20_TD.netMx, CARL20_TD.clusterCoef, CARL20_TD.degreeCent$centralization,
                         CARL20_TD.netDensity, CARL20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
CARL20_QTg2 <- data.frame(CARL20_QT)
CARL20_QTg2 <- CARL20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL20_QTg2$player1
player2vector <- CARL20_QTg2$player2
CARL20_QTg3 <- CARL20_QTg2
CARL20_QTg3$p1inp2vec <- is.element(CARL20_QTg3$player1, player2vector)
CARL20_QTg3$p2inp1vec <- is.element(CARL20_QTg3$player2, player1vector)

addPlayer1 <- CARL20_QTg3[ which(CARL20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL20_QTg3[ which(CARL20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL20_QTg2 <- rbind(CARL20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
CARL20_QTft <- ftable(CARL20_QTg2$player1, CARL20_QTg2$player2)
CARL20_QTft2 <- as.matrix(CARL20_QTft)
numRows <- nrow(CARL20_QTft2)
numCols <- ncol(CARL20_QTft2)
CARL20_QTft3 <- CARL20_QTft2[c(2:numRows) , c(2:numCols)]
CARL20_QTTable <- graph.adjacency(CARL20_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(CARL20_QTTable, vertex.label = V(CARL20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
CARL20_QT.clusterCoef <- transitivity(CARL20_QTTable, type="global") #cluster coefficient
CARL20_QT.degreeCent <- centralization.degree(CARL20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL20_QTftn <- as.network.matrix(CARL20_QTft)
CARL20_QT.netDensity <- network.density(CARL20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL20_QT.entropy <- entropy(CARL20_QTft) #entropy

CARL20_QT.netMx <- cbind(CARL20_QT.netMx, CARL20_QT.clusterCoef, CARL20_QT.degreeCent$centralization,
                         CARL20_QT.netDensity, CARL20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL20_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************
#NA

round = 20
teamName = "COLL"
KIoutcome = "Goal_F"
COLL20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
COLL20_Gg2 <- data.frame(COLL20_G)
COLL20_Gg2 <- COLL20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_Gg2$player1
player2vector <- COLL20_Gg2$player2
COLL20_Gg3 <- COLL20_Gg2
COLL20_Gg3$p1inp2vec <- is.element(COLL20_Gg3$player1, player2vector)
COLL20_Gg3$p2inp1vec <- is.element(COLL20_Gg3$player2, player1vector)

addPlayer1 <- COLL20_Gg3[ which(COLL20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_Gg3[ which(COLL20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_Gg2 <- rbind(COLL20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
COLL20_Gft <- ftable(COLL20_Gg2$player1, COLL20_Gg2$player2)
COLL20_Gft2 <- as.matrix(COLL20_Gft)
numRows <- nrow(COLL20_Gft2)
numCols <- ncol(COLL20_Gft2)
COLL20_Gft3 <- COLL20_Gft2[c(2:numRows) , c(2:numCols)]
COLL20_GTable <- graph.adjacency(COLL20_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(COLL20_GTable, vertex.label = V(COLL20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
COLL20_G.clusterCoef <- transitivity(COLL20_GTable, type="global") #cluster coefficient
COLL20_G.degreeCent <- centralization.degree(COLL20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_Gftn <- as.network.matrix(COLL20_Gft)
COLL20_G.netDensity <- network.density(COLL20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_G.entropy <- entropy(COLL20_Gft) #entropy

COLL20_G.netMx <- cbind(COLL20_G.netMx, COLL20_G.clusterCoef, COLL20_G.degreeCent$centralization,
                        COLL20_G.netDensity, COLL20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "COLL"
KIoutcome = "Behind_F"
COLL20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
COLL20_Bg2 <- data.frame(COLL20_B)
COLL20_Bg2 <- COLL20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_Bg2$player1
player2vector <- COLL20_Bg2$player2
COLL20_Bg3 <- COLL20_Bg2
COLL20_Bg3$p1inp2vec <- is.element(COLL20_Bg3$player1, player2vector)
COLL20_Bg3$p2inp1vec <- is.element(COLL20_Bg3$player2, player1vector)

addPlayer1 <- COLL20_Bg3[ which(COLL20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_Bg3[ which(COLL20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_Bg2 <- rbind(COLL20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
COLL20_Bft <- ftable(COLL20_Bg2$player1, COLL20_Bg2$player2)
COLL20_Bft2 <- as.matrix(COLL20_Bft)
numRows <- nrow(COLL20_Bft2)
numCols <- ncol(COLL20_Bft2)
COLL20_Bft3 <- COLL20_Bft2[c(2:numRows) , c(2:numCols)]
COLL20_BTable <- graph.adjacency(COLL20_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(COLL20_BTable, vertex.label = V(COLL20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
COLL20_B.clusterCoef <- transitivity(COLL20_BTable, type="global") #cluster coefficient
COLL20_B.degreeCent <- centralization.degree(COLL20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_Bftn <- as.network.matrix(COLL20_Bft)
COLL20_B.netDensity <- network.density(COLL20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_B.entropy <- entropy(COLL20_Bft) #entropy

COLL20_B.netMx <- cbind(COLL20_B.netMx, COLL20_B.clusterCoef, COLL20_B.degreeCent$centralization,
                        COLL20_B.netDensity, COLL20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
COLL20_SFg2 <- data.frame(COLL20_SF)
COLL20_SFg2 <- COLL20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_SFg2$player1
player2vector <- COLL20_SFg2$player2
COLL20_SFg3 <- COLL20_SFg2
COLL20_SFg3$p1inp2vec <- is.element(COLL20_SFg3$player1, player2vector)
COLL20_SFg3$p2inp1vec <- is.element(COLL20_SFg3$player2, player1vector)

addPlayer1 <- COLL20_SFg3[ which(COLL20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_SFg3[ which(COLL20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_SFg2 <- rbind(COLL20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
COLL20_SFft <- ftable(COLL20_SFg2$player1, COLL20_SFg2$player2)
COLL20_SFft2 <- as.matrix(COLL20_SFft)
numRows <- nrow(COLL20_SFft2)
numCols <- ncol(COLL20_SFft2)
COLL20_SFft3 <- COLL20_SFft2[c(2:numRows) , c(2:numCols)]
COLL20_SFTable <- graph.adjacency(COLL20_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(COLL20_SFTable, vertex.label = V(COLL20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
COLL20_SF.clusterCoef <- transitivity(COLL20_SFTable, type="global") #cluster coefficient
COLL20_SF.degreeCent <- centralization.degree(COLL20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_SFftn <- as.network.matrix(COLL20_SFft)
COLL20_SF.netDensity <- network.density(COLL20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_SF.entropy <- entropy(COLL20_SFft) #entropy

COLL20_SF.netMx <- cbind(COLL20_SF.netMx, COLL20_SF.clusterCoef, COLL20_SF.degreeCent$centralization,
                         COLL20_SF.netDensity, COLL20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
COLL20_TFg2 <- data.frame(COLL20_TF)
COLL20_TFg2 <- COLL20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_TFg2$player1
player2vector <- COLL20_TFg2$player2
COLL20_TFg3 <- COLL20_TFg2
COLL20_TFg3$p1inp2vec <- is.element(COLL20_TFg3$player1, player2vector)
COLL20_TFg3$p2inp1vec <- is.element(COLL20_TFg3$player2, player1vector)

addPlayer1 <- COLL20_TFg3[ which(COLL20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_TFg3[ which(COLL20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_TFg2 <- rbind(COLL20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
COLL20_TFft <- ftable(COLL20_TFg2$player1, COLL20_TFg2$player2)
COLL20_TFft2 <- as.matrix(COLL20_TFft)
numRows <- nrow(COLL20_TFft2)
numCols <- ncol(COLL20_TFft2)
COLL20_TFft3 <- COLL20_TFft2[c(2:numRows) , c(2:numCols)]
COLL20_TFTable <- graph.adjacency(COLL20_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(COLL20_TFTable, vertex.label = V(COLL20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
COLL20_TF.clusterCoef <- transitivity(COLL20_TFTable, type="global") #cluster coefficient
COLL20_TF.degreeCent <- centralization.degree(COLL20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_TFftn <- as.network.matrix(COLL20_TFft)
COLL20_TF.netDensity <- network.density(COLL20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_TF.entropy <- entropy(COLL20_TFft) #entropy

COLL20_TF.netMx <- cbind(COLL20_TF.netMx, COLL20_TF.clusterCoef, COLL20_TF.degreeCent$centralization,
                         COLL20_TF.netDensity, COLL20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************
#NA

round = 20
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
COLL20_SAMg2 <- data.frame(COLL20_SAM)
COLL20_SAMg2 <- COLL20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_SAMg2$player1
player2vector <- COLL20_SAMg2$player2
COLL20_SAMg3 <- COLL20_SAMg2
COLL20_SAMg3$p1inp2vec <- is.element(COLL20_SAMg3$player1, player2vector)
COLL20_SAMg3$p2inp1vec <- is.element(COLL20_SAMg3$player2, player1vector)

addPlayer1 <- COLL20_SAMg3[ which(COLL20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_SAMg3[ which(COLL20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_SAMg2 <- rbind(COLL20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
COLL20_SAMft <- ftable(COLL20_SAMg2$player1, COLL20_SAMg2$player2)
COLL20_SAMft2 <- as.matrix(COLL20_SAMft)
numRows <- nrow(COLL20_SAMft2)
numCols <- ncol(COLL20_SAMft2)
COLL20_SAMft3 <- COLL20_SAMft2[c(2:numRows) , c(2:numCols)]
COLL20_SAMTable <- graph.adjacency(COLL20_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(COLL20_SAMTable, vertex.label = V(COLL20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
COLL20_SAM.clusterCoef <- transitivity(COLL20_SAMTable, type="global") #cluster coefficient
COLL20_SAM.degreeCent <- centralization.degree(COLL20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_SAMftn <- as.network.matrix(COLL20_SAMft)
COLL20_SAM.netDensity <- network.density(COLL20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_SAM.entropy <- entropy(COLL20_SAMft) #entropy

COLL20_SAM.netMx <- cbind(COLL20_SAM.netMx, COLL20_SAM.clusterCoef, COLL20_SAM.degreeCent$centralization,
                          COLL20_SAM.netDensity, COLL20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
COLL20_TAMg2 <- data.frame(COLL20_TAM)
COLL20_TAMg2 <- COLL20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_TAMg2$player1
player2vector <- COLL20_TAMg2$player2
COLL20_TAMg3 <- COLL20_TAMg2
COLL20_TAMg3$p1inp2vec <- is.element(COLL20_TAMg3$player1, player2vector)
COLL20_TAMg3$p2inp1vec <- is.element(COLL20_TAMg3$player2, player1vector)

addPlayer1 <- COLL20_TAMg3[ which(COLL20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_TAMg3[ which(COLL20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_TAMg2 <- rbind(COLL20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
COLL20_TAMft <- ftable(COLL20_TAMg2$player1, COLL20_TAMg2$player2)
COLL20_TAMft2 <- as.matrix(COLL20_TAMft)
numRows <- nrow(COLL20_TAMft2)
numCols <- ncol(COLL20_TAMft2)
COLL20_TAMft3 <- COLL20_TAMft2[c(2:numRows) , c(2:numCols)]
COLL20_TAMTable <- graph.adjacency(COLL20_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(COLL20_TAMTable, vertex.label = V(COLL20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
COLL20_TAM.clusterCoef <- transitivity(COLL20_TAMTable, type="global") #cluster coefficient
COLL20_TAM.degreeCent <- centralization.degree(COLL20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_TAMftn <- as.network.matrix(COLL20_TAMft)
COLL20_TAM.netDensity <- network.density(COLL20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_TAM.entropy <- entropy(COLL20_TAMft) #entropy

COLL20_TAM.netMx <- cbind(COLL20_TAM.netMx, COLL20_TAM.clusterCoef, COLL20_TAM.degreeCent$centralization,
                          COLL20_TAM.netDensity, COLL20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************
#NA

round = 20
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
COLL20_SDMg2 <- data.frame(COLL20_SDM)
COLL20_SDMg2 <- COLL20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_SDMg2$player1
player2vector <- COLL20_SDMg2$player2
COLL20_SDMg3 <- COLL20_SDMg2
COLL20_SDMg3$p1inp2vec <- is.element(COLL20_SDMg3$player1, player2vector)
COLL20_SDMg3$p2inp1vec <- is.element(COLL20_SDMg3$player2, player1vector)

addPlayer1 <- COLL20_SDMg3[ which(COLL20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_SDMg3[ which(COLL20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_SDMg2 <- rbind(COLL20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
COLL20_SDMft <- ftable(COLL20_SDMg2$player1, COLL20_SDMg2$player2)
COLL20_SDMft2 <- as.matrix(COLL20_SDMft)
numRows <- nrow(COLL20_SDMft2)
numCols <- ncol(COLL20_SDMft2)
COLL20_SDMft3 <- COLL20_SDMft2[c(2:numRows) , c(2:numCols)]
COLL20_SDMTable <- graph.adjacency(COLL20_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(COLL20_SDMTable, vertex.label = V(COLL20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
COLL20_SDM.clusterCoef <- transitivity(COLL20_SDMTable, type="global") #cluster coefficient
COLL20_SDM.degreeCent <- centralization.degree(COLL20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_SDMftn <- as.network.matrix(COLL20_SDMft)
COLL20_SDM.netDensity <- network.density(COLL20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_SDM.entropy <- entropy(COLL20_SDMft) #entropy

COLL20_SDM.netMx <- cbind(COLL20_SDM.netMx, COLL20_SDM.clusterCoef, COLL20_SDM.degreeCent$centralization,
                          COLL20_SDM.netDensity, COLL20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
COLL20_TDMg2 <- data.frame(COLL20_TDM)
COLL20_TDMg2 <- COLL20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_TDMg2$player1
player2vector <- COLL20_TDMg2$player2
COLL20_TDMg3 <- COLL20_TDMg2
COLL20_TDMg3$p1inp2vec <- is.element(COLL20_TDMg3$player1, player2vector)
COLL20_TDMg3$p2inp1vec <- is.element(COLL20_TDMg3$player2, player1vector)

addPlayer1 <- COLL20_TDMg3[ which(COLL20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_TDMg3[ which(COLL20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_TDMg2 <- rbind(COLL20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
COLL20_TDMft <- ftable(COLL20_TDMg2$player1, COLL20_TDMg2$player2)
COLL20_TDMft2 <- as.matrix(COLL20_TDMft)
numRows <- nrow(COLL20_TDMft2)
numCols <- ncol(COLL20_TDMft2)
COLL20_TDMft3 <- COLL20_TDMft2[c(2:numRows) , c(2:numCols)]
COLL20_TDMTable <- graph.adjacency(COLL20_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(COLL20_TDMTable, vertex.label = V(COLL20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
COLL20_TDM.clusterCoef <- transitivity(COLL20_TDMTable, type="global") #cluster coefficient
COLL20_TDM.degreeCent <- centralization.degree(COLL20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_TDMftn <- as.network.matrix(COLL20_TDMft)
COLL20_TDM.netDensity <- network.density(COLL20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_TDM.entropy <- entropy(COLL20_TDMft) #entropy

COLL20_TDM.netMx <- cbind(COLL20_TDM.netMx, COLL20_TDM.clusterCoef, COLL20_TDM.degreeCent$centralization,
                          COLL20_TDM.netDensity, COLL20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
COLL20_SDg2 <- data.frame(COLL20_SD)
COLL20_SDg2 <- COLL20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_SDg2$player1
player2vector <- COLL20_SDg2$player2
COLL20_SDg3 <- COLL20_SDg2
COLL20_SDg3$p1inp2vec <- is.element(COLL20_SDg3$player1, player2vector)
COLL20_SDg3$p2inp1vec <- is.element(COLL20_SDg3$player2, player1vector)

addPlayer1 <- COLL20_SDg3[ which(COLL20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_SDg3[ which(COLL20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_SDg2 <- rbind(COLL20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
COLL20_SDft <- ftable(COLL20_SDg2$player1, COLL20_SDg2$player2)
COLL20_SDft2 <- as.matrix(COLL20_SDft)
numRows <- nrow(COLL20_SDft2)
numCols <- ncol(COLL20_SDft2)
COLL20_SDft3 <- COLL20_SDft2[c(2:numRows) , c(2:numCols)]
COLL20_SDTable <- graph.adjacency(COLL20_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(COLL20_SDTable, vertex.label = V(COLL20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
COLL20_SD.clusterCoef <- transitivity(COLL20_SDTable, type="global") #cluster coefficient
COLL20_SD.degreeCent <- centralization.degree(COLL20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_SDftn <- as.network.matrix(COLL20_SDft)
COLL20_SD.netDensity <- network.density(COLL20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_SD.entropy <- entropy(COLL20_SDft) #entropy

COLL20_SD.netMx <- cbind(COLL20_SD.netMx, COLL20_SD.clusterCoef, COLL20_SD.degreeCent$centralization,
                         COLL20_SD.netDensity, COLL20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
COLL20_TDg2 <- data.frame(COLL20_TD)
COLL20_TDg2 <- COLL20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_TDg2$player1
player2vector <- COLL20_TDg2$player2
COLL20_TDg3 <- COLL20_TDg2
COLL20_TDg3$p1inp2vec <- is.element(COLL20_TDg3$player1, player2vector)
COLL20_TDg3$p2inp1vec <- is.element(COLL20_TDg3$player2, player1vector)

addPlayer1 <- COLL20_TDg3[ which(COLL20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_TDg3[ which(COLL20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_TDg2 <- rbind(COLL20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
COLL20_TDft <- ftable(COLL20_TDg2$player1, COLL20_TDg2$player2)
COLL20_TDft2 <- as.matrix(COLL20_TDft)
numRows <- nrow(COLL20_TDft2)
numCols <- ncol(COLL20_TDft2)
COLL20_TDft3 <- COLL20_TDft2[c(2:numRows) , c(2:numCols)]
COLL20_TDTable <- graph.adjacency(COLL20_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(COLL20_TDTable, vertex.label = V(COLL20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
COLL20_TD.clusterCoef <- transitivity(COLL20_TDTable, type="global") #cluster coefficient
COLL20_TD.degreeCent <- centralization.degree(COLL20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_TDftn <- as.network.matrix(COLL20_TDft)
COLL20_TD.netDensity <- network.density(COLL20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_TD.entropy <- entropy(COLL20_TDft) #entropy

COLL20_TD.netMx <- cbind(COLL20_TD.netMx, COLL20_TD.clusterCoef, COLL20_TD.degreeCent$centralization,
                         COLL20_TD.netDensity, COLL20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
COLL20_QTg2 <- data.frame(COLL20_QT)
COLL20_QTg2 <- COLL20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL20_QTg2$player1
player2vector <- COLL20_QTg2$player2
COLL20_QTg3 <- COLL20_QTg2
COLL20_QTg3$p1inp2vec <- is.element(COLL20_QTg3$player1, player2vector)
COLL20_QTg3$p2inp1vec <- is.element(COLL20_QTg3$player2, player1vector)

addPlayer1 <- COLL20_QTg3[ which(COLL20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL20_QTg3[ which(COLL20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL20_QTg2 <- rbind(COLL20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
COLL20_QTft <- ftable(COLL20_QTg2$player1, COLL20_QTg2$player2)
COLL20_QTft2 <- as.matrix(COLL20_QTft)
numRows <- nrow(COLL20_QTft2)
numCols <- ncol(COLL20_QTft2)
COLL20_QTft3 <- COLL20_QTft2[c(2:numRows) , c(2:numCols)]
COLL20_QTTable <- graph.adjacency(COLL20_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(COLL20_QTTable, vertex.label = V(COLL20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
COLL20_QT.clusterCoef <- transitivity(COLL20_QTTable, type="global") #cluster coefficient
COLL20_QT.degreeCent <- centralization.degree(COLL20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL20_QTftn <- as.network.matrix(COLL20_QTft)
COLL20_QT.netDensity <- network.density(COLL20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL20_QT.entropy <- entropy(COLL20_QTft) #entropy

COLL20_QT.netMx <- cbind(COLL20_QT.netMx, COLL20_QT.clusterCoef, COLL20_QT.degreeCent$centralization,
                         COLL20_QT.netDensity, COLL20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL20_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************
#NA

round = 20
teamName = "ESS"
KIoutcome = "Goal_F"
ESS20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
ESS20_Gg2 <- data.frame(ESS20_G)
ESS20_Gg2 <- ESS20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_Gg2$player1
player2vector <- ESS20_Gg2$player2
ESS20_Gg3 <- ESS20_Gg2
ESS20_Gg3$p1inp2vec <- is.element(ESS20_Gg3$player1, player2vector)
ESS20_Gg3$p2inp1vec <- is.element(ESS20_Gg3$player2, player1vector)

addPlayer1 <- ESS20_Gg3[ which(ESS20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_Gg3[ which(ESS20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_Gg2 <- rbind(ESS20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
ESS20_Gft <- ftable(ESS20_Gg2$player1, ESS20_Gg2$player2)
ESS20_Gft2 <- as.matrix(ESS20_Gft)
numRows <- nrow(ESS20_Gft2)
numCols <- ncol(ESS20_Gft2)
ESS20_Gft3 <- ESS20_Gft2[c(2:numRows) , c(2:numCols)]
ESS20_GTable <- graph.adjacency(ESS20_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(ESS20_GTable, vertex.label = V(ESS20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
ESS20_G.clusterCoef <- transitivity(ESS20_GTable, type="global") #cluster coefficient
ESS20_G.degreeCent <- centralization.degree(ESS20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_Gftn <- as.network.matrix(ESS20_Gft)
ESS20_G.netDensity <- network.density(ESS20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_G.entropy <- entropy(ESS20_Gft) #entropy

ESS20_G.netMx <- cbind(ESS20_G.netMx, ESS20_G.clusterCoef, ESS20_G.degreeCent$centralization,
                       ESS20_G.netDensity, ESS20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "ESS"
KIoutcome = "Behind_F"
ESS20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
ESS20_Bg2 <- data.frame(ESS20_B)
ESS20_Bg2 <- ESS20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_Bg2$player1
player2vector <- ESS20_Bg2$player2
ESS20_Bg3 <- ESS20_Bg2
ESS20_Bg3$p1inp2vec <- is.element(ESS20_Bg3$player1, player2vector)
ESS20_Bg3$p2inp1vec <- is.element(ESS20_Bg3$player2, player1vector)

addPlayer1 <- ESS20_Bg3[ which(ESS20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_Bg3[ which(ESS20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_Bg2 <- rbind(ESS20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
ESS20_Bft <- ftable(ESS20_Bg2$player1, ESS20_Bg2$player2)
ESS20_Bft2 <- as.matrix(ESS20_Bft)
numRows <- nrow(ESS20_Bft2)
numCols <- ncol(ESS20_Bft2)
ESS20_Bft3 <- ESS20_Bft2[c(2:numRows) , c(2:numCols)]
ESS20_BTable <- graph.adjacency(ESS20_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(ESS20_BTable, vertex.label = V(ESS20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
ESS20_B.clusterCoef <- transitivity(ESS20_BTable, type="global") #cluster coefficient
ESS20_B.degreeCent <- centralization.degree(ESS20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_Bftn <- as.network.matrix(ESS20_Bft)
ESS20_B.netDensity <- network.density(ESS20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_B.entropy <- entropy(ESS20_Bft) #entropy

ESS20_B.netMx <- cbind(ESS20_B.netMx, ESS20_B.clusterCoef, ESS20_B.degreeCent$centralization,
                       ESS20_B.netDensity, ESS20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
ESS20_SFg2 <- data.frame(ESS20_SF)
ESS20_SFg2 <- ESS20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_SFg2$player1
player2vector <- ESS20_SFg2$player2
ESS20_SFg3 <- ESS20_SFg2
ESS20_SFg3$p1inp2vec <- is.element(ESS20_SFg3$player1, player2vector)
ESS20_SFg3$p2inp1vec <- is.element(ESS20_SFg3$player2, player1vector)

addPlayer1 <- ESS20_SFg3[ which(ESS20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_SFg3[ which(ESS20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_SFg2 <- rbind(ESS20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
ESS20_SFft <- ftable(ESS20_SFg2$player1, ESS20_SFg2$player2)
ESS20_SFft2 <- as.matrix(ESS20_SFft)
numRows <- nrow(ESS20_SFft2)
numCols <- ncol(ESS20_SFft2)
ESS20_SFft3 <- ESS20_SFft2[c(2:numRows) , c(2:numCols)]
ESS20_SFTable <- graph.adjacency(ESS20_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(ESS20_SFTable, vertex.label = V(ESS20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
ESS20_SF.clusterCoef <- transitivity(ESS20_SFTable, type="global") #cluster coefficient
ESS20_SF.degreeCent <- centralization.degree(ESS20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_SFftn <- as.network.matrix(ESS20_SFft)
ESS20_SF.netDensity <- network.density(ESS20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_SF.entropy <- entropy(ESS20_SFft) #entropy

ESS20_SF.netMx <- cbind(ESS20_SF.netMx, ESS20_SF.clusterCoef, ESS20_SF.degreeCent$centralization,
                        ESS20_SF.netDensity, ESS20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
ESS20_TFg2 <- data.frame(ESS20_TF)
ESS20_TFg2 <- ESS20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_TFg2$player1
player2vector <- ESS20_TFg2$player2
ESS20_TFg3 <- ESS20_TFg2
ESS20_TFg3$p1inp2vec <- is.element(ESS20_TFg3$player1, player2vector)
ESS20_TFg3$p2inp1vec <- is.element(ESS20_TFg3$player2, player1vector)

addPlayer1 <- ESS20_TFg3[ which(ESS20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_TFg3[ which(ESS20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_TFg2 <- rbind(ESS20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
ESS20_TFft <- ftable(ESS20_TFg2$player1, ESS20_TFg2$player2)
ESS20_TFft2 <- as.matrix(ESS20_TFft)
numRows <- nrow(ESS20_TFft2)
numCols <- ncol(ESS20_TFft2)
ESS20_TFft3 <- ESS20_TFft2[c(2:numRows) , c(2:numCols)]
ESS20_TFTable <- graph.adjacency(ESS20_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(ESS20_TFTable, vertex.label = V(ESS20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
ESS20_TF.clusterCoef <- transitivity(ESS20_TFTable, type="global") #cluster coefficient
ESS20_TF.degreeCent <- centralization.degree(ESS20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_TFftn <- as.network.matrix(ESS20_TFft)
ESS20_TF.netDensity <- network.density(ESS20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_TF.entropy <- entropy(ESS20_TFft) #entropy

ESS20_TF.netMx <- cbind(ESS20_TF.netMx, ESS20_TF.clusterCoef, ESS20_TF.degreeCent$centralization,
                        ESS20_TF.netDensity, ESS20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************

round = 20
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
ESS20_SAMg2 <- data.frame(ESS20_SAM)
ESS20_SAMg2 <- ESS20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_SAMg2$player1
player2vector <- ESS20_SAMg2$player2
ESS20_SAMg3 <- ESS20_SAMg2
ESS20_SAMg3$p1inp2vec <- is.element(ESS20_SAMg3$player1, player2vector)
ESS20_SAMg3$p2inp1vec <- is.element(ESS20_SAMg3$player2, player1vector)

addPlayer1 <- ESS20_SAMg3[ which(ESS20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_SAMg3[ which(ESS20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_SAMg2 <- rbind(ESS20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
ESS20_SAMft <- ftable(ESS20_SAMg2$player1, ESS20_SAMg2$player2)
ESS20_SAMft2 <- as.matrix(ESS20_SAMft)
numRows <- nrow(ESS20_SAMft2)
numCols <- ncol(ESS20_SAMft2)
ESS20_SAMft3 <- ESS20_SAMft2[c(2:numRows) , c(2:numCols)]
ESS20_SAMTable <- graph.adjacency(ESS20_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(ESS20_SAMTable, vertex.label = V(ESS20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
ESS20_SAM.clusterCoef <- transitivity(ESS20_SAMTable, type="global") #cluster coefficient
ESS20_SAM.degreeCent <- centralization.degree(ESS20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_SAMftn <- as.network.matrix(ESS20_SAMft)
ESS20_SAM.netDensity <- network.density(ESS20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_SAM.entropy <- entropy(ESS20_SAMft) #entropy

ESS20_SAM.netMx <- cbind(ESS20_SAM.netMx, ESS20_SAM.clusterCoef, ESS20_SAM.degreeCent$centralization,
                         ESS20_SAM.netDensity, ESS20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
ESS20_TAMg2 <- data.frame(ESS20_TAM)
ESS20_TAMg2 <- ESS20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_TAMg2$player1
player2vector <- ESS20_TAMg2$player2
ESS20_TAMg3 <- ESS20_TAMg2
ESS20_TAMg3$p1inp2vec <- is.element(ESS20_TAMg3$player1, player2vector)
ESS20_TAMg3$p2inp1vec <- is.element(ESS20_TAMg3$player2, player1vector)

addPlayer1 <- ESS20_TAMg3[ which(ESS20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_TAMg3[ which(ESS20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_TAMg2 <- rbind(ESS20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
ESS20_TAMft <- ftable(ESS20_TAMg2$player1, ESS20_TAMg2$player2)
ESS20_TAMft2 <- as.matrix(ESS20_TAMft)
numRows <- nrow(ESS20_TAMft2)
numCols <- ncol(ESS20_TAMft2)
ESS20_TAMft3 <- ESS20_TAMft2[c(2:numRows) , c(2:numCols)]
ESS20_TAMTable <- graph.adjacency(ESS20_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(ESS20_TAMTable, vertex.label = V(ESS20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
ESS20_TAM.clusterCoef <- transitivity(ESS20_TAMTable, type="global") #cluster coefficient
ESS20_TAM.degreeCent <- centralization.degree(ESS20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_TAMftn <- as.network.matrix(ESS20_TAMft)
ESS20_TAM.netDensity <- network.density(ESS20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_TAM.entropy <- entropy(ESS20_TAMft) #entropy

ESS20_TAM.netMx <- cbind(ESS20_TAM.netMx, ESS20_TAM.clusterCoef, ESS20_TAM.degreeCent$centralization,
                         ESS20_TAM.netDensity, ESS20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
ESS20_SDMg2 <- data.frame(ESS20_SDM)
ESS20_SDMg2 <- ESS20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_SDMg2$player1
player2vector <- ESS20_SDMg2$player2
ESS20_SDMg3 <- ESS20_SDMg2
ESS20_SDMg3$p1inp2vec <- is.element(ESS20_SDMg3$player1, player2vector)
ESS20_SDMg3$p2inp1vec <- is.element(ESS20_SDMg3$player2, player1vector)

addPlayer1 <- ESS20_SDMg3[ which(ESS20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- ESS20_SDMg3[ which(ESS20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_SDMg2 <- rbind(ESS20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
ESS20_SDMft <- ftable(ESS20_SDMg2$player1, ESS20_SDMg2$player2)
ESS20_SDMft2 <- as.matrix(ESS20_SDMft)
numRows <- nrow(ESS20_SDMft2)
numCols <- ncol(ESS20_SDMft2)
ESS20_SDMft3 <- ESS20_SDMft2[c(2:numRows) , c(2:numCols)]
ESS20_SDMTable <- graph.adjacency(ESS20_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(ESS20_SDMTable, vertex.label = V(ESS20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
ESS20_SDM.clusterCoef <- transitivity(ESS20_SDMTable, type="global") #cluster coefficient
ESS20_SDM.degreeCent <- centralization.degree(ESS20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_SDMftn <- as.network.matrix(ESS20_SDMft)
ESS20_SDM.netDensity <- network.density(ESS20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_SDM.entropy <- entropy(ESS20_SDMft) #entropy

ESS20_SDM.netMx <- cbind(ESS20_SDM.netMx, ESS20_SDM.clusterCoef, ESS20_SDM.degreeCent$centralization,
                         ESS20_SDM.netDensity, ESS20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
ESS20_TDMg2 <- data.frame(ESS20_TDM)
ESS20_TDMg2 <- ESS20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_TDMg2$player1
player2vector <- ESS20_TDMg2$player2
ESS20_TDMg3 <- ESS20_TDMg2
ESS20_TDMg3$p1inp2vec <- is.element(ESS20_TDMg3$player1, player2vector)
ESS20_TDMg3$p2inp1vec <- is.element(ESS20_TDMg3$player2, player1vector)

addPlayer1 <- ESS20_TDMg3[ which(ESS20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_TDMg3[ which(ESS20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_TDMg2 <- rbind(ESS20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
ESS20_TDMft <- ftable(ESS20_TDMg2$player1, ESS20_TDMg2$player2)
ESS20_TDMft2 <- as.matrix(ESS20_TDMft)
numRows <- nrow(ESS20_TDMft2)
numCols <- ncol(ESS20_TDMft2)
ESS20_TDMft3 <- ESS20_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS20_TDMTable <- graph.adjacency(ESS20_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(ESS20_TDMTable, vertex.label = V(ESS20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
ESS20_TDM.clusterCoef <- transitivity(ESS20_TDMTable, type="global") #cluster coefficient
ESS20_TDM.degreeCent <- centralization.degree(ESS20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_TDMftn <- as.network.matrix(ESS20_TDMft)
ESS20_TDM.netDensity <- network.density(ESS20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_TDM.entropy <- entropy(ESS20_TDMft) #entropy

ESS20_TDM.netMx <- cbind(ESS20_TDM.netMx, ESS20_TDM.clusterCoef, ESS20_TDM.degreeCent$centralization,
                         ESS20_TDM.netDensity, ESS20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
ESS20_SDg2 <- data.frame(ESS20_SD)
ESS20_SDg2 <- ESS20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_SDg2$player1
player2vector <- ESS20_SDg2$player2
ESS20_SDg3 <- ESS20_SDg2
ESS20_SDg3$p1inp2vec <- is.element(ESS20_SDg3$player1, player2vector)
ESS20_SDg3$p2inp1vec <- is.element(ESS20_SDg3$player2, player1vector)

addPlayer1 <- ESS20_SDg3[ which(ESS20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_SDg3[ which(ESS20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_SDg2 <- rbind(ESS20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
ESS20_SDft <- ftable(ESS20_SDg2$player1, ESS20_SDg2$player2)
ESS20_SDft2 <- as.matrix(ESS20_SDft)
numRows <- nrow(ESS20_SDft2)
numCols <- ncol(ESS20_SDft2)
ESS20_SDft3 <- ESS20_SDft2[c(2:numRows) , c(2:numCols)]
ESS20_SDTable <- graph.adjacency(ESS20_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(ESS20_SDTable, vertex.label = V(ESS20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
ESS20_SD.clusterCoef <- transitivity(ESS20_SDTable, type="global") #cluster coefficient
ESS20_SD.degreeCent <- centralization.degree(ESS20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_SDftn <- as.network.matrix(ESS20_SDft)
ESS20_SD.netDensity <- network.density(ESS20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_SD.entropy <- entropy(ESS20_SDft) #entropy

ESS20_SD.netMx <- cbind(ESS20_SD.netMx, ESS20_SD.clusterCoef, ESS20_SD.degreeCent$centralization,
                        ESS20_SD.netDensity, ESS20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
ESS20_TDg2 <- data.frame(ESS20_TD)
ESS20_TDg2 <- ESS20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_TDg2$player1
player2vector <- ESS20_TDg2$player2
ESS20_TDg3 <- ESS20_TDg2
ESS20_TDg3$p1inp2vec <- is.element(ESS20_TDg3$player1, player2vector)
ESS20_TDg3$p2inp1vec <- is.element(ESS20_TDg3$player2, player1vector)

addPlayer1 <- ESS20_TDg3[ which(ESS20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_TDg3[ which(ESS20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_TDg2 <- rbind(ESS20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
ESS20_TDft <- ftable(ESS20_TDg2$player1, ESS20_TDg2$player2)
ESS20_TDft2 <- as.matrix(ESS20_TDft)
numRows <- nrow(ESS20_TDft2)
numCols <- ncol(ESS20_TDft2)
ESS20_TDft3 <- ESS20_TDft2[c(2:numRows) , c(2:numCols)]
ESS20_TDTable <- graph.adjacency(ESS20_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(ESS20_TDTable, vertex.label = V(ESS20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
ESS20_TD.clusterCoef <- transitivity(ESS20_TDTable, type="global") #cluster coefficient
ESS20_TD.degreeCent <- centralization.degree(ESS20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_TDftn <- as.network.matrix(ESS20_TDft)
ESS20_TD.netDensity <- network.density(ESS20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_TD.entropy <- entropy(ESS20_TDft) #entropy

ESS20_TD.netMx <- cbind(ESS20_TD.netMx, ESS20_TD.clusterCoef, ESS20_TD.degreeCent$centralization,
                        ESS20_TD.netDensity, ESS20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
ESS20_QTg2 <- data.frame(ESS20_QT)
ESS20_QTg2 <- ESS20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS20_QTg2$player1
player2vector <- ESS20_QTg2$player2
ESS20_QTg3 <- ESS20_QTg2
ESS20_QTg3$p1inp2vec <- is.element(ESS20_QTg3$player1, player2vector)
ESS20_QTg3$p2inp1vec <- is.element(ESS20_QTg3$player2, player1vector)

addPlayer1 <- ESS20_QTg3[ which(ESS20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS20_QTg3[ which(ESS20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS20_QTg2 <- rbind(ESS20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
ESS20_QTft <- ftable(ESS20_QTg2$player1, ESS20_QTg2$player2)
ESS20_QTft2 <- as.matrix(ESS20_QTft)
numRows <- nrow(ESS20_QTft2)
numCols <- ncol(ESS20_QTft2)
ESS20_QTft3 <- ESS20_QTft2[c(2:numRows) , c(2:numCols)]
ESS20_QTTable <- graph.adjacency(ESS20_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(ESS20_QTTable, vertex.label = V(ESS20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
ESS20_QT.clusterCoef <- transitivity(ESS20_QTTable, type="global") #cluster coefficient
ESS20_QT.degreeCent <- centralization.degree(ESS20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS20_QTftn <- as.network.matrix(ESS20_QTft)
ESS20_QT.netDensity <- network.density(ESS20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS20_QT.entropy <- entropy(ESS20_QTft) #entropy

ESS20_QT.netMx <- cbind(ESS20_QT.netMx, ESS20_QT.clusterCoef, ESS20_QT.degreeCent$centralization,
                        ESS20_QT.netDensity, ESS20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS20_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "FRE"
KIoutcome = "Goal_F"
FRE20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
FRE20_Gg2 <- data.frame(FRE20_G)
FRE20_Gg2 <- FRE20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_Gg2$player1
player2vector <- FRE20_Gg2$player2
FRE20_Gg3 <- FRE20_Gg2
FRE20_Gg3$p1inp2vec <- is.element(FRE20_Gg3$player1, player2vector)
FRE20_Gg3$p2inp1vec <- is.element(FRE20_Gg3$player2, player1vector)

addPlayer1 <- FRE20_Gg3[ which(FRE20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- FRE20_Gg3[ which(FRE20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_Gg2 <- rbind(FRE20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
FRE20_Gft <- ftable(FRE20_Gg2$player1, FRE20_Gg2$player2)
FRE20_Gft2 <- as.matrix(FRE20_Gft)
numRows <- nrow(FRE20_Gft2)
numCols <- ncol(FRE20_Gft2)
FRE20_Gft3 <- FRE20_Gft2[c(2:numRows) , c(2:numCols)]
FRE20_GTable <- graph.adjacency(FRE20_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(FRE20_GTable, vertex.label = V(FRE20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
FRE20_G.clusterCoef <- transitivity(FRE20_GTable, type="global") #cluster coefficient
FRE20_G.degreeCent <- centralization.degree(FRE20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_Gftn <- as.network.matrix(FRE20_Gft)
FRE20_G.netDensity <- network.density(FRE20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_G.entropy <- entropy(FRE20_Gft) #entropy

FRE20_G.netMx <- cbind(FRE20_G.netMx, FRE20_G.clusterCoef, FRE20_G.degreeCent$centralization,
                       FRE20_G.netDensity, FRE20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************

round = 20
teamName = "FRE"
KIoutcome = "Behind_F"
FRE20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
FRE20_Bg2 <- data.frame(FRE20_B)
FRE20_Bg2 <- FRE20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_Bg2$player1
player2vector <- FRE20_Bg2$player2
FRE20_Bg3 <- FRE20_Bg2
FRE20_Bg3$p1inp2vec <- is.element(FRE20_Bg3$player1, player2vector)
FRE20_Bg3$p2inp1vec <- is.element(FRE20_Bg3$player2, player1vector)

addPlayer1 <- FRE20_Bg3[ which(FRE20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_Bg3[ which(FRE20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_Bg2 <- rbind(FRE20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
FRE20_Bft <- ftable(FRE20_Bg2$player1, FRE20_Bg2$player2)
FRE20_Bft2 <- as.matrix(FRE20_Bft)
numRows <- nrow(FRE20_Bft2)
numCols <- ncol(FRE20_Bft2)
FRE20_Bft3 <- FRE20_Bft2[c(2:numRows) , c(2:numCols)]
FRE20_BTable <- graph.adjacency(FRE20_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(FRE20_BTable, vertex.label = V(FRE20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
FRE20_B.clusterCoef <- transitivity(FRE20_BTable, type="global") #cluster coefficient
FRE20_B.degreeCent <- centralization.degree(FRE20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_Bftn <- as.network.matrix(FRE20_Bft)
FRE20_B.netDensity <- network.density(FRE20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_B.entropy <- entropy(FRE20_Bft) #entropy

FRE20_B.netMx <- cbind(FRE20_B.netMx, FRE20_B.clusterCoef, FRE20_B.degreeCent$centralization,
                       FRE20_B.netDensity, FRE20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************

round = 20
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
FRE20_SFg2 <- data.frame(FRE20_SF)
FRE20_SFg2 <- FRE20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_SFg2$player1
player2vector <- FRE20_SFg2$player2
FRE20_SFg3 <- FRE20_SFg2
FRE20_SFg3$p1inp2vec <- is.element(FRE20_SFg3$player1, player2vector)
FRE20_SFg3$p2inp1vec <- is.element(FRE20_SFg3$player2, player1vector)

addPlayer1 <- FRE20_SFg3[ which(FRE20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_SFg3[ which(FRE20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_SFg2 <- rbind(FRE20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
FRE20_SFft <- ftable(FRE20_SFg2$player1, FRE20_SFg2$player2)
FRE20_SFft2 <- as.matrix(FRE20_SFft)
numRows <- nrow(FRE20_SFft2)
numCols <- ncol(FRE20_SFft2)
FRE20_SFft3 <- FRE20_SFft2[c(2:numRows) , c(2:numCols)]
FRE20_SFTable <- graph.adjacency(FRE20_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(FRE20_SFTable, vertex.label = V(FRE20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
FRE20_SF.clusterCoef <- transitivity(FRE20_SFTable, type="global") #cluster coefficient
FRE20_SF.degreeCent <- centralization.degree(FRE20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_SFftn <- as.network.matrix(FRE20_SFft)
FRE20_SF.netDensity <- network.density(FRE20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_SF.entropy <- entropy(FRE20_SFft) #entropy

FRE20_SF.netMx <- cbind(FRE20_SF.netMx, FRE20_SF.clusterCoef, FRE20_SF.degreeCent$centralization,
                        FRE20_SF.netDensity, FRE20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
FRE20_TFg2 <- data.frame(FRE20_TF)
FRE20_TFg2 <- FRE20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_TFg2$player1
player2vector <- FRE20_TFg2$player2
FRE20_TFg3 <- FRE20_TFg2
FRE20_TFg3$p1inp2vec <- is.element(FRE20_TFg3$player1, player2vector)
FRE20_TFg3$p2inp1vec <- is.element(FRE20_TFg3$player2, player1vector)

addPlayer1 <- FRE20_TFg3[ which(FRE20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_TFg3[ which(FRE20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_TFg2 <- rbind(FRE20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
FRE20_TFft <- ftable(FRE20_TFg2$player1, FRE20_TFg2$player2)
FRE20_TFft2 <- as.matrix(FRE20_TFft)
numRows <- nrow(FRE20_TFft2)
numCols <- ncol(FRE20_TFft2)
FRE20_TFft3 <- FRE20_TFft2[c(2:numRows) , c(2:numCols)]
FRE20_TFTable <- graph.adjacency(FRE20_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(FRE20_TFTable, vertex.label = V(FRE20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
FRE20_TF.clusterCoef <- transitivity(FRE20_TFTable, type="global") #cluster coefficient
FRE20_TF.degreeCent <- centralization.degree(FRE20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_TFftn <- as.network.matrix(FRE20_TFft)
FRE20_TF.netDensity <- network.density(FRE20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_TF.entropy <- entropy(FRE20_TFft) #entropy

FRE20_TF.netMx <- cbind(FRE20_TF.netMx, FRE20_TF.clusterCoef, FRE20_TF.degreeCent$centralization,
                        FRE20_TF.netDensity, FRE20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************

round = 20
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
FRE20_SAMg2 <- data.frame(FRE20_SAM)
FRE20_SAMg2 <- FRE20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_SAMg2$player1
player2vector <- FRE20_SAMg2$player2
FRE20_SAMg3 <- FRE20_SAMg2
FRE20_SAMg3$p1inp2vec <- is.element(FRE20_SAMg3$player1, player2vector)
FRE20_SAMg3$p2inp1vec <- is.element(FRE20_SAMg3$player2, player1vector)

addPlayer1 <- FRE20_SAMg3[ which(FRE20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_SAMg3[ which(FRE20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_SAMg2 <- rbind(FRE20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
FRE20_SAMft <- ftable(FRE20_SAMg2$player1, FRE20_SAMg2$player2)
FRE20_SAMft2 <- as.matrix(FRE20_SAMft)
numRows <- nrow(FRE20_SAMft2)
numCols <- ncol(FRE20_SAMft2)
FRE20_SAMft3 <- FRE20_SAMft2[c(2:numRows) , c(2:numCols)]
FRE20_SAMTable <- graph.adjacency(FRE20_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(FRE20_SAMTable, vertex.label = V(FRE20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
FRE20_SAM.clusterCoef <- transitivity(FRE20_SAMTable, type="global") #cluster coefficient
FRE20_SAM.degreeCent <- centralization.degree(FRE20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_SAMftn <- as.network.matrix(FRE20_SAMft)
FRE20_SAM.netDensity <- network.density(FRE20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_SAM.entropy <- entropy(FRE20_SAMft) #entropy

FRE20_SAM.netMx <- cbind(FRE20_SAM.netMx, FRE20_SAM.clusterCoef, FRE20_SAM.degreeCent$centralization,
                         FRE20_SAM.netDensity, FRE20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************
#NA

round = 20
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
FRE20_TAMg2 <- data.frame(FRE20_TAM)
FRE20_TAMg2 <- FRE20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_TAMg2$player1
player2vector <- FRE20_TAMg2$player2
FRE20_TAMg3 <- FRE20_TAMg2
FRE20_TAMg3$p1inp2vec <- is.element(FRE20_TAMg3$player1, player2vector)
FRE20_TAMg3$p2inp1vec <- is.element(FRE20_TAMg3$player2, player1vector)

addPlayer1 <- FRE20_TAMg3[ which(FRE20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_TAMg3[ which(FRE20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_TAMg2 <- rbind(FRE20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
FRE20_TAMft <- ftable(FRE20_TAMg2$player1, FRE20_TAMg2$player2)
FRE20_TAMft2 <- as.matrix(FRE20_TAMft)
numRows <- nrow(FRE20_TAMft2)
numCols <- ncol(FRE20_TAMft2)
FRE20_TAMft3 <- FRE20_TAMft2[c(2:numRows) , c(2:numCols)]
FRE20_TAMTable <- graph.adjacency(FRE20_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(FRE20_TAMTable, vertex.label = V(FRE20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
FRE20_TAM.clusterCoef <- transitivity(FRE20_TAMTable, type="global") #cluster coefficient
FRE20_TAM.degreeCent <- centralization.degree(FRE20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_TAMftn <- as.network.matrix(FRE20_TAMft)
FRE20_TAM.netDensity <- network.density(FRE20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_TAM.entropy <- entropy(FRE20_TAMft) #entropy

FRE20_TAM.netMx <- cbind(FRE20_TAM.netMx, FRE20_TAM.clusterCoef, FRE20_TAM.degreeCent$centralization,
                         FRE20_TAM.netDensity, FRE20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
FRE20_SDMg2 <- data.frame(FRE20_SDM)
FRE20_SDMg2 <- FRE20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_SDMg2$player1
player2vector <- FRE20_SDMg2$player2
FRE20_SDMg3 <- FRE20_SDMg2
FRE20_SDMg3$p1inp2vec <- is.element(FRE20_SDMg3$player1, player2vector)
FRE20_SDMg3$p2inp1vec <- is.element(FRE20_SDMg3$player2, player1vector)

addPlayer1 <- FRE20_SDMg3[ which(FRE20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_SDMg3[ which(FRE20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_SDMg2 <- rbind(FRE20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
FRE20_SDMft <- ftable(FRE20_SDMg2$player1, FRE20_SDMg2$player2)
FRE20_SDMft2 <- as.matrix(FRE20_SDMft)
numRows <- nrow(FRE20_SDMft2)
numCols <- ncol(FRE20_SDMft2)
FRE20_SDMft3 <- FRE20_SDMft2[c(2:numRows) , c(2:numCols)]
FRE20_SDMTable <- graph.adjacency(FRE20_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(FRE20_SDMTable, vertex.label = V(FRE20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
FRE20_SDM.clusterCoef <- transitivity(FRE20_SDMTable, type="global") #cluster coefficient
FRE20_SDM.degreeCent <- centralization.degree(FRE20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_SDMftn <- as.network.matrix(FRE20_SDMft)
FRE20_SDM.netDensity <- network.density(FRE20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_SDM.entropy <- entropy(FRE20_SDMft) #entropy

FRE20_SDM.netMx <- cbind(FRE20_SDM.netMx, FRE20_SDM.clusterCoef, FRE20_SDM.degreeCent$centralization,
                         FRE20_SDM.netDensity, FRE20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
FRE20_TDMg2 <- data.frame(FRE20_TDM)
FRE20_TDMg2 <- FRE20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_TDMg2$player1
player2vector <- FRE20_TDMg2$player2
FRE20_TDMg3 <- FRE20_TDMg2
FRE20_TDMg3$p1inp2vec <- is.element(FRE20_TDMg3$player1, player2vector)
FRE20_TDMg3$p2inp1vec <- is.element(FRE20_TDMg3$player2, player1vector)

addPlayer1 <- FRE20_TDMg3[ which(FRE20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_TDMg3[ which(FRE20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_TDMg2 <- rbind(FRE20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
FRE20_TDMft <- ftable(FRE20_TDMg2$player1, FRE20_TDMg2$player2)
FRE20_TDMft2 <- as.matrix(FRE20_TDMft)
numRows <- nrow(FRE20_TDMft2)
numCols <- ncol(FRE20_TDMft2)
FRE20_TDMft3 <- FRE20_TDMft2[c(2:numRows) , c(2:numCols)]
FRE20_TDMTable <- graph.adjacency(FRE20_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(FRE20_TDMTable, vertex.label = V(FRE20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
FRE20_TDM.clusterCoef <- transitivity(FRE20_TDMTable, type="global") #cluster coefficient
FRE20_TDM.degreeCent <- centralization.degree(FRE20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_TDMftn <- as.network.matrix(FRE20_TDMft)
FRE20_TDM.netDensity <- network.density(FRE20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_TDM.entropy <- entropy(FRE20_TDMft) #entropy

FRE20_TDM.netMx <- cbind(FRE20_TDM.netMx, FRE20_TDM.clusterCoef, FRE20_TDM.degreeCent$centralization,
                         FRE20_TDM.netDensity, FRE20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
FRE20_SDg2 <- data.frame(FRE20_SD)
FRE20_SDg2 <- FRE20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_SDg2$player1
player2vector <- FRE20_SDg2$player2
FRE20_SDg3 <- FRE20_SDg2
FRE20_SDg3$p1inp2vec <- is.element(FRE20_SDg3$player1, player2vector)
FRE20_SDg3$p2inp1vec <- is.element(FRE20_SDg3$player2, player1vector)

addPlayer1 <- FRE20_SDg3[ which(FRE20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_SDg3[ which(FRE20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_SDg2 <- rbind(FRE20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
FRE20_SDft <- ftable(FRE20_SDg2$player1, FRE20_SDg2$player2)
FRE20_SDft2 <- as.matrix(FRE20_SDft)
numRows <- nrow(FRE20_SDft2)
numCols <- ncol(FRE20_SDft2)
FRE20_SDft3 <- FRE20_SDft2[c(2:numRows) , c(2:numCols)]
FRE20_SDTable <- graph.adjacency(FRE20_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(FRE20_SDTable, vertex.label = V(FRE20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
FRE20_SD.clusterCoef <- transitivity(FRE20_SDTable, type="global") #cluster coefficient
FRE20_SD.degreeCent <- centralization.degree(FRE20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_SDftn <- as.network.matrix(FRE20_SDft)
FRE20_SD.netDensity <- network.density(FRE20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_SD.entropy <- entropy(FRE20_SDft) #entropy

FRE20_SD.netMx <- cbind(FRE20_SD.netMx, FRE20_SD.clusterCoef, FRE20_SD.degreeCent$centralization,
                        FRE20_SD.netDensity, FRE20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
FRE20_TDg2 <- data.frame(FRE20_TD)
FRE20_TDg2 <- FRE20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_TDg2$player1
player2vector <- FRE20_TDg2$player2
FRE20_TDg3 <- FRE20_TDg2
FRE20_TDg3$p1inp2vec <- is.element(FRE20_TDg3$player1, player2vector)
FRE20_TDg3$p2inp1vec <- is.element(FRE20_TDg3$player2, player1vector)

addPlayer1 <- FRE20_TDg3[ which(FRE20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_TDg3[ which(FRE20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_TDg2 <- rbind(FRE20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
FRE20_TDft <- ftable(FRE20_TDg2$player1, FRE20_TDg2$player2)
FRE20_TDft2 <- as.matrix(FRE20_TDft)
numRows <- nrow(FRE20_TDft2)
numCols <- ncol(FRE20_TDft2)
FRE20_TDft3 <- FRE20_TDft2[c(2:numRows) , c(2:numCols)]
FRE20_TDTable <- graph.adjacency(FRE20_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(FRE20_TDTable, vertex.label = V(FRE20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
FRE20_TD.clusterCoef <- transitivity(FRE20_TDTable, type="global") #cluster coefficient
FRE20_TD.degreeCent <- centralization.degree(FRE20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_TDftn <- as.network.matrix(FRE20_TDft)
FRE20_TD.netDensity <- network.density(FRE20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_TD.entropy <- entropy(FRE20_TDft) #entropy

FRE20_TD.netMx <- cbind(FRE20_TD.netMx, FRE20_TD.clusterCoef, FRE20_TD.degreeCent$centralization,
                        FRE20_TD.netDensity, FRE20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
FRE20_QTg2 <- data.frame(FRE20_QT)
FRE20_QTg2 <- FRE20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE20_QTg2$player1
player2vector <- FRE20_QTg2$player2
FRE20_QTg3 <- FRE20_QTg2
FRE20_QTg3$p1inp2vec <- is.element(FRE20_QTg3$player1, player2vector)
FRE20_QTg3$p2inp1vec <- is.element(FRE20_QTg3$player2, player1vector)

addPlayer1 <- FRE20_QTg3[ which(FRE20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE20_QTg3[ which(FRE20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE20_QTg2 <- rbind(FRE20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
FRE20_QTft <- ftable(FRE20_QTg2$player1, FRE20_QTg2$player2)
FRE20_QTft2 <- as.matrix(FRE20_QTft)
numRows <- nrow(FRE20_QTft2)
numCols <- ncol(FRE20_QTft2)
FRE20_QTft3 <- FRE20_QTft2[c(2:numRows) , c(2:numCols)]
FRE20_QTTable <- graph.adjacency(FRE20_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(FRE20_QTTable, vertex.label = V(FRE20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
FRE20_QT.clusterCoef <- transitivity(FRE20_QTTable, type="global") #cluster coefficient
FRE20_QT.degreeCent <- centralization.degree(FRE20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE20_QTftn <- as.network.matrix(FRE20_QTft)
FRE20_QT.netDensity <- network.density(FRE20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE20_QT.entropy <- entropy(FRE20_QTft) #entropy

FRE20_QT.netMx <- cbind(FRE20_QT.netMx, FRE20_QT.clusterCoef, FRE20_QT.degreeCent$centralization,
                        FRE20_QT.netDensity, FRE20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE20_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************
#NA

round = 20
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
GCFC20_Gg2 <- data.frame(GCFC20_G)
GCFC20_Gg2 <- GCFC20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_Gg2$player1
player2vector <- GCFC20_Gg2$player2
GCFC20_Gg3 <- GCFC20_Gg2
GCFC20_Gg3$p1inp2vec <- is.element(GCFC20_Gg3$player1, player2vector)
GCFC20_Gg3$p2inp1vec <- is.element(GCFC20_Gg3$player2, player1vector)

addPlayer1 <- GCFC20_Gg3[ which(GCFC20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_Gg3[ which(GCFC20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_Gg2 <- rbind(GCFC20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
GCFC20_Gft <- ftable(GCFC20_Gg2$player1, GCFC20_Gg2$player2)
GCFC20_Gft2 <- as.matrix(GCFC20_Gft)
numRows <- nrow(GCFC20_Gft2)
numCols <- ncol(GCFC20_Gft2)
GCFC20_Gft3 <- GCFC20_Gft2[c(2:numRows) , c(2:numCols)]
GCFC20_GTable <- graph.adjacency(GCFC20_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(GCFC20_GTable, vertex.label = V(GCFC20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
GCFC20_G.clusterCoef <- transitivity(GCFC20_GTable, type="global") #cluster coefficient
GCFC20_G.degreeCent <- centralization.degree(GCFC20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_Gftn <- as.network.matrix(GCFC20_Gft)
GCFC20_G.netDensity <- network.density(GCFC20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_G.entropy <- entropy(GCFC20_Gft) #entropy

GCFC20_G.netMx <- cbind(GCFC20_G.netMx, GCFC20_G.clusterCoef, GCFC20_G.degreeCent$centralization,
                        GCFC20_G.netDensity, GCFC20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
GCFC20_Bg2 <- data.frame(GCFC20_B)
GCFC20_Bg2 <- GCFC20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_Bg2$player1
player2vector <- GCFC20_Bg2$player2
GCFC20_Bg3 <- GCFC20_Bg2
GCFC20_Bg3$p1inp2vec <- is.element(GCFC20_Bg3$player1, player2vector)
GCFC20_Bg3$p2inp1vec <- is.element(GCFC20_Bg3$player2, player1vector)

addPlayer1 <- GCFC20_Bg3[ which(GCFC20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_Bg3[ which(GCFC20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_Bg2 <- rbind(GCFC20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
GCFC20_Bft <- ftable(GCFC20_Bg2$player1, GCFC20_Bg2$player2)
GCFC20_Bft2 <- as.matrix(GCFC20_Bft)
numRows <- nrow(GCFC20_Bft2)
numCols <- ncol(GCFC20_Bft2)
GCFC20_Bft3 <- GCFC20_Bft2[c(2:numRows) , c(2:numCols)]
GCFC20_BTable <- graph.adjacency(GCFC20_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(GCFC20_BTable, vertex.label = V(GCFC20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
GCFC20_B.clusterCoef <- transitivity(GCFC20_BTable, type="global") #cluster coefficient
GCFC20_B.degreeCent <- centralization.degree(GCFC20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_Bftn <- as.network.matrix(GCFC20_Bft)
GCFC20_B.netDensity <- network.density(GCFC20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_B.entropy <- entropy(GCFC20_Bft) #entropy

GCFC20_B.netMx <- cbind(GCFC20_B.netMx, GCFC20_B.clusterCoef, GCFC20_B.degreeCent$centralization,
                        GCFC20_B.netDensity, GCFC20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
GCFC20_SFg2 <- data.frame(GCFC20_SF)
GCFC20_SFg2 <- GCFC20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_SFg2$player1
player2vector <- GCFC20_SFg2$player2
GCFC20_SFg3 <- GCFC20_SFg2
GCFC20_SFg3$p1inp2vec <- is.element(GCFC20_SFg3$player1, player2vector)
GCFC20_SFg3$p2inp1vec <- is.element(GCFC20_SFg3$player2, player1vector)

addPlayer1 <- GCFC20_SFg3[ which(GCFC20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GCFC20_SFg2 <- rbind(GCFC20_SFg2, addPlayer1)

#ROUND 20, FWD Stoppage graph using weighted edges
GCFC20_SFft <- ftable(GCFC20_SFg2$player1, GCFC20_SFg2$player2)
GCFC20_SFft2 <- as.matrix(GCFC20_SFft)
numRows <- nrow(GCFC20_SFft2)
numCols <- ncol(GCFC20_SFft2)
GCFC20_SFft3 <- GCFC20_SFft2[c(2:numRows) , c(1:numCols)]
GCFC20_SFTable <- graph.adjacency(GCFC20_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(GCFC20_SFTable, vertex.label = V(GCFC20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
GCFC20_SF.clusterCoef <- transitivity(GCFC20_SFTable, type="global") #cluster coefficient
GCFC20_SF.degreeCent <- centralization.degree(GCFC20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_SFftn <- as.network.matrix(GCFC20_SFft)
GCFC20_SF.netDensity <- network.density(GCFC20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_SF.entropy <- entropy(GCFC20_SFft) #entropy

GCFC20_SF.netMx <- cbind(GCFC20_SF.netMx, GCFC20_SF.clusterCoef, GCFC20_SF.degreeCent$centralization,
                         GCFC20_SF.netDensity, GCFC20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
GCFC20_TFg2 <- data.frame(GCFC20_TF)
GCFC20_TFg2 <- GCFC20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_TFg2$player1
player2vector <- GCFC20_TFg2$player2
GCFC20_TFg3 <- GCFC20_TFg2
GCFC20_TFg3$p1inp2vec <- is.element(GCFC20_TFg3$player1, player2vector)
GCFC20_TFg3$p2inp1vec <- is.element(GCFC20_TFg3$player2, player1vector)

addPlayer1 <- GCFC20_TFg3[ which(GCFC20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_TFg3[ which(GCFC20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_TFg2 <- rbind(GCFC20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
GCFC20_TFft <- ftable(GCFC20_TFg2$player1, GCFC20_TFg2$player2)
GCFC20_TFft2 <- as.matrix(GCFC20_TFft)
numRows <- nrow(GCFC20_TFft2)
numCols <- ncol(GCFC20_TFft2)
GCFC20_TFft3 <- GCFC20_TFft2[c(2:numRows) , c(2:numCols)]
GCFC20_TFTable <- graph.adjacency(GCFC20_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(GCFC20_TFTable, vertex.label = V(GCFC20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
GCFC20_TF.clusterCoef <- transitivity(GCFC20_TFTable, type="global") #cluster coefficient
GCFC20_TF.degreeCent <- centralization.degree(GCFC20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_TFftn <- as.network.matrix(GCFC20_TFft)
GCFC20_TF.netDensity <- network.density(GCFC20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_TF.entropy <- entropy(GCFC20_TFft) #entropy

GCFC20_TF.netMx <- cbind(GCFC20_TF.netMx, GCFC20_TF.clusterCoef, GCFC20_TF.degreeCent$centralization,
                         GCFC20_TF.netDensity, GCFC20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************

round = 20
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
GCFC20_SAMg2 <- data.frame(GCFC20_SAM)
GCFC20_SAMg2 <- GCFC20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_SAMg2$player1
player2vector <- GCFC20_SAMg2$player2
GCFC20_SAMg3 <- GCFC20_SAMg2
GCFC20_SAMg3$p1inp2vec <- is.element(GCFC20_SAMg3$player1, player2vector)
GCFC20_SAMg3$p2inp1vec <- is.element(GCFC20_SAMg3$player2, player1vector)

addPlayer1 <- GCFC20_SAMg3[ which(GCFC20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_SAMg3[ which(GCFC20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_SAMg2 <- rbind(GCFC20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
GCFC20_SAMft <- ftable(GCFC20_SAMg2$player1, GCFC20_SAMg2$player2)
GCFC20_SAMft2 <- as.matrix(GCFC20_SAMft)
numRows <- nrow(GCFC20_SAMft2)
numCols <- ncol(GCFC20_SAMft2)
GCFC20_SAMft3 <- GCFC20_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC20_SAMTable <- graph.adjacency(GCFC20_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(GCFC20_SAMTable, vertex.label = V(GCFC20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
GCFC20_SAM.clusterCoef <- transitivity(GCFC20_SAMTable, type="global") #cluster coefficient
GCFC20_SAM.degreeCent <- centralization.degree(GCFC20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_SAMftn <- as.network.matrix(GCFC20_SAMft)
GCFC20_SAM.netDensity <- network.density(GCFC20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_SAM.entropy <- entropy(GCFC20_SAMft) #entropy

GCFC20_SAM.netMx <- cbind(GCFC20_SAM.netMx, GCFC20_SAM.clusterCoef, GCFC20_SAM.degreeCent$centralization,
                          GCFC20_SAM.netDensity, GCFC20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
GCFC20_TAMg2 <- data.frame(GCFC20_TAM)
GCFC20_TAMg2 <- GCFC20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_TAMg2$player1
player2vector <- GCFC20_TAMg2$player2
GCFC20_TAMg3 <- GCFC20_TAMg2
GCFC20_TAMg3$p1inp2vec <- is.element(GCFC20_TAMg3$player1, player2vector)
GCFC20_TAMg3$p2inp1vec <- is.element(GCFC20_TAMg3$player2, player1vector)

addPlayer1 <- GCFC20_TAMg3[ which(GCFC20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_TAMg3[ which(GCFC20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_TAMg2 <- rbind(GCFC20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
GCFC20_TAMft <- ftable(GCFC20_TAMg2$player1, GCFC20_TAMg2$player2)
GCFC20_TAMft2 <- as.matrix(GCFC20_TAMft)
numRows <- nrow(GCFC20_TAMft2)
numCols <- ncol(GCFC20_TAMft2)
GCFC20_TAMft3 <- GCFC20_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC20_TAMTable <- graph.adjacency(GCFC20_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(GCFC20_TAMTable, vertex.label = V(GCFC20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
GCFC20_TAM.clusterCoef <- transitivity(GCFC20_TAMTable, type="global") #cluster coefficient
GCFC20_TAM.degreeCent <- centralization.degree(GCFC20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_TAMftn <- as.network.matrix(GCFC20_TAMft)
GCFC20_TAM.netDensity <- network.density(GCFC20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_TAM.entropy <- entropy(GCFC20_TAMft) #entropy

GCFC20_TAM.netMx <- cbind(GCFC20_TAM.netMx, GCFC20_TAM.clusterCoef, GCFC20_TAM.degreeCent$centralization,
                          GCFC20_TAM.netDensity, GCFC20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************
#NA

round = 20
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
GCFC20_SDMg2 <- data.frame(GCFC20_SDM)
GCFC20_SDMg2 <- GCFC20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_SDMg2$player1
player2vector <- GCFC20_SDMg2$player2
GCFC20_SDMg3 <- GCFC20_SDMg2
GCFC20_SDMg3$p1inp2vec <- is.element(GCFC20_SDMg3$player1, player2vector)
GCFC20_SDMg3$p2inp1vec <- is.element(GCFC20_SDMg3$player2, player1vector)

addPlayer1 <- GCFC20_SDMg3[ which(GCFC20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_SDMg3[ which(GCFC20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_SDMg2 <- rbind(GCFC20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
GCFC20_SDMft <- ftable(GCFC20_SDMg2$player1, GCFC20_SDMg2$player2)
GCFC20_SDMft2 <- as.matrix(GCFC20_SDMft)
numRows <- nrow(GCFC20_SDMft2)
numCols <- ncol(GCFC20_SDMft2)
GCFC20_SDMft3 <- GCFC20_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC20_SDMTable <- graph.adjacency(GCFC20_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(GCFC20_SDMTable, vertex.label = V(GCFC20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
GCFC20_SDM.clusterCoef <- transitivity(GCFC20_SDMTable, type="global") #cluster coefficient
GCFC20_SDM.degreeCent <- centralization.degree(GCFC20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_SDMftn <- as.network.matrix(GCFC20_SDMft)
GCFC20_SDM.netDensity <- network.density(GCFC20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_SDM.entropy <- entropy(GCFC20_SDMft) #entropy

GCFC20_SDM.netMx <- cbind(GCFC20_SDM.netMx, GCFC20_SDM.clusterCoef, GCFC20_SDM.degreeCent$centralization,
                          GCFC20_SDM.netDensity, GCFC20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
GCFC20_TDMg2 <- data.frame(GCFC20_TDM)
GCFC20_TDMg2 <- GCFC20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_TDMg2$player1
player2vector <- GCFC20_TDMg2$player2
GCFC20_TDMg3 <- GCFC20_TDMg2
GCFC20_TDMg3$p1inp2vec <- is.element(GCFC20_TDMg3$player1, player2vector)
GCFC20_TDMg3$p2inp1vec <- is.element(GCFC20_TDMg3$player2, player1vector)

addPlayer1 <- GCFC20_TDMg3[ which(GCFC20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_TDMg3[ which(GCFC20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_TDMg2 <- rbind(GCFC20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
GCFC20_TDMft <- ftable(GCFC20_TDMg2$player1, GCFC20_TDMg2$player2)
GCFC20_TDMft2 <- as.matrix(GCFC20_TDMft)
numRows <- nrow(GCFC20_TDMft2)
numCols <- ncol(GCFC20_TDMft2)
GCFC20_TDMft3 <- GCFC20_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC20_TDMTable <- graph.adjacency(GCFC20_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(GCFC20_TDMTable, vertex.label = V(GCFC20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
GCFC20_TDM.clusterCoef <- transitivity(GCFC20_TDMTable, type="global") #cluster coefficient
GCFC20_TDM.degreeCent <- centralization.degree(GCFC20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_TDMftn <- as.network.matrix(GCFC20_TDMft)
GCFC20_TDM.netDensity <- network.density(GCFC20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_TDM.entropy <- entropy(GCFC20_TDMft) #entropy

GCFC20_TDM.netMx <- cbind(GCFC20_TDM.netMx, GCFC20_TDM.clusterCoef, GCFC20_TDM.degreeCent$centralization,
                          GCFC20_TDM.netDensity, GCFC20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
GCFC20_SDg2 <- data.frame(GCFC20_SD)
GCFC20_SDg2 <- GCFC20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_SDg2$player1
player2vector <- GCFC20_SDg2$player2
GCFC20_SDg3 <- GCFC20_SDg2
GCFC20_SDg3$p1inp2vec <- is.element(GCFC20_SDg3$player1, player2vector)
GCFC20_SDg3$p2inp1vec <- is.element(GCFC20_SDg3$player2, player1vector)

addPlayer1 <- GCFC20_SDg3[ which(GCFC20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_SDg3[ which(GCFC20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_SDg2 <- rbind(GCFC20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
GCFC20_SDft <- ftable(GCFC20_SDg2$player1, GCFC20_SDg2$player2)
GCFC20_SDft2 <- as.matrix(GCFC20_SDft)
numRows <- nrow(GCFC20_SDft2)
numCols <- ncol(GCFC20_SDft2)
GCFC20_SDft3 <- GCFC20_SDft2[c(2:numRows) , c(2:numCols)]
GCFC20_SDTable <- graph.adjacency(GCFC20_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(GCFC20_SDTable, vertex.label = V(GCFC20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
GCFC20_SD.clusterCoef <- transitivity(GCFC20_SDTable, type="global") #cluster coefficient
GCFC20_SD.degreeCent <- centralization.degree(GCFC20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_SDftn <- as.network.matrix(GCFC20_SDft)
GCFC20_SD.netDensity <- network.density(GCFC20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_SD.entropy <- entropy(GCFC20_SDft) #entropy

GCFC20_SD.netMx <- cbind(GCFC20_SD.netMx, GCFC20_SD.clusterCoef, GCFC20_SD.degreeCent$centralization,
                         GCFC20_SD.netDensity, GCFC20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
GCFC20_TDg2 <- data.frame(GCFC20_TD)
GCFC20_TDg2 <- GCFC20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_TDg2$player1
player2vector <- GCFC20_TDg2$player2
GCFC20_TDg3 <- GCFC20_TDg2
GCFC20_TDg3$p1inp2vec <- is.element(GCFC20_TDg3$player1, player2vector)
GCFC20_TDg3$p2inp1vec <- is.element(GCFC20_TDg3$player2, player1vector)

addPlayer1 <- GCFC20_TDg3[ which(GCFC20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_TDg3[ which(GCFC20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_TDg2 <- rbind(GCFC20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
GCFC20_TDft <- ftable(GCFC20_TDg2$player1, GCFC20_TDg2$player2)
GCFC20_TDft2 <- as.matrix(GCFC20_TDft)
numRows <- nrow(GCFC20_TDft2)
numCols <- ncol(GCFC20_TDft2)
GCFC20_TDft3 <- GCFC20_TDft2[c(2:numRows) , c(2:numCols)]
GCFC20_TDTable <- graph.adjacency(GCFC20_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(GCFC20_TDTable, vertex.label = V(GCFC20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
GCFC20_TD.clusterCoef <- transitivity(GCFC20_TDTable, type="global") #cluster coefficient
GCFC20_TD.degreeCent <- centralization.degree(GCFC20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_TDftn <- as.network.matrix(GCFC20_TDft)
GCFC20_TD.netDensity <- network.density(GCFC20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_TD.entropy <- entropy(GCFC20_TDft) #entropy

GCFC20_TD.netMx <- cbind(GCFC20_TD.netMx, GCFC20_TD.clusterCoef, GCFC20_TD.degreeCent$centralization,
                         GCFC20_TD.netDensity, GCFC20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
GCFC20_QTg2 <- data.frame(GCFC20_QT)
GCFC20_QTg2 <- GCFC20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC20_QTg2$player1
player2vector <- GCFC20_QTg2$player2
GCFC20_QTg3 <- GCFC20_QTg2
GCFC20_QTg3$p1inp2vec <- is.element(GCFC20_QTg3$player1, player2vector)
GCFC20_QTg3$p2inp1vec <- is.element(GCFC20_QTg3$player2, player1vector)

addPlayer1 <- GCFC20_QTg3[ which(GCFC20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC20_QTg3[ which(GCFC20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC20_QTg2 <- rbind(GCFC20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
GCFC20_QTft <- ftable(GCFC20_QTg2$player1, GCFC20_QTg2$player2)
GCFC20_QTft2 <- as.matrix(GCFC20_QTft)
numRows <- nrow(GCFC20_QTft2)
numCols <- ncol(GCFC20_QTft2)
GCFC20_QTft3 <- GCFC20_QTft2[c(2:numRows) , c(2:numCols)]
GCFC20_QTTable <- graph.adjacency(GCFC20_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(GCFC20_QTTable, vertex.label = V(GCFC20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
GCFC20_QT.clusterCoef <- transitivity(GCFC20_QTTable, type="global") #cluster coefficient
GCFC20_QT.degreeCent <- centralization.degree(GCFC20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC20_QTftn <- as.network.matrix(GCFC20_QTft)
GCFC20_QT.netDensity <- network.density(GCFC20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC20_QT.entropy <- entropy(GCFC20_QTft) #entropy

GCFC20_QT.netMx <- cbind(GCFC20_QT.netMx, GCFC20_QT.clusterCoef, GCFC20_QT.degreeCent$centralization,
                         GCFC20_QT.netDensity, GCFC20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC20_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
GEEL20_Gg2 <- data.frame(GEEL20_G)
GEEL20_Gg2 <- GEEL20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_Gg2$player1
player2vector <- GEEL20_Gg2$player2
GEEL20_Gg3 <- GEEL20_Gg2
GEEL20_Gg3$p1inp2vec <- is.element(GEEL20_Gg3$player1, player2vector)
GEEL20_Gg3$p2inp1vec <- is.element(GEEL20_Gg3$player2, player1vector)

addPlayer1 <- GEEL20_Gg3[ which(GEEL20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_Gg3[ which(GEEL20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_Gg2 <- rbind(GEEL20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
GEEL20_Gft <- ftable(GEEL20_Gg2$player1, GEEL20_Gg2$player2)
GEEL20_Gft2 <- as.matrix(GEEL20_Gft)
numRows <- nrow(GEEL20_Gft2)
numCols <- ncol(GEEL20_Gft2)
GEEL20_Gft3 <- GEEL20_Gft2[c(2:numRows) , c(2:numCols)]
GEEL20_GTable <- graph.adjacency(GEEL20_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(GEEL20_GTable, vertex.label = V(GEEL20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
GEEL20_G.clusterCoef <- transitivity(GEEL20_GTable, type="global") #cluster coefficient
GEEL20_G.degreeCent <- centralization.degree(GEEL20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_Gftn <- as.network.matrix(GEEL20_Gft)
GEEL20_G.netDensity <- network.density(GEEL20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_G.entropy <- entropy(GEEL20_Gft) #entropy

GEEL20_G.netMx <- cbind(GEEL20_G.netMx, GEEL20_G.clusterCoef, GEEL20_G.degreeCent$centralization,
                        GEEL20_G.netDensity, GEEL20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
GEEL20_Bg2 <- data.frame(GEEL20_B)
GEEL20_Bg2 <- GEEL20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_Bg2$player1
player2vector <- GEEL20_Bg2$player2
GEEL20_Bg3 <- GEEL20_Bg2
GEEL20_Bg3$p1inp2vec <- is.element(GEEL20_Bg3$player1, player2vector)
GEEL20_Bg3$p2inp1vec <- is.element(GEEL20_Bg3$player2, player1vector)

addPlayer1 <- GEEL20_Bg3[ which(GEEL20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_Bg3[ which(GEEL20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_Bg2 <- rbind(GEEL20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
GEEL20_Bft <- ftable(GEEL20_Bg2$player1, GEEL20_Bg2$player2)
GEEL20_Bft2 <- as.matrix(GEEL20_Bft)
numRows <- nrow(GEEL20_Bft2)
numCols <- ncol(GEEL20_Bft2)
GEEL20_Bft3 <- GEEL20_Bft2[c(2:numRows) , c(2:numCols)]
GEEL20_BTable <- graph.adjacency(GEEL20_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(GEEL20_BTable, vertex.label = V(GEEL20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
GEEL20_B.clusterCoef <- transitivity(GEEL20_BTable, type="global") #cluster coefficient
GEEL20_B.degreeCent <- centralization.degree(GEEL20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_Bftn <- as.network.matrix(GEEL20_Bft)
GEEL20_B.netDensity <- network.density(GEEL20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_B.entropy <- entropy(GEEL20_Bft) #entropy

GEEL20_B.netMx <- cbind(GEEL20_B.netMx, GEEL20_B.clusterCoef, GEEL20_B.degreeCent$centralization,
                        GEEL20_B.netDensity, GEEL20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
GEEL20_SFg2 <- data.frame(GEEL20_SF)
GEEL20_SFg2 <- GEEL20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_SFg2$player1
player2vector <- GEEL20_SFg2$player2
GEEL20_SFg3 <- GEEL20_SFg2
GEEL20_SFg3$p1inp2vec <- is.element(GEEL20_SFg3$player1, player2vector)
GEEL20_SFg3$p2inp1vec <- is.element(GEEL20_SFg3$player2, player1vector)

addPlayer1 <- GEEL20_SFg3[ which(GEEL20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_SFg3[ which(GEEL20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_SFg2 <- rbind(GEEL20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
GEEL20_SFft <- ftable(GEEL20_SFg2$player1, GEEL20_SFg2$player2)
GEEL20_SFft2 <- as.matrix(GEEL20_SFft)
numRows <- nrow(GEEL20_SFft2)
numCols <- ncol(GEEL20_SFft2)
GEEL20_SFft3 <- GEEL20_SFft2[c(2:numRows) , c(2:numCols)]
GEEL20_SFTable <- graph.adjacency(GEEL20_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(GEEL20_SFTable, vertex.label = V(GEEL20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
GEEL20_SF.clusterCoef <- transitivity(GEEL20_SFTable, type="global") #cluster coefficient
GEEL20_SF.degreeCent <- centralization.degree(GEEL20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_SFftn <- as.network.matrix(GEEL20_SFft)
GEEL20_SF.netDensity <- network.density(GEEL20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_SF.entropy <- entropy(GEEL20_SFft) #entropy

GEEL20_SF.netMx <- cbind(GEEL20_SF.netMx, GEEL20_SF.clusterCoef, GEEL20_SF.degreeCent$centralization,
                         GEEL20_SF.netDensity, GEEL20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************
#NA

round = 20
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
GEEL20_TFg2 <- data.frame(GEEL20_TF)
GEEL20_TFg2 <- GEEL20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_TFg2$player1
player2vector <- GEEL20_TFg2$player2
GEEL20_TFg3 <- GEEL20_TFg2
GEEL20_TFg3$p1inp2vec <- is.element(GEEL20_TFg3$player1, player2vector)
GEEL20_TFg3$p2inp1vec <- is.element(GEEL20_TFg3$player2, player1vector)

addPlayer1 <- GEEL20_TFg3[ which(GEEL20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_TFg3[ which(GEEL20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_TFg2 <- rbind(GEEL20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
GEEL20_TFft <- ftable(GEEL20_TFg2$player1, GEEL20_TFg2$player2)
GEEL20_TFft2 <- as.matrix(GEEL20_TFft)
numRows <- nrow(GEEL20_TFft2)
numCols <- ncol(GEEL20_TFft2)
GEEL20_TFft3 <- GEEL20_TFft2[c(2:numRows) , c(2:numCols)]
GEEL20_TFTable <- graph.adjacency(GEEL20_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(GEEL20_TFTable, vertex.label = V(GEEL20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
GEEL20_TF.clusterCoef <- transitivity(GEEL20_TFTable, type="global") #cluster coefficient
GEEL20_TF.degreeCent <- centralization.degree(GEEL20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_TFftn <- as.network.matrix(GEEL20_TFft)
GEEL20_TF.netDensity <- network.density(GEEL20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_TF.entropy <- entropy(GEEL20_TFft) #entropy

GEEL20_TF.netMx <- cbind(GEEL20_TF.netMx, GEEL20_TF.clusterCoef, GEEL20_TF.degreeCent$centralization,
                         GEEL20_TF.netDensity, GEEL20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************
#NA

round = 20
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
GEEL20_SAMg2 <- data.frame(GEEL20_SAM)
GEEL20_SAMg2 <- GEEL20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_SAMg2$player1
player2vector <- GEEL20_SAMg2$player2
GEEL20_SAMg3 <- GEEL20_SAMg2
GEEL20_SAMg3$p1inp2vec <- is.element(GEEL20_SAMg3$player1, player2vector)
GEEL20_SAMg3$p2inp1vec <- is.element(GEEL20_SAMg3$player2, player1vector)

addPlayer1 <- GEEL20_SAMg3[ which(GEEL20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_SAMg3[ which(GEEL20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_SAMg2 <- rbind(GEEL20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
GEEL20_SAMft <- ftable(GEEL20_SAMg2$player1, GEEL20_SAMg2$player2)
GEEL20_SAMft2 <- as.matrix(GEEL20_SAMft)
numRows <- nrow(GEEL20_SAMft2)
numCols <- ncol(GEEL20_SAMft2)
GEEL20_SAMft3 <- GEEL20_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL20_SAMTable <- graph.adjacency(GEEL20_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(GEEL20_SAMTable, vertex.label = V(GEEL20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
GEEL20_SAM.clusterCoef <- transitivity(GEEL20_SAMTable, type="global") #cluster coefficient
GEEL20_SAM.degreeCent <- centralization.degree(GEEL20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_SAMftn <- as.network.matrix(GEEL20_SAMft)
GEEL20_SAM.netDensity <- network.density(GEEL20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_SAM.entropy <- entropy(GEEL20_SAMft) #entropy

GEEL20_SAM.netMx <- cbind(GEEL20_SAM.netMx, GEEL20_SAM.clusterCoef, GEEL20_SAM.degreeCent$centralization,
                          GEEL20_SAM.netDensity, GEEL20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
GEEL20_TAMg2 <- data.frame(GEEL20_TAM)
GEEL20_TAMg2 <- GEEL20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_TAMg2$player1
player2vector <- GEEL20_TAMg2$player2
GEEL20_TAMg3 <- GEEL20_TAMg2
GEEL20_TAMg3$p1inp2vec <- is.element(GEEL20_TAMg3$player1, player2vector)
GEEL20_TAMg3$p2inp1vec <- is.element(GEEL20_TAMg3$player2, player1vector)

addPlayer1 <- GEEL20_TAMg3[ which(GEEL20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_TAMg3[ which(GEEL20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_TAMg2 <- rbind(GEEL20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
GEEL20_TAMft <- ftable(GEEL20_TAMg2$player1, GEEL20_TAMg2$player2)
GEEL20_TAMft2 <- as.matrix(GEEL20_TAMft)
numRows <- nrow(GEEL20_TAMft2)
numCols <- ncol(GEEL20_TAMft2)
GEEL20_TAMft3 <- GEEL20_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL20_TAMTable <- graph.adjacency(GEEL20_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(GEEL20_TAMTable, vertex.label = V(GEEL20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
GEEL20_TAM.clusterCoef <- transitivity(GEEL20_TAMTable, type="global") #cluster coefficient
GEEL20_TAM.degreeCent <- centralization.degree(GEEL20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_TAMftn <- as.network.matrix(GEEL20_TAMft)
GEEL20_TAM.netDensity <- network.density(GEEL20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_TAM.entropy <- entropy(GEEL20_TAMft) #entropy

GEEL20_TAM.netMx <- cbind(GEEL20_TAM.netMx, GEEL20_TAM.clusterCoef, GEEL20_TAM.degreeCent$centralization,
                          GEEL20_TAM.netDensity, GEEL20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************
#NA

round = 20
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
GEEL20_SDMg2 <- data.frame(GEEL20_SDM)
GEEL20_SDMg2 <- GEEL20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_SDMg2$player1
player2vector <- GEEL20_SDMg2$player2
GEEL20_SDMg3 <- GEEL20_SDMg2
GEEL20_SDMg3$p1inp2vec <- is.element(GEEL20_SDMg3$player1, player2vector)
GEEL20_SDMg3$p2inp1vec <- is.element(GEEL20_SDMg3$player2, player1vector)

addPlayer1 <- GEEL20_SDMg3[ which(GEEL20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_SDMg3[ which(GEEL20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_SDMg2 <- rbind(GEEL20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
GEEL20_SDMft <- ftable(GEEL20_SDMg2$player1, GEEL20_SDMg2$player2)
GEEL20_SDMft2 <- as.matrix(GEEL20_SDMft)
numRows <- nrow(GEEL20_SDMft2)
numCols <- ncol(GEEL20_SDMft2)
GEEL20_SDMft3 <- GEEL20_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL20_SDMTable <- graph.adjacency(GEEL20_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(GEEL20_SDMTable, vertex.label = V(GEEL20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
GEEL20_SDM.clusterCoef <- transitivity(GEEL20_SDMTable, type="global") #cluster coefficient
GEEL20_SDM.degreeCent <- centralization.degree(GEEL20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_SDMftn <- as.network.matrix(GEEL20_SDMft)
GEEL20_SDM.netDensity <- network.density(GEEL20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_SDM.entropy <- entropy(GEEL20_SDMft) #entropy

GEEL20_SDM.netMx <- cbind(GEEL20_SDM.netMx, GEEL20_SDM.clusterCoef, GEEL20_SDM.degreeCent$centralization,
                          GEEL20_SDM.netDensity, GEEL20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
GEEL20_TDMg2 <- data.frame(GEEL20_TDM)
GEEL20_TDMg2 <- GEEL20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_TDMg2$player1
player2vector <- GEEL20_TDMg2$player2
GEEL20_TDMg3 <- GEEL20_TDMg2
GEEL20_TDMg3$p1inp2vec <- is.element(GEEL20_TDMg3$player1, player2vector)
GEEL20_TDMg3$p2inp1vec <- is.element(GEEL20_TDMg3$player2, player1vector)

addPlayer1 <- GEEL20_TDMg3[ which(GEEL20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_TDMg3[ which(GEEL20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_TDMg2 <- rbind(GEEL20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
GEEL20_TDMft <- ftable(GEEL20_TDMg2$player1, GEEL20_TDMg2$player2)
GEEL20_TDMft2 <- as.matrix(GEEL20_TDMft)
numRows <- nrow(GEEL20_TDMft2)
numCols <- ncol(GEEL20_TDMft2)
GEEL20_TDMft3 <- GEEL20_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL20_TDMTable <- graph.adjacency(GEEL20_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(GEEL20_TDMTable, vertex.label = V(GEEL20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
GEEL20_TDM.clusterCoef <- transitivity(GEEL20_TDMTable, type="global") #cluster coefficient
GEEL20_TDM.degreeCent <- centralization.degree(GEEL20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_TDMftn <- as.network.matrix(GEEL20_TDMft)
GEEL20_TDM.netDensity <- network.density(GEEL20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_TDM.entropy <- entropy(GEEL20_TDMft) #entropy

GEEL20_TDM.netMx <- cbind(GEEL20_TDM.netMx, GEEL20_TDM.clusterCoef, GEEL20_TDM.degreeCent$centralization,
                          GEEL20_TDM.netDensity, GEEL20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
GEEL20_SDg2 <- data.frame(GEEL20_SD)
GEEL20_SDg2 <- GEEL20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_SDg2$player1
player2vector <- GEEL20_SDg2$player2
GEEL20_SDg3 <- GEEL20_SDg2
GEEL20_SDg3$p1inp2vec <- is.element(GEEL20_SDg3$player1, player2vector)
GEEL20_SDg3$p2inp1vec <- is.element(GEEL20_SDg3$player2, player1vector)

addPlayer1 <- GEEL20_SDg3[ which(GEEL20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_SDg3[ which(GEEL20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_SDg2 <- rbind(GEEL20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
GEEL20_SDft <- ftable(GEEL20_SDg2$player1, GEEL20_SDg2$player2)
GEEL20_SDft2 <- as.matrix(GEEL20_SDft)
numRows <- nrow(GEEL20_SDft2)
numCols <- ncol(GEEL20_SDft2)
GEEL20_SDft3 <- GEEL20_SDft2[c(2:numRows) , c(2:numCols)]
GEEL20_SDTable <- graph.adjacency(GEEL20_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(GEEL20_SDTable, vertex.label = V(GEEL20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
GEEL20_SD.clusterCoef <- transitivity(GEEL20_SDTable, type="global") #cluster coefficient
GEEL20_SD.degreeCent <- centralization.degree(GEEL20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_SDftn <- as.network.matrix(GEEL20_SDft)
GEEL20_SD.netDensity <- network.density(GEEL20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_SD.entropy <- entropy(GEEL20_SDft) #entropy

GEEL20_SD.netMx <- cbind(GEEL20_SD.netMx, GEEL20_SD.clusterCoef, GEEL20_SD.degreeCent$centralization,
                         GEEL20_SD.netDensity, GEEL20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
GEEL20_TDg2 <- data.frame(GEEL20_TD)
GEEL20_TDg2 <- GEEL20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_TDg2$player1
player2vector <- GEEL20_TDg2$player2
GEEL20_TDg3 <- GEEL20_TDg2
GEEL20_TDg3$p1inp2vec <- is.element(GEEL20_TDg3$player1, player2vector)
GEEL20_TDg3$p2inp1vec <- is.element(GEEL20_TDg3$player2, player1vector)

addPlayer1 <- GEEL20_TDg3[ which(GEEL20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_TDg3[ which(GEEL20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_TDg2 <- rbind(GEEL20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
GEEL20_TDft <- ftable(GEEL20_TDg2$player1, GEEL20_TDg2$player2)
GEEL20_TDft2 <- as.matrix(GEEL20_TDft)
numRows <- nrow(GEEL20_TDft2)
numCols <- ncol(GEEL20_TDft2)
GEEL20_TDft3 <- GEEL20_TDft2[c(2:numRows) , c(2:numCols)]
GEEL20_TDTable <- graph.adjacency(GEEL20_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(GEEL20_TDTable, vertex.label = V(GEEL20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
GEEL20_TD.clusterCoef <- transitivity(GEEL20_TDTable, type="global") #cluster coefficient
GEEL20_TD.degreeCent <- centralization.degree(GEEL20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_TDftn <- as.network.matrix(GEEL20_TDft)
GEEL20_TD.netDensity <- network.density(GEEL20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_TD.entropy <- entropy(GEEL20_TDft) #entropy

GEEL20_TD.netMx <- cbind(GEEL20_TD.netMx, GEEL20_TD.clusterCoef, GEEL20_TD.degreeCent$centralization,
                         GEEL20_TD.netDensity, GEEL20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
GEEL20_QTg2 <- data.frame(GEEL20_QT)
GEEL20_QTg2 <- GEEL20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL20_QTg2$player1
player2vector <- GEEL20_QTg2$player2
GEEL20_QTg3 <- GEEL20_QTg2
GEEL20_QTg3$p1inp2vec <- is.element(GEEL20_QTg3$player1, player2vector)
GEEL20_QTg3$p2inp1vec <- is.element(GEEL20_QTg3$player2, player1vector)

addPlayer1 <- GEEL20_QTg3[ which(GEEL20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL20_QTg3[ which(GEEL20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL20_QTg2 <- rbind(GEEL20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
GEEL20_QTft <- ftable(GEEL20_QTg2$player1, GEEL20_QTg2$player2)
GEEL20_QTft2 <- as.matrix(GEEL20_QTft)
numRows <- nrow(GEEL20_QTft2)
numCols <- ncol(GEEL20_QTft2)
GEEL20_QTft3 <- GEEL20_QTft2[c(2:numRows) , c(2:numCols)]
GEEL20_QTTable <- graph.adjacency(GEEL20_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(GEEL20_QTTable, vertex.label = V(GEEL20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
GEEL20_QT.clusterCoef <- transitivity(GEEL20_QTTable, type="global") #cluster coefficient
GEEL20_QT.degreeCent <- centralization.degree(GEEL20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL20_QTftn <- as.network.matrix(GEEL20_QTft)
GEEL20_QT.netDensity <- network.density(GEEL20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL20_QT.entropy <- entropy(GEEL20_QTft) #entropy

GEEL20_QT.netMx <- cbind(GEEL20_QT.netMx, GEEL20_QT.clusterCoef, GEEL20_QT.degreeCent$centralization,
                         GEEL20_QT.netDensity, GEEL20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL20_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************
#NA

round = 20
teamName = "GWS"
KIoutcome = "Goal_F"
GWS20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
GWS20_Gg2 <- data.frame(GWS20_G)
GWS20_Gg2 <- GWS20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_Gg2$player1
player2vector <- GWS20_Gg2$player2
GWS20_Gg3 <- GWS20_Gg2
GWS20_Gg3$p1inp2vec <- is.element(GWS20_Gg3$player1, player2vector)
GWS20_Gg3$p2inp1vec <- is.element(GWS20_Gg3$player2, player1vector)

addPlayer1 <- GWS20_Gg3[ which(GWS20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_Gg3[ which(GWS20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_Gg2 <- rbind(GWS20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
GWS20_Gft <- ftable(GWS20_Gg2$player1, GWS20_Gg2$player2)
GWS20_Gft2 <- as.matrix(GWS20_Gft)
numRows <- nrow(GWS20_Gft2)
numCols <- ncol(GWS20_Gft2)
GWS20_Gft3 <- GWS20_Gft2[c(1:numRows) , c(1:numCols)]
GWS20_GTable <- graph.adjacency(GWS20_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(GWS20_GTable, vertex.label = V(GWS20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
GWS20_G.clusterCoef <- transitivity(GWS20_GTable, type="global") #cluster coefficient
GWS20_G.degreeCent <- centralization.degree(GWS20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_Gftn <- as.network.matrix(GWS20_Gft)
GWS20_G.netDensity <- network.density(GWS20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_G.entropy <- entropy(GWS20_Gft) #entropy

GWS20_G.netMx <- cbind(GWS20_G.netMx, GWS20_G.clusterCoef, GWS20_G.degreeCent$centralization,
                       GWS20_G.netDensity, GWS20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************

round = 20
teamName = "GWS"
KIoutcome = "Behind_F"
GWS20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
GWS20_Bg2 <- data.frame(GWS20_B)
GWS20_Bg2 <- GWS20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_Bg2$player1
player2vector <- GWS20_Bg2$player2
GWS20_Bg3 <- GWS20_Bg2
GWS20_Bg3$p1inp2vec <- is.element(GWS20_Bg3$player1, player2vector)
GWS20_Bg3$p2inp1vec <- is.element(GWS20_Bg3$player2, player1vector)

addPlayer1 <- GWS20_Bg3[ which(GWS20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_Bg3[ which(GWS20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_Bg2 <- rbind(GWS20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
GWS20_Bft <- ftable(GWS20_Bg2$player1, GWS20_Bg2$player2)
GWS20_Bft2 <- as.matrix(GWS20_Bft)
numRows <- nrow(GWS20_Bft2)
numCols <- ncol(GWS20_Bft2)
GWS20_Bft3 <- GWS20_Bft2[c(2:numRows) , c(2:numCols)]
GWS20_BTable <- graph.adjacency(GWS20_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(GWS20_BTable, vertex.label = V(GWS20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
GWS20_B.clusterCoef <- transitivity(GWS20_BTable, type="global") #cluster coefficient
GWS20_B.degreeCent <- centralization.degree(GWS20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_Bftn <- as.network.matrix(GWS20_Bft)
GWS20_B.netDensity <- network.density(GWS20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_B.entropy <- entropy(GWS20_Bft) #entropy

GWS20_B.netMx <- cbind(GWS20_B.netMx, GWS20_B.clusterCoef, GWS20_B.degreeCent$centralization,
                       GWS20_B.netDensity, GWS20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************

round = 20
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
GWS20_SFg2 <- data.frame(GWS20_SF)
GWS20_SFg2 <- GWS20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_SFg2$player1
player2vector <- GWS20_SFg2$player2
GWS20_SFg3 <- GWS20_SFg2
GWS20_SFg3$p1inp2vec <- is.element(GWS20_SFg3$player1, player2vector)
GWS20_SFg3$p2inp1vec <- is.element(GWS20_SFg3$player2, player1vector)

addPlayer1 <- GWS20_SFg3[ which(GWS20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_SFg3[ which(GWS20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_SFg2 <- rbind(GWS20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
GWS20_SFft <- ftable(GWS20_SFg2$player1, GWS20_SFg2$player2)
GWS20_SFft2 <- as.matrix(GWS20_SFft)
numRows <- nrow(GWS20_SFft2)
numCols <- ncol(GWS20_SFft2)
GWS20_SFft3 <- GWS20_SFft2[c(2:numRows) , c(2:numCols)]
GWS20_SFTable <- graph.adjacency(GWS20_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(GWS20_SFTable, vertex.label = V(GWS20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
GWS20_SF.clusterCoef <- transitivity(GWS20_SFTable, type="global") #cluster coefficient
GWS20_SF.degreeCent <- centralization.degree(GWS20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_SFftn <- as.network.matrix(GWS20_SFft)
GWS20_SF.netDensity <- network.density(GWS20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_SF.entropy <- entropy(GWS20_SFft) #entropy

GWS20_SF.netMx <- cbind(GWS20_SF.netMx, GWS20_SF.clusterCoef, GWS20_SF.degreeCent$centralization,
                        GWS20_SF.netDensity, GWS20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************
#NA

round = 20
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
GWS20_TFg2 <- data.frame(GWS20_TF)
GWS20_TFg2 <- GWS20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_TFg2$player1
player2vector <- GWS20_TFg2$player2
GWS20_TFg3 <- GWS20_TFg2
GWS20_TFg3$p1inp2vec <- is.element(GWS20_TFg3$player1, player2vector)
GWS20_TFg3$p2inp1vec <- is.element(GWS20_TFg3$player2, player1vector)

addPlayer1 <- GWS20_TFg3[ which(GWS20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_TFg3[ which(GWS20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_TFg2 <- rbind(GWS20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
GWS20_TFft <- ftable(GWS20_TFg2$player1, GWS20_TFg2$player2)
GWS20_TFft2 <- as.matrix(GWS20_TFft)
numRows <- nrow(GWS20_TFft2)
numCols <- ncol(GWS20_TFft2)
GWS20_TFft3 <- GWS20_TFft2[c(2:numRows) , c(2:numCols)]
GWS20_TFTable <- graph.adjacency(GWS20_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(GWS20_TFTable, vertex.label = V(GWS20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
GWS20_TF.clusterCoef <- transitivity(GWS20_TFTable, type="global") #cluster coefficient
GWS20_TF.degreeCent <- centralization.degree(GWS20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_TFftn <- as.network.matrix(GWS20_TFft)
GWS20_TF.netDensity <- network.density(GWS20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_TF.entropy <- entropy(GWS20_TFft) #entropy

GWS20_TF.netMx <- cbind(GWS20_TF.netMx, GWS20_TF.clusterCoef, GWS20_TF.degreeCent$centralization,
                        GWS20_TF.netDensity, GWS20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************
#NA

round = 20
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
GWS20_SAMg2 <- data.frame(GWS20_SAM)
GWS20_SAMg2 <- GWS20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_SAMg2$player1
player2vector <- GWS20_SAMg2$player2
GWS20_SAMg3 <- GWS20_SAMg2
GWS20_SAMg3$p1inp2vec <- is.element(GWS20_SAMg3$player1, player2vector)
GWS20_SAMg3$p2inp1vec <- is.element(GWS20_SAMg3$player2, player1vector)

addPlayer1 <- GWS20_SAMg3[ which(GWS20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_SAMg3[ which(GWS20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_SAMg2 <- rbind(GWS20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
GWS20_SAMft <- ftable(GWS20_SAMg2$player1, GWS20_SAMg2$player2)
GWS20_SAMft2 <- as.matrix(GWS20_SAMft)
numRows <- nrow(GWS20_SAMft2)
numCols <- ncol(GWS20_SAMft2)
GWS20_SAMft3 <- GWS20_SAMft2[c(2:numRows) , c(2:numCols)]
GWS20_SAMTable <- graph.adjacency(GWS20_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(GWS20_SAMTable, vertex.label = V(GWS20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
GWS20_SAM.clusterCoef <- transitivity(GWS20_SAMTable, type="global") #cluster coefficient
GWS20_SAM.degreeCent <- centralization.degree(GWS20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_SAMftn <- as.network.matrix(GWS20_SAMft)
GWS20_SAM.netDensity <- network.density(GWS20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_SAM.entropy <- entropy(GWS20_SAMft) #entropy

GWS20_SAM.netMx <- cbind(GWS20_SAM.netMx, GWS20_SAM.clusterCoef, GWS20_SAM.degreeCent$centralization,
                         GWS20_SAM.netDensity, GWS20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
GWS20_TAMg2 <- data.frame(GWS20_TAM)
GWS20_TAMg2 <- GWS20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_TAMg2$player1
player2vector <- GWS20_TAMg2$player2
GWS20_TAMg3 <- GWS20_TAMg2
GWS20_TAMg3$p1inp2vec <- is.element(GWS20_TAMg3$player1, player2vector)
GWS20_TAMg3$p2inp1vec <- is.element(GWS20_TAMg3$player2, player1vector)

addPlayer1 <- GWS20_TAMg3[ which(GWS20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_TAMg3[ which(GWS20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_TAMg2 <- rbind(GWS20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
GWS20_TAMft <- ftable(GWS20_TAMg2$player1, GWS20_TAMg2$player2)
GWS20_TAMft2 <- as.matrix(GWS20_TAMft)
numRows <- nrow(GWS20_TAMft2)
numCols <- ncol(GWS20_TAMft2)
GWS20_TAMft3 <- GWS20_TAMft2[c(2:numRows) , c(2:numCols)]
GWS20_TAMTable <- graph.adjacency(GWS20_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(GWS20_TAMTable, vertex.label = V(GWS20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
GWS20_TAM.clusterCoef <- transitivity(GWS20_TAMTable, type="global") #cluster coefficient
GWS20_TAM.degreeCent <- centralization.degree(GWS20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_TAMftn <- as.network.matrix(GWS20_TAMft)
GWS20_TAM.netDensity <- network.density(GWS20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_TAM.entropy <- entropy(GWS20_TAMft) #entropy

GWS20_TAM.netMx <- cbind(GWS20_TAM.netMx, GWS20_TAM.clusterCoef, GWS20_TAM.degreeCent$centralization,
                         GWS20_TAM.netDensity, GWS20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
GWS20_SDMg2 <- data.frame(GWS20_SDM)
GWS20_SDMg2 <- GWS20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_SDMg2$player1
player2vector <- GWS20_SDMg2$player2
GWS20_SDMg3 <- GWS20_SDMg2
GWS20_SDMg3$p1inp2vec <- is.element(GWS20_SDMg3$player1, player2vector)
GWS20_SDMg3$p2inp1vec <- is.element(GWS20_SDMg3$player2, player1vector)

addPlayer1 <- GWS20_SDMg3[ which(GWS20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_SDMg3[ which(GWS20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_SDMg2 <- rbind(GWS20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
GWS20_SDMft <- ftable(GWS20_SDMg2$player1, GWS20_SDMg2$player2)
GWS20_SDMft2 <- as.matrix(GWS20_SDMft)
numRows <- nrow(GWS20_SDMft2)
numCols <- ncol(GWS20_SDMft2)
GWS20_SDMft3 <- GWS20_SDMft2[c(2:numRows) , c(2:numCols)]
GWS20_SDMTable <- graph.adjacency(GWS20_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(GWS20_SDMTable, vertex.label = V(GWS20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
GWS20_SDM.clusterCoef <- transitivity(GWS20_SDMTable, type="global") #cluster coefficient
GWS20_SDM.degreeCent <- centralization.degree(GWS20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_SDMftn <- as.network.matrix(GWS20_SDMft)
GWS20_SDM.netDensity <- network.density(GWS20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_SDM.entropy <- entropy(GWS20_SDMft) #entropy

GWS20_SDM.netMx <- cbind(GWS20_SDM.netMx, GWS20_SDM.clusterCoef, GWS20_SDM.degreeCent$centralization,
                         GWS20_SDM.netDensity, GWS20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
GWS20_TDMg2 <- data.frame(GWS20_TDM)
GWS20_TDMg2 <- GWS20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_TDMg2$player1
player2vector <- GWS20_TDMg2$player2
GWS20_TDMg3 <- GWS20_TDMg2
GWS20_TDMg3$p1inp2vec <- is.element(GWS20_TDMg3$player1, player2vector)
GWS20_TDMg3$p2inp1vec <- is.element(GWS20_TDMg3$player2, player1vector)

addPlayer1 <- GWS20_TDMg3[ which(GWS20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_TDMg3[ which(GWS20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_TDMg2 <- rbind(GWS20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
GWS20_TDMft <- ftable(GWS20_TDMg2$player1, GWS20_TDMg2$player2)
GWS20_TDMft2 <- as.matrix(GWS20_TDMft)
numRows <- nrow(GWS20_TDMft2)
numCols <- ncol(GWS20_TDMft2)
GWS20_TDMft3 <- GWS20_TDMft2[c(2:numRows) , c(2:numCols)]
GWS20_TDMTable <- graph.adjacency(GWS20_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(GWS20_TDMTable, vertex.label = V(GWS20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
GWS20_TDM.clusterCoef <- transitivity(GWS20_TDMTable, type="global") #cluster coefficient
GWS20_TDM.degreeCent <- centralization.degree(GWS20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_TDMftn <- as.network.matrix(GWS20_TDMft)
GWS20_TDM.netDensity <- network.density(GWS20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_TDM.entropy <- entropy(GWS20_TDMft) #entropy

GWS20_TDM.netMx <- cbind(GWS20_TDM.netMx, GWS20_TDM.clusterCoef, GWS20_TDM.degreeCent$centralization,
                         GWS20_TDM.netDensity, GWS20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
GWS20_SDg2 <- data.frame(GWS20_SD)
GWS20_SDg2 <- GWS20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_SDg2$player1
player2vector <- GWS20_SDg2$player2
GWS20_SDg3 <- GWS20_SDg2
GWS20_SDg3$p1inp2vec <- is.element(GWS20_SDg3$player1, player2vector)
GWS20_SDg3$p2inp1vec <- is.element(GWS20_SDg3$player2, player1vector)

addPlayer1 <- GWS20_SDg3[ which(GWS20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_SDg3[ which(GWS20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_SDg2 <- rbind(GWS20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
GWS20_SDft <- ftable(GWS20_SDg2$player1, GWS20_SDg2$player2)
GWS20_SDft2 <- as.matrix(GWS20_SDft)
numRows <- nrow(GWS20_SDft2)
numCols <- ncol(GWS20_SDft2)
GWS20_SDft3 <- GWS20_SDft2[c(2:numRows) , c(2:numCols)]
GWS20_SDTable <- graph.adjacency(GWS20_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(GWS20_SDTable, vertex.label = V(GWS20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
GWS20_SD.clusterCoef <- transitivity(GWS20_SDTable, type="global") #cluster coefficient
GWS20_SD.degreeCent <- centralization.degree(GWS20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_SDftn <- as.network.matrix(GWS20_SDft)
GWS20_SD.netDensity <- network.density(GWS20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_SD.entropy <- entropy(GWS20_SDft) #entropy

GWS20_SD.netMx <- cbind(GWS20_SD.netMx, GWS20_SD.clusterCoef, GWS20_SD.degreeCent$centralization,
                        GWS20_SD.netDensity, GWS20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
GWS20_TDg2 <- data.frame(GWS20_TD)
GWS20_TDg2 <- GWS20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_TDg2$player1
player2vector <- GWS20_TDg2$player2
GWS20_TDg3 <- GWS20_TDg2
GWS20_TDg3$p1inp2vec <- is.element(GWS20_TDg3$player1, player2vector)
GWS20_TDg3$p2inp1vec <- is.element(GWS20_TDg3$player2, player1vector)

addPlayer1 <- GWS20_TDg3[ which(GWS20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_TDg3[ which(GWS20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_TDg2 <- rbind(GWS20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
GWS20_TDft <- ftable(GWS20_TDg2$player1, GWS20_TDg2$player2)
GWS20_TDft2 <- as.matrix(GWS20_TDft)
numRows <- nrow(GWS20_TDft2)
numCols <- ncol(GWS20_TDft2)
GWS20_TDft3 <- GWS20_TDft2[c(2:numRows) , c(2:numCols)]
GWS20_TDTable <- graph.adjacency(GWS20_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(GWS20_TDTable, vertex.label = V(GWS20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
GWS20_TD.clusterCoef <- transitivity(GWS20_TDTable, type="global") #cluster coefficient
GWS20_TD.degreeCent <- centralization.degree(GWS20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_TDftn <- as.network.matrix(GWS20_TDft)
GWS20_TD.netDensity <- network.density(GWS20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_TD.entropy <- entropy(GWS20_TDft) #entropy

GWS20_TD.netMx <- cbind(GWS20_TD.netMx, GWS20_TD.clusterCoef, GWS20_TD.degreeCent$centralization,
                        GWS20_TD.netDensity, GWS20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
GWS20_QTg2 <- data.frame(GWS20_QT)
GWS20_QTg2 <- GWS20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS20_QTg2$player1
player2vector <- GWS20_QTg2$player2
GWS20_QTg3 <- GWS20_QTg2
GWS20_QTg3$p1inp2vec <- is.element(GWS20_QTg3$player1, player2vector)
GWS20_QTg3$p2inp1vec <- is.element(GWS20_QTg3$player2, player1vector)

addPlayer1 <- GWS20_QTg3[ which(GWS20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS20_QTg3[ which(GWS20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS20_QTg2 <- rbind(GWS20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
GWS20_QTft <- ftable(GWS20_QTg2$player1, GWS20_QTg2$player2)
GWS20_QTft2 <- as.matrix(GWS20_QTft)
numRows <- nrow(GWS20_QTft2)
numCols <- ncol(GWS20_QTft2)
GWS20_QTft3 <- GWS20_QTft2[c(2:numRows) , c(2:numCols)]
GWS20_QTTable <- graph.adjacency(GWS20_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(GWS20_QTTable, vertex.label = V(GWS20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
GWS20_QT.clusterCoef <- transitivity(GWS20_QTTable, type="global") #cluster coefficient
GWS20_QT.degreeCent <- centralization.degree(GWS20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS20_QTftn <- as.network.matrix(GWS20_QTft)
GWS20_QT.netDensity <- network.density(GWS20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS20_QT.entropy <- entropy(GWS20_QTft) #entropy

GWS20_QT.netMx <- cbind(GWS20_QT.netMx, GWS20_QT.clusterCoef, GWS20_QT.degreeCent$centralization,
                        GWS20_QT.netDensity, GWS20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS20_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "HAW"
KIoutcome = "Goal_F"
HAW20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
HAW20_Gg2 <- data.frame(HAW20_G)
HAW20_Gg2 <- HAW20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_Gg2$player1
player2vector <- HAW20_Gg2$player2
HAW20_Gg3 <- HAW20_Gg2
HAW20_Gg3$p1inp2vec <- is.element(HAW20_Gg3$player1, player2vector)
HAW20_Gg3$p2inp1vec <- is.element(HAW20_Gg3$player2, player1vector)

addPlayer1 <- HAW20_Gg3[ which(HAW20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW20_Gg3[ which(HAW20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_Gg2 <- rbind(HAW20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
HAW20_Gft <- ftable(HAW20_Gg2$player1, HAW20_Gg2$player2)
HAW20_Gft2 <- as.matrix(HAW20_Gft)
numRows <- nrow(HAW20_Gft2)
numCols <- ncol(HAW20_Gft2)
HAW20_Gft3 <- HAW20_Gft2[c(2:numRows) , c(2:numCols)]
HAW20_GTable <- graph.adjacency(HAW20_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(HAW20_GTable, vertex.label = V(HAW20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
HAW20_G.clusterCoef <- transitivity(HAW20_GTable, type="global") #cluster coefficient
HAW20_G.degreeCent <- centralization.degree(HAW20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_Gftn <- as.network.matrix(HAW20_Gft)
HAW20_G.netDensity <- network.density(HAW20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_G.entropy <- entropy(HAW20_Gft) #entropy

HAW20_G.netMx <- cbind(HAW20_G.netMx, HAW20_G.clusterCoef, HAW20_G.degreeCent$centralization,
                       HAW20_G.netDensity, HAW20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "HAW"
KIoutcome = "Behind_F"
HAW20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
HAW20_Bg2 <- data.frame(HAW20_B)
HAW20_Bg2 <- HAW20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_Bg2$player1
player2vector <- HAW20_Bg2$player2
HAW20_Bg3 <- HAW20_Bg2
HAW20_Bg3$p1inp2vec <- is.element(HAW20_Bg3$player1, player2vector)
HAW20_Bg3$p2inp1vec <- is.element(HAW20_Bg3$player2, player1vector)

addPlayer1 <- HAW20_Bg3[ which(HAW20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW20_Bg2 <- rbind(HAW20_Bg2, addPlayer1)

#ROUND 20, Behind graph using weighted edges
HAW20_Bft <- ftable(HAW20_Bg2$player1, HAW20_Bg2$player2)
HAW20_Bft2 <- as.matrix(HAW20_Bft)
numRows <- nrow(HAW20_Bft2)
numCols <- ncol(HAW20_Bft2)
HAW20_Bft3 <- HAW20_Bft2[c(2:numRows) , c(1:numCols)]
HAW20_BTable <- graph.adjacency(HAW20_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(HAW20_BTable, vertex.label = V(HAW20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
HAW20_B.clusterCoef <- transitivity(HAW20_BTable, type="global") #cluster coefficient
HAW20_B.degreeCent <- centralization.degree(HAW20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_Bftn <- as.network.matrix(HAW20_Bft)
HAW20_B.netDensity <- network.density(HAW20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_B.entropy <- entropy(HAW20_Bft) #entropy

HAW20_B.netMx <- cbind(HAW20_B.netMx, HAW20_B.clusterCoef, HAW20_B.degreeCent$centralization,
                       HAW20_B.netDensity, HAW20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
HAW20_SFg2 <- data.frame(HAW20_SF)
HAW20_SFg2 <- HAW20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_SFg2$player1
player2vector <- HAW20_SFg2$player2
HAW20_SFg3 <- HAW20_SFg2
HAW20_SFg3$p1inp2vec <- is.element(HAW20_SFg3$player1, player2vector)
HAW20_SFg3$p2inp1vec <- is.element(HAW20_SFg3$player2, player1vector)

addPlayer1 <- HAW20_SFg3[ which(HAW20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW20_SFg3[ which(HAW20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_SFg2 <- rbind(HAW20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
HAW20_SFft <- ftable(HAW20_SFg2$player1, HAW20_SFg2$player2)
HAW20_SFft2 <- as.matrix(HAW20_SFft)
numRows <- nrow(HAW20_SFft2)
numCols <- ncol(HAW20_SFft2)
HAW20_SFft3 <- HAW20_SFft2[c(2:numRows) , c(2:numCols)]
HAW20_SFTable <- graph.adjacency(HAW20_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(HAW20_SFTable, vertex.label = V(HAW20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
HAW20_SF.clusterCoef <- transitivity(HAW20_SFTable, type="global") #cluster coefficient
HAW20_SF.degreeCent <- centralization.degree(HAW20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_SFftn <- as.network.matrix(HAW20_SFft)
HAW20_SF.netDensity <- network.density(HAW20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_SF.entropy <- entropy(HAW20_SFft) #entropy

HAW20_SF.netMx <- cbind(HAW20_SF.netMx, HAW20_SF.clusterCoef, HAW20_SF.degreeCent$centralization,
                        HAW20_SF.netDensity, HAW20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
HAW20_TFg2 <- data.frame(HAW20_TF)
HAW20_TFg2 <- HAW20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_TFg2$player1
player2vector <- HAW20_TFg2$player2
HAW20_TFg3 <- HAW20_TFg2
HAW20_TFg3$p1inp2vec <- is.element(HAW20_TFg3$player1, player2vector)
HAW20_TFg3$p2inp1vec <- is.element(HAW20_TFg3$player2, player1vector)

addPlayer1 <- HAW20_TFg3[ which(HAW20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- HAW20_TFg3[ which(HAW20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_TFg2 <- rbind(HAW20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
HAW20_TFft <- ftable(HAW20_TFg2$player1, HAW20_TFg2$player2)
HAW20_TFft2 <- as.matrix(HAW20_TFft)
numRows <- nrow(HAW20_TFft2)
numCols <- ncol(HAW20_TFft2)
HAW20_TFft3 <- HAW20_TFft2[c(2:numRows) , c(2:numCols)]
HAW20_TFTable <- graph.adjacency(HAW20_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(HAW20_TFTable, vertex.label = V(HAW20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
HAW20_TF.clusterCoef <- transitivity(HAW20_TFTable, type="global") #cluster coefficient
HAW20_TF.degreeCent <- centralization.degree(HAW20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_TFftn <- as.network.matrix(HAW20_TFft)
HAW20_TF.netDensity <- network.density(HAW20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_TF.entropy <- entropy(HAW20_TFft) #entropy

HAW20_TF.netMx <- cbind(HAW20_TF.netMx, HAW20_TF.clusterCoef, HAW20_TF.degreeCent$centralization,
                        HAW20_TF.netDensity, HAW20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************

round = 20
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
HAW20_SAMg2 <- data.frame(HAW20_SAM)
HAW20_SAMg2 <- HAW20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_SAMg2$player1
player2vector <- HAW20_SAMg2$player2
HAW20_SAMg3 <- HAW20_SAMg2
HAW20_SAMg3$p1inp2vec <- is.element(HAW20_SAMg3$player1, player2vector)
HAW20_SAMg3$p2inp1vec <- is.element(HAW20_SAMg3$player2, player1vector)

addPlayer1 <- HAW20_SAMg3[ which(HAW20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW20_SAMg3[ which(HAW20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_SAMg2 <- rbind(HAW20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
HAW20_SAMft <- ftable(HAW20_SAMg2$player1, HAW20_SAMg2$player2)
HAW20_SAMft2 <- as.matrix(HAW20_SAMft)
numRows <- nrow(HAW20_SAMft2)
numCols <- ncol(HAW20_SAMft2)
HAW20_SAMft3 <- HAW20_SAMft2[c(2:numRows) , c(2:numCols)]
HAW20_SAMTable <- graph.adjacency(HAW20_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(HAW20_SAMTable, vertex.label = V(HAW20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
HAW20_SAM.clusterCoef <- transitivity(HAW20_SAMTable, type="global") #cluster coefficient
HAW20_SAM.degreeCent <- centralization.degree(HAW20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_SAMftn <- as.network.matrix(HAW20_SAMft)
HAW20_SAM.netDensity <- network.density(HAW20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_SAM.entropy <- entropy(HAW20_SAMft) #entropy

HAW20_SAM.netMx <- cbind(HAW20_SAM.netMx, HAW20_SAM.clusterCoef, HAW20_SAM.degreeCent$centralization,
                         HAW20_SAM.netDensity, HAW20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
HAW20_TAMg2 <- data.frame(HAW20_TAM)
HAW20_TAMg2 <- HAW20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_TAMg2$player1
player2vector <- HAW20_TAMg2$player2
HAW20_TAMg3 <- HAW20_TAMg2
HAW20_TAMg3$p1inp2vec <- is.element(HAW20_TAMg3$player1, player2vector)
HAW20_TAMg3$p2inp1vec <- is.element(HAW20_TAMg3$player2, player1vector)

addPlayer1 <- HAW20_TAMg3[ which(HAW20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW20_TAMg3[ which(HAW20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_TAMg2 <- rbind(HAW20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
HAW20_TAMft <- ftable(HAW20_TAMg2$player1, HAW20_TAMg2$player2)
HAW20_TAMft2 <- as.matrix(HAW20_TAMft)
numRows <- nrow(HAW20_TAMft2)
numCols <- ncol(HAW20_TAMft2)
HAW20_TAMft3 <- HAW20_TAMft2[c(2:numRows) , c(2:numCols)]
HAW20_TAMTable <- graph.adjacency(HAW20_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(HAW20_TAMTable, vertex.label = V(HAW20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
HAW20_TAM.clusterCoef <- transitivity(HAW20_TAMTable, type="global") #cluster coefficient
HAW20_TAM.degreeCent <- centralization.degree(HAW20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_TAMftn <- as.network.matrix(HAW20_TAMft)
HAW20_TAM.netDensity <- network.density(HAW20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_TAM.entropy <- entropy(HAW20_TAMft) #entropy

HAW20_TAM.netMx <- cbind(HAW20_TAM.netMx, HAW20_TAM.clusterCoef, HAW20_TAM.degreeCent$centralization,
                         HAW20_TAM.netDensity, HAW20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
HAW20_SDMg2 <- data.frame(HAW20_SDM)
HAW20_SDMg2 <- HAW20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_SDMg2$player1
player2vector <- HAW20_SDMg2$player2
HAW20_SDMg3 <- HAW20_SDMg2
HAW20_SDMg3$p1inp2vec <- is.element(HAW20_SDMg3$player1, player2vector)
HAW20_SDMg3$p2inp1vec <- is.element(HAW20_SDMg3$player2, player1vector)

addPlayer1 <- HAW20_SDMg3[ which(HAW20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW20_SDMg3[ which(HAW20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_SDMg2 <- rbind(HAW20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
HAW20_SDMft <- ftable(HAW20_SDMg2$player1, HAW20_SDMg2$player2)
HAW20_SDMft2 <- as.matrix(HAW20_SDMft)
numRows <- nrow(HAW20_SDMft2)
numCols <- ncol(HAW20_SDMft2)
HAW20_SDMft3 <- HAW20_SDMft2[c(2:numRows) , c(2:numCols)]
HAW20_SDMTable <- graph.adjacency(HAW20_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(HAW20_SDMTable, vertex.label = V(HAW20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
HAW20_SDM.clusterCoef <- transitivity(HAW20_SDMTable, type="global") #cluster coefficient
HAW20_SDM.degreeCent <- centralization.degree(HAW20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_SDMftn <- as.network.matrix(HAW20_SDMft)
HAW20_SDM.netDensity <- network.density(HAW20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_SDM.entropy <- entropy(HAW20_SDMft) #entropy

HAW20_SDM.netMx <- cbind(HAW20_SDM.netMx, HAW20_SDM.clusterCoef, HAW20_SDM.degreeCent$centralization,
                         HAW20_SDM.netDensity, HAW20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
HAW20_TDMg2 <- data.frame(HAW20_TDM)
HAW20_TDMg2 <- HAW20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_TDMg2$player1
player2vector <- HAW20_TDMg2$player2
HAW20_TDMg3 <- HAW20_TDMg2
HAW20_TDMg3$p1inp2vec <- is.element(HAW20_TDMg3$player1, player2vector)
HAW20_TDMg3$p2inp1vec <- is.element(HAW20_TDMg3$player2, player1vector)

addPlayer1 <- HAW20_TDMg3[ which(HAW20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW20_TDMg3[ which(HAW20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_TDMg2 <- rbind(HAW20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
HAW20_TDMft <- ftable(HAW20_TDMg2$player1, HAW20_TDMg2$player2)
HAW20_TDMft2 <- as.matrix(HAW20_TDMft)
numRows <- nrow(HAW20_TDMft2)
numCols <- ncol(HAW20_TDMft2)
HAW20_TDMft3 <- HAW20_TDMft2[c(2:numRows) , c(2:numCols)]
HAW20_TDMTable <- graph.adjacency(HAW20_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(HAW20_TDMTable, vertex.label = V(HAW20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
HAW20_TDM.clusterCoef <- transitivity(HAW20_TDMTable, type="global") #cluster coefficient
HAW20_TDM.degreeCent <- centralization.degree(HAW20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_TDMftn <- as.network.matrix(HAW20_TDMft)
HAW20_TDM.netDensity <- network.density(HAW20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_TDM.entropy <- entropy(HAW20_TDMft) #entropy

HAW20_TDM.netMx <- cbind(HAW20_TDM.netMx, HAW20_TDM.clusterCoef, HAW20_TDM.degreeCent$centralization,
                         HAW20_TDM.netDensity, HAW20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
HAW20_SDg2 <- data.frame(HAW20_SD)
HAW20_SDg2 <- HAW20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_SDg2$player1
player2vector <- HAW20_SDg2$player2
HAW20_SDg3 <- HAW20_SDg2
HAW20_SDg3$p1inp2vec <- is.element(HAW20_SDg3$player1, player2vector)
HAW20_SDg3$p2inp1vec <- is.element(HAW20_SDg3$player2, player1vector)

addPlayer1 <- HAW20_SDg3[ which(HAW20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW20_SDg3[ which(HAW20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_SDg2 <- rbind(HAW20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
HAW20_SDft <- ftable(HAW20_SDg2$player1, HAW20_SDg2$player2)
HAW20_SDft2 <- as.matrix(HAW20_SDft)
numRows <- nrow(HAW20_SDft2)
numCols <- ncol(HAW20_SDft2)
HAW20_SDft3 <- HAW20_SDft2[c(2:numRows) , c(2:numCols)]
HAW20_SDTable <- graph.adjacency(HAW20_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(HAW20_SDTable, vertex.label = V(HAW20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
HAW20_SD.clusterCoef <- transitivity(HAW20_SDTable, type="global") #cluster coefficient
HAW20_SD.degreeCent <- centralization.degree(HAW20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_SDftn <- as.network.matrix(HAW20_SDft)
HAW20_SD.netDensity <- network.density(HAW20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_SD.entropy <- entropy(HAW20_SDft) #entropy

HAW20_SD.netMx <- cbind(HAW20_SD.netMx, HAW20_SD.clusterCoef, HAW20_SD.degreeCent$centralization,
                        HAW20_SD.netDensity, HAW20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
HAW20_TDg2 <- data.frame(HAW20_TD)
HAW20_TDg2 <- HAW20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_TDg2$player1
player2vector <- HAW20_TDg2$player2
HAW20_TDg3 <- HAW20_TDg2
HAW20_TDg3$p1inp2vec <- is.element(HAW20_TDg3$player1, player2vector)
HAW20_TDg3$p2inp1vec <- is.element(HAW20_TDg3$player2, player1vector)

addPlayer1 <- HAW20_TDg3[ which(HAW20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW20_TDg3[ which(HAW20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_TDg2 <- rbind(HAW20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
HAW20_TDft <- ftable(HAW20_TDg2$player1, HAW20_TDg2$player2)
HAW20_TDft2 <- as.matrix(HAW20_TDft)
numRows <- nrow(HAW20_TDft2)
numCols <- ncol(HAW20_TDft2)
HAW20_TDft3 <- HAW20_TDft2[c(2:numRows) , c(2:numCols)]
HAW20_TDTable <- graph.adjacency(HAW20_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(HAW20_TDTable, vertex.label = V(HAW20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
HAW20_TD.clusterCoef <- transitivity(HAW20_TDTable, type="global") #cluster coefficient
HAW20_TD.degreeCent <- centralization.degree(HAW20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_TDftn <- as.network.matrix(HAW20_TDft)
HAW20_TD.netDensity <- network.density(HAW20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_TD.entropy <- entropy(HAW20_TDft) #entropy

HAW20_TD.netMx <- cbind(HAW20_TD.netMx, HAW20_TD.clusterCoef, HAW20_TD.degreeCent$centralization,
                        HAW20_TD.netDensity, HAW20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
HAW20_QTg2 <- data.frame(HAW20_QT)
HAW20_QTg2 <- HAW20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW20_QTg2$player1
player2vector <- HAW20_QTg2$player2
HAW20_QTg3 <- HAW20_QTg2
HAW20_QTg3$p1inp2vec <- is.element(HAW20_QTg3$player1, player2vector)
HAW20_QTg3$p2inp1vec <- is.element(HAW20_QTg3$player2, player1vector)

addPlayer1 <- HAW20_QTg3[ which(HAW20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW20_QTg3[ which(HAW20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW20_QTg2 <- rbind(HAW20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
HAW20_QTft <- ftable(HAW20_QTg2$player1, HAW20_QTg2$player2)
HAW20_QTft2 <- as.matrix(HAW20_QTft)
numRows <- nrow(HAW20_QTft2)
numCols <- ncol(HAW20_QTft2)
HAW20_QTft3 <- HAW20_QTft2[c(2:numRows) , c(2:numCols)]
HAW20_QTTable <- graph.adjacency(HAW20_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(HAW20_QTTable, vertex.label = V(HAW20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
HAW20_QT.clusterCoef <- transitivity(HAW20_QTTable, type="global") #cluster coefficient
HAW20_QT.degreeCent <- centralization.degree(HAW20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW20_QTftn <- as.network.matrix(HAW20_QTft)
HAW20_QT.netDensity <- network.density(HAW20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW20_QT.entropy <- entropy(HAW20_QTft) #entropy

HAW20_QT.netMx <- cbind(HAW20_QT.netMx, HAW20_QT.clusterCoef, HAW20_QT.degreeCent$centralization,
                        HAW20_QT.netDensity, HAW20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW20_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "MELB"
KIoutcome = "Goal_F"
MELB20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
MELB20_Gg2 <- data.frame(MELB20_G)
MELB20_Gg2 <- MELB20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_Gg2$player1
player2vector <- MELB20_Gg2$player2
MELB20_Gg3 <- MELB20_Gg2
MELB20_Gg3$p1inp2vec <- is.element(MELB20_Gg3$player1, player2vector)
MELB20_Gg3$p2inp1vec <- is.element(MELB20_Gg3$player2, player1vector)

addPlayer1 <- MELB20_Gg3[ which(MELB20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_Gg3[ which(MELB20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_Gg2 <- rbind(MELB20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
MELB20_Gft <- ftable(MELB20_Gg2$player1, MELB20_Gg2$player2)
MELB20_Gft2 <- as.matrix(MELB20_Gft)
numRows <- nrow(MELB20_Gft2)
numCols <- ncol(MELB20_Gft2)
MELB20_Gft3 <- MELB20_Gft2[c(2:numRows) , c(2:numCols)]
MELB20_GTable <- graph.adjacency(MELB20_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(MELB20_GTable, vertex.label = V(MELB20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
MELB20_G.clusterCoef <- transitivity(MELB20_GTable, type="global") #cluster coefficient
MELB20_G.degreeCent <- centralization.degree(MELB20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_Gftn <- as.network.matrix(MELB20_Gft)
MELB20_G.netDensity <- network.density(MELB20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_G.entropy <- entropy(MELB20_Gft) #entropy

MELB20_G.netMx <- cbind(MELB20_G.netMx, MELB20_G.clusterCoef, MELB20_G.degreeCent$centralization,
                        MELB20_G.netDensity, MELB20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************

round = 20
teamName = "MELB"
KIoutcome = "Behind_F"
MELB20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
MELB20_Bg2 <- data.frame(MELB20_B)
MELB20_Bg2 <- MELB20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_Bg2$player1
player2vector <- MELB20_Bg2$player2
MELB20_Bg3 <- MELB20_Bg2
MELB20_Bg3$p1inp2vec <- is.element(MELB20_Bg3$player1, player2vector)
MELB20_Bg3$p2inp1vec <- is.element(MELB20_Bg3$player2, player1vector)

addPlayer1 <- MELB20_Bg3[ which(MELB20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- MELB20_Bg3[ which(MELB20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_Bg2 <- rbind(MELB20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
MELB20_Bft <- ftable(MELB20_Bg2$player1, MELB20_Bg2$player2)
MELB20_Bft2 <- as.matrix(MELB20_Bft)
numRows <- nrow(MELB20_Bft2)
numCols <- ncol(MELB20_Bft2)
MELB20_Bft3 <- MELB20_Bft2[c(2:numRows) , c(2:numCols)]
MELB20_BTable <- graph.adjacency(MELB20_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(MELB20_BTable, vertex.label = V(MELB20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
MELB20_B.clusterCoef <- transitivity(MELB20_BTable, type="global") #cluster coefficient
MELB20_B.degreeCent <- centralization.degree(MELB20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_Bftn <- as.network.matrix(MELB20_Bft)
MELB20_B.netDensity <- network.density(MELB20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_B.entropy <- entropy(MELB20_Bft) #entropy

MELB20_B.netMx <- cbind(MELB20_B.netMx, MELB20_B.clusterCoef, MELB20_B.degreeCent$centralization,
                        MELB20_B.netDensity, MELB20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
MELB20_SFg2 <- data.frame(MELB20_SF)
MELB20_SFg2 <- MELB20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_SFg2$player1
player2vector <- MELB20_SFg2$player2
MELB20_SFg3 <- MELB20_SFg2
MELB20_SFg3$p1inp2vec <- is.element(MELB20_SFg3$player1, player2vector)
MELB20_SFg3$p2inp1vec <- is.element(MELB20_SFg3$player2, player1vector)

addPlayer1 <- MELB20_SFg3[ which(MELB20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_SFg3[ which(MELB20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_SFg2 <- rbind(MELB20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
MELB20_SFft <- ftable(MELB20_SFg2$player1, MELB20_SFg2$player2)
MELB20_SFft2 <- as.matrix(MELB20_SFft)
numRows <- nrow(MELB20_SFft2)
numCols <- ncol(MELB20_SFft2)
MELB20_SFft3 <- MELB20_SFft2[c(2:numRows) , c(2:numCols)]
MELB20_SFTable <- graph.adjacency(MELB20_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(MELB20_SFTable, vertex.label = V(MELB20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
MELB20_SF.clusterCoef <- transitivity(MELB20_SFTable, type="global") #cluster coefficient
MELB20_SF.degreeCent <- centralization.degree(MELB20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_SFftn <- as.network.matrix(MELB20_SFft)
MELB20_SF.netDensity <- network.density(MELB20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_SF.entropy <- entropy(MELB20_SFft) #entropy

MELB20_SF.netMx <- cbind(MELB20_SF.netMx, MELB20_SF.clusterCoef, MELB20_SF.degreeCent$centralization,
                         MELB20_SF.netDensity, MELB20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
MELB20_TFg2 <- data.frame(MELB20_TF)
MELB20_TFg2 <- MELB20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_TFg2$player1
player2vector <- MELB20_TFg2$player2
MELB20_TFg3 <- MELB20_TFg2
MELB20_TFg3$p1inp2vec <- is.element(MELB20_TFg3$player1, player2vector)
MELB20_TFg3$p2inp1vec <- is.element(MELB20_TFg3$player2, player1vector)

addPlayer1 <- MELB20_TFg3[ which(MELB20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_TFg3[ which(MELB20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_TFg2 <- rbind(MELB20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
MELB20_TFft <- ftable(MELB20_TFg2$player1, MELB20_TFg2$player2)
MELB20_TFft2 <- as.matrix(MELB20_TFft)
numRows <- nrow(MELB20_TFft2)
numCols <- ncol(MELB20_TFft2)
MELB20_TFft3 <- MELB20_TFft2[c(2:numRows) , c(2:numCols)]
MELB20_TFTable <- graph.adjacency(MELB20_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(MELB20_TFTable, vertex.label = V(MELB20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
MELB20_TF.clusterCoef <- transitivity(MELB20_TFTable, type="global") #cluster coefficient
MELB20_TF.degreeCent <- centralization.degree(MELB20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_TFftn <- as.network.matrix(MELB20_TFft)
MELB20_TF.netDensity <- network.density(MELB20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_TF.entropy <- entropy(MELB20_TFft) #entropy

MELB20_TF.netMx <- cbind(MELB20_TF.netMx, MELB20_TF.clusterCoef, MELB20_TF.degreeCent$centralization,
                         MELB20_TF.netDensity, MELB20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************
#NA

round = 20
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
MELB20_SAMg2 <- data.frame(MELB20_SAM)
MELB20_SAMg2 <- MELB20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_SAMg2$player1
player2vector <- MELB20_SAMg2$player2
MELB20_SAMg3 <- MELB20_SAMg2
MELB20_SAMg3$p1inp2vec <- is.element(MELB20_SAMg3$player1, player2vector)
MELB20_SAMg3$p2inp1vec <- is.element(MELB20_SAMg3$player2, player1vector)

addPlayer1 <- MELB20_SAMg3[ which(MELB20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_SAMg3[ which(MELB20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_SAMg2 <- rbind(MELB20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
MELB20_SAMft <- ftable(MELB20_SAMg2$player1, MELB20_SAMg2$player2)
MELB20_SAMft2 <- as.matrix(MELB20_SAMft)
numRows <- nrow(MELB20_SAMft2)
numCols <- ncol(MELB20_SAMft2)
MELB20_SAMft3 <- MELB20_SAMft2[c(2:numRows) , c(2:numCols)]
MELB20_SAMTable <- graph.adjacency(MELB20_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(MELB20_SAMTable, vertex.label = V(MELB20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
MELB20_SAM.clusterCoef <- transitivity(MELB20_SAMTable, type="global") #cluster coefficient
MELB20_SAM.degreeCent <- centralization.degree(MELB20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_SAMftn <- as.network.matrix(MELB20_SAMft)
MELB20_SAM.netDensity <- network.density(MELB20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_SAM.entropy <- entropy(MELB20_SAMft) #entropy

MELB20_SAM.netMx <- cbind(MELB20_SAM.netMx, MELB20_SAM.clusterCoef, MELB20_SAM.degreeCent$centralization,
                          MELB20_SAM.netDensity, MELB20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************
#NA

round = 20
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
MELB20_TAMg2 <- data.frame(MELB20_TAM)
MELB20_TAMg2 <- MELB20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_TAMg2$player1
player2vector <- MELB20_TAMg2$player2
MELB20_TAMg3 <- MELB20_TAMg2
MELB20_TAMg3$p1inp2vec <- is.element(MELB20_TAMg3$player1, player2vector)
MELB20_TAMg3$p2inp1vec <- is.element(MELB20_TAMg3$player2, player1vector)

addPlayer1 <- MELB20_TAMg3[ which(MELB20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_TAMg3[ which(MELB20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_TAMg2 <- rbind(MELB20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
MELB20_TAMft <- ftable(MELB20_TAMg2$player1, MELB20_TAMg2$player2)
MELB20_TAMft2 <- as.matrix(MELB20_TAMft)
numRows <- nrow(MELB20_TAMft2)
numCols <- ncol(MELB20_TAMft2)
MELB20_TAMft3 <- MELB20_TAMft2[c(2:numRows) , c(2:numCols)]
MELB20_TAMTable <- graph.adjacency(MELB20_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(MELB20_TAMTable, vertex.label = V(MELB20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
MELB20_TAM.clusterCoef <- transitivity(MELB20_TAMTable, type="global") #cluster coefficient
MELB20_TAM.degreeCent <- centralization.degree(MELB20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_TAMftn <- as.network.matrix(MELB20_TAMft)
MELB20_TAM.netDensity <- network.density(MELB20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_TAM.entropy <- entropy(MELB20_TAMft) #entropy

MELB20_TAM.netMx <- cbind(MELB20_TAM.netMx, MELB20_TAM.clusterCoef, MELB20_TAM.degreeCent$centralization,
                          MELB20_TAM.netDensity, MELB20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
MELB20_SDMg2 <- data.frame(MELB20_SDM)
MELB20_SDMg2 <- MELB20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_SDMg2$player1
player2vector <- MELB20_SDMg2$player2
MELB20_SDMg3 <- MELB20_SDMg2
MELB20_SDMg3$p1inp2vec <- is.element(MELB20_SDMg3$player1, player2vector)
MELB20_SDMg3$p2inp1vec <- is.element(MELB20_SDMg3$player2, player1vector)

addPlayer1 <- MELB20_SDMg3[ which(MELB20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_SDMg3[ which(MELB20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_SDMg2 <- rbind(MELB20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
MELB20_SDMft <- ftable(MELB20_SDMg2$player1, MELB20_SDMg2$player2)
MELB20_SDMft2 <- as.matrix(MELB20_SDMft)
numRows <- nrow(MELB20_SDMft2)
numCols <- ncol(MELB20_SDMft2)
MELB20_SDMft3 <- MELB20_SDMft2[c(2:numRows) , c(2:numCols)]
MELB20_SDMTable <- graph.adjacency(MELB20_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(MELB20_SDMTable, vertex.label = V(MELB20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
MELB20_SDM.clusterCoef <- transitivity(MELB20_SDMTable, type="global") #cluster coefficient
MELB20_SDM.degreeCent <- centralization.degree(MELB20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_SDMftn <- as.network.matrix(MELB20_SDMft)
MELB20_SDM.netDensity <- network.density(MELB20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_SDM.entropy <- entropy(MELB20_SDMft) #entropy

MELB20_SDM.netMx <- cbind(MELB20_SDM.netMx, MELB20_SDM.clusterCoef, MELB20_SDM.degreeCent$centralization,
                          MELB20_SDM.netDensity, MELB20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
MELB20_TDMg2 <- data.frame(MELB20_TDM)
MELB20_TDMg2 <- MELB20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_TDMg2$player1
player2vector <- MELB20_TDMg2$player2
MELB20_TDMg3 <- MELB20_TDMg2
MELB20_TDMg3$p1inp2vec <- is.element(MELB20_TDMg3$player1, player2vector)
MELB20_TDMg3$p2inp1vec <- is.element(MELB20_TDMg3$player2, player1vector)

addPlayer1 <- MELB20_TDMg3[ which(MELB20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_TDMg3[ which(MELB20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_TDMg2 <- rbind(MELB20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
MELB20_TDMft <- ftable(MELB20_TDMg2$player1, MELB20_TDMg2$player2)
MELB20_TDMft2 <- as.matrix(MELB20_TDMft)
numRows <- nrow(MELB20_TDMft2)
numCols <- ncol(MELB20_TDMft2)
MELB20_TDMft3 <- MELB20_TDMft2[c(2:numRows) , c(2:numCols)]
MELB20_TDMTable <- graph.adjacency(MELB20_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(MELB20_TDMTable, vertex.label = V(MELB20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
MELB20_TDM.clusterCoef <- transitivity(MELB20_TDMTable, type="global") #cluster coefficient
MELB20_TDM.degreeCent <- centralization.degree(MELB20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_TDMftn <- as.network.matrix(MELB20_TDMft)
MELB20_TDM.netDensity <- network.density(MELB20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_TDM.entropy <- entropy(MELB20_TDMft) #entropy

MELB20_TDM.netMx <- cbind(MELB20_TDM.netMx, MELB20_TDM.clusterCoef, MELB20_TDM.degreeCent$centralization,
                          MELB20_TDM.netDensity, MELB20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
MELB20_SDg2 <- data.frame(MELB20_SD)
MELB20_SDg2 <- MELB20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_SDg2$player1
player2vector <- MELB20_SDg2$player2
MELB20_SDg3 <- MELB20_SDg2
MELB20_SDg3$p1inp2vec <- is.element(MELB20_SDg3$player1, player2vector)
MELB20_SDg3$p2inp1vec <- is.element(MELB20_SDg3$player2, player1vector)

addPlayer1 <- MELB20_SDg3[ which(MELB20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_SDg3[ which(MELB20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_SDg2 <- rbind(MELB20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
MELB20_SDft <- ftable(MELB20_SDg2$player1, MELB20_SDg2$player2)
MELB20_SDft2 <- as.matrix(MELB20_SDft)
numRows <- nrow(MELB20_SDft2)
numCols <- ncol(MELB20_SDft2)
MELB20_SDft3 <- MELB20_SDft2[c(2:numRows) , c(2:numCols)]
MELB20_SDTable <- graph.adjacency(MELB20_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(MELB20_SDTable, vertex.label = V(MELB20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
MELB20_SD.clusterCoef <- transitivity(MELB20_SDTable, type="global") #cluster coefficient
MELB20_SD.degreeCent <- centralization.degree(MELB20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_SDftn <- as.network.matrix(MELB20_SDft)
MELB20_SD.netDensity <- network.density(MELB20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_SD.entropy <- entropy(MELB20_SDft) #entropy

MELB20_SD.netMx <- cbind(MELB20_SD.netMx, MELB20_SD.clusterCoef, MELB20_SD.degreeCent$centralization,
                         MELB20_SD.netDensity, MELB20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
MELB20_TDg2 <- data.frame(MELB20_TD)
MELB20_TDg2 <- MELB20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_TDg2$player1
player2vector <- MELB20_TDg2$player2
MELB20_TDg3 <- MELB20_TDg2
MELB20_TDg3$p1inp2vec <- is.element(MELB20_TDg3$player1, player2vector)
MELB20_TDg3$p2inp1vec <- is.element(MELB20_TDg3$player2, player1vector)

addPlayer1 <- MELB20_TDg3[ which(MELB20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_TDg3[ which(MELB20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_TDg2 <- rbind(MELB20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
MELB20_TDft <- ftable(MELB20_TDg2$player1, MELB20_TDg2$player2)
MELB20_TDft2 <- as.matrix(MELB20_TDft)
numRows <- nrow(MELB20_TDft2)
numCols <- ncol(MELB20_TDft2)
MELB20_TDft3 <- MELB20_TDft2[c(2:numRows) , c(2:numCols)]
MELB20_TDTable <- graph.adjacency(MELB20_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(MELB20_TDTable, vertex.label = V(MELB20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
MELB20_TD.clusterCoef <- transitivity(MELB20_TDTable, type="global") #cluster coefficient
MELB20_TD.degreeCent <- centralization.degree(MELB20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_TDftn <- as.network.matrix(MELB20_TDft)
MELB20_TD.netDensity <- network.density(MELB20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_TD.entropy <- entropy(MELB20_TDft) #entropy

MELB20_TD.netMx <- cbind(MELB20_TD.netMx, MELB20_TD.clusterCoef, MELB20_TD.degreeCent$centralization,
                         MELB20_TD.netDensity, MELB20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
MELB20_QTg2 <- data.frame(MELB20_QT)
MELB20_QTg2 <- MELB20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB20_QTg2$player1
player2vector <- MELB20_QTg2$player2
MELB20_QTg3 <- MELB20_QTg2
MELB20_QTg3$p1inp2vec <- is.element(MELB20_QTg3$player1, player2vector)
MELB20_QTg3$p2inp1vec <- is.element(MELB20_QTg3$player2, player1vector)

addPlayer1 <- MELB20_QTg3[ which(MELB20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB20_QTg3[ which(MELB20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB20_QTg2 <- rbind(MELB20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
MELB20_QTft <- ftable(MELB20_QTg2$player1, MELB20_QTg2$player2)
MELB20_QTft2 <- as.matrix(MELB20_QTft)
numRows <- nrow(MELB20_QTft2)
numCols <- ncol(MELB20_QTft2)
MELB20_QTft3 <- MELB20_QTft2[c(2:numRows) , c(2:numCols)]
MELB20_QTTable <- graph.adjacency(MELB20_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(MELB20_QTTable, vertex.label = V(MELB20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
MELB20_QT.clusterCoef <- transitivity(MELB20_QTTable, type="global") #cluster coefficient
MELB20_QT.degreeCent <- centralization.degree(MELB20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB20_QTftn <- as.network.matrix(MELB20_QTft)
MELB20_QT.netDensity <- network.density(MELB20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB20_QT.entropy <- entropy(MELB20_QTft) #entropy

MELB20_QT.netMx <- cbind(MELB20_QT.netMx, MELB20_QT.clusterCoef, MELB20_QT.degreeCent$centralization,
                         MELB20_QT.netDensity, MELB20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB20_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************
#NA

round = 20
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
NMFC20_Gg2 <- data.frame(NMFC20_G)
NMFC20_Gg2 <- NMFC20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_Gg2$player1
player2vector <- NMFC20_Gg2$player2
NMFC20_Gg3 <- NMFC20_Gg2
NMFC20_Gg3$p1inp2vec <- is.element(NMFC20_Gg3$player1, player2vector)
NMFC20_Gg3$p2inp1vec <- is.element(NMFC20_Gg3$player2, player1vector)

addPlayer1 <- NMFC20_Gg3[ which(NMFC20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_Gg3[ which(NMFC20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_Gg2 <- rbind(NMFC20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
NMFC20_Gft <- ftable(NMFC20_Gg2$player1, NMFC20_Gg2$player2)
NMFC20_Gft2 <- as.matrix(NMFC20_Gft)
numRows <- nrow(NMFC20_Gft2)
numCols <- ncol(NMFC20_Gft2)
NMFC20_Gft3 <- NMFC20_Gft2[c(2:numRows) , c(2:numCols)]
NMFC20_GTable <- graph.adjacency(NMFC20_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(NMFC20_GTable, vertex.label = V(NMFC20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
NMFC20_G.clusterCoef <- transitivity(NMFC20_GTable, type="global") #cluster coefficient
NMFC20_G.degreeCent <- centralization.degree(NMFC20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_Gftn <- as.network.matrix(NMFC20_Gft)
NMFC20_G.netDensity <- network.density(NMFC20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_G.entropy <- entropy(NMFC20_Gft) #entropy

NMFC20_G.netMx <- cbind(NMFC20_G.netMx, NMFC20_G.clusterCoef, NMFC20_G.degreeCent$centralization,
                        NMFC20_G.netDensity, NMFC20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************

round = 20
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
NMFC20_Bg2 <- data.frame(NMFC20_B)
NMFC20_Bg2 <- NMFC20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_Bg2$player1
player2vector <- NMFC20_Bg2$player2
NMFC20_Bg3 <- NMFC20_Bg2
NMFC20_Bg3$p1inp2vec <- is.element(NMFC20_Bg3$player1, player2vector)
NMFC20_Bg3$p2inp1vec <- is.element(NMFC20_Bg3$player2, player1vector)

addPlayer1 <- NMFC20_Bg3[ which(NMFC20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_Bg3[ which(NMFC20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_Bg2 <- rbind(NMFC20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
NMFC20_Bft <- ftable(NMFC20_Bg2$player1, NMFC20_Bg2$player2)
NMFC20_Bft2 <- as.matrix(NMFC20_Bft)
numRows <- nrow(NMFC20_Bft2)
numCols <- ncol(NMFC20_Bft2)
NMFC20_Bft3 <- NMFC20_Bft2[c(2:numRows) , c(2:numCols)]
NMFC20_BTable <- graph.adjacency(NMFC20_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(NMFC20_BTable, vertex.label = V(NMFC20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
NMFC20_B.clusterCoef <- transitivity(NMFC20_BTable, type="global") #cluster coefficient
NMFC20_B.degreeCent <- centralization.degree(NMFC20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_Bftn <- as.network.matrix(NMFC20_Bft)
NMFC20_B.netDensity <- network.density(NMFC20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_B.entropy <- entropy(NMFC20_Bft) #entropy

NMFC20_B.netMx <- cbind(NMFC20_B.netMx, NMFC20_B.clusterCoef, NMFC20_B.degreeCent$centralization,
                        NMFC20_B.netDensity, NMFC20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
NMFC20_SFg2 <- data.frame(NMFC20_SF)
NMFC20_SFg2 <- NMFC20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_SFg2$player1
player2vector <- NMFC20_SFg2$player2
NMFC20_SFg3 <- NMFC20_SFg2
NMFC20_SFg3$p1inp2vec <- is.element(NMFC20_SFg3$player1, player2vector)
NMFC20_SFg3$p2inp1vec <- is.element(NMFC20_SFg3$player2, player1vector)

addPlayer1 <- NMFC20_SFg3[ which(NMFC20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_SFg3[ which(NMFC20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_SFg2 <- rbind(NMFC20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
NMFC20_SFft <- ftable(NMFC20_SFg2$player1, NMFC20_SFg2$player2)
NMFC20_SFft2 <- as.matrix(NMFC20_SFft)
numRows <- nrow(NMFC20_SFft2)
numCols <- ncol(NMFC20_SFft2)
NMFC20_SFft3 <- NMFC20_SFft2[c(2:numRows) , c(2:numCols)]
NMFC20_SFTable <- graph.adjacency(NMFC20_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(NMFC20_SFTable, vertex.label = V(NMFC20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
NMFC20_SF.clusterCoef <- transitivity(NMFC20_SFTable, type="global") #cluster coefficient
NMFC20_SF.degreeCent <- centralization.degree(NMFC20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_SFftn <- as.network.matrix(NMFC20_SFft)
NMFC20_SF.netDensity <- network.density(NMFC20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_SF.entropy <- entropy(NMFC20_SFft) #entropy

NMFC20_SF.netMx <- cbind(NMFC20_SF.netMx, NMFC20_SF.clusterCoef, NMFC20_SF.degreeCent$centralization,
                         NMFC20_SF.netDensity, NMFC20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************
#NA

round = 20
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
NMFC20_TFg2 <- data.frame(NMFC20_TF)
NMFC20_TFg2 <- NMFC20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_TFg2$player1
player2vector <- NMFC20_TFg2$player2
NMFC20_TFg3 <- NMFC20_TFg2
NMFC20_TFg3$p1inp2vec <- is.element(NMFC20_TFg3$player1, player2vector)
NMFC20_TFg3$p2inp1vec <- is.element(NMFC20_TFg3$player2, player1vector)

addPlayer1 <- NMFC20_TFg3[ which(NMFC20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_TFg3[ which(NMFC20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_TFg2 <- rbind(NMFC20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
NMFC20_TFft <- ftable(NMFC20_TFg2$player1, NMFC20_TFg2$player2)
NMFC20_TFft2 <- as.matrix(NMFC20_TFft)
numRows <- nrow(NMFC20_TFft2)
numCols <- ncol(NMFC20_TFft2)
NMFC20_TFft3 <- NMFC20_TFft2[c(2:numRows) , c(2:numCols)]
NMFC20_TFTable <- graph.adjacency(NMFC20_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(NMFC20_TFTable, vertex.label = V(NMFC20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
NMFC20_TF.clusterCoef <- transitivity(NMFC20_TFTable, type="global") #cluster coefficient
NMFC20_TF.degreeCent <- centralization.degree(NMFC20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_TFftn <- as.network.matrix(NMFC20_TFft)
NMFC20_TF.netDensity <- network.density(NMFC20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_TF.entropy <- entropy(NMFC20_TFft) #entropy

NMFC20_TF.netMx <- cbind(NMFC20_TF.netMx, NMFC20_TF.clusterCoef, NMFC20_TF.degreeCent$centralization,
                         NMFC20_TF.netDensity, NMFC20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************

round = 20
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
NMFC20_SAMg2 <- data.frame(NMFC20_SAM)
NMFC20_SAMg2 <- NMFC20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_SAMg2$player1
player2vector <- NMFC20_SAMg2$player2
NMFC20_SAMg3 <- NMFC20_SAMg2
NMFC20_SAMg3$p1inp2vec <- is.element(NMFC20_SAMg3$player1, player2vector)
NMFC20_SAMg3$p2inp1vec <- is.element(NMFC20_SAMg3$player2, player1vector)

addPlayer1 <- NMFC20_SAMg3[ which(NMFC20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_SAMg3[ which(NMFC20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_SAMg2 <- rbind(NMFC20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
NMFC20_SAMft <- ftable(NMFC20_SAMg2$player1, NMFC20_SAMg2$player2)
NMFC20_SAMft2 <- as.matrix(NMFC20_SAMft)
numRows <- nrow(NMFC20_SAMft2)
numCols <- ncol(NMFC20_SAMft2)
NMFC20_SAMft3 <- NMFC20_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC20_SAMTable <- graph.adjacency(NMFC20_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(NMFC20_SAMTable, vertex.label = V(NMFC20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
NMFC20_SAM.clusterCoef <- transitivity(NMFC20_SAMTable, type="global") #cluster coefficient
NMFC20_SAM.degreeCent <- centralization.degree(NMFC20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_SAMftn <- as.network.matrix(NMFC20_SAMft)
NMFC20_SAM.netDensity <- network.density(NMFC20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_SAM.entropy <- entropy(NMFC20_SAMft) #entropy

NMFC20_SAM.netMx <- cbind(NMFC20_SAM.netMx, NMFC20_SAM.clusterCoef, NMFC20_SAM.degreeCent$centralization,
                          NMFC20_SAM.netDensity, NMFC20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
NMFC20_TAMg2 <- data.frame(NMFC20_TAM)
NMFC20_TAMg2 <- NMFC20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_TAMg2$player1
player2vector <- NMFC20_TAMg2$player2
NMFC20_TAMg3 <- NMFC20_TAMg2
NMFC20_TAMg3$p1inp2vec <- is.element(NMFC20_TAMg3$player1, player2vector)
NMFC20_TAMg3$p2inp1vec <- is.element(NMFC20_TAMg3$player2, player1vector)

addPlayer1 <- NMFC20_TAMg3[ which(NMFC20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_TAMg3[ which(NMFC20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_TAMg2 <- rbind(NMFC20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
NMFC20_TAMft <- ftable(NMFC20_TAMg2$player1, NMFC20_TAMg2$player2)
NMFC20_TAMft2 <- as.matrix(NMFC20_TAMft)
numRows <- nrow(NMFC20_TAMft2)
numCols <- ncol(NMFC20_TAMft2)
NMFC20_TAMft3 <- NMFC20_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC20_TAMTable <- graph.adjacency(NMFC20_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(NMFC20_TAMTable, vertex.label = V(NMFC20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
NMFC20_TAM.clusterCoef <- transitivity(NMFC20_TAMTable, type="global") #cluster coefficient
NMFC20_TAM.degreeCent <- centralization.degree(NMFC20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_TAMftn <- as.network.matrix(NMFC20_TAMft)
NMFC20_TAM.netDensity <- network.density(NMFC20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_TAM.entropy <- entropy(NMFC20_TAMft) #entropy

NMFC20_TAM.netMx <- cbind(NMFC20_TAM.netMx, NMFC20_TAM.clusterCoef, NMFC20_TAM.degreeCent$centralization,
                          NMFC20_TAM.netDensity, NMFC20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************
#NA

round = 20
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
NMFC20_SDMg2 <- data.frame(NMFC20_SDM)
NMFC20_SDMg2 <- NMFC20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_SDMg2$player1
player2vector <- NMFC20_SDMg2$player2
NMFC20_SDMg3 <- NMFC20_SDMg2
NMFC20_SDMg3$p1inp2vec <- is.element(NMFC20_SDMg3$player1, player2vector)
NMFC20_SDMg3$p2inp1vec <- is.element(NMFC20_SDMg3$player2, player1vector)

addPlayer1 <- NMFC20_SDMg3[ which(NMFC20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_SDMg3[ which(NMFC20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_SDMg2 <- rbind(NMFC20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
NMFC20_SDMft <- ftable(NMFC20_SDMg2$player1, NMFC20_SDMg2$player2)
NMFC20_SDMft2 <- as.matrix(NMFC20_SDMft)
numRows <- nrow(NMFC20_SDMft2)
numCols <- ncol(NMFC20_SDMft2)
NMFC20_SDMft3 <- NMFC20_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC20_SDMTable <- graph.adjacency(NMFC20_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(NMFC20_SDMTable, vertex.label = V(NMFC20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
NMFC20_SDM.clusterCoef <- transitivity(NMFC20_SDMTable, type="global") #cluster coefficient
NMFC20_SDM.degreeCent <- centralization.degree(NMFC20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_SDMftn <- as.network.matrix(NMFC20_SDMft)
NMFC20_SDM.netDensity <- network.density(NMFC20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_SDM.entropy <- entropy(NMFC20_SDMft) #entropy

NMFC20_SDM.netMx <- cbind(NMFC20_SDM.netMx, NMFC20_SDM.clusterCoef, NMFC20_SDM.degreeCent$centralization,
                          NMFC20_SDM.netDensity, NMFC20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
NMFC20_TDMg2 <- data.frame(NMFC20_TDM)
NMFC20_TDMg2 <- NMFC20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_TDMg2$player1
player2vector <- NMFC20_TDMg2$player2
NMFC20_TDMg3 <- NMFC20_TDMg2
NMFC20_TDMg3$p1inp2vec <- is.element(NMFC20_TDMg3$player1, player2vector)
NMFC20_TDMg3$p2inp1vec <- is.element(NMFC20_TDMg3$player2, player1vector)

addPlayer1 <- NMFC20_TDMg3[ which(NMFC20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_TDMg3[ which(NMFC20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_TDMg2 <- rbind(NMFC20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
NMFC20_TDMft <- ftable(NMFC20_TDMg2$player1, NMFC20_TDMg2$player2)
NMFC20_TDMft2 <- as.matrix(NMFC20_TDMft)
numRows <- nrow(NMFC20_TDMft2)
numCols <- ncol(NMFC20_TDMft2)
NMFC20_TDMft3 <- NMFC20_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC20_TDMTable <- graph.adjacency(NMFC20_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(NMFC20_TDMTable, vertex.label = V(NMFC20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
NMFC20_TDM.clusterCoef <- transitivity(NMFC20_TDMTable, type="global") #cluster coefficient
NMFC20_TDM.degreeCent <- centralization.degree(NMFC20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_TDMftn <- as.network.matrix(NMFC20_TDMft)
NMFC20_TDM.netDensity <- network.density(NMFC20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_TDM.entropy <- entropy(NMFC20_TDMft) #entropy

NMFC20_TDM.netMx <- cbind(NMFC20_TDM.netMx, NMFC20_TDM.clusterCoef, NMFC20_TDM.degreeCent$centralization,
                          NMFC20_TDM.netDensity, NMFC20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
NMFC20_SDg2 <- data.frame(NMFC20_SD)
NMFC20_SDg2 <- NMFC20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_SDg2$player1
player2vector <- NMFC20_SDg2$player2
NMFC20_SDg3 <- NMFC20_SDg2
NMFC20_SDg3$p1inp2vec <- is.element(NMFC20_SDg3$player1, player2vector)
NMFC20_SDg3$p2inp1vec <- is.element(NMFC20_SDg3$player2, player1vector)

addPlayer1 <- NMFC20_SDg3[ which(NMFC20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_SDg3[ which(NMFC20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_SDg2 <- rbind(NMFC20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
NMFC20_SDft <- ftable(NMFC20_SDg2$player1, NMFC20_SDg2$player2)
NMFC20_SDft2 <- as.matrix(NMFC20_SDft)
numRows <- nrow(NMFC20_SDft2)
numCols <- ncol(NMFC20_SDft2)
NMFC20_SDft3 <- NMFC20_SDft2[c(2:numRows) , c(2:numCols)]
NMFC20_SDTable <- graph.adjacency(NMFC20_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(NMFC20_SDTable, vertex.label = V(NMFC20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
NMFC20_SD.clusterCoef <- transitivity(NMFC20_SDTable, type="global") #cluster coefficient
NMFC20_SD.degreeCent <- centralization.degree(NMFC20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_SDftn <- as.network.matrix(NMFC20_SDft)
NMFC20_SD.netDensity <- network.density(NMFC20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_SD.entropy <- entropy(NMFC20_SDft) #entropy

NMFC20_SD.netMx <- cbind(NMFC20_SD.netMx, NMFC20_SD.clusterCoef, NMFC20_SD.degreeCent$centralization,
                         NMFC20_SD.netDensity, NMFC20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************

round = 20
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
NMFC20_TDg2 <- data.frame(NMFC20_TD)
NMFC20_TDg2 <- NMFC20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_TDg2$player1
player2vector <- NMFC20_TDg2$player2
NMFC20_TDg3 <- NMFC20_TDg2
NMFC20_TDg3$p1inp2vec <- is.element(NMFC20_TDg3$player1, player2vector)
NMFC20_TDg3$p2inp1vec <- is.element(NMFC20_TDg3$player2, player1vector)

addPlayer1 <- NMFC20_TDg3[ which(NMFC20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_TDg3[ which(NMFC20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_TDg2 <- rbind(NMFC20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
NMFC20_TDft <- ftable(NMFC20_TDg2$player1, NMFC20_TDg2$player2)
NMFC20_TDft2 <- as.matrix(NMFC20_TDft)
numRows <- nrow(NMFC20_TDft2)
numCols <- ncol(NMFC20_TDft2)
NMFC20_TDft3 <- NMFC20_TDft2[c(2:numRows) , c(2:numCols)]
NMFC20_TDTable <- graph.adjacency(NMFC20_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(NMFC20_TDTable, vertex.label = V(NMFC20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
NMFC20_TD.clusterCoef <- transitivity(NMFC20_TDTable, type="global") #cluster coefficient
NMFC20_TD.degreeCent <- centralization.degree(NMFC20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_TDftn <- as.network.matrix(NMFC20_TDft)
NMFC20_TD.netDensity <- network.density(NMFC20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_TD.entropy <- entropy(NMFC20_TDft) #entropy

NMFC20_TD.netMx <- cbind(NMFC20_TD.netMx, NMFC20_TD.clusterCoef, NMFC20_TD.degreeCent$centralization,
                         NMFC20_TD.netDensity, NMFC20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
NMFC20_QTg2 <- data.frame(NMFC20_QT)
NMFC20_QTg2 <- NMFC20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC20_QTg2$player1
player2vector <- NMFC20_QTg2$player2
NMFC20_QTg3 <- NMFC20_QTg2
NMFC20_QTg3$p1inp2vec <- is.element(NMFC20_QTg3$player1, player2vector)
NMFC20_QTg3$p2inp1vec <- is.element(NMFC20_QTg3$player2, player1vector)

addPlayer1 <- NMFC20_QTg3[ which(NMFC20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC20_QTg3[ which(NMFC20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC20_QTg2 <- rbind(NMFC20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
NMFC20_QTft <- ftable(NMFC20_QTg2$player1, NMFC20_QTg2$player2)
NMFC20_QTft2 <- as.matrix(NMFC20_QTft)
numRows <- nrow(NMFC20_QTft2)
numCols <- ncol(NMFC20_QTft2)
NMFC20_QTft3 <- NMFC20_QTft2[c(2:numRows) , c(2:numCols)]
NMFC20_QTTable <- graph.adjacency(NMFC20_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(NMFC20_QTTable, vertex.label = V(NMFC20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
NMFC20_QT.clusterCoef <- transitivity(NMFC20_QTTable, type="global") #cluster coefficient
NMFC20_QT.degreeCent <- centralization.degree(NMFC20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC20_QTftn <- as.network.matrix(NMFC20_QTft)
NMFC20_QT.netDensity <- network.density(NMFC20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC20_QT.entropy <- entropy(NMFC20_QTft) #entropy

NMFC20_QT.netMx <- cbind(NMFC20_QT.netMx, NMFC20_QT.clusterCoef, NMFC20_QT.degreeCent$centralization,
                         NMFC20_QT.netDensity, NMFC20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC20_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "PORT"
KIoutcome = "Goal_F"
PORT20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
PORT20_Gg2 <- data.frame(PORT20_G)
PORT20_Gg2 <- PORT20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_Gg2$player1
player2vector <- PORT20_Gg2$player2
PORT20_Gg3 <- PORT20_Gg2
PORT20_Gg3$p1inp2vec <- is.element(PORT20_Gg3$player1, player2vector)
PORT20_Gg3$p2inp1vec <- is.element(PORT20_Gg3$player2, player1vector)

addPlayer1 <- PORT20_Gg3[ which(PORT20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_Gg3[ which(PORT20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_Gg2 <- rbind(PORT20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
PORT20_Gft <- ftable(PORT20_Gg2$player1, PORT20_Gg2$player2)
PORT20_Gft2 <- as.matrix(PORT20_Gft)
numRows <- nrow(PORT20_Gft2)
numCols <- ncol(PORT20_Gft2)
PORT20_Gft3 <- PORT20_Gft2[c(2:numRows) , c(2:numCols)]
PORT20_GTable <- graph.adjacency(PORT20_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(PORT20_GTable, vertex.label = V(PORT20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
PORT20_G.clusterCoef <- transitivity(PORT20_GTable, type="global") #cluster coefficient
PORT20_G.degreeCent <- centralization.degree(PORT20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_Gftn <- as.network.matrix(PORT20_Gft)
PORT20_G.netDensity <- network.density(PORT20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_G.entropy <- entropy(PORT20_Gft) #entropy

PORT20_G.netMx <- cbind(PORT20_G.netMx, PORT20_G.clusterCoef, PORT20_G.degreeCent$centralization,
                        PORT20_G.netDensity, PORT20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "PORT"
KIoutcome = "Behind_F"
PORT20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
PORT20_Bg2 <- data.frame(PORT20_B)
PORT20_Bg2 <- PORT20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_Bg2$player1
player2vector <- PORT20_Bg2$player2
PORT20_Bg3 <- PORT20_Bg2
PORT20_Bg3$p1inp2vec <- is.element(PORT20_Bg3$player1, player2vector)
PORT20_Bg3$p2inp1vec <- is.element(PORT20_Bg3$player2, player1vector)

addPlayer1 <- PORT20_Bg3[ which(PORT20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_Bg3[ which(PORT20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_Bg2 <- rbind(PORT20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
PORT20_Bft <- ftable(PORT20_Bg2$player1, PORT20_Bg2$player2)
PORT20_Bft2 <- as.matrix(PORT20_Bft)
numRows <- nrow(PORT20_Bft2)
numCols <- ncol(PORT20_Bft2)
PORT20_Bft3 <- PORT20_Bft2[c(2:numRows) , c(2:numCols)]
PORT20_BTable <- graph.adjacency(PORT20_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(PORT20_BTable, vertex.label = V(PORT20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
PORT20_B.clusterCoef <- transitivity(PORT20_BTable, type="global") #cluster coefficient
PORT20_B.degreeCent <- centralization.degree(PORT20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_Bftn <- as.network.matrix(PORT20_Bft)
PORT20_B.netDensity <- network.density(PORT20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_B.entropy <- entropy(PORT20_Bft) #entropy

PORT20_B.netMx <- cbind(PORT20_B.netMx, PORT20_B.clusterCoef, PORT20_B.degreeCent$centralization,
                        PORT20_B.netDensity, PORT20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
PORT20_SFg2 <- data.frame(PORT20_SF)
PORT20_SFg2 <- PORT20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_SFg2$player1
player2vector <- PORT20_SFg2$player2
PORT20_SFg3 <- PORT20_SFg2
PORT20_SFg3$p1inp2vec <- is.element(PORT20_SFg3$player1, player2vector)
PORT20_SFg3$p2inp1vec <- is.element(PORT20_SFg3$player2, player1vector)

addPlayer1 <- PORT20_SFg3[ which(PORT20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_SFg3[ which(PORT20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_SFg2 <- rbind(PORT20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
PORT20_SFft <- ftable(PORT20_SFg2$player1, PORT20_SFg2$player2)
PORT20_SFft2 <- as.matrix(PORT20_SFft)
numRows <- nrow(PORT20_SFft2)
numCols <- ncol(PORT20_SFft2)
PORT20_SFft3 <- PORT20_SFft2[c(2:numRows) , c(2:numCols)]
PORT20_SFTable <- graph.adjacency(PORT20_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(PORT20_SFTable, vertex.label = V(PORT20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
PORT20_SF.clusterCoef <- transitivity(PORT20_SFTable, type="global") #cluster coefficient
PORT20_SF.degreeCent <- centralization.degree(PORT20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_SFftn <- as.network.matrix(PORT20_SFft)
PORT20_SF.netDensity <- network.density(PORT20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_SF.entropy <- entropy(PORT20_SFft) #entropy

PORT20_SF.netMx <- cbind(PORT20_SF.netMx, PORT20_SF.clusterCoef, PORT20_SF.degreeCent$centralization,
                         PORT20_SF.netDensity, PORT20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************
#NA

round = 20
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
PORT20_TFg2 <- data.frame(PORT20_TF)
PORT20_TFg2 <- PORT20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_TFg2$player1
player2vector <- PORT20_TFg2$player2
PORT20_TFg3 <- PORT20_TFg2
PORT20_TFg3$p1inp2vec <- is.element(PORT20_TFg3$player1, player2vector)
PORT20_TFg3$p2inp1vec <- is.element(PORT20_TFg3$player2, player1vector)

addPlayer1 <- PORT20_TFg3[ which(PORT20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT20_TFg2 <- rbind(PORT20_TFg2, addPlayer1)

#ROUND 20, FWD Turnover graph using weighted edges
PORT20_TFft <- ftable(PORT20_TFg2$player1, PORT20_TFg2$player2)
PORT20_TFft2 <- as.matrix(PORT20_TFft)
numRows <- nrow(PORT20_TFft2)
numCols <- ncol(PORT20_TFft2)
PORT20_TFft3 <- PORT20_TFft2[c(2:numRows) , c(1:numCols)]
PORT20_TFTable <- graph.adjacency(PORT20_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(PORT20_TFTable, vertex.label = V(PORT20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
PORT20_TF.clusterCoef <- transitivity(PORT20_TFTable, type="global") #cluster coefficient
PORT20_TF.degreeCent <- centralization.degree(PORT20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_TFftn <- as.network.matrix(PORT20_TFft)
PORT20_TF.netDensity <- network.density(PORT20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_TF.entropy <- entropy(PORT20_TFft) #entropy

PORT20_TF.netMx <- cbind(PORT20_TF.netMx, PORT20_TF.clusterCoef, PORT20_TF.degreeCent$centralization,
                         PORT20_TF.netDensity, PORT20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************

round = 20
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
PORT20_SAMg2 <- data.frame(PORT20_SAM)
PORT20_SAMg2 <- PORT20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_SAMg2$player1
player2vector <- PORT20_SAMg2$player2
PORT20_SAMg3 <- PORT20_SAMg2
PORT20_SAMg3$p1inp2vec <- is.element(PORT20_SAMg3$player1, player2vector)
PORT20_SAMg3$p2inp1vec <- is.element(PORT20_SAMg3$player2, player1vector)

addPlayer1 <- PORT20_SAMg3[ which(PORT20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_SAMg3[ which(PORT20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_SAMg2 <- rbind(PORT20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
PORT20_SAMft <- ftable(PORT20_SAMg2$player1, PORT20_SAMg2$player2)
PORT20_SAMft2 <- as.matrix(PORT20_SAMft)
numRows <- nrow(PORT20_SAMft2)
numCols <- ncol(PORT20_SAMft2)
PORT20_SAMft3 <- PORT20_SAMft2[c(2:numRows) , c(2:numCols)]
PORT20_SAMTable <- graph.adjacency(PORT20_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(PORT20_SAMTable, vertex.label = V(PORT20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
PORT20_SAM.clusterCoef <- transitivity(PORT20_SAMTable, type="global") #cluster coefficient
PORT20_SAM.degreeCent <- centralization.degree(PORT20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_SAMftn <- as.network.matrix(PORT20_SAMft)
PORT20_SAM.netDensity <- network.density(PORT20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_SAM.entropy <- entropy(PORT20_SAMft) #entropy

PORT20_SAM.netMx <- cbind(PORT20_SAM.netMx, PORT20_SAM.clusterCoef, PORT20_SAM.degreeCent$centralization,
                          PORT20_SAM.netDensity, PORT20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
PORT20_TAMg2 <- data.frame(PORT20_TAM)
PORT20_TAMg2 <- PORT20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_TAMg2$player1
player2vector <- PORT20_TAMg2$player2
PORT20_TAMg3 <- PORT20_TAMg2
PORT20_TAMg3$p1inp2vec <- is.element(PORT20_TAMg3$player1, player2vector)
PORT20_TAMg3$p2inp1vec <- is.element(PORT20_TAMg3$player2, player1vector)

addPlayer1 <- PORT20_TAMg3[ which(PORT20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_TAMg3[ which(PORT20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_TAMg2 <- rbind(PORT20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
PORT20_TAMft <- ftable(PORT20_TAMg2$player1, PORT20_TAMg2$player2)
PORT20_TAMft2 <- as.matrix(PORT20_TAMft)
numRows <- nrow(PORT20_TAMft2)
numCols <- ncol(PORT20_TAMft2)
PORT20_TAMft3 <- PORT20_TAMft2[c(2:numRows) , c(2:numCols)]
PORT20_TAMTable <- graph.adjacency(PORT20_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(PORT20_TAMTable, vertex.label = V(PORT20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
PORT20_TAM.clusterCoef <- transitivity(PORT20_TAMTable, type="global") #cluster coefficient
PORT20_TAM.degreeCent <- centralization.degree(PORT20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_TAMftn <- as.network.matrix(PORT20_TAMft)
PORT20_TAM.netDensity <- network.density(PORT20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_TAM.entropy <- entropy(PORT20_TAMft) #entropy

PORT20_TAM.netMx <- cbind(PORT20_TAM.netMx, PORT20_TAM.clusterCoef, PORT20_TAM.degreeCent$centralization,
                          PORT20_TAM.netDensity, PORT20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
PORT20_SDMg2 <- data.frame(PORT20_SDM)
PORT20_SDMg2 <- PORT20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_SDMg2$player1
player2vector <- PORT20_SDMg2$player2
PORT20_SDMg3 <- PORT20_SDMg2
PORT20_SDMg3$p1inp2vec <- is.element(PORT20_SDMg3$player1, player2vector)
PORT20_SDMg3$p2inp1vec <- is.element(PORT20_SDMg3$player2, player1vector)

addPlayer1 <- PORT20_SDMg3[ which(PORT20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_SDMg3[ which(PORT20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_SDMg2 <- rbind(PORT20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
PORT20_SDMft <- ftable(PORT20_SDMg2$player1, PORT20_SDMg2$player2)
PORT20_SDMft2 <- as.matrix(PORT20_SDMft)
numRows <- nrow(PORT20_SDMft2)
numCols <- ncol(PORT20_SDMft2)
PORT20_SDMft3 <- PORT20_SDMft2[c(2:numRows) , c(2:numCols)]
PORT20_SDMTable <- graph.adjacency(PORT20_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(PORT20_SDMTable, vertex.label = V(PORT20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
PORT20_SDM.clusterCoef <- transitivity(PORT20_SDMTable, type="global") #cluster coefficient
PORT20_SDM.degreeCent <- centralization.degree(PORT20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_SDMftn <- as.network.matrix(PORT20_SDMft)
PORT20_SDM.netDensity <- network.density(PORT20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_SDM.entropy <- entropy(PORT20_SDMft) #entropy

PORT20_SDM.netMx <- cbind(PORT20_SDM.netMx, PORT20_SDM.clusterCoef, PORT20_SDM.degreeCent$centralization,
                          PORT20_SDM.netDensity, PORT20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
PORT20_TDMg2 <- data.frame(PORT20_TDM)
PORT20_TDMg2 <- PORT20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_TDMg2$player1
player2vector <- PORT20_TDMg2$player2
PORT20_TDMg3 <- PORT20_TDMg2
PORT20_TDMg3$p1inp2vec <- is.element(PORT20_TDMg3$player1, player2vector)
PORT20_TDMg3$p2inp1vec <- is.element(PORT20_TDMg3$player2, player1vector)

addPlayer1 <- PORT20_TDMg3[ which(PORT20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_TDMg3[ which(PORT20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_TDMg2 <- rbind(PORT20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
PORT20_TDMft <- ftable(PORT20_TDMg2$player1, PORT20_TDMg2$player2)
PORT20_TDMft2 <- as.matrix(PORT20_TDMft)
numRows <- nrow(PORT20_TDMft2)
numCols <- ncol(PORT20_TDMft2)
PORT20_TDMft3 <- PORT20_TDMft2[c(2:numRows) , c(2:numCols)]
PORT20_TDMTable <- graph.adjacency(PORT20_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(PORT20_TDMTable, vertex.label = V(PORT20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
PORT20_TDM.clusterCoef <- transitivity(PORT20_TDMTable, type="global") #cluster coefficient
PORT20_TDM.degreeCent <- centralization.degree(PORT20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_TDMftn <- as.network.matrix(PORT20_TDMft)
PORT20_TDM.netDensity <- network.density(PORT20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_TDM.entropy <- entropy(PORT20_TDMft) #entropy

PORT20_TDM.netMx <- cbind(PORT20_TDM.netMx, PORT20_TDM.clusterCoef, PORT20_TDM.degreeCent$centralization,
                          PORT20_TDM.netDensity, PORT20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
PORT20_SDg2 <- data.frame(PORT20_SD)
PORT20_SDg2 <- PORT20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_SDg2$player1
player2vector <- PORT20_SDg2$player2
PORT20_SDg3 <- PORT20_SDg2
PORT20_SDg3$p1inp2vec <- is.element(PORT20_SDg3$player1, player2vector)
PORT20_SDg3$p2inp1vec <- is.element(PORT20_SDg3$player2, player1vector)

addPlayer1 <- PORT20_SDg3[ which(PORT20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_SDg3[ which(PORT20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_SDg2 <- rbind(PORT20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
PORT20_SDft <- ftable(PORT20_SDg2$player1, PORT20_SDg2$player2)
PORT20_SDft2 <- as.matrix(PORT20_SDft)
numRows <- nrow(PORT20_SDft2)
numCols <- ncol(PORT20_SDft2)
PORT20_SDft3 <- PORT20_SDft2[c(2:numRows) , c(2:numCols)]
PORT20_SDTable <- graph.adjacency(PORT20_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(PORT20_SDTable, vertex.label = V(PORT20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
PORT20_SD.clusterCoef <- transitivity(PORT20_SDTable, type="global") #cluster coefficient
PORT20_SD.degreeCent <- centralization.degree(PORT20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_SDftn <- as.network.matrix(PORT20_SDft)
PORT20_SD.netDensity <- network.density(PORT20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_SD.entropy <- entropy(PORT20_SDft) #entropy

PORT20_SD.netMx <- cbind(PORT20_SD.netMx, PORT20_SD.clusterCoef, PORT20_SD.degreeCent$centralization,
                         PORT20_SD.netDensity, PORT20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
PORT20_TDg2 <- data.frame(PORT20_TD)
PORT20_TDg2 <- PORT20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_TDg2$player1
player2vector <- PORT20_TDg2$player2
PORT20_TDg3 <- PORT20_TDg2
PORT20_TDg3$p1inp2vec <- is.element(PORT20_TDg3$player1, player2vector)
PORT20_TDg3$p2inp1vec <- is.element(PORT20_TDg3$player2, player1vector)

addPlayer1 <- PORT20_TDg3[ which(PORT20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_TDg3[ which(PORT20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_TDg2 <- rbind(PORT20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
PORT20_TDft <- ftable(PORT20_TDg2$player1, PORT20_TDg2$player2)
PORT20_TDft2 <- as.matrix(PORT20_TDft)
numRows <- nrow(PORT20_TDft2)
numCols <- ncol(PORT20_TDft2)
PORT20_TDft3 <- PORT20_TDft2[c(2:numRows) , c(2:numCols)]
PORT20_TDTable <- graph.adjacency(PORT20_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(PORT20_TDTable, vertex.label = V(PORT20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
PORT20_TD.clusterCoef <- transitivity(PORT20_TDTable, type="global") #cluster coefficient
PORT20_TD.degreeCent <- centralization.degree(PORT20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_TDftn <- as.network.matrix(PORT20_TDft)
PORT20_TD.netDensity <- network.density(PORT20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_TD.entropy <- entropy(PORT20_TDft) #entropy

PORT20_TD.netMx <- cbind(PORT20_TD.netMx, PORT20_TD.clusterCoef, PORT20_TD.degreeCent$centralization,
                         PORT20_TD.netDensity, PORT20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
PORT20_QTg2 <- data.frame(PORT20_QT)
PORT20_QTg2 <- PORT20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT20_QTg2$player1
player2vector <- PORT20_QTg2$player2
PORT20_QTg3 <- PORT20_QTg2
PORT20_QTg3$p1inp2vec <- is.element(PORT20_QTg3$player1, player2vector)
PORT20_QTg3$p2inp1vec <- is.element(PORT20_QTg3$player2, player1vector)

addPlayer1 <- PORT20_QTg3[ which(PORT20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT20_QTg3[ which(PORT20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT20_QTg2 <- rbind(PORT20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
PORT20_QTft <- ftable(PORT20_QTg2$player1, PORT20_QTg2$player2)
PORT20_QTft2 <- as.matrix(PORT20_QTft)
numRows <- nrow(PORT20_QTft2)
numCols <- ncol(PORT20_QTft2)
PORT20_QTft3 <- PORT20_QTft2[c(2:numRows) , c(2:numCols)]
PORT20_QTTable <- graph.adjacency(PORT20_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(PORT20_QTTable, vertex.label = V(PORT20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
PORT20_QT.clusterCoef <- transitivity(PORT20_QTTable, type="global") #cluster coefficient
PORT20_QT.degreeCent <- centralization.degree(PORT20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT20_QTftn <- as.network.matrix(PORT20_QTft)
PORT20_QT.netDensity <- network.density(PORT20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT20_QT.entropy <- entropy(PORT20_QTft) #entropy

PORT20_QT.netMx <- cbind(PORT20_QT.netMx, PORT20_QT.clusterCoef, PORT20_QT.degreeCent$centralization,
                         PORT20_QT.netDensity, PORT20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT20_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "RICH"
KIoutcome = "Goal_F"
RICH20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
RICH20_Gg2 <- data.frame(RICH20_G)
RICH20_Gg2 <- RICH20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_Gg2$player1
player2vector <- RICH20_Gg2$player2
RICH20_Gg3 <- RICH20_Gg2
RICH20_Gg3$p1inp2vec <- is.element(RICH20_Gg3$player1, player2vector)
RICH20_Gg3$p2inp1vec <- is.element(RICH20_Gg3$player2, player1vector)

addPlayer1 <- RICH20_Gg3[ which(RICH20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_Gg3[ which(RICH20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_Gg2 <- rbind(RICH20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
RICH20_Gft <- ftable(RICH20_Gg2$player1, RICH20_Gg2$player2)
RICH20_Gft2 <- as.matrix(RICH20_Gft)
numRows <- nrow(RICH20_Gft2)
numCols <- ncol(RICH20_Gft2)
RICH20_Gft3 <- RICH20_Gft2[c(2:numRows) , c(2:numCols)]
RICH20_GTable <- graph.adjacency(RICH20_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(RICH20_GTable, vertex.label = V(RICH20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
RICH20_G.clusterCoef <- transitivity(RICH20_GTable, type="global") #cluster coefficient
RICH20_G.degreeCent <- centralization.degree(RICH20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_Gftn <- as.network.matrix(RICH20_Gft)
RICH20_G.netDensity <- network.density(RICH20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_G.entropy <- entropy(RICH20_Gft) #entropy

RICH20_G.netMx <- cbind(RICH20_G.netMx, RICH20_G.clusterCoef, RICH20_G.degreeCent$centralization,
                        RICH20_G.netDensity, RICH20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "RICH"
KIoutcome = "Behind_F"
RICH20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
RICH20_Bg2 <- data.frame(RICH20_B)
RICH20_Bg2 <- RICH20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_Bg2$player1
player2vector <- RICH20_Bg2$player2
RICH20_Bg3 <- RICH20_Bg2
RICH20_Bg3$p1inp2vec <- is.element(RICH20_Bg3$player1, player2vector)
RICH20_Bg3$p2inp1vec <- is.element(RICH20_Bg3$player2, player1vector)

addPlayer1 <- RICH20_Bg3[ which(RICH20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_Bg3[ which(RICH20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_Bg2 <- rbind(RICH20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
RICH20_Bft <- ftable(RICH20_Bg2$player1, RICH20_Bg2$player2)
RICH20_Bft2 <- as.matrix(RICH20_Bft)
numRows <- nrow(RICH20_Bft2)
numCols <- ncol(RICH20_Bft2)
RICH20_Bft3 <- RICH20_Bft2[c(2:numRows) , c(2:numCols)]
RICH20_BTable <- graph.adjacency(RICH20_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(RICH20_BTable, vertex.label = V(RICH20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
RICH20_B.clusterCoef <- transitivity(RICH20_BTable, type="global") #cluster coefficient
RICH20_B.degreeCent <- centralization.degree(RICH20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_Bftn <- as.network.matrix(RICH20_Bft)
RICH20_B.netDensity <- network.density(RICH20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_B.entropy <- entropy(RICH20_Bft) #entropy

RICH20_B.netMx <- cbind(RICH20_B.netMx, RICH20_B.clusterCoef, RICH20_B.degreeCent$centralization,
                        RICH20_B.netDensity, RICH20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
RICH20_SFg2 <- data.frame(RICH20_SF)
RICH20_SFg2 <- RICH20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_SFg2$player1
player2vector <- RICH20_SFg2$player2
RICH20_SFg3 <- RICH20_SFg2
RICH20_SFg3$p1inp2vec <- is.element(RICH20_SFg3$player1, player2vector)
RICH20_SFg3$p2inp1vec <- is.element(RICH20_SFg3$player2, player1vector)

addPlayer1 <- RICH20_SFg3[ which(RICH20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_SFg3[ which(RICH20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_SFg2 <- rbind(RICH20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
RICH20_SFft <- ftable(RICH20_SFg2$player1, RICH20_SFg2$player2)
RICH20_SFft2 <- as.matrix(RICH20_SFft)
numRows <- nrow(RICH20_SFft2)
numCols <- ncol(RICH20_SFft2)
RICH20_SFft3 <- RICH20_SFft2[c(2:numRows) , c(2:numCols)]
RICH20_SFTable <- graph.adjacency(RICH20_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(RICH20_SFTable, vertex.label = V(RICH20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
RICH20_SF.clusterCoef <- transitivity(RICH20_SFTable, type="global") #cluster coefficient
RICH20_SF.degreeCent <- centralization.degree(RICH20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_SFftn <- as.network.matrix(RICH20_SFft)
RICH20_SF.netDensity <- network.density(RICH20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_SF.entropy <- entropy(RICH20_SFft) #entropy

RICH20_SF.netMx <- cbind(RICH20_SF.netMx, RICH20_SF.clusterCoef, RICH20_SF.degreeCent$centralization,
                         RICH20_SF.netDensity, RICH20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************
#NA

round = 20
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
RICH20_TFg2 <- data.frame(RICH20_TF)
RICH20_TFg2 <- RICH20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_TFg2$player1
player2vector <- RICH20_TFg2$player2
RICH20_TFg3 <- RICH20_TFg2
RICH20_TFg3$p1inp2vec <- is.element(RICH20_TFg3$player1, player2vector)
RICH20_TFg3$p2inp1vec <- is.element(RICH20_TFg3$player2, player1vector)

addPlayer1 <- RICH20_TFg3[ which(RICH20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_TFg3[ which(RICH20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_TFg2 <- rbind(RICH20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
RICH20_TFft <- ftable(RICH20_TFg2$player1, RICH20_TFg2$player2)
RICH20_TFft2 <- as.matrix(RICH20_TFft)
numRows <- nrow(RICH20_TFft2)
numCols <- ncol(RICH20_TFft2)
RICH20_TFft3 <- RICH20_TFft2[c(2:numRows) , c(2:numCols)]
RICH20_TFTable <- graph.adjacency(RICH20_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(RICH20_TFTable, vertex.label = V(RICH20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
RICH20_TF.clusterCoef <- transitivity(RICH20_TFTable, type="global") #cluster coefficient
RICH20_TF.degreeCent <- centralization.degree(RICH20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_TFftn <- as.network.matrix(RICH20_TFft)
RICH20_TF.netDensity <- network.density(RICH20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_TF.entropy <- entropy(RICH20_TFft) #entropy

RICH20_TF.netMx <- cbind(RICH20_TF.netMx, RICH20_TF.clusterCoef, RICH20_TF.degreeCent$centralization,
                         RICH20_TF.netDensity, RICH20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************
#NA

round = 20
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
RICH20_SAMg2 <- data.frame(RICH20_SAM)
RICH20_SAMg2 <- RICH20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_SAMg2$player1
player2vector <- RICH20_SAMg2$player2
RICH20_SAMg3 <- RICH20_SAMg2
RICH20_SAMg3$p1inp2vec <- is.element(RICH20_SAMg3$player1, player2vector)
RICH20_SAMg3$p2inp1vec <- is.element(RICH20_SAMg3$player2, player1vector)

addPlayer1 <- RICH20_SAMg3[ which(RICH20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_SAMg3[ which(RICH20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_SAMg2 <- rbind(RICH20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
RICH20_SAMft <- ftable(RICH20_SAMg2$player1, RICH20_SAMg2$player2)
RICH20_SAMft2 <- as.matrix(RICH20_SAMft)
numRows <- nrow(RICH20_SAMft2)
numCols <- ncol(RICH20_SAMft2)
RICH20_SAMft3 <- RICH20_SAMft2[c(2:numRows) , c(2:numCols)]
RICH20_SAMTable <- graph.adjacency(RICH20_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(RICH20_SAMTable, vertex.label = V(RICH20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
RICH20_SAM.clusterCoef <- transitivity(RICH20_SAMTable, type="global") #cluster coefficient
RICH20_SAM.degreeCent <- centralization.degree(RICH20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_SAMftn <- as.network.matrix(RICH20_SAMft)
RICH20_SAM.netDensity <- network.density(RICH20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_SAM.entropy <- entropy(RICH20_SAMft) #entropy

RICH20_SAM.netMx <- cbind(RICH20_SAM.netMx, RICH20_SAM.clusterCoef, RICH20_SAM.degreeCent$centralization,
                          RICH20_SAM.netDensity, RICH20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************
#NA

round = 20
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
RICH20_TAMg2 <- data.frame(RICH20_TAM)
RICH20_TAMg2 <- RICH20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_TAMg2$player1
player2vector <- RICH20_TAMg2$player2
RICH20_TAMg3 <- RICH20_TAMg2
RICH20_TAMg3$p1inp2vec <- is.element(RICH20_TAMg3$player1, player2vector)
RICH20_TAMg3$p2inp1vec <- is.element(RICH20_TAMg3$player2, player1vector)

addPlayer1 <- RICH20_TAMg3[ which(RICH20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_TAMg3[ which(RICH20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_TAMg2 <- rbind(RICH20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
RICH20_TAMft <- ftable(RICH20_TAMg2$player1, RICH20_TAMg2$player2)
RICH20_TAMft2 <- as.matrix(RICH20_TAMft)
numRows <- nrow(RICH20_TAMft2)
numCols <- ncol(RICH20_TAMft2)
RICH20_TAMft3 <- RICH20_TAMft2[c(2:numRows) , c(2:numCols)]
RICH20_TAMTable <- graph.adjacency(RICH20_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(RICH20_TAMTable, vertex.label = V(RICH20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
RICH20_TAM.clusterCoef <- transitivity(RICH20_TAMTable, type="global") #cluster coefficient
RICH20_TAM.degreeCent <- centralization.degree(RICH20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_TAMftn <- as.network.matrix(RICH20_TAMft)
RICH20_TAM.netDensity <- network.density(RICH20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_TAM.entropy <- entropy(RICH20_TAMft) #entropy

RICH20_TAM.netMx <- cbind(RICH20_TAM.netMx, RICH20_TAM.clusterCoef, RICH20_TAM.degreeCent$centralization,
                          RICH20_TAM.netDensity, RICH20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
RICH20_SDMg2 <- data.frame(RICH20_SDM)
RICH20_SDMg2 <- RICH20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_SDMg2$player1
player2vector <- RICH20_SDMg2$player2
RICH20_SDMg3 <- RICH20_SDMg2
RICH20_SDMg3$p1inp2vec <- is.element(RICH20_SDMg3$player1, player2vector)
RICH20_SDMg3$p2inp1vec <- is.element(RICH20_SDMg3$player2, player1vector)

addPlayer1 <- RICH20_SDMg3[ which(RICH20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_SDMg3[ which(RICH20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_SDMg2 <- rbind(RICH20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
RICH20_SDMft <- ftable(RICH20_SDMg2$player1, RICH20_SDMg2$player2)
RICH20_SDMft2 <- as.matrix(RICH20_SDMft)
numRows <- nrow(RICH20_SDMft2)
numCols <- ncol(RICH20_SDMft2)
RICH20_SDMft3 <- RICH20_SDMft2[c(2:numRows) , c(2:numCols)]
RICH20_SDMTable <- graph.adjacency(RICH20_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(RICH20_SDMTable, vertex.label = V(RICH20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
RICH20_SDM.clusterCoef <- transitivity(RICH20_SDMTable, type="global") #cluster coefficient
RICH20_SDM.degreeCent <- centralization.degree(RICH20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_SDMftn <- as.network.matrix(RICH20_SDMft)
RICH20_SDM.netDensity <- network.density(RICH20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_SDM.entropy <- entropy(RICH20_SDMft) #entropy

RICH20_SDM.netMx <- cbind(RICH20_SDM.netMx, RICH20_SDM.clusterCoef, RICH20_SDM.degreeCent$centralization,
                          RICH20_SDM.netDensity, RICH20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
RICH20_TDMg2 <- data.frame(RICH20_TDM)
RICH20_TDMg2 <- RICH20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_TDMg2$player1
player2vector <- RICH20_TDMg2$player2
RICH20_TDMg3 <- RICH20_TDMg2
RICH20_TDMg3$p1inp2vec <- is.element(RICH20_TDMg3$player1, player2vector)
RICH20_TDMg3$p2inp1vec <- is.element(RICH20_TDMg3$player2, player1vector)

addPlayer1 <- RICH20_TDMg3[ which(RICH20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_TDMg3[ which(RICH20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_TDMg2 <- rbind(RICH20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
RICH20_TDMft <- ftable(RICH20_TDMg2$player1, RICH20_TDMg2$player2)
RICH20_TDMft2 <- as.matrix(RICH20_TDMft)
numRows <- nrow(RICH20_TDMft2)
numCols <- ncol(RICH20_TDMft2)
RICH20_TDMft3 <- RICH20_TDMft2[c(2:numRows) , c(2:numCols)]
RICH20_TDMTable <- graph.adjacency(RICH20_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(RICH20_TDMTable, vertex.label = V(RICH20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
RICH20_TDM.clusterCoef <- transitivity(RICH20_TDMTable, type="global") #cluster coefficient
RICH20_TDM.degreeCent <- centralization.degree(RICH20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_TDMftn <- as.network.matrix(RICH20_TDMft)
RICH20_TDM.netDensity <- network.density(RICH20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_TDM.entropy <- entropy(RICH20_TDMft) #entropy

RICH20_TDM.netMx <- cbind(RICH20_TDM.netMx, RICH20_TDM.clusterCoef, RICH20_TDM.degreeCent$centralization,
                          RICH20_TDM.netDensity, RICH20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
RICH20_SDg2 <- data.frame(RICH20_SD)
RICH20_SDg2 <- RICH20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_SDg2$player1
player2vector <- RICH20_SDg2$player2
RICH20_SDg3 <- RICH20_SDg2
RICH20_SDg3$p1inp2vec <- is.element(RICH20_SDg3$player1, player2vector)
RICH20_SDg3$p2inp1vec <- is.element(RICH20_SDg3$player2, player1vector)

addPlayer1 <- RICH20_SDg3[ which(RICH20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_SDg3[ which(RICH20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_SDg2 <- rbind(RICH20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
RICH20_SDft <- ftable(RICH20_SDg2$player1, RICH20_SDg2$player2)
RICH20_SDft2 <- as.matrix(RICH20_SDft)
numRows <- nrow(RICH20_SDft2)
numCols <- ncol(RICH20_SDft2)
RICH20_SDft3 <- RICH20_SDft2[c(2:numRows) , c(2:numCols)]
RICH20_SDTable <- graph.adjacency(RICH20_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(RICH20_SDTable, vertex.label = V(RICH20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
RICH20_SD.clusterCoef <- transitivity(RICH20_SDTable, type="global") #cluster coefficient
RICH20_SD.degreeCent <- centralization.degree(RICH20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_SDftn <- as.network.matrix(RICH20_SDft)
RICH20_SD.netDensity <- network.density(RICH20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_SD.entropy <- entropy(RICH20_SDft) #entropy

RICH20_SD.netMx <- cbind(RICH20_SD.netMx, RICH20_SD.clusterCoef, RICH20_SD.degreeCent$centralization,
                         RICH20_SD.netDensity, RICH20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
RICH20_TDg2 <- data.frame(RICH20_TD)
RICH20_TDg2 <- RICH20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_TDg2$player1
player2vector <- RICH20_TDg2$player2
RICH20_TDg3 <- RICH20_TDg2
RICH20_TDg3$p1inp2vec <- is.element(RICH20_TDg3$player1, player2vector)
RICH20_TDg3$p2inp1vec <- is.element(RICH20_TDg3$player2, player1vector)

addPlayer1 <- RICH20_TDg3[ which(RICH20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_TDg3[ which(RICH20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_TDg2 <- rbind(RICH20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
RICH20_TDft <- ftable(RICH20_TDg2$player1, RICH20_TDg2$player2)
RICH20_TDft2 <- as.matrix(RICH20_TDft)
numRows <- nrow(RICH20_TDft2)
numCols <- ncol(RICH20_TDft2)
RICH20_TDft3 <- RICH20_TDft2[c(2:numRows) , c(2:numCols)]
RICH20_TDTable <- graph.adjacency(RICH20_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(RICH20_TDTable, vertex.label = V(RICH20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
RICH20_TD.clusterCoef <- transitivity(RICH20_TDTable, type="global") #cluster coefficient
RICH20_TD.degreeCent <- centralization.degree(RICH20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_TDftn <- as.network.matrix(RICH20_TDft)
RICH20_TD.netDensity <- network.density(RICH20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_TD.entropy <- entropy(RICH20_TDft) #entropy

RICH20_TD.netMx <- cbind(RICH20_TD.netMx, RICH20_TD.clusterCoef, RICH20_TD.degreeCent$centralization,
                         RICH20_TD.netDensity, RICH20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
RICH20_QTg2 <- data.frame(RICH20_QT)
RICH20_QTg2 <- RICH20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH20_QTg2$player1
player2vector <- RICH20_QTg2$player2
RICH20_QTg3 <- RICH20_QTg2
RICH20_QTg3$p1inp2vec <- is.element(RICH20_QTg3$player1, player2vector)
RICH20_QTg3$p2inp1vec <- is.element(RICH20_QTg3$player2, player1vector)

addPlayer1 <- RICH20_QTg3[ which(RICH20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH20_QTg3[ which(RICH20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH20_QTg2 <- rbind(RICH20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
RICH20_QTft <- ftable(RICH20_QTg2$player1, RICH20_QTg2$player2)
RICH20_QTft2 <- as.matrix(RICH20_QTft)
numRows <- nrow(RICH20_QTft2)
numCols <- ncol(RICH20_QTft2)
RICH20_QTft3 <- RICH20_QTft2[c(2:numRows) , c(2:numCols)]
RICH20_QTTable <- graph.adjacency(RICH20_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(RICH20_QTTable, vertex.label = V(RICH20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
RICH20_QT.clusterCoef <- transitivity(RICH20_QTTable, type="global") #cluster coefficient
RICH20_QT.degreeCent <- centralization.degree(RICH20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH20_QTftn <- as.network.matrix(RICH20_QTft)
RICH20_QT.netDensity <- network.density(RICH20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH20_QT.entropy <- entropy(RICH20_QTft) #entropy

RICH20_QT.netMx <- cbind(RICH20_QT.netMx, RICH20_QT.clusterCoef, RICH20_QT.degreeCent$centralization,
                         RICH20_QT.netDensity, RICH20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH20_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************
#NA

round = 20
teamName = "STK"
KIoutcome = "Goal_F"
STK20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
STK20_Gg2 <- data.frame(STK20_G)
STK20_Gg2 <- STK20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_Gg2$player1
player2vector <- STK20_Gg2$player2
STK20_Gg3 <- STK20_Gg2
STK20_Gg3$p1inp2vec <- is.element(STK20_Gg3$player1, player2vector)
STK20_Gg3$p2inp1vec <- is.element(STK20_Gg3$player2, player1vector)

addPlayer1 <- STK20_Gg3[ which(STK20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK20_Gg3[ which(STK20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_Gg2 <- rbind(STK20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
STK20_Gft <- ftable(STK20_Gg2$player1, STK20_Gg2$player2)
STK20_Gft2 <- as.matrix(STK20_Gft)
numRows <- nrow(STK20_Gft2)
numCols <- ncol(STK20_Gft2)
STK20_Gft3 <- STK20_Gft2[c(2:numRows) , c(2:numCols)]
STK20_GTable <- graph.adjacency(STK20_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(STK20_GTable, vertex.label = V(STK20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
STK20_G.clusterCoef <- transitivity(STK20_GTable, type="global") #cluster coefficient
STK20_G.degreeCent <- centralization.degree(STK20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_Gftn <- as.network.matrix(STK20_Gft)
STK20_G.netDensity <- network.density(STK20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_G.entropy <- entropy(STK20_Gft) #entropy

STK20_G.netMx <- cbind(STK20_G.netMx, STK20_G.clusterCoef, STK20_G.degreeCent$centralization,
                       STK20_G.netDensity, STK20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "STK"
KIoutcome = "Behind_F"
STK20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
STK20_Bg2 <- data.frame(STK20_B)
STK20_Bg2 <- STK20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_Bg2$player1
player2vector <- STK20_Bg2$player2
STK20_Bg3 <- STK20_Bg2
STK20_Bg3$p1inp2vec <- is.element(STK20_Bg3$player1, player2vector)
STK20_Bg3$p2inp1vec <- is.element(STK20_Bg3$player2, player1vector)

addPlayer1 <- STK20_Bg3[ which(STK20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK20_Bg3[ which(STK20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_Bg2 <- rbind(STK20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
STK20_Bft <- ftable(STK20_Bg2$player1, STK20_Bg2$player2)
STK20_Bft2 <- as.matrix(STK20_Bft)
numRows <- nrow(STK20_Bft2)
numCols <- ncol(STK20_Bft2)
STK20_Bft3 <- STK20_Bft2[c(2:numRows) , c(2:numCols)]
STK20_BTable <- graph.adjacency(STK20_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(STK20_BTable, vertex.label = V(STK20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
STK20_B.clusterCoef <- transitivity(STK20_BTable, type="global") #cluster coefficient
STK20_B.degreeCent <- centralization.degree(STK20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_Bftn <- as.network.matrix(STK20_Bft)
STK20_B.netDensity <- network.density(STK20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_B.entropy <- entropy(STK20_Bft) #entropy

STK20_B.netMx <- cbind(STK20_B.netMx, STK20_B.clusterCoef, STK20_B.degreeCent$centralization,
                       STK20_B.netDensity, STK20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************

round = 20
teamName = "STK"
KIoutcome = "Stoppage_F"
STK20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
STK20_SFg2 <- data.frame(STK20_SF)
STK20_SFg2 <- STK20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_SFg2$player1
player2vector <- STK20_SFg2$player2
STK20_SFg3 <- STK20_SFg2
STK20_SFg3$p1inp2vec <- is.element(STK20_SFg3$player1, player2vector)
STK20_SFg3$p2inp1vec <- is.element(STK20_SFg3$player2, player1vector)

addPlayer1 <- STK20_SFg3[ which(STK20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK20_SFg3[ which(STK20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_SFg2 <- rbind(STK20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
STK20_SFft <- ftable(STK20_SFg2$player1, STK20_SFg2$player2)
STK20_SFft2 <- as.matrix(STK20_SFft)
numRows <- nrow(STK20_SFft2)
numCols <- ncol(STK20_SFft2)
STK20_SFft3 <- STK20_SFft2[c(2:numRows) , c(2:numCols)]
STK20_SFTable <- graph.adjacency(STK20_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(STK20_SFTable, vertex.label = V(STK20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
STK20_SF.clusterCoef <- transitivity(STK20_SFTable, type="global") #cluster coefficient
STK20_SF.degreeCent <- centralization.degree(STK20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_SFftn <- as.network.matrix(STK20_SFft)
STK20_SF.netDensity <- network.density(STK20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_SF.entropy <- entropy(STK20_SFft) #entropy

STK20_SF.netMx <- cbind(STK20_SF.netMx, STK20_SF.clusterCoef, STK20_SF.degreeCent$centralization,
                        STK20_SF.netDensity, STK20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "STK"
KIoutcome = "Turnover_F"
STK20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
STK20_TFg2 <- data.frame(STK20_TF)
STK20_TFg2 <- STK20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_TFg2$player1
player2vector <- STK20_TFg2$player2
STK20_TFg3 <- STK20_TFg2
STK20_TFg3$p1inp2vec <- is.element(STK20_TFg3$player1, player2vector)
STK20_TFg3$p2inp1vec <- is.element(STK20_TFg3$player2, player1vector)

addPlayer1 <- STK20_TFg3[ which(STK20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK20_TFg3[ which(STK20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_TFg2 <- rbind(STK20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
STK20_TFft <- ftable(STK20_TFg2$player1, STK20_TFg2$player2)
STK20_TFft2 <- as.matrix(STK20_TFft)
numRows <- nrow(STK20_TFft2)
numCols <- ncol(STK20_TFft2)
STK20_TFft3 <- STK20_TFft2[c(2:numRows) , c(2:numCols)]
STK20_TFTable <- graph.adjacency(STK20_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(STK20_TFTable, vertex.label = V(STK20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
STK20_TF.clusterCoef <- transitivity(STK20_TFTable, type="global") #cluster coefficient
STK20_TF.degreeCent <- centralization.degree(STK20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_TFftn <- as.network.matrix(STK20_TFft)
STK20_TF.netDensity <- network.density(STK20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_TF.entropy <- entropy(STK20_TFft) #entropy

STK20_TF.netMx <- cbind(STK20_TF.netMx, STK20_TF.clusterCoef, STK20_TF.degreeCent$centralization,
                        STK20_TF.netDensity, STK20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************
#NA

round = 20
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
STK20_SAMg2 <- data.frame(STK20_SAM)
STK20_SAMg2 <- STK20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_SAMg2$player1
player2vector <- STK20_SAMg2$player2
STK20_SAMg3 <- STK20_SAMg2
STK20_SAMg3$p1inp2vec <- is.element(STK20_SAMg3$player1, player2vector)
STK20_SAMg3$p2inp1vec <- is.element(STK20_SAMg3$player2, player1vector)

addPlayer1 <- STK20_SAMg3[ which(STK20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK20_SAMg3[ which(STK20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_SAMg2 <- rbind(STK20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
STK20_SAMft <- ftable(STK20_SAMg2$player1, STK20_SAMg2$player2)
STK20_SAMft2 <- as.matrix(STK20_SAMft)
numRows <- nrow(STK20_SAMft2)
numCols <- ncol(STK20_SAMft2)
STK20_SAMft3 <- STK20_SAMft2[c(2:numRows) , c(2:numCols)]
STK20_SAMTable <- graph.adjacency(STK20_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(STK20_SAMTable, vertex.label = V(STK20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
STK20_SAM.clusterCoef <- transitivity(STK20_SAMTable, type="global") #cluster coefficient
STK20_SAM.degreeCent <- centralization.degree(STK20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_SAMftn <- as.network.matrix(STK20_SAMft)
STK20_SAM.netDensity <- network.density(STK20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_SAM.entropy <- entropy(STK20_SAMft) #entropy

STK20_SAM.netMx <- cbind(STK20_SAM.netMx, STK20_SAM.clusterCoef, STK20_SAM.degreeCent$centralization,
                         STK20_SAM.netDensity, STK20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "STK"
KIoutcome = "Turnover_AM"
STK20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
STK20_TAMg2 <- data.frame(STK20_TAM)
STK20_TAMg2 <- STK20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_TAMg2$player1
player2vector <- STK20_TAMg2$player2
STK20_TAMg3 <- STK20_TAMg2
STK20_TAMg3$p1inp2vec <- is.element(STK20_TAMg3$player1, player2vector)
STK20_TAMg3$p2inp1vec <- is.element(STK20_TAMg3$player2, player1vector)

addPlayer1 <- STK20_TAMg3[ which(STK20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK20_TAMg3[ which(STK20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_TAMg2 <- rbind(STK20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
STK20_TAMft <- ftable(STK20_TAMg2$player1, STK20_TAMg2$player2)
STK20_TAMft2 <- as.matrix(STK20_TAMft)
numRows <- nrow(STK20_TAMft2)
numCols <- ncol(STK20_TAMft2)
STK20_TAMft3 <- STK20_TAMft2[c(2:numRows) , c(2:numCols)]
STK20_TAMTable <- graph.adjacency(STK20_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(STK20_TAMTable, vertex.label = V(STK20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
STK20_TAM.clusterCoef <- transitivity(STK20_TAMTable, type="global") #cluster coefficient
STK20_TAM.degreeCent <- centralization.degree(STK20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_TAMftn <- as.network.matrix(STK20_TAMft)
STK20_TAM.netDensity <- network.density(STK20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_TAM.entropy <- entropy(STK20_TAMft) #entropy

STK20_TAM.netMx <- cbind(STK20_TAM.netMx, STK20_TAM.clusterCoef, STK20_TAM.degreeCent$centralization,
                         STK20_TAM.netDensity, STK20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
STK20_SDMg2 <- data.frame(STK20_SDM)
STK20_SDMg2 <- STK20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_SDMg2$player1
player2vector <- STK20_SDMg2$player2
STK20_SDMg3 <- STK20_SDMg2
STK20_SDMg3$p1inp2vec <- is.element(STK20_SDMg3$player1, player2vector)
STK20_SDMg3$p2inp1vec <- is.element(STK20_SDMg3$player2, player1vector)

addPlayer1 <- STK20_SDMg3[ which(STK20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK20_SDMg3[ which(STK20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_SDMg2 <- rbind(STK20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
STK20_SDMft <- ftable(STK20_SDMg2$player1, STK20_SDMg2$player2)
STK20_SDMft2 <- as.matrix(STK20_SDMft)
numRows <- nrow(STK20_SDMft2)
numCols <- ncol(STK20_SDMft2)
STK20_SDMft3 <- STK20_SDMft2[c(2:numRows) , c(2:numCols)]
STK20_SDMTable <- graph.adjacency(STK20_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(STK20_SDMTable, vertex.label = V(STK20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
STK20_SDM.clusterCoef <- transitivity(STK20_SDMTable, type="global") #cluster coefficient
STK20_SDM.degreeCent <- centralization.degree(STK20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_SDMftn <- as.network.matrix(STK20_SDMft)
STK20_SDM.netDensity <- network.density(STK20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_SDM.entropy <- entropy(STK20_SDMft) #entropy

STK20_SDM.netMx <- cbind(STK20_SDM.netMx, STK20_SDM.clusterCoef, STK20_SDM.degreeCent$centralization,
                         STK20_SDM.netDensity, STK20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "STK"
KIoutcome = "Turnover_DM"
STK20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
STK20_TDMg2 <- data.frame(STK20_TDM)
STK20_TDMg2 <- STK20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_TDMg2$player1
player2vector <- STK20_TDMg2$player2
STK20_TDMg3 <- STK20_TDMg2
STK20_TDMg3$p1inp2vec <- is.element(STK20_TDMg3$player1, player2vector)
STK20_TDMg3$p2inp1vec <- is.element(STK20_TDMg3$player2, player1vector)

addPlayer1 <- STK20_TDMg3[ which(STK20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK20_TDMg3[ which(STK20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_TDMg2 <- rbind(STK20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
STK20_TDMft <- ftable(STK20_TDMg2$player1, STK20_TDMg2$player2)
STK20_TDMft2 <- as.matrix(STK20_TDMft)
numRows <- nrow(STK20_TDMft2)
numCols <- ncol(STK20_TDMft2)
STK20_TDMft3 <- STK20_TDMft2[c(2:numRows) , c(2:numCols)]
STK20_TDMTable <- graph.adjacency(STK20_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(STK20_TDMTable, vertex.label = V(STK20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
STK20_TDM.clusterCoef <- transitivity(STK20_TDMTable, type="global") #cluster coefficient
STK20_TDM.degreeCent <- centralization.degree(STK20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_TDMftn <- as.network.matrix(STK20_TDMft)
STK20_TDM.netDensity <- network.density(STK20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_TDM.entropy <- entropy(STK20_TDMft) #entropy

STK20_TDM.netMx <- cbind(STK20_TDM.netMx, STK20_TDM.clusterCoef, STK20_TDM.degreeCent$centralization,
                         STK20_TDM.netDensity, STK20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "STK"
KIoutcome = "Stoppage_D"
STK20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
STK20_SDg2 <- data.frame(STK20_SD)
STK20_SDg2 <- STK20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_SDg2$player1
player2vector <- STK20_SDg2$player2
STK20_SDg3 <- STK20_SDg2
STK20_SDg3$p1inp2vec <- is.element(STK20_SDg3$player1, player2vector)
STK20_SDg3$p2inp1vec <- is.element(STK20_SDg3$player2, player1vector)

addPlayer1 <- STK20_SDg3[ which(STK20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK20_SDg3[ which(STK20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_SDg2 <- rbind(STK20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
STK20_SDft <- ftable(STK20_SDg2$player1, STK20_SDg2$player2)
STK20_SDft2 <- as.matrix(STK20_SDft)
numRows <- nrow(STK20_SDft2)
numCols <- ncol(STK20_SDft2)
STK20_SDft3 <- STK20_SDft2[c(2:numRows) , c(2:numCols)]
STK20_SDTable <- graph.adjacency(STK20_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(STK20_SDTable, vertex.label = V(STK20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
STK20_SD.clusterCoef <- transitivity(STK20_SDTable, type="global") #cluster coefficient
STK20_SD.degreeCent <- centralization.degree(STK20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_SDftn <- as.network.matrix(STK20_SDft)
STK20_SD.netDensity <- network.density(STK20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_SD.entropy <- entropy(STK20_SDft) #entropy

STK20_SD.netMx <- cbind(STK20_SD.netMx, STK20_SD.clusterCoef, STK20_SD.degreeCent$centralization,
                        STK20_SD.netDensity, STK20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************

round = 20
teamName = "STK"
KIoutcome = "Turnover_D"
STK20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
STK20_TDg2 <- data.frame(STK20_TD)
STK20_TDg2 <- STK20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_TDg2$player1
player2vector <- STK20_TDg2$player2
STK20_TDg3 <- STK20_TDg2
STK20_TDg3$p1inp2vec <- is.element(STK20_TDg3$player1, player2vector)
STK20_TDg3$p2inp1vec <- is.element(STK20_TDg3$player2, player1vector)

addPlayer1 <- STK20_TDg3[ which(STK20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- STK20_TDg3[ which(STK20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_TDg2 <- rbind(STK20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
STK20_TDft <- ftable(STK20_TDg2$player1, STK20_TDg2$player2)
STK20_TDft2 <- as.matrix(STK20_TDft)
numRows <- nrow(STK20_TDft2)
numCols <- ncol(STK20_TDft2)
STK20_TDft3 <- STK20_TDft2[c(2:numRows) , c(2:numCols)]
STK20_TDTable <- graph.adjacency(STK20_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(STK20_TDTable, vertex.label = V(STK20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
STK20_TD.clusterCoef <- transitivity(STK20_TDTable, type="global") #cluster coefficient
STK20_TD.degreeCent <- centralization.degree(STK20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_TDftn <- as.network.matrix(STK20_TDft)
STK20_TD.netDensity <- network.density(STK20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_TD.entropy <- entropy(STK20_TDft) #entropy

STK20_TD.netMx <- cbind(STK20_TD.netMx, STK20_TD.clusterCoef, STK20_TD.degreeCent$centralization,
                        STK20_TD.netDensity, STK20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
STK20_QTg2 <- data.frame(STK20_QT)
STK20_QTg2 <- STK20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK20_QTg2$player1
player2vector <- STK20_QTg2$player2
STK20_QTg3 <- STK20_QTg2
STK20_QTg3$p1inp2vec <- is.element(STK20_QTg3$player1, player2vector)
STK20_QTg3$p2inp1vec <- is.element(STK20_QTg3$player2, player1vector)

addPlayer1 <- STK20_QTg3[ which(STK20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK20_QTg3[ which(STK20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK20_QTg2 <- rbind(STK20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
STK20_QTft <- ftable(STK20_QTg2$player1, STK20_QTg2$player2)
STK20_QTft2 <- as.matrix(STK20_QTft)
numRows <- nrow(STK20_QTft2)
numCols <- ncol(STK20_QTft2)
STK20_QTft3 <- STK20_QTft2[c(2:numRows) , c(2:numCols)]
STK20_QTTable <- graph.adjacency(STK20_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(STK20_QTTable, vertex.label = V(STK20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
STK20_QT.clusterCoef <- transitivity(STK20_QTTable, type="global") #cluster coefficient
STK20_QT.degreeCent <- centralization.degree(STK20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK20_QTftn <- as.network.matrix(STK20_QTft)
STK20_QT.netDensity <- network.density(STK20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK20_QT.entropy <- entropy(STK20_QTft) #entropy

STK20_QT.netMx <- cbind(STK20_QT.netMx, STK20_QT.clusterCoef, STK20_QT.degreeCent$centralization,
                        STK20_QT.netDensity, STK20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK20_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "SYD"
KIoutcome = "Goal_F"
SYD20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
SYD20_Gg2 <- data.frame(SYD20_G)
SYD20_Gg2 <- SYD20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_Gg2$player1
player2vector <- SYD20_Gg2$player2
SYD20_Gg3 <- SYD20_Gg2
SYD20_Gg3$p1inp2vec <- is.element(SYD20_Gg3$player1, player2vector)
SYD20_Gg3$p2inp1vec <- is.element(SYD20_Gg3$player2, player1vector)

addPlayer1 <- SYD20_Gg3[ which(SYD20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_Gg3[ which(SYD20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_Gg2 <- rbind(SYD20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
SYD20_Gft <- ftable(SYD20_Gg2$player1, SYD20_Gg2$player2)
SYD20_Gft2 <- as.matrix(SYD20_Gft)
numRows <- nrow(SYD20_Gft2)
numCols <- ncol(SYD20_Gft2)
SYD20_Gft3 <- SYD20_Gft2[c(2:numRows) , c(2:numCols)]
SYD20_GTable <- graph.adjacency(SYD20_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(SYD20_GTable, vertex.label = V(SYD20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
SYD20_G.clusterCoef <- transitivity(SYD20_GTable, type="global") #cluster coefficient
SYD20_G.degreeCent <- centralization.degree(SYD20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_Gftn <- as.network.matrix(SYD20_Gft)
SYD20_G.netDensity <- network.density(SYD20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_G.entropy <- entropy(SYD20_Gft) #entropy

SYD20_G.netMx <- cbind(SYD20_G.netMx, SYD20_G.clusterCoef, SYD20_G.degreeCent$centralization,
                       SYD20_G.netDensity, SYD20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "SYD"
KIoutcome = "Behind_F"
SYD20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
SYD20_Bg2 <- data.frame(SYD20_B)
SYD20_Bg2 <- SYD20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_Bg2$player1
player2vector <- SYD20_Bg2$player2
SYD20_Bg3 <- SYD20_Bg2
SYD20_Bg3$p1inp2vec <- is.element(SYD20_Bg3$player1, player2vector)
SYD20_Bg3$p2inp1vec <- is.element(SYD20_Bg3$player2, player1vector)

addPlayer1 <- SYD20_Bg3[ which(SYD20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_Bg3[ which(SYD20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_Bg2 <- rbind(SYD20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
SYD20_Bft <- ftable(SYD20_Bg2$player1, SYD20_Bg2$player2)
SYD20_Bft2 <- as.matrix(SYD20_Bft)
numRows <- nrow(SYD20_Bft2)
numCols <- ncol(SYD20_Bft2)
SYD20_Bft3 <- SYD20_Bft2[c(2:numRows) , c(2:numCols)]
SYD20_BTable <- graph.adjacency(SYD20_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(SYD20_BTable, vertex.label = V(SYD20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
SYD20_B.clusterCoef <- transitivity(SYD20_BTable, type="global") #cluster coefficient
SYD20_B.degreeCent <- centralization.degree(SYD20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_Bftn <- as.network.matrix(SYD20_Bft)
SYD20_B.netDensity <- network.density(SYD20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_B.entropy <- entropy(SYD20_Bft) #entropy

SYD20_B.netMx <- cbind(SYD20_B.netMx, SYD20_B.clusterCoef, SYD20_B.degreeCent$centralization,
                       SYD20_B.netDensity, SYD20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
SYD20_SFg2 <- data.frame(SYD20_SF)
SYD20_SFg2 <- SYD20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_SFg2$player1
player2vector <- SYD20_SFg2$player2
SYD20_SFg3 <- SYD20_SFg2
SYD20_SFg3$p1inp2vec <- is.element(SYD20_SFg3$player1, player2vector)
SYD20_SFg3$p2inp1vec <- is.element(SYD20_SFg3$player2, player1vector)

addPlayer1 <- SYD20_SFg3[ which(SYD20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_SFg3[ which(SYD20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_SFg2 <- rbind(SYD20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
SYD20_SFft <- ftable(SYD20_SFg2$player1, SYD20_SFg2$player2)
SYD20_SFft2 <- as.matrix(SYD20_SFft)
numRows <- nrow(SYD20_SFft2)
numCols <- ncol(SYD20_SFft2)
SYD20_SFft3 <- SYD20_SFft2[c(2:numRows) , c(2:numCols)]
SYD20_SFTable <- graph.adjacency(SYD20_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(SYD20_SFTable, vertex.label = V(SYD20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
SYD20_SF.clusterCoef <- transitivity(SYD20_SFTable, type="global") #cluster coefficient
SYD20_SF.degreeCent <- centralization.degree(SYD20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_SFftn <- as.network.matrix(SYD20_SFft)
SYD20_SF.netDensity <- network.density(SYD20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_SF.entropy <- entropy(SYD20_SFft) #entropy

SYD20_SF.netMx <- cbind(SYD20_SF.netMx, SYD20_SF.clusterCoef, SYD20_SF.degreeCent$centralization,
                        SYD20_SF.netDensity, SYD20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
SYD20_TFg2 <- data.frame(SYD20_TF)
SYD20_TFg2 <- SYD20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_TFg2$player1
player2vector <- SYD20_TFg2$player2
SYD20_TFg3 <- SYD20_TFg2
SYD20_TFg3$p1inp2vec <- is.element(SYD20_TFg3$player1, player2vector)
SYD20_TFg3$p2inp1vec <- is.element(SYD20_TFg3$player2, player1vector)

addPlayer1 <- SYD20_TFg3[ which(SYD20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- SYD20_TFg3[ which(SYD20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_TFg2 <- rbind(SYD20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
SYD20_TFft <- ftable(SYD20_TFg2$player1, SYD20_TFg2$player2)
SYD20_TFft2 <- as.matrix(SYD20_TFft)
numRows <- nrow(SYD20_TFft2)
numCols <- ncol(SYD20_TFft2)
SYD20_TFft3 <- SYD20_TFft2[c(2:numRows) , c(2:numCols)]
SYD20_TFTable <- graph.adjacency(SYD20_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(SYD20_TFTable, vertex.label = V(SYD20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
SYD20_TF.clusterCoef <- transitivity(SYD20_TFTable, type="global") #cluster coefficient
SYD20_TF.degreeCent <- centralization.degree(SYD20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_TFftn <- as.network.matrix(SYD20_TFft)
SYD20_TF.netDensity <- network.density(SYD20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_TF.entropy <- entropy(SYD20_TFft) #entropy

SYD20_TF.netMx <- cbind(SYD20_TF.netMx, SYD20_TF.clusterCoef, SYD20_TF.degreeCent$centralization,
                        SYD20_TF.netDensity, SYD20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************
#NA

round = 20
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
SYD20_SAMg2 <- data.frame(SYD20_SAM)
SYD20_SAMg2 <- SYD20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_SAMg2$player1
player2vector <- SYD20_SAMg2$player2
SYD20_SAMg3 <- SYD20_SAMg2
SYD20_SAMg3$p1inp2vec <- is.element(SYD20_SAMg3$player1, player2vector)
SYD20_SAMg3$p2inp1vec <- is.element(SYD20_SAMg3$player2, player1vector)

addPlayer1 <- SYD20_SAMg3[ which(SYD20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_SAMg3[ which(SYD20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_SAMg2 <- rbind(SYD20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
SYD20_SAMft <- ftable(SYD20_SAMg2$player1, SYD20_SAMg2$player2)
SYD20_SAMft2 <- as.matrix(SYD20_SAMft)
numRows <- nrow(SYD20_SAMft2)
numCols <- ncol(SYD20_SAMft2)
SYD20_SAMft3 <- SYD20_SAMft2[c(2:numRows) , c(2:numCols)]
SYD20_SAMTable <- graph.adjacency(SYD20_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(SYD20_SAMTable, vertex.label = V(SYD20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
SYD20_SAM.clusterCoef <- transitivity(SYD20_SAMTable, type="global") #cluster coefficient
SYD20_SAM.degreeCent <- centralization.degree(SYD20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_SAMftn <- as.network.matrix(SYD20_SAMft)
SYD20_SAM.netDensity <- network.density(SYD20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_SAM.entropy <- entropy(SYD20_SAMft) #entropy

SYD20_SAM.netMx <- cbind(SYD20_SAM.netMx, SYD20_SAM.clusterCoef, SYD20_SAM.degreeCent$centralization,
                         SYD20_SAM.netDensity, SYD20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************
#NA

round = 20
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
SYD20_TAMg2 <- data.frame(SYD20_TAM)
SYD20_TAMg2 <- SYD20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_TAMg2$player1
player2vector <- SYD20_TAMg2$player2
SYD20_TAMg3 <- SYD20_TAMg2
SYD20_TAMg3$p1inp2vec <- is.element(SYD20_TAMg3$player1, player2vector)
SYD20_TAMg3$p2inp1vec <- is.element(SYD20_TAMg3$player2, player1vector)

addPlayer1 <- SYD20_TAMg3[ which(SYD20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_TAMg3[ which(SYD20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_TAMg2 <- rbind(SYD20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
SYD20_TAMft <- ftable(SYD20_TAMg2$player1, SYD20_TAMg2$player2)
SYD20_TAMft2 <- as.matrix(SYD20_TAMft)
numRows <- nrow(SYD20_TAMft2)
numCols <- ncol(SYD20_TAMft2)
SYD20_TAMft3 <- SYD20_TAMft2[c(2:numRows) , c(2:numCols)]
SYD20_TAMTable <- graph.adjacency(SYD20_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(SYD20_TAMTable, vertex.label = V(SYD20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
SYD20_TAM.clusterCoef <- transitivity(SYD20_TAMTable, type="global") #cluster coefficient
SYD20_TAM.degreeCent <- centralization.degree(SYD20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_TAMftn <- as.network.matrix(SYD20_TAMft)
SYD20_TAM.netDensity <- network.density(SYD20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_TAM.entropy <- entropy(SYD20_TAMft) #entropy

SYD20_TAM.netMx <- cbind(SYD20_TAM.netMx, SYD20_TAM.clusterCoef, SYD20_TAM.degreeCent$centralization,
                         SYD20_TAM.netDensity, SYD20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************

round = 20
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
SYD20_SDMg2 <- data.frame(SYD20_SDM)
SYD20_SDMg2 <- SYD20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_SDMg2$player1
player2vector <- SYD20_SDMg2$player2
SYD20_SDMg3 <- SYD20_SDMg2
SYD20_SDMg3$p1inp2vec <- is.element(SYD20_SDMg3$player1, player2vector)
SYD20_SDMg3$p2inp1vec <- is.element(SYD20_SDMg3$player2, player1vector)

addPlayer1 <- SYD20_SDMg3[ which(SYD20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_SDMg3[ which(SYD20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_SDMg2 <- rbind(SYD20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
SYD20_SDMft <- ftable(SYD20_SDMg2$player1, SYD20_SDMg2$player2)
SYD20_SDMft2 <- as.matrix(SYD20_SDMft)
numRows <- nrow(SYD20_SDMft2)
numCols <- ncol(SYD20_SDMft2)
SYD20_SDMft3 <- SYD20_SDMft2[c(2:numRows) , c(2:numCols)]
SYD20_SDMTable <- graph.adjacency(SYD20_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(SYD20_SDMTable, vertex.label = V(SYD20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
SYD20_SDM.clusterCoef <- transitivity(SYD20_SDMTable, type="global") #cluster coefficient
SYD20_SDM.degreeCent <- centralization.degree(SYD20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_SDMftn <- as.network.matrix(SYD20_SDMft)
SYD20_SDM.netDensity <- network.density(SYD20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_SDM.entropy <- entropy(SYD20_SDMft) #entropy

SYD20_SDM.netMx <- cbind(SYD20_SDM.netMx, SYD20_SDM.clusterCoef, SYD20_SDM.degreeCent$centralization,
                         SYD20_SDM.netDensity, SYD20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
SYD20_TDMg2 <- data.frame(SYD20_TDM)
SYD20_TDMg2 <- SYD20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_TDMg2$player1
player2vector <- SYD20_TDMg2$player2
SYD20_TDMg3 <- SYD20_TDMg2
SYD20_TDMg3$p1inp2vec <- is.element(SYD20_TDMg3$player1, player2vector)
SYD20_TDMg3$p2inp1vec <- is.element(SYD20_TDMg3$player2, player1vector)

addPlayer1 <- SYD20_TDMg3[ which(SYD20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_TDMg3[ which(SYD20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_TDMg2 <- rbind(SYD20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
SYD20_TDMft <- ftable(SYD20_TDMg2$player1, SYD20_TDMg2$player2)
SYD20_TDMft2 <- as.matrix(SYD20_TDMft)
numRows <- nrow(SYD20_TDMft2)
numCols <- ncol(SYD20_TDMft2)
SYD20_TDMft3 <- SYD20_TDMft2[c(2:numRows) , c(2:numCols)]
SYD20_TDMTable <- graph.adjacency(SYD20_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(SYD20_TDMTable, vertex.label = V(SYD20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
SYD20_TDM.clusterCoef <- transitivity(SYD20_TDMTable, type="global") #cluster coefficient
SYD20_TDM.degreeCent <- centralization.degree(SYD20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_TDMftn <- as.network.matrix(SYD20_TDMft)
SYD20_TDM.netDensity <- network.density(SYD20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_TDM.entropy <- entropy(SYD20_TDMft) #entropy

SYD20_TDM.netMx <- cbind(SYD20_TDM.netMx, SYD20_TDM.clusterCoef, SYD20_TDM.degreeCent$centralization,
                         SYD20_TDM.netDensity, SYD20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
SYD20_SDg2 <- data.frame(SYD20_SD)
SYD20_SDg2 <- SYD20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_SDg2$player1
player2vector <- SYD20_SDg2$player2
SYD20_SDg3 <- SYD20_SDg2
SYD20_SDg3$p1inp2vec <- is.element(SYD20_SDg3$player1, player2vector)
SYD20_SDg3$p2inp1vec <- is.element(SYD20_SDg3$player2, player1vector)

addPlayer1 <- SYD20_SDg3[ which(SYD20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_SDg3[ which(SYD20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_SDg2 <- rbind(SYD20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
SYD20_SDft <- ftable(SYD20_SDg2$player1, SYD20_SDg2$player2)
SYD20_SDft2 <- as.matrix(SYD20_SDft)
numRows <- nrow(SYD20_SDft2)
numCols <- ncol(SYD20_SDft2)
SYD20_SDft3 <- SYD20_SDft2[c(2:numRows) , c(2:numCols)]
SYD20_SDTable <- graph.adjacency(SYD20_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(SYD20_SDTable, vertex.label = V(SYD20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
SYD20_SD.clusterCoef <- transitivity(SYD20_SDTable, type="global") #cluster coefficient
SYD20_SD.degreeCent <- centralization.degree(SYD20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_SDftn <- as.network.matrix(SYD20_SDft)
SYD20_SD.netDensity <- network.density(SYD20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_SD.entropy <- entropy(SYD20_SDft) #entropy

SYD20_SD.netMx <- cbind(SYD20_SD.netMx, SYD20_SD.clusterCoef, SYD20_SD.degreeCent$centralization,
                        SYD20_SD.netDensity, SYD20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************

round = 20
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
SYD20_TDg2 <- data.frame(SYD20_TD)
SYD20_TDg2 <- SYD20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_TDg2$player1
player2vector <- SYD20_TDg2$player2
SYD20_TDg3 <- SYD20_TDg2
SYD20_TDg3$p1inp2vec <- is.element(SYD20_TDg3$player1, player2vector)
SYD20_TDg3$p2inp1vec <- is.element(SYD20_TDg3$player2, player1vector)

addPlayer1 <- SYD20_TDg3[ which(SYD20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_TDg3[ which(SYD20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_TDg2 <- rbind(SYD20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
SYD20_TDft <- ftable(SYD20_TDg2$player1, SYD20_TDg2$player2)
SYD20_TDft2 <- as.matrix(SYD20_TDft)
numRows <- nrow(SYD20_TDft2)
numCols <- ncol(SYD20_TDft2)
SYD20_TDft3 <- SYD20_TDft2[c(2:numRows) , c(2:numCols)]
SYD20_TDTable <- graph.adjacency(SYD20_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(SYD20_TDTable, vertex.label = V(SYD20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
SYD20_TD.clusterCoef <- transitivity(SYD20_TDTable, type="global") #cluster coefficient
SYD20_TD.degreeCent <- centralization.degree(SYD20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_TDftn <- as.network.matrix(SYD20_TDft)
SYD20_TD.netDensity <- network.density(SYD20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_TD.entropy <- entropy(SYD20_TDft) #entropy

SYD20_TD.netMx <- cbind(SYD20_TD.netMx, SYD20_TD.clusterCoef, SYD20_TD.degreeCent$centralization,
                        SYD20_TD.netDensity, SYD20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
SYD20_QTg2 <- data.frame(SYD20_QT)
SYD20_QTg2 <- SYD20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD20_QTg2$player1
player2vector <- SYD20_QTg2$player2
SYD20_QTg3 <- SYD20_QTg2
SYD20_QTg3$p1inp2vec <- is.element(SYD20_QTg3$player1, player2vector)
SYD20_QTg3$p2inp1vec <- is.element(SYD20_QTg3$player2, player1vector)

addPlayer1 <- SYD20_QTg3[ which(SYD20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD20_QTg3[ which(SYD20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD20_QTg2 <- rbind(SYD20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
SYD20_QTft <- ftable(SYD20_QTg2$player1, SYD20_QTg2$player2)
SYD20_QTft2 <- as.matrix(SYD20_QTft)
numRows <- nrow(SYD20_QTft2)
numCols <- ncol(SYD20_QTft2)
SYD20_QTft3 <- SYD20_QTft2[c(2:numRows) , c(2:numCols)]
SYD20_QTTable <- graph.adjacency(SYD20_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(SYD20_QTTable, vertex.label = V(SYD20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
SYD20_QT.clusterCoef <- transitivity(SYD20_QTTable, type="global") #cluster coefficient
SYD20_QT.degreeCent <- centralization.degree(SYD20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD20_QTftn <- as.network.matrix(SYD20_QTft)
SYD20_QT.netDensity <- network.density(SYD20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD20_QT.entropy <- entropy(SYD20_QTft) #entropy

SYD20_QT.netMx <- cbind(SYD20_QT.netMx, SYD20_QT.clusterCoef, SYD20_QT.degreeCent$centralization,
                        SYD20_QT.netDensity, SYD20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD20_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "WB"
KIoutcome = "Goal_F"
WB20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
WB20_Gg2 <- data.frame(WB20_G)
WB20_Gg2 <- WB20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_Gg2$player1
player2vector <- WB20_Gg2$player2
WB20_Gg3 <- WB20_Gg2
WB20_Gg3$p1inp2vec <- is.element(WB20_Gg3$player1, player2vector)
WB20_Gg3$p2inp1vec <- is.element(WB20_Gg3$player2, player1vector)

addPlayer1 <- WB20_Gg3[ which(WB20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_Gg3[ which(WB20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_Gg2 <- rbind(WB20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
WB20_Gft <- ftable(WB20_Gg2$player1, WB20_Gg2$player2)
WB20_Gft2 <- as.matrix(WB20_Gft)
numRows <- nrow(WB20_Gft2)
numCols <- ncol(WB20_Gft2)
WB20_Gft3 <- WB20_Gft2[c(2:numRows) , c(2:numCols)]
WB20_GTable <- graph.adjacency(WB20_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(WB20_GTable, vertex.label = V(WB20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
WB20_G.clusterCoef <- transitivity(WB20_GTable, type="global") #cluster coefficient
WB20_G.degreeCent <- centralization.degree(WB20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_Gftn <- as.network.matrix(WB20_Gft)
WB20_G.netDensity <- network.density(WB20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_G.entropy <- entropy(WB20_Gft) #entropy

WB20_G.netMx <- cbind(WB20_G.netMx, WB20_G.clusterCoef, WB20_G.degreeCent$centralization,
                      WB20_G.netDensity, WB20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************
#NA

round = 20
teamName = "WB"
KIoutcome = "Behind_F"
WB20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
WB20_Bg2 <- data.frame(WB20_B)
WB20_Bg2 <- WB20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_Bg2$player1
player2vector <- WB20_Bg2$player2
WB20_Bg3 <- WB20_Bg2
WB20_Bg3$p1inp2vec <- is.element(WB20_Bg3$player1, player2vector)
WB20_Bg3$p2inp1vec <- is.element(WB20_Bg3$player2, player1vector)

addPlayer1 <- WB20_Bg3[ which(WB20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_Bg3[ which(WB20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_Bg2 <- rbind(WB20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
WB20_Bft <- ftable(WB20_Bg2$player1, WB20_Bg2$player2)
WB20_Bft2 <- as.matrix(WB20_Bft)
numRows <- nrow(WB20_Bft2)
numCols <- ncol(WB20_Bft2)
WB20_Bft3 <- WB20_Bft2[c(2:numRows) , c(2:numCols)]
WB20_BTable <- graph.adjacency(WB20_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(WB20_BTable, vertex.label = V(WB20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
WB20_B.clusterCoef <- transitivity(WB20_BTable, type="global") #cluster coefficient
WB20_B.degreeCent <- centralization.degree(WB20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_Bftn <- as.network.matrix(WB20_Bft)
WB20_B.netDensity <- network.density(WB20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_B.entropy <- entropy(WB20_Bft) #entropy

WB20_B.netMx <- cbind(WB20_B.netMx, WB20_B.clusterCoef, WB20_B.degreeCent$centralization,
                      WB20_B.netDensity, WB20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "WB"
KIoutcome = "Stoppage_F"
WB20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
WB20_SFg2 <- data.frame(WB20_SF)
WB20_SFg2 <- WB20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_SFg2$player1
player2vector <- WB20_SFg2$player2
WB20_SFg3 <- WB20_SFg2
WB20_SFg3$p1inp2vec <- is.element(WB20_SFg3$player1, player2vector)
WB20_SFg3$p2inp1vec <- is.element(WB20_SFg3$player2, player1vector)

addPlayer1 <- WB20_SFg3[ which(WB20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_SFg3[ which(WB20_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_SFg2 <- rbind(WB20_SFg2, addPlayers)

#ROUND 20, FWD Stoppage graph using weighted edges
WB20_SFft <- ftable(WB20_SFg2$player1, WB20_SFg2$player2)
WB20_SFft2 <- as.matrix(WB20_SFft)
numRows <- nrow(WB20_SFft2)
numCols <- ncol(WB20_SFft2)
WB20_SFft3 <- WB20_SFft2[c(2:numRows) , c(2:numCols)]
WB20_SFTable <- graph.adjacency(WB20_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(WB20_SFTable, vertex.label = V(WB20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
WB20_SF.clusterCoef <- transitivity(WB20_SFTable, type="global") #cluster coefficient
WB20_SF.degreeCent <- centralization.degree(WB20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_SFftn <- as.network.matrix(WB20_SFft)
WB20_SF.netDensity <- network.density(WB20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_SF.entropy <- entropy(WB20_SFft) #entropy

WB20_SF.netMx <- cbind(WB20_SF.netMx, WB20_SF.clusterCoef, WB20_SF.degreeCent$centralization,
                       WB20_SF.netDensity, WB20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************
#NA

round = 20
teamName = "WB"
KIoutcome = "Turnover_F"
WB20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
WB20_TFg2 <- data.frame(WB20_TF)
WB20_TFg2 <- WB20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_TFg2$player1
player2vector <- WB20_TFg2$player2
WB20_TFg3 <- WB20_TFg2
WB20_TFg3$p1inp2vec <- is.element(WB20_TFg3$player1, player2vector)
WB20_TFg3$p2inp1vec <- is.element(WB20_TFg3$player2, player1vector)

addPlayer1 <- WB20_TFg3[ which(WB20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_TFg3[ which(WB20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_TFg2 <- rbind(WB20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
WB20_TFft <- ftable(WB20_TFg2$player1, WB20_TFg2$player2)
WB20_TFft2 <- as.matrix(WB20_TFft)
numRows <- nrow(WB20_TFft2)
numCols <- ncol(WB20_TFft2)
WB20_TFft3 <- WB20_TFft2[c(2:numRows) , c(2:numCols)]
WB20_TFTable <- graph.adjacency(WB20_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(WB20_TFTable, vertex.label = V(WB20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
WB20_TF.clusterCoef <- transitivity(WB20_TFTable, type="global") #cluster coefficient
WB20_TF.degreeCent <- centralization.degree(WB20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_TFftn <- as.network.matrix(WB20_TFft)
WB20_TF.netDensity <- network.density(WB20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_TF.entropy <- entropy(WB20_TFft) #entropy

WB20_TF.netMx <- cbind(WB20_TF.netMx, WB20_TF.clusterCoef, WB20_TF.degreeCent$centralization,
                       WB20_TF.netDensity, WB20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************
#NA

round = 20
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
WB20_SAMg2 <- data.frame(WB20_SAM)
WB20_SAMg2 <- WB20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_SAMg2$player1
player2vector <- WB20_SAMg2$player2
WB20_SAMg3 <- WB20_SAMg2
WB20_SAMg3$p1inp2vec <- is.element(WB20_SAMg3$player1, player2vector)
WB20_SAMg3$p2inp1vec <- is.element(WB20_SAMg3$player2, player1vector)

addPlayer1 <- WB20_SAMg3[ which(WB20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_SAMg3[ which(WB20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_SAMg2 <- rbind(WB20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
WB20_SAMft <- ftable(WB20_SAMg2$player1, WB20_SAMg2$player2)
WB20_SAMft2 <- as.matrix(WB20_SAMft)
numRows <- nrow(WB20_SAMft2)
numCols <- ncol(WB20_SAMft2)
WB20_SAMft3 <- WB20_SAMft2[c(2:numRows) , c(2:numCols)]
WB20_SAMTable <- graph.adjacency(WB20_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(WB20_SAMTable, vertex.label = V(WB20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
WB20_SAM.clusterCoef <- transitivity(WB20_SAMTable, type="global") #cluster coefficient
WB20_SAM.degreeCent <- centralization.degree(WB20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_SAMftn <- as.network.matrix(WB20_SAMft)
WB20_SAM.netDensity <- network.density(WB20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_SAM.entropy <- entropy(WB20_SAMft) #entropy

WB20_SAM.netMx <- cbind(WB20_SAM.netMx, WB20_SAM.clusterCoef, WB20_SAM.degreeCent$centralization,
                        WB20_SAM.netDensity, WB20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "WB"
KIoutcome = "Turnover_AM"
WB20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
WB20_TAMg2 <- data.frame(WB20_TAM)
WB20_TAMg2 <- WB20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_TAMg2$player1
player2vector <- WB20_TAMg2$player2
WB20_TAMg3 <- WB20_TAMg2
WB20_TAMg3$p1inp2vec <- is.element(WB20_TAMg3$player1, player2vector)
WB20_TAMg3$p2inp1vec <- is.element(WB20_TAMg3$player2, player1vector)

addPlayer1 <- WB20_TAMg3[ which(WB20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- WB20_TAMg3[ which(WB20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_TAMg2 <- rbind(WB20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
WB20_TAMft <- ftable(WB20_TAMg2$player1, WB20_TAMg2$player2)
WB20_TAMft2 <- as.matrix(WB20_TAMft)
numRows <- nrow(WB20_TAMft2)
numCols <- ncol(WB20_TAMft2)
WB20_TAMft3 <- WB20_TAMft2[c(2:numRows) , c(2:numCols)]
WB20_TAMTable <- graph.adjacency(WB20_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(WB20_TAMTable, vertex.label = V(WB20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
WB20_TAM.clusterCoef <- transitivity(WB20_TAMTable, type="global") #cluster coefficient
WB20_TAM.degreeCent <- centralization.degree(WB20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_TAMftn <- as.network.matrix(WB20_TAMft)
WB20_TAM.netDensity <- network.density(WB20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_TAM.entropy <- entropy(WB20_TAMft) #entropy

WB20_TAM.netMx <- cbind(WB20_TAM.netMx, WB20_TAM.clusterCoef, WB20_TAM.degreeCent$centralization,
                        WB20_TAM.netDensity, WB20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************
#NA

round = 20
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
WB20_SDMg2 <- data.frame(WB20_SDM)
WB20_SDMg2 <- WB20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_SDMg2$player1
player2vector <- WB20_SDMg2$player2
WB20_SDMg3 <- WB20_SDMg2
WB20_SDMg3$p1inp2vec <- is.element(WB20_SDMg3$player1, player2vector)
WB20_SDMg3$p2inp1vec <- is.element(WB20_SDMg3$player2, player1vector)

addPlayer1 <- WB20_SDMg3[ which(WB20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_SDMg3[ which(WB20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_SDMg2 <- rbind(WB20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
WB20_SDMft <- ftable(WB20_SDMg2$player1, WB20_SDMg2$player2)
WB20_SDMft2 <- as.matrix(WB20_SDMft)
numRows <- nrow(WB20_SDMft2)
numCols <- ncol(WB20_SDMft2)
WB20_SDMft3 <- WB20_SDMft2[c(2:numRows) , c(2:numCols)]
WB20_SDMTable <- graph.adjacency(WB20_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(WB20_SDMTable, vertex.label = V(WB20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
WB20_SDM.clusterCoef <- transitivity(WB20_SDMTable, type="global") #cluster coefficient
WB20_SDM.degreeCent <- centralization.degree(WB20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_SDMftn <- as.network.matrix(WB20_SDMft)
WB20_SDM.netDensity <- network.density(WB20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_SDM.entropy <- entropy(WB20_SDMft) #entropy

WB20_SDM.netMx <- cbind(WB20_SDM.netMx, WB20_SDM.clusterCoef, WB20_SDM.degreeCent$centralization,
                        WB20_SDM.netDensity, WB20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "WB"
KIoutcome = "Turnover_DM"
WB20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
WB20_TDMg2 <- data.frame(WB20_TDM)
WB20_TDMg2 <- WB20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_TDMg2$player1
player2vector <- WB20_TDMg2$player2
WB20_TDMg3 <- WB20_TDMg2
WB20_TDMg3$p1inp2vec <- is.element(WB20_TDMg3$player1, player2vector)
WB20_TDMg3$p2inp1vec <- is.element(WB20_TDMg3$player2, player1vector)

addPlayer1 <- WB20_TDMg3[ which(WB20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_TDMg3[ which(WB20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_TDMg2 <- rbind(WB20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
WB20_TDMft <- ftable(WB20_TDMg2$player1, WB20_TDMg2$player2)
WB20_TDMft2 <- as.matrix(WB20_TDMft)
numRows <- nrow(WB20_TDMft2)
numCols <- ncol(WB20_TDMft2)
WB20_TDMft3 <- WB20_TDMft2[c(2:numRows) , c(2:numCols)]
WB20_TDMTable <- graph.adjacency(WB20_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(WB20_TDMTable, vertex.label = V(WB20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
WB20_TDM.clusterCoef <- transitivity(WB20_TDMTable, type="global") #cluster coefficient
WB20_TDM.degreeCent <- centralization.degree(WB20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_TDMftn <- as.network.matrix(WB20_TDMft)
WB20_TDM.netDensity <- network.density(WB20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_TDM.entropy <- entropy(WB20_TDMft) #entropy

WB20_TDM.netMx <- cbind(WB20_TDM.netMx, WB20_TDM.clusterCoef, WB20_TDM.degreeCent$centralization,
                        WB20_TDM.netDensity, WB20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "WB"
KIoutcome = "Stoppage_D"
WB20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
WB20_SDg2 <- data.frame(WB20_SD)
WB20_SDg2 <- WB20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_SDg2$player1
player2vector <- WB20_SDg2$player2
WB20_SDg3 <- WB20_SDg2
WB20_SDg3$p1inp2vec <- is.element(WB20_SDg3$player1, player2vector)
WB20_SDg3$p2inp1vec <- is.element(WB20_SDg3$player2, player1vector)

addPlayer1 <- WB20_SDg3[ which(WB20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_SDg3[ which(WB20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_SDg2 <- rbind(WB20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
WB20_SDft <- ftable(WB20_SDg2$player1, WB20_SDg2$player2)
WB20_SDft2 <- as.matrix(WB20_SDft)
numRows <- nrow(WB20_SDft2)
numCols <- ncol(WB20_SDft2)
WB20_SDft3 <- WB20_SDft2[c(2:numRows) , c(2:numCols)]
WB20_SDTable <- graph.adjacency(WB20_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(WB20_SDTable, vertex.label = V(WB20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
WB20_SD.clusterCoef <- transitivity(WB20_SDTable, type="global") #cluster coefficient
WB20_SD.degreeCent <- centralization.degree(WB20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_SDftn <- as.network.matrix(WB20_SDft)
WB20_SD.netDensity <- network.density(WB20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_SD.entropy <- entropy(WB20_SDft) #entropy

WB20_SD.netMx <- cbind(WB20_SD.netMx, WB20_SD.clusterCoef, WB20_SD.degreeCent$centralization,
                       WB20_SD.netDensity, WB20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "WB"
KIoutcome = "Turnover_D"
WB20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
WB20_TDg2 <- data.frame(WB20_TD)
WB20_TDg2 <- WB20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_TDg2$player1
player2vector <- WB20_TDg2$player2
WB20_TDg3 <- WB20_TDg2
WB20_TDg3$p1inp2vec <- is.element(WB20_TDg3$player1, player2vector)
WB20_TDg3$p2inp1vec <- is.element(WB20_TDg3$player2, player1vector)

addPlayer1 <- WB20_TDg3[ which(WB20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_TDg3[ which(WB20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_TDg2 <- rbind(WB20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
WB20_TDft <- ftable(WB20_TDg2$player1, WB20_TDg2$player2)
WB20_TDft2 <- as.matrix(WB20_TDft)
numRows <- nrow(WB20_TDft2)
numCols <- ncol(WB20_TDft2)
WB20_TDft3 <- WB20_TDft2[c(2:numRows) , c(2:numCols)]
WB20_TDTable <- graph.adjacency(WB20_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(WB20_TDTable, vertex.label = V(WB20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
WB20_TD.clusterCoef <- transitivity(WB20_TDTable, type="global") #cluster coefficient
WB20_TD.degreeCent <- centralization.degree(WB20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_TDftn <- as.network.matrix(WB20_TDft)
WB20_TD.netDensity <- network.density(WB20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_TD.entropy <- entropy(WB20_TDft) #entropy

WB20_TD.netMx <- cbind(WB20_TD.netMx, WB20_TD.clusterCoef, WB20_TD.degreeCent$centralization,
                       WB20_TD.netDensity, WB20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
WB20_QTg2 <- data.frame(WB20_QT)
WB20_QTg2 <- WB20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB20_QTg2$player1
player2vector <- WB20_QTg2$player2
WB20_QTg3 <- WB20_QTg2
WB20_QTg3$p1inp2vec <- is.element(WB20_QTg3$player1, player2vector)
WB20_QTg3$p2inp1vec <- is.element(WB20_QTg3$player2, player1vector)

addPlayer1 <- WB20_QTg3[ which(WB20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB20_QTg3[ which(WB20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB20_QTg2 <- rbind(WB20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
WB20_QTft <- ftable(WB20_QTg2$player1, WB20_QTg2$player2)
WB20_QTft2 <- as.matrix(WB20_QTft)
numRows <- nrow(WB20_QTft2)
numCols <- ncol(WB20_QTft2)
WB20_QTft3 <- WB20_QTft2[c(2:numRows) , c(2:numCols)]
WB20_QTTable <- graph.adjacency(WB20_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(WB20_QTTable, vertex.label = V(WB20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
WB20_QT.clusterCoef <- transitivity(WB20_QTTable, type="global") #cluster coefficient
WB20_QT.degreeCent <- centralization.degree(WB20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB20_QTftn <- as.network.matrix(WB20_QTft)
WB20_QT.netDensity <- network.density(WB20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB20_QT.entropy <- entropy(WB20_QTft) #entropy

WB20_QT.netMx <- cbind(WB20_QT.netMx, WB20_QT.clusterCoef, WB20_QT.degreeCent$centralization,
                       WB20_QT.netDensity, WB20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB20_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 20
##

#ROUND 20, Goal***************************************************************

round = 20
teamName = "WCE"
KIoutcome = "Goal_F"
WCE20_G.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Goal with weighted edges
WCE20_Gg2 <- data.frame(WCE20_G)
WCE20_Gg2 <- WCE20_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_Gg2$player1
player2vector <- WCE20_Gg2$player2
WCE20_Gg3 <- WCE20_Gg2
WCE20_Gg3$p1inp2vec <- is.element(WCE20_Gg3$player1, player2vector)
WCE20_Gg3$p2inp1vec <- is.element(WCE20_Gg3$player2, player1vector)

addPlayer1 <- WCE20_Gg3[ which(WCE20_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_Gg3[ which(WCE20_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_Gg2 <- rbind(WCE20_Gg2, addPlayers)

#ROUND 20, Goal graph using weighted edges
WCE20_Gft <- ftable(WCE20_Gg2$player1, WCE20_Gg2$player2)
WCE20_Gft2 <- as.matrix(WCE20_Gft)
numRows <- nrow(WCE20_Gft2)
numCols <- ncol(WCE20_Gft2)
WCE20_Gft3 <- WCE20_Gft2[c(2:numRows) , c(2:numCols)]
WCE20_GTable <- graph.adjacency(WCE20_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Goal graph=weighted
plot.igraph(WCE20_GTable, vertex.label = V(WCE20_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Goal calulation of network metrics
#igraph
WCE20_G.clusterCoef <- transitivity(WCE20_GTable, type="global") #cluster coefficient
WCE20_G.degreeCent <- centralization.degree(WCE20_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_Gftn <- as.network.matrix(WCE20_Gft)
WCE20_G.netDensity <- network.density(WCE20_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_G.entropy <- entropy(WCE20_Gft) #entropy

WCE20_G.netMx <- cbind(WCE20_G.netMx, WCE20_G.clusterCoef, WCE20_G.degreeCent$centralization,
                       WCE20_G.netDensity, WCE20_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_G.netMx) <- varnames

#ROUND 20, Behind***************************************************************

round = 20
teamName = "WCE"
KIoutcome = "Behind_F"
WCE20_B.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, Behind with weighted edges
WCE20_Bg2 <- data.frame(WCE20_B)
WCE20_Bg2 <- WCE20_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_Bg2$player1
player2vector <- WCE20_Bg2$player2
WCE20_Bg3 <- WCE20_Bg2
WCE20_Bg3$p1inp2vec <- is.element(WCE20_Bg3$player1, player2vector)
WCE20_Bg3$p2inp1vec <- is.element(WCE20_Bg3$player2, player1vector)

addPlayer1 <- WCE20_Bg3[ which(WCE20_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_Bg3[ which(WCE20_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_Bg2 <- rbind(WCE20_Bg2, addPlayers)

#ROUND 20, Behind graph using weighted edges
WCE20_Bft <- ftable(WCE20_Bg2$player1, WCE20_Bg2$player2)
WCE20_Bft2 <- as.matrix(WCE20_Bft)
numRows <- nrow(WCE20_Bft2)
numCols <- ncol(WCE20_Bft2)
WCE20_Bft3 <- WCE20_Bft2[c(2:numRows) , c(2:numCols)]
WCE20_BTable <- graph.adjacency(WCE20_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#ROUND 20, Behind graph=weighted
plot.igraph(WCE20_BTable, vertex.label = V(WCE20_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, Behind calulation of network metrics
#igraph
WCE20_B.clusterCoef <- transitivity(WCE20_BTable, type="global") #cluster coefficient
WCE20_B.degreeCent <- centralization.degree(WCE20_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_Bftn <- as.network.matrix(WCE20_Bft)
WCE20_B.netDensity <- network.density(WCE20_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_B.entropy <- entropy(WCE20_Bft) #entropy

WCE20_B.netMx <- cbind(WCE20_B.netMx, WCE20_B.clusterCoef, WCE20_B.degreeCent$centralization,
                       WCE20_B.netDensity, WCE20_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_B.netMx) <- varnames

#ROUND 20, FWD Stoppage**********************************************************
#NA

round = 20
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE20_SF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Stoppage with weighted edges
WCE20_SFg2 <- data.frame(WCE20_SF)
WCE20_SFg2 <- WCE20_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_SFg2$player1
player2vector <- WCE20_SFg2$player2
WCE20_SFg3 <- WCE20_SFg2
WCE20_SFg3$p1inp2vec <- is.element(WCE20_SFg3$player1, player2vector)
WCE20_SFg3$p2inp1vec <- is.element(WCE20_SFg3$player2, player1vector)

addPlayer1 <- WCE20_SFg3[ which(WCE20_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

WCE20_SFg2 <- rbind(WCE20_SFg2, addPlayer1)

#ROUND 20, FWD Stoppage graph using weighted edges
WCE20_SFft <- ftable(WCE20_SFg2$player1, WCE20_SFg2$player2)
WCE20_SFft2 <- as.matrix(WCE20_SFft)
numRows <- nrow(WCE20_SFft2)
numCols <- ncol(WCE20_SFft2)
WCE20_SFft3 <- WCE20_SFft2[c(2:numRows) , c(1:numCols)]
WCE20_SFTable <- graph.adjacency(WCE20_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Stoppage graph=weighted
plot.igraph(WCE20_SFTable, vertex.label = V(WCE20_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Stoppage calulation of network metrics
#igraph
WCE20_SF.clusterCoef <- transitivity(WCE20_SFTable, type="global") #cluster coefficient
WCE20_SF.degreeCent <- centralization.degree(WCE20_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_SFftn <- as.network.matrix(WCE20_SFft)
WCE20_SF.netDensity <- network.density(WCE20_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_SF.entropy <- entropy(WCE20_SFft) #entropy

WCE20_SF.netMx <- cbind(WCE20_SF.netMx, WCE20_SF.clusterCoef, WCE20_SF.degreeCent$centralization,
                        WCE20_SF.netDensity, WCE20_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_SF.netMx) <- varnames

#ROUND 20, FWD Turnover**********************************************************

round = 20
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE20_TF.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, FWD Turnover with weighted edges
WCE20_TFg2 <- data.frame(WCE20_TF)
WCE20_TFg2 <- WCE20_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_TFg2$player1
player2vector <- WCE20_TFg2$player2
WCE20_TFg3 <- WCE20_TFg2
WCE20_TFg3$p1inp2vec <- is.element(WCE20_TFg3$player1, player2vector)
WCE20_TFg3$p2inp1vec <- is.element(WCE20_TFg3$player2, player1vector)

addPlayer1 <- WCE20_TFg3[ which(WCE20_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_TFg3[ which(WCE20_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_TFg2 <- rbind(WCE20_TFg2, addPlayers)

#ROUND 20, FWD Turnover graph using weighted edges
WCE20_TFft <- ftable(WCE20_TFg2$player1, WCE20_TFg2$player2)
WCE20_TFft2 <- as.matrix(WCE20_TFft)
numRows <- nrow(WCE20_TFft2)
numCols <- ncol(WCE20_TFft2)
WCE20_TFft3 <- WCE20_TFft2[c(2:numRows) , c(2:numCols)]
WCE20_TFTable <- graph.adjacency(WCE20_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, FWD Turnover graph=weighted
plot.igraph(WCE20_TFTable, vertex.label = V(WCE20_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, FWD Turnover calulation of network metrics
#igraph
WCE20_TF.clusterCoef <- transitivity(WCE20_TFTable, type="global") #cluster coefficient
WCE20_TF.degreeCent <- centralization.degree(WCE20_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_TFftn <- as.network.matrix(WCE20_TFft)
WCE20_TF.netDensity <- network.density(WCE20_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_TF.entropy <- entropy(WCE20_TFft) #entropy

WCE20_TF.netMx <- cbind(WCE20_TF.netMx, WCE20_TF.clusterCoef, WCE20_TF.degreeCent$centralization,
                        WCE20_TF.netDensity, WCE20_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_TF.netMx) <- varnames

#ROUND 20, AM Stoppage**********************************************************

round = 20
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE20_SAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Stoppage with weighted edges
WCE20_SAMg2 <- data.frame(WCE20_SAM)
WCE20_SAMg2 <- WCE20_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_SAMg2$player1
player2vector <- WCE20_SAMg2$player2
WCE20_SAMg3 <- WCE20_SAMg2
WCE20_SAMg3$p1inp2vec <- is.element(WCE20_SAMg3$player1, player2vector)
WCE20_SAMg3$p2inp1vec <- is.element(WCE20_SAMg3$player2, player1vector)

addPlayer1 <- WCE20_SAMg3[ which(WCE20_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_SAMg3[ which(WCE20_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_SAMg2 <- rbind(WCE20_SAMg2, addPlayers)

#ROUND 20, AM Stoppage graph using weighted edges
WCE20_SAMft <- ftable(WCE20_SAMg2$player1, WCE20_SAMg2$player2)
WCE20_SAMft2 <- as.matrix(WCE20_SAMft)
numRows <- nrow(WCE20_SAMft2)
numCols <- ncol(WCE20_SAMft2)
WCE20_SAMft3 <- WCE20_SAMft2[c(2:numRows) , c(2:numCols)]
WCE20_SAMTable <- graph.adjacency(WCE20_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Stoppage graph=weighted
plot.igraph(WCE20_SAMTable, vertex.label = V(WCE20_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Stoppage calulation of network metrics
#igraph
WCE20_SAM.clusterCoef <- transitivity(WCE20_SAMTable, type="global") #cluster coefficient
WCE20_SAM.degreeCent <- centralization.degree(WCE20_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_SAMftn <- as.network.matrix(WCE20_SAMft)
WCE20_SAM.netDensity <- network.density(WCE20_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_SAM.entropy <- entropy(WCE20_SAMft) #entropy

WCE20_SAM.netMx <- cbind(WCE20_SAM.netMx, WCE20_SAM.clusterCoef, WCE20_SAM.degreeCent$centralization,
                         WCE20_SAM.netDensity, WCE20_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_SAM.netMx) <- varnames

#ROUND 20, AM Turnover**********************************************************

round = 20
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE20_TAM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, AM Turnover with weighted edges
WCE20_TAMg2 <- data.frame(WCE20_TAM)
WCE20_TAMg2 <- WCE20_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_TAMg2$player1
player2vector <- WCE20_TAMg2$player2
WCE20_TAMg3 <- WCE20_TAMg2
WCE20_TAMg3$p1inp2vec <- is.element(WCE20_TAMg3$player1, player2vector)
WCE20_TAMg3$p2inp1vec <- is.element(WCE20_TAMg3$player2, player1vector)

addPlayer1 <- WCE20_TAMg3[ which(WCE20_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_TAMg3[ which(WCE20_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_TAMg2 <- rbind(WCE20_TAMg2, addPlayers)

#ROUND 20, AM Turnover graph using weighted edges
WCE20_TAMft <- ftable(WCE20_TAMg2$player1, WCE20_TAMg2$player2)
WCE20_TAMft2 <- as.matrix(WCE20_TAMft)
numRows <- nrow(WCE20_TAMft2)
numCols <- ncol(WCE20_TAMft2)
WCE20_TAMft3 <- WCE20_TAMft2[c(2:numRows) , c(2:numCols)]
WCE20_TAMTable <- graph.adjacency(WCE20_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, AM Turnover graph=weighted
plot.igraph(WCE20_TAMTable, vertex.label = V(WCE20_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, AM Turnover calulation of network metrics
#igraph
WCE20_TAM.clusterCoef <- transitivity(WCE20_TAMTable, type="global") #cluster coefficient
WCE20_TAM.degreeCent <- centralization.degree(WCE20_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_TAMftn <- as.network.matrix(WCE20_TAMft)
WCE20_TAM.netDensity <- network.density(WCE20_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_TAM.entropy <- entropy(WCE20_TAMft) #entropy

WCE20_TAM.netMx <- cbind(WCE20_TAM.netMx, WCE20_TAM.clusterCoef, WCE20_TAM.degreeCent$centralization,
                         WCE20_TAM.netDensity, WCE20_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_TAM.netMx) <- varnames

#ROUND 20, DM Stoppage**********************************************************
#NA

round = 20
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE20_SDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Stoppage with weighted edges
WCE20_SDMg2 <- data.frame(WCE20_SDM)
WCE20_SDMg2 <- WCE20_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_SDMg2$player1
player2vector <- WCE20_SDMg2$player2
WCE20_SDMg3 <- WCE20_SDMg2
WCE20_SDMg3$p1inp2vec <- is.element(WCE20_SDMg3$player1, player2vector)
WCE20_SDMg3$p2inp1vec <- is.element(WCE20_SDMg3$player2, player1vector)

addPlayer1 <- WCE20_SDMg3[ which(WCE20_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_SDMg3[ which(WCE20_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_SDMg2 <- rbind(WCE20_SDMg2, addPlayers)

#ROUND 20, DM Stoppage graph using weighted edges
WCE20_SDMft <- ftable(WCE20_SDMg2$player1, WCE20_SDMg2$player2)
WCE20_SDMft2 <- as.matrix(WCE20_SDMft)
numRows <- nrow(WCE20_SDMft2)
numCols <- ncol(WCE20_SDMft2)
WCE20_SDMft3 <- WCE20_SDMft2[c(2:numRows) , c(2:numCols)]
WCE20_SDMTable <- graph.adjacency(WCE20_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Stoppage graph=weighted
plot.igraph(WCE20_SDMTable, vertex.label = V(WCE20_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Stoppage calulation of network metrics
#igraph
WCE20_SDM.clusterCoef <- transitivity(WCE20_SDMTable, type="global") #cluster coefficient
WCE20_SDM.degreeCent <- centralization.degree(WCE20_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_SDMftn <- as.network.matrix(WCE20_SDMft)
WCE20_SDM.netDensity <- network.density(WCE20_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_SDM.entropy <- entropy(WCE20_SDMft) #entropy

WCE20_SDM.netMx <- cbind(WCE20_SDM.netMx, WCE20_SDM.clusterCoef, WCE20_SDM.degreeCent$centralization,
                         WCE20_SDM.netDensity, WCE20_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_SDM.netMx) <- varnames

#ROUND 20, DM Turnover**********************************************************

round = 20
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE20_TDM.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, DM Turnover with weighted edges
WCE20_TDMg2 <- data.frame(WCE20_TDM)
WCE20_TDMg2 <- WCE20_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_TDMg2$player1
player2vector <- WCE20_TDMg2$player2
WCE20_TDMg3 <- WCE20_TDMg2
WCE20_TDMg3$p1inp2vec <- is.element(WCE20_TDMg3$player1, player2vector)
WCE20_TDMg3$p2inp1vec <- is.element(WCE20_TDMg3$player2, player1vector)

addPlayer1 <- WCE20_TDMg3[ which(WCE20_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_TDMg3[ which(WCE20_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_TDMg2 <- rbind(WCE20_TDMg2, addPlayers)

#ROUND 20, DM Turnover graph using weighted edges
WCE20_TDMft <- ftable(WCE20_TDMg2$player1, WCE20_TDMg2$player2)
WCE20_TDMft2 <- as.matrix(WCE20_TDMft)
numRows <- nrow(WCE20_TDMft2)
numCols <- ncol(WCE20_TDMft2)
WCE20_TDMft3 <- WCE20_TDMft2[c(2:numRows) , c(2:numCols)]
WCE20_TDMTable <- graph.adjacency(WCE20_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#ROUND 20, DM Turnover graph=weighted
plot.igraph(WCE20_TDMTable, vertex.label = V(WCE20_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, DM Turnover calulation of network metrics
#igraph
WCE20_TDM.clusterCoef <- transitivity(WCE20_TDMTable, type="global") #cluster coefficient
WCE20_TDM.degreeCent <- centralization.degree(WCE20_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_TDMftn <- as.network.matrix(WCE20_TDMft)
WCE20_TDM.netDensity <- network.density(WCE20_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_TDM.entropy <- entropy(WCE20_TDMft) #entropy

WCE20_TDM.netMx <- cbind(WCE20_TDM.netMx, WCE20_TDM.clusterCoef, WCE20_TDM.degreeCent$centralization,
                         WCE20_TDM.netDensity, WCE20_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_TDM.netMx) <- varnames

#ROUND 20, D Stoppage**********************************************************
#NA

round = 20
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE20_SD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Stoppage with weighted edges
WCE20_SDg2 <- data.frame(WCE20_SD)
WCE20_SDg2 <- WCE20_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_SDg2$player1
player2vector <- WCE20_SDg2$player2
WCE20_SDg3 <- WCE20_SDg2
WCE20_SDg3$p1inp2vec <- is.element(WCE20_SDg3$player1, player2vector)
WCE20_SDg3$p2inp1vec <- is.element(WCE20_SDg3$player2, player1vector)

addPlayer1 <- WCE20_SDg3[ which(WCE20_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_SDg3[ which(WCE20_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_SDg2 <- rbind(WCE20_SDg2, addPlayers)

#ROUND 20, D Stoppage graph using weighted edges
WCE20_SDft <- ftable(WCE20_SDg2$player1, WCE20_SDg2$player2)
WCE20_SDft2 <- as.matrix(WCE20_SDft)
numRows <- nrow(WCE20_SDft2)
numCols <- ncol(WCE20_SDft2)
WCE20_SDft3 <- WCE20_SDft2[c(2:numRows) , c(2:numCols)]
WCE20_SDTable <- graph.adjacency(WCE20_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Stoppage graph=weighted
plot.igraph(WCE20_SDTable, vertex.label = V(WCE20_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Stoppage calulation of network metrics
#igraph
WCE20_SD.clusterCoef <- transitivity(WCE20_SDTable, type="global") #cluster coefficient
WCE20_SD.degreeCent <- centralization.degree(WCE20_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_SDftn <- as.network.matrix(WCE20_SDft)
WCE20_SD.netDensity <- network.density(WCE20_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_SD.entropy <- entropy(WCE20_SDft) #entropy

WCE20_SD.netMx <- cbind(WCE20_SD.netMx, WCE20_SD.clusterCoef, WCE20_SD.degreeCent$centralization,
                        WCE20_SD.netDensity, WCE20_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_SD.netMx) <- varnames

#ROUND 20, D Turnover**********************************************************
#NA

round = 20
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE20_TD.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, D Turnover with weighted edges
WCE20_TDg2 <- data.frame(WCE20_TD)
WCE20_TDg2 <- WCE20_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_TDg2$player1
player2vector <- WCE20_TDg2$player2
WCE20_TDg3 <- WCE20_TDg2
WCE20_TDg3$p1inp2vec <- is.element(WCE20_TDg3$player1, player2vector)
WCE20_TDg3$p2inp1vec <- is.element(WCE20_TDg3$player2, player1vector)

addPlayer1 <- WCE20_TDg3[ which(WCE20_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_TDg3[ which(WCE20_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_TDg2 <- rbind(WCE20_TDg2, addPlayers)

#ROUND 20, D Turnover graph using weighted edges
WCE20_TDft <- ftable(WCE20_TDg2$player1, WCE20_TDg2$player2)
WCE20_TDft2 <- as.matrix(WCE20_TDft)
numRows <- nrow(WCE20_TDft2)
numCols <- ncol(WCE20_TDft2)
WCE20_TDft3 <- WCE20_TDft2[c(2:numRows) , c(2:numCols)]
WCE20_TDTable <- graph.adjacency(WCE20_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, D Turnover graph=weighted
plot.igraph(WCE20_TDTable, vertex.label = V(WCE20_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, D Turnover calulation of network metrics
#igraph
WCE20_TD.clusterCoef <- transitivity(WCE20_TDTable, type="global") #cluster coefficient
WCE20_TD.degreeCent <- centralization.degree(WCE20_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_TDftn <- as.network.matrix(WCE20_TDft)
WCE20_TD.netDensity <- network.density(WCE20_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_TD.entropy <- entropy(WCE20_TDft) #entropy

WCE20_TD.netMx <- cbind(WCE20_TD.netMx, WCE20_TD.clusterCoef, WCE20_TD.degreeCent$centralization,
                        WCE20_TD.netDensity, WCE20_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_TD.netMx) <- varnames

#ROUND 20, End of Qtr**********************************************************
#NA

round = 20
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE20_QT.netMx <- cbind(round, teamName, KIoutcome)

#ROUND 20, End of Qtr with weighted edges
WCE20_QTg2 <- data.frame(WCE20_QT)
WCE20_QTg2 <- WCE20_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE20_QTg2$player1
player2vector <- WCE20_QTg2$player2
WCE20_QTg3 <- WCE20_QTg2
WCE20_QTg3$p1inp2vec <- is.element(WCE20_QTg3$player1, player2vector)
WCE20_QTg3$p2inp1vec <- is.element(WCE20_QTg3$player2, player1vector)

addPlayer1 <- WCE20_QTg3[ which(WCE20_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE20_QTg3[ which(WCE20_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE20_QTg2 <- rbind(WCE20_QTg2, addPlayers)

#ROUND 20, End of Qtr graph using weighted edges
WCE20_QTft <- ftable(WCE20_QTg2$player1, WCE20_QTg2$player2)
WCE20_QTft2 <- as.matrix(WCE20_QTft)
numRows <- nrow(WCE20_QTft2)
numCols <- ncol(WCE20_QTft2)
WCE20_QTft3 <- WCE20_QTft2[c(2:numRows) , c(2:numCols)]
WCE20_QTTable <- graph.adjacency(WCE20_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#ROUND 20, End of Qtr graph=weighted
plot.igraph(WCE20_QTTable, vertex.label = V(WCE20_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE20_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#ROUND 20, End of Qtr calulation of network metrics
#igraph
WCE20_QT.clusterCoef <- transitivity(WCE20_QTTable, type="global") #cluster coefficient
WCE20_QT.degreeCent <- centralization.degree(WCE20_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE20_QTftn <- as.network.matrix(WCE20_QTft)
WCE20_QT.netDensity <- network.density(WCE20_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE20_QT.entropy <- entropy(WCE20_QTft) #entropy

WCE20_QT.netMx <- cbind(WCE20_QT.netMx, WCE20_QT.clusterCoef, WCE20_QT.degreeCent$centralization,
                        WCE20_QT.netDensity, WCE20_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE20_QT.netMx) <- varnames
