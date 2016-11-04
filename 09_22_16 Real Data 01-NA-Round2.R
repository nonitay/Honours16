#####
#09-22-16- Real data 02
#Network Analysis
####

library(igraph)
library(network)
library(entropy)

#############################################################################
#ADELAIDE 

##
#ROUND 2
##

#Round 2, Goal***************************************************************

round = 2
teamName = "ADEL"
KIoutcome = "Goal_F"
ADEL02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
ADEL02_Gg2 <- data.frame(ADEL02_G)
ADEL02_Gg2 <- ADEL02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_Gg2$player1
player2vector <- ADEL02_Gg2$player2
ADEL02_Gg3 <- ADEL02_Gg2
ADEL02_Gg3$p1inp2vec <- is.element(ADEL02_Gg3$player1, player2vector)
ADEL02_Gg3$p2inp1vec <- is.element(ADEL02_Gg3$player2, player1vector)

addPlayer1 <- ADEL02_Gg3[ which(ADEL02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_Gg3[ which(ADEL02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_Gg2 <- rbind(ADEL02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
ADEL02_Gft <- ftable(ADEL02_Gg2$player1, ADEL02_Gg2$player2)
ADEL02_Gft2 <- as.matrix(ADEL02_Gft)
numRows <- nrow(ADEL02_Gft2)
numCols <- ncol(ADEL02_Gft2)
ADEL02_Gft3 <- ADEL02_Gft2[c(2:numRows) , c(2:numCols)]
ADEL02_GTable <- graph.adjacency(ADEL02_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(ADEL02_GTable, vertex.label = V(ADEL02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
ADEL02_G.clusterCoef <- transitivity(ADEL02_GTable, type="global") #cluster coefficient
ADEL02_G.degreeCent <- centralization.degree(ADEL02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_Gftn <- as.network.matrix(ADEL02_Gft)
ADEL02_G.netDensity <- network.density(ADEL02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_G.entropy <- entropy(ADEL02_Gft) #entropy

ADEL02_G.netMx <- cbind(ADEL02_G.netMx, ADEL02_G.clusterCoef, ADEL02_G.degreeCent$centralization,
                        ADEL02_G.netDensity, ADEL02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "ADEL"
KIoutcome = "Behind_F"
ADEL02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
ADEL02_Bg2 <- data.frame(ADEL02_B)
ADEL02_Bg2 <- ADEL02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_Bg2$player1
player2vector <- ADEL02_Bg2$player2
ADEL02_Bg3 <- ADEL02_Bg2
ADEL02_Bg3$p1inp2vec <- is.element(ADEL02_Bg3$player1, player2vector)
ADEL02_Bg3$p2inp1vec <- is.element(ADEL02_Bg3$player2, player1vector)

addPlayer1 <- ADEL02_Bg3[ which(ADEL02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_Bg3[ which(ADEL02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_Bg2 <- rbind(ADEL02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
ADEL02_Bft <- ftable(ADEL02_Bg2$player1, ADEL02_Bg2$player2)
ADEL02_Bft2 <- as.matrix(ADEL02_Bft)
numRows <- nrow(ADEL02_Bft2)
numCols <- ncol(ADEL02_Bft2)
ADEL02_Bft3 <- ADEL02_Bft2[c(2:numRows) , c(2:numCols)]
ADEL02_BTable <- graph.adjacency(ADEL02_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(ADEL02_BTable, vertex.label = V(ADEL02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
ADEL02_B.clusterCoef <- transitivity(ADEL02_BTable, type="global") #cluster coefficient
ADEL02_B.degreeCent <- centralization.degree(ADEL02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_Bftn <- as.network.matrix(ADEL02_Bft)
ADEL02_B.netDensity <- network.density(ADEL02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_B.entropy <- entropy(ADEL02_Bft) #entropy

ADEL02_B.netMx <- cbind(ADEL02_B.netMx, ADEL02_B.clusterCoef, ADEL02_B.degreeCent$centralization,
                        ADEL02_B.netDensity, ADEL02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "ADEL"
KIoutcome = "Stoppage_F"
ADEL02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
ADEL02_SFg2 <- data.frame(ADEL02_SF)
ADEL02_SFg2 <- ADEL02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_SFg2$player1
player2vector <- ADEL02_SFg2$player2
ADEL02_SFg3 <- ADEL02_SFg2
ADEL02_SFg3$p1inp2vec <- is.element(ADEL02_SFg3$player1, player2vector)
ADEL02_SFg3$p2inp1vec <- is.element(ADEL02_SFg3$player2, player1vector)

addPlayer1 <- ADEL02_SFg3[ which(ADEL02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_SFg3[ which(ADEL02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_SFg2 <- rbind(ADEL02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
ADEL02_SFft <- ftable(ADEL02_SFg2$player1, ADEL02_SFg2$player2)
ADEL02_SFft2 <- as.matrix(ADEL02_SFft)
numRows <- nrow(ADEL02_SFft2)
numCols <- ncol(ADEL02_SFft2)
ADEL02_SFft3 <- ADEL02_SFft2[c(2:numRows) , c(2:numCols)]
ADEL02_SFTable <- graph.adjacency(ADEL02_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(ADEL02_SFTable, vertex.label = V(ADEL02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
ADEL02_SF.clusterCoef <- transitivity(ADEL02_SFTable, type="global") #cluster coefficient
ADEL02_SF.degreeCent <- centralization.degree(ADEL02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_SFftn <- as.network.matrix(ADEL02_SFft)
ADEL02_SF.netDensity <- network.density(ADEL02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_SF.entropy <- entropy(ADEL02_SFft) #entropy

ADEL02_SF.netMx <- cbind(ADEL02_SF.netMx, ADEL02_SF.clusterCoef, ADEL02_SF.degreeCent$centralization,
                         ADEL02_SF.netDensity, ADEL02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "ADEL"
KIoutcome = "Turnover_F"
ADEL02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
ADEL02_TFg2 <- data.frame(ADEL02_TF)
ADEL02_TFg2 <- ADEL02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_TFg2$player1
player2vector <- ADEL02_TFg2$player2
ADEL02_TFg3 <- ADEL02_TFg2
ADEL02_TFg3$p1inp2vec <- is.element(ADEL02_TFg3$player1, player2vector)
ADEL02_TFg3$p2inp1vec <- is.element(ADEL02_TFg3$player2, player1vector)

addPlayer1 <- ADEL02_TFg3[ which(ADEL02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

ADEL02_TFg2 <- rbind(ADEL02_TFg2, addPlayer1)

#Round 2, FWD Turnover graph using weighted edges
ADEL02_TFft <- ftable(ADEL02_TFg2$player1, ADEL02_TFg2$player2)
ADEL02_TFft2 <- as.matrix(ADEL02_TFft)
numRows <- nrow(ADEL02_TFft2)
numCols <- ncol(ADEL02_TFft2)
ADEL02_TFft3 <- ADEL02_TFft2[c(2:numRows) , c(1:numCols)]
ADEL02_TFTable <- graph.adjacency(ADEL02_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(ADEL02_TFTable, vertex.label = V(ADEL02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
ADEL02_TF.clusterCoef <- transitivity(ADEL02_TFTable, type="global") #cluster coefficient
ADEL02_TF.degreeCent <- centralization.degree(ADEL02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_TFftn <- as.network.matrix(ADEL02_TFft)
ADEL02_TF.netDensity <- network.density(ADEL02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_TF.entropy <- entropy(ADEL02_TFft) #entropy

ADEL02_TF.netMx <- cbind(ADEL02_TF.netMx, ADEL02_TF.clusterCoef, ADEL02_TF.degreeCent$centralization,
                         ADEL02_TF.netDensity, ADEL02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "ADEL"
KIoutcome = "Stoppage_AM"
ADEL02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
ADEL02_SAMg2 <- data.frame(ADEL02_SAM)
ADEL02_SAMg2 <- ADEL02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_SAMg2$player1
player2vector <- ADEL02_SAMg2$player2
ADEL02_SAMg3 <- ADEL02_SAMg2
ADEL02_SAMg3$p1inp2vec <- is.element(ADEL02_SAMg3$player1, player2vector)
ADEL02_SAMg3$p2inp1vec <- is.element(ADEL02_SAMg3$player2, player1vector)

addPlayer1 <- ADEL02_SAMg3[ which(ADEL02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_SAMg3[ which(ADEL02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_SAMg2 <- rbind(ADEL02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
ADEL02_SAMft <- ftable(ADEL02_SAMg2$player1, ADEL02_SAMg2$player2)
ADEL02_SAMft2 <- as.matrix(ADEL02_SAMft)
numRows <- nrow(ADEL02_SAMft2)
numCols <- ncol(ADEL02_SAMft2)
ADEL02_SAMft3 <- ADEL02_SAMft2[c(2:numRows) , c(2:numCols)]
ADEL02_SAMTable <- graph.adjacency(ADEL02_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(ADEL02_SAMTable, vertex.label = V(ADEL02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
ADEL02_SAM.clusterCoef <- transitivity(ADEL02_SAMTable, type="global") #cluster coefficient
ADEL02_SAM.degreeCent <- centralization.degree(ADEL02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_SAMftn <- as.network.matrix(ADEL02_SAMft)
ADEL02_SAM.netDensity <- network.density(ADEL02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_SAM.entropy <- entropy(ADEL02_SAMft) #entropy

ADEL02_SAM.netMx <- cbind(ADEL02_SAM.netMx, ADEL02_SAM.clusterCoef, ADEL02_SAM.degreeCent$centralization,
                          ADEL02_SAM.netDensity, ADEL02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "ADEL"
KIoutcome = "Turnover_AM"
ADEL02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
ADEL02_TAMg2 <- data.frame(ADEL02_TAM)
ADEL02_TAMg2 <- ADEL02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_TAMg2$player1
player2vector <- ADEL02_TAMg2$player2
ADEL02_TAMg3 <- ADEL02_TAMg2
ADEL02_TAMg3$p1inp2vec <- is.element(ADEL02_TAMg3$player1, player2vector)
ADEL02_TAMg3$p2inp1vec <- is.element(ADEL02_TAMg3$player2, player1vector)

addPlayer1 <- ADEL02_TAMg3[ which(ADEL02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_TAMg3[ which(ADEL02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_TAMg2 <- rbind(ADEL02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
ADEL02_TAMft <- ftable(ADEL02_TAMg2$player1, ADEL02_TAMg2$player2)
ADEL02_TAMft2 <- as.matrix(ADEL02_TAMft)
numRows <- nrow(ADEL02_TAMft2)
numCols <- ncol(ADEL02_TAMft2)
ADEL02_TAMft3 <- ADEL02_TAMft2[c(2:numRows) , c(2:numCols)]
ADEL02_TAMTable <- graph.adjacency(ADEL02_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(ADEL02_TAMTable, vertex.label = V(ADEL02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
ADEL02_TAM.clusterCoef <- transitivity(ADEL02_TAMTable, type="global") #cluster coefficient
ADEL02_TAM.degreeCent <- centralization.degree(ADEL02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_TAMftn <- as.network.matrix(ADEL02_TAMft)
ADEL02_TAM.netDensity <- network.density(ADEL02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_TAM.entropy <- entropy(ADEL02_TAMft) #entropy

ADEL02_TAM.netMx <- cbind(ADEL02_TAM.netMx, ADEL02_TAM.clusterCoef, ADEL02_TAM.degreeCent$centralization,
                          ADEL02_TAM.netDensity, ADEL02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "ADEL"
KIoutcome = "Stoppage_DM"
ADEL02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
ADEL02_SDMg2 <- data.frame(ADEL02_SDM)
ADEL02_SDMg2 <- ADEL02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_SDMg2$player1
player2vector <- ADEL02_SDMg2$player2
ADEL02_SDMg3 <- ADEL02_SDMg2
ADEL02_SDMg3$p1inp2vec <- is.element(ADEL02_SDMg3$player1, player2vector)
ADEL02_SDMg3$p2inp1vec <- is.element(ADEL02_SDMg3$player2, player1vector)

addPlayer1 <- ADEL02_SDMg3[ which(ADEL02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_SDMg3[ which(ADEL02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_SDMg2 <- rbind(ADEL02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
ADEL02_SDMft <- ftable(ADEL02_SDMg2$player1, ADEL02_SDMg2$player2)
ADEL02_SDMft2 <- as.matrix(ADEL02_SDMft)
numRows <- nrow(ADEL02_SDMft2)
numCols <- ncol(ADEL02_SDMft2)
ADEL02_SDMft3 <- ADEL02_SDMft2[c(2:numRows) , c(2:numCols)]
ADEL02_SDMTable <- graph.adjacency(ADEL02_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(ADEL02_SDMTable, vertex.label = V(ADEL02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
ADEL02_SDM.clusterCoef <- transitivity(ADEL02_SDMTable, type="global") #cluster coefficient
ADEL02_SDM.degreeCent <- centralization.degree(ADEL02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_SDMftn <- as.network.matrix(ADEL02_SDMft)
ADEL02_SDM.netDensity <- network.density(ADEL02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_SDM.entropy <- entropy(ADEL02_SDMft) #entropy

ADEL02_SDM.netMx <- cbind(ADEL02_SDM.netMx, ADEL02_SDM.clusterCoef, ADEL02_SDM.degreeCent$centralization,
                          ADEL02_SDM.netDensity, ADEL02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "ADEL"
KIoutcome = "Turnover_DM"
ADEL02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
ADEL02_TDMg2 <- data.frame(ADEL02_TDM)
ADEL02_TDMg2 <- ADEL02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_TDMg2$player1
player2vector <- ADEL02_TDMg2$player2
ADEL02_TDMg3 <- ADEL02_TDMg2
ADEL02_TDMg3$p1inp2vec <- is.element(ADEL02_TDMg3$player1, player2vector)
ADEL02_TDMg3$p2inp1vec <- is.element(ADEL02_TDMg3$player2, player1vector)

addPlayer1 <- ADEL02_TDMg3[ which(ADEL02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_TDMg3[ which(ADEL02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_TDMg2 <- rbind(ADEL02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
ADEL02_TDMft <- ftable(ADEL02_TDMg2$player1, ADEL02_TDMg2$player2)
ADEL02_TDMft2 <- as.matrix(ADEL02_TDMft)
numRows <- nrow(ADEL02_TDMft2)
numCols <- ncol(ADEL02_TDMft2)
ADEL02_TDMft3 <- ADEL02_TDMft2[c(2:numRows) , c(2:numCols)]
ADEL02_TDMTable <- graph.adjacency(ADEL02_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(ADEL02_TDMTable, vertex.label = V(ADEL02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
ADEL02_TDM.clusterCoef <- transitivity(ADEL02_TDMTable, type="global") #cluster coefficient
ADEL02_TDM.degreeCent <- centralization.degree(ADEL02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_TDMftn <- as.network.matrix(ADEL02_TDMft)
ADEL02_TDM.netDensity <- network.density(ADEL02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_TDM.entropy <- entropy(ADEL02_TDMft) #entropy

ADEL02_TDM.netMx <- cbind(ADEL02_TDM.netMx, ADEL02_TDM.clusterCoef, ADEL02_TDM.degreeCent$centralization,
                          ADEL02_TDM.netDensity, ADEL02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "ADEL"
KIoutcome = "Stoppage_D"
ADEL02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
ADEL02_SDg2 <- data.frame(ADEL02_SD)
ADEL02_SDg2 <- ADEL02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_SDg2$player1
player2vector <- ADEL02_SDg2$player2
ADEL02_SDg3 <- ADEL02_SDg2
ADEL02_SDg3$p1inp2vec <- is.element(ADEL02_SDg3$player1, player2vector)
ADEL02_SDg3$p2inp1vec <- is.element(ADEL02_SDg3$player2, player1vector)

addPlayer1 <- ADEL02_SDg3[ which(ADEL02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_SDg3[ which(ADEL02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_SDg2 <- rbind(ADEL02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
ADEL02_SDft <- ftable(ADEL02_SDg2$player1, ADEL02_SDg2$player2)
ADEL02_SDft2 <- as.matrix(ADEL02_SDft)
numRows <- nrow(ADEL02_SDft2)
numCols <- ncol(ADEL02_SDft2)
ADEL02_SDft3 <- ADEL02_SDft2[c(2:numRows) , c(2:numCols)]
ADEL02_SDTable <- graph.adjacency(ADEL02_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(ADEL02_SDTable, vertex.label = V(ADEL02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
ADEL02_SD.clusterCoef <- transitivity(ADEL02_SDTable, type="global") #cluster coefficient
ADEL02_SD.degreeCent <- centralization.degree(ADEL02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_SDftn <- as.network.matrix(ADEL02_SDft)
ADEL02_SD.netDensity <- network.density(ADEL02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_SD.entropy <- entropy(ADEL02_SDft) #entropy

ADEL02_SD.netMx <- cbind(ADEL02_SD.netMx, ADEL02_SD.clusterCoef, ADEL02_SD.degreeCent$centralization,
                         ADEL02_SD.netDensity, ADEL02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "ADEL"
KIoutcome = "Turnover_D"
ADEL02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
ADEL02_TDg2 <- data.frame(ADEL02_TD)
ADEL02_TDg2 <- ADEL02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_TDg2$player1
player2vector <- ADEL02_TDg2$player2
ADEL02_TDg3 <- ADEL02_TDg2
ADEL02_TDg3$p1inp2vec <- is.element(ADEL02_TDg3$player1, player2vector)
ADEL02_TDg3$p2inp1vec <- is.element(ADEL02_TDg3$player2, player1vector)

addPlayer1 <- ADEL02_TDg3[ which(ADEL02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_TDg3[ which(ADEL02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_TDg2 <- rbind(ADEL02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
ADEL02_TDft <- ftable(ADEL02_TDg2$player1, ADEL02_TDg2$player2)
ADEL02_TDft2 <- as.matrix(ADEL02_TDft)
numRows <- nrow(ADEL02_TDft2)
numCols <- ncol(ADEL02_TDft2)
ADEL02_TDft3 <- ADEL02_TDft2[c(2:numRows) , c(2:numCols)]
ADEL02_TDTable <- graph.adjacency(ADEL02_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(ADEL02_TDTable, vertex.label = V(ADEL02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
ADEL02_TD.clusterCoef <- transitivity(ADEL02_TDTable, type="global") #cluster coefficient
ADEL02_TD.degreeCent <- centralization.degree(ADEL02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_TDftn <- as.network.matrix(ADEL02_TDft)
ADEL02_TD.netDensity <- network.density(ADEL02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_TD.entropy <- entropy(ADEL02_TDft) #entropy

ADEL02_TD.netMx <- cbind(ADEL02_TD.netMx, ADEL02_TD.clusterCoef, ADEL02_TD.degreeCent$centralization,
                         ADEL02_TD.netDensity, ADEL02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "ADEL"
KIoutcome = "End of Qtr_DM"
ADEL02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
ADEL02_QTg2 <- data.frame(ADEL02_QT)
ADEL02_QTg2 <- ADEL02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ADEL02_QTg2$player1
player2vector <- ADEL02_QTg2$player2
ADEL02_QTg3 <- ADEL02_QTg2
ADEL02_QTg3$p1inp2vec <- is.element(ADEL02_QTg3$player1, player2vector)
ADEL02_QTg3$p2inp1vec <- is.element(ADEL02_QTg3$player2, player1vector)

addPlayer1 <- ADEL02_QTg3[ which(ADEL02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ADEL02_QTg3[ which(ADEL02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ADEL02_QTg2 <- rbind(ADEL02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
ADEL02_QTft <- ftable(ADEL02_QTg2$player1, ADEL02_QTg2$player2)
ADEL02_QTft2 <- as.matrix(ADEL02_QTft)
numRows <- nrow(ADEL02_QTft2)
numCols <- ncol(ADEL02_QTft2)
ADEL02_QTft3 <- ADEL02_QTft2[c(2:numRows) , c(2:numCols)]
ADEL02_QTTable <- graph.adjacency(ADEL02_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(ADEL02_QTTable, vertex.label = V(ADEL02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ADEL02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
ADEL02_QT.clusterCoef <- transitivity(ADEL02_QTTable, type="global") #cluster coefficient
ADEL02_QT.degreeCent <- centralization.degree(ADEL02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ADEL02_QTftn <- as.network.matrix(ADEL02_QTft)
ADEL02_QT.netDensity <- network.density(ADEL02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ADEL02_QT.entropy <- entropy(ADEL02_QTft) #entropy

ADEL02_QT.netMx <- cbind(ADEL02_QT.netMx, ADEL02_QT.clusterCoef, ADEL02_QT.degreeCent$centralization,
                         ADEL02_QT.netDensity, ADEL02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ADEL02_QT.netMx) <- varnames

#############################################################################
#BRISBANE

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA
round = 2
teamName = "BL"
KIoutcome = "Goal_F"
BL02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
BL02_Gg2 <- data.frame(BL02_G)
BL02_Gg2 <- BL02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_Gg2$player1
player2vector <- BL02_Gg2$player2
BL02_Gg3 <- BL02_Gg2
BL02_Gg3$p1inp2vec <- is.element(BL02_Gg3$player1, player2vector)
BL02_Gg3$p2inp1vec <- is.element(BL02_Gg3$player2, player1vector)

addPlayer1 <- BL02_Gg3[ which(BL02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL02_Gg3[ which(BL02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL02_Gg2 <- rbind(BL02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
BL02_Gft <- ftable(BL02_Gg2$player1, BL02_Gg2$player2)
BL02_Gft2 <- as.matrix(BL02_Gft)
numRows <- nrow(BL02_Gft2)
numCols <- ncol(BL02_Gft2)
BL02_Gft3 <- BL02_Gft2[c(2:numRows) , c(2:numCols)]
BL02_GTable <- graph.adjacency(BL02_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(BL02_GTable, vertex.label = V(BL02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
BL02_G.clusterCoef <- transitivity(BL02_GTable, type="global") #cluster coefficient
BL02_G.degreeCent <- centralization.degree(BL02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_Gftn <- as.network.matrix(BL02_Gft)
BL02_G.netDensity <- network.density(BL02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_G.entropy <- entropy(BL02_Gft) #entropy

BL02_G.netMx <- cbind(BL02_G.netMx, BL02_G.clusterCoef, BL02_G.degreeCent$centralization,
                      BL02_G.netDensity, BL02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_G.netMx) <- varnames

#Round 2, Behind***************************************************************

round = 2
teamName = "BL"
KIoutcome = "Behind_F"
BL02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
BL02_Bg2 <- data.frame(BL02_B)
BL02_Bg2 <- BL02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_Bg2$player1
player2vector <- BL02_Bg2$player2
BL02_Bg3 <- BL02_Bg2
BL02_Bg3$p1inp2vec <- is.element(BL02_Bg3$player1, player2vector)
BL02_Bg3$p2inp1vec <- is.element(BL02_Bg3$player2, player1vector)

addPlayer1 <- BL02_Bg3[ which(BL02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

BL02_Bg2 <- rbind(BL02_Bg2, addPlayer1)

#Round 2, Behind graph using weighted edges
BL02_Bft <- ftable(BL02_Bg2$player1, BL02_Bg2$player2)
BL02_Bft2 <- as.matrix(BL02_Bft)
numRows <- nrow(BL02_Bft2)
numCols <- ncol(BL02_Bft2)
BL02_Bft3 <- BL02_Bft2[c(2:numRows) , c(1:numCols)]
BL02_BTable <- graph.adjacency(BL02_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(BL02_BTable, vertex.label = V(BL02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
BL02_B.clusterCoef <- transitivity(BL02_BTable, type="global") #cluster coefficient
BL02_B.degreeCent <- centralization.degree(BL02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_Bftn <- as.network.matrix(BL02_Bft)
BL02_B.netDensity <- network.density(BL02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_B.entropy <- entropy(BL02_Bft) #entropy

BL02_B.netMx <- cbind(BL02_B.netMx, BL02_B.clusterCoef, BL02_B.degreeCent$centralization,
                      BL02_B.netDensity, BL02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************

round = 2
teamName = "BL"
KIoutcome = "Stoppage_F"
BL02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
BL02_SFg2 <- data.frame(BL02_SF)
BL02_SFg2 <- BL02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_SFg2$player1
player2vector <- BL02_SFg2$player2
BL02_SFg3 <- BL02_SFg2
BL02_SFg3$p1inp2vec <- is.element(BL02_SFg3$player1, player2vector)
BL02_SFg3$p2inp1vec <- is.element(BL02_SFg3$player2, player1vector)

addPlayer1 <- BL02_SFg3[ which(BL02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL02_SFg3[ which(BL02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL02_SFg2 <- rbind(BL02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
BL02_SFft <- ftable(BL02_SFg2$player1, BL02_SFg2$player2)
BL02_SFft2 <- as.matrix(BL02_SFft)
numRows <- nrow(BL02_SFft2)
numCols <- ncol(BL02_SFft2)
BL02_SFft3 <- BL02_SFft2[c(2:numRows) , c(2:numCols)]
BL02_SFTable <- graph.adjacency(BL02_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(BL02_SFTable, vertex.label = V(BL02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
BL02_SF.clusterCoef <- transitivity(BL02_SFTable, type="global") #cluster coefficient
BL02_SF.degreeCent <- centralization.degree(BL02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_SFftn <- as.network.matrix(BL02_SFft)
BL02_SF.netDensity <- network.density(BL02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_SF.entropy <- entropy(BL02_SFft) #entropy

BL02_SF.netMx <- cbind(BL02_SF.netMx, BL02_SF.clusterCoef, BL02_SF.degreeCent$centralization,
                       BL02_SF.netDensity, BL02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "BL"
KIoutcome = "Turnover_F"
BL02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
BL02_TFg2 <- data.frame(BL02_TF)
BL02_TFg2 <- BL02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_TFg2$player1
player2vector <- BL02_TFg2$player2
BL02_TFg3 <- BL02_TFg2
BL02_TFg3$p1inp2vec <- is.element(BL02_TFg3$player1, player2vector)
BL02_TFg3$p2inp1vec <- is.element(BL02_TFg3$player2, player1vector)

addPlayer1 <- BL02_TFg3[ which(BL02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL02_TFg3[ which(BL02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL02_TFg2 <- rbind(BL02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
BL02_TFft <- ftable(BL02_TFg2$player1, BL02_TFg2$player2)
BL02_TFft2 <- as.matrix(BL02_TFft)
numRows <- nrow(BL02_TFft2)
numCols <- ncol(BL02_TFft2)
BL02_TFft3 <- BL02_TFft2[c(2:numRows) , c(2:numCols)]
BL02_TFTable <- graph.adjacency(BL02_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(BL02_TFTable, vertex.label = V(BL02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
BL02_TF.clusterCoef <- transitivity(BL02_TFTable, type="global") #cluster coefficient
BL02_TF.degreeCent <- centralization.degree(BL02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_TFftn <- as.network.matrix(BL02_TFft)
BL02_TF.netDensity <- network.density(BL02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_TF.entropy <- entropy(BL02_TFft) #entropy

BL02_TF.netMx <- cbind(BL02_TF.netMx, BL02_TF.clusterCoef, BL02_TF.degreeCent$centralization,
                       BL02_TF.netDensity, BL02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************

round = 2
teamName = "BL"
KIoutcome = "Stoppage_AM"
BL02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
BL02_SAMg2 <- data.frame(BL02_SAM)
BL02_SAMg2 <- BL02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_SAMg2$player1
player2vector <- BL02_SAMg2$player2
BL02_SAMg3 <- BL02_SAMg2
BL02_SAMg3$p1inp2vec <- is.element(BL02_SAMg3$player1, player2vector)
BL02_SAMg3$p2inp1vec <- is.element(BL02_SAMg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- BL02_SAMg3[ which(BL02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

BL02_SAMg2 <- rbind(BL02_SAMg2, addPlayer2)

#Round 2, AM Stoppage graph using weighted edges
BL02_SAMft <- ftable(BL02_SAMg2$player1, BL02_SAMg2$player2)
BL02_SAMft2 <- as.matrix(BL02_SAMft)
numRows <- nrow(BL02_SAMft2)
numCols <- ncol(BL02_SAMft2)
BL02_SAMft3 <- BL02_SAMft2[c(1:numRows) , c(2:numCols)]
BL02_SAMTable <- graph.adjacency(BL02_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(BL02_SAMTable, vertex.label = V(BL02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
BL02_SAM.clusterCoef <- transitivity(BL02_SAMTable, type="global") #cluster coefficient
BL02_SAM.degreeCent <- centralization.degree(BL02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_SAMftn <- as.network.matrix(BL02_SAMft)
BL02_SAM.netDensity <- network.density(BL02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_SAM.entropy <- entropy(BL02_SAMft) #entropy

BL02_SAM.netMx <- cbind(BL02_SAM.netMx, BL02_SAM.clusterCoef, BL02_SAM.degreeCent$centralization,
                        BL02_SAM.netDensity, BL02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "BL"
KIoutcome = "Turnover_AM"
BL02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
BL02_TAMg2 <- data.frame(BL02_TAM)
BL02_TAMg2 <- BL02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_TAMg2$player1
player2vector <- BL02_TAMg2$player2
BL02_TAMg3 <- BL02_TAMg2
BL02_TAMg3$p1inp2vec <- is.element(BL02_TAMg3$player1, player2vector)
BL02_TAMg3$p2inp1vec <- is.element(BL02_TAMg3$player2, player1vector)

addPlayer1 <- BL02_TAMg3[ which(BL02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL02_TAMg3[ which(BL02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL02_TAMg2 <- rbind(BL02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
BL02_TAMft <- ftable(BL02_TAMg2$player1, BL02_TAMg2$player2)
BL02_TAMft2 <- as.matrix(BL02_TAMft)
numRows <- nrow(BL02_TAMft2)
numCols <- ncol(BL02_TAMft2)
BL02_TAMft3 <- BL02_TAMft2[c(2:numRows) , c(2:numCols)]
BL02_TAMTable <- graph.adjacency(BL02_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(BL02_TAMTable, vertex.label = V(BL02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
BL02_TAM.clusterCoef <- transitivity(BL02_TAMTable, type="global") #cluster coefficient
BL02_TAM.degreeCent <- centralization.degree(BL02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_TAMftn <- as.network.matrix(BL02_TAMft)
BL02_TAM.netDensity <- network.density(BL02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_TAM.entropy <- entropy(BL02_TAMft) #entropy

BL02_TAM.netMx <- cbind(BL02_TAM.netMx, BL02_TAM.clusterCoef, BL02_TAM.degreeCent$centralization,
                        BL02_TAM.netDensity, BL02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "BL"
KIoutcome = "Stoppage_DM"
BL02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
BL02_SDMg2 <- data.frame(BL02_SDM)
BL02_SDMg2 <- BL02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_SDMg2$player1
player2vector <- BL02_SDMg2$player2
BL02_SDMg3 <- BL02_SDMg2
BL02_SDMg3$p1inp2vec <- is.element(BL02_SDMg3$player1, player2vector)
BL02_SDMg3$p2inp1vec <- is.element(BL02_SDMg3$player2, player1vector)

addPlayer1 <- BL02_SDMg3[ which(BL02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL02_SDMg3[ which(BL02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL02_SDMg2 <- rbind(BL02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
BL02_SDMft <- ftable(BL02_SDMg2$player1, BL02_SDMg2$player2)
BL02_SDMft2 <- as.matrix(BL02_SDMft)
numRows <- nrow(BL02_SDMft2)
numCols <- ncol(BL02_SDMft2)
BL02_SDMft3 <- BL02_SDMft2[c(2:numRows) , c(2:numCols)]
BL02_SDMTable <- graph.adjacency(BL02_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(BL02_SDMTable, vertex.label = V(BL02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
BL02_SDM.clusterCoef <- transitivity(BL02_SDMTable, type="global") #cluster coefficient
BL02_SDM.degreeCent <- centralization.degree(BL02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_SDMftn <- as.network.matrix(BL02_SDMft)
BL02_SDM.netDensity <- network.density(BL02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_SDM.entropy <- entropy(BL02_SDMft) #entropy

BL02_SDM.netMx <- cbind(BL02_SDM.netMx, BL02_SDM.clusterCoef, BL02_SDM.degreeCent$centralization,
                        BL02_SDM.netDensity, BL02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "BL"
KIoutcome = "Turnover_DM"
BL02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
BL02_TDMg2 <- data.frame(BL02_TDM)
BL02_TDMg2 <- BL02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_TDMg2$player1
player2vector <- BL02_TDMg2$player2
BL02_TDMg3 <- BL02_TDMg2
BL02_TDMg3$p1inp2vec <- is.element(BL02_TDMg3$player1, player2vector)
BL02_TDMg3$p2inp1vec <- is.element(BL02_TDMg3$player2, player1vector)

addPlayer1 <- BL02_TDMg3[ which(BL02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL02_TDMg3[ which(BL02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL02_TDMg2 <- rbind(BL02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
BL02_TDMft <- ftable(BL02_TDMg2$player1, BL02_TDMg2$player2)
BL02_TDMft2 <- as.matrix(BL02_TDMft)
numRows <- nrow(BL02_TDMft2)
numCols <- ncol(BL02_TDMft2)
BL02_TDMft3 <- BL02_TDMft2[c(2:numRows) , c(2:numCols)]
BL02_TDMTable <- graph.adjacency(BL02_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(BL02_TDMTable, vertex.label = V(BL02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
BL02_TDM.clusterCoef <- transitivity(BL02_TDMTable, type="global") #cluster coefficient
BL02_TDM.degreeCent <- centralization.degree(BL02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_TDMftn <- as.network.matrix(BL02_TDMft)
BL02_TDM.netDensity <- network.density(BL02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_TDM.entropy <- entropy(BL02_TDMft) #entropy

BL02_TDM.netMx <- cbind(BL02_TDM.netMx, BL02_TDM.clusterCoef, BL02_TDM.degreeCent$centralization,
                        BL02_TDM.netDensity, BL02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "BL"
KIoutcome = "Stoppage_D"
BL02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
BL02_SDg2 <- data.frame(BL02_SD)
BL02_SDg2 <- BL02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_SDg2$player1
player2vector <- BL02_SDg2$player2
BL02_SDg3 <- BL02_SDg2
BL02_SDg3$p1inp2vec <- is.element(BL02_SDg3$player1, player2vector)
BL02_SDg3$p2inp1vec <- is.element(BL02_SDg3$player2, player1vector)

addPlayer1 <- BL02_SDg3[ which(BL02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL02_SDg3[ which(BL02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL02_SDg2 <- rbind(BL02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
BL02_SDft <- ftable(BL02_SDg2$player1, BL02_SDg2$player2)
BL02_SDft2 <- as.matrix(BL02_SDft)
numRows <- nrow(BL02_SDft2)
numCols <- ncol(BL02_SDft2)
BL02_SDft3 <- BL02_SDft2[c(2:numRows) , c(2:numCols)]
BL02_SDTable <- graph.adjacency(BL02_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(BL02_SDTable, vertex.label = V(BL02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
BL02_SD.clusterCoef <- transitivity(BL02_SDTable, type="global") #cluster coefficient
BL02_SD.degreeCent <- centralization.degree(BL02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_SDftn <- as.network.matrix(BL02_SDft)
BL02_SD.netDensity <- network.density(BL02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_SD.entropy <- entropy(BL02_SDft) #entropy

BL02_SD.netMx <- cbind(BL02_SD.netMx, BL02_SD.clusterCoef, BL02_SD.degreeCent$centralization,
                       BL02_SD.netDensity, BL02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "BL"
KIoutcome = "Turnover_D"
BL02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
BL02_TDg2 <- data.frame(BL02_TD)
BL02_TDg2 <- BL02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_TDg2$player1
player2vector <- BL02_TDg2$player2
BL02_TDg3 <- BL02_TDg2
BL02_TDg3$p1inp2vec <- is.element(BL02_TDg3$player1, player2vector)
BL02_TDg3$p2inp1vec <- is.element(BL02_TDg3$player2, player1vector)

addPlayer1 <- BL02_TDg3[ which(BL02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL02_TDg3[ which(BL02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL02_TDg2 <- rbind(BL02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
BL02_TDft <- ftable(BL02_TDg2$player1, BL02_TDg2$player2)
BL02_TDft2 <- as.matrix(BL02_TDft)
numRows <- nrow(BL02_TDft2)
numCols <- ncol(BL02_TDft2)
BL02_TDft3 <- BL02_TDft2[c(2:numRows) , c(2:numCols)]
BL02_TDTable <- graph.adjacency(BL02_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(BL02_TDTable, vertex.label = V(BL02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
BL02_TD.clusterCoef <- transitivity(BL02_TDTable, type="global") #cluster coefficient
BL02_TD.degreeCent <- centralization.degree(BL02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_TDftn <- as.network.matrix(BL02_TDft)
BL02_TD.netDensity <- network.density(BL02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_TD.entropy <- entropy(BL02_TDft) #entropy

BL02_TD.netMx <- cbind(BL02_TD.netMx, BL02_TD.clusterCoef, BL02_TD.degreeCent$centralization,
                       BL02_TD.netDensity, BL02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "BL"
KIoutcome = "End of Qtr_DM"
BL02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
BL02_QTg2 <- data.frame(BL02_QT)
BL02_QTg2 <- BL02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- BL02_QTg2$player1
player2vector <- BL02_QTg2$player2
BL02_QTg3 <- BL02_QTg2
BL02_QTg3$p1inp2vec <- is.element(BL02_QTg3$player1, player2vector)
BL02_QTg3$p2inp1vec <- is.element(BL02_QTg3$player2, player1vector)

addPlayer1 <- BL02_QTg3[ which(BL02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- BL02_QTg3[ which(BL02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

BL02_QTg2 <- rbind(BL02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
BL02_QTft <- ftable(BL02_QTg2$player1, BL02_QTg2$player2)
BL02_QTft2 <- as.matrix(BL02_QTft)
numRows <- nrow(BL02_QTft2)
numCols <- ncol(BL02_QTft2)
BL02_QTft3 <- BL02_QTft2[c(2:numRows) , c(2:numCols)]
BL02_QTTable <- graph.adjacency(BL02_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(BL02_QTTable, vertex.label = V(BL02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(BL02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
BL02_QT.clusterCoef <- transitivity(BL02_QTTable, type="global") #cluster coefficient
BL02_QT.degreeCent <- centralization.degree(BL02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
BL02_QTftn <- as.network.matrix(BL02_QTft)
BL02_QT.netDensity <- network.density(BL02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
BL02_QT.entropy <- entropy(BL02_QTft) #entropy

BL02_QT.netMx <- cbind(BL02_QT.netMx, BL02_QT.clusterCoef, BL02_QT.degreeCent$centralization,
                       BL02_QT.netDensity, BL02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(BL02_QT.netMx) <- varnames

#############################################################################
#CARLTON

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "CARL"
KIoutcome = "Goal_F"
CARL02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
CARL02_Gg2 <- data.frame(CARL02_G)
CARL02_Gg2 <- CARL02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_Gg2$player1
player2vector <- CARL02_Gg2$player2
CARL02_Gg3 <- CARL02_Gg2
CARL02_Gg3$p1inp2vec <- is.element(CARL02_Gg3$player1, player2vector)
CARL02_Gg3$p2inp1vec <- is.element(CARL02_Gg3$player2, player1vector)

addPlayer1 <- CARL02_Gg3[ which(CARL02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_Gg3[ which(CARL02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_Gg2 <- rbind(CARL02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
CARL02_Gft <- ftable(CARL02_Gg2$player1, CARL02_Gg2$player2)
CARL02_Gft2 <- as.matrix(CARL02_Gft)
numRows <- nrow(CARL02_Gft2)
numCols <- ncol(CARL02_Gft2)
CARL02_Gft3 <- CARL02_Gft2[c(2:numRows) , c(2:numCols)]
CARL02_GTable <- graph.adjacency(CARL02_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(CARL02_GTable, vertex.label = V(CARL02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
CARL02_G.clusterCoef <- transitivity(CARL02_GTable, type="global") #cluster coefficient
CARL02_G.degreeCent <- centralization.degree(CARL02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_Gftn <- as.network.matrix(CARL02_Gft)
CARL02_G.netDensity <- network.density(CARL02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_G.entropy <- entropy(CARL02_Gft) #entropy

CARL02_G.netMx <- cbind(CARL02_G.netMx, CARL02_G.clusterCoef, CARL02_G.degreeCent$centralization,
                        CARL02_G.netDensity, CARL02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "CARL"
KIoutcome = "Behind_F"
CARL02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
CARL02_Bg2 <- data.frame(CARL02_B)
CARL02_Bg2 <- CARL02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_Bg2$player1
player2vector <- CARL02_Bg2$player2
CARL02_Bg3 <- CARL02_Bg2
CARL02_Bg3$p1inp2vec <- is.element(CARL02_Bg3$player1, player2vector)
CARL02_Bg3$p2inp1vec <- is.element(CARL02_Bg3$player2, player1vector)

addPlayer1 <- CARL02_Bg3[ which(CARL02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_Bg3[ which(CARL02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_Bg2 <- rbind(CARL02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
CARL02_Bft <- ftable(CARL02_Bg2$player1, CARL02_Bg2$player2)
CARL02_Bft2 <- as.matrix(CARL02_Bft)
numRows <- nrow(CARL02_Bft2)
numCols <- ncol(CARL02_Bft2)
CARL02_Bft3 <- CARL02_Bft2[c(2:numRows) , c(2:numCols)]
CARL02_BTable <- graph.adjacency(CARL02_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(CARL02_BTable, vertex.label = V(CARL02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
CARL02_B.clusterCoef <- transitivity(CARL02_BTable, type="global") #cluster coefficient
CARL02_B.degreeCent <- centralization.degree(CARL02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_Bftn <- as.network.matrix(CARL02_Bft)
CARL02_B.netDensity <- network.density(CARL02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_B.entropy <- entropy(CARL02_Bft) #entropy

CARL02_B.netMx <- cbind(CARL02_B.netMx, CARL02_B.clusterCoef, CARL02_B.degreeCent$centralization,
                        CARL02_B.netDensity, CARL02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************

round = 2
teamName = "CARL"
KIoutcome = "Stoppage_F"
CARL02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
CARL02_SFg2 <- data.frame(CARL02_SF)
CARL02_SFg2 <- CARL02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_SFg2$player1
player2vector <- CARL02_SFg2$player2
CARL02_SFg3 <- CARL02_SFg2
CARL02_SFg3$p1inp2vec <- is.element(CARL02_SFg3$player1, player2vector)
CARL02_SFg3$p2inp1vec <- is.element(CARL02_SFg3$player2, player1vector)

addPlayer1 <- CARL02_SFg3[ which(CARL02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_SFg3[ which(CARL02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_SFg2 <- rbind(CARL02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
CARL02_SFft <- ftable(CARL02_SFg2$player1, CARL02_SFg2$player2)
CARL02_SFft2 <- as.matrix(CARL02_SFft)
numRows <- nrow(CARL02_SFft2)
numCols <- ncol(CARL02_SFft2)
CARL02_SFft3 <- CARL02_SFft2[c(2:numRows) , c(2:numCols)]
CARL02_SFTable <- graph.adjacency(CARL02_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(CARL02_SFTable, vertex.label = V(CARL02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
CARL02_SF.clusterCoef <- transitivity(CARL02_SFTable, type="global") #cluster coefficient
CARL02_SF.degreeCent <- centralization.degree(CARL02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_SFftn <- as.network.matrix(CARL02_SFft)
CARL02_SF.netDensity <- network.density(CARL02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_SF.entropy <- entropy(CARL02_SFft) #entropy

CARL02_SF.netMx <- cbind(CARL02_SF.netMx, CARL02_SF.clusterCoef, CARL02_SF.degreeCent$centralization,
                         CARL02_SF.netDensity, CARL02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************
#NA

round = 2
teamName = "CARL"
KIoutcome = "Turnover_F"
CARL02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
CARL02_TFg2 <- data.frame(CARL02_TF)
CARL02_TFg2 <- CARL02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_TFg2$player1
player2vector <- CARL02_TFg2$player2
CARL02_TFg3 <- CARL02_TFg2
CARL02_TFg3$p1inp2vec <- is.element(CARL02_TFg3$player1, player2vector)
CARL02_TFg3$p2inp1vec <- is.element(CARL02_TFg3$player2, player1vector)

addPlayer1 <- CARL02_TFg3[ which(CARL02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_TFg3[ which(CARL02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_TFg2 <- rbind(CARL02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
CARL02_TFft <- ftable(CARL02_TFg2$player1, CARL02_TFg2$player2)
CARL02_TFft2 <- as.matrix(CARL02_TFft)
numRows <- nrow(CARL02_TFft2)
numCols <- ncol(CARL02_TFft2)
CARL02_TFft3 <- CARL02_TFft2[c(2:numRows) , c(2:numCols)]
CARL02_TFTable <- graph.adjacency(CARL02_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(CARL02_TFTable, vertex.label = V(CARL02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
CARL02_TF.clusterCoef <- transitivity(CARL02_TFTable, type="global") #cluster coefficient
CARL02_TF.degreeCent <- centralization.degree(CARL02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_TFftn <- as.network.matrix(CARL02_TFft)
CARL02_TF.netDensity <- network.density(CARL02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_TF.entropy <- entropy(CARL02_TFft) #entropy

CARL02_TF.netMx <- cbind(CARL02_TF.netMx, CARL02_TF.clusterCoef, CARL02_TF.degreeCent$centralization,
                         CARL02_TF.netDensity, CARL02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************

round = 2
teamName = "CARL"
KIoutcome = "Stoppage_AM"
CARL02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
CARL02_SAMg2 <- data.frame(CARL02_SAM)
CARL02_SAMg2 <- CARL02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_SAMg2$player1
player2vector <- CARL02_SAMg2$player2
CARL02_SAMg3 <- CARL02_SAMg2
CARL02_SAMg3$p1inp2vec <- is.element(CARL02_SAMg3$player1, player2vector)
CARL02_SAMg3$p2inp1vec <- is.element(CARL02_SAMg3$player2, player1vector)

addPlayer1 <- CARL02_SAMg3[ which(CARL02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_SAMg3[ which(CARL02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_SAMg2 <- rbind(CARL02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
CARL02_SAMft <- ftable(CARL02_SAMg2$player1, CARL02_SAMg2$player2)
CARL02_SAMft2 <- as.matrix(CARL02_SAMft)
numRows <- nrow(CARL02_SAMft2)
numCols <- ncol(CARL02_SAMft2)
CARL02_SAMft3 <- CARL02_SAMft2[c(2:numRows) , c(2:numCols)]
CARL02_SAMTable <- graph.adjacency(CARL02_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(CARL02_SAMTable, vertex.label = V(CARL02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
CARL02_SAM.clusterCoef <- transitivity(CARL02_SAMTable, type="global") #cluster coefficient
CARL02_SAM.degreeCent <- centralization.degree(CARL02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_SAMftn <- as.network.matrix(CARL02_SAMft)
CARL02_SAM.netDensity <- network.density(CARL02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_SAM.entropy <- entropy(CARL02_SAMft) #entropy

CARL02_SAM.netMx <- cbind(CARL02_SAM.netMx, CARL02_SAM.clusterCoef, CARL02_SAM.degreeCent$centralization,
                          CARL02_SAM.netDensity, CARL02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************
#NA

round = 2
teamName = "CARL"
KIoutcome = "Turnover_AM"
CARL02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
CARL02_TAMg2 <- data.frame(CARL02_TAM)
CARL02_TAMg2 <- CARL02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_TAMg2$player1
player2vector <- CARL02_TAMg2$player2
CARL02_TAMg3 <- CARL02_TAMg2
CARL02_TAMg3$p1inp2vec <- is.element(CARL02_TAMg3$player1, player2vector)
CARL02_TAMg3$p2inp1vec <- is.element(CARL02_TAMg3$player2, player1vector)

addPlayer1 <- CARL02_TAMg3[ which(CARL02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_TAMg3[ which(CARL02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_TAMg2 <- rbind(CARL02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
CARL02_TAMft <- ftable(CARL02_TAMg2$player1, CARL02_TAMg2$player2)
CARL02_TAMft2 <- as.matrix(CARL02_TAMft)
numRows <- nrow(CARL02_TAMft2)
numCols <- ncol(CARL02_TAMft2)
CARL02_TAMft3 <- CARL02_TAMft2[c(2:numRows) , c(2:numCols)]
CARL02_TAMTable <- graph.adjacency(CARL02_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(CARL02_TAMTable, vertex.label = V(CARL02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
CARL02_TAM.clusterCoef <- transitivity(CARL02_TAMTable, type="global") #cluster coefficient
CARL02_TAM.degreeCent <- centralization.degree(CARL02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_TAMftn <- as.network.matrix(CARL02_TAMft)
CARL02_TAM.netDensity <- network.density(CARL02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_TAM.entropy <- entropy(CARL02_TAMft) #entropy

CARL02_TAM.netMx <- cbind(CARL02_TAM.netMx, CARL02_TAM.clusterCoef, CARL02_TAM.degreeCent$centralization,
                          CARL02_TAM.netDensity, CARL02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "CARL"
KIoutcome = "Stoppage_DM"
CARL02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
CARL02_SDMg2 <- data.frame(CARL02_SDM)
CARL02_SDMg2 <- CARL02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_SDMg2$player1
player2vector <- CARL02_SDMg2$player2
CARL02_SDMg3 <- CARL02_SDMg2
CARL02_SDMg3$p1inp2vec <- is.element(CARL02_SDMg3$player1, player2vector)
CARL02_SDMg3$p2inp1vec <- is.element(CARL02_SDMg3$player2, player1vector)

addPlayer1 <- CARL02_SDMg3[ which(CARL02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_SDMg3[ which(CARL02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_SDMg2 <- rbind(CARL02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
CARL02_SDMft <- ftable(CARL02_SDMg2$player1, CARL02_SDMg2$player2)
CARL02_SDMft2 <- as.matrix(CARL02_SDMft)
numRows <- nrow(CARL02_SDMft2)
numCols <- ncol(CARL02_SDMft2)
CARL02_SDMft3 <- CARL02_SDMft2[c(2:numRows) , c(2:numCols)]
CARL02_SDMTable <- graph.adjacency(CARL02_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(CARL02_SDMTable, vertex.label = V(CARL02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
CARL02_SDM.clusterCoef <- transitivity(CARL02_SDMTable, type="global") #cluster coefficient
CARL02_SDM.degreeCent <- centralization.degree(CARL02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_SDMftn <- as.network.matrix(CARL02_SDMft)
CARL02_SDM.netDensity <- network.density(CARL02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_SDM.entropy <- entropy(CARL02_SDMft) #entropy

CARL02_SDM.netMx <- cbind(CARL02_SDM.netMx, CARL02_SDM.clusterCoef, CARL02_SDM.degreeCent$centralization,
                          CARL02_SDM.netDensity, CARL02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "CARL"
KIoutcome = "Turnover_DM"
CARL02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
CARL02_TDMg2 <- data.frame(CARL02_TDM)
CARL02_TDMg2 <- CARL02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_TDMg2$player1
player2vector <- CARL02_TDMg2$player2
CARL02_TDMg3 <- CARL02_TDMg2
CARL02_TDMg3$p1inp2vec <- is.element(CARL02_TDMg3$player1, player2vector)
CARL02_TDMg3$p2inp1vec <- is.element(CARL02_TDMg3$player2, player1vector)

addPlayer1 <- CARL02_TDMg3[ which(CARL02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_TDMg3[ which(CARL02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_TDMg2 <- rbind(CARL02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
CARL02_TDMft <- ftable(CARL02_TDMg2$player1, CARL02_TDMg2$player2)
CARL02_TDMft2 <- as.matrix(CARL02_TDMft)
numRows <- nrow(CARL02_TDMft2)
numCols <- ncol(CARL02_TDMft2)
CARL02_TDMft3 <- CARL02_TDMft2[c(2:numRows) , c(2:numCols)]
CARL02_TDMTable <- graph.adjacency(CARL02_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(CARL02_TDMTable, vertex.label = V(CARL02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
CARL02_TDM.clusterCoef <- transitivity(CARL02_TDMTable, type="global") #cluster coefficient
CARL02_TDM.degreeCent <- centralization.degree(CARL02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_TDMftn <- as.network.matrix(CARL02_TDMft)
CARL02_TDM.netDensity <- network.density(CARL02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_TDM.entropy <- entropy(CARL02_TDMft) #entropy

CARL02_TDM.netMx <- cbind(CARL02_TDM.netMx, CARL02_TDM.clusterCoef, CARL02_TDM.degreeCent$centralization,
                          CARL02_TDM.netDensity, CARL02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************

round = 2
teamName = "CARL"
KIoutcome = "Stoppage_D"
CARL02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
CARL02_SDg2 <- data.frame(CARL02_SD)
CARL02_SDg2 <- CARL02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_SDg2$player1
player2vector <- CARL02_SDg2$player2
CARL02_SDg3 <- CARL02_SDg2
CARL02_SDg3$p1inp2vec <- is.element(CARL02_SDg3$player1, player2vector)
CARL02_SDg3$p2inp1vec <- is.element(CARL02_SDg3$player2, player1vector)

addPlayer1 <- CARL02_SDg3[ which(CARL02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_SDg3[ which(CARL02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_SDg2 <- rbind(CARL02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
CARL02_SDft <- ftable(CARL02_SDg2$player1, CARL02_SDg2$player2)
CARL02_SDft2 <- as.matrix(CARL02_SDft)
numRows <- nrow(CARL02_SDft2)
numCols <- ncol(CARL02_SDft2)
CARL02_SDft3 <- CARL02_SDft2[c(2:numRows) , c(2:numCols)]
CARL02_SDTable <- graph.adjacency(CARL02_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(CARL02_SDTable, vertex.label = V(CARL02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
CARL02_SD.clusterCoef <- transitivity(CARL02_SDTable, type="global") #cluster coefficient
CARL02_SD.degreeCent <- centralization.degree(CARL02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_SDftn <- as.network.matrix(CARL02_SDft)
CARL02_SD.netDensity <- network.density(CARL02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_SD.entropy <- entropy(CARL02_SDft) #entropy

CARL02_SD.netMx <- cbind(CARL02_SD.netMx, CARL02_SD.clusterCoef, CARL02_SD.degreeCent$centralization,
                         CARL02_SD.netDensity, CARL02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "CARL"
KIoutcome = "Turnover_D"
CARL02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
CARL02_TDg2 <- data.frame(CARL02_TD)
CARL02_TDg2 <- CARL02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_TDg2$player1
player2vector <- CARL02_TDg2$player2
CARL02_TDg3 <- CARL02_TDg2
CARL02_TDg3$p1inp2vec <- is.element(CARL02_TDg3$player1, player2vector)
CARL02_TDg3$p2inp1vec <- is.element(CARL02_TDg3$player2, player1vector)

addPlayer1 <- CARL02_TDg3[ which(CARL02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_TDg3[ which(CARL02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_TDg2 <- rbind(CARL02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
CARL02_TDft <- ftable(CARL02_TDg2$player1, CARL02_TDg2$player2)
CARL02_TDft2 <- as.matrix(CARL02_TDft)
numRows <- nrow(CARL02_TDft2)
numCols <- ncol(CARL02_TDft2)
CARL02_TDft3 <- CARL02_TDft2[c(2:numRows) , c(2:numCols)]
CARL02_TDTable <- graph.adjacency(CARL02_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(CARL02_TDTable, vertex.label = V(CARL02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
CARL02_TD.clusterCoef <- transitivity(CARL02_TDTable, type="global") #cluster coefficient
CARL02_TD.degreeCent <- centralization.degree(CARL02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_TDftn <- as.network.matrix(CARL02_TDft)
CARL02_TD.netDensity <- network.density(CARL02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_TD.entropy <- entropy(CARL02_TDft) #entropy

CARL02_TD.netMx <- cbind(CARL02_TD.netMx, CARL02_TD.clusterCoef, CARL02_TD.degreeCent$centralization,
                         CARL02_TD.netDensity, CARL02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "CARL"
KIoutcome = "End of Qtr_DM"
CARL02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
CARL02_QTg2 <- data.frame(CARL02_QT)
CARL02_QTg2 <- CARL02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- CARL02_QTg2$player1
player2vector <- CARL02_QTg2$player2
CARL02_QTg3 <- CARL02_QTg2
CARL02_QTg3$p1inp2vec <- is.element(CARL02_QTg3$player1, player2vector)
CARL02_QTg3$p2inp1vec <- is.element(CARL02_QTg3$player2, player1vector)

addPlayer1 <- CARL02_QTg3[ which(CARL02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- CARL02_QTg3[ which(CARL02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

CARL02_QTg2 <- rbind(CARL02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
CARL02_QTft <- ftable(CARL02_QTg2$player1, CARL02_QTg2$player2)
CARL02_QTft2 <- as.matrix(CARL02_QTft)
numRows <- nrow(CARL02_QTft2)
numCols <- ncol(CARL02_QTft2)
CARL02_QTft3 <- CARL02_QTft2[c(2:numRows) , c(2:numCols)]
CARL02_QTTable <- graph.adjacency(CARL02_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(CARL02_QTTable, vertex.label = V(CARL02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(CARL02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
CARL02_QT.clusterCoef <- transitivity(CARL02_QTTable, type="global") #cluster coefficient
CARL02_QT.degreeCent <- centralization.degree(CARL02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
CARL02_QTftn <- as.network.matrix(CARL02_QTft)
CARL02_QT.netDensity <- network.density(CARL02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
CARL02_QT.entropy <- entropy(CARL02_QTft) #entropy

CARL02_QT.netMx <- cbind(CARL02_QT.netMx, CARL02_QT.clusterCoef, CARL02_QT.degreeCent$centralization,
                         CARL02_QT.netDensity, CARL02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(CARL02_QT.netMx) <- varnames

#############################################################################
#COLLINGWOOD

##
#ROUND 2
##

#Round 2, Goal***************************************************************

round = 2
teamName = "COLL"
KIoutcome = "Goal_F"
COLL02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
COLL02_Gg2 <- data.frame(COLL02_G)
COLL02_Gg2 <- COLL02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_Gg2$player1
player2vector <- COLL02_Gg2$player2
COLL02_Gg3 <- COLL02_Gg2
COLL02_Gg3$p1inp2vec <- is.element(COLL02_Gg3$player1, player2vector)
COLL02_Gg3$p2inp1vec <- is.element(COLL02_Gg3$player2, player1vector)

addPlayer1 <- COLL02_Gg3[ which(COLL02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_Gg3[ which(COLL02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_Gg2 <- rbind(COLL02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
COLL02_Gft <- ftable(COLL02_Gg2$player1, COLL02_Gg2$player2)
COLL02_Gft2 <- as.matrix(COLL02_Gft)
numRows <- nrow(COLL02_Gft2)
numCols <- ncol(COLL02_Gft2)
COLL02_Gft3 <- COLL02_Gft2[c(2:numRows) , c(2:numCols)]
COLL02_GTable <- graph.adjacency(COLL02_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(COLL02_GTable, vertex.label = V(COLL02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
COLL02_G.clusterCoef <- transitivity(COLL02_GTable, type="global") #cluster coefficient
COLL02_G.degreeCent <- centralization.degree(COLL02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_Gftn <- as.network.matrix(COLL02_Gft)
COLL02_G.netDensity <- network.density(COLL02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_G.entropy <- entropy(COLL02_Gft) #entropy

COLL02_G.netMx <- cbind(COLL02_G.netMx, COLL02_G.clusterCoef, COLL02_G.degreeCent$centralization,
                        COLL02_G.netDensity, COLL02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "COLL"
KIoutcome = "Behind_F"
COLL02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
COLL02_Bg2 <- data.frame(COLL02_B)
COLL02_Bg2 <- COLL02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_Bg2$player1
player2vector <- COLL02_Bg2$player2
COLL02_Bg3 <- COLL02_Bg2
COLL02_Bg3$p1inp2vec <- is.element(COLL02_Bg3$player1, player2vector)
COLL02_Bg3$p2inp1vec <- is.element(COLL02_Bg3$player2, player1vector)

addPlayer1 <- COLL02_Bg3[ which(COLL02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_Bg3[ which(COLL02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_Bg2 <- rbind(COLL02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
COLL02_Bft <- ftable(COLL02_Bg2$player1, COLL02_Bg2$player2)
COLL02_Bft2 <- as.matrix(COLL02_Bft)
numRows <- nrow(COLL02_Bft2)
numCols <- ncol(COLL02_Bft2)
COLL02_Bft3 <- COLL02_Bft2[c(2:numRows) , c(2:numCols)]
COLL02_BTable <- graph.adjacency(COLL02_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(COLL02_BTable, vertex.label = V(COLL02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
COLL02_B.clusterCoef <- transitivity(COLL02_BTable, type="global") #cluster coefficient
COLL02_B.degreeCent <- centralization.degree(COLL02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_Bftn <- as.network.matrix(COLL02_Bft)
COLL02_B.netDensity <- network.density(COLL02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_B.entropy <- entropy(COLL02_Bft) #entropy

COLL02_B.netMx <- cbind(COLL02_B.netMx, COLL02_B.clusterCoef, COLL02_B.degreeCent$centralization,
                        COLL02_B.netDensity, COLL02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************

round = 2
teamName = "COLL"
KIoutcome = "Stoppage_F"
COLL02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
COLL02_SFg2 <- data.frame(COLL02_SF)
COLL02_SFg2 <- COLL02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_SFg2$player1
player2vector <- COLL02_SFg2$player2
COLL02_SFg3 <- COLL02_SFg2
COLL02_SFg3$p1inp2vec <- is.element(COLL02_SFg3$player1, player2vector)
COLL02_SFg3$p2inp1vec <- is.element(COLL02_SFg3$player2, player1vector)

addPlayer1 <- COLL02_SFg3[ which(COLL02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_SFg3[ which(COLL02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_SFg2 <- rbind(COLL02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
COLL02_SFft <- ftable(COLL02_SFg2$player1, COLL02_SFg2$player2)
COLL02_SFft2 <- as.matrix(COLL02_SFft)
numRows <- nrow(COLL02_SFft2)
numCols <- ncol(COLL02_SFft2)
COLL02_SFft3 <- COLL02_SFft2[c(2:numRows) , c(2:numCols)]
COLL02_SFTable <- graph.adjacency(COLL02_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(COLL02_SFTable, vertex.label = V(COLL02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
COLL02_SF.clusterCoef <- transitivity(COLL02_SFTable, type="global") #cluster coefficient
COLL02_SF.degreeCent <- centralization.degree(COLL02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_SFftn <- as.network.matrix(COLL02_SFft)
COLL02_SF.netDensity <- network.density(COLL02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_SF.entropy <- entropy(COLL02_SFft) #entropy

COLL02_SF.netMx <- cbind(COLL02_SF.netMx, COLL02_SF.clusterCoef, COLL02_SF.degreeCent$centralization,
                         COLL02_SF.netDensity, COLL02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "COLL"
KIoutcome = "Turnover_F"
COLL02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
COLL02_TFg2 <- data.frame(COLL02_TF)
COLL02_TFg2 <- COLL02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_TFg2$player1
player2vector <- COLL02_TFg2$player2
COLL02_TFg3 <- COLL02_TFg2
COLL02_TFg3$p1inp2vec <- is.element(COLL02_TFg3$player1, player2vector)
COLL02_TFg3$p2inp1vec <- is.element(COLL02_TFg3$player2, player1vector)

addPlayer1 <- COLL02_TFg3[ which(COLL02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_TFg3[ which(COLL02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_TFg2 <- rbind(COLL02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
COLL02_TFft <- ftable(COLL02_TFg2$player1, COLL02_TFg2$player2)
COLL02_TFft2 <- as.matrix(COLL02_TFft)
numRows <- nrow(COLL02_TFft2)
numCols <- ncol(COLL02_TFft2)
COLL02_TFft3 <- COLL02_TFft2[c(2:numRows) , c(2:numCols)]
COLL02_TFTable <- graph.adjacency(COLL02_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(COLL02_TFTable, vertex.label = V(COLL02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
COLL02_TF.clusterCoef <- transitivity(COLL02_TFTable, type="global") #cluster coefficient
COLL02_TF.degreeCent <- centralization.degree(COLL02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_TFftn <- as.network.matrix(COLL02_TFft)
COLL02_TF.netDensity <- network.density(COLL02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_TF.entropy <- entropy(COLL02_TFft) #entropy

COLL02_TF.netMx <- cbind(COLL02_TF.netMx, COLL02_TF.clusterCoef, COLL02_TF.degreeCent$centralization,
                         COLL02_TF.netDensity, COLL02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************

round = 2
teamName = "COLL"
KIoutcome = "Stoppage_AM"
COLL02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
COLL02_SAMg2 <- data.frame(COLL02_SAM)
COLL02_SAMg2 <- COLL02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_SAMg2$player1
player2vector <- COLL02_SAMg2$player2
COLL02_SAMg3 <- COLL02_SAMg2
COLL02_SAMg3$p1inp2vec <- is.element(COLL02_SAMg3$player1, player2vector)
COLL02_SAMg3$p2inp1vec <- is.element(COLL02_SAMg3$player2, player1vector)

addPlayer1 <- COLL02_SAMg3[ which(COLL02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_SAMg3[ which(COLL02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_SAMg2 <- rbind(COLL02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
COLL02_SAMft <- ftable(COLL02_SAMg2$player1, COLL02_SAMg2$player2)
COLL02_SAMft2 <- as.matrix(COLL02_SAMft)
numRows <- nrow(COLL02_SAMft2)
numCols <- ncol(COLL02_SAMft2)
COLL02_SAMft3 <- COLL02_SAMft2[c(2:numRows) , c(2:numCols)]
COLL02_SAMTable <- graph.adjacency(COLL02_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(COLL02_SAMTable, vertex.label = V(COLL02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
COLL02_SAM.clusterCoef <- transitivity(COLL02_SAMTable, type="global") #cluster coefficient
COLL02_SAM.degreeCent <- centralization.degree(COLL02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_SAMftn <- as.network.matrix(COLL02_SAMft)
COLL02_SAM.netDensity <- network.density(COLL02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_SAM.entropy <- entropy(COLL02_SAMft) #entropy

COLL02_SAM.netMx <- cbind(COLL02_SAM.netMx, COLL02_SAM.clusterCoef, COLL02_SAM.degreeCent$centralization,
                          COLL02_SAM.netDensity, COLL02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "COLL"
KIoutcome = "Turnover_AM"
COLL02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
COLL02_TAMg2 <- data.frame(COLL02_TAM)
COLL02_TAMg2 <- COLL02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_TAMg2$player1
player2vector <- COLL02_TAMg2$player2
COLL02_TAMg3 <- COLL02_TAMg2
COLL02_TAMg3$p1inp2vec <- is.element(COLL02_TAMg3$player1, player2vector)
COLL02_TAMg3$p2inp1vec <- is.element(COLL02_TAMg3$player2, player1vector)

addPlayer1 <- COLL02_TAMg3[ which(COLL02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, empty, zero)
addPlayer2 <- COLL02_TAMg3[ which(COLL02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_TAMg2 <- rbind(COLL02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
COLL02_TAMft <- ftable(COLL02_TAMg2$player1, COLL02_TAMg2$player2)
COLL02_TAMft2 <- as.matrix(COLL02_TAMft)
numRows <- nrow(COLL02_TAMft2)
numCols <- ncol(COLL02_TAMft2)
COLL02_TAMft3 <- COLL02_TAMft2[c(2:numRows) , c(2:numCols)]
COLL02_TAMTable <- graph.adjacency(COLL02_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(COLL02_TAMTable, vertex.label = V(COLL02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
COLL02_TAM.clusterCoef <- transitivity(COLL02_TAMTable, type="global") #cluster coefficient
COLL02_TAM.degreeCent <- centralization.degree(COLL02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_TAMftn <- as.network.matrix(COLL02_TAMft)
COLL02_TAM.netDensity <- network.density(COLL02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_TAM.entropy <- entropy(COLL02_TAMft) #entropy

COLL02_TAM.netMx <- cbind(COLL02_TAM.netMx, COLL02_TAM.clusterCoef, COLL02_TAM.degreeCent$centralization,
                          COLL02_TAM.netDensity, COLL02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "COLL"
KIoutcome = "Stoppage_DM"
COLL02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
COLL02_SDMg2 <- data.frame(COLL02_SDM)
COLL02_SDMg2 <- COLL02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_SDMg2$player1
player2vector <- COLL02_SDMg2$player2
COLL02_SDMg3 <- COLL02_SDMg2
COLL02_SDMg3$p1inp2vec <- is.element(COLL02_SDMg3$player1, player2vector)
COLL02_SDMg3$p2inp1vec <- is.element(COLL02_SDMg3$player2, player1vector)

addPlayer1 <- COLL02_SDMg3[ which(COLL02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_SDMg3[ which(COLL02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_SDMg2 <- rbind(COLL02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
COLL02_SDMft <- ftable(COLL02_SDMg2$player1, COLL02_SDMg2$player2)
COLL02_SDMft2 <- as.matrix(COLL02_SDMft)
numRows <- nrow(COLL02_SDMft2)
numCols <- ncol(COLL02_SDMft2)
COLL02_SDMft3 <- COLL02_SDMft2[c(2:numRows) , c(2:numCols)]
COLL02_SDMTable <- graph.adjacency(COLL02_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(COLL02_SDMTable, vertex.label = V(COLL02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
COLL02_SDM.clusterCoef <- transitivity(COLL02_SDMTable, type="global") #cluster coefficient
COLL02_SDM.degreeCent <- centralization.degree(COLL02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_SDMftn <- as.network.matrix(COLL02_SDMft)
COLL02_SDM.netDensity <- network.density(COLL02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_SDM.entropy <- entropy(COLL02_SDMft) #entropy

COLL02_SDM.netMx <- cbind(COLL02_SDM.netMx, COLL02_SDM.clusterCoef, COLL02_SDM.degreeCent$centralization,
                          COLL02_SDM.netDensity, COLL02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "COLL"
KIoutcome = "Turnover_DM"
COLL02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
COLL02_TDMg2 <- data.frame(COLL02_TDM)
COLL02_TDMg2 <- COLL02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_TDMg2$player1
player2vector <- COLL02_TDMg2$player2
COLL02_TDMg3 <- COLL02_TDMg2
COLL02_TDMg3$p1inp2vec <- is.element(COLL02_TDMg3$player1, player2vector)
COLL02_TDMg3$p2inp1vec <- is.element(COLL02_TDMg3$player2, player1vector)

addPlayer1 <- COLL02_TDMg3[ which(COLL02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_TDMg3[ which(COLL02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_TDMg2 <- rbind(COLL02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
COLL02_TDMft <- ftable(COLL02_TDMg2$player1, COLL02_TDMg2$player2)
COLL02_TDMft2 <- as.matrix(COLL02_TDMft)
numRows <- nrow(COLL02_TDMft2)
numCols <- ncol(COLL02_TDMft2)
COLL02_TDMft3 <- COLL02_TDMft2[c(2:numRows) , c(2:numCols)]
COLL02_TDMTable <- graph.adjacency(COLL02_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(COLL02_TDMTable, vertex.label = V(COLL02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
COLL02_TDM.clusterCoef <- transitivity(COLL02_TDMTable, type="global") #cluster coefficient
COLL02_TDM.degreeCent <- centralization.degree(COLL02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_TDMftn <- as.network.matrix(COLL02_TDMft)
COLL02_TDM.netDensity <- network.density(COLL02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_TDM.entropy <- entropy(COLL02_TDMft) #entropy

COLL02_TDM.netMx <- cbind(COLL02_TDM.netMx, COLL02_TDM.clusterCoef, COLL02_TDM.degreeCent$centralization,
                          COLL02_TDM.netDensity, COLL02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "COLL"
KIoutcome = "Stoppage_D"
COLL02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
COLL02_SDg2 <- data.frame(COLL02_SD)
COLL02_SDg2 <- COLL02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_SDg2$player1
player2vector <- COLL02_SDg2$player2
COLL02_SDg3 <- COLL02_SDg2
COLL02_SDg3$p1inp2vec <- is.element(COLL02_SDg3$player1, player2vector)
COLL02_SDg3$p2inp1vec <- is.element(COLL02_SDg3$player2, player1vector)

addPlayer1 <- COLL02_SDg3[ which(COLL02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_SDg3[ which(COLL02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_SDg2 <- rbind(COLL02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
COLL02_SDft <- ftable(COLL02_SDg2$player1, COLL02_SDg2$player2)
COLL02_SDft2 <- as.matrix(COLL02_SDft)
numRows <- nrow(COLL02_SDft2)
numCols <- ncol(COLL02_SDft2)
COLL02_SDft3 <- COLL02_SDft2[c(2:numRows) , c(2:numCols)]
COLL02_SDTable <- graph.adjacency(COLL02_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(COLL02_SDTable, vertex.label = V(COLL02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
COLL02_SD.clusterCoef <- transitivity(COLL02_SDTable, type="global") #cluster coefficient
COLL02_SD.degreeCent <- centralization.degree(COLL02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_SDftn <- as.network.matrix(COLL02_SDft)
COLL02_SD.netDensity <- network.density(COLL02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_SD.entropy <- entropy(COLL02_SDft) #entropy

COLL02_SD.netMx <- cbind(COLL02_SD.netMx, COLL02_SD.clusterCoef, COLL02_SD.degreeCent$centralization,
                         COLL02_SD.netDensity, COLL02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "COLL"
KIoutcome = "Turnover_D"
COLL02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
COLL02_TDg2 <- data.frame(COLL02_TD)
COLL02_TDg2 <- COLL02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_TDg2$player1
player2vector <- COLL02_TDg2$player2
COLL02_TDg3 <- COLL02_TDg2
COLL02_TDg3$p1inp2vec <- is.element(COLL02_TDg3$player1, player2vector)
COLL02_TDg3$p2inp1vec <- is.element(COLL02_TDg3$player2, player1vector)

addPlayer1 <- COLL02_TDg3[ which(COLL02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_TDg3[ which(COLL02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_TDg2 <- rbind(COLL02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
COLL02_TDft <- ftable(COLL02_TDg2$player1, COLL02_TDg2$player2)
COLL02_TDft2 <- as.matrix(COLL02_TDft)
numRows <- nrow(COLL02_TDft2)
numCols <- ncol(COLL02_TDft2)
COLL02_TDft3 <- COLL02_TDft2[c(2:numRows) , c(2:numCols)]
COLL02_TDTable <- graph.adjacency(COLL02_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(COLL02_TDTable, vertex.label = V(COLL02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
COLL02_TD.clusterCoef <- transitivity(COLL02_TDTable, type="global") #cluster coefficient
COLL02_TD.degreeCent <- centralization.degree(COLL02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_TDftn <- as.network.matrix(COLL02_TDft)
COLL02_TD.netDensity <- network.density(COLL02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_TD.entropy <- entropy(COLL02_TDft) #entropy

COLL02_TD.netMx <- cbind(COLL02_TD.netMx, COLL02_TD.clusterCoef, COLL02_TD.degreeCent$centralization,
                         COLL02_TD.netDensity, COLL02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************

round = 2
teamName = "COLL"
KIoutcome = "End of Qtr_DM"
COLL02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
COLL02_QTg2 <- data.frame(COLL02_QT)
COLL02_QTg2 <- COLL02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- COLL02_QTg2$player1
player2vector <- COLL02_QTg2$player2
COLL02_QTg3 <- COLL02_QTg2
COLL02_QTg3$p1inp2vec <- is.element(COLL02_QTg3$player1, player2vector)
COLL02_QTg3$p2inp1vec <- is.element(COLL02_QTg3$player2, player1vector)

addPlayer1 <- COLL02_QTg3[ which(COLL02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- COLL02_QTg3[ which(COLL02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

COLL02_QTg2 <- rbind(COLL02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
COLL02_QTft <- ftable(COLL02_QTg2$player1, COLL02_QTg2$player2)
COLL02_QTft2 <- as.matrix(COLL02_QTft)
numRows <- nrow(COLL02_QTft2)
numCols <- ncol(COLL02_QTft2)
COLL02_QTft3 <- COLL02_QTft2[c(2:numRows) , c(2:numCols)]
COLL02_QTTable <- graph.adjacency(COLL02_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(COLL02_QTTable, vertex.label = V(COLL02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(COLL02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
COLL02_QT.clusterCoef <- transitivity(COLL02_QTTable, type="global") #cluster coefficient
COLL02_QT.degreeCent <- centralization.degree(COLL02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
COLL02_QTftn <- as.network.matrix(COLL02_QTft)
COLL02_QT.netDensity <- network.density(COLL02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
COLL02_QT.entropy <- entropy(COLL02_QTft) #entropy

COLL02_QT.netMx <- cbind(COLL02_QT.netMx, COLL02_QT.clusterCoef, COLL02_QT.degreeCent$centralization,
                         COLL02_QT.netDensity, COLL02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(COLL02_QT.netMx) <- varnames

#############################################################################
#ESSENDON

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "ESS"
KIoutcome = "Goal_F"
ESS02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
ESS02_Gg2 <- data.frame(ESS02_G)
ESS02_Gg2 <- ESS02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_Gg2$player1
player2vector <- ESS02_Gg2$player2
ESS02_Gg3 <- ESS02_Gg2
ESS02_Gg3$p1inp2vec <- is.element(ESS02_Gg3$player1, player2vector)
ESS02_Gg3$p2inp1vec <- is.element(ESS02_Gg3$player2, player1vector)

addPlayer1 <- ESS02_Gg3[ which(ESS02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_Gg3[ which(ESS02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_Gg2 <- rbind(ESS02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
ESS02_Gft <- ftable(ESS02_Gg2$player1, ESS02_Gg2$player2)
ESS02_Gft2 <- as.matrix(ESS02_Gft)
numRows <- nrow(ESS02_Gft2)
numCols <- ncol(ESS02_Gft2)
ESS02_Gft3 <- ESS02_Gft2[c(2:numRows) , c(2:numCols)]
ESS02_GTable <- graph.adjacency(ESS02_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(ESS02_GTable, vertex.label = V(ESS02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
ESS02_G.clusterCoef <- transitivity(ESS02_GTable, type="global") #cluster coefficient
ESS02_G.degreeCent <- centralization.degree(ESS02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_Gftn <- as.network.matrix(ESS02_Gft)
ESS02_G.netDensity <- network.density(ESS02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_G.entropy <- entropy(ESS02_Gft) #entropy

ESS02_G.netMx <- cbind(ESS02_G.netMx, ESS02_G.clusterCoef, ESS02_G.degreeCent$centralization,
                       ESS02_G.netDensity, ESS02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "ESS"
KIoutcome = "Behind_F"
ESS02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
ESS02_Bg2 <- data.frame(ESS02_B)
ESS02_Bg2 <- ESS02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_Bg2$player1
player2vector <- ESS02_Bg2$player2
ESS02_Bg3 <- ESS02_Bg2
ESS02_Bg3$p1inp2vec <- is.element(ESS02_Bg3$player1, player2vector)
ESS02_Bg3$p2inp1vec <- is.element(ESS02_Bg3$player2, player1vector)

addPlayer1 <- ESS02_Bg3[ which(ESS02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_Bg3[ which(ESS02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_Bg2 <- rbind(ESS02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
ESS02_Bft <- ftable(ESS02_Bg2$player1, ESS02_Bg2$player2)
ESS02_Bft2 <- as.matrix(ESS02_Bft)
numRows <- nrow(ESS02_Bft2)
numCols <- ncol(ESS02_Bft2)
ESS02_Bft3 <- ESS02_Bft2[c(2:numRows) , c(2:numCols)]
ESS02_BTable <- graph.adjacency(ESS02_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(ESS02_BTable, vertex.label = V(ESS02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
ESS02_B.clusterCoef <- transitivity(ESS02_BTable, type="global") #cluster coefficient
ESS02_B.degreeCent <- centralization.degree(ESS02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_Bftn <- as.network.matrix(ESS02_Bft)
ESS02_B.netDensity <- network.density(ESS02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_B.entropy <- entropy(ESS02_Bft) #entropy

ESS02_B.netMx <- cbind(ESS02_B.netMx, ESS02_B.clusterCoef, ESS02_B.degreeCent$centralization,
                       ESS02_B.netDensity, ESS02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************

round = 2
teamName = "ESS"
KIoutcome = "Stoppage_F"
ESS02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
ESS02_SFg2 <- data.frame(ESS02_SF)
ESS02_SFg2 <- ESS02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_SFg2$player1
player2vector <- ESS02_SFg2$player2
ESS02_SFg3 <- ESS02_SFg2
ESS02_SFg3$p1inp2vec <- is.element(ESS02_SFg3$player1, player2vector)
ESS02_SFg3$p2inp1vec <- is.element(ESS02_SFg3$player2, player1vector)

addPlayer1 <- ESS02_SFg3[ which(ESS02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

ESS02_SFg2 <- rbind(ESS02_SFg2, addPlayer1)

#Round 2, FWD Stoppage graph using weighted edges
ESS02_SFft <- ftable(ESS02_SFg2$player1, ESS02_SFg2$player2)
ESS02_SFft2 <- as.matrix(ESS02_SFft)
numRows <- nrow(ESS02_SFft2)
numCols <- ncol(ESS02_SFft2)
ESS02_SFft3 <- ESS02_SFft2[c(2:numRows) , c(1:numCols)]
ESS02_SFTable <- graph.adjacency(ESS02_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(ESS02_SFTable, vertex.label = V(ESS02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
ESS02_SF.clusterCoef <- transitivity(ESS02_SFTable, type="global") #cluster coefficient
ESS02_SF.degreeCent <- centralization.degree(ESS02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_SFftn <- as.network.matrix(ESS02_SFft)
ESS02_SF.netDensity <- network.density(ESS02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_SF.entropy <- entropy(ESS02_SFft) #entropy

ESS02_SF.netMx <- cbind(ESS02_SF.netMx, ESS02_SF.clusterCoef, ESS02_SF.degreeCent$centralization,
                        ESS02_SF.netDensity, ESS02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "ESS"
KIoutcome = "Turnover_F"
ESS02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
ESS02_TFg2 <- data.frame(ESS02_TF)
ESS02_TFg2 <- ESS02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_TFg2$player1
player2vector <- ESS02_TFg2$player2
ESS02_TFg3 <- ESS02_TFg2
ESS02_TFg3$p1inp2vec <- is.element(ESS02_TFg3$player1, player2vector)
ESS02_TFg3$p2inp1vec <- is.element(ESS02_TFg3$player2, player1vector)

addPlayer1 <- ESS02_TFg3[ which(ESS02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_TFg3[ which(ESS02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_TFg2 <- rbind(ESS02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
ESS02_TFft <- ftable(ESS02_TFg2$player1, ESS02_TFg2$player2)
ESS02_TFft2 <- as.matrix(ESS02_TFft)
numRows <- nrow(ESS02_TFft2)
numCols <- ncol(ESS02_TFft2)
ESS02_TFft3 <- ESS02_TFft2[c(2:numRows) , c(2:numCols)]
ESS02_TFTable <- graph.adjacency(ESS02_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(ESS02_TFTable, vertex.label = V(ESS02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
ESS02_TF.clusterCoef <- transitivity(ESS02_TFTable, type="global") #cluster coefficient
ESS02_TF.degreeCent <- centralization.degree(ESS02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_TFftn <- as.network.matrix(ESS02_TFft)
ESS02_TF.netDensity <- network.density(ESS02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_TF.entropy <- entropy(ESS02_TFft) #entropy

ESS02_TF.netMx <- cbind(ESS02_TF.netMx, ESS02_TF.clusterCoef, ESS02_TF.degreeCent$centralization,
                        ESS02_TF.netDensity, ESS02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "ESS"
KIoutcome = "Stoppage_AM"
ESS02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
ESS02_SAMg2 <- data.frame(ESS02_SAM)
ESS02_SAMg2 <- ESS02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_SAMg2$player1
player2vector <- ESS02_SAMg2$player2
ESS02_SAMg3 <- ESS02_SAMg2
ESS02_SAMg3$p1inp2vec <- is.element(ESS02_SAMg3$player1, player2vector)
ESS02_SAMg3$p2inp1vec <- is.element(ESS02_SAMg3$player2, player1vector)

addPlayer1 <- ESS02_SAMg3[ which(ESS02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_SAMg3[ which(ESS02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_SAMg2 <- rbind(ESS02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
ESS02_SAMft <- ftable(ESS02_SAMg2$player1, ESS02_SAMg2$player2)
ESS02_SAMft2 <- as.matrix(ESS02_SAMft)
numRows <- nrow(ESS02_SAMft2)
numCols <- ncol(ESS02_SAMft2)
ESS02_SAMft3 <- ESS02_SAMft2[c(2:numRows) , c(2:numCols)]
ESS02_SAMTable <- graph.adjacency(ESS02_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(ESS02_SAMTable, vertex.label = V(ESS02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
ESS02_SAM.clusterCoef <- transitivity(ESS02_SAMTable, type="global") #cluster coefficient
ESS02_SAM.degreeCent <- centralization.degree(ESS02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_SAMftn <- as.network.matrix(ESS02_SAMft)
ESS02_SAM.netDensity <- network.density(ESS02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_SAM.entropy <- entropy(ESS02_SAMft) #entropy

ESS02_SAM.netMx <- cbind(ESS02_SAM.netMx, ESS02_SAM.clusterCoef, ESS02_SAM.degreeCent$centralization,
                         ESS02_SAM.netDensity, ESS02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************
#NA

round = 2
teamName = "ESS"
KIoutcome = "Turnover_AM"
ESS02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
ESS02_TAMg2 <- data.frame(ESS02_TAM)
ESS02_TAMg2 <- ESS02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_TAMg2$player1
player2vector <- ESS02_TAMg2$player2
ESS02_TAMg3 <- ESS02_TAMg2
ESS02_TAMg3$p1inp2vec <- is.element(ESS02_TAMg3$player1, player2vector)
ESS02_TAMg3$p2inp1vec <- is.element(ESS02_TAMg3$player2, player1vector)

addPlayer1 <- ESS02_TAMg3[ which(ESS02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_TAMg3[ which(ESS02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_TAMg2 <- rbind(ESS02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
ESS02_TAMft <- ftable(ESS02_TAMg2$player1, ESS02_TAMg2$player2)
ESS02_TAMft2 <- as.matrix(ESS02_TAMft)
numRows <- nrow(ESS02_TAMft2)
numCols <- ncol(ESS02_TAMft2)
ESS02_TAMft3 <- ESS02_TAMft2[c(2:numRows) , c(2:numCols)]
ESS02_TAMTable <- graph.adjacency(ESS02_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(ESS02_TAMTable, vertex.label = V(ESS02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
ESS02_TAM.clusterCoef <- transitivity(ESS02_TAMTable, type="global") #cluster coefficient
ESS02_TAM.degreeCent <- centralization.degree(ESS02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_TAMftn <- as.network.matrix(ESS02_TAMft)
ESS02_TAM.netDensity <- network.density(ESS02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_TAM.entropy <- entropy(ESS02_TAMft) #entropy

ESS02_TAM.netMx <- cbind(ESS02_TAM.netMx, ESS02_TAM.clusterCoef, ESS02_TAM.degreeCent$centralization,
                         ESS02_TAM.netDensity, ESS02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "ESS"
KIoutcome = "Stoppage_DM"
ESS02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
ESS02_SDMg2 <- data.frame(ESS02_SDM)
ESS02_SDMg2 <- ESS02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_SDMg2$player1
player2vector <- ESS02_SDMg2$player2
ESS02_SDMg3 <- ESS02_SDMg2
ESS02_SDMg3$p1inp2vec <- is.element(ESS02_SDMg3$player1, player2vector)
ESS02_SDMg3$p2inp1vec <- is.element(ESS02_SDMg3$player2, player1vector)

addPlayer1 <- ESS02_SDMg3[ which(ESS02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_SDMg3[ which(ESS02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_SDMg2 <- rbind(ESS02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
ESS02_SDMft <- ftable(ESS02_SDMg2$player1, ESS02_SDMg2$player2)
ESS02_SDMft2 <- as.matrix(ESS02_SDMft)
numRows <- nrow(ESS02_SDMft2)
numCols <- ncol(ESS02_SDMft2)
ESS02_SDMft3 <- ESS02_SDMft2[c(2:numRows) , c(2:numCols)]
ESS02_SDMTable <- graph.adjacency(ESS02_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(ESS02_SDMTable, vertex.label = V(ESS02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
ESS02_SDM.clusterCoef <- transitivity(ESS02_SDMTable, type="global") #cluster coefficient
ESS02_SDM.degreeCent <- centralization.degree(ESS02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_SDMftn <- as.network.matrix(ESS02_SDMft)
ESS02_SDM.netDensity <- network.density(ESS02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_SDM.entropy <- entropy(ESS02_SDMft) #entropy

ESS02_SDM.netMx <- cbind(ESS02_SDM.netMx, ESS02_SDM.clusterCoef, ESS02_SDM.degreeCent$centralization,
                         ESS02_SDM.netDensity, ESS02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "ESS"
KIoutcome = "Turnover_DM"
ESS02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
ESS02_TDMg2 <- data.frame(ESS02_TDM)
ESS02_TDMg2 <- ESS02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_TDMg2$player1
player2vector <- ESS02_TDMg2$player2
ESS02_TDMg3 <- ESS02_TDMg2
ESS02_TDMg3$p1inp2vec <- is.element(ESS02_TDMg3$player1, player2vector)
ESS02_TDMg3$p2inp1vec <- is.element(ESS02_TDMg3$player2, player1vector)

addPlayer1 <- ESS02_TDMg3[ which(ESS02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_TDMg3[ which(ESS02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_TDMg2 <- rbind(ESS02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
ESS02_TDMft <- ftable(ESS02_TDMg2$player1, ESS02_TDMg2$player2)
ESS02_TDMft2 <- as.matrix(ESS02_TDMft)
numRows <- nrow(ESS02_TDMft2)
numCols <- ncol(ESS02_TDMft2)
ESS02_TDMft3 <- ESS02_TDMft2[c(2:numRows) , c(2:numCols)] #Had to change no of cols when only adding rows
ESS02_TDMTable <- graph.adjacency(ESS02_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(ESS02_TDMTable, vertex.label = V(ESS02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
ESS02_TDM.clusterCoef <- transitivity(ESS02_TDMTable, type="global") #cluster coefficient
ESS02_TDM.degreeCent <- centralization.degree(ESS02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_TDMftn <- as.network.matrix(ESS02_TDMft)
ESS02_TDM.netDensity <- network.density(ESS02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_TDM.entropy <- entropy(ESS02_TDMft) #entropy

ESS02_TDM.netMx <- cbind(ESS02_TDM.netMx, ESS02_TDM.clusterCoef, ESS02_TDM.degreeCent$centralization,
                         ESS02_TDM.netDensity, ESS02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "ESS"
KIoutcome = "Stoppage_D"
ESS02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
ESS02_SDg2 <- data.frame(ESS02_SD)
ESS02_SDg2 <- ESS02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_SDg2$player1
player2vector <- ESS02_SDg2$player2
ESS02_SDg3 <- ESS02_SDg2
ESS02_SDg3$p1inp2vec <- is.element(ESS02_SDg3$player1, player2vector)
ESS02_SDg3$p2inp1vec <- is.element(ESS02_SDg3$player2, player1vector)

addPlayer1 <- ESS02_SDg3[ which(ESS02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_SDg3[ which(ESS02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_SDg2 <- rbind(ESS02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
ESS02_SDft <- ftable(ESS02_SDg2$player1, ESS02_SDg2$player2)
ESS02_SDft2 <- as.matrix(ESS02_SDft)
numRows <- nrow(ESS02_SDft2)
numCols <- ncol(ESS02_SDft2)
ESS02_SDft3 <- ESS02_SDft2[c(2:numRows) , c(2:numCols)]
ESS02_SDTable <- graph.adjacency(ESS02_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(ESS02_SDTable, vertex.label = V(ESS02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
ESS02_SD.clusterCoef <- transitivity(ESS02_SDTable, type="global") #cluster coefficient
ESS02_SD.degreeCent <- centralization.degree(ESS02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_SDftn <- as.network.matrix(ESS02_SDft)
ESS02_SD.netDensity <- network.density(ESS02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_SD.entropy <- entropy(ESS02_SDft) #entropy

ESS02_SD.netMx <- cbind(ESS02_SD.netMx, ESS02_SD.clusterCoef, ESS02_SD.degreeCent$centralization,
                        ESS02_SD.netDensity, ESS02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************

round = 2
teamName = "ESS"
KIoutcome = "Turnover_D"
ESS02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
ESS02_TDg2 <- data.frame(ESS02_TD)
ESS02_TDg2 <- ESS02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_TDg2$player1
player2vector <- ESS02_TDg2$player2
ESS02_TDg3 <- ESS02_TDg2
ESS02_TDg3$p1inp2vec <- is.element(ESS02_TDg3$player1, player2vector)
ESS02_TDg3$p2inp1vec <- is.element(ESS02_TDg3$player2, player1vector)

addPlayer1 <- ESS02_TDg3[ which(ESS02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_TDg3[ which(ESS02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_TDg2 <- rbind(ESS02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
ESS02_TDft <- ftable(ESS02_TDg2$player1, ESS02_TDg2$player2)
ESS02_TDft2 <- as.matrix(ESS02_TDft)
numRows <- nrow(ESS02_TDft2)
numCols <- ncol(ESS02_TDft2)
ESS02_TDft3 <- ESS02_TDft2[c(2:numRows) , c(2:numCols)]
ESS02_TDTable <- graph.adjacency(ESS02_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(ESS02_TDTable, vertex.label = V(ESS02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
ESS02_TD.clusterCoef <- transitivity(ESS02_TDTable, type="global") #cluster coefficient
ESS02_TD.degreeCent <- centralization.degree(ESS02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_TDftn <- as.network.matrix(ESS02_TDft)
ESS02_TD.netDensity <- network.density(ESS02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_TD.entropy <- entropy(ESS02_TDft) #entropy

ESS02_TD.netMx <- cbind(ESS02_TD.netMx, ESS02_TD.clusterCoef, ESS02_TD.degreeCent$centralization,
                        ESS02_TD.netDensity, ESS02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************

round = 2
teamName = "ESS"
KIoutcome = "End of Qtr_DM"
ESS02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
ESS02_QTg2 <- data.frame(ESS02_QT)
ESS02_QTg2 <- ESS02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- ESS02_QTg2$player1
player2vector <- ESS02_QTg2$player2
ESS02_QTg3 <- ESS02_QTg2
ESS02_QTg3$p1inp2vec <- is.element(ESS02_QTg3$player1, player2vector)
ESS02_QTg3$p2inp1vec <- is.element(ESS02_QTg3$player2, player1vector)

addPlayer1 <- ESS02_QTg3[ which(ESS02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- ESS02_QTg3[ which(ESS02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

ESS02_QTg2 <- rbind(ESS02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
ESS02_QTft <- ftable(ESS02_QTg2$player1, ESS02_QTg2$player2)
ESS02_QTft2 <- as.matrix(ESS02_QTft)
numRows <- nrow(ESS02_QTft2)
numCols <- ncol(ESS02_QTft2)
ESS02_QTft3 <- ESS02_QTft2[c(2:numRows) , c(2:numCols)]
ESS02_QTTable <- graph.adjacency(ESS02_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(ESS02_QTTable, vertex.label = V(ESS02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(ESS02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
ESS02_QT.clusterCoef <- transitivity(ESS02_QTTable, type="global") #cluster coefficient
ESS02_QT.degreeCent <- centralization.degree(ESS02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
ESS02_QTftn <- as.network.matrix(ESS02_QTft)
ESS02_QT.netDensity <- network.density(ESS02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
ESS02_QT.entropy <- entropy(ESS02_QTft) #entropy

ESS02_QT.netMx <- cbind(ESS02_QT.netMx, ESS02_QT.clusterCoef, ESS02_QT.degreeCent$centralization,
                        ESS02_QT.netDensity, ESS02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(ESS02_QT.netMx) <- varnames

#############################################################################
#FREMANTLE

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "FRE"
KIoutcome = "Goal_F"
FRE02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
FRE02_Gg2 <- data.frame(FRE02_G)
FRE02_Gg2 <- FRE02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_Gg2$player1
player2vector <- FRE02_Gg2$player2
FRE02_Gg3 <- FRE02_Gg2
FRE02_Gg3$p1inp2vec <- is.element(FRE02_Gg3$player1, player2vector)
FRE02_Gg3$p2inp1vec <- is.element(FRE02_Gg3$player2, player1vector)

addPlayer1 <- FRE02_Gg3[ which(FRE02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_Gg3[ which(FRE02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_Gg2 <- rbind(FRE02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
FRE02_Gft <- ftable(FRE02_Gg2$player1, FRE02_Gg2$player2)
FRE02_Gft2 <- as.matrix(FRE02_Gft)
numRows <- nrow(FRE02_Gft2)
numCols <- ncol(FRE02_Gft2)
FRE02_Gft3 <- FRE02_Gft2[c(2:numRows) , c(2:numCols)]
FRE02_GTable <- graph.adjacency(FRE02_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(FRE02_GTable, vertex.label = V(FRE02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
FRE02_G.clusterCoef <- transitivity(FRE02_GTable, type="global") #cluster coefficient
FRE02_G.degreeCent <- centralization.degree(FRE02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_Gftn <- as.network.matrix(FRE02_Gft)
FRE02_G.netDensity <- network.density(FRE02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_G.entropy <- entropy(FRE02_Gft) #entropy

FRE02_G.netMx <- cbind(FRE02_G.netMx, FRE02_G.clusterCoef, FRE02_G.degreeCent$centralization,
                       FRE02_G.netDensity, FRE02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "FRE"
KIoutcome = "Behind_F"
FRE02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
FRE02_Bg2 <- data.frame(FRE02_B)
FRE02_Bg2 <- FRE02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_Bg2$player1
player2vector <- FRE02_Bg2$player2
FRE02_Bg3 <- FRE02_Bg2
FRE02_Bg3$p1inp2vec <- is.element(FRE02_Bg3$player1, player2vector)
FRE02_Bg3$p2inp1vec <- is.element(FRE02_Bg3$player2, player1vector)

addPlayer1 <- FRE02_Bg3[ which(FRE02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_Bg3[ which(FRE02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_Bg2 <- rbind(FRE02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
FRE02_Bft <- ftable(FRE02_Bg2$player1, FRE02_Bg2$player2)
FRE02_Bft2 <- as.matrix(FRE02_Bft)
numRows <- nrow(FRE02_Bft2)
numCols <- ncol(FRE02_Bft2)
FRE02_Bft3 <- FRE02_Bft2[c(2:numRows) , c(2:numCols)]
FRE02_BTable <- graph.adjacency(FRE02_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(FRE02_BTable, vertex.label = V(FRE02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
FRE02_B.clusterCoef <- transitivity(FRE02_BTable, type="global") #cluster coefficient
FRE02_B.degreeCent <- centralization.degree(FRE02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_Bftn <- as.network.matrix(FRE02_Bft)
FRE02_B.netDensity <- network.density(FRE02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_B.entropy <- entropy(FRE02_Bft) #entropy

FRE02_B.netMx <- cbind(FRE02_B.netMx, FRE02_B.clusterCoef, FRE02_B.degreeCent$centralization,
                       FRE02_B.netDensity, FRE02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "FRE"
KIoutcome = "Stoppage_F"
FRE02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
FRE02_SFg2 <- data.frame(FRE02_SF)
FRE02_SFg2 <- FRE02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_SFg2$player1
player2vector <- FRE02_SFg2$player2
FRE02_SFg3 <- FRE02_SFg2
FRE02_SFg3$p1inp2vec <- is.element(FRE02_SFg3$player1, player2vector)
FRE02_SFg3$p2inp1vec <- is.element(FRE02_SFg3$player2, player1vector)

addPlayer1 <- FRE02_SFg3[ which(FRE02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_SFg3[ which(FRE02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_SFg2 <- rbind(FRE02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
FRE02_SFft <- ftable(FRE02_SFg2$player1, FRE02_SFg2$player2)
FRE02_SFft2 <- as.matrix(FRE02_SFft)
numRows <- nrow(FRE02_SFft2)
numCols <- ncol(FRE02_SFft2)
FRE02_SFft3 <- FRE02_SFft2[c(2:numRows) , c(2:numCols)]
FRE02_SFTable <- graph.adjacency(FRE02_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(FRE02_SFTable, vertex.label = V(FRE02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
FRE02_SF.clusterCoef <- transitivity(FRE02_SFTable, type="global") #cluster coefficient
FRE02_SF.degreeCent <- centralization.degree(FRE02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_SFftn <- as.network.matrix(FRE02_SFft)
FRE02_SF.netDensity <- network.density(FRE02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_SF.entropy <- entropy(FRE02_SFft) #entropy

FRE02_SF.netMx <- cbind(FRE02_SF.netMx, FRE02_SF.clusterCoef, FRE02_SF.degreeCent$centralization,
                        FRE02_SF.netDensity, FRE02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "FRE"
KIoutcome = "Turnover_F"
FRE02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
FRE02_TFg2 <- data.frame(FRE02_TF)
FRE02_TFg2 <- FRE02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_TFg2$player1
player2vector <- FRE02_TFg2$player2
FRE02_TFg3 <- FRE02_TFg2
FRE02_TFg3$p1inp2vec <- is.element(FRE02_TFg3$player1, player2vector)
FRE02_TFg3$p2inp1vec <- is.element(FRE02_TFg3$player2, player1vector)

addPlayer1 <- FRE02_TFg3[ which(FRE02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_TFg3[ which(FRE02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_TFg2 <- rbind(FRE02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
FRE02_TFft <- ftable(FRE02_TFg2$player1, FRE02_TFg2$player2)
FRE02_TFft2 <- as.matrix(FRE02_TFft)
numRows <- nrow(FRE02_TFft2)
numCols <- ncol(FRE02_TFft2)
FRE02_TFft3 <- FRE02_TFft2[c(2:numRows) , c(2:numCols)]
FRE02_TFTable <- graph.adjacency(FRE02_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(FRE02_TFTable, vertex.label = V(FRE02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
FRE02_TF.clusterCoef <- transitivity(FRE02_TFTable, type="global") #cluster coefficient
FRE02_TF.degreeCent <- centralization.degree(FRE02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_TFftn <- as.network.matrix(FRE02_TFft)
FRE02_TF.netDensity <- network.density(FRE02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_TF.entropy <- entropy(FRE02_TFft) #entropy

FRE02_TF.netMx <- cbind(FRE02_TF.netMx, FRE02_TF.clusterCoef, FRE02_TF.degreeCent$centralization,
                        FRE02_TF.netDensity, FRE02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "FRE"
KIoutcome = "Stoppage_AM"
FRE02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
FRE02_SAMg2 <- data.frame(FRE02_SAM)
FRE02_SAMg2 <- FRE02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_SAMg2$player1
player2vector <- FRE02_SAMg2$player2
FRE02_SAMg3 <- FRE02_SAMg2
FRE02_SAMg3$p1inp2vec <- is.element(FRE02_SAMg3$player1, player2vector)
FRE02_SAMg3$p2inp1vec <- is.element(FRE02_SAMg3$player2, player1vector)

addPlayer1 <- FRE02_SAMg3[ which(FRE02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_SAMg3[ which(FRE02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_SAMg2 <- rbind(FRE02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
FRE02_SAMft <- ftable(FRE02_SAMg2$player1, FRE02_SAMg2$player2)
FRE02_SAMft2 <- as.matrix(FRE02_SAMft)
numRows <- nrow(FRE02_SAMft2)
numCols <- ncol(FRE02_SAMft2)
FRE02_SAMft3 <- FRE02_SAMft2[c(2:numRows) , c(2:numCols)]
FRE02_SAMTable <- graph.adjacency(FRE02_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(FRE02_SAMTable, vertex.label = V(FRE02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
FRE02_SAM.clusterCoef <- transitivity(FRE02_SAMTable, type="global") #cluster coefficient
FRE02_SAM.degreeCent <- centralization.degree(FRE02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_SAMftn <- as.network.matrix(FRE02_SAMft)
FRE02_SAM.netDensity <- network.density(FRE02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_SAM.entropy <- entropy(FRE02_SAMft) #entropy

FRE02_SAM.netMx <- cbind(FRE02_SAM.netMx, FRE02_SAM.clusterCoef, FRE02_SAM.degreeCent$centralization,
                         FRE02_SAM.netDensity, FRE02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "FRE"
KIoutcome = "Turnover_AM"
FRE02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
FRE02_TAMg2 <- data.frame(FRE02_TAM)
FRE02_TAMg2 <- FRE02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_TAMg2$player1
player2vector <- FRE02_TAMg2$player2
FRE02_TAMg3 <- FRE02_TAMg2
FRE02_TAMg3$p1inp2vec <- is.element(FRE02_TAMg3$player1, player2vector)
FRE02_TAMg3$p2inp1vec <- is.element(FRE02_TAMg3$player2, player1vector)

addPlayer1 <- FRE02_TAMg3[ which(FRE02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_TAMg3[ which(FRE02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_TAMg2 <- rbind(FRE02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
FRE02_TAMft <- ftable(FRE02_TAMg2$player1, FRE02_TAMg2$player2)
FRE02_TAMft2 <- as.matrix(FRE02_TAMft)
numRows <- nrow(FRE02_TAMft2)
numCols <- ncol(FRE02_TAMft2)
FRE02_TAMft3 <- FRE02_TAMft2[c(2:numRows) , c(2:numCols)]
FRE02_TAMTable <- graph.adjacency(FRE02_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(FRE02_TAMTable, vertex.label = V(FRE02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
FRE02_TAM.clusterCoef <- transitivity(FRE02_TAMTable, type="global") #cluster coefficient
FRE02_TAM.degreeCent <- centralization.degree(FRE02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_TAMftn <- as.network.matrix(FRE02_TAMft)
FRE02_TAM.netDensity <- network.density(FRE02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_TAM.entropy <- entropy(FRE02_TAMft) #entropy

FRE02_TAM.netMx <- cbind(FRE02_TAM.netMx, FRE02_TAM.clusterCoef, FRE02_TAM.degreeCent$centralization,
                         FRE02_TAM.netDensity, FRE02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "FRE"
KIoutcome = "Stoppage_DM"
FRE02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
FRE02_SDMg2 <- data.frame(FRE02_SDM)
FRE02_SDMg2 <- FRE02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_SDMg2$player1
player2vector <- FRE02_SDMg2$player2
FRE02_SDMg3 <- FRE02_SDMg2
FRE02_SDMg3$p1inp2vec <- is.element(FRE02_SDMg3$player1, player2vector)
FRE02_SDMg3$p2inp1vec <- is.element(FRE02_SDMg3$player2, player1vector)

addPlayer1 <- FRE02_SDMg3[ which(FRE02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_SDMg3[ which(FRE02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_SDMg2 <- rbind(FRE02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
FRE02_SDMft <- ftable(FRE02_SDMg2$player1, FRE02_SDMg2$player2)
FRE02_SDMft2 <- as.matrix(FRE02_SDMft)
numRows <- nrow(FRE02_SDMft2)
numCols <- ncol(FRE02_SDMft2)
FRE02_SDMft3 <- FRE02_SDMft2[c(2:numRows) , c(2:numCols)]
FRE02_SDMTable <- graph.adjacency(FRE02_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(FRE02_SDMTable, vertex.label = V(FRE02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
FRE02_SDM.clusterCoef <- transitivity(FRE02_SDMTable, type="global") #cluster coefficient
FRE02_SDM.degreeCent <- centralization.degree(FRE02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_SDMftn <- as.network.matrix(FRE02_SDMft)
FRE02_SDM.netDensity <- network.density(FRE02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_SDM.entropy <- entropy(FRE02_SDMft) #entropy

FRE02_SDM.netMx <- cbind(FRE02_SDM.netMx, FRE02_SDM.clusterCoef, FRE02_SDM.degreeCent$centralization,
                         FRE02_SDM.netDensity, FRE02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "FRE"
KIoutcome = "Turnover_DM"
FRE02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
FRE02_TDMg2 <- data.frame(FRE02_TDM)
FRE02_TDMg2 <- FRE02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_TDMg2$player1
player2vector <- FRE02_TDMg2$player2
FRE02_TDMg3 <- FRE02_TDMg2
FRE02_TDMg3$p1inp2vec <- is.element(FRE02_TDMg3$player1, player2vector)
FRE02_TDMg3$p2inp1vec <- is.element(FRE02_TDMg3$player2, player1vector)

addPlayer1 <- FRE02_TDMg3[ which(FRE02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_TDMg3[ which(FRE02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_TDMg2 <- rbind(FRE02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
FRE02_TDMft <- ftable(FRE02_TDMg2$player1, FRE02_TDMg2$player2)
FRE02_TDMft2 <- as.matrix(FRE02_TDMft)
numRows <- nrow(FRE02_TDMft2)
numCols <- ncol(FRE02_TDMft2)
FRE02_TDMft3 <- FRE02_TDMft2[c(2:numRows) , c(2:numCols)]
FRE02_TDMTable <- graph.adjacency(FRE02_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(FRE02_TDMTable, vertex.label = V(FRE02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
FRE02_TDM.clusterCoef <- transitivity(FRE02_TDMTable, type="global") #cluster coefficient
FRE02_TDM.degreeCent <- centralization.degree(FRE02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_TDMftn <- as.network.matrix(FRE02_TDMft)
FRE02_TDM.netDensity <- network.density(FRE02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_TDM.entropy <- entropy(FRE02_TDMft) #entropy

FRE02_TDM.netMx <- cbind(FRE02_TDM.netMx, FRE02_TDM.clusterCoef, FRE02_TDM.degreeCent$centralization,
                         FRE02_TDM.netDensity, FRE02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "FRE"
KIoutcome = "Stoppage_D"
FRE02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
FRE02_SDg2 <- data.frame(FRE02_SD)
FRE02_SDg2 <- FRE02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_SDg2$player1
player2vector <- FRE02_SDg2$player2
FRE02_SDg3 <- FRE02_SDg2
FRE02_SDg3$p1inp2vec <- is.element(FRE02_SDg3$player1, player2vector)
FRE02_SDg3$p2inp1vec <- is.element(FRE02_SDg3$player2, player1vector)

addPlayer1 <- FRE02_SDg3[ which(FRE02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_SDg3[ which(FRE02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_SDg2 <- rbind(FRE02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
FRE02_SDft <- ftable(FRE02_SDg2$player1, FRE02_SDg2$player2)
FRE02_SDft2 <- as.matrix(FRE02_SDft)
numRows <- nrow(FRE02_SDft2)
numCols <- ncol(FRE02_SDft2)
FRE02_SDft3 <- FRE02_SDft2[c(2:numRows) , c(2:numCols)]
FRE02_SDTable <- graph.adjacency(FRE02_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(FRE02_SDTable, vertex.label = V(FRE02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
FRE02_SD.clusterCoef <- transitivity(FRE02_SDTable, type="global") #cluster coefficient
FRE02_SD.degreeCent <- centralization.degree(FRE02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_SDftn <- as.network.matrix(FRE02_SDft)
FRE02_SD.netDensity <- network.density(FRE02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_SD.entropy <- entropy(FRE02_SDft) #entropy

FRE02_SD.netMx <- cbind(FRE02_SD.netMx, FRE02_SD.clusterCoef, FRE02_SD.degreeCent$centralization,
                        FRE02_SD.netDensity, FRE02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "FRE"
KIoutcome = "Turnover_D"
FRE02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
FRE02_TDg2 <- data.frame(FRE02_TD)
FRE02_TDg2 <- FRE02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_TDg2$player1
player2vector <- FRE02_TDg2$player2
FRE02_TDg3 <- FRE02_TDg2
FRE02_TDg3$p1inp2vec <- is.element(FRE02_TDg3$player1, player2vector)
FRE02_TDg3$p2inp1vec <- is.element(FRE02_TDg3$player2, player1vector)

addPlayer1 <- FRE02_TDg3[ which(FRE02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_TDg3[ which(FRE02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_TDg2 <- rbind(FRE02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
FRE02_TDft <- ftable(FRE02_TDg2$player1, FRE02_TDg2$player2)
FRE02_TDft2 <- as.matrix(FRE02_TDft)
numRows <- nrow(FRE02_TDft2)
numCols <- ncol(FRE02_TDft2)
FRE02_TDft3 <- FRE02_TDft2[c(2:numRows) , c(2:numCols)]
FRE02_TDTable <- graph.adjacency(FRE02_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(FRE02_TDTable, vertex.label = V(FRE02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
FRE02_TD.clusterCoef <- transitivity(FRE02_TDTable, type="global") #cluster coefficient
FRE02_TD.degreeCent <- centralization.degree(FRE02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_TDftn <- as.network.matrix(FRE02_TDft)
FRE02_TD.netDensity <- network.density(FRE02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_TD.entropy <- entropy(FRE02_TDft) #entropy

FRE02_TD.netMx <- cbind(FRE02_TD.netMx, FRE02_TD.clusterCoef, FRE02_TD.degreeCent$centralization,
                        FRE02_TD.netDensity, FRE02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "FRE"
KIoutcome = "End of Qtr_DM"
FRE02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
FRE02_QTg2 <- data.frame(FRE02_QT)
FRE02_QTg2 <- FRE02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- FRE02_QTg2$player1
player2vector <- FRE02_QTg2$player2
FRE02_QTg3 <- FRE02_QTg2
FRE02_QTg3$p1inp2vec <- is.element(FRE02_QTg3$player1, player2vector)
FRE02_QTg3$p2inp1vec <- is.element(FRE02_QTg3$player2, player1vector)

addPlayer1 <- FRE02_QTg3[ which(FRE02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- FRE02_QTg3[ which(FRE02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

FRE02_QTg2 <- rbind(FRE02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
FRE02_QTft <- ftable(FRE02_QTg2$player1, FRE02_QTg2$player2)
FRE02_QTft2 <- as.matrix(FRE02_QTft)
numRows <- nrow(FRE02_QTft2)
numCols <- ncol(FRE02_QTft2)
FRE02_QTft3 <- FRE02_QTft2[c(2:numRows) , c(2:numCols)]
FRE02_QTTable <- graph.adjacency(FRE02_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(FRE02_QTTable, vertex.label = V(FRE02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(FRE02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
FRE02_QT.clusterCoef <- transitivity(FRE02_QTTable, type="global") #cluster coefficient
FRE02_QT.degreeCent <- centralization.degree(FRE02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
FRE02_QTftn <- as.network.matrix(FRE02_QTft)
FRE02_QT.netDensity <- network.density(FRE02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
FRE02_QT.entropy <- entropy(FRE02_QTft) #entropy

FRE02_QT.netMx <- cbind(FRE02_QT.netMx, FRE02_QT.clusterCoef, FRE02_QT.degreeCent$centralization,
                        FRE02_QT.netDensity, FRE02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(FRE02_QT.netMx) <- varnames

#############################################################################
#GOLD COAST

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "GCFC"
KIoutcome = "Goal_F"
GCFC02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
GCFC02_Gg2 <- data.frame(GCFC02_G)
GCFC02_Gg2 <- GCFC02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_Gg2$player1
player2vector <- GCFC02_Gg2$player2
GCFC02_Gg3 <- GCFC02_Gg2
GCFC02_Gg3$p1inp2vec <- is.element(GCFC02_Gg3$player1, player2vector)
GCFC02_Gg3$p2inp1vec <- is.element(GCFC02_Gg3$player2, player1vector)

addPlayer1 <- GCFC02_Gg3[ which(GCFC02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_Gg3[ which(GCFC02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_Gg2 <- rbind(GCFC02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
GCFC02_Gft <- ftable(GCFC02_Gg2$player1, GCFC02_Gg2$player2)
GCFC02_Gft2 <- as.matrix(GCFC02_Gft)
numRows <- nrow(GCFC02_Gft2)
numCols <- ncol(GCFC02_Gft2)
GCFC02_Gft3 <- GCFC02_Gft2[c(2:numRows) , c(2:numCols)]
GCFC02_GTable <- graph.adjacency(GCFC02_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(GCFC02_GTable, vertex.label = V(GCFC02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
GCFC02_G.clusterCoef <- transitivity(GCFC02_GTable, type="global") #cluster coefficient
GCFC02_G.degreeCent <- centralization.degree(GCFC02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_Gftn <- as.network.matrix(GCFC02_Gft)
GCFC02_G.netDensity <- network.density(GCFC02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_G.entropy <- entropy(GCFC02_Gft) #entropy

GCFC02_G.netMx <- cbind(GCFC02_G.netMx, GCFC02_G.clusterCoef, GCFC02_G.degreeCent$centralization,
                        GCFC02_G.netDensity, GCFC02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "GCFC"
KIoutcome = "Behind_F"
GCFC02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
GCFC02_Bg2 <- data.frame(GCFC02_B)
GCFC02_Bg2 <- GCFC02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_Bg2$player1
player2vector <- GCFC02_Bg2$player2
GCFC02_Bg3 <- GCFC02_Bg2
GCFC02_Bg3$p1inp2vec <- is.element(GCFC02_Bg3$player1, player2vector)
GCFC02_Bg3$p2inp1vec <- is.element(GCFC02_Bg3$player2, player1vector)

addPlayer1 <- GCFC02_Bg3[ which(GCFC02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_Bg3[ which(GCFC02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_Bg2 <- rbind(GCFC02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
GCFC02_Bft <- ftable(GCFC02_Bg2$player1, GCFC02_Bg2$player2)
GCFC02_Bft2 <- as.matrix(GCFC02_Bft)
numRows <- nrow(GCFC02_Bft2)
numCols <- ncol(GCFC02_Bft2)
GCFC02_Bft3 <- GCFC02_Bft2[c(2:numRows) , c(2:numCols)]
GCFC02_BTable <- graph.adjacency(GCFC02_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(GCFC02_BTable, vertex.label = V(GCFC02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
GCFC02_B.clusterCoef <- transitivity(GCFC02_BTable, type="global") #cluster coefficient
GCFC02_B.degreeCent <- centralization.degree(GCFC02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_Bftn <- as.network.matrix(GCFC02_Bft)
GCFC02_B.netDensity <- network.density(GCFC02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_B.entropy <- entropy(GCFC02_Bft) #entropy

GCFC02_B.netMx <- cbind(GCFC02_B.netMx, GCFC02_B.clusterCoef, GCFC02_B.degreeCent$centralization,
                        GCFC02_B.netDensity, GCFC02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************

round = 2
teamName = "GCFC"
KIoutcome = "Stoppage_F"
GCFC02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
GCFC02_SFg2 <- data.frame(GCFC02_SF)
GCFC02_SFg2 <- GCFC02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_SFg2$player1
player2vector <- GCFC02_SFg2$player2
GCFC02_SFg3 <- GCFC02_SFg2
GCFC02_SFg3$p1inp2vec <- is.element(GCFC02_SFg3$player1, player2vector)
GCFC02_SFg3$p2inp1vec <- is.element(GCFC02_SFg3$player2, player1vector)

addPlayer1 <- GCFC02_SFg3[ which(GCFC02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_SFg3[ which(GCFC02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_SFg2 <- rbind(GCFC02_SFg2, addPlayers)


#Round 2, FWD Stoppage graph using weighted edges
GCFC02_SFft <- ftable(GCFC02_SFg2$player1, GCFC02_SFg2$player2)
GCFC02_SFft2 <- as.matrix(GCFC02_SFft)
numRows <- nrow(GCFC02_SFft2)
numCols <- ncol(GCFC02_SFft2)
GCFC02_SFft3 <- GCFC02_SFft2[c(2:numRows) , c(2:numCols)]
GCFC02_SFTable <- graph.adjacency(GCFC02_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(GCFC02_SFTable, vertex.label = V(GCFC02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
GCFC02_SF.clusterCoef <- transitivity(GCFC02_SFTable, type="global") #cluster coefficient
GCFC02_SF.degreeCent <- centralization.degree(GCFC02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_SFftn <- as.network.matrix(GCFC02_SFft)
GCFC02_SF.netDensity <- network.density(GCFC02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_SF.entropy <- entropy(GCFC02_SFft) #entropy

GCFC02_SF.netMx <- cbind(GCFC02_SF.netMx, GCFC02_SF.clusterCoef, GCFC02_SF.degreeCent$centralization,
                         GCFC02_SF.netDensity, GCFC02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "GCFC"
KIoutcome = "Turnover_F"
GCFC02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
GCFC02_TFg2 <- data.frame(GCFC02_TF)
GCFC02_TFg2 <- GCFC02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_TFg2$player1
player2vector <- GCFC02_TFg2$player2
GCFC02_TFg3 <- GCFC02_TFg2
GCFC02_TFg3$p1inp2vec <- is.element(GCFC02_TFg3$player1, player2vector)
GCFC02_TFg3$p2inp1vec <- is.element(GCFC02_TFg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GCFC02_TFg3[ which(GCFC02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GCFC02_TFg2 <- rbind(GCFC02_TFg2, addPlayer2)

#Round 2, FWD Turnover graph using weighted edges
GCFC02_TFft <- ftable(GCFC02_TFg2$player1, GCFC02_TFg2$player2)
GCFC02_TFft2 <- as.matrix(GCFC02_TFft)
numRows <- nrow(GCFC02_TFft2)
numCols <- ncol(GCFC02_TFft2)
GCFC02_TFft3 <- GCFC02_TFft2[c(1:numRows) , c(2:numCols)]
GCFC02_TFTable <- graph.adjacency(GCFC02_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(GCFC02_TFTable, vertex.label = V(GCFC02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
GCFC02_TF.clusterCoef <- transitivity(GCFC02_TFTable, type="global") #cluster coefficient
GCFC02_TF.degreeCent <- centralization.degree(GCFC02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_TFftn <- as.network.matrix(GCFC02_TFft)
GCFC02_TF.netDensity <- network.density(GCFC02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_TF.entropy <- entropy(GCFC02_TFft) #entropy

GCFC02_TF.netMx <- cbind(GCFC02_TF.netMx, GCFC02_TF.clusterCoef, GCFC02_TF.degreeCent$centralization,
                         GCFC02_TF.netDensity, GCFC02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "GCFC"
KIoutcome = "Stoppage_AM"
GCFC02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
GCFC02_SAMg2 <- data.frame(GCFC02_SAM)
GCFC02_SAMg2 <- GCFC02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_SAMg2$player1
player2vector <- GCFC02_SAMg2$player2
GCFC02_SAMg3 <- GCFC02_SAMg2
GCFC02_SAMg3$p1inp2vec <- is.element(GCFC02_SAMg3$player1, player2vector)
GCFC02_SAMg3$p2inp1vec <- is.element(GCFC02_SAMg3$player2, player1vector)

addPlayer1 <- GCFC02_SAMg3[ which(GCFC02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_SAMg3[ which(GCFC02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_SAMg2 <- rbind(GCFC02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
GCFC02_SAMft <- ftable(GCFC02_SAMg2$player1, GCFC02_SAMg2$player2)
GCFC02_SAMft2 <- as.matrix(GCFC02_SAMft)
numRows <- nrow(GCFC02_SAMft2)
numCols <- ncol(GCFC02_SAMft2)
GCFC02_SAMft3 <- GCFC02_SAMft2[c(2:numRows) , c(2:numCols)]
GCFC02_SAMTable <- graph.adjacency(GCFC02_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(GCFC02_SAMTable, vertex.label = V(GCFC02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
GCFC02_SAM.clusterCoef <- transitivity(GCFC02_SAMTable, type="global") #cluster coefficient
GCFC02_SAM.degreeCent <- centralization.degree(GCFC02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_SAMftn <- as.network.matrix(GCFC02_SAMft)
GCFC02_SAM.netDensity <- network.density(GCFC02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_SAM.entropy <- entropy(GCFC02_SAMft) #entropy

GCFC02_SAM.netMx <- cbind(GCFC02_SAM.netMx, GCFC02_SAM.clusterCoef, GCFC02_SAM.degreeCent$centralization,
                          GCFC02_SAM.netDensity, GCFC02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "GCFC"
KIoutcome = "Turnover_AM"
GCFC02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
GCFC02_TAMg2 <- data.frame(GCFC02_TAM)
GCFC02_TAMg2 <- GCFC02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_TAMg2$player1
player2vector <- GCFC02_TAMg2$player2
GCFC02_TAMg3 <- GCFC02_TAMg2
GCFC02_TAMg3$p1inp2vec <- is.element(GCFC02_TAMg3$player1, player2vector)
GCFC02_TAMg3$p2inp1vec <- is.element(GCFC02_TAMg3$player2, player1vector)

addPlayer1 <- GCFC02_TAMg3[ which(GCFC02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_TAMg3[ which(GCFC02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_TAMg2 <- rbind(GCFC02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
GCFC02_TAMft <- ftable(GCFC02_TAMg2$player1, GCFC02_TAMg2$player2)
GCFC02_TAMft2 <- as.matrix(GCFC02_TAMft)
numRows <- nrow(GCFC02_TAMft2)
numCols <- ncol(GCFC02_TAMft2)
GCFC02_TAMft3 <- GCFC02_TAMft2[c(2:numRows) , c(2:numCols)]
GCFC02_TAMTable <- graph.adjacency(GCFC02_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(GCFC02_TAMTable, vertex.label = V(GCFC02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
GCFC02_TAM.clusterCoef <- transitivity(GCFC02_TAMTable, type="global") #cluster coefficient
GCFC02_TAM.degreeCent <- centralization.degree(GCFC02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_TAMftn <- as.network.matrix(GCFC02_TAMft)
GCFC02_TAM.netDensity <- network.density(GCFC02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_TAM.entropy <- entropy(GCFC02_TAMft) #entropy

GCFC02_TAM.netMx <- cbind(GCFC02_TAM.netMx, GCFC02_TAM.clusterCoef, GCFC02_TAM.degreeCent$centralization,
                          GCFC02_TAM.netDensity, GCFC02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************
#NA

round = 2
teamName = "GCFC"
KIoutcome = "Stoppage_DM"
GCFC02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
GCFC02_SDMg2 <- data.frame(GCFC02_SDM)
GCFC02_SDMg2 <- GCFC02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_SDMg2$player1
player2vector <- GCFC02_SDMg2$player2
GCFC02_SDMg3 <- GCFC02_SDMg2
GCFC02_SDMg3$p1inp2vec <- is.element(GCFC02_SDMg3$player1, player2vector)
GCFC02_SDMg3$p2inp1vec <- is.element(GCFC02_SDMg3$player2, player1vector)

addPlayer1 <- GCFC02_SDMg3[ which(GCFC02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_SDMg3[ which(GCFC02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_SDMg2 <- rbind(GCFC02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
GCFC02_SDMft <- ftable(GCFC02_SDMg2$player1, GCFC02_SDMg2$player2)
GCFC02_SDMft2 <- as.matrix(GCFC02_SDMft)
numRows <- nrow(GCFC02_SDMft2)
numCols <- ncol(GCFC02_SDMft2)
GCFC02_SDMft3 <- GCFC02_SDMft2[c(2:numRows) , c(2:numCols)]
GCFC02_SDMTable <- graph.adjacency(GCFC02_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(GCFC02_SDMTable, vertex.label = V(GCFC02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
GCFC02_SDM.clusterCoef <- transitivity(GCFC02_SDMTable, type="global") #cluster coefficient
GCFC02_SDM.degreeCent <- centralization.degree(GCFC02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_SDMftn <- as.network.matrix(GCFC02_SDMft)
GCFC02_SDM.netDensity <- network.density(GCFC02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_SDM.entropy <- entropy(GCFC02_SDMft) #entropy

GCFC02_SDM.netMx <- cbind(GCFC02_SDM.netMx, GCFC02_SDM.clusterCoef, GCFC02_SDM.degreeCent$centralization,
                          GCFC02_SDM.netDensity, GCFC02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************
#NA

round = 2
teamName = "GCFC"
KIoutcome = "Turnover_DM"
GCFC02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
GCFC02_TDMg2 <- data.frame(GCFC02_TDM)
GCFC02_TDMg2 <- GCFC02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_TDMg2$player1
player2vector <- GCFC02_TDMg2$player2
GCFC02_TDMg3 <- GCFC02_TDMg2
GCFC02_TDMg3$p1inp2vec <- is.element(GCFC02_TDMg3$player1, player2vector)
GCFC02_TDMg3$p2inp1vec <- is.element(GCFC02_TDMg3$player2, player1vector)

addPlayer1 <- GCFC02_TDMg3[ which(GCFC02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_TDMg3[ which(GCFC02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_TDMg2 <- rbind(GCFC02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
GCFC02_TDMft <- ftable(GCFC02_TDMg2$player1, GCFC02_TDMg2$player2)
GCFC02_TDMft2 <- as.matrix(GCFC02_TDMft)
numRows <- nrow(GCFC02_TDMft2)
numCols <- ncol(GCFC02_TDMft2)
GCFC02_TDMft3 <- GCFC02_TDMft2[c(2:numRows) , c(2:numCols)]
GCFC02_TDMTable <- graph.adjacency(GCFC02_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(GCFC02_TDMTable, vertex.label = V(GCFC02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
GCFC02_TDM.clusterCoef <- transitivity(GCFC02_TDMTable, type="global") #cluster coefficient
GCFC02_TDM.degreeCent <- centralization.degree(GCFC02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_TDMftn <- as.network.matrix(GCFC02_TDMft)
GCFC02_TDM.netDensity <- network.density(GCFC02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_TDM.entropy <- entropy(GCFC02_TDMft) #entropy

GCFC02_TDM.netMx <- cbind(GCFC02_TDM.netMx, GCFC02_TDM.clusterCoef, GCFC02_TDM.degreeCent$centralization,
                          GCFC02_TDM.netDensity, GCFC02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "GCFC"
KIoutcome = "Stoppage_D"
GCFC02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
GCFC02_SDg2 <- data.frame(GCFC02_SD)
GCFC02_SDg2 <- GCFC02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_SDg2$player1
player2vector <- GCFC02_SDg2$player2
GCFC02_SDg3 <- GCFC02_SDg2
GCFC02_SDg3$p1inp2vec <- is.element(GCFC02_SDg3$player1, player2vector)
GCFC02_SDg3$p2inp1vec <- is.element(GCFC02_SDg3$player2, player1vector)

addPlayer1 <- GCFC02_SDg3[ which(GCFC02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_SDg3[ which(GCFC02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_SDg2 <- rbind(GCFC02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
GCFC02_SDft <- ftable(GCFC02_SDg2$player1, GCFC02_SDg2$player2)
GCFC02_SDft2 <- as.matrix(GCFC02_SDft)
numRows <- nrow(GCFC02_SDft2)
numCols <- ncol(GCFC02_SDft2)
GCFC02_SDft3 <- GCFC02_SDft2[c(2:numRows) , c(2:numCols)]
GCFC02_SDTable <- graph.adjacency(GCFC02_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(GCFC02_SDTable, vertex.label = V(GCFC02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
GCFC02_SD.clusterCoef <- transitivity(GCFC02_SDTable, type="global") #cluster coefficient
GCFC02_SD.degreeCent <- centralization.degree(GCFC02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_SDftn <- as.network.matrix(GCFC02_SDft)
GCFC02_SD.netDensity <- network.density(GCFC02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_SD.entropy <- entropy(GCFC02_SDft) #entropy

GCFC02_SD.netMx <- cbind(GCFC02_SD.netMx, GCFC02_SD.clusterCoef, GCFC02_SD.degreeCent$centralization,
                         GCFC02_SD.netDensity, GCFC02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "GCFC"
KIoutcome = "Turnover_D"
GCFC02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
GCFC02_TDg2 <- data.frame(GCFC02_TD)
GCFC02_TDg2 <- GCFC02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_TDg2$player1
player2vector <- GCFC02_TDg2$player2
GCFC02_TDg3 <- GCFC02_TDg2
GCFC02_TDg3$p1inp2vec <- is.element(GCFC02_TDg3$player1, player2vector)
GCFC02_TDg3$p2inp1vec <- is.element(GCFC02_TDg3$player2, player1vector)

addPlayer1 <- GCFC02_TDg3[ which(GCFC02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_TDg3[ which(GCFC02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_TDg2 <- rbind(GCFC02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
GCFC02_TDft <- ftable(GCFC02_TDg2$player1, GCFC02_TDg2$player2)
GCFC02_TDft2 <- as.matrix(GCFC02_TDft)
numRows <- nrow(GCFC02_TDft2)
numCols <- ncol(GCFC02_TDft2)
GCFC02_TDft3 <- GCFC02_TDft2[c(2:numRows) , c(2:numCols)]
GCFC02_TDTable <- graph.adjacency(GCFC02_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(GCFC02_TDTable, vertex.label = V(GCFC02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
GCFC02_TD.clusterCoef <- transitivity(GCFC02_TDTable, type="global") #cluster coefficient
GCFC02_TD.degreeCent <- centralization.degree(GCFC02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_TDftn <- as.network.matrix(GCFC02_TDft)
GCFC02_TD.netDensity <- network.density(GCFC02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_TD.entropy <- entropy(GCFC02_TDft) #entropy

GCFC02_TD.netMx <- cbind(GCFC02_TD.netMx, GCFC02_TD.clusterCoef, GCFC02_TD.degreeCent$centralization,
                         GCFC02_TD.netDensity, GCFC02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "GCFC"
KIoutcome = "End of Qtr_DM"
GCFC02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
GCFC02_QTg2 <- data.frame(GCFC02_QT)
GCFC02_QTg2 <- GCFC02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GCFC02_QTg2$player1
player2vector <- GCFC02_QTg2$player2
GCFC02_QTg3 <- GCFC02_QTg2
GCFC02_QTg3$p1inp2vec <- is.element(GCFC02_QTg3$player1, player2vector)
GCFC02_QTg3$p2inp1vec <- is.element(GCFC02_QTg3$player2, player1vector)

addPlayer1 <- GCFC02_QTg3[ which(GCFC02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GCFC02_QTg3[ which(GCFC02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GCFC02_QTg2 <- rbind(GCFC02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
GCFC02_QTft <- ftable(GCFC02_QTg2$player1, GCFC02_QTg2$player2)
GCFC02_QTft2 <- as.matrix(GCFC02_QTft)
numRows <- nrow(GCFC02_QTft2)
numCols <- ncol(GCFC02_QTft2)
GCFC02_QTft3 <- GCFC02_QTft2[c(2:numRows) , c(2:numCols)]
GCFC02_QTTable <- graph.adjacency(GCFC02_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(GCFC02_QTTable, vertex.label = V(GCFC02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GCFC02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
GCFC02_QT.clusterCoef <- transitivity(GCFC02_QTTable, type="global") #cluster coefficient
GCFC02_QT.degreeCent <- centralization.degree(GCFC02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GCFC02_QTftn <- as.network.matrix(GCFC02_QTft)
GCFC02_QT.netDensity <- network.density(GCFC02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GCFC02_QT.entropy <- entropy(GCFC02_QTft) #entropy

GCFC02_QT.netMx <- cbind(GCFC02_QT.netMx, GCFC02_QT.clusterCoef, GCFC02_QT.degreeCent$centralization,
                         GCFC02_QT.netDensity, GCFC02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GCFC02_QT.netMx) <- varnames

#############################################################################
#GEELONG

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "GEEL"
KIoutcome = "Goal_F"
GEEL02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
GEEL02_Gg2 <- data.frame(GEEL02_G)
GEEL02_Gg2 <- GEEL02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_Gg2$player1
player2vector <- GEEL02_Gg2$player2
GEEL02_Gg3 <- GEEL02_Gg2
GEEL02_Gg3$p1inp2vec <- is.element(GEEL02_Gg3$player1, player2vector)
GEEL02_Gg3$p2inp1vec <- is.element(GEEL02_Gg3$player2, player1vector)

addPlayer1 <- GEEL02_Gg3[ which(GEEL02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_Gg3[ which(GEEL02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_Gg2 <- rbind(GEEL02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
GEEL02_Gft <- ftable(GEEL02_Gg2$player1, GEEL02_Gg2$player2)
GEEL02_Gft2 <- as.matrix(GEEL02_Gft)
numRows <- nrow(GEEL02_Gft2)
numCols <- ncol(GEEL02_Gft2)
GEEL02_Gft3 <- GEEL02_Gft2[c(2:numRows) , c(2:numCols)]
GEEL02_GTable <- graph.adjacency(GEEL02_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(GEEL02_GTable, vertex.label = V(GEEL02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
GEEL02_G.clusterCoef <- transitivity(GEEL02_GTable, type="global") #cluster coefficient
GEEL02_G.degreeCent <- centralization.degree(GEEL02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_Gftn <- as.network.matrix(GEEL02_Gft)
GEEL02_G.netDensity <- network.density(GEEL02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_G.entropy <- entropy(GEEL02_Gft) #entropy

GEEL02_G.netMx <- cbind(GEEL02_G.netMx, GEEL02_G.clusterCoef, GEEL02_G.degreeCent$centralization,
                        GEEL02_G.netDensity, GEEL02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "GEEL"
KIoutcome = "Behind_F"
GEEL02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
GEEL02_Bg2 <- data.frame(GEEL02_B)
GEEL02_Bg2 <- GEEL02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_Bg2$player1
player2vector <- GEEL02_Bg2$player2
GEEL02_Bg3 <- GEEL02_Bg2
GEEL02_Bg3$p1inp2vec <- is.element(GEEL02_Bg3$player1, player2vector)
GEEL02_Bg3$p2inp1vec <- is.element(GEEL02_Bg3$player2, player1vector)

addPlayer1 <- GEEL02_Bg3[ which(GEEL02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_Bg3[ which(GEEL02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_Bg2 <- rbind(GEEL02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
GEEL02_Bft <- ftable(GEEL02_Bg2$player1, GEEL02_Bg2$player2)
GEEL02_Bft2 <- as.matrix(GEEL02_Bft)
numRows <- nrow(GEEL02_Bft2)
numCols <- ncol(GEEL02_Bft2)
GEEL02_Bft3 <- GEEL02_Bft2[c(2:numRows) , c(2:numCols)]
GEEL02_BTable <- graph.adjacency(GEEL02_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(GEEL02_BTable, vertex.label = V(GEEL02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
GEEL02_B.clusterCoef <- transitivity(GEEL02_BTable, type="global") #cluster coefficient
GEEL02_B.degreeCent <- centralization.degree(GEEL02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_Bftn <- as.network.matrix(GEEL02_Bft)
GEEL02_B.netDensity <- network.density(GEEL02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_B.entropy <- entropy(GEEL02_Bft) #entropy

GEEL02_B.netMx <- cbind(GEEL02_B.netMx, GEEL02_B.clusterCoef, GEEL02_B.degreeCent$centralization,
                        GEEL02_B.netDensity, GEEL02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "GEEL"
KIoutcome = "Stoppage_F"
GEEL02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
GEEL02_SFg2 <- data.frame(GEEL02_SF)
GEEL02_SFg2 <- GEEL02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_SFg2$player1
player2vector <- GEEL02_SFg2$player2
GEEL02_SFg3 <- GEEL02_SFg2
GEEL02_SFg3$p1inp2vec <- is.element(GEEL02_SFg3$player1, player2vector)
GEEL02_SFg3$p2inp1vec <- is.element(GEEL02_SFg3$player2, player1vector)

addPlayer1 <- GEEL02_SFg3[ which(GEEL02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_SFg3[ which(GEEL02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_SFg2 <- rbind(GEEL02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
GEEL02_SFft <- ftable(GEEL02_SFg2$player1, GEEL02_SFg2$player2)
GEEL02_SFft2 <- as.matrix(GEEL02_SFft)
numRows <- nrow(GEEL02_SFft2)
numCols <- ncol(GEEL02_SFft2)
GEEL02_SFft3 <- GEEL02_SFft2[c(2:numRows) , c(2:numCols)]
GEEL02_SFTable <- graph.adjacency(GEEL02_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(GEEL02_SFTable, vertex.label = V(GEEL02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
GEEL02_SF.clusterCoef <- transitivity(GEEL02_SFTable, type="global") #cluster coefficient
GEEL02_SF.degreeCent <- centralization.degree(GEEL02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_SFftn <- as.network.matrix(GEEL02_SFft)
GEEL02_SF.netDensity <- network.density(GEEL02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_SF.entropy <- entropy(GEEL02_SFft) #entropy

GEEL02_SF.netMx <- cbind(GEEL02_SF.netMx, GEEL02_SF.clusterCoef, GEEL02_SF.degreeCent$centralization,
                         GEEL02_SF.netDensity, GEEL02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "GEEL"
KIoutcome = "Turnover_F"
GEEL02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
GEEL02_TFg2 <- data.frame(GEEL02_TF)
GEEL02_TFg2 <- GEEL02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_TFg2$player1
player2vector <- GEEL02_TFg2$player2
GEEL02_TFg3 <- GEEL02_TFg2
GEEL02_TFg3$p1inp2vec <- is.element(GEEL02_TFg3$player1, player2vector)
GEEL02_TFg3$p2inp1vec <- is.element(GEEL02_TFg3$player2, player1vector)

addPlayer1 <- GEEL02_TFg3[ which(GEEL02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_TFg3[ which(GEEL02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_TFg2 <- rbind(GEEL02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
GEEL02_TFft <- ftable(GEEL02_TFg2$player1, GEEL02_TFg2$player2)
GEEL02_TFft2 <- as.matrix(GEEL02_TFft)
numRows <- nrow(GEEL02_TFft2)
numCols <- ncol(GEEL02_TFft2)
GEEL02_TFft3 <- GEEL02_TFft2[c(2:numRows) , c(2:numCols)]
GEEL02_TFTable <- graph.adjacency(GEEL02_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(GEEL02_TFTable, vertex.label = V(GEEL02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
GEEL02_TF.clusterCoef <- transitivity(GEEL02_TFTable, type="global") #cluster coefficient
GEEL02_TF.degreeCent <- centralization.degree(GEEL02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_TFftn <- as.network.matrix(GEEL02_TFft)
GEEL02_TF.netDensity <- network.density(GEEL02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_TF.entropy <- entropy(GEEL02_TFft) #entropy

GEEL02_TF.netMx <- cbind(GEEL02_TF.netMx, GEEL02_TF.clusterCoef, GEEL02_TF.degreeCent$centralization,
                         GEEL02_TF.netDensity, GEEL02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "GEEL"
KIoutcome = "Stoppage_AM"
GEEL02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
GEEL02_SAMg2 <- data.frame(GEEL02_SAM)
GEEL02_SAMg2 <- GEEL02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_SAMg2$player1
player2vector <- GEEL02_SAMg2$player2
GEEL02_SAMg3 <- GEEL02_SAMg2
GEEL02_SAMg3$p1inp2vec <- is.element(GEEL02_SAMg3$player1, player2vector)
GEEL02_SAMg3$p2inp1vec <- is.element(GEEL02_SAMg3$player2, player1vector)

addPlayer1 <- GEEL02_SAMg3[ which(GEEL02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_SAMg3[ which(GEEL02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_SAMg2 <- rbind(GEEL02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
GEEL02_SAMft <- ftable(GEEL02_SAMg2$player1, GEEL02_SAMg2$player2)
GEEL02_SAMft2 <- as.matrix(GEEL02_SAMft)
numRows <- nrow(GEEL02_SAMft2)
numCols <- ncol(GEEL02_SAMft2)
GEEL02_SAMft3 <- GEEL02_SAMft2[c(2:numRows) , c(2:numCols)]
GEEL02_SAMTable <- graph.adjacency(GEEL02_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(GEEL02_SAMTable, vertex.label = V(GEEL02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
GEEL02_SAM.clusterCoef <- transitivity(GEEL02_SAMTable, type="global") #cluster coefficient
GEEL02_SAM.degreeCent <- centralization.degree(GEEL02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_SAMftn <- as.network.matrix(GEEL02_SAMft)
GEEL02_SAM.netDensity <- network.density(GEEL02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_SAM.entropy <- entropy(GEEL02_SAMft) #entropy

GEEL02_SAM.netMx <- cbind(GEEL02_SAM.netMx, GEEL02_SAM.clusterCoef, GEEL02_SAM.degreeCent$centralization,
                          GEEL02_SAM.netDensity, GEEL02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "GEEL"
KIoutcome = "Turnover_AM"
GEEL02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
GEEL02_TAMg2 <- data.frame(GEEL02_TAM)
GEEL02_TAMg2 <- GEEL02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_TAMg2$player1
player2vector <- GEEL02_TAMg2$player2
GEEL02_TAMg3 <- GEEL02_TAMg2
GEEL02_TAMg3$p1inp2vec <- is.element(GEEL02_TAMg3$player1, player2vector)
GEEL02_TAMg3$p2inp1vec <- is.element(GEEL02_TAMg3$player2, player1vector)

addPlayer1 <- GEEL02_TAMg3[ which(GEEL02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_TAMg3[ which(GEEL02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_TAMg2 <- rbind(GEEL02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
GEEL02_TAMft <- ftable(GEEL02_TAMg2$player1, GEEL02_TAMg2$player2)
GEEL02_TAMft2 <- as.matrix(GEEL02_TAMft)
numRows <- nrow(GEEL02_TAMft2)
numCols <- ncol(GEEL02_TAMft2)
GEEL02_TAMft3 <- GEEL02_TAMft2[c(2:numRows) , c(2:numCols)]
GEEL02_TAMTable <- graph.adjacency(GEEL02_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(GEEL02_TAMTable, vertex.label = V(GEEL02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
GEEL02_TAM.clusterCoef <- transitivity(GEEL02_TAMTable, type="global") #cluster coefficient
GEEL02_TAM.degreeCent <- centralization.degree(GEEL02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_TAMftn <- as.network.matrix(GEEL02_TAMft)
GEEL02_TAM.netDensity <- network.density(GEEL02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_TAM.entropy <- entropy(GEEL02_TAMft) #entropy

GEEL02_TAM.netMx <- cbind(GEEL02_TAM.netMx, GEEL02_TAM.clusterCoef, GEEL02_TAM.degreeCent$centralization,
                          GEEL02_TAM.netDensity, GEEL02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "GEEL"
KIoutcome = "Stoppage_DM"
GEEL02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
GEEL02_SDMg2 <- data.frame(GEEL02_SDM)
GEEL02_SDMg2 <- GEEL02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_SDMg2$player1
player2vector <- GEEL02_SDMg2$player2
GEEL02_SDMg3 <- GEEL02_SDMg2
GEEL02_SDMg3$p1inp2vec <- is.element(GEEL02_SDMg3$player1, player2vector)
GEEL02_SDMg3$p2inp1vec <- is.element(GEEL02_SDMg3$player2, player1vector)

addPlayer1 <- GEEL02_SDMg3[ which(GEEL02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_SDMg3[ which(GEEL02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_SDMg2 <- rbind(GEEL02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
GEEL02_SDMft <- ftable(GEEL02_SDMg2$player1, GEEL02_SDMg2$player2)
GEEL02_SDMft2 <- as.matrix(GEEL02_SDMft)
numRows <- nrow(GEEL02_SDMft2)
numCols <- ncol(GEEL02_SDMft2)
GEEL02_SDMft3 <- GEEL02_SDMft2[c(2:numRows) , c(2:numCols)]
GEEL02_SDMTable <- graph.adjacency(GEEL02_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(GEEL02_SDMTable, vertex.label = V(GEEL02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
GEEL02_SDM.clusterCoef <- transitivity(GEEL02_SDMTable, type="global") #cluster coefficient
GEEL02_SDM.degreeCent <- centralization.degree(GEEL02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_SDMftn <- as.network.matrix(GEEL02_SDMft)
GEEL02_SDM.netDensity <- network.density(GEEL02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_SDM.entropy <- entropy(GEEL02_SDMft) #entropy

GEEL02_SDM.netMx <- cbind(GEEL02_SDM.netMx, GEEL02_SDM.clusterCoef, GEEL02_SDM.degreeCent$centralization,
                          GEEL02_SDM.netDensity, GEEL02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "GEEL"
KIoutcome = "Turnover_DM"
GEEL02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
GEEL02_TDMg2 <- data.frame(GEEL02_TDM)
GEEL02_TDMg2 <- GEEL02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_TDMg2$player1
player2vector <- GEEL02_TDMg2$player2
GEEL02_TDMg3 <- GEEL02_TDMg2
GEEL02_TDMg3$p1inp2vec <- is.element(GEEL02_TDMg3$player1, player2vector)
GEEL02_TDMg3$p2inp1vec <- is.element(GEEL02_TDMg3$player2, player1vector)

addPlayer1 <- GEEL02_TDMg3[ which(GEEL02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_TDMg3[ which(GEEL02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_TDMg2 <- rbind(GEEL02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
GEEL02_TDMft <- ftable(GEEL02_TDMg2$player1, GEEL02_TDMg2$player2)
GEEL02_TDMft2 <- as.matrix(GEEL02_TDMft)
numRows <- nrow(GEEL02_TDMft2)
numCols <- ncol(GEEL02_TDMft2)
GEEL02_TDMft3 <- GEEL02_TDMft2[c(2:numRows) , c(2:numCols)]
GEEL02_TDMTable <- graph.adjacency(GEEL02_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(GEEL02_TDMTable, vertex.label = V(GEEL02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
GEEL02_TDM.clusterCoef <- transitivity(GEEL02_TDMTable, type="global") #cluster coefficient
GEEL02_TDM.degreeCent <- centralization.degree(GEEL02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_TDMftn <- as.network.matrix(GEEL02_TDMft)
GEEL02_TDM.netDensity <- network.density(GEEL02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_TDM.entropy <- entropy(GEEL02_TDMft) #entropy

GEEL02_TDM.netMx <- cbind(GEEL02_TDM.netMx, GEEL02_TDM.clusterCoef, GEEL02_TDM.degreeCent$centralization,
                          GEEL02_TDM.netDensity, GEEL02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "GEEL"
KIoutcome = "Stoppage_D"
GEEL02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
GEEL02_SDg2 <- data.frame(GEEL02_SD)
GEEL02_SDg2 <- GEEL02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_SDg2$player1
player2vector <- GEEL02_SDg2$player2
GEEL02_SDg3 <- GEEL02_SDg2
GEEL02_SDg3$p1inp2vec <- is.element(GEEL02_SDg3$player1, player2vector)
GEEL02_SDg3$p2inp1vec <- is.element(GEEL02_SDg3$player2, player1vector)

addPlayer1 <- GEEL02_SDg3[ which(GEEL02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_SDg3[ which(GEEL02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_SDg2 <- rbind(GEEL02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
GEEL02_SDft <- ftable(GEEL02_SDg2$player1, GEEL02_SDg2$player2)
GEEL02_SDft2 <- as.matrix(GEEL02_SDft)
numRows <- nrow(GEEL02_SDft2)
numCols <- ncol(GEEL02_SDft2)
GEEL02_SDft3 <- GEEL02_SDft2[c(2:numRows) , c(2:numCols)]
GEEL02_SDTable <- graph.adjacency(GEEL02_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(GEEL02_SDTable, vertex.label = V(GEEL02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
GEEL02_SD.clusterCoef <- transitivity(GEEL02_SDTable, type="global") #cluster coefficient
GEEL02_SD.degreeCent <- centralization.degree(GEEL02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_SDftn <- as.network.matrix(GEEL02_SDft)
GEEL02_SD.netDensity <- network.density(GEEL02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_SD.entropy <- entropy(GEEL02_SDft) #entropy

GEEL02_SD.netMx <- cbind(GEEL02_SD.netMx, GEEL02_SD.clusterCoef, GEEL02_SD.degreeCent$centralization,
                         GEEL02_SD.netDensity, GEEL02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "GEEL"
KIoutcome = "Turnover_D"
GEEL02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
GEEL02_TDg2 <- data.frame(GEEL02_TD)
GEEL02_TDg2 <- GEEL02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_TDg2$player1
player2vector <- GEEL02_TDg2$player2
GEEL02_TDg3 <- GEEL02_TDg2
GEEL02_TDg3$p1inp2vec <- is.element(GEEL02_TDg3$player1, player2vector)
GEEL02_TDg3$p2inp1vec <- is.element(GEEL02_TDg3$player2, player1vector)

addPlayer1 <- GEEL02_TDg3[ which(GEEL02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_TDg3[ which(GEEL02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_TDg2 <- rbind(GEEL02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
GEEL02_TDft <- ftable(GEEL02_TDg2$player1, GEEL02_TDg2$player2)
GEEL02_TDft2 <- as.matrix(GEEL02_TDft)
numRows <- nrow(GEEL02_TDft2)
numCols <- ncol(GEEL02_TDft2)
GEEL02_TDft3 <- GEEL02_TDft2[c(2:numRows) , c(2:numCols)]
GEEL02_TDTable <- graph.adjacency(GEEL02_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(GEEL02_TDTable, vertex.label = V(GEEL02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
GEEL02_TD.clusterCoef <- transitivity(GEEL02_TDTable, type="global") #cluster coefficient
GEEL02_TD.degreeCent <- centralization.degree(GEEL02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_TDftn <- as.network.matrix(GEEL02_TDft)
GEEL02_TD.netDensity <- network.density(GEEL02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_TD.entropy <- entropy(GEEL02_TDft) #entropy

GEEL02_TD.netMx <- cbind(GEEL02_TD.netMx, GEEL02_TD.clusterCoef, GEEL02_TD.degreeCent$centralization,
                         GEEL02_TD.netDensity, GEEL02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "GEEL"
KIoutcome = "End of Qtr_DM"
GEEL02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
GEEL02_QTg2 <- data.frame(GEEL02_QT)
GEEL02_QTg2 <- GEEL02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GEEL02_QTg2$player1
player2vector <- GEEL02_QTg2$player2
GEEL02_QTg3 <- GEEL02_QTg2
GEEL02_QTg3$p1inp2vec <- is.element(GEEL02_QTg3$player1, player2vector)
GEEL02_QTg3$p2inp1vec <- is.element(GEEL02_QTg3$player2, player1vector)

addPlayer1 <- GEEL02_QTg3[ which(GEEL02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GEEL02_QTg3[ which(GEEL02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GEEL02_QTg2 <- rbind(GEEL02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
GEEL02_QTft <- ftable(GEEL02_QTg2$player1, GEEL02_QTg2$player2)
GEEL02_QTft2 <- as.matrix(GEEL02_QTft)
numRows <- nrow(GEEL02_QTft2)
numCols <- ncol(GEEL02_QTft2)
GEEL02_QTft3 <- GEEL02_QTft2[c(2:numRows) , c(2:numCols)]
GEEL02_QTTable <- graph.adjacency(GEEL02_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(GEEL02_QTTable, vertex.label = V(GEEL02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GEEL02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
GEEL02_QT.clusterCoef <- transitivity(GEEL02_QTTable, type="global") #cluster coefficient
GEEL02_QT.degreeCent <- centralization.degree(GEEL02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GEEL02_QTftn <- as.network.matrix(GEEL02_QTft)
GEEL02_QT.netDensity <- network.density(GEEL02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GEEL02_QT.entropy <- entropy(GEEL02_QTft) #entropy

GEEL02_QT.netMx <- cbind(GEEL02_QT.netMx, GEEL02_QT.clusterCoef, GEEL02_QT.degreeCent$centralization,
                         GEEL02_QT.netDensity, GEEL02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GEEL02_QT.netMx) <- varnames

#############################################################################
#GREATER WESTERN SYDNEY

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "GWS"
KIoutcome = "Goal_F"
GWS02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
GWS02_Gg2 <- data.frame(GWS02_G)
GWS02_Gg2 <- GWS02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_Gg2$player1
player2vector <- GWS02_Gg2$player2
GWS02_Gg3 <- GWS02_Gg2
GWS02_Gg3$p1inp2vec <- is.element(GWS02_Gg3$player1, player2vector)
GWS02_Gg3$p2inp1vec <- is.element(GWS02_Gg3$player2, player1vector)

addPlayer1 <- GWS02_Gg3[ which(GWS02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS02_Gg3[ which(GWS02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS02_Gg2 <- rbind(GWS02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
GWS02_Gft <- ftable(GWS02_Gg2$player1, GWS02_Gg2$player2)
GWS02_Gft2 <- as.matrix(GWS02_Gft)
numRows <- nrow(GWS02_Gft2)
numCols <- ncol(GWS02_Gft2)
GWS02_Gft3 <- GWS02_Gft2[c(1:numRows) , c(1:numCols)]
GWS02_GTable <- graph.adjacency(GWS02_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(GWS02_GTable, vertex.label = V(GWS02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
GWS02_G.clusterCoef <- transitivity(GWS02_GTable, type="global") #cluster coefficient
GWS02_G.degreeCent <- centralization.degree(GWS02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_Gftn <- as.network.matrix(GWS02_Gft)
GWS02_G.netDensity <- network.density(GWS02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_G.entropy <- entropy(GWS02_Gft) #entropy

GWS02_G.netMx <- cbind(GWS02_G.netMx, GWS02_G.clusterCoef, GWS02_G.degreeCent$centralization,
                       GWS02_G.netDensity, GWS02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "GWS"
KIoutcome = "Behind_F"
GWS02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
GWS02_Bg2 <- data.frame(GWS02_B)
GWS02_Bg2 <- GWS02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_Bg2$player1
player2vector <- GWS02_Bg2$player2
GWS02_Bg3 <- GWS02_Bg2
GWS02_Bg3$p1inp2vec <- is.element(GWS02_Bg3$player1, player2vector)
GWS02_Bg3$p2inp1vec <- is.element(GWS02_Bg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS02_Bg3[ which(GWS02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS02_Bg2 <- rbind(GWS02_Bg2, addPlayer2)

#Round 2, Behind graph using weighted edges
GWS02_Bft <- ftable(GWS02_Bg2$player1, GWS02_Bg2$player2)
GWS02_Bft2 <- as.matrix(GWS02_Bft)
numRows <- nrow(GWS02_Bft2)
numCols <- ncol(GWS02_Bft2)
GWS02_Bft3 <- GWS02_Bft2[c(1:numRows) , c(2:numCols)]
GWS02_BTable <- graph.adjacency(GWS02_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(GWS02_BTable, vertex.label = V(GWS02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
GWS02_B.clusterCoef <- transitivity(GWS02_BTable, type="global") #cluster coefficient
GWS02_B.degreeCent <- centralization.degree(GWS02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_Bftn <- as.network.matrix(GWS02_Bft)
GWS02_B.netDensity <- network.density(GWS02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_B.entropy <- entropy(GWS02_Bft) #entropy

GWS02_B.netMx <- cbind(GWS02_B.netMx, GWS02_B.clusterCoef, GWS02_B.degreeCent$centralization,
                       GWS02_B.netDensity, GWS02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************

round = 2
teamName = "GWS"
KIoutcome = "Stoppage_F"
GWS02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
GWS02_SFg2 <- data.frame(GWS02_SF)
GWS02_SFg2 <- GWS02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_SFg2$player1
player2vector <- GWS02_SFg2$player2
GWS02_SFg3 <- GWS02_SFg2
GWS02_SFg3$p1inp2vec <- is.element(GWS02_SFg3$player1, player2vector)
GWS02_SFg3$p2inp1vec <- is.element(GWS02_SFg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS02_SFg3[ which(GWS02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS02_SFg2 <- rbind(GWS02_SFg2, addPlayer2)

#Round 2, FWD Stoppage graph using weighted edges
GWS02_SFft <- ftable(GWS02_SFg2$player1, GWS02_SFg2$player2)
GWS02_SFft2 <- as.matrix(GWS02_SFft)
numRows <- nrow(GWS02_SFft2)
numCols <- ncol(GWS02_SFft2)
GWS02_SFft3 <- GWS02_SFft2[c(1:numRows) , c(2:numCols)]
GWS02_SFTable <- graph.adjacency(GWS02_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(GWS02_SFTable, vertex.label = V(GWS02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
GWS02_SF.clusterCoef <- transitivity(GWS02_SFTable, type="global") #cluster coefficient
GWS02_SF.degreeCent <- centralization.degree(GWS02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_SFftn <- as.network.matrix(GWS02_SFft)
GWS02_SF.netDensity <- network.density(GWS02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_SF.entropy <- entropy(GWS02_SFft) #entropy

GWS02_SF.netMx <- cbind(GWS02_SF.netMx, GWS02_SF.clusterCoef, GWS02_SF.degreeCent$centralization,
                        GWS02_SF.netDensity, GWS02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "GWS"
KIoutcome = "Turnover_F"
GWS02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
GWS02_TFg2 <- data.frame(GWS02_TF)
GWS02_TFg2 <- GWS02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_TFg2$player1
player2vector <- GWS02_TFg2$player2
GWS02_TFg3 <- GWS02_TFg2
GWS02_TFg3$p1inp2vec <- is.element(GWS02_TFg3$player1, player2vector)
GWS02_TFg3$p2inp1vec <- is.element(GWS02_TFg3$player2, player1vector)

addPlayer1 <- GWS02_TFg3[ which(GWS02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

GWS02_TFg2 <- rbind(GWS02_TFg2, addPlayer1)

#Round 2, FWD Turnover graph using weighted edges
GWS02_TFft <- ftable(GWS02_TFg2$player1, GWS02_TFg2$player2)
GWS02_TFft2 <- as.matrix(GWS02_TFft)
numRows <- nrow(GWS02_TFft2)
numCols <- ncol(GWS02_TFft2)
GWS02_TFft3 <- GWS02_TFft2[c(2:numRows) , c(1:numCols)]
GWS02_TFTable <- graph.adjacency(GWS02_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(GWS02_TFTable, vertex.label = V(GWS02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
GWS02_TF.clusterCoef <- transitivity(GWS02_TFTable, type="global") #cluster coefficient
GWS02_TF.degreeCent <- centralization.degree(GWS02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_TFftn <- as.network.matrix(GWS02_TFft)
GWS02_TF.netDensity <- network.density(GWS02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_TF.entropy <- entropy(GWS02_TFft) #entropy

GWS02_TF.netMx <- cbind(GWS02_TF.netMx, GWS02_TF.clusterCoef, GWS02_TF.degreeCent$centralization,
                        GWS02_TF.netDensity, GWS02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************

round = 2
teamName = "GWS"
KIoutcome = "Stoppage_AM"
GWS02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
GWS02_SAMg2 <- data.frame(GWS02_SAM)
GWS02_SAMg2 <- GWS02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_SAMg2$player1
player2vector <- GWS02_SAMg2$player2
GWS02_SAMg3 <- GWS02_SAMg2
GWS02_SAMg3$p1inp2vec <- is.element(GWS02_SAMg3$player1, player2vector)
GWS02_SAMg3$p2inp1vec <- is.element(GWS02_SAMg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- GWS02_SAMg3[ which(GWS02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

GWS02_SAMg2 <- rbind(GWS02_SAMg2, addPlayer2)

#Round 2, AM Stoppage graph using weighted edges
GWS02_SAMft <- ftable(GWS02_SAMg2$player1, GWS02_SAMg2$player2)
GWS02_SAMft2 <- as.matrix(GWS02_SAMft)
numRows <- nrow(GWS02_SAMft2)
numCols <- ncol(GWS02_SAMft2)
GWS02_SAMft3 <- GWS02_SAMft2[c(1:numRows) , c(2:numCols)]
GWS02_SAMTable <- graph.adjacency(GWS02_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(GWS02_SAMTable, vertex.label = V(GWS02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
GWS02_SAM.clusterCoef <- transitivity(GWS02_SAMTable, type="global") #cluster coefficient
GWS02_SAM.degreeCent <- centralization.degree(GWS02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_SAMftn <- as.network.matrix(GWS02_SAMft)
GWS02_SAM.netDensity <- network.density(GWS02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_SAM.entropy <- entropy(GWS02_SAMft) #entropy

GWS02_SAM.netMx <- cbind(GWS02_SAM.netMx, GWS02_SAM.clusterCoef, GWS02_SAM.degreeCent$centralization,
                         GWS02_SAM.netDensity, GWS02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************
#NA

round = 2
teamName = "GWS"
KIoutcome = "Turnover_AM"
GWS02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
GWS02_TAMg2 <- data.frame(GWS02_TAM)
GWS02_TAMg2 <- GWS02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_TAMg2$player1
player2vector <- GWS02_TAMg2$player2
GWS02_TAMg3 <- GWS02_TAMg2
GWS02_TAMg3$p1inp2vec <- is.element(GWS02_TAMg3$player1, player2vector)
GWS02_TAMg3$p2inp1vec <- is.element(GWS02_TAMg3$player2, player1vector)

addPlayer1 <- GWS02_TAMg3[ which(GWS02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS02_TAMg3[ which(GWS02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS02_TAMg2 <- rbind(GWS02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
GWS02_TAMft <- ftable(GWS02_TAMg2$player1, GWS02_TAMg2$player2)
GWS02_TAMft2 <- as.matrix(GWS02_TAMft)
numRows <- nrow(GWS02_TAMft2)
numCols <- ncol(GWS02_TAMft2)
GWS02_TAMft3 <- GWS02_TAMft2[c(2:numRows) , c(2:numCols)]
GWS02_TAMTable <- graph.adjacency(GWS02_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(GWS02_TAMTable, vertex.label = V(GWS02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
GWS02_TAM.clusterCoef <- transitivity(GWS02_TAMTable, type="global") #cluster coefficient
GWS02_TAM.degreeCent <- centralization.degree(GWS02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_TAMftn <- as.network.matrix(GWS02_TAMft)
GWS02_TAM.netDensity <- network.density(GWS02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_TAM.entropy <- entropy(GWS02_TAMft) #entropy

GWS02_TAM.netMx <- cbind(GWS02_TAM.netMx, GWS02_TAM.clusterCoef, GWS02_TAM.degreeCent$centralization,
                         GWS02_TAM.netDensity, GWS02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************
#NA

round = 2
teamName = "GWS"
KIoutcome = "Stoppage_DM"
GWS02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
GWS02_SDMg2 <- data.frame(GWS02_SDM)
GWS02_SDMg2 <- GWS02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_SDMg2$player1
player2vector <- GWS02_SDMg2$player2
GWS02_SDMg3 <- GWS02_SDMg2
GWS02_SDMg3$p1inp2vec <- is.element(GWS02_SDMg3$player1, player2vector)
GWS02_SDMg3$p2inp1vec <- is.element(GWS02_SDMg3$player2, player1vector)

addPlayer1 <- GWS02_SDMg3[ which(GWS02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS02_SDMg3[ which(GWS02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS02_SDMg2 <- rbind(GWS02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
GWS02_SDMft <- ftable(GWS02_SDMg2$player1, GWS02_SDMg2$player2)
GWS02_SDMft2 <- as.matrix(GWS02_SDMft)
numRows <- nrow(GWS02_SDMft2)
numCols <- ncol(GWS02_SDMft2)
GWS02_SDMft3 <- GWS02_SDMft2[c(2:numRows) , c(2:numCols)]
GWS02_SDMTable <- graph.adjacency(GWS02_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(GWS02_SDMTable, vertex.label = V(GWS02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
GWS02_SDM.clusterCoef <- transitivity(GWS02_SDMTable, type="global") #cluster coefficient
GWS02_SDM.degreeCent <- centralization.degree(GWS02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_SDMftn <- as.network.matrix(GWS02_SDMft)
GWS02_SDM.netDensity <- network.density(GWS02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_SDM.entropy <- entropy(GWS02_SDMft) #entropy

GWS02_SDM.netMx <- cbind(GWS02_SDM.netMx, GWS02_SDM.clusterCoef, GWS02_SDM.degreeCent$centralization,
                         GWS02_SDM.netDensity, GWS02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************
#NA

round = 2
teamName = "GWS"
KIoutcome = "Turnover_DM"
GWS02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
GWS02_TDMg2 <- data.frame(GWS02_TDM)
GWS02_TDMg2 <- GWS02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_TDMg2$player1
player2vector <- GWS02_TDMg2$player2
GWS02_TDMg3 <- GWS02_TDMg2
GWS02_TDMg3$p1inp2vec <- is.element(GWS02_TDMg3$player1, player2vector)
GWS02_TDMg3$p2inp1vec <- is.element(GWS02_TDMg3$player2, player1vector)

addPlayer1 <- GWS02_TDMg3[ which(GWS02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS02_TDMg3[ which(GWS02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS02_TDMg2 <- rbind(GWS02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
GWS02_TDMft <- ftable(GWS02_TDMg2$player1, GWS02_TDMg2$player2)
GWS02_TDMft2 <- as.matrix(GWS02_TDMft)
numRows <- nrow(GWS02_TDMft2)
numCols <- ncol(GWS02_TDMft2)
GWS02_TDMft3 <- GWS02_TDMft2[c(2:numRows) , c(2:numCols)]
GWS02_TDMTable <- graph.adjacency(GWS02_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(GWS02_TDMTable, vertex.label = V(GWS02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
GWS02_TDM.clusterCoef <- transitivity(GWS02_TDMTable, type="global") #cluster coefficient
GWS02_TDM.degreeCent <- centralization.degree(GWS02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_TDMftn <- as.network.matrix(GWS02_TDMft)
GWS02_TDM.netDensity <- network.density(GWS02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_TDM.entropy <- entropy(GWS02_TDMft) #entropy

GWS02_TDM.netMx <- cbind(GWS02_TDM.netMx, GWS02_TDM.clusterCoef, GWS02_TDM.degreeCent$centralization,
                         GWS02_TDM.netDensity, GWS02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "GWS"
KIoutcome = "Stoppage_D"
GWS02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
GWS02_SDg2 <- data.frame(GWS02_SD)
GWS02_SDg2 <- GWS02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_SDg2$player1
player2vector <- GWS02_SDg2$player2
GWS02_SDg3 <- GWS02_SDg2
GWS02_SDg3$p1inp2vec <- is.element(GWS02_SDg3$player1, player2vector)
GWS02_SDg3$p2inp1vec <- is.element(GWS02_SDg3$player2, player1vector)

addPlayer1 <- GWS02_SDg3[ which(GWS02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS02_SDg3[ which(GWS02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS02_SDg2 <- rbind(GWS02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
GWS02_SDft <- ftable(GWS02_SDg2$player1, GWS02_SDg2$player2)
GWS02_SDft2 <- as.matrix(GWS02_SDft)
numRows <- nrow(GWS02_SDft2)
numCols <- ncol(GWS02_SDft2)
GWS02_SDft3 <- GWS02_SDft2[c(2:numRows) , c(2:numCols)]
GWS02_SDTable <- graph.adjacency(GWS02_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(GWS02_SDTable, vertex.label = V(GWS02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
GWS02_SD.clusterCoef <- transitivity(GWS02_SDTable, type="global") #cluster coefficient
GWS02_SD.degreeCent <- centralization.degree(GWS02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_SDftn <- as.network.matrix(GWS02_SDft)
GWS02_SD.netDensity <- network.density(GWS02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_SD.entropy <- entropy(GWS02_SDft) #entropy

GWS02_SD.netMx <- cbind(GWS02_SD.netMx, GWS02_SD.clusterCoef, GWS02_SD.degreeCent$centralization,
                        GWS02_SD.netDensity, GWS02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "GWS"
KIoutcome = "Turnover_D"
GWS02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
GWS02_TDg2 <- data.frame(GWS02_TD)
GWS02_TDg2 <- GWS02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_TDg2$player1
player2vector <- GWS02_TDg2$player2
GWS02_TDg3 <- GWS02_TDg2
GWS02_TDg3$p1inp2vec <- is.element(GWS02_TDg3$player1, player2vector)
GWS02_TDg3$p2inp1vec <- is.element(GWS02_TDg3$player2, player1vector)

addPlayer1 <- GWS02_TDg3[ which(GWS02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS02_TDg3[ which(GWS02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS02_TDg2 <- rbind(GWS02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
GWS02_TDft <- ftable(GWS02_TDg2$player1, GWS02_TDg2$player2)
GWS02_TDft2 <- as.matrix(GWS02_TDft)
numRows <- nrow(GWS02_TDft2)
numCols <- ncol(GWS02_TDft2)
GWS02_TDft3 <- GWS02_TDft2[c(2:numRows) , c(2:numCols)]
GWS02_TDTable <- graph.adjacency(GWS02_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(GWS02_TDTable, vertex.label = V(GWS02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
GWS02_TD.clusterCoef <- transitivity(GWS02_TDTable, type="global") #cluster coefficient
GWS02_TD.degreeCent <- centralization.degree(GWS02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_TDftn <- as.network.matrix(GWS02_TDft)
GWS02_TD.netDensity <- network.density(GWS02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_TD.entropy <- entropy(GWS02_TDft) #entropy

GWS02_TD.netMx <- cbind(GWS02_TD.netMx, GWS02_TD.clusterCoef, GWS02_TD.degreeCent$centralization,
                        GWS02_TD.netDensity, GWS02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "GWS"
KIoutcome = "End of Qtr_DM"
GWS02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
GWS02_QTg2 <- data.frame(GWS02_QT)
GWS02_QTg2 <- GWS02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- GWS02_QTg2$player1
player2vector <- GWS02_QTg2$player2
GWS02_QTg3 <- GWS02_QTg2
GWS02_QTg3$p1inp2vec <- is.element(GWS02_QTg3$player1, player2vector)
GWS02_QTg3$p2inp1vec <- is.element(GWS02_QTg3$player2, player1vector)

addPlayer1 <- GWS02_QTg3[ which(GWS02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- GWS02_QTg3[ which(GWS02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

GWS02_QTg2 <- rbind(GWS02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
GWS02_QTft <- ftable(GWS02_QTg2$player1, GWS02_QTg2$player2)
GWS02_QTft2 <- as.matrix(GWS02_QTft)
numRows <- nrow(GWS02_QTft2)
numCols <- ncol(GWS02_QTft2)
GWS02_QTft3 <- GWS02_QTft2[c(2:numRows) , c(2:numCols)]
GWS02_QTTable <- graph.adjacency(GWS02_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(GWS02_QTTable, vertex.label = V(GWS02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(GWS02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
GWS02_QT.clusterCoef <- transitivity(GWS02_QTTable, type="global") #cluster coefficient
GWS02_QT.degreeCent <- centralization.degree(GWS02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
GWS02_QTftn <- as.network.matrix(GWS02_QTft)
GWS02_QT.netDensity <- network.density(GWS02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
GWS02_QT.entropy <- entropy(GWS02_QTft) #entropy

GWS02_QT.netMx <- cbind(GWS02_QT.netMx, GWS02_QT.clusterCoef, GWS02_QT.degreeCent$centralization,
                        GWS02_QT.netDensity, GWS02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(GWS02_QT.netMx) <- varnames

#############################################################################
#HAWTHORN

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "HAW"
KIoutcome = "Goal_F"
HAW02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
HAW02_Gg2 <- data.frame(HAW02_G)
HAW02_Gg2 <- HAW02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_Gg2$player1
player2vector <- HAW02_Gg2$player2
HAW02_Gg3 <- HAW02_Gg2
HAW02_Gg3$p1inp2vec <- is.element(HAW02_Gg3$player1, player2vector)
HAW02_Gg3$p2inp1vec <- is.element(HAW02_Gg3$player2, player1vector)

addPlayer1 <- HAW02_Gg3[ which(HAW02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_Gg3[ which(HAW02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_Gg2 <- rbind(HAW02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
HAW02_Gft <- ftable(HAW02_Gg2$player1, HAW02_Gg2$player2)
HAW02_Gft2 <- as.matrix(HAW02_Gft)
numRows <- nrow(HAW02_Gft2)
numCols <- ncol(HAW02_Gft2)
HAW02_Gft3 <- HAW02_Gft2[c(2:numRows) , c(2:numCols)]
HAW02_GTable <- graph.adjacency(HAW02_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(HAW02_GTable, vertex.label = V(HAW02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
HAW02_G.clusterCoef <- transitivity(HAW02_GTable, type="global") #cluster coefficient
HAW02_G.degreeCent <- centralization.degree(HAW02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_Gftn <- as.network.matrix(HAW02_Gft)
HAW02_G.netDensity <- network.density(HAW02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_G.entropy <- entropy(HAW02_Gft) #entropy

HAW02_G.netMx <- cbind(HAW02_G.netMx, HAW02_G.clusterCoef, HAW02_G.degreeCent$centralization,
                       HAW02_G.netDensity, HAW02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "HAW"
KIoutcome = "Behind_F"
HAW02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
HAW02_Bg2 <- data.frame(HAW02_B)
HAW02_Bg2 <- HAW02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_Bg2$player1
player2vector <- HAW02_Bg2$player2
HAW02_Bg3 <- HAW02_Bg2
HAW02_Bg3$p1inp2vec <- is.element(HAW02_Bg3$player1, player2vector)
HAW02_Bg3$p2inp1vec <- is.element(HAW02_Bg3$player2, player1vector)

addPlayer1 <- HAW02_Bg3[ which(HAW02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

HAW02_Bg2 <- rbind(HAW02_Bg2, addPlayer1)

#Round 2, Behind graph using weighted edges
HAW02_Bft <- ftable(HAW02_Bg2$player1, HAW02_Bg2$player2)
HAW02_Bft2 <- as.matrix(HAW02_Bft)
numRows <- nrow(HAW02_Bft2)
numCols <- ncol(HAW02_Bft2)
HAW02_Bft3 <- HAW02_Bft2[c(2:numRows) , c(1:numCols)]
HAW02_BTable <- graph.adjacency(HAW02_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(HAW02_BTable, vertex.label = V(HAW02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
HAW02_B.clusterCoef <- transitivity(HAW02_BTable, type="global") #cluster coefficient
HAW02_B.degreeCent <- centralization.degree(HAW02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_Bftn <- as.network.matrix(HAW02_Bft)
HAW02_B.netDensity <- network.density(HAW02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_B.entropy <- entropy(HAW02_Bft) #entropy

HAW02_B.netMx <- cbind(HAW02_B.netMx, HAW02_B.clusterCoef, HAW02_B.degreeCent$centralization,
                       HAW02_B.netDensity, HAW02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "HAW"
KIoutcome = "Stoppage_F"
HAW02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
HAW02_SFg2 <- data.frame(HAW02_SF)
HAW02_SFg2 <- HAW02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_SFg2$player1
player2vector <- HAW02_SFg2$player2
HAW02_SFg3 <- HAW02_SFg2
HAW02_SFg3$p1inp2vec <- is.element(HAW02_SFg3$player1, player2vector)
HAW02_SFg3$p2inp1vec <- is.element(HAW02_SFg3$player2, player1vector)

addPlayer1 <- HAW02_SFg3[ which(HAW02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_SFg3[ which(HAW02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_SFg2 <- rbind(HAW02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
HAW02_SFft <- ftable(HAW02_SFg2$player1, HAW02_SFg2$player2)
HAW02_SFft2 <- as.matrix(HAW02_SFft)
numRows <- nrow(HAW02_SFft2)
numCols <- ncol(HAW02_SFft2)
HAW02_SFft3 <- HAW02_SFft2[c(2:numRows) , c(2:numCols)]
HAW02_SFTable <- graph.adjacency(HAW02_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(HAW02_SFTable, vertex.label = V(HAW02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
HAW02_SF.clusterCoef <- transitivity(HAW02_SFTable, type="global") #cluster coefficient
HAW02_SF.degreeCent <- centralization.degree(HAW02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_SFftn <- as.network.matrix(HAW02_SFft)
HAW02_SF.netDensity <- network.density(HAW02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_SF.entropy <- entropy(HAW02_SFft) #entropy

HAW02_SF.netMx <- cbind(HAW02_SF.netMx, HAW02_SF.clusterCoef, HAW02_SF.degreeCent$centralization,
                        HAW02_SF.netDensity, HAW02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "HAW"
KIoutcome = "Turnover_F"
HAW02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
HAW02_TFg2 <- data.frame(HAW02_TF)
HAW02_TFg2 <- HAW02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_TFg2$player1
player2vector <- HAW02_TFg2$player2
HAW02_TFg3 <- HAW02_TFg2
HAW02_TFg3$p1inp2vec <- is.element(HAW02_TFg3$player1, player2vector)
HAW02_TFg3$p2inp1vec <- is.element(HAW02_TFg3$player2, player1vector)

addPlayer1 <- HAW02_TFg3[ which(HAW02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_TFg3[ which(HAW02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_TFg2 <- rbind(HAW02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
HAW02_TFft <- ftable(HAW02_TFg2$player1, HAW02_TFg2$player2)
HAW02_TFft2 <- as.matrix(HAW02_TFft)
numRows <- nrow(HAW02_TFft2)
numCols <- ncol(HAW02_TFft2)
HAW02_TFft3 <- HAW02_TFft2[c(2:numRows) , c(2:numCols)]
HAW02_TFTable <- graph.adjacency(HAW02_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(HAW02_TFTable, vertex.label = V(HAW02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
HAW02_TF.clusterCoef <- transitivity(HAW02_TFTable, type="global") #cluster coefficient
HAW02_TF.degreeCent <- centralization.degree(HAW02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_TFftn <- as.network.matrix(HAW02_TFft)
HAW02_TF.netDensity <- network.density(HAW02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_TF.entropy <- entropy(HAW02_TFft) #entropy

HAW02_TF.netMx <- cbind(HAW02_TF.netMx, HAW02_TF.clusterCoef, HAW02_TF.degreeCent$centralization,
                        HAW02_TF.netDensity, HAW02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "HAW"
KIoutcome = "Stoppage_AM"
HAW02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
HAW02_SAMg2 <- data.frame(HAW02_SAM)
HAW02_SAMg2 <- HAW02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_SAMg2$player1
player2vector <- HAW02_SAMg2$player2
HAW02_SAMg3 <- HAW02_SAMg2
HAW02_SAMg3$p1inp2vec <- is.element(HAW02_SAMg3$player1, player2vector)
HAW02_SAMg3$p2inp1vec <- is.element(HAW02_SAMg3$player2, player1vector)

addPlayer1 <- HAW02_SAMg3[ which(HAW02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_SAMg3[ which(HAW02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_SAMg2 <- rbind(HAW02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
HAW02_SAMft <- ftable(HAW02_SAMg2$player1, HAW02_SAMg2$player2)
HAW02_SAMft2 <- as.matrix(HAW02_SAMft)
numRows <- nrow(HAW02_SAMft2)
numCols <- ncol(HAW02_SAMft2)
HAW02_SAMft3 <- HAW02_SAMft2[c(2:numRows) , c(2:numCols)]
HAW02_SAMTable <- graph.adjacency(HAW02_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(HAW02_SAMTable, vertex.label = V(HAW02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
HAW02_SAM.clusterCoef <- transitivity(HAW02_SAMTable, type="global") #cluster coefficient
HAW02_SAM.degreeCent <- centralization.degree(HAW02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_SAMftn <- as.network.matrix(HAW02_SAMft)
HAW02_SAM.netDensity <- network.density(HAW02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_SAM.entropy <- entropy(HAW02_SAMft) #entropy

HAW02_SAM.netMx <- cbind(HAW02_SAM.netMx, HAW02_SAM.clusterCoef, HAW02_SAM.degreeCent$centralization,
                         HAW02_SAM.netDensity, HAW02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "HAW"
KIoutcome = "Turnover_AM"
HAW02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
HAW02_TAMg2 <- data.frame(HAW02_TAM)
HAW02_TAMg2 <- HAW02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_TAMg2$player1
player2vector <- HAW02_TAMg2$player2
HAW02_TAMg3 <- HAW02_TAMg2
HAW02_TAMg3$p1inp2vec <- is.element(HAW02_TAMg3$player1, player2vector)
HAW02_TAMg3$p2inp1vec <- is.element(HAW02_TAMg3$player2, player1vector)

addPlayer1 <- HAW02_TAMg3[ which(HAW02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_TAMg3[ which(HAW02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_TAMg2 <- rbind(HAW02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
HAW02_TAMft <- ftable(HAW02_TAMg2$player1, HAW02_TAMg2$player2)
HAW02_TAMft2 <- as.matrix(HAW02_TAMft)
numRows <- nrow(HAW02_TAMft2)
numCols <- ncol(HAW02_TAMft2)
HAW02_TAMft3 <- HAW02_TAMft2[c(2:numRows) , c(2:numCols)]
HAW02_TAMTable <- graph.adjacency(HAW02_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(HAW02_TAMTable, vertex.label = V(HAW02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
HAW02_TAM.clusterCoef <- transitivity(HAW02_TAMTable, type="global") #cluster coefficient
HAW02_TAM.degreeCent <- centralization.degree(HAW02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_TAMftn <- as.network.matrix(HAW02_TAMft)
HAW02_TAM.netDensity <- network.density(HAW02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_TAM.entropy <- entropy(HAW02_TAMft) #entropy

HAW02_TAM.netMx <- cbind(HAW02_TAM.netMx, HAW02_TAM.clusterCoef, HAW02_TAM.degreeCent$centralization,
                         HAW02_TAM.netDensity, HAW02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "HAW"
KIoutcome = "Stoppage_DM"
HAW02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
HAW02_SDMg2 <- data.frame(HAW02_SDM)
HAW02_SDMg2 <- HAW02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_SDMg2$player1
player2vector <- HAW02_SDMg2$player2
HAW02_SDMg3 <- HAW02_SDMg2
HAW02_SDMg3$p1inp2vec <- is.element(HAW02_SDMg3$player1, player2vector)
HAW02_SDMg3$p2inp1vec <- is.element(HAW02_SDMg3$player2, player1vector)

addPlayer1 <- HAW02_SDMg3[ which(HAW02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_SDMg3[ which(HAW02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_SDMg2 <- rbind(HAW02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
HAW02_SDMft <- ftable(HAW02_SDMg2$player1, HAW02_SDMg2$player2)
HAW02_SDMft2 <- as.matrix(HAW02_SDMft)
numRows <- nrow(HAW02_SDMft2)
numCols <- ncol(HAW02_SDMft2)
HAW02_SDMft3 <- HAW02_SDMft2[c(2:numRows) , c(2:numCols)]
HAW02_SDMTable <- graph.adjacency(HAW02_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(HAW02_SDMTable, vertex.label = V(HAW02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
HAW02_SDM.clusterCoef <- transitivity(HAW02_SDMTable, type="global") #cluster coefficient
HAW02_SDM.degreeCent <- centralization.degree(HAW02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_SDMftn <- as.network.matrix(HAW02_SDMft)
HAW02_SDM.netDensity <- network.density(HAW02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_SDM.entropy <- entropy(HAW02_SDMft) #entropy

HAW02_SDM.netMx <- cbind(HAW02_SDM.netMx, HAW02_SDM.clusterCoef, HAW02_SDM.degreeCent$centralization,
                         HAW02_SDM.netDensity, HAW02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************
#NA

round = 2
teamName = "HAW"
KIoutcome = "Turnover_DM"
HAW02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
HAW02_TDMg2 <- data.frame(HAW02_TDM)
HAW02_TDMg2 <- HAW02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_TDMg2$player1
player2vector <- HAW02_TDMg2$player2
HAW02_TDMg3 <- HAW02_TDMg2
HAW02_TDMg3$p1inp2vec <- is.element(HAW02_TDMg3$player1, player2vector)
HAW02_TDMg3$p2inp1vec <- is.element(HAW02_TDMg3$player2, player1vector)

addPlayer1 <- HAW02_TDMg3[ which(HAW02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_TDMg3[ which(HAW02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_TDMg2 <- rbind(HAW02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
HAW02_TDMft <- ftable(HAW02_TDMg2$player1, HAW02_TDMg2$player2)
HAW02_TDMft2 <- as.matrix(HAW02_TDMft)
numRows <- nrow(HAW02_TDMft2)
numCols <- ncol(HAW02_TDMft2)
HAW02_TDMft3 <- HAW02_TDMft2[c(2:numRows) , c(2:numCols)]
HAW02_TDMTable <- graph.adjacency(HAW02_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(HAW02_TDMTable, vertex.label = V(HAW02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
HAW02_TDM.clusterCoef <- transitivity(HAW02_TDMTable, type="global") #cluster coefficient
HAW02_TDM.degreeCent <- centralization.degree(HAW02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_TDMftn <- as.network.matrix(HAW02_TDMft)
HAW02_TDM.netDensity <- network.density(HAW02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_TDM.entropy <- entropy(HAW02_TDMft) #entropy

HAW02_TDM.netMx <- cbind(HAW02_TDM.netMx, HAW02_TDM.clusterCoef, HAW02_TDM.degreeCent$centralization,
                         HAW02_TDM.netDensity, HAW02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "HAW"
KIoutcome = "Stoppage_D"
HAW02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
HAW02_SDg2 <- data.frame(HAW02_SD)
HAW02_SDg2 <- HAW02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_SDg2$player1
player2vector <- HAW02_SDg2$player2
HAW02_SDg3 <- HAW02_SDg2
HAW02_SDg3$p1inp2vec <- is.element(HAW02_SDg3$player1, player2vector)
HAW02_SDg3$p2inp1vec <- is.element(HAW02_SDg3$player2, player1vector)

addPlayer1 <- HAW02_SDg3[ which(HAW02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_SDg3[ which(HAW02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_SDg2 <- rbind(HAW02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
HAW02_SDft <- ftable(HAW02_SDg2$player1, HAW02_SDg2$player2)
HAW02_SDft2 <- as.matrix(HAW02_SDft)
numRows <- nrow(HAW02_SDft2)
numCols <- ncol(HAW02_SDft2)
HAW02_SDft3 <- HAW02_SDft2[c(2:numRows) , c(2:numCols)]
HAW02_SDTable <- graph.adjacency(HAW02_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(HAW02_SDTable, vertex.label = V(HAW02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
HAW02_SD.clusterCoef <- transitivity(HAW02_SDTable, type="global") #cluster coefficient
HAW02_SD.degreeCent <- centralization.degree(HAW02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_SDftn <- as.network.matrix(HAW02_SDft)
HAW02_SD.netDensity <- network.density(HAW02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_SD.entropy <- entropy(HAW02_SDft) #entropy

HAW02_SD.netMx <- cbind(HAW02_SD.netMx, HAW02_SD.clusterCoef, HAW02_SD.degreeCent$centralization,
                        HAW02_SD.netDensity, HAW02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "HAW"
KIoutcome = "Turnover_D"
HAW02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
HAW02_TDg2 <- data.frame(HAW02_TD)
HAW02_TDg2 <- HAW02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_TDg2$player1
player2vector <- HAW02_TDg2$player2
HAW02_TDg3 <- HAW02_TDg2
HAW02_TDg3$p1inp2vec <- is.element(HAW02_TDg3$player1, player2vector)
HAW02_TDg3$p2inp1vec <- is.element(HAW02_TDg3$player2, player1vector)

addPlayer1 <- HAW02_TDg3[ which(HAW02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_TDg3[ which(HAW02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_TDg2 <- rbind(HAW02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
HAW02_TDft <- ftable(HAW02_TDg2$player1, HAW02_TDg2$player2)
HAW02_TDft2 <- as.matrix(HAW02_TDft)
numRows <- nrow(HAW02_TDft2)
numCols <- ncol(HAW02_TDft2)
HAW02_TDft3 <- HAW02_TDft2[c(2:numRows) , c(2:numCols)]
HAW02_TDTable <- graph.adjacency(HAW02_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(HAW02_TDTable, vertex.label = V(HAW02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
HAW02_TD.clusterCoef <- transitivity(HAW02_TDTable, type="global") #cluster coefficient
HAW02_TD.degreeCent <- centralization.degree(HAW02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_TDftn <- as.network.matrix(HAW02_TDft)
HAW02_TD.netDensity <- network.density(HAW02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_TD.entropy <- entropy(HAW02_TDft) #entropy

HAW02_TD.netMx <- cbind(HAW02_TD.netMx, HAW02_TD.clusterCoef, HAW02_TD.degreeCent$centralization,
                        HAW02_TD.netDensity, HAW02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "HAW"
KIoutcome = "End of Qtr_DM"
HAW02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
HAW02_QTg2 <- data.frame(HAW02_QT)
HAW02_QTg2 <- HAW02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- HAW02_QTg2$player1
player2vector <- HAW02_QTg2$player2
HAW02_QTg3 <- HAW02_QTg2
HAW02_QTg3$p1inp2vec <- is.element(HAW02_QTg3$player1, player2vector)
HAW02_QTg3$p2inp1vec <- is.element(HAW02_QTg3$player2, player1vector)

addPlayer1 <- HAW02_QTg3[ which(HAW02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- HAW02_QTg3[ which(HAW02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

HAW02_QTg2 <- rbind(HAW02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
HAW02_QTft <- ftable(HAW02_QTg2$player1, HAW02_QTg2$player2)
HAW02_QTft2 <- as.matrix(HAW02_QTft)
numRows <- nrow(HAW02_QTft2)
numCols <- ncol(HAW02_QTft2)
HAW02_QTft3 <- HAW02_QTft2[c(2:numRows) , c(2:numCols)]
HAW02_QTTable <- graph.adjacency(HAW02_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(HAW02_QTTable, vertex.label = V(HAW02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(HAW02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
HAW02_QT.clusterCoef <- transitivity(HAW02_QTTable, type="global") #cluster coefficient
HAW02_QT.degreeCent <- centralization.degree(HAW02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
HAW02_QTftn <- as.network.matrix(HAW02_QTft)
HAW02_QT.netDensity <- network.density(HAW02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
HAW02_QT.entropy <- entropy(HAW02_QTft) #entropy

HAW02_QT.netMx <- cbind(HAW02_QT.netMx, HAW02_QT.clusterCoef, HAW02_QT.degreeCent$centralization,
                        HAW02_QT.netDensity, HAW02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(HAW02_QT.netMx) <- varnames

#############################################################################
#MELBOURNE

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "MELB"
KIoutcome = "Goal_F"
MELB02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
MELB02_Gg2 <- data.frame(MELB02_G)
MELB02_Gg2 <- MELB02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_Gg2$player1
player2vector <- MELB02_Gg2$player2
MELB02_Gg3 <- MELB02_Gg2
MELB02_Gg3$p1inp2vec <- is.element(MELB02_Gg3$player1, player2vector)
MELB02_Gg3$p2inp1vec <- is.element(MELB02_Gg3$player2, player1vector)

addPlayer1 <- MELB02_Gg3[ which(MELB02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

MELB02_Gg2 <- rbind(MELB02_Gg2, addPlayer1)

#Round 2, Goal graph using weighted edges
MELB02_Gft <- ftable(MELB02_Gg2$player1, MELB02_Gg2$player2)
MELB02_Gft2 <- as.matrix(MELB02_Gft)
numRows <- nrow(MELB02_Gft2)
numCols <- ncol(MELB02_Gft2)
MELB02_Gft3 <- MELB02_Gft2[c(2:numRows) , c(1:numCols)]
MELB02_GTable <- graph.adjacency(MELB02_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(MELB02_GTable, vertex.label = V(MELB02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
MELB02_G.clusterCoef <- transitivity(MELB02_GTable, type="global") #cluster coefficient
MELB02_G.degreeCent <- centralization.degree(MELB02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_Gftn <- as.network.matrix(MELB02_Gft)
MELB02_G.netDensity <- network.density(MELB02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_G.entropy <- entropy(MELB02_Gft) #entropy

MELB02_G.netMx <- cbind(MELB02_G.netMx, MELB02_G.clusterCoef, MELB02_G.degreeCent$centralization,
                        MELB02_G.netDensity, MELB02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "MELB"
KIoutcome = "Behind_F"
MELB02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
MELB02_Bg2 <- data.frame(MELB02_B)
MELB02_Bg2 <- MELB02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_Bg2$player1
player2vector <- MELB02_Bg2$player2
MELB02_Bg3 <- MELB02_Bg2
MELB02_Bg3$p1inp2vec <- is.element(MELB02_Bg3$player1, player2vector)
MELB02_Bg3$p2inp1vec <- is.element(MELB02_Bg3$player2, player1vector)

addPlayer1 <- MELB02_Bg3[ which(MELB02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_Bg3[ which(MELB02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_Bg2 <- rbind(MELB02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
MELB02_Bft <- ftable(MELB02_Bg2$player1, MELB02_Bg2$player2)
MELB02_Bft2 <- as.matrix(MELB02_Bft)
numRows <- nrow(MELB02_Bft2)
numCols <- ncol(MELB02_Bft2)
MELB02_Bft3 <- MELB02_Bft2[c(2:numRows) , c(2:numCols)]
MELB02_BTable <- graph.adjacency(MELB02_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(MELB02_BTable, vertex.label = V(MELB02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
MELB02_B.clusterCoef <- transitivity(MELB02_BTable, type="global") #cluster coefficient
MELB02_B.degreeCent <- centralization.degree(MELB02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_Bftn <- as.network.matrix(MELB02_Bft)
MELB02_B.netDensity <- network.density(MELB02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_B.entropy <- entropy(MELB02_Bft) #entropy

MELB02_B.netMx <- cbind(MELB02_B.netMx, MELB02_B.clusterCoef, MELB02_B.degreeCent$centralization,
                        MELB02_B.netDensity, MELB02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "MELB"
KIoutcome = "Stoppage_F"
MELB02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
MELB02_SFg2 <- data.frame(MELB02_SF)
MELB02_SFg2 <- MELB02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_SFg2$player1
player2vector <- MELB02_SFg2$player2
MELB02_SFg3 <- MELB02_SFg2
MELB02_SFg3$p1inp2vec <- is.element(MELB02_SFg3$player1, player2vector)
MELB02_SFg3$p2inp1vec <- is.element(MELB02_SFg3$player2, player1vector)

addPlayer1 <- MELB02_SFg3[ which(MELB02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_SFg3[ which(MELB02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_SFg2 <- rbind(MELB02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
MELB02_SFft <- ftable(MELB02_SFg2$player1, MELB02_SFg2$player2)
MELB02_SFft2 <- as.matrix(MELB02_SFft)
numRows <- nrow(MELB02_SFft2)
numCols <- ncol(MELB02_SFft2)
MELB02_SFft3 <- MELB02_SFft2[c(2:numRows) , c(2:numCols)]
MELB02_SFTable <- graph.adjacency(MELB02_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(MELB02_SFTable, vertex.label = V(MELB02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
MELB02_SF.clusterCoef <- transitivity(MELB02_SFTable, type="global") #cluster coefficient
MELB02_SF.degreeCent <- centralization.degree(MELB02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_SFftn <- as.network.matrix(MELB02_SFft)
MELB02_SF.netDensity <- network.density(MELB02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_SF.entropy <- entropy(MELB02_SFft) #entropy

MELB02_SF.netMx <- cbind(MELB02_SF.netMx, MELB02_SF.clusterCoef, MELB02_SF.degreeCent$centralization,
                         MELB02_SF.netDensity, MELB02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************
#NA

round = 2
teamName = "MELB"
KIoutcome = "Turnover_F"
MELB02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
MELB02_TFg2 <- data.frame(MELB02_TF)
MELB02_TFg2 <- MELB02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_TFg2$player1
player2vector <- MELB02_TFg2$player2
MELB02_TFg3 <- MELB02_TFg2
MELB02_TFg3$p1inp2vec <- is.element(MELB02_TFg3$player1, player2vector)
MELB02_TFg3$p2inp1vec <- is.element(MELB02_TFg3$player2, player1vector)

addPlayer1 <- MELB02_TFg3[ which(MELB02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_TFg3[ which(MELB02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_TFg2 <- rbind(MELB02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
MELB02_TFft <- ftable(MELB02_TFg2$player1, MELB02_TFg2$player2)
MELB02_TFft2 <- as.matrix(MELB02_TFft)
numRows <- nrow(MELB02_TFft2)
numCols <- ncol(MELB02_TFft2)
MELB02_TFft3 <- MELB02_TFft2[c(2:numRows) , c(2:numCols)]
MELB02_TFTable <- graph.adjacency(MELB02_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(MELB02_TFTable, vertex.label = V(MELB02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
MELB02_TF.clusterCoef <- transitivity(MELB02_TFTable, type="global") #cluster coefficient
MELB02_TF.degreeCent <- centralization.degree(MELB02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_TFftn <- as.network.matrix(MELB02_TFft)
MELB02_TF.netDensity <- network.density(MELB02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_TF.entropy <- entropy(MELB02_TFft) #entropy

MELB02_TF.netMx <- cbind(MELB02_TF.netMx, MELB02_TF.clusterCoef, MELB02_TF.degreeCent$centralization,
                         MELB02_TF.netDensity, MELB02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "MELB"
KIoutcome = "Stoppage_AM"
MELB02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
MELB02_SAMg2 <- data.frame(MELB02_SAM)
MELB02_SAMg2 <- MELB02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_SAMg2$player1
player2vector <- MELB02_SAMg2$player2
MELB02_SAMg3 <- MELB02_SAMg2
MELB02_SAMg3$p1inp2vec <- is.element(MELB02_SAMg3$player1, player2vector)
MELB02_SAMg3$p2inp1vec <- is.element(MELB02_SAMg3$player2, player1vector)

addPlayer1 <- MELB02_SAMg3[ which(MELB02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_SAMg3[ which(MELB02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_SAMg2 <- rbind(MELB02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
MELB02_SAMft <- ftable(MELB02_SAMg2$player1, MELB02_SAMg2$player2)
MELB02_SAMft2 <- as.matrix(MELB02_SAMft)
numRows <- nrow(MELB02_SAMft2)
numCols <- ncol(MELB02_SAMft2)
MELB02_SAMft3 <- MELB02_SAMft2[c(2:numRows) , c(2:numCols)]
MELB02_SAMTable <- graph.adjacency(MELB02_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(MELB02_SAMTable, vertex.label = V(MELB02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
MELB02_SAM.clusterCoef <- transitivity(MELB02_SAMTable, type="global") #cluster coefficient
MELB02_SAM.degreeCent <- centralization.degree(MELB02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_SAMftn <- as.network.matrix(MELB02_SAMft)
MELB02_SAM.netDensity <- network.density(MELB02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_SAM.entropy <- entropy(MELB02_SAMft) #entropy

MELB02_SAM.netMx <- cbind(MELB02_SAM.netMx, MELB02_SAM.clusterCoef, MELB02_SAM.degreeCent$centralization,
                          MELB02_SAM.netDensity, MELB02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "MELB"
KIoutcome = "Turnover_AM"
MELB02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
MELB02_TAMg2 <- data.frame(MELB02_TAM)
MELB02_TAMg2 <- MELB02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_TAMg2$player1
player2vector <- MELB02_TAMg2$player2
MELB02_TAMg3 <- MELB02_TAMg2
MELB02_TAMg3$p1inp2vec <- is.element(MELB02_TAMg3$player1, player2vector)
MELB02_TAMg3$p2inp1vec <- is.element(MELB02_TAMg3$player2, player1vector)

addPlayer1 <- MELB02_TAMg3[ which(MELB02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_TAMg3[ which(MELB02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_TAMg2 <- rbind(MELB02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
MELB02_TAMft <- ftable(MELB02_TAMg2$player1, MELB02_TAMg2$player2)
MELB02_TAMft2 <- as.matrix(MELB02_TAMft)
numRows <- nrow(MELB02_TAMft2)
numCols <- ncol(MELB02_TAMft2)
MELB02_TAMft3 <- MELB02_TAMft2[c(2:numRows) , c(2:numCols)]
MELB02_TAMTable <- graph.adjacency(MELB02_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(MELB02_TAMTable, vertex.label = V(MELB02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
MELB02_TAM.clusterCoef <- transitivity(MELB02_TAMTable, type="global") #cluster coefficient
MELB02_TAM.degreeCent <- centralization.degree(MELB02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_TAMftn <- as.network.matrix(MELB02_TAMft)
MELB02_TAM.netDensity <- network.density(MELB02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_TAM.entropy <- entropy(MELB02_TAMft) #entropy

MELB02_TAM.netMx <- cbind(MELB02_TAM.netMx, MELB02_TAM.clusterCoef, MELB02_TAM.degreeCent$centralization,
                          MELB02_TAM.netDensity, MELB02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "MELB"
KIoutcome = "Stoppage_DM"
MELB02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
MELB02_SDMg2 <- data.frame(MELB02_SDM)
MELB02_SDMg2 <- MELB02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_SDMg2$player1
player2vector <- MELB02_SDMg2$player2
MELB02_SDMg3 <- MELB02_SDMg2
MELB02_SDMg3$p1inp2vec <- is.element(MELB02_SDMg3$player1, player2vector)
MELB02_SDMg3$p2inp1vec <- is.element(MELB02_SDMg3$player2, player1vector)

addPlayer1 <- MELB02_SDMg3[ which(MELB02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_SDMg3[ which(MELB02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_SDMg2 <- rbind(MELB02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
MELB02_SDMft <- ftable(MELB02_SDMg2$player1, MELB02_SDMg2$player2)
MELB02_SDMft2 <- as.matrix(MELB02_SDMft)
numRows <- nrow(MELB02_SDMft2)
numCols <- ncol(MELB02_SDMft2)
MELB02_SDMft3 <- MELB02_SDMft2[c(2:numRows) , c(2:numCols)]
MELB02_SDMTable <- graph.adjacency(MELB02_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(MELB02_SDMTable, vertex.label = V(MELB02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
MELB02_SDM.clusterCoef <- transitivity(MELB02_SDMTable, type="global") #cluster coefficient
MELB02_SDM.degreeCent <- centralization.degree(MELB02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_SDMftn <- as.network.matrix(MELB02_SDMft)
MELB02_SDM.netDensity <- network.density(MELB02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_SDM.entropy <- entropy(MELB02_SDMft) #entropy

MELB02_SDM.netMx <- cbind(MELB02_SDM.netMx, MELB02_SDM.clusterCoef, MELB02_SDM.degreeCent$centralization,
                          MELB02_SDM.netDensity, MELB02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "MELB"
KIoutcome = "Turnover_DM"
MELB02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
MELB02_TDMg2 <- data.frame(MELB02_TDM)
MELB02_TDMg2 <- MELB02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_TDMg2$player1
player2vector <- MELB02_TDMg2$player2
MELB02_TDMg3 <- MELB02_TDMg2
MELB02_TDMg3$p1inp2vec <- is.element(MELB02_TDMg3$player1, player2vector)
MELB02_TDMg3$p2inp1vec <- is.element(MELB02_TDMg3$player2, player1vector)

addPlayer1 <- MELB02_TDMg3[ which(MELB02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_TDMg3[ which(MELB02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_TDMg2 <- rbind(MELB02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
MELB02_TDMft <- ftable(MELB02_TDMg2$player1, MELB02_TDMg2$player2)
MELB02_TDMft2 <- as.matrix(MELB02_TDMft)
numRows <- nrow(MELB02_TDMft2)
numCols <- ncol(MELB02_TDMft2)
MELB02_TDMft3 <- MELB02_TDMft2[c(2:numRows) , c(2:numCols)]
MELB02_TDMTable <- graph.adjacency(MELB02_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(MELB02_TDMTable, vertex.label = V(MELB02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
MELB02_TDM.clusterCoef <- transitivity(MELB02_TDMTable, type="global") #cluster coefficient
MELB02_TDM.degreeCent <- centralization.degree(MELB02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_TDMftn <- as.network.matrix(MELB02_TDMft)
MELB02_TDM.netDensity <- network.density(MELB02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_TDM.entropy <- entropy(MELB02_TDMft) #entropy

MELB02_TDM.netMx <- cbind(MELB02_TDM.netMx, MELB02_TDM.clusterCoef, MELB02_TDM.degreeCent$centralization,
                          MELB02_TDM.netDensity, MELB02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "MELB"
KIoutcome = "Stoppage_D"
MELB02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
MELB02_SDg2 <- data.frame(MELB02_SD)
MELB02_SDg2 <- MELB02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_SDg2$player1
player2vector <- MELB02_SDg2$player2
MELB02_SDg3 <- MELB02_SDg2
MELB02_SDg3$p1inp2vec <- is.element(MELB02_SDg3$player1, player2vector)
MELB02_SDg3$p2inp1vec <- is.element(MELB02_SDg3$player2, player1vector)

addPlayer1 <- MELB02_SDg3[ which(MELB02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_SDg3[ which(MELB02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_SDg2 <- rbind(MELB02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
MELB02_SDft <- ftable(MELB02_SDg2$player1, MELB02_SDg2$player2)
MELB02_SDft2 <- as.matrix(MELB02_SDft)
numRows <- nrow(MELB02_SDft2)
numCols <- ncol(MELB02_SDft2)
MELB02_SDft3 <- MELB02_SDft2[c(2:numRows) , c(2:numCols)]
MELB02_SDTable <- graph.adjacency(MELB02_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(MELB02_SDTable, vertex.label = V(MELB02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
MELB02_SD.clusterCoef <- transitivity(MELB02_SDTable, type="global") #cluster coefficient
MELB02_SD.degreeCent <- centralization.degree(MELB02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_SDftn <- as.network.matrix(MELB02_SDft)
MELB02_SD.netDensity <- network.density(MELB02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_SD.entropy <- entropy(MELB02_SDft) #entropy

MELB02_SD.netMx <- cbind(MELB02_SD.netMx, MELB02_SD.clusterCoef, MELB02_SD.degreeCent$centralization,
                         MELB02_SD.netDensity, MELB02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "MELB"
KIoutcome = "Turnover_D"
MELB02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
MELB02_TDg2 <- data.frame(MELB02_TD)
MELB02_TDg2 <- MELB02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_TDg2$player1
player2vector <- MELB02_TDg2$player2
MELB02_TDg3 <- MELB02_TDg2
MELB02_TDg3$p1inp2vec <- is.element(MELB02_TDg3$player1, player2vector)
MELB02_TDg3$p2inp1vec <- is.element(MELB02_TDg3$player2, player1vector)

addPlayer1 <- MELB02_TDg3[ which(MELB02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_TDg3[ which(MELB02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_TDg2 <- rbind(MELB02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
MELB02_TDft <- ftable(MELB02_TDg2$player1, MELB02_TDg2$player2)
MELB02_TDft2 <- as.matrix(MELB02_TDft)
numRows <- nrow(MELB02_TDft2)
numCols <- ncol(MELB02_TDft2)
MELB02_TDft3 <- MELB02_TDft2[c(2:numRows) , c(2:numCols)]
MELB02_TDTable <- graph.adjacency(MELB02_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(MELB02_TDTable, vertex.label = V(MELB02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
MELB02_TD.clusterCoef <- transitivity(MELB02_TDTable, type="global") #cluster coefficient
MELB02_TD.degreeCent <- centralization.degree(MELB02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_TDftn <- as.network.matrix(MELB02_TDft)
MELB02_TD.netDensity <- network.density(MELB02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_TD.entropy <- entropy(MELB02_TDft) #entropy

MELB02_TD.netMx <- cbind(MELB02_TD.netMx, MELB02_TD.clusterCoef, MELB02_TD.degreeCent$centralization,
                         MELB02_TD.netDensity, MELB02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "MELB"
KIoutcome = "End of Qtr_DM"
MELB02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
MELB02_QTg2 <- data.frame(MELB02_QT)
MELB02_QTg2 <- MELB02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- MELB02_QTg2$player1
player2vector <- MELB02_QTg2$player2
MELB02_QTg3 <- MELB02_QTg2
MELB02_QTg3$p1inp2vec <- is.element(MELB02_QTg3$player1, player2vector)
MELB02_QTg3$p2inp1vec <- is.element(MELB02_QTg3$player2, player1vector)

addPlayer1 <- MELB02_QTg3[ which(MELB02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- MELB02_QTg3[ which(MELB02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

MELB02_QTg2 <- rbind(MELB02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
MELB02_QTft <- ftable(MELB02_QTg2$player1, MELB02_QTg2$player2)
MELB02_QTft2 <- as.matrix(MELB02_QTft)
numRows <- nrow(MELB02_QTft2)
numCols <- ncol(MELB02_QTft2)
MELB02_QTft3 <- MELB02_QTft2[c(2:numRows) , c(2:numCols)]
MELB02_QTTable <- graph.adjacency(MELB02_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(MELB02_QTTable, vertex.label = V(MELB02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(MELB02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
MELB02_QT.clusterCoef <- transitivity(MELB02_QTTable, type="global") #cluster coefficient
MELB02_QT.degreeCent <- centralization.degree(MELB02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
MELB02_QTftn <- as.network.matrix(MELB02_QTft)
MELB02_QT.netDensity <- network.density(MELB02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
MELB02_QT.entropy <- entropy(MELB02_QTft) #entropy

MELB02_QT.netMx <- cbind(MELB02_QT.netMx, MELB02_QT.clusterCoef, MELB02_QT.degreeCent$centralization,
                         MELB02_QT.netDensity, MELB02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(MELB02_QT.netMx) <- varnames

#############################################################################
#NORTH MELBOURNE

##
#ROUND 2
##

#Round 2, Goal***************************************************************

round = 2
teamName = "NMFC"
KIoutcome = "Goal_F"
NMFC02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
NMFC02_Gg2 <- data.frame(NMFC02_G)
NMFC02_Gg2 <- NMFC02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_Gg2$player1
player2vector <- NMFC02_Gg2$player2
NMFC02_Gg3 <- NMFC02_Gg2
NMFC02_Gg3$p1inp2vec <- is.element(NMFC02_Gg3$player1, player2vector)
NMFC02_Gg3$p2inp1vec <- is.element(NMFC02_Gg3$player2, player1vector)

addPlayer1 <- NMFC02_Gg3[ which(NMFC02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_Gg3[ which(NMFC02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_Gg2 <- rbind(NMFC02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
NMFC02_Gft <- ftable(NMFC02_Gg2$player1, NMFC02_Gg2$player2)
NMFC02_Gft2 <- as.matrix(NMFC02_Gft)
numRows <- nrow(NMFC02_Gft2)
numCols <- ncol(NMFC02_Gft2)
NMFC02_Gft3 <- NMFC02_Gft2[c(2:numRows) , c(2:numCols)]
NMFC02_GTable <- graph.adjacency(NMFC02_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(NMFC02_GTable, vertex.label = V(NMFC02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
NMFC02_G.clusterCoef <- transitivity(NMFC02_GTable, type="global") #cluster coefficient
NMFC02_G.degreeCent <- centralization.degree(NMFC02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_Gftn <- as.network.matrix(NMFC02_Gft)
NMFC02_G.netDensity <- network.density(NMFC02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_G.entropy <- entropy(NMFC02_Gft) #entropy

NMFC02_G.netMx <- cbind(NMFC02_G.netMx, NMFC02_G.clusterCoef, NMFC02_G.degreeCent$centralization,
                        NMFC02_G.netDensity, NMFC02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "NMFC"
KIoutcome = "Behind_F"
NMFC02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
NMFC02_Bg2 <- data.frame(NMFC02_B)
NMFC02_Bg2 <- NMFC02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_Bg2$player1
player2vector <- NMFC02_Bg2$player2
NMFC02_Bg3 <- NMFC02_Bg2
NMFC02_Bg3$p1inp2vec <- is.element(NMFC02_Bg3$player1, player2vector)
NMFC02_Bg3$p2inp1vec <- is.element(NMFC02_Bg3$player2, player1vector)

addPlayer1 <- NMFC02_Bg3[ which(NMFC02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_Bg3[ which(NMFC02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_Bg2 <- rbind(NMFC02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
NMFC02_Bft <- ftable(NMFC02_Bg2$player1, NMFC02_Bg2$player2)
NMFC02_Bft2 <- as.matrix(NMFC02_Bft)
numRows <- nrow(NMFC02_Bft2)
numCols <- ncol(NMFC02_Bft2)
NMFC02_Bft3 <- NMFC02_Bft2[c(2:numRows) , c(2:numCols)]
NMFC02_BTable <- graph.adjacency(NMFC02_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(NMFC02_BTable, vertex.label = V(NMFC02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
NMFC02_B.clusterCoef <- transitivity(NMFC02_BTable, type="global") #cluster coefficient
NMFC02_B.degreeCent <- centralization.degree(NMFC02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_Bftn <- as.network.matrix(NMFC02_Bft)
NMFC02_B.netDensity <- network.density(NMFC02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_B.entropy <- entropy(NMFC02_Bft) #entropy

NMFC02_B.netMx <- cbind(NMFC02_B.netMx, NMFC02_B.clusterCoef, NMFC02_B.degreeCent$centralization,
                        NMFC02_B.netDensity, NMFC02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************

round = 2
teamName = "NMFC"
KIoutcome = "Stoppage_F"
NMFC02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
NMFC02_SFg2 <- data.frame(NMFC02_SF)
NMFC02_SFg2 <- NMFC02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_SFg2$player1
player2vector <- NMFC02_SFg2$player2
NMFC02_SFg3 <- NMFC02_SFg2
NMFC02_SFg3$p1inp2vec <- is.element(NMFC02_SFg3$player1, player2vector)
NMFC02_SFg3$p2inp1vec <- is.element(NMFC02_SFg3$player2, player1vector)

addPlayer1 <- NMFC02_SFg3[ which(NMFC02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_SFg3[ which(NMFC02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_SFg2 <- rbind(NMFC02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
NMFC02_SFft <- ftable(NMFC02_SFg2$player1, NMFC02_SFg2$player2)
NMFC02_SFft2 <- as.matrix(NMFC02_SFft)
numRows <- nrow(NMFC02_SFft2)
numCols <- ncol(NMFC02_SFft2)
NMFC02_SFft3 <- NMFC02_SFft2[c(2:numRows) , c(2:numCols)]
NMFC02_SFTable <- graph.adjacency(NMFC02_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(NMFC02_SFTable, vertex.label = V(NMFC02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
NMFC02_SF.clusterCoef <- transitivity(NMFC02_SFTable, type="global") #cluster coefficient
NMFC02_SF.degreeCent <- centralization.degree(NMFC02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_SFftn <- as.network.matrix(NMFC02_SFft)
NMFC02_SF.netDensity <- network.density(NMFC02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_SF.entropy <- entropy(NMFC02_SFft) #entropy

NMFC02_SF.netMx <- cbind(NMFC02_SF.netMx, NMFC02_SF.clusterCoef, NMFC02_SF.degreeCent$centralization,
                         NMFC02_SF.netDensity, NMFC02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "NMFC"
KIoutcome = "Turnover_F"
NMFC02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
NMFC02_TFg2 <- data.frame(NMFC02_TF)
NMFC02_TFg2 <- NMFC02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_TFg2$player1
player2vector <- NMFC02_TFg2$player2
NMFC02_TFg3 <- NMFC02_TFg2
NMFC02_TFg3$p1inp2vec <- is.element(NMFC02_TFg3$player1, player2vector)
NMFC02_TFg3$p2inp1vec <- is.element(NMFC02_TFg3$player2, player1vector)

addPlayer1 <- NMFC02_TFg3[ which(NMFC02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

NMFC02_TFg2 <- rbind(NMFC02_TFg2, addPlayer1)

#Round 2, FWD Turnover graph using weighted edges
NMFC02_TFft <- ftable(NMFC02_TFg2$player1, NMFC02_TFg2$player2)
NMFC02_TFft2 <- as.matrix(NMFC02_TFft)
numRows <- nrow(NMFC02_TFft2)
numCols <- ncol(NMFC02_TFft2)
NMFC02_TFft3 <- NMFC02_TFft2[c(2:numRows) , c(1:numCols)]
NMFC02_TFTable <- graph.adjacency(NMFC02_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(NMFC02_TFTable, vertex.label = V(NMFC02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
NMFC02_TF.clusterCoef <- transitivity(NMFC02_TFTable, type="global") #cluster coefficient
NMFC02_TF.degreeCent <- centralization.degree(NMFC02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_TFftn <- as.network.matrix(NMFC02_TFft)
NMFC02_TF.netDensity <- network.density(NMFC02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_TF.entropy <- entropy(NMFC02_TFft) #entropy

NMFC02_TF.netMx <- cbind(NMFC02_TF.netMx, NMFC02_TF.clusterCoef, NMFC02_TF.degreeCent$centralization,
                         NMFC02_TF.netDensity, NMFC02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "NMFC"
KIoutcome = "Stoppage_AM"
NMFC02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
NMFC02_SAMg2 <- data.frame(NMFC02_SAM)
NMFC02_SAMg2 <- NMFC02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_SAMg2$player1
player2vector <- NMFC02_SAMg2$player2
NMFC02_SAMg3 <- NMFC02_SAMg2
NMFC02_SAMg3$p1inp2vec <- is.element(NMFC02_SAMg3$player1, player2vector)
NMFC02_SAMg3$p2inp1vec <- is.element(NMFC02_SAMg3$player2, player1vector)

addPlayer1 <- NMFC02_SAMg3[ which(NMFC02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_SAMg3[ which(NMFC02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_SAMg2 <- rbind(NMFC02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
NMFC02_SAMft <- ftable(NMFC02_SAMg2$player1, NMFC02_SAMg2$player2)
NMFC02_SAMft2 <- as.matrix(NMFC02_SAMft)
numRows <- nrow(NMFC02_SAMft2)
numCols <- ncol(NMFC02_SAMft2)
NMFC02_SAMft3 <- NMFC02_SAMft2[c(2:numRows) , c(2:numCols)]
NMFC02_SAMTable <- graph.adjacency(NMFC02_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(NMFC02_SAMTable, vertex.label = V(NMFC02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
NMFC02_SAM.clusterCoef <- transitivity(NMFC02_SAMTable, type="global") #cluster coefficient
NMFC02_SAM.degreeCent <- centralization.degree(NMFC02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_SAMftn <- as.network.matrix(NMFC02_SAMft)
NMFC02_SAM.netDensity <- network.density(NMFC02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_SAM.entropy <- entropy(NMFC02_SAMft) #entropy

NMFC02_SAM.netMx <- cbind(NMFC02_SAM.netMx, NMFC02_SAM.clusterCoef, NMFC02_SAM.degreeCent$centralization,
                          NMFC02_SAM.netDensity, NMFC02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************
#NA

round = 2
teamName = "NMFC"
KIoutcome = "Turnover_AM"
NMFC02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
NMFC02_TAMg2 <- data.frame(NMFC02_TAM)
NMFC02_TAMg2 <- NMFC02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_TAMg2$player1
player2vector <- NMFC02_TAMg2$player2
NMFC02_TAMg3 <- NMFC02_TAMg2
NMFC02_TAMg3$p1inp2vec <- is.element(NMFC02_TAMg3$player1, player2vector)
NMFC02_TAMg3$p2inp1vec <- is.element(NMFC02_TAMg3$player2, player1vector)

addPlayer1 <- NMFC02_TAMg3[ which(NMFC02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_TAMg3[ which(NMFC02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_TAMg2 <- rbind(NMFC02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
NMFC02_TAMft <- ftable(NMFC02_TAMg2$player1, NMFC02_TAMg2$player2)
NMFC02_TAMft2 <- as.matrix(NMFC02_TAMft)
numRows <- nrow(NMFC02_TAMft2)
numCols <- ncol(NMFC02_TAMft2)
NMFC02_TAMft3 <- NMFC02_TAMft2[c(2:numRows) , c(2:numCols)]
NMFC02_TAMTable <- graph.adjacency(NMFC02_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(NMFC02_TAMTable, vertex.label = V(NMFC02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
NMFC02_TAM.clusterCoef <- transitivity(NMFC02_TAMTable, type="global") #cluster coefficient
NMFC02_TAM.degreeCent <- centralization.degree(NMFC02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_TAMftn <- as.network.matrix(NMFC02_TAMft)
NMFC02_TAM.netDensity <- network.density(NMFC02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_TAM.entropy <- entropy(NMFC02_TAMft) #entropy

NMFC02_TAM.netMx <- cbind(NMFC02_TAM.netMx, NMFC02_TAM.clusterCoef, NMFC02_TAM.degreeCent$centralization,
                          NMFC02_TAM.netDensity, NMFC02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "NMFC"
KIoutcome = "Stoppage_DM"
NMFC02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
NMFC02_SDMg2 <- data.frame(NMFC02_SDM)
NMFC02_SDMg2 <- NMFC02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_SDMg2$player1
player2vector <- NMFC02_SDMg2$player2
NMFC02_SDMg3 <- NMFC02_SDMg2
NMFC02_SDMg3$p1inp2vec <- is.element(NMFC02_SDMg3$player1, player2vector)
NMFC02_SDMg3$p2inp1vec <- is.element(NMFC02_SDMg3$player2, player1vector)

addPlayer1 <- NMFC02_SDMg3[ which(NMFC02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_SDMg3[ which(NMFC02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(empty, empty, zero)
addPlayers <- rbind(empty, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_SDMg2 <- rbind(NMFC02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
NMFC02_SDMft <- ftable(NMFC02_SDMg2$player1, NMFC02_SDMg2$player2)
NMFC02_SDMft2 <- as.matrix(NMFC02_SDMft)
numRows <- nrow(NMFC02_SDMft2)
numCols <- ncol(NMFC02_SDMft2)
NMFC02_SDMft3 <- NMFC02_SDMft2[c(2:numRows) , c(2:numCols)]
NMFC02_SDMTable <- graph.adjacency(NMFC02_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(NMFC02_SDMTable, vertex.label = V(NMFC02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
NMFC02_SDM.clusterCoef <- transitivity(NMFC02_SDMTable, type="global") #cluster coefficient
NMFC02_SDM.degreeCent <- centralization.degree(NMFC02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_SDMftn <- as.network.matrix(NMFC02_SDMft)
NMFC02_SDM.netDensity <- network.density(NMFC02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_SDM.entropy <- entropy(NMFC02_SDMft) #entropy

NMFC02_SDM.netMx <- cbind(NMFC02_SDM.netMx, NMFC02_SDM.clusterCoef, NMFC02_SDM.degreeCent$centralization,
                          NMFC02_SDM.netDensity, NMFC02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "NMFC"
KIoutcome = "Turnover_DM"
NMFC02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
NMFC02_TDMg2 <- data.frame(NMFC02_TDM)
NMFC02_TDMg2 <- NMFC02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_TDMg2$player1
player2vector <- NMFC02_TDMg2$player2
NMFC02_TDMg3 <- NMFC02_TDMg2
NMFC02_TDMg3$p1inp2vec <- is.element(NMFC02_TDMg3$player1, player2vector)
NMFC02_TDMg3$p2inp1vec <- is.element(NMFC02_TDMg3$player2, player1vector)

addPlayer1 <- NMFC02_TDMg3[ which(NMFC02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_TDMg3[ which(NMFC02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_TDMg2 <- rbind(NMFC02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
NMFC02_TDMft <- ftable(NMFC02_TDMg2$player1, NMFC02_TDMg2$player2)
NMFC02_TDMft2 <- as.matrix(NMFC02_TDMft)
numRows <- nrow(NMFC02_TDMft2)
numCols <- ncol(NMFC02_TDMft2)
NMFC02_TDMft3 <- NMFC02_TDMft2[c(2:numRows) , c(2:numCols)]
NMFC02_TDMTable <- graph.adjacency(NMFC02_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(NMFC02_TDMTable, vertex.label = V(NMFC02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
NMFC02_TDM.clusterCoef <- transitivity(NMFC02_TDMTable, type="global") #cluster coefficient
NMFC02_TDM.degreeCent <- centralization.degree(NMFC02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_TDMftn <- as.network.matrix(NMFC02_TDMft)
NMFC02_TDM.netDensity <- network.density(NMFC02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_TDM.entropy <- entropy(NMFC02_TDMft) #entropy

NMFC02_TDM.netMx <- cbind(NMFC02_TDM.netMx, NMFC02_TDM.clusterCoef, NMFC02_TDM.degreeCent$centralization,
                          NMFC02_TDM.netDensity, NMFC02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "NMFC"
KIoutcome = "Stoppage_D"
NMFC02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
NMFC02_SDg2 <- data.frame(NMFC02_SD)
NMFC02_SDg2 <- NMFC02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_SDg2$player1
player2vector <- NMFC02_SDg2$player2
NMFC02_SDg3 <- NMFC02_SDg2
NMFC02_SDg3$p1inp2vec <- is.element(NMFC02_SDg3$player1, player2vector)
NMFC02_SDg3$p2inp1vec <- is.element(NMFC02_SDg3$player2, player1vector)

addPlayer1 <- NMFC02_SDg3[ which(NMFC02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_SDg3[ which(NMFC02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_SDg2 <- rbind(NMFC02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
NMFC02_SDft <- ftable(NMFC02_SDg2$player1, NMFC02_SDg2$player2)
NMFC02_SDft2 <- as.matrix(NMFC02_SDft)
numRows <- nrow(NMFC02_SDft2)
numCols <- ncol(NMFC02_SDft2)
NMFC02_SDft3 <- NMFC02_SDft2[c(2:numRows) , c(2:numCols)]
NMFC02_SDTable <- graph.adjacency(NMFC02_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(NMFC02_SDTable, vertex.label = V(NMFC02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
NMFC02_SD.clusterCoef <- transitivity(NMFC02_SDTable, type="global") #cluster coefficient
NMFC02_SD.degreeCent <- centralization.degree(NMFC02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_SDftn <- as.network.matrix(NMFC02_SDft)
NMFC02_SD.netDensity <- network.density(NMFC02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_SD.entropy <- entropy(NMFC02_SDft) #entropy

NMFC02_SD.netMx <- cbind(NMFC02_SD.netMx, NMFC02_SD.clusterCoef, NMFC02_SD.degreeCent$centralization,
                         NMFC02_SD.netDensity, NMFC02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************

round = 2
teamName = "NMFC"
KIoutcome = "Turnover_D"
NMFC02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
NMFC02_TDg2 <- data.frame(NMFC02_TD)
NMFC02_TDg2 <- NMFC02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_TDg2$player1
player2vector <- NMFC02_TDg2$player2
NMFC02_TDg3 <- NMFC02_TDg2
NMFC02_TDg3$p1inp2vec <- is.element(NMFC02_TDg3$player1, player2vector)
NMFC02_TDg3$p2inp1vec <- is.element(NMFC02_TDg3$player2, player1vector)

addPlayer1 <- NMFC02_TDg3[ which(NMFC02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_TDg3[ which(NMFC02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_TDg2 <- rbind(NMFC02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
NMFC02_TDft <- ftable(NMFC02_TDg2$player1, NMFC02_TDg2$player2)
NMFC02_TDft2 <- as.matrix(NMFC02_TDft)
numRows <- nrow(NMFC02_TDft2)
numCols <- ncol(NMFC02_TDft2)
NMFC02_TDft3 <- NMFC02_TDft2[c(2:numRows) , c(2:numCols)]
NMFC02_TDTable <- graph.adjacency(NMFC02_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(NMFC02_TDTable, vertex.label = V(NMFC02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
NMFC02_TD.clusterCoef <- transitivity(NMFC02_TDTable, type="global") #cluster coefficient
NMFC02_TD.degreeCent <- centralization.degree(NMFC02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_TDftn <- as.network.matrix(NMFC02_TDft)
NMFC02_TD.netDensity <- network.density(NMFC02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_TD.entropy <- entropy(NMFC02_TDft) #entropy

NMFC02_TD.netMx <- cbind(NMFC02_TD.netMx, NMFC02_TD.clusterCoef, NMFC02_TD.degreeCent$centralization,
                         NMFC02_TD.netDensity, NMFC02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "NMFC"
KIoutcome = "End of Qtr_DM"
NMFC02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
NMFC02_QTg2 <- data.frame(NMFC02_QT)
NMFC02_QTg2 <- NMFC02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- NMFC02_QTg2$player1
player2vector <- NMFC02_QTg2$player2
NMFC02_QTg3 <- NMFC02_QTg2
NMFC02_QTg3$p1inp2vec <- is.element(NMFC02_QTg3$player1, player2vector)
NMFC02_QTg3$p2inp1vec <- is.element(NMFC02_QTg3$player2, player1vector)

addPlayer1 <- NMFC02_QTg3[ which(NMFC02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- NMFC02_QTg3[ which(NMFC02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

NMFC02_QTg2 <- rbind(NMFC02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
NMFC02_QTft <- ftable(NMFC02_QTg2$player1, NMFC02_QTg2$player2)
NMFC02_QTft2 <- as.matrix(NMFC02_QTft)
numRows <- nrow(NMFC02_QTft2)
numCols <- ncol(NMFC02_QTft2)
NMFC02_QTft3 <- NMFC02_QTft2[c(2:numRows) , c(2:numCols)]
NMFC02_QTTable <- graph.adjacency(NMFC02_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(NMFC02_QTTable, vertex.label = V(NMFC02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(NMFC02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
NMFC02_QT.clusterCoef <- transitivity(NMFC02_QTTable, type="global") #cluster coefficient
NMFC02_QT.degreeCent <- centralization.degree(NMFC02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
NMFC02_QTftn <- as.network.matrix(NMFC02_QTft)
NMFC02_QT.netDensity <- network.density(NMFC02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
NMFC02_QT.entropy <- entropy(NMFC02_QTft) #entropy

NMFC02_QT.netMx <- cbind(NMFC02_QT.netMx, NMFC02_QT.clusterCoef, NMFC02_QT.degreeCent$centralization,
                         NMFC02_QT.netDensity, NMFC02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(NMFC02_QT.netMx) <- varnames

#############################################################################
#PORT ADELAIDE

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "PORT"
KIoutcome = "Goal_F"
PORT02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
PORT02_Gg2 <- data.frame(PORT02_G)
PORT02_Gg2 <- PORT02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_Gg2$player1
player2vector <- PORT02_Gg2$player2
PORT02_Gg3 <- PORT02_Gg2
PORT02_Gg3$p1inp2vec <- is.element(PORT02_Gg3$player1, player2vector)
PORT02_Gg3$p2inp1vec <- is.element(PORT02_Gg3$player2, player1vector)

addPlayer1 <- PORT02_Gg3[ which(PORT02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer1) <- varnames

PORT02_Gg2 <- rbind(PORT02_Gg2, addPlayer1)

#Round 2, Goal graph using weighted edges
PORT02_Gft <- ftable(PORT02_Gg2$player1, PORT02_Gg2$player2)
PORT02_Gft2 <- as.matrix(PORT02_Gft)
numRows <- nrow(PORT02_Gft2)
numCols <- ncol(PORT02_Gft2)
PORT02_Gft3 <- PORT02_Gft2[c(2:numRows) , c(1:numCols)]
PORT02_GTable <- graph.adjacency(PORT02_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(PORT02_GTable, vertex.label = V(PORT02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
PORT02_G.clusterCoef <- transitivity(PORT02_GTable, type="global") #cluster coefficient
PORT02_G.degreeCent <- centralization.degree(PORT02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_Gftn <- as.network.matrix(PORT02_Gft)
PORT02_G.netDensity <- network.density(PORT02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_G.entropy <- entropy(PORT02_Gft) #entropy

PORT02_G.netMx <- cbind(PORT02_G.netMx, PORT02_G.clusterCoef, PORT02_G.degreeCent$centralization,
                        PORT02_G.netDensity, PORT02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "PORT"
KIoutcome = "Behind_F"
PORT02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
PORT02_Bg2 <- data.frame(PORT02_B)
PORT02_Bg2 <- PORT02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_Bg2$player1
player2vector <- PORT02_Bg2$player2
PORT02_Bg3 <- PORT02_Bg2
PORT02_Bg3$p1inp2vec <- is.element(PORT02_Bg3$player1, player2vector)
PORT02_Bg3$p2inp1vec <- is.element(PORT02_Bg3$player2, player1vector)

addPlayer1 <- PORT02_Bg3[ which(PORT02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT02_Bg3[ which(PORT02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT02_Bg2 <- rbind(PORT02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
PORT02_Bft <- ftable(PORT02_Bg2$player1, PORT02_Bg2$player2)
PORT02_Bft2 <- as.matrix(PORT02_Bft)
numRows <- nrow(PORT02_Bft2)
numCols <- ncol(PORT02_Bft2)
PORT02_Bft3 <- PORT02_Bft2[c(2:numRows) , c(2:numCols)]
PORT02_BTable <- graph.adjacency(PORT02_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(PORT02_BTable, vertex.label = V(PORT02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
PORT02_B.clusterCoef <- transitivity(PORT02_BTable, type="global") #cluster coefficient
PORT02_B.degreeCent <- centralization.degree(PORT02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_Bftn <- as.network.matrix(PORT02_Bft)
PORT02_B.netDensity <- network.density(PORT02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_B.entropy <- entropy(PORT02_Bft) #entropy

PORT02_B.netMx <- cbind(PORT02_B.netMx, PORT02_B.clusterCoef, PORT02_B.degreeCent$centralization,
                        PORT02_B.netDensity, PORT02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "PORT"
KIoutcome = "Stoppage_F"
PORT02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
PORT02_SFg2 <- data.frame(PORT02_SF)
PORT02_SFg2 <- PORT02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_SFg2$player1
player2vector <- PORT02_SFg2$player2
PORT02_SFg3 <- PORT02_SFg2
PORT02_SFg3$p1inp2vec <- is.element(PORT02_SFg3$player1, player2vector)
PORT02_SFg3$p2inp1vec <- is.element(PORT02_SFg3$player2, player1vector)

addPlayer1 <- PORT02_SFg3[ which(PORT02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT02_SFg3[ which(PORT02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT02_SFg2 <- rbind(PORT02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
PORT02_SFft <- ftable(PORT02_SFg2$player1, PORT02_SFg2$player2)
PORT02_SFft2 <- as.matrix(PORT02_SFft)
numRows <- nrow(PORT02_SFft2)
numCols <- ncol(PORT02_SFft2)
PORT02_SFft3 <- PORT02_SFft2[c(2:numRows) , c(2:numCols)]
PORT02_SFTable <- graph.adjacency(PORT02_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(PORT02_SFTable, vertex.label = V(PORT02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
PORT02_SF.clusterCoef <- transitivity(PORT02_SFTable, type="global") #cluster coefficient
PORT02_SF.degreeCent <- centralization.degree(PORT02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_SFftn <- as.network.matrix(PORT02_SFft)
PORT02_SF.netDensity <- network.density(PORT02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_SF.entropy <- entropy(PORT02_SFft) #entropy

PORT02_SF.netMx <- cbind(PORT02_SF.netMx, PORT02_SF.clusterCoef, PORT02_SF.degreeCent$centralization,
                         PORT02_SF.netDensity, PORT02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "PORT"
KIoutcome = "Turnover_F"
PORT02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
PORT02_TFg2 <- data.frame(PORT02_TF)
PORT02_TFg2 <- PORT02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_TFg2$player1
player2vector <- PORT02_TFg2$player2
PORT02_TFg3 <- PORT02_TFg2
PORT02_TFg3$p1inp2vec <- is.element(PORT02_TFg3$player1, player2vector)
PORT02_TFg3$p2inp1vec <- is.element(PORT02_TFg3$player2, player1vector)

addPlayer1 <- PORT02_TFg3[ which(PORT02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT02_TFg3[ which(PORT02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT02_TFg2 <- rbind(PORT02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
PORT02_TFft <- ftable(PORT02_TFg2$player1, PORT02_TFg2$player2)
PORT02_TFft2 <- as.matrix(PORT02_TFft)
numRows <- nrow(PORT02_TFft2)
numCols <- ncol(PORT02_TFft2)
PORT02_TFft3 <- PORT02_TFft2[c(2:numRows) , c(2:numCols)]
PORT02_TFTable <- graph.adjacency(PORT02_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(PORT02_TFTable, vertex.label = V(PORT02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
PORT02_TF.clusterCoef <- transitivity(PORT02_TFTable, type="global") #cluster coefficient
PORT02_TF.degreeCent <- centralization.degree(PORT02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_TFftn <- as.network.matrix(PORT02_TFft)
PORT02_TF.netDensity <- network.density(PORT02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_TF.entropy <- entropy(PORT02_TFft) #entropy

PORT02_TF.netMx <- cbind(PORT02_TF.netMx, PORT02_TF.clusterCoef, PORT02_TF.degreeCent$centralization,
                         PORT02_TF.netDensity, PORT02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************

round = 2
teamName = "PORT"
KIoutcome = "Stoppage_AM"
PORT02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
PORT02_SAMg2 <- data.frame(PORT02_SAM)
PORT02_SAMg2 <- PORT02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_SAMg2$player1
player2vector <- PORT02_SAMg2$player2
PORT02_SAMg3 <- PORT02_SAMg2
PORT02_SAMg3$p1inp2vec <- is.element(PORT02_SAMg3$player1, player2vector)
PORT02_SAMg3$p2inp1vec <- is.element(PORT02_SAMg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- PORT02_SAMg3[ which(PORT02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

PORT02_SAMg2 <- rbind(PORT02_SAMg2, addPlayer2)


#Round 2, AM Stoppage graph using weighted edges
PORT02_SAMft <- ftable(PORT02_SAMg2$player1, PORT02_SAMg2$player2)
PORT02_SAMft2 <- as.matrix(PORT02_SAMft)
numRows <- nrow(PORT02_SAMft2)
numCols <- ncol(PORT02_SAMft2)
PORT02_SAMft3 <- PORT02_SAMft2[c(1:numRows) , c(2:numCols)]
PORT02_SAMTable <- graph.adjacency(PORT02_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(PORT02_SAMTable, vertex.label = V(PORT02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
PORT02_SAM.clusterCoef <- transitivity(PORT02_SAMTable, type="global") #cluster coefficient
PORT02_SAM.degreeCent <- centralization.degree(PORT02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_SAMftn <- as.network.matrix(PORT02_SAMft)
PORT02_SAM.netDensity <- network.density(PORT02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_SAM.entropy <- entropy(PORT02_SAMft) #entropy

PORT02_SAM.netMx <- cbind(PORT02_SAM.netMx, PORT02_SAM.clusterCoef, PORT02_SAM.degreeCent$centralization,
                          PORT02_SAM.netDensity, PORT02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "PORT"
KIoutcome = "Turnover_AM"
PORT02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
PORT02_TAMg2 <- data.frame(PORT02_TAM)
PORT02_TAMg2 <- PORT02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_TAMg2$player1
player2vector <- PORT02_TAMg2$player2
PORT02_TAMg3 <- PORT02_TAMg2
PORT02_TAMg3$p1inp2vec <- is.element(PORT02_TAMg3$player1, player2vector)
PORT02_TAMg3$p2inp1vec <- is.element(PORT02_TAMg3$player2, player1vector)

addPlayer1 <- PORT02_TAMg3[ which(PORT02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT02_TAMg3[ which(PORT02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT02_TAMg2 <- rbind(PORT02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
PORT02_TAMft <- ftable(PORT02_TAMg2$player1, PORT02_TAMg2$player2)
PORT02_TAMft2 <- as.matrix(PORT02_TAMft)
numRows <- nrow(PORT02_TAMft2)
numCols <- ncol(PORT02_TAMft2)
PORT02_TAMft3 <- PORT02_TAMft2[c(2:numRows) , c(2:numCols)]
PORT02_TAMTable <- graph.adjacency(PORT02_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(PORT02_TAMTable, vertex.label = V(PORT02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
PORT02_TAM.clusterCoef <- transitivity(PORT02_TAMTable, type="global") #cluster coefficient
PORT02_TAM.degreeCent <- centralization.degree(PORT02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_TAMftn <- as.network.matrix(PORT02_TAMft)
PORT02_TAM.netDensity <- network.density(PORT02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_TAM.entropy <- entropy(PORT02_TAMft) #entropy

PORT02_TAM.netMx <- cbind(PORT02_TAM.netMx, PORT02_TAM.clusterCoef, PORT02_TAM.degreeCent$centralization,
                          PORT02_TAM.netDensity, PORT02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************
#NA

round = 2
teamName = "PORT"
KIoutcome = "Stoppage_DM"
PORT02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
PORT02_SDMg2 <- data.frame(PORT02_SDM)
PORT02_SDMg2 <- PORT02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_SDMg2$player1
player2vector <- PORT02_SDMg2$player2
PORT02_SDMg3 <- PORT02_SDMg2
PORT02_SDMg3$p1inp2vec <- is.element(PORT02_SDMg3$player1, player2vector)
PORT02_SDMg3$p2inp1vec <- is.element(PORT02_SDMg3$player2, player1vector)

addPlayer1 <- PORT02_SDMg3[ which(PORT02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT02_SDMg3[ which(PORT02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT02_SDMg2 <- rbind(PORT02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
PORT02_SDMft <- ftable(PORT02_SDMg2$player1, PORT02_SDMg2$player2)
PORT02_SDMft2 <- as.matrix(PORT02_SDMft)
numRows <- nrow(PORT02_SDMft2)
numCols <- ncol(PORT02_SDMft2)
PORT02_SDMft3 <- PORT02_SDMft2[c(2:numRows) , c(2:numCols)]
PORT02_SDMTable <- graph.adjacency(PORT02_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(PORT02_SDMTable, vertex.label = V(PORT02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
PORT02_SDM.clusterCoef <- transitivity(PORT02_SDMTable, type="global") #cluster coefficient
PORT02_SDM.degreeCent <- centralization.degree(PORT02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_SDMftn <- as.network.matrix(PORT02_SDMft)
PORT02_SDM.netDensity <- network.density(PORT02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_SDM.entropy <- entropy(PORT02_SDMft) #entropy

PORT02_SDM.netMx <- cbind(PORT02_SDM.netMx, PORT02_SDM.clusterCoef, PORT02_SDM.degreeCent$centralization,
                          PORT02_SDM.netDensity, PORT02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "PORT"
KIoutcome = "Turnover_DM"
PORT02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
PORT02_TDMg2 <- data.frame(PORT02_TDM)
PORT02_TDMg2 <- PORT02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_TDMg2$player1
player2vector <- PORT02_TDMg2$player2
PORT02_TDMg3 <- PORT02_TDMg2
PORT02_TDMg3$p1inp2vec <- is.element(PORT02_TDMg3$player1, player2vector)
PORT02_TDMg3$p2inp1vec <- is.element(PORT02_TDMg3$player2, player1vector)

addPlayer1 <- PORT02_TDMg3[ which(PORT02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT02_TDMg3[ which(PORT02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT02_TDMg2 <- rbind(PORT02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
PORT02_TDMft <- ftable(PORT02_TDMg2$player1, PORT02_TDMg2$player2)
PORT02_TDMft2 <- as.matrix(PORT02_TDMft)
numRows <- nrow(PORT02_TDMft2)
numCols <- ncol(PORT02_TDMft2)
PORT02_TDMft3 <- PORT02_TDMft2[c(2:numRows) , c(2:numCols)]
PORT02_TDMTable <- graph.adjacency(PORT02_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(PORT02_TDMTable, vertex.label = V(PORT02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
PORT02_TDM.clusterCoef <- transitivity(PORT02_TDMTable, type="global") #cluster coefficient
PORT02_TDM.degreeCent <- centralization.degree(PORT02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_TDMftn <- as.network.matrix(PORT02_TDMft)
PORT02_TDM.netDensity <- network.density(PORT02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_TDM.entropy <- entropy(PORT02_TDMft) #entropy

PORT02_TDM.netMx <- cbind(PORT02_TDM.netMx, PORT02_TDM.clusterCoef, PORT02_TDM.degreeCent$centralization,
                          PORT02_TDM.netDensity, PORT02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************

round = 2
teamName = "PORT"
KIoutcome = "Stoppage_D"
PORT02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
PORT02_SDg2 <- data.frame(PORT02_SD)
PORT02_SDg2 <- PORT02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_SDg2$player1
player2vector <- PORT02_SDg2$player2
PORT02_SDg3 <- PORT02_SDg2
PORT02_SDg3$p1inp2vec <- is.element(PORT02_SDg3$player1, player2vector)
PORT02_SDg3$p2inp1vec <- is.element(PORT02_SDg3$player2, player1vector)

addPlayer1 <- PORT02_SDg3[ which(PORT02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT02_SDg3[ which(PORT02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT02_SDg2 <- rbind(PORT02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
PORT02_SDft <- ftable(PORT02_SDg2$player1, PORT02_SDg2$player2)
PORT02_SDft2 <- as.matrix(PORT02_SDft)
numRows <- nrow(PORT02_SDft2)
numCols <- ncol(PORT02_SDft2)
PORT02_SDft3 <- PORT02_SDft2[c(2:numRows) , c(2:numCols)]
PORT02_SDTable <- graph.adjacency(PORT02_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(PORT02_SDTable, vertex.label = V(PORT02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
PORT02_SD.clusterCoef <- transitivity(PORT02_SDTable, type="global") #cluster coefficient
PORT02_SD.degreeCent <- centralization.degree(PORT02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_SDftn <- as.network.matrix(PORT02_SDft)
PORT02_SD.netDensity <- network.density(PORT02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_SD.entropy <- entropy(PORT02_SDft) #entropy

PORT02_SD.netMx <- cbind(PORT02_SD.netMx, PORT02_SD.clusterCoef, PORT02_SD.degreeCent$centralization,
                         PORT02_SD.netDensity, PORT02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "PORT"
KIoutcome = "Turnover_D"
PORT02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
PORT02_TDg2 <- data.frame(PORT02_TD)
PORT02_TDg2 <- PORT02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_TDg2$player1
player2vector <- PORT02_TDg2$player2
PORT02_TDg3 <- PORT02_TDg2
PORT02_TDg3$p1inp2vec <- is.element(PORT02_TDg3$player1, player2vector)
PORT02_TDg3$p2inp1vec <- is.element(PORT02_TDg3$player2, player1vector)

addPlayer1 <- PORT02_TDg3[ which(PORT02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT02_TDg3[ which(PORT02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT02_TDg2 <- rbind(PORT02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
PORT02_TDft <- ftable(PORT02_TDg2$player1, PORT02_TDg2$player2)
PORT02_TDft2 <- as.matrix(PORT02_TDft)
numRows <- nrow(PORT02_TDft2)
numCols <- ncol(PORT02_TDft2)
PORT02_TDft3 <- PORT02_TDft2[c(2:numRows) , c(2:numCols)]
PORT02_TDTable <- graph.adjacency(PORT02_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(PORT02_TDTable, vertex.label = V(PORT02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
PORT02_TD.clusterCoef <- transitivity(PORT02_TDTable, type="global") #cluster coefficient
PORT02_TD.degreeCent <- centralization.degree(PORT02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_TDftn <- as.network.matrix(PORT02_TDft)
PORT02_TD.netDensity <- network.density(PORT02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_TD.entropy <- entropy(PORT02_TDft) #entropy

PORT02_TD.netMx <- cbind(PORT02_TD.netMx, PORT02_TD.clusterCoef, PORT02_TD.degreeCent$centralization,
                         PORT02_TD.netDensity, PORT02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "PORT"
KIoutcome = "End of Qtr_DM"
PORT02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
PORT02_QTg2 <- data.frame(PORT02_QT)
PORT02_QTg2 <- PORT02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- PORT02_QTg2$player1
player2vector <- PORT02_QTg2$player2
PORT02_QTg3 <- PORT02_QTg2
PORT02_QTg3$p1inp2vec <- is.element(PORT02_QTg3$player1, player2vector)
PORT02_QTg3$p2inp1vec <- is.element(PORT02_QTg3$player2, player1vector)

addPlayer1 <- PORT02_QTg3[ which(PORT02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- PORT02_QTg3[ which(PORT02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

PORT02_QTg2 <- rbind(PORT02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
PORT02_QTft <- ftable(PORT02_QTg2$player1, PORT02_QTg2$player2)
PORT02_QTft2 <- as.matrix(PORT02_QTft)
numRows <- nrow(PORT02_QTft2)
numCols <- ncol(PORT02_QTft2)
PORT02_QTft3 <- PORT02_QTft2[c(2:numRows) , c(2:numCols)]
PORT02_QTTable <- graph.adjacency(PORT02_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(PORT02_QTTable, vertex.label = V(PORT02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(PORT02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
PORT02_QT.clusterCoef <- transitivity(PORT02_QTTable, type="global") #cluster coefficient
PORT02_QT.degreeCent <- centralization.degree(PORT02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
PORT02_QTftn <- as.network.matrix(PORT02_QTft)
PORT02_QT.netDensity <- network.density(PORT02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
PORT02_QT.entropy <- entropy(PORT02_QTft) #entropy

PORT02_QT.netMx <- cbind(PORT02_QT.netMx, PORT02_QT.clusterCoef, PORT02_QT.degreeCent$centralization,
                         PORT02_QT.netDensity, PORT02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(PORT02_QT.netMx) <- varnames

#############################################################################
#RICHMOND

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "RICH"
KIoutcome = "Goal_F"
RICH02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
RICH02_Gg2 <- data.frame(RICH02_G)
RICH02_Gg2 <- RICH02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_Gg2$player1
player2vector <- RICH02_Gg2$player2
RICH02_Gg3 <- RICH02_Gg2
RICH02_Gg3$p1inp2vec <- is.element(RICH02_Gg3$player1, player2vector)
RICH02_Gg3$p2inp1vec <- is.element(RICH02_Gg3$player2, player1vector)

addPlayer1 <- RICH02_Gg3[ which(RICH02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_Gg3[ which(RICH02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_Gg2 <- rbind(RICH02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
RICH02_Gft <- ftable(RICH02_Gg2$player1, RICH02_Gg2$player2)
RICH02_Gft2 <- as.matrix(RICH02_Gft)
numRows <- nrow(RICH02_Gft2)
numCols <- ncol(RICH02_Gft2)
RICH02_Gft3 <- RICH02_Gft2[c(2:numRows) , c(2:numCols)]
RICH02_GTable <- graph.adjacency(RICH02_Gft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(RICH02_GTable, vertex.label = V(RICH02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
RICH02_G.clusterCoef <- transitivity(RICH02_GTable, type="global") #cluster coefficient
RICH02_G.degreeCent <- centralization.degree(RICH02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_Gftn <- as.network.matrix(RICH02_Gft)
RICH02_G.netDensity <- network.density(RICH02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_G.entropy <- entropy(RICH02_Gft) #entropy

RICH02_G.netMx <- cbind(RICH02_G.netMx, RICH02_G.clusterCoef, RICH02_G.degreeCent$centralization,
                        RICH02_G.netDensity, RICH02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_G.netMx) <- varnames

#Round 2, Behind***************************************************************

round = 2
teamName = "RICH"
KIoutcome = "Behind_F"
RICH02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
RICH02_Bg2 <- data.frame(RICH02_B)
RICH02_Bg2 <- RICH02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_Bg2$player1
player2vector <- RICH02_Bg2$player2
RICH02_Bg3 <- RICH02_Bg2
RICH02_Bg3$p1inp2vec <- is.element(RICH02_Bg3$player1, player2vector)
RICH02_Bg3$p2inp1vec <- is.element(RICH02_Bg3$player2, player1vector)

addPlayer1 <- RICH02_Bg3[ which(RICH02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_Bg3[ which(RICH02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_Bg2 <- rbind(RICH02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
RICH02_Bft <- ftable(RICH02_Bg2$player1, RICH02_Bg2$player2)
RICH02_Bft2 <- as.matrix(RICH02_Bft)
numRows <- nrow(RICH02_Bft2)
numCols <- ncol(RICH02_Bft2)
RICH02_Bft3 <- RICH02_Bft2[c(2:numRows) , c(2:numCols)]
RICH02_BTable <- graph.adjacency(RICH02_Bft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(RICH02_BTable, vertex.label = V(RICH02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
RICH02_B.clusterCoef <- transitivity(RICH02_BTable, type="global") #cluster coefficient
RICH02_B.degreeCent <- centralization.degree(RICH02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_Bftn <- as.network.matrix(RICH02_Bft)
RICH02_B.netDensity <- network.density(RICH02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_B.entropy <- entropy(RICH02_Bft) #entropy

RICH02_B.netMx <- cbind(RICH02_B.netMx, RICH02_B.clusterCoef, RICH02_B.degreeCent$centralization,
                        RICH02_B.netDensity, RICH02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "RICH"
KIoutcome = "Stoppage_F"
RICH02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
RICH02_SFg2 <- data.frame(RICH02_SF)
RICH02_SFg2 <- RICH02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_SFg2$player1
player2vector <- RICH02_SFg2$player2
RICH02_SFg3 <- RICH02_SFg2
RICH02_SFg3$p1inp2vec <- is.element(RICH02_SFg3$player1, player2vector)
RICH02_SFg3$p2inp1vec <- is.element(RICH02_SFg3$player2, player1vector)

addPlayer1 <- RICH02_SFg3[ which(RICH02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_SFg3[ which(RICH02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_SFg2 <- rbind(RICH02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
RICH02_SFft <- ftable(RICH02_SFg2$player1, RICH02_SFg2$player2)
RICH02_SFft2 <- as.matrix(RICH02_SFft)
numRows <- nrow(RICH02_SFft2)
numCols <- ncol(RICH02_SFft2)
RICH02_SFft3 <- RICH02_SFft2[c(2:numRows) , c(2:numCols)]
RICH02_SFTable <- graph.adjacency(RICH02_SFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(RICH02_SFTable, vertex.label = V(RICH02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
RICH02_SF.clusterCoef <- transitivity(RICH02_SFTable, type="global") #cluster coefficient
RICH02_SF.degreeCent <- centralization.degree(RICH02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_SFftn <- as.network.matrix(RICH02_SFft)
RICH02_SF.netDensity <- network.density(RICH02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_SF.entropy <- entropy(RICH02_SFft) #entropy

RICH02_SF.netMx <- cbind(RICH02_SF.netMx, RICH02_SF.clusterCoef, RICH02_SF.degreeCent$centralization,
                         RICH02_SF.netDensity, RICH02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "RICH"
KIoutcome = "Turnover_F"
RICH02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
RICH02_TFg2 <- data.frame(RICH02_TF)
RICH02_TFg2 <- RICH02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_TFg2$player1
player2vector <- RICH02_TFg2$player2
RICH02_TFg3 <- RICH02_TFg2
RICH02_TFg3$p1inp2vec <- is.element(RICH02_TFg3$player1, player2vector)
RICH02_TFg3$p2inp1vec <- is.element(RICH02_TFg3$player2, player1vector)

addPlayer1 <- RICH02_TFg3[ which(RICH02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_TFg3[ which(RICH02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_TFg2 <- rbind(RICH02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
RICH02_TFft <- ftable(RICH02_TFg2$player1, RICH02_TFg2$player2)
RICH02_TFft2 <- as.matrix(RICH02_TFft)
numRows <- nrow(RICH02_TFft2)
numCols <- ncol(RICH02_TFft2)
RICH02_TFft3 <- RICH02_TFft2[c(2:numRows) , c(2:numCols)]
RICH02_TFTable <- graph.adjacency(RICH02_TFft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(RICH02_TFTable, vertex.label = V(RICH02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
RICH02_TF.clusterCoef <- transitivity(RICH02_TFTable, type="global") #cluster coefficient
RICH02_TF.degreeCent <- centralization.degree(RICH02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_TFftn <- as.network.matrix(RICH02_TFft)
RICH02_TF.netDensity <- network.density(RICH02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_TF.entropy <- entropy(RICH02_TFft) #entropy

RICH02_TF.netMx <- cbind(RICH02_TF.netMx, RICH02_TF.clusterCoef, RICH02_TF.degreeCent$centralization,
                         RICH02_TF.netDensity, RICH02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "RICH"
KIoutcome = "Stoppage_AM"
RICH02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
RICH02_SAMg2 <- data.frame(RICH02_SAM)
RICH02_SAMg2 <- RICH02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_SAMg2$player1
player2vector <- RICH02_SAMg2$player2
RICH02_SAMg3 <- RICH02_SAMg2
RICH02_SAMg3$p1inp2vec <- is.element(RICH02_SAMg3$player1, player2vector)
RICH02_SAMg3$p2inp1vec <- is.element(RICH02_SAMg3$player2, player1vector)

addPlayer1 <- RICH02_SAMg3[ which(RICH02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_SAMg3[ which(RICH02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_SAMg2 <- rbind(RICH02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
RICH02_SAMft <- ftable(RICH02_SAMg2$player1, RICH02_SAMg2$player2)
RICH02_SAMft2 <- as.matrix(RICH02_SAMft)
numRows <- nrow(RICH02_SAMft2)
numCols <- ncol(RICH02_SAMft2)
RICH02_SAMft3 <- RICH02_SAMft2[c(2:numRows) , c(2:numCols)]
RICH02_SAMTable <- graph.adjacency(RICH02_SAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(RICH02_SAMTable, vertex.label = V(RICH02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
RICH02_SAM.clusterCoef <- transitivity(RICH02_SAMTable, type="global") #cluster coefficient
RICH02_SAM.degreeCent <- centralization.degree(RICH02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_SAMftn <- as.network.matrix(RICH02_SAMft)
RICH02_SAM.netDensity <- network.density(RICH02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_SAM.entropy <- entropy(RICH02_SAMft) #entropy

RICH02_SAM.netMx <- cbind(RICH02_SAM.netMx, RICH02_SAM.clusterCoef, RICH02_SAM.degreeCent$centralization,
                          RICH02_SAM.netDensity, RICH02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************
#NA

round = 2
teamName = "RICH"
KIoutcome = "Turnover_AM"
RICH02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
RICH02_TAMg2 <- data.frame(RICH02_TAM)
RICH02_TAMg2 <- RICH02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_TAMg2$player1
player2vector <- RICH02_TAMg2$player2
RICH02_TAMg3 <- RICH02_TAMg2
RICH02_TAMg3$p1inp2vec <- is.element(RICH02_TAMg3$player1, player2vector)
RICH02_TAMg3$p2inp1vec <- is.element(RICH02_TAMg3$player2, player1vector)

addPlayer1 <- RICH02_TAMg3[ which(RICH02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_TAMg3[ which(RICH02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_TAMg2 <- rbind(RICH02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
RICH02_TAMft <- ftable(RICH02_TAMg2$player1, RICH02_TAMg2$player2)
RICH02_TAMft2 <- as.matrix(RICH02_TAMft)
numRows <- nrow(RICH02_TAMft2)
numCols <- ncol(RICH02_TAMft2)
RICH02_TAMft3 <- RICH02_TAMft2[c(2:numRows) , c(2:numCols)]
RICH02_TAMTable <- graph.adjacency(RICH02_TAMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(RICH02_TAMTable, vertex.label = V(RICH02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
RICH02_TAM.clusterCoef <- transitivity(RICH02_TAMTable, type="global") #cluster coefficient
RICH02_TAM.degreeCent <- centralization.degree(RICH02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_TAMftn <- as.network.matrix(RICH02_TAMft)
RICH02_TAM.netDensity <- network.density(RICH02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_TAM.entropy <- entropy(RICH02_TAMft) #entropy

RICH02_TAM.netMx <- cbind(RICH02_TAM.netMx, RICH02_TAM.clusterCoef, RICH02_TAM.degreeCent$centralization,
                          RICH02_TAM.netDensity, RICH02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "RICH"
KIoutcome = "Stoppage_DM"
RICH02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
RICH02_SDMg2 <- data.frame(RICH02_SDM)
RICH02_SDMg2 <- RICH02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_SDMg2$player1
player2vector <- RICH02_SDMg2$player2
RICH02_SDMg3 <- RICH02_SDMg2
RICH02_SDMg3$p1inp2vec <- is.element(RICH02_SDMg3$player1, player2vector)
RICH02_SDMg3$p2inp1vec <- is.element(RICH02_SDMg3$player2, player1vector)

addPlayer1 <- RICH02_SDMg3[ which(RICH02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_SDMg3[ which(RICH02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_SDMg2 <- rbind(RICH02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
RICH02_SDMft <- ftable(RICH02_SDMg2$player1, RICH02_SDMg2$player2)
RICH02_SDMft2 <- as.matrix(RICH02_SDMft)
numRows <- nrow(RICH02_SDMft2)
numCols <- ncol(RICH02_SDMft2)
RICH02_SDMft3 <- RICH02_SDMft2[c(2:numRows) , c(2:numCols)]
RICH02_SDMTable <- graph.adjacency(RICH02_SDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(RICH02_SDMTable, vertex.label = V(RICH02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
RICH02_SDM.clusterCoef <- transitivity(RICH02_SDMTable, type="global") #cluster coefficient
RICH02_SDM.degreeCent <- centralization.degree(RICH02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_SDMftn <- as.network.matrix(RICH02_SDMft)
RICH02_SDM.netDensity <- network.density(RICH02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_SDM.entropy <- entropy(RICH02_SDMft) #entropy

RICH02_SDM.netMx <- cbind(RICH02_SDM.netMx, RICH02_SDM.clusterCoef, RICH02_SDM.degreeCent$centralization,
                          RICH02_SDM.netDensity, RICH02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "RICH"
KIoutcome = "Turnover_DM"
RICH02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
RICH02_TDMg2 <- data.frame(RICH02_TDM)
RICH02_TDMg2 <- RICH02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_TDMg2$player1
player2vector <- RICH02_TDMg2$player2
RICH02_TDMg3 <- RICH02_TDMg2
RICH02_TDMg3$p1inp2vec <- is.element(RICH02_TDMg3$player1, player2vector)
RICH02_TDMg3$p2inp1vec <- is.element(RICH02_TDMg3$player2, player1vector)

addPlayer1 <- RICH02_TDMg3[ which(RICH02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_TDMg3[ which(RICH02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_TDMg2 <- rbind(RICH02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
RICH02_TDMft <- ftable(RICH02_TDMg2$player1, RICH02_TDMg2$player2)
RICH02_TDMft2 <- as.matrix(RICH02_TDMft)
numRows <- nrow(RICH02_TDMft2)
numCols <- ncol(RICH02_TDMft2)
RICH02_TDMft3 <- RICH02_TDMft2[c(2:numRows) , c(2:numCols)]
RICH02_TDMTable <- graph.adjacency(RICH02_TDMft3, mode="directed", weighted = T, diag = F,
                                   add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(RICH02_TDMTable, vertex.label = V(RICH02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
RICH02_TDM.clusterCoef <- transitivity(RICH02_TDMTable, type="global") #cluster coefficient
RICH02_TDM.degreeCent <- centralization.degree(RICH02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_TDMftn <- as.network.matrix(RICH02_TDMft)
RICH02_TDM.netDensity <- network.density(RICH02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_TDM.entropy <- entropy(RICH02_TDMft) #entropy

RICH02_TDM.netMx <- cbind(RICH02_TDM.netMx, RICH02_TDM.clusterCoef, RICH02_TDM.degreeCent$centralization,
                          RICH02_TDM.netDensity, RICH02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "RICH"
KIoutcome = "Stoppage_D"
RICH02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
RICH02_SDg2 <- data.frame(RICH02_SD)
RICH02_SDg2 <- RICH02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_SDg2$player1
player2vector <- RICH02_SDg2$player2
RICH02_SDg3 <- RICH02_SDg2
RICH02_SDg3$p1inp2vec <- is.element(RICH02_SDg3$player1, player2vector)
RICH02_SDg3$p2inp1vec <- is.element(RICH02_SDg3$player2, player1vector)

addPlayer1 <- RICH02_SDg3[ which(RICH02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_SDg3[ which(RICH02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_SDg2 <- rbind(RICH02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
RICH02_SDft <- ftable(RICH02_SDg2$player1, RICH02_SDg2$player2)
RICH02_SDft2 <- as.matrix(RICH02_SDft)
numRows <- nrow(RICH02_SDft2)
numCols <- ncol(RICH02_SDft2)
RICH02_SDft3 <- RICH02_SDft2[c(2:numRows) , c(2:numCols)]
RICH02_SDTable <- graph.adjacency(RICH02_SDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(RICH02_SDTable, vertex.label = V(RICH02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
RICH02_SD.clusterCoef <- transitivity(RICH02_SDTable, type="global") #cluster coefficient
RICH02_SD.degreeCent <- centralization.degree(RICH02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_SDftn <- as.network.matrix(RICH02_SDft)
RICH02_SD.netDensity <- network.density(RICH02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_SD.entropy <- entropy(RICH02_SDft) #entropy

RICH02_SD.netMx <- cbind(RICH02_SD.netMx, RICH02_SD.clusterCoef, RICH02_SD.degreeCent$centralization,
                         RICH02_SD.netDensity, RICH02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "RICH"
KIoutcome = "Turnover_D"
RICH02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
RICH02_TDg2 <- data.frame(RICH02_TD)
RICH02_TDg2 <- RICH02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_TDg2$player1
player2vector <- RICH02_TDg2$player2
RICH02_TDg3 <- RICH02_TDg2
RICH02_TDg3$p1inp2vec <- is.element(RICH02_TDg3$player1, player2vector)
RICH02_TDg3$p2inp1vec <- is.element(RICH02_TDg3$player2, player1vector)

addPlayer1 <- RICH02_TDg3[ which(RICH02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_TDg3[ which(RICH02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_TDg2 <- rbind(RICH02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
RICH02_TDft <- ftable(RICH02_TDg2$player1, RICH02_TDg2$player2)
RICH02_TDft2 <- as.matrix(RICH02_TDft)
numRows <- nrow(RICH02_TDft2)
numCols <- ncol(RICH02_TDft2)
RICH02_TDft3 <- RICH02_TDft2[c(2:numRows) , c(2:numCols)]
RICH02_TDTable <- graph.adjacency(RICH02_TDft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(RICH02_TDTable, vertex.label = V(RICH02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
RICH02_TD.clusterCoef <- transitivity(RICH02_TDTable, type="global") #cluster coefficient
RICH02_TD.degreeCent <- centralization.degree(RICH02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_TDftn <- as.network.matrix(RICH02_TDft)
RICH02_TD.netDensity <- network.density(RICH02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_TD.entropy <- entropy(RICH02_TDft) #entropy

RICH02_TD.netMx <- cbind(RICH02_TD.netMx, RICH02_TD.clusterCoef, RICH02_TD.degreeCent$centralization,
                         RICH02_TD.netDensity, RICH02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "RICH"
KIoutcome = "End of Qtr_DM"
RICH02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
RICH02_QTg2 <- data.frame(RICH02_QT)
RICH02_QTg2 <- RICH02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- RICH02_QTg2$player1
player2vector <- RICH02_QTg2$player2
RICH02_QTg3 <- RICH02_QTg2
RICH02_QTg3$p1inp2vec <- is.element(RICH02_QTg3$player1, player2vector)
RICH02_QTg3$p2inp1vec <- is.element(RICH02_QTg3$player2, player1vector)

addPlayer1 <- RICH02_QTg3[ which(RICH02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- RICH02_QTg3[ which(RICH02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

RICH02_QTg2 <- rbind(RICH02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
RICH02_QTft <- ftable(RICH02_QTg2$player1, RICH02_QTg2$player2)
RICH02_QTft2 <- as.matrix(RICH02_QTft)
numRows <- nrow(RICH02_QTft2)
numCols <- ncol(RICH02_QTft2)
RICH02_QTft3 <- RICH02_QTft2[c(2:numRows) , c(2:numCols)]
RICH02_QTTable <- graph.adjacency(RICH02_QTft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(RICH02_QTTable, vertex.label = V(RICH02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(RICH02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
RICH02_QT.clusterCoef <- transitivity(RICH02_QTTable, type="global") #cluster coefficient
RICH02_QT.degreeCent <- centralization.degree(RICH02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
RICH02_QTftn <- as.network.matrix(RICH02_QTft)
RICH02_QT.netDensity <- network.density(RICH02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
RICH02_QT.entropy <- entropy(RICH02_QTft) #entropy

RICH02_QT.netMx <- cbind(RICH02_QT.netMx, RICH02_QT.clusterCoef, RICH02_QT.degreeCent$centralization,
                         RICH02_QT.netDensity, RICH02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(RICH02_QT.netMx) <- varnames

#############################################################################
#STKILDA

##
#ROUND 2
##

#Round 2, Goal***************************************************************

round = 2
teamName = "STK"
KIoutcome = "Goal_F"
STK02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
STK02_Gg2 <- data.frame(STK02_G)
STK02_Gg2 <- STK02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_Gg2$player1
player2vector <- STK02_Gg2$player2
STK02_Gg3 <- STK02_Gg2
STK02_Gg3$p1inp2vec <- is.element(STK02_Gg3$player1, player2vector)
STK02_Gg3$p2inp1vec <- is.element(STK02_Gg3$player2, player1vector)

addPlayer1 <- STK02_Gg3[ which(STK02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_Gg3[ which(STK02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_Gg2 <- rbind(STK02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
STK02_Gft <- ftable(STK02_Gg2$player1, STK02_Gg2$player2)
STK02_Gft2 <- as.matrix(STK02_Gft)
numRows <- nrow(STK02_Gft2)
numCols <- ncol(STK02_Gft2)
STK02_Gft3 <- STK02_Gft2[c(2:numRows) , c(2:numCols)]
STK02_GTable <- graph.adjacency(STK02_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(STK02_GTable, vertex.label = V(STK02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
STK02_G.clusterCoef <- transitivity(STK02_GTable, type="global") #cluster coefficient
STK02_G.degreeCent <- centralization.degree(STK02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_Gftn <- as.network.matrix(STK02_Gft)
STK02_G.netDensity <- network.density(STK02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_G.entropy <- entropy(STK02_Gft) #entropy

STK02_G.netMx <- cbind(STK02_G.netMx, STK02_G.clusterCoef, STK02_G.degreeCent$centralization,
                       STK02_G.netDensity, STK02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "STK"
KIoutcome = "Behind_F"
STK02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
STK02_Bg2 <- data.frame(STK02_B)
STK02_Bg2 <- STK02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_Bg2$player1
player2vector <- STK02_Bg2$player2
STK02_Bg3 <- STK02_Bg2
STK02_Bg3$p1inp2vec <- is.element(STK02_Bg3$player1, player2vector)
STK02_Bg3$p2inp1vec <- is.element(STK02_Bg3$player2, player1vector)

addPlayer1 <- STK02_Bg3[ which(STK02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_Bg3[ which(STK02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_Bg2 <- rbind(STK02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
STK02_Bft <- ftable(STK02_Bg2$player1, STK02_Bg2$player2)
STK02_Bft2 <- as.matrix(STK02_Bft)
numRows <- nrow(STK02_Bft2)
numCols <- ncol(STK02_Bft2)
STK02_Bft3 <- STK02_Bft2[c(2:numRows) , c(2:numCols)]
STK02_BTable <- graph.adjacency(STK02_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(STK02_BTable, vertex.label = V(STK02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
STK02_B.clusterCoef <- transitivity(STK02_BTable, type="global") #cluster coefficient
STK02_B.degreeCent <- centralization.degree(STK02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_Bftn <- as.network.matrix(STK02_Bft)
STK02_B.netDensity <- network.density(STK02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_B.entropy <- entropy(STK02_Bft) #entropy

STK02_B.netMx <- cbind(STK02_B.netMx, STK02_B.clusterCoef, STK02_B.degreeCent$centralization,
                       STK02_B.netDensity, STK02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "STK"
KIoutcome = "Stoppage_F"
STK02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
STK02_SFg2 <- data.frame(STK02_SF)
STK02_SFg2 <- STK02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_SFg2$player1
player2vector <- STK02_SFg2$player2
STK02_SFg3 <- STK02_SFg2
STK02_SFg3$p1inp2vec <- is.element(STK02_SFg3$player1, player2vector)
STK02_SFg3$p2inp1vec <- is.element(STK02_SFg3$player2, player1vector)

addPlayer1 <- STK02_SFg3[ which(STK02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_SFg3[ which(STK02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_SFg2 <- rbind(STK02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
STK02_SFft <- ftable(STK02_SFg2$player1, STK02_SFg2$player2)
STK02_SFft2 <- as.matrix(STK02_SFft)
numRows <- nrow(STK02_SFft2)
numCols <- ncol(STK02_SFft2)
STK02_SFft3 <- STK02_SFft2[c(2:numRows) , c(2:numCols)]
STK02_SFTable <- graph.adjacency(STK02_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(STK02_SFTable, vertex.label = V(STK02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
STK02_SF.clusterCoef <- transitivity(STK02_SFTable, type="global") #cluster coefficient
STK02_SF.degreeCent <- centralization.degree(STK02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_SFftn <- as.network.matrix(STK02_SFft)
STK02_SF.netDensity <- network.density(STK02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_SF.entropy <- entropy(STK02_SFft) #entropy

STK02_SF.netMx <- cbind(STK02_SF.netMx, STK02_SF.clusterCoef, STK02_SF.degreeCent$centralization,
                        STK02_SF.netDensity, STK02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************

round = 2
teamName = "STK"
KIoutcome = "Turnover_F"
STK02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
STK02_TFg2 <- data.frame(STK02_TF)
STK02_TFg2 <- STK02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_TFg2$player1
player2vector <- STK02_TFg2$player2
STK02_TFg3 <- STK02_TFg2
STK02_TFg3$p1inp2vec <- is.element(STK02_TFg3$player1, player2vector)
STK02_TFg3$p2inp1vec <- is.element(STK02_TFg3$player2, player1vector)

addPlayer1 <- STK02_TFg3[ which(STK02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_TFg3[ which(STK02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_TFg2 <- rbind(STK02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
STK02_TFft <- ftable(STK02_TFg2$player1, STK02_TFg2$player2)
STK02_TFft2 <- as.matrix(STK02_TFft)
numRows <- nrow(STK02_TFft2)
numCols <- ncol(STK02_TFft2)
STK02_TFft3 <- STK02_TFft2[c(2:numRows) , c(2:numCols)]
STK02_TFTable <- graph.adjacency(STK02_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(STK02_TFTable, vertex.label = V(STK02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
STK02_TF.clusterCoef <- transitivity(STK02_TFTable, type="global") #cluster coefficient
STK02_TF.degreeCent <- centralization.degree(STK02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_TFftn <- as.network.matrix(STK02_TFft)
STK02_TF.netDensity <- network.density(STK02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_TF.entropy <- entropy(STK02_TFft) #entropy

STK02_TF.netMx <- cbind(STK02_TF.netMx, STK02_TF.clusterCoef, STK02_TF.degreeCent$centralization,
                        STK02_TF.netDensity, STK02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************

round = 2
teamName = "STK"
KIoutcome = "Stoppage_AM"
STK02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
STK02_SAMg2 <- data.frame(STK02_SAM)
STK02_SAMg2 <- STK02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_SAMg2$player1
player2vector <- STK02_SAMg2$player2
STK02_SAMg3 <- STK02_SAMg2
STK02_SAMg3$p1inp2vec <- is.element(STK02_SAMg3$player1, player2vector)
STK02_SAMg3$p2inp1vec <- is.element(STK02_SAMg3$player2, player1vector)

addPlayer1 <- STK02_SAMg3[ which(STK02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_SAMg3[ which(STK02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_SAMg2 <- rbind(STK02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
STK02_SAMft <- ftable(STK02_SAMg2$player1, STK02_SAMg2$player2)
STK02_SAMft2 <- as.matrix(STK02_SAMft)
numRows <- nrow(STK02_SAMft2)
numCols <- ncol(STK02_SAMft2)
STK02_SAMft3 <- STK02_SAMft2[c(2:numRows) , c(2:numCols)]
STK02_SAMTable <- graph.adjacency(STK02_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(STK02_SAMTable, vertex.label = V(STK02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
STK02_SAM.clusterCoef <- transitivity(STK02_SAMTable, type="global") #cluster coefficient
STK02_SAM.degreeCent <- centralization.degree(STK02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_SAMftn <- as.network.matrix(STK02_SAMft)
STK02_SAM.netDensity <- network.density(STK02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_SAM.entropy <- entropy(STK02_SAMft) #entropy

STK02_SAM.netMx <- cbind(STK02_SAM.netMx, STK02_SAM.clusterCoef, STK02_SAM.degreeCent$centralization,
                         STK02_SAM.netDensity, STK02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "STK"
KIoutcome = "Turnover_AM"
STK02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
STK02_TAMg2 <- data.frame(STK02_TAM)
STK02_TAMg2 <- STK02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_TAMg2$player1
player2vector <- STK02_TAMg2$player2
STK02_TAMg3 <- STK02_TAMg2
STK02_TAMg3$p1inp2vec <- is.element(STK02_TAMg3$player1, player2vector)
STK02_TAMg3$p2inp1vec <- is.element(STK02_TAMg3$player2, player1vector)

addPlayer1 <- STK02_TAMg3[ which(STK02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_TAMg3[ which(STK02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_TAMg2 <- rbind(STK02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
STK02_TAMft <- ftable(STK02_TAMg2$player1, STK02_TAMg2$player2)
STK02_TAMft2 <- as.matrix(STK02_TAMft)
numRows <- nrow(STK02_TAMft2)
numCols <- ncol(STK02_TAMft2)
STK02_TAMft3 <- STK02_TAMft2[c(2:numRows) , c(2:numCols)]
STK02_TAMTable <- graph.adjacency(STK02_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(STK02_TAMTable, vertex.label = V(STK02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
STK02_TAM.clusterCoef <- transitivity(STK02_TAMTable, type="global") #cluster coefficient
STK02_TAM.degreeCent <- centralization.degree(STK02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_TAMftn <- as.network.matrix(STK02_TAMft)
STK02_TAM.netDensity <- network.density(STK02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_TAM.entropy <- entropy(STK02_TAMft) #entropy

STK02_TAM.netMx <- cbind(STK02_TAM.netMx, STK02_TAM.clusterCoef, STK02_TAM.degreeCent$centralization,
                         STK02_TAM.netDensity, STK02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************

round = 2
teamName = "STK"
KIoutcome = "Stoppage_DM"
STK02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
STK02_SDMg2 <- data.frame(STK02_SDM)
STK02_SDMg2 <- STK02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_SDMg2$player1
player2vector <- STK02_SDMg2$player2
STK02_SDMg3 <- STK02_SDMg2
STK02_SDMg3$p1inp2vec <- is.element(STK02_SDMg3$player1, player2vector)
STK02_SDMg3$p2inp1vec <- is.element(STK02_SDMg3$player2, player1vector)

addPlayer1 <- STK02_SDMg3[ which(STK02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_SDMg3[ which(STK02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_SDMg2 <- rbind(STK02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
STK02_SDMft <- ftable(STK02_SDMg2$player1, STK02_SDMg2$player2)
STK02_SDMft2 <- as.matrix(STK02_SDMft)
numRows <- nrow(STK02_SDMft2)
numCols <- ncol(STK02_SDMft2)
STK02_SDMft3 <- STK02_SDMft2[c(2:numRows) , c(2:numCols)]
STK02_SDMTable <- graph.adjacency(STK02_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(STK02_SDMTable, vertex.label = V(STK02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
STK02_SDM.clusterCoef <- transitivity(STK02_SDMTable, type="global") #cluster coefficient
STK02_SDM.degreeCent <- centralization.degree(STK02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_SDMftn <- as.network.matrix(STK02_SDMft)
STK02_SDM.netDensity <- network.density(STK02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_SDM.entropy <- entropy(STK02_SDMft) #entropy

STK02_SDM.netMx <- cbind(STK02_SDM.netMx, STK02_SDM.clusterCoef, STK02_SDM.degreeCent$centralization,
                         STK02_SDM.netDensity, STK02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "STK"
KIoutcome = "Turnover_DM"
STK02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
STK02_TDMg2 <- data.frame(STK02_TDM)
STK02_TDMg2 <- STK02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_TDMg2$player1
player2vector <- STK02_TDMg2$player2
STK02_TDMg3 <- STK02_TDMg2
STK02_TDMg3$p1inp2vec <- is.element(STK02_TDMg3$player1, player2vector)
STK02_TDMg3$p2inp1vec <- is.element(STK02_TDMg3$player2, player1vector)

addPlayer1 <- STK02_TDMg3[ which(STK02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_TDMg3[ which(STK02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_TDMg2 <- rbind(STK02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
STK02_TDMft <- ftable(STK02_TDMg2$player1, STK02_TDMg2$player2)
STK02_TDMft2 <- as.matrix(STK02_TDMft)
numRows <- nrow(STK02_TDMft2)
numCols <- ncol(STK02_TDMft2)
STK02_TDMft3 <- STK02_TDMft2[c(2:numRows) , c(2:numCols)]
STK02_TDMTable <- graph.adjacency(STK02_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(STK02_TDMTable, vertex.label = V(STK02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
STK02_TDM.clusterCoef <- transitivity(STK02_TDMTable, type="global") #cluster coefficient
STK02_TDM.degreeCent <- centralization.degree(STK02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_TDMftn <- as.network.matrix(STK02_TDMft)
STK02_TDM.netDensity <- network.density(STK02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_TDM.entropy <- entropy(STK02_TDMft) #entropy

STK02_TDM.netMx <- cbind(STK02_TDM.netMx, STK02_TDM.clusterCoef, STK02_TDM.degreeCent$centralization,
                         STK02_TDM.netDensity, STK02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "STK"
KIoutcome = "Stoppage_D"
STK02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
STK02_SDg2 <- data.frame(STK02_SD)
STK02_SDg2 <- STK02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_SDg2$player1
player2vector <- STK02_SDg2$player2
STK02_SDg3 <- STK02_SDg2
STK02_SDg3$p1inp2vec <- is.element(STK02_SDg3$player1, player2vector)
STK02_SDg3$p2inp1vec <- is.element(STK02_SDg3$player2, player1vector)

addPlayer1 <- STK02_SDg3[ which(STK02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_SDg3[ which(STK02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_SDg2 <- rbind(STK02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
STK02_SDft <- ftable(STK02_SDg2$player1, STK02_SDg2$player2)
STK02_SDft2 <- as.matrix(STK02_SDft)
numRows <- nrow(STK02_SDft2)
numCols <- ncol(STK02_SDft2)
STK02_SDft3 <- STK02_SDft2[c(2:numRows) , c(2:numCols)]
STK02_SDTable <- graph.adjacency(STK02_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(STK02_SDTable, vertex.label = V(STK02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
STK02_SD.clusterCoef <- transitivity(STK02_SDTable, type="global") #cluster coefficient
STK02_SD.degreeCent <- centralization.degree(STK02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_SDftn <- as.network.matrix(STK02_SDft)
STK02_SD.netDensity <- network.density(STK02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_SD.entropy <- entropy(STK02_SDft) #entropy

STK02_SD.netMx <- cbind(STK02_SD.netMx, STK02_SD.clusterCoef, STK02_SD.degreeCent$centralization,
                        STK02_SD.netDensity, STK02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "STK"
KIoutcome = "Turnover_D"
STK02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
STK02_TDg2 <- data.frame(STK02_TD)
STK02_TDg2 <- STK02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_TDg2$player1
player2vector <- STK02_TDg2$player2
STK02_TDg3 <- STK02_TDg2
STK02_TDg3$p1inp2vec <- is.element(STK02_TDg3$player1, player2vector)
STK02_TDg3$p2inp1vec <- is.element(STK02_TDg3$player2, player1vector)

addPlayer1 <- STK02_TDg3[ which(STK02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_TDg3[ which(STK02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_TDg2 <- rbind(STK02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
STK02_TDft <- ftable(STK02_TDg2$player1, STK02_TDg2$player2)
STK02_TDft2 <- as.matrix(STK02_TDft)
numRows <- nrow(STK02_TDft2)
numCols <- ncol(STK02_TDft2)
STK02_TDft3 <- STK02_TDft2[c(2:numRows) , c(2:numCols)]
STK02_TDTable <- graph.adjacency(STK02_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(STK02_TDTable, vertex.label = V(STK02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
STK02_TD.clusterCoef <- transitivity(STK02_TDTable, type="global") #cluster coefficient
STK02_TD.degreeCent <- centralization.degree(STK02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_TDftn <- as.network.matrix(STK02_TDft)
STK02_TD.netDensity <- network.density(STK02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_TD.entropy <- entropy(STK02_TDft) #entropy

STK02_TD.netMx <- cbind(STK02_TD.netMx, STK02_TD.clusterCoef, STK02_TD.degreeCent$centralization,
                        STK02_TD.netDensity, STK02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "STK"
KIoutcome = "End of Qtr_DM"
STK02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
STK02_QTg2 <- data.frame(STK02_QT)
STK02_QTg2 <- STK02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- STK02_QTg2$player1
player2vector <- STK02_QTg2$player2
STK02_QTg3 <- STK02_QTg2
STK02_QTg3$p1inp2vec <- is.element(STK02_QTg3$player1, player2vector)
STK02_QTg3$p2inp1vec <- is.element(STK02_QTg3$player2, player1vector)

addPlayer1 <- STK02_QTg3[ which(STK02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- STK02_QTg3[ which(STK02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

STK02_QTg2 <- rbind(STK02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
STK02_QTft <- ftable(STK02_QTg2$player1, STK02_QTg2$player2)
STK02_QTft2 <- as.matrix(STK02_QTft)
numRows <- nrow(STK02_QTft2)
numCols <- ncol(STK02_QTft2)
STK02_QTft3 <- STK02_QTft2[c(2:numRows) , c(2:numCols)]
STK02_QTTable <- graph.adjacency(STK02_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(STK02_QTTable, vertex.label = V(STK02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(STK02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
STK02_QT.clusterCoef <- transitivity(STK02_QTTable, type="global") #cluster coefficient
STK02_QT.degreeCent <- centralization.degree(STK02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
STK02_QTftn <- as.network.matrix(STK02_QTft)
STK02_QT.netDensity <- network.density(STK02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
STK02_QT.entropy <- entropy(STK02_QTft) #entropy

STK02_QT.netMx <- cbind(STK02_QT.netMx, STK02_QT.clusterCoef, STK02_QT.degreeCent$centralization,
                        STK02_QT.netDensity, STK02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(STK02_QT.netMx) <- varnames

#############################################################################
#SYDNEY

##
#ROUND 2
##

#Round 2, Goal***************************************************************

round = 2
teamName = "SYD"
KIoutcome = "Goal_F"
SYD02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
SYD02_Gg2 <- data.frame(SYD02_G)
SYD02_Gg2 <- SYD02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_Gg2$player1
player2vector <- SYD02_Gg2$player2
SYD02_Gg3 <- SYD02_Gg2
SYD02_Gg3$p1inp2vec <- is.element(SYD02_Gg3$player1, player2vector)
SYD02_Gg3$p2inp1vec <- is.element(SYD02_Gg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- SYD02_Gg3[ which(SYD02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

SYD02_Gg2 <- rbind(SYD02_Gg2, addPlayer2)

#Round 2, Goal graph using weighted edges
SYD02_Gft <- ftable(SYD02_Gg2$player1, SYD02_Gg2$player2)
SYD02_Gft2 <- as.matrix(SYD02_Gft)
numRows <- nrow(SYD02_Gft2)
numCols <- ncol(SYD02_Gft2)
SYD02_Gft3 <- SYD02_Gft2[c(1:numRows) , c(2:numCols)]
SYD02_GTable <- graph.adjacency(SYD02_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(SYD02_GTable, vertex.label = V(SYD02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
SYD02_G.clusterCoef <- transitivity(SYD02_GTable, type="global") #cluster coefficient
SYD02_G.degreeCent <- centralization.degree(SYD02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_Gftn <- as.network.matrix(SYD02_Gft)
SYD02_G.netDensity <- network.density(SYD02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_G.entropy <- entropy(SYD02_Gft) #entropy

SYD02_G.netMx <- cbind(SYD02_G.netMx, SYD02_G.clusterCoef, SYD02_G.degreeCent$centralization,
                       SYD02_G.netDensity, SYD02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "SYD"
KIoutcome = "Behind_F"
SYD02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
SYD02_Bg2 <- data.frame(SYD02_B)
SYD02_Bg2 <- SYD02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_Bg2$player1
player2vector <- SYD02_Bg2$player2
SYD02_Bg3 <- SYD02_Bg2
SYD02_Bg3$p1inp2vec <- is.element(SYD02_Bg3$player1, player2vector)
SYD02_Bg3$p2inp1vec <- is.element(SYD02_Bg3$player2, player1vector)

addPlayer1 <- SYD02_Bg3[ which(SYD02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD02_Bg3[ which(SYD02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD02_Bg2 <- rbind(SYD02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
SYD02_Bft <- ftable(SYD02_Bg2$player1, SYD02_Bg2$player2)
SYD02_Bft2 <- as.matrix(SYD02_Bft)
numRows <- nrow(SYD02_Bft2)
numCols <- ncol(SYD02_Bft2)
SYD02_Bft3 <- SYD02_Bft2[c(2:numRows) , c(2:numCols)]
SYD02_BTable <- graph.adjacency(SYD02_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(SYD02_BTable, vertex.label = V(SYD02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
SYD02_B.clusterCoef <- transitivity(SYD02_BTable, type="global") #cluster coefficient
SYD02_B.degreeCent <- centralization.degree(SYD02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_Bftn <- as.network.matrix(SYD02_Bft)
SYD02_B.netDensity <- network.density(SYD02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_B.entropy <- entropy(SYD02_Bft) #entropy

SYD02_B.netMx <- cbind(SYD02_B.netMx, SYD02_B.clusterCoef, SYD02_B.degreeCent$centralization,
                       SYD02_B.netDensity, SYD02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "SYD"
KIoutcome = "Stoppage_F"
SYD02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
SYD02_SFg2 <- data.frame(SYD02_SF)
SYD02_SFg2 <- SYD02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_SFg2$player1
player2vector <- SYD02_SFg2$player2
SYD02_SFg3 <- SYD02_SFg2
SYD02_SFg3$p1inp2vec <- is.element(SYD02_SFg3$player1, player2vector)
SYD02_SFg3$p2inp1vec <- is.element(SYD02_SFg3$player2, player1vector)

addPlayer1 <- SYD02_SFg3[ which(SYD02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD02_SFg3[ which(SYD02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD02_SFg2 <- rbind(SYD02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
SYD02_SFft <- ftable(SYD02_SFg2$player1, SYD02_SFg2$player2)
SYD02_SFft2 <- as.matrix(SYD02_SFft)
numRows <- nrow(SYD02_SFft2)
numCols <- ncol(SYD02_SFft2)
SYD02_SFft3 <- SYD02_SFft2[c(2:numRows) , c(2:numCols)]
SYD02_SFTable <- graph.adjacency(SYD02_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(SYD02_SFTable, vertex.label = V(SYD02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
SYD02_SF.clusterCoef <- transitivity(SYD02_SFTable, type="global") #cluster coefficient
SYD02_SF.degreeCent <- centralization.degree(SYD02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_SFftn <- as.network.matrix(SYD02_SFft)
SYD02_SF.netDensity <- network.density(SYD02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_SF.entropy <- entropy(SYD02_SFft) #entropy

SYD02_SF.netMx <- cbind(SYD02_SF.netMx, SYD02_SF.clusterCoef, SYD02_SF.degreeCent$centralization,
                        SYD02_SF.netDensity, SYD02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************
#NA

round = 2
teamName = "SYD"
KIoutcome = "Turnover_F"
SYD02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
SYD02_TFg2 <- data.frame(SYD02_TF)
SYD02_TFg2 <- SYD02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_TFg2$player1
player2vector <- SYD02_TFg2$player2
SYD02_TFg3 <- SYD02_TFg2
SYD02_TFg3$p1inp2vec <- is.element(SYD02_TFg3$player1, player2vector)
SYD02_TFg3$p2inp1vec <- is.element(SYD02_TFg3$player2, player1vector)

addPlayer1 <- SYD02_TFg3[ which(SYD02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD02_TFg3[ which(SYD02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD02_TFg2 <- rbind(SYD02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
SYD02_TFft <- ftable(SYD02_TFg2$player1, SYD02_TFg2$player2)
SYD02_TFft2 <- as.matrix(SYD02_TFft)
numRows <- nrow(SYD02_TFft2)
numCols <- ncol(SYD02_TFft2)
SYD02_TFft3 <- SYD02_TFft2[c(2:numRows) , c(2:numCols)]
SYD02_TFTable <- graph.adjacency(SYD02_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(SYD02_TFTable, vertex.label = V(SYD02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
SYD02_TF.clusterCoef <- transitivity(SYD02_TFTable, type="global") #cluster coefficient
SYD02_TF.degreeCent <- centralization.degree(SYD02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_TFftn <- as.network.matrix(SYD02_TFft)
SYD02_TF.netDensity <- network.density(SYD02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_TF.entropy <- entropy(SYD02_TFft) #entropy

SYD02_TF.netMx <- cbind(SYD02_TF.netMx, SYD02_TF.clusterCoef, SYD02_TF.degreeCent$centralization,
                        SYD02_TF.netDensity, SYD02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************

round = 2
teamName = "SYD"
KIoutcome = "Stoppage_AM"
SYD02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
SYD02_SAMg2 <- data.frame(SYD02_SAM)
SYD02_SAMg2 <- SYD02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_SAMg2$player1
player2vector <- SYD02_SAMg2$player2
SYD02_SAMg3 <- SYD02_SAMg2
SYD02_SAMg3$p1inp2vec <- is.element(SYD02_SAMg3$player1, player2vector)
SYD02_SAMg3$p2inp1vec <- is.element(SYD02_SAMg3$player2, player1vector)

empty <- ""
zero <- 0
addPlayer2 <- SYD02_SAMg3[ which(SYD02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
varnames <- c("player1", "player2", "weight")
colnames(addPlayer2) <- varnames

SYD02_SAMg2 <- rbind(SYD02_SAMg2, addPlayer2)

#Round 2, AM Stoppage graph using weighted edges
SYD02_SAMft <- ftable(SYD02_SAMg2$player1, SYD02_SAMg2$player2)
SYD02_SAMft2 <- as.matrix(SYD02_SAMft)
numRows <- nrow(SYD02_SAMft2)
numCols <- ncol(SYD02_SAMft2)
SYD02_SAMft3 <- SYD02_SAMft2[c(1:numRows) , c(2:numCols)]
SYD02_SAMTable <- graph.adjacency(SYD02_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(SYD02_SAMTable, vertex.label = V(SYD02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
SYD02_SAM.clusterCoef <- transitivity(SYD02_SAMTable, type="global") #cluster coefficient
SYD02_SAM.degreeCent <- centralization.degree(SYD02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_SAMftn <- as.network.matrix(SYD02_SAMft)
SYD02_SAM.netDensity <- network.density(SYD02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_SAM.entropy <- entropy(SYD02_SAMft) #entropy

SYD02_SAM.netMx <- cbind(SYD02_SAM.netMx, SYD02_SAM.clusterCoef, SYD02_SAM.degreeCent$centralization,
                         SYD02_SAM.netDensity, SYD02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "SYD"
KIoutcome = "Turnover_AM"
SYD02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
SYD02_TAMg2 <- data.frame(SYD02_TAM)
SYD02_TAMg2 <- SYD02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_TAMg2$player1
player2vector <- SYD02_TAMg2$player2
SYD02_TAMg3 <- SYD02_TAMg2
SYD02_TAMg3$p1inp2vec <- is.element(SYD02_TAMg3$player1, player2vector)
SYD02_TAMg3$p2inp1vec <- is.element(SYD02_TAMg3$player2, player1vector)

addPlayer1 <- SYD02_TAMg3[ which(SYD02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD02_TAMg3[ which(SYD02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD02_TAMg2 <- rbind(SYD02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
SYD02_TAMft <- ftable(SYD02_TAMg2$player1, SYD02_TAMg2$player2)
SYD02_TAMft2 <- as.matrix(SYD02_TAMft)
numRows <- nrow(SYD02_TAMft2)
numCols <- ncol(SYD02_TAMft2)
SYD02_TAMft3 <- SYD02_TAMft2[c(2:numRows) , c(2:numCols)]
SYD02_TAMTable <- graph.adjacency(SYD02_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(SYD02_TAMTable, vertex.label = V(SYD02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
SYD02_TAM.clusterCoef <- transitivity(SYD02_TAMTable, type="global") #cluster coefficient
SYD02_TAM.degreeCent <- centralization.degree(SYD02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_TAMftn <- as.network.matrix(SYD02_TAMft)
SYD02_TAM.netDensity <- network.density(SYD02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_TAM.entropy <- entropy(SYD02_TAMft) #entropy

SYD02_TAM.netMx <- cbind(SYD02_TAM.netMx, SYD02_TAM.clusterCoef, SYD02_TAM.degreeCent$centralization,
                         SYD02_TAM.netDensity, SYD02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************
#NA

round = 2
teamName = "SYD"
KIoutcome = "Stoppage_DM"
SYD02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
SYD02_SDMg2 <- data.frame(SYD02_SDM)
SYD02_SDMg2 <- SYD02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_SDMg2$player1
player2vector <- SYD02_SDMg2$player2
SYD02_SDMg3 <- SYD02_SDMg2
SYD02_SDMg3$p1inp2vec <- is.element(SYD02_SDMg3$player1, player2vector)
SYD02_SDMg3$p2inp1vec <- is.element(SYD02_SDMg3$player2, player1vector)

addPlayer1 <- SYD02_SDMg3[ which(SYD02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD02_SDMg3[ which(SYD02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD02_SDMg2 <- rbind(SYD02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
SYD02_SDMft <- ftable(SYD02_SDMg2$player1, SYD02_SDMg2$player2)
SYD02_SDMft2 <- as.matrix(SYD02_SDMft)
numRows <- nrow(SYD02_SDMft2)
numCols <- ncol(SYD02_SDMft2)
SYD02_SDMft3 <- SYD02_SDMft2[c(2:numRows) , c(2:numCols)]
SYD02_SDMTable <- graph.adjacency(SYD02_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(SYD02_SDMTable, vertex.label = V(SYD02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
SYD02_SDM.clusterCoef <- transitivity(SYD02_SDMTable, type="global") #cluster coefficient
SYD02_SDM.degreeCent <- centralization.degree(SYD02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_SDMftn <- as.network.matrix(SYD02_SDMft)
SYD02_SDM.netDensity <- network.density(SYD02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_SDM.entropy <- entropy(SYD02_SDMft) #entropy

SYD02_SDM.netMx <- cbind(SYD02_SDM.netMx, SYD02_SDM.clusterCoef, SYD02_SDM.degreeCent$centralization,
                         SYD02_SDM.netDensity, SYD02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "SYD"
KIoutcome = "Turnover_DM"
SYD02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
SYD02_TDMg2 <- data.frame(SYD02_TDM)
SYD02_TDMg2 <- SYD02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_TDMg2$player1
player2vector <- SYD02_TDMg2$player2
SYD02_TDMg3 <- SYD02_TDMg2
SYD02_TDMg3$p1inp2vec <- is.element(SYD02_TDMg3$player1, player2vector)
SYD02_TDMg3$p2inp1vec <- is.element(SYD02_TDMg3$player2, player1vector)

addPlayer1 <- SYD02_TDMg3[ which(SYD02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD02_TDMg3[ which(SYD02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD02_TDMg2 <- rbind(SYD02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
SYD02_TDMft <- ftable(SYD02_TDMg2$player1, SYD02_TDMg2$player2)
SYD02_TDMft2 <- as.matrix(SYD02_TDMft)
numRows <- nrow(SYD02_TDMft2)
numCols <- ncol(SYD02_TDMft2)
SYD02_TDMft3 <- SYD02_TDMft2[c(2:numRows) , c(2:numCols)]
SYD02_TDMTable <- graph.adjacency(SYD02_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(SYD02_TDMTable, vertex.label = V(SYD02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
SYD02_TDM.clusterCoef <- transitivity(SYD02_TDMTable, type="global") #cluster coefficient
SYD02_TDM.degreeCent <- centralization.degree(SYD02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_TDMftn <- as.network.matrix(SYD02_TDMft)
SYD02_TDM.netDensity <- network.density(SYD02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_TDM.entropy <- entropy(SYD02_TDMft) #entropy

SYD02_TDM.netMx <- cbind(SYD02_TDM.netMx, SYD02_TDM.clusterCoef, SYD02_TDM.degreeCent$centralization,
                         SYD02_TDM.netDensity, SYD02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "SYD"
KIoutcome = "Stoppage_D"
SYD02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
SYD02_SDg2 <- data.frame(SYD02_SD)
SYD02_SDg2 <- SYD02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_SDg2$player1
player2vector <- SYD02_SDg2$player2
SYD02_SDg3 <- SYD02_SDg2
SYD02_SDg3$p1inp2vec <- is.element(SYD02_SDg3$player1, player2vector)
SYD02_SDg3$p2inp1vec <- is.element(SYD02_SDg3$player2, player1vector)

addPlayer1 <- SYD02_SDg3[ which(SYD02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD02_SDg3[ which(SYD02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD02_SDg2 <- rbind(SYD02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
SYD02_SDft <- ftable(SYD02_SDg2$player1, SYD02_SDg2$player2)
SYD02_SDft2 <- as.matrix(SYD02_SDft)
numRows <- nrow(SYD02_SDft2)
numCols <- ncol(SYD02_SDft2)
SYD02_SDft3 <- SYD02_SDft2[c(2:numRows) , c(2:numCols)]
SYD02_SDTable <- graph.adjacency(SYD02_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(SYD02_SDTable, vertex.label = V(SYD02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
SYD02_SD.clusterCoef <- transitivity(SYD02_SDTable, type="global") #cluster coefficient
SYD02_SD.degreeCent <- centralization.degree(SYD02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_SDftn <- as.network.matrix(SYD02_SDft)
SYD02_SD.netDensity <- network.density(SYD02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_SD.entropy <- entropy(SYD02_SDft) #entropy

SYD02_SD.netMx <- cbind(SYD02_SD.netMx, SYD02_SD.clusterCoef, SYD02_SD.degreeCent$centralization,
                        SYD02_SD.netDensity, SYD02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "SYD"
KIoutcome = "Turnover_D"
SYD02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
SYD02_TDg2 <- data.frame(SYD02_TD)
SYD02_TDg2 <- SYD02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_TDg2$player1
player2vector <- SYD02_TDg2$player2
SYD02_TDg3 <- SYD02_TDg2
SYD02_TDg3$p1inp2vec <- is.element(SYD02_TDg3$player1, player2vector)
SYD02_TDg3$p2inp1vec <- is.element(SYD02_TDg3$player2, player1vector)

addPlayer1 <- SYD02_TDg3[ which(SYD02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD02_TDg3[ which(SYD02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD02_TDg2 <- rbind(SYD02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
SYD02_TDft <- ftable(SYD02_TDg2$player1, SYD02_TDg2$player2)
SYD02_TDft2 <- as.matrix(SYD02_TDft)
numRows <- nrow(SYD02_TDft2)
numCols <- ncol(SYD02_TDft2)
SYD02_TDft3 <- SYD02_TDft2[c(2:numRows) , c(2:numCols)]
SYD02_TDTable <- graph.adjacency(SYD02_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(SYD02_TDTable, vertex.label = V(SYD02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
SYD02_TD.clusterCoef <- transitivity(SYD02_TDTable, type="global") #cluster coefficient
SYD02_TD.degreeCent <- centralization.degree(SYD02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_TDftn <- as.network.matrix(SYD02_TDft)
SYD02_TD.netDensity <- network.density(SYD02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_TD.entropy <- entropy(SYD02_TDft) #entropy

SYD02_TD.netMx <- cbind(SYD02_TD.netMx, SYD02_TD.clusterCoef, SYD02_TD.degreeCent$centralization,
                        SYD02_TD.netDensity, SYD02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "SYD"
KIoutcome = "End of Qtr_DM"
SYD02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
SYD02_QTg2 <- data.frame(SYD02_QT)
SYD02_QTg2 <- SYD02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- SYD02_QTg2$player1
player2vector <- SYD02_QTg2$player2
SYD02_QTg3 <- SYD02_QTg2
SYD02_QTg3$p1inp2vec <- is.element(SYD02_QTg3$player1, player2vector)
SYD02_QTg3$p2inp1vec <- is.element(SYD02_QTg3$player2, player1vector)

addPlayer1 <- SYD02_QTg3[ which(SYD02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- SYD02_QTg3[ which(SYD02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

SYD02_QTg2 <- rbind(SYD02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
SYD02_QTft <- ftable(SYD02_QTg2$player1, SYD02_QTg2$player2)
SYD02_QTft2 <- as.matrix(SYD02_QTft)
numRows <- nrow(SYD02_QTft2)
numCols <- ncol(SYD02_QTft2)
SYD02_QTft3 <- SYD02_QTft2[c(2:numRows) , c(2:numCols)]
SYD02_QTTable <- graph.adjacency(SYD02_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(SYD02_QTTable, vertex.label = V(SYD02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(SYD02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
SYD02_QT.clusterCoef <- transitivity(SYD02_QTTable, type="global") #cluster coefficient
SYD02_QT.degreeCent <- centralization.degree(SYD02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
SYD02_QTftn <- as.network.matrix(SYD02_QTft)
SYD02_QT.netDensity <- network.density(SYD02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
SYD02_QT.entropy <- entropy(SYD02_QTft) #entropy

SYD02_QT.netMx <- cbind(SYD02_QT.netMx, SYD02_QT.clusterCoef, SYD02_QT.degreeCent$centralization,
                        SYD02_QT.netDensity, SYD02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(SYD02_QT.netMx) <- varnames

#############################################################################
#WESTERN BULLDOGS

##
#ROUND 2
##

#Round 2, Goal***************************************************************
#NA

round = 2
teamName = "WB"
KIoutcome = "Goal_F"
WB02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
WB02_Gg2 <- data.frame(WB02_G)
WB02_Gg2 <- WB02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_Gg2$player1
player2vector <- WB02_Gg2$player2
WB02_Gg3 <- WB02_Gg2
WB02_Gg3$p1inp2vec <- is.element(WB02_Gg3$player1, player2vector)
WB02_Gg3$p2inp1vec <- is.element(WB02_Gg3$player2, player1vector)

addPlayer1 <- WB02_Gg3[ which(WB02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_Gg3[ which(WB02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_Gg2 <- rbind(WB02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
WB02_Gft <- ftable(WB02_Gg2$player1, WB02_Gg2$player2)
WB02_Gft2 <- as.matrix(WB02_Gft)
numRows <- nrow(WB02_Gft2)
numCols <- ncol(WB02_Gft2)
WB02_Gft3 <- WB02_Gft2[c(2:numRows) , c(2:numCols)]
WB02_GTable <- graph.adjacency(WB02_Gft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(WB02_GTable, vertex.label = V(WB02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
WB02_G.clusterCoef <- transitivity(WB02_GTable, type="global") #cluster coefficient
WB02_G.degreeCent <- centralization.degree(WB02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_Gftn <- as.network.matrix(WB02_Gft)
WB02_G.netDensity <- network.density(WB02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_G.entropy <- entropy(WB02_Gft) #entropy

WB02_G.netMx <- cbind(WB02_G.netMx, WB02_G.clusterCoef, WB02_G.degreeCent$centralization,
                      WB02_G.netDensity, WB02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "WB"
KIoutcome = "Behind_F"
WB02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
WB02_Bg2 <- data.frame(WB02_B)
WB02_Bg2 <- WB02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_Bg2$player1
player2vector <- WB02_Bg2$player2
WB02_Bg3 <- WB02_Bg2
WB02_Bg3$p1inp2vec <- is.element(WB02_Bg3$player1, player2vector)
WB02_Bg3$p2inp1vec <- is.element(WB02_Bg3$player2, player1vector)

addPlayer1 <- WB02_Bg3[ which(WB02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_Bg3[ which(WB02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_Bg2 <- rbind(WB02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
WB02_Bft <- ftable(WB02_Bg2$player1, WB02_Bg2$player2)
WB02_Bft2 <- as.matrix(WB02_Bft)
numRows <- nrow(WB02_Bft2)
numCols <- ncol(WB02_Bft2)
WB02_Bft3 <- WB02_Bft2[c(2:numRows) , c(2:numCols)]
WB02_BTable <- graph.adjacency(WB02_Bft3, mode="directed", weighted = T, diag = F,
                               add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(WB02_BTable, vertex.label = V(WB02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
WB02_B.clusterCoef <- transitivity(WB02_BTable, type="global") #cluster coefficient
WB02_B.degreeCent <- centralization.degree(WB02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_Bftn <- as.network.matrix(WB02_Bft)
WB02_B.netDensity <- network.density(WB02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_B.entropy <- entropy(WB02_Bft) #entropy

WB02_B.netMx <- cbind(WB02_B.netMx, WB02_B.clusterCoef, WB02_B.degreeCent$centralization,
                      WB02_B.netDensity, WB02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************
#NA

round = 2
teamName = "WB"
KIoutcome = "Stoppage_F"
WB02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
WB02_SFg2 <- data.frame(WB02_SF)
WB02_SFg2 <- WB02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_SFg2$player1
player2vector <- WB02_SFg2$player2
WB02_SFg3 <- WB02_SFg2
WB02_SFg3$p1inp2vec <- is.element(WB02_SFg3$player1, player2vector)
WB02_SFg3$p2inp1vec <- is.element(WB02_SFg3$player2, player1vector)

addPlayer1 <- WB02_SFg3[ which(WB02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_SFg3[ which(WB02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_SFg2 <- rbind(WB02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
WB02_SFft <- ftable(WB02_SFg2$player1, WB02_SFg2$player2)
WB02_SFft2 <- as.matrix(WB02_SFft)
numRows <- nrow(WB02_SFft2)
numCols <- ncol(WB02_SFft2)
WB02_SFft3 <- WB02_SFft2[c(2:numRows) , c(2:numCols)]
WB02_SFTable <- graph.adjacency(WB02_SFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(WB02_SFTable, vertex.label = V(WB02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
WB02_SF.clusterCoef <- transitivity(WB02_SFTable, type="global") #cluster coefficient
WB02_SF.degreeCent <- centralization.degree(WB02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_SFftn <- as.network.matrix(WB02_SFft)
WB02_SF.netDensity <- network.density(WB02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_SF.entropy <- entropy(WB02_SFft) #entropy

WB02_SF.netMx <- cbind(WB02_SF.netMx, WB02_SF.clusterCoef, WB02_SF.degreeCent$centralization,
                       WB02_SF.netDensity, WB02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************
#NA

round = 2
teamName = "WB"
KIoutcome = "Turnover_F"
WB02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
WB02_TFg2 <- data.frame(WB02_TF)
WB02_TFg2 <- WB02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_TFg2$player1
player2vector <- WB02_TFg2$player2
WB02_TFg3 <- WB02_TFg2
WB02_TFg3$p1inp2vec <- is.element(WB02_TFg3$player1, player2vector)
WB02_TFg3$p2inp1vec <- is.element(WB02_TFg3$player2, player1vector)

addPlayer1 <- WB02_TFg3[ which(WB02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_TFg3[ which(WB02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_TFg2 <- rbind(WB02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
WB02_TFft <- ftable(WB02_TFg2$player1, WB02_TFg2$player2)
WB02_TFft2 <- as.matrix(WB02_TFft)
numRows <- nrow(WB02_TFft2)
numCols <- ncol(WB02_TFft2)
WB02_TFft3 <- WB02_TFft2[c(2:numRows) , c(2:numCols)]
WB02_TFTable <- graph.adjacency(WB02_TFft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(WB02_TFTable, vertex.label = V(WB02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
WB02_TF.clusterCoef <- transitivity(WB02_TFTable, type="global") #cluster coefficient
WB02_TF.degreeCent <- centralization.degree(WB02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_TFftn <- as.network.matrix(WB02_TFft)
WB02_TF.netDensity <- network.density(WB02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_TF.entropy <- entropy(WB02_TFft) #entropy

WB02_TF.netMx <- cbind(WB02_TF.netMx, WB02_TF.clusterCoef, WB02_TF.degreeCent$centralization,
                       WB02_TF.netDensity, WB02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************

round = 2
teamName = "WB"
KIoutcome = "Stoppage_AM"
WB02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
WB02_SAMg2 <- data.frame(WB02_SAM)
WB02_SAMg2 <- WB02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_SAMg2$player1
player2vector <- WB02_SAMg2$player2
WB02_SAMg3 <- WB02_SAMg2
WB02_SAMg3$p1inp2vec <- is.element(WB02_SAMg3$player1, player2vector)
WB02_SAMg3$p2inp1vec <- is.element(WB02_SAMg3$player2, player1vector)

addPlayer1 <- WB02_SAMg3[ which(WB02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_SAMg3[ which(WB02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_SAMg2 <- rbind(WB02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
WB02_SAMft <- ftable(WB02_SAMg2$player1, WB02_SAMg2$player2)
WB02_SAMft2 <- as.matrix(WB02_SAMft)
numRows <- nrow(WB02_SAMft2)
numCols <- ncol(WB02_SAMft2)
WB02_SAMft3 <- WB02_SAMft2[c(2:numRows) , c(2:numCols)]
WB02_SAMTable <- graph.adjacency(WB02_SAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(WB02_SAMTable, vertex.label = V(WB02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
WB02_SAM.clusterCoef <- transitivity(WB02_SAMTable, type="global") #cluster coefficient
WB02_SAM.degreeCent <- centralization.degree(WB02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_SAMftn <- as.network.matrix(WB02_SAMft)
WB02_SAM.netDensity <- network.density(WB02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_SAM.entropy <- entropy(WB02_SAMft) #entropy

WB02_SAM.netMx <- cbind(WB02_SAM.netMx, WB02_SAM.clusterCoef, WB02_SAM.degreeCent$centralization,
                        WB02_SAM.netDensity, WB02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************

round = 2
teamName = "WB"
KIoutcome = "Turnover_AM"
WB02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
WB02_TAMg2 <- data.frame(WB02_TAM)
WB02_TAMg2 <- WB02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_TAMg2$player1
player2vector <- WB02_TAMg2$player2
WB02_TAMg3 <- WB02_TAMg2
WB02_TAMg3$p1inp2vec <- is.element(WB02_TAMg3$player1, player2vector)
WB02_TAMg3$p2inp1vec <- is.element(WB02_TAMg3$player2, player1vector)

addPlayer1 <- WB02_TAMg3[ which(WB02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_TAMg3[ which(WB02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_TAMg2 <- rbind(WB02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
WB02_TAMft <- ftable(WB02_TAMg2$player1, WB02_TAMg2$player2)
WB02_TAMft2 <- as.matrix(WB02_TAMft)
numRows <- nrow(WB02_TAMft2)
numCols <- ncol(WB02_TAMft2)
WB02_TAMft3 <- WB02_TAMft2[c(2:numRows) , c(2:numCols)]
WB02_TAMTable <- graph.adjacency(WB02_TAMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(WB02_TAMTable, vertex.label = V(WB02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
WB02_TAM.clusterCoef <- transitivity(WB02_TAMTable, type="global") #cluster coefficient
WB02_TAM.degreeCent <- centralization.degree(WB02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_TAMftn <- as.network.matrix(WB02_TAMft)
WB02_TAM.netDensity <- network.density(WB02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_TAM.entropy <- entropy(WB02_TAMft) #entropy

WB02_TAM.netMx <- cbind(WB02_TAM.netMx, WB02_TAM.clusterCoef, WB02_TAM.degreeCent$centralization,
                        WB02_TAM.netDensity, WB02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************
#NA

round = 2
teamName = "WB"
KIoutcome = "Stoppage_DM"
WB02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
WB02_SDMg2 <- data.frame(WB02_SDM)
WB02_SDMg2 <- WB02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_SDMg2$player1
player2vector <- WB02_SDMg2$player2
WB02_SDMg3 <- WB02_SDMg2
WB02_SDMg3$p1inp2vec <- is.element(WB02_SDMg3$player1, player2vector)
WB02_SDMg3$p2inp1vec <- is.element(WB02_SDMg3$player2, player1vector)

addPlayer1 <- WB02_SDMg3[ which(WB02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_SDMg3[ which(WB02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_SDMg2 <- rbind(WB02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
WB02_SDMft <- ftable(WB02_SDMg2$player1, WB02_SDMg2$player2)
WB02_SDMft2 <- as.matrix(WB02_SDMft)
numRows <- nrow(WB02_SDMft2)
numCols <- ncol(WB02_SDMft2)
WB02_SDMft3 <- WB02_SDMft2[c(2:numRows) , c(2:numCols)]
WB02_SDMTable <- graph.adjacency(WB02_SDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(WB02_SDMTable, vertex.label = V(WB02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
WB02_SDM.clusterCoef <- transitivity(WB02_SDMTable, type="global") #cluster coefficient
WB02_SDM.degreeCent <- centralization.degree(WB02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_SDMftn <- as.network.matrix(WB02_SDMft)
WB02_SDM.netDensity <- network.density(WB02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_SDM.entropy <- entropy(WB02_SDMft) #entropy

WB02_SDM.netMx <- cbind(WB02_SDM.netMx, WB02_SDM.clusterCoef, WB02_SDM.degreeCent$centralization,
                        WB02_SDM.netDensity, WB02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "WB"
KIoutcome = "Turnover_DM"
WB02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
WB02_TDMg2 <- data.frame(WB02_TDM)
WB02_TDMg2 <- WB02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_TDMg2$player1
player2vector <- WB02_TDMg2$player2
WB02_TDMg3 <- WB02_TDMg2
WB02_TDMg3$p1inp2vec <- is.element(WB02_TDMg3$player1, player2vector)
WB02_TDMg3$p2inp1vec <- is.element(WB02_TDMg3$player2, player1vector)

addPlayer1 <- WB02_TDMg3[ which(WB02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_TDMg3[ which(WB02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_TDMg2 <- rbind(WB02_TDMg2, addPlayers)


#Round 2, DM Turnover graph using weighted edges
WB02_TDMft <- ftable(WB02_TDMg2$player1, WB02_TDMg2$player2)
WB02_TDMft2 <- as.matrix(WB02_TDMft)
numRows <- nrow(WB02_TDMft2)
numCols <- ncol(WB02_TDMft2)
WB02_TDMft3 <- WB02_TDMft2[c(2:numRows) , c(2:numCols)]
WB02_TDMTable <- graph.adjacency(WB02_TDMft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(WB02_TDMTable, vertex.label = V(WB02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
WB02_TDM.clusterCoef <- transitivity(WB02_TDMTable, type="global") #cluster coefficient
WB02_TDM.degreeCent <- centralization.degree(WB02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_TDMftn <- as.network.matrix(WB02_TDMft)
WB02_TDM.netDensity <- network.density(WB02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_TDM.entropy <- entropy(WB02_TDMft) #entropy

WB02_TDM.netMx <- cbind(WB02_TDM.netMx, WB02_TDM.clusterCoef, WB02_TDM.degreeCent$centralization,
                        WB02_TDM.netDensity, WB02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "WB"
KIoutcome = "Stoppage_D"
WB02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
WB02_SDg2 <- data.frame(WB02_SD)
WB02_SDg2 <- WB02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_SDg2$player1
player2vector <- WB02_SDg2$player2
WB02_SDg3 <- WB02_SDg2
WB02_SDg3$p1inp2vec <- is.element(WB02_SDg3$player1, player2vector)
WB02_SDg3$p2inp1vec <- is.element(WB02_SDg3$player2, player1vector)

addPlayer1 <- WB02_SDg3[ which(WB02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_SDg3[ which(WB02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_SDg2 <- rbind(WB02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
WB02_SDft <- ftable(WB02_SDg2$player1, WB02_SDg2$player2)
WB02_SDft2 <- as.matrix(WB02_SDft)
numRows <- nrow(WB02_SDft2)
numCols <- ncol(WB02_SDft2)
WB02_SDft3 <- WB02_SDft2[c(2:numRows) , c(2:numCols)]
WB02_SDTable <- graph.adjacency(WB02_SDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(WB02_SDTable, vertex.label = V(WB02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
WB02_SD.clusterCoef <- transitivity(WB02_SDTable, type="global") #cluster coefficient
WB02_SD.degreeCent <- centralization.degree(WB02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_SDftn <- as.network.matrix(WB02_SDft)
WB02_SD.netDensity <- network.density(WB02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_SD.entropy <- entropy(WB02_SDft) #entropy

WB02_SD.netMx <- cbind(WB02_SD.netMx, WB02_SD.clusterCoef, WB02_SD.degreeCent$centralization,
                       WB02_SD.netDensity, WB02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************

round = 2
teamName = "WB"
KIoutcome = "Turnover_D"
WB02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
WB02_TDg2 <- data.frame(WB02_TD)
WB02_TDg2 <- WB02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_TDg2$player1
player2vector <- WB02_TDg2$player2
WB02_TDg3 <- WB02_TDg2
WB02_TDg3$p1inp2vec <- is.element(WB02_TDg3$player1, player2vector)
WB02_TDg3$p2inp1vec <- is.element(WB02_TDg3$player2, player1vector)

addPlayer1 <- WB02_TDg3[ which(WB02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_TDg3[ which(WB02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_TDg2 <- rbind(WB02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
WB02_TDft <- ftable(WB02_TDg2$player1, WB02_TDg2$player2)
WB02_TDft2 <- as.matrix(WB02_TDft)
numRows <- nrow(WB02_TDft2)
numCols <- ncol(WB02_TDft2)
WB02_TDft3 <- WB02_TDft2[c(2:numRows) , c(2:numCols)]
WB02_TDTable <- graph.adjacency(WB02_TDft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(WB02_TDTable, vertex.label = V(WB02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
WB02_TD.clusterCoef <- transitivity(WB02_TDTable, type="global") #cluster coefficient
WB02_TD.degreeCent <- centralization.degree(WB02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_TDftn <- as.network.matrix(WB02_TDft)
WB02_TD.netDensity <- network.density(WB02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_TD.entropy <- entropy(WB02_TDft) #entropy

WB02_TD.netMx <- cbind(WB02_TD.netMx, WB02_TD.clusterCoef, WB02_TD.degreeCent$centralization,
                       WB02_TD.netDensity, WB02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "WB"
KIoutcome = "End of Qtr_DM"
WB02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
WB02_QTg2 <- data.frame(WB02_QT)
WB02_QTg2 <- WB02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WB02_QTg2$player1
player2vector <- WB02_QTg2$player2
WB02_QTg3 <- WB02_QTg2
WB02_QTg3$p1inp2vec <- is.element(WB02_QTg3$player1, player2vector)
WB02_QTg3$p2inp1vec <- is.element(WB02_QTg3$player2, player1vector)

addPlayer1 <- WB02_QTg3[ which(WB02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WB02_QTg3[ which(WB02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WB02_QTg2 <- rbind(WB02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
WB02_QTft <- ftable(WB02_QTg2$player1, WB02_QTg2$player2)
WB02_QTft2 <- as.matrix(WB02_QTft)
numRows <- nrow(WB02_QTft2)
numCols <- ncol(WB02_QTft2)
WB02_QTft3 <- WB02_QTft2[c(2:numRows) , c(2:numCols)]
WB02_QTTable <- graph.adjacency(WB02_QTft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(WB02_QTTable, vertex.label = V(WB02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WB02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
WB02_QT.clusterCoef <- transitivity(WB02_QTTable, type="global") #cluster coefficient
WB02_QT.degreeCent <- centralization.degree(WB02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WB02_QTftn <- as.network.matrix(WB02_QTft)
WB02_QT.netDensity <- network.density(WB02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WB02_QT.entropy <- entropy(WB02_QTft) #entropy

WB02_QT.netMx <- cbind(WB02_QT.netMx, WB02_QT.clusterCoef, WB02_QT.degreeCent$centralization,
                       WB02_QT.netDensity, WB02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WB02_QT.netMx) <- varnames

#############################################################################
#WEST COAST EAGLES

##
#ROUND 2
##

#Round 2, Goal***************************************************************

round = 2
teamName = "WCE"
KIoutcome = "Goal_F"
WCE02_G.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Goal with weighted edges
WCE02_Gg2 <- data.frame(WCE02_G)
WCE02_Gg2 <- WCE02_Gg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_Gg2$player1
player2vector <- WCE02_Gg2$player2
WCE02_Gg3 <- WCE02_Gg2
WCE02_Gg3$p1inp2vec <- is.element(WCE02_Gg3$player1, player2vector)
WCE02_Gg3$p2inp1vec <- is.element(WCE02_Gg3$player2, player1vector)

addPlayer1 <- WCE02_Gg3[ which(WCE02_Gg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_Gg3[ which(WCE02_Gg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_Gg2 <- rbind(WCE02_Gg2, addPlayers)

#Round 2, Goal graph using weighted edges
WCE02_Gft <- ftable(WCE02_Gg2$player1, WCE02_Gg2$player2)
WCE02_Gft2 <- as.matrix(WCE02_Gft)
numRows <- nrow(WCE02_Gft2)
numCols <- ncol(WCE02_Gft2)
WCE02_Gft3 <- WCE02_Gft2[c(2:numRows) , c(2:numCols)]
WCE02_GTable <- graph.adjacency(WCE02_Gft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Goal graph=weighted
plot.igraph(WCE02_GTable, vertex.label = V(WCE02_GTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_GTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Goal calulation of network metrics
#igraph
WCE02_G.clusterCoef <- transitivity(WCE02_GTable, type="global") #cluster coefficient
WCE02_G.degreeCent <- centralization.degree(WCE02_GTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_Gftn <- as.network.matrix(WCE02_Gft)
WCE02_G.netDensity <- network.density(WCE02_Gftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_G.entropy <- entropy(WCE02_Gft) #entropy

WCE02_G.netMx <- cbind(WCE02_G.netMx, WCE02_G.clusterCoef, WCE02_G.degreeCent$centralization,
                       WCE02_G.netDensity, WCE02_G.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_G.netMx) <- varnames

#Round 2, Behind***************************************************************
#NA

round = 2
teamName = "WCE"
KIoutcome = "Behind_F"
WCE02_B.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, Behind with weighted edges
WCE02_Bg2 <- data.frame(WCE02_B)
WCE02_Bg2 <- WCE02_Bg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_Bg2$player1
player2vector <- WCE02_Bg2$player2
WCE02_Bg3 <- WCE02_Bg2
WCE02_Bg3$p1inp2vec <- is.element(WCE02_Bg3$player1, player2vector)
WCE02_Bg3$p2inp1vec <- is.element(WCE02_Bg3$player2, player1vector)

addPlayer1 <- WCE02_Bg3[ which(WCE02_Bg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_Bg3[ which(WCE02_Bg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_Bg2 <- rbind(WCE02_Bg2, addPlayers)

#Round 2, Behind graph using weighted edges
WCE02_Bft <- ftable(WCE02_Bg2$player1, WCE02_Bg2$player2)
WCE02_Bft2 <- as.matrix(WCE02_Bft)
numRows <- nrow(WCE02_Bft2)
numCols <- ncol(WCE02_Bft2)
WCE02_Bft3 <- WCE02_Bft2[c(2:numRows) , c(2:numCols)]
WCE02_BTable <- graph.adjacency(WCE02_Bft3, mode="directed", weighted = T, diag = F,
                                add.colnames = NULL)

#Round 2, Behind graph=weighted
plot.igraph(WCE02_BTable, vertex.label = V(WCE02_BTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_BTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, Behind calulation of network metrics
#igraph
WCE02_B.clusterCoef <- transitivity(WCE02_BTable, type="global") #cluster coefficient
WCE02_B.degreeCent <- centralization.degree(WCE02_BTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_Bftn <- as.network.matrix(WCE02_Bft)
WCE02_B.netDensity <- network.density(WCE02_Bftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_B.entropy <- entropy(WCE02_Bft) #entropy

WCE02_B.netMx <- cbind(WCE02_B.netMx, WCE02_B.clusterCoef, WCE02_B.degreeCent$centralization,
                       WCE02_B.netDensity, WCE02_B.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_B.netMx) <- varnames

#Round 2, FWD Stoppage**********************************************************

round = 2
teamName = "WCE"
KIoutcome = "Stoppage_F"
WCE02_SF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Stoppage with weighted edges
WCE02_SFg2 <- data.frame(WCE02_SF)
WCE02_SFg2 <- WCE02_SFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_SFg2$player1
player2vector <- WCE02_SFg2$player2
WCE02_SFg3 <- WCE02_SFg2
WCE02_SFg3$p1inp2vec <- is.element(WCE02_SFg3$player1, player2vector)
WCE02_SFg3$p2inp1vec <- is.element(WCE02_SFg3$player2, player1vector)

addPlayer1 <- WCE02_SFg3[ which(WCE02_SFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_SFg3[ which(WCE02_SFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_SFg2 <- rbind(WCE02_SFg2, addPlayers)

#Round 2, FWD Stoppage graph using weighted edges
WCE02_SFft <- ftable(WCE02_SFg2$player1, WCE02_SFg2$player2)
WCE02_SFft2 <- as.matrix(WCE02_SFft)
numRows <- nrow(WCE02_SFft2)
numCols <- ncol(WCE02_SFft2)
WCE02_SFft3 <- WCE02_SFft2[c(2:numRows) , c(2:numCols)]
WCE02_SFTable <- graph.adjacency(WCE02_SFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Stoppage graph=weighted
plot.igraph(WCE02_SFTable, vertex.label = V(WCE02_SFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_SFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Stoppage calulation of network metrics
#igraph
WCE02_SF.clusterCoef <- transitivity(WCE02_SFTable, type="global") #cluster coefficient
WCE02_SF.degreeCent <- centralization.degree(WCE02_SFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_SFftn <- as.network.matrix(WCE02_SFft)
WCE02_SF.netDensity <- network.density(WCE02_SFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_SF.entropy <- entropy(WCE02_SFft) #entropy

WCE02_SF.netMx <- cbind(WCE02_SF.netMx, WCE02_SF.clusterCoef, WCE02_SF.degreeCent$centralization,
                        WCE02_SF.netDensity, WCE02_SF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_SF.netMx) <- varnames

#Round 2, FWD Turnover**********************************************************
#NA

round = 2
teamName = "WCE"
KIoutcome = "Turnover_F"
WCE02_TF.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, FWD Turnover with weighted edges
WCE02_TFg2 <- data.frame(WCE02_TF)
WCE02_TFg2 <- WCE02_TFg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_TFg2$player1
player2vector <- WCE02_TFg2$player2
WCE02_TFg3 <- WCE02_TFg2
WCE02_TFg3$p1inp2vec <- is.element(WCE02_TFg3$player1, player2vector)
WCE02_TFg3$p2inp1vec <- is.element(WCE02_TFg3$player2, player1vector)

addPlayer1 <- WCE02_TFg3[ which(WCE02_TFg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_TFg3[ which(WCE02_TFg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_TFg2 <- rbind(WCE02_TFg2, addPlayers)

#Round 2, FWD Turnover graph using weighted edges
WCE02_TFft <- ftable(WCE02_TFg2$player1, WCE02_TFg2$player2)
WCE02_TFft2 <- as.matrix(WCE02_TFft)
numRows <- nrow(WCE02_TFft2)
numCols <- ncol(WCE02_TFft2)
WCE02_TFft3 <- WCE02_TFft2[c(2:numRows) , c(2:numCols)]
WCE02_TFTable <- graph.adjacency(WCE02_TFft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, FWD Turnover graph=weighted
plot.igraph(WCE02_TFTable, vertex.label = V(WCE02_TFTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_TFTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, FWD Turnover calulation of network metrics
#igraph
WCE02_TF.clusterCoef <- transitivity(WCE02_TFTable, type="global") #cluster coefficient
WCE02_TF.degreeCent <- centralization.degree(WCE02_TFTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_TFftn <- as.network.matrix(WCE02_TFft)
WCE02_TF.netDensity <- network.density(WCE02_TFftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_TF.entropy <- entropy(WCE02_TFft) #entropy

WCE02_TF.netMx <- cbind(WCE02_TF.netMx, WCE02_TF.clusterCoef, WCE02_TF.degreeCent$centralization,
                        WCE02_TF.netDensity, WCE02_TF.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_TF.netMx) <- varnames

#Round 2, AM Stoppage**********************************************************
#NA

round = 2
teamName = "WCE"
KIoutcome = "Stoppage_AM"
WCE02_SAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Stoppage with weighted edges
WCE02_SAMg2 <- data.frame(WCE02_SAM)
WCE02_SAMg2 <- WCE02_SAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_SAMg2$player1
player2vector <- WCE02_SAMg2$player2
WCE02_SAMg3 <- WCE02_SAMg2
WCE02_SAMg3$p1inp2vec <- is.element(WCE02_SAMg3$player1, player2vector)
WCE02_SAMg3$p2inp1vec <- is.element(WCE02_SAMg3$player2, player1vector)

addPlayer1 <- WCE02_SAMg3[ which(WCE02_SAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_SAMg3[ which(WCE02_SAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_SAMg2 <- rbind(WCE02_SAMg2, addPlayers)

#Round 2, AM Stoppage graph using weighted edges
WCE02_SAMft <- ftable(WCE02_SAMg2$player1, WCE02_SAMg2$player2)
WCE02_SAMft2 <- as.matrix(WCE02_SAMft)
numRows <- nrow(WCE02_SAMft2)
numCols <- ncol(WCE02_SAMft2)
WCE02_SAMft3 <- WCE02_SAMft2[c(2:numRows) , c(2:numCols)]
WCE02_SAMTable <- graph.adjacency(WCE02_SAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Stoppage graph=weighted
plot.igraph(WCE02_SAMTable, vertex.label = V(WCE02_SAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_SAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Stoppage calulation of network metrics
#igraph
WCE02_SAM.clusterCoef <- transitivity(WCE02_SAMTable, type="global") #cluster coefficient
WCE02_SAM.degreeCent <- centralization.degree(WCE02_SAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_SAMftn <- as.network.matrix(WCE02_SAMft)
WCE02_SAM.netDensity <- network.density(WCE02_SAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_SAM.entropy <- entropy(WCE02_SAMft) #entropy

WCE02_SAM.netMx <- cbind(WCE02_SAM.netMx, WCE02_SAM.clusterCoef, WCE02_SAM.degreeCent$centralization,
                         WCE02_SAM.netDensity, WCE02_SAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_SAM.netMx) <- varnames

#Round 2, AM Turnover**********************************************************
#NA

round = 2
teamName = "WCE"
KIoutcome = "Turnover_AM"
WCE02_TAM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, AM Turnover with weighted edges
WCE02_TAMg2 <- data.frame(WCE02_TAM)
WCE02_TAMg2 <- WCE02_TAMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_TAMg2$player1
player2vector <- WCE02_TAMg2$player2
WCE02_TAMg3 <- WCE02_TAMg2
WCE02_TAMg3$p1inp2vec <- is.element(WCE02_TAMg3$player1, player2vector)
WCE02_TAMg3$p2inp1vec <- is.element(WCE02_TAMg3$player2, player1vector)

addPlayer1 <- WCE02_TAMg3[ which(WCE02_TAMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_TAMg3[ which(WCE02_TAMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_TAMg2 <- rbind(WCE02_TAMg2, addPlayers)

#Round 2, AM Turnover graph using weighted edges
WCE02_TAMft <- ftable(WCE02_TAMg2$player1, WCE02_TAMg2$player2)
WCE02_TAMft2 <- as.matrix(WCE02_TAMft)
numRows <- nrow(WCE02_TAMft2)
numCols <- ncol(WCE02_TAMft2)
WCE02_TAMft3 <- WCE02_TAMft2[c(2:numRows) , c(2:numCols)]
WCE02_TAMTable <- graph.adjacency(WCE02_TAMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, AM Turnover graph=weighted
plot.igraph(WCE02_TAMTable, vertex.label = V(WCE02_TAMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_TAMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, AM Turnover calulation of network metrics
#igraph
WCE02_TAM.clusterCoef <- transitivity(WCE02_TAMTable, type="global") #cluster coefficient
WCE02_TAM.degreeCent <- centralization.degree(WCE02_TAMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_TAMftn <- as.network.matrix(WCE02_TAMft)
WCE02_TAM.netDensity <- network.density(WCE02_TAMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_TAM.entropy <- entropy(WCE02_TAMft) #entropy

WCE02_TAM.netMx <- cbind(WCE02_TAM.netMx, WCE02_TAM.clusterCoef, WCE02_TAM.degreeCent$centralization,
                         WCE02_TAM.netDensity, WCE02_TAM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_TAM.netMx) <- varnames

#Round 2, DM Stoppage**********************************************************
#NA

round = 2
teamName = "WCE"
KIoutcome = "Stoppage_DM"
WCE02_SDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Stoppage with weighted edges
WCE02_SDMg2 <- data.frame(WCE02_SDM)
WCE02_SDMg2 <- WCE02_SDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_SDMg2$player1
player2vector <- WCE02_SDMg2$player2
WCE02_SDMg3 <- WCE02_SDMg2
WCE02_SDMg3$p1inp2vec <- is.element(WCE02_SDMg3$player1, player2vector)
WCE02_SDMg3$p2inp1vec <- is.element(WCE02_SDMg3$player2, player1vector)

addPlayer1 <- WCE02_SDMg3[ which(WCE02_SDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_SDMg3[ which(WCE02_SDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_SDMg2 <- rbind(WCE02_SDMg2, addPlayers)

#Round 2, DM Stoppage graph using weighted edges
WCE02_SDMft <- ftable(WCE02_SDMg2$player1, WCE02_SDMg2$player2)
WCE02_SDMft2 <- as.matrix(WCE02_SDMft)
numRows <- nrow(WCE02_SDMft2)
numCols <- ncol(WCE02_SDMft2)
WCE02_SDMft3 <- WCE02_SDMft2[c(2:numRows) , c(2:numCols)]
WCE02_SDMTable <- graph.adjacency(WCE02_SDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Stoppage graph=weighted
plot.igraph(WCE02_SDMTable, vertex.label = V(WCE02_SDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_SDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Stoppage calulation of network metrics
#igraph
WCE02_SDM.clusterCoef <- transitivity(WCE02_SDMTable, type="global") #cluster coefficient
WCE02_SDM.degreeCent <- centralization.degree(WCE02_SDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_SDMftn <- as.network.matrix(WCE02_SDMft)
WCE02_SDM.netDensity <- network.density(WCE02_SDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_SDM.entropy <- entropy(WCE02_SDMft) #entropy

WCE02_SDM.netMx <- cbind(WCE02_SDM.netMx, WCE02_SDM.clusterCoef, WCE02_SDM.degreeCent$centralization,
                         WCE02_SDM.netDensity, WCE02_SDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_SDM.netMx) <- varnames

#Round 2, DM Turnover**********************************************************

round = 2
teamName = "WCE"
KIoutcome = "Turnover_DM"
WCE02_TDM.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, DM Turnover with weighted edges
WCE02_TDMg2 <- data.frame(WCE02_TDM)
WCE02_TDMg2 <- WCE02_TDMg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_TDMg2$player1
player2vector <- WCE02_TDMg2$player2
WCE02_TDMg3 <- WCE02_TDMg2
WCE02_TDMg3$p1inp2vec <- is.element(WCE02_TDMg3$player1, player2vector)
WCE02_TDMg3$p2inp1vec <- is.element(WCE02_TDMg3$player2, player1vector)

addPlayer1 <- WCE02_TDMg3[ which(WCE02_TDMg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_TDMg3[ which(WCE02_TDMg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_TDMg2 <- rbind(WCE02_TDMg2, addPlayers)

#Round 2, DM Turnover graph using weighted edges
WCE02_TDMft <- ftable(WCE02_TDMg2$player1, WCE02_TDMg2$player2)
WCE02_TDMft2 <- as.matrix(WCE02_TDMft)
numRows <- nrow(WCE02_TDMft2)
numCols <- ncol(WCE02_TDMft2)
WCE02_TDMft3 <- WCE02_TDMft2[c(2:numRows) , c(2:numCols)]
WCE02_TDMTable <- graph.adjacency(WCE02_TDMft3, mode="directed", weighted = T, diag = F,
                                  add.colnames = NULL)

#Round 2, DM Turnover graph=weighted
plot.igraph(WCE02_TDMTable, vertex.label = V(WCE02_TDMTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_TDMTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, DM Turnover calulation of network metrics
#igraph
WCE02_TDM.clusterCoef <- transitivity(WCE02_TDMTable, type="global") #cluster coefficient
WCE02_TDM.degreeCent <- centralization.degree(WCE02_TDMTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_TDMftn <- as.network.matrix(WCE02_TDMft)
WCE02_TDM.netDensity <- network.density(WCE02_TDMftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_TDM.entropy <- entropy(WCE02_TDMft) #entropy

WCE02_TDM.netMx <- cbind(WCE02_TDM.netMx, WCE02_TDM.clusterCoef, WCE02_TDM.degreeCent$centralization,
                         WCE02_TDM.netDensity, WCE02_TDM.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_TDM.netMx) <- varnames

#Round 2, D Stoppage**********************************************************
#NA

round = 2
teamName = "WCE"
KIoutcome = "Stoppage_D"
WCE02_SD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Stoppage with weighted edges
WCE02_SDg2 <- data.frame(WCE02_SD)
WCE02_SDg2 <- WCE02_SDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_SDg2$player1
player2vector <- WCE02_SDg2$player2
WCE02_SDg3 <- WCE02_SDg2
WCE02_SDg3$p1inp2vec <- is.element(WCE02_SDg3$player1, player2vector)
WCE02_SDg3$p2inp1vec <- is.element(WCE02_SDg3$player2, player1vector)

addPlayer1 <- WCE02_SDg3[ which(WCE02_SDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_SDg3[ which(WCE02_SDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_SDg2 <- rbind(WCE02_SDg2, addPlayers)

#Round 2, D Stoppage graph using weighted edges
WCE02_SDft <- ftable(WCE02_SDg2$player1, WCE02_SDg2$player2)
WCE02_SDft2 <- as.matrix(WCE02_SDft)
numRows <- nrow(WCE02_SDft2)
numCols <- ncol(WCE02_SDft2)
WCE02_SDft3 <- WCE02_SDft2[c(2:numRows) , c(2:numCols)]
WCE02_SDTable <- graph.adjacency(WCE02_SDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Stoppage graph=weighted
plot.igraph(WCE02_SDTable, vertex.label = V(WCE02_SDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_SDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Stoppage calulation of network metrics
#igraph
WCE02_SD.clusterCoef <- transitivity(WCE02_SDTable, type="global") #cluster coefficient
WCE02_SD.degreeCent <- centralization.degree(WCE02_SDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_SDftn <- as.network.matrix(WCE02_SDft)
WCE02_SD.netDensity <- network.density(WCE02_SDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_SD.entropy <- entropy(WCE02_SDft) #entropy

WCE02_SD.netMx <- cbind(WCE02_SD.netMx, WCE02_SD.clusterCoef, WCE02_SD.degreeCent$centralization,
                        WCE02_SD.netDensity, WCE02_SD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_SD.netMx) <- varnames

#Round 2, D Turnover**********************************************************
#NA

round = 2
teamName = "WCE"
KIoutcome = "Turnover_D"
WCE02_TD.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, D Turnover with weighted edges
WCE02_TDg2 <- data.frame(WCE02_TD)
WCE02_TDg2 <- WCE02_TDg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_TDg2$player1
player2vector <- WCE02_TDg2$player2
WCE02_TDg3 <- WCE02_TDg2
WCE02_TDg3$p1inp2vec <- is.element(WCE02_TDg3$player1, player2vector)
WCE02_TDg3$p2inp1vec <- is.element(WCE02_TDg3$player2, player1vector)

addPlayer1 <- WCE02_TDg3[ which(WCE02_TDg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_TDg3[ which(WCE02_TDg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_TDg2 <- rbind(WCE02_TDg2, addPlayers)

#Round 2, D Turnover graph using weighted edges
WCE02_TDft <- ftable(WCE02_TDg2$player1, WCE02_TDg2$player2)
WCE02_TDft2 <- as.matrix(WCE02_TDft)
numRows <- nrow(WCE02_TDft2)
numCols <- ncol(WCE02_TDft2)
WCE02_TDft3 <- WCE02_TDft2[c(2:numRows) , c(2:numCols)]
WCE02_TDTable <- graph.adjacency(WCE02_TDft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, D Turnover graph=weighted
plot.igraph(WCE02_TDTable, vertex.label = V(WCE02_TDTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_TDTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, D Turnover calulation of network metrics
#igraph
WCE02_TD.clusterCoef <- transitivity(WCE02_TDTable, type="global") #cluster coefficient
WCE02_TD.degreeCent <- centralization.degree(WCE02_TDTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_TDftn <- as.network.matrix(WCE02_TDft)
WCE02_TD.netDensity <- network.density(WCE02_TDftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_TD.entropy <- entropy(WCE02_TDft) #entropy

WCE02_TD.netMx <- cbind(WCE02_TD.netMx, WCE02_TD.clusterCoef, WCE02_TD.degreeCent$centralization,
                        WCE02_TD.netDensity, WCE02_TD.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_TD.netMx) <- varnames

#Round 2, End of Qtr**********************************************************
#NA

round = 2
teamName = "WCE"
KIoutcome = "End of Qtr_DM"
WCE02_QT.netMx <- cbind(round, teamName, KIoutcome)

#Round 2, End of Qtr with weighted edges
WCE02_QTg2 <- data.frame(WCE02_QT)
WCE02_QTg2 <- WCE02_QTg2[,c(4:6)]

# Identify players who only disposed but did not receive OR only received but did not dispose of the ball
# These players need to be added into the data frame without a 'partner' in order to create a square matrix
player1vector <- WCE02_QTg2$player1
player2vector <- WCE02_QTg2$player2
WCE02_QTg3 <- WCE02_QTg2
WCE02_QTg3$p1inp2vec <- is.element(WCE02_QTg3$player1, player2vector)
WCE02_QTg3$p2inp1vec <- is.element(WCE02_QTg3$player2, player1vector)

addPlayer1 <- WCE02_QTg3[ which(WCE02_QTg3$p1inp2vec == FALSE), 1]
empty <- ""
zero <- 0
addPlayer1 <- cbind(empty, addPlayer1, zero)
addPlayer2 <- WCE02_QTg3[ which(WCE02_QTg3$p2inp1vec == FALSE), 2]
addPlayer2 <- cbind(addPlayer2, empty, zero)
addPlayers <- rbind(addPlayer1, addPlayer2)
varnames <- c("player1", "player2", "weight")
colnames(addPlayers) <- varnames

WCE02_QTg2 <- rbind(WCE02_QTg2, addPlayers)

#Round 2, End of Qtr graph using weighted edges
WCE02_QTft <- ftable(WCE02_QTg2$player1, WCE02_QTg2$player2)
WCE02_QTft2 <- as.matrix(WCE02_QTft)
numRows <- nrow(WCE02_QTft2)
numCols <- ncol(WCE02_QTft2)
WCE02_QTft3 <- WCE02_QTft2[c(2:numRows) , c(2:numCols)]
WCE02_QTTable <- graph.adjacency(WCE02_QTft3, mode="directed", weighted = T, diag = F,
                                 add.colnames = NULL)

#Round 2, End of Qtr graph=weighted
plot.igraph(WCE02_QTTable, vertex.label = V(WCE02_QTTable)$names, 
            layout = layout.fruchterman.reingold,
            edge.color="black", edge.width = E(WCE02_QTTable)$weight, edge.arrow.size=0.15,vertex.label.cex=0.4)

#Round 2, End of Qtr calulation of network metrics
#igraph
WCE02_QT.clusterCoef <- transitivity(WCE02_QTTable, type="global") #cluster coefficient
WCE02_QT.degreeCent <- centralization.degree(WCE02_QTTable, mode=c("all"), normalized = TRUE) #degree centrality
#network-create a nework object
WCE02_QTftn <- as.network.matrix(WCE02_QTft)
WCE02_QT.netDensity <- network.density(WCE02_QTftn, na.omit=TRUE, discount.bipartite = FALSE) #density
#QuACN- take the original ftable
WCE02_QT.entropy <- entropy(WCE02_QTft) #entropy

WCE02_QT.netMx <- cbind(WCE02_QT.netMx, WCE02_QT.clusterCoef, WCE02_QT.degreeCent$centralization,
                        WCE02_QT.netDensity, WCE02_QT.entropy)
varnames <- c("round", "team", "kickInOutcome", "clusterCoef", "degreeCent", "netDensity", "entropy")
colnames(WCE02_QT.netMx) <- varnames
