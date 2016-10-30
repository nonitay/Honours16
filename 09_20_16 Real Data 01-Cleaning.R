#####
#09-20-16- Real data 01
#Cleaning
####

##
# 1.Setup
##

# Install and Load Packages
library(readr)
library(plyr)

# Read data into R
md <- read_csv("2015 chain data.csv", col_names = TRUE, col_types = NULL, na = "")
# View(md)

# Inital check of data strucute
# length(md$Round)
# str(md) #structure
# class(md$Match) #data class
# class(md$Round)
# head(md) #first part of data 

# Check data values
# min(md$Round)
# max(md$Round)
# mean(md$Round)
# sd(md$Distance)
# median(md$Disp.X.End)

##
# 2.Data Cleaning
##

#Delete any data past rd 23
md <- md[c(1:142941),]

# Rename columns
varnames <- c("ID", "round", "match", "opposition", "chainNum", "team", "chainStartType",
              "chainStartZone", "chainEndType", "chainEndZone", "player", "disposal", "dispXStart",
              "dispYStart", "dispXYStartZone", "dispXEnd", "dispYEnd", "dispXYEndZone",
              "kickI50Launch", "kickI50Entry", "metresGained", "quarter", "periodSeconds", "disposalSeconds", "Score","Games")
colnames(md) <- varnames

# Keep variables for Hons 2016 analysis only
md2 <- md[, c(1,2,5,6:11)]
md2 <- md2[ which(md$chainStartType == 'Kick In'), ]

# Use existing data to create 'graph' format data
a <- length(md2$player)
a2 <- as.data.frame(md2$player[2:a])
a3 <- rbind(a2, NA)
varnames <- c("edge2")
colnames(a3) <- varnames

b <- length(md2$team)
b2 <- as.data.frame(md2$team[2:b])
b3 <- rbind(b2, NA)
b4 <- cbind(md2$team, b3)
varnames <- c("teamDisp", "teamPossess")
colnames(b4) <- varnames
b4$possessRetained <- ifelse(b4$teamDisp == b4$teamPossess, TRUE, FALSE)

c <- cbind(b4, a3)
md3 <- cbind(md2, c)
md3$edge2[md3$possessRetained == FALSE] <- NA

##Duplicate chain endzone
md3$EndType <- md3$chainEndType
# md3 <- md3[,c(1:8,16,9:15)] #reorder (Dont think I need this step)

##Revalue columns (behind and rushed= behind etc.)
md3$EndType <- revalue(md3$EndType, c("Rushed"="Behind", "Stoppage - TU"="Stoppage", "Stoppage - TI"="Stoppage"))

#Merge the endtype and endzone
md3 <- transform(md3, newcol=paste(EndType, chainEndZone, sep="_"))

# Create graph data only (edge 1, edge 2, weight)
md4 <- md3[ which(md3$edge2 != 'NA'), ] #only keep poss where same team disp and recieves
md4$weight <- 1
md4 <- md4[,c(1,2,4,9,13,16,15)]


varnames <- c("ID","round", "team", "player1", "player2","weight", "chainend")
colnames(md4) <- varnames

# Specify data class for all variables
md4$ID <- as.factor(md4$ID)
md4$round <- as.factor(md4$round)
md4$team <- as.factor(md4$team)
md4$player1 <- as.character(md4$player1)
md4$player2 <- as.character(md4$player2)
md4$weight <- as.numeric(md4$weight)
md4$chainend <- as.factor(md4$chainend)

#Just to count how many possession were analysed
md5 <- md4[ which(md4$chainend != "RushedOpp_D"), ]
md5 <- md5[ which(md5$chainend != "End of Qtr_DM"), ]
#9581
##############################################################################
#Create an object for each team
ADEL <- md4[ which(md4$team == 'ADEL'), ]
BL <- md4[ which(md4$team == 'BL'), ]
CARL <- md4[ which(md4$team == 'CARL'), ]
COLL <- md4[ which(md4$team == 'COLL'), ]
ESS <- md4[ which(md4$team == 'ESS'), ]
FRE <- md4[ which(md4$team == 'FRE'), ]
GCFC <- md4[ which(md4$team == 'GCFC'), ]
GEEL <- md4[which (md4$team == 'GEEL'), ]
GWS <- md4[ which(md4$team == 'GWS'), ]
HAW <- md4[ which(md4$team == 'HAW'), ]
MELB <- md4[ which(md4$team == 'MELB'), ]
NMFC <- md4[ which(md4$team == 'NMFC'), ]
PORT <- md4[ which(md4$team == 'PORT'), ]
RICH <- md4[ which(md4$team == 'RICH'), ]
STK <- md4[ which(md4$team == 'STK'), ]
SYD <- md4[ which(md4$team == 'SYD'), ]
WB <- md4[ which(md4$team == 'WB'), ]
WCE <- md4[ which(md4$team == 'WCE'), ]

#################################################################################
#################################################################################
#SPLIT TEAMS BY ROUND & KI OUTCOME

#Adelaide

#Split by rounds
ADEL01 <- ADEL[ which(ADEL$round == '01'), ]
ADEL02 <- ADEL[ which(ADEL$round == '02'), ]
ADEL03 <- ADEL[ which(ADEL$round == '03'), ]
ADEL04 <- ADEL[ which(ADEL$round == '04'), ]
ADEL05 <- ADEL[ which(ADEL$round == '05'), ]
ADEL06 <- ADEL[ which(ADEL$round == '06'), ]
ADEL07 <- ADEL[ which(ADEL$round == '07'), ]
ADEL08 <- ADEL[ which(ADEL$round == '08'), ]
ADEL09 <- ADEL[ which(ADEL$round == '09'), ]
ADEL10 <- ADEL[ which(ADEL$round == '10'), ]
ADEL11 <- ADEL[ which(ADEL$round == '11'), ]
ADEL12 <- ADEL[ which(ADEL$round == '12'), ]
ADEL13 <- ADEL[ which(ADEL$round == '13'), ]
ADEL14 <- ADEL[ which(ADEL$round == '14'), ]
ADEL15 <- ADEL[ which(ADEL$round == '15'), ]
ADEL16 <- ADEL[ which(ADEL$round == '16'), ]
ADEL17 <- ADEL[ which(ADEL$round == '17'), ]
ADEL18 <- ADEL[ which(ADEL$round == '18'), ]
ADEL19 <- ADEL[ which(ADEL$round == '19'), ]
ADEL20 <- ADEL[ which(ADEL$round == '20'), ]
ADEL21 <- ADEL[ which(ADEL$round == '21'), ]
ADEL22 <- ADEL[ which(ADEL$round == '22'), ]
ADEL23 <- ADEL[ which(ADEL$round == '23'), ]


#############################################################################
#RushedOppo_D- might need to add this
ADEL01_RO <- ADEL01[ which(ADEL01$chainend == 'RushedOppo_D'),]

##Split by kick-in outcome

#Round 1:
#Goal_F
ADEL01_G <- ADEL01[ which(ADEL01$chainend == 'Goal_F'), ]
#Behind_F
ADEL01_B <- ADEL01[ which(ADEL01$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL01_SF <- ADEL01[ which(ADEL01$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL01_TF <- ADEL01[ which(ADEL01$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL01_SAM <- ADEL01[ which(ADEL01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL01_TAM <- ADEL01[ which(ADEL01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL01_SDM <- ADEL01[ which(ADEL01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL01_TDM <- ADEL01[ which(ADEL01$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL01_SD <- ADEL01[ which(ADEL01$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL01_TD <- ADEL01[ which(ADEL01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL01_QT <- ADEL01[ which(ADEL01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
ADEL02_G <- ADEL02[ which(ADEL02$chainend == 'Goal_F'), ]
#Behind_F
ADEL02_B <- ADEL02[ which(ADEL02$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL02_SF <- ADEL02[ which(ADEL02$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL02_TF <- ADEL02[ which(ADEL02$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL02_SAM <- ADEL02[ which(ADEL02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL02_TAM <- ADEL02[ which(ADEL02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL02_SDM <- ADEL02[ which(ADEL02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL02_TDM <- ADEL02[ which(ADEL02$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL02_SD <- ADEL02[ which(ADEL02$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL02_TD <- ADEL02[ which(ADEL02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL02_QT <- ADEL02[ which(ADEL02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
ADEL03_G <- ADEL03[ which(ADEL03$chainend == 'Goal_F'), ]
#Behind_F
ADEL03_B <- ADEL03[ which(ADEL03$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL03_SF <- ADEL03[ which(ADEL03$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL03_TF <- ADEL03[ which(ADEL03$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL03_SAM <- ADEL03[ which(ADEL03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL03_TAM <- ADEL03[ which(ADEL03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL03_SDM <- ADEL03[ which(ADEL03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL03_TDM <- ADEL03[ which(ADEL03$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL03_SD <- ADEL03[ which(ADEL03$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL03_TD <- ADEL03[ which(ADEL03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL03_QT <- ADEL03[ which(ADEL03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
ADEL04_G <- ADEL04[ which(ADEL04$chainend == 'Goal_F'), ]
#Behind_F
ADEL04_B <- ADEL04[ which(ADEL04$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL04_SF <- ADEL04[ which(ADEL04$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL04_TF <- ADEL04[ which(ADEL04$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL04_SAM <- ADEL04[ which(ADEL04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL04_TAM <- ADEL04[ which(ADEL04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL04_SDM <- ADEL04[ which(ADEL04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL04_TDM <- ADEL04[ which(ADEL04$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL04_SD <- ADEL04[ which(ADEL04$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL04_TD <- ADEL04[ which(ADEL04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL04_QT <- ADEL04[ which(ADEL04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
ADEL05_G <- ADEL05[ which(ADEL05$chainend == 'Goal_F'), ]
#Behind_F
ADEL05_B <- ADEL05[ which(ADEL05$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL05_SF <- ADEL05[ which(ADEL05$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL05_TF <- ADEL05[ which(ADEL05$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL05_SAM <- ADEL05[ which(ADEL05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL05_TAM <- ADEL05[ which(ADEL05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL05_SDM <- ADEL05[ which(ADEL05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL05_TDM <- ADEL05[ which(ADEL05$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL05_SD <- ADEL05[ which(ADEL05$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL05_TD <- ADEL05[ which(ADEL05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL05_QT <- ADEL05[ which(ADEL05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
ADEL06_G <- ADEL06[ which(ADEL06$chainend == 'Goal_F'), ]
#Behind_F
ADEL06_B <- ADEL06[ which(ADEL06$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL06_SF <- ADEL06[ which(ADEL06$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL06_TF <- ADEL06[ which(ADEL06$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL06_SAM <- ADEL06[ which(ADEL06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL06_TAM <- ADEL06[ which(ADEL06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL06_SDM <- ADEL06[ which(ADEL06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL06_TDM <- ADEL06[ which(ADEL06$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL06_SD <- ADEL06[ which(ADEL06$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL06_TD <- ADEL06[ which(ADEL06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL06_QT <- ADEL06[ which(ADEL06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
ADEL07_G <- ADEL07[ which(ADEL07$chainend == 'Goal_F'), ]
#Behind_F
ADEL07_B <- ADEL07[ which(ADEL07$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL07_SF <- ADEL07[ which(ADEL07$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL07_TF <- ADEL07[ which(ADEL07$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL07_SAM <- ADEL07[ which(ADEL07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL07_TAM <- ADEL07[ which(ADEL07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL07_SDM <- ADEL07[ which(ADEL07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL07_TDM <- ADEL07[ which(ADEL07$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL07_SD <- ADEL07[ which(ADEL07$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL07_TD <- ADEL07[ which(ADEL07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL07_QT <- ADEL07[ which(ADEL07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
ADEL08_G <- ADEL08[ which(ADEL08$chainend == 'Goal_F'), ]
#Behind_F
ADEL08_B <- ADEL08[ which(ADEL08$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL08_SF <- ADEL08[ which(ADEL08$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL08_TF <- ADEL08[ which(ADEL08$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL08_SAM <- ADEL08[ which(ADEL08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL08_TAM <- ADEL08[ which(ADEL08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL08_SDM <- ADEL08[ which(ADEL08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL08_TDM <- ADEL08[ which(ADEL08$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL08_SD <- ADEL08[ which(ADEL08$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL08_TD <- ADEL08[ which(ADEL08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL08_QT <- ADEL08[ which(ADEL08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
ADEL09_G <- ADEL09[ which(ADEL09$chainend == 'Goal_F'), ]
#Behind_F
ADEL09_B <- ADEL09[ which(ADEL09$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL09_SF <- ADEL09[ which(ADEL09$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL09_TF <- ADEL09[ which(ADEL09$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL09_SAM <- ADEL09[ which(ADEL09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL09_TAM <- ADEL09[ which(ADEL09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL09_SDM <- ADEL09[ which(ADEL09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL09_TDM <- ADEL09[ which(ADEL09$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL09_SD <- ADEL09[ which(ADEL09$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL09_TD <- ADEL09[ which(ADEL09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL09_QT <- ADEL09[ which(ADEL09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
ADEL10_G <- ADEL10[ which(ADEL10$chainend == 'Goal_F'), ]
#Behind_F
ADEL10_B <- ADEL10[ which(ADEL10$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL10_SF <- ADEL10[ which(ADEL10$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL10_TF <- ADEL10[ which(ADEL10$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL10_SAM <- ADEL10[ which(ADEL10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL10_TAM <- ADEL10[ which(ADEL10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL10_SDM <- ADEL10[ which(ADEL10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL10_TDM <- ADEL10[ which(ADEL10$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL10_SD <- ADEL10[ which(ADEL10$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL10_TD <- ADEL10[ which(ADEL10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL10_QT <- ADEL10[ which(ADEL10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
ADEL11_G <- ADEL11[ which(ADEL11$chainend == 'Goal_F'), ]
#Behind_F
ADEL11_B <- ADEL11[ which(ADEL11$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL11_SF <- ADEL11[ which(ADEL11$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL11_TF <- ADEL11[ which(ADEL11$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL11_SAM <- ADEL11[ which(ADEL11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL11_TAM <- ADEL11[ which(ADEL11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL11_SDM <- ADEL11[ which(ADEL11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL11_TDM <- ADEL11[ which(ADEL11$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL11_SD <- ADEL11[ which(ADEL11$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL11_TD <- ADEL11[ which(ADEL11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL11_QT <- ADEL11[ which(ADEL11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
ADEL12_G <- ADEL12[ which(ADEL12$chainend == 'Goal_F'), ]
#Behind_F
ADEL12_B <- ADEL12[ which(ADEL12$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL12_SF <- ADEL12[ which(ADEL12$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL12_TF <- ADEL12[ which(ADEL12$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL12_SAM <- ADEL12[ which(ADEL12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL12_TAM <- ADEL12[ which(ADEL12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL12_SDM <- ADEL12[ which(ADEL12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL12_TDM <- ADEL12[ which(ADEL12$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL12_SD <- ADEL12[ which(ADEL12$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL12_TD <- ADEL12[ which(ADEL12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL12_QT <- ADEL12[ which(ADEL12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
ADEL13_G <- ADEL13[ which(ADEL13$chainend == 'Goal_F'), ]
#Behind_F
ADEL13_B <- ADEL13[ which(ADEL13$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL13_SF <- ADEL13[ which(ADEL13$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL13_TF <- ADEL13[ which(ADEL13$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL13_SAM <- ADEL13[ which(ADEL13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL13_TAM <- ADEL13[ which(ADEL13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL13_SDM <- ADEL13[ which(ADEL13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL13_TDM <- ADEL13[ which(ADEL13$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL13_SD <- ADEL13[ which(ADEL13$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL13_TD <- ADEL13[ which(ADEL13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL13_QT <- ADEL13[ which(ADEL13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
ADEL14_G <- ADEL14[ which(ADEL14$chainend == 'Goal_F'), ]
#Behind_F
ADEL14_B <- ADEL14[ which(ADEL14$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL14_SF <- ADEL14[ which(ADEL14$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL14_TF <- ADEL14[ which(ADEL14$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL14_SAM <- ADEL14[ which(ADEL14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL14_TAM <- ADEL14[ which(ADEL14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL14_SDM <- ADEL14[ which(ADEL14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL14_TDM <- ADEL14[ which(ADEL14$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL14_SD <- ADEL14[ which(ADEL14$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL14_TD <- ADEL14[ which(ADEL14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL14_QT <- ADEL14[ which(ADEL14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
ADEL15_G <- ADEL15[ which(ADEL15$chainend == 'Goal_F'), ]
#Behind_F
ADEL15_B <- ADEL15[ which(ADEL15$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL15_SF <- ADEL15[ which(ADEL15$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL15_TF <- ADEL15[ which(ADEL15$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL15_SAM <- ADEL15[ which(ADEL15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL15_TAM <- ADEL15[ which(ADEL15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL15_SDM <- ADEL15[ which(ADEL15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL15_TDM <- ADEL15[ which(ADEL15$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL15_SD <- ADEL15[ which(ADEL15$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL15_TD <- ADEL15[ which(ADEL15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL15_QT <- ADEL15[ which(ADEL15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
ADEL16_G <- ADEL16[ which(ADEL16$chainend == 'Goal_F'), ]
#Behind_F
ADEL16_B <- ADEL16[ which(ADEL16$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL16_SF <- ADEL16[ which(ADEL16$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL16_TF <- ADEL16[ which(ADEL16$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL16_SAM <- ADEL16[ which(ADEL16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL16_TAM <- ADEL16[ which(ADEL16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL16_SDM <- ADEL16[ which(ADEL16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL16_TDM <- ADEL16[ which(ADEL16$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL16_SD <- ADEL16[ which(ADEL16$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL16_TD <- ADEL16[ which(ADEL16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL16_QT <- ADEL16[ which(ADEL16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
ADEL17_G <- ADEL17[ which(ADEL17$chainend == 'Goal_F'), ]
#Behind_F
ADEL17_B <- ADEL17[ which(ADEL17$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL17_SF <- ADEL17[ which(ADEL17$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL17_TF <- ADEL17[ which(ADEL17$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL17_SAM <- ADEL17[ which(ADEL17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL17_TAM <- ADEL17[ which(ADEL17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL17_SDM <- ADEL17[ which(ADEL17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL17_TDM <- ADEL17[ which(ADEL17$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL17_SD <- ADEL17[ which(ADEL17$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL17_TD <- ADEL17[ which(ADEL17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL17_QT <- ADEL17[ which(ADEL17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
ADEL18_G <- ADEL18[ which(ADEL18$chainend == 'Goal_F'), ]
#Behind_F
ADEL18_B <- ADEL18[ which(ADEL18$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL18_SF <- ADEL18[ which(ADEL18$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL18_TF <- ADEL18[ which(ADEL18$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL18_SAM <- ADEL18[ which(ADEL18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL18_TAM <- ADEL18[ which(ADEL18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL18_SDM <- ADEL18[ which(ADEL18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL18_TDM <- ADEL18[ which(ADEL18$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL18_SD <- ADEL18[ which(ADEL18$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL18_TD <- ADEL18[ which(ADEL18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL18_QT <- ADEL18[ which(ADEL18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
ADEL19_G <- ADEL19[ which(ADEL19$chainend == 'Goal_F'), ]
#Behind_F
ADEL19_B <- ADEL19[ which(ADEL19$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL19_SF <- ADEL19[ which(ADEL19$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL19_TF <- ADEL19[ which(ADEL19$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL19_SAM <- ADEL19[ which(ADEL19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL19_TAM <- ADEL19[ which(ADEL19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL19_SDM <- ADEL19[ which(ADEL19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL19_TDM <- ADEL19[ which(ADEL19$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL19_SD <- ADEL19[ which(ADEL19$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL19_TD <- ADEL19[ which(ADEL19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL19_QT <- ADEL19[ which(ADEL19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
ADEL20_G <- ADEL20[ which(ADEL20$chainend == 'Goal_F'), ]
#Behind_F
ADEL20_B <- ADEL20[ which(ADEL20$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL20_SF <- ADEL20[ which(ADEL20$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL20_TF <- ADEL20[ which(ADEL20$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL20_SAM <- ADEL20[ which(ADEL20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL20_TAM <- ADEL20[ which(ADEL20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL20_SDM <- ADEL20[ which(ADEL20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL20_TDM <- ADEL20[ which(ADEL20$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL20_SD <- ADEL20[ which(ADEL20$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL20_TD <- ADEL20[ which(ADEL20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL20_QT <- ADEL20[ which(ADEL20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
ADEL21_G <- ADEL21[ which(ADEL21$chainend == 'Goal_F'), ]
#Behind_F
ADEL21_B <- ADEL21[ which(ADEL21$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL21_SF <- ADEL21[ which(ADEL21$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL21_TF <- ADEL21[ which(ADEL21$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL21_SAM <- ADEL21[ which(ADEL21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL21_TAM <- ADEL21[ which(ADEL21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL21_SDM <- ADEL21[ which(ADEL21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL21_TDM <- ADEL21[ which(ADEL21$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL21_SD <- ADEL21[ which(ADEL21$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL21_TD <- ADEL21[ which(ADEL21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL21_QT <- ADEL21[ which(ADEL21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
ADEL22_G <- ADEL22[ which(ADEL22$chainend == 'Goal_F'), ]
#Behind_F
ADEL22_B <- ADEL22[ which(ADEL22$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL22_SF <- ADEL22[ which(ADEL22$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL22_TF <- ADEL22[ which(ADEL22$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL22_SAM <- ADEL22[ which(ADEL22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL22_TAM <- ADEL22[ which(ADEL22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL22_SDM <- ADEL22[ which(ADEL22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL22_TDM <- ADEL22[ which(ADEL22$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL22_SD <- ADEL22[ which(ADEL22$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL22_TD <- ADEL22[ which(ADEL22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL22_QT <- ADEL22[ which(ADEL22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
ADEL23_G <- ADEL23[ which(ADEL23$chainend == 'Goal_F'), ]
#Behind_F
ADEL23_B <- ADEL23[ which(ADEL23$chainend == 'Behind_F'), ]
#Stoppage_F
ADEL23_SF <- ADEL23[ which(ADEL23$chainend == 'Stoppage_F'), ]
#Turnover_F
ADEL23_TF <- ADEL23[ which(ADEL23$chainend == 'Turnover_F'), ]
#Stoppage_AM
ADEL23_SAM <- ADEL23[ which(ADEL23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ADEL23_TAM <- ADEL23[ which(ADEL23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ADEL23_SDM <- ADEL23[ which(ADEL23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ADEL23_TDM <- ADEL23[ which(ADEL23$chainend == 'Turnover_DM'), ]
#Stoppage_D
ADEL23_SD <- ADEL23[ which(ADEL23$chainend == 'Stoppage_D'), ]
#Turnover_D
ADEL23_TD <- ADEL23[ which(ADEL23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ADEL23_QT <- ADEL23[ which(ADEL23$chainend == 'End of Qtr_DM'), ]


#Brisbane

#Split by rounds
BL01 <- BL[ which(BL$round == '01'), ]
BL02 <- BL[ which(BL$round == '02'), ]
BL03 <- BL[ which(BL$round == '03'), ]
BL04 <- BL[ which(BL$round == '04'), ]
BL05 <- BL[ which(BL$round == '05'), ]
BL06 <- BL[ which(BL$round == '06'), ]
BL07 <- BL[ which(BL$round == '07'), ]
BL08 <- BL[ which(BL$round == '08'), ]
BL09 <- BL[ which(BL$round == '09'), ]
BL10 <- BL[ which(BL$round == '10'), ]
BL11 <- BL[ which(BL$round == '11'), ]
BL12 <- BL[ which(BL$round == '12'), ]
BL13 <- BL[ which(BL$round == '13'), ]
BL14 <- BL[ which(BL$round == '14'), ]
BL15 <- BL[ which(BL$round == '15'), ]
BL16 <- BL[ which(BL$round == '16'), ]
BL17 <- BL[ which(BL$round == '17'), ]
BL18 <- BL[ which(BL$round == '18'), ]
BL19 <- BL[ which(BL$round == '19'), ]
BL20 <- BL[ which(BL$round == '20'), ]
BL21 <- BL[ which(BL$round == '21'), ]
BL22 <- BL[ which(BL$round == '22'), ]
BL23 <- BL[ which(BL$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
BL01_G <- BL01[ which(BL01$chainend == 'Goal_F'), ]
#Behind_F
BL01_B <- BL01[ which(BL01$chainend == 'Behind_F'), ]
#Stoppage_F
BL01_SF <- BL01[ which(BL01$chainend == 'Stoppage_F'), ]
#Turnover_F
BL01_TF <- BL01[ which(BL01$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL01_SAM <- BL01[ which(BL01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL01_TAM <- BL01[ which(BL01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL01_SDM <- BL01[ which(BL01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL01_TDM <- BL01[ which(BL01$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL01_SD <- BL01[ which(BL01$chainend == 'Stoppage_D'), ]
#Turnover_D
BL01_TD <- BL01[ which(BL01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL01_QT <- BL01[ which(BL01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
BL02_G <- BL02[ which(BL02$chainend == 'Goal_F'), ]
#Behind_F
BL02_B <- BL02[ which(BL02$chainend == 'Behind_F'), ]
#Stoppage_F
BL02_SF <- BL02[ which(BL02$chainend == 'Stoppage_F'), ]
#Turnover_F
BL02_TF <- BL02[ which(BL02$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL02_SAM <- BL02[ which(BL02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL02_TAM <- BL02[ which(BL02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL02_SDM <- BL02[ which(BL02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL02_TDM <- BL02[ which(BL02$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL02_SD <- BL02[ which(BL02$chainend == 'Stoppage_D'), ]
#Turnover_D
BL02_TD <- BL02[ which(BL02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL02_QT <- BL02[ which(BL02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
BL03_G <- BL03[ which(BL03$chainend == 'Goal_F'), ]
#Behind_F
BL03_B <- BL03[ which(BL03$chainend == 'Behind_F'), ]
#Stoppage_F
BL03_SF <- BL03[ which(BL03$chainend == 'Stoppage_F'), ]
#Turnover_F
BL03_TF <- BL03[ which(BL03$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL03_SAM <- BL03[ which(BL03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL03_TAM <- BL03[ which(BL03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL03_SDM <- BL03[ which(BL03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL03_TDM <- BL03[ which(BL03$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL03_SD <- BL03[ which(BL03$chainend == 'Stoppage_D'), ]
#Turnover_D
BL03_TD <- BL03[ which(BL03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL03_QT <- BL03[ which(BL03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
BL04_G <- BL04[ which(BL04$chainend == 'Goal_F'), ]
#Behind_F
BL04_B <- BL04[ which(BL04$chainend == 'Behind_F'), ]
#Stoppage_F
BL04_SF <- BL04[ which(BL04$chainend == 'Stoppage_F'), ]
#Turnover_F
BL04_TF <- BL04[ which(BL04$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL04_SAM <- BL04[ which(BL04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL04_TAM <- BL04[ which(BL04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL04_SDM <- BL04[ which(BL04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL04_TDM <- BL04[ which(BL04$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL04_SD <- BL04[ which(BL04$chainend == 'Stoppage_D'), ]
#Turnover_D
BL04_TD <- BL04[ which(BL04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL04_QT <- BL04[ which(BL04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
BL05_G <- BL05[ which(BL05$chainend == 'Goal_F'), ]
#Behind_F
BL05_B <- BL05[ which(BL05$chainend == 'Behind_F'), ]
#Stoppage_F
BL05_SF <- BL05[ which(BL05$chainend == 'Stoppage_F'), ]
#Turnover_F
BL05_TF <- BL05[ which(BL05$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL05_SAM <- BL05[ which(BL05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL05_TAM <- BL05[ which(BL05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL05_SDM <- BL05[ which(BL05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL05_TDM <- BL05[ which(BL05$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL05_SD <- BL05[ which(BL05$chainend == 'Stoppage_D'), ]
#Turnover_D
BL05_TD <- BL05[ which(BL05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL05_QT <- BL05[ which(BL05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
BL06_G <- BL06[ which(BL06$chainend == 'Goal_F'), ]
#Behind_F
BL06_B <- BL06[ which(BL06$chainend == 'Behind_F'), ]
#Stoppage_F
BL06_SF <- BL06[ which(BL06$chainend == 'Stoppage_F'), ]
#Turnover_F
BL06_TF <- BL06[ which(BL06$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL06_SAM <- BL06[ which(BL06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL06_TAM <- BL06[ which(BL06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL06_SDM <- BL06[ which(BL06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL06_TDM <- BL06[ which(BL06$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL06_SD <- BL06[ which(BL06$chainend == 'Stoppage_D'), ]
#Turnover_D
BL06_TD <- BL06[ which(BL06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL06_QT <- BL06[ which(BL06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
BL07_G <- BL07[ which(BL07$chainend == 'Goal_F'), ]
#Behind_F
BL07_B <- BL07[ which(BL07$chainend == 'Behind_F'), ]
#Stoppage_F
BL07_SF <- BL07[ which(BL07$chainend == 'Stoppage_F'), ]
#Turnover_F
BL07_TF <- BL07[ which(BL07$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL07_SAM <- BL07[ which(BL07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL07_TAM <- BL07[ which(BL07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL07_SDM <- BL07[ which(BL07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL07_TDM <- BL07[ which(BL07$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL07_SD <- BL07[ which(BL07$chainend == 'Stoppage_D'), ]
#Turnover_D
BL07_TD <- BL07[ which(BL07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL07_QT <- BL07[ which(BL07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
BL08_G <- BL08[ which(BL08$chainend == 'Goal_F'), ]
#Behind_F
BL08_B <- BL08[ which(BL08$chainend == 'Behind_F'), ]
#Stoppage_F
BL08_SF <- BL08[ which(BL08$chainend == 'Stoppage_F'), ]
#Turnover_F
BL08_TF <- BL08[ which(BL08$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL08_SAM <- BL08[ which(BL08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL08_TAM <- BL08[ which(BL08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL08_SDM <- BL08[ which(BL08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL08_TDM <- BL08[ which(BL08$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL08_SD <- BL08[ which(BL08$chainend == 'Stoppage_D'), ]
#Turnover_D
BL08_TD <- BL08[ which(BL08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL08_QT <- BL08[ which(BL08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
BL09_G <- BL09[ which(BL09$chainend == 'Goal_F'), ]
#Behind_F
BL09_B <- BL09[ which(BL09$chainend == 'Behind_F'), ]
#Stoppage_F
BL09_SF <- BL09[ which(BL09$chainend == 'Stoppage_F'), ]
#Turnover_F
BL09_TF <- BL09[ which(BL09$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL09_SAM <- BL09[ which(BL09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL09_TAM <- BL09[ which(BL09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL09_SDM <- BL09[ which(BL09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL09_TDM <- BL09[ which(BL09$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL09_SD <- BL09[ which(BL09$chainend == 'Stoppage_D'), ]
#Turnover_D
BL09_TD <- BL09[ which(BL09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL09_QT <- BL09[ which(BL09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
BL10_G <- BL10[ which(BL10$chainend == 'Goal_F'), ]
#Behind_F
BL10_B <- BL10[ which(BL10$chainend == 'Behind_F'), ]
#Stoppage_F
BL10_SF <- BL10[ which(BL10$chainend == 'Stoppage_F'), ]
#Turnover_F
BL10_TF <- BL10[ which(BL10$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL10_SAM <- BL10[ which(BL10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL10_TAM <- BL10[ which(BL10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL10_SDM <- BL10[ which(BL10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL10_TDM <- BL10[ which(BL10$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL10_SD <- BL10[ which(BL10$chainend == 'Stoppage_D'), ]
#Turnover_D
BL10_TD <- BL10[ which(BL10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL10_QT <- BL10[ which(BL10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
BL11_G <- BL11[ which(BL11$chainend == 'Goal_F'), ]
#Behind_F
BL11_B <- BL11[ which(BL11$chainend == 'Behind_F'), ]
#Stoppage_F
BL11_SF <- BL11[ which(BL11$chainend == 'Stoppage_F'), ]
#Turnover_F
BL11_TF <- BL11[ which(BL11$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL11_SAM <- BL11[ which(BL11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL11_TAM <- BL11[ which(BL11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL11_SDM <- BL11[ which(BL11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL11_TDM <- BL11[ which(BL11$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL11_SD <- BL11[ which(BL11$chainend == 'Stoppage_D'), ]
#Turnover_D
BL11_TD <- BL11[ which(BL11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL11_QT <- BL11[ which(BL11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
BL12_G <- BL12[ which(BL12$chainend == 'Goal_F'), ]
#Behind_F
BL12_B <- BL12[ which(BL12$chainend == 'Behind_F'), ]
#Stoppage_F
BL12_SF <- BL12[ which(BL12$chainend == 'Stoppage_F'), ]
#Turnover_F
BL12_TF <- BL12[ which(BL12$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL12_SAM <- BL12[ which(BL12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL12_TAM <- BL12[ which(BL12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL12_SDM <- BL12[ which(BL12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL12_TDM <- BL12[ which(BL12$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL12_SD <- BL12[ which(BL12$chainend == 'Stoppage_D'), ]
#Turnover_D
BL12_TD <- BL12[ which(BL12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL12_QT <- BL12[ which(BL12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
BL13_G <- BL13[ which(BL13$chainend == 'Goal_F'), ]
#Behind_F
BL13_B <- BL13[ which(BL13$chainend == 'Behind_F'), ]
#Stoppage_F
BL13_SF <- BL13[ which(BL13$chainend == 'Stoppage_F'), ]
#Turnover_F
BL13_TF <- BL13[ which(BL13$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL13_SAM <- BL13[ which(BL13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL13_TAM <- BL13[ which(BL13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL13_SDM <- BL13[ which(BL13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL13_TDM <- BL13[ which(BL13$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL13_SD <- BL13[ which(BL13$chainend == 'Stoppage_D'), ]
#Turnover_D
BL13_TD <- BL13[ which(BL13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL13_QT <- BL13[ which(BL13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
BL14_G <- BL14[ which(BL14$chainend == 'Goal_F'), ]
#Behind_F
BL14_B <- BL14[ which(BL14$chainend == 'Behind_F'), ]
#Stoppage_F
BL14_SF <- BL14[ which(BL14$chainend == 'Stoppage_F'), ]
#Turnover_F
BL14_TF <- BL14[ which(BL14$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL14_SAM <- BL14[ which(BL14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL14_TAM <- BL14[ which(BL14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL14_SDM <- BL14[ which(BL14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL14_TDM <- BL14[ which(BL14$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL14_SD <- BL14[ which(BL14$chainend == 'Stoppage_D'), ]
#Turnover_D
BL14_TD <- BL14[ which(BL14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL14_QT <- BL14[ which(BL14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
BL15_G <- BL15[ which(BL15$chainend == 'Goal_F'), ]
#Behind_F
BL15_B <- BL15[ which(BL15$chainend == 'Behind_F'), ]
#Stoppage_F
BL15_SF <- BL15[ which(BL15$chainend == 'Stoppage_F'), ]
#Turnover_F
BL15_TF <- BL15[ which(BL15$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL15_SAM <- BL15[ which(BL15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL15_TAM <- BL15[ which(BL15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL15_SDM <- BL15[ which(BL15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL15_TDM <- BL15[ which(BL15$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL15_SD <- BL15[ which(BL15$chainend == 'Stoppage_D'), ]
#Turnover_D
BL15_TD <- BL15[ which(BL15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL15_QT <- BL15[ which(BL15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
BL16_G <- BL16[ which(BL16$chainend == 'Goal_F'), ]
#Behind_F
BL16_B <- BL16[ which(BL16$chainend == 'Behind_F'), ]
#Stoppage_F
BL16_SF <- BL16[ which(BL16$chainend == 'Stoppage_F'), ]
#Turnover_F
BL16_TF <- BL16[ which(BL16$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL16_SAM <- BL16[ which(BL16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL16_TAM <- BL16[ which(BL16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL16_SDM <- BL16[ which(BL16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL16_TDM <- BL16[ which(BL16$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL16_SD <- BL16[ which(BL16$chainend == 'Stoppage_D'), ]
#Turnover_D
BL16_TD <- BL16[ which(BL16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL16_QT <- BL16[ which(BL16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
BL17_G <- BL17[ which(BL17$chainend == 'Goal_F'), ]
#Behind_F
BL17_B <- BL17[ which(BL17$chainend == 'Behind_F'), ]
#Stoppage_F
BL17_SF <- BL17[ which(BL17$chainend == 'Stoppage_F'), ]
#Turnover_F
BL17_TF <- BL17[ which(BL17$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL17_SAM <- BL17[ which(BL17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL17_TAM <- BL17[ which(BL17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL17_SDM <- BL17[ which(BL17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL17_TDM <- BL17[ which(BL17$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL17_SD <- BL17[ which(BL17$chainend == 'Stoppage_D'), ]
#Turnover_D
BL17_TD <- BL17[ which(BL17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL17_QT <- BL17[ which(BL17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
BL18_G <- BL18[ which(BL18$chainend == 'Goal_F'), ]
#Behind_F
BL18_B <- BL18[ which(BL18$chainend == 'Behind_F'), ]
#Stoppage_F
BL18_SF <- BL18[ which(BL18$chainend == 'Stoppage_F'), ]
#Turnover_F
BL18_TF <- BL18[ which(BL18$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL18_SAM <- BL18[ which(BL18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL18_TAM <- BL18[ which(BL18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL18_SDM <- BL18[ which(BL18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL18_TDM <- BL18[ which(BL18$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL18_SD <- BL18[ which(BL18$chainend == 'Stoppage_D'), ]
#Turnover_D
BL18_TD <- BL18[ which(BL18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL18_QT <- BL18[ which(BL18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
BL19_G <- BL19[ which(BL19$chainend == 'Goal_F'), ]
#Behind_F
BL19_B <- BL19[ which(BL19$chainend == 'Behind_F'), ]
#Stoppage_F
BL19_SF <- BL19[ which(BL19$chainend == 'Stoppage_F'), ]
#Turnover_F
BL19_TF <- BL19[ which(BL19$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL19_SAM <- BL19[ which(BL19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL19_TAM <- BL19[ which(BL19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL19_SDM <- BL19[ which(BL19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL19_TDM <- BL19[ which(BL19$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL19_SD <- BL19[ which(BL19$chainend == 'Stoppage_D'), ]
#Turnover_D
BL19_TD <- BL19[ which(BL19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL19_QT <- BL19[ which(BL19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
BL20_G <- BL20[ which(BL20$chainend == 'Goal_F'), ]
#Behind_F
BL20_B <- BL20[ which(BL20$chainend == 'Behind_F'), ]
#Stoppage_F
BL20_SF <- BL20[ which(BL20$chainend == 'Stoppage_F'), ]
#Turnover_F
BL20_TF <- BL20[ which(BL20$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL20_SAM <- BL20[ which(BL20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL20_TAM <- BL20[ which(BL20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL20_SDM <- BL20[ which(BL20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL20_TDM <- BL20[ which(BL20$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL20_SD <- BL20[ which(BL20$chainend == 'Stoppage_D'), ]
#Turnover_D
BL20_TD <- BL20[ which(BL20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL20_QT <- BL20[ which(BL20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
BL21_G <- BL21[ which(BL21$chainend == 'Goal_F'), ]
#Behind_F
BL21_B <- BL21[ which(BL21$chainend == 'Behind_F'), ]
#Stoppage_F
BL21_SF <- BL21[ which(BL21$chainend == 'Stoppage_F'), ]
#Turnover_F
BL21_TF <- BL21[ which(BL21$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL21_SAM <- BL21[ which(BL21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL21_TAM <- BL21[ which(BL21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL21_SDM <- BL21[ which(BL21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL21_TDM <- BL21[ which(BL21$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL21_SD <- BL21[ which(BL21$chainend == 'Stoppage_D'), ]
#Turnover_D
BL21_TD <- BL21[ which(BL21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL21_QT <- BL21[ which(BL21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
BL22_G <- BL22[ which(BL22$chainend == 'Goal_F'), ]
#Behind_F
BL22_B <- BL22[ which(BL22$chainend == 'Behind_F'), ]
#Stoppage_F
BL22_SF <- BL22[ which(BL22$chainend == 'Stoppage_F'), ]
#Turnover_F
BL22_TF <- BL22[ which(BL22$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL22_SAM <- BL22[ which(BL22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL22_TAM <- BL22[ which(BL22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL22_SDM <- BL22[ which(BL22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL22_TDM <- BL22[ which(BL22$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL22_SD <- BL22[ which(BL22$chainend == 'Stoppage_D'), ]
#Turnover_D
BL22_TD <- BL22[ which(BL22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL22_QT <- BL22[ which(BL22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
BL23_G <- BL23[ which(BL23$chainend == 'Goal_F'), ]
#Behind_F
BL23_B <- BL23[ which(BL23$chainend == 'Behind_F'), ]
#Stoppage_F
BL23_SF <- BL23[ which(BL23$chainend == 'Stoppage_F'), ]
#Turnover_F
BL23_TF <- BL23[ which(BL23$chainend == 'Turnover_F'), ]
#Stoppage_AM
BL23_SAM <- BL23[ which(BL23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
BL23_TAM <- BL23[ which(BL23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
BL23_SDM <- BL23[ which(BL23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
BL23_TDM <- BL23[ which(BL23$chainend == 'Turnover_DM'), ]
#Stoppage_D
BL23_SD <- BL23[ which(BL23$chainend == 'Stoppage_D'), ]
#Turnover_D
BL23_TD <- BL23[ which(BL23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
BL23_QT <- BL23[ which(BL23$chainend == 'End of Qtr_DM'), ]

#Carlton

#Split by rounds
CARL01 <- CARL[ which(CARL$round == '01'), ]
CARL02 <- CARL[ which(CARL$round == '02'), ]
CARL03 <- CARL[ which(CARL$round == '03'), ]
CARL04 <- CARL[ which(CARL$round == '04'), ]
CARL05 <- CARL[ which(CARL$round == '05'), ]
CARL06 <- CARL[ which(CARL$round == '06'), ]
CARL07 <- CARL[ which(CARL$round == '07'), ]
CARL08 <- CARL[ which(CARL$round == '08'), ]
CARL09 <- CARL[ which(CARL$round == '09'), ]
CARL10 <- CARL[ which(CARL$round == '10'), ]
CARL11 <- CARL[ which(CARL$round == '11'), ]
CARL12 <- CARL[ which(CARL$round == '12'), ]
CARL13 <- CARL[ which(CARL$round == '13'), ]
CARL14 <- CARL[ which(CARL$round == '14'), ]
CARL15 <- CARL[ which(CARL$round == '15'), ]
CARL16 <- CARL[ which(CARL$round == '16'), ]
CARL17 <- CARL[ which(CARL$round == '17'), ]
CARL18 <- CARL[ which(CARL$round == '18'), ]
CARL19 <- CARL[ which(CARL$round == '19'), ]
CARL20 <- CARL[ which(CARL$round == '20'), ]
CARL21 <- CARL[ which(CARL$round == '21'), ]
CARL22 <- CARL[ which(CARL$round == '22'), ]
CARL23 <- CARL[ which(CARL$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
CARL01_G <- CARL01[ which(CARL01$chainend == 'Goal_F'), ]
#Behind_F
CARL01_B <- CARL01[ which(CARL01$chainend == 'Behind_F'), ]
#Stoppage_F
CARL01_SF <- CARL01[ which(CARL01$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL01_TF <- CARL01[ which(CARL01$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL01_SAM <- CARL01[ which(CARL01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL01_TAM <- CARL01[ which(CARL01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL01_SDM <- CARL01[ which(CARL01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL01_TDM <- CARL01[ which(CARL01$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL01_SD <- CARL01[ which(CARL01$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL01_TD <- CARL01[ which(CARL01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL01_QT <- CARL01[ which(CARL01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
CARL02_G <- CARL02[ which(CARL02$chainend == 'Goal_F'), ]
#Behind_F
CARL02_B <- CARL02[ which(CARL02$chainend == 'Behind_F'), ]
#Stoppage_F
CARL02_SF <- CARL02[ which(CARL02$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL02_TF <- CARL02[ which(CARL02$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL02_SAM <- CARL02[ which(CARL02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL02_TAM <- CARL02[ which(CARL02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL02_SDM <- CARL02[ which(CARL02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL02_TDM <- CARL02[ which(CARL02$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL02_SD <- CARL02[ which(CARL02$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL02_TD <- CARL02[ which(CARL02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL02_QT <- CARL02[ which(CARL02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
CARL03_G <- CARL03[ which(CARL03$chainend == 'Goal_F'), ]
#Behind_F
CARL03_B <- CARL03[ which(CARL03$chainend == 'Behind_F'), ]
#Stoppage_F
CARL03_SF <- CARL03[ which(CARL03$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL03_TF <- CARL03[ which(CARL03$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL03_SAM <- CARL03[ which(CARL03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL03_TAM <- CARL03[ which(CARL03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL03_SDM <- CARL03[ which(CARL03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL03_TDM <- CARL03[ which(CARL03$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL03_SD <- CARL03[ which(CARL03$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL03_TD <- CARL03[ which(CARL03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL03_QT <- CARL03[ which(CARL03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
CARL04_G <- CARL04[ which(CARL04$chainend == 'Goal_F'), ]
#Behind_F
CARL04_B <- CARL04[ which(CARL04$chainend == 'Behind_F'), ]
#Stoppage_F
CARL04_SF <- CARL04[ which(CARL04$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL04_TF <- CARL04[ which(CARL04$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL04_SAM <- CARL04[ which(CARL04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL04_TAM <- CARL04[ which(CARL04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL04_SDM <- CARL04[ which(CARL04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL04_TDM <- CARL04[ which(CARL04$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL04_SD <- CARL04[ which(CARL04$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL04_TD <- CARL04[ which(CARL04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL04_QT <- CARL04[ which(CARL04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
CARL05_G <- CARL05[ which(CARL05$chainend == 'Goal_F'), ]
#Behind_F
CARL05_B <- CARL05[ which(CARL05$chainend == 'Behind_F'), ]
#Stoppage_F
CARL05_SF <- CARL05[ which(CARL05$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL05_TF <- CARL05[ which(CARL05$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL05_SAM <- CARL05[ which(CARL05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL05_TAM <- CARL05[ which(CARL05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL05_SDM <- CARL05[ which(CARL05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL05_TDM <- CARL05[ which(CARL05$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL05_SD <- CARL05[ which(CARL05$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL05_TD <- CARL05[ which(CARL05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL05_QT <- CARL05[ which(CARL05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
CARL06_G <- CARL06[ which(CARL06$chainend == 'Goal_F'), ]
#Behind_F
CARL06_B <- CARL06[ which(CARL06$chainend == 'Behind_F'), ]
#Stoppage_F
CARL06_SF <- CARL06[ which(CARL06$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL06_TF <- CARL06[ which(CARL06$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL06_SAM <- CARL06[ which(CARL06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL06_TAM <- CARL06[ which(CARL06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL06_SDM <- CARL06[ which(CARL06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL06_TDM <- CARL06[ which(CARL06$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL06_SD <- CARL06[ which(CARL06$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL06_TD <- CARL06[ which(CARL06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL06_QT <- CARL06[ which(CARL06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
CARL07_G <- CARL07[ which(CARL07$chainend == 'Goal_F'), ]
#Behind_F
CARL07_B <- CARL07[ which(CARL07$chainend == 'Behind_F'), ]
#Stoppage_F
CARL07_SF <- CARL07[ which(CARL07$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL07_TF <- CARL07[ which(CARL07$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL07_SAM <- CARL07[ which(CARL07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL07_TAM <- CARL07[ which(CARL07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL07_SDM <- CARL07[ which(CARL07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL07_TDM <- CARL07[ which(CARL07$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL07_SD <- CARL07[ which(CARL07$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL07_TD <- CARL07[ which(CARL07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL07_QT <- CARL07[ which(CARL07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
CARL08_G <- CARL08[ which(CARL08$chainend == 'Goal_F'), ]
#Behind_F
CARL08_B <- CARL08[ which(CARL08$chainend == 'Behind_F'), ]
#Stoppage_F
CARL08_SF <- CARL08[ which(CARL08$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL08_TF <- CARL08[ which(CARL08$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL08_SAM <- CARL08[ which(CARL08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL08_TAM <- CARL08[ which(CARL08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL08_SDM <- CARL08[ which(CARL08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL08_TDM <- CARL08[ which(CARL08$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL08_SD <- CARL08[ which(CARL08$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL08_TD <- CARL08[ which(CARL08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL08_QT <- CARL08[ which(CARL08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
CARL09_G <- CARL09[ which(CARL09$chainend == 'Goal_F'), ]
#Behind_F
CARL09_B <- CARL09[ which(CARL09$chainend == 'Behind_F'), ]
#Stoppage_F
CARL09_SF <- CARL09[ which(CARL09$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL09_TF <- CARL09[ which(CARL09$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL09_SAM <- CARL09[ which(CARL09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL09_TAM <- CARL09[ which(CARL09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL09_SDM <- CARL09[ which(CARL09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL09_TDM <- CARL09[ which(CARL09$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL09_SD <- CARL09[ which(CARL09$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL09_TD <- CARL09[ which(CARL09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL09_QT <- CARL09[ which(CARL09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
CARL10_G <- CARL10[ which(CARL10$chainend == 'Goal_F'), ]
#Behind_F
CARL10_B <- CARL10[ which(CARL10$chainend == 'Behind_F'), ]
#Stoppage_F
CARL10_SF <- CARL10[ which(CARL10$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL10_TF <- CARL10[ which(CARL10$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL10_SAM <- CARL10[ which(CARL10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL10_TAM <- CARL10[ which(CARL10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL10_SDM <- CARL10[ which(CARL10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL10_TDM <- CARL10[ which(CARL10$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL10_SD <- CARL10[ which(CARL10$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL10_TD <- CARL10[ which(CARL10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL10_QT <- CARL10[ which(CARL10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
CARL11_G <- CARL11[ which(CARL11$chainend == 'Goal_F'), ]
#Behind_F
CARL11_B <- CARL11[ which(CARL11$chainend == 'Behind_F'), ]
#Stoppage_F
CARL11_SF <- CARL11[ which(CARL11$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL11_TF <- CARL11[ which(CARL11$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL11_SAM <- CARL11[ which(CARL11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL11_TAM <- CARL11[ which(CARL11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL11_SDM <- CARL11[ which(CARL11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL11_TDM <- CARL11[ which(CARL11$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL11_SD <- CARL11[ which(CARL11$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL11_TD <- CARL11[ which(CARL11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL11_QT <- CARL11[ which(CARL11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
CARL12_G <- CARL12[ which(CARL12$chainend == 'Goal_F'), ]
#Behind_F
CARL12_B <- CARL12[ which(CARL12$chainend == 'Behind_F'), ]
#Stoppage_F
CARL12_SF <- CARL12[ which(CARL12$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL12_TF <- CARL12[ which(CARL12$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL12_SAM <- CARL12[ which(CARL12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL12_TAM <- CARL12[ which(CARL12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL12_SDM <- CARL12[ which(CARL12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL12_TDM <- CARL12[ which(CARL12$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL12_SD <- CARL12[ which(CARL12$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL12_TD <- CARL12[ which(CARL12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL12_QT <- CARL12[ which(CARL12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
CARL13_G <- CARL13[ which(CARL13$chainend == 'Goal_F'), ]
#Behind_F
CARL13_B <- CARL13[ which(CARL13$chainend == 'Behind_F'), ]
#Stoppage_F
CARL13_SF <- CARL13[ which(CARL13$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL13_TF <- CARL13[ which(CARL13$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL13_SAM <- CARL13[ which(CARL13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL13_TAM <- CARL13[ which(CARL13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL13_SDM <- CARL13[ which(CARL13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL13_TDM <- CARL13[ which(CARL13$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL13_SD <- CARL13[ which(CARL13$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL13_TD <- CARL13[ which(CARL13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL13_QT <- CARL13[ which(CARL13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
CARL14_G <- CARL14[ which(CARL14$chainend == 'Goal_F'), ]
#Behind_F
CARL14_B <- CARL14[ which(CARL14$chainend == 'Behind_F'), ]
#Stoppage_F
CARL14_SF <- CARL14[ which(CARL14$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL14_TF <- CARL14[ which(CARL14$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL14_SAM <- CARL14[ which(CARL14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL14_TAM <- CARL14[ which(CARL14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL14_SDM <- CARL14[ which(CARL14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL14_TDM <- CARL14[ which(CARL14$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL14_SD <- CARL14[ which(CARL14$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL14_TD <- CARL14[ which(CARL14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL14_QT <- CARL14[ which(CARL14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
CARL15_G <- CARL15[ which(CARL15$chainend == 'Goal_F'), ]
#Behind_F
CARL15_B <- CARL15[ which(CARL15$chainend == 'Behind_F'), ]
#Stoppage_F
CARL15_SF <- CARL15[ which(CARL15$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL15_TF <- CARL15[ which(CARL15$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL15_SAM <- CARL15[ which(CARL15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL15_TAM <- CARL15[ which(CARL15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL15_SDM <- CARL15[ which(CARL15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL15_TDM <- CARL15[ which(CARL15$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL15_SD <- CARL15[ which(CARL15$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL15_TD <- CARL15[ which(CARL15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL15_QT <- CARL15[ which(CARL15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
CARL16_G <- CARL16[ which(CARL16$chainend == 'Goal_F'), ]
#Behind_F
CARL16_B <- CARL16[ which(CARL16$chainend == 'Behind_F'), ]
#Stoppage_F
CARL16_SF <- CARL16[ which(CARL16$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL16_TF <- CARL16[ which(CARL16$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL16_SAM <- CARL16[ which(CARL16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL16_TAM <- CARL16[ which(CARL16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL16_SDM <- CARL16[ which(CARL16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL16_TDM <- CARL16[ which(CARL16$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL16_SD <- CARL16[ which(CARL16$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL16_TD <- CARL16[ which(CARL16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL16_QT <- CARL16[ which(CARL16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
CARL17_G <- CARL17[ which(CARL17$chainend == 'Goal_F'), ]
#Behind_F
CARL17_B <- CARL17[ which(CARL17$chainend == 'Behind_F'), ]
#Stoppage_F
CARL17_SF <- CARL17[ which(CARL17$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL17_TF <- CARL17[ which(CARL17$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL17_SAM <- CARL17[ which(CARL17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL17_TAM <- CARL17[ which(CARL17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL17_SDM <- CARL17[ which(CARL17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL17_TDM <- CARL17[ which(CARL17$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL17_SD <- CARL17[ which(CARL17$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL17_TD <- CARL17[ which(CARL17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL17_QT <- CARL17[ which(CARL17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
CARL18_G <- CARL18[ which(CARL18$chainend == 'Goal_F'), ]
#Behind_F
CARL18_B <- CARL18[ which(CARL18$chainend == 'Behind_F'), ]
#Stoppage_F
CARL18_SF <- CARL18[ which(CARL18$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL18_TF <- CARL18[ which(CARL18$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL18_SAM <- CARL18[ which(CARL18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL18_TAM <- CARL18[ which(CARL18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL18_SDM <- CARL18[ which(CARL18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL18_TDM <- CARL18[ which(CARL18$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL18_SD <- CARL18[ which(CARL18$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL18_TD <- CARL18[ which(CARL18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL18_QT <- CARL18[ which(CARL18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
CARL19_G <- CARL19[ which(CARL19$chainend == 'Goal_F'), ]
#Behind_F
CARL19_B <- CARL19[ which(CARL19$chainend == 'Behind_F'), ]
#Stoppage_F
CARL19_SF <- CARL19[ which(CARL19$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL19_TF <- CARL19[ which(CARL19$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL19_SAM <- CARL19[ which(CARL19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL19_TAM <- CARL19[ which(CARL19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL19_SDM <- CARL19[ which(CARL19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL19_TDM <- CARL19[ which(CARL19$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL19_SD <- CARL19[ which(CARL19$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL19_TD <- CARL19[ which(CARL19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL19_QT <- CARL19[ which(CARL19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
CARL20_G <- CARL20[ which(CARL20$chainend == 'Goal_F'), ]
#Behind_F
CARL20_B <- CARL20[ which(CARL20$chainend == 'Behind_F'), ]
#Stoppage_F
CARL20_SF <- CARL20[ which(CARL20$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL20_TF <- CARL20[ which(CARL20$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL20_SAM <- CARL20[ which(CARL20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL20_TAM <- CARL20[ which(CARL20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL20_SDM <- CARL20[ which(CARL20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL20_TDM <- CARL20[ which(CARL20$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL20_SD <- CARL20[ which(CARL20$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL20_TD <- CARL20[ which(CARL20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL20_QT <- CARL20[ which(CARL20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
CARL21_G <- CARL21[ which(CARL21$chainend == 'Goal_F'), ]
#Behind_F
CARL21_B <- CARL21[ which(CARL21$chainend == 'Behind_F'), ]
#Stoppage_F
CARL21_SF <- CARL21[ which(CARL21$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL21_TF <- CARL21[ which(CARL21$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL21_SAM <- CARL21[ which(CARL21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL21_TAM <- CARL21[ which(CARL21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL21_SDM <- CARL21[ which(CARL21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL21_TDM <- CARL21[ which(CARL21$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL21_SD <- CARL21[ which(CARL21$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL21_TD <- CARL21[ which(CARL21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL21_QT <- CARL21[ which(CARL21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
CARL22_G <- CARL22[ which(CARL22$chainend == 'Goal_F'), ]
#Behind_F
CARL22_B <- CARL22[ which(CARL22$chainend == 'Behind_F'), ]
#Stoppage_F
CARL22_SF <- CARL22[ which(CARL22$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL22_TF <- CARL22[ which(CARL22$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL22_SAM <- CARL22[ which(CARL22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL22_TAM <- CARL22[ which(CARL22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL22_SDM <- CARL22[ which(CARL22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL22_TDM <- CARL22[ which(CARL22$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL22_SD <- CARL22[ which(CARL22$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL22_TD <- CARL22[ which(CARL22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL22_QT <- CARL22[ which(CARL22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
CARL23_G <- CARL23[ which(CARL23$chainend == 'Goal_F'), ]
#Behind_F
CARL23_B <- CARL23[ which(CARL23$chainend == 'Behind_F'), ]
#Stoppage_F
CARL23_SF <- CARL23[ which(CARL23$chainend == 'Stoppage_F'), ]
#Turnover_F
CARL23_TF <- CARL23[ which(CARL23$chainend == 'Turnover_F'), ]
#Stoppage_AM
CARL23_SAM <- CARL23[ which(CARL23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
CARL23_TAM <- CARL23[ which(CARL23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
CARL23_SDM <- CARL23[ which(CARL23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
CARL23_TDM <- CARL23[ which(CARL23$chainend == 'Turnover_DM'), ]
#Stoppage_D
CARL23_SD <- CARL23[ which(CARL23$chainend == 'Stoppage_D'), ]
#Turnover_D
CARL23_TD <- CARL23[ which(CARL23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
CARL23_QT <- CARL23[ which(CARL23$chainend == 'End of Qtr_DM'), ]

#Collingwood

#Split by rounds
COLL01 <- COLL[ which(COLL$round == '01'), ]
COLL02 <- COLL[ which(COLL$round == '02'), ]
COLL03 <- COLL[ which(COLL$round == '03'), ]
COLL04 <- COLL[ which(COLL$round == '04'), ]
COLL05 <- COLL[ which(COLL$round == '05'), ]
COLL06 <- COLL[ which(COLL$round == '06'), ]
COLL07 <- COLL[ which(COLL$round == '07'), ]
COLL08 <- COLL[ which(COLL$round == '08'), ]
COLL09 <- COLL[ which(COLL$round == '09'), ]
COLL10 <- COLL[ which(COLL$round == '10'), ]
COLL11 <- COLL[ which(COLL$round == '11'), ]
COLL12 <- COLL[ which(COLL$round == '12'), ]
COLL13 <- COLL[ which(COLL$round == '13'), ]
COLL14 <- COLL[ which(COLL$round == '14'), ]
COLL15 <- COLL[ which(COLL$round == '15'), ]
COLL16 <- COLL[ which(COLL$round == '16'), ]
COLL17 <- COLL[ which(COLL$round == '17'), ]
COLL18 <- COLL[ which(COLL$round == '18'), ]
COLL19 <- COLL[ which(COLL$round == '19'), ]
COLL20 <- COLL[ which(COLL$round == '20'), ]
COLL21 <- COLL[ which(COLL$round == '21'), ]
COLL22 <- COLL[ which(COLL$round == '22'), ]
COLL23 <- COLL[ which(COLL$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
COLL01_G <- COLL01[ which(COLL01$chainend == 'Goal_F'), ]
#Behind_F
COLL01_B <- COLL01[ which(COLL01$chainend == 'Behind_F'), ]
#Stoppage_F
COLL01_SF <- COLL01[ which(COLL01$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL01_TF <- COLL01[ which(COLL01$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL01_SAM <- COLL01[ which(COLL01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL01_TAM <- COLL01[ which(COLL01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL01_SDM <- COLL01[ which(COLL01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL01_TDM <- COLL01[ which(COLL01$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL01_SD <- COLL01[ which(COLL01$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL01_TD <- COLL01[ which(COLL01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL01_QT <- COLL01[ which(COLL01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
COLL02_G <- COLL02[ which(COLL02$chainend == 'Goal_F'), ]
#Behind_F
COLL02_B <- COLL02[ which(COLL02$chainend == 'Behind_F'), ]
#Stoppage_F
COLL02_SF <- COLL02[ which(COLL02$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL02_TF <- COLL02[ which(COLL02$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL02_SAM <- COLL02[ which(COLL02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL02_TAM <- COLL02[ which(COLL02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL02_SDM <- COLL02[ which(COLL02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL02_TDM <- COLL02[ which(COLL02$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL02_SD <- COLL02[ which(COLL02$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL02_TD <- COLL02[ which(COLL02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL02_QT <- COLL02[ which(COLL02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
COLL03_G <- COLL03[ which(COLL03$chainend == 'Goal_F'), ]
#Behind_F
COLL03_B <- COLL03[ which(COLL03$chainend == 'Behind_F'), ]
#Stoppage_F
COLL03_SF <- COLL03[ which(COLL03$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL03_TF <- COLL03[ which(COLL03$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL03_SAM <- COLL03[ which(COLL03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL03_TAM <- COLL03[ which(COLL03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL03_SDM <- COLL03[ which(COLL03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL03_TDM <- COLL03[ which(COLL03$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL03_SD <- COLL03[ which(COLL03$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL03_TD <- COLL03[ which(COLL03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL03_QT <- COLL03[ which(COLL03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
COLL04_G <- COLL04[ which(COLL04$chainend == 'Goal_F'), ]
#Behind_F
COLL04_B <- COLL04[ which(COLL04$chainend == 'Behind_F'), ]
#Stoppage_F
COLL04_SF <- COLL04[ which(COLL04$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL04_TF <- COLL04[ which(COLL04$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL04_SAM <- COLL04[ which(COLL04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL04_TAM <- COLL04[ which(COLL04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL04_SDM <- COLL04[ which(COLL04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL04_TDM <- COLL04[ which(COLL04$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL04_SD <- COLL04[ which(COLL04$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL04_TD <- COLL04[ which(COLL04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL04_QT <- COLL04[ which(COLL04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
COLL05_G <- COLL05[ which(COLL05$chainend == 'Goal_F'), ]
#Behind_F
COLL05_B <- COLL05[ which(COLL05$chainend == 'Behind_F'), ]
#Stoppage_F
COLL05_SF <- COLL05[ which(COLL05$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL05_TF <- COLL05[ which(COLL05$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL05_SAM <- COLL05[ which(COLL05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL05_TAM <- COLL05[ which(COLL05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL05_SDM <- COLL05[ which(COLL05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL05_TDM <- COLL05[ which(COLL05$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL05_SD <- COLL05[ which(COLL05$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL05_TD <- COLL05[ which(COLL05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL05_QT <- COLL05[ which(COLL05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
COLL06_G <- COLL06[ which(COLL06$chainend == 'Goal_F'), ]
#Behind_F
COLL06_B <- COLL06[ which(COLL06$chainend == 'Behind_F'), ]
#Stoppage_F
COLL06_SF <- COLL06[ which(COLL06$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL06_TF <- COLL06[ which(COLL06$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL06_SAM <- COLL06[ which(COLL06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL06_TAM <- COLL06[ which(COLL06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL06_SDM <- COLL06[ which(COLL06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL06_TDM <- COLL06[ which(COLL06$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL06_SD <- COLL06[ which(COLL06$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL06_TD <- COLL06[ which(COLL06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL06_QT <- COLL06[ which(COLL06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
COLL07_G <- COLL07[ which(COLL07$chainend == 'Goal_F'), ]
#Behind_F
COLL07_B <- COLL07[ which(COLL07$chainend == 'Behind_F'), ]
#Stoppage_F
COLL07_SF <- COLL07[ which(COLL07$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL07_TF <- COLL07[ which(COLL07$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL07_SAM <- COLL07[ which(COLL07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL07_TAM <- COLL07[ which(COLL07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL07_SDM <- COLL07[ which(COLL07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL07_TDM <- COLL07[ which(COLL07$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL07_SD <- COLL07[ which(COLL07$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL07_TD <- COLL07[ which(COLL07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL07_QT <- COLL07[ which(COLL07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
COLL08_G <- COLL08[ which(COLL08$chainend == 'Goal_F'), ]
#Behind_F
COLL08_B <- COLL08[ which(COLL08$chainend == 'Behind_F'), ]
#Stoppage_F
COLL08_SF <- COLL08[ which(COLL08$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL08_TF <- COLL08[ which(COLL08$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL08_SAM <- COLL08[ which(COLL08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL08_TAM <- COLL08[ which(COLL08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL08_SDM <- COLL08[ which(COLL08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL08_TDM <- COLL08[ which(COLL08$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL08_SD <- COLL08[ which(COLL08$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL08_TD <- COLL08[ which(COLL08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL08_QT <- COLL08[ which(COLL08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
COLL09_G <- COLL09[ which(COLL09$chainend == 'Goal_F'), ]
#Behind_F
COLL09_B <- COLL09[ which(COLL09$chainend == 'Behind_F'), ]
#Stoppage_F
COLL09_SF <- COLL09[ which(COLL09$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL09_TF <- COLL09[ which(COLL09$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL09_SAM <- COLL09[ which(COLL09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL09_TAM <- COLL09[ which(COLL09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL09_SDM <- COLL09[ which(COLL09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL09_TDM <- COLL09[ which(COLL09$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL09_SD <- COLL09[ which(COLL09$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL09_TD <- COLL09[ which(COLL09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL09_QT <- COLL09[ which(COLL09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
COLL10_G <- COLL10[ which(COLL10$chainend == 'Goal_F'), ]
#Behind_F
COLL10_B <- COLL10[ which(COLL10$chainend == 'Behind_F'), ]
#Stoppage_F
COLL10_SF <- COLL10[ which(COLL10$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL10_TF <- COLL10[ which(COLL10$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL10_SAM <- COLL10[ which(COLL10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL10_TAM <- COLL10[ which(COLL10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL10_SDM <- COLL10[ which(COLL10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL10_TDM <- COLL10[ which(COLL10$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL10_SD <- COLL10[ which(COLL10$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL10_TD <- COLL10[ which(COLL10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL10_QT <- COLL10[ which(COLL10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
COLL11_G <- COLL11[ which(COLL11$chainend == 'Goal_F'), ]
#Behind_F
COLL11_B <- COLL11[ which(COLL11$chainend == 'Behind_F'), ]
#Stoppage_F
COLL11_SF <- COLL11[ which(COLL11$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL11_TF <- COLL11[ which(COLL11$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL11_SAM <- COLL11[ which(COLL11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL11_TAM <- COLL11[ which(COLL11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL11_SDM <- COLL11[ which(COLL11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL11_TDM <- COLL11[ which(COLL11$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL11_SD <- COLL11[ which(COLL11$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL11_TD <- COLL11[ which(COLL11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL11_QT <- COLL11[ which(COLL11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
COLL12_G <- COLL12[ which(COLL12$chainend == 'Goal_F'), ]
#Behind_F
COLL12_B <- COLL12[ which(COLL12$chainend == 'Behind_F'), ]
#Stoppage_F
COLL12_SF <- COLL12[ which(COLL12$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL12_TF <- COLL12[ which(COLL12$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL12_SAM <- COLL12[ which(COLL12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL12_TAM <- COLL12[ which(COLL12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL12_SDM <- COLL12[ which(COLL12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL12_TDM <- COLL12[ which(COLL12$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL12_SD <- COLL12[ which(COLL12$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL12_TD <- COLL12[ which(COLL12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL12_QT <- COLL12[ which(COLL12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
COLL13_G <- COLL13[ which(COLL13$chainend == 'Goal_F'), ]
#Behind_F
COLL13_B <- COLL13[ which(COLL13$chainend == 'Behind_F'), ]
#Stoppage_F
COLL13_SF <- COLL13[ which(COLL13$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL13_TF <- COLL13[ which(COLL13$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL13_SAM <- COLL13[ which(COLL13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL13_TAM <- COLL13[ which(COLL13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL13_SDM <- COLL13[ which(COLL13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL13_TDM <- COLL13[ which(COLL13$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL13_SD <- COLL13[ which(COLL13$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL13_TD <- COLL13[ which(COLL13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL13_QT <- COLL13[ which(COLL13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
COLL14_G <- COLL14[ which(COLL14$chainend == 'Goal_F'), ]
#Behind_F
COLL14_B <- COLL14[ which(COLL14$chainend == 'Behind_F'), ]
#Stoppage_F
COLL14_SF <- COLL14[ which(COLL14$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL14_TF <- COLL14[ which(COLL14$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL14_SAM <- COLL14[ which(COLL14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL14_TAM <- COLL14[ which(COLL14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL14_SDM <- COLL14[ which(COLL14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL14_TDM <- COLL14[ which(COLL14$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL14_SD <- COLL14[ which(COLL14$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL14_TD <- COLL14[ which(COLL14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL14_QT <- COLL14[ which(COLL14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
COLL15_G <- COLL15[ which(COLL15$chainend == 'Goal_F'), ]
#Behind_F
COLL15_B <- COLL15[ which(COLL15$chainend == 'Behind_F'), ]
#Stoppage_F
COLL15_SF <- COLL15[ which(COLL15$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL15_TF <- COLL15[ which(COLL15$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL15_SAM <- COLL15[ which(COLL15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL15_TAM <- COLL15[ which(COLL15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL15_SDM <- COLL15[ which(COLL15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL15_TDM <- COLL15[ which(COLL15$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL15_SD <- COLL15[ which(COLL15$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL15_TD <- COLL15[ which(COLL15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL15_QT <- COLL15[ which(COLL15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
COLL16_G <- COLL16[ which(COLL16$chainend == 'Goal_F'), ]
#Behind_F
COLL16_B <- COLL16[ which(COLL16$chainend == 'Behind_F'), ]
#Stoppage_F
COLL16_SF <- COLL16[ which(COLL16$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL16_TF <- COLL16[ which(COLL16$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL16_SAM <- COLL16[ which(COLL16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL16_TAM <- COLL16[ which(COLL16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL16_SDM <- COLL16[ which(COLL16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL16_TDM <- COLL16[ which(COLL16$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL16_SD <- COLL16[ which(COLL16$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL16_TD <- COLL16[ which(COLL16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL16_QT <- COLL16[ which(COLL16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
COLL17_G <- COLL17[ which(COLL17$chainend == 'Goal_F'), ]
#Behind_F
COLL17_B <- COLL17[ which(COLL17$chainend == 'Behind_F'), ]
#Stoppage_F
COLL17_SF <- COLL17[ which(COLL17$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL17_TF <- COLL17[ which(COLL17$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL17_SAM <- COLL17[ which(COLL17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL17_TAM <- COLL17[ which(COLL17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL17_SDM <- COLL17[ which(COLL17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL17_TDM <- COLL17[ which(COLL17$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL17_SD <- COLL17[ which(COLL17$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL17_TD <- COLL17[ which(COLL17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL17_QT <- COLL17[ which(COLL17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
COLL18_G <- COLL18[ which(COLL18$chainend == 'Goal_F'), ]
#Behind_F
COLL18_B <- COLL18[ which(COLL18$chainend == 'Behind_F'), ]
#Stoppage_F
COLL18_SF <- COLL18[ which(COLL18$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL18_TF <- COLL18[ which(COLL18$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL18_SAM <- COLL18[ which(COLL18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL18_TAM <- COLL18[ which(COLL18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL18_SDM <- COLL18[ which(COLL18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL18_TDM <- COLL18[ which(COLL18$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL18_SD <- COLL18[ which(COLL18$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL18_TD <- COLL18[ which(COLL18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL18_QT <- COLL18[ which(COLL18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
COLL19_G <- COLL19[ which(COLL19$chainend == 'Goal_F'), ]
#Behind_F
COLL19_B <- COLL19[ which(COLL19$chainend == 'Behind_F'), ]
#Stoppage_F
COLL19_SF <- COLL19[ which(COLL19$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL19_TF <- COLL19[ which(COLL19$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL19_SAM <- COLL19[ which(COLL19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL19_TAM <- COLL19[ which(COLL19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL19_SDM <- COLL19[ which(COLL19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL19_TDM <- COLL19[ which(COLL19$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL19_SD <- COLL19[ which(COLL19$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL19_TD <- COLL19[ which(COLL19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL19_QT <- COLL19[ which(COLL19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
COLL20_G <- COLL20[ which(COLL20$chainend == 'Goal_F'), ]
#Behind_F
COLL20_B <- COLL20[ which(COLL20$chainend == 'Behind_F'), ]
#Stoppage_F
COLL20_SF <- COLL20[ which(COLL20$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL20_TF <- COLL20[ which(COLL20$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL20_SAM <- COLL20[ which(COLL20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL20_TAM <- COLL20[ which(COLL20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL20_SDM <- COLL20[ which(COLL20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL20_TDM <- COLL20[ which(COLL20$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL20_SD <- COLL20[ which(COLL20$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL20_TD <- COLL20[ which(COLL20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL20_QT <- COLL20[ which(COLL20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
COLL21_G <- COLL21[ which(COLL21$chainend == 'Goal_F'), ]
#Behind_F
COLL21_B <- COLL21[ which(COLL21$chainend == 'Behind_F'), ]
#Stoppage_F
COLL21_SF <- COLL21[ which(COLL21$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL21_TF <- COLL21[ which(COLL21$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL21_SAM <- COLL21[ which(COLL21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL21_TAM <- COLL21[ which(COLL21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL21_SDM <- COLL21[ which(COLL21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL21_TDM <- COLL21[ which(COLL21$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL21_SD <- COLL21[ which(COLL21$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL21_TD <- COLL21[ which(COLL21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL21_QT <- COLL21[ which(COLL21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
COLL22_G <- COLL22[ which(COLL22$chainend == 'Goal_F'), ]
#Behind_F
COLL22_B <- COLL22[ which(COLL22$chainend == 'Behind_F'), ]
#Stoppage_F
COLL22_SF <- COLL22[ which(COLL22$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL22_TF <- COLL22[ which(COLL22$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL22_SAM <- COLL22[ which(COLL22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL22_TAM <- COLL22[ which(COLL22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL22_SDM <- COLL22[ which(COLL22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL22_TDM <- COLL22[ which(COLL22$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL22_SD <- COLL22[ which(COLL22$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL22_TD <- COLL22[ which(COLL22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL22_QT <- COLL22[ which(COLL22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
COLL23_G <- COLL23[ which(COLL23$chainend == 'Goal_F'), ]
#Behind_F
COLL23_B <- COLL23[ which(COLL23$chainend == 'Behind_F'), ]
#Stoppage_F
COLL23_SF <- COLL23[ which(COLL23$chainend == 'Stoppage_F'), ]
#Turnover_F
COLL23_TF <- COLL23[ which(COLL23$chainend == 'Turnover_F'), ]
#Stoppage_AM
COLL23_SAM <- COLL23[ which(COLL23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
COLL23_TAM <- COLL23[ which(COLL23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
COLL23_SDM <- COLL23[ which(COLL23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
COLL23_TDM <- COLL23[ which(COLL23$chainend == 'Turnover_DM'), ]
#Stoppage_D
COLL23_SD <- COLL23[ which(COLL23$chainend == 'Stoppage_D'), ]
#Turnover_D
COLL23_TD <- COLL23[ which(COLL23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
COLL23_QT <- COLL23[ which(COLL23$chainend == 'End of Qtr_DM'), ]

#Essendon

#Split by rounds
ESS01 <- ESS[ which(ESS$round == '01'), ]
ESS02 <- ESS[ which(ESS$round == '02'), ]
ESS03 <- ESS[ which(ESS$round == '03'), ]
ESS04 <- ESS[ which(ESS$round == '04'), ]
ESS05 <- ESS[ which(ESS$round == '05'), ]
ESS06 <- ESS[ which(ESS$round == '06'), ]
ESS07 <- ESS[ which(ESS$round == '07'), ]
ESS08 <- ESS[ which(ESS$round == '08'), ]
ESS09 <- ESS[ which(ESS$round == '09'), ]
ESS10 <- ESS[ which(ESS$round == '10'), ]
ESS11 <- ESS[ which(ESS$round == '11'), ]
ESS12 <- ESS[ which(ESS$round == '12'), ]
ESS13 <- ESS[ which(ESS$round == '13'), ]
ESS14 <- ESS[ which(ESS$round == '14'), ]
ESS15 <- ESS[ which(ESS$round == '15'), ]
ESS16 <- ESS[ which(ESS$round == '16'), ]
ESS17 <- ESS[ which(ESS$round == '17'), ]
ESS18 <- ESS[ which(ESS$round == '18'), ]
ESS19 <- ESS[ which(ESS$round == '19'), ]
ESS20 <- ESS[ which(ESS$round == '20'), ]
ESS21 <- ESS[ which(ESS$round == '21'), ]
ESS22 <- ESS[ which(ESS$round == '22'), ]
ESS23 <- ESS[ which(ESS$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
ESS01_G <- ESS01[ which(ESS01$chainend == 'Goal_F'), ]
#Behind_F
ESS01_B <- ESS01[ which(ESS01$chainend == 'Behind_F'), ]
#Stoppage_F
ESS01_SF <- ESS01[ which(ESS01$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS01_TF <- ESS01[ which(ESS01$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS01_SAM <- ESS01[ which(ESS01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS01_TAM <- ESS01[ which(ESS01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS01_SDM <- ESS01[ which(ESS01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS01_TDM <- ESS01[ which(ESS01$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS01_SD <- ESS01[ which(ESS01$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS01_TD <- ESS01[ which(ESS01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS01_QT <- ESS01[ which(ESS01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
ESS02_G <- ESS02[ which(ESS02$chainend == 'Goal_F'), ]
#Behind_F
ESS02_B <- ESS02[ which(ESS02$chainend == 'Behind_F'), ]
#Stoppage_F
ESS02_SF <- ESS02[ which(ESS02$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS02_TF <- ESS02[ which(ESS02$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS02_SAM <- ESS02[ which(ESS02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS02_TAM <- ESS02[ which(ESS02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS02_SDM <- ESS02[ which(ESS02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS02_TDM <- ESS02[ which(ESS02$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS02_SD <- ESS02[ which(ESS02$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS02_TD <- ESS02[ which(ESS02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS02_QT <- ESS02[ which(ESS02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
ESS03_G <- ESS03[ which(ESS03$chainend == 'Goal_F'), ]
#Behind_F
ESS03_B <- ESS03[ which(ESS03$chainend == 'Behind_F'), ]
#Stoppage_F
ESS03_SF <- ESS03[ which(ESS03$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS03_TF <- ESS03[ which(ESS03$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS03_SAM <- ESS03[ which(ESS03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS03_TAM <- ESS03[ which(ESS03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS03_SDM <- ESS03[ which(ESS03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS03_TDM <- ESS03[ which(ESS03$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS03_SD <- ESS03[ which(ESS03$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS03_TD <- ESS03[ which(ESS03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS03_QT <- ESS03[ which(ESS03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
ESS04_G <- ESS04[ which(ESS04$chainend == 'Goal_F'), ]
#Behind_F
ESS04_B <- ESS04[ which(ESS04$chainend == 'Behind_F'), ]
#Stoppage_F
ESS04_SF <- ESS04[ which(ESS04$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS04_TF <- ESS04[ which(ESS04$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS04_SAM <- ESS04[ which(ESS04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS04_TAM <- ESS04[ which(ESS04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS04_SDM <- ESS04[ which(ESS04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS04_TDM <- ESS04[ which(ESS04$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS04_SD <- ESS04[ which(ESS04$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS04_TD <- ESS04[ which(ESS04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS04_QT <- ESS04[ which(ESS04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
ESS05_G <- ESS05[ which(ESS05$chainend == 'Goal_F'), ]
#Behind_F
ESS05_B <- ESS05[ which(ESS05$chainend == 'Behind_F'), ]
#Stoppage_F
ESS05_SF <- ESS05[ which(ESS05$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS05_TF <- ESS05[ which(ESS05$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS05_SAM <- ESS05[ which(ESS05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS05_TAM <- ESS05[ which(ESS05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS05_SDM <- ESS05[ which(ESS05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS05_TDM <- ESS05[ which(ESS05$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS05_SD <- ESS05[ which(ESS05$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS05_TD <- ESS05[ which(ESS05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS05_QT <- ESS05[ which(ESS05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
ESS06_G <- ESS06[ which(ESS06$chainend == 'Goal_F'), ]
#Behind_F
ESS06_B <- ESS06[ which(ESS06$chainend == 'Behind_F'), ]
#Stoppage_F
ESS06_SF <- ESS06[ which(ESS06$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS06_TF <- ESS06[ which(ESS06$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS06_SAM <- ESS06[ which(ESS06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS06_TAM <- ESS06[ which(ESS06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS06_SDM <- ESS06[ which(ESS06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS06_TDM <- ESS06[ which(ESS06$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS06_SD <- ESS06[ which(ESS06$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS06_TD <- ESS06[ which(ESS06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS06_QT <- ESS06[ which(ESS06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
ESS07_G <- ESS07[ which(ESS07$chainend == 'Goal_F'), ]
#Behind_F
ESS07_B <- ESS07[ which(ESS07$chainend == 'Behind_F'), ]
#Stoppage_F
ESS07_SF <- ESS07[ which(ESS07$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS07_TF <- ESS07[ which(ESS07$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS07_SAM <- ESS07[ which(ESS07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS07_TAM <- ESS07[ which(ESS07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS07_SDM <- ESS07[ which(ESS07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS07_TDM <- ESS07[ which(ESS07$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS07_SD <- ESS07[ which(ESS07$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS07_TD <- ESS07[ which(ESS07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS07_QT <- ESS07[ which(ESS07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
ESS08_G <- ESS08[ which(ESS08$chainend == 'Goal_F'), ]
#Behind_F
ESS08_B <- ESS08[ which(ESS08$chainend == 'Behind_F'), ]
#Stoppage_F
ESS08_SF <- ESS08[ which(ESS08$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS08_TF <- ESS08[ which(ESS08$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS08_SAM <- ESS08[ which(ESS08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS08_TAM <- ESS08[ which(ESS08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS08_SDM <- ESS08[ which(ESS08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS08_TDM <- ESS08[ which(ESS08$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS08_SD <- ESS08[ which(ESS08$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS08_TD <- ESS08[ which(ESS08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS08_QT <- ESS08[ which(ESS08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
ESS09_G <- ESS09[ which(ESS09$chainend == 'Goal_F'), ]
#Behind_F
ESS09_B <- ESS09[ which(ESS09$chainend == 'Behind_F'), ]
#Stoppage_F
ESS09_SF <- ESS09[ which(ESS09$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS09_TF <- ESS09[ which(ESS09$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS09_SAM <- ESS09[ which(ESS09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS09_TAM <- ESS09[ which(ESS09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS09_SDM <- ESS09[ which(ESS09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS09_TDM <- ESS09[ which(ESS09$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS09_SD <- ESS09[ which(ESS09$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS09_TD <- ESS09[ which(ESS09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS09_QT <- ESS09[ which(ESS09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
ESS10_G <- ESS10[ which(ESS10$chainend == 'Goal_F'), ]
#Behind_F
ESS10_B <- ESS10[ which(ESS10$chainend == 'Behind_F'), ]
#Stoppage_F
ESS10_SF <- ESS10[ which(ESS10$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS10_TF <- ESS10[ which(ESS10$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS10_SAM <- ESS10[ which(ESS10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS10_TAM <- ESS10[ which(ESS10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS10_SDM <- ESS10[ which(ESS10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS10_TDM <- ESS10[ which(ESS10$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS10_SD <- ESS10[ which(ESS10$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS10_TD <- ESS10[ which(ESS10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS10_QT <- ESS10[ which(ESS10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
ESS11_G <- ESS11[ which(ESS11$chainend == 'Goal_F'), ]
#Behind_F
ESS11_B <- ESS11[ which(ESS11$chainend == 'Behind_F'), ]
#Stoppage_F
ESS11_SF <- ESS11[ which(ESS11$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS11_TF <- ESS11[ which(ESS11$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS11_SAM <- ESS11[ which(ESS11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS11_TAM <- ESS11[ which(ESS11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS11_SDM <- ESS11[ which(ESS11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS11_TDM <- ESS11[ which(ESS11$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS11_SD <- ESS11[ which(ESS11$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS11_TD <- ESS11[ which(ESS11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS11_QT <- ESS11[ which(ESS11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
ESS12_G <- ESS12[ which(ESS12$chainend == 'Goal_F'), ]
#Behind_F
ESS12_B <- ESS12[ which(ESS12$chainend == 'Behind_F'), ]
#Stoppage_F
ESS12_SF <- ESS12[ which(ESS12$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS12_TF <- ESS12[ which(ESS12$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS12_SAM <- ESS12[ which(ESS12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS12_TAM <- ESS12[ which(ESS12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS12_SDM <- ESS12[ which(ESS12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS12_TDM <- ESS12[ which(ESS12$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS12_SD <- ESS12[ which(ESS12$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS12_TD <- ESS12[ which(ESS12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS12_QT <- ESS12[ which(ESS12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
ESS13_G <- ESS13[ which(ESS13$chainend == 'Goal_F'), ]
#Behind_F
ESS13_B <- ESS13[ which(ESS13$chainend == 'Behind_F'), ]
#Stoppage_F
ESS13_SF <- ESS13[ which(ESS13$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS13_TF <- ESS13[ which(ESS13$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS13_SAM <- ESS13[ which(ESS13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS13_TAM <- ESS13[ which(ESS13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS13_SDM <- ESS13[ which(ESS13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS13_TDM <- ESS13[ which(ESS13$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS13_SD <- ESS13[ which(ESS13$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS13_TD <- ESS13[ which(ESS13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS13_QT <- ESS13[ which(ESS13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
ESS14_G <- ESS14[ which(ESS14$chainend == 'Goal_F'), ]
#Behind_F
ESS14_B <- ESS14[ which(ESS14$chainend == 'Behind_F'), ]
#Stoppage_F
ESS14_SF <- ESS14[ which(ESS14$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS14_TF <- ESS14[ which(ESS14$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS14_SAM <- ESS14[ which(ESS14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS14_TAM <- ESS14[ which(ESS14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS14_SDM <- ESS14[ which(ESS14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS14_TDM <- ESS14[ which(ESS14$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS14_SD <- ESS14[ which(ESS14$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS14_TD <- ESS14[ which(ESS14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS14_QT <- ESS14[ which(ESS14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
ESS15_G <- ESS15[ which(ESS15$chainend == 'Goal_F'), ]
#Behind_F
ESS15_B <- ESS15[ which(ESS15$chainend == 'Behind_F'), ]
#Stoppage_F
ESS15_SF <- ESS15[ which(ESS15$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS15_TF <- ESS15[ which(ESS15$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS15_SAM <- ESS15[ which(ESS15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS15_TAM <- ESS15[ which(ESS15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS15_SDM <- ESS15[ which(ESS15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS15_TDM <- ESS15[ which(ESS15$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS15_SD <- ESS15[ which(ESS15$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS15_TD <- ESS15[ which(ESS15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS15_QT <- ESS15[ which(ESS15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
ESS16_G <- ESS16[ which(ESS16$chainend == 'Goal_F'), ]
#Behind_F
ESS16_B <- ESS16[ which(ESS16$chainend == 'Behind_F'), ]
#Stoppage_F
ESS16_SF <- ESS16[ which(ESS16$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS16_TF <- ESS16[ which(ESS16$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS16_SAM <- ESS16[ which(ESS16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS16_TAM <- ESS16[ which(ESS16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS16_SDM <- ESS16[ which(ESS16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS16_TDM <- ESS16[ which(ESS16$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS16_SD <- ESS16[ which(ESS16$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS16_TD <- ESS16[ which(ESS16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS16_QT <- ESS16[ which(ESS16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
ESS17_G <- ESS17[ which(ESS17$chainend == 'Goal_F'), ]
#Behind_F
ESS17_B <- ESS17[ which(ESS17$chainend == 'Behind_F'), ]
#Stoppage_F
ESS17_SF <- ESS17[ which(ESS17$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS17_TF <- ESS17[ which(ESS17$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS17_SAM <- ESS17[ which(ESS17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS17_TAM <- ESS17[ which(ESS17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS17_SDM <- ESS17[ which(ESS17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS17_TDM <- ESS17[ which(ESS17$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS17_SD <- ESS17[ which(ESS17$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS17_TD <- ESS17[ which(ESS17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS17_QT <- ESS17[ which(ESS17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
ESS18_G <- ESS18[ which(ESS18$chainend == 'Goal_F'), ]
#Behind_F
ESS18_B <- ESS18[ which(ESS18$chainend == 'Behind_F'), ]
#Stoppage_F
ESS18_SF <- ESS18[ which(ESS18$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS18_TF <- ESS18[ which(ESS18$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS18_SAM <- ESS18[ which(ESS18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS18_TAM <- ESS18[ which(ESS18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS18_SDM <- ESS18[ which(ESS18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS18_TDM <- ESS18[ which(ESS18$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS18_SD <- ESS18[ which(ESS18$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS18_TD <- ESS18[ which(ESS18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS18_QT <- ESS18[ which(ESS18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
ESS19_G <- ESS19[ which(ESS19$chainend == 'Goal_F'), ]
#Behind_F
ESS19_B <- ESS19[ which(ESS19$chainend == 'Behind_F'), ]
#Stoppage_F
ESS19_SF <- ESS19[ which(ESS19$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS19_TF <- ESS19[ which(ESS19$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS19_SAM <- ESS19[ which(ESS19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS19_TAM <- ESS19[ which(ESS19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS19_SDM <- ESS19[ which(ESS19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS19_TDM <- ESS19[ which(ESS19$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS19_SD <- ESS19[ which(ESS19$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS19_TD <- ESS19[ which(ESS19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS19_QT <- ESS19[ which(ESS19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
ESS20_G <- ESS20[ which(ESS20$chainend == 'Goal_F'), ]
#Behind_F
ESS20_B <- ESS20[ which(ESS20$chainend == 'Behind_F'), ]
#Stoppage_F
ESS20_SF <- ESS20[ which(ESS20$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS20_TF <- ESS20[ which(ESS20$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS20_SAM <- ESS20[ which(ESS20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS20_TAM <- ESS20[ which(ESS20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS20_SDM <- ESS20[ which(ESS20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS20_TDM <- ESS20[ which(ESS20$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS20_SD <- ESS20[ which(ESS20$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS20_TD <- ESS20[ which(ESS20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS20_QT <- ESS20[ which(ESS20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
ESS21_G <- ESS21[ which(ESS21$chainend == 'Goal_F'), ]
#Behind_F
ESS21_B <- ESS21[ which(ESS21$chainend == 'Behind_F'), ]
#Stoppage_F
ESS21_SF <- ESS21[ which(ESS21$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS21_TF <- ESS21[ which(ESS21$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS21_SAM <- ESS21[ which(ESS21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS21_TAM <- ESS21[ which(ESS21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS21_SDM <- ESS21[ which(ESS21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS21_TDM <- ESS21[ which(ESS21$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS21_SD <- ESS21[ which(ESS21$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS21_TD <- ESS21[ which(ESS21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS21_QT <- ESS21[ which(ESS21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
ESS22_G <- ESS22[ which(ESS22$chainend == 'Goal_F'), ]
#Behind_F
ESS22_B <- ESS22[ which(ESS22$chainend == 'Behind_F'), ]
#Stoppage_F
ESS22_SF <- ESS22[ which(ESS22$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS22_TF <- ESS22[ which(ESS22$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS22_SAM <- ESS22[ which(ESS22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS22_TAM <- ESS22[ which(ESS22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS22_SDM <- ESS22[ which(ESS22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS22_TDM <- ESS22[ which(ESS22$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS22_SD <- ESS22[ which(ESS22$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS22_TD <- ESS22[ which(ESS22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS22_QT <- ESS22[ which(ESS22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
ESS23_G <- ESS23[ which(ESS23$chainend == 'Goal_F'), ]
#Behind_F
ESS23_B <- ESS23[ which(ESS23$chainend == 'Behind_F'), ]
#Stoppage_F
ESS23_SF <- ESS23[ which(ESS23$chainend == 'Stoppage_F'), ]
#Turnover_F
ESS23_TF <- ESS23[ which(ESS23$chainend == 'Turnover_F'), ]
#Stoppage_AM
ESS23_SAM <- ESS23[ which(ESS23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
ESS23_TAM <- ESS23[ which(ESS23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
ESS23_SDM <- ESS23[ which(ESS23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
ESS23_TDM <- ESS23[ which(ESS23$chainend == 'Turnover_DM'), ]
#Stoppage_D
ESS23_SD <- ESS23[ which(ESS23$chainend == 'Stoppage_D'), ]
#Turnover_D
ESS23_TD <- ESS23[ which(ESS23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
ESS23_QT <- ESS23[ which(ESS23$chainend == 'End of Qtr_DM'), ]

#Fremantle

#Split by rounds
FRE01 <- FRE[ which(FRE$round == '01'), ]
FRE02 <- FRE[ which(FRE$round == '02'), ]
FRE03 <- FRE[ which(FRE$round == '03'), ]
FRE04 <- FRE[ which(FRE$round == '04'), ]
FRE05 <- FRE[ which(FRE$round == '05'), ]
FRE06 <- FRE[ which(FRE$round == '06'), ]
FRE07 <- FRE[ which(FRE$round == '07'), ]
FRE08 <- FRE[ which(FRE$round == '08'), ]
FRE09 <- FRE[ which(FRE$round == '09'), ]
FRE10 <- FRE[ which(FRE$round == '10'), ]
FRE11 <- FRE[ which(FRE$round == '11'), ]
FRE12 <- FRE[ which(FRE$round == '12'), ]
FRE13 <- FRE[ which(FRE$round == '13'), ]
FRE14 <- FRE[ which(FRE$round == '14'), ]
FRE15 <- FRE[ which(FRE$round == '15'), ]
FRE16 <- FRE[ which(FRE$round == '16'), ]
FRE17 <- FRE[ which(FRE$round == '17'), ]
FRE18 <- FRE[ which(FRE$round == '18'), ]
FRE19 <- FRE[ which(FRE$round == '19'), ]
FRE20 <- FRE[ which(FRE$round == '20'), ]
FRE21 <- FRE[ which(FRE$round == '21'), ]
FRE22 <- FRE[ which(FRE$round == '22'), ]
FRE23 <- FRE[ which(FRE$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
FRE01_G <- FRE01[ which(FRE01$chainend == 'Goal_F'), ]
#Behind_F
FRE01_B <- FRE01[ which(FRE01$chainend == 'Behind_F'), ]
#Stoppage_F
FRE01_SF <- FRE01[ which(FRE01$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE01_TF <- FRE01[ which(FRE01$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE01_SAM <- FRE01[ which(FRE01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE01_TAM <- FRE01[ which(FRE01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE01_SDM <- FRE01[ which(FRE01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE01_TDM <- FRE01[ which(FRE01$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE01_SD <- FRE01[ which(FRE01$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE01_TD <- FRE01[ which(FRE01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE01_QT <- FRE01[ which(FRE01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
FRE02_G <- FRE02[ which(FRE02$chainend == 'Goal_F'), ]
#Behind_F
FRE02_B <- FRE02[ which(FRE02$chainend == 'Behind_F'), ]
#Stoppage_F
FRE02_SF <- FRE02[ which(FRE02$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE02_TF <- FRE02[ which(FRE02$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE02_SAM <- FRE02[ which(FRE02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE02_TAM <- FRE02[ which(FRE02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE02_SDM <- FRE02[ which(FRE02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE02_TDM <- FRE02[ which(FRE02$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE02_SD <- FRE02[ which(FRE02$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE02_TD <- FRE02[ which(FRE02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE02_QT <- FRE02[ which(FRE02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
FRE03_G <- FRE03[ which(FRE03$chainend == 'Goal_F'), ]
#Behind_F
FRE03_B <- FRE03[ which(FRE03$chainend == 'Behind_F'), ]
#Stoppage_F
FRE03_SF <- FRE03[ which(FRE03$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE03_TF <- FRE03[ which(FRE03$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE03_SAM <- FRE03[ which(FRE03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE03_TAM <- FRE03[ which(FRE03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE03_SDM <- FRE03[ which(FRE03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE03_TDM <- FRE03[ which(FRE03$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE03_SD <- FRE03[ which(FRE03$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE03_TD <- FRE03[ which(FRE03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE03_QT <- FRE03[ which(FRE03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
FRE04_G <- FRE04[ which(FRE04$chainend == 'Goal_F'), ]
#Behind_F
FRE04_B <- FRE04[ which(FRE04$chainend == 'Behind_F'), ]
#Stoppage_F
FRE04_SF <- FRE04[ which(FRE04$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE04_TF <- FRE04[ which(FRE04$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE04_SAM <- FRE04[ which(FRE04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE04_TAM <- FRE04[ which(FRE04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE04_SDM <- FRE04[ which(FRE04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE04_TDM <- FRE04[ which(FRE04$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE04_SD <- FRE04[ which(FRE04$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE04_TD <- FRE04[ which(FRE04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE04_QT <- FRE04[ which(FRE04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
FRE05_G <- FRE05[ which(FRE05$chainend == 'Goal_F'), ]
#Behind_F
FRE05_B <- FRE05[ which(FRE05$chainend == 'Behind_F'), ]
#Stoppage_F
FRE05_SF <- FRE05[ which(FRE05$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE05_TF <- FRE05[ which(FRE05$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE05_SAM <- FRE05[ which(FRE05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE05_TAM <- FRE05[ which(FRE05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE05_SDM <- FRE05[ which(FRE05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE05_TDM <- FRE05[ which(FRE05$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE05_SD <- FRE05[ which(FRE05$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE05_TD <- FRE05[ which(FRE05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE05_QT <- FRE05[ which(FRE05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
FRE06_G <- FRE06[ which(FRE06$chainend == 'Goal_F'), ]
#Behind_F
FRE06_B <- FRE06[ which(FRE06$chainend == 'Behind_F'), ]
#Stoppage_F
FRE06_SF <- FRE06[ which(FRE06$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE06_TF <- FRE06[ which(FRE06$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE06_SAM <- FRE06[ which(FRE06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE06_TAM <- FRE06[ which(FRE06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE06_SDM <- FRE06[ which(FRE06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE06_TDM <- FRE06[ which(FRE06$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE06_SD <- FRE06[ which(FRE06$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE06_TD <- FRE06[ which(FRE06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE06_QT <- FRE06[ which(FRE06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
FRE07_G <- FRE07[ which(FRE07$chainend == 'Goal_F'), ]
#Behind_F
FRE07_B <- FRE07[ which(FRE07$chainend == 'Behind_F'), ]
#Stoppage_F
FRE07_SF <- FRE07[ which(FRE07$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE07_TF <- FRE07[ which(FRE07$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE07_SAM <- FRE07[ which(FRE07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE07_TAM <- FRE07[ which(FRE07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE07_SDM <- FRE07[ which(FRE07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE07_TDM <- FRE07[ which(FRE07$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE07_SD <- FRE07[ which(FRE07$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE07_TD <- FRE07[ which(FRE07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE07_QT <- FRE07[ which(FRE07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
FRE08_G <- FRE08[ which(FRE08$chainend == 'Goal_F'), ]
#Behind_F
FRE08_B <- FRE08[ which(FRE08$chainend == 'Behind_F'), ]
#Stoppage_F
FRE08_SF <- FRE08[ which(FRE08$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE08_TF <- FRE08[ which(FRE08$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE08_SAM <- FRE08[ which(FRE08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE08_TAM <- FRE08[ which(FRE08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE08_SDM <- FRE08[ which(FRE08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE08_TDM <- FRE08[ which(FRE08$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE08_SD <- FRE08[ which(FRE08$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE08_TD <- FRE08[ which(FRE08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE08_QT <- FRE08[ which(FRE08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
FRE09_G <- FRE09[ which(FRE09$chainend == 'Goal_F'), ]
#Behind_F
FRE09_B <- FRE09[ which(FRE09$chainend == 'Behind_F'), ]
#Stoppage_F
FRE09_SF <- FRE09[ which(FRE09$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE09_TF <- FRE09[ which(FRE09$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE09_SAM <- FRE09[ which(FRE09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE09_TAM <- FRE09[ which(FRE09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE09_SDM <- FRE09[ which(FRE09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE09_TDM <- FRE09[ which(FRE09$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE09_SD <- FRE09[ which(FRE09$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE09_TD <- FRE09[ which(FRE09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE09_QT <- FRE09[ which(FRE09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
FRE10_G <- FRE10[ which(FRE10$chainend == 'Goal_F'), ]
#Behind_F
FRE10_B <- FRE10[ which(FRE10$chainend == 'Behind_F'), ]
#Stoppage_F
FRE10_SF <- FRE10[ which(FRE10$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE10_TF <- FRE10[ which(FRE10$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE10_SAM <- FRE10[ which(FRE10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE10_TAM <- FRE10[ which(FRE10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE10_SDM <- FRE10[ which(FRE10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE10_TDM <- FRE10[ which(FRE10$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE10_SD <- FRE10[ which(FRE10$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE10_TD <- FRE10[ which(FRE10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE10_QT <- FRE10[ which(FRE10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
FRE11_G <- FRE11[ which(FRE11$chainend == 'Goal_F'), ]
#Behind_F
FRE11_B <- FRE11[ which(FRE11$chainend == 'Behind_F'), ]
#Stoppage_F
FRE11_SF <- FRE11[ which(FRE11$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE11_TF <- FRE11[ which(FRE11$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE11_SAM <- FRE11[ which(FRE11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE11_TAM <- FRE11[ which(FRE11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE11_SDM <- FRE11[ which(FRE11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE11_TDM <- FRE11[ which(FRE11$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE11_SD <- FRE11[ which(FRE11$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE11_TD <- FRE11[ which(FRE11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE11_QT <- FRE11[ which(FRE11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
FRE12_G <- FRE12[ which(FRE12$chainend == 'Goal_F'), ]
#Behind_F
FRE12_B <- FRE12[ which(FRE12$chainend == 'Behind_F'), ]
#Stoppage_F
FRE12_SF <- FRE12[ which(FRE12$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE12_TF <- FRE12[ which(FRE12$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE12_SAM <- FRE12[ which(FRE12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE12_TAM <- FRE12[ which(FRE12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE12_SDM <- FRE12[ which(FRE12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE12_TDM <- FRE12[ which(FRE12$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE12_SD <- FRE12[ which(FRE12$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE12_TD <- FRE12[ which(FRE12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE12_QT <- FRE12[ which(FRE12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
FRE13_G <- FRE13[ which(FRE13$chainend == 'Goal_F'), ]
#Behind_F
FRE13_B <- FRE13[ which(FRE13$chainend == 'Behind_F'), ]
#Stoppage_F
FRE13_SF <- FRE13[ which(FRE13$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE13_TF <- FRE13[ which(FRE13$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE13_SAM <- FRE13[ which(FRE13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE13_TAM <- FRE13[ which(FRE13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE13_SDM <- FRE13[ which(FRE13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE13_TDM <- FRE13[ which(FRE13$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE13_SD <- FRE13[ which(FRE13$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE13_TD <- FRE13[ which(FRE13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE13_QT <- FRE13[ which(FRE13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
FRE14_G <- FRE14[ which(FRE14$chainend == 'Goal_F'), ]
#Behind_F
FRE14_B <- FRE14[ which(FRE14$chainend == 'Behind_F'), ]
#Stoppage_F
FRE14_SF <- FRE14[ which(FRE14$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE14_TF <- FRE14[ which(FRE14$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE14_SAM <- FRE14[ which(FRE14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE14_TAM <- FRE14[ which(FRE14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE14_SDM <- FRE14[ which(FRE14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE14_TDM <- FRE14[ which(FRE14$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE14_SD <- FRE14[ which(FRE14$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE14_TD <- FRE14[ which(FRE14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE14_QT <- FRE14[ which(FRE14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
FRE15_G <- FRE15[ which(FRE15$chainend == 'Goal_F'), ]
#Behind_F
FRE15_B <- FRE15[ which(FRE15$chainend == 'Behind_F'), ]
#Stoppage_F
FRE15_SF <- FRE15[ which(FRE15$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE15_TF <- FRE15[ which(FRE15$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE15_SAM <- FRE15[ which(FRE15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE15_TAM <- FRE15[ which(FRE15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE15_SDM <- FRE15[ which(FRE15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE15_TDM <- FRE15[ which(FRE15$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE15_SD <- FRE15[ which(FRE15$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE15_TD <- FRE15[ which(FRE15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE15_QT <- FRE15[ which(FRE15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
FRE16_G <- FRE16[ which(FRE16$chainend == 'Goal_F'), ]
#Behind_F
FRE16_B <- FRE16[ which(FRE16$chainend == 'Behind_F'), ]
#Stoppage_F
FRE16_SF <- FRE16[ which(FRE16$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE16_TF <- FRE16[ which(FRE16$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE16_SAM <- FRE16[ which(FRE16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE16_TAM <- FRE16[ which(FRE16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE16_SDM <- FRE16[ which(FRE16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE16_TDM <- FRE16[ which(FRE16$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE16_SD <- FRE16[ which(FRE16$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE16_TD <- FRE16[ which(FRE16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE16_QT <- FRE16[ which(FRE16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
FRE17_G <- FRE17[ which(FRE17$chainend == 'Goal_F'), ]
#Behind_F
FRE17_B <- FRE17[ which(FRE17$chainend == 'Behind_F'), ]
#Stoppage_F
FRE17_SF <- FRE17[ which(FRE17$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE17_TF <- FRE17[ which(FRE17$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE17_SAM <- FRE17[ which(FRE17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE17_TAM <- FRE17[ which(FRE17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE17_SDM <- FRE17[ which(FRE17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE17_TDM <- FRE17[ which(FRE17$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE17_SD <- FRE17[ which(FRE17$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE17_TD <- FRE17[ which(FRE17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE17_QT <- FRE17[ which(FRE17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
FRE18_G <- FRE18[ which(FRE18$chainend == 'Goal_F'), ]
#Behind_F
FRE18_B <- FRE18[ which(FRE18$chainend == 'Behind_F'), ]
#Stoppage_F
FRE18_SF <- FRE18[ which(FRE18$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE18_TF <- FRE18[ which(FRE18$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE18_SAM <- FRE18[ which(FRE18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE18_TAM <- FRE18[ which(FRE18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE18_SDM <- FRE18[ which(FRE18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE18_TDM <- FRE18[ which(FRE18$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE18_SD <- FRE18[ which(FRE18$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE18_TD <- FRE18[ which(FRE18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE18_QT <- FRE18[ which(FRE18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
FRE19_G <- FRE19[ which(FRE19$chainend == 'Goal_F'), ]
#Behind_F
FRE19_B <- FRE19[ which(FRE19$chainend == 'Behind_F'), ]
#Stoppage_F
FRE19_SF <- FRE19[ which(FRE19$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE19_TF <- FRE19[ which(FRE19$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE19_SAM <- FRE19[ which(FRE19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE19_TAM <- FRE19[ which(FRE19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE19_SDM <- FRE19[ which(FRE19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE19_TDM <- FRE19[ which(FRE19$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE19_SD <- FRE19[ which(FRE19$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE19_TD <- FRE19[ which(FRE19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE19_QT <- FRE19[ which(FRE19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
FRE20_G <- FRE20[ which(FRE20$chainend == 'Goal_F'), ]
#Behind_F
FRE20_B <- FRE20[ which(FRE20$chainend == 'Behind_F'), ]
#Stoppage_F
FRE20_SF <- FRE20[ which(FRE20$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE20_TF <- FRE20[ which(FRE20$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE20_SAM <- FRE20[ which(FRE20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE20_TAM <- FRE20[ which(FRE20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE20_SDM <- FRE20[ which(FRE20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE20_TDM <- FRE20[ which(FRE20$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE20_SD <- FRE20[ which(FRE20$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE20_TD <- FRE20[ which(FRE20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE20_QT <- FRE20[ which(FRE20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
FRE21_G <- FRE21[ which(FRE21$chainend == 'Goal_F'), ]
#Behind_F
FRE21_B <- FRE21[ which(FRE21$chainend == 'Behind_F'), ]
#Stoppage_F
FRE21_SF <- FRE21[ which(FRE21$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE21_TF <- FRE21[ which(FRE21$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE21_SAM <- FRE21[ which(FRE21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE21_TAM <- FRE21[ which(FRE21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE21_SDM <- FRE21[ which(FRE21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE21_TDM <- FRE21[ which(FRE21$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE21_SD <- FRE21[ which(FRE21$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE21_TD <- FRE21[ which(FRE21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE21_QT <- FRE21[ which(FRE21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
FRE22_G <- FRE22[ which(FRE22$chainend == 'Goal_F'), ]
#Behind_F
FRE22_B <- FRE22[ which(FRE22$chainend == 'Behind_F'), ]
#Stoppage_F
FRE22_SF <- FRE22[ which(FRE22$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE22_TF <- FRE22[ which(FRE22$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE22_SAM <- FRE22[ which(FRE22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE22_TAM <- FRE22[ which(FRE22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE22_SDM <- FRE22[ which(FRE22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE22_TDM <- FRE22[ which(FRE22$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE22_SD <- FRE22[ which(FRE22$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE22_TD <- FRE22[ which(FRE22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE22_QT <- FRE22[ which(FRE22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
FRE23_G <- FRE23[ which(FRE23$chainend == 'Goal_F'), ]
#Behind_F
FRE23_B <- FRE23[ which(FRE23$chainend == 'Behind_F'), ]
#Stoppage_F
FRE23_SF <- FRE23[ which(FRE23$chainend == 'Stoppage_F'), ]
#Turnover_F
FRE23_TF <- FRE23[ which(FRE23$chainend == 'Turnover_F'), ]
#Stoppage_AM
FRE23_SAM <- FRE23[ which(FRE23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
FRE23_TAM <- FRE23[ which(FRE23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
FRE23_SDM <- FRE23[ which(FRE23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
FRE23_TDM <- FRE23[ which(FRE23$chainend == 'Turnover_DM'), ]
#Stoppage_D
FRE23_SD <- FRE23[ which(FRE23$chainend == 'Stoppage_D'), ]
#Turnover_D
FRE23_TD <- FRE23[ which(FRE23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
FRE23_QT <- FRE23[ which(FRE23$chainend == 'End of Qtr_DM'), ]

#Gold Coast

#Split by rounds
GCFC01 <- GCFC[ which(GCFC$round == '01'), ]
GCFC02 <- GCFC[ which(GCFC$round == '02'), ]
GCFC03 <- GCFC[ which(GCFC$round == '03'), ]
GCFC04 <- GCFC[ which(GCFC$round == '04'), ]
GCFC05 <- GCFC[ which(GCFC$round == '05'), ]
GCFC06 <- GCFC[ which(GCFC$round == '06'), ]
GCFC07 <- GCFC[ which(GCFC$round == '07'), ]
GCFC08 <- GCFC[ which(GCFC$round == '08'), ]
GCFC09 <- GCFC[ which(GCFC$round == '09'), ]
GCFC10 <- GCFC[ which(GCFC$round == '10'), ]
GCFC11 <- GCFC[ which(GCFC$round == '11'), ]
GCFC12 <- GCFC[ which(GCFC$round == '12'), ]
GCFC13 <- GCFC[ which(GCFC$round == '13'), ]
GCFC14 <- GCFC[ which(GCFC$round == '14'), ]
GCFC15 <- GCFC[ which(GCFC$round == '15'), ]
GCFC16 <- GCFC[ which(GCFC$round == '16'), ]
GCFC17 <- GCFC[ which(GCFC$round == '17'), ]
GCFC18 <- GCFC[ which(GCFC$round == '18'), ]
GCFC19 <- GCFC[ which(GCFC$round == '19'), ]
GCFC20 <- GCFC[ which(GCFC$round == '20'), ]
GCFC21 <- GCFC[ which(GCFC$round == '21'), ]
GCFC22 <- GCFC[ which(GCFC$round == '22'), ]
GCFC23 <- GCFC[ which(GCFC$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
GCFC01_G <- GCFC01[ which(GCFC01$chainend == 'Goal_F'), ]
#Behind_F
GCFC01_B <- GCFC01[ which(GCFC01$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC01_SF <- GCFC01[ which(GCFC01$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC01_TF <- GCFC01[ which(GCFC01$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC01_SAM <- GCFC01[ which(GCFC01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC01_TAM <- GCFC01[ which(GCFC01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC01_SDM <- GCFC01[ which(GCFC01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC01_TDM <- GCFC01[ which(GCFC01$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC01_SD <- GCFC01[ which(GCFC01$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC01_TD <- GCFC01[ which(GCFC01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC01_QT <- GCFC01[ which(GCFC01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
GCFC02_G <- GCFC02[ which(GCFC02$chainend == 'Goal_F'), ]
#Behind_F
GCFC02_B <- GCFC02[ which(GCFC02$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC02_SF <- GCFC02[ which(GCFC02$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC02_TF <- GCFC02[ which(GCFC02$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC02_SAM <- GCFC02[ which(GCFC02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC02_TAM <- GCFC02[ which(GCFC02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC02_SDM <- GCFC02[ which(GCFC02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC02_TDM <- GCFC02[ which(GCFC02$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC02_SD <- GCFC02[ which(GCFC02$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC02_TD <- GCFC02[ which(GCFC02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC02_QT <- GCFC02[ which(GCFC02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
GCFC03_G <- GCFC03[ which(GCFC03$chainend == 'Goal_F'), ]
#Behind_F
GCFC03_B <- GCFC03[ which(GCFC03$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC03_SF <- GCFC03[ which(GCFC03$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC03_TF <- GCFC03[ which(GCFC03$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC03_SAM <- GCFC03[ which(GCFC03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC03_TAM <- GCFC03[ which(GCFC03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC03_SDM <- GCFC03[ which(GCFC03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC03_TDM <- GCFC03[ which(GCFC03$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC03_SD <- GCFC03[ which(GCFC03$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC03_TD <- GCFC03[ which(GCFC03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC03_QT <- GCFC03[ which(GCFC03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
GCFC04_G <- GCFC04[ which(GCFC04$chainend == 'Goal_F'), ]
#Behind_F
GCFC04_B <- GCFC04[ which(GCFC04$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC04_SF <- GCFC04[ which(GCFC04$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC04_TF <- GCFC04[ which(GCFC04$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC04_SAM <- GCFC04[ which(GCFC04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC04_TAM <- GCFC04[ which(GCFC04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC04_SDM <- GCFC04[ which(GCFC04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC04_TDM <- GCFC04[ which(GCFC04$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC04_SD <- GCFC04[ which(GCFC04$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC04_TD <- GCFC04[ which(GCFC04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC04_QT <- GCFC04[ which(GCFC04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
GCFC05_G <- GCFC05[ which(GCFC05$chainend == 'Goal_F'), ]
#Behind_F
GCFC05_B <- GCFC05[ which(GCFC05$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC05_SF <- GCFC05[ which(GCFC05$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC05_TF <- GCFC05[ which(GCFC05$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC05_SAM <- GCFC05[ which(GCFC05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC05_TAM <- GCFC05[ which(GCFC05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC05_SDM <- GCFC05[ which(GCFC05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC05_TDM <- GCFC05[ which(GCFC05$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC05_SD <- GCFC05[ which(GCFC05$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC05_TD <- GCFC05[ which(GCFC05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC05_QT <- GCFC05[ which(GCFC05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
GCFC06_G <- GCFC06[ which(GCFC06$chainend == 'Goal_F'), ]
#Behind_F
GCFC06_B <- GCFC06[ which(GCFC06$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC06_SF <- GCFC06[ which(GCFC06$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC06_TF <- GCFC06[ which(GCFC06$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC06_SAM <- GCFC06[ which(GCFC06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC06_TAM <- GCFC06[ which(GCFC06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC06_SDM <- GCFC06[ which(GCFC06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC06_TDM <- GCFC06[ which(GCFC06$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC06_SD <- GCFC06[ which(GCFC06$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC06_TD <- GCFC06[ which(GCFC06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC06_QT <- GCFC06[ which(GCFC06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
GCFC07_G <- GCFC07[ which(GCFC07$chainend == 'Goal_F'), ]
#Behind_F
GCFC07_B <- GCFC07[ which(GCFC07$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC07_SF <- GCFC07[ which(GCFC07$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC07_TF <- GCFC07[ which(GCFC07$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC07_SAM <- GCFC07[ which(GCFC07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC07_TAM <- GCFC07[ which(GCFC07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC07_SDM <- GCFC07[ which(GCFC07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC07_TDM <- GCFC07[ which(GCFC07$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC07_SD <- GCFC07[ which(GCFC07$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC07_TD <- GCFC07[ which(GCFC07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC07_QT <- GCFC07[ which(GCFC07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
GCFC08_G <- GCFC08[ which(GCFC08$chainend == 'Goal_F'), ]
#Behind_F
GCFC08_B <- GCFC08[ which(GCFC08$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC08_SF <- GCFC08[ which(GCFC08$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC08_TF <- GCFC08[ which(GCFC08$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC08_SAM <- GCFC08[ which(GCFC08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC08_TAM <- GCFC08[ which(GCFC08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC08_SDM <- GCFC08[ which(GCFC08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC08_TDM <- GCFC08[ which(GCFC08$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC08_SD <- GCFC08[ which(GCFC08$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC08_TD <- GCFC08[ which(GCFC08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC08_QT <- GCFC08[ which(GCFC08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
GCFC09_G <- GCFC09[ which(GCFC09$chainend == 'Goal_F'), ]
#Behind_F
GCFC09_B <- GCFC09[ which(GCFC09$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC09_SF <- GCFC09[ which(GCFC09$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC09_TF <- GCFC09[ which(GCFC09$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC09_SAM <- GCFC09[ which(GCFC09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC09_TAM <- GCFC09[ which(GCFC09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC09_SDM <- GCFC09[ which(GCFC09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC09_TDM <- GCFC09[ which(GCFC09$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC09_SD <- GCFC09[ which(GCFC09$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC09_TD <- GCFC09[ which(GCFC09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC09_QT <- GCFC09[ which(GCFC09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
GCFC10_G <- GCFC10[ which(GCFC10$chainend == 'Goal_F'), ]
#Behind_F
GCFC10_B <- GCFC10[ which(GCFC10$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC10_SF <- GCFC10[ which(GCFC10$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC10_TF <- GCFC10[ which(GCFC10$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC10_SAM <- GCFC10[ which(GCFC10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC10_TAM <- GCFC10[ which(GCFC10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC10_SDM <- GCFC10[ which(GCFC10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC10_TDM <- GCFC10[ which(GCFC10$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC10_SD <- GCFC10[ which(GCFC10$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC10_TD <- GCFC10[ which(GCFC10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC10_QT <- GCFC10[ which(GCFC10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
GCFC11_G <- GCFC11[ which(GCFC11$chainend == 'Goal_F'), ]
#Behind_F
GCFC11_B <- GCFC11[ which(GCFC11$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC11_SF <- GCFC11[ which(GCFC11$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC11_TF <- GCFC11[ which(GCFC11$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC11_SAM <- GCFC11[ which(GCFC11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC11_TAM <- GCFC11[ which(GCFC11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC11_SDM <- GCFC11[ which(GCFC11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC11_TDM <- GCFC11[ which(GCFC11$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC11_SD <- GCFC11[ which(GCFC11$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC11_TD <- GCFC11[ which(GCFC11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC11_QT <- GCFC11[ which(GCFC11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
GCFC12_G <- GCFC12[ which(GCFC12$chainend == 'Goal_F'), ]
#Behind_F
GCFC12_B <- GCFC12[ which(GCFC12$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC12_SF <- GCFC12[ which(GCFC12$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC12_TF <- GCFC12[ which(GCFC12$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC12_SAM <- GCFC12[ which(GCFC12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC12_TAM <- GCFC12[ which(GCFC12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC12_SDM <- GCFC12[ which(GCFC12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC12_TDM <- GCFC12[ which(GCFC12$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC12_SD <- GCFC12[ which(GCFC12$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC12_TD <- GCFC12[ which(GCFC12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC12_QT <- GCFC12[ which(GCFC12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
GCFC13_G <- GCFC13[ which(GCFC13$chainend == 'Goal_F'), ]
#Behind_F
GCFC13_B <- GCFC13[ which(GCFC13$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC13_SF <- GCFC13[ which(GCFC13$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC13_TF <- GCFC13[ which(GCFC13$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC13_SAM <- GCFC13[ which(GCFC13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC13_TAM <- GCFC13[ which(GCFC13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC13_SDM <- GCFC13[ which(GCFC13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC13_TDM <- GCFC13[ which(GCFC13$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC13_SD <- GCFC13[ which(GCFC13$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC13_TD <- GCFC13[ which(GCFC13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC13_QT <- GCFC13[ which(GCFC13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
GCFC14_G <- GCFC14[ which(GCFC14$chainend == 'Goal_F'), ]
#Behind_F
GCFC14_B <- GCFC14[ which(GCFC14$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC14_SF <- GCFC14[ which(GCFC14$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC14_TF <- GCFC14[ which(GCFC14$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC14_SAM <- GCFC14[ which(GCFC14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC14_TAM <- GCFC14[ which(GCFC14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC14_SDM <- GCFC14[ which(GCFC14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC14_TDM <- GCFC14[ which(GCFC14$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC14_SD <- GCFC14[ which(GCFC14$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC14_TD <- GCFC14[ which(GCFC14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC14_QT <- GCFC14[ which(GCFC14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
GCFC15_G <- GCFC15[ which(GCFC15$chainend == 'Goal_F'), ]
#Behind_F
GCFC15_B <- GCFC15[ which(GCFC15$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC15_SF <- GCFC15[ which(GCFC15$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC15_TF <- GCFC15[ which(GCFC15$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC15_SAM <- GCFC15[ which(GCFC15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC15_TAM <- GCFC15[ which(GCFC15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC15_SDM <- GCFC15[ which(GCFC15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC15_TDM <- GCFC15[ which(GCFC15$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC15_SD <- GCFC15[ which(GCFC15$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC15_TD <- GCFC15[ which(GCFC15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC15_QT <- GCFC15[ which(GCFC15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
GCFC16_G <- GCFC16[ which(GCFC16$chainend == 'Goal_F'), ]
#Behind_F
GCFC16_B <- GCFC16[ which(GCFC16$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC16_SF <- GCFC16[ which(GCFC16$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC16_TF <- GCFC16[ which(GCFC16$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC16_SAM <- GCFC16[ which(GCFC16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC16_TAM <- GCFC16[ which(GCFC16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC16_SDM <- GCFC16[ which(GCFC16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC16_TDM <- GCFC16[ which(GCFC16$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC16_SD <- GCFC16[ which(GCFC16$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC16_TD <- GCFC16[ which(GCFC16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC16_QT <- GCFC16[ which(GCFC16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
GCFC17_G <- GCFC17[ which(GCFC17$chainend == 'Goal_F'), ]
#Behind_F
GCFC17_B <- GCFC17[ which(GCFC17$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC17_SF <- GCFC17[ which(GCFC17$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC17_TF <- GCFC17[ which(GCFC17$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC17_SAM <- GCFC17[ which(GCFC17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC17_TAM <- GCFC17[ which(GCFC17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC17_SDM <- GCFC17[ which(GCFC17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC17_TDM <- GCFC17[ which(GCFC17$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC17_SD <- GCFC17[ which(GCFC17$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC17_TD <- GCFC17[ which(GCFC17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC17_QT <- GCFC17[ which(GCFC17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
GCFC18_G <- GCFC18[ which(GCFC18$chainend == 'Goal_F'), ]
#Behind_F
GCFC18_B <- GCFC18[ which(GCFC18$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC18_SF <- GCFC18[ which(GCFC18$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC18_TF <- GCFC18[ which(GCFC18$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC18_SAM <- GCFC18[ which(GCFC18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC18_TAM <- GCFC18[ which(GCFC18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC18_SDM <- GCFC18[ which(GCFC18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC18_TDM <- GCFC18[ which(GCFC18$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC18_SD <- GCFC18[ which(GCFC18$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC18_TD <- GCFC18[ which(GCFC18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC18_QT <- GCFC18[ which(GCFC18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
GCFC19_G <- GCFC19[ which(GCFC19$chainend == 'Goal_F'), ]
#Behind_F
GCFC19_B <- GCFC19[ which(GCFC19$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC19_SF <- GCFC19[ which(GCFC19$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC19_TF <- GCFC19[ which(GCFC19$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC19_SAM <- GCFC19[ which(GCFC19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC19_TAM <- GCFC19[ which(GCFC19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC19_SDM <- GCFC19[ which(GCFC19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC19_TDM <- GCFC19[ which(GCFC19$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC19_SD <- GCFC19[ which(GCFC19$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC19_TD <- GCFC19[ which(GCFC19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC19_QT <- GCFC19[ which(GCFC19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
GCFC20_G <- GCFC20[ which(GCFC20$chainend == 'Goal_F'), ]
#Behind_F
GCFC20_B <- GCFC20[ which(GCFC20$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC20_SF <- GCFC20[ which(GCFC20$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC20_TF <- GCFC20[ which(GCFC20$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC20_SAM <- GCFC20[ which(GCFC20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC20_TAM <- GCFC20[ which(GCFC20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC20_SDM <- GCFC20[ which(GCFC20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC20_TDM <- GCFC20[ which(GCFC20$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC20_SD <- GCFC20[ which(GCFC20$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC20_TD <- GCFC20[ which(GCFC20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC20_QT <- GCFC20[ which(GCFC20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
GCFC21_G <- GCFC21[ which(GCFC21$chainend == 'Goal_F'), ]
#Behind_F
GCFC21_B <- GCFC21[ which(GCFC21$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC21_SF <- GCFC21[ which(GCFC21$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC21_TF <- GCFC21[ which(GCFC21$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC21_SAM <- GCFC21[ which(GCFC21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC21_TAM <- GCFC21[ which(GCFC21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC21_SDM <- GCFC21[ which(GCFC21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC21_TDM <- GCFC21[ which(GCFC21$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC21_SD <- GCFC21[ which(GCFC21$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC21_TD <- GCFC21[ which(GCFC21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC21_QT <- GCFC21[ which(GCFC21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
GCFC22_G <- GCFC22[ which(GCFC22$chainend == 'Goal_F'), ]
#Behind_F
GCFC22_B <- GCFC22[ which(GCFC22$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC22_SF <- GCFC22[ which(GCFC22$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC22_TF <- GCFC22[ which(GCFC22$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC22_SAM <- GCFC22[ which(GCFC22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC22_TAM <- GCFC22[ which(GCFC22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC22_SDM <- GCFC22[ which(GCFC22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC22_TDM <- GCFC22[ which(GCFC22$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC22_SD <- GCFC22[ which(GCFC22$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC22_TD <- GCFC22[ which(GCFC22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC22_QT <- GCFC22[ which(GCFC22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
GCFC23_G <- GCFC23[ which(GCFC23$chainend == 'Goal_F'), ]
#Behind_F
GCFC23_B <- GCFC23[ which(GCFC23$chainend == 'Behind_F'), ]
#Stoppage_F
GCFC23_SF <- GCFC23[ which(GCFC23$chainend == 'Stoppage_F'), ]
#Turnover_F
GCFC23_TF <- GCFC23[ which(GCFC23$chainend == 'Turnover_F'), ]
#Stoppage_AM
GCFC23_SAM <- GCFC23[ which(GCFC23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GCFC23_TAM <- GCFC23[ which(GCFC23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GCFC23_SDM <- GCFC23[ which(GCFC23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GCFC23_TDM <- GCFC23[ which(GCFC23$chainend == 'Turnover_DM'), ]
#Stoppage_D
GCFC23_SD <- GCFC23[ which(GCFC23$chainend == 'Stoppage_D'), ]
#Turnover_D
GCFC23_TD <- GCFC23[ which(GCFC23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GCFC23_QT <- GCFC23[ which(GCFC23$chainend == 'End of Qtr_DM'), ]

#Geelong

#Split by rounds
GEEL01 <- GEEL[ which(GEEL$round == '01'), ]
GEEL02 <- GEEL[ which(GEEL$round == '02'), ]
GEEL03 <- GEEL[ which(GEEL$round == '03'), ]
GEEL04 <- GEEL[ which(GEEL$round == '04'), ]
GEEL05 <- GEEL[ which(GEEL$round == '05'), ]
GEEL06 <- GEEL[ which(GEEL$round == '06'), ]
GEEL07 <- GEEL[ which(GEEL$round == '07'), ]
GEEL08 <- GEEL[ which(GEEL$round == '08'), ]
GEEL09 <- GEEL[ which(GEEL$round == '09'), ]
GEEL10 <- GEEL[ which(GEEL$round == '10'), ]
GEEL11 <- GEEL[ which(GEEL$round == '11'), ]
GEEL12 <- GEEL[ which(GEEL$round == '12'), ]
GEEL13 <- GEEL[ which(GEEL$round == '13'), ]
GEEL14 <- GEEL[ which(GEEL$round == '14'), ]
GEEL15 <- GEEL[ which(GEEL$round == '15'), ]
GEEL16 <- GEEL[ which(GEEL$round == '16'), ]
GEEL17 <- GEEL[ which(GEEL$round == '17'), ]
GEEL18 <- GEEL[ which(GEEL$round == '18'), ]
GEEL19 <- GEEL[ which(GEEL$round == '19'), ]
GEEL20 <- GEEL[ which(GEEL$round == '20'), ]
GEEL21 <- GEEL[ which(GEEL$round == '21'), ]
GEEL22 <- GEEL[ which(GEEL$round == '22'), ]
GEEL23 <- GEEL[ which(GEEL$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
GEEL01_G <- GEEL01[ which(GEEL01$chainend == 'Goal_F'), ]
#Behind_F
GEEL01_B <- GEEL01[ which(GEEL01$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL01_SF <- GEEL01[ which(GEEL01$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL01_TF <- GEEL01[ which(GEEL01$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL01_SAM <- GEEL01[ which(GEEL01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL01_TAM <- GEEL01[ which(GEEL01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL01_SDM <- GEEL01[ which(GEEL01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL01_TDM <- GEEL01[ which(GEEL01$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL01_SD <- GEEL01[ which(GEEL01$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL01_TD <- GEEL01[ which(GEEL01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL01_QT <- GEEL01[ which(GEEL01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
GEEL02_G <- GEEL02[ which(GEEL02$chainend == 'Goal_F'), ]
#Behind_F
GEEL02_B <- GEEL02[ which(GEEL02$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL02_SF <- GEEL02[ which(GEEL02$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL02_TF <- GEEL02[ which(GEEL02$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL02_SAM <- GEEL02[ which(GEEL02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL02_TAM <- GEEL02[ which(GEEL02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL02_SDM <- GEEL02[ which(GEEL02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL02_TDM <- GEEL02[ which(GEEL02$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL02_SD <- GEEL02[ which(GEEL02$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL02_TD <- GEEL02[ which(GEEL02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL02_QT <- GEEL02[ which(GEEL02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
GEEL03_G <- GEEL03[ which(GEEL03$chainend == 'Goal_F'), ]
#Behind_F
GEEL03_B <- GEEL03[ which(GEEL03$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL03_SF <- GEEL03[ which(GEEL03$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL03_TF <- GEEL03[ which(GEEL03$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL03_SAM <- GEEL03[ which(GEEL03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL03_TAM <- GEEL03[ which(GEEL03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL03_SDM <- GEEL03[ which(GEEL03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL03_TDM <- GEEL03[ which(GEEL03$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL03_SD <- GEEL03[ which(GEEL03$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL03_TD <- GEEL03[ which(GEEL03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL03_QT <- GEEL03[ which(GEEL03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
GEEL04_G <- GEEL04[ which(GEEL04$chainend == 'Goal_F'), ]
#Behind_F
GEEL04_B <- GEEL04[ which(GEEL04$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL04_SF <- GEEL04[ which(GEEL04$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL04_TF <- GEEL04[ which(GEEL04$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL04_SAM <- GEEL04[ which(GEEL04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL04_TAM <- GEEL04[ which(GEEL04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL04_SDM <- GEEL04[ which(GEEL04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL04_TDM <- GEEL04[ which(GEEL04$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL04_SD <- GEEL04[ which(GEEL04$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL04_TD <- GEEL04[ which(GEEL04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL04_QT <- GEEL04[ which(GEEL04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
GEEL05_G <- GEEL05[ which(GEEL05$chainend == 'Goal_F'), ]
#Behind_F
GEEL05_B <- GEEL05[ which(GEEL05$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL05_SF <- GEEL05[ which(GEEL05$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL05_TF <- GEEL05[ which(GEEL05$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL05_SAM <- GEEL05[ which(GEEL05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL05_TAM <- GEEL05[ which(GEEL05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL05_SDM <- GEEL05[ which(GEEL05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL05_TDM <- GEEL05[ which(GEEL05$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL05_SD <- GEEL05[ which(GEEL05$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL05_TD <- GEEL05[ which(GEEL05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL05_QT <- GEEL05[ which(GEEL05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
GEEL06_G <- GEEL06[ which(GEEL06$chainend == 'Goal_F'), ]
#Behind_F
GEEL06_B <- GEEL06[ which(GEEL06$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL06_SF <- GEEL06[ which(GEEL06$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL06_TF <- GEEL06[ which(GEEL06$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL06_SAM <- GEEL06[ which(GEEL06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL06_TAM <- GEEL06[ which(GEEL06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL06_SDM <- GEEL06[ which(GEEL06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL06_TDM <- GEEL06[ which(GEEL06$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL06_SD <- GEEL06[ which(GEEL06$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL06_TD <- GEEL06[ which(GEEL06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL06_QT <- GEEL06[ which(GEEL06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
GEEL07_G <- GEEL07[ which(GEEL07$chainend == 'Goal_F'), ]
#Behind_F
GEEL07_B <- GEEL07[ which(GEEL07$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL07_SF <- GEEL07[ which(GEEL07$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL07_TF <- GEEL07[ which(GEEL07$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL07_SAM <- GEEL07[ which(GEEL07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL07_TAM <- GEEL07[ which(GEEL07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL07_SDM <- GEEL07[ which(GEEL07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL07_TDM <- GEEL07[ which(GEEL07$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL07_SD <- GEEL07[ which(GEEL07$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL07_TD <- GEEL07[ which(GEEL07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL07_QT <- GEEL07[ which(GEEL07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
GEEL08_G <- GEEL08[ which(GEEL08$chainend == 'Goal_F'), ]
#Behind_F
GEEL08_B <- GEEL08[ which(GEEL08$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL08_SF <- GEEL08[ which(GEEL08$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL08_TF <- GEEL08[ which(GEEL08$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL08_SAM <- GEEL08[ which(GEEL08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL08_TAM <- GEEL08[ which(GEEL08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL08_SDM <- GEEL08[ which(GEEL08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL08_TDM <- GEEL08[ which(GEEL08$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL08_SD <- GEEL08[ which(GEEL08$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL08_TD <- GEEL08[ which(GEEL08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL08_QT <- GEEL08[ which(GEEL08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
GEEL09_G <- GEEL09[ which(GEEL09$chainend == 'Goal_F'), ]
#Behind_F
GEEL09_B <- GEEL09[ which(GEEL09$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL09_SF <- GEEL09[ which(GEEL09$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL09_TF <- GEEL09[ which(GEEL09$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL09_SAM <- GEEL09[ which(GEEL09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL09_TAM <- GEEL09[ which(GEEL09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL09_SDM <- GEEL09[ which(GEEL09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL09_TDM <- GEEL09[ which(GEEL09$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL09_SD <- GEEL09[ which(GEEL09$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL09_TD <- GEEL09[ which(GEEL09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL09_QT <- GEEL09[ which(GEEL09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
GEEL10_G <- GEEL10[ which(GEEL10$chainend == 'Goal_F'), ]
#Behind_F
GEEL10_B <- GEEL10[ which(GEEL10$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL10_SF <- GEEL10[ which(GEEL10$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL10_TF <- GEEL10[ which(GEEL10$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL10_SAM <- GEEL10[ which(GEEL10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL10_TAM <- GEEL10[ which(GEEL10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL10_SDM <- GEEL10[ which(GEEL10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL10_TDM <- GEEL10[ which(GEEL10$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL10_SD <- GEEL10[ which(GEEL10$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL10_TD <- GEEL10[ which(GEEL10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL10_QT <- GEEL10[ which(GEEL10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
GEEL11_G <- GEEL11[ which(GEEL11$chainend == 'Goal_F'), ]
#Behind_F
GEEL11_B <- GEEL11[ which(GEEL11$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL11_SF <- GEEL11[ which(GEEL11$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL11_TF <- GEEL11[ which(GEEL11$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL11_SAM <- GEEL11[ which(GEEL11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL11_TAM <- GEEL11[ which(GEEL11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL11_SDM <- GEEL11[ which(GEEL11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL11_TDM <- GEEL11[ which(GEEL11$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL11_SD <- GEEL11[ which(GEEL11$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL11_TD <- GEEL11[ which(GEEL11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL11_QT <- GEEL11[ which(GEEL11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
GEEL12_G <- GEEL12[ which(GEEL12$chainend == 'Goal_F'), ]
#Behind_F
GEEL12_B <- GEEL12[ which(GEEL12$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL12_SF <- GEEL12[ which(GEEL12$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL12_TF <- GEEL12[ which(GEEL12$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL12_SAM <- GEEL12[ which(GEEL12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL12_TAM <- GEEL12[ which(GEEL12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL12_SDM <- GEEL12[ which(GEEL12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL12_TDM <- GEEL12[ which(GEEL12$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL12_SD <- GEEL12[ which(GEEL12$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL12_TD <- GEEL12[ which(GEEL12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL12_QT <- GEEL12[ which(GEEL12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
GEEL13_G <- GEEL13[ which(GEEL13$chainend == 'Goal_F'), ]
#Behind_F
GEEL13_B <- GEEL13[ which(GEEL13$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL13_SF <- GEEL13[ which(GEEL13$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL13_TF <- GEEL13[ which(GEEL13$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL13_SAM <- GEEL13[ which(GEEL13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL13_TAM <- GEEL13[ which(GEEL13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL13_SDM <- GEEL13[ which(GEEL13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL13_TDM <- GEEL13[ which(GEEL13$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL13_SD <- GEEL13[ which(GEEL13$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL13_TD <- GEEL13[ which(GEEL13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL13_QT <- GEEL13[ which(GEEL13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
GEEL14_G <- GEEL14[ which(GEEL14$chainend == 'Goal_F'), ]
#Behind_F
GEEL14_B <- GEEL14[ which(GEEL14$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL14_SF <- GEEL14[ which(GEEL14$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL14_TF <- GEEL14[ which(GEEL14$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL14_SAM <- GEEL14[ which(GEEL14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL14_TAM <- GEEL14[ which(GEEL14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL14_SDM <- GEEL14[ which(GEEL14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL14_TDM <- GEEL14[ which(GEEL14$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL14_SD <- GEEL14[ which(GEEL14$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL14_TD <- GEEL14[ which(GEEL14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL14_QT <- GEEL14[ which(GEEL14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
GEEL15_G <- GEEL15[ which(GEEL15$chainend == 'Goal_F'), ]
#Behind_F
GEEL15_B <- GEEL15[ which(GEEL15$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL15_SF <- GEEL15[ which(GEEL15$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL15_TF <- GEEL15[ which(GEEL15$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL15_SAM <- GEEL15[ which(GEEL15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL15_TAM <- GEEL15[ which(GEEL15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL15_SDM <- GEEL15[ which(GEEL15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL15_TDM <- GEEL15[ which(GEEL15$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL15_SD <- GEEL15[ which(GEEL15$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL15_TD <- GEEL15[ which(GEEL15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL15_QT <- GEEL15[ which(GEEL15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
GEEL16_G <- GEEL16[ which(GEEL16$chainend == 'Goal_F'), ]
#Behind_F
GEEL16_B <- GEEL16[ which(GEEL16$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL16_SF <- GEEL16[ which(GEEL16$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL16_TF <- GEEL16[ which(GEEL16$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL16_SAM <- GEEL16[ which(GEEL16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL16_TAM <- GEEL16[ which(GEEL16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL16_SDM <- GEEL16[ which(GEEL16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL16_TDM <- GEEL16[ which(GEEL16$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL16_SD <- GEEL16[ which(GEEL16$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL16_TD <- GEEL16[ which(GEEL16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL16_QT <- GEEL16[ which(GEEL16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
GEEL17_G <- GEEL17[ which(GEEL17$chainend == 'Goal_F'), ]
#Behind_F
GEEL17_B <- GEEL17[ which(GEEL17$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL17_SF <- GEEL17[ which(GEEL17$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL17_TF <- GEEL17[ which(GEEL17$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL17_SAM <- GEEL17[ which(GEEL17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL17_TAM <- GEEL17[ which(GEEL17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL17_SDM <- GEEL17[ which(GEEL17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL17_TDM <- GEEL17[ which(GEEL17$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL17_SD <- GEEL17[ which(GEEL17$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL17_TD <- GEEL17[ which(GEEL17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL17_QT <- GEEL17[ which(GEEL17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
GEEL18_G <- GEEL18[ which(GEEL18$chainend == 'Goal_F'), ]
#Behind_F
GEEL18_B <- GEEL18[ which(GEEL18$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL18_SF <- GEEL18[ which(GEEL18$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL18_TF <- GEEL18[ which(GEEL18$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL18_SAM <- GEEL18[ which(GEEL18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL18_TAM <- GEEL18[ which(GEEL18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL18_SDM <- GEEL18[ which(GEEL18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL18_TDM <- GEEL18[ which(GEEL18$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL18_SD <- GEEL18[ which(GEEL18$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL18_TD <- GEEL18[ which(GEEL18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL18_QT <- GEEL18[ which(GEEL18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
GEEL19_G <- GEEL19[ which(GEEL19$chainend == 'Goal_F'), ]
#Behind_F
GEEL19_B <- GEEL19[ which(GEEL19$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL19_SF <- GEEL19[ which(GEEL19$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL19_TF <- GEEL19[ which(GEEL19$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL19_SAM <- GEEL19[ which(GEEL19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL19_TAM <- GEEL19[ which(GEEL19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL19_SDM <- GEEL19[ which(GEEL19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL19_TDM <- GEEL19[ which(GEEL19$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL19_SD <- GEEL19[ which(GEEL19$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL19_TD <- GEEL19[ which(GEEL19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL19_QT <- GEEL19[ which(GEEL19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
GEEL20_G <- GEEL20[ which(GEEL20$chainend == 'Goal_F'), ]
#Behind_F
GEEL20_B <- GEEL20[ which(GEEL20$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL20_SF <- GEEL20[ which(GEEL20$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL20_TF <- GEEL20[ which(GEEL20$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL20_SAM <- GEEL20[ which(GEEL20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL20_TAM <- GEEL20[ which(GEEL20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL20_SDM <- GEEL20[ which(GEEL20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL20_TDM <- GEEL20[ which(GEEL20$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL20_SD <- GEEL20[ which(GEEL20$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL20_TD <- GEEL20[ which(GEEL20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL20_QT <- GEEL20[ which(GEEL20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
GEEL21_G <- GEEL21[ which(GEEL21$chainend == 'Goal_F'), ]
#Behind_F
GEEL21_B <- GEEL21[ which(GEEL21$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL21_SF <- GEEL21[ which(GEEL21$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL21_TF <- GEEL21[ which(GEEL21$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL21_SAM <- GEEL21[ which(GEEL21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL21_TAM <- GEEL21[ which(GEEL21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL21_SDM <- GEEL21[ which(GEEL21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL21_TDM <- GEEL21[ which(GEEL21$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL21_SD <- GEEL21[ which(GEEL21$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL21_TD <- GEEL21[ which(GEEL21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL21_QT <- GEEL21[ which(GEEL21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
GEEL22_G <- GEEL22[ which(GEEL22$chainend == 'Goal_F'), ]
#Behind_F
GEEL22_B <- GEEL22[ which(GEEL22$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL22_SF <- GEEL22[ which(GEEL22$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL22_TF <- GEEL22[ which(GEEL22$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL22_SAM <- GEEL22[ which(GEEL22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL22_TAM <- GEEL22[ which(GEEL22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL22_SDM <- GEEL22[ which(GEEL22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL22_TDM <- GEEL22[ which(GEEL22$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL22_SD <- GEEL22[ which(GEEL22$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL22_TD <- GEEL22[ which(GEEL22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL22_QT <- GEEL22[ which(GEEL22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
GEEL23_G <- GEEL23[ which(GEEL23$chainend == 'Goal_F'), ]
#Behind_F
GEEL23_B <- GEEL23[ which(GEEL23$chainend == 'Behind_F'), ]
#Stoppage_F
GEEL23_SF <- GEEL23[ which(GEEL23$chainend == 'Stoppage_F'), ]
#Turnover_F
GEEL23_TF <- GEEL23[ which(GEEL23$chainend == 'Turnover_F'), ]
#Stoppage_AM
GEEL23_SAM <- GEEL23[ which(GEEL23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GEEL23_TAM <- GEEL23[ which(GEEL23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GEEL23_SDM <- GEEL23[ which(GEEL23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GEEL23_TDM <- GEEL23[ which(GEEL23$chainend == 'Turnover_DM'), ]
#Stoppage_D
GEEL23_SD <- GEEL23[ which(GEEL23$chainend == 'Stoppage_D'), ]
#Turnover_D
GEEL23_TD <- GEEL23[ which(GEEL23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GEEL23_QT <- GEEL23[ which(GEEL23$chainend == 'End of Qtr_DM'), ]

#Greater Western Sydney

#Split by rounds
GWS01 <- GWS[ which(GWS$round == '01'), ]
GWS02 <- GWS[ which(GWS$round == '02'), ]
GWS03 <- GWS[ which(GWS$round == '03'), ]
GWS04 <- GWS[ which(GWS$round == '04'), ]
GWS05 <- GWS[ which(GWS$round == '05'), ]
GWS06 <- GWS[ which(GWS$round == '06'), ]
GWS07 <- GWS[ which(GWS$round == '07'), ]
GWS08 <- GWS[ which(GWS$round == '08'), ]
GWS09 <- GWS[ which(GWS$round == '09'), ]
GWS10 <- GWS[ which(GWS$round == '10'), ]
GWS11 <- GWS[ which(GWS$round == '11'), ]
GWS12 <- GWS[ which(GWS$round == '12'), ]
GWS13 <- GWS[ which(GWS$round == '13'), ]
GWS14 <- GWS[ which(GWS$round == '14'), ]
GWS15 <- GWS[ which(GWS$round == '15'), ]
GWS16 <- GWS[ which(GWS$round == '16'), ]
GWS17 <- GWS[ which(GWS$round == '17'), ]
GWS18 <- GWS[ which(GWS$round == '18'), ]
GWS19 <- GWS[ which(GWS$round == '19'), ]
GWS20 <- GWS[ which(GWS$round == '20'), ]
GWS21 <- GWS[ which(GWS$round == '21'), ]
GWS22 <- GWS[ which(GWS$round == '22'), ]
GWS23 <- GWS[ which(GWS$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
GWS01_G <- GWS01[ which(GWS01$chainend == 'Goal_F'), ]
#Behind_F
GWS01_B <- GWS01[ which(GWS01$chainend == 'Behind_F'), ]
#Stoppage_F
GWS01_SF <- GWS01[ which(GWS01$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS01_TF <- GWS01[ which(GWS01$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS01_SAM <- GWS01[ which(GWS01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS01_TAM <- GWS01[ which(GWS01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS01_SDM <- GWS01[ which(GWS01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS01_TDM <- GWS01[ which(GWS01$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS01_SD <- GWS01[ which(GWS01$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS01_TD <- GWS01[ which(GWS01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS01_QT <- GWS01[ which(GWS01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
GWS02_G <- GWS02[ which(GWS02$chainend == 'Goal_F'), ]
#Behind_F
GWS02_B <- GWS02[ which(GWS02$chainend == 'Behind_F'), ]
#Stoppage_F
GWS02_SF <- GWS02[ which(GWS02$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS02_TF <- GWS02[ which(GWS02$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS02_SAM <- GWS02[ which(GWS02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS02_TAM <- GWS02[ which(GWS02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS02_SDM <- GWS02[ which(GWS02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS02_TDM <- GWS02[ which(GWS02$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS02_SD <- GWS02[ which(GWS02$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS02_TD <- GWS02[ which(GWS02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS02_QT <- GWS02[ which(GWS02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
GWS03_G <- GWS03[ which(GWS03$chainend == 'Goal_F'), ]
#Behind_F
GWS03_B <- GWS03[ which(GWS03$chainend == 'Behind_F'), ]
#Stoppage_F
GWS03_SF <- GWS03[ which(GWS03$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS03_TF <- GWS03[ which(GWS03$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS03_SAM <- GWS03[ which(GWS03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS03_TAM <- GWS03[ which(GWS03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS03_SDM <- GWS03[ which(GWS03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS03_TDM <- GWS03[ which(GWS03$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS03_SD <- GWS03[ which(GWS03$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS03_TD <- GWS03[ which(GWS03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS03_QT <- GWS03[ which(GWS03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
GWS04_G <- GWS04[ which(GWS04$chainend == 'Goal_F'), ]
#Behind_F
GWS04_B <- GWS04[ which(GWS04$chainend == 'Behind_F'), ]
#Stoppage_F
GWS04_SF <- GWS04[ which(GWS04$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS04_TF <- GWS04[ which(GWS04$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS04_SAM <- GWS04[ which(GWS04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS04_TAM <- GWS04[ which(GWS04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS04_SDM <- GWS04[ which(GWS04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS04_TDM <- GWS04[ which(GWS04$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS04_SD <- GWS04[ which(GWS04$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS04_TD <- GWS04[ which(GWS04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS04_QT <- GWS04[ which(GWS04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
GWS05_G <- GWS05[ which(GWS05$chainend == 'Goal_F'), ]
#Behind_F
GWS05_B <- GWS05[ which(GWS05$chainend == 'Behind_F'), ]
#Stoppage_F
GWS05_SF <- GWS05[ which(GWS05$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS05_TF <- GWS05[ which(GWS05$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS05_SAM <- GWS05[ which(GWS05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS05_TAM <- GWS05[ which(GWS05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS05_SDM <- GWS05[ which(GWS05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS05_TDM <- GWS05[ which(GWS05$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS05_SD <- GWS05[ which(GWS05$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS05_TD <- GWS05[ which(GWS05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS05_QT <- GWS05[ which(GWS05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
GWS06_G <- GWS06[ which(GWS06$chainend == 'Goal_F'), ]
#Behind_F
GWS06_B <- GWS06[ which(GWS06$chainend == 'Behind_F'), ]
#Stoppage_F
GWS06_SF <- GWS06[ which(GWS06$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS06_TF <- GWS06[ which(GWS06$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS06_SAM <- GWS06[ which(GWS06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS06_TAM <- GWS06[ which(GWS06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS06_SDM <- GWS06[ which(GWS06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS06_TDM <- GWS06[ which(GWS06$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS06_SD <- GWS06[ which(GWS06$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS06_TD <- GWS06[ which(GWS06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS06_QT <- GWS06[ which(GWS06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
GWS07_G <- GWS07[ which(GWS07$chainend == 'Goal_F'), ]
#Behind_F
GWS07_B <- GWS07[ which(GWS07$chainend == 'Behind_F'), ]
#Stoppage_F
GWS07_SF <- GWS07[ which(GWS07$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS07_TF <- GWS07[ which(GWS07$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS07_SAM <- GWS07[ which(GWS07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS07_TAM <- GWS07[ which(GWS07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS07_SDM <- GWS07[ which(GWS07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS07_TDM <- GWS07[ which(GWS07$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS07_SD <- GWS07[ which(GWS07$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS07_TD <- GWS07[ which(GWS07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS07_QT <- GWS07[ which(GWS07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
GWS08_G <- GWS08[ which(GWS08$chainend == 'Goal_F'), ]
#Behind_F
GWS08_B <- GWS08[ which(GWS08$chainend == 'Behind_F'), ]
#Stoppage_F
GWS08_SF <- GWS08[ which(GWS08$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS08_TF <- GWS08[ which(GWS08$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS08_SAM <- GWS08[ which(GWS08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS08_TAM <- GWS08[ which(GWS08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS08_SDM <- GWS08[ which(GWS08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS08_TDM <- GWS08[ which(GWS08$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS08_SD <- GWS08[ which(GWS08$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS08_TD <- GWS08[ which(GWS08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS08_QT <- GWS08[ which(GWS08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
GWS09_G <- GWS09[ which(GWS09$chainend == 'Goal_F'), ]
#Behind_F
GWS09_B <- GWS09[ which(GWS09$chainend == 'Behind_F'), ]
#Stoppage_F
GWS09_SF <- GWS09[ which(GWS09$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS09_TF <- GWS09[ which(GWS09$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS09_SAM <- GWS09[ which(GWS09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS09_TAM <- GWS09[ which(GWS09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS09_SDM <- GWS09[ which(GWS09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS09_TDM <- GWS09[ which(GWS09$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS09_SD <- GWS09[ which(GWS09$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS09_TD <- GWS09[ which(GWS09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS09_QT <- GWS09[ which(GWS09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
GWS10_G <- GWS10[ which(GWS10$chainend == 'Goal_F'), ]
#Behind_F
GWS10_B <- GWS10[ which(GWS10$chainend == 'Behind_F'), ]
#Stoppage_F
GWS10_SF <- GWS10[ which(GWS10$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS10_TF <- GWS10[ which(GWS10$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS10_SAM <- GWS10[ which(GWS10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS10_TAM <- GWS10[ which(GWS10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS10_SDM <- GWS10[ which(GWS10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS10_TDM <- GWS10[ which(GWS10$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS10_SD <- GWS10[ which(GWS10$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS10_TD <- GWS10[ which(GWS10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS10_QT <- GWS10[ which(GWS10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
GWS11_G <- GWS11[ which(GWS11$chainend == 'Goal_F'), ]
#Behind_F
GWS11_B <- GWS11[ which(GWS11$chainend == 'Behind_F'), ]
#Stoppage_F
GWS11_SF <- GWS11[ which(GWS11$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS11_TF <- GWS11[ which(GWS11$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS11_SAM <- GWS11[ which(GWS11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS11_TAM <- GWS11[ which(GWS11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS11_SDM <- GWS11[ which(GWS11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS11_TDM <- GWS11[ which(GWS11$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS11_SD <- GWS11[ which(GWS11$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS11_TD <- GWS11[ which(GWS11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS11_QT <- GWS11[ which(GWS11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
GWS12_G <- GWS12[ which(GWS12$chainend == 'Goal_F'), ]
#Behind_F
GWS12_B <- GWS12[ which(GWS12$chainend == 'Behind_F'), ]
#Stoppage_F
GWS12_SF <- GWS12[ which(GWS12$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS12_TF <- GWS12[ which(GWS12$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS12_SAM <- GWS12[ which(GWS12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS12_TAM <- GWS12[ which(GWS12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS12_SDM <- GWS12[ which(GWS12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS12_TDM <- GWS12[ which(GWS12$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS12_SD <- GWS12[ which(GWS12$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS12_TD <- GWS12[ which(GWS12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS12_QT <- GWS12[ which(GWS12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
GWS13_G <- GWS13[ which(GWS13$chainend == 'Goal_F'), ]
#Behind_F
GWS13_B <- GWS13[ which(GWS13$chainend == 'Behind_F'), ]
#Stoppage_F
GWS13_SF <- GWS13[ which(GWS13$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS13_TF <- GWS13[ which(GWS13$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS13_SAM <- GWS13[ which(GWS13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS13_TAM <- GWS13[ which(GWS13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS13_SDM <- GWS13[ which(GWS13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS13_TDM <- GWS13[ which(GWS13$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS13_SD <- GWS13[ which(GWS13$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS13_TD <- GWS13[ which(GWS13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS13_QT <- GWS13[ which(GWS13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
GWS14_G <- GWS14[ which(GWS14$chainend == 'Goal_F'), ]
#Behind_F
GWS14_B <- GWS14[ which(GWS14$chainend == 'Behind_F'), ]
#Stoppage_F
GWS14_SF <- GWS14[ which(GWS14$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS14_TF <- GWS14[ which(GWS14$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS14_SAM <- GWS14[ which(GWS14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS14_TAM <- GWS14[ which(GWS14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS14_SDM <- GWS14[ which(GWS14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS14_TDM <- GWS14[ which(GWS14$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS14_SD <- GWS14[ which(GWS14$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS14_TD <- GWS14[ which(GWS14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS14_QT <- GWS14[ which(GWS14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
GWS15_G <- GWS15[ which(GWS15$chainend == 'Goal_F'), ]
#Behind_F
GWS15_B <- GWS15[ which(GWS15$chainend == 'Behind_F'), ]
#Stoppage_F
GWS15_SF <- GWS15[ which(GWS15$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS15_TF <- GWS15[ which(GWS15$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS15_SAM <- GWS15[ which(GWS15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS15_TAM <- GWS15[ which(GWS15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS15_SDM <- GWS15[ which(GWS15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS15_TDM <- GWS15[ which(GWS15$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS15_SD <- GWS15[ which(GWS15$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS15_TD <- GWS15[ which(GWS15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS15_QT <- GWS15[ which(GWS15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
GWS16_G <- GWS16[ which(GWS16$chainend == 'Goal_F'), ]
#Behind_F
GWS16_B <- GWS16[ which(GWS16$chainend == 'Behind_F'), ]
#Stoppage_F
GWS16_SF <- GWS16[ which(GWS16$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS16_TF <- GWS16[ which(GWS16$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS16_SAM <- GWS16[ which(GWS16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS16_TAM <- GWS16[ which(GWS16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS16_SDM <- GWS16[ which(GWS16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS16_TDM <- GWS16[ which(GWS16$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS16_SD <- GWS16[ which(GWS16$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS16_TD <- GWS16[ which(GWS16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS16_QT <- GWS16[ which(GWS16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
GWS17_G <- GWS17[ which(GWS17$chainend == 'Goal_F'), ]
#Behind_F
GWS17_B <- GWS17[ which(GWS17$chainend == 'Behind_F'), ]
#Stoppage_F
GWS17_SF <- GWS17[ which(GWS17$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS17_TF <- GWS17[ which(GWS17$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS17_SAM <- GWS17[ which(GWS17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS17_TAM <- GWS17[ which(GWS17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS17_SDM <- GWS17[ which(GWS17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS17_TDM <- GWS17[ which(GWS17$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS17_SD <- GWS17[ which(GWS17$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS17_TD <- GWS17[ which(GWS17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS17_QT <- GWS17[ which(GWS17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
GWS18_G <- GWS18[ which(GWS18$chainend == 'Goal_F'), ]
#Behind_F
GWS18_B <- GWS18[ which(GWS18$chainend == 'Behind_F'), ]
#Stoppage_F
GWS18_SF <- GWS18[ which(GWS18$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS18_TF <- GWS18[ which(GWS18$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS18_SAM <- GWS18[ which(GWS18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS18_TAM <- GWS18[ which(GWS18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS18_SDM <- GWS18[ which(GWS18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS18_TDM <- GWS18[ which(GWS18$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS18_SD <- GWS18[ which(GWS18$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS18_TD <- GWS18[ which(GWS18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS18_QT <- GWS18[ which(GWS18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
GWS19_G <- GWS19[ which(GWS19$chainend == 'Goal_F'), ]
#Behind_F
GWS19_B <- GWS19[ which(GWS19$chainend == 'Behind_F'), ]
#Stoppage_F
GWS19_SF <- GWS19[ which(GWS19$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS19_TF <- GWS19[ which(GWS19$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS19_SAM <- GWS19[ which(GWS19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS19_TAM <- GWS19[ which(GWS19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS19_SDM <- GWS19[ which(GWS19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS19_TDM <- GWS19[ which(GWS19$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS19_SD <- GWS19[ which(GWS19$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS19_TD <- GWS19[ which(GWS19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS19_QT <- GWS19[ which(GWS19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
GWS20_G <- GWS20[ which(GWS20$chainend == 'Goal_F'), ]
#Behind_F
GWS20_B <- GWS20[ which(GWS20$chainend == 'Behind_F'), ]
#Stoppage_F
GWS20_SF <- GWS20[ which(GWS20$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS20_TF <- GWS20[ which(GWS20$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS20_SAM <- GWS20[ which(GWS20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS20_TAM <- GWS20[ which(GWS20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS20_SDM <- GWS20[ which(GWS20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS20_TDM <- GWS20[ which(GWS20$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS20_SD <- GWS20[ which(GWS20$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS20_TD <- GWS20[ which(GWS20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS20_QT <- GWS20[ which(GWS20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
GWS21_G <- GWS21[ which(GWS21$chainend == 'Goal_F'), ]
#Behind_F
GWS21_B <- GWS21[ which(GWS21$chainend == 'Behind_F'), ]
#Stoppage_F
GWS21_SF <- GWS21[ which(GWS21$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS21_TF <- GWS21[ which(GWS21$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS21_SAM <- GWS21[ which(GWS21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS21_TAM <- GWS21[ which(GWS21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS21_SDM <- GWS21[ which(GWS21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS21_TDM <- GWS21[ which(GWS21$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS21_SD <- GWS21[ which(GWS21$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS21_TD <- GWS21[ which(GWS21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS21_QT <- GWS21[ which(GWS21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
GWS22_G <- GWS22[ which(GWS22$chainend == 'Goal_F'), ]
#Behind_F
GWS22_B <- GWS22[ which(GWS22$chainend == 'Behind_F'), ]
#Stoppage_F
GWS22_SF <- GWS22[ which(GWS22$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS22_TF <- GWS22[ which(GWS22$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS22_SAM <- GWS22[ which(GWS22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS22_TAM <- GWS22[ which(GWS22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS22_SDM <- GWS22[ which(GWS22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS22_TDM <- GWS22[ which(GWS22$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS22_SD <- GWS22[ which(GWS22$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS22_TD <- GWS22[ which(GWS22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS22_QT <- GWS22[ which(GWS22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
GWS23_G <- GWS23[ which(GWS23$chainend == 'Goal_F'), ]
#Behind_F
GWS23_B <- GWS23[ which(GWS23$chainend == 'Behind_F'), ]
#Stoppage_F
GWS23_SF <- GWS23[ which(GWS23$chainend == 'Stoppage_F'), ]
#Turnover_F
GWS23_TF <- GWS23[ which(GWS23$chainend == 'Turnover_F'), ]
#Stoppage_AM
GWS23_SAM <- GWS23[ which(GWS23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
GWS23_TAM <- GWS23[ which(GWS23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
GWS23_SDM <- GWS23[ which(GWS23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
GWS23_TDM <- GWS23[ which(GWS23$chainend == 'Turnover_DM'), ]
#Stoppage_D
GWS23_SD <- GWS23[ which(GWS23$chainend == 'Stoppage_D'), ]
#Turnover_D
GWS23_TD <- GWS23[ which(GWS23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
GWS23_QT <- GWS23[ which(GWS23$chainend == 'End of Qtr_DM'), ]

#Hawthorn

#Split by rounds
HAW01 <- HAW[ which(HAW$round == '01'), ]
HAW02 <- HAW[ which(HAW$round == '02'), ]
HAW03 <- HAW[ which(HAW$round == '03'), ]
HAW04 <- HAW[ which(HAW$round == '04'), ]
HAW05 <- HAW[ which(HAW$round == '05'), ]
HAW06 <- HAW[ which(HAW$round == '06'), ]
HAW07 <- HAW[ which(HAW$round == '07'), ]
HAW08 <- HAW[ which(HAW$round == '08'), ]
HAW09 <- HAW[ which(HAW$round == '09'), ]
HAW10 <- HAW[ which(HAW$round == '10'), ]
HAW11 <- HAW[ which(HAW$round == '11'), ]
HAW12 <- HAW[ which(HAW$round == '12'), ]
HAW13 <- HAW[ which(HAW$round == '13'), ]
HAW14 <- HAW[ which(HAW$round == '14'), ]
HAW15 <- HAW[ which(HAW$round == '15'), ]
HAW16 <- HAW[ which(HAW$round == '16'), ]
HAW17 <- HAW[ which(HAW$round == '17'), ]
HAW18 <- HAW[ which(HAW$round == '18'), ]
HAW19 <- HAW[ which(HAW$round == '19'), ]
HAW20 <- HAW[ which(HAW$round == '20'), ]
HAW21 <- HAW[ which(HAW$round == '21'), ]
HAW22 <- HAW[ which(HAW$round == '22'), ]
HAW23 <- HAW[ which(HAW$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
HAW01_G <- HAW01[ which(HAW01$chainend == 'Goal_F'), ]
#Behind_F
HAW01_B <- HAW01[ which(HAW01$chainend == 'Behind_F'), ]
#Stoppage_F
HAW01_SF <- HAW01[ which(HAW01$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW01_TF <- HAW01[ which(HAW01$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW01_SAM <- HAW01[ which(HAW01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW01_TAM <- HAW01[ which(HAW01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW01_SDM <- HAW01[ which(HAW01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW01_TDM <- HAW01[ which(HAW01$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW01_SD <- HAW01[ which(HAW01$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW01_TD <- HAW01[ which(HAW01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW01_QT <- HAW01[ which(HAW01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
HAW02_G <- HAW02[ which(HAW02$chainend == 'Goal_F'), ]
#Behind_F
HAW02_B <- HAW02[ which(HAW02$chainend == 'Behind_F'), ]
#Stoppage_F
HAW02_SF <- HAW02[ which(HAW02$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW02_TF <- HAW02[ which(HAW02$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW02_SAM <- HAW02[ which(HAW02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW02_TAM <- HAW02[ which(HAW02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW02_SDM <- HAW02[ which(HAW02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW02_TDM <- HAW02[ which(HAW02$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW02_SD <- HAW02[ which(HAW02$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW02_TD <- HAW02[ which(HAW02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW02_QT <- HAW02[ which(HAW02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
HAW03_G <- HAW03[ which(HAW03$chainend == 'Goal_F'), ]
#Behind_F
HAW03_B <- HAW03[ which(HAW03$chainend == 'Behind_F'), ]
#Stoppage_F
HAW03_SF <- HAW03[ which(HAW03$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW03_TF <- HAW03[ which(HAW03$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW03_SAM <- HAW03[ which(HAW03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW03_TAM <- HAW03[ which(HAW03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW03_SDM <- HAW03[ which(HAW03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW03_TDM <- HAW03[ which(HAW03$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW03_SD <- HAW03[ which(HAW03$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW03_TD <- HAW03[ which(HAW03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW03_QT <- HAW03[ which(HAW03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
HAW04_G <- HAW04[ which(HAW04$chainend == 'Goal_F'), ]
#Behind_F
HAW04_B <- HAW04[ which(HAW04$chainend == 'Behind_F'), ]
#Stoppage_F
HAW04_SF <- HAW04[ which(HAW04$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW04_TF <- HAW04[ which(HAW04$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW04_SAM <- HAW04[ which(HAW04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW04_TAM <- HAW04[ which(HAW04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW04_SDM <- HAW04[ which(HAW04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW04_TDM <- HAW04[ which(HAW04$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW04_SD <- HAW04[ which(HAW04$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW04_TD <- HAW04[ which(HAW04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW04_QT <- HAW04[ which(HAW04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
HAW05_G <- HAW05[ which(HAW05$chainend == 'Goal_F'), ]
#Behind_F
HAW05_B <- HAW05[ which(HAW05$chainend == 'Behind_F'), ]
#Stoppage_F
HAW05_SF <- HAW05[ which(HAW05$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW05_TF <- HAW05[ which(HAW05$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW05_SAM <- HAW05[ which(HAW05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW05_TAM <- HAW05[ which(HAW05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW05_SDM <- HAW05[ which(HAW05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW05_TDM <- HAW05[ which(HAW05$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW05_SD <- HAW05[ which(HAW05$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW05_TD <- HAW05[ which(HAW05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW05_QT <- HAW05[ which(HAW05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
HAW06_G <- HAW06[ which(HAW06$chainend == 'Goal_F'), ]
#Behind_F
HAW06_B <- HAW06[ which(HAW06$chainend == 'Behind_F'), ]
#Stoppage_F
HAW06_SF <- HAW06[ which(HAW06$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW06_TF <- HAW06[ which(HAW06$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW06_SAM <- HAW06[ which(HAW06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW06_TAM <- HAW06[ which(HAW06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW06_SDM <- HAW06[ which(HAW06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW06_TDM <- HAW06[ which(HAW06$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW06_SD <- HAW06[ which(HAW06$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW06_TD <- HAW06[ which(HAW06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW06_QT <- HAW06[ which(HAW06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
HAW07_G <- HAW07[ which(HAW07$chainend == 'Goal_F'), ]
#Behind_F
HAW07_B <- HAW07[ which(HAW07$chainend == 'Behind_F'), ]
#Stoppage_F
HAW07_SF <- HAW07[ which(HAW07$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW07_TF <- HAW07[ which(HAW07$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW07_SAM <- HAW07[ which(HAW07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW07_TAM <- HAW07[ which(HAW07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW07_SDM <- HAW07[ which(HAW07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW07_TDM <- HAW07[ which(HAW07$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW07_SD <- HAW07[ which(HAW07$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW07_TD <- HAW07[ which(HAW07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW07_QT <- HAW07[ which(HAW07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
HAW08_G <- HAW08[ which(HAW08$chainend == 'Goal_F'), ]
#Behind_F
HAW08_B <- HAW08[ which(HAW08$chainend == 'Behind_F'), ]
#Stoppage_F
HAW08_SF <- HAW08[ which(HAW08$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW08_TF <- HAW08[ which(HAW08$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW08_SAM <- HAW08[ which(HAW08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW08_TAM <- HAW08[ which(HAW08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW08_SDM <- HAW08[ which(HAW08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW08_TDM <- HAW08[ which(HAW08$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW08_SD <- HAW08[ which(HAW08$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW08_TD <- HAW08[ which(HAW08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW08_QT <- HAW08[ which(HAW08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
HAW09_G <- HAW09[ which(HAW09$chainend == 'Goal_F'), ]
#Behind_F
HAW09_B <- HAW09[ which(HAW09$chainend == 'Behind_F'), ]
#Stoppage_F
HAW09_SF <- HAW09[ which(HAW09$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW09_TF <- HAW09[ which(HAW09$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW09_SAM <- HAW09[ which(HAW09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW09_TAM <- HAW09[ which(HAW09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW09_SDM <- HAW09[ which(HAW09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW09_TDM <- HAW09[ which(HAW09$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW09_SD <- HAW09[ which(HAW09$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW09_TD <- HAW09[ which(HAW09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW09_QT <- HAW09[ which(HAW09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
HAW10_G <- HAW10[ which(HAW10$chainend == 'Goal_F'), ]
#Behind_F
HAW10_B <- HAW10[ which(HAW10$chainend == 'Behind_F'), ]
#Stoppage_F
HAW10_SF <- HAW10[ which(HAW10$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW10_TF <- HAW10[ which(HAW10$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW10_SAM <- HAW10[ which(HAW10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW10_TAM <- HAW10[ which(HAW10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW10_SDM <- HAW10[ which(HAW10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW10_TDM <- HAW10[ which(HAW10$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW10_SD <- HAW10[ which(HAW10$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW10_TD <- HAW10[ which(HAW10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW10_QT <- HAW10[ which(HAW10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
HAW11_G <- HAW11[ which(HAW11$chainend == 'Goal_F'), ]
#Behind_F
HAW11_B <- HAW11[ which(HAW11$chainend == 'Behind_F'), ]
#Stoppage_F
HAW11_SF <- HAW11[ which(HAW11$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW11_TF <- HAW11[ which(HAW11$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW11_SAM <- HAW11[ which(HAW11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW11_TAM <- HAW11[ which(HAW11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW11_SDM <- HAW11[ which(HAW11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW11_TDM <- HAW11[ which(HAW11$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW11_SD <- HAW11[ which(HAW11$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW11_TD <- HAW11[ which(HAW11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW11_QT <- HAW11[ which(HAW11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
HAW12_G <- HAW12[ which(HAW12$chainend == 'Goal_F'), ]
#Behind_F
HAW12_B <- HAW12[ which(HAW12$chainend == 'Behind_F'), ]
#Stoppage_F
HAW12_SF <- HAW12[ which(HAW12$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW12_TF <- HAW12[ which(HAW12$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW12_SAM <- HAW12[ which(HAW12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW12_TAM <- HAW12[ which(HAW12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW12_SDM <- HAW12[ which(HAW12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW12_TDM <- HAW12[ which(HAW12$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW12_SD <- HAW12[ which(HAW12$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW12_TD <- HAW12[ which(HAW12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW12_QT <- HAW12[ which(HAW12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
HAW13_G <- HAW13[ which(HAW13$chainend == 'Goal_F'), ]
#Behind_F
HAW13_B <- HAW13[ which(HAW13$chainend == 'Behind_F'), ]
#Stoppage_F
HAW13_SF <- HAW13[ which(HAW13$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW13_TF <- HAW13[ which(HAW13$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW13_SAM <- HAW13[ which(HAW13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW13_TAM <- HAW13[ which(HAW13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW13_SDM <- HAW13[ which(HAW13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW13_TDM <- HAW13[ which(HAW13$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW13_SD <- HAW13[ which(HAW13$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW13_TD <- HAW13[ which(HAW13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW13_QT <- HAW13[ which(HAW13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
HAW14_G <- HAW14[ which(HAW14$chainend == 'Goal_F'), ]
#Behind_F
HAW14_B <- HAW14[ which(HAW14$chainend == 'Behind_F'), ]
#Stoppage_F
HAW14_SF <- HAW14[ which(HAW14$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW14_TF <- HAW14[ which(HAW14$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW14_SAM <- HAW14[ which(HAW14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW14_TAM <- HAW14[ which(HAW14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW14_SDM <- HAW14[ which(HAW14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW14_TDM <- HAW14[ which(HAW14$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW14_SD <- HAW14[ which(HAW14$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW14_TD <- HAW14[ which(HAW14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW14_QT <- HAW14[ which(HAW14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
HAW15_G <- HAW15[ which(HAW15$chainend == 'Goal_F'), ]
#Behind_F
HAW15_B <- HAW15[ which(HAW15$chainend == 'Behind_F'), ]
#Stoppage_F
HAW15_SF <- HAW15[ which(HAW15$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW15_TF <- HAW15[ which(HAW15$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW15_SAM <- HAW15[ which(HAW15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW15_TAM <- HAW15[ which(HAW15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW15_SDM <- HAW15[ which(HAW15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW15_TDM <- HAW15[ which(HAW15$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW15_SD <- HAW15[ which(HAW15$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW15_TD <- HAW15[ which(HAW15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW15_QT <- HAW15[ which(HAW15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
HAW16_G <- HAW16[ which(HAW16$chainend == 'Goal_F'), ]
#Behind_F
HAW16_B <- HAW16[ which(HAW16$chainend == 'Behind_F'), ]
#Stoppage_F
HAW16_SF <- HAW16[ which(HAW16$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW16_TF <- HAW16[ which(HAW16$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW16_SAM <- HAW16[ which(HAW16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW16_TAM <- HAW16[ which(HAW16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW16_SDM <- HAW16[ which(HAW16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW16_TDM <- HAW16[ which(HAW16$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW16_SD <- HAW16[ which(HAW16$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW16_TD <- HAW16[ which(HAW16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW16_QT <- HAW16[ which(HAW16$chainend == 'End of Qtr_DM'), ]
#RushedOppo_D
HAW16_RO <- HAW16[ which(HAW16$chainend == 'RushedOpp_D'),]

#Round 17:
#Goal_F
HAW17_G <- HAW17[ which(HAW17$chainend == 'Goal_F'), ]
#Behind_F
HAW17_B <- HAW17[ which(HAW17$chainend == 'Behind_F'), ]
#Stoppage_F
HAW17_SF <- HAW17[ which(HAW17$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW17_TF <- HAW17[ which(HAW17$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW17_SAM <- HAW17[ which(HAW17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW17_TAM <- HAW17[ which(HAW17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW17_SDM <- HAW17[ which(HAW17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW17_TDM <- HAW17[ which(HAW17$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW17_SD <- HAW17[ which(HAW17$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW17_TD <- HAW17[ which(HAW17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW17_QT <- HAW17[ which(HAW17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
HAW18_G <- HAW18[ which(HAW18$chainend == 'Goal_F'), ]
#Behind_F
HAW18_B <- HAW18[ which(HAW18$chainend == 'Behind_F'), ]
#Stoppage_F
HAW18_SF <- HAW18[ which(HAW18$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW18_TF <- HAW18[ which(HAW18$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW18_SAM <- HAW18[ which(HAW18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW18_TAM <- HAW18[ which(HAW18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW18_SDM <- HAW18[ which(HAW18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW18_TDM <- HAW18[ which(HAW18$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW18_SD <- HAW18[ which(HAW18$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW18_TD <- HAW18[ which(HAW18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW18_QT <- HAW18[ which(HAW18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
HAW19_G <- HAW19[ which(HAW19$chainend == 'Goal_F'), ]
#Behind_F
HAW19_B <- HAW19[ which(HAW19$chainend == 'Behind_F'), ]
#Stoppage_F
HAW19_SF <- HAW19[ which(HAW19$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW19_TF <- HAW19[ which(HAW19$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW19_SAM <- HAW19[ which(HAW19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW19_TAM <- HAW19[ which(HAW19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW19_SDM <- HAW19[ which(HAW19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW19_TDM <- HAW19[ which(HAW19$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW19_SD <- HAW19[ which(HAW19$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW19_TD <- HAW19[ which(HAW19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW19_QT <- HAW19[ which(HAW19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
HAW20_G <- HAW20[ which(HAW20$chainend == 'Goal_F'), ]
#Behind_F
HAW20_B <- HAW20[ which(HAW20$chainend == 'Behind_F'), ]
#Stoppage_F
HAW20_SF <- HAW20[ which(HAW20$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW20_TF <- HAW20[ which(HAW20$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW20_SAM <- HAW20[ which(HAW20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW20_TAM <- HAW20[ which(HAW20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW20_SDM <- HAW20[ which(HAW20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW20_TDM <- HAW20[ which(HAW20$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW20_SD <- HAW20[ which(HAW20$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW20_TD <- HAW20[ which(HAW20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW20_QT <- HAW20[ which(HAW20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
HAW21_G <- HAW21[ which(HAW21$chainend == 'Goal_F'), ]
#Behind_F
HAW21_B <- HAW21[ which(HAW21$chainend == 'Behind_F'), ]
#Stoppage_F
HAW21_SF <- HAW21[ which(HAW21$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW21_TF <- HAW21[ which(HAW21$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW21_SAM <- HAW21[ which(HAW21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW21_TAM <- HAW21[ which(HAW21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW21_SDM <- HAW21[ which(HAW21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW21_TDM <- HAW21[ which(HAW21$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW21_SD <- HAW21[ which(HAW21$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW21_TD <- HAW21[ which(HAW21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW21_QT <- HAW21[ which(HAW21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
HAW22_G <- HAW22[ which(HAW22$chainend == 'Goal_F'), ]
#Behind_F
HAW22_B <- HAW22[ which(HAW22$chainend == 'Behind_F'), ]
#Stoppage_F
HAW22_SF <- HAW22[ which(HAW22$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW22_TF <- HAW22[ which(HAW22$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW22_SAM <- HAW22[ which(HAW22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW22_TAM <- HAW22[ which(HAW22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW22_SDM <- HAW22[ which(HAW22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW22_TDM <- HAW22[ which(HAW22$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW22_SD <- HAW22[ which(HAW22$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW22_TD <- HAW22[ which(HAW22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW22_QT <- HAW22[ which(HAW22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
HAW23_G <- HAW23[ which(HAW23$chainend == 'Goal_F'), ]
#Behind_F
HAW23_B <- HAW23[ which(HAW23$chainend == 'Behind_F'), ]
#Stoppage_F
HAW23_SF <- HAW23[ which(HAW23$chainend == 'Stoppage_F'), ]
#Turnover_F
HAW23_TF <- HAW23[ which(HAW23$chainend == 'Turnover_F'), ]
#Stoppage_AM
HAW23_SAM <- HAW23[ which(HAW23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
HAW23_TAM <- HAW23[ which(HAW23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
HAW23_SDM <- HAW23[ which(HAW23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
HAW23_TDM <- HAW23[ which(HAW23$chainend == 'Turnover_DM'), ]
#Stoppage_D
HAW23_SD <- HAW23[ which(HAW23$chainend == 'Stoppage_D'), ]
#Turnover_D
HAW23_TD <- HAW23[ which(HAW23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
HAW23_QT <- HAW23[ which(HAW23$chainend == 'End of Qtr_DM'), ]

#Melbourne

#Split by rounds
MELB01 <- MELB[ which(MELB$round == '01'), ]
MELB02 <- MELB[ which(MELB$round == '02'), ]
MELB03 <- MELB[ which(MELB$round == '03'), ]
MELB04 <- MELB[ which(MELB$round == '04'), ]
MELB05 <- MELB[ which(MELB$round == '05'), ]
MELB06 <- MELB[ which(MELB$round == '06'), ]
MELB07 <- MELB[ which(MELB$round == '07'), ]
MELB08 <- MELB[ which(MELB$round == '08'), ]
MELB09 <- MELB[ which(MELB$round == '09'), ]
MELB10 <- MELB[ which(MELB$round == '10'), ]
MELB11 <- MELB[ which(MELB$round == '11'), ]
MELB12 <- MELB[ which(MELB$round == '12'), ]
MELB13 <- MELB[ which(MELB$round == '13'), ]
MELB14 <- MELB[ which(MELB$round == '14'), ]
MELB15 <- MELB[ which(MELB$round == '15'), ]
MELB16 <- MELB[ which(MELB$round == '16'), ]
MELB17 <- MELB[ which(MELB$round == '17'), ]
MELB18 <- MELB[ which(MELB$round == '18'), ]
MELB19 <- MELB[ which(MELB$round == '19'), ]
MELB20 <- MELB[ which(MELB$round == '20'), ]
MELB21 <- MELB[ which(MELB$round == '21'), ]
MELB22 <- MELB[ which(MELB$round == '22'), ]
MELB23 <- MELB[ which(MELB$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
MELB01_G <- MELB01[ which(MELB01$chainend == 'Goal_F'), ]
#Behind_F
MELB01_B <- MELB01[ which(MELB01$chainend == 'Behind_F'), ]
#Stoppage_F
MELB01_SF <- MELB01[ which(MELB01$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB01_TF <- MELB01[ which(MELB01$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB01_SAM <- MELB01[ which(MELB01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB01_TAM <- MELB01[ which(MELB01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB01_SDM <- MELB01[ which(MELB01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB01_TDM <- MELB01[ which(MELB01$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB01_SD <- MELB01[ which(MELB01$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB01_TD <- MELB01[ which(MELB01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB01_QT <- MELB01[ which(MELB01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
MELB02_G <- MELB02[ which(MELB02$chainend == 'Goal_F'), ]
#Behind_F
MELB02_B <- MELB02[ which(MELB02$chainend == 'Behind_F'), ]
#Stoppage_F
MELB02_SF <- MELB02[ which(MELB02$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB02_TF <- MELB02[ which(MELB02$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB02_SAM <- MELB02[ which(MELB02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB02_TAM <- MELB02[ which(MELB02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB02_SDM <- MELB02[ which(MELB02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB02_TDM <- MELB02[ which(MELB02$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB02_SD <- MELB02[ which(MELB02$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB02_TD <- MELB02[ which(MELB02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB02_QT <- MELB02[ which(MELB02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
MELB03_G <- MELB03[ which(MELB03$chainend == 'Goal_F'), ]
#Behind_F
MELB03_B <- MELB03[ which(MELB03$chainend == 'Behind_F'), ]
#Stoppage_F
MELB03_SF <- MELB03[ which(MELB03$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB03_TF <- MELB03[ which(MELB03$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB03_SAM <- MELB03[ which(MELB03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB03_TAM <- MELB03[ which(MELB03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB03_SDM <- MELB03[ which(MELB03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB03_TDM <- MELB03[ which(MELB03$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB03_SD <- MELB03[ which(MELB03$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB03_TD <- MELB03[ which(MELB03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB03_QT <- MELB03[ which(MELB03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
MELB04_G <- MELB04[ which(MELB04$chainend == 'Goal_F'), ]
#Behind_F
MELB04_B <- MELB04[ which(MELB04$chainend == 'Behind_F'), ]
#Stoppage_F
MELB04_SF <- MELB04[ which(MELB04$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB04_TF <- MELB04[ which(MELB04$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB04_SAM <- MELB04[ which(MELB04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB04_TAM <- MELB04[ which(MELB04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB04_SDM <- MELB04[ which(MELB04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB04_TDM <- MELB04[ which(MELB04$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB04_SD <- MELB04[ which(MELB04$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB04_TD <- MELB04[ which(MELB04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB04_QT <- MELB04[ which(MELB04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
MELB05_G <- MELB05[ which(MELB05$chainend == 'Goal_F'), ]
#Behind_F
MELB05_B <- MELB05[ which(MELB05$chainend == 'Behind_F'), ]
#Stoppage_F
MELB05_SF <- MELB05[ which(MELB05$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB05_TF <- MELB05[ which(MELB05$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB05_SAM <- MELB05[ which(MELB05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB05_TAM <- MELB05[ which(MELB05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB05_SDM <- MELB05[ which(MELB05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB05_TDM <- MELB05[ which(MELB05$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB05_SD <- MELB05[ which(MELB05$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB05_TD <- MELB05[ which(MELB05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB05_QT <- MELB05[ which(MELB05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
MELB06_G <- MELB06[ which(MELB06$chainend == 'Goal_F'), ]
#Behind_F
MELB06_B <- MELB06[ which(MELB06$chainend == 'Behind_F'), ]
#Stoppage_F
MELB06_SF <- MELB06[ which(MELB06$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB06_TF <- MELB06[ which(MELB06$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB06_SAM <- MELB06[ which(MELB06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB06_TAM <- MELB06[ which(MELB06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB06_SDM <- MELB06[ which(MELB06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB06_TDM <- MELB06[ which(MELB06$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB06_SD <- MELB06[ which(MELB06$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB06_TD <- MELB06[ which(MELB06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB06_QT <- MELB06[ which(MELB06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
MELB07_G <- MELB07[ which(MELB07$chainend == 'Goal_F'), ]
#Behind_F
MELB07_B <- MELB07[ which(MELB07$chainend == 'Behind_F'), ]
#Stoppage_F
MELB07_SF <- MELB07[ which(MELB07$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB07_TF <- MELB07[ which(MELB07$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB07_SAM <- MELB07[ which(MELB07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB07_TAM <- MELB07[ which(MELB07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB07_SDM <- MELB07[ which(MELB07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB07_TDM <- MELB07[ which(MELB07$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB07_SD <- MELB07[ which(MELB07$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB07_TD <- MELB07[ which(MELB07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB07_QT <- MELB07[ which(MELB07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
MELB08_G <- MELB08[ which(MELB08$chainend == 'Goal_F'), ]
#Behind_F
MELB08_B <- MELB08[ which(MELB08$chainend == 'Behind_F'), ]
#Stoppage_F
MELB08_SF <- MELB08[ which(MELB08$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB08_TF <- MELB08[ which(MELB08$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB08_SAM <- MELB08[ which(MELB08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB08_TAM <- MELB08[ which(MELB08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB08_SDM <- MELB08[ which(MELB08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB08_TDM <- MELB08[ which(MELB08$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB08_SD <- MELB08[ which(MELB08$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB08_TD <- MELB08[ which(MELB08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB08_QT <- MELB08[ which(MELB08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
MELB09_G <- MELB09[ which(MELB09$chainend == 'Goal_F'), ]
#Behind_F
MELB09_B <- MELB09[ which(MELB09$chainend == 'Behind_F'), ]
#Stoppage_F
MELB09_SF <- MELB09[ which(MELB09$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB09_TF <- MELB09[ which(MELB09$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB09_SAM <- MELB09[ which(MELB09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB09_TAM <- MELB09[ which(MELB09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB09_SDM <- MELB09[ which(MELB09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB09_TDM <- MELB09[ which(MELB09$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB09_SD <- MELB09[ which(MELB09$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB09_TD <- MELB09[ which(MELB09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB09_QT <- MELB09[ which(MELB09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
MELB10_G <- MELB10[ which(MELB10$chainend == 'Goal_F'), ]
#Behind_F
MELB10_B <- MELB10[ which(MELB10$chainend == 'Behind_F'), ]
#Stoppage_F
MELB10_SF <- MELB10[ which(MELB10$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB10_TF <- MELB10[ which(MELB10$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB10_SAM <- MELB10[ which(MELB10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB10_TAM <- MELB10[ which(MELB10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB10_SDM <- MELB10[ which(MELB10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB10_TDM <- MELB10[ which(MELB10$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB10_SD <- MELB10[ which(MELB10$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB10_TD <- MELB10[ which(MELB10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB10_QT <- MELB10[ which(MELB10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
MELB11_G <- MELB11[ which(MELB11$chainend == 'Goal_F'), ]
#Behind_F
MELB11_B <- MELB11[ which(MELB11$chainend == 'Behind_F'), ]
#Stoppage_F
MELB11_SF <- MELB11[ which(MELB11$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB11_TF <- MELB11[ which(MELB11$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB11_SAM <- MELB11[ which(MELB11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB11_TAM <- MELB11[ which(MELB11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB11_SDM <- MELB11[ which(MELB11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB11_TDM <- MELB11[ which(MELB11$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB11_SD <- MELB11[ which(MELB11$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB11_TD <- MELB11[ which(MELB11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB11_QT <- MELB11[ which(MELB11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
MELB12_G <- MELB12[ which(MELB12$chainend == 'Goal_F'), ]
#Behind_F
MELB12_B <- MELB12[ which(MELB12$chainend == 'Behind_F'), ]
#Stoppage_F
MELB12_SF <- MELB12[ which(MELB12$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB12_TF <- MELB12[ which(MELB12$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB12_SAM <- MELB12[ which(MELB12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB12_TAM <- MELB12[ which(MELB12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB12_SDM <- MELB12[ which(MELB12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB12_TDM <- MELB12[ which(MELB12$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB12_SD <- MELB12[ which(MELB12$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB12_TD <- MELB12[ which(MELB12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB12_QT <- MELB12[ which(MELB12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
MELB13_G <- MELB13[ which(MELB13$chainend == 'Goal_F'), ]
#Behind_F
MELB13_B <- MELB13[ which(MELB13$chainend == 'Behind_F'), ]
#Stoppage_F
MELB13_SF <- MELB13[ which(MELB13$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB13_TF <- MELB13[ which(MELB13$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB13_SAM <- MELB13[ which(MELB13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB13_TAM <- MELB13[ which(MELB13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB13_SDM <- MELB13[ which(MELB13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB13_TDM <- MELB13[ which(MELB13$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB13_SD <- MELB13[ which(MELB13$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB13_TD <- MELB13[ which(MELB13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB13_QT <- MELB13[ which(MELB13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
MELB14_G <- MELB14[ which(MELB14$chainend == 'Goal_F'), ]
#Behind_F
MELB14_B <- MELB14[ which(MELB14$chainend == 'Behind_F'), ]
#Stoppage_F
MELB14_SF <- MELB14[ which(MELB14$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB14_TF <- MELB14[ which(MELB14$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB14_SAM <- MELB14[ which(MELB14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB14_TAM <- MELB14[ which(MELB14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB14_SDM <- MELB14[ which(MELB14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB14_TDM <- MELB14[ which(MELB14$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB14_SD <- MELB14[ which(MELB14$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB14_TD <- MELB14[ which(MELB14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB14_QT <- MELB14[ which(MELB14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
MELB15_G <- MELB15[ which(MELB15$chainend == 'Goal_F'), ]
#Behind_F
MELB15_B <- MELB15[ which(MELB15$chainend == 'Behind_F'), ]
#Stoppage_F
MELB15_SF <- MELB15[ which(MELB15$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB15_TF <- MELB15[ which(MELB15$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB15_SAM <- MELB15[ which(MELB15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB15_TAM <- MELB15[ which(MELB15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB15_SDM <- MELB15[ which(MELB15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB15_TDM <- MELB15[ which(MELB15$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB15_SD <- MELB15[ which(MELB15$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB15_TD <- MELB15[ which(MELB15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB15_QT <- MELB15[ which(MELB15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
MELB16_G <- MELB16[ which(MELB16$chainend == 'Goal_F'), ]
#Behind_F
MELB16_B <- MELB16[ which(MELB16$chainend == 'Behind_F'), ]
#Stoppage_F
MELB16_SF <- MELB16[ which(MELB16$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB16_TF <- MELB16[ which(MELB16$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB16_SAM <- MELB16[ which(MELB16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB16_TAM <- MELB16[ which(MELB16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB16_SDM <- MELB16[ which(MELB16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB16_TDM <- MELB16[ which(MELB16$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB16_SD <- MELB16[ which(MELB16$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB16_TD <- MELB16[ which(MELB16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB16_QT <- MELB16[ which(MELB16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
MELB17_G <- MELB17[ which(MELB17$chainend == 'Goal_F'), ]
#Behind_F
MELB17_B <- MELB17[ which(MELB17$chainend == 'Behind_F'), ]
#Stoppage_F
MELB17_SF <- MELB17[ which(MELB17$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB17_TF <- MELB17[ which(MELB17$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB17_SAM <- MELB17[ which(MELB17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB17_TAM <- MELB17[ which(MELB17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB17_SDM <- MELB17[ which(MELB17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB17_TDM <- MELB17[ which(MELB17$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB17_SD <- MELB17[ which(MELB17$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB17_TD <- MELB17[ which(MELB17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB17_QT <- MELB17[ which(MELB17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
MELB18_G <- MELB18[ which(MELB18$chainend == 'Goal_F'), ]
#Behind_F
MELB18_B <- MELB18[ which(MELB18$chainend == 'Behind_F'), ]
#Stoppage_F
MELB18_SF <- MELB18[ which(MELB18$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB18_TF <- MELB18[ which(MELB18$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB18_SAM <- MELB18[ which(MELB18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB18_TAM <- MELB18[ which(MELB18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB18_SDM <- MELB18[ which(MELB18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB18_TDM <- MELB18[ which(MELB18$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB18_SD <- MELB18[ which(MELB18$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB18_TD <- MELB18[ which(MELB18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB18_QT <- MELB18[ which(MELB18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
MELB19_G <- MELB19[ which(MELB19$chainend == 'Goal_F'), ]
#Behind_F
MELB19_B <- MELB19[ which(MELB19$chainend == 'Behind_F'), ]
#Stoppage_F
MELB19_SF <- MELB19[ which(MELB19$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB19_TF <- MELB19[ which(MELB19$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB19_SAM <- MELB19[ which(MELB19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB19_TAM <- MELB19[ which(MELB19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB19_SDM <- MELB19[ which(MELB19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB19_TDM <- MELB19[ which(MELB19$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB19_SD <- MELB19[ which(MELB19$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB19_TD <- MELB19[ which(MELB19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB19_QT <- MELB19[ which(MELB19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
MELB20_G <- MELB20[ which(MELB20$chainend == 'Goal_F'), ]
#Behind_F
MELB20_B <- MELB20[ which(MELB20$chainend == 'Behind_F'), ]
#Stoppage_F
MELB20_SF <- MELB20[ which(MELB20$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB20_TF <- MELB20[ which(MELB20$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB20_SAM <- MELB20[ which(MELB20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB20_TAM <- MELB20[ which(MELB20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB20_SDM <- MELB20[ which(MELB20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB20_TDM <- MELB20[ which(MELB20$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB20_SD <- MELB20[ which(MELB20$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB20_TD <- MELB20[ which(MELB20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB20_QT <- MELB20[ which(MELB20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
MELB21_G <- MELB21[ which(MELB21$chainend == 'Goal_F'), ]
#Behind_F
MELB21_B <- MELB21[ which(MELB21$chainend == 'Behind_F'), ]
#Stoppage_F
MELB21_SF <- MELB21[ which(MELB21$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB21_TF <- MELB21[ which(MELB21$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB21_SAM <- MELB21[ which(MELB21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB21_TAM <- MELB21[ which(MELB21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB21_SDM <- MELB21[ which(MELB21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB21_TDM <- MELB21[ which(MELB21$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB21_SD <- MELB21[ which(MELB21$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB21_TD <- MELB21[ which(MELB21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB21_QT <- MELB21[ which(MELB21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
MELB22_G <- MELB22[ which(MELB22$chainend == 'Goal_F'), ]
#Behind_F
MELB22_B <- MELB22[ which(MELB22$chainend == 'Behind_F'), ]
#Stoppage_F
MELB22_SF <- MELB22[ which(MELB22$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB22_TF <- MELB22[ which(MELB22$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB22_SAM <- MELB22[ which(MELB22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB22_TAM <- MELB22[ which(MELB22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB22_SDM <- MELB22[ which(MELB22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB22_TDM <- MELB22[ which(MELB22$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB22_SD <- MELB22[ which(MELB22$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB22_TD <- MELB22[ which(MELB22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB22_QT <- MELB22[ which(MELB22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
MELB23_G <- MELB23[ which(MELB23$chainend == 'Goal_F'), ]
#Behind_F
MELB23_B <- MELB23[ which(MELB23$chainend == 'Behind_F'), ]
#Stoppage_F
MELB23_SF <- MELB23[ which(MELB23$chainend == 'Stoppage_F'), ]
#Turnover_F
MELB23_TF <- MELB23[ which(MELB23$chainend == 'Turnover_F'), ]
#Stoppage_AM
MELB23_SAM <- MELB23[ which(MELB23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
MELB23_TAM <- MELB23[ which(MELB23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
MELB23_SDM <- MELB23[ which(MELB23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
MELB23_TDM <- MELB23[ which(MELB23$chainend == 'Turnover_DM'), ]
#Stoppage_D
MELB23_SD <- MELB23[ which(MELB23$chainend == 'Stoppage_D'), ]
#Turnover_D
MELB23_TD <- MELB23[ which(MELB23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
MELB23_QT <- MELB23[ which(MELB23$chainend == 'End of Qtr_DM'), ]

#North Melbourne

#Split by rounds
NMFC01 <- NMFC[ which(NMFC$round == '01'), ]
NMFC02 <- NMFC[ which(NMFC$round == '02'), ]
NMFC03 <- NMFC[ which(NMFC$round == '03'), ]
NMFC04 <- NMFC[ which(NMFC$round == '04'), ]
NMFC05 <- NMFC[ which(NMFC$round == '05'), ]
NMFC06 <- NMFC[ which(NMFC$round == '06'), ]
NMFC07 <- NMFC[ which(NMFC$round == '07'), ]
NMFC08 <- NMFC[ which(NMFC$round == '08'), ]
NMFC09 <- NMFC[ which(NMFC$round == '09'), ]
NMFC10 <- NMFC[ which(NMFC$round == '10'), ]
NMFC11 <- NMFC[ which(NMFC$round == '11'), ]
NMFC12 <- NMFC[ which(NMFC$round == '12'), ]
NMFC13 <- NMFC[ which(NMFC$round == '13'), ]
NMFC14 <- NMFC[ which(NMFC$round == '14'), ]
NMFC15 <- NMFC[ which(NMFC$round == '15'), ]
NMFC16 <- NMFC[ which(NMFC$round == '16'), ]
NMFC17 <- NMFC[ which(NMFC$round == '17'), ]
NMFC18 <- NMFC[ which(NMFC$round == '18'), ]
NMFC19 <- NMFC[ which(NMFC$round == '19'), ]
NMFC20 <- NMFC[ which(NMFC$round == '20'), ]
NMFC21 <- NMFC[ which(NMFC$round == '21'), ]
NMFC22 <- NMFC[ which(NMFC$round == '22'), ]
NMFC23 <- NMFC[ which(NMFC$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
NMFC01_G <- NMFC01[ which(NMFC01$chainend == 'Goal_F'), ]
#Behind_F
NMFC01_B <- NMFC01[ which(NMFC01$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC01_SF <- NMFC01[ which(NMFC01$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC01_TF <- NMFC01[ which(NMFC01$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC01_SAM <- NMFC01[ which(NMFC01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC01_TAM <- NMFC01[ which(NMFC01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC01_SDM <- NMFC01[ which(NMFC01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC01_TDM <- NMFC01[ which(NMFC01$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC01_SD <- NMFC01[ which(NMFC01$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC01_TD <- NMFC01[ which(NMFC01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC01_QT <- NMFC01[ which(NMFC01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
NMFC02_G <- NMFC02[ which(NMFC02$chainend == 'Goal_F'), ]
#Behind_F
NMFC02_B <- NMFC02[ which(NMFC02$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC02_SF <- NMFC02[ which(NMFC02$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC02_TF <- NMFC02[ which(NMFC02$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC02_SAM <- NMFC02[ which(NMFC02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC02_TAM <- NMFC02[ which(NMFC02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC02_SDM <- NMFC02[ which(NMFC02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC02_TDM <- NMFC02[ which(NMFC02$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC02_SD <- NMFC02[ which(NMFC02$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC02_TD <- NMFC02[ which(NMFC02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC02_QT <- NMFC02[ which(NMFC02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
NMFC03_G <- NMFC03[ which(NMFC03$chainend == 'Goal_F'), ]
#Behind_F
NMFC03_B <- NMFC03[ which(NMFC03$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC03_SF <- NMFC03[ which(NMFC03$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC03_TF <- NMFC03[ which(NMFC03$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC03_SAM <- NMFC03[ which(NMFC03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC03_TAM <- NMFC03[ which(NMFC03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC03_SDM <- NMFC03[ which(NMFC03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC03_TDM <- NMFC03[ which(NMFC03$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC03_SD <- NMFC03[ which(NMFC03$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC03_TD <- NMFC03[ which(NMFC03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC03_QT <- NMFC03[ which(NMFC03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
NMFC04_G <- NMFC04[ which(NMFC04$chainend == 'Goal_F'), ]
#Behind_F
NMFC04_B <- NMFC04[ which(NMFC04$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC04_SF <- NMFC04[ which(NMFC04$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC04_TF <- NMFC04[ which(NMFC04$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC04_SAM <- NMFC04[ which(NMFC04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC04_TAM <- NMFC04[ which(NMFC04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC04_SDM <- NMFC04[ which(NMFC04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC04_TDM <- NMFC04[ which(NMFC04$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC04_SD <- NMFC04[ which(NMFC04$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC04_TD <- NMFC04[ which(NMFC04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC04_QT <- NMFC04[ which(NMFC04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
NMFC05_G <- NMFC05[ which(NMFC05$chainend == 'Goal_F'), ]
#Behind_F
NMFC05_B <- NMFC05[ which(NMFC05$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC05_SF <- NMFC05[ which(NMFC05$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC05_TF <- NMFC05[ which(NMFC05$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC05_SAM <- NMFC05[ which(NMFC05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC05_TAM <- NMFC05[ which(NMFC05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC05_SDM <- NMFC05[ which(NMFC05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC05_TDM <- NMFC05[ which(NMFC05$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC05_SD <- NMFC05[ which(NMFC05$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC05_TD <- NMFC05[ which(NMFC05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC05_QT <- NMFC05[ which(NMFC05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
NMFC06_G <- NMFC06[ which(NMFC06$chainend == 'Goal_F'), ]
#Behind_F
NMFC06_B <- NMFC06[ which(NMFC06$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC06_SF <- NMFC06[ which(NMFC06$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC06_TF <- NMFC06[ which(NMFC06$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC06_SAM <- NMFC06[ which(NMFC06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC06_TAM <- NMFC06[ which(NMFC06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC06_SDM <- NMFC06[ which(NMFC06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC06_TDM <- NMFC06[ which(NMFC06$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC06_SD <- NMFC06[ which(NMFC06$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC06_TD <- NMFC06[ which(NMFC06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC06_QT <- NMFC06[ which(NMFC06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
NMFC07_G <- NMFC07[ which(NMFC07$chainend == 'Goal_F'), ]
#Behind_F
NMFC07_B <- NMFC07[ which(NMFC07$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC07_SF <- NMFC07[ which(NMFC07$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC07_TF <- NMFC07[ which(NMFC07$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC07_SAM <- NMFC07[ which(NMFC07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC07_TAM <- NMFC07[ which(NMFC07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC07_SDM <- NMFC07[ which(NMFC07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC07_TDM <- NMFC07[ which(NMFC07$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC07_SD <- NMFC07[ which(NMFC07$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC07_TD <- NMFC07[ which(NMFC07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC07_QT <- NMFC07[ which(NMFC07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
NMFC08_G <- NMFC08[ which(NMFC08$chainend == 'Goal_F'), ]
#Behind_F
NMFC08_B <- NMFC08[ which(NMFC08$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC08_SF <- NMFC08[ which(NMFC08$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC08_TF <- NMFC08[ which(NMFC08$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC08_SAM <- NMFC08[ which(NMFC08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC08_TAM <- NMFC08[ which(NMFC08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC08_SDM <- NMFC08[ which(NMFC08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC08_TDM <- NMFC08[ which(NMFC08$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC08_SD <- NMFC08[ which(NMFC08$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC08_TD <- NMFC08[ which(NMFC08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC08_QT <- NMFC08[ which(NMFC08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
NMFC09_G <- NMFC09[ which(NMFC09$chainend == 'Goal_F'), ]
#Behind_F
NMFC09_B <- NMFC09[ which(NMFC09$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC09_SF <- NMFC09[ which(NMFC09$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC09_TF <- NMFC09[ which(NMFC09$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC09_SAM <- NMFC09[ which(NMFC09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC09_TAM <- NMFC09[ which(NMFC09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC09_SDM <- NMFC09[ which(NMFC09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC09_TDM <- NMFC09[ which(NMFC09$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC09_SD <- NMFC09[ which(NMFC09$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC09_TD <- NMFC09[ which(NMFC09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC09_QT <- NMFC09[ which(NMFC09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
NMFC10_G <- NMFC10[ which(NMFC10$chainend == 'Goal_F'), ]
#Behind_F
NMFC10_B <- NMFC10[ which(NMFC10$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC10_SF <- NMFC10[ which(NMFC10$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC10_TF <- NMFC10[ which(NMFC10$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC10_SAM <- NMFC10[ which(NMFC10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC10_TAM <- NMFC10[ which(NMFC10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC10_SDM <- NMFC10[ which(NMFC10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC10_TDM <- NMFC10[ which(NMFC10$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC10_SD <- NMFC10[ which(NMFC10$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC10_TD <- NMFC10[ which(NMFC10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC10_QT <- NMFC10[ which(NMFC10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
NMFC11_G <- NMFC11[ which(NMFC11$chainend == 'Goal_F'), ]
#Behind_F
NMFC11_B <- NMFC11[ which(NMFC11$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC11_SF <- NMFC11[ which(NMFC11$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC11_TF <- NMFC11[ which(NMFC11$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC11_SAM <- NMFC11[ which(NMFC11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC11_TAM <- NMFC11[ which(NMFC11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC11_SDM <- NMFC11[ which(NMFC11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC11_TDM <- NMFC11[ which(NMFC11$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC11_SD <- NMFC11[ which(NMFC11$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC11_TD <- NMFC11[ which(NMFC11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC11_QT <- NMFC11[ which(NMFC11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
NMFC12_G <- NMFC12[ which(NMFC12$chainend == 'Goal_F'), ]
#Behind_F
NMFC12_B <- NMFC12[ which(NMFC12$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC12_SF <- NMFC12[ which(NMFC12$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC12_TF <- NMFC12[ which(NMFC12$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC12_SAM <- NMFC12[ which(NMFC12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC12_TAM <- NMFC12[ which(NMFC12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC12_SDM <- NMFC12[ which(NMFC12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC12_TDM <- NMFC12[ which(NMFC12$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC12_SD <- NMFC12[ which(NMFC12$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC12_TD <- NMFC12[ which(NMFC12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC12_QT <- NMFC12[ which(NMFC12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
NMFC13_G <- NMFC13[ which(NMFC13$chainend == 'Goal_F'), ]
#Behind_F
NMFC13_B <- NMFC13[ which(NMFC13$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC13_SF <- NMFC13[ which(NMFC13$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC13_TF <- NMFC13[ which(NMFC13$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC13_SAM <- NMFC13[ which(NMFC13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC13_TAM <- NMFC13[ which(NMFC13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC13_SDM <- NMFC13[ which(NMFC13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC13_TDM <- NMFC13[ which(NMFC13$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC13_SD <- NMFC13[ which(NMFC13$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC13_TD <- NMFC13[ which(NMFC13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC13_QT <- NMFC13[ which(NMFC13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
NMFC14_G <- NMFC14[ which(NMFC14$chainend == 'Goal_F'), ]
#Behind_F
NMFC14_B <- NMFC14[ which(NMFC14$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC14_SF <- NMFC14[ which(NMFC14$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC14_TF <- NMFC14[ which(NMFC14$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC14_SAM <- NMFC14[ which(NMFC14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC14_TAM <- NMFC14[ which(NMFC14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC14_SDM <- NMFC14[ which(NMFC14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC14_TDM <- NMFC14[ which(NMFC14$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC14_SD <- NMFC14[ which(NMFC14$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC14_TD <- NMFC14[ which(NMFC14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC14_QT <- NMFC14[ which(NMFC14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
NMFC15_G <- NMFC15[ which(NMFC15$chainend == 'Goal_F'), ]
#Behind_F
NMFC15_B <- NMFC15[ which(NMFC15$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC15_SF <- NMFC15[ which(NMFC15$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC15_TF <- NMFC15[ which(NMFC15$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC15_SAM <- NMFC15[ which(NMFC15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC15_TAM <- NMFC15[ which(NMFC15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC15_SDM <- NMFC15[ which(NMFC15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC15_TDM <- NMFC15[ which(NMFC15$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC15_SD <- NMFC15[ which(NMFC15$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC15_TD <- NMFC15[ which(NMFC15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC15_QT <- NMFC15[ which(NMFC15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
NMFC16_G <- NMFC16[ which(NMFC16$chainend == 'Goal_F'), ]
#Behind_F
NMFC16_B <- NMFC16[ which(NMFC16$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC16_SF <- NMFC16[ which(NMFC16$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC16_TF <- NMFC16[ which(NMFC16$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC16_SAM <- NMFC16[ which(NMFC16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC16_TAM <- NMFC16[ which(NMFC16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC16_SDM <- NMFC16[ which(NMFC16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC16_TDM <- NMFC16[ which(NMFC16$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC16_SD <- NMFC16[ which(NMFC16$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC16_TD <- NMFC16[ which(NMFC16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC16_QT <- NMFC16[ which(NMFC16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
NMFC17_G <- NMFC17[ which(NMFC17$chainend == 'Goal_F'), ]
#Behind_F
NMFC17_B <- NMFC17[ which(NMFC17$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC17_SF <- NMFC17[ which(NMFC17$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC17_TF <- NMFC17[ which(NMFC17$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC17_SAM <- NMFC17[ which(NMFC17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC17_TAM <- NMFC17[ which(NMFC17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC17_SDM <- NMFC17[ which(NMFC17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC17_TDM <- NMFC17[ which(NMFC17$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC17_SD <- NMFC17[ which(NMFC17$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC17_TD <- NMFC17[ which(NMFC17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC17_QT <- NMFC17[ which(NMFC17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
NMFC18_G <- NMFC18[ which(NMFC18$chainend == 'Goal_F'), ]
#Behind_F
NMFC18_B <- NMFC18[ which(NMFC18$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC18_SF <- NMFC18[ which(NMFC18$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC18_TF <- NMFC18[ which(NMFC18$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC18_SAM <- NMFC18[ which(NMFC18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC18_TAM <- NMFC18[ which(NMFC18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC18_SDM <- NMFC18[ which(NMFC18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC18_TDM <- NMFC18[ which(NMFC18$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC18_SD <- NMFC18[ which(NMFC18$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC18_TD <- NMFC18[ which(NMFC18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC18_QT <- NMFC18[ which(NMFC18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
NMFC19_G <- NMFC19[ which(NMFC19$chainend == 'Goal_F'), ]
#Behind_F
NMFC19_B <- NMFC19[ which(NMFC19$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC19_SF <- NMFC19[ which(NMFC19$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC19_TF <- NMFC19[ which(NMFC19$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC19_SAM <- NMFC19[ which(NMFC19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC19_TAM <- NMFC19[ which(NMFC19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC19_SDM <- NMFC19[ which(NMFC19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC19_TDM <- NMFC19[ which(NMFC19$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC19_SD <- NMFC19[ which(NMFC19$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC19_TD <- NMFC19[ which(NMFC19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC19_QT <- NMFC19[ which(NMFC19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
NMFC20_G <- NMFC20[ which(NMFC20$chainend == 'Goal_F'), ]
#Behind_F
NMFC20_B <- NMFC20[ which(NMFC20$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC20_SF <- NMFC20[ which(NMFC20$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC20_TF <- NMFC20[ which(NMFC20$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC20_SAM <- NMFC20[ which(NMFC20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC20_TAM <- NMFC20[ which(NMFC20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC20_SDM <- NMFC20[ which(NMFC20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC20_TDM <- NMFC20[ which(NMFC20$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC20_SD <- NMFC20[ which(NMFC20$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC20_TD <- NMFC20[ which(NMFC20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC20_QT <- NMFC20[ which(NMFC20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
NMFC21_G <- NMFC21[ which(NMFC21$chainend == 'Goal_F'), ]
#Behind_F
NMFC21_B <- NMFC21[ which(NMFC21$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC21_SF <- NMFC21[ which(NMFC21$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC21_TF <- NMFC21[ which(NMFC21$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC21_SAM <- NMFC21[ which(NMFC21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC21_TAM <- NMFC21[ which(NMFC21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC21_SDM <- NMFC21[ which(NMFC21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC21_TDM <- NMFC21[ which(NMFC21$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC21_SD <- NMFC21[ which(NMFC21$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC21_TD <- NMFC21[ which(NMFC21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC21_QT <- NMFC21[ which(NMFC21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
NMFC22_G <- NMFC22[ which(NMFC22$chainend == 'Goal_F'), ]
#Behind_F
NMFC22_B <- NMFC22[ which(NMFC22$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC22_SF <- NMFC22[ which(NMFC22$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC22_TF <- NMFC22[ which(NMFC22$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC22_SAM <- NMFC22[ which(NMFC22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC22_TAM <- NMFC22[ which(NMFC22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC22_SDM <- NMFC22[ which(NMFC22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC22_TDM <- NMFC22[ which(NMFC22$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC22_SD <- NMFC22[ which(NMFC22$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC22_TD <- NMFC22[ which(NMFC22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC22_QT <- NMFC22[ which(NMFC22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
NMFC23_G <- NMFC23[ which(NMFC23$chainend == 'Goal_F'), ]
#Behind_F
NMFC23_B <- NMFC23[ which(NMFC23$chainend == 'Behind_F'), ]
#Stoppage_F
NMFC23_SF <- NMFC23[ which(NMFC23$chainend == 'Stoppage_F'), ]
#Turnover_F
NMFC23_TF <- NMFC23[ which(NMFC23$chainend == 'Turnover_F'), ]
#Stoppage_AM
NMFC23_SAM <- NMFC23[ which(NMFC23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
NMFC23_TAM <- NMFC23[ which(NMFC23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
NMFC23_SDM <- NMFC23[ which(NMFC23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
NMFC23_TDM <- NMFC23[ which(NMFC23$chainend == 'Turnover_DM'), ]
#Stoppage_D
NMFC23_SD <- NMFC23[ which(NMFC23$chainend == 'Stoppage_D'), ]
#Turnover_D
NMFC23_TD <- NMFC23[ which(NMFC23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
NMFC23_QT <- NMFC23[ which(NMFC23$chainend == 'End of Qtr_DM'), ]

#Port Adelaide

#Split by rounds
PORT01 <- PORT[ which(PORT$round == '01'), ]
PORT02 <- PORT[ which(PORT$round == '02'), ]
PORT03 <- PORT[ which(PORT$round == '03'), ]
PORT04 <- PORT[ which(PORT$round == '04'), ]
PORT05 <- PORT[ which(PORT$round == '05'), ]
PORT06 <- PORT[ which(PORT$round == '06'), ]
PORT07 <- PORT[ which(PORT$round == '07'), ]
PORT08 <- PORT[ which(PORT$round == '08'), ]
PORT09 <- PORT[ which(PORT$round == '09'), ]
PORT10 <- PORT[ which(PORT$round == '10'), ]
PORT11 <- PORT[ which(PORT$round == '11'), ]
PORT12 <- PORT[ which(PORT$round == '12'), ]
PORT13 <- PORT[ which(PORT$round == '13'), ]
PORT14 <- PORT[ which(PORT$round == '14'), ]
PORT15 <- PORT[ which(PORT$round == '15'), ]
PORT16 <- PORT[ which(PORT$round == '16'), ]
PORT17 <- PORT[ which(PORT$round == '17'), ]
PORT18 <- PORT[ which(PORT$round == '18'), ]
PORT19 <- PORT[ which(PORT$round == '19'), ]
PORT20 <- PORT[ which(PORT$round == '20'), ]
PORT21 <- PORT[ which(PORT$round == '21'), ]
PORT22 <- PORT[ which(PORT$round == '22'), ]
PORT23 <- PORT[ which(PORT$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
PORT01_G <- PORT01[ which(PORT01$chainend == 'Goal_F'), ]
#Behind_F
PORT01_B <- PORT01[ which(PORT01$chainend == 'Behind_F'), ]
#Stoppage_F
PORT01_SF <- PORT01[ which(PORT01$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT01_TF <- PORT01[ which(PORT01$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT01_SAM <- PORT01[ which(PORT01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT01_TAM <- PORT01[ which(PORT01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT01_SDM <- PORT01[ which(PORT01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT01_TDM <- PORT01[ which(PORT01$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT01_SD <- PORT01[ which(PORT01$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT01_TD <- PORT01[ which(PORT01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT01_QT <- PORT01[ which(PORT01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
PORT02_G <- PORT02[ which(PORT02$chainend == 'Goal_F'), ]
#Behind_F
PORT02_B <- PORT02[ which(PORT02$chainend == 'Behind_F'), ]
#Stoppage_F
PORT02_SF <- PORT02[ which(PORT02$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT02_TF <- PORT02[ which(PORT02$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT02_SAM <- PORT02[ which(PORT02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT02_TAM <- PORT02[ which(PORT02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT02_SDM <- PORT02[ which(PORT02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT02_TDM <- PORT02[ which(PORT02$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT02_SD <- PORT02[ which(PORT02$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT02_TD <- PORT02[ which(PORT02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT02_QT <- PORT02[ which(PORT02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
PORT03_G <- PORT03[ which(PORT03$chainend == 'Goal_F'), ]
#Behind_F
PORT03_B <- PORT03[ which(PORT03$chainend == 'Behind_F'), ]
#Stoppage_F
PORT03_SF <- PORT03[ which(PORT03$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT03_TF <- PORT03[ which(PORT03$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT03_SAM <- PORT03[ which(PORT03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT03_TAM <- PORT03[ which(PORT03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT03_SDM <- PORT03[ which(PORT03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT03_TDM <- PORT03[ which(PORT03$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT03_SD <- PORT03[ which(PORT03$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT03_TD <- PORT03[ which(PORT03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT03_QT <- PORT03[ which(PORT03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
PORT04_G <- PORT04[ which(PORT04$chainend == 'Goal_F'), ]
#Behind_F
PORT04_B <- PORT04[ which(PORT04$chainend == 'Behind_F'), ]
#Stoppage_F
PORT04_SF <- PORT04[ which(PORT04$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT04_TF <- PORT04[ which(PORT04$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT04_SAM <- PORT04[ which(PORT04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT04_TAM <- PORT04[ which(PORT04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT04_SDM <- PORT04[ which(PORT04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT04_TDM <- PORT04[ which(PORT04$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT04_SD <- PORT04[ which(PORT04$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT04_TD <- PORT04[ which(PORT04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT04_QT <- PORT04[ which(PORT04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
PORT05_G <- PORT05[ which(PORT05$chainend == 'Goal_F'), ]
#Behind_F
PORT05_B <- PORT05[ which(PORT05$chainend == 'Behind_F'), ]
#Stoppage_F
PORT05_SF <- PORT05[ which(PORT05$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT05_TF <- PORT05[ which(PORT05$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT05_SAM <- PORT05[ which(PORT05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT05_TAM <- PORT05[ which(PORT05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT05_SDM <- PORT05[ which(PORT05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT05_TDM <- PORT05[ which(PORT05$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT05_SD <- PORT05[ which(PORT05$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT05_TD <- PORT05[ which(PORT05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT05_QT <- PORT05[ which(PORT05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
PORT06_G <- PORT06[ which(PORT06$chainend == 'Goal_F'), ]
#Behind_F
PORT06_B <- PORT06[ which(PORT06$chainend == 'Behind_F'), ]
#Stoppage_F
PORT06_SF <- PORT06[ which(PORT06$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT06_TF <- PORT06[ which(PORT06$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT06_SAM <- PORT06[ which(PORT06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT06_TAM <- PORT06[ which(PORT06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT06_SDM <- PORT06[ which(PORT06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT06_TDM <- PORT06[ which(PORT06$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT06_SD <- PORT06[ which(PORT06$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT06_TD <- PORT06[ which(PORT06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT06_QT <- PORT06[ which(PORT06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
PORT07_G <- PORT07[ which(PORT07$chainend == 'Goal_F'), ]
#Behind_F
PORT07_B <- PORT07[ which(PORT07$chainend == 'Behind_F'), ]
#Stoppage_F
PORT07_SF <- PORT07[ which(PORT07$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT07_TF <- PORT07[ which(PORT07$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT07_SAM <- PORT07[ which(PORT07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT07_TAM <- PORT07[ which(PORT07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT07_SDM <- PORT07[ which(PORT07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT07_TDM <- PORT07[ which(PORT07$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT07_SD <- PORT07[ which(PORT07$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT07_TD <- PORT07[ which(PORT07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT07_QT <- PORT07[ which(PORT07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
PORT08_G <- PORT08[ which(PORT08$chainend == 'Goal_F'), ]
#Behind_F
PORT08_B <- PORT08[ which(PORT08$chainend == 'Behind_F'), ]
#Stoppage_F
PORT08_SF <- PORT08[ which(PORT08$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT08_TF <- PORT08[ which(PORT08$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT08_SAM <- PORT08[ which(PORT08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT08_TAM <- PORT08[ which(PORT08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT08_SDM <- PORT08[ which(PORT08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT08_TDM <- PORT08[ which(PORT08$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT08_SD <- PORT08[ which(PORT08$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT08_TD <- PORT08[ which(PORT08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT08_QT <- PORT08[ which(PORT08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
PORT09_G <- PORT09[ which(PORT09$chainend == 'Goal_F'), ]
#Behind_F
PORT09_B <- PORT09[ which(PORT09$chainend == 'Behind_F'), ]
#Stoppage_F
PORT09_SF <- PORT09[ which(PORT09$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT09_TF <- PORT09[ which(PORT09$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT09_SAM <- PORT09[ which(PORT09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT09_TAM <- PORT09[ which(PORT09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT09_SDM <- PORT09[ which(PORT09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT09_TDM <- PORT09[ which(PORT09$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT09_SD <- PORT09[ which(PORT09$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT09_TD <- PORT09[ which(PORT09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT09_QT <- PORT09[ which(PORT09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
PORT10_G <- PORT10[ which(PORT10$chainend == 'Goal_F'), ]
#Behind_F
PORT10_B <- PORT10[ which(PORT10$chainend == 'Behind_F'), ]
#Stoppage_F
PORT10_SF <- PORT10[ which(PORT10$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT10_TF <- PORT10[ which(PORT10$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT10_SAM <- PORT10[ which(PORT10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT10_TAM <- PORT10[ which(PORT10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT10_SDM <- PORT10[ which(PORT10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT10_TDM <- PORT10[ which(PORT10$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT10_SD <- PORT10[ which(PORT10$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT10_TD <- PORT10[ which(PORT10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT10_QT <- PORT10[ which(PORT10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
PORT11_G <- PORT11[ which(PORT11$chainend == 'Goal_F'), ]
#Behind_F
PORT11_B <- PORT11[ which(PORT11$chainend == 'Behind_F'), ]
#Stoppage_F
PORT11_SF <- PORT11[ which(PORT11$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT11_TF <- PORT11[ which(PORT11$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT11_SAM <- PORT11[ which(PORT11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT11_TAM <- PORT11[ which(PORT11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT11_SDM <- PORT11[ which(PORT11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT11_TDM <- PORT11[ which(PORT11$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT11_SD <- PORT11[ which(PORT11$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT11_TD <- PORT11[ which(PORT11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT11_QT <- PORT11[ which(PORT11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
PORT12_G <- PORT12[ which(PORT12$chainend == 'Goal_F'), ]
#Behind_F
PORT12_B <- PORT12[ which(PORT12$chainend == 'Behind_F'), ]
#Stoppage_F
PORT12_SF <- PORT12[ which(PORT12$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT12_TF <- PORT12[ which(PORT12$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT12_SAM <- PORT12[ which(PORT12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT12_TAM <- PORT12[ which(PORT12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT12_SDM <- PORT12[ which(PORT12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT12_TDM <- PORT12[ which(PORT12$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT12_SD <- PORT12[ which(PORT12$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT12_TD <- PORT12[ which(PORT12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT12_QT <- PORT12[ which(PORT12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
PORT13_G <- PORT13[ which(PORT13$chainend == 'Goal_F'), ]
#Behind_F
PORT13_B <- PORT13[ which(PORT13$chainend == 'Behind_F'), ]
#Stoppage_F
PORT13_SF <- PORT13[ which(PORT13$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT13_TF <- PORT13[ which(PORT13$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT13_SAM <- PORT13[ which(PORT13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT13_TAM <- PORT13[ which(PORT13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT13_SDM <- PORT13[ which(PORT13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT13_TDM <- PORT13[ which(PORT13$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT13_SD <- PORT13[ which(PORT13$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT13_TD <- PORT13[ which(PORT13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT13_QT <- PORT13[ which(PORT13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
PORT14_G <- PORT14[ which(PORT14$chainend == 'Goal_F'), ]
#Behind_F
PORT14_B <- PORT14[ which(PORT14$chainend == 'Behind_F'), ]
#Stoppage_F
PORT14_SF <- PORT14[ which(PORT14$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT14_TF <- PORT14[ which(PORT14$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT14_SAM <- PORT14[ which(PORT14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT14_TAM <- PORT14[ which(PORT14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT14_SDM <- PORT14[ which(PORT14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT14_TDM <- PORT14[ which(PORT14$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT14_SD <- PORT14[ which(PORT14$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT14_TD <- PORT14[ which(PORT14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT14_QT <- PORT14[ which(PORT14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
PORT15_G <- PORT15[ which(PORT15$chainend == 'Goal_F'), ]
#Behind_F
PORT15_B <- PORT15[ which(PORT15$chainend == 'Behind_F'), ]
#Stoppage_F
PORT15_SF <- PORT15[ which(PORT15$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT15_TF <- PORT15[ which(PORT15$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT15_SAM <- PORT15[ which(PORT15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT15_TAM <- PORT15[ which(PORT15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT15_SDM <- PORT15[ which(PORT15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT15_TDM <- PORT15[ which(PORT15$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT15_SD <- PORT15[ which(PORT15$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT15_TD <- PORT15[ which(PORT15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT15_QT <- PORT15[ which(PORT15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
PORT16_G <- PORT16[ which(PORT16$chainend == 'Goal_F'), ]
#Behind_F
PORT16_B <- PORT16[ which(PORT16$chainend == 'Behind_F'), ]
#Stoppage_F
PORT16_SF <- PORT16[ which(PORT16$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT16_TF <- PORT16[ which(PORT16$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT16_SAM <- PORT16[ which(PORT16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT16_TAM <- PORT16[ which(PORT16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT16_SDM <- PORT16[ which(PORT16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT16_TDM <- PORT16[ which(PORT16$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT16_SD <- PORT16[ which(PORT16$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT16_TD <- PORT16[ which(PORT16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT16_QT <- PORT16[ which(PORT16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
PORT17_G <- PORT17[ which(PORT17$chainend == 'Goal_F'), ]
#Behind_F
PORT17_B <- PORT17[ which(PORT17$chainend == 'Behind_F'), ]
#Stoppage_F
PORT17_SF <- PORT17[ which(PORT17$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT17_TF <- PORT17[ which(PORT17$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT17_SAM <- PORT17[ which(PORT17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT17_TAM <- PORT17[ which(PORT17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT17_SDM <- PORT17[ which(PORT17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT17_TDM <- PORT17[ which(PORT17$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT17_SD <- PORT17[ which(PORT17$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT17_TD <- PORT17[ which(PORT17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT17_QT <- PORT17[ which(PORT17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
PORT18_G <- PORT18[ which(PORT18$chainend == 'Goal_F'), ]
#Behind_F
PORT18_B <- PORT18[ which(PORT18$chainend == 'Behind_F'), ]
#Stoppage_F
PORT18_SF <- PORT18[ which(PORT18$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT18_TF <- PORT18[ which(PORT18$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT18_SAM <- PORT18[ which(PORT18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT18_TAM <- PORT18[ which(PORT18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT18_SDM <- PORT18[ which(PORT18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT18_TDM <- PORT18[ which(PORT18$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT18_SD <- PORT18[ which(PORT18$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT18_TD <- PORT18[ which(PORT18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT18_QT <- PORT18[ which(PORT18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
PORT19_G <- PORT19[ which(PORT19$chainend == 'Goal_F'), ]
#Behind_F
PORT19_B <- PORT19[ which(PORT19$chainend == 'Behind_F'), ]
#Stoppage_F
PORT19_SF <- PORT19[ which(PORT19$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT19_TF <- PORT19[ which(PORT19$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT19_SAM <- PORT19[ which(PORT19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT19_TAM <- PORT19[ which(PORT19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT19_SDM <- PORT19[ which(PORT19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT19_TDM <- PORT19[ which(PORT19$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT19_SD <- PORT19[ which(PORT19$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT19_TD <- PORT19[ which(PORT19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT19_QT <- PORT19[ which(PORT19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
PORT20_G <- PORT20[ which(PORT20$chainend == 'Goal_F'), ]
#Behind_F
PORT20_B <- PORT20[ which(PORT20$chainend == 'Behind_F'), ]
#Stoppage_F
PORT20_SF <- PORT20[ which(PORT20$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT20_TF <- PORT20[ which(PORT20$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT20_SAM <- PORT20[ which(PORT20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT20_TAM <- PORT20[ which(PORT20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT20_SDM <- PORT20[ which(PORT20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT20_TDM <- PORT20[ which(PORT20$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT20_SD <- PORT20[ which(PORT20$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT20_TD <- PORT20[ which(PORT20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT20_QT <- PORT20[ which(PORT20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
PORT21_G <- PORT21[ which(PORT21$chainend == 'Goal_F'), ]
#Behind_F
PORT21_B <- PORT21[ which(PORT21$chainend == 'Behind_F'), ]
#Stoppage_F
PORT21_SF <- PORT21[ which(PORT21$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT21_TF <- PORT21[ which(PORT21$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT21_SAM <- PORT21[ which(PORT21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT21_TAM <- PORT21[ which(PORT21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT21_SDM <- PORT21[ which(PORT21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT21_TDM <- PORT21[ which(PORT21$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT21_SD <- PORT21[ which(PORT21$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT21_TD <- PORT21[ which(PORT21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT21_QT <- PORT21[ which(PORT21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
PORT22_G <- PORT22[ which(PORT22$chainend == 'Goal_F'), ]
#Behind_F
PORT22_B <- PORT22[ which(PORT22$chainend == 'Behind_F'), ]
#Stoppage_F
PORT22_SF <- PORT22[ which(PORT22$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT22_TF <- PORT22[ which(PORT22$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT22_SAM <- PORT22[ which(PORT22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT22_TAM <- PORT22[ which(PORT22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT22_SDM <- PORT22[ which(PORT22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT22_TDM <- PORT22[ which(PORT22$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT22_SD <- PORT22[ which(PORT22$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT22_TD <- PORT22[ which(PORT22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT22_QT <- PORT22[ which(PORT22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
PORT23_G <- PORT23[ which(PORT23$chainend == 'Goal_F'), ]
#Behind_F
PORT23_B <- PORT23[ which(PORT23$chainend == 'Behind_F'), ]
#Stoppage_F
PORT23_SF <- PORT23[ which(PORT23$chainend == 'Stoppage_F'), ]
#Turnover_F
PORT23_TF <- PORT23[ which(PORT23$chainend == 'Turnover_F'), ]
#Stoppage_AM
PORT23_SAM <- PORT23[ which(PORT23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
PORT23_TAM <- PORT23[ which(PORT23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
PORT23_SDM <- PORT23[ which(PORT23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
PORT23_TDM <- PORT23[ which(PORT23$chainend == 'Turnover_DM'), ]
#Stoppage_D
PORT23_SD <- PORT23[ which(PORT23$chainend == 'Stoppage_D'), ]
#Turnover_D
PORT23_TD <- PORT23[ which(PORT23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
PORT23_QT <- PORT23[ which(PORT23$chainend == 'End of Qtr_DM'), ]

#Richmond

#Split by rounds
RICH01 <- RICH[ which(RICH$round == '01'), ]
RICH02 <- RICH[ which(RICH$round == '02'), ]
RICH03 <- RICH[ which(RICH$round == '03'), ]
RICH04 <- RICH[ which(RICH$round == '04'), ]
RICH05 <- RICH[ which(RICH$round == '05'), ]
RICH06 <- RICH[ which(RICH$round == '06'), ]
RICH07 <- RICH[ which(RICH$round == '07'), ]
RICH08 <- RICH[ which(RICH$round == '08'), ]
RICH09 <- RICH[ which(RICH$round == '09'), ]
RICH10 <- RICH[ which(RICH$round == '10'), ]
RICH11 <- RICH[ which(RICH$round == '11'), ]
RICH12 <- RICH[ which(RICH$round == '12'), ]
RICH13 <- RICH[ which(RICH$round == '13'), ]
RICH14 <- RICH[ which(RICH$round == '14'), ]
RICH15 <- RICH[ which(RICH$round == '15'), ]
RICH16 <- RICH[ which(RICH$round == '16'), ]
RICH17 <- RICH[ which(RICH$round == '17'), ]
RICH18 <- RICH[ which(RICH$round == '18'), ]
RICH19 <- RICH[ which(RICH$round == '19'), ]
RICH20 <- RICH[ which(RICH$round == '20'), ]
RICH21 <- RICH[ which(RICH$round == '21'), ]
RICH22 <- RICH[ which(RICH$round == '22'), ]
RICH23 <- RICH[ which(RICH$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
RICH01_G <- RICH01[ which(RICH01$chainend == 'Goal_F'), ]
#Behind_F
RICH01_B <- RICH01[ which(RICH01$chainend == 'Behind_F'), ]
#Stoppage_F
RICH01_SF <- RICH01[ which(RICH01$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH01_TF <- RICH01[ which(RICH01$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH01_SAM <- RICH01[ which(RICH01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH01_TAM <- RICH01[ which(RICH01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH01_SDM <- RICH01[ which(RICH01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH01_TDM <- RICH01[ which(RICH01$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH01_SD <- RICH01[ which(RICH01$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH01_TD <- RICH01[ which(RICH01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH01_QT <- RICH01[ which(RICH01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
RICH02_G <- RICH02[ which(RICH02$chainend == 'Goal_F'), ]
#Behind_F
RICH02_B <- RICH02[ which(RICH02$chainend == 'Behind_F'), ]
#Stoppage_F
RICH02_SF <- RICH02[ which(RICH02$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH02_TF <- RICH02[ which(RICH02$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH02_SAM <- RICH02[ which(RICH02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH02_TAM <- RICH02[ which(RICH02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH02_SDM <- RICH02[ which(RICH02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH02_TDM <- RICH02[ which(RICH02$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH02_SD <- RICH02[ which(RICH02$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH02_TD <- RICH02[ which(RICH02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH02_QT <- RICH02[ which(RICH02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
RICH03_G <- RICH03[ which(RICH03$chainend == 'Goal_F'), ]
#Behind_F
RICH03_B <- RICH03[ which(RICH03$chainend == 'Behind_F'), ]
#Stoppage_F
RICH03_SF <- RICH03[ which(RICH03$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH03_TF <- RICH03[ which(RICH03$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH03_SAM <- RICH03[ which(RICH03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH03_TAM <- RICH03[ which(RICH03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH03_SDM <- RICH03[ which(RICH03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH03_TDM <- RICH03[ which(RICH03$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH03_SD <- RICH03[ which(RICH03$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH03_TD <- RICH03[ which(RICH03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH03_QT <- RICH03[ which(RICH03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
RICH04_G <- RICH04[ which(RICH04$chainend == 'Goal_F'), ]
#Behind_F
RICH04_B <- RICH04[ which(RICH04$chainend == 'Behind_F'), ]
#Stoppage_F
RICH04_SF <- RICH04[ which(RICH04$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH04_TF <- RICH04[ which(RICH04$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH04_SAM <- RICH04[ which(RICH04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH04_TAM <- RICH04[ which(RICH04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH04_SDM <- RICH04[ which(RICH04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH04_TDM <- RICH04[ which(RICH04$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH04_SD <- RICH04[ which(RICH04$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH04_TD <- RICH04[ which(RICH04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH04_QT <- RICH04[ which(RICH04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
RICH05_G <- RICH05[ which(RICH05$chainend == 'Goal_F'), ]
#Behind_F
RICH05_B <- RICH05[ which(RICH05$chainend == 'Behind_F'), ]
#Stoppage_F
RICH05_SF <- RICH05[ which(RICH05$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH05_TF <- RICH05[ which(RICH05$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH05_SAM <- RICH05[ which(RICH05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH05_TAM <- RICH05[ which(RICH05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH05_SDM <- RICH05[ which(RICH05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH05_TDM <- RICH05[ which(RICH05$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH05_SD <- RICH05[ which(RICH05$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH05_TD <- RICH05[ which(RICH05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH05_QT <- RICH05[ which(RICH05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
RICH06_G <- RICH06[ which(RICH06$chainend == 'Goal_F'), ]
#Behind_F
RICH06_B <- RICH06[ which(RICH06$chainend == 'Behind_F'), ]
#Stoppage_F
RICH06_SF <- RICH06[ which(RICH06$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH06_TF <- RICH06[ which(RICH06$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH06_SAM <- RICH06[ which(RICH06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH06_TAM <- RICH06[ which(RICH06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH06_SDM <- RICH06[ which(RICH06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH06_TDM <- RICH06[ which(RICH06$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH06_SD <- RICH06[ which(RICH06$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH06_TD <- RICH06[ which(RICH06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH06_QT <- RICH06[ which(RICH06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
RICH07_G <- RICH07[ which(RICH07$chainend == 'Goal_F'), ]
#Behind_F
RICH07_B <- RICH07[ which(RICH07$chainend == 'Behind_F'), ]
#Stoppage_F
RICH07_SF <- RICH07[ which(RICH07$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH07_TF <- RICH07[ which(RICH07$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH07_SAM <- RICH07[ which(RICH07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH07_TAM <- RICH07[ which(RICH07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH07_SDM <- RICH07[ which(RICH07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH07_TDM <- RICH07[ which(RICH07$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH07_SD <- RICH07[ which(RICH07$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH07_TD <- RICH07[ which(RICH07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH07_QT <- RICH07[ which(RICH07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
RICH08_G <- RICH08[ which(RICH08$chainend == 'Goal_F'), ]
#Behind_F
RICH08_B <- RICH08[ which(RICH08$chainend == 'Behind_F'), ]
#Stoppage_F
RICH08_SF <- RICH08[ which(RICH08$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH08_TF <- RICH08[ which(RICH08$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH08_SAM <- RICH08[ which(RICH08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH08_TAM <- RICH08[ which(RICH08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH08_SDM <- RICH08[ which(RICH08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH08_TDM <- RICH08[ which(RICH08$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH08_SD <- RICH08[ which(RICH08$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH08_TD <- RICH08[ which(RICH08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH08_QT <- RICH08[ which(RICH08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
RICH09_G <- RICH09[ which(RICH09$chainend == 'Goal_F'), ]
#Behind_F
RICH09_B <- RICH09[ which(RICH09$chainend == 'Behind_F'), ]
#Stoppage_F
RICH09_SF <- RICH09[ which(RICH09$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH09_TF <- RICH09[ which(RICH09$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH09_SAM <- RICH09[ which(RICH09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH09_TAM <- RICH09[ which(RICH09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH09_SDM <- RICH09[ which(RICH09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH09_TDM <- RICH09[ which(RICH09$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH09_SD <- RICH09[ which(RICH09$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH09_TD <- RICH09[ which(RICH09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH09_QT <- RICH09[ which(RICH09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
RICH10_G <- RICH10[ which(RICH10$chainend == 'Goal_F'), ]
#Behind_F
RICH10_B <- RICH10[ which(RICH10$chainend == 'Behind_F'), ]
#Stoppage_F
RICH10_SF <- RICH10[ which(RICH10$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH10_TF <- RICH10[ which(RICH10$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH10_SAM <- RICH10[ which(RICH10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH10_TAM <- RICH10[ which(RICH10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH10_SDM <- RICH10[ which(RICH10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH10_TDM <- RICH10[ which(RICH10$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH10_SD <- RICH10[ which(RICH10$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH10_TD <- RICH10[ which(RICH10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH10_QT <- RICH10[ which(RICH10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
RICH11_G <- RICH11[ which(RICH11$chainend == 'Goal_F'), ]
#Behind_F
RICH11_B <- RICH11[ which(RICH11$chainend == 'Behind_F'), ]
#Stoppage_F
RICH11_SF <- RICH11[ which(RICH11$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH11_TF <- RICH11[ which(RICH11$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH11_SAM <- RICH11[ which(RICH11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH11_TAM <- RICH11[ which(RICH11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH11_SDM <- RICH11[ which(RICH11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH11_TDM <- RICH11[ which(RICH11$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH11_SD <- RICH11[ which(RICH11$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH11_TD <- RICH11[ which(RICH11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH11_QT <- RICH11[ which(RICH11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
RICH12_G <- RICH12[ which(RICH12$chainend == 'Goal_F'), ]
#Behind_F
RICH12_B <- RICH12[ which(RICH12$chainend == 'Behind_F'), ]
#Stoppage_F
RICH12_SF <- RICH12[ which(RICH12$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH12_TF <- RICH12[ which(RICH12$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH12_SAM <- RICH12[ which(RICH12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH12_TAM <- RICH12[ which(RICH12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH12_SDM <- RICH12[ which(RICH12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH12_TDM <- RICH12[ which(RICH12$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH12_SD <- RICH12[ which(RICH12$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH12_TD <- RICH12[ which(RICH12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH12_QT <- RICH12[ which(RICH12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
RICH13_G <- RICH13[ which(RICH13$chainend == 'Goal_F'), ]
#Behind_F
RICH13_B <- RICH13[ which(RICH13$chainend == 'Behind_F'), ]
#Stoppage_F
RICH13_SF <- RICH13[ which(RICH13$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH13_TF <- RICH13[ which(RICH13$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH13_SAM <- RICH13[ which(RICH13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH13_TAM <- RICH13[ which(RICH13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH13_SDM <- RICH13[ which(RICH13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH13_TDM <- RICH13[ which(RICH13$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH13_SD <- RICH13[ which(RICH13$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH13_TD <- RICH13[ which(RICH13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH13_QT <- RICH13[ which(RICH13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
RICH14_G <- RICH14[ which(RICH14$chainend == 'Goal_F'), ]
#Behind_F
RICH14_B <- RICH14[ which(RICH14$chainend == 'Behind_F'), ]
#Stoppage_F
RICH14_SF <- RICH14[ which(RICH14$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH14_TF <- RICH14[ which(RICH14$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH14_SAM <- RICH14[ which(RICH14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH14_TAM <- RICH14[ which(RICH14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH14_SDM <- RICH14[ which(RICH14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH14_TDM <- RICH14[ which(RICH14$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH14_SD <- RICH14[ which(RICH14$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH14_TD <- RICH14[ which(RICH14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH14_QT <- RICH14[ which(RICH14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
RICH15_G <- RICH15[ which(RICH15$chainend == 'Goal_F'), ]
#Behind_F
RICH15_B <- RICH15[ which(RICH15$chainend == 'Behind_F'), ]
#Stoppage_F
RICH15_SF <- RICH15[ which(RICH15$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH15_TF <- RICH15[ which(RICH15$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH15_SAM <- RICH15[ which(RICH15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH15_TAM <- RICH15[ which(RICH15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH15_SDM <- RICH15[ which(RICH15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH15_TDM <- RICH15[ which(RICH15$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH15_SD <- RICH15[ which(RICH15$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH15_TD <- RICH15[ which(RICH15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH15_QT <- RICH15[ which(RICH15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
RICH16_G <- RICH16[ which(RICH16$chainend == 'Goal_F'), ]
#Behind_F
RICH16_B <- RICH16[ which(RICH16$chainend == 'Behind_F'), ]
#Stoppage_F
RICH16_SF <- RICH16[ which(RICH16$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH16_TF <- RICH16[ which(RICH16$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH16_SAM <- RICH16[ which(RICH16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH16_TAM <- RICH16[ which(RICH16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH16_SDM <- RICH16[ which(RICH16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH16_TDM <- RICH16[ which(RICH16$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH16_SD <- RICH16[ which(RICH16$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH16_TD <- RICH16[ which(RICH16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH16_QT <- RICH16[ which(RICH16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
RICH17_G <- RICH17[ which(RICH17$chainend == 'Goal_F'), ]
#Behind_F
RICH17_B <- RICH17[ which(RICH17$chainend == 'Behind_F'), ]
#Stoppage_F
RICH17_SF <- RICH17[ which(RICH17$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH17_TF <- RICH17[ which(RICH17$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH17_SAM <- RICH17[ which(RICH17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH17_TAM <- RICH17[ which(RICH17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH17_SDM <- RICH17[ which(RICH17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH17_TDM <- RICH17[ which(RICH17$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH17_SD <- RICH17[ which(RICH17$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH17_TD <- RICH17[ which(RICH17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH17_QT <- RICH17[ which(RICH17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
RICH18_G <- RICH18[ which(RICH18$chainend == 'Goal_F'), ]
#Behind_F
RICH18_B <- RICH18[ which(RICH18$chainend == 'Behind_F'), ]
#Stoppage_F
RICH18_SF <- RICH18[ which(RICH18$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH18_TF <- RICH18[ which(RICH18$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH18_SAM <- RICH18[ which(RICH18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH18_TAM <- RICH18[ which(RICH18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH18_SDM <- RICH18[ which(RICH18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH18_TDM <- RICH18[ which(RICH18$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH18_SD <- RICH18[ which(RICH18$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH18_TD <- RICH18[ which(RICH18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH18_QT <- RICH18[ which(RICH18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
RICH19_G <- RICH19[ which(RICH19$chainend == 'Goal_F'), ]
#Behind_F
RICH19_B <- RICH19[ which(RICH19$chainend == 'Behind_F'), ]
#Stoppage_F
RICH19_SF <- RICH19[ which(RICH19$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH19_TF <- RICH19[ which(RICH19$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH19_SAM <- RICH19[ which(RICH19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH19_TAM <- RICH19[ which(RICH19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH19_SDM <- RICH19[ which(RICH19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH19_TDM <- RICH19[ which(RICH19$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH19_SD <- RICH19[ which(RICH19$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH19_TD <- RICH19[ which(RICH19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH19_QT <- RICH19[ which(RICH19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
RICH20_G <- RICH20[ which(RICH20$chainend == 'Goal_F'), ]
#Behind_F
RICH20_B <- RICH20[ which(RICH20$chainend == 'Behind_F'), ]
#Stoppage_F
RICH20_SF <- RICH20[ which(RICH20$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH20_TF <- RICH20[ which(RICH20$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH20_SAM <- RICH20[ which(RICH20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH20_TAM <- RICH20[ which(RICH20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH20_SDM <- RICH20[ which(RICH20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH20_TDM <- RICH20[ which(RICH20$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH20_SD <- RICH20[ which(RICH20$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH20_TD <- RICH20[ which(RICH20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH20_QT <- RICH20[ which(RICH20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
RICH21_G <- RICH21[ which(RICH21$chainend == 'Goal_F'), ]
#Behind_F
RICH21_B <- RICH21[ which(RICH21$chainend == 'Behind_F'), ]
#Stoppage_F
RICH21_SF <- RICH21[ which(RICH21$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH21_TF <- RICH21[ which(RICH21$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH21_SAM <- RICH21[ which(RICH21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH21_TAM <- RICH21[ which(RICH21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH21_SDM <- RICH21[ which(RICH21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH21_TDM <- RICH21[ which(RICH21$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH21_SD <- RICH21[ which(RICH21$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH21_TD <- RICH21[ which(RICH21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH21_QT <- RICH21[ which(RICH21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
RICH22_G <- RICH22[ which(RICH22$chainend == 'Goal_F'), ]
#Behind_F
RICH22_B <- RICH22[ which(RICH22$chainend == 'Behind_F'), ]
#Stoppage_F
RICH22_SF <- RICH22[ which(RICH22$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH22_TF <- RICH22[ which(RICH22$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH22_SAM <- RICH22[ which(RICH22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH22_TAM <- RICH22[ which(RICH22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH22_SDM <- RICH22[ which(RICH22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH22_TDM <- RICH22[ which(RICH22$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH22_SD <- RICH22[ which(RICH22$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH22_TD <- RICH22[ which(RICH22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH22_QT <- RICH22[ which(RICH22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
RICH23_G <- RICH23[ which(RICH23$chainend == 'Goal_F'), ]
#Behind_F
RICH23_B <- RICH23[ which(RICH23$chainend == 'Behind_F'), ]
#Stoppage_F
RICH23_SF <- RICH23[ which(RICH23$chainend == 'Stoppage_F'), ]
#Turnover_F
RICH23_TF <- RICH23[ which(RICH23$chainend == 'Turnover_F'), ]
#Stoppage_AM
RICH23_SAM <- RICH23[ which(RICH23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
RICH23_TAM <- RICH23[ which(RICH23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
RICH23_SDM <- RICH23[ which(RICH23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
RICH23_TDM <- RICH23[ which(RICH23$chainend == 'Turnover_DM'), ]
#Stoppage_D
RICH23_SD <- RICH23[ which(RICH23$chainend == 'Stoppage_D'), ]
#Turnover_D
RICH23_TD <- RICH23[ which(RICH23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
RICH23_QT <- RICH23[ which(RICH23$chainend == 'End of Qtr_DM'), ]

#St Kilda

#Split by rounds
STK01 <- STK[ which(STK$round == '01'), ]
STK02 <- STK[ which(STK$round == '02'), ]
STK03 <- STK[ which(STK$round == '03'), ]
STK04 <- STK[ which(STK$round == '04'), ]
STK05 <- STK[ which(STK$round == '05'), ]
STK06 <- STK[ which(STK$round == '06'), ]
STK07 <- STK[ which(STK$round == '07'), ]
STK08 <- STK[ which(STK$round == '08'), ]
STK09 <- STK[ which(STK$round == '09'), ]
STK10 <- STK[ which(STK$round == '10'), ]
STK11 <- STK[ which(STK$round == '11'), ]
STK12 <- STK[ which(STK$round == '12'), ]
STK13 <- STK[ which(STK$round == '13'), ]
STK14 <- STK[ which(STK$round == '14'), ]
STK15 <- STK[ which(STK$round == '15'), ]
STK16 <- STK[ which(STK$round == '16'), ]
STK17 <- STK[ which(STK$round == '17'), ]
STK18 <- STK[ which(STK$round == '18'), ]
STK19 <- STK[ which(STK$round == '19'), ]
STK20 <- STK[ which(STK$round == '20'), ]
STK21 <- STK[ which(STK$round == '21'), ]
STK22 <- STK[ which(STK$round == '22'), ]
STK23 <- STK[ which(STK$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
STK01_G <- STK01[ which(STK01$chainend == 'Goal_F'), ]
#Behind_F
STK01_B <- STK01[ which(STK01$chainend == 'Behind_F'), ]
#Stoppage_F
STK01_SF <- STK01[ which(STK01$chainend == 'Stoppage_F'), ]
#Turnover_F
STK01_TF <- STK01[ which(STK01$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK01_SAM <- STK01[ which(STK01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK01_TAM <- STK01[ which(STK01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK01_SDM <- STK01[ which(STK01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK01_TDM <- STK01[ which(STK01$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK01_SD <- STK01[ which(STK01$chainend == 'Stoppage_D'), ]
#Turnover_D
STK01_TD <- STK01[ which(STK01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK01_QT <- STK01[ which(STK01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
STK02_G <- STK02[ which(STK02$chainend == 'Goal_F'), ]
#Behind_F
STK02_B <- STK02[ which(STK02$chainend == 'Behind_F'), ]
#Stoppage_F
STK02_SF <- STK02[ which(STK02$chainend == 'Stoppage_F'), ]
#Turnover_F
STK02_TF <- STK02[ which(STK02$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK02_SAM <- STK02[ which(STK02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK02_TAM <- STK02[ which(STK02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK02_SDM <- STK02[ which(STK02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK02_TDM <- STK02[ which(STK02$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK02_SD <- STK02[ which(STK02$chainend == 'Stoppage_D'), ]
#Turnover_D
STK02_TD <- STK02[ which(STK02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK02_QT <- STK02[ which(STK02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
STK03_G <- STK03[ which(STK03$chainend == 'Goal_F'), ]
#Behind_F
STK03_B <- STK03[ which(STK03$chainend == 'Behind_F'), ]
#Stoppage_F
STK03_SF <- STK03[ which(STK03$chainend == 'Stoppage_F'), ]
#Turnover_F
STK03_TF <- STK03[ which(STK03$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK03_SAM <- STK03[ which(STK03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK03_TAM <- STK03[ which(STK03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK03_SDM <- STK03[ which(STK03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK03_TDM <- STK03[ which(STK03$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK03_SD <- STK03[ which(STK03$chainend == 'Stoppage_D'), ]
#Turnover_D
STK03_TD <- STK03[ which(STK03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK03_QT <- STK03[ which(STK03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
STK04_G <- STK04[ which(STK04$chainend == 'Goal_F'), ]
#Behind_F
STK04_B <- STK04[ which(STK04$chainend == 'Behind_F'), ]
#Stoppage_F
STK04_SF <- STK04[ which(STK04$chainend == 'Stoppage_F'), ]
#Turnover_F
STK04_TF <- STK04[ which(STK04$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK04_SAM <- STK04[ which(STK04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK04_TAM <- STK04[ which(STK04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK04_SDM <- STK04[ which(STK04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK04_TDM <- STK04[ which(STK04$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK04_SD <- STK04[ which(STK04$chainend == 'Stoppage_D'), ]
#Turnover_D
STK04_TD <- STK04[ which(STK04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK04_QT <- STK04[ which(STK04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
STK05_G <- STK05[ which(STK05$chainend == 'Goal_F'), ]
#Behind_F
STK05_B <- STK05[ which(STK05$chainend == 'Behind_F'), ]
#Stoppage_F
STK05_SF <- STK05[ which(STK05$chainend == 'Stoppage_F'), ]
#Turnover_F
STK05_TF <- STK05[ which(STK05$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK05_SAM <- STK05[ which(STK05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK05_TAM <- STK05[ which(STK05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK05_SDM <- STK05[ which(STK05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK05_TDM <- STK05[ which(STK05$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK05_SD <- STK05[ which(STK05$chainend == 'Stoppage_D'), ]
#Turnover_D
STK05_TD <- STK05[ which(STK05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK05_QT <- STK05[ which(STK05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
STK06_G <- STK06[ which(STK06$chainend == 'Goal_F'), ]
#Behind_F
STK06_B <- STK06[ which(STK06$chainend == 'Behind_F'), ]
#Stoppage_F
STK06_SF <- STK06[ which(STK06$chainend == 'Stoppage_F'), ]
#Turnover_F
STK06_TF <- STK06[ which(STK06$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK06_SAM <- STK06[ which(STK06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK06_TAM <- STK06[ which(STK06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK06_SDM <- STK06[ which(STK06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK06_TDM <- STK06[ which(STK06$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK06_SD <- STK06[ which(STK06$chainend == 'Stoppage_D'), ]
#Turnover_D
STK06_TD <- STK06[ which(STK06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK06_QT <- STK06[ which(STK06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
STK07_G <- STK07[ which(STK07$chainend == 'Goal_F'), ]
#Behind_F
STK07_B <- STK07[ which(STK07$chainend == 'Behind_F'), ]
#Stoppage_F
STK07_SF <- STK07[ which(STK07$chainend == 'Stoppage_F'), ]
#Turnover_F
STK07_TF <- STK07[ which(STK07$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK07_SAM <- STK07[ which(STK07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK07_TAM <- STK07[ which(STK07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK07_SDM <- STK07[ which(STK07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK07_TDM <- STK07[ which(STK07$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK07_SD <- STK07[ which(STK07$chainend == 'Stoppage_D'), ]
#Turnover_D
STK07_TD <- STK07[ which(STK07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK07_QT <- STK07[ which(STK07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
STK08_G <- STK08[ which(STK08$chainend == 'Goal_F'), ]
#Behind_F
STK08_B <- STK08[ which(STK08$chainend == 'Behind_F'), ]
#Stoppage_F
STK08_SF <- STK08[ which(STK08$chainend == 'Stoppage_F'), ]
#Turnover_F
STK08_TF <- STK08[ which(STK08$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK08_SAM <- STK08[ which(STK08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK08_TAM <- STK08[ which(STK08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK08_SDM <- STK08[ which(STK08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK08_TDM <- STK08[ which(STK08$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK08_SD <- STK08[ which(STK08$chainend == 'Stoppage_D'), ]
#Turnover_D
STK08_TD <- STK08[ which(STK08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK08_QT <- STK08[ which(STK08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
STK09_G <- STK09[ which(STK09$chainend == 'Goal_F'), ]
#Behind_F
STK09_B <- STK09[ which(STK09$chainend == 'Behind_F'), ]
#Stoppage_F
STK09_SF <- STK09[ which(STK09$chainend == 'Stoppage_F'), ]
#Turnover_F
STK09_TF <- STK09[ which(STK09$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK09_SAM <- STK09[ which(STK09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK09_TAM <- STK09[ which(STK09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK09_SDM <- STK09[ which(STK09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK09_TDM <- STK09[ which(STK09$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK09_SD <- STK09[ which(STK09$chainend == 'Stoppage_D'), ]
#Turnover_D
STK09_TD <- STK09[ which(STK09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK09_QT <- STK09[ which(STK09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
STK10_G <- STK10[ which(STK10$chainend == 'Goal_F'), ]
#Behind_F
STK10_B <- STK10[ which(STK10$chainend == 'Behind_F'), ]
#Stoppage_F
STK10_SF <- STK10[ which(STK10$chainend == 'Stoppage_F'), ]
#Turnover_F
STK10_TF <- STK10[ which(STK10$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK10_SAM <- STK10[ which(STK10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK10_TAM <- STK10[ which(STK10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK10_SDM <- STK10[ which(STK10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK10_TDM <- STK10[ which(STK10$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK10_SD <- STK10[ which(STK10$chainend == 'Stoppage_D'), ]
#Turnover_D
STK10_TD <- STK10[ which(STK10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK10_QT <- STK10[ which(STK10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
STK11_G <- STK11[ which(STK11$chainend == 'Goal_F'), ]
#Behind_F
STK11_B <- STK11[ which(STK11$chainend == 'Behind_F'), ]
#Stoppage_F
STK11_SF <- STK11[ which(STK11$chainend == 'Stoppage_F'), ]
#Turnover_F
STK11_TF <- STK11[ which(STK11$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK11_SAM <- STK11[ which(STK11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK11_TAM <- STK11[ which(STK11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK11_SDM <- STK11[ which(STK11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK11_TDM <- STK11[ which(STK11$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK11_SD <- STK11[ which(STK11$chainend == 'Stoppage_D'), ]
#Turnover_D
STK11_TD <- STK11[ which(STK11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK11_QT <- STK11[ which(STK11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
STK12_G <- STK12[ which(STK12$chainend == 'Goal_F'), ]
#Behind_F
STK12_B <- STK12[ which(STK12$chainend == 'Behind_F'), ]
#Stoppage_F
STK12_SF <- STK12[ which(STK12$chainend == 'Stoppage_F'), ]
#Turnover_F
STK12_TF <- STK12[ which(STK12$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK12_SAM <- STK12[ which(STK12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK12_TAM <- STK12[ which(STK12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK12_SDM <- STK12[ which(STK12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK12_TDM <- STK12[ which(STK12$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK12_SD <- STK12[ which(STK12$chainend == 'Stoppage_D'), ]
#Turnover_D
STK12_TD <- STK12[ which(STK12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK12_QT <- STK12[ which(STK12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
STK13_G <- STK13[ which(STK13$chainend == 'Goal_F'), ]
#Behind_F
STK13_B <- STK13[ which(STK13$chainend == 'Behind_F'), ]
#Stoppage_F
STK13_SF <- STK13[ which(STK13$chainend == 'Stoppage_F'), ]
#Turnover_F
STK13_TF <- STK13[ which(STK13$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK13_SAM <- STK13[ which(STK13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK13_TAM <- STK13[ which(STK13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK13_SDM <- STK13[ which(STK13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK13_TDM <- STK13[ which(STK13$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK13_SD <- STK13[ which(STK13$chainend == 'Stoppage_D'), ]
#Turnover_D
STK13_TD <- STK13[ which(STK13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK13_QT <- STK13[ which(STK13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
STK14_G <- STK14[ which(STK14$chainend == 'Goal_F'), ]
#Behind_F
STK14_B <- STK14[ which(STK14$chainend == 'Behind_F'), ]
#Stoppage_F
STK14_SF <- STK14[ which(STK14$chainend == 'Stoppage_F'), ]
#Turnover_F
STK14_TF <- STK14[ which(STK14$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK14_SAM <- STK14[ which(STK14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK14_TAM <- STK14[ which(STK14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK14_SDM <- STK14[ which(STK14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK14_TDM <- STK14[ which(STK14$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK14_SD <- STK14[ which(STK14$chainend == 'Stoppage_D'), ]
#Turnover_D
STK14_TD <- STK14[ which(STK14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK14_QT <- STK14[ which(STK14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
STK15_G <- STK15[ which(STK15$chainend == 'Goal_F'), ]
#Behind_F
STK15_B <- STK15[ which(STK15$chainend == 'Behind_F'), ]
#Stoppage_F
STK15_SF <- STK15[ which(STK15$chainend == 'Stoppage_F'), ]
#Turnover_F
STK15_TF <- STK15[ which(STK15$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK15_SAM <- STK15[ which(STK15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK15_TAM <- STK15[ which(STK15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK15_SDM <- STK15[ which(STK15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK15_TDM <- STK15[ which(STK15$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK15_SD <- STK15[ which(STK15$chainend == 'Stoppage_D'), ]
#Turnover_D
STK15_TD <- STK15[ which(STK15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK15_QT <- STK15[ which(STK15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
STK16_G <- STK16[ which(STK16$chainend == 'Goal_F'), ]
#Behind_F
STK16_B <- STK16[ which(STK16$chainend == 'Behind_F'), ]
#Stoppage_F
STK16_SF <- STK16[ which(STK16$chainend == 'Stoppage_F'), ]
#Turnover_F
STK16_TF <- STK16[ which(STK16$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK16_SAM <- STK16[ which(STK16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK16_TAM <- STK16[ which(STK16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK16_SDM <- STK16[ which(STK16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK16_TDM <- STK16[ which(STK16$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK16_SD <- STK16[ which(STK16$chainend == 'Stoppage_D'), ]
#Turnover_D
STK16_TD <- STK16[ which(STK16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK16_QT <- STK16[ which(STK16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
STK17_G <- STK17[ which(STK17$chainend == 'Goal_F'), ]
#Behind_F
STK17_B <- STK17[ which(STK17$chainend == 'Behind_F'), ]
#Stoppage_F
STK17_SF <- STK17[ which(STK17$chainend == 'Stoppage_F'), ]
#Turnover_F
STK17_TF <- STK17[ which(STK17$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK17_SAM <- STK17[ which(STK17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK17_TAM <- STK17[ which(STK17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK17_SDM <- STK17[ which(STK17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK17_TDM <- STK17[ which(STK17$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK17_SD <- STK17[ which(STK17$chainend == 'Stoppage_D'), ]
#Turnover_D
STK17_TD <- STK17[ which(STK17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK17_QT <- STK17[ which(STK17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
STK18_G <- STK18[ which(STK18$chainend == 'Goal_F'), ]
#Behind_F
STK18_B <- STK18[ which(STK18$chainend == 'Behind_F'), ]
#Stoppage_F
STK18_SF <- STK18[ which(STK18$chainend == 'Stoppage_F'), ]
#Turnover_F
STK18_TF <- STK18[ which(STK18$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK18_SAM <- STK18[ which(STK18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK18_TAM <- STK18[ which(STK18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK18_SDM <- STK18[ which(STK18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK18_TDM <- STK18[ which(STK18$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK18_SD <- STK18[ which(STK18$chainend == 'Stoppage_D'), ]
#Turnover_D
STK18_TD <- STK18[ which(STK18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK18_QT <- STK18[ which(STK18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
STK19_G <- STK19[ which(STK19$chainend == 'Goal_F'), ]
#Behind_F
STK19_B <- STK19[ which(STK19$chainend == 'Behind_F'), ]
#Stoppage_F
STK19_SF <- STK19[ which(STK19$chainend == 'Stoppage_F'), ]
#Turnover_F
STK19_TF <- STK19[ which(STK19$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK19_SAM <- STK19[ which(STK19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK19_TAM <- STK19[ which(STK19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK19_SDM <- STK19[ which(STK19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK19_TDM <- STK19[ which(STK19$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK19_SD <- STK19[ which(STK19$chainend == 'Stoppage_D'), ]
#Turnover_D
STK19_TD <- STK19[ which(STK19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK19_QT <- STK19[ which(STK19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
STK20_G <- STK20[ which(STK20$chainend == 'Goal_F'), ]
#Behind_F
STK20_B <- STK20[ which(STK20$chainend == 'Behind_F'), ]
#Stoppage_F
STK20_SF <- STK20[ which(STK20$chainend == 'Stoppage_F'), ]
#Turnover_F
STK20_TF <- STK20[ which(STK20$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK20_SAM <- STK20[ which(STK20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK20_TAM <- STK20[ which(STK20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK20_SDM <- STK20[ which(STK20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK20_TDM <- STK20[ which(STK20$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK20_SD <- STK20[ which(STK20$chainend == 'Stoppage_D'), ]
#Turnover_D
STK20_TD <- STK20[ which(STK20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK20_QT <- STK20[ which(STK20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
STK21_G <- STK21[ which(STK21$chainend == 'Goal_F'), ]
#Behind_F
STK21_B <- STK21[ which(STK21$chainend == 'Behind_F'), ]
#Stoppage_F
STK21_SF <- STK21[ which(STK21$chainend == 'Stoppage_F'), ]
#Turnover_F
STK21_TF <- STK21[ which(STK21$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK21_SAM <- STK21[ which(STK21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK21_TAM <- STK21[ which(STK21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK21_SDM <- STK21[ which(STK21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK21_TDM <- STK21[ which(STK21$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK21_SD <- STK21[ which(STK21$chainend == 'Stoppage_D'), ]
#Turnover_D
STK21_TD <- STK21[ which(STK21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK21_QT <- STK21[ which(STK21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
STK22_G <- STK22[ which(STK22$chainend == 'Goal_F'), ]
#Behind_F
STK22_B <- STK22[ which(STK22$chainend == 'Behind_F'), ]
#Stoppage_F
STK22_SF <- STK22[ which(STK22$chainend == 'Stoppage_F'), ]
#Turnover_F
STK22_TF <- STK22[ which(STK22$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK22_SAM <- STK22[ which(STK22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK22_TAM <- STK22[ which(STK22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK22_SDM <- STK22[ which(STK22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK22_TDM <- STK22[ which(STK22$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK22_SD <- STK22[ which(STK22$chainend == 'Stoppage_D'), ]
#Turnover_D
STK22_TD <- STK22[ which(STK22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK22_QT <- STK22[ which(STK22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
STK23_G <- STK23[ which(STK23$chainend == 'Goal_F'), ]
#Behind_F
STK23_B <- STK23[ which(STK23$chainend == 'Behind_F'), ]
#Stoppage_F
STK23_SF <- STK23[ which(STK23$chainend == 'Stoppage_F'), ]
#Turnover_F
STK23_TF <- STK23[ which(STK23$chainend == 'Turnover_F'), ]
#Stoppage_AM
STK23_SAM <- STK23[ which(STK23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
STK23_TAM <- STK23[ which(STK23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
STK23_SDM <- STK23[ which(STK23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
STK23_TDM <- STK23[ which(STK23$chainend == 'Turnover_DM'), ]
#Stoppage_D
STK23_SD <- STK23[ which(STK23$chainend == 'Stoppage_D'), ]
#Turnover_D
STK23_TD <- STK23[ which(STK23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
STK23_QT <- STK23[ which(STK23$chainend == 'End of Qtr_DM'), ]

#Sydney

#Split by rounds
SYD01 <- SYD[ which(SYD$round == '01'), ]
SYD02 <- SYD[ which(SYD$round == '02'), ]
SYD03 <- SYD[ which(SYD$round == '03'), ]
SYD04 <- SYD[ which(SYD$round == '04'), ]
SYD05 <- SYD[ which(SYD$round == '05'), ]
SYD06 <- SYD[ which(SYD$round == '06'), ]
SYD07 <- SYD[ which(SYD$round == '07'), ]
SYD08 <- SYD[ which(SYD$round == '08'), ]
SYD09 <- SYD[ which(SYD$round == '09'), ]
SYD10 <- SYD[ which(SYD$round == '10'), ]
SYD11 <- SYD[ which(SYD$round == '11'), ]
SYD12 <- SYD[ which(SYD$round == '12'), ]
SYD13 <- SYD[ which(SYD$round == '13'), ]
SYD14 <- SYD[ which(SYD$round == '14'), ]
SYD15 <- SYD[ which(SYD$round == '15'), ]
SYD16 <- SYD[ which(SYD$round == '16'), ]
SYD17 <- SYD[ which(SYD$round == '17'), ]
SYD18 <- SYD[ which(SYD$round == '18'), ]
SYD19 <- SYD[ which(SYD$round == '19'), ]
SYD20 <- SYD[ which(SYD$round == '20'), ]
SYD21 <- SYD[ which(SYD$round == '21'), ]
SYD22 <- SYD[ which(SYD$round == '22'), ]
SYD23 <- SYD[ which(SYD$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
SYD01_G <- SYD01[ which(SYD01$chainend == 'Goal_F'), ]
#Behind_F
SYD01_B <- SYD01[ which(SYD01$chainend == 'Behind_F'), ]
#Stoppage_F
SYD01_SF <- SYD01[ which(SYD01$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD01_TF <- SYD01[ which(SYD01$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD01_SAM <- SYD01[ which(SYD01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD01_TAM <- SYD01[ which(SYD01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD01_SDM <- SYD01[ which(SYD01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD01_TDM <- SYD01[ which(SYD01$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD01_SD <- SYD01[ which(SYD01$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD01_TD <- SYD01[ which(SYD01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD01_QT <- SYD01[ which(SYD01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
SYD02_G <- SYD02[ which(SYD02$chainend == 'Goal_F'), ]
#Behind_F
SYD02_B <- SYD02[ which(SYD02$chainend == 'Behind_F'), ]
#Stoppage_F
SYD02_SF <- SYD02[ which(SYD02$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD02_TF <- SYD02[ which(SYD02$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD02_SAM <- SYD02[ which(SYD02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD02_TAM <- SYD02[ which(SYD02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD02_SDM <- SYD02[ which(SYD02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD02_TDM <- SYD02[ which(SYD02$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD02_SD <- SYD02[ which(SYD02$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD02_TD <- SYD02[ which(SYD02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD02_QT <- SYD02[ which(SYD02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
SYD03_G <- SYD03[ which(SYD03$chainend == 'Goal_F'), ]
#Behind_F
SYD03_B <- SYD03[ which(SYD03$chainend == 'Behind_F'), ]
#Stoppage_F
SYD03_SF <- SYD03[ which(SYD03$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD03_TF <- SYD03[ which(SYD03$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD03_SAM <- SYD03[ which(SYD03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD03_TAM <- SYD03[ which(SYD03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD03_SDM <- SYD03[ which(SYD03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD03_TDM <- SYD03[ which(SYD03$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD03_SD <- SYD03[ which(SYD03$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD03_TD <- SYD03[ which(SYD03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD03_QT <- SYD03[ which(SYD03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
SYD04_G <- SYD04[ which(SYD04$chainend == 'Goal_F'), ]
#Behind_F
SYD04_B <- SYD04[ which(SYD04$chainend == 'Behind_F'), ]
#Stoppage_F
SYD04_SF <- SYD04[ which(SYD04$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD04_TF <- SYD04[ which(SYD04$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD04_SAM <- SYD04[ which(SYD04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD04_TAM <- SYD04[ which(SYD04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD04_SDM <- SYD04[ which(SYD04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD04_TDM <- SYD04[ which(SYD04$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD04_SD <- SYD04[ which(SYD04$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD04_TD <- SYD04[ which(SYD04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD04_QT <- SYD04[ which(SYD04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
SYD05_G <- SYD05[ which(SYD05$chainend == 'Goal_F'), ]
#Behind_F
SYD05_B <- SYD05[ which(SYD05$chainend == 'Behind_F'), ]
#Stoppage_F
SYD05_SF <- SYD05[ which(SYD05$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD05_TF <- SYD05[ which(SYD05$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD05_SAM <- SYD05[ which(SYD05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD05_TAM <- SYD05[ which(SYD05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD05_SDM <- SYD05[ which(SYD05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD05_TDM <- SYD05[ which(SYD05$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD05_SD <- SYD05[ which(SYD05$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD05_TD <- SYD05[ which(SYD05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD05_QT <- SYD05[ which(SYD05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
SYD06_G <- SYD06[ which(SYD06$chainend == 'Goal_F'), ]
#Behind_F
SYD06_B <- SYD06[ which(SYD06$chainend == 'Behind_F'), ]
#Stoppage_F
SYD06_SF <- SYD06[ which(SYD06$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD06_TF <- SYD06[ which(SYD06$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD06_SAM <- SYD06[ which(SYD06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD06_TAM <- SYD06[ which(SYD06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD06_SDM <- SYD06[ which(SYD06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD06_TDM <- SYD06[ which(SYD06$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD06_SD <- SYD06[ which(SYD06$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD06_TD <- SYD06[ which(SYD06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD06_QT <- SYD06[ which(SYD06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
SYD07_G <- SYD07[ which(SYD07$chainend == 'Goal_F'), ]
#Behind_F
SYD07_B <- SYD07[ which(SYD07$chainend == 'Behind_F'), ]
#Stoppage_F
SYD07_SF <- SYD07[ which(SYD07$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD07_TF <- SYD07[ which(SYD07$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD07_SAM <- SYD07[ which(SYD07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD07_TAM <- SYD07[ which(SYD07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD07_SDM <- SYD07[ which(SYD07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD07_TDM <- SYD07[ which(SYD07$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD07_SD <- SYD07[ which(SYD07$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD07_TD <- SYD07[ which(SYD07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD07_QT <- SYD07[ which(SYD07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
SYD08_G <- SYD08[ which(SYD08$chainend == 'Goal_F'), ]
#Behind_F
SYD08_B <- SYD08[ which(SYD08$chainend == 'Behind_F'), ]
#Stoppage_F
SYD08_SF <- SYD08[ which(SYD08$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD08_TF <- SYD08[ which(SYD08$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD08_SAM <- SYD08[ which(SYD08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD08_TAM <- SYD08[ which(SYD08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD08_SDM <- SYD08[ which(SYD08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD08_TDM <- SYD08[ which(SYD08$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD08_SD <- SYD08[ which(SYD08$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD08_TD <- SYD08[ which(SYD08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD08_QT <- SYD08[ which(SYD08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
SYD09_G <- SYD09[ which(SYD09$chainend == 'Goal_F'), ]
#Behind_F
SYD09_B <- SYD09[ which(SYD09$chainend == 'Behind_F'), ]
#Stoppage_F
SYD09_SF <- SYD09[ which(SYD09$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD09_TF <- SYD09[ which(SYD09$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD09_SAM <- SYD09[ which(SYD09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD09_TAM <- SYD09[ which(SYD09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD09_SDM <- SYD09[ which(SYD09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD09_TDM <- SYD09[ which(SYD09$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD09_SD <- SYD09[ which(SYD09$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD09_TD <- SYD09[ which(SYD09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD09_QT <- SYD09[ which(SYD09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
SYD10_G <- SYD10[ which(SYD10$chainend == 'Goal_F'), ]
#Behind_F
SYD10_B <- SYD10[ which(SYD10$chainend == 'Behind_F'), ]
#Stoppage_F
SYD10_SF <- SYD10[ which(SYD10$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD10_TF <- SYD10[ which(SYD10$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD10_SAM <- SYD10[ which(SYD10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD10_TAM <- SYD10[ which(SYD10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD10_SDM <- SYD10[ which(SYD10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD10_TDM <- SYD10[ which(SYD10$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD10_SD <- SYD10[ which(SYD10$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD10_TD <- SYD10[ which(SYD10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD10_QT <- SYD10[ which(SYD10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
SYD11_G <- SYD11[ which(SYD11$chainend == 'Goal_F'), ]
#Behind_F
SYD11_B <- SYD11[ which(SYD11$chainend == 'Behind_F'), ]
#Stoppage_F
SYD11_SF <- SYD11[ which(SYD11$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD11_TF <- SYD11[ which(SYD11$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD11_SAM <- SYD11[ which(SYD11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD11_TAM <- SYD11[ which(SYD11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD11_SDM <- SYD11[ which(SYD11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD11_TDM <- SYD11[ which(SYD11$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD11_SD <- SYD11[ which(SYD11$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD11_TD <- SYD11[ which(SYD11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD11_QT <- SYD11[ which(SYD11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
SYD12_G <- SYD12[ which(SYD12$chainend == 'Goal_F'), ]
#Behind_F
SYD12_B <- SYD12[ which(SYD12$chainend == 'Behind_F'), ]
#Stoppage_F
SYD12_SF <- SYD12[ which(SYD12$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD12_TF <- SYD12[ which(SYD12$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD12_SAM <- SYD12[ which(SYD12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD12_TAM <- SYD12[ which(SYD12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD12_SDM <- SYD12[ which(SYD12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD12_TDM <- SYD12[ which(SYD12$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD12_SD <- SYD12[ which(SYD12$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD12_TD <- SYD12[ which(SYD12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD12_QT <- SYD12[ which(SYD12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
SYD13_G <- SYD13[ which(SYD13$chainend == 'Goal_F'), ]
#Behind_F
SYD13_B <- SYD13[ which(SYD13$chainend == 'Behind_F'), ]
#Stoppage_F
SYD13_SF <- SYD13[ which(SYD13$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD13_TF <- SYD13[ which(SYD13$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD13_SAM <- SYD13[ which(SYD13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD13_TAM <- SYD13[ which(SYD13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD13_SDM <- SYD13[ which(SYD13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD13_TDM <- SYD13[ which(SYD13$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD13_SD <- SYD13[ which(SYD13$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD13_TD <- SYD13[ which(SYD13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD13_QT <- SYD13[ which(SYD13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
SYD14_G <- SYD14[ which(SYD14$chainend == 'Goal_F'), ]
#Behind_F
SYD14_B <- SYD14[ which(SYD14$chainend == 'Behind_F'), ]
#Stoppage_F
SYD14_SF <- SYD14[ which(SYD14$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD14_TF <- SYD14[ which(SYD14$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD14_SAM <- SYD14[ which(SYD14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD14_TAM <- SYD14[ which(SYD14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD14_SDM <- SYD14[ which(SYD14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD14_TDM <- SYD14[ which(SYD14$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD14_SD <- SYD14[ which(SYD14$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD14_TD <- SYD14[ which(SYD14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD14_QT <- SYD14[ which(SYD14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
SYD15_G <- SYD15[ which(SYD15$chainend == 'Goal_F'), ]
#Behind_F
SYD15_B <- SYD15[ which(SYD15$chainend == 'Behind_F'), ]
#Stoppage_F
SYD15_SF <- SYD15[ which(SYD15$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD15_TF <- SYD15[ which(SYD15$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD15_SAM <- SYD15[ which(SYD15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD15_TAM <- SYD15[ which(SYD15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD15_SDM <- SYD15[ which(SYD15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD15_TDM <- SYD15[ which(SYD15$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD15_SD <- SYD15[ which(SYD15$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD15_TD <- SYD15[ which(SYD15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD15_QT <- SYD15[ which(SYD15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
SYD16_G <- SYD16[ which(SYD16$chainend == 'Goal_F'), ]
#Behind_F
SYD16_B <- SYD16[ which(SYD16$chainend == 'Behind_F'), ]
#Stoppage_F
SYD16_SF <- SYD16[ which(SYD16$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD16_TF <- SYD16[ which(SYD16$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD16_SAM <- SYD16[ which(SYD16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD16_TAM <- SYD16[ which(SYD16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD16_SDM <- SYD16[ which(SYD16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD16_TDM <- SYD16[ which(SYD16$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD16_SD <- SYD16[ which(SYD16$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD16_TD <- SYD16[ which(SYD16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD16_QT <- SYD16[ which(SYD16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
SYD17_G <- SYD17[ which(SYD17$chainend == 'Goal_F'), ]
#Behind_F
SYD17_B <- SYD17[ which(SYD17$chainend == 'Behind_F'), ]
#Stoppage_F
SYD17_SF <- SYD17[ which(SYD17$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD17_TF <- SYD17[ which(SYD17$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD17_SAM <- SYD17[ which(SYD17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD17_TAM <- SYD17[ which(SYD17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD17_SDM <- SYD17[ which(SYD17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD17_TDM <- SYD17[ which(SYD17$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD17_SD <- SYD17[ which(SYD17$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD17_TD <- SYD17[ which(SYD17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD17_QT <- SYD17[ which(SYD17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
SYD18_G <- SYD18[ which(SYD18$chainend == 'Goal_F'), ]
#Behind_F
SYD18_B <- SYD18[ which(SYD18$chainend == 'Behind_F'), ]
#Stoppage_F
SYD18_SF <- SYD18[ which(SYD18$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD18_TF <- SYD18[ which(SYD18$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD18_SAM <- SYD18[ which(SYD18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD18_TAM <- SYD18[ which(SYD18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD18_SDM <- SYD18[ which(SYD18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD18_TDM <- SYD18[ which(SYD18$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD18_SD <- SYD18[ which(SYD18$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD18_TD <- SYD18[ which(SYD18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD18_QT <- SYD18[ which(SYD18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
SYD19_G <- SYD19[ which(SYD19$chainend == 'Goal_F'), ]
#Behind_F
SYD19_B <- SYD19[ which(SYD19$chainend == 'Behind_F'), ]
#Stoppage_F
SYD19_SF <- SYD19[ which(SYD19$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD19_TF <- SYD19[ which(SYD19$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD19_SAM <- SYD19[ which(SYD19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD19_TAM <- SYD19[ which(SYD19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD19_SDM <- SYD19[ which(SYD19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD19_TDM <- SYD19[ which(SYD19$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD19_SD <- SYD19[ which(SYD19$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD19_TD <- SYD19[ which(SYD19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD19_QT <- SYD19[ which(SYD19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
SYD20_G <- SYD20[ which(SYD20$chainend == 'Goal_F'), ]
#Behind_F
SYD20_B <- SYD20[ which(SYD20$chainend == 'Behind_F'), ]
#Stoppage_F
SYD20_SF <- SYD20[ which(SYD20$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD20_TF <- SYD20[ which(SYD20$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD20_SAM <- SYD20[ which(SYD20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD20_TAM <- SYD20[ which(SYD20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD20_SDM <- SYD20[ which(SYD20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD20_TDM <- SYD20[ which(SYD20$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD20_SD <- SYD20[ which(SYD20$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD20_TD <- SYD20[ which(SYD20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD20_QT <- SYD20[ which(SYD20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
SYD21_G <- SYD21[ which(SYD21$chainend == 'Goal_F'), ]
#Behind_F
SYD21_B <- SYD21[ which(SYD21$chainend == 'Behind_F'), ]
#Stoppage_F
SYD21_SF <- SYD21[ which(SYD21$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD21_TF <- SYD21[ which(SYD21$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD21_SAM <- SYD21[ which(SYD21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD21_TAM <- SYD21[ which(SYD21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD21_SDM <- SYD21[ which(SYD21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD21_TDM <- SYD21[ which(SYD21$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD21_SD <- SYD21[ which(SYD21$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD21_TD <- SYD21[ which(SYD21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD21_QT <- SYD21[ which(SYD21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
SYD22_G <- SYD22[ which(SYD22$chainend == 'Goal_F'), ]
#Behind_F
SYD22_B <- SYD22[ which(SYD22$chainend == 'Behind_F'), ]
#Stoppage_F
SYD22_SF <- SYD22[ which(SYD22$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD22_TF <- SYD22[ which(SYD22$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD22_SAM <- SYD22[ which(SYD22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD22_TAM <- SYD22[ which(SYD22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD22_SDM <- SYD22[ which(SYD22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD22_TDM <- SYD22[ which(SYD22$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD22_SD <- SYD22[ which(SYD22$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD22_TD <- SYD22[ which(SYD22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD22_QT <- SYD22[ which(SYD22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
SYD23_G <- SYD23[ which(SYD23$chainend == 'Goal_F'), ]
#Behind_F
SYD23_B <- SYD23[ which(SYD23$chainend == 'Behind_F'), ]
#Stoppage_F
SYD23_SF <- SYD23[ which(SYD23$chainend == 'Stoppage_F'), ]
#Turnover_F
SYD23_TF <- SYD23[ which(SYD23$chainend == 'Turnover_F'), ]
#Stoppage_AM
SYD23_SAM <- SYD23[ which(SYD23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
SYD23_TAM <- SYD23[ which(SYD23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
SYD23_SDM <- SYD23[ which(SYD23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
SYD23_TDM <- SYD23[ which(SYD23$chainend == 'Turnover_DM'), ]
#Stoppage_D
SYD23_SD <- SYD23[ which(SYD23$chainend == 'Stoppage_D'), ]
#Turnover_D
SYD23_TD <- SYD23[ which(SYD23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
SYD23_QT <- SYD23[ which(SYD23$chainend == 'End of Qtr_DM'), ]

#Western Bulldogs

#Split by rounds
WB01 <- WB[ which(WB$round == '01'), ]
WB02 <- WB[ which(WB$round == '02'), ]
WB03 <- WB[ which(WB$round == '03'), ]
WB04 <- WB[ which(WB$round == '04'), ]
WB05 <- WB[ which(WB$round == '05'), ]
WB06 <- WB[ which(WB$round == '06'), ]
WB07 <- WB[ which(WB$round == '07'), ]
WB08 <- WB[ which(WB$round == '08'), ]
WB09 <- WB[ which(WB$round == '09'), ]
WB10 <- WB[ which(WB$round == '10'), ]
WB11 <- WB[ which(WB$round == '11'), ]
WB12 <- WB[ which(WB$round == '12'), ]
WB13 <- WB[ which(WB$round == '13'), ]
WB14 <- WB[ which(WB$round == '14'), ]
WB15 <- WB[ which(WB$round == '15'), ]
WB16 <- WB[ which(WB$round == '16'), ]
WB17 <- WB[ which(WB$round == '17'), ]
WB18 <- WB[ which(WB$round == '18'), ]
WB19 <- WB[ which(WB$round == '19'), ]
WB20 <- WB[ which(WB$round == '20'), ]
WB21 <- WB[ which(WB$round == '21'), ]
WB22 <- WB[ which(WB$round == '22'), ]
WB23 <- WB[ which(WB$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
WB01_G <- WB01[ which(WB01$chainend == 'Goal_F'), ]
#Behind_F
WB01_B <- WB01[ which(WB01$chainend == 'Behind_F'), ]
#Stoppage_F
WB01_SF <- WB01[ which(WB01$chainend == 'Stoppage_F'), ]
#Turnover_F
WB01_TF <- WB01[ which(WB01$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB01_SAM <- WB01[ which(WB01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB01_TAM <- WB01[ which(WB01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB01_SDM <- WB01[ which(WB01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB01_TDM <- WB01[ which(WB01$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB01_SD <- WB01[ which(WB01$chainend == 'Stoppage_D'), ]
#Turnover_D
WB01_TD <- WB01[ which(WB01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB01_QT <- WB01[ which(WB01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
WB02_G <- WB02[ which(WB02$chainend == 'Goal_F'), ]
#Behind_F
WB02_B <- WB02[ which(WB02$chainend == 'Behind_F'), ]
#Stoppage_F
WB02_SF <- WB02[ which(WB02$chainend == 'Stoppage_F'), ]
#Turnover_F
WB02_TF <- WB02[ which(WB02$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB02_SAM <- WB02[ which(WB02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB02_TAM <- WB02[ which(WB02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB02_SDM <- WB02[ which(WB02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB02_TDM <- WB02[ which(WB02$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB02_SD <- WB02[ which(WB02$chainend == 'Stoppage_D'), ]
#Turnover_D
WB02_TD <- WB02[ which(WB02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB02_QT <- WB02[ which(WB02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
WB03_G <- WB03[ which(WB03$chainend == 'Goal_F'), ]
#Behind_F
WB03_B <- WB03[ which(WB03$chainend == 'Behind_F'), ]
#Stoppage_F
WB03_SF <- WB03[ which(WB03$chainend == 'Stoppage_F'), ]
#Turnover_F
WB03_TF <- WB03[ which(WB03$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB03_SAM <- WB03[ which(WB03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB03_TAM <- WB03[ which(WB03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB03_SDM <- WB03[ which(WB03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB03_TDM <- WB03[ which(WB03$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB03_SD <- WB03[ which(WB03$chainend == 'Stoppage_D'), ]
#Turnover_D
WB03_TD <- WB03[ which(WB03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB03_QT <- WB03[ which(WB03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
WB04_G <- WB04[ which(WB04$chainend == 'Goal_F'), ]
#Behind_F
WB04_B <- WB04[ which(WB04$chainend == 'Behind_F'), ]
#Stoppage_F
WB04_SF <- WB04[ which(WB04$chainend == 'Stoppage_F'), ]
#Turnover_F
WB04_TF <- WB04[ which(WB04$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB04_SAM <- WB04[ which(WB04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB04_TAM <- WB04[ which(WB04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB04_SDM <- WB04[ which(WB04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB04_TDM <- WB04[ which(WB04$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB04_SD <- WB04[ which(WB04$chainend == 'Stoppage_D'), ]
#Turnover_D
WB04_TD <- WB04[ which(WB04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB04_QT <- WB04[ which(WB04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
WB05_G <- WB05[ which(WB05$chainend == 'Goal_F'), ]
#Behind_F
WB05_B <- WB05[ which(WB05$chainend == 'Behind_F'), ]
#Stoppage_F
WB05_SF <- WB05[ which(WB05$chainend == 'Stoppage_F'), ]
#Turnover_F
WB05_TF <- WB05[ which(WB05$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB05_SAM <- WB05[ which(WB05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB05_TAM <- WB05[ which(WB05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB05_SDM <- WB05[ which(WB05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB05_TDM <- WB05[ which(WB05$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB05_SD <- WB05[ which(WB05$chainend == 'Stoppage_D'), ]
#Turnover_D
WB05_TD <- WB05[ which(WB05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB05_QT <- WB05[ which(WB05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
WB06_G <- WB06[ which(WB06$chainend == 'Goal_F'), ]
#Behind_F
WB06_B <- WB06[ which(WB06$chainend == 'Behind_F'), ]
#Stoppage_F
WB06_SF <- WB06[ which(WB06$chainend == 'Stoppage_F'), ]
#Turnover_F
WB06_TF <- WB06[ which(WB06$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB06_SAM <- WB06[ which(WB06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB06_TAM <- WB06[ which(WB06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB06_SDM <- WB06[ which(WB06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB06_TDM <- WB06[ which(WB06$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB06_SD <- WB06[ which(WB06$chainend == 'Stoppage_D'), ]
#Turnover_D
WB06_TD <- WB06[ which(WB06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB06_QT <- WB06[ which(WB06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
WB07_G <- WB07[ which(WB07$chainend == 'Goal_F'), ]
#Behind_F
WB07_B <- WB07[ which(WB07$chainend == 'Behind_F'), ]
#Stoppage_F
WB07_SF <- WB07[ which(WB07$chainend == 'Stoppage_F'), ]
#Turnover_F
WB07_TF <- WB07[ which(WB07$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB07_SAM <- WB07[ which(WB07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB07_TAM <- WB07[ which(WB07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB07_SDM <- WB07[ which(WB07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB07_TDM <- WB07[ which(WB07$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB07_SD <- WB07[ which(WB07$chainend == 'Stoppage_D'), ]
#Turnover_D
WB07_TD <- WB07[ which(WB07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB07_QT <- WB07[ which(WB07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
WB08_G <- WB08[ which(WB08$chainend == 'Goal_F'), ]
#Behind_F
WB08_B <- WB08[ which(WB08$chainend == 'Behind_F'), ]
#Stoppage_F
WB08_SF <- WB08[ which(WB08$chainend == 'Stoppage_F'), ]
#Turnover_F
WB08_TF <- WB08[ which(WB08$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB08_SAM <- WB08[ which(WB08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB08_TAM <- WB08[ which(WB08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB08_SDM <- WB08[ which(WB08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB08_TDM <- WB08[ which(WB08$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB08_SD <- WB08[ which(WB08$chainend == 'Stoppage_D'), ]
#Turnover_D
WB08_TD <- WB08[ which(WB08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB08_QT <- WB08[ which(WB08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
WB09_G <- WB09[ which(WB09$chainend == 'Goal_F'), ]
#Behind_F
WB09_B <- WB09[ which(WB09$chainend == 'Behind_F'), ]
#Stoppage_F
WB09_SF <- WB09[ which(WB09$chainend == 'Stoppage_F'), ]
#Turnover_F
WB09_TF <- WB09[ which(WB09$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB09_SAM <- WB09[ which(WB09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB09_TAM <- WB09[ which(WB09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB09_SDM <- WB09[ which(WB09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB09_TDM <- WB09[ which(WB09$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB09_SD <- WB09[ which(WB09$chainend == 'Stoppage_D'), ]
#Turnover_D
WB09_TD <- WB09[ which(WB09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB09_QT <- WB09[ which(WB09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
WB10_G <- WB10[ which(WB10$chainend == 'Goal_F'), ]
#Behind_F
WB10_B <- WB10[ which(WB10$chainend == 'Behind_F'), ]
#Stoppage_F
WB10_SF <- WB10[ which(WB10$chainend == 'Stoppage_F'), ]
#Turnover_F
WB10_TF <- WB10[ which(WB10$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB10_SAM <- WB10[ which(WB10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB10_TAM <- WB10[ which(WB10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB10_SDM <- WB10[ which(WB10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB10_TDM <- WB10[ which(WB10$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB10_SD <- WB10[ which(WB10$chainend == 'Stoppage_D'), ]
#Turnover_D
WB10_TD <- WB10[ which(WB10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB10_QT <- WB10[ which(WB10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
WB11_G <- WB11[ which(WB11$chainend == 'Goal_F'), ]
#Behind_F
WB11_B <- WB11[ which(WB11$chainend == 'Behind_F'), ]
#Stoppage_F
WB11_SF <- WB11[ which(WB11$chainend == 'Stoppage_F'), ]
#Turnover_F
WB11_TF <- WB11[ which(WB11$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB11_SAM <- WB11[ which(WB11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB11_TAM <- WB11[ which(WB11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB11_SDM <- WB11[ which(WB11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB11_TDM <- WB11[ which(WB11$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB11_SD <- WB11[ which(WB11$chainend == 'Stoppage_D'), ]
#Turnover_D
WB11_TD <- WB11[ which(WB11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB11_QT <- WB11[ which(WB11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
WB12_G <- WB12[ which(WB12$chainend == 'Goal_F'), ]
#Behind_F
WB12_B <- WB12[ which(WB12$chainend == 'Behind_F'), ]
#Stoppage_F
WB12_SF <- WB12[ which(WB12$chainend == 'Stoppage_F'), ]
#Turnover_F
WB12_TF <- WB12[ which(WB12$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB12_SAM <- WB12[ which(WB12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB12_TAM <- WB12[ which(WB12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB12_SDM <- WB12[ which(WB12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB12_TDM <- WB12[ which(WB12$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB12_SD <- WB12[ which(WB12$chainend == 'Stoppage_D'), ]
#Turnover_D
WB12_TD <- WB12[ which(WB12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB12_QT <- WB12[ which(WB12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
WB13_G <- WB13[ which(WB13$chainend == 'Goal_F'), ]
#Behind_F
WB13_B <- WB13[ which(WB13$chainend == 'Behind_F'), ]
#Stoppage_F
WB13_SF <- WB13[ which(WB13$chainend == 'Stoppage_F'), ]
#Turnover_F
WB13_TF <- WB13[ which(WB13$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB13_SAM <- WB13[ which(WB13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB13_TAM <- WB13[ which(WB13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB13_SDM <- WB13[ which(WB13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB13_TDM <- WB13[ which(WB13$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB13_SD <- WB13[ which(WB13$chainend == 'Stoppage_D'), ]
#Turnover_D
WB13_TD <- WB13[ which(WB13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB13_QT <- WB13[ which(WB13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
WB14_G <- WB14[ which(WB14$chainend == 'Goal_F'), ]
#Behind_F
WB14_B <- WB14[ which(WB14$chainend == 'Behind_F'), ]
#Stoppage_F
WB14_SF <- WB14[ which(WB14$chainend == 'Stoppage_F'), ]
#Turnover_F
WB14_TF <- WB14[ which(WB14$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB14_SAM <- WB14[ which(WB14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB14_TAM <- WB14[ which(WB14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB14_SDM <- WB14[ which(WB14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB14_TDM <- WB14[ which(WB14$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB14_SD <- WB14[ which(WB14$chainend == 'Stoppage_D'), ]
#Turnover_D
WB14_TD <- WB14[ which(WB14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB14_QT <- WB14[ which(WB14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
WB15_G <- WB15[ which(WB15$chainend == 'Goal_F'), ]
#Behind_F
WB15_B <- WB15[ which(WB15$chainend == 'Behind_F'), ]
#Stoppage_F
WB15_SF <- WB15[ which(WB15$chainend == 'Stoppage_F'), ]
#Turnover_F
WB15_TF <- WB15[ which(WB15$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB15_SAM <- WB15[ which(WB15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB15_TAM <- WB15[ which(WB15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB15_SDM <- WB15[ which(WB15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB15_TDM <- WB15[ which(WB15$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB15_SD <- WB15[ which(WB15$chainend == 'Stoppage_D'), ]
#Turnover_D
WB15_TD <- WB15[ which(WB15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB15_QT <- WB15[ which(WB15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
WB16_G <- WB16[ which(WB16$chainend == 'Goal_F'), ]
#Behind_F
WB16_B <- WB16[ which(WB16$chainend == 'Behind_F'), ]
#Stoppage_F
WB16_SF <- WB16[ which(WB16$chainend == 'Stoppage_F'), ]
#Turnover_F
WB16_TF <- WB16[ which(WB16$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB16_SAM <- WB16[ which(WB16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB16_TAM <- WB16[ which(WB16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB16_SDM <- WB16[ which(WB16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB16_TDM <- WB16[ which(WB16$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB16_SD <- WB16[ which(WB16$chainend == 'Stoppage_D'), ]
#Turnover_D
WB16_TD <- WB16[ which(WB16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB16_QT <- WB16[ which(WB16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
WB17_G <- WB17[ which(WB17$chainend == 'Goal_F'), ]
#Behind_F
WB17_B <- WB17[ which(WB17$chainend == 'Behind_F'), ]
#Stoppage_F
WB17_SF <- WB17[ which(WB17$chainend == 'Stoppage_F'), ]
#Turnover_F
WB17_TF <- WB17[ which(WB17$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB17_SAM <- WB17[ which(WB17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB17_TAM <- WB17[ which(WB17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB17_SDM <- WB17[ which(WB17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB17_TDM <- WB17[ which(WB17$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB17_SD <- WB17[ which(WB17$chainend == 'Stoppage_D'), ]
#Turnover_D
WB17_TD <- WB17[ which(WB17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB17_QT <- WB17[ which(WB17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
WB18_G <- WB18[ which(WB18$chainend == 'Goal_F'), ]
#Behind_F
WB18_B <- WB18[ which(WB18$chainend == 'Behind_F'), ]
#Stoppage_F
WB18_SF <- WB18[ which(WB18$chainend == 'Stoppage_F'), ]
#Turnover_F
WB18_TF <- WB18[ which(WB18$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB18_SAM <- WB18[ which(WB18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB18_TAM <- WB18[ which(WB18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB18_SDM <- WB18[ which(WB18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB18_TDM <- WB18[ which(WB18$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB18_SD <- WB18[ which(WB18$chainend == 'Stoppage_D'), ]
#Turnover_D
WB18_TD <- WB18[ which(WB18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB18_QT <- WB18[ which(WB18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
WB19_G <- WB19[ which(WB19$chainend == 'Goal_F'), ]
#Behind_F
WB19_B <- WB19[ which(WB19$chainend == 'Behind_F'), ]
#Stoppage_F
WB19_SF <- WB19[ which(WB19$chainend == 'Stoppage_F'), ]
#Turnover_F
WB19_TF <- WB19[ which(WB19$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB19_SAM <- WB19[ which(WB19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB19_TAM <- WB19[ which(WB19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB19_SDM <- WB19[ which(WB19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB19_TDM <- WB19[ which(WB19$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB19_SD <- WB19[ which(WB19$chainend == 'Stoppage_D'), ]
#Turnover_D
WB19_TD <- WB19[ which(WB19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB19_QT <- WB19[ which(WB19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
WB20_G <- WB20[ which(WB20$chainend == 'Goal_F'), ]
#Behind_F
WB20_B <- WB20[ which(WB20$chainend == 'Behind_F'), ]
#Stoppage_F
WB20_SF <- WB20[ which(WB20$chainend == 'Stoppage_F'), ]
#Turnover_F
WB20_TF <- WB20[ which(WB20$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB20_SAM <- WB20[ which(WB20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB20_TAM <- WB20[ which(WB20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB20_SDM <- WB20[ which(WB20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB20_TDM <- WB20[ which(WB20$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB20_SD <- WB20[ which(WB20$chainend == 'Stoppage_D'), ]
#Turnover_D
WB20_TD <- WB20[ which(WB20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB20_QT <- WB20[ which(WB20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
WB21_G <- WB21[ which(WB21$chainend == 'Goal_F'), ]
#Behind_F
WB21_B <- WB21[ which(WB21$chainend == 'Behind_F'), ]
#Stoppage_F
WB21_SF <- WB21[ which(WB21$chainend == 'Stoppage_F'), ]
#Turnover_F
WB21_TF <- WB21[ which(WB21$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB21_SAM <- WB21[ which(WB21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB21_TAM <- WB21[ which(WB21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB21_SDM <- WB21[ which(WB21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB21_TDM <- WB21[ which(WB21$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB21_SD <- WB21[ which(WB21$chainend == 'Stoppage_D'), ]
#Turnover_D
WB21_TD <- WB21[ which(WB21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB21_QT <- WB21[ which(WB21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
WB22_G <- WB22[ which(WB22$chainend == 'Goal_F'), ]
#Behind_F
WB22_B <- WB22[ which(WB22$chainend == 'Behind_F'), ]
#Stoppage_F
WB22_SF <- WB22[ which(WB22$chainend == 'Stoppage_F'), ]
#Turnover_F
WB22_TF <- WB22[ which(WB22$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB22_SAM <- WB22[ which(WB22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB22_TAM <- WB22[ which(WB22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB22_SDM <- WB22[ which(WB22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB22_TDM <- WB22[ which(WB22$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB22_SD <- WB22[ which(WB22$chainend == 'Stoppage_D'), ]
#Turnover_D
WB22_TD <- WB22[ which(WB22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB22_QT <- WB22[ which(WB22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
WB23_G <- WB23[ which(WB23$chainend == 'Goal_F'), ]
#Behind_F
WB23_B <- WB23[ which(WB23$chainend == 'Behind_F'), ]
#Stoppage_F
WB23_SF <- WB23[ which(WB23$chainend == 'Stoppage_F'), ]
#Turnover_F
WB23_TF <- WB23[ which(WB23$chainend == 'Turnover_F'), ]
#Stoppage_AM
WB23_SAM <- WB23[ which(WB23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WB23_TAM <- WB23[ which(WB23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WB23_SDM <- WB23[ which(WB23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WB23_TDM <- WB23[ which(WB23$chainend == 'Turnover_DM'), ]
#Stoppage_D
WB23_SD <- WB23[ which(WB23$chainend == 'Stoppage_D'), ]
#Turnover_D
WB23_TD <- WB23[ which(WB23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WB23_QT <- WB23[ which(WB23$chainend == 'End of Qtr_DM'), ]

#West Coast Eagles

#Split by rounds
WCE01 <- WCE[ which(WCE$round == '01'), ]
WCE02 <- WCE[ which(WCE$round == '02'), ]
WCE03 <- WCE[ which(WCE$round == '03'), ]
WCE04 <- WCE[ which(WCE$round == '04'), ]
WCE05 <- WCE[ which(WCE$round == '05'), ]
WCE06 <- WCE[ which(WCE$round == '06'), ]
WCE07 <- WCE[ which(WCE$round == '07'), ]
WCE08 <- WCE[ which(WCE$round == '08'), ]
WCE09 <- WCE[ which(WCE$round == '09'), ]
WCE10 <- WCE[ which(WCE$round == '10'), ]
WCE11 <- WCE[ which(WCE$round == '11'), ]
WCE12 <- WCE[ which(WCE$round == '12'), ]
WCE13 <- WCE[ which(WCE$round == '13'), ]
WCE14 <- WCE[ which(WCE$round == '14'), ]
WCE15 <- WCE[ which(WCE$round == '15'), ]
WCE16 <- WCE[ which(WCE$round == '16'), ]
WCE17 <- WCE[ which(WCE$round == '17'), ]
WCE18 <- WCE[ which(WCE$round == '18'), ]
WCE19 <- WCE[ which(WCE$round == '19'), ]
WCE20 <- WCE[ which(WCE$round == '20'), ]
WCE21 <- WCE[ which(WCE$round == '21'), ]
WCE22 <- WCE[ which(WCE$round == '22'), ]
WCE23 <- WCE[ which(WCE$round == '23'), ]


#############################################################################


##Split by kick-in outcome

#Round 1:
#Goal_F
WCE01_G <- WCE01[ which(WCE01$chainend == 'Goal_F'), ]
#Behind_F
WCE01_B <- WCE01[ which(WCE01$chainend == 'Behind_F'), ]
#Stoppage_F
WCE01_SF <- WCE01[ which(WCE01$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE01_TF <- WCE01[ which(WCE01$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE01_SAM <- WCE01[ which(WCE01$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE01_TAM <- WCE01[ which(WCE01$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE01_SDM <- WCE01[ which(WCE01$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE01_TDM <- WCE01[ which(WCE01$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE01_SD <- WCE01[ which(WCE01$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE01_TD <- WCE01[ which(WCE01$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE01_QT <- WCE01[ which(WCE01$chainend == 'End of Qtr_DM'), ]

#Round 2:
#Goal_F
WCE02_G <- WCE02[ which(WCE02$chainend == 'Goal_F'), ]
#Behind_F
WCE02_B <- WCE02[ which(WCE02$chainend == 'Behind_F'), ]
#Stoppage_F
WCE02_SF <- WCE02[ which(WCE02$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE02_TF <- WCE02[ which(WCE02$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE02_SAM <- WCE02[ which(WCE02$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE02_TAM <- WCE02[ which(WCE02$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE02_SDM <- WCE02[ which(WCE02$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE02_TDM <- WCE02[ which(WCE02$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE02_SD <- WCE02[ which(WCE02$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE02_TD <- WCE02[ which(WCE02$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE02_QT <- WCE02[ which(WCE02$chainend == 'End of Qtr_DM'), ]

#Round 3:
#Goal_F
WCE03_G <- WCE03[ which(WCE03$chainend == 'Goal_F'), ]
#Behind_F
WCE03_B <- WCE03[ which(WCE03$chainend == 'Behind_F'), ]
#Stoppage_F
WCE03_SF <- WCE03[ which(WCE03$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE03_TF <- WCE03[ which(WCE03$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE03_SAM <- WCE03[ which(WCE03$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE03_TAM <- WCE03[ which(WCE03$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE03_SDM <- WCE03[ which(WCE03$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE03_TDM <- WCE03[ which(WCE03$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE03_SD <- WCE03[ which(WCE03$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE03_TD <- WCE03[ which(WCE03$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE03_QT <- WCE03[ which(WCE03$chainend == 'End of Qtr_DM'), ]

#Round 4:
#Goal_F
WCE04_G <- WCE04[ which(WCE04$chainend == 'Goal_F'), ]
#Behind_F
WCE04_B <- WCE04[ which(WCE04$chainend == 'Behind_F'), ]
#Stoppage_F
WCE04_SF <- WCE04[ which(WCE04$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE04_TF <- WCE04[ which(WCE04$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE04_SAM <- WCE04[ which(WCE04$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE04_TAM <- WCE04[ which(WCE04$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE04_SDM <- WCE04[ which(WCE04$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE04_TDM <- WCE04[ which(WCE04$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE04_SD <- WCE04[ which(WCE04$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE04_TD <- WCE04[ which(WCE04$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE04_QT <- WCE04[ which(WCE04$chainend == 'End of Qtr_DM'), ]

#Round 5:
#Goal_F
WCE05_G <- WCE05[ which(WCE05$chainend == 'Goal_F'), ]
#Behind_F
WCE05_B <- WCE05[ which(WCE05$chainend == 'Behind_F'), ]
#Stoppage_F
WCE05_SF <- WCE05[ which(WCE05$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE05_TF <- WCE05[ which(WCE05$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE05_SAM <- WCE05[ which(WCE05$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE05_TAM <- WCE05[ which(WCE05$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE05_SDM <- WCE05[ which(WCE05$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE05_TDM <- WCE05[ which(WCE05$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE05_SD <- WCE05[ which(WCE05$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE05_TD <- WCE05[ which(WCE05$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE05_QT <- WCE05[ which(WCE05$chainend == 'End of Qtr_DM'), ]

#Round 6:
#Goal_F
WCE06_G <- WCE06[ which(WCE06$chainend == 'Goal_F'), ]
#Behind_F
WCE06_B <- WCE06[ which(WCE06$chainend == 'Behind_F'), ]
#Stoppage_F
WCE06_SF <- WCE06[ which(WCE06$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE06_TF <- WCE06[ which(WCE06$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE06_SAM <- WCE06[ which(WCE06$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE06_TAM <- WCE06[ which(WCE06$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE06_SDM <- WCE06[ which(WCE06$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE06_TDM <- WCE06[ which(WCE06$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE06_SD <- WCE06[ which(WCE06$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE06_TD <- WCE06[ which(WCE06$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE06_QT <- WCE06[ which(WCE06$chainend == 'End of Qtr_DM'), ]

#Round 7:
#Goal_F
WCE07_G <- WCE07[ which(WCE07$chainend == 'Goal_F'), ]
#Behind_F
WCE07_B <- WCE07[ which(WCE07$chainend == 'Behind_F'), ]
#Stoppage_F
WCE07_SF <- WCE07[ which(WCE07$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE07_TF <- WCE07[ which(WCE07$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE07_SAM <- WCE07[ which(WCE07$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE07_TAM <- WCE07[ which(WCE07$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE07_SDM <- WCE07[ which(WCE07$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE07_TDM <- WCE07[ which(WCE07$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE07_SD <- WCE07[ which(WCE07$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE07_TD <- WCE07[ which(WCE07$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE07_QT <- WCE07[ which(WCE07$chainend == 'End of Qtr_DM'), ]

#Round 8:
#Goal_F
WCE08_G <- WCE08[ which(WCE08$chainend == 'Goal_F'), ]
#Behind_F
WCE08_B <- WCE08[ which(WCE08$chainend == 'Behind_F'), ]
#Stoppage_F
WCE08_SF <- WCE08[ which(WCE08$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE08_TF <- WCE08[ which(WCE08$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE08_SAM <- WCE08[ which(WCE08$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE08_TAM <- WCE08[ which(WCE08$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE08_SDM <- WCE08[ which(WCE08$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE08_TDM <- WCE08[ which(WCE08$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE08_SD <- WCE08[ which(WCE08$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE08_TD <- WCE08[ which(WCE08$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE08_QT <- WCE08[ which(WCE08$chainend == 'End of Qtr_DM'), ]

#Round 9:
#Goal_F
WCE09_G <- WCE09[ which(WCE09$chainend == 'Goal_F'), ]
#Behind_F
WCE09_B <- WCE09[ which(WCE09$chainend == 'Behind_F'), ]
#Stoppage_F
WCE09_SF <- WCE09[ which(WCE09$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE09_TF <- WCE09[ which(WCE09$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE09_SAM <- WCE09[ which(WCE09$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE09_TAM <- WCE09[ which(WCE09$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE09_SDM <- WCE09[ which(WCE09$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE09_TDM <- WCE09[ which(WCE09$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE09_SD <- WCE09[ which(WCE09$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE09_TD <- WCE09[ which(WCE09$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE09_QT <- WCE09[ which(WCE09$chainend == 'End of Qtr_DM'), ]

#Round 10:
#Goal_F
WCE10_G <- WCE10[ which(WCE10$chainend == 'Goal_F'), ]
#Behind_F
WCE10_B <- WCE10[ which(WCE10$chainend == 'Behind_F'), ]
#Stoppage_F
WCE10_SF <- WCE10[ which(WCE10$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE10_TF <- WCE10[ which(WCE10$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE10_SAM <- WCE10[ which(WCE10$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE10_TAM <- WCE10[ which(WCE10$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE10_SDM <- WCE10[ which(WCE10$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE10_TDM <- WCE10[ which(WCE10$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE10_SD <- WCE10[ which(WCE10$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE10_TD <- WCE10[ which(WCE10$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE10_QT <- WCE10[ which(WCE10$chainend == 'End of Qtr_DM'), ]

#Round 11:
#Goal_F
WCE11_G <- WCE11[ which(WCE11$chainend == 'Goal_F'), ]
#Behind_F
WCE11_B <- WCE11[ which(WCE11$chainend == 'Behind_F'), ]
#Stoppage_F
WCE11_SF <- WCE11[ which(WCE11$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE11_TF <- WCE11[ which(WCE11$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE11_SAM <- WCE11[ which(WCE11$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE11_TAM <- WCE11[ which(WCE11$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE11_SDM <- WCE11[ which(WCE11$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE11_TDM <- WCE11[ which(WCE11$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE11_SD <- WCE11[ which(WCE11$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE11_TD <- WCE11[ which(WCE11$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE11_QT <- WCE11[ which(WCE11$chainend == 'End of Qtr_DM'), ]

#Round 12:
#Goal_F
WCE12_G <- WCE12[ which(WCE12$chainend == 'Goal_F'), ]
#Behind_F
WCE12_B <- WCE12[ which(WCE12$chainend == 'Behind_F'), ]
#Stoppage_F
WCE12_SF <- WCE12[ which(WCE12$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE12_TF <- WCE12[ which(WCE12$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE12_SAM <- WCE12[ which(WCE12$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE12_TAM <- WCE12[ which(WCE12$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE12_SDM <- WCE12[ which(WCE12$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE12_TDM <- WCE12[ which(WCE12$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE12_SD <- WCE12[ which(WCE12$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE12_TD <- WCE12[ which(WCE12$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE12_QT <- WCE12[ which(WCE12$chainend == 'End of Qtr_DM'), ]

#Round 13:
#Goal_F
WCE13_G <- WCE13[ which(WCE13$chainend == 'Goal_F'), ]
#Behind_F
WCE13_B <- WCE13[ which(WCE13$chainend == 'Behind_F'), ]
#Stoppage_F
WCE13_SF <- WCE13[ which(WCE13$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE13_TF <- WCE13[ which(WCE13$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE13_SAM <- WCE13[ which(WCE13$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE13_TAM <- WCE13[ which(WCE13$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE13_SDM <- WCE13[ which(WCE13$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE13_TDM <- WCE13[ which(WCE13$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE13_SD <- WCE13[ which(WCE13$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE13_TD <- WCE13[ which(WCE13$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE13_QT <- WCE13[ which(WCE13$chainend == 'End of Qtr_DM'), ]

#Round 14:
#Goal_F
WCE14_G <- WCE14[ which(WCE14$chainend == 'Goal_F'), ]
#Behind_F
WCE14_B <- WCE14[ which(WCE14$chainend == 'Behind_F'), ]
#Stoppage_F
WCE14_SF <- WCE14[ which(WCE14$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE14_TF <- WCE14[ which(WCE14$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE14_SAM <- WCE14[ which(WCE14$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE14_TAM <- WCE14[ which(WCE14$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE14_SDM <- WCE14[ which(WCE14$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE14_TDM <- WCE14[ which(WCE14$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE14_SD <- WCE14[ which(WCE14$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE14_TD <- WCE14[ which(WCE14$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE14_QT <- WCE14[ which(WCE14$chainend == 'End of Qtr_DM'), ]

#Round 15:
#Goal_F
WCE15_G <- WCE15[ which(WCE15$chainend == 'Goal_F'), ]
#Behind_F
WCE15_B <- WCE15[ which(WCE15$chainend == 'Behind_F'), ]
#Stoppage_F
WCE15_SF <- WCE15[ which(WCE15$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE15_TF <- WCE15[ which(WCE15$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE15_SAM <- WCE15[ which(WCE15$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE15_TAM <- WCE15[ which(WCE15$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE15_SDM <- WCE15[ which(WCE15$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE15_TDM <- WCE15[ which(WCE15$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE15_SD <- WCE15[ which(WCE15$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE15_TD <- WCE15[ which(WCE15$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE15_QT <- WCE15[ which(WCE15$chainend == 'End of Qtr_DM'), ]

#Round 16:
#Goal_F
WCE16_G <- WCE16[ which(WCE16$chainend == 'Goal_F'), ]
#Behind_F
WCE16_B <- WCE16[ which(WCE16$chainend == 'Behind_F'), ]
#Stoppage_F
WCE16_SF <- WCE16[ which(WCE16$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE16_TF <- WCE16[ which(WCE16$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE16_SAM <- WCE16[ which(WCE16$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE16_TAM <- WCE16[ which(WCE16$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE16_SDM <- WCE16[ which(WCE16$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE16_TDM <- WCE16[ which(WCE16$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE16_SD <- WCE16[ which(WCE16$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE16_TD <- WCE16[ which(WCE16$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE16_QT <- WCE16[ which(WCE16$chainend == 'End of Qtr_DM'), ]

#Round 17:
#Goal_F
WCE17_G <- WCE17[ which(WCE17$chainend == 'Goal_F'), ]
#Behind_F
WCE17_B <- WCE17[ which(WCE17$chainend == 'Behind_F'), ]
#Stoppage_F
WCE17_SF <- WCE17[ which(WCE17$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE17_TF <- WCE17[ which(WCE17$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE17_SAM <- WCE17[ which(WCE17$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE17_TAM <- WCE17[ which(WCE17$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE17_SDM <- WCE17[ which(WCE17$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE17_TDM <- WCE17[ which(WCE17$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE17_SD <- WCE17[ which(WCE17$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE17_TD <- WCE17[ which(WCE17$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE17_QT <- WCE17[ which(WCE17$chainend == 'End of Qtr_DM'), ]

#Round 18:
#Goal_F
WCE18_G <- WCE18[ which(WCE18$chainend == 'Goal_F'), ]
#Behind_F
WCE18_B <- WCE18[ which(WCE18$chainend == 'Behind_F'), ]
#Stoppage_F
WCE18_SF <- WCE18[ which(WCE18$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE18_TF <- WCE18[ which(WCE18$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE18_SAM <- WCE18[ which(WCE18$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE18_TAM <- WCE18[ which(WCE18$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE18_SDM <- WCE18[ which(WCE18$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE18_TDM <- WCE18[ which(WCE18$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE18_SD <- WCE18[ which(WCE18$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE18_TD <- WCE18[ which(WCE18$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE18_QT <- WCE18[ which(WCE18$chainend == 'End of Qtr_DM'), ]

#Round 19:
#Goal_F
WCE19_G <- WCE19[ which(WCE19$chainend == 'Goal_F'), ]
#Behind_F
WCE19_B <- WCE19[ which(WCE19$chainend == 'Behind_F'), ]
#Stoppage_F
WCE19_SF <- WCE19[ which(WCE19$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE19_TF <- WCE19[ which(WCE19$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE19_SAM <- WCE19[ which(WCE19$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE19_TAM <- WCE19[ which(WCE19$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE19_SDM <- WCE19[ which(WCE19$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE19_TDM <- WCE19[ which(WCE19$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE19_SD <- WCE19[ which(WCE19$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE19_TD <- WCE19[ which(WCE19$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE19_QT <- WCE19[ which(WCE19$chainend == 'End of Qtr_DM'), ]

#Round 20:
#Goal_F
WCE20_G <- WCE20[ which(WCE20$chainend == 'Goal_F'), ]
#Behind_F
WCE20_B <- WCE20[ which(WCE20$chainend == 'Behind_F'), ]
#Stoppage_F
WCE20_SF <- WCE20[ which(WCE20$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE20_TF <- WCE20[ which(WCE20$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE20_SAM <- WCE20[ which(WCE20$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE20_TAM <- WCE20[ which(WCE20$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE20_SDM <- WCE20[ which(WCE20$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE20_TDM <- WCE20[ which(WCE20$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE20_SD <- WCE20[ which(WCE20$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE20_TD <- WCE20[ which(WCE20$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE20_QT <- WCE20[ which(WCE20$chainend == 'End of Qtr_DM'), ]

#Round 21:
#Goal_F
WCE21_G <- WCE21[ which(WCE21$chainend == 'Goal_F'), ]
#Behind_F
WCE21_B <- WCE21[ which(WCE21$chainend == 'Behind_F'), ]
#Stoppage_F
WCE21_SF <- WCE21[ which(WCE21$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE21_TF <- WCE21[ which(WCE21$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE21_SAM <- WCE21[ which(WCE21$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE21_TAM <- WCE21[ which(WCE21$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE21_SDM <- WCE21[ which(WCE21$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE21_TDM <- WCE21[ which(WCE21$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE21_SD <- WCE21[ which(WCE21$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE21_TD <- WCE21[ which(WCE21$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE21_QT <- WCE21[ which(WCE21$chainend == 'End of Qtr_DM'), ]

#Round 22:
#Goal_F
WCE22_G <- WCE22[ which(WCE22$chainend == 'Goal_F'), ]
#Behind_F
WCE22_B <- WCE22[ which(WCE22$chainend == 'Behind_F'), ]
#Stoppage_F
WCE22_SF <- WCE22[ which(WCE22$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE22_TF <- WCE22[ which(WCE22$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE22_SAM <- WCE22[ which(WCE22$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE22_TAM <- WCE22[ which(WCE22$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE22_SDM <- WCE22[ which(WCE22$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE22_TDM <- WCE22[ which(WCE22$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE22_SD <- WCE22[ which(WCE22$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE22_TD <- WCE22[ which(WCE22$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE22_QT <- WCE22[ which(WCE22$chainend == 'End of Qtr_DM'), ]

#Round 23:
#Goal_F
WCE23_G <- WCE23[ which(WCE23$chainend == 'Goal_F'), ]
#Behind_F
WCE23_B <- WCE23[ which(WCE23$chainend == 'Behind_F'), ]
#Stoppage_F
WCE23_SF <- WCE23[ which(WCE23$chainend == 'Stoppage_F'), ]
#Turnover_F
WCE23_TF <- WCE23[ which(WCE23$chainend == 'Turnover_F'), ]
#Stoppage_AM
WCE23_SAM <- WCE23[ which(WCE23$chainend == 'Stoppage_AM'), ]
#Turnover_AM
WCE23_TAM <- WCE23[ which(WCE23$chainend == 'Turnover_AM'), ]
#Stoppage_DM
WCE23_SDM <- WCE23[ which(WCE23$chainend == 'Stoppage_DM'), ]
#Turnover_DM
WCE23_TDM <- WCE23[ which(WCE23$chainend == 'Turnover_DM'), ]
#Stoppage_D
WCE23_SD <- WCE23[ which(WCE23$chainend == 'Stoppage_D'), ]
#Turnover_D
WCE23_TD <- WCE23[ which(WCE23$chainend == 'Turnover_D'), ]
#End of Qtr_DM
WCE23_QT <- WCE23[ which(WCE23$chainend == 'End of Qtr_DM'), ]
