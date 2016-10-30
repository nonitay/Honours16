####
#MOCK DATA SCRIPTS
###

##
# 1.Setup
##

#Install and Load Packages
install.packages("readxl")
library(readxl)


#Read data into R
md <- read_excel("2015 chain data_modified.xlsx", sheet=1, col_names=TRUE, col_types = NULL, na= "")
#View(md)

#Inital check of data strucute
length(md$Round)
str(md) #structure
class(md$Match) #data class
class(md$Round)
head(md) #first part of data 

#Check data values
min(md$Round)
max(md$Round)
mean(md$Round)
sd(md$Distance)
median(md$Disp.X.End)

##
# 2.Data Cleaning
##

#Clean data

#Specify data class for all variables
md$Round <- as.numeric(md$Round)
md$Match <- as.factor(md$Match)
md$Opposition <- as.factor(md$Opposition)
md$Chain <- as.numeric(md$Chain)
md$Team <- as.factor(md$Team)
md$Chain.Start.Type <- as.factor(md$Chain.Start.Type)
md$Chain.Start.Zone <- as.factor(md$Chain.Start.Zone)
md$Chain.End.Type <- as.factor(md$Chain.End.Type)
md$Chain.End.Zone <- as.factor(md$Chain.End.Zone)
md$Player <- as.factor(md$Player)
md$Disposal <- as.factor(md$Disposal)
md$Disp.X.Start <- as.numeric(md$Disp.X.Start)
md$Disp.Y.Start <- as.numeric(md$Disp.Y.Start)
md$Disp.XY.Start.Zone <- as.factor(md$Disp.XY.Start.Zone)
md$Disp.X.End <- as.numeric(md$Disp.X.End)
md$Disp.Y.End <- as.numeric(md$Disp.Y.End)
md$Disp.XY.End.Zone <- as.factor(md$Disp.XY.End.Zone)
md$Kick.Inside50.launch <- as.factor(md$Kick.Inside50.launch)
md$Kick.Inside50.entry <- as.factor(md$Kick.Inside50.entry)
md$Metres.Gained <- as.numeric(md$Metres.Gained)
md$Quarter <- as.factor(md$Quarter)
md$Period.Seconds <- as.numeric(md$Period.Seconds)
md$Disposal.Seconds <- as.numeric(md$Disposal.Seconds)
md$Score <- as.factor(md$Score)
md$Games <- as.numeric(md$Games)
md$Disp.X.Start.Adj..L.to.R.<- as.numeric(md$Disp.X.Start.Adj..L.to.R.)
md$Disp.Y.Start.Adj..L.to.R.<- as.numeric(md$Disp.Y.Start.Adj..L.to.R.)
md$Disp.Y.Start.Adj..L.to.R..Kicks <- as.numeric(md$Disp.Y.Start.Adj..L.to.R..Kicks)
md$Disp.Y.Start.Adj..L.to.R..Handballs <- as.numeric(md$Disp.Y.Start.Adj..L.to.R..Handballs)
md$Left..L...Corridor..CL..C..CR..or.Right..R. <- as.factor(md$Left..L...Corridor..CL..C..CR..or.Right..R.)
md$Zone <- as.factor(md$Zone)
md$Angle..Degrees.<- as.numeric(md$Angle..Degrees.)
md$Distance <- as.numeric(md$Distance)

# Revalue game IDs to dates in mdclean
mdclean <-md
View(mdclean)
View(mdclean)

#Reorganise columns
mdna <- mdclean[,c(1:12)]
View(mdna)

##
# 3.Assumption Testing
##

#NORMALITY (Q-Q Plots)
png(filename="Disp.X.End.png_qq", width = 800, height=600)
qqnorm(md$Disp.X.End, main ="Q-Q:Disp.X.End")
qqline(md$Disp.X.End)
dev.off()

#LINEARITY

#HOMOSCEDASTICITY 

##
# 4.Network Analysis 
##

#Install and Load Packages
install.packages("igraph")
library(igraph)

#Modify data to only include NA information 

#Create column for recieveing player

#Filter data to only include possession chains

#Create a matrix for each team 

#Add weight to matrix interactions


