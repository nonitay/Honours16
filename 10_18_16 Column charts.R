########################################################################################

# Plots

########################################################################################

# Install and load packages
library(ggplot2)
library(plyr)
library(grid)

# For reference
# ggplot2 manual: http://docs.ggplot2.org/current/
# ggplot2 Quick-R tutorial: http://www.statmethods.net/advgraphs/ggplot2.html
# ggplot2 cheat sheet from RStudio: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
# ggplot2 Harvard tutorial: http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
# ggplot2 R Graph Gallery: http://www.r-graph-gallery.com/portfolio/ggplot2-package/
# ggnet2 network visualization with ggplot2 : https://briatte.github.io/ggnet/
# plotly for network graphs: https://plot.ly/r/network-graphs/
# Static and dynamic network visualization in R: https://rpubs.com/kateto/netviz

########################################################################################

# Data wrangling
# Calculate descriptive statistics using NADF data frame (with RushedOpp_D dropped)
# Done

########################################################################################

##############################

# Separate data frames to capture mean and SD
# for each network metric, categorised by kickInOutcome

##############################

NADF_kickInOutcome_clusterCoef <- ddply(NADF, c("kickInOutcome"), summarise,
                                        mean = mean(clusterCoef, na.rm = T),
                                        sd = sd(clusterCoef, na.rm = T)
)
NADF_kickInOutcome_clusterCoef$netMet <- "Cluster coefficient"
NADF_kickInOutcome_clusterCoef <- NADF_kickInOutcome_clusterCoef[c(2,1,6,10,3,7,5,9,4,8),c(4,1:3)]

NADF_kickInOutcome_degreeCent <- ddply(NADF, c("kickInOutcome"), summarise,
                                       mean = mean(degreeCent, na.rm = T),
                                       sd = sd(degreeCent, na.rm = T)
)
NADF_kickInOutcome_degreeCent$netMet <- "Degree centrality"
NADF_kickInOutcome_degreeCent <- NADF_kickInOutcome_degreeCent[c(2,1,6,10,3,7,5,9,4,8),c(4,1:3)]

NADF_kickInOutcome_netDensity <- ddply(NADF, c("kickInOutcome"), summarise,
                                       mean = mean(netDensity, na.rm = T),
                                       sd = sd(netDensity, na.rm = T)
)
NADF_kickInOutcome_netDensity$netMet <- "Network density"
NADF_kickInOutcome_netDensity <- NADF_kickInOutcome_netDensity[c(2,1,6,10,3,7,5,9,4,8),c(4,1:3)]

NADF_kickInOutcome_entropy <- ddply(NADF, c("kickInOutcome"), summarise,
                                    mean = mean(entropy, na.rm = T),
                                    sd = sd(entropy, na.rm = T)
)
NADF_kickInOutcome_entropy$netMet <- "Entropy"
NADF_kickInOutcome_entropy <- NADF_kickInOutcome_entropy[c(2,1,6,10,3,7,5,9,4,8),c(4,1:3)]

# Set data classes and order of levels
NADF_kickInOutcome_clusterCoef$netMet <- as.factor(as.character(NADF_kickInOutcome_clusterCoef$netMet))
NADF_kickInOutcome_clusterCoef$kickInOutcome <- as.factor(as.character(NADF_kickInOutcome_clusterCoef$kickInOutcome))
NADF_kickInOutcome_clusterCoef$kickInOutcome <- ordered(NADF_kickInOutcome_clusterCoef$kickInOutcome,
                                                        levels = c("Goal_F", "Behind_F", "Stoppage_F", "Turnover_F", 
                                                                   "Stoppage_AM", "Turnover_AM", "Stoppage_DM", 
                                                                   "Turnover_DM", "Stoppage_D", "Turnover_D"))

NADF_kickInOutcome_degreeCent$netMet <- as.factor(as.character(NADF_kickInOutcome_degreeCent$netMet))
NADF_kickInOutcome_degreeCent$kickInOutcome <- as.factor(as.character(NADF_kickInOutcome_degreeCent$kickInOutcome))
NADF_kickInOutcome_degreeCent$kickInOutcome <- ordered(NADF_kickInOutcome_degreeCent$kickInOutcome,
                                                       levels = c("Goal_F", "Behind_F", "Stoppage_F", "Turnover_F", 
                                                                  "Stoppage_AM", "Turnover_AM", "Stoppage_DM", 
                                                                  "Turnover_DM", "Stoppage_D", "Turnover_D"))

NADF_kickInOutcome_netDensity$netMet <- as.factor(as.character(NADF_kickInOutcome_netDensity$netMet))
NADF_kickInOutcome_netDensity$kickInOutcome <- as.factor(as.character(NADF_kickInOutcome_netDensity$kickInOutcome))
NADF_kickInOutcome_netDensity$kickInOutcome <- ordered(NADF_kickInOutcome_netDensity$kickInOutcome,
                                                       levels = c("Goal_F", "Behind_F", "Stoppage_F", "Turnover_F", 
                                                                  "Stoppage_AM", "Turnover_AM", "Stoppage_DM", 
                                                                  "Turnover_DM", "Stoppage_D", "Turnover_D"))

NADF_kickInOutcome_entropy$netMet <- as.factor(as.character(NADF_kickInOutcome_entropy$netMet))
NADF_kickInOutcome_entropy$kickInOutcome <- as.factor(as.character(NADF_kickInOutcome_entropy$kickInOutcome))
NADF_kickInOutcome_entropy$kickInOutcome <- ordered(NADF_kickInOutcome_entropy$kickInOutcome,
                                                       levels = c("Goal_F", "Behind_F", "Stoppage_F", "Turnover_F", 
                                                                  "Stoppage_AM", "Turnover_AM", "Stoppage_DM", 
                                                                  "Turnover_DM", "Stoppage_D", "Turnover_D"))

##############################

# Separate data frames to capture mean and SD
# for each network metric, categorised by bracket

##############################

NADF_bracket_clusterCoef <- ddply(NADF, c("bracket"), summarise,
                                  mean = mean(clusterCoef, na.rm = T),
                                  sd = sd(clusterCoef, na.rm = T)
)
NADF_bracket_clusterCoef$netMet <- "Cluster coefficient"
NADF_bracket_clusterCoef <- NADF_bracket_clusterCoef[,c(4,1:3)]

NADF_bracket_degreeCent <- ddply(NADF, c("bracket"), summarise,
                                 mean = mean(degreeCent, na.rm = T),
                                 sd = sd(degreeCent, na.rm = T)
)
NADF_bracket_degreeCent$netMet <- "Degree centrality"
NADF_bracket_degreeCent <- NADF_bracket_degreeCent[,c(4,1:3)]

NADF_bracket_netDensity <- ddply(NADF, c("bracket"), summarise,
                                 mean = mean(netDensity, na.rm = T),
                                 sd = sd(netDensity, na.rm = T)
)
NADF_bracket_netDensity$netMet <- "Network density"
NADF_bracket_netDensity <- NADF_bracket_netDensity[,c(4,1:3)]

NADF_bracket_entropy <- ddply(NADF, c("bracket"), summarise,
                              mean = mean(entropy, na.rm = T),
                              sd = sd(entropy, na.rm = T)
)
NADF_bracket_entropy$netMet <- "Entropy"
NADF_bracket_entropy <- NADF_bracket_entropy[,c(4,1:3)]

# Set data classes and ordering of levels within bracket
NADF_bracket_clusterCoef$netMet <- as.factor(as.character(NADF_bracket_clusterCoef$netMet))
NADF_bracket_clusterCoef$bracket <- as.factor(as.character(NADF_bracket_clusterCoef$bracket))
NADF_bracket_clusterCoef$bracket <- ordered(NADF_bracket_clusterCoef$bracket,
                                            levels = c("Top 6", "Middle 6", "Bottom 6"))

NADF_bracket_degreeCent$netMet <- as.factor(as.character(NADF_bracket_degreeCent$netMet))
NADF_bracket_degreeCent$bracket <- as.factor(as.character(NADF_bracket_degreeCent$bracket))
NADF_bracket_degreeCent$bracket <- ordered(NADF_bracket_degreeCent$bracket,
                                           levels = c("Top 6", "Middle 6", "Bottom 6"))

NADF_bracket_netDensity$netMet <- as.factor(as.character(NADF_bracket_netDensity$netMet))
NADF_bracket_netDensity$bracket <- as.factor(as.character(NADF_bracket_netDensity$bracket))
NADF_bracket_netDensity$bracket <- ordered(NADF_bracket_netDensity$bracket,
                                           levels = c("Top 6", "Middle 6", "Bottom 6"))

NADF_bracket_entropy$netMet <- as.factor(as.character(NADF_bracket_entropy$netMet))
NADF_bracket_entropy$bracket <- as.factor(as.character(NADF_bracket_entropy$bracket))
NADF_bracket_entropy$bracket <- ordered(NADF_bracket_entropy$bracket,
                                        levels = c("Top 6", "Middle 6", "Bottom 6"))

##############################

# Separate data frames to capture mean and SD
# for each network metric, categorised by matchOutcome

##############################

NADF_matchOutcome_clusterCoef <- ddply(NADF, c("matchOutcome"), summarise,
                                       mean = mean(clusterCoef, na.rm = T),
                                       sd = sd(clusterCoef, na.rm = T)
)
NADF_matchOutcome_clusterCoef$netMet <- "Cluster coefficient"
NADF_matchOutcome_clusterCoef <- NADF_matchOutcome_clusterCoef[,c(4,1:3)]

NADF_matchOutcome_degreeCent <- ddply(NADF, c("matchOutcome"), summarise,
                                      mean = mean(degreeCent, na.rm = T),
                                      sd = sd(degreeCent, na.rm = T)
)
NADF_matchOutcome_degreeCent$netMet <- "Degree centrality"
NADF_matchOutcome_degreeCent <- NADF_matchOutcome_degreeCent[,c(4,1:3)]

NADF_matchOutcome_netDensity <- ddply(NADF, c("matchOutcome"), summarise,
                                      mean = mean(netDensity, na.rm = T),
                                      sd = sd(netDensity, na.rm = T)
)
NADF_matchOutcome_netDensity$netMet <- "Network density"
NADF_matchOutcome_netDensity <- NADF_matchOutcome_netDensity[,c(4,1:3)]

NADF_matchOutcome_entropy <- ddply(NADF, c("matchOutcome"), summarise,
                                   mean = mean(entropy, na.rm = T),
                                   sd = sd(entropy, na.rm = T)
)
NADF_matchOutcome_entropy$netMet <- "Entropy"
NADF_matchOutcome_entropy <- NADF_matchOutcome_entropy[,c(4,1:3)]

# Set data classes and ordering of levels within matchOutcome
NADF_matchOutcome_clusterCoef$netMet <- as.factor(as.character(NADF_matchOutcome_clusterCoef$netMet))
NADF_matchOutcome_clusterCoef$matchOutcome <- as.factor(as.character(NADF_matchOutcome_clusterCoef$matchOutcome))
NADF_matchOutcome_clusterCoef$matchOutcome <- ordered(NADF_matchOutcome_clusterCoef$matchOutcome,
                                                      levels = c("Win", "Loss", "Draw"))

NADF_matchOutcome_degreeCent$netMet <- as.factor(as.character(NADF_matchOutcome_degreeCent$netMet))
NADF_matchOutcome_degreeCent$matchOutcome <- as.factor(as.character(NADF_matchOutcome_degreeCent$matchOutcome))
NADF_matchOutcome_degreeCent$matchOutcome <- ordered(NADF_matchOutcome_degreeCent$matchOutcome,
                                                     levels = c("Win", "Loss", "Draw"))

NADF_matchOutcome_netDensity$netMet <- as.factor(as.character(NADF_matchOutcome_netDensity$netMet))
NADF_matchOutcome_netDensity$matchOutcome <- as.factor(as.character(NADF_matchOutcome_netDensity$matchOutcome))
NADF_matchOutcome_netDensity$matchOutcome <- ordered(NADF_matchOutcome_netDensity$matchOutcome,
                                                     levels = c("Win", "Loss", "Draw"))

NADF_matchOutcome_entropy$netMet <- as.factor(as.character(NADF_matchOutcome_entropy$netMet))
NADF_matchOutcome_entropy$matchOutcome <- as.factor(as.character(NADF_matchOutcome_entropy$matchOutcome))
NADF_matchOutcome_entropy$matchOutcome <- ordered(NADF_matchOutcome_entropy$matchOutcome,
                                                  levels = c("Win", "Loss", "Draw"))

########################################################################################

# Plotting using ggplot2: by kickInOutcome
# Column charts

########################################################################################

##############################

# clusterCoef

##############################

p <- ggplot(NADF_kickInOutcome_clusterCoef, aes(kickInOutcome, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#56B4E9")
p <- p + scale_y_continuous(limits = c(0,0.30), breaks = c(0, 0.06, 0.12, 0.18, 0.24, 0.30))
p <- p + ggtitle("Cluster coefficient")
p <- p + labs(x = "TEMP", y = "Cluster coefficient (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.title.x = element_text(size = 22, colour = "#FFFFFF", margin = margin(20,0,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.line.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_kickInOutcome_clusterCoef <- p

# Save individual plot to PNG
png("plot_kickInOutcome_clusterCoef.png", width = 600, height = 450)
plot_kickInOutcome_clusterCoef
dev.off()

##############################

# degreeCent

##############################

p <- ggplot(NADF_kickInOutcome_degreeCent, aes(kickInOutcome, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#D55E00")
p <- p + scale_y_continuous(limits = c(0,0.25), breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25))
p <- p + ggtitle("Degree centrality")
p <- p + labs(x = "TEMP", y = "Degree centrality (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.title.x = element_text(size = 22, colour = "#FFFFFF", margin = margin(20,0,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.line.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_kickInOutcome_degreeCent <- p

# Save individual plot to PNG
png("plot_kickInOutcome_degreeCent.png", width = 600, height = 450)
plot_kickInOutcome_degreeCent
dev.off()

##############################

# netDensity

##############################

p <- ggplot(NADF_kickInOutcome_netDensity, aes(kickInOutcome, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#CC79A7")
p <- p + scale_y_continuous(limits = c(0,0.60), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
p <- p + ggtitle("Network density")
p <- p + labs(x = NULL, y = "Network density (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.ticks.x = element_line(colour = "#000000"),
               axis.text.x = element_text(size = 14, colour = "#000000", angle = 45, hjust = 1),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_kickInOutcome_netDensity <- p

# Save individual plot to PNG
png("plot_kickInOutcome_netDensity.png", width = 600, height = 450)
plot_kickInOutcome_netDensity
dev.off()

##############################

# entropy

##############################

p <- ggplot(NADF_kickInOutcome_entropy, aes(kickInOutcome, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#009E73")
p <- p + scale_y_continuous(limits = c(0,3.0), breaks = c(0, 0.6, 1.2, 1.8, 2.4, 3.0))
p <- p + ggtitle("Entropy")
p <- p + labs(x = NULL, y = "Entropy (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.ticks.x = element_line(colour = "#000000"),
               axis.text.x = element_text(size = 14, colour = "#000000", angle = 45, hjust = 1),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_kickInOutcome_entropy <- p

# Save individual plot to PNG
png("plot_kickInOutcome_entropy.png", width = 600, height = 450)
plot_kickInOutcome_entropy
dev.off()

##############################

# Combine into a four-panel plot
# Use grid.arrange() from the package 'grid'
library(gridExtra)
##############################

# Code to align axes
gA <- ggplotGrob(plot_kickInOutcome_clusterCoef)
gB <- ggplotGrob(plot_kickInOutcome_degreeCent)
gC <- ggplotGrob(plot_kickInOutcome_netDensity)
gD <- ggplotGrob(plot_kickInOutcome_entropy)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5], gD$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)
gD$widths[2:5] <- as.list(maxWidth)

# Save four-panel plot to PNG
png("plot_kickInOutcome_ALL.png", width = 1200, height = 800)
grid.arrange(gA, gB, gC, gD, ncol=2)
dev.off()

########################################################################################

# Plotting using ggplot2: by bracket
# Column charts

########################################################################################

##############################

# clusterCoef

##############################

p <- ggplot(NADF_bracket_clusterCoef, aes(bracket, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#56B4E9")
p <- p + scale_y_continuous(limits = c(0,0.30), breaks = c(0, 0.06, 0.12, 0.18, 0.24, 0.30))
p <- p + ggtitle("Cluster coefficient")
p <- p + labs(x = "TEMP", y = "Cluster coefficient (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.title.x = element_text(size = 22, colour = "#FFFFFF", margin = margin(20,0,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.line.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_bracket_clusterCoef <- p

# Save individual plot to PNG
png("plot_bracket_clusterCoef.png", width = 600, height = 450)
plot_bracket_clusterCoef
dev.off()

##############################

# degreeCent

##############################

p <- ggplot(NADF_bracket_degreeCent, aes(bracket, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#D55E00")
p <- p + scale_y_continuous(limits = c(0,0.25), breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25))
p <- p + ggtitle("Degree centrality")
p <- p + labs(x = "TEMP", y = "Degree centrality (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.title.x = element_text(size = 22, colour = "#FFFFFF", margin = margin(20,0,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.line.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_bracket_degreeCent <- p

# Save individual plot to PNG
png("plot_bracket_degreeCent.png", width = 600, height = 450)
plot_bracket_degreeCent
dev.off()

##############################

# netDensity

##############################

p <- ggplot(NADF_bracket_netDensity, aes(bracket, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#CC79A7")
p <- p + scale_y_continuous(limits = c(0,0.40), breaks = c(0, 0.1, 0.2, 0.3, 0.4))
p <- p + ggtitle("Network density")
p <- p + labs(x = NULL, y = "Network density (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.ticks.x = element_line(colour = "#000000"),
               axis.text.x = element_text(size = 22, colour = "#000000"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_bracket_netDensity <- p

# Save individual plot to PNG
png("plot_bracket_netDensity.png", width = 600, height = 450)
plot_bracket_netDensity
dev.off()

##############################

# entropy

##############################

p <- ggplot(NADF_bracket_entropy, aes(bracket, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#009E73")
p <- p + scale_y_continuous(limits = c(0,3.0), breaks = c(0, 0.6, 1.2, 1.8, 2.4, 3.0))
p <- p + ggtitle("Entropy")
p <- p + labs(x = NULL, y = "Entropy (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.ticks.x = element_line(colour = "#000000"),
               axis.text.x = element_text(size = 22, colour = "#000000"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_bracket_entropy <- p

# Save individual plot to PNG
png("plot_bracket_entropy.png", width = 600, height = 450)
plot_bracket_entropy
dev.off()

##############################

# Combine into a four-panel plot
# Use grid.arrange() from the package 'grid'

##############################

# Code to align axes
gA <- ggplotGrob(plot_bracket_clusterCoef)
gB <- ggplotGrob(plot_bracket_degreeCent)
gC <- ggplotGrob(plot_bracket_netDensity)
gD <- ggplotGrob(plot_bracket_entropy)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5], gD$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)
gD$widths[2:5] <- as.list(maxWidth)

# Save four-panel plot to PNG
png("plot_bracket_ALL.png", width = 1200, height = 800)
grid.arrange(gA, gB, gC, gD, ncol=2)
dev.off()

########################################################################################

# Plotting using ggplot2: by matchOutcome
# Column charts

########################################################################################

##############################

# clusterCoef

##############################

p <- ggplot(NADF_matchOutcome_clusterCoef, aes(matchOutcome, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#56B4E9")
p <- p + scale_y_continuous(limits = c(0,0.25), breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25))
p <- p + ggtitle("Cluster coefficient")
p <- p + labs(x = "TEMP", y = "Cluster coefficient (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.title.x = element_text(size = 22, colour = "#FFFFFF", margin = margin(20,0,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.line.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_matchOutcome_clusterCoef <- p

# Save individual plot to PNG
png("plot_matchOutcome_clusterCoef.png", width = 600, height = 450)
plot_matchOutcome_clusterCoef
dev.off()

##############################

# degreeCent

##############################

p <- ggplot(NADF_matchOutcome_degreeCent, aes(matchOutcome, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#D55E00")
p <- p + scale_y_continuous(limits = c(0,0.25), breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25))
p <- p + ggtitle("Degree centrality")
p <- p + labs(x = "TEMP", y = "Degree centrality (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.title.x = element_text(size = 22, colour = "#FFFFFF", margin = margin(20,0,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.line.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_matchOutcome_degreeCent <- p

# Save individual plot to PNG
png("plot_matchOutcome_degreeCent.png", width = 600, height = 450)
plot_matchOutcome_degreeCent
dev.off()

##############################

# netDensity

##############################

p <- ggplot(NADF_matchOutcome_netDensity, aes(matchOutcome, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#CC79A7")
p <- p + scale_y_continuous(limits = c(0,0.40), breaks = c(0, 0.1, 0.2, 0.3, 0.4))
p <- p + ggtitle("Network density")
p <- p + labs(x = NULL, y = "Network density (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.ticks.x = element_line(colour = "#000000"),
               axis.text.x = element_text(size = 22, colour = "#000000"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_matchOutcome_netDensity <- p

# Save individual plot to PNG
png("plot_matchOutcome_netDensity.png", width = 600, height = 450)
plot_matchOutcome_netDensity
dev.off()

##############################

# entropy

##############################

p <- ggplot(NADF_matchOutcome_entropy, aes(matchOutcome, mean))
p <- p + geom_errorbar(aes(ymax = mean + sd, ymin = mean - 0.05), colour = "#999999", linetype = 1, width = 0.4)
p <- p + geom_bar(stat = "identity", fill = "#009E73")
p <- p + scale_y_continuous(limits = c(0,3.0), breaks = c(0, 0.6, 1.2, 1.8, 2.4, 3.0))
p <- p + ggtitle("Entropy")
p <- p + labs(x = NULL, y = "Entropy (AU)")
p <- p + theme(plot.title = element_text(size = 30, face = "bold"),
               axis.title.y = element_text(size = 22, margin=margin(0,20,0,0)),
               axis.text.y = element_text(size = 22, colour = "#000000"),
               axis.line = element_line(colour = "#000000"),
               axis.ticks.x = element_line(colour = "#000000"),
               axis.text.x = element_text(size = 22, colour = "#000000"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
plot_matchOutcome_entropy <- p

# Save individual plot to PNG
png("plot_matchOutcome_entropy.png", width = 600, height = 450)
plot_matchOutcome_entropy
dev.off()

##############################

# Combine into a four-panel plot
# Use grid.arrange() from the package 'grid'

##############################

# Code to align axes
gA <- ggplotGrob(plot_matchOutcome_clusterCoef)
gB <- ggplotGrob(plot_matchOutcome_degreeCent)
gC <- ggplotGrob(plot_matchOutcome_netDensity)
gD <- ggplotGrob(plot_matchOutcome_entropy)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5], gD$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)
gD$widths[2:5] <- as.list(maxWidth)

# Save four-panel plot to PNG
png("plot_matchOutcome_ALL.png", width = 1200, height = 800)
grid.arrange(gA, gB, gC, gD, ncol=2)
dev.off()