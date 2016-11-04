library(fitdistrplus)
library(pastecs)
library(car)

################################################################################

# Normality: Plots, Shapiro-Wilk, skewness and kurtosis

################################################################################

########################################

# Cluster coefficient

########################################

# Plot distribution checks
#clusterCoef_fitNorm <- fitdist(NADF$clusterCoef, "norm")

#png(filename = "clusterCoef_fitNorm.png", width = 800, height = 600)
#plot(clusterCoef_fitNorm)
#dev.off()

#Distribution plots cannot be attained due to the large number of zeros in clusterCoef
png(filename = "clusterCoef_qq.png", width= 800, height= 600)
qqnorm(NADF$clusterCoef, main= "q-q:clusterCoef")
qqline(NADF$clusterCoef)
dev.off()

# Shapiro-Wilk test
sink(file = "clusterCoef_shapiro.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("clusterCoef\n\n")
cat("=========================\n")
shapiro.test(NADF$clusterCoef)
sink()

# Check skewness and kurtosis (only if the variable fails the Shapiro-Wilk test)
clusterCoef_skewKurt <- as.data.frame(stat.desc(NADF$clusterCoef, norm = TRUE, p = 0.95))
clusterCoef_skewKurt <- as.data.frame(t(clusterCoef_skewKurt))
rownames(clusterCoef_skewKurt) <- NULL
clusterCoef_skewKurt <- clusterCoef_skewKurt[,c(15:18)]
clusterCoef_skewKurt$skew_z <- clusterCoef_skewKurt$skewness / (clusterCoef_skewKurt$skew.2SE / 2)
clusterCoef_skewKurt$kurt_z <- clusterCoef_skewKurt$kurtosis / (clusterCoef_skewKurt$kurt.2SE / 2)
clusterCoef_skewKurt <- clusterCoef_skewKurt[,c(1,2,5,3,4,6)]
write.csv(clusterCoef_skewKurt, "clusterCoef_skewKurt.csv", row.names = FALSE)

########################################

# Degree centrality

########################################

# Plot distribution checks
degreeCent_fitNorm <- fitdist(NADF$degreeCent, "norm")

png(filename = "degreeCent_fitNorm.png", width = 800, height = 600)
plot(degreeCent_fitNorm)
dev.off()

# Shapiro-Wilk test
sink(file = "degreeCent_shapiro.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("degreeCent\n\n")
cat("=========================\n")
shapiro.test(NADF$degreeCent)
sink()

# Check skewness and kurtosis (only if the variable fails the Shapiro-Wilk test)
degreeCent_skewKurt <- as.data.frame(stat.desc(NADF$degreeCent, norm = TRUE, p = 0.95))
degreeCent_skewKurt <- as.data.frame(t(degreeCent_skewKurt))
rownames(degreeCent_skewKurt) <- NULL
degreeCent_skewKurt <- degreeCent_skewKurt[,c(15:18)]
degreeCent_skewKurt$skew_z <- degreeCent_skewKurt$skewness / (degreeCent_skewKurt$skew.2SE / 2)
degreeCent_skewKurt$kurt_z <- degreeCent_skewKurt$kurtosis / (degreeCent_skewKurt$kurt.2SE / 2)
degreeCent_skewKurt <- degreeCent_skewKurt[,c(1,2,5,3,4,6)]
write.csv(degreeCent_skewKurt, "degreeCent_skewKurt.csv", row.names = FALSE)

########################################

# Network density

########################################

# Plot distribution checks
netDensity_fitNorm <- fitdist(NADF$netDensity, "norm")

png(filename = "netDensity_fitNorm.png", width = 800, height = 600)
plot(netDensity_fitNorm)
dev.off()

# Shapiro-Wilk test
sink(file = "netDensity_shapiro.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("netDensity\n\n")
cat("=========================\n")
shapiro.test(NADF$netDensity)
sink()

# Check skewness and kurtosis (only if the variable fails the Shapiro-Wilk test)
netDensity_skewKurt <- as.data.frame(stat.desc(NADF$netDensity, norm = TRUE, p = 0.95))
netDensity_skewKurt <- as.data.frame(t(netDensity_skewKurt))
rownames(netDensity_skewKurt) <- NULL
netDensity_skewKurt <- netDensity_skewKurt[,c(15:18)]
netDensity_skewKurt$skew_z <- netDensity_skewKurt$skewness / (netDensity_skewKurt$skew.2SE / 2)
netDensity_skewKurt$kurt_z <- netDensity_skewKurt$kurtosis / (netDensity_skewKurt$kurt.2SE / 2)
netDensity_skewKurt <- netDensity_skewKurt[,c(1,2,5,3,4,6)]
write.csv(netDensity_skewKurt, "netDensity_skewKurt.csv", row.names = FALSE)

########################################

# Entropy

########################################

# Plot distribution checks
entropy_fitNorm <- fitdist(NADF$entropy, "norm")

png(filename = "entropy_fitNorm.png", width = 800, height = 600)
plot(entropy_fitNorm)
dev.off()

# Shapiro-Wilk test
sink(file = "entropy_shapiro.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("entropy\n\n")
cat("=========================\n")
shapiro.test(NADF$entropy)
sink()

# Check skewness and kurtosis (only if the variable fails the Shapiro-Wilk test)
entropy_skewKurt <- as.data.frame(stat.desc(NADF$entropy, norm = TRUE, p = 0.95))
entropy_skewKurt <- as.data.frame(t(entropy_skewKurt))
rownames(entropy_skewKurt) <- NULL
entropy_skewKurt <- entropy_skewKurt[,c(15:18)]
entropy_skewKurt$skew_z <- entropy_skewKurt$skewness / (entropy_skewKurt$skew.2SE / 2)
entropy_skewKurt$kurt_z <- entropy_skewKurt$kurtosis / (entropy_skewKurt$kurt.2SE / 2)
entropy_skewKurt <- entropy_skewKurt[,c(1,2,5,3,4,6)]
write.csv(entropy_skewKurt, "entropy_skewKurt.csv", row.names = FALSE)

################################################################################

# Linearity and homoscedasticity: Box and whiskers plots, Levene's test

################################################################################

########################################

# Cluster coefficient

########################################

# clusterCoef ~ kickInOutcome
# Box and whiskers
png(filename = "clusterCoef-kickInOutcome_boxPlots.png", width = 800, height = 600)
plot(NADF$clusterCoef ~ NADF$kickInOutcome, data = NADF)
dev.off()

# Levene's test
sink(file = "clusterCoef-kickInOutcome_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("clusterCoef-kickInOutcome\n\n")
cat("=========================\n\n")
leveneTest(NADF$clusterCoef ~ NADF$kickInOutcome, data = NADF)
sink()

# clusterCoef ~ matchOutcome
# Box and whiskers
png(filename = "clusterCoef-matchOutcome_boxPlots.png", width = 800, height = 600)
plot(NADF$clusterCoef ~ NADF$matchOutcome, data = NADF)
dev.off()

# Levene's test
sink(file = "clusterCoef-matchOutcome_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("clusterCoef-matchOutcome\n\n")
cat("=========================\n\n")
leveneTest(NADF$clusterCoef ~ NADF$matchOutcome, data = NADF)
sink()

# clusterCoef ~ bracket
# Box and whiskers
png(filename = "clusterCoef-bracket_boxPlots.png", width = 800, height = 600)
plot(NADF$clusterCoef ~ NADF$bracket, data = NADF)
dev.off()

# Levene's test
sink(file = "clusterCoef-bracket_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("clusterCoef-bracket\n\n")
cat("=========================\n\n")
leveneTest(NADF$clusterCoef ~ NADF$bracket, data = NADF)
sink()

########################################

# Degree centrality

########################################

# degreeCent ~ kickInOutcome
# Box and whiskers
png(filename = "degreeCent-kickInOutcome_boxPlots.png", width = 800, height = 600)
plot(NADF$degreeCent ~ NADF$kickInOutcome, data = NADF)
dev.off()

# Levene's test
sink(file = "degreeCent-kickInOutcome_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("degreeCent-kickInOutcome\n\n")
cat("=========================\n\n")
leveneTest(NADF$degreeCent ~ NADF$kickInOutcome, data = NADF)
sink()

# degreeCent ~ matchOutcome
# Box and whiskers
png(filename = "degreeCent-matchOutcome_boxPlots.png", width = 800, height = 600)
plot(NADF$degreeCent ~ NADF$matchOutcome, data = NADF)
dev.off()

# Levene's test
sink(file = "degreeCent-matchOutcome_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("degreeCent-matchOutcome\n\n")
cat("=========================\n\n")
leveneTest(NADF$degreeCent ~ NADF$matchOutcome, data = NADF)
sink()

# degreeCent ~ bracket
# Box and whiskers
png(filename = "degreeCent-bracket_boxPlots.png", width = 800, height = 600)
plot(NADF$degreeCent ~ NADF$bracket, data = NADF)
dev.off()

# Levene's test
sink(file = "degreeCent-bracket_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("degreeCent-bracket\n\n")
cat("=========================\n\n")
leveneTest(NADF$degreeCent ~ NADF$bracket, data = NADF)
sink()

########################################

# Network density

########################################

# netDensity ~ kickInOutcome
# Box and whiskers
png(filename = "netDensity-kickInOutcome_boxPlots.png", width = 800, height = 600)
plot(NADF$netDensity ~ NADF$kickInOutcome, data = NADF)
dev.off()

# Levene's test
sink(file = "netDensity-kickInOutcome_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("netDensity-kickInOutcome\n\n")
cat("=========================\n\n")
leveneTest(NADF$netDensity ~ NADF$kickInOutcome, data = NADF)
sink()

# netDensity ~ matchOutcome
# Box and whiskers
png(filename = "netDensity-matchOutcome_boxPlots.png", width = 800, height = 600)
plot(NADF$netDensity ~ NADF$matchOutcome, data = NADF)
dev.off()

# Levene's test
sink(file = "netDensity-matchOutcome_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("netDensity-matchOutcome\n\n")
cat("=========================\n\n")
leveneTest(NADF$netDensity ~ NADF$matchOutcome, data = NADF)
sink()

# netDensity ~ bracket
# Box and whiskers
png(filename = "netDensity-bracket_boxPlots.png", width = 800, height = 600)
plot(NADF$netDensity ~ NADF$bracket, data = NADF)
dev.off()

# Levene's test
sink(file = "netDensity-bracket_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("netDensity-bracket\n\n")
cat("=========================\n\n")
leveneTest(NADF$netDensity ~ NADF$bracket, data = NADF)
sink()

########################################

# Entropy

########################################

# entropy ~ kickInOutcome
# Box and whiskers
png(filename = "entropy-kickInOutcome_boxPlots.png", width = 800, height = 600)
plot(NADF$entropy ~ NADF$kickInOutcome, data = NADF)
dev.off()

# Levene's test
sink(file = "entropy-kickInOutcome_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("entropy-kickInOutcome\n\n")
cat("=========================\n\n")
leveneTest(NADF$entropy ~ NADF$kickInOutcome, data = NADF)
sink()

# entropy ~ matchOutcome
# Box and whiskers
png(filename = "entropy-matchOutcome_boxPlots.png", width = 800, height = 600)
plot(NADF$entropy ~ NADF$matchOutcome, data = NADF)
dev.off()

# Levene's test
sink(file = "entropy-matchOutcome_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("entropy-matchOutcome\n\n")
cat("=========================\n\n")
leveneTest(NADF$entropy ~ NADF$matchOutcome, data = NADF)
sink()

# entropy ~ bracket
# Box and whiskers
png(filename = "entropy-bracket_boxPlots.png", width = 800, height = 600)
plot(NADF$entropy ~ NADF$bracket, data = NADF)
dev.off()

# Levene's test
sink(file = "entropy-bracket_levene.txt", append = FALSE, type = "output")
cat("=========================\n\n")
cat("entropy-bracket\n\n")
cat("=========================\n\n")
leveneTest(NADF$entropy ~ NADF$bracket, data = NADF)
sink()

