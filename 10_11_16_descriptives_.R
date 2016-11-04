library(plyr)

###############################################################################################

# Descriptive statistics: By kickInOutcome

###############################################################################################
#Delete rushed opposition
NADF <- NADF[ which(NADF$kickInOutcome != "RushedOpp_D"), ]
###############################################################################################


descStats_kickInOutcome <- ddply(NADF, .(kickInOutcome), summarise,
                                 clusterCoef_M = mean(clusterCoef, na.rm = T),
                                 clusterCoef_SD = sd(clusterCoef, na.rm = T),
                                 degreeCent_M = mean(degreeCent, na.rm = T),
                                 degreeCent_SD = sd(degreeCent, na.rm = T),
                                 netDensity_M = mean(netDensity, na.rm = T),
                                 netDensity_SD = sd(netDensity, na.rm = T),
                                 entropy_M = mean(entropy, na.rm = T),
                                 entropy_SD = sd(entropy, na.rm = T)
)

descStats_kickInOutcome$clusterCoef_M <- round(descStats_kickInOutcome$clusterCoef_M, 3)
descStats_kickInOutcome$clusterCoef_SD <- round(descStats_kickInOutcome$clusterCoef_SD, 3)
descStats_kickInOutcome$degreeCent_M <- round(descStats_kickInOutcome$degreeCent_M, 3)
descStats_kickInOutcome$degreeCent_SD <- round(descStats_kickInOutcome$degreeCent_SD, 3)
descStats_kickInOutcome$netDensity_M <- round(descStats_kickInOutcome$netDensity_M, 3)
descStats_kickInOutcome$netDensity_SD <- round(descStats_kickInOutcome$netDensity_SD, 3)
descStats_kickInOutcome$entropy_M <- round(descStats_kickInOutcome$entropy_M, 3)
descStats_kickInOutcome$entropy_SD <- round(descStats_kickInOutcome$entropy_SD, 3)

descStats_kickInOutcome <- descStats_kickInOutcome[c(2,1,6,10,3,7,5,9,4,8),]

# Save to CSV
write.csv(descStats_kickInOutcome, "descStats_kickInOutcome.csv", row.names = F)

###############################################################################################

# Descriptive statistics: By bracket

###############################################################################################

descStats_bracket <- ddply(NADF, .(bracket), summarise,
                           clusterCoef_M = mean(clusterCoef, na.rm = T),
                           clusterCoef_SD = sd(clusterCoef, na.rm = T),
                           degreeCent_M = mean(degreeCent, na.rm = T),
                           degreeCent_SD = sd(degreeCent, na.rm = T),
                           netDensity_M = mean(netDensity, na.rm = T),
                           netDensity_SD = sd(netDensity, na.rm = T),
                           entropy_M = mean(entropy, na.rm = T),
                           entropy_SD = sd(entropy, na.rm = T)
)

descStats_bracket$clusterCoef_M <- round(descStats_bracket$clusterCoef_M, 3)
descStats_bracket$clusterCoef_SD <- round(descStats_bracket$clusterCoef_SD, 3)
descStats_bracket$degreeCent_M <- round(descStats_bracket$degreeCent_M, 3)
descStats_bracket$degreeCent_SD <- round(descStats_bracket$degreeCent_SD, 3)
descStats_bracket$netDensity_M <- round(descStats_bracket$netDensity_M, 3)
descStats_bracket$netDensity_SD <- round(descStats_bracket$netDensity_SD, 3)
descStats_bracket$entropy_M <- round(descStats_bracket$entropy_M, 3)
descStats_bracket$entropy_SD <- round(descStats_bracket$entropy_SD, 3)

descStats_bracket <- descStats_bracket[c(3,1,2),]

# Save to CSV
write.csv(descStats_bracket, "descStats_bracket.csv", row.names = F)

###############################################################################################

# Descriptive statistics: By matchOutcome

###############################################################################################

descStats_matchOutcome <- ddply(NADF, .(matchOutcome), summarise,
                                clusterCoef_M = mean(clusterCoef, na.rm = T),
                                clusterCoef_SD = sd(clusterCoef, na.rm = T),
                                degreeCent_M = mean(degreeCent, na.rm = T),
                                degreeCent_SD = sd(degreeCent, na.rm = T),
                                netDensity_M = mean(netDensity, na.rm = T),
                                netDensity_SD = sd(netDensity, na.rm = T),
                                entropy_M = mean(entropy, na.rm = T),
                                entropy_SD = sd(entropy, na.rm = T)
)

descStats_matchOutcome$clusterCoef_M <- round(descStats_matchOutcome$clusterCoef_M, 3)
descStats_matchOutcome$clusterCoef_SD <- round(descStats_matchOutcome$clusterCoef_SD, 3)
descStats_matchOutcome$degreeCent_M <- round(descStats_matchOutcome$degreeCent_M, 3)
descStats_matchOutcome$degreeCent_SD <- round(descStats_matchOutcome$degreeCent_SD, 3)
descStats_matchOutcome$netDensity_M <- round(descStats_matchOutcome$netDensity_M, 3)
descStats_matchOutcome$netDensity_SD <- round(descStats_matchOutcome$netDensity_SD, 3)
descStats_matchOutcome$entropy_M <- round(descStats_matchOutcome$entropy_M, 3)
descStats_matchOutcome$entropy_SD <- round(descStats_matchOutcome$entropy_SD, 3)

# Save to CSV
write.csv(descStats_matchOutcome, "descStats_matchOutcome.csv", row.names = F)