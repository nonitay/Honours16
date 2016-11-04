################################################################################

# ANOVA 1: kickInOutcome * bracket

################################################################################

# Drop kickInOutcome == RushedOpp_D
NADF_forANOVA <- NADF[ which(NADF$kickInOutcome != "RushedOpp_D"), ]

##### clusterCoef ##############################################################

# Three-way ANOVA
clusterCoef_anova <- aov(clusterCoef ~ kickInOutcome + matchOutcome + bracket
                          + (kickInOutcome*matchOutcome)
                          + (kickInOutcome*bracket)
                          + (matchOutcome * bracket),
                          data = NADF_forANOVA)

# Save ANOVA output to text
sink("clusterCoef_anova.txt", append = FALSE, type = c("output", "message"))
cat("------------------------------------------------------------------------------------------",
    "",
    "Three-way ANOVA",
    "",
    "Main effects: kickInOutcome, matchOutcome, bracket",
    "Interactions: kickInOutcome*matchOutcome, kickInOutcome*bracket, matchOutcome*bracket",
    "",
    "-----------------------------------------------------------------------------------------",
    "",
    sep = "\n")
summary(clusterCoef_anova)
sink()

##### degreeCent ###############################################################

# Three-way ANOVA
degreeCent_anova <- aov(degreeCent ~ kickInOutcome + matchOutcome + bracket
                         + (kickInOutcome*matchOutcome)
                         + (kickInOutcome*bracket)
                         + (matchOutcome * bracket),
                         data = NADF_forANOVA)

# Multiple comparisons for main effect of kickInOutcome
degreeCent_anova_posthoc <- TukeyHSD(degreeCent_anova, "kickInOutcome", ordered = T)

# Save ANOVA output to text
sink("degreeCent_anova.txt", append = FALSE, type = c("output", "message"))
cat("------------------------------------------------------------",
    "",
    "Three-way ANOVA",
    "",
    "Main effects: kickInOutcome, matchOutcome, bracket",
    "Interactions: kickInOutcome*matchOutcome, kickInOutcome*bracket, matchOutcome*bracket",
    "",
    "------------------------------------------------------------",
    "",
    sep = "\n")
summary(degreeCent_anova)
cat("",
    "------------------------------------------------------------",
    "",
    "Multiple comparisons with Tukey p value adjustment",
    "",
    "Main effect: kickInOutcome",
    "",
    "------------------------------------------------------------",
    "",
    sep = "\n")
degreeCent_anova_posthoc
sink()

##### netDensity ###############################################################

# Three-way ANOVA
netDensity_anova <- aov(netDensity ~ kickInOutcome + matchOutcome + bracket
                        + (kickInOutcome*matchOutcome)
                        + (kickInOutcome*bracket)
                        + (matchOutcome * bracket),
                        data = NADF_forANOVA)

# Multiple comparisons for main effect of kickInOutcome
netDensity_anova_posthoc1 <- TukeyHSD(netDensity_anova, "kickInOutcome", ordered = T)

# Multiple comparisons for main effect of matchOutcome
netDensity_anova_posthoc2 <- TukeyHSD(netDensity_anova, "matchOutcome", ordered = T)

# Save ANOVA output to text
sink("netDensity_anova.txt", append = FALSE, type = c("output", "message"))
cat("------------------------------------------------------------",
    "",
    "Three-way ANOVA",
    "",
    "Main effects: kickInOutcome, matchOutcome, bracket",
    "Interactions: kickInOutcome*matchOutcome, kickInOutcome*bracket, matchOutcome*bracket",
    "",
    "------------------------------------------------------------",
    "",
    sep = "\n")
summary(netDensity_anova)
cat("",
    "------------------------------------------------------------",
    "",
    "Multiple comparisons with Tukey p value adjustment",
    "",
    "Main effect: kickInOutcome",
    "",
    "------------------------------------------------------------",
    "",
    sep = "\n")
netDensity_anova_posthoc1
cat("",
    "------------------------------------------------------------",
    "",
    "Multiple comparisons with Tukey p value adjustment",
    "",
    "Main effect: matchOutcome",
    "",
    "------------------------------------------------------------",
    "",
    sep = "\n")
netDensity_anova_posthoc2
sink()

##### entropy ##################################################################

# Three-way ANOVA
entropy_anova <- aov(entropy ~ kickInOutcome + matchOutcome + bracket
                     + (kickInOutcome*matchOutcome)
                     + (kickInOutcome*bracket)
                     + (matchOutcome * bracket),
                     data = NADF_forANOVA)

# Multiple comparisons for main effect of kickInOutcome
entropy_anova_posthoc1 <- TukeyHSD(entropy_anova, "kickInOutcome", ordered = T)

# Multiple comparisons for main effect of matchOutcome
entropy_anova_posthoc2 <- TukeyHSD(entropy_anova, "matchOutcome", ordered = T)

# Save ANOVA output to text
sink("entropy_anova.txt", append = FALSE, type = c("output", "message"))
cat("------------------------------------------------------------",
    "",
    "Three-way ANOVA",
    "",
    "Main effects: kickInOutcome, matchOutcome, bracket",
    "Interactions: kickInOutcome*matchOutcome, kickInOutcome*bracket, matchOutcome*bracket",
    "",
    "------------------------------------------------------------",
    "",
    sep = "\n")
summary(entropy_anova)
cat("",
    "------------------------------------------------------------",
    "",
    "Multiple comparisons with Tukey p value adjustment",
    "",
    "Main effect: kickInOutcome",
    "",
    "------------------------------------------------------------",
    "",
    sep = "\n")
entropy_anova_posthoc1
cat("",
    "------------------------------------------------------------",
    "",
    "Multiple comparisons with Tukey p value adjustment",
    "",
    "Main effect: matchOutcome",
    "",
    "------------------------------------------------------------",
    "",
    sep = "\n")
entropy_anova_posthoc2
sink()