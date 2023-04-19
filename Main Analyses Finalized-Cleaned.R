##### Main Analyses Finalized/Cleaned Up #####

library(tidyverse)
library(psych)
library(emmeans)
library(lmerTest)
library(ggiraphExtra)

#Long dataset
kinship_long <- read_csv("kinship_long.csv")
kinship_long$Relationship <- factor(kinship_long$Relationship)

#Wide dataset
kinshipstudy <- read_csv("kinship_wide.csv")

### Main Analyses (Time and Money) ###
### Using ANCOVA ###

#Time
#Individualism (copied from Main Analyses)
TimeRelInd <- aov(TimeGiven ~ Relationship * IND_C + COL_C + Error(aid),
                  data = kinship_long)
#See the output
summary(TimeRelInd)

#Compare the time means for each Relationship in pairs (contrasts for which Relationships differed in time given)
#Since only relationship is categorical (and significant)
TimeRelEmmeans <- emmeans(TimeRelInd, ~ Relationship)
pairs(TimeRelEmmeans)

#Collectivism (copied form Main Analyses)
TimeRelCol <- aov(TimeGiven ~ Relationship * COL_C + IND_C + Error(aid),
                  data = kinship_long)

summary(TimeRelCol)
#Same as individualism, so use same TimeRelEmmeans

#Money
#Individualism
MoneyRelInd <- aov(MoneyGiven ~ Relationship * IND_C + COL_C + Error(aid),
                   data = kinship_long)

summary(MoneyRelInd)

#Compare the money means for each Relationship in pairs (contrasts)
#Since only relationship is categorical (and significant)
MoneyRelEmmeans <- emmeans(MoneyRelInd, ~ Relationship)
pairs(MoneyRelEmmeans)

#Collectivism
MoneyRelCol <- aov(MoneyGiven ~ Relationship * COL_C + IND_C + Error(aid),
                   data = kinship_long)

summary(MoneyRelCol)

#Relationship x Collectivism interaction marginal (p = .082), so lm regressions for each relationship
#To see whether collectivism matters for money given to each relationship

SpouseMoney <- lm(MoneyGiven ~ COL_C + IND_C,
                  #Still should control for individualism
                  data = kinship_long[kinship_long$Relationship == "Spouse",])
                  #Just select "Spouse" rows so only comparing within Spouses
summary(SpouseMoney)
#Marginal main effect of collectivism on Spouse (estimate (b) = -73.23, SE = 38.96, p = .062)
#People who are more collectivistic give marginally less money to Spouse

ParentsMoney <- lm(MoneyGiven ~ COL_C + IND_C,
                   data = kinship_long[kinship_long$Relationship == "Parents",])
summary(ParentsMoney)

InLawsMoney <- lm(MoneyGiven ~ COL_C + IND_C,
                  data = kinship_long[kinship_long$Relationship == "InLaws",])
summary(InLawsMoney)

FriendMoney <- lm(MoneyGiven ~ COL_C + IND_C,
                  data = kinship_long[kinship_long$Relationship == "Friend",])
summary(FriendMoney)
#Main effect of collectivism on friends (estimate (b) = 37.14, SE = 17.87, p = .039)
#People who are more collectivistic give more money to friends

AcqMoney <- lm(MoneyGiven ~ COL_C + IND_C,
                  data = kinship_long[kinship_long$Relationship == "Acq",])
summary(AcqMoney)
#Main effect of collectivism on acq (estimate (b) = 26.08, SE = 12.54, p = .039)
#People who are more collectivistic give more money to acquaintances

###############################################################################

### Exploratory Analyses (Closeness, Communal Strength, Support) ###
### Using lmer (Multilevel Modeling) ###

#Releveling for contrasts so Spouse is reference group
kinship_long$Relationship <- relevel(kinship_long$Relationship,
                                     ref = "Spouse")

#Closeness
#Individualism
CloseRelInd <- lmer(Closeness ~ Relationship * IND_C + COL_C + (1 | aid),
                       data = kinship_long)
anova(CloseRelInd)

#Post-hoc and to get estimates (to see how Relationship and Individualism affects Closeness)
#look at "Fixed effects:" output (after "Random effects:", before "Correlation of Fixed Effects:")
summary(CloseRelInd)
#Significant main effects (see output)

#Collectivism
CloseRelCol <- lmer(Closeness ~ Relationship * COL_C + IND_C + (1 | aid),
                   data = kinship_long)
anova(CloseRelCol)

#Post-hoc
summary(CloseRelCol)
#Significant main effects (see output)
#Also significant effect ("simple slopes") of collectivism for Acquaintance and InLaws
#(Compared to spouse)

#Communal Strength
#Individualism
CommStrRelInd <- lmer(CommStr ~ Relationship * IND_C + COL_C + (1 | aid),
                       data = kinship_long)

anova(CommStrRelInd)

#Post-hoc
summary(CommStrRelInd)

#Collectivism
CommStrRelCol <- lmer(CommStr ~ Relationship * COL_C + IND_C + (1 | aid),
                      data = kinship_long)

anova(CommStrRelCol)

#Post-hoc
summary(CommStrRelCol)
#Also significant interaction between Relationship and collectivism for acq and inlaws
#(Compared to spouse)

#Support
#Individualism
SuppRelInd <- lmer(Supp ~ Relationship * IND_C + COL_C + (1 | aid),
                   data = kinship_long)

anova(SuppRelInd)

#Post-hoc
summary(SuppRelInd)

#Collectivism
SuppRelCol <- lmer(Supp ~ Relationship * COL_C + IND_C + (1 | aid),
                   data = kinship_long)

anova(SuppRelCol)

#Post-hoc
summary(SuppRelCol)
#Also significant interaction between Relationship and collectivism for acq, inlaws, and parents
#(Compared to spouse)