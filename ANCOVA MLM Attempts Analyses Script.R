#Analyses
library(tidyverse)
library(psych)
library(lme4)
library(afex)
library(emmeans)
library(multcomp)
library(car)

kinship_long <- read_csv("kinship_long.csv")

kinship_long$Relationship <- factor(kinship_long$Relationship)

#Replace NAs with 0s for Time and Money DVs, if needed
#kinship_long$TimeGiven[is.na(kinship_long$TimeGiven)] = 0
#kinship_long$MoneyGiven[is.na(kinship_long$MoneyGiven)] = 0

#Trying ANCOVA
#https://stats.stackexchange.com/questions/502460/how-can-i-run-repeated-measure-ancova-in-r

#Time DV
#Relationship x Ind (controlling for collectivism)
TimeIndMeans <- aov(TimeGiven ~ Relationship * IND_C + COL_C + Error(aid/Relationship),
                     data = kinship_long)

summary(TimeIndMeans)

#emmeans for just Relationships
TimeRelEmmeans <- emmeans(TimeIndMeans, ~ Relationship)

#See the contrasts
pairs(TimeRelEmmeans)

#Relationship x Col (controlling for individualism)
TimeColMeans <- aov(TimeGiven ~ Relationship * COL_C + IND_C + Error(aid/Relationship),
                    data = kinship_long)

summary(TimeColMeans)

#Money DV
#Relationship x Ind (controlling for collectivism)
MoneyIndMeans <- aov(MoneyGiven ~ Relationship * IND_C + COL_C + Error(aid/Relationship),
                    data = kinship_long)

summary(MoneyIndMeans)

#emmeans for just Relationships
MoneyRelEmmeans <- emmeans(MoneyIndMeans, ~ Relationship)

#Contrasts
pairs(MoneyRelEmmeans)

#Relationship x Col (controlling for individualism)
MoneyColMeans <- aov(MoneyGiven ~ Relationship * COL_C + IND_C + Error(aid/Relationship),
                    data = kinship_long)

summary(MoneyColMeans)

#MLM test
indTime <- lmer(TimeGiven ~ Relationship * IND_C + COL_C + (1 + Relationship | aid),
                data = kinship_long, na.action = "na.exclude")

#Error: number of observations (=1070) <= number of random effects (=1070) for term (1 + Relationship | aid);
#the random-effects parameters and the residual variance (or scale parameter) are probably unidentifiable
#With long dataset, get error "boundary (singular) fit: see help('isSingular')"


### New package found, added April 17 ###
#https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r
library(rstatix)

#Time DV
#Relationship x Ind (controlling for collectivism)
TimeRelInd <- anova_test(
  data = kinship_long,
  dv = TimeGiven,
  wid = aid,
  within = Relationship,
  between = IND_C,
  covariate = COL_C)

get_anova_table(TimeRelInd)

