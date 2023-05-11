### Last Minute Analyses ###

library(tidyverse)
library(psych)
library(emmeans)
library(lmerTest)

#Long dataset
kinship_long <- read_csv("kinship_long.csv")
kinship_long$Relationship <- factor(kinship_long$Relationship)

#Wide dataset
kinshipstudy <- read_csv("kinship_wide.csv")

#Reliabilities
#For repeated measures (e.g., closeness for each relationship, communal strength for each relationship),
#should calculate reliabilities for each relationship --> Use wide "kinshipstudy" dataset

#Correlations for measures w/ just 2 items; Cronbach's alpha for everything else

#Example correlation
cor.test(kinshipstudy$ParClose_1, kinshipstudy$ParClose_2)
#reliability for closeness for parents, r = .785

#Example Cronbach's alpha
ParCommunal <- kinshipstudy %>%
  select('RelBen_1', 'RelHap_1', 'RelCost_1', 'RelPrior_1')

alpha(ParCommunal)
#raw_alpha = .9

#Effect sizes
#Individualism (copied from Main Analyses)
TimeRelInd <- aov(TimeGiven ~ Relationship * IND_C + COL_C + Error(aid),
                  data = kinship_long)
#See the output
summary(TimeRelInd)

#Computing partial eta^2 (effect size) = Sum Sq of IV / (Sum Sq of IV + Sum Sq of Residuals)
#Example for Relationship: Sum Sq of Relationship = 1685, Sum Sq of Residuals = 4840
1685/(1685 + 4840)
