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

#########  CLOSENESS #########
Rel1<-cor.test(kinshipstudy$ParClose_1, kinshipstudy$ParClose_2)
#reliability for closeness for parents, r = .785

Rel2<-cor.test(kinshipstudy$PPClose_1, kinshipstudy$PPClose_2)
#reliability for closeness for inlaws, r = 0.832

Rel3<-cor.test(kinshipstudy$PartClose_1, kinshipstudy$PartClose_2)
#reliability for closeness for spouse, r = 0.767

Rel4<-cor.test(kinshipstudy$FriendClose_1, kinshipstudy$FriendClose_2)
#reliability for closeness for close friend, r = 0.697

Rel5<-cor.test(kinshipstudy$AcqClose_1, kinshipstudy$AcqClose_2)
#reliability for closeness for acquaintances, r = 0.868

# Mean r = 0.7898, Range: 0.697-0.868

######## COMMUNAL STRENGTH ########
#Cronbach's alpha

ParCommunal <- kinshipstudy %>%
  select('RelBen_1', 'RelHap_1', 'RelCost_1', 'RelPrior_1')

alpha(ParCommunal)
#raw_alpha = .9

InLawCommunal <- kinshipstudy %>%
  select('RelBen_2', 'RelHap_2', 'RelCost_2', 'RelPrior_2')

alpha(InLawCommunal)
#raw_alpha = 0.92

SpouseCommunal <- kinshipstudy %>%
  select('RelBen_3', 'RelHap_3', 'RelCost_3', 'RelPrior_3')

alpha(SpouseCommunal)
#raw_alpha = 0.88

FriendCommunal <- kinshipstudy %>%
  select('RelBen_6', 'RelHap_6', 'RelCost_6', 'RelPrior_6')

alpha(FriendCommunal)
#raw_alpha = 0.86

AcqCommunal <- kinshipstudy %>%
  select('RelBen_7', 'RelHap_7', 'RelCost_7', 'RelPrior_7')

alpha(AcqCommunal)
#raw_alpha = 0.88

#Mean alpha = 0.888, Range = 0.86-0.92

####### PERCEIVED SOCIAL SUPPORT ########
ParPSS <- kinshipstudy %>%
  select('ParSup_1', 'ParSup_2', 'ParSup_3')

alpha(ParPSS)
#raw_alpha = .91

InLawPSS <- kinshipstudy %>%
  select('PPSup_1', 'PPSup_2', 'PPSup_3')

alpha(InLawPSS)
#raw_alpha = 0.93

SpousePSS <- kinshipstudy %>%
  select('PartSup_1', 'PartSup_2', 'PartSup_3')

alpha(SpousePSS)
#raw_alpha = 0.89

FriendPSS <- kinshipstudy %>%
  select('FriendSup_1', 'FriendSup_2', 'FriendSup_3')

alpha(FriendPSS)
#raw_alpha = 0.89

AcqPSS <- kinshipstudy %>%
  select('AcqSup_1', 'AcqSup_2', 'AcqSup_3')

alpha(AcqPSS)
#raw_alpha = 0.93

#Mean alpha = 0.91, range = 0.89-0.93

#Effect sizes
#Individualism (copied from Main Analyses)
TimeRelInd <- aov(TimeGiven ~ Relationship * IND_C + COL_C + Error(aid),
                  data = kinship_long)
#See the output
summary(TimeRelInd)

#Computing partial eta^2 (effect size) = Sum Sq of IV / (Sum Sq of IV + Sum Sq of Residuals)
#Example for Relationship: Sum Sq of Relationship = 1685, Sum Sq of Residuals = 4840
1685/(1685 + 4840)
