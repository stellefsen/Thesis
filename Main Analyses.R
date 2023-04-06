install.packages("reghelper")

#####   Correlations   #####

#Individualism x Time Help
cor.test(kinshipstudy$Ind, kinshipstudy$ParentsTime_Z)
cor.test(kinshipstudy$Ind, kinshipstudy$InlawsTime_Z)
cor.test(kinshipstudy$Ind, kinshipstudy$SpouseTime_Z)
cor.test(kinshipstudy$Ind, kinshipstudy$FriendTime_Z)
cor.test(kinshipstudy$Ind, kinshipstudy$AcqTime_Z)

#Individualism x Money help
cor.test(kinshipstudy$Ind, kinshipstudy$ParentsMoney_Z)
cor.test(kinshipstudy$Ind, kinshipstudy$InlawsMoney_Z)
cor.test(kinshipstudy$Ind, kinshipstudy$SpouseMoney_Z)
cor.test(kinshipstudy$Ind, kinshipstudy$FriendMoney_Z)
cor.test(kinshipstudy$Ind, kinshipstudy$AcqMoney_Z)

#Individualism x Confounds
cor.test(kinshipstudy$Ind, kinshipstudy$ConfPar)
cor.test(kinshipstudy$Ind, kinshipstudy$ConfInLaw)
cor.test(kinshipstudy$Ind, kinshipstudy$ConfSpouse)
cor.test(kinshipstudy$Ind, kinshipstudy$ConfFriend)
cor.test(kinshipstudy$Ind, kinshipstudy$ConfAcq)

#Individualism x Closeness
cor.test(kinshipstudy$Ind, kinshipstudy$ClosePar)
cor.test(kinshipstudy$Ind, kinshipstudy$CloseInLaws)
cor.test(kinshipstudy$Ind, kinshipstudy$CloseSpouse)
cor.test(kinshipstudy$Ind, kinshipstudy$CloseFriend)
cor.test(kinshipstudy$Ind, kinshipstudy$CloseAcq)


#Individualism x Communal Strength 
cor.test(kinshipstudy$Ind, kinshipstudy$CommStrPar)
cor.test(kinshipstudy$Ind, kinshipstudy$CommStrInLaw)
cor.test(kinshipstudy$Ind, kinshipstudy$CommStrSpouse)
cor.test(kinshipstudy$Ind, kinshipstudy$CommStrFriend)
cor.test(kinshipstudy$Ind, kinshipstudy$CommStrAcq)

#Individualism x Perceived Social Support 
cor.test(kinshipstudy$Ind, kinshipstudy$SuppPar)
cor.test(kinshipstudy$Ind, kinshipstudy$SuppInLaws)
cor.test(kinshipstudy$Ind, kinshipstudy$SuppSpouse)
cor.test(kinshipstudy$Ind, kinshipstudy$SuppFriend)
cor.test(kinshipstudy$Ind, kinshipstudy$SuppAcq)

#Collectivism x Time Help, standardized 
cor.test(kinshipstudy$Col, kinshipstudy$ParentsTime_Z)
cor.test(kinshipstudy$Col, kinshipstudy$InlawsTime_Z)
cor.test(kinshipstudy$Col, kinshipstudy$SpouseTime_Z)
cor.test(kinshipstudy$Col, kinshipstudy$FriendTime_Z)
cor.test(kinshipstudy$Col, kinshipstudy$AcqTime_Z)

#Collectivism x Money Help, standardized 
cor.test(kinshipstudy$Col, kinshipstudy$ParentsMoney_Z)
cor.test(kinshipstudy$Col, kinshipstudy$InlawsMoney_Z)
cor.test(kinshipstudy$Col, kinshipstudy$SpouseMoney_Z)
cor.test(kinshipstudy$Col, kinshipstudy$FriendMoney_Z)
cor.test(kinshipstudy$Col, kinshipstudy$AcqMoney_Z)

#Collectivism x Confounds
cor.test(kinshipstudy$Col, kinshipstudy$ConfPar)
cor.test(kinshipstudy$Col, kinshipstudy$ConfInLaw)
cor.test(kinshipstudy$Col, kinshipstudy$ConfSpouse)
cor.test(kinshipstudy$Col, kinshipstudy$ConfFriend)
cor.test(kinshipstudy$Col, kinshipstudy$ConfAcq)

#Collectivism x Closeness
cor.test(kinshipstudy$Col, kinshipstudy$ClosePar)
cor.test(kinshipstudy$Col, kinshipstudy$CloseInLaws)
cor.test(kinshipstudy$Col, kinshipstudy$CloseSpouse)
cor.test(kinshipstudy$Col, kinshipstudy$CloseFriend)
cor.test(kinshipstudy$Col, kinshipstudy$CloseAcq)

#Collectivism x Communal Strength
cor.test(kinshipstudy$Col, kinshipstudy$CommStrPar)
cor.test(kinshipstudy$Col, kinshipstudy$CommStrInLaw)
cor.test(kinshipstudy$Col, kinshipstudy$CommStrSpouse)
cor.test(kinshipstudy$Col, kinshipstudy$CommStrFriend)
cor.test(kinshipstudy$Col, kinshipstudy$CommStrAcq)

#Collectivism x PSS
cor.test(kinshipstudy$Col, kinshipstudy$SuppPar)
cor.test(kinshipstudy$Col, kinshipstudy$SuppInLaws)
cor.test(kinshipstudy$Col, kinshipstudy$SuppSpouse)
cor.test(kinshipstudy$Col, kinshipstudy$SuppFriend)
cor.test(kinshipstudy$Col, kinshipstudy$SuppAcq)

#Correlation between money and time help
cor.test(kinshipstudy$TimeHelp_1, kinshipstudy$MoneyHelp_1)
cor.test(kinshipstudy$TimeHelp_2, kinshipstudy$MoneyHelp_2)
cor.test(kinshipstudy$TimeHelp_3, kinshipstudy$MoneyHelp_3)
cor.test(kinshipstudy$TimeHelp_6, kinshipstudy$MoneyHelp_6)
cor.test(kinshipstudy$TimeHelp_7, kinshipstudy$MoneyHelp_7)

## Multiple Regression
# r1<-lm(y~x1+x2,data=data)

##Cultural Orientation x Time/Money Given, by relationship ##

#Parents 
IndParentsM<-lm(ParentsMoney_Z~Ind, data = kinshipstudy)
IndParentsT<-lm(ParentsTime_Z~Ind, data = kinshipstudy)
ColParentsM<-lm(ParentsMoney_Z~Col, data = kinshipstudy)
ColParentsT<-lm(ParentsTime_Z~Col, data = kinshipstudy)

#Inlaws
IndInlawsM<-lm(InlawsMoney_Z~Ind, data = kinshipstudy)
IndInlawsT<-lm(InlawsTime_Z~Ind, data = kinshipstudy)
ColInlawsM<-lm(InlawsMoney_Z~Col, data = kinshipstudy)
ColInlawsT<-lm(InlawsTime_Z~Col, data = kinshipstudy)

#Spouse
IndSpouseM<-lm(SpouseMoney_Z~Ind, data = kinshipstudy)
IndSpouseT<-lm(SpouseTime_Z~Ind, data = kinshipstudy)
ColSpouseM<-lm(SpouseMoney_Z~Col, data = kinshipstudy)
ColSpouseT<-lm(SpouseTime_Z~Col, data = kinshipstudy)

#Friend
IndFriendM<-lm(FriendMoney_Z~Ind, data = kinshipstudy)
IndFriendT<-lm(FriendTime_Z~Ind, data = kinshipstudy)
ColFriendM<-lm(FriendMoney_Z~Col, data = kinshipstudy)
ColFriendT<-lm(FriendTime_Z~Col, data = kinshipstudy)

#Acq
IndAcqM<-lm(AcqMoney_Z~Ind, data = kinshipstudy)
IndAcqT<-lm(AcqTime_Z~Ind, data = kinshipstudy)
ColAcqM<-lm(AcqMoney_Z~Col, data = kinshipstudy)
ColAcqT<-lm(AcqTime_Z~Col, data = kinshipstudy)

#### Interaction Regressions
ParentxCultureM<-lm(ParentsMoney_Z~Ind:Col, data = kinshipstudy)
ParentxCultureT<-lm(ParentsTime_Z~Ind:Col, data = kinshipstudy)

InlawsxCultureM<-lm(InlawsMoney_Z~Ind:Col, data = kinshipstudy)
InlawsxCultureT<-lm(InlawsTime_Z~Ind:Col, data = kinshipstudy)

SpousexCultureM<-lm(SpouseMoney_Z~Ind:Col, data = kinshipstudy)
SpousexCultureT<-lm(SpouseTime_Z~Ind:Col, data = kinshipstudy)


FriendxCultureM<-lm(FriendMoney_Z~Ind:Col, data = kinshipstudy)
FriendxCultureT<-lm(FriendTime_Z~Ind:Col, data = kinshipstudy)

AcqxCultureM<-lm(AcqMoney_Z~Ind:Col, data = kinshipstudy)
AcqxCultureT<-lm(AcqTime_Z~Ind:Col, data = kinshipstudy)

### Regressions Assessing Relationship Helping Differences
ParentsxInlawsM<-lm(ParentsMoney_Z~InlawsMoney_Z, data = kinshipstudy)
ParentsxInlawsT<-lm(ParentsTime_Z~InlawsTime_Z, data = kinshipstudy)

ParentsxSpouseM<-lm(ParentsMoney_Z~SpouseMoney_Z, data = kinshipstudy)
ParentsxSpouseT<-lm(ParentsTime_Z~SpouseTime_Z, data = kinshipstudy)


ParentsxFriendM<-lm(ParentsMoney_Z~FriendMoney_Z, data = kinshipstudy)
ParentsxFriendT<-lm(ParentsTime_Z~FriendTime_Z, data = kinshipstudy)

ParentsxAcqM<-lm(ParentsMoney_Z~AcqMoney_Z, data = kinshipstudy)
ParentsxAcqT<-lm(ParentsTime_Z~AcqTime_Z, data = kinshipstudy)

InlawsxSpouseM<-lm(InlawsMoney_Z~SpouseMoney_Z, data = kinshipstudy)
InlawsxSpouseT<-lm(InlawsTime_Z~SpouseTime_Z, data = kinshipstudy)


InlawsxFriendM<-lm(InlawsMoney_Z~FriendMoney_Z, data = kinshipstudy)
InlawsxFriendT<-lm(InlawsTime_Z~FriendTime_Z, data = kinshipstudy)

InlawsxAcqM<-lm(InlawsMoney_Z~AcqMoney_Z, data = kinshipstudy)
InlawsxAcqT<-lm(InlawsTime_Z~AcqTime_Z, data = kinshipstudy)

SpousexFriendM<-lm(SpouseMoney_Z~FriendMoney_Z, data = kinshipstudy)
SpousexFriendT<-lm(SpouseTime_Z~FriendTime_Z, data = kinshipstudy)

SpousexAcqM<-lm(SpouseMoney_Z~AcqMoney_Z, data = kinshipstudy)
SpousexAcqT<-lm(SpouseTime_Z~AcqTime_Z, data = kinshipstudy)

FriendxAcqM<-lm(FriendMoney_Z~AcqMoney_Z, data = kinshipstudy)
FriendxAcqT<-lm(FriendTime_Z~AcqTime_Z, data = kinshipstudy)

##### Regressions for Time and Money differences for same relationship
MoneyxTimeP<-lm(ParentsMoney_Z~ParentsTime_Z, data = kinshipstudy)
MoneyxTimeInL<-lm(InlawsMoney_Z~InlawsTime_Z, data = kinshipstudy)
MoneyxTimeSp<-lm(SpouseMoney_Z~SpouseTime_Z, data = kinshipstudy)
MoneyxTimeFr<-lm(FriendMoney_Z~FriendTime_Z, data = kinshipstudy)
MoneyxTimeAcq<-lm(AcqMoney_Z~AcqTime_Z, data = kinshipstudy)

# r1<-lm(y~x1+x2,data=data)
###RelClose + Ind/Col, Money/Time Help

##ClosePar, Ind, ParentsMoney_Z
CloseParxIndxMoney<-lm(ParentsMoney_Z~ClosePar+Ind, data = kinshipstudy)
CloseParxIndxTime<-lm(ParentsTime_Z~ClosePar+Ind, data = kinshipstudy)

##ClosePar, Col, ParentsMoney_Z
CloseParxColxMoney<-lm(ParentsMoney_Z~ClosePar+Col, data = kinshipstudy)
CloseParxColxTime<-lm(ParentsTime_Z~ClosePar+Col, data = kinshipstudy)

###3-way interaction
test<-lm(ParentsTime_Z~Col:ClosePar:Ind, data = kinshipstudy)
test2<-lm(ParentsMoney_Z~ClosePar+Col:ClosePar+Ind, data = kinshipstudy)

###CommStr, Ind, ParentsMoney_Z
CommStrParxIndxMoney<-lm(ParentsMoney_Z~CommStrPar+Ind, data = kinshipstudy)
CommStrParxIndxTime<-lm(ParentsTime_Z~CommStrPar+Ind, data = kinshipstudy)

###CommStr, Col, Parents Money
CommStrParxColxMoney<-lm(ParentsMoney_Z~CommStrPar+Col, data = kinshipstudy)
CommStrParxColxTime<-lm(ParentsTime_Z~CommStrPar+Col, data = kinshipstudy)

### Interaction 
Int<-lm(ParentsTime_Z~CommStrPar+Col:CommStrPar+Ind, data = kinshipstudy)
Int2<-lm(ParentsMoney_Z~CommStrPar+Col:CommStrPar+Ind, data = kinshipstudy)
Int3<-lm(ParentsTime_Z~CommStrPar:Col:Ind, data = kinshipstudy)

### Simple Slope for Interactions


### ANCOVA
#Analyses
library(tidyverse)
library(psych)
library(lme4)
install.packages("afex")
library(afex)
install.packages("emmeans")
library(emmeans)
install.packages("multcomp")
library(multcomp)
library(car)

kinship_long <- read_csv("kinship_long.csv")

kinship_long$Relationship <- factor(kinship_long$Relationship)

#Replace NAs with 0s for Time and Money DVs, if needed
kinship_long$TimeGiven[is.na(kinship_long$TimeGiven)] = 0
kinship_long$MoneyGiven[is.na(kinship_long$MoneyGiven)] = 0

#Trying ANCOVA
#https://stats.stackexchange.com/questions/502460/how-can-i-run-repeated-measure-ancova-in-r

#Time DV
#Relationship x Ind (controlling for collectivism)
TimeIndMeans <- aov(TimeGiven ~ Relationship * IND_C + COL_C + Error(aid/Relationship),
                    data = kinship_long)

summary(TimeIndMeans)

#emmeans for just Relationships
TimeRelEmmeans <- emmeans(TimeIndMeans, ~ Relationship)

#Contrasts
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


