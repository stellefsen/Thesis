#Honors Thesis

#Load in relevant packages
#install.packages("Package") if don't have it yet
#Example: install.packages("tidyverse")
library(tidyverse)
library(psych)

##Set working directory

setwd("C:/Users/stell/OneDrive/Documents/R/Thesis/Thesis")

#Import dataset
kinshipstudy<- read_csv("KinshipCleaned2.csv")

#Composites
#Resource allocation: z-score to standardize time and money scenarios
#Cultural orientation
#Confounds by relationship
#Closeness
#Communal strength
#Perceived support

###FINDING THE MEANS OF ITEMS
#dataset$composite_name <- rowMeans(
# subset(dataset,
#        select = c(list each item in composite)
#   )
#)

###Cultural orientation###
#Individualism: IndCol_1, IndCol_3, IndCol_4, IndCol_7, IndCol_10

kinshipstudy$Ind <- rowMeans(
  subset(kinshipstudy,
         select = c(IndCol_1,IndCol_3, IndCol_4, IndCol_7, IndCol_10)
))

view(kinshipstudy$Ind)
#found mean of each individualism question 

#See first 20 entries for Ind
kinshipstudy$Ind %>% head(20)
#head is first n values, in this case, first 20 values of individualism variable
#%>% means values in dataset 

#Collectivism: IndCol_2, IndCol_5, IndCol_6, IndCol_8, IndCol_9, IndCol_11, IndCol_12

kinshipstudy$Col <- rowMeans(
  subset(kinshipstudy,
         select = c(IndCol_2,IndCol_5, IndCol_6, IndCol_8, IndCol_9, IndCol_11, IndCol_12)
  ))

kinshipstudy$Col%>%head(20)

###Confounds by relationship###

#ConfoundParents: RelDist_1, RelCont_1, RelTalk_1, RelPhone_1

kinshipstudy$ConfPar <- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_1, RelCont_1, RelTalk_1, RelPhone_1)
  ))

kinshipstudy$ConfPar%>%head(20)

#ConfoundInlaws: RelDist_2, RelCont_2, RelTalk_2, RelPhone_2

kinshipstudy$ConfInLaw <- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_2, RelCont_2, RelTalk_2, RelPhone_2)
  ))

kinshipstudy$ConfInLaw%>%head(20)

#ConfoundSpouse: RelDist_3, RelCont_3, RelTalk_3, RelPhone_3

kinshipstudy$ConfSpouse<- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_3, RelCont_3, RelTalk_3, RelPhone_3)
  ))

kinshipstudy$ConfSpouse%>%head(20)

#ConfoundSibling:RelDist_4, RelCont_4, RelTalk_4, RelPhone_4

kinshipstudy$ConfSib<- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_4, RelCont_4, RelTalk_4, RelPhone_4)
  ))

kinshipstudy$ConfSib%>%head(20)

#ConfoundCousin:RelDist_5, RelCont_5, RelTalk_5, RelPhone_5

kinshipstudy$ConfCous<- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_5, RelCont_5, RelTalk_5, RelPhone_5)
  ))

kinshipstudy$ConfCous%>%head(20)

#ConfoundFriend:RelDist_6, RelCont_6, RelTalk_6, RelPhone_6

kinshipstudy$ConfFriend<- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_6, RelCont_6, RelTalk_6, RelPhone_6)
  ))

kinshipstudy$ConfFriend%>%head(20)

#ConfoundAcq:RelDist_7, RelCont_7, RelTalk_7, RelPhone_7

kinshipstudy$ConfAcq<- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_7, RelCont_7, RelTalk_7, RelPhone_7)
  ))

kinshipstudy$ConfAcq%>%head(20)

###Closeness###
#CloseParents: ParClose_1, ParClose_2

kinshipstudy$ClosePar<- rowMeans(
  subset(kinshipstudy,
         select = c(ParClose_1, ParClose_2)
  ))

kinshipstudy$ClosePar%>%head(20)

#CloseInlaws: PPClose_1, PPClose_2

kinshipstudy$CloseInLaws<- rowMeans(
  subset(kinshipstudy,
         select = c(PPClose_1, PPClose_2)
  ))

kinshipstudy$CloseInLaws%>%head(20)

#CloseSpouse:

kinshipstudy$CloseSpouse<- rowMeans(
  subset(kinshipstudy,
         select = c(PartClose_1, PartClose_2)
  ))

kinshipstudy$CloseSpouse%>%head(20)

#CloseSibling:

kinshipstudy$CloseSib<- rowMeans(
  subset(kinshipstudy,
         select = c(SibClose_1, SibClose_2)
  ))

kinshipstudy$CloseSib%>%head(20)

#CloseCousin:

kinshipstudy$CloseCousin<- rowMeans(
  subset(kinshipstudy,
         select = c(CousinClose_1, CousinClose_2)
  ))

kinshipstudy$CloseCousin%>%head(20)

#CloseFriend: 

kinshipstudy$CloseFriend<- rowMeans(
  subset(kinshipstudy,
         select = c(FriendClose_1, FriendClose_2)
  ))

kinshipstudy$CloseFriend%>%head(20)

#CloseAcq: 

kinshipstudy$CloseAcq<- rowMeans(
  subset(kinshipstudy,
         select = c(AcqClose_1, AcqClose_2)
  ))

kinshipstudy$CloseAcq%>%head(20)

###Communal Strength###
#CommParents: RelBen_1, RelHap_1

kinshipstudy$CommStrPar<- rowMeans(
  subset(kinshipstudy,
         select = c(RelBen_1, RelHap_1, RelCost_1, RelPrior_1)
  ))

kinshipstudy$CommStrPar%>%head(20)

#CommInLaws:

kinshipstudy$CommStrInLaw<- rowMeans(
  subset(kinshipstudy,
         select = c(RelBen_2, RelHap_2, RelCost_2, RelPrior_2)
  ))

kinshipstudy$CommStrInLaw%>%head(20)

#CommSpouse:

kinshipstudy$CommStrSpouse<- rowMeans(
  subset(kinshipstudy,
         select = c(RelBen_3, RelHap_3, RelCost_3, RelPrior_3)
  ))

kinshipstudy$CommStrSpouse%>%head(20)

#CommSibling:

kinshipstudy$CommStrSib<- rowMeans(
  subset(kinshipstudy,
         select = c(RelBen_4, RelHap_4, RelCost_4, RelPrior_4
)
  ))

kinshipstudy$CommStrSib%>%head(20)

#CommCousin:

kinshipstudy$CommStrCous<- rowMeans(
  subset(kinshipstudy,
         select = c(RelBen_5, RelHap_5, RelCost_5, RelPrior_5)
  ))

kinshipstudy$CommStrCous%>%head(20)

#CommFriend

kinshipstudy$CommStrFriend<- rowMeans(
  subset(kinshipstudy,
         select = c( RelBen_6, RelHap_6, RelCost_6, RelPrior_6
)
  ))

kinshipstudy$CommStrFriend%>%head(20)

#CommAcq:

kinshipstudy$CommStrAcq<- rowMeans(
  subset(kinshipstudy,
         select = c( RelBen_7, RelHap_7, RelCost_7, RelPrior_7)
  ))

kinshipstudy$CommStrAcq%>%head(20)

###Perceived support###
#SuppParents:ParSup_1, ParSup_2, ParSup_3

kinshipstudy$SuppPar<- rowMeans(
  subset(kinshipstudy,
         select = c(ParSup_1, ParSup_2, ParSup_3)
  ))

kinshipstudy$SuppPar%>%head(20)

#SuppInlaws: 

kinshipstudy$SuppInLaws<- rowMeans(
  subset(kinshipstudy,
         select = c(PPSup_1, PPSup_2, PPSup_3)
  ))

kinshipstudy$SuppInLaws%>%head(20)

#SuppSpouse:

kinshipstudy$SuppSpouse<- rowMeans(
  subset(kinshipstudy,
         select = c(PartSup_1, PartSup_2, PartSup_3)
  ))

kinshipstudy$SuppSpouse%>%head(20)

#SuppSibling:

kinshipstudy$SuppSib<- rowMeans(
  subset(kinshipstudy,
         select = c(SibSup_1, SibSup_2, SibSup_3)
  ))

kinshipstudy$SuppSib%>%head(20)

#SuppCousin:

kinshipstudy$SuppCous<- rowMeans(
  subset(kinshipstudy,
         select = c(CousinSup_1, CousinSup_2, CousinSup_3)
  ))

kinshipstudy$SuppCous%>%head(20)

#SuppFriend:

kinshipstudy$SuppFriend<- rowMeans(
  subset(kinshipstudy,
         select = c(FriendSup_1, FriendSup_2, FriendSup_3)
  ))

kinshipstudy$SuppFriend%>%head(20)

#SuppAcq:

kinshipstudy$SuppAcq<- rowMeans(
  subset(kinshipstudy,
         select = c(AcqSup_1, AcqSup_2, AcqSup_3)
  ))

kinshipstudy$SuppAcq%>%head(20)
 

#Race data cleaning
#Create overall race variable
kinshipstudy$Race <- NA

kinshipstudy["RaceEth_1"][is.na(kinshipstudy["RaceEth_1"])] <- 0
kinshipstudy["RaceEth_2"][is.na(kinshipstudy["RaceEth_2"])] <- 0
kinshipstudy["RaceEth_3"][is.na(kinshipstudy["RaceEth_3"])] <- 0
kinshipstudy["RaceEth_4"][is.na(kinshipstudy["RaceEth_4"])] <- 0
kinshipstudy["RaceEth_5"][is.na(kinshipstudy["RaceEth_5"])] <- 0
kinshipstudy["RaceEth_6"][is.na(kinshipstudy["RaceEth_6"])] <- 0
kinshipstudy["RaceEth_7"][is.na(kinshipstudy["RaceEth_7"])] <- 0
kinshipstudy["RaceEth_8"][is.na(kinshipstudy["RaceEth_8"])] <- 0

for (i in 1:nrow(kinshipstudy)) { #For each row
  if ((kinshipstudy$RaceEth_1[i] + kinshipstudy$RaceEth_2[i] + kinshipstudy$RaceEth_3[i] +
       kinshipstudy$RaceEth_4[i] + kinshipstudy$RaceEth_5[i] + kinshipstudy$RaceEth_6[i] +
       kinshipstudy$RaceEth_7[i]) > 1) { #If more than one race category is checked off
    kinshipstudy$Race[i] <- 8 #8 = Multiracial
  } else if (kinshipstudy$RaceEth_1[i] == 1) { #Checked RaceEth_1 (White)
    kinshipstudy$Race[i] <- 1 #1 = White
  } else if (kinshipstudy$RaceEth_2[i] == 1) {
    kinshipstudy$Race[i] <- 2 #2 = Black
  } else if (kinshipstudy$RaceEth_3[i] == 1) {
    kinshipstudy$Race[i] <- 3 #3 = Native American/Alaskan Native
  } else if (kinshipstudy$RaceEth_4[i] == 1) {
    kinshipstudy$Race[i] <- 4 #4 = Latino/a/x
  } else if (kinshipstudy$RaceEth_5[i] == 1) {
    kinshipstudy$Race[i] <- 5 #5 = Asian
  } else if (kinshipstudy$RaceEth_6[i] == 1) {
    kinshipstudy$Race[i] <- 6 #6 = Native Hawaiian
  } else if (kinshipstudy$RaceEth_7[i] == 1) {
    kinshipstudy$Race[i] <- 7 #7 = Other
  } else { #For whatever reason, it's all blank or don't want to say
    kinshipstudy$Race[i] <- NA #Leave Race blank so won't count towards totals
  }}


#Standardizing helping scenarios
#https://statisticsglobe.com/z-score-in-r

#TimeHelp_1 (Parents i think)
kinshipstudy$TimeHelp1_Z<- scale(kinshipstudy$TimeHelp_1)

mean(kinshipstudy$TimeHelp1_Z, na.rm = T)
SD(kinshipstudy$TimeHelp1_Z, na.rm = T)

#MoneyHelp_1
kinshipstudy$MoneyHelp1_z <- scale(kinshipstudy$MoneyHelp_1)

mean(kinshipstudy$MoneyHelp1_z, na.rm = T)
sd(kinshipstudy$MoneyHelp1_z, na.rm = T)

kinshipstudy$SibCous<-0

for (i in 1:nrow(kinshipstudy)) { #For each row
  if ((kinshipstudy$RelCheck_1[i] + kinshipstudy$RelCheck_2[i]) > 0) { #If sib/cousin is checked off
    kinshipstudy$SibCous[i] <- 1 #1 = they have a sibling and/or cousin
  } else { #For whatever reason, it's all blank or don't want to say
    kinshipstudy$SibCous[i] <- 0 #Leave Race blank so won't count towards totals
  }}

##Saving new dataset
write_csv(kinshipstudy, "kinship_cleaned_3.8.2023.csv")

kinshipstudy<-kinshipstudy[,-209]
