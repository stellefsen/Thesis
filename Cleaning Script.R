#Honors Thesis

#Load in relevant packages
#install.packages("Package") if don't have it yet
install.packages("tidyverse")
install.packages("psych")

#Example: install.packages("tidyverse")
library(tidyverse)
library(psych)

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


###Confounds by relationship###
## Z-Score before taking means 
## Parents
kinshipstudy$RelDist_1_z <- scale(kinshipstudy$RelDist_1)
kinshipstudy$RelCont_1_z <- scale(kinshipstudy$RelCont_1)
kinshipstudy$RelTalk_1_z <- scale(kinshipstudy$RelTalk_1)
kinshipstudy$RelPhone_1_z <- scale(kinshipstudy$RelPhone_1)

#Getting rid of the weird [] at the end of the variable name
kinshipstudy$RelDist_1_z <- as.vector(kinshipstudy$RelDist_1_z)
kinshipstudy$RelCont_1_z <- as.vector(kinshipstudy$RelCont_1_z)
kinshipstudy$RelTalk_1_z <- as.vector(kinshipstudy$RelTalk_1_z)
kinshipstudy$RelPhone_1_z <- as.vector(kinshipstudy$RelPhone_1_z)

## Inlaws
kinshipstudy$RelDist_2_z <- scale(kinshipstudy$RelDist_2)
kinshipstudy$RelCont_2_z <- scale(kinshipstudy$RelCont_2)
kinshipstudy$RelTalk_2_z <- scale(kinshipstudy$RelTalk_2)
kinshipstudy$RelPhone_2_z <- scale(kinshipstudy$RelPhone_2)

#Getting rid of the weird [] at the end of the variable name
kinshipstudy$RelDist_2_z <- as.vector(kinshipstudy$RelDist_2_z)
kinshipstudy$RelCont_2_z <- as.vector(kinshipstudy$RelCont_2_z)
kinshipstudy$RelTalk_2_z <- as.vector(kinshipstudy$RelTalk_2_z)
kinshipstudy$RelPhone_2_z <- as.vector(kinshipstudy$RelPhone_2_z)

## Spouse
kinshipstudy$RelDist_3_z <- scale(kinshipstudy$RelDist_3)
kinshipstudy$RelCont_3_z <- scale(kinshipstudy$RelCont_3)
kinshipstudy$RelTalk_3_z <- scale(kinshipstudy$RelTalk_3)
kinshipstudy$RelPhone_3_z <- scale(kinshipstudy$RelPhone_3)

#Getting rid of the weird [] at the end of the variable name
kinshipstudy$RelDist_3_z <- as.vector(kinshipstudy$RelDist_3_z)
kinshipstudy$RelCont_3_z <- as.vector(kinshipstudy$RelCont_3_z)
kinshipstudy$RelTalk_3_z <- as.vector(kinshipstudy$RelTalk_3_z)
kinshipstudy$RelPhone_3_z <- as.vector(kinshipstudy$RelPhone_3_z)

## Friends
kinshipstudy$RelDist_6_z <- scale(kinshipstudy$RelDist_6)
kinshipstudy$RelCont_6_z <- scale(kinshipstudy$RelCont_6)
kinshipstudy$RelTalk_6_z <- scale(kinshipstudy$RelTalk_6)
kinshipstudy$RelPhone_6_z <- scale(kinshipstudy$RelPhone_6)

#Getting rid of the weird [] at the end of the variable name
kinshipstudy$RelDist_6_z <- as.vector(kinshipstudy$RelDist_6_z)
kinshipstudy$RelCont_6_z <- as.vector(kinshipstudy$RelCont_6_z)
kinshipstudy$RelTalk_6_z <- as.vector(kinshipstudy$RelTalk_6_z)
kinshipstudy$RelPhone_6_z <- as.vector(kinshipstudy$RelPhone_6_z)

#Acquaintances
kinshipstudy$RelDist_7_z <- scale(kinshipstudy$RelDist_7)
kinshipstudy$RelCont_7_z <- scale(kinshipstudy$RelCont_7)
kinshipstudy$RelTalk_7_z <- scale(kinshipstudy$RelTalk_7)
kinshipstudy$RelPhone_7_z <- scale(kinshipstudy$RelPhone_7)

#Getting rid of the weird [] at the end of the variable name
kinshipstudy$RelDist_7_z <- as.vector(kinshipstudy$RelDist_7_z)
kinshipstudy$RelCont_7_z <- as.vector(kinshipstudy$RelCont_7_z)
kinshipstudy$RelTalk_7_z <- as.vector(kinshipstudy$RelTalk_7_z)
kinshipstudy$RelPhone_7_z <- as.vector(kinshipstudy$RelPhone_7_z)


#ConfoundParents: RelDist_1, RelCont_1, RelTalk_1, RelPhone_1

kinshipstudy$ConfPar <- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_1_z, RelCont_1_z, RelTalk_1_z, RelPhone_1_z)
  ))


#ConfoundInlaws: RelDist_2, RelCont_2, RelTalk_2, RelPhone_2

kinshipstudy$ConfInLaw <- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_2_z, RelCont_2_z, RelTalk_2_z, RelPhone_2_z)
  ))


#ConfoundSpouse: RelDist_3, RelCont_3, RelTalk_3, RelPhone_3

kinshipstudy$ConfSpouse<- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_3_z, RelCont_3_z, RelTalk_3_z, RelPhone_3_z)
  ))


#ConfoundFriend:RelDist_6, RelCont_6, RelTalk_6, RelPhone_6

kinshipstudy$ConfFriend<- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_6_z, RelCont_6_z, RelTalk_6_z, RelPhone_6_z)
  ))


#ConfoundAcq:RelDist_7, RelCont_7, RelTalk_7, RelPhone_7

kinshipstudy$ConfAcq<- rowMeans(
  subset(kinshipstudy,
         select = c(RelDist_7_z, RelCont_7_z, RelTalk_7_z, RelPhone_7_z)
  ))


###Closeness###
#CloseParents: ParClose_1, ParClose_2

kinshipstudy$ClosePar<- rowMeans(
  subset(kinshipstudy,
         select = c(ParClose_1, ParClose_2)
  ))


#CloseInlaws: PPClose_1, PPClose_2

kinshipstudy$CloseInLaws<- rowMeans(
  subset(kinshipstudy,
         select = c(PPClose_1, PPClose_2)
  ))


#CloseSpouse:

kinshipstudy$CloseSpouse<- rowMeans(
  subset(kinshipstudy,
         select = c(PartClose_1, PartClose_2)
  ))


#CloseFriend: 

kinshipstudy$CloseFriend<- rowMeans(
  subset(kinshipstudy,
         select = c(FriendClose_1, FriendClose_2)
  ))


#CloseAcq: 

kinshipstudy$CloseAcq<- rowMeans(
  subset(kinshipstudy,
         select = c(AcqClose_1, AcqClose_2)
  ))


###Communal Strength###
#CommParents: RelBen_1, RelHap_1

kinshipstudy$CommStrPar<- rowMeans(
  subset(kinshipstudy,
         select = c(RelBen_1, RelHap_1, RelCost_1, RelPrior_1)
  ))

#CommInLaws:

kinshipstudy$CommStrInLaw<- rowMeans(
  subset(kinshipstudy,
         select = c(RelBen_2, RelHap_2, RelCost_2, RelPrior_2)
  ))


#CommSpouse:

kinshipstudy$CommStrSpouse<- rowMeans(
  subset(kinshipstudy,
         select = c(RelBen_3, RelHap_3, RelCost_3, RelPrior_3)
  ))


#CommFriend

kinshipstudy$CommStrFriend<- rowMeans(
  subset(kinshipstudy,
         select = c( RelBen_6, RelHap_6, RelCost_6, RelPrior_6
         )
  ))


#CommAcq:

kinshipstudy$CommStrAcq<- rowMeans(
  subset(kinshipstudy,
         select = c( RelBen_7, RelHap_7, RelCost_7, RelPrior_7)
  ))


###Perceived support###
#SuppParents:ParSup_1, ParSup_2, ParSup_3

kinshipstudy$SuppPar<- rowMeans(
  subset(kinshipstudy,
         select = c(ParSup_1, ParSup_2, ParSup_3)
  ))

#SuppInlaws: 

kinshipstudy$SuppInLaws<- rowMeans(
  subset(kinshipstudy,
         select = c(PPSup_1, PPSup_2, PPSup_3)
  ))

#SuppSpouse:

kinshipstudy$SuppSpouse<- rowMeans(
  subset(kinshipstudy,
         select = c(PartSup_1, PartSup_2, PartSup_3)
  ))

#SuppFriend:

kinshipstudy$SuppFriend<- rowMeans(
  subset(kinshipstudy,
         select = c(FriendSup_1, FriendSup_2, FriendSup_3)
  ))

#SuppAcq:

kinshipstudy$SuppAcq<- rowMeans(
  subset(kinshipstudy,
         select = c(AcqSup_1, AcqSup_2, AcqSup_3)
  ))


##Demographic Cleaning
#RelLength Variable
kinshipstudy$RelLength<-kinshipstudy$RelLength_Text_1+(kinshipstudy$RelLength_Text_2/12)


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

#Time help standarization - z-score
kinshipstudy$ParentsTime_Z<- as.numeric(scale(kinshipstudy$TimeHelp_1))
kinshipstudy$ParentsTime_Z <- as.vector(kinshipstudy$ParentsTime_Z)

kinshipstudy$InlawsTime_Z<- as.numeric(scale(kinshipstudy$TimeHelp_2))
kinshipstudy$InlawsTime_Z <- as.vector(kinshipstudy$InlawsTime_Z)

kinshipstudy$SpouseTime_Z<- as.numeric(scale(kinshipstudy$TimeHelp_3))
kinshipstudy$SpouseTime_Z <- as.vector(kinshipstudy$SpouseTime_Z)

kinshipstudy$FriendTime_Z<- as.numeric(scale(kinshipstudy$TimeHelp_6))
kinshipstudy$FriendTime_Z <- as.vector(kinshipstudy$FriendTime_Z)

kinshipstudy$AcqTime_Z<- as.numeric(scale(kinshipstudy$TimeHelp_7))
kinshipstudy$AcqTime_Z <- as.vector(kinshipstudy$AcqTime_Z)

mean(kinshipstudy$ParentsTime_Z, na.rm = T)
SD(kinshipstudy$ParentsTime_Z, na.rm = T)
# M = 4.985086e-17, SD = 1

mean(kinshipstudy$InlawsTime_Z, na.rm = T)
SD(kinshipstudy$InlawsTime_Z, na.rm = T)
# M = -1.128873e-16, SD = 1

mean(kinshipstudy$SpouseTime_Z, na.rm = T)
SD(kinshipstudy$SpouseTime_Z, na.rm = T)
# M = -8.902712e-17, SD = 1


mean(kinshipstudy$FriendTime_Z, na.rm = T)
SD(kinshipstudy$FriendTime_Z, na.rm = T)
# M = -4.266008e-17, SD = 1

mean(kinshipstudy$AcqTime_Z, na.rm = T)
SD(kinshipstudy$AcqTime_Z, na.rm = T)
# M =  9.379609e-18, SD = 1

#MoneyHelp Standardization - Z-Score
kinshipstudy$ParentsMoney_Z<- as.numeric(scale(kinshipstudy$MoneyHelp_1))
kinshipstudy$ParentsMoney_Z<- as.vector(kinshipstudy$ParentsMoney_Z)

kinshipstudy$InlawsMoney_Z<-as.numeric(scale(kinshipstudy$MoneyHelp_2))
kinshipstudy$InlawsMoney_Z<-as.vector(kinshipstudy$InlawsMoney_Z)

kinshipstudy$SpouseMoney_Z<-as.numeric(scale(kinshipstudy$MoneyHelp_3))
kinshipstudy$SpouseMoney_Z<-as.vector(kinshipstudy$SpouseMoney_Z)

kinshipstudy$FriendMoney_Z<-as.numeric(scale(kinshipstudy$MoneyHelp_6))
kinshipstudy$FriendMoney_Z<-as.vector(kinshipstudy$FriendMoney_Z)

kinshipstudy$AcqMoney_Z<-as.numeric(scale(kinshipstudy$MoneyHelp_7))
kinshipstudy$AcqMoney_Z<-as.vector(kinshipstudy$AcqMoney_Z)

mean(kinshipstudy$ParentsMoney_Z, na.rm = T)
SD(kinshipstudy$ParentsMoney_Z, na.rm = T)
# M = 1.166289e-16, SD = 1

mean(kinshipstudy$InlawsMoney_Z, na.rm = T)
SD(kinshipstudy$InlawsMoney_Z, na.rm = T)
# M = -3.406267e-17, SD = 1

mean(kinshipstudy$SpouseMoney_Z, na.rm = T)
SD(kinshipstudy$SpouseMoney_Z, na.rm = T)
# M = -4.152746e-18, SD = 1

mean(kinshipstudy$FriendMoney_Z, na.rm = T)
SD(kinshipstudy$FriendMoney_Z, na.rm = T)
# M = 5.037418e-17, SD = 1

mean(kinshipstudy$AcqMoney_Z, na.rm = T)
SD(kinshipstudy$AcqMoney_Z, na.rm = T)
# M = -4.515677e-17, SD = 1


##Saving new dataset
write_csv(kinshipstudy, "kinship_cleaned_3.8.2023.csv")
