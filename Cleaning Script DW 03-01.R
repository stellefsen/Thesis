#Honors Thesis

#Load in relevant packages
#install.packages("Package") if don't have it yet
#Example: install.packages("tidyverse")
library(tidyverse)
library(psych)

#Import dataset
kinship_pilot <- read_csv("KinshipStudyFinal2.csv")
#To test race composite
kinship_test <- read_csv("KinshipStudyFinal2.csv")

#Composites
#Resource allocation: z-score to standardize time and money scenarios
#Cultural orientation
#Confounds by relationship
#Closeness
#Communal strength
#Perceived support

#dataset$composite_name <- rowMeans(
# subset(dataset,
#        select = c(list each item in composite)
#   )
#)

#Cultural orientation
#Individualism: IndCol_1, IndCol_3, IndCol_4, IndCol_7, IndCol_10

kinship_pilot$Ind <- rowMeans(
  subset(kinship_pilot,
         select = c(IndCol_1, IndCol_3, IndCol_4, IndCol_7, IndCol_10)
  )
)

#See first 20 entries for Ind
kinship_pilot$Ind %>% head(20)
##head is first n values, in this case, first 20 values of individualism variable
##%>% means values in dataset 

#Collectivism: IndCol_2, IndCol_5, IndCol_6, IndCol_8, IndCol_9, IndCol_11, IndCol_12

#Confounds by relationship
#ConfoundParents: RelDist_1, RelCont_1, RelTalk_1, RelPhone_1
#ConfoundInlaws: RelDist_2, RelCont_2, RelTalk_2, RelPhone_2
#ConfoundSpouse: etc.
#Etc.

#Closeness
#CloseParents: ParClose_1, ParClose_2
#CloseInlaws: PPClose_1, PPClose_2

#Communal Strength
#CommParents: RelBen_1, RelHap_1, etc.

#Perceived support
#SuppParents:ParSup_1, ParSup_2, ParSup_3
#SuppInlaws: etc.

#Saving updated dataset
write_csv(kinship_pilot, "kinship_cleaned_date.csv")



#Reverse coding just in case
#dataset$new_var_name <- (Max value) - dataset$original_var_name
#If score range is neg to pos numbers, 0 - original value

#Likert scale: -3 to +3

#Extraversion
study1b$ExtB5_1R <- 0 - study1b$ExtB5_1

#Agreeableness
study1b$AgreeB5_7R <- 0 - study1b$AgreeB5_7

#Conscientiousness
study1b$ConsB5_3R <- 0 - study1b$ConsB5_3

#Neuroticism
study1b$NeurB5_4R <- 0 - study1b$NeurB5_4

#Openess
study1b$OpenB5_5R <- 0 - study1b$OpenB5_5

#Comparing original w/ reversed to make sure reversed correctly
study1b %>% 
  select(ExtB5_1, ExtB5_1R, AgreeB5_7, AgreeB5_7R) %>% 
  head(10)

#Likert Scale: 0 to 5
#0 --> 5
#1 --> 4
#2 --> 3

#abs(5 - original variable)

#Race data cleaning
#Create overall race variable
kinship_test$Race <- NA

kinship_test["RaceEth_1"][is.na(kinship_test["RaceEth_1"])] <- 0
kinship_test["RaceEth_2"][is.na(kinship_test["RaceEth_2"])] <- 0
kinship_test["RaceEth_3"][is.na(kinship_test["RaceEth_3"])] <- 0
kinship_test["RaceEth_4"][is.na(kinship_test["RaceEth_4"])] <- 0
kinship_test["RaceEth_5"][is.na(kinship_test["RaceEth_5"])] <- 0
kinship_test["RaceEth_6"][is.na(kinship_test["RaceEth_6"])] <- 0
kinship_test["RaceEth_7"][is.na(kinship_test["RaceEth_7"])] <- 0
kinship_test["RaceEth_8"][is.na(kinship_test["RaceEth_8"])] <- 0

for (i in 1:nrow(kinship_test)) { #For each row
  if ((kinship_test$RaceEth_1[i] + kinship_test$RaceEth_2[i] + kinship_test$RaceEth_3[i] +
       kinship_test$RaceEth_4[i] + kinship_test$RaceEth_5[i] + kinship_test$RaceEth_6[i] +
       kinship_test$RaceEth_7[i]) > 1) { #If more than one race category is checked off
    kinship_test$Race[i] <- 8 #8 = Multiracial
  } else if (kinship_test$RaceEth_1[i] == 1) { #Checked RaceEth_1 (White)
    kinship_test$Race[i] <- 1 #1 = White
  } else if (kinship_test$RaceEth_2[i] == 1) {
    kinship_test$Race[i] <- 2 #2 = Black
  } else if (kinship_test$RaceEth_3[i] == 1) {
    kinship_test$Race[i] <- 3 #3 = Native American/Alaskan Native
  } else if (kinship_test$RaceEth_4[i] == 1) {
    kinship_test$Race[i] <- 4 #4 = Latino/a/x
  } else if (kinship_test$RaceEth_5[i] == 1) {
    kinship_test$Race[i] <- 5 #5 = Asian
  } else if (kinship_test$RaceEth_6[i] == 1) {
    kinship_test$Race[i] <- 6 #6 = Native Hawaiian
  } else if (kinship_test$RaceEth_7[i] == 1) {
    kinship_test$Race[i] <- 7 #7 = Other
  } else { #For whatever reason, it's all blank or don't want to say
    kinship_test$Race[i] <- NA #Leave Race blank so won't count towards totals
  }}


#Standardizing helping scenarios
#https://statisticsglobe.com/z-score-in-r

#TimeHelp_1 (Parents i think)
kinship_test$Time_Help1_z <- scale(kinship_test$TimeHelp_1)

mean(kinship_test$Time_Help1_z, na.rm = T)
SD(kinship_test$Time_Help1_z, na.rm = T)

#MoneyHelp_1
kinship_test$MoneyHelp1_z <- scale(kinship_test$MoneyHelp_1)

mean(kinship_test$MoneyHelp1_z, na.rm = T)
sd(kinship_test$MoneyHelp1_z, na.rm = T)
