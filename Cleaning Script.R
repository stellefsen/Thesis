#Honors Thesis

#Load in relevant packages
#install.packages("Package") if don't have it yet
#Example: install.packages("tidyverse")
library(tidyverse)
library(psych)

#Import dataset
kinship_pilot <- read_csv("KinshipStudyFinal.csv")

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
