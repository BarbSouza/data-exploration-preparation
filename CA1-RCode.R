#CA1

#Install the packages:

#tidyverse: dplyr (data manipulation) + ggplot2 (plots) + readr
#gridExtra:  (plots side by side)
#reshape2: Change table's shape
#fastDummies:  0/1
#factoextra: PCA plots
#FactoMineR: PCA package used with factoextra

#install.packages(c("tidyverse", "gridExtra", "reshape2", "fastDummies", "factoextra", "FactoMineR"))

#load packages
library(tidyverse)
library(gridExtra)
library(reshape2)
library(fastDummies)
library(factoextra)
library(FactoMineR)

#Load data
df <- read.csv("data.csv", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE)
#data is separated by ";", c("", "NA") for missing values

#dataset size
dim(df)

#backup copy of original dataset
df_raw <- df


#####Understanding the data#####

#dataset info
names(df)
#Columns names

str(df)
# 1 chr column (Target)
# 7 num columns (Grades: qualification, admission, 1st and 2nd semester; Rates: unemployment, inflation; GDP)
# 29 int columns (all rest)
  #Category codes
    #Marital.status
    #Application.mode
    #Application.order #ordinal
    #Course
    #Previous.qualification
    #Nacionality (typo in the original dataset, should be "Nationality")
    #Mother.s.qualification                         
    #Father.s.qualification                         
    #Mother.s.occupation                            
    #Father.s.occupation

  #Binary (0/1)
    #Displaced
    #Educational.special.needs
    #Educational.special.needs                     
    #Debtor                                         
    #Tuition.fees.up.to.date                        
    #Gender                                         
    #Scholarship.holder
    #International
    #Daytime.evening.attendance

  #Quantities
    #Age.at.enrollment
    #Curricular.units.1st.sem..enrolled.            
    #Curricular.units.1st.sem..evaluations.         
    #Curricular.units.1st.sem..approved.    
    #Curricular.units.1nd.sem..credited. 
    #Curricular.units.1st.sem..without.evaluations. 
    #Curricular.units.2nd.sem..enrolled.            
    #Curricular.units.2nd.sem..evaluations.         
    #Curricular.units.2nd.sem..approved. 
    #Curricular.units.2nd.sem..without.evaluations


glimpse(df)
#All num are dbl

head(df)

#GDP, unemployment, and inflation describes the economy of  Portugal, not the student
table(df$GDP) #GDP has only 10 distinct values (one per enrollment year)
#This could answer if the economic situation reflects on the student

#Data Information
summary(df)
#Category codes: summary is meaningless
#Application order should start at 1, maybe missing value = 0

#Quantitative
#Age: Goes from 17 to 70, the mean is 23 and median 20 so the higher ages pull the mean up

#Grades: Semester 1: from 0 to 18.88 / Semester 2: 0 to 18.57
#Semester 1: the median 12.29 and mean 10.64 / Semester 2: the median 12.20 and mean 10.23
#Row 1 and 3 show  students with 0 units approved
#0 = missing value: the grade is average of the approved; with 0 there is nothing to average
#probably the missing values are pulling the mean down

#Binary
#Mean of a 0/1 column = proportion of 1s
#Gender: Around 35% male

#Attendance time: 89% daytime

#Displaced: 55% Yes

#Debtor: 11% yes

#Tuition fees up to date: 88% yes

#Scholarship: 25% Yes

#International and Educational special needs students are very little (110 students (2%) and 51 students (1%))


######Frequency Tables#########

#Target:
table(df$Target)
prop.table(table(df$Target))
#Dropout: 1421 (32%) / Enrolled: 794 (18%) / Graduate: 2209 (50%)
#Not balanced - compare rates between groups not counts

#Gender:
table(df$Gender)
prop.table(table(df$Gender))
#Female: 2868 (65%) / Male: 1556 (35%)


##### Missing Values and duplicated ######

colSums(is.na(df))  #NAs per column
sum(is.na(df))      #total NAs
sum(duplicated(df))  #duplicated rows
#All 0, no visible missing value

#Hidden missing values
nrow(filter(df, Curricular.units.1st.sem..grade. == 0)) #718
nrow(filter(df, Curricular.units.2nd.sem..grade. == 0)) #870 - more students, maybe dropping out after first semester
nrow(filter(df, Curricular.units.1st.sem..grade. == 0 & Curricular.units.1st.sem..approved. == 0)) #718 - same students, so 0 = no grade
nrow(filter(df, Curricular.units.2nd.sem..grade. == 0 & Curricular.units.2nd.sem..approved. == 0)) #870
nrow(filter(df, Mother.s.qualification == 34)) #130 unknown
nrow(filter(df, Father.s.qualification == 34)) #112 unknown
nrow(filter(df, Application.order == 0)) #Just one, doesn't matter


    

