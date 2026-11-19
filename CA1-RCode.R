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


######### Cleaning ########

#Rename columns to short names and fixing typo
#s1 = 1st semester, s2 = 2nd semester
names(df) <- c("marital_status", "app_mode", "app_order", "course",
               "attendance", "prev_qual", "prev_qual_grade", "nationality",
               "mother_qual", "father_qual", "mother_occ", "father_occ",
               "admission_grade", "displaced", "special_needs", "debtor",
               "fees_up_to_date", "gender", "scholarship", "age",
               "international",
               "s1_credited", "s1_enrolled", "s1_evaluations",
               "s1_approved", "s1_grade", "s1_without_eval",
               "s2_credited", "s2_enrolled", "s2_evaluations",
               "s2_approved", "s2_grade", "s2_without_eval",
               "unemployment", "inflation", "gdp", "target")
names(df)

#Hidden missing values -> NA
#Grade 0 = nothing to average / code 34 = unknow
df <- df%>%
  mutate(s1_grade = ifelse(s1_grade == 0, NA, s1_grade),
        s2_grade = ifelse(s2_grade == 0, NA, s2_grade),
        mother_qual = ifelse(mother_qual == 34, NA, mother_qual),
        father_qual = ifelse(father_qual == 34, NA, father_qual))
    
#Count and plot the missing values now visible
colSums(is.na(df))
na_percent <- round(colSums(is.na(df)) / nrow(df) * 100, 2)
na_df <- data.frame(Feature = names(na_percent), Percent = na_percent)
na_df <- subset(na_df, Percent > 0)
ggplot(na_df, aes(x = reorder(Feature, Percent), y = Percent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Hidden Missing Values Revealed (% of students)",
       x = "Column", y = "% missing") +
  theme_minimal()

#Flag variables before imputing
df$no_grade_1st <- ifelse(is.na(df$s1_grade), 1, 0)
df$no_grade_2nd <- ifelse(is.na(df$s2_grade), 1, 0)
table(df$no_grade_1st, df$target)

#Selecting only columns I will use
#Dropped: nationality, international (97% Portuguese), special_needs (1%), 
#app_mode, app_order, prev_qual (not gonna use it)
#mother_occ, father_occ (small groups, use qualification)
df <- df[c("target", "gender", "age", "marital_status", "course",
           "attendance", "displaced", "debtor", "fees_up_to_date",
           "scholarship", "mother_qual", "father_qual",
           "prev_qual_grade", "admission_grade",
           "s1_credited", "s1_enrolled", "s1_evaluations",
           "s1_approved", "s1_grade", "s1_without_eval",
           "s2_credited", "s2_enrolled", "s2_evaluations",
           "s2_approved", "s2_grade", "s2_without_eval",
           "no_grade_1st", "no_grade_2nd",
           "unemployment", "inflation", "gdp")]
dim(df)
 
#Code -> Labels (UCI table)
df$gender <- factor(df$gender, levels = c(0, 1), labels = c("Female", "Male"))
df$attendance <- factor(df$attendance, levels = c(0, 1), labels = c("Evening", "Daytime"))
df$displaced <- factor(df$displaced, levels = c(0, 1), labels = c("No", "Yes"))
df$debtor <- factor(df$debtor, levels = c(0, 1), labels = c("No", "Yes"))
df$fees_up_to_date <- factor(df$fees_up_to_date, levels = c(0, 1), labels = c("No", "Yes"))
df$scholarship <- factor(df$scholarship, levels = c(0, 1), labels = c("No", "Yes"))
#Marital status: small groups merged
df$marital_status <- factor(df$marital_status,
                            levels = c(1, 2, 3, 4, 5, 6),
                            labels = c("Single", "Married/Union", "Separated/Widowed",
                                       "Separated/Widowed", "Married/Union",
                                       "Separated/Widowed"))
#Course: 17 codes -> names
df$course <- factor(df$course,
                    levels = c(33, 171, 8014, 9003, 9070, 9085, 9119, 9130, 9147,
                               9238, 9254, 9500, 9556, 9670, 9773, 9853, 9991),
                    labels = c("Biofuel Production Tech", "Animation & Multimedia",
                               "Social Service (evening)", "Agronomy",
                               "Communication Design", "Veterinary Nursing",
                               "Informatics Engineering", "Equinculture",
                               "Management", "Social Service", "Tourism",
                               "Nursing", "Oral Hygiene", "Advertising & Marketing",
                               "Journalism & Communication", "Basic Education",
                               "Management (evening)"))

#Parents qualification: 33 codes -> 3 levels (Basic < Secondary < Higher)
qual_codes  <- c(11, 19, 26, 29, 30, 35, 36, 37, 38,
                 1, 9, 10, 12, 13, 14, 18, 20, 22, 25, 27, 31, 33,
                 2, 3, 4, 5, 6, 39, 40, 41, 42, 43, 44)
qual_labels <- c(rep("Basic", 9), rep("Secondary", 13), rep("Higher", 11))
df$mother_edu <- factor(df$mother_qual, levels = qual_codes, labels = qual_labels)
df$father_edu <- factor(df$father_qual, levels = qual_codes, labels = qual_labels)
df$mother_qual <- NULL
df$father_qual <- NULL

table(df$course)
table(df$mother_edu)

#Impute grades with the median
#Median, not mean to not pull by extreme values
df <- df %>%
  mutate(s1_grade = ifelse(is.na(s1_grade), median(s1_grade, na.rm = TRUE), s1_grade)) %>%
  mutate(s2_grade = ifelse(is.na(s2_grade), median(s2_grade, na.rm = TRUE), s2_grade))

#Impute parents education with the mode
get_mode <- function(x) {
  values <- na.omit(x)
  unique_values <- unique(values)
  unique_values[which.max(tabulate(match(values, unique_values)))]
}
df$mother_edu[is.na(df$mother_edu)] <- get_mode(df$mother_edu)
df$father_edu[is.na(df$father_edu)] <- get_mode(df$father_edu)

#Check values
colSums(is.na(df))
dim(df)
str(df)

#Before and after imputation
p1 <- ggplot(df_raw, aes(x = Curricular.units.1st.sem..grade.)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "1st-sem grade - original (0 = no grade)", x = "Grade (0-20)")
p2 <- ggplot(df, aes(x = s1_grade)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black") +
  labs(title = "1st-sem grade - after median imputation", x = "Grade (0-20)")
grid.arrange(p1, p2, ncol = 2)

#Save
write.csv(df, "data_clean.csv", row.names = FALSE)

####### Outliers #####
num_cols <- c("age", "prev_qual_grade", "admission_grade",
              "s1_credited", "s1_enrolled", "s1_evaluations",
              "s1_approved", "s1_grade", "s1_without_eval",
              "s2_credited", "s2_enrolled", "s2_evaluations",
              "s2_approved", "s2_grade", "s2_without_eval")

#Boxplots - scale() to put all columns on the same axis
boxplot(scale(df[num_cols]), main = "Outliers in Numeric Columns", col = "lightblue")
boxplot(df$age, main = "Age at Enrolment")

#Count outliers per column
#IQR: below Q1 -1.5*IQR or above Q3 + 1.5*IQR
count_iqr_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- Q3 - Q1
  sum(x < Q1 - 1.5 * IQR_value | x > Q3 + 1.5 * IQR_value)
}

#Z-score: z below -3 or above 3
count_z_outliers <- function(x){
  z <- scale(x)
  sum(z < -3 | z > 3)
}

#apply(data, 2, function)
outlier_summary <- data.frame(
  IQR_outliers = apply(df[num_cols], 2, count_iqr_outliers),
  Z_outliers = apply(df[num_cols], 2, count_z_outliers))
outlier_summary$IQR_percent <- round(outlier_summary$IQR_outliers / nrow(df) * 100, 2)
outlier_summary

#Check for impossible values
nrow(filter(df, s1_approved > s1_enrolled))
nrow(filter(df, s2_approved > s2_enrolled))
oldest <- filter(df, age > 50)
nrow(oldest)
table(oldest$target)

#remove outliers to check data
Q1 <- quantile(df$age, 0.25)
Q3 <- quantile(df$age, 0.75)
IQR_value <- Q3 - Q1
lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value
upper
removed <- df[df$age < lower | df$age > upper, ]
nrow(df) - nrow(removed)
prop.table(table(removed$target))
prop.table(table(df$target))

#The 441 age outliers represent real students that drop out more often (54% vs 32%)
#Removing them would delete the group that we want to understand

#Keep the outliers, but reduce skew with log
df$age_log <- log10(df$age)

p1 <- ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "orange", color = "black") +
  labs(title = "Age - original", x = "Age (years)")
p2 <- ggplot(df, aes(x = age_log)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  labs(title = "Age - log10", x = "log10(age)")
grid.arrange(p1, p2, ncol = 2)

dim(df)
#Nothing was removed
write.csv(df, "data_outliers.csv", row.names = FALSE)


######### Exploratory Data Analysis: The Student Journey ########
#Finding out the Dropout, Enrolled and Graduate % in different circumstances
#Outcomes are unbalanced (32/12/50%) so comparing using rates
##Questions##
#1. Does dropout rate differ between male and female?
#2. Is parents educational level related to dropout?
#3. Are tuition fee status and scholarships related to dropout?
#4. Does age of enrollment related to dropout?
#5. How do 1st and 2nd semester grades relate, and do dropouts cluster at low grades?

# Does the Enrolled students are more similar to the Graduate or to the Dropouts?

#Outcome#
ggplot(df, aes(x = target)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Academic Outcome of Students", x = "Outcome", y = "Number of students") +
  theme_minimal()

#Age at enrollment#
ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 2, color = "black", fill = "green") +
  labs(title = "Age at Enrolment", x = "Age (years)", y = "Number of students") +
  theme_minimal()
df %>% count(cut_width(age, 5))

#Gender vs Outcome#
ggplot(df, aes(x = gender, fill = target)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Q1. Outcome by Gender", x = "Gender", y = "Number of students") +
  theme_minimal()
df %>%
  group_by(gender) %>%
  summarise(dropout = round(mean(target == "Dropout") * 100, 1),
            enrolled = round(mean(target == "Enrolled") * 100, 1),
            graduate = round(mean(target == "Graduate") * 100, 1),
            n = n())

#Parents education vs outcome#
#mother
ggplot(df, aes(x = mother_edu, fill = target)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Q2a. Outcome by Mother's Education", x = "Mother's education", y = "Number of students") +
  theme_minimal()
df %>%
  group_by(mother_edu) %>%
  summarise(dropout = round(mean(target == "Dropout") * 100, 1),
            graduate = round(mean(target == "Graduate") * 100, 1),
            n = n())
#father
ggplot(df, aes(x = father_edu, fill = target)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Q2b. Outcome by Father's Education", x = "Father's education", y = "Number of students") +
  theme_minimal()
df %>%
  group_by(father_edu) %>%
  summarise(dropout = round(mean(target == "Dropout") * 100, 1),
            graduate = round(mean(target == "Graduate") * 100, 1),
            n = n())
#mother and father side by side
p1 <- ggplot(df, aes(x = mother_edu, fill = target)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Mother's Education", x = "Education level", y = "Number of students") +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- ggplot(df, aes(x = father_edu, fill = target)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Father's Education", x = "Education level", y = "Number of students") +
  theme_minimal() +
  theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 2)

#mother and father in one graph (using melt())
parents <- melt(df[c("target", "mother_edu", "father_edu")],
                id.vars       = "target",
                measure.vars  = c("mother_edu", "father_edu"),
                variable.name = "parent",
                value.name    = "edu_level")
head(parents)

parents$edu_level <- factor(parents$edu_level, levels = c("Basic", "Secondary", "Higher"))
parents$parent <- factor(parents$parent, levels = c("mother_edu", "father_edu"), labels = c("Mother", "Father"))
parent_rates <- parents %>%
  group_by(parent, edu_level) %>%
  summarise(dropout = round(mean(target == "Dropout") * 100, 1), n = n())
parent_rates

ggplot(parent_rates, aes(x = edu_level, y = dropout, fill = parent)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Q2. Dropout Rate by Parents' Education",
       x = "Education level", y = "Dropout rate (%)", fill = "Parent") +
  theme_minimal()