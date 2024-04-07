##### packages ##### 

# for descriptive analytics
install.packages("foreign")
library(foreign) #to read XPT files with read.xport()

install.packages("gtools") #for macro
library(gtools)
library(dplyr)

install.packages("questionr") #for weightening 
library(questionr)

install.packages("MASS") # for categorical bivariate analysis - Chi square and exact Fisher's test

#for regression analysis
install.packages("htmltools")
install.packages("devtools") #output model to CSV
install.packages("broom") #clean up models to output to CSV
install.packages("arm") #makes plots
install.packages("gvlma") #makes plots
install.packages("nlme") #outputs additional estimates

##### DESCRIPTIVE ANALYSIS START #####

setwd("/Users/viktoriazajceva/Desktop/R/linkedin healthcare analytics/descriptive/Exercise Files")
BRFSS_a <- read.xport("LLCP2014.XPT ")

colnames(BRFSS_a)

#
BRFSSVarList <- c("VETERAN3", 
                  "ALCDAY5",
                  "SLEPTIM1",
                  "ASTHMA3",
                  "X_AGE_G",
                  "SMOKE100",
                  "SMOKDAY2",
                  "SEX",
                  "X_HISPANC",
                  "X_MRACE1",
                  "MARITAL",
                  "GENHLTH",
                  "HLTHPLN1",
                  "EDUCA",
                  "INCOME2",
                  "X_BMI5CAT",
                  "EXERANY2")

#subset by varlist, датафрейм с интересными нам столбцами
BRFSS_b <- BRFSS_a[BRFSSVarList]

#check columns
colnames(BRFSS_b)

#check rows
nrow(BRFSS_b)


##### Exclusions ##### 
#фильтрую по рядам, чтобы выбрать только тех, кто в моей 
#subpopulation (veterans), и у кого есть valid measurements of the 
#exposure (alcohol drinking) and 
#the outcomes (sleep duration and asthma status)

#subset the dataset for only veterans
BRFSS_c <- subset(BRFSS_b, VETERAN3==1)
?subset
#make sure the variable looks right
BRFSS_c$VETERAN3

#Figure out how many rows are in resulting dataset
nrow(BRFSS_c)

#only keep rows with with valid alcohol/exposure variable
BRFSS_d <- subset(BRFSS_c, ALCDAY5 < 777 | ALCDAY5==888) #999 означает нет ответа, поэтому 999 не включаем

#make sure variable looks right
BRFSS_d$ALCDAY5

#count rows in new dataset
nrow(BRFSS_d)

#only keep rows with valid sleep data
BRFSS_e <- subset(BRFSS_d, SLEPTIM1 < 77) #77, 99 - нет валидных ответов (отказались отвечать, не знают)

#count rows in the new dataset
nrow(BRFSS_e)

#only keep rows with valid asthma data
BRFSS_f <- subset(BRFSS_e, ASTHMA3 < 7) #77, 99 - нет валидных ответов (отказались отвечать, не знают)

#count rows in the new dataset
nrow(BRFSS_f)

##### Generating EXPOSURE - alcohol drinking variables #####
#Add indicator variable for veterans

#Make a copy of the dataset
BRFSS_g <- BRFSS_f

#create the categorical variable set to 9 to the dataset (9 видимо просто число для создания столбца)
BRFSS_g$ALCGRP <- 9

#Update according to the data dictionary - здесь уже применяем условия, 9 уходят
BRFSS_g$ALCGRP[BRFSS_g$ALCDAY5 <200 ] <- 3
BRFSS_g$ALCGRP[BRFSS_g$ALCDAY5 >=200 & BRFSS_g$ALCDAY5 <777] <- 2
BRFSS_g$ALCGRP[BRFSS_g$ALCDAY5 == 888] <- 1

#Check variable

table(BRFSS_g$ALCGRP, BRFSS_g$ALCDAY5)
#The value at row 2 and column 201 indicates that there were 4353 occurrences where the ALCGRP was 2 and the ALCDAY5 was 201.

#Add flags (binary variables) - drinking monthly and drinking weekly

BRFSS_g$DRKMONTHLY <- 0
BRFSS_g$DRKMONTHLY[BRFSS_g$ALCGRP == 2] <- 1

table(BRFSS_g$ALCGRP, BRFSS_g$DRKMONTHLY)

BRFSS_g$DRKWEEKLY <- 0
BRFSS_g$DRKWEEKLY [BRFSS_g$ALCGRP == 1] <- 1

table(BRFSS_g$ALCGRP, BRFSS_g$DRKWEEKLY)

##### Generating OUTCOME VARIABLES - sleep duration and asthma status ##### 
#we didn't really need to do it because we already removed invalid values

#Make a copy of the dataset
BRFSS_h <- BRFSS_g

#Make and test sleep variable (continuous variable)

BRFSS_h$SLEPTIM2 <- NA
BRFSS_h$SLEPTIM2[!is.na(BRFSS_h$SLEPTIM1) & BRFSS_h$SLEPTIM1 !=77
                 & BRFSS_h$SLEPTIM1 !=99] <- BRFSS_h$SLEPTIM1

table(BRFSS_h$SLEPTIM1, BRFSS_h$SLEPTIM2)

#Make and test asthma variable

BRFSS_h$ASTHMA4 <- 9
BRFSS_h$ASTHMA4[BRFSS_h$ASTHMA3 == 1] <- 1
BRFSS_h$ASTHMA4[BRFSS_h$ASTHMA3 == 2] <- 0

table(BRFSS_h$ASTHMA3, BRFSS_h$ASTHMA4)

##### Generating the AGE variables ##### 

#Make a copy of the dataset
BRFSS_i <- BRFSS_h

#Add and check age variables

BRFSS_i$AGE2 <- 0
BRFSS_i$AGE3 <- 0
BRFSS_i$AGE4 <- 0
BRFSS_i$AGE5 <- 0
BRFSS_i$AGE6 <- 0

BRFSS_i$AGE2[BRFSS_i$X_AGE_G == 2] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE2)

BRFSS_i$AGE3[BRFSS_i$X_AGE_G == 3] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE3)

BRFSS_i$AGE4[BRFSS_i$X_AGE_G == 4] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE4)

BRFSS_i$AGE5[BRFSS_i$X_AGE_G == 5] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE5)

BRFSS_i$AGE6[BRFSS_i$X_AGE_G == 6] <- 1
table(BRFSS_i$X_AGE_G, BRFSS_i$AGE6)

##### Generating the SMOKING variables ##### 

BRFSS_i$NEVERSMK <- 0
BRFSS_i$NEVERSMK [BRFSS_i$SMOKE100 == 2] <- 1
table(BRFSS_i$SMOKE100, BRFSS_i$NEVERSMK)

BRFSS_i$SMOKGRP <- 9
BRFSS_i$SMOKGRP[BRFSS_i$SMOKDAY2 == 1 | BRFSS_i$SMOKDAY2 == 2] <- 1
BRFSS_i$SMOKGRP[BRFSS_i$SMOKDAY2 == 3 | BRFSS_i$NEVERSMK == 1] <- 2

table(BRFSS_i$SMOKGRP, BRFSS_i$SMOKDAY2)
table(BRFSS_i$SMOKGRP, BRFSS_i$SMOKE100)

BRFSS_i$SMOKER <- 0
BRFSS_i$SMOKER[BRFSS_i$SMOKGRP == 1] <- 1

table(BRFSS_i$SMOKGRP, BRFSS_i$SMOKER)

##### Generating the SEX variables ##### 

BRFSS_i$MALE <- 0
BRFSS_i$MALE[BRFSS_i$SEX == 1] <- 1

table(BRFSS_i$MALE, BRFSS_i$SEX)

##### Generating OTHER CATEGORICAL variables ##### 
#make Hispanic variable

BRFSS_i$HISPANIC <- 0
BRFSS_i$HISPANIC[BRFSS_i$X_HISPANC == 1] <- 1

table(BRFSS_i$HISPANIC, BRFSS_i$X_HISPANC)

#make race variables

BRFSS_i$RACEGRP <- 9
BRFSS_i$RACEGRP[BRFSS_i$X_MRACE1 == 1] <- 1
BRFSS_i$RACEGRP[BRFSS_i$X_MRACE1 == 2] <- 2
BRFSS_i$RACEGRP[BRFSS_i$X_MRACE1 == 3] <- 3
BRFSS_i$RACEGRP[BRFSS_i$X_MRACE1 == 4] <- 4
BRFSS_i$RACEGRP[BRFSS_i$X_MRACE1 == 5] <- 5
BRFSS_i$RACEGRP[BRFSS_i$X_MRACE1 == 6 | BRFSS_i$X_MRACE1 == 7] <- 6

table(BRFSS_i$RACEGRP , BRFSS_i$X_MRACE1)

BRFSS_i$BLACK <- 0
BRFSS_i$ASIAN <- 0
BRFSS_i$OTHRACE <- 0

BRFSS_i$BLACK[BRFSS_i$RACEGRP == 2] <- 1
table(BRFSS_i$RACEGRP, BRFSS_i$BLACK)

BRFSS_i$ASIAN[BRFSS_i$RACEGRP == 4] <- 1
table(BRFSS_i$RACEGRP, BRFSS_i$ASIAN)

BRFSS_i$OTHRACE[BRFSS_i$RACEGRP == 3 | BRFSS_i$RACEGRP == 5 | BRFSS_i$RACEGRP == 6 | BRFSS_i$RACEGRP == 7] <- 1
table(BRFSS_i$RACEGRP, BRFSS_i$OTHRACE)

#make marital variables

BRFSS_i$MARGRP <- 9
BRFSS_i$MARGRP[BRFSS_i$MARITAL == 1 | BRFSS_i$MARITAL == 5] <- 1
BRFSS_i$MARGRP[BRFSS_i$MARITAL == 2 | BRFSS_i$MARITAL == 3 ] <- 2
BRFSS_i$MARGRP[BRFSS_i$MARITAL == 4] <- 3

table(BRFSS_i$MARGRP, BRFSS_i$MARITAL)

BRFSS_i$NEVERMAR <- 0
BRFSS_i$FORMERMAR <- 0

BRFSS_i$NEVERMAR[BRFSS_i$MARGRP == 3] <- 1
table(BRFSS_i$MARGRP, BRFSS_i$NEVERMAR)

BRFSS_i$FORMERMAR[BRFSS_i$MARGRP == 2] <- 1
table(BRFSS_i$MARGRP, BRFSS_i$FORMERMAR)

#Make Genhealth variables

BRFSS_i$GENHLTH2 <- 9
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 1] <- 1
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 2] <- 2
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 3] <- 3
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 4] <- 4
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 5] <- 5

table(BRFSS_i$GENHLTH2, BRFSS_i$GENHLTH)

BRFSS_i$FAIRHLTH <- 0
BRFSS_i$POORHLTH <- 0

BRFSS_i$FAIRHLTH [BRFSS_i$GENHLTH2 == 4] <- 1
table(BRFSS_i$FAIRHLTH, BRFSS_i$GENHLTH2)

BRFSS_i$POORHLTH [BRFSS_i$GENHLTH2 == 5] <- 1
table(BRFSS_i$POORHLTH, BRFSS_i$GENHLTH2)

#Make health plan variables

BRFSS_i$HLTHPLN2 <- 9
BRFSS_i$HLTHPLN2[BRFSS_i$HLTHPLN1 == 1] <- 1
BRFSS_i$HLTHPLN2[BRFSS_i$HLTHPLN1 == 2] <- 2

table(BRFSS_i$HLTHPLN1, BRFSS_i$HLTHPLN2)

BRFSS_i$NOPLAN <- 0
BRFSS_i$NOPLAN [BRFSS_i$HLTHPLN2== 2] <- 1
table(BRFSS_i$NOPLAN, BRFSS_i$HLTHPLN2)

#Make education variables

BRFSS_i$EDGROUP <- 9
BRFSS_i$EDGROUP[BRFSS_i$EDUCA == 1 | BRFSS_i$EDUCA == 2 | BRFSS_i$EDUCA == 3] <- 1
BRFSS_i$EDGROUP[BRFSS_i$EDUCA == 4] <- 2
BRFSS_i$EDGROUP[BRFSS_i$EDUCA == 5] <- 3
BRFSS_i$EDGROUP[BRFSS_i$EDUCA == 6] <- 4

table(BRFSS_i$EDGROUP, BRFSS_i$EDUCA)

BRFSS_i$LOWED <- 0
BRFSS_i$SOMECOLL <- 0

BRFSS_i$LOWED[BRFSS_i$EDGROUP == 1 | BRFSS_i$EDGROUP == 2 ] <- 1
table(BRFSS_i$LOWED, BRFSS_i$EDGROUP)

BRFSS_i$SOMECOLL [BRFSS_i$EDGROUP == 3] <- 1
table(BRFSS_i$SOMECOLL, BRFSS_i$EDGROUP)

#Make income variables

BRFSS_i$INCOME3 <- BRFSS_i$INCOME2
BRFSS_i$INCOME3[BRFSS_i$INCOME2 >=77] <- 9

table(BRFSS_i$INCOME2, BRFSS_i$INCOME3)

BRFSS_i$INC1 <- 0
BRFSS_i$INC2 <- 0
BRFSS_i$INC3 <- 0
BRFSS_i$INC4 <- 0
BRFSS_i$INC5 <- 0
BRFSS_i$INC6 <- 0
BRFSS_i$INC7 <- 0

BRFSS_i$INC1[BRFSS_i$INCOME3 == 1] <- 1
table(BRFSS_i$INC1, BRFSS_i$INCOME3)

BRFSS_i$INC2[BRFSS_i$INCOME3 == 2] <- 1
table(BRFSS_i$INC2, BRFSS_i$INCOME3)

BRFSS_i$INC3[BRFSS_i$INCOME3 == 3] <- 1
table(BRFSS_i$INC3, BRFSS_i$INCOME3)

BRFSS_i$INC4[BRFSS_i$INCOME3 == 4] <- 1
table(BRFSS_i$INC4, BRFSS_i$INCOME3)

BRFSS_i$INC5[BRFSS_i$INCOME3 == 5] <- 1
table(BRFSS_i$INC5, BRFSS_i$INCOME3)

BRFSS_i$INC6[BRFSS_i$INCOME3 == 6] <- 1
table(BRFSS_i$INC6, BRFSS_i$INCOME3)

BRFSS_i$INC7[BRFSS_i$INCOME3 == 7] <- 1
table(BRFSS_i$INC7, BRFSS_i$INCOME3)

#Make BMI variables

BRFSS_i$BMICAT<- 9
BRFSS_i$BMICAT[BRFSS_i$X_BMI5CAT ==1] <- 1
BRFSS_i$BMICAT[BRFSS_i$X_BMI5CAT ==2] <- 2
BRFSS_i$BMICAT[BRFSS_i$X_BMI5CAT ==3] <- 3
BRFSS_i$BMICAT[BRFSS_i$X_BMI5CAT ==4] <- 4

table(BRFSS_i$BMICAT, BRFSS_i$X_BMI5CAT)

BRFSS_i$UNDWT <- 0
BRFSS_i$OVWT <- 0
BRFSS_i$OBESE <- 0

BRFSS_i$UNDWT[BRFSS_i$BMICAT== 1] <- 1
table(BRFSS_i$UNDWT, BRFSS_i$BMICAT)

BRFSS_i$OVWT[BRFSS_i$BMICAT== 3] <- 1
table(BRFSS_i$OVWT, BRFSS_i$BMICAT)

BRFSS_i$OBESE[BRFSS_i$BMICAT== 4] <- 1
table(BRFSS_i$OBESE, BRFSS_i$BMICAT)

#make exercise variables

BRFSS_i$EXERANY3<- 9
BRFSS_i$EXERANY3[BRFSS_i$EXERANY2 ==1] <- 1
BRFSS_i$EXERANY3[BRFSS_i$EXERANY2 ==2] <- 2

table(BRFSS_i$EXERANY3, BRFSS_i$EXERANY2)

BRFSS_i$NOEXER <- 0
BRFSS_i$NOEXER[BRFSS_i$EXERANY3 ==2] <- 1
table(BRFSS_i$NOEXER, BRFSS_i$EXERANY3)

nrow(BRFSS_i)

##### Write out "ANALYTIC" dataset in CSV##### 

write.csv(BRFSS_i, file = "analytic.csv")

#read in analytic table

analytic <- read.csv("analytic.csv", header=TRUE, sep=",")

##### univariate analysis - asthma ##### 

#Look at distribution of categorical outcome asthma

AsthmaFreq <- table(analytic$ASTHMA4)
AsthmaFreq
write.csv(AsthmaFreq, file = "AsthmaFreq.csv")

#what proportion of our dataset has ashtma?
PropAsthma <- 5343/52788
PropAsthma

##### bivatiate analysis - asthma and alcohol ##### 

#Look at categorical outcome asthma by exposure, ALCGRP

AsthmaAlcFreq <- table(analytic$ASTHMA4, analytic$ALCGRP)
AsthmaAlcFreq
write.csv(AsthmaAlcFreq, file = "AsthmaAlcFreq.csv")

##### visualise continious variable #####
#Look at distribution of sleep duration 

#summary statistics

summary(analytic$SLEPTIM2)

#look at histogram and box plot of total file

# Adjusting plot size
par(mfrow = c(1, 1), mar = c(3, 3, 2, 1) + 0.1) #The par() function here sets the number of rows and columns in the plot (mfrow = c(1, 1)) and adjusts the margins (mar = c(3, 3, 2, 1) + 0.1)

hist(
  analytic$SLEPTIM2,
  main = "Histogram of SLEPTIM2",
  xlab = "Class SLEPTIM2",
  ylab = "Frequency",
  xlim = c(0, 15),
  ylim = c(0, 20000),
  border = "red",
  col = "yellow",
  las = 1,
  breaks = 20
)


boxplot(analytic$SLEPTIM2, main="Box Plot of SLEPTIM2", 
  	xlab="Total File", ylab="SLEPTIM2")

#See box plots of groups next to each other

boxplot(SLEPTIM2~ALCGRP, data=analytic, main="Box Plot of SLEPTIM2 by ALCGRP", 
  	xlab="ALCGRP", ylab="SLEPTIM2")


##### table 1 shell #####

#categorical variable has: frequencies, %, bivatiate tests for categorical data, stratified
#continuous variable has: mean, SD, bivatiate tests for continuous data, stratified


##### Overall frequencies ##### 

#Start by making frequencies per category

AsthmaFreq <- table(analytic$ASTHMA4)
AsthmaFreq
write.csv(AsthmaFreq, file = "AsthmaFreq.csv")

AlcFreq <- table(analytic$ALCGRP)
AlcFreq 
write.csv(AlcFreq , file = "AlcFreq.csv")

##### FREQUENCY MACRO ##### 

#install package gtools
#then call up library

library(gtools)

#use defmacro to define the macro

FreqTbl <-defmacro(OutputTable, InputVar, CSVTable, 
                   expr={
                     OutputTable <- table(InputVar);
                     write.csv(OutputTable, file = paste0(CSVTable, ".csv"))
                   })

FreqTbl (AlcFreq, analytic$ALCGRP, "Alc")
FreqTbl (AgeFreq, analytic$X_AGE_G, "Age")
FreqTbl (SexFreq, analytic$SEX, "Sex")
FreqTbl (HispFreq, analytic$X_HISPANC, "Hisp")
FreqTbl (RaceFreq, analytic$RACEGRP, "Race")
FreqTbl (MaritalFreq, analytic$MARGRP, "Mar")
FreqTbl (EdFreq, analytic$EDGROUP, "Ed")
FreqTbl (IncFreq, analytic$INCOME3, "Inc")
FreqTbl (BMIFreq, analytic$BMICAT, "BMI")
FreqTbl (SmokeFreq, analytic$SMOKGRP, "Smok")
FreqTbl (ExerFreq, analytic$EXERANY3, "Exer")
FreqTbl (HlthPlanFreq, analytic$HLTHPLN2, "HlthPln")
FreqTbl (GenHlthFreq, analytic$GENHLTH2, "GenHlth")


##### Asthma frequencies ##### 
#categorical variable
#subset dataset with only asthma people

asthmaonly <- subset(analytic, ASTHMA4 == 1)
table(asthmaonly$ASTHMA4)
nrow(asthmaonly)

AsthmaFreq <- table(asthmaonly$ASTHMA4)
AsthmaFreq
write.csv(AsthmaFreq, file = "Asthma.csv")

#USING MACROS

#install package gtools
#then call up library

library(gtools)

#use defmacro to define the macro

FreqTbl <-defmacro(OutputTable, InputVar, CSVTable, 
                   expr={
                     OutputTable <- table(InputVar);
                     write.csv(OutputTable, file = paste0(CSVTable, ".csv"))
                   })

FreqTbl (AlcGrpFreq, asthmaonly$ALCGRP, "Alc")
FreqTbl (AgeGrpFreq, asthmaonly$X_AGE_G, "Age")
FreqTbl (SexFreq, asthmaonly$SEX, "Sex")
FreqTbl (HispFreq, asthmaonly$X_HISPANC, "Hisp")
FreqTbl (RaceFreq, asthmaonly$RACEGRP, "Race")
FreqTbl (MaritalFreq, asthmaonly$MARGRP, "Mar")
FreqTbl (EdFreq, asthmaonly$EDGROUP, "Ed")
FreqTbl (IncFreq, asthmaonly$INCOME3, "Inc")
FreqTbl (BMIFreq, asthmaonly$BMICAT, "BMI")
FreqTbl (SmokeFreq, asthmaonly$SMOKGRP, "Smok")
FreqTbl (ExerFreq, asthmaonly$EXERANY3, "Exer")
FreqTbl (HlthPlanFreq, asthmaonly$HLTHPLN2, "HlthPln")
FreqTbl (GenHlthFreq, asthmaonly$GENHLTH2, "GenHlth")


##### "No asthma" frequencies ##### 

#subset dataset with only asthma people

noasthmaonly <- subset(analytic, ASTHMA4 != 1)
table(noasthmaonly $ASTHMA4)
nrow(noasthmaonly)

AsthmaFreq <- table(noasthmaonly$ASTHMA4)
AsthmaFreq
write.csv(AsthmaFreq, file = "Asthma.csv")

#USING MACROS

#install package gtools
#then call up library

library(gtools)

#use defmacro to define the macro

FreqTbl <-defmacro(OutputTable, InputVar, CSVTable, 
                   expr={
                     OutputTable <- table(InputVar);
                     write.csv(OutputTable, file = paste0(CSVTable, ".csv"))
                   })

FreqTbl (AlcGrpFreq, noasthmaonly$ALCGRP, "Alc")
FreqTbl (AgeGrpFreq, noasthmaonly$X_AGE_G, "Age")
FreqTbl (SexFreq, noasthmaonly$SEX, "Sex")
FreqTbl (HispFreq, noasthmaonly$X_HISPANC, "Hisp")
FreqTbl (RaceFreq, noasthmaonly$RACEGRP, "Race")
FreqTbl (MaritalFreq, noasthmaonly$MARGRP, "Mar")
FreqTbl (EdFreq, noasthmaonly$EDGROUP, "Ed")
FreqTbl (IncFreq, noasthmaonly$INCOME3, "Inc")
FreqTbl (BMIFreq, noasthmaonly$BMICAT, "BMI")
FreqTbl (SmokeFreq, noasthmaonly$SMOKGRP, "Smok")
FreqTbl (ExerFreq, noasthmaonly$EXERANY3, "Exer")
FreqTbl (HlthPlanFreq, noasthmaonly$HLTHPLN2, "HlthPln")
FreqTbl (GenHlthFreq, noasthmaonly$GENHLTH2, "GenHlth")


##### sleeping duration ##### 
#continuous variable - mean, sd

mean(analytic$SLEPTIM2)
sd(analytic$SLEPTIM2)

#load package plyr

library(plyr)

#example

ddply(analytic,~ALCGRP,summarise,mean=mean(SLEPTIM2),sd=sd(SLEPTIM2))

#USING MACROS

library(gtools)

SumTbl <- defmacro(OutputTable, GroupVar, CSVTable,
                   expr={
                     OutputTable <- ddply(analytic,~GroupVar,summarise,mean=mean(SLEPTIM2),sd=sd(SLEPTIM2));
                     write.csv(OutputTable, file = paste0(CSVTable, ".csv"))
                   })

SumTbl (AlcGrpSum, analytic$ALCGRP, "Alc")
SumTbl (AgeGrpSum, analytic$X_AGE_G, "Age")
SumTbl (SexSum, analytic$SEX, "Sex")
SumTbl (HispSum, analytic$X_HISPANC, "Hisp")
SumTbl (RaceSum, analytic$RACEGRP, "Race")
SumTbl (MaritalSum, analytic$MARGRP, "Mar")
SumTbl (EdSum, analytic$EDGROUP, "Ed")
SumTbl (IncSum, analytic$INCOME3, "Inc")
SumTbl (BMISum, analytic$BMICAT, "BMI")
SumTbl (SmokeSum, analytic$SMOKGRP, "Smok")
SumTbl (ExerSum, analytic$EXERANY3, "Exer")
SumTbl (HlthPlanSum, analytic$HLTHPLN2, "HlthPln")
SumTbl (GenHlthSum, analytic$GENHLTH2, "GenHlth")


##### CATEGORICAL Bivariate test with Pearson's Chi square ##### 

#load MASS library

library(MASS)

#make table

AlcTbl = table(analytic$ASTHMA4, analytic$ALCGRP) 
AlcTbl

#run test
chisq.test(AlcTbl)  

#make macro

library(gtools)

ChiTest <- defmacro(VarName, TblName, expr={
  TblName = table(analytic$ASTHMA4, analytic$VarName); 
  chisq.test(TblName)})

ChiTest(ALCGRP, AlcTbl)
ChiTest(X_AGE_G, AgeTbl)
ChiTest(SEX, SexTbl)
ChiTest(X_HISPANC, HispTbl)
ChiTest(RACEGRP, RaceTbl)
ChiTest(MARGRP, MarTbl)
ChiTest(EDGROUP, EdTbl)
ChiTest(INCOME3, IncTbl)
ChiTest(BMICAT, BMITbl)
ChiTest(SMOKGRP, SmokTbl)
ChiTest(EXERANY3, ExerTbl)
ChiTest(HLTHPLN2, HlthPlnTbl)
ChiTest(GENHLTH2, GenHlthTbl)

##### CONTINIOUS Bivariate tests with ANOVA and t-test ##### 

#ANOVAS for Table 1

#example one-way ANOVA
# lm - linear model, x - ALCGRP, y - SLEPTIM2
# lm returns both regression and ANOVA, now we need only p-value for F-statistics (ANOVA result)

AlcANOVA <- lm(formula = SLEPTIM2 ~ ALCGRP, data = analytic)
summary(AlcANOVA)

#make macro

library(gtools)

ANOVATest <- defmacro(VarName, TblName, expr={
  TblName<- lm(formula = SLEPTIM2 ~ VarName, data = analytic);
  summary(TblName)})

#call macro

ANOVATest (ALCGRP, AlcANOVA)
ANOVATest (X_AGE_G, AgeANOVA)
ANOVATest (X_HISPANC, HispANOVA)
ANOVATest (RACEGRP, RaceANOVA)
ANOVATest (MARGRP, MarANOVA)
ANOVATest (EDGROUP, EdANOVA)
ANOVATest (INCOME3, IncANOVA)
ANOVATest (BMICAT, BMIANOVA)
ANOVATest (SMOKGRP, SmokANOVA)
ANOVATest (EXERANY3, ExerANOVA)
ANOVATest (HLTHPLN2, HlthPlnANOVA)
ANOVATest (GENHLTH2, GenHlthANOVA)


#t-tests for Table 1
# t.test(dependent variable, categorical variable)
t.test(analytic$SLEPTIM2~analytic$ASTHMA4)
t.test(analytic$SLEPTIM2~analytic$SEX)


##### weights example ##### 
# asthma frequency weighted to state levels

WeightVarList <- c("X_STATE", "X_LLCPWT", "ASTHMA3")

BRFSS_weights <- subset(BRFSS_a[WeightVarList])

colnames(BRFSS_weights)
nrow(BRFSS_weights)

#use questionr package

library(questionr)

WeightedAsthma <- wtd.table(BRFSS_weights$ASTHMA3, 
                            y=BRFSS_weights$X_STATE, weights = BRFSS_weights$X_LLCPWT, normwt = FALSE, na.rm = TRUE,
                            na.show = FALSE)
write.csv(WeightedAsthma, file = "WeightedAsthma.csv")
WeightedAsthma


##### LINEAR REGRESSION START ##### 
#exposure - alcohol drinking
#outcome - sleep duration

# Parameters of interest from a linear regression model - SLOPES (coefficients) and P-VALUES

#read in analytic table

analytic <- read.csv("analytic.csv", header=TRUE, sep=",")

##### Check Assumptions for Linear Regression ##### 

#Normal probability plot - because we assume normality
#make linear model using grouping variable

AlcSleepTimeRegression = lm(SLEPTIM2 ~ ALCGRP, data=analytic) 

AlcSleepTimeRegression 

summary(AlcSleepTimeRegression)

#Make diagnostic plots

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(AlcSleepTimeRegression, 
     main = "Alcohol by Sleep Duration")

##### INTERPRETATION of graphs ##### 

# ASUMPTION 1 - normal distribution of the dependent variable with respect to the independent variable 
# Normal probability plot (Q-Q Residuals) (left below) - 
# to meet the normality assumption, the black blobby circles need to right across the diagonal dotted line
# out black circles are off the line by the end, but it is still fine

# ASSUMPTION 2 - linear and additive relationship between dependent and independent variables 
# ASSUMPTION 3 - homoscedasticity

# 2 and 3 assupntions are checked by
# Plot of residuals vs predicted values (Residuals vs Fitted) (left upper)
# points should be distributed symmetrically and parallel 

# in our plot it is not perfect (the right one is lower), but it is still ok
# homoscedasticity - the stacks of dots should form an even band across the plot, without increasing or decreasing in length.


##### what if a continuous variable doesn't meet the assumptions? ##### 

##### strategy 1 - categorization strategy #####  
#transform a continuous variable into a categorized one

#copy to new dataset

BRFSS_sleep <- analytic

#summary statistics

summary(BRFSS_sleep$SLEPTIM2)

#look at histogram

hist(BRFSS_sleep$SLEPTIM2, 
     main = "Histogram of SLEPTIM2",
     xlab = "Class SLEPTIM2",
     ylab = "Frequency",
     xlim=c(0,15), 
     ylim=c(0,20000),
     border = "red",
     col= "yellow",
     las = 1,
     breaks = 24)

#categorize at median - below and above the median (7.000)

BRFSS_sleep$SLEEPCAT <- 9 #9 just to create a column and check numbers later
BRFSS_sleep$SLEEPCAT[BRFSS_sleep$SLEPTIM2 >7] <- 1
BRFSS_sleep$SLEEPCAT[BRFSS_sleep$SLEPTIM2 <=7] <- 2

table(BRFSS_sleep$SLEEPCAT, BRFSS_sleep$SLEPTIM2)

##### strategy 2 - log transformation ##### 
#especially if distribution of a continuous variable is skewed

BRFSS_sleep$LOGSLEEP <- log(BRFSS_sleep$SLEPTIM2)

summary(BRFSS_sleep$LOGSLEEP)

hist(BRFSS_sleep$LOGSLEEP, 
     main = "Histogram of LOGSLEEP",
     xlab = "Class LOGSLEEP",
     ylab = "Frequency",
     xlim=c(0,4), 
     ylim=c(0,35000),
     border = "red",
     col= "yellow",
     las = 1,
     breaks = 5)

##### strategy 3 - indices ##### 
#forget about the variable and create a new one out of other variable which in my opinion approximate it
# example is not related to our outcome - sleep duration - because in BFSS there is just 1 question about sleep, so indeces strategy will be demonstrated on cardiovascular diseases variables
#load foreign package

library(foreign)

#read in data to R

BRFSS_a <- read.xport("LLCP2014.XPT ")

#example of an index

BRFSS_vasc<- BRFSS_a

#transform variables

BRFSS_vasc$HA <- 0 #Heart Attack 0-no, 1-yes
BRFSS_vasc$HA[BRFSS_vasc$CVDINFR4 == 1] <- 1
table(BRFSS_vasc$HA, BRFSS_vasc$CVDINFR4)

BRFSS_vasc$CHD <- 0
BRFSS_vasc$CHD[BRFSS_vasc$CVDCRHD4== 1] <- 1
table(BRFSS_vasc$CHD, BRFSS_vasc$CVDCRHD4)

BRFSS_vasc$STROKE <- 0
BRFSS_vasc$STROKE [BRFSS_vasc$CVDSTRK3== 1] <- 1
table(BRFSS_vasc$STROKE, BRFSS_vasc$CVDSTRK3)

#make index

BRFSS_vasc$VASCINDEX <- NA #NA because we are creating a continous variable
BRFSS_vasc$VASCINDEX <- BRFSS_vasc$HA + BRFSS_vasc$CHD + BRFSS_vasc$STROKE
table(BRFSS_vasc$VASCINDEX)

#Look at distribution

summary(BRFSS_vasc$VASCINDEX)

hist(BRFSS_vasc$VASCINDEX, 
     main = "Histogram of VASCINDEX",
     xlab = "Class VASCINDEX",
     ylab = "Frequency",
     xlim=c(0,4), 
     ylim=c(0,500000),
     border = "red",
     col= "yellow",
     las = 1,
     breaks = 4)
#here we failed to improve distribution, but it might work in other cases


##### strategy 4 - quartiles ##### 
#divide variable data into 4 (or as many as needed) even pieces with the same amount of data in each piece
#copy to new dataset

BRFSS_sleep <- analytic

#summary statistics
#quantiles of SLEPTIM2

SLEEPQuantiles <- quantile(BRFSS_sleep$SLEPTIM2)
SLEEPQuantiles # the numbers are the bounders of each quartil
#assign 1234 based on quantiles
BRFSS_sleep$SLEPQUART <- 9
BRFSS_sleep$SLEPQUART[BRFSS_sleep$SLEPTIM2 <= 6] <- 1
BRFSS_sleep$SLEPQUART[BRFSS_sleep$SLEPTIM2 >6 & BRFSS_sleep$SLEPTIM2 <=7] <- 2
BRFSS_sleep$SLEPQUART[BRFSS_sleep$SLEPTIM2 >7 & BRFSS_sleep$SLEPTIM2 <=8] <- 3
BRFSS_sleep$SLEPQUART[BRFSS_sleep$SLEPTIM2 >8] <- 4

table(BRFSS_sleep$SLEPQUART, BRFSS_sleep$SLEPTIM2)
#now we can use SLEPQUART as a continious dependent variable


##### strategy 5 - ranking ##### 
#instead of using the actual number (hours of sleep) in equation, we use rank for the individual
#most sleep - top rank, least sleep- last rank

#Create rank

order.sleeptime <- order(-BRFSS_sleep$SLEPTIM2)

BRFSS_sleep2 <- BRFSS_sleep[order.sleeptime,] #data now sorted by rank

BRFSS_sleep2$SLEPRANK <- rank(BRFSS_sleep2$SLEPTIM2) #SLEPRANK new variable - rank, SLEPTIM2 - amount of sleep hours

head(BRFSS_sleep2[,c("SLEPRANK","SLEPTIM2")], n=25)

tail(BRFSS_sleep2[,c("SLEPRANK","SLEPTIM2")], n=25)
 

##### Beginning Linear Regression Modeling ##### 

# lm() - linear model returns results for regression and ANOVA
# why together? here ANOVA tells us if we are even allowed to interpret regression results
# if F is high and significant, we move to linear regression results
# if F is insignificant, we stop

#####  Reporting order ##### 
#Model 1 Base model - only includes exposure indicator variables (may be more than 1 with multilevel exposure, e.g. drinking weekly, drinking montly)
#Model 2 Adjusted for age and sex - includes covariates in model 1 + age and sex covariates
#Model 3 Fully adjusted - includes all covariates that survive the forward stepwise modeling process

#####  Modeling approaches #####  
#Forward stepwise - run iterative models indroducing a new variable each round
#Backward stepwise - start with all the variables in a model and then run iterative models and remove variables each round

#Which covariates to keep and which not?
#must keep the exposure (at least one)
#keep all the covariates each iteration that are significant
#if they are not significant but i think they are important - i still can keep them
#but don't keep them if they are too insignificant


#####  Model 1 and Model 2 #####  

#make Model 1
#SLEPTIM2 - dependent variable, 
#DRKMONTHLY + DRKWEEKLY - exposure variables
Model1 = lm(SLEPTIM2 ~ DRKMONTHLY + DRKWEEKLY, data=analytic) 

summary(Model1)
#1st - ANOVA results, and only if significant
#look at Coefficients. don't care about (Intercept) line

library (devtools)
library (broom)

Tidy_Model1 <- tidy(Model1)
write.csv(Tidy_Model1, file = "LinearRegressionModel1.csv")

#make Model 2

Model2 = lm(SLEPTIM2 ~ DRKMONTHLY + DRKWEEKLY + MALE + AGE2 + AGE3 + AGE4 + AGE5 + AGE6, data=analytic) 
summary(Model2) 

Tidy_Model2 <- tidy(Model2)
write.csv(Tidy_Model2, file = "LinearRegressionModel2.csv")


#####  Model 3 #####  

summary(Model2)

#Start with Model 2 with the not significant covariates removed

Model3 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6, data=analytic) 
summary(Model3) 

#add smoker

Model4 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
            + SMOKER, data=analytic) 
summary(Model4) 

#smoker is significant
#add Hispanic

Model5 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
            + SMOKER + HISPANIC, data=analytic) 
summary(Model5) 

#Hispanic significant
#add race

Model6 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
            + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE, data=analytic) 
summary(Model6) 

#Race vars significant
#marital status

Model7 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
            + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR, data=analytic) 
summary(Model7)

#Marital significant
#Gen health

Model8 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
            + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
            + FAIRHLTH + POORHLTH, data=analytic) 
summary(Model8)

#Gen Hlth significant
#health plan

Model9 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
            + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
            + FAIRHLTH + POORHLTH + NOPLAN, data=analytic) 
summary(Model9)

#take out NOPLAN
#try education

Model10 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + LOWED + SOMECOLL, data=analytic) 
summary(Model10)

#take out LOWED

Model11 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL, data=analytic) 
summary(Model11)

#add income

Model12 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC1 + INC2 + INC3 + INC4 + INC5 + INC6 + INC7, data=analytic) 
summary(Model12)

#remove insignificant income variables

Model13 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7, data=analytic) 
summary(Model13)

#add BMI

Model14 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + UNDWT + OVWT + OBESE, data=analytic) 
summary(Model14)

#take out UNDWT

Model15 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE, data=analytic) 
summary(Model15)

#add exercise

Model16 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + NOEXER, data=analytic) 
summary(Model16)

#take out noexer
#add back male

Model17 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + MALE, data=analytic) 
summary(Model17)

#take out male
#add back AGE2 and AGE3

Model18 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE2 + AGE3, data=analytic) 
summary(Model18)

#remove AGE2

Model19 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3, data=analytic) 
summary(Model19)

#add back UNDWT

Model20 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + UNDWT, data=analytic) 
summary(Model20)

#remove UNDWT
#add back LOWED

Model21 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED, data=analytic) 
summary(Model21)

#Model21 is working final model

#add back NOEXER

Model22 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED + NOEXER, data=analytic) 
summary(Model22)

#screws up LOWED
#remove NOEXER
#add back NOPLAN

Model23 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED + NOPLAN, data=analytic) 
summary(Model23)

#remove NOPLAN
#add back AGE2

Model24 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED + AGE2, data=analytic) 
summary(Model24)

#remove AGE2
#add back INC1

Model25 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED + INC1, data=analytic) 
summary(Model25)

#remove INC1
#add back INC3

Model26 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED + INC3, data=analytic) 
summary(Model26)

#remove INC3
#add back INC4

Model27 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED + INC4, data=analytic) 
summary(Model27)

#remove INC4
#add back INC5

Model28 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED + INC5, data=analytic) 
summary(Model28)

#remove INC5
#add back INC6

Model29 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED + INC6, data=analytic) 
summary(Model29)

#remove INC6
#add back MALE

Model30 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + AGE3 + LOWED + MALE, data=analytic) 
summary(Model30)

Model31 = lm(SLEPTIM2 ~ DRKWEEKLY + AGE3 + AGE4 + AGE5 + AGE6 
             + SMOKER + HISPANIC + BLACK + ASIAN + OTHRACE + NEVERMAR + FORMERMAR
             + FAIRHLTH + POORHLTH + LOWED + SOMECOLL
             + INC2 + INC7 + OVWT + OBESE + DRKMONTHLY, data=analytic) 
summary(Model31)

###FINAL MODEL

FinalLinearRegressionModel = lm(SLEPTIM2 ~ DRKMONTHLY + DRKWEEKLY + AGE3 + AGE4 + AGE5 + AGE6 
                                + HISPANIC + BLACK + ASIAN + OTHRACE + FORMERMAR + NEVERMAR
                                + LOWED + SOMECOLL + INC2 + INC7 
                                + OVWT + OBESE + SMOKER + FAIRHLTH + POORHLTH, data=analytic) 
summary(FinalLinearRegressionModel)

#output as CSV

library (devtools)
library (broom)

Tidy_FinalModel <- tidy(FinalLinearRegressionModel)
write.csv(Tidy_FinalModel, file = "FinalLinearRegressionModel.csv")

#coefficient plot

library(arm)

#default plot
par(mar = c(10, 10, 10, 10))
coefplot(FinalLinearRegressionModel)
#beta coef for all covariates in the final model - just visualization 

#Final model diagnostics

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(FinalLinearRegressionModel, 
     main = "Final Linear Regression Model")

#Model fit diagnostics

library(gvlma)
gvmodel <- gvlma(FinalLinearRegressionModel) 
summary(gvmodel)


##### Interactions ##### 
#not recommended for artificial improvement of my model
analytic <- read.csv(file="C:/Users/Monika/Dropbox/R Stats Book/Analytics/Data/analytic.csv", header=TRUE, sep=",")

#Interaction Example
#Add black times age interaction

InteractionModel = lm(SLEPTIM2 ~ DRKMONTHLY + DRKWEEKLY + AGE3 + AGE4 + AGE5 + AGE6 
                      + HISPANIC + BLACK + ASIAN + OTHRACE + FORMERMAR + NEVERMAR
                      + LOWED + SOMECOLL + INC2 + INC7 
                      + OVWT + OBESE + SMOKER + FAIRHLTH + POORHLTH
                      + (BLACK*AGE3) + (BLACK*AGE4) + (BLACK*AGE5) + (BLACK*AGE6), data=analytic) 
summary(InteractionModel)


##### LOGISTIC REGRESSION MODEL START ##### 

# Dependent variable in logistic model - binary (yes/no)
# Dependent variable in linear model - continuous 

# Here in logistic model the dependent variable - asthma status (binary)

#Model 1 Base model - only includes exposure indicator variables (may be more than 1 with multilevel exposure, e.g. drinking weekly, drinking montly)
#Model 2 Adjusted for age and sex - includes covariates in model 1 + age and sex covariates
#Model 3 Fully adjusted - includes all covariates that survive the forward stepwise modeling process

#In my candidate final model all the covariates should be significant or very close to significant

# In the last step I put back into the model the covariates i kicked off earlier (because they were insignificant) one a time and see - 
# what if it becomes significant now? If yes, I leave it in the model


# Logistic regression output - log odds and standard error as parameter estimates
# In the console outputs are - log odds, standard errors, and p-values

# Parameters of interest from a logistic model - ODDS RATIOs (ORs) and 95% CONFIDENCE INTERVALs (CIs)

##### Models 1 and 2 ##### 

analytic <- read.csv(file="/Users/viktoriazajceva/Desktop/R/linkedin healthcare analytics/descriptive/Exercise Files/analytic.csv", header=TRUE, sep=",")

#make Model 1
#glm - general linear model (ASTHMA4 - dependent variable)
#family = "binomial" we specify that it is a logistic model
LogModel1 <- glm(ASTHMA4 ~ DRKMONTHLY + DRKWEEKLY, data=analytic, family = "binomial") 
summary(LogModel1)

library (devtools)
library (broom)

Tidy_LogModel1 <- tidy(LogModel1)
Tidy_LogModel1

#Add calculations
# exp because we got ln (Odds)
Tidy_LogModel1$OR <- exp(Tidy_LogModel1$estimate) #95% CI of Odds Ratio
Tidy_LogModel1$LL <- exp(Tidy_LogModel1$estimate - (1.96 * Tidy_LogModel1$std.error)) #95% CI Lower limit of Odds Ratio
Tidy_LogModel1$UL <- exp(Tidy_LogModel1$estimate + (1.96 * Tidy_LogModel1$std.error)) #95% CI Upper limit of Odds Ratio
Tidy_LogModel1

write.csv(Tidy_LogModel1, file = "LogisticRegressionModel1.csv")

#make Model 2

LogModel2 <- glm(ASTHMA4 ~ DRKMONTHLY + DRKWEEKLY + MALE + AGE2 + AGE3 + AGE4 + AGE5 + AGE6, data=analytic, family = "binomial") 
summary(LogModel2) 

#Add calculations

Tidy_LogModel2 <- tidy(LogModel2)
Tidy_LogModel2$OR <- exp(Tidy_LogModel2$estimate)
Tidy_LogModel2$LL <- exp(Tidy_LogModel2$estimate - (1.96 * Tidy_LogModel2$std.error))
Tidy_LogModel2$UL <- exp(Tidy_LogModel2$estimate + (1.96 * Tidy_LogModel2$std.error))
Tidy_LogModel2
write.csv(Tidy_LogModel2, file = "LogisticRegressionModel2.csv")

##### Forward stepwise - round 1 ##### 
#Model 3 is everything significant in Model2

LogModel3 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE, data=analytic, family = "binomial") 
summary(LogModel3)

#add smoker

LogModel4 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER, data=analytic, family = "binomial") 
summary(LogModel4)

#Keep smoker
#add Hispanic

LogModel5 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER + HISPANIC, data=analytic, family = "binomial") 
summary(LogModel5)

#remove Hispanic
#add races

LogModel6 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER 
                 + BLACK + ASIAN + OTHRACE, data=analytic, family = "binomial") 
summary(LogModel6)

#keep only OTHRACE

LogModel7 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER 
                 + OTHRACE, data=analytic, family = "binomial") 
summary(LogModel7)

#add marital

LogModel8 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER 
                 + OTHRACE + NEVERMAR + FORMERMAR, data=analytic, family = "binomial") 
summary(LogModel8)

#keep marital
#add generalhealth

LogModel9 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER 
                 + OTHRACE + NEVERMAR + FORMERMAR + FAIRHLTH + POORHLTH, data=analytic, family = "binomial") 
summary(LogModel9)

#now FORMERMAR not sig
#take out FORMERMAR

LogModel10 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER 
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH, data=analytic, family = "binomial") 
summary(LogModel10)

#add health plan

LogModel11 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER 
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH + NOPLAN, data=analytic, family = "binomial") 
summary(LogModel11)

#take out health plan
#add education

LogModel12 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER 
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + LOWED + SOMECOLL, data=analytic, family = "binomial") 
summary(LogModel12)

#take out education
#add income

LogModel13 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER 
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4 + INC5 + INC6 + INC7, data=analytic, family = "binomial") 
summary(LogModel13)

#take out INC5 through INC7

LogModel14 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE + SMOKER 
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4, data=analytic, family = "binomial") 
summary(LogModel14)

#take out SMOKER

LogModel15 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4, data=analytic, family = "binomial") 
summary(LogModel15)

#add obesity

LogModel16 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + UNDWT + OVWT + OBESE, data=analytic, family = "binomial") 
summary(LogModel16)

#remove UNDWT

LogModel17 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OVWT + OBESE, data=analytic, family = "binomial") 
summary(LogModel17)

#remove OVWT

LogModel18 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE, data=analytic, family = "binomial") 
summary(LogModel18)

#add exercise

LogModel19 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER, data=analytic, family = "binomial") 
summary(LogModel19)

#add back DRKMONTHLY

LogModel20 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY, data=analytic, family = "binomial") 
summary(LogModel20)

#MODEL20 is CANDIDATE FINAL MODEL

##### Forward stepwise - round 3 ##### 
#try to add back SMOKER

LogModel21 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER, data=analytic, family = "binomial") 
summary(LogModel21)

#almost significant, keep.
#add back HISPANIC

LogModel22 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + HISPANIC, data=analytic, family = "binomial") 
summary(LogModel22)

#remove HISPANIC
#add back BLACK

LogModel23 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + BLACK, data=analytic, family = "binomial") 
summary(LogModel23)

#remove BLACK
#add back ASIAN

LogModel24 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + ASIAN, data=analytic, family = "binomial") 
summary(LogModel24)

#remove ASIAN
#add back LOWED

LogModel25 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED, data=analytic, family = "binomial") 
summary(LogModel25)

#keep LOWED
#add back SOMECOLL

LogModel26 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + SOMECOLL, data=analytic, family = "binomial") 
summary(LogModel26)

#remove SOMECOLL
#add back INC5

LogModel27 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + INC5, data=analytic, family = "binomial") 
summary(LogModel27)

#remove INC5
#add back INC6

LogModel28 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + INC6, data=analytic, family = "binomial") 
summary(LogModel28)

#remove INC6
#add back INC7

LogModel29 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + INC7, data=analytic, family = "binomial") 
summary(LogModel29)

#remove INC7
#add back FORMERMAR

LogModel30 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + FORMERMAR, data=analytic, family = "binomial") 
summary(LogModel30)

#remove FORMERMAR
#add back NOPLAN

LogModel31 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + NOPLAN, data=analytic, family = "binomial") 
summary(LogModel31)

#remove FORMERMAR
#add back UNDWT

LogModel32 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + UNDWT, data=analytic, family = "binomial") 
summary(LogModel32)

#remove UNDWT
#add back OVWT

LogModel33 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT, data=analytic, family = "binomial") 
summary(LogModel33)

#add back AGE2

LogModel34 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2, data=analytic, family = "binomial") 
summary(LogModel34)

#add back AGE3

LogModel35 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE3, data=analytic, family = "binomial") 
summary(LogModel35)

#remove AGE3
#add back AGE4

LogModel36 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE4, data=analytic, family = "binomial") 
summary(LogModel36)

#remove AGE4
#add back AGE5

LogModel37 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5, data=analytic, family = "binomial") 
summary(LogModel37)

#add back AGE6

LogModel38 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + AGE6, data=analytic, family = "binomial") 
summary(LogModel38)


#remove AGE6
#add back HISPANIC

LogModel39 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + HISPANIC, data=analytic, family = "binomial") 
summary(LogModel39)

#remove HISPANIC
#add back SOMECOLL

LogModel40 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + SOMECOLL, data=analytic, family = "binomial") 
summary(LogModel40)

#remove SOMECOLL
#add back UNDWT

LogModel41 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + UNDWT, data=analytic, family = "binomial") 
summary(LogModel41)

#remove UNDWT
#add back health plan

LogModel42 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + NOPLAN, data=analytic, family = "binomial") 
summary(LogModel42)

#remove NOPLAN
#add back INC5

LogModel43 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + INC5, data=analytic, family = "binomial") 
summary(LogModel43)

#remove INC5
#add back INC6

LogModel44 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + INC6, data=analytic, family = "binomial") 
summary(LogModel44)

#remove INC6
#add back INC7

LogModel45 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + INC7, data=analytic, family = "binomial") 
summary(LogModel45)

#remove INC7
#add back BLACK

LogModel46 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + BLACK, data=analytic, family = "binomial") 
summary(LogModel46)

#remove BLACK
#add back ASIAN

LogModel47 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + ASIAN, data=analytic, family = "binomial") 
summary(LogModel47)

#remove ASIAN
#add back FORMERMAR

LogModel48 <- glm(ASTHMA4 ~ DRKWEEKLY + MALE
                  + OTHRACE + NEVERMAR + FAIRHLTH + POORHLTH
                  + INC1 + INC2 + INC3 + INC4
                  + OBESE + NOEXER + DRKMONTHLY + SMOKER
                  + LOWED + OVWT + AGE2 + AGE5 + FORMERMAR, data=analytic, family = "binomial") 
summary(LogModel48)

##### FINAL MODEL ##### 
#arrange covariates in order of table 1

FinalLogisticRegressionModel <- glm(ASTHMA4 ~ DRKMONTHLY + DRKWEEKLY 
                                    + AGE2 + AGE5 + MALE
                                    + OTHRACE + NEVERMAR + LOWED 
                                    + INC1 + INC2 + INC3 + INC4
                                    + OVWT + OBESE + SMOKER 
                                    + NOEXER + FAIRHLTH + POORHLTH, data=analytic, family = "binomial") 
summary(FinalLogisticRegressionModel)

#write out CSV of final model

library (devtools)
library (broom)

Tidy_LogModel_a <- tidy(FinalLogisticRegressionModel)
Tidy_LogModel3 <- subset(Tidy_LogModel_a, term != "(Intercept)")

#Add calculations

Tidy_LogModel3$OR <- exp(Tidy_LogModel3$estimate)
Tidy_LogModel3$LL <- exp(Tidy_LogModel3$estimate - (1.96 * Tidy_LogModel3$std.error))
Tidy_LogModel3$UL <- exp(Tidy_LogModel3$estimate + (1.96 * Tidy_LogModel3$std.error))
Tidy_LogModel3
write.csv(Tidy_LogModel3, file = "LogisticRegressionModel3.csv")

##### plot for coefficients ####
#visualize to help interpretation

library(ggplot2)

ggplot(Tidy_LogModel3, 
       aes(x = term, y = OR, ymin = LL, ymax = UL)) + 
  geom_pointrange(aes(col = factor(term)), 
                  position=position_dodge(width=0.30)) + 
  ylab("Odds ratio & 95% CI") + 
  geom_hline(aes(yintercept = 1)) + 
  scale_color_discrete(name = "Term") + 
  xlab("") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


dev.off() #if error invalid graphics state

##### AIC Akaike Information criteria #####
#AIC Akaike Information criteria - for comparing the fit of iterative models and helping me decide which model to select
#Interpretation
#the more covariates in the model, the lower AIC goes down
#I want the lowest AIC among my model
#but I shouldn't just add more covariates to lower AIC
#I should choose among models with only significant (or important) covariates
#I want the least covariates that will give me the lowest AIC
#when I run summary(model), AIC at the end


##### Nested models ##### 
# a model with covariates A and B is nested (contained) in a model with covariates A,B and C
# example -  I am modeling ASTHMA4 and i choode to include categorical variables - SMOKER , OVWT, OBESE
# and one continuous variable - SLEPTIM2. But it may be not linear

#I can transform this continuous variable into a categorical one
#or split it to quartiles etc

#and Nested models with help me to decide which strategy to use for modeling a particular variable

#example for lecture on ORs
#the example will compare nested models using the -2 log likelihood and degrees of freedom from the mode;
#the example  will model our continuous variable  - sleep duration - in 2 ways - and see if one fits better

ExampleModel1 <- glm(ASTHMA4 ~ SMOKER + OVWT + OBESE, data=analytic, family = "binomial") 
summary(ExampleModel1)

#example for model fit - added continuous variable SLEPTIM2

ExampleModel2 <- glm(ASTHMA4 ~ SMOKER + OVWT + OBESE + SLEPTIM2, data=analytic, family = "binomial") 
summary(ExampleModel2)

#plot and look at SLEPTIM2

summary(analytic$SLEPTIM2)
hist(analytic$SLEPTIM2, 
     main = "Histogram of SLEPTIM2",
     xlab = "Class SLEPTIM2",
     ylab = "Frequency",
     xlim=c(0,15), 
     ylim=c(0,20000),
     border = "red",
     col= "yellow",
     las = 1,
     breaks = 24)

#let's do tertiles

SleepTertiles <- quantile(analytic$SLEPTIM2, probs = c(0, 0.33, 0.66, 1))
SleepTertiles #cutpoints 1,7,8,24
analytic$STertile <- 1
analytic$STertile[analytic$SLEPTIM2 >=7 & analytic$SLEPTIM2 < 8] <- 2
analytic$STertile[analytic$SLEPTIM2 >=8] <- 3
table(analytic$SLEPTIM2, analytic$STertile)

#make indicator variables for the first and second tertiles - STERT1 and STERT2

analytic$STERT1 <- 0
analytic$STERT1[analytic$STertile == 1] <- 1
table(analytic$STertile, analytic$STERT1)

analytic$STERT2 <- 0
analytic$STERT2[analytic$STertile == 2] <- 1
table(analytic$STertile, analytic$STERT2)

#example of nested models

#Model1 is nested in Model2 - above

#Model1 is nested in Model3

ExampleModel3 <- glm(ASTHMA4 ~ SMOKER + OVWT + OBESE + STERT1 + STERT2, data=analytic, family = "binomial") 
summary(ExampleModel3)
#we are comparing sleep time as indicator variables in thirds (tertiles) versus just using it continuously
#get the log likelihoods

library(nlme) #package for comparison

logLik(ExampleModel1) #to see he -2 log likelihood and degrees of freedom
logLik(ExampleModel2)
logLik(ExampleModel3)

#see "Nested model comparison.xlsx" file for comparison 
#calculate the difference in -2 log likelihood and degrees of freedom for models 2 vs 1 and 3 vs 1
#and then p-value with =CHIDIST(-2 log likelihood, df)

#use the model with more significant p-value
