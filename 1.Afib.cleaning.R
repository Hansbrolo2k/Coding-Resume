#This is the first file for my INOVA Atrial Fibrillation research project. This shows off a little of my data cleaning capabilities

#################
#install packages
#################
#readxl
install.packages("readxl")
library(readxl)

#corrplot
install.packages("corrplot")
library(corrplot)

#SIS
install.packages("SIS")
library(SIS)

#GGplot
install.packages("ggplot2")
library(ggplot2)

# ggExtra
install.packages("ggExtra")
library(ggExtra)

#data.table package
install.packages("data.table")
library(data.table)

#dplyr
install.packages("dplyr")
library(plyr)
library(dplyr)

#tidyr
install.packages("tidyr")
library(tidyr)

#DemographicTable package
install.packages("DemographicTable")
library(DemographicTable)

#lubridate package
install.packages("lubridate")
library(lubridate)

#matrixStats package
install.packages("matrixStats")
library(matrixStats)

#plotly package
install.packages("plotly")
library(plotly)

#GGally package
install.packages("GGally")
library(GGally)

#randomForest packages
need <- c("partykit","rpart.plot","caret", "ggplot2", "randomForest", "rpart", "rattle", "dplyr", "gridExtra", "tidyr")
new <- need[!(need %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
invisible(lapply(need, require, character.only=T))
rm(need, new)
library(caret)
library(randomForest)
library(rpart)
library(rattle)
library(dplyr)
library(gridExtra)

#Mosaic plot
install.packages("devtools")
devtools::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)

#For RF plots and tables
install.packages("gridExtra")
library(gridExtra)
library(grid)
library(gtable)

#For RF code
library(reshape2)
install.packages("tidytext")
library(tidytext)
library(gridExtra)
library(randomForest)
library(caret)

library(tidyverse)
install.packages("tree")
library(tree)

##########
#read data
##########
#Afib_data <- read_excel("C:/Users/brody/Desktop/School/Work/Internships/INOVA/AFIBRegistry_ForAnalysis_without MRN 9-27-22.xlsx",sheet=1,skip=1)
Afib_data <- read_excel("C:/Users/brody/Desktop/School/Work/Internships/INOVA/AFIBRegistry_without controls.xlsx",sheet=1)

#View(Afib_data)

###########
#clean data
###########
Afib_data<-as.data.frame(Afib_data)
Afib_data$sex<-as.factor(Afib_data$sex)
Afib_data$race<-as.factor(Afib_data$race)
Afib_data$afib_type<-as.factor(Afib_data$afib_type)
Afib_data$SMK  <- as.factor(Afib_data$SMK)
Afib_data$ABL_type <- as.factor(Afib_data$ABL_type)

#Replace Named Variables(sex, race, afib_type, smk, ABL_type)
Afib_data$sex <- recode(Afib_data$sex, '1' = "M", '2' = "F")
Afib_data$race <- recode(Afib_data$race,  "1"="white", "2"="Other", "3"="Other", "4"="Other","7"="Other")
#Afib_data$race <- recode(Afib_data$race,  "1"="white", "2"="black", "3"="Asian", "4"="unspecified","7"="Middle Estern")
Afib_data$afib_type <- recode(Afib_data$afib_type, "1"="parox", "2"="pers", "3"="LSP")
Afib_data$SMK  <- as.factor(recode(Afib_data$SMK, '0' = "non smoker", '1' = "current/former smoker", '2' = "current/former smoker"))
#Afib_data$SMK  <- as.factor(recode(Afib_data$SMK, '0' = "non smoker", '1' = "current smoker", '2' = "former smoker"))
Afib_data$ABL_type <- as.factor(recode(Afib_data$ABL_type, "1"= "RF", "2"="Cryo"))

#Replace Binary Variables
for(i in c(8:16,18:20,22:25,29,31,33,39)) Afib_data[,i] <- as.factor(Afib_data[,i]); rm(i)

#Age at afib1
Afib_data$AfibAge <- interval(Afib_data$DOB,Afib_data$afib1) %/% years(1)

#Remove patients without biomarkers
Afib_data <- Afib_data %>% 
  filter(!is.na(TNFRSF14))

#Change in AFEQT
#AFEQT_change <-(AFEQT12 or AFEQT6)-Base_AFEQT

Afib_data$AFEQT_change<-coalesce(Afib_data$AFEQT12,Afib_data$AFEQT6,Afib_data$AFEQT3)-Afib_data$Base_AFEQT

#Consistency of AFEQT
Afib_data$AFEQT_consis <- Afib_data$AFEQT12-Afib_data$AFEQT6

#remove redundant variables and biomarkers with >75% missingness
Afib_data<-Afib_data[,-c(1,15,35,36,37,38,78)]


