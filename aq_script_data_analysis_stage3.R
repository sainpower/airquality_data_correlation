# This script is to perform data analysis for CN8001 project evaluation
# This Script is written by:
# --------------------------
# ---- Ejaz Hussain --------
# ---- Data Scientist ------
# ---- LB of Hounslow ------
# ---- LU: March 2023 ------
# --------------------------

# Raw datasets contain London (Hounslow) Air Quality data which consist on 5 years of hourly generated data
# The data exact hotspot is Chiswick where the weather station is installed and operated by LB of Hounslow 
# All extracted data is OPEN and Widely available on Ricardo's Online Platform

#----------------------------------------------------

# Required Libraries

library("tidyverse")
library("dplyr")
library("readr")
library("janitor")
library("data.table")
library("lubridate")
library("readxl")
library("ggplot2")

#----------------------------------------------------

# Checking and Setting Working Directory
getwd()
setwd(dirname(file.choose()))
getwd()

#----------------------------------------------------

# ==========================================================================
# Data Analysis Stage 3: Data Analysis
# ==========================================================================

# Adding Extracted raw AQ CSV Files

dfaq1 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2017.csv", stringsAsFactors = FALSE) # Year 2017
dfaq2 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2018.csv", stringsAsFactors = FALSE) # Year 2018
dfaq3 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2019.csv", stringsAsFactors = FALSE) # Year 2019
dfaq4 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2020.csv", stringsAsFactors = FALSE) # Year 2020
dfaq5 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2021.csv", stringsAsFactors = FALSE) # Year 2021

#----------------------------------------------------


# ==========================================================================
# Stage 1 & 2: Data Prep Exploration Steps for Stage 3
# ==========================================================================


# 2017 Dataset Preparation and Notes:
# ==========================================================================

# Pre-Steps for Data Preparation in S1
dfaq1$end.date <- as.Date(dfaq1$end.date)
dfaq1$pm10 <- as.numeric(dfaq1$pm10)
dfaq1$no <- as.numeric(dfaq1$no)
dfaq1$no2 <- as.numeric(dfaq1$no2)
dfaq1$noxasno2 <- as.numeric(dfaq1$noxasno2)
dfaq1$pm25 <- as.numeric(dfaq1$pm25)

# Checking data scheme, understating relationship of Independent Variables and Dependent Variables, data types and to filter unwanted variables.
glimpse(dfaq1)
str(dfaq1) # outlined data types of the data fr i.e. in dfaq1 we have 4 data types are in numerical shape, 1 as integer and the rest of the cols are string based.

# To view full summary of the entire dataset or choose col
summary(dfaq1) # This function has outlined mean, median, min, max and number of blanks in variable cols 

# This is to view cols composition of data set
sapply(dfaq1, typeof)

# To view data composition (head and tail of dataset)
head(dfaq1, n=10)
tail(dfaq1, n=10)

# Identify Total no of col rows and cols
dim(dfaq1)  


# 2018 Dataset Preparation and Notes:
# ==========================================================================

# Pre-Steps for Data Preparation in S1
dfaq2$end.date <- as.Date(dfaq2$end.date)
dfaq2$pm10 <- as.numeric(dfaq2$pm10)
dfaq2$no <- as.numeric(dfaq2$no)
dfaq2$no2 <- as.numeric(dfaq2$no2)
dfaq2$noxasno2 <- as.numeric(dfaq2$noxasno2)
dfaq2$pm25 <- as.numeric(dfaq2$pm25)

# Checking data scheme, understating relationship of Independent Variables and Dependent Variables, data types and to filter unwanted variables.
glimpse(dfaq2)
str(dfaq2) # outlined data types of the data fr i.e. in dfaq2 we have 4 data types are in numerical shape, 1 as integer and the rest of the cols are string based.

# To view full summary of the entire dataset or choose col
summary(dfaq2) # This function has outlined mean, median, min, max and number of blanks in variable cols 

# This is to view cols composition of data set
sapply(dfaq2, typeof)

# To view data composition (head and tail of dataset)
head(dfaq2, n=10)
tail(dfaq2, n=10)

# Identify Total no of col rows and cols
dim(dfaq2)  


# 2019 Dataset Preparation and Notes:
# ==========================================================================

# Pre-Steps for Data Preparation in S1
dfaq3$end.date <- as.Date(dfaq3$end.date)
dfaq3$pm10 <- as.numeric(dfaq3$pm10)
dfaq3$no <- as.numeric(dfaq3$no)
dfaq3$no2 <- as.numeric(dfaq3$no2)
dfaq3$noxasno2 <- as.numeric(dfaq3$noxasno2)
dfaq3$pm25 <- as.numeric(dfaq3$pm25)

# Checking data scheme, understating relationship of Independent Variables and Dependent Variables, data types and to filter unwanted variables.
glimpse(dfaq3)
str(dfaq3) # outlined data types of the data fr i.e. in dfaq3 we have 4 data types are in numerical shape, 1 as integer and the rest of the cols are string based.

# To view full summary of the entire dataset or choose col
summary(dfaq3) # This function has outlined mean, median, min, max and number of blanks in variable cols 

# This is to view cols composition of data set
sapply(dfaq3, typeof)

# To view data composition (head and tail of dataset)
head(dfaq3, n=10)
tail(dfaq3, n=10)

# Identify Total no of col rows and cols
dim(dfaq3)  


# 2020 Dataset Preparation and Notes:
# ==========================================================================

# Pre-Steps for Data Preparation in S1
dfaq4$end.date <- as.Date(dfaq4$end.date)
dfaq4$pm10 <- as.numeric(dfaq4$pm10)
dfaq4$no <- as.numeric(dfaq4$no)
dfaq4$no2 <- as.numeric(dfaq4$no2)
dfaq4$noxasno2 <- as.numeric(dfaq4$noxasno2)
dfaq4$pm25 <- as.numeric(dfaq4$pm25)


# Checking data scheme, understating relationship of Independent Variables and Dependent Variables, data types and to filter unwanted variables.
glimpse(dfaq4)
str(dfaq2) # outlined data types of the data fr i.e. in dfaq4 we have 4 data types are in numerical shape, 1 as integer and the rest of the cols are string based.

# To view full summary of the entire dataset or choose col
summary(dfaq4) # This function has outlined mean, median, min, max and number of blanks in variable cols 

# This is to view cols composition of data set
sapply(dfaq4, typeof)

# To view data composition (head and tail of dataset)
head(dfaq4, n=10)
tail(dfaq4, n=10)

# Identify Total no of col rows and cols
dim(dfaq4)  


# 2021 Dataset Preparation and Notes:
# ==========================================================================

# Pre-Steps for Data Preparation in S1
dfaq5$end.date <- as.Date(dfaq5$end.date)
dfaq5$pm10 <- as.numeric(dfaq5$pm10)
dfaq5$no <- as.numeric(dfaq5$no)
dfaq5$no2 <- as.numeric(dfaq5$no2)
dfaq5$noxasno2 <- as.numeric(dfaq5$noxasno2)
dfaq5$pm25 <- as.numeric(dfaq5$pm25)

# Checking data scheme, understating relationship of Independent Variables and Dependent Variables, data types and to filter unwanted variables.
glimpse(dfaq5)
str(dfaq5) # outlined data types of the data fr i.e. in dfaq5 we have 4 data types are in numerical shape, 1 as integer and the rest of the cols are string based.

# To view full summary of the entire dataset or choose col
summary(dfaq5) # This function has outlined mean, median, min, max and number of blanks in variable cols 
summary(dfaq5$PM25)

# This is to view cols composition of data set
sapply(dfaq5, typeof)

# To view data composition (head and tail of dataset)
head(dfaq5, n=10)
tail(dfaq5, n=10)

# Identify Total no of col rows and cols
dim(dfaq5)  


# ==========================================================================
# Stage 3: Data Analysis
# ==========================================================================

# Task 1: Combining all datasets (5 years) into 1 single dataset to filter overall view of 5 years period
# ------------------------------------------------------

# Using UNION Technique to join all 5 years of data
union_aq_df17_18 <- union_all(dfaq1, dfaq2)
union_aq_df17_18_19 <- union_all(union_aq_df17_18, dfaq3)
union_aq_df17_18_19_20 <- union_all(union_aq_df17_18_19, dfaq4)
union_aq_df17_18_19_20_21 <- union_all(union_aq_df17_18_19_20, dfaq5)
aq_full_df <- union_aq_df17_18_19_20_21

# Remove extra created datasets out of R Env.
rm(union_aq_df17_18)
rm(union_aq_df17_18_19)
rm(union_aq_df17_18_19_20)
rm(union_aq_df17_18_19_20_21)

summary(aq_full_df) # This will generate data analysis report for all 5 years in a single view

# ==========================================================================

# aq_full_df <- readr::write_csv(aq_full_df, file = "C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_3_aq_datasets/aq_comb_years_data.csv")

# ==========================================================================

# ------------------------------------------------------
# Analysis WRITE-UP Notes:
# end.date            end.time              pm10              no              no2            noxasno2            pm25       
# Min.   :2017-01-01   Length:43824       Min.   : -2.50   Min.   :  0.00   Min.   :  0.00   Min.   :   1.00   Min.   : -2.00  
# 1st Qu.:2018-04-02   Class :character   1st Qu.: 11.40   1st Qu.:  8.10   1st Qu.: 23.90   1st Qu.:  38.30   1st Qu.:  7.00  
# Median :2019-07-02   Mode  :character   Median : 16.50   Median : 18.50   Median : 37.50   Median :  66.30   Median :  9.00  
# Mean   :2019-07-02                      Mean   : 18.91   Mean   : 31.24   Mean   : 41.48   Mean   :  89.38   Mean   : 12.28  
# 3rd Qu.:2020-10-01                      3rd Qu.: 23.50   3rd Qu.: 37.50   3rd Qu.: 54.80   3rd Qu.: 111.70   3rd Qu.: 15.00  
# Max.   :2021-12-31                      Max.   :171.80   Max.   :621.80   Max.   :258.00   Max.   :1173.70   Max.   :157.00  
# NA's   :1342     NA's   :492      NA's   :492      NA's   :492       NA's   :5229  
# ------------------------------------------------------


# Task 2: Obtain mean values for all pollutant variables 
# ------------------------------------------------------

mean(aq_full_df$pm10, na.rm = TRUE) #18.91
mean(aq_full_df$no, na.rm = TRUE) #31.24
mean(aq_full_df$no2, na.rm = TRUE) #41.47
mean(aq_full_df$noxasno2, na.rm = TRUE) #89.38
mean(aq_full_df$pm25, na.rm = TRUE) #12.27

# Task 3: Using Impute function / technique filling NA's with discovered values 
# ------------------------------------------------------

aq_full_df$pm10[is.na(aq_full_df$pm10)] <- mean(aq_full_df$pm10, na.rm = T)
aq_full_df$pm25[is.na(aq_full_df$pm25)] <- mean(aq_full_df$pm25, na.rm = T)
aq_full_df$no[is.na(aq_full_df$no)] <- mean(aq_full_df$no, na.rm = T)
aq_full_df$no2[is.na(aq_full_df$no2)] <- mean(aq_full_df$no2, na.rm = T)
aq_full_df$noxasno2[is.na(aq_full_df$noxasno2)] <- mean(aq_full_df$noxasno2, na.rm = T)
summary(aq_full_df)
# ------------------------------------------------------



# Task 4: Box plot to oversee outliers and then applying techniques to fix the out balance in tasks below
# ------------------------------------------------------
# create a boxplot of the PM10 variable
boxplot(aq_full_df$pm10, main = "PM10 Distribution")

# identify outliers using the boxplot's outlier function
outliers <- boxplot(aq_full_df$pm10, plot = FALSE)$out

# print the identified outliers
print(paste("Outliers:", paste(outliers, collapse = ", ")))



# Task 5: Applying Winsorization Technique for Outliers 
# ------------------------------------------------------

# Compute the median and standard deviation of PM10
# med <- median(aq_full_df$pm10, na.rm = TRUE)
# sd <- sd(aq_full_df$pm10, na.rm = TRUE)

# Set the lower and upper limits for Winsorization
# lower_limit <- med - 3 * sd
# upper_limit <- med + 3 * sd

# Winsorize the PM10 column
# aq_full_df$pm10[aq_full_df$pm10 < lower_limit] <- lower_limit
# aq_full_df$pm10[aq_full_df$pm10> upper_limit] <- upper_limit

# create a boxplot of the PM10 variable
# boxplot(aq_full_df$pm10, main = "PM10 Distribution")

# summary(aq_full_df$pm10)


# Task 6: Applying Z-Score statistical technique for Outliers 
# ------------------------------------------------------

# Calculate z-scores for PM10 column
pm10_zscores <- scale(aq_full_df$pm10)

# Set threshold for z-score outliers
z_threshold <- 2.5

# Identify PM10 values with z-score above threshold
pm10_outliers <- aq_full_df$pm10[abs(pm10_zscores) > z_threshold]

# Replace outlier values with mean of non-outlier values
aq_full_df$pm10[aq_full_df$pm10 %in% pm10_outliers] <- mean(aq_full_df$pm10[!aq_full_df$pm10 %in% pm10_outliers])

# Create a boxplot of the PM10 variable
boxplot(aq_full_df$pm10, main = "PM10 Distribution")
summary(aq_full_df$pm10)

# -------------------------------------------------------
# Applying above practice for other variables in a AQ Dataset
# PM2.5 Variable
pm25_zscores <- scale(aq_full_df$pm25)
pm25_outliers <- aq_full_df$pm25[abs(pm25_zscores) > z_threshold]
aq_full_df$pm25[aq_full_df$pm25 %in% pm25_outliers] <- mean(aq_full_df$pm25[!aq_full_df$pm25 %in% pm25_outliers])
boxplot(aq_full_df$pm25, main = "PM2.5 Distribution")
summary(aq_full_df$pm25)

# NO Variable
no_zscores <- scale(aq_full_df$no)
no_outliers <- aq_full_df$no[abs(no_zscores) > z_threshold]
aq_full_df$no[aq_full_df$no %in% no_outliers] <- mean(aq_full_df$no[!aq_full_df$no %in% no_outliers])
boxplot(aq_full_df$no, main = "NO Distribution")
summary(aq_full_df$no)

# NO2 Variable
no2_zscores <- scale(aq_full_df$no2)
no2_outliers <- aq_full_df$no2[abs(no2_zscores) > z_threshold]
aq_full_df$no2[aq_full_df$no2 %in% no2_outliers] <- mean(aq_full_df$no2[!aq_full_df$no2 %in% no2_outliers])
boxplot(aq_full_df$no2, main = "NO2 Distribution")
summary(aq_full_df$no2)

# NOxasNO2 Variable
noxasno2_zscores <- scale(aq_full_df$noxasno2)
noxasno2_outliers <- aq_full_df$noxasno2[abs(noxasno2_zscores) > z_threshold]
aq_full_df$noxasno2[aq_full_df$noxasno2 %in% noxasno2_outliers] <- mean(aq_full_df$noxasno2[!aq_full_df$noxasno2 %in% noxasno2_outliers])
boxplot(aq_full_df$noxasno2, main = "NOxasNO2 Distribution")
summary(aq_full_df$noxasno2)

summary(aq_full_df)



# ------------------------------------------------------
# Task 5 & 6 Conclusion: Winsorization method to tackle pollutant variable is one of the standard approach, however; analysis outcomes are bit lower than z-score methods in task 6.
# ------------------------------------------------------

# ==========================================================================

 aq_full_df <- readr::write_csv(aq_full_df, file = "C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_4_aq_datasets/aq_refined_df.csv")

# ==========================================================================

# ==========================================================================

# remove all variables from the environment
rm(list=ls())

# ==========================================================================