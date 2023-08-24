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
# Data Analysis Stage 1: Data Preparation
# ==========================================================================

# Adding Extracted raw AQ CSV Files

dfaq1 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2017.csv", stringsAsFactors = TRUE) # Year 2017
dfaq2 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2018.csv", stringsAsFactors = TRUE) # Year 2018
dfaq3 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2019.csv", stringsAsFactors = TRUE) # Year 2019
dfaq4 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2020.csv", stringsAsFactors = TRUE) # Year 2020
dfaq5 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2021.csv", stringsAsFactors = TRUE) # Year 2021

#----------------------------------------------------

# ==========================================================================
# Data Analysis Stage 2: Data Exploration
# ==========================================================================

# Checking data scheme, understating relationship of Independent Variables and Dependent Variables, data types and to filter unwanted variables.

glimpse(dfaq1)
str(dfaq1) # outlined data types of the data fr i.e. in dfaq1 we have 4 data types are in numerical shape, 1 as integer and the rest of the cols are string based.

# To view full summary of the entire dataset or choose col
summary(dfaq1) # This function has outlined mean, median, min, max and number of blanks in variable cols 
summary(dfaq1$PM25)

# This is to view cols composition of data set
sapply(dfaq1, typeof)

# To view data composition (head and tail of dataset)
head(dfaq1, n=10)
tail(dfaq1, n=10)

# Identify Total no of col rows and cols
dim(dfaq1)  

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2017: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) %% Data and Time col's are separated %% Status units cols are metric keys which always stays same
# so 5 cols can be excluded in stage 3 %% Number of NA's are much higher in PM2.5 variable (3982 out of 8760) %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------

# Checking data scheme, understating relationship of Independent Variables and Dependent Variables, data types and to filter unwanted variables.

glimpse(dfaq2)
str(dfaq2) # outlined data types of the data fr i.e. in dfaq2 we have 4 data types are in numerical shape, 1 as integer and the rest of the cols are string based.

# To view full summary of the entire dataset or choose col
summary(dfaq2) # This function has outlined mean, median, min, max and number of blanks in variable cols 
summary(dfaq2$PM25)

# This is to view cols composition of data set
sapply(dfaq2, typeof)

# To view data composition (head and tail of dataset)
head(dfaq2, n=10)
tail(dfaq2, n=10)

# Identify Total no of col rows and cols
dim(dfaq2)  

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2018: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) %% Data and Time col's are separated %% Status units cols are metric keys which always stays same
# so 5 cols can be excluded in stage 3 %% Number of NA's are limited in all DV's %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------

# Checking data scheme, understating relationship of Independent Variables and Dependent Variables, data types and to filter unwanted variables.

glimpse(dfaq3)
str(dfaq3) # outlined data types of the data fr i.e. in dfaq3 we have 4 data types are in numerical shape, 1 as integer and the rest of the cols are string based.

# To view full summary of the entire dataset or choose col
summary(dfaq3) # This function has outlined mean, median, min, max and number of blanks in variable cols 
summary(dfaq3$PM25)

# This is to view cols composition of data set
sapply(dfaq3, typeof)

# To view data composition (head and tail of dataset)
head(dfaq3, n=10)
tail(dfaq3, n=10)

# Identify Total no of col rows and cols
dim(dfaq3)  

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2019: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) %% Data and Time col's are separated %% Status units cols are metric keys which always stays same
# so 5 cols can be excluded in stage 3 %% Number of NA's are slightly higher in PM2.5 (342 out of 8740) %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------

# Checking data scheme, understating relationship of Independent Variables and Dependent Variables, data types and to filter unwanted variables.

glimpse(dfaq4)
str(dfaq2) # outlined data types of the data fr i.e. in dfaq4 we have 4 data types are in numerical shape, 1 as integer and the rest of the cols are string based.

# To view full summary of the entire dataset or choose col
summary(dfaq4) # This function has outlined mean, median, min, max and number of blanks in variable cols 
summary(dfaq4$PM25)

# This is to view cols composition of data set
sapply(dfaq4, typeof)

# To view data composition (head and tail of dataset)
head(dfaq4, n=10)
tail(dfaq4, n=10)

# Identify Total no of col rows and cols
dim(dfaq4)  

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2020: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) %% Data and Time col's are separated %% Status units cols are metric keys which always stays same
# so 5 cols can be excluded in stage 3 %% Number of NA's are slightly higher in all DV's %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------

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

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2021: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) %% Data and Time col's are separated %% Status units cols are metric keys which always stays same
# so 5 cols can be excluded in stage 3 %% Number of NA's are limited in all DV's %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------

# Missing Data Analysis using R Visualisations
plot(dfaq1)
plot(dfaq1$PM25)
plot(dfaq1$PM10, dfaq1$PM25)
plot(dfaq1$PM25, type = "b")
plot(dfaq1$PM25, type = "h")

barplot(dfaq1$PM25, horiz = TRUE, main = 'Barplot')

hist(dfaq1[3])

missing_values_2017 <- dfaq1 %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_2017

missing_values_2017 %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "#800080") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow Air Quality YR 2017 Data') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "#800080", face = "bold"))

# ==========================================================================

# remove all variables from the environment
rm(list=ls())

# ==========================================================================