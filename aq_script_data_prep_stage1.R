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

raw_dfaq1 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2017.csv", stringsAsFactors = FALSE) # Year 2017
raw_dfaq3 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2018.csv", stringsAsFactors = FALSE) # Year 2018
raw_dfaq3 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2019.csv", stringsAsFactors = FALSE) # Year 2019
raw_dfaq4 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2020.csv", stringsAsFactors = FALSE) # Year 2020
raw_dfaq5 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/raw_aq_datasets/hounslow_raw_aq_2021.csv", stringsAsFactors = FALSE) # Year 2021

#----------------------------------------------------

# 2017 Dataset Preparation and Notes:
# ==========================================================================
# To check composition of raw data sources 

head(raw_dfaq1, n=10)
tail(raw_dfaq1, n=10)

# Identify Total no of col rows and cols
dim(raw_dfaq1) 

#----------------------------------------------------
# Data Prep steps to clean up dataset 

# To make all cols in lowercase
names(raw_dfaq1)
names(raw_dfaq1) <- tolower(names(raw_dfaq1)) 
names(raw_dfaq1)

# Remove unwanted 'status.units' cols as they always stays the same and repetitive 
raw_dfaq1$status.units <- NULL
raw_dfaq1$status.units.1 <- NULL
raw_dfaq1$status.units.2 <- NULL
raw_dfaq1$status.units.3 <- NULL
raw_dfaq1$status.units.4 <- NULL

# To check and convert date col into the right data type
head(raw_dfaq1)
str(raw_dfaq1)
class(raw_dfaq1$end.date)
raw_dfaq1$end.date <- as.Date(raw_dfaq1$end.date, format = "%d/%m/%Y")
#raw_dfaq1$end.time <- as.POSIXct(raw_dfaq1$end.time, format="%H:%M:%S") # additional step - may required later

# limit digits to max. of 1 for all numerical values
raw_dfaq1$pm10 <- sprintf(raw_dfaq1$pm10, fmt = '%#.1f')
raw_dfaq1$no <- sprintf(raw_dfaq1$no, fmt = '%#.1f')
raw_dfaq1$no2 <- sprintf(raw_dfaq1$no2, fmt = '%#.1f')
raw_dfaq1$noxasno2 <- sprintf(raw_dfaq1$noxasno2, fmt = '%#.1f')
raw_dfaq1$pm25 <- sprintf(raw_dfaq1$pm25, fmt = '%#.1f')

str(raw_dfaq1)

# Change char. class to numerical of all pollutant variables 
raw_dfaq1$pm10 <- as.numeric(raw_dfaq1$pm10)
raw_dfaq1$no <- as.numeric(raw_dfaq1$no)
raw_dfaq1$no2 <- as.numeric(raw_dfaq1$no2)
raw_dfaq1$noxasno2 <- as.numeric(raw_dfaq1$noxasno2)
raw_dfaq1$pm25 <- as.numeric(raw_dfaq1$pm25)

summary(raw_dfaq1)

# Stage 1 cleared dataset 
s1_dfaq1 <- raw_dfaq1

rm(raw_dfaq1)

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2017: Col Titles have been standardised (lowercase) + removed 5 unwanted duplicated cols (status.units) + changed End.Date col into Date Data Type (Class) +
# Changed class of all pollutants to numerical + limited digits to max. of 1 for all numerical values + Removed RAW Dataset from the R Env.
#----------------------------------------------------



# 2018 Dataset Preparation and Notes:
# ==========================================================================
# To check composition of raw data sources 

head(raw_dfaq2, n=10)
tail(raw_dfaq2, n=10)

# Identify Total no of col rows and cols
dim(raw_dfaq2) 

#----------------------------------------------------
# Data Prep steps to clean up dataset 

# To make all cols in lowercase
names(raw_dfaq2)
names(raw_dfaq2) <- tolower(names(raw_dfaq2)) 
names(raw_dfaq2)

# Remove unwanted 'status.units' cols as they always stays the same and repetitive 
raw_dfaq2$status.units <- NULL
raw_dfaq2$status.units.1 <- NULL
raw_dfaq2$status.units.2 <- NULL
raw_dfaq2$status.units.3 <- NULL
raw_dfaq2$status.units.4 <- NULL

# To check and convert date col into the right data type
head(raw_dfaq2)
str(raw_dfaq2)
class(raw_dfaq2$end.date)
raw_dfaq2$end.date <- as.Date(raw_dfaq2$end.date, format = "%d/%m/%Y")
#raw_dfaq2$end.time <- as.POSIXct(raw_dfaq2$end.time, format="%H:%M:%S") # additional step - may required later

# limit digits to max. of 1 for all numerical values
raw_dfaq2$pm10 <- sprintf(raw_dfaq2$pm10, fmt = '%#.1f')
raw_dfaq2$no <- sprintf(raw_dfaq2$no, fmt = '%#.1f')
raw_dfaq2$no2 <- sprintf(raw_dfaq2$no2, fmt = '%#.1f')
raw_dfaq2$noxasno2 <- sprintf(raw_dfaq2$noxasno2, fmt = '%#.1f')
raw_dfaq2$pm25 <- sprintf(raw_dfaq2$pm25, fmt = '%#.1f')

str(raw_dfaq2)

# Change char. class to numerical of all pollutant variables 
raw_dfaq2$pm10 <- as.numeric(raw_dfaq2$pm10)
raw_dfaq2$no <- as.numeric(raw_dfaq2$no)
raw_dfaq2$no2 <- as.numeric(raw_dfaq2$no2)
raw_dfaq2$noxasno2 <- as.numeric(raw_dfaq2$noxasno2)
raw_dfaq2$pm25 <- as.numeric(raw_dfaq2$pm25)

summary(raw_dfaq2)

# Stage 1 cleared dataset 
s2_dfaq2 <- raw_dfaq2

rm(raw_dfaq2)

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2018: Col Titles have been standardised (lowercase) + removed 5 unwanted duplicated cols (status.units) + changed End.Date col into Date Data Type (Class) +
# Changed class of all pollutants to numerical + limited digits to max. of 1 for all numerical values + Removed RAW Dataset from the R Env.
#----------------------------------------------------



# 2019 Dataset Preparation and Notes:
# ==========================================================================
# To check composition of raw data sources 

head(raw_dfaq3, n=10)
tail(raw_dfaq3, n=10)

# Identify Total no of col rows and cols
dim(raw_dfaq3) 

#----------------------------------------------------
# Data Prep steps to clean up dataset 

# To make all cols in lowercase
names(raw_dfaq3)
names(raw_dfaq3) <- tolower(names(raw_dfaq3)) 
names(raw_dfaq3)

# Remove unwanted 'status.units' cols as they always stays the same and repetitive 
raw_dfaq3$status.units <- NULL
raw_dfaq3$status.units.1 <- NULL
raw_dfaq3$status.units.2 <- NULL
raw_dfaq3$status.units.3 <- NULL
raw_dfaq3$status.units.4 <- NULL

# To check and convert date col into the right data type
head(raw_dfaq3)
str(raw_dfaq3)
class(raw_dfaq3$end.date)
raw_dfaq3$end.date <- as.Date(raw_dfaq3$end.date, format = "%d/%m/%Y")
#raw_dfaq3$end.time <- as.POSIXct(raw_dfaq3$end.time, format="%H:%M:%S") # additional step - may required later

# limit digits to max. of 1 for all numerical values
raw_dfaq3$pm10 <- sprintf(raw_dfaq3$pm10, fmt = '%#.1f')
raw_dfaq3$no <- sprintf(raw_dfaq3$no, fmt = '%#.1f')
raw_dfaq3$no2 <- sprintf(raw_dfaq3$no2, fmt = '%#.1f')
raw_dfaq3$noxasno2 <- sprintf(raw_dfaq3$noxasno2, fmt = '%#.1f')
raw_dfaq3$pm25 <- sprintf(raw_dfaq3$pm25, fmt = '%#.1f')

str(raw_dfaq3)

# Change char. class to numerical of all pollutant variables 
raw_dfaq3$pm10 <- as.numeric(raw_dfaq3$pm10)
raw_dfaq3$no <- as.numeric(raw_dfaq3$no)
raw_dfaq3$no2 <- as.numeric(raw_dfaq3$no2)
raw_dfaq3$noxasno2 <- as.numeric(raw_dfaq3$noxasno2)
raw_dfaq3$pm25 <- as.numeric(raw_dfaq3$pm25)

summary(raw_dfaq3)

# Stage 1 cleared dataset 
s3_dfaq3 <- raw_dfaq3

rm(raw_dfaq3)

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2019: Col Titles have been standardised (lowercase) + removed 5 unwanted duplicated cols (status.units) + changed End.Date col into Date Data Type (Class) +
# Changed class of all pollutants to numerical + limited digits to max. of 1 for all numerical values + Removed RAW Dataset from the R Env.
#----------------------------------------------------



# 2020 Dataset Preparation and Notes:
# ==========================================================================
# To check composition of raw data sources 

head(raw_dfaq4, n=10)
tail(raw_dfaq4, n=10)

# Identify Total no of col rows and cols
dim(raw_dfaq4) 

#----------------------------------------------------
# Data Prep steps to clean up dataset 

# To make all cols in lowercase
names(raw_dfaq4)
names(raw_dfaq4) <- tolower(names(raw_dfaq4)) 
names(raw_dfaq4)

# Remove unwanted 'status.units' cols as they always stays the same and repetitive 
raw_dfaq4$status.units <- NULL
raw_dfaq4$status.units.1 <- NULL
raw_dfaq4$status.units.2 <- NULL
raw_dfaq4$status.units.3 <- NULL
raw_dfaq4$status.units.4 <- NULL

# To check and convert date col into the right data type
head(raw_dfaq4)
str(raw_dfaq4)
class(raw_dfaq4$end.date)
raw_dfaq4$end.date <- as.Date(raw_dfaq4$end.date, format = "%d/%m/%Y")
#raw_dfaq4$end.time <- as.POSIXct(raw_dfaq4$end.time, format="%H:%M:%S") # additional step - may required later

# limit digits to max. of 1 for all numerical values
raw_dfaq4$pm10 <- sprintf(raw_dfaq4$pm10, fmt = '%#.1f')
raw_dfaq4$no <- sprintf(raw_dfaq4$no, fmt = '%#.1f')
raw_dfaq4$no2 <- sprintf(raw_dfaq4$no2, fmt = '%#.1f')
raw_dfaq4$noxasno2 <- sprintf(raw_dfaq4$noxasno2, fmt = '%#.1f')
raw_dfaq4$pm25 <- sprintf(raw_dfaq4$pm25, fmt = '%#.1f')

str(raw_dfaq4)

# Change char. class to numerical of all pollutant variables 
raw_dfaq4$pm10 <- as.numeric(raw_dfaq4$pm10)
raw_dfaq4$no <- as.numeric(raw_dfaq4$no)
raw_dfaq4$no2 <- as.numeric(raw_dfaq4$no2)
raw_dfaq4$noxasno2 <- as.numeric(raw_dfaq4$noxasno2)
raw_dfaq4$pm25 <- as.numeric(raw_dfaq4$pm25)

summary(raw_dfaq4)

# Stage 1 cleared dataset 
s4_dfaq4 <- raw_dfaq4

rm(raw_dfaq4)

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2020: Col Titles have been standardised (lowercase) + removed 5 unwanted duplicated cols (status.units) + changed End.Date col into Date Data Type (Class) +
# Changed class of all pollutants to numerical + limited digits to max. of 1 for all numerical values + Removed RAW Dataset from the R Env.
#----------------------------------------------------



# 2021 Dataset Preparation and Notes:
# ==========================================================================
# To check composition of raw data sources 

head(raw_dfaq5, n=10)
tail(raw_dfaq5, n=10)

# Identify Total no of col rows and cols
dim(raw_dfaq5) 

#----------------------------------------------------
# Data Prep steps to clean up dataset 

# To make all cols in lowercase
names(raw_dfaq5)
names(raw_dfaq5) <- tolower(names(raw_dfaq5)) 
names(raw_dfaq5)

# Remove unwanted 'status.units' cols as they always stays the same and repetitive 
raw_dfaq5$status.units <- NULL
raw_dfaq5$status.units.1 <- NULL
raw_dfaq5$status.units.2 <- NULL
raw_dfaq5$status.units.3 <- NULL
raw_dfaq5$status.units.4 <- NULL

# To check and convert date col into the right data type
head(raw_dfaq5)
str(raw_dfaq5)
class(raw_dfaq5$end.date)
raw_dfaq5$end.date <- as.Date(raw_dfaq5$end.date, format = "%d/%m/%Y")
#raw_dfaq5$end.time <- as.POSIXct(raw_dfaq5$end.time, format="%H:%M:%S") # additional step - may required later

# limit digits to max. of 1 for all numerical values
raw_dfaq5$pm10 <- sprintf(raw_dfaq5$pm10, fmt = '%#.1f')
raw_dfaq5$no <- sprintf(raw_dfaq5$no, fmt = '%#.1f')
raw_dfaq5$no2 <- sprintf(raw_dfaq5$no2, fmt = '%#.1f')
raw_dfaq5$noxasno2 <- sprintf(raw_dfaq5$noxasno2, fmt = '%#.1f')
raw_dfaq5$pm25 <- sprintf(raw_dfaq5$pm25, fmt = '%#.1f')

str(raw_dfaq5)

# Change char. class to numerical of all pollutant variables 
raw_dfaq5$pm10 <- as.numeric(raw_dfaq5$pm10)
raw_dfaq5$no <- as.numeric(raw_dfaq5$no)
raw_dfaq5$no2 <- as.numeric(raw_dfaq5$no2)
raw_dfaq5$noxasno2 <- as.numeric(raw_dfaq5$noxasno2)
raw_dfaq5$pm25 <- as.numeric(raw_dfaq5$pm25)

summary(raw_dfaq5)

# Stage 1 cleared dataset 
s5_dfaq5 <- raw_dfaq5

rm(raw_dfaq5)

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2021: Col Titles have been standardised (lowercase) + removed 5 unwanted duplicated cols (status.units) + changed End.Date col into Date Data Type (Class) +
# Changed class of all pollutants to numerical + limited digits to max. of 1 for all numerical values + Removed RAW Dataset from the R Env.
#----------------------------------------------------

# ==========================================================================
# Write CSV's for the NEXT Stage Script 
s1_dfaq1 <- readr::write_csv(s1_dfaq1, file = "C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2017.csv")
s2_dfaq2 <- readr::write_csv(s2_dfaq2, file = "C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2018.csv")
s3_dfaq3 <- readr::write_csv(s3_dfaq3, file = "C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2019.csv")
s4_dfaq4 <- readr::write_csv(s4_dfaq4, file = "C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2020.csv")
s5_dfaq5 <- readr::write_csv(s5_dfaq5, file = "C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2021.csv")

# ==========================================================================

# ==========================================================================

# To remove all variables from the environment (if req.)
rm(list=ls())

# ==========================================================================