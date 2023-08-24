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
# Data Analysis Stage 2: Data Exploration
# ==========================================================================

# Adding Extracted raw AQ CSV Files

dfaq1 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2017.csv", stringsAsFactors = FALSE) # Year 2017
dfaq2 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2018.csv", stringsAsFactors = FALSE) # Year 2018
dfaq3 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2019.csv", stringsAsFactors = FALSE) # Year 2019
dfaq4 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2020.csv", stringsAsFactors = FALSE) # Year 2020
dfaq5 <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_1_aq_datasets/s1_dfaq2021.csv", stringsAsFactors = FALSE) # Year 2021

#----------------------------------------------------



# ==========================================================================
# Stage 1 & 2 Pre-steps for Stage 3 Scripting
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

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2017: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) %% Data and Time col's are separated %% Number of NA's are much higher in PM2.5 variable (3982 out of 8760) %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------



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

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2018: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) 
# %% Number of NA's are limited in all DV's %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------



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

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2019: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) 
# %% Number of NA's are slightly higher in PM2.5 (342 out of 8740) %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------


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

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2020: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) %% Data and Time col's are separated
# %% Number of NA's are slightly higher in all DV's %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------



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

#----------------------------------------------------
# DATA OBSERVATION RECORDS FOR WRITE-UP

# YEAR 2021: Discovered independent Variables (PM10, NO, NO2, NOXasNO2 & PM2.5) %% Data and Time col's are separated %% Status units cols are metric keys which always stays same
# so 5 cols can be excluded in stage 3 %% Number of NA's are limited in all DV's %% Further Data Analysis are required on missing data in stage 3.
#----------------------------------------------------


# ==========================================================================
# All Datasets Visual Exploration for better understanding of areas of opportunities and to undisclosed patterns b/w pollutant variables
# ==========================================================================

# Task 1: Missing Data (NA's) Analysis using R Visualisations
# ...........................................................

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
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "#FFA500") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow Air Quality YR 2017 Data') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "#FFA500", face = "bold"))
# ------------------------------------------------------------------------
missing_values_2018 <- dfaq2 %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_2018

missing_values_2018 %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "#FFA500") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow Air Quality YR 2018 Data') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "#FFA500", face = "bold"))
# ------------------------------------------------------------------------
missing_values_2019 <- dfaq3 %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_2019

missing_values_2019 %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "#FFA500") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow Air Quality YR 2019 Data') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "#FFA500", face = "bold"))
# ------------------------------------------------------------------------
missing_values_2020 <- dfaq4 %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_2020

missing_values_2020 %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "#FFA500") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow Air Quality YR 2020 Data') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "#FFA500", face = "bold"))
# ------------------------------------------------------------------------
missing_values_2021 <- dfaq5 %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_2021

missing_values_2021 %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "#FFA500") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow Air Quality YR 2021 Data') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "#FFA500", face = "bold"))
# ------------------------------------------------------------------------


# Task 2: Combining all NA's into 1 table view
# ...........................................................
missing_values <- all_missing_data_list %>% reduce(left_join, by='key')
# Task 1 Findings on NA's  
# key      num.missing.x num.missing.y num.missing.x.x num.missing.y.y num.missing

# pm25              3982           103             342             372         430
# pm10               235           265             165             384         293
# no                  19            29             185             246          13
# no2                 19            29             185             246          13
# noxasno2            19            29             185             246          13

# ------------------------------------------------------------------------

# Creating data frame for every year

years <- c(2017, 2018, 2019, 2020, 2021)
pm10 <- c(235, 265, 165, 384, 293)
no <- c(19, 29, 185, 246, 13)
no2 <- c(19, 29, 185, 246, 13)
noxasno2 <- c(19, 29, 185, 246, 13)
pm25 <- c(3982, 103, 342, 371, 430)

full_missing_df <- data.frame(years, pm10, no, no2, noxasno2, pm25)

missing_df_pm10 <- data.frame(years, pm10)
missing_df_pm25 <- data.frame(years, pm25)
missing_df_no <- data.frame(years, no)
missing_df_no2 <- data.frame(years, no2)
missing_df_noxasno2 <- data.frame(years, noxasno2)

# ------------------------------------------------------------------------

# Task 3: Visulisations of missing data (variables) for over 5 years period
# ...........................................................

barplot(missing_df_pm10$pm10,
        main = "5 Years Missing Data Analysis for PM10", horiz = TRUE,
        col=c("pink","grey","orange","purple","black"),
        legend = c(2017, 2018, 2019, 2020, 2021),
        border = "orange",
        ylab = "Total Number of Missing Variables by Years", 
        names.arg = missing_df_pm10$pm10)

barplot(missing_df_pm25$pm25,
        main = "5 Years Missing Data Analysis for PM2.5", horiz = TRUE,
        col=c("pink","grey","orange","purple","black"),
        legend = c(2017, 2018, 2019, 2020, 2021),
        border = "orange",
        ylab = "Total Number of Missing Variables by Years", 
        names.arg = missing_df_pm25$pm25)

barplot(missing_df_no$no,
        main = "5 Years Missing Data Analysis for NO", horiz = TRUE,
        col=c("pink","grey","orange","purple","black"),
        legend = c(2017, 2018, 2019, 2020, 2021),
        border = "orange",
        ylab = "Total Number of Missing Variables by Years", 
        names.arg = missing_df_no$no)

barplot(missing_df_no2$no2,
        main = "5 Years Missing Data Analysis for NO2", horiz = TRUE,
        col=c("pink","grey","orange","purple","black"),
        legend = c(2017, 2018, 2019, 2020, 2021),
        border = "orange",
        ylab = "Total Number of Missing Variables by Years", 
        names.arg = missing_df_no2$no2)

barplot(missing_df_noxasno2$noxasno2,
        main = "5 Years Missing Data Analysis for NOxasNO2", horiz = TRUE,
        col=c("pink","grey","orange","purple","black"),
        legend = c(2017, 2018, 2019, 2020, 2021),
        border = "orange",
        ylab = "Total Number of Missing Variables by Years", 
        names.arg = missing_df_noxasno2$noxasno2)

# ------------------------------------------------------------------------

# Task 4: Overall view of yearly air quality data
# ...........................................................

plot(dfaq1) # Basic Overview of all Variables in Year 2017
plot(dfaq2) # Basic Overview of all Variables in Year 2018
plot(dfaq3) # Basic Overview of all Variables in Year 2019
plot(dfaq4) # Basic Overview of all Variables in Year 2020
plot(dfaq5) # Basic Overview of all Variables in Year 2021

# ------------------------------------------------------------------------

# Task 5: To explore outliers using visualization boxplots (per year) - Random Checks
# ...........................................................

boxplot.stats(dfaq1$pm10)$out
out <- boxplot.stats(dfaq1$pm10)$out

boxplot(dfaq1$pm10,
        main = "Boxplot for PM10 Variable in 2017",
        ylab = "PM10 Concentration",
        col = "pink",
        border = "blue")
#mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(dfaq3$pm10,
        main = "Boxplot for PM10 Variable in 2019",
        ylab = "PM10 Concentration",
        col = "pink",
        border = "blue")
#mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(dfaq2$pm25,
        main = "Boxplot for PM2.5 Variable in 2018",
        ylab = "PM2.5 Concentration",
        col = "pink",
        border = "blue")
#mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(dfaq4$no,
        main = "Boxplot for NO Variable in 2020",
        ylab = "NO Concentration",
        col = "pink",
        border = "blue")
#mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(dfaq1$no,
        main = "Boxplot for NO Variable in 2017",
        ylab = "NO Concentration",
        col = "pink",
        border = "blue")
#mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(dfaq3$no2,
        main = "Boxplot for NO2 Variable in 2019",
        ylab = "NO2 Concentration",
        col = "pink",
        border = "blue")
#mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(dfaq5$no2,
        main = "Boxplot for NO2 Variable in 2021",
        ylab = "NO2 Concentration",
        col = "pink",
        border = "blue")
#mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(dfaq5$noxasno2,
        main = "Boxplot for NOxasNO2 Variable in 2021",
        ylab = "NOxasNO2 Concentration",
        col = "pink",
        border = "blue")
#mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(dfaq4$noxasno2,
        main = "Boxplot for NOxasNO2 Variable in 2020",
        ylab = "NOxasNO2 Concentration",
        col = "pink",
        border = "blue")
#mtext(paste("Outliers: ", paste(out, collapse = ", ")))

# ==========================================================================

# remove all variables from the environment
rm(list=ls())

# ==========================================================================