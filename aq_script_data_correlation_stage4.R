# This script is to perform data analysis for CN8001 project evaluation
# This Script is written by:
# --------------------------
# ---- Ejaz Hussain --------
# ---- Data Scientist ------
# ---- LB of Hounslow ------
# ---- LU: April 2023 ------
# --------------------------

# Raw datasets contain London (Hounslow) Air Quality data which consist on 5 years of hourly generated data
# The data exact hotspot is Chiswick where the weather station is installed and operated by LB of Hounslow 
# All extracted data is OPEN and Widely available on Ricardo's Online Platform

#----------------------------------------------------

# Required Libraries

library("tidyverse")
library("tidyr")
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
# Data Correlation Analysis Stage 4: Correlation & Conclusion
# ==========================================================================

# Adding Extracted raw AQ CSV Files

aq_refined_df <- read.csv("C:/Users/ejaz.hussain/Desktop/aq_data_project/stage_4_aq_datasets/aq_refined_df.csv", stringsAsFactors = FALSE) # Refined Dataset from Stage 3


# ==========================================================================
# Stage 1 & 2: Data Prep Exploration Steps for Stage 4
# ==========================================================================


# AQ Refined Dataset Preparation and Notes:
# ==========================================================================

# Pre-Steps for Data Preparation in S1
aq_refined_df$end.date <- as.Date(aq_refined_df$end.date)
aq_refined_df$pm10 <- as.numeric(aq_refined_df$pm10)
aq_refined_df$no <- as.numeric(aq_refined_df$no)
aq_refined_df$no2 <- as.numeric(aq_refined_df$no2)
aq_refined_df$noxasno2 <- as.numeric(aq_refined_df$noxasno2)
aq_refined_df$pm25 <- as.numeric(aq_refined_df$pm25)

# Re-checking data composition
glimpse(aq_refined_df)
str(aq_refined_df) # outlined data types of the dataset
summary(aq_refined_df) # To view full summary of the entire dataset or choose col

# This is to view cols composition of data set
sapply(aq_refined_df, typeof)

# To view data composition (head and tail of dataset)
head(aq_refined_df, n=10)
tail(aq_refined_df, n=10)

# Identify Total no of col rows and cols
dim(aq_refined_df)  


# ==========================================================================
# Stage 4: Data Correlation Analysis
# ==========================================================================
# Task 1: Applying Pearson Correlation Technique to see relationships b/w independent variables (AQ Pollutants)
# ------------------------------------------------------

# Check the correlation between PM10 and PM25 variables
pearson_test1 <- cor.test(aq_refined_df$pm10, aq_refined_df$pm25, method="pearson")
pearson_test1
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Positive Relationship b/w Variables
# Pearson's product-moment correlation
# data:  aq_refined_df$pm10 and aq_refined_df$pm25
# t = 150.26, df = 43822, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5769125 0.5892707
# sample estimates:
#  cor 
# 0.5831253 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO and NO2 variables
pearson_test2 <- cor.test(aq_refined_df$no, aq_refined_df$no2, method="pearson")
pearson_test2
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Strong Positive Relationship b/w Variables
# Pearson's product-moment correlation

# data:  aq_refined_df$no and aq_refined_df$no2
# t = 216.73, df = 43822, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.7147258 0.7237637
# sample estimates:
#      cor 
# 0.7192752
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Check the correlation between NO2 and PM2.5 variables
pearson_test3 <- cor.test(aq_refined_df$no2, aq_refined_df$pm25, method="pearson")
pearson_test3
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Positive Relationship b/w Variables
# Pearson's product-moment correlation
# 
# data:  aq_refined_df$no2 and aq_refined_df$pm25
# t = 103.89, df = 43822, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.436991 0.452016
# sample estimates:
#       cor 
# 0.4445348 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO and NOxasNO2 variables
pearson_test4 <- cor.test(aq_refined_df$no, aq_refined_df$noxasno2, method="pearson")
pearson_test4
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Very Strong Positive Relationship b/w Variables
# Pearson's product-moment correlation
# 
# data:  aq_refined_df$no and aq_refined_df$noxasno2
# t = 603.29, df = 43822, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9437256 0.9457380
# sample estimates:
#       cor 
# 0.9447407 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO2 and NOxasNO2 variables
pearson_test5 <- cor.test(aq_refined_df$no2, aq_refined_df$noxasno2, method="pearson")
pearson_test5
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Strong Positive Relationship b/w Variables
# Pearson's product-moment correlation
# 
# data:  aq_refined_df$no2 and aq_refined_df$noxasno2
# t = 319.86, df = 43822, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.8339008 0.8395164
# sample estimates:
#       cor 
# 0.8367306 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Task 1 Conclusion Matrix: 

Correlation_BW <- c("PM10 and PM25", "NO and NO2", "NO2 and PM2.5", "NO and NOxasNO2", "NO2 and NOxasNO2")
Pearson_Matrix <- c(pearson_test1$estimate, pearson_test2$estimate, pearson_test3$estimate, pearson_test4$estimate, pearson_test5$estimate)
df_pearson <- data.frame(Correlation_BW, Pearson_Matrix)
df_pearson

barplot(df_pearson$Pearson_Matrix,
        main = "Pearson Correlation Matrix",
        col = c("yellow", "red", "green", "orange", "purple"),
        border = "blue",
        names.arg = df_pearson$Correlation_BW,
        xlab = "Correlation B/W Pollutant Variables")

# ------------------------------------------------------
# Task 2: Applying Spearman Correlation Technique to see relationships b/w independent variables (AQ Pollutants)
# ------------------------------------------------------

# Check the correlation between PM10 and PM25 variables
spearman_test1 <- cor.test(aq_refined_df$pm10, aq_refined_df$pm25, method = "spearman")
spearman_test1

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Positive Relationship b/w Variables
# Spearman's rank correlation rho
# 
# data:  aq_refined_df$pm10 and aq_refined_df$pm25
# S = 5.6109e+12, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.600012 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO and NO2 variables
spearman_test2 <- cor.test(aq_refined_df$no, aq_refined_df$no2, method = "spearman")
spearman_test2

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Strong Positive Relationship b/w Variables
# Spearman's rank correlation rho
# 
# data:  aq_refined_df$no and aq_refined_df$no2
# S = 2.7032e+12, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.8072944 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO2 and PM2.5 variables
spearman_test3 <- cor.test(aq_refined_df$no2, aq_refined_df$pm25, method = "spearman")
spearman_test3

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Positive Relationship b/w Variables
# Spearman's rank correlation rho
# 
# data:  aq_refined_df$no2 and aq_refined_df$pm25
# S = 7.0432e+12, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.4979041 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO and NOxasNO2 variables
spearman_test4 <- cor.test(aq_refined_df$no, aq_refined_df$noxasno2, method = "spearman")
spearman_test4

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Very Strong Positive Relationship b/w Variables
# Spearman's rank correlation rho
# 
# data:  aq_refined_df$no and aq_refined_df$noxasno2
# S = 5.5425e+11, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.9604885 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO2 and NOxasNO2 variables
spearman_test5 <- cor.test(aq_refined_df$no2, aq_refined_df$noxasno2, method = "spearman")
spearman_test5

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Very Strong Positive Relationship b/w Variables
# Spearman's rank correlation rho
# 
# data:  aq_refined_df$no2 and aq_refined_df$noxasno2
# S = 1.1011e+12, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.9215038
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Task 2 Conclusion Matrix: 

#Correlation_BW <- c("PM10 and PM25", "NO and NO2", "NO2 and PM2.5", "NO and NOxasNO2", "NO2 and NOxasNO2")
Spearman_Matrix <- c(spearman_test1$estimate, spearman_test2$estimate, spearman_test3$estimate, spearman_test4$estimate, spearman_test5$estimate)
df_spearman <- data.frame(Correlation_BW, Spearman_Matrix)
df_spearman

barplot(df_spearman$Spearman_Matrix,
        main = "Spearman Correlation Matrix",
        col = c("yellow", "red", "green", "orange", "purple"),
        border = "blue",
        names.arg = df_spearman$Correlation_BW,
        xlab = "Correlation B/W Pollutant Variables")

# ------------------------------------------------------
# Task 3: Applying Kendall's Tau Correlation Technique to see relationships b/w independent variables (AQ Pollutants)
# ------------------------------------------------------

# Check the correlation between PM10 and PM25 variables
kendall_test1 <- cor.test(aq_refined_df$pm10, aq_refined_df$pm25, method = "kendall")
kendall_test1

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Positive Relationship b/w Variables
# Kendall's rank correlation tau
# 
# data:  aq_refined_df$pm10 and aq_refined_df$pm25
# z = 132.97, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.4362847
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO and NO2 variables
kendall_test2 <- cor.test(aq_refined_df$no, aq_refined_df$no2, method = "kendall")
kendall_test2

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Strong Positive Relationship b/w Variables
# Kendall's rank correlation tau
# 
# data:  aq_refined_df$no and aq_refined_df$no2
# z = 193.26, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.6169328
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO2 and PM2.5 variables
kendall_test3 <- cor.test(aq_refined_df$no2, aq_refined_df$pm25, method = "kendall")
kendall_test3

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Positive Relationship b/w Variables
# Kendall's rank correlation tau
# 
# data:  aq_refined_df$no2 and aq_refined_df$pm25
# z = 106.93, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.3503702
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO and NOxasNO2 variables
kendall_test4 <- cor.test(aq_refined_df$no, aq_refined_df$noxasno2, method = "kendall")
kendall_test4

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Very Strong Positive Relationship b/w Variables
# Kendall's rank correlation tau
# 
# data:  aq_refined_df$no and aq_refined_df$noxasno2
# z = 263.2, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.8400539 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check the correlation between NO2 and NOxasNO2 variables
kendall_test5 <- cor.test(aq_refined_df$no2, aq_refined_df$noxasno2, method = "kendall")
kendall_test5

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Results Outcome: Very Strong Positive Relationship b/w Variables
# Kendall's rank correlation tau
# 
# data:  aq_refined_df$no2 and aq_refined_df$noxasno2
# z = 240.43, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.7670019 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Task 3 Conclusion Matrix: 

#Correlation_BW <- c("PM10 and PM25", "NO and NO2", "NO2 and PM2.5", "NO and NOxasNO2", "NO2 and NOxasNO2")
Kendall_Matrix <- c(kendall_test1$estimate, kendall_test2$estimate, kendall_test3$estimate, kendall_test4$estimate, kendall_test5$estimate)
df_kendall <- data.frame(Correlation_BW, Kendall_Matrix)
df_kendall

barplot(df_kendall$Kendall_Matrix,
        main = "Kendall's Tau Correlation Matrix",
        col = c("yellow", "red", "green", "orange", "purple"),
        border = "blue",
        names.arg = df_kendall$Correlation_BW,
        xlab = "Correlation B/W Pollutant Variables")

# ------------------------------------------------------

# Combining all above tasks (1,2 & 3) for a comparative analysis b/w 3 different methods:

df_all_matrix <- data.frame(Correlation_BW, Pearson_Matrix, Spearman_Matrix, Kendall_Matrix)
df_all_matrix

# Correlation_BW Pearson_Matrix Spearman_Matrix Kendall_Matrix
# 1    PM10 and PM25      0.5831253       0.6000120      0.4362847
# 2       NO and NO2      0.7192752       0.8072944      0.6169328
# 3    NO2 and PM2.5      0.4445348       0.4979041      0.3503702
# 4  NO and NOxasNO2      0.9447407       0.9604885      0.8400539
# 5 NO2 and NOxasNO2      0.8367306       0.9215038      0.7670019

# Create A Grouped Visual for easy comparative analysis 

# create data frame
data <- data.frame(
  Variables = c("PM10 and PM25", "NO and NO2", "NO2 and PM2.5", "NO and NOxasNO2", "NO2 and NOxasNO2"),
  Pearson = c(0.5831253, 0.7192752, 0.4445348, 0.9447407, 0.8367306),
  Kendall = c(0.6000120, 0.8072944, 0.4979041, 0.9604885, 0.9215038),
  Spearman = c(0.4362847, 0.6169328, 0.3503702, 0.8400539, 0.7670019)
)

# convert data from wide to long format
data_long <- data %>% 
  pivot_longer(cols = Pearson:Spearman, names_to = "Correlation Type", values_to = "Value")

# create grouped bar plot
ggplot(data_long, aes(x = Variables, y = Value, fill = `Correlation Type`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Variables", y = "Correlation Coefficient", fill = "Correlation Type") + 
  scale_fill_discrete(name = "Correlation Type", labels = c("Pearson", "Kendall", "Spearman"))



# ------------------------------------------------------
# Task 4: Applying Visual Scatter plot with Regression Slope method show correlation coefficient relationship b/w pollutants variables
# ------------------------------------------------------

# Visual Correlation Coefficient b/w PM10 and PM2.5 with Regression Slope
plot(aq_refined_df$pm10, aq_refined_df$pm25, main = "Scatter Plot of PM10 and PM25 - Visual Correlation Coefficient",
     xlab = "PM10", ylab = "PM25")
abline(lm(aq_refined_df$pm25 ~ aq_refined_df$pm10), col = "red")
# Obtain the slope of the regression line
model1 <- lm(aq_refined_df$pm25 ~ aq_refined_df$pm10)
slope1 <- coef(model)[2]
slope1 #0.39

# Visual Correlation Coefficient b/w NO and NO2 with Regression Slope
plot(aq_refined_df$no, aq_refined_df$no2, main = "Scatter Plot of NO and NO2 - Visual Correlation Coefficient",
     xlab = "NO", ylab = "NO2")
abline(lm(aq_refined_df$no2 ~ aq_refined_df$no), col = "red")
# Obtain the slope of the regression line
model2 <- lm(aq_refined_df$no2 ~ aq_refined_df$no)
slope2 <- coef(model2)[2]
slope2 #0.58

# Visual Correlation Coefficient b/w NO2 and PM2.5 with Regression Slope
plot(aq_refined_df$no2, aq_refined_df$pm25, main = "Scatter Plot of NO2 and PM2.5 - Visual Correlation Coefficient",
     xlab = "NO2", ylab = "PM2.5")
abline(lm(aq_refined_df$pm25 ~ aq_refined_df$no2), col = "red")
# Obtain the slope of the regression line
model3 <- lm(aq_refined_df$pm25 ~ aq_refined_df$no2)
slope3 <- coef(model3)[2]
slope3 #0.12

# Visual Correlation Coefficient b/w NO and NOxasNO2 with Regression Slope
plot(aq_refined_df$no, aq_refined_df$noxasno2, main = "Scatter Plot of NO and NOxasNO2 - Visual Correlation Coefficient",
     xlab = "NO", ylab = "NOxasNO2")
abline(lm(aq_refined_df$noxasno2 ~ aq_refined_df$no), col = "red")
# Obtain the slope of the regression line
model4 <- lm(aq_refined_df$noxasno2 ~ aq_refined_df$no)
slope4 <- coef(model4)[2]
slope4 #2.11

# Visual Correlation Coefficient b/w NO2 and NOxasNO2 with Regression Slope
plot(aq_refined_df$no2, aq_refined_df$noxasno2, main = "Scatter Plot of NO2 and NOxasNO2 - Visual Correlation Coefficient",
     xlab = "NO2", ylab = "NOxasNO2")
abline(lm(aq_refined_df$noxasno2 ~ aq_refined_df$no2), col = "red")
# Obtain the slope of the regression line
model5 <- lm(aq_refined_df$noxasno2 ~ aq_refined_df$no2)
slope5 <- coef(model5)[2]
slope5 #2.31

# ==========================================================================

# remove all variables from the environment
rm(list=ls())

# ==========================================================================