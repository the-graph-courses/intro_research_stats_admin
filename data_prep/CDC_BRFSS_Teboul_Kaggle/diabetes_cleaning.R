#############################################
# Diabetes Analysis in R
# Author: shivashankari06 (Based on Kaggle Notebook)
# Date: [Insert Date]
#
# This script performs an exploratory data analysis (EDA) and builds
# a logistic regression model to study diabetes outcomes.
#############################################

#' https://www.kaggle.com/code/shivashankari06/diabetes-analysis-in-r/notebook

# Step 1: Load the data and required packages  ----

## To ensure that the dataset is loaded and necessary libraries are installed)

# Load necessary libraries
library(tidyverse)
library(MASS)

# Read the data set
data <- read.csv("/kaggle/input/diabetic-dataset/diabetes_dataset.csv")

# Convert relevant variables to categorical factors
data$HighBP <- factor(data$HighBP, levels = c(0, 1), 
                      labels = c("No High BP", "High BP"))
data$HighChol <- factor(data$HighChol, levels = c(0, 1), 
                        labels = c("No High Chol", "High Chol"))
data$PhysActivity <- factor(data$PhysActivity, levels = c(0, 1), 
                            labels = c("No", "Yes"))
data$Diabetes_binary <- factor(data$Diabetes_binary, levels = c(0, 1), 
                               labels = c("No Diabetes", "Diabetes or Pre-diabetes"))

# Step 2: Descriptive Statistics ------

## Summarise the data to get an understanding of the distribution) BMI 

# Summary for numeric variable (BMI)
bmi_summary <- summary(data$BMI)

# Create a data frame for BMI summary
bmi_summary_df <- data.frame(
  Statistic = names(bmi_summary),
  BMI = as.numeric(bmi_summary)
)

# Numeric variable (BMI) summary table
kable(bmi_summary_df, caption = "Summary Statistics for BMI")

## Summary Statistics for various variables







