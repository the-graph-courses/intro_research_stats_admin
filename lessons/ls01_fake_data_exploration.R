# Load necessary libraries
# Although base R plotting functions are used here (hist, barplot) you may also use ggplot2 for advanced plots.
library(ggplot2)   # For advanced plotting if desired
library(dplyr)     # For data manipulation

# If the CSV file "public_health.csv" does not exist in the working directory,
# create a sample dataset and write it to CSV.
if (!file.exists("public_health.csv")) {
  set.seed(123)  # Ensure reproducibility
  n <- 200  # Number of observations
  
  # Create synthetic public health dataset:
  public_health <- data.frame(
    Age = round(runif(n, min = 18, max = 90)),  # Continuous variable: Age
    BMI = round(rnorm(n, mean = 25, sd = 4), 1),  # Continuous variable: BMI
    DoctorVisits = rpois(n, lambda = 2),  # Discrete variable: count of visits
    HealthStatus = factor(
      sample(c("Poor", "Fair", "Good", "Very Good", "Excellent"), n, replace = TRUE),
      levels = c("Poor", "Fair", "Good", "Very Good", "Excellent"),
      ordered = TRUE
    ),  # Ordinal variable: Health Status
    Gender = factor(sample(c("Male", "Female"), n, replace = TRUE))  # Nominal variable: Gender
  )
  
  # Write the synthetic dataset to a CSV file so it can be read later.
  write.csv(public_health, "public_health.csv", row.names = FALSE)
}

# Read the dataset from the CSV file
data <- read.csv("public_health.csv", stringsAsFactors = TRUE)

# Inspect the structure and summary of the dataset
str(data)
summary(data)

# =============================
# Continuous Variables: Age & BMI
# =============================

# Summary for Age
cat("Summary Statistics for Age (Continuous):\n")
age_summary <- summary(data$Age)
print(age_summary)
cat("Standard Deviation:", sd(data$Age), "\n\n")

# Histogram for Age
hist(data$Age, main = "Histogram of Age", 
     xlab = "Age", col = "lightblue", border = "white")

# Summary for BMI
cat("Summary Statistics for BMI (Continuous):\n")
bmi_summary <- summary(data$BMI)
print(bmi_summary)
cat("Standard Deviation:", sd(data$BMI), "\n\n")

# Histogram for BMI
hist(data$BMI, main = "Histogram of BMI", 
     xlab = "BMI", col = "lightgreen", border = "white")

# =============================
# Discrete Variable: DoctorVisits
# =============================

# Summary for DoctorVisits
cat("Summary Statistics for DoctorVisits (Discrete):\n")
doctor_visits_summary <- summary(data$DoctorVisits)
print(doctor_visits_summary)

# Frequency table for discrete variable
doctor_visits_table <- table(data$DoctorVisits)
print(doctor_visits_table)

# Bar plot for DoctorVisits counts
barplot(doctor_visits_table, main = "Bar Plot of Doctor Visits", 
        xlab = "Number of Visits", ylab = "Frequency", 
        col = "orange", border = "white")

# =============================
# Ordinal Variable: HealthStatus
# =============================

# Summary for HealthStatus (Ordinal)
cat("Summary Statistics for HealthStatus (Ordinal):\n")
health_status_summary <- summary(data$HealthStatus)
print(health_status_summary)

# Bar plot for HealthStatus
health_status_table <- table(data$HealthStatus)
barplot(health_status_table, main = "Bar Plot of Health Status", 
        xlab = "Health Status", ylab = "Frequency", 
        col = "violet", border = "white")

# =============================
# Nominal Variable: Gender
# =============================

# Summary for Gender (Nominal)
cat("Summary Statistics for Gender (Nominal):\n")
gender_summary <- summary(data$Gender)
print(gender_summary)

# Bar plot for Gender
gender_table <- table(data$Gender)
barplot(gender_table, main = "Bar Plot of Gender", 
        xlab = "Gender", ylab = "Count", 
        col = "cyan", border = "white")

kable(data)
