## Title: Regression Extentions Assignment 
## Purpose: 
## Author: Annabel Mehta
## Date 1/29/2026

# Setting working directory 
getwd()
setwd("/Users/annabelmehta/Desktop/Advanced Business Analytics/Week 1 & 2")

# Loading Libraries 
install.packages("readxl")
library(readxl)

# Loading wages dataset
wages <- read_excel('wages.xlsx')
View(wages)

# Age vs Wage Plot 
ggplot(wages, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Age vs Wage",
    x = "Age",
    y = "Wage")
# Quadriatic would be the best model to capture the relationship. 

# Multiple regression model 
m1 <- lm(Wage ~ Age + Educ, data = wages)
summary(m1)

# Quadratic multiple regression model
wages$Age2 <- wages$Age^2
m2 <- lm(Wage ~ Age + Age2 + Educ, data = wages)
summary(m2)

# Predicting Wages 
new_data <- data.frame(
  Age = c(30, 50, 70), 
  Educ = c(16, 16, 16), 
  )
new_data$Age2 <- new_data$Age^2

predicted_wages <- predict(m2, newdata = new_data)
print(predicted_wages)

# AnnArbor Dataset 
AnnArbor <- read_excel('AnnArbor.xlsx')
View(AnnArbor)

#Plotting Rent Against predictor variables 
ggplot(AnnArbor, aes(x = Beds, y = Rent)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Beds vs Rent",
    x = "Beds",
    y = "Rent")

ggplot(AnnArbor, aes(x = Baths, y = Rent)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Baths vs Rent",
    x = "Baths",
    y = "Rent")

ggplot(AnnArbor, aes(x = Sqft, y = Rent)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Squarefootage vs Rent",
    x = "Sqft",
    y = "Rent")

# Log Transforation to Squarefootage 
AnnArbor$Sqft_log <- log(AnnArbor$Sqft) 

# Creating a model
m3 <- lm(Rent ~ Beds + Baths + Sqft_log, data = AnnArbor)

# Predicting rent 
house <- data.frame(
  Beds = 3, 
  Baths = 2, 
  Sqft = 1600)

house$Sqft_log <- log(house$Sqft)

predicted_rent <- predict(m3, newdata = house)
print(predicted_rent)
