# Import packages
install.packages("readxl")
install.packages("GGally")
install.packages("dplyr")
library(readxl)
library(GGally)
library(dplyr)

# Import traveler data
TravelerData <- read_excel('/Users/matthewrickard/Desktop/Coding/Airbnb Case Data.xlsx', sheet = 'Traveler Data')
TravelerData

# Import host data
HostData <- read_excel('/Users/matthewrickard/Desktop/Coding/MKTG 352/Assignment 3/Airbnb Case Data.xlsx', sheet = 'Host Data')
HostData

# Step 1: Exploring the data

# Summary statistics for travelers data
mean(TravelerData$Age)
min(TravelerData$Age)
max(TravelerData$Age)

TravelerData %>% count(Email_25)
TravelerData %>% count(AlaskaFF)
TravelerData %>% count(Tickets)

# Summary statistics for host data
mean(HostData$Length)
min(HostData$Length)
max(HostData$Length)

mean(HostData$ZestimateK)
min(HostData$ZestimateK)
max(HostData$ZestimateK)

HostData %>% count(Type_Hm)
HostData %>% count(Loc_1)
HostData %>% count(Loc_2)

# Step 2: Running Logistic Regression Models

# Traveler logistic regression model 

# Convert dependent variable (Choice) to factor
TravelerData$Choice <- factor(TravelerData$Choice, levels = c(0, 1), labels = c("No", "Yes"))

# Email Promotion Type (Reference: Welcome Email)
TravelerData$Email_25 <- factor(TravelerData$Email_25, levels = c(0, 1))  # 1 = $25 off, 0 = otherwise
TravelerData$Email_Taxi <- factor(TravelerData$Email_Taxi, levels = c(0, 1))  # 1 = Free taxi, 0 = otherwise

# Email Provider (Reference: Other)
TravelerData$Gmail <- factor(TravelerData$Gmail, levels = c(0, 1))
TravelerData$yahoo <- factor(TravelerData$yahoo, levels = c(0, 1))
TravelerData$Edu <- factor(TravelerData$Edu, levels = c(0, 1))

# Alaska Frequent Flier Status (Reference: Not a member)
TravelerData$AlaskaFF <- factor(TravelerData$AlaskaFF, 
                                levels = c(1, 2, 3), 
                                labels = c("NotMember", "FrequentFlier", "MVP"))
TravelerData$AlaskaFF <- relevel(TravelerData$AlaskaFF, ref = "NotMember")

# Address (Reference: Out of state)
TravelerData$Add_Ore <- factor(TravelerData$Add_Ore, levels = c(0, 1))  # 1 = Oregon (not Eugene or Springfield)
TravelerData$Add_Eug <- factor(TravelerData$Add_Eug, levels = c(0, 1))  # 1 = Eugene or Springfield

# Type of Ticket (Reference: One way)
TravelerData$RoundTrip <- factor(TravelerData$RoundTrip, levels = c(0, 1), labels = c("OneWay", "RoundTrip"))
TravelerData$RoundTrip <- relevel(TravelerData$RoundTrip, ref = "OneWay")

# Fit logistic regression model
model_traveler <- glm(Choice ~ Email_25 + Email_Taxi + Gmail + yahoo + Edu + 
                        AlaskaFF + Add_Ore + Add_Eug + Age + Tickets + RoundTrip, 
                      data = TravelerData, family = binomial)

# View model summary
summary(model_traveler)

# Host regression model

# Convert dependent variable (Choice) to factor
HostData$Choice <- factor(HostData$Choice, levels = c(0, 1), labels = c("No", "Yes"))

# Listing Type (Reference: Shared room or private room)
HostData$Type_Hm <- factor(HostData$Type_Hm, levels = c(0, 1), labels = c("SharedOrPrivate", "EntireHome"))
HostData$Type_Hm <- relevel(HostData$Type_Hm, ref = "SharedOrPrivate")

# Location (Reference: Other outskirt locations)
HostData$Loc_1 <- factor(HostData$Loc_1, levels = c(0, 1))  # 1 = Campus to downtown to Whitaker
HostData$Loc_2 <- factor(HostData$Loc_2, levels = c(0, 1))  # 1 = Fairmont to Amazon to South Eugene Hills
HostData$Loc_3 <- factor(HostData$Loc_3, levels = c(0, 1))  # 1 = Friendly to West Eugene
HostData$Loc_4 <- factor(HostData$Loc_4, levels = c(0, 1))  # 1 = North Eugene
HostData$Loc_5 <- factor(HostData$Loc_5, levels = c(0, 1))  # 1 = Springfield

# Fit logistic regression model
model_host <- glm(Choice ~ Length + Type_Hm + Guests + ZestimateK + ListPrice + Rating + Rev_AbB + 
                    Loc_1 + Loc_2 + Loc_3 + Loc_4 + Loc_5, 
                  data = HostData, family = binomial)

# View model summary
summary(model_host)

# Step 3: Model Evaluation and Prediction Analysis

# Save Predicted Probabilities
TravelerData$prob_1 <- model_traveler$fitted.values
HostData$prob_1 <- model_host$fitted.values

# Create Prediction variable for TravelerData
TravelerData$Prediction <- ifelse(TravelerData$prob_1 > 0.5, 1, 0)

# Convert Prediction to factor for correct comparison with Choice
TravelerData$Prediction <- factor(TravelerData$Prediction, levels = c(0, 1), labels = c("No", "Yes"))

# Create Prediction variable for HostData
HostData$Prediction <- ifelse(HostData$prob_1 > 0.5, 1, 0)

# Convert Prediction to factor for correct comparison with Choice
HostData$Prediction <- factor(HostData$Prediction, levels = c(0, 1), labels = c("No", "Yes"))

# Confusion matrix for TravelerData
confusion_matrix_traveler <- table(Prediction = TravelerData$Prediction, Actual = TravelerData$Choice)
print(confusion_matrix_traveler)

# Confusion matrix for HostData
confusion_matrix_host <- table(Prediction = HostData$Prediction, Actual = HostData$Choice)
print(confusion_matrix_host)

# Accuracy for TravelerData Model
correct_predictions_traveler <- sum(TravelerData$Prediction == TravelerData$Choice)
accuracy_traveler <- correct_predictions_traveler / nrow(TravelerData)

# Accuracy for HostData Model
correct_predictions_host <- sum(HostData$Prediction == HostData$Choice)
accuracy_host <- correct_predictions_host / nrow(HostData)

# Output the accuracy
accuracy_traveler
accuracy_host

# Plot the distribution of predicted probabilities for TravelerData
hist(TravelerData$prob_1, main = "Distribution of Predicted Probabilities for TravelerData", 
     xlab = "Predicted Probability", col = "lightblue", border = "black")
summary(TravelerData$prob_1)

# Plot the distribution of predicted probabilities for HostData
hist(HostData$prob_1, main = "Distribution of Predicted Probabilities for HostData", 
     xlab = "Predicted Probability", col = "lightblue", border = "black")
summary(HostData$prob_1)

ggpairs(HostData[, c("Choice", "Length", "Type_Hm", "Guests", "ZestimateK", "ListPrice", "Rating")], 
        aes(color = as.factor(Choice)), 
        title = "Pair Plots for Host Data")

# Elasticity analysis

# Calculate the mean of continuous variables
mean_length <- mean(HostData$Length)
mean_zestimate <- mean(HostData$ZestimateK)
mean_listprice <- mean(HostData$ListPrice)
mean_rating <- mean(HostData$Rating)
mean_revabb <- mean(HostData$Rev_AbB)

# Calculate the predicted probability (Y) for Choice variable
mean_prob <- mean(HostData$prob_1)

# Coefficients from the logistic regression model
coefficients_host <- summary(model_host)$coefficients
beta_length <- coefficients_host["Length", "Estimate"]
beta_zestimate <- coefficients_host["ZestimateK", "Estimate"]
beta_listprice <- coefficients_host["ListPrice", "Estimate"]
beta_rating <- coefficients_host["Rating", "Estimate"]
beta_revabb <- coefficients_host["Rev_AbB", "Estimate"]

# Calculate elasticity for each continuous variable
elasticity_length <- beta_length * (mean_length / mean_prob)
elasticity_zestimate <- beta_zestimate * (mean_zestimate / mean_prob)
elasticity_listprice <- beta_listprice * (mean_listprice / mean_prob)
elasticity_rating <- beta_rating * (mean_rating / mean_prob)
elasticity_revabb <- beta_revabb * (mean_revabb / mean_prob)

# Create the elasticity table
elasticity_table <- data.frame(
  Variable = c("Length", "ZestimateK", "ListPrice", "Rating", "Rev_AbB"),
  Elasticity = c(elasticity_length, elasticity_zestimate, elasticity_listprice, elasticity_rating, elasticity_revabb)
)

# Print elasticity table
print(elasticity_table)

