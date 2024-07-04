# Loading the Required Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)

# Loading the Dataset
titanic_data <- read.csv(file.choose(),header = T)

#Basic Function on the dataset
head(titanic_data)
summary(titanic_data)
str(titanic_data)

# Data Cleaning
# Checking for missing values
colSums(is.na(titanic_data))

# Filling missing age values with median age values
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)

# Filling missing Embarked values with the common embarkation port value
titanic_data$Embarked[is.na(titanic_data$Embarked)] <- "S"

# Convert necessary columns to factors
titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)

# Exploratory Data Analysis (EDA)
# Distribution of Survived
ggplot(titanic_data, aes(x = Survived)) +
  geom_bar() +
  labs(title = "Distribution of Survival", x = "Survived", y = "Count")

# Survival Rate by Gender
ggplot(titanic_data, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival Rate by Gender", x = "Gender", y = "Count")

# Survival Rate by Passenger Class
ggplot(titanic_data, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival Rate by Passenger Class", x = "Passenger Class", y = "Count")

# Age Distribution by Survival
ggplot(titanic_data, aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Age Distribution by Survival", x = "Age", y = "Count")

# Survival Rate by Embarkation Port
ggplot(titanic_data, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival Rate by Embarkation Port", x = "Embarkation Port", y = "Count")

# Exploring Relationships Between Variables
# Passenger Class and Age
ggplot(titanic_data, aes(x = Pclass, y = Age)) +
  geom_boxplot() +
  labs(title = "Passenger Class vs Age", x = "Passenger Class", y = "Age")

# Fare Distribution by Passenger Class
ggplot(titanic_data, aes(x = Pclass, y = Fare)) +
  geom_boxplot() +
  labs(title = "Fare Distribution by Passenger Class", x = "Passenger Class", y = "Fare")

# Survival Rate by Family Size
titanic_data$FamilySize <- titanic_data$SibSp + titanic_data$Parch + 1
ggplot(titanic_data, aes(x = FamilySize, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival Rate by Family Size", x = "Family Size", y = "Count")

# Identify Patterns and Trends
# Example summary
summary_table <- titanic_data %>%
  group_by(Survived) %>%
  summarise(count = n(), mean_age = mean(Age), mean_fare = mean(Fare))
print(summary_table)