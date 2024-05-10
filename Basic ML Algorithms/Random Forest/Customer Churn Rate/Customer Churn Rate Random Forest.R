# Packages to install
#install.packages('dplyr')
#install.packages('randomForest')

# Load necessary libraries
library(randomForest)
library(dplyr)

# Load and preprocess dataset
df <- read.csv("customer_churn.csv")

# Remove customerID column as it is not relevant
df <- select(df, -customerID)

# Convert TotalCharges column to numeric
df$TotalCharges <- as.numeric(df$TotalCharges)

# Remove rows with missing values
df <- na.omit(df)

# Split data into training and testing sets
set.seed(1234)
train_idx <- sample(nrow(df), nrow(df)*0.7)
train <- df[train_idx,]
test <- df[-train_idx,]

# Fit random forest model
train$Churn <- as.factor(train$Churn)
train$Churn <- as.numeric(train$Churn) - 1
model <- randomForest(Churn ~ ., data = train)

# Make predictions on test data
predictions <- predict(model, test)

# Evaluate model performance
confusion_matrix <- table(predictions, test$Churn)

# View results
confusion_matrix
