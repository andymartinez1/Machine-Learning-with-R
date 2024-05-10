# Importing required libraries.
library(stats)

# Loading the data set and inspecting 
insurance <- read.csv("insurance.csv")
str(insurance)
summary(insurance)
View(insurance)

summary(insurance$expenses)

hist(insurance$expenses)

table(insurance$region)

# Correlation matrix for four numeric variables 
cor(insurance[c("age", "bmi", "children", "expenses")])

# Training the model
insurance_train <- insurance[1:1070,]
insurance_test <- insurance[1071:1338,]

# Building the model
ins_model <- lm(expenses ~ ., data = insurance_train)

# Showing estimated beta coefficients
ins_model

# Making predictions
ins_pred <- predict(ins_model, insurance_test)

# Comparing the correlation
cor(ins_pred, insurance_test$expenses)

# Summary of the stored model 
summary(ins_model)
summary(ins_pred)

# Improving the model's performance by adding a new non-linear "age" term
insurance_train$age2 <- insurance_train$age^2

# Showing overweight indivuals with BMI of 30 or higher
insurance_train$bmi30 <- ifelse(insurance_train$bmi >= 30, 1, 0)

# New model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance_train)

summary(ins_model2)
