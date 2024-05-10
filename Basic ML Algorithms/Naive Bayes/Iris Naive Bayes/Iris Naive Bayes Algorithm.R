# Import required libraries.
# install.packages("e1071")
# install.packages("gmodels")
# install.packages("dplyr")
library(e1071)
library(gmodels)
library(dplyr)

# Load the data set.
data("iris")
head(iris)

# Check the structure of the dataset
str(iris)

# Checking the summary.
summary(iris)

# Train - Test Split.
index = sample(2,nrow(iris),prob = c(0.8,0.2),replace=TRUE) 
set.seed(999)
train = iris[index==1,]
test = iris[index==2,]

# Separate the test labels from the test data.
test_data = test[1:4]
test_label = test[,5]

# Train the model.
training_model = naiveBayes(train$Species~.,train)

# Make predictions.
test_result = predict(training_model,test_data)
test_result

# Compare the predicted and actual values.
CrossTable(x = test_label, y = test_result)