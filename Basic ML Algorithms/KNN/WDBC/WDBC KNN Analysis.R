#install.packages("gmodels")
#install.packages("dplyr")
#install.packages("class")
library(class)
library(gmodels)
library(dplyr)

# Loading the CSV dataset under the name of wbcd
wbcd <- read.csv("wdbc.csv")
View(wbcd)

# Showing the structure and summary of the dataset
str(wbcd)
summary(wbcd)

# Removing column 1 to use only relevant data
wbcd <- select(wbcd, -1)

# Showing how many cases have benign masses or malignant
count(wbcd, diagnosis)

# Assigning more informative labels
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("Benign",
                                                                        "Malignant"))
# Using prop.table to show values of diagnosis variable
make_table <- table(wbcd$diagnosis)
make_table_prop <- prop.table(make_table) * 100
print(make_table_prop)

# Summary of radius_mean, area_mean, smoothness_mean features
summary(wbcd[c('radius_mean','area_mean','smoothness_mean')])

# Applying normalization to rescale features in the dataset
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
normalize(wbcd$texture_mean)
normalize(wbcd$symmetry_mean)
normalize(wbcd$perimeter_mean)

# Using lapply to normalize all vectors from 2 to 31
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

# Creating training and test datasets
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_train_z <- as.data.frame(scale(wbcd_n[1:469,]))
wbcd_test_z <- as.data.frame(scale(wbcd_n[470:569,]))
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# Training a model on the data
wbcd_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)
wbcd_pred_z <- knn(train = wbcd_train_z, test = wbcd_test_z, cl = wbcd_train_labels, k = 21)

# Evaluating model performance
CrossTable(x = wbcd_test_labels, y = wbcd_pred, prop.chisq=FALSE)
CrossTable(x = wbcd_test_labels, y = wbcd_pred_z, prop.chisq=FALSE)



