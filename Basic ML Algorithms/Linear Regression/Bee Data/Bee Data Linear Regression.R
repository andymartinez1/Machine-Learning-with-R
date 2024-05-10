# Import required libraries
# install.packages('stats')
# install.packages('dplyr')
# install.packages('zoo')
library(stats)
library(dplyr)
library(zoo)

# Load the dataset.
bee_data <- read.csv("bee_data.csv")
#Removing unecessary columns 
bee_data <- bee_data %>% select(-c(other, unknown, other_pests_and_parasites, renovated_colonies, percent_renovated, added_colonies, max_colonies))

# View the structure and summary of the dataset.
str(bee_data)
summary(bee_data)

# Clean data creating new column to merge quarter and year data
bee_data$quarter_and_year <- as.yearqtr(paste0(bee_data$year, "-", bee_data$quarter))
as.factor(bee_data$quarter_and_year)
                             
# Groupby all colonies by new quarter and year 
yearly_bee_data = bee_data %>% group_by(quarter_and_year) %>%
  summarise(Current_Colonies = sum(num_colonies),
            .groups = 'drop')

# Groupby colonies by state 
state_bee_data = bee_data %>% group_by(state) %>%
  summarise(State_Colonies = sum(num_colonies),
            .groups = 'drop')

# Plot groupby functions
options(scipen=1000000)
par(ask=FALSE)

barplot(yearly_bee_data$Current_Colonies, names=yearly_bee_data$quarter_and_year,
        main='Colonies per Year and Quarter', xlab='', ylab='', ylim = c(0,8000000), las=2)
        mtext(text="Year and Quarter", side=1, line=4.5)
        mtext(text="Total Colonies", side=2, line=5)
        par(mar=c(8,6,4,2)+.1)

barplot(state_bee_data$State_Colonies, names=state_bee_data$state, 
        main='Colonies per State', xlab='', ylab='',las=2)
        mtext(text="States", side=1, line=6)
        mtext(text="Colonies", side=2, line=5)

# Summary, hist and levels dependent variable
summary(bee_data$num_colonies)
table(bee_data$num_colonies)

# Correlation between lost colonies and mites, diseases, and pesticides
cor(bee_data[c("lost_colonies", "varroa_mites", "diseases", "pesticides")])

# Assigning training and testing data
bee_data_train <- bee_data[1:1100,]
bee_data_test <- bee_data[1101:1391,]

# Creating linear regression model
pesticides_bee_model <- lm(lost_colonies ~ pesticides, data = bee_data_train)
varroa_mites_bee_model <- lm(lost_colonies ~ varroa_mites, data = bee_data_train)
diseases_bee_model <- lm(lost_colonies ~ diseases, data = bee_data_train)
pesticides_bee_model
varroa_mites_bee_model
diseases_bee_model
summary(pesticides_bee_model)
summary(varroa_mites_bee_model)
summary(diseases_bee_model)

# Making a prediction
pesticides_bee_pred <- predict(pesticides_bee_model, bee_data_test)
varroa_mites_bee_pred <- predict(varroa_mites_bee_model, bee_data_test)
diseases_bee_pred <- predict(diseases_bee_model, bee_data_test)
summary(pesticides_bee_pred)
summary(varroa_mites_bee_pred)
summary(diseases_bee_pred)

# Plotting predicted values vs actual values
plot(pesticides_bee_pred, bee_data_test$lost_colonies, main='Pesticides Prediction', xlab = "Pesticides", ylab = "Lost Bee Colonies")
abline(a=0,b=1,col="red",lwd=2)
plot(varroa_mites_bee_pred, bee_data_test$lost_colonies, main='Varroa Mites Prediction', xlab = "Varroa Mites", ylab = "Lost Bee Colonies")
abline(a=0,b=1,col="red",lwd=2)
plot(diseases_bee_pred, bee_data_test$lost_colonies, main='Diseases Prediction', xlab = "Diseases", ylab = "Lost Bee Colonies")
abline(a=0,b=1,col="red",lwd=2)

# Improving the model accuracy
new_pesticides_bee_model <- lm(lost_colonies ~ sqrt(pesticides), data = bee_data_train)
new_varroa_mites_bee_model <- lm(lost_colonies ~ sqrt(varroa_mites), data = bee_data_train)
new_diseases_bee_model <- lm(lost_colonies ~ sqrt(diseases), data = bee_data_train)
new_pesticides_bee_model
new_varroa_mites_bee_model
new_diseases_bee_model
summary(new_pesticides_bee_model)
summary(new_varroa_mites_bee_model)
summary(new_diseases_bee_model)

# Making a new prediction
new_pesticides_bee_pred <- predict(new_pesticides_bee_model, bee_data_test)
new_varroa_mites_bee_pred <- predict(new_varroa_mites_bee_model, bee_data_test)
new_diseases_bee_pred <- predict(new_diseases_bee_model, bee_data_test)
summary(new_pesticides_bee_pred)
summary(new_varroa_mites_bee_pred)
summary(new_diseases_bee_pred)

# Plotting new predicted values vs actual values
plot(new_pesticides_bee_pred, bee_data_test$lost_colonies, main='New Pesticides Prediction', xlab = "Pesticides", ylab = "Lost Bee Colonies")
abline(a=0,b=1,col="red",lwd=2)
plot(new_varroa_mites_bee_pred, bee_data_test$lost_colonies, main='New Varroa Mites Prediction', xlab = "Varroa Mites", ylab = "Lost Bee Colonies")
abline(a=0,b=1,col="red",lwd=2)
plot(new_diseases_bee_pred, bee_data_test$lost_colonies, main='New Diseases Prediction', xlab = "Diseases
     ", ylab = "Lost Bee Colonies")
abline(a=0,b=1,col="red",lwd=2)