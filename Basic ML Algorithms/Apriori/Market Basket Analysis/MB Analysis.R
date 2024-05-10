# Install and load packages needed
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("tidyverse")
#install.packages("plyr")
#install.packages("readxml")
#install.packages("RColorBrewer")

library(arules)
library(arulesViz)
library(tidyverse)
library(plyr)
library(dplyr)
library(readxl)
library(RColorBrewer)

# Reading excel file and removing rows with missing values
retail <- read_excel("Online Retail.xlsx")
retail <- retail[complete.cases(retail), ]

# Creating columns in dataframe
retail %>% mutate(Description = as.factor(Description))   
retail %>% mutate(Country = as.factor(Country))

# Converting character data to data format
retail$Date <- as.Date(retail$InvoiceDate)
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")

# Converting invoice number into a numeric value
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

# Binding the new columns into the dataframe
cbind(retail,TransTime)
cbind(retail,InvoiceNo)

glimpse(retail)

# Retrieving the description of what was purchased in each invoice
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))

transactionData

# Now that we got the transaction description, we no longer need data and invoice number
transactionData$InvoiceNo <- NULL
transactionData$Date <- NULL

# Rename the description column to items
colnames(transactionData) <- c("items")
transactionData

# Writing the transaction data into a csv file
write.csv(transactionData,"Market Basket Transactions.csv", quote = FALSE, row.names = FALSE)

# Converting the csv file into an object of the transaction class
tr <- read.transactions('Market Basket Transactions.csv', format = 'basket', sep=',')
tr
# Density is equal to 22191 x 7876 x 0.001930725 = 337445
summary(tr)

# Creating an item frequency plot of the top 20 most frequent items
# Absolute will plot numeric frequencies of each item independently.
# Relative will plot how many times these items have appeared as compared to others
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))

# Viewing only the top 10 rules
inspect(association.rules[1:10])

# Removing redundant rules
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)  #> 3913
subset.association.rules. <- association.rules[-subset.rules] # remove subset rules.

# EXAMPLE: Customers who bought METAL also bought.... 
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="METAL",default="rhs"))
inspect(head(metal.association.rules))

# Filtering rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
# Plot SubRules
plot(subRules)