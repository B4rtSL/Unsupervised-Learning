# setwd
library(tidyverse)

data = read.csv('mxmh_survey_results.csv')
str(data)

# chooosing columns outside area of interest

columnsToDismiss = c(1, 3, 11, 33)

data = data[, -columnsToDismiss]

summary(data)

# remove rows with NA's (only one row without age)

data = data[complete.cases(data),]

# columns with continuous data need introduction of categorical bins

data$Age = ifelse(data$Age<18, "Age below 18", 
                  ifelse(data$Age<=24, "Age 18-24",
                         ifelse(data$Age<=34, "Age 25-34",
                                ifelse(data$Age<=44, "Age 35-44",
                                       ifelse(data$Age<=54, "Age 45-54",
                                              ifelse(data$Age<=64, "Age 55-64",
                                                     ifelse(data$Age>=65, "Age 65 or above", NA)))))))
summary(as.factor(data$Age))

data$Hours.per.day = ifelse(data$Hours.per.day <= 2, '2 hours or less',
                            ifelse(data$Hours.per.day <= 4, 'More than 2 hours, less than 4',
                                   ifelse(data$Hours.per.day <= 6, 'More than 4 hours, less than 6',
                                          ifelse(data$Hours.per.day <= 24, '6 hours +', NA))))

summary(as.factor(data$Hours.per.day))

data$Anxiety = ifelse(data$Anxiety < 4, "Not Anxious", ifelse(data$Anxiety < 7, "Fairly Anxious", ifelse(data$Anxiety < 11, "Anxious", NA)))
summary(as.factor(data$Anxiety))

data$Depression = ifelse(data$Depression < 4, "Not Depressed", ifelse(data$Depression < 7, "Fairly Depressed", ifelse(data$Depression < 11, "Depressed", NA)))
summary(as.factor(data$Depression))

data$Insomnia = ifelse(data$Insomnia < 4, "Not Insomniac", ifelse(data$Insomnia < 7, "Fairly Insomniac", ifelse(data$Insomnia < 11, "Insomniac", NA)))
summary(as.factor(data$Insomnia))

data$OCD = ifelse(data$OCD < 4, "Not obsessive-compulsive", ifelse(data$OCD < 7, "Fairly obsessive-compulsive", ifelse(data$OCD < 11, "Obsessive-compulsive", NA)))
summary(as.factor(data$OCD))

# columns do not need further actions despite converting to factors
str(data)
data = data %>% mutate(across(all_of(colnames(data)), factor))

dataHabitsHealth = data[, c(1:8, 25:28)]

### ASSOCIATION RULES ###
write.csv(dataHabitsHealth, file = 'cleanData.csv')

library(arules)

transactions = read.transactions('cleanData.csv', format='basket', sep=',', skip=0)
inspect(transactions)
size(transactions) 
length(transactions)

rulesDepressed<-apriori(data=transactions, parameter=list(supp=0.01, conf=0.1), appearance=list(default="lhs", rhs="Depressed"), control=list(verbose=F)) 

rulesDebressedSorted<-sort(rulesDepressed, by="confidence", decreasing=TRUE)
inspect(head(rulesDebressedSorted))
