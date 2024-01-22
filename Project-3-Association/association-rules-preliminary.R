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

data$Age <- factor(ifelse(data$Age < 18, "Age < 18", 
                          ifelse(data$Age <= 24, "Age 18-24",
                                 ifelse(data$Age <= 34, "Age 25-34",
                                        ifelse(data$Age <= 44, "Age 35-44",
                                               ifelse(data$Age <= 54, "Age 45-54",
                                                      ifelse(data$Age <= 64, "Age 55-64",
                                                             ifelse(data$Age >= 65, "Age 65+", NA))))))),
                   levels = c("Age < 18", "Age 18-24", "Age 25-34", "Age 35-44", "Age 45-54", "Age 55-64", "Age 65+"))

data$Hours.per.day <- factor(ifelse(data$Hours.per.day <= 2, 'hours <= 2',
                                    ifelse(data$Hours.per.day <= 4, '2 < hours <= 4',
                                           ifelse(data$Hours.per.day <= 6, '4 < hours <= 6',
                                                  ifelse(data$Hours.per.day <= 24, '6+ hours', NA)))),
                             levels = c('hours <= 2', '2 < hours <= 4', '4 < hours <= 6', '6+ hours'))

data$Anxiety <- factor(ifelse(data$Anxiety < 4, "Not Anxious",
                              ifelse(data$Anxiety < 7, "Fairly Anxious",
                                     ifelse(data$Anxiety < 11, "Anxious", NA))),
                       levels = c("Not Anxious", "Fairly Anxious", "Anxious"))

data$Depression <- factor(ifelse(data$Depression < 4, "Not Depressed",
                                 ifelse(data$Depression < 7, "Fairly Depressed",
                                        ifelse(data$Depression < 11, "Depressed", NA))),
                          levels = c("Not Depressed", "Fairly Depressed", "Depressed"))

data$Insomnia <- factor(ifelse(data$Insomnia < 4, "Not Insomniac",
                               ifelse(data$Insomnia < 7, "Fairly Insomniac",
                                      ifelse(data$Insomnia < 11, "Insomniac", NA))),
                        levels = c("Not Insomniac", "Fairly Insomniac", "Insomniac"))

data$OCD <- factor(ifelse(data$OCD < 4, "Not O-C",
                          ifelse(data$OCD < 7, "Fairly O-C",
                                 ifelse(data$OCD < 11, "O-C", NA))),
                   levels = c("Not O-C", "Fairly O-C", "O-C"))

### YES/NO Questions modification.

data$While.working = ifelse(data$While.working == 'Yes', "Listening while working", ifelse(data$While.working == 'No', "Not listening while working", NA))

data$Instrumentalist = ifelse(data$Instrumentalist == 'Yes', "Instrumentalist", ifelse(data$Instrumentalist == 'No', "Non-instrumentalist", NA))

data$Composer = ifelse(data$Composer == 'Yes', "Composer", ifelse(data$Composer == 'No', "Non-composer", NA))

data$Exploratory = ifelse(data$Exploratory == 'Yes', "Exploring", ifelse(data$Exploratory == 'No', "Non-exploring", NA))

data$Foreign.languages = ifelse(data$Foreign.languages == 'Yes', "Polyglot", ifelse(data$Foreign.languages == 'No', "Non-polyglot", NA))

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

rulesDepressed<-apriori(data=transactions, parameter=list(supp=0.1, conf=0.1), appearance=list(default="lhs", rhs="Depressed"), control=list(verbose=F)) 

rulesDebressedSorted<-sort(rulesDepressed, by="confidence", decreasing=TRUE)
inspect(head(rulesDebressedSorted))
