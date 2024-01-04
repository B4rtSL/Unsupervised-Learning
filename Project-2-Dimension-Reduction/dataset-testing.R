library(corrplot)
library('psych')

cereal = read.csv('cereal.csv')
head(cereal)
cereal_num = cereal[,c(4:12)]

cereal_num$calories = as.numeric(cereal_num$calories)
cereal_num$protein = as.numeric(cereal_num$protein)
cereal_num$fat = as.numeric(cereal_num$fat)
cereal_num$sodium = as.numeric(cereal_num$sodium)
cereal_num$sugars = as.numeric(cereal_num$sugars)
cereal_num$potass = as.numeric(cereal_num$potass)
cereal_num$vitamins = as.numeric(cereal_num$vitamins)

str(cereal_num)
cereal_num_st = scale(cereal_num)
str(cereal_num_st)

cor_cereal = cor(cereal_num_st)
corrplot(cor_cereal, method = 'number', type = "lower", order = "hclust", tl.col = "black", tl.cex = 0.7, number.cex = 0.9)

cortest.bartlett(cor_cereal)
KMO(cor_cereal)

### data set about cereal is not good enough for factor analysis

life = read.csv('Life-Expectancy-Data.csv')
str(life)
head(life)

life_num = life[, 4:22]
life_num = life_num[complete.cases(life_num),]

life_scaled = scale(life_num)

cor_life = cor(life_scaled)
corrplot(cor_life, method = 'number', type = "lower", order = "hclust", tl.col = "black", tl.cex = 0.7, number.cex = 0.9)

cortest.bartlett(cor_life)
KMO(cor_life)

### filling NA's with means

life_num[!complete.cases(life_num),]
columns_with_na <- colnames(life_num)[colSums(is.na(life_num)) > 0]

summary(life_num)

library(zoo)
life_num_filled = na.aggregate(life_num, FUN = mean)
life_num_filled[!complete.cases(life_num_filled),]

life_num[is.na(life_num$Life.expectancy),"Life.expectancy"] <- mean(life_num$Life.expectancy, na.rm=T)

life_num$Life.expectancy == life_num_filled$Life.expectancy

### checking factorability after filling missing values

life_num = life[, 4:22]
life_num = life_num[complete.cases(life_num),]

life_scaled = scale(life_num_filled)

cor_life = cor(life_scaled)
corrplot(cor_life, method = 'number', type = "lower", order = "hclust", tl.col = "black", tl.cex = 0.7, number.cex = 0.9)

cortest.bartlett(cor_life)
KMO(cor_life)
