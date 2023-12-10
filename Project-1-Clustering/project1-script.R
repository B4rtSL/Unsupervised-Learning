Sys.setlocale("LC_ALL","English")
Sys.setenv(LANGUAGE='en')

library(ggplot2)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(cluster)
library(ClusterR)
library(hopkins)
library(NbClust)
library(tidyverse)

setwd('D:/Git-Management/Unsupervised-Learning/Project-1-Clustering')


data <- read.csv('nba_2022-23_all_stats_with_salary.csv')
str(data)


data$Position = as.factor(data$Position)
summary(data$Position)

# There are 7 players that are assigned with a mix of o classically considered basketball court positions. 
# In order to achieve coherent position labeling, I have decided to manually assign positions (PG, SG, SF, PF, C) 
# to those players taking into consideration either their historical roles or their role in lineups for the season.

data[data$Position == 'PG-SG' | data$Position == 'SF-PF' | data$Position == 'SF-SG' | data$Position == 'SG-PG',]

# 'TRB', 'PTS', 'AST', 'STL', 'BLK', 
shorter_stats <- data[c('Player.Name', 'Position', 'MP', 'PTS', 'AST', 'STL', 'BLK', 'TRB', 'eFG.', 'TS.', 'USG.', 'WS.48','VORP')]
length(shorter_stats[,1])
shorter_stats[!complete.cases(shorter_stats),]

shorter_stats = shorter_stats[complete.cases(shorter_stats),]
length(shorter_stats[,1])

stats = shorter_stats[, c('MP', 'PTS', 'AST', 'STL', 'BLK', 'TRB', 'eFG.', 'TS.', 'USG.', 'WS.48','VORP')]
shorter_stats[437,]

scaled_stats <- as.data.frame(lapply(stats, scale))
shorter_stats[248,]
get_clust_tendency(scaled_stats, 2, graph=T)
?get_clust_tendency

?kmeans

fviz_nbclust(scaled_stats, FUNcluster = kmeans, method = 'silhouette') + theme_classic()
?eclust
stats_clust <- eclust(scaled_stats, FUNcluster = 'kmeans', k=2, graph=F)
fviz_cluster(stats_clust, data = scaled_stats, elipse.type = 'convex')
fviz_silhouette(stats_clust)

data
