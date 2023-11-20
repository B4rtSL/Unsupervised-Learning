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

data <- read.csv('nba_2022-23_all_stats_with_salary.csv')
head(data)
str(data)

stats <- data[c(7:52)]
scaled_stats <- scale(stats)
get_clust_tendency(scaled_stats, 2, graph=T)
hopkins(scaled_stats)
