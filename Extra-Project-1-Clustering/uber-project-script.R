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
library(leaflet)

# upload of the data set
uber = read.csv('uber-data.csv')

# manually checking data structure
str(uber)

# checking for any missing values, if variable == FALSE => data set is ok to work with
print(anyNA(uber))

# define boundaries for map plotting based on extreme coordinates (left, bottom, right, top)
bbox <- list(
  left = min(uber$Lon),
  bottom = min(uber$Lat),
  right = max(uber$Lon),
  top = max(uber$Lat)
)

# address coordinates from data set, sample to check further details, sample size - as large as possible not to choke on computing (RAM issues)
coordinates = data.frame(longitude = uber$Lon, latitude = uber$Lat)
set.seed(123)
samples = sample(nrow(coordinates), 30000)
coordinates_short = as.data.frame(coordinates[samples,])

# check clusterability
get_clust_tendency(coordinates_short, 2, graph=FALSE)
hopkins(coordinates_short)

# optimal number of clusters
fviz_nbclust(coordinates_short, FUNcluster = cluster::clara, method = "silhouette") + 
  theme_classic() + 
  ggtitle("Optimal numbers of clusters")

# perform clustering with CLARA (large data set), 4 clusters to better explain groups related to location 

clara_clust4 = clara(coordinates, 4, metric="euclidean", stand=FALSE, samples=10,
                   sampsize=60, trace=0, medoids.x=TRUE,
                   rngR=FALSE, pamLike=FALSE, correct.d=TRUE)

#info about avgerage silhouette width
clara4sil = clara_clust4$silinfo$avg.width

fviz_cluster(clara_clust4, ellipse.type="t", palette=c('#003f5c','#7a5195','#ef5675','#ffa600'),
             geom="point", pointsize=1, ggtheme=theme_classic(), stand=FALSE, axes=c('Longitude', 'Latitude'))

fviz_silhouette(clara_clust4, palette=c('#003f5c','#7a5195','#ef5675','#ffa600'))

# perform clustering with CLARA (large data set), 5 clusters to better explain groups related to location 

clara_clust5 = clara(coordinates, 5, metric="euclidean", stand=FALSE, samples=10,
                   sampsize=58, trace=0, medoids.x=TRUE,
                   rngR=FALSE, pamLike=FALSE, correct.d=TRUE)

#info about avgerage silhouette width
clara5sil = clara_clust5$silinfo

fviz_cluster(clara_clust5, ellipse.type="t", palette=c('#003f5c','#58508d','#bc5090','#ff6361','#ffa600'),
             geom="point", pointsize=1, ggtheme=theme_classic(), stand=FALSE, axes=c('Longitude', 'Latitude'))

fviz_silhouette(clara_clust5, palette=c('#003f5c','#58508d','#bc5090','#ff6361','#ffa600'))

# add labels based on cluster ID and combine into new data frame
uber_labels4 = clara_clust4$clustering
uberClustered4 = data.frame(latitude = uber$Lat, longitude = uber$Lon, uber_labels4)

# sample clustered data set to make it possible to be displayed on leaflet map (when size is too big, map crashes)
set.seed(123)
mapsamples = sample(nrow(uberClustered4), 100000)
coordinates_map = as.data.frame(uberClustered4[mapsamples,])

pal = colorFactor(c('#003f5c','#7a5195','#ef5675','#ffa600'), domain=c(1,2,3,4))

my_map <- leaflet(uberClustered4) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude,
                   color = ~pal(uber_labels4),
                   radius = 2,
                   stroke = FALSE, 
                   fillOpacity = 0.6) %>%
  fitBounds(bbox$left, bbox$bottom, bbox$right, bbox$top)
my_map %>% addProviderTiles(providers$CartoDB.Positron)

# add labels based on cluster ID and combine into new data frame
uber_labels5 = clara_clust5$clustering
uberClustered5 = data.frame(latitude = uber$Lat, longitude = uber$Lon, uber_labels5)

# sample clustered data set to make it possible to be displayed on leaflet map (when size is too big, map crashes)
set.seed(123)
mapsamples = sample(nrow(uberClustered5), 100000)
coordinates_map = as.data.frame(uberClustered5[mapsamples,])

pal = colorFactor(c('#003f5c','#58508d','#bc5090','#ff6361','#ffa600'), domain=c(1,2,3,4,5))

my_map <- leaflet(uberClustered5) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude,
                   color = ~pal(uber_labels5),
                   radius = 2,
                   stroke = FALSE, 
                   fillOpacity = 0.6) %>%
  fitBounds(bbox$left, bbox$bottom, bbox$right, bbox$top)
my_map %>% addProviderTiles(providers$CartoDB.Positron)

# reference for medoids of whole data set clustering
global_medoids = clara_clust5$medoids

uber$Date.Time <- as.POSIXct(uber$Date.Time, format="%m/%d/%Y %H:%M:%S")

# extracting day of the month
uber$Day <- as.numeric(format(uber$Date.Time, "%d"))

# creating subsets for each day
day_subsets <- lapply(unique(uber$Day), function(day) {
  uber[uber$Day == day, ]
})

uber$DayOfWeek <- weekdays(uber$Date.Time)

# extracting hour of the day
uber$Hour <- as.numeric(format(uber$Date.Time, "%H"))
# creating subsets for each hour
hour_subsets <- lapply(unique(uber$Hour), function(hour) {
  uber[uber$Hour == hour, ]
})
head(uber)
# empty lists to store results
avg_sils_list_day <- list()
medoids_list_day <- list()

# looping over days
for (day_to_select in 1:30) {
  
  # filter data for the current day
  day_data <- uber[uber$Day == day_to_select, ]
  coordinates_day <- data.frame(latitude = day_data$Lat, longitude = day_data$Lon)
  
  # clustering using CLARA
  clara_clust_day <- clara(coordinates_day, 5, metric="euclidean", stand=FALSE, samples=10,
                           sampsize=58, trace=0, medoids.x=TRUE,
                           rngR=FALSE, pamLike=FALSE, correct.d=TRUE)
  
  # storing results in lists and adding for day index values from iterations
  avg_sils_list_day[[day_to_select]] <- clara_clust_day$silinfo$avg.width
  medoids_list_day[[day_to_select]] <- clara_clust_day$medoids
}

# combining the results
all_avg_sils_day <- unlist(avg_sils_list_day)
all_medoids_day <- do.call(rbind, medoids_list_day)

medoid_labels = rep(c(1:5), 30)

df_medoids_day = data.frame(latitude = all_medoids_day[,1], longitude = all_medoids_day[,2], medoid_labels = as.character(medoid_labels))

medoid_colors = (c('1' = '#083b5c', '2' = '#604b88','3'= '#be4b88','4' = '#fd635c','5' = '#ffa600'))

ggplot(df_medoids_day, aes(x = longitude, y = latitude, color = medoid_labels)) +
  geom_point() +
  scale_color_manual(values = medoid_colors) +
  ggtitle("Cluster centers based on date") +
  xlab("Longitude") +
  ylab("Latitude")

# Gathering medoids of each cluster for each day
medoids_C1 = all_medoids_day[seq(1,150,5),]
dist_med_c1 = as.data.frame(distEuclidean(medoids_C1, t(as.matrix(global_medoids[1,]))))
medoids_C2 = all_medoids_day[seq(2,150,5),]
dist_med_c2 = as.data.frame(distEuclidean(medoids_C2, t(as.matrix(global_medoids[1,]))))
medoids_C3 = all_medoids_day[seq(3,150,5),]
dist_med_c3 = as.data.frame(distEuclidean(medoids_C3, t(as.matrix(global_medoids[1,]))))
medoids_C4 = all_medoids_day[seq(4,150,5),]
dist_med_c4 = as.data.frame(distEuclidean(medoids_C4, t(as.matrix(global_medoids[1,]))))
medoids_C5 = all_medoids_day[seq(5,150,5),]
dist_med_c5 = as.data.frame(distEuclidean(medoids_C5, t(as.matrix(global_medoids[1,]))))

# Combining data frames into a single data frame
combined_df = cbind(dist_med_c1, dist_med_c2, dist_med_c3, dist_med_c4, dist_med_c5)
names(combined_df) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")

df_long <- combined_df %>% 
  mutate(index = row_number()) %>%
  pivot_longer(cols = starts_with("Cluster"), names_to = "Cluster", values_to = "Value")

# line plot of distance changes between cluster centers for each day and original medoids from whole data set
colors_meds = c('Cluster 1' = '#083b5c', 'Cluster 2' = '#604b88','Cluster 3'= '#be4b88','Cluster 4' = '#fd635c','Cluster 5' = '#ffa600')
ggplot(df_long, aes(x = index, y = Value, color = Cluster)) +
  scale_color_manual(values = colors_meds) +
  geom_line() +
  labs(x = "Days of September 2014", y = "Euclidean distance from original medoid", title = "Euclidean distance from original medoid through September 2014") +
  theme_minimal()

# empty lists to store results
avg_sils_list_time <- list()
medoids_list_time <- list()

#to make iteration possible
uber$Hour2 = uber$Hour +1

# looping over hours of the day
for (time_to_select in 1:24) {
  
  # filter data for the current time
  time_data = uber[uber$Hour2 == time_to_select, ]
  print(nrow(time_data))
  coordinates_time = data.frame(latitude = time_data$Lat, longitude = time_data$Lon)
  
  # clustering using CLARA
  clara_clust_time = clara(coordinates_time, 5, metric="euclidean", stand=FALSE, samples=10,
                           sampsize=58, trace=0, medoids.x=TRUE,
                           rngR=FALSE, pamLike=FALSE, correct.d=TRUE)
  
  # storing results in lists and adding for day index values from iterations
  avg_sils_list_time[[time_to_select]] = clara_clust_time$silinfo$avg.width
  medoids_list_time[[time_to_select]] = clara_clust_time$medoids
}

# combining the results
all_avg_sils_time = unlist(avg_sils_list_time)
all_medoids_time = do.call(rbind, medoids_list_time)

medoid_labels = rep(c(1:5), 24)

df_medoids_time = data.frame(latitude = all_medoids_time[,1], longitude = all_medoids_time[,2], medoid_labels = as.character(medoid_labels))

medoid_colors = (c('1' = '#083b5c', '2' = '#604b88','3'= '#be4b88','4' = '#fd635c','5' = '#ffa600'))
ggplot(df_medoids_time, aes(x = longitude, y = latitude, color = medoid_labels)) +
  geom_point() +
  scale_color_manual(values = medoid_colors) +
  ggtitle("Cluster centers based on time") +
  xlab("Longitude") +
  ylab("Latitude")

# Gathering medoids of each cluster for each day
medoids_C1t = all_medoids_time[seq(1,120,5),]
dist_med_c1t = as.data.frame(distEuclidean(medoids_C1t, t(as.matrix(global_medoids[1,]))))
medoids_C2t = all_medoids_time[seq(2,120,5),]
dist_med_c2t = as.data.frame(distEuclidean(medoids_C2t, t(as.matrix(global_medoids[1,]))))
medoids_C3t = all_medoids_time[seq(3,120,5),]
dist_med_c3t = as.data.frame(distEuclidean(medoids_C3t, t(as.matrix(global_medoids[1,]))))
medoids_C4t = all_medoids_time[seq(4,120,5),]
dist_med_c4t = as.data.frame(distEuclidean(medoids_C4t, t(as.matrix(global_medoids[1,]))))
medoids_C5t = all_medoids_time[seq(5,120,5),]
dist_med_c5t = as.data.frame(distEuclidean(medoids_C5t, t(as.matrix(global_medoids[1,]))))

# Combining data frames into a single data frame
combined_dft = cbind(dist_med_c1t, dist_med_c2t, dist_med_c3t, dist_med_c4t, dist_med_c5t)
names(combined_dft) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")

dft_long <- combined_dft %>% 
  mutate(index = row_number()) %>%
  pivot_longer(cols = starts_with("Cluster"), names_to = "Cluster", values_to = "Value")

# line plot of distance changes between cluster centers for each time of the day and original medoids from whole data set
colors_meds = c('Cluster 1' = '#083b5c', 'Cluster 2' = '#604b88','Cluster 3'= '#be4b88','Cluster 4' = '#fd635c','Cluster 5' = '#ffa600')
ggplot(dft_long, aes(x = index-1, y = Value, color = Cluster)) +
  scale_color_manual(values = colors_meds) +
  geom_line() +
  labs(x = "Hours of the day", y = "Euclidean distance from original medoid", title = "Euclidean distance from original medoid through hours of the day") +
  theme_minimal()

#at which date was there the most rides?

day_freq = as.data.frame(table(uber$Day))
ggplot(day_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#E3120B", color = "black") +
  ggtitle("Rides per day of the month (September 2014)") +
  xlab("Day of September 2014") +
  ylab("Number of rides")

#at which day of the week on average was there the most rides?
day_week_freq = as.data.frame(table(uber$DayOfWeek))
day_week_freq$Var1 = factor(day_week_freq$Var1, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# mapping of weekdays to the number of occurrences in September 2014
weekday_counts <- c("Monday" = 5, "Tuesday" = 5, "Wednesday" = 4, "Thursday" = 4, "Friday" = 4, "Saturday" = 4, "Sunday" = 4)

# additional column with the corresponding counts for each day
day_week_freq$WeekdayOccurrences <- weekday_counts[day_week_freq$Var1]

# average frequency
day_week_freq$AverageFrequency <- day_week_freq$Freq / day_week_freq$WeekdayOccurrences

ggplot(day_week_freq, aes(x = Var1, y = AverageFrequency)) +
  geom_bar(stat = "identity", fill = "#E3120B", color = "black") +
  ggtitle("Rides per day of the week (September 2014)") +
  xlab("Day of the week") +
  ylab("Average Number of rides")

#at which time of the day was there the most rides? average rides per certain hour
hour_freq = as.data.frame(table(uber$Hour))

# formatting var1 to look like hour of the day
hour_freq$Var1 <- sprintf("%02d:00", hour_freq$Var1)
hour_freq$AverageFrequency <- hour_freq$Freq / 24

ggplot(hour_freq, aes(x = Var1, y = AverageFrequency)) +
  geom_bar(stat = "identity", fill = "#E3120B", color = "black") +
  ggtitle("Average amount of rides per time of the day across September 2014") +
  xlab("Time of the day") +
  ylab("Average Number of rides")


# forecasting of clusters with kmeans
set.seed(123)
dat <- coordinates
ind <- sample(nrow(dat), 300000)

dat[["train"]] <- TRUE
dat[["train"]][ind] <- FALSE

cl1 = kcca(dat[dat[["train"]]==TRUE, 1:2], k=5, kccaFamily("kmeans"), save.data=TRUE)
cl1

pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=dat[dat[["train"]]==FALSE, 1:2])

image(cl1)
points(dat[dat[["train"]]==TRUE, 1:2], col=pred_train, pch=19, cex=0.3)
points(dat[dat[["train"]]==FALSE, 1:2], col=pred_test, pch=22, bg="orange")

predicted_sil = Silhouette(cl1)
plot(predicted_sil)

#USEFUL CODE FOR LATER

# # Singular iteration tryout
# day_to_select = 1
# day1data = uber[uber$Day == day_to_select,]
# coordinates_day1 = data.frame(latitude = day1data$Lat, longitude = day1data$Lon)
# clara_clust_day1<-clara(coordinates, 5, metric="euclidean", stand=FALSE, samples=10,
#                    sampsize=100, trace=0, medoids.x=TRUE,
#                    rngR=FALSE, pamLike=FALSE, correct.d=TRUE)
# avg_sils = clara_clust_day1$silinfo$avg.width
# medoids = clara_clust_day1$medoids

# # adding new columns containing date and hour at which the rides occurred
# datetime = strsplit(uber$Date.Time, " ")
# date_vector <- do.call(c, lapply(datetime, function(x) x[1]))
# uber_date = as.Date(date_vector, format = "%m/%d/%Y")
# 
# uber_time <- do.call(c, lapply(datetime, function(x) x[2]))
# uberClustered = data.frame(latitude = uber$Lat, longitude = uber$Lon, uber_labels, uber_date, uber_time)
#