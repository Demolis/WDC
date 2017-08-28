getwd()
setwd("/home/olha/dev/R/Light Night")
library(rgdal)
map_ua <- readOGR(dsn = "Fishnet_stat.shp")
plot(map_ua)
head(map_ua@data, n = 20)
sapply(map_ua@data, class)
map_ua@data[map_ua$AREA < 35, ]
sel <- map_ua$AREA > 0
plot(map_ua, col = "lightgrey") 
plot(map_ua[ sel, ], col = "turquoise", add = TRUE) 
mean <- map_ua$MEAN > 1 
plot(map_ua[ mean, ], col = "yellow", add = TRUE) 
mean_2 <- map_ua$MEAN > 2
plot(map_ua[ mean_2, ], col = "orange", add = TRUE)
mean_3 <- map_ua$MEAN > 3
plot(map_ua[ mean_2, ], col = "orange3", add = TRUE)
mean_4 <- map_ua$MEAN > 4
plot(map_ua[ mean_4, ], col = "red", add = TRUE)



