getwd()
#путь к папке
setwd("C:/Fishnet_stat/")
library(rgdal)
#откриваем .shp файл
map_ua <- readOGR(dsn = "Fishnet_stat.shp")
#строем график
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


region_map <- readOGR("Region/Join_map2.shp")
plot(region_map)
head(region_map@data, n = 20)
region_map@data[region_map$AREA < 5, ]
sel_ua <- region_map$AREA > 0
plot(region_map, col = "lightgrey") 
plot(region_map[ sel_ua, ], col = "turquoise", add = TRUE)
mean_ua_min <- region_map$MEAN > 0.5
plot(region_map[ mean_ua_min, ], col = "white", add = TRUE)
mean_ua_1 <- region_map$MEAN > 1 
plot(region_map[ mean_ua_1, ], col = "yellow", add = TRUE)
mean_ua_2 <- region_map$MEAN > 2
plot(region_map[ mean_ua_2, ], col = "yellow3", add = TRUE)
mean_ua_3 <- region_map$MEAN > 3
plot(region_map[ mean_ua_3, ], col = "orange", add = TRUE)
mean_ua_4 <- region_map$MEAN > 4
plot(region_map[ mean_ua_4, ], col = "orange2", add = TRUE)
mean_ua_max <- region_map$MEAN > 10
plot(region_map[ mean_ua_max, ], col = "red", add = TRUE)
region_map$MEAN


wdcu_dataset <- read.csv2("2017_08_09_WDCU_SD-DataSet (1).csv")
wdcu_dataset

#install.packages("readxl")
library(readxl)
library(gplots)
library(plotly)
stat_light <- read_excel("Statistic_light.xlsx", 1)
#функця для нормалізації
normalize <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))}

#графік без номалізації
bar_plot_stat <- plot_ly(stat_light, x = ~stat_light$`Регіон`, y = ~stat_light$Mean, type = 'bar', name = 'Night Light', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~stat_light$`Компонента безпеки життя`, name = 'Компонента безпеки життя', marker = list(color = 'rgb(204,204,204)')) %>%
  add_trace(y = ~stat_light$`Економічна безпека`, name = 'Економічна безпека', marker = list(color = 'rgb(15)')) %>%
  add_trace(y = ~stat_light$`Індекс економічного виміру`, name = 'Індекс економічного виміру', marker = list(color = 'rgb(563)')) %>%
  add_trace(y = ~stat_light$`Індекс людського розвитку`, name = 'Індекс людського розвитку', marker = list(color = 'rgb(638)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')
bar_plot_stat

#перетворення колонки з типу 'character' в тип 'numeric'
sapply(stat_light, mode)
mean_light <- transform(stat_light, Mean = as.numeric(Mean))
sapply(mean_light, class)
normalize(mean_light$Mean)
stat_light$`Індекс соціально-інституціонального виміру`
normalize(as.numeric(stat_light$`Індекс соціально-інституціонального виміру`))

#побудова графіка з нормалізованими данними (від 0 до 1) 
bar_plot_normalize <- plot_ly(stat_light, x = ~stat_light$`Регіон`, y = normalize(mean_light$Mean), type = 'bar', name = 'Night Light', marker = list(color = 'rgb(49,130,189)')) %>%
       add_trace (y = normalize(as.numeric(stat_light$`Компонента безпеки життя`)), name = 'Компонента безпеки життя', marker = list(color = 'rgb(204,204,204)')) %>%
       add_trace(y = normalize(as.numeric(stat_light$`Економічна безпека`)), name = 'Економічна безпека', marker = list(color = 'rgb(15)')) %>%
       add_trace(y = normalize(as.numeric(stat_light$`Індекс економічного виміру`)), name = 'Індекс економічного виміру', marker = list(color = 'rgb(563)')) %>%
       add_trace(y = normalize(as.numeric(stat_light$`Індекс людського розвитку`)), name = 'Індекс людського розвитку', marker = list(color = 'rgb(638)')) %>%
       layout(xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = ""),
          margin = list(b = 100),
          barmode = 'group')
#вивід графіка на екран
bar_plot_normalize