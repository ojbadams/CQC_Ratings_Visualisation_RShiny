library(leaflet)
library(rgdal)
library(tidyr)
library(tidyverse)

cqcData = read.csv("/home/ojba/Documents/University_Work/DALT7016/rshiny_NHS_data/cqc_long_lat.csv")


list.files("/home/ojba/Documents/University_Work/DALT7016/rshiny_NHS_data/")
file.exists("/home/ojba/Documents/University_Work/DALT7016/rshiny_NHS_data/Regions_(December_2019)_Boundaries_EN_BFE.shp")

#Code for shapeData taken from https://stackoverflow.com/questions/33045388/projecting-my-shapefile-data-on-leaflet-map-using-r

shapeData = readOGR(dsn = path.expand("/home/ojba/Documents/University_Work/DALT7016/rshiny_NHS_data/Regions_(December_2019)_Boundaries_EN_BFE.shp"))
shapeData <- spTransform(shapeData, CRS("+proj=longlat +datum=WGS84 +no_defs"))

countyData = readOGR(dsn = path.expand("/home/ojba/Documents/University_Work/DALT7016/rshiny_NHS_data/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BFE.shp"))
countyData = spTransform(countyData, CRS("+proj=longlat +datum=WGS84 +no_defs"))

plot(shapeData)

plot(countyData)

cqcData$Latest.Rating
lat_long_df = cqcData %>% select("lat", "long", "Latest.Rating")

lat_long_df_G = lat_long_df[which(lat_long_df$Latest.Rating == "Good"),]
lat_long_df_O = lat_long_df[which(lat_long_df$Latest.Rating == "Outstanding"),]
lat_long_df_I = lat_long_df[which(lat_long_df$Latest.Rating == "Requires improvement"),]



#https://community.rstudio.com/t/plotting-thousands-of-points-in-leaflet-a-way-to-improve-the-speed/8196/2
m = leaflet(options = leafletOptions(preferCanvas = T)) %>% 
  #addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = shapeData, col = "black", weight = 3, fillColor = "transparent") %>%
  #addCircles(data = lat_long_df_G, weight = 1, color = "orange") %>%
  addCircles(data = lat_long_df_O, weight = 1, color = "green") %>%
  addCircles(data = lat_long_df_I, weight = 1, color = "red")

  #addPolygons(data = countyData, col = "black")
m
