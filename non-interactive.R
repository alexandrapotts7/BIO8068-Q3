# Loading in packages and necessary scripts for GIS analysis ----

library(leaflet)
library(leafem)
library(mapview)
library(sf)
options("rgdal_show_exportToProj4_warnings"="none")
library(raster)
library(rgbif)
library(ggplot2)
library(dplyr)
library(rgbif)
library(BIRDS)

# import raster elevation data ordanance survey projection ----

elevation <- raster("www/elevation.tif")
plot(elevation)

# You will notice that the default colours are the wrong way round, so we can
# use the terrain.colors option to set low elevation to green, high to brown, 
# with 30 colour categories
plot(elevation, col=terrain.colors(30))

# converting lat and long 
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
elevation_ll <- projectRaster(elevation, crs=ll_crs)
mapview(elevation_ll)
elevation500m <- aggregate(elevation, fact=10)
elevation500m_ll <- projectRaster(elevation500m, crs=ll_crs)
mapview(elevation500m_ll)

# creating elevation in leaflet ----
elevation_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "elevation") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Elevation"),
    options = layersControlOptions(collapsed = TRUE)
  )

elevation_view

# saving as RDS
saveRDS(elevation500m_ll, file = "elevation.rds")


# lakes in leaflet ----
lakes <- st_read("www/cumbria_lakes.shp")
lakes_ll <- st_transform(lakes,crs=ll_crs)

lake_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
  addFeatures(lakes_ll, group = "lakes") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("lakes"),
    options = layersControlOptions(collapsed = TRUE)
  )

lake_view

# settlements in Cumbria in leaflet ----
settlements <- st_read("www/cumbria_settlements.shp")
settlements_ll <- st_transform(settlements,crs=ll_crs)

settlement_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
  addFeatures(settlements_ll, group = "Settlements") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("settlements"),
    options = layersControlOptions(collapsed = TRUE)
  )

settlement_view

# rivers in leaflet ----

rivers <- st_read("www/cumbria_rivers.shp")
rivers_ll <- st_transform(rivers,crs=ll_crs)

rivers_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
  addFeatures(rivers_ll, group = "Rivers") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("rivers"),
    options = layersControlOptions(collapsed = TRUE)
  )

rivers_view

# loading in some NBN data for different species to be viewed as part of the cumbrian environment ----

# kingfisher
kingfisher_records <- read.csv("nbn_records/kingfisher_records/kingfisher_records.csv")

# Plotting to see trends over time
ggplot(kingfisher_records, aes(x=year.processed)) +
  geom_histogram()

kingfisher_records_per_yr <- kingfisher_records %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(kingfisher_records_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Birds observed")

# red kite
redkite_records <- read.csv("nbn_records/redkite_records/redkite_records.csv")

# Plotting to see trends over time
ggplot(redkite_records, aes(x=year.processed)) +
  geom_histogram()

redkite_records_per_yr <- redkite_records %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(redkite_records_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Birds observed")

# barn owl
barnowl_records <- read.csv("nbn_records/barnowl_records/barnowl_records.csv")

# Plotting to see trends over time
ggplot(barnowl_records, aes(x=year.processed)) +
  geom_histogram()

barnowl_records_per_yr <- barnowl_records %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(barnowl_records_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Birds observed")

# barn owl plot ----
barnowl_records$common_name <- "Barn Owl"
barnowl_plot <- leaflet() %>%
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(barnowl_records$decimalLongitude.processed, 
                   barnowl_records$decimalLatitude.processed, 
                   label = barnowl_records$common_name, 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 4, fillOpacity = 0.5, opacity = 0.5, col="green") %>% 
  addLegend(colors = "green", opacity=1, labels="Barn Owl")

barnowl_plot

# king fisher plot ----
kingfisher_records$common_name <- "King Fisher"
kingfisher_plot <- leaflet() %>%
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(kingfisher_records$decimalLongitude.processed,
                   kingfisher_records$decimalLatitude.processed, 
                   label = kingfisher_records$common_name,
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 4, fillOpacity = 0.5, opacity = 0.5, col="blue") %>% 
  addLegend(colors = "blue", opacity=1, labels="King Fisher")

kingfisher_plot

# red kite ----
redkite_records$common_name <- "Red Kite"
redkite_plot <- leaflet() %>%
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(redkite_records$decimalLongitude.processed, 
                   redkite_records$decimalLatitude.processed, 
                   label = redkite_records$common_name, 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 4, fillOpacity = 0.5, opacity = 0.5, col="red") %>% 
  addLegend(colors = "red", opacity=1, labels="Red Kite")

redkite_plot

# now to attempt to join them all together into one bird plot ----

bird_plot <- leaflet() %>%
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(kingfisher_records$decimalLongitude.processed,
                   kingfisher_records$decimalLatitude.processed, 
                   label = kingfisher_records$common_name,
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 4, fillOpacity = 0.5, opacity = 0.5, col="blue") %>% 
  addLegend(colors = "blue", opacity=1, labels="King Fisher") %>% 
  addCircleMarkers(barnowl_records$decimalLongitude.processed,
                   barnowl_records$decimalLatitude.processed, 
                   label = barnowl_records$common_name, 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 4, fillOpacity = 0.5, opacity = 0.5, col="green") %>% 
  addLegend(colors = "green", opacity=1, labels="Barn Owl") %>%
  addCircleMarkers(redkite_records$decimalLongitude.processed,
                   redkite_records$decimalLatitude.processed, 
                   label = redkite_records$common_name, 
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 4, fillOpacity = 0.5, opacity = 0.5, col="red") %>%
  addLegend(colors = "red", opacity=1, labels="Red Kite")

bird_plot  

# merging all maps together as all in leaflet form so i can view in shiny easily ----

leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addRasterImage(elevation_ll,col=terrain.colors(30), group = "elevation") %>% 
  addCircleMarkers(barnowl_records$decimalLongitude.processed, 
                   barnowl_records$decimalLatitude.processed, 
                   label = barnowl_records$common_name, group = "Barn Owl",
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 4, fillOpacity = 0.5, opacity = 0.5, col="green") %>%
  addCircleMarkers(kingfisher_records$decimalLongitude.processed,
                   kingfisher_records$decimalLatitude.processed, 
                   label = kingfisher_records$common_name, group = "Kingfisher",
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 4, fillOpacity = 0.5, opacity = 0.5, col="blue") %>% 
  addCircleMarkers(redkite_records$decimalLongitude.processed, 
                   redkite_records$decimalLatitude.processed, 
                   label = redkite_records$common_name, group = "Red Kite",
                   labelOptions = labelOptions(interactive = "TRUE"),
                   radius = 4, fillOpacity = 0.5, opacity = 0.5, col="red") %>%
  addFeatures(lakes_ll, group = "lakes") %>% 
  addFeatures(rivers_ll, group = "rivers") %>%
  addFeatures(settlements_ll, group = "settlements", label = settlements_ll$NAME, labelOptions = labelOptions(interactive = "TRUE")) %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("elevation", "lakes", "rivers", "settlements", "Barn Owl", "Kingfisher", "Red Kite"),
    options = layersControlOptions(collapsed = FALSE)
  )

# all working and all toggling on and off when needed


