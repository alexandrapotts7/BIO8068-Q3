# Loading in packages and necessary scripts for GIS analysis ----

library(leaflet)
library(leafem)
library(mapview)
library(sf)
options("rgdal_show_exportToProj4_warnings"="none")
library(sf)
library(raster)

# Testing to see if leaflet will read in vectors ----
lakes <- st_read("gis_data/cumbria_lakes.shp")
lakes_ll <- st_transform(lakes,crs=ll_crs)

lake_view <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
  addFeatures(lakes_ll) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Lakes"),
    options = layersControlOptions(collapsed = TRUE)
  )

lake_view

# working!!!

# import raster elevation data ordanance survey projection ----

elevation <- raster("gis_data/elevation.tif")
plot(elevation)

# You will notice that the default colours are the wrong way round, so we can
# use the terrain.colors option to set low elevation to green, high to brown, 
# with 30 colour categories
plot(elevation, col=terrain.colors(30))

# converting lat and long 
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
elevation_ll <- projectRaster(elevation, crs=ll_crs)
mapview(elevation_ll)

# hill shade 
hs = hillShade(slope = terrain(elevation, "slope"), aspect = terrain(elevation, "aspect"))
plot(hs, col = gray(0:100 / 100), legend = FALSE)
# overlay with DEM
plot(elevation, col = terrain.colors(25), alpha = 0.5, add = TRUE)

# creation of contours 
elevation_contours <- rasterToContour(elevation) %>% st_as_sf()
plot(elevation, col=terrain.colors(30))
plot(elevation_contours, add=TRUE)

mapview(elevation_contours, add=TRUE) #Interactive view again

# adding in the wind turbine data set ----
wind_turbines <- st_read("gis_data/wind_turbines.shp")
print(wind_turbines)

plot(elevation, col=terrain.colors(30))
plot(wind_turbines, add=TRUE)

# can see the windfarm at the very bottom right hand side 

# now viewing interactively 
mapview(st_transform(wind_turbines, 4326))

# calculate slope and aspect ----
slope  <- terrain(elevation, unit="degrees")
aspect <- terrain(elevation, opt="aspect", unit="degrees")
plot(slope)
plot(aspect)

# adding slope and aspect to wind farm attribute ----
wind_turbines$slope <- extract(slope, wind_turbines)
wind_turbines$aspect <- extract(aspect, wind_turbines)

# looking at the wind turbine table
print(wind_turbines)

