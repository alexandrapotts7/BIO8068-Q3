# Q3 Shiny App ----

# downloading packages necessary ----

library(shiny)
library(shinipsum)
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

# including the code from the non- interactive R script I have made into this shiny app ----

# elevation data ----
elevation <- raster("www/elevation.tif")
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
elevation_ll <- projectRaster(elevation, crs=ll_crs)
elevation500m <- aggregate(elevation, fact=10)
elevation500m_ll <- projectRaster(elevation500m, crs=ll_crs)
elevation500m_ll





# lakes data ----
lakes <- st_read("www/cumbria_lakes.shp")
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

# settlement data ----
settlements <- st_read("www/cumbria_settlements.shp")
settlements_ll <- st_transform(settlements,crs=ll_crs)

settlement_view <- leaflet() %>% 
    addTiles(group = "OSM (default)") %>% 
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
    setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
    addFeatures(settlements_ll) %>% 
    addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"), 
        overlayGroups = c("settlements"),
        options = layersControlOptions(collapsed = TRUE)
    )


# rivers data ----
rivers <- st_read("www/cumbria_rivers.shp")
rivers_ll <- st_transform(rivers,crs=ll_crs)

rivers_view <- leaflet() %>% 
    addTiles(group = "OSM (default)") %>% 
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
    setView(lng = -3.0886, lat=54.4609, zoom=9) %>% 
    addFeatures(rivers_ll) %>% 
    addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"), 
        overlayGroups = c("rivers"),
        options = layersControlOptions(collapsed = TRUE)
    )


# Define UI ----
ui <- fluidPage(
    
    # Application title
    titlePanel("The Rural Environment of Cumbria"),
    
    # Sidebar  
    sidebarLayout(
        sidebarPanel(radioButtons(inputId = "my_checkgroup", 
                                  h3("Choose a species to view changes over time..."), 
                                  choices = list("Barn Owl" = 1, 
                                                 "Kingfisher" = 2, 
                                                 "Red Kite" = 3),)),
        
        mainPanel( p("This website has been created to display environmental data for the viewer. This data includes; elevation, lakes, rivers, settlements and three bird species;", strong("Red Kite, Kingfisher and Barn Owl.")), 
                   leafletOutput(outputId = "map"))))



# server logic ----
server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles(group = "OSM (default)") %>% 
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
            addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "elevation") %>% 
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
        
    })
    observeEvent(input$map, {
        click<-input$map
        text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
        print(text)
    })
    
}

            

# Run the application 
shinyApp(ui = ui, server = server)
