# Q3 Shiny App ----

# downloading packages necessary ----

library(shiny)
library(leaflet)
library(leafem)
options("rgdal_show_exportToProj4_warnings"="none")
library(ggplot2)
library(dplyr)

# including the code from the non- interactive R script I have made into this shiny app ----

# reading in the relevant data including the RDS files ----

# elevation data ----
elevation <- raster("www/elevation.tif")
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
elevation_ll <- projectRaster(elevation, crs=ll_crs)
elevation500m <- aggregate(elevation, fact=10)
elevation500m_ll <- projectRaster(elevation500m, crs=ll_crs)
elevation500m_ll

elevation500m_ll <- readRDS("www/elevation500m_ll.RDS")


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

lakes_ll <- readRDS("www/lakes_ll.RDS")


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

settlement_ll    <- readRDS("www/settlements_ll.RDS")

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


rivers_ll <- readRDS("www/rivers_ll.RDS")

# going to read in the NBN data and also some images of the animals to place in the shiny app ----
barnowl_records <- read.csv("nbn_records/barnowl_records/barnowl_records.csv")

# Plotting to see trends over time
ggplot(barnowl_records, aes(x=year.processed)) +
    geom_histogram()

barnowl_records_per_yr <- barnowl_records %>% 
    group_by(year.processed) %>% 
    summarise(count_per_year = n())

ggplot(barnowl_records_per_yr, aes(x = year.processed, y=count_per_year)) +
    geom_line() + xlab("Years") + ylab("Birds observed")

kingfisher_records <- read.csv("nbn_records/kingfisher_records/kingfisher_records.csv")

# Plotting to see trends over time
ggplot(kingfisher_records, aes(x=year.processed)) +
    geom_histogram()

kingfisher_records_per_yr <- kingfisher_records %>% 
    group_by(year.processed) %>% 
    summarise(count_per_year = n())

ggplot(kingfisher_records_per_yr, aes(x = year.processed, y=count_per_year)) +
    geom_line() + xlab("Years") + ylab("Birds observed")

redkite_records <- read.csv("nbn_records/redkite_records/redkite_records.csv")

# Plotting to see trends over time
ggplot(redkite_records, aes(x=year.processed)) +
    geom_histogram()

redkite_records_per_yr <- redkite_records %>% 
    group_by(year.processed) %>% 
    summarise(count_per_year = n())

ggplot(redkite_records_per_yr, aes(x = year.processed, y=count_per_year)) +
    geom_line() + xlab("Years") + ylab("Birds observed")

# reading in and saving images ----

barnowl_image <- base64enc::dataURI(file="www/barn_owl.jpg", mime="image/jpg")
kingfisher_image <- base64enc::dataURI(file="www/kingfisher.jpg", mime="image/jpg")
redkite_image <- base64enc::dataURI(file="www/red_kite.jpg", mime="image/jpg")


# Define UI ----
ui <- fluidPage(
    
    # Application title
    titlePanel("The Rural Environment of Cumbria"),
    
    # Sidebar ----
    sidebarLayout(
        sidebarPanel(titlePanel("Bird species in Cumbria"),
                     p("This sidebar panel will show images and information about three bird species which are found in Cumbria. They correspond with the three bird species which are shown on the interactive map."),
                     p("Barn Owls have a distinctive heart shaped face and a light coloured body. They hunt mice and small rodents through the night. Unlike the Tawny Owl, the Barn Owl does not hoot but instead shrieks. On the interactive map, they are shown as the green marker."),
                     
                     img(src=barnowl_image,height="50%", width="50%"),
                     
                     p("The Kingfisher is easily distinguishable due to their brightly coloured blue and green upper body constrasting with an orange underside. On the interactive map they are shown as the purple marker. Have you ever seen one?"),
                     
                     img(src=kingfisher_image,height="50%", width="50%"),
                     
                     p("Red Kites prefer woodland areas and are known for their distinct long forked tail."),
                     
                     img(src=redkite_image,height="60%", width="60%"),
                    
                    
                     ),
        
    
        # Main panel ----
    mainPanel( p("This website has been created to display environmental data for the viewer. This data includes; elevation, lakes, rivers, settlements and three bird species;", strong("Red Kite, Kingfisher and Barn Owl."), p("The map is interactive, so you can toggle on and off the layers for a better viewing. At the end there is a couple of questions about your experience with these birds.")), 
                   leafletOutput(outputId = "map"),
               p("Below are graphs indicating Barn Owl, Kingfisher and Red Kite populations over time. These records were taken from the National Biodiversity Network (NBN). This database uses citizen science, in which members of the public can submit oberservations and sightings of species they have seen."),
               plotOutput(outputId = "barnowl_plot"),
               plotOutput(outputId = "kingfisher_plot"),
               plotOutput(outputId = "redkite_plot"),
               p("From these graphs, it is clear Barn Owls and Kingfishers are on the decline."),
               radioButtons(inputId = "my_checkgroup", 
                            h2("Have you ever seen or heard one of these three bird species?"), 
                            choices = list("Yes" = 1, 
                                           "No" = 2, 
                                           "Unsure" = 3),
                                          selected = 1),
               checkboxGroupInput("checkGroup", label = h3("If yes, which ones? You can tick more than one option."), 
                                  choices = list("Barn Owl" = 1, 
                                                 "Kingfisher" = 2, 
                                                 "Red Kite" = 3),
                                  selected = 1),
               actionButton(inputId="my_submitstatus", label="Sumbit answers.")), 
))



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
                             radius = 4, fillOpacity = 0.5, opacity = 0.5, col="green", popup = barnowl_records$common_name) %>%
            addCircleMarkers(kingfisher_records$decimalLongitude.processed,
                             kingfisher_records$decimalLatitude.processed, 
                             label = kingfisher_records$common_name, group = "Kingfisher",
                             labelOptions = labelOptions(interactive = "TRUE"),
                             radius = 4, fillOpacity = 0.5, opacity = 0.5, col="purple", popup = kingfisher_records$common_name) %>% 
            addCircleMarkers(redkite_records$decimalLongitude.processed, 
                             redkite_records$decimalLatitude.processed, 
                             label = redkite_records$common_name, group = "Red Kite",
                             labelOptions = labelOptions(interactive = "TRUE"),
                             radius = 4, fillOpacity = 0.5, opacity = 0.5, col="red", popup = redkite_records$common_name) %>%
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

    output$barnowl_plot <- renderPlot( ggplot(barnowl_records_per_yr, aes(x = year.processed, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Barn Owls Observed"))

    output$kingfisher_plot <- renderPlot( ggplot(kingfisher_records_per_yr, aes(x = year.processed, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Kingfishers observed")) 
    
    output$redkite_plot <- renderPlot( ggplot(redkite_records_per_yr, aes(x = year.processed, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Red Kites observed")) 
        
}

            

# Run the application 
shinyApp(ui = ui, server = server)
