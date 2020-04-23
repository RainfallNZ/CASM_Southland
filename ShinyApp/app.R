library(shiny)
library(leaflet)
library(htmltools)
library(raster)
library(rgdal)
library(grDevices)

MapData <- readRDS("Data/SpatialData.RDS")
#map <- readRDS("Data/map.RDS")

MeasurementSiteLabels <- lapply(seq(nrow(MapData$MeasurementSites@data)), function(i) {
  paste0("Measurement site:", '</br>', 
         MapData$MeasurementSites@data[i, "sitename"]) 
})

EstuarySiteLabels <- lapply(seq(nrow(MapData$EstuarySites@data)), function(i) {
  paste0("Estuary site:", '</br>', 
         MapData$EstuarySites@data[i, "Name"]) 
})

LakeSiteLabels <- lapply(seq(nrow(MapData$LakeSites@data)), function(i) {
  paste0("Lake site:", '</br>', 
         MapData$LakeSites@data[i, "Lake_Name"]) 
})

ExtraSiteLabels <- lapply(seq(nrow(MapData$ExtraSites@data)), function(i) {
  paste0("Subcatchment site: ",  
         MapData$ExtraSites@data[i, "id"],'</br>',MapData$ExtraSites@data[i, "Reason"]) 
})

RiverMouthSiteLabels <- lapply(seq(nrow(MapData$RiverMouthSites@data)), function(i) {
  paste0("Rivermouth site: ",'</br>',MapData$RiverMouthSites@data[i, "River"]) 
})


#Setup the map
map <- leaflet::leaflet() %>% 
  leaflet::addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng=168,lat=-46.0,zoom=9)

#Add the main catchments
Catchmentlabels <- sprintf(
  "<strong>Major Catchment</strong><br/>%s ",
  MapData$MajorCatchments@data$FactSheetZ
) %>% lapply(htmltools::HTML)

MCpal <- colorFactor("Dark2", domain = MapData$MajorCatchments@data$FactSheetZ)

map <- map %>%
  addPolygons(data=MapData$MajorCatchments,
              fillOpacity = 0,
              weight = 2,
              opacity = 1,
              color = "grey",
              highlight = highlightOptions(
                weight = 5,
                color = "#666"),
              label = Catchmentlabels,
              group="Major Catchments")

#Add the groundwater zones
GroundwaterLabels <- sprintf(
  "<strong>Groundwater Zone</strong><br/>%s ",
  MapData$GroundwaterZones@data$Groundwate
) %>% lapply(htmltools::HTML)
GWpal <- colorFactor("YlOrRd", domain = MapData$GroundwaterZones@data$OBJECTID)

map <- map %>%
  addPolygons(data=MapData$GroundwaterZones,
              fillOpacity = 0,
              weight = 2,
              opacity = 1,
              color = "grey",
              highlight = highlightOptions(
                weight = 5,
                color = "#666"),
              label = GroundwaterLabels,
              group="Groundwater Management Zones")

#Add the Physiography raster. Note the custom palette to Match the Environment Southland colours as given in 
Physpal <- colorFactor(palette=c("#ffff73","#38a800","#7a1973","#9ed7c2","#aa66cd","#ffaa00","#734c00","#002673","#00c5ff","#9c9c9c"), levels <- c(1,2,3,4,5,6,7,8,9,10),
                       na.color = "transparent")
map <- map %>%
  addRasterImage(MapData$Physiography, colors = Physpal, opacity = 1, group = "Physiographic Zones") %>%  
  addLegend(pal = Physpal, 
            values = values(MapData$Physiography),
            title = "Physiography",
            opacity = 1,
            labFormat  = labelFormat(
              transform = function(x) {
                levels(MapData$Physiography)[[1]]$Physiography[which(levels(MapData$Physiography)[[1]]$ID == x)]
              }),
            layerId = "Physiography",
            group="Physiographic Zones")

#Add the river network coloured by Water PLan Class
#WPpal2 <- colorFactor("Dark2", MapData$RiverNetwork$WaterPlan,
#                    na.color = "transparent")
WPpal2 <- colorFactor(palette=c("#a4d1a4","#ffc34d","#ffff9e","#dcb496","#ff9ee9","#ff7f7f","#68a84d"), levels <- levels(MapData$RiverNetwork$WaterPlan),
                      na.color = "transparent")

map <- map %>%
  addPolylines(data = MapData$RiverNetwork, color= ~WPpal2(WaterPlan), weight = ~LineWidthPixels, label = ~WaterPlan, opacity = 1,group= "Water Plan Classes") %>%
  addLegend(pal = WPpal2, 
            values = MapData$RiverNetwork$WaterPlan,
            title = "Water Plan Classes",
            opacity = 1,
            #labFormat  = labelFormat(
            #transform = function(x) {
            #  levels(MapData$RiverNetwork$WaterPlan)[[1]]$WaterPlanClass[which(levels(MapData$RiverNetwork$WaterPlan)[[1]]$ID == x)]
            #}),
            layerId = "Water Plan Classess",
            group="Water Plan Classes")


map <- map %>%
  addCircleMarkers(data = MapData$MeasurementSites, color = "#FF3333",fillOpacity = 0.5, label = lapply(MeasurementSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$LakeSites, color = "darkorange", fillOpacity = 0.5, label = lapply(LakeSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$EstuarySites, color = "turquoise", fillOpacity = 0.5,label = lapply(EstuarySiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$RiverMouthSites, color = "brown", fillOpacity = 0.5,label = lapply(RiverMouthSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$ExtraSites, color = "#0066CC", fillOpacity = 0.5,label = lapply(ExtraSiteLabels, htmltools::HTML)) %>%
  
  addPolylines(data = MapData$RiverNetwork, color= "blue", weight = ~LineWidthPixels,group = "River Network") %>%
  
  addLegend("topright", colors = c("#FF3333","darkorange","turquoise","brown","lightblue"), labels = c("RWQ","Lake","Estuary","River Mouth","Subcatchment"),
            title = "Assessment point<br>locations",
            opacity = 1) %>%
  
  addLayersControl(
    overlayGroups =c("Major Catchments","Water Plan Classes","Physiographic Zones", "River Network", "Groundwater Management Zones"),
    options = layersControlOptions(collapsed=FALSE)
  ) %>%
  
  hideGroup(c("Major Catchments","Water Plan Classes","Groundwater Management Zones","River Network","Physiographic Zones"))

map


ui <- fluidPage(
  titlePanel("LWP"),
  leafletOutput("mymap", height = "100vh"),
  
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({map})
  
}

shinyApp(ui, server)