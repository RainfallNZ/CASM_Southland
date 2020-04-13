library(shiny)
library(leaflet)
library(htmltools)
library(raster)
library(rgdal)
library(grDevices)

#r_colors <- rgb(t(col2rgb(colors()) / 255))
#names(r_colors) <- colors()
MapData <- readRDS("Data/SpatialData.RDS")

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
  paste0("Rivermouth site: ",  
         MapData$RiverMouthSites@data[i, "id"],'</br>',MapData$RiverMouthSites@data[i, "River"]) 
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
              color = "white",
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
              color = "white",
              highlight = highlightOptions(
                weight = 5,
                color = "#666"),
              label = GroundwaterLabels,
              group="Groundwater Management Zones")

#Add the Physiography raster
Physpal <- colorFactor(rainbow(10), values(MapData$Physiography),
                       na.color = "transparent")
map <- map %>%
  addRasterImage(MapData$Physiography, colors = Physpal, opacity = 0.8, group = "Physiographic Zones") %>%  
  addLegend(pal = Physpal, 
            values = values(MapData$Physiography),
            title = "Physiography",
            labFormat  = labelFormat(
              transform = function(x) {
                levels(MapData$Physiography)[[1]]$Physiography[which(levels(MapData$Physiography)[[1]]$ID == x)]
              }),
            layerId = "Physiography",
            group="Physiographic Zones")

#Add the Water Plan Classification raster
WPpal <- colorFactor("Paired", values(MapData$WaterPlan),
                     na.color = "transparent")
map <- map %>%
  addRasterImage(MapData$WaterPlan, colors = WPpal, opacity = 0.8, group = "Water Plan Classes") %>%  
  addLegend(pal = WPpal, 
            values = values(MapData$WaterPlan),
            title = "Water Plan Classes",
            labFormat  = labelFormat(
              transform = function(x) {
                levels(MapData$WaterPlan)[[1]]$WaterPlanClass[which(levels(MapData$WaterPlan)[[1]]$ID == x)]
              }),
            layerId = "Water Plan Classess",
            group="Water Plan Classes")


map <- map %>%
  addCircleMarkers(data = MapData$MeasurementSites, color = "#FF3333",fillOpacity = 0.5, label = lapply(MeasurementSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$LakeSites, color = "darkorange", fillOpacity = 0.5, label = lapply(LakeSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$EstuarySites, color = "turquoise", fillOpacity = 0.5,label = lapply(EstuarySiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$RiverMouthSites, color = "brown", fillOpacity = 0.5,label = lapply(RiverMouthSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$ExtraSites, color = "#0066CC", fillOpacity = 0.5,label = lapply(ExtraSiteLabels, htmltools::HTML)) %>%
  
  addPolylines(data = MapData$RiverNetwork, color= "blue", weight = ~LineWidthPixels,group = "River Network") %>%
  
  addLegend("topright", colors = c("#FF3333","darkorange","turquoise","brown","lightblue"), labels = c("Measurement SItes","Lake Sites","Estuary Sites","River Mouth Sites","Subcatchment Sites"),
            title = "Sites",
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