if (!require(shiny)) install.packages("shiny"); library(shiny)
if (!require(leaflet)) install.packages("leaflet"); library(leaflet)
if (!require(htmltools)) install.packages("htmltools"); library(htmltools)
if (!require(raster)) install.packages("raster"); library(raster)
if (!require(rgdal)) install.packages("rgdal"); library(rgdal)
if (!require(sf)) install.packages('sf'); library(sf) #needed to get the projection information on the raster in a reasonable format using the st_crs() function
if (!require(grDevices)) install.packages("grDevices"); library(grDevices)

MapData <- readRDS("Data/SpatialData.RDS")
#map <- readRDS("Data/map.RDS")

#Set the nonline map source to TOPO NZ maps
NZTopo250 = 'http://tiles-a.data-cdn.linz.govt.nz/services;key=8ed417cc81ea45a0b92d597307229b80/tiles/v4/layer=52324/EPSG:3857/{z}/{x}/{y}.png'
NZTopo50 = 'http://tiles-a.data-cdn.linz.govt.nz/services;key=8ed417cc81ea45a0b92d597307229b80/tiles/v4/layer=52343/EPSG:3857/{z}/{x}/{y}.png'

MeasurementSiteLabels <- lapply(seq(nrow(MapData$MeasurementSites@data)), function(i) {
  paste0("Measurement site:", '</br>', 
         MapData$MeasurementSites@data[i, "SiteDescription"], 
         '</br>REC#:',MapData$MeasurementSites@data[i, "nzsegment"]) 
})

EstuarySiteLabels <- lapply(seq(nrow(MapData$EstuarySites@data)), function(i) {
  Label <-  paste0("Estuary site:", '</br>', 
                   MapData$EstuarySites@data[i, "Name"], 
                   '</br>REC#:',MapData$EstuarySites@data[i, "nzsegment"])
  if(MapData$EstuarySites@data[i, "CASMnzsegment"] != MapData$EstuarySites@data[i, "nzsegment"]) {Label <- paste0(Label,'</br>CASM-REC#:',MapData$EstuarySites@data[i, "CASMnzsegment"])}
  return(Label)
})

UnMonitoredLakeSiteLabels <- lapply(which(!MapData$LakeSites@data$Monitored), function(i) {
  Label = paste0("Lake site:", '</br>', 
                 MapData$LakeSites@data[i, "LakeName"],'</br>',"LID: ",MapData$LakeSites@data[i, "LID"], 
                 '</br>REC#:',MapData$LakeSites@data[i, "nzsegment"])
  if(MapData$LakeSites@data[i, "CASMnzsegment"] != MapData$LakeSites@data[i, "nzsegment"]) {Label <- paste0(Label,'</br>CASM-REC#:',MapData$LakeSites@data[i, "CASMnzsegment"])}
  return(Label)
})

MonitoredLakeSiteLabels <- lapply(which(MapData$LakeSites@data$Monitored), function(i) {
  paste0("Lake site:", '</br>', 
         MapData$LakeSites@data[i, "LakeName"],'</br>',"LID: ",MapData$LakeSites@data[i, "LID"], 
         '</br>REC#:',MapData$LakeSites@data[i, "nzsegment"]) 
})

SubCatchmentSites <- lapply(seq(nrow(MapData$SubCatchmentSites@data)), function(i) {
  Label=paste0("Subcatchment site: ",  
               MapData$SubCatchmentSites@data[i, "RiverName"],'</br>',MapData$SubCatchmentSites@data[i, "Reason"], 
               '</br>REC#:',MapData$SubCatchmentSites@data[i, "nzsegment"])
  if(MapData$SubCatchmentSites@data[i, "CASMnzsegment"] != MapData$SubCatchmentSites@data[i, "nzsegment"]) {Label <- paste0(Label,'</br>CASM-REC#:',MapData$SubCatchmentSites@data[i, "CASMnzsegment"])}
  return(Label)
})

RiverMouthSiteLabels <- lapply(seq(nrow(MapData$RiverMouthSites@data)), function(i) {
  paste0("Rivermouth site: ",'</br>',MapData$RiverMouthSites@data[i, "River"], 
         '</br>REC#:',MapData$RiverMouthSites@data[i, "nzsegment"]) 
})

TeAoMaramaSiteLabels <- lapply(seq(nrow(MapData$TeAoMaramaSites@data)), function(i) {
  Label=paste0("Cultural site: ",'</br>',MapData$TeAoMaramaSites@data[i, "Waterbody"],'</br>',MapData$TeAoMaramaSites@data[i, "Location.descriptor"], 
               '</br>REC#:',MapData$TeAoMaramaSites@data[i, "nzsegment"])
  if(MapData$TeAoMaramaSites@data[i, "CASMnzsegment"] != MapData$TeAoMaramaSites@data[i, "nzsegment"]) {Label <- paste0(Label,'</br>CASM-REC#:',MapData$TeAoMaramaSites@data[i, "CASMnzsegment"])}
  return(Label)
})

#Setup the map
map <- leaflet::leaflet() %>% 
  addTiles(urlTemplate = NZTopo250, options = providerTileOptions(maxZoom=12),attribution = "<a href=\"http://https://www.linz.govt.nz/\">LINZ</a>", group = "LINZ Topographic") %>%
  addTiles(urlTemplate = NZTopo50, options = providerTileOptions(minZoom=12), group = "LINZ Topographic") %>%

#  leaflet::addProviderTiles(providers$OpenStreetMap) %>%
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

#Add the point source sites
PointSourceLabels <- sprintf(
  "<strong>Point source</strong><br/>%s ",
  MapData$PointSourceSite@data$Source
) %>% lapply(htmltools::HTML)

map <- map %>%
  addMarkers(data=MapData$PointSourceSites,
             icon= ~ icons(iconUrl = "Data/Icon_warning.png",iconAnchorX = 20, iconAnchorY = 0),  #Note the use of a subdirectory called Data to keep the icon file in. This is done to replicate the situation on the Shiny App so that I can simply copy the map script directly over.
             label = PointSourceLabels,
             #color = "black",
             #fillOpacity = 0.5,
             group="Point Sources")

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

map <- map %>%
  addPolygons(data=MapData$DiffuseSourceAreas,
              fillOpacity = 0,
              weight = 2,
              opacity = 1,
              color = "black",
              highlight = highlightOptions(
                weight = 5,
                color = "#FFF"),
              label = ~nzsegment,
              group="Diffuse load source areas")

#Add the Physiography raster. Note the custom palette to Match the Environment Southland colours as given in https://www.arcgis.com/home/webmap/viewer.html?useExisting=1&layers=fe2093666347411d92b4d0d5f4677af7
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

#Add the river network coloured by Water Plan Class. Note the custom palette set to match the Environment Southland choice of colours on their GIS page: http://gis.es.govt.nz/index.aspx?app=water-and-land
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
  addCircleMarkers(data = MapData$TeAoMaramaSites, color = "#61ba46",fillOpacity = 0.5, label = lapply(TeAoMaramaSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$MeasurementSites, color = "#FF3333",fillOpacity = 0.5, label = lapply(MeasurementSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$LakeSites[MapData$LakeSites@data$Monitored,], color = "darkorange", fillOpacity = 0.5, label = lapply(MonitoredLakeSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$LakeSites[!MapData$LakeSites@data$Monitored,], color = "yellow", fillOpacity = 0.5, label = lapply(UnMonitoredLakeSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$EstuarySites, color = "turquoise", fillOpacity = 0.5,label = lapply(EstuarySiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$RiverMouthSites, color = "brown", fillOpacity = 0.5,label = lapply(RiverMouthSiteLabels, htmltools::HTML)) %>%
  addCircleMarkers(data = MapData$SubCatchmentSites, color = "#0066CC", fillOpacity = 0.5,label = lapply(SubCatchmentSites, htmltools::HTML)) %>%
  
  addPolylines(data = MapData$RiverNetwork, color= "blue", weight = ~LineWidthPixels,group = "River Network",label = ~Label) %>%
  
  addLegend("topright", colors = c("#61ba46","#FF3333","darkorange","yellow","turquoise","brown","lightblue"), labels = c("Cultural","RWQ","Lake-Monitored","Lake-not monitored","Estuary","River Mouth","Subcatchment"),
            title = "Assessment point<br>locations",
            opacity = 1) %>%
  
  addLayersControl(
    overlayGroups =c("Major Catchments","Point Sources","Water Plan Classes","Physiographic Zones", "River Network", "Groundwater Management Zones","Diffuse load source areas"),
    options = layersControlOptions(collapsed=FALSE)
  ) %>%
  
  hideGroup(c("Major Catchments","Point Sources","Water Plan Classes","Groundwater Management Zones","River Network","Physiographic Zones","Diffuse load source areas")) %>%
  
  addScaleBar(position = "topleft", options = scaleBarOptions())

map

ui <- fluidPage(
  titlePanel("LWP"),
  leafletOutput("mymap", height = "100vh"),
  
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({map})
  
}

shinyApp(ui, server)