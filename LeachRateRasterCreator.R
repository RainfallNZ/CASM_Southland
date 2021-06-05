#' A function to combine spatial data sources of land use, soil drainage, slope and precipitation/irrigation, and uses them to look up leach rates based on an Environment Southland designation.
#'
#'This function generates a raster object of leach rates
#'
#'@param LanduseData A spatial data file (in ESRI polygon shapefile format) of the land use, as provided by Environment Southland.
#'@param SoilDrainageData A spatial data file (in ESRI polygon shapefile format) of the soil drainage class, as provided by Environment Southland.
#'@param SlopeClassData A spatial data file (in ESRI polygon shapefile format) of the slope classification, as provided by Environment Southland.
#'@param PrecipIrrig A spatial data file (in ESRI polygon shapefile format) of the precipiation/irrigation classification, as provided by Environment Southland.
#'@param LeachRateData An Excel (.xlsx) file with 4 columns providing: "Land Use";"Landscape Category";"Soil drainage";"Slope Class";"Precipitation". Then a further 2 columns: "N loss (mean)"; "P loss (mean)"
#'@param Resolution The horizontal resolution in metres. Default is 250 m
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster object of leach rates
#'@keywords Water Quality, CASM, leach
#'@export
LeachRateRasterCreator <- function(LanduseData=LanduseShapeFile,
                                   Domain = CompleteDomain,
                                   SoilDrainageData=SoilDrainageShapeFile,
                                   SlopeClassData=SlopeClassShapeFile,
                                   PrecipIrrig=PrecipIrrigShapeFile,
                                   LeachRateData = LeachRateLUTFile,
                                   Resolution = 250){
  
  if (!require(raster)) install.packages("raster"); library(raster)                #used for spatial processing
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)                #used for spatial processing
  if (!require(rasterVis)) install.packages("rasterVis"); library(rasterVis)                #used for plotting discrete rasters
  if (!require(openxlsx)) install.packages("openxlsx"); library(openxlsx)                #used for reading Excel data
  
  #Load look up table of leaching rates based on land use, soil drainage, slope, and precipiation/irrigation
  LeachRateLookUpTable<- read.xlsx(LeachRateData)
  #Check for duplicates!! This has occurred in the past and caused an error later on which took hours to figure out!
  LeachRateLookUpTable <- LeachRateLookUpTable[!duplicated(LeachRateLookUpTable[,1:5]),]
  
  #Expand any "All" rows to have a row for each possible combination
  #Repeat for each of the class types
  for (ClassColumn in c("Soil.Drainage","Slope","Precipitation")){
    #Find the unique classes 
    UniqueClasses <- unique(LeachRateLookUpTable[,ClassColumn])
    #But don't have "All as a unique class
    UniqueClasses <- UniqueClasses[!UniqueClasses == "All"]
    
    #Get the rows that have "All" in the current class column
    AllIndices <- which(LeachRateLookUpTable[,ClassColumn] == "All")
    #work through the "All" indices, duplicating the rows to provide one for each unique classification
    for (AllIndex in AllIndices){
      LeachRateLookUpTable[AllIndex,ClassColumn] <- UniqueClasses[1]
      #Create a small data frame with just the number of rows that match the UniqueClasses (less 1 as the original "All" classed row row can get re-classed as one of the unique classes)
      ExtraRows <- LeachRateLookUpTable[rep(AllIndex,(length(UniqueClasses)-1)),]
      #Re-class them to match the unique classes
      ExtraRows[,ClassColumn] <- UniqueClasses[-1]
      #And add them on to the end of the look up table
      LeachRateLookUpTable <- rbind(LeachRateLookUpTable,ExtraRows)
    } #end of the for loop that worked throught the All indices

  }
  #Convert the strings to factors to enable compatibility with the shapefiles and the rasters generated from them
  LeachRateLookUpTable[,1:5] <- lapply(LeachRateLookUpTable[,1:5], as.factor)
  
  #Create a "CombinedClasses" column
  LeachRateLookUpTable$CombinedClasses <- do.call(paste,lapply(LeachRateLookUpTable[,c(1,3,4,5)],as.numeric))

  #Now move on to the spatial data
  #Load the precipitation/irrigation spatial data
  PrecipIrrigSpatial <- readOGR(PrecipIrrig, stringsAsFactors = TRUE)

  #Convert to raster, note the creation of a base raster, which all subsequent raster's align to
  RasterBase <- raster(resolution = Resolution, ext = extent(Domain), crs = proj4string(Domain) )
  PrecipIrrigRaster <- rasterize(PrecipIrrigSpatial,RasterBase,"Precip2")
  
  #Crop to the Complete domain, and then mask to the same
  PrecipIrrigRaster <- crop(PrecipIrrigRaster,extent(Domain))
  PrecipIrrigRaster <- mask(PrecipIrrigRaster, Domain)
  #Add a raster attribute table which labels the values
  PrecipIrrigRaster <- ratify(PrecipIrrigRaster)
  rat <- levels(PrecipIrrigRaster)[[1]]
  rat$PrecipIrrigClass <- levels(PrecipIrrigSpatial@data$Precip2)
  levels(PrecipIrrigRaster) <- rat
  #levelplot(PrecipIrrigRaster)
  
  #Load slope classification spatial data
  SlopeClassSpatial <- readOGR(SlopeClassData,stringsAsFactors = TRUE)
  SlopeClassRaster <- rasterize(SlopeClassSpatial, RasterBase, "Slope_clas")
  SlopeClassRaster <- crop(SlopeClassRaster,extent(Domain))
  SlopeClassRaster <- mask(SlopeClassRaster, Domain)
  #Add a raster attribute table which labels the values
  SlopeClassRaster <- ratify(SlopeClassRaster)
  rat <- levels(SlopeClassRaster)[[1]]
  rat$SlopeClass <- levels(SlopeClassSpatial@data$Slope_clas)
  levels(SlopeClassRaster) <- rat
  #levelplot(SlopeClassRaster)
  
  #Load irrigated land spatial data
  SoilDrainageSpatial <- readOGR(SoilDrainageData,stringsAsFactors = TRUE)
  SoilDrainageRaster <- rasterize(SoilDrainageSpatial, RasterBase, "Drain")
  SoilDrainageRaster <- crop(SoilDrainageRaster,extent(Domain))
  SoilDrainageRaster <- mask(SoilDrainageRaster, Domain)
   #Add a raster attribute table which labels the values
  SoilDrainageRaster <- ratify(SoilDrainageRaster)
  rat <- levels(SoilDrainageRaster)[[1]]
  rat$DrainClass <- levels(SoilDrainageSpatial@data$Drain)
  levels(SoilDrainageRaster) <- rat
  #levelplot(SoilDrainageRaster)
  
  #Load land use category spatial data
  LanduseSpatial <- readOGR(LanduseData, stringsAsFactors = TRUE)
  
  #Need to match the factor levels of the EcoClass attribute to the factor levels of the LeachRateLookUpTable
  LanduseSpatial@data$EcoClass <- factor(LanduseSpatial@data$EcoClass, levels = levels(LeachRateLookUpTable$Land.Use))
  
  LanduseRaster <- rasterize(LanduseSpatial,RasterBase,"EcoClass")
  LanduseRaster <- crop(LanduseRaster,extent(Domain))
  LanduseRaster <- mask(LanduseRaster, Domain)
  #Add a raster attribute table which labels the values
  LanduseRaster <- ratify(LanduseRaster)
  rat <- levels(LanduseRaster)[[1]]
  rat$LanduseClass <- levels(LanduseSpatial@data$EcoClass)[rat$ID]
  levels(LanduseRaster) <- rat
  #levelplot(LanduseRaster)
  
  #Create a raster brick with all the parameters needed to determine the leach rate from the look up table
  PredictorRasters <- brick(LanduseRaster,SoilDrainageRaster,SlopeClassRaster,PrecipIrrigRaster)
  names(PredictorRasters) <- c("Landuse","SoilDrainage","SlopeClass","PrecipIrrig")
  #Mask the raster brick to just the Southland FMU areas as given in the PrecipIrrig polygon layer.
  PredictorRasters <- rasterize(x=PrecipIrrigSpatial,y=PredictorRasters,mask=TRUE)
  
  #Figure out the leachrates for each of the leach rate types
  LeachTypes <- c("Nloss.(Mean)","Ploss.(Mean)")
  LeachRateRasters <- lapply(LeachTypes, function(LeachType){

    #Use "calc" to work through each x,y cell of the raster brick and select the appropriate leach rate
    LeachRateRaster <- calc(PredictorRasters, function(x) {
      #browser()
      #Concatenate the predictor values of the current x,y, cell
      CriteriaToLookup <- paste(x,collapse=" ")
      #if(all(complete.cases(x))) browser()
      #lookup the current cell's predictor string in the look up table
      LeachRate <- LeachRateLookUpTable[LeachRateLookUpTable$CombinedClasses %in% CriteriaToLookup,LeachType]
      
      #Catch any missing values and make them NA
      if(length(LeachRate) == 0) LeachRate <- NA
      
      return(LeachRate)
    })
    return(LeachRateRaster)
  })
  
  names(LeachRateRasters) <- LeachTypes

#Stick all the rasters together as a stack
  OutputRasterStack <- stack(brick(LeachRateRasters),PredictorRasters)

  
  return(OutputRasterStack)
}
