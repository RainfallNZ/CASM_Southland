#' A function to combine spatial data sources of land use, soil drainage, slope and precipiation/irrigation, and uses them to lookup leach rates based on an Environment Southland designation. The processing is carried out at a 250 m x 250 m grid scale.
#'
#'This function generates a raster object of leach rates
#'
#'@param LanduseData A spatial data file (in ESRI polygon shapefile format) of the land use, as provided by Environment Southland.
#'@param SoilDrainageData A spatial data file (in ESRI polygon shapefile format) of the soil drainage class, as provided by Environment Southland.
#'@param SlopeClassData A spatial data file (in ESRI polygon shapefile format) of the slope classification, as provided by Environment Southland.
#'@param PrecipIrrig A spatial data file (in ESRI polygon shapefile format) of the precipiation/irrigation classification, as provided by Environment Southland.
#'@param LeachRateData An Excel (.xlsx) file with 4 columns providing: "Land Use";"Landscape Category";"Soil drainage";"Slope Class";"Precip (><1000mm)". Then a further 4 columns: "N loss (mean)";"N loss (median)"; "P loss (mean)"; "P loss (median)"
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster object of leach rates
#'@keywords Water Quality, CASM, leach
#'@export
LeachRateRasterCreator <- function(LanduseData=LanduseShapeFile,
                                   SoilDrainageData=SoilDrainageShapeFile,
                                   SlopeClassData=SlopeClassShapeFile,
                                   PrecipIrrig=PrecipIrrigShapeFile,
                                   LeachRateData = LeachRateLUTFile){
  
  if (!require(raster)) install.packages("raster"); library(raster)                #used for spatial processing
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)                #used for spatial processing
  if (!require(rasterVis)) install.packages("rasterVis"); library(rasterVis)                #used for plotting discrete rasters
  if (!require(openxlsx)) install.packages("openxlsx"); library(openxlsx)                #used for reading Excel data
  
  #Load lookup table of leaching rates based on land use, soil drainage, slope, and precipiation/irrigation
  LeachRateLookUpTable<- read.xlsx(LeachRateData)
  #Check for duplicates!! This has ocurred in the past and caused an error later on which took hours to figure out!
  LeachRateLookUpTable <- LeachRateLookUpTable[!duplicated(LeachRateLookUpTable[,1:5]),]
  #Expand any "All" rows to have a row for each possible combination
  AllIndices <- which(LeachRateLookUpTable$`Precip.(><1000mm)` == "All")
  #work through the "All" indices.
  UniqueClasses <- unique(LeachRateLookUpTable$`Precip.(><1000mm)`)
  #Get the number of additional rows needed to be created
  RowsToCreate <- length(UniqueClasses - 2)
  #Create a small data frame with just the number of rows that match the UniqueClasses (less 1 for the All class which we don't want any more)
  ExtraRows <- LeachRateLookUpTable[c()]
  
  #Convert the strings to factors to enable compatibility with the shapefiles and the rasters generated from them
  LeachRateLookUpTable[,1:5] <- lapply(LeachRateLookUpTable[,1:5], as.factor)
  
  #Create a "CombinedClasses" column
  LeachRateLookUpTable$CombinedClasses <- do.call(paste,lapply(LeachRateLookUpTable[,c(1,3,4,5)],as.numeric))


  #Load the precipitation/irrigation spatial data
  PrecipIrrigSpatial <- readOGR(PrecipIrrig, stringsAsFactors = TRUE)

  #Convert to raster, note the creation of a base raster, which all subsequent raster's align to
  RasterBase <- raster(resolution = 250, ext = extent(PrecipIrrigSpatial), crs = proj4string(PrecipIrrigSpatial) )
  PrecipIrrigRaster <- rasterize(PrecipIrrigSpatial,RasterBase,"Precip2")
  #Add a raster attribute table which labels the values
  PrecipIrrigRaster <- ratify(PrecipIrrigRaster)
  rat <- levels(PrecipIrrigRaster)[[1]]
  rat$PrecipIrrigClass <- levels(PrecipIrrigSpatial@data$Precip2)
  levels(PrecipIrrigRaster) <- rat
  #levelplot(PrecipIrrigRaster)
  
  #Load slope classification spatial data
  SlopeClassSpatial <- readOGR(SlopeClassData,stringsAsFactors = TRUE)
  SlopeClassRaster <- rasterize(SlopeClassSpatial, RasterBase, "Slope_clas")
  #Add a raster attribute table which labels the values
  SlopeClassRaster <- ratify(SlopeClassRaster)
  rat <- levels(SlopeClassRaster)[[1]]
  rat$SlopeClass <- levels(SlopeClassSpatial@data$Slope_clas)
  levels(SlopeClassRaster) <- rat
  #levelplot(SlopeClassRaster)
  
  #Load irrigated land spatial data
  SoilDrainageSpatial <- readOGR(SoilDrainageData,stringsAsFactors = TRUE)
  SoilDrainageRaster <- rasterize(SoilDrainageSpatial, RasterBase, "Drain")
  #Add a raster attribute table which labels the values
  SoilDrainageRaster <- ratify(SoilDrainageRaster)
  rat <- levels(SoilDrainageRaster)[[1]]
  rat$DrainClass <- levels(SoilDrainageSpatial@data$Drain)
  levels(SoilDrainageRaster) <- rat
  #levelplot(SoilDrainageRaster)
  
  #Load land use category spatial data
  LanduseSpatial <- readOGR(LanduseData, stringsAsFactors = TRUE)
  LanduseRaster <- rasterize(LanduseSpatial,RasterBase,"EcoClass")
  #Add a raster attribute table which labels the values
  LanduseRaster <- ratify(LanduseRaster)
  rat <- levels(LanduseRaster)[[1]]
  rat$LanduseClass <- levels(LanduseSpatial@data$EcoClass)
  levels(LanduseRaster) <- rat
  #levelplot(LanduseRaster)
  
  #Create a raster brick with all the parameters needed to determine the MPI leach rate
  TotalRaster <- brick(LanduseRaster,SoilDrainageRaster,SlopeClassRaster,PrecipIrrigRaster)
  names(TotalRaster) <- c("Landuse","SoilDrainage","SlopeClass","PrecipIrrig")
  #Mask the raster brick to just the Southland FMU areas as given in the PrecipIrrig polygon layer.
  TotalRaster <- rasterize(x=PrecipIrrigSpatial,y=TotalRaster,mask=TRUE)
  
  #Use "calc" to work through each cell x,y cell of the raster brick and select the appropriate leach rate
  LeachRateRaster <- calc(TotalRaster, function(x) {
    #Concatenate the predictor values of the current x,y, cell
    CriteriaToLookup <- paste(x,collapse=" ")

    #lookup the current cell's predictor string in the look up table
    N_loss <- LeachRateLookUpTable$N_loss[LeachRateLookUpTable$Landscape.Category %in% CriteriaToLookup]
    
    #Catch any missing values and make them NA
    if(length(N_loss) == 0) N_loss <- NA
    
    return(N_loss)
  })
  
}
