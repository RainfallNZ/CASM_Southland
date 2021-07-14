#' A function to combine spatial data sources of land "types" with potential nutrient loss-rate reductions.
#'
#'This function generates a raster object of loss rate reductions
#'
#'The data and loss-rate reductions are derived from McDowell, R.W., Monaghan, R.M., 
#'Smith, C., Manderson, A., Basher, L., Burger, D.F., Laurenson, S., Pletnyakov, P., 
#'Spiekermann, R., Depree, C., 2021. Quantifying contaminant losses to water from 
#'pastoral land uses in New Zealand III. What could be achieved by 2035? New Zealand 
#'Journal of Agricultural Research 64, 390–410. https://doi.org/10.1080/00288233.2020.1844763
#'
#'
#'@param LandTypes A spatial data file of 
#''typologies' as used in McDowell et al (2020). Requires an attribute called 'Typology' 
#'that matches the 'Typology' provided in the MitigationLookupTable. These data are ideally
#'originally sourced from 
#'https://databox.datacomcloud.co.nz/shares/folder/mYouM1LtlEc/
#'@param MitigationLookupTable A dataframe of the values to be mapped for each land 
#''type' (row). The intention was to use data generated from the tables provided in the 
#'McDowell et al (2020) supplementary material, but any Type-to-fraction-reduction 
#'data frame will work as long as the 'types' match the types in McDowell et al. (2020).
#'@param TypologyReclassFile A csv file which lists typologies in the spatial data
#'that are not in the mitigation data, and provides an equivalent mitigation type.
#'@param ReferenceRaster A reference raster to align to. Ideally use the loss-rate-raster
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster object of 
#'@keywords Water Quality, CASM, SCAMP, leach
#'@export
LossRaterReductionRasterCreator <- function(LandTypes=LanduseShapeFile,
                                   MitigationLookUpTable=NA,
                                   TypologyReclassFile = "D:\\Projects\\LWP\\SouthlandRegionalForumModelling\\Data\\OurLandAndWaterDairyTypeReclass.csv",
                                   ReferenceRaster = LossRateRaster){
  
  if (!require(raster)) install.packages("raster"); library(raster)                #used for spatial processing
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)                #used for spatial processing
  if (!require(rasterVis)) install.packages("rasterVis"); library(rasterVis)                #used for plotting discrete rasters
  #browser()
  #Load the Type reclass table
  TypeReclassTable <- read.csv(TypologyReclassFile)
  
  #reclass the spatial typologies using the TypeReclassTable
  UpdatedTypologies <- TypeReclassTable$MitigationTypology[match(LandTypes$Typology,TypeReclassTable$SpatialTypology)]
  LandTypes$Typology[!is.na(UpdatedTypologies)] <- UpdatedTypologies[!is.na(UpdatedTypologies)]
  
  #Join the look up table to the spatial data
  LandTypes$LossRateReduction <- MitigationLookUpTable[match(LandTypes$Typology,MitigationLookUpTable$Typology),2]
  
  #Convert the spatial data to raster
  TypologyRaster <- rasterize(LandTypes,ReferenceRaster,"LossRateReduction")
  

  return(TypologyRaster)
}

#' A function to adjust the leachrate rasters for the Our Land and Water Mitigation scenarios
#'
#'This function generates a raster object of adjusted leach rates
#'
#'@param LeachRates A raster stack of leach rates. Raster 1 is for N, raster 2 is for P
#'@param MitigationDataFile A csv file of Our Land and Water mitigation loads. This file
#'was generated from the tables provided in the McDowell et al (2020) supplementary material.
#'@param MitigationOfInterest The name of our Land and Water mitigaion scenario to apply.
#'Currently limited to one of "Potential2015", the default, and "Potential2035",  
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster stack of leach rates adjusted by the mitigation scenario 
#'@keywords Water Quality, CASM, SCAMP, leach
#'@export
LeachRateAdjuster <- function(LeachRates=LeachRateRaster,
                              MitigationDataFile = "D:\\Projects\\LWP\\SouthlandRegionalForumModelling\\Data\\OurLandAndWaterMitigationLoads.csv",
                              MitigationOfInterest = "Potential2015",
                              DairyLandTypeSpatialDataFile = "D:\\Projects\\LWP\\SouthlandRegionalForumModelling\\Data\\GIS\\OLW_Southland_TypologiesDairy\\OLW_Southland_TypologiesDairy.shp",
                              SheepAndBeefLandTypeSpatialDataFile = "D:\\Projects\\LWP\\SouthlandRegionalForumModelling\\Data\\GIS\\OLW_Southland_TypologiesSnB\\OLW_Southland_TypologiesSnB.shp"){
  
  if (!require(raster)) install.packages("raster"); library(raster)                #used for spatial processing
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)                #used for spatial processing
  if (!require(rasterVis)) install.packages("rasterVis"); library(rasterVis)                #used for plotting discrete rasters
  if (!require(sf)) install.packages("sf"); library(sf)                #used for spatial data
  
  #Read in the Mitigation data
  MitigationData <- read.csv(MitigationDataFile, check.names = FALSE)
  
  #Read in the Our Land and Water Spatial data, and combine into a single spatial file
  DairySpatialData <- sf::st_read(dsn = DairyLandTypeSpatialDataFile)
  SheepAndBeefSpatialData <- sf::st_read(dsn = SheepAndBeefLandTypeSpatialDataFile)
  
  #Clean up the sheep and beef typology names to exactly match those in Mitigation Lookup Table
  SheepAndBeefSpatialData$Typology <- sub("^.*\\. ","",SheepAndBeefSpatialData$Classifica)
  
  #Merge spatial data
  AllTypes <- do.call(rbind,list(DairySpatialData[,c("Typology","geometry")],SheepAndBeefSpatialData[,c("Typology","geometry")]))
  
  for (Nutrient in c('N','P')){
    
    #Calculate the change in load associated with the mitigation
    ColumnsOfInterest <- colnames(MitigationData)[which(grepl(paste0(Nutrient,"_loss"),colnames(MitigationLookUpTable)))]
    #Select the mitigation scenario of choice.
    CurrentColumn <-  which(colnames(MitigationData) == paste0("Current",Nutrient,"_loss"))
    ScenarioColumn <- which(colnames(MitigationData) == paste0(MitigationOfInterest,Nutrient,"_loss"))
    MitigationData$ScenarioMitigationFraction <- (MitigationData[,CurrentColumn] - MitigationData[,ScenarioColumn]) / MitigationData[,CurrentColumn]
    MitigationLookUpTable <- MitigationData[,c("Typology","ScenarioMitigationFraction")]  
  
    #create the loss rate reduction raster
    LossRateReductionRaster <- LossRaterReductionRasterCreator(LandTypes = AllTypes,
                                                             MitigationLookUpTable = MitigationLookUpTable,
                                                             ReferenceRaster = LeachRates[[1]]
    )
    #Set no data values to 0
    LossRateReductionRaster[is.na(LossRateReductionRaster[])] <- 0
    if (Nutrient =='N'){
    adjustedRaster <- stack(LeachRates[[1]] * (1 - LossRateReductionRaster))
    } else {
      adjustedRaster[[2]] <- LeachRates[[2]] * (1 - LossRateReductionRaster)
    }
  }

  
  return(adjustedRaster)
}