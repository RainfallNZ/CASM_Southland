#' A function to reformat CASM input data into SCAMP input data
#'
#'[CASMToSCAMP()]  Takes the data frame 'DiffuseInputsSiteExtendedTable' generated by 
#'SouthlandCASMPreProcess.Rmd' and creates three data.frames ready for transfer 
#'to an excel spreadsheet in the format specified in 'SCAMP input files for Tim K.xlsx'
#'These dataframes describe "Catchment Physical Inputs" and "Catchment WQ Inputs TN" "and Catchment WQ Inputs TP".
#'SCAMP is a revision of CASM. The new tables divide the original into physical and nutrient properties
#'and have a wide, rather than a long format based on land use.
#'@param CASMData A data frame formatted ready for CASM diffuse input. Intended 
#'to be the DiffuseInputsSiteExtendedTable data frame generated by "SouthlandCASMPreProcess.Rmd
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return Three data frames
#'@keywords REC River Environment Classification
#'@export
CASMToSCAMP <- function(CASMData = DiffuseInputsSiteExtendedTable){

  #Load libraries
  if (!require(tidyr)) install.packages('tidyr'); library(tidyr)

  #Parse the Node name to extract the land use
  CASMData$Landuse <- sub("^.*-","",CASMData$'Node Name')
  
  #Create a "Catchment Name" made up of the nzsegment and the Site name
  CASMData$'Catchment Name' <- paste(CASMData$nzsegment,CASMData$`Site Name or No.`,sep="-")
  
  #Create the area distribution table
  #Go from long to wide three times using the Land use variables.
  AreaTable <- as.data.frame(pivot_wider(CASMData,id_cols=c('Catchment Name'),names_from=Landuse,values_from='Land Area (ha)'))
  
  Landuses <- names(AreaTable)[-1]
  
  #Replace NA with 0 for the Landuse values
  AreaTable[,Landuses][is.na(AreaTable[,Landuses])] <- 0
  
  #Calculate total land in each node area
  AreaTable[,'Drainage Area (ha)'] <- rowSums(AreaTable[,Landuses],na.rm=TRUE)
  
  #Convert the landuse into percentages
  AreaTable[,Landuses] <- t(
    apply(AreaTable[,c(Landuses,'Drainage Area (ha)')], 1, function(x) {
    x <- x[Landuses] / x['Drainage Area (ha)'] * 100
      x
    }))
  
  #Stick the Receiving Stream and receiving stream location back on. Use the first match for the receiving stream loaction
  AreaTable$'Receiving Stream Name' <- CASMData$'Receiving Stream'[match(AreaTable$'Catchment Name', CASMData$'Catchment Name')]
  AreaTable$'Receiving Stream Location (km)' <- CASMData$'Discharge Location (km)'[match(AreaTable$'Catchment Name', CASMData$'Catchment Name')]
  AreaTable$'SCAMPBasin' <- CASMData$'CASMBasin'[match(AreaTable$'Catchment Name', CASMData$'Catchment Name')]
  
  #Re-order the columns to match the SCAMP specification
  AreaTable <- AreaTable[,c('SCAMPBasin','Catchment Name','Receiving Stream Name','Receiving Stream Location (km)','Drainage Area (ha)',
                            "Indigenous Forest and Conservation","Forestry","Dairy","Sheep and Beef","Horticulture","Urban and Industry","Public Use (incl lakes and rivers)","Deer","Arable")]
  AreaTable <- AreaTable[order(AreaTable$SCAMPBasin,AreaTable$'Receiving Stream Name'),]
  
  #create an N Export Coefficients table
  NTable <- as.data.frame(pivot_wider(CASMData,id_cols=c('Catchment Name'),names_from=Landuse,values_from='TN Export Coeff (kg/ha/yr)'))
  
  Landuses <- names(NTable)[-1]
  
  #Replace NA with 0 for the Landuse values
  NTable[,Landuses][is.na(NTable[,Landuses])] <- 0
  
  #Stick the Receiving Stream and receiving stream location back on. Use the first match for the receiving stream loaction
  NTable$'Receiving Stream Name' <- CASMData$'Receiving Stream'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  NTable$'Receiving Stream Location (km)' <- CASMData$'Discharge Location (km)'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  NTable$'Diffuse Path Attenuation Coefficient' <- CASMData$'TN Physiographic-based attenuation scale estimate'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  NTable$'SCAMPBasin' <- CASMData$'CASMBasin'[match(NTable$'Catchment Name', CASMData$'Catchment Name')]
  
  #Re-order the columns to match the SCAMP specification
  NTable <- NTable[,c('SCAMPBasin','Catchment Name','Receiving Stream Name','Receiving Stream Location (km)','Diffuse Path Attenuation Coefficient',
                      "Indigenous Forest and Conservation","Forestry","Dairy","Sheep and Beef","Horticulture","Urban and Industry","Public Use (incl lakes and rivers)","Deer","Arable")]
  NTable <- NTable[order(NTable$SCAMPBasin,NTable$'Receiving Stream Name'),]
  
  #create a P Export coefficients table
  PTable <- as.data.frame(pivot_wider(CASMData,id_cols=c('Catchment Name'),names_from=Landuse,values_from='TP Export Coeff (kg/ha/yr)'))
  
  Landuses <- names(PTable)[-1]
  
  #Replace NA with 0 for the Landuse values
  PTable[,Landuses][is.na(PTable[,Landuses])] <- 0
  
  #Stick the Receiving Stream and receiving stream location back on. Use the first match for the receiving stream loaction
  PTable$'Receiving Stream Name' <- CASMData$'Receiving Stream'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  PTable$'Receiving Stream Location (km)' <- CASMData$'Discharge Location (km)'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  PTable$'Diffuse Path Attenuation Coefficient' <- CASMData$'TP Physiographic-based attenuation scale estimate'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  PTable$'SCAMPBasin' <- CASMData$'CASMBasin'[match(PTable$'Catchment Name', CASMData$'Catchment Name')]
  
  #Re-order the columns to match the SCAMP specification
  PTable <- PTable[,c('SCAMPBasin','Catchment Name','Receiving Stream Name','Receiving Stream Location (km)','Diffuse Path Attenuation Coefficient',
                      "Indigenous Forest and Conservation","Forestry","Dairy","Sheep and Beef","Horticulture","Urban and Industry","Public Use (incl lakes and rivers)","Deer","Arable")]
  PTable <- PTable[order(PTable$SCAMPBasin,PTable$'Receiving Stream Name'),]
  
  return(list(AreaTable = AreaTable, NTable = NTable, PTable = PTable))

}
