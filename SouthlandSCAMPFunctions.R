#SCAMP Functions

#' Compare water management sub zone loads between two CASM input files
#'
#' LoadComparer returns the difference in loads for each water management sub zone of two CASM input files
#'
#' This function is intended for use to check how the loads, described by the "Diffuse Inputs"
#' sheet in each CASM input spreadsheet, differ. 
#' @param CASMFiles A vector of file names of the CASM input files to be compared
#' @param Nutrient String depicting which nutrient to sumarise. Default is 'TN'. Only other option is 'TP'
#' @return A dataframe with absolute difference and percent difference of management sub-zone loads.
#' @examples
#' LoadComparer(CASMFiles = c(file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_3b_IntensiveTable14_2Year20OrConsentedWithCoxCalibrated.xlsx"),SecondCASMFIle = file.path(DataDirectory,"Test.xlsx")))
LoadComparer <- function(CASMFiles = c("CASMInputData1.xlsx","CASMInputData2.xlsx"),Nutrient='TN'){

  #Read the "Diffuse Inputs" tab from each input spreadsheet
  ZoneLoadList <- lapply(CASMFiles, function(CASMFile){
    DiffuseData <- read.xlsx(CASMFile,sheet = "Diffuse Inputs")
    DiffuseData$Zone_Code <- sub("-.*$","",DiffuseData$Node.Name)
    NutrientColumnName <- paste0(Nutrient,'.Export.Coeff.(kg/ha/yr)')
    #DiffuseData$load <- DiffuseData$`Land.Area.(ha)` * DiffuseData$`TN.Export.Coeff.(kg/ha/yr)`
    DiffuseData$load <- DiffuseData$`Land.Area.(ha)` * DiffuseData[[NutrientColumnName]]
    ZoneLoads <- ddply(DiffuseData, "Zone_Code", function(x) sum(x$load, na.rm=TRUE))
    return(ZoneLoads)
  })
  CombinedZones <- merge(ZoneLoadList[[1]],ZoneLoadList[[2]],by="Zone_Code")
  CombinedZones$AbsDiff <- CombinedZones[,3] - CombinedZones[,2]
  CombinedZones$PctDiff <- round(CombinedZones$AbsDiff/CombinedZones[,2]*100,1)
  return(CombinedZones)
}

#' Compare water management sub zone loads between two CASM input files
#'
#' LoadComparerV2 returns the difference in loads for each water management sub zone of two CASM input files
#'
#' This function is intended for use to check how the "Catchment Physical Inputs", 
#' sheets compare between two SCAMP input spreadsheets. This is an application of 
#' the base::setdiff() function. See https://statisticsglobe.com/setdiff-r-function/ 
#' @param SCAMPFiles A vector of file names of the SCAMP input files to be compared
#' @param Nutrient String depicting which nutrient to sumarise. Default is 'TN'. Only other option is 'TP'
#' @return A dataframe of columns in the first spreadsheet that are changed in the second sheet.
#' @examples
#' LoadComparer(SCAMPFiles = c(file.path(DataDirectory,"CASM-Inputs_SEL_Scenario_3b_IntensiveTable14_2Year20OrConsentedWithCoxCalibrated.xlsx"),SecondCASMFIle = file.path(DataDirectory,"Test.xlsx")))
LoadComparerV2 <- function(SCAMPFiles = c("CASMInputData1.xlsx","CASMInputData2.xlsx"),Nutrient='TN'){
  
  #Read the "Catchment Physical Inputs" tab from each input spreadsheet
  CatchmentDataList <- lapply(SCAMPFiles, function(SCAMPFile){
    CatchmentData <- read.xlsx(SCAMPFile,sheet = "Catchment Physical Inputs",startRow=3)
    CatchmentData$SCAMPnzsegment <- sub("-.*$","",CatchmentData$Catchment.Name)
    #NutrientColumnName <- paste0(Nutrient,'.Export.Coeff.(kg/ha/yr)')
    #DiffuseData$load <- DiffuseData$`Land.Area.(ha)` * DiffuseData$`TN.Export.Coeff.(kg/ha/yr)`
    #DiffuseData$load <- DiffuseData$`Land.Area.(ha)` * DiffuseData[[NutrientColumnName]]
    #CatchmentData <- ddply(DiffuseData, "Zone_Code", function(x) sum(x$load, na.rm=TRUE))
    return(CatchmentData)
  })
  Difference <- base::setdiff(CatchmentDataList[[1]],CatchmentDataList[[2]])
  #CombinedZones <- merge(ZoneLoadList[[1]],ZoneLoadList[[2]],by="SCAMPnzsegment")
  #CombinedZones$AbsDiff <- CombinedZones[,3] - CombinedZones[,2]
  #CombinedZones$PctDiff <- round(CombinedZones$AbsDiff/CombinedZones[,2]*100,1)
  return(Difference)
}

#' Label stream reaches
#' 
#' This function names reach segments with real-world names.
#' Some names are looked up using the RECReachNamer() function. These
#' names need to be manually checked.
#' The rest are manually named using the NetworkNamer() function.
#' @parma RECV2Network a spatial river network
#' @param ExtraRECReachLabelsFile A two column csv file of RECV2 nzsegment numbers 
#' and related names. Columns must be called "nzsegment" and "name" respectively
#' @param EstuarySites A dataframe that must include a column of nzsegment numbers 
#' and of Estuary names. Columns must be called "nzsegment" and "Name" respectively.
#' @return a labelled version of the spatial river network

NetworkLabeller <- function(RECV2Network = CompleteSpatialNetwork,
                            ExtraRECReachLabelsFile = NA,
                            EstuarySites = NA){

  #Name as much as possible from the LINZ river naming spatial data sets
  NamedCompleteSpatialNetwork <- RECReachNamer(RECV2Network = RECV2Network)
  
  #Talk amongst yourselves while this layer is manually checked in QGIS
  #st_write(NamedCompleteSpatialNetwork, "CASM-StreamNetworkForNaming", driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  #Load in the csv file with a direct nzsegment-to-name relationship (manually prepared!)
  ExtraRECReachLabels <- read.table(ExtraRECReachLabelsFile,sep=",",stringsAsFactors = FALSE, header=TRUE)
  
  #Use this to update the names
  IndexOfNamesToUpdate <- which(NamedCompleteSpatialNetwork$nzsegment %in% ExtraRECReachLabels$nzsegment)
  NamedCompleteSpatialNetwork$name[IndexOfNamesToUpdate] <- ExtraRECReachLabels$name[match(NamedCompleteSpatialNetwork$nzsegment[IndexOfNamesToUpdate],ExtraRECReachLabels$nzsegment)]
  
  #Use the estuary sites list to update any reaches that don't already have a name (useful for all the outlet reaches)
  #Find which of the estuary reach numbers need updating
  EstuaryReachesToBeUpdated <- which(NamedCompleteSpatialNetwork$nzsegment %in% EstuarySites$nzsegment & NamedCompleteSpatialNetwork$name %in% c("TBC",NA))
  #Then update them
  NamedCompleteSpatialNetwork$name[EstuaryReachesToBeUpdated] <- EstuarySites$Name[match(NamedCompleteSpatialNetwork$nzsegment[EstuaryReachesToBeUpdated],EstuarySites$nzsegment)]
  
  #Name all remaining reaches by checking  the name immediately downstream or by getting manual input
  NetworkLabelList <- NetworkNamer(RECNetwork = NamedCompleteSpatialNetwork)
  
  #Add the tributary labels to the network
  #SegmentToLabelLookUpTable <- do.call(rbind,NetworkLabelList)
  #RECV2Network@data$Label <- SegmentToLabelLookUpTable$name[match(RECV2Network@data$nzsegment,SegmentToLabelLookUpTable$nzsegment)]
  return(NetworkLabelList)
}

#' A function to name REC reaches
#'
#'This function accepts a vector of RECV2 river network as a spatial data frame and does it's best to name the reaches through using the \href{https://data.linz.govt.nz/layer/103631-nz-river-name-polygons-pilot/}{LINZ name polygons} and \href{https://data.linz.govt.nz/layer/103632-nz-river-name-lines-pilot/}{LINZ name lines} data
#'
#'@param RECV2Network A spatial line object that needs to be named. Either a spatial data frame, or a simple feature object of the REC network to be named
#'@param LINZNameLines The LINZ name line spatial data frame that  covers the area of interest
#'@param LINZNamePolygons The LINZ name polygon spatial data frame that covers the area of interest
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A copy of the input REC network with an additional attribute called "Name"
#'@keywords REC LINZ river names
#'@export
RECReachNamer <- function(RECV2Network, LINZNameLinesFile="D:\\Projects\\LWP\\SouthlandRegionalForumModelling\\Data\\GIS\\LINZRiverNames\\LINZRiverNames.shp",
                          LINZNamePolygonsFile="D:\\Projects\\LWP\\SouthlandRegionalForumModelling\\Data\\GIS\\LINZRiverNames\\LINZRiverNamePolygons.shp" ){
  
  if (!require(sf)) install.packages("sf"); library(sf)                         # simple features packages for handling vector GIS data
  if (!require(httr)) install.packages("httr"); library(httr)                   # generic webservice package
  if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)    # a suite of packages for data wrangling, transformation, plotting, ...
  if (!require(data.table)) install.packages("data.table"); library(data.table) # useful for summarising data
  
  
  LINZNameLines <- st_read(dsn =LINZNameLinesFile, stringsAsFactors = FALSE)
  LINZNamePolygons <- st_read(dsn =LINZNamePolygonsFile, stringsAsFactors = FALSE)
  
  if(class(RECV2Network)[1] != "sf")  RECV2Network <- st_as_sf(RECV2Network)
  RECV2Network <- st_transform(RECV2Network, crs=st_crs(LINZNameLines))
  
  #Undertake a spatial join between the REC network and the river name line data
  NamedNetwork <- RECV2Network %>% 
    st_join(LINZNamePolygons[,"name"], largest = TRUE ) %>% 
    #st_transform(st_crs(RECV2Network)) %>%                  #This is needed as some of the projection details get lost in the join
    st_join(LINZNameLines[,"name"], suffixes =c(".polygons",".lines"),largest = TRUE )
  
  #Default assumption is that the line name is correct
  NamedNetwork$name <- NamedNetwork$name.x
  NamedNetwork$name[is.na(NamedNetwork$name.x)] <- NamedNetwork$name.y[is.na(NamedNetwork$name.x)]
  
  #For reaches that get named from both the polygon names and the line names, make a sensible decision based on connectivity
  #Find the features with a name clash. This can happen where the REC network is not a perfect match to the LINZ network, or on first reaches of tributaries of a polygon river
  NameClashIndices <- which(NamedNetwork$name.x != NamedNetwork$name.y)
  
  #Work through the name clashes one by one and attempt to fix them.
  #The rules for name allocation are (applied sequentially, so in reverse order of priority):
  #1/ The name is "TBC" indicating it needs to be confirmed manually
  #2/ If an immediately upstream reach is named the same as the downstream reach (and they don't also have clashes) then this is the name to use
  #3/ If a sibling reach has the same name as the downstream reach (and they don't also have name clashes) then the name is the choice that is NOT the downstream name
  
  ClashNames <- sapply(NameClashIndices, function(NameClashIndex){
    
    #Get the possible names
    Names <- NamedNetwork %>% st_drop_geometry() %>% dplyr::select(name.x, name.y) %>% slice(NameClashIndex) %>% unlist()
    
    #Other clash indices except this one
    OtherClashIndices <- NameClashIndices[NameClashIndices != NameClashIndex]
    
    #Find the name of the reach immediately downstream
    DownstreamName <- NamedNetwork$name[NamedNetwork$FROM_NODE == NamedNetwork$TO_NODE[NameClashIndex]]
    
    #Find the upstream names
    UpstreamNames <- NamedNetwork$name[NamedNetwork$TO_NODE == NamedNetwork$FROM_NODE[NameClashIndex]]
    
    #Find the name of any sibling tributaries (i.e. that connect at the downstream end of this reach)
    SiblingIndices <- which(NamedNetwork$TO_NODE == NamedNetwork$TO_NODE[NameClashIndex])
    SiblingName <- NamedNetwork$name[SiblingIndices[SiblingIndices != NameClashIndex]]
    #SiblingName <- NamedNetwork$name[NamedNetwork$TO_NODE == NamedNetwork$TO_NODE[NameClashIndex]]
    
    #Check for sequential clashes, as this indicates manual intervention is required 
    #Check if the reach immediately downstream has a name clash
    DownstreamClash <- which(NamedNetwork$FROM_NODE == NamedNetwork$TO_NODE[NameClashIndex]) %in% OtherClashIndices
    
    #Check if any of the immediately upstream reaches also have a name clash
    UpstreamClash <- any(which(NamedNetwork$TO_NODE == NamedNetwork$FROM_NODE[NameClashIndex]) %in% OtherClashIndices)
    
    #Check if any of the sibling tributaries have a name clash
    SiblingClash <- any(which(NamedNetwork$TO_NODE == NamedNetwork$TO_NODE[NameClashIndex]) %in% OtherClashIndices)
    
    #Rule 1
    Name <- "TBC"
    
    # Rule 2
    #If an upstream and downstream names are the same
    #I need to "sum" the condition in case some of the arguments don't exist
    if (sum(DownstreamName %in% UpstreamNames & !UpstreamClash & !DownstreamClash & !is.na(DownstreamName))) Name <- DownstreamName
    
    #Rule 3
    #If a sibling has the same name as the downstream name, then remove that name from the options and allocate the other name.
    #I need to "sum" the condition in case some of the arguments don't exist
    if (sum(DownstreamName %in% SiblingName & !SiblingClash & !DownstreamClash & !is.na(SiblingName))) Name <- Names[which(Names != SiblingName)]
    
    return(Name)
  })
  NamedNetwork$name[NameClashIndices] <- ClashNames  
  
  #Find siblings named the same and resolve them
  SiblingToNodes <- unique(NamedNetwork$TO_NODE[which(duplicated(NamedNetwork$TO_NODE))])
  
  #Loop through the siblings and correct them where possible, and allocating a "TBC" to the name where not.
  #The rules are
  #1/ If the named reaches immediately upstream of the siblings have different names, and one of them matches the 
  # sibling name and the downstream name (if it exists), then rename the other sibling to match the other upstream name
  #2/ If one of the upstream names is not known, rename its sibling as "TBC"
  
  for (SiblingToNode in SiblingToNodes) {
    
    #Find the cases where the names for siblings are the same. Do this by testing to see if there is only one unique name in the sibling name set
    SiblingNames <- NamedNetwork$name[NamedNetwork$TO_NODE == SiblingToNode]
    SingleSiblingName <- length(unique(SiblingNames)) == 1
    
    if(SingleSiblingName & all(!is.na(SiblingNames))) {
      
      #Get the indices of the siblings
      SiblingIndices <- which(NamedNetwork$TO_NODE == SiblingToNode)
      
      SiblingName <- NamedNetwork$name[SiblingIndices[1]]
      
      #Check the downstream name
      DownstreamName <- NamedNetwork$name[NamedNetwork$FROM_NODE == SiblingToNode]
      
      #Get the upstream name of both siblings
      UpStreamNames <- sapply(SiblingIndices, function(SiblingIndex) {
        NamedNetwork$name[which(NamedNetwork$TO_NODE == NamedNetwork$FROM_NODE[SiblingIndex])]
      })
      
      
      #If the upstream name of each sibling is different, and one of them matches the sibling name and the downstream name (if it exists), then
      #change the name of one of the siblings.
      if(length(unique(UpStreamNames)) > 1 & SiblingName %in% UpStreamNames & (SiblingName == DownstreamName | sum(!is.na(DownstreamName)) == 0)) {
        
        #Figure out which sibling to change. The one with the upstream name that doesn't match the duplicated sibling name
        SiblingToChange <- which(!UpStreamNames %in% SiblingName)
        
        #Figure out what the new name should be
        NewSiblingName <- unlist(UpStreamNames[SiblingToChange])
        if(length(NewSiblingName) == 0 | sum(!is.na(NewSiblingName))==0) NewSiblingName <- "TBC"
        
        #Rename it
        NamedNetwork$name[SiblingIndices[SiblingToChange]] <- NewSiblingName
      } #end of renaming if condition
    } #end of renaming test if condition
  } #end of for loop
  
  
  #Check for gaps in naming
  #In some cases a name is missing even though the reaches immediately upstream and downstream have names. In these cases, allocating a name to the unnmaed reach can be done
  #Start by identifying all the reaches that do not have a name
  UnnamedIndices <- which(is.na(NamedNetwork$name))
  #work through them and name them if possible
  for (UnnamedIndex in UnnamedIndices) {
    #Get the Upstream name(s)
    UpstreamName <- NamedNetwork$name[NamedNetwork$TO_NODE == NamedNetwork$FROM_NODE[UnnamedIndex]]
    
    #Get the downstream name
    DownstreamName <- NamedNetwork$name[NamedNetwork$FROM_NODE == NamedNetwork$TO_NODE[UnnamedIndex]]
    
    #rename the reach of interest if one of the upstream reaches has the same name as the downstream reach
    if(sum(DownstreamName %in% UpstreamName & (sum(!is.na(DownstreamName)) != 0)) == 1) NamedNetwork$name[UnnamedIndex] <- DownstreamName
  }
  return(NamedNetwork)
}


#' A function to name an REC network based on available names
#'
#'This function crawls up an REC network and names unnamed reaches. It starts from the outlet of a network and works up to the next branch, naming each unnamed reach with the same name as the immediately downstream name. If the start of a branch is not named it requests a name from user input. It uses the REC V2.4 network. See \href{https://niwa.co.nz/freshwater-and-estuaries/management-tools/river-environment-classification-0}{NIWA REC V2} for details about REC V2.
#'@param RECNetwork An REC V2 network (either dataframe of spatial dataframe), with at least nzsegment, TO_NODE, FROM_NO and hdw_dst attributes
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A list (one for each each independent network/diconnected catchment)) of vectors of reach names, and a side effect of a file called "OutletReaches.csv" listing the nzsegment attributes of the reaches that are considered to be at the bottom of the network.
#'@keywords REC River Environment Classification
#'@export
NetworkNamer <- function(RECNetwork=MyREC){
  
  #Load libraries
  if (!require(svDialogs)) install.packages('svDialogs'); library(svDialogs)     #Enables dialog boxes
  
  #If the nzsegment column is called nzsgmnt then rename it, The NIWA REC2 data has this name.
  names(RECNetwork)[which(names(RECNetwork) == "nzsgmnt")] <- "nzsegment"
  
  #Find the row indices of all the outlet reach's by finding which "to" nodes  don't have a corresponding "from" node
  OutletReachIndices <- which(!RECNetwork$TO_NODE %in% RECNetwork$FROM_NODE)
  #names(OutletReachIndices) <- 1:length(OutletReachIndices)
  
  #Save the outlet reaches to an external file so that we can figure out their names manually
  #write.table(RECNetwork@data$nzsegment[OutletReachIndices],file.path(DataDirectory,"OutletReaches.csv"),sep=",",row.names = FALSE)
  
  #Crawl each network in turn
  ReachNames <- lapply(OutletReachIndices, function(OutletReachIndex) {
    
    #Initialise the list of name details
    RowNumber          <- 1
    CurrentName       <- RECNetwork$name[OutletReachIndex]
    if(CurrentName %in% c("TBC",NA)) {     #get a name from the audience
      RECNetwork$name[OutletReachIndex] <- dlg_input(message = paste("Reach Name for nzsegment",RECNetwork$nzsegment[OutletReachIndex]) , default = "", gui = .GUI)$res
      CurrentName <- RECNetwork$name[OutletReachIndex]
    }  
    
    names             <- data.frame(nzsegment=as.numeric(NA),name=as.character(NA),stringsAsFactors = FALSE)
    names[RowNumber,] <- list(RECNetwork$nzsegment[OutletReachIndex],CurrentName)
    
    CurrentReachIndex  <- OutletReachIndex
    upstream_indices   <- which(RECNetwork$TO_NODE==RECNetwork$FROM_NODE[CurrentReachIndex])
    LeftToDoIndices    <- upstream_indices
    
    while (length(LeftToDoIndices) > 0) {
      
      CurrentReachIndex  <- upstream_indices[1]
      LeftToDoIndices    <- LeftToDoIndices[LeftToDoIndices != CurrentReachIndex]
      RowNumber          <- RowNumber + 1
      #if(RECNetwork$nzsegment[CurrentReachIndex]==15263054) browser()
      IsBranch <- length(upstream_indices) > 1
      #If it is a branch, check that both of the branch reaches have a name
      if(IsBranch){
        if(RECNetwork$name[upstream_indices[1]] %in% c("TBC",NA)) {#get a name from the audience
          #Update the name in the network data
          RECNetwork$name[upstream_indices[1]] <- dlg_input(message = paste("Reach Name for nzsegment",RECNetwork$nzsegment[upstream_indices[1]]) , default = "", gui = .GUI)$res
          #And update the name being collected for the nzsegment to name table
          #CurrentName <- RECNetwork$name[CurrentReachIndex]
        }
        #Check the name of the other branch
        if(RECNetwork$name[upstream_indices[2]] %in% c("TBC",NA)) {     #get a name from the audience
          #Update the network data as well. Note that the nzsegment to name table does not nee to be updated for this reach here, as it will be crawled later on
          RECNetwork$name[upstream_indices[2]] <- dlg_input(message = paste("Reach Name for nzsegment",RECNetwork$nzsegment[upstream_indices[2]]) , default = "", gui = .GUI)$res
        } 
        #Under the condition it is not a branch , check if the name is missing.
        #If the name is not missing, get the name
      } else if(!RECNetwork$name[CurrentReachIndex] %in% c("TBC",NA)){CurrentName <- RECNetwork$name[CurrentReachIndex]} else 
        #If the name is missing, use the name immediately downstream
      { RECNetwork$name[CurrentReachIndex] <- RECNetwork$name[which(RECNetwork$FROM_NODE==RECNetwork$TO_NODE[CurrentReachIndex])]
      #CurrentName <- RECNetwork$name[which(RECNetwork$FROM_NODE==RECNetwork$TO_NODE[CurrentReachIndex])]
      }
      
      names[RowNumber,] <- list(RECNetwork$nzsegment[CurrentReachIndex],RECNetwork$name[CurrentReachIndex])
      
      upstream_indices   <- which(RECNetwork$TO_NODE==RECNetwork$FROM_NODE[CurrentReachIndex])
      
      #Check if there are no more upstream reaches
      if (length(upstream_indices) == 0) {
        upstream_indices <- LeftToDoIndices[1]
      } else {
        LeftToDoIndices <- c(upstream_indices,LeftToDoIndices)
      }
    }
    return(names)
  })
  
  
  return(ReachNames)
}


#' A function to determine where each tributary connects to its parent river
#'
#'This function provides the river network connectivity table used by CASM.
#'CASM understands a network in terms of Tributary Names, Confluence Names and Confluence Locations.
#'Confluence Name is the name of the river that a tributary flows into.
#'Confluence Location is the distance down the confluence from its headwater, that the tributary joins it.
#'
#'@param RECNetwork An REC V2 network (either dataframe of spatial dataframe)
#'@param TributaryLabelList A list of tributary labels. One list for each independent/disconnected catchment. The output of the NetworkNamer() function
#'@param OutletReachNameLookUpTable A data frame of outlet reach nzsegment numbers and real-world names, or "NULL" (the default) if the reach labels are real-world names. 
#'@param HeadwaterDist TRUE/FALSE to indicate whether the tributary locations are with respect the headwater of the confluence, or its outlet. Default is TRUE to match the CASM standard, that is distances are with respct the headwaters. This is needed for backward compatibility.
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A dataframe. One row for each tributary. Columns of nzsegment, tributary name, confluence name, confluence location in kilometres.
#'@keywords REC River Environment Classification CASM
#'@export
TributaryConnectionCreator <- function(RECNetwork = CompleteSpatialNetwork, TributaryLabelList = TribLabelList, OutletReachNameLookUpTable = "NULL",HeadwaterDist=TRUE){
  #browser()
  #Make sure the nzsegment, headw_dist and LENGTHDOWN attributes are correctly named. This is needed because the RECV2 version available from NIWA has altered attribute names (to meet ESRI column naming limitations)
  #If the nzsegment column is called nzsgmnt then rename it, The NIWA REC2 data has this name.
  names(RECNetwork)[which(names(RECNetwork) == "nzsgmnt")] <- "nzsegment"
  names(RECNetwork)[which(names(RECNetwork) == "FROM_NO")] <- "FROM_NODE"
  names(RECNetwork)[which(names(RECNetwork) == "hdw_dst")] <- "headw_dist"
  names(RECNetwork)[which(names(RECNetwork) == "LENGTHD")] <- "LENGTHDOWN"
  
  
  
  #This function works through "independent catchments" which are defined as networks that are not connected, with their own outlet
  #For each independent catchment, figure out where the tributaries connect to their parent tributary.
  CatchmentTribLinkages <- lapply(seq_along(TributaryLabelList), function(CatchmentIndex) {
    
    #Get the tributary labels for all REC reaches within the current independent catchment
    CatchmentTribLabels <- TributaryLabelList[[CatchmentIndex]]
    
    #For backward compatibility rename "name" to "label"
    names(CatchmentTribLabels)[which(names(CatchmentTribLabels) == "name")] <- "label"
    
    #Find all the unique tributary labels
    UniqueTribs <- unique(CatchmentTribLabels$label)
    
    #for each tributary find either, 
    #1/ the minimum LENGTHDOWN attribute  or 
    #2/ the maximum headw_dist attribute 
    #from the REC data of the REC reach immediately downstream of the tributary, 
    #and the tributary label of the REC reach that is immediately downstream. 
    #This builds a matrix of 4 numbers for each tributary, giving:
    #the minimum LENGTHDOWN/maximum headw_dist, 
    #the lowest nzsegment in the tributary, 
    #the highest nzsegment below the tributary and 
    #the label of the tributary below (i.e. the confluence name)
    AllDistances <- sapply(UniqueTribs, function(TribLabel) {
      
      #Get the REC data for the current tributary
      ReachData <- RECNetwork[RECNetwork$nzsegment %in% CatchmentTribLabels$nzsegment[CatchmentTribLabels$label == TribLabel],]
      
      #Find the smallest LENGTHDOWN attribute for the tributary. Note the special case of reach 7260002 (Kaikokopu Stream) which doesn't have a LENGTHDOWN attribute. Ideally we could simply ignore it, but it is a one-reach tributary (it is the outlet of a sub zone without point source or measurment sites within it) so it has to be used
      TribConnectionTotalDistance <- min(ReachData$LENGTHDOWN)
      if (is.na(TribConnectionTotalDistance)) TribConnectionTotalDistance <- 0
      
      #Find which reach is the lowest in the tributary, based on the headw_dist attribute
      LowestReach <- ReachData$nzsegment[which.max(ReachData$headw_dist)]
      
      #Find the reach immediately below the lowest reach
      ReachBelow <- RECNetwork$nzsegment[which(RECNetwork$FROM_NODE == RECNetwork$TO_NODE[RECNetwork$nzsegment == LowestReach])]
      
      #Special case if it is the lowest tributary, as it is effectively the mainstem
      if(length(ReachBelow)==0) {
        ReachBelow <- LowestReach
        TribBelow <- TribLabel }
      else {TribBelow <- CatchmentTribLabels$label[CatchmentTribLabels$nzsegment == ReachBelow]}
      
      #Find the headwater distance of the reach immediately below the tributary confluence
      TributaryLocations <-  RECNetwork$headw_dist[RECNetwork$nzsegment == ReachBelow]
      
      #Special case where the headwater distance is 0 but it is not a headwater reach which means the data are wrong. This has occurred in at least one situation (nzsegment = 15400003)
      #Move down stream and get the headwater distance from the reach below
      if(TributaryLocations == 0 & RECNetwork$Headwater[RECNetwork$nzsegment == ReachBelow] == 0){
        #Move down the network one more step
        ReachBelow <- RECNetwork$nzsegment[which(RECNetwork$FROM_NODE == RECNetwork$TO_NODE[RECNetwork$nzsegment == ReachBelow])]
        #And get the headwater distance from this reach. This assumes that there are not two reaches in a row missing the headwater distance data
        TributaryLocations <-  RECNetwork$headw_dist[RECNetwork$nzsegment == ReachBelow]
      }
      
      
      
      return(c(TribConnectionTotalDistance,LowestReach,ReachBelow,TribBelow,TributaryLocations))
    })
    
    #If the tributary locations are to be provided based on distances downstream, then update them.
    if (!HeadwaterDist){
      #Find the distance of a confluence above the previous confluence. So the LENGTHDOWN of the bottom of the parent tributary needs to be subtracted from the LENGTHDOWN of the current tributary. This is achieved by using the tributary values just created for each catchment.
      AllDistances[TributaryLocations,] <- apply(ConfluenceTotalDistances, 2, function(x) {
        CorrectedDistance <- x[1] - ConfluenceTotalDistances[1,x[4]]
      })
    }
    
    #These confluence distances are added as a row to the rest of the tributary information
    #AllDistances[TributaryLocations,] <- ConfluenceCorrectedDistances
    
    #Lastly, just the useful information is retained, and the labels are formatted to distinguish one catchment from another
    #I now want the tributary label, and the distance along the parent tributary, and the parent tributary label
    TributaryDetails <- data.frame("nzsegment"=AllDistances[2,],"Tributary Name" =colnames(AllDistances), "Confluence Stream" = AllDistances[4,], "Confluence Location (km)" = round(as.numeric(AllDistances[5,])/1000,0), check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  #Combine all the catchment information into a single data frame.
  AllCatchments <- do.call(rbind,CatchmentTribLinkages)
  rownames(AllCatchments) <- NULL
  return(AllCatchments)
}



#' A function to create a CASM node location table given an REC network with additional attributes of CASM tributary labels and CASM tributary distances
#'
#'This function generates a data frame of CASM Node names, CASM tributary, CASM tributary location
#'
#'@param CASMRECNetwork An REC V2 network (either dataframe of spatial dataframe), with at least nzsegment, CASMTrib and CASMTrib_Loc attributes
#'@param CASMNodes A dataframe of node names and REC V2 reach number (i.e. nzsegment attribute) of the nodes for which the CASM table is to be prepared
#'@param OutletReachNameLookUpTable A data frame of outlet reach nzsegment numbers and real-world names, or "NULL" (the default) if the reach labels are real-world names. 
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A dataframe of CASM Node names, CASM tributary, CASM tributary location
#'@keywords REC River Environment Classification CASM
#'@export
CASMNodeTablePreparer <- function(CASMRECNetwork=RECReachNetwork, NetworkLabelList = NetworkLabelList,OutletReachNameLookUpTable = "NULL", TributaryConnectionTable = TributaryConnectionTable, CASMNodes=data.frame(NodeName = c("test","Manawatu at Teachers College"),nzsegment= c(7140020,7239110),stringsAsFactors = FALSE)){
  
  #Make sure the nzsegment attribute is correctly named. This is needed because the RECV2 version available from NIWA has altered attribute names (to meet ESRI column naming limitations)
  #If the nzsegment column is called nzsgmnt then rename it, The NIWA REC2 data has this name.
  names(CASMRECNetwork)[which(names(CASMRECNetwork) == "nzsgmnt")] <- "nzsegment"
  #names(NetworkLabelList)[which(names(NetworkLabelList) == "nzsgmnt")] <- "nzsegment"
  NetworkLabelList <- lapply(NetworkLabelList, function(SingleNetwork){
    names(SingleNetwork)[which(names(SingleNetwork) == "nzsgmnt")] <- "nzsegment"
    SingleNetwork
  })
  names(CASMNodes)[which(names(CASMNodes) == "nzsgmnt")] <- "nzsegment"
  names(TributaryConnectionTable)[which(names(TributaryConnectionTable) == "nzsgmnt")] <- "nzsegment"
  
  #Work through each catchment
  AllTribLocations <- lapply(NetworkLabelList, function(SingleCatchmentNetworkLabels) {
    
    #Get the catchment name from the reach with the smallest DOWNSTREAM attribute
    SingleCatchmentIndices <- which(CASMRECNetwork$nzsegment %in% SingleCatchmentNetworkLabels$nzsegment)
    CatchmentName <- CASMRECNetwork$Label[SingleCatchmentIndices][which.min(CASMRECNetwork$LENGTHDOWN[SingleCatchmentIndices])]
    #browser()
    #But overwrite this if an OutletReachNameLookUpTable has been provided
    #Lookup the catchment name based on the OutletReachNames look up table. Assume that only one reach in the OutletReachName look up table will match the reach numbers in the tributary.
    if (!OutletReachNameLookUpTable == "NULL") CatchmentName <- OutletReachNameLookUpTable$Name[OutletReachNames$nzsegment %in% CatchmentTribLabels$nzsegment]
    
    #Find which CASMNodes are within the current catchment
    CatchmentNodes <- CASMNodes[(CASMNodes$nzsegment %in% SingleCatchmentNetworkLabels$nzsegment),]
    
    #Work through all the Nodes that are in this catchment
    if (nrow(CatchmentNodes) > 0) {
      
      #For each node find the outlet reach, and then get the distance.
      TribLocations <- sapply(seq_along(CatchmentNodes$NodeName), function(NodeIndex){
        
        CASMNode <- CatchmentNodes[NodeIndex,]
        NodeTribName  <- SingleCatchmentNetworkLabels$name[SingleCatchmentNetworkLabels$nzsegment == CASMNode$nzsegment]
        
        
        #Find the headwater distance of the node's reach 
        TributaryLocations <-  CASMRECNetwork$headw_dist[CASMRECNetwork$nzsegment == CASMNode$nzsegment]
        #Extra special case for nzsegment 15273072, which is wrong! Part of the Oreti/Acton confluence REC upgrade.
        if(CASMNode$nzsegment == 15273072)TributaryLocations <- 95000
        
        Result <- c(nzsegment=CASMNode$nzsegment,CASMNodeName=CASMNode$NodeName,TribName = NodeTribName,TribLocn = round(TributaryLocations/1000,0))
        return(Result)
      })
    } else {NULL}
    
    
  })
  
  #Turn the catchment-based list into a data frame
  CASMNodeTable <- data.frame(t(do.call(cbind,AllTribLocations)),stringsAsFactors = FALSE)
  #Convert the numbers into numbers
  CASMNodeTable$nzsegment <- as.numeric(CASMNodeTable$nzsegment)
  CASMNodeTable$TribLocn <- as.numeric(CASMNodeTable$TribLocn)
  CASMNodeTable$TribName <- as.character(CASMNodeTable$TribName)
  CASMNodeTable$CASMNodeName <- as.character(CASMNodeTable$CASMNodeName)
  return(CASMNodeTable)
}

#' A function to create independent CASM node catchments
#'
#'This function generates a spatial file of catchment areas uniquely associated with each reach in a vector
#'
#'@param RECWatersheds An REC V2 watershed sf spatial object that wholly encompasses the area of interest
#'@param RECNetwork The REC V2 network data that wholly encompasses the area of interest and includes at least the nzsegment, TO_NODE and FROM_NODE attributes
#'@param CASMNodes A vector of REC V2 reach number (i.e. nzsegment attribute) of the nodes for which the CASM table is to be prepared
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A Simple Feature spatial object of polygons with associated nzsegment attribute from its related CASMNode
#'@keywords REC River Environment Classification CASM
#'@export
CASMNodeSourceAreaGenerator <- function(RECWatersheds=RECWatersheds2, RECNetwork = RECReachNetwork@data, CASMNodes = c(15308687,15305100,15309794)){
  
  if (!require(tidyr)) install.packages('tidyr'); library(tidyr)  #needed for the drop_na() function
  #Get the individual watershed for each CASM Node
  NodeWatersheds <- RECWatersheds[which(RECWatersheds$nzsegment %in% CASMNodes),]
  #Get the entire catchment for each node
  CompleteCatchments <- lapply(CASMNodes, function(CASMNode){
    
    CatchmentReaches <- CASMNode
    Upstreamreaches <- RECNetwork$nzsegment[which(RECNetwork$TO_NODE == RECNetwork$FROM_NODE[RECNetwork$nzsegment == CASMNode])]
    while(length(Upstreamreaches) > 0){
      CatchmentReaches <- c(CatchmentReaches,Upstreamreaches)
      Upstreamreaches <- RECNetwork$nzsegment[which(RECNetwork$TO_NODE %in% RECNetwork$FROM_NODE[RECNetwork$nzsegment %in% Upstreamreaches])]
    }
    NodeCatchment <- st_union(RECWatersheds[which(RECWatersheds$nzsegment %in% CatchmentReaches),])
    return(NodeCatchment)
  })
  
  #Figure out which part of the catchment is associated with the node, excluding upstream node catchments.
  IntersectedCatchments <- CompleteCatchments %>% do.call(c,.) %>% #Combined the node catchments into one spatial object
    st_intersection() %>%                                          #intersect them so the catchments are not nested. This is sort of like generating watersheds for just the reaches of interest
    st_collection_extract(type = "POLYGON") %>%                    #just select the polygons. The intersection can also generate linestrings which are not wanted.
    st_as_sf() %>%                                                 #Turn back into a "Simple Feature"object
    st_join(.,NodeWatersheds[,"nzsegment"],join=st_contains) %>%   #spatial join to the original Node watershed to get the nzsegment number
    drop_na(nzsegment)
  
  return(IntersectedCatchments)
}


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
#'@param ScenarioLandUseGISFolder The path to 
#'@param Scenarios A vector of scenario names, some of which are associated with spatial data
#'the order matters, as one scenario may affect land use of a previous scenario.
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
                                   Resolution = 250,
                                   ScenarioLandUseGISFolder = "D:\\Projects\\LWP\\SouthlandSCAMP\\Data\\GIS\\ScenarioLandUse",
                                   Scenarios = c()){
  
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
  
  #If Scenarios are provided, then work through them updating the Land Use accordingly
  for (Scenario in Scenarios){
    #Iterate the for loop if there is no land use spatial file associated with the Scenario
    if (!file.exists(file.path(ScenarioLandUseGISFolder,paste0(Scenario[1],'.shp')))) next
    
    #Open the scenario land use file
    ScenarioLandUse <- readOGR(file.path(ScenarioLandUseGISFolder,paste0(Scenario[1],'.shp')), stringsAsFactors = TRUE)
    
    #Need to match the factor levels of the EcoClass attribute to the factor levels of the LeachRateLookUpTable
    ScenarioLandUse@data$EcoClass <- factor(ScenarioLandUse@data$EcoClass, levels = levels(LeachRateLookUpTable$Land.Use))
    
    LanduseRaster <- rasterize(ScenarioLandUse,LanduseRaster,"EcoClass",update=TRUE)
    #Add a raster attribute table which labels the values
    LanduseRaster <- ratify(LanduseRaster)
    rat <- levels(LanduseRaster)[[1]]
    rat$LanduseClass <- levels(LanduseSpatial@data$EcoClass)[rat$ID]
    levels(LanduseRaster) <- rat
  }

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


#' A function to load the physiographic data and reproject it to WGS84.
#'
#'This function generates a raster object of the physiographic data
#'
#'@param SourceFile A ESRI polygon shapefile of the physiographic data
#'@param Domain A spatial object of the domain within which to limit the raster
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster object of physiography in WGS84
#'@keywords physiography, southland
#'@export
CreatePhysiography <- function(SourceFile = PhysiographicDataFile,Domain = CompleteDomain){
  
  #Load the physiography spatial polygon data
  Physiography <- readOGR(SourceFile, stringsAsFactors = TRUE)
  
  #Explicitly set the projection to NZTM
  #Physiography <- spTransform(Physiography,CRS("+init=epsg:2193") )
  Physiography <- spTransform(Physiography,CRSobj=CRS(SRS_string = "EPSG:2193") )
  
  #Convert to a raster with an attribute table, and  for mapping later on
  #Convert to raster, note the creation of a base raster in WGS84, which all subsequent raster's align to
  #RasterBase <- raster(resolution = 250, ext = extent(Domain), crs = proj4string(Domain) )
  RasterBase <- raster(resolution = 250, ext = extent(Domain), crs = wkt(Domain) )
  PhysiographyRaster <- rasterize(Physiography,RasterBase,"Physiograp")
  #Crop to the Complete domain, and then mask to the same
  PhysiographyRaster <- crop(PhysiographyRaster,extent(Domain))
  PhysiographyRaster <- mask(PhysiographyRaster, Domain)
  
  #Note, for the next line I have had issues with repeated errors of "Error in x$.self$finalize() : attempt to apply non-function"
  #I don't know the cause. It just goes away when I run the command again. Maybe it is doing stuff on the raster before the raster has been created.
  #A possible solution is to transform the vector to WGS84 straight up, rather than transform the raster. I might need to create a transformed version of complete domain as well.
  
  # PhysiographyRasterWGS84 <- projectRaster(PhysiographyRaster,crs = CRS("+init=epsg:4326"),method = "ngb")
  PhysiographyRasterWGS84 <- projectRaster(PhysiographyRaster,crs = CRS(SRS_string = "EPSG:4326"),method = "ngb")
  PhysiographyRasterWGS84 <- ratify(PhysiographyRasterWGS84)
  PhysiographyRAT <- levels(PhysiographyRasterWGS84)[[1]]
  PhysiographyRAT$Physiography <- levels(Physiography$Physiograp)
  levels(PhysiographyRasterWGS84)  <- PhysiographyRAT
  return(PhysiographyRasterWGS84)
}


#' A function to determine physiographic-based attenuation estimates for diffuse source areas.
#'
#'This function calculates the area weighted physiographic-based contaminant attenuation scale factor for diffuse source area-landuse combinations.
#'
#'@param PhysiographicFile An ESRI polygon shapefile of the physiographic data.
#'@param Domain A spatial object of the domain within which to limit the raster
#'@param AttenuationLookupTable An Excel spreadsheet with two tables that relate the physiographic variant classes to attenuation rates
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A dataframe of three columns, one with Diffuse soure area identifier, the second and third columns with the area weighted physiographic-based attenuation estimates for TN and TP.
#'@keywords physiography, southland, contaminant, leach
#'@export
CreatePhysiographyAttenuationEstimates <- function(PhysiographicFile = PhysiographicDataFile,
                                                   Domain = CompleteDomain, 
                                                   AttenuationLookupTable = PhysiographicAttenuationLookupTableFile){
  
  if (!require(openxlsx)) install.packages('openxlsx'); library(openxlsx)
  #Load the physiography spatial polygon data
  Physiography <- readOGR(PhysiographicFile,stringsAsFactors = FALSE)
  
  #Explicitly set the projection to NZTM
  Physiography <- spTransform(Physiography,CRS("+init=epsg:2193") )
  
  #Load the lookup table and extricate the TN and TP attenuation rate estimates
  TNRates <- read.xlsx(AttenuationLookupTable,startRow = 2,rows=c(2:19),cols=c(1:9))
  TPRates <- read.xlsx(AttenuationLookupTable,startRow = 23,rows=c(23:40),cols=c(1:8))
  AttenLookupTable <- merge(TNRates[,c("Variant","Relative.N.attenuation.(higher.=.greater.attenuation)")],TPRates[,c("Variant","Relative.P.attenuation.(higher.=.greater.attenuation)")])
  names(AttenLookupTable) <- c("Variant","TNAttenuation","TPAttenuation")
  
  #Use the lookup table to create a Physiographic-based contaminant leach attenuation value
  Physiography$TNAttenuation <- AttenLookupTable$TNAttenuation[match(Physiography$Variant,AttenLookupTable$Variant)]
  Physiography$TPAttenuation <- AttenLookupTable$TPAttenuation[match(Physiography$Variant,AttenLookupTable$Variant)]
  
  #Convert each to a raster with an attribute table, and  for mapping later on
  #Convert to raster, note the creation of a base raster in WGS84, which all subsequent raster's align to
  RasterBase <- raster(resolution = 250, ext = extent(Domain), crs = proj4string(Domain) )
  TNPhysAttenRaster <- rasterize(Physiography,RasterBase,"TNAttenuation")
  TPPhysAttenRaster <- rasterize(Physiography,RasterBase,"TPAttenuation")
  
  #Combine them as a raster stack
  PhysAtten <- stack(TNPhysAttenRaster,TPPhysAttenRaster)
  
  #Crop to the Complete domain, and then mask to the same
  PhysAtten <- crop(PhysAtten,extent(Domain))
  PhysAtten <- mask(PhysAtten, Domain)
  
  return(PhysAtten)
}



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
#'Ross Monaghan provided expert guidance on loss rates for types not included in McDowell et al. (2020)
#'@param ReferenceRaster A reference raster to align to. Ideally use the loss-rate-raster
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster object of 
#'@keywords Water Quality, CASM, SCAMP, leach
#'@export
LossRaterReductionRasterCreator <- function(LandTypes=LanduseShapeFile,
                                            MitigationLookUpTable=NA,
                                            ReferenceRaster = LossRateRaster){
  
  if (!require(raster)) install.packages("raster"); library(raster)                #used for spatial processing
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)                #used for spatial processing
  if (!require(rasterVis)) install.packages("rasterVis"); library(rasterVis)                #used for plotting discrete rasters
  #browser()

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
                              MitigationDataFile = "D:\\Projects\\LWP\\SouthlandRegionalForumModelling\\Data\\OurLandAndWaterMitigationLoadsV2.csv",
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
    #ColumnsOfInterest <- colnames(MitigationData)[which(grepl(paste0(Nutrient,"_loss"),colnames(MitigationLookUpTable)))]
    #Select the mitigation scenario of choice.
    CurrentColumn <-  which(colnames(MitigationData) == paste0("Current",Nutrient,"_loss"))
    ScenarioColumn <- which(colnames(MitigationData) == paste0(MitigationOfInterest,Nutrient,"_loss"))
    MitigationData$ScenarioMitigationFraction <- (MitigationData[,CurrentColumn] - MitigationData[,ScenarioColumn]) / MitigationData[,CurrentColumn]
    MitigationLookUpTable <- MitigationData[,c("Typology","ScenarioMitigationFraction")]  
    
    #create the loss rate reduction raster
    LossRateReductionRaster <- LossRaterReductionRasterCreator(LandTypes = AllTypes,
                                                               MitigationLookUpTable = MitigationLookUpTable,
                                                               ReferenceRaster = LeachRates[[1]])
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

#' A function to plot the river segment distribution of 1)percent upstream exotic grassland
#' 2) upstream catchment area and 3) Water plan class
#' 
#'  The distribution is for river segments of strahler order 3 and above) and uses
#'  10 bins. The percentage of location points in each bin is also shown
#'
#'[AttributeDistributionPlot()] enables investigation of how representative the SCAMP 
#'network is to a complete river network 
#'@param RECReachNetwork The complete RECV2 network
#'@parm CompleteDomain A spatial polygon that describes the modelling domain boundary
#'@param MajorCatchments A spatial polygon file that describes the major catchments
#'@param SouthlandREC2Utility A data frame of additional REC attributes including
#' "pctExoticPasture"
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return Three data frames
#'@keywords REC River Environment Classification
#'@export
AttributeDistributionPlot <- function(RECReachNetwork = NA,
                                      CompleteDomain = NA,
                                      MajorCatchments = NA,
                                      SouthlandREC2Utility = NA){
  
  #Get all the REC reaches in strahler order 3 and above
  RECOrder3andAbove <- RECReachNetwork[RECReachNetwork@data$StreamOrde >= 3,]
  
  #Limit to sites within the overall domain
  RECOrder3andAbove <- RECOrder3andAbove[!is.na(RECOrder3andAbove %over% CompleteDomain),]
  
  #Add the pctExoticPasture attribute from the SouthlandREC2Utility data
  RECOrder3andAbove@data <- join(RECOrder3andAbove@data,SouthlandREC2Utility[,c("pctExoticPasture","nzsegment")],type="left")
  
  #Add the catchment name as an attribute
  RECOrder3andAbove$Catchments <- as.character(unlist(RECOrder3andAbove %over% MajorCatchments))
  
  #There are a few in New River Estuary and Invercargill area that are outside the Invercargill FMU boundary, but only because the boundary doesn't include the estuary
  RECOrder3andAbove$Catchments[which(is.na(RECOrder3andAbove$Catchments))] <- "Oreti & Invercargill Catchments"
  
  #These are all the "Network" sites, so I'll call them that in an attribute called "What
  RECOrder3andAbove@data$What <- "Network"
  
  #And just extract the dataframe for the graphing
  GraphDataFortheCompleteNetwork <- RECOrder3andAbove@data[,c("nzsegment","CUM_AREA","WaterPlan","Catchments","pctExoticPasture","What")]
  
  #Add an attribute which has the total number of segments in the class
  GraphDataFortheCompleteNetwork$TotalNo <- nrow(GraphDataFortheCompleteNetwork)
  
  #Add an attribute which has the total number of segments in the class in each catchment
  TotalReachesInEachCatchment <- table(GraphDataFortheCompleteNetwork$Catchments)
  GraphDataFortheCompleteNetwork$TotalInEachCatchment <- as.vector(TotalReachesInEachCatchment[GraphDataFortheCompleteNetwork$Catchments])
  
  #Just in case, remove any rows that aren't complete (I think there is one missing the pstUpstream pasture attribute
  GraphDataFortheCompleteNetwork <- GraphDataFortheCompleteNetwork[complete.cases(GraphDataFortheCompleteNetwork),]
  
  #Create a subset of these that are associated with CASM locations
  GraphDataForLocations <- GraphDataFortheCompleteNetwork[which(GraphDataFortheCompleteNetwork$nzsegment %in% AllPoints),]
  
  #And re-label the "What" attribute to "Locations"
  GraphDataForLocations$What <- "Locations"
  
  #Update the TotalNo to match the number of locations, and the total number in each catchment
  GraphDataForLocations$TotalNo <- nrow(GraphDataForLocations)
  TotalLocationsInEachCatchment <- table(GraphDataForLocations$Catchments)
  GraphDataForLocations$TotalInEachCatchment <- as.vector(TotalLocationsInEachCatchment[GraphDataForLocations$Catchments])
  
  #Rbind the full network and the locations
  GraphData <- rbind(GraphDataFortheCompleteNetwork,GraphDataForLocations)
  
  #Convert the catchment area to km2
  GraphData$CUM_AREA <- round(GraphData$CUM_AREA / 1000000,1)
  
  #Rename the attributes of interest to make the automatic graph labelling a bit more intuitive
  GraphData <- rename(GraphData, c("Catchment Area (km2)"="CUM_AREA",
                                   "Percentage of catchment in high producing pasture"="pctExoticPasture",
                                   "Water Plan Classes"="WaterPlan"))
  
  #Initialise an empty plot list
  PlotList <- list()
  
  AttributesOfInterest <- c("Catchment Area (km2)","Percentage of catchment in high producing pasture")
  for (AttributeToBePlotted in AttributesOfInterest){
    
    FullDomainPlot <- ggplot(GraphData, aes(x=GraphData[,AttributeToBePlotted],y=stat(count) * 100,fill=What,weight=1/TotalNo)) + geom_histogram(bins=10,colour="grey",position="dodge") + labs(fill="", title = "All catchments",x = AttributeToBePlotted, y = "Percentage of total")
    
    CatchmentPlot <- ggplot(GraphData, aes(x=GraphData[,AttributeToBePlotted],y=stat(count) * 100,fill=What,weight=1/TotalInEachCatchment))+ geom_histogram(bins=10,colour="grey",position="dodge") + labs(fill="", x = AttributeToBePlotted, y = "Percentage of total") + facet_wrap(. ~Catchments,ncol=2)
    
    PlotList <- c(PlotList,list(FullDomainPlot),list(CatchmentPlot))
  }
  
  WaterPlanFullDomainPlot <- ggplot(GraphData, aes(x=GraphData[,"Water Plan Classes"],y=stat(count) * 100,fill=What,weight=1/TotalNo)) + geom_bar(colour="grey",position="dodge") + labs(fill="",title = "All catchments", x = "Water Plan Classes", y = "Percentage of total") 
  
  WaterPlanByCatchmentPlot <- ggplot(GraphData, aes(x=GraphData[,"Water Plan Classes"],y=stat(count) * 100,fill=What,weight=1/TotalInEachCatchment))+ geom_bar(colour="grey",position="dodge") + labs(fill="", x = "Water Plan Classes", y = "Percentage of total") + theme(axis.text.x = element_text(angle = 45, hjust=1))+ facet_wrap(. ~Catchments,ncol=2)
  
  PlotList <- c(PlotList,list(WaterPlanFullDomainPlot),list(WaterPlanByCatchmentPlot))
  
  library(gridExtra)
  
  # Three pages of plots in one PDF file
  {
    pdf("CASM Node location representativeness.pdf", 8.27, 11.69)
    for (i in seq(1, length(PlotList), 2)) {
      grid.arrange(PlotList[[i]],
                   PlotList[[i+1]],
                   nrow=3,
                   layout_matrix = rbind(1,2,2,2))
      
    }
    dev.off()
  }
}

#' A function to determine new wetland attributes
#' 
#'  New wetlands impact on water quality through attenuating nutrient loads. The SCAMP
#'  model can account for that attenuation if the are upstream of the wetland is know
#'  the land use make up of that are is known.
#'
#'[NewWetlandProperties()] works out the attributes of land areas designated as new wetlands 
#'@param RECReachNetwork The complete RECV2 network
#'@param RECWatersheds Either the RECV2 watershed sf spatial data, or the file name of the RECV2 spatial data
#'@param NewWetlandSpatial Either the wetland sf spatial data, or it's file name
#'@param LandUseRaster A raster object of the land use
#'@param LandUseLUT A dataframe of Land Use values (ID) and land ue names (category)
#'@param RainRasterFile File name and path of a raster file (geoTiff format) of mean annual rainfall
#'@param SubCatchmentSpatial an sf spatial object of the Sub Catchment areas
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return Data frame of Wetland Information
#'@keywords REC River Environment Classification Wetlands
#'@export
NewWetlandProperties <- function(RECReachNetwork = "D:\\Projects\\LWP\\SouthlandRegionalForumModelling/Data/GIS/ES_REC2V5_Riverlines/ES_REC2V5_Riverlines.shp",
                                 RECWatersheds = "D:\\Projects\\LWP\\SouthlandRegionalForumModelling/Data/GIS/ES_REC2_Watersheds/ES_REC2_Watersheds.shp",
                                 NewWetlandSpatial = file.path("D:\\Projects\\LWP\\SouthlandSCAMP\\Data\\GIS\\ScenarioLandUse",
                                                               "Wetland1WithExtraAttributes.shp"),
                                 LandUseRaster = file.path("D:\\Projects\\LWP\\SouthlandRegionalForumModelling/Data/GIS","PredictorRasterV3 250 x 250.tif"),
                                 LandUseLUT = NULL,
                                 RainRasterFile = "D:\\Projects\\LWP\\SouthlandSCAMP\\Data\\GIS\\Rainfall19812010.tif",
                                 SubCatchmentSpatial = file.path("D:\\Projects\\LWP\\SouthlandRegionalForumModelling/Data/GIS","CASMDiffuseLoadSourceAreas","CASMDiffuseLoadSourceAreas.shp")){
  

  if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
  if (!require(tibble)) install.packages("tibble"); library(tibble)
  #Load the Wetland spatial data
  NewWetlandSpatial <- st_read(NewWetlandSpatial)

  #Test if the RECReachNetwork is a spatial data file object, if it is, convert to an 
  #sf object, otherwise assume it is a file name and try to load it
  if(class(RECReachNetwork) == "SpatialLinesDataFrame") {
    RECRivers_SF <- st_as_sf(RECReachNetwork)
  } else {
    RECRivers_SF <- st_read(RECReachNetwork)
  }
  #Re-project to NZTM
  RECRivers_SF <- st_transform(RECRivers_SF, st_crs(NewWetlandSpatial))
  
  #Test if the RECWatersheds is an sf spatial object, if it is not assume it is a file name and try to load it
  if(class(RECWatersheds)[1] != "sf") {
      RECWatersheds <- st_read(RECWatersheds)
      RECWatersheds <- st_transform(RECWatersheds, st_crs(NewWetlandSpatial))
  }
  RECWatersheds <- st_make_valid(RECWatersheds)

  #Test if the SubCatchmentSpatial is an sf spatial object, if it is not assume it is a file name and try to load it
  if(class(SubCatchmentSpatial)[1] != "sf") {
    SubCatchmentSpatial <- st_read(SubCatchmentSpatial)
  }
  SubCatchmentSpatial <- st_transform(SubCatchmentSpatial, st_crs(NewWetlandSpatial))
  SubCatchmentSpatial <- st_make_valid(SubCatchmentSpatial)
  
  #Test if the LandUseRaster is a raster object, if it is not, assume it is a file and try to load it
  if(class(LandUseRaster)[1] != "raster") {
    LandUseRaster <- stack(LandUseRaster)[[1]]
    #Major headache with understanding changes to projections following upgrade to gdal3 and PROJ6
    crs(LandUseRaster) <- st_crs(NewWetlandSpatial)$wkt
  }

  #Load the rain Raster data
  RainRaster <- raster(RainRasterFile)
  #Major headache with understanding changes to projections following upgrade to gdal3 and PROJ6
  crs(RainRaster) <- st_crs(NewWetlandSpatial)$wkt
  
  #The wetland spatial file is a whole lot of pieces of wetlands, where each piece 
  #is a unique wetland-REC watershed combination. Pieces smaller than 500 m2 have been removed.
  #Each piece has a SCAMP_nzsegment identifier to tell which SCAMP sub catchment it is from.
  #Each piece also has an nzsegment number, to say which REC watershed it is in.
  #Each piece has a Number which uniquely identifies each complete wetland, but these
  #can cross SCAMP boundaries, so each piece also has a combination wetland_number-and-SCAMP_nzsegment
  #Identifier
  
  #Initialise a dataframe that will hold all the Wetland attribute information
  WetlandInfo <- data.frame(WetlandUniqueNumber = unique(NewWetlandSpatial$WtLdSCID),
                            SCAMP_nzsegment = NA,
                            UpstreamPctOfSubCatch = NA,
                            Area_ha = NA,
                            Annual_precip = NA,
                            Attenuation = NA,
                            HasWetlandOrSubCatchmentUpstream = NA,
                            DownStreamWetland = NA,
                            Comments = NA)
  #,
  #                          UpStreamSubCatcments = list())
  WetlandInfo$SCAMP_nzsegment <-  NewWetlandSpatial$SCAMP_nzs[match(WetlandInfo$WetlandUniqueNumber,NewWetlandSpatial$WtLdSCID)]
  
  #work through each sub catchment
  for (SCAMP_SubCatchment in unique(NewWetlandSpatial$SCAMP_nzs)){
  #for (SCAMP_SubCatchment in c(15315638)){ #for testing  
    SubCatchment <- SubCatchmentSpatial[which(SubCatchmentSpatial$SCAMPnzseg == SCAMP_SubCatchment),]
    SubCatchmentArea <- st_area(SubCatchment)
    #Select just the wetland areas within the current SCAMP sub catchment
    WetlandsOfInterest <- NewWetlandSpatial %>% filter(SCAMP_nzs == SCAMP_SubCatchment)
    
    #Subset the REC network to the current subcatchment, and remove any resulting duplicates
    st_agr(SubCatchment) = "constant"
    st_agr(RECRivers_SF) = "constant"
    SubCatchmentREC <- st_intersection(RECRivers_SF,SubCatchment)
    SubCatchmentREC <- SubCatchmentREC[!duplicated(SubCatchmentREC$nzsegment),]
    
    #Work through each of the sub-catchment's wetlands
    for (WetlandUniqueID in unique(WetlandsOfInterest$WtLdSCID)) {
      #browser()
      #Select just the wetland areas within the current wetland 
      SingleWetland <- WetlandsOfInterest %>% filter(WtLdSCID == WetlandUniqueID)
      #Get the total area of the pieces of wetland
      WetlandArea <- sum(SingleWetland$Area_m2)
      WetlandInfo$Area_ha[WetlandInfo$WetlandUniqueNumber==WetlandUniqueID] <- round(WetlandArea / 10000,1)
      
      #Figure out the upstream area, within the sub catchment
      #By getting the upstream area of all the REC watershed components of the wetland
      WetlandRECWatershedUpstreamAreas <- lapply(unique(SingleWetland$nzsegment), function(REC_watershed) {
        #browser()
        #Initialise a vector of nzsegment values for the reaches in the catchment, starting with the wetlands nzsegment
        CatchmentReaches <- REC_watershed
        #Get upstream area
        Upstreamreaches <- RECRivers_SF$nzsegment[which(RECRivers_SF$TO_NODE == RECRivers_SF$FROM_NODE[RECRivers_SF$nzsegment == REC_watershed])]
        while(length(Upstreamreaches) > 0){
          #browser()
          #Check if there are any wetlands upstream, if there are, set the global variable. This is bad form, so I should fix it at some later date....
          #The check is for wetlands different to the current wetland. This is a bit tricky as I break wetlands into REC watershed pieces.
          WetlandRECsToCheckFor <- NewWetlandSpatial$nzsegment[NewWetlandSpatial$WtLdSCID != WetlandUniqueID]
          if (any(Upstreamreaches %in% WetlandRECsToCheckFor)) {
            #browser()
            WetlandInfo$HasWetlandOrSubCatchmentUpstream[WetlandInfo$WetlandUniqueNumber==WetlandUniqueID] <<- TRUE }
          CatchmentReaches <- c(CatchmentReaches,Upstreamreaches)
          Upstreamreaches <- RECRivers_SF$nzsegment[which(RECRivers_SF$TO_NODE %in% RECRivers_SF$FROM_NODE[RECRivers_SF$nzsegment %in% Upstreamreaches])]
        }
        UpstreamCatchment <- RECWatersheds[which(RECWatersheds$nzsegment %in% CatchmentReaches),]
        
        return(UpstreamCatchment)
      })
      
      #Find any down-stream wetlands within the same sub-catchment
      #Work down valley and check for a match to the sub-catchment's wetland's nzsegment number.
      #Stop at the first wetland, and get it's number

      for (REC_segment in unique(SingleWetland$nzsegment)) {
        #browser()
        #if (REC_segment == 15318632) browser()

        #Initialise a vector of nzsegment values for the reaches in the catchment, starting with the wetlands nzsegment
        #It is possible for two separate wetlands to be associated with the same reach, so start the check from the wetlands reach
        #But check if the wetland has already been considered to be downstream, and so go further downstream.
        Downstreamreach <- REC_segment

        #Keep working downstream within the subcatchment until a downstream wetland is found, or there are no reaches left
        while(length(Downstreamreach) > 0){
          #Check if there is a wetland down stream, if there is, set the attribute
          #The check is for wetlands different to the current wetland, and not already associated (because they're on the same REC reach). This is a bit tricky as I break wetlands into REC watershed pieces.
          UpstreamWetlands <- WetlandInfo$WetlandUniqueNumber[WetlandInfo$DownStreamWetland == WetlandUniqueID]
          WetlandRECsToCheckFor <- WetlandsOfInterest[(WetlandsOfInterest$WtLdSCID != WetlandUniqueID) & !(WetlandsOfInterest$WtLdSCID %in% UpstreamWetlands),]
          if (any(Downstreamreach %in% WetlandRECsToCheckFor$nzsegment)) {
            #browser()
            WetlandInfo$DownStreamWetland[WetlandInfo$WetlandUniqueNumber == WetlandUniqueID] <-
              WetlandRECsToCheckFor$WtLdSCID[match(Downstreamreach,WetlandRECsToCheckFor$nzsegment)]
            break
          }
          #Get the next downstream reach, but only within the current sub-catchment
          Downstreamreach <- SubCatchmentREC$nzsegment[which(SubCatchmentREC$FROM_NODE == SubCatchmentREC$TO_NODE[SubCatchmentREC$nzsegment == Downstreamreach])]
        }
      }
      
      WetlandUpstreamSubCatchment <- do.call(rbind,WetlandRECWatershedUpstreamAreas)
      WetlandUpstreamSubCatchment <- WetlandUpstreamSubCatchment[!duplicated(WetlandUpstreamSubCatchment$nzsegment),]
      
      #Get the mean annual rainfall of the wetlands catchment
      WetlandCatchmnetRainfall <- mean(raster::extract(RainRaster,as(st_union(WetlandUpstreamSubCatchment),'Spatial'))[[1]],na.rm=TRUE)
      WetlandInfo$Annual_precip[WetlandInfo$WetlandUniqueNumber==WetlandUniqueID] <- round(WetlandCatchmnetRainfall / 1000,2)
      
      #Clip to SCAMP_subcatchment
      st_agr(SubCatchment) = "constant"
      st_agr(WetlandUpstreamSubCatchment) = "constant"
      SubCatchmentUpStreamCatchment <- st_intersection(WetlandUpstreamSubCatchment,SubCatchment)
      
      #Need to figure out if the wetland upstream area includes upstream sub catchments.
      #If it does, then the ID of the immediately upstream subcatchments are needed
      #Turns out upstream/downstream REC watersheds share an REC segment at their 
      #intersection.This means that you can intersect a subcatchments nzsegments 
      #with the outlet nzsegments of all subcatchments, to find the subcatchments of interest,
      #though this includes the original subcatchment
      InterSubCatchmentnzsegments <- intersect(SubCatchmentUpStreamCatchment$nzsegment,SubCatchmentSpatial$nzsegment)
      #Get the unique SCAMP_nzsegment attributes of those immediately upstream subcatchments
      SubCatchmentsInWetlandUpstreamArea <- unique(SubCatchmentSpatial$SCAMPnzseg[SubCatchmentSpatial$nzsegment %in% InterSubCatchmentnzsegments])
      #Get rid of the original subcatchment SCAMP_nzsegment
      SubCatchmentsInWetlandUpstreamArea <- setdiff(SubCatchmentsInWetlandUpstreamArea,SCAMP_SubCatchment)
      if(length(SubCatchmentsInWetlandUpstreamArea)>0){
        #browser()
      WetlandInfo$Comments[WetlandInfo$WetlandUniqueNumber==WetlandUniqueID] <- paste0("Upstream area extends to subcatchments: ",paste(SubCatchmentsInWetlandUpstreamArea,collapse=";"))
      WetlandInfo$HasWetlandOrSubCatchmentUpstream[WetlandInfo$WetlandUniqueNumber==WetlandUniqueID] <- TRUE
      } 
      
      #Get the area of the wetland's upstream area
      SubCatchmentUpStreamCatchmentArea <- st_area(st_union(SubCatchmentUpStreamCatchment))
      WetlandInfo$UpstreamPctOfSubCatch[WetlandInfo$WetlandUniqueNumber==WetlandUniqueID] <- round(SubCatchmentUpStreamCatchmentArea / sum(SubCatchmentArea) * 100,1)
      
      #Get their land use breakdown
      #See https://gis.stackexchange.com/questions/297852/calculating-statistics-per-area-for-categorical-raster-using-r

      #Get the raster cells within the upstream area
      LandUseValues <- raster::extract(LandUseRaster,as(st_union(SubCatchmentUpStreamCatchment),'Spatial'))[[1]]
      #Turn them into factors with levels based on the Land use raster if it has them,
      #or from the LandUseLUT parameter. This was required because the raster RAT
      #appears not to get passed through to a function. Need to double check this.
      if (is.null(LandUseLUT)) LandUseLUT <- levels(LandUseRaster)[[1]][-1,]
      
     

      LandUseFactors <- factor(LandUseValues,levels=LandUseLUT$ID,labels = LandUseLUT$category)
      #get the percentage of each level
      PctLandUse <- round(prop.table(table(LandUseFactors)) * 100,1)
      #Add this information to the Wetland Information data frame.
      WetlandInfo[WetlandInfo$WetlandUniqueNumber==WetlandUniqueID,LandUseLUT$category] <- PctLandUse

      
    }
  }

#Set all non-True values of the Upstream wetland attribute to False
  WetlandInfo$HasWetlandOrSubCatchmentUpstream[is.na(WetlandInfo$HasWetlandOrSubCatchmentUpstream)]  <- FALSE
  
#Rename and re-order the columns to match the Tim Cox requested layout
names(WetlandInfo)[c(1:8)] <- c("Wetland Name","Catchment Name","% of Catchment Area",
                                  "Wetland Size (ha)","Annual Precipitation (m/yr)","Attenuation Rate Constant (m/yr)","Upstream Wetland or Subcatchment (TRUE/FALSE)","Downstream Wetland")
WetlandInfo <- WetlandInfo[,c(1:3,15,13,11,17,14,18,16,12,10,4:9)]
return(WetlandInfo)
}
