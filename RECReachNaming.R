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
      #browser()
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
CASMNodeTablePreparer <- function(CASMRECNetwork=RECReachNetwork, NetworkLabelList = NetworkLabelList,OutletReachNameLookUpTable = "NULL", TributaryConnectionTable = TributaryConnectionTable, CASMNodes=data.frame(NodeName = c("test","Manawatu at Teachers College"),nzsegment= c(7140020,7239110))){
  
  #Make sure the nzsegment attribute is correctly named. This is needed because the RECV2 version available from NIWA has altered attribute names (to meet ESRI column naming limitations)
  #If the nzsegment column is called nzsgmnt then rename it, The NIWA REC2 data has this name.
  names(CASMRECNetwork)[which(names(CASMRECNetwork) == "nzsgmnt")] <- "nzsegment"
  names(NetworkLabelList)[which(names(NetworkLabelList) == "nzsgmnt")] <- "nzsegment"
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
        
        Result <- c(nzsegment=CASMNode$nzsegment,CASMNodeName=CASMNode$NodeName,TribName = NodeTribName,TribLocn = round(TributaryLocations/1000,0))
        return(Result)
      })
    } else {NULL}
    
    
  })
  
  #Turn the catchment-based list into a data frame
  CASMNodeTable <- data.frame(t(do.call(cbind,AllTribLocations)))
  #Convert the numbers into numbers
  CASMNodeTable$nzsegment <- as.numeric(levels(CASMNodeTable$nzsegment))[CASMNodeTable$nzsegment]
  CASMNodeTable$TribLocn <- as.numeric(levels(CASMNodeTable$TribLocn))[CASMNodeTable$TribLocn]
  CASMNodeTable$TribName <- as.character(CASMNodeTable$TribName)
  CASMNodeTable$CASMNodeName <- as.character(CASMNodeTable$CASMNodeName)
  return(CASMNodeTable)
}

#' A function to create independent CASM node catchments
#'
#'This function generates a spatial file of catchment areas uniquely associated with each reach in a vector
#'
#'@param RECWatersheds An REC V2 watershed spatial object that wholly encompases the area of interest
#'@param RECNetwork The REC V2 network data that wholly encompases the area of interest and includes at least the nzsegment, TO_NODE and FROM_NODE attributes
#'@param CASMNodes A dataframe of node names and REC V2 reach number (i.e. nzsegment attribute) of the nodes for which the CASM table is to be prepared
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A Simple Feature spatial object of polygons with associated nzsegment attribute from its related CASMNode
#'@keywords REC River Environment Classification CASM
#'@export
CASMNodeSourceAreaGenerator <- function(RECWatersheds=RECWatersheds2, RECNetwork = RECReachNetwork@data, CASMNodes = c(15308687,15305100,15309794)){
  
  if (!require(tidyr)) install.packages('tidyr'); library(tidyr)  #needed for the drop_na() function
  #Get the individual watershed for each CASM Node
  NodeWatersheds <- st_as_sf(RECWatersheds[which(RECWatersheds$nzsegment %in% CASMNodes),])
  #Get the entire catchment for each node
  CompleteCatchments <- lapply(CASMNodes, function(CASMNode){

    CatchmentReaches <- CASMNode
    Upstreamreaches <- RECNetwork$nzsegment[which(RECNetwork$TO_NODE == RECNetwork$FROM_NODE[RECNetwork$nzsegment == CASMNode])]
    while(length(Upstreamreaches) > 0){
      CatchmentReaches <- c(CatchmentReaches,Upstreamreaches)
      Upstreamreaches <- RECNetwork$nzsegment[which(RECNetwork$TO_NODE %in% RECNetwork$FROM_NODE[RECNetwork$nzsegment %in% Upstreamreaches])]
    }
    NodeCatchment <- st_union(st_as_sf(RECWatersheds[which(RECWatersheds$nzsegment %in% CatchmentReaches),]))
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


#' A function to combine spatial data sources of land use, and diffuse source areas.
#'
#'This function generates a raster object of Diffuse source areas subclassified by landuse
#'
#'@param LanduseData A raster of landuse data
#'@param DiffuseSourceAreas A spatial object of the areas to be intersected with the land use
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A raster object of leach rates
#'@keywords Water Quality, CASM, leach
#'@export
LeachRateRasterCreator <- function(LanduseData=LanduseRaster,
                                   DiffuseSourceAreas=DiffuseSourceAreaSpatialObject){
  
  if (!require(raster)) install.packages("raster"); library(raster)                #used for spatial processing
  if (!require(rgdal)) install.packages("rgdal"); library(rgdal)                #used for spatial processing
  if (!require(rasterVis)) install.packages("rasterVis"); library(rasterVis)                #used for plotting discrete rasters


  #rasterize the diffuse source area spatial data, aligning to the Landuse data
  DiffuseSourceRaster <- rasterize(DiffuseSourceAreas, LanduseData, "nzsegment")
  
  #Calculate a new raster by adding the nzsegment value to a tenth of the landuse class.
  #This leads to 88888888888.1, 888888888.2 etc

  #Create a raster brick with all the parameters needed to determine the leach rate from the look up table
  PredictorRasters <- brick(LanduseRaster,SoilDrainageRaster,SlopeClassRaster,PrecipIrrigRaster)
  names(PredictorRasters) <- c("Landuse","SoilDrainage","SlopeClass","PrecipIrrig")
  #Mask the raster brick to just the Southland FMU areas as given in the PrecipIrrig polygon layer.
  PredictorRasters <- rasterize(x=PrecipIrrigSpatial,y=PredictorRasters,mask=TRUE)
  
  #Figure out the leachrates for each of the leach rate types
  LeachTypes <- c("N.loss.(mean)","N.loss.(median)","P.loss.(mean)","P.loss.(median)")
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
  Physiography <- readOGR(SourceFile)
  
  #Explicitly set the projection to NZTM
  Physiography <- spTransform(Physiography,CRS("+init=epsg:2193") )
  
  #Convert to a raster with an attribute table, and  for mapping later on
  #Convert to raster, note the creation of a base raster in WGS84, which all subsequent raster's align to
  RasterBase <- raster(resolution = 250, ext = extent(Domain), crs = proj4string(Domain) )
  PhysiographyRaster <- rasterize(Physiography,RasterBase,"Physiograp")
  #Crop to the Complete domain, and then mask to the same
  PhysiographyRaster <- crop(PhysiographyRaster,extent(Domain))
  PhysiographyRaster <- mask(PhysiographyRaster, Domain)
  
  #Note, for the next line I have had issues with repeated errors of "Error in x$.self$finalize() : attempt to apply non-function"
  #I don't know the cause. It just goes away when I run the command again. Maybe it is doing stuff on the raster before the raster has been created.
  #A possible solution is to transform the vector to WGS84 straight up, rather than transform the raster. I might need to create a transformed version of complete domain as well.
  
  PhysiographyRasterWGS84 <- projectRaster(PhysiographyRaster,crs = CRS("+init=epsg:4326"),method = "ngb")
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
  Physiography$TPAttenuation <- AttenLookupTable$TNAttenuation[match(Physiography$Variant,AttenLookupTable$Variant)]
  
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

