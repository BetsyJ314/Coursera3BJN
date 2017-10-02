

best <- function(state,outcome) 
  {
  ##read outcome data
  HData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##convert rates to numeric, suppress warnings to get a result
  suppressWarnings(HData[, 11] <- as.numeric(HData[, 11])) # heart attack
  suppressWarnings(HData[, 17] <- as.numeric(HData[, 17])) # heart failure
  suppressWarnings(HData[, 23] <- as.numeric(HData[, 23])) # pneumonia
  
  ##check state and outcome are valid
  ##state: use stop function with message "invalid state"
  HdataST <- c(unique(HData[,7]))
  if (!state %in% HdataST) {stop("invalid state")}
  
  ##outcome: use stop with message "invalid outcome"
  ailment<-c("Heart Attack", "Heart Failure", "Pneumonia")
  if(!outcome %in% ailment) {stop("Invalid outcome")}
  
  ## now gather data by selected state and retrieve selected column
  DataByState <- HData[HData$State == state,]
  ##col rename
  names(DataByState)[c(11, 17, 23)] <- ailment
  ##find min 
  if(outcome == "Heart Attack"){
    HDataSetAsc = DataByState[order(DataByState[,11], decreasing=F),]
    }
  ##testing heart failure answer
  if(outcome == "Heart Failure"){
    HDataSetAsc = DataByState[order(DataByState[,17], decreasing=F),]
    }
  if(outcome == "Pneumonia"){
    HDataSetAsc = DataByState[order(DataByState[,23], decreasing=F),]
    }  
  
  ## return min of selected column (death rate)
  ## first row
  bestHospital <- head(HDataSetAsc, 1)
  ## second column
  return(bestHospital[,2])
}
  