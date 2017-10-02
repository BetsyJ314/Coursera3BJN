rankall <- function(outcome,num="best") 
{
  
  if(is.character(num)){
    if (num %in% c("best","worst")==FALSE){
      stop('Msg: Invalid Rank')
    } 
  }
  
  ##outcome: use stop with message "invalid outcome"
  ailment<-c("Heart Attack", "Heart Failure", "Pneumonia")
  if(!outcome %in% ailment) {stop("Msg: Invalid outcome")}
  
  
  #bring in hospital data
  Hdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #set results as numeric
  suppressWarnings(Hdata[, 11] <- as.numeric(Hdata[, 11])) # heart attack
  suppressWarnings(Hdata[, 17] <- as.numeric(Hdata[, 17])) # heart failure
  suppressWarnings(Hdata[, 23] <- as.numeric(Hdata[, 23])) # pneumonia
  
  #create analysis table a
  if(outcome=="Heart Attack"){
    a<-Hdata[,c(2,7,11)]
    }
  
  if(outcome=="Heart Failure"){
    a<-Hdata[,c(2,7,17)]
    }
  
  if(outcome=="Pneumonia"){
    a<-Hdata[,c(2,7,23)]
    }
  
  ##remove NAs in Hospital Clean Data
  HCleanData <- !is.na(a[,3])
 
  ##bring in the good data for analysis
  a<-a[HCleanData,]
  a[,3]<-as.double(a[,3])
  ##set column names per instructions
  colnames(a)<-cbind('Hospital','state','figure')
  
  ##divide table a for analysis by state
  ast<-a$state
  ast<-unique(ast)
  ##length ast = unique count of states
  
  ##empty vectors
 
  m<-c()
  ## one listing per state
  
  ##best hospital by state
  if (num=="best"){
    #i<- 1
    i=1
    for (st in ast){
      HLoopData<-a["state"]==st
      #just state x rows, all cols
      HStateData<-a[HLoopData,]
      HSTData<-HStateData[order(HStateData["figure"],HStateData["Hospital"]),]
      #row 1 is best
      m<-rbind(m,head(HSTData,1))
      i = i+1
      }
  }
  
  ##worst hospital by state
  if (num=="worst"){
    #i<- 1
    i=1
    for (st in ast){
      HLoopData<-a["state"]==st
      #just state x rows, all cols
      HStateData<-a[HLoopData,]
      HSTData<-HStateData[order(HStateData["figure"],HStateData["Hospital"]),]
      #last row is worst
      m<-rbind(m,tail(HSTData,1))
      i = i+1
    }
  }
  
  ##by choice hospital by state
  if (is.numeric(num)){
    #i<- 1
    i=1
    intRowcount = 1
    for (st in ast){
      HLoopData<-a["state"]==st
      #just state x rows, all cols
      HStateData<-a[HLoopData,]
      HSTData<-HStateData[order(HStateData["figure"],HStateData["Hospital"]),]
      #row count to know when to insert NAs
      intRowcount = nrow(HSTData)
      # if selected num is greater than number of hospitals for state x
      if (intRowcount < num){
          mm<-head(HSTData,1)
          mm$Hospital<- NA
          mm$figure<- NA
      }
      #normal course
      if (intRowcount >= num){
        mm<-head(HSTData,num)
      }
      m<-rbind(m,tail(mm,1))  
      i = i+1
    }
  }
  #only want to display first 2 columns
  output<-m[,1:2]
  #sort alpha
  outputsorted<-output[order(output["state"]),]
  colnames(outputsorted)<-c("Hospital","State")
  return(outputsorted)
}  
  
  
  
  
  
  
 
  

