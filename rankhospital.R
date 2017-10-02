rankhospital <- function(state, outcome,num='best') {
  
  if(is.character(num)){
    if (num %in% c('best','worst')==FALSE){
      stop('Msg: Invalid Rank')
    } }
  
  Hdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  suppressWarnings(Hdata[, 11] <- as.numeric(Hdata[, 11])) # heart attack
  suppressWarnings(Hdata[, 17] <- as.numeric(Hdata[, 17])) # heart failure
  suppressWarnings(Hdata[, 23] <- as.numeric(Hdata[, 23])) # pneumonia
  
  ##check state and outcome are valid
  ##state: use stop function with message "invalid state"
  HdataST <- c(unique(Hdata[,7]))
  if (!state %in% HdataST) {stop("Msg: invalid state")}
  
  ##outcome: use stop with message "invalid outcome"
  ailment<-c("Heart Attack", "Heart Failure", "Pneumonia")
  if(!outcome %in% ailment) {stop("Msg: Invalid outcome")}
  
  ##determine analysis dataset a
    if(outcome=="Heart Attack"){
      a<-Hdata[,c(2,7,11)]
    }
    
    if(outcome=="Heart Failure"){
      a<-Hdata[,c(2,7,17)]
    }
    
    if(outcome=="Pneumonia"){
      a<-Hdata[,c(2,7,23)]
    }
    
  #split dataset a (hospital, state, ailment) by state
    ast<-split(a,a$State)
    c<-ast[[state]]
    #all rows, cols hospital and ailment in dataset c
    c<-c[,c(1,3)]
    colnames(c)<-cbind('Hospital',outcome)
    #set rates to number
    c[,2]<-as.numeric(c[,2])
    
    
    if (num=="best"){
      #create dataet from dataset c where ailment is min on that column, ignoring NAs
      adata<- c[which(c[,2]==min(c[,2],na.rm=TRUE)),]
      #create dataseet H to equal hospital name corresponding to min ailment rate
      H<-adata$Hospital
      hospital<-sort(H)[1]
      
    }
    
    if (num=='worst'){
      #same as above but the max (worst)
      adata<- c[which(c[,2]==max(c[,2],na.rm=TRUE)),]
      
      H<-adata$Hospital
      hospital<-sort(H)[1]
      
    }
    #now we need rank "num"
    #dataset c is all data for selected state
    #normal course
    if(is.numeric(num) & num < length(c$Hospital)){
      
    #c is data with col 1 = hospital and col 2 = outcome
    #remove NAs from outcome rates
      j<-c[!is.na(c[,2]),]
    #sort on outcome rates to determine unique # rows
      jj<-unique(sort(j[,2]))
    #set empty vector  
      m<-c()
    #loop though with max loops = number of rows in jj 
      for ( i in 1:length(jj)){
    #temp vector is needed
        HLoopData<-which(j[,2]==jj[i])
    #add each row to m
        m<-rbind(m,j[HLoopData,])
      }
    
    #now take just the num rows of m and the outcome rate 
      k<-m[num,2]
    #Ltcnt = number of rows < selected rank
      Ltcnt<-which(m[,2]<k)
    #Cnt = row which = num
      Cnt<-which(m[,2]==k)
    #the rank we want
      numb<-num-length(Ltcnt)
      
    #sort m for the first n- rows
      H<-sort(m[Cnt,1])
    #extract desied row and col 1: Hospital name 
      hospital<-H[numb] 
      
    }
    #if selected num is > number of hospitals available
    if (is.numeric(num)){
      if (num > length(c$Hospital)){
        #used error message I know I would recognize & not a default R msg
        return('N/A')}}

  
  return(hospital)   
}
