best <- function(state,outcome,rank=1)
{
  # read csv file 
  rdata<-read.csv("outcome-of-care-measures.csv", header = TRUE)
  #removing NA cells
  #completecase <- complete.cases(rdata)
  #cdata<-rdata[completecase, ]
  if (is.na(state)){
    stop("invalid state")
  }
  if (is.na(outcome)){
    stop("invalid outcome")
  }
  
  checkstate <- unique(rdata$State)
  checkoutcome <- c("heart attack","heart failure","pneumonia")
  
  if (!(state %in% checkstate))
  {
    stop("invalid state")
  }  
  if (!(outcome %in% checkoutcome))
  {
    stop("invalid outcome")
  } 
  
  if (outcome=="heart attack"){
    outcomecolumn<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  if(outcome =="heart failure"){
    outcomecolumn<- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  if(outcome =="pneumonia"){
    outcomecolumn<- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  data<-rdata[rdata$State==state,c(outcomecolumn,"Hospital.Name")]
  data[,outcomecolumn]<-as.numeric(data[,outcomecolumn])
  completecase <- complete.cases(data)
  cdata<-data[completecase, ]
  cdata<-cdata[order(cdata[,outcomecolumn], cdata[,"Hospital.Name"]),]
  
  if (rank ==20000){
    dimen<-dim(cdata)
    result<- cdata[dimen[1],"Hospital.Name"]
    
  }else {
    result<- cdata[rank,"Hospital.Name"]
    
  }
 result 
 
}






