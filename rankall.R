rankall <- function(outcome, num = "best") {
  ## Read outcome data
  rdata<-read.csv("outcome-of-care-measures.csv", header = TRUE)
  
  ## Check that state and outcome are valid
  checkstate <- sort(unique(rdata$State))
  checkoutcome <- c("heart attack","heart failure","pneumonia")
  
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
  
  if (num =="best"){
    rank<-1
  } else if (num == "worst"){
    
    rank<-20000
    
  } else {
    
    rank<-num
    
  }
  ## For each state, find the hospital of the given rank
  resultall <- data.frame( outcomes=character(),state=character())
  for (state in checkstate) {
    
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
    x<- c(result,state)
    resultall<- rbind(resultall,x)
  }
  colnames(resultall)<-c("hospital","State")
  rownames(resultall)<-checkstate
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  resultall
}