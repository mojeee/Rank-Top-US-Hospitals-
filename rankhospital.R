rankhospital <- function(state, outcome, num = "best") {
  
  source("best.R")
  if (num =="best"){
    data<-best(state=state, outcome = outcome, rank=1 )
  }
  else if (num == "worst"){
    
    data<-best(state=state, outcome = outcome, rank=20000)

  }
  else {
    
    data<-best(state=state, outcome = outcome, rank=num )
    
  }
  
  data
  
}