rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack","heart failure","pneumonia"))
  { "Outcome incorrect"
  }
  else {
    result <- data.frame()
    j<-1
    l<-split(data,data$State)
    #head(l)
    r<-names (l)
    for (state in r)
    { 
       #result <-rbind(result,c(state,rankhospital(state,outcome,num)))
      #a<-c(state,rankhospital(state,outcome,num))
      result[j,1]<- state
      result[j,2]<-rankhospital(state,outcome,num)
      j<-j+1
    }
    names(result)<-c("state","hospital")
    result
    

  }

}