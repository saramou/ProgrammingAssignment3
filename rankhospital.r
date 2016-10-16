rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  list <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if (!state %in% list$State)
  {
    "incorrect state" 
  }
  else if (!outcome %in% c("heart attack","heart failure","pneumonia"))
  { "Outcome incorrect"
  }
  else {
    
    oc<-data.frame()
    oc<-cbind(c("heart attack","heart failure","pneumonia"),c(11,17,23))
    i<-as.numeric(oc[which(oc[,1]== outcome),2])
    list <- list[list$State == state,]
    
    if (num=="best")
    {
      list<-list[order(list$Hospital.Name),]
      out<-as.numeric(list[,i])
      min <-min(out, na.rm=TRUE)
      
      index <- which(list[,i]==min,list[,7]==state)
      result <- list[index[1],2]
    }
    else if (num=="worst")
    {
      out<-as.numeric(list[,i])
      max <-max(out, na.rm=TRUE)
      print(max)
      #list<-list[order(list$Hospital.Name),]
      index <- which(list[,i]==max,list[,7]==state)
      print(index)
      result <- list[index[1],2]
      
    }
    else {
      list<-list[order(list$Hospital.Name),]
      #list[,11]<-as.numeric(list[,17])
      list<-list[order(as.numeric(list[,i])),]
      result<-list[num,2]
    }
    result
    }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

}