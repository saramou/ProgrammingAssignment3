best <- function(state, outcome) {
  ## Read outcome data
  
  list <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #Order the list depending on the state
  list<-list[order(list$Hospital.Name),]
  
  ## Check that state and outcome are valid
  if (!state %in% list$State)
  { "incorrect state" 
    }
  else if (!outcome %in% c("heart attack","heart failure","pneumonia"))
  { "Outcome incorrect"
    }
  else {
    if (outcome == "heart attack")
    {
      l <- list[list$State == state,]
      out<-as.numeric(l[,11])
      min <-min(out, na.rm=TRUE)
      index <- which(out==min,l[,7]==state)
      result <- l[index[1],2]
      
    }
    else if (outcome == "heart failure")
    {
      l <- list[list$State == state,]
      out<-as.numeric(l[,17])
      min <-min(out, na.rm=TRUE)
      index <- which(out==min,l[,7]==state)
      result <- l[index[1],2]
      
    }else if (outcome == "pneumonia")
    {
      l <- list[list$State == state,]
      out<-as.numeric(l[,23])
      min <-min(out, na.rm=TRUE)
      index <- which(out==min,l[,7]==state)
      result <- l[index[1],2]
    }
    
    result
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
}