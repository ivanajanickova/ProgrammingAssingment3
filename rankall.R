rankall <- function(outcome, num) {
  dat <- read.csv("outcome-of-care-measures.csv")
  mortality <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                 "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
  names(mortality) <- c("heart attack", "pneumonia", "heart failure")
  
  if(!outcome %in% names(mortality)){
    stop("invalid outcome")
  }
  
  sorted <- dat[order(as.numeric(dat[, mortality[outcome]]), dat$State), ]
  sorted <- subset(sorted, sorted[, mortality[outcome]] != "Not Available")
  
  splitted <- split(sorted, sorted$State)
  
  index <- 1
  hospital <- NULL
  state <- NULL

  for( i in splitted) {
    if(num == "best") {
      hospital[index] <- i[1, "Hospital.Name"]
      state[index] <- i[1, "State"]
      index <- index + 1
    }
    else if(num == "worst") {
      hospital[index] <- i[nrow(i), "Hospital.Name"]
      state[index] <- i[nrow(i), "State"]
      index <- index + 1      
    }
    else if(num > nrow(i)){
      hospital[index] <- NA
      state[index] <- i[1, "State"]
      index <- index + 1
    }
    else {
    hospital[index] <- i[num, "Hospital.Name"]
    state[index] <- i[num, "State"]
    index <- index + 1
    }
  }
  result <- data.frame(hospital, state)
  
  return(result)
}