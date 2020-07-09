rankhospital <- function(state, outcome, num) {
  dat <- read.csv("outcome-of-care-measures.csv")
  mortality <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                 "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
  names(mortality) <- c("heart attack", "pneumonia", "heart failure")
  if(!state %in% dat$State) {
    stop("invalid state")
  }
  if(!outcome %in% names(mortality)){
    stop("invalid outcome")
  }
  
  splitted <- dat[dat$State == state, ]
  sorted <- splitted[order(as.numeric(splitted[, mortality[outcome]]), splitted$Hospital.Name), ]
  sorted <- subset(sorted, sorted[, mortality[outcome]] != "Not Available")
  
  if(num == "worst") return(sorted$Hospital.Name[nrow(sorted)])
  
  else if(num == "best") return(sorted$Hospital.Name[1])
  
  else if(num > nrow(sorted)) return(NA)
  
  return(sorted$Hospital.Name[num])
}