best <- function(state, outcome) {
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
  lowest_mortality <- c(20)
  hospital <- NULL
  
  for(i in 1 : nrow(splitted)) {
    if(splitted[i, mortality[outcome]] != "Not Available") {
      current_mortality <-  as.numeric(splitted[i, mortality[outcome]])

      if(current_mortality < lowest_mortality | is.null(lowest_mortality)) {
        lowest_mortality<- current_mortality
        print(lowest_mortality)
        hospital <- splitted$Hospital.Name[i]
        }
      else if(current_mortality == lowest_mortality) {
        hospital2 <- splitted$Hospital.Name[i]
        if (substr(hospital2, 1, 1) < substr(hospital, 1, 1)) {
          hospital <- hospital2
        }
      }
    }
  }
    return(hospital)
}