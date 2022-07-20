
# Monty Hall simulator

## How many doors? How many goats?

## Variables 
#* Trials
#* Doors
#* Goats

trials <- 1000
doors  <- 3
goats  <- 2

make_deal <- function(doors, goats){
  
  doorVec <- c(1:doors)
  goatVec <- sample(doorVec, goats, replace = FALSE)
  userChoice <- sample(doorVec, 1, replace = FALSE)
  montyChoice <- sample(doorVec[doorVec %in% goatVec[! goatVec %in% userChoice]], 1, replace = FALSE)
  userChoice2 <- sample(doorVec[! doorVec %in% c(userChoice, montyChoice)], 1, replace = FALSE)
  outcomeChange <- as.numeric(userChoice2 %in% doorVec[! doorVec %in% goatVec])
  
  doorVec <- c(1:doors)
  goatVec <- sample(doorVec, goats, replace = FALSE)
  userChoice <- sample(doorVec, 1, replace = FALSE)
  outcomeStay <- as.numeric(userChoice %in% doorVec[! doorVec %in% goatVec])
  
  if(!exists("winLose")){
    winLose <- data.frame(change = outcomeChange,
                          stay   = outcomeStay)
    return(winLose)
  } else{
    tempdata <- data.frame(change = outcomeChange,
                           stay   = outcomeStay)
    winLose <- rbind(winLose, tempdata)
    rm(tempdata)
    return(winLose)
  }

}


winRatio <- do.call(rbind, replicate(trials, make_deal(doors, goats), simplify = FALSE))

print(paste0("Stayers won ", colSums(winRatio)[2], " times. ", "Changers won ", colSums(winRatio)[1], " times."))
