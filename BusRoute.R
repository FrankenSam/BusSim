
busRoute <- function(nstop, nbus, arrivalrate, travelrate, maxsimtime, dgb=FALSE) {
    simlist <- newsim(dgb, busRouteReact)
    simlist$passRate <- arrivalrate
    simlist$busRate <- travelrate
    simlist$passEvnt <- 1
    simlist$busEvnt <- 2
    simlist$immedPass <- 0
    simlist$waitPass <- 0
    simlist$circuitNum<- 0
    
    simlist$busStop <- cbind(rep(0, nstop), rep(F, nstop))
    simlist$busStop[1:nbus,1] <- 1:nbus
    
    for (currStop in 1:nrow(simlist$busStop)) {
        timeofnextarrival <- rexp(1, simlist$passRate)
        schedevnt(timeofnextarrival, simlist$passEvnt, simlist, c(currStop, simlist$busStop[currStop, 1]))
    }
    mainloop(simlist, maxsimtime)
    cat("average circuit time: \n", maxsimtime / (simlist$circuitNum / nbus))
    cat("\nprop. pass. immed. boards:: \n",simlist$immedPass / (simlist$immedPass + simlist$waitPass))
  
}

busRouteReact <- function(evnt, simlist){
    etype <- evnt[2]
    currStop <- evnt[3]

    if(etype == simlist$passEvnt){
        
        #updates time and schedules next arrival of passengers
        timeofnextarrival <- simlist$currtime + rexp(1, simlist$passRate)
        
        schedevnt(timeofnextarrival, simlist$passEvnt, simlist, c(currStop, simlist$busStop[currStop, 1]))
        simlist$busStop[currStop,2] <- TRUE
        #checks for bus waiting
        if(simlist$busStop[currStop,1] != 0){
            timeofnexttravel <- simlist$currtime + rexp(1, simlist$busRate)
            
            if(currStop == nrow(simlist$busStop)){
                nextStop <- 1
            }
            else{
                nextStop <- currStop + 1
            }
            schedevnt(timeofnexttravel, simlist$busEvnt, simlist, c(nextStop, simlist$busStop[currStop, 1]))
            simlist$immedPass <- simlist$immedPass+1
            #resets status
            simlist$busStop[currStop, 1] <- 0
            simlist$busStop[currStop, 2] <- FALSE
        }
        else{
            simlist$waitPass <- simlist$waitPass+1
        }

    }
    else if(etype == simlist$busEvnt){
        currBus <- evnt[4]
        #if bus location is the same again, increment circuit count
        if(currBus == currStop){
            simlist$circuitNum <- simlist$circuitNum + 1 
        }
        #checks for waiting passengers
        if(simlist$busStop[currStop,2] == TRUE){
            timeofnexttravel <- simlist$currtime + rexp(1, simlist$busRate)
            
            if(currStop == nrow(simlist$busStop)){
                nextStop <- 1
            }
            else{
                nextStop <- currStop + 1
            }
            schedevnt(timeofnexttravel, simlist$busEvnt, simlist, c(nextStop, currBus))
            simlist$busStop[currStop, 1] <- 0
            simlist$busStop[currStop, 2] <- FALSE


        }
        #checks to see if there is a bus at the stop
        else if(simlist$busStop[currStop,2] == FALSE){
            if(simlist$busStop[currStop,1] == 0){
                simlist$busStop[currStop,1] <- currBus
            }
            else{
                timeofnexttravel <- simlist$currtime + rexp(1, simlist$busRate)
                if(currStop == nrow(simlist$busStop)){
                    nextStop <- 1

                }
                else{
                    nextStop <- currStop + 1
                }

                schedevnt(timeofnexttravel, simlist$busEvnt, simlist, c(nextStop, currBus))
            }
        }
    }
}