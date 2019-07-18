pollutantmean <- function(directory, pollutant, id = 1:332){
  allMonitors <- data.frame()
  
  for(i in id){
    
    if(i < 10){
      monitor <- paste("00",toString(i),sep="")
    }
    else if (i < 100){
      monitor <- paste("0",toString(i),sep="")
    }
    else {
      monitor <- toString(i)
    }
    
    fid <- paste(monitor,".csv",sep="")
    curMonitor <- read.csv(file.path(directory,fid))
    allMonitors <- rbind(allMonitors,curMonitor)
    
  }
  
  return(mean(allMonitors[,pollutant],na.rm = TRUE))
}