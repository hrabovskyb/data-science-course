corr <- function(directory, threshold = 0, id = 1:332){
  allCor <- data.frame()
  for(i in id) {
    
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
    
    dataSet <- read.csv(file.path(directory,fid))
    
    nNa = is.na(dataSet[,"nitrate"])
    
    dataSet <- dataSet[!nNa,]
    sNa = is.na(dataSet[,"sulfate"])
    
    dataSet <- dataSet[!sNa,]
    
    compObs <- lengths(dataSet,use.names=FALSE)[1]
    
    if (compObs > threshold){
      
      curCor <- cor(dataSet[,"sulfate"],dataSet[,"nitrate"])
      
      curRow <- cbind(i,curCor)
      allCor <- rbind(allCor,curRow)
      
    }
 

  }
  colnames(allCor) <- c("id","Cor")
  return(allCor)
  
}