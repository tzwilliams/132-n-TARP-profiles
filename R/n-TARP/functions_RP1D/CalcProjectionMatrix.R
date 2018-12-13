# 2017.09.20. extracted from 30_

CalcProjectionMatrix <- function(probMatrix, randomVectors, RV_nums){ 
  
  #custom function(s)
  #source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))
  
  #find the number of dimensions for the projection
  numDims <- nrow(randomVectors)
  
  ##Dot product of user vectors (in probMatrix) with random vector (in randomVectors) ####
  #create matrix containing only the probabilities (converted to matrix to do matrix multiplation below) 
  userVector <- probMatrix
  #create empty data frame to store results of matrix multiplication
  projection <- data.frame(matrix(nrow = nrow(userVector), 
                                  ncol = length(randomVectors)))
  #set column names
  for(i in 1:length(randomVectors))
  {
    if(i==1)
    {
      projectionColNames <- paste0("RP1D_",RV_nums[i])
    }else
    {
      projectionColNames[i] <- paste0("RP1D_",RV_nums[i])
    }
  }
  colnames(projection) <- projectionColNames
  
  #set row names to be the users' ID
  rownames(projection) <- rownames(probMatrix)
  
  # calculate dot products for all users with all random vectors
  for(i in 1:length(randomVectors))
  {
    #build current column name
    curColName <- paste0("RP1D_",RV_nums[i])
    
    #calculate dot product and store into projection data frame (dot 
    #   product returns a single scalar by taking the sum of the 
    #   products of the two vectors)
    projection[curColName] <- 
      as.numeric(as.matrix(userVector[,-1])) %*% as.matrix(randomVectors[[i]])  #TODO(TW: get rid of the first row)
    
    
    #| print completion progress to console   ####
    #durring first iteration, create progress status variables for main processing loop
    # if(i==1)
    # {
    #   iCount <- 0 #loop counter for completion updates
    #   pct <- 0  #percentage complete tracker
    # }
    
    #print function
    # updateVars <- DisplayPercentComplete(randomVectors, iCount, pct, displayText = "Calculating projections: ")
    
    #update status variables
    # iCount <- updateVars$iCount
    # pct <- updateVars$pct
    # 
    # #print update
    # cat(updateVars$toPrint)
    
  }
  return(projection)
}
