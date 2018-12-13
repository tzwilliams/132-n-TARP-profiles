## ===================================================== ##
# Title:        Generate Random Vectors ####
# Project:      FutureLearn user clustering analytics. This script is a modified
#               version of the original. Adapted and modified by Doipayan Roy
#               for the Boeing project. The original script can be found at:
#               https://github.com/tzwilliams/futureLearn2017. The aim is model
#               usage trajectories using hidden Markov models and use the RP1D
#               method to project the HMM parameter space onto a set of optimal
#               directions in the hope of finding typical usage 'profiles'.
# 
#
# Authors:      Taylor Williams (principal author), Doipayan Roy (made modifications
#               required for using script on the Boeing data)
#
# Affiliation:  Purdue University
# 
# Description:  Calculate optimal clustering threshold for each projection direction
#               in RP1D 
## ===================================================== ##

## Clean the environment except required variables########## 
rm(list = setdiff(ls(), c("course", "path_files", "path_output", "probMatrix",
                          "CalcProjectionMatrix", "ExtractRVnumsAndNames",
                          "DisplayPercentComplete")))
# path = "/Users/roy57/Documents/R/PNAS"
# setwd(path)
# 
# course <- readline(prompt = "Enter the course number (in format B1 / B2 and so on) : \n")
# course <- as.character(course)
# 

## Required libraries ########## 
# require("readr")
#require("tcltk")
#require("tidyr")
# require("dplyr")
#require("tibble")

#custom function(s)
# source(file.path(getwd(), "R", "functions", "ExtractRVnumsAndNames.R"))
# source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))
# source(paste0(path, "/functions_RP1D/ExtractRVnumsAndNames.R"))
# source(paste0(path, "/functions_RP1D/DisplayPercentComplete.R"))



## Read data from file(s) ####
  #read the PROJECTIONS data file
  # prompt <- "*****Select the PROJECTIONS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
  # cat("\n", prompt, "\n\n")
  # filename <- tcltk::tk_choose.files(caption = prompt, 
  #                                    default = file.path(getwd(), 
  #                                                        "output", 
  #                                                        "30_projections.RData", 
  #                                                        fsep = "/"),
  #                                    filter = matrix(c("RData", ".RData",
  #                                                      "CSV", ".csv",
  #                                                      "All files", ".*"),
  #                                                    3, 2, byrow = TRUE),
  #                                    multi = FALSE)
  # #load in the data based on the type of data file provided
  # if(grepl(x = filename, pattern = "\\.RData$"))
  # {
  #   load(file = filename)
  # }else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
  # {
  #   projection <- read_csv(file = filename)
  # }else
  # {
  #   message("Invalid Data Filetype.")
  #   return
  # }

  #Read in projection values along 1000 directions
  projection <- read.csv(paste0(path_output, "projectionValues_", course,
                                ".csv"), row.names = 1)

  
  
  #get the string values for the random vectors 
  numsAndNames <- ExtractRVnumsAndNames(RP_names = names(projection))
  
  #split the returned list into two seperate variables.  Convert to matricies for use in the next step
  RV_nums <- as.matrix(numsAndNames$nums)
  RV_names <- as.matrix(numsAndNames$names)
  
  
  
## Sort all of the projection columns ####

  #duplicate the projection data frame to store results of sorted projections
  projectionSort <- projection
  #replace the row names with an assending integer 
  #   (after sorting, the rows will no longer refer to a single user's projection)
  rownames(projectionSort) <- c(1:nrow(projectionSort))
  
  #sort each column 
  for(i in 1:ncol(projection))
  {
    projectionSort[,i] <- projection[order(projection[i]),i]
  }

## Test each projection value as the threshold to find best threshold value (minimizing withinSS, W) ####
  #create empty data frame to store results of matrix multiplication
  minW_RandVec <- data.frame(matrix(nrow = 2, ncol = length(projection)))
  #set names
  #set column names
  for(i in 1:length(minW_RandVec))
  {
    if(i==1)
    {
      minW_RandVecColNames <- paste0("RP1D_",RV_nums[i])
    }else
    {
      minW_RandVecColNames[i] <- paste0("RP1D_",RV_nums[i])
    }
  }
  colnames(minW_RandVec) <- minW_RandVecColNames
  
  # 
  # colnames(minW_RandVec) <- paste0("RP1D_",1:length(projection))
  # set row names
  rownames(minW_RandVec) <- c("Min WithinSS (W)", "Group Threshold")
  
  for(i in 1:ncol(projectionSort))
  {
    #build current column name
    curColName <- paste0("RP1D_",RV_nums[i])
    
    #reset the minimimum withinSS value to an abserdly large value
    minW <- 1e10
    
    for(j in 1:nrow(projectionSort))
    {
      #test each of the projection values (x_j) as the threshold 
      testThresh <- projectionSort[j,i]
      
      #split ith sorted projection into two groups
        #subset of projection values less than the threshold
        group1_ltThresh <- projectionSort[1:(j-1),curColName]
        
        #subset of projection values greater than or equal to the threshold
        group2_gteThresh <- projectionSort[j:nrow(projectionSort),curColName]
      
      #calculate variance for each group and for the whole population of projections 
        varGroup1 <- var(group1_ltThresh)
        varGroup2 <- var(group2_gteThresh)
        varAll    <- var(projectionSort[,curColName])
      
      #calculate the withinSS value  
        curW <- (varGroup1 + varGroup2)/varAll
      
      #see if current withinSS value is less than previous minimum, 
      #  if so, set current value to be the minimum  
        if((curW < minW) & !is.na(curW))
        {
          minW <- curW
          bestThresh <- testThresh
        }
    }
    
    #save the min withinSS value and associated threshold for each projection set 
    minW_RandVec["Min WithinSS (W)", curColName] <- minW
    minW_RandVec["Group Threshold", curColName]  <- bestThresh

    
    #| print completion progress to console   ####
    #durring first iteration, create progress status variables for main processing loop
    if(i==1)
    {
      iCount <- 0 #loop counter for completion updates
      pct <- 0  #percentage complete tracker
    }
    
    #print function
    updateVars <- DisplayPercentComplete(projectionSort, iCount, pct, displayText = "Locating best threshold values: ")
    
    #update status variables
    iCount <- updateVars$iCount
    pct <- updateVars$pct
    
    #print update
    cat(updateVars$toPrint)

    #show loop completion progress on console for every 1% complete
    if(i==1)
    {
      #create progress status variables for male processing loop
      iCount <- 0 #loop counter for completion updates
      pct <- 0  #percentage complete tracker
    }else
    {
      iCount <- iCount + 1
      if(iCount%%as.integer(ncol(projectionSort)/100) == 0 &&
         pct <= 100)
      {
        pct <- pct + 1
        cat("\rLocating best threshold values: ", pct, "% complete", sep = "")
    }
    }
  }
  
  #transpose and sort the min. withinSS and group threshold data
  minW_RandVec_sort <- as.data.frame(t(minW_RandVec))
  minW_RandVec_sort <- minW_RandVec_sort[order(minW_RandVec_sort$`Min WithinSS (W)`),]

## |Save min. withinSS and group threshold data to file ####
  #write a CSV file
  # cat("\nSaving CSV file.")
  # write.csv(file = file.path("output", "40_minW_and_threshold.csv"), 
  #           x = minW_RandVec_sort)  
  # #write to a RData file
  # save(minW_RandVec_sort, file = file.path("output", "40_minW_and_threshold.RData"),
  #      precheck = TRUE, compress = TRUE)
  write.csv(minW_RandVec_sort, paste0(path_output, "minW_Threshold_", course, ".csv"))
  beep()
  cat("\nDone finding best directions...\n\n\n")
  
## Identify projections that cluster ####
  #set the threshold value for W
  #clusterWThreshold <- 0.36
  
  #identify the projections that resulted in a W below the threshold
  # clusterCandidates <- minW_RandVec[1,minW_RandVec["Min WithinSS (W)",] < clusterWThreshold]
  # sortedCandidates <- clusterCandidates[1,order(clusterCandidates)]

  #report percentage of promising projections 
  # length(clusterCandidates)/length(minW_RandVec)
  
  #save the names of the cluster candidates
  # sortedCandidateNames <- names(sortedCandidates)
  
##|Save data to file ####
  #write to a CSV file
  # cat("\nSaving CSV file.")
  # write.csv(file = file.path("output", "40_best_RP_names.csv"), 
  #           x = sortedCandidateNames)  
  #write to a RData file
  # save(sortedCandidateNames, file = file.path("output", "40_best_RP_names.RData"),
  #      precheck = TRUE, compress = TRUE)

  
