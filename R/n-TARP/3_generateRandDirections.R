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
# Description:  Generates 1000 random projection directions for the RP1D clustering
#               approach
## ===================================================== ##

## Clean the environment except required variables ########## 
  rm(list = setdiff(ls(), c("course", "path_files", "path_output",
                            "CalcProjectionMatrix", "ExtractRVnumsAndNames",
                            "DisplayPercentComplete")))

# path = "/Users/roy57/Documents/R/PNAS"
# setwd(path)

## Required libraries ########## 
#require("dplyr")

# course <- readline(prompt = "Enter the course number (in format B1 / B2 and so on) : \n")
# course <- as.character(course)


#Read data from files ####
#read the CLEAN probability matrix CSV file
# prompt <- "*****Select the CLEAN PROBABILITY MATRIX CSV file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
# cat("\n", prompt)
# filename <- tcltk::tk_choose.files(caption = prompt, 
#                                    default = file.path(getwd(), 
#                                                        "output", 
#                                                        "10_passForwardData_CPM.RData"),
#                                    filter = matrix(c("RData", ".RData",
#                                                      "CSV", ".csv",
#                                                      "All files", ".*"),
#                                                    3, 2, byrow = TRUE),
#                                    multi = FALSE)
#load in the data based on the type of data file provided
# if(grepl(x = filename, pattern = "\\.RData$"))
# {
#   load(file = filename)
# }else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
# {
#   probMatrix <- read_csv(file = filename)
# }else
# {
#   message("Invalid Data Filetype.")
#   break
# }

  #Read in HMM parameter values for users 
  probMatrix <- read_csv(file.choose())


##Generate random vectors ####
  #set the number of random vectors to generate
  numRandVectors <- 1000

  #save the state of the random number generator's seed 
  #   TO RESTORE SEED if reproduciability is needed, uncomment the following lines:
  #     load(file = paste0(path_output, "randomSeed_", course, ".RData"))
  #     .Random.seed  <- oldSeed
  oldSeed <- .Random.seed
  save(list = "oldSeed", file = paste0(path_output, "randomSeed_", course, ".RData"))

  
  #find the number of dimensions present in the probability matrix 
  #   (by counting the number of columns whose names contain "P(*)")
  # numDims <-  length(select(.data = probMatrix[1,], matches(match = "P\\(.*\\)")))
  numDims <- ncol(probMatrix)
  #generate numRandVectors vectors with numDims dimensions (values range from 0:1 uniformly, from runif() function)
  randomVectors <- replicate(numRandVectors, runif(numDims), simplify=FALSE)
  #convert to a data frame
  randomVectors <- as.data.frame(randomVectors)
  #set column names
  colnames(randomVectors) <- paste0("RV_",1:length(randomVectors))
  
##| Save data to file ####
  # cat("\nSaving files.")
  #Save the random vectors
  #write a CSV file
  # write.csv(file = file.path("output", "28_randomVectors.csv"), 
  #           x = randomVectors)  
  #write to a RData file
  # save(list = c("randomVectors","numDims"), file = file.path("output", "28_passForwardData_RV.RData"),
  #      precheck = TRUE, compress = TRUE)
  write.csv(randomVectors, paste0(path_output, "randomVectors_", course, ".csv"))
  beep()
  cat("Done producing random directions for RP1D...\n\n\n")
