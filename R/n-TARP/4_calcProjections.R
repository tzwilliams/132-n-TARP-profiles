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
# Description:  Calculate projection values of user parameter vectors onto random
#               projection directions
## ===================================================== ##

## Clean the environment except required variables########## 
rm(list = setdiff(ls(), c("course", "path_files", "path_output", "probMatrix",
                          "CalcProjectionMatrix", "ExtractRVnumsAndNames",
                          "DisplayPercentComplete")))
# path_output = "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Learning Pathway Profiles/outputs/B4/"
# # setwd(path)
# 
#  course <- readline(prompt = "Enter the course number (in format B1 / B2 and so on) : \n")
#  course <- as.character(course)
# 
# ## Required libraries ##########
# #packages
#  require("readr")
#  require("tcltk")
#  require("tidyr")
#  require("dplyr")
# 
# #custom functions
#  source(file.path("functions_RP1D", "CalcProjectionMatrix.R"))
#  source(file.path("functions_RP1D", "ExtractRVnumsAndNames.R"))
#  source(file.path("functions_RP1D", "DisplayPercentComplete.R"))

#Read in the random projection vectors
randomVectors <- read.csv(paste0(path_output, "randomVectors_", course, ".csv"), row.names = 1)
#Read in the user vectors
# probMatrix <-  read.csv(paste0(path_output, "hmmParameters_", course, ".csv"), row.names = 1)

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
# #load in the data based on the type of data file provided
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
# 
# #read the RANDOM VECTORS data file (from 28_passForwardData.RData)
# prompt <- "*****Select the RANDOM VECTORS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
# cat("\n", prompt, "\n\n")
# filename <- tcltk::tk_choose.files(caption = prompt, 
#                                    default = file.path(getwd(), 
#                                                        "output", 
#                                                        "28_passForwardData_RV.RData"),
#                                    filter = matrix(c("RData", ".RData",
#                                                      "All files", ".*"),
#                                                    2, 2, byrow = TRUE),
#                                    multi = FALSE)
# #load in the data based on the type of data file provided
# if(grepl(x = filename, pattern = "\\.RData$"))
# {
#   load(file = filename)
# }else
# {
#   message("Invalid Data Filetype.")
#   break
# }



#get the string values for the random vectors 
numsAndNames <- ExtractRVnumsAndNames(RP_names = names(randomVectors))

#split the returned list into two seperate variables.  Convert to matricies for use in the next step
RV_nums <- as.matrix(numsAndNames$nums)
RV_names <- as.matrix(numsAndNames$names)


##Calc Projections (Dot product of user vectors (in probMatrix) with random vectors (in randomVectors)) ####
projection <- CalcProjectionMatrix(probMatrix = probMatrix, 
                                   randomVectors = randomVectors, 
                                   RV_nums = RV_nums)

##| Save data to file ####
#Save projection values
#write a CSV file
# write.csv(file = file.path("output", "30_projections.csv"), 
#           x = projection)  
#write to a RData file
# save(projection, file = file.path("output", "30_projections.RData"),
#      precheck = TRUE, compress = TRUE)
write.csv(projection, paste0(path_output, "projectionValues_", course, ".csv"))
beepr::beep()
cat("Done calculating projections...\n\n\n")
