## ===================================================== ##
# Title:        Master script for ... ####
# Project:      Dissertation
#               
# Authors:      Taylor Williams
#
# Affiliation:  Purdue University
# 
# Description:  Master script for 
## ===================================================== ##


## Clean the environment########## 
rm(list = ls())

##Set the path to working directory
path_wd <- readline(prompt = "Please enter the working directory : \n")
setwd(path_wd)

##Set path to folder containing input files
path_files <- readline(prompt = "Please enter the path to folder containing
                       input files : \n")
path_files <- paste0(path_files, "/")

##Set path to folder where output files are to be saved
path_output <- readline(prompt = "Please enter the path to folder in which
                        output files are to be saved : \n")
path_output <- paste0(path_output, "/")

##Load required libraries
require("readr")
require("dplyr")
require("plotly")
require("beepr")

##Load the funtions required in the RP1D part of pipeline
source(paste0(path_wd, "/functions_RP1D/CalcProjectionMatrix.R"))
source(paste0(path_wd, "/functions_RP1D/ExtractRVnumsAndNames.R"))
source(paste0(path_wd, "/functions_RP1D/DisplayPercentComplete.R"))

##Enter the course number for which the pipeline is to be run
course <<- readline(prompt = "Enter the course number
                    : \n")
course <<- as.character(course)


##The RP1D part of the pipeline begins....
##Run script for generating random directions for RP1D
source("3_generateRandDirections.R")

##Run script for calculating projections of HMM parameter vectors onto RP1D directions
source("4_calcProjections.R")

##Run script for finding the best RP1D projection directions
source("5_findBestThreshold.R")

##Run script for finding usage profiles
source("6_cluster.R")

##Done!!!!
cat("Done for course ", course, "!!!\n\n\n")
beep()
