 
## ===================================================== ##
# Title:        Preprocessing Gradebook data ####
# Project:      132 n-TARP Profiles
#               https://github.com/tzwilliams/
#               
# Authors:      Taylor Williams 
#
# Affiliation:  Purdue University
# 
# Description:  The gradebook data is in a very customized and nonstandard format in an Excel file. This script pulls all of the gradebook data together and cleans it, focusing on midterm and final grades.
# 
# Package dependencies: 
# 
# Input stack: 
#     Gradebook (raw, original)       
#       For example, `ENGR132_Sp18_SemesterGrades_v5_deID.xlsx`
#     CLEANED GRADEBOOK file (Excel)
#       For example, `ENGR132_Sp18_SemesterGrades_v5_deID-3_min_title_meta.xlsx`
#       Modifications from the original raw gradebook are:
#         -remove header information on each of the section sheets, delete first five rows; 
#         -remove unique column IDs from the end of each column, search and replace ` |*` with ``, about 2400 replacements; 
#         -remove point value details from the end of each column, search and replace ` [*` with ``, also about 2400 replacements across the 14 sections; 
#         -delete summary sheets, all sheets that are not raw section data.
#
# Changelog:
#     
#                   
# Feature wish list:  (*: planned but not complete)
#     *2021.03.30.  I would like to pull the reflection data out of this workbook.  The entries seem to be in columns FI - FW
## ===================================================== ##


######### Clean the environment ########## 
rm(list=ls())
# ## alt: Clean the environment except required variables########## 
# rm(list = setdiff(ls(), c(""))
                  

######### Internal functions ########## 



######### Setup ##########
#load required packages
require(tidyverse)
require(readxl)

#Load functions 
source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))
source(file.path(getwd(), "R", "functions", "file-structure-functions.R"))


######### Read Data ##########
if(!exists("dataFolderPath")) dataFolderPath <- NULL
if(!exists("filenameGB")) filenameGB <- NULL

## get data file locations from user ####
#Locate the CLEAN gradebook file
filenameGB <- 
  SelectFile(prompt = "*****Select the CLEANED GRADEBOOK file (Excel)*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
             defaultFilename = "ENGR132_Sp18_SemesterGrades_v5_deID-3_min_title_meta.xlsx",
             fileTypeMatrix = matrix(c("xlsx", "xlsx", "All files", ".*"),
                                     2, 2, byrow = TRUE),
             dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
                                     yes = dataFolderPath, no = ""))



######### Main ##########

#load and process the data based on the type of data file provided
if(grepl(x = filenameGB, 
         pattern = "\\ENGR132_Sp18_SemesterGrades_v5_deID")){
  # load the early columns of the gradebook (Only the first sheet, that is, the first section)
  grades <- read_xlsx(path = filenameGB, 
                      sheet = "Bb001",
                      col_names = T,
                      col_types = "text",
                      skip = 0,
                      range = cell_cols("D:T"))
  
  # Get a preview of column names
  gradebook_names <- names(read_xlsx(path = filenameGB, 
                  sheet = "Bb001",
                  col_names = T, 
                  skip = 0, 
                  n_max = 0,
                  range = cell_cols("D:T")))
  
  # remove unneeded and/or problematic columns
  grades <-  grades %>% 
    select(gradebook_names[c(1,3,6,7,9,11:17)])

  
  ### Attempting to clean the gradebook column names from the original data file. I could not get this to work, so I did some cleaning externally in Excel before proceeding. What I did is described in the script header
  # gsub(x = colnames(grades), 
  #      pattern = "^.*(?=|)",
  #      perl = TRUE)
  # str_view(string = colnames(grades), 
  #      pattern = "^.*(?=[|])")
  
  
  #load the early columns of the gradebook (Sections 2—14)
  for (i in 2:14) {
    temp_grades <- read_xlsx(path = filenameGB, 
                             sheet = i,
                             col_names = gradebook_names, 
                             col_types = "text",
                             skip = 0,
                             range = cell_cols("D:T"))
    
    # remove unneeded and/or problematic columns
    temp_grades <-  temp_grades %>% 
      select(gradebook_names[c(1,3,6,7,9,11:17)])
                                    
    # append the current section's data to the main collection
    grades <- grades %>% bind_rows(temp_grades)
    
    # # Get a preview of column names
    # ### Using the following, I discovered some inconsistencies in how the problems were named. Hence why I used the column names from section 1 in the preceding command
    # print(i)
    # print(names(read_xlsx(path = filenameGB, 
    #                 sheet = i,
    #                 col_names = T, 
    #                 skip = 0, 
    #                 n_max = 0,
    #                 range = cell_cols("D:T"))))
    

  }
}else {
  message("Invalid Data Filetype.")
  break
}

colnames(grades)





######### Save data to file #########
message("\nSaving files.\n")

#write to CSV file
##Save preprocessed gradebook 
write.csv(file = file.path("output", "410_gradebook-MT_final_abs_etc.csv"),
          x = grades, row.names = F)

#write to RData file
save(grades,
     file = file.path("output", "410_gradebook-MT_final_abs_etc.RData"),
     precheck = TRUE, compress = TRUE)

#save input stack details to csv file
write_csv(x = as.data.frame(filenameGB),
     file = file.path("output", "410_gradebook-MT_final_abs_etc-input_stack.csv"))
