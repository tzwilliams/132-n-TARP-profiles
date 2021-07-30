 
## ===================================================== ##
# Title:        Evaluating student assessments at different LO levels ####
# Project:      132 n-TARP Profiles
#               https://github.com/tzwilliams/
#               
# Authors:      Taylor Williams 
#
# Affiliation:  Purdue University
# 
# Description:  
# 
# Package dependencies: 
# 
# Input stack: 
#     
#
# Changelog:
#     2020.05.13.   init
#                   
# Feature wishlist:  (*: planned but not complete)
#     *
## ===================================================== ##


######### Clean the environment ########## 
rm(list=ls())
# ## alt: Clean the environment except required variables########## 
# rm(list = setdiff(ls(), c(""))
                  

######### Internal functions ########## 



######### Setup ##########
#load required packages
require(tidyverse)

#Load functions 
source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))
source(file.path(getwd(), "R", "functions", "file-structure-functions.R"))


######### Read Data ##########
######### get from 4xx_
# if(!exists("dataFolderPath")) dataFolderPath <- NULL
# if(!exists("filename")) filename <- NULL
# 
# ## get data file locations from user ####
# #Locate the ______ file
# filenameGB <- 
#   SelectFile(prompt = "*****Select the CLEANED GRADEBOOK file (Excel)*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
#              defaultFilename = "ENGR132_Sp18_SemesterGrades_v5_deID",
#              fileTypeMatrix = matrix(c("xlsx", "xlsx", "All files", ".*"),
#                                      2, 2, byrow = TRUE),
#              dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
#                                      yes = dataFolderPath, no = ""))
# 
# 100_assessmentData.RData
# 300_profiles-10_criteria.RData

######### Main ##########

## Add numerical value for the proficicency levels
data_raw100_assessment_training <- add_column(data_raw100_assessment_training, rubric_pts = NA)

for (i in 1:nrow(data_raw100_assessment_training)) {
  if (data_raw100_assessment_training$`Rubric Column`[i] == "Proficient") {
    data_raw100_assessment_training$rubric_pts[i] <- 4
  } else if (data_raw100_assessment_training$`Rubric Column`[i] == "Developing") {
    data_raw100_assessment_training$rubric_pts[i] <- 3
  } else if (data_raw100_assessment_training$`Rubric Column`[i] == "Emerging") {
    data_raw100_assessment_training$rubric_pts[i] <- 2
  } else if (data_raw100_assessment_training$`Rubric Column`[i] == "Insufficient Evidence") {
    data_raw100_assessment_training$rubric_pts[i] <- 1
  } else if (data_raw100_assessment_training$`Rubric Column`[i] == "No Submission") {
    data_raw100_assessment_training$rubric_pts[i] <- 0
  } else if (data_raw100_assessment_training$`Rubric Column`[i] == "No Attempt") {
    data_raw100_assessment_training$rubric_pts[i] <- 0
  } else if (data_raw100_assessment_training$`Rubric Column`[i] == "Did Not Attempt") {
    data_raw100_assessment_training$rubric_pts[i] <- 0
  }  
}

## store max values for each LO-grouping
LO_group_names <- c(sort(unique(data_raw100_assessment_training$CO_ID)), 
                    sort(unique(data_raw100_assessment_training$CC_ID)))

# Max possible rubric points 
# max_pts <- tibble("CO01"  = 740, "CO02"  = 168, "CO03"  = 256, "CO04"  = 32, "CO05" = 12,
#                   "01.00" = 60,  "02.00" = 24,  "03.00" = 32,  "04.00" = 32, 
#                   "05.00" = 20,  "06.00" = 12,  "07.00" = 88,  "08.00" = 4, 
#                   "11.00" = 176, "12.00" = 116, "13.00" = 60,  "14.00" = 96, 
#                   "15.00" = 124, "16.00" = 44,  "17.00" = 64,  "18.00" = 24, 
#                   "19.00" = 32,  "20.00" = 32,  "21.00" = 156, "22.00" = 12)
# max_pts <- c("CO01", "CO02", "CO03", "CO04", "CO05", "01.00", "02.00", "03.00", "04.00", "05.00", "06.00", "07.00", "08.00", "11.00", "12.00", "13.00", "14.00", "15.00", "16.00", "17.00", "18.00", "19.00", "20.00", "21.00", "22.00")

# for (i in sort(unique(data_raw100_assessment_training$CO_ID))) {
#   max_pts[i] <- data_raw100_assessment_training %>% 
#     filter(CO_ID == i) %>%
#     select(one_of("LO_ID", "CC_ID", "CO_ID")) %>% 
#     n_distinct()
# }
# for (i in sort(unique(data_raw100_assessment_training$CC_ID))) {
#   max_pts[i] <- data_raw100_assessment_training %>% 
#     filter(CC_ID == i) %>%
#     n_distinct()
# }

## count the number of assessments and total possible (max) points in each category ####
max_pts <- tibble("LO_group" = c("CO01", "CO02", "CO03", "CO04", "CO05", 
                           "01.00", "02.00", "03.00", "04.00", "05.00", 
                           "06.00", "07.00", "08.00", "11.00", "12.00", 
                           "13.00", "14.00", "15.00", "16.00", "17.00", 
                           "18.00", "19.00", "20.00", "21.00", "22.00"),
            "assess_cnt" = NA,
            "assess_pts" = NA)

# loop through all of the learning objective group names
for (i in max_pts$LO_group) {
  if (str_detect(i, "CO")) {  #count the assessments at the CO level 
    max_pts[max_pts$LO_group==i,]$assess_cnt <- 
      length(unique(data_raw100_assessment_all$assmt_item_ID[(data_raw100_assessment_all$CO_ID==i)]))
    max_pts[max_pts$LO_group==i,]$assess_pts <- 
      length(unique(data_raw100_assessment_all$assmt_item_ID[(data_raw100_assessment_all$CO_ID==i)]))*4 
  } else {   #count the assessments at the CC level 
    max_pts[max_pts$LO_group==i,]$assess_cnt <- 
      length(unique(data_raw100_assessment_all$assmt_item_ID[(data_raw100_assessment_all$CC_ID==i)]))
    max_pts[max_pts$LO_group==i,]$assess_pts <- 
      length(unique(data_raw100_assessment_all$assmt_item_ID[(data_raw100_assessment_all$CC_ID==i)]))*4 
  }
}



## sum assessed proficiency levels for each student ####
# create place to store the values for each LO-grouping
stu_profiles_and_rubricPoints <- add_column(all_assignments, 
                "CO01"  = NA, "CO02"  = NA, "CO03"  = NA, "CO04"  = NA, "CO05" = NA,
                "01.00" = NA, "02.00" = NA, "03.00" = NA, "04.00" = NA, 
                "05.00" = NA, "06.00" = NA, "07.00" = NA, "08.00" = NA, 
                "11.00" = NA, "12.00" = NA, "13.00" = NA, "14.00" = NA, 
                "15.00" = NA, "16.00" = NA, "17.00" = NA, "18.00" = NA, 
                "19.00" = NA, "20.00" = NA, "21.00" = NA, "22.00" = NA)



# loop through all student IDs
for (i in 1:nrow(stu_profiles_and_rubricPoints)) {
  # print % complete
  print(i/nrow(stu_profiles_and_rubricPoints))
  
  # loop through all CO groups
  for (j in sort(unique(data_raw100_assessment_training$CO_ID))) {
    # filetr on the student and the LO group, extract the rubric values
    cur_rubric_pts <- data_raw100_assessment_training %>% 
      filter(`User ID` == stu_profiles_and_rubricPoints$User.ID[i]) %>% 
      filter(CO_ID == j) %>%
      pull(rubric_pts)
    
    # store the sum of the rubric values
    stu_profiles_and_rubricPoints[i, j] <- sum(cur_rubric_pts)
  }
  
  # loop through all CC groups
  for (j in sort(unique(data_raw100_assessment_training$CC_ID))) {
    # filetr on the student and the LO group, extract the rubric values
    cur_rubric_pts <- data_raw100_assessment_training %>% 
      filter(`User ID` == stu_profiles_and_rubricPoints$User.ID[i]) %>% 
      filter(CC_ID == j) %>%
      pull(rubric_pts)
    
    # store the sum of the rubric values
    stu_profiles_and_rubricPoints[i, j] <- sum(cur_rubric_pts)
  }
  # length(cur_rubric_pts)*4
}



## check profile groups sepreately ####


## stats on the profiles ####
stats_profileRubricAvg <-  stu_profiles_and_rubricPoints %>% 
        group_by(profile_name) %>% 
        summarise_at(.vars = vars('CO01':'22.00') , list(avg=mean))

stats_profileRubricSD <-  stu_profiles_and_rubricPoints %>% 
  group_by(profile_name) %>% 
  summarise_at(.vars = vars('CO01':'22.00') , list(sd=sd))

#count the number of profiles
numProfiles <- nrow(stats_profileRubricAvg)



#remove row name column
stats_profileAvgsTemp <- stats_profileRubricAvg %>% 
    arrange(profile_name) %>% 
    select(var = -"profile_name") %>% 
    mutate()

#find normalezed value for each profile vs. LO group
stats_profileAvgsNormd <- as_tibble()
for (i in 1:numProfiles) {
  cur_temp <- stats_profileAvgsTemp %>% slice(i)/(max_pts  %>% pull(assess_pts))
  stats_profileAvgsNormd <- stats_profileAvgsNormd %>% bind_rows(cur_temp)
}

#add row column back (the profile names)
stats_profileAvgsNormd <- stats_profileAvgsNormd %>% add_column(profile_name = stats_profileRubricAvg %>% 
                         arrange(profile_name) %>% 
                         select(var = "profile_name"), .before = 1)
# stats_profileRubricValues[1:5,2:26]
# (unlist(max_pts[,], use.names=FALSE))



#Save the percentage of the population present in each of the profiles as a new column
#(I'm not exactly sure where profile_names is coming from.  Must be one of the previous 4xx_ scripts)
profile_names <- profile_names %>% mutate( pctPop = count/sum(profile_names$count))

#Add an indicator about if a profile is considered prominent or not (arbitrarily, if more than 10% of the population is in a profile)
profile_names <- profile_names %>% mutate( prominantProfile = as.logical(profile_names$pctPop > .10))


#Save the stats filtered on the prominent profiles
stats_profileAvgsNormd_PProfiles <- stats_profileAvgsNormd %>% 
  filter(profile_name$var %in% 
           profile_names$short_name[profile_names$prominantProfile])
  




######### Save data to file #########
message("\nSaving files.\n")

#write to CSV file
write_csv(path = file.path("output", "420_stu_profiles_and_rubricPoints.csv"),
          x = stu_profiles_and_rubricPoints, col_names = T)
write_csv(path = file.path("output", "420_maxRubricPoints.csv"),
          x = max_pts, col_names = T)
write_csv(path = file.path("output", "420_stats_profileRubricValues.csv"),
          x = stats_profileRubricValues, col_names = T)
write_csv(path = file.path("output", "420_profile_namesCntsPctsProminence.csv"),
          x = profile_names, col_names = T)
write_csv(path = file.path("output", "420_stats_profileAvgsNormd_CO-CC.csv"),
          x = stats_profileAvgsNormd, col_names = T)
write_csv(path = file.path("output", "420_stats_profileAvgsNormd_CO-CC_PProfiles.csv"),
          x = stats_profileAvgsNormd_PProfiles, col_names = T)

#write to RData file
save(stu_profiles_and_rubricPoints, data_raw100_assessment_training, 
     max_pts, LO_group_names, profile_names,
     stats_profileRubricValues, stats_profileAvgsNormd, stats_profileAvgsNormd_PProfiles,
     file = file.path("output", "420_stu_profiles_and_rubricPoints.RData"),
     precheck = TRUE, compress = TRUE)



# #save input stack details to csv file
# write_csv(x = as.data.frame(filenameGB),
#           file = file.path("output", "420_evalStuLOs-input_stack.csv"))
