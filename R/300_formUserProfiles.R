## ===================================================== ##
# Title:        Form user profiles ####
# Project:      The Boeing project. Data is from four Boeing courses - B1 through
#               B4. The aim is model usage trajectories using hidden Markov
#               models and use the RP1D method to project the HMM parameter
#               space onto a set of optimal directions in the hope of finding
#               typical usage 'profiles'.
#               
# Authors:      Doipayan Roy (principal author), Taylor Williams (made modifications
#               to fix a bug and export important data)
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

##Read file with W values for projection directions
W_values <-  read.csv(paste0(path_output, "minW_Threshold_", course, ".csv"), row.names = 1)

##Read file containing projection directions
projection_directions <- read.csv(paste0(path_output, "randomVectors_",
                                         course, ".csv"), row.names = 1)

# ##Read file containing HMM parameters for users
# probMatrix <- read.csv(paste0(path_output, "hmmParameters_", course, ".csv"), row.names = 1)
# ##Load the courseware_studentModule.sql file
# ##This file contains the list of all student_ids who have accessed atleast one module in the course
# courseware_studModule <- read_tsv(paste0(path_files, "courseware_studModule_", course, ".sql"))
# courseware_studModule <- as.data.frame(courseware_studModule)
# ##Number of students who have had any access activity in course. Required for calculating percentage of students in profiles
# n_learners <- length(unique(courseware_studModule$student_id))
n_learners <- 1000  #TODO(fix this)

##Take the number of best directions to consider for profiling
n_directions <- readline(prompt = "Please enter the number of best projection directions to consider for profiling : \n")
n_directions <- as.integer(n_directions)

##Extract the names of the best projection directions
best_direction_names <- rownames(W_values)[1:n_directions]
best_direction_names <- gsub(x = best_direction_names, pattern = "RP1D", replacement = "RV")

##Form a dataFrame of n_directions columns and column = i is ith best projection direction
best_directions <- c()
for(i in 1:n_directions)
{
  best_directions <- cbind(best_directions, projection_directions[,best_direction_names[i]])
}
best_directions <- as.data.frame(best_directions)
##Name columns using names of corresponding directions
colnames(best_directions) <- best_direction_names

##Form a list of W values for best projection directions
best_direction_thresholds <- W_values[1:n_directions, "Group.Threshold"]

##Loop over learners and find cluster assignment along each best direction
cluster_assignments <- c()
for(i in 1:nrow(probMatrix))
{
  ##User parameter vector
  user_params <- as.matrix(probMatrix[i,])
  cluster <- c()
  for(j in 1:n_directions)
  {
    ##Calculate the projection value along direction j
    proj <- as.numeric(user_params %*% best_directions[,j])
    ##If proj > threshold, cluster = 2. Else, cluster = 1
    if(proj > best_direction_thresholds[j])
    {
      cluster <- c(cluster, 2)
    } else
    {
      cluster <- c(cluster, 1)
    }
  }
  ##Save cluster assignment of each learner by row
  cluster_assignments <- rbind(cluster_assignments, cluster)
}

##Convert cluster_assignment to dataFrame, name rows with corresponding user_id and columns with corresponding direction
cluster_assignments <- as.data.frame(cluster_assignments)
rownames(cluster_assignments) <- rownames(probMatrix)
colnames(cluster_assignments) <- paste0("cluster_", best_direction_names)

##Initialize a dataFrame for saving the profiles of every user
all_assignments <- c()
all_assignments$student_id <- rownames(cluster_assignments)
all_assignments$profile <- rep(NA, length(all_assignments$student_id))

##Convert cluster assignments to strings of length = n_directions. These strings are usage profiles
cluster_assignments_collapsed <- c()
for(i in 1:nrow(cluster_assignments))
{
  cluster_assignments_collapsed <- c(cluster_assignments_collapsed, 
                                     paste0(as.character(cluster_assignments[i, ]), collapse = ""))
}
##Convert all_assignments to dataFrame
all_assignments <- as.data.frame(all_assignments)
##Find set of all profiles observed in course
unique_assignments <- unique(unique_assignments)

##Now, count the number of times each profiles occurs...
unique_assignment_counts <- c()
temp <- c()
for(i in 1:length(unique_assignments))
{
  temp <- c(temp, sum(cluster_assignments_collapsed == unique_assignments[i]))
}

##Form a dataFrame containing profiles, number and percentage of learners in a profile 
unique_assignment_counts$Cluster.Assignment <- unique_assignments
unique_assignment_counts$Number.Students <- temp
##Calculate percentage of learners for each profile
unique_assignment_counts$Percentage.Students <- (temp / n_learners) * 100
##Convert list to dataFrame and sort profiles in decreasing order of Percentage.Students
unique_assignment_counts <- as.data.frame(unique_assignment_counts)
unique_assignment_counts <- unique_assignment_counts[order(unique_assignment_counts$Number.Students, decreasing = T), ]

##Save dataFrame of profiles and percentage students in each profile
write.csv(unique_assignment_counts, paste0(path_output, "profileDistribution_", course, ".csv"))


##Save dataFrame of cluster assignments of individual users
write.csv(cluster_assignments, paste0(path_output, "clusterAssignmentsAllUsers_", course, ".csv"))
write.csv(cluster_assignments_collapsed, paste0(path_output, "clusterAssignmentsAllUsersCollapsed_", course, ".csv"))

