###### DRAFT SPACE #####

StatSummary <- function(sumList) {
  print(paste0(deparse(substitute(sumList)), " stats"))
  print(paste0("sd: ", sd(sumList)))
  print(paste0("mean: ", mean(sumList)))
  print(paste0("max: ", max(sumList)))
  print(paste0("min: ", min(sumList)))
}

require(tidyverse)
require(readxl)
data_usingTableau <- read_csv(#file = file.choose(),
  file = "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\3_Dissertation (TW DB)\\132 data + info\\_2 cleaned data\\132 data with missing rows added (2020.03.04 in Tableau).csv")


#### working from 110-v2 #####
# users from gradebook
stuList_gradebook <- tibble("User ID" = stu_sections$`User ID`)

# data <- data_raw060v2.1
data <- data_usingTableau

# count the number of recorded assessments for each user in the data (should match `unique_items`)
stuList_BB <- data %>% group_by(`User ID`) %>% summarise(n())
stuList_orig <- data_raw_orig %>% group_by(`User ID`) %>% summarise(n())

# students who have recorded assmts but aren't (or are) in gradebook
stu_withdrawals <- anti_join(x = stuList_BB, y = stuList_gradebook, by = "User ID")
stu_completers  <- semi_join(x = stuList_BB, y = stuList_gradebook, by = "User ID")

# std dev and mean number of LOAs in data and subgroups

StatSummary(stuList_BB$`n()`)
StatSummary(stuList_orig$`n()`)
StatSummary(stu_completers$`n()`)
StatSummary(stu_withdrawals$`n()`)


LOAs_rrow <- data %>% group_by(`Rubric Row`) %>% summarise(n())
StatSummary(LOAs_rrow$`n()`)



# data_raw4 <- data %>% 
#   mutate(assmt_item_ID = paste(`Rubric Title`, "---", `Rubric Row`))

cnt_LOAssmts <- data %>% group_by(`assmt_item_ID`) %>% summarise(n())
  # [] investigate: all the counts should be identical, but even in the first 10 items I'm seeing 3 different values (two close and one about double)
StatSummary(cnt_LOAssmts$`n()`)


#Find the student list that matches problematic assessment counts
cnt_LOAssmts$assmt_item_ID[32]



####Searching for which LO items are duplicated in the raw data ####
dup <- duplicated(data_raw060v2.1[data_raw060v2.1$`User ID`=="engr132_Sp18Stu_115147", ]$assmt_item_ID)
dup2 <- duplicated(data_raw060v2.1[data_raw060v2.1$`User ID`=="engr132_Sp18Stu_115147", ]$assmt_item_ID, fromLast = T)
x <- (data_raw060v2.1[data_raw060v2.1$`User ID`=="engr132_Sp18Stu_115147", ][(dup | dup2),])


require(lubridate)


