###### DRAFT SPACE #####



#### working from 110-v2 #####
# users from gradebook
stuList_gradebook <- tibble("User ID" = stu_sections$`User ID`)

data <- data_raw3_assessment_all

# count the number of recorded assessments for each user in the data (should match `unique_items`)
stuList_BB <- data %>% group_by(`User ID`) %>% summarise(n())

# students who have recorded assmts but aren't (or are) in gradebook
stu_withdrawals <- anti_join(x = stuList_BB, y = stuList_gradebook, by = "User ID")
stu_completers  <- semi_join(x = stuList_BB, y = stuList_gradebook, by = "User ID")

# std dev and mean number of LOAs in data and subgroups
stats::sd(stuList_BB$`n()`); mean(stuList_BB$`n()`)
stats::sd(stu_completers$`n()`); mean(stu_completers$`n()`)
stats::sd(stu_withdrawals$`n()`); mean(stu_withdrawals$`n()`)


LOAs_rrow <- data %>% group_by(`Rubric Row`) %>% summarise(n())


data_raw4 <- data %>% 
  mutate(assmt_item_ID = paste(`Rubric Title`, "---", `Rubric Row`))

cnt_LOAssmts <- data_raw4 %>% group_by(`assmt_item_ID`) %>% summarise(n())
  # [] investigate: all the counts should be identical, but even in the first 10 items I'm seeing 3 different values (two close and one about double)


#Find the student list that matches problematic assessment counts
cnt_LOAssmts$assmt_item_ID[32]
