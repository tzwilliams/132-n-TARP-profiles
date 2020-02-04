

spliter <- runif(length(student_IDs$user_ID), 
                min = 0, 
                max = 1)
training <- student_IDs[spliter <= 0.80, ]
verification <- student_IDs[spliter < 0.20, ]
view(training)



write.csv(x = training, file = file.choose())
write.csv(x = verification, file = file.choose())
