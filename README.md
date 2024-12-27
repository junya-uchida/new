# new
file_names1 <- c("subject_train.txt", "y_train.txt", "X_train.txt")
file_names2 <- c("subject_test.txt", "y_test.txt", "X_test.txt" )

### Checking structures of variables of interest
subject_train <- read.table("subject_train.txt")
str(subject_train)
colnames(subject_train) <- "identify"
#subject_train has 7352 rows and 1 variable

X_train <- read.table("X_train.txt")
str(X_train)
#X_train has 7352 rows and 561 varaiables

y_train <- read.table("y_train.txt")
str(y_train)
colnames(y_train) <- "activity"
#y_train has 7352 rows and 1 variable

subject_test <- read.table("subject_test.txt")
str(subject_test)
colnames(subject_test) <- "identify"
#subject_test has 2947 rows and 1 variable

X_test <- read.table("X_test.txt")
str(X_test)
#X_train has 2947 rows and 561 varaiables

y_test <- read.table("y_test.txt")
str(y_test)
colnames(y_test) <- "activity"
#y_test has 2947 rows and 1 variable

combined_train <- cbind(subject_train, y_train, X_train)
str(combined_train)
combined_test <- cbind(subject_test, y_test, X_test)
str(combined_test)
combined_data <- rbind(combined_train, combined_test)

nrow(combined_data)
str(combined_data)
tail(combined_data)
dataset <- combined_data
str(dataset)
head(dataset)

library(dplyr)
dataset <- dataset %>%
  mutate(activity = as.integer(activity)) %>%  # Ensure 'activity' is treated as integer
  arrange(identify, activity)


grouped_data <- dataset %>% 
  arrange(identify, activity)

library(tidyr)

dataset <- dataset %>%
  rowwise() %>%                       # Process each row
  mutate(mean_Vs = mean(c_across(starts_with("V")))) %>%
  ungroup() 

# Drop crude data and select only necessary variables
dataset_final <- dataset %>%
  select(identify, activity, mean_Vs)

colnames(dataset_final) <- c("id", "activity", "Scores")
head(dataset_final)
str(dataset_final)

# Draw means and sds from the variable named Scores above
# Develop mean and sd for each activity for each participant
dataset_w_measurements <- dataset_final %>% 
  group_by(id, activity) %>% 
  summarize(mean=mean(Scores),
            sd=sd(Scores)) %>% 
  ungroup()

head(dataset_w_measurements)
str(dataset_w_measurements)

# Create a tidy data with participants being in rows and means/sds of activities in columns
wide_data <-dataset_w_measurements %>%
  pivot_wider(
    names_from = activity,
    values_from = c(mean, sd),
    names_sep = "_"
  )

# Name all the variables

### activity labels
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING

head(wide_data)
colnames(wide_data) <- c("id", 
                         "mean_WALKING",
                         "mean_WALKING_UPSTAIRS",
                         "mean_WALKING_DOWNSTAIRS",
                        "mean_WALKING_DOWNSTAIRS",
                         "mean_SITTING",
                         "mean_LAYING",
                         "sd_WALKING",
                         "sd_WALKING_UPSTAIRS",
                         "sd_WALKING_DOWNSTAIRS",
                         "sd_WALKING_DOWNSTAIRS",
                         "sd_SITTING",
                         "sd_LAYING")
tidy_data <- wide_data
head(tidy_data)
str(tidy_data)

# Write an independent tidy data
write.table(tidy_data, "tidy_table.txt", row.name = FALSE)
