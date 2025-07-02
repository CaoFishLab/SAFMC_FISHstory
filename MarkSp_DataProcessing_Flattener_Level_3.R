#Code Location (Live Data): CitizenScience/FISHstory/DATAWAREHOUSE/LiveData/Unflattened
#Code Location (VT Data): CitizenScience/FISHstory/DATAWAREHOUSE/ValidationTeamData/Unflattened

#OutPut File name (Live Data): FISHstoryZooData_marksp_flattened
#Output file name (VT Data): FISHstoryVTData_marksp_flattened

#Output File Location (Live Data): CitizenScience/FISHstory/DATAWAREHOUSE/LiveData/Flattened
#Output File Location (VT Data): CitizenScience/FISHstory/DATAWAREHOUSE/ValidationTeamData/Flattened

##REMEMBER - DO NOT OPEN THE SPREADSHEET AFTER DOWNLOAD, DRAG IT TO THE FILE FOLDER AND RUN THE CODE! 
rm(list = ls())
###### REMEMBER TO CHANGE THE DIRECTORY #############
#setwd("/Users/jcao22/Library/CloudStorage/GoogleDrive-souketu@gmail.com/My Drive/WORK_NCSU/Research_NCSU_active/FISHstory_Materials_Jie/Zooniverse/Alex_Files/Fishstory/Zooniverse_Results")

#devtools::install_github("sailthru/tidyjson")

librarian::shelf(tidyjson,magrittr,jsonlite,dplyr,stringr,tidyr,roperators,lubridate,ggplot2,reshape2)

source("Zoo_Functions.R")

# Note you'll want to set working directory as appropriate.
markdata <- read.csv("fishstory-classifications_06_13_25.csv", stringsAsFactors = F)

# Filter to the relevant workflow
# FISH: Classify (#13226) 
# FISH & PEOPLE: Count (#14513)
# LEVEL 1: Count Fish and People (#25161)
# LEVEL 2: Classify King Mackerel, Grouper & Sailfish (#25162)
# LEVEL 3: Classify Red Snapper, Amberjack & Sharks (#25386)
# LEVEL 4: Classify Dolphinfish, Cobia, Gray Triggerfish & Little Tunny (#25364)

################# Flatten LEVEL 3 ####################
dat1 <- markdata %>% filter(., workflow_id == 25386)
# Filter to the relevant workflow version. You might want to combine multiple versions; it depends on the changes that have been made to the project.
dat2 <- dat1 %>% filter(., workflow_version == 2.16) # created after 2024-05-14 14:53:02 UTC

# Check to make sure you have the right workflow.
#check_workflow(dat2)
#View_json(dat2, length(dat2$annotations))

# Grab the top-level info for ALL classifications
# produces one row per classification per subject; final column indicates how many x-y coordinates were made in that classification.
all_submissions <- dat2 %>% 
  select(., subject_ids, classification_id, user_name, workflow_id, workflow_version, created_at, annotations) %>%
  as.tbl_json(json.column = "annotations") %>%
  gather_array(column.name = "task_index") %>%
  spread_values(task = jstring("task"), task_label = (jstring("task_label"))) %>%
  subset(.,.$task!="T1") %>% # 
  gather_object(column.name = 'key') %>%
  json_lengths(column.name = "total_marks") %>% 
  filter(., key == "value") 

# produces one row per mark per classification per subject, but only keeps classifications with >0 marks
flattened <- dat2 %>% 
  select(., subject_ids, classification_id, user_name, workflow_id, workflow_version, created_at, annotations) %>%
  as.tbl_json(json.column = "annotations") %>%
  gather_array(column.name = "task_index") %>%
  spread_values(task = jstring("task"), task_label = jstring("task_label")) %>%
  subset(.,.$task!="T1") %>% 
  enter_object("value") %>%
  gather_array() %>% #don't gather keys, whole point is that you are spreading out the damn keys.
  spread_values(xcoord = jnumber("x"), ycoord = jnumber("y"), tool = jstring("tool"), toolIndex= jstring("toolIndex")) 

flattened$tool_label <- ifelse(flattened$toolIndex ==1, "Amberjack",
                            ifelse(flattened$toolIndex ==0, "Red Snapper",
                                ifelse(flattened$toolIndex ==2, "Shark","Other")))
                    
subject <- dat2 %>% 
  select(., subject_ids, classification_id, user_name, workflow_id, workflow_version, created_at, subject_data) %>%
  as.tbl_json(json.column = "subject_data") %>%
  spread_values(retired = jstring("retired"), Filename = jstring("Filename")) %>%
  spread_values(tool_label = jstring("tool_label"), tool = jstring("tool"), value = jstring("value"))

# recombine datasets: merge flat and empty (okay, do a full + meaty join)
# all_submissions - has one record per classification per subject
# flattened has one record per mark per classification, but only if the counter >0

tot <- left_join(all_submissions, flattened) 
#tot2 <- full_join(tot, difficulty)

data_out <- tot %>% 
  mutate(., task_label = str_trunc(task_label, width = 25)) %>%
  select(., -task_index, -key)

################# issue about NA ######
# count the number of NA values in the tool_label column grouped by subject_ids and user
# check if a user has duplicated zero pic 
data_out %>%
  group_by(subject_ids, user_name) %>%
  summarize(n_na_tool_label = sum(is.na(tool_label)), .groups = "drop") %>%
  filter(n_na_tool_label > 1)

# Step 1: Identify combinations with n_na_tool_label > 1
bad_combos <- data_out %>%
  group_by(subject_ids, user_name) %>%
  summarize(n_na_tool_label = sum(is.na(tool_label)), .groups = "drop") %>%
  filter(n_na_tool_label > 1)

# Step 2: Remove those combinations from the original data (ONE row for NA with no mark fish per photo per user)
cleaned_data <- data_out %>%
  anti_join(bad_combos, by = c("subject_ids", "user_name"))

data_out <- cleaned_data

# check again if a user has duplicated zero after cleaning
data_out %>%
  group_by(subject_ids, user_name) %>%
  summarize(n_na_tool_label = sum(is.na(tool_label)), .groups = "drop") %>%
  filter(n_na_tool_label > 1)

data_out$counter <- 1

## This helps to create a 0 for each individual that was not identified in the dataset
dataforcast <- dcast(
  data_out,                                 # Input data frame
  subject_ids + user_name + classification_id ~ tool_label,  # Rows stay as combinations of these columns; columns are spread based on 'tool_label'
  value.var = "counter",                      # The values to be filled in the new wide-format columns
  fun.aggregate = sum                         # In case of duplicates, values will be summed
)

## Transforming back to column for easier analysis by species
datameltedtarget <- melt(dataforcast, id.vars = c("subject_ids", "user_name", "classification_id"))
datameltedtarget$tool_label <- datameltedtarget$variable
datameltedtar <- cbind.data.frame(subject_ids=datameltedtarget$subject_ids, user_name=datameltedtarget$user_name,
                                  class_id= datameltedtarget$classification_id,
                                  variable=datameltedtarget$variable, value=datameltedtarget$value)

# remove NA in datameltedtar
datameltedtar <- datameltedtar %>%
  filter(variable!='NA')

# positive_data - removed all NA; ind by ind mark data; 0s are in 'datameltedtar'
positive_data <- cleaned_data %>% 
  filter(!is.na(tool_label))

######################################
head(datameltedtar)
head(positive_data)

# count the number of unique users per photo
users_per_photo <- datameltedtar %>%
  select(subject_ids, user_name) %>%
  distinct() %>%
  group_by(subject_ids) %>%
  summarise(n_users = n())
range(users_per_photo$n_users)

# check photo-user-species combination data, see duplicates with different class_id
datameltedtar %>%
  group_by(user_name, subject_ids) %>%
  filter(n() != 3) %>%
  arrange(user_name, subject_ids)

# how many affected photos (duplications with different class_id)
datameltedtar %>%
  group_by(user_name, subject_ids) %>%
  filter(n() != 3) %>%
  arrange(user_name, subject_ids) %>%
  distinct() %>%
  group_by(subject_ids) %>%
  summarise(n_class.ids = n())

# remove affected photos for photo-user-species (PUS) combination count data
PUS_counts_out <- datameltedtar %>%
  group_by(user_name, subject_ids) %>%
  filter(n() == 3)

# double check duplicate classifications 
PUS_counts_out %>%
  count(user_name, subject_ids, value) %>%
  filter(n > 3)

# double check NAs 
PUS_counts_out %>%
  filter(is.na(value))

# output two datasets, i.e., PUS_counts_out and mark by mark data with no zeros (positive_data)
write.csv(x = PUS_counts_out, file = "PUS_counts_level3.csv")
write.csv(x = positive_data, file = "Positive_marks_level3.csv")

