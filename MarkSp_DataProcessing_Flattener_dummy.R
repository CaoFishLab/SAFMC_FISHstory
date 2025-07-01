#Code Location (Live Data): CitizenScience/FISHstory/DATAWAREHOUSE/LiveData/Unflattened
#Code Location (VT Data): CitizenScience/FISHstory/DATAWAREHOUSE/ValidationTeamData/Unflattened

#OutPut File name (Live Data): FISHstoryZooData_marksp_flattened
#Output file name (VT Data): FISHstoryVTData_marksp_flattened

#Output File Location (Live Data): CitizenScience/FISHstory/DATAWAREHOUSE/LiveData/Flattened
#Output File Location (VT Data): CitizenScience/FISHstory/DATAWAREHOUSE/ValidationTeamData/Flattened

##REMEMBER - DO NOT OPEN THE SPREADSHEET AFTER DOWNLOAD, DRAG IT TO THE FILE FOLDER AND RUN THE CODE! 
rm(list = ls())
###### REMEMBER TO CHANGE THE DIRECTORY #############

setwd("/Users/jcao22/Library/CloudStorage/GoogleDrive-souketu@gmail.com/My Drive/WORK_NCSU/Research_NCSU_active/FISHstory_Materials_Jie/Zooniverse/Alex_Files/Fishstory/Zooniverse_Results")

#devtools::install_github("sailthru/tidyjson")

library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(roperators)
library(lubridate)
library(ggplot2)

source("Zoo_Functions.R")

# Note you'll want to set working directory as appropriate.
markdata <- read.csv("dummy-data-workflows-classifications.csv", stringsAsFactors = F)

# Filter to the relevant workflow
# FISH: Classify (#13226) 
# FISH & PEOPLE: Count (#14513)
# LEVEL 1: Count Fish and People (#25161)
# LEVEL 2: Classify King Mackerel, Grouper & Sailfish (#25162)
# LEVEL 3: Classify Red Snapper, Amberjack & Sharks (#25386)
# LEVEL 4: Classify Dolphinfish, Cobia, Gray Triggerfish & Little Tunny (#25364)
# Level 1 Dummy (#29194)
# Level 2 Dummy (#29197)
# Level 3 Dummy (#29198)
# Level 4 Dummy (#29200)

dat1 <- markdata %>% filter(., workflow_id == 29200)
# Filter to the relevant workflow version. You might want to combine multiple versions; it depends on the changes that have been made to the project.
#dat2 <- dat1 %>% filter(., workflow_version == 18.39) # created after 2024-07-09 23:11:08 UTC
dat2 <- dat1

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

################################
# USE FOR THE RIGHT LEVEL
# flattened$tool_label <- ifelse(flattened$tool ==1, "Grouper, All Species",
#                                ifelse(flattened$tool ==0, "King Mackerel",
#                                       ifelse(flattened$tool ==2, "Sailfish","Other")))

# flattened$tool_label <- ifelse(flattened$tool ==1, "Amberjack",
#                                ifelse(flattened$tool ==0, "Red Snapper",
#                                       ifelse(flattened$tool ==2, "Shark","Other")))

flattened$tool_label <- ifelse(flattened$tool ==1, "Cobia",
                               ifelse(flattened$tool ==0, "Dolphin",
                                      ifelse(flattened$tool ==2, "Triggerfish",
                                             ifelse(flattened$tool ==3, "Little Tunny","Other"))))


################################

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
datameltedtar <- cbind.data.frame(subject_ids=datameltedtarget$subject_ids, user_num3=datameltedtarget$user_name,
                                  class_id= datameltedtarget$classification_id,
                                  variable=datameltedtarget$variable, value=datameltedtarget$value)

###### only for LEVEL TWO, COMMENT IT OUT FOR OTHER LEVELS
#data_out$tool_label <- ifelse(data_out$tool ==1, "People",
#                               ifelse(data_out$tool ==0, "Fish","Other"))
#######################################################


#write.csv(x = data_out, file = "FISHstoryZooData_marksp_flattened_level2_6/24/25.csv")


summary_df <- data_out %>%
  group_by(subject_ids, user_name, tool_label) %>%
  summarise(
    n = n(),             # number of rows = number of fish annotations
    .groups = "drop"
  )









