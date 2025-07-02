
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

################# Flatten LEVEL 2 ####################
dat1 <- markdata %>% filter(., workflow_id == 25162)
# Filter to the relevant workflow version. You might want to combine multiple versions; it depends on the changes that have been made to the project.
dat2 <- dat1 %>% filter(., workflow_version == 42.92) # created after 2024-07-09 23:11:08 UTC

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

flattened$tool_label <- ifelse(flattened$toolIndex ==1, "Grouper, All Species",
                            ifelse(flattened$toolIndex ==0, "King Mackerel",
                                ifelse(flattened$toolIndex ==2, "Sailfish","Other")))
                    
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

########################################################################
# output sailfish photo ids
#sailfish_photo_list <- positive_data %>%
#  filter(tool_label=='Sailfish')

#sailfish_photo_ids <- unique(sailfish_photo_list$subject_ids)
#write.csv(x = sailfish_photo_ids, file = "sailfish_photo_ids.csv")

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
write.csv(x = PUS_counts_out, file = "PUS_counts_level2.csv")
write.csv(x = positive_data, file = "Positive_marks_level2.csv")

####### examine and visulize the data
# count the number of unique users per photo
users_per_photo <- PUS_counts_out %>%
  select(subject_ids, user_name) %>%
  distinct() %>%
  group_by(subject_ids) %>%
  summarise(n_users = n())

range(users_per_photo$n_users)

ggplot(users_per_photo, aes(x = factor(n_users))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Number of Users per photo",
    x = "Number of Users",
    y = "Number of Photos"
  ) +
  theme_minimal()

# Count unique photos per user
photos_per_user <- PUS_counts_out %>%
  select(user_name, subject_ids) %>%
  distinct() %>%                          # ensure each photo is only counted once per user
  group_by(user_name) %>%
  summarise(n_photos = n(), .groups = "drop")  # count unique photos per user

ggplot(photos_per_user, aes(x = n_photos)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  scale_x_log10() +
  labs(
    title = "Histogram of Photos Reviewed per User (Log Scale)",
    x = "Number of Photos Reviewed (log10 scale)",
    y = "Number of Users"
  ) +
  theme_minimal()

# Sum total fish counted per user per species
fish_per_user_species <- PUS_counts_out %>%
  group_by(user_name, variable) %>%
  summarise(total_fish_counted = sum(value, na.rm = TRUE), .groups = "drop")

# Merge the summaries
user_species_summary <- left_join(photos_per_user_species, fish_per_user_species,
                                  by = c("user_name", "variable"))

ggplot(user_species_summary, aes(x = n_photos, y = total_fish_counted)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Photos Reviewed vs. Total Fish Counted per User (by Species)",
    x = "Number of Photos Reviewed",
    y = "Total Fish Counted"
  ) +
  geom_text(data = subset(user_species_summary, total_fish_counted > 500),
            aes(label = user_name), hjust = 1.1, vjust = 0.5, size = 3) +
  theme_minimal()

# count variation by species

#############################################################################################
##### agreement among users; the key idea is to evaluate how consistently users count the same number of fish of a given species within each photo.

# Summarize agreement per species per photo

agreement_stats <- PUS_counts_out %>%
  group_by(subject_ids, variable) %>%
  summarise(
    mean_count = mean(value, na.rm = TRUE),
    median_count = median(value, na.rm = TRUE),
    sd_count = sd(value, na.rm = TRUE),
    iqr_count = IQR(value, na.rm = TRUE),
    min_count = min(value),
    max_count = max(value),
    range_count = max(value) - min(value),
    n_users = n(),
    .groups = "drop"
  )

#### Add mode and agreement count/percentages

# Calculate total users per photo–species
total_users <- PUS_counts_out %>%
  distinct(subject_ids, variable, user_name) %>%
  group_by(subject_ids, variable) %>%
  summarise(total_users = n(), .groups = "drop")

# Calculate mode and how many users selected it
mode_summary <- PUS_counts_out %>%
  group_by(subject_ids, variable, value) %>%
  summarise(n_users = n(), .groups = "drop") %>%
  group_by(subject_ids, variable) %>%
  slice_max(n_users, n = 1, with_ties = FALSE) %>%  # Get most common value
  rename(mode_value = value, mode_count = n_users)

# Merge and compute agreement %
mode_agreement_summary <- left_join(mode_summary, total_users, by = c("subject_ids", "variable")) %>%
  mutate(agreement_percent = (mode_count / total_users) * 100) %>%
  mutate(response_type = ifelse(mode_value == 0, "Zero", "Positive"))

# Visualize — Histogram of Agreement Percent by species
ggplot(mode_agreement_summary, aes(x = agreement_percent)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    title = "User Agreement on Species Count per Photo",
    x = "Agreement Percentage",
    y = "Number of Photos"
  ) +
  theme_minimal()

species_stats <- mode_agreement_summary %>%
  group_by(variable) %>%
  summarise(
    mean_agreement = mean(agreement_percent, na.rm = TRUE),
    sd_agreement = sd(agreement_percent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("Mean ± SD:\n", round(mean_agreement, 1), " ± ", round(sd_agreement, 1), "%")
  )

ggplot(mode_agreement_summary, aes(x = agreement_percent, fill = response_type)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.4, position = "identity") +
  geom_vline(
    data = species_means,
    aes(xintercept = mean_agreement),
    color = "darkblue", linetype = "dashed", size = 0.8
  ) +
  facet_wrap(~ variable, scales = "free_y") +
  geom_text(
    data = species_stats,
    aes(x = 80, y = Inf, label = label),  # adjust x as needed, y = Inf places it at top
    vjust = 1.2, hjust = 0, inherit.aes = FALSE,
    size = 3.5
  ) +
  labs(
    title = "User Agreement on Species Count per Photo",
    x = "Agreement Percentage (mode count / total users)",
    y = "Number of Photos"
  ) +
  theme_minimal()

# Merge the stats 
agreement_stats_more <- left_join(agreement_stats, mode_agreement_summary,
                                  by = c("subject_ids", "variable"))
# Visualize variability across species
ggplot(agreement_stats_more, aes(x = variable, y = sd_count)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Variation in Count Estimates Across Species",
    x = "Species",
    y = "Standard Deviation of User Counts"
  ) +
  theme_minimal() +
  coord_flip()



photo_level_counts_long <- agreement_stats_more %>%
  select(subject_ids, variable, mean_count, median_count, mode_value) %>%
  pivot_longer(
    cols = c(mean_count, median_count, mode_value),
    names_to = "statistic",
    values_to = "count"
  )

ggplot(photo_level_counts_long, aes(x = statistic, y = count, fill = statistic)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    title = "Comparison of Mean, Median, and Mode Counts by Species (Photo Level)",
    x = "Statistic",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")














































species_agreement <- PUS_counts_out %>%
  group_by(subject_ids, variable) %>%
  summarise(
    n_users = n(),
    mean_count = mean(value),
    sd_count = sd(value),
    range_count = max(value) - min(value),
    .groups = "drop"
  )

# Step 3: Plot agreement (standard deviation) by species
ggplot(species_agreement, aes(x = tool_label, y = sd_count)) +
  geom_boxplot() +
  labs(
    title = "User Agreement by Species (Standard Deviation of Counts)",
    x = "Species",
    y = "Standard Deviation of Fish Counts per Photo"
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"))

# Add agreement flags
species_agreement <- species_agreement %>%
  mutate(
    perfect_agreement = sd_count == 0,
    high_agreement = sd_count <= 1
  )

# Agreement Percentage by Species
# First calculate total number of unique photos
total_photos <- species_agreement %>%
  summarise(n_photos = n_distinct(subject_ids)) %>%
  pull(n_photos)

# Then do the summary by species
species_summary <- species_agreement %>%
  group_by(tool_label) %>%
  summarise(
    n_total = n(),
    n_high_agreement = sum(high_agreement, na.rm = TRUE),
    percent_high_agreement = 100 * n_high_agreement / n_total,
    .groups = "drop"
  ) %>%
  mutate(n_photos = total_photos)  # repeat total for each species

species_summary

# Which species have highest disagreement?
species_agreement %>%
  group_by(tool_label) %>%
  summarise(mean_sd = mean(sd_count, na.rm = TRUE)) %>%
  arrange(desc(mean_sd))

# Plot distribution of species counts
ggplot(species_counts, aes(x = tool_label, y = species_count)) +
  geom_violin(trim = FALSE, fill = "skyblue", color = "black", alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  labs(
    title = "Distribution of Fish Counts per Species per Photo (by User)",
    x = "Species",
    y = "Number of Fish Counted"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot range of counts by species
ggplot(species_agreement, aes(x = tool_label, y = range_count)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(
    title = "Range of User-Reported Fish Counts per Species per Photo",
    x = "Species",
    y = "Range of Counts (Max - Min)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram of sd_count faceted by species
ggplot(species_agreement, aes(x = sd_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~ tool_label, scales = "free_y") +
  labs(
    title = "User Agreement by Species",
    x = "Standard Deviation of Species Count per Photo",
    y = "Number of Photos"
  ) +
  theme_minimal()

# Histogram of range_count per species
ggplot(species_agreement, aes(x = range_count)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "white") +
  facet_wrap(~ tool_label, scales = "free_y") +
  labs(
    title = "Range of User Counts per Species per Photo",
    x = "Range (Max - Min Count)",
    y = "Number of Photos"
  ) +
  theme_minimal()

#### filter 

# Step 1: Count fish per user per species per photo
species_counts <- data_out %>%
  filter(!is.na(tool_label)) %>%
  group_by(subject_ids, user_name, tool_label) %>%
  summarise(species_count = n(), .groups = "drop")

# Step 2: Flag outliers using IQR per photo × species group
species_counts_filtered <- species_counts %>%
  group_by(subject_ids, tool_label) %>%
  mutate(
    Q1 = quantile(species_count, 0.25),
    Q3 = quantile(species_count, 0.75),
    IQR = Q3 - Q1,
    is_outlier = species_count < (Q1 - 1.5 * IQR) | species_count > (Q3 + 1.5 * IQR)
  ) %>%
  ungroup() %>%
  filter(!is_outlier)  # remove outliers

outliers <- species_counts %>%
  anti_join(species_counts_filtered, by = c("subject_ids", "user_name", "tool_label"))

# Compute SD of fish counts per photo × species (after filtering)
agreement_after <- species_counts_filtered %>%
  group_by(subject_ids, tool_label) %>%
  summarise(sd_after = sd(species_count), .groups = "drop")

# Compute average SD per species
avg_sd_by_species <- agreement_after %>%
  group_by(tool_label) %>%
  summarise(
    mean_sd_after = mean(sd_after, na.rm = TRUE),
    median_sd_after = median(sd_after, na.rm = TRUE),
    n_photos = n()
  ) %>%
  arrange(mean_sd_after)

# View result
print(avg_sd_by_species)

### compare before and after filtering 
agreement_before <- species_counts %>%
  group_by(subject_ids, tool_label) %>%
  summarise(
    sd_before = sd(species_count),
    range_before = max(species_count) - min(species_count),
    n_users_before = n(),
    .groups = "drop"
  )

agreement_after <- species_counts_filtered %>%
  group_by(subject_ids, tool_label) %>%
  summarise(
    sd_after = sd(species_count),
    range_after = max(species_count) - min(species_count),
    n_users_after = n(),
    .groups = "drop"
  )

# Add agreement flags
agreement_after <- agreement_after %>%
  mutate(
    perfect_agreement = sd_after == 0,
    high_agreement = sd_after <= 1
  )

agreement_comparison <- agreement_before %>%
  full_join(agreement_after, by = c("subject_ids", "tool_label")) %>%
  mutate(
    sd_change = sd_before - sd_after,
    range_change = range_before - range_after
  )

ggplot(agreement_comparison, aes(x = sd_change)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  labs(
    title = "Change in SD of Fish Counts After Outlier Filtering",
    x = "SD Before - SD After",
    y = "Number of Photo × Species Combinations"
  ) +
  theme_minimal()

agreement_comparison <- agreement_before %>%
  full_join(agreement_after, by = c("subject_ids", "tool_label")) %>%
  mutate(sd_change = sd_before - sd_after)

ggplot(agreement_comparison, aes(x = sd_before, y = sd_after)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Comparison of Standard Deviation Before and After Outlier Filtering",
    x = "SD Before Filtering",
    y = "SD After Filtering"
  ) +
  theme_minimal()

# Agreement Percentage by Species
# First calculate total number of unique photos
total_photos <- agreement_after %>%
  summarise(n_photos = n_distinct(subject_ids)) %>%
  pull(n_photos)

# Then do the summary by species
species_summary <- agreement_after %>%
  group_by(tool_label) %>%
  summarise(
    n_total = n(),
    n_high_agreement = sum(high_agreement, na.rm = TRUE),
    percent_high_agreement = 100 * n_high_agreement / n_total,
    .groups = "drop"
  ) %>%
  mutate(n_photos = total_photos)  # repeat total for each species

species_summary

############### 
library(dplyr)
library(purrr)
library(dbscan)

# Parameters for DBSCAN
eps <- 10      # Distance threshold in coordinate units
minPts <- 5    # Minimum number of marks to form a cluster

# Apply DBSCAN clustering by subject_ids and tool_label
clustered_data <- data_out %>%
  filter(!is.na(xcoord), !is.na(ycoord), !is.na(tool_label)) %>%
  group_by(subject_ids, tool_label) %>%
  group_split() %>%
  map_dfr(function(df) {
    coords <- df %>% select(xcoord, ycoord)
    
    # Handle cases with too few marks to form a cluster
    if (nrow(coords) < minPts) {
      df$cluster_id <- NA
      return(df)
    }
    # Run DBSCAN
    clustering <- dbscan(coords, eps = eps, minPts = minPts)
    
    # Add cluster ID (0 = noise)
    df$cluster_id <- clustering$cluster
    df
  })

# Count Number of agreement clusters per photo and species
cluster_summary <- clustered_data %>%
  filter(cluster_id > 0) %>%
  group_by(subject_ids, tool_label, cluster_id) %>%
  summarise(n_users = n_distinct(user_name), .groups = "drop")

top_clusters <- cluster_summary %>%
  group_by(subject_ids, tool_label) %>%
  slice_max(order_by = n_users, n = 1, with_ties = FALSE)

# Example: visualize one photo (change subject_ids to your target photo)
clustered_data %>%
  filter(subject_ids == 46488482) %>%
  ggplot(aes(x = xcoord, y = ycoord, color = factor(cluster_id))) +
  geom_point(size = 3, alpha = 0.7) +
  facet_wrap(~tool_label) +
  coord_fixed() +
  labs(
    title = "User Mark Clusters by Species",
    subtitle = "Photo ID: 46488482",
    x = "X Coordinate", y = "Y Coordinate",
    color = "Cluster ID"
  ) +
  theme_minimal()

clustered_data %>%
  filter(subject_ids == 46488482) %>%
  ggplot(aes(x = xcoord, y = ycoord)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", contour = TRUE, alpha = 0.6) +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(
    title = "Agreement Heatmap for Photo 46488482",
    x = "X Coordinate", y = "Y Coordinate", fill = "Density"
  ) +
  theme_minimal()

clustered_data %>%
  filter(subject_ids == 46488482) %>%
  ggplot(aes(x = xcoord, y = ycoord)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.4) +
  geom_point(aes(color = factor(cluster_id)), size = 2) +
  scale_color_brewer(palette = "Dark2", na.value = "gray") +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(
    title = "Heatmap + Clusters: Photo 46488482",
    color = "Cluster ID", fill = "Density"
  ) +
  theme_minimal()


#  Count users per cluster
high_confidence_clusters <- clustered_data %>%
  filter(!is.na(cluster_id), cluster_id > 0) %>%
  group_by(subject_ids, tool_label, cluster_id) %>%
  summarise(n_users = n_distinct(user_name), .groups = "drop")

# Filter clusters with strong agreement (e.g., ≥ 3 users)
high_confidence_clusters <- high_confidence_clusters %>%
  filter(n_users >= 3)

# Join back to original data to extract full rows (coordinates, etc.)
high_confidence_data <- clustered_data %>%
  inner_join(high_confidence_clusters, 
             by = c("subject_ids", "tool_label", "cluster_id"))

# How many strong clusters per photo/species?
high_confidence_data %>%
  group_by(subject_ids, tool_label) %>%
  summarise(
    n_high_conf_clusters = n_distinct(cluster_id),
    max_agreement_users = max(n_users),
    .groups = "drop"
  ) %>%
  arrange(desc(max_agreement_users))


# To compute per-photo agreement scores broken down by species, 
# calculate — for each photo–species combination (subject_ids + tool_label) 
# the proportion of user marks that fall into spatial agreement clusters (cluster_id > 0).
photo_species_agreement <- clustered_data %>%
  filter(!is.na(tool_label)) %>%
  group_by(subject_ids, tool_label) %>%
  summarise(
    total_marks = n(),
    agreed_marks = sum(cluster_id > 0, na.rm = TRUE),
    agreement_score = agreed_marks / total_marks,
    n_users = n_distinct(user_name),
    .groups = "drop"
  ) %>%
  arrange(desc(agreement_score))

library(ggplot2)

ggplot(photo_species_agreement, aes(x = tool_label, y = agreement_score)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip() +
  labs(
    title = "Per-Photo Agreement Scores by Species",
    x = "Species",
    y = "Agreement Score"
  ) +
  theme_minimal()










