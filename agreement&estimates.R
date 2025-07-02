
rm(list = ls())
librarian::shelf(dplyr,ggplot2,purrr,dbscan,forcats)

# read in all levels data
PUS_counts_level1 <- read.csv('PUS_counts_level1.csv')
PUS_counts_level2 <- read.csv('PUS_counts_level2.csv')
PUS_counts_level3 <- read.csv('PUS_counts_level3.csv')
PUS_counts_level4 <- read.csv('PUS_counts_level4.csv')
Positives_level1 <- read.csv('Positive_marks_level1.csv')
Positives_level2 <- read.csv('Positive_marks_level2.csv')
Positives_level3 <- read.csv('Positive_marks_level3.csv')
Positives_level4 <- read.csv('Positive_marks_level4.csv')

PUS_counts_out <- rbind(PUS_counts_level2,PUS_counts_level3,PUS_counts_level4,PUS_counts_level1)
Positives_marks <- rbind(Positives_level2,Positives_level3,Positives_level4,Positives_level1)

head(PUS_counts_out)
####################################################################
# count the number of unique users per photo per species
users_per_photo <- PUS_counts_out %>%
  select(subject_ids, user_name, variable) %>%
  distinct() %>%
  group_by(subject_ids, variable) %>%
  summarise(n_users = n_distinct(user_name), .groups = "drop")

range(users_per_photo$n_users)

ggplot(users_per_photo, aes(x = factor(n_users))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Number of Users per photo per species",
    x = "Number of Users",
    y = "Number of Photos"
  ) +
  theme_minimal()
####################################################################
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
####################################################################
# Sum total fish counted per user per species
fish_per_user_species <- PUS_counts_out %>%
  group_by(user_name, variable) %>%
  summarise(total_fish_counted = sum(value, na.rm = TRUE), .groups = "drop")

photos_per_user_species <- PUS_counts_out %>%
  select(subject_ids, user_name, variable) %>%
  distinct() %>%
  group_by(user_name, variable) %>%
  summarise(n_photos = n(), .groups = "drop")

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

####################################################################
species_stats2 <- mode_agreement_summary %>%
  group_by(variable, response_type) %>%
  summarise(
    mean_agreement = mean(agreement_percent, na.rm = TRUE),
    sd_agreement = sd(agreement_percent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(round(mean_agreement, 1), " ± ", round(sd_agreement, 1), "%")
  )
print(species_stats2)
ggplot(species_stats2, aes(x = response_type, y = mean_agreement, fill = response_type)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_agreement - sd_agreement, ymax = mean_agreement + sd_agreement),
    width = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.6),
    vjust = -0.5,  # adjust for spacing above the bar
    size = 3
  ) +
  facet_wrap(~ variable) +
  scale_fill_manual(
    values = c("Positive" = "#1b9e77", "Zero" = "#d9d9d9")  # custom colors
  ) +
  labs(
    title = "Mean ± SD of Agreement Percent by Species and Response Type",
    x = "Response Type",
    y = "Agreement Percentage"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

####################################################################
# Reorder variable so "Fish" and "People" are last
mode_agreement_summary$variable <- forcats::fct_relevel(
  mode_agreement_summary$variable,
  setdiff(unique(mode_agreement_summary$variable), c("Fish", "People")),
  "Fish", "People"
)
# Also reorder species_stats to match (if used in facet-related geoms)
species_stats$variable <- factor(
  species_stats$variable,
  levels = levels(mode_agreement_summary$variable)
)
ggplot(mode_agreement_summary, aes(x = agreement_percent, fill = response_type)) +
  geom_histogram(
    binwidth = 5, 
    color = "black", 
    alpha = 0.4, 
    position = "identity"
  ) +
  geom_vline(
    data = species_stats,
    aes(xintercept = mean_agreement),
    color = "darkblue", 
    linetype = "dashed", 
    size = 0.8
  ) +
  facet_wrap(~ variable, scales = "free_y") +
  geom_text(
    data = species_stats,
    aes(x = 5, y = Inf, label = label),
    vjust = 1.2, hjust = 0,
    size = 3.5,
    inherit.aes = FALSE
  ) +
  labs(
    title = "User Agreement on Species Count per Photo",
    x = "Agreement Percentage (mode count / total users)",
    y = "Number of Photos",
    fill = "Count Type"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
####################################################################
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
####################################################################
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

##################################################################
################## DBSCAN clustering analysis ####################
### Density-Based Spatial Clustering of Applications with Noise ##
##################################################################
head(Positives_marks)
# Parameters for DBSCAN
eps <- 8      # Distance threshold in coordinate units
minPts <- 2    # Minimum number of marks to form a cluster

# Apply DBSCAN clustering by subject_ids and tool_label
clustered_data <- Positives_marks %>%
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

###################################################################
# Example: visualize one photo (change subject_ids to your target photo and select species)
ggplot(filter(clustered_data, subject_ids == "41136326", tool_label=='Fish'), aes(x = xcoord, y = ycoord, color = factor(cluster_id))) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_viridis_d(na.value = "gray80") +
  labs(title = "Clusters for Subject", color = "Cluster") +
  coord_fixed() +
  theme_minimal()

# calculate the mean x/y coordinates for each cluster (excluding noise) by subject_ids, tool_label, and cluster_id
centroids <- clustered_data %>%
  filter(!is.na(cluster_id)) %>%
  group_by(subject_ids, tool_label, cluster_id) %>%
  summarise(
    centroid_x = mean(xcoord),
    centroid_y = mean(ycoord),
    .groups = "drop"
  )

photo_id <- 100502474
species <- "Fish"

photo_data <- clustered_data %>%
  filter(subject_ids == photo_id, tool_label == species)

photo_centroids <- centroids %>%
  filter(subject_ids == photo_id, tool_label == species)

ggplot(photo_data, aes(x = xcoord, y = ycoord, color = factor(cluster_id))) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_point(
    data = photo_centroids,
    aes(x = centroid_x, y = centroid_y),
    shape = 4, size = 4, color = "black", stroke = 1.2,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = photo_centroids,
    aes(x = centroid_x, y = centroid_y, label = cluster_id),
    inherit.aes = FALSE,
    size = 3.5, vjust = -1, fontface = "bold", color = "black"
  ) +
  scale_color_viridis_d(na.value = "gray80", name = "Cluster ID") +
  labs(
    title = paste("Clusters for", species, "in Photo", photo_id),
    x = "X Coordinate",
    y = "Y Coordinate"
  ) +
  coord_fixed() +
  theme_minimal()
###################################################################
# Count Number of agreement clusters per photo and species
# how many unique users contributed marks to each cluster
cluster_summary <- clustered_data %>%
  filter(!is.na(cluster_id), cluster_id > 0) %>%  # Keep valid clusters only (exclude noise)
  group_by(subject_ids, tool_label, cluster_id) %>%
  summarise(
    n_users = n_distinct(user_name),              # Number of unique users per cluster
    .groups = "drop"
  )

# Filter clusters with strong agreement (e.g., ≥ 50% agreement)
user_totals <- clustered_data %>%
  distinct(subject_ids, tool_label, user_name) %>%
  count(subject_ids, tool_label, name = "total_users")

high_confidence_clusters <- cluster_summary %>%
  left_join(user_totals, by = c("subject_ids", "tool_label")) %>%
  mutate(user_agreement = n_users / total_users) %>%
  filter(user_agreement >= 0.5)  # keep clusters marked by ≥50% of users

# How many strong clusters per photo/species?
n_cluster_high <- high_confidence_clusters %>%
  group_by(subject_ids, tool_label) %>%     # per photo and species
  summarise(
    n_high_conf_clusters = n_distinct(cluster_id),   # how many clusters met ≥ 50% agreement
    max_agreement_users = max(n_users),              # largest number of users agreeing on any single cluster
    .groups = "drop"
  ) %>%
  arrange(desc(max_agreement_users))        # rank by strength of top cluster

high_confidence_clusters %>%
  group_by(subject_ids, tool_label) %>%
  summarise(n_high_conf_clusters = n_distinct(cluster_id), .groups = "drop") %>%
  ggplot(aes(x = fct_reorder(tool_label, n_high_conf_clusters, .fun = median), 
             y = n_high_conf_clusters, 
             fill = tool_label)) +
  geom_boxplot() +
  scale_fill_viridis_d(guide = "none") +
  labs(
    title = "Distribution of High-Confidence Clusters per Species",
    x = "Species (ordered by median # clusters)",
    y = "# High-Confidence Clusters (≥50% user agreement)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total clusters per photo and species
total_clusters <- clustered_data %>%
  filter(!is.na(cluster_id), cluster_id > 0) %>%
  distinct(subject_ids, tool_label, cluster_id) %>%
  count(subject_ids, tool_label, name = "total_clusters")

cluster_proportions <- left_join(n_cluster_high, total_clusters,
                                 by = c("subject_ids", "tool_label")) %>%
  mutate(prop_high_conf_clusters = n_high_conf_clusters / total_clusters)

ggplot(cluster_proportions, aes(x = tool_label, y = prop_high_conf_clusters)) +
  geom_boxplot(fill = "#4daf4a", alpha = 0.7) +
  labs(
    title = "Proportion of High-Confidence Clusters per Species",
    x = "Species",
    y = "Proportion of Clusters (≥50% user agreement)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### compare mode estimate with number of high-confidence cluster

mode_positive <- mode_agreement_summary %>%
  filter(mode_value > 0)
names(mode_positive)[2] <- 'tool_label'
  
compare_data <- left_join(n_cluster_high, mode_positive,
                          by = c("subject_ids", "tool_label"))

compare_data_clean <- compare_data %>%
  filter(!is.na(n_high_conf_clusters), !is.na(mode_value))

ggplot(compare_data, aes(x = mode_count, y = n_high_conf_clusters)) +
  geom_point(alpha = 0.7, size = 2.5, color = "#1f78b4") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
  facet_wrap(~ tool_label) +
  labs(
    title = "High-Confidence Clusters vs. Mode Count",
    x = "Mode Count",
    y = "# High-Confidence Clusters"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))
















