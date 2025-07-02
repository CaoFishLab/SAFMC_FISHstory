
rm(list = ls())
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
head(Positives_marks)

####### examine and visulize the data
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









