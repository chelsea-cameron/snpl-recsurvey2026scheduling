#..............................................................................#      
#........ Snowy Plover Recreation Survey Sampling Windows Generator ...........#
#
# For 2 sites, May 10-August 31, with one survey per day
# Ensuring no overlapping survey dates and balanced site representation
#..............................................................................#

library(lubridate)
library(dplyr)
library(tidyr)

# Set working directory
setwd("C:/Users/Chelsea.Cameron/Box/-.Chelsea.Cameron Individual/SNPL") 

# For reproducibility
set.seed(123)

#..............................................................................#
# Define survey period
start_date <- as.Date("2026-05-18")
end_date <- as.Date("2026-08-31")
# Exclude event dates
excluded_dates <- as.Date(c(
  "2026-06-26",
  "2026-07-10",
  "2026-07-15",
  "2026-08-07",
  "2026-08-08",
  "2026-08-09"
))
# Create a data frame with all dates in the survey period
all_dates <- data.frame(
  date = seq.Date(from = start_date, to = end_date, by = "day")
)

# Add additional columns
all_dates <- all_dates %>%
  mutate(
    day_of_week = weekdays(date),
    month = month(date),
    month_name = month(date, label = TRUE),
    week = isoweek(date),
    year = year(date),
    week_id = paste(year, week, sep = "_"),
    is_weekend = day_of_week %in% c("Saturday", "Sunday"),
    is_weekday = !is_weekend,
    day_type = case_when(
      is_weekend ~ "Weekend",
      TRUE ~ "Weekday"
    )
  )

#..............................................................................#
# Define site names
sites <- c("Saltair", "Kennecott")

# Observational surveys are 2-hour windows
observational_hours <- seq(6, 18, by = 2)

# Visitor intercept surveys are 3-hour windows
intercept_hours <- seq(6, 18, by = 3)

#..............................................................................#
# Helper function to parse time strings to numeric hours
parse_time_to_hour <- function(time_str) {
  hour_num <- 0
  
  if (grepl("AM", time_str)) {
    hour_num <- as.numeric(sub(":00 AM", "", time_str))
    if (hour_num == 12) hour_num <- 0
  } else {
    hour_num <- as.numeric(sub(":00 PM", "", time_str))
    if (hour_num != 12) hour_num <- hour_num + 12
  }
  
  return(hour_num)
}

# Helper function to format hours to time strings
format_hour_to_time <- function(hour) {
  if (hour < 12) {
    return(paste0(hour, ":00 AM"))
  } else if (hour == 12) {
    return("12:00 PM")
  } else {
    return(paste0(hour - 12, ":00 PM"))
  }
}

#..............................................................................#
# Function to generate non-overlapping surveys
generate_non_overlapping_surveys <- function(dates_df, site_names) {
  
  observational_results <- data.frame()
  intercept_results <- data.frame()
  used_dates <- as.Date(character())
  
  # Total possible survey counts
  obs_total_surveys <- length(unique(dates_df$week_id)) * 4
  intercept_total_surveys <- length(unique(dates_df$week_id)) * 1
  
  # Balanced site assignments
  obs_site_assignments <- sample(rep(site_names, length.out = obs_total_surveys))
  intercept_site_assignments <- sample(rep(site_names, length.out = intercept_total_surveys))
  
  obs_counter <- 1
  intercept_counter <- 1
  
  #............................................................................#
  # Observational surveys: 4 days per week, 1 survey per day
  
  for (w in unique(dates_df$week_id)) {
    
    week_dates <- subset(dates_df, week_id == w)
    available_week_dates <- week_dates[!(week_dates$date %in% used_dates), ]
    
    n_dates <- min(4, nrow(available_week_dates))
    
    if (n_dates == 0) next
    
    sampled_indices <- sample(1:nrow(available_week_dates), n_dates, replace = FALSE)
    
    for (idx in sampled_indices) {
      
      date_row <- available_week_dates[idx, ]
      d <- date_row$date
      
      used_dates <- c(used_dates, d)
      
      this_site <- obs_site_assignments[obs_counter]
      obs_counter <- obs_counter + 1
      
      start_hour <- sample(observational_hours, 1)
      end_hour <- start_hour + 2
      
      new_row <- data.frame(
        survey = "Observational",
        site = this_site,
        date = format(d, "%B %d %Y"),
        raw_date = d,
        day = weekdays(d),
        month = as.character(date_row$month_name),
        day_type = date_row$day_type,
        start_time = format_hour_to_time(start_hour),
        end_time = format_hour_to_time(end_hour),
        stringsAsFactors = FALSE
      )
      
      observational_results <- rbind(observational_results, new_row)
    }
  }
  
  #............................................................................#
  # Visitor Intercept surveys: 1 day per week, avoiding observational dates
  
  for (w in unique(dates_df$week_id)) {
    
    week_dates <- subset(dates_df, week_id == w)
    available_week_dates <- week_dates[!(week_dates$date %in% used_dates), ]
    
    n_dates <- min(1, nrow(available_week_dates))
    
    if (n_dates == 0) next
    
    sampled_indices <- sample(1:nrow(available_week_dates), n_dates, replace = FALSE)
    
    for (idx in sampled_indices) {
      
      date_row <- available_week_dates[idx, ]
      d <- date_row$date
      
      used_dates <- c(used_dates, d)
      
      this_site <- intercept_site_assignments[intercept_counter]
      intercept_counter <- intercept_counter + 1
      
      start_hour <- sample(intercept_hours, 1)
      end_hour <- start_hour + 3
      
      new_row <- data.frame(
        survey = "Visitor Intercept",
        site = this_site,
        date = format(d, "%B %d %Y"),
        raw_date = d,
        day = weekdays(d),
        month = as.character(date_row$month_name),
        day_type = date_row$day_type,
        start_time = format_hour_to_time(start_hour),
        end_time = format_hour_to_time(end_hour),
        stringsAsFactors = FALSE
      )
      
      intercept_results <- rbind(intercept_results, new_row)
    }
  }
  
  # Combine results
  combined_results <- rbind(observational_results, intercept_results)
  
  # Sort results
  combined_results <- combined_results %>%
    mutate(
      sort_date = as.Date(raw_date),
      sort_hour = sapply(start_time, parse_time_to_hour)
    ) %>%
    arrange(sort_date, sort_hour, survey, site) %>%
    select(-sort_date, -sort_hour)
  
  return(list(
    combined = combined_results,
    observational = observational_results,
    intercept = intercept_results
  ))
}

#..............................................................................#
# Generate surveys
survey_results <- generate_non_overlapping_surveys(
  all_dates,
  sites
)

# Extract results
combined_schedule <- survey_results$combined
observational_schedule <- survey_results$observational
intercept_schedule <- survey_results$intercept

#..............................................................................#
# Verification checks

cat("\n Verifying observational survey days per week... \n")

observational_days_per_week <- observational_schedule %>%
  mutate(week_id = paste(year(raw_date), isoweek(raw_date), sep = "_")) %>%
  group_by(week_id) %>%
  summarise(
    unique_dates = n_distinct(raw_date),
    total_slots = n(),
    .groups = "drop"
  )

print(observational_days_per_week)

cat("\n Verifying visitor intercept survey days per week... \n")

intercept_days_per_week <- intercept_schedule %>%
  mutate(week_id = paste(year(raw_date), isoweek(raw_date), sep = "_")) %>%
  group_by(week_id) %>%
  summarise(
    unique_dates = n_distinct(raw_date),
    total_slots = n(),
    .groups = "drop"
  )

print(intercept_days_per_week)

cat("\n Checking for overlapping survey dates... \n")

overlap_check <- combined_schedule %>%
  group_by(raw_date) %>%
  summarise(
    number_of_surveys = n(),
    surveys = paste(survey, collapse = ", "),
    sites = paste(site, collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(number_of_surveys > 1)

if (nrow(overlap_check) == 0) {
  cat("No overlapping survey dates found. Each date has only one survey.\n")
} else {
  cat("WARNING: Some dates have more than one survey:\n")
  print(overlap_check)
}

cat("\n Checking site balance... \n")

site_balance <- combined_schedule %>%
  group_by(survey, site) %>%
  summarise(
    total_surveys = n(),
    .groups = "drop"
  )

print(site_balance)

cat("\n Analyzing time distribution... \n")

time_distribution <- combined_schedule %>%
  mutate(hour = sapply(start_time, parse_time_to_hour)) %>%
  group_by(survey, site, hour) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(survey, site, hour)

print(time_distribution)

cat("\n Stratification summary... \n")

strat_summary <- combined_schedule %>%
  group_by(survey, month, day_type) %>%
  summarise(
    unique_dates = n_distinct(raw_date),
    total_slots = n(),
    .groups = "drop"
  ) %>%
  arrange(survey, month, day_type)

print(strat_summary)

cat("\n Overall sampling coverage... \n")

total_unique_days <- n_distinct(combined_schedule$raw_date)
total_days_in_period <- as.integer(end_date - start_date) + 1
coverage_percentage <- round((total_unique_days / total_days_in_period) * 100, 1)

cat(
  "Total unique days sampled:",
  total_unique_days,
  " out of ",
  total_days_in_period,
  " days in the survey period (",
  coverage_percentage,
  "% coverage)\n",
  sep = ""
)

#..............................................................................#
# Write to CSV

write.csv(
  combined_schedule,
  "recreation_survey_sampling_schedule_2026.csv",
  row.names = FALSE
)

write.csv(
  observational_schedule,
  "recreation_Observational_sampling_schedule_2026.csv",
  row.names = FALSE
)

write.csv(
  intercept_schedule,
  "recreation_Visitor_Intercept_sampling_schedule_2026.csv",
  row.names = FALSE
)

#..............................................................................#
