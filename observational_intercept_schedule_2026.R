#..............................................................................#      
#........ Snowy Plover Recreation Survey Sampling Windows Generator ...........#
#
# For 2 sites, May 10-August 31, with one time slot per survey type per site per day
# Ensuring no time overlap between surveys
#..............................................................................#

library(lubridate)
library(dplyr)
library(tidyr)

# Set working directory
setwd("C:/Users/Chelsea.Cameron/Box/-.Chelsea.Cameron Individual/SNPL")
# NOTE: working directory will need to be changed based on the user.
# The file path identified will be where the output files will be saved.

# For reproducibility
set.seed(123)

#..............................................................................#
# Define survey period (May 10 through August 31, 2026)
start_date <- as.Date("2026-05-10")
end_date <- as.Date("2026-08-31")

# Create a data frame with all dates in the survey period
all_dates <- data.frame(
  date = seq.Date(from = start_date, to = end_date, by = "day")
)

# Add additional columns for day of week, month, and week
all_dates <- all_dates %>%
  mutate(
    day_of_week = weekdays(date),
    month = month(date),
    month_name = month(date, label = TRUE),
    week = isoweek(date),
    year = year(date),
    week_id = paste(year, week, sep = "_"),
    is_weekend = day_of_week %in% c("Saturday", "Sunday"),
    is_weekday = !is_weekend
  )

#..............................................................................#
# Create strata for stratified sampling
all_dates <- all_dates %>%
  mutate(
    day_type = case_when(
      is_weekend ~ "Weekend",
      TRUE ~ "Weekday"
    )
  )

#..............................................................................#
# Define site names
sites <- c("Saltair", "Kennecott")

# Define possible sampling hours for observational surveys
# Observational surveys are 2-hour windows
# Last window starts at 6pm and ends at 8pm
observational_hours <- seq(6, 18, by = 2)

# Define possible sampling hours for visitor intercept surveys
# Visitor intercept surveys are 3-hour windows
# Last window starts at 6pm and ends at 9pm
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
generate_non_overlapping_surveys <- function(dates_df, site_names) {
  
  # Create empty data frames for both surveys
  observational_results <- data.frame()
  intercept_results <- data.frame()
  
  # Process each week
  for (w in unique(dates_df$week_id)) {
    week_dates <- subset(dates_df, week_id == w)
    
    # Determine number of observational dates to sample
    # Observational surveys occur 4 days per week
    n_dates <- min(4, nrow(week_dates))
    
    if (n_dates == 0) next
    
    # Sample dates from the week
    sampled_indices <- sample(1:nrow(week_dates), n_dates, replace = FALSE)
    
    # For each sampled date
    for (idx in sampled_indices) {
      date_row <- week_dates[idx, ]
      d <- date_row$date
      
      # Get available hours for the entire day
      available_hours <- observational_hours
      
      # Make sure there are enough available hours to choose one time slot per site
      if (length(available_hours) < length(site_names)) {
        warning(paste("Not enough hours for", d))
        next
      }
      
      # Randomly select unique start hours for each site
      selected_hours <- sample(available_hours, length(site_names), replace = FALSE)
      
      # Assign hours to site/survey combinations
      assignments <- data.frame(
        site = site_names,
        survey = "Observational",
        stringsAsFactors = FALSE
      )
      
      # Randomly assign the selected hours to the site/survey combinations
      assignments$start_hour <- sample(selected_hours, length(site_names), replace = FALSE)
      
      # For each assignment, generate the survey entry
      for (i in 1:nrow(assignments)) {
        this_site <- assignments$site[i]
        this_survey <- assignments$survey[i]
        start_hour <- assignments$start_hour[i]
        end_hour <- start_hour + 2
        
        # Format date and time
        date_str <- format(d, "%B %d %Y")
        weekday <- weekdays(d)
        start_ampm <- format_hour_to_time(start_hour)
        end_ampm <- format_hour_to_time(end_hour)
        
        new_row <- data.frame(
          survey = this_survey,
          site = this_site,
          date = date_str,
          raw_date = d,
          day = weekday,
          month = as.character(date_row$month_name),
          day_type = date_row$day_type,
          start_time = start_ampm,
          end_time = end_ampm,
          stringsAsFactors = FALSE
        )
        
        observational_results <- rbind(observational_results, new_row)
      }
    }
  }
  
  # Process each week
  for (w in unique(dates_df$week_id)) {
    week_dates <- subset(dates_df, week_id == w)
    
    # Determine number of visitor intercept dates to sample
    # Visitor intercept surveys occur 1 day per week
    n_dates <- min(1, nrow(week_dates))
    
    if (n_dates == 0) next
    
    # Sample dates from the week
    sampled_indices <- sample(1:nrow(week_dates), n_dates, replace = FALSE)
    
    # For each sampled date
    for (idx in sampled_indices) {
      date_row <- week_dates[idx, ]
      d <- date_row$date
      
      # Get available hours for the entire day
      available_hours <- intercept_hours
      
      # Make sure there are enough available hours to choose one time slot per site
      if (length(available_hours) < length(site_names)) {
        warning(paste("Not enough hours for", d))
        next
      }
      
      # Randomly select unique start hours for each site
      selected_hours <- sample(available_hours, length(site_names), replace = FALSE)
      
      # Assign hours to site/survey combinations
      assignments <- data.frame(
        site = site_names,
        survey = "Visitor Intercept",
        stringsAsFactors = FALSE
      )
      
      # Randomly assign the selected hours to the site/survey combinations
      assignments$start_hour <- sample(selected_hours, length(site_names), replace = FALSE)
      
      # For each assignment, generate the survey entry
      for (i in 1:nrow(assignments)) {
        this_site <- assignments$site[i]
        this_survey <- assignments$survey[i]
        start_hour <- assignments$start_hour[i]
        end_hour <- start_hour + 3
        
        # Format date and time
        date_str <- format(d, "%B %d %Y")
        weekday <- weekdays(d)
        start_ampm <- format_hour_to_time(start_hour)
        end_ampm <- format_hour_to_time(end_hour)
        
        new_row <- data.frame(
          survey = this_survey,
          site = this_site,
          date = date_str,
          raw_date = d,
          day = weekday,
          month = as.character(date_row$month_name),
          day_type = date_row$day_type,
          start_time = start_ampm,
          end_time = end_ampm,
          stringsAsFactors = FALSE
        )
        
        intercept_results <- rbind(intercept_results, new_row)
      }
    }
  }
  
  # Combine the results from both surveys
  combined_results <- rbind(observational_results, intercept_results)
  
  # Sort by date, site, and start time
  combined_results <- combined_results %>%
    mutate(
      sort_date = as.Date(raw_date),
      sort_hour = sapply(start_time, parse_time_to_hour)
    ) %>%
    arrange(sort_date, site, sort_hour, survey) %>%
    select(-sort_date, -sort_hour)
  
  return(list(
    combined = combined_results,
    observational = observational_results,
    intercept = intercept_results
  ))
}

#..............................................................................#
# Generate both surveys with guaranteed non-overlapping time slots
survey_results <- generate_non_overlapping_surveys(
  all_dates,
  sites
)

# Extract the results
combined_schedule <- survey_results$combined
observational_schedule <- survey_results$observational
intercept_schedule <- survey_results$intercept

#..............................................................................#
# Comprehensive verification checks

# Check observational survey days per week
cat("\n Verifying observational survey days per week... \n")
observational_days_per_week <- observational_schedule %>%
  mutate(week_id = paste(year(raw_date), isoweek(raw_date), sep = "_")) %>%
  group_by(week_id) %>%
  summarise(unique_dates = n_distinct(raw_date), total_slots = n(), .groups = "drop")

print(observational_days_per_week)

# Check visitor intercept survey days per week
cat("\n Verifying visitor intercept survey days per week... \n")
intercept_days_per_week <- intercept_schedule %>%
  mutate(week_id = paste(year(raw_date), isoweek(raw_date), sep = "_")) %>%
  group_by(week_id) %>%
  summarise(unique_dates = n_distinct(raw_date), total_slots = n(), .groups = "drop")

print(intercept_days_per_week)

# Check distribution of survey times across the day
cat("\n Analyzing time distribution... \n")
time_distribution <- combined_schedule %>%
  mutate(hour = sapply(start_time, parse_time_to_hour)) %>%
  group_by(survey, site, hour) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(survey, site, hour)

cat("Time distribution across the day:\n")
print(time_distribution)

# Check stratification results
cat("\n Stratification summary... \n")
strat_summary <- combined_schedule %>%
  group_by(survey, month, day_type) %>%
  summarise(
    unique_dates = n_distinct(raw_date),
    total_slots = n(),
    .groups = "drop"
  ) %>%
  arrange(survey, month, day_type)

cat("Stratification results by month and day type:\n")
print(strat_summary)

# Overall summary of coverage
cat("\n Overall sampling coverage... \n")
total_unique_days <- n_distinct(combined_schedule$raw_date)
total_days_in_period <- as.integer(end_date - start_date) + 1
coverage_percentage <- round((total_unique_days / total_days_in_period) * 100, 1)

cat("Total unique days sampled:", total_unique_days, "out of", total_days_in_period, 
    "days in the survey period (", coverage_percentage, "% coverage)\n", sep="")

#..............................................................................#
# Write to CSV
write.csv(combined_schedule, "recreation_survey_sampling_schedule_2026.csv", row.names = FALSE)

# Write separate CSVs for each survey
write.csv(observational_schedule, "recreation_Observational_sampling_schedule_2026.csv", row.names = FALSE)
write.csv(intercept_schedule, "recreation_Visitor_Intercept_sampling_schedule_2026.csv", row.names = FALSE)

#..............................................................................#
