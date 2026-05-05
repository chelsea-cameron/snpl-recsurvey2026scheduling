#..............................................................................#      
#........ Snowy Plover Recreation Survey Sampling Windows Generator ...........#
#
# For 2 sites, May 10-August 31, 2026
# Observational surveys: 4 days per week, 2-hour windows
# Visitor intercept surveys: 1 day per week, 3-hour windows
# No holidays or festivals factored into sampling
#..............................................................................#

library(lubridate)
library(dplyr)
library(tidyr)

# Set working directory
# CHANGE THIS TO WHERE YOU WANT THE CSV FILES SAVED
# setwd("C:/Users/your.name/Documents/SNPL")

# For reproducibility
set.seed(123)

#..............................................................................#
# Define survey period

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

# Create strata for stratified sampling
# This year we are only using Weekday vs Weekend
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

# Define possible sampling hours

# Observational surveys are 2-hour windows.
# Last observational window starts at 6 PM and ends at 8 PM.
observational_hours <- seq(6, 18, by = 1)

# Visitor intercept surveys are 3-hour windows.
# Last intercept window starts at 5 PM and ends at 8 PM.
intercept_hours <- seq(6, 17, by = 1)

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
# Main function to generate surveys

generate_recreation_surveys <- function(dates_df, site_names) {
  
  observational_results <- data.frame()
  intercept_results <- data.frame()
  
  #............................................................................#
  # OBSERVATIONAL SURVEYS
  # 4 randomly selected survey dates per week
  
  for (w in unique(dates_df$week_id)) {
    
    week_dates <- subset(dates_df, week_id == w)
    
    # Sample up to 4 dates per week
    n_obs_dates <- min(4, nrow(week_dates))
    
    if (n_obs_dates == 0) next
    
    sampled_indices <- sample(1:nrow(week_dates), n_obs_dates, replace = FALSE)
    
    for (idx in sampled_indices) {
      
      date_row <- week_dates[idx, ]
      d <- date_row$date
      
      # One observational survey per site on each selected date
      assignments <- data.frame(
        site = site_names,
        survey = "Observational",
        stringsAsFactors = FALSE
      )
      
      # Randomly assign a 2-hour start time for each site
      assignments$start_hour <- sample(observational_hours,
                                       nrow(assignments),
                                       replace = FALSE)
      
      for (i in 1:nrow(assignments)) {
        
        this_site <- assignments$site[i]
        start_hour <- assignments$start_hour[i]
        end_hour <- start_hour + 2
        
        new_row <- data.frame(
          survey = "Observational",
          site = this_site,
          date = format(d, "%B %d %Y"),
          raw_date = d,
          day = weekdays(d),
          month = as.character(date_row$month_name),
          day_type = date_row$day_type,
          week_id = date_row$week_id,
          start_time = format_hour_to_time(start_hour),
          end_time = format_hour_to_time(end_hour),
          stringsAsFactors = FALSE
        )
        
        observational_results <- rbind(observational_results, new_row)
      }
    }
  }
  
  #............................................................................#
  # VISITOR INTERCEPT SURVEYS
  # 1 randomly selected survey date per week
  
  for (w in unique(dates_df$week_id)) {
    
    week_dates <- subset(dates_df, week_id == w)
    
    if (nrow(week_dates) == 0) next
    
    sampled_index <- sample(1:nrow(week_dates), 1, replace = FALSE)
    date_row <- week_dates[sampled_index, ]
    d <- date_row$date
    
    # One visitor intercept survey per site on the selected date
    assignments <- data.frame(
      site = site_names,
      survey = "Visitor Intercept",
      stringsAsFactors = FALSE
    )
    
    # Randomly assign a 3-hour start time for each site
    assignments$start_hour <- sample(intercept_hours,
                                     nrow(assignments),
                                     replace = FALSE)
    
    for (i in 1:nrow(assignments)) {
      
      this_site <- assignments$site[i]
      start_hour <- assignments$start_hour[i]
      end_hour <- start_hour + 3
      
      new_row <- data.frame(
        survey = "Visitor Intercept",
        site = this_site,
        date = format(d, "%B %d %Y"),
        raw_date = d,
        day = weekdays(d),
        month = as.character(date_row$month_name),
        day_type = date_row$day_type,
        week_id = date_row$week_id,
        start_time = format_hour_to_time(start_hour),
        end_time = format_hour_to_time(end_hour),
        stringsAsFactors = FALSE
      )
      
      intercept_results <- rbind(intercept_results, new_row)
    }
  }
  
  # Combine results
  combined_results <- rbind(observational_results, intercept_results)
  
  # Sort by date, site, survey, and start time
  combined_results <- combined_results %>%
    mutate(
      sort_date = as.Date(raw_date),
      sort_hour = sapply(start_time, parse_time_to_hour)
    ) %>%
    arrange(sort_date, site, survey, sort_hour) %>%
    select(-sort_date, -sort_hour)
  
  return(list(
    combined = combined_results,
    observational = observational_results,
    intercept = intercept_results
  ))
}

#..............................................................................#
# Generate survey schedules

survey_results <- generate_recreation_surveys(
  all_dates,
  sites
)

combined_schedule <- survey_results$combined
observational_schedule <- survey_results$observational
intercept_schedule <- survey_results$intercept

#..............................................................................#
# Verification checks

cat("\n Verifying observational survey days per week... \n")

obs_days_per_week <- observational_schedule %>%
  group_by(week_id) %>%
  summarise(
    unique_dates = n_distinct(raw_date),
    total_slots = n(),
    .groups = "drop"
  )

print(obs_days_per_week)

cat("\n Verifying visitor intercept survey days per week... \n")

intercept_days_per_week <- intercept_schedule %>%
  group_by(week_id) %>%
  summarise(
    unique_dates = n_distinct(raw_date),
    total_slots = n(),
    .groups = "drop"
  )

print(intercept_days_per_week)

cat("\n Checking observational time distribution... \n")

obs_time_distribution <- observational_schedule %>%
  mutate(hour = sapply(start_time, parse_time_to_hour)) %>%
  group_by(site, hour) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(site, hour)

print(obs_time_distribution)

cat("\n Checking visitor intercept time distribution... \n")

intercept_time_distribution <- intercept_schedule %>%
  mutate(hour = sapply(start_time, parse_time_to_hour)) %>%
  group_by(site, hour) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(site, hour)

print(intercept_time_distribution)

cat("\n Checking stratification summary... \n")

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

cat("Total unique days sampled:", total_unique_days, "out of", total_days_in_period,
    "days in the survey period (", coverage_percentage, "% coverage)\n", sep = "")

#..............................................................................#
# Write to CSV

write.csv(combined_schedule, "recreation_survey_sampling_schedule_2026.csv", row.names = FALSE)

write.csv(observational_schedule, "recreation_Observational_sampling_schedule_2026.csv", row.names = FALSE)

write.csv(intercept_schedule, "recreation_Visitor_Intercept_sampling_schedule_2026.csv", row.names = FALSE)

#..............................................................................#
