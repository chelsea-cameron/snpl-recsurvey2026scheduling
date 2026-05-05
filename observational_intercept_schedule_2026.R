#..............................................................................#
# Snowy Plover Recreation Survey Sampling Schedule Generator
# Observational + Visitor Intercept Surveys
#..............................................................................#

library(lubridate)
library(dplyr)
library(tidyr)

set.seed(123)

#..............................................................................#
# Define survey period

start_date <- as.Date("2026-05-10")
end_date <- as.Date("2026-08-31")

sites <- c("Saltair", "Kennecott")

# Observational surveys are 2-hour windows from 6 AM to 8 PM
observational_hours <- seq(6, 18, by = 2)

# Visitor intercept surveys are 3-hour windows from 6 AM to 8 PM
# Last possible start time is 5 PM so the survey ends at 8 PM
intercept_hours <- seq(6, 17, by = 1)

#..............................................................................#
# Helper function to format hours

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
# Create all dates

all_dates <- data.frame(
  date = seq.Date(from = start_date, to = end_date, by = "day")
) %>%
  mutate(
    week = isoweek(date),
    year = year(date),
    week_id = paste(year, week, sep = "_"),
    day = weekdays(date),
    month = month(date, label = TRUE, abbr = FALSE),
    is_weekend = day %in% c("Saturday", "Sunday"),
    day_type = ifelse(is_weekend, "Weekend", "Weekday")
  )

#..............................................................................#
# Randomly choose 4 observational survey days per week

observational_dates <- all_dates %>%
  group_by(week_id) %>%
  sample_n(size = min(4, n()), replace = FALSE) %>%
  ungroup() %>%
  arrange(date)

# Create observational schedule
observational_schedule <- expand_grid(
  date = observational_dates$date,
  site = sites
) %>%
  left_join(observational_dates, by = "date") %>%
  group_by(site) %>%
  mutate(
    start_hour = rep(sample(observational_hours), length.out = n())
  ) %>%
  ungroup() %>%
  mutate(
    survey = "Observational",
    end_hour = start_hour + 2,
    start_time = sapply(start_hour, format_hour_to_time),
    end_time = sapply(end_hour, format_hour_to_time),
    date_formatted = format(date, "%B %d %Y")
  ) %>%
  select(
    survey,
    site,
    date = date_formatted,
    raw_date = date,
    day,
    month,
    day_type,
    start_time,
    end_time,
    start_hour,
    end_hour
  )

#..............................................................................#
# Randomly choose 1 visitor intercept survey day per week

intercept_dates <- all_dates %>%
  group_by(week_id) %>%
  sample_n(size = 1, replace = FALSE) %>%
  ungroup() %>%
  arrange(date)

# Create visitor intercept schedule
intercept_schedule <- expand_grid(
  date = intercept_dates$date,
  site = sites
) %>%
  left_join(intercept_dates, by = "date") %>%
  group_by(site) %>%
  mutate(
    start_hour = rep(sample(intercept_hours), length.out = n())
  ) %>%
  ungroup() %>%
  mutate(
    survey = "Visitor Intercept",
    end_hour = start_hour + 3,
    start_time = sapply(start_hour, format_hour_to_time),
    end_time = sapply(end_hour, format_hour_to_time),
    date_formatted = format(date, "%B %d %Y")
  ) %>%
  select(
    survey,
    site,
    date = date_formatted,
    raw_date = date,
    day,
    month,
    day_type,
    start_time,
    end_time,
    start_hour,
    end_hour
  )

#..............................................................................#
# Combine schedules

combined_schedule <- bind_rows(
  observational_schedule,
  intercept_schedule
) %>%
  arrange(raw_date, site, start_hour, survey)

#..............................................................................#
# Verification checks

cat("\nChecking observational survey days per week...\n")

observational_days_per_week <- observational_schedule %>%
  distinct(raw_date, week = isoweek(raw_date)) %>%
  count(week, name = "observational_survey_days")

print(observational_days_per_week)

cat("\nChecking visitor intercept survey days per week...\n")

intercept_days_per_week <- intercept_schedule %>%
  distinct(raw_date, week = isoweek(raw_date)) %>%
  count(week, name = "intercept_survey_days")

print(intercept_days_per_week)

cat("\nChecking observational time slot coverage by site...\n")

observational_time_coverage <- observational_schedule %>%
  group_by(site, start_time) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(site, start_time)

print(observational_time_coverage)

cat("\nChecking visitor intercept time slot coverage by site...\n")

intercept_time_coverage <- intercept_schedule %>%
  group_by(site, start_time) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(site, start_time)

print(intercept_time_coverage)

# Check for missing observational slots
missing_observational_slots <- expand_grid(
  site = sites,
  start_hour = observational_hours
) %>%
  mutate(start_time = sapply(start_hour, format_hour_to_time)) %>%
  anti_join(observational_time_coverage, by = c("site", "start_time"))

# Check for missing visitor intercept slots
missing_intercept_slots <- expand_grid(
  site = sites,
  start_hour = intercept_hours
) %>%
  mutate(start_time = sapply(start_hour, format_hour_to_time)) %>%
  anti_join(intercept_time_coverage, by = c("site", "start_time"))

if (nrow(missing_observational_slots) == 0) {
  cat("\nAll observational time slots are represented at both sites at least once.\n")
} else {
  cat("\nWARNING: Some observational time slots are missing:\n")
  print(missing_observational_slots)
}

if (nrow(missing_intercept_slots) == 0) {
  cat("\nAll visitor intercept time slots are represented at both sites at least once.\n")
} else {
  cat("\nWARNING: Some visitor intercept time slots are missing:\n")
  print(missing_intercept_slots)
}

cat("\nTotal combined survey rows:\n")
print(nrow(combined_schedule))

cat("\nTotal observational rows:\n")
print(nrow(observational_schedule))

cat("\nTotal visitor intercept rows:\n")
print(nrow(intercept_schedule))

#..............................................................................#
# Write CSV files

write.csv(combined_schedule, "recreation_combined_sampling_schedule_2026.csv", row.names = FALSE)
write.csv(observational_schedule, "recreation_observational_sampling_schedule_2026.csv", row.names = FALSE)
write.csv(intercept_schedule, "recreation_visitor_intercept_sampling_schedule_2026.csv", row.names = FALSE)

#..............................................................................#
