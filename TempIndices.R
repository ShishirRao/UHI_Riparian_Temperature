library(dplyr)
library(lubridate)
library(tidyr)
library(slider)


##### start with daily indices #####

# Keep the original temperature dataset unchanged
temp_indices_data <- temp_data %>%
  filter(!is.na(date_time)) %>%
  mutate(
    date = as.Date(date_time),
    year = year(date_time),
    month = month(date_time)
  )


# Sampling interval for each file
sampling_info <- temp_QC %>%
  select(FileName, sampling_interval_min)

# Function to identify US daylight-saving transition dates
dst_dates <- function(yr) {

  march <- seq(as.Date(paste0(yr, "-03-01")),
               as.Date(paste0(yr, "-03-14")), by = "day")

  november <- seq(as.Date(paste0(yr, "-11-01")),
                  as.Date(paste0(yr, "-11-07")), by = "day")

  # Return the second Sunday in March, when DST begins and
  # the first Sunday in November, when DST ends.
  c(
    march[which(wday(march) == 1)[2]],
    november[which(wday(november) == 1)[1]]
  )
}

# Identify spring and fall DST dates
years <- sort(unique(temp_indices_data$year))

spring_dates <- as.Date(vapply(
  years, function(y) as.character(dst_dates(y)[1]), character(1)
))

fall_dates <- as.Date(vapply(
  years, function(y) as.character(dst_dates(y)[2]), character(1)
))


# Calculate daily indices
daily_temp <- temp_indices_data %>%
  left_join(sampling_info, by = "FileName") %>%
  group_by(FileName, date) %>%
  summarise(
    year = first(year),
    month = first(month),
    sampling_interval_min = first(sampling_interval_min),

    n_obs = sum(!is.na(tempC)),

    mean_temp = mean(tempC, na.rm = TRUE),
    max_temp = max(tempC, na.rm = TRUE),
    min_temp = min(tempC, na.rm = TRUE),
    sd_temp = sd(tempC, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    diel_range = max_temp - min_temp,

    # Number of hours in each day
    hours_day = case_when(
      date %in% spring_dates ~ 23,
      date %in% fall_dates ~ 25,
      TRUE ~ 24
    ),

    expected_obs = hours_day * 60 / sampling_interval_min,

    completeness_pct = round(
      100 * n_obs / expected_obs, 2
    ),

    valid_day = completeness_pct >= 90
  ) %>%
  mutate(
    across(
      c(mean_temp, max_temp, min_temp,
        sd_temp, diel_range),
      ~ if_else(valid_day, .x, NA_real_)
    )
  )

View(daily_temp)


##### now, the 7-day rolling indices #####
# Fill missing dates before rolling calculations

# Calculate 7-day rolling temperature indices

rolling_temp <- daily_temp %>%
  group_by(FileName) %>%

  # Fill missing dates to ensure consecutive 7-day windows
  complete(
    date = seq(min(date), max(date), by = "day")
  ) %>%

  arrange(date, .by_group = TRUE) %>%

  mutate(
    year = year(date),
    month = month(date),

    # 7-day average daily maximum temperature
    DADM7 = slide_dbl(
      max_temp,
      mean,
      .before = 6,
      .complete = TRUE
    ),

    # 7-day average daily mean temperature
    DADMean7 = slide_dbl(
      mean_temp,
      mean,
      .before = 6,
      .complete = TRUE
    ),

    # 7-day average daily minimum temperature
    DADMin7 = slide_dbl(
      min_temp,
      mean,
      .before = 6,
      .complete = TRUE
    ),

    # 7-day average diel temperature range
    DADTR7 = slide_dbl(
      diel_range,
      mean,
      .before = 6,
      .complete = TRUE
    )
  ) %>%
  ungroup()

View(rolling_temp)


#### seasonal indices ####

# Define meteorological season for San Antonio

#Winter	December–February
#Spring	March–May
#Summer	June–August
#Fall	September–November

seasonal_data <- daily_temp %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% 3:5 ~ "Spring",
      month %in% 6:8 ~ "Summer",
      month %in% 9:11 ~ "Fall"
    ),

    season_year = if_else(
      month == 12, year + 1L, year
    )
  )

# Summarise seasonal temperatures
seasonal_temp <- seasonal_data %>%
  group_by(FileName, season_year, season) %>%
  summarise(
    n_valid_days = sum(valid_day, na.rm = TRUE),

    mean_temp = if (all(is.na(mean_temp))) NA_real_
    else mean(mean_temp, na.rm = TRUE),

    mean_daily_max = if (all(is.na(max_temp))) NA_real_
    else mean(max_temp, na.rm = TRUE),

    mean_daily_min = if (all(is.na(min_temp))) NA_real_
    else mean(min_temp, na.rm = TRUE),

    max_temp = if (all(is.na(max_temp))) NA_real_
    else max(max_temp, na.rm = TRUE),

    min_temp = if (all(is.na(min_temp))) NA_real_
    else min(min_temp, na.rm = TRUE),

    mean_diel_range = if (all(is.na(diel_range))) NA_real_
    else mean(diel_range, na.rm = TRUE),

    sd_daily_mean = sd(mean_temp, na.rm = TRUE),

    .groups = "drop"
  )

View(seasonal_temp)

#### Annual temperature indices ####

annual_temp <- daily_temp %>%
  group_by(FileName, year) %>%
  summarise(
    n_valid_days = sum(valid_day, na.rm = TRUE),

    annual_mean = if (all(is.na(mean_temp))) NA_real_
    else mean(mean_temp, na.rm = TRUE),

    annual_mean_max = if (all(is.na(max_temp))) NA_real_
    else mean(max_temp, na.rm = TRUE),

    annual_mean_min = if (all(is.na(min_temp))) NA_real_
    else mean(min_temp, na.rm = TRUE),

    annual_max = if (all(is.na(max_temp))) NA_real_
    else max(max_temp, na.rm = TRUE),

    annual_min = if (all(is.na(min_temp))) NA_real_
    else min(min_temp, na.rm = TRUE),

    annual_diel_range = if (all(is.na(diel_range))) NA_real_
    else mean(diel_range, na.rm = TRUE),

    annual_sd = sd(mean_temp, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    annual_thermal_range = annual_max - annual_min
  )

# Add maximum 7DADM for each year
annual_7dadm <- rolling_temp %>%
  group_by(FileName, year) %>%
  summarise(
    max_7DADM = if (all(is.na(DADM7))) NA_real_
    else max(DADM7, na.rm = TRUE),
    .groups = "drop"
  )

annual_temp <- annual_temp %>%
  left_join(annual_7dadm, by = c("FileName", "year"))

View(annual_temp)


#### simulating a threshold ####

threshold_temp <- 30

daily_temp <- daily_temp %>%
  mutate(
    # Number of degree-days above threshold
    degree_days_above_30 = if_else(
      valid_day,
      pmax(mean_temp - threshold_temp, 0),
      NA_real_
    ),

    # Whether daily maximum exceeded threshold
    hot_day = if_else(
      valid_day,
      max_temp > threshold_temp,
      NA
    )
  )

# Summarise by year
annual_thresholds <- daily_temp %>%
  group_by(FileName, year) %>%
  summarise(
    degree_days_above_30 = if (all(is.na(degree_days_above_30)))
      NA_real_
    else sum(degree_days_above_30, na.rm = TRUE),

    n_hot_days = sum(hot_day, na.rm = TRUE),

    .groups = "drop"
  )


#### plotting a logger's temperature #####


example_logger <- unique(daily_temp$FileName)[1]

daily_temp %>%
  filter(FileName == example_logger) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(ymin = min_temp, ymax = max_temp),
    fill = "lightblue",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = mean_temp),
    color = "blue"
  ) +
  labs(
    title = example_logger,
    x = "Date",
    y = "Water temperature (°C)"
  ) +
  theme_minimal()
