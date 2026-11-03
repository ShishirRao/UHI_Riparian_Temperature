# tempQC with stationID as the merge variable shows substantial duplication of records
overlapping_records <- temp_data_corrected %>%
  group_by(stationID, date_time) %>%
  summarise(
    n_records = n(),
    n_files = n_distinct(FileName),
    n_unique_temps = n_distinct(tempC),
    .groups = "drop"
  ) %>%
  filter(n_files > 1)

overlap_summary <- overlapping_records %>%
  group_by(stationID) %>%
  summarise(
    n_overlapping_timestamps = n(),
    n_conflicting_timestamps = sum(n_unique_temps > 1),
    max_files_per_timestamp = max(n_files),
    .groups = "drop"
  ) %>%
  arrange(desc(n_overlapping_timestamps))

View(overlap_summary)



#write.csv(overlap_summary,"E:/Shishir/postdoc/Data/Results/overlap_summary_v1.csv")

conflicts <- temp_data_corrected %>%
  filter(stationID == "Honey27") %>%
  group_by(date_time) %>%
  filter(
    n_distinct(FileName) > 1,
    n_distinct(tempC) > 1
  ) %>%
  ungroup() %>%
  arrange(date_time)

# Find the date range
conflicts %>%
  summarise(
    start_date = min(date_time),
    end_date = max(date_time),
    n_conflicting_timestamps = n_distinct(date_time)
  )

#Checking the difference in conflicting temperature records


# Calculate the fall DST transition date for each year
fall_dst_date <- function(y) {
  nov1 <- as.Date(paste0(y, "-11-01"))
  nov1 + (7 - wday(nov1, week_start = 1)) %% 7
}

conflict_summary <- temp_data_corrected %>%
  group_by(stationID, date_time) %>%
  summarise(
    n_files = n_distinct(FileName),
    temp_difference = max(tempC, na.rm = TRUE) -
      min(tempC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    n_files > 1,
    temp_difference > 0
  ) %>%
  mutate(
    conflict_type = case_when(
      temp_difference <= 0.02 ~ "Minor (≤0.02°C)",
      temp_difference <= 0.1 ~ "Small (0.02–0.1°C)",
      temp_difference <= 1 ~ "Moderate (0.1–1°C)",
      TRUE ~ "Large (>1°C)"
    ),

    DST_conflict = (
      as.Date(date_time) == fall_dst_date(year(date_time)) &
        hour(date_time) == 1
    )
  )

View(conflict_summary)

conflict_summary %>%
  count(stationID, conflict_type)

##write.csv(conflict_summary,"E:/Shishir/postdoc/Data/Results/conflict_summary_v1.csv")


### Correcting time stamp issue and removing duplicate records ####
# correct the cibolo data for duplicate time stamp issue ####
# Shift timestamps for CiboloPreserveUp and CiboloPreserveDown 2022_07_28 files
# forward by 21 days

temp_data_corrected <- temp_data_joined %>%
  mutate(
    date_time_original = date_time,
    date_time = if_else(
      stationID %in% c(
        "CiboloPreserveUp",
        "CiboloPreserveDown"
      ) & grepl("2022_07_28$", FileName),
      date_time + days(21),
      date_time
    )
  )

## remove duplicate records with the exact same date time stamp and temperature
temp_clean <- temp_data_corrected %>%
  arrange(stationID, date_time, FileName) %>%
  distinct(stationID, date_time, .keep_all = TRUE)



### regularize sampling interval ####
# LeonI35 has 2 files with 1 min, and CiboloNorthrup has 5 min sampling interval


# Stations requiring resampling
stations_resample <- c(
  "LeonI35",
  "CiboloNorthrup"
)

# Resample both stations to 15-minute intervals
temp_resampled <- temp_clean %>%
  filter(stationID %in% stations_resample) %>%
  mutate(
    date_time = floor_date(date_time, unit = "15 minutes")
  ) %>%
  group_by(stationID, date_time) %>%
  summarise(
    tempC = mean(tempC, na.rm = TRUE),
    n_original_obs = n(),
    .groups = "drop"
  )

# Retain other stations unchanged
temp_other <- temp_clean %>%
  filter(!stationID %in% stations_resample)

# Combine
temp_clean_15min <- bind_rows(
  temp_other %>% dplyr::select(stationID, date_time, tempC),
  temp_resampled %>% dplyr::select(stationID, date_time, tempC)
) %>%
  arrange(stationID, date_time)


###### perform quality checks on different sites' temperature data before generating indices #####

# Step 1: Determine the sampling interval for each file
# Use the most common interval between consecutive timestamps

logger_intervals <- temp_clean_15min %>%
  filter(!is.na(date_time)) %>%
  distinct(stationID, date_time) %>%
  arrange(stationID, date_time) %>%
  group_by(stationID) %>%
  mutate(
    interval_min = as.numeric(
      difftime(date_time, lag(date_time), units = "mins")
    )
  ) %>%
  filter(!is.na(interval_min), interval_min > 0) %>%
  count(stationID, interval_min, name = "frequency") %>%
  arrange(stationID, desc(frequency), interval_min) %>%
  group_by(stationID) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(stationID, sampling_interval_min = interval_min)

# Step 2: Calculate observations per day
daily_counts <- temp_clean_15min %>%
  filter(!is.na(date_time)) %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(stationID, date) %>%
  summarise(
    n_obs_day = sum(!is.na(tempC)),
    .groups = "drop"
  )

# Step 3: Create file-level quality-control summary

temp_QC <- temp_clean_15min %>%
  group_by(stationID) %>%
  summarise(
    start_date = if (all(is.na(date_time))) as.POSIXct(NA)
    else min(date_time, na.rm = TRUE),

    end_date = if (all(is.na(date_time))) as.POSIXct(NA)
    else max(date_time, na.rm = TRUE),

    start_year = if (all(is.na(date_time))) NA_integer_
    else year(min(date_time, na.rm = TRUE)),

    end_year = if (all(is.na(date_time))) NA_integer_
    else year(max(date_time, na.rm = TRUE)),

    years_with_data = n_distinct(
      year(date_time[!is.na(date_time) & !is.na(tempC)])
    ),

    n_records = n(),

    n_obs = sum(!is.na(tempC) & !is.na(date_time)),

    missing_temp = sum(is.na(tempC)),

    missing_datetime = sum(is.na(date_time)),

    duplicate_timestamps = sum(
      duplicated(date_time) & !is.na(date_time)
    ),

    min_temp = if (all(is.na(tempC))) NA_real_
    else min(tempC, na.rm = TRUE),

    max_temp = if (all(is.na(tempC))) NA_real_
    else max(tempC, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    years_spanned = end_year - start_year + 1,

    days_spanned = as.numeric(
      as.Date(end_date) - as.Date(start_date)
    ) + 1
  ) %>%
  left_join(logger_intervals, by = "stationID") %>%
  left_join(
    daily_counts %>%
      group_by(stationID) %>%
      summarise(
        days_with_data = sum(n_obs_day > 0),
        median_obs_per_day = median(n_obs_day),
        min_obs_per_day = min(n_obs_day),
        max_obs_per_day = max(n_obs_day),
        .groups = "drop"
      ),
    by = "stationID"
  ) %>%
  mutate(
    expected_obs = as.numeric(
      difftime(end_date, start_date, units = "mins")
    ) / sampling_interval_min + 1,

    completeness_pct = round(
      100 * n_obs / expected_obs, 1
    ),

    days_completeness_pct = round(
      100 * days_with_data / days_spanned, 1
    )
  )

View(temp_QC)


#write.csv(temp_QC,"E:/Shishir/postdoc/Data/Results/temp_QC_v8.csv")






