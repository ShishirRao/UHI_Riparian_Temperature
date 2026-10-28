# tempQC with stationID as the merge variable shows substantial duplication of records
overlapping_records <- temp_data_joined %>%
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


### plotting and observing stations with duplicate temperature records ###
#write.csv(overlap_summary,"E:/Shishir/postdoc/Data/Results/overlap_summary_v1.csv")

conflicts <- temp_data_joined %>%
  filter(stationID == "Honey29") %>%
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




honey29 <- temp_data_joined %>%
  filter(FileName %in% c(
    "ID21321693_BT_Honey29_2022_08_16",
    "ID21321693_BT_Honey29_2023_05_10",
    "ID21321693_BT_Honey29_2024_11_11"
  ))

ggplot(honey29, aes(x = date_time, y = tempC, color = FileName)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  labs(
    title = "Honey29: Temperature Records Across Three Files",
    x = "Date",
    y = "Water temperature (°C)",
    color = "Source file",
  ) +
  theme_minimal() + theme(legend.position = "bottom")+facet_wrap(FileName~.)

### Cibolo Preserve Up ###

#plot as it is
cibolo <- temp_data_joined %>%
  filter(FileName %in% c(
    "ID20880806_V2_CiboloPreserveUp_2021_04_15",
    "ID20880806_V2_CiboloPreserveUp_2022_07_28",
    "ID20880806_V2_CiboloPreserveUp_2024_02_13",
    "ID20880805_V2_CiboloPreserveDown_2021_03_25",
    "ID20880805_V2_CiboloPreserveDown_2022_07_28",
    "ID20880805_V2_CiboloPreserveDown_2024_02_13"
  )) %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(FileName,stationID, date) %>%
  summarise(
    mean_temp = mean(tempC, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(cibolo, aes(date , mean_temp, color = FileName)) +
  geom_line(linewidth = 0.5) +
  labs(
    x = "Date",
    y = "Daily mean water temperature (°C)",
    color = "File"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") + facet_wrap(~stationID)


url <- paste0(
  "https://www.ncei.noaa.gov/access/services/data/v1?",
  "dataset=daily-summaries",
  "&stations=USW00012971",
  #"&stations=USW00012921",
  "&startDate=2021-01-01",
  "&endDate=2023-12-31",
  "&dataTypes=TMAX,TMIN",
  "&units=metric",
  "&includeAttributes=false",
  "&format=csv"
)

air_temp <- read_csv(url) %>%
  mutate(
    DATE = as.Date(DATE),
    air_mean = 10+(TMAX + TMIN) / 2
  )

ggplot(air_temp, aes(DATE, air_mean)) +
  geom_line(color = "darkorange") +
  labs(
    x = "Date",
    y = "Estimated daily mean air temperature (°C)",
    title = "New Braunfels air temperature, 2021–2023"
  ) +
  theme_minimal()

# plot cibolo up, down and air temperature together
# Prepare water temperature data
water_plot <- cibolo %>%
  mutate(
    source = stationID,
    temperature = mean_temp
  ) %>%
  select(date, source, FileName, temperature)

# Prepare air temperature data
air_plot <- air_temp %>%
  transmute(
    date = DATE,
    source = "New Braunfels Air",
    FileName = "Air temperature",
    temperature = air_mean
  )

# Combine air and water temperature
combined_temp <- bind_rows(water_plot, air_plot)

# Plot with separate facets
ggplot(
  combined_temp ,
  aes(x = date, y = temperature, color = FileName)
) +
  geom_line(linewidth = 1) +
  facet_wrap(source~., ncol = 1) +
  labs(
    title = "Cibolo Preserve: Water and Air Temperature",
    x = "Date",
    y = "Daily mean temperature (°C)",
    color = "File"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

# Plot without facets
ggplot(
  combined_temp ,
  aes(x = date, y = temperature, color = FileName)
) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Cibolo Preserve: Water and Air Temperature",
    x = "Date",
    y = "Daily mean temperature (°C)",
    color = "File"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )



## moving cibolo preserve up 2022_07_28 files by a week ahead

water_plot <- cibolo %>%
  mutate(
    date = if_else(
      grepl("2022_07_28$", FileName),
      #date + days(7*3),
      date + days(20),
      date
    ),
    source = stationID,
    temperature = mean_temp
  ) %>%
  select(date, source, FileName, temperature)


# Combine air and water temperature
combined_temp <- bind_rows(water_plot, air_plot)


ggplot(
  combined_temp[combined_temp$date <= ymd("2022-03-27"),] ,
  aes(x = date, y = temperature, color = FileName)
) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Cibolo 2022_07_28: shifted by 3 weeks",
    x = "Date",
    y = "Daily mean temperature (°C)",
    color = "File"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# duplicating air temp

# Prepare water temperature data
water_plot <- cibolo %>%
  transmute(
    date,
    stationID,
    source = FileName,
    temperature = mean_temp
  )

# Add air temperature to both stations
air_plot <- bind_rows(
  air_temp %>%
    transmute(
      date = DATE,
      stationID = "CiboloPreserveUp",
      source = "Air temperature",
      temperature = air_mean
    ),

  air_temp %>%
    transmute(
      date = DATE,
      stationID = "CiboloPreserveDown",
      source = "Air temperature",
      temperature = air_mean
    )
)

# Combine
combined_temp <- bind_rows(water_plot, air_plot)

# Plot
ggplot(
  combined_temp %>% filter(date < ymd("2021-09-15")),
  aes(x = date, y = temperature, color = source)
) +
  geom_line(linewidth = 1) +
  #facet_wrap(~stationID, ncol = 1) +
  labs(
    title = "Cibolo Preserve: Water and Air Temperature",
    x = "Date",
    y = "Daily mean temperature (°C)",
    color = "Data source"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(ncol = 1))


cibolo_2022 <- cibolo %>%
  filter(grepl("2022_07_28", FileName))

ggplot() +
  geom_line(
    data = air_temp,
    aes(x = DATE, y = air_mean),
    color = "grey50",
    linewidth = 0.4
  ) +
  geom_line(
    data = cibolo_2022,
    aes(x = date, y = mean_temp, color = stationID),
    linewidth = 0.6
  ) +
  labs(
    title = "Cibolo Preserve: Comparing 2022 Logger Files",
    x = "Date",
    y = "Daily mean temperature (°C)",
    color = "Station"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



## examinining cibolo down on 03/25


cibolo_down_0325 <- temp_data_joined %>%
  filter(
    FileName %in% c(
      "ID20880805_V2_CiboloPreserveDown_2021_03_25",
      "ID20880805_V2_CiboloPreserveDown_2022_07_28"
    ),
    as.Date(date_time) == ymd("2021-03-25")
  ) %>%
  mutate(
    FileName = if_else(
      grepl("2021_03_25", FileName),
      "down_temp_2021",
      "down_temp_2022"
    )
  ) %>%
  select(date_time, FileName, tempC) %>%
  pivot_wider(
    names_from = FileName,
    values_from = tempC
  ) %>%
  arrange(date_time)

View(cibolo_down_0325)

cibolo_up_0325 <- temp_data_joined %>%
  filter(
    FileName %in% c(
      "ID20880806_V2_CiboloPreserveUp_2021_04_15",
      "ID20880806_V2_CiboloPreserveUp_2022_07_28"
    ),
    as.Date(date_time) == ymd("2021-03-25")
  ) %>%
  mutate(
    FileName = if_else(
      grepl("2021_04_15", FileName),
      "up_temp_2021",
      "up_temp_2022"
    )
  ) %>%
  select(date_time, FileName, tempC) %>%
  pivot_wider(
    names_from = FileName,
    values_from = tempC
  ) %>%
  arrange(date_time) %>%
  mutate(
    temp_diff = up_temp_2022 - up_temp_2021
  )

View(cibolo_up_0325)

cibolo_0325 <- temp_data_joined %>%
  filter(
    FileName %in% c(
      "ID20880805_V2_CiboloPreserveDown_2021_03_25",
      "ID20880805_V2_CiboloPreserveDown_2022_07_28",
      "ID20880806_V2_CiboloPreserveUp_2021_04_15",
      "ID20880806_V2_CiboloPreserveUp_2022_07_28"
    ),
    as.Date(date_time) == ymd("2021-03-25")
  ) %>%
  mutate(
    source = case_when(
      grepl("CiboloPreserveDown_2021", FileName) ~ "down_temp_2021",
      grepl("CiboloPreserveDown_2022", FileName) ~ "down_temp_2022",
      grepl("CiboloPreserveUp_2021", FileName) ~ "up_temp_2021",
      grepl("CiboloPreserveUp_2022", FileName) ~ "up_temp_2022"
    )
  ) %>%
  select(date_time, source, tempC) %>%
  pivot_wider(
    names_from = source,
    values_from = tempC
  ) %>%
  arrange(date_time)

View(cibolo_0325)


# Check Cibolo 2022#

cibolo_2022 <- temp_data_joined %>%
  filter(FileName %in% c(
    "ID20880805_V2_CiboloPreserveDown_2022_07_28",
    "ID20880806_V2_CiboloPreserveUp_2022_07_28"
  )) %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(stationID, date) %>%
  summarise(
    mean_temp = mean(tempC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(stationID) %>%
  complete(
    date = seq(min(date), max(date), by = "day")
  ) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    rolling_mean = slide_dbl(
      mean_temp,
      mean,
      .before = 7,
      .after = 7,
      .complete = TRUE
    ),
    anomaly = mean_temp - rolling_mean
  ) %>%
  ungroup() %>%
  select(stationID, date, anomaly) %>%
  pivot_wider(
    names_from = stationID,
    values_from = anomaly
  )

cibolo_2022 %>%
  summarise(
    correlation = cor(
      CiboloPreserveUp,
      CiboloPreserveDown,
      use = "complete.obs"
    )
  )



temp_data_joined %>%
  filter(FileName %in% c(
    "ID20880805_V2_CiboloPreserveDown_2022_07_28",
    "ID20880806_V2_CiboloPreserveUp_2022_07_28"
  )) %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(stationID, date) %>%
  summarise(
    mean_temp = mean(tempC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = stationID,
    values_from = mean_temp
  ) %>%
  summarise(
    n_days = sum(complete.cases(
      CiboloPreserveUp, CiboloPreserveDown
    )),
    correlation = cor(
      CiboloPreserveUp,
      CiboloPreserveDown,
      use = "complete.obs"
    ),
    mean_difference = mean(
      CiboloPreserveUp - CiboloPreserveDown,
      na.rm = TRUE
    ),
    sd_difference = sd(
      CiboloPreserveUp - CiboloPreserveDown,
      na.rm = TRUE
    )
  )



## correlation with air temp with all 6 cibolo records

cibolo_daily <- temp_data_joined %>%
  filter(stationID %in% c(
    "CiboloPreserveUp",
    "CiboloPreserveDown"
  )) %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(FileName, stationID, date) %>%
  summarise(
    water_mean = mean(tempC, na.rm = TRUE),
    .groups = "drop"
  )


cibolo_air <- cibolo_daily %>%
  left_join(
    air_temp %>%
      select(DATE, air_mean),
    by = c("date" = "DATE")
  )

cibolo_correlations <- cibolo_air %>%
  group_by(stationID, FileName) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),

    n_days = sum(
      !is.na(water_mean) & !is.na(air_mean)
    ),

    correlation = cor(
      water_mean,
      air_mean,
      use = "complete.obs"
    ),

    .groups = "drop"
  ) %>%
  arrange(stationID, FileName)

View(cibolo_correlations)


# Calculate shifted correlations for the 2022 Down file
shifted_correlations <- cibolo_daily %>%
  filter(
    FileName ==
      "ID20880805_V2_CiboloPreserveDown_2022_07_28"
  ) %>%
  group_by(FileName) %>%
  group_modify(~ {

    water <- .x

    # Function to calculate correlation for a given shift
    get_cor <- function(shift_days) {

      shifted <- water %>%
        mutate(date_shifted = date + days(shift_days)) %>%
        left_join(
          air_temp %>% select(DATE, air_mean),
          by = c("date_shifted" = "DATE")
        )

      cor(
        shifted$water_mean,
        shifted$air_mean,
        use = "complete.obs"
      )
    }

    tibble(
      cor_plus_1week = get_cor(7),
      cor_plus_2weeks = get_cor(14),
      cor_plus_3weeks = get_cor(21),
      cor_plus_4weeks = get_cor(28)
    )
  }) %>%
  ungroup()

# Append results to existing correlation table
cibolo_correlations <- cibolo_correlations %>%
  left_join(
    shifted_correlations,
    by = "FileName"
  )

View(cibolo_correlations)


## shifting both files and adding correlation values as rows

# Step 1: Calculate daily mean water temperatures
cibolo_daily <- temp_data_joined %>%
  filter(stationID %in% c(
    "CiboloPreserveUp",
    "CiboloPreserveDown"
  )) %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(FileName, stationID, date) %>%
  summarise(
    water_mean = mean(tempC, na.rm = TRUE),
    .groups = "drop"
  )


# Step 2: Create original and shifted datasets

# Original records (no shift)
cibolo_original <- cibolo_daily %>%
  mutate(
    shift_days = 0,
    date_shifted = date
  )

# Shift both 2022_07_28 files by 1, 2, 3 and 4 weeks
cibolo_shifted <- cibolo_daily %>%
  filter(grepl("2022_07_28$", FileName)) %>%
  tidyr::crossing(shift_days = c(7, 14, 21, 28)) %>%
  mutate(
    date_shifted = date + days(shift_days)
  )

# Combine original and shifted observations
cibolo_all <- bind_rows(
  cibolo_original,
  cibolo_shifted
)


# Step 3: Join corresponding air temperatures
cibolo_air <- cibolo_all %>%
  left_join(
    air_temp %>%
      select(DATE, air_mean),
    by = c("date_shifted" = "DATE")
  )


# Step 4: Calculate correlations
cibolo_correlations <- cibolo_air %>%
  group_by(stationID, FileName, shift_days) %>%
  summarise(
    start_date = min(date_shifted),
    end_date = max(date_shifted),

    n_days = sum(
      !is.na(water_mean) & !is.na(air_mean)
    ),

    correlation = if (
      sum(complete.cases(water_mean, air_mean)) >= 3
    ) {
      cor(water_mean, air_mean, use = "complete.obs")
    } else {
      NA_real_
    },

    .groups = "drop"
  ) %>%
  arrange(stationID, FileName, shift_days)

View(cibolo_correlations)


### finding the exact shift in days between 14 and 28 days

shift_test <- cibolo_daily %>%
  filter(grepl("2022_07_28$", FileName)) %>%
  tidyr::crossing(shift_days = 14:28) %>%
  mutate(date_shifted = date + days(shift_days)) %>%
  left_join(
    air_temp %>% select(DATE, air_mean),
    by = c("date_shifted" = "DATE")
  ) %>%
  group_by(stationID, FileName, shift_days) %>%
  summarise(
    correlation = cor(
      water_mean, air_mean,
      use = "complete.obs"
    ),
    .groups = "drop"
  )

?crossing

ggplot(
  shift_test,
  aes(x = shift_days, y = correlation, color = stationID)
) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 21, linetype = "dashed") +
  labs(
    x = "Forward shift (days)",
    y = "Air–water temperature correlation",
    color = "Station"
  ) +
  theme_minimal()

# Correlation peaks at 20 days


