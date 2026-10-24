library(sf)
library(raster)
library(ggspatial)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(purrr)
library(readr)
library(janitor)
library(lubridate)
library(dataRetrieval)
library(httr2)

# Set path
setwd("E:/Shishir/postdoc/Data")

##### Reading temperature logger data from upper San Antonio watershed ####

#read the site level meta data for temperature loggers
sites <- read.csv("Temperature/sites_v02.csv",header=T)

#Create a shape file using the lat, long of logger locations
site_loc <-  st_as_sf(sites, coords = c("logger_lon","logger_lat"),crs = 4326)

#Get Texas stream network and wshed
# https://research.fs.usda.gov/rmrs/projects/national-stream-internet#download-data
Tex_stream<-st_read("Stream/rmrs-flowline_tx12_nsi/Flowline_TX12_NSI.shp")

#Get watershed polygons at different HUC levels
Tex_wshed_HUC8 <- st_read("Watershed/NHD_H_Texas_State_Shape/Shape/WBDHU8.shp")
Tex_wshed_HUC6 <- st_read("Watershed/NHD_H_Texas_State_Shape/Shape/WBDHU6.shp")
Tex_wshed_HUC4 <- st_read("Watershed/NHD_H_Texas_State_Shape/Shape/WBDHU4.shp")

#HUC 6 seems like the correct resolution. Clip it to San Antonio watershed
SA_wshed <- Tex_wshed_HUC6 %>% filter(name == "San Antonio" | name == "Guadalupe")

# transform the logger locations to the same CRS as that of the watershed and
# then Select loggers within the San Antonio watershed
st_crs(SA_wshed) # this is in EPSG 4269
site_loc <- st_transform(site_loc, st_crs(SA_wshed))
SA_loggers <- st_filter(site_loc, SA_wshed, .predicate = st_intersects)

#Clip the stream network to San Antonio watershed
Tex_stream <- st_transform(Tex_stream,st_crs(SA_wshed))
SA_stream <- st_filter(Tex_stream, SA_wshed, .predicate = st_intersects)

#plot
ggplot() +
  coord_fixed() +
  theme_minimal() +
  #ggspatial::layer_spatial(Tex_wshed_HUC8, fill = NA, color = "grey")+
  ggspatial::layer_spatial(SA_wshed, fill = NA, color = "red")+
  #ggspatial::layer_spatial(Tex_wshed_HUC4, fill = NA, color = "orange")+
  #geom_sf_label(data = st_as_sf(Tex_wshed_HUC4),aes(label = name),fun.geometry = st_centroid,
  #stat = "sf_coordinates")+
  ggspatial::layer_spatial(site_loc, color = "black")+
  ggspatial::layer_spatial(SA_loggers, color = "green")+
  ggspatial::layer_spatial(SA_stream, color = "blue" )

# now get the filenames of the loggers within the watershed of interest
SA_temp_files <- SA_loggers %>% st_drop_geometry() %>% pull(FileName) %>% paste0(".csv")

# Set the folder path where all the temp records are stored
folder_path <- "E:/Shishir/postdoc/Data/Temperature/data_field_loggers_temp"

# Build full file paths
full_paths <- file.path(folder_path, SA_temp_files)

# Check if the paths of files actually exist in the folder
existing_paths <- full_paths[file.exists(full_paths)]

# 2 files are missing. One of them is not necessary and the other needs to be downloaded from USGS
missing_paths <- full_paths[!file.exists(full_paths)]
missing_filenames <- basename(missing_paths)

# Now read the rest of the files that exist, and add the filename as an attribute
names(existing_paths) <- sub("\\.csv$", "", basename(existing_paths))

# Reading the first 3 files to check if it works.
#temp_data = existing_paths[1:3] %>%
#  map_df(~read_csv(.x), .id = "FileName")

#names(temp_data)

#The files are supposed to have two columns named date_time and tempC but the column names differ
#between the files. So, standardize the file names
read_and_standardize <- function(file_path) {

  # Some temperature files are missing column headers. Check if it has the standard headers
  first_line <- read_lines(file_path, n_max = 1)
  has_headers <- any(grepl("date|time|temp", first_line, ignore.case = TRUE))



  if(has_headers){
    # Read the data frame
    df <- read_csv(file_path, show_col_types = FALSE)

    # check if the temperature is in Farenheit and set a flag
    is_fahrenheit <- any(grepl("°F", names(df), ignore.case = TRUE))

    # Clean names to a standard format "Date Time" becomes "date_time"
    df <- janitor::clean_names(df)

    df <- df %>%
      rename(
        # Find whichever column contains "date" or "time" and name it "date_time"
        date_time = matches("date|time"),
        # Find whichever column contains "temp" and name it "tempC"
        tempC = matches("temp")
      ) %>%
      mutate(
        tempC = as.numeric(tempC),
        # Convert Fahrenheit to Celsius where necessary
        tempC = if (is_fahrenheit) {
          (tempC - 32) * 5 / 9
        } else {
          tempC
        }
      )
  }else { #column headers are missing
    # add headers manually
    df <- read_csv(file_path, col_names = c("date_time", "tempC"), show_col_types = FALSE)
  }

  # parse date_time correctly
   df <- df %>% select(date_time, tempC) %>%
    mutate(
      # convert date_time to text first so lubridate can parse it consistently
      date_time = as.character(date_time),
      date_time = parse_date_time(date_time, orders = c(
                                                          "%m/%d/%Y %I:%M:%S %p",
                                                          "%m/%d/%y %I:%M:%S %p",
                                                          "%Y-%m-%d %H:%M:%S",
                                                          "%m/%d/%Y %H:%M:%S",
                                                          "%m/%d/%Y %H:%M",
                                                          "%Y-%m-%d %H:%M",
                                                          "%Y-%m-%d",              # Date only for midnight
                                                          "%m/%d/%Y",
                                                          "%m/%d/%y"
                                                        )),
    )
  return(df)
}

# read data and combine row-wise
temp_data = existing_paths %>%
  map_df(~read_and_standardize(.x), .id = "FileName")

#make sure all files are read
length(unique(temp_data$FileName))

##### combine temperature data with logger metadata to merge temperature records from different years ######

# same logger is used in multiple sites. loggerID is not a unique identifier
sites %>%
  group_by(loggerID) %>%
  summarise(
    n_files = n(),
    n_locations = n_distinct(logger_lon, logger_lat),
    .groups = "drop"
  ) %>%
  filter(n_locations > 1)

# Check if station IDs are unique
sites %>%
  group_by(stationID) %>%
  summarise(
    n_files = n(),
    n_locations = n_distinct(logger_lon, logger_lat),
    .groups = "drop"
  ) %>%
  filter(n_locations > 1)

station_check <- sites %>%
  group_by(stationID) %>%
  summarise(
    n_files = n(),
    n_loggers = n_distinct(loggerID),
    n_locations = n_distinct(logger_lon, logger_lat),
    n_COMIDs = n_distinct(COMID, na.rm = TRUE),
    .groups = "drop"
  )

station_check %>%
  filter(n_locations > 1 | n_COMIDs > 1)


# Yes, stations IDs are unique. Use stationID to combine logger records across different years

length(unique(SA_loggers$stationID)) ## there are 95 unique stations

# Select logger metadata attributes
logger_metadata <- SA_loggers %>%
  st_drop_geometry() %>%
  select(FileName,loggerID,project,stream,loggerType,stationID)

# Check that each filename has only one metadata record
logger_metadata %>%
  count(FileName) %>%
  filter(n > 1)

# Join logger ID to temperature observations
temp_data_joined <- temp_data %>%
  left_join(
    logger_metadata,
    by = "FileName",
    relationship = "many-to-one"
  )%>%
  arrange(stationID, date_time)

#check if all files matched
temp_data_joined %>%
  filter(is.na(stationID)) %>%
  distinct(FileName)

# now summarize by station ID
station_temp_summary <- temp_data_joined %>%
  group_by(stationID) %>%
  summarise(
    n_files = n_distinct(FileName),
    start_date = min(date_time, na.rm = TRUE),
    end_date = max(date_time, na.rm = TRUE),
    years_with_data = n_distinct(year(date_time)),
    n_obs = sum(!is.na(tempC)),
    .groups = "drop"
  )

#station_temp_summary has 93 rows though



###### Read related USGS gauges #####

###### perform quality checks on different sites' temperature data before generating indices #####

# Step 1: Determine the sampling interval for each file
# Use the most common interval between consecutive timestamps

logger_intervals <- temp_data_joined %>%
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
  select(stationID, sampling_interval_min = interval_min)

# Step 2: Calculate observations per day
daily_counts <- temp_data_joined %>%
  filter(!is.na(date_time)) %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(stationID, date) %>%
  summarise(
    n_obs_day = sum(!is.na(tempC)),
    .groups = "drop"
  )

# Step 3: Create file-level quality-control summary

temp_QC <- temp_data_joined %>%
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


#write.csv(temp_QC,"E:/Shishir/postdoc/Data/Results/temp_QC_v5.csv")


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


library(dplyr)
library(ggplot2)

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



