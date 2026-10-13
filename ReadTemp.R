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

# 45 files are missing out of 191 sites
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

  #file_path = "E:/Shishir/postdoc/Data/Temperature/data_field_loggers_temp/ID10862160_V2_Honey30A_2024_11_11.csv"
  # Some temperature files are missing column headers
  first_line <- read_lines(file_path, n_max = 1)

  # check if it has the standard headers
  has_headers <- any(grepl("date|time|temp|deg", first_line, ignore.case = TRUE))

  if(has_headers){
    # Read the data frame
    df <- read_csv(file_path, show_col_types = FALSE)

    # Clean names to a standard format "Date Time" becomes "date_time"
    df <- janitor::clean_names(df)

    df <- df %>%
      rename(
        # Find whichever column contains "date" or "time" and name it "date_time"
        date_time = matches("date|time"),
        # Find whichever column contains "temp" or "deg" and name it "tempC"
        tempC = matches("temp|deg")
      )
  }else { #column headers are missing
    # add headers manually
    df <- read_csv(file_path, col_names = c("date_time", "tempC"), show_col_types = FALSE)
  }

  # Column Renaming based on keyword matching (grep style)
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
                                                          "%Y-%m-%d %H:%M"
                                                        )),
      tempC = as.numeric(tempC)
    )
  return(df)
}


date_time =  as.character(df$date_time)
#date_time =  parse_date_time(date_time, orders = c("mdy IMS p", "ymd HMS","%m/%d/%Y %H:%M"))
date_time =  parse_date_time(date_time, orders = c(
  "%m/%d/%Y %I:%M:%S %p",
  "%m/%d/%y %I:%M:%S %p",
  "%Y-%m-%d %H:%M:%S",
  "%m/%d/%Y %H:%M:%S",
  "%m/%d/%Y %H:%M",
  "%Y-%m-%d %H:%M"
))
head(date_time)
parse_date_time(as.character("5/10/2023  9:00:00 AM"),orders = c(
                                                                    "%m/%d/%Y %I:%M:%S %p",
                                                                    "%Y-%m-%d %H:%M:%S",
                                                                    "%m/%d/%Y %H:%M:%S",
                                                                    "%m/%d/%Y %H:%M",
                                                                    "%Y-%m-%d %H:%M"
                                                                  ))

# read data and combine row-wise
temp_data = existing_paths %>%
  map_df(~read_and_standardize(.x), .id = "FileName")

#make sure all files are read
length(unique(temp_data$FileName))


###### Read related USGS gauges #####

###### perform quality checks on different sites' temperature data before generating indices #####

# Step 1: Determine the sampling interval for each logger
# Use the most common interval between consecutive timestamps

logger_intervals <- temp_data %>%
  filter(!is.na(date_time)) %>%
  distinct(FileName, date_time) %>%
  arrange(FileName, date_time) %>%
  group_by(FileName) %>%
  mutate(
    interval_min = as.numeric(
      difftime(date_time, lag(date_time), units = "mins")
    )
  ) %>%
  filter(!is.na(interval_min), interval_min > 0) %>%
  count(FileName, interval_min, name = "frequency") %>%
  arrange(FileName, desc(frequency), interval_min) %>%
  group_by(FileName) %>%
  slice(1) %>%
  ungroup() %>%
  select(FileName, sampling_interval_min = interval_min)

# Step 2: Calculate observations per day
daily_counts <- temp_data %>%
  filter(!is.na(date_time)) %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(FileName, date) %>%
  summarise(
    n_obs_day = sum(!is.na(tempC)),
    .groups = "drop"
  )

# Step 3: Create site-level quality-control summary

temp_QC <- temp_data %>%
  group_by(FileName) %>%
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
  left_join(logger_intervals, by = "FileName") %>%
  left_join(
    daily_counts %>%
      group_by(FileName) %>%
      summarise(
        days_with_data = sum(n_obs_day > 0),
        median_obs_per_day = median(n_obs_day),
        min_obs_per_day = min(n_obs_day),
        max_obs_per_day = max(n_obs_day),
        .groups = "drop"
      ),
    by = "FileName"
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

temp_data %>%
  filter(is.na(date_time)) %>%
  count(FileName) %>%
  arrange(desc(n))

#write.csv(temp_QC,"E:/Shishir/postdoc/Data/Results/temp_QC_v1.csv")
