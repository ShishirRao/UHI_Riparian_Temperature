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

# Set path
setwd("E:/Shishir/postdoc/Data")

#read the site level meta data for temperature loggers
sites <- read.csv("sites_v02.csv",header=T)

#Create a shape file using the lat, long of logger locations
site_loc=  st_as_sf(sites, coords = c("logger_lon","logger_lat"),crs = 4326)

#Get Texas stream network and wshed
Tex_stream = st_read("rmrs-flowline_tx12_nsi/Flowline_TX12_NSI.shp")

#Get watershed polygons at different HUC levels
Tex_wshed_HUC8 = st_read("NHD_H_Texas_State_Shape/Shape/WBDHU8.shp")
Tex_wshed_HUC6 = st_read("NHD_H_Texas_State_Shape/Shape/WBDHU6.shp")
Tex_wshed_HUC4 = st_read("NHD_H_Texas_State_Shape/Shape/WBDHU4.shp")

#HUC 6 seems like the correct resolution. Clip it to San Antonio watershed
SA_wshed = Tex_wshed_HUC6 %>% filter(name == "San Antonio" | name == "Guadalupe")

# transform the logger locations to the same CRS as that of the watershed and
# then Select loggers within the San Antonio watershed
st_crs(SA_wshed) # this is in EPSG 4269
site_loc <- st_transform(site_loc, st_crs(SA_wshed))
SA_loggers = st_filter(site_loc, SA_wshed, .predicate = st_intersects)

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
folder_path <- "E:/Shishir/postdoc/Data/data_field_loggers_temp_v01"

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
temp_data = existing_paths[1:3] %>%
  map_df(~read_csv(.x), .id = "FileName")

names(temp_data)

#The files are supposed to have two columns named date_time and tempC but the column names differ
#between the files. So, standardize the file names
read_and_standardize <- function(file_path) {
  # Read the data frame
  df <- read_csv(file_path, show_col_types = FALSE)

  # Clean names to a standard format "Date Time" becomes "date_time"
  df <- janitor::clean_names(df)

  # Dynamic Column Renaming based on keyword matching (grep style)
  df <- df %>%
    rename(
      # Find whichever column contains "date" or "time" and name it "date_time"
      date_time = matches("date|time"),
      # Find whichever column contains "temp" or "deg" and name it "tempC"
      tempC = matches("temp|deg")
    ) %>% select(date_time, tempC) %>%
    mutate(
      # convert date_time to text first so lubridate can parse it consistently
      date_time = as.character(date_time),
      date_time = parse_date_time(date_time, orders = c("mdy IMS p", "ymd HMS"))
    )
  return(df)
}

# read again. The column names are changed but the date time format varies across files. So rbind won't work
temp_data = existing_paths %>%
  map_df(~read_and_standardize(.x), .id = "FileName")


