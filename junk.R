#The 2 missing files are associated with USGS gauges
# 1. ID08170990_nwis00010_JacobsWell_2024_12_31.csv and
# 2. ID08181500_nwis00010_Medina_2024_12_31.csv
# Out of these, Jacobs well is outside the study area. Read the Median guage from USGS

site_id <- "USGS-08181500"
param_temp <- "00010" # Code for temperature

instant_temp <- read_waterdata_continuous(
  monitoring_location_id = site_id,
  parameter_code = param_temp,
  time = c("2012-01-01","2012-04-01"),
  #time = c(as.Date("2009-01-01"), as.Date("2009-12-01"))
  #"2007-10-01T00:00:00Z/2008-03-01T00:00:00Z"
  chunk_size = NA
)

#Download quarterly data

# 1. Generate start and end dates for every quarter from 2012 to 2024
quarters <- data.frame(
  start = seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "3 months")
) %>%
  mutate(
    # End date is the day before the next quarter starts
    end = seq(as.Date("2012-04-01"), as.Date("2013-01-01"), by = "3 months") - 1,
    # Format as characters to avoid the sprintf error
    start_str = format(start, "%Y-%m-%d"),
    end_str = format(end, "%Y-%m-%d")
  )

# 2. Define a safe function to download data for a single quarter
fetch_quarterly_data <- function(start_d, end_d) {
  message(paste("Downloading data from", start_d, "to", end_d, "..."))
  # Pause for 3 seconds before executing the request to respect API limits
  Sys.sleep(3)
  # Using tryCatch to handle quarters where no data might exist without crashing the loop
  tryCatch({
    data <- read_waterdata_continuous(
      monitoring_location_id = site_id,
      parameter_code = param_temp,
      time = c(start_d, end_d),
      limit = 5000
    )
    return(data)
  }, error = function(e) {
    message(paste("Warning: Failed to download or no data for period:", start_d, "to", end_d))
    return(NULL)
  })
}

# 3. Loop through all quarters, fetch data, and bind rows dynamically
# purrr::map2_dfr automatically binds the resulting data frames together
all_temp_data <- map2_dfr(quarters$start_str, quarters$end_str, fetch_quarterly_data)




df$date_time =  as.character(df$date_time)
#date_time =  parse_date_time(date_time, orders = c("mdy IMS p", "ymd HMS","%m/%d/%Y %H:%M"))
df$parsed_date_time =  parse_date_time(df$date_time, orders = c(
  "%m/%d/%Y %I:%M:%S %p",
  "%Y-%m-%d %H:%M:%S",
  "%m/%d/%Y %H:%M:%S",
  "%m/%d/%Y %H:%M",
  "%Y-%m-%d %H:%M",
  "%Y-%m-%d",              # Date only for midnight
  "%m/%d/%Y",
  "%m/%d/%y"
))
head(date_time)
parse_date_time(as.character("3/12/2022 16:45"),orders = c("mdy IMS p", "ymd HMS","mdy HM"))
parse_date_time(as.character(df$date_time[1]),orders = c("mdy IMS p", "ymd HMS","mdy HM"))

identical(date_time[1], "3/12/2022 16:45")


temp_data %>%
  group_by(FileName) %>%
  #filter(FileName == "ID20880805_V2_CiboloPreserveDown_2024_02_13") %>%
  filter(FileName == "ID20987513_V2_SanAntonioRoosevelt_2024_02_09") %>%
  summarise(
    start_date = if (all(is.na(date_time))) as.POSIXct(NA)
    else min(date_time, na.rm = TRUE))
