
#Extract the stream attributes from NHDplus layers for each logger location. This is for later use

# Join each temperature logger to its nearest NHD stream segment
SA_loggers_NHD <- st_join(
  site_loc,
  Tex_stream,
  join = st_nearest_feature,
  left = TRUE
)

SA_loggers_NHD %>%
  st_drop_geometry() %>%
  summarise(
    n_total = n(),
    n_match = sum(COMID.x == COMID.y, na.rm = TRUE),
    n_mismatch = sum(COMID.x != COMID.y, na.rm = TRUE),
    n_missing = sum(is.na(COMID.x) | is.na(COMID.y))
  )

# All COMIDs in the flowline shapefile
nhd_comids <- unique(Tex_stream$COMID)

SA_loggers_NHD <- SA_loggers_NHD %>%
  mutate(
    original_comid_exists = COMID.x %in% nhd_comids,
    comid_match = COMID.x == COMID.y
  )

SA_loggers_NHD %>%
  st_drop_geometry() %>%
  count(original_comid_exists, comid_match)


#### let us get the BFI values ####
# https://www.epa.gov/national-aquatic-resource-surveys/streamcat-web-tool-table-view

BFI_Region12 <- read.csv("Stream/BFI_Region12.csv",header=T)
EPA_BFI <- read.csv("Stream/EPA_StreamCat_BFI_Texas.csv",header=T)
