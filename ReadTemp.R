library(sf)
library(raster)
library(ggspatial)
library(ggplot2)

setwd("E:/Shishir/postdoc/Data")

#read the site level meta data
sites <- read.csv("sites_v02.csv",header=T)

#Get San Antonio stream network and wshed
SA_stream = st_read("rmrs-flowline_tx12_nsi/Flowline_TX12_NSI.shp")

Tex_wshed = st_read("NHD_H_Texas_State_Shape/Shape/WBDHU2,shp")

ggplot() +
  coord_fixed() +
  theme_minimal() +
  ggspatial::layer_spatial(Tex_wshed, fill = NA, color = "gray90") +
  ggspatial::layer_spatial(SA_stream, color = "blue" )
