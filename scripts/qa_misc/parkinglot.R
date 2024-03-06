# code parking lot

##### from folder buildng:

#this did not work - upon reading back in, the image is upside down.
# switching from stars to terra
#match crs (5070 CONUS Albers) to hucs crs (3310 CA Albers)
# (aspect <- read_stars("data_topo/LF2020_Asp_220_CONUS/LC20_Asp_220.tif")) 
# aspect_trans <- st_transform(aspect, 3310)
# st_crs(aspect_trans)
# write_stars(aspect_trans, file.path("data_topo", "aspect.tif"))
#(elev <- read_stars("data_topo/LF2020_Elev_220_CONUS/LC20_Elev_220.tif")) 
# elev_trans <- st_transform(elev, 3310)
# write_stars(elev_trans, file.path("data_topo", "elevation.tif"))
#(slope <- read_stars("data_topo/LF2020_SlpD_220_CONUS/LC20_SlpD_220.tif"))
# slope_trans <- st_transform(slope, 3310)
# write_stars(slope_trans, file.path("data_topo", "slope.tif"))
# aspect <- read_stars(file.path("data_topo", "aspect.tif"))
# elev <- read_stars(file.path("data_topo", "elevation.tif"))
# slope2 <- read_stars(file.path("data_topo", "slope.tif"))


# this_huc_raster <- st_rasterize(sf = this_huc_shp[,"huc12"], 
#                                 template = this_huc_buffer_raster,
#                                 align = TRUE)
# 
# this_huc_indicator <- as(!this_huc_raster==0, "Raster")


###### from fuel addition

# #spatial
# this_huc_shp <- hucs_shp %>% filter(huc12 == this_huc)
# #create 1200m buffer
# this_huc_buffer <- st_buffer(this_huc_shp, 1200)
#this_fuel_huc_crop <- crop(this_fuel, this_huc_buffer, mask=T)

# fuel_out_name <- paste0(this_runid_str, "_",
#                         this_layer, "_",
#                         this_year, "_",
#                         this_huc)

# near did not work correctly for fml????
# #resampling to match exactly to this indicator / all rasters
# this_fuel_huc <- resample(this_fuel, 
#                           this_indicator, 
#                           method=this_method, 
#                           threads=T)

