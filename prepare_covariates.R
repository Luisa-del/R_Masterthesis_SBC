#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# COVARIATES FOR OCCUPANCY ANALYSIS       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   MASTER THESIS     LUISA PFLUMM        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(raster)
library(terra)
library(sf)
library(DT)
library(mapview)

# 1. Import data =======================================================================================================

# Import studyarea
aoi <- st_read("C:/Users/s347553/Documents/Masterthesis/Data/SVN_SBC_provinces_final.shp")

# Import cameratrap data
#ct <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/Quy/corrected_CTtable_Quy_editLP_shareJN.csv")
# Remove duplicate stations
#ct <- ct[!duplicated(ct$station),]

# Create sf points from cttable
stations <- st_as_sf(ct, coords = c("long", "lat"),crs = 4326)
stations_utm = st_transform(stations, crs = st_crs(32649))

# Draw buffers around the camera trap stations
station_buff_100m_utm <- st_buffer(stations_utm, dist = 100)
station_buff_100m <- st_transform(station_buff_100m_utm, crs = st_crs(4326))


# # Import elevation raster
# elev <- terra::rast("D:/Luisa/covariates/SRTM30m_NAs_filled_SVN.tif")
# # Import LC probability raster
# LCprob <- terra::rast("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/RF_prob_td2.8_wsmp10000_spl0.7_scale10-100.tif")
# # Select specific class band for SBC
# hab <- LCprob$``


# Import elevation raster
elev <- raster::raster("D:/Luisa/covariates/SRTM30m_NAs_filled_SVN.tif")

# Import LC probability raster
#LCprob <- raster::stack("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/RF_prob_td2.8_wsmp10000_spl0.7_scale10-100.tif")

# Select specific class band for SBC
hab <- LCprob$Semidry.Dry.Forest




# 2. Plot covariate rasters =================================================================================

## Elevation =============================================================================================

# Plot data
plot(elev, main = paste0("Elevation (30x30m)"), col = hcl.colors(100))
#plot(stations, pch = 16, add=TRUE)
plot(station_buff_100m$geometry, add=TRUE)
plot(aoi$geometry, add=TRUE)


## Semidry-Dry Forest class =============================================================================================

plot(hab, main = paste0("Semi-dry Forest (10x10m)"), col = hcl.colors(100))
#plot(stations, pch = 16, add=TRUE)
plot(station_buff_100m$geometry, add=TRUE)
plot(aoi$geometry, add=TRUE)


## ... ==============================================================================================================






# 3. Extract values at stations =========================================================================================
  
#> Consider buffer around station points, because the values can vary significantly from pixel to pixel. 
#> With the radius of 100m this evens out a bit and becomes more representative for the habitat patch (not only the point).
#> Especially if the points are on the edge/corner of pixels this can be relevant (1m to the side and we have a completely 
#> different value).Maybe consider buffer for elevation and village density.
#> Extraction is like for spatialpoints with `extract()`, but you have to set the argument fun = "mean". 
#> Then the function calculates the mean of the extracted cells automatically* 

  
## Elevation ==========================================================================================================

# Extract values from buffered points
station_buff_100m$elevation <- raster::extract(elev, station_buff_100m, method="simple", fun = mean, df = F) 

# Create dataframe
elevation <- data.frame(station = station_buff_100m$station, elevation = station_buff_100m$elevation) 
  

  # # alternative: Extract values from points
  # stations$elevation_p <- raster::extract(elev,stations, method="simple",df = F)
  # # Create a data frame containing station name and elevation:
  # elevation_p <- data.frame(station = stations$station,
  #                         elevation_p = stations$elevation_p)
  # 
  # # Compare 'old' elevation values with 'new' elevation values from buffer
  # compare_elev <- cbind(elevation_p, elevation)
  # #rownames(compare_elev) <- compare_elev$station
  # compare_elev <- compare_elev[-c(1,3)]
  # datatable(round(compare_elev,2),
  #           extensions = 'FixedColumns',
  #           options = list(
  #             scrollX = TRUE,
  #             scrollCollapse = TRUE))




## Semidry-Dry Forest class =========================================================
# (td 2.8, smp10000, spl0.7, scale10-100)

# Extract landcover values from buffered points
station_buff_100m$habitat <- raster::extract(hab, station_buff_100m, method="simple", fun = mean, df = F) 

# Create dataframe
habitat <- data.frame(station = station_buff_100m$station, habitat = station_buff_100m$hab) 
  
  
  # # alternative: Extract values from points 
  # stations$habitat_p <- raster::extract(hab,stations, method="simple",df = F) 
  # # Create a data frame containing station name and elevation: 
  # habitat_p <- data.frame(station = stations$station, 
  #                         habitat_p = stations$habitat_p) 
  # 
  # # Compare 'old' elevation values with 'new' elevation values from buffer 
  # compare_hab <- cbind(habitat_p, habitat) 
  # #rownames(compare_hab) <- compare_hab$station 
  # compare_hab <- compare_hab[-c(1,3)] 
  # datatable(round(compare_hab,2), 
  #           extensions = 'FixedColumns', 
  #           options = list( 
  #             scrollX = TRUE, 
  #             scrollCollapse = TRUE)) 
  

## ... ==============================================================================================================





# 4. Combine and adjust covariate tables ========================================================================

# Combine dataframes 
covariates <- cbind(elevation, habitat) 
  
# CHECK IF ALL COVARIATES HAVE SAME ORDER BY STATION! 

# Set stations as rownames 
rownames(covariates) <- covariates$station 

# Delete all station-columns 
remove.stations <- "station" 
covariates <- covariates[, !(names(covariates) %in% remove.stations)] 

# Scale continuous covariates, check dimension (length) and NA values 
# matrix contains attributes we need for backtransforming for prediction 
covariates_scaled_matrix <- scale(covariates) 
covariatesx <- as.data.frame(covariates_scaled_matrix) 
names(covariatesx) <- paste0("scale.", names(covariatesx)) 
  
# combine covariates and scaled matrix 
covariates <- cbind(covariates, covariatesx) 
  
# check dimension 
dim(covariates) 
  
# check for NAs and remove if existing 
NAs <- apply(covariates, 1, function(x){any(is.na(x))}) 
sum(NAs) 
  
# plot dataset 
datatable(round(covariates,2),extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE)) 






# 5. Export covariates ===============================================================================================

# save covariates as csv file 
#write.csv(covariates, "C:/Users/s347553/Documents/Masterthesis/Data/CT/covariates_quycttable_rmdoublestation_elev_hab3_td2.8_smp10000_spl0.7_scale10-100.csv") 




# ## Import already existing covariate dataset
# cov <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/covariates_update1.csv") #based on R product, corrected station
# rownames(cov) <- cov$X
# covariates <- cov[-1]
