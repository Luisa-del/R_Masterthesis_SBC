library(sf)
library(sp)
library(raster)
library(terra)
library(dplyr)
library(tidyr)
library(caret)
library(sp)
library(data.table)
library(magrittr)



# Import data -------------------------------------------------------------------------------------------------------------------

aoi <- st_read("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/SVN_SBC_provinces_final.shp")
aoi_utm <- st_read("C:/Users/s347553/Documents/Masterthesis/Data/SVN_SBC_provinces_final_utm49nqgis.shp")
td <- st_read("C:/Users/s347553/Documents/Masterthesis/Data/trainingdata/LC_classes_2.8.shp")
td <- st_make_valid(td)
td_utm <- st_transform(td, 32649)

#samp_rst200 <- terra::rast("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_200mscale.tif")
samp_rst10 <- terra::rast("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale.tif")

classdf <- data.frame(classvalues = c(1,2,3,4,5,6,7,8,9), 
                      landcover = c("Evergreen Forest",
                                    "Dry Shrubland",
                                    "Semidry-Dry Forest",
                                    "Bare Ground",
                                    "Cropland",
                                    "Water",
                                    "Built-up",
                                    "Grassland",
                                    "Coniferous Forest"),
                      classnames = c("Evergreen.Forest",
                                     "Dry.Shrubland",
                                     "Semidry.Dry.Forest",
                                     "Bare.Ground",
                                     "Cropland",
                                     "Water",
                                     "Built.up",
                                     "Grassland",
                                     "Coniferous.Forest"),
                      # cpal = c("#162700","#a1a733","#5fa00a","#f86b05","#c22341",
                      #          "#0500ff","#020103", "#8c00ff","#20e283")
                      cpal = c("#1c3200", "#8f942f", "#37590a", "#eb4e0b", "#c20e9b",
                               "#0500ff", "#020103", "#84f403", "#20e283"))


# import sentinel mask 
sentinel_mask <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_10m_mask/S2_median_stddev_2021_months_1-12_SVN_mask_average_res_int2s.tif")

# import sentinel product
# !!! virtual raster !!!!!
sentinel <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles SVN/S2_median_stddev_2021_months_1-12_SVN_average_res.vrt")
sentinel <- sentinel[[c(1:14,16:29)]]    # better call by name ...
names(sentinel) <- names(sentinel_mask)

# import sentinel converted to utm (in QGIS)
# reproject sentinel to utm
#sentinel_stack <- raster::stack(sentinel)
#sentinel_utm <- projectRaster(sentinel_stack, crs = crs(sentinel_mask_utm)) # --> takes too long
sentinel_utm <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles SVN/S2_median_stddev_2021_months_1-12_SVN_average_res_utm49_qgis_int2s_r.tif")

# import sentinel mask utm49n (converted in QGIS, faster)
# s <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_10m_mask/S2_median_stddev_2021_months_1-12_SVN_mask_utm49n.tif")
# names(s) <- names(sentinel)
# terra::writeRaster(s, "D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_10m_mask/S2_median_stddev_2021_months_1-12_SVN_mask_int2s_utm49n.tif", datatype = "INT2S")
sentinel_mask_utm <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_10m_mask/S2_median_stddev_2021_months_1-12_SVN_mask_int2s_utm49n.tif")

# Import 200m sentinel utm
sentinel200_utm <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/S2_median_stddev_2021_months_1-12_SVN_scale200_utm_qgis_int2s.tif")
names(sentinel200_utm) <- names(sentinel_mask_utm)

# Import 200m sentinel mask 
sentinel200_mask <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/S2_median_stddev_2021_months_1-12_SVN_scale200_mask.tif")

# Import 200m utm sentinel mask (converted in QGIS)
sentinel200_mask_utm <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/S2_median_stddev_2021_months_1-12_SVN_scale200_mask_utm49nqgis.tif")




# Pixels per class -------------------------------------------------------------------------------------------------------------------

# Determine the fraction of the total area that each class takes up.
# by evaluating total number of pixels with the given value

# all classified raster vals = total size of pixels
inputRaster <- samp_rst10
#classified_values <- data.frame(values(inputRaster, na.rm=T))
pixelsize <- 7570613 #nrow(classified_values)

# get pixel number of each class
#data_from_1s <- classified_values$LC_classes_2.8_rasterized_10mscale[which(classified_values$LC_classes_2.8_rasterized_10mscale==1)]
number_of_1s <- 3474575 #nrow(data_from_1s)

#data_from_2s <- data.frame(classified_values$LC_classes_2.8_rasterized_10mscale[which(classified_values$LC_classes_2.8_rasterized_10mscale==2)])
number_of_2s <- 35293 #nrow(data_from_2s)

#data_from_3s <- data.frame(classified_values$LC_classes_2.8_rasterized_10mscale[which(classified_values$LC_classes_2.8_rasterized_10mscale==3)])
number_of_3s <- 64648 #nrow(data_from_3s)

#data_from_4s <- data.frame(classified_values$LC_classes_2.8_rasterized_10mscale[which(classified_values$LC_classes_2.8_rasterized_10mscale==4)])
number_of_4s <- 43853 #nrow(data_from_4s)

#data_from_5s <- data.frame(classified_values$LC_classes_2.8_rasterized_10mscale[which(classified_values$LC_classes_2.8_rasterized_10mscale==5)])
number_of_5s <- 1006417 #nrow(data_from_5s)

#data_from_6s <- data.frame(classified_values$LC_classes_2.8_rasterized_10mscale[which(classified_values$LC_classes_2.8_rasterized_10mscale==6)])
number_of_6s <- 2548035 #nrow(data_from_6s)

#data_from_7s <- data.frame(classified_values$LC_classes_2.8_rasterized_10mscale[which(classified_values$LC_classes_2.8_rasterized_10mscale==7)])
number_of_7s <- 206906 #nrow(data_from_7s)

#data_from_8s <- data.frame(classified_values$LC_classes_2.8_rasterized_10mscale[which(classified_values$LC_classes_2.8_rasterized_10mscale==8)])
number_of_8s <- 56563 #nrow(data_from_8s)

#data_from_9s <- data.frame(classified_values$LC_classes_2.8_rasterized_10mscale[which(classified_values$LC_classes_2.8_rasterized_10mscale==9)])
number_of_9s <- 134323 #nrow(data_from_9s)


# # ALL 10m SCALE ===============================================================


# 1. EQUAL STRATIFIED RANDOM SAMPLING ========================================================================


# # Define sample size
# siz <- 1000
# 
# # Set seed
# set.seed(33)
# 
# ### Equalized stratified random sampling ###
# 
# # --> size = samplesize per class
# smpl <- sampleStratified(raster::raster(samp_rst10), size = siz, xy=TRUE, na.rm = TRUE, sp = TRUE)
# smpl <- smpl[,-1]
# colnames(smpl@data)[3] = "class_id"
# smpl <- st_as_sf(smpl)
# 
# # Export/import equalized samples
# st_write(smpl, "D:/Luisa/Rsuperclass/smpl_stratified_equal_1000.shp")
# table(smpl$class_id)
# #smpl <- st_read(file.path(paste0(path, "/equstratsamp_",siz,".shp")))
#
# # --> somehow not working for whole td-raster, not thousand samples per class, total less than 2000
# # --> so take samples per class (divided raster)
# r1 <- raster::raster(samp_rst10)
# r1[r1 != 1] <- NA
# r2 <- raster::raster(samp_rst10)
# r2[r2 != 2] <- NA
# r3 <- raster::raster(samp_rst10)
# r3[r3 != 3] <- NA
# r4 <- raster::raster(samp_rst10)
# r4[r4 != 4] <- NA
# r5 <- raster::raster(samp_rst10)
# r5[r5 != 5] <- NA
# r6 <- raster::raster(samp_rst10)
# r6[r6 != 6] <- NA
# r7 <- raster::raster(samp_rst10)
# r7[r7 != 7] <- NA
# r8 <- raster::raster(samp_rst10)
# r8[r8 != 8] <- NA
# r9 <- raster::raster(samp_rst10)
# r9[r9 != 9] <- NA
# 
# rl <- list(r1,r2,r3,r4,r5,r6,r7,r8,r9)
# 
# writeRaster(r1, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class1.tif")
# writeRaster(r2, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class2.tif")
# writeRaster(r3, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class3.tif")
# writeRaster(r4, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class4.tif")
# writeRaster(r5, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class5.tif")
# writeRaster(r6, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class6.tif")
# writeRaster(r7, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class7.tif")
# writeRaster(r8, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class8.tif")
# writeRaster(r9, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class9.tif")

r1 <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class1.tif")
r2 <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class2.tif")
r3 <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class3.tif")
r4 <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class4.tif")
r5 <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class5.tif")
r6 <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class6.tif")
r7 <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class7.tif")
r8 <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class8.tif")
r9 <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/LC_classes_2.8_rasterized_10mscale_class9.tif")


# smpl_r1 <- sampleStratified(r1, size = siz, xy=TRUE, na.rm = TRUE, sp = TRUE)
# smpl_r1 <- smpl_r1[,-1]
# colnames(smpl_r1@data)[3] = "class_id"
# smpl_r1 <- st_as_sf(smpl_r1)
# val1 <- data.frame(values(r1))


# # convert to spatial points data frame
# r.spgrd1<-as(r1,"SpatialPointsDataFrame")
# r.spgrd2<-as(r2,"SpatialPointsDataFrame") # --> took 1.9 mins
# r.spgrd3<-as(r3,"SpatialPointsDataFrame")
# r.spgrd4<-as(r4,"SpatialPointsDataFrame")
# r.spgrd5<-as(r5,"SpatialPointsDataFrame")
# r.spgrd6<-as(r6,"SpatialPointsDataFrame")
# r.spgrd7<-as(r7,"SpatialPointsDataFrame")
# r.spgrd8<-as(r8,"SpatialPointsDataFrame")
# r.spgrd9<-as(r9,"SpatialPointsDataFrame")
# 
# # export class values
# st_write(st_as_sf(r.spgrd1), "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class1_vals_df.shp")
# st_write(st_as_sf(r.spgrd2), "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class2_vals_df.shp")
# st_write(st_as_sf(r.spgrd3), "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class3_vals_df.shp")
# st_write(st_as_sf(r.spgrd4), "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class4_vals_df.shp")
# st_write(st_as_sf(r.spgrd5), "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class5_vals_df.shp")
# st_write(st_as_sf(r.spgrd6), "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class6_vals_df.shp")
# st_write(st_as_sf(r.spgrd7), "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class7_vals_df.shp")
# st_write(st_as_sf(r.spgrd8), "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class8_vals_df.shp")
# st_write(st_as_sf(r.spgrd9), "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class9_vals_df.shp")

r.spgrd1 <- shapefile("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class1_vals_df.shp")
r.spgrd2 <- shapefile("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class2_vals_df.shp")
r.spgrd3 <- shapefile("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class3_vals_df.shp")
r.spgrd4 <- shapefile("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class4_vals_df.shp")
r.spgrd5 <- shapefile("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class5_vals_df.shp")
r.spgrd6 <- shapefile("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class6_vals_df.shp")
r.spgrd7 <- shapefile("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class7_vals_df.shp")
r.spgrd8 <- shapefile("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class8_vals_df.shp")
r.spgrd9 <- shapefile("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/class9_vals_df.shp")

# elminate NA values
r.spgrd1 = r.spgrd1[!is.na(r.spgrd1[[1]]),] 
r.spgrd2 = r.spgrd2[!is.na(r.spgrd2[[1]]),] 
r.spgrd3 = r.spgrd3[!is.na(r.spgrd3[[1]]),] 
r.spgrd4 = r.spgrd4[!is.na(r.spgrd4[[1]]),] 
r.spgrd5 = r.spgrd5[!is.na(r.spgrd5[[1]]),] 
r.spgrd6 = r.spgrd6[!is.na(r.spgrd6[[1]]),] 
r.spgrd7 = r.spgrd7[!is.na(r.spgrd7[[1]]),] 
r.spgrd8 = r.spgrd8[!is.na(r.spgrd8[[1]]),] 
r.spgrd9 = r.spgrd9[!is.na(r.spgrd9[[1]]),] 

# sample points
set.seed(22)
selectedPoints1 = sample(1:length(r.spgrd1[[1]]), 10000)
set.seed(22)
selectedPoints2 = sample(1:length(r.spgrd2[[1]]), 10000)
set.seed(22)
selectedPoints3 = sample(1:length(r.spgrd3[[1]]), 10000)
set.seed(22)
selectedPoints4 = sample(1:length(r.spgrd4[[1]]), 10000)
set.seed(22)
selectedPoints5 = sample(1:length(r.spgrd5[[1]]), 10000)
set.seed(22)
selectedPoints6 = sample(1:length(r.spgrd6[[1]]), 10000)
set.seed(22)
selectedPoints7 = sample(1:length(r.spgrd7[[1]]), 10000)
set.seed(22)
selectedPoints8 = sample(1:length(r.spgrd8[[1]]), 10000)
set.seed(22)
selectedPoints9 = sample(1:length(r.spgrd9[[1]]), 10000)
r.sampled1 = r.spgrd1[selectedPoints1,]
r.sampled2 = r.spgrd2[selectedPoints2,]
r.sampled3 = r.spgrd3[selectedPoints3,]
r.sampled4 = r.spgrd4[selectedPoints4,]
r.sampled5 = r.spgrd5[selectedPoints5,]
r.sampled6 = r.spgrd6[selectedPoints6,]
r.sampled7 = r.spgrd7[selectedPoints7,]
r.sampled8 = r.spgrd8[selectedPoints8,]
r.sampled9 = r.spgrd9[selectedPoints9,]
r_samp1 <- st_as_sf(r.sampled1)
r_samp2 <- st_as_sf(r.sampled2)
r_samp3 <- st_as_sf(r.sampled3)
r_samp4 <- st_as_sf(r.sampled4)
r_samp5 <- st_as_sf(r.sampled5)
r_samp6 <- st_as_sf(r.sampled6)
r_samp7 <- st_as_sf(r.sampled7)
r_samp8 <- st_as_sf(r.sampled8)
r_samp9 <- st_as_sf(r.sampled9)

# st_write(r_samp1, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/samp_10000_1.shp")
# st_write(r_samp2, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/samp_10000_2.shp")
# st_write(r_samp3, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/samp_10000_3.shp")
# st_write(r_samp4, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/samp_10000_4.shp")
# st_write(r_samp5, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/samp_10000_5.shp")
# st_write(r_samp6, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/samp_10000_6.shp")
# st_write(r_samp7, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/samp_10000_7.shp")
# st_write(r_samp8, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/samp_10000_8.shp")
# st_write(r_samp9, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/samp_10000_9.shp")

colnames(r_samp1) <- colnames(r_samp2) <- colnames(r_samp3) <- colnames(r_samp4) <- 
  colnames(r_samp5) <- colnames(r_samp6) <- colnames(r_samp7) <- colnames(r_samp8) <- 
  colnames(r_samp9) <- c("class_id", "geometry")

samps_10000 <- rbind(r_samp1,
                    r_samp2,
                    r_samp3,
                    r_samp4,
                    r_samp5,
                    r_samp6,
                    r_samp7,
                    r_samp8,
                    r_samp9
                    )

#mapview(aoi, alpha.regions = 0) + mapview(td, zcol = "class_id") + mapview(samps_1000)
plot(aoi$geometry)
plot(samps_10000, add=T)


smpl <- samps_10000
smpl_10000 <- smpl



# 2. Split into training and validation =====================================================

r <- raster::raster(samp_rst10)

smpl <- smpl %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

# Define split factor
spl <- 0.7     # 70% training, 30% validation
val <- 1-spl

# Convert points to data table
poi <- data.frame(smpl)
poi <- poi[c(1,3:4)]
#poi <- poi[c(3:6)]
poi <- setDT(poi)

# A stratified random split of the data
set.seed(2)
idx_train <- createDataPartition(poi$class_id,
                                 p = spl, # percentage of data as training
                                 list = FALSE)
train <- poi[idx_train]
test <- poi[-idx_train]

# Convert to sp objects
train_sp <- SpatialPointsDataFrame(coords = train[, .(lon, lat)],  # x,y
                                   data = train %>% select(-c("lon", "lat")), #"x","y")) ,
                                   proj4string = r@crs)
test_sp <- SpatialPointsDataFrame(coords = test[, .(lon, lat)],  # x,y   #coords.x1, coords.x2
                                  data = test %>% select(-c("lon", "lat")), #"x","y")) , #"coords.x1", "coords.x2"
                                  proj4string = r@crs)
plot(aoi$geometry)
plot(train_sp, add=TRUE)

# Convert class id to factor and set levels to avoid error in training part   (JN: geht wahrscheinlich auch in einem schritt mit factor(x = , levels = ))
train_sp@data$class_id <- as.factor(train_sp@data$class_id)
levels(train_sp@data$class_id) <- as.character(classdf$classnames)

test_sp@data$class_id <- as.factor(test_sp@data$class_id)
levels(test_sp@data$class_id) <- as.character(classdf$classnames)

# Show samples per class
table(smpl$class_id)
table(train_sp@data$class_id)
table(test_sp@data$class_id)

# Convert to sf
train_sf <- st_as_sf(train_sp)
test_sf <- st_as_sf(test_sp)

train_sf_10000 <- train_sf
test_sf_10000 <- test_sf


# st_write(smpl, "D:/Luisa/Rsuperclass/SC_prob_10m/smpl_SVN10_td2.8_10000.shp")
# st_write(train_sf, "D:/Luisa/Rsuperclass/SC_prob_10m/train70_SVN10_td2.8_10000.shp")
# st_write(test_sf, "D:/Luisa/Rsuperclass/SC_prob_10m/test30_SVN10_td2.8_10000.shp")

# smpl <- st_read("D:/Luisa/Rsuperclass/SC_prob_10m/smpl_SVN10_td2.8_10000.shp")
# train_sf <- st_read("D:/Luisa/Rsuperclass/SC_prob_10m/train70_SVN10_td2.8_10000.shp")
# test_sf <- st_read("D:/Luisa/Rsuperclass/SC_prob_10m/test30_SVN10_td2.8_10000.shp")


# 3. SC classification (S2 10m res) -------------------------------------------------------------------------------------------

train_sf_utm <- st_transform(train_sf, 32649)
test_sf_utm <- st_transform(test_sf, 32649)



# sentinel_utm_median <- sentinel_utm[[1:14]]
# terra::writeRaster(sentinel_utm_median, "D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles SVN/S2_median_stddev_2021_months_1-12_SVN_average_res_utm49_qgis_int2s_r_median.tif")
# sentinel_utm_median <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles SVN/S2_median_stddev_2021_months_1-12_SVN_average_res_utm49_qgis_int2s_r_median.tif")



start_time <- Sys.time()
SC_prob_10_utm_10000 <- RStoolbox::superClass(sentinel_utm,
                                                 trainData = train_sf_utm,       
                                                 valData = test_sf_utm,
                                                 responseCol = "class_id",
                                                 model = "rf",
                                                 tuneLength = 3,
                                                 kfold = 5,        
                                                 minDist = 2,
                                                 mode = "classification", 
                                                 predict = FALSE,
                                                 predType = "prob",      
                                                 verbose = TRUE)
end_time <- Sys.time()
end_time - start_time
# --> Time difference of 10 h


#save(SC_prob_10_utm_10000, file = "D:/Luisa/Rsuperclass/SC_prob_10m/SC_prob_10m_utm_SVN_td2.8_10000.RData")
load(file = "D:/Luisa/Rsuperclass/SC_prob_10m/SC_prob_10m_utm_SVN_td2.8_10000.RData")
#load("D:/Luisa/Rsuperclass/SC_prob_200m/SC_prob_200m_utm_SVN_td2.8_50_30pc.RData")

test_sf_SC_10utm_10000 <- SC_prob_10_utm_10000$validation$validationSamples

#st_write(test_sf_SC_10utm_10000, "D:/Luisa/Rsuperclass/SC_prob_10m/test_SC10_SVN_td2.8_10000.shp")
# --> returned warnings():
# ...
# 50: In CPL_write_ogr(obj, dsn, layer, driver, as.character(dataset_options),  ... :
#                       GDAL Message 1: Value 912059637 of field cell of feature 49 not successfully written. Possibly due to too larger number with respect to field width



# write.csv2(SC_prob_10_utm_10000$validation$performance$table,
#            "D:/Luisa/Rsuperclass/SC_prob_10m/SC10_SVN_td2.8_10000_cf.csv",
#            sep = ";")
# 
# write.csv2(round(SC_prob_10_utm_10000$validation$performance$byClass,3),
#            "D:/Luisa/Rsuperclass/SC_prob_10m/SC10_SVN_td2.8_10000_class_stats.csv",
#            sep = ";")

# install.packages("RTools")
# devtools::install_github("MBalthasar/superClassAnalysis")
# [code language="R"]
# x1 <- SampleSaturation(img = sentinel_utm, model = 'rf', trainData = train_sf,
#                        valData = test_sf, nSamples = c(10, 50, 100, 500),
#                        classes = training_data$class_name,
#                        responseCol = "class_name", prodAcc = TRUE,
#                        overall = TRUE, plot_graph = TRUE)
# [/code]
#save(sentinel_utm, file = "D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_10m_S2mask_utm/sentinel_utm.RData")
SC_prob_10_utm_10000$validation$performance$overall
SC_prob_10_utm_10000$validation$performance$byClass
SC_prob_10_utm_10000$validation$performance$table

#### > Predictor importance --------------------------------------------------------

library(plotly)
caret::varImp(SC_prob_10_utm_10000$model)$importance %>% 
  as.matrix %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 600, height = 600, order)

var_imp <- caret::varImp(SC_prob_10_utm_10000$model)$importance %>% 
  as.matrix
#write.csv2(round(var_imp,1), "C:/Users/s347553/Documents/Masterthesis/Data/figures/LC/varImp.csv")



# other option: graph
#png(filename = "C:/Users/s347553/Documents/Masterthesis/Data/figures/LC/varImp_scale.png")
plot(
  varImp(SC_prob_10_utm_10000$model,scale = T),col="black")
#dev.off()




# 4. Predictions -----------------------------------------------------------------------------------

#> classification
#> 

### 200 mask prediction ------------------------------------------------------------------------

start_time = Sys.time()
SC_pred_10_utm_10000 <- terra::predict(SC_prob_10_utm_10000, sentinel200_mask_utm, predType = "prob", progress="text")
end_time = Sys.time()
end_time - start_time
# --> Time difference of ??
unique(values(SC_pred_10_utm_10000))

names(SC_pred_10_utm_10000) <- classdf$classnames
plot(SC_pred_10_utm_10000)

terra::writeRaster(SC_pred_10_utm_10000, "D:/Luisa/Rsuperclass/SC_prob_10m/pred200_SC10_SVN_td2.8_10000.tif")


### 100m mask prediction --------------------------------------------------------------------------

# import 100m mask
sentinel100_mask_utm <- stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles SVN/S2_median_stddev_2021_months_1-12_SVN_aggr100_mask_utm49n_qgis.tif")
names(sentinel100_mask_utm) <- names(sentinel200_mask_utm)

# run prediction
start_time = Sys.time()
SC_pred_10_utm_10000_100m <- terra::predict(SC_prob_10_utm_10000, sentinel100_mask_utm, predType = "prob", progress="text")
end_time = Sys.time()
end_time - start_time
# --> Time difference of ??
unique(values(SC_pred_10_utm_10000_100m))

names(SC_pred_10_utm_10000_100m) <- classdf$classnames
plot(SC_pred_10_utm_10000_100m)

terra::writeRaster(SC_pred_10_utm_10000_100m, "D:/Luisa/Rsuperclass/SC_prob_10m/pred100_SC10_SVN_td2.8_10000.tif")


### 50m mask prediction --------------------------------------------------------------------------

# # aggregate to 50m mask
# start_time = Sys.time()
# sentinel50_mask_utm <- aggregate(sentinel_mask_utm, fact = (50/10), fun = mean, na.rm = TRUE)   # -> took 4.22 hours
# end_time = Sys.time()
# end_time - start_time
# 
# writeRaster(sentinel50_mask_utm, "D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_10m_mask/S2_median_stddev_2021_months_1-12_SVN_mask_int2s_utm49n_aggr50.tif")

# import 50m mask
sentinel50_mask_utm <- stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_10m_mask/S2_median_stddev_2021_months_1-12_SVN_mask_int2s_utm49n_aggr50.tif")


# run prediction
start_time = Sys.time()
SC_pred_10_utm_10000_50m <- terra::predict(SC_prob_10_utm_10000, sentinel50_mask_utm, predType = "prob", progress="text")
end_time = Sys.time()
end_time - start_time
# --> Time difference of 31,27 mins
unique(values(SC_pred_10_utm_10000_50m))

###!!!! only 0 and 1 :(

names(SC_pred_10_utm_10000_50m) <- classdf$classnames
plot(SC_pred_10_utm_10000_50m)

terra::writeRaster(SC_pred_10_utm_10000_50m, "D:/Luisa/Rsuperclass/SC_prob_10m/pred50_SC10_SVN_td2.8_10000.tif")





# 5. Discrete classes --------------------------------------------------------------------------------

prob_stack <- SC_pred_10_utm_5000_100m
prob_stack <- SC_pred_10_utm_10000_100m

# Combine probability layers to one final layer product by selecting the max probability value
raw <- which.max(prob_stack)

# Set levels
levels(raw)=data.frame(ID=classdf$classvalues, class=classdf$classnames) 


# Smoothing of the classification output (majority filter)    
# --> A Majority Filter re-samples the cells in a raster according to the majority value of the neighbor cells
# --> Apply rectangular moving window to discrete data (e.g. integer)
# --> 3x3 moving window; note, function is set to modal, so we will find the mode or count of each number...this essentially treats it as categorical
raw_filterR <- raster::focal(raw, w=matrix(1,3,3), fun=raster::modal)    # 3x3 moving window

# Set levels
levels(raw_filterR)=data.frame(ID=classdf$classvalues, class=classdf$classnames)


writeRaster(raw, "D:/Luisa/Rsuperclass/SC_prob_10m/pred100_SC10_SVN_td2.8_10000_discrete.tif")
writeRaster(raw_filterR, "D:/Luisa/Rsuperclass/SC_prob_10m/pred100_SC10_SVN_td2.8_10000_discrete_filtered.tif")






# 6. Examination ===========================================================================

library(raster)
library(terra)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)

prob <- SC_pred_10_utm_10000_100m
names(prob) <- classdf$classnames



hist(prob$`Evergreen Forest`, xlab = "Probability")
hist(prob$`Semi.Dry Forest`, xlab = "Probability")
hist(prob$`Dry Shrubland`, xlab = "Probability")
hist(prob$`Bare Ground`, xlab = "Probability")
hist(prob$Cropland, xlab = "Probability")
hist(prob$Water, xlab = "Probability")
hist(prob$`Built up`, xlab = "Probability")
hist(prob$Grassland, xlab = "Probability")
hist(prob$`Coniferous Forest`, xlab = "Probability")



# # import data
# td <- st_read("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/trainingdata/SVN/LC_classes_2.8.shp")
# td_utm <- st_transform(td, 32649)
# td <- terra::vect(td)
# td_utm <- terra::vect(td_utm)
# 
# aoi <- st_read("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/SVN_SBC_provinces_final.shp")
# s2 <- terra::rast("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Sentinel/final_SVN/S2_median_stddev_2021_months_1-12_SVN_scale200.tif")
# prob <- terra::rast("D:/Dateien/Uni/Eagle_Master/Masterthesis/R_Masterthesis_SBC/superclass/SC_prob_10m/pred100_SC10_SVN_td2.8_10000.tif")


# extract raster values at training polygons
td_vals <- terra::extract(sentinel_utm, terra::vect(td_utm))
#td_vals <- td_vals[c(1:15,17:30)]
td_prob_vals_ <- terra::extract(prob, terra::vect(td_utm))
td_prob_vals <- na.omit(td_prob_vals_)
any(is.na(td_vals))

# min(s2_new$NDVI_median)
# min(td_vals$NDVI_median)
# max(td_vals$NDVI_median)

s2_new <- sentinel_utm
#s2_new <- s2_new[[c(1:14,16:29)]]
names(s2_new)

# change median minimum
s2_new$B_median[s2_new$B_median < min(td_vals$B_median)] <- NA
s2_new$G_median[s2_new$G_median < min(td_vals$G_median)] <- NA
s2_new$R_median[s2_new$R_median < min(td_vals$R_median)] <- NA
s2_new$RedEdge1_median[s2_new$RedEdge1_median < min(td_vals$RedEdge1_median)] <- NA
s2_new$NIR_median[s2_new$NIR_median < min(td_vals$NIR_median)] <- NA
s2_new$RedEdge4_median[s2_new$RedEdge4_median < min(td_vals$RedEdge4_median)] <- NA
s2_new$SWIR1_median[s2_new$SWIR1_median < min(td_vals$SWIR1_median)] <- NA
s2_new$SWIR2_median[s2_new$SWIR2_median < min(td_vals$SWIR2_median)] <- NA
s2_new$BSI_median[s2_new$BSI_median < min(td_vals$BSI_median)] <- NA
s2_new$NDVI_median[s2_new$NDVI_median < min(td_vals$NDVI_median)] <- NA
s2_new$EVI2_median[s2_new$EVI2_median < min(td_vals$EVI2_median)] <- NA
s2_new$MSAVI_median[s2_new$MSAVI_median < min(td_vals$MSAVI_median)] <- NA
s2_new$NDMI_median[s2_new$NDMI_median < min(td_vals$NDMI_median)] <- NA
s2_new$NBR_median[s2_new$NBR_median < min(td_vals$NBR_median)] <- NA

# change median maximum
s2_new$B_median[s2_new$B_median > max(td_vals$B_median)] <- NA
s2_new$G_median[s2_new$G_median > max(td_vals$G_median)] <- NA
s2_new$R_median[s2_new$R_median > max(td_vals$R_median)] <- NA
s2_new$RedEdge1_median[s2_new$RedEdge1_median > max(td_vals$RedEdge1_median)] <- NA
s2_new$NIR_median[s2_new$NIR_median > max(td_vals$NIR_median)] <- NA
s2_new$RedEdge4_median[s2_new$RedEdge4_median > max(td_vals$RedEdge4_median)] <- NA
s2_new$SWIR1_median[s2_new$SWIR1_median > max(td_vals$SWIR1_median)] <- NA
s2_new$SWIR2_median[s2_new$SWIR2_median > max(td_vals$SWIR2_median)] <- NA
s2_new$BSI_median[s2_new$BSI_median > max(td_vals$BSI_median)] <- NA
s2_new$NDVI_median[s2_new$NDVI_median > max(td_vals$NDVI_median)] <- NA
s2_new$EVI2_median[s2_new$EVI2_median > max(td_vals$EVI2_median)] <- NA

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ----> ab hier (unten) folgender Fehler: 
# ----> Error in h(simpleError(msg, call)) : 
# ----> error in evaluating the argument 'i' in selecting a method for function '[<-': 
# ----> [>] insufficient disk space (perhaps from temporary files?)
s2_new <- terra::rast("D:/Luisa/Rsuperclass/SC_prob_10m/extrapolation/sentinel_utm_cutoff_minmax_td_int2s_r.tif")

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
s2_new$MSAVI_median[s2_new$MSAVI_median > max(td_vals$MSAVI_median)] <- NA
s2_new$NDMI_median[s2_new$NDMI_median > max(td_vals$NDMI_median)] <- NA
s2_new$NBR_median[s2_new$NBR_median > max(td_vals$NBR_median)] <- NA

# change stddev minimum
s2_new$B_stdDev[s2_new$B_stdDev < min(td_vals$B_stdDev)] <- NA
s2_new$G_stdDev[s2_new$G_stdDev < min(td_vals$G_stdDev)] <- NA
s2_new$R_stdDev[s2_new$R_stdDev < min(td_vals$R_stdDev)] <- NA
s2_new$RedEdge1_stdDev[s2_new$RedEdge1_stdDev < min(td_vals$RedEdge1_stdDev)] <- NA
s2_new$NIR_stdDev[s2_new$NIR_stdDev < min(td_vals$NIR_stdDev)] <- NA
s2_new$RedEdge4_stdDev[s2_new$RedEdge4_stdDev < min(td_vals$RedEdge4_stdDev)] <- NA
s2_new$SWIR1_stdDev[s2_new$SWIR1_stdDev < min(td_vals$SWIR1_stdDev)] <- NA
s2_new$SWIR2_stdDev[s2_new$SWIR2_stdDev < min(td_vals$SWIR2_stdDev)] <- NA
s2_new$BSI_stdDev[s2_new$BSI_stdDev < min(td_vals$BSI_stdDev)] <- NA
s2_new$NDVI_stdDev[s2_new$NDVI_stdDev < min(td_vals$NDVI_stdDev)] <- NA
s2_new$EVI2_stdDev[s2_new$EVI2_stdDev < min(td_vals$EVI2_stdDev)] <- NA
s2_new$MSAVI_stdDev[s2_new$MSAVI_stdDev < min(td_vals$MSAVI_stdDev)] <- NA
s2_new$NDMI_stdDev[s2_new$NDMI_stdDev < min(td_vals$NDMI_stdDev)] <- NA
s2_new$NBR_stdDev[s2_new$NBR_stdDev < min(td_vals$NBR_stdDev)] <- NA

# change stddev maximum
s2_new$B_stdDev[s2_new$B_stdDev > max(td_vals$B_stdDev)] <- NA
s2_new$G_stdDev[s2_new$G_stdDev > max(td_vals$G_stdDev)] <- NA
s2_new$R_stdDev[s2_new$R_stdDev > max(td_vals$R_stdDev)] <- NA
s2_new$RedEdge1_stdDev[s2_new$RedEdge1_stdDev > max(td_vals$RedEdge1_stdDev)] <- NA
s2_new$NIR_stdDev[s2_new$NIR_stdDev > max(td_vals$NIR_stdDev)] <- NA
s2_new$RedEdge4_stdDev[s2_new$RedEdge4_stdDev > max(td_vals$RedEdge4_stdDev)] <- NA
s2_new$SWIR1_stdDev[s2_new$SWIR1_stdDev > max(td_vals$SWIR1_stdDev)] <- NA
s2_new$SWIR2_stdDev[s2_new$SWIR2_stdDev > max(td_vals$SWIR2_stdDev)] <- NA
s2_new$BSI_stdDev[s2_new$BSI_stdDev > max(td_vals$BSI_stdDev)] <- NA
s2_new$NDVI_stdDev[s2_new$NDVI_stdDev > max(td_vals$NDVI_stdDev)] <- NA
s2_new$EVI2_stdDev[s2_new$EVI2_stdDev > max(td_vals$EVI2_stdDev)] <- NA
s2_new$MSAVI_stdDev[s2_new$MSAVI_stdDev > max(td_vals$MSAVI_stdDev)] <- NA
s2_new$NDMI_stdDev[s2_new$NDMI_stdDev > max(td_vals$NDMI_stdDev)] <- NA
s2_new$NBR_stdDev[s2_new$NBR_stdDev > max(td_vals$NBR_stdDev)] <- NA


# writeRaster(s2_new, "D:/Luisa/Rsuperclass/SC_prob_10m/extrapolation/sentinel_utm_cutoff_minmax_td.tif")
# s2_new <- terra::rast("D:/Luisa/Rsuperclass/SC_prob_10m/extrapolation/sentinel_utm_cutoff_minmax_td.tif")
terra::writeRaster(s2_new, "D:/Luisa/Rsuperclass/SC_prob_10m/extrapolation/sentinel_utm_cutoff_minmax_td_final_int2s_r.tif", datatype="INT2S")





#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ---> TAKE CARE: raster has size of 83 GB right now....
# ---> HIER STEHEN GEBLIEBEN:   - raster verkleinern, 
#                               - andere noch fehlende Bänder etsprechend NA setzen
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



# # >>>>>>>>>>>>>>>>>> ==============================================================================
# # 7.  10m prediction tiles ==============================================================================
# 
# > CT station tiles -------------------------------------------------------------------------------------
# #> So far it is not possible to predict the trained classifier to a 10m res S2 image 
# #> (output only 0 or 1)
# #> So I clip out tiles around the CT stations from the 10m res S2 image and only predict the
# #> classes to them
# #> Later I can extract the 10m values at the CT stations from that tiles
# 
# # load SCprob file (10000)
# load("D:/Luisa/Rsuperclass/SC_prob_10m/SC_prob_10m_utm_SVN_td2.8_10000.RData")
# 
# ## > So Pai
# # # EPSG 4326
# # pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_sopai.tif")
# # # Convert to utm
# # pr <- projectRaster(pr, crs = crs(train_sf_utm))
# # Import converted to utm in QGIS
# pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_sopai_utm_qgis.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_sopai <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 1.512843 mins
# unique(values(pred_sopai))
# names(pred_sopai) <- classdf$classnames
# plot(pred_sopai)
# 
# # > Krong Trai
# # # EPSG 4326
# # pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_krongtrai.tif")
# # # Convert to utm
# # pr <- projectRaster(pr, crs = crs(train_sf_utm))
# # Import converted to utm in QGIS
# pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_krongtrai_utm_qgis.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_krongtrai <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 1.468808 mins
# unique(values(pred_krongtrai))
# names(pred_krongtrai) <- classdf$classnames
# plot(pred_krongtrai)
# 
# # > Song Hinh
# # # EPSG 4326
# # pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_songhinh.tif")
# # # Convert to utm
# # pr <- projectRaster(pr, crs = crs(train_sf_utm))
# # Import converted to utm in QGIS
# pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_songhinh_utm_qgis.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_songhinh <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 1.463382 mins
# unique(values(pred_songhinh))
# names(pred_songhinh) <- classdf$classnames
# plot(pred_songhinh)
# 
# # > Deo Ca
# # # EPSG 4326
# # pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_deoca.tif")
# # # Convert to utm
# # pr <- projectRaster(pr, crs = crs(train_sf_utm))
# # Import converted to utm in QGIS
# pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_deoca_utm_qgis.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_deoca <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 1.466694 mins
# unique(values(pred_deoca))
# names(pred_deoca) <- classdf$classnames
# plot(pred_deoca)
# 
# # > Hon Heo
# # # EPSG 4326
# # pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_honheo.tif")
# # # Convert to utm
# # pr <- projectRaster(pr, crs = crs(train_sf_utm))
# # Import converted to utm in QGIS
# pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_honheo_utm_qgis.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_honheo <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 1.365592 mins
# unique(values(pred_honheo))
# names(pred_honheo) <- classdf$classnames
# plot(pred_honheo)
# 
# # > Suoi Tien
# # # EPSG 4326
# # pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_suoitien.tif")
# # # Convert to utm
# # pr <- projectRaster(pr, crs = crs(train_sf_utm))
# # Import converted to utm in QGIS
# pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_suoitien_utm_qgis.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_suoitien <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 1.363161 mins
# unique(values(pred_suoitien))
# names(pred_suoitien) <- classdf$classnames
# plot(pred_suoitien)
# 
# # > Cam An Bac
# # # EPSG 4326
# # pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_camanbac.tif")
# # # Convert to utm
# # pr <- projectRaster(pr, crs = crs(train_sf_utm))
# # Import converted to utm in QGIS
# pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_camanbac_utm_qgis.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_camanbac <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 1.362872 mins
# unique(values(pred_camanbac))
# names(pred_camanbac) <- classdf$classnames
# plot(pred_camanbac)
# 
# # > Nui Chua
# # # EPSG 4326
# # pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_nuichua.tif")
# # # Convert to utm
# # pr <- projectRaster(pr, crs = crs(train_sf_utm))
# # Import converted to utm in QGIS
# pr <- raster::stack("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_nuichua_utm_qgis.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_nuichua <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 4.388594 mins
# unique(values(pred_nuichua))
# names(pred_nuichua) <- classdf$classnames
# plot(pred_nuichua)
# 
# 
# # > Bidoup & Phuoc Binh
# # pr <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_bidoup_phuocbinh_utm_fromutmmask_qgis.tif")
# # names(pr) <- names(sentinel)
# # start_time = Sys.time()
# # pred_bid_pb <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# # end_time = Sys.time()
# # end_time - start_time
# # # --> Time difference of 35.8 mins
# # unique(values(pred_bid_pb))
# # ---> only 0 and 1 probabilities, too large raster :(
# 
# # tile1
# pr <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_bidoup1_utm_fromutmmask_qgis.tif")
# # names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_bid1 <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 9.169373 mins
# unique(values(pred_bid1))
# 
# # tile2
# pr <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_bidoup2_pb_utm_fromutmmask_qgis.tif")
# # names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_bid2 <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 6.804 mins
# unique(values(pred_bid2))
# 
# # tile3
# pr <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_bidoup3_pb_utm_fromutmmask_qgis.tif")
# # names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_bid3 <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 
# unique(values(pred_bid3))
# 
# 
# # > Cat Tien
# pr <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_SVN_CTstations/S2_mask_cattien_utm_fromutmmask_qgis.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_cattien <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 2.52 mins
# unique(values(pred_cattien))
# names(pred_cattien) <- classdf$classnames
# plot(pred_cattien)



# > SVN grid 30x30km -----------------------------------------------------------------------------------------

# # > testtile (check ob größe klargeht)
# pr <- terra::rast("C:/Users/s347553/Desktop/test_tile_final_predict.tif")
# names(pr) <- names(sentinel)
# start_time = Sys.time()
# pred_testtile <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 11.01545 mins !!!
# unique(values(pred_testtile))
# names(pred_testtile) <- classdf$classnames
# plot(pred_testtile)
# 
# # > testtile (check wie NA gehändelt ist)
# pr <- terra::rast("C:/Users/s347553/Desktop/testtiles_inclNA_2.tif")
# names(pr) <- names(sentinel)
# df_testNA <- data.frame(values(pr))
# start_time = Sys.time()
# pred_testtile_NA <- terra::predict(SC_prob_10_utm_10000, pr, predType = "prob", progress="text")
# end_time = Sys.time()
# end_time - start_time
# # --> Time difference of 4.662078 mins
# unique(values(pred_testtile_NA))
# names(pred_testtile_NA) <- classdf$classnames
# plot(pred_testtile_NA)



# # import raster tiles and export as INT2S to save memory
# files <- list.files("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_10m_S2mask_utm/S2_tiles_from_grid", full.names=T)
# outdir <- "D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_10m_S2mask_utm/S2_tiles_from_grid_int2s/"
# names <- list.files("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_10m_S2mask_utm/S2_tiles_from_grid")
# 
# x <- lapply(files, terra::rast)
# 
# for (i in 1:length(x)){
#   terra::writeRaster(x[[i]], paste0(outdir, names[i]), datatype="INT2S")
# }

# > make prediction for each raster tile 
files <- list.files("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_10m_S2mask_utm/S2_tiles_from_grid_int2s", full.names=T)
names <- list.files("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles_10m_S2mask_utm/S2_tiles_from_grid_int2s")
outdir <- "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/RF_pred_SVNtiles_10m/"

x <- lapply(files, terra::rast)
pred_tiles <- list()
s <- seq(0, (length(files)-1))

for (i in 71:length(x)){
  pred_tiles[[i]] <- terra::predict(SC_prob_10_utm_10000, x[[i]], predType = "prob", progress="text")
  names(pred_tiles[[i]]) <- classdf$classnames
  
  terra::writeRaster(pred_tiles[[i]], 
    filename = paste0(outdir, "pred10m_SC_SVN_td2.8_10000_fromutmmask_tile_", s[i],".tif"))
}



# # usefule functions for tiles!!
files <- list.files("D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/RF_pred_SVNtiles_10m", full.names = T)

# load raster tiles
x <- lapply(files, terra::rast)
# rename raster bands
y <- lapply(x, function(y) {names(y) <- classdf$classnames; y})

for (i in 1:length(y)){
  terra::writeRaster(y[[i]], 
   filename = paste0(outdir, "pred10m_SC_SVN_td2.8_10000_fromutmmask_tile", s[i],".tif"))
}

 
# 
# #m <- lapply(y, terra::mosaic)   --> takes very long!
# mos <- do.call(mosaic, y)

# merge tiles in QGIS with GDAL raster merge (float32, null values: 0, -9999, highcompression, bigtiff = yes)

# import merged raster, set names and export final 200m SBC prediction for SVN!! :)
pred_imp <- terra::rast()




### discrete class --------------------------------------------------------------------------------------

# # load raster tiles
# tiles <- list.files("D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/RF_pred_SVNtiles_10m/continuous", full.names = T)
# x <- lapply(tiles, terra::rast)
# 
# xy <- lapply(x, which.max)
# sx <- seq(1:length(xy))
# 
# for (i in 1:length(xy)){
#   terra::writeRaster(xy[[i]], 
#                      filename = paste0("D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/RF_pred_SVNtiles_10m/discrete/pred10m_discrete_tile_", sx[i],".tif"))
# }

raw <- rast("D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_10m_SC_SVN_discrete_113tiles_merge_gdal_int1u_final.tif")




# Smoothing of the classification output (majority filter)    
# --> A Majority Filter re-samples the cells in a raster according to the majority value of the neighbor cells
# --> Apply rectangular moving window to discrete data (e.g. integer)
# --> 3x3 moving window; note, function is set to modal, so we will find the mode or count of each number...this essentially treats it as categorical
raw_filterR <- raster::focal(raw, w=matrix(1,3,3), fun=raster::modal)    # 3x3 moving window

# Set levels
levels(raw_filterR)=data.frame(ID=classdf$classvalues, class=classdf$classnames)

terra::writeRaster(raw_filterR, "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_10m_SC_SVN_discrete_filterR_113tiles_merge_gdal_int1u_final.tif", datatype = "INT1U")




### visualization ------------------------------------------------------------------------------------------

# smaller aoi
aoismall <- st_read("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/bb.shp")
mapview::mapview(aoismall)
bb <- st_transform(aoismall, 32649)

LCcrop <- raster::crop(raw_filterR, bb)
plot(LCcrop)

LCcrop <- raw_filterR

vals <- data.frame(values(LCcrop))

any(is.na(vals))
vals2 <- na.omit(vals)

df_plot <- data.frame(

  evergreen.forest = length(which(vals$class == 1)),
  dry.shrubland = length(which(vals$class == 2)),
  semi.dry.forest = length(which(vals$class == 3)),
  bare.ground = length(which(vals$class == 4)),
  cropland = length(which(vals$class == 5)),
  water = length(which(vals$class == 6)),
  built.up = length(which(vals$class == 7)),
  grassland = length(which(vals$class == 8)),
  coniferous.forest= length(which(vals$class == 9))

)


df_plot[2,1] <- df_plot[1,1] * res(LCcrop)[1]^2
df_plot[2,2] <- df_plot[1,2] * res(LCcrop)[1]^2
df_plot[2,3] <- df_plot[1,3] * res(LCcrop)[1]^2
df_plot[2,4] <- df_plot[1,4] * res(LCcrop)[1]^2
df_plot[2,5] <- df_plot[1,5] * res(LCcrop)[1]^2
df_plot[2,6] <- df_plot[1,6] * res(LCcrop)[1]^2
df_plot[2,7] <- df_plot[1,7] * res(LCcrop)[1]^2
df_plot[2,8] <- df_plot[1,8] * res(LCcrop)[1]^2
df_plot[2,9] <- df_plot[1,9] * res(LCcrop)[1]^2

rownames(df_plot) <- c("pred_px", "pred_area_qm")

# # calculate area per class and multiply with pixel size
# sum(ras[] >= 0.1 & ras[] <= 0.2) * res(ras)[1]^2

# flip rows and cols
df_plot2 = data.frame(t(df_plot))

# add area_qkm column
df_plot2$pred_area_qkm <- round((df_plot2$pred_area_qm / 1000000), 1)

df_plot2$class <- factor(seq(1:9))
levels(df_plot2$class) <- classdf$classnames

df_plot2 <- df_plot2[c(4,1,2,3)]

df_plot2$pred_pc <- round((df_plot2$pred_area_qm * 100 / sum(df_plot2$pred_area_qm)),2)


# ggplot
#png(filename = "C:/Users/s347553/Documents/Masterthesis/Data/figures/LC/pred_prop.png")

ggplot(df_plot2, aes(x = "", y = pred_area_qkm, fill = class)) +
  geom_col(color = "black") +
  # geom_text(aes(x = 1.55, label = paste0(round(pred_pc),"%")), 
  #          position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = classdf$cpal) + 
  theme_void()

#dev.off()  
  

# # import train data stats
# td_stat <- read.csv2("C:/Users/s347553/Documents/Masterthesis/Data/trainingdata/td_2.8/trainstats.csv", sep=",")
# # td_stat$class_id <- factor(td_stat$class_id)
# # levels(td_stat$class_id) <- classdf$classnames
# 
# df_plot2$area_qkm_td <- td_stat$area_qkm 



df_plot2$td_px <- c(number_of_1s,
                    number_of_2s,
                    number_of_3s,
                    number_of_4s,
                    number_of_5s,
                    number_of_6s,
                    number_of_7s,
                    number_of_8s,
                    number_of_9s
                    )

df_plot2$td_area_qm <- df_plot2$td_px * res(LCcrop)[1]^2

df_plot2$td_area_qkm <- round((df_plot2$td_area_qm / 1000000),1)

df_plot2$td_pc <- round((df_plot2$td_area_qm * 100 / sum(df_plot2$td_area_qm)),2)


names(df_plot2)
#write.csv2(df_plot2, "C:/Users/s347553/Documents/Masterthesis/Data/figures/LC/df_plot2.csv")


# png(filename = "C:/Users/s347553/Documents/Masterthesis/Data/figures/LC/td_prop.png")
# ggplot
ggplot(df_plot2, aes(x = "", y = td_area_qkm, fill = class)) +
  geom_col(color = "black") +
  # geom_text(aes(x = 1.55, label = paste0(round(td_pc),"%")),
  #           position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = classdf$cpal) + 
  theme_void()

# dev.off()




length(which(test_sf_SC_10utm_10000$reference == classdf$classnames[1])) +
length(which(test_sf_SC_10utm_10000$reference == classdf$classnames[2]))+
length(which(test_sf_SC_10utm_10000$reference == classdf$classnames[3]))+
length(which(test_sf_SC_10utm_10000$reference == classdf$classnames[4]))+
length(which(test_sf_SC_10utm_10000$reference == classdf$classnames[5]))+
length(which(test_sf_SC_10utm_10000$reference == classdf$classnames[6]))+
length(which(test_sf_SC_10utm_10000$reference == classdf$classnames[7]))+
length(which(test_sf_SC_10utm_10000$reference == classdf$classnames[8]))+
length(which(test_sf_SC_10utm_10000$reference == classdf$classnames[9]))





# alternative: https://stackoverflow.com/questions/67447579/cant-place-lables-correctly-using-geom-label-repel-in-piechart

library(tidyverse)
library(ggrepel)

# df_plot3 <- df_plot2
# df_plot3 <- df_plot3 %>% mutate(lab.ypos = cumsum(td_pc) - td_pc/2)
# df_plot3 <- df_plot3 %>% 
#   mutate(
#     cs = rev(cumsum(rev(td_pc))), 
#     pos = td_pc/2 + lead(cs, 1),
#     pos = if_else(is.na(pos), td_pc/2, pos))
# 
# ggplot(data = df_plot3, aes(x = "", y = td_pc, fill= class))+ 
#   geom_col(width=1, col="black") +
#   coord_polar(theta = "y", start = 0) +
#   # geom_label_repel(aes(y = pos, label = paste0(round(td_pc), "%")),   # option 1
#   #                  data = df_plot3, size=4, 
#   #                  show.legend = F, nudge_x = 1, colour="white") +
#   geom_text_repel(aes(y = pos, label = paste0(round(td_pc), "%")),      # option 2
#                    data = df_plot3, size=4, 
#                    show.legend = F, nudge_x = 1, colour="black") +
#   scale_fill_manual(values = classdf$cpal) + 
#   theme_void()

df_plot3 <- df_plot2
df_plot3 <- df_plot3 %>% mutate(lab.ypos = cumsum(pred_pc) - pred_pc/2)
df_plot3 <- df_plot3 %>% 
  mutate(
    cs = rev(cumsum(rev(pred_pc))), 
    pos = pred_pc/2 + lead(cs, 1),
    pos = if_else(is.na(pos), pred_pc/2, pos))

ggplot(data = df_plot3, aes(x = "", y = pred_pc, fill= class))+ 
  geom_col(width=1, col="black") +
  coord_polar(theta = "y", start = 0) +
  # geom_label_repel(aes(y = pos, label = paste0(round(pred_pc), "%")),   # option 1
  #                  data = df_plot3, size=4, 
  #                  show.legend = F, nudge_x = 1, colour="white") +
  geom_text_repel(aes(y = pos, label = paste0(round(pred_pc), "%")),      # option 2
                  data = df_plot3, size=4, 
                  show.legend = F, nudge_x = 1, colour="black") +
  scale_fill_manual(values = classdf$cpal) + 
  theme_void()







# install.packages("ggplot2")
library(ggplot2)
# df <- data.frame(value = c(10, 23, 15, 18),
#                  group = paste0("G", 1:4))
# 
# ggplot(df, aes(x = "", y = value, fill = group)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = value),
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y") +
#   scale_fill_manual(values = c("#BE2A3E", "#EC754A",
#                                "#EACF65", "#3C8D53"))





# >> EXPORT ==============================================================================================

# names(pred_sopai) <- names(pred_krongtrai) <- names(pred_songhinh) <- names(pred_deoca) <-
#   names(pred_honheo) <- names(pred_suoitien) <- names(pred_camanbac) <- names(pred_nuichua) <-
#   classdf$classnames
#
# raster::writeRaster(pred_sopai, "D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_10m_utm/pred_SC_SVN_td2.8_50000_sopai_10m_fromutmmask.tif")
# raster::writeRaster(pred_krongtrai, "D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_10m_utm/pred_SC_SVN_td2.8_50000_krongtrai_10m_fromutmmask.tif")
# raster::writeRaster(pred_songhinh, "D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_10m_utm/pred_SC_SVN_td2.8_50000_songhinh_10m_fromutmmask.tif")
# raster::writeRaster(pred_deoca, "D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_10m_utm/pred_SC_SVN_td2.8_50000_deoca_10m_fromutmmask.tif")
# raster::writeRaster(pred_honheo, "D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_10m_utm/pred_SC_SVN_td2.8_50000_honheo_10m_fromutmmask.tif")
# raster::writeRaster(pred_suoitien, "D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_10m_utm/pred_SC_SVN_td2.8_50000_suoitien_10m_fromutmmask.tif")
# raster::writeRaster(pred_camanbac, "D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_10m_utm/pred_SC_SVN_td2.8_50000_camanbac_10m_fromutmmask.tif")
# raster::writeRaster(pred_nuichua, "D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_10m_utm/pred_SC_SVN_td2.8_50000_nuichua_10m_fromutmmask.tif")


# terra::writeRaster(terra::rast(pred_sopai), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_sopai_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_krongtrai), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_krongtrai_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_songhinh), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_songhinh_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_deoca), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_deoca_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_honheo), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_honheo_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_suoitien), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_suoitien_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_camanbac), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_camanbac_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_nuichua), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_nuichua_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_bid1), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_bidoup1_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_bid2), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_bidoup2_pb_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_bid3), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_bidoup3_pb_10m_fromutmmask.tif")
# terra::writeRaster(terra::rast(pred_cattien), "D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_SC_SVN_td2.8_10000_cattien_10m_fromutmmask.tif")







# >>>>>>>>>>>>>>>>>> ==============================================================================
# xxx. SMOTE =======================================================================================

# https://medium.com/analytics-vidhya/smote-technique-for-unbalanced-data-of-3-classes-in-r-programming-cardiotocography-data-set-474bb5dbf8dd
# https://www.statology.org/smote-in-r/
# https://medium.com/analytics-vidhya/land-cover-classification-97e9a1c77444


