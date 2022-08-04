#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#   Luisa Pflumm - Masterthesis   #
#   Landcover classification      #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# LOAD LIBRARIES =========================================================================================

# Packages for spatial data processing & visualization
library(rgdal)
library(raster)
library(sf)
library(terra)
library(RStoolbox)
library(rasterVis)
library(mapview)
library(grid)
library(RColorBrewer)
library(plotly)
library(caret)
library(randomForest)
library(ranger)
library(data.table)
library(dplyr)
library(stringr)
library(doParallel)
library(parallel)
library(enmSdm)


# INPUT DATA =====================================================================================

# set path where to store/load data
path <- "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis"
#path <- "C:/Arbeit/Luisa/shareJN"    

# clip to smaller test extent for fast processing
#bb <- st_read(path, "bb")   # dsn = path, layer = "bb"



## Import and adjust data ========================================================================

# Import raster data

# !!! 200m scale !!!!!
sentinel <- terra::rast(file.path(path,"S2_median_stddev_2021_months_1-12_SVN_scale200.tif"))   # JN ohne paste
#sentinel <- terra::crop(sentinel,bb)
bands <- names(sentinel)

# !!! virtual raster !!!!!
sentinel <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles SVN/S2_median_stddev_2021_months_1-12_SVN_average_res.vrt")
names(sentinel) <- bands

# Select bands to use for classification
sentinel <- sentinel[[c(1:14,16:29)]]    # better call by name ...
names(sentinel)

# # Subset satellite image with band names
# sentinel <- terra::subset(s2, bands)
# # --> superclass not working when using subset()!

# Import studyarea
aoi <- st_read(path, "SVN_SBC_provinces_final")

# Import and adjust training data
date <- "2.8"  #28.7
td <- st_read(paste0(path,"/LC_classes_",date,".shp"))
td <- st_make_valid(td)
#td <- st_intersection(td, bb)

# Define class parameters dataframe
#JN: evtl  eine Ãberlegung wert das auszulagern in eine csv und zu importieren. Das ist einfacher zu editieren und zu lesen
#> Note: class 3 "semidry-dry forest" was once divided into 

classdf <- data.frame(classvalues = c(1,2,3,4,5,6,7,8,9), #c(1,2,3,5,6,7,8,9,10),
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
                      cpal = c("#162700","#a1a733","#5fa00a","#f86b05","#c22341",
                               "#0500ff","#020103", "#8c00ff","#20e283"))


# # Plot data
# plot(sentinel$B_median)
# plot(aoi$geometry, add=TRUE)
# plot(td$geometry, add=TRUE)



# ## Rasterize training polygons =====================================================================
# 
# # work with raster package
# S2template <- raster::stack(sentinel)
# 
# # Create raster template
# template_rst <- raster::raster(raster::extent(raster::raster(S2template$B_median)), # band B2 has resolution 10 m
#                                resolution = raster::res(raster::raster(S2template$B_median)), # 10,
#                                crs = raster::projection(raster::raster(S2template$B_median)))
# 
# # Convert td from sf to sp format (necessary for raster::rasterize)
# samp <- as_Spatial(td)
# 
# # Rasterize trainingdata
# samp_rst <- raster::rasterize(samp, template_rst, field = "class_id")
# 
# # Export raster
# raster::writeRaster(samp_rst, file.path(paste0(path,"/LC_classes_", date, "_rasterized_10mscale.tif")))
# #raster::writeRaster(samp_rst, file.path(paste0(path,"/LC_classes_", date, "_rasterized_200mscale.tif")))



# Import if already existing (with raster package!)
samp_rst <- raster::raster(file.path(path,"LC_classes_2.8_rasterized_10mscale.tif")) 



## Stratified random sampling ========================================================================

# Define sample size 
siz <- 10000 #10000

# Set seed
set.seed(33)


### Equalized stratified random sampling ###

# # --> size = samplesize per class
# smpl <- sampleStratified(samp_rst, size = siz, xy=TRUE, na.rm = TRUE, sp = TRUE)
# smpl <- smpl[,-1]
# colnames(smpl@data)[3] = "class_id"

# # Export/import equalized samples
# #st_write(st_as_sf(smpl), file.path(paste0(path, "/equstratsamp_",siz,"_td",date,"_scale10.shp")))
# #smpl <- st_read(file.path(paste0(path, "/equstratsamp_",siz,".shp")))



### Weighted stratified random sampling ###

# Return randomly located coordinates over class raster (take care of parameter setting)
#rands1 <- sampleRast(samp_rst, siz)
#rands2 <- sampleRast(samp_rst, siz, adjArea=FALSE)
rands3 <- sampleRast(samp_rst, siz, prob=FALSE)
#rands4 <- sampleRast(samp_rst, siz, adjArea=FALSE, prob=FALSE)

# Plot different sample methods
#par(mfrow=c(2, 2))
#plot(samp_rst, main='adjArea = TRUE & prob = TRUE')
#points(rands1, pch='.')
#plot(samp_rst, main='adjArea = FALSE & prob = TRUE')
#points(rands2, pch='.')
plot(samp_rst, main='adjArea = TRUE & prob = FALSE')
points(rands3, pch='.')
#plot(samp_rst, main='adjArea = FALSE & prob = FALSE')
#points(rands4, pch='.')

# Convert coordiates to spatial object
#r1 <- st_as_sf(data.frame(rands1), coords = c("x", "y"),crs = 4326)
#r2 <- st_as_sf(data.frame(rands2), coords = c("x", "y"),crs = 4326)
r3 <- st_as_sf(data.frame(rands3), coords = c("x", "y"),crs = 4326)
#r4 <- st_as_sf(data.frame(rands4), coords = c("x", "y"),crs = 4326)

# Extract class values from raster
#rp1 <- st_as_sf(cbind(data.frame(class_id = raster::extract(samp_rst, r1)), r1))
#rp2 <- st_as_sf(cbind(data.frame(class_id = raster::extract(samp_rst, r2)), r2))
rp3 <- st_as_sf(cbind(data.frame(class_id = raster::extract(samp_rst, r3)), r3))
#rp4 <- st_as_sf(cbind(data.frame(class_id = raster::extract(samp_rst, r4)), r4))

# Check distribution of samples
#table(rp1$class_id)
#table(rp2$class_id)
table(rp3$class_id)  # -> this represents what I want
#table(rp4$class_id)

smpl <- as_Spatial(rp3)

# Export/import weighted samples
#st_write(st_as_sf(smpl), file.path(paste0(path, "/w_stratsamp_",siz,"_td",date,"_scale10.shp")))
#smpl <- st_read(file.path(paste0(path, "/weighstratsamp_",siz,"_td",date,"_scale10.shp")))




## Split into training and validation =====================================================

# Define split factor
spl <- 0.7     # 70% training, 30% validation
val <- 1-spl

# Convert points to data table
poi <- data.frame(smpl)
poi <- poi[1:3]
poi <- setDT(poi)

# A stratified random split of the data
idx_train <- createDataPartition(poi$class_id,
                                 p = spl, # percentage of data as training
                                 list = FALSE)
train <- poi[idx_train]
test <- poi[-idx_train]

# Convert to sp objects
train_sp <- SpatialPointsDataFrame(coords = train[, .(coords.x1, coords.x2)],  # x,y
                                   data = train %>% select(-c("coords.x1", "coords.x2")), #"x","y")) , 
                                   proj4string = samp_rst@crs)
test_sp <- SpatialPointsDataFrame(coords = test[, .(coords.x1, coords.x2)],  # x,y
                                  data = test %>% select(-c("coords.x1", "coords.x2")), #"x","y")) , 
                                  proj4string = samp_rst@crs)

# Convert class id to factor and set levels to avoid error in training part   (JN: geht wahrscheinlich auch in einem schritt mit factor(x = , levels = ))
train_sp@data$class_id <- as.factor(train_sp@data$class_id)
levels(train_sp@data$class_id) <- as.character(classdf$classnames)

test_sp@data$class_id <- as.factor(test_sp@data$class_id)
levels(test_sp@data$class_id) <- as.character(classdf$classnames)

# Show samples per class
table(train_sp@data$class_id)
table(test_sp@data$class_id)

# Convert to sf
train_sf <- st_as_sf(train_sp)
test_sf <- st_as_sf(test_sp)

# Export train/test samples
#st_write(train_sf, file.path(paste0(path, "/w_stratsamp_",siz,"_td",date,"_scale10_train.shp")))
#st_write(test_sf, file.path(paste0(path, "/w_stratsamp_",siz,"_td",date,"_scale10_test.shp")))




## Plots =========================================================================================

# # Plot sentinel scene
# # JN: stack vorher erstellen. SOnst wird der 2x  on the fly erstellt, das dauert ewig. AuÃerdem, subset auf die verwendeten bÃ¤nder
# stack_sentinel <- raster::stack(sentinel[[c(1, 2, 3, 4, 5)]])
# viewRGB(stack_sentinel, r = 3, g = 2, b = 1, layer.name = "R-G-B") + 
#   viewRGB(stack_sentinel, r = 5, g = 3, b = 2, layer.name = "NIR-R-G")
# 
# # Plot training data and samples
# mapview(aoi,alpha.regions=0,layer.name="VN provinces") + 
#   mapview(td, zcol = "landcover", layer.name = "Landcover classes",
#           col.regions = list("#f86b05","#020103","#20e283","#c22341","#a1a733","#162700","#e9f411","#5fa00a","#0500ff"), alpha.regions=0.5) +
#   mapview(train_sf, col.regions="magenta", color="magenta",layer.name="Training samples",cex=1) + 
#   mapview(test_sf, col.regions="cyan",color="cyan",layer.name="Validation samples",cex=1)




# CLASSIFICATION =============================================================================

## superClass() =============================================================================

# select FALSE for internal prediction
SC_prob<- RStoolbox::superClass(sentinel,
                                trainData = train_sf,
                                valData = test_sf,
                                responseCol = "class_id",
                                #nSamples = 1000,           # ignored if input is of class spatial points
                                #polygonBasedCV = TRUE,     # ignored if input is of class spatial points
                                #trainPartition = 0.7,      # ignored if input valData is provided
                                model = "rf",
                                tuneLength = 3,
                                kfold = 5,        
                                minDist = 1,
                                mode = "classification",   # regression
                                predict = FALSE,
                                predType = "prob",         # raw
                                #filename = path-to-file,
                                verbose = TRUE)
                                #overwrite = TRUE)

# save / load superclass object
save(SC_prob, file = file.path(
  paste0(path,"/SC_prob_smp",siz,"_spl",spl,"_td",date,"scale10.RData")))

#SC_prob <- load(file.path(paste0(path,"/SC_prob_smp",siz,"_spl",spl,"_td",date,".RData")))




## Prediction =================================================================================================

### Option 1: repredict with sentinel scene ==========================================================================

# Mask raster with studyarea
#s2mask <- sentinel
#s2mask <- terra::mask(sentinel, terra::vect(aoi))


  #______________________________________________________________________________
  # OPTIONAL: AGGREGATE RASTER TO HIGHER RESOLUTION TO SPEED UP COMPUTATION TIME

  # Clip to extent of aoi
  #sentinel <- raster::crop(sentinel, extent(aoi))

  # Aggregate 10m sentinel raster to 100m resolution to reduce prediction time
  #s2_agg <- aggregate(sentinel, fact = (100/10), fun = mean, na.rm = TRUE)

  # Export / Import aggregated raster
  #terra::writeRaster(s2_agg, file.path(paste0(path,"S2_median_stddev_2021_months_1-12_SVN_aggr100.tif")))
  s2_agg <- terra::rast(file.path(path,"S2_median_stddev_2021_months_1-12_SVN_aggr100.tif"))

  # Mask raster with studyarea
  s2mask <- terra::mask(x=s2_agg, mask=terra::vect(aoi))
  
  #______________________________________________________________________________


# Predict with masked raster
newpred1 <- terra::predict(SC_prob, s2mask, predType = "prob", progress="text")
names(newpred1) <- classdf$classnames  # JN: scheint nicht nÃ¶tig



### Option 2: parallel prediction ================================================================================

???




### Option 3: create data frame with rastervalues and make predictions out of this ==============================================================


# # Create df of rastervals
# vals <- as.data.frame(values(s2mask))
# 
# # Remove NA
# vals_subset <- vals[which(!is.na(rowSums(vals))),]
# colnames(vals_subset) <- names(sentinel)
# 
# # Predict from dataframe
# newpred2 <- terra::predict(SC_prob, vals_subset, predType = "prob", progress="text")
 
??? ## ---> Error in eval(predvars, data, env) : object 'R_median' not found ??





#### Multiband probability layer ======================================================================

# if superClass() "predict = TRUE" (internal prediction)
#prob_stack <- SC_prob$map

# if superClass() "predict = FALSE" (external prediction)
prob_stack <- raster::stack(newpred1)
#probstack <- newpred2

# plot output
plot(prob_stack)
rasterVis::levelplot(prob_stack)




#### Singleband classification layer ===================================================================

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



### Plots ========================================================================================

# Plot discrete classification output
mapview(raw, col.regions=list("#162700","#a1a733","#37590a","#f86b05","#c22341","#0500ff","#020103","#e9f411","#20e283"), alpha.regions=1, layer.name="LC classification") 
mapview(raw_filterR, col.regions=list("#162700","#a1a733","#37590a","#f86b05","#c22341","#0500ff","#020103","#e9f411","#20e283"), alpha.regions=1, layer.name="LC classification (filter)")

# Plot class probabilities
mapview(prob_stack$semidry.dry.forest)
#viewRGB(prob_stack, r=3,g=2,b=1, layer.name = "Semi-Dry-Ever") + mapview(prob_stack$semidry.dry.forest,layer.name = "semidry.dry.forest")





### Export ===========================================================================================

# exp1 <- terra::rast(prob_stack)
# exp2 <- terra::rast(raw)
# exp3 <- terra::rast(raw_filterR)
# 
# name1 <- "/RF_prob"
# name2 <- "/RF_class"
# name3 <- "/RF_class_filteredR"
# 
# terra::writeRaster(exp1, paste0(path,name1,"_td",date,"_wsmp",siz,"_spl",spl,"_scale10-pred100.tif"))
# terra::writeRaster(exp2,paste0(path,name2,"_td",date,"_wsmp",siz,"_spl",spl,"_scale10-pred100.tif"))
# terra::writeRaster(exp3, paste0(path,name3,"_td",date,"_wsmp",siz,"_spl",spl,"_scale10-pred100.tif"))




### Accuracy assessment ================================================================================

# Validation performance estimates based on independent validation (confusion matrix etc.)
SC_prob$validation$performance

# Predictor importance
caret::varImp(SC_prob$model)$importance %>% 
  as.matrix %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 600, height = 600)




### Modelstatistics ================================================================================

# the fitted model
SC_prob$model

# model fit statistics
SC_prob$modelFit

# # indexes of samples used for training
# SC_prob$training
# 
# # actual pixel coordinates plus reference and predicted values used for validation
# SC_prob$validation$validationSamples
# 
# # validation polygpns (clipped with mindist to training geometries)
# SC_prob$validation$validationGeometry
# 
# # the predicted raster
# SC_prob$map
# 
# # data.frame containing an integer <-> label mapping
# SC_prob$classMapping



