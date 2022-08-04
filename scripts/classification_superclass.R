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


# INPUT DATA =====================================================================================

# set path where to store/load data
path <- "C:/Users/s347553/Documents/Masterthesis/shareJN/"

# clip to smaller test extent for fast processing
bb <- st_read(file.path(paste0(path, "bb.shp")))


## Import and adjust data ========================================================================

# Import raster data

# !!! 200m scale !!!!!
sentinel <- terra::rast(file.path(paste0(path,"S2_median_stddev_2021_months_1-12_SVN_scale200.tif")))
sentinel <- terra::crop(sentinel,bb)
bands <- names(sentinel)

# !!! virtual raster !!!!!
#sentinel <- terra::rast("D:/Luisa/covariates/Sentinel/final_medianstddev/Sentinel_tiles SVN/S2_median_stddev_2021_months_1-12_SVN_average_res.vrt")
#names(sentinel) <- bands

# Select bands to use for classification
sentinel <- sentinel[[c(1:14,16:29)]]
names(sentinel)

# # Subset satellite image with band names
# sentinel <- terra::subset(s2, bands)
# # --> superclass not working when using subset()!

# Import studyarea
aoi <- st_read(file.path(paste0(path,"SVN_SBC_provinces_final.shp")))

# Import and adjust training data
td <- st_read(file.path(paste0(path,"LC_classes_13.7.shp")))
td <- st_make_valid(td)
td <- st_intersection(td,bb)

# Define class parameters dataframe
classdf <- data.frame(classvalues = c(1,2,3,5,6,7,8,9,10),
                      landcover = c("Evergreen Forest",
                                    "Dry Shrubland",
                                    "Semidry/Dry Forest",
                                    "Bare Ground",
                                    "Cropland",
                                    "Water",
                                    "Built-up",
                                    "Grassland",
                                    "Coniferous Forest"),
                      classnames = c("Evergreen.Forest",
                                     "Dry.Shrubland",
                                     "Semidry.Dry.Shrubland",
                                     "Bare.Ground",
                                     "Cropland",
                                     "Water",
                                     "Built.up",
                                     "Grassland",
                                     "Coniferous.Forest"),
                      cpal = c("#162700","#a1a733","#5fa00a","#f86b05","#c22341",
                               "#0500ff","#020103", "#8c00ff","#20e283"))

# Need to have a numeric id for each class - helps with rasterization later on.
td$class_id <- as.integer(td$class_id)


# # Plot data
# plot(sentinel$B_median)
# plot(aoi$geometry, add=TRUE)
# plot(td$geometry, add=TRUE)



## Rasterize training polygons =====================================================================

# # Create raster template
# template_rst <- raster::raster(raster::extent(raster::raster(sentinel$B_median)), # band B2 has resolution 10 m
#                                resolution = raster::res(raster::raster(sentinel$B_median)), # 10,
#                                crs = projection(raster::raster(sentinel$B_median)))
# 
# # Convert td from sf to sp format (necessary for raster::rasterize)
# samp <- as_Spatial(td)
# 
# # Need to have a numeric id for each class - helps with rasterization later on.
# samp@data$class_id <- as.integer(factor(samp@data$class_id))
# 
# # Set as data table
# #setDT(samp@data)
# 
# # Rasterize trainingdata
# samp_rst <- raster::rasterize(samp, template_rst, field = "class_id")
#
# # Export raster
# #raster::writeRaster(samp_rst, file.path(paste0(path,"LC_classes_13.7_rasterized_10mscale.tif")))

# Import if already existing (with raster package!)
samp_rst <- raster::raster(file.path(paste0(path,"LC_classes_13.7_rasterized_10mscale.tif")))
samp_rst <- raster::crop(samp_rst,bb)




## Stratified random sampling ========================================================================

# Define sample size per class
siz <- 100 #10000

# Define split factor
spl <- 0.3 #0.7


# Take samples from LC raster
set.seed(33)
smpl <- sampleStratified(samp_rst, size = siz, xy=TRUE, na.rm = TRUE, sp = TRUE)
smpl <- smpl[,-1]
colnames(smpl@data)[3] = "class_id"



## Split into training and validation =====================================================

# Convert points to data table
poi <- data.frame(smpl)
poi <- poi[1:3]
poi <- setDT(poi)

# A stratified random split of the data
set.seed(321)
idx_train <- createDataPartition(poi$class_id,
                                 p = spl, # percentage of data as training
                                 list = FALSE)
train <- poi[idx_train]
test <- poi[-idx_train]

# Convert to sp objects
train_sp <- SpatialPointsDataFrame(coords = train[, .(x, y)],
                                   data = train %>% select(-c("x","y")) ,
                                   proj4string = samp_rst@crs)
test_sp <- SpatialPointsDataFrame(coords = test[, .(x, y)],
                                  data = test %>% select(-c("x","y")) ,
                                  proj4string = samp_rst@crs)

# Convert class id to factor and set levels to avoid error in training part
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




## Plots =========================================================================================

# Plot sentinel scene
viewRGB(raster::stack(sentinel), r=3, g=2, b=1, layer.name = "R-G-B") + viewRGB(raster::stack(sentinel), r=5, g=3, b=2, layer.name = "NIR-R-G")

# Plot training data and samples
mapview(aoi,alpha.regions=0,layer.name="VN provinces") + mapview(td, zcol = "landcover", layer.name = "Landcover classes",col.regions = list("#f86b05","#020103","#20e283","#c22341","#a1a733","#162700","#e9f411","#5fa00a","#0500ff"), alpha.regions=0.5) + mapview(train_sf, col.regions="magenta", color="magenta",layer.name="Training samples",cex=1) + mapview(test_sf, col.regions="cyan",color="cyan",layer.name="Validation samples",cex=1)




# CLASSIFICATION =============================================================================

## superClass() =============================================================================

# select FALSE for internal prediction
SC_prob<- RStoolbox::superClass(sentinel,
                                trainData = train_sp,
                                valData = test_sp,
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
obj <- "SC_prob_FALSEpredict.RData"

#save(SC_prob, file = file.path(paste0(path,"SC_smp",siz,"_spl",spl,"_",obj)))
#load(file.path(paste0(path,"SC_smp",siz,"_spl",spl,"_",obj)))




### Prediction ====================================================================================

# ---> if superclass() "predict = FALSE" <---

  # #______________________________________________________________________________
  # # OPTIONAL: AGGREGATE RASTER TO HIGHER RESOLUTION TO SPEED UP COMPUTATION TIME
  # 
  # # Clip to extent of aoi
  # #sentinel <- raster::crop(sentinel, extent(aoi))
  # 
  # # Aggregate 10m sentinel raster to 100m resolution to reduce prediction time
  # s2_agg <- aggregate(sentinel, fact = (100/10), fun = mean, na.rm = TRUE)
  # 
  # # Mask raster with studyarea
  # s2mask <- terra::mask(x=s2_agg, mask=terra::vect(aoi))
  # 
  # # Export raster
  # #terra::writeRaster(s2mask, file.path(paste0(path,"S2_median_stddev_2021_months_1-12_SVN_aggr100_mask.tif")))
  # #______________________________________________________________________________

# Option 1: repredict with (aggregated) sentinel scene

# Mask raster with studyarea
s2mask <- sentinel
#s2mask <- terra::mask(sentinel, terra::vect(aoi))    

# Predict with masked raster
newpred1 <- terra::predict(SC_prob, s2mask, predType = "prob", progress="text")
names(newpred1) <- classdf$classnames


# Option 2: create data frame with rastervalues and make predictions out of this

...



#### Multiband probability layer ======================================================================

# if superClass() "predict = TRUE" (internal prediction)
#prob_stack <- SC_prob$map

# if superClass() "predict = FALSE" (external prediction)
prob_stack <- raster::stack(newpred1)
#probstack <- newpred2

# plot output
plot(prob_stack)
#rasterVis::levelplot(prob_stack)




#### Singleband classification layer ===================================================================

# Combine probability layers to one final layer product by selecting the max probability value
which.max2 <- function(x, ...) which.max(x) [1]  # helper function to absorb "na.rm"
                                                 # added "[1]", originally worked without      
raw <-  stackApply(prob_stack, rep(1,nrow(classdf)), fun=which.max2, na.rm=NULL)
levels(raw)=data.frame(ID=1:nrow(classdf), class=classdf$classnames)


# Smoothing of the classification output (majority filter)    
# --> A Majority Filter re-samples the cells in a raster according to the majority value of the neighbor cells
# --> Apply rectangular moving window to discrete data (e.g. integer)
# --> 3x3 moving window; note, function is set to modal, so we will find the mode or count of each number...this essentially treats it as categorical
raw_filterR <- raster::focal(raw, w=matrix(1,3,3), fun=raster::modal)    # 3x3 moving window
levels(raw_filterR)=data.frame(ID=1:nrow(classdf), class=classdf$classnames)




### Plots ========================================================================================

# Plot discrete classification output
mapview(raw, col.regions=list("#162700","#a1a733","#37590a","#f86b05","#c22341","#0500ff","#020103","#e9f411","#20e283"), alpha.regions=1, layer.name="LC classification") + mapview(raw_filterR, col.regions=list("#162700","#a1a733","#37590a","#f86b05","#c22341","#0500ff","#020103","#e9f411","#20e283"), alpha.regions=1, layer.name="LC classification (filter)")

# Plot class probabilities
mapview(prob_stack)
#viewRGB(prob_stack, r=3,g=2,b=1, layer.name = "Semi-Dry-Ever") + mapview(prob_stack$Semidry.Dry.Shrubland,layer.name = "Semidry.Dry.Shrubland")




### Export ===========================================================================================

# exp1 <- terra::rast(prob_stack)
# exp2 <- terra::rast(raw)
# exp3 <- terra::rast(raw_filterR)
# 
# name1 <- "RF_prob_repredict"
# name2 <- "RF_class_repredict"
# name3 <- "RF_class_filteredR_repredict"
# 
# terra::writeRaster(exp1, paste0(path,name1,".tif"))
# terra::writeRaster(exp2,paste0(path,name2,".tif"))
# terra::writeRaster(exp3, paste0(path,name3,".tif"))




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



