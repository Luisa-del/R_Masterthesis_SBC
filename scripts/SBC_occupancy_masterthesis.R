#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# SINGLE SPECIES OCCUPANCY MODELING       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#     MASTER THESIS     LUISA PFLUMM      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(unmarked)
library(raster)
library(rgeos)
library(lubridate)
library(usdm)
library(sf)
library(ggcorrplot)
library(ggpubr)
library(GGally)
library(DT)
library(data.table)
library(dplyr)
library(spatialEco)
# install.packages("remotes")
# install.packages("R.rsp")
library(remotes)
library(R.rsp)
#remotes::install_github("jniedballa/camtrapR", build_vignettes = TRUE)
library(camtrapR)


path1 <- "C:/Users/s347553/Documents/Masterthesis/Data/CT"
#path <- "C:/Arbeit/Luisa/shareJN/CTdata"   

# # don't allow to show e numbers
# options(scipen = 100, digits = 4)


# 1. CT data ================================================================================================================

# Import CT and record tables
recTable <- read.csv(file = file.path(path1, "Quy/corrected_recTable_Quy_editLP_shareJN.csv"))
CTtable <- read.csv2(file = file.path(path1, "CTfinal/CTtables_combined.csv"), sep = ";")




# Correct species names
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("Blue rumped", "Blue-rumped", x)))
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("Orange headed", "Orange-headed", x)))
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("Pig tailed", "Pig-tailed", x)))
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("Small Indian", "Small indian", x)))
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("White crested", "White-crested", x)))




# Warning: At 1 stations there were records before camera operation date range:  PY029 
recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-03 06:02:00" & 
                            recTable$station == "PY029"] <- "2021-04-03 06:02:00" 
recTable$Date[recTable$Date == "4/3/2020" & recTable$Date == "PY029"] <- "4/3/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-15 18:06:00" & 
                            recTable$station == "PY029"] <- "2021-04-15 18:06:00" 
recTable$Date[recTable$Date == "4/15/2020" & recTable$Date == "PY029"] <- "4/15/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-05-07 18:09:00" & 
                            recTable$station == "PY029"] <- "2021-05-07 18:09:00" 
recTable$Date[recTable$Date == "5/7/2020" & recTable$Date == "PY029"] <- "5/7/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-05 11:59:00" & 
                            recTable$station == "PY029"] <- "2021-04-05 11:59:00" 
recTable$Date[recTable$Date == "4/5/2020" & recTable$Date == "PY029"] <- "4/5/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-05-19 18:00:00" & 
                            recTable$station == "PY029"] <- "2021-05-19 18:00:00" 
recTable$Date[recTable$Date == "5/19/2020" & recTable$Date == "PY029"] <- "5/19/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-08 11:44:00" & 
                            recTable$station == "PY029"] <- "2021-04-08 11:44:00" 
recTable$Date[recTable$Date == "4/8/2020" & recTable$Date == "PY029"] <- "4/8/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-01 06:52:00" & 
                            recTable$station == "PY029"] <- "2021-04-01 06:52:00" 
recTable$Date[recTable$Date == "4/1/2020" & recTable$Date == "PY029"] <- "4/1/2021"



# 2. Covariates =============================================================================================================

# import elevation
e <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_elev_CTall_utm.csv")

# import habitat
h <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_hab10-100_10000_CTall_utm.csv")
h[167,2] <- 0.106   #-> in this case station ncs003 has no value because point to close at rim of mask, aggregated values --> 0

# import remoteness
#d <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_dist_CTall_utm.csv")
# --> Distance raster mit vorsicht genießen, werte verändern sich nur alle 30m, da SRTM als basis

# # combine covariates
# covariates <- cbind(e[2],h[2],d[2])
# names(covariates) <- c("elevation", "habitat", "distance")

# combine covariates
covariates <- cbind(e[2],h[2])
names(covariates) <- c("elevation", "habitat")



# # import remoteness
# r <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_rem_CTall_utm.csv")
# # combine covariates
# covariates <- cbind(e[2],h[2],r[2],d[2])
# names(covariates) <- c("elevation", "habitat", "remoteness", "distance")


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



# add columns with squared covariate
covariates <- cbind(covariates,
                    scale.elevation_sq = covariates$scale.elevation^2)
datatable(round(covariates,2),extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))
dim(covariates)


# 3. Collinearity test ================================================================================================

#> Covariates with <-70% or >70% collinearity are considered as highly correlated and they are excluded from the analysis. 
#> There are several statistical tests than estimate collinearity between covariates, such as Pearson collinearity test 
#> and VIF multicollinearity test.
#> 
#> You can calculate variance inflation factor (VIF) for a set of variables and exclude the highly correlated 
#> variables from the set through a stepwise procedure. 
#> This method can be used to deal with multicollinearity problems when you fit statistical models


# Compute a correlation matrix
(corr <- round(cor(covariates %>% select( -matches("scale."))), 2))

# create corrplot for manyand export as pdf or image
ggcorrplot <- ggcorrplot(corr,                             # visualize the correlation-matrix
                         hc.order = FALSE,                  # use hierarchical clustering
                         type = "lower",                    # alternative: upper, full
                         lab = TRUE,                        # add correlation coefficients, change size
                         ggtheme = ggplot2::theme_grey(),   # alternative: ggplot2::theme_minimal, ggplot2::theme_dark
                         title = "Correlation of Covariates"
                         # p.mat = p.mat,                   # Barring the no significant coefficient
) +
  theme(plot.title = element_text(size=22))                  # add other relevant theme and guides methods (still ggplot2 object)

plot(ggcorrplot)

(vif <- vifcor(covariates %>% select( -matches("scale.")), th=0.7))





# 4. Species selection =================================================================================================

## 4.1 Camera operation ======================================================================================

camop_matrix <- cameraOperation(CTtable = CTtable,
                                stationCol ="station",
                                cameraCol = "camera",
                                setupCol = "date_setting",
                                retrievalCol ="date_retrieval",
                                hasProblems = TRUE,
                                byCamera = FALSE,
                                allCamsOn = FALSE,
                                camerasIndependent = TRUE,
                                writecsv = FALSE,
                                dateFormat = "dmy" #"ymd"
                                #outDir = outDir,
)


## 4.2 Survey summary ===========================================================================================

# surveyReport() now with reduced datasets
reportTest <- surveyReport( recordTable          = recTable,
                            CTtable              = CTtable,
                            camOp                = camop_matrix,
                            speciesCol           = "Species",
                            stationCol           = "station",
                            cameraCol            = "camera",
                            setupCol             = "date_setting",
                            retrievalCol         = "date_retrieval",
                            CTDateFormat         = "ymd",
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "ymd HMS",#"%Y-%m-%d %H:%M:%S",
                            #CTHasProblems        = TRUE,
                            makezip              = FALSE)                  # set TRUE if you want Zip-file



# Check list elements of survey report
datatable(reportTest[[1]],extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))    # camera trap operation times and image date ranges
datatable(reportTest[[2]],extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))    # number of species by station
datatable(reportTest[[3]],extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))    # number of events and number of stations by species
datatable(reportTest[[4]],extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))    # number of species events by station


## 4.3 Select relevant species ==============================================================================================

myspecies <- "Silver-backed chevrotain" #rownumber 43

# For occupancy analysis select species with **n_events >= 10 and n_stations >=3**
relevantspecies <- reportTest[[3]]
relevantspecies <- relevantspecies[as.numeric(relevantspecies$n_events) >= 10 & as.numeric(relevantspecies$n_stations) >= 3,]

# Set stations as rownames
rownames(relevantspecies) <- relevantspecies$species

# Remove not wanted species
species.remove <- c("Blank", "Retrieval", "Setting", "Squirrel", "Unidentified animal", "Unidentified macaque", "Unidentified bird", "Unidentified mammal")
relevantspecies <- relevantspecies[!(row.names(relevantspecies) %in% species.remove), ]

# Set numbers as rownames
rownames(relevantspecies) <- nrow(1:length(relevantspecies))




## 4.4 Species activity&presence ======================================================================================

### Activity plot ###

for (i in 43:43) { #1:length(unique(relevantspecies[,"species"]))) {
  activityDensity(recordTable = recTable,
                  species     = myspecies)   #relevantspecies$species[43]
}


### Species presence ###

# Define shapefile parameters
aoi <- shapefile("C:/Users/s347553/Documents/Masterthesis/Data/SVN_SBC_provinces_final.shp")

# Plot
for (i in 43:43){ #1:length(unique(relevantspecies[,"species"]))) {
  detectionMaps( CTtable       = CTtable,
                 recordTable   = recTable,
                 Xcol          = "long", #"UTM_X",
                 Ycol          = "lat", #"UTM_Y",
                 backgroundPolygon = aoi,        # enable if you want to add background polygon
                 stationCol    = "station",
                 speciesCol    = "Species",
                 speciesToShow = myspecies,  #relevantspecies$species[43],          # enable if you want to plot just one species
                 printLabels   = FALSE,
                 richnessPlot  = FALSE,                      # set TRUE to create map of the number of observed species
                 speciesPlots  = TRUE,                       # set TRUE to create single plots for each species
                 writePNG      = FALSE,                      # set TRUE for png output
                 #plotDirectory = file.path(wd,"/Output/Speciesplots"),
                 writeShapefile      = FALSE                  # set TRUE for shapefile creation
                 # shapefileName       = "SBC_occ_detections", #shapefileName,
                 # shapefileDirectory  = file.path(path1, "shps"), #pathout_shapefiles,
                 # shapefileProjection = "+proj=longlat +datum=WGS84 +no_defs" #shapefileProjection)
  )
}



# 5. Occupancy modeling ====================================================================================================

## 5.1 Camera operation ====================================================================================================

#--> see 4.1 
camop_matrix <- camop_matrix





## 5.2 Detection history&effort ====================================================================================================

# Define timezone and occasion length.
timeZone <- "Asia/Ho_Chi_Minh" # change the time zone based on your study area
occasionLength <- 5 # change the occasion length based on your study


  detHist <- detectionHistory(recordTable = recTable,
                                 species = myspecies, #relevantspecies$species[[i]],
                                 camOp = camop_matrix,
                                 speciesCol = "Species",
                                 stationCol = "station",
                                 occasionLength =  occasionLength,
                                 day1 = "station",
                                 datesAsOccasionNames = FALSE,
                                 includeEffort = TRUE,
                                 scaleEffort = FALSE,
                                 writecsv = FALSE,                            # set TRUE if you want csv-file
                                 timeZone = timeZone#,
                                 #outDir = file.path(wd,"/Output")
  )
  




## 5.3 Unmarked frames ====================================================================================================


  umf <- unmarkedFrameOccu(y        = as.matrix(detHist[[1]]),
                          siteCovs = covariates,  #covariates_sq,
                          obsCovs  = list(Effort=as.matrix(detHist[[2]])))
  
  

## 5.4 Detection models ====================================================================================================


    # Define null models
  (d.mod00 <- occu(~1 ~1, data = umf))            # no covariates
  (d.mod0 <- occu(~ Effort ~ 1, data = umf))      # effort as covariate
  
  # Create list for modelselection 
  modellist_detection <- fitList(fits = list("p(.) psi(.) data" = d.mod00,
                                             "p(Effort) psi(.) data"  = d.mod0))
  
  # Modelselection
  (d_modsel <- modSel(modellist_detection))
  
  # Convert to df
  d_modsel_df <- data.frame(model = d_modsel@Full$model,
                                 covariate = gsub("~Effort ~ ", "", d_modsel@Full$formula, ),
                                 nPars = d_modsel@Full$nPars,
                                 AIC=d_modsel@Full$AIC,
                                 delta=d_modsel@Full$delta,
                                 AICwt=d_modsel@Full$AICwt,
                                 cumltvWt=d_modsel@Full$cumltvWt
                                 #p.Effort=d_modsel@Full$`p(Effort)`,
                                 #SEp.Effort=d_modsel@Full$`SEp(Effort)`,
                                 #p.Int=d_modsel@Full$`p(Int)`,
                                 #SEp.Int=d_modsel@Full$`SEp(Int)`,
                                 #psi.Int=d_modsel@Full$`psi(Int)`,
                                 #SEpsi.Int=d_modsel@Full$`SEpsi(Int)`
  )
  
  # Round dataset
  (d_modsel_df_round <- data.frame(d_modsel_df[, c(1,2)], round(d_modsel_df[, -c(1,2)], 3)))
  
  
  # # Plot data
  # DT::datatable(d_modsel_df_round)






## 5.5 Occupancy models ====================================================================================================

### model combinations -----------------------------------------------------------

# scaled covariates have to exist as variables in R environment.
scale.elevation <- covariates$scale.elevation
scale.habitat<- covariates$scale.habitat
#scale.remoteness <- covariates$scale.remoteness
#scale.distance <- covariates$scale.distance
scale.elevation_sq <- covariates$scale.elevation_sq

# Select best detection model
if (d_modsel@Full$model[1] == "p(.) psi(.) data") {
  d.mod <- d.mod00
  d <- ~1
} else {
  d.mod <- d.mod0
  d <- ~Effort
}



DOUBLECHECK ~Effort or ~1 for o.models & fitlist below
DOUBLECHECK ~Effort or ~1 for o.models & fitlist below
DOUBLECHECK ~Effort or ~1 for o.models & fitlist below
DOUBLECHECK ~Effort or ~1 for o.models & fitlist below
DOUBLECHECK ~Effort or ~1 for o.models & fitlist below




  # Define occupancy models
  #single linear effect
  o.mod1 <- occu(~ Effort ~ scale.elevation, data = umf) # --> NEEDS AUTOMATISATION!!
  o.mod2 <- occu(~ Effort ~ scale.habitat, data = umf)   # --> can be ~Effort, can be ~1
  #o.mod3 <- occu(~ Effort ~ scale.distance, data = umf) 
  
  #multiple linear effect
  o.mod4 <- occu(~ Effort ~ scale.elevation + scale.habitat, data = umf) 
  #o.mod5 <- occu(~ Effort ~ scale.elevation + scale.distance, data = umf) 
  #o.mod6 <- occu(~ Effort ~ scale.habitat + scale.distance, data = umf) 
  
  o.mod5 <- occu(~ Effort ~ scale.elevation + scale.habitat + scale.elevation_sq, data = umf) 
  
  # multiple qudratic effects
  #o.mod7 <- occu(~ Effort ~ scale.elevation + scale.habitat + scale.distance, data = umf) 
  #o.mod8 <- occu(~ Effort ~ scale.elevation + scale.habitat + scale.distance + scale.elevation_sq, data = umf) 
  
  # # add remoteness?
  # o.mod9 <- occu(~ Effort ~ scale.remoteness, data = umf) 
  # o.mod10 <- occu(~ Effort ~ scale.remoteness + scale.distance, data = umf)
  # o.mod11 <- occu(~ Effort ~ scale.remoteness + scale.habitat, data = umf) 
  # o.mod12 <- occu(~ Effort ~ scale.remoteness + scale.elevation, data = umf)
  # o.mod13 <- occu(~ Effort ~ scale.remoteness + scale.elevation + scale.habitat + scale.distance, data = umf) 
  # o.mod14 <- occu(~ Effort ~ scale.remoteness + scale.elevation + scale.habitat + scale.distance, scale.elevation_sq, data = umf)

# c <- as.character(o.mod1@formula)
# c <- paste0(c[2:3], collapse = " ")
# dm <- as.character(d.mod@formula)
# dm <- paste0(dm[2:3], collapse = " ")

  
  # Create list for modelselection
  fms <-  #modellist_occupancy
    fitList("p(Effort) psi(.) data" = d.mod,   #to do!: find best nullmodel for each species
            "p(Effort) psi(elevation) data" = o.mod1,
            "p(Effort) psi(habitat) data" = o.mod2,
            #"p(Effort) psi(distance) data" = o.mod3,
            "p(Effort) psi(elevation + habitat) data" = o.mod4,
            
            "p(Effort) psi(elevation + habitat + habitat_sq) data" = o.mod5
            
            #"p(Effort) psi(elevation + distance) data" = o.mod5,
            #"p(Effort) psi(habitat + distance) data" = o.mod6,
            #"p(Effort) psi(elevation + habitat + distance) data" = o.mod7,
            #"p(Effort) psi(elevation + habitat + distance + habitat_sq) data" = o.mod8
            
            # ,
            # "p(Effort) psi(remoteness) data" = o.mod9,
            # "p(Effort) psi(remoteness + distance) data" = o.mod10,
            # "p(Effort) psi(remoteness + habitat) data" = o.mod11,
            # "p(Effort) psi(remoteness + elevation) data" = o.mod12,
            # "p(Effort) psi(remoteness + elevation + habitat + distance) data" = o.mod13,
            # "p(Effort) psi(remoteness + elevation + habitat + distance + habitat_sq) data" = o.mod14
            )

  
  # Modelselection
  (modsel <- modSel(fms))
  
  
  # Convert to df
  modsel_df <- data.frame(model = modsel@Full$model,
                               covariate = gsub("~Effort ~ ", "", modsel@Full$formula, ),
                               nPars = modsel@Full$nPars,
                               AIC=modsel@Full$AIC,
                               delta=modsel@Full$delta,
                               AICwt=modsel@Full$AICwt,
                               cumltvWt=modsel@Full$cumltvWt
                               #p.Effort=modsel@Full$`p(Effort)`,
                               #SEp.Effort=modsel@Full$`SEp(Effort)`,
                               #p.Int=modsel@Full$`p(Int)`,
                               #SEp.Int=modsel@Full$`SEp(Int)`,
                               #psi.Int=modsel@Full$`psi(Int)`,
                               #SEpsi.Int=modsel@Full$`SEpsi(Int)`
  )
  
  
  # Round dataset
  (modsel_df_round <- data.frame(modsel_df[, c(1,2)], round(modsel_df[, -c(1,2)], 3)))
  
  # Plot data
  DT::datatable(modsel_df_round)



# modsel_df_round_SC_prob_eq10000_10_100 <- modsel_df_round[[43]]
# write.csv(modsel_df_round_SC_prob_eq10000_10_100, 
#           "D:/Luisa/Rsuperclass/.../modsel_df_round_SC_prob_eq10000_10_100.csv")     






#### --> save / import R.data ---------------------------------------------------------------------------------------------

# Save whole workspace to working directory (date: 01.09.2022)
#save.image("C:/Users/s347553/Documents/Masterthesis/Data/occupancy/SBC_occupancy_11.11.RData")            ##save.image("D:/Dateien/Uni/Eagle_Master/Masterthesis/R_Masterthesis_SBC/testing/occupancy/SBC_occupancy_1.9.RData")

# Load workspace back to RStudio
#load("C:/Users/s347553/Documents/Masterthesis/Data/occupancy/SBC_occupancy_11.11.RData")                  ##load("D:/Dateien/Uni/Eagle_Master/Masterthesis/R_Masterthesis_SBC/testing/occupancy/SBC_occupancy_1.9.RData")




# ## 5.6 Response Curves ========================================================================================

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df_pred <- data.frame(scale.habitat = seq(-2,2, length.out = 100),
                      scale.remoteness = 0)
x <- predict(o.mod6[[43]], newdata  = df_pred, type = "state")
new <- cbind(df_pred,x)

plot(new$Predicted ~ new$scale.habitat, ylim=c(0,1), type="l", )
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



# ### HIER HÃNGTS NOCH BISSCHEN ####
# # an daten im script angepasst, aber plots wollen nicht
# 
# 
# #> To visualise habitat associations, we can plot response curves for each species,
# #> based on the models in modelselection tables above (A2.), showing standard errors (darkgrey)
# #> and 95% confidence intervals (lightgrey).
# 
# 
# Set path to source code for adapted plotfunction
# source(file.path("C:/Users/s347553/Documents/R/PlotResponseCurves_uni.R"))
# 
# covplot <- sapply(fms@fits[-1], continuous.plot, modsel = modsel, covariates = covariates %>% select( -matches("scale.")), quiet = TRUE)
# print(covplot)

# Set path to source code for adapted plotfunction
#source(file.path(wd,"/scripts/PlotResponseCurves_uni.R"))
source("C:/Users/s347553/Documents/R/multi-plot_edit_LP_v2.R")

tmpdir <- tempdir()

# for (i in 43:43){#1:length(unique(detHist))){
# 
#   # cat("\n")
#   # cat("#####", names(detHist)[i], "  \n")
# 
#   if (names(fms@fits) != "~Effort ~ 1")  # alternativ: "~1 ~ 1"
#     #print("true"

    multiplot.wrapper(model.list =  fms@fits,
                      siteCovs_unscaled = covariates %>% select( matches("scale.")), # original: ( -matches("scale."))
                      modelRanking = modsel,                                         # -> !! geht dann aber nicht !!
                      occasionLength = occasionLength,
                      #plotnames.by.rank ,
                      plotDir = tmpdir,
                      speciesVector = myspecies, #relevantspecies$species[[i]],
                      index_tmp = 1,
                      addRug = TRUE,
                      #maximumRankToPlot = 200,
                      plotModelsBelowNullModel = F,
                      quiet = T
)

    

# 
#   else(
#     print("nullmodel was bestmodel, so no covariates explain occuence")
#     # multiplot.wrapper(model.list = fms@fits[2],
#     #                           siteCovs_unscaled = covariates %>% select( -matches("scale.")),
#     #                           modelRanking = modsel,
#     #                           occasionLength = occasionLength,
#     #                           #plotnames.by.rank ,
#     #                           plotDir = tmpdir,
#     #                           speciesVector = myspecies, #relevantspecies$species[[i]],
#     #                           #index_tmp = 1,
#     #                           addRug = TRUE,
#     #                           #maximumRankToPlot = 200,
#     #                           plotModelsBelowNullModel = F,
#     #                           quiet = T
#     # )
#     # )
#     # cat("   \n")
# }




# 6. Predictions ==============================================================================================

## 6.1 Import and scale covariate rasters =========================================================================

#> 1. Import whole raster       (r <- raster::raster("path-to-raster.tif"))
#> 2. Crop to aoi               (rc <- raster::crop(r, as_spatial(aoi)))
#> 3. Mask with AOI             (rm <- raster::mask(rc, as_Spatial(aoi)))
#> 4. Aggregate to higher res   (ragg <- raster::aggregate(r, fact = (200/30), fun = mean, na.rm = TRUE))



### Elevation ======================================================================================================

# Import elevation raster

#elevationraster <- raster("D:/Luisa/covariates/elevation/SRTM30m_NAs_filled_SVN_mask_int2s.tif")    ##elevationraster <- raster("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Elevation/final_SVN/SRTM30m_NAs_filled_SVN_mask_int2s.tif")

# Aggregate 30m elevation raster to 200m resolution to match with other rasters
#elevr_agg <- aggregate(elevationraster, fact = (200/30), fun = mean, na.rm = TRUE)
#writeRaster(elevr_agg, "D:/Luisa/covariates/elevation/SRTM30m_NAs_filled_SVN_mask_int2s_200m.tif")
#elevr_agg <- raster("D:/Luisa/covariates/elevation/SRTM30m_NAs_filled_SVN_mask_int2s_200m.tif")
elevr_agg <- raster("D:/Luisa/covariates/elevation/SRTM30m_NAs_filled_SVN_mask_int2s_200m_utm_qgis.tif")
#elevr_agg <- aggregate(elevr_agg, fact = (1000/200), fun = mean, na.rm = TRUE)

# Scale elevation raster
elevationraster_scaled <- (elevr_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "elevation"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "elevation"]



### Habitat ======================================================================================================

# Import LC probability raster
#LCprob <- raster::stack("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/RF_prob_td2.8_wsmp10000_spl0.7_scale10-pred100.tif")
#Hab3raster <- LCprob$Semidry.Dry.Forest

# Clip to extent of aoi
#Hab3raster <- raster::crop(Hab3raster, extent(aoi))

# Aggregate 100m LC raster to 200m resolution to match with other rasters
#Hab3r_agg <- aggregate(Hab3raster, fact = (200/100), fun = mean, na.rm = TRUE)
#writeRaster(Hab3r_agg, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/RF_prob_hab3_td2.8_wsmp10000_spl0.7_scale10-100_200.tif")
#Hab3r_agg <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/RF_prob_hab3_td2.8_wsmp10000_spl0.7_scale10-100_200.tif")




Hab3r_agg <- raster::stack("D:/Luisa/Rsuperclass/SC_prob_10m/pred200_SC10_SVN_td2.8_10000.tif")
Hab3r_agg <- Hab3r_agg$pred200_SC10_SVN_td2.8_10000.3

# Hab3r_agg <- raster::stack("D:/Luisa/Rsuperclass/SC_prob_200m/pred_SC200_SVN_td2.8_80.tif")
# Hab3r_agg <- Hab3r_agg$pred_SC200_SVN_td2.8_80.3

# Hab3r_agg <- raster::stack("D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_pred200m.tif")
# Hab3r_agg <- Hab3r_agg$pred_SC_SVN_td2.8_50000_pred200m.3

# Hab3r_agg <- raster::stack("D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_sdf_pred200m.tif")
# Hab3r_agg <- Hab3r_agg$pred_SC_SVN_td2.8_50000_sdf_pred200m.3

#Hab3r_agg <- raster::stack("D:/Luisa/Rsuperclass/SC_prob_50000/pred_SC_SVN_td2.8_50000_comb23_pred200m.tif")
#Hab3r_agg <- Hab3r_agg$pred_SC_SVN_td2.8_50000_comb23_pred200m.2


#Hab3r_agg <- aggregate(Hab3r_agg, fact = (1000/200), fun = mean, na.rm = TRUE)

# Mask raster with studyarea
#Hab3maskp <- raster::mask(x=Hab3r_agg, mask=aoi)

# Scale Hab3 raster
Hab3raster_scaled <- (Hab3r_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "habitat"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "habitat"]



### Remoteness ======================================================================================================

# Import remoteness raster
#remraster <- raster("D:/Luisa/covariates/remoteness/cumCostraster_h_SVN_osm05052022_10kmbuffer_excl_footpath.tif")

# Aggregate 30m remoteness raster to 200m resolution to match with other rasters
#remr_agg <- aggregate(remraster, fact = (200/30), fun = mean, na.rm = TRUE)
#writeRaster(remr_agg, "D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Accessibility/cumCostraster_h_SVN_osm05052022_10kmbuffer_excl_footpath_200.tif")
remr_agg <- raster("D:/Luisa/covariates/remoteness/cumCostraster_h_SVN_osm05052022_10kmbuffer_excl_footpath_200_utm_qgis.tif")
#remr_agg <- aggregate(remr_agg, fact = (1000/200), fun = mean, na.rm = TRUE)

# Scale elevation raster
remraster_scaled <- (remr_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "remoteness"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "remoteness"]


# attr(covariates_scaled_matrix, "scaled:center")
# attr(covariates_scaled_matrix2, "scaled:center")
# attr(covariates_scaled_matrix, "scaled:scale")
# attr(covariates_scaled_matrix2, "scaled:scale")
# 
# head(covariates_scaled_matrix)
# head(covariates_scaled_matrix2)


### Distance Coast ======================================================================================================

# Import remoteness raster
#distraster <- raster("D:/Luisa/covariates/Distance_coast/SVN_proximity_utm49n_in_meter.tif")

# Aggregate 30m remoteness raster to 200m resolution to match with other rasters
#dist_agg <- aggregate(distraster, fact = (200/30), fun = mean, na.rm = TRUE)
#writeRaster(dist_agg, "D:/Luisa/covariates/Distance_coast/SVN_proximity_utm49n_in_meter_aggr200.tif")
dist_agg <- raster("D:/Luisa/covariates/Distance_coast/SVN_proximity_utm49n_in_meter_aggr200.tif")
#remr_agg <- aggregate(remr_agg, fact = (1000/200), fun = mean, na.rm = TRUE)

# Scale elevation raster
distraster_scaled <- (dist_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "distance"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "distance"]


# attr(covariates_scaled_matrix, "scaled:center")
# attr(covariates_scaled_matrix2, "scaled:center")
# attr(covariates_scaled_matrix, "scaled:scale")
# attr(covariates_scaled_matrix2, "scaled:scale")
# 
# head(covariates_scaled_matrix)
# head(covariates_scaled_matrix2)



# 6.2 Create rasterstack ============================================================================

# rename scaled covariate rasters
elevr <- elevationraster_scaled
hab3r <- Hab3raster_scaled
#remr <- remraster_scaled
distr <- distraster_scaled

# resample rasters for raster stack
#elevr <- resample(elevr, villdensr)
elevr
hab3r <- resample(hab3r, elevr)
#remr <- resample(remr, elevr)
distr <- resample(distr, elevr)

# stack rasters
predstack <- raster::stack(elevr, hab3r)#, distr)
names(predstack) <- c("scale.elevation", "scale.habitat")#, "scale.distance")#, "scale.evelvation_sq", "scale.habitat_sq", "scale.remoteness_sq")


# 6.3 Make predictions ---------------------------------------------------------------------------------------

# --> check out SBC bestmodels:     sfd     5,3,6,7,1,4,2
modsel_df_round


# ## --> predict_manual not applicable to multiple covariate models! 
# # Set path to sourcecode to calculate predictions (single covariate models)
# source(file.path("D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/Screenforbio_Dopbox_backup/scripts/predict_manual.R"))
# prediction_o.mod5 <- lapply(o.mod5, FUN = predict_manual, covariate_raster_stack = predstack)
# prediction_o.mod14 <- lapply(o.mod14, FUN = predict_manual, covariate_raster_stack = predstack)
# prediction_o.mod12 <- lapply(o.mod12, FUN = predict_manual, covariate_raster_stack = predstack)


# Either use predict ...
start_time <- Sys.time()
prediction_o.mod4 <- predict(o.mod4, newdata  = predstack, type = "state")
end_time <- Sys.time()
end_time - start_time
# --> Time difference 1000 scale of 47.91 secs
# --> Time difference 200 scale of 10.1 mins / 8.03 mins / 9.8 mins
writeRaster(prediction_o.mod4, "D:/Luisa/Rsuperclass/SC_prob_10m/SBC_predictions_200m/SVN_SBC_pred200_CTall_ehh2_1_mod4_eh.tif")



# prediction_o.mod7 <- predict(o.mod7, newdata  = predstack, type = "state")
# prediction_o.mod4 <- predict(o.mod4, newdata  = predstack, type = "state")
# prediction_o.mod5 <- predict(o.mod5, newdata  = predstack, type = "state")
# prediction_o.mod3 <- predict(o.mod3, newdata  = predstack, type = "state")
# prediction_o.mod1 <- predict(o.mod1, newdata  = predstack, type = "state")
# prediction_o.mod2 <- predict(o.mod2, newdata  = predstack, type = "state")





# # normal 10m utm bestmodels 5-3-7-6-1-4-2
# writeRaster(prediction_o.mod5, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_10m_utm/SVN_SBC_pred10_1_mod5_er.tif")
# writeRaster(prediction_o.mod3, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_10m_utm/SVN_SBC_pred10_2_mod3_r.tif")
# writeRaster(prediction_o.mod7, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_10m_utm/SVN_SBC_pred10_3_mod7_ehr.tif")
# writeRaster(prediction_o.mod6, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_10m_utm/SVN_SBC_pred10_4_mod6_hr.tif")
# writeRaster(prediction_o.mod1, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_10m_utm/SVN_SBC_pred10_5_mod1_e.tif")
# writeRaster(prediction_o.mod4, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_10m_utm/SVN_SBC_pred10_6_mod4_eh.tif")
# writeRaster(prediction_o.mod2, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_10m_utm/SVN_SBC_pred10_7_mod2_h.tif")

# # normal 200m utm bestmodels 5-3-7-6-1-4-2
# writeRaster(prediction_o.mod5, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_200m_utm/SVN_SBC_pred200_1_mod5_er.tif")
# writeRaster(prediction_o.mod3, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_200m_utm/SVN_SBC_pred200_2_mod3_r.tif")
# writeRaster(prediction_o.mod7, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_200m_utm/SVN_SBC_pred200_3_mod7_ehr.tif")
# writeRaster(prediction_o.mod6, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_200m_utm/SVN_SBC_pred200_4_mod6_hr.tif")
# writeRaster(prediction_o.mod1, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_200m_utm/SVN_SBC_pred200_5_mod1_e.tif")
# writeRaster(prediction_o.mod4, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_200m_utm/SVN_SBC_pred200_6_mod4_eh.tif")
# writeRaster(prediction_o.mod2, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_normal_200m_utm/SVN_SBC_pred200_7_mod2_h.tif")

# # sfd 10m utm bestmodels 6-5-3-7-4-1-2
# terra::writeRaster(prediction_o.mod6$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_10m/SVN_SBC_pred10_1_mod6_hr.tif")
# terra::writeRaster(prediction_o.mod5$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_10m/SVN_SBC_pred10_2_mod5_er.tif")
# terra::writeRaster(prediction_o.mod3$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_10m/SVN_SBC_pred10_3_mod3_r.tif")
# terra::writeRaster(prediction_o.mod7$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_10m/SVN_SBC_pred10_4_mod7_ehr.tif")
# terra::writeRaster(prediction_o.mod4$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_10m/SVN_SBC_pred10_5_mod4_eh.tif")
# terra::writeRaster(prediction_o.mod1$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_10m/SVN_SBC_pred10_6_mod1_e.tif")
# terra::writeRaster(prediction_o.mod2$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_10m/SVN_SBC_pred10_7_mod2_h.tif")

# # sfd 200m utm bestmodels 5-3-6-7-4-1-2
# writeRaster(prediction_o.mod5, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_200m_utm/SVN_SBC_pred200_1_mod5_er.tif")
# writeRaster(prediction_o.mod3, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_200m_utm/SVN_SBC_pred200_2_mod3_r.tif")
# writeRaster(prediction_o.mod6, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_200m_utm/SVN_SBC_pred200_3_mod6_hr.tif")
# writeRaster(prediction_o.mod7, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_200m_utm/SVN_SBC_pred200_4_mod7_ehr.tif")
# writeRaster(prediction_o.mod4, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_200m_utm/SVN_SBC_pred200_5_mod4_eh.tif")
# writeRaster(prediction_o.mod1, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_200m_utm/SVN_SBC_pred200_6_mod1_e.tif")
# writeRaster(prediction_o.mod2, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_sfd_200m_utm/SVN_SBC_pred200_7_mod2_h.tif")

# # comb23 10m utm bestmodels 5-3-7-6-1-4-2
# terra::writeRaster(prediction_o.mod5, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_10m_utm/SVN_SBC_pred10_1_mod5_er_.tif")
# terra::writeRaster(prediction_o.mod3, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_10m_utm/SVN_SBC_pred10_2_mod3_r_.tif")
# terra::writeRaster(prediction_o.mod7, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_10m_utm/SVN_SBC_pred10_3_mod7_ehr_.tif")
# terra::writeRaster(prediction_o.mod6, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_10m_utm/SVN_SBC_pred10_4_mod6_hr_.tif")
# terra::writeRaster(prediction_o.mod1, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_10m_utm/SVN_SBC_pred10_5_mod1_e_.tif")
# terra::writeRaster(prediction_o.mod4, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_10m_utm/SVN_SBC_pred10_6_mod4_eh_.tif")
# terra::writeRaster(prediction_o.mod2, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_10m_utm/SVN_SBC_pred10_7_mod2_h_.tif")

# # comb23 200m utm bestmodels 5-3-7-6-1-4-2
# terra::writeRaster(prediction_o.mod5$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_200m_utm/SVN_SBC_pred200_1_mod5_er.tif")
# terra::writeRaster(prediction_o.mod3$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_200m_utm/SVN_SBC_pred200_2_mod3_r.tif")
# terra::writeRaster(prediction_o.mod7$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_200m_utm/SVN_SBC_pred200_3_mod7_ehr.tif")
# terra::writeRaster(prediction_o.mod6$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_200m_utm/SVN_SBC_pred200_4_mod6_hr.tif")
# terra::writeRaster(prediction_o.mod1$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_200m_utm/SVN_SBC_pred200_5_mod1_e.tif")
# terra::writeRaster(prediction_o.mod4$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_200m_utm/SVN_SBC_pred200_6_mod4_eh.tif")
# terra::writeRaster(prediction_o.mod2$Predicted, "D:/Luisa/Rsuperclass/SC_prob_50000/SBC_predictions_200m/SBC_comb23_200m_utm/SVN_SBC_pred200_7_mod2_h.tif")








# ... or try to create data frame with rastervalues and make predictions out of this
vals <- as.data.frame(values(predstack))
vals_subset <- vals[which(!is.na(rowSums(vals))),]

start_time <- Sys.time()
tmp2 <- predict(o.mod5[[43]], type = "state", newdata = vals_subset)
#tmp2 <- lapply(o.mod5, FUN = predict, type = "state", newdata = vals_subset)
end_time <- Sys.time()
end_time - start_time



# prediction_o.mod5 <- lapply(o.mod5, FUN = predict, newdata  = predstack, type = "state")
# prediction_o.mod14 <- lapply(o.mod14, FUN = predict, newdata  = predstack, type = "state")
# prediction_o.mod12 <- lapply(o.mod12, FUN = predict, newdata  = predstack, type = "state")

plot(prediction_o.mod6$Predicted)
terra::writeRaster(prediction_o.mod6$Predicted, "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/SBC_pred_hab_rem_1000.tif")

# # Save as Rdata-file in working directory (getwd)
# save(#list=c(
#   "prediction_o.mod5",
#   #           "prediction_o.mod14",
#   #           "prediction_o.mod12"), 
#   #   #file = "D:/Dateien/Uni/Eagle_Master/Masterthesis/R_Masterthesis_SBC/testing/occupancy/SBC_bestmodels_prediction_1.9.Rdata")
#       file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/SBC_o.mod5_prediction_1.9_.Rdata")
#      
# # Load prediction data into global environment
# #load(file = file.path("D:/Dateien/Uni/Eagle_Master/Masterthesis/R_Masterthesis_SBC/testing/occupancy/SBC_bestmodels_prediction_1.9.Rdata"))
# load(file = file.path("C:/Users/s347553/Documents/Masterthesis/Data/occupancy/SBC_o.mod5_prediction_1.9_.Rdata"))




