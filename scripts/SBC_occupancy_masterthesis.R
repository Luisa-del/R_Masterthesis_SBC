#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# SINGLE SPECIES OCCUPANCY MODELING       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#     MASTER THESIS     LUISA PFLUMM      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(unmarked)
library(ubms)
library(raster)
library(rgeos)
library(lubridate)
library(gridExtra)
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
# library(remotes)
# library(R.rsp)
# remotes::install_github("jniedballa/camtrapR", build_vignettes = TRUE)
library(camtrapR)


path1 <- "C:/Users/s347553/Documents/Masterthesis/Data/CT"
#path <- "C:/Arbeit/Luisa/shareJN/CTdata"   

# # don't allow to show e numbers
options(scipen = 100, digits = 4)
                                                                                                        # occ15 !?!
#save.image(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_occupancy_allcovs_occ20.RData")
#save.image(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_occupancy_eh_occ15.RData")
#save.image(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_occupancy_eh_fromall_occ15.RData")
#save.image(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_occupancy_allcovs_occ20_sure.RData")
#save.image(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_occupancy_allcovs_occ15-20_sure.RData")
#save.image(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_occupancy_allcovs_occ20_mod1-9_200m_rasteragg_correlation_17.12.RData") # fail: I overwrote file with only path parameter

#save.image(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_occupancy_finalcovs_occ15_seed3.RData")
#save.image(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_occupancy_finalcovs_occ15_seed5.RData")
load(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_occupancy_finalcovs_occ15_seed5.RData")

# 1. CT data ================================================================================================================

# Import CT and record tables
recTable <- read.csv(file = file.path(path1, "Quy/corrected_recTable_Quy_editLP_shareJN.csv"))
CTtable <- read.csv2(file = file.path(path1, "CTfinal/CTtables_combined_editLP2.csv"), sep = ";")




# Correct species names
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("Blue rumped", "Blue-rumped", x)))
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("Orange headed", "Orange-headed", x)))
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("Pig tailed", "Pig-tailed", x)))
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("Small Indian", "Small indian", x)))
recTable$Species <- sapply(recTable$Species, function(x) as.character(gsub("White crested", "White-crested", x)))




# Warning: At 1 stations there were records before camera operation date range:  PY029 
recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-03 06:02:00" & 
                            recTable$station == "PY029"] <- "2021-04-03 06:02:00" 
recTable$Date[recTable$Date == "4/3/2020" & recTable$station == "PY029"] <- "4/3/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-15 18:06:00" & 
                            recTable$station == "PY029"] <- "2021-04-15 18:06:00" 
recTable$Date[recTable$Date == "4/15/2020" & recTable$station == "PY029"] <- "4/15/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-05-07 18:09:00" & 
                            recTable$station == "PY029"] <- "2021-05-07 18:09:00" 
recTable$Date[recTable$Date == "5/7/2020" & recTable$station == "PY029"] <- "5/7/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-05 11:59:00" & 
                            recTable$station == "PY029"] <- "2021-04-05 11:59:00" 
recTable$Date[recTable$Date == "4/5/2020" & recTable$station == "PY029"] <- "4/5/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-05-19 18:00:00" & 
                            recTable$station == "PY029"] <- "2021-05-19 18:00:00" 
recTable$Date[recTable$Date == "5/19/2020" & recTable$station == "PY029"] <- "5/19/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-08 11:44:00" & 
                            recTable$station == "PY029"] <- "2021-04-08 11:44:00" 
recTable$Date[recTable$Date == "4/8/2020" & recTable$station == "PY029"] <- "4/8/2021"

recTable$DateTimeOriginal[recTable$DateTimeOriginal == "2020-04-01 06:52:00" & 
                            recTable$station == "PY029"] <- "2021-04-01 06:52:00" 
recTable$Date[recTable$Date == "4/1/2020" & recTable$station == "PY029"] <- "4/1/2021"



# remove Cat Tien data
stat <- CTtable[CTtable$site %in% "Cat Tien",]$station
CTtable <- CTtable[!(CTtable$site %in% "Cat Tien"),]
CTtable <- CTtable[c(2:11, 20)]



# Error in cameraOperation(CTtable = CTtable, stationCol = "station", cameraCol = "camera",  : 
#  Negative effort calculated in ctp33d04__CAM_ct04 on: 2013-09-17, 2013-10-07, 2013-10-08, 2013-10-09 
#  Check for overlapping dates in Problem columns
# ---> I changed manually in csv file!! Check back with An/Quy
# after checking back with An: remove problem 4

# CTtable$Problem4_from[CTtable$Problem4_from == "25.08.2013" & CTtable$station == "ctp33d03"] <- NA
# CTtable$Problem4_to[CTtable$Problem4_to == "16.09.2013" & CTtable$station == "ctp33d03"] <- NA
# CTtable$Problem4_from[CTtable$Problem4_from == "17.10.2013" & CTtable$station == "ctp33d04"] <- NA
# CTtable$Problem4_to[CTtable$Problem4_to == "04.11.2013" & CTtable$station == "ctp33d04"] <- NA





# save(CTtable, file = "C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/final_CTtable_corrected_editLP_28.11.RData")
# save(recTable, file = "C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/final_recTable_corrected_editLP_28.11.RData")



# 2. Covariates =============================================================================================================

# > site covs ------------------------------------------------------------------------------------------------------------------------

# # import elevation
# e <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_elev_CTall_editLP2_utm.csv")
# 
# # import semidry/dry forest probability layer
# h <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_hab10-10_10000_CTall_editLP2_utm_frombigtiff.csv")
# #h <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_hab10-10_10000_CTall_utm.csv")
# # h <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_hab10-100_10000_CTall_utm.csv")
# # h[167,2] <- 0.106   #-> in this case station ncs003 has no value because point to close at rim of mask, aggregated values --> 0
# 
# # import distance (units in pixel (S2 mask))
# d <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_dist_px_CTall_editLP2_utm.csv")
# 
# # # import remoteness
# r <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_rem_CTall_editLP2_utm.csv")

# # import other class probability layer
# ds <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_hab10-10_10000_CTall_utm_class2.csv")
# ef <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_hab10-10_10000_CTall_utm_class1.csv")
# gl <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_hab10-10_10000_CTall_utm_class8.csv")




all <- read.csv2("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/elev_vegclasses_rem_dist_CTall.csv", sep = ";")

rownames(all) <- all$X
covariates <- all[c(1:5,8:9)]

# change remoteness values (previous has wrong values)
rem_new <- read.csv2("C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/cov_rem_CTall_editLP2_GEEutm.csv")
covariates <- covariates[1:6]
covariates$remoteness <- rem_new$remoteness

covariates <- covariates[!(covariates$X %in% stat),]
covariates <- covariates[2:7]

#>>>>>>>>>>>>>>>>>>>>>>>>> -------------------------------------------------------------------------
# COMBINE COVARIATES

# covariates <- cbind(e[2],h[2])
# names(covariates) <- c("elevation", "habitat")

# covariates <- cbind(e[2],h[2],d[2])
# names(covariates) <- c("elevation", "habitat", "distance")

# covariates <- cbind(e[2],h[2],r[2])
# names(covariates) <- c("elevation", "habitat", "remoteness")

# covariates <- cbind(e[2],h[2],r[2],d[2])
# names(covariates) <- c("elevation", "habitat", "remoteness", "distance")



# # remove cat tien data!
# covariates_all <- cbind(e[1:2],h[2])
# names(covariates_all) <- c("station","elevation", "habitat")
# covariates <- covariates_all[!(covariates_all$station %in% stat),]
# covariates <- covariates[2:3]

#>>>>>>>>>>>>>>>>>>>>>>>>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>------
# # optional: select only SBC sites
# load(file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/SBC_sites_only.RData")
# head(recTable_SBC)
# head(CTtable_SBC)
# recTable <- recTable_SBC
# CTtable <- CTtable_SBC
# covariates <- cbind(e[1:2],h[2])
# names(covariates) <- c("station","elevation", "habitat")
# covariates <- covariates[covariates$station %in% stat_sbc,]
# covariates <- covariates[2:3]
# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>------

# #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# # optional with all data, remove cat tien data!
# covariates_all <- all
# covariates <- covariates_all[!(covariates_all$X %in% stat),]
# rownames(covariates) <- covariates$X
# covariates <- covariates[-1]
# #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


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



# # add columns with squared covariate
# covariates <- cbind(covariates,
#                     scale.elevation_sq = covariates$scale.elevation^2)
# datatable(round(covariates,2),extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))
# dim(covariates)





#> create obs covs -------------------------------------------------------------------------------------------------

# aggregate stations
stations <- st_as_sf(CTtable, coords = c("long", "lat"),crs = 4326)

stations_aggr <- aggregate(st_coordinates(stations),
                           by = list(stations$station),
                           mean)

# stations_aggr2 <- aggregate(st_coordinates(stations),
#                             by = list(stations$station, stations$site),
#                             mean)
# --> not in right order!

# # add site name
# # -> needs automatization!
# stations_aggr$site <- NA
# stations_aggr$site[1:23] <- "Deo Ca "
# stations_aggr$site[c(24:38,54)] <- "Suoi Tien"
# stations_aggr$site[c(39:53,55)] <- "Cam An Bac"
# stations_aggr$site[56:79] <- "Hon Heo"
# stations_aggr$site[c(80:125, 333:371)] <- "Nui Chua NP"
# stations_aggr$site[126:165] <- "Krong Trai"
# stations_aggr$site[166:190] <- "Song Hinh"
# stations_aggr$site[191:220] <- "So Pai"
# stations_aggr$site[221:332] <- "Bidoup"
# stations_aggr$site[372:435] <- "Phuoc Binh"

# # add survey name
# stations_aggr$survey[1:23] <- "Deo Ca "
# stations_aggr$survey[c(24:38,54)] <- "Suoi Tien"
# stations_aggr$survey[c(39:53,55)] <- "Cam An Bac"
# stations_aggr$survey[56:79] <- "Hon Heo"
# #!! stations_aggr$survey[c(80:125, 333:371)] <- "Nui Chua NP"
# stations_aggr$survey[126:165] <- "Krong Trai"
# stations_aggr$survey[166:190] <- "Song Hinh"
# stations_aggr$survey[191:220] <- "So Pai"
# stations_aggr$survey[221:332] <- "Bidoup"
# stations_aggr$survey[372:435] <- "Phuoc Binh"


# create sf
# stations_aggr <- st_as_sf(stations_aggr, coords = c("X", "Y"), crs = st_crs(4326))
# colnames(stations_aggr)[1:2] <- c("station", "site")

stations_aggr <- st_read("C:/Users/s347553/Documents/Masterthesis/Data/CT/shps/stations_agg_site.shp")

# add nearest distance
stations_aggr$near_id <- st_nearest_feature(stations_aggr)
stations_aggr$near_dist <- st_distance(stations_aggr, stations_aggr[dist,], by_element = TRUE)

# set station as rownames
rownames(stations_aggr) <- stations_aggr$Group.1

# transform to utm
stations_aggr_utm = st_transform(stations_aggr, crs = st_crs(32649))





# # factor data ----> doesn't work!
# # solution: eschange categorical with numeric values
# stations_aggr_utm$site[stations_aggr_utm$site == "So Pai"] <- 1
# stations_aggr_utm$site[stations_aggr_utm$site == "Krong Trai"] <- 2
# stations_aggr_utm$site[stations_aggr_utm$site == "Deo Ca "] <- 3
# stations_aggr_utm$site[stations_aggr_utm$site == "Song Hinh"] <- 4
# stations_aggr_utm$site[stations_aggr_utm$site == "Hon Heo"] <- 5
# stations_aggr_utm$site[stations_aggr_utm$site == "Suoi Tien"] <- 6
# stations_aggr_utm$site[stations_aggr_utm$site == "Cam An Bac"] <- 7
# stations_aggr_utm$site[stations_aggr_utm$site == "Nui Chua NP"] <- 8
# stations_aggr_utm$site[stations_aggr_utm$site == "Bidoup"] <- 9
# stations_aggr_utm$site[stations_aggr_utm$site == "Phuoc Binh"] <- 10

# drop geometry
obs <- stations_aggr_utm %>% st_drop_geometry()





# 3. Collinearity test ================================================================================================

#> Covariates with <-70% or >70% collinearity are considered as highly correlated and they are excluded from the analysis. 
#> There are several statistical tests than estimate collinearity between covariates, such as Pearson collinearity test 
#> and VIF multicollinearity test.
#> 
#> You can calculate variance inflation factor (VIF) for a set of variables and exclude the highly correlated 
#> variables from the set through a stepwise procedure. 
#> This method can be used to deal with multicollinearity problems when you fit statistical models


# Compute a correlation matrix
# cc <- covariates %>% select( -matches("scale."))
# cc <- cc[,c(2:5,1,6)]
# names(cc) <- names(vals2)
# (corr_ct <- round(cor(cc),2))
(corr_ct <- round(cor(covariates %>% select( -matches("scale."))), 2))

# create corrplot for manyand export as pdf or image
ggcorrplot <- ggcorrplot(corr_ct,                             # visualize the correlation-matrix
                         hc.order = FALSE,                  # use hierarchical clustering
                         type = "lower",                    # alternative: upper, full
                         lab = TRUE,                        # add correlation coefficients, change size
                         ggtheme = ggplot2::theme_grey(),   # alternative: ggplot2::theme_minimal, ggplot2::theme_dark
                         title = "Correlation of Covariates"
                         # p.mat = p.mat,                   # Barring the no significant coefficient
) +
  theme(plot.title = element_text(size=22))                  # add other relevant theme and guides methods (still ggplot2 object)

#png(filename = "C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/CT_correlation.png")
plot(ggcorrplot)
#dev.off()

(vif <- vifcor(covariates %>% select( -matches("scale.")), th=0.7))



#>>>>>>>>>>>>>>>>>>>>>>>>> -------------------------------------------------------------------------
# REMOVE COVARIATE?

#covariates <- covariates %>% select( -matches("remoteness"))
#>>>>>>>>>>>>>>>>>>>>>>>>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -




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
                            CTDateFormat         = "dmy",
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "ymd HMS",#"%Y-%m-%d %H:%M:%S",
                            #CTHasProblems        = TRUE,
                           # sinkpath = "C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal",
                            makezip              = F)                  # set TRUE if you want Zip-file



# Check list elements of survey report
datatable(reportTest[[1]],extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))    # camera trap operation times and image date ranges
datatable(reportTest[[2]],extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))    # number of species by station
datatable(reportTest[[3]],extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))    # number of events and number of stations by species
datatable(reportTest[[4]],extensions = 'FixedColumns',options = list(scrollX = TRUE,scrollCollapse = TRUE))    # number of species events by station

ct_param <- data.frame(reportTest[[1]])

d <- reportTest[[1]]
hist(d$n_calendar_days_active, breaks = 100, xlab = "active calendar days", xlim=c(0,250), main = "Histogram of active calendar days per CT station")
sd(d$n_calendar_days_active)


hist(stations_aggr$near_dist, breaks = 100, xlab = "distance to nearest station", main = "Histogram of distance to nearest CT station")
sd(stations_aggr$near_dist)



## 4.3 Select relevant species ==============================================================================================

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

myspecies <- "Silver-backed chevrotain" #relevantspecies[43]



## 4.4 Species activity&presence ======================================================================================

### Activity plot ###

activityDensity(recordTable = recTable,
                species     = myspecies)   #relevantspecies$species[43]


### Species presence ###

# Define shapefile parameters
aoi <- shapefile("C:/Users/s347553/Documents/Masterthesis/Data/SVN_SBC_provinces_final.shp")

# Plot
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
                 # shapefileName       = "SBC_occ_detections_editLP2", #shapefileName,
                 # shapefileDirectory  = file.path(path1, "shps"), #pathout_shapefiles,
                 # shapefileProjection = "+proj=longlat +datum=WGS84 +no_defs" #shapefileProjection)
  )




  
  
# 5. Occupancy modeling ====================================================================================================
#load(file = "C:/Users/s347553/Desktop/SBC_occupancy_chapter1-51.RData")

## 5.1 Camera operation ====================================================================================================

#--> see 4.1 
camop_matrix <- camop_matrix





## 5.2 Detection history&effort ====================================================================================================

# Define timezone and occasion length.
timeZone <- "Asia/Ho_Chi_Minh" # change the time zone based on your study area
occasionLength <- 15 # change the occasion length based on your study


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

# Remove NAs from Detection history ---> EDIT JN
keep_these <- which(apply(detHist[[1]], 1, FUN = function(x) !all(is.na(x))))
covariates_subset <- covariates[keep_these,]
# covariates_scaled_matrix <- covariates_scaled_matrix[keep_these,]


# define detection non-detection matrix
y <- as.matrix(detHist[[1]])[keep_these, ]

# adapt site as covariate 
obs <- obs[keep_these,]
obs <- obs[2]

# define site covariates
sitecovs <- covariates_subset
  #sitecovs <- cbind(covariates_subset, obs)
  #sitecovs$site <- as.factor(sitecovs$site)


# define observation covariates
# only effort
obscovs <- list(Effort=as.matrix(detHist[[2]])[keep_these,])

# # effort + site
#   site <- data.frame(matrix(obs$site, 
#                             nrow = nrow(detHist[[2]][keep_these, ]), ncol = ncol(detHist[[2]][keep_these, ])))
#   for (i in 1:ncol(site)){
#     site[,i] <- as.factor(site[,i])
#   }
# 
#   obscovs <- list(Effort = as.matrix(detHist[[2]])[keep_these,],
#                   Site = site)


# create unmarked frame
umf <- unmarkedFrameOccu(y        = y, #as.matrix(detHist[[1]])[keep_these, ],
                         siteCovs = sitecovs, #covariates_subset,  #covariates_sq,
                         obsCovs  = obscovs) #list(Effort=as.matrix(detHist[[2]])[keep_these,]))

# umf <- unmarkedFrameOccu(y        = as.matrix(detHist[[1]]),
#                         siteCovs = covariates,
#                         obsCovs  = list(Effort=as.matrix(detHist[[2]])))





  
## 5.4 Detection models ====================================================================================================

# ubms ______________________________________________________________________________________________________________

  # Define null model
  set.seed(5)
  d.mod00_ubms <- stan_occu(~1 ~1, data = umf)           # no covariates
  
  # Define null modeleoth effort as covariate
  set.seed(5)
  d.mod0_ubms <- stan_occu(~ Effort ~ 1, data = umf)      # effort as covariate
  # Warning messages:
  # Covariates possibly not standardized (max value = 30).
  # Standardizing covariates is highly recommended. 

  # d.mod_s_ubms <- stan_occu(~ Site ~ 1, data = umf)      # site as covariate
  # d.mod_es_ubms <- stan_occu(~ Effort + Site ~ 1, data = umf)      # site and effort as covariate
  
  
  
  # Create list for modelselection
  modellist_detection_ubms <- fitList("p(.) psi(.) data" = d.mod00_ubms,
                                      "p(Effort) psi(.) data"  = d.mod0_ubms
                                      # ,
                                      # "p(Site) psi(.) data" = d.mod_s_ubms,
                                      # "p(Effort + Site) psi(.) data" = d.mod_es_ubms
                                      )
  
  
  # Modelselection
  set.seed(5)
  d_modsel_ubms <- modSel(modellist_detection_ubms)
  
  # # --> manchmal wurde hier auch beim erstellen von d.mod0_ubms % Modelselection folgende Warnung angezeigt,
  # Warning message:
  # Some Pareto k diagnostic values are slightly high. See help('pareto-k-diagnostic') for details.
  
  
  
  # Select best detection model
  if (row.names(d_modsel_ubms[1,]) == "p(Effort) psi() data") {
    d.mod_ubms <- d.mod00_ubms
    d_ubms <- ~1
  } else {
    d.mod_ubms <- d.mod0_ubms
    d_ubms <- ~Effort
  }
  
  # # Select best detection model
  # if (row.names(d_modsel_ubms[1,]) == "p(.) psi(.) data") {
  #   d.mod_ubms <- d.mod00_ubms
  #   d_ubms <- ~1
  # } 
  # if (row.names(d_modsel_ubms[1,]) == "p(Effort) psi(.) data") {
  #   d.mod_ubms <- d.mod0_ubms
  #   d_ubms <- ~Effort
  # }
  # if (row.names(d_modsel_ubms[1,]) == "p(Site) psi(.) data") {
  #   d.mod_ubms <- d.mod_s_ubms
  #   d_ubms <- ~Site
  # }
  # if (row.names(d_modsel_ubms[1,]) == "p(Effort + Site) psi(.) data") {
  #   d.mod_ubms <- d.mod_es_ubms
  #   d_ubms <- ~Effort + Site
  # }
  

  d.mod_ubms
  
  
  # # Extracting individual parameters
  # summary(d.mod_ubms, "state")
  # summary(d.mod_ubms, "det")
  
 
  
    
# unmarked _________________________________________________________________________________________________
# # Warning message: 38 sites have been discarded because of missing data.
# # --> means those stations where whole detection history is NA, but no problem, they will be automatically ignored
# 
#     # Define null models
#   (d.mod00 <- occu(~1 ~1, data = umf))            # no covariates
#   (d.mod0 <- occu(~ Effort ~ 1, data = umf))      # effort as covariate
#   
#   # Create list for modelselection 
#   modellist_detection <- fitList(fits = list("p(.) psi(.) data" = d.mod00,
#                                              "p(Effort) psi(.) data"  = d.mod0))
#   
#   # Modelselection
#   (d_modsel <- modSel(modellist_detection))
#   
#   # Convert to df
#   d_modsel_df <- data.frame(model = d_modsel@Full$model,
#                                  covariate = gsub("~1 ~ ", "", d_modsel@Full$formula, ),
#                                  nPars = d_modsel@Full$nPars,
#                                  AIC=d_modsel@Full$AIC,
#                                  delta=d_modsel@Full$delta,
#                                  AICwt=d_modsel@Full$AICwt,
#                                  cumltvWt=d_modsel@Full$cumltvWt
#                                  #p.Effort=d_modsel@Full$`p(Effort)`,
#                                  #SEp.Effort=d_modsel@Full$`SEp(Effort)`,
#                                  #p.Int=d_modsel@Full$`p(Int)`,
#                                  #SEp.Int=d_modsel@Full$`SEp(Int)`,
#                                  #psi.Int=d_modsel@Full$`psi(Int)`,
#                                  #SEpsi.Int=d_modsel@Full$`SEpsi(Int)`
#   )
#   
#   # Round dataset
#   (d_modsel_df_round <- data.frame(d_modsel_df[, c(1,2)], round(d_modsel_df[, -c(1,2)], 3)))
#   
#   
#   # # Plot data
#   # DT::datatable(d_modsel_df_round)





## 5.5 Occupancy models ====================================================================================================

# Warning message: 38 sites have been discarded because of missing data.
# --> means those stations where whole detection history is NA, but no problem, they will be automatically ignored

# #>>>>>>>>>>>>>>>>>>>>>>>>> -------------------------------------------------------------------------
# # # scaled covariates have to exist as variables in R environment.
# scale.elevation <- covariates_subset$scale.elevation
# scale.semi.dry.forest <- covariates_subset$scale.semi.dry.forest
# #scale.elevation_sq <- covariates_subset$scale.elevation^2
# scale.remoteness <- covariates_subset$scale.remoteness
# #site <- sitecovs$site

# optional >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
scale.elevation <- covariates_subset$scale.elevation
scale.dry.shrubland <- covariates_subset$scale.dry.shrubland
scale.semi.dry.forest <- covariates_subset$scale.semi.dry.forest
#scale.cropland <- covariates_subset$scale.cropland
#scale.grassland <- covariates_subset$scale.grassland
scale.coniferous.forest <- covariates_subset$scale.coniferous.forest
scale.evergreen.forest <- covariates_subset$scale.evergreen.forest
scale.remoteness <- covariates_subset$scale.remoteness
#scale.distance <- covariates_subset$scale.distance
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



# ubms __________________________________________________________________________________________________________________

# # takes between 1 and 2 minutes, but already imported with RData file
# options(mc.cores = 3)
# o.mod1_ubms <- stan_occu(~ 1 ~ scale.elevation, data = umf)
# 
# o.mod2_ubms <- stan_occu(~ 1 ~ scale.semi.dry.forest, data = umf)
# 
# o.mod3_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.semi.dry.forest, data = umf)
# 
# o.mod4_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.semi.dry.forest + I(scale.elevation^2), data = umf)
# # Warning messages:
# # 1: Covariates possibly not standardized (max value = 6.54504881236174).
# # Standardizing covariates is highly recommended.
# # 2: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
# 
# o.mod5r_ubms <- stan_occu(~ 1 ~ scale.remoteness, data = umf)
# o.mod6r_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.remoteness, data = umf)
# o.mod7r_ubms <- stan_occu(~ 1 ~ scale.remoteness + scale.semi.dry.forest, data = umf)
# o.mod8r_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.semi.dry.forest + scale.remoteness, data = umf)
# o.mod9r_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.semi.dry.forest + scale.remoteness + I(scale.elevation^2), data = umf)
# 
# 
# o.mod5_ubms <- stan_occu(~ 1 ~ site + scale.elevation + scale.semi.dry.forest, data = umf)
# o.mod6_ubms <- stan_occu(~ 1 ~ site + scale.elevation + scale.semi.dry.forest + I(scale.elevation^2), data = umf)
# o.mod7_ubms <- stan_occu(~ 1 ~ site + scale.semi.dry.forest, data = umf)
# o.mod8_ubms <- stan_occu(~ 1 ~ site + scale.elevation, data = umf)
# o.mod9_ubms <- stan_occu(~ 1 ~ site, data = umf)


# optional >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
set.seed(5)
o.mod11_ubms <- stan_occu(~ Effort ~ scale.elevation, data = umf)
set.seed(5)
o.mod12_ubms <- stan_occu(~ Effort ~ scale.evergreen.forest, data = umf)
set.seed(5)
o.mod13_ubms <- stan_occu(~ Effort ~ scale.dry.shrubland, data = umf)
set.seed(5)
o.mod14_ubms <- stan_occu(~ Effort ~ scale.semi.dry.forest, data = umf)
#o.mod15_ubms <- stan_occu(~ Effort ~ scale.cropland, data = umf)
#o.mod16_ubms <- stan_occu(~ Effort ~ scale.grassland, data = umf)
set.seed(5)
o.mod17_ubms <- stan_occu(~ Effort ~ scale.coniferous.forest, data = umf)
set.seed(5)
o.mod18_ubms <- stan_occu(~ Effort ~ scale.remoteness, data = umf)
#o.mod19_ubms <- stan_occu(~ Effort ~ scale.distance, data = umf)

set.seed(5)
o.mod20_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.evergreen.forest, data = umf)
set.seed(5)
o.mod21_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.dry.shrubland, data = umf)
set.seed(5)
o.mod22_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.semi.dry.forest, data = umf)
#o.mod23_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.cropland, data = umf)
#o.mod24_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.grassland, data = umf)
set.seed(5)
o.mod25_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.coniferous.forest, data = umf)
set.seed(5)
o.mod26_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.remoteness, data = umf)
#o.mod27_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.distance, data = umf)

set.seed(5)
o.mod28_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.evergreen.forest + I(scale.elevation^2), data = umf)
set.seed(5)
o.mod29_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.dry.shrubland + I(scale.elevation^2), data = umf)
set.seed(5)
o.mod30_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.semi.dry.forest + I(scale.elevation^2), data = umf)
#o.mod31_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.cropland + I(scale.elevation^2), data = umf)
#o.mod32_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.grassland + I(scale.elevation^2), data = umf)
set.seed(5)
o.mod33_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.coniferous.forest + I(scale.elevation^2), data = umf)
set.seed(5)
o.mod34_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.remoteness + I(scale.elevation^2), data = umf)
#o.mod35_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.distance + I(scale.elevation^2), data = umf)

set.seed(5)
o.mod36_ubms <- stan_occu(~ Effort ~ scale.elevation + scale.semi.dry.forest + scale.remoteness, data = umf)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>





d.mod_ubms # ---> DOUBLECHECK ~Effort or ~1 for o.models & fitlist below


# # Create list for modelselection
# fms_ubms <-  #modellist_occupancy
#   fitList("p(.) psi(.) data" = d.mod_ubms,    #d.mod0_ubms
#           "p(.) psi(elevation) data" = o.mod1_ubms,
#           "p(.) psi(semi.dry.forest) data" = o.mod2_ubms,
#           "p(.) psi(elevation + semi.dry.forest) data" = o.mod3_ubms,
#           "p(.) psi(elevation + semi.dry.forest + elevation^2) data" = o.mod4_ubms
#           ,
#           "p(.) psi(remoteness) data" = o.mod5r_ubms,
#           "p(.) psi(remoteness + elevation) data" = o.mod6r_ubms,
#           "p(.) psi(remoteness + semi.dry.forest) data" = o.mod7r_ubms,
#           "p(.) psi(remoteness + elevation + semi.dry.forest) data" = o.mod8r_ubms,
#           "p(.) psi(remoteness + elevation + semi.dry.forest + elevation^2) data" = o.mod9r_ubms
#           # ,
#           # "p(.) psi(site) data" = o.mod9_ubms,
#           # "p(.) psi(site + semi.dry.forest) data" = o.mod7_ubms,
#           # "p(.) psi(site + elevation) data" = o.mod8_ubms,
#           # "p(.) psi(site + elevation + semi.dry.forest) data" = o.mod5_ubms,
#           # "p(.) psi(site + elevation + semi.dry.forest + elevation^2) data" = o.mod6_ubms
#   )

# optional >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Create list for modelselection
fms_ubms <-  #modellist_occupancy
  fitList("p(Effort) psi(.) data" = d.mod_ubms, #d.mod0_ubms
          "p(Effort) psi(elevation) data" = o.mod11_ubms,
          "p(Effort) psi(ef) data" = o.mod12_ubms,
          "p(Effort) psi(ds) data" = o.mod13_ubms,
          "p(Effort) psi(sdf) data" = o.mod14_ubms,
          #"p(Effort) psi(cr) data" = o.mod15_ubms,
          #"p(Effort) psi(gr) data" = o.mod16_ubms,
          "p(Effort) psi(cf) data" = o.mod17_ubms,
          "p(Effort) psi(remoteness) data" = o.mod18_ubms,
          #"p(Effort) psi(distance) data" = o.mod19_ubms,
          "p(Effort) psi(elevation + ef) data" = o.mod20_ubms,
          "p(Effort) psi(elevation + ds) data" = o.mod21_ubms,
          "p(Effort) psi(elevation + sdf) data" = o.mod22_ubms,
          #"p(Effort) psi(elevation + cr) data" = o.mod23_ubms,
          #"p(Effort) psi(elevation + gr) data" = o.mod24_ubms,
          "p(Effort) psi(elevation + cf) data" = o.mod25_ubms,      # df, 24
          "p(Effort) psi(elevation + remoteness) data" = o.mod26_ubms,
          #"p(Effort) psi(elevation + distance) data" = o.mod27_ubms,
          "p(Effort) psi(elevation + elevation^2 + ef) data" = o.mod28_ubms,
          "p(Effort) psi(elevation + elevation^2 + ds) data" = o.mod29_ubms,
          "p(Effort) psi(elevation + elevation^2 + sdf) data" = o.mod30_ubms,
          #"p(Effort) psi(elevation + elevation^2 + cr) data" = o.mod31_ubms,
          #"p(Effort) psi(elevation + elevation^2 + gr) data" = o.mod32_ubms,
          "p(Effort) psi(elevation + elevation^2 + cf) data" = o.mod33_ubms,
          "p(Effort) psi(elevation + elevation^2 + remoteness) data" = o.mod34_ubms,
          #"p(Effort) psi(elevation + elevation^2 + distance) data" = o.mod35_ubms,
          "p(Effort) psi(elevation + sdf + remoteness) data" = o.mod36_ubms
  )




# Modelselection
# --> im allgemeinen sagt man, dass wenn delta_elpd > se_delta_elpd, dann hat das bessere Modell signifikant mehr 
# --> support. Ist wieder mehr eine Faustregel, aber nützlich. 
# --> Wenn delta_elpd < se_delta_elpd, dann sind beide Modelle mehr oder weniger gleich gut (oder schlecht).♦
set.seed(5)
modsel_ubms <- modSel(fms_ubms)
round(modsel_ubms[2:5],3)

#write.csv2(round(modsel_ubms,3), "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_predictions/modsel_o.modd22_eh_rounded.csv")
# Warning message:
# Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.



# 97.5% und 2.5%, unteres und oberes Ende des 95% Konfidenzintervalls (bzw. Bayesian Credible Interval, BCI)
# es gibt keinen p-Wert. Aber man kann mit Hilfe dieses Kriteriums (beinhaltet das 95% BCI den Wert 0?) 
# eine binäre Entscheidung treffen ob ein parameter signifikant ist oder nicht 
# (mit einem alpha-level = Wahrscheinlichkeit für Fehler 1. Art = Wharscheinlichkeit dass diese Entscheidung 
# falsch positiv ist) von 5%)



# unmarked _________________________________________________________________________________________________________________

# # Define occupancy models
# #single linear effect
# (o.mod1 <- occu(~ 1 ~ scale.elevation, data = umf)) # --> NEEDS AUTOMATISATION!!
# (o.mod2 <- occu(~ 1 ~ scale.habitat, data = umf))   # --> can be ~Effort, can be ~1
# #(o.mod3 <- occu(~ Effort ~ scale.distance, data = umf)) 
# 
# #multiple linear effect
# (o.mod4 <- occu(~ 1 ~ scale.elevation + scale.habitat, data = umf)) 
# #(o.mod5 <- occu(~ Effort ~ scale.elevation + scale.distance, data = umf)) 
# #(o.mod6 <- occu(~ Effort ~ scale.habitat + scale.distance, data = umf)) 
# 
# # ----> Version 1
# (o.mod15 <- occu(~ 1 ~ scale.elevation + scale.habitat + scale.habitat_sq, data = umf)) 
# # ----> Version 2
# (o.mod15_v2 <- occu(~ 1 ~ scale.elevation + I(scale.elevation^2) + scale.habitat, data = umf))
# #(o.mod15 <- occu(~ 1 ~ scale.elevation + scale.habitat + scale.elevation_sq, data = umf)) 
# 
# 
# # multiple qudratic effects
# #(o.mod7 <- occu(~ Effort ~ scale.elevation + scale.habitat + scale.distance, data = umf)) 
# #(o.mod8 <- occu(~ Effort ~ scale.elevation + scale.habitat + scale.distance + scale.elevation_sq, data = umf)) 
# 
# # # add remoteness?
#  # (o.mod9 <- occu(~ Effort ~ scale.remoteness, data = umf)) 
#  # (o.mod10 <- occu(~ Effort ~ scale.remoteness + scale.distance, data = umf))
#  # (o.mod11 <- occu(~ Effort ~ scale.remoteness + scale.habitat, data = umf)) 
#  # (o.mod12 <- occu(~ Effort ~ scale.remoteness + scale.elevation, data = umf))
#  # (o.mod13 <- occu(~ Effort ~ scale.remoteness + scale.elevation + scale.habitat + scale.distance, data = umf)) 
#  # (o.mod14 <- occu(~ Effort ~ scale.remoteness + scale.elevation + scale.habitat + scale.distance, scale.elevation_sq, data = umf))
# 
# 
# # # Idea for automatisation
# # c <- as.character(o.mod1@formula)
# # c <- paste0(c[2:3], collapse = " ")
# # dm <- as.character(d.mod@formula)
# # dm <- paste0(dm[2:3], collapse = " ")
# 
#  
# 
# 
# 
# # Select best detection model
# if (d_modsel@Full$model[1] == "p(.) psi(.) data") {
#   d.mod <- d.mod00
#   d <- ~1
# } else {
#   d.mod <- d.mod0
#   d <- ~Effort
# }
# 
# d.mod
# 
# DOUBLECHECK ~Effort or ~1 for o.models & fitlist below
# DOUBLECHECK ~Effort or ~1 for o.models & fitlist below
# DOUBLECHECK ~Effort or ~1 for o.models & fitlist below
# DOUBLECHECK ~Effort or ~1 for o.models & fitlist below
# DOUBLECHECK ~Effort or ~1 for o.models & fitlist below
# 
#   
#   # Create list for modelselection
#   fms <-  #modellist_occupancy
#     fitList("p(.) psi(.) data" = d.mod,   #to do!: find best nullmodel for each species
#             "p(.) psi(elevation) data" = o.mod1,
#             "p(.) psi(habitat) data" = o.mod2,
#             #"p(.) psi(distance) data" = o.mod3,
#             "p(.) psi(elevation + habitat) data" = o.mod4
#             ,
#             "p(.) psi(elevation + habitat + elevation_sq) data" = o.mod15
#             ,
#             "p(.) psi(elevation + elevation^2 + habitat) data" = o.mod15_v2
#             
#             #,
#                 #"p(.) psi(elevation + distance) data" = o.mod5,
#             #"p(.) psi(habitat + distance) data" = o.mod6,
#             #"p(.) psi(elevation + habitat + distance) data" = o.mod7,
#             #"p(.) psi(elevation + habitat + distance + elevation_sq) data" = o.mod8
#             
#             #,
#              # "p(.) psi(remoteness) data" = o.mod9,
#              # "p(.) psi(remoteness + distance) data" = o.mod10,
#              # "p(.) psi(remoteness + habitat) data" = o.mod11,
#              # "p(.) psi(remoteness + elevation) data" = o.mod12,
#              # "p(.) psi(remoteness + elevation + habitat + distance) data" = o.mod13,
#              # "p(.) psi(remoteness + elevation + habitat + distance + elevation_sq) data" = o.mod14
#             )
# 
# #>>>>>>>>>>>>>>>>>>>>>>>>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   
#   
#   # Modelselection
#   (modsel <- modSel(fms))
#   
#   
#   # Convert to df
#   modsel_df <- data.frame(model = modsel@Full$model,
#                                covariate = gsub("~1 ~ ", "", modsel@Full$formula, ),
#                                nPars = modsel@Full$nPars,
#                                AIC=modsel@Full$AIC,
#                                delta=modsel@Full$delta,
#                                AICwt=modsel@Full$AICwt,
#                                cumltvWt=modsel@Full$cumltvWt
#                                #p.Effort=modsel@Full$`p(Effort)`,
#                                #SEp.Effort=modsel@Full$`SEp(Effort)`,
#                                #p.Int=modsel@Full$`p(Int)`,
#                                #SEp.Int=modsel@Full$`SEp(Int)`,
#                                #psi.Int=modsel@Full$`psi(Int)`,
#                                #SEpsi.Int=modsel@Full$`SEpsi(Int)`
#   )
#   
#   
#   # Round dataset
#   (modsel_df_round <- data.frame(modsel_df[, c(1,2)], round(modsel_df[, -c(1,2)], 3)))
#   
#   # Plot data
#   DT::datatable(modsel_df_round)
# 
# 
# 
# # modsel_df_round_SC_prob_eq10000_10_10 <- modsel_df_round[[43]]
# # write.csv(modsel_df_round_SC_prob_eq10000_10_100,
# #           "D:/Luisa/Rsuperclass/.../modsel_df_round_SC_prob_eq10000_10_100.csv")
# 
# 





## 5.6 Goodness of fit test ==============================================================================================

# ubms _______________________________________________________________________________________________________

# (gof_result4 <- gof(o.mod4_ubms, draws = 500))
# (gof_result3 <- gof(o.mod3_ubms, draws = 500))   # best model?
# (gof_result2 <- gof(o.mod2_ubms, draws = 500))
# (gof_result1 <- gof(o.mod1_ubms, draws = 500))
# 
# plot(gof_result3@samples)
# 
# 
# (gof_result1 <- gof(o.mod1_ubms, draws = 500))
# (gof_result2 <- gof(o.mod2_ubms, draws = 500))
# (gof_result3 <- gof(o.mod3_ubms, draws = 500))
# (gof_result4 <- gof(o.mod4_ubms, draws = 500))
# (gof_result5r <- gof(o.mod5r_ubms, draws = 500))
# (gof_result6r <- gof(o.mod6r_ubms, draws = 500))
# (gof_result7r <- gof(o.mod7r_ubms, draws = 500))
# (gof_result8r <- gof(o.mod8r_ubms, draws = 500))
# (gof_result9r <- gof(o.mod9r_ubms, draws = 500))
# 
# (gof_result5 <- gof(o.mod5_ubms, draws = 500))
# (gof_result6 <- gof(o.mod6_ubms, draws = 500))
# (gof_result7 <- gof(o.mod7_ubms, draws = 500))
# (gof_result8 <- gof(o.mod8_ubms, draws = 500))
# (gof_result9 <- gof(o.mod9_ubms, draws = 500))




set.seed(5)
(gof_result11 <- gof(o.mod11_ubms, draws = 500))
set.seed(5)
(gof_result12 <- gof(o.mod12_ubms, draws = 500))   # best model?
set.seed(5)
(gof_result13 <- gof(o.mod13_ubms, draws = 500))
set.seed(5)
(gof_result14 <- gof(o.mod14_ubms, draws = 500))
#(gof_result15 <- gof(o.mod15_ubms, draws = 500))
#(gof_result16 <- gof(o.mod16_ubms, draws = 500))
set.seed(5)
(gof_result17 <- gof(o.mod17_ubms, draws = 500))
set.seed(5)
(gof_result18 <- gof(o.mod18_ubms, draws = 500))
#(gof_result19 <- gof(o.mod19_ubms, draws = 500))
set.seed(5)
(gof_result20 <- gof(o.mod20_ubms, draws = 500))
set.seed(5)
(gof_result21 <- gof(o.mod21_ubms, draws = 500))
set.seed(5)
(gof_result22 <- gof(o.mod22_ubms, draws = 500))
#(gof_result23 <- gof(o.mod23_ubms, draws = 500))
#(gof_result24 <- gof(o.mod24_ubms, draws = 500))
set.seed(5)
(gof_result25 <- gof(o.mod25_ubms, draws = 500))
set.seed(5)
(gof_result26 <- gof(o.mod26_ubms, draws = 500))
#(gof_result27 <- gof(o.mod27_ubms, draws = 500))
set.seed(5)
(gof_result28 <- gof(o.mod28_ubms, draws = 500))
set.seed(5)
(gof_result29 <- gof(o.mod29_ubms, draws = 500))
set.seed(5)
(gof_result30 <- gof(o.mod30_ubms, draws = 500))
#(gof_result31 <- gof(o.mod31_ubms, draws = 500))
#(gof_result32 <- gof(o.mod32_ubms, draws = 500))
set.seed(5)
(gof_result33 <- gof(o.mod33_ubms, draws = 500))
set.seed(5)
(gof_result34 <- gof(o.mod34_ubms, draws = 500))
#(gof_result35 <- gof(o.mod35_ubms, draws = 500))

set.seed(5)
(gof_result36 <- gof(o.mod36_ubms, draws = 500))



# unmarked _______________________________________________________________________________________________________
#
# # see ?unmarked::parboot
# fm <- fms_unmarked@fits$`p(.) psi(elevation + habitat) data`
# #fm <- fms_unmarked@fits$`p(.) psi(elevation + habitat) data`
# 
# # Function returning three fit-statistics.
# fitstats <- function(fm) {
#   observed <- getY(fm@data)
#   expected <- fitted(fm)
#   resids <- residuals(fm)
#   sse <- sum(resids^2, na.rm = T)
#   chisq <- sum((observed - expected)^2 / expected, na.rm = T)
#   freeTuke <- sum((sqrt(observed) - sqrt(expected))^2, na.rm = T)
#   out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
#   return(out)
# }
# 
# (pb <- parboot(fm, fitstats, nsim=500 ))
# plot(pb, main="")
# 
# 
# hist(pb@t.star[,1], breaks = 50, main = names(pb@t0) [1])
# abline(v = pb@t0[1], col = "red")
# 
# hist(pb@t.star[,2], breaks = 100, xlim = c(0, 20000), main = names(pb@t0) [2])
# abline(v = pb@t0[2], col = "red")
# 
# hist(pb@t.star[,3], breaks = 50, main = names(pb@t0) [3])
# abline(v = pb@t0[3], col = "red")
# 
# # JN: looks ok, no lack of fit from what I can tell
# 
# 
# 
# # alternative, from unmarked overview vignette
# chisq <- function(fm) {
#   umf <- fm@data
#   y <- umf@y
#   y[y>1] <- 1
#   sr <- fm@sitesRemoved
#   if(length(sr)>0)
#     y <- y[-sr,,drop=FALSE]
#   fv <- fitted(fm, na.rm=TRUE)
#   y[is.na(fv)] <- NA
#   sum((y-fv)^2/(fv*(1-fv)), na.rm=TRUE)
# }
# 
# (pb2 <- parboot(fm, statistic=chisq, nsim=100, parallel=FALSE))









# ## 5.7 Response Curves ========================================================================================

# ubms ___________________________________________________________________________________________________________

### >>> 1. model 36 ------------------------------------------------------------------------------------

#### e ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.semi.dry.forest = 0,
             scale.remoteness = 0)     
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod36_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 1                                                  # change

# prepare rugs for plots
# rug1 = rug for stations with records, rug2 = for staitons without records
rug1 <- rug2 <- tmp
rug1[which(rowSums(o.mod36_ubms@data@y, na.rm=T) == 0),] <- NA   # remove values at stations without records
rug2[which(rowSums(o.mod36_ubms@data@y, na.rm=T) >= 1),] <- NA   # remove values at stations with records
tmp <- cbind(tmp, rug1 = rug1[,1], rug2 = rug2[,1])

# plot
p361_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + sdf* + remoteness)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation") +                                                #change!
  ylab("occupancy") +
  #geom_rug(mapping = aes(x = rug2), sides = "b", col = "black", alpha = 1/4) +   # stations with non-detection
  geom_rug(mapping = aes(x = rug1), sides = "t", col = "black", alpha = 1/4)     # stations with detections


#### sdf ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.semi.dry.forest = seq(min(scale.semi.dry.forest),max(scale.semi.dry.forest), length.out = length(scale.semi.dry.forest)),
             scale.remoteness = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <-               #change!
  (nd[,2] * attributes(scale(covariates_subset[,4]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,4]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod36_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# prepare rugs for plots
# rug1 = rug for stations with records, rug2 = for staitons without records
rug1 <- rug2 <- tmp
rug1[which(rowSums(o.mod36_ubms@data@y, na.rm=T) == 0),] <- NA   # remove values at stations without records
rug2[which(rowSums(o.mod36_ubms@data@y, na.rm=T) >= 1),] <- NA   # remove values at stations with records
tmp <- cbind(tmp, rug1 = rug1[,2], rug2 = rug2[,2])

# plot
p362_ubms <- ggplot(tmp, aes(x = scale.semi.dry.forest, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + sdf* + remoteness)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("semi-dry forest") +                                               #change!
  ylab("occupancy") +
  #geom_rug(mapping = aes(x = rug2), sides = "b", col = "black", alpha = 1/4) +   # stations with non-detection
  geom_rug(mapping = aes(x = rug1), sides = "t", col = "black", alpha = 1/4)     # stations with detections


#### r ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.semi.dry.forest = 0,
             scale.remoteness = seq(min(scale.remoteness), max(scale.remoteness), length.out = length(scale.remoteness)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,3] <-               #change!
  (nd[,3] * attributes(scale(covariates_subset[,6]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,6]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod36_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# prepare rugs for plots
# rug1 = rug for stations with records, rug2 = for staitons without records
rug1 <- rug2 <- tmp
rug1[which(rowSums(o.mod36_ubms@data@y, na.rm=T) == 0),] <- NA   # remove values at stations without records
rug2[which(rowSums(o.mod36_ubms@data@y, na.rm=T) >= 1),] <- NA   # remove values at stations with records
tmp <- cbind(tmp, rug1 = rug1[,3], rug2 = rug2[,3])


# plot
p363_ubms <- ggplot(tmp, aes(x = scale.remoteness, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + sdf* + remoteness)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("remoteness") +                                               #change!
  ylab("occupancy") +
  #geom_rug(mapping = aes(x = rug2), sides = "b", col = "black", alpha = 1/4) +   # stations with non-detection
  geom_rug(mapping = aes(x = rug1), sides = "t", col = "black", alpha = 1/4)     # stations with detections


grid.arrange(p361_ubms, p362_ubms, p363_ubms)

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/1.ele-sdf-rem_omod36.png',
#     width = 600, height = 400)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p361_ubms, p362_ubms, p363_ubms                     #change!
# )
# 
# dev.off()



### >>> 2. model 22 ------------------------------------------------------------------------------------

#### e ####
nd <- data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
                 scale.semi.dry.forest = 0)
#nd$scale.elevation_sq <- nd$scale.elevation^2
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <- (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod22_ubms,
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 2

# prepare rugs for plots
# rug1 = rug for stations with records, rug2 = for staitons without records
rug1 <- rug2 <- tmp
rug1[which(rowSums(o.mod22_ubms@data@y, na.rm=T) == 0),] <- NA   # remove values at stations without records
rug2[which(rowSums(o.mod22_ubms@data@y, na.rm=T) >= 1),] <- NA   # remove values at stations with records
tmp <- cbind(tmp, rug1 = rug1[,1], rug2 = rug2[,1])


# plot
p221_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + sdf*)")) +
  theme(plot.title = element_text(size=11)) +
  xlab("elevation") + 
  ylab("occupancy") +
  #geom_rug(mapping = aes(x = rug2), sides = "b", col = "black", alpha = 1/4) +   # stations with non-detection
  geom_rug(mapping = aes(x = rug1), sides = "t", col = "black", alpha = 1/4)     # stations with detections





#### sdf ####
nd <- data.frame(scale.elevation = 0,
                 scale.semi.dry.forest = seq(min(scale.semi.dry.forest),max(scale.semi.dry.forest), length.out = length(scale.semi.dry.forest)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <- (nd[,2] * attributes(scale(covariates_subset[,4]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,4]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod22_ubms,
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# prepare rugs for plots
# rug1 = rug for stations with records, rug2 = for staitons without records
rug1 <- rug2 <- tmp
rug1[which(rowSums(o.mod22_ubms@data@y, na.rm=T) == 0),] <- NA   # remove values at stations without records
rug2[which(rowSums(o.mod22_ubms@data@y, na.rm=T) >= 1),] <- NA   # remove values at stations with records
tmp <- cbind(tmp, rug1 = rug1[,2], rug2 = rug2[,2])


# plot
p222_ubms <- ggplot(tmp, aes(x = scale.semi.dry.forest, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + sdf*)")) +
  theme(plot.title = element_text(size=11)) +
  xlab("semi-dry forest") + 
  ylab("occupancy") +
  #geom_rug(mapping = aes(x = rug2), sides = "b", col = "black", alpha = 1/4) +   # stations with non-detection
  geom_rug(mapping = aes(x = rug1), sides = "t", col = "black", alpha = 1/4)     # stations with detections




grid.arrange(p221_ubms, p222_ubms)       #change!

# png('C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/2.ele-sdf_omod22_rug.png',
#     # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
#     # width = 11.7, height = 8.3)
#     width = 600, height = 200)
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p221_ubms, p222_ubms
# )
# 
# dev.off()


### >>> 3. model 30 ------------------------------------------------------------------------------------

#### e^2 ####
nd <- data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
                 scale.semi.dry.forest = 0)
#nd$scale.elevation_sq <- nd$scale.elevation^2
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <- (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod30_ubms,
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 3

# plot
p301_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + sdf*)")) +
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")


#### sdf ####
nd <- data.frame(scale.elevation = 0,
                 scale.semi.dry.forest = seq(min(scale.semi.dry.forest),max(scale.semi.dry.forest), length.out = length(scale.semi.dry.forest)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <- (nd[,2] * attributes(scale(covariates_subset[,4]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,4]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod30_ubms,
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p302_ubms <- ggplot(tmp, aes(x = scale.semi.dry.forest, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + sdf*)")) +
  theme(plot.title = element_text(size=11)) +
  xlab("semi-dry forest")



grid.arrange(p301_ubms, p302_ubms)       #change!
                      #           
# png('C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/3.ele^2-sdf_omod30.png',
#     # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
#     # width = 11.7, height = 8.3)
#     width = 600, height = 200)
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p301_ubms, p302_ubms
# )
# 
# dev.off()



### >>> 4. model 34 ------------------------------------------------------------------------------------

#### e^2 ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.remoteness = 0)     
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod34_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 4                                                  # change

# plot
p341_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + remoteness*)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")                                                 #change!



#### r ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.remoteness = seq(min(scale.remoteness), max(scale.remoteness), length.out = length(scale.remoteness)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <-               #change!
  (nd[,2] * attributes(scale(covariates_subset[,6]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,6]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod34_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p342_ubms <- ggplot(tmp, aes(x = scale.remoteness, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + remoteness*)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("remoteness")                                                #change!


grid.arrange(p341_ubms, p342_ubms)                     #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/4.ele^2-rem_omod34.png',
#     width = 600, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p341_ubms, p342_ubms                     #change!
# )
# 
# dev.off()




### >>> 5. model 26 ------------------------------------------------------------------------------------

#### e ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.remoteness = 0)     
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod26_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 5                                                  # change

# plot
p261_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + remoteness*)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")                                                 #change!



#### r ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.remoteness = seq(min(scale.remoteness), max(scale.remoteness), length.out = length(scale.remoteness)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <-               #change!
  (nd[,2] * attributes(scale(covariates_subset[,6]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,6]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod26_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p262_ubms <- ggplot(tmp, aes(x = scale.remoteness, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + remoteness*)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("remoteness")                                                #change!


grid.arrange(p261_ubms, p262_ubms)                     #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/5.ele-rem_omod26.png',
#     width = 600, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p261_ubms, p262_ubms                     #change!
# )
# 
# dev.off()




### >>> 6. model 28 ------------------------------------------------------------------------------------

#### e^2 ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.evergreen.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod28_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 6                                                  # change

# plot
p281_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + ef*)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")                                                 #change!


#### ef ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.evergreen.forest = seq(min(scale.evergreen.forest),max(scale.evergreen.forest), length.out = length(scale.evergreen.forest)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <-               #change!
  (nd[,2] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod28_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p282_ubms <- ggplot(tmp, aes(x = scale.evergreen.forest, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + ef*)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("evergreen forest")                                                #change!


grid.arrange(p281_ubms, p282_ubms)

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/6.ele^2-ef_omod28.png',
#     width = 600, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p281_ubms, p282_ubms                     #change!
# )
# 
# dev.off()



### >>> 7. model 20 ------------------------------------------------------------------------------------

#### e ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.evergreen.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod20_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 7                                                  # change

# plot
p201_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + ef)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")                                                 #change!


#### ef ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.evergreen.forest = seq(min(scale.evergreen.forest),max(scale.evergreen.forest), length.out = length(scale.evergreen.forest)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <-               #change!
  (nd[,2] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod20_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p202_ubms <- ggplot(tmp, aes(x = scale.evergreen.forest, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + ef)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("evergreen forest")                                                #change!


grid.arrange(p201_ubms, p202_ubms)

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/7.ele-ef_omod20.png',
#     width = 600, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p201_ubms, p202_ubms                     #change!
# )
# 
# dev.off()



### >>> 8. model 25 ------------------------------------------------------------------------------------

#### e ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.coniferous.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod25_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 8                                                  # change

# plot
p251_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + cf)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")                                                 #change!


#### cf ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.coniferous.forest = seq(min(scale.coniferous.forest),max(scale.coniferous.forest), length.out = length(scale.coniferous.forest)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <-               #change!
  (nd[,2] * attributes(scale(covariates_subset[,5]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,5]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod25_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p252_ubms <- ggplot(tmp, aes(x = scale.coniferous.forest, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + cf)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("coniferous forest")                                                #change!


grid.arrange(p251_ubms, p252_ubms)         #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/8.ele-cf_omod25.png',
#     width = 600, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p251_ubms, p252_ubms                     #change!
# )
# 
# dev.off()



### >>> 9. model 11 ------------------------------------------------------------------------------------

#### e ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.coniferous.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod11_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 9                                                  # change

# plot
p111_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation*)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")                                                 #change!





grid.arrange(p111_ubms)         #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/9.ele_omod11.png',
#     width = 300, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p111_ubms                     #change!
# )
# 
# dev.off()



### >>> 10. model 33 ------------------------------------------------------------------------------------

#### e^2 ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.coniferous.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod33_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 10                                                  # change

# plot
p331_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + cf)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")                                                 #change!


#### cf ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.coniferous.forest = seq(min(scale.coniferous.forest),max(scale.coniferous.forest), length.out = length(scale.coniferous.forest)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <-               #change!
  (nd[,2] * attributes(scale(covariates_subset[,5]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,5]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod33_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p332_ubms <- ggplot(tmp, aes(x = scale.coniferous.forest, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + cf)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("coniferous forest")                                                #change!


grid.arrange(p331_ubms, p332_ubms)         #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/10.ele^2-cf_omod33.png',
#     width = 600, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p331_ubms, p332_ubms                     #change!
# )
# 
# dev.off()



### >>> 11. model 21 ------------------------------------------------------------------------------------

#### e ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.dry.shrubland = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod21_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 11                                                  # change

# plot
p211_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + ds)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")                                                 #change!


#### ds ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.dry.shrubland = seq(min(scale.dry.shrubland),max(scale.dry.shrubland), length.out = length(scale.dry.shrubland)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <-               #change!
  (nd[,2] * attributes(scale(covariates_subset[,3]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,3]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod21_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p212_ubms <- ggplot(tmp, aes(x = scale.dry.shrubland, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation* + ds)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("dry shrubland")                                                #change!


grid.arrange(p211_ubms, p212_ubms)         #change!
 
# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/11.ele-ds_omod21.png',
#     width = 600, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p211_ubms, p212_ubms                     #change!
# )
# 
# dev.off()



### >>> 12. model 29 ------------------------------------------------------------------------------------

#### e^2 ####
nd <-                        #change!
  data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
             scale.dry.shrubland = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod29_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 12                                                  # change

# plot
p291_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + ds)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("elevation")                                                 #change!


#### ds ####
nd <-                     #change!
  data.frame(scale.elevation = 0,
             scale.dry.shrubland = seq(min(scale.dry.shrubland),max(scale.dry.shrubland), length.out = length(scale.dry.shrubland)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <-               #change!
  (nd[,2] * attributes(scale(covariates_subset[,3]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,3]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod29_ubms,        #change!
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p292_ubms <- ggplot(tmp, aes(x = scale.dry.shrubland, y = Predicted)) +  #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(elevation^2 + ds)")) +    #change!
  theme(plot.title = element_text(size=11)) +
  xlab("dry shrubland")                                                #change!


grid.arrange(p291_ubms, p292_ubms)         #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/12.ele^2-ds_omod29.png',
#     width = 600, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p291_ubms, p292_ubms                     #change!
# )
# 
# dev.off()




### >>> 13. model 18 ------------------------------------------------------------------------------------

#### r ####
nd <-                        #change!
  data.frame(scale.remoteness = seq(min(scale.remoteness),max(scale.remoteness), length.out = length(scale.remoteness)),
             scale.coniferous.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,6]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,6]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod18_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 13                                                 # change

# plot
p181_ubms <- ggplot(tmp, aes(x = scale.remoteness, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(remoteness*)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("remoteness")                                                 #change!



grid.arrange(p181_ubms)         #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/13.rem_omod18.png',
#   width = 300, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p181_ubms                     #change!
# )
# 
# dev.off()


### >>> 14. model 14 ------------------------------------------------------------------------------------

#### sdf ####
nd <-                        #change!
  data.frame(scale.semi.dry.forest = seq(min(scale.semi.dry.forest),max(scale.semi.dry.forest), length.out = length(scale.semi.dry.forest)),
             scale.coniferous.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,4]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,4]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod14_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 14                                                 # change

# plot
p141_ubms <- ggplot(tmp, aes(x = scale.semi.dry.forest, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(sdf*)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("semi-dry forest")                                                 #change!



grid.arrange(p141_ubms)         #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/14.rem_omod14.png',
#   width = 300, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p141_ubms                     #change!
# )
# 
# dev.off()


### >>> 15. model 12 ------------------------------------------------------------------------------------

#### ef ####
nd <-                        #change!
  data.frame(scale.evergreen.forest = seq(min(scale.evergreen.forest),max(scale.evergreen.forest), length.out = length(scale.evergreen.forest)),
             scale.coniferous.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod12_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 15                                                 # change

# plot
p121_ubms <- ggplot(tmp, aes(x = scale.evergreen.forest, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(ef*)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("evergreen forest")                                                 #change!



grid.arrange(p121_ubms)         #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/15.ef_omod12.png',
#   width = 300, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p121_ubms                     #change!
# )
# 
# dev.off()


### >>> 16. model 17 ------------------------------------------------------------------------------------

#### cf ####
nd <-                        #change!
  data.frame(scale.coniferous.forest = seq(min(scale.coniferous.forest),max(scale.coniferous.forest), length.out = length(scale.coniferous.forest)),
             scale.evergreen.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,5]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,5]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod17_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 16                                                # change

# plot
p171_ubms <- ggplot(tmp, aes(x = scale.coniferous.forest, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(cf*)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("coniferous forest")                                                 #change!



grid.arrange(p171_ubms)         #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/16.cf_omod17.png',
#   width = 300, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p171_ubms                     #change!
# )
# 
# dev.off()



### >>> 17. model 13 ------------------------------------------------------------------------------------

#### ds ####
nd <-                        #change!
  data.frame(scale.dry.shrubland = seq(min(scale.dry.shrubland),max(scale.dry.shrubland), length.out = length(scale.dry.shrubland)),
             scale.evergreen.forest = 0)
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <-                 #change!
  (nd[,1] * attributes(scale(covariates_subset[,3]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,3]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod13_ubms,              #change!
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- 17                                                # change

# plot
p131_ubms <- ggplot(tmp, aes(x = scale.dry.shrubland, y = Predicted)) +    #change!
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(Effort) psi(ds)")) +  #change!
  theme(plot.title = element_text(size=11)) +
  xlab("dry shrubland")                                                 #change!



grid.arrange(p131_ubms)         #change!

# png(                                                     #change!
#   'C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/psi_rug/17.ds_omod13.png',
#   width = 300, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   p131_ubms                     #change!
# )
# 
# dev.off()


####>>> detection prob plots --------
# png(                                                     #change!
#   "C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/final_occ15_seed5/detection/1.ds_omod36_det.png",
#   width = 300, height = 200)                         #change!
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   plot_marginal(o.mod36_ubms, submodel="det")                     #change!
#   )
# 
# dev.off()


# backtransform logit values (from https://sebastiansauer.github.io/convert_logit2prob/)

## coef(glm1)

## (Intercept)      Pclass 
##   1.4467895  -0.8501067

coef(o.mod22_ubms)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(o.mod22_ubms))
#logit2prob(c(-4.126, -2.145, 0.679, -0.06323, -0.00893))
logit2prob(c(0.573, 0.586, 0.232, 0.3883, 0.0255))

# logit2prob(coef(o.mod22_ubms)[1:2])
# logit2prob(coef(o.mod22_ubms)[c(1,3)])
# logit2prob(coef(o.mod22_ubms)[4:5])

logit_psi <- seq(-5,5, 0.1)
psi <- invlogit(logit_psi)
plot(psi ~logit_psi, type = "l")


# occupancy ###

# all sites
n_pred <- predict(o.mod22_ubms, submodel="state")
mean(n_pred$Predicted)
mean(n_pred$SD)

# sbc sites only
sbc <- c(CTtable[CTtable$site %in% "Nui Chua NP",]$station,
         CTtable[CTtable$site %in% "Hon Heo",]$station,
         CTtable[CTtable$site %in% "Krong Trai",]$station)
n_pred$stat <- row.names(covariates_subset)
n_pred_sbc <- n_pred[(n_pred$stat %in% sbc),]
mean(n_pred_sbc$Predicted)
mean(n_pred_sbc$SD)


# detection ###

# all sites
n_pred_d <- predict(o.mod22_ubms, submodel="det")
mean(n_pred_d$Predicted, na.rm = T)
mean(n_pred_d$SD, na.rm = T)

# sbc only sites
#??




### >>> Model 1

# plot_marginal(o.mod1_ubms, "state") +
#   ggtitle(paste(myspecies, "; ", 
#                 format(fms_ubms[[2]]@call$formula), "; \n",
#                 "rank = ", which(row.names(modsel_ubms) == names(fms_ubms)[2]),
#                 "; LOOIC = ", round(fms_ubms[[2]]@loo$estimates[3], 2),
#                 #"; uncertainty =      ;", #??
#                 # backtransform?
#                 sep = ""))


nd <- data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
                 scale.habitat = 0)
#nd$scale.elevation_sq <- nd$scale.elevation^2
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <- (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod11_ubms,
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- which(rownames(modsel_ubms) == "p(Effort) psi(elevation) data")

# plot
p11_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank 9", rank,"; p(Effort) psi(elevation)")) +
  xlab("elevation (m)")
p11_ubms


### >>> Model 2


nd <- data.frame(scale.elevation = 0,
                 scale.semi.dry.forest = seq(min(scale.semi.dry.forest),max(scale.semi.dry.forest), length.out = length(scale.semi.dry.forest)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <- (nd[,2] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod2_ubms,
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- which(rownames(modsel_ubms) == "p(Effort) psi(semi.dry.forest) data")

# plot
p2_ubms <- ggplot(tmp, aes(x = scale.semi.dry.forest, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(.) psi(semi.dry.forest)")) +
  xlab("semi.dry.forest")

p2_ubms



### >>> Model 3

#plot_marginal(o.mod3_ubms, "state")

nd <- data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
                 scale.semi.dry.forest = 0)
#nd$scale.elevation_sq <- nd$scale.elevation^2
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <- (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod3_ubms,
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- which(rownames(modsel_ubms) == "p(.) psi(elevation + semi.dry.forest) data")

# plot
p31_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(.) psi(elevation + semi.dry.forest)")) +
  xlab("elevation")



nd <- data.frame(scale.elevation = 0,
                 scale.semi.dry.forest = seq(min(scale.semi.dry.forest),max(scale.semi.dry.forest), length.out = length(scale.semi.dry.forest)))
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,2] <- (nd[,2] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod3_ubms,
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p32_ubms <- ggplot(tmp, aes(x = scale.semi.dry.forest, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(.) psi(elevation + semi.dry.forest)")) +
  xlab("semi.dry.forest")



grid.arrange(p31_ubms, p32_ubms)




### >>> Model 4

#plot_marginal(o.mod4_ubms, "state")


nd <- data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
                 scale.semi.dry.forest = 0)
nd$scale.elevation_sq <- nd$scale.elevation^2
# backtransform to unscaled values
nd_backtransform <- nd
nd_backtransform[,1] <- (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod4_ubms,
                     newdata = nd,
                     submodel = "state"))

colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"
rank <- which(rownames(modsel_ubms) == "p(.) psi(elevation + semi.dry.forest + elevation^2) data")

# plot
p41_ubms <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(.) psi(elevation + elevation^2 + semi.dry.forest)")) +
  xlab("elevation")



# select covariate and set other to 0
nd <- data.frame(scale.elevation = 0,
                 scale.semi.dry.forest = seq(min(scale.semi.dry.forest),max(scale.semi.dry.forest), length.out = length(scale.semi.dry.forest)))
# backtransform to unscaled values. tmp4plot is scaled (mean = 0, sd = 1)
nd_backtransform <- nd
nd_backtransform[,2] <- (nd[,2] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
# create dataset for ggplot
tmp <- cbind(nd_backtransform,
             predict(o.mod4_ubms,
                     newdata = nd,
                     submodel = "state"))
colnames(tmp) [colnames(tmp) == "2.5%"] <- "lower"
colnames(tmp) [colnames(tmp) == "97.5%"] <- "upper"

# plot
p42_ubms <- ggplot(tmp, aes(x = scale.semi.dry.forest, y = Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggtitle(paste0("Rank ", rank, "; p(.) psi(elevation + elevation^2 + semi.dry.forest)")) +
  xlab("semi.dry.forest")


grid.arrange(p41_ubms, p42_ubms)




plot_residuals(o.mod1_ubms, "state")
plot_residuals(o.mod2_ubms, "state")
plot_residuals(o.mod3_ubms, "state")
plot_residuals(o.mod4_ubms, "state")

plot_residuals(o.mod1_ubms, "det")
plot_residuals(o.mod2_ubms, "det")
plot_residuals(o.mod4_ubms, "det")
plot_residuals(o.mod4_ubms, "det")
# --> Error: Couldn't find working breakpoints


# plot_marginal(o.mod1_ubms, "state")
# plot_marginal(o.mod8_ubms, "state")
# plot_marginal(o.mod2_ubms, "state")
# plot_marginal(o.mod7_ubms, "state")
# plot_marginal(o.mod3_ubms, "state")
# plot_marginal(o.mod5_ubms, "state")
# plot_marginal(o.mod4_ubms, "state")
# plot_marginal(o.mod6_ubms, "state")
# plot_marginal(o.mod9_ubms, "state")





# pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/test.pdf', width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# par(mfrow = c(3,2))                        ## 2x3 grid of plotting areas
# replicate(plot(rnorm(99)), n = 6)          ## 6 example plots
# dev.off()


# o.mod1_ubms <- stan_occu(~ 1 ~ scale.elevation, data = umf)
# o.mod2_ubms <- stan_occu(~ 1 ~ scale.semi.dry.forest, data = umf)
# o.mod3_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.semi.dry.forest, data = umf)
# o.mod4_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.semi.dry.forest + I(scale.elevation^2), data = umf)
# 
# o.mod5r_ubms <- stan_occu(~ 1 ~ scale.remoteness, data = umf)
# o.mod6r_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.remoteness, data = umf)
# o.mod7r_ubms <- stan_occu(~ 1 ~ scale.remoteness + scale.semi.dry.forest, data = umf)
# o.mod8r_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.semi.dry.forest + scale.remoteness, data = umf)
# o.mod9r_ubms <- stan_occu(~ 1 ~ scale.elevation + scale.semi.dry.forest + scale.remoteness + I(scale.elevation^2), data = umf)
# 
# o.mod5_ubms <- stan_occu(~ 1 ~ site + scale.elevation + scale.semi.dry.forest, data = umf)
# o.mod6_ubms <- stan_occu(~ 1 ~ site + scale.elevation + scale.semi.dry.forest + I(scale.elevation^2), data = umf)
# o.mod7_ubms <- stan_occu(~ 1 ~ site + scale.semi.dry.forest, data = umf)
# o.mod8_ubms <- stan_occu(~ 1 ~ site + scale.elevation, data = umf)
# o.mod9_ubms <- stan_occu(~ 1 ~ site, data = umf)





# pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/plotx1.pdf', 
#     # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
#     width = 11.7, height = 8.3)  
#     #width = 11.7, height = 5.53)  
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(
#   # plot_marginal(o.mod1_ubms, "state"),
#   # plot_marginal(o.mod2_ubms, "state"),
#   # plot_marginal(o.mod5r_ubms, "state"),
#   # plot_marginal(o.mod3_ubms, "state"),
#   # plot_marginal(o.mod6r_ubms, "state"),
#   # plot_marginal(o.mod7r_ubms, "state"),
#   # plot_marginal(o.mod4_ubms, "state"),
#   # plot_marginal(o.mod8r_ubms, "state"),
#   # plot_marginal(o.mod9r_ubms, "state"),
#    # plot_marginal(o.mod5_ubms, "state"),
#    # plot_marginal(o.mod6_ubms, "state"),
#    # plot_marginal(o.mod7_ubms, "state"),
#    # plot_marginal(o.mod8_ubms, "state"),
#    # plot_marginal(o.mod9_ubms, "state"),
# 
#   ncol = 2, nrow = 1
#   )
# 
# dev.off()
# 
# pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/plot1-.pdf', 
#     # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
#     # width = 11.7, height = 8.3)  
#     width = 11.7, height = 8.3)  
# par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
# ggarrange(plotlist = list(
#   plot_marginal(o.mod2_ubms, "state"),
#   plot_marginal(o.mod2_ubms, "state"),
#   plot_marginal(o.mod2_ubms, "state"),
#   plot_marginal(o.mod2_ubms, "state"),
#   plot_marginal(o.mod2_ubms, "state"),
#   plot_marginal(o.mod2_ubms, "state"),
#   plot_marginal(o.mod2_ubms, "state"),
#   plot_marginal(o.mod2_ubms, "state")),
#   ncol = 4, nrow = 2
# )
# dev.off()
# 
# 
# pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/plot1-.pdf', 
#     # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
#     # width = 11.7, height = 8.3)  
#     width = 11.7, height = 8.3)  
# par(omi = rep(.5, 4))  
# ggarrange(plotlist = list(
#   plot_marginal(o.mod2_ubms, "state"),
#   plot_marginal(o.mod2_ubms, "state"),
#   plot_marginal(o.mod3_ubms, "state"),
#   plot_marginal(o.mod3_ubms, "state"),
#   plot_marginal(o.mod3_ubms, "state"),
#   plot_marginal(o.mod3_ubms, "state"),
#   plot_marginal(o.mod3_ubms, "state"),
#   plot_marginal(o.mod3_ubms, "state")),
#   nrow = 2, ncol = 4
# )
# dev.off()











pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/final_single.pdf',
   # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
   # width = 11.7, height = 8.3)
   width = 11.7, height = 8.3)
par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
ggarrange(
  plot_marginal(o.mod11_ubms, "state"),
  plot_marginal(o.mod12_ubms, "state"),
  plot_marginal(o.mod13_ubms, "state"),
  plot_marginal(o.mod14_ubms, "state"),
  plot_marginal(o.mod17_ubms, "state"),
  plot_marginal(o.mod18_ubms, "state"))
dev.off()

pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/final_double_1-3.pdf',
    # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
    # width = 11.7, height = 8.3)
    width = 11.7, height = 8.3)
par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
ggarrange(plotlist = list(
  plot_marginal(o.mod20_ubms, "state"),
  plot_marginal(o.mod21_ubms, "state"),
  plot_marginal(o.mod22_ubms, "state")),
  # plot_marginal(o.mod25_ubms, "state"),
  # plot_marginal(o.mod26_ubms, "state")),
  ncol = 3, nrow = 1
)
dev.off()

pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/final_double_4-5.pdf',
    # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
    # width = 11.7, height = 8.3)
    width = 11.7, height = 8.3)
par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
ggarrange(plotlist = list(
  # plot_marginal(o.mod20_ubms, "state"),
  # plot_marginal(o.mod21_ubms, "state"),
  # plot_marginal(o.mod22_ubms, "state")),
  plot_marginal(o.mod25_ubms, "state"),
  plot_marginal(o.mod26_ubms, "state")),
  ncol = 3, nrow = 1
)
dev.off()


pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/final_triple.pdf',
    # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
    # width = 11.7, height = 8.3)
    width = 11.7, height = 8.3)
par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
ggarrange(plotlist = list(
  plot_marginal(o.mod36_ubms,"state"))
)
dev.off()


pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/final_double-squared_1-3.pdf',
    # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
    # width = 11.7, height = 8.3)
    width = 11.7, height = 8.3)
par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
ggarrange(plotlist = list(
  plot_marginal(o.mod28_ubms, "state"),
  plot_marginal(o.mod29_ubms, "state"),
  plot_marginal(o.mod30_ubms, "state")),
  # plot_marginal(o.mod33_ubms, "state"),
  # plot_marginal(o.mod34_ubms, "state")),
  ncol = 3, nrow = 1
)
dev.off()

pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/final_double-squared_4-5.pdf',
    # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
    # width = 11.7, height = 8.3)
    width = 11.7, height = 8.3)
par(omi = rep(.5, 4))                      ## 1/2 inch outer margins
ggarrange(plotlist = list(
  # plot_marginal(o.mod28_ubms, "state"),
  # plot_marginal(o.mod29_ubms, "state"),
  # plot_marginal(o.mod30_ubms, "state")),
  plot_marginal(o.mod33_ubms, "state"),
  plot_marginal(o.mod34_ubms, "state")),
  ncol = 3, nrow = 1
)
dev.off()







# pdf('C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/shareJN_AW/plot28_35_eff.pdf',
#     # width = 8.3, height = 11.7)  ## Device with dimensions of A4 paper
#     # width = 11.7, height = 8.3)
#     width = 11.7, height = 8.3)
# par(omi = rep(.5, 4))
# ggarrange(plotlist = list(
#   plot_marginal(o.mod28_ubms, "state"),
#   plot_marginal(o.mod29_ubms, "state"),
#   plot_marginal(o.mod30_ubms, "state"),
#   plot_marginal(o.mod31_ubms, "state"),
#   plot_marginal(o.mod32_ubms, "state"),
#   plot_marginal(o.mod33_ubms, "state"),
#   plot_marginal(o.mod34_ubms, "state"),
#   plot_marginal(o.mod35_ubms, "state")),
#   nrow = 2, ncol = 4
#     )
# dev.off()







# unmarked ______________________________________________________________________________________________________________________
# 
# # Set path to source code for adapted plotfunction
# source("C:/Users/s347553/Documents/R/multi-plot_edit_LP_v2.R")
# 
# tmpdir <- tempdir()
# multiplot.wrapper(model.list =  fms_unmarked@fits,
#                   siteCovs_unscaled = covariates_subset %>% select( -matches("scale.")), # original: ( -matches("scale."))
#                   modelRanking = modsel_unmarked,                                         # -> !! geht dann aber nicht !!
#                   occasionLength = occasionLength,
#                   #plotnames.by.rank ,
#                   plotDir = tmpdir,
#                   speciesVector = myspecies, #relevantspecies$species[[i]],
#                   index_tmp = 1,
#                   addRug = TRUE,
#                   #maximumRankToPlot = 200,
#                   plotModelsBelowNullModel = F,
#                   quiet = T
# )
# # --> works only for o.mod1_unmarked, o.mod2_unmarked, o.mod3_unmarked
#     
#     
#     
#     
#     ### >>> Model 1 rank5
#     
#     # select covariate and set other to 0
#     nd <- data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
#                      scale.habitat = 0)
#     # backtransform to unscaled values
#     nd_backtransform <- nd
#     nd_backtransform[,1] <- (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
#     # create dataset for ggplot
#     tmp <- cbind(nd_backtransform,
#                  predict(o.mod1_unmarked,   # select model
#                          newdata = nd,
#                          type = "state"))
#     # plot 
#     p1 <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#       ggtitle("Rank 4; o.mod1; p(.) psi(elevation)") + 
#       xlab("elevation")
#     
#     p1
#     
#     
#     
#     
#     
#     
#     
#     ### >>> Model 2 rank4
#     
#     # select covariate and set other to 0
#     nd <- data.frame(scale.elevation = 0,
#                      scale.habitat = seq(min(scale.habitat),max(scale.habitat), length.out = length(scale.habitat)))
#     # backtransform to unscaled values
#     nd_backtransform <- nd
#     nd_backtransform[,2] <- (nd[,2] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
#     # create dataset for ggplot
#     tmp <- cbind(nd_backtransform,
#                  predict(o.mod2_unmarked,
#                          newdata = nd,
#                          type = "state"))
#     # plot
#     p2 <- ggplot(tmp, aes(x = scale.habitat, y = Predicted)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#       ggtitle("Rank 3; o.mod2; p(.) psi(habitat)") + 
#       xlab("habitat")
#     
#     p2
#     
#     
#     
#     
#     
#     
#     ### >>> Model 3 rank 3
#     
#     # select covariate and set other to 0
#     nd <- data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
#                      scale.habitat = 0)
#     # backtransform to unscaled values
#     nd_backtransform <- nd
#     nd_backtransform[,1] <- (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
#     # create dataset for ggplot
#     tmp <- cbind(nd_backtransform,
#                  predict(o.mod3_unmarked,
#                          newdata = nd,
#                          type = "state"))
#     # plot
#     p31 <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#       ggtitle("Rank 2; o.mod3; p(.) psi(elevation + habitat)") + 
#       xlab("elevation")
#     
#     
#     
#     
#     # select covariate and set other to 0
#     nd <- data.frame(scale.elevation = 0,
#                      scale.habitat = seq(min(scale.habitat),max(scale.habitat), length.out = length(scale.habitat)))
#     # backtransform to unscaled values
#     nd_backtransform <- nd
#     nd_backtransform[,2] <- (nd[,2] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
#     # create dataset for ggplot
#     tmp <- cbind(nd_backtransform,
#                  predict(o.mod3_unmarked,
#                          newdata = nd,
#                          type = "state"))
#     # plot
#     p32 <- ggplot(tmp, aes(x = scale.habitat, y = Predicted)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#       ggtitle("Rank 3; o.mod3; p(.) psi(elevation + habitat)") + 
#       xlab("habitat")
#     
#     
#     
#     grid.arrange(p31, p32)
#     
#     
#     
#     
#     ### >>> Model 4 (^2) rank1
#     
#     # select covariate and set other to 0
#     nd <- data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
#                      scale.habitat = 0,
#                      scale.elevation2 = 0)
#     # backtransform to unscaled values
#     nd_backtransform <- nd
#     nd_backtransform[,1] <- (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
#     # create dataset for ggplot
#     tmp <- cbind(nd_backtransform,
#                  predict(o.mod4_unmarked,
#                          newdata = nd,
#                          type = "state"))
#     # plot
#     p41 <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#       ggtitle("Rank 1;  o.mod4; p(.) psi(elevation + elevation^2 + habitat)") + 
#       xlab("elevation")
#     
#     
#     
#     
#     
#     # select covariate and set other to 0
#     nd <- data.frame(scale.elevation = 0,
#                      scale.habitat = seq(min(scale.habitat),max(scale.habitat), length.out = length(scale.habitat)),
#                      scale.elevation2 = 0)
#     # backtransform to unscaled values
#     nd_backtransform <- nd
#     nd_backtransform[,2] <- (nd[,2] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
#     # create dataset for ggplot
#     tmp <- cbind(nd_backtransform,
#                  predict(o.mod4_unmarked,
#                          newdata = nd,
#                          type = "state"))
#     # plot
#     p42 <- ggplot(tmp, aes(x = scale.habitat, y = Predicted)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#       ggtitle("Rank 1;  o.mod4; p(.) psi(elevation + elevation^2 + habitat)") + 
#       xlab("habitat")
#     
#     
#     
#     grid.arrange(p41, p42)
#     
#     
#     
#     
#     
#     ### >>> Model 4 (sq) rank2
#     
#     # select covariate and set other to 0
#     nd <- data.frame(scale.elevation = seq(min(scale.elevation),max(scale.elevation), length.out = length(scale.elevation)),
#                      scale.habitat = 0)
#     # backtransform to unscaled values. tmp4plot is scaled (mean = 0, sd = 1)
#     nd_backtransform <- nd
#     nd_backtransform[,1] <- (nd[,1] * attributes(scale(covariates_subset[,1]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,1]))$`scaled:center`[1]
#     # create dataset for ggplot
#     tmp <- cbind(nd_backtransform,
#                  predict(o.mod4_sq_unmarked,
#                          newdata = nd,
#                          type = "state"))
#     # plot
#     p43 <- ggplot(tmp, aes(x = scale.elevation, y = Predicted)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#       ggtitle("Rank 2;  o.mod4_sq; p(.) psi(elevation + habitat + elevation_sq)") + 
#       xlab("elevation")
#     
#     
#     
#     
#     # select covariate and set other to 0
#     nd <- data.frame(scale.elevation = 0,
#                      scale.habitat = seq(min(scale.habitat),max(scale.habitat), length.out = length(scale.habitat)))
#     # backtransform to unscaled values. tmp4plot is scaled (mean = 0, sd = 1)
#     nd_backtransform <- nd
#     nd_backtransform[,2] <- (nd[,2] * attributes(scale(covariates_subset[,2]))$`scaled:scale`[1]) + attributes(scale(covariates_subset[,2]))$`scaled:center`[1]
#     # create dataset for ggplot
#     tmp <- cbind(nd_backtransform,
#                  predict(o.mod4_sq_unmarked,
#                          newdata = nd,
#                          type = "state"))
#     # plot
#     p44 <- ggplot(tmp, aes(x = scale.habitat, y = Predicted)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#       ggtitle("Rank 2; o.mod4_sq; p(.) psi(elevation + habitat + elevation_sq)") + 
#       xlab("habitat")
#     
#     
#     
#     grid.arrange(p43, p44)
#     
#     
#     
#     
    

#### --> save / import R.data

# covariates <- covariates[1:2]
# save(CTtable, recTable, covariates, camop_matrix,
#   d_modsel_ubms, d.mod0_ubms, d.mod00_ubms,
#      modsel_ubms, o.mod1_ubms, o.mod2_ubms, o.mod3_ubms, o.mod4_ubms,
#   gof_result1, gof_result2, gof_result3, gof_result4,
#   file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/compare_all_SBC/occupancy_sites_all.RData")




# 7. Predictions ==============================================================================================

## 7.1 Import and scale covariate rasters =========================================================================

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



### LC probabiliy ======================================================================================================

# Import LC probability raster
#LCprob <- raster::stack("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/RF_prob_td2.8_wsmp10000_spl0.7_scale10-pred100.tif")
#Hab3raster <- LCprob$Semidry.Dry.Forest

# Clip to extent of aoi
#Hab3raster <- raster::crop(Hab3raster, extent(aoi))

# Aggregate 100m LC raster to 200m resolution to match with other rasters
#Hab3r_agg <- aggregate(Hab3raster, fact = (200/100), fun = mean, na.rm = TRUE)
#writeRaster(Hab3r_agg, "C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/RF_prob_hab3_td2.8_wsmp10000_spl0.7_scale10-100_200.tif")
#Hab3r_agg <- raster("C:/Users/s347553/Documents/Masterthesis/final_LCanalysis/RF_prob_hab3_td2.8_wsmp10000_spl0.7_scale10-100_200.tif")




Hab3r_agg <- raster::stack("D:/Luisa/Rsuperclass/SC_prob_10m/pred100_SC10_SVN_td2.8_10000.tif")
Hab3r_agg <- Hab3r_agg$pred100_SC10_SVN_td2.8_10000.3

Hab3r_agg <- raster::stack("D:/Luisa/Rsuperclass/SC_prob_10m/pred200_SC10_SVN_td2.8_10000.tif")
Hab3r_agg <- Hab3r_agg$pred200_SC10_SVN_td2.8_10000.3
names(Hab3r_agg) <- "Semidry.Dry.Forest"

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
Hab3raster_scaled <- (Hab3r_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "semi.dry.forest"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "semi.dry.forest"]



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

# # Import remoteness raster
# #distraster <- raster("D:/Luisa/covariates/Distance_coast/SVN_proximity_utm49n_in_meter.tif")
# 
# # Aggregate 30m remoteness raster to 200m resolution to match with other rasters
# #dist_agg <- aggregate(distraster, fact = (200/30), fun = mean, na.rm = TRUE)
# #writeRaster(dist_agg, "D:/Luisa/covariates/Distance_coast/SVN_proximity_utm49n_in_meter_aggr200.tif")
# dist_agg <- raster("D:/Luisa/covariates/Distance_coast/SVN_proximity_utm49n_in_meter_aggr200.tif")
# #remr_agg <- aggregate(remr_agg, fact = (1000/200), fun = mean, na.rm = TRUE)
# 
# # Scale elevation raster
# distraster_scaled <- (dist_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "distance"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "distance"]
# 
# 
# # attr(covariates_scaled_matrix, "scaled:center")
# # attr(covariates_scaled_matrix2, "scaled:center")
# # attr(covariates_scaled_matrix, "scaled:scale")
# # attr(covariates_scaled_matrix2, "scaled:scale")
# # 
# # head(covariates_scaled_matrix)
# # head(covariates_scaled_matrix2)


#### Correlation --------------------------------------------------------------------------------------

# resample
#LC_prob <- Hab3r_agg
LC_prob <- Hab3r_agg[[c(1:3,9)]]
names(LC_prob) <- c("Evergreen.Forest",
                    "Dry.Shrubland",
                    "Semidry.Dry.Forest",
                    #"Bare.Ground",
                    #"Cropland",
                    #"Water",
                    #"Built.up",
                    #"Grassland",
                    "Coniferous.Forest")
elevr_resamp <- resample(elevr_agg, LC_prob)
#distr_resamp <- resample(dist_agg, LC_prob)
remr_resamp <- resample(remr_agg, LC_prob)

# extract values
vals_e <- data.frame(values(elevr_resamp))
vals_r <- data.frame(values(remr_resamp))
#vals_d <- data.frame(values(distr_resamp))
vals_lc <- data.frame(values(LC_prob))

# combine values in dataframe
vals <- cbind(vals_lc, vals_e, vals_r)#, vals_d)

# check for NAs and remove if existing
NAs <- apply(vals, 1, function(x){any(is.na(x))})
sum(NAs)
vals2 <- na.omit(vals) # remove NAs
n <- names(vals2)
#vals3 <- vals2[,c(1:3,5,8:12)]
names(vals2) <- c(n[1:4], "Elevation", "Remoteness")

(corr <- cor(vals2))

# create corrplot for manyand export as pdf or image
ggcorrplot_r <- ggcorrplot(corr,                             # visualize the correlation-matrix
                         hc.order = FALSE,                  # use hierarchical clustering
                         type = "lower",                    # alternative: upper, full
                         lab = TRUE,                        # add correlation coefficients, change size
                         ggtheme = ggplot2::theme_grey(),   # alternative: ggplot2::theme_minimal, ggplot2::theme_dark
                         title = "Correlation of Covariates"
                         # p.mat = p.mat,                   # Barring the no significant coefficient
) +
  theme(plot.title = element_text(size=22))                  # add other relevant theme and guides methods (still ggplot2 object)

#png(filename = "C:/Users/s347553/Documents/Masterthesis/Data/figures/occupancy/raster200m_correlation.png")
plot(ggcorrplot_r)
#dev.off()

(vif <- vifcor(vals2, th=0.7))


# 7.2 Create rasterstack ============================================================================

# rename scaled covariate rasters
elevr <- elevationraster_scaled
hab3r <- Hab3raster_scaled
remr <- remraster_scaled
#distr <- distraster_scaled

res(elevr)
res(remr)
res(hab3r)
# resample rasters for raster stack
#elevr <- resample(elevr, villdensr)

elevr <- resample(elevr,hab3r)
remr <- resample(remr,hab3r)

# #remr <- resample(remr, elevr)
# distr <- resample(distr, elevr)

# stack rasters
predstack <- raster::stack(elevr, hab3r)#, remr)
names(predstack) <- c("scale.elevation", "scale.semi.dry.forest")#, "scale.remoteness")#, "scale.evelvation_sq", "scale.habitat_sq", "scale.remoteness_sq")

#predstack_200_ehr <- predstack
#predstack_200_eh <- predstack
 
# predstack1000 <- aggregate(predstack, fact = (1000/200), fun = mean, na.rm = TRUE)
# predstack1000 <- raster::stack(predstack1000)



# 7.3 Make predictions ---------------------------------------------------------------------------------------

# Predict ubms model _________________________________________________________________________________________________________

# #--> carefule, ubms model prediction can fail quite fast due to large data size. Need tiles.
# # --> 30m preds possible at tile size of small potential new ct area close to Hon Heo
# # --> 150/200, preds possible at tile size of larger potential new ct area close to Hon Heo
# 
# start_time <- Sys.time()
# prediction_o.mod3_ubms_1000 <- predict(o.mod3_ubms, newdata  = predstack1000, submodel = "state")
# end_time <- Sys.time()
# end_time - start_time
# # --> Time difference 1000 scale of 47.91 secs
# # --> Time difference 200 scale of 10.1 mins / 8.03 mins / 9.8 mins
# 
# plot(prediction_o.mod3_ubms_1000)
# 
# #terra::writeRaster(terra::rast(prediction_o.mod3_ubms), "D:/Luisa/Rsuperclass/SC_prob_10m/SBC_predictions_200m/SVN_SBC_pred200_CTall_editLP2_ehh2_occ10_1_mod4_eh_frombigtiff_ubms_nocattien_aggr1000.tif")
# terra::writeRaster(terra::rast(prediction_o.mod8r_ubms_1000), "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_predictions/SVN_SBC_pred1000_CTall_editLP2_ehr_occ20.tif")
# terra::writeRaster(terra::rast(prediction_o.mod3_ubms_1000), "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_predictions/SVN_SBC_pred1000_CTall_editLP2_eh_occ20.tif")


## predict ubms to 200m raster tiles -------

# create grid for smaller tiles
a <- aggregate(raster(predstack), 200)
p <- as(a, 'SpatialPolygons')
plot(p)
aoi_utm <- st_transform(st_as_sf(aoi), 32649)
plot(aoi_utm, add=TRUE)
x <- lapply(seq_along(p), function(i) crop(predstack, p[i]))


# start_time <- Sys.time()
# prediction_o.mod22_ubms_200_tile7 <- predict(o.mod22_ubms, newdata  = stack(x[[7]]), submodel = "state")
# end_time <- Sys.time()
# end_time - start_time
# # --> Time difference 200 scale of 1,9 mins
pred_tiles <- list()
s <- seq(1, (length(x)))

start_time <- Sys.time()
for (i in 1:length(x)){
  pred_tiles[[i]] <- predict(o.mod22_ubms, stack(x[[i]]), submodel = "state")

  terra::writeRaster(terra::rast(pred_tiles[[i]]), 
                     filename = paste0("C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_predictions/SVN_SBC_pred200_tiles_mod22-eh_occ15_seed5/SVN_SBC_pred200_tiles_mod22-eh_occ15_seed5_tile_", s[i],".tif"))
}
end_time <- Sys.time()
end_time - start_time


# merge tiles in QGIS with GDAL raster merge (float32, null values: 0, -9999, highcompression, bigtiff = yes)

# import merged raster, set names and export final 200m SBC prediction for SVN!! :)
pred_imp <- terra::rast("C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_predictions/SVN_SBC_pred200_tiles_mod22-eh_occ15_seed5/SVN_SBC_pred200_tiles_mod22-eh_occ15_seed5_float32_0_-9999_highcompress_bigtiffyes.tif")
names(pred_imp) <- names(pred_tiles[[1]])

terra::writeRaster(pred_imp, "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_predictions/SVN_SBC_pred200_tiles_mod22-eh_occ15_seed5/SVN_SBC_pred200_tiles_mod22-eh_occ15_seed5_float32_0_-9999_highcompress_bigtiffyes_named-r.tif")


###> Predict Khanh Hoa Province for An ------------------------------------------------------------

aoi2 <- shapefile("C:/Users/s347553/Documents/Masterthesis/Data/Khanh_Hoa_province_utm49n.shp")

predstack_200_ehr_kh <- raster::crop(predstack_200_ehr, aoi2)
predstack_200_ehr_kh <- stack(predstack_200_ehr_kh)

start_time <- Sys.time()
prediction_o.mod36_ubms_kh <- predict(o.mod36_ubms, newdata  = predstack_200_ehr_kh, submodel = "state")
end_time <- Sys.time()
end_time - start_time
# --> Time difference 200 scale of 3.35 mins (21.12.2022)

plot(prediction_o.mod36_ubms_kh)

#terra::writeRaster(terra::rast(prediction_o.mod36_ubms_kh), "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_predictions/SVN_SBC_pred200_ele_sdf_rem_occ20_khanhhoa.tif")



predstack_200_eh_kh <- raster::crop(predstack_200_eh, aoi2)
predstack_200_eh_kh <- stack(predstack_200_eh_kh)

start_time <- Sys.time()
prediction_o.mod22_ubms_kh <- predict(o.mod22_ubms, newdata  = predstack_200_eh_kh, submodel = "state")
end_time <- Sys.time()
end_time - start_time
# --> Time difference 200 scale of 3.33 mins

plot(prediction_o.mod22_ubms_kh)

#terra::writeRaster(terra::rast(prediction_o.mod22_ubms_kh), "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/all_covs/SBC_predictions/SVN_SBC_pred200_ele_sdf_occ20_khanhhoa.tif")




# # 7.4 potential new area ============================================================================
# 
# ## pot. aoi small 30m --------------------------------------------------------------------------------
# 
# aoi_pot_small <- terra::vect("C:/Users/s347553/Documents/Masterthesis/Data/Potential_survey_area_utm.shp")
# #--> diese größe geht klar mit predict ubms model at 30m scale
# 
# ### elevation -----
# elevationraster <- terra::rast("D:/Luisa/covariates/elevation/SRTM30m_NAs_filled_SVN_utm49qgis.tif")
# elev_pot <- terra::crop(elevationraster, aoi_pot_small)
# 
# 
# elev_pot_scaled <- (elev_pot - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "elevation"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "elevation"]
# 
# 
# ### habitat -----
# SC_pred <- terra::rast("D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_10m_SC_SVN_td2-8_10000_fromutmmask_113tiles_merge_gdal_float32_0_-9999_highcompr_bigtiffyes_sdfonly.tif")
# sdf_pot <- terra::crop(SC_pred, aoi_pot_small)
# 
# sdf_pot_agg <- aggregate(sdf_pot, fact = (30/10), fun = mean, na.rm = TRUE)
# 
# sdf_pot_scaled <- (sdf_pot_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "habitat"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "habitat"]
# 
# sdf_pot_scaled <- raster::resample(sdf_pot_scaled, elev_pot_scaled)
# 
# 
# sdf_pot_scaled <- raster(sdf_pot_scaled)
# elev_pot_scaled <- raster(elev_pot_scaled)
# 
# 
# predstack_pot <- raster::stack(elev_pot_scaled, sdf_pot_scaled)
# names(predstack_pot) <- c("scale.elevation", "scale.habitat")
# plot(predstack_pot)
# 
# 
# start_time <- Sys.time()
# prediction_o.mod3_ubms <- predict(o.mod3_ubms_20, newdata  = predstack_pot, submodel = "state")
# end_time <- Sys.time()
# end_time - start_time
# 
# # --> Time difference small aoi 30scale of 2.01 mins (ubms)
# plot(prediction_o.mod3_ubms)
# terra::writeRaster(terra::rast(prediction_o.mod3_ubms), "D:/Luisa/Rsuperclass/SC_prob_10m/SBC_predictions_200m/SVN_SBC_pred30_ubms3_occ20_eh_pot_aoi.tif")
# 
# 
# 
# ## pot. aoi 200m ========================================================================
# 
# aoi_pot <- terra::vect("C:/Users/s347553/Documents/Masterthesis/Data/newSBC.shp")
# #--> diese größe geht klar mit predict ubms model at 200m scale
# 
# ### elevation -----
# elevationraster <- terra::rast("D:/Luisa/covariates/elevation/SRTM30m_NAs_filled_SVN_utm49qgis.tif")
# elev_pot <- terra::crop(elevationraster, aoi_pot)
# 
# elev_pot_agg <- aggregate(elev_pot, fact = (150/30), fun = mean, na.rm = TRUE)
# 
# elev_pot_scaled <- (elev_pot_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "elevation"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "elevation"]
# 
# 
# ### habitat -----
# SC_pred <- terra::rast("D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_10m_SC_SVN_td2-8_10000_fromutmmask_113tiles_merge_gdal_float32_0_-9999_highcompr_bigtiffyes_sdfonly.tif")
# # sdf <- SC_pred$`pred_10m_SC_SVN_td2-8_10000_fromutmmask_113tiles_merge_gdal_float32_0_-9999_highcompr_bigtiffyes_3`
# # names(sdf) <- "Semi.Dry.Forest"
# # terra::writeRaster(sdf,"D:/Luisa/Rsuperclass/SC_prob_10m/pred10_SC10_SVN_td2.8_10000/pred_10m_SC_SVN_td2-8_10000_fromutmmask_113tiles_merge_gdal_float32_0_-9999_highcompr_bigtiffyes_sdfonly.tif")
# sdf_pot <- terra::crop(SC_pred, aoi_pot)
# 
# sdf_pot_agg <- aggregate(sdf_pot, fact = (150/10), fun = mean, na.rm = TRUE)
# #res(sdf_pot_agg)
# 
# sdf_pot_scaled <- (sdf_pot_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "habitat"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "habitat"]
# 
# 
# 
# sdf_pot_scaled <- resample(sdf_pot_scaled, elev_pot_scaled)
# 
# sdf_pot_scaled <- raster(sdf_pot_scaled)
# elev_pot_scaled <- raster(elev_pot_scaled)
# 
# predstack_pot <- raster::stack(elev_pot_scaled, sdf_pot_scaled)
# names(predstack_pot) <- c("scale.elevation", "scale.habitat")
# plot(predstack_pot)
# 
# start_time <- Sys.time()
# prediction_o.mod3_ubms <- predict(o.mod3_ubms_20, newdata  = predstack_pot, submodel = "state")
# end_time <- Sys.time()
# end_time - start_time
# 
# # --> Time difference pot aoi 100scale of 3.32 mins (ubms)
# plot(prediction_o.mod3_ubms)
# 
# 
# terra::writeRaster(terra::rast(prediction_o.mod3_ubms), "D:/Luisa/Rsuperclass/SC_prob_10m/SBC_predictions_200m/SVN_SBC_pred150_ubms3_occ20_eh_pot_aoi.tif")
# 
# 



# Predict unmarked model _________________________________________________________________________________________________________
# 
# # --> check out SBC bestmodels:     sfd     5,3,6,7,1,4,2
# modsel_df_round
# 
# 
# # ## --> predict_manual not applicable to multiple covariate models! 
# # # Set path to sourcecode to calculate predictions (single covariate models)
# # source(file.path("D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/Screenforbio_Dopbox_backup/scripts/predict_manual.R"))
# # prediction_o.mod5 <- lapply(o.mod5, FUN = predict_manual, covariate_raster_stack = predstack)
# # prediction_o.mod14 <- lapply(o.mod14, FUN = predict_manual, covariate_raster_stack = predstack)
# # prediction_o.mod12 <- lapply(o.mod12, FUN = predict_manual, covariate_raster_stack = predstack)
# 
# 
# # Either use predict ...
# start_time <- Sys.time()
# prediction_o.mod4 <- predict(o.mod4, newdata  = predstack, type = "state")
# end_time <- Sys.time()
# end_time - start_time
# # --> Time difference 1000 scale of 47.91 secs
# # --> Time difference 200 scale of 10.1 mins / 8.03 mins / 9.8 mins / 4 h
# 
# #writeRaster(prediction_o.mod4, "D:/Luisa/Rsuperclass/SC_prob_10m/SBC_predictions_200m/SVN_SBC_pred100_CTall_ehh2_occ10_1_mod4_eh.tif")
# terra::writeRaster(terra::rast(prediction_o.mod4), "D:/Luisa/Rsuperclass/SC_prob_10m/SBC_predictions_200m/SVN_SBC_pred200_CTall_editLP2_ehh2_occ10_1_mod4_eh_frombigtiff_unmarked.tif")
# 
# 
# 
# 










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




# END __________________________________________________________________________________________________


# <> CT BY SITE ==================================================================================

# load(file = "C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/final_CTtable_corrected_editLP_28.11.RData")
# load(file = "C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal/final_recTable_corrected_editLP_28.11.RData")

site <- unique(CTtable$site)

ct_site <- list(
  CT_krongtrai <- CTtable[CTtable$site == "Krong Trai",],
  CT_camanbac <- CTtable[CTtable$site == "Cam An Bac",],
  CT_suoitien <- CTtable[CTtable$site == "Suoi Tien",],
  CT_honheo <- CTtable[CTtable$site == "Hon Heo",],
  CT_nuichua <- CTtable[CTtable$site == "Nui Chua NP",],
  CT_sopai <- CTtable[CTtable$site == "So Pai",],
  CT_deoca <- CTtable[CTtable$site == "Deo Ca ",],
  CT_songhinh <- CTtable[CTtable$site == "Song Hinh",],
  CT_cattien <- CTtable[CTtable$site == "Cat Tien",],
  CT_bidoup <- CTtable[CTtable$site == "Bidoup",],
  CT_phuocbinh <- CTtable[CTtable$site == "Phuoc Binh",]
)
  ct_site[[1]] <- ct_site[[1]][,c(1:10, 19)]
  ct_site[[2]] <- ct_site[[2]][,c(1:10, 19)]
  ct_site[[3]] <- ct_site[[3]][,c(1:10, 19)]
  ct_site[[4]] <- ct_site[[4]][,c(1:10, 19)]
  ct_site[[5]] <- ct_site[[5]][,c(1:10, 19)]
  ct_site[[6]] <- ct_site[[6]][,c(1:10, 19)]
  ct_site[[7]] <- ct_site[[7]][,c(1:10, 19)]
  ct_site[[8]] <- ct_site[[8]][,c(1:10, 19)]
  ct_site[[10]] <- ct_site[[10]][,c(1:10, 19)]
  ct_site[[11]] <- ct_site[[11]][,c(1:10, 19)]
names(ct_site) <- site



# #check out single site parameters
#
#   site_report <- reportTest[[1]]
#   rownames(site_report) <- site_report$station
#   keep_stations1 <- ct_site[[1]]$station
#   keep_stations2 <- ct_site[[2]]$station
#   keep_stations3 <- ct_site[[3]]$station
#   keep_stations4 <- ct_site[[4]]$station
#   keep_stations5 <- ct_site[[5]]$station
#   keep_stations6 <- ct_site[[6]]$station
#   keep_stations7 <- ct_site[[7]]$station
#   keep_stations8 <- ct_site[[8]]$station
#   keep_stations9 <- ct_site[[9]]$station
#   keep_stations10 <- ct_site[[10]]$station
#   keep_stations11 <- ct_site[[11]]$station
# report_site <- list(
#   site_report_1 <- site_report[(row.names(site_report) %in% keep_stations1), ],
#   site_report_2 <- site_report[(row.names(site_report) %in% keep_stations2), ],
#   site_report_3 <- site_report[(row.names(site_report) %in% keep_stations3), ],
#   site_report_4 <- site_report[(row.names(site_report) %in% keep_stations4), ],
#   site_report_5 <- site_report[(row.names(site_report) %in% keep_stations5), ],
#   site_report_6 <- site_report[(row.names(site_report) %in% keep_stations6), ],
#   site_report_7 <- site_report[(row.names(site_report) %in% keep_stations7), ],
#   site_report_8 <- site_report[(row.names(site_report) %in% keep_stations8), ],
#   site_report_9 <- site_report[(row.names(site_report) %in% keep_stations9), ],
#   site_report_10 <- site_report[(row.names(site_report) %in% keep_stations10), ],
#   site_report_11 <- site_report[(row.names(site_report) %in% keep_stations11), ]
# )
# names(report_site) <- site 
# 
# 
# min(reportTest$survey_dates$setup)
# max(reportTest$survey_dates$retrieval)
# 
# head(site_report_1)
# str(site_report_1)
# 
# names(report_site[11])
# min(report_site[[11]]$setup)
# max(report_site[[11]]$retrieval)
# sum(report_site[[11]]$n_cameras)
# sum(report_site[[11]]$n_calendar_days_total)
# sum(report_site[[11]]$n_calendar_days_active)
# sum(report_site[[1]]$n_calendar_days_inactive)
# sum(report_site[[11]]$n_trap_nights_active)




# select only sbc stations
stat <- c(CT_nuichua$station, CT_honheo$station, CT_krongtrai$station)

recTable_SBC <- recTable[recTable$station %in% stat,]

CTtable_SBC <- rbind(CTtable[CTtable$site == "Krong Trai",],
                     CTtable[CTtable$site == "Hon Heo",],
                     CTtable[CTtable$site == "Nui Chua NP",])
CTtable_SBC <- CTtable_SBC[,c(1:10, 19)]


camop_sbc <- cameraOperation(CTtable = CTtable_SBC,
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


report_sbc <- surveyReport(recordTable          = recTable_SBC,
                           CTtable              = CTtable_SBC,
                           camOp                = camop_sbc,
                           speciesCol           = "Species",
                           stationCol           = "station",
                           cameraCol            = "camera",
                           setupCol             = "date_setting",
                           retrievalCol         = "date_retrieval",
                           CTDateFormat         = "dmy",
                           recordDateTimeCol    = "DateTimeOriginal",
                           recordDateTimeFormat = "ymd HMS",#"%Y-%m-%d %H:%M:%S",
                           #CTHasProblems        = TRUE,
                           # sinkpath = "C:/Users/s347553/Documents/Masterthesis/Data/CT/CTfinal",
                           makezip              = F) 

# save(stat_sbc, CTtable_SBC, recTable_SBC, camop_sbc, report_sbc, 
#      file = "C:/Users/s347553/Documents/Masterthesis/Data/occupancy/SBC_sites_only.RData")





########> o.mod3_ubms_20 <- o.mod3_ubms



 
 gof_result1_ehall <- gof_result1
 gof_result2_ehall <- gof_result2
 gof_result4_ehall <- gof_result4
 gof_result3_ehall <- gof_result3
 p1_ubms_ehall <- p1_ubms
 p2_ubms_ehall <- p2_ubms
 p31_ubms_ehall <- p31_ubms
 p32_ubms_ehall <- p32_ubms
 p42_ubms_ehall <- p42_ubms
 p41_ubms_ehall <- p41_ubms
 d.mod_ubms_ehall <- d.mod_ubms
 d_modsel_ubms_ehall <- d_modsel_ubms
 o.mod4_ubms_ehall <- o.mod4_ubms
 o.mod3_ubms_ehall <- o.mod3_ubms
 o.mod2_ubms_ehall <- o.mod2_ubms
 o.mod1_ubms_ehall <- o.mod1_ubms
 modsel_ubms_ehall <- modsel_ubms
 
 
 o.mod4_ubms
 modsel_ubms
 d.mod_ubms
 gof_result4
 gof_result3
 gof_result2
 gof_result1
 p1_ubms
 p2_ubms
 p31_ubms
 p32_ubms
 p42_ubms
 p41_ubms
 d_modsel_ubms
 o.mod3_ubms
 o.mod2_ubms
 o.mod1_ubms
 
 o.mod4_ubms_20 
 modsel_ubms_20 
 d.mod_ubms_20 
 gof_result4_20 
 gof_result3_20 
 p1_ubms_20 
 p2_ubms_20 
 p31_ubms_20 
 p32_ubms_20 
 p42_ubms_20 
 p41_ubms_20 
 d_modsel_ubms_20 
 o.mod3_ubms_20
 o.mod2_ubms_20 
 o.mod1_ubms_20 
 
 o.mod4_ubms_30 
 modsel_ubms_30 
 d.mod_ubms_30 
 gof_result4_30 
 gof_result3_30 
 gof_result2_30
 gof_result1_30
 p1_ubms_30 
 p2_ubms_30 
 p31_ubms_30 
 p32_ubms_30 
 p42_ubms_30 
 p41_ubms_30 
 d_modsel_ubms_30 
 o.mod2_ubms_30 
 o.mod1_ubms_30 
 
 
 
 
 o.mod11_ubms_20sure <- o.mod11_ubms
 o.mod12_ubms_20sure <- o.mod12_ubms   # best model?
 o.mod13_ubms_20sure <- o.mod13_ubms
 o.mod14_ubms_20sure <- o.mod14_ubms
 o.mod15_ubms_20sure <- o.mod15_ubms
 o.mod16_ubms_20sure <- o.mod16_ubms
 o.mod17_ubms_20sure <- o.mod17_ubms
 o.mod18_ubms_20sure <- o.mod18_ubms
 o.mod19_ubms_20sure <- o.mod19_ubms
 o.mod20_ubms_20sure <- o.mod20_ubms
 o.mod21_ubms_20sure <- o.mod21_ubms
 o.mod22_ubms_20sure <- o.mod22_ubms
 o.mod23_ubms_20sure <- o.mod23_ubms
 o.mod24_ubms_20sure <- o.mod24_ubms
 o.mod25_ubms_20sure <- o.mod25_ubms
 o.mod26_ubms_20sure <- o.mod26_ubms
 o.mod27_ubms_20sure <- o.mod27_ubms
 o.mod28_ubms_20sure <- o.mod28_ubms
 o.mod29_ubms_20sure <- o.mod29_ubms
 o.mod30_ubms_20sure <- o.mod30_ubms
 o.mod31_ubms_20sure <- o.mod31_ubms
 o.mod32_ubms_20sure <- o.mod32_ubms
 o.mod33_ubms_20sure <- o.mod33_ubms
 o.mod34_ubms_20sure <- o.mod34_ubms
 o.mod35_ubms_20sure <- o.mod35_ubms
 modsel_ubms_20sure <- modsel_ubms
 
 gof_result11_20sure <- gof_result11
 gof_result12_20sure <- gof_result12   # best model?
 gof_result13_20sure <- gof_result13
 gof_result14_20sure <- gof_result14
 gof_result15_20sure <- gof_result15
 gof_result16_20sure <- gof_result16
 gof_result17_20sure <- gof_result17
 gof_result18_20sure <- gof_result18
 gof_result19_20sure <- gof_result19
 gof_result20_20sure <- gof_result20
 gof_result21_20sure <- gof_result21
 gof_result22_20sure <- gof_result22
 gof_result23_20sure <- gof_result23
 gof_result24_20sure <- gof_result24
 gof_result25_20sure <- gof_result25
 gof_result26_20sure <- gof_result26
 gof_result27_20sure <- gof_result27
 gof_result28_20sure <- gof_result28
 gof_result29_20sure <- gof_result29
 gof_result30_20sure <- gof_result30
 gof_result31_20sure <- gof_result31
 gof_result32_20sure <- gof_result32
 gof_result33_20sure <- gof_result33
 gof_result34_20sure <- gof_result34
 gof_result35_20sure <- gof_result35
 
 
 
 
