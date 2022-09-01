#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# SINGLE SPECIES OCCUPANCY MODELING       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#     MASTER THESIS     LUISA PFLUMM      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(camtrapR)        # latest version: install_github("jniedballa/camtrapR"), library(remotes)
library(unmarked)
library(raster)
library(rgeos)
# library(Hmisc)
# library(corrplot)
library(lubridate)
library(usdm)
# library(sp)
library(sf)
#library(gdistance)
library(ggcorrplot)
library(ggpubr)
library(GGally)
# library(rgdal)
# library(RColorBrewer)
library(DT)
library(data.table)
library(dplyr)
library(spatialEco)


path1 <- "D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/CT"
#path <- "C:/Arbeit/Luisa/shareJN/CTdata"   



# 1. CT data ================================================================================================================

# Import CT and record tables
recTable <- read.csv(file = file.path(path1, "Quy/corrected_recTable_Quy_editLP_shareJN.csv"))
CTtable <- read.csv(file = file.path(path1, "Quy/corrected_CTtable_Quy_editLP_shareJN.csv"))

# Remove duplicate stations -- case specific!
CTtable <- CTtable[!duplicated(CTtable$station),]

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

# Import covariates
cov <- read.csv(file = file.path(path1, "covariates_quycttable_rmdoublestation_elev_hab3_rem_td2.8_smp10000_spl0.7_scale10-100.csv"))
rownames(cov) <- cov$X
covariates <- cov[-1]

# matrix contains attributes of scaled covs we need for backtransforming for prediction 
covariates_scaled_matrix <- scale(covariates[,1:(length(covariates)/2)]) 

# add columns with squared covariates
covariates_sq <- cbind(covariates,
                       scale.elevation_sq = covariates$scale.elevation^2,
                       scale.habitat_sq = covariates$scale.habitat^2,
                       scale.remoteness_sq = covariates$scale.remoteness^2
)


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
                                dateFormat = "ymd"
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

for (i in 1:length(unique(relevantspecies[,"species"]))) {
 activityDensity(recordTable = recTable,
                 species     = relevantspecies$species[43])   #43
}


### Species presence ###

# Define shapefile parameters
aoi <- shapefile("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/SVN_SBC_provinces_final.shp")

# Plot
for (i in 1:length(unique(relevantspecies[,"species"]))) {
  detectionMaps( CTtable       = CTtable,
                 recordTable   = recTable,
                 Xcol          = "UTM_X",
                 Ycol          = "UTM_Y",
                 backgroundPolygon = aoi,        # enable if you want to add background polygon
                 stationCol    = "station",
                 speciesCol    = "Species",
                 speciesToShow = relevantspecies$species[43],          # enable if you want to plot just one species
                 printLabels   = FALSE,
                 richnessPlot  = FALSE,                      # set TRUE to create map of the number of observed species
                 speciesPlots  = TRUE,                       # set TRUE to create single plots for each species
                 writePNG      = FALSE,                      # set TRUE for png output
                 #plotDirectory = file.path(wd,"/Output/Speciesplots"),
                 writeShapefile      = FALSE#,                # set TRUE for shapefile creation
                 #shapefileName       = shapefileName,
                 #shapefileDirectory  = pathout_shapefiles,
                 #shapefileProjection = shapefileProjection)
                 )
}



# 5. Occupancy modeling ====================================================================================================

## 5.1 Camera operation ====================================================================================================

#--> see 4.1 
camop_matrix <- camop_matrix





## 5.2 Detection history&effort ====================================================================================================

# Define timezone and occasion length.
timeZone <- "Asia/Ho_Chi_Minh" # change the time zone based on your study area
occasionLength <- 15 # change the occasion length based on your study

# containers for new outputlists
detHist <- list()
detection_history <- list()   # not necessary
effort <- list()              # not necessary

for (i in 1:length(unique(relevantspecies[,"species"]))) {
   detHist.sb <- detectionHistory(recordTable = recTable,
                                  species = relevantspecies$species[[i]],
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

  # list with effort und detectionhistory for all species
  detHist[[i]] <- detHist.sb                                   
  
  # lists with detectionhistory and effort for all species (not necessary)
  detection_history[[i]] <- detHist.sb$detection_history      
  effort[[i]] <- detHist.sb$effort

}

# assign species names to detHist-list
names(detHist) <- unique(relevantspecies[,"species"])






## 5.3 Unmarked frames ====================================================================================================

# containers for list
umf <- list()

# create unmarked dataframes to organize detection / non-detection data along with the covariates.
for (i in 1:length(unique(detHist))) {
  umf[[i]] <- unmarkedFrameOccu(y        = as.matrix(detHist[[i]][[1]]),
                                siteCovs = covariates_sq,
                                obsCovs  = list(Effort=as.matrix(detHist[[i]][[2]])))
}

# assign species names to umf lists
names(umf) <- names(detHist)



## 5.4 Detection models ====================================================================================================

#> Create detection models based on covariates dataset and choose the best detection model based on AIC 
#> for each relevant species. The lower the AIC value the better the model.

# Containes for lists
d.mod00 <- list()
d.mod0 <- list()
modellist_detection <- list()
d_modsel <- list()
d_modsel_df <- list()
d_modsel_df_round <- list()


for (i in 1:length(unique(detHist))) {
  
  # Define null models
  d.mod00[[i]] <- occu(~1 ~1, data = umf[[i]])            # no covariates
  d.mod0[[i]] <- occu(~ Effort ~ 1, data = umf[[i]])      # effort as covariate
  
  # Create list for modelselection 
  modellist_detection[[i]] <- fitList(fits = list("p(.) psi(.) data" = d.mod00[[i]],
                                                  "p(Effort) psi(.) data"  = d.mod0[[i]]))
  
  # Modelselection
  d_modsel[[i]] <- modSel(modellist_detection[[i]])
  
  # Convert to df
  d_modsel_df[[i]] <- data.frame(model = d_modsel[[i]]@Full$model,
                            covariate = gsub("~Effort ~ ", "", d_modsel[[i]]@Full$formula, ),
                            nPars = d_modsel[[i]]@Full$nPars,
                            AIC=d_modsel[[i]]@Full$AIC,
                            delta=d_modsel[[i]]@Full$delta,
                            AICwt=d_modsel[[i]]@Full$AICwt,
                            cumltvWt=d_modsel[[i]]@Full$cumltvWt
                            #p.Effort=d_modsel[[i]]@Full$`p(Effort)`,
                            #SEp.Effort=d_modsel[[i]]@Full$`SEp(Effort)`,
                            #p.Int=d_modsel[[i]]@Full$`p(Int)`,
                            #SEp.Int=d_modsel[[i]]@Full$`SEp(Int)`,
                            #psi.Int=d_modsel[[i]]@Full$`psi(Int)`,
                            #SEpsi.Int=d_modsel[[i]]@Full$`SEpsi(Int)`
  )
  
  # Round dataset
  d_modsel_df_round[[i]] <- data.frame(d_modsel_df[[i]][, c(1,2)], round(d_modsel_df[[i]][, -c(1,2)], 3))
  
  # Plot data
  #print(datatable(d_modsel_df_round[[i]]))
  print(knitr::kable(d_modsel_df_round[[i]], row.names = FALSE, caption = paste0("Modelselection ", relevantspecies$species[[i]])))
}

warnings() # --> 4 sites have been discarded because of missing data.

# assign species names to lists
names(d.mod00) <- names(d.mod0) <- names(modellist_detection) <- names(d_modsel)  <- 
  names(d_modsel_df) <- names(d_modsel_df_round) <- names(detHist)

# detection modelselection of SBC
d_modsel_df_round[43]




## 5.5 Occupancy models ====================================================================================================

### ??? list all possible model combinations -----------------------------------------------------------

# # create list of al possible combinations for each species
# vars <- list()
# vc <- list()
# models <- list()
# 
# for (i in 1:length(unique(detHist))){
#   vars[[i]] <- modsel_df[[i]]$covariate
#   for (l in 1:length(unique(vars[[i]]))){
#     vc[[l]] <- combn(vars[[i]],l)
#     for (j in 1:ncol(vc[[l]])){
#       model <- as.formula(paste0("~ Effort ~", paste0(vc[[l]][,j], collapse = "+")))
#       models <- c(models, model)
#     }
#   }
# }
# 



# scaled covariates have to exist as variables in R environment.
scale.elevation <- covariates_sq$scale.elevation
scale.habitat<- covariates_sq$scale.habitat
scale.remoteness <- covariates_sq$scale.remoteness

scale.elevation_sq <- covariates_sq$scale.elevation_sq
scale.habitat_sq <- covariates_sq$scale.habitat_sq
scale.remoteness_sq <- covariates_sq$scale.remoteness_sq


# Containes for lists
d.mod <- list()
o.mod1 <- list()
o.mod2 <- list()
o.mod3 <- list()
o.mod4 <- list()
o.mod5 <- list()
o.mod6 <- list()
o.mod7 <- list()
o.mod8 <- list()
o.mod9 <- list()
o.mod10 <- list()
o.mod11 <- list()
o.mod12 <- list()
o.mod13 <- list()
o.mod14 <- list()
o.mod15 <- list()
o.mod16 <- list()
o.mod17 <- list()
o.mod18 <- list()

fms <- list()
modsel <- list()
modsel_df <- list()
modsel_df_round <- list()


for (i in 1:length(unique(detHist))) {       # 43:43){

  # Define occupancy models
  o.mod1[[i]] <- occu(~ Effort ~ scale.elevation, data = umf[[i]]) #single linear effect
  o.mod2[[i]] <- occu(~ Effort ~ scale.habitat, data = umf[[i]]) #single linear effect
  o.mod3[[i]] <- occu(~ Effort ~ scale.remoteness, data = umf[[i]]) #single linear model effect
  o.mod4[[i]] <- occu(~ Effort ~ scale.elevation + scale.habitat, data = umf[[i]]) #multiple linear&quadratic effect
  o.mod5[[i]] <- occu(~ Effort ~ scale.elevation + scale.remoteness, data = umf[[i]]) #multiple linear&quadratic effect
  o.mod6[[i]] <- occu(~ Effort ~ scale.habitat + scale.remoteness, data = umf[[i]]) #multiple linear&quadratic effect
  o.mod7[[i]] <- occu(~ Effort ~ scale.elevation + scale.habitat + scale.remoteness, data = umf[[i]]) #multiple linear effect

  o.mod8[[i]] <- occu(~ Effort ~ scale.elevation + scale.elevation_sq, data = umf[[i]]) #single linear&quadratic effect
  o.mod9[[i]] <- occu(~ Effort ~ scale.habitat + scale.habitat_sq, data = umf[[i]]) #single linear&quadratic effect
  o.mod10[[i]] <- occu(~ Effort ~ scale.remoteness + scale.remoteness_sq, data = umf[[i]]) #single linear&quadratic effect
  
  o.mod11[[i]] <- occu(~ Effort ~ scale.elevation_sq, data = umf[[i]]) #multiple linear&quadratic effect
  o.mod12[[i]] <- occu(~ Effort ~ scale.habitat_sq, data = umf[[i]]) #multiple linear&quadratic effect
  o.mod13[[i]] <- occu(~ Effort ~ scale.remoteness_sq, data = umf[[i]]) #multiple linear&quadratic effect
  o.mod14[[i]] <- occu(~ Effort ~ scale.elevation_sq + scale.habitat_sq, data = umf[[i]]) #multiple linear&quadratic effect
  o.mod15[[i]] <- occu(~ Effort ~ scale.elevation_sq + scale.remoteness_sq, data = umf[[i]]) #multiple linear&quadratic effect
  o.mod16[[i]] <- occu(~ Effort ~ scale.habitat_sq + scale.remoteness_sq, data = umf[[i]]) #multiple linear&quadratic effect
  o.mod17[[i]] <- occu(~ Effort ~ scale.elevation_sq + scale.habitat_sq + scale.remoteness_sq, data = umf[[i]]) #multiple linear effect
  
  o.mod18[[i]] <- occu(~ Effort ~ scale.elevation + scale.habitat + scale.remoteness, + scale.elevation_sq + scale.habitat_sq + scale.remoteness_sq, data = umf[[i]]) #multiple linear&quadratic effect
  
  
  # Select best detection model
  if (d_modsel[[i]]@Full$model[1] == "p(.) psi(.) data") {
    d.mod[[i]] <- d.mod00[[i]]
  } else {
    d.mod[[i]] <- d.mod0[[i]]
  }
  
 
  # Create list for modelselection
  fms[[i]] <-  #modellist_occupancy
    fitList("p(Effort) psi(.) data" = d.mod[[i]],   #to do!: find best nullmodel for each species
            "p(Effort) psi(elevation) data" = o.mod1[[i]],
            "p(Effort) psi(habitat) data" = o.mod2[[i]],
            "p(Effort) psi(remoteness) data" = o.mod3[[i]],
            "p(Effort) psi(elevation + habitat) data" = o.mod4[[i]],
            "p(Effort) psi(elevation + remoteness) data" = o.mod5[[i]],
            "p(Effort) psi(habitat + remoteness) data" = o.mod6[[i]],
            "p(Effort) psi(elevation + habitat + remoteness) data" = o.mod7[[i]],
            
            "p(Effort) psi(elevation + elevation_sq) data" = o.mod8[[i]],
            "p(Effort) psi(habitat + habitat_sq) data" = o.mod9[[i]],
            "p(Effort) psi(remoteness + remoteness_sq) data" = o.mod10[[i]],
            
            "p(Effort) psi(elevation_sq) data" = o.mod11[[i]],
            "p(Effort) psi(habitat_sq) data" = o.mod12[[i]],
            "p(Effort) psi(remoteness_sq) data" = o.mod13[[i]],
            "p(Effort) psi(elevation_sq + habitat_sq) data" = o.mod14[[i]],
            "p(Effort) psi(elevation_sq + remoteness_sq) data" = o.mod15[[i]],
            "p(Effort) psi(habitat_sq + remoteness_sq) data" = o.mod16[[i]],
            "p(Effort) psi(elevation_sq + habitat_sq + remoteness_sq) data" = o.mod17[[i]],
            
            "p(Effort) psi(elevation + habitat + remoteness + elevation_sq + habitat_sq + remoteness_sq) data" = o.mod18[[i]]
            )



  # Modelselection
  modsel[[i]] <- modSel(fms[[i]])
  
  
  # Convert to df
  modsel_df[[i]] <- data.frame(model = modsel[[i]]@Full$model,
                            covariate = gsub("~Effort ~ ", "", modsel[[i]]@Full$formula, ),
                            nPars = modsel[[i]]@Full$nPars,
                            AIC=modsel[[i]]@Full$AIC,
                            delta=modsel[[i]]@Full$delta,
                            AICwt=modsel[[i]]@Full$AICwt,
                            cumltvWt=modsel[[i]]@Full$cumltvWt
                            #p.Effort=modsel[[i]]@Full$`p(Effort)`,
                            #SEp.Effort=modsel[[i]]@Full$`SEp(Effort)`,
                            #p.Int=modsel[[i]]@Full$`p(Int)`,
                            #SEp.Int=modsel[[i]]@Full$`SEp(Int)`,
                            #psi.Int=modsel[[i]]@Full$`psi(Int)`,
                            #SEpsi.Int=modsel[[i]]@Full$`SEpsi(Int)`
  )
  
  
  # Round dataset
  modsel_df_round[[i]] <- data.frame(modsel_df[[i]][, c(1,2)], round(modsel_df[[i]][, -c(1,2)], 3))

  # # Plot data
  # #datatable(modsel_df_round)
  # print(knitr::kable(modsel_df_round[[i]], row.names = FALSE, caption = paste0("Modelselection ", relevantspecies$species[[i]])))
  
}

# assign species names to lists
names(fms) <- names(modsel) <- names(modsel_df) <- names(modsel_df_round) <- names(detHist)

# detection modelselection of SBC
modsel_df_round[43]
DT::datatable(modsel_df_round[[43]])






#### --> save / import R.data ---------------------------------------------------------------------------------------------

# Save whole workspace to working directory (date: 01.09.2022)
save.image("D:/Dateien/Uni/Eagle_Master/Masterthesis/R_Masterthesis_SBC/testing/occupancy/SBC_occupancy_1.9.RData")

# Load workspace back to RStudio
load("D:/Dateien/Uni/Eagle_Master/Masterthesis/R_Masterthesis_SBC/testing/occupancy/all_data.RData")



## 5.6 Response Curves ========================================================================================

### HIER HÄNGTS NOCH ####
# an daten im script angepasst, aber plots wollen nicht

# 
# #> To visualise habitat associations, we can plot response curves for each species, 
# #> based on the models in modelselection tables above (A2.), showing standard errors (darkgrey) 
# #> and 95% confidence intervals (lightgrey).
# 
# 
# # Set path to source code for adapted plotfunction
# source(file.path("D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/Screenforbio_Dopbox_backup/scripts/PlotResponseCurves_uni.R"))
# 
# covplot <- sapply(fms[[43]]@fits[-1], continuous.plot2, modsel = modsel, covariates = covariates_sq %>% select( -matches("scale.")), quiet = TRUE)
# 
# 
# # Set path to source code for adapted plotfunction
# #source(file.path(wd,"/scripts/PlotResponseCurves_uni.R"))
# source(file.path("D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/Screenforbio_Dopbox_backup/scripts/multi-plot_edit_LP.R"))
# 
# tmpdir <- tempdir()
# 
# for (i in 43:43){#1:length(unique(detHist))){
# 
#   # cat("\n")
#   # cat("#####", names(detHist)[i], "  \n")
# 
#   if (names(fms[[i]]@fits) != "~1 ~ 1")  # original: "~Effort ~ 1"
#     #print("true"
# 
#     multiplot.wrapper(model.list = fms[[i]]@fits,
#                               siteCovs_unscaled = covariates %>% select( -matches("scale.")),
#                               modelRanking = modsel[[i]],
#                               occasionLength = occasionLength,
#                               #plotnames.by.rank ,
#                               plotDir = tmpdir,
#                               speciesVector = relevantspecies$species[[i]],
#                               #index_tmp = 1,
#                               addRug = TRUE,
#                               #maximumRankToPlot = 200,
#                               plotModelsBelowNullModel = F,
#                               quiet = T
# )
# 
# 
#   else(
#     #print("nullmodel was bestmodel, so no covariates explain occuence")
#     multiplot.wrapper(model.list = fms[[i]]@fits[2],
#                               siteCovs_unscaled = covariates %>% select( -matches("scale.")),
#                               modelRanking = modsel[[i]],
#                               occasionLength = occasionLength,
#                               #plotnames.by.rank ,
#                               plotDir = tmpdir,
#                               speciesVector = relevantspecies$species[[i]],
#                               #index_tmp = 1,
#                               addRug = TRUE,
#                               #maximumRankToPlot = 200,
#                               plotModelsBelowNullModel = F,
#                               quiet = T
#     )
#     )
#     # cat("   \n")
# }
# 
# 
# 
# 
# # 6. Predictions ==============================================================================================
# 
# # Set path to sourcecode to calculate predictions
# 
# source(file.path("D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/Screenforbio_Dopbox_backup/scripts/predict_manual.R"))
# 
# 
# ## 6.1 Import and scale covariate rasters =========================================================================
# 
# ### Elevation ======================================================================================================
# 
# # Import elevation raster
# elevationraster <- raster("D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/USAID/clippedmasked_covariates/merged_SRTM30m_AOI_NAs_filled_R_int32_highcompress_clipmask.tif")
# 
# # Clip to extent of aoi
# elevationraster <- raster::crop(elevationraster, extent(aoi))
# 
# # Aggregate 30m elevation raster to 200m resolution to match with other rasters
# elevr_agg <- aggregate(elevationraster, fact = (200/30), fun = mean, na.rm = TRUE)
# 
# # Mask raster with studyarea
# elevmaskp <- raster::mask(x=elevr_agg, mask=aoi)
# 
# # Scale elevation raster
# elevationraster_scaled <- (elevmaskp - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "elevation"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "elevation"]
# 
# 
# 
# ### Habitat ======================================================================================================
# 
# # Import LC probability raster
# LCprob <- raster::stack("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Sentinel/final_SVN/RF_prob_td2.8_wsmp10000_spl0.7_scale10-100.tif")
# Hab3raster <- LCprob$Semidry.Dry.Forest
#   
# # Clip to extent of aoi
# #Hab3raster <- raster::crop(Hab3raster, extent(aoi))
# 
# # Aggregate 100m LC raster to 200m resolution to match with other rasters
# Hab3r_agg <- aggregate(Hab3raster, fact = (200/100), fun = mean, na.rm = TRUE)
# 
# # Mask raster with studyarea
# #Hab3maskp <- raster::mask(x=Hab3r_agg, mask=aoi)
# 
# # Scale Hab3 raster
# Hab3raster_scaled <- (Hab3r_agg - attr(covariates_scaled_matrix, "scaled:center")[colnames(covariates_scaled_matrix) == "habitat"] ) /  attr(covariates_scaled_matrix, "scaled:scale")[colnames(covariates_scaled_matrix) == "habitat"]
# 
# 
# 
# ### Remoteness ======================================================================================================
# 
# 
# 
# 
# 
# 
# # 6.2 Create rasterstack ============================================================================
# 
# # rename scaled covariate rasters
# elevr <- elevationraster_scaled
# hab3r <- Hab3raster_scaled
# 
# 
# # resample rasters for raster stack
# #elevr <- resample(elevr, villdensr)
# hab3r <- resample(hab3r, elevr)
# 
# 
# # stack rasters
# predstack <- raster::stack(elevr, hab3r)
# names(predstack) <- c("scale.elevation", "scale.habitat")
# 
# 
# # Make predictions
# 
# 
# prediction_o.mod1 <- lapply(o.mod1, FUN = predict_manual, covariate_raster_stack = predstack)
# 
