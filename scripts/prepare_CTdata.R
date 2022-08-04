#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# CAMERA TRAP DATA FOR OCCUPANCY ANALYSIS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#     MASTER THESIS     LUISA PFLUMM      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(lubridate)
library(mapview)
library(sf)
library(camtrapR)
library(tidyr)

path <- "D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/CT"
#path <- "C:/Arbeit/Luisa/shareJN/CTdata"   

# 1. Quy ============================================================================================

## Krong Trai, Cam An Bac, Suoi Tien, Hon Heo, Nui Chua NP, So Pai, Deo Ca, Song Hinh ============ 

### 1.1 CT table ======================================================================================

# Load CTtable 
#quy_cttable <- read.csv("C:/Users/s347553/Documents/Masterthesis/Data/CT/master_ct_table.csv", sep = ";")
quy_cttable <- read.csv(file.path(path, "Quy/master_ct_table.csv"), sep = ";")
CTtableQ <- quy_cttable

# # This will give you the number of duplicates:
# dim(CTtableQ[duplicated(CTtableQ$station),])[1]
# 
# # This will give you duplicate rows:
# x <- CTtableQ[duplicated(CTtableQ$station),]



#### Adjust CTtable ===============================================================================
#-> to fit camtrapR::cameraOperation()

# long/lat have comma, exchange with point to create sf object
CTtableQ$long <- sapply(CTtableQ$long, function(x) as.numeric(gsub(",", ".", x)))
CTtableQ$lat <- sapply(CTtableQ$lat, function(x) as.numeric(gsub(",", ".", x)))

CTtableQ$UTM_X <- sapply(CTtableQ$long, function(x) as.numeric(gsub(",", ".", x)))
CTtableQ$UTM_Y <- sapply(CTtableQ$lat, function(x) as.numeric(gsub(",", ".", x)))


# adjust and convert to date-time format
CTtableQ$date_setting <- parse_date_time(CTtableQ$date_setting, "dmy")

CTtableQ$date_retrieval <- gsub('Mrz', 'Mar', CTtableQ$date_retrieval)
CTtableQ$date_retrieval <- gsub('Okt', 'Oct', CTtableQ$date_retrieval)
CTtableQ$date_retrieval <- gsub('2020-10-22', '22. Oct 20', CTtableQ$date_retrieval)
CTtableQ$date_retrieval <- parse_date_time(CTtableQ$date_retrieval, "dmy")

CTtableQ$Problem1_from <- gsub('Mrz', 'Mar', CTtableQ$Problem1_from)
CTtableQ$Problem1_from <- gsub('Mai', 'May', CTtableQ$Problem1_from)
CTtableQ$Problem1_from <- gsub('Okt', 'Oct', CTtableQ$Problem1_from)
CTtableQ$Problem1_from <- gsub('Dez', 'Dec', CTtableQ$Problem1_from)

CTtableQ$Problem1_from[290] <- "09-Nov-17"  # Error: nct002 : Problem begins before setup, so I set setup date == problem from --> need to clarify!
CTtableQ$Problem1_from[68] <- "29/11/20"  # row68: Problem ends before it starts.
CTtableQ$Problem1_from[182] <- "17/12/19"  # row182: Problem ends before it starts.
CTtableQ$Problem1_from <- parse_date_time(CTtableQ$Problem1_from, "dmy")

CTtableQ$Problem1_to <- gsub('Mrz', 'Mar', CTtableQ$Problem1_to)
CTtableQ$Problem1_to <- gsub('Okt', 'Oct', CTtableQ$Problem1_to)
CTtableQ$Problem1_to <- parse_date_time(CTtableQ$Problem1_to, "dmy")


# Compare in df
# df <- data.frame(station = quy_cttable$station,camera = quy_cttable$camera,
#                  date_setting_orig = quy_cttable$date_setting,date_setting = CTtableQ$date_setting,
#                  date_retrieval_orig = quy_cttable$date_retrieval, date_retrieval = CTtableQ$date_retrieval,
#                  Problem1_from_orig = quy_cttable$Problem1_from, Problem1_from = CTtableQ$Problem1_from,
#                  Problem1_to_orig = quy_cttable$Problem1_to,Problem1_to = CTtableQ$Problem1_to)




#### cameraOperation() ==================================================================================

camop_matrixQ <- cameraOperation(CTtable = CTtableQ,
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



#### Plot CT stations ======================================================================================

# Create sf object and reproject
stationsQ <- st_as_sf(CTtableQ, coords = c("long", "lat"),crs = 4326)

# Load the aoi shapefile
#aoi <- st_read("C:/Arbeit/Luisa/shareJN/SVN_SBC_provinces_final.shp")
aoi <- st_read("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/SVN_SBC_provinces_final.shp")
aoi <- st_cast(aoi$geometry, "MULTILINESTRING")

# Plot stations in aoi
(mq <- mapview(aoi, color="black") + mapview(stationsQ, zcol="site", layer.name = "stations by Quy"))



### 1.2 RecordTable ====================================================================================

# Load Recordtable from Quy  to fit camtrapR::surveyReport()
recTable_Quy <- read.csv(file.path(path, "Quy/master_record_table_60mins_new.csv"))
recTableQ <- recTable_Quy

#### Adjust recTable ===============================================================================
#-> to fit camtrapR::surveyReport()

# convert to date-time format
recTableQ$DateTimeOriginal <- parse_date_time(recTableQ$DateTimeOriginal, "mdy HM") 



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> ERROR in surveyReport(recordTable = recTableQ, CTtable = CTtableQ, camOp = camop_matrix,  : 
#> Not all values of stationCol in recordTable are matched by values of stationCol in CTtable
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# check unique stations in CT table
ct <- data.frame(station_CT = unique(CTtableQ$station))
ct <- data.frame(ct[order(ct$station_CT),])
colnames(ct) <- "station_CT"
dim(ct)

# check unique stations in record table
rec <- data.frame(station_rec = unique(recTableQ$station))
dim(rec)
rec[235:252,] <- NA
rec <- data.frame(rec[order(rec$station_rec),])
colnames(rec) <- "station_rec"


# find common stations
stationsdf <- cbind(ct,rec)
common <- intersect(stationsdf$station_CT, stationsdf$station_rec)

# select only rows with common stations
CTtableQ2 <- CTtableQ[CTtableQ$station %in% common, ] 
recTableQ2 <- recTableQ[recTableQ$station %in% common, ] 


CTtableQ2$date_setting <- as.Date(CTtableQ2$date_setting)
CTtableQ2$date_retrieval <- as.Date(CTtableQ2$date_retrieval)
recTableQ2$DateTimeOriginal <- as_datetime(recTableQ2$DateTimeOriginal)

# set same colnames in CTtable and recTable
names(recTableQ2)[names(recTableQ2) == 'Camera'] <- 'camera'


#### surveyReport() ======================================================================================

# calculate camop matrix with 'reduced' CTtable (matching station cols with record table)
camop_matrixQ2 <- cameraOperation(CTtable = CTtableQ2,
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

# surveyReport() now with reduced datasets
reportTestQ2 <- surveyReport( recordTable          = recTableQ2,
                              CTtable              = CTtableQ2,
                              camOp                = camop_matrixQ2,
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


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Error in as.POSIXlt.character(x, tz, ...) : 
#> Zeichenkette ist nicht in einem eindeutigen Standardformat
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

str(CTtableQ2$date_setting)
str(CTtableQ2$date_retrieval)
str(CTtableQ2$Problem1_from)
str(CTtableQ2$Problem1_to)
str(recTableQ2$DateTimeOriginal)


# --> Link to camtrapR vignette 1: https://jniedballa.github.io/camtrapR/articles/camtrapr1.html
...

#### species activity plots 
...


#### detectionHistory() ================================================================================

timeZone <- "Asia/Ho_Chi_Minh" # change the time zone based on your study area
occasionLength <- 15 # change the occasion length based on your study

detHist <- detectionHistory(recordTable = recTableQ,
                            species = "Silver-backed chevrotain",
                            camOp = camop_matrixQ_red,
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

detHist$detection_history
detHist$effort


### Export ====================================================================================================

# # Export corrected CTtable
# write.csv(CTtableQ2, file = file.path(path, "Quy/corrected_CTtable_Quy_editLP_shareJN.csv"))
# write.csv(recTableQ2, file = file.path(path, "Quy/corrected_recTable_Quy_editLP_shareJN.csv"))




# 2. An ======================================================================================================

## 2.1 Cat Tien -------------------------------------------------------------------------------------

### Adjust CTtable to fit camtrapR::cameraOperation() ###

# Import CTtable
An_cattien_cttable <- read.csv(file.path(path, "An/Cat Tien data_An.csv"), sep = ";")

# long/lat have comma, exchange with point to create sf object
An_cattien_cttable$long <- sapply(An_cattien_cttable$long, function(x) as.numeric(gsub(",", ".", x)))
An_cattien_cttable$lat <- sapply(An_cattien_cttable$lat, function(x) as.numeric(gsub(",", ".", x)))

# select only valid rows
CTtableCT <- An_cattien_cttable[1:91,]

# set right date-time format
CTtableCT$setting <- parse_date_time(CTtableCT$setting, "dmy")
CTtableCT$retrieval <- parse_date_time(CTtableCT$retrieval, "dmy")
CTtableCT$problem1_start <- parse_date_time(CTtableCT$problem1_start, "dmy")
CTtableCT$problem1_end <- parse_date_time(CTtableCT$problem1_end, "dmy")
CTtableCT$problem2_start <- parse_date_time(CTtableCT$problem2_start, "dmy")
CTtableCT$problem2_end <- parse_date_time(CTtableCT$problem2_end, "dmy")
CTtableCT$problem3_start <- parse_date_time(CTtableCT$problem3_start, "dmy")
CTtableCT$problem3_end <- parse_date_time(CTtableCT$problem3_end, "dmy")
CTtableCT$problem4_start <- parse_date_time(CTtableCT$problem4_start, "dmy")
CTtableCT$problem4_end <- parse_date_time(CTtableCT$problem4_end, "dmy")
CTtableCT$problem5_start <- parse_date_time(CTtableCT$problem5_start, "dmy")
CTtableCT$problem5_end <- parse_date_time(CTtableCT$problem5_end, "dmy")

# remove NAs in setting/retrieval
CTtableCT <- CTtableCT %>% drop_na(setting)

# correct hasProblems-colnames
colnames(CTtableCT) <- gsub('problem', 'Problem', colnames(CTtableCT))
colnames(CTtableCT) <- gsub('start', 'from', colnames(CTtableCT))
colnames(CTtableCT) <- gsub('end', 'to', colnames(CTtableCT))

names(CTtableCT)[names(CTtableCT) == 'setting'] <- 'date_setting'
names(CTtableCT)[names(CTtableCT) == 'retrieval'] <- 'date_retrieval'


## adjust specific errors
# Error: ctpa_a : Problem ends after retrieval
CTtableCT[CTtableCT$station == "ctpa_a" & CTtableCT$camera == "ct11",]$retrieval <- as.Date("2012-07-29")
#Error: ctp33r01 : Problem ends after retrieval
CTtableCT[CTtableCT$station == "ctp33r01" & CTtableCT$camera == "ct01",]$retrieval <- as.Date("2013-11-14")
# Error: ctp27d11 : problem intervals are not within interval from setup to retrieval
CTtableCT[CTtableCT$station == "ctp27d11" & CTtableCT$camera == "ct01",]$Problem2_from <- NA
CTtableCT[CTtableCT$station == "ctp27d11" & CTtableCT$camera == "ct01",]$Problem2_to <- NA




### cameraOperation() ###

camop_matrixCT <- cameraOperation(CTtable = CTtableCT,
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





### Plot CT stations ###

# Create sf object and reproject
stationsCT <- st_as_sf(CTtableCT, coords = c("long", "lat"),crs = 4326)

# aoi <- st_read("C:/Arbeit/Luisa/shareJN/SVN_SBC_provinces_final.shp")
# aoi <- st_cast(aoi$geometry, "MULTILINESTRING")

# Plot stations in aoi
(mct <- mapview(aoi, color="black") + mapview(stationsCT, layer.name = "stations - cat tien"))





## 2.2 Bidoup  -------------------------------------------------------------------------------------

### Adjust CTtable to fit camtrapR::cameraOperation() ###

# Import CTtable
An_bidoup_cttable <- read.csv(file.path(path, "An/CTtable_bidoup_v3_camera.csv"), sep = ";")
An_bid_gen <- read.csv(file.path(path, "An/CTtable_bidoup_v3_general.csv"), sep = ";")
An_bid_hab <- read.csv(file.path(path, "An/CTtable_bidoup_v3_habitat.csv"), sep = ";")
An_bid_ll <- read.csv(file.path(path, "An/CTtable_bidoup_v3_leaflitter.csv"), sep = ";")

# long/lat have comma, exchange with point to create sf object
An_bidoup_cttable$camera_lat <- sapply(An_bidoup_cttable$camera_lat, function(x) as.numeric(gsub(",", ".", x)))
An_bidoup_cttable$camera_long <- sapply(An_bidoup_cttable$camera_long, function(x) as.numeric(gsub(",", ".", x)))

CTtableB <- An_bidoup_cttable

# set right date-time format
CTtableB$date_setting <- as.Date(CTtableB$date_setting)
CTtableB$date_retrieval <- as.Date(CTtableB$date_retrieval)




### cameraOperation() ###

camop_matrixB <- cameraOperation(CTtable = CTtableB,
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



### Plot CT stations ###

# Create sf object and reproject
stationsB <- st_as_sf(CTtableB, coords = c("camera_long", "camera_lat"),crs = 4326)

# aoi <- st_read("C:/Arbeit/Luisa/shareJN/SVN_SBC_provinces_final.shp")
# aoi <- st_cast(aoi$geometry, "MULTILINESTRING")

# Plot stations in aoi
(mb <- mapview(aoi, color="black") + mapview(stationsB, layer.name = "stations - bidoup", col.regions = "orange"))





## 2.3 Phuoc Binh  ------------------------------------------------------------------------------------- 

### Adjust CTtable to fit camtrapR::cameraOperation() ###

# Import CTtable
An_pb_cttable <- read.csv(file.path(path, "An/CTtable_pb-dd_v2_camera.csv"), sep = ";")
An_hab <- read.csv(file.path(path, "An/CTtable_pb-dd_v2_habitat.csv"), sep = ";")
An_stat <- read.csv(file.path(path, "An/CTtable_pb-dd_v2_station.csv"), sep = ";")

# long/lat have comma, exchange with point to create sf object
An_pb_cttable$lat_station <- sapply(An_pb_cttable$lat_station, function(x) as.numeric(gsub(",", ".", x)))
An_pb_cttable$lon_station <- sapply(An_pb_cttable$lon_station, function(x) as.numeric(gsub(",", ".", x)))

CTtablePB <- An_pb_cttable

# set right date-time format
CTtablePB$date_setting_cam <- as.Date(CTtablePB$date_setting_cam)
CTtablePB$date_retrieval <- as.Date(CTtablePB$date_retrieval)
CTtablePB$Problem1_from <- parse_date_time(CTtablePB$Problem1_from, "ymd")
CTtablePB$Problem1_to <- parse_date_time(CTtablePB$Problem1_to, "ymd")

names(CTtablePB)[names(CTtablePB) == 'Station'] <- 'station'
names(CTtablePB)[names(CTtablePB) == 'date_setting_cam'] <- 'date_setting'



### cameraOperation() ###

camop_matrixPB <- cameraOperation(CTtable = CTtablePB,
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



### Plot CT stations ###

# Create sf object and reproject
stationsPB <- st_as_sf(CTtablePB, coords = c("lon_station", "lat_station"),crs = 4326)

# aoi <- st_read("C:/Arbeit/Luisa/shareJN/SVN_SBC_provinces_final.shp")
# aoi <- st_cast(aoi$geometry, "MULTILINESTRING")

# Plot stations in aoi
(mpb <- mapview(aoi, color="black") + mapview(stationsPB, layer.name = "stations - phuoc binh", col.regions = "purple"))







## 2.4 Kon Plong -------------------------------------------------------------------------------------

### Adjust CTtable to fit camtrapR::cameraOperation() ###

# Import CTtable
An_kp_cttable <- read.csv(file.path(path, "An/KonPlong_2019-20_camera_trap_deployment_v3.csv"), sep = ";", fileEncoding = "latin1")

# select only valid rows by removing NAs in camera
CTtableKP <- An_kp_cttable %>% drop_na(Camera)

# set column names
names(CTtableKP)[names(CTtableKP) == 'Date.setup'] <- 'date_setting'
names(CTtableKP)[names(CTtableKP) == 'Date.retrieved'] <- 'date_retrieval'

# set right date-time format
CTtableKP$date_setting <- parse_date_time(CTtableKP$date_setting, "dmy")
CTtableKP$date_retrieval <- parse_date_time(CTtableKP$date_retrieval, "dmy")


# --> Any problem from - to??



### cameraOperation() ###

camop_matrixKP <- cameraOperation(CTtable = CTtableKP,
                                stationCol ="Station",
                                cameraCol = "Camera",
                                setupCol = "date_setting",
                                retrievalCol ="date_retrieval",
                                hasProblems = FALSE,  # set to FALSE, but is that real?
                                byCamera = FALSE,
                                allCamsOn = FALSE,
                                camerasIndependent = TRUE,
                                writecsv = FALSE,
                                dateFormat = "ymd"
                                #outDir = outDir,
)




### Plot CT stations ###

# Create sf object and reproject
stationsKP <- st_as_sf(CTtableKP, coords = c("Station.X..UTM.49N....actual", "Station.Y..UTM.49N....actual"),crs = 32649)

# aoi <- st_read("C:/Arbeit/Luisa/shareJN/SVN_SBC_provinces_final.shp")
# aoi <- st_cast(aoi$geometry, "MULTILINESTRING")

# Plot stations in aoi
(mkp <- mapview(aoi, color="black") + mapview(stationsKP, layer.name = "stations - Kon Plong", col.regions = "darkgreen"))




### Adjust RecordTable to fit camtrapR::surveyReport() ###

# Load Recordtable
An_kp_rectable <- read.csv("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/CT/An/recordTable_03072020_KPL.csv", sep = ",")

recTableKP <- An_kp_rectable

# set names
names(recTableKP)[names(recTableKP) == 'station'] <- 'Station'


# --> some rows in camera column have species as names!!



### surveyReport() ###

reportTestKP <- surveyReport( recordTable          = recTableKP,
                            CTtable              = CTtableKP,
                            camOp                = camop_matrixKP,
                            speciesCol           = "Species",
                            stationCol           = "Station",
                            cameraCol            = "Camera",
                            setupCol             = "date_setting",
                            retrievalCol         = "date_retrieval",
                            CTDateFormat         = "ymd",
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "ymd HMS",#"%Y-%m-%d %H:%M:%S",
                            #CTHasProblems        = TRUE,
                            makezip              = FALSE)                  # set TRUE if you want Zip-file



# SUMMARY =============================================================================

# Stationplots
mq + mct + mb + mpb + mkp

# available CTtables
head(CTtableQ2)    # by Quy for Krong Trai, Cam An Bac, Suoi Tien, Hon Heo, Nui Chua NP, So Pai, Deo Ca, Song Hinh
head(CTtableCT)    # by An for Cat Tien
head(CTtableB)     # by An for Bidoup
head(CTtablePB)    # by An for Phuoc Binh
head(CTtableKP)    # by An for Kon Plong

# available recTables
head(recTableQ2)   # by Quy for Krong Trai, Cam An Bac, Suoi Tien, Hon Heo, Nui Chua NP, So Pai, Deo Ca, Song Hinh
head(recTableKP)   # by An for Kon Plong
