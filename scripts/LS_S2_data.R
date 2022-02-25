#################################
### M A S T E R   T H E S I S ###
#################################

## load libraries
library(raster)
library(ggplot2)


## define working directories
wd <- "D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/"
wd1 <- "D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Sentinel/cloudprob_65/"
#wd2 <- "D:/Dateien/Dropbox (ScreenForBio)/Projects/SBC/"


## load files
files <- list.files(file.path(wd1), pattern = ".tif")
files

jan <- stack(file.path(wd1, "S2_01_2021_nuichua.tif"))
# feb <- stack(file.path(wd1, "S2_02_2021_nuichua.tif"))
# mar <- stack(file.path(wd1, "S2_03_2021_nuichua.tif"))
# apr <- stack(file.path(wd1, "S2_04_2021_nuichua.tif"))
# may <- stack(file.path(wd1, "S2_05_2021_nuichua.tif"))
# jun <- stack(file.path(wd1, "S2_06_2021_nuichua.tif"))
# jul <- stack(file.path(wd1, "S2_07_2021_nuichua.tif"))
# aug <- stack(file.path(wd1, "S2_08_2021_nuichua.tif"))
# sep <- stack(file.path(wd1, "S2_09_2021_nuichua.tif"))
# oct <- stack(file.path(wd1, "S2_10_2021_nuichua.tif"))
# nov <- stack(file.path(wd1, "S2_11_2021_nuichua.tif"))
# dec <- stack(file.path(wd1, "S2_12_2021_nuichua.tif"))


#################
### PLOT DATA ###
#################

# convert to a df for plotting in two steps,
# First, to a SpatialPointsDataFrame
pts <- rasterToPoints(jan, spatial = TRUE)

# Then to a 'conventional' dataframe
df  <- data.frame(pts)
#rm(pts, jan)



ggplot() +
  geom_raster(data = df , aes(x = x, y = y, fill = NDVI)) + 
  ggtitle("NDVI S2 January 2021") + 
  scale_fill_manual(values = terrain.colors(3))


plot(jan$NDVI)


ggplot(df) +
  geom_raster(aes(x = x, y = y, fill = NDVI))+
  scale_fill_viridis_c() +
  facet_wrap(~NDVI) +
  coord_quickmap()+
  ggtitle("Sentinel 2 Loch tay, raster plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))























