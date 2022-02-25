### Load libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(sf)


### Make violin & boxplot plots

## Example dataset from https://www.r-graph-gallery.com/violin_and_boxplot_ggplot2.html

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# sample size
sample_size = data %>% group_by(name) %>% summarize(num=n())

# Plot
data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=value, fill=name)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("")


### Read my extracted LS TS dataset

gps_ct <- st_read("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Landsat/extracted_pixelvalues/nuichua/2.download/shps_orig/L8_extract_values_from_locations_GPS_stations_nuichua_nobuffer.shp")
# gps_ct_100m <- st_read("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Landsat/extracted_pixelvalues/nuichua/2.download/shps_orig/L8_extract_values_from_locations_GPS_stations_nuichua_100mbuffer.shp")
# nuichua_pixelcoords <- st_read("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Landsat/extracted_pixelvalues/nuichua/2.download/shps_orig/L8_extract_values_from_pixelcoords_nuichua_nobuffer.shp")
# tejas_pixelcoords <- st_read("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Landsat/extracted_pixelvalues/nuichua/2.download/shps_orig/L8_extract_values_from_pixelcoords_of_trainingpolygons_tejas_nuichua_nobuffer.shp")
# tejas_pixelcoords_50m <- st_read("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Landsat/extracted_pixelvalues/nuichua/2.download/shps_orig/L8_extract_values_from_pixelcoords_of_trainingspolygons_tejas_nuichua_50mbuffer.shp")

plot(gps_ct$geometry)


# sample size
sample_size_gps_ct = gps_ct %>% group_by(plot_id) %>% summarize(num=n())
gps_ct <- data.frame(gps_ct)


# example plot id 1
#plotid = gps_ct[gps_ct$plot_id == 3,]
plotid <- filter(gps_ct, plot_id <= 10)
sample_size_plotid = plotid %>% group_by(plot_id) %>% summarize(num=n())

# violin around boxplot Plot
plotid %>%
  left_join(sample_size_plotid) %>%
  mutate(myaxis = paste0("plot id ", plot_id, "\n", "n=", num, "\n", "habitat id: ", hb_id)) %>%
  ggplot( aes(x=myaxis, y=NDVI, fill=plot_id)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis() +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("NDVI value distribution per plot") +
  xlab("") +
  facet_wrap(~ plot_id)



## tejas 111 training polygons

tejas <- st_read("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Landsat/extracted_pixelvalues/nuichua/2.download/shps_orig/L8_extract_values_from_locations_trainingpolygons_tejas_nuichua_nobuffer.shp")

plot(tejas$geometry)


# sample size
sample_size_tejas = tejas %>% group_by(plot_id) %>% summarize(num=n())
tejas <- data.frame(tejas)


# example plot id 1
#plotid = gps_ct[gps_ct$plot_id == 3,]
plotid <- filter(tejas, plot_id <= 10)
sample_size_tejas = plotid %>% group_by(plot_id) %>% summarize(num=n())

# violin around boxplot Plot
plotid %>%
  left_join(sample_size_tejas) %>%
  mutate(myaxis = paste0("plot id ", plot_id, "\n", "n=", num, "\n", "class id: ", class_id)) %>%
  ggplot( aes(x=myaxis, y=NDVI, fill=plot_id)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis() +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("NDVI value distribution per plot") +
  xlab("")
 
