library(raster)
library(sf)
library(sp)

#r <- stack("D:/Dateien/Uni/Eagle_Master/Masterthesis/Data/Landsat/L8_median_2021_months_1-12_annamitespecies_200mscale.tif")
#r <- raster("D:/Dateien/Dropbox (ScreenForBio)/Projects/Luisa_Pflumm/Input_data/Spatial_layers/Annamites_Ecoregion-Speciesrange/elevation/SRTM 1 Arc-Second Global/merged_SRTM30m_AOI_NAs_filled_R_int32_highcompress.tif")
#r <- raster("D:/Dateien/Dropbox (ScreenForBio)/Projects/Luisa_Pflumm/Input_data/Spatial_layers/Annamites_Ecoregion-Speciesrange/terrain/TPI_7x7.tif")
r <- raster("D:/Dateien/Dropbox (ScreenForBio)/Projects/Luisa_Pflumm/Input_data/Spatial_layers/Annamites_Ecoregion-Speciesrange/terrain/TRI_7x7.tif")

#aoi <- st_read("D:/Dateien/Dropbox (ScreenForBio)/Projects/Luisa_Pflumm/Input_data/Spatial_layers/Annamites_Ecoregion-Speciesrange/ecoregion-specieranges/border_annamites_species_aoi.shp")
p <- shapefile("D:/Dateien/Dropbox (ScreenForBio)/Projects/Luisa_Pflumm/Input_data/Spatial_layers/Annamites_Ecoregion-Speciesrange/ecoregion-specieranges/border_annamites_species_aoi.shp")

clip <- raster::crop(r, p)
plot(clip)

#mask <- raster::mask(r$NDMI, p)
mask <- raster::mask(clip, p)
plot(mask)


writeRaster(clip, "D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/USAID/clippedmasked_covariates/TRI_7x7_clip.tif")



#####################################
sites <- st_read("D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/USAID/sites_USAID_editLP.shp")
x <- st_make_valid(x)
st_write(x, "D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/USAID/sites_USAID_editLP_valid.shp")
valid <- st_read("D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/USAID/sites_USAID_editLP_valid.shp")


