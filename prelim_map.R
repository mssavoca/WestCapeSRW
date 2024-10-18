###########################################
# Map of study sites and relevant locations
###########################################


# devtools::install_github("MikkoVihtakari/ggOceanMapsData") # required by ggOceanMaps
# devtools::install_github("MikkoVihtakari/ggOceanMaps")
# 
# install.packages("ggOceanMapsData", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org"))
# remotes::install_github("MikkoVihtakari/ggOceanMaps")

library(raster)
library(sf)

library(rgdal)
library(ggOceanMapsData)
library(ggOceanMaps)
library(ggspatial)


country_borders <- st_read("DoS_LSIB_v11_3_19Dec2023.shp")


dt <- data.frame(lon = c(9, 9, 27, 27), lat = c(-20, -20, -37, -37))

BoxFieldwork <- data.frame(
  lon = c(17.7, 17.7, 18.3, 18.3), 
  lat = c(-33.6,-32.3, -32.3,  -33.6)
)

BoxWalvisBG <- data.frame(
  lon = c(14.1, 13.8, 14.35, 14.6), 
  lat = c(-23.4, -21.7, -21.7, -23.4)
)


BoxSABG <- data.frame(
  lon = c(18.8, 18.8, 24.9, 24.9), 
  lat = c(-35, -34.4, -33.9, -34.5)
)


SRW_regional_map <- basemap(data = dt, 
                            bathymetry = TRUE, 
                            bathy.style = "rcb"
                            ) + 
  geom_sf(data = country_borders, color = "black") +
  xlim(10, 27) +
  ylim(-37,-21) +
  coord_sf(expand = F) +
  ggspatial::geom_spatial_polygon(
    data = BoxFieldwork, 
    aes(x = lon, y = lat), 
    fill = NA, color = "firebrick3", size = 1.25) +
  ggspatial::geom_spatial_polygon(
    data = BoxWalvisBG, 
    aes(x = lon, y = lat), 
    fill = NA, color = "darkviolet", size = 1.25) +
  ggspatial::geom_spatial_polygon(
    data = BoxSABG, 
    aes(x = lon, y = lat), 
    fill = NA, color = "darkviolet", size = 1.25) +
  # annotate("text", x = c(15.85,21.25), y = c(-22.8,-33.8), 
  #          label = c("Walvis Bay,\nNamibia","Cape Town, South Africa"), 
  #          size = 4) +
  annotation_scale(location = "br", line_width = 2, text_cex = 1) + 
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1.75, "cm"),
                         width = unit(1.75, "cm")) +
  theme(text = element_text(size = 16))
SRW_regional_map




