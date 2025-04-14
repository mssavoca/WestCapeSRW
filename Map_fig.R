###########################################
# Map of study sites and relevant locations
###########################################


devtools::install_github("MikkoVihtakari/ggOceanMapsData") # required by ggOceanMaps
devtools::install_github("MikkoVihtakari/ggOceanMaps")
# 
install.packages("ggOceanMapsData", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org"))
remotes::install_github("MikkoVihtakari/ggOceanMaps")

library(raster)
library(sf)

library(rgdal)
library(ggOceanMapsData)
library(ggOceanMaps)
library(ggspatial)


country_borders <- st_read("DoS_LSIB_v11_3_19Dec2023.shp")


dt <- data.frame(lon = c(9, 9, 27, 27), lat = c(-20, -20, -37, -37))

BoxFieldwork <- data.frame(
  lon = c(17.5, 17.5, 18.6, 18.6),
  lat = c(-33.8,-32.3, -32.3,  -33.8)
)

BoxWalvisBG <- data.frame(
  lon = c(14.1, 13.8, 14.35, 14.6), 
  lat = c(-23.4, -21.7, -21.7, -23.4)
)


BoxSABG <- data.frame(
  lon = c(18.8, 18.8, 24.9, 24.9), 
  lat = c(-35, -34.4, -33.9, -34.5)
)


SRW_regional_map <- basemap(data = dt 
                            #bathymetry = TRUE, 
                            #bathy.style = "rcb"
                            ) + 
  geom_sf(data = country_borders, color = "black") +
  xlim(14, 25) +
  ylim(-37,-25) +
  coord_sf(expand = F) +
  ggspatial::geom_spatial_polygon(
    data = BoxFieldwork,
    aes(x = lon, y = lat),
    fill = NA, color = "black", size = 1.25) +
  # ggspatial::geom_spatial_polygon(
  #   data = BoxWalvisBG, 
  #   aes(x = lon, y = lat), 
  #   fill = NA, color = "darkviolet", size = 1.25) +
  # ggspatial::geom_spatial_polygon(
  #   data = BoxSABG, 
  #   aes(x = lon, y = lat), 
  #   fill = NA, color = "darkviolet", size = 1.25) +
  # annotate("text", x = c(15.85,21.25), y = c(-22.8,-33.8), 
  #          label = c("Walvis Bay,\nNamibia","Cape Town, South Africa"), 
  #          size = 4) +
  annotation_scale(location = "br", line_width = 3, text_cex = 1.5) + 
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1.75, "cm"),
                         width = unit(1.75, "cm")) +
  theme(text = element_text(size = 28))
SRW_regional_map

ggsave("Region_map_bw.pdf", width = 6, height = 8)


dt_fieldwork =  data.frame(lon = c(17.3, 17.3, 18.35, 18.35), lat = c(-33.7,-32.4, -32.4,  -33.7))

whale_encouters = read.csv("encounters.csv")
prey_stations = read.csv("stations.csv")
prey_target = read.csv("target.csv")
tagging_loc = read_xlsx("Tag_Guide_SRW.xlsx")

# Standardize column names
whale_encouters <- whale_encouters %>% rename(lon = long)
prey_stations <- prey_stations %>% rename(lon = long)
tagging_loc <- tagging_loc %>% rename(lon = Long_On, lat = Lat_On)

# Plot with consistent 'lon' and 'lat'
Fieldwork_map <- basemap(data = dt_fieldwork) +
  geom_point(data = whale_encouters, 
             aes(x = lon, y = lat, color = period), size = 3.5, alpha = 0.7) +
  geom_point(data = prey_target, 
             aes(x = lon, y = lat, color = period), shape = 2, size = 3.5) +
  geom_point(data = prey_stations, 
             aes(x = lon, y = lat), shape = 3, color = "darkred", size = 3.5) +
  geom_point(data = tagging_loc, 
             aes(x = lon, y = lat), shape = 23, color = "blue", size = 3.5) +
  scale_color_manual(values = c("hist" = "orange2", "new" = "darkgreen", "old" = "orange2")) +
  labs(color = "Period") +
  annotation_scale(location = "br", line_width = 4, text_cex = 1.5) +
  scale_x_continuous(
    breaks = seq(floor(min(dt_fieldwork$lon, na.rm = TRUE)),
                 ceiling(max(dt_fieldwork$lon, na.rm = TRUE)), by = 0.5)
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(dt_fieldwork$lat, na.rm = TRUE)),
                 ceiling(max(dt_fieldwork$lat, na.rm = TRUE)), by = 0.5)
  ) +
  theme_bw(base_size = 22) +
  theme(legend.position = "none")

Fieldwork_map


ggsave("Fieldwork_map_v3.pdf", width = 6, height = 8)



