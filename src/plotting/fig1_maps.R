#######################
#  maps for Figure 1  #
#######################

# script by Eva Lieungh
# started 2023-06-06

setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/")

library(rnaturalearth) 
library(tidyverse)
library(ggplot2)
library(mapdata)
library(raster)
library(sf)

theme_set(theme_bw()) # set ggplot theme

# set coordinates for sites
site_lat <- c(61.0243, 60.8231, 60.8328, 60.9335)
site_lon <- c(8.12343, 7.27596, 7.17561, 6.41504)
site_name <- c("Ulvehaugen", 
               "Lavisdalen", 
               "Gudmedalen", 
               "Skjellingahaugen")

# 1. plot study region in N Europe
# -------------------------------------------------------
# set coordinates for study region
xfloor = site_lon - 0.005
xceil = site_lon + 0.005
yfloor = site_lat - 0.005
yceil = site_lat + 0.005

# get extent
print(c(min(xfloor), 
        max(xceil), 
        min(yfloor), 
        max(yceil)))
  # 6.41004  8.12843 60.81810 61.02930

# download background map 
europe <- ne_countries(scale ='medium', 
                       continent = 'Europe', 
                       returnclass = 'sf')

# plot map with box marking region
fig1_europe <- ggplot(data = europe) +
  geom_sf(fill = "darkgrey") +
  coord_sf(xlim = c(-5, 25), ylim = c(47, 65)) +
  xlab('') + ylab('') +
  # mark region with broader extent coordinates
  geom_rect(aes(
    xmin = 6.3,
    xmax = 8.2,
    ymin = 60.6,
    ymax = 61.3
  ),
  fill = "red")

ggsave(fig1_europe,
       filename = "results/figures/fig1_map_europe.pdf",
       device = "pdf")
ggsave(fig1_europe,
       filename = "results/figures/fig1_map_europe.png",
       device = "png",
       height = 500,
       width = 500,
       units = "px",
       dpi = 150)


# 2. plot sites in regional map with terrain
# -----------------------------------------------
# define sites and some info for labels
site_info <- c("Ulvehaugen\n1226 mm", 
               "Lavisdalen\n1561 mm", 
               "Gudmedalen\n2130 mm", 
               "Skjellingahaugen\n3402 mm")

pts_sites <- data.frame(site = site_name,
                        site_info = site_info,
                        longitude = site_lon,
                        latitude = site_lat
                        )

# download background map 
norway <- ne_countries(scale ='large', 
                       country = "Norway",
                       returnclass = 'sf')
# set some axis parameters
tick_positions <- c(5.7, 6, 6.5, 7, 7.5, 8, 8.5)
tick_label_breaks <- c(6, 7, 8)
  
# plot
(fig1_region <-
    # plot background map
    ggplot(data = norway) +
    geom_sf(fill = "darkgrey") +
    # zoom in on region
    coord_sf(xlim = c(6, 8.4),
             ylim = c(60.3, 61.5)) +
    # scale_x_continuous(
    #   breaks = tick_label_breaks,
    #   sec.axis = sec_axis( ~ ., breaks = tick_positions, labels = "")
    # ) +
    xlab('') + ylab('') +
  # add site points
    geom_point(
      data = pts_sites,
      aes(x = longitude,
          y = latitude,
          group = site),
      pch = 21,
      color = "black",
      fill = "red"
    ) +
    # add site info next to points
    ggrepel::geom_text_repel(
      data = pts_sites,
      aes(x = longitude,
          y = latitude),
      label = site_info
    )
)

ggsave(fig1_region,
       filename = "results/figures/fig1_map_region.pdf",
       device = "pdf")
ggsave(fig1_region,
       filename = "results/figures/fig1_map_region.png",
       device = "png",
       height = 500,
       width = 500,
       units = "px",
       dpi = 150)


# ---------------
# alternative with raster digital terrain model, 
# currently only working for the low resolution option

# define region with rounded site extent coordinates
region <- as(extent(6.3,
                    8.2,
                    60.6,
                    61.3), 
             'SpatialPolygons')

# get elevation (digital terrain model) data and crop to region
dtm_Norway <-
  getData('worldclim', var = 'alt', res = 2.5) 
# for better resolution, download res=0.5 elevation from worldclim.org
dtm_Norway <-
  raster(
    'C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/Div/GIS/WorldClim/wc2.1_30s_elev.tif'
  ) # better resolution
dtm_region <- crop(dtm_Norway, region)

# Convert the raster to a data frame
dtm_df <- as.data.frame(dtm_region, xy = TRUE)

# plot the map (terrain detail, raster) - NOT WORKING for the better resolution, something wrong/different with data formats
ggplot(dtm_df) +
  geom_raster(aes(x = x, y = y, fill = alt)) +
  scale_fill_gradient(low = 'black', high = 'white') +
  coord_equal() +
  geom_point(data = pts_sites,
             aes(x = longitude,
                 y = latitude,
                 group = site),
             color = "red") +
  xlab('') + ylab('') +
  ggtitle("Study sites in western Norway \n over digital terrain model") +
  theme(legend.position = "none")



