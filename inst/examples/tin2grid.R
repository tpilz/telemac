# plot model bathymetry with ggplot
library(raster)
library(sf)
library(tidyverse)
# t2d_geo object
bnd <- st_read(system.file("dem/boundary_lagos.gpkg", package = "telemac"))
tin_obj <- tin(list(boundary = bnd), s = 90, a = 100^2, q = 30)
dem_rast <- raster(system.file("dem/dem_merit_lagos.tif", package = "telemac"))
geo_obj <- geo(tin_obj, dem = dem_rast)
# interpolate to regular grid as df and plot
geo_df <- tin2grid(geo_obj, s = 90, output = "data.frame")
ggplot(geo_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c()
