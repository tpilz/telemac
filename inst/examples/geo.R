\dontrun{
library(raster)
library(sf)
library(tidyverse)

# get a tin
bnd <- st_read(system.file("dem/boundary_lagos.gpkg", package = "telemac"))
tin_obj <- tin(list(boundary = bnd), s = 90, a = 100^2, q = 30)

# load raster
dem_rast <- raster(system.file("dem/dem_merit_lagos.tif", package = "telemac"))

# create a geo object (interpolates raster to mesh points)
geo_obj <- geo(tin_obj, dem = dem_rast)
geo_obj
str(geo_obj)

# adjust file name
geo_obj <- geo(geo_obj, fname = "geo.slf")
geo_obj

# plot: mesh elevations interpolated to grid with resolution s
plot(geo_obj, s = 30)

# add additional private variable (in this case Curve Numbers)
dem_priv <- list(
  # mandatory 'elevation' as raster object
  elevation = dem_rast,
  # additional variable 'cn' as (in this case) data.frame
  cn = list(values = as.data.frame(dem_rast, xy = TRUE, na.rm = TRUE) %>%
              select(x, y) %>%
              mutate(z = case_when(
                y > 740000 ~ 95,
                x > 534000 ~ 90,
                (x <= 534000) & (y <= 740000) ~ 80
              )),
            unit = "-",
            # nearest-neighbour interpolation of CN values
            pars_interp = list(n = 1))
)

geo_priv <- geo(tin_obj, dem = dem_priv)

geo_priv
str(geo_priv)
plot(geo_priv, s = 30, v = "cn")
}
