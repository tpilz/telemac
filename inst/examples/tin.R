
### BASIC FUNCTIONALITY ###
library(sf)

# load boundary as sf linestring
bnd <- st_read(system.file("dem/boundary_lagos.gpkg", package = "telemac"))

# create t2d_tin object
tin_obj <- tin(list(boundary = bnd), s = 90, a = 100^2, q = 30)

# inspection
tin_obj
str(tin_obj)
plot(tin_obj, pch = ".")

### DEALING WITH INTERSECTING BREAKLINES ###
library(sf)
library(tidyverse)

# example boundary and with intersecting breaklines
test_bnd <- st_linestring(
  matrix(c(seq(0,100,5),  rep(100,21), seq(100,0,-5),     rep(0,21),
           rep(0,21), seq(0,100,5),   rep(100,21), seq(100,0,-5)),
         ncol = 2)
) %>% st_sfc()
test_brk <- list(
  st_linestring(matrix(c(seq(0,100,5), rep(50,21)), ncol = 2)),
  st_linestring(matrix(c(rep(50,21), seq(0,100,5)), ncol = 2)),
  st_linestring(matrix(c(seq(30,60,5), rep(60,11),
                         rep(20,7), seq(20,70,5)), ncol = 2))) %>% st_sfc()

# get intersection points and define buffer of 2 around these points
pt_inters <- c(test_bnd, test_brk) %>%
  st_intersection() %>%
  st_collection_extract(type = "POINT") %>%
  st_buffer(2)

plot(test_bnd)
plot(test_brk, add = TRUE)
plot(pt_inters, add = TRUE)

# split breaklines
test_brk_unique <- st_difference(st_union(test_brk), st_union(pt_inters))
plot(test_bnd)
plot(test_brk_unique, add = TRUE)

# create mesh
tin_obj <- tin(list(boundary = test_bnd, breaklines = test_brk_unique),
               s = 2, s_brk = 2, a = 4, q = 30)
plot(tin_obj, pch = ".")
