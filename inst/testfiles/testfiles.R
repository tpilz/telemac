# Skript to generate test_gmsh.msh and test_geo.slf

library(telemac)
library(raster)
library(dplyr)

bnd_df <- data.frame(x = c(0, 100, 100,    0, 0),
                     y = c(0,   0, 100,  100, 0))

tin_obj <- tin(list(boundary = bnd_df), s = 10, a = 10^2)

rast <- raster(nrows = 20, ncols = 20, xmn = 0, xmx = 100, ymn = 0, ymx = 100)
values(rast) <- rep(1:5, each = 20*4, length.out = ncell(rast))

plot(rast)
lines(tin_obj)

geo_obj <- geo(tin_obj, fname = "test_geo.slf", dem = rast)
write_geo(geo_obj)


bnd_df_spac <- line_spacing(mutate(bnd_df, line = 1), s = 10)
write.table(bnd_df_spac[,c("x", "y", "line")], file = "master_nodes.csv", quote = F, sep = ",", row.names = F, col.names = F)
write.table(bnd_df_spac %>%
              mutate(grp = 0) %>%
              select(grp, x, y, line), file = "boundary.csv", quote = F, sep = ",", row.names = F, col.names = F)
# call (pputils https://github.com/pprodano/pputils): gis2gmsh.py -n master_nodes.csv -b boundary.csv -l none -h none -o mesh.geo
# call Gmsh: gmsh mesh.geo -o test_gmsh.msh -format msh2 -2 -algo meshadapt
