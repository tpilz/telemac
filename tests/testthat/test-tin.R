# create some test objects
library(sp)
library(sf)

bnd_df <- data.frame(x = c(0, 100, 100,    0, 0),
                     y = c(0,   0, 100,  100, 0))
bnd_mat <- matrix(c(0, 100, 100,    0, 0,
                    0,   0, 100,  100, 0), ncol = 2,
                  dimnames = list(NULL, c("x", "y")))
bnd_sfc <- st_sfc(st_linestring(bnd_mat))
bnd_sp <- SpatialLines(list(Lines(list(Line(bnd_mat)), ID = "outer")))

brkl_df <- data.frame(x =    c(20, 80, 30, 60, 60, 40),
                      y =    c(30, 30, 50, 50, 70, 70),
                      line = c( 1,  1,  2,  2,  2,  2))
brkl_mat <- matrix(c(20, 80, 30, 60, 60, 40,
                     30, 30, 50, 50, 70, 70,
                     1,  1,  2,  2,  2,  2), ncol = 3,
                   dimnames = list(NULL, c("x", "y", "line")))
brkl_sfc <- st_sfc(st_linestring(brkl_mat[1:2, 1:2]),
                   st_linestring(brkl_mat[3:6, 1:2]))
brkl_sp <- SpatialLines(list(Lines(list(Line(brkl_mat[1:2, 1:2])), ID = "L1"),
                             Lines(list(Line(brkl_mat[3:6, 1:2])), ID = "L2")))

tin_df <- tin(list(boundary = bnd_df, breaklines = brkl_df), s = 10, s_brk = 5, a = 10^2)
tin_mat <- tin(list(boundary = bnd_mat, breaklines = brkl_mat), s = 10, s_brk = 5, a = 10^2)
tin_sfc <- tin(list(boundary = bnd_sfc, breaklines = brkl_sfc), s = 10, s_brk = 5, a = 10^2)
tin_sp <- tin(list(boundary = bnd_sp, breaklines = brkl_sp), s = 10, s_brk = 5, a = 10^2)


test_that("if x is a list: identical results for df, matrix, sf, and sp objects", {
  expect_equal(tin_df, tin_mat)
  expect_equal(tin_df, tin_sfc)
  expect_equal(tin_df, tin_sp)
})


test_that("works as expected if x is a matrix", {
  tin_obj <- tin(tin_df$points, ikle = tin_df$triangles, ipobo = tin_df$boundaries)
  tin_df_t <- tin_obj
  tin_df_t["breaklines"] <- list(NULL)
  expect_equal(tin_obj, tin_df_t)
})


test_that("throws error if boundary definition is incompatible with TELEMAC and BlueKenue", {
  pts <- tin_df$points
  bnd <- tin_df$boundaries
  bnd_reord <- as.integer(c(1,3,2, 21:35, 109:130)) # 40 boundary points as defined above
  pts_reord <- pts
  pts_reord[bnd_reord,] <- pts[bnd,]
  pts_reord[-bnd_reord,] <- pts[-bnd,]
  expect_error(tin(pts_reord, ikle = tin_df$triangles, ipobo = bnd_reord),
               "Boundary points do not comply with requirements: outer boundary has to come first and point sequence has to be continuous!")
  expect_error(tin(tin_df$points, ikle = tin_df$triangles, ipobo = rev(tin_df$boundaries)),
               "Boundary points do not comply with requirements: orientation has to be anticlockwise!")
})


test_that("supported files are correctly imported", {
  for (f in c("test_geo.slf", "test_gmsh.msh")) {
    tin_obj <- tin(system.file(paste0("testfiles/", f), package = "telemac"))
    expect_s3_class(tin_obj, c("t2d_tin", "list"))
  }
  expect_error(tin("foo.bar"), "Unsupported file extension!")
})


test_that("crashes in case of intersecting breaklines", {
  brkl_inters <- data.frame(x =    c(20, 80, 30, 60, 60, 40),
                            y =    c(30, 30, 20, 20, 70, 70),
                            line = c( 1,  1,  2,  2,  2,  2))
  expect_error(tin(list(boundary = bnd_df, breaklines = brkl_inters), s = 10, s_brk = 5, a = 10^2),
               "There are dplicated points in the breaklines, e.g. because breaklines intersect, which is not supported by the Triangle algorithm!")
})

