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


# ensure test objects are all identical
test_that("if x is a list: identical results for df, matrix, sf, and sp objects", {
  expect_equal(tin_df, tin_mat)
  expect_equal(tin_df, tin_sfc)
  expect_equal(tin_df, tin_sp)
})

