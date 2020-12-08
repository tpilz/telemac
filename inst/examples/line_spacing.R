# one line given as numeric vectors
x = c(1,1.5,2.5,3,3.2,5, 5.8, 6.5, 7, 6.7, 6, 5.5, 4.8, 4.3, 4, 4, 4.3)
y = c(1,1.8,2,2.8,3.3,4.5, 4.2, 3.8, 3, 2.7, 2.5, 2.5, 2.2, 2.5, 2.8, 3.2, 3.4)
plot(x, y, pch = 16, asp = 1)
lines(x, y)
ldf_harm <- line_spacing(x = x, y = y, s = 1)
points(ldf_harm$x, ldf_harm$y, pch = 16, col = "red")
lines(ldf_harm$x, ldf_harm$y, col = "red")

# two lines in a matrix
x2 = c(10, 12, 12.2, 11.5, 11.4, 12.5, 13.5, 14)
y2 = c(4, 4.2, 3.5, 3.3, 2.5, 2.2, 2.5, 3)
lmat <- rbind(cbind(x, y), cbind(x2, y2))
line <- c(rep(1, length(x)), rep(2, length(x2)))
plot(x, y, pch = 16, xlim = c(min(lmat[,1]), max(lmat[,1])),
     ylim = c(min(lmat[,2]), max(lmat[,2])), asp = 1)
points(x2, y2, pch = 16)
lines(x, y)
lines(x2, y2)
ldf_harm <- line_spacing(x = lmat, line = line, s = 1)
lh1 <- ldf_harm[ldf_harm$line == 1,]
lh2 <- ldf_harm[ldf_harm$line == 2,]
points(lh1$x, lh1$y, pch = 16, col = "red")
lines(lh1$x, lh1$y, col = "red")
points(lh2$x, lh2$y, pch = 16, col = "red")
lines(lh2$x, lh2$y, col = "red")

# data.frame
library(dplyr)
line1 <- data.frame(
  xcoord = x,
  ycoord = y
)
line2 <- data.frame(
  xcoord = x2,
  ycoord = y2
)
ldf <- bind_rows(line1, line2, .id = "id")
plot(line1, pch = 16, xlim = c(min(ldf$x), max(ldf$x)),
     ylim = c(min(ldf$y), max(ldf$y)), asp = 1)
points(line2, pch = 16)
lines(line1)
lines(line2)
ldf_harm <- line_spacing(ldf, s = 1, col_x = xcoord, col_y = ycoord, col_line = id)
line1_harm <- ldf_harm %>%
  filter(id == 1) %>%
  dplyr::select(xcoord, ycoord)
line2_harm <- ldf_harm %>%
  filter(id == 2) %>%
  dplyr::select(xcoord, ycoord)
points(line1_harm, pch = 16, col = "red")
points(line2_harm, pch = 16, col = "red")
lines(line1_harm, col = "red")
lines(line2_harm, col = "red")

# SpatialLines object
library(sp)
sl <- SpatialLines(list(Lines(Line(line1), 1), Lines(Line(line2), 2)))
plot(sl, asp = 1)
sl_harm <- line_spacing(sl, s = 1, output = "sp")
lines(sl_harm, col = "red")
