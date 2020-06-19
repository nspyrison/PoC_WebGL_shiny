## Cleaned up from:
browseURL("https://stackoverflow.com/questions/41145959/picture-convex-hull-in-3d-scatter-plot")

library("rgl")
library("geometry")

data(iris)
x <- iris$Sepal.Length
y <- iris$Petal.Length
z <- iris$Sepal.Width
pal <- c("blue", "gold2", "forestgreen")
col <- pal[as.integer(factor(iris$Species))]


ps1 <- matrix(c(x[1:50],y[1:50],z[1:50]), ncol=3)  # generate points on a sphere
ts.surf1 <- t(geometry::convhulln(ps1))  # see the qhull documentations for the options

ps2 <- matrix(c(x[51:150],y[51:150],z[51:150]), ncol=3)  # generate points on a sphere
ts.surf2 <- t(geometry::convhulln(ps2))  # see the qhull documentations for the options

nsCloseRGL <- function() {
  closed_last_rgl <- NULL
  while (is.null(closed_last_rgl))
    closed_last_rgl <- try(rgl.close(), silent = TRUE)
}
nsCloseRGL()
open3d()

spheres3d(x, y, z,  box = FALSE, radius = 0.15, col="blue")
ellips <- ellipse3d(cov(cbind(x,y,z)), 
                    centre=c(mean(x), mean(y), mean(z)), level = 0.95)
plot3d(ellips, col = "blue", alpha = 0.2, add = TRUE, box = FALSE)
spheres3d(x, y, z, box = FALSE, radius = 0.15,
          col = c(rep("gold2", 50), rep("forestgreen", 100)))
rglwidget()


triangles3d(ps1[ts.surf1,1],ps1[ts.surf1,2],ps1[ts.surf1,3], col="gold2", alpha = .6)
triangles3d(ps2[ts.surf2,1],ps2[ts.surf2,2],ps2[ts.surf2,3], col="forestgreen", alpha=.6)
rglwidget()

