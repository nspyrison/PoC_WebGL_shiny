{ ## Setup -----
  library("rgl")
  options(rgl.useNULL=TRUE) ## if FALSE prints to an X11 window. (MacOS needs XQuarts for X11.)
  options(rgl.printRglwidget = T) ## if TRUE sends the scene to the RStuido Viewer pane after every change.
  ##
  ## Call rglwidget() to display the scene
  
  nsCloseRGL <- function(n_tries = 5) {
    try_num <- 1
    while (try_num <= n_tries){
      try(rgl.close(), silent = TRUE)
      try_num <- try_num + 1
    }
  }
}

message("Call rglwidget() to view a webGL object from the RStudio Viewer pane.")
message("Alternatively, run options(rgl.printRglwidget = TRUE), to print widget to Viewer.")

x <- rnorm(20); y <- rnorm(20); z <- rnorm(20)
nsCloseRGL()

#### ns example 1 
open3d(FOV = 0, zoom = 1) ## parallel perspective, zoom is 1/magnification.
title3d(xlab = "x", ylab = "y", zlab = "z")
aspect3d(1, 1, 1)
bbox3d(xlen = 3, ylen = 3, zlen = 3,
       color = "black" , alpha = .7, emission = NULL, lwd = 1)

spheres3d(x = x, y = y, z = z, radius = .2)
segments3d(rep(x, each = 2), rep(y, each = 2), c(rbind(z+1, z-1)), color = "red")
rglwidget()
highlevel()



x <- rnorm(20); y <- rnorm(20); z <- rnorm(20)
nsCloseRGL()
#### ns2 -- bg3d(), bbox3d()------
## background and bounding boxes, bg3d(), bbox3d().
open3d()
bg3d(color = "grey100") ## Back ground color: lightgrey
bbox3d(xlen = 0, ylen = 0, zlen = 0, color = "grey40", alpha =.3, emission = "grey100")
## emission is sort of like blending color, if you have good background color, set to that for data vis.
spheres3d(x, y, z, 
          radius = .1, col = rep(rainbow(10), 2))
text3d(x = x, y = y, z = z, text = 1:20, adj = c(.5, 1.3), cex = 1, col = "black")
rglwidget()




nsCloseRGL()
## other aesthetics -----
# title3d('main', 'sub', 'xlab', 'ylab', 'zlab') ## Add various titles and labels
# axes3d(c('x', 'y', 'z')) ## Use fixed axes
# axes3d(c('x--', 'x-+', 'x+-', 'x++')) ## add 4 x-axes on the plot, like an engine cell.

