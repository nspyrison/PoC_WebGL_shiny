{ ## Setup -----
  library("rgl")
  options(rgl.useNULL=TRUE)
  #options(rgl.printRglwidget = F)
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

#### ns example 1 -- unit lines -----
open3d()
spheres3d(z, y, z, radius = .05) ## radius is the same size as the values.
lines3d(c(0, 1), c(0, 0), c(0, 0), color = "black") 
lines3d(c(0, 0), c(0, 1), c(0, 0), color = "red")
lines3d(c(0, 0), c(0, 0), c(0, 1), color = "green")
text3d(c(0, .5), c(0, 0), c(0, 0), texts = "x", adj = c(.5, 1.3), color = "black") 
text3d(c(0, 0), c(0, .5), c(0, 0), texts = "y", adj = c(.5, 1.3), color = "red")
text3d(c(0, 0), c(0, 0), c(0, .5), texts = "z", adj = c(.5, 1.3), color = "green")
## Note that these are colored lines. Use axes3d() to interact with true axes.
axes3d(c('x', 'y', 'z'), color = c("black", "red", "green"))
axes3d(c('x', 'y', 'z'), color = rainbow(20)) ## Use fixed axes, note that color mapping sucks
legend3d(x = .7, y = .9, ## manual legends, similar to ?graphics::legend()
         paste0("mark ", letters[1:4]), 
         pch = c(1, 16), 
         col = c("black", "red", "green")
         )
rglwidget()



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

