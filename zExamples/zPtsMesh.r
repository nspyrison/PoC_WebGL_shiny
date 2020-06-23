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

## init 
xyz <- matrix(c(-1, -1, -1,
                -1,  1, -1,
                1,  1, -1,
                1, -1, -1,
                -1,  1, -1,
                -1,  1,  1,
                1,  1,  1,
                1,  1, -1,
                1, -1, -1,
                1,  1, -1,
                1,  1,  1,
                1, -1,  1), byrow = TRUE, ncol = 3)
length(xyz)
mesh <- as.mesh3d(xyz, triangles = F, col = "red")
mesh$vb
mesh$ib

nsCloseRGL()

open3d()
shade3d(mesh) # alt: wire3d(mesh)
spheres3d(xyz[,1], xyz[,2], xyz[,3], radius = .05) ## radius is the same size as the values.

rglwidget()

#### as.mesh3d doesn't make a convex mesh; look to geometry: ----
# following: https://stackoverflow.com/questions/41145959/picture-convex-hull-in-3d-scatter-plot
library("rgl")
data(iris)
x <- sep.l <- iris$Sepal.Length
y <- pet.l <- iris$Petal.Length
z <- sep.w <- iris$Sepal.Width

library("geometry")
ps1 <- matrix(c(x[1:50],y[1:50],z[1:50]), ncol=3)
ts.surf1 <- t(convhulln(ps1))
ps2 <- matrix(c(x[51:150],y[51:150],z[51:150]), ncol=3)  # generate points on a sphere
ts.surf2 <- t(convhulln(ps2))  # see the qhull documentations for the options

nsCloseRGL()

open3d()
plot3d(x, y, z, col="blue", box = FALSE,
       type ="s", radius = 0.15)
spheres3d(x, y, z, radius = .05) ## radius is the same size as the values.
triangles3d(ps1[ts.surf1,1],ps1[ts.surf1,2],ps1[ts.surf1,3],col="gold2",alpha=.6)
triangles3d(ps2[ts.surf2,1],ps2[ts.surf2,2],ps2[ts.surf2,3],col="forestgreen",alpha=.6)
rglwidget()


##### We have concavity in bottoms and last top; let's look for aplha hulls. ---
## Following: https://cran.r-project.org/web/packages/alphashape3d/index.html
#install.packages("alphashape3d")
library("alphashape3d")
?ashape3d
T1 <- rtorus(1000, 0.5, 2)
T2 <- rtorus(1000, 0.5, 2, ct = c(2, 0, 0), rotx = pi/2)
x <- rbind(T1, T2)
# Value of alpha
alpha <- 0.25
# 3D alpha-shape
ashape3d.obj <- ashape3d(x, alpha = alpha)
plot(ashape3d.obj)

# For new values of alpha, we can use ashape3d.obj as input (faster)
alpha <- c(0.15, 1)
ashape3d.obj <- ashape3d(ashape3d.obj, alpha = alpha)
plot(ashape3d.obj, indexAlpha = 2:3, col = ) 

#### Let's extract the triangles so we can use rgl::triangles3d(). ----
ashape3d.triang <- ashape3d.obj$triang
table(ashape3d.triang[, "fc:0.25"]) ## values can be: 0 (not on a.hull) 1 (interitor triang), 2 (regular), 3 (singular)
rows_on_a.hull <- ashape3d.triang[, "fc:0.25"] > 1
## only rows on the a.hull given the evaluated alpha = 0.25
a.hull_triang  <- t(ashape3d.triang[rows_on_a.hull, 1:3])

triangles3d(x[a.hull_triang, 1],
            x[a.hull_triang, 2],
            x[a.hull_triang, 3],
            col = ..pal[i], alpha = .2) ## transparency not a.hull alpha


