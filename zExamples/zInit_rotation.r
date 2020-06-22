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

## Use par3d() to get or get parameter settings
?par3d
r3dDefaults ## View default parameters
par3d("userMatrix") ## Get specific parameter
save <- par3d(userMatrix = rotationMatrix(10, 20, 30, 40), FOV = 0) ## Assign a par3d_obj list.
save ## Print list of 3d parameters.
par3d(save) ##  not sure? maybe sets parameters; also pass into open3d()

nsCloseRGL()
## Init defaults on a cube
open3d()
shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
rglwidget()
####
nsCloseRGL()
## Init cube with saved parameters
open3d(params = save)
shade3d(cube3d(color = rep(rainbow(6), rep(4, 6))))
rglwidget()


