set.seed(20200528)
library(shiny)
library(rmarkdown) ## which pkg req wants this?
library(tictoc)
library(rayshader)
options(rgl.useNULL=TRUE) # Must be executed BEFORE rgl is loaded on headless devices.
library(ggplot2)
try(rgl.close())


# ## Diamond facets
# ggdiamonds = ggplot(diamonds) +
#   stat_density_2d(aes(x = x, y = depth, fill = stat(nlevel)), 
#                   geom = "polygon", n = 100, bins = 10, contour = TRUE) +
#   facet_wrap(clarity~.) +
#   scale_fill_viridis_c(option = "A")
# 
# #par(mfrow = c(1, 2))
# tictoc::tic()
# plot_gg(ggdiamonds, width = 5, height = 5, raytrace = FALSE, preview = TRUE) ## tictoc: 6.4s
# tictoc::toc()
# tictoc::tic()
# plot_gg(ggdiamonds, width = 5, height = 5, multicore = TRUE, scale = 250, ## tictoc: 37 sec
#         zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
# tictoc::toc()
# tictoc::tic()
# render_snapshot(clear = TRUE) ## .83 sec, black sqaure.
# tictoc::toc()


## Hexbin raster

library(ggplot2)
n_x000size <- .5
n_size <- n_x000size * 1000
max_err <- .001 ## Like tolerance or reduction size? 
{
  try(rgl.close())
  tictoc::tic(paste0("== Start to finish (n=", n_size, ", max_err= ", max_err, ") =="))
  tictoc::tic(paste0("data prep and ggplot (n=", n_size, ", max_err= ", max_err, ")"))
  a = data.frame(x = rnorm(n_size, 10, 1.9),   y = rnorm(n_size, 10, 1.2))
  b = data.frame(x = rnorm(n_size, 14.5, 1.9), y = rnorm(n_size, 14.5, 1.9))
  c = data.frame(x = rnorm(n_size, 9.5, 1.9),  y = rnorm(n_size, 15.5, 1.9))
  data = rbind(a, b, c)
  
  pp = ggplot(data, aes(x = x, y = y)) +
    geom_hex(bins = 20, size = 0.5, color = "black") +
    scale_fill_viridis_c(option = "C")
  tictoc::toc()
  
  # par(mfrow = c(1, 2))
  # tictoc::tic()
  # plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE) ## 3.74 s @200000
  # tictoc::toc()
  tictoc::tic(paste0("plot_gg (n=", n_size, ", max_err= ", max_err, ")"))
  ## 29s @2000, 36.7s @50000, 71.1s @1250000
  plot_gg(pp, multicore = TRUE, max_error = max_err,
          width = 5, height = 4, scale = 300,  windowsize = c(1000, 800))
  tictoc::toc()
  
  tictoc::tic(paste0("render and widget (n=", n_size, ", max_err= ", max_err, ")"))
  ## .73s @2000, 1.71s @50000, 1.76 @1250000
  render_camera()#fov = 70, zoom = .5, theta = 40, phi = 40)
  ## fov Field of view max=180, zoom "Positive value indicating camera magnification"
  ## theta is "Rotation angle", phi is "Azimuth angle" max=90] 
  render_snapshot(clear = TRUE)
  tictoc::toc()
  tictoc::toc()
  
}

## adding max_err arg "## 98.8% reduction: Number of triangles reduced from 3600000 to 41446. Error: 0.009996"
## BUT doesn't seem to effect time
tic()
plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, windowsize = c(1200, 960),
        fov = 70, zoom = 0.4, theta = 330, phi = 20,  max_error = 0.01, verbose = TRUE)
toc() ## 12.44 sec elapsed, how?
tic()
montereybay %>% 
  sphere_shade(zscale = 10, texture = "imhof1") %>% 
  plot_3d(montereybay, zscale = 50, fov = 0, theta = -45, phi = 45, windowsize = c(1000, 800), zoom = 0.6,
          water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5, baseshape = "hex")
render_snapshot(clear = TRUE)
toc()
# ## Causes hang:
# tic()
# render_highquality(samples = 400, aperture=30, light = FALSE, ambient = TRUE,focal_distance = 1700,
#                    obj_material = rayrender::dielectric(attenuation = c(1,1,0.3)/200),
#                    ground_material = rayrender::diffuse(checkercolor = "grey80",sigma=90,checkerperiod = 100))
# toc()