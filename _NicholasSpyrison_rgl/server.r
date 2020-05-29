##### Setup -----
library(shiny)
library(rgl)
library(MASS)
library(spinifex)  ## for flea, and util func
library(tictoc)
source('ui.r', local = TRUE)
set.seed(20200527)
options(rgl.useNULL=TRUE) ## Must be executed BEFORE rgl is loaded on headless devices.
### exmples: 
# http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization



##### Global initialize -----
## Above server scope and not reactive
## parameters:
ptSize        <- 1.5 ## size (radius? diameter?) of data points
surfaceAlpha  <- .3  ## Opacity of the surface/grid in [0-1], fully transparent and opaque respectively
surfaceShine  <- 128 ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
nGridLevels   <- 50  ## The number of levels to grid search on each axis

## data and work:
dat <- { ## numeric data, rescaled flea
  f <- tourr::flea[,1:6]
  tourr::rescale(f)
}
n     <- nrow(dat)
ptCol <- spinifex::col_of(tourr::flea$species)
ptPch <- spinifex::pch_of(tourr::flea$species)

pca_bas  <- prcomp(x = dat)$rotation ## p-dim basis of PCA on mat_dat()
#pca_m_sp <- create_manip_space(basis = pca_bas[,1:2], manip_var = 4) ## 3d manip space on PCA bas
pca_proj <- tibble::as_tibble(dat %*% pca_bas)
#colnames(proj) <- c("x", "y")

# estimate 2d density surface via kernel smoothing
pca_de2d <- kde2d(pca_proj$PC1, pca_proj$PC2, n = nGridLevels)

#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison@gmail.com}
####### shiny server start =====
server <- shinyServer(function(input, output) {
  try(rgl.close())
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  ##### pca3d =====
  try(rgl.close())
  plot3d(pca_proj$PC1, pca_proj$PC2, pca_proj$PC3, 
         type = 's', size = ptSize, col = ptCol)  
  ellips <- 
    ellipse3d(cov(pca_proj[, 1:3]), level = 0.68,
              centre = c(mean(pca_proj$PC1), mean(pca_proj$PC2), mean(pca_proj$PC3)))
  wire3d(ellips,  add = T, type = "wire",
         col = "grey", alpha = surfaceAlpha, shininess = surfaceShine)
  scene_pca3d <- scene3d()
  
  output$widget_pca3d <- renderRglwidget(
    rglwidget(scene_pca3d)
  )
  
  ##### pca_de2d =====
  ## Can't seem to get type="wire" and some other options working
  try(rgl.close())
  plot3d(pca_proj$PC1, pca_proj$PC2, rep(0, n), col = col)
  persp3d(pca_de2d,  add = T,
          col = "red", alpha = surfaceAlpha, shininess = surfaceShine)

  
  
  scene_pca_de2d <- scene3d()
  
  
  output$widget_pca_de2d <- renderRglwidget(
    rglwidget(scene_pca_de2d)
  )
  
  ##### fitDistr =====
  try(rgl.close())
  x <- rgamma(100, shape = 5, rate = 0.1)
  fit <- fitdistr(x, dgamma, list(shape = 1, rate = 0.1), lower = 0.001)
  loglik <- function(shape, rate)
    sum(dgamma(x, shape = shape, rate = rate, log = TRUE))
  loglik <- Vectorize(loglik)
  
  xlim <- fit$estimate[1] + 4 * fit$sd[1] * c(-1, 1)
  ylim <- fit$estimate[2] + 4 * fit$sd[2] * c(-1, 1)
  mfrow3d(1, 2, sharedMouse = TRUE)
  persp3d(loglik,
          xlim = xlim, ylim = ylim,
          n = 30)
  
  zlim <- fit$loglik + c(-qchisq(0.99, 2) / 2, 0)
  next3d()
  persp3d(loglik,
          xlim = xlim, ylim = ylim, zlim = zlim,
          n = 30)
  scene_fitDistr <- scene3d()
  
  output$widget_fitDistr <- renderRglwidget(
    rglwidget(scene_fitDistr)
  )
  
  ##### surface3d =====
  ### following example for surf3D in: 
  ## browseURL("https://cran.r-project.org/web/packages/plot3D/vignettes/plot3D.pdf")
  try(rgl.close())
  # mfrow3d(2, 2, sharedMouse = FALSE)
  # ## Shape 1
  # M <- mesh(seq(0, 6 * pi, length.out = 80),
  #           seq(pi / 3, pi, length.out = 80))
  # u <- M$x; v <- M$y
  # x <- u / 2 * sin(v) * cos(u)
  # y <- u / 2 * sin(v) * sin(u)
  # z <- u / 2 * cos(v)
  # .col <- scales::rescale(z)
  # message(.col)
  # surface3d(x, y, z, col = .col)
  # next3d()
  # ## Shape 2
  # M <- mesh(seq(0, 2 * pi, length.out = 80),
  #           seq(0, 2 * pi, length.out = 80))
  # u <- M$x; v <- M$y
  # x <- sin(u); y <- sin(v); z <- sin(u + v)
  # surface3d(x, y, z, col = scales::rescale(z))
  # next3d()
  # ## Shape 3
  # x <- (3 + cos(v / 2) * sin(u) - sin(v / 2) * sin(2 * u)) * cos(v)
  # y <- (3 + cos(v / 2) * sin(u) - sin(v / 2) * sin(2 * u)) * sin(v)
  # z <- sin(v / 2) * sin(u) + cos(v / 2) * sin(2 * u)
  # surface3d(x, y, z, col = z)
  # next3d()
  # ## Shape 4: more complex color var
  # M <- mesh(seq(-13.2, 13.2, length.out = 50),
  #           seq(-37.4, 37.4, length.out = 50))
  # u <- M$x; v <- M$y
  # b <- 0.4; r <- 1 - b^2; w <- sqrt(r)
  # D <- b * ((w * cosh(b * u))^2 + (b * sin(w * v))^2)
  # x <- -u + (2 * r * cosh(b * u) * sinh(b * u)) / D
  # y <- (2 * w * cosh(b * u) * (-(w * cos(v) * cos(w * v)) - sin(v) * sin(w * v))) / D
  # z <- (2 * w * cosh(b * u) * (-(w * sin(v) * cos(w * v)) + cos(v) * sin(w * v))) / D
  # surface3d(x, y, z, col = scales::rescale(sqrt(x + 8.3)))
  # scene_surface3d <- scene3d()
  # 
  # output$widget_surface3d <- renderRglwidget(
  #   rglwidget(scene_surface3d)
  # )
  
})

shinyApp(ui = ui, server = server)
