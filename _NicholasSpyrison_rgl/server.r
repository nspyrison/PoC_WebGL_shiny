##### Setup -----

#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison@gmail.com}

library(shiny)
library(rgl)
library(MASS)
library(spinifex)  ## for flea, and util func
library(tourr)
library(tictoc)
source('ui.r', local = TRUE)
set.seed(20200527)
options(rgl.useNULL=TRUE) ## Must be executed BEFORE rgl is loaded on headless devices.

##### exmples: 
## http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
## https://cran.r-project.org/web/packages/rgl/vignettes/rgl.html


##### Global initialize -----
## Above server scope and not reactive
## Parameters:
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
p     <- ncol(dat)
d     <- 3
ptCol <- spinifex::col_of(tourr::flea$species)
ptPch <- spinifex::pch_of(tourr::flea$species)

pca2cmass_tpath <- save_history(dat, tour_path = 
                                  guided_tour(cmass(), d = d, max.tries = 100), 
                                step_size = .6, rescale = FALSE)
n_tpath_bases <- dim(pca2cmass_tpath)[3]
## 12 bases (w/ step_size = .6, d=2), 10 bases (w/ step_size = .6, d=3)
pca2cmass_proj <- array(NA, dim = c(n, d, n_tpath_bases))
for (i in 1:n_tpath_bases){
  pca2cmass_proj[,, i] <- dat %*% matrix(pca2cmass_tpath[,, i], nrow = p)
}
# w <- h <- 100 ## height and width of the rgl widget in pixels, 
## w/h not working atm.

pca_bas  <- prcomp(x = dat)$rotation ## p-dim basis of PCA on mat_dat()
pca_proj <- tibble::as_tibble(dat %*% pca_bas)

## Estimate 2d density surface via MASS kernel smoothing
pca_kde2d <- kde2d(pca_proj$PC1, pca_proj$PC2, n = nGridLevels)

####### shiny server start =====
server <- shinyServer(function(input, output, session) {
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  ##### pca_kde3d =====
  ## Kernal estimation on covar matrix (1 SD, 68% obs within volume)
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  plot3d(pca_proj$PC1, pca_proj$PC2, pca_proj$PC3, 
         type = 's', size = ptSize, col = ptCol)  
  ellips <- 
    ellipse3d(cov(pca_proj[, 1:3]), level = 0.68,
              centre = c(mean(pca_proj$PC1), mean(pca_proj$PC2), mean(pca_proj$PC3)))
  wire3d(ellips,  add = T, type = "wire",
         col = "grey", alpha = surfaceAlpha, shininess = surfaceShine)
  scene_pca_kde3d <- scene3d()
  
  output$widget_pca_kde3d <- renderRglwidget(
    rglwidget(scene_pca_kde3d)
  )
  
  ##### pca_kde2d =====
  ## via MASS::kde2d()
  ## Can't seem to get type="wire" and some other options working
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  plot3d(pca_proj$PC1, pca_proj$PC2, rep(0, n), col = ptCol)
  persp3d(pca_kde2d, add = T,
          col = "red", alpha = surfaceAlpha, shininess = surfaceShine)
  scene_pca_kde2d <- scene3d()
  
  output$widget_pca_kde2d <- renderRglwidget(
    rglwidget(scene_pca_kde2d)
  )
  
  ##### logLik =====
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
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
  scene_logLik <- scene3d()
  
  output$widget_logLik <- renderRglwidget(
    rglwidget(scene_logLik)
  )
  
  ##### functionSurfaces =====
  ## Following example for surf3D in: 
  # browseURL("https://cran.r-project.org/web/packages/plot3D/vignettes/plot3D.pdf")
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  mfrow3d(1, 2, sharedMouse = FALSE)
  .f1 = function(x, y){
    z = ((x^2) + (3 * y^2)) * exp(-(x^2) - (y^2))
  }
  plot3d(.f1, col = colorRampPalette(c("blue", "red")), 
         xlab = "X", ylab = "Y", zlab = "Z", 
         xlim = c(-3, 3), ylim = c(-3, 3),
         aspect = c(1, 1, 0.5))
  
  .f2 = function(x, y){
    z = (x^2) + (y^3)
  }
  plot3d(.f2, col = colorRampPalette(c("white", "black")),
         xlab = "X", ylab = "Y", zlab = "Z",
         xlim = c(-10, 10), ylim = c(-4, 4))
  
  
  scene_functionSurfaces <- scene3d()

  output$widget_functionSurfaces <- renderRglwidget(
    rglwidget(scene_functionSurfaces)
  )
  
  ##### pca2cmass =====
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  updateSliderInput(session, "pca2cmass_basis_slider", 
                    value = 1, step = 1, min = 1, max = n_tpath_bases)
  
  slider_t <- throttle(reactive(input$pca2cmass_basis_slider), millis = 50) 
  ## Reactive function, throttled slider value, returning value every 'millis' milliseconds, 
  #### _ie_ "Try to return value even while manipulating slider every .05 sec."
  
  pca2cmass_rglwidget <- reactive({
    ## Selected projection plane from input slider.
    sel_proj <- pca2cmass_proj[,, slider_t()]
    plot3d(sel_proj[, 1], sel_proj[, 2], sel_proj[, 3],
           type = 's', size = ptSize, col = ptCol)
    
    ## Some logistic setup:
    bg3d(color = "grey100") ## Back ground color: lightgrey
    bbox3d(xlen = 0, ylen = 0, zlen = 0, color = "grey40", alpha =.3, emission = "grey100")
    lines3d(c(0, 1), c(0, 0), c(0, 0), color = "black") 
    lines3d(c(0, 0), c(0, 1), c(0, 0), color = "red")
    lines3d(c(0, 0), c(0, 0), c(0, 1), color = "green")
    text3d(c(0, .5), c(0, 0), c(0, 0), texts = "x", adj = c(.5, 1.3), color = "black") 
    text3d(c(0, 0), c(0, .5), c(0, 0), texts = "y", adj = c(.5, 1.3), color = "red")
    text3d(c(0, 0), c(0, 0), c(0, .5), texts = "z", adj = c(.5, 1.3), color = "green")
    
    
    scene_pca2cmass <- scene3d()
    rglwidget(scene_pca2cmass)
  })
  
  output$widget_pca2cmass <- renderRglwidget(
    pca2cmass_rglwidget()
  )
  
})

shinyApp(ui = ui, server = server)
