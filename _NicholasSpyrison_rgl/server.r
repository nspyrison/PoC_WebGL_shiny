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
{
  ptRad         <- .02 ## size (radius? diameter?) of data points
  surfaceAlpha  <- .3  ## Opacity of the surface/grid in [0-1], fully transparent and opaque respectively
  surfaceShine  <- 128 ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
  nGridLevels   <- 50  ## The number of levels to grid search on each axis
  .a <- .7 ## alpha for boundingbox and axes lines
  
  ## data and work:
  dat <- tourr::rescale(tourr::flea[, 1:6])
  n     <- nrow(dat)
  p     <- ncol(dat)
  d     <- 3
  ptCol <- spinifex::col_of(tourr::flea$species)
  ptPch <- spinifex::pch_of(tourr::flea$species)
  n_col <- length(unique(ptCol))
  rb <- tourr::basis_random(p, d)
  
  rb2holes_tpath <- save_history(dat,
                                 start = rb,step_size = .6,
                                 tour_path = guided_tour(holes(), d = d, max.tries = 100))
  n_tpath_bases <- dim(rb2holes_tpath)[3]
  ## 12 bases (w/ step_size = .6, d=2), 10 bases (w/ step_size = .6, d=3)
  rb2holes_proj <- array(NA, dim = c(n, d, n_tpath_bases))
  for (i in 1:n_tpath_bases){
    rb2holes_proj[,, i] <- tourr::rescale(dat %*% matrix(rb2holes_tpath[,, i], nrow = p))
  }
  
  ## w/h not working atm.
  
  pca_bas  <- prcomp(x = dat)$rotation ## p-dim basis of PCA on mat_dat()
  pca_proj <- tibble::as_tibble(tourr::rescale(dat %*% pca_bas))
  
  ## Estimate 2d density surface via MASS kernel smoothing
  pca_kde2d <- kde2d(pca_proj$PC1, pca_proj$PC2, n = nGridLevels)
}

####### shiny server start =====
server <- shinyServer(function(input, output, session) {
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  ##### pca_kde3d =====
  ## Kernal estimation on covar matrix (1 SD, 68% obs within volume)
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  spheres3d(pca_proj$PC1, pca_proj$PC2, pca_proj$PC3, 
            radius = ptRad, col = ptCol)  
  ellips <- 
    ellipse3d(cov(pca_proj[, 1:3]), level = 0.68,
              centre = c(mean(pca_proj$PC1), mean(pca_proj$PC2), mean(pca_proj$PC3)))
  wire3d(ellips,  add = T, type = "wire",
         col = "grey", alpha = surfaceAlpha, shininess = surfaceShine)
  
  # for (i in 1:n_col){
  #   .dat <- pca_proj[ptCol == ptCol[i],]
  #   cl_ellips <- ellipse3d(cov(.dat[, 1:3]), level = 0.68,
  #                          centre = c(mean(pca_proj$PC1), mean(pca_proj$PC2), mean(pca_proj$PC3)))
  #   wire3d(cl_ellips,  add = T, type = "wire",
  #          col = ptCol[i], alpha = surfaceAlpha, shininess = surfaceShine)
  # }
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
  
  ##### rb2holes =====
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  updateSliderInput(session, "rb2holes_basis_slider", 
                    value = 1, step = 1, min = 1, max = n_tpath_bases)
  
  slider_t <- throttle(reactive({
    req(input$rb2holes_basis_slider)
    input$rb2holes_basis_slider
  }), millis = 50) 
  ## Reactive function, throttled slider value, returning value every 'millis' milliseconds, 
  #### _ie_ "Try to return value even while manipulating slider every .05 sec."
  
  rb2holes_rglwidget <- reactive({
    ## Selected projection plane from input slider.
    req(slider_t())
    this_proj <- tibble::as.tibble(rb2holes_proj[,, slider_t()])
    ## Some aesthetics setup:
    .l <- max(rb2holes_proj)
    .i <- .l / 2
    .bg <- "lightgrey" ##"grey100" ## lighter grey Back Ground
    .bb <- "darkgrey"  ##"grey40"  ## darker grey  Bounding Box
    .pal <- RColorBrewer::brewer.pal(3, "Paired")
    .bb_s <- 1.5## scale of bounding box
    .xlim <- .bb_s * c(0, max(rb2holes_proj[, 1, ]))
    .ylim <- .bb_s * c(0, max(rb2holes_proj[, 2, ]))
    .zlim <- .bb_s * c(0, max(rb2holes_proj[, 3, ]))
    
    spheres3d(this_proj$V1, this_proj$V2, this_proj$V3, 
              radius = ptRad, col = ptCol) # xlab = "x", ylab = "y", zlab = "z") ## not applying in shiny.
    #,xlim = .xlim, ylim = .ylim, zlim = .zlim, expand = .bb_s) ## not applying in shiny.
    bg3d(color = .bg) 
    bbox3d(xlen = 0, ylen = 0, zlen = 0, #xlab = "x", ylab = "y", zlab = "z", ## not applying in shiny
           color = c("black", "red", "orange") , alpha = .a, emission = .bg, lwd = 1)
    #,#xlim = .xlim, ylim = .ylim, zlim = .zlim, expand = .bb_s) ## not applying in shiny
    lines3d(c(0, .l), c(0,  0), c(0,  0), color = .pal[1], lwd = 8, alpha = .9, ) 
    lines3d(c(0,  0), c(0, .l), c(0,  0), color = .pal[2], lwd = 8, alpha = .9)
    lines3d(c(0,  0), c(0,  0), c(0, .l), color = .pal[3], lwd = 8, alpha = .9)
    text3d(.i,  0,  0, color = "black", texts = "x", cex = 2, adj = c(.5 , 1.3))
    text3d( 0, .i,  0, color = "black", texts = "y", cex = 2, adj = c(.5 , 1.3))
    text3d( 0,  0, .i, color = "black", texts = "z", cex = 2, adj = c(1.3, 1.3))
    
    
    scene_rb2holes <- scene3d()
    # Make it bigger
    scene_rb2holes$par3d$windowRect <- 1.5 * scene_rb2holes$par3d$windowRect
    rglwidget(scene_rb2holes, width = w, height = h)
  })
  
  output$widget_rb2holes <- renderRglwidget(
    rb2holes_rglwidget(),
  )
  
})

shinyApp(ui = ui, server = server)
