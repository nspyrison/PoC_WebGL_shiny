####### _NicholasSpyrison_rgl_ server.r -----
#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison@gmail.com}

##### Setup -----
require("shiny")
require("rgl")
require("MASS")
require("tourr")
source("ui.r", local = TRUE)
set.seed(20200527)
options(rgl.useNULL = TRUE) ## Must be executed BEFORE rgl is loaded on headless devices.

##### exmples: 
## http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
## https://cran.r-project.org/web/packages/rgl/vignettes/rgl.html


##### Global initialize -----
## Above server scope and not reactive
## Parameters:
{
  ## Some aesthetics setup:
  .ptRad <- .02 ## size (radius? diameter?) of data points
  .shine <- 128 ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
  .a     <- .7 ## alpha for boundingbox and axes lines
  .bg    <- "lightgrey" ##"grey100" ## lighter grey Back Ground
  .bb    <- "darkgrey"  ##"grey40"  ## darker grey  Bounding Box
  .pal   <- RColorBrewer::brewer.pal(3, "Paired")
  
  ## Data, dim, basis, ases init
  dat   <- tourr::rescale(tourr::flea[, 1:6])
  n     <- nrow(dat)
  p     <- ncol(dat)
  d     <- 3
  rb    <- tourr::basis_random(p, d)
  ptCol <- app_col_of(tourr::flea$species)
  ptPch <- app_pch_of(tourr::flea$species)
  ptCol_pal <- unique(ptCol)
  
  ## Find tour bases and project data
  rb2holes_tpath <- 
    save_history(dat,
                 start = rb,step_size = .6,
                 tour_path = guided_tour(holes(), d = d, max.tries = 100))
  n_tpath_bases <- dim(rb2holes_tpath)[3]
  
  rb2holes_proj    <- array(NA, dim = c(  n, d, n_tpath_bases))
  basis_convex_box <- array(NA, dim = c(2^d, d, n_tpath_bases))
  for (i in 1:n_tpath_bases){
    rb2holes_proj[,, i] <- dat %*% matrix(rb2holes_tpath[,, i], nrow = p)
    
    x_min <- min(rb2holes_proj[, 1, i])
    y_min <- min(rb2holes_proj[, 2, i])
    z_min <- min(rb2holes_proj[, 3, i])
    x_max <- max(rb2holes_proj[, 1, i])
    y_max <- max(rb2holes_proj[, 2, i])
    z_max <- max(rb2holes_proj[, 3, i])
    basis_convex_box[,, i] <- as.matrix(data.frame(
      x = c(x_min, x_max, x_min, x_min, x_max, x_max, x_max, x_min),
      y = c(y_min, y_min, y_max, y_min, y_max, y_max, y_min, y_max),
      z = c(z_min, z_min, z_min, z_max, z_max, z_min, z_max, z_max)
    ))
  }
  holes_proj <- rb2holes_proj[,, n_tpath_bases]
  
  ### 2d Kernal density estimate
  holes_kde2d <- MASS::kde2d(holes_proj[, 1], holes_proj[, 2])
  
  ### Aesthetic init
  x_min <- min(rb2holes_proj[, 1, ])
  y_min <- min(rb2holes_proj[, 2, ])
  z_min <- min(rb2holes_proj[, 3, ])
  x_max <- max(rb2holes_proj[, 1, ])
  y_max <- max(rb2holes_proj[, 2, ])
  z_max <- max(rb2holes_proj[, 3, ])
  tour_convex_box <- data.frame(
    x = c(x_min, x_max, x_min, x_min, x_max, x_max, x_max, x_min) , 
    y = c(y_min, y_min, y_max, y_min, y_max, y_max, y_min, y_max), 
    z = c(z_min, z_min, z_min, z_max, z_max, z_min, z_max, z_max)
  )
}

####### shiny server start =====
server <- shinyServer(function(input, output, session) { ## Session required.
  # try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  # app_CloseRGL()
  save <- options(rgl.inShiny = TRUE)
  on.exit({options(save); try(rgl.close(), silent = TRUE)})
  
  ##### rb2holes =====
  #app_CloseRGL()
  updateSliderInput(session, "rb2holes_basis_slider", 
                    value = 1, step = 1, min = 1, max = n_tpath_bases)
  
  slider <- reactive(input$rb2holes_basis_slider)
  slider_t <- throttle(slider, millis = 50) 
  output$slider_t <- renderText(slider_t())
  ## Reactive function, throttled slider value, returning value every 'millis' milliseconds, 
  #### _ie_ "Try to return value even while manipulating slider every .05 sec."
  
  
  rb2holes_rglwidget <- reactive({
    #app_CloseRGL()
    req(slider_t())
    ## Selected projection basis and convex box from input slider.
    this_proj <- rb2holes_proj[,, slider_t()]
    this_convex_box <- basis_convex_box[,, slider_t()]
    
    ## Open scene with isopectric perspective
    open3d(FOV = 0)
    ## Plot projection points
    spheres3d(this_proj[, 1], this_proj[, 2], this_proj[, 3], 
              radius = .ptRad, col = ptCol)
    
    ## Prop up basis and fulll tour convex bounding box
    spheres3d(this_convex_box[, 1], this_convex_box[, 2], this_convex_box[, 3], 
              radius = .ptRad / 2, col = "black")
    spheres3d(tour_convex_box[, 1], tour_convex_box[, 2], tour_convex_box[, 3], 
              radius = .ptRad / 2, col = "grey")
    ## Aesthetic setup
    bg3d(color = .bg)
    bbox3d(xlen = 0, ylen = 0, zlen = 0,
           color = "black" , alpha = .a, emission = .bg, lwd = 1)
    lines3d(c(0, -1), c(0, 0), c(0, 0), lwd = 4, alpha = .a) # color = .pal[1],
    lines3d(c(0, 0), c(0, -1), c(0, 0), lwd = 4, alpha = .a) # color = .pal[2],
    lines3d(c(0, 0), c(0, 0), c(0, -1), lwd = 4, alpha = .a) # color = .pal[3],
    text3d(-.5,  0,  0, color = "black", texts = "x", cex = 2, adj = c(.5 , 1.3))
    text3d( 0, -.5,  0, color = "black", texts = "y", cex = 2, adj = c(.5 , 1.3))
    text3d( 0,  0, -.5, color = "black", texts = "z", cex = 2, adj = c(1.3, 1.3))
    
    scene_rb2holes <- scene3d()
    
    rglwidget(scene_rb2holes, width = w, height = h)
  })
  
  output$widget_rb2holes <- renderRglwidget(
    rb2holes_rglwidget(),
  )
  
  
  ##### holes_kde3d =====
  ## Kernal estimation on covar matrix 
  ## Loosely, the smallest n-D ellipsoid containing 'level'% of the observations from the estimated distribution.
  # try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  #app_CloseRGL()
  open3d(FOV = 0)
  spheres3d(holes_proj[, 1], holes_proj[, 2], holes_proj[, 3], 
            radius = .ptRad, col = ptCol)  
  ellips <- 
    ellipse3d(cov(holes_proj[, 1:3]), level = 0.68,
              centre = apply(holes_proj, 2, mean))
  wire3d(ellips,  add = T, type = "wire",
         col = "grey", alpha = 1 - .a, shininess = .shine)
  
  for (i in 1:length(ptCol_pal)){
    .dat      <- holes_proj[ptCol == ptCol_pal[i], ]
    cl_ellips <- ellipse3d(cov(.dat[, 1:3]), level = 0.68,
                           centre = apply(.dat, 2, mean))
    wire3d(cl_ellips,  add = T, lwd = 1.5,
           col = ptCol_pal[i], alpha = 1 - .a, shininess = .shine)
  }
  
  scene_pca_kde3d <- scene3d()
  
  output$widget_pca_kde3d <- renderRglwidget(
    rglwidget(scene_pca_kde3d)
  )
  
  
  ##### holes_kde2d =====
  ## via MASS::kde2d()
  ## Can't seem to get type="wire" and some other options working
  # try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  #app_CloseRGL()
  open3d(FOV = 0)
  spheres3d(holes_proj[, 1], holes_proj[, 2], rep(0, n), 
            radius = .ptRad, col = ptCol)
  persp3d(holes_kde2d, add = T,
          col = "red", alpha = 1 - .a, shininess = .shine)
  
  scene_holes_kde2d <- scene3d()
  
  output$widget_holes_kde2d <- renderRglwidget(
    rglwidget(scene_holes_kde2d)
  )
  
  
  ##### logLik =====
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  #app_CloseRGL()
  open3d(FOV = 0)
  x   <- rgamma(100, shape = 5, rate = 0.1)
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
  #app_CloseRGL()
  open3d(FOV = 0)
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
  
  ### widget_rotation -----
  observe({
    # tell our rglWidgetAux to query the plot3d for its par3d
    input$queryumat
    session$sendInputMessage("ctrlplot3d", list("cmd"="getpar3d","rglwidgetId"="plot3d"))
  })
  
  output$usermatrix <- renderTable({
    ## NOTE .js file may have hardcoded names, change slowly and carfully.
    shiny::validate(need(!is.null(input$ctrlplot3d),"User Matrix not yet queried"))
    rmat <- matrix(0,4,4)
    jsonpar3d <- input$ctrlplot3d
    if (jsonlite::validate(jsonpar3d)){
      par3dout <- fromJSON(jsonpar3d)
      rmat <- matrix(unlist(par3dout$userMatrix),4,4) # make list into matrix
    }
    return(rmat)
  })
  
  scenegen <- reactive({
    # make a random scene
    input$regen
    n <- 1000
    x <- sort(rnorm(n))
    y <- rnorm(n)
    z <- rnorm(n) + atan2(x, y)
    open3d(theta = 20, phi = 45, fov = 60, zoom = 1)
    plot3d(x, y, z, col = rainbow(n))
    scene1 <- scene3d()
    rgl.close() # make the app window go away
    return(scene1)
  })
  output$plot3d <- renderRglwidget({ rglwidget(scenegen()) })
  
  
 
  
  
})

shinyApp(ui = ui, server = server)
