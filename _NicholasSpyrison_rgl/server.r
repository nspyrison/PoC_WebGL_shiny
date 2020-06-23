####### _NicholasSpyrison_rgl_ server.r -----
#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison@gmail.com}

##### Setup -----
source("./ui.r", local = TRUE)
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
  .a_bbox     <- .6 ## alpha for boundingbox and axes lines
  .a_surface  <- .6 ## alpha for surfaces and meshes
  .a_a.hull   <- .2 ## alpha for many triangles of the alpha hull
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
  
  rb2holes_proj <- array(NA, dim = c(  n, d, n_tpath_bases))
  for (i in 1:n_tpath_bases){
    rb2holes_proj[,, i] <- dat %*% matrix(rb2holes_tpath[,, i], nrow = p)
  }
  
  ## Pan tour to be in the "first quadrant"
  x_min <- min(rb2holes_proj[, 1, ])
  y_min <- min(rb2holes_proj[, 2, ])
  z_min <- min(rb2holes_proj[, 3, ])
  rb2holes_proj[, 1, ] <- rb2holes_proj[, 1, ] - x_min
  rb2holes_proj[, 2, ] <- rb2holes_proj[, 2, ] - y_min
  rb2holes_proj[, 3, ] <- rb2holes_proj[, 3, ] - z_min
  holes_proj <- rb2holes_proj[,, n_tpath_bases] ## Save final holes() basis. 
  
  ## 2d Kernal density estimate
  holes_kde2d <- MASS::kde2d(holes_proj[, 1], holes_proj[, 2], n = 60)
  m <- holes_kde2d
  str(m)
  dat <- holes_proj
  hist(dat[,1])
  hist(m$x)
  hist(dat[,2])
  hist(m$y)
  mesh <- ellipse3d(cov(dat), level = 0.68,
                    centre = apply(dat, 2, mean))
}

####### Shiny server start =====
server <- shinyServer(function(input, output, session) { ## Session required.
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  save <- options(rgl.inShiny = TRUE)
  on.exit({
    options(save); 
    try(rgl.close(), silent = TRUE)
    # try(rstudioapi::restartSession(), silent = TRUE) ## Release greedy CPU usage.
  })
  
  ##### rb2holes =====
  slider <- reactive(input$rb2holes_basis_slider)
  slider_t <- throttle(slider, millis = 50) 
  output$slider_t <- renderText(slider_t())
  ## Reactive function, throttled slider value, returning value every 'millis' milliseconds, 
  #### _ie_ "Try to return value even while manipulating slider every .05 sec."
  
  updateSliderInput(session, "rb2holes_basis_slider", 
                    value = 1, step = 1, min = 1, max = n_tpath_bases)
  
  rb2holes_rglwidget <- reactive({
    req(slider_t()) ## req(x) works, validate(need(x)) does not.
    ..proj <- rb2holes_proj[,, slider_t()]
    
    ## Open scene with isopectric/parallel perspective
    try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
    open3d(FOV = 0)
    ## Plot projection points
    spheres3d(..proj[, 1], ..proj[, 2], ..proj[, 3], 
              radius = .ptRad, col = ptCol)
    
    ## Aesthetic setup
    bg3d(color = .bg)
    bbox3d(xlen = 0, ylen = 0, zlen = 0,
           color = "black" , alpha = .a_bbox, emission = .bg, lwd = 1)
    lines3d(c(0, 1), c(0, 0), c(0, 0), lwd = 4, alpha = .a_bbox) # color = .pal[1],
    lines3d(c(0, 0), c(0, 1), c(0, 0), lwd = 4, alpha = .a_bbox) # color = .pal[2],
    lines3d(c(0, 0), c(0, 0), c(0, 1), lwd = 4, alpha = .a_bbox) # color = .pal[3],
    text3d(.5,  0,  0, color = "black", texts = "x", cex = 2, adj = c(.5 , 1.3))
    text3d( 0, .5,  0, color = "black", texts = "y", cex = 2, adj = c(.5 , 1.3))
    text3d( 0,  0, .5, color = "black", texts = "z", cex = 2, adj = c(1.3, 1.3))
    
    scene_rb2holes <- scene3d()
    
    rglwidget(scene_rb2holes, width = w, height = h)
  })
  
  output$widget_rb2holes <- renderRglwidget(
    rb2holes_rglwidget(),
  )
  
  
  ##### holes_kde3d =====
  ## Kernal estimation on covar matrix 
  ## Loosely, the smallest n-D ellipsoid containing 'level'% of the observations from the estimated distribution.
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  open3d(FOV = 0)
  spheres3d(holes_proj[, 1], holes_proj[, 2], holes_proj[, 3], 
            radius = .ptRad, col = ptCol)
  ## Create and add ellipsoid for full sample 
  ellips <- 
    ellipse3d(cov(holes_proj[, 1:3]), level = 0.68,
              centre = apply(holes_proj, 2, mean))
  wire3d(ellips,  add = T, col = "grey", alpha = .a_surface, shininess = .shine)
  
  ## Create and add ellipsoid for each species
  for (i in 1:length(ptCol_pal)){
    .dat      <- holes_proj[ptCol == ptCol_pal[i], ]
    cl_ellips <- ellipse3d(cov(.dat[, 1:3]), level = 0.68,
                           centre = apply(.dat, 2, mean))
    wire3d(cl_ellips,  add = T, lwd = 1.5,
           col = ptCol_pal[i], alpha = .a_surface, shininess = .shine)
  }
  
  scene_pca_kde3d <- scene3d()
  
  output$widget_pca_kde3d <- renderRglwidget(
    rglwidget(scene_pca_kde3d)
  )
  
  
  ##### holes_kde2d =====
  ## via MASS::kde2d()
  ## Can't seem to get type="wire" and some other options working
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  open3d(FOV = 0)
  
  ## Plot 2D scatterplot
  spheres3d(holes_proj[, 1], holes_proj[, 2], rep(0, n), 
            radius = .ptRad, col = ptCol)
  ## Add kde2d wire surface
  persp3d(holes_kde2d, add = T,
          col = "red", alpha = .a_surface, shininess = .shine)
  
  scene_holes_kde2d <- scene3d()
  
  output$widget_holes_kde2d <- renderRglwidget(
    rglwidget(scene_holes_kde2d)
  )
  
  
  ##### logLik =====
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  open3d(FOV = 0)
  
  ## Calculation init
  x   <- rgamma(100, shape = 5, rate = 0.1)
  fit <- fitdistr(x, dgamma, list(shape = 1, rate = 0.1), lower = 0.001)
  loglik <- function(shape, rate)
    sum(dgamma(x, shape = shape, rate = rate, log = TRUE))
  loglik <- Vectorize(loglik)
  
  xlim <- fit$estimate[1] + 4 * fit$sd[1] * c(-1, 1)
  ylim <- fit$estimate[2] + 4 * fit$sd[2] * c(-1, 1)
  
  ## Rendering
  mfrow3d(1, 2, sharedMouse = TRUE)
  ## (left) log-likelihood surface scaling to x and y limits.
  persp3d(loglik,
          xlim = xlim, ylim = ylim,
          n = 30)
  
  ## (right) log-likelihood surface scaling all 3 axes limits.
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
  open3d(FOV = 0)
  
  ## Init
  load(file = "./data/df_func_surface2.rda") ## Brings df into global environment.
  ..ptRad <- diff(range(df$x1)) * .ptRad
  ..pal   <- c("blue", "red", "cyan", "purple")
  ..y_nms <- paste0("y", c(1:2, 1.5, 3))
  ..y_idx <- 4:7
  
  
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  open3d(FOV = 0, zoom = 1)
  bbox3d(#xlen = 0, ylen = 0, zlen = 0,
    color = "black" , alpha = .a_bbox, emission = .bg, lwd = 1)
  mfrow3d(2, 2, sharedMouse = TRUE)
  for (i in 1:length(..y_idx)){
    ## Create df subsets
    ..df     <- df[, c(1:2, ..y_idx[i])]
    ## Create convex hulls
    ..c.hull <- t(geometry::convhulln(..df))
    ## Create alpha hulls
    ashape_alpha <- .8 #Example explicitly uses 0; may impact fc_nms; columns at the end
    fc_nm        <- paste0("fc:", ashape_alpha)
    
    ..ashape_triang  <- ashape3d(as.matrix(..df), alpha = ashape_alpha)$triang
    ..rows_on_a.hull <- ..ashape_triang[,fc_nm] > 1 ## only rows on the a.hull
    ## values can be: 0 (not on a.hull) 1 (interitor triang), 2 (regular), 3 (singular)
    ..a.hull_triang  <- t(..ashape_triang[..rows_on_a.hull, 1:3])
    
    # ## Assign objects
    # ..df_nm <- paste0("df_", ..y_nms[i])
    # ..c.hull_nm <- paste0("c.hull_", ..y_nms[i])
    # ..a.hull_nm <- paste0("a.hull_", ..y_nms[i])
    # assign(..df_nm, ..df) ## Assign ..df subsets to df object: _ie_ df_y1
    # assign(..c.hull_nm, ..c.hull) ## Assign ..c.hull to a convhulln obj: _ie_ c.hull_y1
    ## Or try to Render -- not going swimmingly
    spheres3d(x = ..df[, 1], y = ..df[, 2], z = ..df[, 3],
              radius = ..ptRad, col = ..pal[i])

    triangles3d(..df[..a.hull_triang, 1],
                ..df[..a.hull_triang, 2],
                ..df[..a.hull_triang, 3],
                col = ..pal[i], alpha = .a_a.hull)
    if (i < length(..y_idx)) next3d() ## if not last plot then advance.
  }
  scene_functionSurfaces <- scene3d()
  
  output$widget_functionSurfaces <- renderRglwidget(
    rglwidget(scene_functionSurfaces)
  )
  
  ## Static functions
  .f1 = function(x, y) z = ((x^2) + (3 * y^2)) * exp(-(x^2) - (y^2))
  .f2 = function(x, y) z = (x^2)  + (y^3)
  
  ## Render
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  open3d(FOV = 0)
  mfrow3d(1, 2, sharedMouse = FALSE)
  plot3d(.f1, col = colorRampPalette(c("blue", "red")), alpha = .a_surface,
         xlab = "X", ylab = "Y", zlab = "Z", 
         xlim = c(-3, 3), ylim = c(-3, 3),
         aspect = c(1, 1, 0.5))
  next3d()
  plot3d(.f2, col = colorRampPalette(c("white", "black")), alpha = .a_surface,
         xlab = "X", ylab = "Y", zlab = "Z",
         xlim = c(-10, 10), ylim = c(-4, 4))
  scene_functionSurfaces_STATIC <- scene3d()
  
  output$widget_functionSurfaces_STATIC <- renderRglwidget(
    rglwidget(scene_functionSurfaces_STATIC)
  )
  
  ### widget_rotation -----
  ## !!NOTE .js file may have hardcoded names, change slowly and carfully.
  observe({
    ## Tells .www/rglwidgetaux.js to query the plot3d for its par3d
    input$queryumat
    session$sendInputMessage("ctrlplot3d", list("cmd"="getpar3d","rglwidgetId"="plot3d"))
  })
  
  output$usermatrix <- renderTable({
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
    ## Create scene
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
