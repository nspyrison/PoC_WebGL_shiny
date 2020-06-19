### _rgl_lite.r -----
## A lighter version of _NicholasSpyrison_rgl

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
  n_col <- length(unique(ptCol))
  
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
server <- shinyServer(function(input, output, session) {
  app_CloseRGL()
  save <- options(rgl.inShiny = TRUE)
  on.exit({options(save); app_CloseRGL()})
  
  ##### rb2holes =====
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  updateSliderInput(session, "rb2holes_basis_slider", 
                    value = 1, step = 1, min = 1, max = n_tpath_bases)
  
  slider <- reactive(input$rb2holes_basis_slider)
  slider_t <- throttle(slider, millis = 50) 
  output$slider_t <- renderText(slider_t())
  ## Reactive function, throttled slider value, returning value every 'millis' milliseconds, 
  #### _ie_ "Try to return value even while manipulating slider every .05 sec."
  
  
  rb2holes_rglwidget <- reactive({
    app_CloseRGL()
    req(slider_t())
    ## Selected projection basis and convex box from input slider.
    this_proj <- rb2holes_proj[,, slider_t()]
    this_convex_box <- basis_convex_box[,, slider_t()]
    
    ## Open scene with isopectric perspective
    open3d(FOV = 0)
    ## Plot projection points
    spheres3d(this_proj[, 1], this_proj[, 2], this_proj[, 3], 
              radius = ptRad, col = ptCol)
    
    ## Prop up basis and fulll tour convex bounding box
    spheres3d(this_convex_box[, 1], this_convex_box[, 2], this_convex_box[, 3], 
              radius = ptRad / 2, col = "black")
    spheres3d(tour_convex_box[, 1], tour_convex_box[, 2], tour_convex_box[, 3], 
              radius = ptRad / 2, col = "grey")
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
  
})


shinyApp(ui = ui, server = server)
