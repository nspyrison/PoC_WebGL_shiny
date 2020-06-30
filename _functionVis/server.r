####### _functionVis/server.r -----
#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison@gmail.com}

##### Setup -----
source("./ui.r", local = TRUE)
set.seed(20200629)
options(rgl.useNULL = TRUE) ## Must be executed BEFORE rgl is loaded on headless devices.

##### exmples: 
## http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
## https://cran.r-project.org/web/packages/rgl/vignettes/rgl.html


##### Global initialize -----
## Above server scope and not reactive
## Parameters:
{
  ## Some aesthetics setup:
  .ptRad     <- .02 ## size (radius? diameter?) of data points
  .shine     <- 128 ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
  .a_bbox    <- .6 ## alpha for boundingbox and axes lines
  .a_surface <- .6 ## alpha for surfaces and meshes
  .a_a.hull  <- .2 ## alpha for many triangles of the alpha hull
  .bg        <- "lightgrey" ##"grey100" ## lighter grey Back Ground
  .bb        <- "darkgrey"  ##"grey40"  ## darker grey  Bounding Box
  .pal       <- RColorBrewer::brewer.pal(3, "Paired")
  
  ## Data, dim, basis, ases init
  dat       <- tourr::rescale(tourr::flea[, 1:6])
  n         <- nrow(dat)
  p         <- ncol(dat)
  d         <- 3
  rb        <- tourr::basis_random(p, d)
  ptCol     <- spinifex::col_of(tourr::flea$species)
  ptPch     <- spinifex::pch_of(tourr::flea$species)
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
  
  ## Offset tour to be in the "first quadrant"
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
  
  ##### functionVis =====
  dat_dmvn <- reactive({
    dat <- tourr::flea[, 1:6]
    dat <- tourr::rescale(dat)
    dat_mn  <- apply(dat, 2, mean)
    dat_cov <- cov(dat)
    dmvn <- mvtnorm::dmvnorm(dat, mean = dat_mn, sigma = dat_cov)
    cbind(dmvn, dat)
  })
  
  dat_star <- reactive({
    .dat <- dat_dmvn()
    dim_nms <- colnames(.dat)
    p <- ncol(.dat)
    d <- 3
    bd <- p-d
    
    i_s <- bd:p
    bd_cond_mat <- NULL
    apply(i_s, 2, function(i){
      .dim   <- .dat[,i]
      .range <- abs(max(.dim) - min(.dim))
      .midpt <- input[[paste0("bd_slider_", i)]]
      .thickness <- input[[paste0("bd_sliceSize_", i)]] * .range
      .lb <- .midpt - .thickness / 2
      .ub <- .midpt + .thickness / 2
      .cond <- .dim >= .lb & .dim <= .ub
      bd_cond_mat <- cbind(bd_cond, .cond)
    })
    
    j_s <- i_s - d
    bd_cond_vect <- T
    apply(j_s, 2, function(i){
      .increment <- bd_cond_vect[, i]
      bd_cond_vect <- bd_cond_vect & .increment
    })
    
    .dat[bd_cond_vect, ]
  })
  
  
  
  output$backDimensionInputs <- renderUI({
    .dat <- dat_dmvn()
    dim_nms <- colnames(.dat)
    p <- ncol(.dat)
    d <- 3
    bd <- p-d
    
    ## Make midpoint sliders
    i_s <- bd:p
    bd_sliders <- lapply(i_s, function(i) {
      .min <- min(.dat[, i])
      .max <- max(.dat[, i])
      .step <- round((.max - .min) / 10, 1)
      sliderInput(inputId = paste0("bd_slider_", i), 
                  label = paste(dim_nms[i], "slice midpoint"),
                  min = .min, max = .max, value = (.min + .max) /2, 
                  step = .step, round = -1)
    })
    ## Make thickness numeric inputs
    bd_thickness <- lapply(i_s, function(i) {
      numericInput(inputId = paste0("bd_sliceSize_", i), 
                   label = "rel slice size [h/r_max]",
                   min = 0, max = 1, value = .2, step = .05)
    })
    
    ## Make sliders and thickness into ordered rows.
    j_s <- 1:length(bd_sliders)
    lapply(j_s, function(j) {
      fluidRow(
        column(7, bd_sliders[[j]]),
        column(5, bd_thickness[[j]])
      )
    })
  })
  
  
  ##### STALE_functionSurfaces =====
  ## ON ICE, see same tab in \_NicholasSpyrison_rgl\ app for recency.
  
  ## Init
  load(file = "./data/df_func_surface2.rda") ## Brings df into global environment.
  ..ptRad <- diff(range(df$x1)) * .ptRad
  ..pal   <- c("blue", "red", "cyan", "purple")
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
    ..rows_on_a.hull <- ..ashape_triang[,fc_nm] == 2 ## only rows on the a.hull
    ## values can be: 0 (not on a.hull) 1 (interitor triang), 2 (regular), 3 (singular)
    ..a.hull_triang  <- t(..ashape_triang[..rows_on_a.hull, 1:3])
    
    ## Render
    spheres3d(x = ..df[, 1], y = ..df[, 2], z = ..df[, 3],
              radius = ..ptRad, col = ..pal[i])

    triangles3d(..df[..a.hull_triang, 1],
                ..df[..a.hull_triang, 2],
                ..df[..a.hull_triang, 3],
                col = ..pal[i], alpha = .a_a.hull)
    if (i < length(..y_idx)) next3d() ## if not last plot then advance.
  }
  scene_functionSurfaces_STALE <- scene3d()
  
  output$widget_functionSurfaces_STALE <- renderRglwidget(
    rglwidget(scene_functionSurfaces_STALE)
  )
  
})

shinyApp(ui = ui, server = server)
