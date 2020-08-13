####### _functionVis/server.r -----
#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison with gmail.com}

##### Setup -----
source("./ui.r", local = TRUE)
source("./R/sim_pDim_kCl_STATIC.r")
set.seed(20200629)
options(rgl.useNULL = TRUE) ## Must be executed BEFORE rgl is loaded on headless devices.

##TODO 
# surface: a-hull vs quads of grid.

##### Global initialize -----
## Above server scope and not reactive
## Parameters:
{
  ## Some aesthetics setup:
  alpha_a_hull   <- .2  ## alpha opacity for triangles of the exterior alpha hull
  pal            <- RColorBrewer::brewer.pal(n = 4, "Set2")
  col_pts        <- pal[1]
  col_surfaces   <- pal[2]
  num_bbox_ticks <- 1L ## in addition to 0 tick
  d              <- 3L ## Display dimensionality, number of front dimensions inc function
  # alpha_surfaces <- .6 ## alpha opacity for surfaces and meshes
  # shine          <- 128  ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
}

####### Shiny server start -----
server <- shinyServer(function(input, output, session) { ## Session required.
  ## on.start:
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  save <- options(rgl.inShiny = TRUE)
  on.exit({
    options(save); 
    try(rgl.close(), silent = TRUE)
  })
  
  output$widget_functionSurfaces_STALE <- renderRglwidget(
    rglwidget(scene_functionSurfaces_STALE)
  )
  
  ##### functionVis -----
  grid_pts <- reactive({
    req(input$grid_pts)
    input$grid_pts
  })
  
  ## 1) dat_raw(); rescaled input data -----
  dat_raw <- reactive({
    if (input$dat == "cube") {
      cube <- geozoo::cube.solid.grid(p = 3L, n = grid_pts())$points %>% 
        tourr::rescale() %>% 
        as.data.frame()
      p <- ncol(cube)
      colnames(cube) <- paste0("V", 1:p)
      return(cube)
    }
    if (input$dat == "simulation") {
      ## Remove spaces
      in_a <- gsub(" ", "", input$sim_mns_a)
      in_b <- gsub(" ", "", input$sim_mns_b)
      ## as.vector, delimiting on ","
      mns_a <- as.numeric(unlist(strsplit(in_a, ",")))
      mns_b <- as.numeric(unlist(strsplit(in_b, ",")))
      if (length(mns_a) != length(mns_a)) stop("Means of different length.")
      
      ## Initialize
      p <- length(mns_a)
      mns <- list(mns_a, mns_b)
      sigs <- diag(p)
      sigs <- list(sigs, sigs)
      ## Simulate
      sim <- sim_pDim_kCl(means = mns, 
                          sigmas = sigs,
                          cl_points = rep(list(1000), length(mns)),
                          method = "eigen",
                          do_shuffle = FALSE)
      ## rescale to [0,1]
      sim_std <- tourr::rescale(sim)
      ## Discretize to grid
      rnd_near <- 1/ grid_pts()
      sim_std_disc <- apply(sim_std, 2L, function(x){ 
        round(x / rnd_near, 0L) * rnd_near}
      ) %>% as.data.frame()
      
      return(sim_std_disc)
    }
  })
  dat_fd <- reactive({
    dat <- dat_raw()
    fd_nums <- c(1L, 2L) ## Could change to input or hold one out style
    fd_nms <- colnames(dat)[fd_nums]
    dat[fd_nms]
  })
  dat_bd <- reactive({
    dat <- dat_raw()
    fd_nums <- c(1L, 2L) ## Could change to input or hold one out style
    bd_nums <- setdiff(1:ncol(dat), fd_nums)
    bd_nms <- colnames(dat)[bd_nums]
    dat[bd_nms]
  })
  
  
  
  ## 2) dat_star(); obs in back dimensions -----
  dat_star <- reactive({
    dat_bd   <- dat_bd()
    ncol_bd  <- ncol(dat_bd)
    if (ncol_bd == 0) return()
    ## At least 1 bd:
    req(input$bd_slice_1)
    dat_raw <- dat_raw()
    
    ## Subset to only the rows within all back dimension slices
    IS_in_all_bd_slices <- TRUE ## Logical vector of rows in ALL back dimenion slices
    for(i in 1:ncol_bd){
      dim         <- dat_bd[, i]
      slice       <- input[[paste0("bd_slice_", i)]]
      lb          <- min(slice)
      ub          <- max(slice)
      IS_in_slice <- data.frame(dim >= lb & dim <= ub) ## Logical vector of rows within this .dim's slice.
      IS_in_all_bd_slices <- IS_in_all_bd_slices & IS_in_slice
    }
    if (sum(IS_in_all_bd_slices) == 0) stop("Currect slices of the back dimensions contain no observations.")
    ## A df subset (with all bd slice)
    dat_star <- dat_raw[IS_in_all_bd_slices, ]
    
    ## the subset of dat_raw that is in all back dimension slices
    return(dat_star)
  })
  
  
  
  ## 3) dat_func(); apply function -----
  #### df of the the display-x, -y, and function before aggregation
  dat_func <- reactive({
    dat_star <- dat_star()
    
    if (input$func_nm == "kde2d"){
      ## kde2d on only the front dimensions, of the subset obs in the bd slices
      den2d <- MASS::kde2d(dat_star[, 1L], dat_star[, 2L], n = grid_pts())
      den2d_cross_join <- merge(den2d$x, den2d$y, all = TRUE)
      dat_func <- data.frame(den2d_cross_join, as.vector(den2d$z))

    }
    if (input$func_nm == "dmvnorm"){
      ## dmvnorm on ALL var, subset to the bd slices
      col_mns <- colMeans(dat_star)
      covar <- cov(dat_star)
      print(input$dat)
      print(col_mns)
      print(covar)
      dmvn <- mvtnorm::dmvnorm(dat_star, mean = col_mns, sigma = covar)
      dat_func <- cbind(dat_star[, 1:2], dmvn)
    }
    ## Rescale is crutial for a hull to work disp aspect ratio.
    dat_func <- tourr::rescale(dat_func) %>%
      as.data.frame()
    colnames(dat_func) <- c(paste0("V", 1L:2L), "func")
    
    ## Aggregate across the slice of the backdimensions
    agg_func <- max ## Previously used input$bslice_agg
    
    dat_func <- dat_func %>% dplyr::group_by(V1, V2) %>% 
      dplyr::summarise(.groups = "drop",
                       z_agg = agg_func(func),
                       z_min = min(func),
                       z_max = max(func)) %>% 
      as.data.frame()
    
    ## df of 3 front dimensions
    return(dat_func)
  })
  
  
  
  ## 4) full_ashape(); superset of triangles -----
  ## Creates a list of 3D Delaunay triangulation matrices, as a function of alpha(s).
  #### Contains list elements for each of the shapes: (tetra, triang, edge, vertex, x),
  #### Delaunay triangulations maximizes the smallest angle of the triangles to avoid sliver triangles.
  full_ashape <- reactive({
    if(input$DO_DISP_a_hull_triang == TRUE){
      dat_star_3mat <- as.matrix(dat_func()[, 1L:3L])
      ## Possible alpha values for the input$a_hull_alpha
      a_hull_alpha_seq <- seq(.1, .2, by = .01)
      ## ashape3d obj of all alphas and all shapes (tetra, triang, edge, vertex, x)
      ret <- alphashape3d::ashape3d( ## Expects numeric matrix in 3 dimensions.
        x = dat_star_3mat, alpha = a_hull_alpha_seq, pert = TRUE)
      return(ret)
    }
  })
  
  ## 5) Render scene -----
  scene_functionVis <- reactive({
    req(dat_func())
    
    ## Init
    dat_star   <- dat_func()
    func_nm    <- input$func_nm
    agg_nm     <- "max" #input$bslice_agg 
    bd_col_nms <- colnames(dat_bd())
    fd_col_nms <- c(colnames(dat_fd()), paste0(agg_nm, " ", func_nm))
    labs <- paste0(c("x, ", "y, ", "z, "), fd_col_nms)

    
    ##### rgl scene graphics 
    try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
    open3d(FOV = 0L, zoom = .8) ## Zoom is 1/magification
    title3d(xlab = labs[1L], ylab = labs[2L], zlab = labs[3L])
    bbox3d(xlen = num_bbox_ticks, ylen = num_bbox_ticks, zlen = num_bbox_ticks,
           lwd = 1L)
    ## Change h/w in ui, applied to rglWidgetOutput().
    
    ### Data point Spheres
    spheres3d(x = dat_star[, 1L], y = dat_star[, 2L], z = dat_star[, 3L],
              radius = .5 / grid_pts(), col = col_pts)
    ### Add red aggregation lines if needed
    if(all(dat_star$z_min == dat_star$z_max) == FALSE) {
      segments3d(color = "red", alpha = 2L * alpha_a_hull,
                 rep(dat_star[, 1L], each = 2L), 
                 rep(dat_star[, 2L], each = 2L), 
                 c(rbind(dat_star$z_min, dat_star$z_max)))
    }
    ### Add a_hull triangs, if needed
    if (input$DO_DISP_a_hull_triang == TRUE) {
      ##### Init a_hull triangles via alphashape3d::ashape3d()
      ## Grab only the triangles from the 3D Delaunay triangulation
      ashape_triang  <- suppressWarnings(full_ashape()$triang) ## Suppresses: "Warning: Duplicate points were removed."
      ## Column name specifying the current alpha value:
      alpha_col_nm    <- paste0("fc:", input$a_hull_alpha)
      alpha_col_num   <- which(colnames(ashape_triang) == alpha_col_nm)
      ## rows of the exterior triangles
      rows_ext_triang <- ashape_triang[, alpha_col_num] == 2L
      #### Values can be: 0 (not on a_hull) 1 (interitor triang), 2 (regular), 3 (singular)
      ## Triangles to display; only those on the exterior of the alpha hull.
      xyz_rows_ext_triang <- t(ashape_triang[rows_ext_triang, 1L:3L])
      ## df of the exterior triangles of the bd slices, columns: V1, V2, z_agg, z_min, z_max
      
      ##### Add the triangles
      triangles3d(dat_star[xyz_rows_ext_triang, "V1"],
                  dat_star[xyz_rows_ext_triang, "V2"],
                  dat_star[xyz_rows_ext_triang, "z_agg"],
                  col = col_surfaces[1], alpha = alpha_a_hull)
    }
    
    scene_functionVis <- scene3d()
    scene_functionVis
  })
  output$widget_functionVis <- renderRglwidget(
    rglwidget(scene_functionVis())
  )
  
  ##### Back dimension slices ui ----
  output$bd_slices_ui <- renderUI({
    dat <- dat_raw()
    p   <- ncol(dat)
    dat_bd <- dat_bd()
    bd_col_nms <- colnames(dat_bd)
    ## Half of the targeted relative variable volume
    half_tgt_slice <- .5 * input$tgt_rel_h^(1L / (p - d))
    
    ## Make slider numeric inputs for the back dimension slice
    ncol_bd <- ncol(dat_bd)
    if(ncol_bd == 0L) return("There are no back dimensions.")
    i_s <- 1:ncol_bd
    bd_slice_midpts <- lapply(i_s, function(i) {
      med <- median(dat_bd[, i])
      sliderInput(inputId = paste0("bd_slice_", i), 
                  label = paste0(bd_col_nms[i], " slice midpoint"),
                  min = 0L, max = 1L, step = .05,
                  value = c(med - half_tgt_slice, ## Creates a width slider when you give 2 values.
                            med + half_tgt_slice))
    })
    
    ## Layout the midpoint and width sliders in a row
    lapply(i_s, function(i) { bd_slice_midpts[[i]] })
  }) ## Assign output$back_dimensions_ui, closing RenderUI()
  
  ## Front dimension histograms -----
  fd_histograms <- reactive({
    ### fd histograms:
    dat_fd <- dat_fd()#dat_func()
    bd_col_nms <- colnames(dat_fd)
    i_s <- 1:ncol(dat_fd)
    
    ## Find aggregated stats for ear variable
    df_fd_stats <- NULL
    for (i in i_s) {
      dim <- dat_fd[, i]
      den <- density(dim)
      
      var_nm <- as.factor(bd_col_nms[i])
      x_min  <- min(den$x)
      x_max  <- max(den$x)
      y_min  <- min(den$y)
      y_max  <- max(den$y)
      asp_r  <- .4 * (y_max - y_min) / (x_max - x_min)
      
      this_fd_stats <- data.frame(var_nm = var_nm,
                                  x_min  = x_min,
                                  x_max  = x_max,
                                  y_min  = y_min,
                                  y_max  = y_max,
                                  asp_r  = asp_r)
      df_fd_stats <- rbind(df_fd_stats, this_fd_stats)
    }
    
    ## Pivot longer the back dimensions for faceting
    tib_fd_long <- tidyr::pivot_longer(dat_fd, cols = 1:ncol(dat_fd),
                                       names_to = "var_nm", values_to = "value")
    # ## Left Join, the long table and the aggregated stats
    tib_fd_long_stats <- dplyr::left_join(tib_fd_long, df_fd_stats,
                                          by = "var_nm", copy = TRUE)
    
    ## Single column of stylized histograms of each backdimension
    fd_hists <- ggplot(data = tib_fd_long_stats, aes(x = value)) +
      geom_rug(aes(y = 0L), size = 1L) + #, position = "jitter") +
      geom_density(alpha = .3, fill = "red") +
      labs(x = var_nm) +
      facet_wrap(~ var_nm, ncol = 1L) +
      theme_void()
    ## Return
    return(fd_hists)
  })
  output$fd_histograms <- renderPlot(fd_histograms())
    
    
  ## Back dimension histograms -----
  bd_histograms <- reactive({
    req(input$bd_slice_1)
    dat_bd <- dat_bd()
    bd_col_nms <- colnames(dat_bd)
    i_s <- 1:ncol(dat_bd)
    
    ## Find aggregated stats for ear variable
    df_bd_stats <- NULL
    for (i in i_s) {
      dim   <- dat_bd[, i]
      den   <- density(dim)
      slice <- input[[paste0("bd_slice_", i)]]
      
      var_nm <- as.factor(bd_col_nms[i])
      med    <- median(dim)
      x_min  <- min(den$x)
      x_max  <- max(den$x)
      y_min  <- min(den$y)
      y_max  <- max(den$y)
      asp_r  <- .4 * (y_max - y_min) / (x_max - x_min)
      lb     <- min(slice)
      ub     <- max(slice)
      
      this_bd_stats <- data.frame(var_nm = var_nm,
                                  med    = med,
                                  x_min  = x_min,
                                  x_max  = x_max,
                                  y_min  = y_min,
                                  y_max  = y_max,
                                  asp_r  = asp_r,
                                  lb     = lb,
                                  ub     = ub)
      df_bd_stats <- rbind(df_bd_stats, this_bd_stats)
    }
    
    ## Pivot longer the back dimensions for faceting
    tib_bd_long <- tidyr::pivot_longer(dat_bd, cols = 1:ncol(dat_bd),
                                       names_to = "var_nm", values_to = "value")
    # ## Left Join, the long table and the aggregated stats
    tib_bd_long_stats <- dplyr::left_join(tib_bd_long, df_bd_stats,
                                          by = "var_nm", copy = TRUE)
    
    ## Single column of stylized histograms of each backdimension
    bd_hists <- ggplot(data = tib_bd_long_stats, aes(x = value)) +
      #geom_histogram(aes(y = ..density..), bins = n_bins, colour = "black", fill = "grey") +
      geom_rect(aes(ymin = y_min, ymax = y_max, xmin = lb, xmax = ub), 
                fill = "lightblue", alpha = .05) +
      geom_vline(aes(xintercept = med),
                 color = "blue", linetype = "dashed", size = 1L) +
      geom_vline(aes(xintercept = lb), color = "blue", size = 1L) +
      geom_vline(aes(xintercept = ub), color = "blue", size = 1L) +
      geom_rug(aes(y = 0L), size = 1L) + #, position = "jitter") +
      geom_density(alpha = .3, fill = "red") +
      labs(x = var_nm) +
      facet_wrap(~ var_nm, ncol = 1L) +
      theme_void()
    ## Return
    return(bd_hists)
  })
  output$bd_histograms <- renderPlot(bd_histograms(), height = 400, width = 200)
  
}) ## Assign server function to be used in shinyApp()

shinyApp(ui = ui, server = server)
