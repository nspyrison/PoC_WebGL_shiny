####### _functionVis/server.r -----
#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison with gmail.com}

##### Setup -----
source("./ui.r", local = TRUE)
set.seed(20200629)
options(rgl.useNULL = TRUE) ## Must be executed BEFORE rgl is loaded on headless devices.

##TODO 
#- better simulation of two peak bi-variate dist
#- Average all of the function pts
#- pairs()/ splom-like display
#- kerl density vs a_hull

##### Global initialize -----
## Above server scope and not reactive
## Parameters:
{
  ## Some aesthetics setup:
  rad_pts        <- .02 ## radius of data points spheres
  alpha_bbox     <- .9  ## alpha opacity for boundingbox and axes lines
  alpha_a_hull   <- .2  ## alpha opacity for triangles of the exterior alpha hull
  alpha_red_bslice_agg <- 2 * alpha_a_hull ## alpha opacity for red lines indicating aggregating the bslice
  col_bg         <- "lightgrey" # "grey80" ## color for the back ground & "emission" arg
  col_bbox       <- "black"     # "grey20" ## color for the bounding box (widget cell)
  pal_surfaces   <- c("blue", "red", "cyan", "purple") # RColorBrewer::brewer.pal(n = 4, "Paired") 
  num_bbox_ticks <- 4
  d              <- 3  ## display dimensionality
  # alpha_surfaces <- .6 ## alpha opacity for surfaces and meshes
  # shine          <- 128  ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
}

app_hist <- function(gg_df, df_lb_ub){
  gg_df
  
  tidyr::pivot_longer(gg_df, cols = 1:4, names_to = "variable", values_to = "value")
  
  .med <- median(x)
  .bins <- nclass.FD(x) ## bins on Freedman-Diaconis, func(n, IQR).
  .den <-  density(x)
  .y_q1 <- 3 * summary(.den$y)[2] ## First quartile of the density of the variable
  .y_range <- diff(range(.den$y))
  .x_range <- diff(range(x))
  
  x <- as.data.frame(x)
  colnames(x) <- var_nm
  ggplot(x, aes_string(x = var_nm)) +
    geom_histogram(aes(y = ..density..), bins = .bins, colour = "black", fill = "grey") +
    #geom_density(alpha = .3, fill = "red") +
    geom_vline(aes(xintercept = .med),
               color = "blue", linetype = "dashed", size = 1) + 
    geom_rect(aes(ymin = -.y_q1, ymax = .y_q1, xmin = lb, xmax = ub),
              fill = "blue", alpha = .01) +
    facet_grid(rows = n_bd, vars(cyl))
    theme_void() + 
    theme(axis.title.x = element_text()) + 
    labs(x = var_nm) +
    coord_fixed(ratio =  .2 * .x_range / .y_range) ## Ratio of y/x
}




####### Shiny server start =====
server <- shinyServer(function(input, output, session) { ## Session required.
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  save <- options(rgl.inShiny = TRUE)
  on.exit({
    options(save); 
    try(rgl.close(), silent = TRUE)
  })
  
  output$widget_functionSurfaces_STALE <- renderRglwidget(
    rglwidget(scene_functionSurfaces_STALE)
  )
  
  ##### functionVis =====
  a_hull_alpha <- reactive(input$a_hull_alpha)
  output$a_hull_alpha <- 
    renderText(paste0("Alpha: ", round(a_hull_alpha(), 2)))
  output$a_hull_radius <- 
    renderText(paste0("Alpha hull radius: ", round(1 / a_hull_alpha(), 2)))
  
  dat_raw <- reactive({
    if (input$dat == "grid cube") {
      load(file = "./data/df_func_surface2.rda") ## Brings df into global environment.
      return(df[, 1:3])
    }
    if (input$dat == "simulation") {
      ## remove spaces
      in_a <- gsub(" ", "", input$sim_mns_a)
      in_b <- gsub(" ", "", input$sim_mns_b)
      ## as.vector, spliting on ","
      mns_a <- as.numeric(unlist(strsplit(in_a, ",")))
      mns_b <- as.numeric(unlist(strsplit(in_b, ",")))
      if (length(mns_a) != length(mns_a)) stop("Means of different length.")
      
      ## Initialize
      p <- length(mns_a)
      mns <- list(mns_a, mns_b)
      sigs <- diag(p)
      sigs <- list(sigs, sigs)
      ## Simulate
      source("../../spinifex_study/R/sim_pDim_kCl.r")
      sim <- sim_pDim_kCl(means = mns, 
                          sigmas = sigs,
                          n_points = rep(list(500), length(mns)),
                          method = "eigen",
                          do_shuffle = FALSE)
      ## Discretize to grid; will be rescaled to [0,1] later
      grain <- .25
      sim <- apply(sim, 2, function(x) round(x / grain, 1) * grain)
      return(sim)
    }
  })
  
  ## character vector of the hex colors to use based on first factor or character column
  pt_color <- reactive({
    dat_raw <- dat_raw()
    dat_raw <- dat_raw[complete.cases(dat_raw), ] ## Row-wise complete
    IS_fct_col <- sapply(dat_raw, function(col){
      is.factor(col) | is.character(col)
    })
    
    ## If not fct|char columns give dummy color
    if (sum(IS_fct_col) == 0) return(rep(col_of("NO_FCT|CHAR"), nrow(dat_raw))) 
    fst_fct_col_num <- Position(function(x) x == T, IS_fct_col) ## Number of first column that is fct|char
    fst_class <- dat_raw[, fst_fct_col_num]
    col_of(fst_class)
  })
  
  dat_dmvn <- reactive({
    dat_raw <- dat_raw()
    IS_numeric_col <- apply(dat_raw, 2, function(x) all(is.numeric(x)))
    dat_num <- dat_raw[, IS_numeric_col]
    dat_mn  <- colMeans(dat_num)
    dat_cov <- cov(dat_num)
    dmvn <- mvtnorm::dmvnorm(dat_num, mean = dat_mn, sigma = dat_cov)
    dat_dmvn <- cbind(dat_num, dmvn)
    ret <- tourr::rescale(dat_dmvn) ## Rescale is crutial for a hull to work correctly and disp aes.
    as.data.frame(ret)
  })
  bd_col_nms <- reactive({
    dat <- dat_dmvn()
    p <- ncol(dat)
    x_num <- 1 ## Could change to input or hold one out style
    y_num <- 2 ## Could change to input or hold one out style
    z_num <- p ## Could be any function, but should be appended to end
    colnames(dat)[-c(x_num, y_num, z_num)] ## Column numbers of dim not displayed
  })
  fd_col_nms <- reactive({
    dat <- dat_dmvn()
    p <- ncol(dat)
    x_num <- 1 ## Could change to input or hold one out style
    y_num <- 2 ## Could change to input or hold one out style
    z_num <- p ## Could be any function, but should be appended to end
    colnames(dat)[c(x_num, y_num, z_num)] ## Column numbers of dim not displayed
  })
  dat_bd <- reactive({
    dat_dmvn()[bd_col_nms()]
  })
  dat_fd <- reactive({
    dat_dmvn()[fd_col_nms()]
  })
  
  dat_star <- reactive({
    req(input$bd_slice_1)
    ## Data frame of full sample the back dimension data
    dat_bd <- dat_bd()
    ## Sub set to only the rows within ALL back dimension slices
    IS_in_bd_slice_mat  <- NULL ## Logical matrix of rows in each back dimenion slice
    IS_in_all_bd_slices <- T    ## Logical vector of rows in ALL back dimenion slices
    for(i in 1:ncol(dat_bd)){
      dim         <- dat_bd[, i]
      slice       <- input[[paste0("bd_slice_", i)]]
      lb          <- min(slice)
      ub          <- max(slice)
      IS_in_slice <- dim >= lb & dim <= ub ## Logical vector of rows within this .dim's slice.
      IS_in_bd_slice_mat  <- cbind(IS_in_bd_slice_mat, IS_in_slice)
      IS_in_all_bd_slices <- IS_in_all_bd_slices & IS_in_slice
    }
    if (sum(IS_in_all_bd_slices) == 0) stop("Currect slices of the back dimensions contain no observations.")
    
    dat         <- as.data.frame(dat_dmvn())
    dat$rownum  <- 1:nrow(dat)
    IS_disp_col <- !(colnames(dat) %in% colnames(dat_bd))
    ## A df subset (with all bd slice), of the front (display) dimensions
    dat_star    <- dat[IS_in_all_bd_slices, IS_disp_col]
    
    ## Agggregate the function values within bd slices
    #req(input$bslice_agg)
    agg_func <- max ## case when doesn't want to return functions; closure not subsettable.
      # if(       input$bslice_agg == "max")    {max
      # } else if(input$bslice_agg == "mean")   {mean
      # } else if(input$bslice_agg == "median") {median
      # } else if(input$bslice_agg == "min")    {min}
    
    colnames(dat_star) <- c("x1", "x2", "func", "rownum")
    agg_df <- dplyr::group_by(dat_star, x1, x2) %>%
      dplyr::summarise(.groups = "drop", 
                       z_agg = agg_func(func), 
                       z_min = min(func), 
                       z_max = max(func), 
                       rownum = first(rownum)) %>% 
      as.data.frame()
    
    ## Return df of aggregated dat_star; rows in all bd slices, columns: x1:x2, y_mn, y_min, y_man
    agg_df
  })
  
  ## Creates a list of 3D Delaunay triangulation matrices, as a function of alpha(s).
  #### Contains list elements for each of the shapes: (tetra, triang, edge, vertex, x),
  #### Delaunay triangulations maximizes the smallest angle of the triangles to avoid sliver triangles.
  full_ashape <- reactive({
    dat_star_3mat  <- as.matrix(dat_star()[c("x1", "x2", "z_agg")])
    ## Possible alpha values for the a_hull_alpha()
    a_hull_alpha_seq <- seq(.1, 10, by = .1) ## fixed to 1 atm
    ## ashape3d obj of all alphas and all shapes (tetra, triang, edge, vertex, x)
    alphashape3d::ashape3d( ## Expects numeric matrix in 3 dimensions.
      x = dat_star_3mat, alpha = a_hull_alpha_seq, pert = TRUE)
  })
  
  scene_functionVis <- reactive({
    req(dat_star())
    ##### Init a_hull triangles via alphashape3d::ashape3d()
    ## Grab only the triangles from the 3D Delaunay triangulation
    ashape_triang   <- suppressWarnings(full_ashape()$triang) ## Suppresses: "Warning: Duplicate points were removed."
    ## Column name specifying the current alpha value:
    alpha_col_nm    <- paste0("fc:", a_hull_alpha())
    alpha_col_num   <- which(colnames(ashape_triang) == alpha_col_nm)
    ## rows of the exterior triangles
    rows_ext_triang <- ashape_triang[, alpha_col_num] == 2
    #### Values can be: 0 (not on a_hull) 1 (interitor triang), 2 (regular), 3 (singular)
    ## Triangles to display; only those on the exterior of the alpha hull.
    xyz_rows_ext_triang <- t(ashape_triang[rows_ext_triang, 1:3])
    ## df of X[r, 6], rows within all bd slices, columns: x1, x2, rownum, z_agg, z_min, z_max
    
    ## Aesthetic init
    dat_star   <- dat_star()
    bd_col_nms <- bd_col_nms()
    fd_col_nms <- fd_col_nms()
    labs <- paste0(c("x, ", "y, ", "z, "), fd_col_nms)
    if(all.equal(dat_star$z_min, dat_star$z_max) == FALSE)
      labs[3] <- paste0("z, mean of ", fd_col_nms(3))
    pt_col <- pt_color()[dat_star$rownum]
    disp_df <- dat_dmvn()[fd_col_nms] ## Full disp cols for setting aspect ratio
    x_asp <- 1 / diff(range(disp_df[, 1]))
    y_asp <- 1 / diff(range(disp_df[, 2]))
    z_asp <- 1 / diff(range(disp_df[, 3]))
    
    ##### rgl scene graphics 
    try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
    open3d(FOV = 0, zoom = 1)
    title3d(xlab = labs[1], ylab = labs[2], zlab = labs[3])
    aspect3d(x_asp, y_asp, z_asp)
    bbox3d(xlen = num_bbox_ticks, ylen = num_bbox_ticks, zlen = num_bbox_ticks,
           color = col_bbox , alpha = alpha_bbox, emission = col_bg, lwd = 1)
    
    ### Data point Spheres
    spheres3d(x = dat_star$x1, y = dat_star$x2, z = dat_star$z_agg,
              radius = rad_pts, col = pt_col)
    ### Add red aggregation lines if needed
    if(sum(dat_star$z_min == dat_star$z_max) != nrow(dat_star)) {
      segments3d(color = "red", alpha = alpha_red_bslice_agg,
                 rep(dat_star$x1, each = 2), 
                 rep(dat_star$x2, each = 2), 
                 c(rbind(dat_star$z_min, dat_star$z_max)))
    }
    ### a_hull triangs
    if (input$DO_DISP_a_hull_triang) {
      triangles3d(dat_star[xyz_rows_ext_triang, "x1"],
                  dat_star[xyz_rows_ext_triang, "x2"],
                  dat_star[xyz_rows_ext_triang, "z_agg"],
                  col = pal_surfaces[1], alpha = alpha_a_hull)
    }
    
    scene_functionVis <- scene3d()
    scene_functionVis
  })
  output$widget_functionVis <- renderRglwidget(
    rglwidget(scene_functionVis())
  )
  
  ##### Back dimensions ui ----
  output$back_dimensions_ui <- renderUI({
    dat <- dat_dmvn()
    p   <- ncol(dat)
    dat_bd <- dat_bd()
    bd_col_nms <- colnames(dat_bd)
    ## Half of the targeted relative variable volume
    half_def_rel_size <- .5 * input$tgt_rel_h^(1 / (p - d))
    
    ## Make slider numeric inputs for the back dimension slice
    ncol_bd <- ncol(dat_bd)
    if(ncol_bd == 0) stop("dat_bd() not returning columns.")
    i_s <- 1:ncol_bd
    bd_slice_midpts <- lapply(i_s, function(i) {
      med <- median(dat_bd[, i])
      sliderInput(inputId = paste0("bd_slice_", i), 
                  label = paste0(bd_col_nms[i], " slice midpoint"),
                  min = 0, max = 1, step = .05,
                  value = c(med - half_def_rel_size, ## Creates a width slider when you give 2 values.
                            med + half_def_rel_size))
    })
    
    ## Layout the midpoint and width sliders in a row
    lapply(i_s, function(i) { column(7, bd_slice_midpts[[i]]) })
  }) ## Assign output$back_dimensions_ui, closing RenderUI()
  
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
      n_bins <- nclass.FD(dim) ## bins on Freedman-Diaconis, func(n, IQR).
      x_min  <- min(den$x)
      x_max  <- max(den$x)
      y_min  <- min(den$y)
      y_max  <- max(den$y)
      asp_r  <- .4 * (y_max - y_min) / (x_max - x_min)
      lb     <- min(slice)
      ub     <- max(slice)
      
      this_bd_stats <- data.frame(var_nm = var_nm,
                                  med    = med,
                                  n_bins = n_bins, ## bins on Freedman-Diaconis, func(n, IQR).
                                  x_min  = x_min,
                                  x_max  = x_max,
                                  y_min  = y_min,
                                  y_max  = y_max,
                                  asp_r  = asp_r, ## Ratio of y / x
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
                 color = "blue", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = lb), color = "blue", size = 1) +
      geom_vline(aes(xintercept = ub), color = "blue", size = 1) +
      geom_rug(size = 1) +
      geom_density(alpha = .3, fill = "red") +
      labs(x = var_nm) +
      facet_wrap(~ var_nm, ncol = 1) +
      theme_void() +
      coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) #+
      #coord_fixed(ratio = vars(asp_r)) 
    
    gg_blank <- ggplot() + theme_void()
    
    ## Display in order with white space at the top to allign with sliders.
    bd_histograms <- cowplot::plot_grid(
      gg_blank, bd_hists, ncol = 1
      #labels = c(NA, bd_col_nms), ncol = 1
    )
    
    bd_histograms
  })
  output$bd_histograms <- renderPlot(bd_histograms())
  
}) ## Assign server function to be used in shinyApp()

shinyApp(ui = ui, server = server)
