####### _functionVis/server.r -----
#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison with gmail.com}

##### Setup -----
source("./ui.r", local = TRUE)
source("./R/sim_pDim_kCl_STATIC.r")
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
  num_bbox_ticks <- 4L
  d              <- 3L  ## display dimensionality
  # alpha_surfaces <- .6 ## alpha opacity for surfaces and meshes
  # shine          <- 128  ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
  
  load(file = "./data/df_func_surface2.rda") ## Brings df into global environment.
  
  ## local spinifex::col_of() to remove dev depandancy
  col_of <- function (class, pallet_name = "Dark2") {
    class <- as.factor(class)
    .l_lvls <- length(levels(class))
    if (.l_lvls == 0L) 
      stop("Length of 'class' cannot be zero.")
    if (.l_lvls > 12L) 
      stop("'class' has more than the expected max of 12 levels.")
    pal <- suppressWarnings(RColorBrewer::brewer.pal(.l_lvls, 
                                                     pallet_name))
    pal[as.integer(factor(class))]
  }
}

app_hist <- function(gg_df, df_lb_ub){
  gg_df
  
  tidyr::pivot_longer(gg_df, cols = 1L:4L, names_to = "variable", values_to = "value")
  
  .med <- median(x)
  .bins <- nclass.FD(x) ## bins on Freedman-Diaconis, func(n, IQR).
  .den <-  density(x)
  .y_q1 <- 3 * summary(.den$y)[2L] ## First quartile of the density of the variable
  .y_range <- diff(range(.den$y))
  .x_range <- diff(range(x))
  
  x <- as.data.frame(x)
  colnames(x) <- var_nm
  ggplot(x, aes_string(x = var_nm)) +
    geom_histogram(aes(y = ..density..), bins = .bins, colour = "black", fill = "grey") +
    #geom_density(alpha = .3, fill = "red") +
    geom_vline(aes(xintercept = .med),
               color = "blue", linetype = "dashed", size = 1L) + 
    geom_rect(aes(ymin = -.y_q1, ymax = .y_q1, xmin = lb, xmax = ub),
              fill = "blue", alpha = .01) +
    facet_grid(rows = n_bd, vars(cyl))
    theme_void() + 
    theme(axis.title.x = element_text()) + 
    labs(x = var_nm) +
    coord_fixed(ratio =  .2 * .x_range / .y_range) ## Ratio of y/x
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
  a_hull_alpha <- reactive(input$a_hull_alpha)
  output$a_hull_alpha <- 
    renderText(paste0("Alpha: ", round(a_hull_alpha(), 2L)))
  output$a_hull_radius <- 
    renderText(paste0("Alpha hull radius: ", round(1 / a_hull_alpha(), 2L)))
  
  ## 1) dat_raw(); rescaled input data -----
  dat_raw <- reactive({
    if (input$dat == "grid cube") {
      return(df[, 1L:3L]) ## df from load(file = "./data/df_func_surface2.rda")
    }
    if (input$dat == "simulation") {
      ## Remove spaces
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
      sim <- sim_pDim_kCl(means = mns, 
                          sigmas = sigs,
                          n_points = rep(list(500), length(mns)),
                          method = "eigen",
                          do_shuffle = FALSE)
      ## rescale to [0,1]
      sim_std <- tourr::rescale(sim)
      ## Discretize to grid
      sim_std_disc <- apply(sim_std, 2L, function(x) round(x, 1)) %>%
        as.data.frame()
      
      return(sim_std_disc)
    }
  })
  dat_fd <- reactive({
    dat <- dat_raw()
    x_num <- 1L ## Could change to input or hold one out style
    y_num <- 2L ## Could change to input or hold one out style
    dat[, c(x_num, y_num)]
  })
  dat_bd <- reactive({
    dat <- dat_raw()
    x_num <- 1L ## Could change to input or hold one out style
    y_num <- 2L ## Could change to input or hold one out style
    dat[, -c(x_num, y_num)]
  })
  
  ## 2) dat_func(); apply function -----
  #### df of the the display-x, -y, and function before aggregation
  dat_func <- reactive({
    dat_raw <- dat_raw()
    IS_numeric_col <- apply(dat_raw, 2L, function(x) all(is.numeric(x)))
    dat_num <- dat_raw[, IS_numeric_col]
    if (input$func_nm == "kde2d"){
      ## TODO; NEED TO GO TO MORE GENERALIZED KERNAL DENSITY????
      # rgl::ellipse3d(cov(z[, 1:3]), level = 0.68,
      #           centre = apply(z, 2, mean))
      den2d <- MASS::kde2d(dat_num[,1], dat_num[,2], n = 10L)
      den2d_cross_join <- merge(den2d$x, den2d$y, all = TRUE)
      dat_func <- data.frame(den2d_cross_join, as.vector(den2d$z))
    }
    if (input$func_nm == "dmvnorm"){
      col_mns  <- colMeans(dat_num)
      dat_cov <- cov(dat_num)
      dmvn <- mvtnorm::dmvnorm(dat_num, mean = col_mns, sigma = dat_cov)
      dat_func <- cbind(dat_num, dmvn)
    }
    ## Rescale is crutial for a hull to work disp aspect ratio.
    dat_func <- tourr::rescale(dat_func) %>% 
      as.data.frame()
    colnames(dat_func) <- c(paste0("V", 1:2), "func")
    dat_func
  })
  
  ## 3) dat_star(); obs in backdimensions -----
  dat_star <- reactive({
    dat_bd   <- dat_bd()
    ncol_bd  <- ncol(dat_bd)
    dat_func <- dat_func()
    
    ## Agggregate the function values within bd slices
    #req(input$bslice_agg)
    agg_func <- max ## case when doesn't want to return functions; closure not subsettable.
    # if(       input$bslice_agg == "max")    {max
    # } else if(input$bslice_agg == "mean")   {mean
    # } else if(input$bslice_agg == "median") {median
    # } else if(input$bslice_agg == "min")    {min}
    if (ncol_bd != 0) {
      req(input$bd_slice_1)
      
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
      dat_star <- dat_func[IS_in_all_bd_slices, ]
    }
    dat_star <- dplyr::group_by(dat_star, V1, V2) %>%
      dplyr::summarise(.groups = "drop",
                       z_agg = agg_func(func),
                       z_min = min(func),
                       z_max = max(func)) %>%
      as.data.frame()
    dat_star <- dat_star[complete.cases(dat_star), ] ## Sometimes adds a NULL end line?
    
    ## Return df of aggregated dat_star; rows in all bd slices, columns: V1:V2, z_agg, z_min, z_max
    dat_star
  })
  
  ## Character vector of the hex colors to use based on first factor or character column
  pt_color <- reactive({
    dat_star <- dat_star()
    dat_star <- dat_star[complete.cases(dat_star), ] ## Row-wise complete
    IS_fct_col <- sapply(dat_star, function(col){
      is.factor(col) | is.character(col)
    })
    
    ## If not fct|char columns give dummy color
    if (sum(IS_fct_col) == 0L) return(rep(col_of("NO_FCT|CHAR"), nrow(dat_star))) 
    fst_fct_col_num <- Position(function(x) x == T, IS_fct_col) ## Number of first column that is fct|char
    class <- dat_star[, fst_fct_col_num]
    col_of(class)
  })
  
  
  ## 4) full_ashape(); superset of triangles -----
  ## Creates a list of 3D Delaunay triangulation matrices, as a function of alpha(s).
  #### Contains list elements for each of the shapes: (tetra, triang, edge, vertex, x),
  #### Delaunay triangulations maximizes the smallest angle of the triangles to avoid sliver triangles.
  full_ashape <- reactive({
    dat_star_3mat <- as.matrix(dat_func())
    ## Possible alpha values for the a_hull_alpha()
    a_hull_alpha_seq <- seq(1L, 20L, by = 1L) ## fixed to 1 atm
    ## ashape3d obj of all alphas and all shapes (tetra, triang, edge, vertex, x)
    alphashape3d::ashape3d( ## Expects numeric matrix in 3 dimensions.
      x = dat_star_3mat, alpha = a_hull_alpha_seq, pert = TRUE)
  })
  
  ## 5) Render scene -----
  scene_functionVis <- reactive({
    req(dat_star())
    ##### Init a_hull triangles via alphashape3d::ashape3d()
    ## Grab only the triangles from the 3D Delaunay triangulation
    ashape_triang  <- suppressWarnings(full_ashape()$triang) ## Suppresses: "Warning: Duplicate points were removed."
    ## Column name specifying the current alpha value:
    alpha_col_nm    <- paste0("fc:", a_hull_alpha())
    alpha_col_num   <- which(colnames(ashape_triang) == alpha_col_nm)
    ## rows of the exterior triangles
    rows_ext_triang <- ashape_triang[, alpha_col_num] == 2L
    #### Values can be: 0 (not on a_hull) 1 (interitor triang), 2 (regular), 3 (singular)
    ## Triangles to display; only those on the exterior of the alpha hull.
    xyz_rows_ext_triang <- t(ashape_triang[rows_ext_triang, 1L:3L])
    ## df of the exterior triangles of the bd slices, columns: V1, V2, z_agg, z_min, z_max
    
    ## Aesthetic init
    dat_star   <- dat_star()
    func_nm    <- input$func_nm
    agg_nm     <- "max" #input$bslice_agg 
    bd_col_nms <- colnames(dat_bd())
    fd_col_nms <- c(colnames(dat_fd()), paste0(func_nm, "_", agg_nm))
    labs <- paste0(c("x, ", "y, ", "z, "), fd_col_nms)
    if(all.equal(dat_star$z_min, dat_star$z_max) == FALSE)
      labs[3] <- paste0("z, ", fd_col_nms(3L))
    pt_col <- pt_color()
    disp_df <- dat_func() ## Full disp cols for setting aspect ratio
    x_asp <- 1L / diff(range(disp_df[, 1L]))
    y_asp <- 1L / diff(range(disp_df[, 2L]))
    z_asp <- 1L / diff(range(disp_df[, 3L]))
    
    ##### rgl scene graphics 
    try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
    open3d(FOV = 0L, zoom = 1L)
    title3d(xlab = labs[1L], ylab = labs[2L], zlab = labs[3L])
    aspect3d(x_asp, y_asp, z_asp)
    bbox3d(xlen = num_bbox_ticks, ylen = num_bbox_ticks, zlen = num_bbox_ticks,
           color = col_bbox , alpha = alpha_bbox, emission = col_bg, lwd = 1L)
    
    ### Data point Spheres
    spheres3d(x = dat_star[, 1], y = dat_star[, 2], z = dat_star[, 3],
              radius = rad_pts, col = pt_col)
    ### Add red aggregation lines if needed
    if(all(dat_star$z_min == dat_star$z_max) == FALSE) {
      segments3d(color = "red", alpha = alpha_red_bslice_agg,
                 rep(dat_star[, 1], each = 2L), 
                 rep(dat_star[, 2], each = 2L), 
                 c(rbind(dat_star$z_min, dat_star$z_max)))
    }
    ### a_hull triangs
    if (input$DO_DISP_a_hull_triang) {
      triangles3d(dat_star[xyz_rows_ext_triang, "V1"],
                  dat_star[xyz_rows_ext_triang, "V2"],
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
    dat <- dat_raw()
    p   <- ncol(dat)
    dat_bd <- dat_bd()
    bd_col_nms <- colnames(dat_bd)
    ## Half of the targeted relative variable volume
    half_def_rel_size <- .5 * input$tgt_rel_h^(1 / (p - d))
    
    ## Make slider numeric inputs for the back dimension slice
    ncol_bd <- ncol(dat_bd)
    if(ncol_bd == 0L) return("There are no back dimensions.")
    i_s <- 1:ncol_bd
    bd_slice_midpts <- lapply(i_s, function(i) {
      med <- median(dat_bd[, i])
      sliderInput(inputId = paste0("bd_slice_", i), 
                  label = paste0(bd_col_nms[i], " slice midpoint"),
                  min = 0L, max = 1L, step = .05,
                  value = c(med - half_def_rel_size, ## Creates a width slider when you give 2 values.
                            med + half_def_rel_size))
    })
    
    ## Layout the midpoint and width sliders in a row
    lapply(i_s, function(i) { column(7, bd_slice_midpts[[i]]) })
  }) ## Assign output$back_dimensions_ui, closing RenderUI()
  
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
      geom_rug(aes(y = 0L), size = 1L, position = "jitter") +
      geom_density(alpha = .3, fill = "red") +
      labs(x = var_nm) +
      facet_wrap(~ var_nm, ncol = 1L) +
      theme_void() +
      coord_cartesian(xlim = c(x_min, x_max), 
                      ylim = c(y_min, y_max))
    
    gg_blank <- ggplot() + theme_void()
    
    ## Display in order with white space at the top to allign with sliders.
    bd_histograms <- cowplot::plot_grid(
      gg_blank, bd_hists, ncol = 1L
    )
    
    bd_histograms
  })
  output$bd_histograms <- renderPlot(bd_histograms())
  
}) ## Assign server function to be used in shinyApp()

shinyApp(ui = ui, server = server)
