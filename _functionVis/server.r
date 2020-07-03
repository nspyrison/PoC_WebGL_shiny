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
  alpha_bbox     <- .6  ## alpha opacity for boundingbox and axes lines
  alpha_a_hull   <- .2  ## alpha opacity for triangles of the exterior alpha hull
  col_bg         <- "lightgrey" # "grey80" ## color for the back ground & "emission" arg
  col_bbox       <- "black"     # "grey20" ## color for the bounding box (widget cell)
  pal_surfaces   <- c("blue", "red", "cyan", "purple") # RColorBrewer::brewer.pal(n = 4, "Paired") 
  num_bbox_ticks <- 4
  # alpha_surfaces <- .6 ## alpha opacity for surfaces and meshes
  # shine        <- 128  ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
  
  ## Data, dim, basis, ases init
  dat       <- tourr::rescale(tourr::flea[, 1:6])
  n         <- nrow(dat)
  p         <- ncol(dat)
  d         <- 3
  rb        <- tourr::basis_random(p, d)
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

app_hist_grid <- function(df_bd, var_nm, lb, ub){
  ##TODO: Might need to pivot_longer the df, and need lb, ub; functions of input
  df_bd<-mtcars
  
  var_nms <- colnames(df_bd)
  .call <- NULL
  for(i in 1:ncol(df_bd)){
    x <- df_bd[, i]
    var_nm <- var_nms[i]
    .med <- median(x)
    .bins <- nclass.FD(x) ## bins on Freedman-Diaconis, func(n, IQR).
    .den <-  density(x)
    .y_q1 <- 3 * summary(.den$y)[2] ## First quartile of the density of the variable
    .y_range <- diff(range(.den$y))
    .x_range <- diff(range(x))
    
    x <- as.data.frame(x)
    colnames(x) <- var_nm
    gg <- 
      ggplot(x, aes_string(x = var_nm)) +
      geom_histogram(aes(y = ..density..), bins = .bins, colour = "black", fill = "grey") +
      #geom_density(alpha = .3, fill = "red") +
      geom_vline(aes(xintercept = .med),
                 color = "blue", linetype = "dashed", size = 1) + 
      geom_rect(aes(ymin = -.y_q1, ymax = .y_q1, xmin = lb, xmax = ub),
                fill = "blue", alpha = .01) +
      theme_void() + 
      theme(axis.title.x = element_text()) + 
      labs(x = var_nm) +
      coord_fixed(ratio =  .2 * .x_range / .y_range) ## Ratio of y/x
    
    assign(paste0("gg", i), gg)
    
    .call_char <- paste0(.call, "gg", i)
    if (i != ncol(df_bd)) .call_char <- paste0(.call, ", ")
  }
  
  grid.arrange(eval(parse(.call_char)), ncol = 1)
}

app_simulate_clusters <- function() {
  p <- 3
  k_cl <- 2
  n_by_cl  <- c(100, 100)
  mn_by_cl <- list(c(0, 0, 0), c(4, 0, 0))
  sd_by_cl <- list(c(1, 1, 1), c(1, 1, 1))
  { ## Make cov_by_cl given the sd of xy_sd_by_cl, sd of z=0
    .cov1 <- diag(p) * sd_by_cl[[1]]^2
    .cov2 <- diag(p) * sd_by_cl[[2]]^2
    lt_idx <- lower.tri(.cov2) 
    # matrix(1:9, nrow=3)[lt_idx]  ## Review positions of off diag elements
    .cov2[lt_idx] <- .cov2[t(lt_idx)] <- c(-.9, 0,0) ## set off diag elements to .7
    .cov1 <- lqmm::make.positive.definite(.cov1)
    .cov2 <- lqmm::make.positive.definite(.cov2)
    
  }
  cov_by_cl <- list(.cov1, .cov2)
  
  ## Create each cluster
  df <- NULL
  for (i in 1:k_cl){
    .cluster <- letters[i]
    .clust_mat <- mvtnorm::rmvnorm(n = n_by_cl[i], 
                                   mean = mn_by_cl[[i]], 
                                   sigma = cov_by_cl[[i]]
    )
    .df <- data.frame(x1 = .clust_mat[, 1], 
                      x2 = .clust_mat[, 2],
                      x3 = .clust_mat[, 3],
                      cluster = .cluster)
    df <- rbind(df, .df)
  }
  df$cluster <- as.factor(df$cluster)
  df
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
  a_hull_alpha <- reactive({
    1 / input$a_hull_radius
  })
  output$a_hull_alpha <- 
    renderText(paste0("Applied alpha: ", round(a_hull_alpha(), 2)))
  
  dat_raw <- reactive({
    if (input$dat == "grid cube") {
      load(file = "./data/df_func_surface2.rda") ## Brings df into global environment.
      return(df[, 1:3])
    }
    if (input$dat == "simulation") return(app_simulate_clusters())
    if (input$dat == "flea") return(tourr::flea)
    if (input$dat == "wine") return(spinifex::wine)
  })
  
  ## character vector of the hex colors to use based on first factor or character column.
  pt_color <- reactive({
    .dat_raw <- dat_raw()
    .IS_fct_col <- sapply(.dat_raw, function(col){
      is.factor(col) | is.character(col)
    })
    
    ## if not fct|char columns give dummy color
    if (sum(.IS_fct_col) == 0) return(rep(col_of("NO_FCT|CHAR"), nrow(.dat_raw))) 
    .fst_fct_col_num <- Position(function(x) x == T, .IS_fct_col) ## number of first column that is fct|char
    .fst_class <- .dat_raw[, .fst_fct_col_num]
    col_of(.fst_class)
  })
  
  dat_dmvn <- reactive({
    .dat_raw <- dat_raw()
    .IS_numeric_col <- sapply(.dat_raw, is.numeric)
    .dat_num <- .dat_raw[, .IS_numeric_col]
    
    .dat_mn  <- sapply(.dat_num, mean)
    .dat_cov <- cov(.dat_num)
    dmvn <- mvtnorm::dmvnorm(.dat_num, mean = .dat_mn, sigma = .dat_cov) ## don't add . infront; becomes a colname
    .dat_dmvn <- cbind(.dat_num, dmvn)
    ret <- tourr::rescale(.dat_dmvn) ## Rescale is crutial for a hull to work correctly and disp aes.
    as.data.frame(ret)
  })
  
  dat_bd <- reactive ({
    .dat <- dat_dmvn()
    .p <- ncol(.dat)
    .x_num <- 1   ## Could change to input or hold one out style
    .y_num <- 2   ## Could change to input or hold one out style
    .z_num <- .p  ## Could be any function, but should be appended to end.
    .bd_col_nums <- (1:.p)[-c(.x_num, .y_num, .z_num)]
    .bd_col_nms  <- colnames(.dat)[.bd_col_nums]
    as.data.frame(.dat)[.bd_col_nms]
  })
  
  dat_star <- reactive({
    req(input$bd_slice_1)
    
    ## Data frame of full sample the back dimension data
    .dat_bd <- dat_bd()
    ## Sub set to only the rows within ALL back dimension slices.
    i_s <- 1:ncol(.dat_bd)
    IS_in_bd_slice_mat  <- NULL ## Logical matrix of rows in each back dimenion slice.
    IS_in_all_bc_slices <- T    ## Logical vector of rows in ALL back dimenion slices.
    for(i in i_s){
      .dim         <- .dat_bd[, i]
      .bd_slice    <- input[[paste0("bd_slice_", i)]]
      .lb          <- min(.bd_slice)
      .ub          <- max(.bd_slice)
      .IS_in_slice <- .dim >= .lb & .dim <= .ub ## Logical vector of rows within this .dim's slice.
      IS_in_bd_slice_mat  <- cbind(IS_in_bd_slice_mat, .IS_in_slice)
      IS_in_all_bc_slices <- IS_in_all_bc_slices & .IS_in_slice
    }
    if (sum(IS_in_all_bc_slices) == 0) stop("Currect slices of the back dimensions contain no observations.")
    
    .dat         <- as.data.frame(dat_dmvn())
    .dat$rownum  <- 1:nrow(.dat)
    .IS_disp_col <- !(colnames(.dat) %in% colnames(.dat_bd))
    ## A df subset (with all bd slice), of the front (display) dimensions
    dat_star     <- .dat[IS_in_all_bc_slices, .IS_disp_col]
    
    ## Agggregate the function values within bd slices
    .df <- dat_star
    colnames(.df) <- c("x1", "x2", "func", "rownum")
    .grp_tib <- dplyr::group_by(.df, x1, x2)
    .agg_tib <- dplyr::summarise(
      .data = .grp_tib, .groups = "drop", 
      func_mean = mean(func), func_min = min(func), func_max = max(func), 
      rownum = first(rownum))
    
    ## Return df of aggregated dat_star ; rows in all bd slices, columns: x1:x2, y_mn, y_min, y_man
    as.data.frame(.agg_tib)
  })
  
  ## Creates a list of 3D Delaunay triangulation matrices, as a function of alpha(s).
  #### Contains list elements for each of the shapes: (tetra, triang, edge, vertex, x),
  #### Delaunay triangulations maximizes the smallest angle of the triangles to avoid sliver triangles.
  full_ashape <- reactive({
    .dat_star_3mat  <- as.matrix(dat_star()[c("x1", "x2", "func_mean")])
    ## Possible alpha values for the input$a_hull_radius
    .a_hull_alpha_seq <- 1 / seq(.5, 10, by = .5) 
    ## ashape3d obj of all alphas and all shapes (tetra, triang, edge, vertex, x)
    alphashape3d::ashape3d( ## Expects numeric matrix in 3 dimensions.
      x = .dat_star_3mat, alpha = .a_hull_alpha_seq, pert = TRUE)
  })
  
  scene_functionVis <- reactive({
    req(dat_star())
    ##### Init a_hull triangles via alphashape3d::ashape3d()
    ## Grab only the triangles from the 3D Delaunay triangulation
    .ashape_triang   <- suppressWarnings(full_ashape()$triang) ## Suppresses: "Warning: Duplicate points were removed."
    ## Column name specifying the current alpha value:
    .alpha_col_nm    <- paste0("fc:", a_hull_alpha())
    .alpha_col_num   <- which(colnames(.ashape_triang) == .alpha_col_nm)
    ## rows of the exterior triangles
    .rows_ext_triang <- .ashape_triang[, .alpha_col_num] == 2
    #### Values can be: 0 (not on a_hull) 1 (interitor triang), 2 (regular), 3 (singular)
    ## Triangles to display; only those on the exterior of the alpha hull.
    .xyz_rows_ext_triang <- t(.ashape_triang[.rows_ext_triang, 1:3])
    ## df of X[r, 6], rows within all bd slices, columns: x1, x2, rownum, func_mean, func_min, func_max
    .dat_star <- dat_star()
    
    ## Aesthetic init
    .dat_dmvn       <- dat_dmvn()
    .dat_col_nms    <- colnames(.dat_dmvn) 
    .dat_bd_col_nms <- colnames(dat_bd())
    .disp_col_nms   <- .dat_col_nms[!(.dat_col_nms %in% .dat_bd_col_nms)]
    .labs <- paste0(c("x, ", "y, ", "z, "), .disp_col_nms)
    if(all.equal(.dat_star$func_min, .dat_star$func_max) == FALSE)
      .labs[3] <- paste0("z, mean of ", .disp_col_nms(3))
    .pt_col <- pt_color()[.dat_star$rownum]
    .full_disp_cols <- as.data.frame(dat_dmvn())[.disp_col_nms] ## get full disp cols for setting aspect ratio
    .x_asp <- 1 / diff(range(.dat_dmvn[.disp_col_nms[1]]))
    .y_asp <- 1 / diff(range(.dat_dmvn[.disp_col_nms[2]]))
    .z_asp <- 1 / diff(range(.dat_dmvn[.disp_col_nms[3]]))
    
    ##### rgl scene graphics 
    try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
    open3d(FOV = 0, zoom = 1)
    title3d(xlab = .labs[1], ylab = .labs[2], zlab = .labs[3])
    aspect3d(.x_asp, .y_asp, .z_asp)
    bbox3d(xlen = num_bbox_ticks, ylen = num_bbox_ticks, zlen = num_bbox_ticks,
           color = col_bbox , alpha = alpha_bbox, emission = col_bg, lwd = 1)

    
    ### Data point Spheres
    spheres3d(x = .dat_star$x1, y = .dat_star$x2, z = .dat_star$func_mean,
              radius = rad_pts, col = .pt_col)
    ### Add red aggregation lines if needed
    if(sum(.dat_star$func_min == .dat_star$func_max) != nrow(.dat_star)) {
      segments3d(color = "red", alpha = alpha_a_hull,
                 rep(.dat_star$x1, each = 2), 
                 rep(.dat_star$x2, each = 2), 
                 c(rbind(.dat_star$func_min, .dat_star$func_max)))
    }
    ### a_hull triangs
    if (input$DO_DISP_a_hull_triang) {
      triangles3d(.dat_star[.xyz_rows_ext_triang, "x1"],
                  .dat_star[.xyz_rows_ext_triang, "x2"],
                  .dat_star[.xyz_rows_ext_triang, "func_mean"],
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
    .dat <- dat_dmvn()
    .p   <- ncol(.dat)
    .dat_bd <- dat_bd()
    .bd_col_nms <- colnames(.dat_bd)
    ## Half of the targeted relative variable volume
    .half_def_rel_size <- .5 * input$tgt_rel_h^(1 / (.p - d))
    
    ## Make slider numeric inputs for the back dimension slice
    ncol_bd <- ncol(.dat_bd)
    if(ncol_bd == 0) stop("dat_bd() not returning columns.")
    i_s <- 1:ncol_bd
    bd_slice_midpts <- lapply(i_s, function(i) {
      .med <- median(.dat_bd[, i])
      sliderInput(inputId = paste0("bd_slice_", i), 
                  label = paste(.bd_col_nms[i], "slice midpoint"),
                  min = 0, max = 1, step = .05,
                  value = c(.med - .half_def_rel_size, ## Creates a width slider when you give 2 values.
                            .med + .half_def_rel_size))
    })
    
    ## Layout the midpoint and width sliders in a row
    lapply(i_s, function(i) { column(7, bd_slice_midpts[[i]]) })
  }) ## Assign output$back_dimensions_ui, closing RenderUI()
  
  bd_histograms <- reactive({
    # .dat <- dat_dmvn()
    # .p   <- ncol(.dat)
    .dat_bd <- dat_bd()
    # .bd_col_nms <- colnames(.dat_bd)
    .def_rel_size <- round(input$tgt_rel_h^(1 / (.p - d)), 2)
    # 
    ## Make slider numeric inputs for the back dimension slice
    i_s <- 1:ncol(.dat_bd)
    ## Make histograms of the back dimensions, highlighting their slices
    bd_histograms <- lapply(i_s, function(i) {
      .dim       <- .dat_bd[, i]
      .dim_nm    <- .bd_col_nms[i]
      .range     <- abs(max(.dim) - min(.dim))
      .midpt     <- median(.dim)
      # input[[paste0("bd_slice_midpt_", i)]]
      .rel_size  <- .def_rel_size
      # input[[paste0("bd_slice_rel_size_", i)]] * .range
      .lb        <- .midpt - .rel_size / 2
      .ub        <- .midpt + .rel_size / 2
      #output[[paste0("bd_histogram_", i)]] <-
      renderPlot({
        app_hist(.dim, .dim_nm, .lb, .ub)
      }, height = 50) ## integer of pixels
      # output[[paste0("bd_histogram_", i)]]
    })
  })
  
}) ## Assign server function to be used in shinyApp()

shinyApp(ui = ui, server = server)
