####### _functionVis/server.r -----
#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison@gmail.com}

##### Setup -----
source("./ui.r", local = TRUE)
set.seed(20200629)
options(rgl.useNULL = TRUE) ## Must be executed BEFORE rgl is loaded on headless devices.


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

app_hist <- function(x, var_nm, lb, ub){
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
    theme_void() + 
    theme(axis.title.x = element_text()) + 
    labs(x = var_nm) +
    coord_fixed(ratio =  .2 * .x_range / .y_range) ## Ratio of y/x
}


app_simulate_clusters <- function() {
  p <- 3
  k_cl <- 2
  n_by_cl  <- c(50, 500)
  mn_by_cl <- list(c(0, 0, 0), c(5, 5, 0))
  sd_by_cl <- list(c(1, 1, 1), c(1.5, 1.5, 1))
  { ## Make cov_by_cl given the sd of xy_sd_by_cl, sd of z=0
    .cov1 <- diag(p) * sd_by_cl[[1]]^2
    .cov2 <- diag(p) * sd_by_cl[[2]]^2
    lt_idx <- lower.tri(.cov2) 
    # matrix(1:9, nrow=3)[lt_idx]  ## Review positions of off diag elements
    .cov2[lt_idx] <- .cov2[t(lt_idx)] <- c(.7, 0,0) ## set off diag elements to .7
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
    .df <- data.frame(x = .clust_mat[, 1], 
                      y = .clust_mat[, 2],
                      z = .clust_mat[, 3],
                      cluster = .cluster)
    df <- rbind(df, .df)
  }
  df$cluster <- as.factor(df$cluster)
  df
}

##### STALE_functionSurfaces =====
## ON ICE, see same tab in \_NicholasSpyrison_rgl\ app for recency.
{ ## !!Note: this is a global, outside of the server and reactive()
  ## Initialize
  load(file = "./data/df_func_surface2.rda") ## Brings df into global environment.
  col_nms <- colnames(df)
  y_col_nums <- which(startsWith(col_nms, "y") == TRUE) ## columns
  
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  open3d(FOV = 0, zoom = 1)
  bbox3d(xlen = num_bbox_ticks, ylen = num_bbox_ticks, zlen = num_bbox_ticks,
         color = col_bbox , alpha = alpha_bbox, emission = col_bg, lwd = 1)
  mfrow3d(2, 2, sharedMouse = TRUE)
  for (i in 1:length(y_col_nums)){
    ## Create df subsets
    df_disp     <- df[, c(1:2, y_col_nums[i])]
    ## Create alpha hulls
    ashape_alpha <- .8 #Example explicitly uses 0; may impact fc_nms; columns at the end
    fc_nm        <- paste0("fc:", ashape_alpha)
    
    ashape_triang  <- ashape3d(as.matrix(df_disp), alpha = ashape_alpha)$triang
    rows_on_a_hull <- ashape_triang[, fc_nm] == 2 ## only rows on the a_hull
    ## values can be: 0 (not on a_hull) 1 (interitor triang), 2 (regular), 3 (singular)
    a_hull_triang  <- t(ashape_triang[rows_on_a_hull, 1:3])
    
    ## Render
    aspect3d(1 / diff(range(df_disp[, 1])),
             1 / diff(range(df_disp[, 2])),
             1 / diff(range(df_disp[, 3])))
    spheres3d(x = df_disp[, 1], y = df_disp[, 2], z = df_disp[, 3],
              radius = rad_pts, col = pal_surfaces[i])
    
    triangles3d(df_disp[a_hull_triang, 1],
                df_disp[a_hull_triang, 2],
                df_disp[a_hull_triang, 3],
                col = pal_surfaces[i], alpha = alpha_a_hull)
    if (i < length(y_col_nums)) next3d() ## if not last plot then advance.
  }
  scene_functionSurfaces_STALE <- scene3d()
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
  
  dat_raw <- reactive({
    if (input$dat == "grid cube") {
      load(file = "./data/df_func_surface2.rda") ## Brings df into global environment.
      return(df[, 1:3])
    }
    if (input$dat == "simulation") return(app_simulate_clusters())
    if (input$dat == "flea") return(tourr::flea)
    if (input$dat == "wine") return(spinifex::wine)
  })
  
  pt_color <- reactive({
    .dat_raw <- dat_raw()
    .IS_fct_col <- sapply(.dat_raw, function(col){
      is.factor(col) | is.character(col)
    })
    
    if (sum(.IS_fct_col) == 0) return(col_of("NO_FCT|CHAR")) ## if not fct|char columns give dummy color
    .fst_fct_col <- Position(function(x) x == T, .IS_fct_col) ## first column that is fct|char
    .fst_class <- .dat_raw[, .fst_fct_col]
    return(col_of(.fst_class))
  })
  
  dat_dmvn <- reactive({
    .dat_raw <- dat_raw()
    .IS_numeric_col <- sapply(.dat_raw, is.numeric)
    .dat_num <- .dat_raw[, .IS_numeric_col]
    
    .dat_mn  <- sapply(.dat_num, mean)
    .dat_cov <- cov(.dat_num)
    .dmvn <- mvtnorm::dmvnorm(.dat_num, mean = .dat_mn, sigma = .dat_cov)
    .dat_dmvn <- cbind(.dat_num, .dmvn)
    tourr::rescale(.dat_dmvn) ## Rescale is crutial for a hull to work correctly and disp aes.
  })
  
  dat_bd <- reactive ({
    .dat <- dat_dmvn()
    .p <- ncol(.dat)
    .x_num <- 1   ## Could change to input
    .y_num <- 2   ## Could change to input
    .z_num <- .p  ## Could be any function, but should be appended to end
    .bd_col_nums <- (1:.p)[-c(.x_num, .y_num, .z_num)]
    .bd_col_nms  <- colnames(.dat)[.bd_col_nums]
    as.data.frame(.dat)[.bd_col_nms]
  })
  
  dat_star <- reactive({
    req(input$bd_slice_midpt_1)
    req(input$bd_slice_rel_size_1)
    
    .dat_bd <- dat_bd()
    i_s <- 1:ncol(.dat_bd)
    IS_in_bd_slice_mat  <- NULL ## Logical matrix of rows in each back dimenion slice.
    IS_in_all_bc_slices <- T    ## Logical vector of rows in ALL back dimenion slices.
    for(i in i_s){
      .dim         <- .dat_bd[, i]
      .range       <- abs(max(.dim) - min(.dim))
      .midpt       <- input[[paste0("bd_slice_midpt_", i)]]
      .thickness   <- input[[paste0("bd_slice_rel_size_", i)]] * .range
      if (is.null(.midpt)) stop(paste0("bd_slice_midpt_", i, " returning NULL in dat_star()."))
      if (is.null(.thickness)) stop(paste0("bd_slice_rel_size_", i, " returning NULL in dat_star()."))
      .lb          <- .midpt - .thickness / 2
      .ub          <- .midpt + .thickness / 2
      .IS_in_slice <- .dim >= .lb & .dim <= .ub ## Logical vector of rows within this .dim's slice.
      IS_in_bd_slice_mat  <- cbind(IS_in_bd_slice_mat, .IS_in_slice)
      IS_in_all_bc_slices <- IS_in_all_bc_slices & .IS_in_slice
    }

    if (sum(IS_in_all_bc_slices) == 0) stop("Currect slices of the back dimensions contain no observations.")
    

    .dat     <- dat_dmvn()
    .bd_col_nms <- colnames(.dat_bd)
    .disp_col_nums <- !(colnames(.dat) %in% .bd_col_nms)
    dat_star <- .dat[IS_in_all_bc_slices, .disp_col_nums]
    dat_star
  })
  
  ## Creates super set of alpha hull shapes (tetra, triang, edge, vertex, x)
  #### for all alpha values given a_hull radius in [.05, 1] by .05.
  #### each triangle alpha level gets indicator factors of: 0 (not on a_hull) 1 (interitor), 2 (regular), 3 (singular)
  full_ashape <- reactive({
    .dat_star_mat  <- as.matrix(dat_star())
    if(ncol(.dat_star_mat) != 3) stop("full_ashape expected a matrix of 3 columns.")
    ## Possible alpha values for a_hull radius in [.05, 1] by .05.
    .a_hull_alpha_seq <- 1 / seq(.05, 1, by = .05) 
    ## ashape3d obj of all alphas and all shapes (tetra, triang, edge, vertex, x)
    ashape3d(x = .dat_star_mat, 
             alpha = .a_hull_alpha_seq,
             pert = TRUE)
    
  })
  
  scene_functionVis <- reactive({
    req(dat_star())
    .dat_star <- dat_star()
    
    ##### Create a_hull trangles:
    ## Column name specifying the alpha level within a given shape of an ashape3d obj list:
    .alpha_col_nm    <- paste0("fc:", a_hull_alpha()) 
    .ashape_triang   <- full_ashape()$triang
    .rows_ext_triang <- .ashape_triang[, .alpha_col_nm] == 2 ## rows that are only exterior triangles
    #### Values can be: 0 (not on a_hull) 1 (interitor triang), 2 (regular), 3 (singular)
    ## Triangles to display; only those on the exterior of the alpha hull.
    .disp_triang <- t(.ashape_triang[.rows_ext_triang, 1:3])
    
    ## Aesthetic init
    .disp_col_nms <- colnames(dat_star())
    .labs <- paste0(c("x, ", "y, ", "z, "), .disp_col_nms)
    .col <- pt_color()
    .full_disp_cols <- as.data.frame(dat_dmvn())[.disp_col_nms] ## get full disp cols for setting aspect ratio
    .x_asp <- 1 / diff(range(.full_disp_cols[, 1]))
    .y_asp <- 1 / diff(range(.full_disp_cols[, 2]))
    .z_asp <- 1 / diff(range(.full_disp_cols[, 3]))
    
    ## Render
    try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
    open3d(FOV = 0, zoom = 1)
    axes3d(.labs, nticks = num_bbox_ticks)
    title3d(xlab = .labs[1], ylab = .labs[2], zlab = .labs[3])
    aspect3d(.x_asp, .y_asp, .z_asp)
    bbox3d(xlen = 0, ylen = 0, zlen = 0,
           color = col_bbox , alpha = alpha_bbox, emission = col_bg, lwd = 1)
  
    ### Spheres
    spheres3d(x = .dat_star[, 1], y = .dat_star[, 2], z = .dat_star[, 3],
              radius = rad_pts, col = .col)
    
    ### a_hull triangs
    if (input$DO_DISP_a_hull_triang) {
      triangles3d(.dat_star[.disp_triang, 1],
                  .dat_star[.disp_triang, 2],
                  .dat_star[.disp_triang, 3],
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
    ## Make relative thickness numeric inputs for the back dimensions
    i_s <- 1:ncol(.dat_bd)
    .def_rel_size <- round(input$tgt_rel_h^(1 / (.p - d)), 2)
    bd_slice_rel_sizes <- lapply(i_s, function(i) {
      numericInput(inputId = paste0("bd_slice_rel_size_", i), 
                   label = "rel slice size [h/r_max]",
                   min = 0, max = 1, value = .def_rel_size, step = .05)
    })
    if (sum(i_s %in% 0) >=1) browser()
    ## Make midpoint sliders inputs for the back dimensions
    bd_slice_midpts <- lapply(i_s, function(i) {
      .dim <- .dat_bd[, i]
      .min <- min(.dim)
      .max <- max(.dim)
      .median <- median(.dim)
      .step <- round((.max - .min) / 10, 1)
      sliderInput(inputId = paste0("bd_slice_midpt_", i), 
                  label = paste(.bd_col_nms[i], "slice midpoint"),
                  min = .min, max = .max, value = .median, 
                  step = .step, round = -1)
    })
    
    lapply(i_s, function(i) {
      fluidRow(
        column(7, bd_slice_midpts[[i]]),
        column(5, bd_slice_rel_sizes[[i]])
      )
    })
    
  }) ## Assign output$back_dimensions_ui, closing RenderUI()
  
  # {
  #   ## Make histograms of the back dimensions, highlighting their slices
  #   bd_histograms <- lapply(i_s, function(i) {
  #     .dim       <- .dat_bd[, i]
  #     .dim_nm    <- .bd_col_nms[i] 
  #     .range     <- abs(max(.dim) - min(.dim))
  #     .midpt     <- median(.dim)
  #     # input[[paste0("bd_slice_midpt_", i)]]
  #     .rel_size  <- .def_rel_size
  #     # input[[paste0("bd_slice_rel_size_", i)]] * .range
  #     .lb        <- .midpt - .rel_size / 2
  #     .ub        <- .midpt + .rel_size / 2
  #     #output[[paste0("bd_histogram_", i)]] <- 
  #     renderPlot({
  #       app_hist(.dim, .dim_nm, .lb, .ub)
  #     }, height = 50) ## integer of pixels
  #     # output[[paste0("bd_histogram_", i)]]
  #   })
  # }
  
}) ## Assign server function to be used in shinyApp()

shinyApp(ui = ui, server = server)
