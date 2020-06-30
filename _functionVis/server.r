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
  num_bbox_ticks <- 3
  # alpha_surfaces <- .6 ## alpha opacity for surfaces and meshes
  # shine        <- 128  ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
  
  ## Data, dim, basis, ases init
  dat       <- tourr::rescale(tourr::flea[, 1:6])
  n         <- nrow(dat)
  p         <- ncol(dat)
  d         <- 3
  rb        <- tourr::basis_random(p, d)
  col_pts   <- spinifex::col_of(tourr::flea$species)
  pal_pts   <- unique(col_pts)
}

app_hist <- function(df, var_nm, lb, ub){
  .med <- median(df[, which(colnames(df) == var_nm)])
  ggplot(df, aes_string(x = var_nm)) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "grey") +
    geom_density(alpha = .3, fill = "red") +
    geom_vline(aes(xintercept = .med),
               color = "blue", linetype = "dashed", size = 1) + 
    geom_rect(aes(ymin = -.003, ymax = .003, xmin = lb, xmax = ub),
              fill = "blue", alpha = .006) +
    theme_void() + 
    theme(axis.title.x = element_text()) + 
    labs(x = var_nm) #+
    #coord_fixed(ratio = 10) ## Ratio of y/x
}

##### STALE_functionSurfaces =====
## ON ICE, see same tab in \_NicholasSpyrison_rgl\ app for recency.
{ ## !!Note: this is a global, outside of the server and reactive()
  ## Initialize
  load(file = "./data/df_func_surface2.rda") ## Brings df into global environment.
  col_nms <- colnames(df)
  y_col_nums <- which(startsWith(.col_nms, "y") == TRUE) ## columns
  
  try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
  open3d(FOV = 0, zoom = 1)
  bbox3d(xlen = num_bbox_ticks, ylen = num_bbox_ticks, zlen = num_bbox_ticks,
         color = col_bbox , alpha = alpha_bbox, emission = col_bg, lwd = 1)
  mfrow3d(2, 2, sharedMouse = TRUE)
  for (i in 1:length(y_col_nums)){
    ## Create df subsets
    df_disp     <- df[, c(1:2, .y_col_nums[i])]
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
  
  a_hull_alpha <- reactive({
    1 / input$a_hull_radius
  })
  
  ##### functionVis =====
  dat_dmvn <- reactive({
    dat <- tourr::flea[, 1:6]
    dat_mn  <- apply(dat, 2, mean)
    dat_cov <- cov(dat)
    dmvn <- mvtnorm::dmvnorm(dat, mean = dat_mn, sigma = dat_cov)
    dat <- cbind(dat, dmvn)
    tourr::rescale(dat) ## Rescale is crutial for a hull to work correctly and disp aes.
  })
  
  dat_bd <- reactive ({
    .dat <- dat_dmvn()
    .p <- ncol(.dat)
    .d <- 3
    .x_num <- 1
    .y_num <- 2
    .z_num <- .p
    .bd_nums <- (1:.p)[-c(.x_num, .y_num, .z_num)]
    as.data.frame(.dat[, .bd_nums])
  })
  
  dat_star <- reactive({
    req(input$bd_slider_4)
    req(input$bd_sliceSize_4)
    .dat <- dat_bd()
    
    bd_cond_mat <- NULL
    for(i in 1:ncol(.dat)){
      .dim       <- .dat[, i]
      .range     <- abs(max(.dim) - min(.dim))
      .midpt     <- input[[paste0("bd_slider_", i)]]
      .thickness <- input[[paste0("bd_sliceSize_", i)]] * .range
      .lb        <- .midpt - .thickness / 2
      .ub        <- .midpt + .thickness / 2
      .cond      <- .dim >= .lb & .dim <= .ub
      bd_cond_mat <- cbind(bd_cond_mat, .cond)
    }
    
    j_s <- 1:length(i_s)
    bd_cond_vect <- T
    for(j in j_s) {bd_cond_vect <- bd_cond_vect & bd_cond_mat[, j]}
    if (sum(bd_cond_vect) == 0) stop("Currect slices of the back dimensions contain no observations.")
    
    dat_star <- .dat[bd_cond_vect, ]
    dat_star
  })
  
  scene_functionVis <- reactive({
    .dat      <- dat_star()
    .labs     <- paste0(c("x, ", "y, ", "z, "), colnames(.dat))
    ## Create alpha hulls
    .a_hull_alpha    <- a_hull_alpha()
    .fc_nm           <- paste0("fc:", .a_hull_alpha)
    .ashape_triang   <- ashape3d(x = as.matrix(.dat), alpha = .a_hull_alpha,
                                 pert = TRUE)$triang
    .rows_ext_a_hull <- .ashape_triang[, .fc_nm] == 2 ## only exterior rows of the a_hull triangle obj
    ## values can be: 0 (not on a_hull) 1 (interitor triang), 2 (regular), 3 (singular)
    .ext_a_hull_triang <- t(.ashape_triang[.rows_ext_a_hull, 1:3])
    
    ## Render
    try(rgl.close(), silent = T) ## Shiny doesn't like rgl.clear() or purrr::
    open3d(FOV = 0, zoom = 1)
    bbox3d(xlen = num_bbox_ticks, ylen = num_bbox_ticks, zlen = num_bbox_ticks,
           color = col_bbox , alpha = alpha_bbox, emission = col_bg, lwd = 1)
    title3d(xlab = .labs[1], ylab = .labs[2], zlab = .labs[3])
    aspect3d(1 / diff(range(.dat[, 1])),
             1 / diff(range(.dat[, 2])),
             1 / diff(range(.dat[, 3])))
    
    ### Spheres
    spheres3d(x = .dat_disp[, 1], y = .dat[, 2], z = .dat[, 3],
              radius = rad_pts, col = col_pts)
    
    ### a_hull triangs
    if (input$DO_DISP_a_hull_triang) {
      triangles3d(.dat[.ext_a_hull_triang, 1],
                  .dat[.ext_a_hull_triang, 2],
                  .dat[.ext_a_hull_triang, 3],
                  col = pal_surfaces[1], alpha = alpha_a_hull)
    }
    
    scene_functionVis <- scene3d()
    scene_functionVis
  })
  output$widget_functionVis <- renderRglwidget(
    rglwidget(scene_functionVis())
  )
  
  output$backDimensionInputs <- renderUI({
    .dat <- dat_bd()
    .bd_nms <- colnames(.dat)
    
    ## Make midpoint sliders inputs
    i_s <- 1:ncol(.dat)
    bd_sliders <- lapply(i_s, function(i) {
      .min <- min(.dat[, i])
      .max <- max(.dat[, i])
      .median <- median(.dat[, i])
      .step <- round((.max - .min) / 10, 1)
      sliderInput(inputId = paste0("bd_slider_", i), 
                  label = paste(.bd_nms[i], "slice midpoint"),
                  min = .min, max = .max, value = .median, 
                  step = .step, round = -1)
    })
    
    ## Make thickness numeric inputs
    bd_thickness <- lapply(i_s, function(i) {
      .def_val <- round(input$tgt_rel_h^(1 / (p - d)), 2)
      numericInput(inputId = paste0("bd_sliceSize_", i), 
                   label = "rel slice size [h/r_max]",
                   min = 0, max = 1, value = .def_val, step = .05)
    })
    
    ## Make histograms
    bd_histograms <- lapply(i_s, function(i) {
      .dim_nm    <- .bd_nms[i]  
      .dim       <- .dat[.dim_nm]
      .range     <- abs(max(.dim) - min(.dim))
      .midpt     <- input[[paste0("bd_slider_", i)]]
      .thickness <- input[[paste0("bd_sliceSize_", i)]] * .range
      .lb        <- .midpt - .thickness / 2
      .ub        <- .midpt + .thickness / 2
      renderPlot({
        app_hist(.dim, .dim_nm, lb = .lb, ub = .ub)
      }, height = 50) ## integer of pixels
    })
    
    ## Make sliders and thickness into ordered rows.
    lapply(i_s, function(i) {
      #fluidPage(
        fluidRow(
          column(7, bd_sliders[[i]]),
          column(5, bd_thickness[[i]])
        )#,
        #bd_histograms[[i]]
      #)
    })
    
  })
  
  output$widget_functionSurfaces_STALE <- renderRglwidget(
    rglwidget(scene_functionSurfaces_STALE)
  )
  
})

shinyApp(ui = ui, server = server)
