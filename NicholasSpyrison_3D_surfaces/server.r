### Setup -----
set.seed(20200527)
library(shiny)
library(rmarkdown) ## which pkg req wants this?
library(spinifex)  ## for flea, and util func
library(tictoc)
library(purrr)
library(rgl)
library(shinyRGL)
library(MASS)
library(rayshader)
options(rgl.useNULL=TRUE) # Must be executed BEFORE rgl is loaded on headless devices.
source('ui.R', local = TRUE)

##### Global initialize -----
### Above server scope and not reactive

## parameters:
ngrid  <- 40 #<- input$ngrid  ## The number of levels to grid search on each axis
zscale <- .4 #<- input$zscale ## Coeffiencient to scale the z values of the 2d density
alpha  <- .3  ## Opacity of the surface/grid in [0-1], fully transparent and opaque respectively
shine  <- 100 ## "Shininess" of some surfaces (including shinyRGL), in [0, 128] low values (<50) are too reflective
radius <- .3  ## Radius of data points (including shinyRGL)

## data and work:
dat <- { ## numeric data, rescaled flea
  f <- tourr::flea[,1:6]
  tourr::rescale(f)
}
col <- spinifex::col_of(tourr::flea$species)
pch <- spinifex::pch_of(tourr::flea$species)
n   <- nrow(dat)

pca_bas <- prcomp(x = dat)$rotation[, 1:2] ## 2d basis of PCA on mat_dat()
#pca_m_sp <- create_manip_space(basis = pca_bas[,1:2], manip_var = 4) ## 3d manip space on PCA bas
proj    <- tibble::as_tibble(dat %*% pca_bas)
colnames(proj) <- c("x", "y")

# estimate 2d density surface via kernel smoothing
proj_de2d <- kde2d(proj$x, proj$y, n = ngrid)

#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison@gmail.com}
server <- shinyServer(function(input, output) {
  ### rgl =====
  # output$rgl <- 
  #   #renderWebGL({
  #   try(rgl.close())
  #   
  #   ### visualize:
  #   # setup env:
  #   light3d()
  #   # Draws the simulated data as spheres on the baseline
  #   spheres3d(proj$x, proj$y, rep(0, n), radius = 0.03, color = col)
  #   # Draws non-parametric 2d density surface via kernel smoothing
  #   surface3d(proj_de2d$x, proj_de2d$y, proj_de2d$z * zscale, color = "#FF2222",
  #             alpha = alpha, front = "lines", back = "fill", smooth = T, shininess = shine)
  # })
  
  ### shinyRGL =====
  # Expression that generates a rgl scene with a number of points corresponding
  # to the value currently set in the slider.
  ### USE rgl OVER shinyRGL (shiny content moved over and actively supported)
  output$shinyRGL <- renderWebGL({
    try(rgl.close())
    
    ### visualize:
    # setup env:
    light3d()
    # Draws the simulated data as spheres on the baseline
    spheres3d(proj$x, proj$y, rep(0, n), radius = 0.03, color = col)
    # Draws non-parametric 2d density surface via kernel smoothing
    surface3d(proj_de2d$x, proj_de2d$y, proj_de2d$z * zscale, color = "#FF2222",
              alpha = alpha, front = "lines", back = "fill", smooth = T, shininess = shine)
  })
  
  ### rayshader =====
  ### Too long, higher hardware requirements, causes hangs, cannot change theme on ggplots.
  output$rayshader <- renderRglwidget({
    try(rgl.close())
    
    # tic("rayshader, fastest example; montereybay")
    # montereybay %>% 
    #   sphere_shade(texture = "imhof1") %>%
    #   plot_3d(montereybay, zscale = 50, fov = 0, theta = -45, phi = 45, windowsize = c(1000, 800), zoom = 0.6,
    #           water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
    #           waterlinecolor = "white", waterlinealpha = 0.5, baseshape = "hex")
    # render_snapshot()
    # rglwidget()
    # toc()
    ambmat = ambient_shade(elmat)
    
    elmat_3d <- elmat %>%
      sphere_shade(texture = "desert") %>%
      add_water(detect_water(elmat), color="desert") %>%
      add_shadow(ray_shade(elmat,zscale=3,maxsearch = 300),0.5) %>%
      add_shadow(ambmat,0.5) %>%
      plot_3d(elmat,zscale=10,fov=0,theta=135,zoom=0.75,phi=45, windowsize = c(1000,800))
    render_snapshot()
    rglwidget()

  })
  
})

shinyApp(ui = ui, server = server)
