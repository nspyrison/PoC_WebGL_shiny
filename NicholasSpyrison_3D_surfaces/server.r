options(rgl.useNULL=TRUE) # Must be executed BEFORE rgl is loaded on headless devices.

library(shiny)
library(rgl)
library(shinyRGL)
library(MASS)
library(spinifex) ## for flea, and util func
set.seed(20200527)


source('ui.R', local = TRUE)

## Global init
dat <- { ## numeric data, rescaled flea
  f <- tourr::flea[,1:6]
  tourr::rescale(f)
}
n   <- nrow(dat)
col <- spinifex::col_of(tourr::flea$species)
pch <- spinifex::pch_of(tourr::flea$species)

pca_bas <- prcomp(x = dat)$rotation[, 1:2] ## 2d basis of PCA on mat_dat()
#pca_m_sp <- create_manip_space(basis = pca_bas[,1:2], manip_var = 4) ## 3d manip space on PCA bas
proj    <- dat %*% pca_bas
#colnames(proj) <- c("x", "y")


#' PoC of different graphics packaes. Recreating from the other example folders in this repo.
#' @author Nicholas Spyrison \email{spyrison@gmail.com}
server <- shinyServer(function(input, output) {
  # num_dat <- reactive({ ## numeric data, rescaled flea
  #   f <- tourr::flea[,1:6]
  #   tourr::rescale(f)
  # })
  # col <- reactive(spinifex::col_of(tourr::flea$species))
  # pch <- reactive(spinifex::pch_of(tourr::flea$species))
  
  ### shinyRGL: =====
  # Expression that generates a rgl scene with a number of points corresponding
  # to the value currently set in the slider.
  
  output$shinyRGL <- renderWebGL({
    
    # parameters:
    ngrid  <- 40 #<- input$ngrid
    zscale <- .4 #<- input$zscale
    
    # estimate 2d density surface via kernel smoothing
    proj_den2d <- kde2d(proj[,1], proj[,2], n = ngrid)
    xgrid <- proj_den2d$x
    ygrid <- proj_den2d$y
    
    ### visualize:
    # setup env:
    light3d()
    # Draws the simulated data as spheres on the baseline
    spheres3d(proj[, 1], proj[, 2], rep(0, n), radius = 0.03, color = col)
    # Draws non-parametric 2d density surface via kernel smoothing
    surface3d(xgrid, ygrid, proj_den2d$z * zscale, color = "#FF2222",
              alpha = 0.3, front = "lines", back = "fill", smooth = T, shininess= 200)
    
  })
})

shinyApp(ui = ui, server = server)
