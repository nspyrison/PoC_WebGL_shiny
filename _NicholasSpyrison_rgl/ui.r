####### _NicholasSpyrison_rlg/ui.r ----
#' shiny UI for  server.r
require("shiny")
require("MASS")
require("tourr")
require("spinifex") ## v0.2.9000 and up
require("rgl")
require("RColorBrewer")
require("htmlwidgets")
require("jsonlite")
require("geometry")
library("alphashape3d")
set.seed(20200527)
options(rgl.useNULL = TRUE) ## Must be executed BEFORE rgl is loaded on headless devices.

w <- h <- "600px" ## height and width of the rgl widget in pixels, 

##### Start of shiny ui ----
### Following: 
## https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo",   package = "rgl"), launch.browser = TRUE, display.mode = "showcase")
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")


##### Global initialize -----
## Above server scope and not reactive
## Parameters:
{
  ## Some aesthetic parmaters for board use
  .ptRad <- .02 ## size (radius? diameter?) of data points
  .shine <- 128 ## "Shininess" of some surfaces, in [0, 128] low values (<50) are too reflective
  .a_bbox     <- .6 ## alpha for boundingbox and axes lines
  .a_surface  <- .6 ## alpha for surfaces and meshes
  .a_a.hull   <- .2 ## alpha for many triangles of the alpha hull
  .bg    <- "lightgrey" ##"grey100" ## lighter grey Back Ground
  .bb    <- "darkgrey"  ##"grey40"  ## darker grey  Bounding Box
  .pal   <- RColorBrewer::brewer.pal(3, "Paired")
  
  ## Data, dim, basis, aes 
  dat   <- tourr::rescale(tourr::flea[, 1:6])
  n     <- nrow(dat)
  p     <- ncol(dat)
  d     <- 3
  rb    <- tourr::basis_random(p, d)
  ptCol <- spinifex::col_of(tourr::flea$species)
  ptPch <- spinifex::pch_of(tourr::flea$species)
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
  
  ## Pan tour to be in the "first quadrant"
  x_min <- min(rb2holes_proj[, 1, ])
  y_min <- min(rb2holes_proj[, 2, ])
  z_min <- min(rb2holes_proj[, 3, ])
  rb2holes_proj[, 1, ] <- rb2holes_proj[, 1, ] - x_min
  rb2holes_proj[, 2, ] <- rb2holes_proj[, 2, ] - y_min
  rb2holes_proj[, 3, ] <- rb2holes_proj[, 3, ] - z_min
  holes_proj <- rb2holes_proj[,, n_tpath_bases] ## Save off final holes() basis
  
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


##### rb2holes  -----
## tourr guided tour from rb to holes
rb2holes_panel <- tabPanel("rb2holes", fluidPage(
  mainPanel(
    h2("Guided tour from a random basis to holes() @: step_size = .6, d = 3"),
    uiOutput("rb2holes_basis_slider"),
    textOutput("slider_t"),
    rglwidgetOutput("widget_rb2holes", width = w, height = h)
  )
))

##### rgl -----
rgl_panel <- tabPanel("rgl", fluidPage(
  mainPanel(
    h2("Loosely, the smallest 3D ellipsoid containing 68% of the observations from the estimated distribution."),
    rglwidgetOutput("rgl")
  )
))


##### pca_kde3d -----
## Kernal estimation on covar matrix (1 SD, 68% obs within volume)
pca_kde3d_panel <- tabPanel("pca_kde3d", fluidPage(
  mainPanel(
    h2("3D kernel density estimation on covar matrix"),
    p("Loosely, the smallest 3D ellipsoid containing 68% of the observations from the estimated distribution."),
    rglwidgetOutput("widget_pca_kde3d")
  )
))

##### pca_kde2d  -----
## Via MASS::kde2d()
pca_kde2d_panel <- tabPanel("pca_kde2d", fluidPage(
  mainPanel(
    h2("2D kernel density estimation. via MASS::kde2d()."),
    rglwidgetOutput("widget_holes_kde2d")
    
  )
))

##### logLik -----
logLik_panel <- tabPanel("logLik", fluidPage(
  mainPanel(
    h2("log likelihood parameter estimation of a sampled gamma(5, .1) distribution."),
    p("left:  whole surface over the range of parameters"),
    p("right: only region of the surface with log likelihood values near the maximum; z <- fit$loglik + c(-qchisq(0.99, 2)/2, 0) "),
    rglwidgetOutput("widget_logLik")
  )
))

##### functionSurfaces -----
functionSurfaces_panel <- tabPanel("functionSurfaces", fluidPage(
  mainPanel(
    h2("function surfaces, sampled from `geozoo` and y from `mvtnorm`::dmvnorm()"),
    p("x1:3: grid values of 3D cube between [-3,3]"),
    p("y1:2: 'density function for the multivariate normal distribution given 3D mean and covar matrix"),
    p("top left (blue): orthogonal view; x1, x2, y1 = dmvnorm(0, cov(x))"),
    p("top right (red): orthogonal view; x1, x2, y2 = dmvnorm(sim. var-cov mat)"),
    p("bottom left (cyan): orthogonal view; x1, x2, 'y1.5' = .5*y1 + .5*y2"),
    p("bottom right (purple): orthogonal view; x1, x2, y3 = .5 * (max(y1) - y1) + .5 *y2"),
    rglwidgetOutput("widget_functionSurfaces"),
    h4("Thoughts on function vis:"),
    p("- We can only visualize shapes/surfaces in `d = {2,3}`."),
    p("- We could treat function values as another dimension, though this seems uninteresting, might need freezing."),
    p("- Projections of a function values/grid-points understandable, but don't understand if it's possible to project continuouse function surfaces, without quickly going to a non-euclidian projection space."),
    p("- I don't think we have to lock in values for `p-d`-dimensions, because each variable has a contribution to projections space."),
    h4("Re: Kavan & fermi surfaces; I am going to tour on densely highlighted points of equal radius on a complex (fermi) surface "),
    p("- 3D example: given an oragami crane; draw lines on the surface that are exactly `r` radius away from the center. Sample points from the surface, highlighting points on the lines against a backdrop of grey points on the remaining surface. Tour in the original, but highlighted `p`-space."),
    p("-- Simpler 2,3D example: 2,3D heart"),
    p("-- Note that only a small region of `r` (if any) will be a single closed loop."),
    p("--> Map each closed loop of the same radius as a different shape in the same color?"),
    p("- 4D example: on a 4D cube, draw lines on surface that are exactly `r` radius away from the center (enscribe 4D-sphere of radius `r`). Sample from the points surface, highlighting points on the lines against a backdrop of grey points on the remaining surface. Tour in the original, but highlighted `p`-space."),
    p("- Potentially interesting: as a function of radius, plot some measure of the range of the phase-space/volume able to be reached."),
    p("- Is this related to function visualization? _ie_ The same as defining the a function value (exact radius) and highlighting only where it intersects the surface/sample?"),
    hr(),
    h2("Static function surfaces, z = f(x,y)"),
    p("left:  z = ((x^2) + (3 * y^2)) * exp(-(x^2) - (y^2))"),
    p("right: z = (x^2) + (y^3); inflection point not at a global extrema."),
    rglwidgetOutput("widget_functionSurfaces_STATIC")
  )
))


##### widget_rotation  -----
rgl_widget_rotation <- function(inputId, value="", nrows, ncols) {
  ## This code includes the javascript to return the rotation matrix of the widget.
  tagList(
    singleton(tags$head(tags$script(src = "rglwidgetaux.js"))),
    tags$div(id = inputId,class = "rglWidgetAux",as.character(value))
  )
}

widget_rotation_panel <- tabPanel("widget rotation", fluidPage(
  rgl_widget_rotation('ctrlplot3d'),
  actionButton("regen", "Regen Scene"),
  actionButton("queryumat", "Query User Matrix"),
  rglwidgetOutput("plot3d"),
  tableOutput("usermatrix")
))

##### ui -----
## Bring the panels together for full UI
ui <- fluidPage(
  h2("Nicholas Spyrison, 2020/05/29"),
  navbarPage("WebGL 3D visualizations, `rgl` package",
             rb2holes_panel,
             functionSurfaces_panel,
             widget_rotation_panel,
             pca_kde3d_panel,
             pca_kde2d_panel,
             logLik_panel
  )
)

