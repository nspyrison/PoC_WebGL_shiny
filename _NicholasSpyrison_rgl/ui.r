##### _NicholasSpyrison_rlg/ui.r setup ----
#' shiny UI for  server.r
require("shiny")
require("MASS")
require("tourr")
require("rgl")
require("RColorBrewer")
require("htmlwidgets")
require("jsonlite")
require("geometry")
library("alphashape3d")
set.seed(20200527)
options(rgl.useNULL = TRUE) ## Must be executed BEFORE rgl is loaded on headless devices.

w <- h <- "600px" ## height and width of the rgl widget in pixels, 

##### Local app_* functions -----
app_col_of <- function(category, pallet_name = "Dark2") {
  .l_lvls <- length(levels(category))
  if (.l_lvls == 0) stop("Length of 'category' cannot be zero.")
  if (.l_lvls > 12) stop("'category' has more than the expected max of 12 levels.")
  pal <- suppressWarnings(RColorBrewer::brewer.pal(.l_lvls, pallet_name))
  pal[as.integer(factor(category))]
}

app_pch_of <- function(category) {
  .l_lvls <- length(levels(category))
  if (.l_lvls == 0) stop("Length of 'category' cannot be zero.")
  if (.l_lvls > 12) stop("'category' has more than the expected max of 12 levels.")
  y_ord <- c(21:25, 3:4, 7:11)
  int_lvls <- as.integer(factor(category))
  y_ord[int_lvls]
}

##### Start of shiny ui ----
### Following: 
## https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo",  package = "rgl"), launch.browser = TRUE, display.mode = "showcase")
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")


##### rb2holes  -----
## tourr guided tour from rb to holes
rb2holes_panel <- tabPanel("rb2holes", fluidPage(
  mainPanel(
    h2("Guided tour from a random basis to holes() @: step_size = .6, d = 3"),
    sliderInput("rb2holes_basis_slider", label = "Basis number", 
                value = 1, min = 1, max = 1),
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
    h2("function surfaces, as sampled from geozoo and mvtnorm"),
    p("x1:3: grid values of 3D cube between [-3,3]"),
    p("top left (blue): orthogonal view; x1, x2, y1 = dmvnorm(0, cov(x))"),
    p("top right (red): orthogonal view; x1, x2, y2 = dmvnorm(sim. var-cov mat)"),
    p("bottom left (cyan): orthogonal view; x1, x2, 'y1.5' = .5*y1 + .5*y2"),
    p("bottom right (purple): orthogonal view; x1, x2, y3 = .5 * (max(y1) - y1) + .5 *y2"),
    rglwidgetOutput("widget_functionSurfaces"),
    hr(),
    h2("Static function surfaces, z = f(x,y)"),
    p("left:  z = ((x^2) + (3 * y^2)) * exp(-(x^2) - (y^2))"),
    p("right: z = (x^2) + (y^3); inflection pt not at extrema."),
    rglwidgetOutput("widget_functionSurfaces_STATIC")
  )
))

##### rb2holes  -----
## tourr guided tour from pca to cmass.
rb2holes_panel <- tabPanel("rb2holes", fluidPage(
  mainPanel(
    h2("Guided tour from a random basis to holes(), step_size = .6, d = 3 ;; 10 bases"),
    sliderInput("rb2holes_basis_slider", label = "Basis number", 
                value = 1, min = 1, max = 1),
    rglwidgetOutput("widget_rb2holes", width = w, height = h)
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
             functionSurfaces_panel,
             rb2holes_panel,
             widget_rotation_panel,
             pca_kde3d_panel,
             pca_kde2d_panel,
             logLik_panel
  )
)

