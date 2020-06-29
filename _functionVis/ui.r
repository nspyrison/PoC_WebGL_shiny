####### _functionVis/ui.r ----
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

w <- h <- "600px" ## height and width of the rgl widget in pixels, as applied in UI *Output() function.

##### Local app_* functions -----
##TODO: move to spinifex
app_col_of <- function(class, pallet_name = "Dark2") {
  .l_lvls <- length(levels(class))
  if (.l_lvls == 0) stop("Length of 'class' cannot be zero.")
  if (.l_lvls > 12) stop("'class' has more than the expected max of 12 levels.")
  pal <- suppressWarnings(RColorBrewer::brewer.pal(.l_lvls, pallet_name))
  pal[as.integer(factor(class))]
}
##TODO: move to spinifex
app_pch_of <- function(class) {
  .shape_ord <- c(21:25, 3:4, 7:14)
  .l_shape_lvls <- length(unique(.shape_ord))
  if (is.factor(class) == FALSE) class <- as.factor(class)
  .l_class_lvls <- length(levels(class))
  if (.l_lvls == 0) stop("Length of 'class' cannot be zero.")
  if (.l_lvls > 12) stop("'class' has more than the expected max of 12 levels.")
  
  int_lvls <- as.integer(factor(class))
  y_ord[int_lvls]
}

##### Start of shiny ui ----
### Following: 
## https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo",  package = "rgl"), launch.browser = TRUE, display.mode = "showcase")
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")

functionSurfaces_panel <- tabPanel("function vis -- slicing on 'back variables'", fluidPage(
  1
))

##### STALE_functionSurfaces -----
STALE_functionSurfaces_panel <- tabPanel("functionSurfaces", fluidPage(
  mainPanel(
    h2("function surfaces, sampled from `geozoo` and y from `mvtnorm`::dmvnorm()"),
    p("x1:3: grid values of 3D cube between [-3,3]"),
    p("y1:2: 'density function for the multivariate normal distribution given 3D mean and covar matrix"),
    p("top left (blue): orthogonal view; x1, x2, y1 = dmvnorm(0, cov(x))"),
    p("top right (red): orthogonal view; x1, x2, y2 = dmvnorm(sim. var-cov mat)"),
    p("bottom left (cyan): orthogonal view; x1, x2, 'y1.5' = .5*y1 + .5*y2"),
    p("bottom right (purple): orthogonal view; x1, x2, y3 = .5 * (max(y1) - y1) + .5 *y2"),
    rglwidgetOutput("widget_functionSurfaces_STALE")
  )
))


##### ui -----
## Bring the panels together for full UI
ui <- fluidPage(
  h2("Nicholas Spyrison, 2020/05/29"),
  navbarPage("functionVis",
             functionSurfaces_panel,
             STALE_functionSurfaces_panel
  )
)

