####### _functionVis/ui.r ----
#' shiny UI for  server.r
require("shiny")
require("shinyWidgets")
require("MASS")
require("spinifex") ## v0.2.9000 and up
require("tourr")
require("rgl")
require("RColorBrewer")
require("htmlwidgets")
require("jsonlite")
require("geometry")
library("alphashape3d")

w <- h <- "600px" ## height and width of the rgl widget in pixels, as applied in UI *Output() function.

##### Start of shiny ui ----
### Following: 
## https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo",  package = "rgl"), launch.browser = TRUE, display.mode = "showcase")
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")

functionSurfaces_panel <- tabPanel("function vis -- slicing on 'back variables'", fluidPage(
  sidebarPanel(
    width = 3,
    fluidRow(
      column(width = 6,
             shinyWidgets::switchInput(inputId = "DO_DISP_a_hull_triang", 
                                       label = "Display alpha hull triangles", 
                                       value = TRUE)),
      column(width = 6,
             conditionalPanel(
               "input.DO_DISP_a_hull_triang == true",
               numericInput("a_hull_radius", label = "Alpha hull radius [1/alpha]", 
                            value = 1, min = .05, max = 1, step = .05)
             ) 
      ),
      numericInput("tgt_rel_h", 
                   "Target fraction of obs (slice thickness defaults to x^(p-d))",
                   value = .25, ## .25 as per [Laa et al. 2019] Hole or Grain 5.1 #3.
                   min = .05, max = 1, step = .05)
    ),
    uiOutput("backDimensionInputs")
  ),
  mainPanel(
    rglwidgetOutput("widget_functionVis")
  )
))

##### STALE_functionSurfaces -----
STALE_functionSurfaces_panel <- tabPanel("functionSurfaces_fromPoC", fluidPage(
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

