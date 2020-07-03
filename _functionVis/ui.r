####### _functionVis/ui.r ----
#' shiny UI for  server.r
require("shiny")
require("shinyWidgets")
require("tourr")
require("spinifex") ## v0.2.9000 and up
require("rgl")
library("alphashape3d")
require("RColorBrewer")
require("ggplot2")
require("gridExtra")
require("lqmm")
require("mvtnorm")
require("dplyr")
require("tidyr")
require("ggpubr")

w <- h <- "640px" ## height and width of the rgl widget in pixels, as applied in UI *Output() function.

##### Start of shiny ui ----
### Following: 
## https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo",  package = "rgl"), launch.browser = TRUE, display.mode = "showcase")
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")

functionSurfaces_panel <- tabPanel("function vis -- slicing on 'back variables'", fluidPage(
  sidebarPanel(width = 3, fluidRow(
    selectInput("dat", label = "Data",
                choices = c("grid cube", "simulation", "flea", "wine")),
    numericInput("tgt_rel_h", 
                 "Target fraction of backdimension volume (slice widths adjust to x^(1/p-d))",
                 value = .25, ## .25 as per [Laa et al. 2019] Hole or Grain 5.1 #3.
                 min = .05, max = 1, step = .05),
    uiOutput("back_dimensions_ui"),
    column(width = 6,
           shinyWidgets::switchInput(inputId = "DO_DISP_a_hull_triang", 
                                     label = "Display alpha hull triangles", 
                                     value = TRUE)
    ),
    column(width = 6,
           conditionalPanel(
             "input.DO_DISP_a_hull_triang == true",
             numericInput("a_hull_radius", label = "Alpha hull radius [1/alpha]", 
                          value = 5, min = .5, max = 10, step = .5),
             verbatimTextOutput("a_hull_alpha")
           )
    )
  )), ### Close sidebarPanel()
  mainPanel(
    fluidRow(
      column(width = 2, plotOutput("bd_histograms")),
      column(width = 10, rglwidgetOutput("widget_functionVis", w, h))
    )
  )
  
)) ### Close tabPanel(), assigning functionSurfaces_panel



##### ui -----
## Bring the panels together for full UI
ui <- fluidPage(
  navbarPage("functionVis",
             functionSurfaces_panel
  )
)

