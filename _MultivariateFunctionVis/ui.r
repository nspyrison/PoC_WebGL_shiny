####### _functionVis/ui.r -----
#' shiny UI for server.r
require("shiny")
require("shinyWidgets")
require("tourr")
require("rgl")
require("alphashape3d")
require("RColorBrewer")
require("ggplot2")
require("lqmm")
require("mvtnorm")
require("dplyr")
require("tidyr")

w <- h <- "800px" ## height and width of the rgl widget in pixels, as applied in UI *Output() function.
def_rel_h <- .25  ## .25 as per [Laa et al. 2019] Hole or Grain 5.1 #3.


##### Start of shiny ui -----
### Following: 
## https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo",  package = "rgl"), launch.browser = TRUE, display.mode = "showcase")
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")

functionSurfaces_panel <- tabPanel("function vis -- slicing on 'back variables'", fluidPage(
  sidebarPanel(width = 3, fluidRow(
    selectInput("dat", label = "Data", choices = c("grid cube", "simulation")),
    selectInput("numFunc", "Function to apply", choices = c("kde2d", "dmvnorm")),
    conditionalPanel(
      "input.dat == 'simulation'",
      textInput('sim_mns_a', 'Variable means for cluster a (comma delimited)', "0,8,0,0"),
      textInput('sim_mns_b', 'Variable means for cluster b (comma delimited)', "8,0,0,0"),
      p("Covariance matrices fixed to identity(p).")
    ),
    p("Back slice is aggregating to the max of all points, with red bar extending to the minimum."),
    # selectInput("bslice_agg", "Back slice aggregation",
    #             choices =  c("max", "mean", "median", "min")),
    numericInput("tgt_rel_h", "Target fraction of backdimension volume (slice widths adjust to x^(1/p-d))",
                 value = def_rel_h, min = .05, max = 1, step = .05),
    uiOutput("back_dimensions_ui"),
    column(width = 6,
           shinyWidgets::switchInput(inputId = "DO_DISP_a_hull_triang", 
                                     label = "Display alpha hull triangles", 
                                     value = TRUE)
    ),
    column(width = 6,
           conditionalPanel(
             "input.DO_DISP_a_hull_triang == true",
             numericInput("a_hull_alpha", label = "Alpha (~1/alpha hull",
                          value = 1, #round(def_rel_h, 1),
                          min = .1, max = 8, step = .1),
             #verbatimTextOutput("a_hull_alpha"),
             verbatimTextOutput("a_hull_radius")
           )
    )
  )), ## Close sidebarPanel()
  mainPanel(
    fluidRow(
      column(width = 2, plotOutput("bd_histograms")),
      column(width = 10, rglwidgetOutput("widget_functionVis", w, h))
    )
  ) ## Close mainPanel()
)) ## Close tabPanel(), assigning functionSurfaces_panel


##### ui -----
## Bring the panels together for full UI
ui <- fluidPage(
  navbarPage("functionVis",
             functionSurfaces_panel
  )
)

