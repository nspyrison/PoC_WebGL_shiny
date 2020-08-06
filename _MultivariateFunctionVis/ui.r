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
backDimensionSlices_panel <- tabPanel("Slicing on back-dimenions", fluidPage(
  sidebarPanel(width = 3L, fluidRow(
    selectInput("dat", label = "Data", choices = c("simulation", "cube")),
    selectInput("func_nm", "Function to apply", choices = c("kde2d", "dmvnorm")),
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
                 value = def_rel_h, min = .05, max = 1L, step = .05),
    uiOutput("bd_slices_ui"),
    column(width = 6L,
           shinyWidgets::switchInput(inputId = "DO_DISP_a_hull_triang", 
                                     label = "Display alpha hull triangles", 
                                     value = TRUE)
    ),
    column(width = 6L,
           conditionalPanel(
             "input.DO_DISP_a_hull_triang == true",
             numericInput("a_hull_alpha", label = "Alpha (~1/alpha hull",
                          value = 10L, #round(def_rel_h, 1),
                          min = 1L, max = 20L, step = 1L),
             #verbatimTextOutput("a_hull_alpha"),
             verbatimTextOutput("a_hull_radius")
           )
    )
  )), ## Close sidebarPanel()
  mainPanel(
    fluidRow(
      column(width = 2L, 
             h4("Front dimensions"),
             plotOutput("fd_histograms"),
             h4("Back dimensions"),
             plotOutput("bd_histograms")
      ),
      column(width = 10L, 
             rglwidgetOutput("widget_functionVis", w, h)
      )
    )
  ) ## Close mainPanel()
)) ## Close tabPanel(), assigns backDimensionSlices_panel


##### ui -----
## Bring the panels together for full UI
ui <- fluidPage(
  navbarPage("Multivariate Function Visualization",
             backDimensionSlices_panel
  )
)

