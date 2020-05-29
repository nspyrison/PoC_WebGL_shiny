#" Minimal shiny UI for respective server.r

### Following: https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")

##### rgl -----
rgl_panel <- tabPanel("rgl", fluidPage(
  mainPanel(
    webGLOutput("rgl")
  )
))


##### pca3d -----
pca3d_panel <- tabPanel("pca3d", fluidPage(
  mainPanel(
    rglwidgetOutput("widget_pca3d")
  )
))

##### pca_de2d -----
pca_de2d_panel <- tabPanel("pca_de2d", fluidPage(
  mainPanel(
    p("text place holder, output in development"),
    rglwidgetOutput("widget_pca_de2d")
    
  )
))

##### fitDistr -----
fitDistr_panel <- tabPanel("fitDistr", fluidPage(
  mainPanel(
    rglwidgetOutput("widget_fitDistr")
  )
))

##### surface3d -----
surface3d_panel <- tabPanel("surface3d", fluidPage(
  mainPanel(
    p("text place holder, output in development"),
    rglwidgetOutput("widget_surface3d")
  )
))


##### ui -----
## Bring the panels together for full UI
ui <- fluidPage(
  h2("Nicholas Spyrison, 2020/05/29"),
  navbarPage("WebGL 3D visualizations, `rgl` package",
             pca3d_panel,
             pca_de2d_panel,
             fitDistr_panel,
             surface3d_panel
  ),
)