#" Minimal shiny UI for respective server.r
library(shiny)
library(rgl)

### Following: https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")

##### rgl -----
rgl_panel <- tabPanel("rgl", fluidPage(
  mainPanel(
    h2("3D ellipsoid of full data to 1 Std Dev (conf = 68%)"),
    rglwidgetOutput("rgl")
  )
))


##### pca3d -----
pca3d_panel <- tabPanel("pca3d", fluidPage(
  mainPanel(
    h2("2D kernel density estimation"),
    rglwidgetOutput("widget_pca3d")
  )
))

##### pca_de2d -----
pca_de2d_panel <- tabPanel("pca_de2d", fluidPage(
  mainPanel(
    h2("Two-dimensional kernel density estimation"),
    rglwidgetOutput("widget_pca_de2d")
    
  )
))

##### fitDistr -----
fitDistr_panel <- tabPanel("fitDistr", fluidPage(
  mainPanel(
    h2("Parameter estimation of a sampled gamma(5, .1)"),
    rglwidgetOutput("widget_fitDistr")
  )
))

##### surface3d -----
surface3d_panel <- tabPanel("surface3d", fluidPage(
  
  mainPanel(
    h2("function surfaces, z = f(x,y)"),
    p("(left) z = ((x^2) + (3 * y^2)) * exp(-(x^2) - (y^2))"),
    p("(right) z = (x^2) + (y^3) (saddle shape)"),
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

