#" Minimal shiny UI for respective server.r
library(shiny)
library(rgl)

### Following: 
## https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE, display.mode = "showcase")

##### rgl -----
rgl_panel <- tabPanel("rgl", fluidPage(
  mainPanel(
    h2("3D ellipsoid of full data to 1 Std Dev (conf = 68%)"),
    rglwidgetOutput("rgl")
  )
))


##### pca_kde3d -----
## Kernal estimation on covar matrix (1 SD, 68% obs within volume)
pca_kde3d_panel <- tabPanel("pca_kde3d", fluidPage(
  mainPanel(
    h2("3D kernel density estimation. on covar matrix, 1 SD mesh, ~68% obs lie within bounds"),
    rglwidgetOutput("widget_pca_kde3d")
  )
))

##### pca_kde2d  -----
## Via MASS::kde2d()
pca_kde2d_panel <- tabPanel("pca_kde2d", fluidPage(
  mainPanel(
    h2("2D kernel density estimation. via MASS::kde3d."),
    rglwidgetOutput("widget_pca_kde2d")
    
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
    h2("Function surfaces, z = f(x,y), widget rotation not linked."),
    p("left:  z = ((x^2) + (3 * y^2)) * exp(-(x^2) - (y^2))"),
    p("right: z = (x^2) + (y^3); inflection pt not at extrema."),
    rglwidgetOutput("widget_functionSurfaces")
  )
))

##### pca2cmass  -----
## tourr guided tour from pca to cmass.
pca2cmass_panel <- tabPanel("pca2cmass", fluidPage(
  mainPanel(
    h2("Guided tour from pca to cmass, step_size = .6, d = 3 ;; 10 bases"),
    sliderInput("pca2cmass_basis_slider", label = "Basis number", 
                value = -999, min = -999, max = -999),
    rglwidgetOutput("widget_pca2cmass")
  )
))


##### ui -----
## Bring the panels together for full UI
ui <- fluidPage(
  h2("Nicholas Spyrison, 2020/05/29"),
  navbarPage("WebGL 3D visualizations, `rgl` package",
             pca_kde3d_panel,
             pca_kde2d_panel,
             logLik_panel,
             functionSurfaces_panel,
             pca2cmass_panel
  )
)

