#' Minimal shiny UI for respective server.r

### Following: https://stackoverflow.com/questions/39363384/how-to-remove-unwanted-text-output-with-shiny-rgl
# shiny::runApp(system.file("shinyDemo", package = "rgl"), launch.browser = TRUE)
# shiny::runApp(system.file("shinySimple", package = "rgl"), launch.browser = TRUE)

##### rgl -----
rgl_panel <- tabPanel("rgl", fluidPage(
  mainPanel(
    webGLOutput("rgl")
  )
))


##### shinyRGL -----
shinyRGL_panel <- tabPanel("shinyRGL", fluidPage(
                           # sidebarPanel(
                           #   sliderInput("ngrid", 
                           #               "Number of density grid levelss:", 
                           #               min = 1, 
                           #               max = 100, 
                           #               value = 40),
                           #   sliderInput("zscale", 
                           #               "z coef for 2d mass surfaces:", 
                           #               min = .1, 
                           #               max = 2, 
                           #               value = .5)
                           # ),
                           mainPanel(
                             webGLOutput("shinyRGL")
                           )
))

##### rayshader -----
rayshader_panel <- tabPanel("rayshader", fluidPage(
  mainPanel(
    p("this is text"),
    rglwidgetOutput("rayshader")
  )
))

##### another pkg? -----

##### ui -----
## Bring the panels together for full UI
ui <- fluidPage(
  navbarPage("Exploring WebGL packages -- NS",
             shinyRGL_panel,
             rayshader_panel
  ),
)