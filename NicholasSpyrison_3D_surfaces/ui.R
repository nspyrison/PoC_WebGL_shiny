library(shiny)
library(shinyRGL)

#' Minimal shiny UI for respective server.r
ui <- fluidPage(
  # Application title
  headerPanel("package 'shinyRGL'"),
  
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
  
  # Show the generated 3d scatterplot
  mainPanel(
    webGLOutput("shinyRGL")
  )
)