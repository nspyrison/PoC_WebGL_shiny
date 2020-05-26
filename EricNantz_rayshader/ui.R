shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Shiny WebGL!"),
    
    # Sidebar with a slider input for number of points
    sidebarPanel(
        sliderInput("pts", 
                    "Number of points:", 
                    min = 10, 
                    max = 1000, 
                    value = 250)
    ),
    
    # Show the generated 3d scatterplot
    mainPanel(
        rglwidgetOutput("sctPlot")
    )
))
