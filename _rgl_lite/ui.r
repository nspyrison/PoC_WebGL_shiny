#" Minimal shiny UI for respective server.r
require("shiny")
require("rgl")
require("RColorBrewer")

w <- h <- "600px" ## height and width of the rgl widget in pixels, 

### Define a couple local function to remove spinifex.
app_col_of <- function(category, pallet_name = "Dark2") {
  .l_lvls <- length(levels(category))
  if (.l_lvls == 0) stop("Length of 'category' cannot be zero.")
  if (.l_lvls > 12) stop("'category' has more than the expected max of 12 levels.")
  pal <- suppressWarnings(RColorBrewer::brewer.pal(.l_lvls, pallet_name))
  pal[as.integer(factor(category))]
}

app_pch_of <- function(category) {
  .l_lvls <- length(levels(category))
  if (.l_lvls == 0) stop("Length of 'category' cannot be zero.")
  if (.l_lvls > 12) stop("'category' has more than the expected max of 12 levels.")
  y_ord <- c(21:25, 3:4, 7:11)
  int_lvls <- as.integer(factor(category))
  y_ord[int_lvls]
}

##### rb2holes  -----
## tourr guided tour from rb to holes
rb2holes_panel <- tabPanel("rb2holes", fluidPage(
  mainPanel(
    h2("Guided tour from a random basis to holes() @: step_size = .6, d = 3"),
    sliderInput("rb2holes_basis_slider", label = "Basis number", 
                value = 1, min = 1, max = 1),
    textOutput("slider_t"),
    rglwidgetOutput("widget_rb2holes", width = w, height = h)
  )
))


##### ui -----
## Bring the panels together for full UI
ui <- fluidPage(
  h2("Nicholas Spyrison, 2020/05/29"),
  navbarPage("WebGL 3D visualizations, `rgl` package",
             rb2holes_panel
  )
)

