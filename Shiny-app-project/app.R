library(shiny)

# Define UI for app that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Predict MPG from the mtcars dataset"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("disp", "Displacement:",
                  min = min(mtcars$disp),
                  max = max(mtcars$disp),
                  value = median(mtcars$disp)),
      sliderInput("hp", "Horsepower:",
                  min = min(mtcars$hp),
                  max = max(mtcars$hp),
                  value = median(mtcars$hp)),
      sliderInput("wt", "Weight (1000 lbs):",
                  min = min(mtcars$wt),
                  max = max(mtcars$wt),
                  value = median(mtcars$wt))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("mpgPrediction")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive expression to create the linear model
  model <- reactive({
    lm(mpg ~ disp + hp + wt, data = mtcars)
  })
  
  # Calculate the predicted mpg based on user input
  output$mpgPrediction <- renderText({
    pred <- predict(model(), newdata = data.frame(disp = input$disp, hp = input$hp, wt = input$wt))
    paste("Predicted MPG:", round(pred, 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
