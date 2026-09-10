library(shiny)

ui <- fluidPage(
  titlePanel("Simple Calculator"),
  numericInput("x", "First Number:", 10),
  numericInput("y", "Second Number:", 5),
  selectInput("op", "Operation:", c("Add" = "+", "Subtract" = "-", "Multiply" = "*", "Divide" = "/")),
  verbatimTextOutput("res")
)

server <- function(input, output, session) {
  output$res <- renderText({
    res <- switch(input$op,
                  "+" = input$x + input$y,
                  "-" = input$x - input$y,
                  "*" = input$x * input$y,
                  "/" = if(input$y != 0) input$x / input$y else "Division by zero error"
    )
    paste("Result:", res)
  })
}

shinyApp(ui, server)