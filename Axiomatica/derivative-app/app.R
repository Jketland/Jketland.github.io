library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.16.8/dist/katex.min.css"),
    tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.16.8/dist/katex.min.js"),
    tags$script(HTML("
      function insertAtCursor(value) {
        var input = document.getElementById('expr');
        var start = input.selectionStart;
        var end = input.selectionEnd;
        input.value = input.value.substring(0, start) + value + input.value.substring(end);
        input.focus();
        input.setSelectionRange(start + value.length, start + value.length);
        input.dispatchEvent(new Event('input'));
      }
    "))
  ),
  
  titlePanel("Axiomatica: Derivative Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("expr", "Expression:", value = "x^2 + \\sin(x)"),
      div(style = "margin-bottom: 10px;",
          tags$button(type = "button", class = "btn btn-default", onclick = "insertAtCursor('x^2')", "x²"),
          tags$button(type = "button", class = "btn btn-default", onclick = "insertAtCursor('\\\\sin(')", "sin"),
          tags$button(type = "button", class = "btn btn-default", onclick = "insertAtCursor('\\\\cos(')", "cos"),
          tags$button(type = "button", class = "btn btn-default", onclick = "insertAtCursor('\\\\frac{}{}')", "a/b")
      ),
      actionButton("calc", "Calculate Derivative", class = "btn-primary")
    ),
    
    mainPanel(
      h4("Live Preview:"),
      uiOutput("math_preview"),
      hr(),
      h4("Derivative Steps:"),
      verbatimTextOutput("output_result")
    )
  )
)

server <- function(input, output, session) {
  output$math_preview <- renderUI({
    req(input$expr)
    math_string <- paste0("$$", input$expr, "$$")
    withMathJax(helpText(math_string))
  })
  
  output$output_result <- renderText({
    input$calc
    paste("Derivative engine step-by-step logic for:", isolate(input$expr))
  })
}

shinyApp(ui = ui, server = server)