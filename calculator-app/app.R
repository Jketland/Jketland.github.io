# ==============================================================================
# Standalone Calculator App (calculator-app/app.R)
# ==============================================================================
library(shiny)

# --- 1. UI Component Definition ---
ui <- fluidPage(
  padding = 4,
  # --- Dual Display Screen Container ---
  fluidRow(
    column(width = 12, style = "display: flex; align-items: center; margin-bottom: 10px;",
           tags$input(id = "input_expr", type = "text", value = "0", 
                      style = "width: 280px; height: 34px; padding: 6px 12px; font-size: 14px; line-height: 1.42857143; color: #555; background-color: #fff; background-image: none; border: 1px solid #ccc; border-radius: 4px; box-shadow: inset 0 1px 1px rgba(0,0,0,.075);"),
           tags$span("=", style = "font-weight: bold; font-size: 18px; margin: 0 8px;"),
           tags$input(id = "mantissa_out", type = "text", value = "", 
                      style = "width: 140px; height: 34px; padding: 6px 12px; font-size: 14px; line-height: 1.42857143; color: #555; background-color: #fff; background-image: none; border: 1px solid #ccc; border-radius: 4px; box-shadow: inset 0 1px 1px rgba(0,0,0,.075);"),
           tags$span("e", style = "font-weight: bold; font-size: 16px; margin: 0 4px;"),
           tags$input(id = "exponent_out", type = "text", value = "", 
                      style = "width: 70px; height: 34px; padding: 6px 12px; font-size: 14px; line-height: 1.42857143; color: #555; background-color: #fff; background-image: none; border: 1px solid #ccc; border-radius: 4px; box-shadow: inset 0 1px 1px rgba(0,0,0,.075);")
    )
  ),
  # --- Keypad Container ---
  fluidRow(
    # Numbers Panel
    wellPanel(
      style = "padding: 8px; display: inline-block; vertical-align: top; margin-right: 5px;",
      tags$h4("Numbers", style = "margin-top: 0; font-size: 14px;"),
      fluidRow(
        column(12, 
               actionButton("btn_7", "7", class = "btn-default btn-sm"),
               actionButton("btn_8", "8", class = "btn-default btn-sm"),
               actionButton("btn_9", "9", class = "btn-default btn-sm")
        )
      ),
      fluidRow(column(12, style = "margin-top: 4px;",
                      actionButton("btn_4", "4", class = "btn-default btn-sm"),
                      actionButton("btn_5", "5", class = "btn-default btn-sm"),
                      actionButton("btn_6", "6", class = "btn-default btn-sm")
      )),
      fluidRow(column(12, style = "margin-top: 4px;",
                      actionButton("btn_1", "1", class = "btn-default btn-sm"),
                      actionButton("btn_2", "2", class = "btn-default btn-sm"),
                      actionButton("btn_3", "3", class = "btn-default btn-sm")
      )),
      fluidRow(column(12, style = "margin-top: 4px;",
                      actionButton("btn_0", "0", class = "btn-default btn-sm"),
                      actionButton("btn_dot", ".", class = "btn-default btn-sm"),
                      actionButton("btn_ee", "EE", class = "btn-default btn-sm")
      )),
      fluidRow(column(12, style = "margin-top: 4px;",
                      actionButton("btn_c", "C", class = "btn-danger btn-sm"),
                      actionButton("btn_del", "DEL", class = "btn-warning btn-sm")
      ))
    ),
    # Operators Panel
    wellPanel(
      style = "padding: 8px; display: inline-block; vertical-align: top; margin-right: 5px;",
      tags$h4("Ops", style = "margin-top: 0; font-size: 14px;"),
      actionButton("btn_lparen", "(", class = "btn-default btn-sm"),
      actionButton("btn_rparen", ")", class = "btn-default btn-sm"), br(),
      actionButton("btn_div", "/", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_mul", "x", class = "btn-default btn-sm", style = "margin-top: 4px;"), br(),
      actionButton("btn_sub", "-", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_add", "+", class = "btn-default btn-sm", style = "margin-top: 4px;"), br(),
      actionButton("btn_eq", "=", class = "btn-primary btn-sm", style = "margin-top: 4px;")
    ),
    # Memory Panel
    wellPanel(
      style = "padding: 8px; display: inline-block; vertical-align: top; margin-right: 5px;",
      tags$h4("Memory", style = "margin-top: 0; font-size: 14px;"),
      actionButton("btn_store_m1", "Store M1", class = "btn-info btn-sm"), br(),
      actionButton("btn_m1", "M1", class = "btn-default btn-sm", style = "margin-top: 4px;"), br(),
      actionButton("btn_store_m2", "Store M2", class = "btn-info btn-sm", style = "margin-top: 4px;"), br(),
      actionButton("btn_m2", "M2", class = "btn-default btn-sm", style = "margin-top: 4px;")
    ),
    # Constants Panel
    wellPanel(
      style = "padding: 8px; display: inline-block; vertical-align: top; margin-right: 5px;",
      tags$h4("Constants", style = "margin-top: 0; font-size: 14px;"),
      actionButton("btn_pi", "pi", class = "btn-default btn-sm"), br(),
      actionButton("btn_e_const", "e", class = "btn-default btn-sm", style = "margin-top: 4px;")
    ),
    # Functions Panel
    wellPanel(
      style = "padding: 8px; display: inline-block; vertical-align: top; margin-right: 5px;",
      tags$h4("Functions", style = "margin-top: 0; font-size: 14px;"),
      actionButton("btn_inv", "1/x", class = "btn-default btn-sm"),
      actionButton("btn_sq", "x^2", class = "btn-default btn-sm"),
      actionButton("btn_sqrt", "sqrt", class = "btn-default btn-sm"), br(),
      actionButton("btn_pow", "x^y", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_10pow", "10^x", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_exp", "e^x", class = "btn-default btn-sm", style = "margin-top: 4px;"), br(),
      actionButton("btn_log", "log", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_ln", "ln", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_fact", "fact", class = "btn-default btn-sm", style = "margin-top: 4px;")
    ),
    # Trig Functions Panel
    wellPanel(
      style = "padding: 8px; display: inline-block; vertical-align: top;",
      tags$h4("Trig", style = "margin-top: 0; font-size: 14px;"),
      radioButtons("angle_mode", label = NULL, choices = c("Rad" = "rad", "Deg" = "deg"), inline = TRUE, selected = "rad"),
      actionButton("btn_sin", "sin", class = "btn-default btn-sm"),
      actionButton("btn_cos", "cos", class = "btn-default btn-sm"),
      actionButton("btn_tan", "tan", class = "btn-default btn-sm"), br(),
      actionButton("btn_asin", "asin", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_acos", "acos", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_atan", "atan", class = "btn-default btn-sm", style = "margin-top: 4px;"), br(),
      actionButton("btn_sinh", "sinh", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_cosh", "cosh", class = "btn-default btn-sm", style = "margin-top: 4px;"),
      actionButton("btn_tanh", "tanh", class = "btn-default btn-sm", style = "margin-top: 4px;")
    )
  )
)

# --- 2. Server Definition ---
server <- function(input, output, session) {
  current_input <- reactiveVal("0")
  memory_1 <- reactiveVal("0")
  memory_2 <- reactiveVal("0")
  
  clean_zero <- function(val, eps = 1e-12) {
    if (is.numeric(val) && abs(val) < eps) return(0)
    return(val)
  }
  
  sin_mode <- function(x) {
    if (input$angle_mode == "deg") x <- x * pi / 180
    clean_zero(sin(x))
  }
  
  cos_mode <- function(x) {
    if (input$angle_mode == "deg") x <- x * pi / 180
    clean_zero(cos(x))
  }
  
  tan_mode <- function(x) {
    if (input$angle_mode == "deg") x <- x * pi / 180
    clean_zero(tan(x))
  }
  
  asin_mode <- function(x) {
    res <- asin(x)
    if (input$angle_mode == "deg") res <- res * 180 / pi
    clean_zero(res)
  }
  
  acos_mode <- function(x) {
    res <- acos(x)
    if (input$angle_mode == "deg") res <- res * 180 / pi
    clean_zero(res)
  }
  
  atan_mode <- function(x) {
    res <- atan(x)
    if (input$angle_mode == "deg") res <- res * 180 / pi
    clean_zero(res)
  }
  
  append_char <- function(char) {
    curr <- current_input()
    if (curr == "0" || curr == "Error") {
      if (char == ".") {
        current_input("0.")
      } else {
        current_input(char)
      }
    } else {
      current_input(paste0(curr, char))
    }
    updateTextInput(session, "input_expr", value = current_input())
  }
  
  observeEvent(input$btn_7, append_char("7"))
  observeEvent(input$btn_8, append_char("8"))
  observeEvent(input$btn_9, append_char("9"))
  observeEvent(input$btn_4, append_char("4"))
  observeEvent(input$btn_5, append_char("5"))
  observeEvent(input$btn_6, append_char("6"))
  observeEvent(input$btn_1, append_char("1"))
  observeEvent(input$btn_2, append_char("2"))
  observeEvent(input$btn_3, append_char("3"))
  observeEvent(input$btn_0, append_char("0"))
  observeEvent(input$btn_dot, append_char("."))
  observeEvent(input$btn_ee, append_char("e"))
  observeEvent(input$btn_lparen, append_char("("))
  observeEvent(input$btn_rparen, append_char(")"))
  observeEvent(input$btn_div, append_char("/"))
  observeEvent(input$btn_mul, append_char("*"))
  observeEvent(input$btn_sub, append_char("-"))
  observeEvent(input$btn_add, append_char("+"))
  observeEvent(input$btn_pi, append_char("pi"))
  observeEvent(input$btn_e_const, append_char("exp(1)"))
  observeEvent(input$btn_sin, append_char("sin("))
  observeEvent(input$btn_cos, append_char("cos("))
  observeEvent(input$btn_tan, append_char("tan("))
  observeEvent(input$btn_asin, append_char("asin("))
  observeEvent(input$btn_acos, append_char("acos("))
  observeEvent(input$btn_atan, append_char("atan("))
  observeEvent(input$btn_sinh, append_char("sinh("))
  observeEvent(input$btn_cosh, append_char("cosh("))
  observeEvent(input$btn_tanh, append_char("tanh("))
  observeEvent(input$btn_ln, append_char("log("))
  observeEvent(input$btn_log, append_char("log10("))
  observeEvent(input$btn_log2, append_char("log2("))
  observeEvent(input$btn_exp, append_char("exp("))
  observeEvent(input$btn_10pow, append_char("10^("))
  observeEvent(input$btn_sq, append_char("^2"))
  observeEvent(input$btn_pow, append_char("^"))
  observeEvent(input$btn_sqrt, append_char("sqrt("))
  observeEvent(input$btn_inv, append_char("1/("))
  observeEvent(input$btn_abs, append_char("abs("))
  observeEvent(input$btn_fact, append_char("factorial("))
  observeEvent(input$btn_pct, append_char("*(1/100)"))
  
  observeEvent(input$btn_c, {
    current_input("0")
    updateTextInput(session, "input_expr", value = "0")
    updateTextInput(session, "mantissa_out", value = "")
    updateTextInput(session, "exponent_out", value = "")
  })
  
  observeEvent(input$btn_del, {
    curr <- current_input()
    if (nchar(curr) <= 1 || curr == "Error") {
      current_input("0")
    } else {
      current_input(substr(curr, 1, nchar(curr) - 1))
    }
    updateTextInput(session, "input_expr", value = current_input())
  })
  
  observeEvent(input$btn_eq, {
    expr_str <- input$input_expr
    if (is.null(expr_str) || expr_str == "" || expr_str == "Error") return()
    clean_expr <- expr_str
    clean_expr <- gsub("\\bsin\\(", "sin_mode(", clean_expr)
    clean_expr <- gsub("\\bcos\\(", "cos_mode(", clean_expr)
    clean_expr <- gsub("\\btan\\(", "tan_mode(", clean_expr)
    clean_expr <- gsub("\\basin\\(", "asin_mode(", clean_expr)
    clean_expr <- gsub("\\bacos\\(", "acos_mode(", clean_expr)
    clean_expr <- gsub("\\batan\\(", "atan_mode(", clean_expr)
    res <- tryCatch({
      val <- eval(parse(text = clean_expr))
      clean_zero(val)
    }, error = function(e) {
      "Error"
    })
    if (is.na(res) || (is.character(res) && res == "Error")) {
      updateTextInput(session, "mantissa_out", value = "Error")
      updateTextInput(session, "exponent_out", value = "")
    } else {
      num <- as.numeric(res)
      if (num == 0) {
        updateTextInput(session, "mantissa_out", value = "0")
        updateTextInput(session, "exponent_out", value = "")
      } else if (abs(num) >= 1e10 || (abs(num) > 0 && abs(num) < 1e-4)) {
        sci_str <- sprintf("%.4e", num)
        parts <- strsplit(sci_str, "e")[[1]]
        updateTextInput(session, "mantissa_out", value = parts[1])
        updateTextInput(session, "exponent_out", value = parts[2])
      } else {
        updateTextInput(session, "mantissa_out", value = format(num, scientific = FALSE, digits = 8))
        updateTextInput(session, "exponent_out", value = "")
      }
    }
  })
  
  observeEvent(input$btn_store_m1, {
    memory_1(input$mantissa_out)
  })
  
  observeEvent(input$btn_m1, {
    append_char(memory_1())
  })
  
  observeEvent(input$btn_store_m2, {
    memory_2(input$mantissa_out)
  })
  
  observeEvent(input$btn_m2, {
    append_char(memory_2())
  })
}

shinyApp(ui, server)