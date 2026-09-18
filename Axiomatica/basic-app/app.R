library(shiny)

# ==============================================================================
# BASIC Interpreter Core Logic
# ==============================================================================
split_by_delimiter <- function(s, delim) {
  chars <- unlist(strsplit(s, ""))
  chunks <- c()
  current <- c()
  in_quotes <- FALSE
  
  for (ch in chars) {
    if (ch == '"') {
      in_quotes <- !in_quotes
      current <- c(current, ch)
    } else if (ch == delim && !in_quotes) {
      chunks <- c(chunks, paste0(current, collapse = ""))
      current <- c()
    } else {
      current <- c(current, ch)
    }
  }
  chunks <- c(chunks, paste0(current, collapse = ""))
  return(chunks)
}

run_basic_interpreter <- function(program_lines, input_fn = readline) {
  env <- new.env(parent = emptyenv())
  arr_env <- new.env(parent = emptyenv())
  prog <- list()
  out_buffer <- c()
  
  for (line_str in program_lines) {
    line_str <- trimws(line_str)
    if (!nzchar(line_str)) next
    
    m <- regexec("^([0-9]+)\\s+(.*)$", line_str, perl = TRUE)
    matches <- regmatches(line_str, m)[[1]]
    if (length(matches) == 3) {
      prog[[matches[2]]] <- trimws(matches[3])
    }
  }
  
  line_nums <- sort(as.numeric(names(prog)))
  if (length(line_nums) == 0) return(invisible(NULL))
  
  eval_expr <- function(expr_str, env, arr_env, exclude_vars = c()) {
    e_str <- trimws(expr_str)
    if (!nzchar(e_str)) return(0)
    
    # 1. Built-in Functions
    if (grepl("^(SQR|INT|ABS|SGN)\\s*\\(", e_str, ignore.case = TRUE)) {
      m <- regexec("^([A-Za-z]+)\\s*\\((.*)\\)$", e_str)
      parts <- regmatches(e_str, m)[[1]]
      if (length(parts) == 3) {
        fn_name <- toupper(parts[2])
        inner_val <- eval_expr(parts[3], env, arr_env, exclude_vars)
        return(switch(fn_name,
                      "SQR" = sqrt(inner_val),
                      "INT" = floor(inner_val),
                      "ABS" = abs(inner_val),
                      "SGN" = sign(inner_val),
                      NA
        ))
      }
    }
    
    # 2. Array Lookups
    if (grepl("^[A-Za-z][A-Za-z0-9_]*\\s*\\(", e_str)) {
      m <- regexec("^([A-Za-z][A-Za-z0-9_]*)\\s*\\((.*)\\)$", e_str)
      parts <- regmatches(e_str, m)[[1]]
      if (length(parts) == 3) {
        arr_name <- parts[2]
        idx_val <- as.integer(eval_expr(parts[3], env, arr_env, exclude_vars))
        if (!is.na(idx_val) && exists(arr_name, envir = arr_env)) {
          arr_data <- get(arr_name, envir = arr_env)
          pos <- idx_val + 1
          if (pos >= 1 && pos <= length(arr_data)) {
            val <- arr_data[pos]
            return(if (is.na(val)) 0 else val)
          }
        }
        return(0)
      }
    }
    
    # 3. Literals
    if (grepl("^[0-9]+(\\.[0-9]+)?$", e_str)) return(as.numeric(e_str))
    if (startsWith(e_str, "\"") && endsWith(e_str, "\"")) return(gsub("^\"|\"$", "", e_str))
    
    # 4. Scalar Variables
    if (exists(e_str, envir = env) && !(e_str %in% exclude_vars)) {
      val <- get(e_str, envir = env)
      return(if (is.null(val) || is.na(val)) 0 else val)
    }
    
    # 5. Compound Expressions
    parsed_str <- e_str
    
    while (grepl("([A-Za-z][A-Za-z0-9_]*)\\s*\\(([^)]+)\\)", parsed_str)) {
      m <- regexec("([A-Za-z][A-Za-z0-9_]*)\\s*\\(([^)]+)\\)", parsed_str)
      match_res <- regmatches(parsed_str, m)[[1]]
      if (length(match_res) >= 3) {
        full_sub <- match_res[1]
        arr_name <- match_res[2]
        idx_expr <- match_res[3]
        
        idx_val <- as.integer(eval_expr(idx_expr, env, arr_env, exclude_vars))
        val <- 0
        if (!is.na(idx_val) && exists(arr_name, envir = arr_env)) {
          arr_data <- get(arr_name, envir = arr_env)
          pos <- idx_val + 1
          if (pos >= 1 && pos <= length(arr_data)) {
            v <- arr_data[pos]
            if (!is.na(v)) val <- v
          }
        }
        parsed_str <- sub(full_sub, as.character(val), parsed_str, fixed = TRUE)
      } else {
        break
      }
    }
    
    var_names <- setdiff(ls(env), exclude_vars)
    if (length(var_names) > 0) {
      var_names <- var_names[order(nchar(var_names), decreasing = TRUE)]
      for (v in var_names) {
        val <- get(v, envir = env)
        if (is.null(val)) val <- 0
        parsed_str <- gsub(paste0("\\b", v, "\\b"), as.character(val), parsed_str)
      }
    }
    
    parsed_str <- gsub("(?<![<>=!])=(?![=])", "==", parsed_str, perl = TRUE)
    res <- tryCatch({ eval(parse(text = parsed_str)) }, error = function(err) NA)
    return(if (is.null(res) || is.na(res)) 0 else res)
  }
  
  set_val <- function(target_str, val, env, arr_env) {
    t_str <- trimws(target_str)
    m <- regexec("^([A-Za-z][A-Za-z0-9_]*)\\s*\\((.*)\\)$", t_str)
    parts <- regmatches(t_str, m)[[1]]
    
    if (length(parts) == 3) {
      arr_name <- parts[2]
      idx_val <- as.integer(eval_expr(parts[3], env, arr_env))
      if (!is.na(idx_val)) {
        pos <- idx_val + 1
        arr_data <- if (exists(arr_name, envir = arr_env)) get(arr_name, envir = arr_env) else c()
        if (pos > length(arr_data)) {
          length(arr_data) <- pos
          arr_data[is.na(arr_data)] <- 0
        }
        arr_data[pos] <- val
        assign(arr_name, arr_data, envir = arr_env)
      }
    } else {
      assign(t_str, val, envir = env)
    }
  }
  
  pc <- 1
  for_stack <- list()
  
  while (pc <= length(line_nums)) {
    cur_line_num <- line_nums[pc]
    statements <- split_by_delimiter(prog[[as.character(cur_line_num)]], ":")
    
    st_idx <- 1
    jumped <- FALSE
    
    while (st_idx <= length(statements)) {
      stmt <- trimws(statements[st_idx])
      if (!nzchar(stmt)) { st_idx <- st_idx + 1; next }
      
      if (grepl("^REM\\b", stmt, ignore.case = TRUE) || grepl("^CLS\\b", stmt, ignore.case = TRUE)) {
        # No-op
      }
      else if (grepl("^DIM\\s+", stmt, ignore.case = TRUE)) {
        dim_content <- sub("^DIM\\s+", "", stmt, ignore.case = TRUE)
        m <- regexec("^([A-Za-z][A-Za-z0-9_]*)\\s*\\((.*)\\)$", dim_content)
        parts <- regmatches(dim_content, m)[[1]]
        if (length(parts) == 3) {
          arr_name <- parts[2]
          size <- as.integer(eval_expr(parts[3], env, arr_env))
          if (!is.na(size)) assign(arr_name, rep(0, size + 1), envir = arr_env)
        }
      }
      else if (grepl("^LET\\s+", stmt, ignore.case = TRUE) || grepl("^[A-Za-z][A-Za-z0-9_]*(\\s*\\([^)]+\\))?\\s*=", stmt)) {
        clean_stmt <- sub("^LET\\s+", "", stmt, ignore.case = TRUE)
        eq_parts <- strsplit(clean_stmt, "=", fixed = TRUE)[[1]]
        target <- trimws(eq_parts[1])
        expr <- trimws(paste(eq_parts[-1], collapse = "="))
        val <- eval_expr(expr, env, arr_env)
        set_val(target, val, env, arr_env)
      }
      else if (grepl("^PRINT\\b", stmt, ignore.case = TRUE)) {
        p_content <- sub("^PRINT\\s*", "", stmt, ignore.case = TRUE)
        suppress_nl <- endsWith(trimws(p_content), ";")
        if (suppress_nl) p_content <- sub(";\\s*$", "", trimws(p_content))
        
        if (nzchar(p_content)) {
          chunks <- split_by_delimiter(p_content, ";")
          line_buf <- c()
          for (chunk in chunks) {
            chunk <- trimws(chunk)
            if (!nzchar(chunk)) next
            val <- eval_expr(chunk, env, arr_env)
            line_buf <- c(line_buf, as.character(val))
          }
          out_buffer <- c(out_buffer, paste0(line_buf, collapse = ""))
          if (!suppress_nl) out_buffer <- c(out_buffer, "\n")
        } else {
          out_buffer <- c(out_buffer, "\n")
        }
      }
      else if (grepl("^IF\\s+", stmt, ignore.case = TRUE)) {
        if_content <- sub("^IF\\s+", "", stmt, ignore.case = TRUE)
        if_parts <- unlist(strsplit(if_content, "(?i)\\bthen\\b", perl = TRUE))
        if (length(if_parts) >= 2) {
          cond_val <- eval_expr(if_parts[1], env, arr_env)
          if (!is.na(cond_val) && cond_val != 0) {
            action_stmt <- trimws(paste(if_parts[-1], collapse = "THEN"))
            if (nzchar(action_stmt)) {
              if (grepl("^GOTO\\s+", action_stmt, ignore.case = TRUE) || grepl("^[0-9]+$", action_stmt)) {
                target_line <- as.character(as.integer(sub("^(GOTO\\s+)?", "", action_stmt, ignore.case = TRUE)))
                new_pc <- match(as.numeric(target_line), line_nums)
                if (!is.na(new_pc)) { pc <- new_pc; jumped <- TRUE; break }
              } else {
                action_stmts <- split_by_delimiter(action_stmt, ":")
                statements <- c(head(statements, st_idx), action_stmts, tail(statements, -st_idx))
              }
            }
          }
        }
      }
      else if (grepl("^FOR\\s+", stmt, ignore.case = TRUE)) {
        for_content <- sub("^FOR\\s+", "", stmt, ignore.case = TRUE)
        eq_parts <- strsplit(for_content, "=", fixed = TRUE)[[1]]
        v_name <- trimws(eq_parts[1])
        rest <- trimws(paste(eq_parts[-1], collapse = "="))
        
        step_val <- 1
        if (grepl("(?i)\\bstep\\b", rest)) {
          step_parts <- unlist(strsplit(rest, "(?i)\\bstep\\b", perl = TRUE))
          rest <- trimws(step_parts[1])
          step_val <- eval_expr(step_parts[2], env, arr_env)
        }
        
        to_parts <- unlist(strsplit(rest, "(?i)\\bto\\b", perl = TRUE))
        start_val <- eval_expr(to_parts[1], env, arr_env)
        end_val <- eval_expr(to_parts[2], env, arr_env)
        
        assign(v_name, start_val, envir = env)
        
        next_st_idx <- st_idx + 1
        next_pc <- pc
        if (next_st_idx > length(statements)) {
          next_pc <- pc + 1
          next_st_idx <- 1
        }
        for_stack[[v_name]] <- list(pc_idx = next_pc, stmt_idx = next_st_idx, end_val = end_val, step_val = step_val, var_name = v_name)
      }
      else if (grepl("^NEXT\\b", stmt, ignore.case = TRUE)) {
        v_name <- trimws(sub("^NEXT\\s*", "", stmt, ignore.case = TRUE))
        if (!nzchar(v_name) && length(for_stack) > 0) v_name <- for_stack[[length(for_stack)]]$var_name
        
        if (nzchar(v_name) && !is.null(for_stack[[v_name]])) {
          loop_info <- for_stack[[v_name]]
          cur_val <- get(v_name, envir = env) + loop_info$step_val
          assign(v_name, cur_val, envir = env)
          
          condition_met <- if (loop_info$step_val > 0) (cur_val <= loop_info$end_val) else (cur_val >= loop_info$end_val)
          
          if (condition_met) {
            pc <- loop_info$pc_idx
            st_idx <- loop_info$stmt_idx
            jumped <- TRUE
            break
          } else {
            for_stack[[v_name]] <- NULL
          }
        }
      }
      else if (grepl("^GOTO\\s+", stmt, ignore.case = TRUE)) {
        target_line <- as.character(as.integer(sub("^GOTO\\s+", "", stmt, ignore.case = TRUE)))
        new_pc <- match(as.numeric(target_line), line_nums)
        if (!is.na(new_pc)) { pc <- new_pc; jumped <- TRUE; break }
      }
      st_idx <- st_idx + 1
    }
    if (!jumped) pc <- pc + 1
  }
  return(paste(out_buffer, collapse = ""))
}

# ==============================================================================
# Shiny Application UI & Server
# ==============================================================================
ui <- fluidPage(
  titlePanel("Axiomatica: BASIC Emulator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 6,
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
          tags$h4("BASIC Editor", style = "margin: 0;"),
          actionButton("run_btn", "RUN", class = "btn-primary btn-sm")
      ),
      textAreaInput("script_code", label = NULL, 
                    value = paste(
                      '10 REM Sieve of Eratosthenes',
                      '20 CLS',
                      '30 PRINT "Sieve of Eratosthenes - Prime Numbers"',
                      '40 N = 50',
                      '50 DIM PRIME(N)',
                      '60 FOR I = 2 TO N',
                      '70 PRIME(I) = 1',
                      '80 NEXT I',
                      '90 FOR I = 2 TO SQR(N)',
                      '100 IF PRIME(I) = 0 THEN 140',
                      '110 FOR J = I * I TO N STEP I',
                      '120 PRIME(J) = 0',
                      '130 NEXT J',
                      '140 NEXT I',
                      '150 PRINT "Primes up to "; N; ":"',
                      '160 FOR I = 2 TO N',
                      '170 IF PRIME(I) = 1 THEN PRINT I; " ";',
                      '180 NEXT I',
                      '190 PRINT',
                      sep = "\n"
                    ),
                    rows = 22, width = "100%")
    ),
    
    mainPanel(
      width = 6,
      tags$h4("Program Output"),
      verbatimTextOutput("program_output"),
      
      tags$h4("Interpreter Status & Logs"),
      verbatimTextOutput("interpreter_status")
    )
  )
)

server <- function(input, output, session) {
  exec_state <- reactiveVal(list(output = "Ready to run.", status = "Idle."))
  
  observeEvent(input$run_btn, {
    req(input$script_code)
    code_str <- input$script_code
    program_lines <- unlist(strsplit(code_str, "\n"))
    
    result <- tryCatch({
      out_text <- run_basic_interpreter(program_lines, input_fn = function(p) "")
      out_val <- if (is.null(out_text) || !nzchar(out_text)) "[Program completed with no output]" else out_text
      list(output = out_val, status = "Execution finished successfully.")
    }, error = function(e) {
      list(output = "", status = paste0("Error: ", e$message))
    })
    
    exec_state(result)
  })
  
  output$program_output <- renderText({ exec_state()$output })
  output$interpreter_status <- renderText({ exec_state()$status })
}

shinyApp(ui = ui, server = server)