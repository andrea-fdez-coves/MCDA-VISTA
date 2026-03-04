#' Performance Main Module
#'
#' This module coordinates the performance assessment functionality, including
#' performance matrix, scoring method selection, and calling specific scoring modules.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param decision_data Reactive values from decision module
#' @param criteria_data Reactive values from criteria module
#' @param config Configuration list with pre-defined settings (optional)
#'
#' @return A list containing:
#' \itemize{
#'   \item{data - Reactive values for performance data}
#'   \item{ui_outputs - List of UI rendering functions}
#'   \item{utilities - Utility functions for filtering and data access}
#' }
#'
#' @export
performance_main_module <- function(input, output, session, 
                                    decision_data, criteria_data,
                                    config = NULL) {
  
  # Namespace for this module
  ns <- session$ns
  
  # Initialize reactive values for performance
  performance_data <- reactiveValues(
    intervention = list(),
    comparators = list(),
    matrix = NULL,
    scoring_method = "direct",
    scoring_justification = "",
    direct_min = 0,
    direct_max = 100,
    ahp_comparisons = list(),
    ahp_scores = NULL,
    ahp_matrix = NULL,
    ahp_consistency = NULL,
    value_functions = list(),
    value_scores = list(),
    notes = "",
    config_loaded = FALSE,
    config_name = "",
    config_description = "",
    config_version = "",
    config_author = "",
    config_date = "",
    locked_fields = list(
      selected_levels = FALSE,
      scoring_method = FALSE,
      scoring_justification_required = FALSE,
      score_range = FALSE
    ),
    default_selected_levels = c(1, 2, 3, 4),
    default_scoring_method = "direct"
  )
  
  #### Configuration Loading ####
  
  #' Load Configuration
  #'
  #' Loads pre-defined settings from configuration if provided
  observe({
    req(!is.null(config))
    
    # Store configuration info
    performance_data$config_loaded <- TRUE
    performance_data$config_name <- config$config_name %||% "Unknown"
    performance_data$config_description <- config$config_description %||% ""
    performance_data$config_version <- config$config_version %||% ""
    performance_data$config_author <- config$config_author %||% ""
    performance_data$config_date <- config$config_date %||% Sys.Date()
    
    # Load performance-specific settings from config
    if (!is.null(config$performance)) {
      
      # Set default selected levels
      if (!is.null(config$performance$default_selected_levels)) {
        performance_data$default_selected_levels <- config$performance$default_selected_levels
      }
      
      # Set default scoring method
      if (!is.null(config$performance$default_scoring_method)) {
        performance_data$default_scoring_method <- config$performance$default_scoring_method
        performance_data$scoring_method <- config$performance$default_scoring_method
      }
      
      # Set default scoring justification (optional)
      if (!is.null(config$performance$default_scoring_justification)) {
        performance_data$scoring_justification <- config$performance$default_scoring_justification
      }
      
      # Set score range limits
      if (!is.null(config$performance$direct_min)) {
        performance_data$direct_min <- config$performance$direct_min
      }
      
      if (!is.null(config$performance$direct_max)) {
        performance_data$direct_max <- config$performance$direct_max
      }
      
      # Load performance data if provided
      if (!is.null(config$performance$intervention)) {
        performance_data$intervention <- config$performance$intervention
      }
      
      if (!is.null(config$performance$comparators)) {
        performance_data$comparators <- config$performance$comparators
      }
      
      if (!is.null(config$performance$matrix)) {
        performance_data$matrix <- config$performance$matrix
      }
      
      if (!is.null(config$performance$value_functions)) {
        performance_data$value_functions <- config$performance$value_functions
      }
      
      if (!is.null(config$performance$ahp_comparisons)) {
        performance_data$ahp_comparisons <- config$performance$ahp_comparisons
      }
      
      if (!is.null(config$performance$ahp_scores)) {
        performance_data$ahp_scores <- config$performance$ahp_scores
      }
      
      if (!is.null(config$performance$ahp_matrix)) {
        performance_data$ahp_matrix <- config$performance$ahp_matrix
      }
      
      # Set locked fields
      if (!is.null(config$performance$locked_fields)) {
        for (field in names(config$performance$locked_fields)) {
          performance_data$locked_fields[[field]] <- config$performance$locked_fields[[field]]
        }
      }
      
      # For backward compatibility with older config structure
      if (!is.null(config$locked_fields)) {
        if (!is.null(config$locked_fields$performance_selected_levels)) {
          performance_data$locked_fields$selected_levels <- config$locked_fields$performance_selected_levels
        }
        if (!is.null(config$locked_fields$performance_scoring_method)) {
          performance_data$locked_fields$scoring_method <- config$locked_fields$performance_scoring_method
        }
      }
      
      # Log configuration loaded
      showNotification(
        "Performance configuration loaded successfully", 
        type = "message",
        duration = 3
      )
    }
  })
  
  #### UI State Management ####
  
  #' Set up observers for UI disabling/enabling based on locked fields
  observe({
    # Disable selected levels if locked
    if (performance_data$locked_fields$selected_levels) {
      shinyjs::disable("selected_levels")
    } else {
      shinyjs::enable("selected_levels")
    }
  })
  
  observe({
    # Disable scoring method selection if locked
    if (performance_data$locked_fields$scoring_method) {
      shinyjs::disable("scoring_method")
    } else {
      shinyjs::enable("scoring_method")
    }
  })
  
  observe({
    # Disable score range inputs if locked
    if (!is.null(performance_data$locked_fields$score_range) && 
        performance_data$locked_fields$score_range) {
      shinyjs::disable("direct_min")
      shinyjs::disable("direct_max")
    } else {
      shinyjs::enable("direct_min")
      shinyjs::enable("direct_max")
    }
  })
  
  #' Initialize UI with default values from config
  observe({
    # Update selected levels with config defaults
    updateCheckboxGroupInput(
      session,
      "selected_levels",
      selected = performance_data$default_selected_levels
    )
    
    # Update scoring method with config defaults
    updateSelectInput(
      session,
      "scoring_method",
      selected = performance_data$default_scoring_method
    )
    
    # Update scoring justification if provided in config
    if (performance_data$scoring_justification != "") {
      updateTextAreaInput(
        session,
        "scoring_justification",
        value = performance_data$scoring_justification
      )
    }
    
    # Update score range inputs with config defaults
    updateNumericInput(
      session,
      "direct_min",
      value = performance_data$direct_min
    )
    
    updateNumericInput(
      session,
      "direct_max",
      value = performance_data$direct_max
    )
  })
  
  #### Performance Logic ####
  
  #' Filter Criteria Based on Selected Levels
  #'
  #' Filters criteria based on user-selected hierarchy levels from UI
  #'
  #' @return Reactive expression with filtered criteria
  filtered_criteria <- reactive({
    req(input$selected_levels)
    selected_levels <- as.numeric(input$selected_levels)
    criteria_data$df[criteria_data$df$level %in% selected_levels, ]
  })
  
  #' Reactive Performance Matrix
  #'
  #' Creates and maintains performance matrix that respects level filtering
  #'
  #' @return Reactive expression with filtered performance matrix
  filtered_performance_matrix <- reactive({
    req(nrow(filtered_criteria()) > 0)
    req(decision_data$intervention)
    
    # Get options (intervention + comparators)
    options <- c(decision_data$intervention)
    comps <- unlist(strsplit(decision_data$comparators, "\n"))
    comps <- comps[comps != ""]
    if (length(comps) > 0) {
      options <- c(options, comps)
    }
    
    # Get filtered criteria names
    criteria <- filtered_criteria()$name
    
    # If no existing matrix, create a new one with just the filtered criteria
    if (is.null(performance_data$matrix)) {
      mat <- matrix(
        "",
        nrow = length(options),
        ncol = length(criteria) + 1,
        dimnames = list(options, c(criteria, "Data Source"))
      )
      return(mat)
    }
    
    # Get the full existing matrix
    mat <- performance_data$matrix
    
    # Create a new matrix with the filtered criteria
    new_mat <- matrix(
      "",
      nrow = nrow(mat),
      ncol = length(criteria) + 1,
      dimnames = list(rownames(mat), c(criteria, "Data Source"))
    )
    
    # Copy over existing data for criteria that are both in the filtered set and existing matrix
    common_criteria <- intersect(criteria, colnames(mat))
    if (length(common_criteria) > 0) {
      new_mat[, common_criteria] <- mat[rownames(new_mat), common_criteria, drop = FALSE]
    }
    
    # Copy over data source column if it exists
    if ("Data Source" %in% colnames(mat)) {
      new_mat[, "Data Source"] <- mat[rownames(new_mat), "Data Source", drop = FALSE]
    }
    
    new_mat
  })
  
  #### Observers for Scoring Method ####
  
  # Observe scoring method changes
  observeEvent(input$scoring_method, {
    # Only update if not locked
    if (!performance_data$locked_fields$scoring_method) {
      performance_data$scoring_method <- input$scoring_method
    }
  })
  
  observeEvent(input$scoring_justification, {
    performance_data$scoring_justification <- input$scoring_justification
  })
  
  observeEvent(input$direct_min, {
    # Only update if not locked
    if (!performance_data$locked_fields$score_range) {
      performance_data$direct_min <- input$direct_min
    }
  })
  
  observeEvent(input$direct_max, {
    # Only update if not locked
    if (!performance_data$locked_fields$score_range) {
      performance_data$direct_max <- input$direct_max
    }
  })
  
  #### Performance Matrix UI ####
  
  #' Render Performance Matrix
  #'
  #' Displays editable performance matrix
  #'
  #' @return renderDT output
  output$performance_matrix <- renderDT({
    req(nrow(filtered_criteria()) > 0)
    
    # Get the filtered matrix
    mat <- filtered_performance_matrix()
    
    # Convert to data frame with rownames as first column
    df <- as.data.frame(mat)
    df <- cbind(Option = rownames(df), df)
    rownames(df) <- NULL
    
    # Display the matrix
    datatable(
      df,
      editable = TRUE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = '200px', targets = 0),
          list(width = '150px', targets = '_all')
        ),
        server = FALSE
      ),
      class = "stripe hover"
    )
  })
  
  # Save matrix edits
  observeEvent(input$performance_matrix_cell_edit, {
    info <- input$performance_matrix_cell_edit
    mat <- filtered_performance_matrix()
    
    if (info$col > 0) {
      col_name <- colnames(mat)[info$col] 
      row_name <- rownames(mat)[info$row]
      
      mat[info$row, info$col] <- info$value
      
      if (is.null(performance_data$matrix)) {
        performance_data$matrix <- mat
      } else {
        if (!col_name %in% colnames(performance_data$matrix)) {
          new_col <- matrix("", nrow = nrow(performance_data$matrix), ncol = 1,
                            dimnames = list(rownames(performance_data$matrix), col_name))
          performance_data$matrix <- cbind(performance_data$matrix, new_col)
        }
        
        if (!row_name %in% rownames(performance_data$matrix)) {
          new_row <- matrix("", nrow = 1, ncol = ncol(performance_data$matrix),
                            dimnames = list(row_name, colnames(performance_data$matrix)))
          performance_data$matrix <- rbind(performance_data$matrix, new_row)
        }
        
        performance_data$matrix[row_name, col_name] <- info$value
      }
    }
  }) %>% debounce(500)
  
  # Dynamic title for intervention performance
  output$intervention_perf_title <- renderText({
    if (is.null(decision_data$intervention) || decision_data$intervention == "") {
      "Intervention Performance"
    } else {
      paste("Performance:", decision_data$intervention)
    }
  })
  
  #' Configuration Info Display
  #'
  #' Shows configuration information in the UI when config is loaded
  output$performance_config_info <- renderUI({
    if (performance_data$config_loaded) {
      locked_indicators <- list()
      
      if (performance_data$locked_fields$selected_levels) {
        locked_indicators <- c(locked_indicators, 
                               tags$span(class = "label label-info", 
                                         icon("lock"), " Criteria levels locked"))
      }
      
      if (performance_data$locked_fields$scoring_method) {
        locked_indicators <- c(locked_indicators,
                               tags$span(class = "label label-info", 
                                         icon("lock"), " Scoring method locked"))
      }
      
      if (!is.null(performance_data$locked_fields$score_range) && 
          performance_data$locked_fields$score_range) {
        locked_indicators <- c(locked_indicators,
                               tags$span(class = "label label-info", 
                                         icon("lock"), 
                                         paste(" Score range locked (", 
                                               performance_data$direct_min, 
                                               "-", 
                                               performance_data$direct_max, 
                                               ")", sep = "")))
      }
      
      tagList(
        div(class = "alert alert-info",
            icon("cog"),
            strong(" Performance configuration loaded: "),
            performance_data$config_name,
            " v", performance_data$config_version,
            br(),
            icon("user"), " ", performance_data$config_author,
            " | ", icon("calendar"), " ", format(performance_data$config_date, "%Y-%m-%d"),
            if (length(locked_indicators) > 0) {
              tagList(br(), locked_indicators)
            }
        )
      )
    }
  })
  
  #### Initialize Sub-modules ####
  
  # We need to initialize the modules but only activate their outputs conditionally
  direct_module <- reactiveVal(NULL)
  value_function_module <- reactiveVal(NULL)
  ahp_module <- reactiveVal(NULL)
  
  # Create reactive to track active scoring method
  active_scoring_method <- reactive({
    # If locked, always return the default
    if (performance_data$locked_fields$scoring_method) {
      return(performance_data$default_scoring_method)
    }
    # Otherwise return user selection
    input$scoring_method
  })
  
  # Initialize direct rating module when needed
  observe({
    if (active_scoring_method() == "direct") {
      if (is.null(direct_module())) {
        source("performance_score_submodule/performance_direct_rating_module.R", local = TRUE)
        module <- performance_direct_rating_module(
          input, output, session,
          performance_data = performance_data,
          decision_data = decision_data,
          criteria_data = criteria_data,
          filtered_criteria = filtered_criteria,
          is_active = reactive(active_scoring_method() == "direct"),
          config = if (performance_data$config_loaded) config else NULL
        )
        direct_module(module)
      }
    } else {
      direct_module(NULL)
    }
  })
  
  # Initialize value function module when needed
  observe({
    if (active_scoring_method() == "value_function") {
      if (is.null(value_function_module())) {
        source("performance_score_submodule/performance_value_function_module.R", local = TRUE)
        module <- performance_value_function_module(
          input, output, session,
          performance_data = performance_data,
          decision_data = decision_data,
          criteria_data = criteria_data,
          filtered_criteria = filtered_criteria,
          is_active = reactive(active_scoring_method() == "value_function"),
          config = if (performance_data$config_loaded) config else NULL
        )
        value_function_module(module)
      }
    } else {
      value_function_module(NULL)
    }
  })
  
  # Initialize AHP module when needed
  observe({
    if (active_scoring_method() == "ahp") {
      if (is.null(ahp_module())) {
        source("performance_score_submodule/performance_ahp_module.R", local = TRUE)
        module <- performance_ahp_module(
          input, output, session,
          performance_data = performance_data,
          decision_data = decision_data,
          criteria_data = criteria_data,
          filtered_criteria = filtered_criteria,
          is_active = reactive(active_scoring_method() == "ahp"),
          config = if (performance_data$config_loaded) config else NULL
        )
        ahp_module(module)
      }
    } else {
      ahp_module(NULL)
    }
  })
  
  #### Report Data Functions ####
  
  #' Report Performance Data Frame
  #'
  #' Creates performance data frame for reporting based on current method
  #'
  #' @return Reactive data frame with performance scores
  report_performance_df <- reactive({
    # Use filtered criteria for consistency with performance tab
    criteria_to_use <- filtered_criteria()
    if (nrow(criteria_to_use) == 0) {
      return(data.frame())
    }
    
    intervention_name <- ifelse(
      is.null(decision_data$intervention) || decision_data$intervention == "",
      "Intervention",
      decision_data$intervention
    )
    
    comps <- unlist(strsplit(decision_data$comparators, "\n"))
    comps <- comps[comps != ""]
    options <- c(intervention_name, comps)
    
    rows <- list()
    
    for (opt in options) {
      for (i in seq_len(nrow(criteria_to_use))) {
        criterion <- criteria_to_use[i, ]
        cid <- criterion$id
        cname <- criterion$name
        
        if (opt == intervention_name) {
          # Get from saved performance data
          score <- if (!is.null(performance_data$intervention[[as.character(cid)]])) {
            performance_data$intervention[[as.character(cid)]]
          } else {
            0  # Default value if not set
          }
        } else {
          # Get from saved comparator data
          score <- if (!is.null(performance_data$comparators[[opt]]) && 
                       !is.null(performance_data$comparators[[opt]][[as.character(cid)]])) {
            performance_data$comparators[[opt]][[as.character(cid)]]
          } else {
            0  # Default value if not set
          }
        }
        
        rows[[length(rows) + 1]] <- data.frame(
          option = opt,
          criterion = cname,
          score = as.numeric(score),
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(rows) == 0) {
      return(data.frame())
    }
    
    df <- do.call(rbind, rows)
    df$criterion <- factor(df$criterion, levels = criteria_to_use$name)
    df
  })
  
  #### Report UI Functions ####
  
  # Report Performance Matrix
  output$report_performance_matrix <- renderDT({
    req(!is.null(performance_data$matrix))
    
    datatable(
      performance_data$matrix,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      ),
      class = "stripe hover",
      rownames = TRUE
    )
  })
  
  # Report performance justification
  output$report_scoring_justification <- renderText({
    if (is.null(performance_data$scoring_justification) || 
        performance_data$scoring_justification == "") {
      "No scoring justification provided."
    } else {
      paste("Scoring Method:", 
            switch(performance_data$scoring_method,
                   "direct" = "Direct Rating",
                   "ahp" = "Analytical Hierarchy Process (AHP)",
                   "value_function" = "Value Function"),
            "\n\nScore Range:", 
            performance_data$direct_min, "to", performance_data$direct_max,
            "\n\nJustification:\n",
            performance_data$scoring_justification)
    }
  })
  
  # Report Performance Scores UI
  output$report_performance_ui <- renderUI({
    req(performance_data$scoring_method)
    
    method <- performance_data$scoring_method
    
    if (method == "direct") {
      tagList(
        h4("Direct Rating Performance Scores"),
        p(strong("Score Range:"), 
          performance_data$direct_min, "to", performance_data$direct_max),
        DTOutput("report_performance_table"),
        br(),
        h4("Performance Visualization"),
        plotlyOutput("report_performance_bar_chart", height = "400px")
      )
    } else if (method == "ahp") {
      # Check if AHP data exists
      if (is.null(performance_data$ahp_scores_matrix)) {
        return(p("AHP data not calculated yet. Please go to the Performance Assessment tab and calculate AHP scores."))
      }
      
      tagList(
        h4("AHP Performance Scores"),
        verbatimTextOutput("report_ahp_normalized_matrix_output"),
        br(),
        h4("Performance Visualization"),
        plotlyOutput("report_ahp_bar_chart", height = "400px")
      )
    } else if (method == "value_function") {
      # Get value function data if available
      vf_module <- value_function_module()
      
      if (!is.null(vf_module)) {
        tagList(
          h4("Value Functions"),
          DTOutput("report_value_functions_table"),
          br(),
          h4("Transformed Performance Scores"),
          DTOutput("report_value_function_table"),
          br(),
          h4("Performance Visualization"),
          plotlyOutput("report_value_function_bar_chart", height = "400px")
        )
      } else {
        p("Value functions not defined yet. Please go to the Performance Assessment tab and define value functions.")
      }
    }
  })
  
  # Report performance table
  output$report_performance_table <- renderDT({
    req(nrow(filtered_criteria()) > 0)
    
    # Get performance data based on current method
    if (performance_data$scoring_method == "direct") {
      df <- report_performance_df()
    } else if (performance_data$scoring_method == "value_function") {
      # Try to get data from value function module
      vf_module <- value_function_module()
      if (!is.null(vf_module)) {
        df <- vf_module$utilities$get_value_function_df()
      } else {
        return(datatable(data.frame(Message = "Value function module not loaded")))
      }
    } else if (performance_data$scoring_method == "ahp") {
      return(datatable(data.frame(Message = "AHP scores are weights, not performance scores")))
    } else {
      return(datatable(data.frame(Message = "No performance data available")))
    }
    
    if (nrow(df) == 0 || all(df$score == 0)) {
      return(datatable(data.frame(Message = "No performance data available")))
    }
    
    # Pivot to wide format
    df_wide <- tryCatch({
      tidyr::pivot_wider(
        df,
        id_cols = option,
        names_from = criterion,
        values_from = score,
        values_fill = 0
      )
    }, error = function(e) {
      return(data.frame(Message = "Error creating performance table"))
    })
    
    if ("Message" %in% names(df_wide)) {
      return(datatable(df_wide))
    }
    
    # Add total score column
    score_cols <- setdiff(names(df_wide), "option")
    df_wide$total_score <- rowSums(df_wide[, score_cols, drop = FALSE], na.rm = TRUE)
    
    datatable(
      df_wide,
      rownames = FALSE,
      options = list(
        pageLength = 10, 
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      class = "stripe hover"
    ) %>%
      formatRound(columns = c("total_score"), digits = 2) %>%
      formatRound(columns = setdiff(names(df_wide), c("option", "total_score")), digits = 1)
  })
  
  # Report performance bar chart (for direct rating) 
  output$report_performance_bar_chart <- renderPlotly({
    df <- report_performance_df()
    
    if (nrow(df) == 0 || all(df$score == 0)) {
      return(plot_ly() %>% layout(title = "No performance data available"))
    }
    
    # Calculate max y value for proper scaling
    max_y <- df %>%
      group_by(option) %>%
      summarise(total_score = sum(score, na.rm = TRUE)) %>%
      pull(total_score) %>%
      max(na.rm = TRUE)
    
    # Add buffer to max y value
    max_y <- max_y * 1.1
    
    # Use configured score range for y-axis limits
    y_min <- performance_data$direct_min
    y_max <- max(performance_data$direct_max, max_y)
    
    plot_ly(
      data = df,
      x = ~option,
      y = ~score,
      color = ~criterion,
      type = 'bar',
      text = ~paste0(criterion, ": ", score),
      hoverinfo = 'text+y'
    ) %>%
      layout(
        barmode = 'stack',
        title = paste('Performance by Option (Direct Rating: ', 
                      y_min, '-', y_max, ')', sep = ''),
        yaxis = list(
          title = 'Score', 
          range = c(y_min, y_max),
          automargin = TRUE
        ),
        xaxis = list(
          title = 'Option',
          automargin = TRUE
        ),
        margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
      )
  })
  
  # Report value functions table
  output$report_value_functions_table <- renderDT({
    # Check if we're in value function mode
    if (performance_data$scoring_method != "value_function") {
      return(datatable(data.frame(Message = "Value function mode not active")))
    }
    
    # Try to get the module if it exists
    vf_module <- value_function_module()
    
    if (is.null(vf_module)) {
      return(datatable(data.frame(Message = "Value function module not loaded yet")))
    }
    
    # Safely try to get the functions data
    tryCatch({
      # Get the saved functions from the module's data
      if (length(vf_module$data$functions) == 0) {
        return(datatable(data.frame(Message = "No value functions defined")))
      }
      
      # Get filtered criteria
      criteria_to_use <- filtered_criteria()
      if (nrow(criteria_to_use) == 0) {
        return(datatable(data.frame(Message = "No criteria available")))
      }
      
      # Build functions table
      func_data <- lapply(criteria_to_use$id, function(cid) {
        func <- vf_module$data$functions[[as.character(cid)]]
        criterion_name <- criteria_to_use$name[criteria_to_use$id == cid]
        
        if (!is.null(func)) {
          if (func$type == "linear") {
            func_text <- paste0("f(x) = ", func$params$m, "x + ", func$params$b)
          } else {
            func_text <- paste0("f(x) = ", func$params$a, "x² + ", 
                                func$params$b, "x + ", func$params$c)
          }
        } else {
          func_text <- "Not defined"
        }
        
        data.frame(
          Criterion = criterion_name,
          Function = func_text,
          stringsAsFactors = FALSE
        )
      })
      
      datatable(
        do.call(rbind, func_data),
        rownames = FALSE,
        options = list(dom = 't', paging = FALSE)
      )
    }, error = function(e) {
      datatable(data.frame(
        Error = "Error loading value functions",
        Details = e$message
      ))
    })
  })
  
  # Report value function performance table
  output$report_value_function_table <- renderDT({
    # Check if we're in value function mode
    if (performance_data$scoring_method != "value_function") {
      return(datatable(data.frame(Message = "Value function mode not active")))
    }
    
    # Try to get the module if it exists
    vf_module <- value_function_module()
    
    if (is.null(vf_module)) {
      return(datatable(data.frame(Message = "Value function module not loaded yet")))
    }
    
    # Safely try to get the value function data frame
    tryCatch({
      df <- vf_module$utilities$get_value_function_df()
      
      if (nrow(df) == 0) {
        return(datatable(data.frame(Message = "No value function data available")))
      }
      
      # Pivot to wide format
      df_wide <- tryCatch({
        tidyr::pivot_wider(
          df,
          id_cols = option,
          names_from = criterion,
          values_from = score,
          values_fill = 0
        )
      }, error = function(e) {
        return(data.frame(Message = "Error creating value function table"))
      })
      
      if ("Message" %in% names(df_wide)) {
        return(datatable(df_wide))
      }
      
      datatable(
        df_wide,
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE),
        class = "stripe hover"
      )
    }, error = function(e) {
      datatable(data.frame(
        Error = "Error loading value function data",
        Details = e$message
      ))
    })
  })
  
  # Report value function bar chart
  output$report_value_function_bar_chart <- renderPlotly({
    # Check if we're in value function mode
    if (performance_data$scoring_method != "value_function") {
      return(plot_ly() %>% layout(title = "Value function mode not active"))
    }
    
    # Try to get the module if it exists
    vf_module <- value_function_module()
    
    if (is.null(vf_module)) {
      return(plot_ly() %>% layout(title = "Value function module not loaded yet"))
    }
    
    # Safely try to get the value function data frame
    tryCatch({
      df <- vf_module$utilities$get_value_function_df()
      
      if (nrow(df) == 0 || all(df$score == 0)) {
        return(plot_ly() %>% layout(title = "No value function data available"))
      }
      
      plot_ly(
        data = df,
        x = ~option,
        y = ~score,
        color = ~criterion,
        type = 'bar',
        text = ~paste0(criterion, ": ", round(score, 1)),
        hoverinfo = 'text+y'
      ) %>%
        layout(
          barmode = 'stack',
          title = 'Transformed Performance by Option (Value Function)',
          yaxis = list(title = 'Transformed Score'),
          xaxis = list(title = 'Option')
        )
    }, error = function(e) {
      plot_ly() %>% layout(
        title = paste("Error:", e$message),
        annotations = list(
          text = "Error loading value function data",
          xref = "paper",
          yref = "paper",
          showarrow = FALSE
        )
      )
    })
  })
  
  # Report AHP comparison matrix
  output$report_ahp_normalized_matrix_output <- renderPrint({
    # Check if we're in AHP mode
    if (performance_data$scoring_method != "ahp") {
      cat("AHP mode not active\n")
      return(NULL)
    }
    
    # Try to get the module if it exists
    ahp_mod <- ahp_module()
    
    if (is.null(ahp_mod)) {
      cat("AHP module not loaded yet\n")
      return(NULL)
    }
    
    # Safely try to get AHP scores
    tryCatch({
      if (!is.null(performance_data$ahp_scores_matrix)) {
        # Normalize by column (criterion)
        normalized_matrix <- apply(performance_data$ahp_scores_matrix, 2, function(x) {
          round(x / sum(x) * 100, 1)
        })
        
        cat("Normalized Performance Matrix (Percentage by Criterion):\n\n")
        result_df <- data.frame(
          Alternative = rownames(normalized_matrix),
          as.data.frame(normalized_matrix),
          stringsAsFactors = FALSE
        )
        
        # Add row sums for verification (each column should sum to 100)
        col_sums <- colSums(normalized_matrix)
        result_df <- rbind(result_df, c("TOTAL", col_sums))
        
        print(result_df, row.names = FALSE)
      } else {
        cat("No AHP scores calculated yet.\n")
      }
    }, error = function(e) {
      cat("Error loading AHP data:", e$message, "\n")
    })
  })
  
  # Report AHP bar chart
  output$report_ahp_bar_chart <- renderPlotly({
    # Check if we're in AHP mode
    if (performance_data$scoring_method != "ahp") {
      return(plot_ly() %>% layout(title = "AHP mode not active"))
    }
    
    # Try to get the module if it exists
    ahp_mod <- ahp_module()
    
    if (is.null(ahp_mod)) {
      return(plot_ly() %>% layout(title = "AHP module not loaded yet"))
    }
    
    # Safely try to create the chart
    tryCatch({
      if (!is.null(performance_data$ahp_scores_matrix)) {
        df <- as.data.frame(performance_data$ahp_scores_matrix)
        df$option <- rownames(df)
        
        df_long <- tidyr::pivot_longer(df, cols = -option, names_to = "criterion", values_to = "score")
        df_long$criterion <- factor(df_long$criterion, levels = colnames(performance_data$ahp_scores_matrix))
        
        if (nrow(df_long) == 0 || all(df_long$score == 0)) {
          return(plot_ly() %>% layout(title = "No AHP performance data yet"))
        }
        
        # Calculate max y value for proper scaling
        max_y <- df_long %>%
          group_by(option) %>%
          summarise(total_score = sum(score, na.rm = TRUE)) %>%
          pull(total_score) %>%
          max(na.rm = TRUE)
        
        # Add buffer to max y value
        max_y <- max_y * 1.1
        
        plot_ly(
          data = df_long,
          x = ~option,
          y = ~score,
          color = ~criterion,
          type = 'bar',
          text = ~paste0(criterion, ": ", round(score, 1)),
          hoverinfo = 'text+y'
        ) %>%
          layout(
            barmode = 'stack',
            title = 'Performance by Alternative (AHP Scoring)',
            yaxis = list(
              title = 'Score', 
              range = c(0, max_y),
              automargin = TRUE
            ),
            xaxis = list(
              title = 'Alternative',
              automargin = TRUE
            ),
            margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
          )
      } else {
        plot_ly() %>% layout(title = "No AHP scores calculated yet")
      }
    }, error = function(e) {
      plot_ly() %>% layout(
        title = paste("Error:", e$message),
        annotations = list(
          text = "Error loading AHP data",
          xref = "paper",
          yref = "paper",
          showarrow = FALSE
        )
      )
    })
  })
  
  # Utility function for NULL coalescing
  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }
  
  #### Return Module Components ####
  return(
    list(
      data = performance_data,
      utilities = list(
        filtered_criteria = filtered_criteria,
        filtered_performance_matrix = filtered_performance_matrix,
        report_performance_df = report_performance_df,
        active_scoring_method = active_scoring_method
      ),
      ui_outputs = list(
        performance_config_info = renderUI({ output$performance_config_info })
      ),
      direct_rating_module = direct_module,
      ahp_module = ahp_module,
      value_function_module = value_function_module
    )
  )
}