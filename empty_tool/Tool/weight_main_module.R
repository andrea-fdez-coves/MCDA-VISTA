#' Weight Main Module
#'
#' This module coordinates the weighting functionality, including
#' weighting method selection and calling specific weighting modules.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param decision_data Reactive values from decision module
#' @param criteria_data Reactive values from criteria module
#' @param performance_data Reactive values from performance module
#' @param config Configuration list with pre-defined settings (optional)
#'
#' @return A list containing:
#' \itemize{
#'   \item{data - Reactive values for weight data}
#'   \item{utilities - Utility functions for filtering and data access}
#' }
#'
#' @export
weight_main_module <- function(input, output, session,
                               decision_data, criteria_data,
                               performance_data = NULL,
                               config = NULL) {
  
  # Initialize reactive values for weights
  weights_data <- reactiveValues(
    method = "direct",
    justification = "",
    weights = list(),
    direct = list(
      values = list(),
      total_points = 100,
      calculated_weights = NULL
    ),
    ahp = list(
      matrix = NULL,
      scores = NULL,
      consistency = NULL,
      comparisons = list()
    ),
    swing = list(
      values = list(),
      first_criterion = NULL,
      calculated_weights = NULL
    ),
    config_loaded = FALSE,
    config_name = "",
    config_description = "",
    config_version = "",
    config_author = "",
    config_date = "",
    locked_fields = list(
      weighting_method = FALSE,
      weighting_justification_required = FALSE,
      total_points = FALSE,
      swing_first_criterion = FALSE
    ),
    default_selected_levels = c(1, 2, 3, 4),
    default_weighting_method = "direct"
  )
  
  #' Load Configuration
  #'
  #' Loads pre-defined settings from configuration if provided
  observe({
    req(!is.null(config))
    
    # Store configuration info
    weights_data$config_loaded <- TRUE
    weights_data$config_name <- config$config_name %||% "Unknown"
    weights_data$config_description <- config$config_description %||% ""
    weights_data$config_version <- config$config_version %||% ""
    weights_data$config_author <- config$config_author %||% ""
    weights_data$config_date <- config$config_date %||% Sys.Date()
    
    # Load weight-specific settings from config
    if (!is.null(config$weights)) {
      
      # Set default weighting method
      if (!is.null(config$weights$default_weighting_method)) {
        weights_data$default_weighting_method <- config$weights$default_weighting_method
        weights_data$method <- config$weights$default_weighting_method
      }
      
      # Set default weighting justification (optional)
      if (!is.null(config$weights$default_weighting_justification)) {
        weights_data$justification <- config$weights$default_weighting_justification
      }
      
      # Set total points for direct weighting
      if (!is.null(config$weights$direct$total_points)) {
        weights_data$direct$total_points <- config$weights$direct$total_points
      }
      
      # ONLY load pre-defined weights if NO weights have been saved yet
      # This prevents overwriting user's saved weights when config reloads
      if (length(weights_data$weights) == 0) {
        
        # Load direct weights if provided in config
        if (!is.null(config$weights$direct$values)) {
          weights_data$direct$values <- config$weights$direct$values
          
          # Also load the calculated weights if they exist
          if (!is.null(config$weights$direct$calculated_weights)) {
            weights_data$direct$calculated_weights <- config$weights$direct$calculated_weights
            
            # Update the main weights_data with these initial weights
            for (criterion_name in names(weights_data$direct$calculated_weights)) {
              criterion_id <- filtered_criteria()$id[filtered_criteria()$name == criterion_name]
              if (length(criterion_id) > 0) {
                weights_data$weights[[as.character(criterion_id)]] <- 
                  weights_data$direct$calculated_weights[criterion_name]
              }
            }
          }
        }
        
        # Load AHP weights if provided in config
        if (!is.null(config$weights$ahp$scores)) {
          weights_data$ahp$scores <- config$weights$ahp$scores
          weights_data$ahp$matrix <- config$weights$ahp$matrix
          weights_data$ahp$consistency <- config$weights$ahp$consistency
        }
        
        # Load swing weights if provided in config
        if (!is.null(config$weights$swing$calculated_weights)) {
          weights_data$swing$calculated_weights <- config$weights$swing$calculated_weights
          weights_data$swing$first_criterion <- config$weights$swing$first_criterion
          weights_data$swing$values <- config$weights$swing$values
        }
        
      } else {
        # User has already saved weights - don't overwrite them
        message("User weights already exist - skipping config weight initialization")
      }
      
      # Set locked fields (these should always be applied regardless of saved weights)
      if (!is.null(config$weights$locked_fields)) {
        for (field in names(config$weights$locked_fields)) {
          weights_data$locked_fields[[field]] <- config$weights$locked_fields[[field]]
        }
      }
      
      # For backward compatibility
      if (!is.null(config$locked_fields)) {
        if (!is.null(config$locked_fields$weighting_method)) {
          weights_data$locked_fields$weighting_method <- config$locked_fields$weighting_method
        }
      }
      
      # Log configuration loaded - use console instead of notification
      message("Weight configuration loaded successfully: ", config$config_name)
      if (length(weights_data$weights) > 0) {
        message("Preserved ", length(weights_data$weights), " existing user weights")
      }
    }
  })
  
  #### UI State Management ####
  
  #' Set up observers for UI disabling/enabling based on locked fields
  observe({
    if (weights_data$locked_fields$weighting_method) {
      shinyjs::disable("weighting_method")
    } else {
      shinyjs::enable("weighting_method")
    }
  })
  
  #' Initialize UI with default values from config
  observe({
    # Update weighting method with config defaults
    updateSelectInput(
      session,
      "weighting_method",
      selected = weights_data$default_weighting_method
    )
    
    # Update weighting justification if provided in config
    if (weights_data$justification != "") {
      updateTextAreaInput(
        session,
        "weighting_justification",
        value = weights_data$justification
      )
    }
    
    # Update total points if provided in config
    updateNumericInput(
      session,
      "total_points",
      value = weights_data$direct$total_points
    )
  })
  
  #### Weight Logic ####
  
  #' Filter Criteria Based on Selected Levels
  #'
  #' Filters criteria based on user-selected hierarchy levels from performance tab
  #' or uses all criteria if performance module not available
  #'
  #' @return Reactive expression with filtered criteria
  filtered_criteria <- reactive({
    if (!is.null(performance_data) && !is.null(performance_data$utilities)) {
      # Use performance module's filtered criteria if available
      tryCatch({
        performance_data$utilities$filtered_criteria()
      }, error = function(e) {
        criteria_data$df
      })
    } else {
      # Otherwise use all criteria
      criteria_data$df
    }
  })
  
  # Observe weighting method changes
  observeEvent(input$weighting_method, {
    if (!weights_data$locked_fields$weighting_method) {
      weights_data$method <- input$weighting_method
    }
  })
  
  observeEvent(input$weighting_justification, {
    weights_data$justification <- input$weighting_justification
  })
  
  #### Initialize Sub-modules ####
  
  # Initialize sub-modules
  direct_weight_module <- reactiveVal(NULL)
  ahp_weight_module <- reactiveVal(NULL)
  swing_weight_module <- reactiveVal(NULL)
  
  # Create reactive to track active weighting method
  active_weighting_method <- reactive({
    if (weights_data$locked_fields$weighting_method) {
      return(weights_data$default_weighting_method)
    }
    input$weighting_method
  })
  
  # Initialize direct weighting module when needed
  observe({
    if (active_weighting_method() == "direct") {
      if (is.null(direct_weight_module())) {
        source("weight_submodule/weight_direct_module.R", local = TRUE)
        module <- weight_direct_module(
          input, output, session,
          weights_data = weights_data,
          criteria_data = criteria_data,
          filtered_criteria = filtered_criteria,
          is_active = reactive(active_weighting_method() == "direct"),
          config = if (weights_data$config_loaded) config else NULL
        )
        direct_weight_module(module)
      }
    }
  })
  
  # Initialize AHP weighting module when needed
  observe({
    if (active_weighting_method() == "ahp") {
      if (is.null(ahp_weight_module())) {
        source("weight_submodule/weight_ahp_module.R", local = TRUE)
        module <- weight_ahp_module(
          input, output, session,
          weights_data = weights_data,
          criteria_data = criteria_data,
          filtered_criteria = filtered_criteria,
          is_active = reactive(active_weighting_method() == "ahp"),
          config = if (weights_data$config_loaded) config else NULL
        )
        ahp_weight_module(module)
      }
    }
  })
  
  # Initialize swing weighting module when needed
  observe({
    if (active_weighting_method() == "swing") {
      if (is.null(swing_weight_module())) {
        source("weight_submodule/weight_swing_module.R", local = TRUE)
        module <- weight_swing_module(
          input, output, session,
          weights_data = weights_data,
          criteria_data = criteria_data,
          filtered_criteria = filtered_criteria,
          is_active = reactive(active_weighting_method() == "swing"),
          config = if (weights_data$config_loaded) config else NULL
        )
        swing_weight_module(module)
      }
    }
  })
  
  
  #### Report Output Functions  ####
  # These write directly to the global report outputs
  
  # Report: Weighting Method Used
  output$report_weighting_method <- renderPrint({
    req(weights_data$method)
    
    method_name <- switch(weights_data$method,
                          "direct" = "Direct Rating",
                          "ahp" = "Analytical Hierarchy Process (AHP)",
                          "swing" = "Swing Weighting",
                          "Unknown Method")
    
    cat(method_name)
  })
  
  # Report: Method Justification
  output$report_weighting_justification <- renderPrint({
    if (is.null(weights_data$justification) || weights_data$justification == "") {
      cat("No justification provided.")
    } else {
      cat(weights_data$justification)
    }
  })
  
  # Report: Weight Summary Table
  output$report_weights_table <- renderDT({
    req(length(weights_data$weights) > 0)
    req(nrow(filtered_criteria()) > 0)
    
    criteria <- filtered_criteria()
    weights_df <- data.frame(
      Criterion = character(),
      Weight = numeric(),
      Percentage = character(),
      stringsAsFactors = FALSE
    )
    
    # Get weights for all criteria
    for (i in 1:nrow(criteria)) {
      criterion_id <- as.character(criteria$id[i])
      criterion_name <- criteria$name[i]
      
      weight_value <- ifelse(!is.null(weights_data$weights[[criterion_id]]),
                             weights_data$weights[[criterion_id]],
                             0)
      
      weights_df <- rbind(weights_df, data.frame(
        Criterion = criterion_name,
        Weight = round(weight_value, 2),
        Percentage = paste0(round(weight_value, 1), "%")
      ))
    }
    
    # Sort by weight descending
    weights_df <- weights_df[order(-weights_df$Weight), ]
    
    # Add rank
    weights_df$Rank <- rank(-weights_df$Weight, ties.method = "min")
    
    datatable(
      weights_df[, c("Rank", "Criterion", "Weight", "Percentage")],
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        pageLength = nrow(weights_df)
      )
    ) %>%
      formatStyle('Weight', fontWeight = 'bold') %>%
      formatStyle('Rank', fontWeight = 'bold')
  })
  
  # Report: Weight Distribution Pie Chart
  output$report_weights_pie_chart <- renderPlotly({
    req(length(weights_data$weights) > 0)
    req(nrow(filtered_criteria()) > 0)
    
    criteria <- filtered_criteria()
    weights_list <- list()
    
    # Get weights for all criteria
    for (i in 1:nrow(criteria)) {
      criterion_id <- as.character(criteria$id[i])
      criterion_name <- criteria$name[i]
      
      weight_value <- ifelse(!is.null(weights_data$weights[[criterion_id]]),
                             weights_data$weights[[criterion_id]],
                             0)
      
      weights_list[[criterion_name]] <- weight_value
    }
    
    plot_data <- data.frame(
      Criterion = names(weights_list),
      Weight = unlist(weights_list)
    )
    
    # Remove zero weights for pie chart
    plot_data <- plot_data[plot_data$Weight > 0, ]
    
    if (nrow(plot_data) == 0) {
      return(plot_ly() %>% layout(title = "No weights assigned yet"))
    }
    
    # Use different color schemes based on method
    method_colors <- switch(weights_data$method,
                            "direct" = "Set3",
                            "ahp" = "Pastel1",
                            "swing" = "Set1",
                            "Set3")
    
    colors <- tryCatch({
      RColorBrewer::brewer.pal(nrow(plot_data), method_colors)
    }, error = function(e) {
      NULL
    })
    
    p <- plot_ly(plot_data, labels = ~Criterion, values = ~Weight, type = 'pie',
                 textposition = 'inside', textinfo = 'label+percent',
                 hoverinfo = 'label+value+percent',
                 hole = 0.3)
    
    if (!is.null(colors)) {
      p <- p %>% layout(marker = list(colors = colors))
    }
    
    method_display <- switch(weights_data$method,
                             "direct" = "Direct Rating",
                             "ahp" = "AHP",
                             "swing" = "Swing Weighting",
                             "Unknown")
    
    p %>% layout(
      title = list(text = paste("Weight Distribution -", method_display), y = 0.95),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.1),
      margin = list(t = 50, b = 50)
    )
  })
  
  # Report: Weight Comparison Bar Chart
  output$report_weights_bar_chart <- renderPlotly({
    req(length(weights_data$weights) > 0)
    req(nrow(filtered_criteria()) > 0)
    
    criteria <- filtered_criteria()
    weights_list <- list()
    
    # Get weights for all criteria
    for (i in 1:nrow(criteria)) {
      criterion_id <- as.character(criteria$id[i])
      criterion_name <- criteria$name[i]
      
      weight_value <- ifelse(!is.null(weights_data$weights[[criterion_id]]),
                             weights_data$weights[[criterion_id]],
                             0)
      
      weights_list[[criterion_name]] <- weight_value
    }
    
    plot_data <- data.frame(
      Criterion = names(weights_list),
      Weight = unlist(weights_list)
    ) %>%
      arrange(desc(Weight))
    
    # Use different colors based on method
    bar_color <- switch(weights_data$method,
                        "direct" = 'rgba(55, 128, 191, 0.7)',
                        "ahp" = 'rgba(191, 128, 55, 0.7)',
                        "swing" = 'rgba(128, 191, 55, 0.7)',
                        'rgba(55, 128, 191, 0.7)')
    
    plot_ly(plot_data, x = ~Criterion, y = ~Weight, type = 'bar',
            marker = list(color = bar_color,
                          line = list(color = 'rgba(0, 0, 0, 0.8)', width = 1.5)),
            text = ~paste0(round(Weight, 1), "%"), textposition = 'auto') %>%
      layout(
        title = list(text = "Weight Comparison by Criterion", y = 0.95),
        xaxis = list(title = "Criterion", tickangle = -45),
        yaxis = list(title = "Weight (%)", range = c(0, max(plot_data$Weight) * 1.1)),
        margin = list(b = 100, t = 50)
      )
  })
  
  # Report: Consistency Analysis (for AHP)
  output$report_weights_consistency <- renderUI({
    req(weights_data$method)
    
    if (weights_data$method == "ahp" && !is.null(weights_data$ahp$consistency)) {
      cons <- weights_data$ahp$consistency
      n <- length(weights_data$ahp$scores)
      
      RI_VALUES <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)
      
      tagList(
        h5("Consistency Analysis"),
        p(strong("Consistency Ratio (CR):"), round(cons$CR, 3)),
        p(strong("Consistency Index (CI):"), round(cons$CI, 3)),
        p(strong("Random Index (RI):"), RI_VALUES[n]),
        p(strong("Maximum Eigenvalue (λmax):"), round(cons$lambda_max, 3)),
        br(),
        if (cons$CR < 0.1) {
          p(style = "color: green;", icon("check-circle"), " CR < 0.1: Comparisons are consistent")
        } else {
          p(style = "color: red;", icon("exclamation-triangle"), 
            " CR ≥ 0.1: Comparisons are inconsistent - review recommended")
        }
      )
    } else if (weights_data$method == "direct") {
      p("Direct rating method does not require consistency checking.")
    } else if (weights_data$method == "swing") {
      p("Swing weighting method does not require formal consistency checking.")
    } else {
      p("No consistency information available.")
    }
  })
  
  # Report: Weights Notes Output (for the notes section)
  output$weights_notes_output <- renderPrint({
    req(weights_data$method)
    
    method_text <- switch(weights_data$method,
                          "direct" = "Weighting Method: Direct Rating\n",
                          "ahp" = "Weighting Method: Analytical Hierarchy Process (AHP)\n",
                          "swing" = "Weighting Method: Swing Weighting\n",
                          "Weighting Method: Unknown\n")
    
    justification_text <- ifelse(!is.null(weights_data$justification) && 
                                   weights_data$justification != "",
                                 paste("Method Justification:", weights_data$justification, "\n\n"),
                                 "No justification provided.\n\n")
    
    # Add AHP consistency info
    consistency_text <- ""
    if (weights_data$method == "ahp" && !is.null(weights_data$ahp$consistency)) {
      cons <- weights_data$ahp$consistency
      consistency_text <- paste(
        "Consistency Analysis:\n",
        sprintf("  CR: %.3f (%s)\n", cons$CR, ifelse(cons$CR < 0.1, "Consistent", "Inconsistent")),
        sprintf("  CI: %.3f\n", cons$CI),
        sprintf("  λmax: %.3f\n", cons$lambda_max)
      )
    }
    
    # Add swing info
    swing_text <- ""
    if (weights_data$method == "swing" && !is.null(weights_data$swing$first_criterion)) {
      criteria <- filtered_criteria()
      first_criterion_name <- criteria$name[criteria$id == weights_data$swing$first_criterion]
      swing_text <- paste(
        "Swing Weighting Details:\n",
        sprintf("  First improved criterion: %s (100 points)\n", first_criterion_name)
      )
    }
    
    cat(method_text, justification_text, consistency_text, swing_text, sep = "")
  })
  
  #### Weight Summary Functions ####
  
  #' Get Current Weights as Data Frame
  weights_df <- reactive({
    req(nrow(filtered_criteria()) > 0)
    
    criteria <- filtered_criteria()
    result <- data.frame(
      id = criteria$id,
      criterion = criteria$name,
      level = criteria$level,
      weight = 0,
      percentage = "0%",
      stringsAsFactors = FALSE
    )
    
    if (length(weights_data$weights) > 0) {
      for (i in 1:nrow(result)) {
        cid <- as.character(result$id[i])
        if (!is.null(weights_data$weights[[cid]])) {
          result$weight[i] <- weights_data$weights[[cid]]
          result$percentage[i] <- paste0(round(weights_data$weights[[cid]], 1), "%")
        }
      }
    }
    
    result
  })
  
  #' Get Weights for Specific Method
  current_method_weights <- reactive({
    method <- active_weighting_method()
    
    if (method == "direct") {
      if (!is.null(weights_data$direct$calculated_weights)) {
        return(weights_data$direct$calculated_weights)
      }
    } else if (method == "ahp") {
      if (!is.null(weights_data$ahp$scores)) {
        return(weights_data$ahp$scores * 100)
      }
    } else if (method == "swing") {
      if (!is.null(weights_data$swing$calculated_weights)) {
        return(weights_data$swing$calculated_weights)
      }
    }
    
    return(NULL)
  })
  
  weights_calculated <- reactive({
    !is.null(current_method_weights())
  })
  
  #' Get Weights for Report
  #'
  #' Returns formatted weights data for the report tab
  #'
  #' @return List of report-ready weight data
  get_report_data <- reactive({
    list(
      method = weights_data$method,
      method_name = switch(weights_data$method,
                           "direct" = "Direct Rating",
                           "ahp" = "Analytical Hierarchy Process (AHP)",
                           "swing" = "Swing Weighting",
                           "Unknown"
      ),
      justification = weights_data$justification %||% "",
      weights = weights_data$weights,
      weights_df = weights_df(),
      current_weights = current_method_weights(),
      ahp_consistency = weights_data$ahp$consistency,
      has_weights = length(weights_data$weights) > 0
    )
  })
  
  # Utility function for NULL coalescing
  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }
  
  #### Return Module Components ####
  return(
    list(
      data = weights_data,
      utilities = list(
        filtered_criteria = filtered_criteria,
        weights_df = weights_df,
        current_method_weights = current_method_weights,
        weights_calculated = weights_calculated,
        active_weighting_method = active_weighting_method,
        get_report_data = get_report_data  # <-- ADD THIS LINE
      ),
      modules = list(
        direct = direct_weight_module,
        ahp = ahp_weight_module,
        swing = swing_weight_module
      )
    )
  )
}