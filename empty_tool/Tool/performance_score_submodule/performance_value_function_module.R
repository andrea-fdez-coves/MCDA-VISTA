#' Performance Value Function Module
#'
#' This module handles value function scoring method for performance assessment.
#' It allows users to define mathematical functions to transform raw performance
#' values into scores.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param performance_data Reactive values from performance main module
#' @param decision_data Reactive values from decision module
#' @param criteria_data Reactive values from criteria module
#' @param filtered_criteria Reactive expression for filtered criteria
#' @param is_active Reactive expression indicating if this module is active
#'
#' @return A list containing:
#' \itemize{
#'   \item{data - Reactive values for value function data}
#'   \item{utilities - Utility functions for data access}
#' }
#'
#' @export
performance_value_function_module <- function(input, output, session,
                                              performance_data, decision_data, 
                                              criteria_data, filtered_criteria,
                                              is_active = reactive(TRUE),
                                              config = NULL) {
  
  # Namespace for this module
  ns <- session$ns
  
  # Initialize local reactive values for value function data
  value_function_data <- reactiveValues(
    functions = list(),
    performance_values = list(intervention = list(), comparators = list()),
    current_criterion = NULL
  )
  
  #### Value Function UI ####
  
  # Dynamic title for intervention value input
  output$intervention_value_title <- renderText({
    req(is_active())
    if (is.null(decision_data$intervention) || decision_data$intervention == "") {
      "Intervention Raw Values"
    } else {
      paste("Raw Values:", decision_data$intervention)
    }
  })
  
  # Update criterion selection dropdown
  observe({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    criteria_choices <- setNames(filtered_criteria()$id, filtered_criteria()$name)
    updateSelectInput(session, "selected_criterion", choices = criteria_choices)
  })
  
  # When criterion is selected, load its function parameters
  observeEvent(input$selected_criterion, {
    req(is_active())
    req(input$selected_criterion)
    value_function_data$current_criterion <- input$selected_criterion
    
    # Load existing function parameters if they exist
    if (!is.null(value_function_data$functions[[input$selected_criterion]])) {
      func <- value_function_data$functions[[input$selected_criterion]]
      
      updateSelectInput(session, "value_function_type", selected = func$type)
      
      if (func$type == "linear") {
        updateNumericInput(session, "linear_m", value = func$params$m)
        updateNumericInput(session, "linear_b", value = func$params$b)
      } else {
        updateNumericInput(session, "square_a", value = func$params$a)
        updateNumericInput(session, "square_b", value = func$params$b)
        updateNumericInput(session, "square_c", value = func$params$c)
      }
    } else {
      # Set defaults for new criterion
      updateSelectInput(session, "value_function_type", selected = "linear")
      updateNumericInput(session, "linear_m", value = 1)
      updateNumericInput(session, "linear_b", value = 0)
      updateNumericInput(session, "square_a", value = 1)
      updateNumericInput(session, "square_b", value = 0)
      updateNumericInput(session, "square_c", value = 0)
    }
  })
  
  # Function preview
  output$function_preview <- renderText({
    req(is_active())
    req(input$selected_criterion)
    
    criterion_name <- filtered_criteria()$name[filtered_criteria()$id == input$selected_criterion]
    
    if (input$value_function_type == "linear") {
      func_text <- paste0("f(x) = ", input$linear_m, "x + ", input$linear_b)
    } else {
      func_text <- paste0("f(x) = ", input$square_a, "x² + ", 
                          input$square_b, "x + ", input$square_c)
    }
    
    paste0("Criterion: ", criterion_name, "\n", func_text)
  })
  
  # Save function for selected criterion
  observeEvent(input$save_function, {
    req(is_active())
    req(input$selected_criterion)
    
    params <- if (input$value_function_type == "linear") {
      list(m = input$linear_m, b = input$linear_b)
    } else {
      list(a = input$square_a, b = input$square_b, c = input$square_c)
    }
    
    value_function_data$functions[[input$selected_criterion]] <- list(
      type = input$value_function_type,
      params = params
    )
    
    # Also update the main performance_data
    performance_data$value_functions[[input$selected_criterion]] <- list(
      type = input$value_function_type,
      params = params
    )
    
    showNotification(paste("Value function saved for criterion", 
                           filtered_criteria()$name[filtered_criteria()$id == input$selected_criterion]),
                     type = "message")
  })
  
  # Apply current function to all criteria
  observeEvent(input$apply_to_all, {
    req(is_active())
    req(input$selected_criterion)
    req(nrow(filtered_criteria()) > 0)
    
    params <- if (input$value_function_type == "linear") {
      list(m = input$linear_m, b = input$linear_b)
    } else {
      list(a = input$square_a, b = input$square_b, c = input$square_c)
    }
    
    # Apply to all criteria
    for (criterion_id in filtered_criteria()$id) {
      value_function_data$functions[[as.character(criterion_id)]] <- list(
        type = input$value_function_type,
        params = params
      )
      
      # Also update the main performance_data
      performance_data$value_functions[[as.character(criterion_id)]] <- list(
        type = input$value_function_type,
        params = params
      )
    }
    
    showNotification("Value function applied to all criteria!", type = "message")
  })
  
  # Display saved functions table
  output$saved_functions_table <- renderDT({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    func_data <- lapply(filtered_criteria()$id, function(cid) {
      func <- value_function_data$functions[[as.character(cid)]]
      criterion_name <- filtered_criteria()$name[filtered_criteria()$id == cid]
      
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
  })
  
  #### Performance Input UI ####
  
  # Intervention value input UI
  output$intervention_value_ui <- renderUI({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    req(decision_data$intervention)
    
    lapply(1:nrow(filtered_criteria()), function(i) {
      criterion <- filtered_criteria()[i, ]
      numericInput(
        inputId = paste0("intervention_value_", criterion$id),
        label = criterion$name,
        value = ifelse(
          is.null(value_function_data$performance_values$intervention[[as.character(criterion$id)]]),
          0,
          value_function_data$performance_values$intervention[[as.character(criterion$id)]]
        ),
        step = 0.1,
        width = "100%"
      )
    })
  })
  
  # Comparators value input UI
  output$comparators_value_ui <- renderUI({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    req(decision_data$comparators)
    
    comps <- unlist(strsplit(decision_data$comparators, "\n"))
    comps <- comps[comps != ""]
    if (length(comps) == 0) return(NULL)
    
    lapply(comps, function(comp) {
      tagList(
        h4(comp),
        lapply(1:nrow(filtered_criteria()), function(i) {
          criterion <- filtered_criteria()[i, ]
          numericInput(
            inputId = paste0("comparator_value_", make.names(comp), "_", criterion$id),
            label = criterion$name,
            value = ifelse(
              is.null(value_function_data$performance_values$comparators[[comp]][[as.character(criterion$id)]]),
              0,
              value_function_data$performance_values$comparators[[comp]][[as.character(criterion$id)]]
            ),
            step = 0.1,
            width = "100%"
          )
        }),
        hr()
      )
    })
  })
  
  #### Value Function Logic ####
  
  #' Calculate Value Function Score
  #'
  #' Applies the value function to transform raw performance values
  #'
  #' @param x Raw performance value
  #' @param criterion_id Criterion ID
  #' @return Transformed score
  calculate_value_score <- function(x, criterion_id) {
    func <- value_function_data$functions[[as.character(criterion_id)]]
    
    if (is.null(func)) {
      return(x) # fallback to raw value if no function defined
    }
    
    if (func$type == "linear") {
      return(func$params$m * x + func$params$b)
    } else if (func$type == "square") {
      return(func$params$a * x^2 + func$params$b * x + func$params$c)
    }
    return(x)
  }
  
  # Save value function performance values
  observeEvent(input$save_value_function, {
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    # Save intervention values
    value_function_data$performance_values$intervention <- list()
    for (i in 1:nrow(filtered_criteria())) {
      criterion_id <- filtered_criteria()$id[i]
      input_id <- paste0("intervention_value_", criterion_id)
      if (!is.null(input[[input_id]])) {
        value_function_data$performance_values$intervention[[as.character(criterion_id)]] <- input[[input_id]]
      }
    }
    
    # Save comparator values
    comps <- unlist(strsplit(decision_data$comparators, "\n"))
    comps <- comps[comps != ""]
    value_function_data$performance_values$comparators <- list()
    
    for (comp in comps) {
      value_function_data$performance_values$comparators[[comp]] <- list()
      for (i in 1:nrow(filtered_criteria())) {
        criterion_id <- filtered_criteria()$id[i]
        input_id <- paste0("comparator_value_", make.names(comp), "_", criterion_id)
        if (!is.null(input[[input_id]])) {
          value_function_data$performance_values$comparators[[comp]][[as.character(criterion_id)]] <- input[[input_id]]
        }
      }
    }
    
    showNotification("Performance values saved successfully!", type = "message")
  })
  
  #### Value Function Data ####
  
  #' Value Function Data Frame
  #'
  #' Creates a data frame with raw and transformed performance scores
  #'
  #' @return Reactive data frame with value function scores
  value_function_df <- reactive({
    req(nrow(filtered_criteria()) > 0)
    
    intervention_name <- ifelse(
      is.null(decision_data$intervention) || decision_data$intervention == "",
      "Intervention",
      decision_data$intervention
    )
    
    rows <- list()
    
    # Intervention rows
    for (i in seq_len(nrow(filtered_criteria()))) {
      cid <- filtered_criteria()$id[i]
      cname <- filtered_criteria()$name[i]
      raw_value <- value_function_data$performance_values$intervention[[as.character(cid)]]
      
      if (is.null(raw_value)) {
        input_id <- paste0("intervention_value_", cid)
        raw_value <- if (!is.null(input[[input_id]])) input[[input_id]] else 0
      }
      
      score <- calculate_value_score(raw_value, cid)
      
      rows[[length(rows) + 1]] <- data.frame(
        option = intervention_name,
        criterion = cname,
        criterion_id = cid,
        raw_value = raw_value,
        score = ifelse(is.na(score), 0, score),
        stringsAsFactors = FALSE
      )
    }
    
    # Comparator rows
    comps <- unlist(strsplit(decision_data$comparators, "\n"))
    comps <- comps[comps != ""]
    
    if (length(comps) > 0) {
      for (comp in comps) {
        for (i in seq_len(nrow(filtered_criteria()))) {
          cid <- filtered_criteria()$id[i]
          cname <- filtered_criteria()$name[i]
          raw_value <- NULL
          
          if (!is.null(value_function_data$performance_values$comparators[[comp]])) {
            raw_value <- value_function_data$performance_values$comparators[[comp]][[as.character(cid)]]
          }
          
          if (is.null(raw_value)) {
            input_id <- paste0("comparator_value_", make.names(comp), "_", cid)
            raw_value <- if (!is.null(input[[input_id]])) input[[input_id]] else 0
          }
          
          score <- calculate_value_score(raw_value, cid)
          
          rows[[length(rows) + 1]] <- data.frame(
            option = comp,
            criterion = cname,
            criterion_id = cid,
            raw_value = raw_value,
            score = ifelse(is.na(score), 0, score),
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    df <- do.call(rbind, rows)
    df$criterion <- factor(df$criterion, levels = filtered_criteria()$name)
    df
  })
  
  #### Visualizations ####
  
  # Value Function Bar Chart (Stacked)
  output$value_function_bar_chart <- renderPlotly({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    df <- value_function_df()
    
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
  })
  
  # Value Function Spider/Radar Chart
  output$value_function_spider_plot <- renderPlotly({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    df <- value_function_df()
    
    if (nrow(df) == 0 || all(df$score == 0)) {
      return(plot_ly() %>% layout(title = "No value function data available"))
    }
    
    # Calculate dynamic range for transformed scores
    max_score <- max(df$score, na.rm = TRUE)
    min_score <- min(df$score, na.rm = TRUE)
    range_padding <- (max_score - min_score) * 0.1
    
    cats <- filtered_criteria()$name
    cats_char <- as.character(cats)
    
    traces <- lapply(unique(df$option), function(opt) {
      sub <- df[df$option == opt, ]
      sub <- sub[match(cats_char, as.character(sub$criterion)), ]
      vals <- as.numeric(sub$score)
      vals_closed <- c(vals, vals[1])
      
      list(
        name = opt,
        r = vals_closed,
        theta = c(cats_char, cats_char[1])
      )
    })
    
    p <- plot_ly(type = 'scatterpolar', fill = 'toself')
    
    for (t in traces) {
      p <- add_trace(
        p,
        r = t$r,
        theta = t$theta,
        name = t$name,
        mode = 'lines+markers',
        fill = 'none',
        line = list(width = 2),
        marker = list(size = 6),
        hoverinfo = 'name+theta+text',
        text = paste0(t$theta, ": ", round(t$r, 1))
      )
    }
    
    p %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(
            max(0, min_score - range_padding), 
            min(ifelse(max_score > 100, max_score * 1.1, 100), max_score + range_padding)
          ),
          tickmode = "auto",
          tickformat = ".1f"
        )
      ),
      showlegend = TRUE,
      title = "Transformed Performance Radar (Value Function)",
      margin = list(t = 50, b = 50)
    )
  })
  
  # Value Function Performance by Criterion (Subplots)
  output$value_function_by_criterion <- renderPlotly({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    df <- value_function_df()
    
    if (nrow(df) == 0 || all(df$score == 0)) {
      return(plot_ly() %>% layout(title = "No value function data available"))
    }
    
    plots <- lapply(unique(df$criterion), function(crit) {
      crit_df <- df[df$criterion == crit, ]
      
      # Calculate dynamic range for this criterion's transformed scores
      max_score <- max(crit_df$score, na.rm = TRUE)
      min_score <- min(crit_df$score, na.rm = TRUE)
      range_padding <- (max_score - min_score) * 0.1
      
      plot_ly(
        crit_df,
        x = ~option,
        y = ~score,
        type = 'bar',
        name = crit,
        text = ~paste(option, "<br>Raw:", round(raw_value, 1), "<br>Transformed:", round(score, 1)),
        hoverinfo = 'text',
        marker = list(line = list(width = 1, color = 'rgb(0,0,0)'))
      ) %>%
        layout(
          title = list(text = paste(crit, "(Value Function)"), font = list(size = 10)),
          xaxis = list(title = ""),
          yaxis = list(
            title = "Transformed Score", 
            range = c(
              max(0, min_score - range_padding), 
              min(ifelse(max_score > 100, max_score * 1.1, 100), max_score + range_padding)
            ),
            autorange = FALSE,
            fixedrange = FALSE
          ),
          showlegend = FALSE
        )
    })
    
    n <- length(plots)
    ncols <- min(3, n)
    nrows <- ceiling(n / ncols)
    
    subplot(
      plots,
      nrows = nrows,
      shareX = FALSE,
      shareY = FALSE,
      titleX = FALSE,
      margin = 0.05
    ) %>%
      layout(
        title = "Transformed Performance by Criterion (Value Function)",
        margin = list(t = 50, b = 100)
      )
  })
  
  # Raw vs Transformed Comparison Chart
  output$value_function_comparison <- renderPlotly({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    df <- value_function_df()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No value function data available"))
    }
    
    # Create a comparison plot
    plot_ly(df, x = ~interaction(option, criterion)) %>%
      add_trace(
        y = ~raw_value, 
        type = 'bar', 
        name = 'Raw Value', 
        marker = list(color = 'rgba(100, 149, 237, 0.6)'),
        text = ~paste("Raw:", round(raw_value, 1)),
        hoverinfo = 'text+y'
      ) %>%
      add_trace(
        y = ~score, 
        type = 'bar', 
        name = 'Transformed',
        marker = list(color = 'rgba(255, 99, 71, 0.6)'),
        text = ~paste("Transformed:", round(score, 1)),
        hoverinfo = 'text+y'
      ) %>%
      layout(
        barmode = 'group',
        title = 'Raw Values vs Transformed Scores (Value Function)',
        yaxis = list(title = 'Value'),
        xaxis = list(
          title = 'Option - Criterion',
          tickangle = 45,
          tickmode = 'array',
          tickvals = ~seq_along(interaction(option, criterion)),
          ticktext = ~paste(option, "-", criterion)
        ),
        showlegend = TRUE,
        margin = list(b = 120)
      )
  })
  
  #### Return Module Components ####
  return(
    list(
      data = value_function_data,
      utilities = list(
        get_value_function_df = value_function_df,
        calculate_value_score = calculate_value_score
      )
    )
  )
}