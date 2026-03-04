# ============================================================================
# FILE: weight_submodule/weight_direct_module.R
# ============================================================================
#' Weight Direct Rating Module
#'
#' This module handles direct rating weighting method.
#' It writes directly to the global output objects (no namespace needed)
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param weights_data Reactive values from weight main module
#' @param criteria_data Reactive values from criteria module
#' @param filtered_criteria Reactive expression for filtered criteria
#' @param is_active Reactive expression indicating if this module is active
#' @param config Configuration list with pre-defined settings (optional)
#'
#' @export
weight_direct_module <- function(input, output, session,
                                 weights_data, criteria_data,
                                 filtered_criteria,
                                 is_active = reactive(TRUE),
                                 config = NULL) {
  
  #### Direct Weighting UI ####
  
  # Render direct weights UI
  output$direct_weights_ui <- renderUI({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    criteria <- filtered_criteria()
    inputs <- list()
    
    for (i in 1:nrow(criteria)) {
      criterion_id <- criteria$id[i]
      criterion_name <- criteria$name[i]
      
      current_value <- ifelse(
        !is.null(weights_data$direct$values[[as.character(criterion_id)]]),
        weights_data$direct$values[[as.character(criterion_id)]],
        0
      )
      
      inputs[[i]] <- fluidRow(
        column(
          width = 8,
          p(strong(criterion_name))
        ),
        column(
          width = 4,
          numericInput(
            inputId = paste0("weight_", criterion_id),  # No namespace!
            label = NULL,
            value = current_value,
            min = 0,
            max = weights_data$direct$total_points,
            step = 1,
            width = "100%"
          )
        )
      )
    }
    
    do.call(tagList, inputs)
  })
  
  # Current weight total display
  output$current_weight_total <- renderText({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    criteria <- filtered_criteria()
    total <- 0
    
    for (i in 1:nrow(criteria)) {
      criterion_id <- criteria$id[i]
      input_id <- paste0("weight_", criterion_id)  # No namespace!
      
      if (!is.null(input[[input_id]])) {
        total <- total + input[[input_id]]
      }
    }
    
    paste0(total, " / ", weights_data$direct$total_points)
  })
  
  # Weights results table
  output$weights_results_table <- renderDT({
    req(is_active())
    req(weights_data$direct$calculated_weights)
    
    weights_df <- data.frame(
      Criterion = names(weights_data$direct$calculated_weights),
      Points = sapply(names(weights_data$direct$calculated_weights), function(name) {
        criterion_id <- filtered_criteria()$id[filtered_criteria()$name == name]
        if (length(criterion_id) > 0) {
          weights_data$direct$values[[as.character(criterion_id)]]
        } else {
          0
        }
      }),
      Weight = round(weights_data$direct$calculated_weights, 2),
      Percentage = paste0(round(weights_data$direct$calculated_weights, 1), "%"),
      stringsAsFactors = FALSE
    )
    
    datatable(
      weights_df,
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        pageLength = nrow(weights_df)
      )
    ) %>%
      formatStyle('Weight', fontWeight = 'bold')
  })
  
  # Pie chart
  output$weights_pie_chart <- renderPlotly({
    req(is_active())
    req(weights_data$direct$calculated_weights)
    
    plot_data <- data.frame(
      Criterion = names(weights_data$direct$calculated_weights),
      Weight = weights_data$direct$calculated_weights
    )
    
    colors <- tryCatch({
      RColorBrewer::brewer.pal(nrow(plot_data), "Set3")
    }, error = function(e) {
      NULL
    })
    
    p <- plot_ly(plot_data, labels = ~Criterion, values = ~Weight, type = 'pie',
                 textposition = 'inside', textinfo = 'label+percent',
                 hoverinfo = 'label+value+percent')
    
    if (!is.null(colors)) {
      p <- p %>% layout(marker = list(colors = colors))
    }
    
    p %>% layout(
      title = list(text = "Weight Distribution (Pie Chart)", y = 0.95),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.1),
      margin = list(t = 50, b = 50)
    )
  })
  
  # Bar chart
  output$weights_bar_chart <- renderPlotly({
    req(is_active())
    req(weights_data$direct$calculated_weights)
    
    plot_data <- data.frame(
      Criterion = names(weights_data$direct$calculated_weights),
      Weight = weights_data$direct$calculated_weights
    ) %>%
      arrange(desc(Weight))
    
    plot_ly(plot_data, x = ~Criterion, y = ~Weight, type = 'bar',
            marker = list(color = 'rgba(55, 128, 191, 0.7)',
                          line = list(color = 'rgba(55, 128, 191, 1.0)', width = 1.5)),
            text = ~paste0(round(Weight, 1), "%"), textposition = 'auto') %>%
      layout(
        title = list(text = "Weight Comparison (Bar Chart)", y = 0.95),
        xaxis = list(title = "Criterion", tickangle = -45),
        yaxis = list(title = "Weight (%)", range = c(0, max(plot_data$Weight) * 1.1)),
        margin = list(b = 100, t = 50)
      )
  })
  
  #### Direct Weighting Logic ####
  
  # Update total points
  observeEvent(input$total_points, {
    if (!is.null(input$total_points) && is_active()) {
      weights_data$direct$total_points <- input$total_points
    }
  })
  
  # Calculate weights
  observeEvent(input$calculate_direct_weights, {
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    criteria <- filtered_criteria()
    total_entered <- 0
    weight_values <- list()
    
    for (i in 1:nrow(criteria)) {
      criterion_id <- criteria$id[i]
      criterion_name <- criteria$name[i]
      input_id <- paste0("weight_", criterion_id)  # No namespace!
      
      if (!is.null(input[[input_id]])) {
        value <- input[[input_id]]
        total_entered <- total_entered + value
        weight_values[[as.character(criterion_id)]] <- value
        weights_data$direct$values[[as.character(criterion_id)]] <- value
      }
    }
    
    if (total_entered != weights_data$direct$total_points) {
      showNotification(
        paste("Total points (", total_entered, ") do not match required total (",
              weights_data$direct$total_points, "). Please adjust your weights."),
        type = "error",
        duration = 10
      )
      return()
    }
    
    calculated_weights <- sapply(weight_values, function(x) {
      (x / weights_data$direct$total_points) * 100
    })
    
    weight_names <- sapply(names(calculated_weights), function(id) {
      criteria$name[criteria$id == as.numeric(id)]
    })
    
    if (length(weight_names) == length(calculated_weights)) {
      names(calculated_weights) <- weight_names
    }
    
    weights_data$direct$calculated_weights <- calculated_weights
    
    showNotification("Weights calculated successfully!", type = "message", duration = 3)
  })
  
  # Reset weights
  observeEvent(input$reset_weights, {
    req(is_active())
    
    criteria <- filtered_criteria()
    
    for (i in 1:nrow(criteria)) {
      criterion_id <- criteria$id[i]
      weights_data$direct$values[[as.character(criterion_id)]] <- 0
    }
    
    weights_data$direct$calculated_weights <- NULL
    showNotification("Weights reset to zero.", type = "message", duration = 3)
  })
  
  # Save direct weights
  observeEvent(input$save_direct_weights, {
    req(is_active())
    req(weights_data$direct$calculated_weights)
    
    for (criterion_name in names(weights_data$direct$calculated_weights)) {
      criterion_id <- filtered_criteria()$id[filtered_criteria()$name == criterion_name]
      if (length(criterion_id) > 0) {
        weights_data$weights[[as.character(criterion_id)]] <- 
          weights_data$direct$calculated_weights[criterion_name]
      }
    }
    
    showNotification("Direct weights saved successfully!", type = "message", duration = 3)
  })
  
  #### Return Module Components ####
  return(
    list(
      data = list(
        values = reactive({ weights_data$direct$values }),
        calculated_weights = reactive({ weights_data$direct$calculated_weights }),
        total_points = reactive({ weights_data$direct$total_points })
      )
    )
  )
}