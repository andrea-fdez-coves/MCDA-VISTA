#' Weight Swing Module
#'
#' This module handles swing weighting method.
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
weight_swing_module <- function(input, output, session,
                                weights_data, criteria_data,
                                filtered_criteria,
                                is_active = reactive(TRUE),
                                config = NULL) {
  
  #### Swing Weighting UI ####
  
  # Update criterion choices for swing weighting
  observeEvent(filtered_criteria(), {
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    criteria <- filtered_criteria()
    criteria_choices <- setNames(criteria$id, criteria$name)
    
    # Check if we have a saved first criterion
    saved_first <- NULL
    if (!is.null(weights_data$swing$first_criterion)) {
      saved_first <- weights_data$swing$first_criterion
    } else {
      saved_first <- criteria_choices[1]
    }
    
    updateSelectInput(
      session,
      "swing_first_criterion",  # No namespace!
      choices = criteria_choices,
      selected = saved_first
    )
  })
  
  # UI for swing weight inputs
  output$swing_weights_ui <- renderUI({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    req(input$swing_first_criterion)
    
    criteria <- filtered_criteria()
    first_criterion_id <- input$swing_first_criterion
    first_criterion_name <- criteria$name[criteria$id == first_criterion_id]
    
    inputs <- list()
    index <- 1
    
    # Header
    inputs[[index]] <- div(
      class = "well",
      h4("Swing Weighting Method"),
      p("Imagine all criteria are at their WORST performance level."),
      p(strong("Which criterion would you improve to its BEST performance level FIRST?"))
    )
    index <- index + 1
    
    inputs[[index]] <- h4("Assign points to other criteria (0-100):")
    index <- index + 1
    
    inputs[[index]] <- p(strong("Reference: "), first_criterion_name, " = 100 points")
    index <- index + 1
    
    inputs[[index]] <- br()
    index <- index + 1
    
    # Inputs for other criteria
    for (i in 1:nrow(criteria)) {
      criterion_id <- criteria$id[i]
      criterion_name <- criteria$name[i]
      
      # Skip the first criterion (already set to 100)
      if (criterion_id == first_criterion_id) next
      
      # Get current value from weights_data
      current_value <- ifelse(
        !is.null(weights_data$swing$values[[as.character(criterion_id)]]),
        weights_data$swing$values[[as.character(criterion_id)]],
        50
      )
      
      inputs[[index]] <- fluidRow(
        column(
          width = 8,
          p(strong(criterion_name))
        ),
        column(
          width = 4,
          numericInput(
            inputId = paste0("swing_weight_", criterion_id),  # No namespace!
            label = NULL,
            value = current_value,
            min = 0,
            max = 100,
            step = 1,
            width = "100%"
          )
        )
      )
      index <- index + 1
      
      # Add description of what the number means
      inputs[[index]] <- div(
        style = "margin-top: -10px; margin-bottom: 15px; margin-left: 15px; color: #666; font-size: 0.9em;",
        p("Compared to improving ", first_criterion_name, 
          " (100 points), how important is it to improve ", criterion_name, "?")
      )
      index <- index + 1
    }
    
    do.call(tagList, inputs)
  })
  
  # Swing weights results table
  output$swing_weights_results_table <- renderDT({
    req(is_active())
    req(weights_data$swing$calculated_weights)
    
    criteria <- filtered_criteria()
    
    results_df <- data.frame(
      Criterion = names(weights_data$swing$calculated_weights),
      Points = sapply(names(weights_data$swing$calculated_weights), function(name) {
        criterion_id <- criteria$id[criteria$name == name]
        if (length(criterion_id) > 0) {
          weights_data$swing$values[[as.character(criterion_id)]]
        } else {
          0
        }
      }),
      Weight = round(weights_data$swing$calculated_weights, 2),
      Percentage = paste0(round(weights_data$swing$calculated_weights, 1), "%"),
      stringsAsFactors = FALSE
    ) %>%
      arrange(desc(Weight))
    
    datatable(
      results_df,
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        pageLength = nrow(results_df)
      )
    ) %>%
      formatStyle('Weight', fontWeight = 'bold')
  })
  
  # Swing weights pie chart
  output$swing_weights_pie_chart <- renderPlotly({
    req(is_active())
    req(weights_data$swing$calculated_weights)
    
    plot_data <- data.frame(
      Criterion = names(weights_data$swing$calculated_weights),
      Weight = weights_data$swing$calculated_weights
    )
    
    # Use RColorBrewer if available
    colors <- tryCatch({
      RColorBrewer::brewer.pal(nrow(plot_data), "Pastel1")
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
      title = list(text = "Swing Weight Distribution", y = 0.95),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.1),
      margin = list(t = 50, b = 50)
    )
  })
  
  # Swing weights bar chart
  output$swing_weights_bar_chart <- renderPlotly({
    req(is_active())
    req(weights_data$swing$calculated_weights)
    
    criteria <- filtered_criteria()
    first_criterion_id <- weights_data$swing$first_criterion
    first_criterion_name <- NULL
    
    if (!is.null(first_criterion_id)) {
      first_criterion_name <- criteria$name[criteria$id == first_criterion_id]
    }
    
    plot_data <- data.frame(
      Criterion = names(weights_data$swing$calculated_weights),
      Weight = weights_data$swing$calculated_weights
    ) %>%
      arrange(desc(Weight))
    
    # Highlight the first criterion
    colors <- if (!is.null(first_criterion_name)) {
      ifelse(plot_data$Criterion == first_criterion_name,
             'rgba(220, 120, 50, 0.8)',  # Highlight color
             'rgba(100, 180, 200, 0.7)') # Normal color
    } else {
      rep('rgba(100, 180, 200, 0.7)', nrow(plot_data))
    }
    
    p <- plot_ly(plot_data, x = ~Criterion, y = ~Weight, type = 'bar',
                 marker = list(color = colors,
                               line = list(color = 'rgba(0, 0, 0, 0.8)', width = 1.5)),
                 text = ~paste0(round(Weight, 1), "%"), textposition = 'auto') %>%
      layout(
        title = list(text = "Swing Weight Comparison", y = 0.95),
        xaxis = list(title = "Criterion", tickangle = -45),
        yaxis = list(title = "Weight (%)", range = c(0, max(plot_data$Weight) * 1.1)),
        margin = list(b = 100, t = 50)
      )
    
    # Add annotation for first criterion
    if (!is.null(first_criterion_name)) {
      p <- p %>% layout(
        annotations = list(
          list(
            x = which(plot_data$Criterion == first_criterion_name) - 1,
            y = plot_data$Weight[plot_data$Criterion == first_criterion_name],
            text = "First choice",
            showarrow = TRUE,
            arrowhead = 4,
            arrowsize = 0.7,
            ax = 0,
            ay = -40
          )
        )
      )
    }
    
    p
  })
  
  #### Swing Weighting Logic ####
  
  # Track first criterion selection
  observeEvent(input$swing_first_criterion, {
    req(is_active())
    if (!is.null(input$swing_first_criterion)) {
      weights_data$swing$first_criterion <- input$swing_first_criterion
    }
  })
  
  # Calculate swing weights
  observeEvent(input$calculate_swing_weights, {
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    req(input$swing_first_criterion)
    
    criteria <- filtered_criteria()
    first_criterion_id <- input$swing_first_criterion
    weight_values <- list()
    
    # Set first criterion to 100
    weight_values[[as.character(first_criterion_id)]] <- 100
    weights_data$swing$values[[as.character(first_criterion_id)]] <- 100
    
    # Get weights for other criteria
    for (i in 1:nrow(criteria)) {
      criterion_id <- criteria$id[i]
      
      # Skip the first criterion
      if (criterion_id == first_criterion_id) next
      
      input_id <- paste0("swing_weight_", criterion_id)  # No namespace!
      
      if (!is.null(input[[input_id]])) {
        value <- input[[input_id]]
        weight_values[[as.character(criterion_id)]] <- value
        weights_data$swing$values[[as.character(criterion_id)]] <- value
      }
    }
    
    # Check if any weights are zero (should at least have 100 for first)
    if (length(weight_values) == 1) {
      # Only first criterion has weights, others default to 0
      # This is okay - they just don't have assigned weights yet
    }
    
    # Convert to percentages
    total_points <- sum(unlist(weight_values))
    
    if (total_points <= 0) {
      showNotification("Total points must be greater than zero", type = "error", duration = 5)
      return()
    }
    
    calculated_weights <- lapply(weight_values, function(x) {
      (x / total_points) * 100
    })
    
    # Convert to named vector with criterion names
    weight_names <- sapply(names(calculated_weights), function(id) {
      criteria$name[criteria$id == as.numeric(id)]
    })
    
    calculated_weights_vec <- unlist(calculated_weights)
    
    if (length(weight_names) == length(calculated_weights_vec)) {
      names(calculated_weights_vec) <- weight_names
    }
    
    weights_data$swing$calculated_weights <- calculated_weights_vec
    weights_data$swing$first_criterion <- first_criterion_id
    
    showNotification("Swing weights calculated successfully!", type = "message", duration = 3)
  })
  
  # Reset swing weights
  observeEvent(input$reset_swing_weights, {
    req(is_active())
    
    criteria <- filtered_criteria()
    first_criterion_id <- input$swing_first_criterion
    
    for (i in 1:nrow(criteria)) {
      criterion_id <- criteria$id[i]
      
      if (criterion_id == first_criterion_id) {
        weights_data$swing$values[[as.character(criterion_id)]] <- 100
      } else {
        weights_data$swing$values[[as.character(criterion_id)]] <- 50
      }
    }
    
    weights_data$swing$calculated_weights <- NULL
    showNotification("Swing weights reset to default values.", type = "message", duration = 3)
  })
  
  # Save swing weights
  observeEvent(input$save_swing_weights, {
    req(is_active())
    req(weights_data$swing$calculated_weights)
    
    # Update main weights_data with swing weights
    for (criterion_name in names(weights_data$swing$calculated_weights)) {
      criterion_id <- filtered_criteria()$id[filtered_criteria()$name == criterion_name]
      if (length(criterion_id) > 0) {
        weights_data$weights[[as.character(criterion_id)]] <- 
          weights_data$swing$calculated_weights[criterion_name]
      }
    }
    
    showNotification("Swing weights saved successfully!", type = "message", duration = 3)
  })
  
  #### Return Module Components ####
  return(
    list(
      data = list(
        values = reactive({ weights_data$swing$values }),
        calculated_weights = reactive({ weights_data$swing$calculated_weights }),
        first_criterion = reactive({ weights_data$swing$first_criterion })
      )
    )
  )
}