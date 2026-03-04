#' Aggregate Score Main Module
#'
#' This module coordinates the aggregate score calculation, combining
#' performance scores with weights to generate final rankings and visualizations.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param decision_data Reactive values from decision module
#' @param criteria_data Reactive values from criteria module
#' @param performance_data Reactive values from performance module
#' @param performance_module The complete performance module object (for accessing utilities)
#' @param weights_data Reactive values from weights module
#' @param config Configuration list with pre-defined settings (optional)
#'
#' @return A list containing:
#' \itemize{
#'   \item{data - Reactive values for aggregate data}
#'   \item{utilities - Utility functions for accessing aggregate results}
#'   \item{aggregate_scores - Reactive data frame with aggregate scores}
#'   \item{detailed_breakdown - Reactive data frame with detailed breakdown}
#' }
#'
#' @export
aggregate_main_module <- function(input, output, session,
                                  decision_data,
                                  criteria_data,
                                  performance_data,
                                  performance_module,
                                  weights_data,
                                  config = NULL) {
  
  # Initialize reactive values for aggregate data
  aggregate_data <- reactiveValues(
    notes = "",
    config_loaded = FALSE,
    config_name = "",
    config_version = "",
    config_author = "",
    config_date = ""
  )
  
  #### Configuration Loading ####
  
  observe({
    req(!is.null(config))
    
    aggregate_data$config_loaded <- TRUE
    aggregate_data$config_name <- config$config_name %||% "Unknown"
    aggregate_data$config_version <- config$config_version %||% ""
    aggregate_data$config_author <- config$config_author %||% ""
    aggregate_data$config_date <- config$config_date %||% Sys.Date()
    
    # Load aggregate-specific settings from config
    if (!is.null(config$aggregate)) {
      if (!is.null(config$aggregate$notes)) {
        aggregate_data$notes <- config$aggregate$notes
      }
    }
    
    message("Aggregate configuration loaded successfully: ", config$config_name)
  })
  
  #### Filtered Criteria ####
  
  #' Get filtered criteria from performance module or criteria data
  filtered_criteria <- reactive({
    if (!is.null(performance_module) && !is.null(performance_module$utilities)) {
      tryCatch({
        performance_module$utilities$filtered_criteria()
      }, error = function(e) {
        criteria_data$df
      })
    } else {
      criteria_data$df
    }
  })
  
  #### Performance Data Extraction ####
  
  #' Get performance data frame based on current scoring method
  get_performance_df <- reactive({
    req(nrow(filtered_criteria()) > 0)
    req(performance_data$scoring_method)
    
    method <- performance_data$scoring_method
    
    if (method == "direct") {
      # Get from performance module's report function
      if (!is.null(performance_module$utilities$report_performance_df)) {
        return(performance_module$utilities$report_performance_df())
      } else {
        return(data.frame())
      }
      
    } else if (method == "value_function") {
      # Get from value function module
      if (!is.null(performance_module$value_function_module)) {
        vf_module <- performance_module$value_function_module()
        if (!is.null(vf_module) && !is.null(vf_module$utilities$get_value_function_df)) {
          return(vf_module$utilities$get_value_function_df())
        }
      }
      return(data.frame())
      
    } else if (method == "ahp") {
      # Handle AHP matrix format
      req(performance_data$ahp_scores_matrix)
      ahp_matrix <- performance_data$ahp_scores_matrix
      
      perf_df <- data.frame(
        option = rownames(ahp_matrix),
        ahp_matrix,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ) %>%
        tidyr::pivot_longer(
          cols = -option,
          names_to = "criterion",
          values_to = "score"
        )
      
      return(perf_df)
      
    } else {
      return(data.frame())
    }
  })
  
  #### Weight Data Extraction ####
  
  #' Get weights data frame
  get_weights_df <- reactive({
    req(nrow(filtered_criteria()) > 0)
    req(length(weights_data$weights) > 0)
    
    criteria <- filtered_criteria()
    
    weights_df <- data.frame(
      criterion = criteria$name,
      weight = sapply(criteria$id, function(id) {
        ifelse(!is.null(weights_data$weights[[as.character(id)]]),
               weights_data$weights[[as.character(id)]] / 100,  # Convert to decimal
               0)
      }),
      stringsAsFactors = FALSE
    )
    
    return(weights_df)
  })
  
  #### Aggregate Score Calculation ####
  
  #' Calculate aggregate scores
  aggregate_scores <- reactive({
    req(nrow(filtered_criteria()) > 0)
    req(length(weights_data$weights) > 0)
    
    # Get performance data
    perf_df <- get_performance_df()
    if (nrow(perf_df) == 0) return(data.frame())
    
    # Get weights
    weights_df <- get_weights_df()
    if (nrow(weights_df) == 0) return(data.frame())
    
    # Check if required columns exist
    if (!all(c("option", "criterion", "score") %in% colnames(perf_df))) {
      return(data.frame())
    }
    
    # Calculate aggregate scores
    aggregate_results <- perf_df %>%
      dplyr::left_join(weights_df, by = "criterion") %>%
      dplyr::group_by(option) %>%
      dplyr::summarise(
        aggregate_score = sum(score * weight, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::arrange(dplyr::desc(aggregate_score)) %>%
      dplyr::mutate(
        percentage = paste0(round(aggregate_score / sum(aggregate_score) * 100, 1), "%"),
        rank = rank(-aggregate_score, ties.method = "min")
      )
    
    return(aggregate_results)
  })
  
  #' Calculate detailed breakdown
  detailed_breakdown <- reactive({
    req(nrow(filtered_criteria()) > 0)
    req(length(weights_data$weights) > 0)
    
    # Get performance data
    perf_df <- get_performance_df()
    if (nrow(perf_df) == 0) return(data.frame())
    
    # Get weights
    weights_df <- get_weights_df()
    if (nrow(weights_df) == 0) return(data.frame())
    
    # Check if required columns exist
    if (!all(c("option", "criterion", "score") %in% colnames(perf_df))) {
      return(data.frame())
    }
    
    # Calculate detailed breakdown
    detailed_df <- perf_df %>%
      dplyr::left_join(weights_df, by = "criterion") %>%
      dplyr::mutate(
        weighted_score = score * weight,
        contribution_pct = ifelse(
          sum(weighted_score, na.rm = TRUE) > 0,
          (weighted_score / sum(weighted_score, na.rm = TRUE)) * 100,
          0
        ),
        contribution = paste0(round(contribution_pct, 1), "%")
      ) %>%
      dplyr::select(option, criterion, score, weight, weighted_score, contribution_pct, contribution) %>%
      dplyr::arrange(option, dplyr::desc(weighted_score))
    
    return(detailed_df)
  })
  
  #### UI Output Functions - ONLY WRITE TO EXISTING OUTPUT IDs ####
  
  # Detailed breakdown table - THIS EXISTS IN YOUR UI
  output$detailed_breakdown_table <- renderDT({
    req(detailed_breakdown())
    
    df <- detailed_breakdown() %>%
      dplyr::rename(
        Alternative = option,
        Criterion = criterion,
        `Raw Score` = score,
        Weight = weight,
        `Weighted Score` = weighted_score,
        Contribution = contribution
      ) %>%
      dplyr::select(Alternative, Criterion, `Raw Score`, Weight, `Weighted Score`, Contribution)
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = 't',
        pageLength = 15,
        scrollX = TRUE,
        ordering = TRUE
      )
    ) %>%
      formatRound(columns = c("Raw Score", "Weight", "Weighted Score"), digits = 3)
  })
  
  # Radar chart for alternative comparison - THIS EXISTS IN YOUR UI
  output$radar_chart <- renderPlotly({
    req(detailed_breakdown())
    
    detailed_df <- detailed_breakdown()
    if (nrow(detailed_df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    # Prepare data for radar chart
    radar_data <- detailed_df %>%
      dplyr::select(option, criterion, weighted_score) %>%
      tidyr::pivot_wider(
        names_from = criterion,
        values_from = weighted_score,
        values_fill = 0
      )
    
    # Get criteria names
    criteria_names <- colnames(radar_data)[-1]
    
    if (length(criteria_names) == 0) {
      return(plot_ly() %>% layout(title = "No criteria data available"))
    }
    
    # Create radar chart
    p <- plot_ly(type = 'scatterpolar', fill = 'toself')
    
    # Add a trace for each alternative
    for(i in 1:nrow(radar_data)) {
      alternative <- radar_data$option[i]
      values <- as.numeric(radar_data[i, -1])
      
      p <- p %>% add_trace(
        r = c(values, values[1]),  # Close the polygon
        theta = c(criteria_names, criteria_names[1]),
        name = alternative,
        mode = 'lines+markers',
        hoverinfo = 'name+theta+r',
        marker = list(size = 6)
      )
    }
    
    max_value <- max(detailed_df$weighted_score, na.rm = TRUE) * 1.1
    if (!is.finite(max_value) || max_value == 0) max_value <- 1
    
    p %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, max_value),
          tickmode = "auto",
          tickformat = ".1f"
        )
      ),
      title = list(text = "Performance Comparison (Radar Chart)", y = 0.95),
      showlegend = TRUE,
      margin = list(t = 50, b = 50, l = 80, r = 80)
    )
  })
  
  # Stacked bar chart showing contribution by criterion - THIS EXISTS IN YOUR UI
  output$stacked_bar_chart <- renderPlotly({
    req(detailed_breakdown())
    
    detailed_df <- detailed_breakdown()
    if (nrow(detailed_df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    # Prepare data for stacked bar chart
    stacked_data <- detailed_df %>%
      dplyr::mutate(
        criterion = as.factor(criterion),
        option = as.factor(option)
      )
    
    # Get unique criteria for color mapping
    criteria <- unique(stacked_data$criterion)
    
    # Generate colors
    if (length(criteria) <= 12) {
      colors <- RColorBrewer::brewer.pal(max(3, length(criteria)), "Set3")
    } else {
      colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(criteria))
    }
    
    plot_ly(stacked_data, 
            x = ~option, 
            y = ~weighted_score, 
            color = ~criterion, 
            type = 'bar',
            colors = colors,
            text = ~paste0("Criterion: ", criterion, 
                           "<br>Weighted Score: ", round(weighted_score, 3),
                           "<br>Contribution: ", contribution),
            hoverinfo = 'text') %>%
      layout(
        title = list(text = "Score Composition by Alternative", y = 0.95),
        xaxis = list(title = "Alternative"),
        yaxis = list(title = "Weighted Score"),
        barmode = 'stack',
        legend = list(orientation = "h", y = -0.2),
        margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
      )
  })
  
  # Download handler for detailed results - THIS EXISTS IN YOUR UI
  output$download_breakdown <- downloadHandler(
    filename = function() {
      paste0("detailed_breakdown_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- detailed_breakdown()
      if (nrow(df) > 0) {
        write.csv(df, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
      }
    }
  )
  
  # Save aggregate results - THIS USES AN EXISTING BUTTON
  observeEvent(input$save_aggregate_notes, {
    aggregate_data$notes <- input$aggregate_notes
    showNotification("Aggregate score notes saved successfully!", type = "message", duration = 3)
  })
  
  # Output for aggregate score notes in report tab - THIS EXISTS IN YOUR UI
  output$aggregate_notes_output <- renderPrint({
    if (is.null(aggregate_data$notes) || aggregate_data$notes == "") {
      cat("No aggregate score notes recorded.")
    } else {
      cat(aggregate_data$notes)
    }
  })
  
  #### Report Tab Outputs - THESE EXIST IN YOUR UI ####
  
  # Aggregate results table for report tab
  output$report_aggregate_table <- renderDT({
    req(aggregate_scores())
    
    df <- aggregate_scores() %>%
      dplyr::select(rank, option, aggregate_score, percentage) %>%
      dplyr::rename(
        Rank = rank,
        Alternative = option,
        `Aggregate Score` = aggregate_score,
        `Percentage Share` = percentage
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        pageLength = nrow(df)
      )
    ) %>%
      formatRound(columns = "Aggregate Score", digits = 3) %>%
      formatStyle('Aggregate Score', fontWeight = 'bold')
  })
  
  # Aggregate bar chart for report tab
  output$report_aggregate_chart <- renderPlotly({
    req(aggregate_scores())
    
    df <- aggregate_scores() %>%
      dplyr::arrange(dplyr::desc(aggregate_score))
    
    plot_ly(df, 
            x = ~reorder(option, aggregate_score), 
            y = ~aggregate_score, 
            type = 'bar',
            marker = list(
              color = ~aggregate_score,
              colorscale = 'Blues',
              showscale = FALSE,
              line = list(color = 'rgb(0,0,0)', width = 0.5)
            ),
            text = ~paste0("Alternative: ", option, 
                           "<br>Score: ", round(aggregate_score, 3),
                           "<br>Rank: ", rank,
                           "<br>Share: ", percentage),
            hoverinfo = 'text') %>%
      layout(
        title = list(text = "Aggregate Scores by Alternative", y = 0.95),
        xaxis = list(title = "Alternative", tickangle = -45),
        yaxis = list(title = "Aggregate Score"),
        margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
      )
  })
  
  #### Utility Functions ####
  
  #' Get the top-ranked alternative
  get_top_alternative <- reactive({
    req(aggregate_scores())
    scores <- aggregate_scores()
    if (nrow(scores) > 0) {
      return(scores$option[which.min(scores$rank)])
    }
    return(NULL)
  })
  
  #' Check if aggregate scores are available
  has_aggregate_scores <- reactive({
    !is.null(aggregate_scores()) && nrow(aggregate_scores()) > 0
  })
  
  # Utility function for NULL coalescing
  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }
  
  #### Return Module Components ####
  
  return(
    list(
      data = aggregate_data,
      utilities = list(
        filtered_criteria = filtered_criteria,
        get_performance_df = get_performance_df,
        get_weights_df = get_weights_df,
        get_top_alternative = get_top_alternative,
        has_aggregate_scores = has_aggregate_scores
      ),
      aggregate_scores = aggregate_scores,
      detailed_breakdown = detailed_breakdown
      # REMOVED ui_outputs list since we're writing directly to global outputs
    )
  )
}