#' Performance Direct Rating Module
#'
#' This module handles direct rating scoring method for performance assessment.
#' It provides sliders for rating each criterion and generates performance visualizations.
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
#'   \item{data - Reactive values for direct rating data}
#'   \item{ui_outputs - List of UI rendering functions}
#'   \item{utilities - Utility functions for data access}
#' }
#'
#' @export
performance_direct_rating_module <- function(input, output, session,
                                             performance_data, decision_data, 
                                             criteria_data, filtered_criteria,
                                             is_active = reactive(TRUE),
                                             config = NULL) {
  
  #### Direct Rating UI ####
  
  # Intervention performance UI
  output$intervention_performance_ui <- renderUI({
    # Only render if module is active
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    req(decision_data$intervention)
    
    lapply(1:nrow(filtered_criteria()), function(i) {
      criterion <- filtered_criteria()[i, ]
      sliderInput(
        inputId = paste0("intervention_", criterion$id),
        label = criterion$name,
        min = performance_data$direct_min,
        max = performance_data$direct_max,
        value = ifelse(
          is.null(performance_data$intervention[[as.character(criterion$id)]]),
          (performance_data$direct_min + performance_data$direct_max) / 2,
          performance_data$intervention[[as.character(criterion$id)]]
        ),
        step = 1,
        width = "100%"
      )
    })
  })
  
  # Comparators performance UI
  output$comparators_performance_ui <- renderUI({
    # Only render if module is active
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
          sliderInput(
            inputId = paste0("comparator_", make.names(comp), "_", criterion$id),
            label = criterion$name,
            min = performance_data$direct_min,
            max = performance_data$direct_max,
            value = ifelse(
              is.null(performance_data$comparators[[comp]][[as.character(criterion$id)]]),
              (performance_data$direct_min + performance_data$direct_max) / 2,
              performance_data$comparators[[comp]][[as.character(criterion$id)]]
            ),
            step = 1,
            width = "100%"
          )
        }),
        hr()
      )
    })
  })
  
  #### Direct Rating Data ####
  
  #' Performance Data Frame
  #'
  #' Creates a data frame with current performance scores
  #'
  #' @return Reactive data frame with performance scores
  performance_df <- reactive({
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
      saved_val <- performance_data$intervention[[as.character(cid)]]
      
      if (!is.null(saved_val)) {
        score <- saved_val
      } else {
        input_id <- paste0("intervention_", cid)
        score <- if (!is.null(input[[input_id]])) input[[input_id]] else NA
      }
      
      rows[[length(rows) + 1]] <- data.frame(
        option = intervention_name,
        criterion = cname,
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
          saved_val <- NULL
          
          if (!is.null(performance_data$comparators[[comp]])) {
            saved_val <- performance_data$comparators[[comp]][[as.character(cid)]]
          }
          
          if (!is.null(saved_val)) {
            score <- saved_val
          } else {
            input_id <- paste0("comparator_", make.names(comp), "_", cid)
            score <- if (!is.null(input[[input_id]])) input[[input_id]] else NA
          }
          
          rows[[length(rows) + 1]] <- data.frame(
            option = comp,
            criterion = cname,
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
  
  #### Observers ####
  
  # Save performance scores
  observeEvent(input$save_direct_performance, {
    req(nrow(filtered_criteria()) > 0)
    
    # Save intervention scores
    performance_data$intervention <- list()
    for (i in 1:nrow(filtered_criteria())) {
      criterion <- filtered_criteria()[i, ]
      input_id <- paste0("intervention_", criterion$id)
      if (!is.null(input[[input_id]])) {
        performance_data$intervention[[as.character(criterion$id)]] <- input[[input_id]]
      }
    }
    
    # Save comparator scores
    comps <- unlist(strsplit(decision_data$comparators, "\n"))
    comps <- comps[comps != ""]
    performance_data$comparators <- list()
    
    for (comp in comps) {
      performance_data$comparators[[comp]] <- list()
      for (i in 1:nrow(filtered_criteria())) {
        criterion <- filtered_criteria()[i, ]
        input_id <- paste0("comparator_", make.names(comp), "_", criterion$id)
        if (!is.null(input[[input_id]])) {
          performance_data$comparators[[comp]][[as.character(criterion$id)]] <- input[[input_id]]
        }
      }
    }
    
    showNotification("Direct performance scores saved successfully!", type = "message")
  })
  
  #### Visualizations ####
  
  # Performance bar chart
  output$performance_bar_chart <- renderPlotly({
    req(nrow(filtered_criteria()) > 0)
    df <- performance_df()
    if (all(df$score == 0)) {
      plot_ly() %>% layout(title = "No performance data yet")
    } else {
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
          title = 'Performance by option (stacked by criterion)',
          yaxis = list(title = 'Score'),
          xaxis = list(title = 'Option')
        )
    }
  })
  
  # Spider/radar chart
  output$performance_spider_plot <- renderPlotly({
    req(nrow(filtered_criteria()) > 0)
    df <- performance_df()
    
    if (nrow(df) == 0 || all(df$score == 0)) {
      return(plot_ly() %>% layout(title = "No performance data yet"))
    }
    
    # Calculate dynamic range (with some padding)
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
            min(100, max_score + range_padding)
          ),
          tickmode = "auto",
          tickformat = ".0f"
        )
      ),
      showlegend = TRUE,
      title = "Performance Radar",
      margin = list(t = 50, b = 50)
    )
  })
  
  # Performance by criterion (subplots)
  output$performance_by_criterion <- renderPlotly({
    req(nrow(filtered_criteria()) > 0)
    df <- performance_df()
    
    if (nrow(df) == 0 || all(df$score == 0)) {
      return(plot_ly() %>% layout(title = "No performance data yet"))
    }
    
    plots <- lapply(unique(df$criterion), function(crit) {
      crit_df <- df[df$criterion == crit, ]
      
      # Calculate dynamic range for this criterion
      max_score <- max(crit_df$score, na.rm = TRUE)
      min_score <- min(crit_df$score, na.rm = TRUE)
      range_padding <- (max_score - min_score) * 0.1
      
      plot_ly(
        crit_df,
        x = ~option,
        y = ~score,
        type = 'bar',
        name = crit,
        text = ~paste(option, "<br>Score:", score),
        hoverinfo = 'text',
        marker = list(line = list(width = 1, color = 'rgb(0,0,0)'))
      ) %>%
        layout(
          title = list(text = crit, font = list(size = 10)),
          xaxis = list(title = ""),
          yaxis = list(
            title = "Score", 
            range = c(max(0, min_score - range_padding), 
                      min(100, max_score + range_padding)),
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
        title = "Performance by Criterion",
        margin = list(t = 50, b = 100)
      )
  })
  
  #### Return Module Components ####
  return(
    list(
      data = list(
        performance_df = performance_df
      ),
      utilities = list(
        get_performance_df = performance_df
      )
    )
  )
}