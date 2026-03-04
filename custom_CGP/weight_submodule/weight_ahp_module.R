#' Weight AHP Module
#'
#' This module handles Analytical Hierarchy Process (AHP) weighting method.
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
weight_ahp_module <- function(input, output, session,
                              weights_data, criteria_data,
                              filtered_criteria,
                              is_active = reactive(TRUE),
                              config = NULL) {
  
  # Random Index for consistency check (Saaty's RI)
  RI_VALUES <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)
  
  #### AHP Weighting UI ####
  
  # AHP pairwise comparison UI
  output$ahp_weights_matrix_ui <- renderUI({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    criteria <- filtered_criteria()$name
    n <- length(criteria)
    
    if (n < 2) {
      return(p("Need at least 2 criteria for AHP pairwise comparisons."))
    }
    
    comparisons <- list()
    index <- 1
    
    # Saaty's scale choices (1-9 and reciprocals)
    choices <- list(
      "Extremely more important (9)" = "9",
      "Very strongly more important (8)" = "8",
      "Very strongly more important (7)" = "7",
      "Strongly more important (6)" = "6",
      "Strongly more important (5)" = "5",
      "Moderately more important (4)" = "4",
      "Moderately more important (3)" = "3",
      "Equal importance (2)" = "2",
      "Equal importance (1)" = "1",
      "Moderately less important (1/2)" = "1/2",
      "Moderately less important (1/3)" = "1/3",
      "Strongly less important (1/4)" = "1/4",
      "Strongly less important (1/5)" = "1/5",
      "Very strongly less important (1/6)" = "1/6",
      "Very strongly less important (1/7)" = "1/7",
      "Extremely less important (1/8)" = "1/8",
      "Extremely less important (1/9)" = "1/9"
    )
    
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        
        # Check if we have a saved comparison value
        comp_key <- paste0(i, "_", j)
        saved_value <- if (!is.null(weights_data$ahp$comparisons[[comp_key]])) {
          weights_data$ahp$comparisons[[comp_key]]
        } else {
          "1"
        }
        
        comparisons[[index]] <- fluidRow(
          column(4, p(strong(criteria[i]), "vs", strong(criteria[j]))),
          column(8,
                 selectInput(
                   inputId = paste0("ahp_weight_comp_", i, "_", j),  # No namespace!
                   label = NULL,
                   choices = choices,
                   selected = saved_value,
                   width = "100%"
                 )
          )
        )
        index <- index + 1
      }
    }
    
    do.call(tagList, comparisons)
  })
  
  # AHP weights output
  output$ahp_weights_output <- renderPrint({
    req(is_active())
    req(weights_data$ahp$scores)
    
    scores <- weights_data$ahp$scores * 100  # Convert to percentages
    
    # Create formatted data frame
    results_df <- data.frame(
      Criterion = names(scores),
      Weight = round(scores, 2),
      Percentage = paste0(round(scores, 1), "%"),
      Rank = rank(-scores, ties.method = "min")
    ) %>%
      arrange(desc(Weight))
    
    cat("AHP Derived Weights:\n\n")
    print(results_df, row.names = FALSE)
    cat("\n")
    
    # Add summary statistics
    cat("Summary Statistics:\n")
    cat(sprintf("Total Weight: %.1f%%\n", sum(scores)))
    cat(sprintf("Weight Range: %.1f%% - %.1f%%\n", min(scores), max(scores)))
    cat(sprintf("Standard Deviation: %.2f\n", sd(scores)))
  })
  
  # AHP weights consistency output
  output$ahp_weights_consistency_output <- renderPrint({
    req(is_active())
    req(weights_data$ahp$consistency)
    
    cons <- weights_data$ahp$consistency
    n <- length(weights_data$ahp$scores)
    
    cat("\nConsistency Analysis:\n")
    cat("=====================\n")
    cat(sprintf("Consistency Index (CI): %.3f\n", cons$CI))
    cat(sprintf("Random Index (RI): %.3f\n", RI_VALUES[n]))
    cat(sprintf("Consistency Ratio (CR): %.3f\n", cons$CR))
    cat(sprintf("Maximum Eigenvalue (λmax): %.3f\n", cons$lambda_max))
    cat("\n")
    
    if (cons$CR < 0.1) {
      cat("✓ CR < 0.1: Comparisons are CONSISTENT\n")
      cat("The pairwise comparisons show acceptable consistency.\n")
    } else {
      cat("✗ CR ≥ 0.1: Comparisons are INCONSISTENT\n")
      cat("Please review your pairwise comparisons for better consistency.\n")
    }
  })
  
  # AHP weights full matrix
  output$ahp_weights_full_matrix <- renderDT({
    req(is_active())
    req(weights_data$ahp$matrix)
    
    mat <- weights_data$ahp$matrix
    
    df <- as.data.frame(mat)
    df <- cbind(Criterion = rownames(df), df)
    rownames(df) <- NULL
    
    # Format values as fractions or whole numbers
    for (i in 2:ncol(df)) {
      df[[i]] <- sapply(df[[i]], function(x) {
        if (abs(x - round(x)) < 1e-6) {
          as.character(round(x))
        } else {
          # Find fraction representation
          recip <- 1/x
          if (abs(recip - round(recip)) < 1e-6) {
            paste0("1/", round(recip))
          } else {
            as.character(round(x, 2))
          }
        }
      })
    }
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        scrollX = TRUE,
        ordering = FALSE
      ),
      class = "stripe hover"
    )
  })
  
  # AHP comparison matrix output (summary table)
  output$ahp_weights_comparison_matrix <- renderDT({
    req(is_active())
    req(weights_data$ahp$scores)
    
    # Create summary table with weights and consistency
    scores_df <- data.frame(
      Criterion = names(weights_data$ahp$scores),
      Weight = round(weights_data$ahp$scores * 100, 2),
      stringsAsFactors = FALSE
    )
    
    datatable(
      scores_df,
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = TRUE,
        pageLength = nrow(scores_df)
      )
    ) %>%
      formatStyle('Weight', fontWeight = 'bold')
  })
  
  # AHP Weights Pie Chart
  output$ahp_weights_pie_chart <- renderPlotly({
    req(is_active())
    req(weights_data$ahp$scores)
    
    # Convert to percentages for better visualization
    ahp_weights_percent <- weights_data$ahp$scores * 100
    
    plot_data <- data.frame(
      Criterion = names(ahp_weights_percent),
      Weight = ahp_weights_percent
    )
    
    # Use RColorBrewer if available
    colors <- tryCatch({
      RColorBrewer::brewer.pal(nrow(plot_data), "Pastel1")
    }, error = function(e) {
      NULL
    })
    
    p <- plot_ly(plot_data, labels = ~Criterion, values = ~Weight, type = 'pie',
                 textposition = 'inside', textinfo = 'label+percent',
                 hoverinfo = 'label+value+percent',
                 hole = 0.3)  # Donut chart
    
    if (!is.null(colors)) {
      p <- p %>% layout(marker = list(colors = colors))
    }
    
    p %>% layout(
      title = list(text = "AHP Weight Distribution", y = 0.95),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.1),
      margin = list(t = 50, b = 50)
    )
  })
  
  # AHP Weights Bar Chart
  output$ahp_weights_bar_chart <- renderPlotly({
    req(is_active())
    req(weights_data$ahp$scores)
    
    # Convert to percentages and sort for better visualization
    ahp_weights_percent <- weights_data$ahp$scores * 100
    
    plot_data <- data.frame(
      Criterion = names(ahp_weights_percent),
      Weight = ahp_weights_percent
    ) %>%
      arrange(desc(Weight))
    
    p <- plot_ly(plot_data, x = ~Criterion, y = ~Weight, type = 'bar',
                 marker = list(color = 'rgba(191, 128, 55, 0.7)',
                               line = list(color = 'rgba(191, 128, 55, 1.0)', width = 1.5)),
                 text = ~paste0(round(Weight, 1), "%"), textposition = 'auto') %>%
      layout(
        title = list(text = "AHP Weight Comparison", y = 0.95),
        xaxis = list(title = "Criterion", tickangle = -45),
        yaxis = list(title = "Weight (%)", range = c(0, max(plot_data$Weight) * 1.1)),
        margin = list(b = 100, t = 50)
      )
    
    # Add consistency annotation if available
    if (!is.null(weights_data$ahp$consistency)) {
      cons <- weights_data$ahp$consistency
      p <- p %>% layout(
        annotations = list(
          list(
            x = 0.5,
            y = 1.05,
            xref = "paper",
            yref = "paper",
            text = paste("Consistency Ratio:", round(cons$CR, 3),
                         ifelse(cons$CR < 0.1, "✓ Consistent", "✗ Inconsistent")),
            showarrow = FALSE,
            font = list(size = 12,
                        color = ifelse(cons$CR < 0.1, "green", "red"))
          )
        )
      )
    }
    
    p
  })
  
  #### AHP Weighting Logic ####
  
  # Calculate AHP weights
  observeEvent(input$calculate_ahp_weights, {
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    criteria_names <- filtered_criteria()$name
    n <- length(criteria_names)
    
    if (n < 2) {
      showNotification("Need at least 2 criteria for AHP analysis", type = "error", duration = 5)
      return()
    }
    
    # Initialize pairwise comparison matrix
    mat <- matrix(1, nrow = n, ncol = n, dimnames = list(criteria_names, criteria_names))
    
    # Fill matrix from user inputs
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        input_id <- paste0("ahp_weight_comp_", i, "_", j)  # No namespace!
        if (!is.null(input[[input_id]])) {
          val_str <- input[[input_id]]
          
          # Parse the value (could be numeric or fraction like "1/3")
          val <- tryCatch({
            eval(parse(text = val_str))
          }, error = function(e) {
            1
          })
          
          mat[i, j] <- val
          mat[j, i] <- 1/val
          
          # Store the comparison value
          comp_key <- paste0(i, "_", j)
          weights_data$ahp$comparisons[[comp_key]] <- val_str
        }
      }
    }
    
    # Store matrix
    weights_data$ahp$matrix <- mat
    
    # Calculate weights using geometric mean method
    tryCatch({
      # Geometric mean of each row
      geo_means <- apply(mat, 1, function(x) exp(mean(log(x))))
      
      # Normalize to sum to 1
      weights <- geo_means / sum(geo_means)
      names(weights) <- criteria_names
      
      weights_data$ahp$scores <- weights
      
      # Consistency check
      eigen_result <- eigen(mat)
      lambda_max <- Re(eigen_result$values[1])
      CI <- (lambda_max - n) / (n - 1)
      RI <- RI_VALUES[n]
      CR <- CI / RI
      
      weights_data$ahp$consistency <- list(
        CR = CR,
        CI = CI,
        lambda_max = lambda_max
      )
      
      showNotification("AHP weights calculated successfully!", type = "message", duration = 3)
      
      if (CR >= 0.1) {
        showNotification(
          paste("Consistency Ratio =", round(CR, 3), "- Please review your comparisons"),
          type = "warning",
          duration = 10
        )
      }
      
    }, error = function(e) {
      showNotification(
        paste("Error calculating AHP weights:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # Save AHP weights
  observeEvent(input$save_ahp_weights, {
    req(is_active())
    req(weights_data$ahp$scores)
    
    # Update main weights_data with AHP weights (convert to percentage)
    for (i in 1:length(weights_data$ahp$scores)) {
      criterion_name <- names(weights_data$ahp$scores)[i]
      criterion_id <- filtered_criteria()$id[filtered_criteria()$name == criterion_name]
      
      if (length(criterion_id) > 0) {
        weights_data$weights[[as.character(criterion_id)]] <- 
          weights_data$ahp$scores[i] * 100
      }
    }
    
    showNotification("AHP weights saved successfully!", type = "message", duration = 3)
  })
  
  #### Return Module Components ####
  return(
    list(
      data = list(
        matrix = reactive({ weights_data$ahp$matrix }),
        scores = reactive({ weights_data$ahp$scores }),
        consistency = reactive({ weights_data$ahp$consistency }),
        comparisons = reactive({ weights_data$ahp$comparisons })
      )
    )
  )
}