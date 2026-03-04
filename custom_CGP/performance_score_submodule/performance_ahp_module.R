#' Performance AHP Module
#'
#' This module handles Analytical Hierarchy Process (AHP) scoring method
#' for performance assessment. It provides pairwise comparisons between
#' alternatives for each criterion.
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
#'   \item{data - Reactive values for AHP data}
#'   \item{utilities - Utility functions for data access}
#' }
#'
#' @export
performance_ahp_module <- function(input, output, session,
                                   performance_data, decision_data, 
                                   criteria_data, filtered_criteria,
                                   is_active = reactive(TRUE),
                                   config = NULL) {
  
  # Namespace for this module
  ns <- session$ns
  
  #### AHP Helper Functions ####
  
  #' Calculate Consistency Ratio
  #'
  #' Calculates consistency ratio for AHP pairwise comparison matrix
  #'
  #' @param mat Pairwise comparison matrix
  #' @return List with consistency results
  calculate_consistency <- function(mat) {
    n <- nrow(mat)
    
    # Calculate principal eigenvector
    eig <- eigen(mat)
    max_eigenvalue <- max(Re(eig$values))
    
    # Calculate consistency index
    CI <- (max_eigenvalue - n) / (n - 1)
    
    # Random consistency index (RI) values
    RI_table <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)
    RI <- ifelse(n <= length(RI_table), RI_table[n], 1.49)
    
    # Calculate consistency ratio
    CR <- CI / RI
    
    return(list(
      max_eigenvalue = max_eigenvalue,
      CI = CI,
      RI = RI,
      CR = CR,
      is_consistent = CR < 0.1
    ))
  }
  
  #### AHP UI ####
  
  # AHP Scoring UI
  output$ahp_scoring_ui <- renderUI({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    # Get alternatives
    alternatives <- c(decision_data$intervention)
    comps <- unlist(strsplit(decision_data$comparators, "\n"))
    comps <- comps[comps != ""]
    if (length(comps) > 0) {
      alternatives <- c(alternatives, comps)
    }
    
    criteria <- filtered_criteria()$name
    
    # Create UI for each criterion
    lapply(criteria, function(criterion) {
      box(
        title = criterion,
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        p("Compare alternatives for this criterion:"),
        uiOutput(paste0("ahp_criterion_", make.names(criterion), "_ui"))
      )
    })
  })
  
  # UI for each criterion's pairwise comparisons
  observe({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    # Get alternatives
    alternatives <- c(decision_data$intervention)
    comps <- unlist(strsplit(decision_data$comparators, "\n"))
    comps <- comps[comps != ""]
    if (length(comps) > 0) {
      alternatives <- c(alternatives, comps)
    }
    
    n_alt <- length(alternatives)
    
    if (n_alt < 2) return()
    
    criteria <- filtered_criteria()$name
    
    for (criterion in criteria) {
      local({
        crit_name <- criterion
        output_id <- paste0("ahp_criterion_", make.names(crit_name), "_ui")
        
        output[[output_id]] <- renderUI({
          req(is_active())
          comparisons <- list()
          index <- 1
          
          choices <- list(
            "9" = "9", "8" = "8", "7" = "7", "6" = "6", "5" = "5",
            "4" = "4", "3" = "3", "2" = "2", "1" = "1",
            "1/2" = "1/2", "1/3" = "1/3", "1/4" = "1/4", "1/5" = "1/5",
            "1/6" = "1/6", "1/7" = "1/7", "1/8" = "1/8", "1/9" = "1/9"
          )
          
          for (i in 1:(n_alt-1)) {
            for (j in (i+1):n_alt) {
              comparisons[[index]] <- fluidRow(
                column(4, p(strong(alternatives[i]), "vs", strong(alternatives[j]))),
                column(8, 
                       selectInput(
                         inputId = paste0("ahp_score_", make.names(crit_name), "_", i, "_", j),
                         label = NULL,
                         choices = choices,
                         selected = "1"
                       )
                )
              )
              index <- index + 1
            }
          }
          
          do.call(tagList, comparisons)
        })
      })
    }
  })
  
  #### AHP Logic ####
  
  # Calculate AHP performance scores
  observeEvent(input$calculate_ahp_scores, {
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    # Set the scoring method to 'ahp' for the report
    performance_data$scoring_method <- "ahp"
    
    # Get alternatives
    alternatives <- c(decision_data$intervention)
    comps <- unlist(strsplit(decision_data$comparators, "\n"))
    comps <- comps[comps != ""]
    if (length(comps) > 0) {
      alternatives <- c(alternatives, comps)
    }
    
    criteria <- filtered_criteria()$name
    performance_data$ahp_scores_matrix <- matrix(0, nrow = length(alternatives), 
                                                 ncol = length(criteria),
                                                 dimnames = list(alternatives, criteria))
    
    # Store matrices and consistency results
    performance_data$ahp_matrices <- list()
    performance_data$ahp_consistency <- list()
    
    # Calculate scores for each criterion
    for (criterion in criteria) {
      safe_criterion <- make.names(criterion)
      n_alt <- length(alternatives)
      
      if (n_alt < 2) next
      
      # Create pairwise comparison matrix
      mat <- matrix(1, nrow = n_alt, ncol = n_alt, dimnames = list(alternatives, alternatives))
      
      for (i in 1:(n_alt-1)) {
        for (j in (i+1):n_alt) {
          input_id <- paste0("ahp_score_", safe_criterion, "_", i, "_", j)
          if (!is.null(input[[input_id]])) {
            val <- eval(parse(text = input[[input_id]]))
            mat[i,j] <- val
            mat[j,i] <- 1/val
          }
        }
      }
      
      # Store the matrix
      performance_data$ahp_matrices[[criterion]] <- mat
      
      # Calculate consistency
      consistency <- calculate_consistency(mat)
      performance_data$ahp_consistency[[criterion]] <- consistency
      
      # Calculate scores using geometric mean
      scores <- apply(mat, 1, function(x) exp(mean(log(x))))
      scores <- scores/sum(scores)
      
      # Store scores
      performance_data$ahp_scores_matrix[, criterion] <- scores * 100  # Scale to 0-100
    }
    
    showNotification("AHP performance scores calculated successfully!", type = "message")
  })
  
  #### AHP Outputs ####
  
  # AHP performance scores output
  output$ahp_performance_scores_output <- renderPrint({
    req(is_active())
    
    if (is.null(performance_data$ahp_scores_matrix)) {
      cat("No AHP scores calculated yet. Click 'Calculate AHP Performance Scores' to generate scores.\n")
      return(NULL)
    }
    
    scores_df <- as.data.frame(performance_data$ahp_scores_matrix)
    total_scores <- rowSums(scores_df)
    
    cat("AHP Performance Scores:\n\n")
    result_df <- data.frame(
      Alternative = rownames(scores_df),
      scores_df,
      Total_Score = total_scores,
      Percentage = paste0(round(total_scores/sum(total_scores)*100, 1), "%"),
      stringsAsFactors = FALSE
    )
    
    print(result_df, row.names = FALSE)
  })
  
  # AHP performance consistency output
  output$ahp_consistency_output <- renderPrint({
    req(is_active())
    
    if (is.null(performance_data$ahp_consistency)) {
      cat("No AHP consistency data available.\n")
      return(NULL)
    }
    
    cons_results <- performance_data$ahp_consistency
    criteria_names <- names(cons_results)
    
    cat("Consistency Check Results:\n\n")
    for (criterion in criteria_names) {
      cons <- cons_results[[criterion]]
      cat(sprintf("Criterion: %s\n", criterion))
      cat(sprintf("Consistency Ratio (CR): %.3f", cons$CR))
      if(cons$CR < 0.1) {
        cat(" - Consistent (CR < 0.1)\n")
      } else {
        cat(" - INCONSISTENT (CR ≥ 0.1) - please review!\n")
      }
      cat(sprintf("Max Eigenvalue: %.3f, CI: %.3f, RI: %.3f\n\n", 
                  cons$max_eigenvalue, cons$CI, cons$RI))
    }
  })
  
  # AHP performance comparison matrix
  output$ahp_weights_comparison_matrix <- renderDT({
    req(is_active())
    
    if (is.null(performance_data$ahp_matrices) || length(performance_data$ahp_matrices) == 0) {
      return(datatable(data.frame(Message = "No AHP matrices calculated yet")))
    }
    
    # Get the first criterion's matrix for display
    criterion <- names(performance_data$ahp_matrices)[1]
    mat <- performance_data$ahp_matrices[[criterion]]
    
    df <- as.data.frame(mat)
    df <- cbind(Alternative = rownames(df), df)
    rownames(df) <- NULL
    
    # Format the values for display
    for(i in 2:ncol(df)) {
      df[[i]] <- sapply(df[[i]], function(x) {
        if(abs(x - round(x)) < 1e-6) {
          as.character(round(x))
        } else if (x < 1) {
          paste0("1/", round(1/x))
        } else {
          as.character(round(x, 2))
        }
      })
    }
    
    datatable(
      df,
      caption = paste("Pairwise Comparison Matrix for:", criterion),
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        scrollX = TRUE
      )
    )
  })
  
  # AHP performance full matrices (all criteria)
  output$ahp_weights_full_matrix <- renderDT({
    req(is_active())
    
    if (is.null(performance_data$ahp_matrices) || length(performance_data$ahp_matrices) == 0) {
      return(datatable(data.frame(Message = "No AHP matrices calculated yet")))
    }
    
    # Combine all matrices into one data frame
    all_matrices <- list()
    
    for (criterion in names(performance_data$ahp_matrices)) {
      mat <- performance_data$ahp_matrices[[criterion]]
      df <- as.data.frame(mat)
      df$Criterion <- criterion
      df$Alternative <- rownames(df)
      all_matrices[[criterion]] <- df
    }
    
    combined_df <- do.call(rbind, all_matrices)
    combined_df <- combined_df[, c("Criterion", "Alternative", setdiff(names(combined_df), c("Criterion", "Alternative")))]
    
    # Format the values for display
    numeric_cols <- sapply(combined_df, is.numeric)
    for(col in names(combined_df)[numeric_cols]) {
      combined_df[[col]] <- sapply(combined_df[[col]], function(x) {
        if(abs(x - round(x)) < 1e-6) {
          as.character(round(x))
        } else if (x < 1) {
          paste0("1/", round(1/x))
        } else {
          as.character(round(x, 2))
        }
      })
    }
    
    datatable(
      combined_df,
      caption = "Pairwise Comparison Matrices for All Criteria",
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        scrollX = TRUE,
        pageLength = 20
      )
    )
  })
  
  # Normalized performance matrix output
  output$ahp_normalized_matrix_output <- renderPrint({
    req(is_active())
    
    if (is.null(performance_data$ahp_scores_matrix)) {
      cat("No AHP scores calculated yet.\n")
      return(NULL)
    }
    
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
  })
  
  #### AHP Data ####
  
  #' AHP Performance Data Frame
  #'
  #' Creates a data frame with AHP performance scores
  #'
  #' @return Reactive data frame with AHP scores
  ahp_performance_df <- reactive({
    req(nrow(filtered_criteria()) > 0)
    
    if (is.null(performance_data$ahp_scores_matrix)) {
      return(data.frame())
    }
    
    df <- as.data.frame(performance_data$ahp_scores_matrix)
    df$option <- rownames(df)
    
    df_long <- tidyr::pivot_longer(df, cols = -option, names_to = "criterion", values_to = "score")
    df_long$criterion <- factor(df_long$criterion, levels = colnames(performance_data$ahp_scores_matrix))
    
    df_long
  })
  
  #### AHP Visualizations ####
  
  # AHP performance bar chart
  output$ahp_performance_bar_chart <- renderPlotly({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    df <- ahp_performance_df()
    
    if (nrow(df) == 0 || all(df$score == 0)) {
      return(plot_ly() %>% layout(title = "No AHP performance data yet"))
    }
    
    # Calculate max y value for proper scaling
    max_y <- df %>%
      group_by(option) %>%
      summarise(total_score = sum(score, na.rm = TRUE)) %>%
      pull(total_score) %>%
      max(na.rm = TRUE)
    
    # Add buffer to max y value
    max_y <- max_y * 1.1
    
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
        title = 'Performance by option (AHP Scoring)',
        yaxis = list(
          title = 'Score', 
          range = c(0, max_y),
          automargin = TRUE
        ),
        xaxis = list(
          title = 'Option',
          automargin = TRUE
        ),
        margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
      )
  })
  
  # AHP Spider Chart
  output$ahp_performance_spider_plot <- renderPlotly({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    df <- ahp_performance_df()
    
    if (nrow(df) == 0 || all(df$score == 0)) {
      return(plot_ly() %>% layout(title = "No AHP performance data yet"))
    }
    
    # Prepare data for spider chart
    spider_data <- df %>%
      group_by(option, criterion) %>%
      summarise(score = mean(score), .groups = 'drop') %>%
      pivot_wider(names_from = criterion, values_from = score)
    
    # Create spider chart for each alternative
    plot <- plot_ly(type = 'scatterpolar', fill = 'toself')
    
    for (i in 1:nrow(spider_data)) {
      plot <- plot %>% add_trace(
        r = as.numeric(spider_data[i, -1]),
        theta = names(spider_data)[-1],
        name = spider_data$option[i]
      )
    }
    
    plot %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 100)
        )
      ),
      title = "Performance Spider Chart (AHP)"
    )
  })
  
  # AHP Performance by Criterion
  output$ahp_performance_by_criterion <- renderPlotly({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    df <- ahp_performance_df()
    
    if (nrow(df) == 0 || all(df$score == 0)) {
      return(plot_ly() %>% layout(title = "No AHP performance data yet"))
    }
    
    plot_ly(
      data = df,
      x = ~criterion,
      y = ~score,
      color = ~option,
      type = 'bar',
      text = ~paste0(option, ": ", round(score, 1)),
      hoverinfo = 'text+y'
    ) %>%
      layout(
        barmode = 'group',
        title = 'Performance by Criterion (AHP)',
        yaxis = list(title = 'Score', range = c(0, 100)),
        xaxis = list(title = 'Criterion')
      )
  })
  
  # AHP Heat Map
  output$ahp_performance_heatmap <- renderPlotly({
    req(is_active())
    req(nrow(filtered_criteria()) > 0)
    
    if (is.null(performance_data$ahp_scores_matrix)) {
      return(plot_ly() %>% layout(title = "No AHP scores calculated yet"))
    }
    
    df <- as.data.frame(performance_data$ahp_scores_matrix)
    df$Alternative <- rownames(df)
    
    df_long <- tidyr::pivot_longer(df, cols = -Alternative, names_to = "Criterion", values_to = "Score")
    
    plot_ly(
      data = df_long,
      x = ~Criterion,
      y = ~Alternative,
      z = ~Score,
      type = "heatmap",
      colors = colorRamp(c("white", "steelblue")),
      colorbar = list(title = "Score"),
      hoverinfo = "text",
      text = ~paste("Alternative:", Alternative, "<br>",
                    "Criterion:", Criterion, "<br>",
                    "Score:", round(Score, 1))
    ) %>%
      layout(
        title = "Performance Heat Map (AHP)",
        xaxis = list(title = "Criterion"),
        yaxis = list(title = "Alternative")
      )
  })
  
  # Save AHP scores
  observeEvent(input$save_ahp_scores, {
    req(is_active())
    
    if (is.null(performance_data$ahp_scores_matrix)) {
      showNotification("No AHP scores to save. Please calculate scores first.", type = "warning")
      return()
    }
    
    # Create a data frame with the scores
    scores_df <- as.data.frame(performance_data$ahp_scores_matrix)
    scores_df$Alternative <- rownames(scores_df)
    scores_df <- scores_df[, c("Alternative", setdiff(names(scores_df), "Alternative"))]
    
    # Save to a CSV file
    write.csv(scores_df, "ahp_performance_scores.csv", row.names = FALSE)
    
    showNotification("AHP performance scores saved successfully!", type = "message")
  })
  
  #### Return Module Components ####
  return(
    list(
      data = list(
        ahp_matrices = reactive(performance_data$ahp_matrices),
        ahp_scores_matrix = reactive(performance_data$ahp_scores_matrix),
        ahp_consistency = reactive(performance_data$ahp_consistency)
      ),
      utilities = list(
        get_ahp_performance_df = ahp_performance_df,
        calculate_consistency = calculate_consistency
      )
    )
  )
}