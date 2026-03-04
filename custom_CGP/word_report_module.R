# ============================================================================
# FILE: word_report_module.R
# ============================================================================
#' Word Report Module 
#' 
#' Generates a comprehensive Word document report including all MCDA components.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param decision_data Reactive values from decision module
#' @param criteria_data Reactive values from criteria module
#' @param performance_data Reactive values from performance module
#' @param performance_module The complete performance module object (for accessing utilities)
#' @param weights_data Reactive values from weights module (optional)
#' @param aggregate_data Reactive values from aggregate module (optional)
#' @param aggregate_module The complete aggregate module object (for accessing scores and breakdowns)
#' @param uncertainty_data Reactive values from uncertainty module (optional)
#' @param uncertainty_module The complete uncertainty module object (for accessing uncertainty data)
#'
#' @return A list containing:
#' \itemize{
#'   \item{generate_word_report - Function to generate the Word report}
#' }
#' 
#' @export
word_report_module <- function(input, output, session, 
                               decision_data = NULL,
                               criteria_data = NULL,
                               performance_data = NULL,
                               performance_module = NULL,
                               weights_data = NULL,
                               aggregate_data = NULL,
                               aggregate_module = NULL,
                               uncertainty_data = NULL,
                               uncertainty_module = NULL) {
  
  # Generate Word report function
  generate_word_report <- function(filename = "MCDA_Report.docx") {
    
    # --- ISOLATE ALL DATA EXTRACTION IN A REACTIVE CONTEXT ---
    extracted_data <- isolate({
      
      # ---------------------------------------------------------
      # EXTRACT DECISION DATA
      # ---------------------------------------------------------
      decision_data_list <- NULL
      if (!is.null(decision_data)) {
        decision_data_list <- list(
          population = ifelse(is.null(decision_data$population), "", decision_data$population),
          intervention = ifelse(is.null(decision_data$intervention), "", decision_data$intervention),
          comparators = ifelse(is.null(decision_data$comparators), "", decision_data$comparators),
          outcomes = ifelse(is.null(decision_data$outcomes), "", decision_data$outcomes),
          time_frame = ifelse(is.null(decision_data$time_frame), "", decision_data$time_frame),
          incidence = ifelse(is.null(decision_data$incidence), 5000, decision_data$incidence),
          prevalence = ifelse(is.null(decision_data$prevalence), 25000, decision_data$prevalence),
          severity = ifelse(is.null(decision_data$severity), 0.5, decision_data$severity),
          objective = ifelse(is.null(decision_data$objective), "", decision_data$objective),
          notes = ifelse(is.null(decision_data$notes), "", decision_data$notes)
        )
      }
      
      # ---------------------------------------------------------
      # EXTRACT CRITERIA DATA
      # ---------------------------------------------------------
      criteria_data_list <- NULL
      if (!is.null(criteria_data)) {
        criteria_df <- NULL
        if (!is.null(criteria_data$df) && nrow(criteria_data$df) > 0) {
          criteria_df <- as.data.frame(criteria_data$df)
        }
        
        criteria_data_list <- list(
          df = criteria_df,
          notes = ifelse(is.null(criteria_data$notes), "", criteria_data$notes)
        )
      }
      
      # ---------------------------------------------------------
      # EXTRACT PERFORMANCE DATA
      # ---------------------------------------------------------
      performance_data_list <- NULL
      direct_scores_data <- NULL
      ahp_data <- NULL
      vf_data <- NULL
      
      if (!is.null(performance_data)) {
        performance_matrix <- NULL
        if (!is.null(performance_data$matrix)) {
          performance_matrix <- performance_data$matrix
        }
        
        intervention_scores <- NULL
        if (!is.null(performance_data$intervention)) {
          intervention_scores <- as.list(performance_data$intervention)
        }
        
        comparator_scores <- NULL
        if (!is.null(performance_data$comparators)) {
          comparator_scores <- list()
          for (comp in names(performance_data$comparators)) {
            comparator_scores[[comp]] <- as.list(performance_data$comparators[[comp]])
          }
        }
        
        ahp_scores_matrix <- NULL
        if (!is.null(performance_data$ahp_scores_matrix)) {
          ahp_scores_matrix <- performance_data$ahp_scores_matrix
        }
        
        ahp_matrices <- NULL
        if (!is.null(performance_data$ahp_matrices)) {
          ahp_matrices <- performance_data$ahp_matrices
        }
        
        ahp_consistency <- NULL
        if (!is.null(performance_data$ahp_consistency)) {
          ahp_consistency <- performance_data$ahp_consistency
        }
        
        value_functions <- NULL
        if (!is.null(performance_data$value_functions)) {
          value_functions <- performance_data$value_functions
        }
        
        performance_data_list <- list(
          scoring_method = ifelse(is.null(performance_data$scoring_method), "", performance_data$scoring_method),
          scoring_justification = ifelse(is.null(performance_data$scoring_justification), "", performance_data$scoring_justification),
          matrix = performance_matrix,
          intervention = intervention_scores,
          comparators = comparator_scores,
          ahp_scores_matrix = ahp_scores_matrix,
          ahp_matrices = ahp_matrices,
          ahp_consistency = ahp_consistency,
          value_functions = value_functions,
          direct_min = ifelse(is.null(performance_data$direct_min), 0, performance_data$direct_min),
          direct_max = ifelse(is.null(performance_data$direct_max), 100, performance_data$direct_max),
          notes = ifelse(is.null(performance_data$notes), "", performance_data$notes)
        )
        
        # Extract direct rating scores
        if (!is.null(performance_data$scoring_method) && 
            performance_data$scoring_method == "direct" &&
            !is.null(decision_data) && !is.null(criteria_data)) {
          
          direct_scores_data <- extract_direct_scores_for_report(
            performance_data, decision_data, criteria_data
          )
        }
        
        # Extract AHP data
        if (!is.null(performance_data$scoring_method) && 
            performance_data$scoring_method == "ahp") {
          
          ahp_data <- list(
            scores = ahp_scores_matrix,
            matrices = ahp_matrices,
            consistency = ahp_consistency
          )
        }
        
        # Extract value function data
        if (!is.null(performance_data$scoring_method) && 
            performance_data$scoring_method == "value_function" &&
            !is.null(performance_module)) {
          
          vf_data <- extract_value_function_scores_for_report(performance_module)
        }
      }
      
      # ---------------------------------------------------------
      # EXTRACT WEIGHTS DATA
      # ---------------------------------------------------------
      weights_data_list <- NULL
      if (!is.null(weights_data)) {
        
        # Get criteria names for weight mapping
        criteria_names <- NULL
        if (!is.null(criteria_data) && !is.null(criteria_data$df)) {
          criteria_names <- setNames(criteria_data$df$name, criteria_data$df$id)
        }
        
        weights_data_list <- list(
          method = ifelse(is.null(weights_data$method), "", weights_data$method),
          method_name = switch(weights_data$method,
                               "direct" = "Direct Rating",
                               "ahp" = "Analytical Hierarchy Process (AHP)",
                               "swing" = "Swing Weighting",
                               "Unknown"),
          justification = ifelse(is.null(weights_data$justification), "", weights_data$justification),
          weights = if (!is.null(weights_data$weights)) weights_data$weights else list(),
          direct = weights_data$direct,
          ahp = weights_data$ahp,
          swing = weights_data$swing,
          criteria_names = criteria_names,
          notes = ifelse(is.null(weights_data$notes), "", weights_data$notes)
        )
      }
      
      # ---------------------------------------------------------
      # EXTRACT AGGREGATE DATA - SILENT FAILURE
      # ---------------------------------------------------------
      aggregate_data_list <- NULL
      if (!is.null(aggregate_module)) {
        
        # Get aggregate scores - suppress warnings
        scores_df <- NULL
        tryCatch({
          scores_df <- aggregate_module$aggregate_scores()
          if (!is.null(scores_df) && nrow(scores_df) > 0) {
            scores_df <- as.data.frame(scores_df)
          }
        }, error = function(e) {
          # Silently fail
          NULL
        }, warning = function(w) {
          # Suppress warnings
          NULL
        })
        
        # Get detailed breakdown - suppress warnings
        breakdown_df <- NULL
        tryCatch({
          breakdown_df <- aggregate_module$detailed_breakdown()
          if (!is.null(breakdown_df) && nrow(breakdown_df) > 0) {
            breakdown_df <- as.data.frame(breakdown_df)
          }
        }, error = function(e) {
          # Silently fail
          NULL
        }, warning = function(w) {
          # Suppress warnings
          NULL
        })
        
        # Get notes
        notes <- ""
        if (!is.null(aggregate_data) && !is.null(aggregate_data$notes)) {
          notes <- aggregate_data$notes
        }
        
        # Get top alternative - suppress warnings
        top_alt <- NULL
        tryCatch({
          if (!is.null(aggregate_module$utilities$get_top_alternative)) {
            top_alt <- aggregate_module$utilities$get_top_alternative()
          }
        }, error = function(e) {
          # Silently fail
          NULL
        }, warning = function(w) {
          # Suppress warnings
          NULL
        })
        
        # Check if we have valid data
        has_data <- !is.null(scores_df) && nrow(scores_df) > 0
        
        aggregate_data_list <- list(
          scores = scores_df,
          breakdown = breakdown_df,
          notes = notes,
          top_alternative = top_alt,
          has_data = has_data,
          n_alternatives = if (!is.null(scores_df)) nrow(scores_df) else 0,
          n_criteria = if (!is.null(breakdown_df)) length(unique(breakdown_df$criterion)) else 0
        )
      }
      
      # ---------------------------------------------------------
      # EXTRACT UNCERTAINTY DATA - WITH PROPER ESCAPING
      # ---------------------------------------------------------
      uncertainty_data_list <- NULL
      if (!is.null(uncertainty_module)) {
        
        # Get uncertainty dataframe
        uncertainty_df <- NULL
        tryCatch({
          if (!is.null(uncertainty_module$utilities$get_uncertainty_for_report)) {
            uncertainty_df <- uncertainty_module$utilities$get_uncertainty_for_report()
          } else {
            uncertainty_df <- uncertainty_module$utilities$get_uncertainty_df()
          }
          
          if (!is.null(uncertainty_df) && nrow(uncertainty_df) > 0) {
            # Convert to plain data frame
            uncertainty_df <- as.data.frame(uncertainty_df)
            
            # Clean all string columns to prevent R Markdown errors
            for (col in names(uncertainty_df)) {
              if (is.character(uncertainty_df[[col]])) {
                # Replace newlines with spaces
                uncertainty_df[[col]] <- gsub("\n", " ", uncertainty_df[[col]], fixed = TRUE)
                uncertainty_df[[col]] <- gsub("\r", " ", uncertainty_df[[col]], fixed = TRUE)
              }
            }
          } else {
            uncertainty_df <- NULL
          }
        }, error = function(e) {
          uncertainty_df <- NULL
        })
        
        # Get uncertainty notes
        uncertainty_notes <- ""
        tryCatch({
          notes <- uncertainty_module$utilities$get_uncertainty_notes()
          if (!is.null(notes) && notes != "") {
            # Clean notes
            notes <- gsub("\n", " ", notes, fixed = TRUE)
            notes <- gsub("\r", " ", notes, fixed = TRUE)
            notes <- gsub("'", "\\\\'", notes, fixed = TRUE)
            notes <- gsub('"', '\\\\"', notes, fixed = TRUE)
            uncertainty_notes <- notes
          }
        }, error = function(e) {
          uncertainty_notes <- ""
        })
        
        uncertainty_data_list <- list(
          df = uncertainty_df,
          notes = uncertainty_notes,
          has_data = !is.null(uncertainty_df) && nrow(uncertainty_df) > 0,
          count = if (!is.null(uncertainty_df)) nrow(uncertainty_df) else 0
        )
      }
      
      # Return all extracted data as a single list
      list(
        decision = decision_data_list,
        criteria = criteria_data_list,
        performance = performance_data_list,
        direct_scores = direct_scores_data,
        ahp = ahp_data,
        vf = vf_data,
        weights = weights_data_list,
        aggregate = aggregate_data_list,
        uncertainty = uncertainty_data_list
      )
    })  # End of isolate
    
    # Now build the RMD content using the extracted static data
    temp_dir <- tempdir()
    temp_rmd  <- file.path(temp_dir, "report.Rmd")
    temp_docx <- file.path(temp_dir, filename)
    
    rmd_content <- create_word_rmd_content(extracted_data)
    
    writeLines(rmd_content, temp_rmd)
    
    showNotification("Creating Word document...", type = "message", duration = NULL)
    
    result <- tryCatch({
      rmarkdown::render(
        input = temp_rmd,
        output_file = temp_docx,
        output_format = "word_document",
        quiet = TRUE
      )
    }, error = function(e) {
      showNotification(paste("Error generating report:", e$message), 
                       type = "error", duration = 10)
      return(NULL)
    })
    
    if (is.null(result)) return(NULL)
    return(temp_docx)
  }
  
  # ---------------------------------------------------------
  # HELPER FUNCTIONS FOR DATA EXTRACTION (NON-REACTIVE)
  # ---------------------------------------------------------
  
  #' Extract Direct Rating Scores (Non-reactive version)
  extract_direct_scores_for_report <- function(performance_data, decision_data, criteria_data) {
    if (is.null(performance_data$intervention) && is.null(performance_data$comparators)) {
      return(NULL)
    }
    
    criteria_names <- if (!is.null(criteria_data$df)) {
      criteria_data$df$name
    } else {
      NULL
    }
    
    if (is.null(criteria_names) || length(criteria_names) == 0) {
      return(NULL)
    }
    
    intervention_name <- if (!is.null(decision_data$intervention) && decision_data$intervention != '') {
      decision_data$intervention
    } else {
      'Intervention'
    }
    
    comparators <- character(0)
    if (!is.null(decision_data$comparators) && decision_data$comparators != '') {
      comparators <- unlist(strsplit(decision_data$comparators, '\n'))
      comparators <- comparators[comparators != '']
    }
    
    results <- list()
    
    if (!is.null(performance_data$intervention) && length(performance_data$intervention) > 0) {
      intervention_scores <- data.frame(
        Criterion = criteria_names[1:min(length(criteria_names), length(performance_data$intervention))],
        Score = as.numeric(unlist(performance_data$intervention))[1:min(length(criteria_names), length(performance_data$intervention))],
        stringsAsFactors = FALSE
      )
      results[[intervention_name]] <- intervention_scores
    }
    
    if (!is.null(performance_data$comparators) && length(performance_data$comparators) > 0) {
      for (comp in comparators) {
        if (comp %in% names(performance_data$comparators)) {
          comp_scores <- data.frame(
            Criterion = criteria_names[1:min(length(criteria_names), length(performance_data$comparators[[comp]]))],
            Score = as.numeric(unlist(performance_data$comparators[[comp]]))[1:min(length(criteria_names), length(performance_data$comparators[[comp]]))],
            stringsAsFactors = FALSE
          )
          results[[comp]] <- comp_scores
        }
      }
    }
    
    return(results)
  }
  
  #' Extract Value Function Scores (Non-reactive version)
  extract_value_function_scores_for_report <- function(performance_module) {
    if (is.null(performance_module)) return(NULL)
    
    tryCatch({
      if (!is.null(performance_module$value_function_module)) {
        vf_module <- performance_module$value_function_module()
        
        if (!is.null(vf_module) && !is.null(vf_module$utilities$get_value_function_df)) {
          vf_df <- vf_module$utilities$get_value_function_df()
          
          if (!is.null(vf_df) && nrow(vf_df) > 0) {
            return(vf_df)
          }
        }
      }
      
      if (!is.null(performance_module$data$value_scores) && 
          length(performance_module$data$value_scores) > 0) {
        return(performance_module$data$value_scores)
      }
      
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # ---------------------------------------------------------
  # RMD CONTENT CREATION
  # ---------------------------------------------------------
  
  create_word_rmd_content <- function(extracted_data) {
    
    # Extract all data from the list
    decision_data <- extracted_data$decision
    criteria_data <- extracted_data$criteria
    performance_data <- extracted_data$performance
    direct_scores_list <- extracted_data$direct_scores
    ahp_data <- extracted_data$ahp
    vf_data <- extracted_data$vf
    weights_data <- extracted_data$weights
    aggregate_data <- extracted_data$aggregate
    uncertainty_data <- extracted_data$uncertainty
    
    rmd <- c(
      "---",
      "title: 'Multi-Criteria Decision Analysis Report'",
      "author: 'Decision Support Tool'",
      paste0("date: '", Sys.Date(), "'"),
      "output: word_document",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6)",
      "",
      "# Load required libraries",
      "library(knitr)",
      "library(dplyr)",
      "library(tidyr)",
      "library(ggplot2)",
      "library(scales)",
      "library(RColorBrewer)",
      "",
      "# Set global theme for ggplot",
      "theme_set(theme_minimal() +",
      "  theme(",
      "    plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),",
      "    plot.subtitle = element_text(hjust = 0.5, size = 11),",
      "    axis.text.x = element_text(angle = 45, hjust = 1)",
      "  ))",
      "```",
      "",
      "# Executive Summary",
      "",
      "This report presents the results of a Multi-Criteria Decision Analysis (MCDA) conducted using the Decision Support Tool.",
      "",
      paste0("**Report Generated:** ", format(Sys.time(), '%Y-%m-%d %H:%M:%S')),
      "",
      "---",
      "",
      "# 1. Decision Problem",
      ""
    )
    
    # ---------------------------------------------------------
    # DECISION PROBLEM
    # ---------------------------------------------------------
    
    if (!is.null(decision_data)) {
      
      comparators_display <- gsub("\n", ", ", decision_data$comparators)
      outcomes_display <- gsub("\n", ", ", decision_data$outcomes)
      
      rmd <- c(rmd,
               "## 1.1 PICO Framework",
               "",
               "```{r pico-section}",
               "pico_table <- data.frame(",
               "  Component = c('Population', 'Intervention', 'Comparators', 'Outcomes', 'Time Frame'),",
               "  Description = c(",
               paste0("    '", ifelse(decision_data$population == "", "Not specified", gsub("'", "\\\\'", decision_data$population)), "',"),
               paste0("    '", ifelse(decision_data$intervention == "", "Not specified", gsub("'", "\\\\'", decision_data$intervention)), "',"),
               paste0("    '", ifelse(decision_data$comparators == "", "Not specified", gsub("'", "\\\\'", comparators_display)), "',"),
               paste0("    '", ifelse(decision_data$outcomes == "", "Not specified", gsub("'", "\\\\'", outcomes_display)), "',"),
               paste0("    '", ifelse(decision_data$time_frame == "", "Not specified", gsub("'", "\\\\'", decision_data$time_frame)), "'"),
               "  ),",
               "  stringsAsFactors = FALSE",
               ")",
               "",
               "knitr::kable(pico_table, caption = 'PICO Framework', booktabs = TRUE)",
               "```",
               
               "## 1.2 Decision Context",
               "",
               "```{r context}",
               "context_data <- data.frame(",
               "  Parameter = c('Incidence per year', 'Prevalence per year', 'Disease Severity'),",
               "  Value = c(",
               paste0("    '", format(decision_data$incidence, big.mark = ","), "',"),
               paste0("    '", format(decision_data$prevalence, big.mark = ","), "',"),
               paste0("    '", decision_data$severity, "'"),
               "  ),",
               "  stringsAsFactors = FALSE",
               ")",
               "",
               "knitr::kable(context_data, caption = 'Decision Context Parameters', booktabs = TRUE)",
               "```",
               
               "## 1.3 Objective",
               "",
               "```{r objective}",
               "cat('", ifelse(decision_data$objective == "", "No objective specified", gsub("'", "\\\\'", gsub("\n", "\\n", decision_data$objective))), "')",
               "```",
               
               "## 1.4 Deliberation Notes",
               "",
               "```{r decision-notes}",
               "cat('", ifelse(decision_data$notes == "", "No notes recorded", gsub("'", "\\\\'", gsub("\n", "\\n", decision_data$notes))), "')",
               "```"
      )
      
    } else {
      rmd <- c(rmd, "*No decision problem data available*")
    }
    
    # ---------------------------------------------------------
    # CRITERIA DEFINITION
    # ---------------------------------------------------------
    
    rmd <- c(rmd,
             "",
             "---",
             "",
             "# 2. Criteria Definition",
             "")
    
    if (!is.null(criteria_data) && !is.null(criteria_data$df) && nrow(criteria_data$df) > 0) {
      
      criteria_df <- criteria_data$df
      criteria_notes <- ifelse(is.null(criteria_data$notes), "", criteria_data$notes)
      
      parent_names <- sapply(1:nrow(criteria_df), function(i) {
        id <- criteria_df$parent_id[i]
        if (id == 0) {
          return("None (Top Level)")
        }
        parent_row <- criteria_df[criteria_df$id == id, ]
        if (nrow(parent_row) > 0) {
          return(parent_row$name[1])
        } else {
          return("Unknown")
        }
      })
      
      rmd <- c(rmd,
               "## 2.1 Criteria Table with Definitions",
               "",
               "```{r criteria-table}",
               "display_df <- data.frame(",
               "  `Criterion Name` = c(",
               paste0("    '", paste(gsub("'", "\\\\'", criteria_df$name), collapse = "', '"), "'"),
               "  ),",
               "  `Hierarchy Level` = c(",
               paste0("    ", paste(criteria_df$level, collapse = ", ")),
               "  ),",
               "  `Parent Criterion` = c(",
               paste0("    '", paste(gsub("'", "\\\\'", parent_names), collapse = "', '"), "'"),
               "  ),",
               "  `Definition` = c(",
               paste0("    '", paste(gsub("'", "\\\\'", criteria_df$definition), collapse = "', '"), "'"),
               "  ),",
               "  stringsAsFactors = FALSE, check.names = FALSE",
               ")",
               "",
               "display_df <- display_df[order(display_df$`Hierarchy Level`, display_df$`Criterion Name`), ]",
               "",
               "knitr::kable(display_df, caption = 'Criteria Definitions and Hierarchy',",
               "             align = c('l', 'c', 'l', 'l'),",
               "             booktabs = TRUE, format = 'pipe')",
               "```",
               "",
               "## 2.2 Deliberation Notes",
               "",
               "```{r criteria-notes}",
               "cat('", ifelse(criteria_notes == "", "No notes recorded for criteria definition", 
                               gsub("'", "\\\\'", gsub("\n", "\\n", criteria_notes))), "')",
               "```")
      
    } else {
      rmd <- c(rmd, "*No criteria data available*")
    }
    
    # ---------------------------------------------------------
    # PERFORMANCE ASSESSMENT
    # ---------------------------------------------------------
    
    rmd <- c(rmd,
             "",
             "---",
             "",
             "# 3. Performance Assessment",
             "")
    
    if (!is.null(performance_data)) {
      
      method_name <- switch(performance_data$scoring_method,
                            "direct" = "Direct Rating",
                            "ahp" = "Analytical Hierarchy Process (AHP)",
                            "value_function" = "Value Function",
                            "Unknown")
      
      scoring_justification <- ifelse(is.null(performance_data$scoring_justification) || 
                                        performance_data$scoring_justification == "",
                                      "No justification provided",
                                      performance_data$scoring_justification)
      
      rmd <- c(rmd,
               "## 3.1 Scoring Method and Justification",
               "",
               "```{r scoring-method}",
               paste0("cat('**Method Used:** ", method_name, "')"),
               "",
               paste0("cat('\\n\\n**Justification:**\\n", 
                      gsub("'", "\\\\'", gsub("\n", "\\n", scoring_justification)), "')"),
               "```",
               "",
               "## 3.2 Qualitative Performance Matrix",
               "")
      
      if (!is.null(performance_data$matrix) && nrow(performance_data$matrix) > 0) {
        
        matrix_data <- as.data.frame(performance_data$matrix)
        options <- rownames(matrix_data)
        
        rmd <- c(rmd,
                 "```{r performance-matrix}",
                 "matrix_df <- data.frame(",
                 "  Option = c(",
                 paste0("    '", paste(gsub("'", "\\\\'", options), collapse = "', '"), "'"),
                 "  ),",
                 "  stringsAsFactors = FALSE",
                 ")",
                 "")
        
        for (col in colnames(matrix_data)) {
          col_data <- matrix_data[, col]
          col_data_clean <- gsub("'", "\\\\'", as.character(col_data))
          rmd <- c(rmd,
                   "matrix_df$`", col, "` <- c(",
                   paste0("  '", paste(col_data_clean, collapse = "', '"), "'"),
                   ")")
        }
        
        rmd <- c(rmd,
                 "",
                 "knitr::kable(matrix_df, caption = 'Performance Matrix (Qualitative Data)',",
                 "             booktabs = TRUE, align = c('l', rep('c', ncol(matrix_df)-1)))",
                 "```")
      } else {
        rmd <- c(rmd, "```{r performance-matrix-empty}\ncat('*No performance matrix data available*')\n```")
      }
      
      # Direct Rating Section
      if (!is.null(performance_data$scoring_method) && 
          performance_data$scoring_method == "direct" &&
          !is.null(direct_scores_list) && length(direct_scores_list) > 0) {
        
        rmd <- c(rmd,
                 "",
                 "## 3.3 Direct Rating Scores",
                 "",
                 "```{r direct-scores}",
                 "direct_scores_list <- list()",
                 "")
        
        for (option in names(direct_scores_list)) {
          scores_df <- direct_scores_list[[option]]
          rmd <- c(rmd,
                   paste0("direct_scores_list[['", gsub("'", "\\\\'", option), "']] <- data.frame("),
                   "  Criterion = c(",
                   paste0("    '", paste(gsub("'", "\\\\'", scores_df$Criterion), collapse = "', '"), "'"),
                   "  ),",
                   "  Score = c(",
                   paste0("    ", paste(scores_df$Score, collapse = ", ")),
                   "  ),",
                   "  stringsAsFactors = FALSE",
                   ")",
                   "")
        }
        
        rmd <- c(rmd,
                 "all_scores <- data.frame()",
                 "for (option in names(direct_scores_list)) {",
                 "  option_scores <- direct_scores_list[[option]]",
                 "  option_scores$Option <- option",
                 "  all_scores <- rbind(all_scores, option_scores)",
                 "}",
                 "",
                 "scores_wide <- all_scores %>%",
                 "  dplyr::select(Option, Criterion, Score) %>%",
                 "  tidyr::pivot_wider(names_from = Criterion, values_from = Score, values_fill = NA)",
                 "",
                 "numeric_cols <- sapply(scores_wide, is.numeric)",
                 "scores_wide$Total_Score <- rowSums(scores_wide[, numeric_cols, drop = FALSE], na.rm = TRUE)",
                 "names(scores_wide)[names(scores_wide) == 'Total_Score'] <- 'Total Score'",
                 "",
                 "knitr::kable(scores_wide, caption = 'Direct Rating Performance Scores',",
                 "             booktabs = TRUE, digits = 1)",
                 "```")
      }
      
      # AHP Section
      if (!is.null(performance_data$scoring_method) && 
          performance_data$scoring_method == "ahp" &&
          !is.null(ahp_data) && !is.null(ahp_data$scores)) {
        
        rmd <- c(rmd,
                 "",
                 "## 3.3 AHP Performance Scores",
                 "",
                 "```{r ahp-scores}",
                 "# AHP Scores Matrix",
                 "ahp_scores_matrix <- matrix(",
                 "  c(", paste(c(t(ahp_data$scores)), collapse = ", "), "),",
                 "  nrow = ", nrow(ahp_data$scores), ",",
                 "  ncol = ", ncol(ahp_data$scores), ",",
                 "  dimnames = list(",
                 "    c(", paste0("'", paste(gsub("'", "\\\\'", rownames(ahp_data$scores)), collapse = "', '"), "'"), "),",
                 "    c(", paste0("'", paste(gsub("'", "\\\\'", colnames(ahp_data$scores)), collapse = "', '"), "'"), ")",
                 "  )",
                 ")",
                 "",
                 "ahp_scores_df <- as.data.frame(ahp_scores_matrix)",
                 "ahp_scores_df <- cbind('Alternative' = rownames(ahp_scores_df), ahp_scores_df)",
                 "",
                 "total_scores <- rowSums(ahp_scores_matrix, na.rm = TRUE)",
                 "ahp_scores_df$'Total Score' <- total_scores",
                 "ahp_scores_df$'Percentage' <- paste0(",
                 "  round(total_scores / sum(total_scores, na.rm = TRUE) * 100, 1), '%')",
                 "ahp_scores_df$'Rank' <- rank(-total_scores, ties.method = 'min')",
                 "",
                 "ahp_scores_df <- ahp_scores_df[order(ahp_scores_df$Rank), ]",
                 "",
                 "knitr::kable(ahp_scores_df, caption = 'AHP Performance Scores and Ranking',",
                 "             booktabs = TRUE, digits = 2)",
                 "```")
      }
      
      # Value Function Section
      if (!is.null(performance_data$scoring_method) && 
          performance_data$scoring_method == "value_function" &&
          !is.null(vf_data) && nrow(vf_data) > 0) {
        
        value_functions <- performance_data$value_functions
        
        rmd <- c(rmd,
                 "",
                 "## 3.3 Value Functions",
                 "",
                 "### 3.3.1 Value Function Definitions",
                 "",
                 "```{r value-function-definitions}",
                 "func_df <- data.frame(",
                 "  Criterion = character(),",
                 "  `Function Type` = character(),",
                 "  `Function Definition` = character(),",
                 "  stringsAsFactors = FALSE, check.names = FALSE",
                 ")",
                 "")
        
        if (!is.null(value_functions) && length(value_functions) > 0) {
          for (criterion_id in names(value_functions)) {
            func <- value_functions[[criterion_id]]
            
            criterion_name <- "Unknown"
            if (!is.null(criteria_data$df)) {
              match_idx <- which(criteria_data$df$id == as.integer(criterion_id))
              if (length(match_idx) > 0) {
                criterion_name <- criteria_data$df$name[match_idx[1]]
              }
            }
            
            if (func$type == "linear") {
              func_text <- paste0('f(x) = ', func$params$m, 'x + ', func$params$b)
              func_type <- 'Linear'
            } else {
              func_text <- paste0('f(x) = ', func$params$a, 'x² + ', 
                                  func$params$b, 'x + ', func$params$c)
              func_type <- 'Quadratic'
            }
            
            rmd <- c(rmd,
                     "func_df <- rbind(func_df, data.frame(",
                     paste0("  Criterion = '", gsub("'", "\\\\'", criterion_name), "',"),
                     paste0("  `Function Type` = '", func_type, "',"),
                     paste0("  `Function Definition` = '", gsub("'", "\\\\'", func_text), "',"),
                     "  stringsAsFactors = FALSE, check.names = FALSE",
                     "))",
                     "")
          }
        }
        
        rmd <- c(rmd,
                 "knitr::kable(func_df, caption = 'Value Function Definitions', booktabs = TRUE)",
                 "```",
                 "",
                 "### 3.3.2 Value Function Performance Scores",
                 "",
                 "```{r value-function-scores}",
                 "vf_scores <- data.frame(",
                 "  option = c(", paste0("'", paste(gsub("'", "\\\\'", vf_data$option), collapse = "', '"), "'"), "),",
                 "  criterion = c(", paste0("'", paste(gsub("'", "\\\\'", as.character(vf_data$criterion)), collapse = "', '"), "'"), "),",
                 "  raw_value = c(", paste(vf_data$raw_value, collapse = ", "), "),",
                 "  score = c(", paste(vf_data$score, collapse = ", "), ")",
                 ")",
                 "",
                 "vf_display <- vf_scores %>%",
                 "  dplyr::select(option, criterion, score) %>%",
                 "  tidyr::pivot_wider(names_from = criterion, values_from = score, values_fill = 0)",
                 "",
                 "numeric_cols <- sapply(vf_display, is.numeric)",
                 "vf_display$Total_Score <- rowSums(vf_display[, numeric_cols, drop = FALSE], na.rm = TRUE)",
                 "names(vf_display)[names(vf_display) == 'Total_Score'] <- 'Total Score'",
                 "",
                 "knitr::kable(vf_display, caption = 'Value Function Transformed Scores',",
                 "             booktabs = TRUE, digits = 1)",
                 "```")
      }
      
      # Performance Assessment Notes
      rmd <- c(rmd,
               "",
               "## 3.4 Performance Assessment Notes",
               "",
               "```{r performance-notes}",
               "cat('", ifelse(is.null(performance_data$notes) || performance_data$notes == "", 
                               "No notes recorded for performance assessment",
                               gsub("'", "\\\\'", gsub("\n", "\\n", performance_data$notes))), "')",
               "```")
      
    } else {
      rmd <- c(rmd, "*No performance assessment data available*")
    }
    
    # ---------------------------------------------------------
    # WEIGHTS ASSIGNMENT
    # ---------------------------------------------------------
    
    rmd <- c(rmd,
             "",
             "---",
             "",
             "# 4. Weight Assignment",
             "")
    
    if (!is.null(weights_data) && !is.null(weights_data$method) && weights_data$method != "") {
      
      method_name <- weights_data$method_name
      justification <- ifelse(is.null(weights_data$justification) || weights_data$justification == "",
                              "No justification provided.",
                              weights_data$justification)
      
      rmd <- c(rmd,
               "## 4.1 Weighting Method and Justification",
               "",
               "```{r weight-method}",
               paste0("cat('**Method Used:** ", method_name, "')"),
               "",
               paste0("cat('\\n\\n**Justification:**\\n", 
                      gsub("'", "\\\\'", gsub("\n", "\\n", justification)), "')"),
               "```",
               "")
      
      # Weight Summary Table
      if (!is.null(weights_data$weights) && length(weights_data$weights) > 0 && 
          !is.null(criteria_data) && !is.null(criteria_data$df)) {
        
        # Create weight mapping
        weight_values <- sapply(criteria_data$df$id, function(id) {
          w <- weights_data$weights[[as.character(id)]]
          ifelse(is.null(w), 0, w)
        })
        
        weight_df <- data.frame(
          Criterion = criteria_data$df$name,
          Weight = round(weight_values, 2),
          Percentage = paste0(round(weight_values, 1), '%'),
          stringsAsFactors = FALSE
        )
        
        weight_df <- weight_df[order(-weight_df$Weight), ]
        weight_df$Rank <- 1:nrow(weight_df)
        rownames(weight_df) <- NULL
        
        rmd <- c(rmd,
                 "## 4.2 Weight Summary Table",
                 "",
                 "```{r weight-summary}",
                 "weight_df <- data.frame(",
                 "  Rank = c(", paste(weight_df$Rank, collapse = ", "), "),",
                 "  Criterion = c(", paste0("'", paste(gsub("'", "\\\\'", weight_df$Criterion), collapse = "', '"), "'"), "),",
                 "  Weight = c(", paste(weight_df$Weight, collapse = ", "), "),",
                 "  Percentage = c(", paste0("'", weight_df$Percentage, "'", collapse = ", "), ")",
                 ")",
                 "",
                 "knitr::kable(weight_df, caption = 'Final Criteria Weights',",
                 "             booktabs = TRUE, digits = 1)",
                 "```",
                 "")
        
        # Weight visualization
        rmd <- c(rmd,
                 "## 4.3 Weight Visualization",
                 "",
                 "```{r weight-visualization, fig.height=6, fig.width=8}",
                 "weight_df$Criterion <- factor(weight_df$Criterion, levels = weight_df$Criterion[order(weight_df$Weight)])",
                 "",
                 "ggplot(weight_df, aes(x = Criterion, y = Weight, fill = Criterion)) +",
                 "  geom_bar(stat = 'identity') +",
                 "  geom_text(aes(label = Percentage), ",
                 "            hjust = -0.1, size = 3.5) +",
                 "  coord_flip() +",
                 "  labs(title = 'Criteria Weights',",
                 "       x = 'Criterion', y = 'Weight (%)') +",
                 "  theme_minimal() +",
                 "  theme(",
                 "    plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),",
                 "    legend.position = 'none'",
                 "  ) +",
                 "  scale_fill_brewer(palette = 'Set3') +",
                 "  ylim(0, max(weight_df$Weight) * 1.15)",
                 "```",
                 "")
      }
      
      # Method-specific details
      rmd <- c(rmd, "## 4.4 Method-Specific Details", "")
      
      if (weights_data$method == "direct" && !is.null(weights_data$direct$total_points)) {
        rmd <- c(rmd,
                 "```{r direct-details}",
                 "cat('**Direct Rating Method**\\n')",
                 paste0("cat('Total Points Distributed: ", weights_data$direct$total_points, "\\n')"),
                 "```",
                 "")
      }
      
      if (weights_data$method == "ahp" && !is.null(weights_data$ahp$consistency)) {
        cons <- weights_data$ahp$consistency
        n <- length(weights_data$ahp$scores)
        RI_VALUES <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)
        
        rmd <- c(rmd,
                 "```{r ahp-consistency-details}",
                 "cat('**AHP Consistency Analysis**\\n\\n')",
                 sprintf("cat('Consistency Ratio (CR): %.3f\\n', ", cons$CR),
                 sprintf("    '%s')\n", ifelse(cons$CR < 0.1, "(Consistent ✓)", "(Inconsistent ✗)")),
                 sprintf("cat('Consistency Index (CI): %.3f\\n')\n", cons$CI),
                 sprintf("cat('Random Index (RI): %.3f\\n')\n", RI_VALUES[n]),
                 sprintf("cat('Maximum Eigenvalue (λmax): %.3f\\n')\n", cons$lambda_max),
                 "```",
                 "")
      }
      
      if (weights_data$method == "swing" && !is.null(weights_data$swing$first_criterion) && 
          !is.null(criteria_data) && !is.null(criteria_data$df)) {
        
        first_criterion_name <- criteria_data$df$name[criteria_data$df$id == weights_data$swing$first_criterion]
        
        rmd <- c(rmd,
                 "```{r swing-details}",
                 "cat('**Swing Weighting Details**\\n\\n')",
                 paste0("cat('First improved criterion: ", first_criterion_name, " (100 points)\\n')"),
                 "```",
                 "")
      }
      
      if (!is.null(weights_data$notes) && weights_data$notes != "") {
        rmd <- c(rmd,
                 "## 4.5 Deliberation Notes",
                 "",
                 "```{r weight-notes}",
                 "cat('", gsub("'", "\\\\'", gsub("\n", "\\n", weights_data$notes)), "')",
                 "```",
                 "")
      }
      
    } else {
      rmd <- c(rmd, "*No weight assignment data available*")
    }
    
    # ---------------------------------------------------------
    # AGGREGATE SCORES
    # ---------------------------------------------------------
    
    rmd <- c(rmd,
             "",
             "---",
             "",
             "# 5. Aggregate Scores",
             "")
    
    if (!is.null(aggregate_data) && !is.null(aggregate_data$has_data) && aggregate_data$has_data) {
      
      # Extract data frames for use in chunks
      scores_df <- aggregate_data$scores
      breakdown_df <- aggregate_data$breakdown
      
      # Executive summary of results
      rmd <- c(rmd,
               "## 5.1 Executive Summary",
               "",
               "```{r aggregate-executive}",
               paste0("cat('**Number of alternatives evaluated:** ", aggregate_data$n_alternatives, "\\n')"),
               paste0("cat('**Number of criteria included:** ", aggregate_data$n_criteria, "\\n')"),
               "")
      
      if (!is.null(aggregate_data$top_alternative)) {
        rmd <- c(rmd,
                 paste0("cat('**Top-ranked alternative:** ", aggregate_data$top_alternative, "\\n')"),
                 "")
      }
      
      rmd <- c(rmd,
               "```",
               "")
      
      # Final ranking table
      rmd <- c(rmd,
               "## 5.2 Final Ranking",
               "",
               "```{r aggregate-ranking}",
               "# Aggregate scores and ranking",
               "rank_df <- data.frame(",
               "  Rank = c(", paste(scores_df$rank, collapse = ", "), "),",
               "  Alternative = c(", paste0("'", paste(gsub("'", "\\\\'", scores_df$option), collapse = "', '"), "'"), "),",
               "  `Aggregate Score` = c(", paste(round(scores_df$aggregate_score, 3), collapse = ", "), "),",
               "  `Percentage Share` = c(", paste0("'", scores_df$percentage, "'", collapse = ", "), ")",
               ")",
               "",
               "# Order by rank",
               "rank_df <- rank_df[order(rank_df$Rank), ]",
               "rownames(rank_df) <- NULL",
               "",
               "knitr::kable(rank_df,",
               "             caption = 'Final Ranking of Alternatives',",
               "             booktabs = TRUE, digits = 3)",
               "```",
               "")
      
      # Bar chart of aggregate scores
      rmd <- c(rmd,
               "## 5.3 Aggregate Scores Visualization",
               "",
               "```{r aggregate-bar-chart, fig.height=5, fig.width=8}",
               "# Bar chart of aggregate scores",
               "scores_chart <- rank_df",
               "scores_chart$Alternative <- factor(scores_chart$Alternative, ",
               "                              levels = scores_chart$Alternative[order(scores_chart$Aggregate.Score)])",
               "",
               "ggplot(scores_chart, aes(x = Alternative, y = Aggregate.Score, fill = Alternative)) +",
               "  geom_bar(stat = 'identity') +",
               "  geom_text(aes(label = round(Aggregate.Score, 2)), ",
               "            vjust = -0.5, size = 4) +",
               "  labs(title = 'Aggregate Scores by Alternative',",
               "       x = 'Alternative', y = 'Aggregate Score') +",
               "  theme_minimal() +",
               "  theme(",
               "    plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),",
               "    axis.text.x = element_text(angle = 45, hjust = 1),",
               "    legend.position = 'none'",
               "  ) +",
               "  scale_fill_brewer(palette = 'Set2') +",
               "  ylim(0, max(scores_chart$Aggregate.Score) * 1.15)",
               "```",
               "")
      
      # Score Composition by Alternative (Stacked Bar Chart)
      if (!is.null(breakdown_df) && nrow(breakdown_df) > 0) {
        
        rmd <- c(rmd,
                 "## 5.4 Score Composition by Alternative",
                 "",
                 "```{r aggregate-composition, fig.height=6, fig.width=10}",
                 "# Prepare data for stacked bar chart",
                 "breakdown_data <- data.frame(",
                 "  Alternative = c(", paste0("'", paste(gsub("'", "\\\\'", breakdown_df$option), collapse = "', '"), "'"), "),",
                 "  Criterion = c(", paste0("'", paste(gsub("'", "\\\\'", breakdown_df$criterion), collapse = "', '"), "'"), "),",
                 "  WeightedScore = c(", paste(round(breakdown_df$weighted_score, 3), collapse = ", "), ")",
                 ")",
                 "",
                 "# Get unique criteria for color mapping",
                 "criteria <- unique(breakdown_data$Criterion)",
                 "n_criteria <- length(criteria)",
                 "",
                 "# Generate colors",
                 "if (n_criteria <= 12) {",
                 "  colors <- RColorBrewer::brewer.pal(max(3, n_criteria), 'Set3')",
                 "} else {",
                 "  colors <- colorRampPalette(RColorBrewer::brewer.pal(12, 'Set3'))(n_criteria)",
                 "}",
                 "",
                 "# Stacked bar chart",
                 "ggplot(breakdown_data, aes(x = Alternative, y = WeightedScore, fill = Criterion)) +",
                 "  geom_bar(stat = 'identity', position = 'stack') +",
                 "  labs(title = 'Score Composition by Alternative',",
                 "       subtitle = 'Stacked by Criterion Contribution',",
                 "       x = 'Alternative', y = 'Weighted Score') +",
                 "  scale_fill_manual(values = colors, name = 'Criterion') +",
                 "  theme_minimal() +",
                 "  theme(",
                 "    plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),",
                 "    plot.subtitle = element_text(hjust = 0.5, size = 11),",
                 "    axis.text.x = element_text(angle = 45, hjust = 1),",
                 "    legend.position = 'right',",
                 "    legend.text = element_text(size = 9)",
                 "  )",
                 "```",
                 "")
        
        # Radar Chart with scales - NO PLOTLY, ONLY GGPLOT2
        rmd <- c(rmd,
                 "## 5.5 Performance Profile (Radar Chart)",
                 "",
                 "```{r aggregate-radar, fig.height=7.5, fig.width=8.5}",
                 "# Prepare data for radar chart",
                 "radar_data <- breakdown_data %>%",
                 "  dplyr::group_by(Alternative, Criterion) %>%",
                 "  dplyr::summarise(WeightedScore = sum(WeightedScore), .groups = 'drop') %>%",
                 "  tidyr::pivot_wider(names_from = Criterion, values_from = WeightedScore, values_fill = 0)",
                 "",
                 "# Get criteria names",
                 "criteria_cols <- colnames(radar_data)[-1]",
                 "n_criteria <- length(criteria_cols)",
                 "",
                 "# Normalize scores to 0-1 scale for better visualization",
                 "radar_scaled <- radar_data",
                 "for (col in criteria_cols) {",
                 "  max_val <- max(radar_scaled[[col]])",
                 "  if (max_val > 0) {",
                 "    radar_scaled[[col]] <- radar_scaled[[col]] / max_val",
                 "  }",
                 "}",
                 "",
                 "# Reshape for ggplot",
                 "radar_long <- radar_scaled %>%",
                 "  tidyr::pivot_longer(cols = -Alternative, names_to = 'Criterion', values_to = 'Value')",
                 "",
                 "# Create coordinates for radar chart",
                 "angles <- seq(0, 2*pi, length.out = n_criteria + 1)[1:n_criteria]",
                 "radar_long$x <- cos(angles[match(radar_long$Criterion, criteria_cols)]) * radar_long$Value",
                 "radar_long$y <- sin(angles[match(radar_long$Criterion, criteria_cols)]) * radar_long$Value",
                 "",
                 "# Add starting point to close polygons",
                 "radar_close <- radar_long %>%",
                 "  dplyr::group_by(Alternative) %>%",
                 "  dplyr::filter(Criterion == criteria_cols[1]) %>%",
                 "  dplyr::mutate(Criterion = 'START')",
                 "",
                 "radar_long <- dplyr::bind_rows(radar_long, radar_close)",
                 "",
                 "# Create grid circles (0.25, 0.5, 0.75, 1.0)",
                 "grid_values <- seq(0.25, 1, 0.25)",
                 "grid_data <- data.frame()",
                 "for (r in grid_values) {",
                 "  circle_angles <- seq(0, 2*pi, length.out = 100)",
                 "  grid_data <- rbind(grid_data, data.frame(",
                 "    x = cos(circle_angles) * r,",
                 "    y = sin(circle_angles) * r,",
                 "    r = factor(r)",
                 "  ))",
                 "}",
                 "",
                 "# Create axis lines",
                 "axis_data <- data.frame()",
                 "for (i in 1:n_criteria) {",
                 "  axis_data <- rbind(axis_data, data.frame(",
                 "    x = c(0, cos(angles[i]) * 1),",
                 "    y = c(0, sin(angles[i]) * 1),",
                 "    criterion = criteria_cols[i]",
                 "  ))",
                 "}",
                 "",
                 "# Create the radar chart",
                 "ggplot() +",
                 "  # Add grid circles",
                 "  geom_path(data = grid_data, aes(x = x, y = y, group = r),",
                 "            color = 'gray80', size = 0.3, linetype = 'dotted') +",
                 "  # Add axis lines",
                 "  geom_path(data = axis_data, aes(x = x, y = y, group = criterion),",
                 "            color = 'gray70', size = 0.3, alpha = 0.8) +",
                 "  # Add radial scale labels",
                 "  annotate('text', x = 0, y = grid_values, label = paste0(grid_values * 100, '%'),",
                 "           size = 2.8, color = 'gray40', hjust = -0.2, vjust = -0.5) +",
                 "  # Add alternative polygons",
                 "  geom_polygon(data = radar_long, aes(x = x, y = y, group = Alternative, ",
                 "                                      fill = Alternative, color = Alternative),",
                 "               alpha = 0.2, size = 0.8) +",
                 "  # Add points at criteria",
                 "  geom_point(data = subset(radar_long, Criterion != 'START'), ",
                 "             aes(x = x, y = y, color = Alternative), size = 2.5) +",
                 "  # Add criteria labels",
                 "  annotate('text', x = cos(angles) * 1.15, y = sin(angles) * 1.15, ",
                 "           label = criteria_cols, size = 3.5, fontface = 'bold') +",
                 "  # Add center point",
                 "  geom_point(aes(x = 0, y = 0), color = 'gray50', size = 1) +",
                 "  coord_fixed() +",
                 "  labs(title = 'Performance Profile (Radar Chart)',",
                 "       subtitle = 'Normalized scores (0-1 scale)',",
                 "       x = '', y = '') +",
                 "  theme_void() +",
                 "  theme(",
                 "    plot.title = element_text(hjust = 0.5, size = 14, face = 'bold', margin = margin(b = 5)),",
                 "    plot.subtitle = element_text(hjust = 0.5, size = 10, color = 'gray40', margin = margin(b = 15)),",
                 "    legend.position = 'bottom',",
                 "    legend.title = element_blank(),",
                 "    legend.text = element_text(size = 10),",
                 "    legend.margin = margin(t = 10),",
                 "    panel.background = element_rect(fill = 'white', color = NA)",
                 "  ) +",
                 "  scale_fill_brewer(palette = 'Set2') +",
                 "  scale_color_brewer(palette = 'Set2')",
                 "```",
                 "")
      }
      
      # Detailed Score Breakdown Table
      if (!is.null(breakdown_df) && nrow(breakdown_df) > 0) {
        
        rmd <- c(rmd,
                 "## 5.6 Detailed Score Breakdown",
                 "",
                 "```{r aggregate-breakdown-table}",
                 "# Detailed breakdown table",
                 "breakdown_table <- data.frame(",
                 "  Alternative = c(", paste0("'", paste(gsub("'", "\\\\'", breakdown_df$option), collapse = "', '"), "'"), "),",
                 "  Criterion = c(", paste0("'", paste(gsub("'", "\\\\'", breakdown_df$criterion), collapse = "', '"), "'"), "),",
                 "  `Raw Score` = c(", paste(round(breakdown_df$score, 2), collapse = ", "), "),",
                 "  Weight = c(", paste(round(breakdown_df$weight, 3), collapse = ", "), "),",
                 "  `Weighted Score` = c(", paste(round(breakdown_df$weighted_score, 3), collapse = ", "), "),",
                 "  Contribution = c(", paste0("'", breakdown_df$contribution, "'", collapse = ", "), ")",
                 ")",
                 "",
                 "knitr::kable(breakdown_table,",
                 "             caption = 'Detailed Score Breakdown by Criterion and Alternative',",
                 "             booktabs = TRUE, digits = 3, format = 'pipe')",
                 "```",
                 "")
      }
      
      # Contribution Analysis
      if (!is.null(breakdown_df) && nrow(breakdown_df) > 0) {
        
        rmd <- c(rmd,
                 "## 5.7 Contribution Analysis",
                 "",
                 "```{r aggregate-contribution, fig.height=6, fig.width=8}",
                 "# Calculate contribution percentages by criterion across all alternatives",
                 "contribution_summary <- breakdown_data %>%",
                 "  dplyr::group_by(Criterion) %>%",
                 "  dplyr::summarise(TotalContribution = sum(WeightedScore, na.rm = TRUE)) %>%",
                 "  dplyr::ungroup() %>%",
                 "  dplyr::mutate(Percentage = TotalContribution / sum(TotalContribution) * 100) %>%",
                 "  dplyr::arrange(desc(Percentage))",
                 "",
                 "# Bar chart of criterion contributions",
                 "contribution_summary$Criterion <- factor(contribution_summary$Criterion, ",
                 "                                        levels = contribution_summary$Criterion[order(contribution_summary$Percentage)])",
                 "",
                 "ggplot(contribution_summary, aes(x = Criterion, y = Percentage, fill = Criterion)) +",
                 "  geom_bar(stat = 'identity') +",
                 "  geom_text(aes(label = paste0(round(Percentage, 1), '%')), ",
                 "            hjust = -0.1, size = 3.5) +",
                 "  coord_flip() +",
                 "  labs(title = 'Overall Contribution by Criterion',",
                 "       subtitle = 'Sum of weighted scores across all alternatives',",
                 "       x = 'Criterion', y = 'Contribution (%)') +",
                 "  theme_minimal() +",
                 "  theme(",
                 "    plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),",
                 "    plot.subtitle = element_text(hjust = 0.5, size = 11),",
                 "    legend.position = 'none'",
                 "  ) +",
                 "  scale_fill_brewer(palette = 'Set3') +",
                 "  ylim(0, max(contribution_summary$Percentage) * 1.15)",
                 "```",
                 "")
      }
      
      # Aggregate Notes
      if (!is.null(aggregate_data$notes) && aggregate_data$notes != "") {
        rmd <- c(rmd,
                 "## 5.8 Deliberation Notes",
                 "",
                 "```{r aggregate-notes}",
                 "cat('", gsub("'", "\\\\'", gsub("\n", "\\n", aggregate_data$notes)), "')",
                 "```",
                 "")
      }
      
    } else {
      rmd <- c(rmd, 
               "*No aggregate score data available. Please ensure both performance scores and weights have been calculated and saved.*")
    }
    
    # ---------------------------------------------------------
    # UNCERTAINTY ANALYSIS - FIXED VERSION
    # ---------------------------------------------------------
    
    rmd <- c(rmd,
             "",
             "---",
             "",
             "# 6. Uncertainty Analysis",
             "")
    
    if (!is.null(uncertainty_data) && !is.null(uncertainty_data$has_data) && uncertainty_data$has_data) {
      
      uncertainty_df <- uncertainty_data$df
      uncertainty_notes <- uncertainty_data$notes
      
      # Only proceed if we have data
      if (!is.null(uncertainty_df) && nrow(uncertainty_df) > 0) {
        
        # Calculate counts for summary
        n_uncertainties <- nrow(uncertainty_df)
        type_counts <- table(uncertainty_df$Type)
        n_sources <- length(unique(uncertainty_df$Source))
        
        # Executive summary
        rmd <- c(rmd,
                 "## 6.1 Executive Summary",
                 "",
                 "```{r uncertainty-executive}",
                 paste0("cat('**Total uncertainties identified:** ", n_uncertainties, "\\n')"),
                 "",
                 "# Count by type",
                 "cat('\\n**Breakdown by uncertainty type:**\\n')",
                 paste0("cat('  - ", names(type_counts), ": ", type_counts, "\\n', collapse = '')"),
                 "",
                 paste0("cat('\\n**Unique sources of uncertainty:** ", n_sources, "\\n')"),
                 "```",
                 "",
                 "## 6.2 Uncertainty Identification and Management",
                 "",
                 "```{r uncertainty-table}",
                 "# Format uncertainty table for display",
                 "uncertainty_display <- data.frame(",
                 "  `Type` = uncertainty_df$Type,",
                 "  `Source` = uncertainty_df$Source,",
                 "  `Definition` = uncertainty_df$Definition,",
                 "  `Impact` = uncertainty_df$Impact,",
                 "  `Mitigation` = uncertainty_df$Mitigation,",
                 "  stringsAsFactors = FALSE, check.names = FALSE",
                 ")",
                 "",
                 "knitr::kable(uncertainty_display,",
                 "             caption = 'Identified Uncertainties and Management Strategies',",
                 "             booktabs = TRUE, format = 'pipe', longtable = TRUE)",
                 "```",
                 "")
      
        
        # Uncertainty notes
        if (!is.null(uncertainty_notes) && uncertainty_notes != "" && uncertainty_notes != "''") {
          rmd <- c(rmd,
                   "## 6.3 Deliberation Notes",
                   "",
                   "```{r uncertainty-notes}",
                   paste0("cat('", uncertainty_notes, "')"),
                   "```",
                   "")
        }
        
      } else {
        rmd <- c(rmd, "*Uncertainty data frame is empty.*")
      }
      
    } else {
      rmd <- c(rmd,
               "*No uncertainty analysis data available. Use the Uncertainty tab to identify and document uncertainties in your decision analysis.*")
    }
    
    # ---------------------------------------------------------
    # CONCLUSIONS AND RECOMMENDATIONS
    # ---------------------------------------------------------
    
    rmd <- c(rmd,
             "",
             "---",
             "",
             "# 7. Conclusions and Recommendations",
             "")
    
    if (!is.null(aggregate_data) && !is.null(aggregate_data$has_data) && aggregate_data$has_data) {
      rmd <- c(rmd,
               "## 7.1 Summary of Findings",
               "",
               "```{r conclusions}",
               paste0("cat('Based on the MCDA results, **", aggregate_data$top_alternative, "** is the top-ranked alternative.\\n\\n')"),
               "",
               paste0("cat('The analysis considered ', ", aggregate_data$n_criteria, ", ' criteria and evaluated ', ",
                      aggregate_data$n_alternatives, ", ' alternatives.\\n\\n')"),
               "",
               "cat('The ranking of alternatives is as follows:\\n')",
               "for (i in 1:nrow(rank_df)) {",
               "  cat(sprintf('  %d. %s (Score: %.3f)\\n', ",
               "              rank_df$Rank[i], rank_df$Alternative[i], rank_df$Aggregate.Score[i]))",
               "}",
               "```",
               "")
    } else {
      rmd <- c(rmd, "*No conclusions available - aggregate scores not calculated.*")
    }
    
    # ---------------------------------------------------------
    # APPENDIX
    # ---------------------------------------------------------
    
    rmd <- c(rmd,
             "",
             "---",
             "",
             "# Appendix A: Technical Details",
             "",
             "```{r appendix}",
             "cat(paste0('Report generated with R version: ', R.version.string))",
             "cat('\\n')",
             "cat(paste0('Using rmarkdown package version: ', packageVersion('rmarkdown')))",
             "cat('\\n')",
             "cat('Output format: Word Document (.docx)')",
             "cat('\\n')",
             "cat('Tool Version: 1.0')",
             "cat('\\n')",
             "cat(paste0('Report generated on: ', Sys.time()))",
             "```",
             "",
             "---",
             "",
             "*End of Report*"
    )
    
    paste(rmd, collapse = "\n")
  }
  
  # ---------------------------------------------------------
  # DOWNLOAD HANDLER - WITH ERROR HANDLING
  # ---------------------------------------------------------
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("MCDA_Report_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      tryCatch({
        docx <- generate_word_report()
        
        if (!is.null(docx) && file.exists(docx)) {
          file.copy(docx, file, overwrite = TRUE)
          # Clean up temp file
          unlink(docx)
          showNotification("Report generated successfully!", type = "message", duration = 5)
        } else {
          # Create a simple error message file
          writeLines("Report generation failed. Please check that all required data is available.", file)
          showNotification("Report generation failed. Please check console for details.", type = "error", duration = 10)
        }
      }, error = function(e) {
        writeLines(paste("Error generating report:", e$message), file)
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  return(
    list(
      generate_word_report = generate_word_report
    )
  )
}