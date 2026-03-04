#' Decision Problem Module
#' 
#' This module handles the decision problem definition including PICO framework,
#' decision context, and objective. It provides reactive storage and summary outputs.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param config Configuration list with case-specific data and locked fields
#'
#' @return A list containing:
#' \itemize{
#'   \item{data - Reactive values for decision problem data}
#'   \item{ui_outputs - List of UI rendering functions}
#' }
#' 
#' @export
decision_problem_module <- function(input, output, session, config = NULL) {
  
  # Initialize reactive values for decision problem
  decision_data <- reactiveValues(
    population = "",
    intervention = "",
    comparators = "",
    outcomes = "",
    time_frame = "",
    incidence = 5000,
    prevalence = 25000,
    severity = 0.5,
    objective = "",
    notes = "",
    config_loaded = FALSE,
    config_name = "",
    config_description = "",
    config_version = "",
    config_author = "",
    config_date = "",
    locked_fields = list(
      population = FALSE,
      intervention = FALSE,
      comparators = FALSE,
      outcomes = FALSE,
      time_frame = FALSE,
      incidence = FALSE,
      prevalence = FALSE,
      severity = FALSE,
      objective = FALSE
    )
  )
  
  #### Decision Problem Logic ####
  
  # Set up observers for UI disabling/enabling
  observe({
    # Population field
    if (decision_data$locked_fields$population) {
      shinyjs::disable("population")
    } else {
      shinyjs::enable("population")
    }
  })
  
  observe({
    # Intervention field
    if (decision_data$locked_fields$intervention) {
      shinyjs::disable("intervention")
    } else {
      shinyjs::enable("intervention")
    }
  })
  
  observe({
    # Comparators field
    if (decision_data$locked_fields$comparators) {
      shinyjs::disable("comparators")
    } else {
      shinyjs::enable("comparators")
    }
  })
  
  observe({
    # Outcomes field
    if (decision_data$locked_fields$outcomes) {
      shinyjs::disable("outcomes")
    } else {
      shinyjs::enable("outcomes")
    }
  })
  
  observe({
    # Time frame field
    if (decision_data$locked_fields$time_frame) {
      shinyjs::disable("time_frame")
    } else {
      shinyjs::enable("time_frame")
    }
  })
  
  observe({
    # Incidence field
    if (decision_data$locked_fields$incidence) {
      shinyjs::disable("incidence")
    } else {
      shinyjs::enable("incidence")
    }
  })
  
  observe({
    # Prevalence field
    if (decision_data$locked_fields$prevalence) {
      shinyjs::disable("prevalence")
    } else {
      shinyjs::enable("prevalence")
    }
  })
  
  observe({
    # Severity field
    if (decision_data$locked_fields$severity) {
      shinyjs::disable("severity")
    } else {
      shinyjs::enable("severity")
    }
  })
  
  observe({
    # Objective field
    if (decision_data$locked_fields$objective) {
      shinyjs::disable("objective_definition_DP")
    } else {
      shinyjs::enable("objective_definition_DP")
    }
  })
  
  # Load configuration when available
  observe({
    req(!is.null(config))
    
    # Store configuration info
    decision_data$config_loaded <- TRUE
    decision_data$config_name <- config$config_name
    decision_data$config_description <- config$config_description
    decision_data$config_version <- config$config_version
    decision_data$config_author <- config$config_author
    decision_data$config_date <- config$config_date
    
    # Load decision problem data from config
    if (!is.null(config$decision_problem)) {
      # Set all values from config
      for (field in names(config$decision_problem)) {
        decision_data[[field]] <- config$decision_problem[[field]]
      }
    }
    
    # Update locked fields from config
    if (!is.null(config$locked_fields)) {
      for (field in names(config$locked_fields)) {
        decision_data$locked_fields[[field]] <- config$locked_fields[[field]]
      }
    }
    
    # Update UI inputs with config values
    updateTextInput(session, "population", value = decision_data$population)
    updateTextInput(session, "intervention", value = decision_data$intervention)
    updateTextAreaInput(session, "comparators", value = decision_data$comparators)
    updateTextAreaInput(session, "outcomes", value = decision_data$outcomes)
    updateTextInput(session, "time_frame", value = decision_data$time_frame)
    updateSliderInput(session, "incidence", value = decision_data$incidence)
    updateSliderInput(session, "prevalence", value = decision_data$prevalence)
    updateSliderInput(session, "severity", value = decision_data$severity)
    updateTextAreaInput(session, "objective_definition_DP", value = decision_data$objective)
  })
  
  #' Save Decision Parameters
  #' 
  #' Observes the save button and stores all decision parameters wrote by the user
  #' Only saves fields that are not locked
  observeEvent(input$save_decision, {
    # Save only non-locked fields
    if (!decision_data$locked_fields$population) {
      decision_data$population <- input$population
    }
    if (!decision_data$locked_fields$intervention) {
      decision_data$intervention <- input$intervention
    }
    if (!decision_data$locked_fields$comparators) {
      decision_data$comparators <- input$comparators
    }
    if (!decision_data$locked_fields$outcomes) {
      decision_data$outcomes <- input$outcomes
    }
    if (!decision_data$locked_fields$time_frame) {
      decision_data$time_frame <- input$time_frame
    }
    if (!decision_data$locked_fields$incidence) {
      decision_data$incidence <- input$incidence
    }
    if (!decision_data$locked_fields$prevalence) {
      decision_data$prevalence <- input$prevalence
    }
    if (!decision_data$locked_fields$severity) {
      decision_data$severity <- input$severity
    }
    if (!decision_data$locked_fields$objective) {
      decision_data$objective <- input$objective_definition_DP
    }
    
    # Notes are never locked
    decision_data$notes <- input$deliberation_notes_DP
    
    showNotification("Decision parameters saved", type = "message")
  })
  
  #' Generate PICO Summary Table
  #' 
  #' Creates a formatted table of PICO elements for reporting
  #' 
  #' @return renderTable function output
  generate_pico_table <- function() {
    renderTable({
      # Parse comparators (one per line)
      comps <- unlist(strsplit(decision_data$comparators, "\n"))
      comps <- comps[comps != ""]
      
      # Parse outcomes (one per line)
      outs <- unlist(strsplit(decision_data$outcomes, "\n"))
      outs <- outs[outs != ""]
      
      # Create data frame with proper formatting
      df <- data.frame(
        Element = c(
          "Population",
          "Intervention",
          if (length(comps) > 0) c("Comparator", rep("", length(comps) - 1)),
          if (length(outs) > 0) c("Outcome", rep("", length(outs) - 1)),
          "Time frame"
        ),
        Value = c(
          decision_data$population,
          decision_data$intervention,
          comps,
          outs,
          decision_data$time_frame
        )
      )
      
      return(df)
    }, striped = TRUE, hover = TRUE, width = "100%")
  }
  
  #' Generate Context Summary Table
  #' 
  #' Creates a table of decision context parameters for reporting
  #' 
  #' @return renderTable function output
  generate_context_table <- function() {
    renderTable({
      data.frame(
        Parameter = c("Incidence per year", "Prevalence per year", "Disease severity"),
        Value = c(
          format(decision_data$incidence, big.mark = ","),
          format(decision_data$prevalence, big.mark = ","),
          decision_data$severity
        )
      )
    }, striped = TRUE, hover = TRUE, width = "100%")
  }
  
  #' Render Objective Text
  #' 
  #' Displays the defined objective or a placeholder message
  #' 
  #' @return renderText function output
  render_objective <- function() {
    renderText({
      if (is.null(decision_data$objective) || decision_data$objective == "") {
        "No objective defined yet."
      } else {
        decision_data$objective
      }
    })
  }
  
  #' Render Notes Text
  #' 
  #' Displays the deliberation notes
  #' 
  #' @return renderText function output
  render_notes <- function() {
    renderText({
      decision_data$notes
    })
  }
  
  #' Render Configuration Info for Report
  #' 
  #' Displays information about the loaded configuration
  #' 
  #' @return renderUI function output
  render_config_info <- function() {
    renderUI({
      if (decision_data$config_loaded) {
        # Get locked fields count
        locked_count <- sum(unlist(decision_data$locked_fields))
        total_fields <- length(decision_data$locked_fields)
        
        tagList(
          h4("Active Case Study Configuration"),
          p(strong("Name:"), decision_data$config_name),
          p(strong("Description:"), decision_data$config_description),
          p(strong("Version:"), decision_data$config_version),
          p(strong("Author:"), decision_data$config_author),
          p(strong("Date:"), format(decision_data$config_date, "%Y-%m-%d")),
          p(em(paste0("Note: ", locked_count, " of ", total_fields, 
                      " fields are pre-configured and cannot be modified.")))
        )
      }
    })
  }
  
  # Initialize UI outputs
  output$pico_table <- generate_pico_table()
  output$context_table <- generate_context_table()
  output$objective_output <- render_objective()
  output$notes_output <- render_notes()
  output$config_info <- render_config_info()
  
  # Return module components
  return(
    list(
      data = decision_data,
      ui_outputs = list(
        pico_table = generate_pico_table,
        context_table = generate_context_table,
        objective_output = render_objective,
        notes_output = render_notes,
        config_info = render_config_info
      )
    )
  )
}