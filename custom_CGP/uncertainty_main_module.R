#' Uncertainty Main Module
#'
#' This module handles the uncertainty analysis functionality, allowing users
#' to identify, document, and manage uncertainties in the MCDA process.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param config Configuration list with pre-defined settings (optional)
#'
#' @return A list containing:
#' \itemize{
#'   \item{data - Reactive values for uncertainty data}
#'   \item{utilities - Utility functions for accessing uncertainty data}
#' }
#'
#' @export
uncertainty_main_module <- function(input, output, session,
                                    config = NULL) {
  
  # Initialize reactive values for uncertainty data
  uncertainty_data <- reactiveVal(
    data.frame(
      Type = character(),
      Source = character(),
      Definition = character(),
      Impact = character(),
      Mitigation = character(),
      stringsAsFactors = FALSE
    )
  )
  
  uncertainty_notes <- reactiveVal("")
  
  uncertainty_config <- reactiveValues(
    config_loaded = FALSE,
    config_name = "",
    config_version = "",
    config_author = "",
    config_date = ""
  )
  
  #### Configuration Loading ####
  
  observe({
    req(!is.null(config))
    
    uncertainty_config$config_loaded <- TRUE
    uncertainty_config$config_name <- config$config_name %||% "Unknown"
    uncertainty_config$config_version <- config$config_version %||% ""
    uncertainty_config$config_author <- config$config_author %||% ""
    uncertainty_config$config_date <- config$config_date %||% Sys.Date()
    
    # Load uncertainty-specific settings from config
    if (!is.null(config$uncertainty)) {
      
      # Load pre-defined uncertainties if provided
      if (!is.null(config$uncertainty$default_uncertainties) && 
          nrow(config$uncertainty$default_uncertainties) > 0) {
        uncertainty_data(config$uncertainty$default_uncertainties)
      }
      
      # Load default notes if provided
      if (!is.null(config$uncertainty$default_notes)) {
        uncertainty_notes(config$uncertainty$default_notes)
        
        # Update the text input with the loaded notes
        updateTextAreaInput(session, "uncertainty_notes", value = config$uncertainty$default_notes)
      }
    }
    
    message("Uncertainty configuration loaded successfully: ", config$config_name)
  })
  
  # Initialize notes from UI if they exist
  observe({
    if (!is.null(input$uncertainty_notes) && input$uncertainty_notes != "") {
      uncertainty_notes(input$uncertainty_notes)
    }
  })
  
  #### Uncertainty Logic ####
  
  # Add new uncertainty row
  observeEvent(input$add_uncertainty, {
    current_data <- uncertainty_data()
    new_row <- data.frame(
      Type = "Parameter uncertainty",
      Source = "New source",
      Definition = "Describe the uncertainty",
      Impact = "Describe potential impact",
      Mitigation = "Describe mitigation strategy",
      stringsAsFactors = FALSE
    )
    uncertainty_data(rbind(current_data, new_row))
    
    showNotification("New uncertainty row added", type = "message", duration = 2)
  })
  
  # Remove selected uncertainty
  observeEvent(input$remove_uncertainty, {
    if (!is.null(input$uncertainty_table_rows_selected)) {
      current_data <- uncertainty_data()
      updated_data <- current_data[-input$uncertainty_table_rows_selected, ]
      
      # If no rows left, keep empty dataframe with correct structure
      if (nrow(updated_data) == 0) {
        updated_data <- data.frame(
          Type = character(),
          Source = character(),
          Definition = character(),
          Impact = character(),
          Mitigation = character(),
          stringsAsFactors = FALSE
        )
      }
      
      uncertainty_data(updated_data)
      showNotification("Selected uncertainty row removed", type = "message", duration = 2)
    } else {
      showNotification("Please select a row to remove", type = "warning", duration = 3)
    }
  })
  
  # Update uncertainty table cell edits
  observeEvent(input$uncertainty_table_cell_edit, {
    info <- input$uncertainty_table_cell_edit
    current_data <- isolate(uncertainty_data())
    
    # DT uses 0-based column indexing, convert to 1-based for R
    row <- info$row
    col <- info$col + 1
    
    # Update the data frame
    current_data[row, col] <- info$value
    uncertainty_data(current_data)
  })
  
  # Save uncertainty notes
  observeEvent(input$save_uncertainty_notes, {
    uncertainty_notes(input$uncertainty_notes)
    showNotification("Uncertainty notes saved successfully!", type = "message", duration = 3)
  })
  
  #### UI Output Functions ####
  
  # Render editable uncertainty table
  output$uncertainty_table <- renderDT({
    datatable(
      uncertainty_data(),
      editable = TRUE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 't',
        scrollX = TRUE,
        columnDefs = list(
          list(width = '120px', targets = 0),
          list(width = '150px', targets = 1),
          list(width = '250px', targets = 2),
          list(width = '250px', targets = 3),
          list(width = '250px', targets = 4)
        )
      ),
      class = 'display compact stripe hover',
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-weight: bold; color: #3c8dbc;',
        'Uncertainty Identification and Management - Double-click cells to edit'
      )
    )
  }, server = TRUE)
  
  # Render uncertainty notes output for Report tab
  output$uncertainty_notes_output <- renderPrint({
    notes <- uncertainty_notes()
    if (is.null(notes) || notes == "") {
      cat("No additional notes on uncertainty analysis provided.")
    } else {
      cat(notes)
    }
  })
  
  # Render uncertainty summary for report tab
  output$report_uncertainty_table <- renderDT({
    req(nrow(uncertainty_data()) > 0)
    
    datatable(
      uncertainty_data(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'display compact stripe',
      caption = 'Uncertainty Analysis Summary'
    )
  })
  
  #### Utility Functions ####
  
  #' Get uncertainty data as data frame
  get_uncertainty_df <- reactive({
    uncertainty_data()
  })
  
  #' Get uncertainty data for Word report (serializable)
  get_uncertainty_for_report <- reactive({
    df <- uncertainty_data()
    if (nrow(df) > 0) {
      # Convert to plain data frame for serialization
      df <- as.data.frame(df)
    }
    df
  })
  
  #' Check if uncertainties exist
  has_uncertainties <- reactive({
    nrow(uncertainty_data()) > 0
  })
  
  #' Get uncertainty count
  get_uncertainty_count <- reactive({
    nrow(uncertainty_data())
  })
  
  #' Get uncertainty notes
  get_uncertainty_notes <- reactive({
    uncertainty_notes()
  })
  
  #' Reset uncertainty data
  reset_uncertainties <- function() {
    uncertainty_data(
      data.frame(
        Type = character(),
        Source = character(),
        Definition = character(),
        Impact = character(),
        Mitigation = character(),
        stringsAsFactors = FALSE
      )
    )
    uncertainty_notes("")
    updateTextAreaInput(session, "uncertainty_notes", value = "")
    showNotification("Uncertainty data reset", type = "warning", duration = 3)
  }
  
  #' Load pre-defined uncertainties
  load_default_uncertainties <- function(default_df) {
    if (!is.null(default_df) && nrow(default_df) > 0) {
      uncertainty_data(default_df)
      showNotification("Default uncertainties loaded", type = "message", duration = 3)
    }
  }
  
  # Utility function for NULL coalescing
  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }
  
  #### Return Module Components ####
  
  return(
    list(
      data = list(
        uncertainties = uncertainty_data,
        notes = uncertainty_notes,
        config = uncertainty_config
      ),
      utilities = list(
        get_uncertainty_df = get_uncertainty_df,
        get_uncertainty_for_report = get_uncertainty_for_report,
        has_uncertainties = has_uncertainties,
        get_uncertainty_count = get_uncertainty_count,
        get_uncertainty_notes = get_uncertainty_notes,
        reset_uncertainties = reset_uncertainties,
        load_default_uncertainties = load_default_uncertainties
      )
    )
  )
}