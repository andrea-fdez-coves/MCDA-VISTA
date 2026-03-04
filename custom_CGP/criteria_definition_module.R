#' Criteria Definition Module
#' 
#' This module handles the definition of decision criteria including hierarchical
#' structure, definitions, and notes. It provides reactive storage and display
#' of criteria hierarchy and properties.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param config Configuration list with pre-defined criteria (optional)
#'
#' @return A list containing:
#' \itemize{
#'   \item{data - Reactive values for criteria data}
#'   \item{ui_outputs - List of UI rendering functions}
#' }
#' 
#' @export
criteria_definition_module <- function(input, output, session, config = NULL) {
  
  # Initialize reactive values for criteria
  criteria_data <- reactiveValues(
    df = data.frame(
      id = integer(),
      name = character(),
      definition = character(),
      level = integer(),
      parent_id = integer(),
      stringsAsFactors = FALSE
    ),
    next_id = 1,
    notes = "",
    config_loaded = FALSE,
    config_name = "",
    config_description = "",
    config_version = "",
    config_author = "",
    config_date = "",
    locked_fields = list(
      criteria_names = FALSE,
      criteria_structure = FALSE
    )
  )
  
  #### Criteria Definition Logic ####
  
  # Set up observers for UI disabling/enabling
  observe({
    # Disable criterion name field if locked
    if (criteria_data$locked_fields$criteria_names) {
      shinyjs::disable("criterion_name")
    } else {
      shinyjs::enable("criterion_name")
    }
  })
  
  observe({
    # Disable criterion level selection if locked
    if (criteria_data$locked_fields$criteria_structure) {
      shinyjs::disable("criterion_level")
    } else {
      shinyjs::enable("criterion_level")
    }
  })
  
  observe({
    # Disable criterion definition field if locked
    if (criteria_data$locked_fields$criteria_names) {
      shinyjs::disable("criterion_definition")
    } else {
      shinyjs::enable("criterion_definition")
    }
  })
  
  observe({
    # Disable add criterion button if locked
    if (criteria_data$locked_fields$criteria_structure) {
      shinyjs::disable("add_criterion")
    } else {
      shinyjs::enable("add_criterion")
    }
  })
  
  #' Update Parent Criteria Selection UI
  #' 
  #' Dynamically updates the parent criteria dropdown based on selected level
  #' and existing criteria structure
  #' 
  #' @return renderUI output for parent criteria selection
  output$parent_criteria_ui <- renderUI({
    req(input$criterion_level)
    
    # Don't show parent selection if criteria structure is locked
    if (criteria_data$locked_fields$criteria_structure) {
      return(
        tags$div(
          class = "alert alert-info",
          icon("lock"),
          " Criteria structure is pre-defined and cannot be modified."
        )
      )
    }
    
    level <- as.integer(input$criterion_level)
    
    if (level > 1) {
      # Get potential parents (one level above)
      potential_parents <- criteria_data$df[criteria_data$df$level == (level - 1), ]
      
      if (nrow(potential_parents) > 0) {
        choices <- setNames(potential_parents$id, potential_parents$name)
        selectInput(
          "parent_criterion", 
          "Parent Criterion:", 
          choices = choices,
          width = "100%"
        )
      } else {
        helpText(
          paste("No Level", level - 1, "criteria available. Please add Level", 
                level - 1, "criteria first."),
          style = "color: red; font-weight: bold;"
        )
      }
    } else {
      NULL
    }
  })
  
  #' Add New Criterion
  #' 
  #' Validates and adds a new criterion to the data structure
  #' 
  #' @details Validates name uniqueness and hierarchical consistency
  observeEvent(input$add_criterion, {
    # Check if criteria structure is locked
    if (criteria_data$locked_fields$criteria_structure) {
      showNotification("Criteria structure is locked. Cannot add new criteria.", 
                       type = "warning")
      return()
    }
    
    req(input$criterion_name)
    
    # Validate inputs
    name <- trimws(input$criterion_name)
    level <- as.integer(input$criterion_level)
    definition <- trimws(input$criterion_definition)
    
    # Check for empty name
    if (name == "") {
      showNotification("Criterion name cannot be empty.", type = "error")
      return()
    }
    
    # Check for duplicate name
    if (name %in% criteria_data$df$name) {
      showNotification("Criterion name must be unique.", type = "error")
      return()
    }
    
    # Handle parent selection
    if (level == 1) {
      parent_id <- 0
    } else {
      # Validate parent exists for higher levels
      req(input$parent_criterion)
      parent_id <- as.integer(input$parent_criterion)
      
      # Verify parent exists
      if (!any(criteria_data$df$id == parent_id)) {
        showNotification("Selected parent criterion no longer exists. Please refresh and try again.", 
                         type = "error")
        return()
      }
    }
    
    # Add to data frame
    new_id <- criteria_data$next_id
    new_row <- data.frame(
      id = new_id,
      name = name,
      level = level,
      parent_id = parent_id,
      definition = definition,
      stringsAsFactors = FALSE
    )
    
    criteria_data$df <- rbind(criteria_data$df, new_row)
    criteria_data$next_id <- new_id + 1
    
    # Reset form
    updateTextInput(session, "criterion_name", value = "")
    updateTextAreaInput(session, "criterion_definition", value = "")
    showNotification("Criterion added successfully!", type = "message")
  })
  
  #' Display Criteria Hierarchy
  #' 
  #' Renders a hierarchical visual representation of criteria
  #' 
  #' @return renderUI with hierarchical list structure
  output$hierarchy_display <- renderUI({
    if (nrow(criteria_data$df) == 0) {
      return(
        tags$div(
          class = "empty-state",
          icon("folder-open", class = "fa-3x"),
          tags$p("No criteria defined yet."),
          tags$p("Start by adding your first criterion using the form on the left."),
          style = "text-align: center; color: #666; padding: 40px;"
        )
      )
    }
    
    # Create hierarchical list
    create_hierarchy_list <- function(parent_id = 0, level = 0) {
      children <- criteria_data$df[criteria_data$df$parent_id == parent_id, ]
      if (nrow(children) == 0) return(NULL)
      
      # Sort children by name for consistent display
      children <- children[order(children$name), ]
      
      items <- lapply(1:nrow(children), function(i) {
        child <- children[i, ]
        sub_items <- create_hierarchy_list(child$id, level + 1)
        
        # Determine color based on level
        level_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
        color_index <- min(child$level, length(level_colors))
        
        # Add lock icon if config is loaded
        lock_icon <- if (criteria_data$config_loaded) {
          icon("lock", class = "fa-xs", style = "margin-left: 5px; color: #888;")
        } else {
          NULL
        }
        
        tags$li(
          tags$div(
            class = "criterion-item",
            tags$span(
              class = "badge",
              style = paste0("background-color: ", level_colors[color_index], ";"),
              paste("L", child$level)
            ),
            tags$strong(child$name),
            lock_icon,
            if (!is.null(sub_items)) tags$ul(sub_items, class = "nested-list")
          ),
          style = "margin: 5px 0;"
        )
      })
      
      tags$ul(items, class = "hierarchy-list")
    }
    
    # Add CSS for styling
    css <- tags$style(HTML("
      .hierarchy-list {
        list-style-type: none;
        padding-left: 20px;
      }
      .nested-list {
        border-left: 2px solid #ddd;
        margin-left: 10px;
        padding-left: 15px;
      }
      .criterion-item {
        padding: 8px;
        background-color: #f8f9fa;
        border-radius: 4px;
        margin-bottom: 4px;
      }
      .badge {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 12px;
        color: white;
        font-size: 0.8em;
        margin-right: 8px;
      }
      .empty-state {
        text-align: center;
        color: #666;
        padding: 40px;
      }
      .alert-info {
        padding: 10px;
        background-color: #d9edf7;
        border: 1px solid #bce8f1;
        border-radius: 4px;
        color: #31708f;
      }
    "))
    
    # Add configuration info if loaded
    config_info <- if (criteria_data$config_loaded) {
      tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        " Using pre-configured criteria from: ", 
        tags$strong(criteria_data$config_name)
      )
    } else {
      NULL
    }
    
    tagList(
      css,
      config_info,
      tags$div(
        style = "max-height: 400px; overflow-y: auto; padding: 10px;",
        create_hierarchy_list()
      )
    )
  })
  
  #' Display Criteria Table
  #' 
  #' Renders a data table with all criteria and their properties
  #' 
  #' @return renderDT with criteria table
  output$criteria_table <- renderDT({
    # Get the current criteria data
    current_df <- criteria_data$df
    
    # Check if we have any data
    if (is.null(current_df) || nrow(current_df) == 0) {
      return(
        datatable(
          data.frame(Message = "No criteria defined yet."),
          options = list(dom = "t"),
          rownames = FALSE
        )
      )
    }
    
    # We have data, proceed with display
    tryCatch({
      # Create parent names vector
      parent_names <- sapply(current_df$parent_id, function(id) {
        if (id == 0) return("None (Top Level)")
        parent_row <- current_df[current_df$id == id, ]
        if (nrow(parent_row)) parent_row$name else "Unknown"
      })
      
      # Create level display with colors
      level_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
      level_display <- sapply(current_df$level, function(lvl) {
        color <- level_colors[min(lvl, length(level_colors))]
        paste0('<span style="color:', color, '; font-weight: bold;">Level ', lvl, '</span>')
      })
      
      # Build the base display data
      base_data <- data.frame(
        name = current_df$name,
        level = level_display,
        parent = parent_names,
        definition = current_df$definition,
        stringsAsFactors = FALSE
      )
      
      # Add lock column if config is loaded
      if (criteria_data$config_loaded) {
        # Create lock icons with the same number of rows
        n_rows <- nrow(base_data)
        lock_icons <- character(n_rows)
        for (i in 1:n_rows) {
          lock_icons[i] <- as.character(icon("lock", style = "color: #888;"))
        }
        
        # Create the final data frame
        final_data <- data.frame(
          locked = lock_icons,
          name = base_data$name,
          level = base_data$level,
          parent = base_data$parent,
          definition = base_data$definition,
          stringsAsFactors = FALSE
        )
        
        col_names <- c("", "Criterion", "Level", "Parent", "Definition")
        col_widths <- list(
          list(width = '3%', targets = 0),
          list(width = '22%', targets = 1),
          list(width = '15%', targets = 2),
          list(width = '20%', targets = 3),
          list(width = '40%', targets = 4)
        )
      } else {
        final_data <- base_data
        col_names <- c("Criterion", "Level", "Parent", "Definition")
        col_widths <- list(
          list(width = '25%', targets = 0),
          list(width = '15%', targets = 1),
          list(width = '20%', targets = 2),
          list(width = '40%', targets = 3)
        )
      }
      
      # Create the datatable
      datatable(
        final_data,
        colnames = col_names,
        rownames = FALSE,
        escape = FALSE,  # Allow HTML in level_display
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = col_widths
        ),
        class = "stripe hover nowrap",
        extensions = 'Buttons'
      )
    }, error = function(e) {
      # Return error message in case of any issues
      datatable(
        data.frame(
          Error = paste("Error displaying criteria:", e$message),
          Details = "Please check the console for more information"
        ),
        options = list(dom = "t"),
        rownames = FALSE
      )
    })
  })
  
  #' Save Criteria and Notes
  #' 
  #' Observes the save button and stores criteria data and notes
  observeEvent(input$save_criteria, {
    criteria_data$notes <- input$criteria_notes
    showNotification("Criteria and notes saved successfully!", type = "message")
  })
  
  #' Load Configuration
  #' 
  #' Loads pre-defined criteria from configuration if provided
  observe({
    req(!is.null(config))
    
    # Store configuration info
    criteria_data$config_loaded <- TRUE
    criteria_data$config_name <- config$config_name
    criteria_data$config_description <- config$config_description
    criteria_data$config_version <- config$config_version
    criteria_data$config_author <- config$config_author
    criteria_data$config_date <- config$config_date
    
    # Load criteria data from config
    if (!is.null(config$criteria)) {
      # Set all values from config
      for (field in names(config$criteria)) {
        criteria_data[[field]] <- config$criteria[[field]]
      }
    }
    
    # Update locked fields from config
    if (!is.null(config$locked_fields_criteria)) {
      for (field in names(config$locked_fields_criteria)) {
        criteria_data$locked_fields[[field]] <- config$locked_fields_criteria[[field]]
      }
    } else if (!is.null(config$locked_fields)) {
      # Backward compatibility: check for locked_fields directly
      if (!is.null(config$locked_fields$criteria_names)) {
        criteria_data$locked_fields$criteria_names <- config$locked_fields$criteria_names
      }
      if (!is.null(config$locked_fields$criteria_structure)) {
        criteria_data$locked_fields$criteria_structure <- config$locked_fields$criteria_structure
      }
    }
    
    # Update UI to show locked state
    updateTextInput(session, "criterion_name", 
                    placeholder = if (criteria_data$locked_fields$criteria_names) 
                      "Pre-defined criteria (locked)" else 
                        "Enter criterion name")
  })
  
  #### Report Output Functions ####
  
  #' Generate Report Criteria Table
  #' 
  #' Creates a formatted criteria table for the report tab
  #' 
  #' @return renderDT function output
  generate_report_criteria_table <- function() {
    renderDT({
      if (nrow(criteria_data$df) == 0) {
        return(
          datatable(
            data.frame(Message = "No criteria defined."),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }
      
      display_df <- criteria_data$df
      
      # Add parent name for display
      display_df$parent_name <- sapply(display_df$parent_id, function(id) {
        if (id == 0) return("None")
        parent_row <- criteria_data$df[criteria_data$df$id == id, ]
        if (nrow(parent_row)) parent_row$name else "Unknown"
      })
      
      # Create display data - ensure all columns have the same number of rows
      display_data <- data.frame(
        name = display_df$name,
        level = display_df$level,
        parent_name = display_df$parent_name,
        definition = display_df$definition,
        stringsAsFactors = FALSE
      )
      
      # Add configuration indicator
      if (criteria_data$config_loaded) {
        display_data$source <- rep("Pre-configured", nrow(display_data))
      } else {
        display_data$source <- rep("User-defined", nrow(display_data))
      }
      
      datatable(
        display_data,
        colnames = c("Criterion", "Level", "Parent", "Definition", "Source"),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          dom = 't'
        ),
        class = "stripe hover"
      )
    })
  }
  
  #' Generate Report Hierarchy Display
  #' 
  #' Creates a simplified hierarchy display for the report tab
  #' 
  #' @return renderUI function output
  generate_report_hierarchy_display <- function() {
    renderUI({
      if (nrow(criteria_data$df) == 0) {
        return(tags$p("No criteria defined."))
      }
      
      create_hierarchy_list <- function(parent_id = 0, level = 0) {
        children <- criteria_data$df[criteria_data$df$parent_id == parent_id, ]
        if (nrow(children) == 0) return(NULL)
        
        children <- children[order(children$name), ]
        
        items <- lapply(1:nrow(children), function(i) {
          child <- children[i, ]
          sub_items <- create_hierarchy_list(child$id, level + 1)
          tags$li(
            tags$span(
              style = paste0("font-weight: bold; color: ",
                             c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")[child$level]),
              child$name
            ),
            if (!is.null(sub_items)) tags$ul(sub_items)
          )
        })
        
        tags$ul(items)
      }
      
      tagList(
        if (criteria_data$config_loaded) {
          tags$div(
            class = "alert alert-info",
            icon("info-circle"),
            " Pre-configured criteria set: ", 
            tags$strong(criteria_data$config_name)
          )
        },
        tags$div(
          style = "max-height: 300px; overflow-y: auto; padding: 10px;",
          create_hierarchy_list()
        )
      )
    })
  }
  
  #' Render Criteria Notes
  #' 
  #' Displays the criteria definition notes
  #' 
  #' @return renderText function output
  render_criteria_notes <- function() {
    renderText({
      criteria_data$notes
    })
  }
  
  #' Render Configuration Info
  #' 
  #' Displays information about loaded criteria configuration
  #' 
  #' @return renderUI function output
  render_criteria_config_info <- function() {
    renderUI({
      if (criteria_data$config_loaded) {
        tagList(
          h4("Active Criteria Configuration"),
          p(strong("Name:"), criteria_data$config_name),
          p(strong("Description:"), criteria_data$config_description),
          p(strong("Version:"), criteria_data$config_version),
          p(strong("Author:"), criteria_data$config_author),
          p(strong("Date:"), format(criteria_data$config_date, "%Y-%m-%d")),
          p(em(paste("Number of pre-defined criteria:", nrow(criteria_data$df)))),
          p(em("Criteria are locked and cannot be modified in this session."))
        )
      }
    })
  }
  
  #' Get Selected Criteria Levels
  #' 
  #' Returns a reactive expression for selected hierarchy levels
  #' 
  #' @return reactive expression with selected levels
  get_selected_levels <- reactive({
    req(input$selected_levels)
    as.integer(input$selected_levels)
  })
  
  #' Get Criteria at Selected Levels
  #' 
  #' Returns criteria filtered by selected hierarchy levels
  #' 
  #' @return reactive expression with filtered criteria
  get_filtered_criteria <- reactive({
    req(nrow(criteria_data$df) > 0)
    selected <- get_selected_levels()
    criteria_data$df[criteria_data$df$level %in% selected, ]
  })
  
  #### Initialize UI Outputs ####
  output$report_criteria_table <- generate_report_criteria_table()
  output$report_hierarchy_display <- generate_report_hierarchy_display()
  output$criteria_notes_output <- render_criteria_notes()
  output$criteria_config_info <- render_criteria_config_info()
  
  #### Return Module Components ####
  return(
    list(
      data = criteria_data,
      ui_outputs = list(
        report_criteria_table = generate_report_criteria_table,
        report_hierarchy_display = generate_report_hierarchy_display,
        criteria_notes_output = render_criteria_notes,
        criteria_config_info = render_criteria_config_info
      ),
      utilities = list(
        get_selected_levels = get_selected_levels,
        get_filtered_criteria = get_filtered_criteria
      )
    )
  )
}