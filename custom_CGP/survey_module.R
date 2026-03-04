# ============================================================================
# FILE: survey_module.R
# ============================================================================
#' Survey Data Analysis Module
#'
#' This module handles all survey data analysis, including stakeholder responses,
#' performance assessments, and aggregated scores from the survey data.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param config Configuration list with pre-defined settings (optional)
#'
#' @return A list containing:
#' \itemize{
#'   \item{data - Reactive values for survey data}
#'   \item{utilities - Utility functions for accessing survey results}
#' }
#'
#' @export
survey_module <- function(input, output, session, config = NULL) {
  
  # Utility function for NULL coalescing
  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }
  
  # Namespace for this module (though we'll write directly to global outputs)
  ns <- session$ns
  
  # ---------------------------------------------------------
  # DATA PREPARATION
  # ---------------------------------------------------------
  
  # Load survey data
  survey_data <- reactive({
    # You can make this reactive to allow for file uploads later
    tryCatch({
      read.csv("survey_data.csv", header = TRUE, sep = ";")
    }, error = function(e) {
      # Return empty data frame with expected columns if file not found
      data.frame(
        Stakeholder.type = character(),
        Workplace.setting = character(),
        stringsAsFactors = FALSE
      )
    })
  })
  
  # Full determinant names for display
  qmcda_full_names <- c(
    Clinical     = "Clinical Benefit",
    Cost         = "Cost‑Effectiveness",
    Feasibility  = "Feasibility",
    Test         = "Test Journey",
    Implications = "Wider Implications",
    Organization = "Organization of Laboratories",
    Scientific   = "Scientific Spillover"
  )
  
  # Response colors and levels
  response_colors <- c(
    "Agree" = "#5DA899",
    "Somewhat agree" = "#94CBEC",
    "Neither agree nor disagree" = "#DCCD7D",
    "Somewhat disagree" = "#C26A77",
    "Disagree" = "#9F4A96",
    "Don't know" = "#DDDDDD",
    "NA" = "#848484"
  )
  
  response_levels <- c("Agree", "Somewhat agree", "Neither agree nor disagree", 
                       "Somewhat disagree", "Disagree", "Don't know", "NA")
  
  # Processed survey data (with cleaned values)
  processed_survey_data <- reactive({
    data <- survey_data()
    
    if (nrow(data) == 0) return(data)
    
    # Handle "Not Applicable" workplace setting for patient representatives
    data <- data %>%
      mutate(Workplace.setting = ifelse(
        Stakeholder.type == "Patient representative" & 
          Workplace.setting == "Not Applicable",
        "Neither (Patient representative)",
        as.character(Workplace.setting)
      ))
    
    return(data)
  })
  
  # ---------------------------------------------------------
  # HELPER FUNCTIONS (defined inside module)
  # ---------------------------------------------------------
  
  # Process data for Likert-style plots
  process_data <- function(data, columns) {
    data_long <- data %>%
      select(all_of(columns)) %>%
      mutate(across(everything(), ~case_when(
        . == "Dont know" ~ "Don't know",
        is.na(.) ~ "NA",
        TRUE ~ as.character(.)
      ))) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
      mutate(Response = factor(Response, levels = response_levels))
    
    return(data_long)
  }
  
  # Create Likert-style bar plot
  create_plot <- function(df, variable_labels) {
    # Prepare label text with percentages
    label_data <- df %>%
      group_by(Variable, Response) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(Variable) %>%
      mutate(Percentage = Count / sum(Count),
             Label = ifelse(Percentage > 0.05, paste0(round(Percentage * 100), "%"), ""))
    
    ggplot(df, aes(x = Variable, fill = Response)) +
      geom_bar(position = "fill", width = 0.8) +
      geom_text(
        data = label_data,
        aes(x = Variable, y = Percentage, label = Label, group = Response),
        position = position_stack(vjust = 0.5),
        color = "white", size = 4, fontface = "bold"
      ) +
      scale_fill_manual(values = response_colors, drop = FALSE) +
      scale_x_discrete(labels = variable_labels) +
      scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
      labs(y = "Percentage", x = "") +
      coord_flip() +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  }
  
  # Create performance analysis summary
  create_performance_analysis_survey <- function(data, small_col, broad_col) {
    # Calculate summary statistics
    summary_data <- data %>%
      summarise(
        TP_Mean = mean(get(small_col), na.rm = TRUE),
        TP_SD = sd(get(small_col), na.rm = TRUE),
        CGP_Mean = mean(get(broad_col), na.rm = TRUE),
        CGP_SD = sd(get(broad_col), na.rm = TRUE),
        TP_CI_lower = TP_Mean - 1.96 * (TP_SD / sqrt(n())),
        TP_CI_upper = TP_Mean + 1.96 * (TP_SD / sqrt(n())),
        CGP_CI_lower = CGP_Mean - 1.96 * (CGP_SD / sqrt(n())),
        CGP_CI_upper = CGP_Mean + 1.96 * (CGP_SD / sqrt(n())),
        .groups = "drop"
      )
    
    return(summary_data)
  }
  
  # Create performance plot
  create_performance_plot_survey <- function(summary_data, title) {
    plot_data <- data.frame(
      Method = rep(c("TP", "CGP"), each = nrow(summary_data)),
      Mean = c(summary_data$TP_Mean, summary_data$CGP_Mean),
      CI_lower = c(summary_data$TP_CI_lower, summary_data$CGP_CI_lower),
      CI_upper = c(summary_data$TP_CI_upper, summary_data$CGP_CI_upper)
    )
    
    ggplot(plot_data, aes(x = Method, y = Mean, fill = Method)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                    width = 0.2, position = position_dodge(0.7)) +
      scale_fill_manual(values = c("TP" = "#6BAED6", "CGP" = "#FD8D3C")) +
      labs(title = title, y = "Score", x = "") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 12, hjust = 0.5))
  }
  
  # Create performance table
  create_performance_table_survey <- function(summary_data) {
    table_data <- data.frame(
      Method = c("TP", "CGP"),
      Mean = c(round(summary_data$TP_Mean, 1), round(summary_data$CGP_Mean, 1)),
      SD = c(round(summary_data$TP_SD, 1), round(summary_data$CGP_SD, 1)),
      CI = c(
        paste0("(", round(summary_data$TP_CI_lower, 1), " - ", round(summary_data$TP_CI_upper, 1), ")"),
        paste0("(", round(summary_data$CGP_CI_lower, 1), " - ", round(summary_data$CGP_CI_upper, 1), ")")
      )
    )
    
    return(table_data)
  }
  
  # Create grouped performance plot
  create_grouped_performance_plot_survey <- function(summary_data, group_var, title) {
    plot_data <- summary_data %>%
      pivot_longer(
        cols = c(TP_Mean, CGP_Mean, TP_CI_lower, TP_CI_upper, CGP_CI_lower, CGP_CI_upper),
        names_to = "Metric",
        values_to = "Value"
      ) %>%
      mutate(
        Method = ifelse(grepl("TP", Metric), "TP", "CGP"),
        Metric_Type = case_when(
          grepl("_Mean", Metric) ~ "Mean",
          grepl("_lower", Metric) ~ "CI_lower",
          grepl("_upper", Metric) ~ "CI_upper"
        )
      ) %>%
      pivot_wider(names_from = Metric_Type, values_from = Value) %>%
      select(-Metric)
    
    ggplot(plot_data, aes(x = Method, y = Mean, fill = Method)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                    width = 0.2, position = position_dodge(0.7)) +
      scale_fill_manual(values = c("TP" = "#6BAED6", "CGP" = "#FD8D3C")) +
      labs(title = title, y = "Score", x = "") +
      facet_wrap(as.formula(paste0("~ ", group_var)), scales = "free_y", nrow = 1) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 12, hjust = 0.5),
            strip.text = element_text(size = 11, face = "bold"))
  }
  
  # Create grouped performance table
  create_grouped_performance_table_survey <- function(summary_data, group_var) {
    table_data <- summary_data %>%
      rowwise() %>%
      mutate(
        TP_Stats = paste0(round(TP_Mean, 1), " ± ", round(TP_SD, 1), 
                          " (", round(TP_CI_lower, 1), "-", round(TP_CI_upper, 1), ")"),
        CGP_Stats = paste0(round(CGP_Mean, 1), " ± ", round(CGP_SD, 1), 
                           " (", round(CGP_CI_lower, 1), "-", round(CGP_CI_upper, 1), ")")
      ) %>%
      select(!!sym(group_var), TP_Stats, CGP_Stats) %>%
      pivot_longer(
        cols = c(TP_Stats, CGP_Stats),
        names_to = "Method",
        values_to = "Statistics"
      ) %>%
      mutate(Method = ifelse(Method == "TP_Stats", "TP", "CGP"))
    
    return(table_data)
  }
  
  # Calculate mean weights from survey data
  mean_weights <- reactive({
    data <- processed_survey_data()
    
    if (nrow(data) == 0) {
      return(data.frame(
        Criterion = c("Clinical", "Cost", "Feasibility", "Test", "Implications", "Organization", "Scientific"),
        Weight = c(0, 0, 0, 0, 0, 0, 0)
      ))
    }
    
    data %>%
      summarise(
        Clinical = mean(QMCDA1_Clinical, na.rm = TRUE),
        Cost = mean(QMCDA1_Cost, na.rm = TRUE),
        Feasibility = mean(QMCDA1_Feasibility, na.rm = TRUE),
        Test = mean(QMCDA1_Test, na.rm = TRUE),
        Implications = mean(QMCDA1_Implications, na.rm = TRUE),
        Organization = mean(QMCDA1_Organization, na.rm = TRUE),
        Scientific = mean(QMCDA1_Scientific, na.rm = TRUE)
      ) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
      pivot_longer(everything(), names_to = "Criterion", values_to = "Weight")
  })
  
  # Define criterion colors for display
  criterion_colors_display <- c(
    "Clinical Benefit" = "#36454F",
    "Cost Effectiveness" = "#FFFFF0", 
    "Feasibility" = "#648FFF",
    "Test Journey" = "#785EF0",
    "Wider Implications of diagnostic results" = "#DC267F",
    "Organization of Laboratories" = "#FE6100",
    "Scientific Spillover" = "#FFB000"
  )
  
  # Criterion name mapping
  criterion_names <- c(
    "Clinical" = "Clinical Benefit",
    "Cost" = "Cost Effectiveness",
    "Feasibility" = "Feasibility",
    "Test" = "Test Journey",
    "Implications" = "Wider Implications of diagnostic results",
    "Organization" = "Organization of Laboratories",
    "Scientific" = "Scientific Spillover"
  )
  
  # Collect performance data and calculate aggregated scores
  aggregated_scores <- eventReactive(input$update_aggregate_survey, {
    data <- processed_survey_data()
    
    # If no data, return empty dataframe with structure
    if (nrow(data) == 0) {
      return(data.frame(
        Criterion = c("Clinical", "Cost", "Feasibility", "Test", "Implications", "Organization", "Scientific"),
        TP_Score = c(0, 0, 0, 0, 0, 0, 0),
        CGP_Score = c(0, 0, 0, 0, 0, 0, 0),
        Weight = c(0, 0, 0, 0, 0, 0, 0),
        TP_Aggregated = c(0, 0, 0, 0, 0, 0, 0),
        CGP_Aggregated = c(0, 0, 0, 0, 0, 0, 0)
      ))
    }
    
    weights <- mean_weights()
    
    # Get performance scores from survey data (replace NA with 0)
    performance_data <- data.frame(
      Criterion = c("Clinical", "Cost", "Feasibility", "Test", "Implications", "Organization", "Scientific"),
      TP_Score = c(
        ifelse(input$include_clinical_survey, input$clinical_tp_survey %||% 0, 0),
        ifelse(input$include_costeff_survey, input$cost_tp_survey %||% 0, 0),
        mean(data$QFeasibility_Small, na.rm = TRUE) %||% 0,
        mean(data$QTestingPathwaySmall, na.rm = TRUE) %||% 0,
        mean(data$QImplicationsSmall, na.rm = TRUE) %||% 0,
        mean(data$QOrganizationSmall, na.rm = TRUE) %||% 0,
        mean(data$QScientificSmall, na.rm = TRUE) %||% 0
      ),
      CGP_Score = c(
        ifelse(input$include_clinical_survey, input$clinical_cgp_survey %||% 0, 0),
        ifelse(input$include_costeff_survey, input$cost_cgp_survey %||% 0, 0),
        mean(data$QFeasibility_Broad, na.rm = TRUE) %||% 0,
        mean(data$QTestingPathwayBroad, na.rm = TRUE) %||% 0,
        mean(data$QImplicationsBroad, na.rm = TRUE) %||% 0,
        mean(data$QOrganizationBroad, na.rm = TRUE) %||% 0,
        mean(data$QScientificBroad, na.rm = TRUE) %||% 0
      )
    ) %>%
      mutate(across(c(TP_Score, CGP_Score), ~ ifelse(is.na(.), 0, .)))
    
    # Calculate aggregated scores
    aggregated <- performance_data %>%
      left_join(weights, by = "Criterion") %>%
      mutate(
        TP_Aggregated = TP_Score * Weight / 100,
        CGP_Aggregated = CGP_Score * Weight / 100
      )
    
    return(aggregated)
  })
  
  # ---------------------------------------------------------
  # FILTERED DATA REACTIVES FOR EACH TAB
  # ---------------------------------------------------------
  
  # Feasibility tab
  filtered_feas_data <- eventReactive(input$update_feas, {
    req(input$feas_stakeholders, input$feas_settings)
    
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$feas_stakeholders,
             Workplace.setting %in% input$feas_settings)
    
    process_data(data, c("Data.storage", "Interpretation.of.results", "Tissue.quality"))
  })
  
  # Patient Journey tab
  filtered_journey_data <- eventReactive(input$update_journey, {
    req(input$journey_stakeholders, input$journey_settings)
    
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$journey_stakeholders,
             Workplace.setting %in% input$journey_settings)
    
    process_data(data, c("Number.of.biopsy.procedures", "Completeness.of.results"))
  })
  
  # Diagnostic Results tab
  filtered_diagnostic_data <- eventReactive(input$update_diagnostic, {
    req(input$diagnostic_stakeholders, input$diagnostic_settings)
    
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$diagnostic_stakeholders,
             Workplace.setting %in% input$diagnostic_settings)
    
    process_data(data, c("Treatment.options", "Germline.alterations", 
                         "Biomarkers.for.informing.diagnosis", "Pharmacogenomic.profiles",
                         "Biomarkers.for.non.response.or.prognosis"))
  })
  
  # Organization of Laboratories tab
  filtered_orglab_data <- eventReactive(input$update_orglab, {
    req(input$orglab_stakeholders, input$orglab_settings)
    
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$orglab_stakeholders,
             Workplace.setting %in% input$orglab_settings)
    
    process_data(data, c("Test.Uniformity", "Future.proofing"))
  })
  
  # Scientific Spillover tab
  filtered_scispill_data <- eventReactive(input$update_scispill, {
    req(input$scispill_stakeholders, input$scispill_settings)
    
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$scispill_stakeholders,
             Workplace.setting %in% input$scispill_settings)
    
    process_data(data, c("Infrastructure.for.learning.care.system", "Learning.care.system"))
  })
  
  # ---------------------------------------------------------
  # OUTPUTS - FEASIBILITY TAB
  # ---------------------------------------------------------
  
  # Feasibility Likert plot
  output$feasibility_plot <- renderPlot({
    df <- filtered_feas_data()
    variable_labels <- c(
      "Data.storage" = "Data Storage",
      "Interpretation.of.results" = "Interpretation of Results",
      "Tissue.quality" = "Tissue Quality"
    )
    create_plot(df, variable_labels)
  })
  
  # Feasibility Total Performance Plot
  output$feas_total_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$feas_stakeholders,
             Workplace.setting %in% input$feas_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QFeasibility_Small", "QFeasibility_Broad")
    create_performance_plot_survey(summary_data, "Total Performance")
  })
  
  # Feasibility Total Performance Table
  output$feas_total_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$feas_stakeholders,
             Workplace.setting %in% input$feas_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QFeasibility_Small", "QFeasibility_Broad")
    table_data <- create_performance_table_survey(summary_data)
    
    datatable(table_data, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Feasibility Stakeholder Plot
  output$feas_stakeholder_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$feas_stakeholders,
             Workplace.setting %in% input$feas_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QFeasibility_Small", "QFeasibility_Broad"))
    
    create_grouped_performance_plot_survey(summary_data, "Stakeholder.type", "By Stakeholder Type")
  })
  
  # Feasibility Stakeholder Table
  output$feas_stakeholder_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$feas_stakeholders,
             Workplace.setting %in% input$feas_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QFeasibility_Small", "QFeasibility_Broad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Stakeholder.type")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 8, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Stakeholder Type', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # Feasibility Setting Plot
  output$feas_setting_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$feas_stakeholders,
             Workplace.setting %in% input$feas_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QFeasibility_Small", "QFeasibility_Broad"))
    
    create_grouped_performance_plot_survey(summary_data, "Workplace.setting", "By Workplace Setting")
  })
  
  # Feasibility Setting Table
  output$feas_setting_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$feas_stakeholders,
             Workplace.setting %in% input$feas_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QFeasibility_Small", "QFeasibility_Broad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Workplace.setting")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 6, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Workplace Setting', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # ---------------------------------------------------------
  # OUTPUTS - PATIENT JOURNEY TAB
  # ---------------------------------------------------------
  
  # Patient Journey Likert plot
  output$journey_plot <- renderPlot({
    df <- filtered_journey_data()
    variable_labels <- c(
      "Number.of.biopsy.procedures" = "Number of Biopsy Procedures",
      "Completeness.of.results" = "Completeness of Results"
    )
    create_plot(df, variable_labels)
  })
  
  # Patient Journey Total Performance Plot
  output$journey_total_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$journey_stakeholders,
             Workplace.setting %in% input$journey_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QTestingPathwaySmall", "QTestingPathwayBroad")
    create_performance_plot_survey(summary_data, "Total Performance")
  })
  
  # Patient Journey Total Performance Table
  output$journey_total_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$journey_stakeholders,
             Workplace.setting %in% input$journey_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QTestingPathwaySmall", "QTestingPathwayBroad")
    table_data <- create_performance_table_survey(summary_data)
    datatable(table_data, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Patient Journey Stakeholder Plot
  output$journey_stakeholder_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$journey_stakeholders,
             Workplace.setting %in% input$journey_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QTestingPathwaySmall", "QTestingPathwayBroad"))
    
    create_grouped_performance_plot_survey(summary_data, "Stakeholder.type", "By Stakeholder Type")
  })
  
  # Patient Journey Stakeholder Table
  output$journey_stakeholder_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$journey_stakeholders,
             Workplace.setting %in% input$journey_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QTestingPathwaySmall", "QTestingPathwayBroad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Stakeholder.type")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 8, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Stakeholder Type', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # Patient Journey Setting Plot
  output$journey_setting_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$journey_stakeholders,
             Workplace.setting %in% input$journey_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QTestingPathwaySmall", "QTestingPathwayBroad"))
    
    create_grouped_performance_plot_survey(summary_data, "Workplace.setting", "By Workplace Setting")
  })
  
  # Patient Journey Setting Table
  output$journey_setting_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$journey_stakeholders,
             Workplace.setting %in% input$journey_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QTestingPathwaySmall", "QTestingPathwayBroad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Workplace.setting")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 6, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Workplace Setting', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # ---------------------------------------------------------
  # OUTPUTS - DIAGNOSTIC RESULTS TAB
  # ---------------------------------------------------------
  
  # Diagnostic Results Likert plot
  output$diagnostic_plot <- renderPlot({
    df <- filtered_diagnostic_data()
    variable_labels <- c(
      "Treatment.options" = "Treatment Options",
      "Germline.alterations" = "Germline Alterations",
      "Biomarkers.for.informing.diagnosis" = "Biomarkers for Diagnosis",
      "Pharmacogenomic.profiles" = "Pharmacogenomic Profiles",
      "Biomarkers.for.non.response.or.prognosis" = "Biomarkers for Non-response/Prognosis"
    )
    create_plot(df, variable_labels)
  })
  
  # Diagnostic Results Total Performance Plot
  output$diagnostic_total_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$diagnostic_stakeholders,
             Workplace.setting %in% input$diagnostic_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QImplicationsSmall", "QImplicationsBroad")
    create_performance_plot_survey(summary_data, "Total Performance")
  })
  
  # Diagnostic Results Total Performance Table
  output$diagnostic_total_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$diagnostic_stakeholders,
             Workplace.setting %in% input$diagnostic_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QImplicationsSmall", "QImplicationsBroad")
    table_data <- create_performance_table_survey(summary_data)
    datatable(table_data, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Diagnostic Results Stakeholder Plot
  output$diagnostic_stakeholder_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$diagnostic_stakeholders,
             Workplace.setting %in% input$diagnostic_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QImplicationsSmall", "QImplicationsBroad"))
    
    create_grouped_performance_plot_survey(summary_data, "Stakeholder.type", "By Stakeholder Type")
  })
  
  # Diagnostic Results Stakeholder Table
  output$diagnostic_stakeholder_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$diagnostic_stakeholders,
             Workplace.setting %in% input$diagnostic_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QImplicationsSmall", "QImplicationsBroad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Stakeholder.type")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 8, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Stakeholder Type', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # Diagnostic Results Setting Plot
  output$diagnostic_setting_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$diagnostic_stakeholders,
             Workplace.setting %in% input$diagnostic_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QImplicationsSmall", "QImplicationsBroad"))
    
    create_grouped_performance_plot_survey(summary_data, "Workplace.setting", "By Workplace Setting")
  })
  
  # Diagnostic Results Setting Table
  output$diagnostic_setting_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$diagnostic_stakeholders,
             Workplace.setting %in% input$diagnostic_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QImplicationsSmall", "QImplicationsBroad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Workplace.setting")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 6, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Workplace Setting', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # ---------------------------------------------------------
  # OUTPUTS - ORGANIZATION OF LABORATORIES TAB
  # ---------------------------------------------------------
  
  # Organization Likert plot
  output$orglab_plot <- renderPlot({
    df <- filtered_orglab_data()
    variable_labels <- c(
      "Test.Uniformity" = "Test Uniformity",
      "Future.proofing" = "Future-proofing"
    )
    create_plot(df, variable_labels)
  })
  
  # Organization Total Performance Plot
  output$orglab_total_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$orglab_stakeholders,
             Workplace.setting %in% input$orglab_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QOrganizationSmall", "QOrganizationBroad")
    create_performance_plot_survey(summary_data, "Total Performance")
  })
  
  # Organization Total Performance Table
  output$orglab_total_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$orglab_stakeholders,
             Workplace.setting %in% input$orglab_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QOrganizationSmall", "QOrganizationBroad")
    table_data <- create_performance_table_survey(summary_data)
    datatable(table_data, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Organization Stakeholder Plot
  output$orglab_stakeholder_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$orglab_stakeholders,
             Workplace.setting %in% input$orglab_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QOrganizationSmall", "QOrganizationBroad"))
    
    create_grouped_performance_plot_survey(summary_data, "Stakeholder.type", "By Stakeholder Type")
  })
  
  # Organization Stakeholder Table
  output$orglab_stakeholder_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$orglab_stakeholders,
             Workplace.setting %in% input$orglab_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QOrganizationSmall", "QOrganizationBroad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Stakeholder.type")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 8, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Stakeholder Type', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # Organization Setting Plot
  output$orglab_setting_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$orglab_stakeholders,
             Workplace.setting %in% input$orglab_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QOrganizationSmall", "QOrganizationBroad"))
    
    create_grouped_performance_plot_survey(summary_data, "Workplace.setting", "By Workplace Setting")
  })
  
  # Organization Setting Table
  output$orglab_setting_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$orglab_stakeholders,
             Workplace.setting %in% input$orglab_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QOrganizationSmall", "QOrganizationBroad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Workplace.setting")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 6, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Workplace Setting', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # ---------------------------------------------------------
  # OUTPUTS - SCIENTIFIC SPILLOVER TAB
  # ---------------------------------------------------------
  
  # Scientific Spillover Likert plot
  output$scispill_plot <- renderPlot({
    df <- filtered_scispill_data()
    variable_labels <- c(
      "Infrastructure.for.learning.care.system" = "Infrastructure for Learning Care System",
      "Learning.care.system" = "Learning Care System"
    )
    create_plot(df, variable_labels)
  })
  
  # Scientific Spillover Total Performance Plot
  output$scispill_total_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$scispill_stakeholders,
             Workplace.setting %in% input$scispill_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QScientificSmall", "QScientificBroad")
    create_performance_plot_survey(summary_data, "Total Performance")
  })
  
  # Scientific Spillover Total Performance Table
  output$scispill_total_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$scispill_stakeholders,
             Workplace.setting %in% input$scispill_settings)
    
    summary_data <- create_performance_analysis_survey(data, "QScientificSmall", "QScientificBroad")
    table_data <- create_performance_table_survey(summary_data)
    datatable(table_data, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Scientific Spillover Stakeholder Plot
  output$scispill_stakeholder_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$scispill_stakeholders,
             Workplace.setting %in% input$scispill_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QScientificSmall", "QScientificBroad"))
    
    create_grouped_performance_plot_survey(summary_data, "Stakeholder.type", "By Stakeholder Type")
  })
  
  # Scientific Spillover Stakeholder Table
  output$scispill_stakeholder_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$scispill_stakeholders,
             Workplace.setting %in% input$scispill_settings)
    
    summary_data <- data %>%
      group_by(Stakeholder.type) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QScientificSmall", "QScientificBroad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Stakeholder.type")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 8, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Stakeholder Type', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # Scientific Spillover Setting Plot
  output$scispill_setting_plot <- renderPlot({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$scispill_stakeholders,
             Workplace.setting %in% input$scispill_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QScientificSmall", "QScientificBroad"))
    
    create_grouped_performance_plot_survey(summary_data, "Workplace.setting", "By Workplace Setting")
  })
  
  # Scientific Spillover Setting Table
  output$scispill_setting_table <- renderDT({
    data <- processed_survey_data() %>%
      filter(Stakeholder.type %in% input$scispill_stakeholders,
             Workplace.setting %in% input$scispill_settings)
    
    summary_data <- data %>%
      group_by(Workplace.setting) %>%
      group_modify(~ create_performance_analysis_survey(.x, "QScientificSmall", "QScientificBroad"))
    
    table_data <- create_grouped_performance_table_survey(summary_data, "Workplace.setting")
    
    datatable(
      table_data, 
      options = list(dom = 't', pageLength = 6, autoWidth = TRUE), 
      rownames = FALSE,
      colnames = c('Workplace Setting', 'Method', 'Mean ± SD (95% CI)')
    )
  })
  
  # ---------------------------------------------------------
  # OUTPUTS - AGGREGATE SCORES (for Uncertainty Tab)
  # ---------------------------------------------------------
  
  # Aggregate scores plot
  output$aggregate_plot <- renderPlot({
    # Add requirement to ensure data is available
    req(input$update_aggregate_survey)
    
    data <- aggregated_scores()
    
    # Check if data is valid
    if (is.null(data) || nrow(data) == 0) {
      # Return empty plot with message
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available. Click 'Recompute Aggregate'.") +
        theme_void()
    } else {
      # Prepare data for plotting
      plot_data <- data %>%
        select(Criterion, TP_Aggregated, CGP_Aggregated) %>%
        pivot_longer(cols = c(TP_Aggregated, CGP_Aggregated), 
                     names_to = "Method", values_to = "Score") %>%
        mutate(
          Method = ifelse(Method == "TP_Aggregated", "TP", "CGP"),
          Criterion_Label = criterion_names[Criterion]
        )
      
      ggplot(plot_data, aes(x = Score, y = Method, fill = Criterion_Label)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = criterion_colors_display, name = "Criterion") +
        labs(title = "Aggregated Scores by Criterion",
             x = "Aggregated Score (Performance × Weight)",
             y = "") +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })
  
  # Aggregate scores table
  output$aggregate_table <- renderDT({
    # Add requirement to ensure data is available
    req(input$update_aggregate_survey)
    
    data <- aggregated_scores()
    
    # Check if data is valid
    if (is.null(data) || nrow(data) == 0) {
      # Return empty datatable with message
      datatable(
        data.frame(Message = "No data available. Click 'Recompute Aggregate'."),
        options = list(dom = 't'),
        rownames = FALSE
      )
    } else {
      summary_data <- data %>%
        group_by(Criterion) %>%
        summarise(
          Weight = first(Weight),
          TP_Score = first(TP_Score),
          CGP_Score = first(CGP_Score),
          TP_Aggregated = first(TP_Aggregated),
          CGP_Aggregated = first(CGP_Aggregated),
          .groups = "drop"
        ) %>%
        mutate(Criterion = criterion_names[Criterion])
      
      total_data <- data.frame(
        Criterion = "TOTAL",
        Weight = sum(data$Weight),
        TP_Score = sum(data$TP_Score * data$Weight) / sum(data$Weight),
        CGP_Score = sum(data$CGP_Score * data$Weight) / sum(data$Weight),
        TP_Aggregated = sum(data$TP_Aggregated),
        CGP_Aggregated = sum(data$CGP_Aggregated)
      )
      
      final_data <- bind_rows(summary_data, total_data)
      
      datatable(
        final_data,
        options = list(dom = 't', pageLength = 10),
        rownames = FALSE,
        colnames = c('Criterion', 'Weight', 'TP Score', 'CGP Score', 'TP Aggregated', 'CGP Aggregated')
      ) %>%
        formatRound(columns = c(2, 3, 4, 5, 6), digits = 1)
    }
  })
  
  # Criterion detail table (if needed elsewhere)
  output$criterion_table <- renderDT({
    req(input$update_aggregate_survey)
    
    data <- aggregated_scores()
    
    if (is.null(data) || nrow(data) == 0) {
      datatable(
        data.frame(Message = "No data available"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    } else {
      data %>%
        mutate(Criterion = criterion_names[Criterion]) %>%
        select(Criterion, Weight, TP_Score, CGP_Score, TP_Aggregated, CGP_Aggregated) %>%
        datatable(
          options = list(dom = 't', pageLength = 10),
          rownames = FALSE,
          colnames = c('Criterion', 'Weight', 'TP Score', 'CGP Score', 'TP Aggregated', 'CGP Aggregated')
        ) %>%
        formatRound(columns = c(2, 3, 4, 5, 6), digits = 1)
    }
  })
  # ---------------------------------------------------------
  # RETURN MODULE COMPONENTS
  # ---------------------------------------------------------
  
  return(list(
    data = list(
      survey_data = processed_survey_data,
      mean_weights = mean_weights,
      aggregated_scores = aggregated_scores
    ),
    utilities = list(
      get_survey_data = processed_survey_data,
      get_mean_weights = mean_weights,
      get_aggregated_scores = aggregated_scores,
      criterion_names = criterion_names,
      criterion_colors = criterion_colors_display
    )
  ))
}