ui <- dashboardPage(
  skin = "blue",
  
  ##### Header #####
  dashboardHeader(
    title = "MCDA-VISTA",
    tags$li(
      class = "dropdown", 
      tags$a("Value-based Interactive Shiny Tool for Appraisal")
    )
  ),
  
  ##### Sidebar #####
  dashboardSidebar(
    sidebarMenu(
      menuItem("Background", tabName = "background", icon = icon("home")),
      menuItem("Decision Problem", tabName = "decision_problem", icon = icon("question-circle")),
      menuItem("Criteria Definition", tabName = "criteria_definition", icon = icon("list-ol")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Weights", tabName = "weights", icon = icon("balance-scale")),
      menuItem("Aggregate Score", tabName = "aggregate_score", icon = icon("calculator")),
      menuItem("Uncertainty", tabName = "uncertainty", icon = icon("random")),
      menuItem("Report and Deliberation", tabName = "report", icon = icon("file-pdf"))
    )
  ),
  
  ##### Body #####
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      
      # Background tab
      tabItem(
        tabName = "background",
        h2("Background Information"),
        p("We aimed to develop an adaptive R Shiny tool to facilitate the execution and communication of MCDA within healthcare decision-making processes. In this initial version, we focused on value-measurement models, as they represent the most widely applied MCDA methods across healthcare decision contexts."),
        p("This tool is currently under development."),
        p(strong("Disclaimer:"), " MCDA is intended to support decision-making, not replace it. Please take the results of this tool as input for deliberation and further decision-making, not as a final assessment."),
        p("For any issues please contact andrea.fernandez.coves@mumc.nl")
      ),
      
      ###### Decision Problem tab ######
      tabItem(
        tabName = "decision_problem",
        h2("Decision Problem"),
        
        fluidRow(
          box(
            title = "Information", status = "warning", solidHeader = TRUE, width = 12,
            p("The decision context describes the broader circumstances in which the appraisal takes place, including the epidemiological, clinical, and societal factors that may influence the decision. Defining the decision context helps ensure transparency and supports consistent deliberation."),
            p("For further reading, we recommend: Multiple Criteria Decision Analysis for Health Care Decision Making—Emerging Good Practices: Report 1 and 2")
          )
        ),
        
        fluidRow(
          box(
            title = "PICOT Elements", status = "primary", solidHeader = TRUE, width = 6,
            textInput("population", "Population:", placeholder = "Describe target population"),
            textInput("intervention", "Intervention:", placeholder = "Describe the intervention"),
            h4("Comparators:"),
            helpText("Enter one comparator per line"),
            textAreaInput("comparators", NULL, placeholder = "e.g., Standard care\nPlacebo", rows = 3),
            h4("Outcomes:"),
            helpText("Enter one outcome per line"),
            textAreaInput("outcomes", NULL, placeholder = "e.g., Mortality\nQuality of life", rows = 5),
            textInput("time_frame", "Time frame:", placeholder = "e.g., One-off, 6 months, 20 years")
          ),
          
          box(
            title = "Decision Context", status = "primary", solidHeader = TRUE, width = 6,
            sliderInput("incidence", "Incidence per year:", min = 0, max = 100000, value = 5000, step = 1000),
            sliderInput("prevalence", "Prevalence per year:", min = 0, max = 500000, value = 25000, step = 5000),
            sliderInput("severity", "Disease Severity (0-1 scale):", min = 0, max = 1, value = 0.5, step = 0.1)
          ),
          
          box(
            title = "Objective", status = "primary", solidHeader = TRUE, width = 6,
            p("Please define the objective of this appraisal."),
            textAreaInput("objective_definition_DP", NULL, placeholder = " Rank strategies/Determine the strategy that bring the most value/...", rows = 6)
          )
        ),
        
        fluidRow(
          box(
            title = "Notes", status = "primary", solidHeader = TRUE, width = 12,
            textAreaInput("deliberation_notes_DP", NULL, placeholder = "Enter notes here...", rows = 6)
          )
        ),
        
        fluidRow(
          box(
            width = 12, align = "center",
            actionButton("save_decision", "Save All Decision Parameters", class = "btn-primary", icon = icon("save"))
          )
        )
      ),
      
      ###### Criteria Definition tab ######
      tabItem(
        tabName = "criteria_definition",
        h2("Criteria Definition"),
        
        fluidRow(
          box(
            title = "Information", status = "warning", solidHeader = TRUE, width = 12,
            p("Define your decision criteria. Check that your criteria ensures"),
            tags$ul(
              tags$li("Completeness"),
              tags$li("Non redundancy"),
              tags$li("Nonoverlap"),
              tags$li("Preference independence")
            ),
            p("The highest level is 1. Each criterion of level 2 must have a parent of level 1, and so on."),
            tags$ul(
              tags$li("Level 1: Top-level criteria (no parent required)"),
              tags$li("Level 2: Sub-criteria of Level 1"),
              tags$li("Level 3: Sub-criteria of Level 2"),
              tags$li("Level 4: Sub-criteria of Level 3")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Define Criteria", status = "primary", solidHeader = TRUE, width = 6,
            textInput("criterion_name", "Criterion Name:", placeholder = "Enter criterion name"),
            selectInput(
              "criterion_level", "Hierarchy Level:",
              choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4),
              selected = 1
            ),
            conditionalPanel(
              condition = "input.criterion_level != 1",
              uiOutput("parent_criteria_ui")
            ),
            textAreaInput("criterion_definition", "Definition:", 
                          placeholder = "Provide a clear definition of this criterion", 
                          rows = 3),
            actionButton("add_criterion", "Add Criterion", class = "btn-success")
          ),
          
          box(
            title = "Criteria Hierarchy", status = "primary", solidHeader = TRUE, width = 6,
            div(
              style = "min-height: 300px; border: 1px solid #ddd; padding: 10px;",
              uiOutput("hierarchy_display")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Current Criteria", status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("criteria_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Notes", status = "info", solidHeader = TRUE, width = 12,
            textAreaInput("criteria_notes", NULL, placeholder = "Enter notes here...", rows = 6)
          )
        ),
        
        fluidRow(
          box(
            width = 12, align = "center",
            actionButton("save_criteria", "Save Criteria and Notes", class = "btn-primary", icon = icon("save"))
          )
        )
      ),
      
      ###### Performance score #####
      tabItem(tabName = "performance",
              h2("Performance Assessment"),
              
              # 1. Information box
              fluidRow(
                box(
                  title = "Information", status = "warning", solidHeader = TRUE, width = 12,
                  p("This section has two parts:"),
                  tags$ul(
                    tags$li("1. Performance Matrix: Enter qualitative performance data and sources"),
                    tags$li("2. Performance Scores: Quantitatively score each option")
                  ),
                  p("Complete both sections for a comprehensive assessment.")
                )
              ),
              
              #Hierarchy selection
              fluidRow(
                box(
                  title = "Select Hierarchy Levels to Include", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  p("Select which levels of the criteria hierarchy you want to include in the performance assessment:"),
                  checkboxGroupInput(
                    "selected_levels",
                    label = NULL,
                    choices = c(
                      "Level 1 " = 1,
                      "Level 2 " = 2,
                      "Level 3 " = 3,
                      "Level 4 " = 4
                    ),
                    selected = c(1, 2, 3, 4),
                    inline = TRUE
                  )
                )
              ),
              
              # 2. Performance Matrix
              fluidRow(
                box(
                  title = "Performance Matrix", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("performance_matrix"),
                  br(),
                  actionButton("save_matrix", "Save Performance Matrix", 
                               class = "btn-primary", icon = icon("save"))
                )
              ),
              
              # Information box
              fluidRow(
                box(
                  title = "Information", status = "warning", solidHeader = TRUE, width = 12,
                  p("Assess the performance of each option against the defined criteria."),
                  p("Three scoring approaches are available:"),
                  tags$ul(
                    tags$li(strong("Direct Rating:"), " Assign scores directly to each alternative using the sliders below. 
              Scores range from 0 (poor performance) to 100 (excellent performance), where:"),
                    tags$li(strong("Analytical Hierarchy Process (AHP):"), 
                            " Perform pairwise comparisons between alternatives for each criterion. 
              Each comparison is scored from 1 to 9, reflecting relative importance, 
              with 1 meaning equal importance and 9 meaning extreme importance of one option over another. 
              The system calculates normalized scores and checks for consistency of judgments."),
                    tags$li(strong("Value Functions:"), 
                            " Transform raw performance values into standardized scores using a mathematical function. 
              Currently, only Linear and Square functions are supported. Linear functions apply a straight-line mapping, 
              while Square functions allow curvature to reflect nonlinear value perceptions. 
              Value functions help capture how improvements in outcomes translate into value.")
                  ),
                  p("After selecting your preferred scoring method, please justify your choice in the 'Method Justification' box below."),
                  p("For further reading on these methods, see:"),
                  tags$ul(
                    tags$li("Marsh K. et al. (2017). Multiple criteria decision analysis for health care decision making – An introduction. _Value in Health_, 20(1), 1-7."),
                    tags$li("Keeney R. L., & Raiffa H. (1993). _Decisions with Multiple Objectives: Preferences and Value Trade-Offs_. Cambridge University Press."),
                    tags$li("ISPOR MCDA Emerging Good Practices Task Force (2016). Multiple Criteria Decision Analysis for Health Care Decision Making—Emerging Good Practices: Report 2. _Value in Health_, 19(2), 125-137.")
                  )
                )
              ),
              
              # Scoring method selection
              fluidRow(
                box(
                  title = "Scoring Method", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(
                      width = 12,
                      selectInput(
                        "scoring_method",
                        "Select Scoring Method:",
                        choices = c(
                          "Direct Rating" = "direct",
                          "Analytical Hierarchy Process (AHP)" = "ahp",
                          "Value Function" = "value_function"
                        ),
                        selected = "direct"
                      ),
                      conditionalPanel(
                        condition = "input.scoring_method == 'direct'",
                        fluidRow(
                          column(
                            width = 6,
                            numericInput("direct_min", "Minimum Value:", value = 0, min = 0, max = 100)
                          ),
                          column(
                            width = 6,
                            numericInput("direct_max", "Maximum Value:", value = 100, min = 0, max = 100)
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Method justification", 
                  status = "info",
                  solidHeader = TRUE, 
                  width = 12,
                  column(
                    width = 12,
                    textAreaInput("scoring_justification", "Justification for Method Choice:", 
                                  placeholder = "Explain why you chose this scoring method...",
                                  rows = 3)
                  )
                )
              ),
              
              # Direct Rating Section (conditionally shown)
              conditionalPanel(
                condition = "input.scoring_method == 'direct'",
                fluidRow(
                  box(
                    title = "Direct Performance Rating", 
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    p("Rate the performance of each option against the criteria:"),
                    fluidRow(
                      box(
                        title = textOutput("intervention_perf_title"), 
                        status = "primary", solidHeader = TRUE, width = 6,
                        uiOutput("intervention_performance_ui")
                      ),
                      box(
                        title = "Comparators Performance", 
                        status = "primary", solidHeader = TRUE, width = 6,
                        uiOutput("comparators_performance_ui")
                      )
                    )
                  ),
                  # Graphic results (shown for all methods)
                  box(
                    title = "Performance Comparison", 
                    status = "primary",
                    solidHeader = TRUE, 
                    width = 12,
                    tabsetPanel(
                      tabPanel(
                        "Stacked Bars",
                        plotlyOutput("performance_bar_chart", height = "420px")
                      ),
                      tabPanel(
                        "Spider Chart",
                        plotlyOutput("performance_spider_plot", height = "420px")
                      ),
                      tabPanel(
                        "Bar by Criterion", 
                        plotlyOutput("performance_by_criterion", height = "420px")
                      )
                    )
                  )
                ),
                
                # Save button for direct rating
                fluidRow(
                  box(
                    width = 12, align = "center",
                    actionButton("save_direct_performance", "Save Direct Performance Scores", 
                                 class = "btn-primary", icon = icon("save"))
                  )
                )
              ),
              
              # AHP Section (conditionally shown)
              conditionalPanel(
                condition = "input.scoring_method == 'ahp'",
                fluidRow(
                  box(
                    title = "AHP Performance Scoring", 
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    p("For each criterion, perform pairwise comparisons between alternatives:"),
                    uiOutput("ahp_scoring_ui"),
                    actionButton("calculate_ahp_scores", "Calculate AHP Performance Scores", class = "btn-primary"),
                    br(), br(),
                    
                    # AHP Results in the same format as weights
                    h4("AHP Performance Results"),
                    DTOutput("ahp_weights_comparison_matrix"),
                    verbatimTextOutput("ahp_performance_scores_output")
                    #verbatimTextOutput("ahp_consistency_output"),
                    #h4("All Pairwise Comparison Matrices"),
                    #DTOutput("ahp_weights_full_matrix"),
                    #h4("Normalized Performance Matrix"),
                    #verbatimTextOutput("ahp_normalized_matrix_output")
                  )
                ),
                
                # Graphic results for AHP scoring
                box(
                  title = "Performance Comparison", 
                  status = "primary",
                  solidHeader = TRUE, 
                  width = 12,
                  tabsetPanel(
                    tabPanel(
                      "Stacked Bars",
                      plotlyOutput("ahp_performance_bar_chart", height = "420px")
                    ),
                    tabPanel(
                      "Spider Chart",
                      plotlyOutput("ahp_performance_spider_plot", height = "420px")
                    ),
                    tabPanel(
                      "Bar by Criterion", 
                      plotlyOutput("ahp_performance_by_criterion", height = "420px")
                    ),
                    tabPanel(
                      "Heat Map",
                      plotlyOutput("ahp_performance_heatmap", height = "420px")
                    )
                  )
                ),
                
                # Save button for AHP scoring
                fluidRow(
                  box(
                    width = 12, align = "center",
                    actionButton("save_ahp_scores", "Save AHP Performance Scores", 
                                 class = "btn-primary", icon = icon("save"))
                  )
                )
              ),
              
              # Value Function Section 
              conditionalPanel(
                condition = "input.scoring_method == 'value_function'",
                fluidRow(
                  box(
                    title = "Value Function Definition", 
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    p("Define the value function for each criterion. Select a criterion to define its function:"),
                    
                    # Criterion selection
                    fluidRow(
                      column(
                        width = 6,
                        selectInput(
                          "selected_criterion",
                          "Select Criterion:",
                          choices = NULL
                        )
                      ),
                      column(
                        width = 6,
                        actionButton("apply_to_all", "Apply to All Criteria", class = "btn-warning",
                                     icon = icon("copy"))
                      )
                    ),
                    
                    # Function definition for selected criterion
                    fluidRow(
                      column(
                        width = 6,
                        selectInput(
                          "value_function_type",
                          "Function Type:",
                          choices = c("Linear" = "linear", "Square" = "square"),
                          selected = "linear"
                        )
                      ),
                      column(
                        width = 6,
                        conditionalPanel(
                          condition = "input.value_function_type == 'linear'",
                          numericInput("linear_m", "Slope (m):", value = 1, step = 0.1),
                          numericInput("linear_b", "Intercept (b):", value = 0, step = 0.1)
                        ),
                        conditionalPanel(
                          condition = "input.value_function_type == 'square'",
                          numericInput("square_a", "Coefficient a:", value = 1, step = 0.1),
                          numericInput("square_b", "Coefficient b:", value = 0, step = 0.1),
                          numericInput("square_c", "Coefficient c:", value = 0, step = 0.1)
                        )
                      )
                    ),
                    
                    # Function preview and save
                    fluidRow(
                      column(
                        width = 12,
                        wellPanel(
                          h5("Function Preview:"),
                          verbatimTextOutput("function_preview"),
                          actionButton("save_function", "Save Function for Selected Criterion", 
                                       class = "btn-primary")
                        )
                      )
                    ),
                    
                    # Saved functions table
                    fluidRow(
                      column(
                        width = 12,
                        h4("Saved Functions"),
                        DTOutput("saved_functions_table")
                      )
                    )
                  )
                ),
                
                # Performance input for value function
                fluidRow(
                  box(
                    title = "Performance Values Input", 
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    p("Enter the raw performance values for each option:"),
                    
                    fluidRow(
                      box(
                        title = textOutput("intervention_value_title"), 
                        status = "primary", solidHeader = TRUE, width = 6,
                        uiOutput("intervention_value_ui")
                      ),
                      box(
                        title = "Comparators Performance Values", 
                        status = "primary", solidHeader = TRUE, width = 6,
                        uiOutput("comparators_value_ui")
                      )
                    )
                  )
                ),
                
                # Graphic results
                box(
                  title = "Performance Comparison", 
                  status = "primary",
                  solidHeader = TRUE, 
                  width = 12,
                  tabsetPanel(
                    tabPanel(
                      "Stacked Bars",
                      plotlyOutput("value_function_bar_chart", height = "420px")
                    ),
                    tabPanel(
                      "Spider Chart",
                      plotlyOutput("value_function_spider_plot", height = "420px")
                    ),
                    tabPanel(
                      "Bar by Criterion", 
                      plotlyOutput("value_function_by_criterion", height = "420px")
                    ),
                    tabPanel(
                      "Raw vs Transformed",
                      plotlyOutput("value_function_comparison", height = "420px")
                    )
                  )
                ),
                
                # Save button for value function
                fluidRow(
                  box(
                    width = 12, align = "center",
                    actionButton("save_value_function", "Save Value Function Scores", 
                                 class = "btn-primary", icon = icon("save"))
                  )
                )
              )
      ),
      
      ###### Weights tab ######
      tabItem(
        tabName = "weights",
        h2("Weighting Criteria"),
        
        fluidRow(
          box(
            title = "Information", status = "warning", solidHeader = TRUE, width = 12,
            p("Assign weights to each criterion to reflect their relative importance in the decision. The total of all weights must equal 100%."),
            p("Three weighting approaches are available:"),
            tags$ul(
              tags$li(strong("Direct Rating:"), " Assign points directly to each criterion. The total points distributed across all criteria represent their relative importance. This method is simple and intuitive, but requires careful attention to ensure the total adds up correctly."),
              tags$li(strong("Analytical Hierarchy Process (AHP):"), " Perform pairwise comparisons between criteria, scoring each comparison from 1 to 9, where 1 means equal importance and 9 means extreme importance of one criterion over another. AHP calculates normalized weights and consistency ratios to help validate judgments."),
              tags$li(strong("Swing Weighting:"), " Compare criteria based on the improvement from worst to best performance. Identify the criterion that would provide the greatest value if improved first and assign points (0–100) to other criteria relative to it. This method emphasizes the decision impact of moving from worst to best outcomes.")
            ),
            p("After selecting your preferred weighting method, please justify your choice in the 'Method Justification' box below."),
            p("For further reading on these methods, see:"),
            tags$ul(
              tags$li("Marsh K. et al. (2017). Multiple criteria decision analysis for health care decision making – An introduction. _Value in Health_, 20(1), 1-7."),
              tags$li("Keeney R. L., & Raiffa H. (1993). _Decisions with Multiple Objectives: Preferences and Value Trade-Offs_. Cambridge University Press."),
              tags$li("Saaty T. L. (1980). _The Analytic Hierarchy Process_. McGraw-Hill."),
              tags$li("ISPOR MCDA Emerging Good Practices Task Force (2016). Multiple Criteria Decision Analysis for Health Care Decision Making—Emerging Good Practices: Report 2. _Value in Health_, 19(2), 125-137.")
            )
          )
        ),
        
        # Weighting method selection
        fluidRow(
          box(
            title = "Weighting Method", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(
                width = 12,
                selectInput(
                  "weighting_method",
                  "Select Weighting Method:",
                  choices = c(
                    "Direct Rating" = "direct",
                    "Analytical Hierarchy Process (AHP)" = "ahp",
                    "Swing Weighting" = "swing"
                  ),
                  selected = "direct"
                )
              )
            )
          )
        ),
        
        # Method justification
        fluidRow(
          box(
            title = "Method justification", 
            status = "info",
            solidHeader = TRUE, 
            width = 12,
            column(
              width = 12,
              textAreaInput("weighting_justification", "Justification for Method Choice:", 
                            placeholder = "Explain why you chose this weighting method...",
                            rows = 3)
            )
          )
        ),
        
        # Direct Weighting Section
        conditionalPanel(
          condition = "input.weighting_method == 'direct'",
          fluidRow(
            box(
              title = "Direct Weight Assignment", 
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              # Total points input
              fluidRow(
                column(
                  width = 6,
                  numericInput("total_points", "Total Points to Distribute:", 
                               value = 100, min = 1, max = 1000, step = 1)
                ),
                column(
                  width = 6,
                  wellPanel(
                    strong("Current Total:"),
                    textOutput("current_weight_total"),
                    style = "margin-top: 25px;"
                  )
                )
              ),
              
              # Weight inputs for each criterion
              uiOutput("direct_weights_ui"),
              
              # Action buttons
              fluidRow(
                column(
                  width = 12,
                  actionButton("calculate_direct_weights", "Calculate Weights", 
                               class = "btn-primary", icon = icon("calculator")),
                  actionButton("reset_weights", "Reset Weights", 
                               class = "btn-warning", icon = icon("refresh"))
                )
              )
            )
          ),
          
          # Visualization section
          fluidRow(
            box(
              title = "Weight Distribution", 
              status = "success",
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("weights_pie_chart", height = "400px")
            ),
            box(
              title = "Weight Comparison", 
              status = "success",
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("weights_bar_chart", height = "400px")
            )
          ),
          
          # Results table
          fluidRow(
            box(
              width = 12, align = "center",
              actionButton("save_direct_weights", "Save Direct Weights", 
                           class = "btn-primary", icon = icon("save"))
            )
          )
        ),
        
        # AHP Weighting Section
        conditionalPanel(
          condition = "input.weighting_method == 'ahp'",
          fluidRow(
            box(
              title = "AHP Pairwise Comparisons for Criteria Weights", 
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              p("For each pair, select which criterion is more important and by how much:"),
              uiOutput("ahp_weights_matrix_ui"),
              actionButton("calculate_ahp_weights", "Calculate AHP Weights", class = "btn-primary"),
              br(), br(),
              h4("AHP Results"),
              DTOutput("ahp_weights_comparison_matrix"),
              verbatimTextOutput("ahp_weights_output"),
              verbatimTextOutput("ahp_weights_consistency_output"),
              h4("Pairwise Comparison Matrix"),
              DTOutput("ahp_weights_full_matrix")
            )
          ),
          
          # AHP Visualization section
          fluidRow(
            box(
              title = "AHP Weight Distribution", 
              status = "success",
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("ahp_weights_pie_chart", height = "400px")
            ),
            box(
              title = "AHP Weight Comparison", 
              status = "success",
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("ahp_weights_bar_chart", height = "400px")
            )
          ),
          
          fluidRow(
            box(
              width = 12, align = "center",
              actionButton("save_ahp_weights", "Save AHP Weights", 
                           class = "btn-primary", icon = icon("save"))
            )
          )
        ),
        
        # Swing Weighting Section
        conditionalPanel(
          condition = "input.weighting_method == 'swing'",
          fluidRow(
            box(
              title = "Swing Weighting Method", 
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              p("The Swing Weighting method compares the improvement from worst to best performance for each criterion."),
              tags$ol(
                tags$li("Imagine all criteria are at their WORST performance level"),
                tags$li("Select which criterion you would improve to its BEST performance first"),
                tags$li("Assign points (0-100) to other criteria based on their relative importance compared to the first")
              ),
              
              # Worst and Best scenario definitions
              fluidRow(
                column(
                  width = 6,
                  wellPanel(
                    h5("Worst Case Scenario:"),
                    p("All criteria at their minimum performance level"),
                    style = "background-color: #f8d7da;"
                  )
                ),
                column(
                  width = 6,
                  wellPanel(
                    h5("Best Case Scenario:"),
                    p("All criteria at their maximum performance level"),
                    style = "background-color: #d4edda;"
                  )
                )
              ),
              
              # Criterion selection for first improvement
              fluidRow(
                column(
                  width = 6,
                  selectInput(
                    "swing_first_criterion",
                    "Which criterion would you improve FIRST from worst to best?",
                    choices = NULL
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    "swing_first_weight",
                    "Points for first criterion (reference = 100):",
                    value = 100,
                    min = 0,
                    max = 100,
                    step = 1
                  )
                )
              ),
              
              # Swing weights for other criteria
              uiOutput("swing_weights_ui"),
              
              # Action buttons
              fluidRow(
                column(
                  width = 12,
                  actionButton("calculate_swing_weights", "Calculate Swing Weights", 
                               class = "btn-primary", icon = icon("calculator")),
                  actionButton("reset_swing_weights", "Reset Swing Weights", 
                               class = "btn-warning", icon = icon("refresh"))
                )
              )
            )
          ),
          
          # Visualization section
          fluidRow(
            box(
              title = "Swing Weight Distribution", 
              status = "success",
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("swing_weights_pie_chart", height = "400px")
            ),
            box(
              title = "Swing Weight Comparison", 
              status = "success",
              solidHeader = TRUE,
              width = 6,
              plotlyOutput("swing_weights_bar_chart", height = "400px")
            )
          ),
          
          # Results and explanation
          fluidRow(
            box(
              title = "Swing Weight Results", 
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              DTOutput("swing_weights_results_table"),
              br(),
              actionButton("save_swing_weights", "Save Swing Weights", 
                           class = "btn-primary", icon = icon("save"))
            ),
            box(
              title = "Swing Weight Explanation",
              status = "info",
              solidHeader = TRUE,
              width = 6,
              p("Swing weights represent the relative importance of moving from worst to best performance on each criterion."),
              p("The weights are normalized to sum to 1 (or 100%) for use in the aggregate score calculation.")
            )
          )
        )
      ),
      
      ###### Aggregate score tab ######
      tabItem(tabName = "aggregate_score", 
              h2("Aggregate Score Results"),
              
              # Information box
              fluidRow(
                box(
                  title = "Information", status = "warning", solidHeader = TRUE, width = 12,
                  p("This section shows the final aggregate scores calculated by combining:"),
                  tags$ul(
                    tags$li("Performance scores from the Performance Assessment tab"),
                    tags$li("Weights from the Weighting Criteria tab")
                  ),
                  p("The aggregate score represents the overall value of each alternative.")
                )
              ),
              
              # Charts
              fluidRow(
                box(
                  title = "Alternative Comparison (Radar Chart)", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("radar_chart", height = "500px")
                ),
                box(
                  title = "Score Composition by Alternative", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("stacked_bar_chart", height = "500px")
                )
              ),
              
              # Detailed breakdown
              fluidRow(
                box(
                  title = "Detailed Score Breakdown", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("detailed_breakdown_table")
                )
              ),
              
              # Notes section
              fluidRow(
                box(
                  title = "Aggregate Score Notes", status = "primary", solidHeader = TRUE, width = 12,
                  textAreaInput("aggregate_notes", NULL, placeholder = "Enter notes about the aggregate scores and final decision here...", rows = 6)
                )
              ),
              
              fluidRow(
                box(
                  width = 12, align = "center",
                  actionButton("save_aggregate_notes", "Save Aggregate Score Notes", class = "btn-primary", icon = icon("save"))
                )
              )
      ),
      
      ###### Uncertainty tab ######
      tabItem(
        tabName = "uncertainty",
        h2("Uncertainty Analysis"),
        
        # Information box about uncertainty importance
        fluidRow(
          box(
            title = "Importance of Reporting Uncertainty in MCDA", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 12,
            p("This tab is designed to help you identify and report uncertainties in your decision analysis. Documenting uncertainty is critical for transparent and robust decision-making, and helps users understand the reliability and limitations of the results."),
            p("Four main types of uncertainty can affect your analysis:"),
            tags$ul(
              tags$li(strong("Structural uncertainty:"), " Uncertainty about the way the decision problem is modeled, including assumptions about the relationships between variables."),
              tags$li(strong("Stochastic uncertainty:"), " Variability due to chance events or random fluctuations in the system being modeled."),
              tags$li(strong("Parameter uncertainty:"), " Uncertainty about the true values of model parameters, such as probabilities, costs, or effectiveness estimates."),
              tags$li(strong("Heterogeneity:"), " Differences across populations or subgroups that may affect the outcomes and applicability of the results.")
            ),
            p("For each uncertainty, please provide:"),
            tags$ul(
              tags$li("Type: type of uncertainty"),
              tags$li("Source: Where the uncertainty originates"),
              tags$li("Definition: A clear description of the uncertainty"),
              tags$li("Impact: How this uncertainty might affect your analysis"),
              tags$li("Mitigation: Strategies to address or reduce this uncertainty")
            ),
            p("This information will be included in your final report to support transparent decision-making."),
            p("Reference: Broekhuizen, H., Groothuis-Oudshoorn, C.G.M., van Til, J.A., Hummel, J.M., & IJzerman, M.J. (2015). A Review and Classification of Approaches for Dealing with Uncertainty in Multi-Criteria Decision Analysis for Healthcare Decisions. _PharmacoEconomics_, 33, 445–455. https://doi.org/10.1007/s40273-015-0232-2")
          )
        ),
        
        # Uncertainty definition table
        fluidRow(
          box(
            title = "Uncertainty Definition Table", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            DTOutput("uncertainty_table"),
            br(),
            actionButton("add_uncertainty", "Add New Uncertainty", icon = icon("plus")),
            actionButton("remove_uncertainty", "Remove Selected", icon = icon("minus"))
          )
        ),
        
        # Uncertainty Analysis Notes with Save Button
        fluidRow(
          box(
            title = "Uncertainty Analysis Notes", 
            status = "info", 
            solidHeader = TRUE, 
            width = 12,
            textAreaInput("uncertainty_notes", "Additional notes on uncertainty analysis:", 
                          rows = 4, placeholder = "Enter any additional notes about uncertainty in your analysis"),
            br(),
            actionButton("save_uncertainty_notes", "Save Notes", 
                         class = "btn-primary", icon = icon("save"))
          )
        )
      ),
      
      ###### Report tab ######
      tabItem(
        tabName = "report",
        h2("Report and Deliberation"),
        h3("Decision Problem"),
        fluidRow(
          box(
            title = "PICO Summary", status = "primary", solidHeader = TRUE, width = 6,
            tableOutput("pico_table")
          ),
          box(
            title = "Context Summary", status = "primary", solidHeader = TRUE, width = 6,
            tableOutput("context_table")
          ),
          box(
            title = "Objective Summary", status = "primary", solidHeader = TRUE, width = 6,
            verbatimTextOutput("objective_output")
          )
        ),
        h3("Criteria definition"),
        fluidRow(
          box(
            title = "Criteria Summary", status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("report_criteria_table")
          ),
          box(
            title = "Hierarchy Visualization", status = "primary", solidHeader = TRUE, width = 12,
            uiOutput("report_hierarchy_display")
          )
        ),
        h3("Performance"),
        fluidRow(
          box(
            title = "Performance Matrix", status = "primary", solidHeader = TRUE, width = 6,
            DTOutput("report_performance_matrix")
          ),
          box(
            title = "Performance Score", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            uiOutput("report_performance_ui")
          )
        ),
        h3("Weights"),
        fluidRow(
          box(
            title = "Weight Assignment Results", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            fluidRow(
              column(
                width = 6,
                h4("Weighting Method Used:"),
                verbatimTextOutput("report_weighting_method"),
                h4("Method Justification:"),
                verbatimTextOutput("report_weighting_justification"),
                br(),
                h4("Weight Summary Table:"),
                DTOutput("report_weights_table")
              ),
              column(
                width = 6,
                h4("Weight Distribution:"),
                plotlyOutput("report_weights_pie_chart", height = "400px"),
                br(),
                h4("Weight Comparison:"),
                plotlyOutput("report_weights_bar_chart", height = "300px")
              )
            ),
            fluidRow(
              column(
                width = 12,
                h4("Consistency Analysis:"),
                uiOutput("report_weights_consistency")
              )
            )
          )
        ),
        h3("Aggregate results"),
        fluidRow(
          box(
            title = "Aggregate Results Summary", status = "primary", solidHeader = TRUE, width = 6,
            DTOutput("report_aggregate_table")
          ),
          box(
            title = "Aggregate Scores Visualization", status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("report_aggregate_chart", height = "400px")
          )
        ),
        h3("Uncertainty Analysis"),
        fluidRow(
          box(
            title = "Uncertainty Analysis", status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("report_uncertainty_table")
          )
        ),
        h3("Notes"),
        fluidRow(
          box(
            title = "Deliberation Notes", status = "success", solidHeader = TRUE, width = 12,
            h4("Decision Problem Notes:"),
            verbatimTextOutput("notes_output"),
            h4("Criteria Definition Notes:"),
            verbatimTextOutput("criteria_notes_output"),
            h4("Scoring Method Justification"),
            verbatimTextOutput("report_scoring_justification"),
            h4("Weights Assignment Notes:"),
            verbatimTextOutput("weights_notes_output"),
            h4("Aggregate Score Notes:"),
            verbatimTextOutput("aggregate_notes_output"),
            h4("Uncertainty Analysis Notes:"),
            verbatimTextOutput("uncertainty_notes_output")
          )
        ),
        
        fluidRow(
          box(
            width = 12, align = "center",
            downloadButton("download_report", "Generate Word Report", class = "btn-success")          )
        )
      )
    )
  )
)