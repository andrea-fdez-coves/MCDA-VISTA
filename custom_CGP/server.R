server <- function(input, output, session) {
  
  #### Initialization and reactive values ####
  
  # Load all modules
  source("decision_problem_module.R", local = TRUE)
  source("criteria_definition_module.R", local = TRUE)
  source("performance_main_module.R", local = TRUE)
  source("weight_main_module.R", local = TRUE)
  source("aggregate_main_module.R", local = TRUE)
  source("uncertainty_main_module.R", local = TRUE) 
  source("word_report_module.R", local = TRUE)
  
  # Load survey specific modules
  source("survey_module.R", local = TRUE)
  
  # Load configuration
  # To use the NSCLC case, uncomment the line below:
  source("config/config_nsclc.R", local = TRUE)
  
  active_config <- config_nsclc

  
  #### Decision Problem  ####
  # The user writes the PICOT elements on the web-browser, selects the decision
  # context, and defines the objective of the appraisal. Further notes can be
  # written down. All user inputs are recorded on the report and deliberation
  # tab and can be later downloaded.
  decision_module <- decision_problem_module(input, output, session, 
                                              config = active_config
  )
  
  
  #### Criteria Definition ####
  # Users define hierarchical criteria with up to 4 levels, including definitions
  # and parent-child relationships. Supports both user-defined and pre-configured
  # criteria structures.
  criteria_module <- criteria_definition_module(
    input, output, session, 
    config = config_nsclc
  )
  
  
  #### Performance Assessment ####
  performance_module <- performance_main_module(
    input, output, session,
    decision_data = decision_module$data,
    criteria_data = criteria_module$data,
    config = active_config
  )

  #### Survey Module ####
  survey_module <- survey_module(input, output, session, config = active_config)
  
  #### Weight Assessment #### 
  weight_module <- weight_main_module(
    input, output, session,
    decision_data = decision_module$data,
    criteria_data = criteria_module$data,
    performance_data = performance_module,
    config = active_config
  )
  
  #### Aggregate Score ####
  aggregate_module <- aggregate_main_module(
    input, output, session,
    decision_data = decision_module$data,
    criteria_data = criteria_module$data,
    performance_data = performance_module$data,
    performance_module = performance_module,
    weights_data = weight_module$data
  )
  
  #### Uncertainty Module ####
  uncertainty_module <- uncertainty_main_module(
    input, output, session
  )
  
  #### Report ####
  report_module <- word_report_module(
    input, output, session,
    decision_data = decision_module$data,
    criteria_data = criteria_module$data,
    performance_data = performance_module$data,
    performance_module = performance_module,
    weights_data = weight_module$data,
    aggregate_data = aggregate_module$data,
    aggregate_module = aggregate_module,
    uncertainty_data = uncertainty_module$data,
    uncertainty_module = uncertainty_module
  )
  
}