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
  
  #### Decision Problem ####
  decision_module <- decision_problem_module(input, output, session)
  
  #### Criteria Definition ####
  criteria_module <- criteria_definition_module(input, output, session)
  
  #### Performance Assessment ####
  performance_module <- performance_main_module(
    input, output, session,
    decision_data = decision_module$data,
    criteria_data = criteria_module$data
  )
  
  #### Weight Assessment #### 
  weight_module <- weight_main_module(
    input, output, session,
    decision_data = decision_module$data,
    criteria_data = criteria_module$data,
    performance_data = performance_module
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