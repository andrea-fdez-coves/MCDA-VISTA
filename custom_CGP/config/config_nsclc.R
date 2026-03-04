#' NSCLC Case Study Configuration
#' 
#' This configuration file contains pre-defined data for the Non-Small Cell
#' Lung Cancer (NSCLC) case study, including decision problem parameters
#' and criteria hierarchy for comprehensive genomic profiling assessment.
#'
#' @format A list containing:
#' \describe{
#'   \item{config_name}{Configuration name}
#'   \item{config_description}{Configuration description}
#'   \item{config_version}{Configuration version}
#'   \item{config_author}{Configuration author}
#'   \item{config_date}{Configuration date}
#'   \item{decision_problem}{List of decision problem parameters}
#'   \item{locked_fields}{List of locked UI fields}
#'   \item{criteria}{List containing criteria data structure}
#'   \item{performance}{List containing performance assessment configuration}
#' }
#'
#' @details This configuration is based on a comprehensive genomic profiling
#' (CGP) assessment for NSCLC patients, with criteria organized in a 2-level
#' hierarchical structure covering 7 main domains and 15 sub-criteria.
#' Performance assessment is configured to use only Level 1 criteria with
#' direct rating scoring method, with both settings locked.
#' 
#' @examples
#' # Load the configuration in your app:
#' # source("config/config_nsclc.R", local = TRUE)
#' # criteria_module <- criteria_definition_module(input, output, session, config_nsclc)
#' # performance_module <- performance_main_module(input, output, session, 
#' #   decision_data = decision_module$data,
#' #   criteria_data = criteria_module$data,
#' #   config = config_nsclc)
#' 
#' @export
config_nsclc <- list(
  config_name = "NSCLC Case Study: Comprehensive Genomic Profiling Assessment",
  config_description = "Non-small cell lung cancer decision analysis for comprehensive genomic profiling implementation. This case study evaluates the value of implementing CGP testing versus standard care for advanced NSCLC patients.",
  config_version = "2.0",
  config_author = "Andrea Fernández Coves - ORCID (0009-0000-7698-3849)",
  config_date = Sys.Date(),
  
  # ---------------------------------------------------------
  # Decision Problem Configuration
  # ---------------------------------------------------------
  
  decision_problem = list(
    population = "Adult patients with advanced non-small cell lung cancer (Stage IIIB-IV)",
    intervention = "Comprehensive Genomic Profiling",
    comparators = "Targeted Profiling",
    outcomes = "Overall Survival\nProgression Free Survival\nQuality-adjusted life years (QALYs)\nCosts\nFeasibility\nPatient and physician Test Journey\nWider Implications for diagnostic results\nOrganization of Laboratories\nScientific Spillover",
    time_frame = "Lifetime horizon",
    incidence = 45000,
    prevalence = 120000,
    severity = 0.8,
    objective = "To evaluate the value of implementing comprehensive genomic profiling versus standard testing approaches for advanced NSCLC patients, considering clinical, economic, and additional dimensions"
  ),
  
  locked_fields = list(
    #DECISION
    population = TRUE,
    intervention = TRUE,
    comparators = TRUE,
    outcomes = TRUE,
    time_frame = TRUE,
    incidence = TRUE,
    prevalence = TRUE,
    severity = TRUE,
    objective = TRUE,
    #CRITERIA
    criteria_names = TRUE,
    criteria_structure = TRUE
  ),
  
  # ---------------------------------------------------------
  # Criteria Configuration
  # ---------------------------------------------------------
  
  criteria = list(
    # Data frame containing all criteria with hierarchical structure
    df = data.frame(
      id = 1:22,
      name = c(
        # Level 1 Criteria (Main Domains)
        "Clinical benefit",
        "Cost-Effectiveness", 
        "Feasibility",
        "Patient/physician Test Journey",
        "Wider Implications for diagnostic results",
        "Organization of Laboratories",
        "Scientific Spillover",
        
        # Level 2 Criteria - Feasibility Sub-criteria
        "Data storage",
        "Interpretation of test results",
        "Tissue quality",
        
        # Level 2 Criteria - Patient/physician Test Journey Sub-criteria
        "Turnaround time",
        "Number of biopsy procedures", 
        "Completeness of results",
        
        # Level 2 Criteria - Wider Implications Sub-criteria
        "Treatment options",
        "Biomarkers for non-response or prognosis",
        "Germline alterations",
        "Pharmacogenomic profiles",
        "Biomarkers for informing diagnosis",
        
        # Level 2 Criteria - Organization of Laboratories Sub-criteria
        "Test uniformity",
        "Future proofness",
        
        # Level 2 Criteria - Scientific Spillover Sub-criteria
        "Infrastructure learning healthcare system",
        "Impact learning healthcare system"
      ),
      
      definition = c(
        # Level 1 Definitions
        "Effectiveness of care following evidence-based medicine principles/ Proven ability to improve patient health outcomes relative to existing standard treatments.",
        "Trade-off between treatment effectiveness and the costs incurred to achieve this effect. Measured as costs per QALY.",
        "To what extent are the available infrastructure and human resources suitable to perform the diagnostic tests",
        "The diagnostic experience from the request of the test till the retrieval of the results",
        "Possibility of using test results for the identification of various clinical factors",
        "The impact of the diagnostic test on the organisation of laboratories",
        "Potential of using the test results to extend scientific knowledge",
        
        # Level 2 Definitions - Feasibility
        "Preservation of sequencing data for a minimum of five years, enabling reanalysis in the future",
        "Availability of expertise to interpret results, potentially requiring a higher degree of expertise due to complexity of CGP results",
        "Collection of tissue with sufficient quality and quantity for sequencing, as required by the sequencing platform",
        
        # Level 2 Definitions - Patient/physician Test Journey
        "Time interval from requesting a test or sample collection to sharing the results",
        "Total number of biopsy procedures required to collect tissue for genomic profiling throughout the patient trajectory",
        "Degree to which test results can be utilized throughout the patient trajectory",
        
        # Level 2 Definitions - Wider Implications
        "Identification of treatment options beyond tumour-specific on-label options, including investigational phase I-III, off-label, early access and tumour-agnostic treatments",
        "Identification of targets predicting low likelihood of treatment response or tumour prognosis",
        "Identification of potential germline alterations indicating hereditary predisposition. Confirmatory germline testing may be required",
        "Identification of pharmacogenomic profiles providing information on treatment metabolism, potentially supporting dose optimization",
        "Identification of specific targets or genomic signatures indicating refinement or revision of a diagnosis",
        
        # Level 2 Definitions - Organization of Laboratories
        "Variety of tests and workflows used in laboratories for genomic profiling",
        "Extent to which platforms require modification or replacement to incorporate novel targets",
        
        # Level 2 Definitions - Scientific Spillover
        "Presence of organizational and technical structures necessary for use of routinely obtained genomic data and patient outcomes for research",
        "Impact on improving patient care of a learning care system utilizing genomic data and patient outcomes"
      ),
      
      level = c(
        # Level 1 (Main domains)
        rep(1, 7),
        # Level 2 (Sub-criteria)
        rep(2, 15)
      ),
      
      parent_id = c(
        # Level 1 criteria have no parent (parent_id = 0)
        rep(0, 7),
        # Level 2 - Feasibility sub-criteria (parent = Feasibility, id = 3)
        rep(3, 3),
        # Level 2 - Patient/physician Test Journey sub-criteria (parent = Patient/physician Test Journey, id = 4)
        rep(4, 3),
        # Level 2 - Wider Implications sub-criteria (parent = Wider Implications for diagnostic results, id = 5)
        rep(5, 5),
        # Level 2 - Organization of Laboratories sub-criteria (parent = Organization of Laboratories, id = 6)
        rep(6, 2),
        # Level 2 - Scientific Spillover sub-criteria (parent = Scientific Spillover, id = 7)
        rep(7, 2)
      ),
      
      stringsAsFactors = FALSE
    ),
    
    # Next ID for adding new criteria (should be max id + 1)
    next_id = 23
  ),
  
  # ---------------------------------------------------------
  # Performance Configuration
  # ---------------------------------------------------------
  
  performance = list(
    # Default hierarchy levels to include in performance assessment
    default_selected_levels = c(1),  # Only Level 1 criteria
    
    # Default scoring method (direct, ahp, or value_function)
    default_scoring_method = "direct",
    
    # Default scoring justification
    default_scoring_justification = "Direct rating was selected as the scoring method. 
    Only Level 1 criteria are included in this assessment",
    
    # Score range limits
    direct_min = 0,      # Minimum score value
    direct_max = 100,    # Maximum score value
    
    # Lock specific UI elements to prevent user changes
    locked_fields = list(
      selected_levels = TRUE,    # Users cannot change which hierarchy levels are included
      scoring_method = TRUE      # Users cannot change the scoring method
    )
  ),
  
  # ---------------------------------------------------------
  # Weight Configuration 
  # ---------------------------------------------------------
  
  weights = list(
    # Default weighting method (direct, ahp, or swing)
    default_weighting_method = "direct",
    
    # Default weighting justification
    default_weighting_justification = "Direct rating weighting was selected to assign relative importance to the seven value domains. Stakeholders distributed 100 points across criteria based on their perceived importance in the NSCLC decision context.",
    
    # Direct weighting configuration
    direct = list(
      total_points = 100,  # Total points to distribute across criteria
      
      # Pre-defined weights for Level 1 criteria (as percentages)
      # These will be loaded when the app starts
      calculated_weights = c(
        "Clinical benefit" = 0,
        "Cost-Effectiveness" = 0,
        "Feasibility" = 0,
        "Patient/physician Test Journey" = 0,
        "Wider Implications for diagnostic results" = 0,
        "Organization of Laboratories" = 0,
        "Scientific Spillover" = 0
      ),
      
      # Raw point values (sum to total_points = 100)
      values = list(
        "1" = 0,  # Clinical benefit (id = 1)
        "2" = 0,  # Cost-Effectiveness (id = 2)
        "3" = 0,  # Feasibility (id = 3)
        "4" = 0,  # Patient/physician Test Journey (id = 4)
        "5" = 0,   # Wider Implications (id = 5)
        "6" = 0,   # Organization of Laboratories (id = 6)
        "7" = 0    # Scientific Spillover (id = 7)
      )
    ),
    
    # Lock specific UI elements to prevent user changes
    locked_fields = list(
      weighting_method = TRUE,  # Users cannot change the weighting method
      total_points = TRUE,      # Users cannot change total points to distribute
      weight_values = FALSE     # Users can adjust the actual weight values (set to TRUE to lock weights completely)
    )
  )
  
) 