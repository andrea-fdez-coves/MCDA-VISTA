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
#' }
#'
#' @details This configuration is based on a comprehensive genomic profiling
#' (CGP) assessment for NSCLC patients, with criteria organized in a 2-level
#' hierarchical structure covering 7 main domains and 15 sub-criteria.
#' 
#' @examples
#' # Load the configuration in your app:
#' # source("config/config_nsclc.R", local = TRUE)
#' # criteria_module <- criteria_definition_module(input, output, session, config_nsclc)
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
    population = "Adult patients with advanced non-small cell lung cancer (Stage IIIB-IV) who are candidates for systemic therapy",
    intervention = "Comprehensive Genomic Profiling (CGP) with next-generation sequencing to identify targetable genomic alterations",
    comparators = "Standard care with single-gene or limited panel testing\nTissue biopsy with conventional molecular testing",
    outcomes = "Identification of targetable genomic alterations\nTime to appropriate targeted therapy initiation\nOverall survival\nQuality-adjusted life years (QALYs)\nTest turnaround time\nDiagnostic yield",
    time_frame = "5-year time horizon for outcomes assessment",
    incidence = 45000,
    prevalence = 120000,
    severity = 0.8,
    objective = "To evaluate the value of implementing comprehensive genomic profiling versus standard testing approaches for advanced NSCLC patients, considering clinical, economic, and operational dimensions"
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
  )
) 