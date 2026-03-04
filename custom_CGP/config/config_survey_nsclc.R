#' NSCLC Survey Configuration
#' 
#' This configuration file contains pre-defined data for the Non-Small Cell
#' Lung Cancer (NSCLC) survey analysis, including determinant structure,
#' question mappings, and analysis parameters.
#'
#' @format A list containing:
#' \describe{
#'   \item{config_name}{Configuration name}
#'   \item{config_description}{Configuration description}
#'   \item{data_source}{Path to survey data file}
#'   \item{stakeholder_groups}{Available stakeholder groups}
#'   \item{workplace_settings}{Available workplace settings}
#'   \item{determinants}{List of determinants with their sub-determinants}
#'   \item{performance_metrics}{Performance scoring configuration}
#'   \item{weights_config}{Weight calculation configuration}
#'   \item{visualization}{Visualization settings}
#' }
#'
#' @export
config_survey_nsclc <- list(
  config_name = "NSCLC Survey Analysis Configuration",
  config_description = "Survey analysis configuration for NSCLC case study, including stakeholder mapping and determinant definitions.",
  config_version = "1.0",
  config_author = "Andrea Fernández Coves - ORCID (0009-0000-7698-3849)",
  config_date = Sys.Date(),
  
  # ---------------------------------------------------------
  # Data Source Configuration
  # ---------------------------------------------------------
  data_source = list(
    file_path = "config/survey_data.csv",
    file_separator = ";",
    file_header = TRUE
  ),
  
  # ---------------------------------------------------------
  # Stakeholder Configuration
  # ---------------------------------------------------------
  stakeholders = list(
    groups = c(
      "Pulmonologist" = "Pulmonologist",
      "CMBP" = "CMBP",
      "Pathologist" = "Pathologist",
      "Patient representative" = "Patient representative"
    ),
    default_selected = c("Pulmonologist", "CMBP", "Pathologist", "Patient representative"),
    
    workplace_settings = c(
      "Academic Center" = "Academic Center",
      "General Center" = "General Center",
      "Neither (Patient representative)" = "Neither (Patient representative)"
    ),
    default_workplace_settings = c("Academic Center", "General Center", "Neither (Patient representative)")
  ),
  
  # ---------------------------------------------------------
  # Determinant Configuration
  # ---------------------------------------------------------
  determinants = list(
    # Feasibility determinant
    feasibility = list(
      id = "feasibility",
      display_name = "Feasibility Determinant",
      sub_determinants = list(
        list(
          id = "Data.storage",
          display_name = "Data Storage",
          column_name = "Data.storage"
        ),
        list(
          id = "Interpretation.of.results",
          display_name = "Interpretation of Results",
          column_name = "Interpretation.of.results"
        ),
        list(
          id = "Tissue.quality",
          display_name = "Tissue Quality",
          column_name = "Tissue.quality"
        )
      ),
      performance = list(
        small_col = "QFeasibility_Small",
        broad_col = "QFeasibility_Broad"
      )
    ),
    
    # Patient Journey determinant
    patient_journey = list(
      id = "journey",
      display_name = "Patient Journey Determinant",
      sub_determinants = list(
        list(
          id = "Number.of.biopsy.procedures",
          display_name = "Number of Biopsy Procedures",
          column_name = "Number.of.biopsy.procedures"
        ),
        list(
          id = "Completeness.of.results",
          display_name = "Completeness of Results",
          column_name = "Completeness.of.results"
        )
      ),
      performance = list(
        small_col = "QTestingPathwaySmall",
        broad_col = "QTestingPathwayBroad"
      )
    ),
    
    # Diagnostic Results determinant
    diagnostic_results = list(
      id = "diagnostic",
      display_name = "Diagnostic Results Determinant",
      sub_determinants = list(
        list(
          id = "Treatment.options",
          display_name = "Treatment Options",
          column_name = "Treatment.options"
        ),
        list(
          id = "Germline.alterations",
          display_name = "Germline Alterations",
          column_name = "Germline.alterations"
        ),
        list(
          id = "Biomarkers.for.informing.diagnosis",
          display_name = "Biomarkers for Diagnosis",
          column_name = "Biomarkers.for.informing.diagnosis"
        ),
        list(
          id = "Pharmacogenomic.profiles",
          display_name = "Pharmacogenomic Profiles",
          column_name = "Pharmacogenomic.profiles"
        ),
        list(
          id = "Biomarkers.for.non.response.or.prognosis",
          display_name = "Biomarkers for Non-response/Prognosis",
          column_name = "Biomarkers.for.non.response.or.prognosis"
        )
      ),
      performance = list(
        small_col = "QImplicationsSmall",
        broad_col = "QImplicationsBroad"
      )
    ),
    
    # Organization of Laboratories determinant
    organization = list(
      id = "orglab",
      display_name = "Laboratory Organization Determinant",
      sub_determinants = list(
        list(
          id = "Test.Uniformity",
          display_name = "Test Uniformity",
          column_name = "Test.Uniformity"
        ),
        list(
          id = "Future.proofing",
          display_name = "Future-proofing",
          column_name = "Future.proofing"
        )
      ),
      performance = list(
        small_col = "QOrganizationSmall",
        broad_col = "QOrganizationBroad"
      )
    ),
    
    # Scientific Spillover determinant
    scientific_spillover = list(
      id = "scispill",
      display_name = "Scientific Spillover Determinant",
      sub_determinants = list(
        list(
          id = "Infrastructure.for.learning.care.system",
          display_name = "Infrastructure for Learning Care System",
          column_name = "Infrastructure.for.learning.care.system"
        ),
        list(
          id = "Learning.care.system",
          display_name = "Learning Care System",
          column_name = "Learning.care.system"
        )
      ),
      performance = list(
        small_col = "QScientificSmall",
        broad_col = "QScientificBroad"
      )
    )
  ),
  
  # ---------------------------------------------------------
  # Weight Configuration for Uncertainty Tab
  # ---------------------------------------------------------
  weights = list(
    criteria = list(
      "Clinical" = "QMCDA1_Clinical",
      "Cost" = "QMCDA1_Cost",
      "Feasibility" = "QMCDA1_Feasibility",
      "Test" = "QMCDA1_Test",
      "Implications" = "QMCDA1_Implications",
      "Organization" = "QMCDA1_Organization",
      "Scientific" = "QMCDA1_Scientific"
    ),
    criterion_display_names = c(
      "Clinical" = "Clinical Benefit",
      "Cost" = "Cost Effectiveness",
      "Feasibility" = "Feasibility",
      "Test" = "Test Journey",
      "Implications" = "Wider Implications of diagnostic results",
      "Organization" = "Organization of Laboratories",
      "Scientific" = "Scientific Spillover"
    )
  ),
  
  # ---------------------------------------------------------
  # Visualization Configuration
  # ---------------------------------------------------------
  visualization = list(
    response_colors = c(
      "Agree" = "#5DA899",
      "Somewhat agree" = "#94CBEC",
      "Neither agree nor disagree" = "#DCCD7D",
      "Somewhat disagree" = "#C26A77",
      "Disagree" = "#9F4A96",
      "Don't know" = "#DDDDDD",
      "NA" = "#848484"
    ),
    response_levels = c("Agree", "Somewhat agree", "Neither agree nor disagree", 
                        "Somewhat disagree", "Disagree", "Don't know", "NA"),
    performance_colors = c(
      "TP" = "#6BAED6",
      "CGP" = "#FD8D3C"
    ),
    criterion_colors = c(
      "Clinical Benefit" = "#36454F",
      "Cost Effectiveness" = "#FFFFF0", 
      "Feasibility" = "#648FFF",
      "Test Journey" = "#785EF0",
      "Wider Implications of diagnostic results" = "#DC267F",
      "Organization of Laboratories" = "#FE6100",
      "Scientific Spillover" = "#FFB000"
    )
  ),
  
  # ---------------------------------------------------------
  # UI Configuration
  # ---------------------------------------------------------
  ui = list(
    update_button_text = "Update View",
    update_button_icon = "refresh",
    plot_height = "500px",
    performance_plot_height = "300px"
  ),
  
  # ---------------------------------------------------------
  # Locked Fields Configuration
  # ---------------------------------------------------------
  locked_fields = list(
    stakeholders = FALSE,           # Users can change stakeholder selections
    workplace_settings = FALSE,      # Users can change workplace settings
    determinants = TRUE,             # Determinant structure is locked
    performance_metrics = TRUE,      # Performance metrics are locked
    weights_config = TRUE            # Weight configuration is locked
  )
)