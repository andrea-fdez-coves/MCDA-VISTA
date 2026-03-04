# Description: MCDA-VISTA ao=pp
# Inspired by: Making health economics Shiny: a tutorial
# Author: Andrea Fernández Coves - ORCID (0009-0000-7698-3849)
# Publicatioon: Pending

#Inspired by:  
# Making health economics Shiny: a tutorial (https://pubmed.ncbi.nlm.nih.gov/32904933/)
# Cost-effectiveness of software with artificial intelligence algorithms for analysing 
# CT brain scans in suspected stroke: an R Shiny model (Ramaekers et al., PENDING FOR PUBLICATION)



##### Setup ##### 
options(scipen = 999) # setting for scientific notation
options(max.print = 10000) # setting for maximum output to display

# Load required packages
packages <- c(
  "shiny", "shinydashboard", "dampack", "BCEA", "ggplot2", 
  "magrittr", "dplyr", "DT", "rmarkdown", "tidyr", "knitr", 
  "tableHTML", "plotly", "summarytools", "ggpattern", "matrixStats", 
  "kableExtra", "shinyBS", "shinyTree", "data.tree", "DiagrammeR", 
  "collapsibleTree", "officer", "flextable", "shinyjs", "rsconnect", 
  "scales"
)

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))


# Clear workspace
rm(list = ls())

#### General ####
#n_title <- "MCDA-VISTA (Value-based Interactive Shiny Tool for Appraisal)"

#### Files and functions ####
# load main model functions 
source("ui.R") # shiny graphical user interface 
source("server.R") # function for shiny server


#### Running the App ####
runApp(
  appDir = shinyApp( 
    ui = ui,
    server = server,
    options = list(launch.browser = TRUE)
  )
)