# Description: Function to install and/or update packages 

f_packages <- function(
  packages, # packages to be installed / updated
  install = TRUE, # install new packages if required
  update = FALSE # update new packages if required
){
  # install packages if required
  if (install == TRUE) {
    new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]  # check for new packages
    if (length(new.packages)) {install.packages(new.packages)}  # install new packages (if needed)
  }
  
  # update packages and dependencies if required
  if (update == TRUE) {
    dependencies <- unname(unlist(tools::package_dependencies(packages, db = available.packages(), which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)))
    packages_deps <- c(packages, dependencies)
    outdated.packages <- packages_deps[(packages_deps %in% old.packages()[, "Package"])]
    if (length(outdated.packages)) {install.packages(outdated.packages)}  # update packages (if needed)
  }
}