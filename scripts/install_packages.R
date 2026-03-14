# Install required R packages if not already installed
packages <- c('tidyverse', 'jsonlite', 'data.table', 'httr')

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    message(sprintf("Installing %s...", pkg))
    install.packages(pkg, repos = 'http://cran.r-project.org', dependencies = TRUE)
  }
}

message("All packages ready!")