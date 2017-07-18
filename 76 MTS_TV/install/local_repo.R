# https://docs.microsoft.com/en-us/sql/advanced-analytics/r/create-a-local-package-repository-using-minicran

library(miniCRAN)
# library(igraph)

# Define the package source: a CRAN mirror, or an MRAN snapshot
# CRAN_mirror <- c(CRAN="https://mran.microsoft.com/snapshot/2016-04-01")
CRAN_mirror <- c(CRAN="http://cran.r-project.org")
# CRAN_mirror <- c(CRAN="http://cran.rstudio.com")

# Define the local download location
local_repo <- "~/miniCRAN"
local_repo <- path.expand("~/miniCRAN")
local_repo <- path.expand("d:/temp/miniCRAN")

# List the packages to get. Do not specify dependencies.
# pkgs_needed <- c("ggplot2", "purrr")
pkgs_needed <- c("knitr", "rmarkdown", "rsconnect", "packrat", 
                 "tidyverse", "lubridate", "readxl", "magrittr", "stringi", "hrbrthemes", "Cairo", 
                 "shiny", "shinythemes", "shinyBS", "shinyjs", "config", "DBI", "RODBC", "RPostgreSQL", 
                 "tictoc", "anytime", "fasttime", "futile.logger", "iterators", "foreach", "doParallel",
                 "DT")
# Plot the dependency graph 
plot(makeDepGraph(pkgs_needed))

# Create the local repo for source and win.binary
pkgs_expanded <- pkgDep(pkgs_needed, repos=CRAN_mirror) 
# makeRepo(pkgs_expanded, path=local_repo, repos=CRAN_mirror, type=c("source", "win.binary")) #, Rversion = "3.2")
makeRepo(pkgs_expanded, path=local_repo, repos=CRAN_mirror, type=c("source"))

# Рекомендации по работе с локально установленными пакетами
# Enumerate the packages currently available by using: 
pkgs_inst <- as.data.frame(installed.packages())
