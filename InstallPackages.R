# Install all packages for the directory.

packages <- c("tidyverse",
              "lubridate",
              "Matrix", 
              "lme4",
              "Rcpp",
              "rstanarm", 
              "magrittr",
              "abind",
              "jsonlite",
              "highcharter",
              "shinydashboard",
              "scales",
              "shiny",
              "readxl",
              "gridExtra",
              "performance",
              "MuMIn")

installed <- packages %in% rownames(installed.packages())
if (any(installed == FALSE)) {
  install.packages(packages[!installed])
}

invisible(lapply(packages,library,character.only=T))
