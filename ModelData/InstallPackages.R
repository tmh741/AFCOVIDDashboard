packages <- c("tidyverse","lubridate","lme4","rstanarm", "magrittr","abind")

installed <- packages %in% rownames(installed.packages())
if (any(installed == FALSE)) {
  install.packages(packages[!installed])
}

invisible(lapply(packages,library,character.only=T))
