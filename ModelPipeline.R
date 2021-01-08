#This file includes a pipeline to read JHU data, filter the data, and then run a multilevel linear mixed effects model with rstanarm.
#InstallPackages.R installs all packages for the pipeline and loads them.
#ReadData.R downloads the data from JHU and also loads in population data.
#ModelFiles.R includes a process to filter the most recent data, and also creates the baseline matrix for making forecasts.
#ModelRun.R uses the filtered data and constructs a model with rstanarm, and generates forecasts with predictive intervals.

source("InstallPackages.R")

source("ReadData.R")

source("ModelFiles.R")

source("ModelRun.R")