# This file includes a pipeline to read JHU data, filter the data, and then run a multilevel linear mixed effects model with rstanarm.
# The goal is to make files and automate data download, and prepare it for display in our dashboard.

# ReadData.R downloads the data from JHU and also loads in population data.
# ModelFiles.R includes a process to filter the most recent data, and also creates the baseline matrix for making forecasts.
# ModelRun.R uses the filtered data and constructs a model with rstanarm, and generates forecasts with predictive intervals.

source("InstallPackages.R")
# Installs and loads all packages for the pipeline.

source("ReadPopData.R")
# Downloads

source("ReadData.R")
# Downloads JHU data and population data.

source("ModelFiles.R")
# Uses data + COVID data to make new files and prepare for model development.

source("ModelRun.R")
# Run the COVID case projection model and save forecasts for app display.