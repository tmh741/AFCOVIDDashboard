#Installs all packages necessary for the directory.
install.packages("pacman")

pacman::p_load(tidyverse,lubridate,Matrix, lme4,Rcpp,rstanarm,magrittr,abind)