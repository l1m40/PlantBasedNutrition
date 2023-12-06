# File:   
# Description: 

# INSTALL AND LOAD PACKAGES ################################
#library(datasets)  # Load base packages manually
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")
# Use pacman to load add-on packages as desired
# Packages I load every time; uses "pacman"
pacman::p_load(pacman,rio,tidyverse,rvest) 

# CODING ###################################################