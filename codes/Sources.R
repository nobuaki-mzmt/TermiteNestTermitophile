# Data analysis for Termithophile and termite nesting
# This file is for dependencies
# last update: 12/21/2021 N.Mizumoto

# Packages ---------------------
{
  library(ape)
  library(stringr)
  library(viridis)
  require(phytools)
  library(ggplot2)
  library(car)
  library(phylolm)
  library(extrafont)
  font_import(pattern="PT")
  loadfonts()
  library(scales)
  
  library(caper) # for pgls()
  
}

# parameters ---------------------
{
  today <- Sys.Date()
}