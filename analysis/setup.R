rm(list=ls())
# packages
library(scales)
library(RColorBrewer)
library(tidyverse)
library(broom)
library(knitr)
library(nlstools)
library(guppies)
library(Hmisc)
# set plot defaults
guppies::roses_set_ggtheme()
# pull in objects
data("guppies")
source("analysis/wild.R")
source("analysis/control.R")
source("analysis/predation.R")
source("analysis/predation_density.R")
# global options
options(knitr.table.format = "latex")
options(kableExtra.latex.load_packages = FALSE)
