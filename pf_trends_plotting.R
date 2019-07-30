library(tidyverse)
library(ggplot2)
library(googlesheets)
library(ggthemes)

###### Plotting the trend data for PF components.
setwd("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics") # if this isn't set. 

load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda") ## loads the data into the file. 