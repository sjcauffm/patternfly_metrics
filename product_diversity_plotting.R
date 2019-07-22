library(tidyverse)
library(ggplot2)
library(googlesheets)

##### Plotting diversity of components for patternfly data. 
load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_current.rda") #loads data file from shared drive

##### need to obtain the number of unique elemements per product. This can probably be done by getting the number of rows per product. 
diversity <- table(data3$product)
diversity2 <- as.data.frame(diversity)

import <- gs_title("PatternFly Components")
pf_list <- gs_read_csv(import)

pf_list$Component <- as.character(pf_list$Component)
pf_list$Component <- tolower(pf_list$Component)

comparison <- data3$component_name %in% pf_list$Component
data3$is_component <- comparison

pf_data <- data3[which(data3$is_component == TRUE),]

save(pf_data, file = "patternfly_adoption_final.rda")

##### graphing product diversity

## USE "patternfly_adoption_final.rda" for graphing. It has is limited to only components that are from the PatternFly Library ##

diversity <- table(pf_data$product)
diversity_df <- as.data.frame(diversity)
names(diversity_df) <- c("product", "componets")

