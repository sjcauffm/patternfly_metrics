library(tidyverse)

## need to do some extra cleaning to remove the dates from the poroduct names
data <- load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")

pf_data$product <- gsub("_july", "", pf_data$product)
pf_data$product <- gsub("_aug", "", pf_data$product)
pf_data$product <- gsub("_sep", "", pf_data$product)
pf_data$product <- gsub("_oct", "", pf_data$product)
pf_data$product <- gsub("_nov", "", pf_data$product)

save(pf_data, 
     file = "/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")