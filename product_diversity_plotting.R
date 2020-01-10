library(tidyverse)
library(ggplot2)
library(googlesheets)
library(ggthemes)

##### Plotting diversity of components for patternfly data. 
load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_current.rda") #loads data file from shared drive

##### need to obtain the number of unique elemements per product. This can probably be done by getting the number of rows per product.
import <- gs_title("PatternFly Components")
pf_list <- gs_read_csv(import)

pf_list$Component <- as.character(pf_list$Component)
pf_list$Component <- tolower(pf_list$Component)

comparison <- data4$component %in% pf_list$Component
data4$is_component <- comparison

pf_data <- data4[which(data4$is_component == TRUE),]
save(pf_data, 
     file = "/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")

## Need to get the dates without the times. 

dates <- strsplit(as.character(pf_data$date), "\\s+")
dates2 <- do.call(rbind.data.frame, dates)
names(dates2) <- c("date", "time")

pf_data$date <- dates2$date
save(pf_data, file = "/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")















