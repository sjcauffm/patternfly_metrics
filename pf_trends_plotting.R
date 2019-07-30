library(tidyverse)
library(ggplot2)
library(googlesheets)
library(ggthemes)

###### Plotting the trend data for PF components.
setwd("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics") # if this isn't set. 

load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda") ## loads the data into the file. 

##### Graphing component imports over time. 
components <- aggregate(pf_data$imports, by = list(pf_data$component, pf_data$date), FUN = sum)
names(components) <- c("component", "date", "imports")

components_trend <- ggplot(components, aes(x = date, y = imports, group = component, color = component)) +
  geom_line(stat = "identity") + theme_tufte() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 275))