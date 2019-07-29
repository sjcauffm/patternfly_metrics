library(tidyverse)
library(ggplot2)
library(googlesheets)
library(ggthemes)

##### Plotting diversity of components for patternfly data. 
load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_data.rda") #loads data file from shared drive

##### need to obtain the number of unique elemements per product. This can probably be done by getting the number of rows per product.
data3$date <- as.character(data3$date) ## makes it easier to grep
current <- grep("2019-07-26", data3$date) ## provides a vector of all rows with current data

diversity <- data3[current,] ### subsets by the current data

diversity <- table(data3$product)
diversity2 <- as.data.frame(diversity)

import <- gs_title("PatternFly Components")
pf_list <- gs_read_csv(import)

pf_list$Component <- as.character(pf_list$Component)
pf_list$Component <- tolower(pf_list$Component)

comparison <- data3$component %in% pf_list$Component
data3$is_component <- comparison

pf_data <- data3[which(data3$is_component == TRUE),]
save(pf_data, 
     file = "/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")

##### graphing product diversity

## USE "patternfly_adoption_final.rda" for graphing. It has is limited to only components that are from the PatternFly Library ##

diversity <- table(pf_data$product)
diversity_df <- as.data.frame(diversity)
names(diversity_df) <- c("product", "components")

div_plot <- ggplot(diversity_df, aes(x = reorder(product, -components), y = components)) +
  geom_bar(stat = "identity") + theme_tufte() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,40)) +
  labs(x = "Product", y = "Number of PF Components", title = "Number of PF Components Used by Each Product")

ggsave("diversity_plot.png", div_plot, height = 6, width = 10, units = "in")

##### Graphing Change in Diversity
diversity_trend <- count(pf_data$product, vars = c(pf_data$date)) 

div_trend <- ggplot()


