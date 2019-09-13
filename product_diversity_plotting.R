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

##### graphing product diversity

## USE "patternfly_adoption_final.rda" for graphing. It has is limited to only components that are from the PatternFly Library ##
load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")

current <- grep("2019-07-30", pf_data$date)
diversity <- pf_data[current,]

diversity_df <- as.data.frame(table(diversity$product))
names(diversity_df) <- c("product", "components")

div_plot <- ggplot(diversity_df, aes(x = reorder(product, -components), y = components)) +
  geom_bar(stat = "identity", position = position_dodge()) + theme_tufte() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,40)) +
  labs(x = "Product", y = "Number of PF Components", title = "Number of PF Components Used by Each Product")

ggsave("diversity_plot.png", div_plot, height = 6, width = 10, units = "in")

## Need to get the dates without the times. 

dates <- strsplit(as.character(pf_data$date), "\\s+")
dates2 <- do.call(rbind.data.frame, dates)
names(dates2) <- c("date", "time")

pf_data$date <- dates2$date
save(pf_data, file = "/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")


##### Graphing Change in Diversity
trends <- as.data.frame(table(pf_data$product, pf_data$date))
names(trends) <- c("product", "date", "diversity")

div_trend <- ggplot(trends, aes(x = date, y = diversity, group = product)) +
  geom_line(stat = "identity") + theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1),
        text = element_text(family = "Red Hat Display")) + scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,40)) +
  labs(x = "Date", y = "PF Diversity (number of PF components added to product)", title = "Diversity of PF Components Over Time by Product", color = "Product") +
  facet_wrap(~product)

ggsave("diversity_trend_plot.png", div_trend, height = 12, width = 20, units = "in")
















