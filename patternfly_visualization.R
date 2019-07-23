library(googlesheets)
library(tidyverse)
library(ggplot2)
library(psych)
library(ggthemes)

load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")

components_sum <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component_name), FUN = sum)
components_mean <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component_name), FUN = mean) ## counts the sum of imports for each component
products <- aggregate.data.frame(pf_data$imports, by = list(pf_data$product), FUN = sum) ## counts the sum of imports for each product, gives total imports for each product
versions <- aggregate.data.frame(pf_data$imports, by = list(pf_data$version_grep), FUN = sum) ## counts the number of imports by patternfly version
products_versions <- aggregate(pf_data$imports, by = list(pf_data$product, pf_data$version_grep), FUN = sum) ## counts the number of imports by each product for each patternfly version

names(products_versions) <- c("products", "version", "imports")

##### Graphing Products and Versions
prod_vers_plot <- ggplot(products_versions, aes(x = products, y = imports, fill = version)) +
  geom_bar(stat = "identity", position = position_dodge()) + theme_tufte() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,200)) +
  scale_fill_manual(values = c("PatternFly 3" = "#72767B", "PatternFly 4" = "#0066CC")) +
  labs(x = "Product", y = "Imports", title = "Total Imports of PatternFly Components by Product and Version", fill = "PatternFly Version")

ggsave("Products_Versions_Plot.png", prod_vers_plot, width = 10, height = 6, units = "in")

##### Graphing total number of imports by product
names(products) <- c("product", "imports")

products_plot <- ggplot(products, aes(x = product, y = imports)) +
  geom_bar(stat = "identity", position = position_dodge()) + theme_tufte() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,200)) +
  labs(x = "Product", y = "Imports", title = "Total Imports of PatternFly Components by Product")

ggsave("Product_imports_plot.png", products_plot, width = 10, height = 6, units = "in")

##### Graphing top components
names(components_sum) <- c("component", "imports_sum")
names(components_mean) <- c("component", "imports_mean")

components_sum$imports_mean <- components_mean$imports_mean 
components_sum$imports_prop <- components_sum$imports_sum/sum(components_sum$imports_sum)

components_sum$adj_prop <- (components_sum$imports_sum + 2) / (sum(components_sum$imports_sum) + 4) ###get adjusted sample proportion
components_sum$x <- components_sum$adj_prop * (1 - components_sum$adj_prop) ###standard error calculation - step 1
components_sum$y <- components_sum$x/(sum(components_sum$imports_sum) + 4) ###standard error calculation - step 2
components_sum$z <- sqrt(components_sum$y) ###standard error calculation - step 3
components_sum$margin_error <- components_sum$z * 2 ###gets margin of error
components_sum$upper_ci <- components_sum$imports_prop + components_sum$margin_error ###calculate upper ci
components_sum$lower_ci <- components_sum$imports_prop - components_sum$margin_error

components_trim <- components_sum[which(components_sum$imports_prop < .001) ,]

components_plot <- ggplot(components_sum, aes(x = reorder(component, -imports_prop), y = imports_prop)) + 
  geom_bar(stat = "identity", position= position_dodge()) + 
  geom_errorbar(ymin = components_sum$imports_prop - components_sum$margin_error, 
                                              ymax = components_sum$imports_prop + components_sum$margin_error, 
                                              width = .5) +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) + 
  scale_x_discrete(expand = c(0, 0), labels = components_sum$component) + scale_y_continuous(expand = c(0, 0), limits = c(0, .185)) +
  labs(x = "Component", y = "Proportion of Total Imports", title = "Top Components as a Proportion of Total Component Imports")
  
ggsave("Top_Components.png", components_plot, width = 24, height = 16, units = "in")
  
