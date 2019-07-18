library(googlesheets)
library(tidyverse)
library(ggplot2)
library(psych)
library(ggthemes)

gs_auth()
import <- gs_title("patternfly_adoption")

data <- gs_read_csv(import) ### Imports data to a dataframe

data <- data[,2:6] # removes extra column

components_sum <- aggregate.data.frame(data$imports, by = list(data$full_component), FUN = sum)
components_mean <- aggregate.data.frame(data$imports, by = list(data$full_component), FUN = mean) ## counts the sum of imports for each component
products <- aggregate.data.frame(data$imports, by = list(data$product), FUN = sum) ## counts the sum of imports for each product, gives total imports for each product
versions <- aggregate.data.frame(data$imports, by = list(data$version_grep), FUN = sum) ## counts the number of imports by patternfly version
products_versions <- aggregate(data$imports, by = list(data$product, data$version_grep), FUN = sum) ## counts the number of imports by each product for each patternfly version

names(products_versions) <- c("products", "version", "imports")

prod_vers <- data.frame(products = rep(levels(as.factor(data$product)), 2),
                        version = c(rep("PatternFly 3", 19), rep("PatternFly 4",19)),
                        imports = 0)

prod_vers2 <- full_join(prod_vers, products_versions, by = c("products", "version")) ### creates an even data frame with product and versions data

prod_vers2 <- prod_vers2[,c(1:2, 4)] #trims extraneous columns
names(prod_vers2) <- c("products", "version", "imports")  

prod_vers2[is.na(prod_vers2)] <- 0

data$full_component <- as.factor(data$full_component)
data$product <- as.factor(data$product)


##### Graphing Products and Versions
prod_vers_plot <- ggplot(prod_vers2, aes(x = products, y = imports, fill = version)) +
  geom_bar(stat = "identity", position = position_dodge()) + theme_tufte() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  labs(x = "Product", y = "Imports", title = "Total Imports of PatternFly Components by Product and Version", fill = "PatternFly Version")

ggsave("Products_Versions_Plot.png", prod_vers_plot, width = 10, height = 6, units = "in")

##### Graphing total number of imports by product
names(products) <- c("product", "imports")

products_plot <- ggplot(products, aes(x = product, y = imports)) +
  geom_bar(stat = "identity", position = position_dodge()) + theme_tufte() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
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

components_trim <- components_sum[which(components_sum$imports_mean > 5),]

components_plot <- ggplot(components_trim, aes(x = reorder(component, -imports_prop), y = imports_prop)) + 
  geom_bar(stat = "identity") + geom_errorbar(ymin = components_trim$imports_prop - components_trim$margin_error, 
                                              ymax = components_trim$imports_prop + components_trim$margin_error, 
                                              width = .5) +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 0.03)) +
  labs(x = "Component", y = "Proportion of Total Imports", title = "Top Components as a Proportion of Total Component Imports")
  
ggsave("Top_Components.png", components_plot, width = 12, height = 8, units = "in")
  


##### Diversity is going to be it's own beast
diversity <- 
