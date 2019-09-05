library(googlesheets)
library(tidyverse)
library(ggplot2)
library(psych)
library(ggthemes)
library(extrafont)

setwd("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics")
load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")

components_sum <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component, pf_data$date), FUN = sum)
components_totals <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component), FUN = sum) ## counts the sum of imports for each component
components_mean <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component), FUN = mean)
products <- aggregate.data.frame(pf_data$imports, by = list(pf_data$product), FUN = sum) ## counts the sum of imports for each product, gives total imports for each product
versions <- aggregate.data.frame(pf_data$imports, by = list(pf_data$version_grep), FUN = sum) ## counts the number of imports by patternfly version
products_versions <- aggregate(pf_data$imports, by = list(pf_data$product, pf_data$version_grep), FUN = sum) ## counts the number of imports by each product for each patternfly version

names(products_versions) <- c("products", "version", "imports")

##### Importing Red Hat Font
loadfonts()

##### Graphing Products and Versions ####
prod_vers_plot <- ggplot(products_versions, aes(x = products, y = imports, fill = version)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + theme_tufte() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,200)) +
  scale_fill_manual(values = c("PatternFly 3" = "#72767B", "PatternFly 4" = "#0066CC")) +
  labs(x = "Product", y = "Imports", 
       title = "Total Imports of PatternFly Components by Product and Version", fill = "PatternFly Version")

ggsave("Products_Versions_Plot.png", prod_vers_plot, width = 10, height = 6, units = "in")

##### Graphing total number of imports by product ####
names(products) <- c("product", "imports")

products_plot <- ggplot(products, aes(x = product, y = imports)) +
  geom_bar(stat = "identity", position = position_dodge()) + theme_tufte() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,200)) +
  labs(x = "Product", y = "Imports", title = "Total Imports of PatternFly Components by Product")

ggsave("Product_imports_plot.png", products_plot, width = 10, height = 6, units = "in")

##### Graphing top components ####
names(components_totals) <- c("component", "imports_sum")
names(components_mean) <- c("component", "imports_mean")

components_totals$imports_mean <- components_mean$imports_mean 
components_totals$imports_prop <- components_totals$imports_sum/sum(components_totals$imports_sum)

components_totals$adj_prop <- (components_totals$imports_sum + 2) / (sum(components_totals$imports_sum) + 4) ###get adjusted sample proportion
components_totals$x <- components_totals$adj_prop * (1 - components_totals$adj_prop) ###standard error calculation - step 1
components_totals$y <- components_totals$x/(sum(components_totals$imports_sum) + 4) ###standard error calculation - step 2
components_totals$z <- sqrt(components_totals$y) ###standard error calculation - step 3
components_totals$margin_error <- components_totals$z * 2 ###gets margin of error
components_totals$upper_ci <- components_totals$imports_prop + components_totals$margin_error ###calculate upper ci
components_totals$lower_ci <- components_totals$imports_prop - components_totals$margin_error

components_trim <- components_sum[which(components_totals$imports_prop < .001) ,]

components_plot <- ggplot(components_totals, aes(x = reorder(component, -imports_prop), y = imports_prop)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(ymin = components_totals$imports_prop - components_totals$margin_error, 
                                              ymax = components_totals$imports_prop + components_totals$margin_error, 
                                              width = .5) +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, .2)) +
  labs(x = "Component", y = "Proportion of Total Imports", title = "Top Components as a Proportion of Total Component Imports as of 07-2019") ## Update the date as needed. 
  
ggsave("Top_Components.png", components_plot, width = 24, height = 16, units = "in")


##### Graphing Most recent component totals ####
names(components_sum) <- c("component", "date", "imports") 
temp <- strsplit(as.character(components_sum$date), "-2")
temp <- do.call(rbind.data.frame, temp)
names(temp) <- c("date", "extra")
components_sum$date <- temp$date

component_totals_plot <- ggplot(components_sum, aes(x = component, y = imports, fill = date)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + theme_tufte() +
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,300), 
                                                          breaks = c(0,50,100,150,200,250,300)) +
  scale_fill_manual(values = c("2019-06" = "#72767B", "2019-07" = "#0066CC")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) +
  labs(x = "Component", y = "Total Number of Imports", title = "Number of Component Imports per Month", fill = "Date")

ggsave(filename = "component_totals.png", component_totals_plot, height = 12, width = 20, units = "in")


##### Splitting the products by portfolio membership. #####
pf_data2 <- pf_data
pf_data2$portfolio <- 0

pf_data2$portfolio[c(1:47, 136:199, 239:297)] <- "Cloud Native"
pf_data2$portfolio[c(48:97, 111:135, 200:238)] <- "Management and Automation"
pf_data2$portfolio[c(98:110, 298:355)] <- "Hybrid Cloud Infrastructure"

products_portfolios <- aggregate(pf_data2$imports, by = list(pf_data2$product, pf_data2$portfolio), FUN = sum)
names(products_portfolios) <- c("product", "portfolio", "imports")
prod_vers_port <- aggregate(pf_data2$imports, by = list(pf_data2$product, pf_data2$version_grep, pf_data2$portfolio), FUN = sum)
names(prod_vers_port) <- c("product", "version", "portfolio", "imports")


prod_port_plot <- ggplot(products_portfolios, aes(x = product, y = imports, fill = portfolio)) +
  geom_bar(stat = "identity", position = position_dodge()) + theme_tufte() +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1), 
        text = element_text(family = "Red Hat Display")) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,200)) +
  scale_fill_manual(values = c("Cloud Native" = "#73BCF7" , "Hybrid CLoud Infrastructure" = "#72767B", "Management and Automation" = "#0066CC")) +
  labs(x = "Product", y = "Imports", title = "Total Imports of PatternFly Components by Product")

ggsave("products_portfolios.png", prod_port_plot, width = 10, height = 6, units = "in")

prod_vers_port_plot <- ggplot(prod_vers_port, aes(x = product, y = imports, fill = version)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + theme_linedraw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,200)) +
  scale_fill_manual(values = c("PatternFly 3" = "#72767B", "PatternFly 4" = "#0066CC")) +
  facet_grid(.~portfolio, scales = "free_x", space = "free") +
  labs(x = "Product", y = "Imports", title = "Total Imports of PatternFly Components by Product and Version", fill = "PatternFly Version")

ggsave("products_versions_portfolio.png", prod_vers_port_plot, width = 24, height = 16, units = "in")


##### Plotting the proportion of PF4 library used. 
pf4_comp <- pf_data[which(pf_data$version_grep == "PatternFly 4"),]
pf4_data <- as.data.frame(table(pf4_comp$product, pf4_comp$date))

names(pf4_data) <- c("product", "date", "frequency")

pf4_plot <- ggplot(pf4_data, aes(x = date, y = frequency, group = product)) +
  geom_line(stat = "identity") + geom_point() +
  theme_linedraw() + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) + 
  facet_wrap(.~product) + 
  labs(x = "Date", y = "Number of PF4 Components Used", 
       title = "Number of PatternFly 4 Components Used by Each Product")

ggsave("pf4_plot.png", pf4_plot, width = 20, height = 16, units = "in")



  
