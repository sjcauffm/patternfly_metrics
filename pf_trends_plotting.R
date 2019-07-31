library(tidyverse)
library(ggplot2)
library(googlesheets)
library(ggthemes)

###### Plotting the trend data for PF components.
setwd("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics") # if this isn't set. 

load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda") ## loads the data into the file. 

##### Graphing component imports over time.#### 
components <- aggregate(pf_data$imports, by = list(pf_data$component, pf_data$date), FUN = sum)
names(components) <- c("component", "date", "imports")

### Not sure best way to represent this data. Lines are a bit to busy....
components_trend_line <- ggplot(components, aes(x = date, y = imports, group = component, color = component)) +
  geom_line(stat = "identity") + theme_tufte() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 275),
                                                          breaks = c(0, 50, 100, 150, 200, 250))

### And bars aren't normally used to represent this type of information. 
components_trend_bar <- ggplot(components, aes(x = component, y = imports, fill = date)) +
  geom_bar(stat = "identity", position = position_dodge(preserve= "single")) + theme_tufte() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 275), 
                                                          breaks = c(0, 50, 100, 150, 200, 250))


### Trying some faceting
components_trend_facet<- ggplot(components, aes(x = date, y = imports, group = as.factor(component))) +
  geom_line() + theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1),
        text = element_text(family = "Red Hat Display")) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 275)) +
  facet_wrap(~component) +
  labs(x = "Date", y = "Number of Imports", title = "Number of Imports for PF Components Over Time")

ggsave(filename = "components_over_time.png", components_trend_facet, height = 12, width = 20, units = "in")

##### Graphing product trends over time. ####
products <- aggregate(pf_data$imports, by = list(pf_data$product, pf_data$date), FUN = sum)
names(products) <- c("product", "date", "import")
products$portfolio <- 0
for (i in 1:length(products$portfolio)){
  if(products$product[i] == "3scale" | products$product[i] == "AMQ_Everything_Else" | products$product[i] == "AMQ_Streams" | products$product[i] == "Fuse_Online" |
     products$product[i] == "Fuse_Online_React" | products$product[i] == "Integreatly" | products$product[i] == "Kiali_App" | products$product[i] == "Mobile_Dev_Consle"){
    products$portfolio[i] <- "Cloud Native"
  } else if (products$product[i] == "Ansible" | products$product[i] == "Cloud_Meter" | products$product[i] == "Cost_Management" | products$product[i] == "Insights_Frontend" |
             products$product[i] == "Insights_Library") {
    products$portfolio[i] <- "Management and Automation"
  } else {
    products$portfolio[i] <- "Hybrid Cloud Infrastructure"
  }
}

## going to merge june dates to so it representes one month
temp <- strsplit(as.character(products$date), "-2")
temp <- do.call(rbind.data.frame, temp)
names(temp) <- c("date", "extra")
products$date <- temp$date


products_trends <- ggplot(products, aes(x = date, y = import, fill = portfolio)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + theme_linedraw() + facet_wrap(~products$product) +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
        text = element_text(family = "Red Hat Display")) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 250)) +
  scale_fill_manual(values = c("#73BCF7" ,"#72767B", "#0066CC")) +
  labs(x = "Product", y = "Imports", title = "Imports of PatternFly Components by Product Over Time", fill = "Portfolio")










