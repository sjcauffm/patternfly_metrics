#### Cleaning Diversity Data
library(tidyverse)
library(googlesheets)
library(ggplot2)
library(chartr)

gs_auth()
import <- gs_title("patternfly_adoption")
data <- gs_read_csv(import)

##### need to isolate the component column
component_names <- data$full_component  
components_lower <- component_names %>% ### turns each element of the list to all lower case for easier cleaning
  lapply(tolower) 

components_lower <- do.call(rbind.data.frame, components_lower) ## unlists ouput to a data frame 
names(components_lower) <- c("component_names") # changed the column name to something more readable


##### separating out the names with imports in the name
imports_grep <- grep("imports", components_lower$component_names)

names_import <- components_lower[imports_grep,]
names_import <- names_import %>%
  as.character() %>%
  as.data.frame()

names(names_import) <- c("names")
names_import$names <- as.character(names_import$names)

imports_split <- strsplit(names_import$names, "react-")
imports_split_names <- do.call(rbind.data.frame, imports_split)

names(imports_split_names) <- c("extra", "component")

imports_split_names$component <- as.character(imports_split_names$component)

split_names2 <- strsplit(imports_split_names$component, "[.]")
split_names3 <- do.call(rbind.data.frame, split_names2)
split_names4 <- as.data.frame(split_names2)
