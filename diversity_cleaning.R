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

components_lower$original <- data$full_component

components_lower <- do.call(rbind.data.frame, components_lower) ## unlists ouput to a data frame 
names(components_lower) <- c("component_names") # changed the column name to something more readable


##### separating out the names with "import" in the name
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

split_names2 <- strsplit(imports_split_names$component, "[.]") ## splits the strings by ".", the brackets are a regex to make R recognize the period. 
split_names4 <- lapply(split_names2, tail, n = 1L) # the output from split_names2 is in the form of a list of lists with uneven numbers of elements. This command obtains the last subelement of each list element. 

split_names5 <- do.call(rbind.data.frame, split_names4)
names(split_names5) <- c("component_name")

names_import2 <- cbind(names_import, split_names5)

##### Getting the component names from components without "import" in the name.
names_classes <- components_lower[-imports_grep,]
names_classes <- names_classes %>%
  as.data.frame()

names(names_classes) <- c("names")

names_classes$names <- as.character(names_classes$names)

names_split <- strsplit(names_classes$names, ".pf-")#### Need to rethink this one. 
names_split2 <- lapply(names_split, tail, n = 1L)
names_split3 <- do.call(rbind.data.frame, names_split2)

names_classes <- cbind(names_classes, names_split3)
names(names_classes) <- c("names", "component_name")

##### combining the names now and adding them to the main data frame. 
names_combined <- rbind(names_import2, names_classes)
names(names_combined) <- c("component_names", "component")



#data2 <- full_join(data, names_combined, by = "full_component") 
### The line above does not function like it should, maybe want to try a "for"

data2 <- data
data2$component <- 0

for (i in 1:length(data2$full_component)){
  if (data2$full_component[i] == names_combined$full_component[i]){
    data2$component[i] <- names_combined$component[i]
  } 
}

data3 <- merge(data, names_combined, by = "full_component")
