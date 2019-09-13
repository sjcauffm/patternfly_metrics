#### Cleaning Diversity Data
library(tidyverse)
library(googlesheets)
library(ggplot2)

gs_auth()
import <- gs_title("patternfly_adoption")
data <- gs_read_csv(import)

##### need to isolate the component column
component_names <- data[,c(2,4)]  
component_names$lower <- component_names$full_component %>% ### turns each element of the list to all lower case for easier cleaning
  lapply(tolower)

##### separating out the names with "import" in the name
imports_grep <- grep("imports", component_names$lower)

names_import <- component_names[imports_grep,]

names_import$lower <- as.character(names_import$lower)

imports_split <- strsplit(names_import$lower, "react-")
imports_split_names <- do.call(rbind.data.frame, imports_split)

names(imports_split_names) <- c("extra", "component")

imports_split_names$component <- as.character(imports_split_names$component)

split_names2 <- strsplit(imports_split_names$component, "[.]") ## splits the strings by ".", the brackets are a regex to make R recognize the period. 
split_names4 <- lapply(split_names2, tail, n = 1L) # the output from split_names2 is in the form of a list of lists with uneven numbers of elements. This command obtains the last subelement of each list element. 

split_names5 <- do.call(rbind.data.frame, split_names4)
names(split_names5) <- c("component_name") 

names_import2 <- cbind(names_import, split_names5)

##### Getting the component names from components without "import" in the name.
names_classes <- component_names[-imports_grep,]

names_classes$lower <- as.character(names_classes$lower)

names_split <- strsplit(names_classes$lower, ".pf-")#### Need to rethink this one. 
names_split2 <- lapply(names_split, tail, n = 1L)
names_split3 <- do.call(rbind.data.frame, names_split2)

names_classes <- cbind(names_classes, names_split3)
names(names_classes) <- c("full_component", "product", "lower", "component_name")

##### combining the names now and adding them to the main data frame. 
names_combined <- rbind(names_import2, names_classes)
names(names_combined) <- c("component_names", "full_component", "component")

names_trim <- names_combined[,c(1:2,4)]
names_trim$full_component <- as.character(names_trim$full_component)
names(names_trim) <- c("full_component", "product", "component")

##### Need to merge names_trim back with the main data frame in order get the real component names. 
data2 <- data
data3 <- merge(data2, unique(names_trim), by = c("full_component", "product"))

test <- names_trim$product %in% data2$product

data4 <- data3[complete.cases(data3$date),]

save(data4 , file = "/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_current.rda")




