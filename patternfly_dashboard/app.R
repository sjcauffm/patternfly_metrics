#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")

# Define UI for application that draws a histogram
ui <- dashboardPage( # creates the dashboard layout
    dashboardHeader(title = "PatternFly Adoption"), # gives the dashboard page a title
    
    dashboardSidebar(
      sidebarMenu(
      menuItem("Component Data", tabName = "Components",
               menuSubItem("Top Components", tabName = "top"),
               menuSubItem("Component Totals", tabName = "totals")), # edits the dashboard sidebar. allows for menuItems and menuSubitems
      menuItem("Product Data", tabName = "Products"),
      menuItem("Raw Data", tabName = "Raw Data"))),
   
     dashboardBody( # edits the body of the dashboard.
      tabItems(
        tabItem(tabName = "Components"),
        tabItem(tabName ="top",
                h3("Top Components"),
                p("This plot shows each component as a proportion of the total number of imports for all components. It reflects how popular each component is.
                  Proportions are used here so that we can calculate confidence intervals using the adjusted Wald technique. With confidence intervals we can determine if some components
                  are statistically more popular than others."),
                p("The general rule is if the error bars for each bar don't touch, then the difference is statistically signficiant."),
                fluidRow(
                  plotOutput("top_components")
                     )),
        tabItem(tabName = "totals",
                h3("Components Totals"),
                p("This plot shows the total number of component imports split by date."),
                fluidRow(
                  plotOutput("totals")
                     )),
        tabItem(tabName = "Products"),
        tabItem(tabName = "Raw Data"))
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  top_data <- reactive({
    load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")
    components_totals <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component), FUN = sum)
    components_mean <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component), FUN = mean)
    names(components_totals) <- c("component", "imports_sum")
    components_totals$imports_mean <- components_mean$imports_mean 
    components_totals$imports_prop <- components_totals$imports_sum/sum(components_totals$imports_sum)
    components_totals$adj_prop <- (components_totals$imports_sum + 2) / (sum(components_totals$imports_sum) + 4) ###get adjusted sample proportion
    components_totals$x <- components_totals$adj_prop * (1 - components_totals$adj_prop) ###standard error calculation - step 1
    components_totals$y <- components_totals$x/(sum(components_totals$imports_sum) + 4) ###standard error calculation - step 2
    components_totals$z <- sqrt(components_totals$y) ###standard error calculation - step 3
    components_totals$margin_error <- components_totals$z * 2 ###gets margin of error
    components_totals$upper_ci <- components_totals$imports_prop + components_totals$margin_error ###calculate upper ci
    components_totals$lower_ci <- components_totals$imports_prop - components_totals$margin_error
    components_totals
  })
  
  components <- reactive({
    load("/Volumes/GoogleDrive/My Drive/UXD-Share/Usability and User Research/Studies 2019/PatternFly Adoption Visualization/patternfly_metrics/patternfly_adoption_final.rda")
    components_sum <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component, pf_data$date), FUN = sum)
    names(components_sum) <- c("component", "date", "imports") 
    temp <- strsplit(as.character(components_sum$date), "-2")
    temp <- do.call(rbind.data.frame, temp)
    names(temp) <- c("date", "extra")
    components_sum$date <- temp$date
    components_sum
  })
  
  
  output$totals <- renderPlot({
    ggplot(components(), aes(x = component, y = imports, fill = date)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + theme_tufte() +
    scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,300), 
                                                            breaks = c(0,50,100,150,200,250,300)) +
    scale_fill_manual(values = c("2019-06" = "#72767B", "2019-07" = "#0066CC")) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
          text = element_text(family = "Red Hat Display")) +
    labs(x = "Component", y = "Total Number of Imports", title = "Number of Component Imports per Month", fill = "Date")
  })
  
  output$top_components <- renderPlot({
    ggplot(top_data(), aes(x = reorder(component, -imports_prop), y = imports_prop)) + 
    geom_bar(stat = "identity") + 
    geom_errorbar(ymin = top_data()$imports_prop - top_data()$margin_error, 
                  ymax = top_data()$imports_prop + top_data()$margin_error, 
                  width = .5) +
    theme_tufte() +
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
          text = element_text(family = "Red Hat Display")) + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, .2)) +
    labs(x = "Component", y = "Proportion of Total Imports", title = "Top Components as a Proportion of Total Component Imports as of 07-2019") ## Update the date as needed.
})
}

# Run the application 
shinyApp(ui = ui, server = server)
