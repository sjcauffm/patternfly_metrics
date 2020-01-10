library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(rsconnect)
library(plotly)

### set working directory if it is not already set to this path. 
load("patternfly_adoption_final.rda")

# Define UI for application that draws a histogram
ui <- dashboardPage( # creates the dashboard layout
    dashboardHeader(title = "PatternFly Adoption"), # gives the dashboard page a title
    
    dashboardSidebar(
      sidebarMenu(
      menuItem("Home", tabName = "homepage"),
      menuItem("Component Data", tabName = "Components",
               menuSubItem("Top Components", tabName = "top"),
               menuSubItem("Component Totals", tabName = "totals"),
               menuSubItem("Component Trends", tabName = "comptrends"),
               menuSubItem("Products Using Components", tabName = "compprods")), # edits the dashboard sidebar. allows for menuItems and menuSubitems
      menuItem("Product Data", tabName = "Products",
               menuSubItem("Component Diversity Current", tabName = "diverse"),
               menuSubItem("Component Diversity Trends", tabName = "divtrend"),
               menuSubItem("Product Imports of Components", tabName = "products_port"),
               menuSubItem("Product Imports by Version", tabName = "versions")),
      menuItem("Raw Data", tabName = "data"))),
   
     dashboardBody( # edits the body of the dashboard.
      tabItems(
        tabItem(tabName = "homepage",
                h1("PatternFly Adoption Metrics and Trends"),
                h3("Welcome! This dashboard displays the usage of PatternFly components across the Red Hat portfolio. the sections are divided by metrics associated with components themselves and the products within 
                  the portfolio. Feel free to click through and see how PF is being used!"),
                h3("NOTE: Due to the nature of product development at Red Hat, this dashboard does not possess data for every single product yet. It will be updated as we obtain more data. "),
                hr(),
                br(),
                br(),
                br(),
                fluidRow(
                  column(width = 5, tags$img(src = "redhat_logo.png", height = 300, width = 300)),
                  column(width = 5, tags$img(src = "redhat_uxd_logo.jpg", height = 300, width = 450, align = "center"))
                  )),
        tabItem(tabName = "Components"),
        tabItem(tabName ="top",
                h1("Top Components"),
                p("This plot shows each component as a proportion of the total number of imports for all components. It reflects how popular each component is.
                  Proportions are used here so that we can calculate confidence intervals using the adjusted Wald technique. With confidence intervals we can determine if some components
                  are statistically more popular than others."),
                p("The general rule is if the error bars for each bar don't touch, then the difference is statistically signficiant."),
                fluidRow(
                  plotlyOutput("top_components", height = "800px")
                     )),
        tabItem(tabName = "totals",
                h1("Components Totals"),
                p("This plot shows the most recent total number of component imports"),
                fluidRow(
                  plotlyOutput("totals", height = "800px")
                     )),
        tabItem(tabName = "comptrends",
                h1("Trends in Imports by Component and PatternFly Version"),
                p("This plot shows the trends in imports of each component over time as well as changes between PatternFly 3 and PatternFly 4"),
                fluidRow(
                  plotlyOutput("component_trends", height = "900px")
                )),
        tabItem(tabName = "compprods",
                h1("Number of Products That Are Using Each Component"),
                p("This graph shows how many products within the Red Hat Portfolio are using each PatternFly component"),
                fluidRow(
                  plotlyOutput("comp_prods", height = "800px")
                )),
        tabItem(tabName = "Products"),
        tabItem(tabName = "diverse",
                h1("Diversity of Components Used by Each Product"),
                p("This plot shows the number of unique PatternFly components being used by each product within the Red Hat portfolio."),
                fluidRow(
                  plotlyOutput("diversity", height = "800px")
                )),
        tabItem(tabName = "divtrend",
                h1("Changes in Diversity of PF Components for Each Product Over Time"),
                p("This plot displays the changes in the number of unique PatternFly components over time."),
                fluidRow(
                  plotlyOutput("div_trend", height = "1000px")
                )),
        tabItem(tabName = "products_port",
                h1("Changes in the Number of Imports by Each Product Over Time"),
                p("This plot shows the change in the number of imports of PatternFly components over time. This differs from diversity because diversity shows us the number of unique imports each
                  product uses, while the number of imports gives us an idea of how much products are implementing the components they are importing. Gathering both metrics gives us a more complete
                  picture of PatternFly usage across the Red Hat Portfolio."),
                fluidRow(
                  plotlyOutput("prod_port", height = "1000px")
                )),
        tabItem(tabName = "versions",
                h1("Changes in Product Imports Split by PatternFly Version"),
                p("This plot is similar to the trends on overall product imports plot, but here the data is broken down by the number of imports for both versions of PatternFly. This gives us a
                  sense of how products are adopting or converting to PatternFly 4 over PatternFly 3."),
                fluidRow(
                  plotlyOutput("prod_vers", height = "1000px")
                )),
        tabItem(tabName = "data",
        h1("Raw Import Data"),
        p("This page contains the raw data that were used to produce the plots on this dashboard."),
        p("X1 is a column that is generated when data are imported from googlesheets and can be ignored."),
        p("full_component is the full name of the component as it is represented in the .JSON files from each product repository."),
        p("Imports is the number of imports for that specific component within each product."),
        p("Product is which product the component is being used by."),
        p("Date is the date that we obtained that particular data point."),
        p("version_grep shows which version of patternfly the component belongs to."),
        p("Component is the short name of the component. Having the shorted name makes it easier to plot to data."),
        p("is_component shows whether the component belongs to the PatternFly library or npt. The data shown here all belong to the library, but the original import required us to filter 
          so we only analyzed PF components."),
        dataTableOutput("raw_data"))
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

##### Preparing data subsets for all plots.   
  
#preparing top components data
  top_data <- reactive({
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
  
#preparing component trends data. 
  components <- reactive({
    components_sum <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component), FUN = sum)
    names(components_sum) <- c("component", "imports") 
    components_sum
  })
  
#preparing component trends data
  comp_trends <- reactive({
    comp_trends <- aggregate.data.frame(pf_data$imports, by = list(pf_data$component, pf_data$version_grep, pf_data$date), FUN = sum)
    names(comp_trends) <- c("component", "version", "date", "imports")
    comp_trends
  })
  
  comp_prods <- reactive({
    comp_prods <- pf_data[which(pf_data$date == "2019-09-13"),]
    
    temp <- comp_prods %>%
      group_by(product,component) %>%
      dplyr::count()
    
    temp$n <- 1
    
    comp_prods <- aggregate.data.frame(temp$n, by = list(temp$component), FUN = sum)  
    names(comp_prods) <- c("component", "products")
    comp_prods
  })
  

#preparing diversity data
  diversity <- reactive({
    current <- grep("2019-09-13", pf_data$date)
    diversity <- pf_data[current,]
    
    diversity_df <- as.data.frame(table(diversity$product))
    names(diversity_df) <- c("product", "components")
    diversity <- diversity_df
    diversity
  })

#preparing diversity trends data. 
  div_trend <- reactive({
    trends <- as.data.frame(table(pf_data$product, pf_data$date))
    names(trends) <- c("product", "date", "diversity")
    trends
  })

#preparing products/portfolios data
  products_portfolios <- reactive({
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
    temp <- strsplit(as.character(products$date), "-2")
    temp <- do.call(rbind.data.frame, temp)
    names(temp) <- c("date", "extra")
    products$date <- temp$date
    products
  })
  
  products_versions <- reactive({
    products_versions <- aggregate(pf_data$imports, by = list(pf_data$product, pf_data$version_grep, pf_data$date), FUN = sum)
    names(products_versions) <- c("product", "version", "date", "imports")
    products_versions
  })
  
##### Plotting
  output$top_components <- renderPlotly({
    temp <- ggplot(top_data(), aes(x = reorder(component, -imports_prop), y = imports_prop)) + 
      geom_bar(stat = "identity") + 
      geom_errorbar(ymin = top_data()$imports_prop - top_data()$margin_error, 
                    ymax = top_data()$imports_prop + top_data()$margin_error, 
                    width = .5) +
      theme_tufte() +
      theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
            text = element_text(family = "Red Hat Display")) + 
      scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, .21))
    temp2 <- ggplotly(temp)
    layout(temp2, xaxis = list(title = "Component"), yaxis = list(title= "Proportion of Total Imports"))
    }) ## Update the date as needed.
  
  output$totals <- renderPlotly({
   temp <- ggplot(components(), aes(x = reorder(component, -imports), y = imports)) +
    geom_bar(stat = "identity") + theme_tufte() +
    scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 2000)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
          text = element_text(family = "Red Hat Display")) +
    labs(x = "Component", y = "Total Number of Imports", title = "Number of Component Imports per Month")
   temp2 <- ggplotly(temp)
   layout(temp2, xaxis = list(title = "Component"), yaxis = list(title = "Imports"))
  })
  
  output$component_trends <- renderPlotly({
    temp <- ggplot(comp_trends(), aes(x = date, y = imports, color = version, group = version)) +
      geom_line(stat = "identity") + geom_point() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            text = element_text(family = "Red Hat Display")) +
      scale_y_continuous(limits = c(0, 300)) +
      facet_wrap(.~component) +
      labs(color = "PatternFly Version", title = "Trends in Component Imports Across PatternFly Library Over Time")
    temp2 <- ggplotly(temp)
    layout(temp2, xaxis = list(title = "Date"), yaxis = list(title = "Number of Imports"))
  })
  
  output$comp_prods <- renderPlotly({
    temp <- ggplot(comp_prods(), aes(x = component, y = products)) +
      geom_bar(stat = "identity") + theme_tufte() +
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
            text = element_text(family = "Red Hat Display")) +
      scale_y_continuous(expand = c(0,0), limits = c(0, 20)) + scale_x_discrete(expand = c(0,0)) +
      labs(x = "PatternFly Component", y = "Number of Products Using Component", title = "Number of Products Using Each PatternFly Component")
    
    temp2 <- ggplotly(temp)
  })

  output$diversity <-  renderPlotly({
    temp <- ggplot(diversity(), aes(x = reorder(product, -components), y = components)) +
    geom_bar(stat = "identity", position = position_dodge()) + theme_tufte() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          text = element_text(family = "Red Hat Display")) + 
    scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0,50))
    temp2 <- ggplotly(temp)
    layout(temp2, xaxis = list(title = "Product", yaxis = list(title = "Number of Unique PF Components")))
    })
  
  output$div_trend <- renderPlotly({
    temp <- ggplot(div_trend(), aes(x = date, y = diversity, group = product)) +
    geom_line(stat = "identity") + geom_point() + theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1),
          text = element_text(family = "Red Hat Display")) + scale_y_continuous(limits = c(0,40)) +
    facet_wrap(~product)
    temp2 <- ggplotly(temp)
    layout(temp2, xaxis = list(title = "Date"), yaxis = list(title = "PF Diversity (Number of PF Components added to a product.)"))
  })
  
  output$prod_port <- renderPlotly({
    temp <- ggplot(products_portfolios(), aes(x = date, y = import, group = portfolio, color = portfolio)) +
      geom_point() + geom_line(stat = "identity") + theme_bw() + 
      facet_wrap(~products_portfolios()$product) +
      theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
            text = element_text(family = "Red Hat Display")) +
      scale_y_continuous(limits = c(0, 250)) +
      scale_color_manual(values = c("#73BCF7" ,"#72767B", "#0066CC")) +
      labs(x = "Date", y = "Imports", 
           title = "Imports of PatternFly Components by Product Over Time", color = "Portfolio")
    temp2 <- ggplotly(temp)
  })
  
  output$prod_vers <- renderPlotly({
   temp <- ggplot(products_versions(), aes(x = date, y = imports, group = version, color = version)) +
      geom_point() + geom_line(stat = "identity") + facet_wrap(~products_versions()$product) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1),
            text = element_text(family = "Red Hat Display"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) + 
      scale_y_continuous(limits = c(0, 250)) + 
      scale_color_manual(values = c("#72767B", "#0066CC")) +
      labs(title = "Imports of PatternFly Components by Product Over Time", color = "PatternFly Version")
   temp2 <- ggplotly(temp)
   layout(temp2, xaxis = list(title = "Date"), yaxis = list(title= "Imports"))
  })
  
  output$raw_data <- renderDataTable({
    pf_data
      })

}

# Run the application 
shinyApp(ui = ui, server = server)
