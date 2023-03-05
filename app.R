#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
library(plotly)

data <- read_delim("nces330_20.csv")

ui <- fluidPage(
  titlePanel("PS6: College Tuition Data"),
  
  tabsetPanel(
    tabPanel("General Info",
             br(),
             h1("Dataset Information"),
             p("The dataset being used is called",
               strong("Average Cost of Undergraduate College by State."), 
               "Collected by the", em("National Center of Education Statistics"), "Annual 
               Digest. Found on Kaggle - @kenmoretoast, updated a month ago. The 
               dataset focuses on average undergraduate tuition and fees and room 
               and board rates charged for full-time students in degree-granting 
               postsecondary institutions, by control and level of institution 
               and state or jurisdiction."), 
             p("The dataset has ", nrow(data), "rows, and ", ncol(data), "columns."),
             img(alt = "Tuition Graphic", 
                 src = "https://th.bing.com/th/id/R.f44fde31f669039af688c9de699da2da?rik=dCncQPN0qfHJrg&riu=http%3a%2f%2f614now.com%2fwp-content%2fuploads%2f2017%2f05%2ftuition-cash.jpg&ehk=iqnn7hEBQM9bbxFDF0uGiSBxhBPXvWd4bPi9CYwHCiQ%3d&risl=&pid=ImgRaw&r=0"),
             h3("Target Audience"), 
             p("Our target audience is prospective college students. 
                Prospective college students would be interested in 
                visualizing the cost of colleges based on their location (state), 
                so transforming this data about college costs into a friendly 
                and presentable way would be helpful for them."), 
             h3("Research questions being answered: "), 
             HTML("<ul>
                  <li>What state, on average, has the cheapest college tuition for 4-years, public universities?</li>
                  <li>Do private universities cost more than public universities?</li>
                  <li>What state, on average, has the most expensive college tuition for 4-years, public universities?</li>
                  <li>For both private and public universities, has the cost of college increased over time on average?</li>
                </ul>"),
             p("The dataset we are using can be accessed", a("here.", 
                                                             href = "https://www.kaggle.com/datasets/kfoster150/avg-cost-of-undergrad-college-by-state")), 
             ),
    
    
    tabPanel("Plot",
        sidebarLayout(
          sidebarPanel(
            fluidRow(
              column(3, 
                     radioButtons("colors", 
                                  "Select color:", 
                                  choices = c("pink", "skyblue", "lawngreen", "lightyellow", "lavender"))),
              column(3, 
                     checkboxGroupInput("states",
                                        "Select the state(s):",
                                        choices = unique(data$State))
              ), 
              column(6, 
                     radioButtons("type", 
                                  "Select the type of school:", 
                                  choices = unique(data$Type))
              )
            ), 
            
            textOutput("plot_text")
          
          ),
          
          mainPanel(
            plotOutput("plot"), 
          ),
        
        )
    ),

    tabPanel("Table", 
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(6,
                          selectInput("table_years", "Select a year:", choices = 2013:2021)
                   ),
                   column(6, 
                          sliderInput("rows", "Select number of states to display:", 
                                      min = 1,
                                      max = 50,
                                      value = 10)
                   )
                 ),
                 
                 textOutput("text_output")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Most Expensive States",
                            dataTableOutput("most_expensive_states")),
                   tabPanel("Least Expensive States",
                            dataTableOutput("least_expensive_states"))
                 )
               )
             )
    ),

    tabPanel("Conclusion", 
             h1("Overall Takeaways"), 
             h3("Research questions we were answering: "), 
             HTML("<ul>
                  <li>What state, on average, has the cheapest college tuition for 4-years, public universities, and 
                  what state has the most expensive?</li>
                  <li>Do private universities cost more than public universities?</li>
                  <li>For both private and public universities, has the cost of college increased over time on average?</li>
                </ul>"),
             p("1. On average,", strong("DC and Vermont"), "are the 2 most expensive states overall for college tuition.", strong("Wyoming, 
               Idaho, and Utah"), "are consistently the top 3 cheapest states for college tuition."), 
             p("2. Yes, private universities", em("do"), "cost more than public univerisites consistently from 2013 to 2021."),
             p("3. Yes, college tuition in the US", em("has"), "gotten more expensive from 2013 to 2021, with the most expensive
               state (DC) gaining almost $8000 in total value during that time period.")
             )

)

)


server <- function(input, output) {
  data <- read_delim("nces330_20.csv")
  
  year_data <- reactive({
    data %>% 
      filter(Year == input$table_years)
  
  })
  
  output$most_expensive_states <- renderDataTable({
    year_data <- year_data()
    
    if(nrow(year_data) == 0) {
      return(data.frame())
    }
    
    p <- year_data %>%
      group_by(State) %>%
      summarize(total_cost = mean(Value, na.rm = TRUE)) %>%
      arrange(desc(total_cost)) %>%
      head(input$rows)
    
    if(nrow(p) == 0) {
      p <- p +
        labs(title = "Please select a year")
    }
    
    p
  })
  
  
  output$least_expensive_states <- renderDataTable({
    year_data <- year_data()
    
    if(nrow(year_data) == 0) {
      return(data.frame())
    }
    
    p <- year_data %>%
      group_by(State) %>%
      summarize(total_cost = mean(Value, na.rm = TRUE)) %>%
      arrange(total_cost) %>%
      head(input$rows)
    
    if(nrow(p) == 0) {
      p <- p +
        labs(title = "Please select a year")
    }
    
    p
    
  })
  
  output$text_output <- renderText({
    year_data <- year_data()
    p <- year_data %>%
      group_by(State) %>%
      summarize(total_cost = mean(Value, na.rm = TRUE)) %>%
      arrange(total_cost) %>%
      head(input$rows)
    
    paste("Selected subset contains", nrow(p), "observations (rows).")
    
  })
  
  plot_reactive <- reactive({
    data %>%
      filter(State %in% input$states, Type == input$type)
  })
  
  output$plot <- renderPlot({
    ggplot(plot_reactive(), aes(x = Year, y = Value, color = factor(State))) +
      geom_col(position = position_dodge(), fill = input$colors, size = 3) +
      xlab("Year") +
      ylab("Tuition Costs") +
      ggtitle("Average Tuition Costs By Year") +
      scale_fill_discrete(name = "State")
  })
  
  output$plot_text <- renderText({
    plot_data <- data %>%
      filter(State %in% input$states, Type == input$type)
    
    paste("Selected subset contains", nrow(plot_data), "observations.")
  })
  

  
}


# Run the application 
shinyApp(ui = ui, server = server)
