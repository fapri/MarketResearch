

# Load libraries
library(shiny)
library(shinyjs)
library(datasets)
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(scales)

# Load data
cornBasis = read_csv("https://raw.githubusercontent.com/fapri/MarketResearch/project/Decoupling/Basis/basisCharting/Data/cornBasis.csv")
soybeanBasis = read_csv("https://raw.githubusercontent.com/fapri/MarketResearch/project/Decoupling/Basis/basisCharting/Data/soybeanBasis.csv")
marketingYears = read_csv("Data/marketingYears.csv")

# Convert dates to date type
cornBasis$date = mdy(cornBasis$date)
soybeanBasis$date = mdy(soybeanBasis$date)
marketingYears$Start = mdy(marketingYears$Start)
marketingYears$Stop = mdy(marketingYears$Stop)

# Create city and terminal name merged variable
cornBasis$cityTermName = paste(cornBasis$city, cornBasis$terminalName, sep = ", ")
soybeanBasis$cityTermName = paste(soybeanBasis$city, soybeanBasis$terminalName, sep = ", ")


marketingYears$interval = interval(marketingYears$Start, marketingYears$Stop)






# Initialize years available to select
years = c("2010-11",
          "2011-12",
          "2012-13",
          "2013-14",
          "2014-15",
          "2015-16",
          "2016-17",
          "2017-18",
          "2018-19",
          "2019-20",
          "2020-21")

# get corn and soybean terminals individually for dynamic lists
cornTerminals = sort(unique(cornBasis$cityTermName))
soybeanTerminals = sort(unique(soybeanBasis$cityTermName))

shinyApp(
    ui = fluidPage(
        useShinyjs(),
        
        fluidRow(
            column(4,
                   # Select crop type
                   selectInput("cropType", "Select Crop Type:", 
                               choices = c("Corn", "Soybeans"))),
            column(4,
                   # Select terminal
                   selectInput("terminal", "Select Terminal:", 
                               choices = cornTerminals)),
            column(4,
                   # Select year
                   selectInput("year", "Select Year:", 
                               choices = years))
        ),
        
        fluidRow(
            column(12,
                   p("Data last updated September 14, 2020", 
                     style = "font-style: italic;
                     font-size: 15px;
                     text-align: center")
            )
        ),
        
        fluidRow(
            column(12,
                   # Output for when no data is available
                   textOutput("noDataText"),
                   
                   # Styling for textOutput above
                   tags$head(tags$style("#noDataText{color: red;
                                 font-size: 30px;
                                 font-style: italic;
                                 text-align: center;
                                 }"
                   )),
                   
                   # Plot output
                   plotlyOutput("plot")
            )
        )
    ),
    server = function(input, output) {
        
        # Dynamic year selection
        yearInput <- reactive({
            switch(input$year,
                   "2010-11" = 2010,
                   "2011-12" = 2011,
                   "2012-13" = 2012,
                   "2013-14" = 2013,
                   "2014-15" = 2014,
                   "2015-16" = 2015,
                   "2016-17" = 2016,
                   "2017-18" = 2017,
                   "2018-19" = 2018,
                   "2019-20" = 2019,
                   "2020-21" = 2020)
        })
        
        # Selecting data as indicated by drop-downs
        datasetInput <- reactive({
            if(input$cropType == "Corn") {
                
                # selection = cornBasis %>% 
                #     select(date, basis, cityTermName) %>% 
                #     filter(cityTermName == input$terminal, year(date) == cornBasis$date[which(cornBasis$date %within% 
                #                                                                                   marketingYears$interval[which(marketingYears$Year == yearInput())])])
                
                selection = cornBasis %>% 
                    select(date, basis, cityTermName) %>% 
                    filter(cityTermName == input$terminal)
                
                selection = selection[which(selection$date %within% 
                                                marketingYears$interval[which(marketingYears$Year == yearInput())]),]
                
                return(selection)
            }
            
            if(input$cropType == "Soybeans") {

                selection = soybeanBasis %>% 
                    select(date, basis, cityTermName) %>% 
                    filter(cityTermName == input$terminal)
                
                selection = selection[which(selection$date %within% 
                                                marketingYears$interval[which(marketingYears$Year == yearInput())]),]
                                
                # selection = soybeanBasis %>% 
                #     select(date, basis, cityTermName) %>% 
                #     filter(cityTermName == input$terminal, year(date) == yearInput())
                
                return(selection)
            }
        })
        
        # Output for when no data is available
        output$noDataText <- renderText({
            print("No data available")
        })
        
        # Determine if warning should show or not
        noDataWarning <- reactive({
            if(nrow(datasetInput()) == 0) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        })
        
        # Output for the plot
        output$plot <- renderPlotly(
            if (nrow(datasetInput()) != 0) {
                ggplotly(ggplot(data = datasetInput(), aes(x = date, y = basis, group = 1)) +
                             geom_line() +
                             labs(x = "Date", y = "Basis ($)") +
                             ggtitle(paste(input$terminal, ": ", paste(yearInput(), "-", (yearInput() + 1)), 
                                           " (", input$cropType, ")", sep = "")) +
                             scale_x_date(date_breaks = "1 month", labels = date_format("%b")))
            }
        )
        
        # Hide or show warning
        output$noDataText <- renderText(
            if (noDataWarning() == TRUE) {
                print("No Data Available")
            } else {
                print("")
            }
        )
        
        
        # selection = cornBasis %>% 
        #     select(date, basis, cityTermName) %>% 
        #     filter(cityTermName == "Adrian, Scoular Grain")
        # 
        # selection = selection[which(selection$date %within% 
        #                                 marketingYears$interval[which(marketingYears$Year == 2018)]),]
        
        
        # selection = soybeanBasis %>% select(date, basis, cityTermName) %>%
        #     filter(cityTermName == "Adrian, Scoular Grain", year(date) == 2018)
        
        # library(scales) 
        # ggplotly(ggplot(data = selection, aes(x = date, y = basis, group = 1)) + geom_line() + 
        #              ggtitle(paste("title: ", year(selection$date)[1])) + 
        #              scale_x_date(date_breaks = "1 month", labels = date_format("%b")) )
        
        # ggplot(data = soybeanBasis[colsSoybean], aes(x = date, y = basis, group = 1)) + geom_line()
        
    }
)