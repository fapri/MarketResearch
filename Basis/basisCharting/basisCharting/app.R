
library(shiny)
library(datasets)
library(readr)
library(ggplot2)

cornBasis = read_csv("Data/cornBasis.csv")
soybeanBasis = read_csv("Data/soybeanBasis.csv")



basisList = list("corn" = cornBasis,
                 "soybeans" = soybeanBasis)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Give the page a title
    titlePanel("Telephones by region"),
    
    # # Generate a row with a sidebar
    # sidebarLayout(      
    #     
    #     # Define the sidebar with one input
    #     sidebarPanel(
    #         selectInput("cropType", "Select Crop Type:", 
    #                     choices = c("Corn", "Soybeans")),
    #         hr(),
    #         helpText("Data from Refinitiv")
    #     )
    # ),
    
    fluidRow(
        column(12,
               dataTableOutput('table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # datasetInput <- reactive({
    #     switch(input$cropType,
    #            "Corn" = cornBasis,
    #            "Soybeans" = soybeanBasis)
    # })
    
    # Fill in the spot we created for a plot
    # output$phonePlot <- renderPlot({
    #     # barplot(basisList[[input$cropType]]$basis[1:100], 
    #     #         main=paste(input$cropType, "Basis"),
    #     #         ylab="Number of Telephones",
    #     #         xlab="Year")
    #     
    #     # ggplot(data =datasetInput) + 
    #     #     geom_line(aes(x = date, y = basis, group = 1))
    #     
    #     
    #     
    # })
    
    output$table <- renderTable(iris)
    
    # observeEvent(input$cropType, {
    #     output$table <- renderTable(cornBasis)
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
