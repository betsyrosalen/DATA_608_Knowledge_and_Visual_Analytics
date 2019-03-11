# Data can be found here:
# https://github.com/charleyferrari/CUNY_DATA608/tree/master/module3/data

# Question 1:
#    As a researcher, you frequently compare mortality rates from particular
# causes across different States. You need a visualization that will let you see
# (for 2010 only) the crude mortality rate, across all States, from one cause
# (for example, Neoplasms, which are effectively cancers). Create a visualization
# that allows you to rank States by crude mortality for each cause of death.


library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)

URL <- 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv'
df <- read.csv(URL)

ui <- fluidPage(
    headerPanel('Housing Price Explorer'),
    sidebarPanel(
        selectInput('seas', 'Seasonality', unique(df$Seasonality), selected='SA'),
        selectInput('metro', 'Metro Area', unique(df$Metro), selected='Atlanta'),
        selectInput('tier', 'Housing Tier', unique(df$Tier), selected='High')
    ),
    mainPanel(
        plotlyOutput('plot1'),
        verbatimTextOutput('stats')
    )
)

server <- function(input, output, session) {

    selectedData <- reactive({
        dfSlice <- df %>%
            filter(Seasonality == input$seas, Metro == input$metro)
    })

    output$plot1 <- renderPlotly({

        dfSlice <- df %>%
            filter(Seasonality == input$seas, Metro == input$metro)

        plot_ly(selectedData(), x = ~DATE, y = ~HPI, color = ~Tier, type='scatter',
                mode = 'lines')
    })

    output$stats <- renderPrint({
        dfSliceTier <- selectedData() %>%
            filter(Tier == input$tier)

        summary(dfSliceTier$HPI)
    })

}

shinyApp(ui = ui, server = server)