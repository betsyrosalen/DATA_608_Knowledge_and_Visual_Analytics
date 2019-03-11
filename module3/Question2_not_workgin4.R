# Data can be found here:
# https://github.com/charleyferrari/CUNY_DATA608/tree/master/module3/data

# Question 2:
#    Often you are asked whether particular States are improving their mortality
# rates (per cause) faster than, or slower than, the national average. Create a
# visualization that lets your clients see this for themselves for one cause of
# death at the time. Keep in mind that the national average should be weighted by
# the national population.


library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)

URL <- 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv'
df <- read.csv(URL)

national <- df %>%
    group_by(ICD.Chapter, Year) %>%
    summarize(Crude.Rate = round((sum(Deaths) / sum(Population)) * 100000, 2)) %>%
    mutate(State = "National") %>%
    arrange(Year) %>%
    tbl_df()

state <- df %>%
    select(ICD.Chapter, Year, State, Crude.Rate)%>%
    tbl_df()

DATA <- rbind(national, state)

ui <- fluidPage(
    headerPanel('State Mortality Rates Explorer'),
    sidebarPanel(
        selectInput('Cause', 'Cause of Death', unique(DATA$ICD.Chapter), selected='Certain infectious and parasitic diseases'),
        selectInput('State', 'State', unique(DATA$State), selected='NY', multiple = TRUE)
    ),
    mainPanel(
        plotlyOutput('plot1'),
        verbatimTextOutput('stats')
    )
)

server <- function(input, output, session) {

    selectedData <- reactive({

        national <- df %>%
            filter(ICD.Chapter == input$Cause) %>%
            group_by(Year) %>%
            summarize(Crude.Rate = round((sum(Deaths) / sum(Population)) * 100000, 2)) %>%
            mutate(State = "National") %>%
            arrange(Year)

        state <- DATA %>%
            filter(ICD.Chapter == input$Cause & State %in% input$State) %>%
            select(Year, State, Crude.Rate)

        DATA <- rbind(national, state)

    })


    output$plot1 <- renderPlotly({
        #req(input$Cause)
        #req(input$State)

        #dfSlice <- df %>%
            #filter(ICD.Chapter == input$Cause, State == input$State)

        plot_ly(combinedData(), x = ~Year, y = ~Rate, color = ~State, type='scatter',
                mode = 'lines')

    })

}

shinyApp(ui = ui, server = server)