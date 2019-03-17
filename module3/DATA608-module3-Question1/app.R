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
library(shiny)

URL <- 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv'
df <- read.csv(URL)

ui <- fluidPage(
    headerPanel('State Mortality Rates Explorer'),
    sidebarPanel(
        selectInput('Cause', 'Cause of Death', unique(df$ICD.Chapter),
                    selected='Certain infectious and parasitic diseases')
    ),
    mainPanel(
        htmlOutput(outputId = 'selection'),
        plotOutput('plot1', height="auto"),
        h6("States ranked by number of deaths per 100,000 people for the selected cause of death.")
    )
)

server <- shinyServer(function(input, output, session) {

    selectedData <- reactive({
        df %>% filter(ICD.Chapter == input$Cause & Year == 2010)
    })

    output$selection <- renderText({
        paste('<b>Death rate per 100,000 people for: </b>', input$Cause)
    })

    output$plot1 <- renderPlot({

        ggplot(selectedData(), aes(x=reorder(State, -Crude.Rate), y=Crude.Rate)) +
            geom_col(fill = "#638DC7") +
            coord_flip() +
            geom_text(aes(label=Crude.Rate),
                      size=3,
                      hjust=-0.2,
                      color="#3379BA") +
            xlab("State") +
            ylab("Rate") +
            theme(panel.background = element_blank())
    }, height = function() {
        session$clientData$output_plot1_width}
    )
})

shinyApp(ui = ui, server = server)