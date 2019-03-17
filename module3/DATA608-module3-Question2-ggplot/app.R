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
library(shiny)

URL <- 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv'
df <- read.csv(URL)

ui <- fluidPage(
    headerPanel('State Mortality Rates Explorer'),
    sidebarPanel(
        selectInput('Cause', 'Cause of Death', unique(df$ICD.Chapter), selected='Certain infectious and parasitic diseases'),
        #selectInput('State', 'State', unique(df$State), selected='NY', multiple = TRUE)
        selectizeInput('State', 'Select up to 5 states to compare with the national average', unique(df$State), selected='NY', options = list(maxItems = 5))
    ),
    mainPanel(
        htmlOutput(outputId = 'selection'),
        plotOutput('plot1'),
        h6("Number of deaths per 100,000 people")
    )
)

server <- function(input, output) {

    output$selection <- renderText({
        paste('<b>Death rate for: </b>', input$Cause)
    })

    output$plot1 <- renderPlot({
        national <- df %>%
            filter(ICD.Chapter == input$Cause) %>%
            group_by(Year) %>%
            summarize(Crude.Rate = round((sum(Deaths) / sum(Population)) * 100000, 2)) %>%
            mutate(State = "National") %>%
            arrange(Year)

        state <- df %>%
            filter(ICD.Chapter == input$Cause & State %in% input$State) %>%
            select(Year, State, Crude.Rate)

        DATA <- rbind(national, state)

        ggplot(DATA, aes(x = Year, y = Crude.Rate, color = State)) +
            geom_line(size = 2) +
            scale_x_continuous(minor_breaks = seq(1999, 2010, by = 1),
                               breaks = seq(1999, 2010, by = 1)) +
            xlab("") +
            ylab("") +
            scale_color_brewer(palette="Paired") +
            theme_minimal()
    })

}

shinyApp(ui = ui, server = server)