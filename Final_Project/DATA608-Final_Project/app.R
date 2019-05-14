# # Data - CDC Youth Risk Behavior Surveillance System (YRBSS)
# 
# Original data files can be found here:
# [https://www.kaggle.com/raylo168/dash-yrbss-hs-2017]
# 
# There are 6 files that total about 2 GB of data.
# 
# 1. Alcohol and Other Drug Use.csv
# 2. Dietary Behaviors.csv
# 3. Obesity Overweight and Weight Control.csv
# 4. Physical Activity.csv
# 5. Sexual Behaviors.csv
# 6. Tobacco Use.csv
# 
# Each file contains data at the National, State, Territory, Local, and 'Other' 
# regional levels for the years 1991 through 2017 (odd years only) in one file.  
# Each question is separated into a higher risk and lower risk category with the 
# aggregated percentage of respondents in each cateogory in separate columns.  
# Variables for race, gender, and geolocation are also included.  
# 
# Data was downloaded and processed first to reduce the number of columns and remove 
# rows without and risk values.
# 
# The code for preprocessing can be found here:
# https://github.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/tree/master/Final_Project/DATA608_Final_Project_Data_Processing.Rmd
# 
# Pre-processed files can be found here:
# https://github.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/tree/master/Final_Project/data
# 

library(ggplot2)
library(dplyr)
library(shiny)

ActivityURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Activity.csv'
Activity <- read.csv(ActivityURL)
AlcoholDrugsURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/AlcoholDrugs.csv'
AlcoholDrugs <- read.csv(AlcoholDrugsURL)
DietURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Diet.csv'
Diet <- read.csv(DietURL)
ObesityURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Obesity.csv'
Obesity <- read.csv(ObesityURL)
SexURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Sex.csv'
Sex <- read.csv(SexURL)
TobaccoURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Tobacco.csv'
Tobacco <- read.csv(TobaccoURL)

# URL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/'
# 
# files <- c('Activity', 'AlcoholDrugs', 'Diet', 'Obesity', 'Sex', 'Tobacco')
# 
# for (file in files) {
#     file <- read.csv(paste(URL, file, ".csv", sep=""))
# }
# 
# lapply(files, function(file) {
#     file <- read.csv(paste(URL, file, ".csv", sep=""))
# })

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