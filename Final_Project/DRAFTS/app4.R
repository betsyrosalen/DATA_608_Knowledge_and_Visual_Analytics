##### READ ME >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# CDC Youth Risk Behavior Surveillance System (YRBSS)
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

##### LOAD PACKAGES >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(shiny)
library(shinythemes)
library(ggplot2)
library(forcats)
library(stringr)
library(plyr)
library(dplyr)
library(readr)
library(DT)
#library(tools)


##### LOAD DATA >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

ActivityURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Activity.csv'
Activity <- read.csv(ActivityURL, stringsAsFactors = FALSE)
AlcoholDrugsURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/AlcoholDrugs.csv'
AlcoholDrugs <- read.csv(AlcoholDrugsURL, stringsAsFactors = FALSE)
DietURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Diet.csv'
Diet <- read.csv(DietURL, stringsAsFactors = FALSE)
ObesityURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Obesity.csv'
Obesity <- read.csv(ObesityURL, stringsAsFactors = FALSE)
SexURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Sex.csv'
Sex <- read.csv(SexURL, stringsAsFactors = FALSE)
TobaccoURL <- 'https://raw.githubusercontent.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/master/Final_Project/data/Tobacco.csv'
Tobacco <- read.csv(TobaccoURL, stringsAsFactors = FALSE)


##### DATA CLEANING AND PREP >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Replace N/A's in Greater_Risk_Question field
Diet[Diet[, 'ShortQuestionText']=='Fruit consumption >= 1 time', 'Greater_Risk_Question']='Ate fruit 1 or less times per day'
Diet[Diet[, 'ShortQuestionText']=='Fruit consumption >= 2 times', 'Greater_Risk_Question']='Ate fruit 2 or less times per day'
Diet[Diet[, 'ShortQuestionText']=='Fruit consumption >= 3 times', 'Greater_Risk_Question']='Ate fruit 3 or less times per day'
Diet[Diet[, 'ShortQuestionText']=='Milk drinking >= 1 glass', 'Greater_Risk_Question']='Drank milk 1 or less times per day'
Diet[Diet[, 'ShortQuestionText']=='Milk drinking >= 2 glasses', 'Greater_Risk_Question']='Drank milk 2 or less times per day'
Diet[Diet[, 'ShortQuestionText']=='Milk drinking >= 3 glasses', 'Greater_Risk_Question']='Drank milk 3 or less times per day'
Diet[Diet[, 'ShortQuestionText']=='Vegetable eating >=1 time', 'Greater_Risk_Question']='Ate vegetables 1 or less times per day'
Diet[Diet[, 'ShortQuestionText']=='Vegetable eating >=2 times', 'Greater_Risk_Question']='Ate vegetables 2 or less times per day'
Diet[Diet[, 'ShortQuestionText']=='Vegetable eating >=3 times', 'Greater_Risk_Question']='Ate vegetables 3 or less times per day'
Diet[Diet[, 'ShortQuestionText']=='Water drinking >= 1 glass', 'Greater_Risk_Question']='Drank 1 or less glasses of water per day'
Diet[Diet[, 'ShortQuestionText']=='Water drinking >= 2 glasses', 'Greater_Risk_Question']='Drank 2 or less glasses of water per day'
Diet[Diet[, 'ShortQuestionText']=='Water drinking >= 3 glasses', 'Greater_Risk_Question']='Drank 3 or less glasses of water per day'

# Replace REALLY LONG Questions with shorter text
Sex[Sex[, 'Greater_Risk_Question']=='Did not use birth control pills; an IUD (e.g., Mirena or ParaGard) or implant (e.g., Implanon or Nexplanon); or a shot (e.g., Depo-Provera), patch (e.g., OrthoEvra), or birth control ring (e.g., NuvaRing) before last sexual intercourse', 'Greater_Risk_Question']='Did not use birth control pills; an IUD or implant; or a shot, patch, or birth control ring before last sexual intercourse'
Sex[Sex[, 'Greater_Risk_Question']=='Did not use both a condom during last sexual intercourse and birth control pills; an IUD (e.g., Mirena or ParaGard) or implant (e.g., Implanon or Nexplanon); or a shot (e.g., Depo-Provera), patch (e.g., OrthoEvra), or birth control ring (e.g., NuvaRing) before last sexual intercourse', 'Greater_Risk_Question']='Did not use both a condom during last sexual intercourse and birth control pills; an IUD or implant; or a shot, patch, or birth control ring before last sexual intercourse'

# Combine all data into one data frame
df <- rbind(Activity, AlcoholDrugs, Diet, Obesity, Sex, Tobacco)

# Rename 'Total' to 'All' in Sex and Race fields
df[df$Sex=="Total", "Sex"] <- "All"
df[df$Race=="Total", "Race"] <- "All"

# Get rid of NULL values in Description field
df[df[, 'Description']=='NULL', 'Description']=''

# Create Sorted Lists for dropdown menus
year <- df[, c("YEAR")] %>% unique() %>% sort()
location <- df[, c("LocationDesc","StratificationType")] %>% distinct() %>% arrange(StratificationType, LocationDesc)
gender <- df[, c("Sex")] %>% unique() %>% sort()
race <- df[, c("Race")] %>% unique() %>% sort()
topics <- df[, c("Topic","Subtopic")] %>% distinct() %>% arrange(Topic,Subtopic)
questions <- df[, c("Topic","Subtopic","ShortQuestionText","Greater_Risk_Question",
                    "Description")] %>% distinct() %>% arrange(Topic,Subtopic,Greater_Risk_Question)


# # NOT NEEDED unless I can figure out how to color code the x-axis labels for plot2b
# palette <- factor(c("#1A0725", "#2A1505", "#0A1229", "#001808", "#20050F", "#191919"))
# names(palette) <- levels(factor(topics$Topic))
# df$Colors <- palette[df$Topic]

##### USER INTERFACE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

ui <- fluidPage(
    #themeSelector(),
    theme = shinytheme("flatly"),
    titlePanel('CDC Youth Risk Behavior Surveillance System Explorer'),

    navbarPage("YRBSS",

# <<<<<<<<<<<<<<<<<<<<<<<  Compare Locations  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

        tabPanel("Compare Locations",
            sidebarPanel(
                h3('Line Plot Options'),
                selectInput('risk', 'Risky Behavior', questions$Greater_Risk_Question,
                            selected='Ever used marijuana'),
                selectizeInput('location',
                               'Select up to 5 locations to compare with the national average',
                               location$LocationDesc, selected='New York',
                               options = list(maxItems = 5)),
                selectInput('gender', 'Gender', gender, selected='Total'),
                selectInput('race', 'Race', race, selected='Total'),
                hr(),
                br(),
                br(),
                h3('Bar Plot Options'),
                selectInput('year1', 'Select a year to compare all other locations', year,
                            selected='2005'),
                br()
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel('Plots',
                        br(),
                        h4(htmlOutput('selection1')),
                        h5(htmlOutput('description1')),
                        plotOutput('plot1a'),
                        h6(strong('Note:'), 'Not all questions were asked in all locations in all years.  Missing plot points indicate data that was not collected.'),
                        br(),
                        h4(htmlOutput('locations1')),
                        plotOutput('plot1b', height = "900px")
                    ),
                    tabPanel('Line Plot Data',
                        br(),
                        h3('Line Plot Data'),
                        tableOutput('table1a')
                    ),
                    tabPanel('Bar Plot Data', 
                        br(),
                        h3('Bar Plot Data'),
                        tableOutput('table1b')
                    )
                )
            )
        ),

# <<<<<<<<<<<<<<<<<<<<<<<<<<  Compare Risks  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

        tabPanel("Compare Risks",
            sidebarPanel(
                h3('Line Plot Options'),
                selectInput('location2', 'Location', location$LocationDesc,
                            selected='New York City, NY'),
                selectizeInput('risk2',
                                'Select up to 5 risky behaviors to compare',
                                questions$Greater_Risk_Question,
                                selected='Ever used marijuana',
                                options = list(maxItems = 5)),
                selectInput('gender2', 'Gender', gender, selected='Total'),
                selectInput('race2', 'Race', race, selected='Total'),
                hr(),
                br(),
                br(),
                h3('Bar Plot Options'),
                selectInput('year2', 'Select a year to compare all behaviors', year,
                            selected='2005'),
                br()
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel('Plots',
                        br(),
                        h4(htmlOutput('selection2')),
                        plotOutput('plot2a', height = "450px"),
                        h6(strong('Note:'), 'Not all questions were asked in all locations in all years.  Missing plot points indicate data that was not collected.'),
                        br(),
                        h4(htmlOutput('behavior2')),
                        plotOutput('plot2b', height = "1200px")
                    ),
                    tabPanel('Line Plot Data',
                        br(),
                        h3('Line Plot Data'),
                        tableOutput('table2a')
                    ),
                    tabPanel('Bar Plot Data',   
                        br(),
                        h3('Bar Plot Data'),
                        tableOutput('table2b')
                    )
                )
            )
        ),

# <<<<<<<<<<<<<<<<<<<<<<<  Project Description  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

        tabPanel("Project Description",
            #includeMarkdown("./intro.md"),
            br(),
            h2('CDC Youth Risk Behavior Surveillance System (YRBSS)'),
            p("The Youth Risk Behavior Surveillance System is the only study of it's kind that 'monitors six categories of health-related behaviors that contribute to the leading causes of death and disability among youth and adults' over the past 16 years.  Understanding which of these behaviors youth are engaged in, how often or to what extent they engage in them and how those behaviors are changing over time can lead to the development of better preventative programs and health education."),
            h4('Original CDC data files can be found here:'),
            a('www.cdc.gov/healthyyouth/data/yrbs/index.htm', href = 'www.cdc.gov/healthyyouth/data/yrbs/index.htm'),
            h4('Kaggle data files used for this project can be found here:'),
            p(a('https://www.kaggle.com/raylo168/dash-yrbss-hs-2017', href = 'https://www.kaggle.com/raylo168/dash-yrbss-hs-2017')),
            p('There are 6 files that total about 2 GB of data.'),
            tags$ol(
                tags$li('Alcohol and Other Drug Use.csv'),
                tags$li('Dietary Behaviors.csv'),
                tags$li('Obesity Overweight and Weight Control.csv'),
                tags$li('Physical Activity.csv'),
                tags$li('Sexual Behaviors.csv'),
                tags$li('Tobacco Use.csv')
            ),
            p("Each file contains data at the National, State, Territory, Local, and 'Other' regional levels for the years 1991 through 2017 (odd years only) in one file.  Each question is separated into a higher risk and lower risk category with the aggregated percentage of respondents in each cateogory in separate columns.  Variables for race, gender, and geolocation are also included."),
            p('Data was downloaded from Kaggle and processed first to reduce the number of columns and remove rows without risk values.'),
            h4('The R code for preprocessing can be found here:'),
            a('https://github.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/tree/master/Final_Project/DATA608_Final_Project_Data_Processing.Rmd', href='https://github.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/tree/master/Final_Project/DATA608_Final_Project_Data_Processing.Rmd'),
            h4('Pre-processed csv files can be found here:'),
            a('https://github.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/tree/master/Final_Project/data', href='https://github.com/betsyrosalen/DATA_608_Knowledge_and_Visual_Analytics/tree/master/Final_Project/data'),
            hr()
        )
    )
)


##### SERVER >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

server <- function(input, output) {
    
# <<<<<<<<<<<<<<<<<<<<<<<  Reactive Data Prep  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    national <- reactive({
        df %>%
        filter(Greater_Risk_Question==input$risk &
                   StratificationType=="National" &
                   Sex==input$gender & Race==input$race) %>%
        select(YEAR, LocationDesc, Greater_Risk_Question, Greater_Risk_Data_Value) %>%
        arrange(YEAR, LocationDesc, Greater_Risk_Question)
    })

    selectedLoc <- reactive({
        df %>%
        filter(Greater_Risk_Question==input$risk &
                   LocationDesc %in% input$location &
                   Sex==input$gender & Race==input$race) %>%
        select(YEAR, LocationDesc, Greater_Risk_Question, Greater_Risk_Data_Value) %>%
        arrange(YEAR, LocationDesc, Greater_Risk_Question)
    })

    DATA1a <- reactive({
        rbind(national(), selectedLoc())
    })

    DATA1b <- reactive({
        df %>%
            filter(Greater_Risk_Question==input$risk &
                       YEAR==input$year1 &
                       Sex==input$gender & Race==input$race) %>%
            select(YEAR, LocationDesc, Greater_Risk_Question, Greater_Risk_Data_Value) %>%
            arrange(YEAR, LocationDesc, Greater_Risk_Question)
    })

    DATA2a <- reactive({
        df %>%
            filter(Greater_Risk_Question %in% input$risk2 &
                       LocationDesc==input$location2 &
                       Sex==input$gender2 & Race==input$race2) %>%
            select(YEAR, LocationDesc, Greater_Risk_Question, Greater_Risk_Data_Value) %>%
            arrange(YEAR, LocationDesc, Greater_Risk_Question)
    })

    DATA2b <- reactive({
        df %>%
            filter(LocationDesc==input$location2 &
                       YEAR==input$year2 &
                       Sex==input$gender2 & Race==input$race2) %>%
            select(YEAR, LocationDesc, Greater_Risk_Question, Greater_Risk_Data_Value) %>%
            arrange(YEAR, LocationDesc, Greater_Risk_Question)
    })

# <<<<<<<<<<<<<<<<<<<<<<<  Compare Locations  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    output$selection1 <- renderText({
        paste('<strong>Percent of', if(input$gender != "All"){tolower(input$gender)},
              if(input$race != "All"){input$race},
              'high schoolers who reported that they... </strong>', input$risk)
    })

    output$description1 <- renderText({
        paste('<br> (', questions[questions[, 'Greater_Risk_Question']==input$risk,
                                  'Description'], ')')
    })

    output$locations1 <- renderText({
        paste('<strong>',input$year1, ' data for',
              if(input$gender != "All"){tolower(input$gender)},
              if(input$race != "All"){input$race},
              'high schoolers in all locations who...</strong>', input$risk)
    })

    output$plot1a <- renderPlot({
        ggplot(DATA1a(), aes(x = YEAR, y = Greater_Risk_Data_Value,
                            color = LocationDesc)) +
            geom_line(size = 2) +
            scale_x_continuous(minor_breaks = seq(min(year), max(year), by = 2),
                               breaks = seq(min(year), max(year), by = 2)) +
            xlab("") +
            ylab("") +
            scale_color_brewer(palette="Paired") +
            theme_minimal()
    })

    output$plot1b <- renderPlot({
        ggplot(DATA1b(), aes(x = reorder(LocationDesc, Greater_Risk_Data_Value),
                             y = Greater_Risk_Data_Value,
                         fill=factor(LocationDesc %in% input$location))) +
            geom_bar(stat="identity", show.legend=FALSE) +
            scale_fill_manual(name = "area", values=c("#58BFFF", "#3300FF")) +
            coord_flip() +
            geom_text(aes(label=round(Greater_Risk_Data_Value, 1)),
                      size=4, hjust=-0.3, color="darkgray") +
            xlab("Location") +
            ylab("Percent reported behavior") +
            theme_minimal() +
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
    })
    
    output$table1a <- renderTable({
        DATA1a()},
        striped = TRUE, spacing = "l", align = "lllc", digits = 2, width = "100%",
        caption = ""
    )
    
    output$table1b <- renderTable({
        DATA1b()},
        striped = TRUE, spacing = "l", align = "lllc", digits = 2, width = "100%",
        caption = ""
    )
    
# <<<<<<<<<<<<<<<<<<<<<<<<<  Compare Risks  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    output$selection2 <- renderText({
        paste('<strong>Comparative risks for ',
              if(input$gender2 != "All"){tolower(input$gender2)},
              if(input$race2 != "All"){input$race2},
              'high schoolers in... </strong>', input$location2)
    })

    output$behavior2 <- renderText({
        paste('<strong>All risky behaviors for',
              if(input$gender2 != "All"){tolower(input$gender2)},
              if(input$race2 != "All"){input$race2},
              'high schoolers in', input$year2, 'in...</strong>' ,input$location2)
    })

    output$plot2a <- renderPlot({
        ggplot(DATA2a(), aes(x = YEAR, y = Greater_Risk_Data_Value,
                            color = Greater_Risk_Question)) +
            geom_line(size = 2) +
            scale_x_continuous(minor_breaks = seq(min(year), max(year), by = 2),
                               breaks = seq(min(year), max(year), by = 2)) +
            xlab("") +
            ylab("") +
            scale_color_brewer(palette="Paired") +
            theme_minimal() +
            theme(legend.position="bottom", legend.direction="vertical")
    })

    output$plot2b <- renderPlot({
        ggplot(DATA2b(), aes(x = reorder(Greater_Risk_Question, Greater_Risk_Data_Value),
                             y = Greater_Risk_Data_Value,
                             fill=factor(Greater_Risk_Question %in% input$risk2))) +
            geom_bar(stat="identity", show.legend=FALSE) +
            scale_fill_manual(name = "area", values=c("#58BFFF", "#3300FF")) +
            geom_text(aes(label=round(Greater_Risk_Data_Value, 1)),
                      size=4, hjust = -0.3, angle = 90, color="darkgray") +
            xlab("Risky Behavior") +
            ylab("Percent reported behavior") +
            theme_minimal() +
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"),
                  axis.text.x=element_text(angle = 90, hjust = 1))
    })
    
    output$table2a <- renderTable({
        DATA2a()},
        striped = TRUE, spacing = "l", align = "lllc", digits = 2, width = "100%",
        caption = ""
    )
    
    output$table2b <- renderTable({
        DATA2b()},
        striped = TRUE, spacing = "l", align = "lllc", digits = 2, width = "100%",
        caption = ""
    )
    
}

shinyApp(ui = ui, server = server)

# https://stackoverflow.com/questions/22972478/color-axis-text-by-variable
# myPalette <- getColors(numColors)
# names(myPalette) <- levels(state_data$Region) # Give every color an appropriate name
# p <- p + theme(axis.text.x = element_text(colour=myPalette[state_data$Region])))