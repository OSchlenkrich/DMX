library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(yaml)
library(dplyr)
library(data.table)
library(gridExtra)

democracymatrix = fread("website_data_v3.csv")
curr_year = max(democracymatrix$year, na.rm=T)

# Names of countries and regions
choices = list(regions=democracymatrix %>% 
                 pull(country) %>%
                 unique() %>% 
                 .[grepl('[[:digit:]]',.) == T] %>% 
                 as.character,
               countries = 
                 democracymatrix %>% 
                 pull(country) %>%
                 unique() %>% 
                 .[grepl('[[:digit:]]',.) == F] %>% 
                 as.character
)

ui = shinyUI(navbarPage("Democracy Matrix v3, 01/07/2020",
                        tabPanel("15-Field-Matrix",
                                 checkboxInput("checkbox", "Compare with other country", value = FALSE, width = NULL),
                                 
                                 conditionalPanel(
                                   condition = "input.checkbox == false",
                                   selectInput("country15", "Select Country", choices=c("Choose countries"="", choices),
                                               selected = "Germany", multiple = F),
                                   selectInput("year15", "Select Year", choices=c("Choose year"="", unique(democracymatrix$year)),
                                               selected = curr_year, multiple = F),
                                   radioButtons("radio15", "Select Type of Measurment", 
                                                choices = c("Core Measurement", "Context Measurement", "Trade-Off Measurement"), 
                                                selected = "Core Measurement"),
                                   plotlyOutput("heat15"),
                                   column(width=6, verbatimTextOutput("class_output15"))
                                 ),
                                 conditionalPanel(
                                   condition = "input.checkbox == true",
                                   fluidRow(
                                     column(width=6, uiOutput("SecondCountry")
                                     ),
                                     column(width=6, selectInput("country17", "Select 2. Country", choices=c("Choose countries"="", choices),
                                                                 selected = "Germany", multiple = F)
                                     )
                                   ),
                                   fluidRow(
                                     column(width=6, uiOutput("SecondYear")
                                     ),
                                     column(width=6, selectInput("year17", "Select 2. Year", choices=c("Choose year"="", unique(democracymatrix$year)),
                                                                 selected = curr_year, multiple = F)
                                     )
                                   ),
                                   fluidRow(
                                     column(width=6, uiOutput("SecondRadio")
                                     ),
                                     column(width=6, radioButtons("radio17", "Select 2. Type of Measurement", 
                                                                  choices = c("Core Measurement", "Context Measurement", "Trade-Off Measurement"), 
                                                                  selected = "Core Measurement")
                                     )
                                   ),
                                   plotOutput("heat16"),
                                   fluidRow(
                                     column(width=6, verbatimTextOutput("class_output16")),
                                     column(width=6, verbatimTextOutput("class_output17"))
                                   )
                                   
                                 )
                        ),
                        tabPanel("Radar graph",
                                 helpText("Analyze several countries using radar graph"),
                                 selectInput("country_radar", "Select Country", choices=c("Choose countries"="", choices),
                                             selected = "Germany", multiple = T),
                                 selectInput("year_radar", "Select Year", choices=c("Choose year"="", unique(democracymatrix$year)),
                                             selected = curr_year, multiple = F),
                                 radioButtons("radio_radar", "Select Type of Measurement", 
                                              choices = c("Core Measurement", "Context Measurement", "Trade-Off Measurement"), 
                                              selected = "Core Measurement"),
                                 fluidRow(
                                   column(width=6, style='padding:0px;', plotlyOutput("radargraph")
                                   ),
                                   column(width=6, style='padding:0px;', plotlyOutput("radargraph_dim")
                                   )
                                 )
                        ),
                        tabPanel("Country graph",
                                 helpText("Analyze one country by multiple variables"),
                                 selectInput("country_cg", "Select Country", choices=c("Choose countries"="", choices),
                                             selected = "Germany", multiple = F),
                                 selectInput("variable_cg", "Select variables", choices=c("Choose variables"="", colnames(democracymatrix)[-c(1:2)]),
                                             selected = "total_index_core", multiple = T),
                                 plotlyOutput("countrygraph")
                        ),
                        tabPanel("Variable graph",
                                 helpText("Compare multiple countries by one variable"),
                                 selectInput("variable_vg", "Select variable", choices=c("Choose variable"="", colnames(democracymatrix)[-c(1:2)]),
                                             selected = "total_index_core", multiple = F),
                                 selectInput("country_vg", "Select Countries", choices=c("Choose countries"="", choices),
                                             selected = "Germany", multiple = T),
                                 plotlyOutput("variablegraph"))
))
