## load packages 
library(shiny)
library(knitr)
library(bslib)
library(ggplot2)
library(plotly)
library(leaflet)
source("resource/src/data.R")
thematic::thematic_shiny(font = "auto")
vars <- c(
  "Confirmed cases" = "Confirmed",
  "Deaths" = "Deaths",
  "Recovered" = "Recovered"
)

per <- c(
  "All" = "all",
  "3 Months" = "three_months",
  "1 Month" = "one_month",
  "15 Days" = "fifteen_days"
)
# Define UI for random distribution application 
ui <- navbarPage("Fundamental Statistics",
                theme = bs_theme(bootswatch = "journal"),
                 ############# tab #############
                 ############# tab #############
                 tabPanel("Home Page",
                          fluidRow(
                          column(2),
                          column(8, align = "left",
                                 withMathJax(includeMarkdown("resource/Hello_World.md")),
                                 tags$iframe(src = "https://shiny.rstudio.com/gallery/", width="100%", height=500)
                                 ),
                          column(2)
                          )),
                 ############# tab #############
                 ############# tab #############
                 tabPanel("Statistics Distributions",
                    # Application title
                    headerPanel("Distribution Visualization"),
                    sidebarLayout(
                      position = c("left", "right"),
                      fluid = TRUE,
                    sidebarPanel(
                      width = 4,
                      wellPanel(
                        selectInput("DistType", p(strong("Continuous or Discrete?")),
                                    list("Continous" = "con",
                                         "Discrete" = "dis")),
                        uiOutput("distribution")
                      ),
                      
                      wellPanel(
                        numericInput("seed",p(strong("Set a seed for reproduciblity:")), 42)
                      ),
                      wellPanel(
                        selectInput(inputId = "pdf_cdf",
                                    label = "Please specify 'PDF/PMF' or 'CDF/CMF':",
                                    choices = c("PDF/PMF" = "PDF", "CDF/CMF" = "CDF"))
                      ),
                      wellPanel(
                        sliderInput("n", 
                                    p(strong("Number of observations:")), 
                                    value = 500,
                                    min = 1, 
                                    max = 1000)
                      ),
                      wellPanel(
                        
                        p(strong("Parameters:")),
                        uiOutput("parameter"))),
                    # Show a tabset that includes a plot, summary, and table view
                    # of the generated distribution
                    mainPanel(
                      width = 8,
                      tags$h4("Plot"),
                      plotlyOutput("plot"),
                      tags$br(),
                      tags$h4("Summary"),
                      verbatimTextOutput("summary"),
                      tags$h4("Formula"),
                      uiOutput("formula")
                      ))),
                  ############# tab #############
                  ############# tab #############
                  tabPanel("COVID19 Data",
                           headerPanel("COVID19 Data Visualization"),
                           div(class="outer",
                               tags$head(
                                 HTML('<meta name="viewport" content="width=800">'),
                                 includeCSS("resource/src/style.css")
                               ),
                               br(),
                               leafletOutput("map", width="100%", height="90%"),
                               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                             draggable = FALSE, top = 200, left = "auto", right = 20, bottom = "auto",
                                             width =500, height = "auto",
                                             h2("COVID19 Daily Data"),
                                             selectInput("type", "Stat Type", vars),
                                             dateInput("startdate", "Start Date:", value = "2020-02-01"),
                                             dateInput("enddate", "End Date:", value = "2021-02-01"),
                                             plotlyOutput("country", width = 400, height = 300),
                                             plotlyOutput("fig_usa", width = 400, height = 300)
                               ),
                               tags$div(id="cite", 'Data source: ', icon("github"), a(tags$em('John Hopkins University'), href="https://github.com/CSSEGISandData/COVID-19"), '.')
                           )),
                  tabPanel("bayes theorem",
                           )
)