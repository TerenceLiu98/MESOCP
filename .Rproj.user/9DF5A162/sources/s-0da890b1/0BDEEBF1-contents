library(shiny)
library(knitr)
library(bslib)
library(ggplot2)
library(plotly)
thematic::thematic_shiny(font = "auto")
# Define UI for random distribution application 
ui <- navbarPage("Fundamental Statistics",
                 theme = bs_theme(bootswatch = "sandstone"),
                 ############# tab #############
                 ############# tab #############
                 tabPanel("Home Page",
                          fluidRow(
                          column(2),
                          column(8, align = "left",
                                 withMathJax(includeMarkdown("resource/Hello_World.md")),
                                 tags$iframe(src = "https://shiny.rstudio.com/gallery/", width="100%", height=500)),
                          column(2)
                          )),
                 ############# tab #############
                 ############# tab #############
                 tabPanel("Distributions",
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
                        numericInput("seed",p(strong("Set a seed for reproduciblity:")), 2021)
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
                  tabPanel("COVID-2019 Map", 
                           headerPanel("Distribution Visualization"),
                           fluidRow(
                             column(2),
                             column(8, align = "left",
                                    tags$iframe(src = "https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", width="100%", height=500)),
                             column(2)
                           )
                           )
)