source("global.R")

thematic::thematic_shiny(font = "auto")

ui <- navbarPage("MES - Analysis of Variance",
                 theme = bs_theme(verion = 4, bootswatch = "materia"),
                 #theme = bs_theme(bootswatch = "journal"),
                 
                 tabPanel(
                          headerPanel(""),
                          sidebarLayout(
                              sidebarPanel(
                                  width = 3,
                                  fluidRow(
                                      column(width = 12,
                                             div(style = "font-size:15px;",
                                             selectInput("dataset", h4("Dataset"), choices=c("Fuel Consumption" = "fuel", 
                                                                                             "Temperature Effect" = "temperature", 
                                                                                             "Random Data" = "random"))),
                                      uiOutput("random_parameter"))),
                                  
                                  HTML(paste0("<h4>","Type of Plot", "</h4>")),
                                  div(style = "font-size:15px;",
                                  radioButtons("aovplottype", label=(""),
                                               choices=c("scatter plot"="scatter", "boxplot"="boxplot", "histogram"="histogram"), selected = "scatter", inline=T)),
                                  hr(),
                                  box(title="", "", actionButton(inputId='ab', 
                                                                 label="home page",
                                                                 icon = icon("home"),
                                                                 onclick ="window.open('https://falcon.uicstat.com/shiny/MES/index.html', '_blank')", 
                                                                 style='padding:5px; font-size:80%')
                                  )
                              ),
                              
                          mainPanel(
                              width = 9,
                              fluidRow(
                                  #align="center",
                                  column(width = 5, 
                                         HTML(paste0("<h4>","Plot","</h4>")),
                                         plotOutput("aovPlot")),
                                  column(width = 7,
                                         HTML(paste0("<h4>","Descriptive Statistics", "</h4>")),
                                         dataTableOutput("aovdataSummary"),
                                         br(),
                                         
                                         HTML(paste0("<h4>","ANOVA Table","</h4>")),
                                         dataTableOutput("aovSummary")
                                  ),
                                  column(width = 12,
                                         br(),
                                         hr(),
                                         withMathJax(includeMarkdown("resource/anova.md")),
                                         br())
                              ))),
                          )
                 
                 
                 
)

# library(profvis)
# profvis(runApp())