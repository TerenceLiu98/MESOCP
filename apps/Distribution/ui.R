source("global.R")
thematic::thematic_shiny(font = "auto")

#options(shiny.sanitize.errors = TRUE)
# Define UI for random distribution application 

ui <- navbarPage("MES - Distribution Visualization",
                 theme = bs_theme(verion = 4, bootswatch = "materia"),
                 windowTitle = "Title that appears in the browser bar",
                 tabPanel(headerPanel(""),
                          sidebarLayout(
                              position = c("left", "right"),
                              fluid = TRUE,
                              sidebarPanel(
                                tags$h4("Parameter"),
                                fluidRow(
                                  width = 3,
                                  column(width=6,
                                         selectInput("DistType", p(strong("Choose a distribution")), list("Continous" = "con",  "Discrete" = "dis")),
                                         uiOutput("distribution"),
                                         selectInput(inputId = "pdf_cdf", label = "PDF/PMF or CDF/CMF", choices = c("PDF/PMF" = "PDF", "CDF/CMF" = "CDF"))),
                                  column(width=6, 
                                         p("Parameters:"), uiOutput("parameter"))), 
                                  column(width=12, 
                                         sliderInput("plot_slider", label="X-axis Range", min = -100, max = 100, value = c(-10, 10)),
                                         hr(),
                                         box(title="", "", actionButton(inputId='ab1', 
                                                                        label="home page",
                                                                        icon = icon("home"),
                                                                        onclick ="window.open('https://falcon.uicstat.com/shiny/MES/index.html', '_blank')", 
                                                                        style='padding:5px; font-size:80%'),
                                                           actionButton(inputId='ab2', 
                                                                        label="get code",
                                                                        icon = icon("github"),
                                                                        onclick ="window.open('https://github.com/Bayes-Cluster/MES', '_blank')", 
                                                                        style='padding:5px; font-size:80%'))
                                  )),
                              # Show a tabset that includes a plot, summary, and table view
                              # of the generated distribution
                              mainPanel(
                                fluidRow(
                                    width = 8,
                                    tags$h4("Plot"),
                                    plotlyOutput("plot"),
                                    tags$br(),
                                    #tags$h4("Summary"),
                                    #verbatimTextOutput("summary"),
                                    uiOutput("formula")
                              )))
                          )
)