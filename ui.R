source("resource/src/libraries.R")

thematic::thematic_shiny(font = "auto")

# Define UI for random distribution application 
ui <- navbarPage("Mordern Elementary Statistics",
                theme = bs_theme(verion = 4, bootswatch = "minty"),
                 ############# tab #############
                 ############# tab #############
                 tabPanel("Home Page",
                          fluidRow(
                          column(1),
                          column(10, align = "center",
                                 withMathJax(includeMarkdown("resource/Hello_World.md"))
                                 ),
                          column(1)
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
                        numericInput("seed",p(strong("Set a seed for reproduciblity:")), 42),
                      ),
                      wellPanel(
                        selectInput(inputId = "pdf_cdf",
                                    label = "'PDF/PMF' or 'CDF/CMF':",
                                    choices = c("PDF/PMF" = "PDF", "CDF/CMF" = "CDF"))
                      ),
                      wellPanel(
                        sliderInput("n",
                                    p(strong("Number of observations:")),
                                    value = 100,
                                    min = 1,
                                    max = 500)
                      ),
                      wellPanel(
                        
                        p(strong("Parameters:")),
                        uiOutput("parameter"))),
                    # Show a tabset that includes a plot, summary, and table view of the generated distribution
                    mainPanel(
                      width = 8,
                      tags$h4("Plot"),
                      plotlyOutput("plot"),
                      tags$br(),
                      tags$h4("Summary"),
                      verbatimTextOutput("summary"),
                      uiOutput("formula")
                      ))),
                  ############# tab #############
                  ############# tab #############
                tabPanel("COVID19 Data",
                         headerPanel("COVID19 Data Visualization"),
                         fluidRow(
                           sidebarPanel(width = 2,
                                        tags$head(tags$style("/* Position and style citation */
                                                            #cite {
                                                              position: absolute;
                                                              bottom: 10px;
                                                              left: 10px;
                                                              font-size: 12px;
                                                            }")),
                                        selectizeInput(
                                          'country', 'Choose Country', choices = NULL,
                                          options = list(
                                            placeholder = 'Choose Country',
                                            onInitialize = I('function() { this.setValue(""); }')
                                          )
                                        ),
                                        
                                        # choose province 
                                        selectizeInput('province', 'Choose Province', 
                                                       choices = c("Select Province" =  NULL  ),
                                                       options = list( placeholder = 'Province')),
                                        tags$p(""),
                                        
                                        tags$div(
                                          print("Download chosen data"),
                                          br(),
                                          downloadButton('dataDownload', 'Download')),
                                        br(),
                                        tags$p("Summary"),
                                          valueBoxOutput(outputId="summary_confirm", width = "100%"),
                                          valueBoxOutput(outputId="summary_cure", width = "100%"),
                                          valueBoxOutput(outputId="summary_dead", width = "100%"),
                                        br(),
                                        br(),
                                        br(),
                                        tags$div(id="container", 'Data Source:', icon("book-open"), a(tags$em(' Wu, T., Hu, E., Ge, X., & Yu, G. (2021). nCov2019: an R package for studying the COVID-19 coronavirus pandemic. PeerJ, 9, e11421.Wu, T., Hu, E., Ge, X., & Yu, G. (2021). nCov2019: an R package for studying the COVID-19 coronavirus pandemic. PeerJ, 9, e11421'), href="https://doi.org/10.7717/peerj.11421"), '.')
                                        ),
                           mainPanel(width = 10, 
                                     tags$head(tags$style("
                                                      .shiny-notification {position: fixed; top: 45% ;left: 50%}
                                                      .logo, .main-sidebar {position: fixed }")), 
                                     
                                     fluidRow(
                                       # data table
                                       shinydashboard::box(title = "Historical Data Table",
                                                           solidHeader = T,
                                                           width = 6,
                                                           collapsible = F,
                                                           shinycssloaders::withSpinner(DT::dataTableOutput("data_table")), 
                                                           style = "font-size:60%;"),
                                       
                                       # line plot
                                       shinydashboard::box(title = "Cumulative Curve", 
                                                           solidHeader = T,
                                                           width = 6, 
                                                           collapsible = F,
                                                           shinycssloaders::withSpinner(plotlyOutput("line_plot")))
                                     ),  
                                     
                                     fluidRow(
                                       tabBox(
                                         width= 12,
                                         title = "",
                                         selected = "Global Statistics",
                                         tabPanel("Global Statistics", 
                                                  dateInput('date', label = 'Date:', min = '2020-01-22', value =NULL),
                                                  shinycssloaders::withSpinner(
                                                    plotlyOutput("Global_plot",height = 'auto', width = 'auto')
                                                  )),
                                         tabPanel("Vaccine Statisics", 
                                                  shinydashboard::box(
                                                    width = 12,
                                                    collapsible = T,
                                                    shinycssloaders::withSpinner(DT::dataTableOutput("vaccine_table")), 
                                                    style = "font-size: 70%;")),
                                         tabPanel("Therapeutics Statisics", 
                                                  shinydashboard::box(
                                                    width = 12,
                                                    collapsible = T,
                                                    shinycssloaders::withSpinner(DT::dataTableOutput("therapeutics_table")), 
                                                    style = "font-size: 70%;")),
                                         tabPanel("Medical Summary Table", 
                                                  fluidRow(
                                                    column(6,
                                                          shinydashboard::box(title = "current therapeutics candidates ",
                                                                              width = 12,
                                                                              collapsible = T,
                                                                              shinycssloaders::withSpinner(DT::dataTableOutput("Summary_table1")), 
                                                                              style = "font-size: 70%;"),),
                                                    column(6,
                                                          shinydashboard::box(title = "current vaccine candidates",
                                                                              width = 6,
                                                                              collapsible = T,
                                                                              shinycssloaders::withSpinner(DT::dataTableOutput("Summary_table2")), 
                                                                              style = "font-size: 70%;"))
                                                  )
                                         ),
                                         tabPanel("Daily Increase Comparison", 
                                                  selectizeInput('country_list2', label = "Click or input countries to plot curve", 
                                                                 choices = NULL,multiple = TRUE,options = list(create = TRUE,multiple = TRUE)), 
                                                  plotlyOutput("wave_plot"))
                                     ))
                         )),  
                         ),
                tabPanel("Linear Regression",
                         headerPanel("Linear Regression"),
                         sidebarLayout(
                           position = c("left", "right"),
                         sidebarPanel(
                           width = 4,
                           selectInput("lm_x", label = h4("Select the feature:"), 
                                       choices = list("GDP" = "x2", "completed building" = "x1"), 
                                       selected = "x1"),
                           print("1. To remove the *outlier*, first browsing your mouse over a set of points"),
                           print("2. You can toggle these points out of the dataset by click the toggle button"),
                           hr(),
                           h4("Simple Linear Regression Summary:"),
                           verbatimTextOutput("lm_summary"),
                           hr(),
                           actionButton("exclude_toggle", "Toggle in/out the values!"),
                           br(),br(),br(),
                           actionButton("exclude_reset", "Reset")
                         ),
                        mainPanel(
                          width = 8,
                          fluidRow(
                              plotOutput("plot1", height = 600, width = 1200,
                                         click = "plot1_click",
                                         brush = brushOpts(
                                           id = "plot1_brush"
                                         )
                                   ),
                              plotOutput("lm_plot", height=600, width=1200)
                          ))
                         )
                ),
                tabPanel("Confidence Interval",
                         headerPanel("Confidence Interval"),
                         sidebarLayout(
                         sidebarPanel(
                           width = 4,
                           h5("Population mean estimation with known standard deviation (plot 1)"),
                           numericInput("sample_size", label= p("Sample size"), value = 5, min = 1, max = 10, step = 1),
                           numericInput("sample_mean_1", label = p("Sample mean 1"), value = 31.3, min = 27, max = 35, step=0.01),
                           numericInput("sample_mean_2", label = p("Sample mean 2"), value = 31.7, min = 27, max = 35, step=0.01),
                           numericInput("sample_mean_3", label = p("Sample mean 3"), value = 32.5, min = 27, max = 35, step=0.01),
                           #hr(),
                           # h5("Population mean estimation with CLT (plot 2)"),
                           # selectInput("ci_dist", p(strong("Distribution")),
                           #             list("Normal Distribution" = "norm",
                           #                  "Exponential Distribution" = "exp")),
                           # numericInput("sample_size_2", label= p("Sample size (you can try 5, 30, 50, 100, 1000 and find the differences)"), 
                           #              value = 30, min = 1, max = 1000, step = 1),
                           # sliderInput("alpha", label= p("Confidence Level"), value = 0.5, min = 0.1, max = 1),
                          ),
                         mainPanel(
                           width = 8,
                           fluidRow(
                              plotlyOutput("ci_plot", height = 400, width = 1200),
                              br(),
                              withMathJax(includeMarkdown("resource/help/estimation_with_sigma.md")),
                              #hr(),
                              #plotlyOutput("ci_plot_2", height = 400, width = 1200),
                              #withMathJax(includeMarkdown("resource/help/estimation_with_CLT.md")),
                           )
                           
                         )))
                
)