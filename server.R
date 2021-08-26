source("resource/src/libraries.R")
cities <-read.csv("resource/data/cities.csv")

# Define server logic for random distribution application
server <- shinyServer(function(input, output, session){
  
  # Draw Distribution
  output$distribution <- renderUI({
    if(input$DistType == "con"){
      list(
        selectInput("dist", p(strong("Continuous Distributions")),
                    list("Normal Distribution" = "norm",
                         "t Distribution" = "t",
                         "Beta Distribution" = "beta",
                         "Gamma Distribution" = "gamma",
                         "Chi-Squared Distribution" = "chisq",
                         "Exponential Distribution" = "exp",
                         "F Distribution" = "f",
                         "Uniform Distribution" = "unif"),
                    selected = NULL
        )
      )
    }
    else if (input$DistType == "dis"){
      list(
        selectInput("dist", p(strong("Discrete Distributions")),
                    list("Binomial Distribution" = "binom",
                         "Geometric Distribution" = "geom",
                         "Poisson Distribution" = "pois"),
                    selected = NULL
        )
      )
    }
  })
  
  output$parameter <- renderUI({
    if(input$dist == "beta"){
      list(
        numericInput("shape1", "shape1:", 1),
        numericInput("shape2", "shape2:", 1)
      )
    } else if(input$dist == "binom"){
      list(
        numericInput("size", "size:", 10),
        numericInput("prob", "prob:", 0.1)
      )
    } else if(input$dist == "cauchy"){
      list(
        numericInput("location", "location:", 0),
        numericInput("scale", "scale:", 1)
      )
    } else if(input$dist == "chisq"){
      numericInput("df1", "df:", 1)
    } else if(input$dist == "exp"){
      numericInput("rate", "rate:", 1)
    } else if(input$dist == "f"){
      list(
        numericInput("df1", "df1:", 1),
        numericInput("df2", "df2:", 1)
      )
    } else if(input$dist == "gamma"){
      list(
        numericInput("shape1", "shape:", 1),
        numericInput("rate", "rate:", 1)
      )
    } else if(input$dist == "geom"){
      numericInput("prob", "prob:", 0.1)
    } else if(input$dist == "hyper"){
      list(
        numericInput("m", "number of white balls in the urn:", 1),
        numericInput("n", "number of black balls in the urn:", 1),
        numericInput("k", "number of balls drawn from the urn:", 1)
      )
    } else if(input$dist == "lnorm"){
      list(
        numericInput("meanlog", "mean on the log scale:", 0),
        numericInput("sdlog", "sd on the log scale:", 1)
      )
    } else if(input$dist == "nbinom"){
      list(
        numericInput("size", "size:", 10),
        numericInput("prob", "prob:", 0.1)
      )
    } else if(input$dist == "norm"){
      list(
        numericInput("mean", "mean:", 0),
        numericInput("sd", "sd:", 1)
      )
    } else if(input$dist == "pois"){
      numericInput("lamda", "lamda:", 1) 
    } else if(input$dist == "t"){
      numericInput("df1", "df:", 1)
    } else if(input$dist == "unif"){
      list(
        numericInput("min", "min:", 0),
        numericInput("max", "max:", 1)
      )
    }else if(input$dist == "weibull"){
      list(
        numericInput("shape1", "shape:", 1),
        numericInput("scale", "scale:", 1)
      )
    }  
  })
  
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  data <- reactive({  
    
    set.seed(input$seed)
    
    # crate parameters of dist
    if (input$pdf_cdf == "PDF"){
      if(input$dist == "beta"){
        data.frame(pdf = rbeta(input$n, input$shape1, input$shape2))
      } else if(input$dist == "binom"){
        data.frame(pdf = rbinom(input$n, input$size, input$prob))
      } else if(input$dist == "cauchy"){
        data.frame(pdf = rcauchy(input$n, input$location, input$scale))
      } else if(input$dist == "chisq"){
        data.frame(pdf = rchisq(input$n, input$df1))
      } else if(input$dist == "exp"){
        data.frame(pdf = rexp(input$n, input$rate))
      } else if(input$dist == "f"){
        data.frame(pdf = rf(input$n, input$df1, input$df2))
      } else if(input$dist == "gamma"){
        data.frame(pdf = rgamma(input$n, input$shape1, input$rate))
      } else if(input$dist == "geom"){
        data.frame(pdf = rgeom(input$n, input$prob))
      } else if(input$dist == "hyper"){
        data.frame(pdf = rhyper(input$n, input$m, input$n, input$k))
      } else if(input$dist == "lnorm"){
        data.frame(pdf = rlnorm(input$n, input$meanlog, input$sdlog))
      } else if(input$dist == "multinom"){
        data.frame(pdf = rmultinom(input$n, input$size, input$prob))
      } else if(input$dist == "nbinom"){
        data.frame(pdf = rnbinom(input$n, input$size, input$prob))
      } else if(input$dist == "norm"){
        data.frame(pdf = rnorm(input$n, input$mean, input$sd))
      } else if(input$dist == "pois"){
        data.frame(pdf = rpois(input$n, input$lamda))
      } else if(input$dist == "t"){
        data.frame(pdf = rt(input$n, input$df1))
      } else if(input$dist == "unif"){
        data.frame(pdf = runif(input$n, input$min, input$max))
      }else if(input$dist == "weibull"){
        data.frame(pdf = rweibull(input$n, input$shape1, input$scale))
      }
    }
    else if(input$pdf_cdf == "CDF"){
      pdf_cdf = input$pdf_cdf
      if(input$dist == "beta"){
        x = rbeta(input$n, input$shape1, input$shape2)
        data.frame(x = x, cdf = pbeta(x,input$shape1, input$shape2))
      } else if(input$dist == "binom"){
        x = rbinom(input$n, input$size, input$prob)
        data.frame(x = x, cdf = pbinom(x, input$size, input$prob))
      } else if(input$dist == "cauchy"){
        x = rcauchy(input$n, input$location, input$scale)
        data.frame(x = x, cdf = pcauchy(x, input$location, input$scale))
      } else if(input$dist == "chisq"){
        x = rchisq(input$n, input$df1)
        data.frame(x = x, cdf = pchisq(x, input$df1))
      } else if(input$dist == "exp"){
        x = rexp(input$n, input$rate)
        data.frame(x = x, cdf = pexp(x, input$rate))
      } else if(input$dist == "f"){
        x = rf(input$n, input$df1, input$df2)
        data.frame(x = x, cdf = pf(x, input$df1, input$df2))
      } else if(input$dist == "gamma"){
        x = rgamma(input$n, input$shape1, input$rate)
        data.frame(x = x, cdf = pgamma(x, input$shape1, input$rate))
      } else if(input$dist == "geom"){
        x = rgeom(input$n, input$prob)
        data.frame(x = x, cdf = pgeom(x, input$prob))
      } else if(input$dist == "hyper"){
        x = rhyper(input$n, input$m, input$n, input$k)
        data.frame(x = x, cdf = phyper(x, input$m, input$n, input$k))
      } else if(input$dist == "lnorm"){
        x = rlnorm(input$n, input$meanlog, input$sdlog)
        data.frame(x = x, cdf = plnorm(x, input$meanlog, input$sdlog))
      } else if(input$dist == "nbinom"){
        x = rbinom(input$n, input$size, input$prob)
        data.frame(x = x, cdf = pnbinom(x, input$size, input$prob))
      } else if(input$dist == "norm"){
        x = rnorm(input$n, input$mean, input$sd)
        data.frame(x = x, cdf = pnorm(x, input$mean, input$sd))
      } else if(input$dist == "pois"){
        x = rpois(input$n, input$lambda)
        data.frame(x = x, cdf = ppois(x, input$lamda))
      } else if(input$dist == "t"){
        x = rt(input$n, input$df1)
        data.frame(x = x, cdf = pt(x, input$df1))
      } else if(input$dist == "unif"){
        x = runif(input$n, input$min, input$max)
        data.frame(x = x, cdf = punif(x, input$min, input$max))
      }else if(input$dist == "weibull"){
        x = rweibull(input$n, input$shape1, input$scale)
        data.frame(x = x, cdf = pweibull(x, input$shape1, input$scale))
      }
    }
  })
  # Generate a plot of the data
  output$plot <- renderPlotly(
    if (input$pdf_cdf == "PDF"){
      {g<-ggplot(data=data(), aes(x = pdf)) + 
        geom_histogram(aes(y=..density..,fill = ..count..))+
        geom_density(alpha=.8) 
      ggplotly(g)}
    }
    else if(input$pdf_cdf == "CDF"){
      {g<-ggplot(data=data(), aes(x = x, y = cdf)) + 
        geom_area(fill = "lightblue")
      ggplotly(g)}}
  )
  #hist(data(), 
  #     main=paste("random generation for the", dist, "distribution", sep=" "))
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())})
  
  
  output$formula <- renderUI({
    ## Continuous
    if(input$dist == "norm"){
      withMathJax(includeMarkdown("resource/distributions/continuous/Normal.md"))
    }
    else if(input$dist == "t"){
      withMathJax(includeMarkdown("resource/distributions/continuous/StudentT.md"))
    }
    else if (input$dist == "beta"){
      withMathJax(includeMarkdown("resource/distributions/continuous/Beta.md"))
    }
    else if (input$dist == "gamma"){
      withMathJax(includeMarkdown("resource/distributions/continuous/Gamma.md"))
    }
    else if(input$dist == "chisq"){
      withMathJax(includeMarkdown("resource/distributions/continuous/Chisquare.md"))
    }
    else if(input$dist == "exp"){
      withMathJax(includeMarkdown("resource/distributions/continuous/Expnential.md"))
    }
    else if(input$dist == "f"){
      withMathJax(includeMarkdown("resource/distributions/continuous/Fdistribution.md"))
    }
    else if(input$dist == "unif"){
      withMathJax(includeMarkdown("resource/distributions/continuous/Uniform.md"))
    }
    ## Discrete
    else if(input$dist == "binom"){
      withMathJax(includeMarkdown("resource/distributions/discrete/Binomial.md"))
    }
    else if(input$dist == "geom"){
      withMathJax(includeMarkdown("resource/distributions/discrete/Geometric.md"))
    }
    else if(input$dist == "pois"){
      withMathJax(includeMarkdown("resource/distributions/discrete/Poisson.md"))
    }
    })
  
  ## COVID19
  reflash <- function(){
    withProgress({
      incProgress(message = "Querying data from source...")
      latest_data <- get_latest_data()
      incProgress(message = "Querying data from source...")
      global_data <- get_global_data()
      incProgress(message = "Querying data from source...")
      historical_data <- get_history_data()
      incProgress(message = "Querying data from source...")
      vaccine_data <- get_vaccine_data()
      incProgress(message = "Querying data from source...")
      therapeutics_data <- get_therapeutics_data()
      res = list(lastest=latest_data,
                 global=global_data,
                 historical=historical_data,
                 vaccine=vaccine_data,
                 therapeutics=therapeutics_data)
      return(res)
    })
    return(res)
  }
  res <- reflash()
  latest_data=res$lastest
  global_data=res$global
  historical_data=res$historical
  vaccine_data=res$vaccine
  therapeutics_data=res$therapeutics
  
  # update country list
  country_list <- dplyr::filter(latest_data$table, updated == latest_data$time) %>% 
    arrange(desc(cases)) %>% .$country %>% sort() 
  updateSelectizeInput(session, 'country', choices = country_list, server = TRUE)
  
  t = historical_data$time
  
  # update province list
  observe({
    province_list <- unique(subset(historical_data$province, country == input$country)$province)
    if (length(province_list) > 0) {
      updateSelectInput(session, "province", choices = c("All",province_list))
    } else {
      updateSelectInput(session, "province", choices = c("--",province_list))
    }
    
  })
  
  # update cor_type list
  observe({
    cor_type_list <- colnames(latest_data$detail) 
    cor_type_list <- cor_type_list[!cor_type_list %in% c("updated","country","countryInfo")]
    updateSelectInput(session, "cor_type1", 
                      choices = cor_type_list)
    updateSelectInput(session, "cor_type2", 
                      choices = cor_type_list[c(2,1,3:length(cor_type_list))])
  })
  
  # update country list2
  country_list <- unique(historical_data$table$country) %>% sort() 
  updateSelectizeInput(session, 'country_list2', choices = country_list, server = TRUE)
  
  updateDateInput(session, 'date', value = t, max=t)
  # prepare the table content
  df <- reactive({
    if ( input$province == "All" | input$province == "--"  ) {
      x = subset(historical_data$table, country == input$country)
    }
    else {
      x = subset(historical_data$province, province == input$province)
    }
    x = x[,c("date","cases","deaths","recovered")]
    return(x)
  })
  historical_data$table %>%
    group_by(country) %>%
    arrange(country,date) %>%
    mutate(diff = cases -  dplyr::lag(cases, default =  dplyr::first(cases))) -> a
  # output data table
  output$data_table = DT::renderDataTable({
    validate(need(input$country != "", "Loading"))
    df()
  },rownames = FALSE )
  
  # output header summary 
  output$summary_confirm <- renderValueBox({
    validate(need(input$country != "", "Loading"))
    x = df()
    valueBox(
      paste0(x[which(x$date == t),]$cases, " Cases"), t)
  })
  
  output$summary_cure <- renderValueBox({
    validate(need(input$country != "", "Loading"))
    x = df()
    valueBox(
      paste0(x[which(x$date == t),]$recovered, " Recovered"), t)
  })
  
  output$summary_dead <- renderValueBox({
    validate(need(input$country != "", "Loading"))
    x = df()
    valueBox(
      paste0(x[which(x$date == t),]$deaths, " Deaths"), t)
  })
  
  # Growth Curve
  output$line_plot <- renderPlotly({
    validate(need(input$country != "", "Loading"))
    x = gather(df(), curve, count, -date)
    p = ggplot(x, aes(date, log2(count), color = curve, Counts=count, Type=curve )) +
      geom_point() + geom_line() + xlab(NULL) + ylab("Log2 of count") +
      scale_color_manual(values=c("#f39c12", "#d81b60", "#000080")) +
      theme_bw() + 
      theme(legend.position = "none") +
      theme(axis.text = element_text(angle = 15, hjust = 1)) +
      scale_x_date(date_labels = "%Y-%m-%d")
    ggplotly(p,tooltip=c("x","Counts","Type"))
  })
  
  # data download
  
  output$dataDownload <- downloadHandler(
    filename = function() {paste0("coronavirus_histrical_",t,".tsv")},
    content = function(file) {
      # issues with Chinese characters solved
      # http://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
      con <- file(file, open = "w+", encoding = "native.enc")
      df <- df()
      df$country = input$country
      df$province = input$province
      writeLines( paste( colnames(df), collapse = "\t"), con = con, useBytes = TRUE)
      for(i in 1:nrow( df) )
        #write line by line 
        writeLines( paste( as.character(df[i,]), collapse = "\t"), con = con, useBytes = TRUE)
      close(con)
    }
  )
  
  # bottom panel plots
  
  
  output$Global_plot <- renderPlotly({
    #validate(need(input$type != "", "Loading"))
    
    geoINFO = data.frame(
      country = latest_data$detail$country, 
      ISO3 = latest_data$detail$countryInfo$iso3,
      long = latest_data$detail$countryInfo$long,
      lat = latest_data$detail$countryInfo$lat,
      population = latest_data$detail$population
    )
    df = subset(historical_data$table ,date == input$date)
    df2 = merge(df, geoINFO, by='country')
    df_last = subset(historical_data$table, date == input$date - 1)
    df2_last = merge(df_last, geoINFO, by='country')
    df2$NewCases <- df2$cases - df2_last$cases
    df2$rates <- df2$cases / df2$population
    
    g <- list(
      scope = 'world',
      showland = TRUE,
      showcountries = TRUE,
      landcolor = toRGB("gray95"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("black")
    )
    
    # specify map projection/options
    l <- list(color = toRGB("grey95"), width = 0.5)
    g2 <- list(
      showframe = TRUE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator')
    )
    
    fig <- plot_geo(df2,
                    hoverinfo = 'text',
                    text = ~paste(
                      '</br> Region: ', country,
                      '</br> Cases: ', cases,
                      '</br> Deaths: ', deaths,
                      '</br> Population: ', population,
                      '</br> Cases/Poluation:', round(rates, 5),
                      '</br> New cases:', NewCases,
                      '</br> </br> Date: ', date 
                    )
    )
    fig <- fig %>% add_trace(
      #        z = as.formula(paste0("~`", input$type, "`")) , 
      #        color = as.formula(paste0("~`", input$type, "`")), colors = 'Reds', 
      #        hoverinfo= as.formula(paste0("~`", input$type, "`")),
      z = ~cases,
      color = ~cases,
      colors = 'Reds',
      locations = ~ISO3
    )
    fig <- fig %>% colorbar(title = "cases" )
    fig <- fig %>% plotly::layout(
      title = paste0('Covid-19 World Map: ', input$date),
      geo = g
    ) 
  })
  
  output$vaccine_table = DT::renderDataTable({
    validate(need( exists("vaccine_data"), "Loading"))
    vaccine_data$table[,!colnames(vaccine_data$table) %in% "details"]
  },rownames = FALSE ) 
  
  output$therapeutics_table = DT::renderDataTable({
    validate(need( exists("therapeutics_data"), "Loading"))
    therapeutics_data$table[,!colnames(therapeutics_data$table) %in% c("details")]
  },rownames = FALSE ) 
  
  output$Summary_table1 = DT::renderDataTable({
    validate(need( exists("vaccine_data"), "Loading"))
    therapeutics_data$summary
  },rownames = FALSE ) 
  output$Summary_table2 = DT::renderDataTable({
    validate(need( exists("therapeutics_data"), "Loading"))
    vaccine_data$summary
  },rownames = FALSE ) 
  
  output$active_plot <- renderPlotly({
    validate(need( exists("latest_data"), "Loading"))
    df = latest_data$detail
    df = df[order(df$activePerOneMillion,decreasing = T),] 
    p <- ggplot(df, aes(country,activePerOneMillion)) + 
      geom_col(color="firebrick")  + scale_x_discrete(limits= df$country) +
      geom_hline(yintercept = mean(df$activePerOneMillion)) + 
      theme_minimal() + ylab("Active cases per million population") +
      theme(  axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = 'none')
    ggplotly(p, tooltip = c("country","activePerOneMillion"))
  })
  
  output$Mortality_plot <- renderPlotly({
    validate(need( exists("latest_data"), "Loading"))
    df = latest_data$detail
    df = df[order(df$deathsPerOneMillion,decreasing = T),] 
    p <- ggplot(df, aes(country,deathsPerOneMillion)) + 
      geom_col(color="firebrick")  + scale_x_discrete(limits= df$country) +
      geom_hline(yintercept = mean(df$deathsPerOneMillion)) + 
      theme_minimal() + ylab('Mortality per million population') +
      theme(  axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = 'none')
    ggplotly(p, tooltip = c("country","deathsPerOneMillion"))
  })
  
  # cor_plot
  output$cor_plot <- renderPlotly({
    validate(need( exists("latest_data"), "Loading"))
    df = latest_data$detail 
    x = input$cor_type1
    y = input$cor_type2
    p = ggplot(df, aes_string(x,y, color="country")) + 
      geom_jitter() +  guides(color = FALSE )   +
      theme_minimal() +  
      labs(subtitle="The size of the dots in the graph corresponds to the number of patients diagnosed today") +
      theme(  axis.text=element_blank(),
              axis.ticks=element_blank(),
              legend.position = 'none' )    
  })
  # wave_plot
  output$wave_plot <- renderPlotly({
    validate(need(input$country_list2, "Pleas choose some countries"))
    tmp = subset(a, country %in% input$country_list2)
    tmp$Log2Increase = log2(tmp$diff + 1) 
    tmp$Increase = tmp$diff
    p <- ggplot(tmp,aes(date, Log2Increase,color=country, Increase = Increase)) + geom_line()  +
      labs(y="Daily increase cases(log2 scale)") + 
      theme(axis.text = element_text(angle = 15, hjust = 1)) +
      scale_x_date(date_labels = "%Y-%m-%d") + theme_minimal()
    ggplotly(p, tooltip = c("country","date", "Increase"))
  })
  ### reflash button
  observeEvent(input$reflashButton, {
    res <- reflash()
    latest_data=res$lastest
    global_data=res$global
    historical_data=res$historical
    vaccine_data=res$vaccine
    therapeutics_data=res$therapeutics
  })
  
  ## Linear Regression
    vals <- reactiveValues(
      keeprows = rep(TRUE, nrow(cities))
    )
    vals2 <- reactiveValues(
      keeprows2 = rep(TRUE, nrow(cities))
    )

    output$plot1 <- renderPlot({
      if (input$lm_x == "x2"){
      # Plot the kept and excluded points as two separate data sets
        keep    <- cities[ vals$keeprows, , drop = FALSE]
        exclude <- cities[!vals$keeprows, , drop = FALSE]
  
        ggplot(keep, aes(planing, GDP)) + geom_point() +
          geom_smooth(method = lm, fullrange = TRUE, color = "blue") +
          geom_point(data = exclude, shape = 2, fill = "red", color = "red", alpha = 0.75) +
          coord_cartesian(xlim = c(0, 50), ylim = c(500,20000))  +
          geom_text(aes(label = keep$city), family="Arial", size = 3)
      } else if(input$lm_x == "x1"){
        keep    <- cities[ vals$keeprows, , drop = FALSE]
        exclude <- cities[!vals$keeprows, , drop = FALSE]
        
        ggplot(keep, aes(planing, completed)) + geom_point() +
          geom_smooth(method = lm, fullrange = TRUE, color = "blue") +
          geom_point(data = exclude, shape = 2, fill = "red", color = "red", alpha = 0.75) +
          coord_cartesian(xlim = c(0, 50), ylim = c(0,60))  +
          geom_text(aes(label = keep$city), family="Arial", size = 3)
      }
    })

    output$lm_summary <- renderPrint({
      if (input$lm_x == "x2"){
        keep <- cities[ vals$keeprows, , drop = FALSE]
        fit <- with(keep, lm(planing ~ GDP))
      } else if (input$lm_x == "x1"){
        keep <- cities[ vals$keeprows, , drop = FALSE]
        fit <- with(keep, lm(planing ~ completed))
      }
      summary(fit)
    })
    output$lm_plot <- renderPlot({
      if (input$lm_x == "x2"){
        keep <- cities[ vals$keeprows, , drop = FALSE]
        fit <- with(keep, lm(planing ~ GDP))
      } else if (input$lm_x == "x1"){
        keep <- cities[ vals$keeprows, , drop = FALSE]
        fit <- with(keep, lm(planing ~ completed))
      }
      layout(matrix(1:4, ncol = 2))
      plot(fit)
    })

    # Toggle points that are clicked
    observeEvent(input$plot1_click, {
      res <- nearPoints(cities, input$plot1_click, allRows = TRUE)})
    # Toggle points that are brushed
    observeEvent(input$exclude_toggle, {
      res <- brushedPoints(cities, input$plot1_brush, allRows = TRUE)

      vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    # Reset all
    observeEvent(input$exclude_reset, {
      vals$keeprows <- rep(TRUE, nrow(cities))
    })
  
  
  mu <- 31.5
  std <- 0.8
  ci_seq <- seq(27.5, 35.5, length.out = 100)
  ## Confidence Interval 
  # ci_data <- reactive({
  #   data.frame(x = ci_seq,
  #              y = dnorm(ci_seq, mean = mu, sd=std),
  #              e = dnorm(ci_seq, mean=input$input$sample_mean, sd=std)
  #     )})

  output$ci_plot <- renderPlotly({
    mu <- 31.5
    std <- 0.8
    ci_seq <- seq(27.5, 35.5, length.out=100)
    ci_data <- data.frame(x = ci_seq, 
                          y = dnorm(ci_seq, mean = mu, sd=std))
    sigma <- std / sqrt(input$sample_size)
    two_sigma <- 2 * round(sigma, 2)
    ggplot(ci_data) +
      geom_line(aes(x=x, y=y)) +
      geom_vline(xintercept = mu, color="sky blue",linetype="dashed") +
      geom_ribbon(data = subset(ci_data, x > (31.5 - two_sigma) & x < (31.5 + two_sigma)),
                  aes(x=x, ymin=0, ymax=y),
                  fill = "sky blue", alpha = .3) +
      ## population mean
      annotate(geom="text", x=(mu - two_sigma), y= -0.01, label=paste0("[ ", (mu - two_sigma)),
               color="sky blue") + 
      annotate(geom="text", x=((mu + two_sigma) + (mu - two_sigma))/2, y= -0.01, label=paste0(" ---- ", mu, " ---- "),
               color="sky blue") + 
      annotate(geom="text", x=(mu + two_sigma), y= -0.01, label=paste0((mu + two_sigma), " ]"),
               color="sky blue") + 
      ## sample mean 1
      geom_segment(aes(x=input$sample_mean_1, y = -0.06, xend=input$sample_mean_1, yend=-0.02), color="red", linetype=2) + 
      annotate(geom="text", x=(input$sample_mean_1 - two_sigma), y= -0.04, label=paste0("[ ", (input$sample_mean_1 - two_sigma)),
               color="red") + 
      annotate(geom="text", x=((input$sample_mean_1 + two_sigma) + (input$sample_mean_1 - two_sigma))/2, y= -0.04, label=paste0(" ---- ", input$sample_mean_1, " ---- "),
               color="red") + 
      annotate(geom="text", x=(input$sample_mean_1 + two_sigma), y= -0.04, label=paste0((input$sample_mean_1 + two_sigma), " ]"),
               color="red") + 
      ## sample mean 2
      geom_segment(aes(x=input$sample_mean_2, y = -0.10, xend=input$sample_mean_2, yend=-0.06), color="chocolate1", linetype=2) + 
      annotate(geom="text", x=(input$sample_mean_2 - two_sigma), y= -0.08, label=paste0("[ ", (input$sample_mean_2 - two_sigma)),
               color="chocolate1") + 
      annotate(geom="text", x=((input$sample_mean_2 + two_sigma) + (input$sample_mean_2 - two_sigma))/2, y= -0.08, label=paste0(" ---- ", input$sample_mean_2, " ---- "),
               color="chocolate1") + 
      annotate(geom="text", x=(input$sample_mean_2 + two_sigma), y= -0.08, label=paste0((input$sample_mean_2 + two_sigma), " ]"),
               color="chocolate1") + 
      ## sample mean 3
      geom_segment(aes(x=input$sample_mean_3, y = -0.14, xend=input$sample_mean_3, yend=-0.10), color="darkgreen", linetype=2) + 
      annotate(geom="text", x=(input$sample_mean_3 - two_sigma), y= -0.12, label=paste0("[ ", (input$sample_mean_3 - two_sigma)),
               color="darkgreen") + 
      annotate(geom="text", x=((input$sample_mean_3 + two_sigma) + (input$sample_mean_3 - two_sigma))/2, y= -0.12, label=paste0(" ---- ", input$sample_mean_3, " ---- "),
               color="darkgreen") + 
      annotate(geom="text", x=(input$sample_mean_3 + two_sigma), y= -0.12, label=paste0((input$sample_mean_3 + two_sigma), " ]"),
               color="darkgreen") + 
      theme_bw()
  })
  
  output$ci_plot_2 <- renderPlotly({
    x <- rnorm(seq(-3, 3, length.out=input$sample_size_2), mean=0, sd=1)
    y <- rep(0.05, input$sample_size_2)
    sample_df <- data.frame(x = x, y = y)
    stand_norm_x <- seq(-3, 3, length.out=2000)
    x1 <- CI(x, ci=input$alpha)
    ci_data_2 <- data.frame(x = stand_norm_x, 
                          y = dnorm(stand_norm_x, mean = 0, sd=1))
    class(x1[1])
    dat<-with(density(x),data.frame(x,y))
    dat1<-dat[dat$x>x1[3]&dat$x<x1[1],]
    ggplot(ci_data_2)+
      geom_line(aes(x=x, y=y))+
      geom_vline(xintercept = 0, color = "sky blue") + 
      geom_vline(xintercept = x1[1],lty="dashed", color="red")+
      geom_vline(xintercept = x1[3],lty="dashed", color="red")+
      geom_vline(xintercept = x1[2],lty="dashed")+
      annotate(geom="text", x=-0.1, y= 0.1, label=paste0("population mean:"),
               color="sky blue") +
      annotate(geom="text", x=-0.1, y= 0.07, label=paste0("0"),
               color="sky blue") +
      annotate(geom="text", x=x1[3] - 1, y= 0, label=paste0("lower bound: "),
               color="red") +
      annotate(geom="text", x=x1[3] - 1, y= -0.03, label=paste0(round(x1[3], 2)),
               color="red") + 
      annotate(geom="text", x=x1[1] + 1, y= 0, label=paste0("upper bound: "),
               color="red") +
      annotate(geom="text", x=x1[1] + 1, y= -0.03, label=paste0(round(x1[1], 2)),
               color="red") + 
      geom_point(data = sample_df, 
                 mapping = aes(x = x, y = y), colour = "red", size = 1) + 
      theme_bw()
  })
  
  
})