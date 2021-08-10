library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(rgeos)

# Define server logic for random distribution application
shinyServer(function(input, output){
  output$distribution <- renderUI({
    if(input$DistType == "con"){
      list(
        selectInput("dist", p(strong("Continuous Distributions")),
                    list("Normal" = "norm",
                         "Student's t" = "t",
                         "Beta" = "beta",
                         "Gamma" = "gamma",
                         "Cauchy" = "cauchy",
                         "Chi-Squared" = "chisq",
                         "Exponential" = "exp",
                         "F" = "f",
                         "Log-normal" = "lnorm",
                         "Uniform" = "unif",
                         "Weibull" = "weibull"),
                    selected = NULL
        )
      )
    }
    else if (input$DistType == "dis"){
      list(
        selectInput("dist", p(strong("Discrete Distributions")),
                    list("Geometric" = "geom",
                         "Hypergeometric" = "hyper",
                         "Binomial" = "binom",
                         "Negative Binomial" = "nbinom",
                         "Poisson" = "pois"),
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
    if(input$dist == "norm"){
      withMathJax(helpText("Probability Density Distribution: $p(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} \\exp\\left( -\\frac{1}{2}\\left(\\frac{x-\\mu}{\\sigma}\\right)^{\\!2}\\,\\right)$"))
    }
    else if(input$dist == "t"){
      withMathJax(helpText("Probability Density Distribution: $t = \\frac{x - \\mu}{s / \\sqrt{n}}$$ $$\\text{where } s^2 = \\frac{1}{n} \\sum_{i=1}^{n} (x_i - \\overline{x})$"))
    }
    else if(input$dist == "beta"){
      withMathJax(helpText("Probability Density Distribution: $p(x) = \\frac{x^{\\alpha - 1}(1 - x)^{\\beta - 1}}{B(\\alpha, \\beta)}, \\text{where } B(\\alpha, \\beta) = \\frac{\\Gamma(\\alpha)\\Gamma(\\beta)}{\\Gamma(\\alpha + \\beta)}, \\text{and } \\Gamma \\text{ is the Gamma function$"))
    }
    else if(input$dist == "gamma"){
      withMathJax(helpText("Probability Density Distribution: $p(x) = \\frac{x^{\\alpha-1} \\lambda^{\\alpha} e^{-\\lambda x}}{\\Gamma(\\alpha)} = \\frac{x^{\\alpha -1} e^{- \\frac{1}{\\beta}x}}{\\beta^{\\alpha}\\Gamma(\\alpha)}, x >0$"))
    }
    else if(input$dist == "cauchy"){
      withMathJax(helpText("Probability Density Distribution: $p(x) = \\frac{1}{\\pi \\gamma [1 + (\\frac{x - x_0}{\\gamma})^2]$"))
    }
    else if(input$dist == "chiq"){
      withMathJax(helpText("Probability Density Distribution: $p(x) = \\frac{1}{\\Gamma (\\frac{k}{2})} \\gamma(\\frac{k}{2}$"))
    }
    else if(input$dist == "exp"){
      withMathJax(helpText("Probability Density Distribution: $p(x) = \\lambda e^{- \\lambda x}$"))
    }
    else if(input$dist == "f"){
      withMathJax(helpText)("Probability Density Distribution: $p(x) = \\frac{\\sqrt{\\frac{(d_1 x)^{d_1} d_2^{d_2}}{(d_1 x + d_2)^{d_1 + d_2}}}}{x B(\\frac{d_1}{2}, \\frac{d_2}{2})}$")
    }
    else if(input$dist == "lnorm"){
      withMathJax(helpText("Probability Density Distribution: $p(x) = \\frac{1}{x \\sigma \\sqrt{2 \\pi}} exp(- \\frac{[ln(x) - \\mu]^2}{2 \\sigma^2})$"))
    }
    else if(input$dist == "unif"){
      withMathJax(helpText("Probability Density Distribution: $p(x) = \\frac{1}{b-a}$"))
    }
    else if(input$dist == "weibull"){
      withMathJax("Probability Density Distribution: $$p(x) = \\frac{k}{\\lambda} (\\frac{x}{\\lambda})^{k-1} e^{-(x/\\lambda)^{k}}, x \\geq 0$$  $$p(x) = 0,  x < 0$$")
    }
  })
  
  ## COVID MAP
  observeEvent(input$map_shape_click, {
    p <- input$map_shape_click 
    cnty <- p$id
    type <- input$type
    startdate <- input$startdate
    enddate <- input$enddate
    if(type == "Confirmed"){
      plot_data <- conf_ts%>% 
        filter(Country_Region == cnty, Cases > 0) %>% 
        mutate(Daily_Cases = Cases - lag(Cases, default = Cases[1]))
      countryname <- cnty
      plot_data_us <- conf_ts %>%
        filter(Country_Region == "United States")%>% 
        mutate(Daily_Cases = Cases - lag(Cases, default = Cases[1]))
    }else if(type == "Deaths"){
      plot_data <- deaths_ts%>% 
        filter(Country_Region == cnty, Cases > 0) %>% 
        mutate(Daily_Cases = Cases - lag(Cases, default = Cases[1]))
      countryname <<- cnty
      plot_data_us <- deaths_ts %>%
        filter(Country_Region == "United States")%>% 
        mutate(Daily_Cases = Cases - lag(Cases, default = Cases[1]))
    }else if(type == "Recovered"){
      plot_data <- recovered_ts%>% 
        filter(Country_Region == cnty, Cases > 0) %>% 
        mutate(Daily_Cases = Cases - lag(Cases, default = Cases[1]))
      countryname <- cnty
      plot_data_us <- recovered_ts %>%
        filter(Country_Region == "United States")%>% 
        mutate(Daily_Cases = Cases - lag(Cases, default = Cases[1]))
    }
    
    plot_data <- subset(plot_data, Date >= startdate & Date <= enddate)
    plot_data_us <- subset(plot_data_us, Date >= startdate & Date <= enddate)
    output$country <- renderPlotly({
      fig <- plot_ly(plot_data, x = ~Date, y = ~Daily_Cases, type = 'scatter', mode = 'lines+markers') %>% 
        layout(xaxis = list(title = cnty, tickformat = "%d/%m/%y"),
               yaxis = list(title = "", tickformat = "s"),
               margin = list(l=0, r=0)) %>% 
        config(displayModeBar = FALSE, displaylogo = FALSE)
      fig
    })
    output$fig_usa <- renderPlotly({
      fig_usa <- plot_ly(plot_data_us, x = ~Date, y = ~Daily_Cases, type = 'scatter', mode = 'lines+markers') %>% 
        layout(xaxis = list(title = "USA", tickformat = "%d/%m/%y"),
               yaxis = list(title = "", tickformat = "s"),
               margin = list(l=0, r=0)) %>% 
        config(displayModeBar = FALSE, displaylogo = FALSE)
      fig_usa
    })
  }) 
  
  output$map <- renderLeaflet({
    col_name <- as.name(input$type)
    map_data <- world_today[[col_name]] - world_yesterday[[col_name]]
    pal <- colorBin("YlOrRd", map_data, right=FALSE)
    leaflet(world_today) %>% 
      addTiles() %>% 
      addPolygons(stroke = TRUE, weight=1, smoothFactor = 0.8, fillOpacity = 1,
                  fillColor = ~pal(map_data),
                  label = ~paste0(name, ": ", formatC(map_data, big.mark = ",")),
                  layerId = ~name)  %>%
      addLegend(position = "bottomright", pal = pal, values = map_data,
                title = col_name,
                opacity = 1) %>% 
      setView(lng = 30, lat = 30, zoom = 03)
  })
})