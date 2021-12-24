source("global.R")

# Define server logic for random distribution application
shinyServer(function(input, output, session){
    
    # Draw Distribution
    output$distribution <- renderUI({
        if(input$DistType == "con"){
            list(
                selectInput("dist", label="",
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
                selectInput("dist", label="",
                            list("Binomial Distribution" = "binom",
                                 "Geometric Distribution" = "geom",
                                 "Poisson Distribution" = "pois"),
                            selected = NULL
                )
            )
        }
    })
    
    output$parameter <- renderUI({
        if (input$dist == "norm"){
            list(
                numericInput("mean", "mean", 0),
                numericInput("std", "std",1)
            )
        } else if (input$dist == "t"){
            list(
                numericInput("df", "df", 1)
            )
        } else if (input$dist == "beta"){
            list(
                numericInput("shape1", "shape1", 0.5),
                numericInput("shape2", "shape2", 1)
            )
        } else if (input$dist == "gamma"){
            list(
                numericInput("shape", "shape", 0.2),
                numericInput("rate", "rate", 0.5)
            )
        } else if (input$dist == "chisq"){
            list(
                numericInput("df", "df", 1)
            )
        } else if (input$dist == "exp"){
            list(
                numericInput("rate", "rate", 1)
            )
        } else if (input$dist == "f"){
            list(
                numericInput("df1", "df1", 1),
                numericInput("df2", "df2", 1)
            )
        } else if (input$dist == "unif"){
            list(
                numericInput("min", "min", 0),
                numericInput("max", "max", 1)
            )
        } else if (input$dist == "binom"){
            list(
                numericInput("prob", "prob", 0.5),
                numericInput("size", "size", 10)
            )
        } else if (input$dist == "geom"){
            list(
                numericInput("prob", "prob", 0.5),
                numericInput("size", "size", 10)
            )
        } else if (input$dist == "pois"){
            list(
                numericInput("lambda", "lambda", 0.5),
                numericInput("size", "size", 10)
            )
        }
    })
    
    
    # Reactive expression to generate the requested distribution. This is 
    # called whenever the inputs change. The renderers defined 
    # below then all use the value computed from this expression
    data <- reactive({  
        
        set.seed(input$seed)
        
        # PDF
        if (input$pdf_cdf == "PDF"){
            if(input$dist == "norm"){
                x = seq(-10, 10, 0.01)
                data.frame(x=x, pdf=dnorm(x, input$mean, input$std))
            } else if (input$dist == "t"){
                x = seq(-10, 10, 0.01)
                data.frame(x=x, pdf=dt(x, input$df))
            } else if (input$dist == "beta"){
                x = seq(0, 1, 0.001)
                data.frame(x=x, pdf=dbeta(x, input$shape1, input$shape2))
            } else if (input$dist == "gamma"){
                x = seq(0, 10, 0.01)
                data.frame(x=x, pdf=dgamma(x, input$shape, input$rate))
            } else if (input$dist == "chisq"){
                x = seq(0, 10, 0.01)
                data.frame(x=x, pdf=dchisq(x, df=input$df))
            } else if (input$dist == "exp"){
                x = seq(0, 10, 0.01)
                data.frame(x=x, pdf=dexp(x, rate=input$rate))
            } else if (input$dist == "f"){
                x = seq(0, 10, 0.01)
                data.frame(x=x, pdf=df(x, input$df1, input$df2))
            } else if (input$dist == "unif"){
                x = seq(input$min, input$max, 0.01)
                data.frame(x=x, pdf=dunif(x, input$min, input$max))
            }
                else if (input$dist == "binom"){
                x = seq(0, input$size, 1)
                data.frame(x=x, pdf=dbinom(x, input$size, input$prob))
            } else if (input$dist == "geom"){
                x = seq(0, input$size, 1)
                data.frame(x=x, pdf=dgeom(x, input$prob))
            } else if (input$dist == "pois"){
                x = seq(0, input$size, 1)
                data.frame(x=x, pdf=dpois(x, input$lambda))
            }
            
        }
        # CDF
        else if (input$pdf_cdf == "CDF"){
            if(input$dist == "norm"){
                x = seq(-10, 10, 0.01)
                data.frame(x=x, pdf=pnorm(x, input$mean, input$std))
            } else if (input$dist == "t"){
                x = seq(-10, 10, 0.01)
                data.frame(x=x, pdf=pt(x, input$df))
            } else if (input$dist == "beta"){
                x = seq(0, 1, 0.001)
                data.frame(x=x, pdf=pbeta(x, input$shape1, input$shape2))
            } else if (input$dist == "gamma"){
                x = seq(0, 10, 0.01)
                data.frame(x=x, pdf=pgamma(x, input$shape, input$rate))
            } else if (input$dist == "chisq"){
                x = seq(0, 10, 0.01)
                data.frame(x=x, pdf=pchisq(x, df=input$df))
            } else if (input$dist == "exp"){
                x = seq(0, 10, 0.01)
                data.frame(x=x, pdf=pexp(x, rate=input$rate))
            } else if (input$dist == "f"){
                x = seq(0, 10, 0.01)
                data.frame(x=x, pdf=pf(x, input$df1, input$df2))
            } else if (input$dist == "unif"){
                x = seq(input$min, input$max, 0.01)
                data.frame(x=x, pdf=punif(x, input$min, input$max))
            }
            else if (input$dist == "binom"){
                x = seq(0, input$size, 1)
                data.frame(x=x, pdf=pbinom(x, input$size, input$prob))
            } else if (input$dist == "geom"){
                x = seq(0, input$size, 1)
                data.frame(x=x, pdf=pgeom(x, input$prob))
            } else if (input$dist == "pois"){
                x = seq(0, input$size, 1)
                data.frame(x=x, pdf=ppois(x, input$lambda))
            }
            
        }
    })
    
    # Generate a plot of the data
    output$plot <- renderPlotly(
        if (input$pdf_cdf == "PDF"){
            if (input$DistType == "con"){
                {g<-ggplot(data=data(), aes(x=x, y = pdf)) + geom_bar(stat = "identity", col = "steelblue", fill="steelblue") + 
                    scale_y_continuous(expand = c(0.01, 0)) + xlab("x") + ylab("Density") + xlim(input$plot_slider[1], input$plot_slider[2]) + theme_minimal()
                ggplotly(g)}
            } else if (input$DistType == "dis"){
                {g<-ggplot(data=data(), aes(x = x, y = pdf)) + geom_bar(stat = "identity", col = "steelblue", fill="steelblue") + 
                    scale_y_continuous(expand = c(0.01, 0)) + xlab("x") + ylab("Density") + xlim(input$plot_slider[1], input$plot_slider[2]) + theme_minimal()
                ggplotly(g)}
            }
        }
        
        else if(input$pdf_cdf == "CDF"){
            if (input$DistType == "con"){
                {g<-ggplot(data=data(), aes(x=x, y = pdf)) + geom_bar(stat = "identity", col = "steelblue", fill="steelblue") + 
                    scale_y_continuous(expand = c(0.01, 0)) + xlab("x") + ylab("Density") + xlim(input$plot_slider[1], input$plot_slider[2]) + theme_minimal()
                ggplotly(g)}
            } else if (input$DistType == "dis"){
                {g<-ggplot(data=data(), aes(x = x, y = pdf)) + geom_bar(stat = "identity", col = "steelblue", fill="steelblue") + 
                    scale_y_continuous(expand = c(0.01, 0)) + xlab("x") + ylab("Density") + xlim(input$plot_slider[1], input$plot_slider[2]) + theme_minimal()
                ggplotly(g)}
            }
            }
    )
    #hist(data(), 
    #     main=paste("random generation for the", dist, "distribution", sep=" "))
    
    #Generate a summary of the data
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
    
})