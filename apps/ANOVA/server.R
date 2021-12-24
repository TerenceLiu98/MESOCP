source("global.R")

# ANOVA Server
shinyServer(function(input, output, session){
    output$random_parameter <- renderUI({
        validate(
            need(input$dataset, "Please select a dataset")
        )
        if (input$dataset == "fuel"){
            column(width = 12,
                   textInput("fuel_group_a", label = h5("Brand A (Group 1)"), value = "7.80, 8.20, 8.65, 8.00, 8.36"),
                   textInput("fuel_group_b", label = h5("Brand B (Group 2)"), value = "9.50, 10.21, 9.85, 10.02, 9.39"),
                   textInput("fuel_group_c", label = h5("Brand C (Group 3)"), value = "8.20, 8.87, 8.35, 9.03, 8.68"))
        }
        else if (input$dataset == "temperature"){
            column(width = 12,
                   textInput("temp_group_a", label = ("80°C (Group 1)"), value = "254, 263, 241, 237, 251"),
                   textInput("temp_group_b", label = ("85°C (Group 2)"), value = "234, 218, 235, 227, 216"),
                   textInput("temp_group_c", label = ("90°C (Group 3)"), value = "200, 222, 197, 206, 204"))
        }
        else if(input$dataset == "random"){
            fluidRow(
                column(width = 4,
                       div(style = "font-size:15px;",numericInput("anova_size1", label=("size of group 1"), value=5, min=5, max=100, step = 1)),
                       div(style = "font-size:15px;",numericInput("anova_size2", label=("size of group 2"), value=5, min=5, max=100, step = 1)),
                       div(style = "font-size:15px;",numericInput("anova_size3", label=("size of group 3"), value=5, min=5, max=100, step = 1))),
                column(width = 4, 
                       div(style = "font-size:15px;",numericInput("anova_mu1", label=("mean of group 1"), value=1, min=0, max=10, step = 1)),
                       div(style = "font-size:15px;",numericInput("anova_mu2", label=("mean of group 2"), value=2, min=0, max=10, step = 1)),
                       div(style = "font-size:15px;",numericInput("anova_mu3", label=("mean of group 3"), value=3, min=0, max=10, step = 1))),
                hr(),
                column(width = 4,
                       div(style = "font-size:15px;",numericInput("anova_sigma", label=("total sigma"), value=1, min=-10, max=10, step=0.01))))
        }
        })
    
    rnorm1 = rnorm
    rnorm2 = rnorm
    rnorm3 = rnorm #repeatable(rnorm)
    
    gen_data = reactive({
        if (input$dataset == "fuel"){
            validate(
                need(input$fuel_group_a, "Please select a size of fuel_group_a"),
                need(input$fuel_group_b, "Please select a size of fuel_group_b"),
                need(input$fuel_group_c, "Please select a size of fuel_group_c")
            )} else if (input$dataset == "temperature"){
                validate(
                    need(input$fuel_group_a, "Please select a size of fuel_group_a"),
                    need(input$fuel_group_b, "Please select a size of fuel_group_b"),
                    need(input$fuel_group_c, "Please select a size of fuel_group_c")
                )
            }
        if (input$dataset == "fuel"){
            group_a <- as.vector(as.numeric(unlist(strsplit(input$fuel_group_a, ","))))
            group_b <- as.vector(as.numeric(unlist(strsplit(input$fuel_group_b, ","))))
            group_c <- as.vector(as.numeric(unlist(strsplit(input$fuel_group_c, ","))))
            y <- c(group_a, group_b, group_c)
            group <- c(rep(sprintf("class A"), each = length(group_a)),
                       rep(sprintf("class B"), each = length(group_b)),
                       rep(sprintf("class C"), each = length(group_c)))
            data.frame(datapoint = y, group = group)
        }
        
        else if (input$dataset == "temperature"){
            group_a <- as.vector(as.numeric(unlist(strsplit(input$temp_group_a, ","))))
            group_b <- as.vector(as.numeric(unlist(strsplit(input$temp_group_b, ","))))
            group_c <- as.vector(as.numeric(unlist(strsplit(input$temp_group_c, ","))))
            y <- c(group_a, group_b, group_c)
            group <- c(rep(sprintf("80°C"), each = length(group_a)),
                       rep(sprintf("85°C"), each = length(group_b)),
                       rep(sprintf("90°C"), each = length(group_c)))
            data.frame(datapoint = y, group = group)
        }
        
        else if (input$dataset == "random"){
            validate(
                need(input$anova_size1, "Please select a size of anova group 1"),
                need(input$anova_size2, "Please select a size of anova group 2"),
                need(input$anova_size3, "Please select a size of anova group 3"),
                need(input$anova_mu1, "Please select a mean of anova group 1"),
                need(input$anova_mu2, "Please select a mean of anova group 2"),
                need(input$anova_mu3, "Please select a mean of anova group 3"),
                need(input$anova_sigma, "Please select a sigma of anova")
            )
            anova_size1 = input$anova_size1; anova_size2 = input$anova_size2; anova_size3 = input$anova_size3;
            anova_mu1 = input$anova_mu1; anova_mu2 = input$anova_mu2; anova_mu3 = input$anova_mu3; anova_sigma = input$anova_sigma;
            anova_data1 = rnorm1(anova_size1, anova_mu1, anova_sigma)
            anova_data2 = rnorm2(anova_size2, anova_mu2, anova_sigma)
            anova_data3 = rnorm3(anova_size3, anova_mu3, anova_sigma)
            y = c(anova_data1, anova_data2, anova_data3)
            group = c(rep(sprintf("group %s", 1), each = anova_size1),
                      rep(sprintf("group %s", 2), each = anova_size2),
                      rep(sprintf("group %s", 3), each = anova_size3))
            data.frame(datapoint = y, group = group)}
    })

    
    output$aovPlot = renderPlot({
        if (input$aovplottype == "scatter"){
            means <- aggregate(datapoint ~ group, gen_data(), mean)
            ggplot(gen_data(), aes(y = group, x = datapoint, fill = group)) + 
                geom_point(alpha=0.6, size = 2.5, aes(color = factor(group))) + 
                stat_summary(fun = mean, colour = "brown2", geom="point", shape=17, size = 3.0, show.legend=FALSE) +
                geom_text(data = means, aes(label = sprintf("mean: %0.3f", datapoint), x = datapoint + 0.08), fontface='bold') + 
                theme(legend.position="none", 
                      plot.title = element_text(size=12, hjust = 1.0), 
                      legend.text = element_text(size=30),
                      axis.text.x = element_text(size=12, hjust=1.0),
                      axis.text.y = element_text(size=12, hjust=1.0)) + coord_flip()}
        
        else if (input$aovplottype == "boxplot"){
            means <- aggregate(datapoint ~  group, gen_data(), mean)
            ggplot(gen_data(), aes(y=group, x=datapoint, fill=group)) + 
                geom_boxplot(alpha=0.6, width=0.3) +
                stat_summary(fun=mean, colour="brown2", geom="point", shape=17, size=3.0, show.legend=FALSE) + 
                geom_text(data = means, aes(label = sprintf("mean: %0.3f", datapoint), x = datapoint + 0.08), fontface='bold') + 
                theme(legend.position="none", 
                      plot.title = element_text(size=12, hjust = 0.5), 
                      legend.text = element_text(size=30),
                      axis.text.x = element_text(size=12, hjust=0.5),
                      axis.text.y = element_text(size=12, hjust=0.5)) + coord_flip() + 
                scale_fill_brewer(palette="Accent")} 
        
        else if (input$aovplottype == "histogram"){
            ggplot(gen_data(), aes(x = datapoint, color = group)) + 
                geom_histogram(binwidth = 0.05, fill = "white") + 
                facet_grid( ~ group) + 
                # ggtitle("Histogram of three groups") + 
                scale_fill_brewer(palette = "Accent")}
    })
    
    output$aovdataSummary = renderDataTable({
        if (input$dataset == "fuel"){
            validate(
                need(input$fuel_group_a, "Please select a size of fuel_group_a"),
                need(input$fuel_group_b, "Please select a size of fuel_group_b"),
                need(input$fuel_group_c, "Please select a size of fuel_group_c")
            )} else if (input$dataset == "temperature"){
                validate(
                    need(input$fuel_group_a, "Please select a size of fuel_group_a"),
                    need(input$fuel_group_b, "Please select a size of fuel_group_b"),
                    need(input$fuel_group_c, "Please select a size of fuel_group_c")
                )
            }
        options(DT.options = list(pageLength = 5, scrollX=T, dom = 't'))
        anova_summary <- gen_data() %>% group_by(group) %>%
            dplyr::summarise(count = n(), 
                             mean = round(mean(datapoint, na.rm = TRUE), 3), 
                             sd = round(sd(datapoint, na.rm = TRUE), 3))
        return(datatable(anova_summary) %>% formatStyle(columns = c(1,2,3,4), fontWeight="bold", fontSize="120%"))
        })
    
    output$aovSummary = renderDataTable({
        if (input$dataset == "fuel"){
            validate(
                need(input$fuel_group_a, "Please select a size of fuel_group_a"),
                need(input$fuel_group_b, "Please select a size of fuel_group_b"),
                need(input$fuel_group_c, "Please select a size of fuel_group_c")
            )} else if (input$dataset == "temperature"){
                validate(
                    need(input$fuel_group_a, "Please select a size of fuel_group_a"),
                    need(input$fuel_group_b, "Please select a size of fuel_group_b"),
                    need(input$fuel_group_c, "Please select a size of fuel_group_c")
                )
            }
        options(DT.options = list(pageLength = 5, scrollX=T, dom = 't'))
        anova_data <- round(as.data.frame(unclass(summary(aov(datapoint ~ group, data = gen_data()))), check.names = FALSE, stringsAsFactors = FALSE),3) 
        return(datatable(anova_data) %>% formatStyle(columns = c(1,2,3,4,5), fontWeight="bold", fontSize="120%"))
    })
    
})