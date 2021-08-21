# GIF map
library(webshot)
library(gifski)
source("libraries.R")

latest_data <- get_latest_data()
historical_data <- get_history_data()

geoINFO = data.frame(
  country = latest_data$detail$country, 
  ISO3 = latest_data$detail$countryInfo$iso3,
  long = latest_data$detail$countryInfo$long,
  lat = latest_data$detail$countryInfo$lat,
  population = latest_data$detail$population
)

date_map <-seq(as.Date('2020-05-31'), as.Date('2020-06-03'),by = 1)

for(i in 1:length(date_map)){
    df = subset(historical_data$table ,date == date_map[i])
    df2 = merge(df, geoINFO, by='country')
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
      title = paste0('Covid-19 World Map: ', date_map[i]),
      geo = g
    )
    print(paste0("saving ", date_map[i]))
    htmlwidgets::saveWidget(fig, paste0("covid_map/html/", date_map[i], ".html"))
    webshot(paste0("covid_map/html/", date_map[i], ".html"), paste0("covid_map/png/", date_map[i], ".png"), vwidth=1920, vheight=1080, delay=10)
}

setwd("~/MES-shinyapp/resource/src/covid_map/png/")
png_files <- list.files(".", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "covid_animation.gif", width = 1920, height = 1080, delay = 0.1)
