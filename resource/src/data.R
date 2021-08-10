library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)
library(RCurl)
library(leaflet)

# Country uid
uid_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv")
uid_lookup <- read.csv(text = uid_url)

# Today 
x <- Sys.Date() - 1
x <- format(x,"%m-%d-%Y")
url_str <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", x, ".csv", sep = "")
jh_url <- getURL(url_str)
jh <- read.csv(text = jh_url)

jh_today <- jh %>%
  #  filter(Active >= 0) %>%
  group_by(Country_Region) %>%
  summarise(Lat = first(Lat), Long_ = first(Long_),
            Confirmed = sum(Confirmed), Deaths = sum(Deaths), 
            Recovered = sum(Recovered), Active = sum(Active),
            Incidence_Rate = mean(Incident_Rate, na.rm = TRUE),
            Case.Fatality_Ratio = mean(Case_Fatality_Ratio, na.rm = TRUE),
            .groups = "keep")

world_today <- ne_countries(scale = 110, returnclass = "sf")
for(i in seq_along(jh_today$Country_Region)){
  jh_today$iso3[i] <- head(uid_lookup[uid_lookup$Country_Region == jh_today$Country_Region[i],]$iso3, n = 1)
}
world_today <- left_join(world_today, jh_today, by=c("iso_a3" = "iso3"))

world <- world_today

# Yesterday
x_y <- Sys.Date() - 2
x_y <- format(x_y,"%m-%d-%Y")
url_str_y <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", x_y, ".csv", sep = "")
jh_url_y <- getURL(url_str_y)
jh_y <- read.csv(text = jh_url_y)

jh_yesterday <- jh_y %>%
  #  filter(Active >= 0) %>%
  group_by(Country_Region) %>%
  summarise(Lat = first(Lat), Long_ = first(Long_),
            Confirmed = sum(Confirmed), Deaths = sum(Deaths), 
            Recovered = sum(Recovered), Active = sum(Active),
            Incidence_Rate = mean(Incident_Rate, na.rm = TRUE),
            Case.Fatality_Ratio = mean(Case_Fatality_Ratio, na.rm = TRUE),
            .groups = "keep")

world_yesterday <- ne_countries(scale = 110, returnclass = "sf")
for(i in seq_along(jh_yesterday$Country_Region)){
  jh_yesterday$iso3[i] <- head(uid_lookup[uid_lookup$Country_Region == jh_yesterday$Country_Region[i],]$iso3, n = 1)
}
world_yesterday <- left_join(world_yesterday, jh_yesterday, by=c("iso_a3" = "iso3"))


# Time series
confirmed_ts_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
conf_ts <- read.csv(text = confirmed_ts_url)
deaths_ts_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
deaths_ts <- read.csv(text = deaths_ts_url)
recovered_ts_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recovered_ts <- read.csv(text = recovered_ts_url)
# Time series
uid_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv")
uid_lookup <- read.csv(text = uid_url)
confirmed_ts_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
conf_ts <- read.csv(text = confirmed_ts_url)
deaths_ts_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
deaths_ts <- read.csv(text = deaths_ts_url)
recovered_ts_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recovered_ts <- read.csv(text = recovered_ts_url)

# Confirmed time series
conf_ts <- conf_ts %>% 
  gather(5:ncol(conf_ts), key = "Date", value = "Cases")
conf_ts <- rename(conf_ts, Country_Region = starts_with("Country"))
conf_ts$Date <- str_replace(conf_ts$Date, "X","")
conf_ts$Date <- parse_date_time(conf_ts$Date, orders = "mdy")
conf_ts <- conf_ts %>% 
  group_by(Country_Region, Date, Lat, Long) %>% 
  summarise(Cases = sum(Cases))
conf_ts[conf_ts$Country_Region == "US",]$Country_Region <- "United States"
conf_ts[conf_ts$Country_Region == "Congo (Kinshasa)",]$Country_Region <- "Dem. Rep. Congo"
conf_ts[conf_ts$Country_Region == "Congo (Brazzaville)",]$Country_Region <- "Congo"
conf_ts[conf_ts$Country_Region == "Korea, South",]$Country_Region <- "Korea"

#Deaths time series
deaths_ts <- deaths_ts %>% 
  gather(5:ncol(deaths_ts), key = "Date", value = "Cases")
deaths_ts <- rename(deaths_ts, Country_Region = starts_with("Country"))
deaths_ts$Date <- str_replace(deaths_ts$Date, "X","")
deaths_ts$Date <- parse_date_time(deaths_ts$Date, orders = "mdy")
deaths_ts <- deaths_ts %>% 
  group_by(Country_Region, Date, Lat, Long) %>% 
  summarise(Cases = sum(Cases))
deaths_ts[deaths_ts$Country_Region == "US",]$Country_Region <- "United States"
conf_ts[conf_ts$Country_Region == "Congo (Kinshasa)",]$Country_Region <- "Dem. Rep. Congo"
conf_ts[conf_ts$Country_Region == "Congo (Brazzaville)",]$Country_Region <- "Congo"
conf_ts[conf_ts$Country_Region == "Korea, South",]$Country_Region <- "Korea"

#Recovered time series
recovered_ts <- recovered_ts %>% 
  gather(5:ncol(recovered_ts), key = "Date", value = "Cases")
recovered_ts <- rename(recovered_ts, Country_Region = starts_with("Country"))
recovered_ts$Date <- str_replace(recovered_ts$Date, "X","")
recovered_ts$Date <- parse_date_time(recovered_ts$Date, orders = "mdy")
recovered_ts <- recovered_ts %>% 
  group_by(Country_Region, Date, Lat, Long) %>% 
  summarise(Cases = sum(Cases))

# Replace name
deaths_ts[deaths_ts$Country_Region == "US",]$Country_Region <- "United States"
conf_ts[conf_ts$Country_Region == "Congo (Kinshasa)",]$Country_Region <- "Dem. Rep. Congo"
conf_ts[conf_ts$Country_Region == "Congo (Brazzaville)",]$Country_Region <- "Congo"
conf_ts[conf_ts$Country_Region == "Korea, South",]$Country_Region <- "Korea"
# 
# # Combine conf_ts, deaths_ts, recovered_ts 
# conf_ts <- conf_ts %>% rename(Confirmed = Cases)
# deaths_ts <- deaths_ts %>% rename (Deaths = Cases)
# recovered_ts <- recovered_ts %>% rename(Recovered = Cases)
# 
# total <- conf_ts %>% 
#   full_join(deaths_ts, by=c("Country_Region", "Date", "Lat", "Long"))  %>% 
#     full_join(recovered_ts, by=c("Country_Region", "Date", "Lat", "Long")) %>%
#       as.data.frame()
# 
# world <- within(world, rm(Confirmed, Deaths, Recovered))
# world <- world %>% rename(Long = Long_)
# world <- world %>% full_join(total, by=c("Country_Region", "Lat", "Long")) %>% group_by (Country_region)
# 
# gc()
# ## leaflet map
# col_name <- "Confirmed"
# choicen_date <- today(tzone = "UTC") - 2
# map_data <- world %>% filter(Date == choicen_date) %>%
#   group_by(Country_Region) %>%
#   
# pal <- colorBin("YlOrRd", map_data[[col_name]], bins = 9, right=FALSE)
# leaflet(map_data) %>%
#   addPolygons(data = map_data,
#               stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,
#               fillColor = ~pal(map_data[[col_name]]),
#               popup = paste("Region: ", map_data$Country_Region, "<br>",
#                             "Value: ", map_data[[col_name]], "<br>")) %>%
#   addLegend(position = "bottomright", pal = pal, values = map_data[[col_name]],
#             title = col_name,
#             opacity = 1)%>% 
#   setView(lng = 30, lat = 30, zoom = 03)