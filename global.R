library(shiny)
library(leaflet)
library(dplyr)
library(coronavirus)

## Cargar datos
datacov2 <- coronavirus %>%
  select(country = Country.Region, type, cases, Lat, Long) %>%
  group_by(country, Lat, Long, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(-confirmed) %>%
  mutate(deadRate = death/confirmed * 100) %>%
  ungroup()

# Cambios de nombres a español
datacov2 <- datacov2 %>% 
  mutate(country = ifelse(country == "Peru", "Perú", country)) %>%
  mutate(country = ifelse(country == "Dominican Republic", "República Dominicana", country)) %>%
  mutate(country = ifelse(country == "Brazil", "Brasil", country)) %>%
  mutate(country = ifelse(country == "Haiti", "Haití", country)) %>%
  mutate(country = ifelse(country == "Panama", "Panamá", country)) %>%
  mutate(country = ifelse(country == "Mexico", "México", country))
