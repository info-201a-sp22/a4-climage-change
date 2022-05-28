library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)
library("maps")
library("countrycode")
library("ggpolypath")
source("Summary.R")


  server <- function(input, output) {
    output$map <- renderPlot({
      iso_code <- input$iso3
      
      data <- Climate_proj %>% 
        filter(year == max(year)) %>% 
        filter(iso_code != "") %>% 
        filter(iso_code == input$iso3)  %>%
          group_by(country) %>% 
         summarise(co2 = mean(co2, na.rm = TRUE))
      
      
      county_shapefile <- shapefile %>% 
        mutate(iso_code = countrycode(region, origin = "country.name", destination = 'iso3c')) %>% 
        right_join(data, by= c("region" = "country")) 
      
      min_cO2 <- min(county_shapefile$co2)
      max_cO2 <- max(county_shapefile$co2)
      
      
      World_map <- ggplot(data = county_shapefile) +
        geom_polypath(mapping = aes(x = long, y = lat, fill = co2)
        ) +
        blank_theme +
        labs(title = paste("Annual production of CO2, measured in million tonnes per year in a country")) +
        scale_fill_gradient(limits = c(min_cO2,max_cO2)) +
        coord_quickmap()
      return(World_map)
      
    })
  }