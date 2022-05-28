
library("dplyr")
library("tidyverse")
library("stringr")
library("maps")
library("mapproj")
library("bslib")

Climate <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
Climate_proj <- Climate %>% select(year, country,co2, iso_code)


#5 Functions
# What amount of co2 did the United States produce past year?
America <- Climate_proj %>%
  filter(year == max(year)) %>%
  filter(country == "United States") %>%
  summarise(year, co2)

#How much Co2 did the world produce in 2019?
World_2019 <- Climate_proj %>%
  filter(year == max(year)) %>%
  filter(country == "World") %>%
  summarise(year, co2)
# What percent did the Us contribute to climate change last year?

US_Contribution <- Climate_proj %>%
  filter(year == max(year)) %>%
  filter(country == "World") %>%
  left_join(America, by = "year") %>%
  left_join(World_2019, by = "year") %>%
  mutate(US_percent = co2.y / co2) %>%
  pull(US_percent)


# Which area produced the most Co2 levels in 2019?
Co2_2019 <- Climate_proj %>%
  filter(year == "2019") %>%
  filter(country != "World") %>%
  filter(co2 == max(co2, na.rm = TRUE)) %>%
  pull(country)

#Percent Asia contributed to co2 last year
Asia <- Climate_proj %>%
  filter(year == "2019") %>%
  filter(country != "World") %>%
  filter(co2 == max(co2, na.rm = T)) %>%
  summarise(year, co2)


Asia_contribution <- Climate_proj %>%
  filter(year == max(year)) %>%
  filter(country == "World") %>%
  left_join(Asia, by = "year") %>%
  left_join(World_2019, by = "year") %>%
  mutate(Asia_percent = co2.y / co2) %>%
  pull(Asia_percent)

shapefile <- map_data("world")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

World_map <- ggplot(data = shapefile) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = co2)
  ) +
  blank_theme +
  labs(title = paste("Annual production of CO2, measured in million tonnes per year in a country"))

