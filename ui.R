library("dplyr")
library("purrr")
library("maps")
source("Summary.R")

Climate <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
country_df <- Climate %>%
  distinct(iso_code, country)

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
World_2019 <- Climate_proj %>%
  filter(year == max(year)) %>%
  filter(country == "World") %>%
  summarise(year, co2)

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
  filter(co2 == max(co2, na.rm = TRUE)) %>%
  summarise(year, co2)


Asia_contribution <- Climate_proj %>%
  filter(year == max(year)) %>%
  filter(country == "World") %>%
  left_join(Asia, by = "year") %>%
  left_join(World_2019, by = "year") %>%
  mutate(Asia_percent = co2.y / co2) %>%
  pull(Asia_percent)

country_filter <- country_df %>%
  filter(country %in% c("Afghanistan", "China", "Eritrea", "Peru", "Spain", "Australia"))

choices <- setNames(country_filter$iso_code, country_filter$country)

map_layout <- sidebarLayout(
  sidebarPanel(selectInput(
    inputId = "iso3",
    label = "Choose a country",
    choices = choices 
  )
  
  ),
  mainPanel(plotOutput("map"))
)

map_panel <- tabPanel("Map of World", map_layout,
                      "I decided to include this map as my main interactive variable, because of the amount of great information that is shown all around the world. This map generates a sample

  for you to search for any of the select countryies in the search bar, and on the side bar you can see the mean CO2 emmision per ton
This can help us determine where the high number are around the world, and maybe have those nations take accountability and lower their emmissions. We can
see from the map that if you search for the more larger nations on the map the country is a larger mean. This is easy to see if you search for the China. If you search up Peru, You can see that the mean is much lower")

Introduction_panel <- tabPanel("Introduction", p("Climate Change is one of the largest issues facing the earth today. Human emissions and activity have 
contributed to this threat on our planet. Fossil fuels, coal, oil, gas are all resources we rely on that damage our planet. The data that we have here show 
the level of carbon dioxide produced by different countries and continents around the world. This data also shows how much the levels of carbon dioxide have increased year 
to year. Looking at the data presented can show which areas of the world are contributing most to global warming. It can help us understand where the majority of the issue exists.
This data has been collected, aggregated, and documented by Hannah Ritchie, Max Roser, Edouard Mathieu, Bobbie Macdonald and Pablo Rosado from the our world mission group.
To add, the data was collected by reputable sources such as the UNWPP, the United world population prospects. The motivation behind the groups research was CO2 emissions, 
other greenhouse gas emissions, energy mix, as well as other variables of potential interest. Limitations may include of lack of specific data, as well as leaving specific variables out
of the question.

                                                 
For this project  I will be focusing mainly on the Annual production of carbon dioxide (CO2) for nations around the world. 
We can see how much the Green house gases, contributed to Co2 levels in specific countries, big and small, and how the mean greenhouse emissions numbers relate to one another.
))





ui <- navbarPage(
  "Co2 Levels",
  Introduction_panel,
  map_panel
)
