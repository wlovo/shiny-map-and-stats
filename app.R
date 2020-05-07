library(ggplot2)
library(ggrepel)
library(ggspatial)
library(maps)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(shiny)
library(tidyverse)
library(tools)

var_to_title <- function(string) {
  toTitleCase(paste(strsplit(string, "_")[[1]], collapse = " "))
}

# Dataframes to display necessary information
data <- read_csv('./data/sector_2011_2018.csv') %>%
  replace_na(list(rescues = 0, deaths = 0))
locations <- read_csv('./data/locations.csv')
sector_info <-
  inner_join(locations, data, by = c("city" = "sector"))

# Geometric Data
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
states$ID <- toTitleCase(states$ID)
cities <-
  st_as_sf(
    sector_info,
    crs = 4326,
    agr = "constant",
    coords = c("lng", "lat"),
    remove = FALSE
  )

# Server code
server <- function(input, output) {
  output$stats_caption <- renderText({
    var_to_title(input$stats_var)
  })
  
  output$map.caption <- renderText({
    paste("Map:",
          var_to_title(input$map.var),
          "in",
          input$map.year)
  })
  
  output$plot <- renderPlot({
    ggplot(data) +
      aes_string(x = "fiscal_year",
                 y = input$stats_var,
                 fill = input$stats_var) +
      geom_bar(stat = "identity") +
      xlab("Fiscal Year") + ylab(var_to_title(input$stats_var)) +
      scale_fill_continuous(name = var_to_title(input$stats_var)) +
      facet_wrap( ~ sector, scales = "free_y")
  }, height = 850)
  
  output$map.plot <- renderPlot({
    filtered_cities <- cities %>% filter(fiscal_year == input$map.year)
    ggplot(data = world) +
      geom_sf(fill = "antiquewhite3") +
      geom_sf(data = states, fill = NA) +
      geom_sf(data = filtered_cities) +
      geom_point(
        data = filtered_cities,
        aes_string(
          x = "lng",
          y = "lat",
          size = input$map.var,
          color = input$map.var
        ),
        alpha = 0.9
      ) +
      geom_text_repel(
        data = filtered_cities,
        aes(x = lng, y = lat, label = city),
        fontface = "bold",
        nudge_x = c(1,-1.5, 2, 2,-1),
        nudge_y = c(0.25,-0.25, 0.5, 0.5,-0.5)
      ) +
      coord_sf(
        xlim = c(-126,-64),
        ylim = c(17, 51),
        expand = FALSE
      ) +
      scale_color_distiller(
        name = var_to_title(input$map.var),
        palette = "RdYlGn",
        direction = -1,
        aesthetics = c("colour")
      ) +
      scale_size_continuous(name = var_to_title(input$map.var),
                            range = c(3, 10)) +
      xlab("Longitude") + ylab("Latitude") +
      annotation_scale(location = "bl", width_hint = 0.4) +
      annotation_north_arrow(
        location = "bl",
        which_north = "true",
        pad_x = unit(0.75, "in"),
        pad_y = unit(0.5, "in"),
        style = north_arrow_fancy_orienteering
      ) +
      theme(
        panel.grid.major = element_line(
          color = gray(0.5),
          linetype = "dashed",
          size = 0.5
        ),
        panel.background = element_rect(fill = "aliceblue")
      )
  }, height = 800)
}

# UI code
ui <-
  fluidPage(tabsetPanel(
    tabPanel("Map",
             fluid = T,
             sidebarLayout(
               sidebarPanel(
                 selectInput("map.year",
                             "Year:",
                             sort(
                               unique(sector_info$fiscal_year), decreasing = T
                             )),
                 selectInput(
                   "map.var",
                   "Variable:",
                   c(
                     "Number of Agents" = "number_of_agents",
                     "Total Apprehensions" = "total_apprehensions",
                     "Accepted Prosecutions" = "accepted_prosecutions",
                     "Rescues" = "rescues",
                     "Deaths" = "deaths",
                     "Accompanied Juveniles" = "accompanied_juveniles",
                     "Unaccompanied Juveniles" = "unaccompanied_juveniles",
                     "Total Juveniles" = "total_juveniles",
                     "Total Adults" = "total_adults",
                     "Female Apprehensions" = "female_apprehensions",
                     "Male Apprehensions" = "male_apprehensions"
                   )
                 ),
                 width = 2
               ),
               #mainPanel(h3(textOutput("map_caption")))
               mainPanel(h3(textOutput("map.caption")), plotOutput("map.plot"))
             )),
    tabPanel(
      "Immigration Statistics by Sectors",
      fluid = T,
      sidebarLayout(
        sidebarPanel(selectInput(
          "stats_var",
          "Variable:",
          c(
            "Number of Agents" = "number_of_agents",
            "Total Apprehensions" = "total_apprehensions",
            "Accepted Prosecutions" = "accepted_prosecutions",
            "Rescues" = "rescues",
            "Deaths" = "deaths",
            "Accompanied Juveniles" = "accompanied_juveniles",
            "Unaccompanied Juveniles" = "unaccompanied_juveniles",
            "Total Juveniles" = "total_juveniles",
            "Total Adults" = "total_adults",
            "Female Apprehensions" = "female_apprehensions",
            "Male Apprehensions" = "male_apprehensions"
          )
        ),
        width = 2),
        mainPanel(h3(textOutput("stats_caption")),
                  plotOutput("plot"))
      )
    )
  ),
  title = "Immigration Statistics Application - William Lovo",
  theme = "bootstrap.css")

shinyApp(ui, server)