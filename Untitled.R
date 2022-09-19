library(tidyverse)
library(sf)
library(furrr)
library(lubridate)
library(rnaturalearth)
library(dplyr)
library(geojsonsf)
library(shinydashboard)
library(dashboardthemes)
library(shiny)
library(shinycustomloader)

# Define UI for application that draws a histogram
library(leaflet)
library(formattable)
library(cranlogs)
ui <- dashboardPage(
  dashboardHeader(title = "Real-Time Hotspot"),
  dashboardSidebar(sidebarMenu(
    menuItem("Real-Time Map",
             tabName = "Map1", icon = icon("list")),
    menuItem("Last month",
             tabName = "Description", icon = icon("list"))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "realtime",
            #####dashboard theme######
            shinyDashboardThemes(theme = "blue_gradient"),
            ####Title####
            h2("Hotspot Map: Real Time ", 
               style = 	"color:#4682B4"), 
            fluidRow(
              #####map output with loader###
              box(leafletOutput("map1",width = "100%")))
    ),
    tabItem(tabName = "Description",
            fluidRow(tabBox(title = "Total downloads",
                            tabPanel("Total", plotOutput("last_month_barplot"))),
                     tabBox(title = "Top downloads",
                            tabPanel("Top", formattableOutput("last_month_top_table")))))
  ))
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  au_map <-ne_states(country = c("australia"), returnclass ="sf")%>%
    select(state = name, geometry)
  vic_map <- au_map%>%
    filter(state == "Victoria")
  
  t <- reactive({
    invalidateLater(600000)  #1000毫秒之后重新执行
    recent_hotspot_data <- geojsonsf::geojson_sf("https://hotspots.dea.ga.gov.au/data/recent-hotspots.json")
    vic_hotspot1 <- recent_hotspot_data %>%
      st_set_crs("WGS 84")
    vic_map <- vic_map %>%st_set_crs("WGS 84")
    recent_hotspot_data <- recent_hotspot_data %>%st_set_crs("WGS 84")
    vic_hotspot2 = st_intersects(vic_map$geometry, recent_hotspot_data$geometry)
    vic_hotspot2 = vic_hotspot1[vic_hotspot2[[1]],]
    hotspot_show <- vic_hotspot2%>%
      filter(confidence > 50)
    hotspot_show$hours_since_hotspot_class <- cut(hotspot_show$hours_since_hotspot,
                                                  breaks = c(0,2,6,24,48,72),
                                                  labels =c("0-2","2-6","6-24","24-48","48-72"))
    pals = colorFactor(palette =c("#800000","#FF0000","#FF4500","#FF8C00","#F5DEB3"),
                       levels = c("0-2","2-6","6-24","24-48","48-72"))
    hotspot_show
  })
  
  basemap <- leaflet() %>%
    # add different provider tiles
    addProviderTiles(
      "OpenStreetMap",
      # give the layer a name
      group = "OpenStreetMap"
    ) %>%
    addProviderTiles(
      "Stamen.Toner",
      group = "Stamen.Toner"
    ) %>%
    addProviderTiles(
      "Stamen.Terrain",
      group = "Stamen.Terrain"
    ) %>%
    addProviderTiles(
      "Esri.WorldStreetMap",
      group = "Esri.WorldStreetMap"
    ) %>%
    addProviderTiles(
      "Wikimedia",
      group = "Wikimedia"
    ) %>%
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron"
    ) %>%
    addProviderTiles(
      "Esri.WorldImagery",
      group = "Esri.WorldImagery"
    ) %>%
    # add a layers control
    addLayersControl(
      baseGroups = c(
        "OpenStreetMap", "Stamen.Toner",
        "Stamen.Terrain", "Esri.WorldStreetMap",
        "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
      ),
      # position it on the topleft
      position = "topleft"
    )
  output$map1 <- reactive({
    map_1 <- basemap %>%
      addCircles(
        data = t(),
        color = ~pals(hours_since_hotspot_class),
        # create custom labels
        label = paste(
          "Time: ", hotspot_show$load_dt, "<br>",
          "Hours: ",hotspot_show$hours_since_hotspot,  "<br>",
          "Satellite: ", hotspot_show$satellite_operating_agency
        ) %>%
          lapply(htmltools::HTML)
      ) %>%
      # add a legend
      addLegend(
        colors = c("#800000","#FF0000","#FF4500","#FF8C00","#F5DEB3"),
        labels = c("0 - 2 hours","2 - 6 hours","6 - 24 hours","24 - 48 hours","48 - 72 hours"),
        title = "Hours",
        opacity = 1, 
        position = "bottomleft"
      )
    map_1
    
  })
  output$last_week_top_table <- renderFormattable({
    data <- cran_top_downloads("last-week")
    formattable(data, list(count = color_bar("lightblue"),
                           package = formatter("span",
                                               style = "font-family: monospace;")))
  })
  output$last_month_barplot <- renderPlot({
    data <- subset(cran_downloads(when = "last-month"),
                   count > 0)
    with(data, barplot(count, names.arg = date),
         main = "Last month downloads")
  })
  output$last_month_top_table <- renderFormattable({
    data <- cran_top_downloads("last-month")
    formattable(data, list(count = color_bar("lightblue"),
                           package = formatter("span",
                                               style = "font-family: monospace;")))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
