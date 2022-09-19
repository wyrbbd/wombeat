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
    menuItem("Historical Hotspots",
             tabName = "Map2", icon = icon("list"))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "Map1",
            #####dashboard theme######
            shinyDashboardThemes(theme = "blue_gradient"),
            ####Title####
            h2("Hotspot Map: Real Time ", 
               style = 	"color:#4682B4"), 
            fluidRow(
                #####map output with loader###
                box(withLoader(leafletOutput("map1", height =800,width = "200%"),type = "html", loader = "loader1")))
    ),
    tabItem(tabName = "Map2",
            #####dashboard theme######
            shinyDashboardThemes(theme = "blue_gradient"),
            ####Title####
            h2("Historical Hotspots", 
               style = 	"color:#4682B4"), 
            fluidRow(
              #####map output with loader###
              box(withLoader(leafletOutput("map2", height =600,width = "200%"),type = "html", loader = "loader1"))),
            sliderInput("DatesMerge", "Dates:",
                        min = as.Date("2020-10-01","%Y-%m-%d"),
                        max = as.Date("2021-02-28","%Y-%m-%d"),
                        value=as.Date("2020-10-01","%Y-%m-%d"),
                        width = "100%",
                        animate =
                          animationOptions(interval = 500, loop = F)
                        ))
    )
  )
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
    hotspot_show$datetime <- str_replace(hotspot_show$datetime,"T"," ")
    hotspot_show$datetime <- str_replace(hotspot_show$datetime,"Z"," ")
    hotspot_show$datetime <- as_datetime(hotspot_show$datetime) + dhours(10)
    hotspot_show
  })
  
  pals = colorFactor(palette =c("#800000","#FF0000","#FF4500","#FF8C00","#F5DEB3"),
                     levels = c("0-2","2-6","6-24","24-48","48-72"))
  
  basemap <- leaflet() %>%
    setView(145,-30,zoom = 5)%>%
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
        "Esri.WorldImagery", "Stamen.Toner",
        "Stamen.Terrain", "Esri.WorldStreetMap",
        "Wikimedia", "CartoDB.Positron","OpenStreetMap"
      ),
      # position it on the topleft
      position = "topleft"
    )
  output$map1 <- renderLeaflet({
    map_1 <- basemap %>%
      addCircles(
        data = t(),
        color = ~pals(hours_since_hotspot_class),
        # create custom labels
        label = paste(
          "Time: ", t()$datetime, "<br>",
          "Hours: ",t()$hours_since_hotspot,  "<br>",
          "Satellite:",t()$satellite_operating_agency
        ) %>%
          lapply(htmltools::HTML)
      ) %>%
      # add a legend
      addLegend(
        colors = c("#800000","#FF0000","#FF4500","#FF8C00","#F5DEB3"),
        labels = c("0 - 2 hours","2 - 6 hours","6 - 24 hours","24 - 48 hours","48 - 72 hours"),
        title = "Hours",
        opacity = 1, 
        position = "bottomleft")
    map_1
    
  })
  
  test <- read.csv("2020-2021hotpots.csv")
  test$datetime <- as.Date(test$datetime)
  vic_map <- vic_map %>%st_set_crs("WGS 84")
  test <-  st_as_sf(x = test, coords = c('longitude','latitude')) %>%st_set_crs("WGS 84")
  test2 = st_intersects(vic_map$geometry, test$geometry)
  test2 = test[test2[[1]],]
  
  
  timeData <- reactive({
    test2 <- test2%>%
      filter(datetime == input$DatesMerge)
  })
  

  output$map2 <- renderLeaflet({
    map_2 <- basemap %>%
      addCircles(
        data = timeData(),
        color = "red"
      )
    map_2
    
  })

}
# Run the application 
shinyApp(ui = ui, server = server)
