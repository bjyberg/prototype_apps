library(shiny)
library(leaflet)
library(shinyWidgets)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)

# backend code
countries <- unlist(st_read("www/GADM_adm1.gpkg",
  query = "select COUNTRY from 'GADM_adm1'", quite = TRUE)[[1]] |>
  unique())

# UI
ui <- fluidPage(
  navbarPage("Atlas",
    tabPanel("Map",
      sidebarLayout(
        sidebarPanel(
          pickerInput("Country", "Select Country", choices = countries,
            selected = "Kenya",
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE),
            multiple = TRUE),
          # virtualSelectInput("Admin", "Customer", choices = NULL),
          hr(),
          actionButton(inputId = "crop", label = "Crop")
        ),
        mainPanel(
          tabsetPanel(type = "tabs",
            tabPanel("Interactive Map",
              leafletOutput("map")
            ),
            tabPanel("Variable Maps",
              plotOutput("variable_plot")
            ),
          )
        )
      ),
      tabPanel("Bivariate Mapping", # I think this can be done by having colours be the attribute and then have legend in the
        verbatimTextOutput("summary")
))))

server <- function(input, output, session) {
  region_selection <- reactive({
    req(input$Country)
    if (length(input$Country) == 1) {
      st_read("www/GADM_adm1.gpkg",
        query = paste("select * from 'GADM_adm1' where COUNTRY = ",
          paste0("'", input$Country, "'")))
    } else if (length(input$Country) > 1) {
      st_read("www/GADM_adm1.gpkg",
        query = paste("select * from 'GADM_adm1' where COUNTRY in",
          paste0("(",
            paste(paste0("'", input$Country, "'"), collapse = ", "),
            ")")
        )
      )
    }
  })
  cropped_rasters <- eventReactive(input$crop, {
    rast("www/Gender_Equity_hotspot_unmasked.tif") |>
      crop(region_selection(), mask = TRUE)
  })

  region_filled <- reactive({
    req(cropped_rasters())
    extracted_vals <- exact_extract(cropped_rasters(), region_selection(),
      c("mean", "stdev"))
    cbind(region_selection(), extracted_vals) |>
      st_as_sf()
  })

   output$variable_plot <- renderPlot({
    req(cropped_rasters())
     plot(cropped_rasters())
   })

   leaflet_rast <- reactive({
    req(cropped_rasters())
    cropped_rasters() |>
      raster::raster()
  })

    output$map <- renderLeaflet({
      leaflet() |>
        addTiles() |>
        #addRasterImage(leaflet_rast(), opacity = 0.8) |>
        addPolygons(data = region_selection())
    })

  observe({
    leaf_rast <- leaflet_rast()
        pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
          values(leaf_rast), na.color = "transparent")
    leafletProxy("map") |>
      clearShapes() |>
      addRasterImage(leaf_rast, colors = pal, opacity = 0.8) |>
      addLegend(position = "bottomright", pal = pal,
       values = values(leaf_rast),
      title = "Gender Equity") |>
      addPolygons(data = region_filled(), fillColor = "transparent",
        color = "black", weight = .5,
        popup = ~ paste0("Mean: ", mean, "<br>Stdev: ", stdev))
  })

}
shinyApp(ui, server)
