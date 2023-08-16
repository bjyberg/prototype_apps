library(shiny)
library(leaflet)
library(shinyWidgets)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(ggplot2)
source('R/functions.r')

# backend code
countries_df <- st_read("www/GADM_adm1.gpkg",
  query = "select GID_0, NAME_0 from 'GADM_adm1'", quiet = TRUE) |>
  unique()
pop <- rast("www/AfriPop-total.tiff")

# UI
ui <- fluidPage(
  navbarPage("Atlas",
    tabPanel("Map",
      sidebarLayout(
        sidebarPanel(
          pickerInput("Country", "Select Country", choices = countries_df$NAME_0,
            selected = NULL,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE),
            multiple = TRUE),
            conditionalPanel(
              condition = "input.Country.length == 1",
              pickerInput("Admin", "Admin Region", choices = NULL,
               options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE),
            multiple = TRUE)
          ),
          sliderInput("pop_range", "Population Range:",
            min = 0, max = 200000, # minmax(pop)[2],
            value = c(0, 50000)),
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
            tabPanel("Summary Table",
              tableOutput("init_Table")
            ),
          )
        )
      ),
    ),
    tabPanel("Bivariate Mapping",
      sidebarLayout(
        sidebarPanel(
          pickerInput("bivar_x", "Select X Variable", choices = NULL),
          pickerInput("bivar_y", "Select Y Variable", choices = NULL)
        ),
        mainPanel(
          leafletOutput("bivar_map"),
          absolutePanel(id = "bivar_leg", class = "panel panel-default",
            fixed = TRUE, draggable = FALSE, top = 80, left = "auto",
            right = 30, bottom = "auto", width = 100, height = "auto",
            style = "opacity: 0.8;",
            h2("Legend"),
            plotOutput("bivar_legend", height = 100),
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  region_selection <- reactive({
    req(input$Country)
    gids <- countries_df$GID_0[match(input$Country, countries_df$NAME_0)]
    st_read("www/GADM_adm1.gpkg",
      query = paste("select GID_0, NAME_0, NAME_1, geom from 'GADM_adm1'",
        "where GID_0 in",
        paste0("(",
          paste(paste0("'", gids, "'"), collapse = ", "),
          ")")
      ),  quiet = TRUE
    )
  })
  region_selection <- debounce(region_selection, 2000)
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
        setView(lng = 20, lat = -6, zoom = 3) |>
      addCircles(lng = 20, lat = -6, group = "ADM1", # added for group delete
        fill = FALSE, weight = 0) |>
      addCircles(lng = 20, lat = -6, group = "ADM0", fill = FALSE, weight = 0)

  })

  observeEvent(region_selection(), {
    if (length(input$Country) == 1) {
      admn_1 <- unique(region_selection()$NAME_1)
      updatePickerInput(session = session, inputId = "Admin", choices = admn_1,
        selected = NULL)
    }
  })

  observe({
    req(region_selection())
    leafletProxy("map") |>
      clearGroup(group = "ADM0") |>
      addPolygons(data = region_selection(), weight = .6,
        fillColor = "grey10", color = "black", group = "ADM0")
  })
  
  adm_region_select <- reactive({
    req(input$Admin)
    region_selection()[region_selection()$NAME_1 %in% input$Admin, ]
  })

  observe({
    req(input$Admin)
    leafletProxy("map") |>
      clearGroup(group = "ADM1") |>
      addPolygons(data = adm_region_select(), group = "ADM1",
        color = "blue", weight = .7, fillOpacity = 0.2)
  })

  final_region <- reactive({
    if (isTruthy(input$Admin)) {
      adm_region_select()
    } else {
      region_selection()
    }
  })

  pop_mask <- eventReactive(input$crop, {
    req(final_region())
    crop(pop, final_region(), mask = TRUE) |>
      clamp(lower = input$pop_range[1], upper = input$pop_range[2],
            values = FALSE)
    # }
  })

  cropped_rasters <- eventReactive(input$crop, {
    rast("www/Gender_Equity_hotspot_unmasked.tif") |>
      crop(final_region(), mask = TRUE) |>
      crop(pop_mask(), mask = TRUE)
  })

  region_filled <- reactive({
    req(cropped_rasters())
    extracted_var <- exact_extract(cropped_rasters(), final_region(),
      c("mean", "stdev"))
    extracted_var$pop <- exact_extract(pop, final_region(), fun = "sum")
    cbind(final_region(), extracted_var) |>
      st_as_sf()
  })

  output$init_Table <- renderTable(
    st_drop_geometry(region_filled())
  )

  output$variable_plot <- renderPlot({
    req(cropped_rasters())
    plot(cropped_rasters())
  })

  leaflet_rast <- reactive({ # Leaflet issues with spatrasters on cran version
    req(cropped_rasters())
    cropped_rasters() |>
      raster::raster()
  })


  observeEvent(input$crop, {
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
      values(leaflet_rast()), na.color = "transparent")
    leafletProxy("map") |>
      clearShapes() |>
      clearImages() |>
      clearControls() |>
      addRasterImage(leaflet_rast(), colors = pal, opacity = 0.8) |>
      addLegend(position = "bottomright", pal = pal,
       values = values(leaflet_rast()),
      title = "Gender Equity") |>
      addPolygons(data = region_filled(), fillColor = "transparent",
        color = "black", weight = .5,
        popup = ~ paste0(
          "Country: ", NAME_0,
          "<br>Region: ", NAME_1,
          "<br>Mean Equality: ", mean,
          "<br>Stdev: ", stdev,
          "<br>Population: ", pop)
      )
  })


  # Bivariate Mapping
  output$bivar_map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(data = final_region())
  })
  output$bivar_legend <- renderPlot({
    req(final_region())
    bivar_legend('x', 'y')
  })

}
shinyApp(ui, server)