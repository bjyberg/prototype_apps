library(shiny)
library(leaflet)
library(shinyWidgets)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(ggplot2)

# backend code
countries <- unlist(st_read("www/GADM_adm1.gpkg",
  query = "select COUNTRY from 'GADM_adm1'", quiet = TRUE)[[1]] |>
  unique())
pop <- rast("www/AfriPop-total.tiff")

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
            conditionalPanel(
              condition = "input.Country.length == 1",
              pickerInput("Admin", "Admin Region", choices = NULL,
               options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE),
            multiple = TRUE)
          ),
          sliderInput("pop_range", "Population Range:",
            min = 1, max = 200000, # minmax(pop)[2],
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
            fixed = TRUE, draggable = TRUE, top = 40, left = "auto",
            right = 20, bottom = "auto", width = 200, height = "auto",
            style = "opacity: 0.8;",
            h2("Legend"),
            plotOutput("bivar_legend", height = 200),
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  region_selection <- reactive({
    req(input$Country)
    if (length(input$Country) == 1) {
      st_read("www/GADM_adm1.gpkg",
        query = paste("select * from 'GADM_adm1' where COUNTRY = ",
          paste0("'", input$Country, "'")), quiet = TRUE)
    } else if (length(input$Country) > 1) {
      st_read("www/GADM_adm1.gpkg",
        query = paste("select * from 'GADM_adm1' where COUNTRY in",
          paste0("(",
            paste(paste0("'", input$Country, "'"), collapse = ", "),
            ")")
        ),  quiet = TRUE
      )
    }
  })
  observeEvent(region_selection(), {
    if (length(input$Country) == 1) {
      admn_1 <- unique(region_selection()$NAME_1)
      updatePickerInput(session = session, inputId = "Admin", choices = admn_1,
        selected = NULL)
    }
  })

  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(data = region_selection(), 
        weight = .6, fillColor = "grey10", color = 'black') |>
      addPolygons(data = region_selection(), group = 'ADM1',
        weight = .6, fillColor = "grey10", color = 'black', 
        fillOpacity = 0) |>
      setView(lng = 20, lat = -6, zoom = 3)
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
    #if (isTruthy(input$Admin)) {

    #   crop(pop, adm_region_select(), mask = TRUE) |>
    #     clamp(lower = input$pop_range[1], upper = input$pop_range[2],
    #           values = FALSE)
    # } else {
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
  
  output$init_Table <- renderTable(st_drop_geometry(region_filled()))

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
          "Region: ", NAME_1,
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
    bivar_pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
      "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
    legend_df <- data.frame(suitability = rep(c(1:3), 3),
      adpative = rep(1:3, each = 3),
      fill_col = bivar_pal)

    ggplot(legend_df) +
      geom_tile(mapping = aes(
        x = suitability,
        y = adpative,
        fill = fill_col)) +
      scale_fill_identity() +
      # labs(x = paste(clean_name, "Hazard →"),
      #   y = "Adaptive Capacity →") +
      labs(x = paste("Climate Hazard →"),
        y = "Adaptive Capacity →") +
      theme_void() +
      theme(
        axis.title = element_text(
          size = 18,
        ),
        axis.title.y = element_text(angle = 90)) +
      coord_fixed()
  })

}
shinyApp(ui, server)