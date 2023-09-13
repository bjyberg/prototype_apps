library(shiny)
library(leaflet)
library(shinyWidgets)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(ggplot2)
source("R/functions.r")

# backend code
# countries_df <- st_read("www/GADM_adm1.gpkg",
#   query = "select GID_0, NAME_0 from 'GADM_adm1'", quiet = TRUE) |>
#   unique()
countries_df <- st_read('www/aggregated_data_adm1.gpkg', 
  query = "select GID_0, NAME_0 from 'aggregated_data_adm1'", quiet = TRUE) |>
  unique()

# filters_df <- data.frame( # In half dev - need to complete
#   name = c("population", "poverty"),
#   path = c("www/AfriPop-total.tiff", "www/grdi_r1r3r2_filled.tif")
# )
AC_df <- read.csv("www/file_dict.csv")
ac_weightings <- read.csv("www/ac_weights.csv")

# pop <- rast("www/AfriPop-total.tiff")

# layers <- read.csv("www/file_dict.csv")
# ac_layers <- layers[layers$group == "ad_cap", ]
# hazard_layers <- layers[layers$group == "hazard", ]

# UI
ui <- fluidPage(
  navbarPage("Vulnerability Prototype",
    tabPanel("Map",
      sidebarLayout(
        sidebarPanel(
          pickerInput("Country", "Select Country",
            choices = countries_df$NAME_0,
            selected = countries_df$NAME_0[5],
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE),
            multiple = TRUE
          ),
          conditionalPanel(
            condition = "input.Country.length == 1",
            pickerInput("Admin", "Admin Region", choices = NULL,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE),
              multiple = TRUE)
          ),
          pickerInput("AC_dim", "Select AC Dimension",
            choices = AC_df$name,
            selected = AC_df$name[2],
            options = list(`actions-box` = TRUE),
            multiple = TRUE
          ),
          # pickerInput("filterVar", "Filter by:", choices = filters_df$name,
          #   selected = NULL),
          # conditionalPanel(
          #   condition = paste0("input.filterVar == '", filters_df$name[1], "'"),
          #   sliderInput("pop_range", paste(filters_df$name[1], "Range:"),
          #     min = 0, max = 200000, # minmax(pop)[2],
          #     value = c(0, 50000))
          # ),
          # sliderInput("pop_range", "Population Range:",
          #   min = 0, max = 200000, # minmax(pop)[2],
          #   value = c(0, 50000)),
          hr(),
          actionButton(inputId = "crop", label = "Crop")
        ),
        mainPanel(
          tabsetPanel(type = "tabs",
            tabPanel("Interactive Map",
              leafletOutput("map"),
              #tableOutput("init_Table")
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
    tabPanel("Adaptive Capacity Index",
      sidebarLayout(
        sidebarPanel(
          pickerInput("ac_weight", "Weighting Scenario",
            choices = names(ac_weightings[-1]))
        ),
        mainPanel(
          plotOutput("ac_index_plot")
        )
      )
    ),
    tabPanel("Bivariate Mapping",
      sidebarLayout(
        sidebarPanel(
          pickerInput("bivar_x", "Select X Variable", choices = AC_df$name,
            selected = "gender"),
          pickerInput("bivar_y", "Select Y Variable", choices = AC_df$name,
            selected = "poverty"),
          checkboxInput("addZ", "Plot a 3rd variable?", FALSE),
          conditionalPanel(
            condition = "input.addZ == 1",
            pickerInput("bivar_z", "Select 3rd Variable", choices = AC_df$name,
              selected = NULL)
          ),
          # hr(),
          # verbatimTextOutput("Bivar_messages"),
        ),
        mainPanel(
          leafletOutput("bivar_map"),
          absolutePanel(id = "bivar_leg", class = "panel panel-default",
            fixed = TRUE, draggable = FALSE, top = 80, left = "auto",
            right = 30, bottom = "auto", width = 100, height = "auto",
            style = "opacity: 0.8;",
            # h2(),
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
    st_read("www/aggregated_data_adm1.gpkg",
      query = paste(
        "select * from 'aggregated_data_adm1'",
        "where GID_0 in",
        paste0("(",
          paste(paste0("'", gids, "'"), collapse = ", "),
          ")")
      ),  quiet = TRUE
    )
  })
  region_selection <- debounce(region_selection, 1500)
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
    } else {
      updatePickerInput(session = session, inputId = "Admin", choices = NULL)
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
    if (length(input$Country) > 1) {
      region_selection()
    } else if (isTruthy(input$Admin)) {
      adm_region_select()
    } else {
      region_selection()
    }
  })

  # pop_mask <- eventReactive(input$crop, {
  #   req(final_region())
  #   crop(pop, final_region(), mask = TRUE) |>
  #     clamp(lower = input$pop_range[1], upper = input$pop_range[2],
  #       values = FALSE)
  # })

  # filter_rast <- eventReactive(input$crop, {
  #   req(input$filter_var)
  #   filter_paths <- filters_df$path[match(input$filter_var, filters_df$name)]
  #   rast(filter_paths)
  # })

  cropped_rasters <- eventReactive(input$crop, {
    paths <- AC_df$path[match(input$AC_dim, AC_df$name)]
    ac_names <- AC_df$name[match(input$AC_dim, AC_df$name)]
    ac_rast <- rast(paths) |>
      crop(final_region(), mask = TRUE)
    #  |>
    # crop(pop_mask(), mask = TRUE)
    names(ac_rast) <- ac_names
    return(ac_rast)
  })

  ac_index <- reactive({
    req(input$ac_weight)
    weights <- ac_weightings[[input$ac_weight]]
    ac_stack <- rast(AC_df$path)
    names(ac_stack) <- AC_df$name
    minmax(ac_stack, compute = TRUE)
    indexer(ac_stack, weights, fun = "mean")
  })

  output$ac_index_plot <- renderPlot({
    plot(ac_index())
  })

  region_filled <- reactive({
    req(input$AC_dim)
    col_clean_names <- gsub(".*\\.", "", names(final_region()))
    usr_cols <- names(final_region()[input$AC_dim == col_clean_names])
    return(final_region()[c("GID_0", "NAME_0", "NAME_1", usr_cols)])
    # extract_fn <- AC_df[match(ac_names, AC_df$name), "fn"]
    # extracted <- list()
    # if ("mean" %in% extract_fn) {
    #   mean_rasts <- cropped_rasters()[[match("mean", extract_fn)]]
    #   means <- exact_extract(mean_rasts, final_region(),
    #     c("mean", "stdev"), colname_fun = function(fun_name, values) {
    #       paste(values, fun_name, sep = "_")
    #     })
    #   extracted <- append(extracted, means)
    # }
    # if ("sum" %in% extract_fn) {
    #   sum_rasts <- cropped_rasters()[[match("sum", extract_fn)]]
    #   sums <- exact_extract(sum_rasts, final_region(), "sum",
    #     colname_fun = function(fun_name, values) {
    #       paste(values, fun_name, sep = "_")
    #     })
    #   extracted <- append(extracted, sums)
    # }
    # extracted$pop <- exact_extract(pop, final_region(), fun = "sum")
    # cbind(final_region(), as.data.frame(extracted)) |>
    #   st_as_sf()
  })

  output$init_Table <- renderTable(
    st_drop_geometry(region_filled())
  )

  output$variable_plot <- renderPlot({
    req(cropped_rasters())
    plot(cropped_rasters())
  })

  observeEvent(input$crop, {
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
      values(cropped_rasters()), na.color = "transparent")
    
    leafletProxy("map") |>
      clearShapes() |>
      clearImages() |>
      clearControls() |>
      addRasterImage(cropped_rasters(), colors = pal, opacity = 0.8) |>
      addLegend(position = "bottomright", pal = pal,
        values = values(cropped_rasters()),
        title = paste(names(cropped_rasters()))) |>
      addPolygons(data = region_filled(), fillColor = "transparent",
        color = "black", weight = .5 #,
        # popup = ~ paste0(
        #   "Country: ", NAME_0,
        #   "<br>Region: ", NAME_1,
        #   "<br>Mean Equality: ", mean,
        #   "<br>Stdev: ", stdev,
        #   "<br>Population: ", pop)
      )
  })


  # Bivariate Mapping
  output$Bivar_messages <- renderPrint({
    req(input$bivar_x, input$bivar_y)
    if (input$bivar_x == input$bivar_y) {
      print("X and Y variables cannot be the same.")
    }
  })

  bi_vars <- reactive({
    req(final_region(), input$bivar_x, input$bivar_y)
    if (input$bivar_x != input$bivar_y) {
      paths <- AC_df$path[match(c(input$bivar_x, input$bivar_y), AC_df$name)]
      var_stack <- rast(paths) |>
        crop(final_region(), mask = TRUE)
    }
  })

  bivar_data <- reactive({
    req(bi_vars())
    make_bivariate_data(bi_vars())
  })
  output$bivar_legend <- renderPlot({
    req(final_region(), bivar_data())
    bivar_legend(input$bivar_x, input$bivar_y)
  })
  output$bivar_map <- renderLeaflet({
    pal1 <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
      values(raster::raster(bi_vars()[[1]])), na.color = "transparent")
    pal2 <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
      values(raster::raster(bi_vars()[[2]])), na.color = "transparent")
    bi_pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
      "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
    # the palette needs transposed to match the legend
    bi_pal <- as.vector(t(matrix(bi_pal, nrow = 3, ncol = 3)))
    leaflet() |>
      addTiles() |>
      addRasterImage(raster::raster(bivar_data()), group = "Bivariate Map",
        color = colorFactor(palette = bi_pal, values(bivar_data()),
          na.color = "transparent", alpha = .8)) |>
      addRasterImage(raster::raster(bi_vars()[[1]]),
        group = "X", opacity = 0.8, colors = pal1) |>
      addLegend(position = "bottomleft", pal = pal1, group = "X",
        values = values(raster::raster(bi_vars()[[1]]))) |>
      addRasterImage(raster::raster(bi_vars()[[2]]),
        group = "Y", opacity = 0.8, colors = pal2) |>
      addLegend(position = "bottomleft", pal = pal2, group = "Y",
        values = values(raster::raster(bi_vars()[[2]]))) |>
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Bivariate Map", "X", "Y"),
        options = layersControlOptions(collapsed = FALSE)) |>
      hideGroup(c("X", "Y"))
  })
  z_dots <- reactive({
    req(input$bivar_z, input$bivar_x, input$bivar_y)
    path <- AC_df$path[match(c(input$bivar_z), AC_df$name)]
    z_var <- rast(path) |>
      crop(final_region(), mask = TRUE)
    centers <- st_point_on_surface(final_region())
    centers$z <- exact_extract(z_var, final_region(), "mean")
    return(centers)
  })
  observe({
    req(input$bivar_z, input$bivar_x, input$bivar_y)
    if (isTRUE(input$addZ)) {
      symbols <- leaflegend::makeSymbolsSize(
        values = log(z_dots()$z + 1) * 10,
        shape = "circle",
        color = "#f0e446",
        fillColor = "#f0e446",
        opacity = .5,
        baseSize = 10
      )
      leafletProxy("bivar_map") |>
        clearMarkers() |>
        clearControls() |>
        addMarkers(data = z_dots(), icon = symbols, group = "z_group") |>
        leaflegend::addLegendSize(shape = "circle", color = "black",
          baseSize = 10, fillOpacity = .7,
          values = log(z_dots()$z + 1) * 10,
          orientation = "horizontal",
          title = "z", position = "bottomleft", data = symbols,
          group = "z_group")
    }
  })
}
shinyApp(ui, server)