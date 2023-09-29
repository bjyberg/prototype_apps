library(shiny)
library(leaflet)
# library(mapview)
library(shinyWidgets)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(htmlTable)
source("R/functions.r")

# backend code
# countries_df <- st_read("www/GADM_adm1.gpkg",
#   query = "select GID_0, NAME_0 from 'GADM_adm1'", quiet = TRUE) |>
#   unique()


sub_s_full <- st_read("www/aggregated_data_adm0.gpkg",
  query = paste(
    "select * from 'aggregated_data_adm0'",
    "where  NAME_0='region_avg'")) |>
  st_drop_geometry()
sub_s_full$vulnerability_index <- NA # dummy to allow binding

countries_df <- st_read("www/aggregated_data_adm1.gpkg",
  query = "select GID_0, NAME_0 from 'aggregated_data_adm1'", quiet = TRUE) |>
  unique()
# filters_df <- data.frame( # In half dev - need to complete
#   name = c("population", "poverty"),
#   path = c("www/AfriPop-total.tiff", "www/grdi_r1r3r2_filled.tif")
# )
dict_df <- read.csv("www/file_dict.csv")
AC_df <- dict_df[dict_df$group == "ad_cap", ]
hazard_df <- dict_df[dict_df$group == "hazard", ]

future_dict_df <- read.csv("www/future_file_dict.csv")
fut_AC_df <- future_dict_df[future_dict_df$group == "ad_cap", ]
# fut_hazard_df <- future_dict_df[future_dict_df$group == "hazard", ]

# AC_df <- read.csv("www/cloud_file_dict.csv")
# AC_df$path <- paste0("/vsicurl/", AC_df$path)
ac_weightings <- read.csv("www/ac_weights.csv")

# pop <- rast("www/AfriPop-total.tiff")

# UI
ui <- fluidPage(
  navbarPage("Vulnerability Prototype",
  ### The UI section for baseline ac -----
    tabPanel("Baseline",
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
          pickerInput("ac_weight", "Vulnerability Index Calculation Method",
            choices = c('Mean', 'Geometric Mean'),
            #choices = names(ac_weightings[-c(1, 2)])
          ),
          checkboxInput("explore_dims",
            label = "Explore Individual Vulnerability Dimensions?",
            FALSE
          ),
          conditionalPanel(
            condition = "input.explore_dims == 1",
            pickerInput("AC_dim", "Select Vulnerability Dimension",
              choices = AC_df$name,
              selected = NULL,
              options = list(`actions-box` = TRUE),
              multiple = TRUE
            ),
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
          #actionButton(inputId = "crop", label = "Crop"),
          downloadButton("download", "Download Data"),
          radioButtons("downloadType", "Download Type",
            choices = c("CSV" = ".csv",
              "GeoTIFF" = ".tif",
              "Geopackage" = ".gpkg",
              "Shapefile" = ".shp"),
          )
        ),
        mainPanel(
          leafletOutput("map"),
          tabsetPanel(type = "tabs",
            tabPanel("Summary Table",
              DTOutput("init_Table")),
            tabPanel("Regional Comparison",
              plotlyOutput("bar_plot")),
            tabPanel("Distribution Comparison",
              plotlyOutput("box_plot")),
            tabPanel("Correlation",
            textOutput("cor_plot_warning"),
              plotOutput("cor_plot"))
          ),
          # tabsetPanel(type = "tabs",
          #   tabPanel("Interactive Map",
          #     leafletOutput("map"),
          #     # tableOutput("init_Table")
          #   ),
          #   tabPanel("Variable Maps",
          #     plotOutput("variable_plot")
          #   ),
          #   tabPanel("Summary Table",
          #     DTOutput("init_Table")
          #   ),
          # )
        )
      ),
    ),
    ### The UI section for the future projections of ac -----
    tabPanel("Future",
      sidebarLayout(
        sidebarPanel(
          pickerInput("fut_Country", "Select Country",
            choices = countries_df$NAME_0,
            selected = NULL,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE),
            multiple = TRUE
          ),
          conditionalPanel(
            condition = "input.Country.length == 1",
            pickerInput("fut_Admin", "Admin Region", choices = NULL,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE),
              multiple = TRUE)
          ),
          pickerInput("proj_year", "Projection Year",
            choices = unique(future_dict_df$year)),
          pickerInput("scenario", "Select Scenario",
            choices = unique(future_dict_df$projection)),
          pickerInput("fut_ac_weight", "Vulnerability Index Calculation Method",
            choices = c("Mean", "Geometric Mean"),
          ),
          checkboxInput("fut_explore_dims",
            label = "Explore Individual Vulnerability Dimensions?",
            FALSE
          ),
          conditionalPanel(
            condition = "input.fut_explore_dims == 1",
            pickerInput("fut_AC_dim", "Future Vulnerability Dimension",
              choices = unique(fut_AC_df$name),
              selected = NULL,
              options = list(`actions-box` = TRUE),
              multiple = TRUE
            ),
          ),
          hr(),
          downloadButton("fut_download", "Download Data"),
          radioButtons("fut_downloadType", "Download Type",
            choices = c("CSV" = ".csv",
              "GeoTIFF" = ".tif",
              "Geopackage" = ".gpkg",
              "Shapefile" = ".shp"),
          )
        ),
        mainPanel(
          leafletOutput("fut_map"),
          tabsetPanel(type = "tabs",
            tabPanel("Summary Table",
              DTOutput("fut_init_Table")),
            tabPanel("Regional Comparison",
              plotlyOutput("fut_bar_plot")),
            tabPanel("Distribution Comparison",
              plotlyOutput("fut_box_plot")),
            tabPanel("Correlation",
            textOutput("fut_cor_plot_warning"),
              plotOutput("fut_cor_plot"))
          )
        )
      )
    ),
    ### The UI section for bi-variate mapping -----
    tabPanel("Hazard Interaction",
      sidebarLayout(
        sidebarPanel(
          pickerInput("bivar_x", "Select Vulnerability Variable", 
          choices = c(AC_df$name, 'Vulnerability Index'),
            selected = "gender"),
          pickerInput("bivar_y", "Select Hazard Variable",
           choices = hazard_df$name,
            selected = "heat_stress"),
          checkboxInput("addZ", "Plot a 3rd variable?", FALSE),
          conditionalPanel(
            condition = "input.addZ == 1",
            pickerInput("bivar_z", "Select 3rd Variable", choices = AC_df$name,
              selected = NULL)
          ),
          hr(),
          downloadButton("download_bivar", "Download Bivariate analysis")
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
    if (length(input$Country) == 1) {
      st_read("www/aggregated_data_adm1.gpkg",
        query = paste(
          "select * from 'aggregated_data_adm1'",
          "where GID_0 in",
          paste0("(",
            paste(paste0("'", gids, "'"), collapse = ", "),
            ")")
        ),  quiet = TRUE
      )
    } else if (length(input$Country) > 1) {
        st_read("www/aggregated_data_adm0.gpkg",
          query = paste(
            "select * from 'aggregated_data_adm0'",
            "where GID_0 in",
            paste0("(",
              paste(paste0("'", gids, "'"), collapse = ", "),
              ")")
          ),  quiet = TRUE
        )
    }
  })
  region_selection <- debounce(region_selection, 2000)

  observeEvent(region_selection(), {
    if (length(input$Country) == 1) {
      admn_1 <- unique(region_selection()$NAME_1)
      updatePickerInput(session = session, inputId = "Admin", choices = admn_1,
        selected = NULL)
    } else {
      updatePickerInput(session = session, inputId = "Admin", choices = NULL)
    }
  })

  adm_region_select <- reactive({
    req(input$Admin)
    region_selection()[region_selection()$NAME_1 %in% input$Admin, ]
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

  ac_index <- reactive({
    req(input$ac_weight)
    weights <- ac_weightings[[input$ac_weight]]
    ac_stack <- rast(AC_df$path) |>
      crop(final_region(), mask = TRUE)
    names(ac_stack) <- AC_df$name
    # Round the pov layer atm to fix floating point errors
    ac_stack[["poverty"]] <- round(ac_stack[["poverty"]], 10) # fix data instead
    need_inv <- AC_df[AC_df$inverse, "name"]
    ac_stack[[need_inv]] <-  (
      minmax(ac_stack[[need_inv]])[2, ]
      - ac_stack[[need_inv]]
        + minmax(ac_stack[[need_inv]])[1, ]
    )
    if (input$ac_weight == "Mean") {
      ac_index <- indexer(ac_stack, fun = "mean")
    } else if (input$ac_weight == "Geometric Mean") {
      ac_index <- indexer(ac_stack, fun = "geometric_mean")
    }
    names(ac_index) <- 'vulnerabiltiy_index'
    return(ac_index)
  })

  region_filled <- reactive({
    req_cols <- c("GID_0", "NAME_0")
    if ("NAME_1" %in% names(final_region())) {
      req_cols <- c("NAME_1", req_cols)
    }
    if (isTruthy(input$AC_dim)) {
      col_clean_names <- gsub(".*\\.", "", names(final_region()))
      usr_cols <- names(final_region()[col_clean_names %in% input$AC_dim])
      selected_region_data <- final_region()[c(req_cols, usr_cols)]
    } else {
      selected_region_data <- final_region()[req_cols]
    }
    selected_region_data$vulnerability_index <- exact_extract(ac_index(),
     final_region(),
      fun = "mean")
    return(selected_region_data)
  })

  output$map <- renderLeaflet({
    req(ac_index(), final_region())
    ac_pal <- colorNumeric("OrRd", values(ac_index()), na.color = "transparent")
    leaflet() |>
      addTiles() |>
      addRasterImage(ac_index(), colors = ac_pal, group = 'ac_index') |>
      addLegend(pal = ac_pal, values = values(ac_index()), group = 'ac_index') |>
      addCircles(lng = 20, lat = -6, group = "ADM1", # added for group delete
        fill = FALSE, weight = 0) |>
      addCircles(lng = 20, lat = -6, group = "ADM0", fill = FALSE, weight = 0)
  })

  # output$ac_index_plot <- renderPlot({
  #   plot(ac_index())
  # })

  observe({
    req(region_selection())
    leafletProxy("map") |>
      clearGroup(group = "ADM0") |>
      addPolygons(data = region_selection(), weight = .6,
        fillColor = "grey10", color = "black", group = "ADM0")
  })

  observe({
    req(input$Admin)
    leafletProxy("map") |>
      clearGroup(group = "ADM1") |>
      addPolygons(data = adm_region_select(), group = "ADM1",
        color = "blue", weight = .7, fillOpacity = 0.2)
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

  cropped_rasters <- reactive({
    req(input$AC_dim)
    paths <- AC_df$path[match(input$AC_dim, AC_df$name)]
    ac_names <- AC_df$name[match(input$AC_dim, AC_df$name)]
    ac_dims <- rast(paths) |>
      crop(final_region(), mask = TRUE)
    #  |>
    # crop(pop_mask(), mask = TRUE)
    names(ac_dims) <- ac_names
    # ac_stack <- c(ac_dims, ac_index())
    # names(ac_stack) <- c(names(ac_dims), "ac_index")
    # return(ac_stack)
    return(ac_dims)
  })


  output$init_Table <- renderDT(
    # expr = ({
    #   table_df <- st_drop_geometry(region_filled())
    #   if (length(input$Country) > 1) {
    #     sub_s_full <- sub_s_full[names(sub_s_full) %in% names(table_df)]
    #     rbind(table_df, sub_s_full)
    #   } else {
    #     table_df
    #   }
    # }),
    st_drop_geometry(region_filled()),
    options = list(paging = TRUE,
                   pageLength = 5)
  )

  output$bar_plot <- renderPlotly({
    req(region_filled())
    region_df <- st_drop_geometry(region_filled())
    if ("NAME_1" %in% names(region_df)) {
      region_df$region <- region_df$NAME_1
    } else {
      region_df$region <- region_df$NAME_0
    }
    if (is.null(input$AC_dim)){
      gplot <- ggplot(region_df) +
        geom_bar(aes(x = region, y = vulnerability_index, fill = region),
          stat = "identity", position = "dodge") +
        labs(fill = "") +
          theme(
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 10)) +
        coord_flip()
    } else {
      if (length(input$Country) > 1) {
        sub_s_full <- sub_s_full[names(sub_s_full) %in% names(region_df)]
        sub_s_full$region <- sub_s_full$NAME_0
        region_df <- rbind(region_df, sub_s_full)
      }
      mean_cols <- grep("mean\\.", names(region_df))
      long_region <- tidyr::pivot_longer(region_df,
        cols = mean_cols, names_to = "Varible", values_to = "value")
      print(long_region$value)
      long_region$value <- round(as.numeric(long_region$value), 4)
      gplot <- ggplot(long_region) +
        geom_bar(aes(x = region, y = value, fill = region),
          stat = "identity", position = "dodge") +
        labs(fill = "") +
        facet_wrap(~ Varible, scales = "free_x") +
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 10)) +
        coord_flip()
    }
    ggplotly(gplot, tooltip = c('fill', 'y'))
  })
  
  output$box_plot <- renderPlotly({
    req(final_region(), ac_index())
    if (is.null(input$AC_dim)) {
      vals <- extract(ac_index(), final_region())
      if ("NAME_1" %in% names(final_region())) {
        vals$region <- st_drop_geometry(final_region())[vals[["ID"]], "NAME_1"]
      } else {
        vals$region <- st_drop_geometry(final_region())[vals[["ID"]], "NAME_0"]
      }
      vals$region <- as.factor(vals$region)
      ggbox <- ggplot(vals) +
        geom_boxplot(aes(x = region, y = vulnerabiltiy_index,
          group = region, fill = region)) +
        theme(
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 10)) +
        coord_flip()
    } else {
      vals <- extract(cropped_rasters(), final_region())
      if ("NAME_1" %in% names(final_region())) {
        vals$region <- st_drop_geometry(final_region())[vals[["ID"]], "NAME_1"]
      } else {
        vals$region <- st_drop_geometry(final_region())[vals[["ID"]], "NAME_0"]
      }
      vals$region <- as.factor(vals$region)
      long_vals <- tidyr::pivot_longer(vals, cols = names(cropped_rasters()),
        names_to = "layer", values_to = 'values')
      ggbox <- ggplot(long_vals) +
        geom_boxplot(aes(x = region, y = round(values, 3),
          group = region, fill = region)) +
        facet_wrap(~layer, scales = "free_x") +
          theme(
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 10)) +
          coord_flip()
    }
    ggplotly(ggbox, tooltip = c('x', 'y', 'fill'))
  })
  output$cor_plot_warning <- renderText({
    if (!isTruthy(input$AC_dim)) {
      print("At least one vulnerability dimension must be selected.")
    }
  })
  output$cor_plot <- renderPlot({
    req(cropped_rasters())
    cor_stack <- c(cropped_rasters(), ac_index())
    cor_mat <- layerCor(cor_stack, "pearson", na.rm = T)[[1]] |>
      as.data.frame()
    row.names(cor_mat) <- names(cor_stack)
    names(cor_mat) <- names(cor_stack)
    long_cormat <- as.table(as.matrix(cor_mat)) |>
      as.data.frame()
    pal <- RColorBrewer::brewer.pal(10, "Spectral")
    pal <- paste0(pal, 'A6')
    ggplot(long_cormat, aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = round(Freq, 3))) +
      scale_fill_gradientn(colours = pal,
        breaks = c(-1, 0, 1),
        limits = c(-1, 1)) +
      labs(fill = "Correlation") +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20)
        )
  })

  observe({ #input$AC_dim
    req(input$AC_dim)
    leafletProxy("map") |>
      clearImages() |>
      clearControls()
    ac_dims <- c(cropped_rasters(), ac_index())
    for (layer in 1:nlyr(ac_dims)) {
      pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
        values(ac_dims[[layer]]), na.color = "transparent")
      lyr_name <- names(ac_dims[[layer]])
      units <- AC_df[AC_df$name == lyr_name, "units"]
      leafletProxy("map") |>
        # clearShapes() |>
         clearControls() |>
        addRasterImage(ac_dims[[layer]], colors = pal, opacity = 0.8,
          group = lyr_name) |>
        addLegend(position = "bottomleft", pal = pal,
          values = values(ac_dims[[layer]]),
          title = paste(lyr_name, units),
          group = lyr_name) |>
        addLayersControl(
          position = "bottomright",
          overlayGroups = c(names(ac_dims)),
          options = layersControlOptions(collapsed = FALSE)) |>
        hideGroup(names(cropped_rasters())) # |>
      # addPolygons(data = region_filled(), fillColor = "transparent",
      #   color = "black", weight = .5
      # )
    }
    leafletProxy("map") |>
      addPolygons(data = region_filled(), fillColor = "transparent",
        color = "black", weight = .5)
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0("Vulnerability_data", input$downloadType)
    },
    content = function(file) {
      if (input$downloadType == ".csv") {
        st_drop_geometry(region_filled()) |>
          data.frame() |>
          write.csv(file)
      } else if (input$downloadType == ".tif") {
        if (isTruthy(input$AC_dim)) {
          c(ac_index(), cropped_rasters()) |>
            writeRaster(file, overwrite = TRUE)
        }
        writeRaster(ac_index(), file, overwrite = TRUE)
      } else if (input$downloadType == ".gpkg") {
        write_sf(region_filled(), file, append = FALSE)
      } else if (input$downloadType == ".shp") {
        write_sf(region_filled(), file, append = FALSE)
      }
    }
  )

  # Future/projections page -----
  
  # Update the Selections based on baseline, if user has selected
  observe({
    updateSelectInput(inputId = "fut_Country", selected = input$Country)
  })
  observe({
    updateSelectInput(inputId = "fut_ac_weight", selected = input$ac_weight)
  })
  fut_region_selection <- reactive({
    req(input$fut_Country)
    gids <- countries_df$GID_0[match(input$fut_Country, countries_df$NAME_0)]
    if (length(input$fut_Country) == 1) {
      st_read("www/aggregated_data_adm1.gpkg",
        query = paste(
          "select * from 'aggregated_data_adm1'",
          "where GID_0 in",
          paste0("(",
            paste(paste0("'", gids, "'"), collapse = ", "),
            ")")
        ),  quiet = TRUE
      )
    } else if (length(input$fut_Country) > 1) {
        st_read("www/aggregated_data_adm0.gpkg",
          query = paste(
            "select * from 'aggregated_data_adm0'",
            "where GID_0 in",
            paste0("(",
              paste(paste0("'", gids, "'"), collapse = ", "),
              ")")
          ),  quiet = TRUE
        )
    }
  })
  fut_region_selection <- debounce(fut_region_selection, 2000)

  observeEvent(fut_region_selection(), {
    if (length(input$fut_Country) == 1) {
      admn_1 <- unique(fut_region_selection()$NAME_1)
      updatePickerInput(session = session, inputId = "fut_Admin",
       choices = admn_1, selected = NULL)
    } else {
      updatePickerInput(session = session, inputId = "fut_Admin", choices = NULL)
    }
  })

  fut_adm_region_select <- reactive({
    req(input$fut_Admin)
    fut_region_selection()[fut_region_selection()$NAME_1 %in% input$fut_Admin, ]
  })

  fut_final_region <- reactive({
    if (length(input$fut_Country) > 1) {
      region_selection()
    } else if (isTruthy(input$fut_Admin)) {
      adm_region_select()
    } else {
      region_selection()
    }
  })

  usr_fut_AC_df <- reactive({
    req(input$proj_year, input$scenario)
    subset(fut_AC_df,
      year == input$proj_year    &
        projection == input$scenario)
  })
  
  fut_ac_index <- reactive({
    req(usr_fut_AC_df())
    ac_stack <- rast(usr_fut_AC_df()$path) |>
      crop(fut_final_region(), mask = TRUE)
    names(ac_stack) <- usr_fut_AC_df()$name
    # Round the pov layer atm to fix floating point errors
    # ac_stack[["poverty"]] <- round(ac_stack[["poverty"]], 10)
    # need_inv <- usr_fut_AC_df()[usr_fut_AC_df()$inverse, "name"]
    # ac_stack[[need_inv]] <- (
    #   minmax(ac_stack[[need_inv]])[2, ]
    #   - ac_stack[[need_inv]]
    #     + minmax(ac_stack[[need_inv]])[1, ]
    # )
    if (input$ac_weight == "Mean") {
      ac_index <- indexer(ac_stack, fun = "mean")
    } else if (input$ac_weight == "Geometric Mean") {
      ac_index <- indexer(ac_stack, fun = "geometric_mean")
    }
    names(ac_index) <- 'vulnerabiltiy_index'
    return(ac_index)
  })
  
  fut_region_filled <- reactive({
    req_cols <- c("GID_0", "NAME_0")
    if ("NAME_1" %in% names(fut_final_region())) {
      req_cols <- c("NAME_1", req_cols)
    }
    if (isTruthy(input$AC_dim)) {
      col_clean_names <- gsub(".*\\.", "", names(fut_final_region()))
      usr_cols <- names(fut_final_region()[col_clean_names %in% input$fut_AC_dim])
      selected_region_data <- fut_final_region()[c(req_cols, usr_cols)]
    } else {
      selected_region_data <- fut_final_region()[req_cols]
    }
    selected_region_data$vulnerability_index <- exact_extract(fut_ac_index(),
     fut_final_region(),
      fun = "mean")
    return(selected_region_data)
  })

  output$fut_map <- renderLeaflet({
    req(fut_ac_index(), fut_final_region())
    ac_pal <- colorNumeric("OrRd", values(fut_ac_index()),
      na.color = "transparent")
    leaflet() |>
      addTiles() |>
      addRasterImage(fut_ac_index(), colors = ac_pal, group = 'ac_index') |>
      addLegend(pal = ac_pal, values = values(fut_ac_index()),
       group = 'ac_index') |>
      addCircles(lng = 20, lat = -6, group = "ADM1", # added for group delete
        fill = FALSE, weight = 0) |>
      addCircles(lng = 20, lat = -6, group = "ADM0", fill = FALSE, weight = 0)
  })

  # output$ac_index_plot <- renderPlot({
  #   plot(ac_index())
  # })

  observe({
    req(fut_region_selection())
    leafletProxy("fut_map") |>
      clearGroup(group = "ADM0") |>
      addPolygons(data = fut_region_selection(), weight = .6,
        fillColor = "grey10", color = "black", group = "ADM0")
  })

  observe({
    req(input$Admin)
    leafletProxy("fut_map") |>
      clearGroup(group = "ADM1") |>
      addPolygons(data = fut_adm_region_select(), group = "ADM1",
        color = "blue", weight = .7, fillOpacity = 0.2)
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

  fut_cropped_rasters <- reactive({
    req(input$fut_AC_dim)
    paths <- usr_fut_AC_df()$path[match(input$fut_AC_dim, usr_fut_AC_df()$name)]
    ac_names <- usr_fut_AC_df()$name[match(input$fut_AC_dim, usr_fut_AC_df()$name)]
    ac_dims <- rast(paths) |>
      crop(fut_final_region(), mask = TRUE)
    #  |>
    # crop(pop_mask(), mask = TRUE)
    names(ac_dims) <- ac_names
    # ac_stack <- c(ac_dims, ac_index())
    # names(ac_stack) <- c(names(ac_dims), "ac_index")
    # return(ac_stack)
    return(ac_dims)
  })


  output$fut_init_Table <- renderDT(
    # expr = ({
    #   table_df <- st_drop_geometry(region_filled())
    #   if (length(input$Country) > 1) {
    #     sub_s_full <- sub_s_full[names(sub_s_full) %in% names(table_df)]
    #     rbind(table_df, sub_s_full)
    #   } else {
    #     table_df
    #   }
    # }),
    st_drop_geometry(fut_region_filled()),
    options = list(paging = TRUE,
                   pageLength = 5)
  )

  output$fut_bar_plot <- renderPlotly({
    req(fut_region_filled())
    region_df <- st_drop_geometry(fut_region_filled())
    if ("NAME_1" %in% names(region_df)) {
      region_df$region <- region_df$NAME_1
    } else {
      region_df$region <- region_df$NAME_0
    }
    if (is.null(input$fut_AC_dim)){
       gbarplot <- ggplot(region_df) +
        geom_bar(aes(x = region, y = vulnerability_index, fill = region),
          stat = "identity", position = "dodge") +
        labs(fill = "") +
          theme(
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 10)) +
        coord_flip()
    } else {
      sd.vars <- grep("stdev\\.", names(region_df))
      region_df <- region_df[-sd.vars]
      if (length(input$fut_Country) > 1) {
        sub_s_full <- sub_s_full[names(sub_s_full) %in% names(region_df)]
        sub_s_full$region <- sub_s_full$NAME_0
        region_df <- rbind(region_df, sub_s_full)
      }
      mean_cols <- grep("mean\\.", names(region_df))
      long_region <- tidyr::pivot_longer(region_df,
        cols = mean_cols, names_to = "Varible", values_to = "value")
      gbarplot <- ggplot(long_region) +
        geom_bar(aes(x = region, y = value, fill = region),
          stat = "identity", position = "dodge") +
        labs(fill = '') +
        theme( 
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 10)) +
          facet_wrap(~Varible, scales = "free_x") +
        coord_flip()
    }
    ggplotly(gbarplot, tooltip = c('fill', 'y'))
  })
  
  output$fut_box_plot <- renderPlotly({
    req(fut_final_region(), fut_ac_index())
    if (is.null(input$fut_AC_dim)) {
      vals <- extract(fut_ac_index(), fut_final_region())
      if ("NAME_1" %in% names(fut_final_region())) {
        vals$region <- st_drop_geometry(fut_final_region())[vals[["ID"]], "NAME_1"]
      } else {
        vals$region <- st_drop_geometry(fut_final_region())[vals[["ID"]], "NAME_0"]
      }
      vals$region <- as.factor(vals$region)
      ggbox <- ggplot(vals) +
        geom_boxplot(aes(x = region, y = vulnerabiltiy_index,
          group = region, fill = region)) +
        coord_flip()
    } else {
      vals <- extract(fut_cropped_rasters(), fut_final_region())
      if ("NAME_1" %in% names(fut_final_region())) {
        vals$region <- st_drop_geometry(fut_final_region())[vals[["ID"]], "NAME_1"]
      } else {
        vals$region <- st_drop_geometry(fut_final_region())[vals[["ID"]], "NAME_0"]
      }
      vals$region <- as.factor(vals$region)
      long_vals <- tidyr::pivot_longer(vals, cols = names(fut_cropped_rasters()),
        names_to = "layer", values_to = 'values')
      ggbox <- ggplot(long_vals) +
        geom_boxplot(aes(x = region, y = values,
          group = region, fill = region)) +
        facet_wrap(~layer, scales = "free_y") +
          coord_flip()
    }
    ggplotly(ggbox, tooltip = c("x", "y", "fill"))
  })
  output$fut_cor_plot_warning <- renderText({
    if (!isTruthy(input$fut_AC_dim)) {
      print("At least one vulnerability dimension must be selected.")
    }
  })
  output$fut_cor_plot <- renderPlot({
    req(fut_cropped_rasters())
    cor_stack <- c(fut_cropped_rasters(), fut_ac_index())
    cor_mat <- layerCor(cor_stack, "pearson", na.rm = T)[[1]] |>
      as.data.frame()
    row.names(cor_mat) <- names(cor_stack)
    names(cor_mat) <- names(cor_stack)
    long_cormat <- as.table(as.matrix(cor_mat)) |>
      as.data.frame()
    pal <- RColorBrewer::brewer.pal(10, "Spectral")
    pal <- paste0(pal, 'A6')
    ggplot(long_cormat, aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = round(Freq, 3))) +
      scale_fill_gradientn(colours = pal,
        breaks = c(-1, 0, 1),
        limits = c(-1, 1)) +
      labs(fill = "Correlation") +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20)
        )
  })

  observeEvent(input$fut_AC_dim, {
    req(input$fut_AC_dim)
    leafletProxy("fut_map") |>
      clearImages() |>
      clearControls()
  #   top_lyr <- cropped_rasters()[[grep(input$AC_dim, names(cropped_rasters()))]]
  #   pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
  #     values(top_lyr), na.color = "transparent")
  # units <- AC_df[AC_df$name == names(top_lyr), 'units']
  # leafletProxy("map") |>
  #   clearShapes() |>
  #   clearControls() |>
  #   clearImages() |>
  #       addRasterImage(top_lyr, colors = pal, opacity = 0.8) |>
  #       addLegend(position = "bottomright", pal = pal,
  #         values = values(top_lyr),
  #         title = paste(names(top_lyr), units))
  ac_dims <- c(fut_cropped_rasters(), fut_ac_index())
    for (layer in 1:nlyr(ac_dims)) {
      pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
        values(ac_dims[[layer]]), na.color = "transparent")
      lyr_name <- names(ac_dims[[layer]])
      print(lyr_name)
      print(names(fut_cropped_rasters()))
      units <- AC_df[AC_df$name == lyr_name, 'units'] 
      leafletProxy("fut_map") |>
        # clearShapes() |>
        # clearControls() |>
        addRasterImage(ac_dims[[layer]], colors = pal, opacity = 0.8,
          group = lyr_name) |>
        addLegend(position = "bottomleft", pal = pal,
          values = values(ac_dims[[layer]]),
          title = paste(lyr_name, units),
          group = lyr_name) |>
        addLayersControl(
          position = "bottomright",
          overlayGroups = c(names(ac_dims)),
          options = layersControlOptions(collapsed = FALSE)) |>
         hideGroup(names(fut_cropped_rasters())) # |>
        # addPolygons(data = region_filled(), fillColor = "transparent",
        #   color = "black", weight = .5
        # )
    }
    leafletProxy("fut_map") |>
      addPolygons(data = fut_region_filled(), fillColor = "transparent",
          color = "black", weight = .5)
  })

  output$fut_download <- downloadHandler(
    filename = function() {
      paste0("Future_Vulnerability","-", input$scenario, "_", input$proj_year,
        input$fut_downloadType)
    },
    content = function(file) {
      if (input$fut_downloadType == ".csv") {
        st_drop_geometry(fut_region_filled()) |>
          data.frame() |>
          write.csv(file)
      } else if (input$fut_downloadType == ".tif") {
        if (isTruthy(input$fut_AC_dim)) {
          c(fut_ac_index(), fut_cropped_rasters()) |>
            writeRaster(file, overwrite = TRUE)
        }
        writeRaster(fut_ac_index(), file, overwrite = TRUE)
      } else if (input$fut_downloadType == ".gpkg") {
        write_sf(fut_region_filled(), file, append = FALSE)
      } else if (input$fut_downloadType == ".shp") {
        write_sf(fut_region_filled(), file, append = FALSE)
      }
    }
  )



  # Bivariate Mapping
  output$Bivar_messages <- renderPrint({
    req(input$bivar_x, input$bivar_y)
    if (input$bivar_x == input$bivar_y) {
      print("X and Y variables cannot be the same.")
    }
  })

  bi_vars <- reactive({
    req(final_region(), input$bivar_x, input$bivar_y)
    if (input$bivar_x == "Vulnerability Index") {
      ac_rast <- ac_index()
    } else {
      ac_rast <- rast(AC_df$path[match(input$bivar_x, AC_df$name)]) |>
        crop(final_region(), mask = TRUE)
    }
    haz_rast <- rast(hazard_df$path[match(input$bivar_y, hazard_df$name)]) |>
      crop(final_region(), mask = TRUE) |>
      resample(ac_rast)
    var_stack <- c(ac_rast, haz_rast)
    return(var_stack)
    # if (input$bivar_x != input$bivar_y) {
    #   paths <- AC_df$path[match(c(input$bivar_x, input$bivar_y), AC_df$name)]
    #   var_stack <- rast(paths) |>
    #     crop(final_region(), mask = TRUE)
    # }
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
      values(bi_vars()[[1]]), na.color = "transparent")
    pal2 <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
      values(bi_vars()[[2]]), na.color = "transparent")
    bi_pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
      "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
    # the palette needs transposed to match the legend
    bi_pal <- as.vector(t(matrix(bi_pal, nrow = 3, ncol = 3)))
    leaflet() |>
      addTiles() |>
      addRasterImage(bivar_data(), group = "Bivariate Map",
        color = colorFactor(palette = bi_pal, values(bivar_data()),
          na.color = "transparent", alpha = .8)) |>
      addRasterImage(bi_vars()[[1]],
        group = input$bivar_x, opacity = 0.8, colors = pal1) |>
      addLegend(position = "bottomleft", pal = pal1, group = input$bivar_x,
        values = values(bi_vars()[[1]])) |>
      addRasterImage(bi_vars()[[2]],
        group = input$bivar_y, opacity = 0.8, colors = pal2) |>
      addLegend(position = "bottomleft", pal = pal2, group = input$bivar_y,
        values = values(bi_vars()[[2]])) |>
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Bivariate Map", input$bivar_x, input$bivar_y),
        options = layersControlOptions(collapsed = FALSE)) |>
      hideGroup(c(input$bivar_x, input$bivar_y))
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
          title = input$bivar_z, position = "bottomleft", data = symbols,
          group = "z_group")
    }
  })
  output$download_bivar <- downloadHandler(
    filename = function() {
      paste0('placeholder', '.tif')
    },
    content = function(file) {
      writeRaster(bivar_data(), file, overwrite = TRUE)
    }
  )
}
shinyApp(ui, server)
