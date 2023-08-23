library(ggplot2)
library(terra)
library(raster)
library(sf)

# bivariate functions
bivar_legend <- function(x.title, y.title, n = 3, pal = NULL) {
  if (is.null(pal) & n == 3) {
    pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
      "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
  } else if (is.null(pal) & n != 3) {
    stop("Provide a color palette. Default palette is only allowed for n = 3.")
  }
  legend_df <- data.frame(x.var = rep(c(1:3), 3),
    y.var = rep(1:3, each = 3),
    fill_col = pal)
  # unnecessary, but base r way
  # legend_matrix <- matrix(c(1:(n * n)), nrow = n, ncol = n)
  # image(legend_matrix, col = pal)
  legend <- ggplot(legend_df) +
    geom_tile(mapping = aes(
      x = x.var,
      y = y.var,
      fill = fill_col)) +
    scale_fill_identity() +
    labs(x = paste(x.title, "→"),
      y = paste(y.title, "→")) +
    theme_void() +
    theme(
      axis.title = element_text(size = 18),
      axis.title.y = element_text(angle = 90)
    ) +
    coord_fixed()
  return(legend)
}

.make_bivar_raster <- function(data, n = 3, categorical = FALSE, pal = NULL) {
  if (class(data) != "SpatRaster" | nlyr(data) != 2) {
    stop("Data must be a SpatRaster with 2 layers")
  }
  if (is.null(pal)) {
    if (n == 3) {
      pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
        "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
    } else {
      stop(paste("Default Palette is only allowed for n = 3.",
       "Provide a palette of", n*n, "colors."))
    }
  }
  if (categorical) {
    n_cats <- nrow(unique(data[[1]]))
    if (n != n_cats) {
      print(
        "n is not equal to number of categories. Defaulted number of categories"
      )
      n <- n_cats
    }
    if (n > 4) {
      print("Too many categories. The map will be hard to read")
    }
    if (n_cats != length(unique(data[[2]]))) {
      print("Categorical is TRUE, but number of categories don't match.")
      stop("Rasters must be categorical and have same number of categories.")
    }
  } else {
    quan <- global(data, quantile,
      probs = seq(0, 1, length.out = (n + 1)), na.rm = T)
    x_quans <- unname(unlist(quan[1, ]))
    data[[1]] <- classify(data[[1]], x_quans, include.lowest = TRUE)
    y_quans <- unname(unlist(quan[2, ]))
    data[[2]] <- classify(data[[2]], y_quans, include.lowest = TRUE)
    levels(data) <- rep(list(data.frame(
      ID = c(0:(n - 1)),
      group = c(1:n))), 2)
    cat_rast <- concats(data[[1]], data[[2]])
     levels(cat_rast) <- data.frame(ID = 0:(n*n - 1), pal = pal)
    plot_colors <- pal[match(unname(unlist(unique(cat_rast))), pal)]
     plot(cat_rast, col = plot_colors)
    return(c(data, cat_rast))
  }
}

.make_bivar_sf <- function(data, n = 3, x.val, y.val, pal = NULL) {
  if (is.null(pal)) {
    if (n == 3) {
      pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
        "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
    } else {
      stop(paste("Default Palette is only allowed for n = 3.",
        "Provide a palette of", n * n, "colors."))
    }
  }
  if (!class(x.val) %in% c("character", "numeric") |
    !class(y.val) %in% c("character", "numeric")) {
    stop(paste0("x.val and y.val must be numeric index or",
      "character string of x/y columns in data.")
    )
  }
  pal_dict <- data.frame(
    bivar_val = paste(rep(c(1:n), n), rep(1:n, each = n), sep = "_"),
    bivar_color = pal
  )

  x_cut <- cut(
    data[[x.val]],
    breaks = quantile(data[[x.val]],
      probs = seq(0, 1, length.out = (n + 1)), na.rm = T),
    include.lowest = TRUE,
    labels = 1:n
  )
  y_cut <- cut(
    data[[y.val]],
    breaks = quantile(data[[y.val]],
      probs = seq(0, 1, length.out = (n + 1)), na.rm = T),
    include.lowest = TRUE,
    labels = 1:n
  )
  data[[paste0(x.val, "_cat")]] <- x_cut
  data[[paste0(y.val, "_cat")]] <- y_cut
  data[["bivariate_groups"]] <- paste0(x_cut, "_", y_cut)
  data$bi_color <- pal_dict[match(data[["bivariate_groups"]], pal_dict[[1]]), 2]
  return(data)
}

make_bivariate_data <- function(data, n = 3, x.val = NULL, y.val = NULL,
    pal = NULL) {
  if (is.null(pal)) {
    if (n == 3) {
      pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
        "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
    } else {
      stop(paste("Default Palette is only allowed for n = 3.",
        "Provide a palette of", n * n, "colors."))
    }
  }
  if (any(class(data) == "SpatRaster")) {
    if (nlyr(data) == 2) {
      bivar_map <- .make_bivar_raster(data, n = n, pal = pal)[[3]]
    } else {
      stop("If using a Spat Raster, it must have 2 layers")
    }
  } else if (any(class(data) %in% c("SpatVector", "sf", "sfc"))) {
    if (is.null(x.val) | is.null(y.val)) {
      stop("Provide x and y attribute names.")
    }
    if (any(class(data) == "SpatVector")) {
      data <- st_as_sf(data)
    }
    bivar_map <- .make_bivar_sf(data, n = n, pal = pal,
      x.val = x.val, y.val = y.val)
  } else {
    stop(paste("Data must be a SpatRaster, SpatVector, or sf. Found. ",
               class(data)))
  }
}

# bivariate_plotter <- function(data, x.val, y.val, n = 3) {
#   make_bivariate_data(data)
  
#     bivar_legend(x.title = deparse(substitute(x.val)),
#       y.title = deparse(substitute(y.val)),
#       n = n)
#   }
# }


# #test data 
# data_1 <- rast("/home/bjyberg/Biodiversity_International/Adaptation_Atlas/poverty/grdi_r1r3r2_filled.tif")
# data_2 <- rast("www/Gender_Equity_hotspot_unmasked.tif")
# # data_3 <- rast("/home/bjyberg/Biodiversity_International/Adaptation_Atlas/irrigated_area.tiff")
# data <- c(data_1, data_2)
# data <- crop(data, ken, mask = T)
# map <- make_bivariate_data(data)
# unique(as.factor(raster::raster(map[[1]])[[1]]))
# values(map)
# plot(map)

# writeRaster(map, "map.tif", overwrite = T)

# levels(map)

# colorFactor(palette = c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
#           "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33"),
#         values(raster::raster(map)), na.color = "transparent", alpha = .8)

# ken <-st_read("www/GADM_adm1.gpkg",
#   query = paste("select GID_0, NAME_0, NAME_1, geom from 'GADM_adm1'",
#     "where GID_0 == 'KEN'"
#   ),  quiet = TRUE
# )
# leaflet() |>
# addRasterImage(raster::raster(map), group = "Bivariate Map",
#   color = colorFactor(palette = c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
#           "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33"),
#   values(map), na.color = "transparent", alpha = .8))
