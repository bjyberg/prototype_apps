library(ggplot2)




#bivariate functions
bivar_legend <- function(x.title, y.title, n = 3, pal = NULL) {
  if (is.null(pal) & n == 3) {
    pal <- c("#d3d3d3", "#97c5c5", "#52b6b6", "#cd9b88",
      "#92917f", "#4f8575", "#c55a33", "#8d5430", "#3f3f33")
  } else if (is.null(pal) & n != 3) {
    stop("Provide a color palette. Default is only allowed for n = 3.")
  }
  legend_df <- data.frame(suitability = rep(c(1:3), 3),
    adpative = rep(1:3, each = 3),
    fill_col = pal)

  legend <- ggplot(legend_df) +
    geom_tile(mapping = aes(
        x = suitability,
        y = adpative,
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
      Stop(paste("Default Palette is only allowed for n = 3.",
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
      Stop("Rasters should have same number of categories.")
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
      group = c(1:n))),
    2)
    cat_rast <- concats(data[[1]], data[[2]])
    levels(cat_rast) <- pal
    # plot_colors <- pal[match(unname(unlist(unique(cat_rast))), pal)]
    # plot(cat_rast, col = plot_colors)
  }
}


make_bivariate_data <- function(data, n = 3,x.val = NULL, y.val = NULL,
 pal = NULL) {
  if (class(data) == 'SpatRaster') {
    if (nlyr(data) == 2) {
      quan <- global(data, quantile,
        probs = seq(0, 1, length.out = (n + 1)), na.rm = T)

    }


  }


  data |>
    mutate(x = data[[x.val]],
      y = data[[y.val]]) |>
    dplyr::select(x, y)



bivariate_plotter <- function(data, x.val, y.val, n = 3, legend = FALSE) {
  if (legend) {
    bivar_legend(x.title = deparse(substitute(x.val)),
      y.title = deparse(substitute(y.val)),
      n = n)
  }


}