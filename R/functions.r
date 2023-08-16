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

bivariate_mapper <- function(data, x.val, y.val, n = 3, legend = FALSE) {
  if (legend) {
    bivar_legend(x.title = deparse(substitute(x.val)),
      y.title = deparse(substitute(y.val)),
      n = n)
  }

}