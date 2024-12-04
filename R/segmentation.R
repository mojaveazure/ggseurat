
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Methods for ggplot2-defined generics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @method autolayer Segmentation
#' @export
#'
autolayer.Segmentation <- function(
  object,
  data = NULL,
  fill = NULL,
  na.rm = TRUE,
  show.legend = NA,
  ...
) {
  # Create data frame with coordinates and extra plotting data
  rnd <- SeuratObject::RandomName()
  coords <- ggplot2::fortify(model = object, data = data, name = fill %||% rnd)
  # Attempt to find data to fill polygons by
  fill <- fill %||% setdiff(x = colnames(x = coords), y = c('x', 'y', 'cell'))
  fill <- fill[1L]
  if (is.na(x = fill)) {
    fill <- NULL
  } else if (fill == rnd) {
    show.legend <- FALSE
  }
  # Drop polygons without a fill
  if (isTRUE(x = na.rm) && !is.null(x = fill)) {
    coords <- coords[!is.na(x = coords[[fill]]), , drop = FALSE]
  }
  # Do not show a legend when filling by cells
  if (is.na(x = show.legend) && isTRUE(x = fill == 'cell')) {
    show.legend <- FALSE
  }
  # Build the layer
  return(ggplot2::geom_polygon(
    mapping = ggplot2::aes_string(x = 'y', y = 'x', group = 'cell', fill = fill),
    data = coords,
    show.legend = show.legend,
    ...
  ))
}

#' @method fortify Segmentation
#' @export
#'
fortify.Segmentation <- function(model, data, ...) {
  df <- SeuratObject::GetTissueCoordinates(object = model, full = TRUE)
  if (missing(x = data)) {
    data <- NULL
  }
  data <- .prep_plot_data(data = data, idx = lengths(x = model), ...)
  # data <- .PrepSpirula(data = data, cells = lengths(x = model), ...)
  df <- cbind(df, data)
  return(df)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
