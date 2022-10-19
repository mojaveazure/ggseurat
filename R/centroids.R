#' @include zzz.R
#' @importFrom methods as
#'
NULL

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Methods for ggplot2-defined generics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @method autolayer Centroids
#' @export
#'
autolayer.Centroids <- function(
  object,
  data = NULL,
  fill = NULL,
  full = TRUE,
  na.rm = TRUE,
  show.legend = NA,
  ...
) {
  if (isTRUE(x = full) && is.finite(x = object)) {
    return(autolayer(
      object = as(object = object, Class = 'Segmentation'),
      data = data,
      fill = fill,
      na.rm = na.rm,
      show.legend = show.legend,
      ...
    ))
  }
  # Create data frame with coordinates and extra plotting data
  rnd <- RandomName()
  coords <- fortify(model = object, data = data, name = fill %||% rnd)
  # Attempt to find data to fill points by
  fill <- fill %||% setdiff(x = colnames(x = coords), y = c('x', 'y', 'cell'))
  fill <- fill[1L]
  if (is.na(x = fill)) {
    fill <- NULL
  } else if (fill == rnd) {
    show.legend <- FALSE
  }
  # Drop points without a fill
  if (isTRUE(x = na.rm) && !is.null(x = fill)) {
    coords <- coords[!is.na(x = coords[[fill]]), , drop = FALSE]
  }
  # Build the layer
  return(geom_point(
    mapping = aes_string(x = 'y', y = 'x', fill = fill),
    data = coords,
    shape = 21L,
    show.legend = show.legend,
    ...
  ))
}

#' @method fortify Centroids
#' @export
#'
fortify.Centroids <- function(model, data = missing_arg(), ...) {
  df <- GetTissueCoordinates(object = model, full = FALSE)
  if (is_missing(x = data)) {
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
