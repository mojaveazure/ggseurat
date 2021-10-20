#' @docType package
#'
"_PACKAGE"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Get aesthetics from a layer
#'
#' @param plot A ggplot object
#' @param layer A ggplot layer
#' @param split Split the resulting vector into a list based on whether or not
#' the value is present in the layer data or the plot data
#'
#' @return A character vector of all aesthetics defined for the layer
#'
#' @keywords internal
#'
#' @noRd
#'
GetAesthetics <- function(plot, layer, split = TRUE) {
  if (!inherits(x = plot, what = 'ggplot')) {
    stop("'plot' must be a ggplot object", call. = FALSE)
  } else if (!inherits(x = layer, what = 'Layer')) {
    stop("'layer' must be a ggplot layer", call. = FALSE)
  }
  l.map <- layer$mapping
  p.map <- plot$mapping
  aesthetics <- if (layer$inherit.aes) {
    c(l.map, p.map[setdiff(x = names(x = p.map), y = names(x = l.map))])
  } else {
    l.map
  }
  aesthetics <- gsub(
    pattern = '^~',
    replacement = '',
    x = as.character(x = aesthetics)
  )
  if (split) {
    aesthetics <- split(
      x = aesthetics,
      f = sapply(
        X = aesthetics,
        FUN = function(x) {
          return(ifelse(
            test = x %in% names(x = layer$data),
            yes = 'layer',
            no = 'plot'
          ))
        }
      )
    )
  }
  return(aesthetics)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Hooks
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @importFrom SeuratObject AttachDeps
#'
.onAttach <- function(libname, pkgname) {
  AttachDeps(deps = c('SeuratObject', 'ggplot2'))
  return(invisible(x = NULL))
}
