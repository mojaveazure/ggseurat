#' @docType package
#'
"_PACKAGE"

#' Attach Required Packages
#'
#' Helper function to attach required packages. Detects if a package is already
#' attached and if so, skips it. Should be called in \code{\link[base]{.onAttach}}
#'
#' @param deps A character vector of packages to attach
#'
#' @return Invisibly returns \code{NULL}
#'
#' @keywords internal
#'
#' @noRd
#'
AttachDeps <- function(deps) {
  for (d in deps) {
    if (!paste0('package:', d) %in% search()) {
      packageStartupMessage("Attaching ", d)
      attachNamespace(ns = d)
    }
  }
  return(invisible(x = NULL))
}

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

.onAttach <- function(libname, pkgname) {
  AttachDeps(deps = c('SeuratObject', 'ggplot2'))
}
