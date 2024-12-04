
#' Visualize Seurat objects
#'
#' @inheritParams ggplot2::ggplot
#' @param data A \code{\link[SeuratObject]{Seurat}} object
#'
#' @return A ggplot object
#'
#' @aliases ggplot
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @export
#' @method ggplot Seurat
#'
ggplot.Seurat <- function(
  data = NULL,
  mapping = aes(),
  ...,
  environment = parent.frame()
) {
  p <- structure(
    .Data = list(
      data = data,
      layers = list(),
      scales = .GG$scales_list(),
      guides = .GG$guides_list(),
      mapping = mapping,
      theme = list(),
      coordinates = ggplot2::coord_cartesian(default = TRUE),
      facet = ggplot2::facet_null(),
      plot_env = environment,
      layout = ggplot2::ggproto(`_class` = NULL, `_inherit` = ggplot2::Layout)
    ),
    class = c('gg', 'ggseurat', 'ggplot')
  )
  p$labels <- .GG$make_labels(mapping = mapping)
  ggplot2::set_last_plot(value = p)
  return(p)
}

#' ggplot_build.Seurat
#'
#' @inheritParams ggplot2::ggplot_build
#'
# @inherit ggplot2::ggplot_build
#'
#' @return A built ggplot ready for viewing
#'
#' @aliases ggplot_build
#' @seealso \code{\link[ggplot2]{ggplot_build}}
#'
#' @export
#' @method ggplot_build ggseurat
#'
ggplot_build.ggseurat <- function(plot) {
  plot <- .GG$plot_clone(plot = plot)
  if (!length(x = plot$layers)) {
    plot <- plot + ggplot2::geom_blank()
  }
  facets <- NULL
  if (!inherits(x = plot$facet, what = 'FacetNull')) {
    facets <- names(x = plot$facet$params$facets)
  }
  # data <- vector(mode = 'list', length = length(x = plot$layers))
  # for (i in seq_along(along.with = data)) {
  #   vars <- unique(x = .get_aesthetics(plot = plot, layer = plot$layers[[i]], split = FALSE))
  #   data[[i]] <- SeuratObject::FetchData(object = plot$data, vars = vars)
  # }
  # data <- Reduce(
  #   f = \(x, y) merge(x = x, y = y, all = TRUE, sort = FALSE, suffixes = character(length = 2L)),
  #   x = data
  # )
  # plot$data <- data
  # return(NextMethod(generic = 'ggplot_build', object = plot))
  for (i in seq_along(along.with = plot$layers)) {
    vars <- unique(x = .get_aesthetics(
      plot = plot,
      layer = plot$layers[[i]],
      split = FALSE
    ))
    if (length(x = vars) || length(x = facets)) {
      plot$layers[[i]]$data <- plot$layers[[i]]$layer_data(SeuratObject::FetchData(
        object = plot$data,
        vars = c(vars, facets)
      ))
    }
  }
  plot$data <- ggplot2::waiver()
  return(NextMethod(generic = 'ggplot_build', object = plot))
}

ggplot_build_ggseurat3 <- function(plot) {
  plot$scales <- plot$scales$clone()
  if (!length(x = plot$layers)) {
    plot <- plot + ggplot2::geom_blank()
  }
  browser()
  layer.data <- lapply(
    X = plot$layers,
    FUN = function(l) {
      plot.data <- unique(x = .get_aesthetics(plot = plot, layer = l)$plot)
      plot.data <- SeuratObject::FetchData(object = plot$data, vars = plot.data)
      return(l$layer_data(plot.data))
    }
  )
  layout <- ggplot2::ggproto(
    `_class` = NULL,
    `_inherit` = ggplot2::Layout,
    facet = plot$facet,
    coord = plot$coordinates
  )
  layout.facets <- as.character(x = layout$facet$params$facets)
  layout.facets <- gsub(pattern = '^~', replacement = '', x = layout.facets)
  layout.data <- if (length(x = layout.facets)) {
    SeuratObject::FetchData(object = plot$data, vars = layout.facets)
  } else {
    data.frame()
  }
  browser()
}
