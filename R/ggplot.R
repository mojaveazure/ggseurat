#' @include zzz.R
#'
NULL

#' Visualize Seurat objects
#'
#' @inheritParams ggplot2::ggplot
#'
#' @return A ggplot object
#'
#' @aliases ggplot
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @importFrom ggplot2 ggplot aes coord_cartesian facet_null set_last_plot
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
      scales = scales_list(),
      mapping = mapping,
      theme = list(),
      coordinates = coord_cartesian(default = TRUE),
      facet = facet_null(),
      plot_env = environment
    ),
    class = c('gg', 'ggseurat', 'ggplot')
  )
  p$labels <- make_labels(mapping = mapping)
  set_last_plot(value = p)
  return(p)
}

#' ggplot_build.Seurat
#'
#' @inheritParams ggplot2::ggplot_build
#'
#' @return A built ggplot ready for viewing
#'
#' @aliases ggplot_build
#' @seealso \code{\link[ggplot2]{ggplot_build}}
#'
#' @importFrom SeuratObject FetchData
#' @importFrom ggplot2 ggplot_build geom_blank
#'
#' @export
#' @method ggplot_build ggseurat
#'
ggplot_build.ggseurat <- function(plot) {
  plot <- plot_clone(plot = plot)
  if (length(x = plot$layers) == 0) {
    plot <- plot + geom_blank()
  }
  layers <- plot$layers
  layer.data <- lapply(
    X = layers,
    FUN = function(l) {
      plot.data <- unique(x = GetAesthetics(plot = plot, layer = l)$plot)
      plot.data <- FetchData(object = plot$data, vars = plot.data)
      return(l$layer_data(plot.data))
    }
  )
  scales <- plot$scales
  ByLayer <- function(f) {
    out <- vector(mode = "list", length = length(x = data))
    for (i in seq_along(along.with = data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    return(out)
  }
  data <- layer.data
  data <- ByLayer(f = function(l, d) l$setup_layer(d, plot))
  layout <- create_layout(facet = plot$facet, coord = plot$coordinates)
  layout.facets <- as.character(x = layout$facet$params$facets)
  layout.facets <- gsub(pattern = '^~', replacement = '', x = layout.facets)
  layout.data <- if (length(x = layout.facets) > 0) {
    FetchData(object = plot$data, vars = layout.facets)
  } else {
    data.frame()
  }
  data <- layout$setup(data, layout.data, plot$plot_env)
  data <- ByLayer(f = function(l, d) l$compute_aesthetics(d, plot))
  data <- lapply(X = data, FUN = scales_transform_df, scales = scales)
  ScaleX <- function() {
    return(scales$get_scales("x"))
  }
  ScaleY <- function() {
    return(scales$get_scales("y"))
  }
  layout$train_position(data, ScaleX(), ScaleY())
  data <- layout$map_position(data)
  data <- ByLayer(f = function(l, d) l$compute_statistic(d, layout))
  data <- ByLayer(f = function(l, d) l$map_statistic(d, plot))
  scales_add_missing(plot = plot, aesthetics = c("x", "y"), env = plot$plot_env)
  data <- ByLayer(f = function(l, d) l$compute_geom_1(d))
  data <- ByLayer(f = function(l, d) l$compute_position(d, layout))
  layout$reset_scales()
  layout$train_position(data, ScaleX(), ScaleY())
  layout$setup_panel_params()
  data <- layout$map_position(data)
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }
  data <- ByLayer(f = function(l, d) l$compute_geom_2(d))
  data <- ByLayer(f = function(l, d) l$finish_statistics(d))
  data <- layout$finish_data(data)
  return(structure(
    .Data = list(
      data = data,
      layout = layout,
      plot = plot
    ),
    class = 'ggplot_built'
  ))
}
