
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Methods for ggplot2-defined generics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @method autoplot FOV
#' @export
#'
autoplot.FOV <- function(
  object,
  which = NULL,
  data = NULL,
  alpha = NA,
  molecules = NULL,
  pt.size = 1.5,
  nmols = NULL,
  seed = NA,
  ...
) {
  which <- which %||% SeuratObject::DefaultBoundary(object = object)
  molecules <- SeuratObject::Features(x = object) %iff% molecules
  if (!rlang::is_na(x = which) && which == 'molecules' && is.null(x = molecules)) {
    rlang::abort(message = "No molecules present, please plot a segmentation")
  }
  plot <- ggplot2::ggplot() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )

  # Add centroids/segmentations
  if (!rlang::is_na(x = which)) {
    plot <- plot + ggplot2::autolayer(
      object = object[[which]],
      data = data,
      alpha = alpha,
      ...
    )
  }
  # Add molecules
  if (!is.null(x = molecules)) {
    plot <- plot + ggplot2::autolayer(
      object = object[['molecules']],
      feature = molecules,
      nmols = nmols,
      seed = seed,
      size = pt.size
    ) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1.5)))
  }
  return(plot)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
