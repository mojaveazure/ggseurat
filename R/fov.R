#' @include zzz.R
#'
NULL

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
  which <- which %||% DefaultBoundary(object = object)
  molecules <- Features(x = object) %iff% molecules
  if (!is_na(x = which) && which == 'molecules' && is.null(x = molecules)) {
    abort(message = "No molecules present, please plot a segmentation")
  }
  plot <- ggplot() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank()
    )

  # Add centroids/segmentations
  if (!is_na(x = which)) {
    plot <- plot + autolayer(
      object = object[[which]],
      data = data,
      alpha = alpha,
      ...
    )
  }
  # Add molecules
  if (!is.null(x = molecules)) {
    plot <- plot + autolayer(
      object = object[['molecules']],
      feature = molecules,
      nmols = nmols,
      seed = seed,
      size = pt.size
    ) +
      guides(color = guide_legend(override.aes = list(size = 1.5)))
  }
  return(plot)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
