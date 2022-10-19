#' @include zzz.R
#'
NULL

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Methods for ggplot2-defined generics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @method autolayer Molecules
#' @export
#'
autolayer.Molecules <- function(
  object,
  feature,
  na.rm = TRUE,
  show.legend = NA,
  nmols = NULL,
  seed = NA_integer_,
  ...
) {
  coords <- fortify(model = object, data = feature, nmols = nmols, seed = seed)
  if (isTRUE(x = na.rm)) {
    coords <- na.omit(object = coords)
  }
  return(geom_point(
    mapping = aes_string(x = 'y', y = 'x', color = 'molecule'),
    data = coords,
    show.legend = show.legend,
    ...
  ))
}
#' @method fortify Molecules
#' @export
#'
fortify.Molecules <- function(
  model,
  data,
  nmols = NULL,
  seed = NA_integer_,
  ...
) {
  coords <- GetTissueCoordinates(object = model, features = data)
  if (!is.null(x = nmols)) {
    if (!is.na(x = seed)) {
      set.seed(seed = seed)
    }
    coords <- lapply(
      X = unique(x = coords$molecule),
      FUN = function(m) {
        df <- coords[coords$molecule == m, , drop = FALSE]
        if (nrow(x = df) > nmols) {
          idx <- sample(x = seq_len(length.out = nrow(x = df)), size = nmols)
          df <- df[idx, , drop = FALSE]
        }
        return(df)
      }
    )
    coords <- do.call(what = 'rbind', args = coords)
  }
  return(coords)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
