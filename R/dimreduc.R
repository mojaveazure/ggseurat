#' @include zzz.R
#' @importFrom SeuratObject Embeddings
#'
NULL

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Methods for ggplot2-defined generics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @templateVar cls DimReduc
#' @templateVar geom geom_point
#' @template method-autolayer
#'
#' @inheritParams fortify.DimReduc
#' @param object A \code{\link[SeuratObject]{DimReduc}} object
#' @param dims Dimensions for visualization
# @template param-mapping
#' @template param-dots-geom
#'
#' @details The following aesthetics are mapped automatically and overwrite
#' said aesthetics at the plot level if set:
#' \Sexpr[stage=build,results=rd]{ggseurat::.rd_ilist(c("x", "y", "color"))}
#'
#' @method autolayer DimReduc
#' @export
#'
#' @family dimreduc
#'
#' @examples
#' data("pbmc_small")
#' tsne <- pbmc_small[["tsne"]]
#' md <- FetchData(pbmc_small, vars = c("ident", "MS4A1"))
#'
#' # Generate a plot using `autolayer()`
#' ggplot() + autolayer(tsne)
#' ggplot() + autolayer(tsne, data = md)
#'
autolayer.DimReduc <- function(
  object,
  data = missing_arg(),
  dims = c(1L, 2L),
  # mapping = aes(),
  na.rm = TRUE,
  show.legend = NA,
  ...
) {
  # Generate the data frame
  coords <- fortify(model = object, data = data, na.rm = na.rm)
  # Identify the dimensions
  dims <- .prep_dims(
    dims = dims,
    cols = names(x = coords),
    key = Key(object = object)
  )
  # Identify the coloring
  color <- .prep_color(
    cols = names(x = coords),
    data_missing = is_missing(x = data),
    dims = dims
  )
  # Return the layer
  return(geom_point(
    # Use aes_string because ggplot2 sucks
    mapping = aes_string(x = dims[1L], y = dims[2L], color = color),
    data = coords,
    show.legend = show.legend,
    ...
  ))
}

#' @templateVar cls DimReduc
#' @template method-autoplot
#'
#' @inheritParams autolayer.DimReduc
#'
#' @details The following aesthetics are mapped automatically:
#' \Sexpr[stage=build,results=rd]{ggseurat::.rd_ilist(c("x", "y", "color"))}
#'
#' @method autoplot DimReduc
#' @export
#'
#' @family dimreduc
#'
#' @examples
#' data("pbmc_small")
#' tsne <- pbmc_small[["tsne"]]
#' md <- FetchData(pbmc_small, vars = c("ident", "MS4A1"))
#'
#' # Generate a plot using `autoplot()`
#' autoplot(tsne)
#' autoplot(tsne, data = md)
#'
autoplot.DimReduc <- function(
  object,
  data = missing_arg(),
  dims = c(1L, 2L),
  # mapping = aes(),
  na.rm = TRUE,
  show.legend = NA,
  ...
) {
  # Generate the data frame
  coords <- fortify(model = object, data = data, na.rm = na.rm)
  # Identify the dimensions
  dims <- .prep_dims(
    dims = dims,
    cols = names(x = coords),
    key = Key(object = object)
  )
  # Identify the coloring
  color <- .prep_color(
    cols = names(x = coords),
    data_missing = is_missing(x = data),
    dims = dims
  )
  return(
    ggplot(
      data = coords,
      mapping = aes_string(x = dims[1L], y = dims[2L], color = color)
    ) + geom_point(show.legend = show.legend, ...)
  )
}

#' @templateVar cls DimReduc
#' @template method-fortify
#'
#' @inheritParams .fortify
#' @param model A \code{\link[SeuratObject]{DimReduc}} object
#' @template param-data
#' @template param-dots-ignored
#'
#' @details \code{fortify.DimReduc()} generates a data frame based on the
#' \link[SeuratObject:Embeddings]{cell embeddings}. Automatically creates a
#' column for cell names (\dQuote{\code{cell}}). Also creates a column for
#' identity classes (\dQuote{\code{ident}}) if not present in \code{data}; the
#' default identity class is
#' \Sexpr[stage=render, results=rd]{ggseurat::.rd_ident()}; final column output
#' order is:
#' \itemize{
#'  \item \dQuote{\code{cell}}
#'  \item \dQuote{\code{ident}}
#'  \item cell embeddings
#'  \item additional meta data provided by \code{data}
#' }
#'
#' @method fortify DimReduc
#' @export
#'
#' @family dimreduc
#'
#' @examples
#' data("pbmc_small")
#' tsne <- pbmc_small[["tsne"]]
#' md <- FetchData(pbmc_small, vars = c("ident", "MS4A1"))
#'
#' # Create a data frame for `ggplot()`
#' df <- fortify(tsne, data = md)
#' head(df)
#' ggplot(df, mapping = aes(x = tSNE_1, y = tSNE_2, color = ident)) +
#'   geom_point()
#'
#' # Use a `DimReduc` directly in `ggplot()`
#' ggplot(tsne, mapping = aes(x = tSNE_1, y = tSNE_2, color = MS4A1), md) +
#'   geom_point()
#'
fortify.DimReduc <- function(model, data, na.rm = FALSE, ...) {
  # Pull the embeddings matrix
  df <- as.data.frame(x = Embeddings(object = model))
  # Add cell information
  df$cell <- row.names(x = df)
  df <- df[, c('cell', setdiff(x = names(x = df), y = 'cell')), drop = FALSE]
  # Add associated meta data
  if (is_missing(x = data)) {
    data <- NULL
  }
  data <- .prep_plot_data(
    data = data,
    idx = rle(x = df$cell),
    restricted = names(x = df),
    ...
  )
  # Assemble, clean, and return the data frame
  df <- cbind(df, data)
  df <- .fortify(df = df, na.rm = na.rm)
  return(df)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.prep_color <- function(cols, data_missing, dims) {
  # Identify the coloring
  if (isTRUE(x = data_missing)) {
    return(NULL)
  }
  return(setdiff(x = cols, c(dims, 'cell'))[1L])
}

.prep_dims <- function(dims, cols, key) {
  # Check dimensions
  if (!(is_bare_integerish(x = dims, n = 2L, finite = TRUE) && all(dims > 0))) {
    abort(
      message = "'dims' must be a vector of two positive integers",
      call = caller_env()
    )
  }
  dims <- paste0(Key(object = key, quiet = TRUE), dims)
  if (!all(dims %in% cols)) {
    abort(message = "Unable to find dimensions requested", call = caller_env())
  }
  return(dims)
}
