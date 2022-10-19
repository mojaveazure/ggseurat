#' @include zzz.R
#' @importFrom Matrix t
#' @importFrom ggplot2 geom_violin
#' @importFrom SeuratObject GetAssayData VariableFeatures
#'
NULL

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Methods for ggplot2-defined generics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @templateVar cls Assay
#' @templateVar geom geom_violin
#' @template method-autolayer
#'
#' @inheritParams fortify.Assay
#' @param object An \code{\link[SeuratObject]{Assay}} object
# @template param-mapping
#' @template param-dots-geom
#'
#' @details The following aesthetics are mapped automatically and overwrite
#' said aesthetics at the plot level if set:
#' \Sexpr[stage=build,results=rd]{ggseurat::.rd_ilist(c("x", "y", "fill"))}
#'
#' @method autolayer Assay
#' @export
#'
#' @family assay
#'
#' @examples
#' data("pbmc_small")
#' rna <- pbmc_small[["RNA"]]
#' md <- FetchData(pbmc_small, vars = "ident")
#'
#' # Generate a plot using `autolayer()`
#'
autolayer.Assay <- function(
  object,
  data = missing_arg(),
  features = NULL,
  layer = c('data', 'scale.data', 'counts'),
  # mapping = aes(),
  na.rm = TRUE,
  show.legend = NA,
  ...
) {
  .NotYetImplemented()
  # Generate the data frame
  coords <- fortify(
    model = object,
    data = data,
    features = features,
    layer = layer,
    na.rm = na.rm
  )
  # # Identify the dimensions
  # dims <- .prep_dims(
  #   dims = dims,
  #   cols = names(x = coords),
  #   key = Key(object = object)
  # )
  # # Identify the coloring
  # color <- .prep_color(
  #   cols = names(x = coords),
  #   data_missing = is_missing(x = data),
  #   dims = dims
  # )
  # Return the layer
  return(geom_violin(
    # Use aes_string because ggplot2 sucks
    mapping = aes_string(x = ''),
    data = coords,
    show.legend = show.legend,
    ...
  ))
}

#' @templateVar cls Assay
#' @template method-autoplot
#'
#' @inheritParams autolayer.Assay
#'
#' @details The following aesthetics are mapped automatically:
#' \Sexpr[stage=build,results=rd]{ggseurat::.rd_ilist(c("x", "y", "fill"))}
#'
#' @method autoplot Assay
#' @export
#'
#' @family assay
#'
#' @examples
#' data("pbmc_small")
#' rna <- pbmc_small[["RNA"]]
#' md <- FetchData(pbmc_small, vars = "ident")
#'
#' # Generate a plot using `autoplot()`
#'
autoplot.Assay <- function(
  object,
  data = missing_arg(),
  features = NULL,
  layer = c('data', 'scale.data', 'counts'),
  na.rm = TRUE,
  show.legend = NA,
  ...
) {
  .NotYetImplemented()
  coords <- fortify(model = object, data = data, na.rm = na.rm)
  # TODO: Identify the fill
  # TODO: Identify the group
  return(
    ggplot(
      data = coords,
      mapping = aes_string()
    ) + geom_violin(show.legend = show.legend, ...)
  )
}

#' @templateVar cls Assay
#' @template method-fortify
#'
#' @inheritParams .fortify
#' @param model An \code{\link[SeuratObject]{Assay}} object
#' @template param-data
#' @param features A vector of features to include in the fortified data;
#' defaults to the \link[SeuratObject:VariableFeatures]{variable features}
#' @param layer Name of layer to pull expression data for
#' @template param-dots-ignored
#'
#' @details \code{fortify.Assay()} generates a data frame based on the
#' expression data stored in \code{layer}; automatically creates a column for
#' cell names (\dQuote{\code{cell}}). Also creates creates a column for
#' identity classes (\dQuote{\code{ident}}) if not present in \code{data}; the
#' default identity class is
#' \Sexpr[stage=render, results=rd]{ggseurat::.rd_ident()}; final column output
#' order is:
#' \itemize{
#'  \item \dQuote{\code{cell}}
#'  \item \dQuote{\code{ident}}
#'  \item expression data for \code{features}
#'  \item additional meta data provided by \code{data}
#' }
#'
#' @method fortify Assay
#' @export
#'
#' @family assay
#'
#' @examples
#' data("pbmc_small")
#' rna <- pbmc_small[["RNA"]]
#' md <- FetchData(pbmc_small, vars = "ident")
#'
#' # Create a data frame for `ggplot()`
#' df <- fortify(rna, data = md)
#' head(df)
#' ggplot(df, mapping = aes(x = ident, y = PPBP, fill = ident)) +
#'   geom_violin()
#'
#' # Use an `Assay` directly in `ggplot()`
#' ggplot(rna, mapping = aes(x = ident, y = GNLY, fill = ident), md) +
#'   geom_violin()
#'
fortify.Assay <- function(
  model,
  data,
  features = NULL,
  layer = c('data', 'scale.data', 'counts'),
  na.rm = FALSE,
  ...
) {
  layer <- arg_match(arg = layer)
  # Check features
  orig <- features <- features %||% VariableFeatures(object = model)
  if (!length(x = features)) {
    orig <- features <- head(x = rownames(x = model), n = 10L)
  }
  df <- t(x = GetAssayData(object = model, slot = layer))
  features <- intersect(x = features, y = colnames(x = df))
  if (!length(x = features)) {
    abort(message = "None of the requested features found")
  } else if (length(x = features) != length(x = orig)) {
    warn(message = paste('missing features'))
  }
  # Pull the expression data for the features requested
  df <- df[, features, drop = FALSE]
  if (inherits(x = df, what = 'CsparseMatrix')) {
    df <- as.matrix(x = df)
  }
  df <- as.data.frame(x = df)
  # Add cell information
  df$cell <- row.names(x = df)
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

# .prep_fill <- function() {}
