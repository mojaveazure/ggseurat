#' @importFrom rlang missing_arg
#' @importFrom SeuratObject %iff%
#' @importFrom ggplot2 %+replace% aes autolayer autoplot fortify
#' ggplot ggplot_build
#'
NULL

#' @docType package
#'
#' @keywords package
#'
"_PACKAGE"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ggplot2 Helpers
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.GG <- sapply(
  X = c('create_layout', 'make_labels', 'plot_clone', 'guides_list', 'scales_list'),
  FUN = get,
  envir = getNamespace(name = 'ggplot2'),
  simplify = FALSE,
  USE.NAMES = TRUE
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Prep Data for Plotting
#'
#' @param data Extra data for plotting
#' @param idx An \code{\link[base]{rle}} object where the values are the
#' possible index names and the lengths are the number of times those indexes
#' are present
#' @param ... Arguments passed to other methods
#'
#' @return ...
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' data("pbmc_small")
#'
.prep_plot_data <- function(data, idx, ...) {
  if (!inherits(x = idx, what = 'rle')) {
    rlang::abort("'idx' must be an 'rle' object")
  }
  UseMethod(generic = '.prep_plot_data', object = data)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Methods for ggseurat-defined generics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @method .prep_plot_data array
#' @export
#'
.prep_plot_data.array <- function(data, idx, ...) {
  .NotYetImplemented()
}

#' @rdname dot-prep_plot_data
#' @method .prep_plot_data data.frame
#' @export
#'
.prep_plot_data.data.frame <- function(data, idx, ...) {
  nn <- names(x = data)
  if (is.null(x = nn) || !all(nzchar(x = nn))) {
    rlang::abort(message = "All values of 'data' must be named")
  }
  data <- sapply(
    X = nn,
    FUN = function(x) {
      d <- data[[x]]
      names(x = d) <- row.names(x = data)
      return(.prep_plot_data(data = d, idx = idx, name = x, ...))
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  return(do.call(what = 'cbind', args = data))
}

#' @param name Name for resulting \code{\link[base]{data.frame}}
#' @param restricted A vector of names that \code{name} should not be
#'
#' @rdname dot-prep_plot_data
#' @method .prep_plot_data default
#' @export
#'
.prep_plot_data.default <- function(data, idx, name, restricted = 'cell', ...) {
  if (!(rlang::is_bare_atomic(x = data) || is.factor(x = data))) {
    rlang::abort("'data' must be an atomic vector or a factor")
  }
  name <- as.character(x = name)
  if (name %in% restricted) {
    rlang::abort(message = paste("'name' cannot be", .oxford(restricted)))
  }
  nn <- idx$values
  if (is.null(x = names(x = data))) {
    minlen <- min(sapply(X = list(data, nn), FUN = length))
    names(x = data)[1:minlen] <- nn[1:minlen]
  }
  if (anyDuplicated(x = names(x = data))) {
    ndup <- sum(duplicated(x = names(x = data)))
    rlang::warn(message = paste(
      ndup,
      ifelse(test = ndup == 1L, yes = "entry", no = "entries"),
      "duplicated, using only the first occrance"
    ))
  }
  if (!length(x = intersect(x = names(x = data), y = nn))) {
    rlang::abort("None of the entries provided are in the index")
  }
  miss <- setdiff(x = nn, y = names(x = data))
  if (length(x = miss)) {
    rlang::warn(message = paste(
      length(x = miss),
      "entries missing from provided data, filling with 'NA'"
    ))
    data[miss] <- NA
  }
  extra <- setdiff(x = names(x = data), y = nn)
  if (length(x = extra)) {
    rlang::warn(message = paste(length(x = extra), "extra entries provided, removing"))
  }
  data <- data.frame(rep.int(x = data[nn], times = idx$lengths))
  names(x = data) <- name
  if (all(idx$lengths == 1L)) {
    row.names(x = data) <- nn
  }
  return(data)
}

#' @rdname dot-prep_plot_data
#' @method .prep_plot_data list
#' @export
#'
.prep_plot_data.list <- .prep_plot_data.data.frame

#' @rdname dot-prep_plot_data
#' @method .prep_plot_data NULL
#' @export
#'
.prep_plot_data.NULL <- function(data, idx, ...) {
  df <- as.data.frame(x = matrix(nrow = sum(idx$lengths), ncol = 0L))
  row.names(x = df) <- rep.int(x = idx$values, times = idx$lengths)
  return(df)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Prepare Fortified Data for `ggplot()`
#'
#' @param df A data frame with at least one column called \dQuote{\code{cell}}
#' @param ident If \code{TRUE}, add an identity column to \code{df}
#' @template param-na-rm
#'
#' @return ...
#'
#' @keywords internal
#'
#' @export
#'
.fortify <- function(df, ident = TRUE, na.rm = FALSE) {
  if (!'cell' %in% names(x = df)) {
    rlang::abort(message = "No cell identifier in data frame")
  }
  init <- if (isTRUE(x = ident)) {
    if (!'ident' %in% names(x = df)) {
      df$ident <- factor(x = .project())
    }
    c('cell', 'ident')
  } else {
    'cell'
  }
  df <- df[, c(init, setdiff(x = names(x = df), y = init)), drop = FALSE]
  row.names(x = df) <- NULL
  if (isTRUE(x = na.rm)) {
    df <- stats::na.omit(object = df)
  }
  return(df)
}

#' Get Aesthetics From a Layer
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
#' @export
#'
#' @examples
#' data("pbmc_small")
#' p <- ggplot(pbmc_small, mapping = aes(color = ident)) +
#'   geom_point(aes(x = tSNE_1, y = tSNE_2))
#' p
#'
#' .get_aesthetics(p, p$layers[[1L]])
#'
.get_aesthetics <- function(plot, layer, split = TRUE) {
  if (!inherits(x = plot, what = 'ggplot')) {
    rlang::abort(message = "'plot' must be a ggplot object")
  } else if (!inherits(x = layer, what = 'Layer')) {
    rlang::abort(message = "'layer' must be a ggplot layer")
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
  if (isTRUE(x = split)) {
    aesthetics <- base::split(
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

#' Create a List with a Serial Comma
#'
#' @param ... A character vector to join
#' @param cnj Conjunction to use for final entry
#' @param quote Quote the entries of \code{...}; choose from:
#' \itemize{
#'  \item \dQuote{\code{single}}: regular single quotes
#'  \item \dQuote{\code{fancysingle}}: fancy single quotes
#'  \item \dQuote{\code{double}}: regular double quotes
#'  \item \dQuote{\code{fancydouble}}: fancy double quotes
#'  \item \dQuote{\code{none}}: no extra quoting
#' }
#'
#' @return \code{...} arranged into an English list with a serial comma
#' when needed
#'
#' @keywords internal
#'
#' @export
#'
#' @seealso \code{\link[base]{sQuote}}
#'
#' @examples
#' .oxford('cell')
#' .oxford('cell', 'ident')
#' .oxford('cell', 'ident', 'gene')
#'
.oxford <- function(
  ...,
  cnj = c('or', 'and'),
  quote = c('single', 'fancysingle', 'double', 'fancydouble', 'none')
) {
  x <- as.character(x = c(...))
  cnj <- rlang::arg_match(arg = cnj)
  quote <- rlang::arg_match(arg = quote)
  x <- switch(
    EXPR = quote,
    single = sQuote(x = x, q = FALSE),
    fancysingle = sQuote(x = x, q = TRUE),
    double = dQuote(x = x, q = FALSE),
    fancydouble = dQuote(x = x, q = TRUE),
    x
  )
  if (length(x = x) <= 1L) {
    return(x)
  } else if (length(x = x) == 2L) {
    return(paste(x, collapse = paste0(' ', cnj, ' ')))
  }
  return(paste(
    paste0(paste(x[1:(length(x = x) - 1L)], collapse = ', '), ','),
    cnj,
    x[length(x = x)]
  ))
}

.project <- function() {
  x <- getOption(x = 'Seurat.object.project', default = 'SeuratProject')
  if (!is.character(x = x) || length(x = x) != 1L || !nzchar(x = x) || is.na(x = x)) {
    rlang::abort(message = "'Seurat.object.project' must be a single, non-empty string")
  }
  return(x)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Hooks
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname = pkgname, obj = '%||%')
}
