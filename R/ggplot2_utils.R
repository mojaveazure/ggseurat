#' @include zzz.R
#'
NULL

#' ggplot2 utility functions
#'
#' These functions are internal functions copied from ggplot2 to maintain
#' functionality. All credit goes to the ggplot2 authors
#'
#' @importFrom ggplot2 CoordCartesian FacetNull ggproto Layout
#' @importFrom rlang as_string is_quosure quo_get_expr quo_is_symbol quo_text
#'
#' @author ggplot2 developers
#' @seealso \code{\link[ggplot2]{ggplot2-package}}
#' @keywords internal
#'
#' @noRd
#'
NULL

create_layout <- function(facet = FacetNull, coord = CoordCartesian) {
  ggproto(NULL, Layout, facet = facet, coord = coord)
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }
  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  NULL
}

make_labels <- function(mapping) {
  default_label <- function(aesthetic, mapping) {
    if (is.atomic(mapping)) {
      return(aesthetic)
    }
    mapping <- strip_dots(mapping)
    if (is_quosure(mapping) && quo_is_symbol(mapping)) {
      name <- as_string(quo_get_expr(mapping))
    }
    else {
      name <- quo_text(mapping)
      name <- gsub("\n.*$", "...", name)
    }
    name
  }
  Map(default_label, names(mapping), mapping)
}

match_calculated_aes <- "^\\.\\.([a-zA-Z._]+)\\.\\.$"

message_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  message(paste0(wrapped, collapse = "\n"))
}

new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x)))
    stop("Elements must be named", call. = FALSE)
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0)
      0
    else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n)
      next
    if (lengths[i] != 1)
      stop("Elements must equal the number of rows or 1",
           call. = FALSE)
    x[[i]] <- rep(x[[i]], n)
  }
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(n)
  x
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()
  p
}

scales_add_missing <- function(plot, aesthetics, env) {
  aesthetics <- setdiff(aesthetics, plot$scales$input())
  for (aes in aesthetics) {
    scale_name <- paste("scale", aes, "continuous", sep = "_")
    scale_f <- find_global(scale_name, env, mode = "function")
    plot$scales$add(scale_f())
  }
}

ScalesList <- ggproto(
  "ScalesList", NULL,
  scales = NULL,
  find = function(self, aesthetic) {
    vapply(self$scales, function(x) any(aesthetic %in% x$aesthetics), logical(1))
  },
  has_scale = function(self, aesthetic) {
    any(self$find(aesthetic))
  },
  add = function(self, scale) {
    if (is.null(scale)) {
      return()
    }
    prev_aes <- self$find(scale$aesthetics)
    if (any(prev_aes)) {
      # Get only the first aesthetic name in the returned vector -- it can
      # sometimes be c("x", "xmin", "xmax", ....)
      scalename <- self$scales[prev_aes][[1]]$aesthetics[1]
      message_wrap("Scale for '", scalename,
                   "' is already present. Adding another scale for '", scalename,
                   "', which will replace the existing scale.")
    }
    # Remove old scale for this aesthetic (if it exists)
    self$scales <- c(self$scales[!prev_aes], list(scale))
  },
  n = function(self) {
    length(self$scales)
  },
  input = function(self) {
    unlist(lapply(self$scales, "[[", "aesthetics"))
  },
  # This actually makes a descendant of self, which is functionally the same
  # as a actually clone for most purposes.
  clone = function(self) {
    ggproto(NULL, self, scales = lapply(self$scales, function(s) s$clone()))
  },
  non_position_scales = function(self) {
    ggproto(NULL, self, scales = self$scales[!self$find("x") & !self$find("y")])
  },
  get_scales = function(self, output) {
    scale <- self$scales[self$find(output)]
    if (length(scale) == 0) return()
    scale[[1]]
  }
)

scales_list <- function() {
  ggproto(NULL, ScalesList)
}

scales_map_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0)
    return(df)
  mapped <- unlist(lapply(scales$scales, function(scale) scale$map_df(df = df)),
                   recursive = FALSE)
  new_data_frame(c(mapped, df[setdiff(names(df), names(mapped))]))
}

scales_train_df <- function(scales, df, drop = FALSE) {
  if (empty(df) || length(scales$scales) == 0)
    return()
  lapply(scales$scales, function(scale) scale$train_df(df = df))
}

scales_transform_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0)
    return(df)
  transformed <- unlist(lapply(scales$scales, function(s) s$transform_df(df = df)),
                        recursive = FALSE)
  new_data_frame(c(transformed, df[setdiff(names(df), names(transformed))]))
}

strip_dots <- function(expr) {
  if (is.atomic(expr)) {
    expr
  }
  else if (is.name(expr)) {
    expr_ch <- as.character(expr)
    if (nchar(expr_ch) > 0) {
      as.name(gsub(match_calculated_aes, "\\1", expr_ch))
    }
    else {
      expr
    }
  }
  else if (is.call(expr)) {
    if (identical(expr[[1]], quote(stat))) {
      strip_dots(expr[[2]])
    }
    else {
      expr[-1] <- lapply(expr[-1], strip_dots)
      expr
    }
  }
  else if (is.pairlist(expr)) {
    as.pairlist(lapply(expr, strip_dots))
  }
  else if (is.list(expr)) {
    lapply(expr, strip_dots)
  }
  else {
    stop("Unknown input:", class(expr)[1])
  }
}
