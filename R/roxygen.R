#' @include zzz.R
#'
NULL

#' Rdocumentation Default Identity
#'
#' Generate an Rdocumentation-formatted string with the default identity class
#' from \pkg{SeuratObject} as determined by
#' \code{getOption("Seurat.object.project")}; the default identity class is
#' double-quoted and in a monospace font (\code{\\dQuote{\\code{}}})
#'
#' @return An Rdocumentation-formatted string containing the value of
#' \code{getOption("Seurat.object.project")}
#'
#' @templateVar fname .rd_ident
#' @template section-default-rendering
#'
#' @keywords internal documentation
#' @export
#'
#' @template example-rd-inject
#'
.rd_ident <- function() {
  return(paste0(
    '\\dQuote{\\code{',
    getOption(x = 'Seurat.object.project', default = 'SeuratProject'),
    '}}'
  ))
}

#' Rdocumentation Itemized List
#'
#' Generate an Rdocumentation itemized list
#'
#' @param x A vector of values for the list
#'
#' @return An Rdocumentation itemized list suitable for injecting into
#' documentation files with \code{\\Sexpr}
#'
#' @templateVar fname .rd_ilist
#'
#' @keywords internal documentation
#' @export
#'
#' @template example-rd-inject
#'
.rd_ilist <- function(x) {
  return(paste(
    "\\itemize{",
    paste0(' \\item \\dQuote{\\code{', x, "}}", collapse = '\n'),
    "}",
    sep = '\n'
  ))
}
