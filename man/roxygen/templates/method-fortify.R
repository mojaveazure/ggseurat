#' @title Create a Data Frame Suitable for \code{ggplot()}
#'
#' @description Generate a long-format data frame for ease of use with
#' \code{\link[ggplot2]{ggplot}()}; as a bonus, this method enables direct
#' calling of \code{\link[SeuratObject]{<%= cls %>}} objects in
#' \code{\link[ggplot2]{ggplot}()} (see examples)
#'
#' @return A long-format data frame for use with \code{\link[ggplot2]{ggplot}()}
#'
#' @seealso \code{\link[ggplot2:ggplot]{ggplot2::ggplot}()},
#' \code{\link[ggplot2:fortify]{ggplot2::fortify}()}
