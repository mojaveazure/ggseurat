
theme_base <- function(
  font_size = 14L,
  font_family = '',
  line_size = 0.5,
  rel_small = 0.86,
  rel_tiny = 0.79,
  rel_large = 1.14
) {
  .NotYetImplemented()
  half_line <- font_size / 2
  small_size <- rel_small * font_size
  return(ggplot2::theme_grey(base_size = font_size, base_family = font_family) %+replace%
      ggplot2::theme())
}

dark_theme <- \(...) ggplot2::theme(
  plot.background = ggplot2::element_rect(fill = 'black'),
  panel.background = ggplot2::element_rect(fill = 'black'),
  legend.background = ggplot2::element_rect(fill = 'black'),
  legend.box.background = ggplot2::element_rect(fill = 'black', size = 0L),
  legend.key = ggplot2::element_rect(fill = 'black', size = 0L),
  strip.background = ggplot2::element_rect(fill = 'grey50', colour = NA),
  plot.title = ggplot2::element_text(
    colour = 'white',
    margin = ggplot2::margin(t = 4L, r = 4L, b = 4L, l = 4L)
  ),
  plot.subtitle = ggplot2::element_text(
    colour = 'white',
    margin = ggplot2::margin(t = 4L, r = 4L, b = 4L, l = 4L)
  ),
  axis.title = ggplot2::element_text(
    colour = 'white',
    margin = ggplot2::margin(t = 4L, r = 4L, b = 4L, l = 4L)
  ),
  axis.text = ggplot2::element_text(
    colour = 'white',
    margin = ggplot2::margin(t = 4L, r = 4L, b = 4L, l = 4L)
  ),
  legend.title = ggplot2::element_text(
    colour = 'white',
    margin = ggplot2::margin(t = 4L, r = 4L, b = 4L, l = 4L)
  ),
  legend.text = ggplot2::element_text(
    colour = 'white',
    margin = ggplot2::margin(t = 4L, r = 4L, b = 4L, l = 4L)
  ),
  strip.text = ggplot2::element_text(
    colour = 'white',
    margin = ggplot2::margin(t = 4L, r = 4L, b = 4L, l = 4L)
  ),
  axis.line.x = ggplot2::element_line(colour = 'white', size = 1L),
  axis.line.y = ggplot2::element_line(colour = 'white', size = 1L),
  panel.grid = ggplot2::element_line(size = 0L),
  panel.grid.minor = ggplot2::element_line(size = 0L),
  validate = TRUE,
  ...
)

no_axes <- function(..., keep_text = FALSE, keep_ticks = FALSE) {
  blank <- ggplot2::element_blank()
  theme <- ggplot2::theme(
    axis.line.x = blank,
    axis.line.y = blank,
    ...,
    validate = TRUE
  )
  if (!isTRUE(x = keep_text)) {
    theme <- theme + ggplot2::theme(
      axis.text.x = blank,
      axis.text.y = blank,
      axis.title.x = blank,
      axis.title.y = blank,
      ...,
      validate = TRUE
    )
  }
  if (!isTRUE(x = keep_ticks)) {
    theme <- theme + ggplot2::theme(
      axis.ticks.x = blank,
      axis.ticks.y = blank,
      ...,
      validate = TRUE
    )
  }
  return(theme)
}
