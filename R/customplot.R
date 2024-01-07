
# public function =============================================================
#' Annotate
#'
#' Arranges the ggplots into a grid and annotate it. The number of the grid
#' columns is user-defined. The number of the rows are recalculated based on
#' the plotlist length.
#' @param plotlist a list of the ggplots
#' @param title a title of an annotated figure; character string
#' @param xlab x-axis label common for all plots in the grid; character string
#' @param ylab y-axis label common for all plots in the grid; character string
#' @param xlim a two-element numeric vector delimiting x-axis; numeric vector
#' @param ylim a two-element numeric vector delimiting y-axis; numeric vector
#' @param ncol a number of the columns in the grid; integer
#' @return the ggplots arranged into an annotated grid
#' @examples
#' library(customplot)
#' p1 <- scatter_plot(iris, "Sepal.Width", "Sepal.Width", color = "Species")
#' p2 <- scatter_plot(iris, "Sepal.Width", "Sepal.Length", color = "Species")
#' p3 <- scatter_plot(iris, "Sepal.Width", "Petal.Width", color = "Species")
#' p4 <- scatter_plot(iris, "Sepal.Width", "Petal.Length", color = "Species")
#' annotate(plotlist = list(p1, p2, p3, p4),
#'          title = "iris",
#'          xlim = c(0.5, 5.1),
#'          ylim = -0.4, 8.3),
#'          xlab = "Sepal.Width",
#'          ylab = "floating",
#'          ncol = 2)
#' @export
annotate <- function(plotlist, title = NULL, xlim = NULL, ylim = NULL,
                     xlab = NULL, ylab = NULL, ncol = 1) {
  for (i in 1:length(plotlist)) {
    if (!is.null(xlim)) {plotlist[[i]] <- plotlist[[i]] + ggplot2::xlim(xlim)}
    if (!is.null(ylim)) {plotlist[[i]] <- plotlist[[i]] + ggplot2::ylim(ylim)}
  }

  nrow <- ceiling(length(plotlist)/ncol)
  figure <- ggpubr::ggarrange(plotlist = plotlist, ncol = ncol, nrow = nrow)
  figure <- ggpubr::annotate_figure(figure,
                                    bottom = ggpubr::text_grob(xlab, size = 10),
                                    left = ggpubr::text_grob(ylab, size = 10,
                                                             rot = 90),
                                    fig.lab = title, fig.lab.face = "bold")
  return(figure)
}
# end function

# internal function ===========================================================
#' Between
#'
#' Returns a numeric value delimited by an maxima and minima. If the input
#' value is higher than maxima the function returns maxima. If it is lower than
#' minima the result is minima. Otherwise the input value is retained.
#' @param value input numeric value
#' @param min lower limit
#' @param max upper limit
#' @keywords internal
#' @export
between <- function(value, min, max) {
  if (value < min) {value <- min}
  if (value > max) {value <- max}
  return(value)
}
# end function

# public function =============================================================
#' Scatter Plot
#'
#' Returns a figure of a customized scatter ggplot.
#' @param data_frame source data frame
#' @param x name of the column containing x-coordinates; character string.
#' The column must be factor or numeric.
#' @param y name of the column containing y-coordinates; character string.
#' The column must be factor or numeric.
#' @param color name of the column for point color values; character string.
#' The column must be factor or numeric.
#' @param shape name of the column for point shape values; character string.
#' The column must be factor.
#' @param size name of the column for point size values; character string.
#' The column must be numeric.
#' @param label name of the column for point labels; character string. The
#' column must be character.
#' @param lines if TRUE then origin is highlighted by dashed x- and y-lines
#' @param x_text_direction axis text x direction; "horizontal", "vertical",
#' "oblique", NULL
#' @param y_text_direction axis text y direction; "horizontal", "vertical",
#' "oblique", NULL
#' @param ellipse if TRUE and color column is factor then 95-percent
#' t-distribution ellipses appear around the color groups.
#' @param point_alpha point transparency; numeric value between 0
#' (fully transparent) and 1 (fully solid).
#' @param legend position of a legend; "top", "bottom", "left", "right",
#' "topleft", "bottomleft", "topright", "bottomright", "center"
#' @param legend_hshift legend horizontal shift; numeric value between 0
#' (very left) and 1 (very right).
#' @param legend_vshift legend vertical shift; numeric value between 0
#' (very bottom) and 1 (very top).
#' @param legend_box orientation of multiple legend (if more than one point
#' proportion is defined); "horizontal", "vertical"
#' @param legend_direction direction of legend keys; "horizontal", "vertical"
#' @param legend_key_size legend key size in text lines; numeric value
#' @param text_overlap if FALSE then point label overlapping is avoided. This
#' action can take more computation time.
#' @return customized ggplot
#' @examples
#' library(customplot)
#' scatter_plot(data_frame = iris, x = "Sepal.Width", y = "Sepal.Length",
#' color = "Petal.Width", size = "Petal.Length", shape = "Species",
#' label = "Species", lines = TRUE, legend = "right", point_alpha = 0.5,
#' legend_hshift = -0.05, legend_vshift = 0.1)
#' @export
scatter_plot <- function(data_frame, x, y, color = NULL, shape = NULL,
                         size = NULL, label = NULL, xlim = NULL, ylim = NULL,
                         lines = FALSE, legend = "topright",
                         x_text_direction = "horizontal",
                         y_text_direction = "horizontal", ellipse = FALSE,
                         point_alpha = 1, legend_hshift = 0,
                         legend_vshift = 0, text_overlap = TRUE,
                         legend_box = "vertical",
                         legend_direction = "vertical",
                         legend_key_size = 1) {

  mapping <- get_mapping(x, y, color, shape, size, label)
  ggplot2::ggplot(data_frame, mapping) +
    get_lines(lines)[[1]] +
    get_lines(lines)[[2]] +
    ggplot2::geom_point(alpha = point_alpha) +
    get_label(label, text_overlap) +
    get_xlim(xlim) +
    get_ylim(ylim) +
    ggplot2::labs(x = NULL, y = NULL) +
    get_ellipse(data_frame[, color], ellipse) +
    get_color_scheme(data_frame[, color]) +
    get_custom_theme(legend = legend,
                     x_text_direction = x_text_direction,
                     y_text_direction = y_text_direction,
                     legend_hshift = legend_hshift,
                     legend_vshift = legend_vshift,
                     legend_box = legend_box,
                     legend_direction = legend_direction,
                     legend_key_size = legend_key_size)
}
# end function

# internal function ===========================================================
#' Get Adjusted Coordinates
#'
#' Returns an two-element numeric vector of the legend coordinates
#' @param coordinates two-element numeric vector of input coordinates
#' @param legend_hshift horizontal shift; numeric value
#' @param legend_vshift vertical shift; numeric value
#' @keywords internal
#' @export
get_adjusted_coords <- function(coordinates, legend_hshift, legend_vshift) {
  c(between(coordinates[1] + legend_hshift, 0, 1),
    between(coordinates[2] + legend_vshift, 0, 1))
}
# end function

# internal function============================================================
#' Get Axis Text
#'
#' Returns the text at ggplot axis
#' @param direction direction of the text; "horizontal", "vertical", "oblique",
#'  NULL
#' @keywords internal
#' @export
get_axis_text <- function(direction) {
  if (is.null(direction)) {
    ggplot2::element_blank()
  } else if (direction == "vertical") {
    ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
  } else if (direction == "oblique") {
    ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
  } else if (direction == "horizontal") {
    ggplot2::element_text()
  } else {
    stop("get_axis_text: direction is not defined")
  }
}
# end function

# internal function ===========================================================
#' Get Color Scheme
#'
#' Returns a gg color scheme based on input vector class.
#' @param color_column input vector. If class is factor then color scheme is
#' 6-point brewer. If class is numeric then color scheme is continuous
#' three-point gradient with mean value in the middle.
#' @keywords internal
#' @export
get_color_scheme <- function(color_column) {
  if (class(color_column) == "factor") {
    ggplot2::scale_color_brewer(palette = "Set2")
  } else if (class(color_column) == "numeric") {
    ggplot2::scale_color_gradient2(low = "#00AFBB",
                                   mid = "#E7B800",
                                   high = "#FC4E07",
                                   midpoint = mean(color_column))
  } else {
    NULL
  }
}
# end function

# internal function ===========================================================
#' Get Custom Theme
#'
#' Returns gg theme
#' @param legend position of a legend; "top", "bottom", "left", "right",
#' "topleft", "bottomleft", "topright", "bottomright", "center"
#' @param legend_hshift legend horizontal shift; numeric value between 0
#' (very left) and 1 (very right).
#' @param legend_vshift legend vertical shift; numeric value between 0
#' (very bottom) and 1 (very top).
#' @param legend_box orientation of multiple legend (if more than one point
#' proportion is defined); "horizontal", "vertical"
#' @param legend_direction direction of legend keys; "horizontal", "vertical"
#' @param legend_key_size legend key size in text lines; numeric value
#' @param x_text_direction axis text x direction; "horizontal", "vertical",
#' "oblique", NULL
#' @param y_text_direction axis text y direction; "horizontal", "vertical",
#' "oblique", NULL
#' @keywords internal
#' @export
get_custom_theme <- function(legend, legend_hshift, legend_vshift, legend_box,
                             legend_direction, legend_key_size,
                             x_text_direction, y_text_direction) {

  # plot elements
  bold_text <- ggplot2::element_text(face = "bold")
  small_text <- ggplot2::element_text(size = 8)
  transparent_rect <- ggplot2::element_rect(fill = "transparent")
  legend_coordinates <- get_legend_coordinates(legend,
                                               legend_hshift,
                                               legend_vshift)

  # custom theme
  custom_theme <- ggplot2::theme_bw() +  # black and white background
    ggplot2::theme(
      text = small_text,  # plot text
      axis.text.x = get_axis_text(x_text_direction),  # axis text
      axis.text.y = get_axis_text(y_text_direction),
      legend.background = transparent_rect,  # legend properties
      legend.key = transparent_rect,
      legend.justification = legend_coordinates,
      legend.position = legend_coordinates,
      legend.title = bold_text,
      legend.box = legend_box,
      legend.direction = legend_direction,
      legend.key.size = ggplot2::unit(legend_key_size, "lines"))

  return(custom_theme)
}
# end function

# internal function ===========================================================
#' Get Ellipse
#'
#' If color_column is factor and ellipse is TRUE, the function get_ellipse
#' returns 95 percent t-distrubution ellipse around the color point group.
#' @param input_factor an input factor
#' @param allowed if TRUE then the ellipse appears
#' @keywords internal
#' @export
get_ellipse <- function(input_factor, allowed) {
  if (class(input_factor) == "factor" & allowed == TRUE) {
    ggplot2::stat_ellipse(level = 0.95, type = "t")
  } else {
    NULL
  }
}
# end function

# internal function =============================================================
#' Get Legend Coordinates
#'
#' Returns two-element numeric vector of legend coordinates.
#' @param legend position of a legend; "top", "bottom", "left", "right",
#' "topleft", "bottomleft", "topright", "bottomright", "center"
#' @param hjust legend horizontal shift; numeric value between 0 (very left)
#' and 1 (very right).
#' @param vjust legend vertical shift; numeric value between 0 (very bottom)
#' and 1 (very top).
#' @keywords internal
#' @export
get_legend_coordinates <- function(legend, hjust, vjust) {
  enum <- list("bottomleft" = get_adjusted_coords(c(0, 0), hjust, vjust),
               "bottom" = get_adjusted_coords(c(0.5, 0), hjust, vjust),
               "bottomright" = get_adjusted_coords(c(1, 0), hjust, vjust),
               "left" = get_adjusted_coords(c(0, 0.5), hjust, vjust),
               "topleft" = get_adjusted_coords(c(0, 1), hjust, vjust),
               "top" = get_adjusted_coords(c(0.5, 1), hjust, vjust),
               "topright" = get_adjusted_coords(c(1, 1), hjust, vjust),
               "right" = get_adjusted_coords(c(1, 0.5), hjust, vjust),
               "center" = get_adjusted_coords(c(0.5, 0.5), hjust, vjust))
  return(enum[[legend]])
}
# end function

# internal function ===========================================================
#' Get Lines
#'
#' Returns list of gg dashed lines at origin of x- and y-axis.
#' @param allowed if TRUE then the lines are retained in the output list
#' @keywords internal
#' @export
get_lines <- function(allowed) {
  if (allowed) {
    list(ggplot2::geom_vline(xintercept = 0, linetype = "dashed"),
         ggplot2::geom_hline(yintercept = 0, linetype = "dashed"))
  } else {
    NULL
  }
}
# end function

# internal function ===========================================================
#' Get Mapping
#'
#' Returns gg aes() containing the proportions of a ggplot.
#' @param x name of the column containing x-coordinates; character string.
#' The column must be factor or numeric.
#' @param y name of the column containing y-coordinates; character string.
#' The column must be factor or numeric.
#' @param color name of the column for point color values; character string.
#' The column must be factor or numeric.
#' @param shape name of the column for point shape values; character string.
#' The column must be numeric.
#' @param size name of the column for point size values; character string.
#' The column must be numeric.
#' @param label name of the column for point labels; character string. The
#' column must be numeric.
#' @keywords internal
#' @export
get_mapping <- function(x, y, color, shape, size, label) {

  blank <- is.null(color) & is.null(shape) & is.null(size) & is.null(label)
  co <- !is.null(color) & is.null(shape) & is.null(size) & is.null(label)
  sh <- is.null(color) & !is.null(shape) & is.null(size) & is.null(label)
  si <- is.null(color) & is.null(shape) & !is.null(size) & is.null(label)
  la <- is.null(color) & is.null(shape) & is.null(size) & !is.null(label)
  cosh <- !is.null(color) & !is.null(shape) & is.null(size) & is.null(label)
  cosi <- !is.null(color) & is.null(shape) & !is.null(size) & is.null(label)
  cola <- !is.null(color) & is.null(shape) & is.null(size) & !is.null(label)
  shsi <- is.null(color) & !is.null(shape) & !is.null(size) & is.null(label)
  shla <- is.null(color) & !is.null(shape) & is.null(size) & !is.null(label)
  sila <- is.null(color) & is.null(shape) & !is.null(size) & !is.null(label)
  coshsi <- !is.null(color) & !is.null(shape) & !is.null(size) & is.null(label)
  coshla <- !is.null(color) & !is.null(shape) & is.null(size) & !is.null(label)
  cosila <- !is.null(color) & is.null(shape) & !is.null(size) & !is.null(label)
  shsila <- is.null(color) & !is.null(shape) & !is.null(size) & !is.null(label)
  coshsila <- !is.null(color) & !is.null(shape) & !is.null(size) & !is.null(label)

  if (blank) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]])

  } else if (co) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 color = .data[[color]])

  } else if (sh) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 shape = .data[[shape]])

  } else if (si) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 size = .data[[size]])

  } else if (la) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 label = .data[[label]])

  } else if (cosh) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 color = .data[[color]],
                 shape = .data[[shape]])
  } else if (cosi) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 color = .data[[color]],
                 size = .data[[size]])
  } else if (cola) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 color = .data[[color]],
                 label = .data[[label]])
  } else if (shsi) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 shape = .data[[shape]], size = .data[[size]])
  } else if (sila) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 size = .data[[size]], label = .data[[label]])
  } else if (coshsi) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 color = .data[[color]],
                 shape = .data[[shape]], size = .data[[size]])
  } else if (coshla) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 color = .data[[color]],
                 shape = .data[[shape]], label = .data[[label]])
  } else if (cosila) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 color = .data[[color]],
                 size = .data[[size]], label = .data[[label]])
  } else if (shsila) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 shape = .data[[shape]], size = .data[[size]],
                 label = .data[[label]])
  } else if (coshsila) {
    ggplot2::aes(x = .data[[x]], y = .data[[y]],
                 color = .data[[color]],
                 shape = .data[[shape]], size = .data[[size]],
                 label = .data[[label]])
  } else {
    NULL
  }
}
# end function

# internal function ===========================================================
#' Get Label
#'
#' Returns gg label element.
#' @param label name of the column for point labels; character string. The
#' column must be numeric.
#' @param overlap if FALSE then point label overlapping is avoided. This
#' action can take more computation time.
#' @keywords internal
#' @export
get_label <- function(label, overlap) {
  if (is.null(label)) {
    NULL
  } else {
    if (overlap) {
      ggplot2::geom_text()
    } else {
      ggrepel::geom_text_repel(max.overlaps = Inf, size = 3)
    }
  }
}
# end function

# internal function ===========================================================
#' Get xlim
#'
#' Returns gg xlim
#' @param xlim two-element numeric vector delimiting x-axis
#' @keywords internal
#' @export
get_xlim <- function(xlim) {
  if (is.null(xlim)) {NULL} else {ggplot2::xlim(xlim)}
}
# end function

# module function =============================================================
#' Get ylim
#'
#' Returns gg ylim
#' @param ylim two-element numeric vector delimiting y-axis
#' @keywords internal
#' @export
get_ylim <- function(ylim) {
  if (is.null(ylim)) {NULL} else {ggplot2::ylim(ylim)}
}
# end function
