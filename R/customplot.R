
# module function =============================================================
#' Annotate
#'
#' Arrange ggplots into grid and annotate the grid.
#' @param plotlist list of ggplots
#' @param title common title
#' @param xlab common x axis label
#' @param ylab common y axis label
#' @param xlim vector of two numeric values delimiting common x axis
#' @param ylim vector of two numeric values delimiting common y axis
#' @param ncol number of columns
#' @return annotated figure
#' @examples
#' library(customplot)
#' p1 <- scatter_plot(iris, "Sepal.Width", "Sepal.Width", color = "Species")
#' p2 <- scatter_plot(iris, "Sepal.Width", "Sepal.Length", color = "Species")
#' p3 <- scatter_plot(iris, "Sepal.Width", "Petal.Width", color = "Species")
#' p4 <- scatter_plot(iris, "Sepal.Width", "Petal.Length", color = "Species")
#' annotate(list(p1, p2, p3, p4), title = "iris", xlim = c(0, 5), ylim = 0, 8),
#' xlab = "Sepal.Width", ylab = "floating", ncol = 2)
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

# module function =============================================================
between <- function(value, min, max) {
  if (value < min) {value <- min}
  if (value > max) {value <- max}
  return(value)
}
# end function

# module function =============================================================
#' Scatter Plot
#'
#' Customized scatter ggplot
#' @param data_frame source (data.frame)
#' @param x column name (character) determining x coordinates
#' @param y column name (character) determining y coordinates
#' @param color column name (character) determining point color
#' @param shape column name (character) determining point shape
#' @param size column name (character) determining point size
#' @param label column name (character) determining point labels
#' @param lines boolean to highlight axis origin
#' @param angle_text_x angle of tics labels at x-axis; numeric (0, 180, NULL)
#' @param angle_text_y angle of tics labels at y-axis; numeric (0, 180, NULL)
#' @param ellipse boolean to show 95 percent t-distribution ellipse
#' @param point_alpha numeric between 0 and 1 determining point transparency
#' @param legend character description of legend position.
#' @param legend_hshift numeric between -1 and 1 determining legend horizontal shift
#' @param legend_vshift numeric between -1 and 1 determining legend vertical shift
#' @param legend_box multiple legend orientation; string ("horizontal",
#' "vertical")
#' @param legend_direction legend direction; string ("horizontal", "vertical")
#' @param text_overlap boolean allowing text overlap
#' @return ggplot
#' @examples
#' library(customplot)
#' # legend: "top", "left", "right", "bottom", "center", "topleft", "topright",
#' # "bottomleft", "bottomright"
#' scatter_plot(data_frame = iris, x = "Sepal.Width", y = "Sepal.Length",
#' color = "Petal.Width", size = "Petal.Length", shape = "Species",
#' label = "Species", lines = TRUE, legend = "right", xaxt = TRUE,
#' yaxt = FALSE, ellipse = FALSE, alpha = 0.5, hjust = -0.05, vjust = 0.1)
#' @export
scatter_plot <- function(data_frame, x, y, color = NULL, shape = NULL,
                         size = NULL, label = NULL, xlim = NULL, ylim = NULL,
                         lines = FALSE, legend = "topright", angle_text_x = 0,
                         angle_text_y = 0, ellipse = FALSE, point_alpha = 1, legend_hshift = 0,
                         legend_vshift = 0, text_overlap = TRUE,
                         legend_box = "vertical",
                         legend_direction = "vertical") {

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
                     angle_text_x = angle_text_x,
                     angle_text_y = angle_text_y,
                     legend_hshift = legend_hshift,
                     legend_vshift = legend_vshift,
                     legend_box = legend_box,
                     legend_direction = legend_direction)
}
# end function

# module function =============================================================
#' Control Legend Coordinates
#'
#' support function.
#' @param coordinates input legend coordinates; numeric (c(0, 0), c(1, 1))
#' @param legend_hshift legend horizontal shift; numeric (-1, 1)
#' @param legend_vshift legend vertical shift; numeric (-1, 1)
#' @return vector of two coordinates
#' @examples
#' library(customplot)
#' coords <- c(1, 1)  # topright
#' get_adjusted_coords(coords, hjust = -0.1, vjust = -0.1)
#' # -0.9 -0.9
#' get_adjusted_coords(coords, hjust = 0.3, vjust = 0.2)
#' # 1.0 1.0
#' @export
get_adjusted_coords <- function(coordinates, legend_hshift, legend_vshift) {
  c(between(coordinates[1] + legend_hshift, 0, 1),
    between(coordinates[2] + legend_vshift, 0, 1))
}
# end function

# module function =============================================================
#' Get Axis Text
#'
#' support function
#' @param angle angle of tics labels at axis; numeric (0, 180, NULL)
#' @return ggplot2::element_text, ggplot2::element_blank
#' @examples
#' library(customplot)
#' get_axis_text(TRUE)
#' # ggplot2::element_text()
#' get_axis_text(FALSE)
#' # ggplot2::element_blank()
#' @export
get_axis_text <- function(angle) {
  if (is.null(angle)) {
    ggplot2::element_blank()  # blank text
  } else {
    ggplot2::element_text(angle = angle, vjust = 0.5, hjust = 2.5)  # normal text
  }
}
# end function

# module function =============================================================
#' Get Color Scheme
#'
#' Get color scale for continuous or categorical scale
#' @param color_column a single factor or numeric column from a data.frame
#' determining the color
#' @return ggplot2::scale_color_brewer or _gradient2 respectivelly
#' @examples
#' library(customplot)
#' get_color_scheme(iris[, "Species"])
#' # ggplot2::scale_color_brewer(palette = "Set2")
#' get_color_scheme(iris[, "Sepal.Width"])
#' # ggplot2::scale_color_gradient2(low = "#00AFBB", mid = "#E7B800",
#' high = "#FC4E07", midpoint = 3.057333)
#' @export
get_color_scheme <- function(color_column) {
  if (class(color_column) == "factor") {
    ggplot2::scale_color_brewer(palette = "Set2")
    #viridis::scale_color_viridis(discrete = TRUE)
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

# module function =============================================================
#' Get Custom Theme
#'
#' Customized ggplot theme, text size 8, transparent legend with custom
#' position, presence/absence of axis text.
#' @param legend legend position; string ("top", "left", "right", "bottom",
#' "center", "topleft", "topright", "bottomleft", "bottomright")
#' @param angle_text_x angle of labels at x-axis; numeric (0, 180, NULL)
#' @param angle_text_y angle of labels at y-axis; numeric (0, 180, NULL)
#' @param legend_hshift legend horizontal shift; numeric (-1, 1)
#' @param legend_vshift legend vertical shift; numeric (-1, 1)
#' @param legend_box multiple legend orientation; string ("horizontal",
#' "vertical")
#' @param legend_direction legend direction; string ("horizontal", "vertical")
#' @return ggplot2::theme()
#' @examples
#' library(customplot)
#' p1 <- scatter_plot(iris, "Sepal.Width", "Sepal.Length")
#' p1 + get_custom_theme(legend = "topleft", xaxt = TRUE, yaxt = FALSE,
#' hjust = 0.1, vjust = -0.1)
#' @export
get_custom_theme <- function(legend, angle_text_x = FALSE, angle_text_y,
                             legend_hshift = 0,
                             legend_vshift, legend_box = "vertical",
                             legend_direction = "vertical") {

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
      axis.text.x = get_axis_text(angle_text_x),  # axis text
      axis.text.y = get_axis_text(angle_text_y),
      legend.background = transparent_rect,  # legend properties
      legend.key = transparent_rect,
      legend.justification = legend_coordinates,
      legend.position = legend_coordinates,
      legend.title = bold_text,
      legend.box = legend_box,
      legend.direction = legend_direction)

  return(custom_theme)
}
# end function

# module function =============================================================
#' Get Ellipse
#'
#' 95 percent t-distribution ellipse corresponding with point color
#' @param color_column numeric vector determining ellipse shape
#' @param ellipse boolean enabling ellipse display
#' @return ggplot2::stat_ellipse()
#' @examples
#' library(customplot)
#' get_ellipse(iris[, "Sepal.Width"], TRUE)
#' # ggplot2::stat_ellipse(level = 0.95, type = "t")
#' get_ellipse(iris[, "Sepal.Width"], FALSE)
#' # NULL
#' @export
get_ellipse <- function(color_column, ellipse) {
  if (class(color_column) == "factor" & ellipse == TRUE) {
    ggplot2::stat_ellipse(level = 0.95, type = "t")
  } else {
    NULL
  }
}
# end function

# module function =============================================================
#' Get Legend Coordinates
#'
#' Translate character description of legend position to vector of two
#' coordinates. Can be adjusted.
#' @param legend character description of legend position. Possible values:
#' "top", "left", "right", "bottom", "center", "topleft", "topright",
#' "bottomleft", "bottomright"
#' @param hjust numeric between -1 and 1 determining legend horizontal shift
#' @param vjust numeric between -1 and 1 determining legend vertical shift
#' @return vector of two numerics
#' @examples
#' library(customplot)
#' get_legend_coordinates("left", hjust = 0.1, vjust = 0.1)
#' # ggplot2::stat_ellipse(level = 0.95, type = "t")
#' get_ellipse(iris[, "Sepal.Width"], FALSE)
#' # NULL
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

# module function =============================================================
#' Get Lines
#'
#' Highlight axis x and y at their origin
#' @param lines boolean showing axis origin
#' @return list of ggplot2::geom_hline() and _vline()
#' @examples
#' library(customplot)
#' get_lines(TRUE)
#' # [[1]]
#' # mapping: xintercept = ~xintercept
#' # geom_vline: na.rm = FALSE
#' # stat_identity: na.rm = FALSE
#' # position_identity
#' # [[2]]
#' # mapping: yintercept = ~yintercept
#' # geom_hline: na.rm = FALSE
#' # stat_identity: na.rm = FALSE
#' # position_identity
#' @export
get_lines <- function(lines) {
  if (lines) {
    list(ggplot2::geom_vline(xintercept = 0, linetype = "dashed"),
         ggplot2::geom_hline(yintercept = 0, linetype = "dashed"))
  } else {
    NULL
  }
}
# end function

# module function =============================================================
#' Get Mapping
#'
#' Calculate the proportions of a ggplot
#' @param x column name (character) determining x coordinates
#' @param y column name (character) determining y coordinates
#' @param color column name (character) determining point color
#' @param shape column name (character) determining point shape
#' @param size column name (character) determining point size
#' @param label column name (character) determining point labels
#' @return ggplot2::aes()
#' @examples
#' library(customplot)
#' get_mapping(x = "Sepal.Width", y = "Sepal.Length", color = "Petal.Width",
#' size = "Petal.Length", shape = "Species", label = "Species")
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

# module function =============================================================
#' Get Label
#'
#' Positions of labels avoiding an overlap
#' @param label column name indicating label presence
#' @param overlap boolean allowing text overlap
#' @examples
#' library(customplot)
#' get_text_repel(TRUE)
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

# module function =============================================================
#' Get xlim
#'
#' Adjust an x axis proportion.
#' @param xlim vector of two numerics delimiting x axis
#' @examples
#' library(customplot)
#' x <- c(0, 22)
#' get_xlim(x)
#' @export
get_xlim <- function(xlim) {
  if (is.null(xlim)) {NULL} else {ggplot2::xlim(xlim)}
}
# end function

# module function =============================================================
#' Get ylim
#'
#' Adjust an y axis proportion.
#' @param ylim vector of two numerics delimiting y axis
#' @examples
#' library(customplot)
#' y <- c(0, 22)
#' get_xlim(y)
#' @export
get_ylim <- function(ylim) {
  if (is.null(ylim)) {NULL} else {ggplot2::ylim(ylim)}
}
# end function
