#' Alternating shaded background to create zebra stripe appearance
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @param trim If `TRUE` (default), stripes trimmed to axis limits. If `FALSE` (default), stripes extend to data limits
#' @param direction Options: `"horizontal"` (each stripe extends from left to right), `"vertical"` (each stripe extends from top to bottom). If `NULL`, direction guessed from variable types for x and y axis
#' @returns ggproto object
#' @export
geom_stripes <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    ...,
    show.legend = NA,
    inherit.aes = TRUE,
    trim = TRUE,
    direction = NULL) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStripes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      direction = direction,
      ...
    )
  )
}

#' Geom for alternating background zebra stripes
#'
#' @export
GeomStripes <- ggplot2::ggproto(
  "GeomStripes",
  ggplot2::Geom,
  required_aes = "x|y",
  default_aes = ggplot2::aes(
    xmin = -Inf,
    xmax = Inf,
    odd = "#22222222",
    even = "#00000000",
    # Change 'linewidth' below from 0 to NA. When not NA then when *printing in pdf device* borders are there despite requested 0th size. Seems to be some ggplot2 bug caused by grid overriding an lwd parameter somewhere, unless the linewidth is set to NA. Found solution here https://stackoverflow.com/questions/43417514/getting-rid-of-border-in-pdf-output-for-geom-label-for-ggplot2-in-r
    alpha = NA,
    colour = "black",
    linetype = "solid",
    linewidth = NA
  ),
  draw_key = ggplot2::draw_key_blank,
  setup_params = function(data, params) {
    if (is.null(params$direction)) {
      if (is.numeric(data$y) && !is.integer(utils::type.convert(data$y, as.is = TRUE))) {
        params$direction <- "vertical"
      } else {
        params$direction <- "horizontal"
      }
    }
    z <- if (params$direction == "horizontal") "y" else "x"
    params$n_breaks <- length(unique(data[[z]]))
    params
  },
  draw_panel = function(data, panel_params, coord, trim = TRUE, direction = "horizontal", n_breaks = NULL, odd = NULL, even = NULL) {
    if (direction == "horizontal") {
      axis <- "x"
      axis_min <- "xmin"
      axis_max <- "xmax"
      position <- "y.sec"
      opposite <- "y"
      opposite_min <- "ymin"
      opposite_max <- "ymax"
    } else {
      axis <- "y"
      axis_min <- "ymin"
      axis_max <- "ymax"
      position <- "x.sec"
      opposite <- "x"
      opposite_min <- "xmin"
      opposite_max <- "xmax"
    }
    z <- seq_len(n_breaks %||% attributes(panel_params[[position]]$breaks)$pos)
    data <- merge(data, vec2df(opposite = z, .col_names = opposite), by = opposite, all = TRUE, sort = FALSE)
    for (i in c(axis_min, axis_max, "odd", "even", "alpha", "colour", "linetype", "linewidth")) {
      col_values <- .subset2(data, i)
      idx_na <- is.na(col_values)
      data[[i]] <- col_values[!idx_na][1L]
      #data[[i]][idx_na] <- col_values[!idx_na][1L]
    }
    if (trim) {
      #limits <- panel_params[[axis]]$get_limits()
      limits <- range(panel_params[[axis]]$get_breaks(), na.rm = TRUE)
      data[[axis_min]] <- limits[1L]
      data[[axis_max]] <- limits[2L]
    }
    data[[opposite]] <- z <- round(data[[opposite]])
    data[[opposite_min]] <- z - 0.5
    data[[opposite_max]] <- z + 0.5
    data <- data[c("xmin", "xmax", "ymin", "ymax", "odd", "even", "alpha", "colour", "linetype", "linewidth")]
    data <- data[!duplicated(data), , drop = FALSE]
    data <- data[order(data[[opposite_min]]), , drop = FALSE]
    n_rows <- nrow(data)
    is_odd <- seq_len(n_rows) %% 2L == 1L
    if (!is.null(even)) {
      data$even[!is_odd] <- rep_len(even, length.out = sum(!is_odd))
    }
    if (!is.null(odd)) {
      data$odd[is_odd] <- rep_len(odd, length.out = sum(is_odd))
    }
    data$fill <- data$even
    data$fill[is_odd] <- data$odd[is_odd]
    data$odd <- data$even <- NULL
    data$colour <- data$fill
    ggplot2::GeomRect$draw_panel(data, panel_params, coord)
  }
)
