#' Add x axis scale to forest plot
#'
#' @noRd
add_x_axis_forest <- function(
  p,
  scale = "log2",
  title = "log2 risk ratio",
  limits = NULL,
  breaks = NULL,
  labels = ggplot2::waiver(),
  cap = "both",
  expand_lower = 0,
  expand_upper = 0,
  include = NULL) {
  x_axis <- continuous_axis(
    axis = "x",
    scale = scale,
    title = title,
    limits = limits,
    breaks = breaks,
    labels = labels,
    include = include,
    expand_lower = expand_lower,
    expand_upper = expand_upper,
    cap = cap
  )
  p <- p + x_axis
  p
}

#' Add annotation above forest plot
#'
#' @noRd
add_forest_direction_annotation <- function(
  p,
  nudge = 0.01,
  labels = c("Favors\nvaccination", "Favors\ncontrol")) {
  is_patchwork <- inherits(p, "patchwork")
  plot <- if (is_patchwork) p[[2L]] else p
  plot_layout <- ggplot2::ggplot_build(plot)@layout
  x_trans <- plot_layout$panel_scales_x[[1L]]$trans
  x_limits <- range(plot_layout$panel_params[[1L]]$x$breaks, na.rm = TRUE)
  #x_limits <- get_plot_axis_limits(plot, axis = "x", units = "data")
  #x_limits <- get_plot_axis_breaks(plot, axis = "x", units = "data")
  x_limits <- x_trans$inverse(x_limits)
  x_min <- max(x_limits[1L], 0)
  x_max <- x_limits[2L]
  nudge <- (x_max - x_min)*nudge
  key <- legendry::key_range_manual(
    #start = c(x_min, 1 + nudge),
    #end   = c(1 - nudge, x_max),
    #start = c(0, 1 + nudge),
    #end   = c(1 - nudge, Inf),
    start = c(1 - nudge, 1 + nudge),
    end   = c(0, Inf),
    name  = labels
  )

  #key <- legendry::primitive_bracket(key)
  key <- legendry::primitive_bracket(key, oob = "squish")
  key <- ggplot2::guides(x.sec = key)
  #p[[2L]] <- p[[2L]] + ggplot2::theme(legendry.bracket = ggplot2::element_line(arrow = grid::arrow()))
  if (is_patchwork) {
    p[[2L]] <- p[[2L]] + key
  } else {
    p <- p + key
  }
  p
}

#' Choose sensible x axis breaks for risk ratio axis
#'
#' @noRd
choose_x_breaks_rr <- function(p, base = 10, min = NULL, max = NULL) {
  plot <- p[[2L]]
  data <- plot@data
  #log_rng <- log(c(data$.x_min, data$.x, data$.x_max), base = base)
  #z <- max(abs(log_rng), na.rm = TRUE)
  #breaks <- breaks_linear()(c(base^-z, base^z))
  if (min(data$.xmin, na.rm = TRUE) < 0.05) {
    c(0.01, 0.1, 0.5, 1, 5)
  } else {
    c(0.05, 0.25, 0.65, 1, 5)
  }
}

#' Clip data limits for forest plot without influencing estimate and 95% CI labels
#'
#' @noRd
clip_forest_x_limits <- function(plot, min = NULL, max = NULL, idx_patchwork = 2) {
  replace_plot_data <- function(.x, .min = NULL, .max = NULL) {
    data <- .x@data
    if (!is.null(.min)) {
      data$.x_min[!is.na(data$.x_min) & data$.x_min < .min] <- .min
      data$.x[!is.na(data$.x) & data$.x < .min] <- .min
    }
    if (!is.null(.max)) {
      data$.x_max[!is.na(data$.x_max) & data$.x_max > .max] <- .max
      data$.x[!is.na(data$.x) & data$.x > .max] <- .max
    }
    .x@data <- data
    .x
  }
  if (inherits(plot, "patchwork")) {
    idx_patchwork <- idx_patchwork %||% seq_along(plot)
    for (i in idx_patchwork) {
      plot[[i]] <- replace_plot_data(plot[[i]], .min = min, .max = max)
    }
    plot
  } else if (inherits(plot, "ggplot")) {
    replace_plot_data(plot, .min = min, .max = max)
  } else {
    lapply(plot, clip_forest_x_limits, min = min, max = max, idx_patchwork = idx_patchwork)
  }
}

#' Reorder y axis levels of forest plot
#'
#' @noRd
reorder_y_axis <- function(plot, ..., .y = c(".y_var", ".study_label"), .first = "Pooled") {
  .relevel <- function(.x) {
    for (i in intersect(.y, names(.x))) {
      .x[[i]] <- factor(.subset2(.x, i), levels = unique(c(.first, as.character(.subset2(.x, i)))))
    }
    .x
  }
  if (inherits(plot, "patchwork")) {
    for (i in seq_along(plot)) {
      plot[[i]]@data <- .relevel(dplyr::arrange(plot[[i]]@data, ...))
    }
    plot
  } else if (inherits(plot, "ggplot")) {
    plot@data <- .relevel(dplyr::arrange(plot@data, ...))
    plot
  } else {
    lapply(plot, reorder_y_axis, ..., .y = .y, .first = .first)
  }
}

#' Format text for heterogeneity label
#'
#' @noRd
.format_heterogeneity_label <- function(
    i2 = NULL,
    tau2 = NULL,
    p = NULL,
    sep = "*','~",
    tau_digits = 4,
    title = "'Heterogeneity:'",
    i2_prefix = "italic(I)^2",
    tau2_prefix = "\u03c4^2",
    p_prefix = "italic(P)",
    as_string = FALSE) {
  f <- function(.x, .prefix) sprintf("%s~`=`~'%s'", .prefix, .x)
  i2_entered <- !is.null(i2)
  if (i2_entered) {
    i2 <- f(round_up(i2, 1), i2_prefix)
  }
  tau2_entered <- !is.null(tau2)
  if (tau2_entered) {
    tau2 <- f(round_up(tau2, tau_digits), tau2_prefix)
  }
  if (!is.null(p)) {
    p <- .format_p_value(p)
    idx <- grepl("<", p, fixed = TRUE)
    p <- f(gsub("[^\\.0-9]", "", p), p_prefix)
    p[idx] <- gsub("`=`", "`<`", p[idx], fixed = TRUE)
  }
  if (as_string && !is.null(sep)) {
    if (!is.null(title)) {
      if (substr(title, 1L, 1L) %in% LETTERS) {
        title <- shQuote(title)
      }
      if (!endsWith(title, "~")) {
        title <- paste0(title, "~")
      }
    }
    if (i2_entered) {
      i2 <- paste0(i2, sep)
    }
    if (tau2_entered) {
      tau2 <- paste0(tau2, sep)
    }
    paste0(title, i2, tau2, p)
  } else {
    c(title, i2, tau2, p)
  }
}
