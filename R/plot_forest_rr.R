#' Forrest plot
#'
#' @noRd
plot_rr <- function(
    x,
    study_design = NULL,
    age_at_vax = NULL,
    hpv_type = NULL,
    outcome = NULL,
    follow_up = NULL,
    ...,
    ratio = 0.75,
    colors = c("white", "black"),
    ordering_var = "rr_order",
    x_axis_scale = "log10",
    x_axis_title = "Risk ratio",
    x_axis_limits = NULL,
    x_axis_breaks = NULL,
    x_axis_labels = ggplot2::waiver(),
    title = NULL,
    base_size = 14,
    point_size = 5,
    point_border_thickness = 1,
    point_border_color = "black",
    vert_line_x_position = 1,
    show_het = FALSE) {
  n <- nrow(x) + 1L
  if (missing(ratio)) {
    ratio <- 0.09*n + 0.56
  }
  if (missing(base_size)) {
    base_size <- if (n >= 10) 10 else (19 - n)
  }
  if (missing(point_size)) {
    point_size <- if (n >= 10) 3.5 else if (n <= 5) 4.5 else 4
  }
  #x_axis_breaks <- x_axis_breaks %||% abers:::breaks_linear()(c(x$rr_lower, x$rr_upper))
  out <- plot_forest(
    x,
    estimate = "rr",
    lower = "rr_lower",
    upper = "rr_upper",
    y_var = "study",
    point_color_var = "is_meta",
    label_digits = 2L,
    point_color = colors,
    point_border_thickness = point_border_thickness,
    point_border_color = point_border_color,
    aspect_ratio = ratio,
    x_axis_scale = x_axis_scale,
    x_axis_title = x_axis_title,
    x_axis_limits = x_axis_limits,
    x_axis_breaks = x_axis_breaks,
    x_axis_labels = x_axis_labels,
    odd_stripe_colors = rep_len("#22222222", length.out = nrow(x)),
    even_stripe_colors = rep_len("#FFFFFF00", length.out = nrow(x)),
    y_axis_labels = NULL,
    vert_line_x_position = vert_line_x_position,
    ordering_var = ordering_var,
    base_size = base_size,
    point_size = point_size,
    ...
  )
  if (is.null(title)) {
    cols <- c("study_design", "outcome", "age_at_vax", "follow_up", "hpv_type")
    prefix <- c("Study design", "Outcome", "Age at vaccination", "Follow up duration", "HPV type")
    labels <- lapply(
      seq_along(cols),
      function(i) {
        vals <- unique(.subset2(x, cols[i]))
        if (length(vals) != 1L || anyNA(vals)) return(NULL)
        paste(prefix[i], vals, sep = ": ")
      }
    )
    labels <- labels[lengths(labels, use.names = FALSE) == 1L]
    if (length(labels) != 0L) {
      title <- paste(labels, sep = "\n")
    } else {
      title <- NULL
    }
  }
  out <- out + ggplot2::ggtitle(title)
  out <- add_column_table(
    out,
    plot_margin = ggplot2::margin(),
    right_cols = list("Risk ratio (95% CI)" = ".estimate_label"),
    left_cols = list(Study = ".y_var"),
    left_args = list(label_hjust = 1)
  )
  #return(out)
  out <- reorder_y_axis(out, dplyr::desc(.x))
  if (show_het) {
    .add_het_anno <- function(plot, data = NULL, size = 12/.pt, x_pos = -1, y_pos = c(-0.1, -0.2, -0.3, -0.4), header = NULL, long = FALSE, show_tau = FALSE) {
      data <- data %||% plot@data
      data <- data[data$is_meta, , drop = FALSE]
      label <- .format_heterogeneity_label(
        i2 = data$i_sq[1L],
        tau2 = data$tau_sq[1L],
        p = data$p[1L],
        sep = NULL
      )
      if (!show_tau) {
        label <- label[-3L]
      }
      if (long) {
        label[1L] <- header %||% "underline('Heterogeneity:')"
        label <- paste0(label, collapse = "~")
        n <- length(label)
        x_pos <- rep_len(x_pos, length.out = n)
        y_pos <- rep_len(y_pos, length.out = n)
        plot[[2L]] <- plot[[2L]] + ggplot2::annotate(
          geom = "text",
          size = size,
          x = I(x_pos), y = I(y_pos),
          label = label,
          hjust = 0, vjust = 0.5,
          parse = TRUE
        )
      } else {
        label[1L] <- header %||% "'Heterogeneity:'"
        label <- paste0(label, collapse = "~")
        n <- length(label)
        x_pos <- rep_len(x_pos, length.out = n)
        y_pos <- rep_len(y_pos, length.out = n)
        plot[[2L]] <- plot[[2L]] + ggplot2::annotate(
          geom = "text",
          size = size,
          x = I(x_pos[1L]), y = I(y_pos[1L]),
          label = label,
          hjust = 0, vjust = 0.5,
          parse = TRUE
        )
      }
      plot
    }
    out <- .add_het_anno(out, x)
  }
  out
}
