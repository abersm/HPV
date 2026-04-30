#' UI for meta-analysis module
#'
#' @param id Input id. Must match `id` entered in `metaAnalysisServer2`
#' @param data Data frame with all analysis
#' @param sidebar_open If `TRUE` (default), sidebar is open by default
#' @param sidebar_width Width of sidebar. Default is `"30%"`
#' @param plot_resizable If `TRUE` (default), plot can be resized by draggling bottom right corner
#' @param plot_width,plot_height Default width and height of plot. Default is `"100%"` for `plot_width` and `"200px"` for `plot_height`
#' @param show_color_picker If `TRUE`, color picker is displayed above plot. Default is `FALSE`
#' @param show_legend_switch If `TRUE`, switch input is displayed to control whether plot legend is displayed. Default is `FALSE`
#' @param primary_color,secondary_color Primary and secondary color used for bootstrap theme. Default is `"#A63B86"` for `secondary_color`
#' @param slider_color,switch_color Color for sliders and switches
#' @param incrementor_button_color Color for incrementor button
#' @param accordion_btn_border_color Color for accordion buttons
#' @param accordion_btn_background_color_alpha Alpha filter for accordion buttons. Default is `0.1`
#' @param slider_color Color for slider and switch
#' @param id_export_plot_btn ID for export plot button. Default is `"export_plot_btn"`
#' @returns Enter as input to UI
metaAnalysisUI2 <- function(
    id,
    data,
    sidebar_open = FALSE,
    sidebar_width = "30%",
    plot_resizable = TRUE,
    plot_width = "100%",
    plot_height = "400px",
    show_color_picker = FALSE,
    show_legend_switch = FALSE,
    primary_color = "#246A87",
    secondary_color = "#A63B86",
    slider_color = primary_color,
    incrementor_button_color = primary_color,
    id_export_plot_btn = "export_plot_btn") {
  ns <- shiny::NS(id)
  resizer_color <- primary_color

  # Popover style
  header_border_color <- "#00498F"
  popover_style <- shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".popover{border-color: %s;}.popover-header{background-color: %s;color:%s;}.popover .btn-close{--bs-btn-close-opacity:1;}", header_border_color, header_border_color, "white"))))
  js_popover <- shiny::tags$head(
    shiny::tags$script(
      shiny::HTML(
        "$(document).ready(function () {",
        "  $('body').on('click', function (e) {",
        "    $('[data-bs-toggle=popover]').each(function () {",
        "      if (!$(this).is(e.target) &&",
        "          $(this).has(e.target).length === 0 &&",
        "          $('.popover').has(e.target).length === 0) {",
        "        $(this).popover('hide');",
        "      }",
        "    });",
        "  });",
        "})"
      )
    )
  )

  # Plot style controls in sidebar
  interactive_switch <- switchInput(
    ns("make_plot_interactive"),
    #on_color = primary_color, off_color = .clr_alpha_filter(primary_color, 0.1)
    label = "Interactive",
    value = FALSE
  )
  font_size_ui <- incrementorInput(
    inputId = ns("base_size"),
    label = shiny::tags$strong("Font size", style = sprintf("color:%s;font-weight:bolder;", incrementor_button_color)),
    button_color = incrementor_button_color,
    button_text_color = .clr_text(incrementor_button_color),
    min = 0,
    max = 40,
    value = 14,
    step = 1
  )

  # Plot export settings
  plot_width <- incrementorInput(
    inputId = ns("export_plot_width"),
    label = "Width (inches)",
    font_size = 12,
    button_color = primary_color,
    button_text_color = .clr_text(primary_color),
    value = 6, min = 1, max = 50, step = 2
  )
  plot_height <- incrementorInput(
    inputId = ns("export_plot_height"),
    label = "Height (inches)",
    font_size = 12,
    button_color = primary_color,
    button_text_color = .clr_text(primary_color),
    value = 10, min = 1, max = 50, step = 2
  )
  plot_dpi <- incrementorInput(
    inputId = ns("export_plot_dpi"),
    label = "Dots per inch (dpi)",
    font_size = 12,
    button_color = primary_color,
    button_text_color = .clr_text(primary_color),
    value = 300, min = 50, max = 1000, step = 50
  )

  # Export plot button
  export_plot_popover <- popover_btn(
    id_btn = ns(id_export_plot_btn),
    icon = shiny::icon("image", verify_fa = FALSE),
    title = "Export plot",
    hover_text = "Export plot",
    shiny::selectInput(
      inputId = ns("export_plot_filetype"),
      label = "File type",
      choices = c("png", "jpeg", "svg")
    ),
    shiny::textInput(
      inputId = ns("export_plot_filename"),
      label = "File name (without file extension)",
      value = "vip_forest_plot",
      placeholder = "Enter file name"
    ),
    plot_width,
    plot_height,
    shiny::conditionalPanel(
      condition = "input.export_plot_filetype != 'svg'",
      plot_dpi,
      ns = ns
    ),
    shiny::downloadButton(
      outputId = ns("export_plot"),
      label = "Export plot"
    )
  )

  # Export data button
  ## Code from https://icons.getbootstrap.com/icons/table/
  table_icon <- shiny::tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    width = "16",
    height = "16",
    fill = "currentColor",
    class = "bi bi-table",
    viewbox = "0 0 16 16",
    shiny::tags$path(d = "M0 2a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2zm15 2h-4v3h4zm0 4h-4v3h4zm0 4h-4v3h3a1 1 0 0 0 1-1zm-5 3v-3H6v3zm-5 0v-3H1v2a1 1 0 0 0 1 1zm-4-4h4V8H1zm0-4h4V4H1zm5-3v3h4V4zm4 4H6v3h4z")
  )
  export_data_popover <- popover_btn(
    icon = shiny::tags$i(table_icon),
    title = "Export meta-analysis data",
    hover_text = "Export data",
    shiny::selectInput(
      inputId = ns("export_data_filetype"),
      label = "File type",
      choices = c("csv", "xlsx")
    ),
    shiny::textInput(
      inputId = ns("export_data_filename"),
      label = "File name (without file extension)",
      value = "vip_meta_analysis_data",
      placeholder = "Enter file name"
    ),
    shiny::downloadButton(
      outputId = ns("export_data"),
      #class = "btn rounded-pill action-button",
      label = "Export data"
    )
  )
  # Export plot/data buttons
  export_btns <- shiny::div(
    class = "btn-group btn-group",
    role = "group",
    style = "gap:2px;float:left;",
    export_plot_popover,
    export_data_popover
  )

  # Button group
  plot_btns <- shiny::div(
    class = "btn-toolbar justify-content-between",
    #class = "btn-toolbar",
    role = "toolbar",
    #interactive_switch,
    export_btns
    #plot_btns
  )

  # Plot resizer
  css_resizer <- shiny::tags$head(
    shiny::tags$style(
      shiny::HTML(
        sprintf("
      .resizable-plot {
        position: relative;
        display: inline-block;
        min-width: 300px;
        min-height: 250px;
        max-width: 100%%;
        max-height: 80vh;
        border: 1px solid var(--bs-border-color);
        border-radius: var(--bs-border-radius);
        overflow: hidden;
      }
      .resizable-plot.resizing {
        border: 2px solid %s;
        box-shadow: 0 0 0 0.2rem rgba(var(--bs-primary-rgb), 0.25);
      }
      .resize-handle {
        position: absolute;
        background: transparent;
        z-index: 1000;
        transition: all 0.15s ease-in-out;
      }
      .resize-handle::before {
        content: '';
        position: absolute;
        background: var(--bs-border-color);
        transition: all 0.15s ease-in-out;
        opacity: 0;
      }
      .resize-handle:hover::before {
        background: %s;
        opacity: 1;
      }
      .resize-horizontal {
        right: -3px;
        top: 0;
        width: 6px;
        height: 100%%;
        cursor: ew-resize;
      }
      .resize-horizontal::before {
        left: 50%%;
        top: 20%%;
        width: 2px;
        height: 60%%;
        transform: translateX(-50%%);
        border-radius: 1px;
      }
      .resize-vertical {
        bottom: -3px;
        left: 0;
        width: 100%%;
        height: 6px;
        cursor: ns-resize;
      }
      .resize-vertical::before {
        top: 50%%;
        left: 20%%;
        width: 60%%;
        height: 2px;
        transform: translateY(-50%%);
        border-radius: 1px;
      }
      .resize-diagonal {
        right: -3px;
        bottom: -3px;
        width: 16px;
        height: 16px;
        cursor: se-resize;
      }
      .resize-diagonal::before {
        right: 2px;
        bottom: 2px;
        width: 8px;
        height: 8px;
        border-right: 2px solid;
        border-bottom: 2px solid;
        border-color: inherit;
      }
      .resize-diagonal:hover::before {
        border-color: %s;
      }
      /* Active state styling */
      .resize-handle:active::before,
      .resizing .resize-handle::before {
        background: %s;
        opacity: 1;
      }
      .resizing .resize-diagonal::before {
        border-color: %s;
      }
      /* Additional visual feedback */
      .resize-handle:hover {
        background: rgba(var(--bs-primary-rgb), 0.1);
      }
      /* Ensure handles are visible on hover */
      .resizable-plot:hover .resize-handle::before {
        opacity: 0.3;
      }
      .resizable-plot:hover .resize-handle:hover::before {
        opacity: 1;
      }
    ",
                # Plot border while dragging
                resizer_color,
                # Bottom righthand handle on hover
                resizer_color,
                # Border (bottom and right) of bottom righthand handle on hover
                resizer_color,
                # All handles while dragging
                resizer_color,
                # Border (bottom and right) of bottom righthand handle while dragging
                resizer_color
        )
      )
    )
  )
  js_resizer <- shiny::tags$script(
    shiny::HTML(sprintf("
    $(document).ready(function() {
      let isResizing = false;
      let resizeType = '';
      let startX, startY, startWidth, startHeight;
      const container = $('%s');
      // Horizontal resize
      $('.resize-horizontal').on('mousedown', function(e) {
        isResizing = true;
        resizeType = 'horizontal';
        startX = e.clientX;
        startWidth = container.width();
        container.addClass('resizing');
        $('body').addClass('user-select-none');
        e.preventDefault();
      });
      // Vertical resize
      $('.resize-vertical').on('mousedown', function(e) {
        isResizing = true;
        resizeType = 'vertical';
        startY = e.clientY;
        startHeight = container.height();
        container.addClass('resizing');
        $('body').addClass('user-select-none');
        e.preventDefault();
      });
      // Bottom righthand corner resize
      $('.resize-diagonal').on('mousedown', function(e) {
        isResizing = true;
        resizeType = 'diagonal';
        startX = e.clientX;
        startY = e.clientY;
        startWidth = container.width();
        startHeight = container.height();
        container.addClass('resizing');
        $('body').addClass('user-select-none');
        e.preventDefault();
      });
      // Mouse move
      $(document).on('mousemove', function(e) {
        if (!isResizing) return;
        if (resizeType === 'horizontal') {
          const newWidth = Math.max(300, startWidth + (e.clientX - startX));
          container.css('width', newWidth + 'px');
        } else if (resizeType === 'vertical') {
          const newHeight = Math.max(250, startHeight + (e.clientY - startY));
          container.css('height', newHeight + 'px');
        } else if (resizeType === 'diagonal') {
          const newWidth = Math.max(300, startWidth + (e.clientX - startX));
          const newHeight = Math.max(250, startHeight + (e.clientY - startY));
          container.css({
            'width': newWidth + 'px',
            'height': newHeight + 'px'
          });
        }
      });
      // Mouse up
      $(document).on('mouseup', function() {
        if (isResizing) {
          isResizing = false;
          resizeType = '';
          container.removeClass('resizing');
          $('body').removeClass('user-select-none');

          // Trigger plot resize in Shiny
          $(window).trigger('resize');
        }
      });
      // Prevent text selection while resizing
      $(document).on('selectstart', function(e) {
        if (isResizing) {
          e.preventDefault();
        }
      });
    });
  ", paste0("#", ns("plot-container")))))

  # JS to extract current plot dimensions
  js_get_plot_dim <- sprintf("
    $(document).on('click', '#%s', function() {
      const plotContainer = document.getElementById('%s');
      if (plotContainer) {
        const rect = plotContainer.getBoundingClientRect();
        const width = Math.round(rect.width);
        const height = Math.round(rect.height);
        Shiny.setInputValue('%s', width, {priority: 'event'});
        Shiny.setInputValue('%s', height, {priority: 'event'});
      }
    });", ns(id_export_plot_btn), ns("plot-container"), ns("resize_plot_width"), ns("resize_plot_height"))

  # Plot
  #plot_output <- shiny::plotOutput(outputId = ns("plot"), width = "340px", height = "400px")
  #plotly_output <- plotly::plotlyOutput(outputId = ns("plotly"), width = "340px", height = "400px")
  plot_output <- shiny::plotOutput(outputId = ns("plot"), width = "100%", height = "100%")
  #plotly_output <- plotly::plotlyOutput(outputId = ns("plotly"), width = "100%", height = "100%")

  # Plot cards
  card_title <- shiny::uiOutput(ns("analysis_title"))
  plot_card <- function(x, ...) {
    bslib::card(
      style = "border-color:black;",
      bslib::card_header(plot_btns, style = "background-color:#E5E5E5;border-color:black;"),
      card_title,
      bslib::card_body(
        shiny::div(
          id = ns("plot-container"),
          class = "resizable-plot",
          style = "width:650px;height:400px;",
          x,
          shiny::div(class = "resize-handle resize-horizontal"),
          shiny::div(class = "resize-handle resize-vertical"),
          shiny::div(class = "resize-handle resize-diagonal")
        ),
        ...
      )
    )
  }

  # Meta-analyisis table of contents
  meta_toc <- shiny::tags$button(
    id = ns("meta_toc"),
    type = "button",
    class = "btn rounded-pill action-button",
    `data-val` = NULL,
    list(shiny::icon("table-list", verify_fa = FALSE), "Table of contents"),
    style = sprintf("background-color:%s;color:%s;", secondary_color, .clr_text(secondary_color))
  )

  # Modal window style
  css_modal <- shiny::tags$head(
    shiny::tags$style(
      shiny::HTML("
      .modal-xl {
        max-width: 90% !important;
        min-width: 400px !important;
      }
      @media (min-width: 1200px) {
        .modal-xl {
          max-width: 1140px !important;
          min-width: 400px !important;
        }
      }
      @media (max-width: 450px) {
        .modal {
          overflow-x: auto !important;
        }
        .modal-dialog {
          margin: 10px !important;
          min-width: 400px !important;
          width: 400px !important;
        }
        .modal-content {
          min-width: 400px !important;
        }
      }
      .modal-body {
        min-width: 0;
      }
      .modal-body .row {
        min-width: 350px;
      }")
    )
  )

  # Analysis options
  meta_options <- dplyr::arrange(data, outcome, follow_up, hpv_type, age_at_vax, study_design)
  meta_options_names <- meta_options <- unique(meta_options$analysis_label)
  meta_options_names <- gsub(" years at vaccination", "y", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("≥ ", "≥", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("months", "m", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("Chronic fatigue syndrome/myalgic encephalomyelitis", "CFS/ME", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("Cervical screening attendance", "Cervical screening", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("self-controlled case series", "self-cntrl. series", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("Guillain-Barré syndrome", "Guillain-Barré", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("Persistent infection at 12 m with HPV 16/18", "Persistent (12 mo) HPV 16/18 infect.", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("Persistent infection at 6 m with HPV 16/18", "Persistent (6 mo) HPV 16/18 infect.", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("Invasive cervical cancer", "Cervical cancer", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("cross-sectional", "cross-sect.", meta_options_names, fixed = TRUE)
  meta_options_names <- gsub("Anogenital warts", "Anogen. warts", meta_options_names, fixed = TRUE)
  meta_options <- as.list(c("TABLE OF CONTENTS", meta_options))
  names(meta_options) <- c("TABLE OF CONTENTS", meta_options_names)

  # UI
  bslib::layout_sidebar(
    css_modal,
    shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".irs--shiny .irs-bar{border-top:1px solid %s;border-bottom:1px solid %s;background:%s;}.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single{background-color:%s;}.irs--shiny .irs-handle{background-color:%s;}", slider_color, slider_color, slider_color, slider_color, slider_color)))),
    css_resizer,
    js_resizer,
    popover_style,
    js_popover,
    shiny::tags$head(shiny::tags$script(shiny::HTML(js_get_plot_dim))),
    shiny::tags$head(shiny::tags$script(shiny::HTML("$(document).ready(function() {
  $('.btn-close').addClass('btn-close-white');
});"))),
    sidebar = bslib::sidebar(
      open = sidebar_open,
      width = sidebar_width,
      meta_toc,
      shiny::tag("hr", list(style = "border:0.5px dashed black;opacity:0.8;"), .noWS = NULL, .renderHook = NULL),
      font_size_ui
    ),
    #abers::debug_editorUI(id = ns("debug")),
    shiny::selectInput(
      inputId = ns("analysis_selected"),
      label = "Select meta-analysis",
      choices = meta_options
    ),
    plot_card(
      plot_output,
      shiny::conditionalPanel(
        condition = "input.show_heterogeneity === true",
        shiny::uiOutput(ns("het_annot")),
        ns = ns
      )
    ),
    #plot_card(plot_output),
    #shiny::conditionalPanel(
    #  condition = "input.show_heterogeneity === true",
    #  shiny::uiOutput(ns("het_annot")),
    #  ns = ns
    #),
    #shiny::tag("hr", list(style = "border:0.5px solid black;opacity:0.8;"), .noWS = NULL, .renderHook = NULL),
    switchInput(ns("show_heterogeneity"), label = shiny::tags$strong("Show heterogeneity", style = sprintf("color:%s;", primary_color)), value = FALSE, on_color = primary_color)
  )
}

#' Server for meta-analysis module
#'
#' @param meta Data frame containing raw data and meta-analysis
#' @param id_export_plot_btn ID for button to export plot. Default is `"export_plot_btn"`
#' @returns Enter inside server function of shiny app
metaAnalysisServer2 <- function(
    id,
    data,
    plotly_toolbar_buttons = "toImage",
    id_export_plot_btn = "export_plot_btn") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }
  #`%#%` <- function(x, y) if (is.null(x) || !is.numeric(x)) y else x
  meta_options <- dplyr::distinct(data, study_design, outcome, follow_up, hpv_type, age_at_vax)
  meta_options$id <- rev(seq_len(nrow(meta_options)))
  plot_meta_toc <- tidyr::pivot_longer(meta_options, cols = -"id") |>
    dplyr::mutate(
      name = dplyr::case_when(
        name == "study_design" ~ "Design",
        name == "outcome" ~ "Outcome",
        name == "age_at_vax" ~ "Age",
        name == "follow_up" ~ "Follow up",
        name == "hpv_type" ~ "HPV strain",
        .default = name
      ),
      value = gsub("years", "y", value, fixed = TRUE),
      value = gsub("months", "mo", value, fixed = TRUE),
      value = gsub(" and/or ", "/", value, fixed = TRUE),
      value = dplyr::case_when(
        value == "Persistent infection" ~ "Persistent inf.",
        value == "Cervical screening attendance" ~ "Cervical screen",
        value == "Invasive cervical cancer" ~ "Cervical cancer",
        value == "hpv_type" ~ "HPV strain",
        .default = value
      )
    )
  blank <- ggplot2::element_blank()
  plot_meta_toc <- ggplot2::ggplot(plot_meta_toc, ggplot2::aes(x = name, y = id, label = value)) +
    geom_stripes(
      #odd = rep_len(rep(c("#F1C23230", "#36689530", "#61915030"), times = c(5, 3, 2)), length.out = 95L),
      trim = FALSE
    ) +
    ggplot2::geom_text() +
    ggplot2::scale_x_discrete(position = "top") +
    theme_hpv(ratio = 0.6, base_size = 8) +
    ggplot2::theme(
      axis.title.x = blank,
      axis.title.y = blank,
      axis.text.y = blank,
      axis.text.x = ggplot2::element_text(size = 10, face = "bold"),
      axis.ticks.x = blank,
      axis.ticks.y = blank,
      axis.line.x = blank,
      axis.line.y = blank
    )
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update plot card title
    output$analysis_title <- shiny::renderUI({
      bslib::card_title(input$analysis_selected)
    })

    # Create plot
    plot_static <- shiny::reactive({
      shiny::req(input$analysis_selected)
      analysis <- input$analysis_selected
      if (analysis %in% data$analysis_label) {
        #data_all <- prepare_meta_analysis_data(
        #  data,
        #  study_design = input$study_design,
        #  age_at_vax = input$age_at_vax,
        #  hpv_type = input$hpv_type,
        #  outcome = input$outcome_long,
        #  follow_up = input$follow_up
        #)
        data_all <- data[data$analysis_label == analysis, , drop = FALSE]
        x_breaks <- if (min(data_all$rr_lower, na.rm = TRUE) < 0.05) {
          c(0.01, 0.1, 0.5, 1, 2, 5)
        } else {
          c(0.05, 0.2, 0.5, 1, 2, 5)
        }
        if (max(data_all$rr_upper, na.rm = TRUE) > 5) {
          x_breaks <- c(setdiff(x_breaks, 2), 10)
        }
        if (max(data_all$rr_upper, na.rm = TRUE) > 10) {
          x_breaks <- c(x_breaks, 100)
        }
        tryCatch({
          fp <- plot_rr(
            data_all,
            #virus = input$virus,
            #study_design = input$study_design,
            #outcome = input$outcome_long,
            #follow_up = input$follow_up,
            #hpv_type = input$hpv_type,
            #age_at_vax = input$age_at_vax,
            #show_het = input$show_heterogeneity,
            title = "",
            x_axis_scale = "log10",
            x_axis_breaks = x_breaks,
            x_axis_labels = identity,
            #aspect_ratio = input$aspect_ratio %#% NULL,
            base_size = input$base_size
          )
          add_forest_direction_annotation(fp)
        }, error = function(e) plot_meta_toc)
      } else {
        plot_meta_toc
      }
    })

    output$het_annot <- shiny::renderUI({
      shiny::req(input$analysis_selected)
      analysis <- input$analysis_selected
      if (input$show_heterogeneity && analysis %in% data$analysis_label) {
        data_all <- data[data$is_meta == 1 & data$analysis_label == analysis, , drop = FALSE]
        p <- data_all$p
        if (!startsWith(p, "<")) {
          p <- paste("=", p)
        }
        tau_sq <- data_all$tau_sq
        if (!startsWith(tau_sq, "<")) {
          tau_sq <- paste("=", tau_sq)
        }
        shiny::div(
          shiny::tags$p(
            shiny::tags$b("Heterogeneity:"),
            shiny::tags$br(),
            shiny::tags$i("I"), shiny::tags$sup("2"), paste0(" = ", round_up(data_all$i_sq, 0), "%"),
            shiny::tags$br(),
            shiny::tags$i("τ"), shiny::tags$sup("2"), paste0(" ", tau_sq),
            shiny::tags$br(),
            shiny::tags$i("P"), paste0(" ", p)
          )
        )
      } else {
        NULL
      }
    })

    # Show table of contents
    shiny::observeEvent(input$meta_toc, {
      shiny::showModal(shiny::modalDialog(shiny::plotOutput(ns("plot_toc")), size = "xl", easyClose = TRUE, title = "Meta-analyses options"))
    })
    output$plot_toc <- shiny::renderPlot({
      plot_meta_toc
    })

    # Plot output
    ## Static plot (non-interactive)
    output$plot <- shiny::renderPlot({
      plot_static()
    })
    ## Interactive plot
    #output$plotly <- plotly::renderPlotly({
    #  shiny::req(inherits(plot_static(), "ggplot"))
    #  out <- plot_interactive(
    #    .plot = plot_static(),
    #    .toolbar_buttons = plotly_toolbar_buttons,
    #    .x_axis_title = NULL,
    #    .y_axis_title = NULL,
    #    .x_axis_position = "top"
    #  )
    #  plotly::style(
    #    out,
    #    #traces = which(!vapply(out$x$data, function(x) any(names(x) == "fill"), logical(1), USE.NAMES = FALSE))
    #    hoverinfo = "skip"
    #  )
    #})

    # Update tooltip for switch to display interactive plot
    #shiny::observeEvent(input$make_plot_interactive, {
    #  text <- if (input$make_plot_interactive) "Make plot static (non-interactive)" else "Make plot interactive"
    #  bslib::update_tooltip(id = "tooltip_interactive_switch", text)
    #})
    #abers::debug_editorServer()

    # Update default inputs for export plot width/height with current plot size at the time the export button is clicked
    shiny::observeEvent(c(input$resize_plot_width, input$resize_plot_height), {
      new_width <- input$resize_plot_width/72
      new_height <- input$resize_plot_height/72
      new_width <- round_up(new_width, 2L)
      new_height <- round_up(new_height, 2L)
      updateIncrementor(inputId = "export_plot_width", value = new_width)
      updateIncrementor(inputId = "export_plot_height", value = new_height)
    })

    # Export forest plot
    # Download handler for data
    output$export_data <- shiny::downloadHandler(
      filename = function() {
        shiny::req(input$export_data_filename)
        base_name <- if(input$export_data_filename == "") "vip_meta_analysis_data" else input$export_data_filename
        paste0(base_name, ".", input$export_data_filetype)
      },
      content = function(file) {
        out <- prepare_meta_analysis_data(
          data,
          study_design = input$study_design,
          age_at_vax = input$age_at_vax,
          hpv_type = input$hpv_type,
          outcome = input$outcome,
          follow_up = input$follow_up
        )
        if (input$export_data_filetype == "csv") {
          utils::write.csv(out, file, row.names = FALSE)
        } else {
          openxlsx::buildWorkbook(x = list(Sheet_1 = out), keepNA = FALSE, na.string = "NA", firstActiveCol = 3, firstActiveRow = 2, withFilter = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "bold", valign = "center"))
          openxlsx::modifyBaseFont(wb = workbook, fontSize = 12, fontColour = "black", fontName = "Arial")
          openxlsx::saveWorkbook(wb = workbook, file = file)
        }
      }
    )

    # Export forest plot
    output$export_plot <- shiny::downloadHandler(
      filename = function() {
        shiny::req(input$export_plot_filename)
        base_name <- if(input$export_plot_filename == "") "vip_forest_plot" else input$export_plot_filename
        paste0(base_name, ".", input$export_plot_filetype)
      },
      content = function(file) {
        if (input$export_plot_filetype == "svg") {
          ggplot2::ggsave(file, plot = plot_static(), device = "svg", width = input$export_plot_width, height = input$export_plot_height)
        } else {
          ggplot2::ggsave(file, plot = plot_static(), device = input$export_plot_filetype, width = input$export_plot_width, height = input$export_plot_height, dpi = input$export_plot_dpi)
        }
      }
    )

    # Output
    shiny::reactive(
      list(plot = plot_static)
    )
  })
}
