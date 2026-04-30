#gihub_token_set()
#devtools::install_github("abersm/HPV")
library(ggplot2)
library(patchwork)
library(HPV)

# Functions ---------------------------------------------------------------

tryElse <- function(x, otherwise = NULL) tryCatch(suppressWarnings(x), error = function(e) otherwise)

# Data --------------------------------------------------------------------

# Upload data
data_studies <- utils::read.csv(system.file("v1", "df_shiny.csv", package = "HPV"))
rownames(data_studies) <- NULL

# Create clickable link to article
#data_studies$article <- paste0('<a href="', utils::URLdecode(data_studies$link), '" target="_blank">', data_studies$article, "</a>")
#data_studies$article <- paste0('<a href="', data_studies$link, '" target="_blank">', data_studies$article, "</a>")

# Efficacy data
efficacy <- data_studies[data_studies$domain == "Efficacy", , drop = FALSE]

# Safety data
safety <- data_studies[data_studies$domain == "Safety", , drop = FALSE]

# Style --------------------------------------------------------------------
primary_color <- "#246A87"
secondary_color <- "#A63B86"
header_color <- "#EDF2F7"
header_border_color <- "#00498F"
slider_color <- primary_color
pills <- FALSE
tab_style <- paste0("p-3 border ", if (pills) "rounded ", "border-top-0 rounded-bottom")

# Data table export file button style
dt_btn_bg_color <- "white"
dt_btn_text_color <- secondary_color
dt_btn_border_color <- dt_btn_text_color

dt_btn_bg_color_hover <- dt_btn_text_color
dt_btn_text_color_hover <- dt_btn_bg_color
dt_btn_border_color_hover <- dt_btn_text_color

dt_btn_bg_color_active <- "#D29DC2"
dt_btn_text_color_active <- dt_btn_bg_color
dt_btn_border_color_active <- dt_btn_text_color

dt_btn_style <- sprintf("
  div.dt-buttons{margin-top:10px;}
  .btn-secondary.buttons-csv{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
    font-weight:bold;
    border-radius:4px;
    padding:6px 16px 6px 12px;
    display:flex;
    align-items:center;
    justify-content:center;
    transition:background 0.3s, color 0.3s;
    gap:0.5em;
  }
  .btn-secondary.buttons-csv svg{
    fill:%s !important;
    vertical-align:text-bottom;
    margin-right:6px;
  }
  .btn-secondary.buttons-csv:hover{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
  }
  .btn-secondary.buttons-csv:hover svg{fill:%s !important;}
  .btn-secondary.buttons-csv:active{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
  }
  .btn-secondary.buttons-csv:active svg{fill:%s !important;}
  .btn-secondary.buttons-excel{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
    font-weight:bold;
    border-radius:4px;
    padding:6px 16px 6px 12px;
    display:flex;
    align-items:center;
    justify-content:center;
    transition:background 0.3s, color 0.3s;
    gap:0.5em;
  }
  .btn-secondary.buttons-excel svg{
    fill:%s !important;
    vertical-align:text-bottom;
    margin-right:6px;
  }
  .btn-secondary.buttons-excel:hover{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
  }
  .btn-secondary.buttons-excel:hover svg{fill:%s !important;}
  .btn-secondary.buttons-excel:active{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
  }
  .btn-secondary.buttons-excel:active svg{fill:%s !important;}",
                        # CSV button
                        dt_btn_bg_color, dt_btn_text_color, dt_btn_border_color, dt_btn_text_color,
                        dt_btn_bg_color_hover, dt_btn_text_color_hover, dt_btn_border_color_hover, dt_btn_text_color_hover,
                        dt_btn_bg_color_active, dt_btn_text_color_active, dt_btn_border_color_active, dt_btn_text_color_active,
                        # Excel button
                        dt_btn_bg_color, dt_btn_text_color, dt_btn_border_color, dt_btn_text_color,
                        dt_btn_bg_color_hover, dt_btn_text_color_hover, dt_btn_border_color_hover, dt_btn_text_color_hover,
                        dt_btn_bg_color_active, dt_btn_text_color_active, dt_btn_border_color_active, dt_btn_text_color_active
)

# Nav tabs
nav_inactive_text_color <- primary_color
nav_inactive_text_hover_color <- nav_inactive_text_color
nav_active_bg_color <- nav_inactive_text_color
nav_active_text_color <- "white"
nav_active_border_color <- nav_active_text_color
nav_style <- sprintf("
.nav{
    --bs-nav-link-color:%s;
}
.nav-tabs{
    --bs-nav-tabs-border-width: 1px;
    --bs-nav-tabs-border-color: %s;
    --bs-nav-tabs-border-radius: 3px;
    --bs-nav-tabs-link-active-color: %s;
    --bs-nav-tabs-link-active-bg: %s;
    --bs-nav-tabs-link-active-border-color: var(--bs-nav-tabs-link-active-bg);
    border-bottom: var(--bs-nav-tabs-border-width) solid var(--bs-nav-tabs-border-color);
}
.nav-link:hover, .nav-tabs>li>a:hover, .nav-pills>li>a:hover, :where(ul.nav.navbar-nav > li)>a:hover, .nav-link:focus, .nav-tabs>li>a:focus, .nav-pills>li>a:focus, :where(ul.nav.navbar-nav > li)>a:focus{
   color:%s;
}", nav_inactive_text_color, nav_active_border_color, nav_active_text_color, nav_active_bg_color, nav_inactive_text_hover_color)

# Slider style
slider_style <- sprintf(".irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover{background-color:%s;}", slider_color)

# Popover style
popover_style <- sprintf(".popover{border-color: %s;}.popover-header{background-color: %s;color:%s;}.popover .btn-close{--bs-btn-close-opacity:1;}", header_border_color, header_border_color, "white")

# Accordion style
accordion_btn_border_color <- "white"
accordion_text_color <- "white"
accordion_btn_fill_color <- "#E9F0F3"
accordion_style <- sprintf("
  --bs-accordion-color:%s;
  --bs-accordion-border-color:%s;
  --bs-accordion-active-bg:%s;
  --bs-accordion-active-color:%s;
  --bs-accordion-btn-bg:%s;
  --bs-accordion-btn-focus-box-shadow:none;",
                           primary_color,
                           accordion_btn_border_color,
                           primary_color,
                           accordion_text_color,
                           accordion_btn_fill_color
)

# All style elements
css_style <- paste0(
  "#plot_studies:hover{cursor:pointer;}",
  popover_style,
  dt_btn_style,
  nav_style,
  slider_style
)
css_style <- shiny::tags$head(shiny::tags$style(shiny::HTML(css_style)))

# JS code
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

# UI ----------------------------------------------------------------------

# Header
cidrap_logo <- shiny::tags$img(src = "cidrap_logo.svg", height = "75px", style = "padding-top:10px;padding-bottom:10px;")
vip_text <- shiny::tags$h1("Human Papillomavirus (HPV) Vaccination", style = sprintf("color:%s;vertical-align:middle;padding-top:10px;padding-bottom:10px;font-size:36px;", secondary_color))
vip_logo <- shiny::tags$img(src = "vip_logo_white_bg.svg", height = "100px", style = "padding-top:10px;padding-bottom:10px;")
header <- shiny::headerPanel(
  shiny::fluidRow(
    shiny::column(cidrap_logo, width = 6), shiny::column(vip_logo, width = 6, align = "right"),
    style = sprintf("background:%s;border:solid 2px %s;padding:2px;align-items:center;border-radius:10px;margin:0.1px;margin-top:", header_color, header_border_color)
  ),
  windowTitle = "Vaccine Integrity Project"
)

# User input buttons
# Reset data button
reset_btn <- shiny::tags$button(
  id = "data_reset",
  type = "button",
  class = "btn rounded-pill action-button",
  #style = sprintf("width:35px;height:35px;text-align:center;padding:2px 0;font-size:20px;line-height:50%%;border-radius:30px;outline:none;background:%s;", secondary_color),
  style = sprintf("width:30px;height:30px;text-align:center;padding:2px 0;font-size:18px;line-height:50%%;border-radius:30px;outline:none;background:%s;", secondary_color),
  shiny::tags$span(shiny::icon("arrow-rotate-left", verify_fa = FALSE)),
  style = "color:white;float:left;"
)
reset_btn <- shiny::tagList(shiny::tagAppendAttributes(reset_btn, title = shiny::HTML("Reset data"), `data-bs-toggle` = "tooltip", `data-bs-html` = "true", `data-bs-placement` = "auto"))

# Study design
design_options <- HPV::popover_btn(
  icon = shiny::icon("magnifying-glass"),
  title = "Select study design",
  hover_text = "Study design",
  shiny::selectInput(
    inputId = "study_design",
    label = NULL,
    choices = c("RCT", "Cohort", "Cross-sectional", "Self-controlled case series")
  )
)

if (FALSE) {
  # Vaccine product options
  vax_options <- HPV::popover_btn(
    icon = shiny::icon("syringe"),
    title = "Select vaccine product",
    hover_text = "Vaccine product",
    shiny::selectInput(
      inputId = "vax_product",
      label = NULL,
      choices = c("All HPV vaccines", "Cervarix")
    )
  )

  # Population
  population_options <- HPV::popover_btn(
    icon = shiny::icon("person"),
    title = "Select population",
    hover_text = "Population",
    shiny::checkboxGroupInput(
      inputId = "studies_population",
      label = NULL,
      choiceNames = c("Pregnancy", "Pediatrics", "Adults", "Immunocompromised"),
      choiceValues = c("Pregnancy", "Pediatrics", "Adults", "Immunocomp."),
      selected = c("Pregnancy", "Pediatrics", "Adults", "Immunocomp.")
    )
  )

  # ROB
  rob_options <- HPV::popover_btn(
    icon = shiny::icon("award"),
    title = "Select risk of bias",
    hover_text = "Risk of bias",
    shiny::checkboxGroupInput(
      inputId = "rob",
      label = NULL,
      choices = c("Low", "Moderate", "High"),
      selected = c("Low", "Moderate", "High")
    )
  )
}

# Button group
plot_btns <- shiny::div(
  class = "btn-group btn-group",
  role = "group",
  style = "gap:2px;float:right;",
  #vax_options,
  #population_options,
  design_options
  #rob_options
)
plot_btns <- shiny::div(
  class = "btn-toolbar justify-content-between",
  role = "toolbar",
  reset_btn,
  plot_btns
)

ui <- function(request) {
  bslib::page_fluid(
    title = "Vaccine Integrity Project",
    theme = bslib::bs_theme(
      version = 5,
      base_font = c(
        # Modern system fonts (iOS 9+, macOS 10.11+)
        "-apple-system",
        # Chrome on macOS, Safari on macOS
        "BlinkMacSystemFont",
        # Windows 10+
        "Segoe UI",
        # Android 4.0+
        "Roboto",
        # Older macOS versions
        "Helvetica Neue",
        # Windows (older versions)
        "Arial",
        # Linux and other systems
        "Noto Sans",
        "Liberation Sans",
        # Fallbacks
        "sans-serif"
      ),
      heading_font = c(
        "-apple-system",
        "BlinkMacSystemFont",
        "Segoe UI",
        "Roboto",
        "Helvetica Neue",
        "Arial",
        "Noto Sans",
        "Liberation Sans",
        "sans-serif"
      ),
      bg = "#FFFFFF",
      fg = primary_color,
      primary = primary_color,
      secondary = secondary_color
    ),
    header,
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    css_style,
    js_popover,
    shiny::tags$head(shiny::tags$script(shiny::HTML("$(document).ready(function() {
  $('.btn-close').addClass('btn-close-white');
});"))),
    shiny::tabsetPanel(
      id = "tabs",

      # Efficacy tab
      shiny::tabPanel(
        title = "Efficacy",
        # Code from https://icons.getbootstrap.com/icons/bar-chart-steps/
        icon = shiny::tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
          width = "16",
          height = "16",
          fill = "currentColor",
          class = "bi bi-bar-chart-steps",
          viewbox = "0 0 16 16",
          shiny::tags$path(d = "M.5 0a.5.5 0 0 1 .5.5v15a.5.5 0 0 1-1 0V.5A.5.5 0 0 1 .5 0M2 1.5a.5.5 0 0 1 .5-.5h4a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-4a.5.5 0 0 1-.5-.5zm2 4a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-7a.5.5 0 0 1-.5-.5zm2 4a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-6a.5.5 0 0 1-.5-.5zm2 4a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-7a.5.5 0 0 1-.5-.5z")
        ),
        style = tab_style,
        metaAnalysisUI("meta")
      ),

      # Safety tab
      shiny::tabPanel(
        title = "Safety",
        icon = shiny::icon("warning"),
        style = tab_style,
        metaAnalysisUI("safety")
      ),

      # Studies tab -------------------------------------------------------------
      shiny::tabPanel(
        title = "Studies",
        icon = shiny::icon("book", verfiy_fa = FALSE),
        style = tab_style,
        bslib::card(
          bslib::layout_columns(
            bslib::card(
              style = "border-color:black;",
              bslib::card_header(
                plot_btns,
                style = "background-color:#E5E5E5;border-color:black;"
              ),
              shiny::plotOutput(outputId = "plot_studies", width = "340px", height = "400px", click = "plot_studies_click")
            ),
            bslib::card(
              style = "border-color:black;",
              DT::DTOutput("table_studies")
            )
          )
        )
      )
    )
  )
}

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  plot_data <- shiny::reactiveVal(data_studies)
  table_data <- shiny::reactiveVal(data_studies)
  shiny::observeEvent(input$data_reset, {
    shiny::updateCheckboxGroupInput(inputId = "studies_virus", selected = c("COVID", "RSV", "Influenza"))
    shiny::updateCheckboxGroupInput(inputId = "studies_population", selected = c("Pregnancy", "Pediatrics", "Adults", "Immunocomp."))
    shiny::updateCheckboxGroupInput(inputId = "rob", selected = c("Low", "Moderate", "High"))
    shiny::updateCheckboxGroupInput(inputId = "study_design", selected = c("RCT", "Case-control", "Cohort", "Observational - other"))
    plot_data(data_studies)
    table_data(data_studies)
  })

  # Update both plot data and table data
  shiny::observeEvent(c(input$studies_virus, input$studies_population, input$rob, input$study_design), {
    viruses <- input$studies_virus %||% c("COVID", "RSV", "Influenza")
    populations <- input$studies_population %||% c("Pregnancy", "Pediatrics", "Adults", "Immunocomp.")
    design <- input$study_design %||% c("RCT", "Case-control", "Cohort", "Observational - other")
    idx <- data_studies$virus %in% viruses & data_studies$population %in% populations & data_studies$study_design %in% design
    tmp_data <- data_studies[idx, ]
    plot_data(tmp_data)
    table_data(tmp_data)
  })

  # Raw data table
  output$table_studies <- DT::renderDataTable({
    tmp <- table_data()
    tmp <- tmp[unique(c("study", names(tmp)))]
    #names(tmp)[names(tmp) == "rob"] <- "Risk of bias"
    out <- DT::datatable(
      tmp,
      escape = FALSE,
      extensions = c("Buttons", "FixedColumns", "FixedHeader"),
      selection = "none",
      rownames = FALSE,
      class = "display compact cell-border",
      options = list(
        scrollX = TRUE,
        fixedHeader = FALSE,
        fixedColumns = list(leftColumns = 1),
        initComplete = DT::JS(
          sprintf("function(settings, json) {
        $('th').css('background-color', '%s');
        $('th').css('color', 'white');
      }", primary_color)
        ),
        dom = "Bfrtip",
        buttons = list(
          list(
            extend = "csv",
            text = '<i class="fa fa-file-csv"></i>',
            filename = "vip",
            titleAttr = "Export as csv file",
            title = NULL,
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "excel",
            text = '<i class="fa fa-file-excel-o"></i>',
            filename = "vip",
            titleAttr = "Export as excel file",
            title = NULL,
            exportOptions = list(modifier = list(page = "all"))
          )
        )
      )
    )
    DT::formatStyle(
      out,
      columns = seq_along(tmp),
      border = "0.5px solid #CCC",
    )
  }, server = FALSE)

  # Efficacy tab
  plot_efficacy <- metaAnalysisServer("efficacy", data = data)

  # Safety tab
  plot_safety <- metaAnalysisServer("efficacy", data = data)
}

shiny::shinyApp(ui, server, enableBookmarking = "url")
