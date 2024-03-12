#' main_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_page_ui <- function(id){
  ns <- NS(id)
  ui <-
    tagList(
      shinydashboardPlus::dashboardPage(
        options = list(sidebarExpandOnHover = FALSE),
        header = main_page_header(ns),
        sidebar = main_page_sidebar(ns),
        body = main_page_body(ns),
        controlbar = NULL,
        skin = "blue"
      )
    )
  return(ui)
}

#' main_page Server Functions
#'
#' @noRd
mod_main_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_tab_basic_plot_server("tab_basic_plot")

    mod_tab_venn_upset_server("tab_venn_upset")

    mod_subtab_is_calibration_server("is_calibration")
  })
}

# utilities ----

#' Main page header
#'
#' @noRd
#'
#' @importFrom shinydashboardPlus dashboardHeader
main_page_header <- function(ns) {
  shinydashboardPlus::dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "湖州申科 shenkebio"),
      img(src = "www/HZSKBIO.svg")
    ),
    fixed = TRUE
  )
}

#' Main page sidebar
#'
#' @noRd
#'
#' @importFrom shiny icon
#' @importFrom shinydashboard sidebarMenu menuItem menuSubItem
#' @importFrom shinydashboardPlus dashboardSidebar
main_page_sidebar <- function(ns) {
  shinydashboardPlus::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        text = i18n("Interactive ggplot"),
        tabName = "basic_plot",
        expandedName = "basic_plot_expand",
        newtab = FALSE,
        selected = TRUE,
        badgeLabel = NULL,
        badgeColor = "green",
        icon = icon("chart-line", class = "fa-solid", lib = "font-awesome")
      ),
      shinydashboard::menuItem(
        text = i18n("Venn diagram"),
        tabName = "venn_upset",
        expandedName = "venn_upset_expand",
        newtab = FALSE,
        selected = FALSE,
        badgeLabel = NULL,
        badgeColor = "green",
        icon = icon("hand-spock", class = "fa-solid", lib = "font-awesome")
      ),
      shinydashboard::menuItem(
        text = i18n("Utilities"),
        tabName = "utilities",
        expandedName = "utilities_expand",
        newtab = FALSE,
        selected = FALSE,
        badgeLabel = NULL,
        badgeColor = "green",
        icon = icon("toolbox", class = "fa-solid", lib = "font-awesome"),
        shinydashboard::menuSubItem(
          text = i18n("IS Calibration"),
          tabName = "is_calibration",
          newtab = FALSE,
          selected = FALSE,
          icon = icon("calculator", class = "fa-solid", lib = "font-awesome")
        )
      )
    ),
    id = "main_page_sidebar",
    collapsed = FALSE,
    minified = TRUE
  )
}

#' Main page body
#'
#' @noRd
#'
#' @importFrom shinydashboard dashboardBody tabItems
main_page_body <- function(ns) {
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      mod_tab_basic_plot_ui(ns("tab_basic_plot")),
      mod_tab_venn_upset_ui(ns("tab_venn_upset")),
      mod_subtab_is_calibration_ui(ns("is_calibration"))
    )
  )
}

#' @importFrom shinydashboardPlus dashboardFooter
# main_page_footer <- function(ns) {
#   shinydashboardPlus::dashboardFooter(
#     left = "Shenkebio",
#     right = "2023-2024"
#   )
# }
