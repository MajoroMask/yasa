#' tab_basic_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow
#' @importFrom esquisse esquisse_ui esquisseContainer
#' @importFrom shinydashboardPlus box
#' @importFrom shinydashboard tabItem
mod_tab_basic_plot_ui <- function(id){
  ns <- NS(id)

  shinydashboard::tabItem(
    tabName = "basic_plot",
    shiny::fluidRow(
      shinydashboardPlus::box(
        width = 12,
        esquisse::esquisse_ui(
          ns("my_esquisse"),
          container = esquisse::esquisseContainer(height = "850px")
        )
      )
    )
    # fluidRow(
    #   shinydashboardPlus::box(
    #     width = 12,
    #     title = i18n("Description")
    #   )
    # )
  )
}

#' tab_basic_plot Server Functions
#'
#' @noRd
mod_tab_basic_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # esquisse module server
    esquisse::esquisse_server("my_esquisse")
  })
}
