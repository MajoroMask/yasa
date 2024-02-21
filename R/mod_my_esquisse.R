#' my_esquisse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar box
#' @importFrom shinydashboard dashboardBody

mod_my_esquisse_ui <- function(id) {
  ns <- NS(id)
  ui <- tagList(
    shinydashboardPlus::dashboardPage(
      options = list(sidebarExpandOnHover = FALSE),
      header = shinydashboardPlus::dashboardHeader(),
      sidebar = shinydashboardPlus::dashboardSidebar(
        minified = TRUE,
        collapsed = TRUE
      ),
      body = shinydashboard::dashboardBody(
        fluidRow(
          shinydashboardPlus::box(
            width = 12,
            esquisse::esquisse_ui(ns("my_esquisse"))
          )
        )
      ),
      controlbar = NULL,
      footer = NULL
    )
  )
  return(ui)
}

#' my_esquisse Server Functions
#'
#' @noRd
#'
#' @importFrom esquisse esquisse_server
#' @importFrom datamods import_modal
mod_my_esquisse_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    x <- esquisse::esquisse_server("my_esquisse")
  })
}

## To be copied in the UI
# mod_my_esquisse_ui("my_esquisse_1")

## To be copied in the server
# mod_my_esquisse_server("my_esquisse_1")
