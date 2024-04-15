#' subtab_is_calibration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
mod_subtab_is_calibration_ui <- function(id){
  ns <- NS(id)

  shinydashboard::tabItem(
    tabName = "is_calibration",

    br(),
    br(),

    shiny::fluidRow(

      # upper left box ----

      shinydashboardPlus::box(
        title = i18n("Data upload & settings"),
        width = 4,
        status = "warning",
        shinydashboard::tabBox(
          id = ns("iscal_tab"),
          height = "100%",
          width = "100%",

          ## upload tab ----

          shiny::tabPanel(
            title = i18n("Upload data"),
            shiny::fileInput(
              ns("file_pro_iscal"),
              label = i18n("Upload protein list"),
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                "text/tab-separated-values",
                ".csv",
                ".tsv",
                ".xlsx"
              )
            ),
            shiny::downloadLink(
              ns("download_test_data_iscal_prot"),
              label = i18n("Protein list example data"),
            ),

            shiny::fileInput(
              ns("file_pep_iscal"),
              label = i18n("Upload peptide list"),
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                "text/tab-separated-values",
                ".csv",
                ".tsv",
                ".xlsx"
              )
            ),
            shiny::downloadLink(
              ns("download_test_data_iscal_pep"),
              label = i18n("Peptide list example data"),
            ),

            shiny::fileInput(
              ns("file_is_iscal"),
              label = i18n("Upload IS table"),
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                "text/tab-separated-values",
                ".csv",
                ".tsv",
                ".xlsx"
              )
            ),
            shiny::downloadLink(
              ns("download_test_data_iscal_is"),
              label = i18n("IS table example data"),
            ),
          ),

          ## setting tab ----

          shiny::tabPanel(
            title = i18n("Settings"),
            shiny::selectInput(
              ns("is_to_kda"),
              label = i18n("Unit in IS table"),
              choices = list(
                "kDa" = "kda",
                "Da" = "da"
              ),
              selected = "kda"
            ),
            shiny::checkboxInput(
              ns("drop_is_top2"),
              label = i18n("Drop IS Top2"),
              value = TRUE
            )
          )
        )
      ),

      # upper right box ----

      shinydashboardPlus::box(
        title = i18n("Output & Usage Instructions"),
        width = 8,
        status = "warning",
        shinydashboard::tabBox(
          id = ns("iscal_tab_result"),
          height = "100%",
          width = "100%",

          shiny::tabPanel(
            title = i18n("Table Output preview"),
            DT::DTOutput(
              ns("iscal_output"),
              width = "100%",
              height = "auto"
            ),
            shiny::downloadButton(
              ns("download_iscal_output"),
              label = i18n("Download Table")
            )
          ),
          shiny::tabPanel(
            title = i18n("Usage Instructions"),
            render_yasa_markdown_docs("docs_iscal.md")
          )
        )
      )
    )
  )
}

#' subtab_is_calibration Server Functions
#'
#' @noRd
#'
#' @importFrom DT renderDT
mod_subtab_is_calibration_server <- function(id) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # data init ----

    rv_iscal <- rv(
      tb_pro = NULL,
      tb_pep = NULL,
      tb_is = NULL,
      result = NULL
    )

    rv_iscal$tb_pro <- reactive({
      req(input$file_pro_iscal)
      tb_out <- read_input_tb(input$file_pro_iscal$datapath)
    })

    rv_iscal$tb_pep <- reactive({
      req(input$file_pep_iscal)
      tb_out <- read_input_tb(input$file_pep_iscal$datapath)
    })

    rv_iscal$tb_is <- reactive({
      requireNamespace("dplyr", quietly = TRUE)

      req(input$file_is_iscal)
      req(input$is_to_kda)

      tb_out <-
        read_input_tb(input$file_is_iscal$datapath) %>%
        rename(
          acc = 1,
          pro_name = 2,
          m_is = 3,
          mw_is = 4
        ) %>%
        select(-pro_name) %>%
        mutate(
          mw_is = if_else(
            rep(input$is_to_kda == "kda", times = n()),
            round(mw_is / 1000, digits = 1),
            as.double(mw_is)
          )
        )
    })

    ## render output table ----

    output$iscal_output <- DT::renderDT(
      expr = {
        req(rv_iscal$tb_pro)
        req(rv_iscal$tb_pep)
        req(rv_iscal$tb_is)

        rv_iscal$result <- is_calibrate(
          tb_pro = rv_iscal$tb_pro(),
          tb_pep = rv_iscal$tb_pep(),
          tb_is = rv_iscal$tb_is(),
          drop_is_top2 = input$drop_is_top2
        )

        rv_iscal$result
      },
      options = list(scrollX = TRUE)
    )

    ## download output ----

    output$download_iscal_output <- shiny::downloadHandler(
      filename = "iscal_result.xlsx",
      content = function(file) {
        openxlsx::write.xlsx(
          rv_iscal$result,
          file = file,
          asTable = FALSE,
          overwrite = TRUE
        )
      }
    )

    ## download example data ----

    output$download_test_data_iscal_prot <- shiny::downloadHandler(
      filename = "iscal_proteins.xlsx",
      content = function(file) {
        file.copy(
          from = system.file("ext/iscal_proteins.xlsx", package = "yasa"),
          to = file
        )
      }
    )

    output$download_test_data_iscal_pep <- shiny::downloadHandler(
      filename = "iscal_peptides.xlsx",
      content = function(file) {
        file.copy(
          from = system.file("ext/iscal_peptides.xlsx", package = "yasa"),
          to = file
        )
      }
    )

    output$download_test_data_iscal_is <- shiny::downloadHandler(
      filename = "iscal_is.xlsx",
      content = function(file) {
        file.copy(
          from = system.file("ext/iscal_is.xlsx", package = "yasa"),
          to = file
        )
      }
    )
  })
}
