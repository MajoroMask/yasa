#' tab_venn_upset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow h2 tabPanel fileInput checkboxInput
#'   br downloadButton downloadLink htmlOutput sliderInput numericInput
#'   plotOutput
#' @importFrom shinydashboardPlus box
#' @importFrom shinydashboard tabItem tabBox
#' @importFrom colourpicker colourInput
mod_tab_venn_upset_ui <- function(id){
  ns <- NS(id)

  shinydashboard::tabItem(
    tabName = "venn_upset",

    br(),
    br(),

    shiny::fluidRow(

      # upper left box ----

      shinydashboardPlus::box(
        title = i18n("Data upload & settings"),
        width = 4,
        status = "warning",
        shinydashboard::tabBox(
          id = ns("venn_tab_setting"),
          height = "100%",
          width = "100%",

          ## upload tab ----

          shiny::tabPanel(
            title = i18n("Upload data"),
            shinyWidgets::switchInput(
              inputId = ns("venn_input_type"),
              label = i18n("Input type"),
              value = TRUE,
              labelWidth = "100px",
              onLabel = "PD",
              offLabel = "venn list",
              onStatus = "primary",
              offStatus = "success"
            ),
            shinyWidgets::actionBttn(
              inputId = ns("launch_import_modal"),
              label = i18n("Click to upload data"),
              style = "bordered",
              block = TRUE,
              color = "success",
              icon = icon("sliders")
            ),
            br(),
            shiny::uiOutput(ns("ui_column_selection")),
            br(),
            shiny::downloadLink(
              ns("download_test_data_venn_pd"),
              label = i18n("Download example PD data"),
            ),
            tags$a(style = "display: inline-block;width: 20px;"),
            shiny::downloadLink(
              ns("download_test_data_venn_list"),
              label = i18n("Download example venn list"),
            )
          ),

          ## setting tab ----

          shiny::tabPanel(
            title = i18n("Settings"),
            shiny::uiOutput(ns("venn_data_sets")),
            # shiny::selectInput(
            #   ns("venn_sets_selected"),
            #   label = i18n("Select sets"),
            #   choices = list(),
            #   multiple = TRUE,
            #   selectize = TRUE,
            #   selected = NULL
            # ),
            shiny::selectInput(
              ns("venn_type"),
              label = i18n("Venn diagram type"),
              choices = list(
                "Classical venn" = "Classical",
                "Chow-Ruskey venn" = "ChowRuskey",
                "Edwards venn" = "AWFE",
                "Squares venn" = "squares",
                "Battle venn" = "battle"
              ),
              selected = "Classical"
            ),
            shiny::checkboxInput(
              ns("doWeights"),
              label = i18n("Weighted area"),
              value = FALSE
            ),
            shiny::checkboxInput(
              ns("venn_label_pct"),
              label = i18n("Label percentage"),
              value = FALSE
            ),
            shiny::sliderInput(
              ns("venn_lwd"),
              label = i18n("Border line width"),
              value = 2.0,
              min = 0.0,
              max = 10.0,
              ticks = TRUE,
              step = 0.5
            ),
            shiny::selectInput(
              ns("venn_lty"),
              label = i18n("Border line type"),
              choices = list(
                "Solid" = 1,
                "Dashed" = 2,
                "Dotted" = 3,
                "Dot dash" = 4,
                "Long dash" = 5,
                "Two dash" = 6,
                "Blank" = 0
              ),
              selected = "1"
            ),
            shiny::sliderInput(
              ns("venn_size"),
              label = i18n("Zoom in/out Venn diagram"),
              value = 500,
              min = 200,
              max = 1000,
              ticks = FALSE,
              step = 10
            )
          ),

          ## color & font setting ----

          shiny::tabPanel(
            title = i18n("Color & Font"),
            shiny::selectInput(
              ns("venn_color_type"),
              label = i18n("Select color theme"),
              choices = list(
                Set1 = "Set1",
                AAAS = "aaas",
                NPG = "npg",
                NEJM = "nejm",
                Lancet = "lancet",
                JAMA = "jama",
                JCO = "jco",
                Custom = "custom"
              ),
              selected = "Set1"
            ),
            shiny::conditionalPanel(
              condition = "input.venn_color_type=='custom'",
              ns = ns,
              shiny::fluidRow(
                shiny::column(
                  width = 2,
                  colourpicker::colourInput(
                    ns("set1_color"),
                    label = i18n("Set1"),
                    value = "#E41A1C"
                  )
                ),
                shiny::column(
                  width = 2,
                  colourpicker::colourInput(
                    ns("set2_color"),
                    label = i18n("Set2"),
                    value = "#377EB8"
                  )
                ),
                shiny::column(
                  width = 2,
                  colourpicker::colourInput(
                    ns("set3_color"),
                    label = i18n("Set3"),
                    value = "#4DAF4A"
                  )
                ),
                shiny::column(
                  width = 2,
                  colourpicker::colourInput(
                    ns("set4_color"),
                    label = i18n("Set4"),
                    value = "#984EA3"
                  )
                ),
                shiny::column(
                  width = 2,
                  colourpicker::colourInput(
                    ns("set5_color"),
                    label = i18n("Set5"),
                    value = "#FF7F00"
                  )
                ),
                shiny::column(
                  width = 2,
                  colourpicker::colourInput(
                    ns("set6_color"),
                    label = i18n("Set6"),
                    value = "#FFFF33"
                  )
                )
              )
            ),
            shiny::sliderInput(
              ns("venn_labelsize"),
              label = i18n("Label font size"),
              value = 15,
              min = 1,
              max = 50,
              ticks = TRUE,
              step = 1
            ),
            shiny::sliderInput(
              ns("venn_cex"),
              label = i18n("Number font size"),
              value = 3,
              min = 0,
              max = 10,
              ticks = TRUE,
              step = 0.1
            )
          )
        )
      ),

      # upper right box ----

      shinydashboardPlus::box(
        title = i18n("Plot & Usage Instructions"),
        status = "warning",
        width = 8,

        ## plot tab ----

        shinydashboard::tabBox(
          id = ns("venn_tab_plot"),
          height = "100%",
          width = "100%",
          shiny::tabPanel(
            title = i18n("Venn diagram"),
            shiny::plotOutput(
              ns("venn_plot"),
              width = "100%",
              height = "100%"
            ),
            shinydashboardPlus::box(
              width = NULL,
              status = "warning",
              shiny::radioButtons(
                ns("venn_filetype"),
                label = i18n("Choose output file type:"),
                inline = TRUE,
                choices = list("PDF", "PNG", "SVG", "TIFF")
              ),
              shiny::downloadButton(
                ns("download_venn_output"),
                label = i18n("Download Plot")
              )
            )
          ),

          ## docs tab ----

          shiny::tabPanel(
            title = i18n("Usage Instructions"),
            render_yasa_markdown_docs("docs_venn.md")
          )
        )
      )
    )
  )
}

#' tab_venn_upset Server Functions
#'
#' @noRd
#'
#' @importFrom vroom vroom_write
#' @importFrom tibble tibble
#' @import Vennerable
#' @importFrom scales percent
#' @importFrom grDevices dev.off pdf png svg tiff
mod_tab_venn_upset_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rv_venn_plot <- rv(plot = NULL)

    # input init ----

    ob_import_modal <-
      observe({
        datamods::import_modal(
          id = ns("import_data"),
          from = c("file", "copypaste", "url"),
          title = i18n("Upload PD file or venn list")
        )
      }) %>%
      bindEvent(input$launch_import_modal)

    rv_imported_data <- datamods::import_server(
      "import_data",
      allowed_status = "OK",
      return_class = "tbl_df"
    )

    rv_input_type <- reactive({
      ifelse(input$venn_input_type, "pd", "venn_list")
    })

    output$ui_column_selection <- renderUI({
      req(rv_imported_data$data())
      req(rv_input_type())

      if (rv_input_type() == "pd") {
        ui_out <- tagList(
          shinyWidgets::pickerInput(
            ns("cols_sets"),
            label = i18n("Select sets data"),
            choices =
              rv_imported_data$data() %>%
              select(where(~ is.character(.x) | is.factor(.x))) %>%
              colnames() %>%
              prioritize_colnames(
                pattern = "^found in sample",
                ignore_case = TRUE
              ),
            multiple = TRUE,
            options = list(
              title = i18n("Select by column name"),
              `live-search` = TRUE,
              size = 8
            )
          ),
          shinyWidgets::materialSwitch(
            ns("peak_found_as_positive"),
            label = i18n('`Peak found` as positive'),
            value = TRUE,
            status = "primary"
          )
        )
      }
      if (rv_input_type() == "venn_list") {
        ui_out <- tagList()
      }
      ui_out
    })

    venn_data <- reactive({
      req(rv_input_type())

      if (!isTruthy(rv_imported_data$data())) {
        l_out <-
          system.file("ext/venn_example_data.xlsx", package = "yasa") %>%
          read_input_tb(col_names = TRUE) %>%
          purrr::map(.f = ~ .x[!is.na(.x)])
      } else if (rv_input_type() == "pd") {
        req(input$cols_sets)
        req(all(input$cols_sets %in% colnames(rv_imported_data$data())))

        l_out <-
          rv_imported_data$data() %>%
          select(all_of(input$cols_sets)) %>%
          add_hidden_index() %>%
          mutate(.idx = as.character(.idx)) %>%
          rename_with(
            .cols = -.idx,
            .fn = ~
              stringr::str_replace(
                .x,
                pattern = stringr::regex(
                  "found in sample\\:?(.*)(\\: sample)$", ignore_case = TRUE
                ),
                replacement = "\\1"
              ) %>%
              stringr::str_trim()
          ) %>%
          mutate(
            across(
              .cols = -.idx,
              .fns = ~ recode_found_in_sample(.x, input$peak_found_as_positive)
            ),
            across(
              .cols = -.idx,
              .fns = ~ if_else(is.na(.x), NA_character_, .idx)
            )
          ) %>%
          select(-.idx) %>%
          purrr::map(.f = ~ .x[!is.na(.x)])
      } else if (rv_input_type() == "venn_list") {
        l_out <-
          rv_imported_data$data() %>%
          purrr::map(.f = ~ .x[!is.na(.x)])
      }
      l_out
    })

    set_names <- reactive({
      names <- names(venn_data())
      return(names)
    })

    output$venn_data_sets <- renderUI({
      req(set_names())
      ui_out <- tagList(
        selectInput(
          ns("venn_sets_selected"),
          label = i18n("Select sets"),
          choices = as.character(set_names()),
          multiple = TRUE,
          selectize = TRUE,
          selected = as.character(set_names()[1:5])
        )
      )
      ui_out
    })
    # ob_update_sets_selected <- observe({
    #   set_names <- names(venn_data())
    #
    #   shiny::updateSelectInput(
    #     session,
    #     inputId = "venn_sets_selected",
    #     label = i18n("Select sets"),
    #     choices = as.character(set_names),
    #     selected = as.character(set_names[1:5])
    #   )
    # })

    venn_selected_names <- reactive({
      venn_selected_names <- as.character(c(input$venn_sets_selected))
    })

    venn_data_selected <- reactive({
      data <- venn_data()
      if(!is.null(input$venn_sets_selected)){
        data <- data[c(venn_selected_names())]
      }
      return(data)
    })

    venn_combinations <- reactive({
      data <- venn_data_selected()
      Vennerable::Venn(data)
    })

    get_venn_gp <- reactive({
      req(rv_venn_plot$plot)

      venn_gp <-
        rv_venn_plot$plot %>%
        Vennerable::VennThemes()

      venn_gp$SetText <- lapply(
        venn_gp$SetText,
        function(x) {
          x$fontsize <- as.numeric(input$venn_labelsize)
          return(x)
        }
      )
      venn_gp$FaceText <- lapply(
        venn_gp$FaceText,
        function(x) {
          x$cex <- as.numeric(input$venn_cex / 2)
          return(x)
        }
      )
      venn_gp$Set <- lapply(
        venn_gp$Set,
        function(x) {
          x$lwd <- as.numeric(input$venn_lwd)
          return(x)
        }
      )
      venn_gp$Set <- lapply(
        venn_gp$Set,
        function(x) {
          x$lty <- as.numeric(input$venn_lty)
          return(x)
        }
      )

      if (input$venn_color_type == 'Set1') {
        # do nothing
      } else if (input$venn_color_type == 'custom') {
        venn_gp$Set$Set1$col <- input$set1_color
        venn_gp$Set$Set2$col <- input$set2_color
        venn_gp$Set$Set3$col <- input$set3_color
        venn_gp$Set$Set4$col <- input$set4_color
        venn_gp$Set$Set5$col <- input$set5_color
        venn_gp$Set$Set6$col <- input$set6_color

        venn_gp$SetText$Set1$col <- input$set1_color
        venn_gp$SetText$Set2$col <- input$set2_color
        venn_gp$SetText$Set3$col <- input$set3_color
        venn_gp$SetText$Set4$col <- input$set4_color
        venn_gp$SetText$Set5$col <- input$set5_color
        venn_gp$SetText$Set6$col <- input$set6_color
      } else {
        venn_gp <- update_venn_color(
          venn_gp,
          ggsci_pal_name = input$venn_color_type
        )
      }

      return(venn_gp)
    })

    data_size <- reactive({
      return(length(venn_data_selected()))
    })

    get_venn_type <- reactive({
      if(input$venn_type == "Classical") {
        if(data_size() < 4) {
          return("circles")
        } else {
          return("ellipses")
        }
      } else if (input$venn_type == 'ChowRuskey' && data_size() < 3){
        return("circles")
      } else {
        return(input$venn_type)
      }
    })

    rv_venn_size <- reactive({
      return(input$venn_size)
    })

    # plot ----

    output$venn_plot <- renderPlot(
      expr = {
        req(Vennerable:::NumberOfSets(venn_combinations()) >= 2)

        venn_plot <- Vennerable::compute.Venn(
          venn_combinations(),
          doWeights = input$doWeights,
          doEuler = FALSE,
          type = get_venn_type()
        )

        if (input$venn_label_pct) {
          venn_label <- venn_plot@IndicatorWeight[, ".Weight"]
          venn_label_pct <-
            paste(
              venn_label,
              scales::percent(
                venn_label / sum(venn_label, na.rm = TRUE),
                accuracy = 0.1
              ),
              sep = "|"
            ) %>%
            {dplyr::if_else(
              venn_label == 0,
              true = stringr::str_replace(., "\\|0.0%", ""),
              false = .
            )}
          venn_plot@IndicatorWeight[, ".Weight"] <- venn_label_pct
        }

        rv_venn_plot$plot <- venn_plot

        plot(
          venn_plot,
          gp = get_venn_gp(),
          show = list(Universe = FALSE)
        )
      },
      width = rv_venn_size,
      height = rv_venn_size,
      outputArgs = list()
    )

    # download ----

    output$download_venn_output <- downloadHandler(
      filename = function() {
        paste("Venn_diagram", tolower(input$venn_filetype), sep = ".")
      },
      content = function(file) {
        req(rv_venn_plot$plot)

        width <- input$venn_size
        height <- input$venn_size
        pixelratio <- 2

        if (input$venn_filetype == "PNG") {
          png(
            file,
            width = width * pixelratio,
            height = height * pixelratio,
            units = "px",
            res = 72 * pixelratio
          )
        } else if (input$venn_filetype == "SVG") {
          svg(file, width = 8, height = 8)
        } else if (input$venn_filetype == "TIFF") {
          tiff(
            file,
            width = width * pixelratio,
            height = height * pixelratio,
            units = "px"
          )
        } else if (input$venn_filetype == "PDF") {
          pdf(file, width = 8, height = 8)
        }
        plot(
          rv_venn_plot$plot,
          gp = get_venn_gp(),
          show = list(Universe = FALSE)
        )
        dev.off()
      }
    )

    output$download_test_data_venn_pd <- shiny::downloadHandler(
      filename = "venn_pd_example_data.xlsx",
      content = function(file) {
        file.copy(
          from = system.file("ext/hcp_example_data.xlsx", package = "yasa"),
          to = file
        )
      }
    )

    output$download_test_data_venn_list <- shiny::downloadHandler(
      filename = "venn_list_example_data.xlsx",
      content = function(file) {
        file.copy(
          from = system.file("ext/venn_example_data.xlsx", package = "yasa"),
          to = file
        )
      }
    )
  })
}

# utils ----


#' update_venn_color
#'
#' @noRd
#'
#' @import ggsci
update_venn_color <- function(venn_gp, ggsci_pal_name) {
  requireNamespace("ggsci", quietly = TRUE)

  ggsci_pal_func <-
    paste0("pal_", ggsci_pal_name) %>%
    get(envir = getNamespace("ggsci"))
  colors <- ggsci_pal_func()(6)

  venn_gp$Set$Set1$col <- colors[1]
  venn_gp$Set$Set2$col <- colors[2]
  venn_gp$Set$Set3$col <- colors[3]
  venn_gp$Set$Set4$col <- colors[4]
  venn_gp$Set$Set5$col <- colors[5]
  venn_gp$Set$Set6$col <- colors[6]

  venn_gp$SetText$Set1$col <- colors[1]
  venn_gp$SetText$Set2$col <- colors[2]
  venn_gp$SetText$Set3$col <- colors[3]
  venn_gp$SetText$Set4$col <- colors[4]
  venn_gp$SetText$Set5$col <- colors[5]
  venn_gp$SetText$Set6$col <- colors[6]

  return(venn_gp)
}

recode_found_in_sample <- function(string, allow_peak_found = TRUE) {
  if (allow_peak_found) {
    string_out <-
      string %>%
      dplyr::na_if("Peak Found") %>%
      dplyr::na_if("n/a")
  } else {
    string_out <-
      string %>%
      dplyr::na_if("n/a")
  }
  string_out
}
