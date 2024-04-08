#' subtab_hcp_visual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList validateCssUnit uiOutput downloadButton
#'   plotOutput
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboardPlus box
#' @importFrom shinyWidgets switchInput
#' @importFrom datamods filter_data_ui
#' @importFrom plotly plotlyOutput
mod_subtab_hcp_visual_ui <- function(id){
  ns <- NS(id)

  shinydashboard::tabItem(
    tabName = "hcp_visual",
    br(),
    br(),
    # h2(i18n("HCP analysis")),

    # 1st row ----
    shiny::fluidRow(

      ## upper left box ----
      shinydashboardPlus::box(
        title = i18n("Data upload & settings"),
        width = 4,
        status = "warning",
        shinydashboard::tabBox(
          id = ns("hcp_tab_setting"),
          height = "100%",
          width = "100%",

          ### upload tab ----
          shiny::tabPanel(
            title = i18n("Upload data"),
            shiny::fileInput(
              ns("file_hcp_input"),
              label = i18n("Upload protein list"),
              accept = c(".xlsx")
            ),
            shiny::downloadLink(
              ns("download_test_data_hcp"),
              label = i18n("Download example data"),
            ),
            br(),
            shiny::uiOutput(ns("ui_column_selection"))
          ),

          ### setting tab ----
          shiny::tabPanel(
            title = i18n("Settings"),
            shiny::selectInput(
              ns("mw_trans"),
              label = i18n("MW transformation"),
              choices = list(
                "Log 2" = "log2",
                "Log 10" = "log10",
                "None" = "none"
              ),
              selected = "log10"
            ),
            shiny::selectInput(
              ns("abun_trans"),
              label = i18n("Abundance transformation"),
              choices = list(
                "Log 2" = "log2",
                "Log 10" = "log10",
                "None" = "none"
              ),
              selected = "none"
            ),
            shiny::selectInput(
              ns("high_risk_warning"),
              label = i18n("High-risk protein warning"),
              choices = list(
                "CHO" = "cho",
                "None" = "none"
              ),
              selected = "none"
            )
          ),

          ### filtering tab ----
          shiny::tabPanel(
            title = i18n("Filter data"),
            datamods::filter_data_ui(
              ns("hcp_filter"),
              show_nrow = TRUE,
              max_height = validateCssUnit("800px")
            )
          ),

          ### output tab ----
          shiny::tabPanel(
            title = i18n("Download output"),
            # TODO add in-line instruction
            shiny::actionButton(
              ns("download_hcp_bubble"),
              label = i18n("Download bubble plot"),
              icon = shiny::icon("download")
            ),
            # TODO add in-line instruction
            shiny::downloadButton(
              ns("download_hcp_stats_table"),
              label = i18n("Download statistical results")
            )
          )
        )
      ),

      ## upper right box ----
      shinydashboardPlus::box(
        title = i18n("Plots & Usage Instructions"),
        width = 8,
        status = "warning",
        shinydashboard::tabBox(
          id = ns("hcp_tab_plot"),
          height = "100%",
          width = "100%",

          ## bubble plot tab ----
          shiny::tabPanel(
            title = i18n("Bubble plot"),
            plotly::plotlyOutput(
              ns("hcp_bubble"),
              width = "100%",
              height = "550px"
            )
          ),

          ## venn tab ----
          shiny::tabPanel(
            title = i18n("Venn diagram"),
            plotOutput(
              ns("hcp_venn"),
              width = validateCssUnit("100%"),
              height = validateCssUnit("550px")
            )
          ),

          ## docs tab ----
          shiny::tabPanel(
            title = i18n("Usage Instructions"),
            h4("Instructions for HCP visualization"),
            p("Some instruction."),
            p("And some other instruction.")
          )
        )
      )
    ),

    # 2nd row ----
    shiny::fluidRow(
      ## lower left box ----
      # shinydashboardPlus::box(
      #   title = i18n("Venn diagram"),
      #   width = 4,
      #   status = "warning",
      #   plotOutput(
      #     ns("hcp_venn"),
      #     width = validateCssUnit("100%"),
      #     height = validateCssUnit("500px")
      #   )
      # ),

      ## lower right box ----
      shinydashboardPlus::box(
        title = i18n("Top differential HCPs"),
        width = 12,
        status = "warning",
        DT::DTOutput(
          ns("hcp_table"),
          width = "100%",
          height = "500px"
        )
      )
    )

    # 3rd row ----
    # shiny::fluidRow(
    #   shinydashboardPlus::box(
    #     title = i18n("Playground"),
    #     width = 6,
    #     status = "danger",
    #     shiny::verbatimTextOutput(ns("test01"))
    #   ),
    #   shinydashboardPlus::box(
    #     title = i18n("Another playground"),
    #     width = 6,
    #     status = "danger",
    #     shiny::verbatimTextOutput(ns("test02"))
    #   ),
    # )
  )
}

#' subtab_hcp_visual Server Functions
#'
#' @noRd
#'
#' @import ggplot2
#' @import Vennerable
#' @importFrom shinyWidgets pickerInput
#' @importFrom datamods filter_data_server
#' @importFrom plotly renderPlotly plot_ly layout event_register event_data
#' @importFrom tidyr pivot_longer replace_na
#  @importFrom ggVennDiagram Venn process_data venn_regionedge venn_setedge
#    venn_setlabel
mod_subtab_hcp_visual_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # reactive values init ----

    rv_hcp <- rv(
      tb_input = NULL,  # origin input
      tb_to_filter = NULL,  # tb sent to datamods::filter_data module
      origin_colnames = NULL,  # selected colnames
      selected_colnames = NULL,
      tb_plot = NULL,
      p_bubble = NULL,  # bubble plot from plotly
      tb_plotly_selecting = NULL, # selecting data from plotly
      tb_display = NULL,  # tb displayed after filtering & plotly selecting
      p_venn = NULL,  # venn diagram from Vennerable
      stats_table_one = NULL  # table one
      # TODO add more if needed
    )

    rv_dummy_bubble <- rv(plot = NULL)  # bubble plot for saving

    rv_hcp$tb_input <- reactive({
      if (!is.null(input$file_hcp_input)) {
        tb <-
          read_input_tb(input$file_hcp_input$datapath) %>%
          add_hidden_index()
      } else {
        # tb <-
        #   system.file("ext/hcp_example_data.xlsx", package = "yasa") %>%
        #   read_input_tb() %>%
        #   add_hidden_index()
        tb <- NULL
      }
      tb
    })

    rv_hcp$origin_colnames <- reactive({
      req(rv_hcp$tb_input())
      origin_colnames <- colnames(rv_hcp$tb_input())
      origin_colnames
    })

    ## column selection UI ----

    output$ui_column_selection <- renderUI({
      req(rv_hcp$tb_input())
      req(rv_hcp$origin_colnames())

      tbi <- rv_hcp$tb_input()  # table input
      ocn <- rv_hcp$origin_colnames()  # original column names

      ui_out <- tagList(
        shinyWidgets::pickerInput(
          ns("col_abun_case"),
          label = i18n("Select case abundance"),
          choices = prioritize_colnames(
            ocn,
            pattern = "(?=.*Abundance.*)(?!.*Ratio.*)",
            ignore_case = TRUE
          ),
          options = list(
            title = i18n("Select by column name"),
            `live-search` = TRUE,
            size = 8
          )
        ),
        shinyWidgets::pickerInput(
          ns("col_abun_control"),
          label = i18n("Select control abundance"),
          choices = prioritize_colnames(
            ocn,
            pattern = "(?=.*Abundance.*)(?!.*Ratio.*)",
            ignore_case = TRUE
          ),
          options = list(
            title = i18n("Select by column name"),
            `live-search` = TRUE,
            size = 8
          )
        ),
        shinyWidgets::pickerInput(
          ns("col_abun_ratio"),
          label = i18n("Select case/control ratio"),
          choices = prioritize_colnames(
            ocn,
            pattern = "Abundance Ratio",
            ignore_case = TRUE
          ),
          options = list(
            title = i18n("Select by column name"),
            `live-search` = TRUE,
            size = 8
          )
        ),
        shinyWidgets::pickerInput(
          ns("col_acc"),
          label = i18n("Select protein accession"),
          choices = prioritize_colnames(
            ocn,
            pattern = "Accession",
            ignore_case = TRUE
          ),
          selected = "Accession",
          options = list(
            title = i18n("Select by column name"),
            `live-search` = TRUE,
            size = 8
          )
        ),
        shinyWidgets::pickerInput(
          ns("col_mw"),
          label = i18n("Select molecular weight (MW)"),
          choices = prioritize_colnames(
            ocn,
            pattern = "(molecular weight)|(MW)",
            ignore_case = FALSE
          ),
          selected = "MW [kDa]",
          options = list(
            title = i18n("Select by column name"),
            `live-search` = TRUE,
            size = 8
          )
        ),
        shinyWidgets::pickerInput(
          ns("col_pi"),
          label = i18n("Select isoelectric point (pI)"),
          choices = prioritize_colnames(
            ocn,
            pattern = "pI",
            ignore_case = FALSE
          ),
          selected = "calc. pI",
          options = list(
            title = i18n("Select by column name"),
            `live-search` = TRUE,
            size = 8
          )
        )
      )
      ui_out
    })

    # select data ----

    rv_hcp$selected_colnames <- reactive({
      req(input$col_acc)
      req(input$col_abun_case)
      req(input$col_abun_control)
      req(input$col_abun_ratio)
      req(input$col_mw)
      req(input$col_pi)

      selected_colnames <- c(
        # The order must NOT be changed!
        acc = input$col_acc,
        case = input$col_abun_case,
        control = input$col_abun_control,
        ratio = input$col_abun_ratio,
        mw = input$col_mw,
        pi = input$col_pi
      )
      selected_colnames
    })

    rv_hcp$tb_to_filter <- reactive({
      req(rv_hcp$tb_input())
      req(rv_hcp$selected_colnames())
      req(input$mw_trans)
      req(input$abun_trans)

      tb <- rv_hcp$tb_input()
      selected_colnames <- rv_hcp$selected_colnames()
      mw_trans <- input$mw_trans
      abun_trans <- input$abun_trans

      tb_to_filter <-
        tb %>%
        dplyr::select(.idx, dplyr::all_of(selected_colnames)) %>%
        ratio_and_intensity(mw_trans, abun_trans)
      tb_to_filter
    })

    # filter data ----

    filter_result <- filter_data_server(
      "hcp_filter",
      data = rv_hcp$tb_to_filter,
      vars = reactive({
        req(input$abun_trans)
        req(input$mw_trans)

        label_on_ui <- c(
          rename_case_match("Case abundance", input$abun_trans),
          rename_case_match("Control abundance", input$abun_trans),
          "abundance ratio (|log2|)",
          rename_case_match("MW", input$mw_trans),
          "pI"
        )
        rv_hcp$tb_to_filter() %>%
          select(-.idx, -acc) %>%
          colnames() %>%
          rlang::set_names(nm = label_on_ui)
      }),
      widget_num = "slider",
      widget_date = "slider",
      label_na = "NA"
    )

    rv_hcp$tb_plot <- reactive({
      req(rv_hcp$tb_input())
      req(rv_hcp$selected_colnames())
      req(filter_result)

      tb_label <-
        rv_hcp$tb_input() %>%
        dplyr::select(.idx, dplyr::all_of(rv_hcp$selected_colnames())) %>%
        dplyr::transmute(
          .idx,
          label_case = sprintf("%.1f", case),
          label_control = sprintf("%.1f", control),
          label_ratio = sprintf("%.2f", ratio)
        )
      tb_plot <-
        filter_result$filtered() %>%
        left_join(tb_label, by = ".idx")
      tb_plot
    })

    # bubble plotly ----

    output$hcp_bubble <- plotly::renderPlotly({
      req(rv_hcp$tb_plot())
      req(input$mw_trans)

      mw_trans <- input$mw_trans

      tb_p <-
        rv_hcp$tb_plot() %>%
        tidyr::pivot_longer(
          cols = c(case, control),
          names_to = "sample",
          values_to = "abun"
        ) %>%
        mutate(abun_filled = tidyr::replace_na(abun, 0))

      p_bubble <-
        tb_p %>%
        plotly::plot_ly(
          x = ~ pi,
          y = ~ mw,

          color = ~ sample,
          colors = c("#1972A4", "#C61951"),
          size = ~ abun_filled,
          sizes = c(20, 200),
          type = "scatter",
          mode = "markers",
          marker = list(
            symbol = "circle",
            sizemode = "area",
            opacity = 0.4,
            line = list(width = 1.5, color = "#FFFFFF")
          ),

          hoverinfo = "text",
          text = ~ paste(
            acc,
            '<br>Case:', label_case,
            '<br>Control:', label_control,
            '<br>ratio:', label_ratio
          ),
          textposition = "top",

          customdata = tb_p$.idx
        ) %>%
        plotly::layout(
          dragmode = "select",
          xaxis = list(
            title = "pI",
            gridcolor = 'rgb(255, 255, 255)'
          ),
          yaxis = list(
            title = rename_case_match("MW", mw_trans),
            gridcolor = 'rgb(255, 255, 255)'
          ),
          paper_bgcolor = "rgb(243, 243, 243)",
          plot_bgcolor = "rgb(243, 243, 243)"
        ) %>%
        plotly::event_register("plotly_selecting")

      rv_hcp$p_bubble <- p_bubble

      rv_dummy_bubble$plot <-
        ggplot(tb_p) +
        geom_point(
          aes(pi, mw, fill = sample, size = abun_filled),
          shape = "circle filled", alpha = I(0.4),
          stroke = 1, color = "#ffffff"
        ) +
        scale_fill_manual(
          values = c(case = "#1972A4", control = "#C61951"),
          guide = guide_legend(override.aes = list(size = 5))
        ) +
        scale_size(guide = "none") +
        theme_bw() +
        ggplot2::theme(
          legend.title = element_blank(),
          legend.justification = c("right", "top"),
          legend.background = element_rect(fill = "#f3f3f3"),
          legend.key = element_rect(fill = "#f3f3f3"),
          panel.background = element_rect(fill = "#f3f3f3"),
          panel.border = element_blank(),
          panel.grid = element_line(color = "#ffffff"),
          plot.background = element_rect(fill = "#f3f3f3")
        ) +
        labs(
          x = "pI",
          y = rename_case_match("MW", mw_trans)
        )

      p_bubble
    })

    # venn ----

    output$hcp_venn <- renderPlot({
      req(rv_hcp$tb_plot())
      req(nrow(rv_hcp$tb_plot()) > 0L)

      l_venn <- list(
        case = rv_hcp$tb_plot() %>% get_enlist_acc(case),
        control = rv_hcp$tb_plot() %>% get_enlist_acc(control)
      )

      # DEPRECATED: using another venn diagram interface seems stupid

      # data_venn <-
      #   ggVennDiagram::Venn(l_venn) %>%
      #   ggVennDiagram::process_data()
      #
      # p_venn <-
      #   ggplot() +
      #   geom_polygon(
      #     mapping = aes(X, Y, fill = count, group = id),
      #     data = ggVennDiagram::venn_regionedge(data_venn)
      #   ) +
      #   geom_path(
      #     mapping = aes(X, Y, group = id),
      #     color = "grey",
      #     linewidth = 2,
      #     linetype = "dashed",
      #     data = ggVennDiagram::venn_setedge(data_venn),
      #     show.legend = FALSE
      #   ) +
      #   geom_text(
      #     mapping = aes(X, Y, label = name),
      #     data = ggVennDiagram::venn_setlabel(data_venn)
      #   ) +
      #   geom_label(
      #     mapping = aes(X, Y, label = count),
      #     data = ggVennDiagram::venn_regionlabel(data_venn)
      #   ) +
      #   scale_x_continuous(expand = expansion(mult = 0.25)) +
      #   scale_fill_viridis_c() +
      #   coord_equal() +
      #   theme_void()

      p_venn <- Vennerable::compute.Venn(
        Vennerable::Venn(l_venn),
        doWeights = TRUE,
        doEuler = FALSE,
        type = "circles"
      )

      rv_hcp$p_venn <- p_venn

      plot(
        p_venn,
        show = list(Universe = FALSE)
      )
    })

    # table ----

    output$hcp_table <- DT::renderDT(
      expr = {
        req(rv_hcp$tb_input())
        req(rv_hcp$tb_plot())
        req(rv_hcp$selected_colnames())
        req(rv_hcp$p_bubble)

        # display points either:
        #   - passed the filter module
        #   - selected on plotly (they passed the filter, too)
        if (isTruthy(plotly::event_data("plotly_selecting"))) {
          selecting_idx <-
            plotly::event_data("plotly_selecting") %>%
            dplyr::pull(customdata) %>%
            unique()
        } else {
          selecting_idx <-
            rv_hcp$tb_plot() %>%
            dplyr::pull(.idx)
        }

        tb_display <-
          rv_hcp$tb_input() %>%
          filter(.idx %in% selecting_idx) %>%
          select(
            all_of(unname(rv_hcp$selected_colnames())),
            everything()
          ) %>%
          select(-.idx)

        rv_hcp$tb_display <- tb_display

        tb_display
      },
      options = list(
        paging = TRUE,      # paginate the output
        pageLength = 10,    # number of rows to output for each page
        scrollX = TRUE,     # enable scrolling on X axis
        scrollY = TRUE,     # enable scrolling on Y axis
        autoWidth = TRUE,   # use smart column width handling
        server = TRUE,      # use client-side processing
        dom = "Bfrtip",
        buttons = c("csv", "excel")
      ),
      extensions = 'Buttons',
      selection = 'single', # enable selection of a single row
      filter = 'bottom',    # include column filters at the bottom
      rownames = FALSE      # don't show row numbers/names
    )

    # download render ----

    ## save rigged bubble plot by ggplot2 ----

    observeEvent(input$download_hcp_bubble, {
      req(rv_dummy_bubble$plot)
      esquisse::save_ggplot_modal(
        ns("save_ggplot_bubble"),
        title = i18n("Download Plot")
      )
    })

    esquisse::save_ggplot_server(
      "save_ggplot_bubble",
      plot_rv = rv_dummy_bubble
    )

    # TODO table one

    ## save table ----

    output$download_hcp_stats_table <- shiny::downloadHandler(
      # TODO table one
      filename = "HCP_table.xlsx",
      content = function(file) {
        openxlsx::write.xlsx(
          rv_hcp$tb_display,
          file = file,
          asTable = FALSE,
          overwrite = TRUE
        )
      }
    )

    output$download_test_data_hcp <- shiny::downloadHandler(
      filename = "hcp_example_data.xlsx",
      content = function(file) {
        file.copy(
          from = system.file("ext/hcp_example_data.xlsx", package = "yasa"),
          to = file
        )
      }
    )

    # pg ----

    # output$test01 <- renderPrint({
    #   rv_hcp$tb_to_filter()
    # })
    #
    # output$test02 <- renderPrint({
    #   rv_hcp$tb_plot()
    # })
  })
}

# utilities ----

add_hidden_index <- function(tb) {
  tb_out <- tb %>% mutate(.idx = row_number(), .before = 1)
  return(tb_out)
}

remove_hidden_index <- function(tb) {
  tb_out <- tb %>% select(-any_of(".idx"))
  return(tb_out)
}


#' Prioritize colnames by pattern
#'
#' @noRd
#'
#' @importFrom stringr regex str_subset
#' @importFrom rlang set_names
prioritize_colnames <- function(x,
                                pattern,
                                ignore_case = FALSE,
                                negate = FALSE) {
  if_first_class <- grepl(pattern, x, ignore.case = ignore_case, perl = TRUE)
  if (negate) if_first_class <- !if_first_class
  first_class <- x[if_first_class] %>% rlang::set_names()
  second_class <- x[!if_first_class] %>% rlang::set_names()

  if (length(first_class) == 0L) {
    l_out <- x
  } else {
    l_out <- list(
      "You may looking for" = first_class,
      "Other columns" = second_class
    )
  }
  return(l_out)
}

ratio_and_intensity <- function(tb,
                                mw_trans = NULL,
                                abun_trans = NULL) {
  # tb must have:
  # .idx acc case control ratio mw pi

  tb_out <-
    tb %>%
    mutate(
      case = case_when(
        abun_trans == "log2" ~ log2(case),
        abun_trans == "log10" ~ log10(case),
        abun_trans == "none" ~ case,
        .default = case
      ),
      control = case_when(
        abun_trans == "log2" ~ log2(control),
        abun_trans == "log10" ~ log10(control),
        abun_trans == "none" ~ control,
        .default = control
      ),
      mw = case_when(
        mw_trans =="log2" ~ log2(mw),
        mw_trans =="log10" ~ log10(mw),
        mw_trans =="none" ~ mw,
        .default = mw
      ),
      ratio = ratio %>% log2() %>% abs()
    ) %>%
    filter(!(is.na(case) & is.na(control) & is.na(ratio)))
  return(tb_out)
}

rename_case_match <- function(name, case) {
  dplyr::case_match(
    case,
    "log10" ~ paste(name, "(log10)"),
    "log2" ~ paste(name, "(log2)"),
    "none" ~ name
  )
}

get_enlist_acc <- function(tb, var) {
  acc_out <-
    tb %>%
    filter(!is.na({{var}})) %>%
    pull(acc)
  return(acc_out)
}