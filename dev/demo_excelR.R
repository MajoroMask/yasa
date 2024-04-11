
library(shiny)
library(excelR)
library(semantic.dashboard)

library(forcats)
library(tidyr)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "home", text = "Home", icon = icon("home")),
      menuItem(tabName = "another", text = "Another Tab", icon = icon("heart"))
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Plot",
        width = 10,
        plotOutput("plot", height = "500px")
      ),
      box(
        title = "模型信息",
        width = 6,
        style = "overflow-y: scroll; height: 500px;",
        box(
          title = "模型汇总",
          ribbon = FALSE,
          title_side = "top left",
          tableOutput("summary_lm")
        ),
        box(
          title = "方差分析",
          ribbon = FALSE,
          title_side = "top left",
          tableOutput("summary_aov")
        ),
        box(
          title = "均值",
          ribbon = FALSE,
          title_side = "top left",
          tableOutput("tb_sum")
        )
      )
    ),
    fluidRow(
      style = "height: 500px;",
      box(
        title = "Data",
        width = 16,
        action_button(
          "fire",
          label = "Confirm selection"
        ),
        excelR::excelOutput("excel_tb")
      )
    )
  )
)

server <- function(input, output, session) {

  # init ----

  rv <- reactiveValues(
    ori = NULL,
    selected = NULL,
    selected_d = NULL,
    tb_test = NULL,
    res_lm = NULL,
    res_aov = NULL
  )

  output$excel_tb <- excelR::renderExcel({
    data <-
      c(
        c(26.6, 28.03, 26.33, 25.31, 27.26, 27.61, 28.11, 27.67, 27.93, 28.67, 28.98, 28.69, 27.31, 28.37, 28.42),
        c(28.59, 28.79, 28.33, 27.87, 27.44, 30.59, 28.27, 26.35, 28.67, 29.61, 28.75, 28.32, 29.16, 27.27, 29.08)
      ) %>%
      matrix(ncol = 2) %>%
      t()
    excelR::excelTable(data, getSelectedData = TRUE)
  })

  rv$selected <- reactive({
    excelR::get_selected_data(input$excel_tb)
  })

  rv$tb_test <-
    reactive({
      tb_out <-
        rv$selected() %>%
        as.data.frame() %>%
        rlang::set_names(nm = as.character(1:ncol(.))) %>%
        as_tibble() %>%
        pivot_longer(
          cols = everything(),
          names_to = "group",
          values_to = "value"
        ) %>%
        mutate(group = fct_inorder(group)) %>%
        arrange(group)
      tb_out
    }) %>%
    bindEvent(input$fire, ignoreNULL = TRUE, ignoreInit = TRUE)

  # stats ----

  rv$res_lm <- reactive({
    req(rv$tb_test())

    res_lm <- stats::lm(value ~ group, data = rv$tb_test())
  })

  rv$res_lm_plot <- reactive({
    req(rv$tb_test())

    res_lm_plot <- stats::lm(value ~ group - 1, data = rv$tb_test())
  })

  rv$res_aov <- reactive({
    req(rv$res_lm())
    req(rv$tb_test())

    res_aov <- stats::aov(rv$res_lm(), data = rv$tb_test())
  })

  rv$tb_p <- reactive({
    req(rv$tb_test())
    req(rv$res_lm_plot())

    ci <- confint(rv$res_lm_plot())
    tb_p <-
      rv$tb_test() %>%
      group_by(group) %>%
      summarise(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      ungroup() %>%
      mutate(
        lower = ci[, 1],
        upper = ci[, 2]
      )
    tb_p
  })

  # output ----

  output$plot <- renderPlot({
    req(rv$tb_p())

    p <-
      ggplot(rv$tb_p()) +
      geom_errorbar(
        aes(group, ymin = lower, ymax = upper),
        color = "blue", width = 0.25
      ) +
      geom_point(aes(group, mean), size = 3, color = "blue") +
      geom_line(aes(group, mean), group = 1, color = "blue") +
      theme_bw() +
      theme(axis.title.x = element_blank())
    p
  })

  output$summary_lm <- renderTable({
    req(rv$res_lm())

    info <- summary(rv$res_lm())
    tb_display <-
      tibble(
        "S" = round(info$sigma, 5),
        "R-sq" = scales::percent(info$r.squared, 0.01),
        "R-sq (adj)" = scales::percent(info$adj.r.squared, 0.01)
      )
    tb_display
  })

  output$summary_aov <- renderTable({
    req(rv$res_aov())

    tb_display <-
      summary(rv$res_aov())[[1]] %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "name") %>%
      mutate(
        Df = as.integer(Df),
        across(where(is.double), ~ round(.x, 3))
      )
    tb_display
  })

  output$tb_sum <- renderTable({
    req(rv$tb_p())

    tb_sum <-
      rv$tb_p() %>%
      transmute(
        group,
        mean = round(mean, 3),
        sd = round(sd, 3),
        `95% CI` = paste0("(", round(lower, 3), ",   ", round(upper, 3), ")")
      )
    tb_sum
  })
}

shinyApp(ui, server)
