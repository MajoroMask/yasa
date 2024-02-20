# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file


wd <- "/usr/local/etc/yasa/"
default_renv_lib <- "/usr/local/etc/yasa/renv/library/R-4.2/x86_64-pc-linux-gnu/"

setwd(wd)
# renv::load(wd)
.libPaths(default_renv_lib)
library(shiny)
library(yasa)

path_config <- head(commandArgs(TRUE), n = 1L)
if (length(path_config) == 0L) {
  path_config <- system.file(
    "ext", "default_config.json",
    package = "yasa",
    mustWork = TRUE
  )
}
l_options <-
  path_config |>
  jsonlite::read_json(simplifyVector = FALSE)

options("golem.app.prod" = TRUE)

shiny::runApp(
  yasa::run_app(golem_options = l_options),
  host = l_options$host,
  port = l_options$port
)
