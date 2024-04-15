#' render_yasa_markdown_docs
#'
#' @description A fct function
#'
#' @param filename Markdown file name. The dir is fixed to `ext/`.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
render_yasa_markdown_docs <- function(filename) {
  html_output <-
    paste0("ext/", filename) %>%
    system.file(package = "yasa") %>%
    readLines() %>%
    shiny::markdown()
  return(html_output)
}
