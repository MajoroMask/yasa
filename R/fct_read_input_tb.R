#' read_input_tb
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom fs path_ext
#' @importFrom openxlsx read.xlsx
#' @importFrom tibble as_tibble
#' @importFrom vroom vroom
read_input_tb <- function(file,
                          col_names,
                          ...) {
  if (fs::path_ext(file) == "xlsx") {
    tb_out <-
      openxlsx::read.xlsx(
        xlsxFile = file,
        colNames = col_names,
        sep.names = " ",
        ...
      ) %>%
      tibble::as_tibble()
  } else {
    tb_out <- vroom::vroom(
      file = file,
      col_names = col_names,
      ...
    )
  }
  return(tb_out)
}
