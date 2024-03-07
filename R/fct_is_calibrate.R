#' is_calibrate
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
is_calibrate <- function(tb_pro,
                         tb_pep,
                         tb_is,
                         drop_is_top2 = FALSE) {
  requireNamespace("dplyr", quietly = TRUE)

  tb_pro_mw <-
    tb_pro %>%
    select(
      acc = `Accession`,
      mw = `MW [kDa]`
    )
  tb_pep_abun <-
    # 4 columns: acc, seq (of peptide), sample, abun
    calculate_pep_abun(tb_pep) %>%
    filter(acc %in% union(tb_pro_mw$acc, tb_is$acc))
  tb_is_abun <-
    # 5 cols: acc, sample, abun_is, m_is, mw_is
    tb_pep_abun %>%
    filter(acc %in% tb_is$acc) %>%
    group_by(acc, sample) %>%
    summarise(
      abun_is = calculate_is_abun(abun, drop_is_top2 = drop_is_top2),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    filter(!is.na(abun_is)) %>%
    left_join(tb_is, by = "acc")
  tb_pro_abun <-
    # 6 columns: acc, sample, abun, pep_all, pep_quant, mw
    tb_pep_abun %>%
    group_by(acc, sample) %>%
    group_map(
      .f = calculate_pro_abun,
      .keep = TRUE
    ) %>%
    bind_rows() %>%
    left_join(tb_pro_mw, by = "acc")

  tb_pro_m <-
    # 2n+1 columns: acc, and 3 columns for each sample:
    #   `Mass: {sample_name}`
    #   `# peptides: {sample_name}`
    calculate_pro_m(tb_pro_abun, tb_is_abun, debug = FALSE)
  tb_pro_output <-
    tb_pro %>%
    left_join(tb_pro_m, by = c("Accession" = "acc"))
}

# utilities ----

#' Title
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom stringr str_replace regex str_detect
#' @importFrom tidyr pivot_longer
calculate_pep_abun <- function(tb) {
  requireNamespace("dplyr", quietly = TRUE)

  tb_out <-
    tb %>%
    rename(
      acc = `Master Protein Accessions`,
      seq = `Annotated Sequence`
    ) %>%
    rename_with(
      .cols = matches(stringr::regex("[Aa]bundance\\: ?")),
      .fn = ~ stringr::str_replace(.x, "[Aa]bundance\\: ?", "abun__")
    ) %>%
    filter(!stringr::str_detect(acc, ";")) %>%
    select(acc, seq, starts_with("abun__")) %>%
    tidyr::pivot_longer(
      cols = starts_with("abun__"),
      names_to = "sample",
      values_to = "abun"
    ) %>%
    mutate(sample = stringr::str_replace(sample, "^abun__", "")) %>%
    mutate(abun = as.double(abun)) %>%
    filter(!is.na(abun)) %>%
    arrange(acc, sample, desc(abun))
  return(tb_out)
}

#' Title
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom utils head
calculate_is_abun <- function(pep_abuns, drop_is_top2 = FALSE) {
  top3 <- pep_abuns %>% utils::head(n = 3L)
  n_peps <- length(top3)
  if (n_peps == 1L) return(NA_real_)
  if (drop_is_top2) {
    if (n_peps == 2L) return(NA_real_)
  }
  return(sum(top3))
}

#' Title
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom tibble tibble
calculate_pro_abun <- function(tb_sub, the_group) {
  tb_output <-
    tibble::tibble(
      abun = tb_sub %>% pull(abun) %>% head(n = 3L) %>% sum(na.rm = TRUE),
      pep_quant = tb_sub %>% pull(abun) %>% length() %>% min(., 3L),
    ) %>%
    bind_cols(the_group, .)
  return(tb_output)
}

#' Title
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom tidyr expand_grid pivot_wider
calculate_pro_m <- function(tb_pro_abun, tb_is_abun, debug = FALSE) {
  tb_work <-
    tidyr::expand_grid(
      tb_pro_abun,
      tb_is_abun %>% rename(acc_is = acc, sample_is = sample),
    ) %>%
    filter(acc != acc_is, sample == sample_is) %>%
    mutate(m = abun / abun_is * m_is / mw_is * mw)
  tb_out1 <-
    tb_work %>%
    group_by(acc, sample) %>%
    summarise(
      m_mean = sum(m) / n(),
      n_peps = pep_quant[1],
      .groups = "drop"
    ) %>%
    ungroup()
  tb_out2 <-
    tb_work %>%
    select(acc, sample, acc_is, m) %>%
    tidyr::pivot_wider(
      id_cols = c(acc, sample),
      names_from = acc_is,
      values_from = m,
      names_prefix = "m_IS-"
    )
  tb_out <-
    left_join(tb_out1, tb_out2, by = c("acc", "sample")) %>%
    tidyr::pivot_wider(
      id_cols = acc,
      names_from = sample,
      values_from = c(-acc, -sample),
      names_glue = c("{.value}-{sample}"),
      values_fill = NA_real_
    )
  return(tb_out)
}
