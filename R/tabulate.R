tabulate_row <- function(mapping) {
  types <- mapping$qsheet$df$Type
  l <- mapping$qsheet$df |>
    dplyr::rowwise() |>
    dplyr::group_split()
  mapping$qsheet$df$tab_data <- purrr::map2(
    l,
    types,
    \(row, type) new_tab_row(row, type)
  ) |>
    purrr::map(\(row) pivot_table_data(row, mapping))
}

new_tab_row <- function(df_row, subclass) {
  class(df_row) <- c(paste0("tab_type_", subclass), class(df_row))
  df_row
}

pivot_table_data <- function(row, mapping) {
  UseMethod("pivot_table_data")
}
pivot_table_data.default <- function(row, mapping) {
  tibble::tibble()
}
pivot_table_data.tab_type_cat <- function(row, mapping) {
  rowvars <- row$RowVars[[1]]
  colvars <- dplyr::coalesce(row$ColVars, mapping$options$l_macro_scenario$ColVar)
  weightvar <- dplyr::coalesce(row$Weight, mapping$options$l_macro_scenario$Weight)
  if (is.na(weightvar)) {
    weightvar = character()
  }

  long_cols <- c(
    rowvars |> purrr::set_names(paste0("rowvar_", rowvars)),
    colvars |> purrr::set_names(paste0("colvar_", colvars)),
    weightvar |> purrr::set_names(paste0("weight_", weightvar))
  )
  mapping$dat_mod |>
    dplyr::filter(!!!rlang::parse_exprs(row$Filter[[1]])) |>
    dplyr::select(!!!long_cols) |>
    tidyr::pivot_longer(
      matches("^rowvar_"),
      names_pattern = "rowvar_(.*)",
      names_to = "rowvar",
      values_to = "rowval",
      values_transform = strip_attributes
    ) |>
    tidyr::pivot_longer(
      matches("^colvar_"),
      names_pattern = "colvar_(.*)",
      names_to = "colvar",
      values_to = "colval",
      values_transform = strip_attributes
    )
}
pivot_table_data.tab_type_mw <- pivot_table_data.tab_type_cat
