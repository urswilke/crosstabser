tabulate_row <- function(mapping) {
  types <- mapping$qsheet$qsheet_processed$Type
  l <- mapping$qsheet$qsheet_processed |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    # to reduce the row dataframes to non-empty columns, uncomment:
    # purrr::map(\(x) x[!is.na(as.list(x))]) |>
    purrr::map2(
      types,
      \(row, type) new_tab_row(row, type)
    )
  mapping$qsheet$tables <- mapping$qsheet$qsheet_processed[c("row", "Type")] |> tidyr::unnest(Type)
  mapping$qsheet$tables$object <- l
  mapping$qsheet$tables$long_data <- l |>
    purrr::map(\(row) pivot_table_data(row, mapping))
  mapping$qsheet$tables$counts <- purrr::map2(
    mapping$qsheet$tables$object,
    mapping$qsheet$tables$long_data,
    \(row, long_data) crosstab(row, long_data, mapping)
  )
  mapping$qsheet$tables$row <- purrr::map(
    mapping$qsheet$tables$object,
    \(row, counts) gen_row_table(row, mapping)
  )
  mapping$qsheet$tables$val <- purrr::map2(
    mapping$qsheet$tables$object,
    mapping$qsheet$tables$counts,
    \(row, counts) gen_val_table(row, counts, mapping)
  )
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
  rowvars <- row$RowVar[[1]]
  colvars <- dplyr::coalesce(row$ColPl, mapping$options$l_macro_scenario$ColPl)
  weightvar <- dplyr::coalesce(row$Weight, mapping$options$l_macro_scenario$Weight)
  if (is.na(weightvar)) {
    weightvar = character()
  }

  long_cols <- c(
    rowvars |> purrr::set_names(paste0("rowvar_", rowvars)),
    colvars |> purrr::set_names(paste0("colvar_", colvars)),
    weightvar |> purrr::set_names("weight")
  )
  prep_data <- function() {
    # same as:
    # mapping$dat_mod |>
    #   dplyr::filter(!!!rlang::parse_exprs(row$Filter[[1]])) |>
    #   dplyr::select(!!!long_cols) |>
    #   dplyr::mutate(across(everything(), strip_attributes))
    # ... but with base R (for better performance)
    if (length(row$Filter[[1]]) == 0) {
      row_lgl <- TRUE
    } else {
      filter_exprs <- rlang::parse_exprs(row$Filter[[1]])
      row_lgls <- filter_exprs |> purrr::map(\(e) rlang::eval_tidy(e, mapping$dat_mod))
      row_lgl <- all_true(row_lgls)
    }
    dat <- mapping$dat_mod[row_lgl, long_cols]
    names(dat) <- names(long_cols)
    for (col in seq_len(ncol(dat))) {
      attributes(dat[[col]]) <- NULL
    }
    dat
  }
  prep_data() |>
    tidyr::pivot_longer(
      matches("^rowvar_"),
      names_pattern = "rowvar_(.*)",
      names_to = "rowvar",
      values_to = "rowval"
    ) |>
    tidyr::pivot_longer(
      matches("^colvar_"),
      names_pattern = "colvar_(.*)",
      names_to = "colvar",
      values_to = "colval"
    )
}
pivot_table_data.tab_type_mw <- pivot_table_data.tab_type_cat
pivot_table_data.tab_type_mdg <- function(row, mapping) {
  df_long <- pivot_table_data.tab_type_cat(row, mapping)
  mdg_val <- coalesce(row$MdgVal |> as.numeric(), 1)
  df_long[df_long$rowval == mdg_val,]
}
pivot_table_data.tab_type_mcg <- function(row, mapping) {
  df_long <- pivot_table_data.tab_type_cat(row, mapping)
  rowvars <- unique(df_long$rowvar) |> paste(collapse = ", ")
  res <- df_long[df_long$rowvar == df_long$rowvar[1] | !df_long$rowval %in% mapping$options$l_macro_scenario$Unguelt,]
  res$rowvar <- rowvars
  res
}

crosstab <- function(row, long_data, mapping) {
  UseMethod("crosstab")
}
crosstab.tab_type_cat <- function(row, long_data, mapping) {
  weight <- dplyr::coalesce(mapping$options$l_macro_scenario$Weight, row$Weight)
  stat_fun <- row$ZsfgMW
  long_data |>
    dplyr::group_by(dplyr::across(-matches("weight"))) |>
    new_sum_stat(weight, stat_fun) |>
    apply_sum_stat()
}
crosstab.tab_type_mw <- crosstab.tab_type_cat
crosstab.tab_type_mcg <- crosstab.tab_type_cat
crosstab.tab_type_mdg <- crosstab.tab_type_cat

gen_val_table <- function(row, counts, mapping) {
  UseMethod("gen_val_table")
}
gen_val_table.tab_type_cat <- function(row, counts, mapping) {
  invalid_vals <- row$Unguelt[[1]]
  if (is.na(invalid_vals)) {
    invalid_vals <- mapping$options$l_macro_scenario$Unguelt
  }
  rowval_levels <- mapping$dat_mod[[row$RowVar[[1]]]] |>
    strip_attributes() |>
    unique() |>
    sort() |>
    c(invalid_vals)
  colvars <- dplyr::coalesce(
    row$ColPl,
    mapping$options$l_macro_scenario$ColPl
  )
  # The following is equivalent to:
  # counts |>
  #   dplyr::transmute(
  #     RowNo = factor(rowval, rowval_levels) |> as.numeric(),
  #     ColNo = factor(colvar, colvars) |> as.numeric(),
  #     Value = value
  #   ) |>
  #   dplyr::arrange(RowNo, ColNo)

  # but faster, with base R...:
  res <- counts["value"]
  res$RowNo <- factor(counts$rowval, rowval_levels) |> as.numeric()
  res$ColNo <- factor(counts$colvar, colvars) |> as.numeric()
  res[order(res$RowNo, res$ColNo), c("RowNo", "ColNo", "value")]
}

