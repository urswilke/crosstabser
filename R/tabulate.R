gen_tab_and_col_tables <- function(mapping) {
  mapping$qsheet$tables <- mapping$qsheet$qsheet_processed[c("row", "Type")] |> tidyr::unnest(Type)
  mapping$qsheet$tab_table <- gen_tab_table(mapping)
  mapping$qsheet$head_table <- gen_head_table(mapping)
  mapping$qsheet$col_table <- gen_col_table(mapping)
}

get_raw_data <- function(qtab) {
  UseMethod("get_raw_data")
}
get_raw_data.default <- function(qtab) {
  rowvars <- qtab$p$RowVar
  if (qtab$p$Type == "mdg") {
    rowvars <- c(rowvars, qtab$p$Unguelt)
  }
  colvars <- qtab$p$ColVar
  weightvar <- qtab$p$Weight[[1]]

  long_cols <- c(
    rowvars |> purrr::set_names(paste0("rowvar_", rowvars)),
    colvars |> purrr::set_names(paste0("colvar_", colvars)),
    weightvar
  )
  prep_data <- function() {
    # same as:
    # mapping$dat_mod |>
    #   dplyr::filter(!!!rlang::parse_exprs(df_row$Filter[[1]])) |>
    #   dplyr::select(!!!long_cols) |>
    #   dplyr::mutate(across(everything(), strip_attributes))
    # ... but with base R (for better performance)
    if (length(qtab$p$Filter) == 0) {
      row_lgl <- TRUE
    } else {
      filter_exprs <- rlang::parse_exprs(qtab$p$Filter)
      row_lgls <- filter_exprs |> purrr::map(\(e) rlang::eval_tidy(e, qtab$d$dat_mod))
      row_lgl <- all_true(row_lgls)
    }
    dat <- qtab$d$dat_mod[row_lgl, long_cols]
    names(dat) <- names(long_cols)

    # remove label information:
    for (col in seq_len(ncol(dat))) {
      attributes(dat[[col]]) <- NULL
    }
    dat
  }
  prep_data()
}
pivot_table_data <- function(qtab) {
  UseMethod("pivot_table_data")
}
pivot_table_data.qtab_type_mw <- pivot_table_data.qtab_type_cat <- function(qtab) {
  # for TOTAL column:
  df <- qtab$d$raw_data
  df$"colvar_DC#STICHPROBE" <- 1

  qtab$d$long_data <- df |>
    pivot_rows() |>
    pivot_cols()
}
pivot_cols <- function(df) {
  df |>
    tidyr::pivot_longer(
      dplyr::matches("^colvar_"),
      names_pattern = "colvar_(.*)",
      names_to = "colvar",
      values_to = "colval",
      values_drop_na = TRUE
    )
}
pivot_rows <- function(df) {
  df |>
    tidyr::pivot_longer(
      dplyr::matches("^rowvar_"),
      names_pattern = "rowvar_(.*)",
      names_to = "rowvar",
      values_to = "rowval",
      values_drop_na = TRUE
    )
}
pivot_table_data.qtab_type_mdg <- function(qtab) {
  df_long <- pivot_table_data.qtab_type_cat(qtab)
  mdg_val <- qtab$p$MdgVal
  qtab$d$long_data <- df_long[df_long$rowval == mdg_val,]
}

pivot_table_data.qtab_type_mcg <- function(qtab) {
  # for TOTAL column:
  df <- qtab$d$raw_data
  df$"colvar_DC#STICHPROBE" <- 1

  df$i <- seq_len(nrow(df))

  df_long <- df |>
    pivot_rows() |>
    pivot_cols()

  # invalid_vals <- qtab$p$Unguelt
  # res <- df_long[!df_long$rowval %in% invalid_vals,] |>
  res <- df_long |>
    # remove duplicated choices:
    dplyr::distinct(dplyr::across(dplyr::all_of(
      c("i", "rowval", "colvar", "colval")
    )))
  rowvars <- unique(df_long$rowvar) |> paste(collapse = ", ")
  res$rowvar <- rowvars
  qtab$d$long_data <- res
}


gen_val_table <- function(qtab) {
  row_table <- qtab$d$row_table[!is.na(qtab$d$row_table$RowVariable),]

  col_table <- qtab$d$col_table

  tab_values <- qtab$d$tab_values
  res <- tab_values |>
    merge(row_table |> dplyr::rename(rowval = RowValue, rowvar = RowVariable), all = TRUE) |>
    merge(col_table |> dplyr::rename(colval = ColValue, colvar = ColVariable), all = TRUE) |>
    dplyr::as_tibble()
  res[order(res$RowNo, res$ColNo), c("RowNo", "ColNo", "value")]
}
