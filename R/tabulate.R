tabulate_row <- function(mapping) {
  types <- mapping$qsheet$qsheet_processed$Type
  l <- mapping$qsheet$qsheet_processed |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    # to reduce the df_row dataframes to non-empty columns, uncomment:
    # purrr::map(\(x) x[!is.na(as.list(x))]) |>
    purrr::map2(
      types,
      \(df_row, type) new_tab_row(df_row, type)
    )
  mapping$qsheet$tables <- mapping$qsheet$qsheet_processed[c("row", "Type")] |> tidyr::unnest(Type)
  mapping$qsheet$tables$object <- l
  mapping$qsheet$tab_table <- gen_tab_table(mapping$qsheet$tables)
  mapping$qsheet$head_table <- gen_head_table(mapping)
  mapping$qsheet$col_table <- gen_col_table(mapping)
  mapping$qsheet$tables$raw_data <- l |>
    purrr::map(\(df_row) get_raw_data(df_row, mapping))
  # mapping$qsheet$tables$long_data <- l |>
  #   purrr::map(\(df_row) pivot_table_data(df_row, mapping))
  mapping$qsheet$tables$long_data <- purrr::map2(
    mapping$qsheet$tables$object,
    mapping$qsheet$tables$raw_data,
    \(df_row, raw_data) pivot_table_data(df_row, raw_data, mapping)
  )
  mapping$qsheet$tables$counts <- purrr::map2(
    mapping$qsheet$tables$object,
    mapping$qsheet$tables$long_data,
    \(df_row, long_data) crosstab(df_row, long_data, mapping)
  )
  mapping$qsheet$tables$row_table <- purrr::map(
    mapping$qsheet$tables$object,
    \(df_row, counts) gen_row_table(df_row, mapping)
  )
  mapping$qsheet$tables$val <- purrr::pmap(
    list(
      df_row = mapping$qsheet$tables$object,
      counts = mapping$qsheet$tables$counts,
      row_table = mapping$qsheet$tables$row_table
    ),
    \(df_row, counts, row_table) gen_val_table(df_row, counts, row_table, mapping)
  )
}

new_tab_row <- function(df_row, subclass) {
  class(df_row) <- c(paste0("tab_type_", subclass), class(df_row))
  df_row
}

get_raw_data <- function(df_row, mapping) {
  UseMethod("get_raw_data")
}
get_raw_data.default <- function(df_row, mapping) {
  rowvars <- df_row$RowVar[[1]]
  colvars <- mapping$options$l_macro_scenario$ColVar
  weightvar <- dplyr::coalesce(df_row$Weight, mapping$options$l_macro_scenario$Weight)
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
    #   dplyr::filter(!!!rlang::parse_exprs(df_row$Filter[[1]])) |>
    #   dplyr::select(!!!long_cols) |>
    #   dplyr::mutate(across(everything(), strip_attributes))
    # ... but with base R (for better performance)
    if (length(df_row$Filter[[1]]) == 0) {
      row_lgl <- TRUE
    } else {
      filter_exprs <- rlang::parse_exprs(df_row$Filter[[1]])
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
  prep_data()
}
pivot_table_data <- function(df_row, raw_data, mapping) {
  UseMethod("pivot_table_data")
}
pivot_table_data.tab_type_cat <- function(df_row, raw_data, mapping) {
  # for TOTAL column:
  raw_data$"colvar_DC#STICHPROBE" <- 1

  raw_data |>
    pivot_rows() |>
    pivot_cols()
}
pivot_cols <- function(df) {
  df |>
    tidyr::pivot_longer(
      matches("^colvar_"),
      names_pattern = "colvar_(.*)",
      names_to = "colvar",
      values_to = "colval"
    )
}
pivot_rows <- function(df) {
  df |>
    tidyr::pivot_longer(
      matches("^rowvar_"),
      names_pattern = "rowvar_(.*)",
      names_to = "rowvar",
      values_to = "rowval"
    )
}
pivot_table_data.tab_type_mw <- pivot_table_data.tab_type_cat
pivot_table_data.tab_type_mdg <- function(df_row, raw_data, mapping) {
  df_long <- pivot_table_data.tab_type_cat(df_row, raw_data, mapping)
  mdg_val <- dplyr::coalesce(df_row$MdgVal |> as.numeric(), 1)
  df_long[df_long$rowval == mdg_val,]
}
pivot_table_data.tab_type_mcg <- function(df_row, raw_data, mapping) {
  #TODO: calculate before!...:
  invalid_vals <- df_row$Unguelt[[1]]
  if (is.na(invalid_vals[1])) {
    invalid_vals <- mapping$options$l_macro_scenario$Unguelt
  }
  # for TOTAL column:
  raw_data$"colvar_DC#STICHPROBE" <- 1

  row_var_data <- raw_data[stringr::str_subset(names(raw_data), "^rowvar_")]
  any_in_row_filled <- rowSums(!is.na(row_var_data)) > 0

  n_valids_in_row <- apply(row_var_data, 1, \(r) sum(!unique(r) %in% invalid_vals))


  raw_data$any_in_row_filled <- any_in_row_filled
  raw_data$n_valids_in_row <- n_valids_in_row


  col_long_data <- raw_data |>
    pivot_cols()

  #TODO:
  col_long_data |> select(-matches("rowvar")) |> group_by(colvar, colval) |> summarise(sum(n_valids_in_row))
  col_long_data |> select(-matches("rowvar"), -n_valids_in_row) |> gen_total_counts(NA)
  df_long <- col_long_data |> pivot_rows()
  rowvars <- unique(df_long$rowvar) |> paste(collapse = ", ")
  res <- df_long
  res$rowvar <- rowvars
  res
}


gen_val_table <- function(df_row, counts, row_table, mapping) {
  UseMethod("gen_val_table")
}
gen_val_table.tab_type_cat <- gen_val_table.tab_type_mcg <- function(df_row, counts, row_table, mapping) {
  df_unique_rowvar_val <- row_table[!is.na(row_table$RowValue),c("RowVariable", "RowValue")] |>
    dplyr::distinct()

  row_levels <- paste(df_unique_rowvar_val$RowVariable, df_unique_rowvar_val$RowValue)
  # row_levels <- row_table$RowValue[!is.na(row_table$RowValue)] |>
  #   unique()

  # TODO: calculate before to prevent repeated calculation for every table...:
  col_table <- mapping$qsheet$col_table[-c(1:3),]
  col_levels <- paste(col_table$ColVariable, col_table$ColValue)
  # The following is equivalent to:
  # counts |>
  #   dplyr::transmute(
  #     RowNo = factor(rowval, row_levels) |> as.numeric(),
  #     ColNo = as.numeric(factor(paste(colvar, colval), col_levels)) + 3,
  #     Value = value
  #   ) |>
  #   dplyr::arrange(RowNo, ColNo)

  # but faster, with base R...:
  res <- counts["value"]
  # res$RowNo <- factor(counts$rowval, row_levels) |> as.numeric()
  res$RowNo <- factor(paste(counts$rowvar, counts$rowval), row_levels) |> as.numeric()
  res$ColNo <- as.numeric(factor(paste(counts$colvar, counts$colval), col_levels)) + 3
  res[order(res$RowNo, res$ColNo), c("RowNo", "ColNo", "value")]
}

gen_val_table.tab_type_mw <- gen_val_table.tab_type_mdg <- function(df_row, counts, row_table, mapping) {
  row_levels <- row_table$RowVariable[!is.na(row_table$RowVariable)] |>
    unique()

  # TODO: calculate before to prevent repeated calculation for every table...:
  col_table <- mapping$qsheet$col_table[-c(1:3),]
  col_levels <- paste(col_table$ColVariable, col_table$ColValue)
  # The following is equivalent to:
  # counts |>
  #   dplyr::transmute(
  #     RowNo = factor(rowval, row_levels) |> as.numeric(),
  #     ColNo = as.numeric(factor(paste(colvar, colval), col_levels)) + 3,
  #     Value = value
  #   ) |>
  #   dplyr::arrange(RowNo, ColNo)

  # but faster, with base R...:
  res <- counts["value"]
  res$RowNo <- factor(counts$rowvar, row_levels) |> as.numeric()
  res$ColNo <- as.numeric(factor(paste(counts$colvar, counts$colval), col_levels)) + 3
  res[order(res$RowNo, res$ColNo), c("RowNo", "ColNo", "value")]
}

