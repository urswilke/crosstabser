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

get_raw_data <- function(tab) {
  UseMethod("get_raw_data")
}
get_raw_data.default <- function(tab) {
  rowvars <- tab$p$RowVar
  colvars <- tab$p$ColVar
  weightvar <- tab$p$Weight
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
    if (length(tab$p$Filter) == 0) {
      row_lgl <- TRUE
    } else {
      filter_exprs <- rlang::parse_exprs(tab$p$Filter)
      row_lgls <- filter_exprs |> purrr::map(\(e) rlang::eval_tidy(e, tab$d$dat_mod))
      row_lgl <- all_true(row_lgls)
    }
    dat <- tab$d$dat_mod[row_lgl, long_cols]
    names(dat) <- names(long_cols)
    for (col in seq_len(ncol(dat))) {
      attributes(dat[[col]]) <- NULL
    }
    dat
  }
  prep_data()
}
pivot_table_data <- function(tab) {
  UseMethod("pivot_table_data")
}
pivot_table_data.tab_type_cat <- function(tab) {
  # for TOTAL column:
  df <- tab$d$raw_data
  df$"colvar_DC#STICHPROBE" <- 1

  tab$d$long_data <- df |>
    pivot_rows() |>
    pivot_cols()
}
pivot_cols <- function(df) {
  df |>
    tidyr::pivot_longer(
      dplyr::matches("^colvar_"),
      names_pattern = "colvar_(.*)",
      names_to = "colvar",
      values_to = "colval"
    )
}
pivot_rows <- function(df) {
  df |>
    tidyr::pivot_longer(
      dplyr::matches("^rowvar_"),
      names_pattern = "rowvar_(.*)",
      names_to = "rowvar",
      values_to = "rowval"
    )
}
pivot_table_data.tab_type_mw <- pivot_table_data.tab_type_cat
pivot_table_data.tab_type_mdg <- function(tab) {
  df_long <- pivot_table_data.tab_type_cat(tab)
  mdg_val <- tab$p$MdgVal |> as.numeric()
  if (length(mdg_val) == 0) {
    mdg_val <- 1
  }
  tab$d$long_data <- df_long[df_long$rowval == mdg_val,]
}
pivot_table_data.tab_type_mcg <- function(tab) {
  #TODO: calculate before!...:
  invalid_vals <- tab$p$Unguelt
  # for TOTAL column:
  df <- tab$d$raw_data
  df$"colvar_DC#STICHPROBE" <- 1

  row_var_data <- df[stringr::str_subset(names(df), "^rowvar_")]
  any_in_row_filled <- rowSums(!is.na(row_var_data)) > 0

  n_valids_in_row <- apply(row_var_data, 1, \(r) sum(!unique(r) %in% invalid_vals))


  df$any_in_row_filled <- any_in_row_filled
  df$n_valids_in_row <- n_valids_in_row


  col_long_data <- df |>
    pivot_cols()

  # this already does counting, but on the data where only the colvar are
  # pivoted to long. Thus it's done here:
  weight <- tab$p$Weight

  cols <- stringr::str_subset(names(col_long_data), "^(colva[rl]|weight)$")
  tab$d$total_row_counts <-
    col_long_data[col_long_data$any_in_row_filled, cols] |>
    gen_total_counts(weight)
  tab$d$sum_of_valid_counts <-
    col_long_data[col_long_data$any_in_row_filled, c(cols, "n_valids_in_row")] |>
    # dplyr::group_by(dplyr::across(-matches("^(weight|n_valids_in_row)$"))) |>
    dplyr::summarise(
      value = sum(n_valids_in_row * dplyr::coalesce(weight |> as.numeric(), 1)),
      .by = -matches("^(weight|n_valids_in_row)$")
    )

  cols <- stringr::str_subset(
    names(col_long_data),
    "^(any_in_row_filled|n_valids_in_row)$",
    negate = TRUE
  )
  df_long <- col_long_data[cols] |> pivot_rows()
  rowvars <- unique(df_long$rowvar) |> paste(collapse = ", ")
  df_long$rowvar <- rowvars
  tab$d$long_data <- df_long
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

