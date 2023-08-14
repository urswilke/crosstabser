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
  weightvar <- qtab$p$Weight
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
  invalid_vals <- qtab$p$Unguelt
  # for TOTAL column:
  df <- qtab$d$raw_data
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
  weight <- qtab$p$Weight

  cols <- stringr::str_subset(names(col_long_data), "^(colva[rl]|weight)$")
  qtab$d$total_row_counts <-
    col_long_data[col_long_data$any_in_row_filled, cols] |>
    gen_total_counts(weight)
  qtab$d$sum_of_valid_counts <-
    col_long_data[col_long_data$any_in_row_filled, c(cols, "n_valids_in_row")] |>
    dplyr::summarise(
      value = sum(n_valids_in_row * dplyr::coalesce(weight |> as.numeric(), 1)),
      .by = -dplyr::matches("^(weight|n_valids_in_row)$")
    )

  cols <- stringr::str_subset(
    names(col_long_data),
    "^(any_in_row_filled|n_valids_in_row)$",
    negate = TRUE
  )
  df_long <- col_long_data[cols] |> pivot_rows()
  rowvars <- unique(df_long$rowvar) |> paste(collapse = ", ")
  df_long$rowvar <- rowvars
  qtab$d$long_data <- df_long
}


gen_val_table <- function(qtab) {
  UseMethod("gen_val_table")
}
gen_val_table.qtab_type_mcg <- gen_val_table.qtab_type_cat <- function(qtab) {
  row_table <- qtab$d$row_table[!is.na(qtab$d$row_table$RowVariable),]
  df_unique_rowvar_val <- row_table[!is.na(row_table$RowValue), c("RowVariable", "RowValue")] |>
    dplyr::distinct()

  row_levels <- paste(row_table$RowContent, row_table$RowAbsPercent, row_table$RowVariable, row_table$RowValue)
  # row_levels <- row_table$RowValue[!is.na(row_table$RowValue)] |>
  #   unique()

  # TODO: calculate before to prevent repeated calculation for every table...:
  col_table <- qtab$d$col_table[-c(1:3),]
  col_levels <- paste(col_table$ColVariable, col_table$ColValue)
  tab_values <- qtab$d$tab_values
  # The following is equivalent to:
  # tab_values |>
  #   dplyr::transmute(
  #     RowNo = factor(rowval, row_levels) |> as.numeric(),
  #     ColNo = as.numeric(factor(paste(colvar, colval), col_levels)) + 3,
  #     Value = value
  #   ) |>
  #   dplyr::arrange(RowNo, ColNo)

  # but faster, with base R...:
  res <- tab_values["value"]
  # res$RowNo <- factor(tab_values$rowval, row_levels) |> as.numeric()
  res$RowNo <- factor(paste(tab_values$RowContent, tab_values$RowAbsPercent, tab_values$rowvar, tab_values$rowval), row_levels) |> as.numeric()
  res$ColNo <- as.numeric(factor(paste(tab_values$colvar, tab_values$colval), col_levels)) + 3
  res[order(res$RowNo, res$ColNo), c("RowNo", "ColNo", "value")]
}

gen_val_table.qtab_type_mw <- gen_val_table.qtab_type_mdg <- function(qtab) {
  # TODO: remove filtering together with unneeded rows in row_table!...:
  row_table <- qtab$d$row_table[!is.na(qtab$d$row_table$RowVariable),]
  col_table <- qtab$d$col_table[-c(1:3),]

  row_levels <- paste(row_table$RowContent, row_table$RowAbsPercent, row_table$RowVariable, row_table$RowValue)

  col_table <- qtab$d$col_table[-c(1:3),]
  col_levels <- paste(col_table$ColVariable, col_table$ColValue)
  tab_values <- qtab$d$tab_values

  res <- tab_values["value"]
  res$RowNo <- factor(paste(tab_values$RowContent, tab_values$RowAbsPercent, tab_values$rowvar, tab_values$rowval), row_levels) |> as.numeric()
  res$ColNo <- as.numeric(factor(paste(tab_values$colvar, tab_values$colval), col_levels)) + 3
  res[order(res$RowNo, res$ColNo), c("RowNo", "ColNo", "value")]
}

