gen_col_tables <- function(mapping) {
  mapping$ditw$ct$db_tables$head_table <- gen_head_table(mapping)
  gen_col_table(mapping)
}

get_raw_data <- function(qtab) {
  UseMethod("get_raw_data")
}
get_raw_data.qtab_type_mdg <- function(qtab) {
  res <- get_raw_data.default(qtab)
  if (is.na(suppressWarnings(as.numeric(qtab$p$MdgVal)))) {
    rowvars <- qtab$p$l_selvar$valid %||% qtab$p$rowvars_valid_qtab
    rowvars_named <- rowvars |> purrr::set_names(rv(rowvars))
    res <- as.data.frame(res)
    res[names(rowvars_named)] <- catrec(
      res[names(rowvars_named)] |>
        unlist(use.names = FALSE),
      paste0("(", qtab$p$MdgVal, " = 1)")
    )
    qtab$p$MdgVal = 1
  } else {
    qtab$p$MdgVal = as.numeric(qtab$p$MdgVal)
  }
  res
}
get_raw_data.default <- function(qtab) {
  colvars <- qtab$p$ColVar
  colvars_named <- colvars |> purrr::set_names(cv(colvars))
  weightvar <- qtab$p$Weight[[1]]
  row_in_filter <- get_row_filter_lgl(qtab)


  if (is.null(qtab$p$SelVar)) {
    rowvars <- qtab$p$rowvars_qtab
    dat <- prep_data(
      qtab,
      rowvars = rowvars,
      new_rowvars = rv(rowvars),
      colvars_named = colvars_named,
      weightvar = weightvar,
      row_in_filter = row_in_filter
    )
  } else {
    # treat selvar:

    dat <- seq_along(qtab$p$SelVar) |> lapply(\(i) {
      rowvars <- c(qtab$p$l_selvar$rowvars[[i]], qtab$p$l_selvar$rowvars_inv[[i]])
      new_rowvars <- rv(c(qtab$p$l_selvar$valid, qtab$p$l_selvar$invalid))
      selvar_name <- qtab$p$SelVar[i]
      selval <- qtab$p$SelVal
      res <- prep_data(
        qtab,
        rowvars = rowvars,
        new_rowvars = new_rowvars,
        colvars_named = colvars_named,
        weightvar = weightvar,
        row_in_filter = row_in_filter & selvar_eq_selval(qtab$m$dat_tab[[selvar_name]], selval)
      )
      res$selvar = selvar_name
      res$selval = selval
      res
    }) |>
      dplyr::bind_rows()
  }
  # for TOTAL column:
  dat$"colvar_DC#TOTAL" <- 1
  dat$i <- seq_len(nrow(dat))
  dat
}
prep_data <- function(
    qtab,
    rowvars,
    new_rowvars,
    colvars_named,
    weightvar,
    row_in_filter
) {
  rowvars_named <- rowvars |> purrr::set_names(new_rowvars)
  if (!is.null(weightvar)) {
    weightvar <- weightvar |> purrr::set_names("weight")
  }

  long_cols <- c(
    c(row = "row"),
    rowvars_named,
    colvars_named,
    weightvar
  )

  # same as:
  # mapping$dat_tab |>
  #   dplyr::filter(!!!rlang::parse_exprs(df_row$Filter[[1]])) |>
  #   dplyr::select(!!!long_cols) |>
  #   dplyr::mutate(across(everything(), strip_attributes))
  # ... but with base R (for better performance)
  df <- qtab$m$dat_tab
  df$row <- seq_len(nrow(df))
  dat <- df[row_in_filter, long_cols]
  names(dat) <- names(long_cols)
  # remove label information:
  for (col in names(dat)) {
    attributes(dat[[col]]) <- NULL
  }
  if (!is.null(weightvar)) {
    weight_not_pos <- dat[["weight"]] <= 0
    if (any(weight_not_pos)) {
      dat <- dat[!weight_not_pos,]
      warning("There are cases where the weight variable is not positive. They were removed.")
    }
  }
  dat
}

get_row_filter_lgl <- function(qtab) {
  if (length(qtab$p$Filter) == 0) {
    return(TRUE)
  }
  filter_exprs <- rlang::parse_exprs(qtab$p$Filter)
  row_lgls <- filter_exprs |> purrr::map(\(e) rlang::eval_tidy(e, qtab$m$dat_tab))
  all_true(row_lgls)
}
selvar_eq_selval <- function(selvar, selval) {
  if (!is.na(as.numeric(selval) |> suppressWarnings())) {
    return(selvar == as.numeric(selval))
  }
  catrec(vec = selvar, paste0("(", selval, " = 1)"), 0) == 1
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
pivot_rowvar_data <- function(qtab) {
  UseMethod("pivot_rowvar_data")
}
pivot_rowvar_data.default <- function(qtab) {
  res <- qtab$d$raw_data |>
    pivot_rows()
  if (!qtab$p$Mult) {
    res <- res[
      !duplicated(res[c("rowvar", "row")], fromLast = TRUE),
    ]
  }
  res
}
pivot_rowvar_data.qtab_type_mdg <- function(qtab) {
  res <- pivot_rowvar_data.default(qtab)

  mdg_val <- qtab$p$MdgVal

  invalids <- qtab$p$l_selvar$invalid %||% qtab$p[["Unguelt"]]

  # TODO: find cleaner solution for df_rowvar_long_with_invalids
  qtab$d$df_rowvar_long_with_invalids <- res
  res[res$rowval == mdg_val,] |>
    add_exclusive_info(
      c(qtab$p[["Exclusive"]], invalids),
      "rowvar"
    )
}
pivot_rowvar_data.qtab_type_mcg <- function(qtab) {
  df_rows_long <- pivot_rowvar_data.default(qtab) |>
    # remove duplicated choices:
    dplyr::distinct(dplyr::across(dplyr::all_of(
      c("i", "rowval")
    )), .keep_all = TRUE)

  df_long <- add_exclusive_info(
    df_rows_long,
    c(qtab$p[["Exclusive"]], qtab$p[["Unguelt"]]),
    "rowval"
  )
  df_long$rowvar <- qtab$p$rowvars_string

  df_long
}
now_do_colvar <- function(qtab) {
  UseMethod("now_do_colvar")
}
now_do_colvar.default <- function(qtab) {
  qtab$d$df_rowvar_long |>
    pivot_cols()
}
now_do_colvar.qtab_type_mcg <- function(qtab) {
  res <- now_do_colvar.default(qtab)

  res$rowvar <- qtab$p$rowvars_string
  res
}
now_do_colvar.qtab_type_mdg <- function(qtab) {
  qtab$d$long_data_all <- qtab$d$df_rowvar_long_with_invalids |>
    pivot_cols()
  res <- now_do_colvar.default(qtab)
  res
}

gen_val_table <- function(qtab) {
  row_table <- qtab$d$row_table |> rm_header_footer()

  col_table <- qtab$d$col_table

  tab_values <- qtab$d$tab_values
  res <- tab_values |>
    merge(row_table |> dplyr::rename(rowval = RowValue, rowvar = RowVariable)) |>
    merge(col_table |> dplyr::rename(colval = ColValue, colvar = ColVariable))

  is_stat_row <- row_table$RowContent |> stringr::str_detect("^M?Statistics$")
  res[c("RowNo", "ColNo", "value")] |>
    # add rows with value = NA for every RowNo x ColNo combination not occuring in the data:
    tidyr::complete(
      RowNo = row_table$RowNo[is_stat_row],
      ColNo = col_table$ColNo
    ) |>
    # add rows with value = 0 for every RowNo x ColNo combination not occuring in the rest of the data:
    tidyr::complete(
      RowNo = row_table$RowNo,
      ColNo = col_table$ColNo,
      fill = list(value = 0),
      explicit = FALSE
    )
}
