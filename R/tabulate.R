gen_col_tables <- function(mapping) {
  mapping$qsheet$head_table <- gen_head_table(mapping)
  mapping$qsheet$col_table <- gen_col_table(mapping)
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
    return(dat)
  }
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
      row_in_filter = row_in_filter & selvar_eq_selval(qtab$m$dat_mod[[selvar_name]], selval)
    )
    res$selvar = selvar_name
    res$selval = selval
    res
  }) |>
    dplyr::bind_rows()
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
    rowvars_named,
    colvars_named,
    weightvar
  )

  # same as:
  # mapping$dat_mod |>
  #   dplyr::filter(!!!rlang::parse_exprs(df_row$Filter[[1]])) |>
  #   dplyr::select(!!!long_cols) |>
  #   dplyr::mutate(across(everything(), strip_attributes))
  # ... but with base R (for better performance)
  dat <- qtab$m$dat_mod[row_in_filter, long_cols]
  names(dat) <- names(long_cols)
  # remove label information:
  for (col in names(dat)) {
    attributes(dat[[col]]) <- NULL
  }
  dat
}

get_row_filter_lgl <- function(qtab) {
  if (length(qtab$p$Filter) == 0) {
    return(TRUE)
  }
  filter_exprs <- rlang::parse_exprs(qtab$p$Filter)
  row_lgls <- filter_exprs |> purrr::map(\(e) rlang::eval_tidy(e, qtab$m$dat_mod))
  all_true(row_lgls)
}
selvar_eq_selval <- function(selvar, selval) {
  if (!is.na(as.numeric(selval) |> suppressWarnings())) {
    return(selvar == as.numeric(selval))
  }
  # TODO: ask Wolf which possibilities are needed apart from e.g. "1-3" ...:
  selval_interval <- selval |> stringr::str_remove(":.*") |> stringr::str_split_1("-") |> as.numeric()
  selvar >=  selval_interval[1] & selvar <= selval_interval[2]
}

pivot_table_data <- function(qtab) {
  UseMethod("pivot_table_data")
}
pivot_table_data.qtab_type_mw <- pivot_table_data.qtab_type_cat <- function(qtab) {
  # for TOTAL column:
  df <- qtab$d$raw_data
  df$"colvar_DC#STICHPROBE" <- 1
  df$i <- seq_len(nrow(df))

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
pivot_table_data.qtab_type_mdg <- function(qtab) {
  df <- qtab$d$raw_data
  df$"colvar_DC#STICHPROBE" <- 1

  df$i <- seq_len(nrow(df))

  df_rows_long <- df |>
    pivot_rows()

  mdg_val <- qtab$p$MdgVal

  invalids <- qtab$p$l_selvar$invalid %||% qtab$p$Unguelt

  qtab$d$long_data <- df_rows_long[df_rows_long$rowval == mdg_val,] |>
    dplyr::mutate(
      val_to_count = flag_invalids(rowvar, invalids),
      .by = "i"
    ) |>
    pivot_cols()
}

pivot_table_data.qtab_type_mcg <- function(qtab) {
  # for TOTAL column:
  df <- qtab$d$raw_data
  df$"colvar_DC#STICHPROBE" <- 1

  df$i <- seq_len(nrow(df))

  df_rows_long <- df |>
    pivot_rows() |>
    # remove duplicated choices:
    dplyr::distinct(dplyr::across(dplyr::all_of(
      c("i", "rowval")
    )), .keep_all = TRUE)

  # TODO: add test to check if this works correctly!...:
  invalids_to_filter <- intersect(
    qtab$m$options$mapping_r_params$miss_rec_val,
    qtab$p$Unguelt
  )
  df_rows_long_invalids <- df_rows_long[df_rows_long$rowval %in% qtab$p$Unguelt,] |>
    dplyr::mutate(
      # TODO: also put into helper function like flag_exclusives() or flag_invalids() (?):
      temp = order(factor(rowval, levels = qtab$p$Unguelt)),
      # calculate boolean that's TRUE if:
      val_to_count =
        # for each case, only take count one invalid value (the one that occurs
        # first in the list of invalid values):
        temp == 1 &
        # values not equal to the value defined in the cell of the named region
        # "R_miss_rec_val" in the mapping file:
        !rowval %in% invalids_to_filter,
      temp = NULL,
    .by = "i")

  df_rows_long_valids <- df_rows_long[!df_rows_long$rowval %in% qtab$p$Unguelt,]
  exclusives <- qtab$p$Exclusive
  if (!is.null(exclusives)) {
    df_rows_long_valids <- df_rows_long_valids |>
      dplyr::mutate(
        val_to_count = flag_exclusives(rowval, exclusives),
        .by = "i"
      )
  } else {
    df_rows_long_valids$val_to_count <- TRUE
  }


  df_long <- dplyr::bind_rows(
    # TODO: remove rows of cases with multiple values in column "Ëxclusive"...:
    df_rows_long_valids,
    df_rows_long_invalids
  ) |>
    pivot_cols()

  rowvars <- qtab$p$rowvars_string
  df_long$rowvar <- rowvars
  qtab$d$long_data <- df_long
}
flag_exclusives <- function(rowval, exclusives) {
  # TODO:
  # - find a cleaner way for Exclusive!...:
  # - also needed for mdg?
  # - discuss with Wolf if something like in flag_invalids() can be done...:
  # - same needed for mcg?
  none_exclusive <- !any(rowval %in% exclusives)
  temp <- order(factor(rowval, levels = exclusives))
  # order doesn't deal correctly with levels not occuring in the factors.
  # Therefore we set these values to Inf to not select values not in the
  # set of exclusives here...
  temp <- ifelse(!rowval %in% exclusives, Inf, temp)
  first_exclusive <- temp %in% min(temp, na.rm = TRUE)
  first_exclusive | none_exclusive
  # same result but slower:
  # dplyr::case_when(
  #   !any(rowval %in% exclusives) ~ TRUE,
  #   !rowval %in% exclusives ~ FALSE,
  #   .default = {
  #     temp <- order(factor(rowval, levels = exclusives))
  #     temp[!rowval %in% exclusives] <- Inf
  #     temp == min(temp)
  #   }
  # )
}

flag_invalids <- function(rowvar, invalids) {
  is_valid <- !rowvar %in% invalids
  if (any(is_valid)) {
    return(is_valid)
  }
  # same as:
  # temp <- order(factor(rowvar, levels = invalids))
  # temp %in% min(temp, na.rm = TRUE)
  # but faster...:
  i <- match(rowvar, invalids) |> which.min()
  res <- rep(FALSE, length(rowvar))
  res[i] <- TRUE
  res
}

gen_val_table <- function(qtab) {
  row_table <- qtab$d$row_table |> rm_header_footer()

  col_table <- qtab$d$col_table

  tab_values <- qtab$d$tab_values
  res <- tab_values |>
    merge(row_table |> dplyr::rename(rowval = RowValue, rowvar = RowVariable)) |>
    merge(col_table |> dplyr::rename(colval = ColValue, colvar = ColVariable))

  # HACK: replace NA with 0 in the table data (replace implicit NA with explicit 0....)
  res[c("RowNo", "ColNo", "value")] |> tidyr::complete(
    RowNo = row_table$RowNo,
    ColNo = col_table$ColNo,
    fill = list(value = 0),
    explicit = FALSE
  )
}

gen_long_tab_data = function(mapping) {
  res <- mapping$qrows |>
    lapply(\(x) x$qtabs$obj |> lapply(\(x) x$d$long_tab)) |>
    dplyr::bind_rows() |>
    tidyr::drop_na(value) |>
    # TODO name value = Value from the beginiing or adapt:
    dplyr::relocate(TabNo, RowNo, ColNo, Value = value) |>
    dplyr::arrange(QuestNo, TabNo, RowNo, ColNo)
  df_total_values <- res[
    res$RowContent == "Valid" & res$RowAbsPercent == "Abs",
  ] |>
    dplyr::select(QuestNo, TabNo, ColNo, ColValidCases = Value)
  # TODO: maybe generalize to stats different from mean...?:
  is_mean <- if (!"RowStatFun" %in% names(res)) {
    FALSE
  } else {
    res$RowStatFun == "mean"
  }

  df_mean_values <- res[
    res$RowContent == "Statistics" & is_mean,
  ] |>
    dplyr::select(QuestNo, TabNo, ColNo, ColMean = Value)

  mapping$long_tab_data <- res |>
    dplyr::left_join(df_total_values, by = dplyr::join_by(TabNo, ColNo, QuestNo)) |>
    dplyr::left_join(df_mean_values, by = dplyr::join_by(TabNo, ColNo, QuestNo))
}
