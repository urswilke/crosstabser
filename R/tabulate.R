gen_col_tables <- function(mapping) {
  mapping$qsheet$head_table <- gen_head_table(mapping)
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
  selval_interval <- selval |> stringr::str_split_1("-") |> as.numeric()
  selvar >=  selval_interval[1] & selvar <= selval_interval[2]
}

pivot_table_data <- function(qtab) {
  UseMethod("pivot_table_data")
}
pivot_table_data.qtab_type_mw <- pivot_table_data.qtab_type_cat <- function(qtab) {
  # for TOTAL column:
  df <- qtab$d$raw_data
  df$"colvar_DC#TOTAL" <- 1
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
  df$"colvar_DC#TOTAL" <- 1

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
  df$"colvar_DC#TOTAL" <- 1

  df$i <- seq_len(nrow(df))

  df_rows_long <- df |>
    pivot_rows() |>
    # remove duplicated choices:
    dplyr::distinct(dplyr::across(dplyr::all_of(
      c("i", "rowval")
    )), .keep_all = TRUE)

  # TODO: add test to check if this works correctly!...:
  invalids_to_filter <- intersect(
    # TODO: put datenanpassr Mapping$params in the same structure as Tabula$p
    qtab$m$params$miss_rec_val,
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
    # TODO: remove rows of cases with multiple values in column "Exclusive"...:
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


five_table_names <- c("row_table", "col_table_all", "val_table", "head_table", "tab_table")
frame_table_parts <- function(tabula) {
  qrow <- tabula$qrows
  qtab <- qrow |> lapply(\(x) x$qtabs)
  warn <- tabula$qrows |> purrr::map(\(x) x$log$warn)
  error <- tabula$qrows |> purrr::map(\(x) x$log$error)
  QuestNo <- qrow |> purrr::map_chr(\(x) x$p$Abbreviation) |> forcats::as_factor()
  dplyr::tibble(QuestNo, qtab) |>
    tidyr::unnest_longer(c(qtab), keep_empty = TRUE) |>
    dplyr::mutate(l = purrr::map(qtab$obj, \(x) x$d[five_table_names])) |> tidyr::unnest_wider(l)
}

aggregate_5_tables <- function(tabula) {
  # TODO: remove older data structures and refactor the code using `table_parts`...!
  table_parts <- frame_table_parts(tabula)
  tabula$crosstabs$table_parts <- table_parts

  row_has_error <- tabula$qrows |>
    lapply(\(x) x$log$error) |>
    tibble::tibble(a = _) |>
    tidyr::unnest(a, keep_empty = TRUE) |>
    dplyr::pull()
  # all qrows have an error:
  if (all(!is.na(row_has_error))) {
    stop("No crosstabs calculated")
  }
  q <- table_parts$QuestNo
  tab_table <- table_parts$qtab$obj |> purrr::map_dfr(\(x) x$d$tab_table)
  # TODO: when in the Val table in the database we also have TabNo,
  # we can remove all this ascending RowNo per QuestNo story (cf. ascend_rownos_within_questno())
  # and refactor everything...
  # val_table <- table_parts$qtab$obj |> purrr::set_names(q) |> purrr::map_dfr(\(x) x$d$val_table, .id = "QuestNo")
  val_table <- table_parts |>
    dplyr::select(tab_table, val_table) |>
    tidyr::unpack(tab_table) |>
    dplyr::select(QuestNo, TabNo, val_table) |>
    tidyr::unnest(val_table)
  row_table <- table_parts$qtab$obj |> purrr::set_names(q) |> purrr::map_dfr(\(x) x$d$row_table, .id = "QuestNo")
  head_table <- tabula$qsheet$head_table
  col_table_all <- tabula$qsheet$col_table_all

  tabula$crosstabs$data <- tibble::lst(
    tab_table,
    val_table,
    row_table,
    head_table,
    col_table_all
  )
}
add_columns_for_tablebook <- function(tabula) {
  five_tables <- tabula$crosstabs$data
  res <- five_tables |>
    lapply(\(x) dplyr::mutate(x, BookNo = tabula$options$V_BookNo, .before = 1))

  # the group_by QuestNo, TabNo only works if QuestNo is unique
  # see also in ascend_rownos_within_questno()
  # => perhaps better additionally group_by QuestLine and allow duplicated QuestNo?
  # TODO: discuss with Wolf if it wouldn't be better to allow empty strings, and it that case replace QuestNO with QuestLine... / add _<index> to duplicated `QuestNo`s...!
  df_tabcount <- res$row_table |>
    dplyr::group_by(QuestNo, TabNo) |>
    dplyr::summarise(TabCount = dplyr::n(), .groups = "drop")

  res$tab_table <- res$tab_table |> dplyr::mutate(
    TabName = paste0(
      TabType,
      "#",
      ifelse(!is.na(repov_name), repov_name, ""),
      QuestNo,
      ifelse(!is.na(SelVal), paste0("_", SelVal), ""),
      "@",
      dplyr::row_number()
    ),
    .by = c("QuestNo", "TabType", "SelVal", "repov_name"),
    .after = "QuestNo"
  ) |>
    dplyr::full_join(df_tabcount, by = c("QuestNo", "TabNo")) |>
    dplyr::mutate(TabRowTypes = NA_integer_)

  # constants for bitwise or operation for Row$RowTypeg
  cArt <- list()
  cArt$Title <- 1
  cArt$Header <- 2
  cArt$Total <- 256
  cArt$Detail <- 16
  cArt$Summary <- 32
  cArt$Statistics <- 64
  cArt$Valid <- 512
  cArt$Missing <- 1024
  cArt$Filter <- 2048
  cArt$Empty <- 4
  cArt$MStatistics <- 65536
  cArt$MValid <- 131072
  cArt$AbsWeighted <- 1048576
  cArt$AbsUnweighted <- 2097152
  cArt$PercentWeighted <- 16777216
  cArt$PercentUnweighted <- 33554432
  cArt$Abs <- 4194304
  cArt$Percent <- 67108864

  res$row_table <- res$row_table |>
    dplyr::mutate(
      RowTypeS = paste0(
        RowContent,
        dplyr::if_else(RowAbsPercent == "", "", "|"),
        RowAbsPercent,
        RowWeighted
      ),
      RowType = bitwOr(unlist(cArt[gsub("^.*\\|", "", RowTypeS)]), unlist(cArt[gsub("\\|.*$", "", RowTypeS)])),
      # TODO: discuss with Wolf if I should remove the space in front of the weight sign in the row labels...:
      # "\\u2696" is the unicode escape for the weight sign
      RowContentDetail = dplyr::if_else(grepl("Statistics$", RowContent), sub(" \u2696", "", RowTitle3), ""),
    )
  res$head_table <- res$head_table |> dplyr::left_join(
    res$col_table_all |> dplyr::count(HeadNo, name = "HeadCount"),
    by = "HeadNo"
  )
  res$col_table_all
  res$val_table <- res$val_table |> dplyr::rename(Value = value)

  tabula$crosstabs$data <- res
}

write_to_db <- function(tabula) {
  five_tables <- tabula$crosstabs$data |> order_tables()
  conn <- DBI::dbConnect(odbc::odbc(), dsn=tabula$params$database_dsn)

  DBI::dbWriteTable(conn, "Tab", five_tables$tab_table, append = TRUE)
  DBI::dbWriteTable(conn, "Row", five_tables$row_table, append = TRUE)
  DBI::dbWriteTable(conn, "Head", five_tables$head_table, append = TRUE)
  DBI::dbWriteTable(conn, "Col", five_tables$col_table_all, append = TRUE)
  DBI::dbWriteTable(conn, "Val", five_tables$val_table, append = TRUE)

  errors <- tabula$qrows |> lapply(\(x) x$log$error)
  warns <- tabula$qrows |> lapply(\(x) x$log$warn)
  sql = ""

  # change Quest table for each QuestNo line by line:
  for (i in seq_along(errors)) {

    #PostgreSQL dialect
    sql <- paste0(sql,
      DBI::sqlInterpolate(
        conn,
        'UPDATE "Quest"
          SET "EndTime" = CURRENT_TIMESTAMP,
          "CountRow" = 1,
          "ErrorLog" = ?error_log,
          "WarnLog" = ?warn_log
          WHERE ("QuestNo" = ?questno) AND ("BookNo" = ?book_no)',
        error_log = paste0("", errors[[i]]),
        warn_log = paste0("", warns[[i]]),
        questno = tabula$qrows[[i]]$p$Abbreviation,
        book_no = tabula$options$V_BookNo
      ),
    sep = ";")
  }
  DBI::dbExecute(conn, sql)
  DBI::dbDisconnect(conn)
}

order_tables <- function(five_tables) {
  five_tables$tab_table <- dplyr::select(five_tables$tab_table, BookNo, QuestNo, QuestLine, TabNo, TabName, TabType, TabTitle, TabTitle1, TabTitle2, TabTitle3, TabRowTypes, TabCaption, TabCount)
  five_tables$row_table <- dplyr::select(five_tables$row_table,  BookNo, QuestNo, RowNo, TabNo, RowTypeS, RowType, RowContent, RowContentDetail, RowAbsPercent, RowWeighted, RowTitle1, RowTitle2, RowTitle3, RowDecimals, RowVariable, RowValue)
  five_tables$head_table <- dplyr::select(five_tables$head_table, BookNo, HeadNo, HeadName, HeadTitle, HeadCount)
  five_tables$col_table_all <- dplyr::select(five_tables$col_table_all, BookNo, ColNo, HeadNo, ColTitle1, ColTitle2, ColVariable, ColValue)
  five_tables$val_table <- dplyr::select(five_tables$val_table, BookNo, QuestNo, TabNo, RowNo, ColNo, Value)
  five_tables
}
