five_table_names <- c("row_table", "col_table_all", "val_table", "head_table", "tab_table")
frame_table_parts <- function(qrows) {
  qtab <- qrows |> lapply(\(x) x$qtabs)
  warn <- qrows |> purrr::map(\(x) x$log$warn)
  error <- qrows |> purrr::map(\(x) x$log$error)
  # all qrows have an error:
  if (all(!purrr::map_lgl(error, is.null))) {
    stop("No crosstabs calculated")
  }

  QuestNo <- qrows |> purrr::map_chr(\(x) x$p$Abbreviation) |> forcats::as_factor()
  dplyr::tibble(QuestNo, qtab) |>
    tidyr::unnest_longer(c(qtab), keep_empty = TRUE) |>
    dplyr::mutate(l = purrr::map(qtab, \(x) x$d[five_table_names])) |> tidyr::unnest_wider(l)
}
assemble_crosstab_data_ <- function(obj) {
  UseMethod("assemble_crosstab_data_")
}
assemble_crosstab_data_.Tabula <- function(obj) {
  extract_5_tables(obj$qrows, obj)
}
assemble_crosstab_data_.Qrow <- function(obj) {
  extract_5_tables(list(obj), obj$m)
}
extract_5_tables <- function(qrows, mapping) {
  table_parts <- frame_table_parts(qrows)

  q <- table_parts$QuestNo
  tab_table <- table_parts$qtab |> purrr::map_dfr(\(x) x$d$tab_table)
  val_table <- table_parts |>
    dplyr::select(tab_table, val_table) |>
    tidyr::unpack(tab_table) |>
    dplyr::select(QuestNo, TabNo, val_table) |>
    tidyr::unnest(val_table)
  row_table <- table_parts$qtab |> purrr::set_names(q) |> purrr::map_dfr(\(x) x$d$row_table, .id = "QuestNo")
  head_table <- mapping$qsheet$head_table
  col_table_all <- mapping$qsheet$col_table_all

  data <- tibble::lst(
    tab_table,
    val_table,
    row_table,
    head_table,
    col_table_all
  )

  tibble::lst(
    table_parts,
    data
  )
}
add_columns_for_tablebook <- function(obj, BookNo) {
  five_tables <- obj$crosstabs$data
  res <- five_tables |>
    lapply(\(x) dplyr::mutate(x, BookNo, .before = 1))

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
      # "\\u2696" is the unicode escape for the weight sign
      # TODO: get from cTabWeighted...!:
      RowContentDetail = dplyr::if_else(grepl("Statistics$", RowContent), sub("\u2696", "", RowTitle3), ""),
    )
  res$head_table <- res$head_table |> dplyr::left_join(
    res$col_table_all |> dplyr::count(HeadNo, name = "HeadCount"),
    by = "HeadNo"
  )
  res$col_table_all
  res$val_table <- res$val_table |> dplyr::rename(Value = value)

  obj$crosstabs$data <- res
}

write_to_db_ <- function(obj, dsn, errors, warns, book_no, questno, is_first) {
  five_tables <- obj$crosstabs$data |> order_tables()
  conn <- DBI::dbConnect(odbc::odbc(), dsn = dsn)
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbWriteTable(conn, "Tab", five_tables$tab_table, append = TRUE)
  DBI::dbWriteTable(conn, "Row", five_tables$row_table, append = TRUE)
  if (is_first) {
    DBI::dbWriteTable(conn, "Head", five_tables$head_table, append = TRUE)
    DBI::dbWriteTable(conn, "Col", five_tables$col_table_all, append = TRUE)
    # works/needed only if obj is of class Qrow:
    if (!is.null(obj$m)) obj$m$opts$ct$is_first <- FALSE
  }
  DBI::dbWriteTable(conn, "Val", five_tables$val_table, append = TRUE)

  sql = ""

  # change Quest table for each QuestNo line by line:
  for (i in seq_along(errors)) {
    #PostgreSQL dialect
    sql <- paste0(
      sql,
      DBI::sqlInterpolate(
        conn,
        'UPDATE "Quest"
          SET "EndTime" = CURRENT_TIMESTAMP,
          "CountRow" = ?count_row,
          "ErrorLog" = ?error_log,
          "WarnLog" = ?warn_log
          WHERE ("QuestNo" = ?questno) AND ("BookNo" = ?book_no)',
        count_row = nrow(five_tables$row_table),
        error_log = errors[[i]] %||% "",
        warn_log = warns[[i]] %||% "",
        questno = questno[[i]],
        book_no = book_no
      ),
      sep = ";"
    )
  }
  DBI::dbExecute(conn, sql)
}

order_tables <- function(five_tables) {
  five_tables$tab_table <- dplyr::select(five_tables$tab_table, BookNo, QuestNo, QuestLine, TabNo, TabName, TabType, TabTitle, TabTitle1, TabTitle2, TabTitle3, TabRowTypes, TabCaption, TabCount)
  five_tables$row_table <- dplyr::select(five_tables$row_table,  BookNo, QuestNo, RowNo, TabNo, RowTypeS, RowType, RowContent, RowContentDetail, RowAbsPercent, RowWeighted, RowTitle1, RowTitle2, RowTitle3, RowDecimals, RowVariable, RowValue)
  five_tables$head_table <- dplyr::select(five_tables$head_table, BookNo, HeadNo, HeadName, HeadTitle, HeadCount)
  five_tables$col_table_all <- dplyr::select(five_tables$col_table_all, BookNo, ColNo, HeadNo, ColTitle1, ColTitle2, ColVariable, ColValue)
  five_tables$val_table <- dplyr::select(five_tables$val_table, BookNo, QuestNo, TabNo, RowNo, ColNo, Value)
  five_tables
}
