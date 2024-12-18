frame_table_parts <- function(qrow) {
  warn <- qrow$log$warn
  error <- qrow$log$error
  if (!is.null(error)) return()

  QuestNo <- qrow$p$Abbreviation
  # TODO when calling from Tabula... ) |> forcats::as_factor()
  dplyr::tibble(QuestNo, qtab = list(qrow$qtabs), warn, error) |>
    tidyr::unnest_longer(c(qtab), keep_empty = TRUE) |>
    dplyr::mutate(l = purrr::map(qtab, \(x) x$d[five_table_names])) |>
    tidyr::unnest_wider(l)
}
assemble_crosstab_data_ <- function(qrow) {
  table_parts <- frame_table_parts(qrow)

  q <- table_parts$QuestNo
  tab_table <- table_parts$qtab |> purrr::map_dfr(\(x) x$d$tab_table)
  val_table <- table_parts |>
    dplyr::select(tab_table, val_table) |>
    tidyr::unpack(tab_table) |>
    dplyr::select(QuestNo, TabNo, val_table) |>
    tidyr::unnest(val_table)
  row_table <- table_parts$qtab |> purrr::set_names(q) |> purrr::map_dfr(\(x) x$d$row_table, .id = "QuestNo")

  data <- tibble::lst(
    tab_table,
    val_table,
    row_table,
  )

  tibble::lst(
    table_parts,
    data
  )
}


five_table_names <- c("row_table", "col_table_all", "val_table", "head_table", "tab_table")


add_book_no <- function(df, BookNo) {
  df |> dplyr::mutate(BookNo, .before = 1)
}
prepare_head_col_tables <- function(tabula) {
  book_no <- tabula$opts$ct$V_BookNo
  col_table_all <- tabula$ditw$ct$db_tables$col_table_all |>
    add_book_no(book_no)
  head_table <- tabula$ditw$ct$db_tables$head_table |> dplyr::left_join(
    col_table_all |> dplyr::count(HeadNo, name = "HeadCount"),
    by = "HeadNo"
  ) |>
    add_book_no(book_no) |>
    dplyr::select(BookNo, HeadNo, HeadName, HeadTitle, HeadCount)
  tabula$crosstabs$data$head_table <- head_table
  tabula$crosstabs$data$col_table_all <- col_table_all |>
    dplyr::select(BookNo, ColNo, HeadNo, ColTitle1, ColTitle2, ColVariable, ColValue)
}

prepare_tab_table_tb <- function(qrow) {
  row_table <- qrow$crosstabs$data$row_table
  tab_table <- qrow$crosstabs$data$tab_table
  # the group_by QuestNo, TabNo only works if QuestNo is unique
  # see also in ascend_rownos_within_questno()
  # => perhaps better additionally group_by QuestLine and allow duplicated QuestNo?
  # TODO: discuss with Wolf if it wouldn't be better to allow empty strings, and it that case replace QuestNO with QuestLine... / add _<index> to duplicated `QuestNo`s...!
  df_tabcount <- row_table |>
    dplyr::group_by(QuestNo, TabNo) |>
    dplyr::summarise(TabCount = dplyr::n(), .groups = "drop")

  tab_table <- tab_table |> dplyr::mutate(
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
    dplyr::mutate(TabRowTypes = NA_integer_) |>
    add_book_no(qrow$m$opts$ct$V_BookNo)


  qrow$crosstabs$data$tab_table <- tab_table |>
    # TODO: source out for db:
    # dplyr::select(QuestNo, QuestLine, TabNo, TabName, TabType, TabTitle, TabTitle1, TabTitle2, TabTitle3, TabRowTypes, TabCaption, TabCount) |>
    add_book_no(qrow$m$opts$ct$V_BookNo)
}
prepare_row_table_tb <- function(qrow) {
  row_table <- qrow$crosstabs$data$row_table

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

  qrow$crosstabs$data$row_table <- row_table |>
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
    ) |>
    # TODO: source out for db:
    # dplyr::select(QuestNo, RowNo, TabNo, RowTypeS, RowType, RowContent, RowContentDetail, RowAbsPercent, RowWeighted, RowTitle1, RowTitle2, RowTitle3, RowDecimals, RowVariable, RowValue) |>
    add_book_no(qrow$m$opts$ct$V_BookNo)

}
prepare_val_table_tb <- function(qrow) {
  val_table <- qrow$crosstabs$data$val_table
  qrow$crosstabs$data$val_table <- val_table |>
    dplyr::rename(Value = value) |>
    dplyr::select(QuestNo, TabNo, RowNo, ColNo, Value) |>
    add_book_no(qrow$m$opts$ct$V_BookNo)
}
