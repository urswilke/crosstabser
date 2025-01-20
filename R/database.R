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
prep_tab_row_val_ <- function(qrow) {
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
prepare_head_col_tables_ <- function(tabula) {
  book_no <- tabula$opts$ct$V_BookNo
  col_table_all <- tabula$ditw$ct$db_tables$col_table_all |>
    add_book_no(book_no)
  head_table <- tabula$ditw$ct$db_tables$head_table |> dplyr::left_join(
    col_table_all |> dplyr::count(HeadNo, name = "HeadCount"),
    by = "HeadNo"
  ) |>
    add_book_no(book_no) |>
    dplyr::select(BookNo, HeadNo, HeadName, HeadTitle, HeadCount)
  tabula$ditw$ct$crosstabs$data$head_table <- head_table
  tabula$ditw$ct$crosstabs$data$col_table_all <- col_table_all |>
    dplyr::select(BookNo, ColNo, HeadNo, ColTitle1, ColTitle2, ColVariable, ColValue)
}

prepare_tab_table_tb <- function(qrow) {
  row_table <- qrow$ditw$ct$crosstabs$data$row_table
  tab_table <- qrow$ditw$ct$crosstabs$data$tab_table
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


  qrow$ditw$ct$crosstabs$data$tab_table <- tab_table |>
    add_book_no(qrow$m$opts$ct$V_BookNo)
}
prepare_row_table_tb <- function(qrow) {
  row_table <- qrow$ditw$ct$crosstabs$data$row_table
  lexikon <- qrow$m$opts$ct$l_lexikon

  qrow$ditw$ct$crosstabs$data$row_table <- row_table |>
    dplyr::mutate(
      RowTypeS = paste0(
        RowContent,
        dplyr::if_else(RowAbsPercent == "", "", "|"),
        RowAbsPercent,
        RowWeighted
      ),
      RowType = bitwOr(
        as.numeric(lexikon[paste0("cArt", gsub("^.*\\|", "", RowTypeS))]),
        as.numeric(lexikon[paste0("cArt", gsub("\\|.*$", "", RowTypeS))])
      ),
      RowContentDetail = dplyr::if_else(
        grepl("Statistics$", RowContent),
        sub(lexikon["cTabWeighted"], "", RowTitle3),
        ""
      ),
    ) |>
    add_book_no(qrow$m$opts$ct$V_BookNo)

}
prepare_val_table_tb <- function(qrow) {
  val_table <- qrow$ditw$ct$crosstabs$data$val_table
  qrow$ditw$ct$crosstabs$data$val_table <- val_table |>
    dplyr::rename(Value = value) |>
    dplyr::select(QuestNo, TabNo, RowNo, ColNo, Value) |>
    add_book_no(qrow$m$opts$ct$V_BookNo)
}
