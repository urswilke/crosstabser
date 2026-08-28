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

prepare_tab_table_tb_ <- function(qtab) {
  row_table <- qtab$d$row_table
  tab_table <- qtab$d$tab_table

  tab_table |> dplyr::mutate(
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
    dplyr::mutate(
      TabCount = nrow(row_table),
      TabRowTypes = NA_integer_
    ) |>
    add_book_no(qtab$m$opts$ct$V_BookNo)
}
prepare_row_table_tb_ <- function(qtab) {
  row_table <- qtab$d$row_table
  lexikon <- qtab$m$opts$ct$l_lexikon

  row_table |>
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
    add_book_no(qtab$m$opts$ct$V_BookNo)
}
prepare_val_table_tb_ <- function(qtab) {
  qtab$d$val_table |>
    dplyr::mutate(
      QuestNo = qtab$p$Abbreviation,
      TabNo = qtab$p$i_tab,
    ) |>
    dplyr::rename(Value = value) |>
    dplyr::select(QuestNo, TabNo, RowNo, ColNo, Value) |>
    add_book_no(qtab$m$opts$ct$V_BookNo)
}
