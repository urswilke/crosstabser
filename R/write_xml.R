write_xml_tables <- function(mapping) {
  qsheet_row_idx <- split(
    mapping$qsheet$tab_table$TabNo,
    mapping$qsheet$tab_table$row
  )
  l_row_tabs <- qsheet_row_idx |>
    lapply(\(row) lapply(row, \(i_tab) mapping$qtabs[[i_tab]] |> gen_5_tables()))

  file_names <- paste0("dev/xml/tab", stringr::str_pad(names(qsheet_row_idx), 4, pad = "0"), ".xml")
  purrr::map2(
    l_row_tabs,
    file_names,
    \(l_row, filename) write_xml_file(l_row, filename)
  )
}
# TODO: use the same functionality as write_xml_tables():
write_xml_tables_from_qrows <- function(mapping) {
  l_row_tabs <- mapping$qrows$qsheet_row_idx |>
    lapply(\(row) lapply(row, \(i_tab) mapping$qtabs[[i_tab]] |> gen_5_tables()))

  file_names <- paste0("dev/xml/tab", stringr::str_pad(names(mapping$qrows$qsheet_row_idx), 4, pad = "0"), ".xml")
  purrr::map2(
    l_row_tabs,
    file_names,
    \(l_row, filename) write_xml_file(l_row, filename)
  )
}

gen_5_tables <- function(qtab) {
  tabs_ex <- list(
    Row = qtab$d$row_table,
    Tab = qtab$d$tab_table,
    Head = qtab$d$head_table,
    Col = qtab$d$col_table,
    Val = qtab$d$val_table
  )

}

write_xml_file <- function(l_row, filename) {
  names(l_row) <- rep("Table", length(l_row))
  l_xml <- table_as_xml_list(l_row)

  list(Root = l_xml) |> xml2::as_xml_document() |> xml2::write_xml(filename)
}
table_to_line_lists <- function(df) {
  l <- transpose(df)
  names(l) <- rep("Line", length(l))
  l |> lapply(row_to_list_of_lists)
}
row_to_list_of_lists <- function(row) {
  na_idx <- is.na(row)
  row[na_idx] <- lapply(row[na_idx], \(x) list())
  row[!na_idx] <- lapply(row[!na_idx], \(x) list(x))
  row
}

table_el_as_xml_list <- function(rows) {
  rows |> lapply(table_to_line_lists)
}

table_as_xml_list <- function(l_row) {
  l_row |>
    lapply(table_el_as_xml_list)
}
