write_xml_tables_from_qrows <- function(mapping, row = NULL) {
  # filter row indices specified, otherwise all:
  row <- set_row(mapping, row)
  do_xml <- mapping$qsheet$qrows_params |> purrr::map_int("row") %in% row

  mapping$qrows[do_xml] |> lapply(\(x) x$xml())
}

gen_5_tables <- function(qtab) {
  tabs_ex <- list(
    Tab = qtab$d$tab_table,
    Row = qtab$d$row_table,
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
  l <- purrr::transpose(df)
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
