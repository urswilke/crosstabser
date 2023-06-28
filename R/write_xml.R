write_xml_tables_from_qrows <- function(row = NULL, mapping) {
  # filter row indices specified, otherwise all:
  if (is.null(row)) {
    row <- mapping$qrows$row
  }
  qrows <- mapping$qrows[mapping$qrows$row %in% row,]

  l_row_tabs <- qrows$qtabs |> purrr::map("qtabs") |> purrr::map(\(x) purrr::map(x, gen_5_tables))

  file_names <- paste0(mapping$options$V_XMLName, stringr::str_pad(qrows$row, 4, pad = "0"), ".xml")
  purrr::walk2(
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
