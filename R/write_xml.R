write_xml_tables <- function(mapping) {
  qsheet_row_idx <- split(
    mapping$qsheet$tab_table$TabNo,
    mapping$qsheet$tab_table$row
  )
  l_row_tabs <- qsheet_row_idx |>
    lapply(\(row) lapply(row, \(i_tab) mapping$tabs[[i_tab]] |> gen_5_tables()))

  file_names <- paste0("dev/xml/tab", stringr::str_pad(names(qsheet_row_idx), 4, pad = "0"), ".xml")
  purrr::map2(
    l_row_tabs,
    file_names,
    \(l_row, filename) write_xml_file(l_row, filename)
  )
}

gen_5_tables <- function(tab) {
  tabs_ex <- list(
    Row = tab$d$row_table,
    Tab = tab$d$tab_table,
    Head = tab$d$head_table,
    Col = tab$d$col_table,
    Val = tab$d$val_table
  )

}

write_xml_table <- function(el_5_tabs, filename) {
  doc <- xml2::xml_new_root("Root")
  xml2::xml_add_child(doc, "Table")
  all_xml <- xml2::xml_find_all(doc, "//Table")

  for (i in 1:length(el_5_tabs)) {
    tab_name <- names(el_5_tabs)[i]
    xml2::xml_add_child(all_xml, tab_name)
    el_xml <- xml2::xml_find_all(doc, paste0("//", tab_name))
    seq_len(nrow(el_5_tabs[[i]])) |> purrr::walk(\(x) xml2::xml_add_child(el_xml, "Line"))
    line_xml <- xml2::xml_find_all(doc, paste0("//", tab_name, "/Line"))

    for(i_tab_el in names(el_5_tabs[[i]])) {
      col_vec <- el_5_tabs[[i]][[i_tab_el]]
      na_cells <- is.na(col_vec)
      xml2::xml_add_child(line_xml[na_cells], i_tab_el)
      xml2::xml_add_child(line_xml[!na_cells], i_tab_el, col_vec[!na_cells])
    }
  }
  xml2::write_xml(doc, filename)
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
