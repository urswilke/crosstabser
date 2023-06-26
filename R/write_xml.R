write_xml_tables <- function(mapping) {
  l_5_tables <- lapply(
    seq_len(length(mapping$tabs)),
    \(i_tab) gen_5_tables(i_tab, mapping)
  )
  file_names <- paste0("dev/xml/tab", stringr::str_pad(mapping$qsheet$tables$row, 4, pad = "0"), ".xml")
  purrr::map2(
    l_5_tables,
    file_names,
    \(el_5_tabs, filename) write_xml_table(el_5_tabs, filename)
  )
}

gen_5_tables <- function(i_tab, mapping) {
  tabs_ex <- list(
    Row = mapping$tabs[[i_tab]]$d$row_table,
    Tab = mapping$tabs[[i_tab]]$d$tab_table,
    Head = mapping$tabs[[i_tab]]$d$head_table,
    Col = mapping$tabs[[i_tab]]$d$col_table,
    Val = mapping$tabs[[i_tab]]$d$val_table
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
