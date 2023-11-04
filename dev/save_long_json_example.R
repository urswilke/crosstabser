# for use in table_charter
devtools::load_all()
spss_file <- "tests/testthat/spss/30_Datenanpassungen_mapping_neu.sav"
mapping_file <- "tests/testthat/excel/mapping_neu_reduced.xlsx"
tabsi <- Tabula$new(spss_file, mapping_file, 5)
# set ColVar to empty (for smaller size) and recalculate crosstab:
tabsi$options$l_macro_scenario$ColVar <- character()
tabsi |> gen_col_tables()
tabsi$calc_qtabs(5)
tabsi$merge_long_tab_data()

tabsi$long_json("dev/example.json", pretty = TRUE)
