# TODO: put code shared with other tests into helper function...:
spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/30_Datenanpassungen_mapping_neu.sav"
spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping_neu_reduced.xlsx"
mapping_file <- "excel/mapping_neu_reduced.xlsx"
tabsi <- Tabula$new(spss_file, mapping_file, 5)
# set ColVar to empty (for smaller size) and recalculate crosstab:
tabsi$options$l_macro_scenario$ColVar <- character()
tabsi |> gen_col_tables()
tabsi$calc_qtabs(5)
tabsi$merge_long_tab_data()
tabsi$long_tab_data
save_csv <- function() {
  path <- tempfile(fileext = ".csv")
  tabsi$long_csv(path)

  path
}

expect_snapshot_csv <- function(name) {
  announce_snapshot_file(name = name)

  path <- save_csv()
  expect_snapshot_file(path, name)
}


test_that("csv export works", {
  expect_snapshot_csv("row5.csv")
})
