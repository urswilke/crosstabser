spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/30_Datenanpassungen_mapping_neu.sav"
spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping_neu_reduced.xlsx"
mapping_file <- "excel/mapping_neu_reduced.xlsx"
tabsi <- Tabula$new(spss_file, mapping_file, c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 22))

test_that("crosstab prints are reproduced", {
  testthat::expect_snapshot(tabsi)
})

