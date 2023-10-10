spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/30_Datenanpassungen_mapping_neu.sav"
spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping_neu_reduced.xlsx"
mapping_file <- "excel/mapping_neu_reduced.xlsx"
tabsi <- Tabula$new(spss_file, mapping_file, c(5:17))

test_that("crosstab prints are reproduced", {
  testthat::expect_snapshot(tabsi)
})

