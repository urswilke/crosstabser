# TODO: put code shared with other tests into helper function...:
spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/30_Datenanpassungen_mapping_neu.sav"
spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping_neu_reduced.xlsx"
mapping_file <- "excel/mapping_neu_reduced.xlsx"
tabsi <- Tabula$new(spss_file, mapping_file, 5)

save_xml <- function() {
  path <- tempfile(fileext = ".xml")
  tabsi$qrows[[1]]$xml(path)

  path
}

expect_snapshot_xml <- function(name) {
  # Other packages might affect results
  skip_if_not_installed("xml2")

  announce_snapshot_file(name = name)

  path <- save_xml()
  expect_snapshot_file(path, name)
}


test_that("xml export works", {
  expect_snapshot_xml("row5.xml")
})
