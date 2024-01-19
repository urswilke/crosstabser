spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav" |> testthat::test_path()
mapping_file <- "excel/mapping_neu_reduced.xlsx" |> testthat::test_path()
tabsi <- Tabula$new(spss_file, mapping_file)

test_that("crosstab prints are reproduced", {
  testthat::expect_snapshot(tabsi)
})

tabsi$dat_mod$q3a[1:9] <- 101
testthat::expect_warning(tabsi$calc_qtabs(7))
