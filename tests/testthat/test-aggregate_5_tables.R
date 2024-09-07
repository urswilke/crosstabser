spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav" |> testthat::test_path()
mapping_file <- "excel/mapping_neu_weighted.xlsx" |> testthat::test_path()

tabsi <- Tabula$new(spss_file, mapping_file, 5)$aggregate_5_tables()

test_that("5 tables' prints are reproduced", {
  testthat::expect_snapshot(
    withr::with_options(
      list(pillar.print_max = Inf, width = 1000),
      print(tabsi$crosstabs$data)
    )
  )
})
