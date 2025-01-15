spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav" |> testthat::test_path()
mapping_file <- "excel/mapping_neu_reduced.xlsx" |> testthat::test_path()
df <- haven::read_sav(spss_file)
tabsi <- Tabula$new(df, mapping_file, tabulate = FALSE, row = 5)
tabsi$opts$ct$l_macro_scenario$Weight <- "gew"
tabsi$opts$ct$l_macro_scenario$Unwgt <- TRUE
tabsi$calc_qtabs(5)

tabsi$prepare_5_tables()

test_that("5 tables' prints are reproduced", {
  testthat::expect_snapshot(
    withr::with_options(
      list(pillar.print_max = Inf, width = 1000),
      print(tabsi$.__enclos_env__$private$crosstabs$data)
    )
  )
})
