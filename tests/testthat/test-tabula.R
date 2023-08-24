spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/30_Datenanpassungen_mapping_neu_reduced.sav"
spss_file <- "spss/30_Datenanpassungen_mapping_neu_reduced.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping_neu_reduced.xlsx"
mapping_file <- "excel/mapping_neu_reduced.xlsx"
tt <- Tabula$new(spss_file, mapping_file)$calc_qtabs()

wide_tabs <- 1:5 |> lapply(\(i) tt$qrows$qrow[[i]]$qtabs[[1]]$wide_tab()$d$wide_tab)

wide_tab <- tt$qrows$qrow[[3]]$qtabs[[2]]$wide_tab()$d$wide_tab
test_that("wide_tab is repoduced for MetrMac cat", {
  testthat::expect_snapshot(print(wide_tab, n = Inf))
})
test_that("wide_tab is repoduced with new mapping", {
  testthat::expect_snapshot(wide_tabs)
})

