spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/fake_survey.sav"
spss_file <- "spss/fake_survey.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping.xlsx"
mapping_file <- "excel/mapping.xlsx"
tt <- Tabula$new(spss_file, mapping_file)$calc_qtabs(row = 5)

df_crosstab_cat <- tt$qrows$qtabs[[1]]$qtabs[[1]]$d$tab_values |>
  dplyr::arrange(colvar, colval) |>
  tidyr::pivot_wider(
    names_from = c(colvar, colval),
    values_from = value
  )

test_that("cross-tabulation works for cat", {
  testthat::expect_snapshot(df_crosstab_cat)
})

# long_tab <- tt$qrows$qtabs[[1]]$qtabs[[1]]$long_tab()$d$long_tab
wide_tab <- tt$qrows$qtabs[[1]]$qtabs[[1]]$wide_tab()$d$wide_tab

# test_that("long_tab repoduced for cat", {
#   testthat::expect_snapshot(long_tab)
# })
test_that("wide_tab is repoduced for cat", {
  testthat::expect_snapshot(wide_tab)
})

spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/30_Datenanpassungen_mapping_neu_reduced.sav"
spss_file <- "spss/30_Datenanpassungen_mapping_neu_reduced.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping_neu_reduced.xlsx"
mapping_file <- "excel/mapping_neu_reduced.xlsx"
tt <- Tabula$new(spss_file, mapping_file)$calc_qtabs()

wide_tabs <- 1:4 |> lapply(\(i) tt$qrows$qtabs[[i]]$qtabs[[1]]$wide_tab()$d$wide_tab)

# test_that("long_tab repoduced for cat", {
#   testthat::expect_snapshot(long_tab)
# })
test_that("wide_tab is repoduced with new mapping", {
  testthat::expect_snapshot(wide_tabs)
})
