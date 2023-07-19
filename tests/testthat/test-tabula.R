spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/fake_survey.sav"
spss_file <- "spss/fake_survey.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping.xlsx"
mapping_file <- "excel/mapping.xlsx"
tt <- Tabula$new(spss_file, mapping_file)$calc_qtabs()

df_crosstab_cat <- tt$qrows$qtabs[[1]]$qtabs[[1]]$d$detail_freqs |>
  dplyr::arrange(colvar, colval) |>
  tidyr::pivot_wider(
    names_from = c(colvar, colval),
    values_from = value
  )

test_that("cross-tabulation works for cat", {
  testthat::expect_snapshot(df_crosstab_cat)
})
