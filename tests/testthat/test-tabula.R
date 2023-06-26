spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/fake_survey.sav"
spss_file <- "spss/fake_survey.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping.xlsx"
mapping_file <- "excel/mapping.xlsx"
tt <- Tabula$new(spss_file, mapping_file)$add_qtab_data()

df_crosstab_cat <- tt$qtabs[[1]]$d$counts |>
  dplyr::arrange(colvar, colval) |>
  tidyr::pivot_wider(
    names_from = c(colvar, colval),
    values_from = value
  )

test_that("cross-tabulation works for cat", {
  testthat::expect_snapshot(df_crosstab_cat)
})
