spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav" |> testthat::test_path()
df <- haven::read_sav(spss_file)
# Hack to test if (M)Statistics Rows are set to NA if calculated on 0 valid cases:
# TODO: check if the resulting values of the statistics are correctly calculated if NAs occur in the data...!
df$q3a_NA <- df$q3a
df[df$kregio == 1,]$q3a_NA <- NA_real_

mapping_file <- "excel/mapping_neu_reduced.xlsx" |> testthat::test_path()

tabsi <- Tabula$new(df, mapping_file)

test_that("crosstab prints are reproduced", {
  testthat::expect_snapshot(tabsi)
})

tabsi$dat_mod$q3a[1:9] <- 101
testthat::expect_warning(tabsi$calc_qtabs(7))
