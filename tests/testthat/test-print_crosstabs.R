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
tabsi$calc_qtabs(7)
tabsi$aggregate_5_tables()

test_that("A warning correctly is written to the log", {
  testthat::expect_true(!is.null(tabsi$qrows[[1]]$log$warn))
})


tabsi$dat_mod$q1 <- NULL
# TODO: if the crosstabs of none of the rows can be calculated, aggregate_5_tables() errors out
#  -> discuss with Wolf how we should treat these edge cases
# ... therefore we add the 6th row (which doesn't error out),
tabsi$calc_qtabs(5:6)
tabsi$aggregate_5_tables()
test_that("An error is correctly written to the log", {
  testthat::expect_true(!is.null(tabsi$qrows[[1]]$log$error))
})

# ... otherwise an error is thrown:
tabsi$calc_qtabs(5)
test_that("An informative error is thrown when no crosstabs are calculated", {
  testthat::expect_error(
    tabsi$aggregate_5_tables(),
    regexp = "No crosstabs calculated"
  )
})
