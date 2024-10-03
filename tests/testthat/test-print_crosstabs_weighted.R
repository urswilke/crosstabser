spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav" |> testthat::test_path()
mapping_file <- "excel/mapping_neu_reduced.xlsx" |> testthat::test_path()
# Hack to test if (M)Statistics Rows are set to NA if calculated on 0 valid cases:
df <- haven::read_sav(spss_file)
df$q3a_NA <- df$q3a
df[df$kregio == 1,]$q3a_NA <- NA_real_
tabsi <- Tabula$new(df, mapping_file, tabulate = FALSE)
tabsi$options$l_lexikon["cTabWeighted"] <- "W"
tabsi$options$l_macro_scenario$Weight <- "gew"
tabsi$options$l_macro_scenario$Unwgt <- TRUE
tabsi$calc_qtabs()
test_that("crosstab prints are reproduced", {
  testthat::expect_snapshot(tabsi)
})

