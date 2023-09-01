spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/30_Datenanpassungen_mapping_neu.sav"
spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping_neu_reduced.xlsx"
mapping_file <- "excel/mapping_neu_reduced.xlsx"
tt <- Tabula$new(spss_file, mapping_file)$calc_qtabs()

qtab_objects <- seq_len(nrow(tt$qrows)) |> lapply(\(i) tt$qrows$qrow[[i]]$qtabs)

test_that("crosstab prints are reproduced", {
  testthat::expect_snapshot(qtab_objects)
})

