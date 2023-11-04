# TODO: put code shared with other tests into helper function...:
spss_file <- "K:/Projects/UW git/crosstabser/tests/testthat/spss/30_Datenanpassungen_mapping_neu.sav"
spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav"
mapping_file <- "K:/Projects/UW git/crosstabser/tests/testthat/excel/mapping_neu_reduced.xlsx"
mapping_file <- "excel/mapping_neu_reduced.xlsx"
tabsi <- Tabula$new(spss_file, mapping_file, 5)
# set ColVar to empty (for smaller size) and recalculate crosstab:
tabsi$options$l_macro_scenario$ColVar <- character()
tabsi |> gen_col_tables()
tabsi$calc_qtabs(5)
tabsi$merge_long_tab_data()
tabsi$long_tab_data
save_csv <- function() {
  path <- tempfile(fileext = ".csv")
  tabsi$long_csv(path)

  path
}

expect_snapshot_csv <- function(name) {
  announce_snapshot_file(name = name)

  path <- save_csv()
  expect_snapshot_file(path, name)
}
# see here: https://stackoverflow.com/a/26931460
# expect_snapshot_csv <- function(name) {
#   announce_snapshot_file(name = name)
#
#   path <- save_csv()
#   tryCatch(
#     expr = {
#       expect_snapshot_file(path, name)
#     },
#     error = function(e){
#       # https://cli.r-lib.org/reference/links.html#click-to-run-code
#       cli::cli_text("Run {.run source("dev/save_long_json_example.R", echo=TRUE)} to review")
#     }
#   )
# }
# ... but doesn't work because somehow the output only works if cli::cli_text is called in the console and not in Rstudio build tab
# clickable ...

test_that('csv export works\nOTHERWISE RUN:\nsource("dev/save_long_json_example.R", echo=TRUE)\nto REGENERATE JSON', {
  expect_snapshot_csv("row5.csv")
})
