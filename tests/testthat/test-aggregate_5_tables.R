spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav" |> testthat::test_path()
mapping_file <- "excel/mapping_neu_reduced.xlsx" |> testthat::test_path()
df <- haven::read_sav(spss_file)
tabsi <- Tabula$new(df, mapping_file, tabulate = FALSE, row = 5)
tabsi$opts$ct$l_macro_scenario$Weight <- "gew"
tabsi$opts$ct$l_macro_scenario$Unwgt <- TRUE
tabsi$calc_crosstabs(5)

test_that("5 tables' prints are reproduced", {
  testthat::expect_snapshot(
    withr::with_options(
      list(pillar.print_max = Inf, width = 1000),
      print(tabsi$get_crosstabs_data())
    )
  )
})

df <- tibble::tibble(
  q1_1 = c(1, 0, NA) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 1"),
  q1_2 = c(1, 1, 0) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 2"),
  age = c(2, 1, 1) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age"),
  age2 = c(2, 1, 1) |> haven::labelled(c("1-label from another var" = 1, "2-label from another var" = 2), label = "age"),
  gew = c(0.5, 1.2, 0.4)
)

dfq <- tibble::tribble(
  ~Title,                                         ~Type, ~RowVar,
  "mdg with no entry",                            "mdg", "q1_1 q1_2",
)
mapping_file = list(Questions = dfq, Macro = list(ColVar = c("age", "age2")))
m <- Tabula$new(
  df,
  mapping_file,
)
m$opts$ct$l_macro_scenario$Weight <- "gew"
m$opts$ct$l_macro_scenario$Unwgt <- TRUE
m$opts$ct$l_lexikon["cTabWeighted"] <- "W"
m$calc_crosstabs()

test_that("5 tables' prints are reproduced", {
  testthat::expect_snapshot(
    withr::with_options(
      list(pillar.print_max = Inf, width = 1000),
      print(m$get_crosstabs_data())
    )
  )
})
