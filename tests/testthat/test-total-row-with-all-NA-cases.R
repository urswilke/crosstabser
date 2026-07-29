df <- tibble::tibble(
  q1n1 = c(NA,  2, 3) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  q1n2 = c(NA, -2, 4) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  q1n3 = c(NA, -2,-2) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  q1n4 = c(NA, -2,-2) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  age  = c(NA,  1, 2) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age"),
  not_needed = 1:3,
)

dfq <- tibble::tribble(
  ~Title,      ~Type, ~RowVar,
  "mcg table", "mcg", "q1n1 q1n2 q1n3 q1n4",
)
mapping_file = list(Questions = dfq, Macro = list(ColVar = "age"))
m <- Tabula$new(
  df,
  mapping_file,
)
test_that("mcg tables with all NA cases are reproduced (regarding TOTAL row)", {
  testthat::expect_snapshot(m)
})
