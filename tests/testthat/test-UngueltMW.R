df <- tibble::tibble(
  q1n1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  age  = c(2, 1, 1, 1, 2, 2, 1, 1, 2) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age"),
)

dfq <- tibble::tribble(
  ~Title,                              ~Type, ~RowVar, ~Unguelt,    ~UngueltMW,                  ~MetrMac,
  "cat with invalids",                 "cat", "q1n1",  "3, 4, -2",  NA,                          "S1M0",
  "cat with invalids & exclusive",     "cat", "q1n1",  "3, 4, -2",  "5, 6 THRU 8",               "S1M0",
  "cat with NA & exclusive",           "cat", "q1n1",  "3, 4, -2",  "5, 6 THRU 6.5, 6.5 THRU 8", "S1M0",
)
mapping_file = list(Questions = dfq, Macro = list(ColVar = "age"))
m <- Tabula$new(
  df,
  mapping_file,
)
test_that("cat tables with UngueltMW are reproduced", {
  testthat::expect_snapshot(m)
})
