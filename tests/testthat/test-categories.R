df <- tibble::tibble(
  q1n1 = c(1, 2, 3, 2, 3) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  q1n2 = c(3, 2, 3, 1, 2) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  q2_1 = c(1, 1, 0, 0, 1) |> haven::labelled(label = "option 1", labels = c("code 0" = 0, "code 1" = 1)),
  q2_2 = c(0, 1, 1, 1, 0) |> haven::labelled(label = "option 2"),
  q2_3 = c(0, 0, 0, 0, 1) |> haven::labelled(label = "option 3"),
  q2_4 = c(1, 0, 0, 0, 1) |> haven::labelled(label = "option 4"),
  age  = c(2, 1, 3, 3, 2) |> haven::labelled(c("18-39" = 1, "40-59" = 2, "60+" = 3), label = "age"),
)

dfq <- tibble::tribble(
  ~Title,                        ~Type, ~RowVar,               ~Categories, ~SelVar, ~SelVal, ~RvEmp, ~Filter,
  "cat with 'Categories'",       "cat", "q1n1",                "1 2", NA, NA, NA, NA,
  "mcg with 'Categories'",       "mcg", "q1n1 q1n2",           "1 2", NA, NA, NA, NA,
  "mcg with OVERCODES",          "mcg", "q1n1 q1n2",           "subtotal=\"OVERCODE 1\"1,2,3,subtotal='OTHERS'othernm", NA, NA, NA, NA,
  "mcg with OVERCODES 2",        "mcg", "q1n1 q1n2",           "subtotal=\"OVERCODE 1\"1,3", NA, NA, NA, NA,
  "mcg with OVERCODES & SelVar", "mcg", "q1n1 q1n2",           "subtotal=\"OVERCODE 1\"1,2,subtotal='OTHERS'othernm", "q2_1", "0 1", "EXCLUDE", NA,
  "mdg with OVERCODES",          "mdg", "q2_1 q2_2 q2_3 q2_4", "q2_1 q2_2:OVERCODE 1,q2_3 q2_4:OVERCODE 2", NA, NA, NA, NA,
  "mcg with OVERCODES & filter", "mcg", "q1n1 q1n2", "subtotal=\"OVERCODE 1\"1,2,3,subtotal='OTHERS'othernm", NA, NA, NA, "q2_1 == 1",
  "mcg with OVERCODES & SelVar", "mcg", "q1n1 q1n2", "subtotal=\"OVERCODE 1\"1,2,3,subtotal='OTHERS'othernm", "q2_1", "0 1", NA, NA,
)
mapping_file = list(Questions = dfq, Macro = list(ColVar = "age"))

m <- Tabula$new(
  df,
  mapping_file,
  verbose = TRUE,
  # error_out = "safe",
)
test_that("table with 'Categories' reproduced", {
  testthat::expect_snapshot(m)
})

tab_selvar <- m$qrows[[7]]$qtabs[[1]] |> print() |> capture.output() |> _[-1]
tab_filter <- m$qrows[[8]]$qtabs[[2]] |> print() |> capture.output() |> _[-1]
tab_selvar |> identical(tab_filter)
test_that("selvar table result identical to filter result (except titles)", {
  testthat::expect_identical(
    tab_selvar,
    tab_filter
  )
})
