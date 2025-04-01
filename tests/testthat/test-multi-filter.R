df <- tibble::tibble(
  q0   = c(1, 1, 1, 2, 1) |> haven::labelled(c("selval1" = 1, "selval2" = 2)),
  q1_1 = c(1, 2, 3, 4, 5) |> haven::labelled(label = "q1 scale var 1"),
  q1_2 = c(4, 5, 4, 2, 3) |> haven::labelled(label = "q1 scale var 2"),
  q2_1 = c(1, 2, 5, 3, 4) |> haven::labelled(label = "q2 scale var 1"),
  q2_2 = c(4, 5, 2, 3, 1) |> haven::labelled(label = "q2 scale var 2"),
  age  = c(2, 1, 3, 3, 2) |> haven::labelled(c("18-39" = 1, "40-59" = 2, "60+" = 3), label = "age"),
)

dfq <- tibble::tribble(
  ~Title,                          ~Type, ~RowVar,     ~Filter,                                      ~SelVar,  ~SelVal,
  "mw with multi-filter & SelVar", "mw",  "q2_1 q2_2", "q1_{[1|2]} > 3 & q1_{[1|2]} <= 5",           "q0",     "1 2",
  "mw with multi-filter",          "mw",  "q2_1 q2_2", "q1_{[1|2]} > 3 & q1_{[1|2]} <= 5 & q0 == 1", NA,       NA,
  "mw with {rowvar}-filter",       "mw",  "q2_1 q2_2", "{rowvar} <= 3",                              NA,       NA,
)
mapping_file = list(Questions = dfq, Macro = list(ColVar = "age"))

m <- Tabula$new(
  df,
  mapping_file,
  verbose = TRUE,
)

testthat::expect_true(
  all(purrr::map2_lgl(
    1:3 |> lapply(\(i) m$qrows[[1]]$qtabs[[i]] |> capture.output() |> _[-1]),
    1:3 |> lapply(\(i) m$qrows[[2]]$qtabs[[i]] |> capture.output() |> _[-1]),
    \(x, y) all.equal(x, y)
  ))
)
m$qrows[2] <- NULL
test_that("tables with multi-filters are reproduced", {
  testthat::expect_snapshot(m)
})

dfq <- tibble::tribble(
  ~Title,                          ~Type, ~RowVar,     ~Filter,                            ~SelVar,  ~SelVal,
  "mw wi multi-filter & -SelVar",  "mw",  "q2_1 q2_2", "q1_{[1|2]} > 3 & q1_{[1|2]} <= 5", "q0 age", "1 2",
)
mapping_file = list(Questions = dfq, Macro = list(ColVar = "age"))

testthat::expect_error(
  Tabula$new(
    df,
    mapping_file,
    verbose = TRUE,
  )
)
