df <- tibble::tibble(
  q1n1 = c(1, 2, 3, 2, 3) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  q1n2 = c(3, 2, 3, 1, 2) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  age  = c(2, 1, 3, 3, 2) |> haven::labelled(c("18-39" = 1, "40-59" = 2, "60+" = 3), label = "age"),
)

dfq <- tibble::tribble(
  ~Title,                ~Type, ~RowVar,     ~Filter,
  "table with error",    "mcg", "q1n1 q1n2", "this yields an error",
  "table without error", "cat", "q1n1",      NA_character_,
)
mapping_file = list(Questions = dfq, Macro = list(ColVar = "age"))

testthat::expect_error(
  m <- Tabula$new(
    df,
    mapping_file,
    verbose = TRUE,
  )
)

testthat::expect_no_error(
  testthat::expect_message(
    m <- Tabula$new(
      df,
      mapping_file,
      verbose = TRUE,
      error_out = "safe"
    )
  )
)
testthat::expect_true(is.character(m$qrows[[1]]$log$error))
testthat::expect_true(!is.null(m$qrows[[2]]$qtabs[[1]]))
