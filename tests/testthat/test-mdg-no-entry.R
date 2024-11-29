df <- tibble::tibble(
  q1_1 = c(1, 0, 0) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 1"),
  q1_2 = c(1, 1, 0) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 2"),
  age = c(2, 1, 1) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age"),
  gew = c(0.5, 1.2, 0.4)
)

dfq <- tibble::tribble(
  ~Title,                                         ~Type, ~RowVar,     ~MdgMissLab, ~MdgMissValid,
  "mdg with no entry",                            "mdg", "q1_1 q1_2", NA,          NA,
  "mdg with MdgMissLab = FILTER",                 "mdg", "q1_1 q1_2", "FILTER",    NA,
  "mdg with MdgMissLab = new_lab & MdgMissValid", "mdg", "q1_1 q1_2", "new_lab",   "TRUE",
)
mapping_file = list(Questions = dfq)
m_mdg_no_entry <- Tabula$new(
  df,
  mapping_file,
  ColVar = c("age")
)
m_mdg_no_entry
test_that("mdg tables with no entries are reproduced", {
  testthat::expect_snapshot(m_mdg_no_entry)
})
