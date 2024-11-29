df <- tibble::tibble(
  q1_1 = c(1, 0, 0) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 1"),
  q1_2 = c(1, 1, 0) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 2"),
  q1_3 = c(1, 0, 1) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 3"),
  q1_4 = c(1, 0, 1) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 4"),
  q1n1 = c(1, 2, 3) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  q1n2 = c(2,-2, 4) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  q1n3 = c(3,-2,-2) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  q1n4 = c(4,-2,-2) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4)),
  age = c(2, 1, 1) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age"),
  gew = c(0.5, 1.2, 0.4)
)

dfq <- tibble::tribble(
  ~Title,                              ~Type, ~RowVar,               ~Unguelt,    ~Exclusive,
  "mcg with invalids",                 "mcg", "q1n1 q1n2 q1n3 q1n4", "3, 4, -2",  NA,
  "mcg with invalids & exclusive",     "mcg", "q1n1 q1n2 q1n3 q1n4", "3, 4, -2",  "1",
  "mdg with invalids",                 "mdg", "q1_1 q1_2",           "q1_3 q1_4", NA,
  "mdg with invalids & exclusive",     "mdg", "q1_1 q1_2",           "q1_3 q1_4", "q1_1",
)
mapping_file = list(Questions = dfq)
m_rm_invalids <- Tabula$new(
  df,
  mapping_file,
  ColVar = c("age"),
  # Weight = "gew",
  # Unwgt = TRUE,
  tabulate = FALSE
)
m_rm_invalids$options$l_lexikon["cTabWeighted"] <- "W"
m_rm_invalids$calc_qtabs()
m_rm_invalids
test_that("mcg tables with invalids & Exclusive are reproduced", {
  testthat::expect_snapshot(m_rm_invalids)
})
