df <- tibble::tibble(
  q1_1 = c(1, 0, 0) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 1 & 2"),
  q1_2 = c(1, 1, 0) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 1 & 2"),
  q1_3 = c(0, 1, 0) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 3 & 4"),
  q1_4 = c(1, 1, 1) |> haven::labelled(c(Selected = 1, Unselected = 0), label = "Choice 3 & 4"),
  q1n1 = c(1, 2, 3) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4), label = "Choice 1 & 2"),
  q1n2 = c(2,-2, 4) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4), label = "Choice 1 & 2"),
  q1n3 = c(3,-2,-2) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4), label = "Choice 3 & 4"),
  q1n4 = c(4,-2,-2) |> haven::labelled(c(ch1=1, ch2=2, ch3 = 3, ch4=4), label = "Choice 3 & 4"),
  age  = c(2, 1, 1) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age"),
  sel1 = c(1, 2, 3) |> haven::labelled(c("sel1" = 1, "sel2" = 2, sel3 = 3)),
  sel2 = c(1, 3, 2) |> haven::labelled(c("sel1" = 1, "sel2" = 2, sel3 = 3)),
  gew = c(0.5, 1.2, 0.4)
)

dfq <- tibble::tribble(
  ~Title,             ~Type, ~RowVar,               ~SelVar,     ~SelVal,          ~Mult,
  "mdg without Mult", "mdg", "q1_1 q1_2 q1_3 q1_4", "sel1 sel2", "2-3:sel2_&_3", NA,
  "mdg with Mult",    "mdg", "q1_1 q1_2 q1_3 q1_4", "sel1 sel2", "2-3:sel2_&_3", "TRUE",
  "mcg without Mult", "mcg", "q1n1 q1n2 q1n3 q1n4", "sel1 sel2", "2-3:sel2_&_3", NA,
  "mcg with Mult",    "mcg", "q1n1 q1n2 q1n3 q1n4", "sel1 sel2", "2-3:sel2_&_3", "TRUE",
  "mw without Mult",  "mw",  "q1n1 q1n2 q1n3 q1n4", "sel1 sel2", "1-2:sel1_&_2", NA,
  "mw with Mult",     "mw",  "q1n1 q1n2 q1n3 q1n4", "sel1 sel2", "1-2:sel1_&_2", "TRUE",
)
mapping_file = list(Questions = dfq)

m_selvar <- Tabula$new(
  df,
  mapping_file,
  verbose = TRUE,
  ColVar = "age"
)
test_that("tables of all 4 types with 2 `SelVar`s & with / without `Mult` are reproduced", {
  testthat::expect_snapshot(m_selvar)
})
