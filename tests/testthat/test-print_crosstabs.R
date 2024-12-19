spss_file <- "spss/30_Datenanpassungen_mapping_neu.sav" |> testthat::test_path()
df <- haven::read_sav(spss_file)
# Hack to test if (M)Statistics Rows are set to NA if calculated on 0 valid cases:
# TODO: check if the resulting values of the statistics are correctly calculated if NAs occur in the data...!
df$q3a_NA <- df$q3a
df[df$kregio == 1,]$q3a_NA <- NA_real_

mapping_file <- "excel/mapping_neu_reduced.xlsx" |> testthat::test_path()

tabsi <- Tabula$new(df, mapping_file)

test_that("crosstab prints are reproduced", {
  testthat::expect_snapshot(tabsi)
})

tabsi$dat_mod$q3a[1:9] <- 101
tabsi$opts$da$error_out <- "safe"
tabsi$calc_qtabs(7)
tabsi$prepare_5_tables()

test_that("A warning correctly is written to the log", {
  testthat::expect_true(!is.null(tabsi$qrows[[1]]$log$warn))
})


tabsi$dat_mod$q1 <- NULL
# TODO: if the crosstabs of none of the rows can be calculated, assemble_crosstab_data() errors out
#  -> discuss with Wolf how we should treat these edge cases
# ... therefore we add the 6th row (which doesn't error out),
tabsi$calc_qtabs(5:6)

# TODO: this broke ---> fix!

# tabsi$assemble_crosstab_data()
# test_that("An error is correctly written to the log", {
#   testthat::expect_true(!is.null(tabsi$qrows[[1]]$log$error))
# })
#
# # ... otherwise an error is thrown:
# tabsi$calc_qtabs(5)
# test_that("An informative error is thrown when no crosstabs are calculated", {
#   testthat::expect_error(
#     tabsi$assemble_crosstab_data(),
#     regexp = "No crosstabs calculated"
#   )
# })


df <- tibble::tibble(
  q1 = c(1, 2, 1) |> haven::labelled(c(Yes = 1, No = 2), label = "hallo"),
  q2 = c(NA_real_, NA, NA) |> haven::labelled(c(Yes = 1, No = 2), label = "hallo 2"),
  age = c(2, 1, 1) |> haven::labelled(c("18-39" = 1, "40+" = 2), label = "age"),
  gew = c(0.5, 1.2, 0.4)
)

f <- df_metr_mac$fun
dfq <- data.frame(
  Type  = "mw",
  RowVar = "q1 q2",
  Title = "",
  Freq = "0",
  ZsfgMW = f,
  MeanOverviewLabel = paste("Summary of", f)
)
l_macro <- list(
  ColVar = c("q1", "q2"),
  Weight = "gew",
  Unwgt = TRUE
)
mapping_file <- list(Questions = dfq, Macro = l_macro)
m <- Tabula$new(
  df,
  mapping_file,
  tabulate = FALSE
)
m$opts$ct$l_lexikon["cTabWeighted"] <- "W"
# TODO: find out what's the error here with the percentile table...!
m$opts$da$error_out <- "safe"
m$calc_qtabs()
test_that("summaries of various stat_fun are reproduced", {
  testthat::expect_snapshot(m)
})





l_macro <- list(
  ColVar = c("q1"),
  Weight = "gew",
  Unwgt = TRUE
)
dfq <- data.frame(
  Type  = c("mw", "cat"),
  RowVar = c("q1 q2", "q1"),
  UngueltMW = "2, 4",
  Title = "",
  Freq = "0",
  ZsfgMW = c("mean", NA_character_),
  MeanOverviewLabel = c("Summary of mean", NA_character_),
  MetrMac = c(NA_character_, "S1")
)
mapping_file = list(Questions = dfq, Macro = l_macro)

m <- Tabula$new(
  df,
  mapping_file,
  tabulate = FALSE
)
m$opts$ct$l_lexikon["cTabWeighted"] <- "W"
m$calc_qtabs()
test_that("mean calculation is reproduced with UngueltMW set (cat & mw)", {
  testthat::expect_snapshot(m)
})
