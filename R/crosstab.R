crosstab <- function(df_row, long_data, mapping) {
  UseMethod("crosstab")
}
crosstab.tab_type_cat <- function(df_row, long_data, mapping) {
  weight <- dplyr::coalesce(mapping$options$l_macro_scenario$Weight, df_row$Weight)
  stat_fun <- df_row$ZsfgMW
  total_row_data <- long_data |>
    dplyr::group_by(dplyr::across(-matches("weight|rowva[rl]"))) |>
    new_sum_stat(weight, NA) |>
    apply_sum_stat()
  total_row_data$rowvar <- paste0(long_data$rowvar[1], "_TC")
  total_row_data$rowval <- 1
  if (!is.na(df_row$CatRec)) {
    long_data_catrec <- gen_catrec_long_data(df_row, long_data, mapping)
    long_data <- dplyr::bind_rows(long_data, long_data_catrec)
  }
  all_counts <- long_data |>
    dplyr::group_by(dplyr::across(-matches("weight"))) |>
    new_sum_stat(weight, stat_fun) |>
    apply_sum_stat()
  rbind(
    total_row_data,
    all_counts
  )
}
gen_catrec_long_data <- function(df_row, long_data, mapping) {
  cat_rec_string <- df_row$CatRec
  cat_lab_string <- df_row$CatLab
  cat_rec_interval_splits <- split_cat_rec_string(cat_rec_string)
  cat_lab_splits <- split_cat_lab_string(cat_lab_string)
  cat_rec_quos <- lapply(cat_rec_interval_splits$interval_strings, gen_cat_rec_fun)
  cat_rec_exprs <- stringr::str_extract_all(cat_rec_string, "(?<=\\().*?(?=\\))")[[1]]
  cat_rec_vals <- stringr::str_extract(cat_rec_exprs, "(?<=\\=) *\\d+$") |> as.numeric()

  invalid_vals <- dplyr::coalesce(df_row$Unguelt[[1]], mapping$options$l_macro_scenario$Unguelt)

  long_data_catrec <- long_data[!long_data$rowval %in% invalid_vals,]
  # TODO: find cleander solution where `vec` doesn't need to be calculated in advance!
  vec <- long_data_catrec$rowval
  l_cat_rec <- purrr::map2(
    cat_rec_quos,
    cat_rec_vals,
    \(f, x) rlang::quo(!!f(vec) ~ !!x)
  )
  long_data_catrec$rowvar <- paste0(long_data_catrec$rowvar, "__summary")
  long_data_catrec$rowval <- dplyr::case_when(!!!l_cat_rec)
  # TODO: also tabulate non-recoded (not covered by CatRec) valid `RowVal`s:
  non_recoded_idx <- is.na(long_data_catrec$rowval)
  if (any(non_recoded_idx)) {
    warning(
      "\nIn table in row ", df_row$row, ":\n",
      "These valid values are not recoded by CatRec: ",
      vec[non_recoded_idx] |> unique(),
      "\nTabulation not implemented yet!!!"
    )
    long_data_catrec <- long_data_catrec[!non_recoded_idx,]
  }
  long_data_catrec
}

crosstab.tab_type_mw <- function(df_row, long_data, mapping) {
  weight <- dplyr::coalesce(mapping$options$l_macro_scenario$Weight, df_row$Weight)
  stat_fun <- dplyr::coalesce(df_row$ZsfgMW, "mean")
  invalid_vals <- df_row$Unguelt[[1]]
  if (is.na(invalid_vals[1])) {
    invalid_vals <- mapping$options$l_macro_scenario$Unguelt
  }

  long_data[!long_data$rowval %in% invalid_vals,] |>
    dplyr::group_by(dplyr::across(-matches("weight|rowval"))) |>
    new_sum_stat(weight, stat_fun) |>
    apply_sum_stat()
}
crosstab.tab_type_mcg <- crosstab.tab_type_cat
crosstab.tab_type_mdg <- crosstab.tab_type_cat


# hack to do double dispatch on is.na(weight) & stat_fun:
new_sum_stat <- function(df_long, weight, stat_fun) {
  subclass_str <- dplyr::case_when(
    is.na(weight)  & is.na(stat_fun)    ~ "counts_unweighted",
    !is.na(weight) & is.na(stat_fun)    ~ "counts_weighted",
    is.na(weight)  & stat_fun == "mean"   ~ "mean_unweighted",
    is.na(weight)  & stat_fun == "median" ~ "median_unweighted",
    is.na(weight)  & stat_fun == "sum"    ~ "sum_unweighted",
    !is.na(weight) & stat_fun == "mean"   ~ "mean_weighted",
    !is.na(weight) & stat_fun == "median" ~ "median_weighted",
    !is.na(weight) & stat_fun == "sum"    ~ "sum_weighted"
  )
  if (length(subclass_str) == 0) {
    subclass_str <- "counts_unweighted"
  }
  structure(df_long, class = c(subclass_str, class(df_long)))
}
apply_sum_stat <- function(df_long, ...) {
  UseMethod("apply_sum_stat")
}
apply_sum_stat.counts_unweighted <- function(df_long, ...) {
  df_long |>
    dplyr::summarize(
      value = dplyr::n(),
      .groups = "drop"
    )
}
apply_sum_stat.counts_weighted <- function(df_long, ...) {
  df_long |>
    dplyr::summarize(value = sum(weight, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.mean_unweighted <- function(df_long, ...) {
  df_long |>
    dplyr::summarize(value = mean(rowval, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.median_unweighted <- function(df_long, ...) {
  df_long |>
    dplyr::summarize(value = stats::median(rowval, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.sum_unweighted <- function(df_long, ...) {
  df_long |>
    dplyr::summarize(value = sum(rowval, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.mean_weighted <- function(df_long, ...) {
  df_long |>
    tidyr::drop_na(weight) |>
    dplyr::summarize(value = stats::weighted.mean(rowval, weight, na.rm = TRUE), .groups = "drop")
}
apply_sum_stat.median_weighted <- function(df_long, ...) {
  df_long |>
    dplyr::summarize(
      value = matrixStats::weightedMedian(rowval, weight, na.rm = TRUE, ties = "mean"),
      .groups = "drop"
    )
}
apply_sum_stat.sum_weighted <- function(df_long, ...) {
  df_long |>
    dplyr::summarize(value = sum(rowval * weight, na.rm = TRUE), .groups = "drop")
}
