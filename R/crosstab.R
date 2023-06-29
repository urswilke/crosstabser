crosstab <- function(qtab) {
  UseMethod("crosstab")
}
crosstab.qtab_type_mdg <- crosstab.qtab_type_cat <- function(qtab) {
  weight <- qtab$p$Weight
  stat_fun <- qtab$p$ZsfgMW
  if (length(stat_fun) == 0) {
    stat_fun = NA
  }
  long_data <- qtab$d$long_data

  total_row_data <- gen_total_counts(long_data, weight)
  total_row_data$rowvar <- paste0(long_data$rowvar[1], "_TC")
  total_row_data$rowval <- 1

  # TODO: move to class definitions...:
  row_id_name <- ifelse(qtab$p$Type == "mdg", "rowvar", "rowval")
  valid_row_data <- long_data[!long_data[[row_id_name]] %in% qtab$p$Unguelt,] |> gen_total_counts(weight)
  valid_row_data$rowvar <- paste0(paste(qtab$p$RowVar, collapse = ", "), "_VC")
  valid_row_data$rowval <- 1

  if (!is.null(qtab$p$CatRec)) {
    long_data_catrec <- gen_catrec_long_data(qtab)
    long_data <- dplyr::bind_rows(long_data, long_data_catrec)
  }
  all_counts <- gen_all_counts(long_data, weight, stat_fun)

  rbind(
    total_row_data,
    all_counts,
    valid_row_data
  )
}

gen_total_counts <- function(long_data, weight) {
  long_data |>
    dplyr::group_by(dplyr::across(-matches("weight|rowva[rl]"))) |>
    new_sum_stat(weight, NA) |>
    apply_sum_stat()
}

gen_all_counts <- function(long_data, weight, stat_fun) {
  long_data |>
    dplyr::group_by(dplyr::across(-matches("weight"))) |>
    new_sum_stat(weight, stat_fun) |>
    apply_sum_stat()
}

gen_catrec_long_data <- function(qtab) {
  cat_rec_string <- qtab$p$CatRec
  cat_lab_string <- qtab$p$CatLab
  cat_rec_interval_splits <- split_cat_rec_string(cat_rec_string)
  cat_lab_splits <- split_cat_lab_string(cat_lab_string)
  cat_rec_quos <- lapply(cat_rec_interval_splits$interval_strings, gen_cat_rec_fun)
  cat_rec_exprs <- stringr::str_extract_all(cat_rec_string, "(?<=\\().*?(?=\\))")[[1]]
  cat_rec_vals <- stringr::str_extract(cat_rec_exprs, "(?<=\\=) *\\d+$") |> as.numeric()

  invalid_vals <- qtab$p$Unguelt

  long_data_catrec <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,]
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

crosstab.qtab_type_mw <- function(qtab) {
  weight <- qtab$p$Weight
  stat_fun <- qtab$p$ZsfgMW
  invalid_vals <- qtab$p$Unguelt

  qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,] |>
    dplyr::group_by(dplyr::across(-matches("weight|rowval"))) |>
    new_sum_stat(weight, stat_fun) |>
    apply_sum_stat()
}
crosstab.qtab_type_mcg <- function(qtab) {
  weight <- qtab$p$Weight
  stat_fun <- qtab$p$ZsfgMW
  long_data <- qtab$d$long_data
  sum_of_valid_row_data <- qtab$d$sum_of_valid_counts
  sum_of_valid_row_data$rowvar <- paste0(long_data$rowvar[1], "_VC")
  sum_of_valid_row_data$rowval <- 1
  total_row_data <- qtab$d$total_row_counts
  total_row_data$rowvar <- paste0(long_data$rowvar[1], "_TC")
  total_row_data$rowval <- 1
  all_counts <- gen_all_counts(long_data, weight, stat_fun)
  rbind(
    total_row_data,
    sum_of_valid_row_data,
    all_counts
  )
}

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
