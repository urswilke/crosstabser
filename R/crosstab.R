calc_detail_freqs <- function(qtab) {
  UseMethod("calc_detail_freqs")
}
calc_detail_freqs.qtab_type_cat <- function(qtab) {
  weight <- qtab$p$Weight
  stat_fun <- qtab$p$ZsfgMW
  if (length(stat_fun) == 0) {
    stat_fun = NA
  }
  long_data <- qtab$d$long_data

  # TODO: better use separate methods for cat & mdg..?
  if (!is.null(qtab$p$CatRec)) {
    long_data_catrec <- gen_catrec_long_data(qtab)
    long_data <- dplyr::bind_rows(long_data, long_data_catrec)
  }
  all_counts <- gen_all_counts(long_data, weight, stat_fun)
  all_counts$RowContent <- "Detail"
  all_counts$RowAbsPercent <- "Abs"

  rbind(
    qtab$d$stats_rows$total,
    qtab$d$stats_rows$sum_of_valid,
    all_counts,
    qtab$d$stats_rows$n_valid,
    qtab$d$stats_rows$no_entry
  )
}

calc_stats_rows <- function(qtab) {
  UseMethod("calc_stats_rows")
}
calc_stats_rows.default <- function(qtab) {
  NULL
}
calc_stats_rows.qtab_type_cat <- function(qtab) {
  long_data <- qtab$d$long_data
  weight <- qtab$p$Weight

  total_row_data <- gen_total_counts(long_data, weight)
  total_row_data$rowvar <- paste(qtab$p$RowVar, collapse = ", ")
  total_row_data$rowval <- 1
  total_row_data$RowContent <- "Total"
  total_row_data$RowAbsPercent <- "Abs"


  valid_row_data <- long_data[!long_data$rowval %in% qtab$p$Unguelt,] |> gen_total_counts(weight)
  valid_row_data$rowvar <- paste(qtab$p$RowVar, collapse = ", ")
  valid_row_data$rowval <- 1
  valid_row_data$RowContent <- "Valid"
  valid_row_data$RowAbsPercent <- "Abs"

  qtab$d$stats_rows <- list()
  qtab$d$stats_rows$n_valid <- valid_row_data
  qtab$d$stats_rows$total <- total_row_data

}

calc_detail_freqs.qtab_type_mdg <- function(qtab) {
  calc_detail_freqs.qtab_type_cat(qtab)
}
calc_stats_rows.qtab_type_mdg <- function(qtab) {
  df <- qtab$d$raw_data
  # for TOTAL column:
  df$"colvar_DC#STICHPROBE" <- 1

  mdg_val <- qtab$p$MdgVal

  df_cols <- df[paste0("colvar_", c(qtab$p$ColVar, "DC#STICHPROBE"))]
  df_rows <- df[paste0("rowvar_", qtab$p$RowVar)]

  df_cols$total <- rowSums(is.na(df[paste0("rowvar_", qtab$p$RowVar)])) < ncol(df_rows)
  sum_of_valid <- rowSums(df_rows == mdg_val, na.rm = TRUE)
  df_cols$sum_of_valid <- sum_of_valid
  df_cols$n_valid <- sum_of_valid >= 1
  df_cols$invalid_cts <- rowSums(df[paste0("rowvar_", qtab$p$Unguelt)] == mdg_val, na.rm = TRUE) != 0
  df_cols$no_entry <- as.numeric(sum_of_valid + df_cols$invalid_cts == 0)
  df_cols_long <- df_cols |>
    pivot_cols()

  row_types <- c("total", "sum_of_valid", "n_valid", "no_entry")
  if (!is.na(qtab$p$Weight)) {
    #TODO: check if that works and is good..:
    purrr::walk(row_types, \(x) df_cols_long[[x]] <- df_cols_long[[x]] * df_cols_long[[qtab$p$Weight]])
  }
  df_stats_rows <- stats::aggregate(. ~ colvar + colval, data = df_cols_long, sum) |> dplyr::as_tibble()
  l_row_types <- row_types |>
    purrr::set_names() |>
    lapply(\(x) {
      res <- df_stats_rows[c("colvar", "colval", x)]
      names(res)[3] <- "value"
      res$rowval <- 1
      res$rowvar <- paste(qtab$p$RowVar, collapse = ", ")
      res
    })
  l_row_types$total$RowContent <- "Total"
  l_row_types$sum_of_valid$RowContent <- "SumOfValid"
  l_row_types$no_entry$RowContent <- "Missing"
  l_row_types$total$RowAbsPercent <- "Abs"
  l_row_types$sum_of_valid$RowAbsPercent <- "Abs"
  l_row_types$no_entry$RowAbsPercent <- "Abs"
  l_row_types$no_entry <- l_row_types$no_entry[l_row_types$no_entry$value > 0,]
  # if (sum(l_row_types$no_entry$value) == 0 ) {
  #   l_row_types$no_entry$value <- NULL
  # }
  l_row_types$n_valid$RowContent <- "Valid"
  l_row_types$n_valid$RowAbsPercent <- "Abs"

  # stats_rows <- df_stats_rows |>
  #   tidyr::pivot_longer(
  #     -c(colvar, colval),
  #     names_to = "rowvar"
  #   )
  # stats_rows$rowval <- 1

  qtab$d$stats_rows <- l_row_types
}

# TODO: remove or use instead of new method..: (?)
gen_total_counts <- function(long_data, weight) {
  long_data |>
    dplyr::group_by(dplyr::across(-dplyr::matches("weight|rowva[rl]"))) |>
    new_sum_stat(weight, NA) |>
    apply_sum_stat()
}

gen_all_counts <- function(long_data, weight, stat_fun) {
  long_data |>
    dplyr::group_by(dplyr::across(-dplyr::matches("weight"))) |>
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

calc_detail_freqs.qtab_type_mw <- function(qtab) {
  weight <- qtab$p$Weight
  stat_fun <- qtab$p$ZsfgMW
  invalid_vals <- qtab$p$Unguelt

  res <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,] |>
    dplyr::group_by(dplyr::across(-dplyr::matches("weight|rowval"))) |>
    new_sum_stat(weight, stat_fun) |>
    apply_sum_stat()

  # to prevent warning when calling `gen_val_table()`...:
  # TODO: remove when refactoring gen_val_table()...!
  res$rowval <- NA_real_
  res$RowContent <- "MStatistics"
  res$RowAbsPercent <- "Percent"
  res
}
calc_detail_freqs.qtab_type_mcg <- function(qtab) {
  weight <- qtab$p$Weight
  stat_fun <- qtab$p$ZsfgMW
  long_data <- qtab$d$long_data
  sum_of_valid_row_data <- qtab$d$sum_of_valid_counts
  sum_of_valid_row_data$rowvar <- long_data$rowvar[1]
  sum_of_valid_row_data$rowval <- 1
  total_row_data <- qtab$d$total_row_counts
  total_row_data$rowvar <- long_data$rowvar[1]
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
    is.na(weight)  & is.na(stat_fun)      ~ "counts_unweighted",
    !is.na(weight) & is.na(stat_fun)      ~ "counts_weighted",
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
apply_sum_stat.sum_unweighted <- function(df_long, value_col = "rowval", ...) {
  df_long |>
    dplyr::summarize(value = sum(!!rlang::sym(value_col), na.rm = TRUE), .groups = "drop")
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


calc_percentages <- function(qtab) {
  UseMethod("calc_percentages")
}
# TODO:
calc_percentages.qtab_type_mcg <- calc_percentages.qtab_type_mw <- function(qtab) {
  NULL
}
calc_percentages.default <- function(qtab) {
  # TODO: store counts ("Detail") in separate field..:
  cts <- qtab$d$detail_freqs[qtab$d$detail_freqs$RowContent == "Detail",]
  percentages <- cts
  percentages$value <- as.numeric(percentages$value)
  vc <- qtab$d$stats_rows$n_valid
  # correspomding indices of percentages values in vc:
  idx <- match(paste(percentages$colvar, percentages$colval), paste(vc$colvar, vc$colval))
  percentages$value <- percentages$value / vc$value[idx]
  qtab$d$percentages <- percentages
}
