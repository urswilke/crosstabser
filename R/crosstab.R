merge_table_parts <- function(qtab) {
  # TODO: replaced rbind -> check if the dataframe parts can by simplified (e.g.
  # setting a column to NA shouldn't be necessary anymore...):
  dplyr::bind_rows(
    qtab$d$stats_rows$total,
    qtab$d$stats_rows$sum_of_valid,
    qtab$d$fun_stats,
    qtab$d$detail_freqs,
    qtab$d$catrec_freqs,
    qtab$d$percentages,
    qtab$d$vc_percentages,
    qtab$d$stats_rows$n_valid,
    qtab$d$stats_rows$no_entry
  )
}

# TODO: think whether cat and mw can/should be treated with the same function:
calc_stat_fun <- function(qtab) {
  UseMethod("calc_stat_fun")
}
calc_stat_fun.default <- function(qtab) {
  NULL
}
# TODO: add tests for stat_fun = median, sum, ...:
calc_stat_fun.qtab_type_mw <- function(qtab) {
  long_data <- qtab$d$long_data
  res <- long_data |>
    summarize_stats(
      "rowval",
      wt = qtab$p$Weight[[1]],
      stat_fun = qtab$p$stat_fun,
      .by = c("rowvar", "colvar", "colval")
    )
  res$RowContent <- "MStatistics"
  res$rowval <- NA_real_
  res$RowAbsPercent <- "Percent"
  qtab$d$fun_stats <- res
}
calc_stat_fun.qtab_type_cat <- function(qtab) {
  if (is.null(qtab$p$MetrMac)) {
    return(NULL)
  }
  long_data <- qtab$d$long_data
  df_stat_funs <- qtab$p$df_stat_funs
  l <- df_stat_funs |>
    split(seq_len(nrow(df_stat_funs))) |>
    purrr::set_names(df_stat_funs$fun)
  res <- l |>
    purrr::map_dfr(
      \(x) long_data |>
        summarize_stats(
          "rowval",
          wt = qtab$p$Weight[[1]],
          stat_fun = x$fun,
          probs = x$quantile_val[[1]],
          .by = c("rowvar", "colvar", "colval")
        ),
      .id = "RowStatFun"
    )
  res$RowContent <- "Statistics"
  res$rowval <- 100
  res$RowAbsPercent <- "Percent"
  qtab$d$fun_stats <- res
}

calc_detail_freqs <- function(qtab) {
  UseMethod("calc_detail_freqs")
}
calc_detail_freqs.qtab_type_cat <- function(qtab) {
  if (!is.null(qtab$p$Einzelauspraegung) && qtab$p$Einzelauspraegung == "0") {
    return(NULL)
  }
  long_data <- qtab$d$long_data

  all_counts <- long_data |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  all_counts$RowContent <- "Detail"
  all_counts$RowAbsPercent <- "Abs"

  all_counts
}

calc_stats_rows <- function(qtab) {
  UseMethod("calc_stats_rows")
}
calc_stats_rows.default <- function(qtab) {
  NULL
}
calc_stats_rows.qtab_type_cat <- function(qtab) {
  df <- qtab$d$raw_data
  # for TOTAL column:
  df$"colvar_DC#STICHPROBE" <- 1

  df_cols <- df[c(qtab$p$long_colvars, qtab$p$long_weight)]

  df_cols$n_valid <- !df[[qtab$p$long_rowvars]] %in% qtab$p$Unguelt
  df_cols$total <- !is.na(df[[qtab$p$long_rowvars]])

  # TODO: find better organisation (redundant code with calc_stats_rows.qtab_type_mdg):
  df_cols_long <- df_cols |>
    pivot_cols()

  row_types <- c("total", "n_valid")
  df_stats_rows <- df_cols_long |>
    summarize_stats(
      row_types,
      wt = qtab$p$Weight[[1]],
      stat_fun = "sum",
      .by = c("colvar", "colval")
    )

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
  l_row_types$total$RowAbsPercent <- "Abs"
  l_row_types$n_valid$RowContent <- "Valid"
  l_row_types$n_valid$RowAbsPercent <- "Abs"

  qtab$d$stats_rows <- l_row_types
}

calc_detail_freqs.qtab_type_mdg <- function(qtab) {
  calc_detail_freqs.qtab_type_cat(qtab)
}
calc_stats_rows.qtab_type_mdg <- function(qtab) {
  df <- qtab$d$raw_data
  # for TOTAL column:
  df$"colvar_DC#STICHPROBE" <- 1

  mdg_val <- qtab$p$MdgVal

  df_cols <- df[c(qtab$p$long_colvars, qtab$p$long_weight)]
  df_rows <- df[qtab$p$long_rowvars]

  df_cols$total <- rowSums(is.na(df[qtab$p$long_rowvars])) < ncol(df_rows)
  sum_of_valid <- rowSums(df_rows == mdg_val, na.rm = TRUE)
  df_cols$sum_of_valid <- sum_of_valid
  df_cols$n_valid <- sum_of_valid >= 1
  df_cols$invalid_cts <- rowSums(df[paste0("rowvar_", qtab$p$Unguelt)] == mdg_val, na.rm = TRUE) != 0
  df_cols$no_entry <- as.numeric(sum_of_valid + df_cols$invalid_cts == 0)
  df_cols_long <- df_cols |>
    pivot_cols()

  row_types <- c("total", "sum_of_valid", "n_valid", "no_entry")
  df_stats_rows <- df_cols_long |>
    summarize_stats(
      row_types,
      wt = qtab$p$Weight[[1]],
      stat_fun = "sum",
      .by = c("colvar", "colval")
    )

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

  l_row_types$n_valid$RowContent <- "Valid"
  l_row_types$n_valid$RowAbsPercent <- "Abs"

  qtab$d$stats_rows <- l_row_types
}

calc_stats_rows.qtab_type_mw <- function(qtab) {
  invalid_vals <- qtab$p$Unguelt
  df <- qtab$d$raw_data
  # for TOTAL column:
  df$"colvar_DC#STICHPROBE" <- 1

  mdg_val <- qtab$p$MdgVal

  df_cols <- df[c(qtab$p$long_colvars, qtab$p$long_weight)]
  df_rows <- df[qtab$p$long_rowvars]

  df_cols$n_valid <- rowSums(sapply(df_rows, Negate(`%in%`), invalid_vals)) >= 1
  df_cols_long <- df_cols |>
    pivot_cols()

  row_types <- c("n_valid")
  df_stats_rows <- df_cols_long |>
    summarize_stats(
      row_types,
      wt = qtab$p$Weight[[1]],
      stat_fun = "sum",
      .by = c("colvar", "colval")
    )

  df_stats_rows$rowval <- 1
  df_stats_rows$rowvar <- paste(qtab$p$RowVar, collapse = ", ")

  df_stats_rows$RowContent <- "Valid"
  df_stats_rows$RowAbsPercent <- "Abs"

  qtab$d$stats_rows <- list(n_valid = df_stats_rows)
}

calc_stats_rows.qtab_type_mcg <- function(qtab) {
  invalid_vals <- qtab$p$Unguelt

  df_long <- qtab$d$long_data
  df_long_valid <- df_long[!df_long$rowval %in% invalid_vals,]
  group_variables <- c("colvar", "colval")
  # the next is equivalent to (for unweighted):
  # total <- df_long |> dplyr::summarise(value = dplyr::n_distinct(i), .by = c("colvar", "colval"))
  total <- df_long |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("i", group_variables)))) |>
    summarize_stats(NULL, qtab$p$Weight[[1]], .by = group_variables)
  sum_of_valid <- df_long_valid |>
    summarize_stats(NULL, qtab$p$Weight[[1]], .by = group_variables)
  n_valid <- df_long_valid |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("i", group_variables)))) |>
    summarize_stats(NULL, qtab$p$Weight[[1]], .by = group_variables)


  total$RowContent <- "Total"
  total$RowAbsPercent <- "Abs"
  total$rowvar <- df_long$rowvar[1]
  total$rowval <- 1
  sum_of_valid$RowContent <- "SumOfValid"
  sum_of_valid$RowAbsPercent <- "Abs"
  sum_of_valid$rowvar <- df_long$rowvar[1]
  sum_of_valid$rowval <- 1
  n_valid$RowContent <- "Valid"
  n_valid$RowAbsPercent <- "Abs"
  n_valid$rowvar <- df_long$rowvar[1]
  n_valid$rowval <- 1


  qtab$d$stats_rows <- list(
    total = total,
    sum_of_valid = sum_of_valid,
    n_valid = n_valid
  )
}

calc_catrec_freqs <- function(qtab) {
  UseMethod("calc_catrec_freqs")
}
calc_catrec_freqs.default <- function(qtab) {
  NULL
}

calc_catrec_freqs.qtab_type_cat <- function(qtab) {
  if (is.null(qtab$p$CatRec)) {
    return(NULL)
  }
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
  all_counts <- long_data_catrec |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  all_counts$RowContent <- "Summary"
  all_counts$RowAbsPercent <- "Abs"
  all_counts
}

calc_detail_freqs.qtab_type_mw <- function(qtab) {
  weight <- qtab$p$Weight[[1]]
  invalid_vals <- qtab$p$Unguelt

  res <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,] |>
    summarize_stats(
      "rowval",
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "colvar", "colval")
    )

  # to prevent warning when calling `gen_val_table()`...:
  # TODO: remove when refactoring gen_val_table()...!
  res$rowval <- NA_real_
  res$RowContent <- "MValid"
  res$RowAbsPercent <- "Abs"
  res
}
calc_detail_freqs.qtab_type_mcg <- function(qtab) {
  weight <- qtab$p$Weight[[1]]
  stat_fun <- qtab$p$ZsfgMW
  long_data <- qtab$d$long_data
  long_data[["i"]] <- NULL
  res <- long_data |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "rowval", "colvar", "colval")
    )
  res$RowContent <- "Detail"
  res$RowAbsPercent <- "Abs"
  res
}

calc_percentages <- function(qtab) {
  UseMethod("calc_percentages")
}
calc_percentages.qtab_type_mw <- function(qtab) {
  NULL
}
calc_percentages.default <- function(qtab) {
  cts <- rbind(qtab$d$detail_freqs, qtab$d$catrec_freqs)
  percentages <- cts
  percentages$value <- as.numeric(percentages$value)
  vc <- qtab$d$stats_rows$n_valid
  # avoid to divide by zero:
  vc$value[vc$value == 0] <- NA_integer_
  # correspomding indices of percentages values in vc:
  idx <- match(paste(percentages$colvar, percentages$colval), paste(vc$colvar, vc$colval))
  percentages$value <- percentages$value / vc$value[idx]
  percentages$RowAbsPercent <- "Percent"
  qtab$d$percentages <- percentages
}

calc_valid_counts_percentages <- function(qtab) {
  UseMethod("calc_valid_counts_percentages")
}
calc_valid_counts_percentages.qtab_type_mw <- function(qtab) {
  NULL
}
calc_valid_counts_percentages.default <- function(qtab) {
  total_cts <- qtab$d$stats_rows$total
  valid_cts <- qtab$d$stats_rows$n_valid

  valid_cts$value <- valid_cts$value / total_cts$value
  valid_cts$RowAbsPercent <- "Percent"

  qtab$d$vc_percentages <- valid_cts
}
