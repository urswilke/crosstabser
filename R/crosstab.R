rbind_table_numbers <- function(qtab) {
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
    qtab$d$invalid_freqs,
    qtab$d$invalid_percentages,
    qtab$d$stats_rows$no_entry,
    qtab$d$no_entry_percentages
  )
}

calc_stat_fun <- function(qtab) {
  UseMethod("calc_stat_fun")
}
calc_stat_fun.default <- function(qtab) {
  NULL
}
calc_stat_fun.qtab_type_mw <- function(qtab) {
  invalid_vals <- c(qtab$p[["Unguelt"]], qtab$p[["UngueltMW"]])
  long_data <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,]

  if (!is.null(qtab$p$MWRec)) {
    long_data$rowval <- mw_rec_helper(long_data$rowval, qtab$p$MWRec)
  }
  stat_fun <- qtab$p$df_stat_funs$fun %||% "mean"
  res <- long_data |>
    summarize_stats(
      "rowval",
      wt = qtab$p$Weight[[1]],
      stat_fun = stat_fun,
      .by = c("rowvar", "colvar", "colval")
    )
  # TODO: With the refactoring, RowStatFun needs to be added here
  # and I don't understand why...
  # perhaps we need qtab$p$df_stat_funs$row_title instead (?)
  res$RowStatFun <- stat_fun
  res <- res |> add_missing_cases(qtab)

  res$row_type <- "fun_stats"
  res$RowContent <- "MStatistics"
  res$rowval <- NA_real_
  res$RowAbsPercent <- "Percent"
  qtab$d$fun_stats <- res
}
add_missing_cases <- function(df, qtab) {
  df |> tidyr::complete(
    rowvar = qtab$p$rowvars_string,
    tidyr::nesting(
      colvar = qtab$d$col_table$ColVariable,
      colval = qtab$d$col_table$ColValue
    ),
    RowStatFun = qtab$p$df_stat_funs$row_title,
  )
}
calc_stat_fun.qtab_type_cat <- function(qtab) {
  if (is.null(qtab$p$MetrMac)) {
    return(NULL)
  }
  invalid_vals <- c(qtab$p[["Unguelt"]], qtab$p[["UngueltMW"]])
  long_data <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,]
  if (nrow(long_data) == 0) {
    return()
  }
  if (!is.null(qtab$p$MWRec)) {
    long_data$rowval <- mw_rec_helper(long_data$rowval, qtab$p$MWRec)
  }

  df_stat_funs <- qtab$p$df_stat_funs
  l <- df_stat_funs |>
    split(seq_len(nrow(df_stat_funs))) |>
    purrr::set_names(df_stat_funs$row_title)
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
    ) |> add_missing_cases(qtab)
  res$row_type <- "fun_stats"
  res$RowContent <- "Statistics"
  res$rowval <- 100
  res$RowAbsPercent <- "Percent"
  qtab$d$fun_stats <- res
}

calc_detail_freqs <- function(qtab) {
  UseMethod("calc_detail_freqs")
}
calc_detail_freqs.qtab_type_cat <- function(qtab) {
  long_data <- qtab$d$long_data[!is.na(qtab$d$long_data$rowval),]
  if (nrow(long_data) == 0) {
    return()
  }
  all_counts <- long_data |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  all_counts$RowAbsPercent <- "Abs"

  if (!is.null(qtab$p$Einzelauspraegung) && qtab$p$Einzelauspraegung %in% c("0", "FALSE")) {
    detail_freqs <- NULL
  } else {
    detail_freqs <- all_counts[!all_counts$rowval %in% qtab$p[["Unguelt"]],]
    detail_freqs$row_type <- "detail_freqs_valid"
    detail_freqs$RowContent <- "Detail"
  }

  invalid_freqs <- all_counts[all_counts$rowval %in% qtab$p[["Unguelt"]],]
  invalid_freqs$row_type <- "detail_freqs_invalid"
  invalid_freqs$RowContent <- "Missing"

  qtab$d$detail_freqs <- detail_freqs
  qtab$d$invalid_freqs <- invalid_freqs
}

calc_stats_rows <- function(qtab) {
  UseMethod("calc_stats_rows")
}
calc_stats_rows.default <- function(qtab) {
  NULL
}
calc_stats_rows.qtab_type_cat <- function(qtab) {
  weight <- qtab$p$Weight[[1]]
  group_variables <- c("colvar", "colval")
  invalid_vals <- c(qtab$p[["Unguelt"]], NA)

  long_data <- qtab$d$long_data

  total <- long_data[!is.na(long_data$rowval),] |>
    summarize_stats(NULL, weight, .by = group_variables)
  n_valid <- long_data[!long_data$rowval %in% invalid_vals,] |>
    summarize_stats(NULL, weight, .by = group_variables)

  total$row_type <- "total"
  total$RowContent <- "Total"
  total$RowAbsPercent <- "Abs"
  total$rowvar <- qtab$p$rowvars_string
  total$rowval <- 1

  n_valid$row_type <- "n_valid_freqs"
  n_valid$RowContent <- "Valid"
  n_valid$RowAbsPercent <- "Abs"
  n_valid$rowvar <- qtab$p$rowvars_string
  n_valid$rowval <- 1

  qtab$d$stats_rows <- tibble::lst(
    total,
    n_valid,
  )
}

calc_detail_freqs.qtab_type_mdg <- function(qtab) {
  long_data <- qtab$d$long_data
  if (nrow(long_data) == 0) {
    return()
  }
  all_counts <- long_data[long_data$val_to_count,] |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  all_counts$RowAbsPercent <- "Abs"

  is_valid <- !all_counts$rowvar %in% (qtab$p$l_selvar$invalid %||% qtab$p[["Unguelt"]])
  detail_freqs <- all_counts[is_valid,]


  detail_freqs$row_type <- "detail_freqs_valid"
  detail_freqs$RowContent <- "Detail"


  # TODO: fix counting as done when Exclusive is set...:
  invalid_freqs <- all_counts[!is_valid,]
  invalid_freqs$row_type <- "detail_freqs_invalid"
  invalid_freqs$RowContent <- "Missing"


  qtab$d$detail_freqs <- detail_freqs
  qtab$d$invalid_freqs <- invalid_freqs
}
calc_stats_rows.qtab_type_mdg <- function(qtab) {
  mdg_val <- qtab$p$MdgVal

  valid_rowvars <- qtab$p$l_selvar$valid %||% qtab$p$RowVar
  invalid_rowvars <- qtab$p$l_selvar$invalid %||% qtab$p[["Unguelt"]]
  all_rowvars <- c(
    valid_rowvars,
    invalid_rowvars
  )
  case_distinguisher <- qtab$p$case_distinguisher
  group_variables <- c("colvar", "colval")
  all_group_variables <- c(case_distinguisher, group_variables)


  # TODO: move the handling of no_entry data in a pre-processing step...(?):
  # cf. pivot_rowvar_data.qtab_type_mdg & now_do_colvar.qtab_type_mdg
  long_data_all <- qtab$d$long_data_all
  no_entry_data <- long_data_all |>
    dplyr::summarise(
      has_no_entry = !any(rowvar %in% all_rowvars & rowval == mdg_val),
      .by = all_of(c(all_group_variables, qtab$p$long_weight))
    )
  no_entry_data <- no_entry_data[no_entry_data$has_no_entry,]
  no_entry_data$has_no_entry <- NULL

  valid_no_entry <- NULL
  if (qtab$p$MdgMissValid) {
    valid_no_entry <- no_entry_data
    no_entry_data$val_to_count <- TRUE
    no_entry_data$rowvar <- "valid_no_entry"
    no_entry_data$rowval <- 1
    qtab$d$long_data <- qtab$d$long_data |> dplyr::bind_rows(no_entry_data)
    no_entry_data <- no_entry_data[c(),]
  }
  df_long <- qtab$d$long_data

  df_long_total <- df_long[df_long$val_to_count,]
  df_long_valid <- df_long[df_long$val_to_count & df_long$rowvar %in% c(valid_rowvars, "valid_no_entry"),]

  weight <- qtab$p$Weight[[1]]

  total <- df_long_total[!duplicated(df_long_total[c(case_distinguisher, group_variables)], fromLast = TRUE),] |>
    dplyr::bind_rows(no_entry_data) |>
    summarize_stats(NULL, weight, .by = group_variables)

  no_entry <- no_entry_data |>
    summarize_stats(NULL, weight, .by = group_variables)
  sum_of_valid <- df_long_valid |>
    summarize_stats(NULL, weight, .by = group_variables)
  n_valid <- df_long_valid |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(case_distinguisher, group_variables, qtab$p$long_weight)))) |>
    summarize_stats(NULL, weight, .by = group_variables)


  no_entry$rowvar <- qtab$p$rowvars_string
  no_entry$rowval <- 1
  no_entry$RowContent <- "Missing"
  no_entry$RowAbsPercent <- "Abs"
  no_entry$row_type <- "detail_freqs_invalid"

  sum_of_valid$RowContent <- "SumOfValid"
  sum_of_valid$RowAbsPercent <- "Abs"
  sum_of_valid$rowvar <- qtab$p$rowvars_string
  sum_of_valid$rowval <- 1
  sum_of_valid$row_type <- "sum_of_valid"

  total$RowContent <- "Total"
  total$RowAbsPercent <- "Abs"
  total$rowvar <- qtab$p$rowvars_string
  total$rowval <- 1
  total$row_type <- "total"

  n_valid$RowContent <- "Valid"
  n_valid$RowAbsPercent <- "Abs"
  n_valid$rowvar <- qtab$p$rowvars_string
  n_valid$rowval <- 1
  n_valid$row_type <- "n_valid_freqs"

  qtab$d$stats_rows <- tibble::lst(
    total,
    sum_of_valid,
    no_entry,
    n_valid,
    # TODO: move the valid_no_entry methodology somewhere else (?):
    valid_no_entry,
  )
}

calc_stats_rows.qtab_type_mw <- function(qtab) {
  invalid_vals <- c(qtab$p[["Unguelt"]], NA)

  long_data <- qtab$d$long_data

  keep_row <- long_data[["selvar_dup"]] %||% TRUE

  case_distinguisher <- qtab$p$case_distinguisher
  weight_string <- if (
    !is.null(qtab$p$Weight[[1]])
  ) {
    "weight"
  } else {
    NULL
  }

  any_valid_grouping <- c(
    "colvar",
    "colval",
    case_distinguisher,
    weight_string
  )
  at_least_one_valid <- long_data |>
    _[keep_row, ] |>
    dplyr::summarise(
      n_valid = any_valid(rowval, invalid_vals),
      .by = all_of(any_valid_grouping)
    )

  row_types <- c("n_valid")
  df_stats_rows <- at_least_one_valid |>
    summarize_stats(
      row_types,
      wt = qtab$p$Weight[[1]],
      stat_fun = "sum",
      .by = c("colvar", "colval")
    )


  df_stats_rows$row_type <- "n_valid_mw"
  df_stats_rows$rowval <- 1
  df_stats_rows$rowvar <- qtab$p$rowvars_string

  df_stats_rows$RowContent <- "Valid"
  df_stats_rows$RowAbsPercent <- "Abs"

  qtab$d$stats_rows <- list(n_valid = df_stats_rows)
}

any_valid <- function(rowval, invalid_vals) {
  !all(rowval %in% invalid_vals)
}

calc_stats_rows.qtab_type_mcg <- function(qtab) {
  invalid_vals <- qtab$p[["Unguelt"]]

  df_long <- qtab$d$long_data
  df_long_valid <- df_long[!(df_long$rowval %in% invalid_vals) & df_long$val_to_count,]
  group_variables <- c("colvar", "colval")
  total <- df_long |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("i", group_variables, qtab$p$long_weight)))) |>
    summarize_stats(NULL, qtab$p$Weight[[1]], .by = group_variables)
  sum_of_valid <- df_long_valid |>
    summarize_stats(NULL, qtab$p$Weight[[1]], .by = group_variables)
  n_valid <- df_long_valid |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("i", group_variables, qtab$p$long_weight)))) |>
    summarize_stats(NULL, qtab$p$Weight[[1]], .by = group_variables)


  total$row_type <- "total"
  total$RowContent <- "Total"
  total$RowAbsPercent <- "Abs"
  total$rowvar <- df_long$rowvar[1]
  total$rowval <- 1

  sum_of_valid$row_type <- "sum_of_valid"
  sum_of_valid$RowContent <- "SumOfValid"
  sum_of_valid$RowAbsPercent <- "Abs"
  sum_of_valid$rowvar <- df_long$rowvar[1]
  sum_of_valid$rowval <- 1

  n_valid$row_type <- "n_valid_freqs"
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
  invalid_vals <- c(NA, qtab$p[["Unguelt"]])

  df_long <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,]
  if (nrow(df_long) == 0) {
    return()
  }

  catrec_strings <- strsplit(qtab$p$CatRec, "\\|")[[1]]
  all_counts <- catrec_strings |>
    lapply(\(x) summarise_catrec(df_long, x, qtab$p$Weight[[1]])) |>
    dplyr::bind_rows(.id = "i_catrec")
  all_counts$RowContent <- "Summary"
  all_counts$RowAbsPercent <- "Abs"
  all_counts$row_type <- "summary_freqs"
  all_counts
}
summarise_catrec <- function(df_long, catrec_string, wt) {
  catrec_sum_string <- catrec_string |> stringr::str_extract("(?<=\\{).*(?=\\})")
  df_long$rowval <- mw_rec_helper(df_long$rowval, catrec_string)
  df_long$rowvar <- paste0(df_long$rowvar, "__summary")
  non_recoded_idx <- is.na(df_long$rowval)

  res <- df_long |>
    summarize_stats(
      NULL,
      wt = wt,
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  if (is.na(catrec_sum_string)) {
    return(res)
  }
  df_user_expr <- parse_catrec_sum_expr(res, catrec_sum_string)
  rbind(res, df_user_expr)
}
parse_catrec_sum_expr <- function(df_summary, catrec_sum_string) {
  expr_string <- catrec_sum_string |> stringr::str_remove("=.*")
  translate_user_expression <- function(expr_string) {
    expr_translated <- expr_string |>
      stringr::str_replace_all("(\\[)(\\d+)(\\])", "value[rowval == \\2]")
    rlang::parse_expr(expr_translated)
  }
  df_user_expr <- df_summary |>
    tidyr::complete(
      rowvar, rowval,
      tidyr::nesting(colvar, colval),
      fill = list(value = 0)
    ) |>
    dplyr::summarise(
      value = !!translate_user_expression(expr_string),
      .by = c("rowvar", "colvar", "colval")
    )
  df_user_expr$rowval <- max(df_summary$rowval) + 1
  df_user_expr
}

calc_detail_freqs.qtab_type_mw <- function(qtab) {
  weight <- qtab$p$Weight[[1]]
  # TODO: check all occurrences of qtab$p[["Unguelt"]] (a lot) if NA also has to be included!
  # (except mdg of course)
  invalid_vals <- c(NA, qtab$p[["Unguelt"]])

  res <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,] |>
    summarize_stats(
      "rowval",
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "colvar", "colval")
    )
  res$row_type <- "fun_valid"

  res$RowContent <- "MValid"
  res$RowAbsPercent <- "Abs"
  qtab$d$detail_freqs <- res
}
calc_detail_freqs.qtab_type_mcg <- function(qtab) {
  weight <- qtab$p$Weight[[1]]
  long_data <- qtab$d$long_data
  long_data[["i"]] <- NULL

  all_counts <- long_data[long_data$val_to_count,] |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  all_counts$RowAbsPercent <- "Abs"

  detail_freqs <- all_counts[!all_counts$rowval %in% qtab$p[["Unguelt"]],]
  detail_freqs$row_type <- "detail_freqs_valid"
  detail_freqs$RowContent <- "Detail"

  invalid_freqs <- all_counts[all_counts$rowval %in% qtab$p[["Unguelt"]],]
  invalid_freqs$row_type <- "detail_freqs_invalid"
  invalid_freqs$RowContent <- "Missing"

  qtab$d$detail_freqs <- detail_freqs
  qtab$d$invalid_freqs <- invalid_freqs
}

calc_percentages <- function(qtab) {
  UseMethod("calc_percentages")
}
calc_percentages.qtab_type_mw <- function(qtab) {
  NULL
}
calc_percentages.default <- function(qtab) {
  # TODO: put elements in empty tibbles from the beginning?...:
  detail_freqs <- qtab$d$detail_freqs %||% tibble::tibble()
  detail_freqs$row_type <- "detail_perc_valid"
  catrec_freqs <- qtab$d$catrec_freqs %||% tibble::tibble()
  catrec_freqs$row_type <- "summary_perc"
  cts <- dplyr::bind_rows(detail_freqs, catrec_freqs)
  vc <- qtab$d$stats_rows$n_valid
  calc_percentage_helper(cts, vc)
}
calc_invalid_percentages <- function(qtab) {
  UseMethod("calc_invalid_percentages")
}
calc_invalid_percentages.qtab_type_mw <- function(qtab) {
  NULL
}
calc_invalid_percentages.default <- function(qtab) {
  cts <- qtab$d$invalid_freqs
  tc <- qtab$d$stats_rows$total
  res <- calc_percentage_helper(cts, tc) %||% tibble::tibble()
  res$row_type <- "detail_perc_invalid"
  res
}
calc_percentage_helper <- function(cts, divider_cts) {
  if (nrow(cts) == 0) {
    return(NULL)
  }
  percentages <- cts
  percentages$value <- as.numeric(percentages$value)
  # avoid to divide by zero:
  divider_cts$value[divider_cts$value == 0] <- NA_integer_
  # corresponding indices of percentages values in divider_cts:
  idx <- match(
    paste(percentages$colvar, percentages$colval),
    paste(divider_cts$colvar, divider_cts$colval)
  )
  percentages$value <- 100 * percentages$value / divider_cts$value[idx]
  percentages$RowAbsPercent <- "Percent"
  percentages
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
  # for mdg, when there aren't any valid counts, they aren't written...:
  if (is.null(valid_cts)) return(NULL)

  # TODO: check if this can be changed to include implicit missing values in the
  # raw_data or rather the counts (make them explicit...)
  # valid_cts$value <- 100 * valid_cts$value / total_cts$value
  # this doesn't work if the number of valid counts is not equal to the number of total counts...
  res <- total_cts |> dplyr::select(-RowContent, value_tot = value, -row_type) |> merge(valid_cts, all.x = TRUE)
  raw_value <- 100 * res$value / res$value_tot
  # TODO: check with Wolf if this shouldn't be NA instead of 0:
  raw_value[res$value_tot == 0] <- 0
  res$row_type <- "n_valid_perc"
  res$value <- raw_value
  res$value_tot <- NULL
  res$RowAbsPercent <- "Percent"

  res
}


calc_no_entry_percentages <- function(qtab) {
  UseMethod("calc_no_entry_percentages")
}
calc_no_entry_percentages.default <- function(qtab) {
  NULL
}
calc_no_entry_percentages.qtab_type_mdg <- function(qtab) {
  if (is.null(qtab$d$stats_rows$no_entry)) {
    return(NULL)
  }
  total_cts <- qtab$d$stats_rows$total
  no_entry_cts <- qtab$d$stats_rows$no_entry
  res <- total_cts |> dplyr::select(-RowContent, value_tot = value, -row_type) |> merge(no_entry_cts, all.x = TRUE)
  res$value <- 100 * res$value / res$value_tot
  res$value_tot <- NULL
  res$row_type <- "detail_perc_invalid"
  res$RowAbsPercent <- "Percent"

  res

}
