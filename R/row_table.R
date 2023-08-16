gen_row_table <- function(qtab) {
  row_table <- rbind(
    row_table_total_line(qtab),
    row_table_valid_mw(qtab),
    row_table_valid_answers_line(qtab),
    row_table_body(qtab),
    row_table_summary(qtab),
    row_table_stats(qtab),
    row_table_valid_cases(qtab),
    row_table_invalid_vals(qtab)
  )

  row_table$TabNo <- qtab$p$TabNo
  row_table$RowNo <- seq_len(nrow(row_table)) + 3
  row_table
}
empty_row_table <- function() {
  c("RowNo", "RowContent", "RowAbsPercent", "RowWeighted", "TabNo", "RowTitle1", "RowTitle2", "RowTitle3", "RowFormat", "RowDecimals", "RowVariable", "RowValue")


  tibble::tibble(
    RowNo = integer(),
    RowContent = character(),
    RowAbsPercent = character(),
    RowWeighted = character(),
    TabNo = integer(),
    # RowMerge1 = integer(),
    # RowMerge2 = integer(),
    RowTitle1 = character(),
    RowTitle2 = character(),
    RowTitle3 = character(),
    RowFormat = character(),
    RowDecimals = integer(),
    RowVariable = character(),
    RowValue = double()
  )
}
row_table_total_line <- function(qtab) {
  UseMethod("row_table_total_line")
}

row_table_total_line.default <- function(qtab) {
  row_table <- empty_row_table()
  total_row_text <- qtab$p$l_lexikon["cTabGesamt"]
  abs_text <- qtab$p$l_lexikon["cTabAbs"]
  row_variable <- qtab$p$RowVar |> paste(collapse = ", ")
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals", "RowVariable", "RowValue")] <- list("Total", "Abs", total_row_text, total_row_text, abs_text, 0L, row_variable, 1)
  row_table
}
row_table_total_line.qtab_type_mw <- function(qtab) {
  NULL
}

row_table_valid_mw <- function(qtab) {
  UseMethod("row_table_valid_mw")
}

row_table_valid_mw.default <- function(qtab) {
  NULL
}
row_table_valid_mw.qtab_type_mw <- function(qtab) {
  row_table <- empty_row_table()
  valid_mw_text <- qtab$p$l_lexikon["cTabGesamtMW"]
  abs_text <- qtab$p$l_lexikon["cTabAbs"]
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals")] <- list("Valid", "Abs", valid_mw_text, valid_mw_text, abs_text, 0L)
  row_table
}


row_table_valid_answers_line <- function(qtab) {
  UseMethod("row_table_valid_answers_line")
}

row_table_valid_answers_line.qtab_type_mcg <- row_table_valid_answers_line.qtab_type_mdg <- function(qtab) {
  row_table <- empty_row_table()
  valid_answers_row_text <- qtab$p$l_lexikon["cTabGesamtMFA"]
  abs_text <- qtab$p$l_lexikon["cTabAbs"]
  row_table[
    1,
    c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3")
  ] <- list(
    # TODO: Wolf sagen dass geaendert zu "SumOfValid"...:
    "SumOfValid", "Abs", valid_answers_row_text, valid_answers_row_text, abs_text
  )
  row_table$RowVariable <- qtab$p$RowVar |> paste(collapse = ", ")
  row_table
}
row_table_valid_answers_line.qtab_type_cat <- row_table_valid_answers_line.qtab_type_mw <- function(qtab) {
  NULL
}

row_table_body <- function(qtab) {
  UseMethod("row_table_body")
}
row_table_body.qtab_type_mcg <- row_table_body.qtab_type_cat <- function(qtab) {
  occuring_vals <- qtab$d$dat_mod[qtab$p$RowVar] |> unlist(use.names = FALSE) |> unique()
  invalid_vals <- qtab$p$Unguelt
  vallabs <- attr(qtab$d$dat_mod[[qtab$p$RowVar[1]]], "labels")

  # the following is equivalent to (but faster with base R):
  # vallab_table <- vallabs |>
  #   tibble::enframe("vallab", "val") |>
  #   dplyr::full_join(tibble(val = occuring_vals), by = "val") |>
  #   dplyr::filter(!val %in% invalid_vals) |>
  #   dplyr::slice(rep(seq_len(n_vals), each = 2))

  # the following is the same as:
  # all_valid_vals <- vallabs |> c(occuring_vals) |> dplyr::setdiff(invalid_vals)
  # but keeping the names (setdiff removes the names)
  all_valid_vals <- c(vallabs, occuring_vals)
  do_sort <- qtab$p$Sort %in% "ORDER=D"
  if (length(do_sort) == 0) {
    do_sort <- FALSE
  }
  all_valid_vals <- all_valid_vals[!duplicated(all_valid_vals) & !all_valid_vals %in% invalid_vals] |>
    # TODO: Wolf fragen was es alles gibt:
    sort(decreasing = do_sort)

  vallab_table <- all_valid_vals |>
    tibble::enframe("vallab", "val")
  n_vals <- nrow(vallab_table)

  vallab_table <- vallab_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowValue <- vallab_table$val
  row_table$RowTitle1 <- vallab_table$vallab
  row_table$RowTitle2 <- vallab_table$vallab
  row_table$RowTitle3 <- c(
    qtab$p$l_lexikon["cTabAbs"],
    qtab$p$l_lexikon["cTabProz"]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(
    0L,
    1L
  ) |> rep(n_vals)
  row_table$RowValue <- strip_attributes(vallab_table$val)
  row_table$RowVariable <- qtab$p$RowVar |> paste(collapse = ", ")
  row_table$RowContent <- "Detail"
  row_table
}

row_table_body.qtab_type_mdg <- function(qtab) {
  l_varlabs <- qtab$d$dat_mod[qtab$p$RowVar] |> purrr::map(\(x) attr(x, "label", exact = TRUE))
  no_varlab_idx <- l_varlabs |> sapply(is.null)
  if (sum(no_varlab_idx) > 0) {
    l_varlabs[no_varlab_idx] <- names(l_varlabs[no_varlab_idx])
    warning(
      "There is no variable label for these mdg variable(s): ",
      no_varlab_idx[no_varlab_idx] |> names() |> paste(collapse = ", ")
    )
  }
  label_table <- data.frame(
    var = names(l_varlabs),
    label = unlist(l_varlabs, use.names = FALSE)
  )
  n_vals <- nrow(label_table)

  label_table <- label_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  mdg_val <- qtab$p$MdgVal
  row_table[seq_len(n_vals * 2),]$RowValue <- mdg_val
  row_table$RowTitle1 <- label_table$label
  row_table$RowTitle2 <- label_table$label
  row_table$RowTitle3 <- c(
    qtab$p$l_lexikon["cTabAbs"],
    qtab$p$l_lexikon["cTabProz"]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(
    0L,
    1L
  ) |> rep(n_vals)
  row_table$RowContent <- "Detail"
  row_table$RowVariable <- label_table$var
  row_table
}

row_table_body.qtab_type_mw <- function(qtab) {
  l_varlabs <- qtab$d$dat_mod[qtab$p$RowVar] |> purrr::map(\(x) attr(x, "label", exact = TRUE))
  no_varlab_idx <- l_varlabs |> sapply(is.null)
  if (sum(no_varlab_idx) > 0) {
    l_varlabs[no_varlab_idx] <- names(l_varlabs[no_varlab_idx])
    warning(
      "There is no variable label for these mdg variable(s): ",
      no_varlab_idx[no_varlab_idx] |> names() |> paste(collapse = ", ")
    )
  }
  label_table <- data.frame(
    var = names(l_varlabs),
    label = unlist(l_varlabs, use.names = FALSE)
  )
  n_vals <- nrow(label_table)

  label_table <- label_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowTitle1 <- label_table$label
  row_table$RowTitle2 <- label_table$label
  row_table$RowTitle3 <- c(
    # TODO: generalize for media std err etc.:
    qtab$p$l_lexikon["cTabMean"],
    qtab$p$l_lexikon["cTabGueltig"]
  ) |> rep(n_vals)
  row_table$RowContent <- c(
    "MStatistics",
    "MValid"
  ) |> rep(n_vals)
  row_table$RowDecimals <- c(
    1L,
    0L
  ) |> rep(n_vals)
  row_table$RowVariable <- label_table$var
  row_table
}



row_table_valid_cases <- function(qtab) {
  UseMethod("row_table_valid_cases")
}
row_table_valid_cases.default <- function(qtab) {
  row_table <- empty_row_table()
  valid_cases_text <- qtab$p$l_lexikon["cTabGueltig"]
  abs_text <- qtab$p$l_lexikon["cTabAbs"]
  percent_text <- qtab$p$l_lexikon["cTabProz"]
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals", "RowValue")] <- list("Valid", "Abs", valid_cases_text, valid_cases_text, abs_text, 0, 1)
  row_table[2, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals", "RowValue")] <- list("Valid", "Percent", valid_cases_text, valid_cases_text, percent_text, 1, 1)
  row_table$RowVariable <- qtab$p$RowVar |> paste(collapse = ", ")
  row_table
}
row_table_valid_cases.qtab_type_mw <- function(qtab) {
  NULL
}

row_table_summary <- function(qtab) {
  UseMethod("row_table_summary")
}
row_table_summary.default <- function(qtab) {
  NULL
}
row_table_summary.qtab_type_cat <- function(qtab) {
  cat_rec_string <- qtab$p$CatRec
  if (is.null(cat_rec_string)) {
    return(NULL)
  }
  # TODO: implement NPS...:
  # uncomment/replace stuff to tabulate non-recoded (not covered by CatRec) valid `RowVal`s:

  # cat_rec_string <- cat_rec_string |> stringr::str_remove("\\{.*\\}")
  cat_lab_string <- qtab$p$CatLab
  # cat_rec_interval_splits <- split_cat_rec_string(cat_rec_string)
  cat_lab_splits <- split_cat_lab_string(cat_lab_string)
  # cat_rec_quos <- lapply(cat_rec_interval_splits$interval_strings, gen_cat_rec_fun)
  # vec <- qtab$d$dat_mod[[qtab$p$RowVar]]
  # unique_vals <- unique(vec) |> strip_attributes()
  # vals_in_cat_rec <- purrr::map(
  #   cat_rec_quos,
  #   \(f, x) f(unique_vals)
  # ) |>
  #   any_true()
  # invalid_vals <- dplyr::coalesce(qtab$p$Unguelt, mapping$options$l_macro_scenario$Unguelt)
  # vals_not_in_cat_rec <- unique_vals[!vals_in_cat_rec] |> setdiff(invalid_vals)
  # all_catrec_labs <- c(cat_lab_splits, vals_not_in_cat_rec |> purrr::set_names())
  row_table <- empty_row_table()
  # n_vals <- length(all_catrec_labs)
  n_vals <- length(cat_lab_splits)
  # row_table[seq_len(n_vals * 2),]$RowValue <- unname(all_catrec_labs) |> rep(each = 2)
  row_table[seq_len(n_vals * 2),]$RowValue <- unname(cat_lab_splits) |> rep(each = 2)
  row_table$RowContent <- "Summary"
  row_table$RowWeighted <- "Unweighted"
  row_table$RowTitle1 <- qtab$p$l_lexikon[["cTabZsfg"]]
  # row_table$RowTitle2 <- names(all_catrec_labs) |> rep(each = 2)
  row_table$RowTitle2 <- names(cat_lab_splits) |> rep(each = 2)
  row_table$RowTitle3 <- c(
    qtab$p$l_lexikon["cTabAbs"],
    qtab$p$l_lexikon["cTabProz"]
  ) |> rep(n_vals)
  row_table$RowDecimals <- c(0L, 1L) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowVariable <- paste0(qtab$p$RowVar, "__summary")
  row_table
}

row_table_stats <- function(qtab) {
  #TODO
  NULL
}
# TODO: source out common functionality with row_table_body!
row_table_invalid_vals <- function(qtab) {
  UseMethod("row_table_invalid_vals")
}
row_table_invalid_vals.qtab_type_mcg <- row_table_invalid_vals.qtab_type_cat <- function(qtab) {
  occuring_vals <- qtab$d$dat_mod[qtab$p$RowVar] |> unlist(use.names = FALSE) |> unique()
  invalid_vals <- qtab$p$Unguelt
  if (all(!occuring_vals %in% invalid_vals)) {
    return(NULL)
  }
  vallabs <- attr(qtab$d$dat_mod[[qtab$p$RowVar[1]]], "labels")

  occuring_invalid_vals <- intersect(invalid_vals, occuring_vals)
  all_invalid_vals <- c(vallabs, occuring_invalid_vals)
  all_invalid_vals <- all_invalid_vals[!duplicated(all_invalid_vals) & all_invalid_vals %in% occuring_invalid_vals]

  vallab_table <- all_invalid_vals |>
    tibble::enframe("vallab", "val")
  n_vals <- nrow(vallab_table)

  vallab_table <- vallab_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowValue <- vallab_table$val
  row_table$RowTitle1 <- vallab_table$vallab
  row_table$RowTitle2 <- vallab_table$vallab
  row_table$RowTitle3 <- c(
    qtab$p$l_lexikon["cTabAbs"],
    qtab$p$l_lexikon["cTabProz"]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(0L, 1L) |> rep(n_vals)
  row_table$RowValue <- strip_attributes(vallab_table$val)
  row_table$RowVariable <- qtab$p$RowVar |> paste(collapse = ", ")
  row_table$RowContent <- "Valid"

  row_table
}
row_table_invalid_vals.qtab_type_mdg <- function(qtab) {
  l_varlabs <- qtab$d$dat_mod[qtab$p$Unguelt] |> purrr::map(\(x) attr(x, "label", exact = TRUE))
  mdg_val <- qtab$p$MdgVal
  invalids_present <- qtab$d$dat_mod[names(l_varlabs)] |>
    purrr::map_lgl(\(x) mdg_val %in% x)
  if (sum(invalids_present) == 0) {
    return(NULL)
  }
  l_varlabs <- l_varlabs[invalids_present]
  no_varlab_idx <- l_varlabs |> sapply(is.null)
  if (sum(no_varlab_idx) > 0) {
    l_varlabs[no_varlab_idx] <- names(l_varlabs[no_varlab_idx])
    warning(
      "There is no variable label for these mdg variable(s): ",
      no_varlab_idx[no_varlab_idx] |> names() |> paste(collapse = ", ")
    )
  }
  label_table <- data.frame(
    var = names(l_varlabs),
    label = unlist(l_varlabs, use.names = FALSE)
  )
  n_vals <- nrow(label_table)

  label_table <- label_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowValue <- mdg_val
  row_table$RowTitle1 <- label_table$label
  row_table$RowTitle2 <- label_table$label
  row_table$RowTitle3 <- c(
    qtab$p$l_lexikon["cTabAbs"],
    qtab$p$l_lexikon["cTabProz"]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(
    0L,
    1L
  ) |> rep(n_vals)
  row_table$RowVariable <- label_table$var
  row_table$RowContent <- "Detail"
  row_table
}
row_table_invalid_vals.qtab_type_mw <- function(qtab) {
  #TODO
  NULL
}
