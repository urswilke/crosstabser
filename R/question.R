read_qsheet_raw <- function(mapping_file, row, sheet = "Questions") {
  # TODO: only use English column names
  df_questions <- readxl::read_excel(
    mapping_file, sheet = sheet,
    col_types = "text"
  )
  res <- df_questions |>
    # first row column names... (=> " + 1"):
    dplyr::mutate(row = dplyr::row_number() + 1, .before = 1) |>
    tidyr::drop_na("Type") |>
    dplyr::select(-dplyr::matches("^Col[A-Z]$"))
  # filter row indices specified, otherwise all:
  if (is.null(row)) {
    return(res)
  }
  # TODO: think if this should also be reduced to only reading the specified `row`s (e.g. with openxlsx)!
  # In the example mapping now, it only takes < 0.3 seconds,
  # but for big mappings this probably takes more than a second easily.
  res[res$row %in% row,]
}

preprocess_qrows_params <- function(mapping) {
  mapping$qsheet$qsheet_raw |>
    dplyr::mutate(
      Title = Title |> strsplit("' '"),
      RowVar = lapply(RowVar, \(x) extract_rowvars(x, mapping$dat_mod)),
      Unguelt = split_cell(Unguelt),
      Unguelt = purrr::map_if(Unguelt, Type %in% c("cat", "mcg", "mw"), as.numeric, .else = ~.x),
      Type = as.list(Type),
      # hopefully, won't be needed one day:
      Filter = spss_to_r(Filter),
      Filter = as.list(Filter) |>
        purrr::map(\(x) x |> append(mapping$options$l_macro_scenario$Filter)) |>
        purrr::map(\(x) x[!is.na(x)]),
      SelVar = split_cell(SelVar),
      SelVal = split_cell(SelVal),
      RvEmp = stringr::str_trim(RvEmp) == "EXCLUDE",
      Exclusive = split_cell(Exclusive),
    ) |>
    purrr::transpose() |>
    lapply(\(x) x[!is.na(x)])
}

process_qrow_params <- function(qrow_params, mapping) {
  qrow_params |>
    process_selval(mapping) |>
    lapply(\(x) process_mw_rows(x, mapping)) |> unlist(recursive = FALSE) |>
    lapply(\(x) process_cat_rows(x, mapping)) |> unlist(recursive = FALSE) |>
    lapply(\(x) process_repov_rows(x, mapping)) |> unlist(recursive = FALSE) |>
    purrr::imap(\(x, .y) {x$i_tab <- .y; x})
}
process_selval <- function(qrow_params, mapping) {
  selvar <- qrow_params$SelVar
  if (is.null(selvar[1])) {
    return(list(qrow_params))
  }
  rowvars <- qrow_params$RowVar

  n_selval <- length(qrow_params$SelVal)
  res0 <- rep(list(qrow_params), each = n_selval)

  selvar_vallabs <- attr(mapping$dat_mod[[qrow_params$SelVar[[1]][1]]], "labels")
  selval_relabels <- qrow_params$SelVal |>
    stringr::str_extract("(?<=:)[^ ]+") |>
    stringr::str_replace_all("_", " ")
  selvals <- qrow_params$SelVal |>
    as.numeric() |>
    suppressWarnings()

  subtitles <- dplyr::coalesce(
    selval_relabels,
    selvar_vallabs[match(selvals, selvar_vallabs)] |> names()
  )

  purrr::pmap(
    list(
      res0,
      subtitles,
      qrow_params$SelVal
    ),
    edit_selval_info
  )
}

edit_selval_info <- function(x, subtitle, selval) {
  x$Title <- add_selval_title(x$Title, subtitle)
  x$SelVal <- selval
  x
}
add_selval_title <- function(title, subtitle) {
  if (any(stringr::str_detect(title, "DC#SELVALLAB"))) {
    return(stringr::str_replace(title, "DC#SELVALLAB", subtitle))
  }
  title |> append(subtitle)
}

process_mw_rows <- function(qrow_params, mapping) {
  res <- list(qrow_params)
  if (qrow_params$Type != "mw") {
    return(res)
  }
  mw_label <- qrow_params$MeanOverviewLabel %||% mapping$options$l_lexikon[["cTabMeanOV"]]
  title <- qrow_params$Title
  res[[1]]$Title <- title |> append(mw_label)
  if (!is.null(qrow_params$Freq) && qrow_params$Freq %in% c("0", "FALSE")) {
    return(res)
  }
  res <- res[c(1, 1)]
  res[[2]]$Title <- title
  res[[2]]$Type <- "cat"
  res
}

process_cat_rows <- function(qrow_params, mapping) {
  if (qrow_params$Type != "cat") {
    return(list(qrow_params))
  }
  if (length(qrow_params$RowVar) == 1) {
    qrow_params$i_cat <- 1L
    return(list(qrow_params))
  }
  n_cats <- if (length(qrow_params$SelVar) > 0) {
    length(qrow_params$RowVar) / length(qrow_params$SelVar)
  } else {
    length(qrow_params$RowVar)
  }
  cat_rowvars <- get_multi_cat_subtitle_vars(qrow_params)
  varlabs <- lapply(cat_rowvars, \(rowvar) attr(mapping$dat_mod[[rowvar]], "label", exact = TRUE))


  res <- rep(list(qrow_params), each = n_cats)

  for (i_cat in seq_len(n_cats)) {
    res[[i_cat]]$i_cat <- i_cat
    if (n_cats > 1) {
      res[[i_cat]]$Title <- c(res[[i_cat]]$Title, varlabs[[i_cat]])
    }
  }
  res
}
get_multi_cat_subtitle_vars <- function(qrow_params) {
  if (length(qrow_params$SelVar) == 0) {
    return(qrow_params$RowVar)
  }
  qrow_params$RowVar[seq(1, length(qrow_params$RowVar), length(qrow_params$SelVar))]
}
process_repov_rows <- function(qrow_params, mapping) {
  if (qrow_params$Type != "mw" || is.null(qrow_params$RepOV)) {
    return(list(qrow_params))
  }
  repov_strings <- strsplit(qrow_params$RepOV, "\\|")[[1]]
  repov_names <- repov_strings |> stringr::str_extract("^.*(?=:)")
  mw_rec_strings <- repov_strings |> stringr::str_remove("^.*:")

  mw_title_string <- qrow_params$Title
  repov_title_appendices <- paste0(
    repov_names,
    mapping$options$l_lexikon[["cTabOverview"]]
  )
  repov_titles <- repov_title_appendices |> lapply(\(x) c(
    mw_title_string[-length(mw_title_string)],
    x
  ))

  n_repov <- length(repov_strings)
  if (!is.null(qrow_params$MW) && qrow_params$MW %in% c("0", "FALSE")) {
    l_mw <- NULL
  } else {
    l_mw <- list(qrow_params)
  }
  l_repov <- list(qrow_params)[rep(1, n_repov)]
  l_repov <- purrr::pmap(
    list(
      l_repov,
      repov_titles,
      mw_rec_strings,
      repov_names
    ),
    edit_repov_lists
  )
  append(l_repov, l_mw)
}
edit_repov_lists <- function(x, title, mw_rec_string, repov_name) {
  x$Title <- title
  x$MWRec <- mw_rec_string
  x$repov_names <- repov_name
  x
}
