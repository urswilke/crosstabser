read_qsheet_raw <- function(mapping, row) {
  UseMethod("read_qsheet_raw", mapping$mapping_file)
}

#' @export
read_qsheet_raw.excel <- function(mapping, row) {
  do_all <- is.null(row)
  # filter row indices specified, otherwise all...
  # ... add the first row with the titles, if only specific rows are read:
  rows <- if (do_all) NULL else c(1, row)
  df_questions <- openxlsx2::wb_read(
    mapping$wb,
    sheet = "Questions",
    check_names = TRUE,
    rows = rows
  ) |>
    datadaptor::format_sheet_data()
  if (do_all) {
    res0 <- df_questions |>
      # first row column names... (=> " + 1"):
      dplyr::mutate(row = dplyr::row_number() + 1, .before = 1) |>
      tidyr::drop_na("Type")
  } else {
    res0 <- df_questions |>
      dplyr::mutate(row, .before = 1)
  }

  res0[stringr::str_subset(names(res0), "^Col[A-Z]$", negate = TRUE)]
}
#' @export
read_qsheet_raw.list <- function(mapping, row) {
  df_questions <- dplyr::bind_rows(
    # HACK to complement all columns that are currently in the Questions sheet...
    # TODO: find cleaner solution
    empty_qsheet(),
    mapping$mapping_file$Questions |>
      datadaptor::format_sheet_data()
  )
  df_questions |>
      # first row column names... (=> " + 1"):
      dplyr::mutate(row = dplyr::row_number() + 1, .before = 1) |>
      tidyr::drop_na("Type")
}
#' @export
read_qsheet_raw.google <- function(mapping, row) {
  # TODO: google spreadsheets...
  stop("Not yet implemented for google sheets.")
}

empty_qsheet <- function() {
  tibble::tibble(
    Abbreviation = character(0), Title = character(0), RowVar = character(0),
    Type = character(0), Unguelt = character(0),
    Filter = character(0), CatRec = character(0), CatLab = character(0),
    MWRec = character(0), UngueltMW = character(0), MetrMac = character(0),
    R = character(0), Fussnote = character(0), Sort = character(0),
    SelVar = character(0), SelVal = character(0), MW = character(0),
    Freq = character(0), Zsfg = character(0), NoVLab = character(0),
    MdgVal = character(0), MdgMissLab = character(0), MdgMissValid = character(0),
    Einzelauspraegung = character(0), ZsfgMW = character(0),
    ZsfgMFA = character(0), Weight = character(0), WeightLab = character(0),
    MeanOverviewLabel = character(0), Mult = character(0), RvEmp = character(0),
    Categories = character(0), RepOV = character(0), MWVar = character(0),
    Exclusive = character(0), Checks = character(0)
  )
}
process_qrow_params <- function(df_qrow, mapping) {
  unguelt_mw_interval <- df_qrow$UngueltMW |>
    split_cell(",") |>
    _[[1]] |>
    stringr::str_subset("THRU") |>
    split_cell(" *THRU *") |>
    lapply(as.numeric) |>
    list()
  if (length(unguelt_mw_interval) == 0) {
    unguelt_mw_interval <- NULL
  }
  res <- df_qrow |>
    dplyr::mutate(
      Title = Title |> strsplit("' '"),
      RowVar = lapply(RowVar, \(x) extract_rowvars(x, mapping$ditw$ct$dat_tab)),
      Unguelt = split_cell(Unguelt),
      Unguelt = purrr::map_if(Unguelt, Type %in% c("cat", "mcg", "mw"), as.numeric, .else = ~.x),
      Type = as.list(Type),
      unguelt_mw_interval,
      UngueltMW = split_cell(UngueltMW) |>
        _[[1]] |>
        stringr::str_subset("THRU", negate = TRUE) |>
        as.numeric() |>
        list(),
      SelVar = split_cell(SelVar),
      # HACK to also use CatRec syntax with "THRU" instead of the traditional "=":
      SelVal  = SelVal |> stringr::str_replace_all("(?<!(^|THRU))-", "THRU"),
      # it's important to only split on spaces here to use the CatRec syntax...:
      SelVal = split_cell(SelVal, " "),
      RvEmp = stringr::str_trim(RvEmp) == "EXCLUDE",
      Exclusive = split_cell(Exclusive),
    ) |>
    purrr::transpose() |>
    lapply(\(x) x[!is.na(x)]) |>
    _[[1]]
  if (!"Title" %in% names(res)) {
    res$Title <- ""
    warning("Title not specified.")
  }
  if ("Filter" %in% names(res)) {
    if (length(res$SelVar) > 1) {
      stop(
        "Multiple `Filter`s
        together with multiple `SelVar`s
        are not implemented yet"
      )
    }

    res$Filter <- parse_filter(res)
  }

  res
}

parse_filter <- function(params) {
  params$Filter <- params$Filter |> spss_to_r()
  if (stringr::str_detect(params$Filter, "\\{\\[.*\\]\\}")) {
    return(parse_multi_filter_strings(params$Filter))
  }
  if (stringr::str_detect(params$Filter, "\\{rowvar\\}")) {
    return(parse_rowvar_filter_strings(params))
  }
  params$Filter
}

parse_multi_filter_strings <- function(filter_string) {
  filter_string_parts <- filter_string |>
    stringr::str_extract_all("(?<=\\[)(.*?)(?=\\])") |>
    _[[1]] |>
    stringr::str_split("\\|")

  filter_string_parts |>
    purrr::reduce(
      \(acc, x) stringr::str_replace(acc, "\\{\\[.*?\\]\\}", x),
      .init = filter_string
    )
}
parse_rowvar_filter_strings <- function(params) {
  params$Filter |>
    stringr::str_replace_all(
      "\\{rowvar\\}",
      params$RowVar
    )
}


gen_qtabs_params <- function(qrow_params, mapping) {
  qrow_params |>
    process_selval(mapping) |>
    lapply(\(x) process_mw_rows(x, mapping)) |> unlist(recursive = FALSE) |>
    lapply(\(x) process_cat_rows(x, mapping)) |> unlist(recursive = FALSE) |>
    lapply(\(x) process_repov_rows(x, mapping)) |> unlist(recursive = FALSE) |>
    purrr::imap(\(x, .y) {
      x$i_tab <- .y
      if (is.null(x$Weight)) {
        return(x)
      }
      weight_label <- attr(mapping$ditw$ct$dat_tab[[x$Weight]], "label", exact = TRUE) %||% x$Weight
      x$Title <- append(
        x$Title,
        paste0(mapping$opts$ct$l_lexikon["cTxtWeightedBy"], weight_label)
      )
      x
    })
}
process_selval <- function(qrow_params, mapping) {
  selvar <- qrow_params$SelVar
  if (is.null(selvar[1])) {
    return(list(qrow_params))
  }
  rowvars <- qrow_params$RowVar

  n_selval <- length(qrow_params$SelVal)
  res0 <- rep(list(qrow_params), each = n_selval)

  selvar_vallabs <- attr(mapping$ditw$ct$dat_tab[[qrow_params$SelVar[[1]][1]]], "labels")
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
  x$SelVal <- selval |> stringr::str_remove(":.*")
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
  mw_label <- qrow_params$MeanOverviewLabel %||% mapping$opts$ct$l_lexikon[["cTabMeanOV"]]
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
  varlabs <- lapply(cat_rowvars, \(rowvar) attr(mapping$ditw$ct$dat_tab[[rowvar]], "label", exact = TRUE))


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
    mapping$opts$ct$l_lexikon[["cTabOverview"]]
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
