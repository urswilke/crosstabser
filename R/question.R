read_qsheet_raw <- function(mapping_file, sheet = "Questions") {
  # TODO: only use English column names
  df_questions <- readxl::read_excel(
    mapping_file, sheet = sheet,
    col_types = "text"
  )
  df_questions |>
    # first row column names... (=> " + 1"):
    dplyr::mutate(row = dplyr::row_number() + 1, .before = 1) |>
    tidyr::drop_na("Type") |>
    dplyr::select(-dplyr::matches("^Col[A-Z]$"))
}

# to be removed:
process_qsheet_df <- function(mapping) {
  mapping$qsheet$qsheet_raw |>
    dplyr::mutate(
      Title = Title |> strsplit("' '"),
      RowVar = RowVar |> split_cell(" "),
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
    )
}
extract_qrow_param_list <- function(mapping) {
  mapping$qsheet$qsheet_raw |>
    dplyr::mutate(
      Title = Title |> strsplit("' '"),
      RowVar = RowVar |> split_cell(" "),
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
    purrr::map(\(x) x[!is.na(x)])
}
gen_qrows_df_intermediate <- function(df, mapping) {
  df |>
    unnest_qsheet_rows(mapping) |>
    dplyr::mutate(TabNo = dplyr::row_number(), .before = 1)
}

# to be removed:
unnest_qsheet_rows <- function(df_qsheet, mapping) {
  df_qsheet |>
    row_split() |>
    purrr::map_dfr(\(df_row) unnest_selvar(df_row, mapping)) |>
    row_split() |>
    purrr::map_dfr(\(df_row) unnest_mw_rows(df_row, mapping)) |>
    row_split() |>
    purrr::map_dfr(\(df_row) unnest_cat_rows(df_row, mapping)) |>
    row_split() |>
    purrr::map_dfr(\(df_row) unnest_repov_rows(df_row, mapping))
}
process_qrow_params <- function(qrow_param_list, mapping) {
  qrow_param_list |>
    process_selvar(mapping) |>
    lapply(\(qrow_params) process_mw_rows(qrow_params, mapping)) |> unlist(recursive = FALSE) |>
    lapply(\(qrow_params) process_cat_rows(qrow_params, mapping)) |> unlist(recursive = FALSE) |>
    lapply(\(qrow_params) process_repov_rows(qrow_params, mapping)) |> unlist(recursive = FALSE)

}

process_selvar <- function(qrow_params, mapping) {
  qrow_params$n_selvar <- ifelse(is.null(qrow_params$SelVar), 0, length(qrow_params$SelVar))
  selvar <- qrow_params$SelVar
  if (is.null(selvar[1])) {
    return(list(qrow_params))
  }
  rowvars <- qrow_params$RowVar
  n_selvar <- length(selvar)
  df_multi_selvar <- tibble::tibble(selvar, rowvar = vector("list", n_selvar))
  for (i_row in seq_len(n_selvar)) {
    df_multi_selvar[i_row,]$rowvar <-
      rowvars[seq(
        i_row,
        length(rowvars),
        n_selvar
      )] |>
      list()
  }
  qrow_params$df_multi_selvar <- list(df_multi_selvar)
  # TODO: make new variable...:
  qrow_params$RowVar <- df_multi_selvar$rowvar[1]

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
  dfsel <- qrow_params$df_multi_selvar[[1]]
  n_rowvar <- dfsel$rowvar |> length()
  res <- rep(list(qrow_params), each = n_rowvar)
  if (qrow_params$n_selvar > 1) {
    cat_rowvars <- dfsel$rowvar |> lapply(unlist)
    res$df_multi_selvar <- seq_along(cat_rowvars) |>
      purrr::map(
        \(i) {
          res[[i]]$df_multi_selvar[[1]][["rowvar"]] <- cat_rowvars[[i]] |>
            as.list()
          res[[i]]$df_multi_selvar[[1]]
        }
      )
    cat_rowvars <- cat_rowvars |> lapply(\(x) x[1])
  } else {
    if (n_rowvar == 1) {
      return(list(qrow_params))
    }
    cat_rowvars <- as.list(unlist(qrow_params$RowVar))
  }

  res$RowVar <- cat_rowvars
  if (n_rowvar > 1) {
    res$Title <- purrr::map2(
      res[[1]]$Title,
      res[[1]]$RowVar,
      \(title, rowvar){
        varlab <- attr(mapping$dat_mod[[rowvar]], "label", exact = TRUE)
        title |> append(varlab)
      }
    )
  }

  res
}
process_repov_rows <- function(qrow_params, mapping) {
  if (qrow_params$Type != "mw" || is.null(qrow_params$RepOV)) {
    return(list(qrow_params))
  }
  repov_strings <- strsplit(qrow_params$RepOV, "\\|")[[1]]
  repov_names <- repov_strings |> stringr::str_extract("^.*(?=:)")
  mw_rec_strings <- repov_strings |> stringr::str_remove("^.*:")

  mw_title_string <- qrow_params$Title[[1]]
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









# to be removed:

# TODO: clean up this mess!...:
unnest_selvar <- function(df_row, mapping) {
  selvar <- df_row$SelVar[[1]]
  if (is.na(selvar[1])) {
    return(df_row)
  }
  rowvars <- df_row$RowVar[[1]]
  n_selvar <- length(selvar)
  df_multi_selvar <- tibble::tibble(selvar, rowvar = vector("list", n_selvar))
  for (i_row in seq_len(n_selvar)) {
    df_multi_selvar[i_row,]$rowvar <-
      rowvars[seq(
        i_row,
        length(rowvars),
        n_selvar
      )] |>
      list()
  }
  df_row$df_multi_selvar <- list(df_multi_selvar)
  df_row$RowVar <- df_multi_selvar$rowvar[1]

  n_selval <- length(df_row$SelVal[[1]])
  res <- df_row[rep(1, n_selval),]
  selvar_vallabs <- attr(mapping$dat_mod[[df_row$SelVar[[1]][1]]], "labels")
  selval_relabels <- df_row$SelVal[[1]] |> stringr::str_extract("(?<=:)[^ ]+") |> stringr::str_replace_all("_", " ")
  selvals <- df_row$SelVal[[1]] |>
    as.numeric() |>
    suppressWarnings()

  subtitles <- dplyr::coalesce(
    selval_relabels,
    selvar_vallabs[match(selvals, selvar_vallabs)] |> names()
  )
  res$Title <- res$Title |> purrr::map2(
    subtitles,
    \(title, subtitle) add_selval_title(title, subtitle))
  res$SelVal <- res$SelVal[[1]] |> as.list()

  res
}
add_selval_title <- function(title, subtitle) {
  if (any(stringr::str_detect(title, "DC#SELVALLAB"))) {
    return(stringr::str_replace(title, "DC#SELVALLAB", subtitle))
  }
  title |> append(subtitle)
}

unnest_mw_rows <- function(df_row, mapping) {
  if (df_row$Type != "mw") {
    return(df_row)
  }
  mw_label <- dplyr::coalesce(df_row$MeanOverviewLabel, mapping$options$l_lexikon[["cTabMeanOV"]])
  title <- df_row$Title[[1]]
  df_row$Title[[1]] <- title |> append(mw_label)
  if (df_row$Freq %in% c("0", "FALSE")) {
    return(df_row)
  }
  res <- df_row[c(1, 1),]
  res$Title[[2]] <- title
  res$Type[[2]] <- "cat"
  res
}

unnest_cat_rows <- function(df_row, mapping) {
  if (df_row$Type != "cat") {
    return(df_row)
  }
  n_selvar <- ifelse(is.na(df_row$SelVar[[1]][1]), 0, length(df_row$SelVar[[1]]))
  if (n_selvar > 1) {
    dfsel <- df_row$df_multi_selvar[[1]]
    n_rowvar <- dfsel$rowvar[[1]] |> length()
    cat_rowvars <- dfsel$rowvar |> purrr::transpose() |> lapply(unlist)
    res <- df_row[rep(1, n_rowvar),]
    res$df_multi_selvar <- seq_along(cat_rowvars) |> purrr::map(\(i) {res[i,]$df_multi_selvar[[1]][["rowvar"]] <- cat_rowvars[[i]] |> as.list(); res[i,]$df_multi_selvar[[1]]})
    cat_rowvars <- cat_rowvars |> lapply(\(x) x[1])
  } else {
    n_rowvar <- length(df_row$RowVar[[1]])
    if (n_rowvar == 1) {
      return(df_row)
    }
    cat_rowvars <- as.list(unlist(df_row$RowVar))
    res <- df_row[rep(1, n_rowvar),]
  }

  res$RowVar <- cat_rowvars
  if (n_rowvar > 1) {
    res$Title <- purrr::map2(
      res$Title,
      res$RowVar,
      \(title, rowvar){
        varlab <- attr(mapping$dat_mod[[rowvar]], "label", exact = TRUE)
        title |> append(varlab)
      }
    )
  }

  res
}
unnest_repov_rows <- function(df_row, mapping) {
  if (df_row$Type != "mw" || is.na(df_row$RepOV)) {
    return(df_row)
  }
  repov_strings <- strsplit(df_row$RepOV, "\\|")[[1]]
  repov_names <- repov_strings |> stringr::str_extract("^.*(?=:)")
  mw_rec_strings <- repov_strings |> stringr::str_remove("^.*:")

  mw_title_string <- df_row$Title[[1]]
  repov_title_appendices <- paste0(
    repov_names,
    mapping$options$l_lexikon[["cTabOverview"]]
  )
  repov_titles <- repov_title_appendices |> lapply(\(x) c(
    mw_title_string[-length(mw_title_string)],
    x
  ))

  n_repov <- length(repov_strings)
  if (df_row$MW %in% c("0", "FALSE")) {
    df_mw <- NULL
  } else {
    df_mw <- df_row
  }
  df_repov <- df_row[rep(1, n_repov),]
  df_repov$Title <- repov_titles
  df_repov$MWRec <- mw_rec_strings
  df_repov$repov_names <- repov_names
  dplyr::bind_rows(df_repov, df_mw)
}
