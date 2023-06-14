read_qsheet <- function(mapping) {
  mapping$qsheet$qsheet_raw <- read_qsheet_raw(mapping$mapping_file)
  mapping$qsheet$qsheet_processed <- process_qsheet(mapping)
}

read_qsheet_raw <- function(mapping_file, sheet = "Questions") {
  # TODO: only use English column names
  df_questions <- readxl::read_excel(
    mapping_file, sheet = sheet,
    col_types = "text"
  )
  df_questions |>
    dplyr::mutate(row = dplyr::row_number(), .before = 1) |>
    # https://stackoverflow.com/a/66136167
    dplyr::filter(dplyr::if_any(-row, ~ !is.na(.x)))
}

process_qsheet <- function(mapping) {
  mapping$qsheet$qsheet_raw |>
    tidyr::drop_na(Type) |>
    dplyr::mutate(
      Title = as.list(Title),
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
      RvEmp = stringr::str_trim(RvEmp) == "EXCLUDE"
    ) |>
    unnest_qsheet_rows(mapping)
}

unnest_qsheet_rows <- function(df_qsheet, mapping) {
  df_qsheet |>
    row_split() |>
    purrr::map_dfr(\(row) unnest_selvar(row, mapping)) |>
    row_split() |>
    purrr::map_dfr(\(row) unnest_selval(row, mapping)) |>
    row_split() |>
    purrr::map_dfr(\(row) unnest_mw_rows(row, mapping))
}
unnest_mw_rows <- function(df_row, mapping) {
  if (df_row$Type != "mw") {
    return(df_row)
  }
  mw_label <- dplyr::coalesce(df_row$MeanOverviewLabel, mapping$options$l_lexikon[["cTabMeanOV"]])
  df_row$Title[[1]] <- df_row$Title[[1]] |> append(mw_label)
  if (df_row$Freq %in% "0") {
    return(df_row)
  }
  n_rowvar <- length(df_row$RowVar[[1]])
  res <- df_row[rep(1, n_rowvar + 1),]
  res$Type <- list("mw") |> append(as.list(rep("cat", n_rowvar)))
  res$RowVar <- df_row$RowVar |> append(as.list(unlist(df_row$RowVar)))
  res$Title[-1] <- purrr::map2(
    res$Title[-1],
    res$RowVar[-1],
    \(title, rowvar){
      title[-length(title)] |> append(attr(mapping$dat_mod[[rowvar]], "label", exact = TRUE))
    }
  )
  res
}
unnest_selvar <- function(df_row, mapping) {
  selvars <- df_row$SelVar[[1]]
  if (is.na(selvars[1])) {
    return(df_row)
  }
  rowvars <- df_row$RowVar[[1]]
  n_selvar <- length(selvars)
  res <- df_row[rep(1, n_selvar),]
  res$SelVar <- selvars |> as.list()
  for (i_row in seq_len(n_selvar)) {
    res[i_row,]$RowVar <- rowvars[seq(
      i_row,
      length(rowvars),
      n_selvar
    )] |>
      list()
  }

  res
}
unnest_selval <- function(df_row, mapping) {
  if (is.na(df_row$SelVar[[1]][1])) {
    return(df_row)
  }
  n_selval <- length(df_row$SelVal[[1]])
  res <- df_row[rep(1, n_selval),]
  selvar_vallabs <- attr(mapping$dat_mod[[df_row$SelVar[[1]]]], "labels")
  selval_relabels <- df_row$SelVal[[1]] |> stringr::str_extract("(?<=:)[^ ]+") |> stringr::str_replace_all("_", " ")
  selvals <- df_row$SelVal[[1]] |>
    # stringr::str_extract("^\\d+") |>
    as.numeric() |>
    suppressWarnings()

  subtitles <- dplyr::coalesce(
    selval_relabels,
    selvar_vallabs[match(selvals, selvar_vallabs)] |> names()
  )
  res$Title <- res$Title |> purrr::map2(
    subtitles,
    \(title, subtitle) title |> append(subtitle))

  selval_intervals <- df_row$SelVal[[1]] |> stringr::str_remove(paste0(":?", subtitles))
  res$Filter <- purrr::map2(
    res$Filter,
    selval_intervals,
    \(filt, selval) append(filt, write_selval_filter_string(df_row$SelVar, selval))
  )
  res
}

write_selval_filter_string <- function(selvar, selval) {
  if (!is.na(as.numeric(selval) |> suppressWarnings())) {
    return(paste0(selvar, " == ", selval))
  }
  selval_interval <- stringr::str_split_1(selval, "-")
  paste0(selvar, " >= ", selval_interval[1], " & ", selvar, " <= ", selval_interval[2])
}
