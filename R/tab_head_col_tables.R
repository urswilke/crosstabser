# to be removed:
gen_tab_table <- function(mapping) {
  mapping$qsheet$qsheet_processed |>
    dplyr::mutate(
      Type = toupper(Type),
      Abbreviation = ifelse(is.na(Abbreviation), "", Abbreviation),
    # TODO: Wolf fragen ob so  nicht besser nummeriert... (?):
    # ) |>
    # dplyr::mutate(
      TabNo = dplyr::row_number(),
      # .by = row
    ) |>
    dplyr::mutate(
      TabNo = TabNo,
      TabName = paste0(Type, "#", Abbreviation, "@", dplyr::row_number()),
      TabType = Type,
      QuestNo = row,
      TabTitle = Title |> purrr::map_chr(\(x) paste(x, collapse = "\n")),
      TabCaption = Fussnote,
      .by = c(row, Type),
      .keep = "none"
    )
}
gen_tab_table2 <- function(params) {
  # TODO: use correct numbers instead of placeholder 9999...:
  # (this needs information from the mapping, not only params)
  tibble::tibble(
    TabNo = 9999,
    row = params$row,
    Type = params$Type |> toupper(),
    TabName = paste0(params$Type, "#", params$Abbreviation  %||% "", "@", 9999),
    TabType = params$Type |> toupper(),
    QuestNo = 9999,
    TabTitle = params$Title |> paste(collapse = "\n"),
    TabCaption = params$Fussnote %||% NA_character_
  )
}


gen_head_table <- function(mapping) {
  header_vars <- mapping$options$l_macro_scenario$ColVar
  header_varlabs <- lapply(header_vars, \(x) attr(mapping$dat_mod[[x]], "label", exact = TRUE))
  no_varlab_idx <- header_varlabs |> sapply(is.null)
  if (sum(no_varlab_idx) > 0) {
    header_varlabs[no_varlab_idx] <- header_vars[no_varlab_idx]
    warning(
      "There is no variable label for these header variable(s): ",
      header_vars[no_varlab_idx] |> names() |> paste(collapse = ", ")
    )
  }
  header_varlabs <- unlist(header_varlabs)
  res <- tibble::tibble(
    HeadNo = integer(),
    HeadName = character(),
    HeadTitle = character()
  )
  res[1,c("HeadName", "HeadTitle")] <- list("DC#STICHPROBE", mapping$options$l_lexikon["cTabGesamt"])
  res[seq_len(length(header_vars)) + 1,]$HeadName <- header_vars
  res[seq_len(length(header_vars)) + 1,]$HeadTitle <- header_varlabs
  res$HeadNo <- seq_len(nrow(res)) + 1
  res
}

gen_col_table <- function(mapping) {
  res <- tibble::tibble(
    ColNo = integer(),
    HeadNo = integer(),
    ColTitle1 = character(),
    ColTitle2 = character(),
    ColVariable = character(),
    ColValue = integer(),
  )
  res[1, c("ColNo", "HeadNo", "ColTitle1", "ColVariable", "ColValue")] <- list(1L, 2L, mapping$options$l_lexikon["cTabGesamt"], "DC#STICHPROBE", 1L)

  head_table <- mapping$qsheet$head_table
  if (nrow(head_table) == 1) {
    return(res)
  }

  # TODO: get from paramter in mapping...!
  colvar_headers <- head_table[1:nrow(head_table),]

  # TODO: clean up this mess: ...!
  names(colvar_headers)[names(colvar_headers) == "HeadTitle"] <- "ColTitle1"
  names(colvar_headers)[names(colvar_headers) == "HeadName"] <- "ColVariable"
  colvar_headers$ColValue <- lapply(colvar_headers$ColVariable, \(x) attr(mapping$dat_mod[[x]], "labels"))
  colvar_headers$ColTitle2 <- lapply(colvar_headers$ColValue, \(x) names(x))
  res <- res |> dplyr::bind_rows(colvar_headers |> tidyr::unnest(c(ColValue, ColTitle2)))
  res$ColNo <- 1:nrow(res) + 3
  res
}
