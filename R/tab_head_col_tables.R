gen_tab_table <- function(params) {
  title_vec <- params$Title
  tibble::tibble(
    QuestNo = params$Abbreviation,
    QuestLine = params$row,
    TabNo = params$i_tab,
    TabType = params$Type |> toupper(),
    TabTitle = title_vec |> paste(collapse = "\n"),
    TabTitle1 = title_vec[1],
    TabTitle2 = dplyr::coalesce(title_vec[2], TabTitle1),
    TabTitle3 = dplyr::coalesce(title_vec[3], TabTitle2),
    TabCaption = params$Fussnote %||% NA_character_,
    SelVal = params$SelVal %||% NA_character_,
    repov_name = params$repov_names %||% NA_character_,
  )
}


gen_head_table <- function(mapping) {
  header_vars <- mapping$options$l_macro_scenario$ColVar
  # lapply needed instead of purrr::map_chr because variables could have no variable label:
  header_varlabs <- lapply(header_vars, \(x) attr(mapping$dat_mod[[x]], "label", exact = TRUE))
  no_varlab_idx <- header_varlabs |> purrr::map_lgl(is.null)
  if (sum(no_varlab_idx) > 0) {
    header_varlabs[no_varlab_idx] <- header_vars[no_varlab_idx]
    warning(
      "There is no variable label for these header variable(s): ",
      header_vars[no_varlab_idx] |> names() |> paste(collapse = ", ")
    )
  }
  header_varlabs <- unlist(header_varlabs) %||% character()

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
  head_table <- mapping$qsheet$head_table

  ColBegin <- data.frame(HeadNo = c(1,1,1))
  ColBegin$ColTitle1 = ""
  ColBegin$ColTitle2 = ""
  # TODO: ask Wolf if ColVariable is needed...:
  # ... not implemented like Wolf until now ...
  # ColBegin$ColVariable = "DC#ROWHEADER"

  ColEnd <- data.frame(HeadNo = c(nrow(head_table) + 2:3))
  ColEnd$ColTitle1 = ""
  ColEnd$ColTitle2 = ""
  # ColEnd$ColVariable = c("DC#EMPTY", "DC#TITLE")

  value_col_table0 <- tibble::tibble(
    ColNo = integer(),
    HeadNo = integer(),
    ColTitle1 = character(),
    ColTitle2 = character(),
    ColVariable = character(),
    ColValue = integer(),
  )
  # TODO: ask Wolf if " " is necessary for ColTitle2...:
  value_col_table0[1, c(
    "ColNo",
    "HeadNo",
    "ColTitle1",
    "ColTitle2",
    "ColVariable",
    "ColValue"
  )] <- list(
    1L,
    2L,
    mapping$options$l_lexikon["cTabGesamt"],
    " ",
    "DC#STICHPROBE",
    1L
  )
  value_col_table1 <- dplyr::bind_rows(
    ColBegin,
    value_col_table0,
  )
  if (nrow(head_table) > 1) {
    # TODO: get from paramter in mapping...!
    colvar_headers <- head_table[1:nrow(head_table),]

    # TODO: clean up this mess: ...!
    names(colvar_headers)[names(colvar_headers) == "HeadTitle"] <- "ColTitle1"
    names(colvar_headers)[names(colvar_headers) == "HeadName"] <- "ColVariable"
    colvar_headers$ColValue <- lapply(colvar_headers$ColVariable, \(x) attr(mapping$dat_mod[[x]], "labels"))
    colvar_headers$ColTitle2 <- lapply(colvar_headers$ColValue, \(x) names(x))
    value_col_table1 <- dplyr::bind_rows(
      value_col_table1,
      colvar_headers |> tidyr::unnest(c(ColValue, ColTitle2))
    )
  }
  value_col_table1 <- dplyr::bind_rows(
    value_col_table1,
    ColEnd,
  )
  value_col_table1$ColNo <- 1:nrow(value_col_table1)
  mapping$qsheet$col_table_all <- value_col_table1
  mapping$qsheet$col_table <- value_col_table1 |> dplyr::slice(-c(1:3, nrow(value_col_table1) - 0:1))
}
