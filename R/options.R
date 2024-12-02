set_options <- function(tabula, ...) {
  UseMethod("set_options", tabula$mapping_file)
}

set_options.excel <- function(tabula, book_no, ...) {
  # TODO: clean up this mess...!
  v_scenario <- openxlsx2::wb_read(
    tabula$wb,
    named_region = "V_Scenario",
    col_names = FALSE
  )[[1]]
  V_Language <- openxlsx2::wb_read(
    tabula$wb,
    named_region = "V_Language",
    col_names = FALSE
  )[[1]]
  df_macro_raw <- openxlsx2::wb_read(
    tabula$wb,
    sheet = "Macro",
    col_names = FALSE,
    check_names = TRUE
  ) |>
    datenanpassr::format_sheet_data()

  names(df_macro_raw) <- paste0("X", seq_len(ncol(df_macro_raw)))

  l_macro_scenario <- extract_scenario_options(df_macro_raw, v_scenario)

  l_lexikon <- read_dictionary(V_Language)

  if (is.null(book_no)) {
    book_no <- openxlsx2::wb_read(
      tabula$wb,
      named_region = "V_BookNo",
      col_names = FALSE
    )[[1]]
  }

  tabula$options <- tibble::lst(
    v_scenario,
    V_Language,
    l_macro_scenario,
    l_lexikon,
    V_BookNo = book_no
  )
}
# TODO: find cleaner solution to use default parameters
# probably the best solution is to override the datenanpassr::Mapping method reading in the parameters in Tabula
set_options.list <- function(
    tabula,
    v_scenario = 1,
    V_Language = 4,
    l_macro_scenario = NULL,
    l_lexikon = read_dictionary(V_Language),
    book_no = 999999999,
    ...
) {
  # HACK to allow to pass the dots (...) to override parameters from the Macro "sheet":
  # TODO: probably much cleaner to add a Macro list element to the mapping_file object,
  # that either accepts a data.frame like from the Excel file Macro sheet or a named list
  # (if not interested in running different scenarios)
  l_macro_scenario <- modifyList(
    list(ColVar = character(), Unguelt = c(-1, -3, -2), Weight = NA_character_,
       Unwgt = FALSE, Filter = NA_character_, scenario_name = "Scenario 2"
    ),
    list(...)
  )
  tabula$options <- tibble::lst(
    v_scenario,
    V_Language,
    l_macro_scenario,
    l_lexikon,
    V_BookNo = book_no
  )
}
set_options.google <- function(tabula, ...) {
  # TODO: google spreadsheets...
  stop("Not yet implemented for google sheets.")
}

read_dictionary <- function(V_Language) {
  df_lexikon_raw <- utils::read.delim(
    # TODO: derive path to Lexikon in Funktionen.xlsm from mapping file:
    system.file("extdata", "lexikon.csv", package = "crosstabser"),
    header = FALSE,
    sep = ";"
  )

  df_lexikon_raw[-1, c(1, V_Language + 1)] |> tibble::deframe()
}

extract_scenario_options <- function(df_macro_raw, v_scenario) {
  param_list <- df_macro_raw[c(1, 4 + v_scenario)] |> tibble::deframe()

  res <- list()
  ColVar <- param_list[["ColVar"]]

  res$ColVar <- if (is.na(ColVar)) {
    character()
  } else {
    ColVar |> stringr::str_split_1("[ \t]+")
  }

  # TODO: also allow ranges...:
  res$Unguelt <- param_list[["Invalid"]] |> split_cell() |> _[[1]] |> as.numeric()
  res$Weight <- param_list[["Weight"]]
  if (!is.na(param_list["Unwgt"]) && param_list["Unwgt"] == "TRUE") {
    res$Unwgt <- TRUE
  } else {
    res$Unwgt <- FALSE
  }

  if ("gesplab" %in% names(param_list) && !is.na(param_list[["gesplab"]])) res$gesplab <- param_list[["gesplab"]]

  res$Filter <- param_list[["Filter"]] |>
    # hopefully, won't be needed one day:
    spss_to_r()
  res$scenario_name <- param_list[[4]]
  res


}

add_global_options <- function(params, mapping) {
  global_options <- mapping$options$l_macro_scenario
  res <- params
  res$Filter <- res$Filter |>
    # hopefully, won't be needed one day:
    spss_to_r() |>
    append(global_options$Filter[!is.na(global_options$Filter)])
  res$Weight <- dplyr::coalesce(res$Weight, global_options$Weight)
  if (is.na(res$Weight)) {
    res$Weight <- list(NULL)
  }
  res$Unwgt <- global_options$Unwgt
  # - quick fix - TODO: find cleaner way for mdg...:
  if (length(res$Unguelt) == 0 && params$Type != "mdg") {
    res$Unguelt <- global_options$Unguelt
  }
  res$ColVar <- global_options$ColVar
  res
}

qtab_params <- function(params, mapping) {
  new_qtab_params(params) |>
    # TODO: think if it's better to separate the parts of the parameters
    # from the Tabula / Qrow more...!
    set_qtab_params(mapping)
}
# S3 class (qtab_params_... cat, mw, mcg or mdg):
new_qtab_params <- function(params) {
  class(params) <- c(paste0("qtab_params_", params$Type), class(params))
  params
}

set_qtab_params <- function(params, mapping) {
  UseMethod("set_qtab_params")
}

set_qtab_params.default <- function(params, mapping) {
  params$rowvars_string <- paste(params$l_selvar$valid %||% params$rowvars_qtab, collapse = ", ")
  # TODO: tell Wolf: Here we could also use ColVar defined in the Questions sheet...:
  # (with params$ColVar %||% ...)
  params$raw_data_colvars <- cv(c(mapping$options$l_macro_scenario$ColVar, "DC#TOTAL"))
  if (is.null(params$Weight[[1]]) & is.na(mapping$options$l_macro_scenario$Weight)) {
    params$long_weight <- character()
  } else {
    params$long_weight <- "weight"
  }

  if (!is.null(params$Sort)) {
    sort_list <- stringr::str_extract_all(
      params$Sort,
      "\\w+ *= *\\w+"
    )[[1]] |>
      stringr::str_split(" *= *")
    # TODO: find a cleaner way to treat this and discuss with Wolf which sorting
    # options should be implemented:
    order_d <- any(sort_list |> purrr::map_lgl(\(x) all(x == c("ORDER", "D"))))
    key_count <- any(sort_list |> purrr::map_lgl(\(x) all(x == c("KEY", "COUNT"))))
    params$sort_params <- tibble::lst(
      order_d,
      key_count
    )
  }
  params |>
    add_global_options(mapping)
}
set_qtab_params.qtab_params_mdg <- function(params, mapping) {
  params$MdgVal <- params$MdgVal %||% "1"
  params$rowvars_valid_qtab <- params$RowVar
  params$rowvars_qtab <- c(params$RowVar, params$Unguelt)
  # TODO: instead of refering to the elements in l_selvar later use helper methods to generate what's needed ?
  if (!is.null(params$SelVar)) {
    params$l_selvar <- list()
    params$l_selvar$rowvars <- gen_selvar_rowvars(params$RowVar, params$SelVar)
    params$l_selvar$rowvars_inv <- gen_selvar_rowvars(params$Unguelt, params$SelVar)
    params$l_selvar$valid <- concat_selvar_rowvars(params$RowVar, params$SelVar)
    params$l_selvar$invalid <- concat_selvar_rowvars(params$Unguelt, params$SelVar)
  }
  NextMethod()
}

gen_selvar_rowvars <- function(rowvars, selvars) {
  if (is.null(rowvars)) {
    return(NULL)
  }
  res <- vector("list", length(selvars))
  for (i in seq_along(selvars)) {
    res[i] <- list(rowvars[seq(i, length(rowvars), length(selvars))])
  }
  res
}

concat_selvar_rowvars <- function(rowvar, selvar) {
  if (is.null(rowvar)) {
    return(NULL)
  }
  rowvar |>
    matrix(nrow = length(selvar)) |>
    asplit(2) |>
    lapply(\(x) paste(x, collapse = "/")) |>
    unlist(use.names = FALSE)
}

set_qtab_params.qtab_params_mw <- function(params, mapping) {
  params$rowvars_qtab <- params$RowVar
  if (!is.null(params$SelVar)) {
    params$l_selvar <- list()
    params$l_selvar$rowvars <- gen_selvar_rowvars(params$rowvars_qtab, params$SelVar)
    params$l_selvar$valid <- concat_selvar_rowvars(params$RowVar, params$SelVar)
  }

  if (!is.null(params$ZsfgMW)) {
    params$df_stat_funs <- process_metr_mac(params, mapping, "ZsfgMW")
    if (nrow(params$df_stat_funs) > 1) stop(
      "Error in ZsfgMW string:\n",
      params$ZsfgMW,
      "\nSetting multiple statistical functions to use in ZsfgMW isn't implemented yet."
    )
  }

  NextMethod()
}
set_qtab_params.qtab_params_cat <- function(params, mapping) {
  params$rowvars_qtab <- get_rowvars_cat(params)
  if (!is.null(params$SelVar)) {
    params$l_selvar <- list()
    params$l_selvar$rowvars <- get_rowvars_cat(params)
    params$l_selvar$valid <- params$rowvars_qtab |>
      paste(collapse = "/")
  }
  if (!is.null(params$MetrMac)) {
    params$df_stat_funs <- process_metr_mac(params, mapping)
  }
  NextMethod()
}
get_rowvars_cat <- function(params) {
  i_cat <- params$i_cat
  nsel <- max(length(params$SelVar), 1)
  if (length(params$SelVar) < 2) {
    return(params$RowVar[i_cat])
  }
  params$RowVar[((i_cat - 1) * nsel + 1):(i_cat * nsel)]
}
set_qtab_params.qtab_params_mcg <- function(params, mapping) {
  params$Exclusive <- as.numeric(params$Exclusive)
  params$rowvars_qtab <- params$RowVar
  if (!is.null(params$SelVar)) {
    params$l_selvar <- list()
    params$l_selvar$rowvars <- gen_selvar_rowvars(params$rowvars_qtab, params$SelVar)
    params$l_selvar$valid <- concat_selvar_rowvars(params$RowVar, params$SelVar)
  }
  NextMethod()
}

df_metr_mac <- data.frame(
  shortcut = c("E", "M", "S", "P", "I", "A", "T"),
  decimals = c(2L, 0L, 1L, 0L, 0L, 0L, 0L),
  fun = c("se", "median", "mean", "percentile", "min", "max", "sum"),
  ctab_entry = c("cTabStdErr", "cTabMedian", "cTabMean", "cTabPercentile", "cTabMin", "cTabMax", "cTabSum")
)

extract_metr_mac_unit <- function(x) {
  data.frame(x) |>
    dplyr::mutate(
      shortcut = stringr::str_extract(x, "^[A-Z]"),
      fun = stringr::str_extract(x, "^[a-z]+"),
      decimals = x |>
        stringr::str_remove("^([A-Z]|[a-z]+)") |>
        stringr::str_extract("^\\d+"),
      further_args = stringr::str_extract_all(x, "(?<=\\[).*?(?=\\])"),
    )
}
get_named_args <- function(x) {
  if (length(x) == 0) {
    NULL
  } else {
    name <- x |> stringr::str_extract("[a-zA-Z]+(?=\\=)")
    val <- x |> stringr::str_extract("(?<=\\=).*")
    purrr::set_names(val, name)
  }
}

process_metr_mac <- function(params, mapping, questions_column = "MetrMac") {
  l <- stringr::str_extract_all(params[[questions_column]], "([A-Z]|[a-z]+)\\d*(\\[.*?\\])*")[[1]]
  df_raw <- l |>
    purrr::map_dfr(extract_metr_mac_unit) |>
    dplyr::mutate(further_args = lapply(further_args, get_named_args)) |>
    tidyr::unnest_wider(further_args)
  df_stat_funs <- df_raw |>
    dplyr::mutate(
      fun = dplyr::coalesce(fun, df_metr_mac$fun[match(shortcut, df_metr_mac$shortcut)]),
      shortcut = dplyr::coalesce(shortcut, df_metr_mac$shortcut[match(fun, df_metr_mac$fun)]),
      percentile_string = ifelse(fun == "percentile", decimals |> stringr::str_sub(1, 2), ""),
      row_title = mapping$options$l_lexikon[
        df_metr_mac$ctab_entry[match(shortcut, df_metr_mac$shortcut)]
      ] |> unname(),
      decimals = ifelse(fun == "percentile", decimals |> stringr::str_sub(3), decimals),
      decimals = as.integer(decimals),
      decimals = dplyr::coalesce(decimals, df_metr_mac$decimals[match(fun, df_metr_mac$fun)]),
      row_title = paste0(percentile_string, row_title),
    )

  # TODO: clean up legacy code structure in df_stat_funs$quantile_val!
  is_percentile <- df_stat_funs$shortcut == "P"
  df_stat_funs$quantile_val <- vector("list", length(l))
  df_stat_funs[is_percentile,]$quantile_val <-
    df_stat_funs[is_percentile,]$percentile_string |>
    as.numeric() |>
    (\(x) x/100)() |>
    as.list()

  if (anyNA(df_stat_funs[c("shortcut", "fun", "decimals")])) {
    warning(
      "There were probably problems with reading some of the strings: ",
      df_stat_funs$x
    )
  }
  df_stat_funs$x <- NULL
  df_stat_funs$percentile_string <- NULL
  df_stat_funs
}

